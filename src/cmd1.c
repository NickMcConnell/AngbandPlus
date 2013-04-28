/* File: cmd1.c */

/*
 * Searching for traps, doors, and essences.  Pickup.  Move player
 * (terrain effects), the running code.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Search for hidden things
 *
 * The deeper you go, the harder traps and secret doors are to find.
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;

	object_type *o_ptr;

	int range = 1;

	/* Start with base search ability, modified by depth */
	chance = (2 * p_ptr->skill_srh) - p_ptr->depth;

	/* Penalize various conditions */
	if (p_ptr->blind) chance = chance / 3;
	if (p_ptr->confused || p_ptr->image) chance = chance / 3;
	if (p_ptr->berserk || p_ptr->necro_rage) chance = chance / 2;

	/* Increase searching range sometimes */
	if (chance >= rand_range(40,  70)) range++;
	if (chance >= rand_range(80, 140)) range++;

	/* Search all grids in range and in los */
	for (y = py - range; y <= py + range; y++)
	{
		for (x = px - range; x <= px + range; x++)
		{
			/* Get adjusted chance */
			int chance2 = chance - ((range-1) * 40);

			/* Require that grid be fully in bounds and in LOS */
			if (!in_bounds_fully(y, x)) continue;
			if (!los(py, px, y, x)) continue;

			/* If grid is not lit, chance is greatly reduced */
			if (!player_can_see_or_infra_bold(y, x)) chance2 /= 3;

			/* Invisible trap that is not directly under the character */
			if ((cave_invisible_trap(y, x)) && ((y != py) || (x != px)))
			{
				if (rand_int(100) < chance2)
				{
					/* Reveal one or more traps, display a message */
					if (reveal_trap(y, x, chance2, TRUE, FALSE))
					{
						/* Disturb */
						disturb(0, 0);
					}
				}
			}

			/* Secret door */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				if (rand_int(100) < chance2)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door */
					place_closed_door(y, x);

					/* Disturb */
					disturb(0, 0);
				}
			}

			/* Only scan objects if adjacent */
			if (distance(py, px, y, x) <= 1)
			{
				/* Scan all objects in the grid */
				for (o_ptr = get_first_object(y, x); o_ptr;
					  o_ptr = get_next_object(o_ptr))
				{
					/* Skip chests without traps */
					if (!check_chest_traps(o_ptr, FALSE)) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						if (rand_int(o_ptr->pval + 50) < p_ptr->skill_srh)
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
}


/*
 * Search for hidden essences
 *
 * - Finding essences requires infusion or alchemy (and also perception) skill.
 * - The deeper you go, the harder essences are to find.
 * - We do not need light to find essences.
 */
void search_essence(bool strong)
{
	/* Skill ranges from 0 to 200 */
	int skill;
	int range;
	int y, x;

	int py = p_ptr->py;
	int px = p_ptr->px;

	char o_name[DESC_LEN];
	object_type *o_ptr;

	int infus, alchm;

	/* Allow users to use both infusion and alchemy */
	infus = get_skill(S_INFUSION, 0, 130);
	alchm = get_skill(S_ALCHEMY, 0, 130);

	skill = rsqrt((infus * infus) + (alchm + alchm)) +
			get_skill(S_PERCEPTION, 0, 50);

	/* Various conditions prevent finding essences */
	if (p_ptr->confused || p_ptr->image) return;
	if (p_ptr->berserk || p_ptr->necro_rage) return;

	/* Deliberate search is better */
	if (strong) skill += 10 + skill / 3;

	/* Modify effective skill by randomized depth */
	skill -= randint(20 + 5 * p_ptr->depth / 3);

	/* Search only sometimes */
	if (skill <= 0) return;


	/* Get range */
	range = 1;
	if (skill >=  25) range++;
	if (skill >=  50) range++;
	if (skill >= 100) range++;

	/* Search all grids in range and in los */
	for (y = py - range; y <= py + range; y++)
	{
		for (x = px - range; x <= px + range; x++)
		{
			/* Require that grid be fully in bounds and in LOS */
			if (!in_bounds_fully(y, x)) continue;
			if (!player_has_los_bold(y, x)) continue;

			/* Scan any objects */
			for (o_ptr = get_first_object(y, x); o_ptr;
				  o_ptr = get_next_object(o_ptr))
			{
				/* Note essences */
				if ((o_ptr->tval == TV_ESSENCE) && (!o_ptr->marked))
				{
					/* Note essence */
					o_ptr->marked = TRUE;

					/* Describe the object */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 1);

					/* Hack -- disturb */
					disturb(0, 0);

					/* Message */
					if ((!p_ptr->blind) && (!no_light()))
					{
						msg_format("You see %s.", o_name);
					}
					else
					{
						msg_format("You sense %s.", o_name);
					}

					/* See the essence */
					lite_spot(y, x);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

					/* Check the next object */
					continue;
				}
			}
		}
	}
}

/*
 * Notice unmarked non-essences underneath the character
 */
void notice_unseen_objects(void)
{
	object_type *o_ptr;
	char o_name[DESC_LEN];

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Scan any objects present */
	for (o_ptr = get_first_object(py, px); o_ptr;
		  o_ptr = get_next_object(o_ptr))
	{
		/* Object is not marked, and is not an essence */
		if ((!o_ptr->marked) && (o_ptr->tval != TV_ESSENCE))
		{
			/* Note object */
			o_ptr->marked = TRUE;

			/* Describe the object */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 1);

			/* Hack -- disturb */
			disturb(0, 0);

			/* Message */
			if ((!p_ptr->blind) && (!no_light()))
			{
				msg_format("You see %s.", o_name);
			}
			else
			{
				msg_format("You feel %s.", o_name);
			}

			/* See the object */
			lite_spot(py, px);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
		}
	}
}


/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Search */
	search();

	/* Search for essences; note, infusion or alchemy required */
	search_essence(TRUE);

	/* Notice unseen objects */
	notice_unseen_objects();
}


/*
 * Return TRUE if the given object is inscribed with "=g".
 *
 * Alternatively, also return TRUE if any similar item in the
 * backpack is marked "=g".
 */
static bool auto_pickup_okay(object_type *o_ptr, bool check_pack, bool pickup, bool quiver)
{
	cptr s;

	/* It can't be carried */
	if (!inven_carry_okay(o_ptr) && (!quiver_carry_okay(o_ptr) || !quiver)) return (FALSE);

	/* Option to vacuum up things on the floor (not recommended) */
	if ((always_pickup) && (!query_floor) && (pickup)) return (TRUE);

	/* Return quivered items to the quiver */
	if (quiver && o_ptr->quivered && quiver_carry_okay(o_ptr)) return (TRUE);

	/* Check inscription */
	if (o_ptr->note)
	{
		/* Find a '=' */
		s = strchr(quark_str(o_ptr->note), '=');

		/* Process permissions */
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

			/* Process permissions */
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

	bool blind = ((p_ptr->blind) || (no_light() && (p_ptr->see_infra == 0)));
	bool autop;
	int old_num;

	object_type *i_ptr;


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Must be ammo or throwing weapon */
	if ((!is_missile(o_ptr)) && (!(f1 & (TR1_THROWING))))
	{
		return (FALSE);
	}

	/* Check for autopickup (has to have a "=g" inscription) */
	autop = auto_pickup_okay(o_ptr, FALSE, FALSE, TRUE);

	/* Hack - Also allow autopickup when poisoning missiles already in the quiver */
	if (o_idx == -1) autop = (TRUE);

	/* No missiles to combine with and no autopickup. */
	if (!autop) return (FALSE);

	/* Check quiver for similar objects or empty space. */
	for (i = INVEN_Q1; i <= INVEN_Q0; i++)
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
			char o_name[DESC_LEN];

			/* Increase carried weight */
			p_ptr->total_weight += i_ptr->weight * (i_ptr->number - old_num);

			/* Get the object again */
			o_ptr = &inventory[i];

			/* Describe the object */
			if (blind) object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
			else       object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Message */
			msg_format("You have %s (%c).", o_name, index_to_label(i));

			/* Delete the object */
			if (o_idx != -1) delete_object_idx(o_idx);

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
 * Hack -- make a pouch
 */
static void make_pouch(void)
{
	object_type *i_ptr;
	object_type *j_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Make a pouch */
	object_prep(i_ptr, lookup_kind(TV_POUCH, 0));

	/* Get the pouch slot */
	j_ptr = &inventory[INVEN_POUCH];

	/* Copy the pouch to the slot */
	object_copy(j_ptr, i_ptr);

	/* Fully identify the pouch */
	object_known(j_ptr);
	j_ptr->ident |= (IDENT_MENTAL);

	/* Increase the weight */
	p_ptr->total_weight += i_ptr->weight * i_ptr->number;

	/* Increment the equip counter by hand */
	p_ptr->equip_cnt++;
}


/*
 * Handle picking up special kinds of objects, such as treasures.
 */
static bool pickup_special(object_type *o_ptr, int this_o_idx)
{
	char o_name[DESC_LEN];
	int sound_msg;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Pick up gold (actually, only special treasures should be left) */
	if (o_ptr->tval == TV_GOLD)
	{
		/* Determine which sound to play */
		if      (o_ptr->pval < 200) sound_msg = MSG_MONEY1;
		else if (o_ptr->pval < 600) sound_msg = MSG_MONEY2;
		else                        sound_msg = MSG_MONEY3;

		/* Message -- regular gold */
		if ((o_ptr->sval == SV_COPPER) || (o_ptr->sval == SV_SILVER) ||
			 (o_ptr->sval == SV_GOLD) || (o_ptr->sval == SV_PLATINUM) ||
			 (o_ptr->sval == SV_MITHRIL) || (o_ptr->sval == SV_ADAMANTITE))
		{
			message_format(sound_msg, 0, "You have found %ld gold pieces worth of %s.",
				(long)o_ptr->pval, o_name);
		}

		/* Message -- regular gems */
		else if (o_ptr->sval < SV_SPECIAL_GOLD_MIN)
		{
			message_format(sound_msg, 0, "You have found %s worth %ld gold pieces.",
				o_name, (long)o_ptr->pval);
		}

		/* Message -- special treasures */
		else
		{
			message_format(sound_msg, 0, "You have found %s worth %ld gold pieces!",
				o_name, (long)o_ptr->pval);
		}

		/* Collect the gold */
		p_ptr->au += o_ptr->pval;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Delete the gold */
		delete_object_idx(this_o_idx);

		/* Check the next object */
		return (TRUE);
	}

	/* Pick up essences */
	if (o_ptr->tval == TV_ESSENCE)
	{
		/* Message */
		msg_format("You have found %s.", o_name);

		/* Collect the essence(s) -- check legality, limit quantity */
		if (o_ptr->sval < NUM_ESSENCE)
		{
			if  (250 - o_ptr->number >= p_ptr->essence[o_ptr->sval])
			{
				p_ptr->essence[o_ptr->sval] += o_ptr->number;
			}
			else
			{
				p_ptr->essence[o_ptr->sval] = 250;
			}
		}

		/* Character has no pouch to store essences */
		if (!inventory[INVEN_POUCH].k_idx)
		{
			/* Make a pouch */
			make_pouch();
		}

		/* Redraw the equipment window (if shown) */
		p_ptr->window |= (PW_EQUIP);

		/* Delete the essence */
		delete_object_idx(this_o_idx);

		/* Check the next object */
		return (TRUE);
	}

	/* Do not skip to the next object just yet */
	return (FALSE);
}



/*
 * Carry an object and delete it.
 */
static void py_pickup_aux(int o_idx, bool msg)
{
	int slot;

	char o_name[DESC_LEN];
	object_type *o_ptr = &o_list[o_idx];

	/* Copy the object to inventory */
	slot = inven_carry(o_ptr);

	/* Handle errors (paranoia) */
	if (slot < 0) return;

	/* Get the new object */
	o_ptr = &inventory[slot];

	/* Optionally, display a message */
	if (msg)
	{
		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Message */
		msg_format("You have %s (%c).", o_name, index_to_label(slot));
	}

	/* Attempt to sense the object, if it has not already been sensed */
	sense_object(o_ptr, slot, FALSE, FALSE);

	/* Note if an artifact */
	if (artifact_p(o_ptr)) history_add_artifact(o_ptr->artifact_index, (o_ptr->ident & IDENT_KNOWN));

	/* Delete the original object */
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

	char o_name[DESC_LEN];
	char ch;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	/* Objects picked up.  Used to determine time cost of command. */
	byte objs_picked_up = 0;

	int floor_num = 0, floor_list[MAX_FLOOR_STACK + 1], floor_o_idx = 0;
	int sound_msg;

	int can_pickup = 0;
	bool call_function_again = FALSE;

	bool blind = ((p_ptr->blind) || (no_light() && (p_ptr->see_infra == 0)));

	bool force_display_list = FALSE;
	bool msg = TRUE;

	s32b total_gold = 0L;
	byte *treasure;


	/* Nothing to pick up -- return */
	if (!cave_o_idx[py][px]) return (0);



	/* Allocate and wipe an array of ordinary gold objects */
	C_MAKE(treasure, FIRST_SPECIAL_TREASURE, byte);
	(void)C_WIPE(treasure, FIRST_SPECIAL_TREASURE, byte);

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
		    (o_ptr->sval >= FIRST_SPECIAL_TREASURE)) continue;

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
		char buf[1024];
		char tmp[DESC_LEN];
		int i, count, total, k_idx;

		/* Build a message */
		(void)strnfmt(buf, sizeof(buf), "You have found %ld gold pieces worth of ",  total_gold);

		/* Count the types of treasure present */
		for (total = 0, i = 0; i < FIRST_SPECIAL_TREASURE; i++)
		{
			if (treasure[i]) total++;
		}

		/* List the treasure types */
		for (count = 0, i = 0; i < FIRST_SPECIAL_TREASURE; i++)
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
			(void)my_strcat(buf, tmp, sizeof(buf));

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

		/* Determine which sound to play */
		if      (total_gold < 200) sound_msg = MSG_MONEY1;
		else if (total_gold < 600) sound_msg = MSG_MONEY2;
		else                       sound_msg = MSG_MONEY3;

		/* Display the message */
		message_format(sound_msg, 0, "%s", buf);

		/* Add gold to purse */
		p_ptr->au += total_gold;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Free the gold array */
	FREE(treasure);


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

		/* Handle special kinds of objects (no time cost) */
		if (pickup_special(o_ptr, this_o_idx)) continue;

		/* Automatically pick up objects into the quiver */
		if ((p_ptr->auto_pickup_okay) && (quiver_carry(o_ptr, this_o_idx)))
		{
			objs_picked_up++;
			continue;
		}

		/* Automatically pick up some items into the backpack */
		if ((p_ptr->auto_pickup_okay) && (auto_pickup_okay(o_ptr, TRUE, pickup, FALSE)))
		{
			/* Pick up the object (with a message) */
			py_pickup_aux(this_o_idx, TRUE);

			/* Take a small amount of time */
			objs_picked_up++;
			continue;
		}

		/* Attempt to sense all other objects */
		sense_object(o_ptr, -4, FALSE, FALSE);

		/* Flush any sensing messages  XXX */
		message_flush();


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
		if (floor_num == MAX_FLOOR_STACK) break;
	}

	/* There are no objects left */
	if (!floor_num) return (objs_picked_up);


	/* Mention the objects if player is not picking them up. */
	if (!pickup)
	{
		/* Optionally, display more information about floor items */
		if ((query_floor) && (floor_num > 1))
		{
			/* Scan all marked objects in the grid */
			(void)scan_floor(floor_list, &floor_num, py, px, 0x03);

			/* Save screen */
			screen_save(FALSE);

			/* Display objects on the floor */
			show_floor(floor_list, floor_num, FALSE, blind);

			/* Display prompt */
			prt(format("You %s: ",
			    (blind ? "feel something on the floor" : "see")), 0, 0);

			/* Move cursor back to character, if needed */
			if (highlight_player) move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Wait for it.  Use key as next command. */
			p_ptr->command_new = inkey(FALSE);

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
				if (blind) object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
				else       object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				message_flush();

				/* Message */
				msg_format("You %s %s.", (blind ? "feel" : "see"),
					o_name);
			}

			/* Several objects */
			else
			{
				message_flush();

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
			if (blind) object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
			else       object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Message */
			message_flush();
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
				screen_save(FALSE);

				/* Display objects on the floor */
				show_floor(floor_list, floor_num, FALSE, blind);

				/* Display prompt */
				prt("You have no room for the following objects: ", 0, 0);

				/* Move cursor back to character, if needed */
				if (highlight_player) move_cursor_relative(p_ptr->py, p_ptr->px);

				/* Wait for it.  Use key as next command. */
				p_ptr->command_new = inkey(FALSE);

				/* Restore screen */
				screen_load();
			}

			/* Display compact information */
			else
			{
				/* Message -- not very informative */
				message_flush();
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
			screen_save(FALSE);

			/* Display objects on the floor */
			show_floor(floor_list, floor_num, FALSE, blind);

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

			/* Move cursor back to character, if needed */
			if (highlight_player) move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Get response */
			ch = inkey(FALSE);

			/* Restore screen */
			screen_load();

			/* We don't want to pick up this item */
			if ((ch != '\r') && (ch != '\n') && (ch != 'g'))
			{
				/* Save as a new command; move later */
				p_ptr->command_new = ch;

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

		/* Request a list */
		p_ptr->command_see = TRUE;

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

		/* With a list, we do not need explicit pickup messages */
		msg = FALSE;
	}

	/* Pick up object, if legal */
	if (this_o_idx)
	{
		/* Pick up the object */
		py_pickup_aux(this_o_idx, msg);

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
 * It takes some dexterity, or failing that, strength, to get out of
 * pits.  -LM-
 */
static bool escape_pit(void)
{
	/* It is easier, but takes longer, to clamber than to leap */
	int str_escape = adj_dis[p_ptr->stat_ind[A_STR]] - 116;
	int dex_escape = (adj_dis[p_ptr->stat_ind[A_DEX]] - 127) * 4;

	/* Characters with feather fall always succeed */
	if (p_ptr->ffall)
	{
		msg_print("You skip easily out of the pit.");
		return (TRUE);
	}

	/* First attempt to leap out of the pit, */
	if (dex_escape < randint(p_ptr->depth + 40))
	{
		/* then attempt to climb out of the pit. */
		if (str_escape < randint(p_ptr->depth + 40))
		{
			/* Failure costs a turn. */
			msg_print("You remain stuck in the pit.");
			return (FALSE);
		}
		else msg_print("You clamber out of the pit.");
	}
	else msg_print("You leap out of the pit.");

	/* We're free! */
	return (TRUE);
}


/*
 * Turn off running, updating visuals as necessary
 */
void cancel_running()
{
	/* Cancel */
	p_ptr->running = 0;

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update object list and monster list */
	p_ptr->window |= (PW_O_LIST | PW_M_LIST);
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

	int temp;
	int y, x;

	/* Permit the player to move? */
	bool can_move = FALSE;


	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Character is stuck in a pit */
	if (cave_pit_trap(py, px))
	{
		/* Lose turn unless we escape */
		if (!escape_pit()) return;
	}

	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack.  Usually do not move. */
		if (py_attack(y, x)) return;
	}


	/* Attempt to alter known doors and visible traps */
	if ((!p_ptr->confused) &&
	    ((cave_known_closed_door(y, x)) || (cave_visible_trap(y, x))))
	{
		bool confirmed = FALSE;

		/* Check for nasty, visible traps */
		int num = nasty_traps(y, x, 1);

		/* Automatic disarming (if more than 1 trap, can be cancelled). */
		if ((num > 1) ||
			((num == 1) && (p_ptr->command_rep) && (!p_ptr->running)))
		{
			confirmed = TRUE;
		}

		/* There is one trap.  Allow various options. */
		else if (num == 1)
		{
			/* Disturb the character */
			disturb(0, 0);

			/* Traps are tricky -- we'll want to find out what to do */
			confirmed = get_check("Attempt to disarm this trap?");

			/* Nope, I don't want to disarm this trap. */
			if (!confirmed)
			{
				bool test = get_check("Walk onto this trap and set it off?");

				/* Character didn't want to do anything. */
				if (!test)
				{
					/* Hack -- Use no energy */
					p_ptr->energy_use = 0;

					/* Hack -- cancel movement */
					return;
				}

				/* If we are sure we want to, walk onto the trap. */
				msg_print("Walking onto the trap...");
			}
		}

		/* Known closed door -- unless in wraithform */
		else if ((!p_ptr->wraithform) &&
		         (cave_known_closed_door(y, x)) && (cave_info[y][x] & (CAVE_MARK)))
		{
			/* It's hard to get into trouble with an untrapped door */
			confirmed = TRUE;
		}

		/* Attempt to alter the grid */
		if (confirmed)
		{
			/* Not already repeating */
			if (!p_ptr->command_rep)
			{
				/* Hack -- auto-repeat */
				if (p_ptr->command_arg <= 0)
				{
					/* Keep moving in this direction */
					p_ptr->command_cmd = ';';
					p_ptr->command_dir = dir;

					/* Stop any run */
					cancel_running();

					/* Repeat 99 times */
					p_ptr->command_rep = 99;

					/* Reset the command count */
					p_ptr->command_arg = 0;
				}
			}

			/* Alter */
			do_cmd_alter(FALSE);

			/* Turn over */
			return;
		}
	}


	/* Some terrain is usually impassable for the player */
	if (!cave_passable_bold(y, x))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Special case - Allow wraiths to go through some walls */
		if (p_ptr->wraithform)
		{
			/* Nobody can go through solid walls */
			if (cave_permwall(y, x))
			{
				message(MSG_HITWALL, 0, "You feel a solid wall blocking your way.");

				/* Hack -- Use no energy */
				p_ptr->energy_use = 0;
			}

			/* Wraiths suffer some hurt from moving through walls.  -TY- */
			else
			{
				int dam = 1 + div_round(p_ptr->power, 10);
				if (dam > p_ptr->chp) dam = p_ptr->chp;

				if (take_hit(dam, 0, "Your molecules feel disrupted!",
					"becoming one with a wall")) return;

				if (!p_ptr->leaving) can_move = TRUE;
			}
		}

		/* If not in wraithform, ordinary non-passables are also a barrier */
		else
		{
			/* Known closed door */
			if (cave_known_closed_door(y, x))
			{
				/* Already known */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					message(MSG_HITWALL, 0, "There is a door blocking your way.");

					/* Hack -- Use no energy */
					if (!p_ptr->confused) p_ptr->energy_use = 0;
				}
				/* Not known */
				else
				{
					message(MSG_HITWALL, 0, "You feel a door blocking your way.");
					cave_info[y][x] |= (CAVE_MARK);
					lite_spot(y, x);
				}
			}

			/* Wall (or secret door) */
			else if (cave_wall_bold(y, x))
			{
				/* Already known */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					message(MSG_HITWALL, 0, "There is a wall in your way.");

					/* Hack -- Use no energy */
					if (!p_ptr->confused) p_ptr->energy_use = 0;
				}
				/* Not known */
				else
				{
					message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
					cave_info[y][x] |= (CAVE_MARK);
					lite_spot(y, x);
				}
			}

			/* Special non-passable */
			else
			{
				/* Message -- should be elaborated later */
				msg_format("You cannot cross %s.",
					f_name + f_info[cave_feat[y][x]].name);

				/* Already known */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					/* Hack -- Use no energy */
					if (!p_ptr->confused) p_ptr->energy_use = 0;
				}
				/* Not known */
				else
				{
					cave_info[y][x] |= (CAVE_MARK);
					lite_spot(y, x);
				}
			}
		}
	}

	/* Terrain in new grid is passable. */
	else
	{
		/* Make a sound */
		sound(MSG_WALK);

		/*** Handle traversable terrain.  ***/
		switch (cave_feat[y][x])
		{
			case FEAT_RUBBLE:
			{
				/* Characters in wraithform move easily through rubble. */
				if (p_ptr->wraithform) can_move = TRUE;

				/* Require four turns to get through rubble */
				else if (p_ptr->crossing_moves >= 3)
				{
					can_move = TRUE;
				}
				else
				{
					cancel_running();

					if (p_ptr->crossing_dir == dir)
					{
						p_ptr->crossing_moves++;
					}
					else
					{
						p_ptr->crossing_dir = dir;
						p_ptr->crossing_moves = 1;
					}

					/* Automate follow-up movement commands, if not disturbed. */
					p_ptr->command_cmd = ';';
					p_ptr->command_rep = 2;
					p_ptr->command_dir = dir;
				}

				break;
			}

			case FEAT_TREE:
			{
				/* Nature magic-users slip easily under trees. */
				if ((p_ptr->realm == DRUID)) can_move = TRUE;

				/* Characters in wraithform move easily through trees */
				else if (p_ptr->wraithform) can_move = TRUE;

				/* Ents and Woses can move through trees easily */
				else if (p_ptr->prace == RACE_ENT) can_move = TRUE;
				else if (p_ptr->prace == RACE_WOSES) can_move = TRUE;

				/* Require two turns to get through trees */
				else if (p_ptr->crossing_moves)
				{
					can_move = TRUE;
				}
				else
				{
					if (p_ptr->crossing_dir == dir)
					{
						p_ptr->crossing_moves++;
					}
					else
					{
						p_ptr->crossing_dir = dir;
						p_ptr->crossing_moves = 1;
					}

					/* Automate 2nd movement command, if not disturbed. */
					p_ptr->command_cmd = ';';
					p_ptr->command_rep = 1;
					p_ptr->command_dir = dir;
				}

				break;
			}

			case FEAT_WATER:
			{
				/* Characters in wraithform move easily over water. */
				if (p_ptr->wraithform) can_move = TRUE;

				/* Cannot cross with an over-heavy burden. */
				else if (p_ptr->total_weight >=
					adj_str_wgt[p_ptr->stat_ind[A_STR]] * 50)
				{
					can_move = FALSE;
					msg_print("You dare not cross carrying so much weight.");

					/* Hack -- Use no energy */
					p_ptr->energy_use = 0;

					/* Stop any run. */
					disturb(0, 0);
				}

				/* Require two turns to get through water */
				else if (p_ptr->crossing_moves)
				{
					can_move = TRUE;
				}
				else
				{
					if (p_ptr->crossing_dir == dir)
					{
						p_ptr->crossing_moves++;
					}
					else
					{
						p_ptr->crossing_dir = dir;
						p_ptr->crossing_moves = 1;
					}

					/* Automate 2nd movement command, if not disturbed. */
					p_ptr->command_cmd = ';';
					p_ptr->command_rep = 1;
					p_ptr->command_dir = dir;
				}

				break;
			}

			case FEAT_LAVA:
			{
				/* Assume player will continue. */
				temp = TRUE;

				/* Smart enough to stop running. */
				if (p_ptr->running)
				{
					/* Stop the run */
					cancel_running();

					/* Hack -- Use no energy */
					p_ptr->energy_use = 0;

					temp = FALSE;
				}

				/* Smart enough to sense trouble. */
				else if ((!p_ptr->resist_fire) &&
				         (!p_ptr->oppose_fire) &&
				         (!p_ptr->immune_fire) &&
				         (!p_ptr->confused))
				{
					if (!get_check("The heat of the lava scalds you!  Really enter?"))
					{
						temp = FALSE;
					}
				}

				/* Enter if OK or confirmed. */
				if (temp)
				{
					/* Can always cross. */
					can_move = TRUE;

					/* Feather fall makes one light-footed. */
					if (p_ptr->ffall) temp = rand_range(50, 100);
					else temp = rand_range(125, 250);

					/* Will take serious fire damage. */
					fire_dam(temp, 0, "You are burnt!",
					   "being burnt to a cinder in molten lava");
				}
				break;
			}

			default:
			{
				/* All other terrain can be traversed normally. */
				can_move = TRUE;
			}
		}
	}

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* If the player can move, handle various things. */
	if (can_move)
	{
		/* Move player */
		monster_swap(py, px, y, x);

		/* Handle "leaving" */
		if (p_ptr->leaving) return;

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;

		/* Spontaneous searching for traps and doors */
		if ((p_ptr->skill_srh >= 30 + p_ptr->depth) ||
			 (rand_int(30 + p_ptr->depth) < p_ptr->skill_srh))
		{
			search();
		}

		/* Spontaneous searching for essences */
		/* Skill competes with depth */
		if (rand_int(25 + p_ptr->depth) < get_skill(S_INFUSION, 0, 100) ||
			rand_int(25 + p_ptr->depth) < get_skill(S_ALCHEMY, 0, 100))
		{
			search_essence(FALSE);
		}

		/* Handle store doors */
		if (cave_shop_bold(y, x))
		{
			/* Enter the store */
			do_cmd_store();
		}

		/* Normal grid */
		else
		{
			/* Handle objects (later) */
			if (do_pickup) p_ptr->notice |= (PN_PICKUP1);
			else           p_ptr->notice |= (PN_PICKUP0);
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
	if (!cave_wall_bold(y, x)) return (FALSE);

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
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
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
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
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
 * o@x       1
 * ########### ######
 * #2          #
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
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
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
 *    ###             o##
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
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
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
static const byte cycle[] =
{1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1};

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static const byte chome[] =
{0, 8, 9, 10, 7, 0, 11, 6, 5, 4};



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diagonal entry into corridors.
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
	if (see_wall(cycle[i + 1], py, px))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i + 1], row, col))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i - 1], py, px))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i - 1], row, col))
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
 * Return TRUE if the running should be stopped.
 */
static bool run_test(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	object_type *o_ptr;

	int row, col, row_next, col_next;
	int i, max;
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
			if (m_ptr->ml && !(m_ptr->mflag & (MFLAG_MIME))) return (TRUE);
		}

		/* Look ahead */
		row_next = row + ddy[prev_dir];
		col_next = col + ddx[prev_dir];

		/* Visible monsters about to become adjacent also abort running  -LM- */
		if ((in_bounds(row_next, col_next)) &&
		    (cave_m_idx[row_next][col_next] > 0))
		{
			monster_type *m_ptr =
				&m_list[cave_m_idx[row_next][col_next]];

			/* Visible monster */
			if (m_ptr->ml && !(m_ptr->mflag & (MFLAG_MIME))) return (TRUE);
		}

		/* Visible objects abort running */
		for (o_ptr = get_first_object(row, col); o_ptr;
		     o_ptr = get_next_object(o_ptr))
		{
			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}

		/* Visible traps abort running */
		if (cave_visible_trap(row, col)) return (TRUE);


		/* Check known grids */
		if (cave_info[row][col] & (CAVE_MARK))
		{
			bool ignore = FALSE;

			/* We always ignore walls */
			if (cave_wall_bold(row, col))
			{
				ignore = TRUE;
			}

			/* We may or may not ignore stair and entrances */
			else if (cave_any_stairs(row, col))
			{
				if (run_ignore_stairs) ignore = TRUE;
			}

			/* We may or may not ignore closed doors */
			else if (cave_known_closed_door(row, col))
			{
				if (run_ignore_doors) ignore = TRUE;
			}

			/* We ignore all other passable terrain */
			else if (cave_passable_bold(row, col))
			{
				ignore = TRUE;
			}

			/* We notice everything else */
			if (!ignore)
			{
				return (TRUE);
			}
		}


		/* Grid is unknown, or known to be a floor or open door */
		if (!(cave_info[row][col] & (CAVE_MARK)) ||
		    (cave_floor_bold(row, col)) ||
		    (cave_any_door(row, col) && !(cave_closed_door(row, col))))
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

			/* Unknown grid or "runnable" */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
				(!cave_wall_bold(row, col)))
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

			/* Unknown grid or "runnable"  XXX */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			     (!cave_wall_bold(row, col)))
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
		else if (!run_cut_corners)
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
				if (see_nothing(option, row, col) &&
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

	/* Move the player.  Never pick up objects */
	move_player(p_ptr->run_cur_dir, FALSE);
}
