/* File: cmd2.c */

/*
 * Go up and down stairs, toggle sneaking.  Handle chests.  Handle doors,
 * tunnel, disarm, bash, alter a grid, and spike.  Walk, run, stay still,
 * pickup, and rest.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Use an up staircase
 */
void do_cmd_go_up(void)
{
	/* Verify stairs */
	if (!cave_up_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no up staircase here.");
		return;
	}

	/* Ironman */
	if (birth_ironman)
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of up staircases.");

	/* Create a way back (usually) */
	p_ptr->create_stair = FEAT_MORE;

	/* New depth */
	p_ptr->depth--;

	/* Go up another level if allowed */
	if ((cave_feat[p_ptr->py][p_ptr->px] == FEAT_LESS2) &&
	    (!quest_check(p_ptr->depth)) && (p_ptr->depth > 0))
	{
		p_ptr->depth--;

		/* Create a way back (usually) */
		p_ptr->create_stair = FEAT_MORE2;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Use a down staircase
 */
void do_cmd_go_down(void)
{
	/* Verify stairs */
	if (!cave_down_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of down staircases.");

	/* Create a way back (usually) */
	p_ptr->create_stair = FEAT_LESS;

	/* New level */
	p_ptr->depth++;

	/* Go down another level if allowed */
	if ((cave_feat[p_ptr->py][p_ptr->px] == FEAT_MORE2) &&
	    (!quest_check(p_ptr->depth)) && (p_ptr->depth < MAX_DEPTH - 1))
	{
		p_ptr->depth++;

		/* Create a way back (usually) */
		p_ptr->create_stair = FEAT_LESS2;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Hack -- toggle sneaking mode
 */
void do_cmd_sneaking(void)
{
	/* Stop sneaking */
	if (p_ptr->sneaking)
	{
		/* Clear the sneaking flag */
		p_ptr->sneaking = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start sneaking */
	else if (can_use_talent(TALENT_SNEAKING) > 0)
	{
		/* Set the sneaking flag */
		p_ptr->sneaking = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
	else
	{
		msg_format("You are not good enough at %s to sneak effectively.",
			skill_info[talent_info[TALENT_SNEAKING].skill].name);
	}
}


/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		if (!o_ptr->marked) continue;

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/*
 * A function that returns the tval of the items that will be generated
 * when a chest is opened.  -LM-
 */
static byte get_choice(void)
{
	byte choice;

	choice = randint(100);

	if (choice < 3)   return (TV_SHOT);
	if (choice < 7)   return (TV_ARROW);
	if (choice < 11)  return (TV_BOLT);
	if (choice < 14)  return (TV_BOW);
	if (choice < 18)  return (TV_HAFTED);
	if (choice < 22)  return (TV_POLEARM);
	if (choice < 26)  return (TV_SWORD);
	if (choice < 31)  return (TV_BOOTS);
	if (choice < 36)  return (TV_GLOVES);
	if (choice < 41)  return (TV_HELM);
	if (choice < 46)  return (TV_CROWN);
	if (choice < 51)  return (TV_SHIELD);
	if (choice < 56)  return (TV_CLOAK);
	if (choice < 60)  return (TV_SOFT_ARMOR);
	if (choice < 64)  return (TV_HARD_ARMOR);
	if (choice < 69)  return (TV_SCROLL);
	if (choice < 74)  return (TV_POTION);
	if (choice < 79)  return (TV_RING);
	if (choice < 84)  return (TV_AMULET);
	if (choice < 89)  return (TV_WAND);
	if (choice < 94)  return (TV_STAFF);
	if (choice < 99)  return (TV_ROD);
	else return (TV_COMPONENT);
}

/*
 * Allocate objects upon opening a chest.
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * In Oangband, chests are nice finds.  Small chests distribute 3-4 items,
 * while large chests can distribute 5-6.  Item types are biased to be
 * useful for the character, and they can frequently be of good quality
 * (or better).   Code in object2.c helps these items be even better. -LM-
 *
 * The "value" of the items in a chest is based on the "power" of the chest,
 * which is in turn based on the level on which the chest is generated.
 */
static void chest_death(bool scattered, int y, int x, s16b o_idx)
{
	int number, i;
	bool obj_success = FALSE;
	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;


	/* Access chest */
	o_ptr = &o_list[o_idx];

	/* Determine how much to drop. */
	if (o_ptr->sval >= SV_CHEST_MIN_LARGE) number = rand_range(5, 6);
	else number = rand_range(3, 4);

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Set object level to the power of the chest */
	object_level = ABS(o_ptr->pval);

	/* Select an item type that the chest will disperse. */
	required_tval = get_choice();

	/* Drop some objects */
	for (i = 0; i < number; i++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/*
		 * Make an object with a specified tval.  Grant a possibility for
		 * items to be forced good, or even great.  With the new definitions
		 * of goodness, this can make for quite interesting loot.  -LM-
		 */
		switch (required_tval)
		{
			case TV_HARD_ARMOR:
			case TV_SOFT_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_BOW:
			case TV_SWORD:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_DIGGING:
			case TV_SHOT:
			case TV_BOLT:
			case TV_ARROW:
			{
				if (rand_int(200) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, TRUE, TRUE);
					break;
				}

				else if (rand_int(40) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
					break;
				}
				else
				{
					obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
					break;
				}
			}

			case TV_MAGIC_BOOK:
			case TV_PRAYER_BOOK:
			case TV_NATURE_BOOK:
			case TV_DARK_BOOK:
			{
				if (rand_int(80) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
				}
				else
				{
					obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				}

				break;
			}

			case TV_SCROLL:
			case TV_POTION:
			case TV_RING:
			case TV_AMULET:
			case TV_WAND:
			case TV_STAFF:
			case TV_ROD:
			{
				if (rand_int(100) < (object_level - 10) / 2)
				{
					obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
				}

				else
				{
					obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				}

				break;
			}

			default:
			{
				obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				break;
			}
		}

		/* If no object was made, we need to try another tval.  XXX */
		if ((!obj_success) && (i == 0))
		{
			required_tval = get_choice();
			i--;
		}

		/* Otherwise, we can place the object */
		else
		{
			/* If chest scatters its contents, pick any floor square. */
			if (scattered)
			{
				int j;

				for (j = 0; j < 200; j++)
				{
					/* Pick a totally random spot. */
					y = rand_int(dungeon_hgt);
					x = rand_int(dungeon_wid);

					/* Grid must be capable of (safely) containing an object. */
					if (!cave_object_allowed(y, x)) continue;

					/* Place the object there. */
					drop_near(i_ptr, -1, y, x);

					/* Done. */
					break;
				}
			}

			/* Normally, drop object near the chest. */
			else drop_near(i_ptr, -1, y, x);
		}
	}

	/* Clear this global variable, to avoid messing up object generation. */
	required_tval = 0;

	/* Reset the object level */
	object_level = p_ptr->depth;


	/* Clear restriction */
	get_obj_num_hook = NULL;

	/* Prepare allocation table */
	get_obj_num_prep();


	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);
}


/*
 * Chests have traps too.  High-level chests can be very dangerous, no
 * matter what level they are opened at.  Various traps added in Oangband.
 * -LM-
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
	int i, trap, nasty_tricks_count;

	object_type *o_ptr = &o_list[o_idx];

	/* Summon level is the same as chest power. */
	int summon_level = o_ptr->pval;

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
		take_hit(damroll(1, 4), 0, "A small needle has pricked you!",
			"a poison needle");
		(void)do_dec_stat(A_STR, 1, FALSE, "You feel weakened!", NULL);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		take_hit(damroll(1, 4), 0, "A small needle has pricked you!",
			"a poison needle");
		(void)do_dec_stat(A_CON, 1, FALSE, "Your health is damaged!",
			"Your body resists the effects of the disease.");
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		/* Disease */
		if (randint(o_ptr->pval) >= 30)
		{
			int dummy = o_ptr->pval;

			msg_print("The lock ejects a cloud of spores!");

			/* Cause disease, but don't physically hurt the character */
			disease(&dummy);
		}

		/* Poisoning */
		else
		{
			msg_print("A puff of green gas surrounds you!");
			if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
			{
				if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
				{
					(void)set_poisoned(p_ptr->poisoned + rand_range(20, 30));
				}
				else
				{
					(void)set_poisoned(p_ptr->poisoned + rand_range(10, 15));
				}
			}
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you.");
		if (!p_ptr->free_act)
		{
			msg_print("You choke and pass out!");
			(void)set_paralyzed(p_ptr->paralyzed + rand_range(20, 30));
		}
		else
		{
			msg_print("You are unaffected.");
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = rand_range(3, 6);
		msg_print("You are enveloped in a cloud of smoke!");
		for (i = 0; i < num; i++)
		{
			(void)summon_specific(y, x, FALSE, summon_level, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		take_hit(damroll(5, 8), 0, NULL, "an exploding chest");
	}

	/* Scatter contents. */
	if (trap & (CHEST_SCATTER))
	{
		msg_print("The contents of the chest scatter all over the dungeon!");
		chest_death(TRUE, y, x, o_idx);
		o_ptr->pval = 0;
	}

	/* Elemental summon. */
	if (trap & (CHEST_E_SUMMON))
	{
		msg_print("Elemental beings appear to protect their treasures!");
		for (i = 0; i < rand_range(4, 8); i++)
		{
			summon_specific(y, x, FALSE, summon_level, SUMMON_ELEMENTAL);
		}
	}

	/* Force clouds, then summon birds. */
	if (trap & (CHEST_BIRD_STORM))
	{
		msg_print("A storm of birds swirls around you!");

		for (i = 0; i < rand_range(3, 6); i++)
		{
			(void)explosion(0, 7, y, x, o_ptr->pval / 3, GF_FORCE);
		}

		for (i = 0; i < rand_range(4, 8); i++)
		{
			summon_specific(y, x, TRUE, summon_level, SUMMON_BIRD);
		}
	}

	/* Various colorful summonings. */
	if (trap & (CHEST_H_SUMMON))
	{
		/* Summon demons. */
		if (one_in_(4))
		{
			msg_print("Demons materialize in clouds of fire and brimstone!");

			for (i = 0; i < rand_range(3, 6); i++)
			{
				(void)explosion(0, 4, y, x, o_ptr->pval / 3, GF_FIRE);
			}

			for (i = 0; i < rand_range(3, 6); i++)
			{
				summon_specific(y, x, FALSE, summon_level, SUMMON_DEMON);
			}
		}

		/* Summon dragons. */
		else if (one_in_(3))
		{
			msg_print("Draconic forms loom out of the darkness!");

			for (i = 0; i < rand_range(3, 4); i++)
			{
				summon_specific(y, x, FALSE, summon_level, SUMMON_DRAGON);
			}
		}

		/* Summon hybrids. */
		else if (one_in_(2))
		{
			msg_print("Creatures strange and twisted assault you!");

			for (i = 0; i < rand_range(4, 8); i++)
			{
				summon_specific(y, x, FALSE, summon_level, SUMMON_HYBRID);
			}
		}

		/* Summon vortices (scattered) */
		else
		{
			msg_print("Vortices coalesce and wreak destruction!");

			for (i = 0; i < rand_range(3, 7); i++)
			{
				summon_specific(y, x, TRUE, summon_level, SUMMON_VORTEX);
			}
		}
	}

	/* Dispel player. */
	if (trap & (CHEST_RUNES_OF_EVIL))
	{
		/* Message. */
		msg_print("Hideous voices bid: 'Let the darkness have thee!'");

		/* Determine how many nasty tricks can be played. */
		nasty_tricks_count = rand_range(4, 6);

		/* This is gonna hurt... */
		for (; nasty_tricks_count > 0; nasty_tricks_count--)
		{
			/* ...but a high saving throw does help a little. */
			if (randint(2 * o_ptr->pval) > p_ptr->skill_sav)
			{
				int choice = rand_int(6);

				if (choice == 0)
				{
					take_hit(damroll(5, 20), 0, NULL, "a chest dispel-player trap");
				}
				else if (choice == 1)
				{
					(void)set_cut(p_ptr->cut + 200);
				}
				else if (choice == 2)
				{
					if (!p_ptr->free_act)
						(void)set_paralyzed(p_ptr->paralyzed + rand_range(4, 8));
					else
						(void)set_stun(p_ptr->stun + rand_range(10, 110));
				}
				else if (choice == 3)
				{
					apply_disenchant(0);
				}
				else if (choice == 4)
				{
					(void)do_dec_stat(A_STR, 1, FALSE, NULL, NULL);
					(void)do_dec_stat(A_DEX, 1, FALSE, NULL, NULL);
					(void)do_dec_stat(A_CON, 1, FALSE, NULL, NULL);
					(void)do_dec_stat(A_INT, 1, FALSE, NULL, NULL);
					(void)do_dec_stat(A_WIS, 1, FALSE, NULL, NULL);
					(void)do_dec_stat(A_CHR, 1, FALSE, NULL, NULL);
				}
				else
				{
					(void)explosion(0, 4, y, x, 4 * o_ptr->pval, GF_NETHER);
				}
			}
		}
	}
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
	int i, j;

	bool flag = TRUE;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind    || no_light())   i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Difficulty rating.  Tweaked to compensate for higher pvals. */
		j = i - 2 * o_ptr->pval / 3;

		/* High disarming improves success with lower-level locks */
		if (2 * o_ptr->pval / 3 <= get_skill(S_DISARM, 0, 100))
		{
			if (j < get_skill(S_DISARM, 0, 100))
			    j = get_skill(S_DISARM, 0, 100);
		}

		/* Success -- May still have traps */
		if (rand_int(100) < j)
		{
			msg_print("You have picked the lock.");
			gain_exp(1, S_DISARM);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");
		}
	}

	/* Allowed to open */
	if (flag)
	{
		/* Apply chest traps, if any */
		chest_trap(y, x, o_idx);

		/* Let the chest drop items */
		chest_death(FALSE, y, x, o_idx);
	}

	/* Result */
	return (more);
}


/*
 * Attempt to disarm the chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, s16b o_idx)
{
	int skill, chance, diff;

	bool more = FALSE;
	bool safe = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Get disarm skill */
	skill = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind    || no_light())   skill /= 10;
	if (p_ptr->confused || p_ptr->image) skill /= 10;


	/* The difficulty of the traps on a chest depends on pval */
	diff = o_ptr->pval;

	/* Extract the disarm probability */
	chance = skill - diff;

	/* Hack -- always have a chance */
	if (chance < 1) chance = 1;

	/* High disarming renders one safe from lower-level traps */
	if (diff <= get_skill(S_DISARM, 0, 100))
	{
		safe = TRUE;
	}


	/* Must find the trap first. */
	if (!object_known_p(o_ptr))
	{
		msg_print("I don't see any traps.");
	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
		msg_print("The chest is not trapped.");
	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
		msg_print("The chest is not trapped.");
	}

	/* Success */
	else if ((diff == 0) || (chance >= 100) || (rand_int(100) < chance))
	{
		msg_print("You have disarmed the chest.");
		gain_exp(randint(o_ptr->pval + (o_ptr->pval * o_ptr->pval / 15)),
			S_DISARM);

		/* Hack -- mark the chest as being untrapped */
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if (safe || randint(chance) > 5)
	{
		/* We may keep trying */
		more = TRUE;
		if (flush_failure) flush();
		msg_print("You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		msg_print("You set off a trap!");
		chest_trap(y, x, o_idx);
	}

	/* Result */
	return (more);
}

/*
 * Return TRUE if the given feature is a closed door
 */
static bool is_closed(int feat)
{
	return ((feat >= FEAT_DOOR_HEAD) &&
	        (feat <= FEAT_DOOR_TAIL));
}


/*
 * Return the number of doors/traps around (or under) the character.
 */
static int count_feats(int *y, int *x, bool (*test)(int feat), bool under)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Option -- search under the character */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

		/* Not looking for this feature */
		if (!((*test)(cave_feat[yy][xx]))) continue;


		/* Count it */
		count++;

		/* Remember the location of the last feature of this type found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return (count);
}

/*
 * Return the number of chests around (or under) the character.  -TNB-
 * If requested, count only trapped chests.
 */
static int count_chests(int *y, int *x, bool trapped)
{
	int d, count, o_idx;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* No (visible) chest is there */
		if ((o_idx = chest_check(yy, xx)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Already open */
		if (o_ptr->pval == 0) continue;

		/* Option -- search for trapped chests only */
		if (trapped)
		{
			/* No known traps */
			if (!object_known_p(o_ptr)) continue;
			if (o_ptr->pval < 0) continue;
			if (!chest_traps[o_ptr->pval]) continue;
		}

		/* OK */
		count++;

		/* Remember the location of the last chest found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return (count);
}

/*
 * Convert an adjacent location to a direction.
 */
static int coords_to_dir(int y, int x)
{
	int d[3][3] =
	{
		{ 7, 4, 1 },
		{ 8, 5, 2 },
		{ 9, 6, 3 }
	};
	int dy, dx;

	dy = y - p_ptr->py;
	dx = x - p_ptr->px;

	/* Ensure legal table access */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return (d[dx + 1][dy + 1]);
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		message(MSG_NOTHING_TO_OPEN, 0, "You see nothing there to open.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
	int skill, j;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_open_test(y, x)) return (FALSE);


	/* Jammed door */
	if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		skill = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind    || no_light())   skill /= 10;
		if (p_ptr->confused || p_ptr->image) skill /= 10;

		/* Extract the lock power  (change the coefficient as needed) XXX */
		j = (cave_feat[y][x] - FEAT_DOOR_HEAD) * 6;

		/* Always have a chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			message(MSG_OPENDOOR, 0, "You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Experience -- unless able to lock doors */
			if (get_skill(S_BURGLARY, 0, 100) < LEV_REQ_LOCK_DOOR)
				gain_exp(1, S_DISARM);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");

			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_OPENDOOR);
	}

	/* Result */
	return (more);
}



/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 * Hack -- doors are only worth one point if you aren't a burglar who
 * can lock doors.  XXX
 */
void do_cmd_open(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	int num_doors, num_chests;

	/* Count closed doors */
	num_doors = count_feats(&y, &x, is_closed, FALSE);

	/* Count chests (locked) */
	num_chests = count_chests(&y, &x, FALSE);

	/* See if only one target  -TNB- */
	if ((num_doors + num_chests) == 1)
	{
		p_ptr->command_dir = coords_to_dir(y, x);
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !do_cmd_open_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chest */
		o_idx = chest_check(y, x);
	}


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

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Chest */
	if (o_idx)
	{
		/* Open the chest */
		more = do_cmd_open_chest(y, x, o_idx);
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(y, x);
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "closed"
 */
static bool do_cmd_close_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require open/broken door */
	if ((cave_feat[y][x] != FEAT_OPEN) &&
	    (cave_feat[y][x] != FEAT_BROKEN))
	{
		/* Message */
		msg_print("You see nothing there to close.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return (FALSE);


	/* Broken door */
	if (cave_feat[y][x] == FEAT_BROKEN)
	{
		/* Message */
		msg_print("The door appears to be broken.");
		p_ptr->energy_use = 0;
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


#if 0  /* Interacts poorly with door locking */

	/* Handle a single open door  -TNB- */
	if (count_feats(&y, &x, is_open, FALSE) == 1)
	{
		p_ptr->command_dir = coords_to_dir(y, x);
	}
#endif

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


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

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}


	/* Close door */
	more = do_cmd_close_aux(y, x);

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}



/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
	{
		/* Message */
		msg_print("You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
static bool twall(int y, int x)
{
	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Sound */
	sound(MSG_DIG);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Result */
	return (TRUE);
}



/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Uses "twall" (above) to do all "terrain feature changing".
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;

	char buf[80];

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);


	/* Get base name of digger */
	if (inventory[INVEN_WIELD].k_idx)
		strip_name(buf, inventory[INVEN_WIELD].k_idx);
	else
		strcpy(buf, "bare hands");


	/* Permanent rock */
	if (cave_feat[y][x] >= FEAT_PERM_EXTRA &&
	    cave_feat[y][x] < FEAT_SHOP_HEAD)
	{
		msg_print("This seems to be permanent rock.");
	}

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
		/* No chance */
		if (p_ptr->skill_dig <= 40)
		{
			msg_format("Your %s make%s no impression on the granite wall.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		if ((p_ptr->skill_dig > rand_range(40, 1640)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if (cave_feat[y][x] >= FEAT_MAGMA)
	{
		bool okay = FALSE;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (cave_feat[y][x] >= FEAT_MAGMA_H)
		{
			gold = TRUE;
		}

		/* Extract "quartz" flag XXX XXX XXX */
		if ((cave_feat[y][x] - FEAT_MAGMA) & 0x01)
		{
			hard = TRUE;
		}

		/* Quartz */
		if (hard)
		{
			/* No chance */
			if (p_ptr->skill_dig <= 25)
			{
				msg_format("Your %s make%s no impression on the quartz vein.",
					buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
				return (FALSE);
			}

			okay = (p_ptr->skill_dig > rand_range(25, 625));
		}

		/* Magma */
		else
		{
			/* No chance */
			if (p_ptr->skill_dig <= 20)
			{
				msg_format("Your %s make%s no impression on the magma intrusion.",
					buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
				return (FALSE);
			}

			okay = (p_ptr->skill_dig > rand_range(20, 420));
		}

		/* Success */
		if (okay && twall(y, x))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(y, x);

				/* Message */
				msg_print("You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				msg_print("You have finished the tunnel.");
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			msg_print("You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			msg_print("You tunnel into the magma intrusion.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skill_dig > rand_int(200)) && twall(y, x))
		{
			/* Message */
			msg_print("You have removed the rubble.");

			/* Hack -- place an object */
			if (one_in_(33))
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE, FALSE);

				/* Observe new object */
				if ((cave_o_idx[y][x] != 0) && (player_can_see_bold(y, x)))
				{
					msg_print("You have found something!");
				}
			}

			/* Hack -- place a boulder (no messages) */
			else if (one_in_(2))
			{
				make_boulder(y, x, p_ptr->depth);
			}
		}

		else
		{
			/* Message, keep digging */
			msg_print("You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (cave_feat[y][x] >= FEAT_SECRET)
	{
		/* No chance */
		if (p_ptr->skill_dig <= 30)
		{
			msg_format("Your %s make%s no impression on the granite wall.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		else if ((p_ptr->skill_dig > rand_range(30, 1230)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (one_in_(4)) search();
		}
	}

	/* Doors */
	else
	{
		/* No chance */
		if (p_ptr->skill_dig <= 30)
		{
			msg_format("Your %s make%s no impression on the door.",
				buf, inventory[INVEN_WIELD].k_idx ? "s" : "");
			return (FALSE);
		}

		/* Tunnel */
		else if ((p_ptr->skill_dig > rand_range(30, 1230)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunneling */
			msg_print("You tunnel into the door.");
			more = TRUE;
		}
	}

	/* Result */
	return (more);
}


/*
 * Tunnel through "walls" (including rubble and secret doors)
 *
 * Digging is very difficult without a digging tool, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Oops */
	if (!do_cmd_tunnel_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


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

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Tunnel through walls */
	more = do_cmd_tunnel_aux(y, x);

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Return the number of traps around (or under) the character.
 */
static int count_disarmable_traps(int *y, int *x)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;

		/* Check the trap marker */
		if (!(cave_info[yy][xx] & (CAVE_TRAP))) continue;

		/* Grid contains disarmable traps */
		if (has_disarmable_trap(yy, xx))
		{
			/* Count it */
			count++;

			/* Remember the location of the last feature of this type found */
			*y = yy;
			*x = xx;
		}
	}

	/* All done */
	return (count);
}


/*
 * Perform the basic "disarm" command on a trap or glyph.
 *
 * Assume there is no monster blocking the destination (tested by
 * do_cmd_disarm).  Traps now have level-dependent difficulty.
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_trap(int y, int x)
{
	int skill, chance, diff;

	cptr name;

	bool more = FALSE;
	bool safe = FALSE;

	int idx;
	trap_type *t_ptr;


	/* Paranoia -- must be fully in bounds */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Verify legality */
	if (!has_disarmable_trap(y, x)) return (FALSE);


	/* Choose a trap to disarm (automatic if only one) */
	get_trap(y, x, &idx);

	/* No trap was chosen */
	if (idx < 0) return (FALSE);

	/* Get the trap */
	t_ptr = &t_list[idx];

	/* Get the trap name */
	name = t_kind_info[t_ptr->t_idx].name;


	/* Cannot disarm pit if inside it (unless you have feather fall) */
	if ((t_ptr->t_idx == TRAP_PIT) && (p_ptr->py == y) &&
	    (p_ptr->px == x) && (!p_ptr->ffall))
	{
		msg_print("You cannot disarm the pit you're in.");
		return (FALSE);
	}


	/* Get disarm skill */
	skill = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind    || no_light())   skill /= 10;
	if (p_ptr->confused || p_ptr->image) skill /= 10;


	/* Calculate trap difficulty, which is simply the trap depth. */
	diff = p_ptr->depth;
	if (diff > 100) diff = 100;
	if (diff <   1) diff =   1;

	/* Some traps are easy to disarm. */
	if (t_kind_info[t_ptr->t_idx].flags & (TRAP_EASY_DISARM)) diff = 0;

	/* Extract the disarm probability */
	chance = skill - diff;

	/* Hack -- always have some chance */
	if (chance < 10) chance = 10;

	/* High disarming renders one safe from lower-level traps */
	if (diff <= get_skill(S_DISARM, 0, 100))
	{
		safe = TRUE;
	}

	/* Success */
	if ((diff == 0) || (chance >= 100) || (rand_int(100) < chance))
	{
		/* Special message for glyphs. */
		if (t_ptr->t_idx == TRAP_GLYPH)
			msg_format("You have desanctified the %s.", name);

		/* Normal message otherwise */
		else msg_format("You have disarmed the %s.", name);

		/* Reward */
		if (diff)
		{
			/* Trap must be allowed to give exp */
			if (!(t_ptr->flags & (TRAP_NO_EXP)))
			{
				gain_exp(randint(diff + (diff * diff / 15)), S_DISARM);
			}
		}

		/* Remove the trap */
		remove_trap(y, x, idx);
	}

	/* Failure -- Keep trying */
	else if (safe || randint(chance) > 5)
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msg_format("You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msg_format("You set off the %s!", name);

		/* Hit the trap */
		hit_trap(-1, y, x);
	}

	/* Result */
	return (more);
}



/*
 * Disarms a trap, a glyph, or a chest.
 */
void do_cmd_disarm(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	int num_traps, num_chests;

	/* Count disarmable traps */
	num_traps = count_disarmable_traps(&y, &x);

	/* Count chests (trapped) */
	num_chests = count_chests(&y, &x, TRUE);

	/* Handle a single visible trap or trapped chest  -TNB- */
	if (num_traps || num_chests)
	{
		if (num_traps + num_chests <= 1)
			p_ptr->command_dir = coords_to_dir(y, x);
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !has_disarmable_trap(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chests */
		o_idx = chest_check(y, x);
	}


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

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Disarm trap */
	if (has_disarmable_trap(y, x))
	{
		/* Disarm the trap */
		more = do_cmd_disarm_trap(y, x);
	}

	/* Chest */
	else
	{
		/* Disarm the chest */
		more = do_cmd_disarm_chest(y, x, o_idx);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "bashed"
 */
static bool do_cmd_bash_test(int y, int x)
{
	object_type *o_ptr;

	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Scan objects, if any are present */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Chest (first one in the stack) */
		if (o_ptr->tval == TV_CHEST)
		{
			/* Locked or trapped chests can be bashed */
			if (o_ptr->pval > 0)
			{
				return (TRUE);
			}
		}
	}

	/* Require a door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to bash.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "bash" command.  Characters may bash chests and doors.
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x)
{
	object_type *o_ptr;
	int bash, diff, chance;
	bool more = FALSE;

	s16b this_o_idx, next_o_idx = 0;

	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get object */
		o_ptr = &o_list[this_o_idx];

		/* Chest (first one in the stack) */
		if (o_ptr->tval == TV_CHEST)
		{
			/* Locked or trapped chests can be bashed */
			if (o_ptr->pval > 0)
			{
				/* Make a fair amount of noise. */
				add_wakeup_chance = MAX(add_wakeup_chance, 5000);

				/* Hack -- Bash power based on strength (-12 to 8) */
				bash = (p_ptr->stat_ind[A_STR] > 20 ? 20 :
				        p_ptr->stat_ind[A_STR]) - 12;

				/* Good wrestlers get a substantial bonus */
				bash += get_skill(S_WRESTLING, 0, 4);


				/* We are wimpy */
				if (bash < ABS(o_ptr->pval) / 20)
				{
					msg_print("Your weak blows bounce off the chest.");
					return (FALSE);
				}

				/* Compare bash power to chest power */
				chance = (25 + bash * 3) - (ABS(o_ptr->pval) / 10);

				/* Chest is smashed open */
				if (rand_int(100) < chance)
				{
					/* Message */
					message(MSG_OPENDOOR, 0, "You smash open the chest!");

					/* Loot! */
					chest_death(FALSE, y, x, this_o_idx);
				}

				/* Bashing failed -- hurt the chest */
				else
				{
					o_ptr->pval -= 1 + o_ptr->pval / 20;
					if (o_ptr->pval < 0) o_ptr->pval = 0;

					msg_print("The chest is damaged.");
				}
			}

			/* Never automate the bashing of chests. */
			return (FALSE);
		}
	}


	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return (FALSE);

	/* Message */
	msg_print("You smash into the door!");

	/* Make a lot of noise. */
	add_wakeup_chance = 9000;


	/* Hack -- Bash power based on strength  (From 17 to 115) */
	bash = 15 + adj_str_hold[p_ptr->stat_ind[A_STR]];

	/* Good wrestlers get a bonus */
	bash += get_skill(S_WRESTLING, 0, 25);


	/* Extract door power */
	diff = ((cave_feat[y][x] - FEAT_DOOR_HEAD) % 8);

	/* Compare bash power to door power XXX XXX XXX */
	chance = (bash - (diff * 10));

	/* Hack -- always have a chance */
	if (chance < 2) chance = 2;


	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < chance)
	{
		/* Break down the door */
		if (one_in_(2))
		{
			cave_set_feat(y, x, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
		}

		/* Message */
		message(MSG_OPENDOOR, 0, "The door crashes open!");

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(40) < p_ptr->stat_ind[A_DEX] +
		get_skill(S_WRESTLING, 0, 8) + get_skill(S_KARATE, 0, 8))
	{
		/* Message */
		msg_print("The door holds firm.");

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity (and skill) yields coolness */
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		/* Hack -- Lose balance a la paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 3));
	}

	/* Result */
	return (more);
}


/*
 * Bash open a door, success based on character strength
 *
 * For a closed door, pval is positive if locked; negative if stuck.
 *
 * For an open door, pval is positive for a broken door.
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 */
void do_cmd_bash(void)
{
	int y, x, dir;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
	}


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

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}

	/* Bash the door */
	if (!do_cmd_bash_aux(y, x))
	{
		/* Cancel repeat */
		disturb(0, 0);
	}
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors,
 * or, for burglars, set traps, lock doors, and steal money.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The direction of the alter command must be chosen before the player
 * is confused, and the grid in the actual direction must be verified.
 *
 * Note that there are two versions of this command; one with extra
 * power, and one with any "annoyance" removed.  The first should only
 * be activated when one deliberately types the '+' key.
 */
void do_cmd_alter(bool deliberate)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	int feat;

	bool more = FALSE;

	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Original feature */
	feat = cave_feat[y][x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) feat = FEAT_NONE;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


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

	/*
	 * If a monster is present, and visible, a Burglar may steal from it.
	 * Otherwise, the player will simply attack. -LM-
	 */
	if (cave_m_idx[y][x] > 0)
	{
		if ((!p_ptr->schange) &&
		    (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_STEAL))
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

			/* Steal or attack -- never move */
			if (mon_fully_visible(m_ptr)) py_steal(y, x);
			else                    (void)py_attack(y, x);
		}
		else
		{
			(void)py_attack(y, x);
		}
	}

	/* Disarm traps */
	else if (cave_visible_trap(y, x))
	{
		int action = -1;

		/* Monster traps can be loaded or disarmed  -LM- */
		if ((cave_monster_trap(y, x)) &&
		    (!p_ptr->blind) && (!p_ptr->confused))
		{
			/* Cancel repeat */
			disturb(0, 0);

			/* Interact with the trap, note if we want to disarm */
			action = load_trap(y, x);

			/* Allow cancel */
			if (!action)
			{
				p_ptr->energy_use = 0;
				return;
			}
		}

		/* Disarm */
		if (action == -1) more = do_cmd_disarm_trap(y, x);
	}

	/* Tunnel through walls */
	else if (feat >= FEAT_SECRET && feat < FEAT_SHOP_HEAD)
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Open closed doors */
	else if (feat >= FEAT_DOOR_HEAD && feat <= FEAT_DOOR_TAIL)
	{
		bool flag = FALSE;

		/* If the door is a closed, unlocked door, Burglars can lock it */
		if ((feat == FEAT_DOOR_HEAD) && (deliberate))
		{
			if (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_LOCK_DOOR)
			{
				if (get_check("Lock this door?"))
				{
					int pow = rand_range(get_skill(S_BURGLARY, 1, 6),
					                     get_skill(S_BURGLARY, 1, 8));

					cave_set_feat(y, x, FEAT_DOOR_HEAD + pow);

					flag = TRUE;
				}
			}
		}

		/* Open -- unless we just locked it */
		if (!flag) more = do_cmd_open_aux(y, x);
	}

	/* Close open doors */
	else if (feat == FEAT_OPEN)
	{
		/* Close */
		more = do_cmd_close_aux(y, x);
	}

	/* If a burglar, set a trap.  -LM- */
	else if ((cave_trap_allowed(y, x)) &&
	         (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_TRAP))
	{
		more = py_set_trap(y, x, dir);
	}

	/* Oops */
	else
	{
		/* Oops */
		msg_print("You can make no alterations here.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Find the index of some "spikes", if possible.
 */
static bool get_spike(int *ip)
{
	int i;

	/* Check every item in the pack */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Save the spike index */
			(*ip) = i;

			/* Success */
			return (TRUE);
		}
	}

	/* Oops */
	return (FALSE);
}


/*
 * Determine if a given grid may be "spiked"
 */
static bool do_cmd_spike_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to spike.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Jam a closed door
 */
bool do_cmd_spike_aux(int y, int x)
{
	/* Convert "locked" to "stuck" XXX XXX XXX */
	if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08)
	{
		cave_feat[y][x] += 0x08;
	}

	/* Add one spike to the door */
	if (cave_feat[y][x] < FEAT_DOOR_TAIL)
	{
		cave_feat[y][x] += 0x01;
	}
	return (TRUE);
}



/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir, item;


	/* Get a spike */
	if (!get_spike(&item))
	{
		/* Message */
		msg_print("You have no spikes.");

		/* Done */
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_spike_test(y, x)) return;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Take a full turn  XXX */
		p_ptr->energy_use = 100;

		/* Attack -- usually do not move */
		if (py_attack(y, x)) return;
	}


	/* Door is fully barred and character is perceptive enough to notice */
	if ((cave_feat[y][x] == FEAT_DOOR_TAIL) &&
	         (get_skill(S_PERCEPTION, 0, 100) > rand_int(100)))
	{
		msg_print("This door is so firmly barred that further spiking will have no effect.");
	}

	/* Go for it */
	else
	{
		/* Take half a turn */
		p_ptr->energy_use = 50;

		/* Verify legality */
		if (!do_cmd_spike_test(y, x)) return;
		if (!do_cmd_spike_aux(y, x)) return;

		/* Successful jamming */
		msg_print("You jam the door with a spike.");

		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}

/*
 * Determine if a given grid may be "walked".
 *
 * Players can "walk" into visible monsters in walls.  This prevents the
 * annoyance of having to tunnel into a wall to get at a ghost you can see
 * perfectly well.
 */
static bool do_cmd_walk_test(int y, int x)
{
	/* Check for monsters. */
	if (cave_m_idx[y][x] != 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* If a monster can be seen, it can be attacked normally. */
		if (m_ptr->ml) return (TRUE);
	}


	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Require open space */
	if (!cave_passable_bold(y, x))
	{
		/* Door */
		if (cave_feat[y][x] < FEAT_SECRET)
		{
			/* Doors can be opened. */
			return (TRUE);
		}

		/* Wall */
		else
		{
			/* Message */
			msg_print("There is a wall in the way.");
		}

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for the "walk" and "jump" commands.
 */
static void do_cmd_walk_or_jump(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality (even if confused) */
	if (!do_cmd_walk_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Verify legality of new location */
		if (!do_cmd_walk_test(y, x)) return;
	}

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

	/* Move the player */
	move_player(dir, pickup);
}

/*
 * Walk into a grid (pick up objects as set by the auto-pickup option)
 */
void do_cmd_walk(void)
{
	/* Move (usually pickup) */
	do_cmd_walk_or_jump(always_pickup);
}


/*
 * Jump into a grid (flip pickup mode)
 */
void do_cmd_jump(void)
{
	/* Move (usually do not pickup) */
	do_cmd_walk_or_jump(!always_pickup);
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Hack XXX XXX XXX */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Start run */
	run_step(dir);
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure and objects if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;


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

	/* Spontaneous searching for traps and doors (doubly effective) */
	if ((p_ptr->skill_srh >= 80) ||
	    (rand_int(80) < p_ptr->skill_srh))
	{
		search();
	}

	/* Spontaneous searching -- essences */
	if (get_skill(S_WIZARDRY, 0, 100) >= LEV_REQ_SEE_ESSENCE)
	{
		search_essence();
	}

	/* Notice unseen objects */
	notice_unseen_objects();

	/* Handle objects.  Do not charge extra energy for objects picked up. */
	(void)py_pickup(pickup);

	/* Hack -- enter a store if we are on one */
	if ((cave_feat[py][px] >= FEAT_SHOP_HEAD) &&
		(cave_feat[py][px] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';

		/* Free turn XXX XXX XXX */
		p_ptr->energy_use = 0;
	}
}


/*
 * Hold still (pick up objects if always_pickup is TRUE).
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	do_cmd_hold_or_stay(always_pickup);
}


/*
 * Pick up objects on the floor beneath you.  -LM-
 */
void do_cmd_pickup(void)
{
	int energy_cost;

	/* Pick up floor objects, forcing a menu for multiple objects. */
	energy_cost = py_pickup(2) * 10;

	/* Maximum time expenditure is a full turn. */
	if (energy_cost > 100) energy_cost = 100;

	/* Charge this amount of energy. */
	p_ptr->energy_use = energy_cost;
}



/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed):";

		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 5)) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			p_ptr->command_arg = (-1);
		}

		/* Rest some */
		else
		{
			p_ptr->command_arg = atoi(out_val);
			if (p_ptr->command_arg <= 0) return;
		}
	}

	/* Paranoia */
	if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;

	/* Take a turn XXX XXX XXX (?) */
	p_ptr->energy_use = 100;

	/* Save the rest code */
	p_ptr->resting = p_ptr->command_arg;

	/* Cancel the arg */
	p_ptr->command_arg = 0;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Print "regen" */
	left_panel_display(DISPLAY_REGEN, 0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh XXX XXX XXX */
	Term_fresh();
}
