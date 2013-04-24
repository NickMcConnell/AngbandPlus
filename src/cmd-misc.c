/* File: cmd-misc.c */
/* Misc. dungeon and movement commands */

/*
 * "And so what he told me was that in order to escape to the carny 
 * he had to Go up and down stairs, toggle sneaking.  Handle chests.  Handle doors,
 * tunnel, disarm, bash, alter a grid, and spike.  Walk, run, stay still,
 * pickup, and rest. And that was all, too, by George!"
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Go up one level 
 *
 * NOTE:
 * This command takes the player *up* a level. p_ptr->depth is reduced
 * by one, the terrain is 'feat_less' indicating a return to the 'town'
 * depth. In Steam however, you start in the center and work your way 
 * to the surface. _THEREFORE_ the character for the FEAT_LESS staircase
 * is > (not < which it is in traditional angband), because heading 
 * to the center of the earth (down) is how you get back to town.
 * 
 * Basically the internal system remains the same (FEAT_LESS reducing
 * the p_ptr-depth value etc.) but the terrain characters have changed,
 * and the text associated with these functions has changed and in dungeon.c
 * the commands have changed. ('>' is do_cmd_go_up). This allows all internals
 * to stay consistant, while providing the illusion of the proper effects to 
 * the player.
 * 
 * Which is great and all, but still real confusing. -CCC
 *
 */
void do_cmd_go_up(void)
{
	char out_val[160];
	byte quest;

	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_LESS)
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/*find out of leaving a level*/
	quest = quest_check(p_ptr->depth);

	/* Verify leaving quest level */
	if ((verify_leave_quest) &&
		(quest == QUEST_GUILD || quest == QUEST_UNIQUE ||
       ((quest == QUEST_VAULT) && (quest_item_slot() == -1))))
	{
		sprintf(out_val, "Really risk failing your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Ironman */
	if (adult_ironman)
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Hack -- take a (partial) turn */
	p_ptr->energy_use = 25;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of down staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* New depth */
	p_ptr->depth--;

	/*find out of entering a quest level (should never happen going up)*/
	quest = quest_check(p_ptr->depth);

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	byte quest;
	char out_val[160];

	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_MORE)
	{
		msg_print("I see no up staircase here.");
		return;
	}

	/*find out if leaving a quest level*/
	quest = quest_check(p_ptr->depth);

	/* Verify leaving quest level */
	if ((verify_leave_quest) &&
		(quest == QUEST_GUILD || quest == QUEST_UNIQUE ||
        ((quest == QUEST_VAULT) && (quest_item_slot() == -1))))
		{
			sprintf(out_val, "Really risk failing your quest? ");

			if (!get_check(out_val)) return;
		}

	/* This only works as long as there is no reset recall */
	if (!p_ptr->max_depth) do_cmd_special_message(ANGBAND_DIR_FILE, "entrance.txt");
		
	/* Hack -- take a partial turn */
	p_ptr->energy_use = 25;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_up_stair = TRUE;

	/* New level */
	p_ptr->depth++;

	/*find out if entering a quest level*/
	quest = quest_check(p_ptr->depth);

	/* Leaving */
	p_ptr->leaving = TRUE;
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
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
	/* Stop searching */
	if (p_ptr->searching)
	{
		/* Clear the searching flag */
		p_ptr->searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start searching */
	else
	{
		/* Set the searching flag */
		p_ptr->searching = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
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
		/* if (!o_ptr->marked) continue; */

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}

/* A function that returns the tval of the items that will be generated
 * when a chest is opened. -LM-
 *
 * I'm going to add a *sub* option that restricts the items the chest can drop 
 * This needs to be by *chest*.
 */
static byte get_choice(int chest_sval)
{
	byte choice;

	choice = randint(100);
	
	switch (chest_sval)
	{
		case SV_TB_CHEST:
		{
			if (choice < 5) return (TV_BOOTS);
			if (choice < 25) return (TV_GLOVES);
			if (choice < 35) return (TV_HELM);
			if (choice < 50) return (TV_LEG);
			if (choice < 60) return (TV_CLOAK);
			if (choice < 85) return (TV_SOFT_ARMOR);
			if (choice < 95) return (TV_HARD_ARMOR);
			if (choice < 101) return (TV_GOLD);
			break;
		}
		case SV_DB_CHEST:
		{
			if (choice < 70) return (TV_TONIC);
			if (choice < 80) return (TV_FOOD);
			if (choice < 101) return (TV_GOLD);
			break;
		}
		case SV_LB_CHEST:
		{
			if (choice < 60) return (TV_GOLD);
			if (choice < 70) return (TV_AMULET);
			if (choice < 85) return (TV_RING);
			if (choice < 90) return (TV_FOOD);
			if (choice < 101) return (TV_MAGIC_BOOK);
			break;
		}
		case SV_BB_CHEST:
		{
			if (choice < 30) return (TV_BOOTS);
			if (choice < 60) return (TV_GLOVES);
			if (choice < 90) return (TV_HELM);
			if (choice < 101) return (TV_SOFT_ARMOR);
			break;
		}
		case SV_HB_CHEST:
		{
			if (choice < 101) return (TV_HELM);
			break;
		}
		case SV_GB_CHEST:
		{
			if (choice < 60) return (TV_SOFT_ARMOR);
			if (choice < 101) return (TV_HARD_ARMOR);
			break;
		}
		case SV_CT_CHEST:
		{
			if (choice < 3) return (TV_AMMO);
			if (choice < 6) return (TV_BULLET);
			if (choice < 9) return (TV_SHOT);
			if (choice < 20) return (TV_GUN);
			if (choice < 25) return (TV_HAFTED);
			if (choice < 30) return (TV_POLEARM);
			if (choice < 35) return (TV_SWORD);
			if (choice < 40) return (TV_DAGGER);
			if (choice < 45) return (TV_AXES);
			if (choice < 50) return (TV_BLUNT);
			if (choice < 65) return (TV_TOOL);
			if (choice < 60) return (TV_RAY);
			if (choice < 75) return (TV_APPARATUS);
			if (choice < 85) return (TV_MECHANISM);
			if (choice < 95) return (TV_MAGIC_BOOK);
			if (choice < 101) return (TV_GOLD);			
			break;
		}
		case SV_ST_CHEST:
		{
			if (choice < 2) return (TV_AMMO);
			if (choice < 4) return (TV_BULLET);
			if (choice < 6) return (TV_SHOT);
			if (choice < 12) return (TV_GUN);
			if (choice < 14) return (TV_HAFTED);
			if (choice < 16) return (TV_POLEARM);
			if (choice < 18) return (TV_SWORD);
			if (choice < 20) return (TV_DAGGER);
			if (choice < 22) return (TV_AXES);
			if (choice < 24) return (TV_BLUNT);
			if (choice < 28) return (TV_BOOTS);
			if (choice < 30) return (TV_GLOVES);
			if (choice < 32) return (TV_HELM);
			if (choice < 34) return (TV_LEG);
			if (choice < 36) return (TV_CLOAK);
			if (choice < 45) return (TV_SOFT_ARMOR);
			if (choice < 54) return (TV_HARD_ARMOR);
			if (choice < 60) return (TV_AMULET);			
			if (choice < 65) return (TV_TOOL);
			if (choice < 70) return (TV_RAY);
			if (choice < 75) return (TV_APPARATUS);
			if (choice < 80) return (TV_MECHANISM);
			if (choice < 82) return (TV_MAGIC_BOOK);
			if (choice < 101) return (TV_GOLD);			
			break;
		}
		case SV_CS_CHEST:
		{
			if (choice < 80) return (TV_GOLD);
			if (choice < 90) return (TV_RING);
			if (choice < 101) return (TV_AMULET);
			break;
		}
		case SV_BC_CHEST:
		{
			if (choice < 80) return (TV_MAGIC_BOOK);
			if (choice < 101) return (TV_TEXT);
			break;
		}
		case SV_JB_CHEST:
		{
			if (choice < 30) return (TV_GOLD);
			if (choice < 60) return (TV_RING);
			if (choice < 101) return (TV_AMULET);
			break;
		}
	}

	/* If the function fails, do not specify a tval */
	return (0);
}


/*
 * Allocate objects upon opening a chest
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Chests contain a variety of items depending on what type of chest
 * they are. The "value" of the items in a chest is based on the "power"
 * of the chest, which is in turn based on the level on which 
 * the chest is generated.
 *
 * Wheee! I'm stealing code by LM! :-) see Oangband for details.
 */
static void chest_death(int y, int x, s16b o_idx)
{
	int number;
	bool obj_success = FALSE;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	/* Get the chest */
	o_ptr = &o_list[o_idx];

	/* Determine how much to drop */
	if (o_ptr->sval >= SV_CHEST_MIN_LARGE) number = 4 + randint(3);
	else number = 2 + randint(3);
	

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 20;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Select an item type that the chest will disperse */
		required_tval = get_choice(o_ptr->sval);

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make an object with a specified tval.  Grant a possibility for 
		 * items to be forced good, or even great.  With the new definitions 
		 * of goodness, this can make for quite interesting loot.  -LM-
		 */
		switch (required_tval)
		{
			case TV_HARD_ARMOR:
			case TV_SOFT_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_LEG:
			case TV_CLOAK:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_GUN:
			case TV_SWORD:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_DIGGING:
			case TV_AMMO:
			case TV_BULLET:
			case TV_SHOT:
			{
				if (randint(50) < object_level)
				{
					obj_success = make_object(i_ptr, TRUE, TRUE, TRUE);
					break;
				}

				else if (randint(10) < object_level)
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
			{
				if (randint(15) < object_level)
				{
				  obj_success = make_object(i_ptr, TRUE, FALSE, TRUE);
				}
				else
				{
				  obj_success = make_object(i_ptr, FALSE, FALSE, TRUE);
				}

				break;
			}

			case TV_MECHANISM:
			case TV_TONIC:
			case TV_RING:
			case TV_AMULET:
			case TV_RAY:
			case TV_TOOL:
			case TV_APPARATUS:
			{
				if (randint(25) < (object_level - 10) / 2)
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

		/* If no object was made, we need to try another tval. */
		if (!obj_success)
		{
			required_tval = get_choice(o_ptr->sval);
		}

		/* Normally, drop object near the chest. */
		else drop_near(i_ptr, -1, y, x);
	}

	/* Clear this global variable, to avoid messing up object generation. */
	required_tval = 0;

	/* Reset the object level */
	object_level = p_ptr->depth;

	/* No longer opening a chest */
	opening_chest = FALSE;

	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
	int i, trap, available_traps;

	object_type *o_ptr = &o_list[o_idx];

	/* Compensate for the averaging routine in the summon monster function. */
	int summon_level = o_ptr->pval + o_ptr->pval - p_ptr->depth;

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;
	
	/* HACK - preserve space for future traps! */
	available_traps = o_ptr->pval;
	if (available_traps > 63) available_traps = 63;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_MUS))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle", TRUE);
		(void)do_dec_stat(A_MUS);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_VIG))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle", TRUE);
		(void)do_dec_stat(A_VIG);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(resist_effect(RS_PSN)))
		{
			(void)set_poisoned(p_ptr->poisoned + 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");
		for (i = 0; i < num; i++)
		{
			(void)summon_specific(y, x, summon_level, 0, FALSE);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		take_hit(damroll(5, 8), "an exploding chest", TRUE);
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
	int disarmgood, disarmnorm, disarmpoor;

	bool flag = TRUE;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];

	/*never open quest chests*/
	if(o_ptr->ident & IDENT_QUEST)
	{
		msg_print("This chest is to be opened by the Guild!");

		flag = FALSE;
	}

	/* Attempt to unlock it */
	else if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		disarmgood = p_ptr->skills[SK_DISARM_GOOD].skill_rank;
		disarmnorm = p_ptr->skills[SK_DISARM_NORM].skill_rank;
		disarmpoor = p_ptr->skills[SK_DISARM_POOR].skill_rank;
		
		/* Insure good values */
		/* Note the room for another skill to increase disarming */
		if (disarmgood >= 0) i = disarmgood * 4;
		if (disarmnorm >= 0) i = disarmnorm * 3;
		if (disarmpoor >= 0) i = disarmpoor * 2;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10; 
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (rand_int(100) < j)
		{
			msg_print("You have picked the lock.");
			gain_exp(1);
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

		/* Let the Chest drop items */
		chest_death(y, x, o_idx);
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
	int i, j;
	int disarmgood, disarmnorm, disarmpoor;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];

	/* Get the "disarm" factor */
	disarmgood = p_ptr->skills[SK_DISARM_GOOD].skill_rank;
	disarmnorm = p_ptr->skills[SK_DISARM_NORM].skill_rank;
	disarmpoor = p_ptr->skills[SK_DISARM_POOR].skill_rank;
	
	/* Insure good values */
	/* Note the room for another skill to increase disarming */
	if (disarmgood >= 0) i = disarmgood * 4;
	if (disarmnorm >= 0) i = disarmnorm * 3;
	if (disarmpoor >= 0) i = disarmpoor * 2;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	j = i - o_ptr->pval;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

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

	/* Success (get a lot of experience) */
	else if (rand_int(100) < j)
	{
		msg_print("You have disarmed the chest.");
		gain_exp(o_ptr->pval);
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
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


#if defined(ALLOW_EASY_OPEN)

/*
 * Return TRUE if the given feature is an open (or broken) door
 */
static bool is_open(int feat)
{
	return ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN));
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
 * Return TRUE if the given feature is a trap
 */
static bool is_trap(int feat)
{
	return ((feat >= FEAT_TRAP_HEAD) &&
	        (feat <= FEAT_TRAP_TAIL));
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
		/* if not searching under player continue */
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
		++count;

		/* Remember the location of the last door found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}


/*
 * Return the number of chests around (or under) the character.
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

		/* No (known) traps here */
		if (trapped &&
		    (!object_known_p(o_ptr) ||
		     (o_ptr->pval < 0) ||
		     !chest_traps[o_ptr->pval]))
		{
			continue;
		}

		/* Count it */
		++count;

		/* Remember the location of the last chest found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}


/*
 * Extract a "direction" which will move one step from the player location
 * towards the given "target" location (or "5" if no motion necessary).
 */
static int coords_to_dir(int y, int x)
{
	return (motion_dir(p_ptr->py, p_ptr->px, y, x));
}

#endif /* ALLOW_EASY_OPEN */


/*
 * Determine if a given grid may be "opened"
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
	int i, j;
	int disarmgood, disarmnorm, disarmpoor;
	
	bool more = FALSE;

	/* Paranoia */
	i = 0;
	
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
		/* Get the "disarm" factor */
		disarmgood = p_ptr->skills[SK_DISARM_GOOD].skill_rank;
		disarmnorm = p_ptr->skills[SK_DISARM_NORM].skill_rank;
		disarmpoor = p_ptr->skills[SK_DISARM_POOR].skill_rank;
		
		/* Insure good values */
		/* Note the room for another skill to increase disarming */
		if (disarmgood >= 0) i = disarmgood * 4;
		if (disarmnorm >= 0) i = disarmnorm * 3;
		if (disarmpoor >= 0) i = disarmpoor * 2;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		/* Not quite sure what the lock power range is */
		/* doesn't matter, it's out of randint 100, so we work from that */
		j = cave_feat[y][x] - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
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

			/* Experience */
			gain_exp(1);
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
 */
void do_cmd_open(void)
{
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Open */
	if (easy_open)
	{
		int num_doors, num_chests;

		/* Count closed doors */
		num_doors = count_feats(&y, &x, is_closed, FALSE);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

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
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x, 0);
	}

	/* Chest */
	else if (o_idx)
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
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0);

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
	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Close */
	if (easy_open)
	{
		/* Count open doors */
		if (count_feats(&y, &x, is_open, FALSE) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return;


	/* removed energy use from this line */
	/* Hopefully there shouldn't be any strange behavior */

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Take a turn */
		p_ptr->energy_use = 100;

		/* Attack */
		py_attack(y, x, 0);
		return;
	}

	/* Door */
	else
	{
		/* Close door */
		more = do_cmd_close_aux(y, x);
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

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

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

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
	
	int digchance = p_ptr->skill_dig;

	/* sanity check */
	if (digchance > 2000) digchance = 2000;

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);


	/* Sound XXX XXX XXX */
	/* sound(MSG_DIG); */

	/* Titanium */
	if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
	{
		msg_print("This seems to be permanent rock.");
	}
	

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
		/* Tunnel */
		if ((digchance > 40 + rand_int(1600)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
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
			okay = (digchance > 20 + rand_int(800));
		}

		/* Magma */
		else
		{
			okay = (digchance > 10 + rand_int(400));
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
			msg_print("You tunnel into the magma vein.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((digchance > rand_int(200)) && twall(y, x))
		{
			/* Message */
			msg_print("You have removed the rubble.");

			/* Hack -- place an object */
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(y, x))
				{
					msg_print("You have found something!");
				}
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
		/* Tunnel */
		if ((digchance > 30 + rand_int(1200)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25) search();
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((digchance > 30 + rand_int(1200)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
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
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Oops */
	if (!do_cmd_tunnel_test(y, x)) return;

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x, 0);
	}

	/* Walls */
	else
	{
		/* Tunnel through walls */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require an actual trap */
	if (!((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
	      (cave_feat[y][x] <= FEAT_TRAP_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to disarm.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(int y, int x)
{
	int i, j, power;
	int disarmgood, disarmnorm, disarmpoor;

	cptr name;

	bool more = FALSE;

	/* Paranoia */
	i = 0;
	
	/* Verify legality */
	if (!do_cmd_disarm_test(y, x)) return (FALSE);


	/* Get the trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	disarmgood = p_ptr->skills[SK_DISARM_GOOD].skill_rank;
	disarmnorm = p_ptr->skills[SK_DISARM_NORM].skill_rank;
	disarmpoor = p_ptr->skills[SK_DISARM_POOR].skill_rank;
	
	/* Insure good values */
	/* Note the room for another skill to increase disarming */
	if (disarmgood >= 0) i = disarmgood * 4;
	if (disarmnorm >= 0) i = disarmnorm * 3;
	if (disarmpoor >= 0) i = disarmpoor * 2;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */
	/* What's up here??? -CCC */
	/* Extract trap "power" */
	power = p_ptr->depth / 2;

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if (rand_int(100) < j)
	{
		/* Message */
		msg_format("You have disarmed the %s.", name);

		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		cave_info[y][x] &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
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
		hit_trap(y, x);
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or a chest
 */
void do_cmd_disarm(void)
{
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Disarm */
	if (easy_open)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_feats(&y, &x, is_trap, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps || num_chests)
		{
			if (num_traps + num_chests <= 1)
				p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !do_cmd_disarm_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;



	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x, 0);
	}

	/* Chest */
	else if (o_idx)
	{
		/* Disarm the chest */
		more = do_cmd_disarm_chest(y, x, o_idx);
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "bashed"
 */
static bool do_cmd_bash_test(int y, int x)
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
		msg_print("You see nothing there to bash.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "bash" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x)
{
	int bash, temp;

	bool more = FALSE;
	int soak;


	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return (FALSE);


	/* Message */
	msg_print("You smash into the door!");

	/* Make a lot of noise. */
	add_wakeup_chance = 9000;

	/* Hack -- Bash power based on strength */
	bash = p_ptr->stat_use[A_MUS] / 4;

	/* Extract door power */
	temp = ((cave_feat[y][x] - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 20));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Break down the door */
		if (rand_int(100) < 50)
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
	/* This isn't based off anything having to do with the door. . . 
	 * It appears to be completely independent of anything but random chance
	 * which is I suppose ok for whether or not you're off balance when you 
	 * smash into a door. . . With this new code you have a range of 1 to
	 * 333 + 1-50. This makes you be totally safe from being stunned when your
	 * agility is around 333 (and level 10) around then you should be safe from
	 * getting stunned by bashing down doors-CCC
	 */
	else if (rand_int(120) < p_ptr->stat_use[A_AGI] / 3 + p_ptr->lev)
	{
		/* Message */
		msg_print("The door holds firm.");

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		/* Caculate the soak */
		/* @STAT@ */
		soak = 8 - (p_ptr->stat_use[A_VIG] / 50);
		
		/* Sanity Check */
		if (soak < 0) soak = 0;
		
		/* Hack -- Lose balance ala paralysis */
		/* There should also be a low level toughness save, or something */
		(void)set_paralyzed(p_ptr->paralyzed + 2 + rand_int(soak));
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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x, 0);
	}

	/* Door */
	else
	{
		/* Bash the door */
		if (!do_cmd_bash_aux(y, x))
		{
			/* Cancel repeat */
			disturb(0, 0);
		}
	}
}



/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(void)
{
	int y, x, dir;
	int px = p_ptr->px;
	int py = p_ptr->py;
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

	/* Attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x, 0);
	}

	/* Tunnel through walls */
	else if (feat >= FEAT_SECRET)
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Bash jammed doors */
	else if (feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Tunnel */
		more = do_cmd_bash_aux(y, x);
	}

	/* Open closed doors */
	else if (feat >= FEAT_DOOR_HEAD)
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}

	/* Disarm traps */
	else if (feat >= FEAT_TRAP_HEAD)
	{
		/* Tunnel */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Close open doors */
	else if (feat == FEAT_OPEN)
	{
		/* Close */
		more = do_cmd_close_aux(y, x);
	}


	/* Oops */
	else
	{
		/* Oops */
		msg_print("You spin around.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
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
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int y, x, dir, item;


	/* Get a spike */
	if (!get_spike(&item))
	{
		/* Message */
		msg_print("You have no spikes!");

		/* Done */
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_spike_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
	}


	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x, 0);
	}

	/* Go for it */
	else
	{
		/* Verify legality */
		if (!do_cmd_spike_test(y, x)) return;

		/* Successful jamming */
		msg_print("You jam the door with a spike.");

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

		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}




/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int y, int x)
{
	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	if ((cave_m_idx[y][x] > 0) && (easy_alter)) return (TRUE);
	
	/* Require open space */
	if ((!cave_floor_bold(y, x)) && (!p_ptr->tim_wraith) && (!p_ptr->wraith_form))
	{
		/* Rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE)
		{
			/* Message */
			msg_print("There is a pile of rubble in the way!");
		}

		/* Door */
		else if (cave_feat[y][x] < FEAT_SECRET)
		{

#ifdef ALLOW_EASY_ALTER

			/* Hack -- Handle "easy_alter" */
			if (easy_alter) return (TRUE);

#endif /* ALLOW_EASY_ALTER */

			/* Message */
			msg_print("There is a door in the way!");
		}

		/* Wall */
		else
		{
			/* Message */
			msg_print("There is a wall in the way!");
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
static void do_cmd_walk_or_jump(int jumping)
{
	int y, x, dir;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
	}


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

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
	move_player(dir, jumping);
}


/*
 * Walk into a grid.
 */
void do_cmd_walk(void)
{
	/* Move (normal) */
	do_cmd_walk_or_jump(FALSE);
}


/*
 * Jump into a grid.
 */
void do_cmd_jump(void)
{
	/* Move (jump) */
	do_cmd_walk_or_jump(TRUE);
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
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
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;
	if ((p_ptr->tim_wraith || p_ptr->wraith_form) && !cave_floor_bold(y, x)) return;

	/* Start run */
	run_step(dir);
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
{
	/* searching freq. */
	int searchfrqgood, searchfrqnorm, searchfrqpoor;
	int searchfrqchance;
	
	/* Paranoia */
	searchfrqchance = 0;

	/* Get the "Frequency" factor */
	searchfrqgood = p_ptr->skills[SK_SEARCHING_GOOD].skill_rank;
	searchfrqnorm = p_ptr->skills[SK_SEARCHING_NORM].skill_rank;
	searchfrqpoor = p_ptr->skills[SK_SEARCHING_POOR].skill_rank;
	
	/* Insure good values */
	/* Note the room for another skill to increase disarming */
	if (searchfrqgood >= 0) searchfrqchance = searchfrqgood * 3;
	if (searchfrqnorm >= 0) searchfrqchance = searchfrqnorm * 2;
	if (searchfrqpoor >= 0) searchfrqchance = searchfrqpoor;

	if (p_ptr->skills[SK_TRAILBLAZER].skill_max > 0)
		searchfrqchance += p_ptr->skills[SK_TRAILBLAZER].skill_rank / 2;
		
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

	/* Spontaneous Searching */
	if ((searchfrqchance >= 50) || (0 == rand_int(50 - searchfrqchance)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}

	/* Handle "objects" */
	py_pickup(pickup);

	/* Hack -- enter a store if we are on one */
	if ((cave_feat[p_ptr->py][p_ptr->px] >= FEAT_SHOP_HEAD) &&
	    (cave_feat[p_ptr->py][p_ptr->px] <= FEAT_SHOP_TAIL))
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
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	do_cmd_hold_or_stay(always_pickup);
}


/*
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
	/* Stay still (usually do not pickup) */
	do_cmd_hold_or_stay(!always_pickup);
}


/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";

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

	/* Cancel searching */
	p_ptr->searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh XXX XXX XXX */
	if (fresh_before) Term_fresh();
}








/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(int dir, int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* searching freq. */
	int searchfrqgood, searchfrqnorm, searchfrqpoor;
	int searchfrqchance;
	
	int y, x;
	bool p_can_pass_walls = FALSE;
	bool wall_is_perma = FALSE;
	
	/* Paranoia */
	searchfrqchance = 0;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];
		
	/* Get the "Frequency" factor */
	searchfrqgood = p_ptr->skills[SK_SEARCHING_GOOD].skill_rank;
	searchfrqnorm = p_ptr->skills[SK_SEARCHING_NORM].skill_rank;
	searchfrqpoor = p_ptr->skills[SK_SEARCHING_POOR].skill_rank;
	
	/* Insure good values */
	/* Note the room for another skill to increase disarming */
	if (searchfrqgood >= 0) searchfrqchance = searchfrqgood * 3;
	if (searchfrqnorm >= 0) searchfrqchance = searchfrqnorm * 2;
	if (searchfrqpoor >= 0) searchfrqchance = searchfrqpoor;

	if (p_ptr->skills[SK_TRAILBLAZER].skill_max > 0)
		searchfrqchance += p_ptr->skills[SK_TRAILBLAZER].skill_rank / 2;

	/* Player can not walk through "walls"... */
	/* unless in Shadow Form */
	if ((p_ptr->wraith_form) || (p_ptr->tim_wraith))
		p_can_pass_walls = TRUE;

	if (((cave_feat[y][x]) >= FEAT_PERM_EXTRA) &&
	    ((cave_feat[y][x]) <= FEAT_PERM_SOLID))
	{
		wall_is_perma = TRUE;
		p_can_pass_walls = FALSE;
	}

	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);
		/* careful here, just checking for pet, not friendly */
		if(is_pet(m_ptr))
		{
			msg_format("You push past %s.", m_name);
			monster_swap(py, px, y, x);
			update_mon(cave_m_idx[y][x], TRUE, FALSE);
			
			/* No searching */
			/* Handle "objects" */
			py_pickup(jumping != always_pickup);
			
			/* Discover invisible traps */
			if (cave_feat[y][x] == FEAT_INVIS)
			{
					/* Disturb */
					disturb(0, 0);

					/* Message */
					msg_print("You found a trap!");

					/* Pick a trap */
					pick_trap(y, x);

					/* Hit the trap */
					hit_trap(y, x);
			}

			/* Set off an visible trap */
			else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		  	       (cave_feat[y][x] <= FEAT_TRAP_TAIL))
			{
					/* Disturb */
					disturb(0, 0);

					/* Hit the trap */
					hit_trap(y, x);
			}

		}
		else
		/* Attack */
		py_attack(y, x, 0);
	}

#ifdef ALLOW_EASY_ALTER

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if (easy_alter && !jumping &&
	         (cave_info[y][x] & (CAVE_MARK)) &&
	         (cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
	         (cave_feat[y][x] <= FEAT_DOOR_TAIL))
	{
		/* Auto-repeat if not already repeating */
		if (!p_ptr->command_rep && (p_ptr->command_arg <= 0))
		{
			/* Repeat 99 times */
			p_ptr->command_rep = 99;

			/* Reset the command count */
			p_ptr->command_arg = 0;
		}

		/* Alter */
		do_cmd_alter();
	}

#endif /* ALLOW_EASY_ALTER */

	/* Player can not walk through "walls" */
	else if ((!cave_floor_bold(y, x)) && (!p_can_pass_walls))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
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
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "There is a door blocking your way.");
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "There is a wall blocking your way.");
			}
		}
	}

	/* Normal movement */
	else
	{
		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* Move player */
		monster_swap(py, px, y, x);

		/* If you didn't attack, take a turn. */
		/* This turn was moved from do cmd walk run. */
		/* Why the hell did I move this? */
		/* p_ptr->energy_use = 100; */

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;


		/* Spontaneous Searching */
		if ((searchfrqchance >= 50) ||
		    (0 == rand_int(50 - searchfrqchance)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		py_pickup(jumping != always_pickup);

		/* Handle "store doors" */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Free turn XXX XXX XXX */
			p_ptr->energy_use = 0;
		}

		/* Discover invisible traps */
		else if (cave_feat[y][x] == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(y, x);

			/* Hit the trap */
			hit_trap(y, x);
		}

		/* Set off an visible trap */
		else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		         (cave_feat[y][x] <= FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap(y, x);
		}
	}
}
/*
 * Search for hidden things
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;

	int searchgood, searchnorm, searchpoor;
	
	/* Paranoia */
	chance = 0;
	
	/* Get the "search" factor */
	searchgood = p_ptr->skills[SK_SEARCHING_GOOD].skill_rank;
	searchnorm = p_ptr->skills[SK_SEARCHING_NORM].skill_rank;
	searchpoor = p_ptr->skills[SK_SEARCHING_POOR].skill_rank;
	
	/* Insure good values */
	if (searchgood >= 0) chance = searchgood * 5;
	if (searchnorm >= 0) chance = searchnorm * 3;
	if (searchpoor >= 0) chance = searchpoor;

	if (p_ptr->skills[SK_TRAILBLAZER].skill_max > 0)
		chance += p_ptr->skills[SK_TRAILBLAZER].skill_rank / 2;

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

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
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
static bool auto_pickup_okay(const object_type *o_ptr)
{
	cptr s;

	/* It can't be carried */
	if (!inven_carry_okay(o_ptr)) return (FALSE);

	/* No inscription */
	if (!o_ptr->note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->note), '=');

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
 * Helper routine for py_pickup() and py_pickup_floor().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
static void py_pickup_aux(int o_idx)
{
	int slot;

	char o_name[80];
	object_type *o_ptr;
	object_kind *k_ptr;

	o_ptr = &o_list[o_idx];

	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Make us aware of objects *when we look at them* instead of at generation */
	/* Unless they are 'hidden' objects with a flavor */
	if (!k_ptr->flavor) k_ptr->aware = TRUE;

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
 * Make the player carry everything in a grid.
 *
 * If "pickup" is FALSE then only gold will be picked up.
 */
void py_pickup(int pickup)
{
	char action;

	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	char o_name[80];

#ifdef ALLOW_EASY_FLOOR

	int last_o_idx = 0;

	int can_pickup = 0;
	int not_pickup = 0;

#endif /* ALLOW_EASY_FLOOR */


	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
			msg_format("You have found %ld gold pieces worth of %s.",
			           (long)o_ptr->pval, o_name);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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

#ifdef ALLOW_EASY_FLOOR

		/* Easy Floor */
		if (easy_floor)
		{
			/* Pickup if possible */
			if (pickup && inven_carry_okay(o_ptr))
			{
				/* Pick up if allowed */
				if (!carry_query_flag)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}

				/* Else count */
				else
				{
					/* Remember */
					last_o_idx = this_o_idx;

					/* Count */
					++can_pickup;
				}
			}

			/* Else count */
			else
			{
				/* Remember */
				last_o_idx = this_o_idx;

				/* Count */
				++not_pickup;
			}

			/* Check the next object */
			continue;
		}

#endif /* ALLOW_EASY_FLOOR */

		/* Describe the object */
		if (!pickup)
		{
			msg_format("You see %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Note that the pack is too full */
		if (!inven_carry_okay(o_ptr))
		{
			msg_format("You have no room for %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Query before picking up */
		if (carry_query_flag)
		{
			char out_val[160];
			sprintf(out_val, "Pick up %s? ", o_name);
			/* RMG: new function returning y, n, k or ESC */
			action = get_pickup_check(out_val);
		}
		/* Testing -> currently can't pick up if query flag is off */
		else py_pickup_aux(this_o_idx);

		if (action == 'y' || action == 'Y')
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);
		}
		else if (action == 'k' || action == 'K')
		{
			/* RMG: item desctruction without item query - destroy query is still asked when verify_destroy is TRUE */
			do_cmd_destroy(0 - this_o_idx);
		}
		else if (action == 'n' || action == 'N' || action == ' ')
		{
			/* continue with next item, current item will stay on the ground */
			continue;
		}
		else
		{
			/* RMG: ESC will exit loop and stop pickup */
			break;
		}

	}

#ifdef ALLOW_EASY_FLOOR

	/* Easy floor, objects left */
	if (easy_floor && (can_pickup + not_pickup > 0))
	{
		/* Not picking up */
		if (!pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Get the object */
				o_ptr = &o_list[last_o_idx];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
				msg_format("You see %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_format("You see a pile of %d objects.", not_pickup);
			}

			/* Done */
			return;
		}

		/* No room */
		if (!can_pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Get the object */
				o_ptr = &o_list[last_o_idx];

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

		/* Pick up objects */
		while (1)
		{
			cptr q, s;

			int item;

			/* Restrict the choices */
			item_tester_hook = inven_carry_okay;

			/* Get an object*/
			q = "Get which item? ";
			s = NULL;
			if (!get_item(&item, q, s, (USE_FLOOR))) break;

			/* Pick up the object */
			py_pickup_aux(0 - item);
		}
	}

#endif /* ALLOW_EASY_FLOOR */

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
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static const byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. (?)
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

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
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

    /* Move the player */
	move_player(p_ptr->run_cur_dir, FALSE);
}

