/* File: cmd2.c */

/* Going up and down stairs, items that a chest may contain, opening 
 * chests, tunnelling, disarming, opening doors, alter adjacent grid, 
 * spiking, starting various movement and resting routines, chance of an 
 * object breaking, and firing and throwing all objects.
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
 */
void do_cmd_go_up(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char answer;

	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_LESS)
	{
		msg_print("I see no up staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	msg_print("You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* New depth */
	p_ptr->depth--;

	/* If the new level is not a quest level, or the town, grant a 33% 
	 * chance of going up another level. -LM-
	 */
	if ((is_quest(p_ptr->depth) == FALSE) && 
		(p_ptr->depth != 0) && (randint(3) == 1))
	{
		msg_print("The stairs continue up.  Go up another level? (y/n)");

		answer = inkey();
		if ((answer == 'Y') || (answer == 'y')) p_ptr->depth--;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char answer;

	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_MORE)
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	msg_print("You enter a maze of down staircases.");

	/* Create a way back */
	p_ptr->create_up_stair = TRUE;

	/* New level */
	p_ptr->depth++;

	/* If the new level is not a quest level, or the bottom of the dungeon, grant a 50% chance of descending another level. -LM- */
	if ((is_quest(p_ptr->depth) == FALSE) && (p_ptr->depth < MAX_DEPTH -1) && (randint(2) == 1))
	{
		msg_print("The stairs continue down.  Go down another level? (y/n)");

		answer = inkey();
		if ((answer == 'Y') || (answer == 'y')) p_ptr->depth++;
	}

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

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		/* if (!o_ptr->marked) continue; */

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}

/* A function that returns the TVAL of the items that will be generated
 * when a chest is opened. -LM-
 */
static byte get_choice(void)
{
	byte choice;

	choice = randint(100);

	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
		{
			if (choice < 2) return (TV_SHOT);
			if (choice < 5) return (TV_ARROW);
			if (choice < 9) return (TV_BOLT);
			if (choice < 13) return (TV_BOW);
			if (choice < 25) return (TV_HAFTED);
			if (choice < 37) return (TV_POLEARM);
			if (choice < 49) return (TV_SWORD);
			if (choice < 54) return (TV_BOOTS);
			if (choice < 59) return (TV_GLOVES);
			if (choice < 64) return (TV_HELM);
			if (choice < 67) return (TV_CROWN);
			if (choice < 72) return (TV_SHIELD);
			if (choice < 76) return (TV_CLOAK);
			if (choice < 79) return (TV_SOFT_ARMOR);
			if (choice < 89) return (TV_HARD_ARMOR);
			if (choice < 95) return (TV_SCROLL);
			if (choice < 101) return (TV_POTION);
			break;
		}

		case CLASS_MAGE:
		{
			if (choice < 4) return (TV_BOOTS);
			if (choice < 7) return (TV_HELM);
			if (choice < 11) return (TV_CROWN);
			if (choice < 16) return (TV_SHIELD);
			if (choice < 22) return (TV_CLOAK);
			if (choice < 28) return (TV_SOFT_ARMOR);
			if (choice < 34) return (TV_SCROLL);
			if (choice < 40) return (TV_POTION);
			if (choice < 46) return (TV_RING);
			if (choice < 52) return (TV_AMULET);
			if (choice < 64) return (TV_WAND);
			if (choice < 76) return (TV_STAFF);
			if (choice < 88) return (TV_ROD);
			if (choice < 101) return (TV_MAGIC_BOOK);
			break;
		}

		case CLASS_PRIEST:
		{
			if (choice < 4) return (TV_BOOTS);
			if (choice < 7) return (TV_HELM);
			if (choice < 12) return (TV_CROWN);
			if (choice < 16) return (TV_SHIELD);
			if (choice < 21) return (TV_GLOVES);
			if (choice < 27) return (TV_CLOAK);
			if (choice < 33) return (TV_SOFT_ARMOR);
			if (choice < 39) return (TV_SCROLL);
			if (choice < 46) return (TV_POTION);
			if (choice < 53) return (TV_RING);
			if (choice < 60) return (TV_AMULET);
			if (choice < 69) return (TV_HAFTED);
			if (choice < 76) return (TV_WAND);
			if (choice < 81) return (TV_STAFF);
			if (choice < 86) return (TV_ROD);
			if (choice < 101) return (TV_PRAYER_BOOK);
			break;
		}

		case CLASS_ROGUE:
		{
			if (choice < 11) return (TV_SHOT);
			if (choice < 17) return (TV_ARROW);
			if (choice < 20) return (TV_BOLT);
			if (choice < 29) return (TV_BOW);
			if (choice < 33) return (TV_HAFTED);
			if (choice < 37) return (TV_POLEARM);
			if (choice < 44) return (TV_SWORD);
			if (choice < 48) return (TV_BOOTS);
			if (choice < 50) return (TV_GLOVES);
			if (choice < 54) return (TV_HELM);
			if (choice < 58) return (TV_CROWN);
			if (choice < 62) return (TV_SHIELD);
			if (choice < 68) return (TV_CLOAK);
			if (choice < 71) return (TV_SOFT_ARMOR);
			if (choice < 74) return (TV_HARD_ARMOR);
			if (choice < 80) return (TV_SCROLL);
			if (choice < 86) return (TV_POTION);
			if (choice < 92) return (TV_STAFF);
			if (choice < 101) return (TV_MAGIC_BOOK);
			break;
		}

		case CLASS_RANGER:
		{
			if (choice < 15) return (TV_ARROW);
			if (choice < 21) return (TV_BOLT);
			if (choice < 31) return (TV_BOW);
			if (choice < 34) return (TV_HAFTED);
			if (choice < 37) return (TV_POLEARM);
			if (choice < 40) return (TV_SWORD);
			if (choice < 45) return (TV_RING);
			if (choice < 50) return (TV_AMULET);
			if (choice < 54) return (TV_BOOTS);
			if (choice < 58) return (TV_GLOVES);
			if (choice < 62) return (TV_HELM);
			if (choice < 66) return (TV_CROWN);
			if (choice < 70) return (TV_SHIELD);
			if (choice < 74) return (TV_CLOAK);
			if (choice < 78) return (TV_SOFT_ARMOR);
			if (choice < 82) return (TV_ROD);
			if (choice < 87) return (TV_WAND);
			if (choice < 92) return (TV_STAFF);
			if (choice < 101) return (TV_DRUID_BOOK);
			break;
		}

		case CLASS_PALADIN:
		{
			if (choice < 4) return (TV_BOOTS);
			if (choice < 8) return (TV_HELM);
			if (choice < 12) return (TV_CROWN);
			if (choice < 16) return (TV_SHIELD);
			if (choice < 20) return (TV_GLOVES);
			if (choice < 24) return (TV_CLOAK);
			if (choice < 30) return (TV_HARD_ARMOR);
			if (choice < 35) return (TV_SCROLL);
			if (choice < 40) return (TV_POTION);
			if (choice < 47) return (TV_RING);
			if (choice < 54) return (TV_AMULET);
			if (choice < 70) return (TV_HAFTED);
			if (choice < 90) return (TV_STAFF);
			if (choice < 94) return (TV_ROD);
			if (choice < 101) return (TV_PRAYER_BOOK);
			break;
		}

		case CLASS_DRUID:
		{
			if (choice < 3) return (TV_BOOTS);
			if (choice < 5) return (TV_HELM);
			if (choice < 11) return (TV_CROWN);
			if (choice < 14) return (TV_SHIELD);
			if (choice < 24) return (TV_CLOAK);
			if (choice < 27) return (TV_SOFT_ARMOR);
			if (choice < 37) return (TV_SCROLL);
			if (choice < 48) return (TV_POTION);
			if (choice < 58) return (TV_RING);
			if (choice < 68) return (TV_AMULET);
			if (choice < 74) return (TV_WAND);
			if (choice < 80) return (TV_STAFF);
			if (choice < 86) return (TV_ROD);
			if (choice < 101) return (TV_DRUID_BOOK);
			break;
		}


		case CLASS_NECRO:
		{
			if (choice < 3) return (TV_BOOTS);
			if (choice < 5) return (TV_HELM);
			if (choice < 11) return (TV_CROWN);
			if (choice < 14) return (TV_SHIELD);
			if (choice < 24) return (TV_CLOAK);
			if (choice < 27) return (TV_SOFT_ARMOR);
			if (choice < 37) return (TV_SCROLL);
			if (choice < 48) return (TV_POTION);
			if (choice < 58) return (TV_RING);
			if (choice < 68) return (TV_AMULET);
			if (choice < 74) return (TV_WAND);
			if (choice < 80) return (TV_STAFF);
			if (choice < 86) return (TV_ROD);
			if (choice < 101) return (TV_NECRO_BOOK);
			break;
		}

		case CLASS_ASSASSIN:
		{
			if (choice < 11) return (TV_SHOT);
			if (choice < 17) return (TV_ARROW);
			if (choice < 20) return (TV_BOLT);
			if (choice < 29) return (TV_BOW);
			if (choice < 33) return (TV_HAFTED);
			if (choice < 37) return (TV_POLEARM);
			if (choice < 44) return (TV_SWORD);
			if (choice < 48) return (TV_BOOTS);
			if (choice < 50) return (TV_GLOVES);
			if (choice < 54) return (TV_HELM);
			if (choice < 58) return (TV_CROWN);
			if (choice < 62) return (TV_SHIELD);
			if (choice < 68) return (TV_CLOAK);
			if (choice < 71) return (TV_SOFT_ARMOR);
			if (choice < 74) return (TV_HARD_ARMOR);
			if (choice < 80) return (TV_SCROLL);
			if (choice < 86) return (TV_POTION);
			if (choice < 92) return (TV_STAFF);
			if (choice < 101) return (TV_NECRO_BOOK);
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
 * In Oangband, chests are nice finds.  Small chests distribute 3-5 items,
 * while large chests can distribute 5-7.  Item types are biased to be
 * useful for the character, and they can frequently be of good quality 
 * (or better).  Code in object2.c helps these items be even better. -LM-
 *
 * The "value" of the items in a chest is based on the "power" of the chest,
 * which is in turn based on the level on which the chest is generated.
 */
static void chest_death(int y, int x, s16b o_idx)
{
	int number;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	/* Access chest */
	o_ptr = &o_list[o_idx];

	/* Determine how much to drop. -LM- */
	if (o_ptr->sval >= SV_CHEST_MIN_LARGE) number = 4 + randint(3);
	else number = 2 + randint(3);

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval);

	/* Select an item type that the chest will disperse. -LM- */
	required_tval = get_choice();

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
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
				if (randint(200) < object_level)
				{
					if (!make_object(i_ptr, TRUE, TRUE, TRUE)) continue;
					break;
				}

				else if (randint(40) < object_level)
				{
					if (!make_object(i_ptr, TRUE, FALSE, TRUE)) continue;
					break;
				}
				else
				{
					if (!make_object(i_ptr, FALSE, FALSE, TRUE)) continue;
					break;
				}
			}

			case TV_MAGIC_BOOK:
			case TV_PRAYER_BOOK:
			case TV_DRUID_BOOK:
			case TV_NECRO_BOOK:
			{
				if (randint(80) < object_level)
				{
					if (!make_object(i_ptr, TRUE, FALSE, TRUE)) continue;
				}

				else
				{
				 if (!make_object(i_ptr, FALSE, FALSE, TRUE)) continue;
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
				if (randint(100) < (object_level - 10) / 2)
				{
					if (!make_object(i_ptr, TRUE, FALSE, TRUE)) continue;
				}

				else
				{
					if (!make_object(i_ptr, FALSE, FALSE, TRUE)) continue;
				}

				break;
			}

			default:
			{
				if (!make_object(i_ptr, FALSE, FALSE, TRUE)) continue;
				break;
			}
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Clear this global variable, to avoid messing up object generation. -LM- */
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
	int i, trap;

	object_type *o_ptr = &o_list[o_idx];


	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
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
			(void)summon_specific(y, x, p_ptr->depth, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		take_hit(damroll(5, 8), "an exploding chest");
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
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Difficulty rating.  Tweaked to compensate for higher pvals. -LM- */
		j = i - 2 * o_ptr->pval / 3;

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
			msg_print("You failed to pick the lock.");
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

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Difficulty rating.  Tweaked to compensate for higher pvals. -LM- */
	j = i - 3 * o_ptr->pval / 4;

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

	/* Success (get a fair amount of experience) */
	else if (rand_int(100) < j)
	{
		msg_print("You have disarmed the chest.");
		gain_exp(o_ptr->pval * o_ptr->pval / 5);
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

#ifdef ALLOW_EASY_OPEN /* TNB */

/*
 * easy_open_door --
 *
 *     If there is a jammed/closed/locked door at the given location,
 *     then attempt to unlock/open it. Return TRUE if an attempt was
 *     made (successful or not), otherwise return FALSE.
 *
 *     The code here should be nearly identical to that in
 *     do_cmd_open_test() and do_cmd_open_aux().
 */

bool easy_open_door(int y, int x)
{
       int i, j;

       /* Must be a closed door */
       if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
             (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
       {
               /* Nope */
               return (FALSE);
       }

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
               i = p_ptr->skill_dis;

               /* Penalize some conditions */
               if (p_ptr->blind || no_lite()) i = i / 10;
               if (p_ptr->confused || p_ptr->image) i = i / 10;

               /* Extract the lock power */
               j = cave_feat[y][x] - FEAT_DOOR_HEAD;

               /* Extract the difficulty XXX XXX XXX */
               j = i - (j * 4);

               /* Always have a small chance of success */
               if (j < 2) j = 2;

               /* Success */
               if (rand_int(100) < j)
               {
                       /* Message */
                       msg_print("You have picked the lock.");

                       /* Open the door */
                       cave_set_feat(y, x, FEAT_OPEN);

                       /* Update some things */
                       p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

                       /* Sound */
                       sound(SOUND_OPENDOOR);

                       /* Experience */
                       gain_exp(1);
               }

               /* Failure */
               else
               {
                       /* Failure */
                       if (flush_failure) flush();

                       /* Message */
                       msg_print("You failed to pick the lock.");
               }
       }

       /* Closed door */
       else
       {
               /* Open the door */
               cave_set_feat(y, x, FEAT_OPEN);

               /* Update some things */
               p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

               /* Sound */
               sound(SOUND_OPENDOOR);
       }

       /* Result */
       return (TRUE);
}

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_dt(int *y, int *x, byte f1, byte f2)
{
       int d, count;

       /* Count how many matches */
       count = 0;

       /* Check around (and under) the character */
       for (d = 0; d < 9; d++) {

               /* Extract adjacent (legal) location */
               int yy = p_ptr->py + ddy_ddd[d];
               int xx = p_ptr->px + ddx_ddd[d];

               /* Must have knowledge */
               if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

               /* Not looking for this feature */
               if (cave_feat[yy][xx] < f1) continue;
               if (cave_feat[yy][xx] > f2) continue;

               /* OK */
               ++count;

               /* Remember the location. Only useful if only one match */
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
       for (d = 0; d < 9; d++) {

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
               if (trapped && (!object_known_p(o_ptr) ||
                       !chest_traps[o_ptr->pval])) continue;

               /* OK */
               ++count;

               /* Remember the location. Only useful if only one match */
               *y = yy;
               *x = xx;
       }

       /* All done */
       return count;
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

	/* Paranoia */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return d[dx + 1][dy + 1];
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
		msg_print("You see nothing there to open.");

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
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = cave_feat[y][x] - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			msg_print("You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			msg_print("You failed to pick the lock.");

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
		sound(SOUND_OPENDOOR);
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN /* TNB */

	/* Option: Pick a direction */
	if (easy_open) 
	{
		int num_doors, num_chests;

		/* Count closed doors (locked or jammed) */
		num_doors = count_dt(&y, &x, FEAT_DOOR_HEAD, FEAT_DOOR_TAIL);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if (num_doors || num_chests)
		{
			bool too_many = (num_doors && num_chests) || (num_doors > 1) ||
                               (num_chests > 1);
			if (!too_many) p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
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
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(SOUND_SHUTDOOR);
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

#ifdef ALLOW_EASY_OPEN /* TNB */

       /* Option: Pick a direction */
       if (easy_open) {

               /* Count open doors */
               if (count_dt(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1) {
                       p_ptr->command_dir = coords_to_dir(y, x);
               }
       }

#endif /* ALLOW_EASY_OPEN */

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Door */
	else
	{
		/* Close door */
		more = do_cmd_close_aux(y, x);
	}

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
	if (cave_floor_bold(y, x) || (cave_feat[y][x] == FEAT_TREE))
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
	if (cave_floor_bold(y, x) && (cave_feat[y][x] == FEAT_TREE)) return (FALSE);

	/* Sound */
	sound(SOUND_DIG);

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


	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);


	/* Sound XXX XXX XXX */
	/* sound(SOUND_DIG); */

	/* Titanium */
	if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
	{
		msg_print("This seems to be permanent rock.");
	}

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + rand_int(1600)) && twall(y, x))
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
			okay = (p_ptr->skill_dig > 20 + rand_int(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->skill_dig > 10 + rand_int(400));
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
		if ((p_ptr->skill_dig > rand_int(200)) && twall(y, x))
		{
			/* Message */
			msg_print("You have removed the rubble.");

			/* Hack -- place an object */
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE, 0);

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
		if ((p_ptr->skill_dig > 30 + rand_int(1200)) && twall(y, x))
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
		if ((p_ptr->skill_dig > 30 + rand_int(1200)) && twall(y, x))
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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
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
#ifdef ALLOW_EASY_DISARM /* TNB */
bool do_cmd_disarm_aux(int y, int x)
#else
static bool do_cmd_disarm_aux(int y, int x)
#endif
{
	int i, j, power;

	cptr name;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_disarm_test(y, x)) return (FALSE);


	/* Access trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power".  Increased in Oangband. */
	power = 5 + p_ptr->depth / 4;

	/* Prevent the player's own traps granting exp. -LM- */
	if (cave_feat[y][x] == FEAT_MONSTER_TRAP) power = 0;

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if (rand_int(100) < j)
	{
		/* Message */
		msg_format("You have disarmed the %s.", name);

		/* If the trap was a Rogue's monster trap, decrement the trap count. -LM-*/
		if (cave_feat[y][x] == FEAT_MONSTER_TRAP) monster_trap_on_level--;

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_DISARM /* TNB */

       /* Option: Pick a direction */
       if (easy_disarm) {

           int num_traps, num_chests;

           /* Count visible traps */
           num_traps = count_dt(&y, &x, FEAT_TRAP_HEAD, FEAT_TRAP_TAIL);

           /* Count chests (trapped) */
           num_chests = count_chests(&y, &x, TRUE);

           /* See if only one target */
           if (num_traps || num_chests) {
               bool too_many = (num_traps && num_chests) || (num_traps > 1) ||
                               (num_chests > 1);
               if (!too_many) p_ptr->command_dir = coords_to_dir(y, x);
           }
       }

#endif /* ALLOW_EASY_DISARM */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
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


	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return (FALSE);


	/* Message */
	msg_print("You smash into the door!");

	/* Make a lot of noise. -LM- */
	add_wakeup_chance = 9000;

	/* Hack -- Bash power based on strength */
	/* (Ranges from 14 to 40 to 90 to 110) */
	bash = 10 + adj_str_hold[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((cave_feat[y][x] - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 8));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Message */
		msg_print("The door crashes open!");

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

		/* Sound */
		sound(SOUND_OPENDOOR);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
	         p_ptr->lev)
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

		/* Hack -- Lose balance ala paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + 2 + rand_int(2));
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return;


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
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);

		/* Done */
		return;
	}

	/* Door */
	else
	{
		/* Bash the door */
		more = do_cmd_bash_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}



/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors, 
 * or, for rogues, set traps and steal money.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	int feat;

	bool did_nothing = TRUE;
	bool more = FALSE;

	monster_type *m_ptr;

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

	/* If a monster is present, and visible, Rogues may steal from it.  
	 * Otherwise, the player will simply attack. -LM-
	 */
	if (cave_m_idx[y][x] > 0)
	{
		if (p_ptr->pclass == CLASS_ROGUE)
		{
			m_ptr = &m_list[cave_m_idx[y][x]];
			if (m_ptr->ml) py_steal(y, x);
			else py_attack(y, x);
		}
		else py_attack(y, x);
		did_nothing = FALSE;
	}

	/* If a rogue, and the target square is a naked floor, set a trap if 
	 * one does not already exist on the level.  If one does, notify
	 * the player. -LM-
	 */
	if ((p_ptr->pclass == CLASS_ROGUE) && (cave_naked_bold(y, x)))
	{
		if (monster_trap_on_level == 0) py_set_trap(y, x);
		else msg_print("You must disarm your existing trap to free up your equipment.");
		did_nothing = FALSE;
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

	/* Oops */
	else if (did_nothing)
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
bool do_cmd_spike_test(int y, int x)
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
 * Jam a closed door with a spike.  Now takes only 4/10ths normal energy
 * if no monster is in the way. -LM-
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
		msg_print("You have no spikes!");

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


	/* Take a partial turn.  Now jamming is more useful. -LM- */
	p_ptr->energy_use = 40;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Monster.  Make the action now take a full turn */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

			p_ptr->energy_use += 60;

		/* Attack */
		py_attack(y, x);
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
	/* Assume no monster. */
	monster_type *m_ptr = 0;

	/* Access the monster, if any is present. -LM- */
	if (cave_m_idx[y][x] != 0) m_ptr = &m_list[cave_m_idx[y][x]];

	/* If a monster can be seen, it can be attacked normally.  Code in cmd1.c 
	 * controls whether a player can actually move to the destination grid. -LM-
	 */
	if ((m_ptr) && (m_ptr->ml)) return (TRUE);


	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Require open space */
	if (!cave_passable_bold(y, x))
	{
		/* Door */
		if (cave_feat[y][x] < FEAT_SECRET)
		{
#ifdef ALLOW_EASY_OPEN /* TNB */
			return (TRUE);
#endif
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
 * Helper function for the "walk" and "jump" commands
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


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
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
	move_player(dir, pickup);
}


/*
 * Walk into a grid (usually pickup)
 */
void do_cmd_walk(void)
{
	/* Move (usually pickup) */
#ifdef ALLOW_EASY_DISARM /* TNB */
	do_cmd_walk_or_jump(FALSE);
#else
	do_cmd_walk_or_jump(always_pickup);
#endif /* ALLOW_EASY_DISARM */
}


/*
 * Jump into a grid (usually do not pickup)
 */
void do_cmd_jump(void)
{
	/* Move (usually do not pickup) */
#ifdef ALLOW_EASY_DISARM /* TNB */
	do_cmd_walk_or_jump(TRUE);
#else
	do_cmd_walk_or_jump(!always_pickup);
#endif
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

	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == rand_int(50 - p_ptr->skill_fos)))
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
	if ((cave_feat[py][px] >= FEAT_SHOP_HEAD) &&
	    (cave_feat[py][px] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';
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
		if (!get_string(p, out_val, 4)) return;

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
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_ARROW:
		case TV_SKELETON:
		{
			return (40);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		{
			return (20);
		}
	}

	/* Rarely break */
	return (10);
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_cmd_fire(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, j, y, x, ty, tx;
	int tdis, thits, tmul;

	int bonus, chance, total_deadliness;

	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	int tdam, tdam_remainder, tdam_plus, tdam_whole;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];
	char m_name[80];

	int path_n = 0;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Require a usable launcher */
	if (!j_ptr->tval || !p_ptr->ammo_tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}

	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* sum all the applicable additions to Deadliness. */
	total_deadliness = p_ptr->to_d + i_ptr->to_d + j_ptr->to_d;

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}

	/* Sound */
	sound(SOUND_SHOOT);

	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Use a base distance */
	tdis = 10;

	/* Actually "fire" the object. */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Base range XXX XXX */
	tdis = 10 + 5 * tmul;

	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int visible = m_ptr->ml;

			int chance2 = chance - distance(py, px, y, x);


			/* Sleeping monsters are easier to hit. -LM- */
			if (m_ptr->csleep) sleeping_bonus = 5 + p_ptr->lev / 5;


			/* Monsters in rubble can take advantage of cover. -LM- */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in trees can take advantage of cover, except from rangers and druids. -LM- */
			if ((cave_feat[y][x] == FEAT_TREE) && 
				(p_ptr->pclass != CLASS_RANGER) && 
					(p_ptr->pclass != CLASS_DRUID))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}


			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize distance travelled) */
			if (test_hit_fire(chance2 + sleeping_bonus, r_ptr->ac + terrain_bonus, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_UNDEAD)) ||
				    (r_ptr->flags2 & (RF2_STUPID)) ||
				    (strchr("Evg", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Make some noise. -LM- */
				add_wakeup_chance = p_ptr->base_wakeup_chance / 3 + 1200;

				/* Hack -- Assassins are silent missile weapon killers. -LM- */
				if (p_ptr->pclass == CLASS_ASSASSIN) add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 4 + 600;

				/* Handle unseen monster */
				if (!visible)
				{
				;  /*  Message now in formula "critical_shot".  -LM- */
				}

				/* Handle visible monster */
				else
				{
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* The basic damage-determination formula is the same in
				 * archery as it is in melee (apart from the launcher mul-
				 * tiplier).  See formula "py_attack" in "cmd1.c" for more 
				 * details. -LM-
				 */

				/* Base damage dice. */
				tdam = i_ptr->dd;

				/* Multiply the number of damage dice by the missile weapon 
				 * multiplier.
				 */
				tdam *= tmul;

				/*  And again by any slay or brand applicable. */
				tdam *= tot_dam_aux(i_ptr, m_ptr);

				/* Determine the critical hit. */
				tdam *= critical_shot(chance2 + sleeping_bonus, 
					o_name, m_name, visible);

				/* add 2% extra per point to Deadliness.  Deadliness from 
				 * all equipment now added. 
				 */
				tdam += (tdam * (p_ptr->to_d + i_ptr->to_d + 
					j_ptr->to_d) / 50);

				/* deflate the result, and get the truncated number of dice... */
				tdam_whole = tdam / 100;

				/* but don't forget the remainder (the fractional dice). */
				tdam_remainder = tdam % 100;

				/* Use that remainder to calculate the extra damage 				 * caused by the fractional die.
				 */
				tdam_plus = tdam_remainder * damroll(1, i_ptr->ds) / 100;

				/* Roll the truncated number of dice. */
				tdam = damroll(tdam_whole, i_ptr->ds);

				/* Now, add the extra damage. */
				tdam += tdam_plus;

				/* Again, see the melee formula. */
				if (total_deadliness > 20) total_deadliness = 20;
				tdam += (((total_deadliness * 40) - (total_deadliness * 
						total_deadliness)) / 50);

				/* If a "enhance missile" spell has been cast, increase 
				 * the damage, and cancel the spell.
				 */
				if (p_ptr->confusing == 3)
				{
					tdam = 5 * tdam / 4 + 40;
					p_ptr->confusing = 0;
				}

				/* Hack - Assassins are deadly... */
				if (p_ptr->pclass == CLASS_ASSASSIN)
				{
				/* Increase damage directly (to avoid excessive total
				 * damage by granting too high a Deadliness).
				 */
					if (p_ptr->ammo_tval == TV_SHOT)
					{

						tdam += 3 * p_ptr->lev / 5;
					}
					if (p_ptr->ammo_tval == TV_ARROW)
					{
						tdam += p_ptr->lev / 2;
					}
					if (p_ptr->ammo_tval == TV_BOLT)
					{
						tdam += 2 * p_ptr->lev / 5;
					}
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Hit the monster, check for death. */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
						msg_format("%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}



/*
 * Throw an object from the pack or floor.  Now allows for throwing weapons.  
 * Unlike all other thrown objects, throwing weapons can take advantage of 
 * bonuses to Skill or Deadliness from other equipped items.
 *
 * Note: "unseen" monsters are very hard to hit.
 */
void do_cmd_throw(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdis;
	int mul, div;

	int total_deadliness;
	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	int tdam, tdam_remainder, tdam_plus, tdam_whole;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];
	char m_name[80];

	int path_n = 0;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	u32b f1, f2, f3;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}



	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Extract the thrown object's flags. */
	object_flags(i_ptr, &f1, &f2, &f3);

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}


	/* Description */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;


	/* Chance of hitting.  Other thrown objects are easier to use, but 
	 * only throwing  weapons take advantage of bonuses to Skill from 
	 * other items. -LM-
	 */
	if (f1 & (TR1_THROWING)) chance = ((p_ptr->skill_tht) + 
		((p_ptr->to_h + i_ptr->to_h) * BTH_PLUS_ADJ));
	else chance = ((3 * p_ptr->skill_tht / 2) + 
		(i_ptr->to_h * BTH_PLUS_ADJ));


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int visible = m_ptr->ml;

			/* Calculate the projectile accuracy, modified by distance. */
			int chance2 = chance - distance(py, px, y, x);


			/* If the monster is sleeping, it'd better pray there are no 
			 * Assassins nearby. -LM-
			 */
			if ((m_ptr->csleep) && (f1 & (TR1_THROWING)))
			{
				if (p_ptr->pclass == CLASS_ASSASSIN) 
					sleeping_bonus = 15 + p_ptr->lev / 2;
				else sleeping_bonus = 0;
			}

			/* Monsters in rubble can take advantage of cover. -LM- */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in trees can take advantage of cover, except from rangers and druids. -LM- */
			if ((cave_feat[y][x] == FEAT_TREE) && 
				(p_ptr->pclass != CLASS_RANGER) && 
					(p_ptr->pclass != CLASS_DRUID))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}


			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);


			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2 + sleeping_bonus, r_ptr->ac + terrain_bonus, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";


				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_UNDEAD)) ||
				    (r_ptr->flags2 & (RF2_STUPID)) ||
				    (strchr("Evg", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Make some noise. -LM- */
				add_wakeup_chance = p_ptr->base_wakeup_chance / 3 + 1200;

				/* Hack -- Assassins are silent thrown weapon killers. -LM- */
				if ((p_ptr->pclass == CLASS_ASSASSIN) && 
					(f1 & (TR1_THROWING))) add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 4 + 300;


				/* Encourage the player to throw weapons at sleeping 
				 * monsters. -LM- */
				if (sleeping_bonus)
				{ 
					if (p_ptr->pclass == CLASS_ASSASSIN) 
						msg_print("Assassin Strike!");
					else msg_print("You rudely awaken the monster!");
				}


				/* Handle unseen monster */
				if (!visible)
				{
				;  /* messages now in critical hit formula.  -LM- */
				}

				/* Handle visible monster */
				else
				{
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}


				/* sum all the applicable additions to Deadliness. */
				total_deadliness = p_ptr->to_d + i_ptr->to_d;


				/* The basic damage-determination formula is the same in
				 * throwing as it is in melee (apart from the thrown weapon 
				 * multiplier, and the ignoring of non-object bonuses to 				 * Deadliness for objects that are not thrown weapons).  See 
				 * formula "py_attack" in "cmd1.c" for more details. -LM-
				 */

				tdam = i_ptr->dd;

				/* Multiply the number of damage dice by the throwing weapon 
				 * multiplier, if applicable.  This is not the prettiest 
				 * function, but it does at least try to keep throwing 
				 * weapons competitive.
				 */
				if (f1 & (TR1_THROWING))
				{
					tdam *= 2 + p_ptr->lev / 12;

					/* Perfect Balance weapons do even more damage. */
					if (f1 & (TR1_PERFECT_BALANCE)) tdam *= 2;
				}

				/*  And again by any slay or brand applicable. */
				tdam *= tot_dam_aux(i_ptr, m_ptr);

				/* Only allow critical hits if the object is a throwing 
				 * weapon.  Note that the formula compensates for the 
				 * earlier bonus to Skill.  Otherwise, grant the default 
				 * multiplier.
				 */
				if (f1 & (TR1_THROWING)) tdam *= critical_shot
					(chance2 + sleeping_bonus, o_name, m_name, visible);
				else tdam *= 10;

				/* add 2% extra per point to total Deadliness, if the object 				 * is a throwing weapon, and the same only to the object 
				 * bonus to Deadliness otherwise.
				 */
				if (f1 & (TR1_THROWING)) tdam += 
					(tdam * (p_ptr->to_d + i_ptr->to_d) / 50);
				else tdam += (tdam * (i_ptr->to_d) / 50);

				/* deflate the result, and get the truncated number of dice... */
				tdam_whole = tdam / 100;

				/* but don't forget the remainder (the fractional dice). */
				tdam_remainder = tdam % 100;

				/* Use that remainder to calculate the extra damage 				 * caused by the fractional die.
				 */
				tdam_plus = tdam_remainder * damroll(1, i_ptr->ds) / 100;

				/* Roll the truncated number of dice. */
				tdam = damroll(tdam_whole, i_ptr->ds);

				/* Now, add the extra damage. */
				tdam += tdam_plus;

				/* Again, see the melee formula. */
				if (total_deadliness > 20) total_deadliness = 20;
				tdam += (((total_deadliness * 40) - (total_deadliness * 
						total_deadliness)) / 50);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
						msg_format("%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks).  Throwing weapons are designed 
	 * not to break.  -LM- */
	if (f1 & (TR1_THROWING)) j = (hit_body ? (breakage_chance(i_ptr) / 5) : 0);
	else j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}


