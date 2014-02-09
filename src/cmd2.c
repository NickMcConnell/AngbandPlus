
/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "cmds.h"
#include "game-cmd.h"
#include "game-event.h"


/*
 * Check if action permissible here.
 */
bool do_cmd_test(int y, int x, int action, bool message)
{
	u32b bitzero = 0x01;
	u32b flag;

	cptr act = "";

	cptr here = ((p_ptr->px == x ) && (p_ptr->py == y)) ? "here": "there";

	feature_type *f_ptr;

	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		if (message) msg_format("You see nothing %s.", here);

		/* Nope */
		return (FALSE);
	}

	/* Get memorised feature */
	f_ptr = &f_info[cave_feat[y][x]];

	switch (action)
	{
		case FS_SECRET:							break;
		case FS_OPEN:	act = " to open";		break;
		case FS_CLOSE:	act = " to close";		break;
		case FS_BASH:	act = " to bash";		break;
		case FS_SPIKE:	act = " to spike";		break;
		case FS_TUNNEL:	act = " to tunnel";		break;
		case FS_FLOOR:	act = " to set a trap on";	break;
		default: break;
	}

	if (action < FS_FLAGS2)
	{
		flag = bitzero << (action - FS_FLAGS1);
		if (!(f_ptr->f_flags1 & flag))
		{
			if (message) msg_format("You see nothing %s%s.", here, act);
		 	return (FALSE);
		}
	}

	else if (action < FS_FLAGS3)
	{
		flag = bitzero << (action - FS_FLAGS2);
		if (!(f_ptr->f_flags2 & flag))
		{
			if (message) msg_format("You see nothing %s%s.", here, act);
		 	return (FALSE);
		}
	}

	else if (action < FS_FLAGS_END)
	{
		flag = bitzero << (action - FS_FLAGS3);
		if (!(f_ptr->f_flags2 & flag))
		{
			if (message) msg_format("You see nothing %s%s.", here, act);
		 	return (FALSE);
		}
	}

	return (TRUE);
}


/*
 * Go up one level
 */
void do_cmd_go_up(cmd_code code, cmd_arg args[])
{
	char out_val[160];
	byte quest;

	int decrease = 0;

	feature_type *f_ptr= &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Verify stairs */
	if (!cave_up_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no up staircase here.");

		return;
	}

	/* Ironman */
	if (adult_ironman)
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) && quest_might_fail_if_leave_level())
	{
		sprintf(out_val, "Really risk failing your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) && quest_shall_fail_if_leave_level())
	{
		sprintf(out_val, "Really fail your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Hack -- take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Success */
	message(MSG_STAIRS_UP, 0, "You enter a maze of up staircases.");
	if (game_mode == GAME_NPPMORIA) msg_print("You pass through a one-way door.");

	/* Create a way back */
	if (adult_connected_stairs) p_ptr->create_stair = FEAT_MORE;

	/* New depth */
	decrease++;

	/*find out of entering a quest level (unusual going up)*/
	quest = quest_check(p_ptr->depth);

	/*go up another level if it is a shaft*/
	if ((f_ptr->f_flags2 & (FF2_SHAFT)) &&
	    (!quest) && (p_ptr->depth > 0))
	{
		decrease++;

		/* Create a way back (usually) */
		if (adult_connected_stairs) p_ptr->create_stair = FEAT_MORE_SHAFT;
	}

	/* Change level */
	dungeon_change_level(p_ptr->depth - decrease);
}


/*
 * Go down one level
 */
void do_cmd_go_down(cmd_code code, cmd_arg args[])
{
	byte quest;
	char out_val[160];

	int increase = 0;

	feature_type *f_ptr= &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/*find out if entering a quest level*/
	quest = quest_check(p_ptr->depth);

	/* Verify stairs */
	if (!cave_down_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) && quest_might_fail_if_leave_level())
	{
		sprintf(out_val, "Really risk failing your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) && quest_shall_fail_if_leave_level())
	{
		sprintf(out_val, "Really fail your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Hack -- take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Success */
	message(MSG_STAIRS_DOWN, 0, "You enter a maze of down staircases.");
	if (game_mode == GAME_NPPMORIA) msg_print("You pass through a one-way door.");

	/* Create a way back (usually) */
	if (adult_connected_stairs) p_ptr->create_stair = FEAT_LESS;

	/* New level */
	increase++;

	/*find out if entering a quest level*/
	quest = quest_check(p_ptr->depth);

	/* Go down a shaft if allowed */
	if ((f_ptr->f_flags2 & (FF2_SHAFT)) &&
	    (!quest) && (p_ptr->depth < MAX_DEPTH - 1))
	{
		increase++;

		/* Create a way back (usually) */
		if (adult_connected_stairs) p_ptr->create_stair = FEAT_LESS_SHAFT;
	}

	/* Change level */
	dungeon_change_level(p_ptr->depth + increase);
}


/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(cmd_code code, cmd_arg args[])
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
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Search */
	search();
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(cmd_code code, cmd_arg args[])
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
		p_ptr->redraw |= (PR_STATE | PR_SPEED | PR_STATUS);
	}
}


/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x, bool check_locked)
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
		if (o_ptr->tval != TV_CHEST) continue;

		/* Don't count special quest items */
		if (o_ptr->ident & (IDENT_QUEST)) continue;

		/* Handle the option to check if it is locked*/
		if (check_locked)
		{
			/* Ignore disarmed chests or chests with no traps. */
			if ((o_ptr->pval <= 0) || (!chest_traps[o_ptr->pval])) continue;
		}

		/*Success*/
		return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/*
 * Allocate objects upon opening a chest
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 */
static void chest_death(int y, int x, s16b o_idx)
{
	int number, quality, num;

	int chesttheme = 0;

	int minlevel = 0;

	object_type *o_ptr;

	object_type *i_ptr;

	object_type object_type_body;

	/* Get the chest */
	o_ptr = &o_list[o_idx];

	/* Determine how much to drop (see above)
	 *
	 * Small chests get 3-5 objects */

	number = 2 + randint (3);

	/* large chests get 5-7*/
	if (o_ptr->sval >= SV_CHEST_MIN_LARGE) number += 2;

	/*Jeweled chests get 7-10*/
	if (o_ptr->sval == SV_CHEST_JEWELED_LARGE) number += randint (3);

	/* Zero pval means empty chest */
	if (!o_ptr->pval) return;

	/* Opening a chest */
	object_generation_mode = OB_GEN_MODE_CHEST;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval);

	/*paranoia*/
	if (object_level < 1) object_level = 1;

	/*the theme of the chest is created during object generation*/
	chesttheme = (o_ptr->xtra1);

	/*large chests have a better chance of great times*/
	if (o_ptr->sval >= SV_CHEST_MIN_LARGE) minlevel = object_level / 4;

	/*Hack - don't wan't results over 100*/
	if ((object_level + minlevel) > 100) num = 100 - minlevel;

	else num = object_level;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/*used to determine quality of item, gets more likely
	 	 *to be great as you get deeper.
	 	 */
		quality = randint (num) + minlevel;

		/* Moria has less levels */
		if (game_mode == GAME_NPPMORIA) quality += quality / 5;

		/* Wipe the object */
		object_wipe(i_ptr);

		/*theme 1 is gold, themes 2-15 are objects*/

		if (chesttheme == DROP_TYPE_GOLD) make_gold(i_ptr);

		else if (chesttheme >= 2)
		{
			bool good, great;

			/* Regular objects in chests will become quite
			 * rare as depth approaches 5000'.
			 * All items with i > 50 are guaranteed great,
			 * all items with i > 80  get 4 chances
			 * to become an artifact.
		 	 * Chests should be extremely lucrative
			 * as a player approaches 5000'.
			 * For potions, scrolls, and wands, having the
			 * good and great flags checked increase the
			 * max object generation level, but have no
			 * other effect.  JG
		 	 */
			if (quality < 26)
			{
				good = FALSE;
				great = FALSE;
			}
		    else if (quality < 51)
			{
				good = TRUE;
			 	great = FALSE;
			}
			else if (quality < 81)
			{
				good = FALSE;
			 	great = TRUE;
			}
			else
			{
				good = TRUE;
				great = TRUE;
			}

			while (!make_object(i_ptr, good, great, chesttheme, FALSE)) continue;

			/* Remember history */
			object_history(i_ptr, ORIGIN_CHEST, 0);

			/* Hack -- Remember depth of the chest */
			i_ptr->origin_dlvl = o_ptr->origin_dlvl;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = effective_depth(p_ptr->depth);

	/* No longer opening a chest */
	object_generation_mode = OB_GEN_MODE_NORMAL;

	/* Empty */
	o_ptr->pval = 0;

	/*Paranoia, delete chest theme*/
	o_ptr->xtra1 = 0;

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
		if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
		{
			(void)inc_timed(TMD_POISONED, 10 + randint(20), TRUE);
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->state.free_act)
		{
			(void)inc_timed(TMD_PARALYZED, 10 + randint(20), TRUE);
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");
		sound(MSG_SUM_MONSTER);
		for (i = 0; i < num; i++)
		{
			(void)summon_specific(y, x, p_ptr->depth, 0, MPLACE_OVERRIDE);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		o_ptr->xtra1 = 0;
		take_hit(damroll(5, 8), "an exploding chest");

		/* squelch chest */
		delete_object_idx(o_idx);
		msg_print("The chest is destroyed.");
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

	/* paranoia - make sure it is a chest */
	if (o_ptr->tval != TV_CHEST)
	{
		msg_print("This object is not a chest!");
		return (FALSE);
	}

	if (o_ptr->ident & (IDENT_QUEST))
	{
		msg_print("This chest cannot be opened!");
		return (FALSE);
	}

	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->state.skills[SKILL_DISARM];

		/* Penalize some conditions */
		if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
		if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (rand_int(100) < j)
		{
			message(MSG_LOCKPICK, 0, "You have picked the lock.");
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

		/* squelch chest */
		delete_object_idx(o_idx);
		msg_print("Chest squelched after it was opened.");

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
	i = p_ptr->state.skills[SKILL_DISARM];

	/* Penalize some conditions */
	if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

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
		message(MSG_DISARM, 0, "You have disarmed the chest.");
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


/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 * ANDY - Counts features that allow action.
 */
static int count_feats(int *y, int *x, int action)
{
	int d, count;

	feature_type *f_ptr;

	u32b flag, bitzero = 0x01;

	/* Count how many matches */
	count = 0;

	/* Check around the character */
	for (d = 0; d < 8; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

		/* Get the mimiced feature */
		f_ptr = &f_info[cave_feat[yy][xx]];

		if (action < FS_FLAGS2)
		{
			flag = bitzero << (action - FS_FLAGS1);
			if (!(f_ptr->f_flags1 & flag)) continue;
		}

		else if (action < FS_FLAGS3)
		{
			flag = bitzero << (action - FS_FLAGS2);
			if (!(f_ptr->f_flags2 & flag)) continue;
		}

		else if (action < FS_FLAGS_END)
		{
			flag = bitzero << (action - FS_FLAGS3);
			if (!(f_ptr->f_flags3 & flag)) continue;
		}

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
 * Return the number of traps or glyphs around (or under) the character.
 * If requested, count only known traps.
 */
static int count_traps(int *y, int *x, bool known)
{
	int d, count;

	effect_type *x_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* No trap effect is there */
		if (!cave_any_trap_bold(yy, xx)) continue;

		/* Grab the object */
		x_ptr = &x_list[cave_x_idx[yy][xx]];

		/* Hidden */
		if ((known) && (x_ptr->x_flags & (EF1_HIDDEN))) continue;

		/* Count it */
		++count;

		/* Remember the location of the last trap found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return (count);
}


/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
int count_chests(int *y, int *x, bool trapped)
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
		if ((o_idx = chest_check(yy, xx, trapped)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Hack - Don't open mimic chests */
		if (o_ptr->mimic_r_idx) continue;

		/* Don't count special quest items */
		if (o_ptr->ident & (IDENT_QUEST)) continue;

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

	int feat = cave_feat[y][x];

	int door_power;

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_OPEN, TRUE)) return (FALSE);

	/* Secrets on doors */
	if (feat_ff1_match(feat, FF1_DOOR | FF1_SECRET) == (FF1_DOOR | FF1_SECRET))
	{
		/* Reveal */
		find_secret(y, x);

		/* Get the new door */
		feat = cave_feat[y][x];

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Jammed door */
	if (feat_ff3_match(feat, FF3_DOOR_JAMMED))
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (feat_ff3_match(feat, FF3_DOOR_LOCKED) &&
		((door_power = feat_state_power(feat, FS_OPEN)) > 0))
	{
		/*Mark the feature lore*/
		feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
		f_l_ptr->f_l_flags1 |= (FF1_CAN_OPEN);

		/* Disarm factor */
		i = p_ptr->state.skills[SKILL_DISARM];

		/* Penalize some conditions */
		if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
		if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

		/* Extract the lock power */
		/* door_power must be between 1 and 7 */
		j = i - (door_power * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			message(MSG_LOCKPICK, 0, "You have picked the lock.");

			/* Open the door */
			cave_alter_feat(y, x, FS_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

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
		cave_alter_feat(y, x, FS_OPEN);

		/* Update the visuals */
		p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);

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
void do_cmd_open(cmd_code code, cmd_arg args[])
{
	int cy, cx, y, x;
	int dir = args[0].direction;

	/* Count chests (locked) */
	int num_chests, o_idx;

	bool more = FALSE;

	dir = args[0].direction;

	/* Get location */
	cy = y = p_ptr->py + ddy[dir];
	cx = x = p_ptr->px + ddx[dir];

	/* Check for chests */
	num_chests = count_chests(&y, &x, FALSE);
	o_idx = chest_check(y, x, FALSE);

	/* Verify legality */
	if (!o_idx && !do_cmd_test(y, x, FS_OPEN, TRUE)) return;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		cy = y = p_ptr->py + ddy[dir];
		cy = x = p_ptr->px + ddx[dir];

		/* Check for chest */
		num_chests = count_chests(&y, &x, FALSE);
		o_idx = chest_check(y, x, FALSE);
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
	else if (num_chests)
	{
		/* Get top chest */
		o_idx = chest_check(y, x, FALSE);

		/* Open the chest if confused, or only one */
		if ((p_ptr->timed[TMD_CONFUSED]) || (num_chests == 1))  more = do_cmd_open_chest(y, x, o_idx);

		/* More than one */
		else
		{
			cptr q, s;

			o_idx = 0;

			/* Get an item */
			q = "Open which chest? ";
			s = "There are no chests in that direction!";

			/*clear the restriction*/
			item_tester_hook = obj_is_openable_chest;

			/*player chose escape*/
			if (!get_item_beside(&o_idx, q, s, cy, cx)) more = 0;

			/* Open the chest */
			else more = do_cmd_open_chest(cy, cx, -o_idx);
		}
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(cy, cx);
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}

void textui_cmd_open(void)
{
	int y, x;
	int dir = DIR_UNKNOWN;

	/* Easy Open */
	if (easy_open)
	{
		int num_doors, num_chests;

		/* Count closed doors */
		num_doors = count_feats(&y, &x, FS_OPEN);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			dir = coords_to_dir(y, x);
		}
	}

	cmd_insert(CMD_OPEN, dir);
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

	int feat = cave_feat[y][x];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_CLOSE, TRUE)) return (FALSE);

	/* Broken door */
	if (feat_ff3_match(feat, FF3_DOOR_BROKEN))
	{
		/* Message */
		msg_print("The door appears to be broken.");
	}

	/* Secrets on door/permanent doors */
	else if (feat_ff1_match(feat, FF1_SECRET | FF1_PERMANENT))
	{
		/* Stuck */
		find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);
	}

	/* Open door */
	else
	{
		/*Mark the feature lore*/
		feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
		f_l_ptr->f_l_flags1 |= (FF1_CAN_CLOSE);

		/* Close the door */
		cave_alter_feat(y, x, FS_CLOSE);

		/* Update the visuals */
		p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);

		/* Sound */
		sound(MSG_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(cmd_code code, cmd_arg args[])
{
	int y, x, dir;

	bool more = FALSE;

	dir = args[0].direction;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_CLOSE, TRUE))
	{
		/* Cancel repeat */
		disturb(0, 0);
		return;
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Apply confusion */
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


void textui_cmd_close(void)
{
	int y, x, dir = DIR_UNKNOWN;

	/* Easy Close */
	if (easy_open)
	{
		/* Count open doors */
		if (count_feats(&y, &x, FS_CLOSE) == 1)
		{

			dir = coords_to_dir(y, x);

		}
	}
	else
	{
		if (!get_rep_dir(&dir))
			return;
	}


	cmd_insert(CMD_CLOSE, dir);
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

	int feat;

	char name[80];

	int j;

	feat = cave_feat[y][x];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_TUNNEL, TRUE)) return (FALSE);

	j = feat_state_power(feat, FS_TUNNEL);

	/* Sound XXX XXX XXX */
	/* sound(MSG_DIG); */

	/* Make some noise. */
	add_wakeup_chance = 1000;

	/* Permanent doors/rock */
	if (cave_ff1_match(y, x, FF1_PERMANENT))
	{
		/* Stuck */
		find_secret(y, x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Silent watchers */
	else if (feat == FEAT_SILENT_WATCHER)
	{
		/* Hurt the player */
		hit_silent_watcher(y, x);
	}

	/* Dig or tunnel */
	else if (cave_ff1_match(y, x, FF1_CAN_TUNNEL))
	{

		/*Mark the feature lore*/
		feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
		f_l_ptr->f_l_flags1 |= (FF1_CAN_TUNNEL);

		/* Dig */
		if (p_ptr->state.skills[SKILL_DIGGING] > rand_int(40* j))
		{
			sound(MSG_DIG);

			/* Get the name */
			feature_desc(name, sizeof(name), feat, FALSE, TRUE);

			/* Give the message */
			msg_format("You have removed the %s.", name);

			cave_alter_feat(y, x, FS_TUNNEL);

			/* Update the visuals */
			p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Take damage from the feature sometimes */
		else if ((f_info[feat].dam_non_native > 0) &&
			!is_player_native(y, x) && one_in_(50))
		{
			cptr kb_str;

			/* Get the name */
			feature_desc(name, sizeof(name), feat, TRUE, TRUE);

			/* Format the killer string */
			kb_str = format("digging %s", name);

			/* Take the hit */
			take_terrain_hit(f_info[feat].dam_non_native, feat, kb_str);
		}

		/* Keep trying */
		else
		{
			/* Get the name */
			feature_desc(name, sizeof(name), feat, FALSE, TRUE);

			/* We may continue tunneling */
			msg_format("You dig into the %s.", name);

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
void do_cmd_tunnel(cmd_code code, cmd_arg args[])
{
	int y, x, dir;

	bool more = FALSE;

	dir = args[0].direction;

	/* Easy Tunnel */
	if (easy_open)
	{
		/* Handle a single open door */
		if (count_feats(&y, &x, FS_TUNNEL) == 1)
		{
			/* Don't close door player is on */
			if ((y != p_ptr->py) || (x != p_ptr->px))
			{
				p_ptr->command_dir = coords_to_dir(y, x);
			}
		}
	}

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Oops */
	if (!do_cmd_test(y, x, FS_TUNNEL, TRUE)) return;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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


void textui_cmd_tunnel(void)
{
	int dir;
	if (!get_rep_dir(&dir)) return;
	cmd_insert(CMD_TUNNEL, dir);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(int y, int x, bool disarm)
{
	int i, j, power;

	cptr act;

	char name[80];

	bool more = FALSE;

	int feat;

	feature_lore *f_l_ptr;

	/* Arm or disarm */
	if (disarm) act = "disarm";
	else act = "arm";

	/* Verify legality */
	if (!cave_any_trap_bold(y, x)) return (FALSE);

	feat = x_list[cave_x_idx[y][x]].x_f_idx;

	f_l_ptr = &f_l_list[feat];

	/* Get the trap name */
	feature_desc(name, sizeof(name), feat, FALSE, TRUE);

	/* Get the "disarm" factor */
	i = p_ptr->state.skills[SKILL_DISARM];

	/* Penalize some conditions */
	if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = 5 + effective_depth(p_ptr->depth) / 4;

	/* Prevent the player's own traps granting exp. */
	if (feat_ff2_match(feat, FF2_TRAP_MON)) power = 0;

	/* Prevent glyphs of warding granting exp. */
	if  (feat_ff1_match(feat, FF1_GLYPH)) power = 0;

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/*Mark the feature lore*/
	f_l_ptr->f_l_flags1 |= (FF1_CAN_DISARM);

	/* Success, always succeed with player trap or glyphs */
	if (feat_ff2_match(feat, FF2_TRAP_MON) ||
		feat_ff1_match(feat, FF1_GLYPH) || (rand_int(100) < j))
	{

		/* Special message for glyphs. */
		if  (feat_ff1_match(feat, FF1_GLYPH))
			msg_format("You have desanctified the %s.", name);

		/* Normal message otherwise */
		else msg_format("You have %sed the %s.", act, name);

		/* If a Rogue's monster trap, decrement the trap count. */
		if (feat_ff2_match(feat, FF2_TRAP_MON)) num_trap_on_level--;

		/* Reward */
		gain_exp(power);

		/* Disarm */
		delete_effect_idx(cave_x_idx[y][x]);

		/* Forget the trap */

		cave_info[y][x] &= ~(CAVE_MARK);

		/* Check if the grid is still viewable */
		note_spot(y, x);

		light_spot(y, x);

	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msg_format("You failed to %s the %s.", act, name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else if (cave_passive_trap_bold(y, x))
	{
		/* Message */
		msg_format("You set off the %s!", name);

		/* Hit the trap */
		hit_trap(feat, y, x, MODE_ACTION);
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or a chest
 */
void do_cmd_disarm(cmd_code code, cmd_arg args[])
{
	int dir = args[0].direction;
	int dir_y, dir_x, chest_y, chest_x, trap_y, trap_x;

	int num_traps, o_idx, num_chests;

	bool more = FALSE;

	/* Get location */
	chest_y = trap_y = dir_y = p_ptr->py + ddy[dir];
	chest_x = trap_x = dir_x = p_ptr->px + ddx[dir];

	/* Count visible traps */
	num_traps = count_traps(&trap_y, &trap_x, TRUE);

	/* Count chests (trapped) */
	num_chests = count_chests(&chest_y, &chest_x, TRUE);

	/* Check for trapped chests */
	o_idx = chest_check(chest_y, chest_x, TRUE);

	/* Verify legality */
	if (!num_traps && !num_chests) return;

	/* Easy Disarm */
	if (easy_open)
	{
		/* See if only one target */
		if ((num_traps + num_chests) == 1)
		{
			if (num_traps)
			{
				dir_y = trap_y;
				dir_x = trap_x;
			}
			else  /* (num_chests) */
			{
				dir_y = chest_y;
				dir_x = chest_x;
			}

			p_ptr->command_dir = coords_to_dir(dir_y, dir_x);
		}
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		chest_y = trap_y = dir_y = p_ptr->py + ddy[dir];
		chest_x = trap_x = dir_x = p_ptr->px + ddx[dir];

		/* re-count the chests and traps */
		num_traps = count_traps(&trap_y, &trap_x, TRUE);

		num_chests= count_chests(&chest_y, &chest_x, TRUE);
		o_idx = chest_check(chest_y, chest_x, TRUE);

		/* Verify legality */
		if (!num_traps && !num_chests)
		{
			msg_print("You are too confused!");
			return;
		}

		if (num_chests)
		{
			dir_y = chest_y;
			dir_x = chest_x;
		}
		else  /* (num_traps) */
		{
			dir_y = trap_y;
			dir_x = trap_x;
		}
	}
	/* One final check to see if we are opening a chest or a trap, if both are present */
	else if ((num_chests) && (num_traps))
	{
		/* Did the player initially specify a trap or a chest? */
		if (cave_any_trap_bold(dir_y, dir_x))
		{
			num_chests = 0;
			trap_y = dir_y;
			trap_x = dir_x;

		}
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
	if (cave_m_idx[dir_y][dir_x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(dir_y, dir_x);
	}

	/* Chest */
	else if (num_chests)
	{

		/* Disarm the chest if confused, or only one */
		if ((p_ptr->timed[TMD_CONFUSED]) || (num_chests == 1))
		{
			more = do_cmd_disarm_chest(chest_y, chest_x, o_idx);
		}

		/* More than one */
		else
		{
			cptr q, s;
			o_idx = 0;

			/* Get an item */
			q = "Disarm which chest? ";
			s = "There are no trapped chests in that direction!";

			/*clear the restriction*/
			item_tester_hook = chest_requires_disarming;

			/*player chose escape*/
			if (!get_item_beside(&o_idx, q, s, chest_y, chest_x)) more = 0;

			/* Disarm the chest */
			else more = do_cmd_disarm_chest(chest_y, chest_x, -o_idx);
		}
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_cmd_disarm_aux(trap_y, trap_x, TRUE);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


void textui_cmd_disarm(void)
{
	int y, x, dir;

	dir = DIR_UNKNOWN;

	/* Easy Disarm */
	if (easy_open)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_traps(&y, &x, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps || num_chests)
		{
			if (num_traps + num_chests <= 1)
			{
				dir = coords_to_dir(y, x);
			}

		}
	}
	else
	{
		if (!get_rep_dir(&dir))
			return;
	}

	cmd_insert(CMD_DISARM, dir);
}

static bool player_bash(int y, int x)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
	monster_race *r_ptr;
	char m_name[80];
	object_type *o_ptr = &inventory[INVEN_ARM];

	/* Chance to hit based on strength and weight */
	int base_to_hit = p_ptr->state.stat_ind[A_STR] + o_ptr->weight / 2 + p_ptr->total_weight/10;

	/* Paranoia */
	if (!(cave_m_idx[y][x] > 0)) return (FALSE);

	m_ptr = &mon_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Boundry control */
	if (base_to_hit < 4)  base_to_hit = 4;

	if (test_hit(base_to_hit, r_ptr->ac, m_ptr->ml))
	{
		int dd = 4;
		int ds = (base_to_hit / 4);
		int plus = p_ptr->state.to_d;
		int damage;
		bool fear = FALSE;

		msg_print(format("You bash %s.", m_name));

		/* Allow for a critical hit */
		(void)critical_hit_check(o_ptr, &dd, &plus);

		damage = damroll(dd, ds) + plus;

		/* Monster is still alive */
		if (!mon_take_hit(cave_m_idx[y][x], damage, &fear, NULL, SOURCE_PLAYER))
		{
			/* Reduce its energy (half-paralysis) */
			m_ptr->m_energy = BASE_ENERGY_MOVE / 2;
			if (!m_ptr->m_timed[MON_TMD_STUN])
			{
				(void)mon_inc_timed(cave_m_idx[y][x], MON_TMD_STUN, (randint(3) + 1), MON_TMD_FLG_NOTIFY);
			}
		}

		return (TRUE);
	}

	else msg_print(format("You miss %s.", m_name));

	/* High dexterity yields coolness */
	if (randint1(150) < p_ptr->state.stat_ind[A_DEX])
	{
		/* Message */
		msg_print("You retain your balance.");
	}
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		/* Hack -- Lose balance aka paralysis */
		(void)set_timed(TMD_PARALYZED, 2 + randint(2), FALSE);
	}

	return (FALSE);
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

	int feat = cave_feat[y][x];

	char name[80];

	feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_BASH, TRUE)) return (FALSE);

	/* Get the name */
	feature_desc(name, sizeof(name), feat, FALSE, TRUE);

	/* Message */
	msg_format("You smash into the %s!", name);

	/* Make a lot of noise. */
	add_wakeup_chance = 9000;

	/* Secrets on doors */
	if (feat_ff1_match(feat, FF1_DOOR | FF1_SECRET) == (FF1_DOOR | FF1_SECRET))
	{
		/* Reveal */
		find_secret(y, x);

		/* Get the new door */
		feat = cave_feat[y][x];

		/* Update the visuals */
		p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3-20 with step=1 and 20-240 with step=10) */
	/* (A character with 18/00 STR gets 20)*/
	bash = adj_str_blow[p_ptr->state.stat_ind[A_STR]];

	/* Extract door power (must be between 0 and 6) */
	temp = feat_state_power(feat, FS_BASH);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/*Mark the feature lore*/
	f_l_ptr->f_l_flags1 |= (FF1_CAN_BASH);

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{

		/* Break down the door */
		if (!feat_ff1_match(feat, FF1_CAN_OPEN) || one_in_(2))
		{
			cave_alter_feat(y, x, FS_BASH);
		}

		/* Open the door */
		else
		{
			cave_alter_feat(y, x, FS_OPEN);
		}

		/* Message */
		if (feat_ff1_match(f_info[feat].f_mimic, FF1_DOOR))
		{
			message(MSG_OPENDOOR, 0, "The door crashes open!");
		}
		else
		{
			msg_format("The %s crashes!", name);
		}

		/* Update the visuals */
		p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Take a hit from the feature, sometimes */
	else if ((f_info[feat].dam_non_native > 0) &&
		!is_player_native(y, x) && one_in_(3))
	{
		cptr kb_str;

		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		/* Format the killer string */
		kb_str = format("bashing %s", name);

		/* Take the hit */
		take_terrain_hit(f_info[feat].dam_non_native, feat, kb_str);
	}

	/* Some features can't stun the player */
	else if (cave_passable_bold(y, x))
	{
		/* Message */
		msg_format("The %s remains intact.", name);

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] +
	         p_ptr->lev)
	{
		/* Message */
		msg_format("The %s holds firm.", name);

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		/* Hack -- Lose balance ala paralysis */
		(void)inc_timed(TMD_PARALYZED, 2 + rand_int(2), TRUE);
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
void do_cmd_bash(cmd_code code, cmd_arg args[])
{
	int y, x, dir;

	dir = args[0].direction;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* In Moria, possibly bash a monster */
	if ((cave_m_idx[y][x] > 0) && (game_mode == GAME_NPPMORIA))
	{
		/* Do nothing except avoid the next check */
	}

	/* Verify legality */
	else if (!do_cmd_test(y, x, FS_BASH, TRUE)) return;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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
		player_bash(y, x);
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


void textui_cmd_bash(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_BASH, dir);
}

/*
 * Attempt to set or modify a trap
 */
void do_cmd_make_trap(cmd_code code, cmd_arg args[])
{
	int max_traps;

	int dir = args[0].direction;

	/* Get location */
	int y = p_ptr->py + ddy[dir];
	int x = p_ptr->px + ddx[dir];

	/* Oops */
	if (!(cp_ptr->flags & CF_SET_TRAPS))
	{
		msg_print("You don't have the ability to set traps!");
		return;
	}

	/* Hack XXX XXX XXX */
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE])
	{
		msg_print("You are too confused!");
		return;
	}

	if (!in_bounds(y, x)) return;

	if (cave_trappable_bold(y, x) && !cave_m_idx[y][x])
	{
		/*two traps for advanced rogues, one for lower level*/
		if (p_ptr->lev >= 26) max_traps = 2;
		else max_traps = 1;

		/*if not at max traps, set a trap, else fail*/
		if (num_trap_on_level < max_traps)
		{
			py_set_trap(y, x);

			/* Take a turn */
			p_ptr->p_energy_use = BASE_ENERGY_MOVE;
		}
		else
		{
			/*give a message and don't burn any energy*/
			msg_print("You must disarm an existing trap to free up your equipment.");

			return;
		}
	}

	/* Only rogues can modify basic monster traps */
	else if (cave_monster_trap_bold(y, x))
	{
		/* Modify */
		if (!py_modify_trap(y, x))return;

		/* Take a turn */
		else p_ptr->p_energy_use = BASE_ENERGY_MOVE;
	}

	/*empty floor space*/
	else msg_print("You can not set a trap here.");
}

void textui_cmd_make_trap(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_MAKE_TRAP, dir);
}

/*
 * Try to steal from a monster
 */
void do_cmd_steal(cmd_code code, cmd_arg args[])
{
	int dir = args[0].direction;
	int y = p_ptr->py + ddy[dir];
	int x = p_ptr->px + ddx[dir];

	monster_type *m_ptr;

	/* Oops */
	if (!(cp_ptr->flags & CF_ROGUE_COMBAT))
	{
		msg_print("Theft isn't one of your talents!");
		return;
	}

	/* Hack XXX XXX XXX */
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE])
	{
		msg_print("You are too confused!");
		return;
	}

	/* Check bounds */
	if (!in_bounds(y, x)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* If a monster is present, and visible, rogues may steal from it.
	 * Otherwise, the player will simply attack.
	 */
	if (cave_m_idx[y][x] > 0)
	{
		m_ptr = &mon_list[cave_m_idx[y][x]];

		if (m_ptr->ml)
		{
			py_steal(y, x);

			/* Take a turn */
			p_ptr->p_energy_use = BASE_ENERGY_MOVE;

		}

		else msg_print("You don't see anything to steal from.");
	}

	else msg_print("You don't see anything to steal from.");
}


void textui_cmd_steal(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_STEAL, dir);
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
void do_cmd_alter(cmd_code code, cmd_arg args[])
{
	do_cmd_alter_aux(args[0].direction);
}


void do_cmd_alter_aux(int dir)
{
	int y, x;

	u16b feat;

	bool more = FALSE;

	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Original feature */
	feat = cave_feat[y][x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) feat = FEAT_NONE;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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

	/*Is there a monster on the space?*/
	if (cave_m_idx[y][x] > 0)
	{
		py_attack(y, x);
	}

	/* Tunnel through walls */
	else if (feat_ff1_match(feat, FF1_DOOR | FF1_CAN_TUNNEL) ==	(FF1_CAN_TUNNEL))
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}
#if 0
	/* Bash jammed doors */
	else if (feat_ff1_match(feat, FF1_CAN_BASH))
	{
		/* Bash */
		more = do_cmd_bash_aux(y, x);
	}
#endif
	/* Open closed doors */
	else if (feat_ff1_match(feat, FF1_CAN_OPEN))
	{
		/* Open */
		more = do_cmd_open_aux(y, x);
	}

	/* Disarm traps */
	else if (cave_any_trap_bold(y, x))
	{
		/* Disarm */
		more = do_cmd_disarm_aux(y, x, TRUE);
	}

#if 0

	/* Close open doors */
	else if (feat_ff1_match(feat, FF1_CAN_CLOSE))
	{
		/* Close */
		more = do_cmd_close_aux(y, x);
	}

#endif

	/* Oops */
	else
	{
		/* Oops */
		msg_print("You spin around.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


void textui_cmd_alter(void)
{
	int dir;

	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_ALTER, dir);
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
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(cmd_code code, cmd_arg args[])
{
	int y, x, dir, item = 0;

	dir = args[0].direction;

	/* Get a spike */
	if (!get_spike(&item))
	{
		/* Message */
		msg_print("You have no spikes!");

		/* Done */
		return;
	}

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_SPIKE, TRUE)) return;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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
		py_attack(y, x);
	}

	/* Go for it */
	else
	{
		int feat = cave_feat[y][x];

		char name[80];

		/*Mark the feature lore*/
		feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
		f_l_ptr->f_l_flags1 |= (FF1_CAN_SPIKE);

		/* Verify legality */
		if (!do_cmd_test(y, x, FS_SPIKE, TRUE)) return;

		/* Secrets on door/permanent doors */
		if (feat_ff1_match(feat, FF1_SECRET | FF1_PERMANENT))
		{
			/* Stuck */
			find_secret(y,x);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Get the name */
		feature_desc(name, sizeof (name), feat, FALSE, TRUE);

		/* Successful jamming */
		msg_format("You jam the %s with a spike.", name);

		/*Spike the door*/
		cave_alter_feat(y, x, FS_SPIKE);

		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}


void textui_cmd_spike(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_JAM, dir);
}


/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int y, int x)
{
	int feat;

	char name[80];

	/* Get feature */
	feat = cave_feat[y][x];

	/* Get mimiced feature Playtesting */
	/*feat = f_info[feat].f_mimic;*/

	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Allow attack on visible monsters */
	if ((cave_m_idx[y][x] > 0) && (mon_list[cave_m_idx[y][x]].ml))
	{
		return TRUE;
	}

	/* Known unpassable grids */
	else if (!feat_ff1_match(feat, FF1_MOVE))
	{
		/* Check presence of interesting walls */
		if (hit_wall(y, x, FALSE)) return (TRUE);

		/* Some doors are allowed */
		if (easy_alter)
		{
			if (feat_ff1_match(feat, FF1_CAN_OPEN)) return (TRUE);
		}

		/* Get the name */
		feature_desc(name, sizeof (name), feat, TRUE, TRUE);

		/* Message */
		message_format(MSG_HITWALL, 0, "There is %s in the way.",name);

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Return TRUE if the feature located in the given location is dangerous for the
 * player and he/she doesn't want to walk over/touch it.
 * Return FALSE if a monster occupies that grid.
 */
static bool found_dangerous_grid(int y, int x)
{
	u16b feat = cave_feat[y][x];
	int gf_type;

	/* Grid is occupied by a monster. Perform a melee attack */
	if (cave_m_idx[y][x] > 0) return (FALSE);

	/* Flying entities aren't affected by terrain */
	if (p_ptr->timed[TMD_FLYING] && feat_ff2_match(feat, FF2_CAN_FLY)) return (FALSE);

	/* Player is native to that feature or feature is harmless */
	if ((f_info[feat].dam_non_native < 1) || is_player_native(y, x)) return (FALSE);

	/* Get the spell type */
	get_spell_type_from_feature(feat, &gf_type, NULL);

	/* Player is harmless to that spell type */
	if (is_player_immune(gf_type)) return (FALSE);

	/* Stop running */
	disturb(0, 0);

	/* Ask the player for confirmation */
	if (get_check(format("It seems dangerous. Do you want to %s that grid? ",
		feat_ff1_match(feat, FF1_MOVE) ? "walk over": "touch"))) return (FALSE);

	/* Dangerous */
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
	if (!p_ptr->timed[TMD_CONFUSED])
	{
		/* Can the player walk over there? */
		if (!do_cmd_walk_test(y, x)) return;

		/* Dangerous grid? */
		if (found_dangerous_grid(y, x)) return;
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Verify legality */
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

	/* Move the player, record energy used */
	p_ptr->p_energy_use = move_player(dir, jumping);
}


/*
 * Walk into a grid.
 */
void do_cmd_walk(cmd_code code, cmd_arg args[])
{
	/* Move (normal) */
	do_cmd_walk_or_jump(FALSE);
}


/*
 * Tell the game we want to walk - in future we might want to supply
 * directions here rather than rely on keymap/macro things.
 */
void textui_cmd_walk(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_WALK, dir);
}


/*
 * Jump into a grid.
 */
void do_cmd_jump(cmd_code code, cmd_arg args[])
{
	/* Move (jump) */
	do_cmd_walk_or_jump(TRUE);
}

void textui_cmd_jump(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_JUMP, dir);
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(cmd_code code, cmd_arg args[])
{
	int y, x, dir;

	dir = args[0].direction;

	/* Hack XXX XXX XXX */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Start run */
	run_step(dir);
}


void textui_cmd_run(void)
{
	int dir;
	if (!get_rep_dir(&dir))
		return;

	cmd_insert(CMD_RUN, dir);
}


/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_hold(cmd_code code, cmd_arg args[])
{

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Spontaneous Searching */
	if ((p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] >= 50) ||
		(0 == rand_int(50 - p_ptr->state.skills[SKILL_SEARCH_FREQUENCY])))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}

	/* Handle "objects" */
	py_pickup(always_pickup);

	/* Hack -- enter a store if we are on one */
	if (cave_shop_bold(p_ptr->py,p_ptr->px))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';

		/* Free turn XXX XXX XXX */
		p_ptr->p_energy_use = 0;
	}
}


/*
 * Start running with pathfinder.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_pathfind(cmd_code code, cmd_arg args[])
{
	/* Hack XXX XXX XXX */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}

	if (findpath(args[0].point.y, args[0].point.x))
	{
		p_ptr->running = 1000;
		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
		p_ptr->running_withpathfind = TRUE;
		run_step(0);
	}
}


/*
 * Pick up objects on the floor beneath you.  -LM-
 */
void do_cmd_pickup(cmd_code code, cmd_arg args[])
{

	(void)code;
	(void)args[0].choice;

	do_cmd_pickup_from_pile(TRUE, TRUE);

	/*
	 * Intentionally use some energy in case
	 * the player is intentionally using a turn.
	 */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;
}


/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(cmd_code code, cmd_arg args[])
{
	/* Save the rest code */
	switch (args[0].choice)
	{
		case REST_ALL:
		{
			p_ptr->resting = -2;
			break;
		}

		case REST_ALL_POINTS:
		{
			p_ptr->resting = -1;
			break;
		}

		case REST_HP_MAXED:
		{
			p_ptr->resting = -3;
			break;
		}

		case REST_SP_MAXED:
		{
			p_ptr->resting = -4;
			break;
		}

		default:
		{
			p_ptr->resting = p_ptr->command_arg;
			break;
		}
	}

	/* Take a turn XXX XXX XXX (?) */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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
	Term_fresh();
}


void textui_cmd_rest(void)
{
  	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, 'h' for HP, 's' for SP, '*' for HP and SP, '&' as needed): ";

		char out_val[5] = "& ";

		/* Buttons */
		button_kill_all();
		button_add("[Rest-all]", '&');
		button_add("[Rest-HP&SP]", '*');
		button_add("[Rest-HP]", 'h');
		button_add("[Rest-SP]", 's');
		event_signal(EVENT_MOUSEBUTTONS);

		/* Ask for duration */
		if (!get_string(p, out_val, sizeof(out_val))) return;

		/* Restore normal buttons */
		basic_buttons();
		event_signal(EVENT_MOUSEBUTTONS);

		/* Rest until done */
		if (out_val[0] == '&')
		{
			cmd_insert(CMD_REST, REST_ALL);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			cmd_insert(CMD_REST, REST_ALL_POINTS);
		}

		/* Rest until HP or SP filled */
		else if (out_val[0] == 'h')
		{
			cmd_insert(CMD_REST, REST_HP_MAXED);
		}

		/* Rest until HP or SP filled */
		else if (out_val[0] == 's')
		{
			cmd_insert(CMD_REST, REST_SP_MAXED);
		}

		/* Rest some */
		else
		{
			p_ptr->command_arg = atoi(out_val);
			if (p_ptr->command_arg <= 0) return;
			if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;

			cmd_insert(CMD_REST, REST_TURNS);
		}
	}
}


/*
 * Hack -- commit suicide
 */
void do_cmd_suicide(cmd_code code, cmd_arg args[])
{
	/* Commit suicide */
	p_ptr->is_dead = TRUE;

	/* Stop playing */
	p_ptr->playing = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;

	/* Cause of death */
	my_strcpy(p_ptr->died_from, "Quitting", sizeof(p_ptr->died_from));
}


void textui_cmd_suicide(void)
{
	/* Flush input */
	flush();

	/* Verify Retirement */
	if (p_ptr->total_winner)
	{
		/* Verify */
		if (!get_check("Do you want to retire? ")) return;
	}

	/* Verify Suicide */
	else
	{
		char ch;

		/* Verify */
		if (!get_check("Do you really want to commit suicide? ")) return;

		/* Special Verification for suicide */
		prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
		flush();
		ch = inkey();
		prt("", 0, 0);
		if (ch != '@') return;
	}

	cmd_insert(CMD_SUICIDE);
}


void do_cmd_save_game(cmd_code code, cmd_arg args[])
{
	save_game();
}

