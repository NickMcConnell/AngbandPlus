
/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Check if action permissable here.
 */


static bool do_cmd_test(int y, int x, int action)
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
		msg_format("You see nothing %s.", here);

		/* Nope */
		return (FALSE);
	}

	/* Get memorised feature */
	f_ptr = &f_info[cave_feat[y][x]];

	switch (action)
	{
		case FS_SECRET:					break;
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
			msg_format("You see nothing %s%s.", here, act);
		 	return (FALSE);
		}
	}

	else if (action < FS_FLAGS3)
	{
		flag = bitzero << (action - FS_FLAGS2);
		if (!(f_ptr->f_flags2 & flag))
		{
		 	msg_format("You see nothing %s%s.", here, act);
		 	return (FALSE);
		}
	}

	else if (action < FS_FLAGS_END)
	{
		flag = bitzero << (action - FS_FLAGS3);
		if (!(f_ptr->f_flags2 & flag))
		{
			msg_format("You see nothing %s%s.", here, act);
		 	return (FALSE);
		}
	}

	return (TRUE);
}


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{

	char out_val[160];
	byte quest;

	feature_type *f_ptr= &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Verify stairs */
	if (!cave_up_stairs(p_ptr->py, p_ptr->px))
	{
		msg_print("I see no up staircase here.");

		return;
	}

	/*find out of leaving a level*/
	quest = quest_check(p_ptr->depth);

	/* Ironman */
	if (adult_ironman)
	{
		msg_print("There is no retreat!");
		return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) &&
		((quest == QUEST_MONSTER) || (quest == QUEST_UNIQUE)))
	{
		sprintf(out_val, "Really risk failing your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) &&
        (((quest == QUEST_VAULT) && (quest_item_slot() == -1)) ||
		 (quest == QUEST_PIT) || quest == QUEST_NEST || (quest == QUEST_THEMED_LEVEL)))
	{
		sprintf(out_val, "Really fail your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Hack -- take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Success */
	message(MSG_STAIRS_UP, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_stair = FEAT_MORE;

	/* New depth */
	p_ptr->depth--;

	/*find out of entering a quest level (should never happen going up)*/
	quest = quest_check(p_ptr->depth);

	/*go up another level if it is a shaft*/
	if ((f_ptr->f_flags2 & (FF2_SHAFT)) &&
	    (!quest) && (p_ptr->depth > 0))
  	{
		p_ptr->depth--;

		/* Create a way back (usually) */
		p_ptr->create_stair = FEAT_MORE_SHAFT;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}

static cptr desc_stat_pos[] =
{
	"strong",
	"smart",
	"wise",
	"dextrous",
	"healthy",
	"agile",
	"sneaky",
	"perceptive",
	"lucky"
};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"slow",
	"noisy",
	"short-sighted",
	"unlucky"
};


void potential_effect_on_stats(){
	int change1, change2, change3;

	if (p_ptr->max_depth < p_ptr->depth){
		if (p_ptr->depth == 2 || p_ptr->depth == 4 || p_ptr->depth == 6){
			if (p_ptr->depth == 2){
				change1 = p_ptr->statgain1;
				change2 = p_ptr->statgain2;
				change3 = p_ptr->statgain3;
			} else if (p_ptr->depth == 4){
				change1 = p_ptr->statgain4;
				change2 = p_ptr->statgain5;
				change3 = p_ptr->statgain6;
			} else {
				change1 = p_ptr->statgain7;
				change2 = p_ptr->statgain8;
				change3 = p_ptr->statgain9;
			}	 
			msg_print("You feel like you are discovering your true potential!");
			inc_stat(change1);
			inc_stat(change1);
			inc_stat(change1);
			inc_stat(change1);
			msg_format("You feel much more %s!", desc_stat_pos[change1]);
			inc_stat(change2);
			msg_format("You feel more %s!", desc_stat_pos[change2]);
			dec_stat(change3, 10, TRUE);
			dec_stat(change3, 10, TRUE);
			msg_format("You feel quite %s!", desc_stat_neg[change3]);
			if (!depth_in_feet){
				if (p_ptr->depth == 2){
					msg_print("Expect more of your abilities to be revealed at DL4 and DL6.");
				} else if (p_ptr->depth == 4){
					msg_print("Expect more of your abilities to be revealed at DL6.");
				} else {
					msg_print("From now on you will gain only one stat point every two DLs.");
				}
			} else {
				if (p_ptr->depth == 2){
					msg_print("Expect more of your abilities to be revealed at 200' and 300'.");
				} else if (p_ptr->depth == 4){
					msg_print("Expect more of your abilities to be revealed at 300'.");
				} else {
					msg_print("From now on you will gain only one stat point every 100'.");
				}
			}
		} else if (2*(p_ptr->depth/2) == p_ptr->depth){
			while (1){
				change1 = randint(A_MAX)-1;
				if (rp_ptr->r_adj[change1] >= 2){
					break;
				} else if (rp_ptr->r_adj[change1] >= 1 && !one_in_(3)) {
					break;
				} else if (rp_ptr->r_adj[change1] >= 0 && one_in_(2)) {
					break;
				} else if (rp_ptr->r_adj[change1] >= -1 && one_in_(3)) {
					break;
				} else if (rp_ptr->r_adj[change1] >= -2 && one_in_(5)) {
					break;
				} else if (one_in_(7)) {
					break;
				}
			}
			inc_stat(change1);
			msg_format("You feel more %s!", desc_stat_pos[change1]);
		}
	}
}

/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	byte quest;
	char out_val[160];

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
	if ((verify_leave_quest) &&
		((quest == QUEST_MONSTER) || (quest == QUEST_UNIQUE)))
	{
		sprintf(out_val, "Really risk failing your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Verify leaving normal quest level */
	if ((verify_leave_quest) &&
        (((quest == QUEST_VAULT) && (quest_item_slot() == -1)) ||
		 (quest == QUEST_PIT) || quest == QUEST_NEST || (quest == QUEST_THEMED_LEVEL)))
	{
		sprintf(out_val, "Really fail your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Hack -- take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Success */
	message(MSG_STAIRS_DOWN, 0, "You enter a maze of down staircases.");

	regenmana(100);

	/* Create a way back (usually) */
	p_ptr->create_stair = FEAT_LESS;

	/* New level */
	p_ptr->depth++;

	potential_effect_on_stats();

	/*find out if entering a quest level*/
	quest = quest_check(p_ptr->depth);

	/* Go down a shaft if allowed */
	if ((f_ptr->f_flags2 & (FF2_SHAFT)) &&
	    (!quest) && (p_ptr->depth < MAX_DEPTH - 1))
	{
		p_ptr->depth++;

		/* Create a way back (usually) */
		p_ptr->create_stair = FEAT_LESS_SHAFT;
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
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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

		/* Wipe the object */
		object_wipe(i_ptr);

		/*theme 1 is gold, themes 2-15 are objects*/

		if (chesttheme == 1) make_gold(i_ptr);

		else if (chesttheme >= 2)
		{
			bool good, great;

			/* Regular objects in chests will become quite
			 * rare as depth approaches 5000'.
			 * All items with i > 50 are guaranteed great,
			 * all items with i > 80  get 4 chances
			 * to become an artifact.
		 	 * Chests should be extremely lucritive
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
	object_level = danger(p_ptr->depth);

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
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois || p_ptr->immune_pois))
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
		sound(MSG_SUM_MONSTER);
		for (i = 0; i < num; i++)
		{
			(void)summon_specific(y, x, danger(p_ptr->depth), 0);
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

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (rand_int(100) < j)
		{
			message(MSG_LOCKPICK, 0, "You have picked the lock.");
			msg_print("You have picked the lock.");
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

		/*squelch chest if autosquelch calls for it*/
		if ((squelch_level[CHEST_INDEX]) == SQUELCH_OPENED_CHESTS)
		{

				delete_object_idx(o_idx);
				msg_print("Chest squelched after it was opened.");
		}
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
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 10))
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

		/* Remember the location of the last chest found */
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
		if ((o_idx = chest_check(yy, xx, trapped)) == 0) continue;

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
	if (!do_cmd_test(y, x, FS_OPEN)) return (FALSE);

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
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

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
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

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
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);

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
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x, FALSE);

	/* Verify legality */
	if (!o_idx && !do_cmd_test(y, x, FS_OPEN)) return;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Check for chest */
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
	if (!do_cmd_test(y, x, FS_CLOSE)) return (FALSE);

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
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);
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
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);

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

	/* Easy Close */
	if (easy_open)
	{
		/* Count open doors */
		if (count_feats(&y, &x, FS_CLOSE) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_CLOSE)) return;

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
	if (!do_cmd_test(y, x, FS_TUNNEL)) return (FALSE);

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
		if (p_ptr->skill_dig > rand_int(200 * j))
		{
			sound(MSG_DIG);

			/* Get the name */
			feature_desc(name, sizeof(name), feat, FALSE, TRUE);

			/* Give the message */
			msg_format("You have removed the %s.", name);

			cave_alter_feat(y, x, FS_TUNNEL);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

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

			/* We may continue tunelling */
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
void do_cmd_tunnel(void)
{
	int y, x, dir;

	bool more = FALSE;

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

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Oops */
	if (!do_cmd_test(y, x, FS_TUNNEL)) return;

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
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = 5 + danger(p_ptr->depth) / 2;

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


		/* Disarm */
		delete_effect_idx(cave_x_idx[y][x]);

		/* Forget the trap */

 		cave_info[y][x] &= ~(CAVE_MARK);

 		/* Check if the grid is still viewable */
 		note_spot(y, x);

 		lite_spot(y, x);

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
		hit_trap(feat, y, x, MODE_ACTION, 0);
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

	/* Easy Disarm */
	if (easy_open)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_traps(&y, &x, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if ((num_traps + num_chests) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x, TRUE);

	/* Verify legality */
	if (!o_idx && !cave_any_trap_bold(y, x)) return;

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Check for chests */
		o_idx = chest_check(y, x, TRUE);
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
		more = do_cmd_disarm_aux(y, x, TRUE);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
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
	if (!do_cmd_test(y, x, FS_BASH)) return (FALSE);

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
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3-20 with step=1 and 20-240 with step=10) */
	/* (A character with 18/00 STR gets 20)*/
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

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
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
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
	else if (rand_int(100) < adj_agi_safe[p_ptr->stat_ind[A_AGI]] +
	         p_ptr->lev)
	{
		/* Message */
		msg_format("The %s holds firm.", name);

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High agility yields coolness */
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
	int y, x, dir;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_BASH)) return;

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
 * Attempt to set or modify a trap
 */
void do_cmd_make_trap(void)
{
	int y, x, dir, max_traps;

	/* Oops */
	if (!(cp_ptr->flags & CF_SET_TRAPS))
	{
		msg_print("You don't have the ability to set traps!");
		return;
	}

	/* Hack XXX XXX XXX */
	if (p_ptr->confused || p_ptr->image)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

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

/*
 * Try to steal from a monster
 */
void do_cmd_steal(void)
{
	int y, x, dir;

	monster_type *m_ptr;

	/* Oops */
	if (!(cp_ptr->flags & CF_ROGUE_COMBAT))
	{
		msg_print("Theft isn't one of your talents!");
		return;
	}

	/* Hack XXX XXX XXX */
	if (p_ptr->confused || p_ptr->image)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

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
void do_cmd_spike(void)
{
	int y, x, dir, item = 0;

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
	if (!do_cmd_test(y, x, FS_SPIKE)) return;

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
		if (!do_cmd_test(y, x, FS_SPIKE)) return;

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
	if (!p_ptr->confused && !do_cmd_walk_test(y, x)) return;

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


	/* Start run */
	run_step(dir);
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
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
		cptr p = "Rest (0-9999, 'h' or '*' for HP, '&' as needed): ";

		char out_val[5];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, sizeof(out_val))) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*' || out_val[0] == 'h')
		{
			p_ptr->command_arg = (-1);

		}

		/* Rest spell points */
		else if (out_val[0] == 's')
		{
			return;
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
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

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
static int breakage_chance(const object_type *o_ptr)
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
		case TV_SKELETON:
		{
			return (50);
		}

		/* Sometimes break */
		case TV_ARROW:
		{
			return (35);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		{
			return (25);
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
 * Ranger  s (with Bows) and Anyone (with "Extra Shots") get extra shots.
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
	int dir, item;
	int i, j, y, x, ty, tx;
	int tdam, tdis, thits_times_ten, tmul;
	int bonus, chance;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

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
	if (!get_item(&item, q, s, (USE_QUIVER | USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];

		/* A cursed quiver disables the use of non-cursed ammo */
		if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver && !cursed_p(o_ptr))
		{
			msg_print("Your quiver is cursed!");
			return;
		}
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir, FALSE)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

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
	sound(MSG_SHOOT);

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Use the proper number of shots */
	thits_times_ten = p_ptr->num_fire_times_ten;

	/* Base damage from thrown object plus launcher bonus - now plus player bonus*/
	tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d + j_ptr->to_d + p_ptr->to_d_missile;

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h_missile + i_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range XXX XXX */
	tdis = 10 + 5 * tmul;

	/* Take a (partial) turn */
	p_ptr->p_energy_use = (BASE_ENERGY_MOVE * 10) / thits_times_ten;

	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	(void)project_path(tdis, p_ptr->py, p_ptr->px, &ty, &tx, 0);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_project_bold(ny, nx) && !cave_passable_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before)
			{
				Term_fresh();
				if (p_ptr->window) window_stuff();
			}
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before)
			{
				Term_fresh();
				if (p_ptr->window) window_stuff();
			}
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if ((cave_m_idx[y][x] > 0) && !(mon_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)))
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			monster_lore *l_ptr = &l_list[m_ptr->r_idx];

			int chance2;

			int visible = m_ptr->ml;


			/*Adjust for player terrain*/
			chance = feat_adjust_combat_for_player(chance, FALSE);

			/*Adjust for monster terrain*/
			chance = feat_adjust_combat_for_monster(m_ptr, chance, TRUE);

			chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			/* Note the collision */
			hit_body = TRUE;

			/* Some monsters are great at dodging  -EZ- */
			if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
				(!m_ptr->stunned) && (!m_ptr->confused) && (!m_ptr->monfear)
				&& (rand_int(5 + m_ptr->cdis) >= 3))
			{
				if (visible)
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					message_format(MSG_MISS, 0, "%^s dodges!", m_name);

					/* Learn that monster can dodge */
					l_ptr->r_l_flags2 |= (RF2_EVASIVE);
				}

				continue;
			}

			/* Did we hit it (penalize distance travelled) */
			else if (test_hit(chance2, r_ptr->ac, m_ptr->ml)>0)
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/*Mark the monster as attacked by the player*/
				m_ptr->mflag |= (MFLAG_HIT_BY_RANGED);

				if (monster_nonliving(r_ptr))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Make some noise */
				add_wakeup_chance += p_ptr->base_wakeup_chance / 2;

				/* Reveal fully visible mimics */
				if ((m_ptr->mimic_k_idx) && (m_ptr->ml))
				{
					/* Reveal it */
					reveal_mimic(m_ptr->fy, m_ptr->fx, TRUE);

					/*We can not see it*/
					visible = TRUE;

				}

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					message_format(MSG_SHOOT_HIT, 0, "The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Message */
					message_format(MSG_SHOOT_HIT, 0, "The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr, FALSE);
				tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam);

				/*rogues are deadly with slings*/
				if ((cp_ptr->flags & CF_ROGUE_COMBAT) && (p_ptr->ammo_tval == TV_SHOT))
				{

					tdam += p_ptr->lev * 2 / 3;
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies, SOURCE_PLAYER))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

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

/*handle special effects of throwing certain potions*/
static bool thrown_potion_effects(object_type *o_ptr, bool *is_dead, bool *fear, int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	/*Assume it hurts everything*/
	u32b flag = (PROJECT_KILL | PROJECT_PLAY);

	int y = m_ptr->fy;
	int x = m_ptr->fx;

	bool ident = FALSE;

	bool do_stun = FALSE;
	bool un_confuse = FALSE;
	bool un_stun = FALSE;
	bool un_fear = FALSE;

	bool used_potion = TRUE;

	/* Hold the monster name */
	char m_name[80];
	char m_poss[80];

	/* Get the monster name*/
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_name), m_ptr, 0x22);

	/* Analyze the potion */
	switch (o_ptr->sval)
	{

		case SV_POTION_SLOWNESS:
		{
			/*slowness explosion at the site, radius 1*/
			ident = explosion(SOURCE_PLAYER, 1, y, x, damroll (2, p_ptr->lev), GF_OLD_SLOW, flag);
			break;
		}

		case SV_POTION_CONFUSION:
		{
			/*confusion explosion at the site, radius 1*/
			ident = explosion(SOURCE_PLAYER, 1, y, x, damroll (2, p_ptr->lev), GF_OLD_CONF, flag);
			break;

		}

		case SV_POTION_SLEEP:
		{
			/*sleep explosion at the site, radius 1*/
			ident = explosion(SOURCE_PLAYER, 1, y, x, damroll (2, p_ptr->lev), GF_OLD_SLEEP, flag);
			break;
		}

		case SV_POTION_LOSE_MEMORIES:
		{

			if (m_ptr->smart)
			{

				/*erase monster memory of player*/
				m_ptr->smart = 0L;

				if (m_ptr->ml)
				{
					ident = TRUE;

					/*monster forgets player history*/
					msg_format("%^s forgets all %s knows about you!", m_name, m_poss);
				}
			}
			/*monster forgets player history*/
			else used_potion = FALSE;

			break;
		}

		case SV_POTION_DRAIN_MANA:
		{
			if (m_ptr->mana)
			{
				if (m_ptr->ml)
				{
					ident = TRUE;

					/*monster forgets player history*/
					msg_format("%^s loses some of %s mana!", m_name, m_poss);
				}

				/*reduce mana by about 11%*/
				m_ptr->mana = m_ptr->mana * 9 / 10;

			}

			/*monster forgets player history*/
			else used_potion = FALSE;

			break;
		}

		case SV_POTION_RUINATION:
		{
			ident = TRUE;

			/*slight damage to monster*/
			mon_take_hit(cave_m_idx[y][x], damroll(10, 10), fear, NULL, SOURCE_PLAYER);

			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if ((!m_ptr->ml)&& (r_ptr->flags2 & (RF2_INVISIBLE)))
			{
				/* Mark as visible */
				m_ptr->ml = TRUE;

				/*re-draw the spot*/
				lite_spot(y, x);

				/* Update the monster name*/
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				/*monster forgets player history*/
				msg_format("%^s appears for an instant!", m_name);

				/*update the lore*/
				l_ptr->r_l_flags2 |= (RF2_INVISIBLE);

				ident = TRUE;
			}

			/* Potion isn't idntified */
			else used_potion = FALSE;

			break;
		}

		case SV_POTION_BOLDNESS:
		{

			un_fear = TRUE;

			break;
		}

		case SV_POTION_SPEED:
		{

			/*speed explosion at the site, radius 1*/
			ident = explosion(SOURCE_PLAYER, 1, y, x, 20 + rand_int(20), GF_OLD_SPEED, flag);
			break;
		}

		case SV_POTION_HEROISM:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, 10, GF_OLD_HEAL, flag)) ident = TRUE;

			un_fear = TRUE;

			break;
		}

		case SV_POTION_BERSERK_STRENGTH:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, 10, GF_OLD_HEAL, flag)) ident = TRUE;

			un_fear = TRUE;

			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, damroll(3, 8), GF_OLD_HEAL, flag)) ident = TRUE;

			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, damroll(5, 10), GF_OLD_HEAL, flag)) ident = TRUE;

			un_confuse = TRUE;

			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, damroll(8, 10), GF_OLD_HEAL, flag)) ident = TRUE;

			un_confuse = TRUE;
			un_stun = TRUE;

			break;
		}

		case SV_POTION_HEALING:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, 325, GF_OLD_HEAL, flag)) ident = TRUE;

			un_confuse = TRUE;
			un_stun = TRUE;

			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, 1500, GF_OLD_HEAL, flag)) ident = TRUE;

			un_confuse = TRUE;
			un_stun = TRUE;

			break;
		}

		case SV_POTION_LIFE:
		{
			/*only for the living*/
			if monster_nonliving(r_ptr)
			{
				used_potion = FALSE;

				break;
			}

			/*healing explosion at the site, radius 1*/
			if (explosion(SOURCE_PLAYER, 1, y, x, 5000, GF_OLD_HEAL, flag)) ident = TRUE;

			un_confuse = TRUE;
			un_stun = TRUE;

			break;
		}

		case SV_POTION_RESTORE_MANA:
		{

			if (r_ptr->mana > m_ptr->mana)
			{
				if (m_ptr->ml)
				{
					ident = TRUE;

					/*monster forgets player history*/
					msg_format("%^s gains back all %s mana!", m_name, m_poss);
				}

				/*restore mana%*/
				m_ptr->mana = r_ptr->mana;
			}

			/*monster forgets player history*/
			else used_potion = FALSE;

			break;
		}

		/*potion just gets thrown as normal object*/
		default:
		{
			used_potion = FALSE;

			break;
		}
	}

	/*monster is now dead, skip messages below*/
	if (cave_m_idx[y][x] == 0)
	{
		do_stun = FALSE;
		un_confuse = FALSE;
		un_stun = FALSE;
		un_fear = FALSE;
		*is_dead = TRUE;
	}

	if (un_confuse)
	{
		if (m_ptr->confused)
		{
			/* No longer confused */
			m_ptr->confused = 0;

			/* Dump a message */
			if (m_ptr->ml)
			{
				msg_format("%^s is no longer confused.", m_name);

				ident = TRUE;
			}
		}
	}

	if (un_stun)
	{
		if (m_ptr->stunned)
		{
			/* No longer confused */
			m_ptr->stunned = 0;

			/* Dump a message */
			if (m_ptr->ml)
			{

				msg_format("%^s is no longer stunned.", m_name);

				ident = TRUE;
			}
		}
	}

	if (un_fear)
	{
		if (m_ptr->monfear)
		{
			/* Cancel fear */
			set_mon_fear(m_ptr, 0, FALSE);

			/* Message */
			if (m_ptr->ml)
				msg_format("%^s recovers %s courage.", m_name, m_poss);

			ident = TRUE;
		}
	}

	/* Sound and Impact breathers never stun */
	if (do_stun &&
	         !(r_ptr->flags4 & (RF4_BRTH_SOUND)) &&
	         !(r_ptr->flags4 & (RF4_BRTH_FORCE)))
	{
		int tmp = 0;

		/*some creatures are resistant to stunning*/
		if (r_ptr->flags3 & RF3_NO_STUN)
		{
			/*mark the lore*/
			if (m_ptr->ml) l_ptr->r_l_flags3 |= (RF3_NO_STUN);
		}

		/* Get confused */
		else if (m_ptr->stunned)
		{
			msg_format("%^s is more dazed.", m_name);
			tmp = m_ptr->stunned / 2;
		}
		else
		{
			msg_format("%^s is dazed.", m_name);
			tmp = 20;
		}

		/* Apply stun */
		m_ptr->stunned += tmp;

		ident = TRUE;

	}

	/*inform them of the potion, mark it as known*/
	if ((ident) && (!(k_info[o_ptr->k_idx].aware)))
	{

		char o_name[80];

		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Describe the potion */
		msg_format("You threw %s.", o_name);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	}

	/* Redraw if necessary*/
	if (used_potion) p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);

	/* Handle stuff */
	handle_stuff();

	return (used_potion);

}


/*
 * Flavor. Some objects cause random effects when thrown.
 * Returns TRUE if the object affected the dungeon in some way.
 */
static bool do_flavor_breakage(const object_type *o_ptr, int y, int x)
{
	/* Get the feature */
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/* Analyze object */
	switch (o_ptr->tval)
	{
		/* Flasks of oil can create oil patches */
		case TV_FLASK:
		{
			/* Hack -- Check if oil exists */
			if (!f_info[FEAT_OIL].name) break;

			/* Hack -- Graphics (don't hurt anything) */
			project(SOURCE_OTHER, 1, y, x, y, x, 0, GF_SMOKE, PROJECT_BOOM, 0, 0);

			/* Message. Flasks of oil are always broken now */
			msg_print("The flask is broken!");

			/* Check if the grid can be transformed to oil */
			if (!_feat_ff3_match(f_ptr, TERRAIN_MASK) && cave_passable_bold(y, x) &&
					!_feat_ff1_match(f_ptr, FF1_PERMANENT | FF1_STAIRS | FF1_DOOR) &&
					(_feat_ff1_match(f_ptr, FF1_LOS | FF1_PROJECT) ==
					 feat_ff1_match(FEAT_OIL, FF1_LOS | FF1_PROJECT)) &&
					one_in_(7))
			{
				/* Create oil */
				cave_set_feat(y, x, FEAT_OIL);

				/* Message */
				msg_print("There is oil on the floor.");
			}

			/* Success */
			return (TRUE);
		}
		/* Torches can ignite oil and forests */
		case TV_LITE:
		{
			/* Check if the lite is a torch and if the grid can be burned */
			if ((o_ptr->sval == SV_LITE_TORCH || o_ptr->sval == SV_LITE_MAGELIGHT) && _feat_ff3_match(f_ptr, FF3_OIL | FF3_FOREST) &&
				_feat_ff2_match(f_ptr, FF2_HURT_FIRE))
			{
				/* Create fire */
				cave_alter_feat(y, x, FS_HURT_FIRE);

				/* Message */
				msg_print("The torch sets the dungeon on fire!");

				/* Get the fire feature */
				f_ptr = &f_info[cave_feat[y][x]];

				/* Hurt objects sensitive to fire on the floor */
				if (_feat_ff3_match(f_ptr, FF3_FIRE) && (f_ptr->dam_non_native > 0))
				{
					/* Hurt only items */
					u32b flags = PROJECT_ITEM | PROJECT_HIDE;

					/* Burn objects */
					project(SOURCE_OTHER, 0, y, x, y, x, f_ptr->dam_non_native, GF_FIRE, flags, 0, 0);
				}

				/* Success */
				return (TRUE);
			}

			break;
		}
	}

	/* Failure */
	return (FALSE);
}



/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
void do_cmd_throw(void)
{
	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdam, tdis;
	int mul, divider;
	u32b f1, f2, f3, fn;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_QUIVER | USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];

		/* A cursed quiver disables the use of non-cursed ammo */
		if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver && !cursed_p(o_ptr))
		{
			msg_print("Your quiver is cursed!");
			return;
		}
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Examine the item */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir, FALSE)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

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
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Cursed throwing weapons can hurt the player sometimes */
	if (IS_QUIVER_SLOT(item) && cursed_p(i_ptr) && (rand_int(100) < 70))
	{
		/* Get amount of damage */
		int dam = damroll(i_ptr->dd, i_ptr->ds) + ABS(i_ptr->to_d);

		/* Message */
		msg_print("The %s releases its curse on you!");

		/* Hurt the player */
		project_p(SOURCE_OTHER, p_ptr->py, p_ptr->px, dam, GF_NETHER, "throwing a cursed weapon");
	}

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	divider = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / divider;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d;

	/* Chance of hitting */
	if (f3 & (TR3_THROWING))
	{
		chance = p_ptr->skill_tht + BTH_PLUS_ADJ * (p_ptr->to_h_missile + i_ptr->to_h);
	}
	else
	{
		chance = (3 * p_ptr->skill_tht / 2) + (BTH_PLUS_ADJ * i_ptr->to_h);
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;


	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	(void)project_path(tdis, p_ptr->py, p_ptr->px, &ty, &tx, 0);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_project_bold(ny, nx) && !cave_passable_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before)
			{
				Term_fresh();
				if (p_ptr->window) window_stuff();
			}
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before)
			{
				Term_fresh();
				if (p_ptr->window) window_stuff();
			}
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if ((cave_m_idx[y][x] > 0) && !(mon_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)))
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			monster_lore *l_ptr = &l_list[m_ptr->r_idx];

			int chance2;

			int visible = m_ptr->ml;

			int sleeping_bonus = 0;

			bool potion_effect = FALSE;
			int pdam = 0;
			bool is_dead = FALSE;


			/*Adjust for player terrain*/
			chance = feat_adjust_combat_for_player(chance, FALSE);

			/*Adjust for monster terrain*/
			chance = feat_adjust_combat_for_monster(m_ptr, chance, TRUE);

			chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			/* Note the collision */
			hit_body = TRUE;

			/* Rogues Get extra to-hit from throwing weapons*/

			if ((cp_ptr->flags & CF_ROGUE_COMBAT)
				&& (m_ptr->ml) && (f3 & (TR3_THROWING)))
			{
				sleeping_bonus = 30 + p_ptr->lev / 2;
			}

			/* Some monsters are great at dodging  -EZ- */
			if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
				(!m_ptr->stunned) && (!m_ptr->confused) && (!m_ptr->monfear)
				&& (rand_int(5 + m_ptr->cdis) >= 3))
			{
				if (visible)
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					message_format(MSG_MISS, 0, "%^s dodges!", m_name);

					/* Learn that monster can dodge */
					l_ptr->r_l_flags2 |= (RF2_EVASIVE);
				}

				continue;
			}

			/* Did we hit it (penalize range) */
			else if (test_hit((chance2 + sleeping_bonus), r_ptr->ac, m_ptr->ml)>0)
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/*Mark the monster as attacked by the player*/
				m_ptr->mflag |= (MFLAG_HIT_BY_RANGED);

				/* Some monsters get "destroyed" */
				if (monster_nonliving(r_ptr))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Make some noise */
				add_wakeup_chance += p_ptr->base_wakeup_chance / 2;

				/* Reveal fully visible mimics */
				if ((m_ptr->mimic_k_idx) && (m_ptr->ml))
				{
					/* Reveal it */
					reveal_mimic(m_ptr->fy, m_ptr->fx, TRUE);

					/*We can not see it*/
					visible = TRUE;

				}

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					if (f3 & (TR3_THROWING))
					{
						/* Message */
						msg_format("The %s hits %s with great accuracy!.", o_name, m_name);
					}
					else
					{
					/* Message */
						msg_format("The %s hits %s.", o_name, m_name);
					}
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);

				}

				/*special effects sometimes reveal the kind of potion*/
				if (i_ptr->tval == TV_POTION)
				{
					/*record monster hit points*/
					pdam = m_ptr->hp;

					/*returns true if the damage has already been handled*/
					potion_effect = (thrown_potion_effects(i_ptr, &is_dead, &fear, cave_m_idx[y][x]));

					/*check the change in monster hp*/
					pdam -= m_ptr->hp;

					/*monster could have been healed*/
					if (pdam < 0) pdam = 0;

				}

				/* Apply special damage XXX XXX XXX */
				if (!potion_effect) tdam = tot_dam_aux(i_ptr, tdam, m_ptr, FALSE);

				/* Object is a throwing weapon. */
				if (f3 & (TR3_THROWING))
				{
					/* Perfectly balanced weapons do even more damage. */
					if (i_ptr->ident & IDENT_PERFECT_BALANCE) tdam *= 2;

					/* Critical hits may add damage dice. */
					tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam);

					/*
					 * Double the damage for throwing weapons
					 */
					tdam *= 2;
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{

					msg_format("You do %d (out of %d) damage.",
					           (potion_effect ? pdam : tdam), m_ptr->hp);
				}

				/* Hit the monster, unless a potion effect has already been done */
				if (!potion_effect)
				{
					 is_dead = (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies, SOURCE_PLAYER));
				}

				/* Still alive */
				if (!is_dead)
				{
					/* Message if applicable*/
					if ((!potion_effect) || (pdam > 0))
						message_pain(cave_m_idx[y][x],  (pdam ? pdam : tdam));
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Check for special breakage */
	if (!hit_body && do_flavor_breakage(i_ptr, y, x)) return;

	/* Chance of breakage (during attacks) - potions always break*/
	if (i_ptr->ident & IDENT_PERFECT_BALANCE) j = 0;
	else j = (hit_body ? breakage_chance(i_ptr) : 0);

	/*hack - throwing weapons have a lesser chance*/
	if (f3 & (TR3_THROWING)) j /= 2;

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}

