/* File: cmd2.c */

/* Purpose: Movement commands (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	bool go_up = FALSE;
	cave_type *c_ptr;

	/* Player grid */
	c_ptr = &cave[py][px];

	/* Quest up stairs */
	if (c_ptr->feat == FEAT_QUEST_UP)
	{
		/* Success */
#ifdef JP
		msg_print("上の階に登った。");
#else
		msg_print("You enter the up staircase.");
#endif

		leave_quest_check();

		p_ptr->inside_quest = c_ptr->special;

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
		}

		/* Leaving a quest */
		if (!p_ptr->inside_quest)
		{
			dun_level = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;

		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		return;
	}

	/* Normal up stairs */
	if (c_ptr->feat != FEAT_LESS && c_ptr->feat != FEAT_LESS_LESS)
	{
#ifdef JP
		msg_print("ここには上り階段が見当たらない。");
#else
		msg_print("I see no up staircase here.");
#endif
		return;
	}

	if (!dun_level)
	{
		go_up = TRUE;
	}
	else
	{
		if (confirm_stairs)
		{
#ifdef JP
			if (get_check("本当にこの階を去りますか？"))
#else
			if (get_check("Really leave the level? "))
#endif
				go_up = TRUE;
		}
		else
		{
			go_up = TRUE;
		}
	}

	if (go_up)
	{
#if 0
	/*
	 * I'm experimenting without this... otherwise the monsters get to
	 * act first when we go up stairs, theoretically resulting in a possible
	 * insta-death.
	 */
		/* Hack -- take a turn */
		energy_use = 100;
#else
		energy_use = 0;
		not_gain_energy = TRUE;
#endif

		/* Success */
		if (p_ptr->inside_quest && (p_ptr->inside_quest < MIN_RANDOM_QUEST))
		{
			/* No Message */
		}
		else
		{
#ifdef JP
			msg_print("階段を上って新たなる迷宮へと足を踏み入れた。");
#else
			msg_print("You enter a maze of up staircases.");
#endif
	  		sound(SOUND_STAIRS_UP);
		}

		if (autosave_l) do_cmd_save_game(TRUE);

		if (p_ptr->inside_quest)
		{
			if(quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM) dun_level = 1;

			leave_quest_check();

			p_ptr->inside_quest = c_ptr->special;
		}

		/* New depth */
		if (c_ptr->feat == FEAT_LESS_LESS)
		{
			/* Create a way back */
			create_down_stair = 2;

			dun_level -= 2;
		}
		else
		{
			/* Create a way back */
			create_down_stair = 1;

			dun_level -= 1;
		}

		/* Leaving the dungeon to town */
		if (!dun_level && p_ptr->town_num && !leaving_quest)
			p_ptr->leaving_dungeon = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	cave_type *c_ptr;
	bool go_down = FALSE;
	bool fall_trap = FALSE;

	/* Player grid */
	c_ptr = &cave[py][px];

	if (c_ptr->feat == (FEAT_TRAP_TRAPDOOR)) fall_trap = TRUE;

	/* Quest down stairs */
	if (c_ptr->feat == FEAT_QUEST_DOWN)
	{
#ifdef JP
		msg_print("下の階に降りた。");
#else
		msg_print("You enter the down staircase.");
#endif

		leave_quest_check();

		p_ptr->inside_quest = c_ptr->special;

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
		}

		/* Leaving a quest */
		if (!p_ptr->inside_quest)
		{
			dun_level = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;

		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		return;
	}

	/* Verify stairs */
	if ((c_ptr->feat != FEAT_MORE) && (c_ptr->feat != FEAT_MORE_MORE) && !fall_trap)
	{
#ifdef JP
		msg_print("ここには下り階段が見当たらない。");
#else
		msg_print("I see no down staircase here.");
#endif
		return;
	}

	if (!dun_level)
	{
		go_down = TRUE;

		/* Save old player position */
		p_ptr->oldpx = px;
		p_ptr->oldpy = py;
	}
	else
	{
		if (confirm_stairs)
		{
#ifdef JP
			if (get_check("本当にこの階を去りますか？"))
#else
			if (get_check("Really leave the level? "))
#endif
				go_down = TRUE;
		}
		else
		{
			go_down = TRUE;
		}
	}

	if (go_down)
	{
#if 0
		/* Hack -- take a turn */
		energy_use = 100;
#else
		energy_use = 0;
		not_gain_energy = TRUE;
#endif

		if (fall_trap)
#ifdef JP
			msg_print("わざと落し戸に落ちた。");
#else
			msg_print("You deliberately jump through the trap door.");
#endif
		else /* Success */
		{
#ifdef JP
			msg_print("階段を下りて新たなる迷宮へと足を踏み入れた。");
#else
			msg_print("You enter a maze of down staircases.");
#endif
	  		sound(SOUND_STAIRS_DOWN);
	  	}
		if (autosave_l) do_cmd_save_game(TRUE);

		/* Go down */
		if (fall_trap)
		{
			/* Create a way back */
			create_up_stair = 0;

			dun_level += 1;
		}
		else if (c_ptr->feat == FEAT_MORE_MORE)
		{
			/* Create a way back */
			create_up_stair = 2;

			dun_level += 2;
		}
		else
		{
			/* Create a way back */
			create_up_stair = 1;

			dun_level += 1;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Take a turn */
	energy_use = 100;

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
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx;

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		/* if (!(o_ptr->marked & OM_FOUND)) continue; */

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/*
 * Allocates objects upon opening a chest    -BEN-
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Small chests often contain "gold", while Large chests always contain
 * items.  Wooden chests contain 2 items, Iron chests contain 4 items,
 * and Steel chests contain 6 items.  The "value" of the items in a
 * chest is based on the "power" of the chest, which is in turn based
 * on the level on which the chest is generated.
 */
static void chest_death(int y, int x, s16b o_idx)
{
	int number;

	bool small;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr = &o_list[o_idx];


	/* Small chests often hold "gold" */
	small = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Small chests often drop gold */
		if (small && (randint0(100) < 25))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make a good object */
			if (!make_object(q_ptr, TRUE, FALSE)) continue;
		}

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = base_level;

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
	int  i, trap;

	object_type *o_ptr = &o_list[o_idx];


	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
#ifdef JP
		msg_print("仕掛けられていた小さな針に刺されてしまった！");
		take_hit(damroll(1, 4), "毒針");
#else
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
#endif

		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
#ifdef JP
		msg_print("仕掛けられていた小さな針に刺されてしまった！");
		take_hit(damroll(1, 4), "毒針");
#else
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
#endif

		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
#ifdef JP
		msg_print("突如吹き出した緑色のガスに包み込まれた！");
#else
		msg_print("A puff of green gas surrounds you!");
#endif

		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)set_poisoned(p_ptr->poisoned + 10 + randint1(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
#ifdef JP
		msg_print("突如吹き出した黄色いガスに包み込まれた！");
#else
		msg_print("A puff of yellow gas surrounds you!");
#endif


		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + 10 + randint1(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint1(3);
#ifdef JP
		msg_print("突如吹き出した煙に包み込まれた！");
#else
		msg_print("You are enveloped in a cloud of smoke!");
#endif


		for (i = 0; i < num; i++)
		{
			if (randint1(100) < dun_level)
				(void)activate_hi_summon();
			else
				(void)summon_specific(0, y, x, dun_level, 0, TRUE, FALSE, FALSE);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
#ifdef JP
		msg_print("突然、箱が爆発した！");
		msg_print("箱の中の物はすべて粉々に砕け散った！");
#else
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
#endif

		o_ptr->pval = 0;
		sound(SOUND_BR_FIRE); /* No explode sound - use breath fire instead */
#ifdef JP
		take_hit(damroll(5, 8), "爆発する箱");
#else
		take_hit(damroll(5, 8), "an exploding chest");
#endif

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


	/* Take a turn */
	energy_use = 100;

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
		if (randint0(100) < j)
		{
#ifdef JP
			msg_print("鍵をはずした。");
#else
			msg_print("You have picked the lock.");
#endif
	  		sound(SOUND_LOCKPICK);
			gain_exp(1);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
#ifdef JP
			msg_print("鍵をはずせなかった。");
#else
			msg_print("You failed to pick the lock.");
#endif

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
 * Return TRUE if the given feature is an open door
 */
static bool is_open(int feat)
{
	return (feat == FEAT_OPEN);
}

/*
 * Return TRUE if the given feature is a closed door
 */
static bool is_closed(int feat)
{
	return ((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL));
}

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_dt(int *y, int *x, bool (*test)(int feat), bool under)
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
		yy = py + ddy_ddd[d];
		xx = px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave[yy][xx].info & (CAVE_MARK))) continue;

		/* Not looking for this feature */
		if (!((*test)(cave[yy][xx].feat))) continue;

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
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = py + ddy_ddd[d];
		int xx = px + ddx_ddd[d];

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
	int d[3][3] = { {7, 4, 1}, {8, 5, 2}, {9, 6, 3} };
	int dy, dx;

	dy = y - py;
	dx = x - px;

	/* Paranoia */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return d[dx + 1][dy + 1];
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
	int i, j;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get requested grid */
	c_ptr = &cave[y][x];

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
#ifdef JP
		msg_print("ドアはがっちりと閉じられているようだ。");
#else
		msg_print("The door appears to be stuck.");
#endif

	}

	/* Locked door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (randint0(100) < j)
		{
			/* Message */
#ifdef JP
			msg_print("鍵をはずした。");
#else
			msg_print("You have picked the lock.");
#endif
	  		sound(SOUND_LOCKPICK);

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

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
#ifdef JP
			msg_print("鍵をはずせなかった。");
#else
			msg_print("You failed to pick the lock.");
#endif


			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

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
	int y, x, dir;

	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN /* TNB */

	/* Option: Pick a direction */
	if (easy_open)
	{
		int num_doors, num_chests;

		/* Count closed doors*/
		num_doors = count_dt(&y, &x, is_closed, FALSE);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN -- TNB */

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir_aux(&dir, TRUE))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get requested grid */
		c_ptr = &cave[y][x];

		/* Check for chest */
		o_idx = chest_check(y, x);

		/* Nothing useful */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_DOOR_TAIL)) &&
		    !o_idx)
		{
			/* Message */
#ifdef JP
		msg_print("そこには開けるものが見当たらない。");
#else
			msg_print("You see nothing there to open.");
#endif
	  		sound(SOUND_NOTHING_TO_OPEN);

		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x);
		}

		/* Handle chests */
		else if (o_idx)
		{
			/* Open the chest */
			more = do_cmd_open_chest(y, x, o_idx);
		}

		/* Handle doors */
		else
		{
			/* Open the door */
			more = do_cmd_open_aux(y, x);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Perform the basic "close" command
 *
 * Assume destination is an open/broken door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Broken door */
	if (c_ptr->feat == FEAT_BROKEN)
	{
		/* Message */
#ifdef JP
		msg_print("ドアは壊れてしまっている。");
#else
		msg_print("The door appears to be broken.");
#endif

	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

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
	int y, x, dir;

	cave_type *c_ptr;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN /* TNB */

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count open doors */
		if (count_dt(&y, &x, is_open, FALSE) == 1)
		{
			command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN -- TNB */

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Require open/broken door */
		if ((c_ptr->feat != FEAT_OPEN) && (c_ptr->feat != FEAT_BROKEN))
		{
			/* Message */
#ifdef JP
		msg_print("そこには閉じるものが見当たらない。");
#else
			msg_print("You see nothing there to close.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x);
		}

		/* Close the door */
		else
		{
			/* Close the door */
			more = do_cmd_close_aux(y, x);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave[y][x].info & (CAVE_MARK)))
	{
		/* Message */
#ifdef JP
		msg_print("そこには何も見当たらない。");
#else
		msg_print("You see nothing there.");
#endif


		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
	{
		/* Message */
#ifdef JP
		msg_print("そこには掘るものが見当たらない。");
#else
		msg_print("You see nothing there to tunnel.");
#endif


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
 * This will, however, produce grids which are NOT illuminated
 * (or darkened) along with the rest of the room.
 */
static bool twall(int y, int x, byte feat)
{
	cave_type	*c_ptr = &cave[y][x];

	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, feat);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

	/* Result */
	return (TRUE);
}



/*
 * Perform the basic "tunnel" command
 *
 * Assumes that the destination is a wall, a vein, a secret
 * door, or rubble.
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	cave_type *c_ptr;

	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	/* Titanium */
	if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
	    (c_ptr->feat <= FEAT_PERM_SOLID))
	{
#ifdef JP
		msg_print("この岩は硬すぎて掘れないようだ。");
#else
		msg_print("This seems to be permanent rock.");
#endif

	}

	/* No tunnelling through mountains */
	else if (c_ptr->feat == FEAT_MOUNTAIN)
	{
#ifdef JP
		msg_print("そこは掘れない!");
#else
		msg_print("You can't tunnel through that!");
#endif

	}

	else if (c_ptr->feat == FEAT_TREES) /* -KMW- */
	{
		/* Chop Down */
		if ((p_ptr->skill_dig > 10 + randint0(400)) && twall(y, x, FEAT_GRASS))
		{
#ifdef JP
			msg_print("木を切り払った。");
#else
			msg_print("You have cleared away the trees.");
#endif
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
#ifdef JP
			msg_print("木を切っている。");
#else
			msg_print("You chop away at the tree.");
#endif

			more = TRUE;

			/* Occasional Search XXX XXX */
			if (randint0(100) < 25) search();
		}
	}


	/* Granite */
	else if ((c_ptr->feat >= FEAT_WALL_EXTRA) &&
		 (c_ptr->feat <= FEAT_WALL_SOLID))
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + randint0(1600)) && twall(y, x, FEAT_FLOOR))
		{
#ifdef JP
			msg_print("穴を掘り終えた。");
#else
			msg_print("You have finished the tunnel.");
#endif
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
#ifdef JP
			msg_print("花崗岩の壁に穴を掘っている。");
#else
			msg_print("You tunnel into the granite wall.");
#endif

			more = TRUE;
		}
	}


	/* Quartz / Magma */
	else if ((c_ptr->feat >= FEAT_MAGMA) &&
	    (c_ptr->feat <= FEAT_QUARTZ_K))
	{
		bool okay;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (c_ptr->feat >= FEAT_MAGMA_H) gold = TRUE;

		/* Extract "quartz" flag XXX XXX XXX */
		if ((c_ptr->feat - FEAT_MAGMA) & 0x01) hard = TRUE;

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->skill_dig > 20 + randint0(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->skill_dig > 10 + randint0(400));
		}

		/* Success */
		if (okay && twall(y, x, FEAT_FLOOR))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(y, x);

				/* Message */
#ifdef JP
				msg_print("何かを発見した！");
#else
				msg_print("You have found something!");
#endif

			}

			/* Found nothing */
			else
			{
				/* Message */
#ifdef JP
				msg_print("穴を掘り終えた。");
#else
				msg_print("You have finished the tunnel.");
#endif
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
#ifdef JP
			msg_print("石英の鉱脈に穴を掘っている。");
#else
			msg_print("You tunnel into the quartz vein.");
#endif

			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
#ifdef JP
			msg_print("溶岩の鉱脈に穴を掘っている。");
#else
			msg_print("You tunnel into the magma vein.");
#endif

			more = TRUE;
		}
	}

	/* Rubble */
	else if (c_ptr->feat == FEAT_RUBBLE)
	{
		int prob = 8;

		/* Remove the rubble */
		if ((p_ptr->skill_dig > randint0(200)) && twall(y, x, FEAT_FLOOR))
		{
			/* Message */
#ifdef JP
			msg_print("岩石をくずした。");
#else
			msg_print("You have removed the rubble.");
#endif

			/* Hack -- place an object */
			if (one_in_(prob))
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(y, x))
				{
#ifdef JP
					msg_print("何かを発見した！");
#else
					msg_print("You have found something!");
#endif
				}
			}
		}
		else
		{
			/* Message, keep digging */
#ifdef JP
			msg_print("岩石をくずしている。");
#else
			msg_print("You dig in the rubble.");
#endif
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (c_ptr->feat >= FEAT_SECRET)
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 30 + randint0(1200)) && twall(y, x, FEAT_FLOOR))
		{
#ifdef JP
			msg_print("穴を掘り終えた。");
#else
			msg_print("You have finished the tunnel.");
#endif
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
#ifdef JP
			msg_print("花崗岩の壁に穴を掘っている。");
#else
			msg_print("You tunnel into the granite wall.");
#endif
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (randint0(100) < 25) search();
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 30 + randint0(1200)) && twall(y, x, FEAT_FLOOR))
		{
#ifdef JP
			msg_print("穴を掘り終えた。");
#else
			msg_print("You have finished the tunnel.");
#endif
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
#ifdef JP
			msg_print("ドアに穴を開けている。");
#else
			msg_print("You tunnel into the door.");
#endif
			more = TRUE;
		}
	}

	/* Notice new floor grids */
	if (!cave_floor_bold(y, x))
	{
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
	}

	/* Result */
	return (more);
}


/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Note that you must tunnel in order to hit invisible monsters
 * in walls, though moving into walls still takes a turn anyway.
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int			y, x, dir;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction to tunnel, or Abort */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* No tunnelling through doors */
		if (((c_ptr->feat >= FEAT_DOOR_HEAD) && (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
		    ((c_ptr->feat >= FEAT_BLDG_HEAD) && (c_ptr->feat <= FEAT_BLDG_TAIL)) ||
		    ((c_ptr->feat >= FEAT_SHOP_HEAD) && (c_ptr->feat <= FEAT_SHOP_TAIL)))
		{
			/* Message */
#ifdef JP
			msg_print("ドアは掘れない。");
#else
			msg_print("You cannot tunnel through doors.");
#endif

		}

		/* No tunnelling through air */
		else if (cave_floor_grid(c_ptr) || ((c_ptr->feat >= FEAT_MINOR_GLYPH) &&
		    (c_ptr->feat <= FEAT_PATTERN_XTRA2)))
		{
			/* Message */
#ifdef JP
			msg_print("空気は掘れない。");
#else
			msg_print("You cannot tunnel through air.");
#endif

		}

		/* No tunnelling through mountains */
		else if (c_ptr->feat == FEAT_MOUNTAIN)
		{
#ifdef JP
			msg_print("そこは掘れない。");
#else
			msg_print("You can't tunnel through that!");
#endif

		}

		/* A monster is in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x);
		}

		/* Try digging */
		else
		{
			/* Tunnel through walls */
			more = do_cmd_tunnel_aux(y, x);
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


#ifdef ALLOW_EASY_OPEN /* TNB */

/*
 * easy_open_door --
 *
 *	If there is a jammed/closed/locked door at the given location,
 *	then attempt to unlock/open it. Return TRUE if an attempt was
 *	made (successful or not), otherwise return FALSE.
 *
 *	The code here should be nearly identical to that in
 *	do_cmd_open_test() and do_cmd_open_aux().
 */
bool easy_open_door(int y, int x)
{
	int i, j;

	cave_type *c_ptr = &cave[y][x];

	/* Must be a closed door */
	if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
	      (c_ptr->feat <= FEAT_DOOR_TAIL)))
	{
		/* Nope */
		return (FALSE);
	}

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
#ifdef JP
		msg_print("ドアはがっちりと閉じられているようだ。");
#else
		msg_print("The door appears to be stuck.");
#endif

	}

	/* Locked door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (randint0(100) < j)
		{
			/* Message */
#ifdef JP
			msg_print("鍵をはずした。");
#else
			msg_print("You have picked the lock.");
#endif
	  		sound(SOUND_LOCKPICK);

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

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
#ifdef JP
			msg_print("鍵をはずせなかった。");
#else
			msg_print("You failed to pick the lock.");
#endif
	  		sound(SOUND_LOCKPICK_FAIL);

		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Result */
	return (TRUE);
}

#endif /* ALLOW_EASY_OPEN -- TNB */


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
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


	/* Take a turn */
	energy_use = 100;

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
#ifdef JP
		msg_print("トラップが見あたらない。");
#else
		msg_print("I don't see any traps.");
#endif

	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
#ifdef JP
		msg_print("箱にはトラップが仕掛けられていない。");
#else
		msg_print("The chest is not trapped.");
#endif

	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
#ifdef JP
		msg_print("箱にはトラップが仕掛けられていない。");
#else
		msg_print("The chest is not trapped.");
#endif

	}

	/* Success (get a lot of experience) */
	else if (randint0(100) < j)
	{
#ifdef JP
		msg_print("箱に仕掛けられていたトラップを解除した。");
#else
		msg_print("You have disarmed the chest.");
#endif
	  	sound(SOUND_DISARM);
		gain_exp(o_ptr->pval);
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint1(i) > 5))
	{
		/* We may keep trying */
		more = TRUE;
		if (flush_failure) flush();
#ifdef JP
		msg_print("箱のトラップ解除に失敗した。");
#else
		msg_print("You failed to disarm the chest.");
#endif

	}

	/* Failure -- Set off the trap */
	else
	{
#ifdef JP
		msg_print("トラップを作動させてしまった！");
#else
		msg_print("You set off a trap!");
#endif

 		sound(SOUND_STORE2);  /* (Sound substitute) HACK! No fail sound, use strore 2*/
		chest_trap(y, x, o_idx);
	}

	/* Result */
	return (more);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
bool do_cmd_disarm_aux(int y, int x, int dir)
{
	int i, j, power;

	cave_type *c_ptr;

	cptr name;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Access trap name */
	name = (f_name + f_info[c_ptr->feat].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = 5;

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if (randint0(100) < j)
	{
		/* Message */
#ifdef JP
		msg_format("%sを解除した。", name);
#else
		msg_format("You have disarmed the %s.", name);
#endif
	  	sound(SOUND_DISARM);

		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);

		/* Move the player onto the trap */
		move_player(dir, FALSE);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint1(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
#ifdef JP
		msg_format("%sの解除に失敗した。", name);
#else
		msg_format("You failed to disarm the %s.", name);
#endif


		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
#ifdef JP
		msg_format("%sを作動させてしまった！", name);
#else
		msg_format("You set off the %s!", name);
#endif

		/* Move the player onto the trap */
		move_player(dir, FALSE);
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
	int y, x, dir;

	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

	int num_traps, num_chests;

	/* Count visible traps */
	num_traps = count_dt(&y, &x, is_trap, TRUE);

	/* Count chests (trapped) */
	num_chests = count_chests(&y, &x, TRUE);

	/* See if only one target */
	if (num_traps || num_chests)
	{
		bool too_many = (num_traps && num_chests) || (num_traps > 1) ||
			(num_chests > 1);
		if (!too_many) command_dir = coords_to_dir(y, x);
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction (or abort) */
	if (get_rep_dir_aux(&dir, TRUE))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Check for chests */
		o_idx = chest_check(y, x);

		/* Disarm a trap */
		if (!is_trap(c_ptr->feat) && !o_idx)
		{
			/* Message */
#ifdef JP
		msg_print("そこには解除するものが見当たらない。");
#else
			msg_print("You see nothing there to disarm.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x);
		}

		/* Disarm chest */
		else if (o_idx)
		{
			/* Disarm the chest */
			more = do_cmd_disarm_chest(y, x, o_idx);
		}

		/* Disarm trap */
		else
		{
			/* Disarm the trap */
			more = do_cmd_disarm_aux(y, x, dir);
		}
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Perform the basic "bash" command
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x, int dir)
{
	int			bash, temp;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Message */
#ifdef JP
	msg_print("ドアに体当たりをした！");
#else
	msg_print("You smash into the door!");
#endif


	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (randint0(100) < temp)
	{
		/* Message */
#ifdef JP
		msg_print("ドアを壊した！");
#else
		msg_print("The door crashes open!");
#endif


		/* Break down the door */
		if (randint0(100) < 50)
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

		/* Hack -- Fall through the door */
		move_player(dir, FALSE);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (randint0(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
		 p_ptr->lev)
	{
		/* Message */
#ifdef JP
		msg_print("このドアは頑丈だ。");
#else
		msg_print("The door holds firm.");
#endif


		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
#ifdef JP
		msg_print("体のバランスをくずしてしまった。");
#else
		msg_print("You are off-balance.");
#endif


		/* Hack -- Lose balance ala paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + 2 + randint0(2));
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
	int			y, x, dir;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Bash location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Nothing useful */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
#ifdef JP
		msg_print("そこには体当たりするものが見当たらない。");
#else
			msg_print("You see nothing there to bash.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x);
		}

		/* Bash a closed door */
		else
		{
			/* Bash the door */
			more = do_cmd_bash_aux(y, x, dir);
		}
	}

	/* Unless valid action taken, cancel bash */
	if (!more) disturb(0, 0);
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn, to prevent free detection
 * of invisible monsters.
 */
void do_cmd_alter(void)
{
	int			y, x, dir;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Take a turn */
		energy_use = 100;

		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			py_attack(y, x);
		}

		/* Tunnel through walls */
		else if (((c_ptr->feat >= FEAT_SECRET) &&
			  (c_ptr->feat < FEAT_MINOR_GLYPH)) ||
			 ((c_ptr->feat == FEAT_TREES) ||
			  (c_ptr->feat == FEAT_MOUNTAIN)))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x);
		}

		/* Bash jammed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD + 0x08) &&
			 (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x, dir);
		}

		/* Open closed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
			 (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_open_aux(y, x);
		}

		/* Close open doors */
		else if ((c_ptr->feat == FEAT_OPEN) ||
			 (c_ptr->feat == FEAT_BROKEN))
		{
			/* Tunnel */
			more = do_cmd_close_aux(y, x);
		}

		/* Disarm traps */
		else if (is_trap(c_ptr->feat))
		{
			/* Tunnel */
			more = do_cmd_disarm_aux(y, x, dir);
		}

		/* Oops */
		else
		{
			/* Oops */
#ifdef JP
			msg_print("何もない空中を攻撃した。");
#else
			msg_print("You attack the empty air.");
#endif

		}
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
	int y, x, dir, item;

	cave_type *c_ptr;


	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Require closed door */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
#ifdef JP
		msg_print("そこにはくさびを打てるものが見当たらない。");
#else
			msg_print("You see nothing there to spike.");
#endif

		}

		/* Get a spike */
		else if (!get_spike(&item))
		{
			/* Message */
#ifdef JP
		msg_print("くさびを持っていない！");
#else
			msg_print("You have no spikes!");
#endif

		}

		/* Is a monster in the way? */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x);
		}

		/* Go for it */
		else
		{
			/* Take a turn */
			energy_use = 100;

			/* Successful jamming */
#ifdef JP
		msg_print("ドアにくさびを打ち込んだ。");
#else
			msg_print("You jam the door with a spike.");
#endif


			/* Convert "locked" to "stuck" XXX XXX XXX */
			if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

			/* Add one spike to the door */
			if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

			/* Use up, and describe, a single spike, from the bottom */
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
	}
}



/*
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(int pickup)
{
	int dir;

	bool more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Take a turn */
		energy_use = 100;

		/* Actually move the character */
		move_player(dir, pickup);

		/* Allow more walking */
		more = TRUE;
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Start running.
 */
void do_cmd_run(void)
{
	int dir;

	/* Hack -- no running when confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて走れない！");
#else
		msg_print("You are too confused!");
#endif

		return;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Hack -- Set the run counter */
		running = (command_arg ? command_arg : 1000);

		/* First step */
		run_step(dir);
	}
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay(int pickup)
{
	cave_type *c_ptr = &cave[py][px];


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}


	/* Take a turn */
	energy_use = 100;


	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == randint0(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}


	/* Handle "objects" */
	carry(pickup);


	/* Hack -- enter a store if we are on one */
	if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
	    (c_ptr->feat <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		command_new = SPECIAL_KEY_STORE;
	}

	/* Hack -- enter a building if we are on one -KMW- */
	else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
	    (c_ptr->feat <= FEAT_BLDG_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter building */
		command_new = SPECIAL_KEY_BUILDING;
	}

	/* Exit a quest if reach the quest exit */
	else if (c_ptr->feat == FEAT_QUEST_EXIT)
	{
		int q_index = p_ptr->inside_quest;

		/* Was quest completed? */
		if (quest[q_index].type == QUEST_TYPE_FIND_EXIT)
		{
			quest[q_index].status = QUEST_STATUS_COMPLETED;
			quest[q_index].complev = (byte)p_ptr->lev;
#ifdef JP
			msg_print("クエストを達成した！");
#else
			msg_print("You accomplished your quest!");
#endif
	  		sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
			msg_print(NULL);
		}

		leave_quest_check();

		p_ptr->inside_quest = cave[py][px].special;
		dun_level = 0;
		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;
		p_ptr->leaving = TRUE;
	}
}



/*
 * Resting allows a player to safely restore his hp	-RAK-
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (command_arg <= 0)
	{
#ifdef JP
		cptr p = "休憩 (0-9999, '*' で HP/MP, '&' で必要なだけ): ";
#else
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";
#endif


		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 4)) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			command_arg = (-1);
		}

		/* Rest some */
		else
		{
			command_arg = atoi(out_val);
			if (command_arg <= 0) return;
		}
	}

	/* Paranoia */
	if (command_arg > 9999) command_arg = 9999;

	/* Take a turn XXX XXX XXX (?) */
	energy_use = 100;

	/* Save the rest code */
	resting = command_arg;

	/* Cancel searching */
	p_ptr->searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	Term_fresh();
}


/* At the moment this function is exactly the same as the melee function */
static bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- Instant hit.  Chance to miss removed in Oangband because
	 * of the way monster ACs now work (fewer truly easy targets).
	 */
	if (k < 5) return (TRUE);

	if (chance <= 0) return (FALSE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor. */
	if (randint0(chance) >= (ac * 3 / 4)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(object_type *o_ptr)
{
	int shoot_bonus = 0;

	/* Culcurate class bonus */
	switch (p_ptr->pclass) {
		case CLASS_ARCHER:
			shoot_bonus = (p_ptr->lev - 1) / 7 + 4;
			break;
		default:
			shoot_bonus = 0;
	}

	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
			return (100);

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_SKELETON:
			return (50);

		/* Sometimes break */
		case TV_WAND:
		case TV_SPIKE:
			return (25);

		case TV_ARROW:
			return (20 - shoot_bonus * 2);

		/* Rarely break */
		case TV_SHOT:
		case TV_BOLT:
			return (10 - shoot_bonus);
		default:
			return (10);
	}
}


static bool is_stick(object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
		case TV_ARROW:
		case TV_BOLT:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_SPIKE:
			return (TRUE);
	}
	return (FALSE);
}


/*
 * Calculation of critical hits for objects fired or thrown by the player. -LM-
 */
static s16b critical_shot(object_type *o_ptr, int chance,
	int sleeping_bonus, int dam, monster_type *m_ptr, int visible)
{
	int i, k, chance2;
	bool stick;
	char o_name[MAX_NLEN];
	char m_name[MAX_NLEN];

	chance2 = chance + sleeping_bonus;
	stick = is_stick(o_ptr);
	object_desc(o_name, o_ptr, OD_OMIT_PREFIX);
	monster_desc(m_name, m_ptr, FALSE);

	if (!visible)
	{
#ifdef JP
		msg_format("%sが敵を捕捉した。", o_name);
#else
		msg_format("The %s finds a mark.", o_name);
#endif
	}

	/* Extract "shot" power */
	i = (o_ptr->weight + (chance2 * 2) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint1(5000) <= i)
	{
		/* Encourage the player to throw weapons at sleeping
		 * monsters. -LM-
		 */
		if (sleeping_bonus && visible)
		{
#ifdef JP
			msg_print("モンスターを乱暴に目覚めさせた！");
#else
			msg_print("You rudely awaken the monster!");
#endif
		}

		/* Determine level of critical hit */
		k = o_ptr->weight+ randint1(500) + randint1(chance2);

		/* This portion of the function determines the level of critical hit,
		 * then adjusts the damage dice multiplier and displays an appropriate
		 * combat message.
		 * A distinction is made between visible and invisible monsters.
		 */
		if (k < 500)
		{
			if (visible)
			{
#ifdef JP
					msg_format("%sが%sを直撃した。", o_name, m_name);
#else
					msg_format("The %s strikes %s.", o_name, m_name);
#endif
			}

			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			if (visible)
			{
#ifdef JP
				if (stick) msg_format("%sが%sに突き刺さった。", o_name, m_name);
				else msg_format("%sが%sに食い込んだ。", o_name, m_name);
#else
				msg_format("The %s penetrates %s.", o_name, m_name);
#endif
			}

			dam = 2 * dam + 10;
		}
		else
		{
			if (visible)
			{
#ifdef JP
				msg_format("%sが%sを貫通した！", o_name, m_name);
#else
				msg_format("The %s transpierces %s!", o_name, m_name);
#endif
			}

			dam = 3 * dam + 15;
		}
	}
	/* If the shot is not a critical hit, then the default message is shown. */
	else
	{
		if (visible)
		{
#ifdef JP
			msg_format("%sが%sに命中した。", o_name, m_name);
#else
			msg_format("The %s hits %s.", o_name, m_name);
#endif
		}
	}
	sound(SOUND_SHOOT_HIT);
	return (dam);
}


/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x2, except Slay Animal (x2.5),
 * Slay Evil (x2), and Kill dragon (x3). -SF-
 */
static s16b tot_dam_aux_shot(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
	int mult = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	object_type *i_ptr;

	u32b f1, f2, f3;
	u32b g1, g2, g3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Slay Human */
			if ((f1 & TR1_SLAY_HUMAN) &&
			    (r_ptr->flags3 & RF3_HUMAN))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_HUMAN;
				}

				if (mult < 20) mult = 20;
			}

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

			/* Slay Dragon  */
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

				if ((o_ptr->name1 == ART_BARD_ARROW) &&
					(m_ptr->r_idx == MON_SMAUG) &&
					(inventory[INVEN_BOW].name1 == ART_BARD))
					mult *= 5;
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
					if (mult < 17) mult = 17;
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
					if (mult < 17) mult = 17;
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
					if (mult < 17) mult = 17;
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
					if (mult < 17) mult = 17;
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
					if (mult < 17) mult = 17;
				}
			}
			break;
		}
	}

	/* Some "bow" do extra damage */
	i_ptr = &inventory[INVEN_BOW];
	if (i_ptr->tval)
	{
		switch (o_ptr->tval)
		{
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			{
				/* Extract the flags */
				object_flags(i_ptr, &g1, &g2, &g3);

				/* Brand (Acid) */
				if (g1 & TR1_BRAND_ACID)
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
				if (g1 & TR1_BRAND_ELEC)
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
				if (g1 & TR1_BRAND_FIRE)
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
				if (g1 & TR1_BRAND_COLD)
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
				if (g1 & TR1_BRAND_POIS)
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
	}

	/* Return the total damage */
	return (tdam * mult / 10);
}


static void draw_firing_missile(object_type *o_ptr, int y, int x, int msec)
{
	/* The player can see the (on screen) missile */
	if (panel_contains(y, x) && player_can_see_bold(y, x))
	{
		char c = object_char(o_ptr);
		byte a = object_attr(o_ptr);

		/* Draw, Hilite, Fresh, Pause, Erase */
		print_rel(c, a, y, x);
		move_cursor_relative(y, x);
		Term_fresh();
		Term_xtra(TERM_XTRA_DELAY, msec);
		lite_spot(y, x);
		Term_fresh();
	}

	/* The player cannot see the missile */
	else
	{
		/* Pause anyway, for consistancy */
		Term_xtra(TERM_XTRA_DELAY, msec);
	}
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
int do_cmd_fire_aux(int item, object_type *j_ptr)
{
	int dir;
	int j, y, x, ny, nx, ty, tx, prev_y, prev_x;

	int armour, bonus, chance;

	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	int tdam;

	/* Assume no weapon of velocity or accuracy bonus. */
	int special_dam = 0;
	int special_hit = 0;

	object_type *o_ptr;

	int tdis, thits, tmul;
	int cur_dis, visible;

	int chance2;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	char o_name[MAX_NLEN];

	int msec = delay_factor * delay_factor * delay_factor;

	cave_type *c_ptr;

	/* STICK TO */
	bool stick_to = FALSE;

	/* Missile launchers of Velocity and Accuracy sometimes "supercharge" */
	switch (j_ptr->name2)
	{
		case EGO_VELOCITY:
			if (one_in_(16)) special_dam = TRUE; break;
		case EGO_ACCURACY:
			if (one_in_(16)) special_hit = TRUE; break;
	}

	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Describe the object */
	object_desc(o_name, o_ptr, OD_OMIT_PREFIX);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from thrown object plus launcher bonus */
	tdam = damroll(o_ptr->dd, o_ptr->ds) + o_ptr->to_d + j_ptr->to_d;

	/* Actually "fire" the object. */
	bonus = (p_ptr->to_b + o_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	energy_use = bow_energy(j_ptr->sval);
	tmul = bow_tmul(j_ptr->sval);

	/* Get extra "power" from "extra might" */
	if (p_ptr->xtra_might) tmul++;

	tmul = tmul * (100 + (int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);

	/* Boost the damage */
	tdam *= tmul;
	tdam /= 100;

	/* Base range */
	tdis = 10 + tmul / 40;

	/* Get a direction (or cancel) */
	project_length = tdis + 1;

	if (!get_aim_dir(&dir))
	{
		energy_use = 0;
		return (FALSE);
	}

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	project_length = 0;

	if ((tx == px) && (ty == py))
	{
		energy_use = 0;
		return (FALSE);
	}

	/* Take a (partial) turn */
	energy_use = (energy_use * 100 / thits);

	/* XTRA HACK BARD_ARROW */
	if (randint1(3) != 1 && o_ptr->name1 == ART_BARD_ARROW)
	{
		special_hit = TRUE;

		/* Describe the object */
		object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

		/* Let player know that weapon is activated. */
#ifdef JP
		msg_format("%sはうなりをあげて飛んだ！", o_name);
#endif
	}

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
	sound(SOUND_SHOOT);

	/* Fire ammo of backbiting, and it will turn on you. -LM- */
	if (i_ptr->name2 == EGO_BACKBITING)
	{
		/* Message. */
#ifdef JP
		msg_print("矢が空中で反転してあなたに当たった！");
#else
		msg_print("Your missile turns in midair and strikes you!");
#endif

		/* Calculate damage. */
		tdam = damroll(tmul * 4, i_ptr->ds);

		/* Inflict both normal and wound damage. */
#ifdef JP
		take_hit(tdam, "陰口の矢");
#else
		take_hit(tdam, "ammo of backbiting.");
#endif
		set_cut(randint1(tdam * 3));

		/* That ends that shot! */
		return (TRUE);
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save the old location */
	prev_y = y;
	prev_x = x;

	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by wilderness boundary */
		if (!in_bounds2(ny, nx)) break;

		/* Stopped by walls/doors */
		c_ptr = &cave[ny][nx];
		if (!cave_floor_grid(c_ptr) && !c_ptr->m_idx)
		{
			break;
		}

		/* Advance the distance */
		cur_dis++;

		/* Draw a missile if the player can see it */
		draw_firing_missile(i_ptr, ny, nx, msec);

		/* Save the old location */
		prev_y = y;
		prev_x = x;

		/* Save the new location */
		x = nx;
		y = ny;


		/* Monster here, Try to hit it */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			chance2 = chance - cur_dis;

			/* Note the collision */
			hit_body = TRUE;

			/* Sleeping, visible monsters are easier to hit. -LM- */
			if ((m_ptr->csleep) && (visible))
				sleeping_bonus = 5 + p_ptr->lev / 5;

			/* Monsters in rubble can take advantage of cover. -LM- */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 10 + 5;
			}
			/*
			* Monsters in trees can take advantage of cover,
			* except from rangers.
			*/
			else if (c_ptr->feat == FEAT_TREES)
			{
				terrain_bonus = r_ptr->ac / 10 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			else if (c_ptr->feat == FEAT_DEEP_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}

			/* Get effective armour class of monster. */
			armour = r_ptr->ac + terrain_bonus;

			/* Weapons of velocity sometimes almost negate monster armour. */
			if (special_hit) armour /= 3;
			if (superb_shot) armour /= 2;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2 + sleeping_bonus, armour, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
#ifdef JP
				cptr note_dies = "は死んだ。";
#else
				cptr note_dies = " dies.";
#endif

				/* Some monsters get "destroyed" */
				if (!monster_living(r_ptr))
				{
					/* Special note at death */
#ifdef JP
					note_dies = "を倒した。";
#else
					note_dies = " is destroyed.";
#endif
				}

				if (visible)
				{
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
 
					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux_shot(i_ptr, tdam, m_ptr);
				tdam = critical_shot(i_ptr, chance2, sleeping_bonus, tdam, m_ptr, visible);

				/* If a weapon of velocity activates, increase damage. */
				if (special_dam) tdam += 15;
				if (superb_shot) tdam += 15;

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Modify the damage */
				tdam = mon_damage_mod(m_ptr, tdam, 0);

				/* Complex message */
				if (wizard)
				{
#ifdef JP
					msg_format("%d/%d のダメージを与えた。", tdam, m_ptr->hp);
#else
					msg_format("You do %d (out of %d) damage.", tdam, m_ptr->hp);
#endif
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					char m_name[80];

					/* Get the monster name (or "it") */
					monster_desc(m_name, m_ptr, 0);

					/* STICK TO */
					if (i_ptr->name1 || superb_shot)
					{
						if (i_ptr->name1) stick_to = TRUE;
#ifdef JP
						msg_format("%sは%sに突き刺さった！", o_name, m_name);
#else
						msg_format("%^s have stuck into %^s!", o_name, m_name);
#endif
					}

					/* Message */
					message_pain(c_ptr->m_idx, tdam);

					/* Anger the monster */
					if (tdam > 0) anger_monster(m_ptr);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
#ifdef JP
						msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
						msg_format("%^s flees in terror!", m_name);
#endif
					}

					/* Damaged monster can counter if projectable */
					if (ironman_hengband)
					{
						m_ptr->target_y = py;
						m_ptr->target_x = px;
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	if (stick_to)
	{
		int m_idx = cave[y][x].m_idx;
		monster_type *m_ptr = &m_list[m_idx];
		int o_idx = o_pop();

		if (!o_idx)
		{
			msg_format("%sはどこかへ行った。", o_name);
			if (i_ptr->name1)
			{
				a_info[i_ptr->name1].cur_num = 0;
			}
			return (TRUE);
		}

		o_ptr = &o_list[ o_idx ];
		object_copy(o_ptr, i_ptr);

		/* Forget mark */
		o_ptr->marked &= OM_TOUCHED;

		/* Forget location */
		o_ptr->iy = o_ptr->ix = 0;

		/* Memorize monster */
		o_ptr->held_m_idx = m_idx;

		/* Build a stack */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Carry object */
		m_ptr->hold_o_idx = o_idx;
	}
	else if (cave_floor_bold(y, x))
	{
		/* Drop (or break) near that location */
		(void)drop_near(i_ptr, j, y, x);
	}
	else
	{
		/* Drop (or break) near that location */
		(void)drop_near(i_ptr, j, prev_y, prev_x);
	}

	return (TRUE);
}


int do_cmd_fire(void)
{
	int item;
	object_type *j_ptr;
	cptr q, s;

	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Require a launcher */
	if (!j_ptr->tval)
	{
#ifdef JP
		msg_print("射撃用の武器を持っていない。");
#else
		msg_print("You have nothing to fire with.");
#endif
		return (FALSE);
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->tval_ammo;

	/* Get an item */
#ifdef JP
	q = "どれを撃ちますか? ";
	s = "発射されるアイテムがありません。";
#else
	q = "Fire which item? ";
	s = "You have nothing to fire.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		return (FALSE);
	}

	/* Fire the item */
	if (!do_cmd_fire_aux(item, j_ptr)) return (FALSE);

	return (TRUE);
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
static void do_cmd_throw_aux(int item, int mult)
{
	int dir;
	int j, y, x, ny, nx, ty, tx, prev_y, prev_x;
	int ry[19], rx[19];
	int chance, chance2, tdis;
	int mul, div;
	int cur_dis, visible;

	int tdam;

	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	bool hit_body = FALSE;
	bool hit_wall = FALSE;

	char o_name[MAX_NLEN];

	int msec = delay_factor * delay_factor * delay_factor;

	u32b f1, f2, f3;
	
	cave_type *c_ptr;

	bool return_when_thrown = FALSE;
	bool do_drop = TRUE;
	bool come_back = FALSE;


	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}
	
	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
#ifdef JP
		msg_print("ふーむ、どうやら呪われているようだ。");
#else
		msg_print("Hmmm, it seems to be cursed.");
#endif
		return;
	}

	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Extract the thrown object's flags. */
	object_flags(q_ptr, &f1, &f2, &f3);

	/* Distribute the charges of rods/wands between the stacks */
	distribute_charges(o_ptr, q_ptr, 1);

	/* Single object */
	q_ptr->number = 1;

	/* Description */
	object_desc(o_name, q_ptr, OD_OMIT_PREFIX);

	/* Extract a "distance multiplier" */
	/* Changed for 'launcher' mutation */
	mul = 10 + 2 * (mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);
	if (f2 & (TR2_THROW)) div /= 2;

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	if (f2 & (TR2_THROW))
		chance = ((p_ptr->skill_tht) + ((p_ptr->to_b + q_ptr->to_h) * BTH_PLUS_ADJ));
	else
		chance = ((p_ptr->skill_tht) + (p_ptr->to_b * BTH_PLUS_ADJ));

	project_length = tdis + 1;
	
	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Predict the "target" location */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	project_length = 0;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		if(!return_when_thrown) inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}

	/* Take a turn */
	energy_use = 100;

	/* Start at the player */
	y = py;
	x = px;

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save the old location */
	prev_y = y;
	prev_x = x;

	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by wilderness boundary */
		if (!in_bounds2(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}		
		
		/* Stopped by walls/doors */
		c_ptr = &cave[ny][nx];
		if (!cave_floor_grid(c_ptr) && !c_ptr->m_idx)
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance the distance */
		cur_dis++;

		/* Draw a missile if the player can see it */
		draw_firing_missile(q_ptr, ny, nx, msec);

		/* Save the old location */
		prev_y = y;
		prev_x = x;

		/* Save the new location */
		x = nx;
		y = ny;
		ry[cur_dis-1] = ny;
		rx[cur_dis-1] = nx;


		/* Monster here, Try to hit it */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Monsters in rubble can take advantage of cover. -LM- */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/*
			 * Monsters in trees can take advantage of cover,
			 * except from rangers.
			 */
			else if (c_ptr->feat == FEAT_TREES)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			else if (c_ptr->feat == FEAT_DEEP_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}

			/* Note the collision */
			hit_body = TRUE;

			/* Calculate the projectile accuracy, modified by distance. */
			chance2 = (f2 & (TR2_THROW)) ? chance : (chance - cur_dis);

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2, r_ptr->ac + terrain_bonus, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
#ifdef JP
				cptr note_dies = "は死んだ。";
#else
				cptr note_dies = " dies.";
#endif

				/* Some monsters get "destroyed" */
				if (!monster_living(r_ptr))
				{
					/* Special note at death */
#ifdef JP
					note_dies = "を倒した。";
#else
					note_dies = " is destroyed.";
#endif
				}

				/* Handle visible monster */
				if (visible)
				{
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Hack -- Base damage from thrown object */
				tdam = damroll(q_ptr->dd, q_ptr->ds);

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
				tdam = critical_shot(q_ptr, chance2, sleeping_bonus, tdam, m_ptr, visible);
				if (q_ptr->to_d > 0)
					tdam += q_ptr->to_d;
				else
					tdam += -q_ptr->to_d;

				if (f2 & (TR2_THROW))
				{
					tdam *= (3 + mult);
					tdam += p_ptr->to_d;
				}
				else
				{
					tdam *= mult;
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Modify the damage */
				tdam = mon_damage_mod(m_ptr, tdam, 0);

				/* Complex message */
				if (wizard)
				{
#ifdef JP
					msg_format("%d/%dのダメージを与えた。",
#else
					msg_format("You do %d (out of %d) damage.",
#endif
						   tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(c_ptr->m_idx, tdam);

					/* Anger the monster */
					if ((tdam > 0) && !object_is_potion(q_ptr))
						anger_monster(m_ptr);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Sound */
						sound(SOUND_FLEE);

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
#ifdef JP
						msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
						msg_format("%^s flees in terror!", m_name);
#endif
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(q_ptr) : 0);

	/* Figurines transform */
	if ((q_ptr->tval == TV_FIGURINE) && !(p_ptr->inside_arena))
	{
		j = 100;

		if (!(summon_named_creature(y, x, q_ptr->pval, FALSE, FALSE,
				(bool)!(q_ptr->ident & IDENT_CURSED))))
#ifdef JP
			msg_print("人形は捻じ曲がり砕け散ってしまった！");
#else
			msg_print("The Figurine writhes and then shatters.");
#endif
		else if (q_ptr->ident & IDENT_CURSED)
#ifdef JP
			msg_print("これはあまり良くない気がする。");
#else
			msg_print("You have a bad feeling about this.");
#endif
	}

	/* Potions smash open */
	if (object_is_potion(q_ptr))
	{
		if (hit_body || hit_wall || (randint1(100) < j))
		{
			/* Message */
#ifdef JP
			msg_format("%sは砕け散った！", o_name);
#else
			msg_format("The %s shatters!", o_name);
#endif

			if (potion_smash_effect(0, y, x, q_ptr->k_idx))
			{
				monster_type *m_ptr = &m_list[cave[y][x].m_idx];

				/* ToDo (Robert): fix the invulnerability */
				if (cave[y][x].m_idx &&
				    !is_hostile(&m_list[cave[y][x].m_idx]) &&
				    !(m_ptr->invulner))
				{
					char m_name[80];
					monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
#ifdef JP
					msg_format("%sは怒った！", m_name);
#else
					msg_format("%^s gets angry!", m_name);
#endif
					set_hostile(&m_list[cave[y][x].m_idx]);
				}
			}

			return;
		}
		else
		{
			j = 0;
		}
	}

	/* Thrown thing will return */
	if (return_when_thrown)
	{
		int i, back_chance;

		j = -1;
		back_chance = randint1(30) + 20 + ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
		object_desc(o_name, q_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);

		if((back_chance > 30) && !one_in_(100))
		{
			for (i = cur_dis - 1; i >= 0; i--)
			{
				/* Draw a missile if the player can see it */
				draw_firing_missile(q_ptr, ry[i], rx[i], msec);
			}
			if((back_chance > 37) && !p_ptr->blind && (item >= 0))
			{
#ifdef JP
				msg_format("%sが手元に返ってきた。", o_name);
#else
				msg_format("%s comes back to you.", o_name);
#endif
				come_back = TRUE;
			}
			else
			{
				if (item >= 0)
				{
#ifdef JP
					msg_format("%sを受け損ねた！", o_name);
#else
					msg_format("%s backs, but you can't catch!", o_name);
#endif
				}
				else
				{
#ifdef JP
					msg_format("%sが返ってきた。", o_name);
#else
					msg_format("%s comes back.", o_name);
#endif
				}
				y = py;
				x = px;
			}
		}
		else
		{
#ifdef JP
			msg_format("%sが返ってこなかった！", o_name);
#else
			msg_format("%s doesn't back!", o_name);
#endif
		}
	}

	if (come_back)
	{
		do_drop = FALSE;

		if (item == INVEN_WIELD)
		{
			/* Access the wield slot */
			o_ptr = &inventory[item];

			/* Wear the new stuff */
			object_copy(o_ptr, q_ptr);

			/* Increase the weight */
			p_ptr->total_weight += q_ptr->weight;

			/* Increment the equip counter by hand */
			equip_cnt++;

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Recalculate torch */
			p_ptr->update |= (PU_TORCH);

			/* Recalculate mana XXX */
			p_ptr->update |= (PU_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}
		else
		{
			inven_carry(q_ptr);
		}
	}

	/* Drop (or break) near that location */
	if (do_drop)
	{
		if (cave_floor_bold(y, x))
		{
			/* Drop (or break) near that location */
			(void)drop_near(q_ptr, j, y, x);
		}
		else
		{
			/* Drop (or break) near that location */
			(void)drop_near(q_ptr, j, prev_y, prev_x);
		}
	}

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Throw an object from the pack or floor.
 */
void do_cmd_throw(int mult)
{
	int item;
	cptr q, s;

	/* Get an item */
#ifdef JP
	q = "どのアイテムを投げますか? ";
	s = "投げるアイテムがない。";
#else
	q = "Throw which item? ";
	s = "You have nothing to throw.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	do_cmd_throw_aux(item, mult);
}


#ifdef TRAVEL
/*
 * Hack: travel command
 */
#define TRAVEL_UNABLE 9999

static int flow_head = 0;
static int flow_tail = 0;
static s16b temp2_x[MAX_SHORT];
static s16b temp2_y[MAX_SHORT];

/* Hack: forget the "flow" information */
void forget_travel_flow(void)
{
	int x, y;

	/* Check the entire dungeon */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Forget the old data */
			travel.cost[y][x] = TRAVEL_UNABLE;
		}
	}
}

static bool travel_flow_aux(int y, int x, int n, bool wall)
{
	cave_type *c_ptr = &cave[y][x];
	int old_head = flow_head;

	n = n % TRAVEL_UNABLE;

	/* Ignore out of bounds */
	if (!in_bounds(y, x)) return wall;

	/* Ignore "pre-stamped" entries */
	if (travel.cost[y][x] != TRAVEL_UNABLE) return wall;

	/* Ignore "walls" and "rubble" (include "secret doors") */
	if (((c_ptr->feat >= FEAT_RUBBLE) && (c_ptr->feat <= FEAT_PERM_SOLID)) ||
		(c_ptr->feat == FEAT_SECRET) ||
		((!p_ptr->ffall) && (c_ptr->feat == FEAT_DARK_PIT)))
	{
		if (!wall) return wall;
	}
	else
	{
		wall = FALSE;
	}

	/* Save the flow cost */
	travel.cost[y][x] = n;
	if (wall) travel.cost[y][x] += TRAVEL_UNABLE;

	/* Enqueue that entry */
	temp2_y[flow_head] = y;
	temp2_x[flow_head] = x;

	/* Advance the queue */
	if (++flow_head == MAX_SHORT) flow_head = 0;

	/* Hack -- notice overflow by forgetting new entry */
	if (flow_head == flow_tail) flow_head = old_head;

	return wall;
}


static void travel_flow(int ty, int tx)
{
	int x, y, d;
	bool wall = FALSE;

	/* Reset the "queue" */
	flow_head = flow_tail = 0;

	if (!cave_floor_bold(ty, tx)) wall = TRUE;

	/* Add the player's grid to the queue */
	wall = travel_flow_aux(ty, tx, 0, wall);

	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		y = temp2_y[flow_tail];
		x = temp2_x[flow_tail];

		/* Forget that entry */
		if (++flow_tail == MAX_SHORT) flow_tail = 0;

		/* Add the "children" */
		for (d = 0; d < 8; d++)
		{
			/* Add that child if "legal" */
			wall = travel_flow_aux(y + ddy_ddd[d], x + ddx_ddd[d], travel.cost[y][x] + 1, wall);
		}
	}

	/* Forget the flow info */
	flow_head = flow_tail = 0;
}

void do_cmd_travel(void)
{
	int x, y, i;
	int dx, dy, sx, sy;

	if (!tgt_pt(&x, &y)) return;

	if ((x == px) && (y == py))
	{
#ifdef JP
		msg_print("すでにそこにいます！");
#else
		msg_print("You are already there!!");
#endif
		return;
	}

	if ((cave[y][x].info & CAVE_MARK) && !cave_floor_bold(y, x))
	{
#ifdef JP
		msg_print("そこには行くことができません！");
#else
		msg_print("You cannot travel there!");
#endif
		return;
	}

	travel.x = x;
	travel.y = y;

	forget_travel_flow();
	travel_flow(y, x);

	/* Travel till 255 steps */
	travel.run = 255;

	/* Paranoia */
	travel.dir = 0;

	/* Decides first direction */
	dx = abs(px - x);
	dy = abs(py - y);
	sx = ((x == px) || (dx < dy)) ? 0 : ((x > px) ? 1 : -1);
	sy = ((y == py) || (dy < dx)) ? 0 : ((y > py) ? 1 : -1);

	for (i = 1; i <= 9; i++)
	{
		if ((sx == ddx[i]) && (sy == ddy[i])) travel.dir = i;
	}
}
#endif
