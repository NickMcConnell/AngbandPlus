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
	cave_type *c_ptr;

	/* Player grid */
	c_ptr = area(p_ptr->px, p_ptr->py);

	if (c_ptr->feat == FEAT_LESS)
	{
		/*
		 * I'm experimenting without this... otherwise the monsters get to
		 * act first when we go up stairs, theoretically resulting in a
		 * possible insta-death.
		 */
		p_ptr->state.energy_use = 0;

		/* Success */
		msgf(MSGT_STAIRS, "You enter a maze of up staircases.");

		/* Create a way back */
		p_ptr->state.create_down_stair = TRUE;

		/* Go up */
		move_dun_level(-1);

		/*
		 * Hack XXX XXX Take some time
		 *
		 * This will need to be rethought in multiplayer
		 */
		turn += 100;
	}
	else
	{
		msgf("I see no up staircase here.");
		return;
	}
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	cave_type *c_ptr;

	/* Player grid */
	c_ptr = area(p_ptr->px, p_ptr->py);

	if (c_ptr->feat != FEAT_MORE)
	{
		msgf("I see no down staircase here.");
		return;
	}
	else
	{
		p_ptr->state.energy_use = 0;

		/* Success */
		msgf(MSGT_STAIRS, "You enter a maze of down staircases.");

		/* Create a way back */
		p_ptr->state.create_up_stair = TRUE;
		
		/* Go down */
		move_dun_level(1);

		/*
		 * Hack XXX XXX Take some time
		 *
		 * This will need to be rethought in multiplayer
		 */
		turn += 100;
	}
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Search */
	search();
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
	/* Stop searching */
	if (p_ptr->state.searching)
	{
		/* Clear the searching flag */
		p_ptr->state.searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start searching */
	else
	{
		/* Set the searching flag */
		p_ptr->state.searching = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
}



/*
 * Determine if a grid contains a chest
 */
static object_type *chest_check(int x, int y)
{
	cave_type *c_ptr = area(x, y);

	object_type *o_ptr;

	/* Scan all objects in the grid */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (o_ptr);
	}
	OBJ_ITT_END;

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
static void chest_death(int x, int y, object_type *o_ptr)
{
	int number;

	byte sval, tval;

	bool small_chest;

	object_type *q_ptr;

	int level;

	/* 
	 * Pick type of item to find in the chest.
	 *
	 * Hack - chests are not on this list...
	 * this prevents chests from nesting.
	 */
	switch (randint1(8))
	{
		case 1:
		{
			/* Swords */
			tval = TV_SWORD;
			sval = SV_ANY;
			break;
		}
		case 2:
		{
			/* Boots */
			tval = TV_BOOTS;
			sval = SV_ANY;
			break;
		}

		case 3:
		{
			/* Rings */
			tval = TV_RING;
			sval = SV_ANY;
			break;
		}

		case 4:
		{
			/* Staves */
			tval = TV_STAFF;
			sval = SV_ANY;
			break;
		}

		case 5:
		{
			/* Potions */
			tval = TV_POTION;
			sval = SV_ANY;
			break;
		}

		case 6:
		{
			/* Cloaks */
			tval = TV_CLOAK;
			sval = SV_ANY;
			break;
		}

		case 7:
		{
			/* Rods */
			tval = TV_ROD;
			sval = SV_ANY;
			break;
		}
		default:
		{
			/* Match anything */
			tval = TV_GLOVES;
			sval = SV_ANY;
		}
	}

	/* Select only those types of object */
	init_match_hook(tval, sval);

	/* Prepare allocation table */
	get_obj_num_prep(kind_is_match);

	/* Small chests often hold "gold" */
	small_chest = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Determine the "value" of the items */
	level = ABS(o_ptr->pval);

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Small chests often drop gold */
		if (small_chest && one_in_(4))
		{
			/* Make some gold */
			q_ptr = make_gold(level, 0);
		}

		/* Otherwise drop an item */
		else
		{
			/* Make a good themed object */
			q_ptr = make_object(level, 15, NULL);
			if (!q_ptr) continue;
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, x, y);
	}

	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);

	make_noise(2);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int x, int y, object_type *o_ptr)
{
	int i, trap;

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
		msgf("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msgf("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msgf("A puff of green gas surrounds you!");
		(void)pois_dam(10, "poison", rand_range(10, 30));
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msgf("A puff of yellow gas surrounds you!");

		if (!(FLAG(p_ptr, TR_FREE_ACT)))
		{
			(void)inc_paralyzed(rand_range(10, 30));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = rand_range(3, 5);
		msgf("You are enveloped in a cloud of smoke!");

		for (i = 0; i < num; i++)
		{
			if (randint1(100) < p_ptr->depth)
				(void)activate_hi_summon();
			else
				(void)summon_specific(0, x, y, p_ptr->depth, 0, TRUE, FALSE,
									  FALSE);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msgf("There is a sudden explosion!");
		msgf("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		sound(SOUND_EXPLODE);
		take_hit(damroll(5, 8), "an exploding chest");
	}

	make_noise(2);
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int x, int y, object_type *o_ptr)
{
	int i, j;

	bool flag = TRUE;

	bool more = FALSE;


	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skills[SKILL_DIS];

		/* Penalize some conditions */
		if (p_ptr->tim.blind || no_lite()) i = i / 10;
		if (p_ptr->tim.confused || p_ptr->tim.image) i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (randint0(100) < j)
		{
			msgf(MSGT_OPENDOOR, "You have picked the lock.");
			gain_exp(1);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
			msgf(MSGT_LOCKPICK_FAIL, "You failed to pick the lock.");
		}
	}

	/* Allowed to open */
	if (flag)
	{
		/* Apply chest traps, if any */
		chest_trap(x, y, o_ptr);

		/* Let the Chest drop items */
		chest_death(x, y, o_ptr);
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
	return (feat == FEAT_CLOSED);
}


/*
 * Return the number of traps around (or under) the character.
 */
int count_traps(int *x, int *y, bool under)
{
	int d;
	int xx, yy;
	int count = 0;	/* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* if not searching under player continue */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* paranoia */
		if (!in_bounds2(xx, yy)) continue;

		/* Not looking for this feature */
		if (!is_visible_trap(area(xx, yy))) continue;

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
 * Return the number of doors around (or under) the character.
 */
static int count_doors(int *x, int *y, bool (*test) (int feat), bool under)
{
	int d;
	int xx, yy;
	int count = 0;	/* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* if not searching under player continue */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* paranoia */
		if (!in_boundsp(xx, yy)) continue;

		/* Must have knowledge */
		if (!(parea(xx, yy)->feat)) continue;

		/* Not looking for this feature */
		if (!((*test) (area(xx, yy)->feat))) continue;

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
static int count_chests(int *x, int *y, bool trapped)
{
	int d, count;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		o_ptr = chest_check(xx, yy);

		/* No (visible) chest is there */
		if (!o_ptr) continue;

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
static int coords_to_dir(int x, int y)
{
	int d[3][3] =
	{
	{7, 4, 1},
	{8, 5, 2},
	{9, 6, 3}
	};
	int dy = y - p_ptr->py;
	int dx = x - p_ptr->px;

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
bool do_cmd_open_aux(int x, int y)
{
	int i;

	cave_type *c_ptr;

	field_type *f_ptr;

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Get requested grid */
	c_ptr = area(x, y);

	/* Must be a closed door */
	if (c_ptr->feat != FEAT_CLOSED)
	{
		/* Nope */
		return (FALSE);
	}

	/* Get fields */
	f_ptr = field_is_type(c_ptr, FTYPE_DOOR);

	/* If the door is locked / jammed */
	if (f_ptr)
	{
		/* Get the "disarm" factor */
		i = p_ptr->skills[SKILL_DIS];

		/* Penalize some conditions */
		if (p_ptr->tim.blind || no_lite()) i = i / 10;
		if (p_ptr->tim.confused || p_ptr->tim.image) i = i / 10;

		/* Success? */
		if (!field_script_single(f_ptr, FIELD_ACT_INTERACT,
									"i", LUA_VAR_NAMED(i, "power")))
		{
			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Gain experience, but not for the locked doors in town */
			if (p_ptr->depth) gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* We may keep trying */
			return (TRUE);
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(x, y, FEAT_OPEN);

		/* Sound */
		sound(SOUND_OPENDOOR);

		make_noise(3);
	}
	
	/* We know about the change */
	note_spot(x, y);

	/* Done - no more to try. */
	return (FALSE);
}


/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
	int y, x, dir;

	object_type *o_ptr;

	cave_type *c_ptr;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_open)
	{
		int num_doors, num_chests;

		/* Count closed doors */
		num_doors = count_doors(&x, &y, is_closed, TRUE);

		/* Count chests (locked) */
		num_chests = count_chests(&x, &y, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			p_ptr->cmd.dir = coords_to_dir(x, y);
		}
	}

	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get requested location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y)) return;

		/* Get requested grid */
		c_ptr = area(x, y);

		/* Check for chest */
		o_ptr = chest_check(x, y);

		/* Nothing useful */
		if (!((c_ptr->feat == FEAT_CLOSED) || o_ptr))
		{
			/* Message */
			msgf(MSGT_NOTHING_TO_OPEN, "You see nothing there to open.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			p_ptr->state.energy_use = 100;

			/* Message */
			msgf("There is a monster in the way!");

			/* Attack */
			py_attack(x, y);
		}

		/* Handle chests */
		else if (o_ptr)
		{
			/* Open the chest */
			more = do_cmd_open_chest(x, y, o_ptr);
		}

		/* Handle doors */
		else
		{
			/* Open the door */
			more = do_cmd_open_aux(x, y);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(FALSE);
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
static bool do_cmd_close_aux(int x, int y)
{
	cave_type *c_ptr;

	bool more = FALSE;


	if ((x == p_ptr->px) && (y == p_ptr->py))
	{
		/* You cannot close a door beneith yourself */
		msgf("You cannot close it now.");

		/* No more */
		return (more);
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Get grid and contents */
	c_ptr = area(x, y);

	/* Broken door */
	if (c_ptr->feat == FEAT_BROKEN)
	{
		/* Message */
		msgf("The door appears to be broken.");
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(x, y, FEAT_CLOSED);

		/* Sound */
		sound(SOUND_SHUTDOOR);

		make_noise(3);
	}

	/* We know about the change */
	note_spot(x, y);

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

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count open doors */
		if (count_doors(&x, &y, is_open, FALSE) == 1)
		{
			p_ptr->cmd.dir = coords_to_dir(x, y);
		}
	}

	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get requested location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y))
		{
			/* Message */
			msgf("You see nothing there to close.");

			disturb(FALSE);
			return;
		}

		/* Get grid and contents */
		c_ptr = area(x, y);

		/* Require open/broken door */
		if ((c_ptr->feat != FEAT_OPEN) && (c_ptr->feat != FEAT_BROKEN))
		{
			/* Message */
			msgf("You see nothing there to close.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			p_ptr->state.energy_use = 100;

			/* Message */
			msgf("There is a monster in the way!");

			/* Attack */
			py_attack(x, y);
		}

		/* Close the door */
		else
		{
			/* Close the door */
			more = do_cmd_close_aux(x, y);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(FALSE);
}


/*
 * Tunnel through wall.  Assumes valid location.
 */
static bool twall(int x, int y, byte feat)
{
	cave_type *c_ptr = area(x, y);

	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_grid(c_ptr)) return (FALSE);

	/* Remove the feature */
	cave_set_feat(x, y, feat);

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
static bool do_cmd_tunnel_aux(int x, int y)
{
	bool more = FALSE;

	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);

	int action;

	int dig = p_ptr->skills[SKILL_DIG];

	field_type *f_ptr = field_script_find(c_ptr,
										FIELD_ACT_INTERACT_TEST, ":i", LUA_RETURN(action));

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Sound */
	sound(SOUND_DIG);

	/* Must have knowledge */
	if (!(pc_ptr->feat))
	{
		/* Message */
		msgf("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	if (f_ptr && (action == ACT_TUNNEL))
	{
		if (!field_script_single(f_ptr, FIELD_ACT_INTERACT,
									"i", LUA_VAR_NAMED(dig, "power")))
		{
			/* Finished tunneling */
			return (FALSE);
		}

		/* Keep on tunneling */
		return (TRUE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_grid(c_ptr) && !(cave_semi_grid(c_ptr)))
	{
		/* Message */
		msgf("You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Titanium */
	if (cave_perma_grid(c_ptr) && cave_wall_grid(c_ptr))
	{
		msgf("This seems to be permanent rock.");
	}

	else if ((c_ptr->feat == FEAT_TREES) || (c_ptr->feat == FEAT_PINE_TREE))
	{
		/* Chop Down */
		if ((p_ptr->skills[SKILL_DIG] > 10 + randint0(400)) && twall(x, y, FEAT_GRASS))
		{
			msgf("You have cleared away the trees.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -1);
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
			msgf("You chop away at the tree.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (one_in_(4)) search();
		}
	}

	else if (c_ptr->feat == FEAT_SNOW_TREE)
	{
		/* Chop Down */
		if ((p_ptr->skills[SKILL_DIG] > 10 + randint0(400)) && twall(x, y, FEAT_SNOW))
		{
			msgf("You have cleared away the trees.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -1);
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
			msgf("You chop away at the tree.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (one_in_(4)) search();
		}
	}

	/* Jungle */
	else if (c_ptr->feat == FEAT_JUNGLE)
	{
		/* Chop Down */
		if ((p_ptr->skills[SKILL_DIG] > 10 + randint0(800)) && twall(x, y, FEAT_BUSH))
		{
			msgf("You have cleared away the jungle.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -2);
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
			msgf("You chop away at the undergrowth.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (one_in_(4)) search();
		}
	}

	/* Granite + mountain side */
	else if (((c_ptr->feat >= FEAT_WALL_EXTRA) &&
			  (c_ptr->feat <= FEAT_WALL_SOLID)) ||
			 (c_ptr->feat == FEAT_MOUNTAIN) ||
			 (c_ptr->feat == FEAT_SNOW_MOUNTAIN) ||
			 (c_ptr->feat == FEAT_PILLAR))
	{
		/* Tunnel */
		if ((p_ptr->skills[SKILL_DIG] > 40 + randint0(1600)) && twall(x, y, the_floor()))
		{
			msgf("You have finished the tunnel.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -1);
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msgf("You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if ((c_ptr->feat >= FEAT_MAGMA) && (c_ptr->feat <= FEAT_QUARTZ_K))
	{
		bool okay;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (c_ptr->feat >= FEAT_MAGMA_K) gold = TRUE;

		/* Extract "quartz" flag XXX XXX XXX */
		if ((c_ptr->feat - FEAT_MAGMA) & 0x01) hard = TRUE;

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->skills[SKILL_DIG] > 20 + randint0(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->skills[SKILL_DIG] > 10 + randint0(400));
		}

		/* Success */
		if (okay && twall(x, y, the_floor()))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(x, y);

				/* Message */
				msgf("You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				msgf("You have finished the tunnel.");

				chg_virtue(V_DILIGENCE, 1);
				chg_virtue(V_NATURE, -1);
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			msgf("You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			msgf("You tunnel into the magma vein.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (c_ptr->feat == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skills[SKILL_DIG] > randint0(200)) && twall(x, y, the_floor()))
		{
			/* Message */
			msgf("You have removed the rubble.");

			/* Hack -- place an object */
			if (p_ptr->depth && one_in_(10))
			{
				/* Create a simple object */
				place_object(x, y, FALSE, FALSE, 0);

				/* Observe new object */
				if (player_can_see_grid(pc_ptr))
				{
					msgf("You have found something!");
				}
			}
		}

		else
		{
			/* Message, keep digging */
			msgf("You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (c_ptr->feat >= FEAT_SECRET)
	{
		/* Tunnel */
		if ((p_ptr->skills[SKILL_DIG] > 30 + randint0(1200)) && twall(x, y, the_floor()))
		{
			msgf("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msgf("You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (one_in_(4)) search();
		}
	}

	make_noise(4);

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
	int y, x, dir;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Get a direction to tunnel, or Abort */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Cannot escape the wilderness by tunneling */
		if (!in_bounds2(x, y))
		{
			/* Message */
			msgf("You cannot tunnel outside the wilderness.");

			/* Do not repeat */
			disturb(FALSE);

			/* exit */
			return;
		}

		/* Get grid */
		c_ptr = area(x, y);

		/* No tunnelling through doors */
		if (c_ptr->feat == FEAT_CLOSED)
		{
			/* Message */
			msgf("You cannot tunnel through doors.");
		}

		/* No tunnelling through air */
		else if (cave_floor_grid(c_ptr) && !cave_semi_grid(c_ptr))
		{
			/* Message */
			msgf("You cannot tunnel through air.");
		}

		/* No tunneling through obelisks */
		else if (c_ptr->feat == FEAT_OBELISK)
		{
			/* Message */
			msgf("You cannot tunnel through that.");
		}

		/* A monster is in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			p_ptr->state.energy_use = 100;

			/* Message */
			msgf("There is a monster in the way!");

			/* Attack */
			py_attack(x, y);
		}

		/* Try digging */
		else
		{
			/* Tunnel through walls */
			more = do_cmd_tunnel_aux(x, y);
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(FALSE);
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
static bool do_cmd_disarm_chest(int x, int y, object_type *o_ptr)
{
	int i, j;

	bool more = FALSE;

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Get the "disarm" factor */
	i = p_ptr->skills[SKILL_DIS];

	/* Penalize some conditions */
	if (p_ptr->tim.blind || no_lite()) i = i / 10;
	if (p_ptr->tim.confused || p_ptr->tim.image) i = i / 10;

	/* Extract the difficulty */
	j = i - o_ptr->pval;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Must find the trap first. */
	if (!object_known_p(o_ptr))
	{
		msgf("I don't see any traps.");
	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
		msgf("The chest is not trapped.");
	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
		msgf("The chest is not trapped.");
	}

	/* Success (get a lot of experience) */
	else if (randint0(100) < j)
	{
		msgf("You have disarmed the chest.");
		gain_exp(o_ptr->pval);
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint1(i) > 5))
	{
		/* We may keep trying */
		more = TRUE;
		if (flush_failure) flush();
		msgf("You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		msgf("You set off a trap!");
		sound(SOUND_FAIL);
		chest_trap(x, y, o_ptr);
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

bool do_cmd_disarm_aux(cave_type *c_ptr, int dir)
{
	int i;

	field_type *f_ptr;
	field_thaum *t_ptr;

	bool more = FALSE;

	/* Get trap */
	f_ptr = field_first_known(c_ptr, FTYPE_TRAP);

	/* This should never happen - no trap here to disarm */
	if (!f_ptr)
	{
		msgf("Error condition:  Trying to disarm a non-existant trap.");
		return (FALSE);
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Get type of trap */
	t_ptr = &t_info[f_ptr->t_idx];

	/* Get the "disarm" factor */
	i = p_ptr->skills[SKILL_DIS];

	/* Penalize some conditions */
	if (p_ptr->tim.blind || no_lite()) i = i / 10;
	if (p_ptr->tim.confused || p_ptr->tim.image) i = i / 10;

	/* Success */
	if (!field_script_single(f_ptr, FIELD_ACT_INTERACT,
								"i", LUA_VAR_NAMED(i, "power")))
	{
		/* Message */
		msgf("You have disarmed the %s.", t_ptr->name);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint1(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msgf("You failed to disarm the %s.", t_ptr->name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msgf("You set off the %s!", t_ptr->name);

		/* Move the player onto the trap */
		move_player(dir, easy_disarm);
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

	object_type *o_ptr;

	cave_type *c_ptr;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_disarm)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_traps(&x, &y, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&x, &y, TRUE);

		/* See if only one target */
		if (num_traps || num_chests)
		{
			bool too_many = (num_traps + num_chests > 1);

			if (!too_many) p_ptr->cmd.dir = coords_to_dir(x, y);
		}
	}

	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Get a direction (or abort) */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y))
		{
			/* Message */
			msgf("You see nothing there to disarm.");

			disturb(FALSE);
			return;
		}

		/* Get grid and contents */
		c_ptr = area(x, y);

		/* Check for chests */
		o_ptr = chest_check(x, y);

		/* Disarm a trap */
		if (!is_visible_trap(c_ptr) && !o_ptr)
		{
			/* Message */
			msgf("You see nothing there to disarm.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Message */
			msgf("There is a monster in the way!");

			/* Attack */
			py_attack(x, y);
		}

		/* Disarm chest */
		else if (o_ptr)
		{
			/* Disarm the chest */
			more = do_cmd_disarm_chest(x, y, o_ptr);
		}

		/* Disarm trap */
		else
		{
			/* Disarm the trap */
			more = do_cmd_disarm_aux(c_ptr, dir);
		}
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(FALSE);
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
	int y, x, dir;
	int action;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Get a direction */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y))
		{
			/* Oops */
			msgf("You attack the empty air.");

			disturb(FALSE);
			return;
		}

		/* Get grid */
		c_ptr = area(x, y);

		/* Take a turn */
		p_ptr->state.energy_use = 100;

		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			py_attack(x, y);
		}
		else if (field_script_find(c_ptr,
									FIELD_ACT_INTERACT_TEST,
									":i", LUA_RETURN(action)))
		{
			switch (action)
			{
				case ACT_TUNNEL:
				{
					/* Tunnel */
					more = do_cmd_tunnel_aux(x, y);
					break;
				}

				case ACT_DISARM:
				{
					/* Disarm */
					more = do_cmd_disarm_aux(c_ptr, dir);
					break;
				}

				case ACT_OPEN:
				{
					/* Unlock / open */
					more = do_cmd_open_aux(x, y);
					break;
				}
			}
		}

		/* Open closed doors */
		else if (c_ptr->feat == FEAT_CLOSED)
		{
			/* open */
			more = do_cmd_open_aux(x, y);
		}

		/* Tunnel through walls */
		else if (cave_wall_grid(c_ptr))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(x, y);
		}

		/* Close open doors */
		else if ((c_ptr->feat == FEAT_OPEN) || (c_ptr->feat == FEAT_BROKEN))
		{
			/* close */
			more = do_cmd_close_aux(x, y);
		}

		/* Oops */
		else
		{
			/* Oops */
			msgf("You attack the empty air.");
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(FALSE);
}


/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static object_type *get_spike(void)
{
	object_type *o_ptr;

	/* Check every item in the pack */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Save the spike index */
			return (o_ptr);
		}
	}
	OBJ_ITT_END;

	/* Oops */
	return (NULL);
}


/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int dir;
	s16b y, x;

	object_type *o_ptr;
	cave_type *c_ptr;


	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y))
		{
			/* Message */
			msgf("You see nothing there to spike.");

			disturb(FALSE);
			return;
		}

		/* Get grid and contents */
		c_ptr = area(x, y);

		/* Require closed door */
		if (c_ptr->feat != FEAT_CLOSED)
		{
			/* Message */
			msgf("You see nothing there to spike.");

			disturb(FALSE);
			return;
		}

		/* Get a spike */
		o_ptr = get_spike();

		if (!o_ptr)
		{
			/* Message */
			msgf("You have no spikes!");
		}

		/* Is a monster in the way? */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			p_ptr->state.energy_use = 100;

			/* Message */
			msgf("There is a monster in the way!");

			/* Attack */
			py_attack(x, y);
		}

		/* Go for it */
		else
		{
			/* Take a turn */
			p_ptr->state.energy_use = 100;

			/* Successful jamming */
			msgf("You jam the door with a spike.");

			/* Make a jammed door on the square */
			make_lockjam_door(x, y, 1, TRUE);

			/* Use up, and describe, a single spike, from the bottom */
			item_increase(o_ptr, -1);
		}
	}

	make_noise(4);
}



/*
 * Support code for the "Walk" command
 */
void do_cmd_walk(int pickup)
{
	int dir;

	bool more = FALSE;


	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Take a turn */
		p_ptr->state.energy_use = 100;

		/* Actually move the character */
		move_player(dir, pickup);

		/* Allow more walking */
		more = TRUE;
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(FALSE);
}



/*
 * Start running.
 */
void do_cmd_run(void)
{
	int dir;

	/* Hack -- no running when confused */
	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Hack -- Set the run counter */
		p_ptr->state.running = (p_ptr->cmd.arg ? p_ptr->cmd.arg : 1000);

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
	/* Allow repeated command */
	if (p_ptr->cmd.arg)
	{
		/* Set repeat count */
		p_ptr->cmd.rep = p_ptr->cmd.arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->cmd.arg = 0;
	}


	/* Take a turn */
	p_ptr->state.energy_use = 100;


	/* Spontaneous Searching */
	if ((p_ptr->skills[SKILL_FOS] >= 50) || one_in_(50 - p_ptr->skills[SKILL_FOS]))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->state.searching)
	{
		search();
	}


	/* Handle "objects" */
	carry(pickup);

	/* 
	 * Fields you are standing on may do something.
	 */
	field_script(area(p_ptr->px, p_ptr->py), FIELD_ACT_PLAYER_ENTER, "");
}


/*
 * Resting allows a player to safely restore his hp	-RAK-
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->cmd.arg <= 0)
	{
		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(out_val, 5,
        				"Rest (0-9999, '*' for HP/SP, '&' as needed): "))
        {
        	return;
		}
        
		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->cmd.arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			p_ptr->cmd.arg = (-1);
		}

		/* Rest some */
		else
		{
			p_ptr->cmd.arg = atoi(out_val);
			if (p_ptr->cmd.arg <= 0) return;
		}
	}


	/* Paranoia */
	if (p_ptr->cmd.arg > 9999) p_ptr->cmd.arg = 9999;

	/* The sin of sloth */
	if (p_ptr->cmd.arg > 100)
		chg_virtue(V_DILIGENCE, -1);

	/* Why are you sleeping when there's no need?  WAKE UP! */
	if ((p_ptr->chp == p_ptr->mhp) &&
		(p_ptr->csp == p_ptr->msp) &&
		!p_ptr->tim.blind && !p_ptr->tim.confused &&
		!p_ptr->tim.poisoned && !p_ptr->tim.afraid &&
		!p_ptr->tim.stun && !p_ptr->tim.cut &&
		!p_ptr->tim.slow && !p_ptr->tim.paralyzed &&
		!p_ptr->tim.image && !p_ptr->tim.word_recall)
		chg_virtue(V_DILIGENCE, -1);

	/* Take a turn XXX XXX XXX (?) */
	p_ptr->state.energy_use = 100;

	/* Save the rest code */
	p_ptr->state.resting = p_ptr->cmd.arg;

	/* Cancel searching */
	p_ptr->state.searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	Term_fresh();
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
			return (100);

			/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_SKELETON:
			return (50);

			/* Sometimes break */
		case TV_WAND:
		case TV_SPIKE:
		case TV_ARROW:
			return (25);

			/* Rarely break */
		case TV_SHOT:
		case TV_BOLT:
		default:
			return (10);
	}
}


/*
 * Calculation of critical hits for objects fired or thrown by the player. -LM-
 */
static int critical_shot(int chance, int sleeping_bonus, cptr o_name,
                         cptr m_name, int visible)
{
	int power = (chance + sleeping_bonus);
	int bonus = 0;

	if (!visible)
	{
		msgf("The %s finds a mark.", o_name);
	}

	/* Test for critical hit. */
	if (randint1(power + 240) <= power)
	{
		/*
		 * Encourage the player to throw weapons at sleeping
		 * monsters. -LM-
		 */
		if (sleeping_bonus && visible)
		{
			msgf("You rudely awaken the monster!");
		}

		/* Determine deadliness bonus from critical hit. */
		if (randint0(100) == 0)     bonus = 800;
		if (randint0(40) == 0)      bonus = 500;
		else if (randint0(12) == 0) bonus = 300;
		else if (randint0(3) == 0)  bonus = 200;
		else                        bonus = 100;

		/* Only give a message if we see it hit. */
		if (visible)
		{
			if (bonus <= 100)
			{
				msgf("The %s strikes %s.", o_name, m_name);
			}
			else if (bonus <= 200)
			{
				msgf("The %s penetrates %s.", o_name, m_name);
			}
			else if (bonus <= 300)
			{
				msgf("The %s drives into %s!", o_name, m_name);
			}
			else if (bonus <= 500)
			{
				msgf("The %s transpierces %s!", o_name, m_name);
			}
			else
			{
				msgf("The %s *smites* %s!", o_name, m_name);
			}
		}
	}

	/*
	 * If the blow is not a critical hit, display the default attack
	 * message and apply the standard multiplier.
	 */
	else
	{
		if (visible)
		{
			msgf("The %s hits %s.", o_name, m_name);
		}
	}

	return (bonus);
}



/*
 * Process the effect of hitting something with a
 * thrown item.
 */
static void throw_item_effect(object_type *o_ptr, bool hit_body, bool hit_wall,
                              bool hit_success, int x, int y)
{
	/* Chance of breakage (during attacks) */
	int breakage = (hit_body ? breakage_chance(o_ptr) : 0);

	/* Figurines transform */
	if (o_ptr->tval == TV_FIGURINE)
	{
		/* Always break */
		breakage = 100;

		if (!(summon_named_creature(x, y, o_ptr->pval, FALSE, FALSE, TRUE)))
		{
			msgf("The Figurine writhes and then shatters.");
		}
	}

	if ((FLAG(o_ptr, TR_RETURN)) && randint0(100) < 95)
	{
		msgf("The %v returns to your hand.", OBJECT_FMT(o_ptr, FALSE, 3));

		inven_carry(o_ptr);

		return;
	}

	/* Exploding arrows */
	if ((FLAG(o_ptr, TR_EXPLODE)) && hit_body && hit_success)
	{
		project(0, 2, x, y, 100, GF_FIRE, (PROJECT_JUMP |
					PROJECT_ITEM | PROJECT_KILL));

		return;
	}

	/* Potions smash open */
	if (object_is_potion(o_ptr))
	{
		if (hit_body || hit_wall || (randint1(100) < breakage))
		{
			/* Message */
			msgf("The %v shatters!", OBJECT_FMT(o_ptr, FALSE, 3));

			if (potion_smash_effect(0, x, y, o_ptr))
			{
				monster_type *m_ptr = &m_list[area(x, y)->m_idx];

				/* ToDo (Robert): fix the invulnerability */
				if (area(x, y)->m_idx &&
					!is_hostile(&m_list[area(x, y)->m_idx]) &&
					!(m_ptr->invulner))
				{
					monster_type *m2_ptr = &m_list[area(x, y)->m_idx];
				
					msgf("%^v gets angry!", MONSTER_FMT(m2_ptr, 0));
					set_hostile(m2_ptr);
				}
			}

			return;
		}
		else
		{
			breakage = 0;
		}
	}

	/* Drop (or break) near that location */
	drop_near(o_ptr, breakage, x, y);

	p_ptr->redraw |= (PR_EQUIPPY);

	make_noise(3);
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
void do_cmd_fire_aux(int mult, object_type *o_ptr, const object_type *j_ptr)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;
	int x, y, nx, ny, tx, ty;

	int armour, bonus, chance, total_deadliness;

	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	long tdam;
	int slay;

#if 0
	/* Assume no weapon of velocity or accuracy bonus. */
	int special_dam = 0;
	int special_hit = 0;
#endif /* 0 */

	int tdis, thits, tmul;
	int cur_dis;

	int mul, div;
		
	int chance2;

	object_type *i_ptr;

	char o_name[256];
	char m_name[80];

	int msec = delay_factor * delay_factor * delay_factor;

	bool hit_wall = FALSE;

	cave_type *c_ptr;

	/* This "exception" will have to be added via python. */
#if 0
	/* Missile launchers of Velocity and Accuracy sometimes "supercharge" */
	if ((j_ptr->name2 == EGO_VELOCITY) || (j_ptr->name2 == EGO_ACCURACY))
	{
		/* Occasional boost to shot. */
		if (one_in_(16))
		{
			if (j_ptr->name2 == EGO_VELOCITY) special_dam = TRUE;
			else if (j_ptr->name2 == EGO_ACCURACY) special_hit = TRUE;

			/* Let player know that weapon is activated. */
			msgf("You feel your %v tremble in your hand.",
				 OBJECT_FMT(j_ptr, FALSE, 0));
		}
	}
#endif /* 0 */

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Duplicate the object */
	i_ptr = object_dup(o_ptr);

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	item_increase(o_ptr, -1);

	/* Sound */
	if (j_ptr)
	{
		sound(SOUND_SHOOT);
	}

	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, 0, 256);

	/* Use the proper number of shots */
	if (j_ptr)
	{
		thits = p_ptr->num_fire;
	}
	else
	{
		thits = 1;

		if (p_ptr->rp.pclass == CLASS_ROGUE &&
				i_ptr->tval == TV_SWORD && i_ptr->sval == SV_DAGGER)
		{
			if (p_ptr->lev >= 10) thits++;
			if (p_ptr->lev >= 30) thits++;
		}
	}

	/* Actually "fire" the object. */
	if (j_ptr)
	{
		total_deadliness = p_ptr->to_d + i_ptr->to_d + j_ptr->to_d;
		
		bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
		chance = (p_ptr->skills[SKILL_THB] + (bonus * BTH_PLUS_ADJ));
	}
	else
	{
		total_deadliness = p_ptr->to_d + i_ptr->to_d;

		if (FLAG(i_ptr, TR_THROW))
			bonus = p_ptr->to_h + i_ptr->to_h;
		else
			bonus = i_ptr->to_h;

		chance = p_ptr->skills[SKILL_THT] + (bonus * BTH_PLUS_ADJ);
	}

	/* Cursed arrows tend not to hit anything */
	if (cursed_p(i_ptr)) chance = chance / 2;

	/* Shooter properties */
	if (j_ptr)
	{
		p_ptr->state.energy_use = p_ptr->bow_energy;
		tmul = p_ptr->ammo_mult;

		/* Get extra "power" from "extra might" */
		if ((FLAG(p_ptr, TR_XTRA_MIGHT))) tmul++;
	}
	else
	{
		p_ptr->state.energy_use = 100;

		if (FLAG(i_ptr, TR_THROW))
		{
			tmul = 5;
		}
		else
		{
			tmul = 1;
		}
	}

	/* Extract a "distance multiplier" */
	mul = 5 + 5 * tmul + 2 * (mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat[A_STR].ind] + 10) * mul / div;
	
	/* Maximum distance */
	if (tdis > mul) tdis = mul;

	/* Paranoia */
	if (tdis > MAX_RANGE) tdis = MAX_RANGE;

	/* Take a (partial) turn - note strange formula. */

	/* The real number of shots per round is (1 + n)/2 */
	p_ptr->state.energy_use = (2 * p_ptr->state.energy_use / (1 + thits));

	/* Another thing to do in python */
#if 0

	/* Fire ammo of backbiting, and it will turn on you. -LM- */
	if (i_ptr->name2 == EGO_BACKBITING)
	{
		/* Message. */
		msgf("Your missile turns in midair and strikes you!");

		/* Calculate damage. */
		tdam = damroll(tmul * 4, i_ptr->ds);

		/* Inflict both normal and wound damage. */
		take_hit(tdam, "ammo of backbiting.");
		inc_cut(randint1(tdam * 3));

		/* That ends that shot! */
		return;
	}
#endif /* 0 */

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


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Initialise the multi-move */
	mmove_init(px, py, tx, ty);

	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis;)
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove(&nx, &ny, px, py);

		/* Stopped by wilderness boundary */
		if (!in_bounds2(nx, ny)) 
		{
			hit_wall = TRUE;
			break;
		}

		/* Stopped by walls/doors */
		c_ptr = area(nx, ny);
		if (cave_wall_grid(c_ptr)) 
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance the distance */
		cur_dis++;


		/* The player can see the (on screen) missile */
		if (panel_contains(nx, ny) && player_can_see_bold(nx, ny))
		{
			char c = object_char(i_ptr);
			byte a = object_attr(i_ptr);

			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(c, a, nx, ny);
			move_cursor_relative(nx, ny);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(nx, ny);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Save the new location */
		x = nx;
		y = ny;


		/* Monster here, Try to hit it */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			chance2 = chance - cur_dis;

			/* Sleeping, visible monsters are easier to hit. -LM- */
			if ((m_ptr->csleep) && (m_ptr->ml))
				sleeping_bonus = 5 + p_ptr->lev / 5;

			/* Monsters in rubble can take advantage of cover. -LM- */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/*
			 * Monsters in trees can take advantage of cover,
			 * except from rangers.
			 */
			else if ((c_ptr->feat == FEAT_TREES) &&
				!FLAG(p_ptr, TR_WILD_SHOT))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			else if (c_ptr->feat == FEAT_DEEP_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}

			/* Get effective armour class of monster. */
			armour = r_ptr->ac + terrain_bonus;

			/* Adjacent monsters are harder to hit if awake */
			if ((cur_dis == 1) && (!sleeping_bonus)) armour += armour;

#if 0
			/* Weapons of velocity sometimes almost negate monster armour. */
			if (special_hit) armour /= 3;
#endif /* 0 */

			/* Look to see if we've spotted a mimic */
			if ((m_ptr->smart & SM_MIMIC) && m_ptr->ml)
			{
				/* We've spotted it */
				msgf("You've found %v!", MONSTER_FMT(m_ptr, 0x88));
				
				/* Toggle flag */
				m_ptr->smart &= ~(SM_MIMIC);

				/* It is in the monster list now */
				update_mon_vis(m_ptr->r_idx, 1);
			}

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2 + sleeping_bonus, armour, m_ptr->ml))
			{
				bool fear = FALSE;
				int multiplier = deadliness_calc(total_deadliness);

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if (!monster_living(r_ptr))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Get "the monster" or "it" */
				monster_desc(m_name, m_ptr, 0, 80);

				/* Hack -- Track this monster race */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack -- Track this monster */
				if (m_ptr->ml) health_track(c_ptr->m_idx);

				/*
				 * The basic damage-determination formula is the same in
				 * archery as it is in melee (apart from the launcher mul-
				 * tiplier).  See formula "py_attack" in "cmd1.c" for more
				 * details. -LM-
				 */

				/* Base damage dice. */
				tdam = i_ptr->ds;

				/* Multiply by the missile weapon multiplier. */
				tdam *= tmul;

				/* Add deadliness bonus from slays/brands */
				slay = (tot_dam_aux(i_ptr, m_ptr) - 100);
				multiplier += slay;

				/*
				 * Add deadliness bonus from critical shot
				 */
				multiplier += critical_shot(tmul > 1 ? chance2 : 0,
					sleeping_bonus, o_name, m_name, m_ptr->ml);

				/*
				 * Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				tdam *= multiplier;


				/*
				 * Get the whole number of dice sides by deflating,
				 * and then get total dice damage.
				 */
				tdam = damroll(i_ptr->dd, tdam / 100 +
							   (randint0(100) < (tdam % 100) ? 1 : 0));

				/* Add in extra effect due to slays */
				tdam += slay / 20;

#if 0
				/* If a weapon of velocity activates, increase damage. */
				if (special_dam)
				{
					tdam += 15;
				}
#endif /* 0 */

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Modify the damage */
				tdam = mon_damage_mod(m_ptr, tdam, 0);
								
				/* Drop (or break) near that location (i_ptr is now invalid) */
				throw_item_effect(i_ptr, TRUE, FALSE, TRUE, x, y);

				/* Complex message */
				if (p_ptr->state.wizard)
				{
					msgf("You do %d (out of %d) damage.",
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
					if (tdam > 0) anger_monster(m_ptr);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						flee_message(m_name, m_ptr->r_idx);
					}
				}
			}
			else
			{
				/* Drop (or break) near that location (i_ptr is now invalid) */
				throw_item_effect(i_ptr, TRUE, FALSE, FALSE, x, y);
			}

			/* Stop looking */
			return;
		}
	}

	/* Drop (or break) near that location (i_ptr is now invalid) */
	throw_item_effect(i_ptr, FALSE, hit_wall, FALSE, x, y);
}


void do_cmd_fire(void)
{
	object_type *j_ptr, *o_ptr;
	cptr q, s;

	/* Get the "bow" (if any) */
	j_ptr = &p_ptr->equipment[EQUIP_BOW];

	/* Require a launcher */
	if (!j_ptr->tval)
	{
		msgf("You have nothing to fire with.");
		return;
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Fire the item */
	do_cmd_fire_aux(1, o_ptr, j_ptr);
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
void do_cmd_throw_aux(int mult)
{
	cptr q, s;

	object_type *o_ptr;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Hack -- Cannot remove cursed items */
	if ((!o_ptr->allocated) && cursed_p(o_ptr))
	{
		/* Oops */
		msgf("Hmmm, it seems to be cursed.");

		/* Set the knowledge flag for the player */
		o_ptr->kn_flags[2] |= TR2_CURSED;

		/* Nope */
		return;
	}

	do_cmd_fire_aux(mult, o_ptr, NULL);
}


/*
 * Throw an object from the pack or floor.
 */
void do_cmd_throw(void)
{
	do_cmd_throw_aux(1);
}
