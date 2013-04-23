#define CMD2_C
/* File: cmd2.c */

/* Purpose: Movement commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM)

static int chest_check(int y, int x);

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
		int yy = py + ddy_ddd[d];
		int xx = px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave[yy][xx].info & (CAVE_MARK))) continue;

		/* Not looking for this feature */
		if (cave[yy][xx].feat < f1) continue;
		if (cave[yy][xx].feat > f2) continue;

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

#endif /* defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) -- TNB */

/*
 * Move up or down via stairs.
 * deeper stores whether we are moving to a deeper or a shallower level.
 * stairs stores whether we want stairs at the player's feet or not.
 */
static void use_stairs(cptr dir, bool deeper, bool trapdoor)
{
	int i, j = (multi_stair) ? randint(5) : 1;
	int start = (trapdoor) ? START_RANDOM : START_STAIRS;

	if (dun_level && confirm_stairs)
	{
		if (!get_check("Really leave the level? ")) return;
	}

	/* Take some time... */
	energy_use = extract_energy[p_ptr->pspeed];

	if (trapdoor)
	{
		msg_print("You deliberately jump through the trap door.");
	}
	else if (!dun_level)
	{
		cur_dungeon = wild_grid[wildy][wildx].dungeon;
		p_ptr->max_dlv = 1;
		msg_format("You enter %s",dun_name+dun_defs[cur_dungeon].name);
	}
	else
	{
		msg_format("You enter a maze of %s staircases.", dir);
	}

	if (j > dun_level) j = 1;

	/* Don't allow the player to pass a quest level.
	 * This is very inefficient, but it's a very simple process.
	 */
	if (!deeper)
	{
		j *= -1;
	}
	else
	{
		for (i = 1; i < j; i++)
		{
			if (is_quest(dun_level+i)) break;
			if (dun_level+i == dun_defs[cur_dungeon].max_level) break;
		}
		j = i;
	}

	change_level(dun_level+j, start);

	/* Check for leaving dungeon */
	if(!dun_level)
	{
		recall_dungeon = cur_dungeon;
		wildx=dun_defs[cur_dungeon].x;
		wildy=dun_defs[cur_dungeon].y;
	}
}

/*
 * Go up one level -RAK-
 */
void do_cmd_go_up(void)
{
	/* Player grid */
	cave_type *c_ptr = &cave[py][px];
	bool deeper;

	/* Verify stairs */
	if (c_ptr->feat != FEAT_LESS)
	{
		msg_print("I see no up staircase here.");
		return;
	}

	deeper = (!dun_level || dun_defs[cur_dungeon].flags & DF_TOWER);
	use_stairs("up", deeper, FALSE);
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	/* Player grid */
	cave_type *c_ptr = &cave[py][px];
	bool deeper, trapdoor;

	/* Verify stairs */
	if ((c_ptr->feat != FEAT_MORE) && (c_ptr->feat != FEAT_TRAP_DOOR))
	{
		msg_print("I see no down staircase here.");
		return;
	}

	deeper = (!dun_level || ~dun_defs[cur_dungeon].flags & DF_TOWER);
	trapdoor = (c_ptr->feat == FEAT_TRAP_DOOR);
	use_stairs("down", deeper, trapdoor);
}


/*
 * Search for hidden things
 */
static void search(void)
{
	int y, x, chance;

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
				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Invisible trap */
				if (c_ptr->feat == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Gain exp */
					skill_exp(SKILL_SEARCH);
					skill_exp(SKILL_PERCEPTION);

					/* Disturb */
					disturb(0);
				}

				/* Secret door */
				if (c_ptr->feat == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");
					gain_exp(1);

					/* Gain exp */
					skill_exp(SKILL_SEARCH);
					skill_exp(SKILL_PERCEPTION);

					/* Pick a door XXX XXX XXX */
					replace_secret_door(y, x);

					/* Disturb */
					disturb(0);
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

						/* Gain exp */
						skill_exp(SKILL_SEARCH);
						skill_exp(SKILL_PERCEPTION);

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0);
					}
				}
			}
		}
	}
}


/*
 * If command_arg is set, set command_rep to it (less 1 for this iteration).
 * Otherwise, do nothing.
 */
void cnv_arg_to_rep(void)
{
	if (!command_arg) return;

	/* Set repeat count */
	command_rep = command_arg - 1;

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Cancel the arg */
	command_arg = 0;
}


/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Search */
	search();
}


/*
 * Hack -- toggle sneak mode
 */
void do_cmd_toggle_sneak(void)
{
	/* Toggle sneaking */
	p_ptr->sneaking = !p_ptr->sneaking;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE | PR_SPEED);
}



/*
 * Determine if a grid contains a chest
 */
static int chest_check(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;


	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
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
	object_kind *k_ptr = &k_info[o_ptr->k_idx];


	/* Small chests often hold "gold" */
	small = (k_ptr->extra / 10) == XT_CHEST_SMALL;

	/* Determine how much to drop (see above) */
	number = chest_number(k_ptr);

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
		if (small && !one_in(4))
		{
			/* Make some gold */
			if (!make_gold(q_ptr, FOUND_CHEST, o_ptr->k_idx, 0)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(q_ptr, FALSE, FALSE, FOUND_CHEST, o_ptr->k_idx))
				continue;
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = dun_depth;

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
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle", MON_TRAP);
		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle", MON_TRAP);
		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)add_flag(TIMED_POISONED, 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)add_flag(TIMED_PARALYZED, 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");
		for (i = 0; i < num; i++)
		{
			if (randint(100)<(dun_depth))
				activate_hi_summon();
			else
				(void)summon_specific(y, x, dun_depth, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		take_hit(damroll(5, 8), "an exploding chest", MON_TRAP);
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
	energy_use = extract_energy[p_ptr->pspeed];

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
			msg_print("You have picked the lock.");
			skill_exp(SKILL_DISARM);
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
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get requested grid */
	c_ptr = &cave[y][x];

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
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
		if (rand_int(100) < j)
		{
			/* Message */
			msg_print("You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			skill_exp(SKILL_DISARM);
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

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

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
	int y, x;

	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count closed doors (locked or jammed) */
		int num_doors = count_dt(&y, &x, FEAT_DOOR_HEAD, FEAT_DOOR_TAIL);

		/* Count chests (locked) */
		int num_chests = count_chests(&y, &x, FALSE);

		/* Set if only one target */
		if (num_doors + num_chests == 1) command_dir = coords_to_dir(y, x);
	}

#endif /* ALLOW_EASY_OPEN -- TNB */

	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a "repeated" direction */
	if (get_rep_target(&x, &y))
	{
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
			msg_print("You see nothing there to open.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

			/* Message */
			msg_print("There is a monster in the way!");

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
	if (!more) disturb(0);
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
	cave_type *c_ptr;

	bool more = FALSE;

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Broken door */
	if (c_ptr->feat == FEAT_BROKEN)
	{
		/* Message */
		msg_print("The door appears to be broken.");
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

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
	int y, x;

	cave_type *c_ptr;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Select an open door if allowed. */
	if (easy_open && count_dt(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1)
			command_dir = coords_to_dir(y, x);

#endif /* ALLOW_EASY_OPEN -- TNB */

	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a "repeated" direction */
	if (get_rep_target(&x, &y))
	{
		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Require open/broken door */
		if ((c_ptr->feat != FEAT_OPEN) && (c_ptr->feat != FEAT_BROKEN))
		{
			/* Message */
			msg_print("You see nothing there to close.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

			/* Message */
			msg_print("There is a monster in the way!");

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
	if (!more) disturb(0);
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
static void twall(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
}



/* Special wall features for do_cmd_tunnel_aux(). */
#define WALL_GOLD 1 /* Contains gold. */
#define WALL_OBJ 2 /* May contain an object */
#define WALL_DOOR 3 /* Is really a secret door. */
#define WALL_NO_DIG 4 /* Undiggable. */

/* Messages given by a particular type of wall. */
#define WMSG_TREE 0 /* Tree, Bush */
#define WMSG_WATER 1 /* Water */
#define WMSG_WALL 2 /* Walls (all types), Door */
#define WMSG_GOLD 3 /* Walls with gold in them (no success message) */
#define WMSG_RUBBLE 4 /* Rubble */
#define WMSG_MAX 5


typedef struct wall_type wall_type;
typedef struct wall_message_type wall_message_type;

struct wall_type
{
	byte feature; /* Name of the feature being dug. */
	s16b sk_min; /* Minimum p_ptr->skill_dig to dig through the wall. */
	s16b sk_max; /* Minimum skill to dig through the wall at once. */
	byte special; /* WALL_* entry for special cases. */
	byte msg; /* The set of messages to use (see wall_message) */
	cptr name; /* Name of the wall type. Derived from f_name if none. */
};

struct wall_message_type
{
	cptr cont;
	cptr fail;
	cptr succeed;
};

static wall_type wall_info[] =
{
	{FEAT_SECRET, 31, 1230, WALL_DOOR, WMSG_WALL, NULL},
	{FEAT_RUBBLE, 0, 200, WALL_OBJ, WMSG_RUBBLE, "rubble"},
	{FEAT_MAGMA, 11, 410, 0, WMSG_WALL, NULL},
	{FEAT_QUARTZ, 21, 820, 0, WMSG_WALL, NULL},
	{FEAT_MAGMA_H, 11, 410, WALL_GOLD, WMSG_GOLD, NULL},
	{FEAT_QUARTZ_H, 21, 820, WALL_GOLD, WMSG_GOLD, NULL},
	{FEAT_MAGMA_K, 11, 410, WALL_GOLD, WMSG_GOLD, "magma vein"},
	{FEAT_QUARTZ_K, 21, 820, WALL_GOLD, WMSG_GOLD, "quartz vein"},
	{FEAT_WALL_EXTRA, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_WALL_INNER, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_WALL_OUTER, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_WALL_SOLID, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_PERM_BUILDING, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_PERM_INNER, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_PERM_OUTER, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_PERM_SOLID, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_WATER, 0, 0, WALL_NO_DIG, WMSG_WATER, NULL},
	{FEAT_TREE, 41, 140, 0, WMSG_TREE, NULL},
	{FEAT_BUSH, 0, 0, 0, WMSG_TREE, NULL},
};

static const wall_message_type wall_message[WMSG_MAX] =
{
	{"You hack away at the %s.", "You leave no mark on the %s.", "You have chopped down the %s."},
	{"You empty out the %s.", "The %s fills up your tunnel as quickly as you dig!", "You have removed the %s."},
	{"You tunnel into the %s.", "You make no impression on the %s.", "You have finished the tunnel."},
	{"You tunnel into the %s.", "You make no impression on the %s.", ""},
	{"You dig in the %s.", "You make no impression on the %s.", "You have removed the %s."},
};

/*
 * Perform the basic "tunnel" command
 *
 * Assumes that the feature type is in wall_info[].feature below.
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	cave_type *c_ptr;
	wall_type *w_ptr;
	const wall_message_type *wm_ptr;
	cptr w_name;

	bool more = FALSE;

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	for (w_ptr = wall_info;; w_ptr++)
	{
		if (w_ptr->feature == c_ptr->feat) break;

		/* Paranoia - not a valid wall (should not reach here). */
		if (w_ptr == wall_info+N_ELEMENTS(wall_info))
		{
			if (alert_failure) msg_print("Strange wall type found!");
			return FALSE;
		}
	}

	/* If a name is given here, use it. */
	/* If no name has been set, read it from f_name. */
	if (w_ptr->name)
	{
		w_name = w_ptr->name;
	}
	else
	{
		w_name = format("%v", feature_desc_f2, w_ptr->feature, 0);
	}

	/* Find the message set. */
	wm_ptr = &wall_message[w_ptr->msg];

	/* Certain failure */
	if (w_ptr->special == WALL_NO_DIG || p_ptr->skill_dig < w_ptr->sk_min)
	{
		msg_format(wm_ptr->fail, w_name);
	}
	/* Normal failure */
	else if (p_ptr->skill_dig < rand_range(w_ptr->sk_min, w_ptr->sk_max))
	{
		msg_format(wm_ptr->cont, w_name);
		more = TRUE;

		/* Occasional Search XXX XXX */
		if (w_ptr->special == WALL_DOOR && rand_int(100) < 25) search();
	}
		/* Success */
	else
	{
		bool found = FALSE;

		/* Actually tunnel through wall. */
		twall(y, x);

		/* Message */
		msg_format(wm_ptr->succeed, w_name);

		/* Handle special cases. */
		switch (w_ptr->special)
		{
			case WALL_GOLD:
			{
				place_gold(y,x, FOUND_DIG, c_ptr->feat);
				found = TRUE;
				break;
			}
			case WALL_OBJ:
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE, FOUND_DIG, c_ptr->feat);

				/* Observe new object */
				if (player_can_see_bold(y, x)) found = TRUE;
				break;
			}
		}
		if (found) msg_print("You have found something!");
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
	int y, x;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a direction to tunnel, or Abort */
	if (get_rep_target(&x, &y))
	{
		/* Get grid */
		c_ptr = &cave[y][x];

		/* Oops */
		if (cave_floor_grid(c_ptr) || ((c_ptr->feat >= FEAT_MINOR_GLYPH)
			&& (c_ptr->feat <= FEAT_PATTERN_XTRA2)))
		{
			/* Message */
			msg_print("You cannot tunnel through air.");
		}

		/* No tunnelling through doors */
		else if (c_ptr->feat < FEAT_SECRET)
		{
			/* Message */
			msg_print("You cannot tunnel through doors.");
		}

		/* A monster is in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

			/* Message */
			msg_print("There is a monster in the way!");

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
	if (!more) disturb(0);
}

#ifdef ALLOW_EASY_OPEN

/*
 * easy_open_door --
 *
 * If there is a jammed/closed/locked door at the given location,
 * then attempt to unlock/open it. Return TRUE if an attempt was
 * made (successful or not), otherwise return FALSE.
 *
 * The code here should be nearly identical to that in
 * do_cmd_open_test() and do_cmd_open_aux().
 */

static bool easy_open_door(int y, int x)
{
	int i, j;

	cave_type *c_ptr = &cave[y][x];

	/* Must be a closed door */
	if (!((c_ptr->feat >= FEAT_DOOR_HEAD) && (c_ptr->feat <= FEAT_DOOR_TAIL)))
	{
		/* Nope */
		return (FALSE);
	}

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
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
		if (rand_int(100) < j)
		{
			/* Message */
			msg_print("You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			skill_exp(SKILL_DISARM);
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
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

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
	energy_use = extract_energy[p_ptr->pspeed];

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
		msg_print("You have disarmed the chest.");
		gain_exp(o_ptr->pval);
		skill_exp(SKILL_DISARM);
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


/* Forward declaration. */
static void move_player(int y, int x, int do_pickup);

/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(int y, int x)
{
	int i, j, power;

	cave_type *c_ptr;
	cptr name;

	bool more = FALSE;


	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Access trap name */
	name = format("%v", feature_desc_f2, c_ptr->feat, 0);

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
	if (rand_int(100) < j)
	{
		/* Message */
		msg_format("You have disarmed the %s.", name);

		/* Reward */
		gain_exp(power);
		skill_exp(SKILL_DISARM);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);

#ifdef ALLOW_EASY_DISARM

		/* Move the player onto the trap */
		move_player(y, x, easy_disarm);

#else /* ALLOW_EASY_DISARM -- TNB */

			/* move the player onto the trap grid */
			move_player(y, x, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
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

#ifdef ALLOW_EASY_DISARM

		/* Move the player onto the trap */
		move_player(y, x, easy_disarm);

#else /* ALLOW_EASY_DISARM -- TNB */

			/* Move the player onto the trap */
			move_player(y, x, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
	int y, x;

	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

#ifdef ALLOW_EASY_DISARM

	/* Option: Pick a direction */
	if (easy_disarm)
	{
		/* Count visible traps */
		int num_traps = count_dt(&y, &x, FEAT_TRAP_HEAD, FEAT_TRAP_TAIL);

		/* Count chests (trapped) */
		int num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps + num_chests == 1) command_dir = coords_to_dir(y, x);
	}

#endif /* ALLOW_EASY_DISARM -- TNB */

	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a direction (or abort) */
	if (get_rep_target(&x, &y))
	{
		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Check for chests */
		o_idx = chest_check(y, x);

		/* Disarm a trap */
		if (!((c_ptr->feat >= FEAT_TRAP_HEAD) &&
				(c_ptr->feat <= FEAT_TRAP_TAIL)) &&
			!o_idx)
		{
			/* Message */
			msg_print("You see nothing there to disarm.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Message */
			msg_print("There is a monster in the way!");

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
			more = do_cmd_disarm_aux(y, x);
		}
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0);
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
static bool do_cmd_bash_aux(int y, int x)
{
	int bash, temp;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Message */
	msg_print("You smash into the door!");

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

		/* Hack -- Fall through the door */
		move_player(y, x, FALSE);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
			skill_set[SKILL_TOUGH].value/2)
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
		(void)add_flag(TIMED_PARALYZED, 2 + rand_int(2));
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
	int y, x;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a "repeated" direction */
	if (get_rep_target(&x, &y))
	{
		/* Get grid */
		c_ptr = &cave[y][x];

		/* Nothing useful */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				(c_ptr->feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
			msg_print("You see nothing there to bash.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Bash a closed door */
		else
		{
			/* Bash the door */
			more = do_cmd_bash_aux(y, x);
		}
	}

	/* Unless valid action taken, cancel bash */
	if (!more) disturb(0);
}


/*
 * Attack a monster at a given location, return FALSE if it dies or disappears.
 *
 * This only aborts the repetition when the player is unable to continue
 * attacking. Any abort desired based on the monster's actions should be
 * handled elsewhere.
 */
static bool alter_monster(int y, int x)
{
	cave_type *c_ptr = cave[y]+x;

	monster_type *m_ptr = m_list+c_ptr->m_idx;

	/* Paranoia */
	if (!cave[y][x].m_idx || !m_ptr->r_idx) return FALSE;

	/* Attack it. */
	py_attack(y, x);

	/* Dead monster. */
	if (!m_ptr->r_idx) return FALSE;

	/* Vanished monster. */
	if (!m_ptr->ml) return FALSE;

	/* Still there to hit again. */
	return TRUE;
}

/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn for the initial request to prevent free
 * detection of invisible monsters.
 */
void do_cmd_alter(void)
{
	/* Track the command actually being executed by do_cmd_alter().
	 * This is necessary to enable the "fight" command to be repeated without
	 * the command continuing after the fight has finished. */
	static char alter_cmd = 0;

	int y, x;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a direction */
	if (get_rep_target(&x, &y))
	{
		/* Get grid */
		c_ptr = &cave[y][x];

		/* Take a turn */
		energy_use = extract_energy[p_ptr->pspeed];

		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			more = alter_monster(y, x);
			alter_cmd = 'H';
		}

		/* Avoid continuing to fight after the monster has gone. */
		else if (alter_cmd == 'H')
		{
			energy_use = 0;
		}

		/* Tunnel through walls */
		else if (((c_ptr->feat >= FEAT_SECRET)
			&& (c_ptr->feat < FEAT_MINOR_GLYPH)) ||
				(c_ptr->feat == FEAT_WATER) ||
				(c_ptr->feat == FEAT_TREE) ||
				(c_ptr->feat == FEAT_BUSH))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x);
			alter_cmd = 'T';
		}

		/* Bash jammed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
			&& (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x);
			alter_cmd = 'B';
		}

		/* Open closed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD)
			&& (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_open_aux(y, x);
			alter_cmd = 'o';
		}

		/* Disarm traps */
		else if ((c_ptr->feat >= FEAT_TRAP_HEAD)
			&& (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_disarm_aux(y, x);
			alter_cmd = 'D';
		}

		/* Oops */
		else
		{
			/* Oops */
			msg_print("You attack the empty air.");
			alter_cmd = ',';
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more)
	{
		disturb(0);
		alter_cmd = 0;
	}
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
	for (o_ptr = inventory; o_ptr < inventory+INVEN_PACK; o_ptr++)
	{
		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Return the spike index */
			return o_ptr;
		}
	}

	/* Oops */
	return NULL;
}



/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int                  y, x;
	object_type *o_ptr;

	cave_type *c_ptr;


	/* Get a "repeated" direction */
	if (get_rep_target(&x, &y))
	{
		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Require closed door */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				(c_ptr->feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
			msg_print("You see nothing there to spike.");
		}

		/* Get a spike */
		else if (!(o_ptr = get_spike()))
		{
			/* Message */
			msg_print("You have no spikes!");
		}

		/* Is a monster in the way? */
		else if (c_ptr->m_idx)
		{
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Go for it */
		else
		{
			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];

			/* Successful jamming */
			msg_print("You jam the door with a spike.");

			/* Convert "locked" to "stuck" XXX XXX XXX */
			if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

			/* Add one spike to the door */
			if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

			/* Use up, and describe, a single spike, from the bottom */
			item_increase(o_ptr, -1);
			item_describe(o_ptr);
			item_optimize(o_ptr);
		}
	}
}



/*
 * Modified version of get_check() to give a [y/n/q] prompt, returning
 * information available. Returns 'y', 'n' or 'q' as appropriate.
 */
static char get_check_ynq(cptr prompt)
{
	char rc;

	/* Help */
	help_track("ynq_prompt");

	rc = get_check_aux(prompt, "$!%.*s[y/n/q]%s%v", "nN\033yY\rQq", "nnnyyyqq");

	/* Leave a message. */
	message_add(format(0));

	/* Done with help */
	help_track(NULL);

	return rc;
}


/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 */
static void carry(int pickup)
{
	cave_type *c_ptr = &cave[py][px];

	s16b this_o_idx, next_o_idx = 0;

	bool gold_only = FALSE;

	/* First check the pile for squelching opportunities. */
	p_ptr->notice |= PN_FSQUELCH;
	notice_stuff();

	/* Then pick everything up. */

	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
			msg_format("You collect %ld gold pieces worth of %v.",
					(long)o_ptr->pval, object_desc_f3, o_ptr, FALSE, 0);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

			/* Delete the gold */
			delete_dun_object(o_ptr);
		}

		/* gold_only cancels the collection of objects, but gold is picked up
		 * automatically. */
		else if (gold_only);

		/* Pick up objects */
		else
		{
			bool pickup_this = FALSE;

			/* Notice previously untouched items. */
			if (~o_ptr->ident & IDENT_TOUCHED)
			{
				/* Allow further skill checks. */
				object_skill_count = 0;

				/* Only do this once per item. */
				object_touch(o_ptr);
			}

			/* Hack - ignore hidden items altogether. */
			if (hidden_p(o_ptr)) continue;

			/* Display description if needed. */
			object_track(o_ptr);

			/* Note that the pack is too full */
			if (!inven_carry_okay(o_ptr))
			{
				if (always_pickup)
				{
					msg_format("You have no room for %v.",
						object_desc_f3, o_ptr, TRUE, 3);
					continue;
				}
			}
			else if (strstr(get_inscription(o_ptr), "=g"))
			{
				pickup_this = TRUE;
			}
			else if (!pickup)
			{
				pickup_this = FALSE;
			}
			else if (carry_query_flag)
			{
				char c = get_check_ynq(format("Pick up %.*v? ",
					Term->wid-strlen("Pick up ? "),
					object_desc_f3, o_ptr, TRUE, 3));

				/* Pick up no more objects. */
				gold_only = (c == 'q');

				/* Pick up this object. */
				if (c == 'y') pickup_this = TRUE;

				/* Say nothing more. */
				else continue;
			}
			else
			{
				pickup_this = TRUE;
			}

			if (pickup_this)
			{
				/* Carry the item */
				object_type *j_ptr = inven_carry(o_ptr);

				/* Message */
				msg_format("You have %v (%c).",
					object_desc_f3, j_ptr, TRUE, 3, index_to_label(j_ptr));

				/* Remember the object */
				object_track(j_ptr);

				/* Delete the object */
				delete_dun_object(o_ptr);
			}
			else
			{
				/* Give a message. */
				msg_format("You see %v.", object_desc_f3, o_ptr, TRUE, 3);
			}
		}
	}
}






/*
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(int pickup)
{
	int x, y;

	bool more = FALSE;


	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a "repeated" direction */
	if (get_rep_target(&x, &y))
	{
		/* Take a turn */
		energy_use = extract_energy[p_ptr->pspeed];

		/* Actually move the character */
		move_player(y, x, pickup);

		/* Allow more walking */
		more = TRUE;
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0);
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay(int pickup)
{
	cave_type *c_ptr = &cave[py][px];


	/* Set repeat if requested. */
	cnv_arg_to_rep();


	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];


	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == rand_int(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous sneaking */
	if (p_ptr->sneaking)
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
		disturb(0);

		/* Hack -- enter store */
		command_new = KTRL('E');
	}
}






/*
 * Resting allows a player to safely restore his hp -RAK-
 */
void do_cmd_rest(void)
{
	/* Don't actually do anything if already resting. */
	if (command_rep)
	{
		/* Take a turn XXX XXX XXX (?) */
		energy_use = extract_energy[p_ptr->pspeed];

		return;
	}

	/* Prompt for time if needed */
	if (command_arg <= 0)
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
	energy_use = extract_energy[p_ptr->pspeed];

	/* Save the rest code */
	cnv_arg_to_rep();

	/* Cancel sneaking */
	p_ptr->sneaking = FALSE;

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
 * Handle player hitting a real trap
 */
static void hit_trap(void)
{
	int                     i, num, dam;

	cave_type               *c_ptr;

	cptr            name = "a trap";


	/* Disturb the player */
	disturb(0);

	/* Get the cave grid */
	c_ptr = &cave[py][px];

	/* Analyze XXX XXX XXX */
	switch (c_ptr->feat)
	{
		case FEAT_TRAP_HEAD + 0x00:
		{

			if (p_ptr->ffall)
			{
				msg_print("You fly over a trap door.");
			}
			else
			{
				msg_print("You fell through a trap door!");
				dam = damroll(2, 8);
				name = "a trap door";
					if (dun_defs[cur_dungeon].flags & DF_TOWER)
				{
					change_level(dun_level-1, START_RANDOM);
				}
				else
				{
					change_level(dun_level+1, START_RANDOM);
				}
			take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{

			if (p_ptr->ffall)
			{
				msg_print("You fly over a pit trap.");
			}
			else
			{
				msg_print("You fell into a pit!");
				dam = damroll(2, 6);
				name = "a pit trap";
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{


			if (p_ptr->ffall)
			{
				msg_print("You fly over a spiked pit.");
			}

			else
			{
				msg_print("You fall into a spiked pit!");
				/* Base damage */
		name = "a pit trap";
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

			name = "a spiked pit";
					dam = dam * 2;
					(void)add_flag(TIMED_CUT, randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{


			if (p_ptr->ffall)
			{
				msg_print("You fly over a spiked pit.");
			}

			else
			{
			msg_print("You fall into a spiked pit!");
				/* Base damage */
				dam = damroll(2, 6);

		name = "a pit trap";

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

			name = "a spiked pit";

					dam = dam * 2;
					(void)add_flag(TIMED_CUT, randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");
					}

					else
					{
						dam = dam * 2;
						(void)add_flag(TIMED_POISONED, randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name, MON_TRAP);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
			msg_print("There is a flash of shimmering light!");
			c_ptr->info &= ~(CAVE_MARK);
			cave_set_feat(py, px, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
				{
					(void)summon_specific(py, px, dun_depth, 0);

				}
			if ((dun_depth)>randint(100)) /* No nasty effect for low levels */
						{ do { activate_ty_curse(); } while (randint(6)==1); }
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			msg_print("You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			msg_print("You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap", MON_TRAP);
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap", MON_TRAP);
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, MON_TRAP);
				(void)add_flag(TIMED_SLOW, rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x09:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
				(void)do_dec_stat(A_STR);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0A:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
				(void)do_dec_stat(A_DEX);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0B:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
				(void)do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("A black gas surrounds you!");
			if (!p_ptr->resist_blind)
			{
				(void)add_flag(TIMED_BLIND, rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("A gas of scintillating colors surrounds you!");
			if (!p_ptr->resist_conf)
			{
				(void)add_flag(TIMED_CONFUSED, rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("A pungent green gas surrounds you!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)add_flag(TIMED_POISONED, rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("A strange white mist surrounds you!");
			if (!p_ptr->free_act)
			{
				(void)add_flag(TIMED_PARALYZED, rand_int(10) + 5);
			}
			break;
		}
	}
}


static bool pattern_tile(byte y, byte x)
{
	return
	((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
	(cave[y][x].feat >= FEAT_PATTERN_START));

}

static bool pattern_seq(byte c_y, byte c_x, byte n_y,byte  n_x)
{
	if (!(pattern_tile(c_y, c_x)) && !(pattern_tile(n_y, n_x)))
			return TRUE;

	if (cave[n_y][n_x].feat == FEAT_PATTERN_START)
	{
		if ((!(pattern_tile(c_y, c_x)))
				&& !(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			if (get_check("If you start walking the Pattern, you must walk the whole way. Ok? "))
				return TRUE;
			else
				return FALSE;
		}
		else
			return TRUE;
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_OLD) ||
			(cave[n_y][n_x].feat == FEAT_PATTERN_END) ||
			(cave[n_y][n_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (pattern_tile(c_y, c_x))
		{
			return TRUE;
		}
		else
		{
			msg_print("You must start walking the Pattern from the startpoint.");
			return FALSE;
		}
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_XTRA1)||
			(cave[c_y][c_x].feat == FEAT_PATTERN_XTRA1))
	{
		return TRUE;
	}
	else if (cave[c_y][c_x].feat == FEAT_PATTERN_START)
	{
		if (pattern_tile(n_y, n_x))
			return TRUE;
		else
			{
				msg_print("You must walk the Pattern in correct order.");
				return FALSE;
			}
	}
	else if ((cave[c_y][c_x].feat == FEAT_PATTERN_OLD) ||
			(cave[c_y][c_x].feat == FEAT_PATTERN_END) ||
			(cave[c_y][c_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (!pattern_tile(n_y, n_x))
		{
			msg_print("You may not step off from the Pattern.");
			return FALSE;
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
			msg_print("You must start walking the Pattern from the startpoint.");
			return FALSE;
		}
		else
		{
			byte ok_move = FEAT_PATTERN_START;
			switch (cave[c_y][c_x].feat)
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
					if (cheat_wzrd)
						msg_format("Funny Pattern walking, %d.", cave[c_y][c_x].feat);
					return TRUE; /* Goof-up */
			}
			if ((cave[n_y][n_x].feat == ok_move) ||
				(cave[n_y][n_x].feat == cave[c_y][c_x].feat))
				return TRUE;
			else
			{
				if (!pattern_tile(n_y, n_x))
					msg_print("You may not step off from the Pattern.");
				else
					msg_print("You must walk the Pattern in correct order.");

				return FALSE;
			}
		}
	}

}


/*
 * Actually move the player to a given location.
 */
void move_to(s16b y, s16b x)
{
		int oy, ox;

		/* Save old location */
		oy = py;
		ox = px;

		/* Move the player */
		py = y;
		px = x;

		/* Redraw new spot */
		lite_spot(py, px);

		/* Redraw old spot */
		lite_spot(oy, ox);

		/* Check for new panel (redraw map) */
		verify_panel(FALSE);

		/* Track the player's location. */
		cave_track(py, px);

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update floor objects. */
		p_ptr->notice |= PN_FSQUELCH;

		/* A different area of the grid is eligible for monster creation. */
		full_grid = MAX_FULL_GRID;

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);
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
static void move_player(int y, int x, int do_pickup)
{
	cave_type               *c_ptr;
	monster_type    *m_ptr;

	C_TNEW(m_name, MNAME_MAX, char);

	bool p_can_pass_walls = FALSE;
	bool wall_is_perma = FALSE;
	bool stormbringer = FALSE;

	/* Examine the destination */
	c_ptr = &cave[y][x];

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];


	if (inventory[INVEN_WIELD].name1 == ART_STORMBRINGER)
	{
		stormbringer = TRUE;
	}

	/* Player can not walk through "permanent walls"... */
	/* unless in Shadow Form */
	if (p_ptr->wraith_form || p_ptr->weak_wraith)
	{
		p_can_pass_walls = TRUE;
		if ((cave[y][x].feat >= FEAT_PERM_BUILDING) && (cave[y][x].feat <= FEAT_PERM_SOLID))
		{
			wall_is_perma = TRUE;
			p_can_pass_walls = FALSE;
		}
	}

	/* Hack -- attack monsters */
	if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y,x) || p_can_pass_walls))
	{
		/* Attack -- only if we can see it OR it is not in a wall */
		if ((m_ptr->smart & SM_ALLY) &&
			!(p_ptr->confused || p_ptr->image || !(m_ptr->ml) || p_ptr->stun
			|| ((p_has_mutation(MUT_BERS_RAGE)) && p_ptr->shero))
			&& (pattern_seq((byte)py,(byte)px,(byte)y,(byte)x)) &&
			((cave_floor_bold(y, x)) || (p_can_pass_walls)))
		{
			m_ptr->csleep = 0;
			/* Extract monster name (or "it") */
			strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);
			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);
			/* displace? */
			if ( stormbringer && (randint(1000)>666))
			{
				py_attack(y,x);
			}
			else if (cave_floor_bold(py, px) ||
				(r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
			{
				msg_format("You push past %s.", m_name);
				m_ptr->fy = (byte)py;
				m_ptr->fx = (byte)px;
				cave[py][px].m_idx = c_ptr->m_idx;
				c_ptr->m_idx = 0;
				update_mon(cave[py][px].m_idx, TRUE);
			}
			else
			{
				msg_format("%^s is in your way!", m_name);
				energy_use = 0;
				TFREE(m_name);
				return;
			}
			/* now continue on to 'movement' */
		}
		else
		{
			py_attack(y, x);
			TFREE(m_name);
			return;
		}
	}
#ifdef ALLOW_EASY_DISARM

	/* Disarm a visible trap */
	if ((do_pickup != easy_disarm) &&
		(c_ptr->feat >= FEAT_TRAP_HEAD) &&
		(c_ptr->feat <= FEAT_TRAP_TAIL))
	{
		(void) do_cmd_disarm_aux(y, x);
		TFREE(m_name);
		return;
	}

#endif /* ALLOW_EASY_DISARM -- TNB */


	/* Player can not walk through "walls" unless in wraith form...*/
	if (((!cave_floor_bold(y, x)) && (c_ptr->feat != FEAT_BUSH) && ((!p_can_pass_walls) ||
		(c_ptr->feat == FEAT_WILD_BORDER) || (c_ptr->feat == FEAT_PATH_BORDER))))
	{
		/* Disturb the player */
		disturb(0);

		/* Notice things in the dark */
		if (!(c_ptr->info & (CAVE_MARK)) &&
			(p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				msg_print("You feel some rubble blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Tree */
			else if (c_ptr->feat == FEAT_TREE)
			{
				msg_print("You feel a tree blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			}
			/* Water */
			else if (c_ptr->feat == FEAT_WATER)
			{
				msg_print("Your way seems to be blocked by water.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			}

			else if ((c_ptr->feat == FEAT_WILD_BORDER) || (c_ptr->feat == FEAT_PATH_BORDER))
			{

				/* Leave town */
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You stumble out of %s.",town_name+town_defs[cur_town].name);
				}
				/* Test which border has been crossed */
				if(y==0)
				{
					py=cur_hgt-2;
					wildy--;
				}
				if(y==cur_hgt-1)
				{
					py=1;
					wildy++;
				}
				if(x==0)
				{
					px=cur_wid-2;
					wildx--;
				}
				if(x==cur_wid-1)
				{
					px=1;
					wildx++;
				}
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You stumble into %s.",town_name+town_defs[cur_town].name);
				}
				change_level(0, START_WALK);
			}

			/* Closed door */
			else if (c_ptr->feat < FEAT_SECRET)
			{
				msg_print("You feel a closed door blocking your way.");
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

				/* Well, it makes sense that you lose time bumping into
					a wall _if_ you are confused, stunned or blind; but
					typing mistakes should not cost you a turn... */
			}

			/* Tree */
			else if (c_ptr->feat == FEAT_TREE)
			{
				msg_print("There is a tree blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			/* Assume that the player didn't really want to do that. */
						if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			/* Water */
			else if (c_ptr->feat == FEAT_WATER)
			{
				msg_print("You cannot swim.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y,x);
			/* Assume that the player didn't really want to do that. */
						if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			else if ((c_ptr->feat == FEAT_WILD_BORDER) || (c_ptr->feat == FEAT_PATH_BORDER))
			{

				/* Leave town */
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You leave %s.",town_name+town_defs[cur_town].name);
				}
				/* Test which border has been crossed */
				if(y==0)
				{
					py=cur_hgt-2;
					wildy--;
				}
				if(y==cur_hgt-1)
				{
					py=1;
					wildy++;
				}
				if(x==0)
				{
					px=cur_wid-2;
					wildx--;
				}
				if(x==cur_wid-1)
				{
					px=1;
					wildx++;
				}
				if(is_town_p(wildy, wildx))
				{
					cur_town = wild_grid[wildy][wildx].dungeon;
					msg_format("You enter %s.",town_name+town_defs[cur_town].name);
				}
				change_level(0, START_WALK);
			}

			/* Closed doors */
			else if (c_ptr->feat < FEAT_SECRET)
			{
#ifdef ALLOW_EASY_OPEN

				if (easy_open_door(y, x))
				{
					TFREE(m_name);
					return;
				}

#else /* ALLOW_EASY_OPEN -- TNB */

					msg_print("There is a closed door blocking your way.");

					if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
						energy_use = 0;

#endif /* ALLOW_EASY_OPEN -- TNB */
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
		TFREE(m_name);
		return;
	}

	/* Normal movement */
	if (!pattern_seq((byte)py,(byte)px,(byte)y,(byte)x))
	{
				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
				{
					energy_use = 0;
				}
				disturb(0); /* To avoid a loop with running */
				TFREE(m_name);
				return;
	}
/*    else */
	move_to(y,x);

		/* Sound */
		/* sound(SOUND_WALK); */

		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
			(0 == rand_int(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->sneaking)
		{
			search();
		}

		/* Handle "objects" */
#ifdef ALLOW_EASY_DISARM

		carry(do_pickup != always_pickup);

#else /* ALLOW_EASY_DISARM -- TNB */

			carry(do_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */


		/* Handle "store doors" */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
			(c_ptr->feat <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0);

			/* Hack -- Enter store */
			command_new = KTRL('E');
		}

		/* Discover invisible traps */
		else if (c_ptr->feat == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(py, px);

			/* Hit the trap */
			hit_trap();
		}

		/* Set off an visible trap */
		else if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
			(c_ptr->feat <= FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0);

			/* Hit the trap */
			hit_trap();
		}
	TFREE(m_name);
	}


/*
 * Hack -- Check for a "motion blocker" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are blank */
	if (!in_bounds2(y, x)) return (FALSE);

	/* Must be a motion blocker */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Must be known to the player */
	if (!(cave[y][x].info & (CAVE_MARK))) return (FALSE);

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

	/* Illegal grids are unknown */
	if (!in_bounds2(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave[y][x].info & (CAVE_MARK)) return (FALSE);

	/* Non-floor grids are unknown */
	if (!cave_floor_bold(y, x)) return (TRUE);

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


static s16b ignm_idx;

/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int                     prev_dir, new_dir, check_dir = 0;

	int                     row, col;
	int                     i, max, inv;
	int                     option, option2;

	cave_type               *c_ptr;


	/* No options yet */
	option = 0;
	option2 = 0;

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

		/* Access grid */
		c_ptr = &cave[row][col];


		/* Visible monsters abort running */
		if (c_ptr->m_idx && c_ptr->m_idx != ignm_idx)
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
				case FEAT_PERM_BUILDING:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:

				/* Surface Features */
				case FEAT_TREE:
				case FEAT_BUSH:
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
		if (inv || cave_floor_bold(row, col))
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

			/* Access grid */
			c_ptr = &cave[row][col];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!(c_ptr->info & (CAVE_MARK)) || (c_ptr->feat < FEAT_SECRET))
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

			/* Access grid */
			c_ptr = &cave[row][col];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!(c_ptr->info & (CAVE_MARK)) || (c_ptr->feat < FEAT_SECRET))
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

		/* At least one option which isn't in the current direction */
		else if (stop_corner && (option2 || option != prev_dir))
		{
			return TRUE;
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
static void run_step(int dir)
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
			disturb(0);

			/* Done */
			return;
		}

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Initialize */
		run_init(dir);

		/* Hack - attempting to run into a monster turns the run command
		 * into a command which attempts to fight the monster until the
		 * player is disturbed or the monster dies. */
		ignm_idx = cave[py+ddy[find_current]][px+ddx[find_current]].m_idx;
		if (!m_list[ignm_idx].r_idx || !m_list[ignm_idx].ml) ignm_idx = 0;
	}

	/* Keep fighting */
	else if (ignm_idx)
	{
		/* If the expected monster isn't seen to be there, stop running. */
		if ((cave[py+ddy[find_current]][px+ddx[find_current]].m_idx != ignm_idx)
			|| !(m_list[ignm_idx].r_idx) || !(m_list[ignm_idx].ml))
		{
			/* Disturb */
			disturb(0);

			/* Done */
			return;
		}
	}
	/* Keep running */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0);

			/* Done */
			return;
		}
	}

	/* Take time */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM

	move_player(py + ddy[find_current], px + ddx[find_current], FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

	move_player(py + ddy[find_current], px + ddx[find_current], always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */
}


/*
 * Run around.
 */
void do_cmd_run(void)
{
	int dir;

	/* Hack -- no running when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
	}

	/* Continue running. */
	else if (command_rep)
	{
		run_step(0);
	}

	/* Get a "repeated" direction */
	else if (get_rep_dir(&dir))
	{
		/* Run until stopped unless told otherwise. */
		if (!command_arg) command_arg = -1;

		/* Hack -- Set the run counter */
		cnv_arg_to_rep();

		/* First step */
		run_step(dir);
	}
}
