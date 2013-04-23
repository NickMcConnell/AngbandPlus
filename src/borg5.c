/* File: borg5.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG



/*
 * This file is responsible for the "borg_update" routine, which is used
 * to notice changes in the world, and to keep track of terrain features,
 * objects, monsters, both from visual changes, and from world messages.
 *
 * One big thing this file does is "object/monster tracking", which
 * attempts to gather information about the objects and monsters in
 * the dungeon, including their identity, location, and state, and
 * to "follow" them if they "move", and to delete them when they die.
 *
 * Information about terrain is used to help plan "flow" paths.  Info
 * about objects and monsters is used to optimize planning paths to
 * those objects and monsters.  Info about monsters is also used for
 * the "danger" functions, which help avoid dangerous situations.
 *
 * Normally, the Borg will never assign more than one object, or more than
 * one monster, to the same grid.  But during the "borg_update()" function,
 * it may be possible for multiple objects/monsters to be assigned to the
 * same grid.  Whether or not this is actually possible is not important.
 * If it does happen, only one of them will be "remembered" in the dungeon
 * array, and at the end of "borg_update", all non-remembered entities are
 * explicitly forgotten, just in case.
 *
 * Notes:
 *   We assume that monsters/objects can never appear in walls/doors
 *   We count the occurance of invisible or offscreen monsters
 *   We treat "mimics" and "trappers" as "invisible" monsters
 *
 * To Do:
 *   Analyze the probable danger of offscreen monsters
 *   Track approximate monster hitpoints (min/max hitpoints?)
 *   If so, factor in regeneration and various spell attacks
 *   Take account of monster "fear" when "following" monsters
 *
 * Bugs:
 *   Groups of monsters may induce faulty monster matching
 *   Teleporting monsters may induce faulty monster matching
 *   Monsters which appear singly and in groups are "weird"
 *   The timestamps are not quite in sync properly (?)
 */




/*
 * Attempt to guess what kind of object is at the given location.
 *
 * This routine should rarely, if ever, return "zero".
 *
 * Hack -- we use "base level" instead of "allocation levels".
 */
static int borg_guess_kind(byte a, char c)
{
	int i, s;

	int b_i = 0, b_s = 0;


	/* Find an "acceptable" object */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip non-objects */
		if (!k_ptr->name) continue;


		/* Base score */
		s = 10000;


		/* Hack -- penalize "extremely" out of depth */
		if (k_ptr->level > b_ptr->depth + 50) s = s - 500;

		/* Hack -- penalize "very" out of depth */
		if (k_ptr->level > b_ptr->depth + 15) s = s - 100;

		/* Hack -- penalize "rather" out of depth */
		if (k_ptr->level > b_ptr->depth + 5) s = s - 50;

		/* Hack -- penalize "somewhat" out of depth */
		if (k_ptr->level > b_ptr->depth) s = s - 10;

		/* Hack -- Penalize "depth miss" */
		s = s - ABS(k_ptr->level - b_ptr->depth);


		/* Hack -- Penalize INSTA_ART items */
		if (k_ptr->flags3 & TR3_INSTA_ART) s = s - 1000;


		/* Hack -- Penalize CURSED items */
		if (k_ptr->flags3 & TR3_LIGHT_CURSE) s = s - 5000;

		/* Hack -- Penalize BROKEN items */
		if (k_ptr->cost <= 0) s = s - 5000;


		/* Verify char */
		if (c != k_ptr->x_char) continue;

		/* Flavored objects */
		if (k_ptr->flavor)
		{
			/* Hack -- penalize "flavored" objects */
			s = s - 20;
		}

		/* Normal objects */
		else
		{
			/* Verify attr */
			if (a != k_ptr->x_attr) continue;
		}


		/* Desire "best" possible score */
		if (b_i && (s < b_s)) continue;

		/* Track it */
		b_i = i; b_s = s;
	}

	/* Result */
	return (b_i);
}


/*
 * Delete an old "object" record
 */
static void borg_delete_take(int i)
{
	auto_take *take = &borg_takes[i];

	/* Paranoia -- Already wiped */
	if (!take->k_idx) return;

	/* Note */
	borg_note(format("# Forgetting an object '%s' at (%d,%d)",
	                 (k_name + k_info[take->k_idx].name),
	                 take->y, take->x));

	/* Update the grids (carefully) */
	if (borg_cave_o_idx[take->y][take->x] == i)
	{
		borg_cave_o_idx[take->y][take->x] = 0;
	}

	/* Kill the object */
	WIPE(take, auto_take);

	/* One less object */
	borg_takes_cnt--;

	/* Forget task */
	borg_task = 0;
}


/*
 * Determine if an object is "expected" to be "viewable"
 *
 * Since certain objects may really be monsters, we only "expect" objects
 * in the most "obvious" grid locations (including the player grid) to be
 * viewable, to prevent objects which are really monsters from suddenly
 * "disappearing" as we walk "towards" them, resulting in infinite loops.
 *
 * We would *like* to use "CAVE_SEEN", instead of "CAVE_VIEW", in the test
 * below, but "CAVE_SEEN" is usually incorrect for grids which have always
 * contained objects, since it is not easy to tell if such grids are lit.
 *
 * Unfortunately, this means that objects which are really monsters will
 * still be deleted if they are located in "dark" grids, or outside the
 * player's "infravision" range (where relevant).
 *
 * As a hack, we could assume that only objects which are actually in the
 * player's current grid should be "visable", which would cause them only
 * to be deleted when they were stepped on, and which might simplify the
 * creation of code to "predict" objects dropped by monsters.  If we do
 * this, we must be careful to deal with predicted object locations which
 * end up being inside walls, to prevent attempting to dig into the wall.
 *
 * We assume that objects under monsters are not expected to be visible.
 */
static bool borg_follow_take_aux(int i, int y, int x)
{
	/* auto_take *take = &borg_takes[i]; */


	/* Not on-screen */
	if (!borg_panel_contains(y, x)) return (FALSE);


	/* Not in line of sight */
	if (!(borg_cave_info[y][x] & (CAVE_VIEW))) return (FALSE);


	/* Under a monster */
	if (borg_cave_m_idx[y][x]) return (FALSE);


	/* Assume viewable */
	return (TRUE);
}


/*
 * Attempt to "follow" (or expire) a missing object
 *
 * This routine is not called when the player is blind or hallucinating.
 *
 * This function just deletes objects which have disappeared.
 *
 * We assume that a monster walking onto an object destroys the object.
 */
static void borg_follow_take(int i)
{
	int ox, oy;

	auto_take *take = &borg_takes[i];


	/* Paranoia */
	if (!take->k_idx) return;


	/* Old location */
	ox = take->x;
	oy = take->y;


	/* Out of sight, assume stationary */
	if (!borg_follow_take_aux(i, oy, ox)) return;


	/* Note */
	borg_note(format("# There was an object '%s' at (%d,%d)",
	                 (k_name + k_info[take->k_idx].name),
	                 oy, ox));


	/* Kill the object */
	borg_delete_take(i);
}



/*
 * Obtain a new "take" index
 */
static int borg_new_take(int k_idx, int y, int x, byte a, char c)
{
	int i, n = -1;

	auto_take *take;


	/* Look for a "dead" object */
	for (i = 1; (n < 0) && (i < borg_takes_nxt); i++)
	{
		/* Reuse "dead" objects */
		if (!borg_takes[i].k_idx) n = i;
	}

	/* Allocate a new object */
	if ((n < 0) && (borg_takes_nxt < BORG_MAX_TAKE))
	{
		/* Acquire the entry, advance */
		n = borg_takes_nxt++;
	}

	/* Hack -- steal an old object */
	if (n < 0)
	{
		/* Note */
		borg_note("# Too many objects");

		/* Hack -- Pick a random object */
		n = rand_range(1, BORG_MAX_TAKE-1);

		/* Delete it */
		borg_delete_take(n);
	}


	/* Count new object */
	borg_takes_cnt++;

	/* Obtain the object */
	take = &borg_takes[n];

	/* Save the kind */
	take->k_idx = k_idx;

	/* Save attr/char */
	take->t_a = a;
	take->t_c = c;

	/* Save the location */
	take->x = x;
	take->y = y;

	/* Save the index */
	borg_cave_o_idx[y][x] = n;

	/* Timestamp */
	take->when = borg_time;

	/* Note */
	borg_note(format("# Creating an object '%s' at (%d,%d)",
	                 (k_name + k_info[take->k_idx].name),
	                 take->y, take->x));

	/* Forget task */
	borg_task = 0;

	/* Result */
	return (n);
}



/*
 * Attempt to notice a changing "take"
 */
static bool observe_take_diff(int y, int x, byte a, char c)
{
	int i, k_idx;

	auto_take *take;

	/* Guess the kind */
	k_idx = borg_guess_kind(a, c);

	/* Oops */
	if (!k_idx) return (FALSE);

	/* Make a new object */
	i = borg_new_take(k_idx, y, x, a, c);

	/* Get the object */
	take = &borg_takes[i];

	/* Timestamp */
	take->when = borg_time;

	/* Okay */
	return (TRUE);
}


/*
 * Attempt to "track" a "take" at the given location
 * Assume that the object has not moved more than "d" grids
 * Note that, of course, objects are never supposed to move,
 * but we may want to take account of "falling" missiles later.
 */
static bool observe_take_move(int y, int x, int d, byte a, char c)
{
	int i, z, ox, oy;

	object_kind *k_ptr;

	/* Scan the objects */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		auto_take *take = &borg_takes[i];

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Skip assigned objects */
		if (take->seen) continue;

		/* Require matching char */
		if (c != take->t_c) continue;

		/* Require matching attr */
		if (a != take->t_a) continue;

		/* Extract old location */
		ox = take->x;
		oy = take->y;

		/* Calculate distance */
		z = distance(oy, ox, y, x);

		/* Possible match */
		if (z > d) continue;

		/* Access kind */
		k_ptr = &k_info[take->k_idx];

		/* Actual movement (?) */
		if (z)
		{
			/* Update the grids (carefully) */
			if (borg_cave_o_idx[take->y][take->x] == i)
			{
				borg_cave_o_idx[take->y][take->x] = 0;
			}

			/* Track it */
			take->x = x;
			take->y = y;

			/* Update the grids */
			borg_cave_o_idx[take->y][take->x] = i;

			/* Note */
			borg_note(format("# Tracking an object '%s' at (%d,%d) from (%d,%d)",
			                 (k_name + k_ptr->name),
			                 take->y, take->x, oy, ox));

			/* Forget task */
			borg_task = 0;
		}

		/* Timestamp */
		take->when = borg_time;

		/* Mark as seen */
		take->seen = TRUE;

		/* Done */
		return (TRUE);
	}

	/* Oops */
	return (FALSE);
}




/*
 * Attempt to guess what type of monster is at the given location.
 *
 * If we are unable to think of any monster that could be at the
 * given location, we will assume the monster is a player ghost.
 * This is a total hack, but may prevent crashes.  XXX XXX XXX
 *
 * The guess can be improved by the judicious use of a specialized
 * "attr/char" mapping, especially for unique monsters.  Currently,
 * the Borg does not stoop to such redefinitions.
 *
 * We will probably fail to identify "trapper" and "lurker" monsters,
 * since they look like whatever they are standing on.  Now we will
 * probably just assume they are player ghosts.  XXX XXX XXX
 *
 * Note that "town" monsters may only appear in town, and in "town",
 * only "town" monsters may appear, unless we summon or polymorph
 * a monster while in town, which should never happen.
 *
 * To guess which monster is at the given location, we consider every
 * possible race, keeping the race (if any) with the best "score".
 *
 * Certain monster races are "impossible", including town monsters
 * in the dungeon, dungeon monsters in the town, unique monsters
 * known to be dead, monsters more than 50 levels out of depth,
 * and monsters with an impossible char, or an impossible attr.
 *
 * Certain aspects of a monster race are penalized, including extreme
 * out of depth, minor out of depth, clear/multihued attrs.
 *
 * Certain aspects of a monster race are rewarded, including monsters
 * that appear in groups, monsters that reproduce, monsters that have
 * been seen on this level a lot.
 *
 * We are never called for "trapper", "lurker", or "mimic" monsters.
 *
 * The actual rewards and penalties probably need some tweaking.
 *
 * Hack -- try not to choose "unique" monsters, or we will flee a lot.
 *
 * Note that when a monster of one color pushes past another monster of
 * the same char code but a different attr code, in some situations, this
 * function will be called to see if any multi-hued monsters might apply,
 * and if not, the "player ghost" message below will be generated.
 */
static int borg_guess_race(byte a, char c, bool multi)
{
	int i, s, n;

	int b_i = 0, b_s = 0;


	/* Find an "acceptable" monster */
	for (i = 1; i < MAX_R_IDX-1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;


		/* Base score */
		s = 10000;


		/* Verify char */
		if (c != r_ptr->x_char) continue;

		/* Clear or multi-hued monsters */
		if (r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR))
		{
			/* Penalize "weird" monsters */
			if (!multi) s = s - 1000;
		}

		/* Normal monsters */
		else
		{
			/* Verify multi */
			if (multi) continue;

			/* Verify attr */
			if (a != r_ptr->x_attr) continue;
		}


		/* Check uniques */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/* Hack -- Dead uniques stay dead */
			if (borg_race_death[i] > 0) continue;

			/* Prefer normals */
			s = s - 10;
		}


		/* Hack -- penalize "extremely" out of depth */
		if (r_ptr->level > b_ptr->depth + 50) continue;

		/* Hack -- penalize "very" out of depth */
		if (r_ptr->level > b_ptr->depth + 15) s = s - 100;

		/* Hack -- penalize "rather" out of depth */
		if (r_ptr->level > b_ptr->depth + 5) s = s - 50;

		/* Hack -- penalize "somewhat" out of depth */
		if (r_ptr->level > b_ptr->depth) s = s - 10;

		/* Penalize "depth miss" */
		s = s - ABS(r_ptr->level - b_ptr->depth);


		/* Hack -- Reward group monsters */
		if (r_ptr->flags1 & (RF1_FRIEND | RF1_FRIENDS)) s = s + 5;

		/* Hack -- Reward multiplying monsters */
		if (r_ptr->flags2 & RF2_MULTIPLY) s = s + 10;


		/* Count occurances */
		n = borg_race_count[i];

		/* Mega-Hack -- Reward occurances XXX XXX XXX */
		s = s + (n / 100) + (((n < 100) ? n : 100) / 10) + ((n < 10) ? n : 10);


		/* Desire "best" possible score */
		if (b_i && (s < b_s)) continue;

		/* Track it */
		b_i = i; b_s = s;
	}

	/* Success */
	if (b_i) return (b_i);


	/* Message */
	borg_note(format("# Assuming player ghost (char %d, attr %d)", c, a));

	/* Assume player ghost */
	return (MAX_R_IDX - 1);
}


/*
 * Attempt to convert a monster name into a race index
 *
 * We handle the new "offscreen" notation, but we do not pass back
 * any indication that the monster must be offscreen.
 *
 * First we check for all possible "unique" monsters, including
 * ones we have killed, and even if the monster name is "prefixed"
 * (as in "The Tarrasque" and "The Lernean Hydra").  Since we use
 * a fast binary search, this is acceptable.
 *
 * Otherwise, if the monster is NOT named "The xxx", we assume it
 * must be a "player ghost" (which is impossible).
 *
 * Then, we do a binary search on all "normal" monster names, using
 * a search which is known to find the last matching entry, if one
 * exists, and otherwise to find an entry which would follow the
 * matching entry if there was one, unless the matching entry would
 * follow all the existing entries, in which case it will find the
 * final entry in the list.  Thus, we can search *backwards* from
 * the result of the search, and know that we will access all of
 * the matching entries, as long as we stop once we find an entry
 * which does not match, since this will catch all cases above.
 *
 * Finally, we assume the monster must be a "player ghost" (which
 * as noted above is impossible), which is a hack, but may prevent
 * crashes, even if it does induce strange behavior.
 */
static int borg_guess_race_name(cptr who)
{
	int k, m, n;

	int i, b_i = 0;
	int s, b_s = 0;

	monster_race *r_ptr;

	char partial[160];

	int len = strlen(who);

	bool offscreen = FALSE;

	/* Verify length */
	if (len >= 80)
	{
		/* Message */
		borg_note(format("# Assuming player ghost (%s)", who));

		/* Oops */
		return (MAX_R_IDX-1);
	}	


	/* Hack -- handle "offscreen" */
	if (suffix(who, " (offscreen)"))
	{
		/* Remove the suffix */
		strcpy(partial, who);
		partial[len - 12] = '\0';
		who = partial;

		/* Message */
		borg_note(format("# Handling offscreen monster (%s)", who));

		/* Set flag */
		offscreen = TRUE;
	}


	/* Start the search */
	m = 0; n = ab_ptr->unique_num;

	/* Binary search */
	while (m < n - 1)
	{
		/* Pick a "middle" entry */
		i = (m + n) / 2;

		/* Search to the right (or here) */
		if (strcmp(ab_ptr->unique_text[i], who) <= 0)
		{
			m = i;
		}

		/* Search to the left */
		else
		{
			n = i;
		}
	}

	/* Check for equality */
	if (streq(who, ab_ptr->unique_text[m]))
	{
		/* Use this monster */
		if (!offscreen)
			return (ab_ptr->unique_what[m]);
		else return (-(ab_ptr->unique_what[m]));
	}
	

	/* Assume player ghost */
	if (!prefix(who, "The "))
	{
		/* Message */
		borg_note(format("# Assuming player ghost (%s)", who));

		/* Oops */
		return (MAX_R_IDX-1);
	}

	/* Skip the prefix */
	who += 4;


	/* Start the search */
	m = 0; n = ab_ptr->normal_num;

	/* Binary search */
	while (m < n - 1)
	{
		/* Pick a "middle" entry */
		i = (m + n) / 2;

		/* Search to the right (or here) */
		if (strcmp(ab_ptr->normal_text[i], who) <= 0)
		{
			m = i;
		}

		/* Search to the left */
		else
		{
			n = i;
		}
	}

	/* Scan possibilities */
	for (k = m; k >= 0; k--)
	{
		/* Stop when done */
		if (!streq(who, ab_ptr->normal_text[k])) break;

		/* Extract the monster */
		i = ab_ptr->normal_what[k];

		/* Access the monster */
		r_ptr = &r_info[i];

		/* Basic score */
		s = 1000;

		/* Penalize "depth miss" */
		s = s - ABS(r_ptr->level - b_ptr->depth);

		/* Track best */
		if (b_i && (s < b_s)) continue;

		/* Track it */
		b_i = i; b_s = s;
	}

	/* Success */
	if (!offscreen && b_i)
	{
		return (b_i);
	}
	else if (b_i)
	{
		return (-(b_i));
	}


	/* Message */
	borg_note(format("# Assuming player ghost (%s)", who));

	/* Oops */
	return (MAX_R_IDX-1);
}


/*
 * Hack -- Update a "new" monster
 */
static void borg_update_kill_new(int i)
{
	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


	/* Guess at the monster speed */
	kill->speed = (r_ptr->speed);

#if 0
	/* Hack -- assume optimal racial variety */
	if (!(r_ptr->flags1 & RF1_UNIQUE))
	{
		/* Hack -- Assume full speed bonus */
		kill->speed += (extract_energy[kill->speed] / 10);
	}
#endif


	/* Extract max hitpoints */
	kill->power = kill->curhp = r_ptr->hdice * r_ptr->hside;


	/* Some monsters never sleep */
	if (r_ptr->sleep == 0) kill->awake = TRUE;

	/* Some monsters never move */
	if (r_ptr->flags1 & RF1_NEVER_MOVE) kill->awake = TRUE;
	
	/* Track uniques */
	if (r_ptr->flags1 & RF1_UNIQUE)
	{
		/* If unique isn't out of depth, stick around to kill it */
		if (r_ptr->level <= b_ptr->depth)
		{
			/* Boost bravery */
			borg_unique_level = TRUE;
			borg_unique_near = TRUE;

			/* Note */
			borg_note(format("# Fighting unique '%s'", r_ptr->name + r_name));
		}
		else
		{
			/* Note */
			borg_note(format("# Out of depth unique '%s'", r_ptr->name + r_name));
		}
	}
}


/*
 * Hack -- Update an "old" monster
 *
 * We round the player speed down, and the monster speed up,
 * and we assume maximum racial speed for each monster.
 */
static void borg_update_kill_old(int i)
{
	int t, e;

	auto_kill *kill = &borg_kills[i];

	/* Player energy per game turn */
	e = extract_energy[b_ptr->pspeed];

	/* Game turns per player move */
	t = (100 + (e - 1)) / e;

	/* Monster energy per game turn */
	e = extract_energy[kill->speed];

	/* Monster moves (times ten) */
	kill->moves = (t * e) / 10;
}


/*
 * Delete an old "kill" record
 */
static void borg_delete_kill(int i)
{
	auto_kill *kill = &borg_kills[i];
	monster_race *r_ptr;

	/* Paranoia -- Already wiped */
	if (!kill->r_idx) return;

	/* Acquire race pointer */
	r_ptr = &r_info[kill->r_idx];

	/* Note */
	borg_note(format("# Forgetting a monster '%s' at (%d,%d)",
	                 (r_name + r_info[kill->r_idx].name),
	                 kill->y, kill->x));

	/* Update the grids (carefully) */
	if (borg_cave_m_idx[kill->y][kill->x] == i)
	{
		borg_cave_m_idx[kill->y][kill->x] = 0;
	}

	/* No longer "near" a unique */
	borg_unique_near = FALSE;

	/* Kill the monster */
	WIPE(kill, auto_kill);

	/* One less monster */
	borg_kills_cnt--;

	/* Recalculate danger */
	borg_do_wipe_danger = TRUE;

	/* Forget task */
	borg_task = 0;
}



/*
 * Determine if a monster is "expected" to be "viewable"
 */
static bool borg_follow_kill_aux(int i, int y, int x)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int d;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


	/* Distance to player */
	d = distance(py, px, y, x);

	/* Too far away */
	if (d > MAX_SIGHT) return (FALSE);


	/* Not on-screen */
	if (!borg_panel_contains(y, x)) return (FALSE);


	/* Use "illumination" */
	if (borg_cave_info[y][x] & (CAVE_SEEN))
	{
		/* We can see invisible */
		if (b_ptr->see_inv) return (TRUE);

		/* Monster is not invisible */
		if (!(r_ptr->flags2 & RF2_INVISIBLE)) return (TRUE);
	}

	/* Line of sight */
	if (borg_cave_info[y][x] & (CAVE_VIEW))
	{
		/* Use "infravision" */
		if (d <= b_ptr->see_infra)
		{
			/* Infravision works on "warm" creatures */
			if (!(r_ptr->flags2 & RF2_COLD_BLOOD)) return (TRUE);
		}
	}


	/* Telepathy requires "telepathy" */
	if (b_ptr->telepathy)
	{
		/* Telepathy fails on "strange" monsters */
		if (r_ptr->flags2 & RF2_EMPTY_MIND) return (FALSE);
		if (r_ptr->flags2 & RF2_WEIRD_MIND) return (FALSE);

		/* Success */
		return (TRUE);
	}


	/* Nope */
	return (FALSE);
}


/*
 * Attempt to "follow" (or expire) a missing monster
 *
 * This function is not called when the player is blind or hallucinating.
 *
 * Currently this function is a total hack, but handles the case of only
 * one possible location (taking it), two adjacent possible locations
 * (taking the diagonal one), and three adjacent locations with the
 * central one being a diagonal (taking the diagonal one).
 *
 * We should perhaps handle the case of three adjacent locations with
 * the central one being a non-diagonal (taking the non-diagonal one).
 *
 * We should perhaps attempt to take into account "last known direction",
 * which would allow us to "predict" motion up to walls, and we should
 * perhaps attempt to take into account the "standard" flee algorithm,
 * though that feels a little like cheating.
 */
static void borg_follow_kill(int i)
{
	int j;
	int x, y;
	int ox, oy;

	int dx, b_dx = 0;
	int dy, b_dy = 0;

	auto_kill *kill = &borg_kills[i];


	/* Paranoia */
	if (!kill->r_idx) return;


	/* Old location */
	ox = kill->x;
	oy = kill->y;


	/* Out of sight, assume stationary */
	if (!borg_follow_kill_aux(i, oy, ox)) return;


	/* Note */
	borg_note(format("# There was a monster '%s' at (%d,%d)",
	                 (r_name + r_info[kill->r_idx].name),
	                 oy, ox));


	/* Prevent silliness */
	if (!borg_cave_floor_bold(oy, ox))
	{
		/* Delete the monster */
		borg_delete_kill(i);

		/* Done */
		return;
	}

	/* Prevent loops */
	if (rand_int(100) < 1)
	{
		/* Just delete the monster */
		borg_delete_kill(i);

		/* Done */
		return;
	}


	/* Scan locations */
	for (j = 0; j < 8; j++)
	{
		/* Access offset */
		dx = ddx_ddd[j];
		dy = ddy_ddd[j];

		/* Access location */
		x = ox + dx;
		y = oy + dy;

		/* Skip known walls and doors */
		if (!borg_cave_floor_bold(y, x)) continue;

		/* Skip known monsters */
		if (borg_cave_m_idx[y][x]) continue;

		/* Skip visible grids */
		if (borg_follow_kill_aux(i, y, x)) continue;

		/* Collect the offsets */
		b_dx += dx;
		b_dy += dy;
	}


	/* Don't go too far */
	if (b_dx < -1) b_dx = -1;
	else if (b_dx > 1) b_dx = 1;

	/* Don't go too far */
	if (b_dy < -1) b_dy = -1;
	else if (b_dy > 1) b_dy = 1;


	/* Access location */
	x = ox + b_dx;
	y = oy + b_dy;

	/* Avoid walls and doors */
	if (!borg_cave_floor_bold(y, x))
	{
		/* Just delete the monster */
		borg_delete_kill(i);

		/* Done */
		return;
	}

	/* Avoid monsters */
	if (borg_cave_m_idx[y][x])
	{
		/* Just delete the monster */
		borg_delete_kill(i);

		/* Done */
		return;
	}


	/* Assume awake */
	kill->awake = TRUE;

	/* Update the grids (carefully) */
	if (borg_cave_m_idx[kill->y][kill->x] == i)
	{
		borg_cave_m_idx[kill->y][kill->x] = 0;
	}

	/* Save the old Location */
	kill->ox = ox;
	kill->oy = oy;

	/* Save the Location */
	kill->x = x;
	kill->y = y;

	/* Update the grids */
	borg_cave_m_idx[kill->y][kill->x] = i;

	/* Note */
	borg_note(format("# Following a monster '%s' to (%d,%d) from (%d,%d)",
	                 (r_name + r_info[kill->r_idx].name),
	                 kill->y, kill->x, oy, ox));

	/* Recalculate danger */
	borg_do_wipe_danger = TRUE;

	/* Forget task */
	borg_task = 0;
}



/*
 * Obtain a new "kill" index
 */
static int borg_new_kill(int r_idx, int y, int x, byte a, char c)
{
	int i, n = -1;

	auto_kill *kill;


	/* Look for a "dead" monster */
	for (i = 1; (n < 0) && (i < borg_kills_nxt); i++)
	{
		/* Skip real entries */
		if (!borg_kills[i].r_idx) n = i;
	}

	/* Allocate a new monster */
	if ((n < 0) && (borg_kills_nxt < BORG_MAX_KILL))
	{
		/* Acquire the entry, advance */
		n = borg_kills_nxt++;
	}

	/* Hack -- steal an old monster */
	if (n < 0)
	{
		/* Note */
		borg_note("# Too many monsters");

		/* Hack -- Pick a random monster */
		n = rand_range(1, BORG_MAX_KILL-1);

		/* Kill it */
		borg_delete_kill(n);
	}


	/* Count the monsters */
	borg_kills_cnt++;

	/* Access the monster */
	kill = &borg_kills[n];

	/* Save the race */
	kill->r_idx = r_idx;

	/* Save attr/char */
	kill->t_a = a;
	kill->t_c = c;

	/* Location */
	kill->ox = kill->x = x;
	kill->oy = kill->y = y;

	/* Update the grids */
	borg_cave_m_idx[kill->y][kill->x] = n;

	/* Timestamp */
	kill->when = borg_time;

	/* Update the monster */
	borg_update_kill_new(n);

	/* Update the monster */
	borg_update_kill_old(n);

	borg_note(format("# Kill->moves = %d", kill->moves));

	/* Note */
	borg_note(format("# Creating a monster '%s' at (%d,%d)",
	                 (r_name + r_info[kill->r_idx].name),
	                 kill->y, kill->x));

	/* Recalculate danger */
	borg_do_wipe_danger = TRUE;

	/* Forget task */
	borg_task = 0;

	/* Return the monster */
	return (n);
}



/*
 * Attempt to notice a changing "kill"
 */
static bool observe_kill_diff(int y, int x, byte a, char c)
{
	int i, r_idx;

	auto_kill *kill;

	/* Guess the race */
	r_idx = borg_guess_race(a, c, FALSE);

	/* Oops XXX XXX XXX */
	/* if (!r_idx) return (FALSE); */

	/* Create a new monster */
	i = borg_new_kill(r_idx, y, x, a, c);

	/* Get the object */
	kill = &borg_kills[i];

	/* Timestamp */
	kill->when = borg_time;

	/* Done */
	return (TRUE);
}


/*
 * Attempt to "track" a "kill" at the given location
 * Assume that the monster moved at most "d" grids.
 * If "flag" is TRUE, allow monster "conversion"
 */
static bool observe_kill_move(int y, int x, int d, byte a, char c, bool flag)
{
	int i, z, ox, oy;

	int r_idx;

	auto_kill *kill;

	monster_race *r_ptr;


	/* Look at the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Skip assigned monsters */
		if (kill->seen) continue;

		/* Old location */
		ox = kill->x;
		oy = kill->y;

		/* Calculate distance */
		z = distance(oy, ox, y, x);

		/* Verify distance */
		if (z > d) continue;

		/* Verify "reasonable" motion, if allowed */
		if (!flag && (z > (kill->moves / 10) + 1)) continue;

		/* Access the monster race */
		r_ptr = &r_info[kill->r_idx];

		/* Verify matching char */
		if (c != kill->t_c) continue;

		/* Verify matching attr */
		if (a != kill->t_a)
		{
			/* Require matching attr (for normal monsters) */
			if (!(r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR)))
			{
				/* Require flag */
				if (!flag) continue;

				/* Never flicker known monsters XXX XXX XXX */
				if (kill->known) continue;

				/* Find a multi-hued monster */
				r_idx = borg_guess_race(a, c, TRUE);

				/* Handle failure */
				if (r_idx == MAX_R_IDX - 1) continue;

				/* Note */
				borg_note(format("# Flickering monster '%s' at (%d,%d)",
				                 (r_name + r_info[r_idx].name),
				                 y, x));
		
				/* Note */
				borg_note(format("# Converting a monster '%s' at (%d,%d)",
				                 (r_name + r_info[kill->r_idx].name),
				                 kill->y, kill->x));
		
				/* Change the race */
				kill->r_idx = r_idx;
		
				/* Update the monster */
				borg_update_kill_new(i);
		
				/* Update the monster */
				borg_update_kill_old(i);
		
				/* Recalculate danger */
				borg_do_wipe_danger = TRUE;
		
				/* Forget task */
				borg_task = 0;
			}
		}

		/* Actual movement */
		if (z)
		{
			/* Assume awake */
			kill->awake = TRUE;

			/* Update the grids (carefully) */
			if (borg_cave_m_idx[kill->y][kill->x] == i)
			{
				borg_cave_m_idx[kill->y][kill->x] = 0;
			}

			/* Save the old Location */
			kill->ox = kill->x;
			kill->oy = kill->y;

			/* Save the Location */
			kill->x = x;
			kill->y = y;

			/* Update the grids */
			borg_cave_m_idx[kill->y][kill->x] = i;

			/* Note */
			borg_note(format("# Tracking a monster '%s' at (%d,%d) from (%d,%d)",
			                 (r_name + r_ptr->name),
			                 kill->y, kill->x, oy, ox));

			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;
		}

		/* Note when last seen */
		kill->when = borg_time;

		/* Update the monster */
		borg_update_kill_old(i);

		/* Mark as seen */
		kill->seen = TRUE;

		/* Done */
		return (TRUE);
	}

	/* Oops */
	return (FALSE);
}



/*
 * Calculate base danger from a spell attack by an invisible monster
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 */
static int borg_fear_spell(int i)
{
	int z = 0;
	int p = 0;

	int ouch = 0;


	/* Hack -- Maximum damage taken */
	if (xb_ptr->old_chp > b_ptr->chp)
	{
		ouch = (xb_ptr->old_chp - b_ptr->chp) * 2;
	}


	/* Check the spell */
	switch (i)
	{
		case 96+0:    /* RF4_SHRIEK */
		p += 10;
		break;

		case 96+1:    /* RF4_XXX2X4 */
		break;

		case 96+2:    /* RF4_XXX3X4 */
		break;

		case 96+3:    /* RF4_XXX4X4 */
		break;

		case 96+4:    /* RF4_ARROW_1 */
		z = (1 * 6);
		break;

		case 96+5:    /* RF4_ARROW_2 */
		z = (3 * 6);
		break;

		case 96+6:    /* RF4_ARROW_3 */
		z = (5 * 6);
		break;

		case 96+7:    /* RF4_ARROW_4 */
		z = (7 * 6);
		break;

		case 96+8:    /* RF4_BR_ACID */
		if (b_ptr->immune_acid) break;
		z = ouch;
		p += 40;
		break;

		case 96+9:    /* RF4_BR_ELEC */
		if (b_ptr->immune_elec) break;
		z = ouch;
		p += 20;
		break;

		case 96+10:    /* RF4_BR_FIRE */
		if (b_ptr->immune_fire) break;
		z = ouch;
		p += 40;
		break;

		case 96+11:    /* RF4_BR_COLD */
		if (b_ptr->immune_cold) break;
		z = ouch;
		p += 20;
		break;

		case 96+12:    /* RF4_BR_POIS */
		z = ouch;
		if (b_ptr->resist_pois) break;
		p += 20;
		break;

		case 96+13:    /* RF4_BR_NETH */
		z = ouch;
		if (b_ptr->resist_nethr) break;
		p += 50;
		if (b_ptr->hold_life) break;
		p += 150;
		break;

		case 96+14:    /* RF4_BR_LITE */
		z = ouch;
		if (b_ptr->resist_lite) break;
		if (b_ptr->resist_blind) break;
		p += 20;
		break;

		case 96+15:    /* RF4_BR_DARK */
		z = ouch;
		if (b_ptr->resist_dark) break;
		if (b_ptr->resist_blind) break;
		p += 20;
		break;

		case 96+16:    /* RF4_BR_CONF */
		z = ouch;
		if (b_ptr->resist_confu) break;
		p += 100;
		break;

		case 96+17:    /* RF4_BR_SOUN */
		z = ouch;
		if (b_ptr->resist_sound) break;
		p += 50;
		break;

		case 96+18:    /* RF4_BR_CHAO */
		z = ouch;
		if (b_ptr->resist_chaos) break;
		p += 200;
		if (b_ptr->resist_nethr) break;
		if (b_ptr->hold_life) break;
		p += 100;
		if (b_ptr->resist_confu) break;
		p += 50;
		break;

		case 96+19:    /* RF4_BR_DISE */
		z = ouch;
		if (b_ptr->resist_disen) break;
		p += 500;
		break;

		case 96+20:    /* RF4_BR_NEXU */
		z = ouch;
		if (b_ptr->resist_nexus) break;
		p += 100;
		break;

		case 96+21:    /* RF4_BR_TIME */
		z = ouch;
		p += 200;
		break;

		case 96+22:    /* RF4_BR_INER */
		z = ouch;
		p += 50;
		break;

		case 96+23:    /* RF4_BR_GRAV */
		z = ouch;
		p += 50;
		if (b_ptr->resist_sound) break;
		p += 50;
		break;

		case 96+24:    /* RF4_BR_SHAR */
		z = ouch;
		if (b_ptr->resist_shard) break;
		p += 50;
		break;

		case 96+25:    /* RF4_BR_PLAS */
		z = ouch;
		if (b_ptr->resist_sound) break;
		p += 50;
		break;

		case 96+26:    /* RF4_BR_WALL */
		z = ouch;
		if (b_ptr->resist_sound) break;
		p += 50;
		break;

		case 96+27:    /* RF4_BR_MANA */
		/* XXX XXX XXX */
		break;

		case 96+28:    /* RF4_XXX5X4 */
		break;

		case 96+29:    /* RF4_XXX6X4 */
		break;

		case 96+30:    /* RF4_XXX7X4 */
		break;

		case 96+31:    /* RF4_XXX8X4 */
		break;



		case 128+0:    /* RF5_BA_ACID */
		if (b_ptr->immune_acid) break;
		z = ouch;
		p += 40;
		break;

		case 128+1:    /* RF5_BA_ELEC */
		if (b_ptr->immune_elec) break;
		z = ouch;
		p += 20;
		break;

		case 128+2:    /* RF5_BA_FIRE */
		if (b_ptr->immune_fire) break;
		z = ouch;
		p += 40;
		break;

		case 128+3:    /* RF5_BA_COLD */
		if (b_ptr->immune_cold) break;
		z = ouch;
		p += 20;
		break;

		case 128+4:    /* RF5_BA_POIS */
		z = (12 * 2);
		if (b_ptr->resist_pois) z = (z + 2) / 3;
		if (b_ptr->resist_pois) break;
		p += 20;
		break;

		case 128+5:    /* RF5_BA_NETH */
		z = ouch;
		if (b_ptr->resist_nethr) break;
		p += 200;
		break;

		case 128+6:    /* RF5_BA_WATE */
		z = ouch;
		p += 50;
		break;

		case 128+7:    /* RF5_BA_MANA */
		z = ouch;
		break;

		case 128+8:    /* RF5_BA_DARK */
		z = ouch;
		if (b_ptr->resist_dark) break;
		if (b_ptr->resist_blind) break;
		p += 20;
		break;

		case 128+9:    /* RF5_DRAIN_MANA */
		if (b_ptr->msp) p += 10;
		break;

		case 128+10:    /* RF5_MIND_BLAST */
		z = 20;
		break;

		case 128+11:    /* RF5_BRAIN_SMASH */
		z = (12 * 15);
		p += 100;
		break;

		case 128+12:    /* RF5_CAUSE_1 */
		z = (3 * 8);
		break;

		case 128+13:    /* RF5_CAUSE_2 */
		z = (8 * 8);
		break;

		case 128+14:    /* RF5_CAUSE_3 */
		z = (10 * 15);
		break;

		case 128+15:    /* RF5_CAUSE_4 */
		z = (15 * 15);
		p += 50;
		break;

		case 128+16:    /* RF5_BO_ACID */
		if (b_ptr->immune_acid) break;
		z = ouch;
		p += 40;
		break;

		case 128+17:    /* RF5_BO_ELEC */
		if (b_ptr->immune_elec) break;
		z = ouch;
		p += 20;
		break;

		case 128+18:    /* RF5_BO_FIRE */
		if (b_ptr->immune_fire) break;
		z = ouch;
		p += 40;
		break;

		case 128+19:    /* RF5_BO_COLD */
		if (b_ptr->immune_cold) break;
		z = ouch;
		p += 20;
		break;

		case 128+20:    /* RF5_BO_POIS */
		/* XXX XXX XXX */
		break;

		case 128+21:    /* RF5_BO_NETH */
		z = ouch;
		if (b_ptr->resist_nethr) break;
		p += 200;
		break;

		case 128+22:    /* RF5_BO_WATE */
		z = ouch;
		p += 20;
		break;

		case 128+23:    /* RF5_BO_MANA */
		z = ouch;
		break;

		case 128+24:    /* RF5_BO_PLAS */
		z = ouch;
		p += 20;
		break;

		case 128+25:    /* RF5_BO_ICEE */
		z = ouch;
		p += 20;
		break;

		case 128+26:    /* RF5_MISSILE */
		z = ouch;
		break;

		case 128+27:    /* RF5_SCARE */
		p += 10;
		break;

		case 128+28:    /* RF5_BLIND */
		p += 10;
		break;

		case 128+29:    /* RF5_CONF */
		p += 10;
		break;

		case 128+30:    /* RF5_SLOW */
		p += 5;
		break;

		case 128+31:    /* RF5_HOLD */
		p += 20;
		break;



		case 160+0:    /* RF6_HASTE */
		p += 10;
		break;

		case 160+1:    /* RF6_XXX1X6 */
		break;

		case 160+2:    /* RF6_HEAL */
		p += 10;
		break;

		case 160+3:    /* RF6_XXX2X6 */
		break;

		case 160+4:    /* RF6_BLINK */
		break;

		case 160+5:    /* RF6_TPORT */
		break;

		case 160+6:    /* RF6_XXX3X6 */
		break;

		case 160+7:    /* RF6_XXX4X6 */
		break;

		case 160+8:    /* RF6_TELE_TO */
		p += 20;
		break;

		case 160+9:    /* RF6_TELE_AWAY */
		p += 10;
		break;

		case 160+10:    /* RF6_TELE_LEVEL */
		p += 50;
		break;

		case 160+11:    /* RF6_XXX5 */
		break;

		case 160+12:    /* RF6_DARKNESS */
		p += 5;
		break;

		case 160+13:    /* RF6_TRAPS */
		p += 50;
		break;

		case 160+14:    /* RF6_FORGET */
		p += 500;
		break;

		case 160+15:    /* RF6_XXX6X6 */
		break;

		case 160+16:    /* RF6_XXX7X6 */
		break;

		case 160+17:    /* RF6_XXX8X6 */
		break;

		case 160+18:    /* RF6_S_MONSTER */
		p += (b_ptr->depth) * 10;
		break;

		case 160+19:    /* RF6_S_MONSTERS */
		p += (b_ptr->depth) * 20;
		break;

		case 160+20:    /* RF6_S_ANT */
		p += (b_ptr->depth) * 20;
		break;

		case 160+21:    /* RF6_S_SPIDER */
		p += (b_ptr->depth) * 20;
		break;

		case 160+22:    /* RF6_S_HOUND */
		p += (b_ptr->depth) * 20;
		break;

		case 160+23:    /* RF6_S_HYDRA */
		p += (b_ptr->depth) * 20;
		break;

		case 160+24:    /* RF6_S_ANGEL */
		p += (b_ptr->depth) * 30;
		break;

		case 160+25:    /* RF6_S_DEMON */
		p += (b_ptr->depth) * 30;
		break;

		case 160+26:    /* RF6_S_UNDEAD */
		p += (b_ptr->depth) * 30;
		break;

		case 160+27:    /* RF6_S_DRAGON */
		p += (b_ptr->depth) * 30;
		break;

		case 160+28:    /* RF6_S_HI_UNDEAD */
		p += (b_ptr->depth) * 50;
		break;

		case 160+29:    /* RF6_S_HI_DRAGON */
		p += (b_ptr->depth) * 50;
		break;

		case 160+30:    /* RF6_S_WRAITH */
		p += (b_ptr->depth) * 50;
		break;

		case 160+31:    /* RF6_S_UNIQUE */
		p += (b_ptr->depth) * 50;
		break;
	}

	/* Notice damage */
	return (p + z);
}



/*
 * Increase the "region danger"
 */
static void borg_fear_grid(int y, int x, int k)
{
	int x0, y0, x1, x2, y1, y2;


	/* Message */
	borg_note(format("# Fearing grid (%d,%d) value %d", y, x, k));


	/* Current region */
	y0 = (y/11);
	x0 = (x/11);

	/* Nearby regions */
	y1 = (y0 > 0) ? (y0 - 1) : 0;
	x1 = (x0 > 0) ? (x0 - 1) : 0;
	y2 = (x0 < 5) ? (x0 + 1) : 5;
	x2 = (x0 < 17) ? (x0 + 1) : 17;


	/* Collect "fear", limit to 10000, spread around */
	borg_fear_region[y0][x0] = MIN(borg_fear_region[y0][x0] + k * 3, 10000);
	borg_fear_region[y0][x1] = MIN(borg_fear_region[y0][x1] + k * 2, 10000);
	borg_fear_region[y0][x2] = MIN(borg_fear_region[y0][x2] + k * 2, 10000);
	borg_fear_region[y1][x0] = MIN(borg_fear_region[y1][x0] + k * 2, 10000);
	borg_fear_region[y2][x0] = MIN(borg_fear_region[y2][x0] + k * 2, 10000);
	borg_fear_region[y1][x1] = MIN(borg_fear_region[y1][x1] + k * 1, 10000);
	borg_fear_region[y1][x2] = MIN(borg_fear_region[y1][x2] + k * 1, 10000);
	borg_fear_region[y2][x1] = MIN(borg_fear_region[y2][x1] + k * 1, 10000);
	borg_fear_region[y2][x2] = MIN(borg_fear_region[y2][x1] + k * 1, 10000);
}

/*
 * React to a change in player speed, by updating the "moves" element
 * of each monster, to reflect the fact they they will have a different
 * number of chances to move each turn, now that our speed has changed.
 */
void borg_react_speed(void)
{
	int i;

	auto_kill *kill;

	/* Iterate over each kill */
	for (i = 0; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Update this kill record */
		borg_update_kill_old(i);
	}
}


/*
 * Attempt to locate a monster which could explain a message involving
 * the given monster name, near the given location, up to the given
 * distance from the given location.
 *
 * Invisible monsters, bizarre monsters, and unexplainable monsters are
 * all treated the same way, and should eventually contribute some amount
 * of basic "fear" to the current region.
 *
 * Unexplainable monsters are actually quite different, and we return the
 * negation of their racial index to indicate that we know what they are,
 * but we do not know where they are.  This should be used to assign fear
 * based on the type of monster.  This is usually induced by "offscreen"
 * monsters, not monsters in dark hallways, by the way.  XXX XXX XXX
 *
 * First, we attempt to convert "similar" objects into the desired monster,
 * then we attempt to convert "similar" monsters into the desired monster,
 * then we attempt to match an existing monster, and finally, we give up.
 *
 * Hack -- To prevent fatal situations, every time we think there may be a
 * monster nearby, we look for a nearby object which could be the indicated
 * monster, and convert it into that monster.  This allows us to correctly
 * handle a room full of multiplying clear mushrooms.  Unfortunately, this
 * causes nasty problems when the player steps onto a pile of coins at the
 * same moment that a more distant animate pile of similar coins wakes up,
 * which causes the coins which are about to be under the player (for which
 * an appropriate "You have xxx." message will have been generated) to be
 * converted into a monster (leaving the monster itself as an object), and
 * then there will be a monster "under" the player, which can induce the
 * "infinite loop of recall" bug.  XXX XXX XXX
 *
 * When surrounded by multiple monsters of the same type, we will ascribe
 * most messages to one of those monsters, and ignore the existance of all
 * the other similar monsters.  This is bad.
 *
 * Currently, confusion may cause messages to be ignored.  XXX XXX XXX
 */
static int borg_locate_kill(cptr who, int y, int x, int r)
{
	int i, d, r_idx;

	int b_i, b_d;

	auto_take *take;
	auto_kill *kill;

	object_kind *k_ptr;

	monster_race *r_ptr;

	bool offscreen = FALSE;

	/* Handle invisible monsters */
	if (streq(who, "It") ||
	    streq(who, "Someone") ||
	    streq(who, "Something"))
	{
		/* Note */
		borg_note("# Invisible monster nearby");

		/* Ignore */
		return (0);
	}


	/* Guess the monster race */
	r_idx = borg_guess_race_name(who);

	/* Handle offscreen monsters */
	if (r_idx < 0)
	{
		/* Monster is offscreen */
		offscreen = TRUE;

		/* Repair index */
		r_idx = 0 - r_idx;
	}

	/* Access the monster race */
	r_ptr = &r_info[r_idx];

	/* Note */
	borg_note(format("# There is a monster '%s' within %d grids of %d,%d",
	                 (r_name + r_ptr->name),
	                 r, y, x));

	/* Hack -- count racial appearances */
	if (borg_race_count[r_idx] < MAX_SHORT) borg_race_count[r_idx]++;


	/* Handle trappers and lurkers and mimics */
	if (r_ptr->flags1 & (RF1_CHAR_CLEAR | RF1_CHAR_MULTI))
	{
		/* Note */
		borg_note("# Bizarre monster nearby");

		/* Ignore */
		return (-(r_idx));
	}


	/*** Hack -- Find a similar object ***/

	/* Nothing yet */
	b_i = -1; b_d = 999;

	/* Scan the objects */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		take = &borg_takes[i];

		/* Skip "dead" objects */
		if (!take->k_idx) continue;

		/* Access kind */
		k_ptr = &k_info[take->k_idx];

		/* Verify char */
		if (take->t_c != r_ptr->x_char) continue;

		/* Verify attr (unless clear or multi-hued) */
		if (!(r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR)))
		{
			/* Verify attr */
			if (take->t_a != r_ptr->x_attr) continue;
		}

		/* Calculate distance */
		d = distance(take->y, take->x, y, x);

		/* Skip "wrong" objects */
		if (d > r) continue;

		/* Skip onscreen objects if monster is offscreen */
		if (offscreen && borg_panel_contains(take->y, take->x)) continue;

		/* Track closest one */
		if (d > b_d) continue;

		/* Track it */
		b_i = i; b_d = d;
	}

	/* Found one */
	if (b_i >= 0)
	{
		byte a;
		char c;

		take = &borg_takes[b_i];

		/* Access kind */
		k_ptr = &k_info[take->k_idx];

		/* Note */
		borg_note(format("# Converting an object '%s' at (%d,%d)",
		                 (k_name + k_ptr->name),
		                 take->y, take->x));

		/* Get location */
		x = take->x;
		y = take->y;

		/* Get attr/char */
		a = take->t_a;
		c = take->t_c;

		/* Delete the object */
		borg_delete_take(b_i);

		/* Make a new monster */
		b_i = borg_new_kill(r_idx, y, x, a, c);

		/* Get the monster */
		kill = &borg_kills[b_i];

		/* Timestamp */
		kill->when = borg_time;

		/* Known identity */
		if (!r) kill->known = TRUE;

		/* Assume awake */
		kill->awake = TRUE;

		/* Return the index */
		return (b_i);
	}


	/*** Hack -- Find a similar monster ***/

	/* Nothing yet */
	b_i = -1; b_d = 999;

	/* Scan the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Skip "dead" monsters */
		if (!kill->r_idx) continue;

		/* Skip "matching" monsters */
		if (kill->r_idx == r_idx) continue;

		/* Verify char */
		if (kill->t_c != r_ptr->x_char) continue;

		/* Verify attr (unless clear or multi-hued) */
		if (!(r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR)))
		{
			/* Verify attr */
			if (kill->t_a != r_ptr->x_attr) continue;
		}

		/* Distance away */
		d = distance(kill->y, kill->x, y, x);

		/* Check distance */
		if (d > r) continue;

		/* Skip onscreen monsters if monster is offscreen */
		if (offscreen && borg_panel_contains(kill->y, kill->x)) continue;

		/* Track closest one */
		if (d > b_d) continue;

		/* Track it */
		b_i = i; b_d = d;
	}

	/* Found one */
	if (b_i >= 0)
	{
		kill = &borg_kills[b_i];

		/* Note */
		borg_note(format("# Converting a monster '%s' at (%d,%d)",
		                 (r_name + r_info[kill->r_idx].name),
		                 kill->y, kill->x));

		/* Change the race */
		kill->r_idx = r_idx;

		/* Update the monster */
		borg_update_kill_new(b_i);

		/* Update the monster */
		borg_update_kill_old(b_i);

		/* Known identity */
		if (!r) kill->known = TRUE;

		/* Wake the monster */
		kill->awake = TRUE;

		/* Recalculate danger */
		borg_do_wipe_danger = TRUE;

		/* Forget task */
		borg_task = 0;

		/* Index */
		return (b_i);
	}


	/*** Hack -- Find an existing monster ***/

	/* Nothing yet */
	b_i = -1; b_d = 999;

	/* Scan the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];

		/* Skip "dead" monsters */
		if (!kill->r_idx) continue;

		/* Skip "different" monsters */
		if (kill->r_idx != r_idx) continue;

		/* Distance away */
		d = distance(kill->y, kill->x, y, x);

		/* Check distance */
		if (d > r) continue;

		/* Skip onscreen monsters if monster is offscreen */
		if (offscreen && borg_panel_contains(kill->y, kill->x)) continue;

		/* Track closest one */
		if (d > b_d) continue;

		/* Track it */
		b_i = i; b_d = d;
	}

	/* Found one */
	if (b_i >= 0)
	{
		kill = &borg_kills[b_i];

		/* Note */
		borg_note(format("# Matched a monster '%s' at (%d,%d)",
		                 (r_name + r_info[kill->r_idx].name),
		                 kill->y, kill->x));

		/* Known identity */
		if (!r) kill->known = TRUE;

		/* Wake the monster */
		kill->awake = TRUE;

		/* Index */
		return (b_i);
	}


	/*** Oops ***/

	/* Note */
	borg_note(format("# Ignoring a monster '%s' near (%d,%d)",
	                 (r_name + r_ptr->name),
	                 y, x));

	/* Hidden monster */
	return (-(r_idx));
}




/*
 * Notice the "death" of a monster
 */
static void borg_count_death(int i)
{
	int r_idx;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr;

	/* Access race */
	r_idx = kill->r_idx;

	/* Access race entry */
	r_ptr = &r_info[r_idx];

	/* Hack -- count racial deaths */
	if (borg_race_death[r_idx] < MAX_SHORT) borg_race_death[r_idx]++;

	/* Hack -- clear unique flags if necessary */
	if (r_ptr->flags1 & RF1_UNIQUE)
	{
		borg_unique_level = FALSE;
		borg_unique_near = FALSE;
	}
}




/*
 * Handle "detection" spells and "call lite"
 *
 * Note that we must use the "old" player location
 */
static bool borg_handle_self(cptr str)
{
	int i;

	int opy, opx;

	int oqy, oqx;


	/* Old location */
	opy = xb_ptr->old_py;
	opx = xb_ptr->old_px;

	/* Extract panel */
	oqy = xb_ptr->old_wy / 11;
	oqx = xb_ptr->old_wx / 33;


	/* Handle failure */
	if (borg_failure)
	{
		borg_note("# Something failed");
	}

	/* Handle "call lite" */
	else if (prefix(str, "lite"))
	{
		int r = 2;

		/* Message */
		borg_note(format("# Called lite (%d) at (%d,%d)",
		                 r, opy, opx));

		/* Hack -- mark nearby grids as perma-lit */
		for (i = 0; i < borg_view_n; i++)
		{
			int y = GRID_Y(borg_view_g[i]);
			int x = GRID_X(borg_view_g[i]);

			/* Skip distant grids */
			if (distance(opy, opx, y, x) > r) continue;

			/* Mark as perma-lit */
			borg_cave_info[y][x] |= (CAVE_GLOW);

			/* Mark as not dark */
			borg_cave_info[y][x] &= ~(CAVE_DARK);
		}
	}

	/* Handle "detect walls" */
	else if (prefix(str, "wall"))
	{
		/* Message */
		borg_note(format("# Detected walls (%d,%d to %d,%d)",
		                 oqy, oqx, oqy+1, oqx+1));

		/* Mark detected walls */
		borg_detect_wall[oqy+0][oqx+0] = TRUE;
		borg_detect_wall[oqy+0][oqx+1] = TRUE;
		borg_detect_wall[oqy+1][oqx+0] = TRUE;
		borg_detect_wall[oqy+1][oqx+1] = TRUE;
	}

	/* Handle "detect traps" */
	else if (prefix(str, "trap"))
	{
		/* Message */
		borg_note(format("# Detected traps (%d,%d to %d,%d)",
		                 oqy, oqx, oqy+1, oqx+1));

		/* Mark detected traps */
		borg_detect_trap[oqy+0][oqx+0] = TRUE;
		borg_detect_trap[oqy+0][oqx+1] = TRUE;
		borg_detect_trap[oqy+1][oqx+0] = TRUE;
		borg_detect_trap[oqy+1][oqx+1] = TRUE;
	}

	/* Handle "detect doors" */
	else if (prefix(str, "door"))
	{
		/* Message */
		borg_note(format("# Detected doors (%d,%d to %d,%d)",
		                 oqy, oqx, oqy+1, oqx+1));

		/* Mark detected doors */
		borg_detect_door[oqy+0][oqx+0] = TRUE;
		borg_detect_door[oqy+0][oqx+1] = TRUE;
		borg_detect_door[oqy+1][oqx+0] = TRUE;
		borg_detect_door[oqy+1][oqx+1] = TRUE;
	}

	/* Handle "detect traps and doors" */
	else if (prefix(str, "both"))
	{
		/* Message */
		borg_note(format("# Detected traps and doors (%d,%d to %d,%d)",
		                 oqy, oqx, oqy+1, oqx+1));

		/* Mark detected traps */
		borg_detect_trap[oqy+0][oqx+0] = TRUE;
		borg_detect_trap[oqy+0][oqx+1] = TRUE;
		borg_detect_trap[oqy+1][oqx+0] = TRUE;
		borg_detect_trap[oqy+1][oqx+1] = TRUE;

		/* Mark detected doors */
		borg_detect_door[oqy+0][oqx+0] = TRUE;
		borg_detect_door[oqy+0][oqx+1] = TRUE;
		borg_detect_door[oqy+1][oqx+0] = TRUE;
		borg_detect_door[oqy+1][oqx+1] = TRUE;
	}

	/* Done */
	return (TRUE);
}


/*
 * Handle "weird" situations
 *
 * Note that we must use the "old" player location
 */
static bool borg_handle_weird(cptr str)
{
	int i;

	int opy, opx;


	/* Old location */
	opy = xb_ptr->old_py;
	opx = xb_ptr->old_px;

	/* Scan monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Skip non-adjacent monsters */
		if (distance(opy, opx, kill->y, kill->x) > 1) continue;

		/* Note */
		borg_note(format("# Expiring a monster '%s' (%d) at (%d,%d)",
		                 (r_name + r_info[kill->r_idx].name), kill->r_idx,
		                 kill->y, kill->x));

		/* Kill the monster */
		borg_delete_kill(i);
	}

	/* Done */
	return (TRUE);
}


/*
 * Update the Borg based on the current "map"
 */
void borg_forget_map(void)
{
	int x, y;


	/* Clean up the grids */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Forget info */
			borg_cave_info[y][x] = 0;

			/* Forget features */
			borg_cave_feat[y][x] = 0;

			/* Forget objects */
			borg_cave_o_idx[y][x] = 0;

			/* Forget monsters */
			borg_cave_m_idx[y][x] = 0;

			/* Forget searching */
			borg_cave_search[y][x] = 0;

			/* Lay down permanent walls */
			borg_cave_feat[y][x] = FEAT_PERM_SOLID;

			/* Grid blocks line of sight */
			borg_cave_info[y][x] |= (CAVE_WALL);
		}
	}

	/* Clean up the grids */
	for (y = 1; y < DUNGEON_HGT-1; y++)
	{
		for (x = 1; x < DUNGEON_WID-1; x++)
		{
			/* Forget the feature */
			borg_cave_feat[y][x] = FEAT_NONE;

			/* Grid does not block line of sight */
			borg_cave_info[y][x] &= ~(CAVE_WALL);

			/* Hack -- prepare the town XXX XXX XXX */
			if (!b_ptr->depth) borg_cave_feat[y][x] = FEAT_FLOOR;
		}
	}


	/* Reset "borg_flow_cost" */
	C_BSET(borg_flow_cost, 0xFF, DUNGEON_HGT, byte_wid);

	/* Reset "borg_flow_work" */
	C_BSET(borg_flow_work, 0xFF, DUNGEON_HGT, byte_wid);


	/* Hack -- Reset "borg_cave_danger" XXX XXX XXX */
	C_BSET(borg_cave_danger, 0xFF, DUNGEON_HGT, s16b_wid);


	/* Forget the view */
	borg_forget_view();
}



/*
 * Update the "map" based on visual info on the screen
 *
 * In general, we use the same "feat" codes as the game itself, but
 * sometimes we are just guessing (as with "visible traps"), and we
 * use some special codes, explained below.
 *
 * Note that we use the "feat" code of "FEAT_NONE" for grids which
 * have never been seen, or which, when seen, have always contained
 * an object or monster.  These grids are probably walls, unless
 * they contain a monster or object, in which case they are probably
 * floors, unless they contain a monster which passes through walls,
 * in which case they may be walls.
 *
 * Note that we use the "feat" code of "FEAT_FLOOR" for grids which
 * were a normal floor last time we checked.  These grids may have
 * changed into non-floor grids recently (via earthquake?), unless
 * the grid is on the current panel, and is currently "lit" in some
 * manner, and does not contain a monster.
 *
 * Note that we use the "feat" code of "FEAT_INVIS" for grids which
 * once contained a wall/door, but then contained a monster or object.
 * These grids are probably floors, unless the grid contains a monster
 * which can pass through walls without destroying them, in which case
 * the grid probably still contains a wall, so we need to be careful
 * not to attempt to attack that grid with missiles or spells.
 *
 * Note that we use the other "feat" codes for grids which probably
 * contain the given feature type, unless several feature types use
 * the same symbol, in which case we use some "default" code, changing
 * our guess when messages provide us with more information.  This is
 * especially necessary for distinguishing magma from quartz, and for
 * distinguishing normal doors from locked doors from jammed doors.
 * Note that most "feat" codes, even if they are not "guesses", may
 * not be valid unless the grid is on the current panel, since there
 * is no (easy) way to acquire knowledge about offscreen grids.
 *
 * We use the "CAVE_MARK" flag to mark a grid as having been "observed",
 * though this may or may not indicate that the "feature" code is known,
 * since observations of monsters or objects via telepathy and/or remote
 * detection may trigger this flag.
 *
 * Note the specialized code used to learn which floor grids are "dark"
 * and which are "perma-lit", by tracking those floor grids which appear
 * to be "lit", and then marking all of these grids which do not appear
 * to be lit by the torch as "known" to be illuminated, and by marking
 * any grids which "disappear" or which are displayed as "dark floors"
 * as "known" to be "dark".  This leaves many grids, especially those
 * lit by the torch, as being neither lit nor dark, which is weird.
 *
 * The basic problem is that, especially with no special options set,
 * the player has very little direct information about which grids
 * are perma-lit, since all non-floor grids are memorized when they
 * are seen, and torch-lit floor grids often look just like perma-lit
 * floor grids.  Also, monsters hide any feature (and objects) in their
 * grid, and objects hide any feature in their grid, and objects are
 * memorized when they are seen, and monsters can be detected by various
 * methods (including infravision and telepathy), so the features of grids
 * which contain monsters or objects are rarely directly observed.
 *
 * So we ignore most non-floor grids, and we mark any floor grids which
 * are "known" to be perma-lit as "CAVE_GLOW", and any which are "known"
 * to be dark as "CAVE_DARK".  These flags are used for many purposes,
 * most importantly, to determine when "call lite" would be useful, and
 * to help determine when a monster is standing in a viewable perma-lit
 * grid, and should thus be "visible", and to determine when the player
 * has "lite", even though his torch has gone out.
 *
 * When a "call lite" spell succeeds, we mark the grids around the
 * player as "CAVE_GLOW" and not "CAVE_DARK", but we do not attempt
 * to "spread" the lite to the entire room, since, in general, it is
 * not possible to know what grids are in the "room", if any.
 *
 * Note that we assume that normally, when the player steps onto
 * something, it disappears, and turns into a normal floor, unless
 * the player is stepping onto a grid which is normally "permanent"
 * (floors, stairs, store doors), in which case it does not change,
 * which allows us to make assumptions about the player grid, which
 * prevents painful situations such as thinking that we are standing
 * in a wall, or on a trap, etc.
 *
 * Note that when we encounter a grid which blocks motion, but which
 * was previously thought to not block motion, we must be sure to
 * remove it from any "flow" which might be in progress, to prevent
 * nasty situations in which we attempt to flow into a wall grid
 * which was thought to be something else, like an unknown grid.
 *
 * Running out of lite can induce fatal confusion. XXX XXX XXX
 *
 * Note that we assume that the total "screen" area is only 66x22
 * (which is less than 1536), so the "borg_hack" array can hold all
 * possible observable floors, and the "borg_wank" array can hold all
 * possible observable wanks.
 *
 * Note that this function makes various assumptions about the "attr/char"
 * codes of various things, including the "char" codes of several terrain
 * features, and the "attr" codes induced by the special lighting effects
 * of "map_info()".  These attr/char codes must not be redefined by the
 * user, and this is enforced by "borg_prepare()" (?).
 *
 * Since "darkness" is normally by far the most common thing on the map,
 * and because it can be handled very efficiently, we handle it first.
 *
 * Note that "darkness" and the "player" must have the default symbols.
 */
void borg_update_map(void)
{
	int dy, dx;
	int y, x;

	int g;

	byte t_a;
	char t_c;

	const byte *aa;
	const char *cc;

	byte *fast_borg_cave_info = &borg_cave_info[0][0];


	/* Track floors */
	borg_hack_n = 0;

	/* Track wanks */
	borg_wank_num = 0;

	/* Analyze the current map panel */
	for (dy = 0; dy < SCREEN_HGT; ++dy)
	{
		/* Efficient direct access XXX XXX XXX */
		aa = &(borg_term_pointer->scr->a[dy+ROW_MAP][COL_MAP]);
		cc = &(borg_term_pointer->scr->c[dy+ROW_MAP][COL_MAP]);

		/* Location */
		y = b_ptr->wy + dy;
		x = b_ptr->wx;

		/* Grid */
		g = GRID(y,x);

		/* Scan the row */
		for (dx = SCREEN_WID; dx; --dx, ++x, ++g)
		{
			byte old_info;

			byte info;

			byte feat;


			/* Access */
			t_a = *aa++;
			t_c = *cc++;


			/* Get grid info */
			info = fast_borg_cave_info[g];


			/* Darkness XXX XXX XXX */
			if (t_c == ' ')
			{
				/* Dark grid */
				info |= (CAVE_DARK);
				info &= ~(CAVE_GLOW);

				/* Save info */
				fast_borg_cave_info[g] = info;

				/* Done */
				continue;
			}


			/* Save old info */
			old_info = info;


			/* Player XXX XXX XXX */
			if (t_c == '@')
			{
				/* Save location */
				b_ptr->py = y;
				b_ptr->px = x;

				/* Hack -- white */
				t_a = TERM_WHITE;

				/* Permanent stuff */
				switch (borg_cave_feat[y][x])
				{
					case FEAT_OPEN:
					case FEAT_BROKEN:
					case FEAT_LESS:
					case FEAT_MORE:
					case FEAT_SHOP_HEAD + 0x00:
					case FEAT_SHOP_HEAD + 0x01:
					case FEAT_SHOP_HEAD + 0x02:
					case FEAT_SHOP_HEAD + 0x03:
					case FEAT_SHOP_HEAD + 0x04:
					case FEAT_SHOP_HEAD + 0x05:
					case FEAT_SHOP_HEAD + 0x06:
					case FEAT_SHOP_HEAD + 0x07:
					{
						feat = borg_cave_feat[y][x];
						break;
					}

					default:
					{
						feat = FEAT_FLOOR;
						break;
					}
				}
			}

			/* Notice "knowledge" */
			else
			{
				/* Mark as known */
				info |= (CAVE_MARK);

				/* Extract a feat */
				feat = borg_char_feat[t_c];
			}


			/* Analyze symbol */
			switch (feat)
			{
				/* Floors */
				case FEAT_FLOOR:
				{
					/* Handle "blind" */
					if (borg_base_is_blind)
					{
						/* Nothing */
					}

					/* Handle "dark" floors */
					else if (t_a == TERM_L_DARK)
					{
						/* Dark floor grid */
						info |= (CAVE_DARK);
						info &= ~(CAVE_GLOW);
					}

					/* Handle "torch-lit" floors */
					else if (t_a == TERM_YELLOW)
					{
						/* Dark floor grid */
						info |= (CAVE_DARK);
						info &= ~(CAVE_GLOW);
					}

					/* Handle "lit" floors */
					else
					{
						/* Mark as "CAVE_HACK" */
						info |= (CAVE_HACK);

						/* Collect observable floors */
						borg_hack_y[borg_hack_n] = y;
						borg_hack_x[borg_hack_n] = x;
						borg_hack_n++;
					}

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Done */
					break;
				}

				/* Runes */
				case FEAT_GLYPH:
				{
					/* Accept broken door (or open door) */
					if (borg_cave_feat[y][x] == FEAT_GLYPH) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = FEAT_GLYPH;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				{
					/* Accept broken door (or open door) */
					if (borg_cave_feat[y][x] == FEAT_BROKEN) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = FEAT_OPEN;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Done */
					break;
				}

				/* Walls XXX XXX XXX */
				case FEAT_SECRET:
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				{
					/* Accept walls */
					if (borg_cave_feat[y][x] >= FEAT_WALL_EXTRA) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Set wall flag */
					info |= (CAVE_WALL);

					/* Done */
					break;
				}

				/* Seams XXX XXX XXX */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:
				{
					/* Accept quartz (or magma) */
					if (borg_cave_feat[y][x] == FEAT_QUARTZ) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Set wall flag */
					info |= (CAVE_WALL);

					/* Done */
					break;
				}

				/* Known gold */
				case FEAT_MAGMA_K:
				case FEAT_QUARTZ_K:
				{
					/* Accept quartz (or magma) */
					if (borg_cave_feat[y][x] == FEAT_QUARTZ_K) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Set wall flag */
					info |= (CAVE_WALL);

					/* Done */
					break;
				}

				/* Rubble */
				case FEAT_RUBBLE:
				{
					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Set wall flag */
					info |= (CAVE_WALL);

					/* Done */
					break;
				}

				/* Doors XXX XXX XXX */
				case FEAT_DOOR_HEAD + 0x00:
				{
					/* Accept closed/locked/jammed doors */
					if ((borg_cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
					    (borg_cave_feat[y][x] <= FEAT_DOOR_TAIL))
					{
						break;
					}

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Set wall flag */
					info |= (CAVE_WALL);

					/* Done */
					break;
				}

				/* Traps XXX XXX XXX */
				case FEAT_TRAP_HEAD + 0x00:
				{
					/* Accept traps */
					if ((borg_cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
					    (borg_cave_feat[y][x] <= FEAT_TRAP_TAIL))
					{
						break;
					}

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Done */
					break;
				}

				/* Up stairs */
				case FEAT_LESS:
				{
					/* Accept stairs up */
					if (borg_cave_feat[y][x] == FEAT_LESS) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Track the newly discovered stairs */
					if (borg_track_less_num < BORG_MAX_LESS)
					{
						borg_track_less_x[borg_track_less_num] = x;
						borg_track_less_y[borg_track_less_num] = y;
						borg_track_less_num++;
					}

					/* Done */
					break;
				}

				/* Down stairs */
				case FEAT_MORE:
				{
					/* Accept stairs down */
					if (borg_cave_feat[y][x] == FEAT_MORE) break;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = FEAT_MORE;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Track the newly discovered stairs */
					if (borg_track_more_num < BORG_MAX_MORE)
					{
						borg_track_more_x[borg_track_more_num] = x;
						borg_track_more_y[borg_track_more_num] = y;
						borg_track_more_num++;
					}

					/* Done */
					break;
				}

				/* Store doors */
				case FEAT_SHOP_HEAD + 0x00:
				case FEAT_SHOP_HEAD + 0x01:
				case FEAT_SHOP_HEAD + 0x02:
				case FEAT_SHOP_HEAD + 0x03:
				case FEAT_SHOP_HEAD + 0x04:
				case FEAT_SHOP_HEAD + 0x05:
				case FEAT_SHOP_HEAD + 0x06:
				case FEAT_SHOP_HEAD + 0x07:
				{
					/* Shop type */
					int i = feat - FEAT_SHOP_HEAD;

					/* Accept (basic) feat */
					borg_cave_feat[y][x] = feat;

					/* Clear wall flag */
					info &= ~(CAVE_WALL);

					/* Save new information */
					borg_track_shop_y[i] = y;
					borg_track_shop_x[i] = x;

					/* Done */
					break;
				}

				/* Monsters/Objects */
				default:
				{
					auto_wank *wank;

					/* Handle old walls */
					if (info & (CAVE_WALL))
					{
						/* Assume probable floor */
						borg_cave_feat[y][x] = FEAT_INVIS;

						/* Clear wall flag */
						info &= ~(CAVE_WALL);
					}

					/* Collect observable wanks */
					wank = &borg_wanks[borg_wank_num++];

					/* Save some information */
					wank->y = y;
					wank->x = x;
					wank->t_a = t_a;
					wank->t_c = t_c;
					wank->is_take = borg_char_is_take[(byte)(t_c)];
					wank->is_kill = borg_char_is_kill[(byte)(t_c)];

					/* Done */
					break;
				}
			}


			/* Info has changed */
			if (old_info != info)
			{
				/* The "CAVE_WALL" flag has changed */
				if ((info & (CAVE_WALL)) != (old_info & (CAVE_WALL)))
				{
					/* Remove from current flow */
					borg_flow_cost[y][x] = 255;

					/* Hack -- Forget danger */
					borg_cave_danger[y][x] = -1;

					/* Recalculate the view */
					borg_do_update_view = TRUE;
				}

				/* Save grid info */
				fast_borg_cave_info[g] = info;
			}
		}
	}
}



/*
 * Look at the screen and update the borg
 *
 * Uses the "panel" info (b_ptr->wx, b_ptr->wy) obtained earlier
 *
 * Note that all the "important" messages that occured after our last
 * action have been "queued" in a usable form.  We must attempt to use
 * these messages to update our knowledge about the world, keeping in
 * mind that the world may have changed in drastic ways.
 *
 * Note that "borg_time" corresponds *roughly* to player turns, except that
 * resting and "repeated" commands count as a single turn, and "free" moves
 * (including "illegal" moves, such as moving into a wall, or tunneling into
 * thin air) are counted as turns.
 *
 * Also note that "borg_time" is not incremented until the Borg is about to
 * do something, so nothing ever has a time-stamp of the "current" time.
 *
 * The basic problem with timestamping the monsters and objects is that
 * we often get a message about a monster, and so we want to timestamp it,
 * but then we cannot use the timestamp to indicate that the monster has
 * not been "checked" yet.  Perhaps we need to do something like give each
 * monster a "moved" flag, and clear the flags to FALSE each turn before
 * tracking.  XXX XXX XXX
 *
 * We rely on the fact that all "perma-lit" grids are memorized when they
 * are seen, so any grid on the current panel that appears "dark" must not
 * be perma-lit.  This information is used by "borg_update_view()".
 *
 * Note that when two monsters of the same race are standing next to
 * each other, and they both move, such that the second monster ends
 * up where the first one began, we will incorrectly assume that the
 * first monster stayed still, and either the second monster moved
 * two spaces, or the second monster disappeared and a third monster
 * appeared, which is technically possible, if the first monster ate
 * the second, and then cloned the third.
 */
void borg_update(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, k, x, y;

	cptr msg;

	cptr what;

	bool reset = FALSE;


	/*** Process objects/monsters ***/

	/* Scan monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Clear flags */
		kill->seen = FALSE;
		kill->used = FALSE;

		/* Skip recently seen monsters */
		if (borg_time - kill->when < 2000) continue;

		/* Note */
		borg_note(format("# Expiring a monster '%s' (%d) at (%d,%d)",
		                 (r_name + r_info[kill->r_idx].name), kill->r_idx,
		                 kill->y, kill->x));

		/* Kill the monster */
		borg_delete_kill(i);
	}

	/* Scan objects */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		auto_take *take = &borg_takes[i];

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Clear flags */
		take->seen = FALSE;

		/* Skip recently seen objects */
		if (borg_time - take->when < 2000) continue;

		/* Note */
		borg_note(format("# Expiring an object '%s' (%d) at (%d,%d)",
		                 (k_name + k_info[take->k_idx].name), take->k_idx,
		                 take->y, take->x));

		/* Kill the object */
		borg_delete_take(i);
	}


	/*** Handle messages ***/

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Note the message */
		borg_note(format("# %s (+)", msg));
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Hack -- Handle "SELF" info */
		if (prefix(msg, "SELF:"))
		{
			(void)borg_handle_self(what);
			borg_msg_use[i] = 1;
		}

		/* Hack -- Handle "WEIRD" info */
		else if (prefix(msg, "WEIRD:"))
		{
			(void)borg_handle_weird(what);
			borg_msg_use[i] = 1;
		}

		/* Handle "You feel..." */
		else if (prefix(msg, "FEELING:"))
		{
			borg_feeling = atoi(what);
			borg_msg_use[i] = 1;
		}
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "You hit xxx." */
		if (prefix(msg, "HIT:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "You miss xxx." */
		else if (prefix(msg, "MISS:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "You have killed xxx." */
		else if (prefix(msg, "KILL:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 0)) > 0)
			{
				borg_count_death(k);
				borg_delete_kill(k);
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "xxx dies." */
		else if (prefix(msg, "DIED:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 3)) > 0)
			{
				borg_count_death(k);
				borg_delete_kill(k);
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "xxx screams in pain." */
		else if (prefix(msg, "PAIN:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 3)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__BOLD:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 0)) > 0)
			{
				borg_msg_use[i] = 2;
			}
		}
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "You hit xxx." */
		if (prefix(msg, "HIT:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 1)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "You miss xxx." */
		else if (prefix(msg, "MISS:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 1)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "You have killed xxx." */
		else if (prefix(msg, "KILL:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->gy, xb_ptr->gx, 1)) > 0)
			{
				borg_count_death(k);
				borg_delete_kill(k);
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "xxx dies." */
		else if (prefix(msg, "DIED:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_count_death(k);
				borg_delete_kill(k);
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "xxx screams in pain." */
		else if (prefix(msg, "PAIN:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "xxx hits you." */
		else if (prefix(msg, "HIT_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 1)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "xxx misses you." */
		else if (prefix(msg, "MISS_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 1)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE_SLEEP:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "awake" */
		else if (prefix(msg, "STATE_AWAKE:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__BOLD:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}

		/* Hack -- Handle "spell" */
		else if (prefix(msg, "SPELL_"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, xb_ptr->old_py, xb_ptr->old_px, 20)) > 0)
			{
				borg_msg_use[i] = 3;
			}
		}
	}


	/*** Handle new levels ***/

	/* Hack -- note new levels */
	if (xb_ptr->old_depth != b_ptr->depth)
	{
		/* Hack -- Restart the clock */
		borg_time = 1000;

		/* When level was begun */
		borg_when_began = borg_time;

		/* Reset danger avoidance */
		borg_avoid = 0;

		/* Reset boosted avoidance */
		borg_boost = 0;

		/* Wipe the danger */
		borg_do_wipe_danger = TRUE;

		/* Update some stuff */
		borg_do_update_view = TRUE;

		/* Examine self */
		borg_do_inven = TRUE;
		borg_do_equip = TRUE;
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		/* Examine the world */
		borg_do_panel = TRUE;
		borg_do_frame = TRUE;

		/* Enable some functions */
		borg_do_crush_junk = TRUE;
		borg_do_crush_hole = TRUE;
		borg_do_crush_slow = TRUE;

		/* Mega-Hack -- Clear "call lite" stamp */
		borg_when_call_lite = 0;

		/* Mega-Hack -- Clear "wizard lite" stamp */
		borg_when_wizard_lite = 0;

		/* Mega-Hack -- Clear "detect traps" stamp */
		borg_when_detect_traps = 0;

		/* Mega-Hack -- Clear "detect doors" stamp */
		borg_when_detect_doors = 0;

		/* Mega-Hack -- Clear "detect walls" stamp */
		borg_when_detect_walls = 0;

		/* Hack -- Clear "panel" flags */
		for (y = 0; y < 6; y++)
		{
			for (x = 0; x < 6; x++)
			{
				borg_detect_wall[y][x] = FALSE;
				borg_detect_trap[y][x] = FALSE;
				borg_detect_door[y][x] = FALSE;
			}
		}

		/* Hack -- Clear "fear" */
		for (y = 0; y < 6; y++)
		{
			for (x = 0; x < 18; x++)
			{
				borg_fear_region[y][x] = 0;
			}
		}

		/* Hack -- Clear "shop visit" stamps */
		for (i = 0; i < BORG_MAX_SHOP; i++) borg_shops[i].when = 0;

		/* Forget task */
		borg_task = 0;

		/* Hack -- Clear "shop" goals */
		borg_goal_shop = borg_goal_ware = borg_goal_item = -1;

		/* Do not use any stairs */
		borg_stair_less = borg_stair_more = FALSE;

		/* Hack -- cannot rise past town */
		if (!b_ptr->depth) borg_rising = FALSE;

		/* Assume not bored yet */
		borg_completed = FALSE;

		/* Assume not leaving the level */
		borg_leaving = FALSE;

		/* Assume not fleeing the level */
		borg_fleeing = FALSE;

		/* Assume not ignoring monsters */
		borg_ignoring = FALSE;

		/* No unique around */
		borg_unique_near = FALSE;
		borg_unique_level = FALSE;

		/* No known stairs */
		borg_track_less_num = 0;
		borg_track_more_num = 0;

		/* No objects here */
		borg_takes_cnt = 0;
		borg_takes_nxt = 1;

		/* Forget old objects */
		C_WIPE(borg_takes, BORG_MAX_TAKE, auto_take);

		/* No bad location */
		borg_bad_location_y = -1;
		borg_bad_location_x = -1;

		/* No monsters here */
		borg_kills_cnt = 0;
		borg_kills_nxt = 1;

		/* Forget old monsters */
		C_WIPE(borg_kills, BORG_MAX_KILL, auto_kill);

		/* Hack -- Forget race counters */
		C_WIPE(borg_race_count, MAX_R_IDX, s16b);

		/* Forget the map */
		borg_forget_map();

		/* Reset */
		reset = TRUE;

		/* Save new depth */
		xb_ptr->old_depth = b_ptr->depth;

		/* Mention the new depth */
		borg_note(format("# Starting depth %d", b_ptr->depth));

		/* Save game if requested */
		if (borg_flag_save_depth) borg_need_save = TRUE;
	}

	/* Handle old level */
	else
	{
		/* Reduce fear over time */
		if (!(borg_time % 10))
		{
			for (y = 0; y < 6; y++)
			{
				for (x = 0; x < 18; x++)
				{
					if (borg_fear_region[y][x]) borg_fear_region[y][x]--;
				}
			}
		}
	}


	/*** Update the map ***/

	/* Update the map */
	borg_update_map();

	/* XXX XXX XXX */
	py = b_ptr->py;
	px = b_ptr->px;

	/* Reset */
	if (reset)
	{
		/* Fake old panel */
		xb_ptr->old_wx = b_ptr->wx;
		xb_ptr->old_wy = b_ptr->wy;

		/* Fake old location */
		xb_ptr->old_px = px;
		xb_ptr->old_py = py;

		/* Fake goal location */
		xb_ptr->gx = px;
		xb_ptr->gy = py;
	}

	/* Player moved */
	if ((xb_ptr->old_px != px) || (xb_ptr->old_py != py))
	{
		/* Update view */
		borg_do_update_view = TRUE;
	}

	/* Player grid cannot be "CAVE_HACK" */
	borg_cave_info[py][px] &= ~(CAVE_HACK);

	/* Update the view */
	if (borg_do_update_view)
	{
		/* Update the view */
		borg_update_view();

		/* Take note */
		borg_do_update_view = FALSE;
	}

	/* Hack -- Clear "CAVE_HACK" flags */
	for (i = 0; i < borg_hack_n; i++)
	{
		/* Get location */
		x = borg_hack_x[i];
		y = borg_hack_y[i];

		/* Clear "CAVE_HACK" flags */
		borg_cave_info[y][x] &= ~(CAVE_HACK);
	}

	/* Hack -- Reset "borg_hack_n" */
	borg_hack_n = 0;


	/*** Track objects and monsters ***/

	/* Pass 1 -- stationary monsters */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track stationary monsters */
		if (wank->is_kill &&
		    observe_kill_move(wank->y, wank->x, 0, wank->t_a, wank->t_c, FALSE))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 2 -- stationary objects */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track stationary objects */
		if (wank->is_take &&
		    observe_take_move(wank->y, wank->x, 0, wank->t_a, wank->t_c))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 3a -- moving monsters (distance 1) */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track moving monsters */
		if (wank->is_kill &&
		    observe_kill_move(wank->y, wank->x, 1, wank->t_a, wank->t_c, FALSE))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 3b -- moving monsters (distance 2) */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track moving monsters */
		if (wank->is_kill &&
		    observe_kill_move(wank->y, wank->x, 2, wank->t_a, wank->t_c, FALSE))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 3c -- moving monsters (distance 3) */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track moving monsters */
		if (wank->is_kill &&
		    observe_kill_move(wank->y, wank->x, 3, wank->t_a, wank->t_c, FALSE))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 3d -- moving monsters (distance 3, allow changes) */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track moving monsters */
		if (wank->is_kill &&
		    observe_kill_move(wank->y, wank->x, 3, wank->t_a, wank->t_c, TRUE))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 4 -- new objects */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track new objects */
		if (wank->is_take &&
		    observe_take_diff(wank->y, wank->x, wank->t_a, wank->t_c))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 5 -- new monsters */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Track new monsters */
		if (wank->is_kill &&
		    observe_kill_diff(wank->y, wank->x, wank->t_a, wank->t_c))
		{
			/* Hack -- excise the entry */
			borg_wanks[i] = borg_wanks[--borg_wank_num];
		}
	}

	/* Pass 6 -- oops */
	for (i = borg_wank_num - 1; i >= 0; i--)
	{
		auto_wank *wank = &borg_wanks[i];

		/* Note the left-over wanks */
		borg_note(format("? Unidentified wank at %d,%d (%c)",
		                 wank->x, wank->y, wank->t_c));
	}


	/*** Handle messages ***/

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "xxx dies." */
		if (prefix(msg, "DIED:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_count_death(k);
				borg_delete_kill(k);
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "xxx screams in pain." */
		else if (prefix(msg, "PAIN:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "xxx hits you." */
		else if (prefix(msg, "HIT_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 1)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "xxx misses you." */
		else if (prefix(msg, "MISS_BY:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 1)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE_SLEEP:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "awake" */
		else if (prefix(msg, "STATE_AWAKE:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__FEAR:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Handle "sleep" */
		else if (prefix(msg, "STATE__BOLD:"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}

		/* Hack -- Handle "spell" */
		else if (prefix(msg, "SPELL_"))
		{
			/* Attempt to find the monster */
			if ((k = borg_locate_kill(what, py, px, 20)) > 0)
			{
				borg_msg_use[i] = 4;
			}
		}
	}

	/* Hack */
	if (borg_msg_num)
	{
		borg_note(format("# Current location (%d,%d)", py, px));
	}

	/* Process messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Skip parsed messages */
		if (borg_msg_use[i]) continue;

		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Get the arguments */
		what = strchr(msg, ':') + 1;

		/* Handle "xxx hits you." */
		if (prefix(msg, "HIT_BY:"))
		{
			borg_fear_grid(py, px, 4 * ((b_ptr->depth / 5) + 1));
			borg_msg_use[i] = 5;
		}

		/* Handle "xxx misses you." */
		else if (prefix(msg, "MISS_BY:"))
		{
			borg_fear_grid(py, px, 2 * ((b_ptr->depth / 5) + 1));
			borg_msg_use[i] = 5;
		}

		/* Hack -- Handle "spell" */
		else if (prefix(msg, "SPELL_"))
		{
			borg_fear_grid(py, px, borg_fear_spell(atoi(msg+6)));
			borg_msg_use[i] = 5;
		}
	}

	/* Display messages */
	for (i = 0; i < borg_msg_num; i++)
	{
		/* Get the message */
		msg = borg_msg_buf + borg_msg_pos[i];

		/* Final message */
		borg_note(format("# %s (%d)", msg, borg_msg_use[i]));
	}


	/*** Notice missing monsters ***/

	/* Scan the monster list */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Skip seen monsters */
		if (kill->when == borg_time) continue;

		/* Hack -- blind or hallucinating */
		if (borg_base_is_blind || borg_base_is_image) continue;

		/* Predict the monster */
		borg_follow_kill(i);
	}


	/*** Notice missing objects ***/

	/* Scan the object list */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		auto_take *take = &borg_takes[i];

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Skip seen objects */
		if (take->when == borg_time) continue;

		/* Hack -- blind or hallucinating */
		if (borg_base_is_blind || borg_base_is_image) continue;

		/* Follow the object */
		borg_follow_take(i);
	}


	/*** Notice broken monsters ***/

	/* Player grid cannot contain monsters */
	borg_cave_m_idx[py][px] = 0;

	/* Scan the monster list */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Skip seen monsters */
		if (borg_cave_m_idx[kill->y][kill->x] != i)
		{
			/* Warning */
			borg_note(format("# Broken monster '%s' at (%d,%d)",
	                 		(r_name + r_info[kill->r_idx].name),
	                 		kill->y, kill->x));

			/* Delete it */
			borg_delete_kill(i);
		}
	}


	/*** Notice broken objects ***/

	/* Player grid cannot contain objects */
	borg_cave_o_idx[py][px] = 0;

	/* Scan the object list */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		auto_take *take = &borg_takes[i];

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Skip seen monsters */
		if (borg_cave_o_idx[take->y][take->x] != i)
		{
			/* Warning */
			borg_note(format("# Broken object '%s' at (%d,%d)",
	                 		(k_name + k_info[take->k_idx].name),
	                 		take->y, take->x));

			/* Delete it */
			borg_delete_take(i);
		}
	}


	/*** Various things ***/

	/* Forget goals while "impaired" in any way */
	if (borg_base_is_blind || borg_base_is_confused ||
	    borg_base_is_afraid || borg_base_is_image)
	{
		/* Forget goals */
		borg_task = 0;
	}

	/* Forget goals while "bleeding" in any way */
	if (borg_base_is_weak || borg_base_is_poisoned ||
	    borg_base_is_cut || borg_base_is_stun)
	{
		/* Forget goals */
		borg_task = 0;
	}

	/* Handle changing spell points */
	if (b_ptr->csp != xb_ptr->old_csp)
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		/* Forget goals */
		borg_task = 0;
	}

	/* Handle changing hitpoints */
	if (b_ptr->chp != xb_ptr->old_chp)
	{
		/* Forget goals */
		borg_task = 0;
	}
	
	/* Save the hit points */
	xb_ptr->old_chp = b_ptr->chp;

	/* Save the spell points */
	xb_ptr->old_csp = b_ptr->csp;

	/* Forget failure */
	borg_failure = FALSE;

	/* Forget the messages */
	borg_msg_len = 0;
	borg_msg_num = 0;


	/*** Save old info ***/

	/* Save the old "location" */
	xb_ptr->old_py = py;
	xb_ptr->old_px = px;

	/* Save the old "panel" */
	xb_ptr->old_wy = b_ptr->wy;
	xb_ptr->old_wx = b_ptr->wx;


	/*** Defaults ***/

	/* Default "goal" location */
	xb_ptr->gy = py;
	xb_ptr->gx = px;
}


/*
 * Handle various "important" messages
 *
 * Actually, we simply "queue" them for later analysis
 */
void borg_react(cptr msg, cptr buf)
{
	int len;

	/* Note actual message */
	borg_note(format("> %s", msg));

	/* Extract length of parsed message */
	len = strlen(buf);

	/* Verify space */
	if (borg_msg_num + 1 > borg_msg_max)
	{
		borg_oops("too many messages");
		return;
	}

	/* Verify space */
	if (borg_msg_len + len + 1 > borg_msg_siz)
	{
		borg_oops("too much messages");
		return;
	}

	/* Assume not used yet */
	borg_msg_use[borg_msg_num] = 0;

	/* Save the message position */
	borg_msg_pos[borg_msg_num] = borg_msg_len;

	/* Save the message text */
	strcpy(borg_msg_buf + borg_msg_len, buf);

	/* Advance the buf */
	borg_msg_len += len + 1;

	/* Advance the pos */
	borg_msg_num++;
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

