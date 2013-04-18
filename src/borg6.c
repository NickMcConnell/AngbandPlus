/* File: borg6.c */

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
 * This file is responsible for the low level dungeon goals.
 *
 * This includes calculating the danger from monsters, determining
 * how and when to attack monsters, and calculating "flow" paths
 * from place to place for various reasons.
 *
 * Notes:
 *   We assume that invisible/offscreen monsters are dangerous
 *   We consider physical attacks, missile attacks, spell attacks,
 *     wand attacks, etc, as variations on a single theme.
 *   We take account of monster resistances and susceptibilities
 *   We try not to wake up sleeping monsters by throwing things
 *
 * To Do:
 *   Consider various "special" attacks like sleep and slow
 *
 * Bugs:
 *   We use missiles and bolt spells against ghosts in walls
 *   Currently the "twitchy()" function is not very smart
 *   We get "twitchy" when we are afraid of the monsters
 *   Annoyance and Danger are very different things (!)
 *
 * The artifact activations (only some are currently used):
 *
 *   ART_GALADRIEL    simple -- call light -- GF_LITE (3,2d15)
 *   ART_ELENDIL      simple -- mapping
 *   ART_THRAIN       simple -- wiz-lite, detect traps/doors/stairs
 *   ART_CARLAMMAS    unused -- protection from evil
 *   ART_INGWE        unused -- dispel evil (PLEV*5)
 *   ART_TULKAS       unused -- high speed
 *   ART_NARYA        attack -- GF_FIRE (3,120)
 *   ART_NENYA        attack -- GF_COLD (3,200)
 *   ART_VILYA        attack -- GF_ELEC (3,250)
 *   ART_POWER        unused -- ring of power
 *   ART_RAZORBACK    unused -- star ball -- GF_ELEC (3,150)
 *   ART_BLADETURNER  unused -- resist acid/elec/firs/cold/pois
 *               and  unused -- cure 30 hp and fear
 *               and  unused -- blessed and beserk strength
 *   ART_SOULKEEPER   unused -- cure 1000 hp and cuts
 *   ART_BELEGENNON   unused -- phase door
 *   ART_CELEBORN     unused -- genocide
 *   ART_CASPANION    unused -- destroy doors touch
 *   ART_HOLHENNETH   unused -- detection
 *   ART_GONDOR       unused -- cure 500 hp and cuts
 *   ART_COLLUIN      unused -- resist acid/elec/fire/cold/pois
 *   ART_HOLCOLLETH   unused -- sleep monsters touch
 *   ART_THINGOL      unused -- recharge
 *   ART_COLANNON     unused -- teleport
 *   ART_LUTHIEN      unused -- restore life levels
 *   ART_CAMMITHRIM   attack -- GF_MISSILE (1,2d6)
 *   ART_PAURHACH     attack -- GF_FIRE (1,9d8)
 *   ART_PAURNIMMEN   attack -- GF_COLD (1,6d8)
 *   ART_PAURAEGEN    attack -- GF_ELEC (1,4d8)
 *   ART_PAURNEN      attack -- GF_ACID (1,5d8)
 *   ART_FINGOLFIN    attack -- GF_ARROW (1,150)
 *   ART_FEANOR       unused -- speed
 *   ART_DAL          unused -- cure fear and poison
 *   ART_NARTHANC     attack -- GF_FIRE (1,9d8)
 *   ART_NIMTHANC     attack -- GF_COLD (1,6d8)
 *   ART_DETHANC      attack -- GF_ELEC (1,4d8)
 *   ART_RILIA        attack -- GF_POIS (3,12)
 *   ART_BELANGIL     attack -- GF_COLD (2,48)
 *   ART_ARUNRUTH     attack -- GF_COLD (1,12d8)
 *   ART_RINGIL       attack -- GF_COLD (2,100)
 *   ART_ANDURIL      attack -- GF_FIRE (2,72)
 *   ART_THEODEN      attack -- GF_OLD_DRAIN (1,120)
 *   ART_AEGLOS       attack -- GF_COLD (2,100)
 *   ART_OROME        unused -- stone to mud
 *   ART_EONWE        unused -- mass genocide
 *   ART_LOTHARANG    unused -- cure some wounds
 *   ART_ULMO         unused -- teleport away (-1,?)
 *   ART_AVAVIR       unused -- word of recall
 *   ART_TOTILA       unused -- confuse monster (1,?)
 *   ART_FIRESTAR     attack -- GF_FIRE (3,72)
 *   ART_TARATOL      unused -- speed
 *   ART_ERIRIL       simple -- identify
 *   ART_OLORIN       unused -- probing
 *   ART_TURMIL       attack -- GF_OLD_DRAIN (1,90)
 *   ART_CUBRAGOL     unused -- brand bolts
 */



/*
 * Given a "source" and "target" locations, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
static int borg_extract_dir(int y1, int x1, int y2, int x2)
{
	/* No movement required */
	if ((y1 == y2) && (x1 == x2)) return (5);

	/* South or North */
	if (x1 == x2) return ((y1 < y2) ? 2 : 8);

	/* East or West */
	if (y1 == y2) return ((x1 < x2) ? 6 : 4);

	/* South-east or South-west */
	if (y1 < y2) return ((x1 < x2) ? 3 : 1);

	/* North-east or North-west */
	if (y1 > y2) return ((x1 < x2) ? 9 : 7);

	/* Paranoia */
	return (5);
}


/*
 * Given a "source" and "target" locations, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * We prefer "non-diagonal" motion, which allows us to save the
 * "diagonal" moves for avoiding pillars and other obstacles.
 *
 * If no "obvious" path is available, we use "borg_extract_dir()".
 *
 * We return "5" if no motion is needed.
 */
static int borg_goto_dir(int y1, int x1, int y2, int x2)
{
	int d, e;

	int ay = (y2 > y1) ? (y2 - y1) : (y1 - y2);
	int ax = (x2 > x1) ? (x2 - x1) : (x1 - x2);


	/* Default direction */
	e = borg_extract_dir(y1, x1, y2, x2);


	/* Adjacent location, use default */
	if ((ay <= 1) && (ax <= 1)) return (e);


	/* Try south/north (primary) */
	if (ay > ax)
	{
		d = (y1 < y2) ? 2 : 8;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);
	}

	/* Try east/west (primary) */
	if (ay < ax)
	{
		d = (x1 < x2) ? 6 : 4;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);
	}


	/* Try diagonal */
	d = borg_extract_dir(y1, x1, y2, x2);

	/* Check for walls */
	if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);


	/* Try south/north (secondary) */
	if (ay <= ax)
	{
		d = (y1 < y2) ? 2 : 8;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);
	}

	/* Try east/west (secondary) */
	if (ay >= ax)
	{
		d = (x1 < x2) ? 6 : 4;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);
	}


	/* Circle obstacles */
	if (!ay)
	{
		/* Circle to the south */
		d = (x1 < x2) ? 3 : 1;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);

		/* Circle to the north */
		d = (x1 < x2) ? 9 : 7;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);
	}

	/* Circle obstacles */
	if (!ax)
	{
		/* Circle to the east */
		d = (y1 < y2) ? 3 : 9;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);

		/* Circle to the west */
		d = (y1 < y2) ? 1 : 7;
		if (borg_cave_floor_bold(y1 + ddy[d], x1 + ddx[d])) return (d);
	}


	/* Oops */
	return (e);
}



/*
 * Clear the "flow" information
 *
 * This function must be called before computing any flow.  Note that
 * the initial insertion of grids into a flow is considered to be part
 * of the flow computation, so most routines first collect destination
 * grids, and then perform the entire flow computation.
 *
 * This function was once a major bottleneck, so we now use several
 * slightly bizarre, but highly optimized, memory fill methods.
 *
 * Note that we only reset the "borg_cave_danger" array when we have
 * been told that our cached values may be incorrect.  Normally, most
 * of them are still correct, but it is probably faster just to reset
 * the entire array.
 */
static void borg_flow_clear(void)
{
	/* Reset the working flow costs */
	C_BSET(borg_flow_work, 0xFF, DUNGEON_HGT, byte_wid);

	/* Reset the cached danger */
	if (borg_do_wipe_danger)
	{
		/* Hack -- Reset the cached danger XXX XXX XXX */
		C_BSET(borg_cave_danger, 0xFF, DUNGEON_HGT, s16b_wid);

		/* Reset the forget flag */
		borg_do_wipe_danger = FALSE;
	}

	/* Start over */
	borg_flow_head = 0;
	borg_flow_tail = 0;
}




/*
 * Spread a "flow" from the "destination" grids outwards
 *
 * We fill in the "cost" field of every grid that the player can
 * "reach" with the number of steps needed to reach that grid,
 * if the grid is "reachable", and otherwise, with "255", which
 * is the largest possible value that can be stored in a byte.
 *
 * Thus, certain grids which are actually "reachable" but only by
 * a path which is at least 255 steps in length will thus appear
 * to be "unreachable", but this is not a major concern.
 *
 * We use the "flow" array as a "circular queue", and thus we must
 * be careful not to allow the "queue" to "overflow".  This could
 * only happen with a large number of distinct destination points,
 * each several units away from every other destination point, and
 * in a dungeon with no walls and no dangerous monsters.  But this
 * is technically possible, so we must check for it just in case.
 *
 * We do not need a "priority queue" because the cost from grid to
 * grid is always "one" and we process them in order.  If we did
 * use a priority queue, this function might become unusably slow.
 *
 * We refuse to let any flow pass through "wall" grids.  We allow
 * it to pass through "door" grids, because doors can be opened.
 * We treat rubble as a wall, and attempt to remove it elsewhere,
 * but we never attempt to "flow" through it.  Note that the outer
 * edge of the dungeon is always known to contain walls, and so we
 * never attempt to leave the dungeon with a flow.  Note that this
 * assumes that nobody ever gives a grid in the outer dungeon wall
 * as a flow destination.
 *
 * We refuse to let any flow pass through "dangerous" grids, where
 * "dangerous" is defined relative to half the current "borg_avoid"
 * value, which is normally equal to the current hit-points.  The
 * flow routines maintain the special "borg_cave_danger" array as a
 * way to "cache" the danger of grids as determined during the flow
 * calculations.  This cached danger is cleared to "unknown" when
 * a flow is begun and the "borg_do_wipe_danger" flag is set, which
 * is only done when something happens to disturb the global danger
 * caching information, such as a monster changing state or location.
 * For efficiency, we assume twos complement integer encoding, so we
 * can set both bytes of an "s16b" to "0xFF" instead of setting the
 * "s16b" directly to "-1".  XXX XXX XXX
 *
 * Note that the "borg_enqueue_grid()" function should refuse to
 * enqueue "dangeous" destination grids, but does not need to set
 * the "KNOW" or "ICKY" flags, since having a "cost" field of zero
 * means that these grids will never be queued again.  In fact,
 * the "borg_enqueue_grid()" function can be used to enqueue grids
 * which are "walls", such as "doors" or "rubble".
 *
 * This function is extremely expensive, and is a major bottleneck
 * in the code, due more to internal processing than to the use of
 * the "borg_danger()" function, especially now that the use of the
 * "borg_danger()" function has been optimized several times.
 *
 * The "optimize" flag allows this function to stop as soon as it
 * finds any path which reaches the player, since in general we are
 * looking for paths to destination grids which the player can take,
 * and we can stop this function as soon as we find any usable path,
 * since it will always be as short a path as possible.
 *
 * We queue the "children" in reverse order, to allow any "diagonal"
 * neighbors to be processed first, since this may boost efficiency.
 *
 * Note that we should recalculate "danger", and reset all "flows"
 * if we notice that a wall has disappeared, and if one appears, we
 * must give it a maximal cost, and mark it as "icky", in case it
 * was currently included in any flow.  XXX XXX XXX
 *
 * If a "depth" is given, then the flow will only be spread to that
 * depth, note that the maximum legal value of "depth" is 250.
 */
static void borg_flow_spread(int depth, bool optimize, bool avoid)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i;
	int n, o = 0;
	int x1, y1;
	int x, y;


	/* Now process the queue */
	while (borg_flow_head != borg_flow_tail)
	{
		/* Extract the next entry */
		x1 = borg_flow_x[borg_flow_tail];
		y1 = borg_flow_y[borg_flow_tail];

		/* Circular queue -- dequeue the next entry */
		if (++borg_flow_tail == BORG_FLOW_MAX) borg_flow_tail = 0;


		/* Working cost (one per movement grid) */
		n = borg_flow_work[y1][x1] + 1;

		/* New depth */
		if (n > o)
		{
			/* Optimize (if requested) */
			if (optimize && (n > borg_flow_work[py][px])) break;

			/* Limit depth */
			if (n > depth) break;

			/* Save */
			o = n;
		}

		/* Queue the "children" */
		for (i = 0; i < 8; i++)
		{
			int old_head;

			/* Neighbor grid */
			x = x1 + ddx_ddd[i];
			y = y1 + ddy_ddd[i];


			/* Skip "reached" grids */
			if (borg_flow_work[y][x] <= n) continue;


			/* Avoid "wall" grids (not doors) */
			if (borg_cave_feat[y][x] >= FEAT_SECRET) continue;


			/* Avoid unknown grids (if requested) */
			if (avoid && (borg_cave_feat[y][x] == FEAT_NONE)) continue;


			/* Analyze every grid once */
			if (borg_cave_danger[y][x] < 0)
			{
				/* Get the danger */
				borg_cave_danger[y][x] = borg_danger(y, x, 1);
			}

			/* Ignore "dangerous" grids */
			if (borg_cave_danger[y][x] > borg_avoid / 2) continue;


			/* Save the working flow cost */
			borg_flow_work[y][x] = n;

			/* Enqueue that entry */
			borg_flow_x[borg_flow_head] = x;
			borg_flow_y[borg_flow_head] = y;


			/* Circular queue -- memorize head */
			old_head = borg_flow_head;

			/* Circular queue -- insert with wrap */
			if (++borg_flow_head == BORG_FLOW_MAX) borg_flow_head = 0;

			/* Circular queue -- handle overflow (badly) */
			if (borg_flow_head == borg_flow_tail) borg_flow_head = old_head;
		}
	}

	/* Forget the flow info */
	borg_flow_head = borg_flow_tail = 0;
}



/*
 * Enqueue a fresh (legal) starting grid, if it is safe
 */
static void borg_flow_enqueue_grid(int y, int x)
{
	int old_head;


	/* Analyze every grid once */
	if (borg_cave_danger[y][x] < 0)
	{
		borg_cave_danger[y][x] = borg_danger(y, x, 1);
	}

	/* Avoid dangerous grids */
	if (borg_cave_danger[y][x] > borg_avoid / 2) return;


	/* Only enqueue a grid once */
	if (borg_flow_work[y][x] != 0)
	{
		/* Save the working flow cost (zero) */
		borg_flow_work[y][x] = 0;

		/* Enqueue that entry */
		borg_flow_y[borg_flow_head] = y;
		borg_flow_x[borg_flow_head] = x;


		/* Circular queue -- memorize head */
		old_head = borg_flow_head;

		/* Circular queue -- insert with wrap */
		if (++borg_flow_head == BORG_FLOW_MAX) borg_flow_head = 0;

		/* Circular queue -- handle overflow */
		if (borg_flow_head == borg_flow_tail) borg_flow_head = old_head;
	}
}



/*
 * Do a "reverse" flow from the player outwards
 */
static void borg_flow_reverse(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue the player's grid */
	borg_flow_enqueue_grid(py, px);

	/* Spread, but do NOT optimize */
	borg_flow_spread(250, FALSE, FALSE);
}





/*
 * Attempt to induce "word of recall"
 */
bool borg_recall(void)
{
	/* Multiple "recall" fails */
	if (!borg_recalling)
	{
		/* Try to "recall" */
		if (borg_spell(5, 4) ||
		    borg_prayer(4, 4) ||
		    borg_zap_rod(SV_ROD_RECALL) ||
		    borg_read_scroll(SV_SCROLL_WORD_OF_RECALL))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Nothing */
	return (FALSE);
}



/*
 * Prevent starvation by any means possible
 */
static bool borg_eat_food_any(void)
{
	int i;

	/* Scan the inventory for "normal" food */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip unknown food */
		if (!item->kind) continue;

		/* Skip non-food */
		if (item->tval != TV_FOOD) continue;

		/* Skip "flavored" food */
		if (item->sval < SV_FOOD_MIN_FOOD) continue;

		/* Eat something of that type */
		if (borg_eat_food(item->sval)) return (TRUE);
	}

	/* Scan the inventory for "okay" food */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip unknown food */
		if (!item->kind) continue;

		/* Skip non-food */
		if (item->tval != TV_FOOD) continue;

		/* Skip "icky" food */
		if (item->sval < SV_FOOD_MIN_OKAY) continue;

		/* Eat something of that type */
		if (borg_eat_food(item->sval)) return (TRUE);
	}

	/* Nothing */
	return (FALSE);
}


/*
 * Mega-Hack -- evaluate the "freedom" of the given location
 *
 * The theory is that often, two grids will have equal "danger",
 * but one will be "safer" than the other, perhaps because it
 * is closer to stairs, or because it is in a corridor, or has
 * some other characteristic that makes it "safer".
 *
 * Then, if the Borg is in danger, say, from a normal speed monster
 * which is standing next to him, he will know that walking away from
 * the monster is "pointless", because the monster will follow him,
 * but if the resulting location is "safer" for some reason, then
 * he will consider it.  This will allow him to flee towards stairs
 * in the town, and perhaps towards corridors in the dungeon.
 *
 * This method is used in town to chase the stairs.
 *
 * XXX XXX XXX We should attempt to walk "around" buildings.
 */
static int borg_freedom(int y, int x)
{
	int d, f = 0;

	/* Hack -- chase stairs in town */
	if (!b_ptr->depth && borg_track_more_num)
	{
		/* Love the stairs! */
		d = double_distance(y, x, borg_track_more_y[0], borg_track_more_x[0]);

		/* Proximity is good */
		f += (1000 - d);

		/* Close proximity is great */
		if (d < 4) f += (2000 - (d * 500));
	}

	/* Freedom */
	return (f);
}


/*
 * Check a floor grid for "happy" status
 *
 * These grids are floor grids which contain stairs, or which
 * are non-corners in corridors, or which are directly adjacent
 * to pillars.  Stairs are good because they can be used to leave
 * the level.  Corridors are good because you can back into them
 * to avoid groups of monsters and because they can be used for
 * escaping.  Pillars are good because while standing next to a
 * pillar, you can walk "around" it in two different directions,
 * allowing you to retreat from a single normal monster forever.
 */
static bool borg_happy_grid_bold(int y, int x)
{
	/* Accept stairs */
	if (borg_cave_feat[y][x] == FEAT_LESS) return (TRUE);
	if (borg_cave_feat[y][x] == FEAT_MORE) return (TRUE);


	/* Hack -- weak/dark is very unhappy */
	if (borg_base_is_weak || !b_ptr->cur_lite) return (FALSE);


	/* Case 1a: north-south corridor */
	if (borg_cave_floor_bold(y-1, x) && borg_cave_floor_bold(y+1, x) &&
	    !borg_cave_floor_bold(y, x-1) && !borg_cave_floor_bold(y, x+1))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 1b: east-west corridor */
	if (borg_cave_floor_bold(y, x-1) && borg_cave_floor_bold(y, x+1) &&
	    !borg_cave_floor_bold(y-1, x) && !borg_cave_floor_bold(y+1, x))
	{
		/* Happy */
		return (TRUE);
	}


	/* Case 2a: north pillar */
	if (!borg_cave_floor_bold(y-1, x) &&
	    borg_cave_floor_bold(y-1, x-1) &&
	    borg_cave_floor_bold(y-1, x+1) &&
	    borg_cave_floor_bold(y-2, x))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 2b: south pillar */
	if (!borg_cave_floor_bold(y+1, x) &&
	    borg_cave_floor_bold(y+1, x-1) &&
	    borg_cave_floor_bold(y+1, x+1) &&
	    borg_cave_floor_bold(y+2, x))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 2c: east pillar */
	if (!borg_cave_floor_bold(y, x+1) &&
	    borg_cave_floor_bold(y-1, x+1) &&
	    borg_cave_floor_bold(y+1, x+1) &&
	    borg_cave_floor_bold(y, x+2))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 2d: west pillar */
	if (!borg_cave_floor_bold(y, x-1) &&
	    borg_cave_floor_bold(y-1, x-1) &&
	    borg_cave_floor_bold(y+1, x-1) &&
	    borg_cave_floor_bold(y, x-2))
	{
		/* Happy */
		return (TRUE);
	}


	/* Not happy */
	return (FALSE);
}


/*
 * Help determine if "phase door" seems like a good idea
 */
static bool borg_caution_phase(int dis)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int n, k, i, d, x, y, p;

	int min = dis / 2;

	/* Simulate 100 attempts */
	for (n = k = 0; k < 100; k++)
	{
		/* Pick a location */
		for (i = 0; i < 100; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if ((y <= 0) || (y >= DUNGEON_HGT - 1)) continue;
			if ((x <= 0) || (x >= DUNGEON_WID - 1)) continue;

			/* Skip unknown grids */
			if (borg_cave_feat[y][x] == FEAT_NONE) continue;

			/* Skip weird grids */
			if (borg_cave_feat[y][x] == FEAT_INVIS) continue;

			/* Skip walls */
			if (!borg_cave_floor_bold(y, x)) continue;

			/* Skip monsters */
			if (borg_cave_m_idx[y][x]) continue;

			/* Stop looking */
			break;
		}

		/* No location */
		if (i >= 100) return (FALSE);

		/* Examine */
		p = borg_danger(y, x, 2);

		/* Count "safe" locations */
		if (p <= borg_avoid / 2) n++;
	}

	/* Too much danger */
	if (n < 90) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Be "cautious" and attempt to prevent death or dishonor.
 *
 * Strategy:
 *
 *   (1) Caution
 *   (1a) Analyze the situation
 *   (1b) Teleport from danger
 *   (1c) Handle critical stuff
 *   (1d) Retreat to happy grids
 *   (1e) Back away from danger
 *   (1f) Heal various conditions
 *
 *   (2) Attack
 *   (2a) Simulate possible attacks
 *   (2b) Perform optimal attack
 *
 *   (3) Recover
 *   (3a) Recover by spells/prayers
 *   (3b) Recover by items/etc
 *   (3c) Recover by resting
 *
 * XXX XXX XXX
 * In certain situations, the "proper" course of action is to simply
 * attack a nearby monster, since often most of the danger is due to
 * a single monster which can sometimes be killed in a single blow.
 *
 * Actually, both "borg_caution()" and "borg_recover()" need to
 * be more intelligent, and should probably take into account
 * such things as nearby monsters, and/or the relative advantage
 * of simply pummeling nearby monsters instead of recovering.
 *
 * Note that invisible/offscreen monsters contribute to the danger
 * of an extended "region" surrounding the observation, so we will
 * no longer rest near invisible monsters if they are dangerous.
 *
 * We sometimes try to "rest" to restore mana near "wimpy" monsters
 * which happen to drain mana, which is counter-productive.
 *
 * XXX XXX XXX
 * We should perhaps reduce the "fear" values of each region over
 * time, to take account of obsolete invisible monsters.
 *
 * Note that walking away from a fast monster is counter-productive,
 * since the monster will often just follow us, so we use a special
 * method which allows us to factor in the speed of the monster and
 * predict the state of the world after we move one step.  Of course,
 * walking away from a spell casting monster is even worse, since the
 * monster will just get to use the spell attack multiple times.  But,
 * if we are trying to get to known safety, then fleeing in such a way
 * might make sense.  Actually, this has been done too well, note that
 * it makes sense to flee some monsters, if they "stumble", or if we
 * are trying to get to stairs.  XXX XXX XXX
 *
 * Note that the "flow" routines attempt to avoid entering into
 * situations that are dangerous, but sometimes we do not see the
 * danger coming, and then we must attempt to survive by any means.
 * If we use any of these methods, especially those that cause us to
 * "retreat" by moving a single grid, we must remember to cancel any
 * flow goal in progress, to prevent infinite loops.
 *
 * We will attempt to "teleport" if the danger in the current situation,
 * as well as that resulting from attempting to "back away" from danger,
 * are sufficient to kill us in one or two blows.  This allows us to
 * avoid teleportation in situations where simply backing away is the
 * proper course of action, for example, when standing next to a nasty
 * stationary monster, but also to teleport when backing away will not
 * reduce the danger sufficiently.
 *
 * But note that in "nasty" situations (when we are running out of light,
 * or when we are starving, blind, confused, or hallucinating), we will
 * ignore the possibility of "backing away" from danger, when considering
 * the possibility of using "teleport" to escape.  But if the teleport
 * fails, we will still attempt to "retreat" or "back away" if possible.
 *
 * XXX XXX XXX Note that it should be possible to do some kind of nasty
 * "flow" algorithm which would use a priority queue, or some reasonably
 * efficient normal queue stuff, to determine the path which incurs the
 * smallest "cumulative danger", and minimizes the total path length.
 * It may even be sufficient to treat each step as having a cost equal
 * to the danger of the destination grid, plus one for the actual step.
 * This would allow the Borg to prefer a ten step path passing through
 * one grid with danger 10, to a five step path, where each step has
 * danger 9.  Currently, he often chooses paths of constant danger over
 * paths with small amounts of high danger.  However, the current method
 * is very fast, which is certainly a point in its favor...
 *
 * When in danger, attempt to "flee" by "teleport" or "recall", and if
 * this is not possible, attempt to "heal" damage, if needed, and else
 * attempt to "flee" by "running".
 *
 * XXX XXX XXX Both "borg_caution()" and "borg_recover()" should only
 * perform the "healing" tasks if they will cure more "damage"/"stuff"
 * than may be re-applied in the next turn, this should prevent using
 * wimpy healing spells next to dangerous monsters, and resting to regain
 * mana near a mana-drainer.
 *
 * Whenever we are in a situation in which, even when fully healed, we
 * could die in a single round, we set the "borg_fleeing" flag, and if
 * we could die in two rounds, we set the "borg_leaving" flag.
 *
 * In town, whenever we could die in two rounds if we were to stay still,
 * we set the "borg_leaving" flag.  In combination with the "retreat" and
 * the "back away" code, this should allow us to leave town before getting
 * into situations which might be fatal.
 *
 * Flag "borg_fleeing" means get off this level right now, using recall
 * if possible when we get a chance, and otherwise, take stairs, even if
 * it is very dangerous to do so.
 *
 * Flag "borg_leaving" means get off this level when possible, using
 * stairs if possible when we get a chance.
 *
 * We will also take stairs if we happen to be standing on them, and we
 * could die in two rounds.  This is often "safer" than teleportation,
 * and allows the "retreat" code to retreat towards stairs, knowing that
 * once there, we will leave the level.
 */
bool borg_caution(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int j, p;

	int q, b_q = -1;

	bool nasty = FALSE;


	/*** Notice "nasty" situations ***/

	/* About to run out of light is extremely nasty */
	if (!b_ptr->lite && borg_items[INVEN_LITE].pval < 250) nasty = TRUE;

	/* Starvation is nasty */
	if (borg_base_is_weak) nasty = TRUE;

	/* Blind-ness is nasty */
	if (borg_base_is_blind) nasty = TRUE;

	/* Confusion is nasty */
	if (borg_base_is_confused) nasty = TRUE;

	/* Hallucination is nasty */
	if (borg_base_is_image) nasty = TRUE;


	/*** Evaluate local danger ***/

	/* Look around */
	p = borg_danger(py, px, 1);

	/* Unless "nasty"... */
	if (!nasty || !b_ptr->depth)
	{
		int i;

		/* Attempt to find a better grid */
		for (i = 0; i < 8; i++)
		{
			int x = px + ddx_ddd[i];
			int y = py + ddy_ddd[i];

			/* Skip walls/doors */
			if (!borg_cave_floor_bold(y, x)) continue;

			/* Skip unknown grids XXX XXX XXX */
			if (borg_cave_feat[y][x] == FEAT_NONE) continue;

			/* Skip monster grids */
			if (borg_cave_m_idx[y][x]) continue;

			/* Extract the danger there */
			q = borg_danger(y, x, 2);

			/* Skip larger danger */
			if ((b_q >= 0) && (b_q < q)) continue;

			/* Track */
			b_q = q;
		}
	}

	/* Danger (ignore stupid "fear" danger) */
	if ((p > borg_avoid / 2) || (p > borg_fear_region[py/11][px/11]))
	{
		/* Describe (briefly) the current situation */
		borg_note(format("# Loc:%d,%d Dep:%d Lev:%d HP:%d/%d SP:%d/%d Danger:%d/%d Avoid:%d",
		                 py, px, b_ptr->depth, b_ptr->lev,
		                 b_ptr->chp, b_ptr->mhp, b_ptr->csp, b_ptr->msp,
		                 b_q, p, borg_avoid));
	}

	/* No (good) retreat */
	if ((b_q < 0) || (b_q > p)) b_q = p;


	/*** Danger ***/

	/* Impending doom */
	if (borg_restock())
	{
		/* Start leaving */
		if (!borg_leaving)
		{
			/* Note */
			borg_note("# Leaving (restock)");

			/* Start leaving */
			borg_leaving = TRUE;
		}

		/* Start fleeing */
		if (!borg_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (restock)");

			/* Start fleeing */
			borg_fleeing = TRUE;
		}
	}

	/* Excessive danger */
	else if (b_q > b_ptr->mhp)
	{
		/* Start leaving */
		if (!borg_leaving)
		{
			/* Note */
			borg_note("# Leaving (excessive danger)");

			/* Start leaving */
			borg_leaving = TRUE;
		}

		/* Start fleeing */
		if (!borg_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (excessive danger)");

			/* Start fleeing */
			borg_fleeing = TRUE;
		}

		/* Be less happy about this level */
		if (--borg_happy_count == -1)
		{
			/* Reset happy count */
			borg_happy_count = 0;

			/* Decrease happy depth */
			if (borg_happy_depth > b_ptr->max_depth / 2) borg_happy_depth--;

			/* Note */
			borg_note(format("# Decreased happy depth to %d", borg_happy_depth));
		}
		/* Already unhappy */
		else if (borg_happy_count < 0)
		{
			/* Freeze count */
			borg_happy_count = -1;
		}
	}

	/* Moderate danger */
	else if (b_q > b_ptr->mhp / 2)
	{
		/* Start leaving */
		if (!borg_leaving)
		{
			/* Flee! */
			borg_note("# Leaving (moderate danger)");

			/* Start leaving */
			borg_leaving = TRUE;
		}
	}

	/* Potential instant death in town */
	else if (!b_ptr->depth && (p > b_ptr->chp))
	{
		/* Flee now */
		if (!borg_fleeing)
		{
			/* Flee! */
			borg_note("# Fleeing (potential danger)");

			/* Start leaving */
			borg_fleeing = TRUE;
		}
	}


	/*** Stairs ***/

	/* Leaving */
	else if (borg_leaving || borg_fleeing)
	{
		/* Take stairs up */
		borg_stair_less = TRUE;

		/* Take stairs down (if ready) */
		if (borg_prepared(b_ptr->depth+1))
		{
			borg_stair_more = TRUE;
		}
	}

	/* Take stairs up */
	if (borg_stair_less || (b_q > b_ptr->chp / 2))
	{
		/* Usable stairs */
		if (borg_cave_feat[py][px] == FEAT_LESS)
		{
			/* Send action (take stairs up) */
			borg_keypress('<');

			/* Done */
			return (TRUE);
		}
	}

	/* Take stairs down (unless recalling) */
	if (borg_stair_more || (b_q > b_ptr->chp / 2))
	{
		/* Usable stairs */
		if (borg_cave_feat[py][px] == FEAT_MORE)
		{
			/* Send action (take stairs down) */
			borg_keypress('>');

			/* Done */
			return (TRUE);
		}
	}


	/*** Escape if possible ***/

	/* Attempt to escape */
	if (b_q > borg_avoid)
	{
		/* XXX XXX XXX Count close calls */

		/* Phase door, if useful */
		if (borg_caution_phase(10) &&
			 (borg_spell(0, 2) ||
			  borg_prayer(4, 0) ||
			  borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Success */
			return (TRUE);
		}

		/* Teleport */
		if (TRUE &&
		    (borg_spell(1, 5) ||
		     borg_prayer(4, 1) ||
		     borg_prayer(1, 1) ||
		     borg_read_scroll(SV_SCROLL_TELEPORT) ||
		     borg_use_staff(SV_STAFF_TELEPORTATION)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Success */
			return (TRUE);
		}
	}

	/* Attempt to teleport (usually) */
	if (b_q > borg_avoid / 2)
	{
		/* XXX XXX XXX Count close calls */

		/* Phase door, if useful */
		if (borg_caution_phase(10) &&
		    (borg_spell(0, 2) ||
		     borg_prayer(4, 0) ||
		     borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Success */
			return (TRUE);
		}

		/* Try teleportation */
		if ((rand_int(100) < 50) &&
		    (borg_spell(1, 5) ||
		     borg_prayer(4, 1) ||
		     borg_prayer(1, 1)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Success */
			return (TRUE);
		}
	}


	/*** Deal with critical situations ***/

	/* Hack -- require light */
	if (!b_ptr->lite)
	{
		auto_item *item = &borg_items[INVEN_LITE];

		/* Must have light -- Refuel current torch */
		if ((item->tval == TV_LITE) && (item->sval == SV_LITE_TORCH))
		{
			/* Try to refuel the torch */
			if ((item->pval < 500) && borg_refuel_torch()) return (TRUE);
		}

		/* Must have light -- Refuel current lantern */
		if ((item->tval == TV_LITE) && (item->sval == SV_LITE_LANTERN))
		{
			/* Try to refill the lantern */
			if ((item->pval < 1000) && borg_refuel_lantern()) return (TRUE);
		}

		/* Flee for fuel */
		if (b_ptr->depth && (item->pval < 250))
		{
			/* Start leaving */
			if (!borg_leaving)
			{
				/* Flee */
				borg_note("# Leaving (need fuel)");

				/* Start leaving */
				borg_leaving = TRUE;
			}

			/* Start fleeing */
			if (!borg_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (need fuel)");

				/* Start fleeing */
				borg_fleeing = TRUE;
			}
		}
	}

	/* Hack -- prevent starvation */
	if (borg_base_is_weak)
	{
		/* Attempt to satisfy hunger */
		if (borg_eat_food_any() ||
		    borg_spell(2, 0) ||
		    borg_prayer(1, 5))
		{
			/* Success */
			return (TRUE);
		}

		/* Flee for food */
		if (b_ptr->depth)
		{
			/* Start leaving */
			if (!borg_leaving)
			{
				/* Flee */
				borg_note("# Leaving (need food)");

				/* Start leaving */
				borg_leaving = TRUE;
			}

			/* Start fleeing */
			if (!borg_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (need food)");

				/* Start fleeing */
				borg_fleeing = TRUE;
			}
		}
	}


	/*** Flee on foot ***/

	/* Strategic retreat */
	if (p > borg_avoid / 2)
	{
		int d, b_d = -1;
		int r, b_r = -1;

		int b_x = px;
		int b_y = py;


		/* Scan the useful viewable grids */
		for (j = 1; j < borg_view_n; j++)
		{
			int x1 = px;
			int y1 = py;

			int g = borg_view_g[j];

			int x2 = GRID_X(g);
			int y2 = GRID_Y(g);


			/* Require "floor" grids */
			if (!borg_cave_floor_bold(y2, x2)) continue;

			/* Require "happy" grids */
			if (!borg_happy_grid_bold(y2, x2)) continue;

			/* Track "nearest" grid */
			if (b_r >= 0)
			{
				int ay = ((y2 > y1) ? (y2 - y1) : (y1 - y2));
				int ax = ((x2 > x1) ? (x2 - x1) : (x1 - x2));

				/* Ignore "distant" locations */
				if ((ax > b_r) || (ay > b_r)) continue;
			}


			/* Reset */
			r = 0;

			/* Simulate movement */
			while (1)
			{
				/* Obtain direction */
				d = borg_goto_dir(y1, x1, y2, x2);

				/* Verify direction */
				if ((d == 0) || (d == 5)) break;

				/* Track distance */
				r++;

				/* Simulate the step */
				y1 += ddy[d];
				x1 += ddx[d];


				/* Skip walls/doors */
				if (!borg_cave_floor_bold(y1, x1)) break;

				/* Skip non-viewable grids */
				if (!borg_los(y1, x1, y2, x2)) break;

				/* Skip monsters */
				if (borg_cave_m_idx[y1][x1]) break;

				/* Skip more dangerous grids (apply time) */
				if (borg_danger(y1, x1, r+1) > p) break;


				/* Safe arrival */
				if ((x1 == x2) && (y1 == y2))
				{
					/* Save distance */
					b_r = r;

					/* Save location */
					b_x = x2;
					b_y = y2;

					/* Done */
					break;
				}
			}
		}

		/* Retreat */
		if (b_r >= 0)
		{
			/* Save direction */
			b_d = borg_goto_dir(py, px, b_y, b_x);

			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- set goal */
			xb_ptr->gx = px + ddx[b_d];
			xb_ptr->gy = py + ddy[b_d];

			/* Note */
			borg_note(format("# Retreating to %d,%d (distance %d) via %d,%d (%d >= %d)",
			                 b_y, b_x, b_r, xb_ptr->gy, xb_ptr->gx,
			                 p, borg_danger(xb_ptr->gy, xb_ptr->gx, 2)));

			/* Send action (walk) */
			borg_keypress(';');

			/* Send direction */
			borg_keypress(I2D(b_d));

			/* Done */
			return (TRUE);
		}
	}


	/* Want to back away */
	if (p > borg_avoid / 2)
	{
		int i, b_i = -1;
		int k, b_k = -1;
		int f, b_f = -1;

		/* Current danger */
		b_k = p;

		/* Check the freedom */
		b_f = borg_freedom(py, px);

		/* Attempt to find a better grid */
		for (i = 0; i < 8; i++)
		{
			int x = px + ddx_ddd[i];
			int y = py + ddy_ddd[i];

			/* Skip walls/doors */
			if (!borg_cave_floor_bold(y, x)) continue;

			/* Skip monster grids */
			if (borg_cave_m_idx[y][x]) continue;

			/* Extract the danger there */
			k = borg_danger(y, x, 2);

			/* Skip higher danger */
			if (b_k < k) continue;

			/* Check the freedom */
			f = borg_freedom(y, x);

			/* Skip bad locations */
			if ((b_k == k) && (b_f > f)) continue;

			/* Save the info */
			b_i = i; b_k = k; b_f = f;
		}

		/* Back away */
		if (b_i >= 0)
		{
			int dir;

			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- set goal */
			xb_ptr->gx = px + ddx_ddd[b_i];
			xb_ptr->gy = py + ddy_ddd[b_i];

			/* Note */
			borg_note(format("# Backing up to %d,%d (%d >= %d)",
			                 xb_ptr->gy, xb_ptr->gx,
			                 p, borg_danger(xb_ptr->gy, xb_ptr->gx, 2)));

			/* Get direction */
			dir = ddd[b_i];

			/* Send action (walk) */
			borg_keypress(';');

			/* Send direction */
			borg_keypress(I2D(dir));

			/* Done */
			return (TRUE);
		}

		/* Note */
		borg_note(format("# Cornered (danger %d)", p));
	}


	/*** Try healing ***/

	/* Hack -- heal when wounded (prayers) */
	if ((b_ptr->chp <= b_ptr->mhp / 2) && (rand_int(100) < 20))
	{
		if (borg_prayer(2, 2) ||
		    borg_prayer(2, 7) ||
		    borg_prayer(6, 0) ||
		    borg_prayer(6, 1))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal when wounded */
	if ((b_ptr->chp <= b_ptr->mhp / 2) && (rand_int(100) < 20))
	{
		if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_quaff_potion(SV_POTION_CURE_SERIOUS))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal when blind/confused */
	if ((borg_base_is_blind || borg_base_is_confused) && (rand_int(100) < 20))
	{
		if (borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure wounds when bleeding */
	if (borg_base_is_cut && (rand_int(100) < 10))
	{
		if (borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure poison when poisoned */
	if (borg_base_is_poisoned && (rand_int(100) < 10))
	{
		if (borg_spell(1, 4) ||
		    borg_prayer(2, 0) ||
		    borg_quaff_potion(SV_POTION_CURE_POISON) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure fear when afraid */
	if (borg_base_is_afraid && (rand_int(100) < 10))
	{
		if (borg_prayer(0, 3) ||
		    borg_quaff_potion(SV_POTION_BOLDNESS) ||
		    borg_quaff_potion(SV_POTION_HEROISM) ||
		    borg_quaff_potion(SV_POTION_BESERK_STRENGTH))
		{
			return (TRUE);
		}
	}


	/*** Note impending death XXX XXX XXX ***/

	/* Flee from low hit-points */
	if ((b_ptr->chp < b_ptr->mhp / 2) &&
		 (amt_cure_critical < 5))
	{
		/* Flee from low hit-points */
		if (b_ptr->depth && (rand_int(100) < 25))
		{
			/* Start leaving */
			if (!borg_leaving)
			{
				/* Flee */
				borg_note("# Leaving (low hit-points)");

				/* Start leaving */
				borg_leaving = TRUE;
			}

			/* Start fleeing */
			if (!borg_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (low hit-points)");

				/* Start fleeing */
				borg_fleeing = TRUE;
			}
		}
	}

	/* Hack -- use "recall" to flee if possible */
	if (borg_fleeing && b_ptr->depth && (borg_recall()))
	{
		/* Note */
		borg_note("# Fleeing the level (recall)");

		/* Success */
		return (TRUE);
	}


	/* Nothing */
	return (FALSE);
}



/*
 * New method for handling attacks, missiles, and spells
 *
 * Every turn, we evaluate every known method of causing damage
 * to monsters, and evaluate the "reward" inherent in each of
 * the known methods which is usable at that time, and then
 * we actually use whichever method, if any, scores highest.
 *
 * For each attack, we need a function which will determine the best
 * possible result of using that attack, and return its value.  Also,
 * if requested, the function should actually perform the action.
 *
 * Note that the functions should return zero if the action is not
 * usable, or if the action is not useful.
 *
 * These functions need to apply some form of "cost" evaluation, to
 * prevent the use of expensive spells with minimal reward.  Also,
 * we should always prefer attacking by hand to using spells if the
 * damage difference is "small", since there is no "cost" in making
 * a physical attack.
 *
 * We should take account of "spell failure", as well as "missile
 * missing" and "blow missing" probabilities.
 *
 * Note that the functions may store local state information when
 * doing a "simulation" and then they can use this information if
 * they are asked to implement their strategy.
 *
 * XXX XXX XXX
 *
 * We should "reward" doing damage to monsters which are "dangerous",
 * unless they are sleeping, in which case we should penalize waking
 * them up, unless we can kill them instantly.  Note that this means
 * that killing a single "gnome mage" with 10 hitpoints should be
 * considered "better" than doing 30 damage to a "white jelly",
 * since the "gnome mage" is much more "dangerous".  We should
 * check the danger over several turns, to take account of nasty
 * monsters which are not right next to us.
 *
 * We should attempt to apply any "brand" effects of the current
 * "weapon" and/or "ammo".
 *
 * We should "attempt" to keep track of each monsters "hitpoints",
 * since this will make the attack code "smarter", but we should
 * not be too optimistic, since mistakes could be fatal.
 *
 * We should try to avoid damaging objects on the ground, that is,
 * we should not use "frost ball" near potions, etc.
 *
 * Note that all "fire" commands have a minimum range of 20, but the
 * various "throw" commands have a range which is limited by strength
 * and weight, and is limited to a total distance of ten.  We should
 * attempt to take into to account the "effective" range.  XXX XXX XXX
 *
 * There are several types of damage inducers:
 *
 *   Attacking physically
 *   Launching missiles
 *   Throwing objects
 *   Casting spells
 *   Praying prayers
 *   Using rods
 *   Using wands
 *   Using staffs
 *   Using scrolls
 *   Using dragon scale mail
 *   Using artifact activations
 */
enum
{
	BF_THRUST,

	BF_LAUNCH,

	BF_OBJECT,

	BF_SPELL_MAGIC_MISSILE,
	BF_SPELL_ELEC_BOLT,
	BF_SPELL_COLD_BOLT,
	BF_SPELL_FIRE_BOLT,
	BF_SPELL_ACID_BOLT,

	BF_SPELL_LITE_BEAM,

	BF_SPELL_POISON_BALL,
	BF_SPELL_COLD_BALL,
	BF_SPELL_ACID_BALL,
	BF_SPELL_FIRE_BALL,

	BF_SPELL_POISON_STORM,
	BF_SPELL_COLD_STORM,
	BF_SPELL_METEOR_STORM,
	BF_SPELL_MANA_STORM,

	BF_PRAYER_HOLY_ORB_BALL,

	BF_ROD_ELEC_BOLT,
	BF_ROD_COLD_BOLT,
	BF_ROD_ACID_BOLT,
	BF_ROD_FIRE_BOLT,

	BF_ROD_LITE_BEAM,

	BF_ROD_ELEC_BALL,
	BF_ROD_COLD_BALL,
	BF_ROD_ACID_BALL,
	BF_ROD_FIRE_BALL,

	BF_WAND_MAGIC_MISSILE,
	BF_WAND_ELEC_BOLT,
	BF_WAND_COLD_BOLT,
	BF_WAND_ACID_BOLT,
	BF_WAND_FIRE_BOLT,

	BF_WAND_LITE_BEAM,

	BF_WAND_STINKING_CLOUD,
	BF_WAND_ELEC_BALL,
	BF_WAND_COLD_BALL,
	BF_WAND_ACID_BALL,
	BF_WAND_FIRE_BALL,

	BF_WAND_DRAGON_COLD,
	BF_WAND_DRAGON_FIRE,

	BF_DRAGON_BLUE,
	BF_DRAGON_WHITE,
	BF_DRAGON_BLACK,
	BF_DRAGON_GREEN,
	BF_DRAGON_RED,
#if 0
	BF_DRAGON_MULTIHUED,
#endif
	BF_DRAGON_BRONZE,
	BF_DRAGON_GOLD,
#if 0
	BF_DRAGON_CHAOS,
	BF_DRAGON_LAW,
	BF_DRAGON_BALANCE,
	BF_DRAGON_SHINING,
#endif
	BF_DRAGON_POWER,

	BF_ART_NARYA,
	BF_ART_NENYA,
	BF_ART_VILYA,
	BF_ART_CAMMITHRIM,
	BF_ART_PAURHACH,
	BF_ART_PAURNIMMEN,
	BF_ART_PAURAEGEN,
	BF_ART_PAURNEN,
	BF_ART_FINGOLFIN,
	BF_ART_NARTHANC,
	BF_ART_NIMTHANC,
	BF_ART_DETHANC,
	BF_ART_RILIA,
	BF_ART_BELANGIL,
	BF_ART_ARUNRUTH,
	BF_ART_RINGIL,
	BF_ART_ANDURIL,
	BF_ART_THEODEN,
	BF_ART_AEGLOS,
	BF_ART_FIRESTAR,
	BF_ART_TURMIL,

	BF_MAX
};



/*
 * Guess how much damage a physical attack will do to a monster
 *
 * We ignore "brands" and such for now
 */
static int borg_thrust_damage_one(int i)
{
	int dam;

	auto_kill *kill;

	monster_race *r_ptr;

	auto_item *item;


	/* Examine current weapon */
	item = &borg_items[INVEN_WIELD];


	/* Monster record */
	kill = &borg_kills[i];

	/* Monster race */
	r_ptr = &r_info[kill->r_idx];


	/* Damage */
	dam = (item->dd * (item->ds + 1) / 2);
	dam = dam + item->to_d + b_ptr->to_d;
	dam = dam * b_ptr->num_blow;


	/* Limit damage to twice maximal hitpoints */
	if (dam > kill->power * 2) dam = kill->power * 2;


	/* Damage */
	return (dam);
}



/*
 * Simulate/Apply the optimal result of making a physical attack
 */
static int borg_attack_aux_thrust(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int p, dir;

	int m_idx;

	int num = 0;

	int i, b_i = -1;
	int d, b_d = -1;

	auto_kill *kill;


	/* Too afraid to attack */
	if (borg_base_is_afraid) return (0);


	/* Examine possible destinations */
	for (i = 0; i < borg_temp_n; i++)
	{
		int x = borg_temp_x[i];
		int y = borg_temp_y[i];

		/* Require "adjacent" */
		if (distance(py, px, y, x) > 1) continue;

		/* Monster code */
		m_idx = borg_cave_m_idx[y][x];

		/* Calculate "average" damage */
		d = borg_thrust_damage_one(m_idx);

		/* No damage */
		if (d <= 0) continue;

		/* Obtain the monster */
		kill = &borg_kills[m_idx];

		/* Calculate danger */
		p = borg_danger_aux(y, x, 1, m_idx);

		/* Hack -- avoid waking most "hard" sleeping monsters */
		if (!kill->awake && (p > borg_avoid / 2) && (d <= kill->power))
		{
			continue;
		}

		/* Calculate "danger" to player */
		p = borg_danger_aux(py, px, 4, m_idx);

		/* Reduce "bonus" of partial kills */
		if (d <= kill->power) p = p / 10;

		/* Add the danger to the damage */
		d += p;

		/* Ignore lower damage */
		if ((b_i >= 0) && (d < b_d)) continue;

		/* Hack -- reset chooser */
		if ((b_i >= 0) && (d > b_d)) num = 0;

		/* Apply the randomizer */
		if ((num > 1) && (rand_int(num) != 0)) continue;

		/* Save the info */
		b_i = i;
		b_d = d;
	}

	/* Nothing to attack */
	if (b_i < 0) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering normal attack with value %d", b_d));

	/* Simulation */
	if (borg_simulate) return (b_d);


	/* Save the location */
	xb_ptr->gx = borg_temp_x[b_i];
	xb_ptr->gy = borg_temp_y[b_i];


	/* Note */
	borg_note(format("# Facing location (%d,%d)",
	                 xb_ptr->gy, xb_ptr->gx));

	/* Note */
	borg_note(format("# Attacking with weapon '%s'",
	                 borg_items[INVEN_WIELD].desc));

	/* Get a direction for attacking */
	dir = borg_extract_dir(py, px, xb_ptr->gy, xb_ptr->gx);

	/* Send action (alter) */
	borg_keypress('+');

	/* Send direction */
	borg_keypress(I2D(dir));

	/* Value */
	return (b_d);
}




/*
 * Target a location.
 *
 * Can be used as a command or at the "Direction?" prompt.
 *
 * This will only work for locations on the current panel.  XXX XXX XXX
 */
static bool borg_target(int y, int x)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int x1, y1, x2, y2;

	/* Log */
	borg_note(format("# Targetting %s (%d,%d)",
						  borg_cave_m_idx[y][x] ? "monster" : "location", y, x));

	/* Send action (target) */
	borg_keypress('*');

	/* Send target from player key */
	borg_keypress('p');

	/* Determine "path" */
	x1 = px;
	y1 = py;
	x2 = x;
	y2 = y;

	/* Send diagonal motion keys */
	for (; (y1 < y2) && (x1 < x2); y1++, x1++) borg_keypress('3');
	for (; (y1 < y2) && (x1 > x2); y1++, x1--) borg_keypress('1');
	for (; (y1 > y2) && (x1 < x2); y1--, x1++) borg_keypress('9');
	for (; (y1 > y2) && (x1 > x2); y1--, x1--) borg_keypress('7');

	/* Send horizontal motion keys */
	for (; y1 < y2; y1++) borg_keypress('2');
	for (; y1 > y2; y1--) borg_keypress('8');
	for (; x1 < x2; x1++) borg_keypress('6');
	for (; x1 > x2; x1--) borg_keypress('4');

	/* Send target selection key */
	borg_keypress('5');

	/* Done */
	return (TRUE);
}



/*
 * Guess how much damage a spell attack will do to a monster
 *
 * We only handle the "standard" damage types.
 *
 * We are paranoid about monster resistances
 *
 * We ignore "special" effects for now
 */
static int borg_launch_damage_one(int i, int dam, int typ)
{
	auto_kill *kill;

	monster_race *r_ptr;


	/* Monster record */
	kill = &borg_kills[i];

	/* Monster race */
	r_ptr = &r_info[kill->r_idx];


	/* Analyze the damage type */
	switch (typ)
	{
		/* Magic Missile */
		case GF_MISSILE:
		{
			break;
		}

		/* Arrow */
		case GF_ARROW:
		{
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			break;
		}

		/* Meteor -- powerful magic missile */
		case GF_METEOR:
		{
			break;
		}


		/* Acid */
		case GF_ACID:
		{
			if (r_ptr->flags3 & RF3_IM_ACID) dam /= 9;
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			if (r_ptr->flags3 & RF3_IM_ELEC) dam /= 9;
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			if (r_ptr->flags3 & RF3_IM_FIRE) dam /= 9;
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			if (r_ptr->flags3 & RF3_IM_COLD) dam /= 9;
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (r_ptr->flags3 & RF3_IM_POIS) dam /= 9;
			break;
		}

		/* Ice */
		case GF_ICE:
		{
			if (r_ptr->flags3 & RF3_IM_COLD) dam /= 9;
			break;
		}


		/* Holy Orb */
		case GF_HOLY_ORB:
		{
			if (r_ptr->flags3 & RF3_EVIL) dam *= 2;
			break;
		}


		/* Weak Lite */
		case GF_LITE_WEAK:
		{
			if (!(r_ptr->flags3 & RF3_HURT_LITE)) dam = 0;
			break;
		}


		/* Drain Life */
		case GF_OLD_DRAIN:
		{
			if ((r_ptr->flags3 & RF3_UNDEAD) ||
			    (r_ptr->flags3 & RF3_DEMON) ||
			    (strchr("Egv", r_ptr->d_char)))
			{
				dam = 0;
			}
			break;
		}


		/* Weird attacks */
		case GF_PLASMA:
		case GF_NETHER:
		case GF_WATER:
		case GF_CHAOS:
		case GF_SHARD:
		case GF_SOUND:
		case GF_CONFUSION:
		case GF_DISENCHANT:
		case GF_NEXUS:
		case GF_FORCE:
		case GF_INERTIA:
		case GF_TIME:
		case GF_GRAVITY:
		case GF_LITE:
		case GF_DARK:
		{
			dam /= 2;
			break;
		}


		/* Various */
		case GF_OLD_SLOW:
		case GF_OLD_CONF:
		case GF_OLD_SLEEP:
		case GF_OLD_POLY:
		case GF_OLD_HEAL:
		case GF_OLD_CLONE:
		case GF_OLD_SPEED:
		case GF_DARK_WEAK:
		case GF_KILL_WALL:
		case GF_KILL_DOOR:
		case GF_KILL_TRAP:
		case GF_MAKE_WALL:
		case GF_MAKE_DOOR:
		case GF_MAKE_TRAP:
		case GF_AWAY_UNDEAD:
		case GF_AWAY_EVIL:
		case GF_AWAY_ALL:
		case GF_TURN_UNDEAD:
		case GF_TURN_EVIL:
		case GF_TURN_ALL:
		case GF_DISP_UNDEAD:
		case GF_DISP_EVIL:
		case GF_DISP_ALL:
		{
			dam = 0;
			break;
		}
	}

	/* Damage */
	return (dam);
}



/*
 * Simulate / Invoke the launching of a bolt at a monster
 */
static int borg_launch_bolt_aux_hack(int i, int dam, int typ)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int raw, val, x, y, d;

	int pow_melee, pow_range, pow;

	int ay, ax;

	auto_kill *kill;

	monster_race *r_ptr;

	/* Monster */
	kill = &borg_kills[i];

	/* Skip dead monsters */
	if (!kill->r_idx) return (0);

	/* Acquire race */
	r_ptr = &r_info[kill->r_idx];

	/* Require current knowledge */
	if (kill->when < borg_time) return (0);

	/* Acquire location */
	x = kill->x;
	y = kill->y;

	/* Distance components */
	ax = (px > x) ? (px - x) : (x - px);
	ay = (py > y) ? (py - y) : (y - py);

	/* Distance */
	d = MAX(ax, ay);

	/* Minimal distance */
	if (d < 1) d = 1;

	/* Never shoot walls/doors */
	if (!borg_cave_floor_bold(y, x)) return (0);

	/* Hack -- Unknown grids should be avoided some of the time */
	if ((borg_cave_feat[y][x] == FEAT_NONE) && ((borg_time % 8) == 0)) return (0);

	/* Hack -- Weird grids should be avoided some of the time */
	if ((borg_cave_feat[y][x] == FEAT_INVIS) && ((borg_time % 8) == 0)) return (0);

	/* Calculate damage */
	val = raw = borg_launch_damage_one(i, dam, typ);

	/* Penalize overkill */
	if (raw > kill->power) val = 2 * kill->power - raw;

	/* No damage */
	if (val <= 0) return (0);

	/* Calculate danger */
	pow_melee = borg_danger_aux(y, x, 1, i);
	pow_range = borg_danger_aux(py, px, 4, i);

	/* Immobile monsters aren't dangerous at a distance */
	if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (d > 1)) pow_melee = 0;

	pow = MAX(pow_melee, pow_range);

	/* Hack -- ignore "easy" / "sleeping" town monsters */
	if (!b_ptr->depth && (!kill->awake || (pow <= 0)))
	{
		return (0);
	}

	if (pow > borg_avoid / 2)
	{
		if (borg_simulate) borg_note(format("#    Dangerous grid %d,%d (%d > %d)",
														y, x, pow, borg_avoid / 2));
	}

	if (borg_simulate) borg_note(format("#    %s kill for %d,%d (%d %s %d)",
													(raw <= kill->power) ? "Slow" : "Quick", y, x, raw,
													(raw <= kill->power) ? "<=" : ">", kill->power));

	/* Hack -- avoid waking most "hard" sleeping monsters */
	if (!kill->awake && (pow > borg_avoid / 2) && (raw <= kill->power))
	{
		return (-999);
	}

	/* Reduce "bonus" of partial kills */
	if (raw <= kill->power) pow /= 10;

	/* Add in power */
	val += pow;

	/* Result */
	return (val);
}


/*
 * Determine the "reward" of launching a beam/bolt/ball at a location
 *
 * The "player grid" itself always has zero reward, to prevent various
 * possible complications, even though sometimes the player grid is the
 * best target for a ball attack.
 *
 * An "unreachable" location always has zero reward, for various reasons,
 * even though sometimes such a target can cause some reasonable "splash"
 * damage.
 *
 * Basically, we sum the "rewards" of doing the appropriate amount of
 * damage to each of the "affected" monsters.
 */
static int borg_launch_bolt_aux(int y, int x, int rad, int dam, int typ, int max)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, j;

	int oy, ox;
	int ay, ax;

	int x1, y1;
	int x2, y2;

	int r, n;

	int d;

	int t;

	int pow_melee, pow_range, pow;

	int m_idx;

	auto_kill *kill;

	/* Projection flags */
	int flg = 0;

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	u16b path_g[512];


	/* Initial location */
	x1 = px;
	y1 = py;

	/* Final location */
	x2 = x;
	y2 = y;

	/* Avoid unknown grids (see above) */
	if ((borg_cave_feat[y][x] == FEAT_NONE) &&
		 ((typ == BF_LAUNCH) || (typ == BF_OBJECT) || !borg_cave_m_idx[y][x]))
	{
		if (borg_simulate) borg_note("#    Bad shot (unknown grid)");
		return (0);
	}

	/* Avoid weird grids (see above) */
	if ((borg_cave_feat[y][x] == FEAT_INVIS) &&
		 ((typ == BF_LAUNCH) || (typ == BF_OBJECT) || !borg_cave_m_idx[y][x]))
	{
		if (borg_simulate) borg_note("#    Bad shot (invisible grid)");
		return (0);
	}

	/* Stop at monsters (bolts) */
	/* if (!rad) flg |= (PROJECT_STOP); */

	/* Obtain a local projection path */
	path_n = borg_project_path(path_g, max, y1, x1, y2, x2, flg);

	/* Paranoia */
	if (!path_n) return (0);

	/* Final location */
	y = GRID_Y(path_g[path_n-1]);
	x = GRID_X(path_g[path_n-1]);

	/* Make sure destination is reachable */
	if ((y != y2) || (x != x2)) return (0);

	/* Balls explode before hitting walls */
	if (rad && !borg_cave_floor_bold(y, x))
	{
		/* Forget the last grid */
		path_n--;

		/* New final location */
		y = GRID_Y(path_g[path_n-1]);
		x = GRID_X(path_g[path_n-1]);
	}

	/* Reset damage */
	n = 0;

	/* Process the path */
	for (i = 0; i < path_n; ++i)
	{
		/* Location */
		y = GRID_Y(path_g[i]);
		x = GRID_X(path_g[i]);

		/* Monster code */
		m_idx = borg_cave_m_idx[y][x];

		/* Fake code if necessary */
		for (j = 0; !m_idx && (j < borg_temp_n); j++)
		{
			oy = borg_temp_y[j];
			ox = borg_temp_x[j];

			/* Find a nearby monster */
			if (distance(oy, ox, y, x) <= 1) m_idx = borg_cave_m_idx[y][x];
		}

		/* Avoid unknown grids (see above) */
		if ((borg_cave_feat[y][x] == FEAT_NONE) && (i != path_n - 1))
		{
			if (borg_simulate) borg_note("#    Bad shot (unknown grid)");
			return (0);
		}

		/* Avoid weird grids (see above) */
		if ((borg_cave_feat[y][x] == FEAT_INVIS) && (i != path_n - 1))
		{
			if (borg_simulate) borg_note("#    Bad shot (invisible grid)");
			return (0);
		}

		/* Avoid non-terminal monsters (bolts) */
		if ((rad == 0) && (i != path_n - 1) && m_idx)
		{
			if (borg_simulate) borg_note("#    Bad shot (blocked)");
			return (0);
		}

		/* Collect damage (bolts/beams) */
		if (rad <= 0) n += borg_launch_bolt_aux_hack(m_idx, dam, typ);
	}

	/* Bolt/Beam attack */
	if (rad <= 0) return (n);

	/* Check monsters for blast damage */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Acquire location */
		x = borg_temp_x[i];
		y = borg_temp_y[i];

		/* Check distance */
		r = distance(y2, x2, y, x);

		/* Maximal distance */
		if (r > rad) continue;

		/* Never pass through walls */
		if (!borg_los(y2, x2, y, x)) continue;

		/* Monster code */
		m_idx = borg_cave_m_idx[y][x];

		/* Ignore non-monsters */
		if (!m_idx) continue;

		/* Acquire monster */
		kill = &borg_kills[m_idx];

		/* Distance components */
		ax = (px > x) ? (px - x) : (x - px);
		ay = (py > y) ? (py - y) : (y - py);

		/* Distance */
		d = MAX(ax, ay);

		/* Calculate danger */
		pow_melee = borg_danger_aux(y, x, 1, i);
		pow_range = borg_danger_aux(py, px, 4, i);

		/* Immobile monsters aren't dangerous at a distance */
		if ((r_info[kill->r_idx].flags1 & RF1_NEVER_MOVE) && (d > 1)) pow_melee = 0;

		/* Determine potential danger */
		pow = MAX(pow_melee, pow_range);

		/* Collect damage, lowered by distance */
		t = borg_launch_bolt_aux_hack(m_idx, dam / (r + 1), typ);

		/* Don't wake dangerous monsters */
		if (!kill->awake && (pow > borg_avoid / 2) &&
			 (t / (r + 1) <= kill->power))
		{
			n = -999;
		}
		else
		{
			n += t;
		}
	}

	/* Result */
	return (n);
}


/*
 * Simulate/Apply the optimal result of launching a beam/bolt/ball
 *
 * Note that "beams" have a "rad" of "-1", "bolts" have a "rad" of "0",
 * and "balls" have a "rad" of "2" or "3", depending on "blast radius".
 */
static int borg_launch_bolt(int rad, int dam, int typ, int max)
{
	int num = 0;

	int i, b_i = -1;
	int n, b_n = 0;

	int m_idx;

	/* Examine possible destinations */
	for (i = 0; i < borg_temp_n; i++)
	{
		int x = borg_temp_x[i];
		int y = borg_temp_y[i];

		/* Monster code */
		m_idx = borg_cave_m_idx[y][x];

		/* Message */
		if (m_idx)
		{
			if (borg_simulate) borg_note(format("#   Examining '%^s' at %d,%d",
								  							r_name + r_info[borg_kills[m_idx].r_idx].name, y, x));
		}
		/* Avoid useless bolts */
		else if (rad <= 0)
		{
			continue;
		}
		/* Message */
		else
		{
			if (borg_simulate) borg_note(format("#   Examining offset at %d,%d", y, x));
		}

		/* Consider it */
		n = borg_launch_bolt_aux(y, x, rad, dam, typ, max);

		/* Skip useless attacks */
		if (n <= 0) continue;

		/* Message */
		if (borg_simulate) borg_note(format("#   Found shot with value %d to %d,%d", n, y, x));

		/* Collect best attack */
		if ((b_i >= 0) && (n < b_n)) continue;

		/* Hack -- reset chooser */
		if ((b_i >= 0) && (n > b_n)) num = 0;

      num++;

		/* Apply the randomizer */
		if ((num > 1) && (rand_int(num) != 0)) continue;

		/* Track it */
		b_i = i;
		b_n = n;
	}

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Save the location */
	xb_ptr->gx = borg_temp_x[b_i];
	xb_ptr->gy = borg_temp_y[b_i];

	/* Target the location */
	(void)borg_target(xb_ptr->gy, xb_ptr->gx);

	/* Result */
	return (b_n);
}


/*
 * Simulate/Apply the optimal result of launching a normal missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch(void)
{
	int b_n;

	int k, b_k = -1;
	int d, b_d = -1;

	auto_item *bow = &borg_items[INVEN_BOW];


	/* Scan the pack */
	for (k = 0; k < INVEN_PACK; k++)
	{
		auto_item *item = &borg_items[k];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip bad missiles */
		if (item->tval != b_ptr->ammo_tval) continue;

		/* Skip worthless missiles */
		if (item->value <= 0) continue;

		/* Skip non-known, non-average, missiles */
		if (!item->able && !item->average) continue;

		/* Determine average damage */
		d = (item->dd * (item->ds + 1) / 2);
		d = d + item->to_d + bow->to_d;
		d = d * b_ptr->ammo_mult * b_ptr->num_fire;

		/* Paranoia */
		if (d <= 0) continue;

		/* Ignore worse damage */
		if ((b_k >= 0) && (d <= b_d)) continue;

		/* Track */
		b_k = k;
		b_d = d;
	}

	/* Nothing to use */
	if (b_k < 0) return (0);


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering firing '%^s'",
													k_name + k_info[borg_items[b_k].kind].name));


	/* Choose optimal location */
	b_n = borg_launch_bolt(0, b_d, GF_ARROW, MAX_RANGE);

	/* Cost one point */
	b_n = b_n - 1;

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Do it */
	borg_note(format("# Firing standard missile '%s'",
	                 borg_items[b_k].desc));

	/* Send action (fire) */
	borg_keypress('f');

	/* Send item index */
	borg_send_item_index(b_k);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of throwing an object
 *
 * First choose the "best" object to throw, then check targets.
 */
static int borg_attack_aux_object(void)
{
	int b_n;

	int b_r = 0;

	int k, b_k = -1;
	int d, b_d = -1;

	int div, mul;

	/* Scan the pack */
	for (k = 0; k < INVEN_PACK; k++)
	{
		auto_item *item = &borg_items[k];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-known, non-average, objects */
		if (!item->able && !item->average) continue;

		/* Skip "equipment" items (not ammo) */
		if (borg_wield_slot(item) >= 0) continue;

		/* Determine average damage from object */
		d = (k_info[item->kind].dd * (k_info[item->kind].ds + 1) / 2);

		/* Skip useless stuff */
		if (d <= 0) continue;

		/* Skip "expensive" stuff */
		if (d < item->value) continue;

		/* Hack -- Save last five flasks for fuel, if needed */
		if ((item->tval == TV_FLASK) && (amt_fuel <= 5)) continue;

		/* Ignore worse damage */
		if ((b_k >= 0) && (d <= b_d)) continue;

		/* Track */
		b_k = k;
		b_d = d;

		/* Extract a "distance multiplier" */
		mul = 10;

		/* Enforce a minimum "weight" of one pound */
		div = ((item->weight > 10) ? item->weight : 10);

		/* Hack -- Distance -- Reward strength, penalize weight */
		b_r = (adj_str_blow[b_ptr->stat_ind[A_STR]] + 20) * mul / div;

		/* Max distance of 10 */
		if (b_r > 10) b_r = 10;
	}

	/* Nothing to use */
	if (b_k < 0) return (0);


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering throwing '%^s'",
													k_name + k_info[borg_items[b_k].kind].name));


	/* Choose optimal location */
	b_n = borg_launch_bolt(0, b_d, GF_ARROW, b_r);

	/* Cost one point */
	b_n = b_n - 1;

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Do it */
	borg_note(format("# Throwing painful object '%s'",
	                 borg_items[b_k].desc));

	/* Send action (throw) */
	borg_keypress('v');

	/* Send item index */
	borg_send_item_index(b_k);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}




/*
 * Simulate/Apply the optimal result of using a "normal" attack spell
 *
 * Take into account the failure rate of spells/objects/etc.  XXX XXX XXX
 */
static int borg_attack_aux_spell_bolt(int book, int what, int rad, int dam, int typ)
{
	int b_n;

	auto_magic *as = &borg_magics[book][what];


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


	/* Require ability (right now) */
	if (!borg_spell_okay(book, what)) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering spell '%^s'", as->name));


	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Penalize mana usage */
	b_n -= as->power;

	/* Penalize use of reserve mana */
	if (b_ptr->csp <= as->power * 2) b_n -= as->power * 2;

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Send action (cast spell) */
	(void)borg_spell(book, what);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of using a "normal" attack prayer
 */
static int borg_attack_aux_prayer_bolt(int book, int what, int rad, int dam, int typ)
{
	int b_n;

	auto_magic *as = &borg_magics[book][what];


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


	/* Require ability */
	if (!borg_prayer_okay(book, what)) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering prayer '%^s'", as->name));


	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Penalize mana usage */
	b_n -= as->power;

	/* Penalize use of reserve mana */
	if (b_ptr->csp <= as->power * 2) b_n -= as->power * 2;

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Send action (pray prayer) */
	(void)borg_prayer(book, what);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of using a "normal" attack rod
 */
static int borg_attack_aux_rod_bolt(int sval, int rad, int dam, int typ)
{
	int i;

	int b_n;


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


	/* Look for that rod */
	i = borg_slot(TV_ROD, sval);

	/* None available */
	if (i < 0) return (0);

	/* Still "charging" */
	if (!borg_items[i].pval) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering rod '%^s'",
													k_name + k_info[borg_items[i].kind].name));


	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Send action (zap rod) */
	(void)borg_zap_rod(sval);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of using a "normal" attack wand
 */
static int borg_attack_aux_wand_bolt(int sval, int rad, int dam, int typ)
{
	int i;

	int b_n;


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


	/* Look for that wand */
	i = borg_slot(TV_WAND, sval);

	/* None available */
	if (i < 0) return (0);

	/* No charges */
	if (!borg_items[i].pval) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering wand '%^s'",
													k_name + k_info[borg_items[i].kind].name));

	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Penalize charge usage */
	b_n = b_n - 5;

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Send action (aim wand) */
	(void)borg_aim_wand(sval);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of using dragon scale mail
 */
static int borg_attack_aux_dragon_bolt(int sval, int rad, int dam, int typ)
{
	int i;

	int b_n;


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


	/* Hack -- Find the dragon scale mail XXX XXX */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Require dragon scale mail */
		if (item->tval != TV_DRAG_ARMOR) continue;

		/* Require requested dragon scale mail */
		if (item->sval != sval) continue;

		/* Check charge */
		if (item->timeout) continue;

		/* Hack -- decline artifacts */
		if (item->name1) continue;

		/* Okay */
		break;
	}

	/* None available */
	if (i >= INVEN_TOTAL) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering dragon scale mail '%^s'",
													k_name + k_info[borg_items[i].kind].name));


	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Send action (activate dragon scale mail) */
	(void)borg_activate_dragon(sval);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of using a "normal" attack artifact
 */
static int borg_attack_aux_artifact_bolt(int name1, int rad, int dam, int typ)
{
	int i;

	int b_n;


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


	/* Hack -- Find the artifact XXX XXX */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip incorrect artifacts */
		if (item->name1 != name1) continue;

		/* Check charge */
		if (item->timeout) continue;

		/* Okay */
		break;
	}

	/* None available */
	if (i >= INVEN_TOTAL) return (0);

	/* Message */
	if (borg_simulate) borg_note(format("#  Considering artifact '%^s'",
													a_name + a_info[borg_items[i].name1].name));

	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Send action (activate artifact) */
	(void)borg_activate_artifact(name1);

	/* Send direction (use target) */
	borg_keypress('5');

	/* Value */
	return (b_n);
}



/*
 * Simulate/Apply the optimal result of using the given "type" of attack
 */
static int borg_attack_aux(int what)
{
	int dam = 0, rad = 0;

	/* Analyze */
	switch (what)
	{
		/* Physical attack */
		case BF_THRUST:
		{
			return (borg_attack_aux_thrust());
		}


		/* Missile attack */
		case BF_LAUNCH:
		{
			return (borg_attack_aux_launch());
		}


		/* Object attack */
		case BF_OBJECT:
		{
			return (borg_attack_aux_object());
		}


		/* Spell -- magic missile */
		case BF_SPELL_MAGIC_MISSILE:
		{
			dam = (3+((b_ptr->lev-1)/5))*(4+1)/2;
			return (borg_attack_aux_spell_bolt(0, 0, rad, dam, GF_MISSILE));
		}

		/* Spell -- electric bolt */
		case BF_SPELL_ELEC_BOLT:
		{
			dam = (3+((b_ptr->lev-5)/4))*(8+1)/2;
			return (borg_attack_aux_spell_bolt(1, 1, rad, dam, GF_ELEC));
		}

		/* Spell -- cold bolt */
		case BF_SPELL_COLD_BOLT:
		{
			dam = (5+((b_ptr->lev-5)/4))*(8+1)/2;
			return (borg_attack_aux_spell_bolt(1, 7, rad, dam, GF_COLD));
		}

		/* Spell -- fire bolt */
		case BF_SPELL_FIRE_BOLT:
		{
			dam = (8+((b_ptr->lev-5)/4))*(8+1)/2;
			return (borg_attack_aux_spell_bolt(2, 6, rad, dam, GF_FIRE));
		}

		/* Spell -- acid bolt */
		case BF_SPELL_ACID_BOLT:
		{
			dam = (6+((b_ptr->lev-5)/4))*(8+1)/2;
			return (borg_attack_aux_spell_bolt(8, 0, rad, dam, GF_ACID));
		}


		/* Spell -- light beam */
		case BF_SPELL_LITE_BEAM:
		{
			rad = -1;
			dam = (6*(8+1)/2);
			return (borg_attack_aux_spell_bolt(8, 0, rad, dam, GF_LITE_WEAK));
		}


		/* Spell -- stinking cloud */
		case BF_SPELL_POISON_BALL:
		{
			rad = 2;
			dam = (10 + (b_ptr->lev/2));
			return (borg_attack_aux_spell_bolt(0, 8, rad, dam, GF_POIS));
		}

		/* Spell -- cold ball */
		case BF_SPELL_COLD_BALL:
		{
			rad = 2;
			dam = (30 + b_ptr->lev);
			return (borg_attack_aux_spell_bolt(3, 0, rad, dam, GF_COLD));
		}

		/* Spell -- acid ball */
		case BF_SPELL_ACID_BALL:
		{
			rad = 2;
			dam = (40 + (b_ptr->lev/2));
			return (borg_attack_aux_spell_bolt(8, 2, rad, dam, GF_ACID));
		}

		/* Spell -- fire ball */
		case BF_SPELL_FIRE_BALL:
		{
			rad = 2;
			dam = (55 + b_ptr->lev);
			return (borg_attack_aux_spell_bolt(3, 4, rad, dam, GF_FIRE));
		}


		/* Spell -- poison storm */
		case BF_SPELL_POISON_STORM:
		{
			rad = 3;
			dam = (20 + (b_ptr->lev/2));
			return (borg_attack_aux_spell_bolt(8, 1, rad, dam, GF_POIS));
		}

		/* Spell -- cold storm */
		case BF_SPELL_COLD_STORM:
		{
			rad = 3;
			dam = (70 + b_ptr->lev);
			return (borg_attack_aux_spell_bolt(8, 3, rad, dam, GF_COLD));
		}

		/* Spell -- meteor storm */
		case BF_SPELL_METEOR_STORM:
		{
			rad = 3;
			dam = (65 + b_ptr->lev);
			return (borg_attack_aux_spell_bolt(8, 4, rad, dam, GF_METEOR));
		}

		/* Spell -- mana storm */
		case BF_SPELL_MANA_STORM:
		{
			rad = 3;
			dam = (300 + (b_ptr->lev * 2));
			return (borg_attack_aux_spell_bolt(8, 5, rad, dam, GF_MANA));
		}


		/* Prayer -- orb of draining */
		case BF_PRAYER_HOLY_ORB_BALL:
		{
			rad = ((b_ptr->lev >= 30) ? 3 : 2);
			dam = ((b_ptr->pclass == 2) ? 2 : 4);
			dam = (3*(8+1)/2 + b_ptr->lev + (b_ptr->lev/dam));
			return (borg_attack_aux_prayer_bolt(2, 1, rad, dam, GF_HOLY_ORB));
		}


		/* Wand -- elec bolt */
		case BF_ROD_ELEC_BOLT:
		{
			dam = (3*(8+1)/2);
			return (borg_attack_aux_rod_bolt(SV_ROD_ELEC_BOLT, rad, dam, GF_ELEC));
		}

		/* Wand -- cold bolt */
		case BF_ROD_COLD_BOLT:
		{
			dam = (5*(8+1)/2);
			return (borg_attack_aux_rod_bolt(SV_ROD_COLD_BOLT, rad, dam, GF_COLD));
		}

		/* Wand -- acid bolt */
		case BF_ROD_ACID_BOLT:
		{
			dam = (6*(8+1)/2);
			return (borg_attack_aux_rod_bolt(SV_ROD_ACID_BOLT, rad, dam, GF_ACID));
		}

		/* Wand -- fire bolt */
		case BF_ROD_FIRE_BOLT:
		{
			dam = (8*(8+1)/2);
			return (borg_attack_aux_rod_bolt(SV_ROD_FIRE_BOLT, rad, dam, GF_FIRE));
		}


		/* Spell -- light beam */
		case BF_ROD_LITE_BEAM:
		{
			rad = -1;
			dam = (6*(8+1)/2);
			return (borg_attack_aux_rod_bolt(SV_ROD_LITE, rad, dam, GF_LITE_WEAK));
		}


		/* Wand -- elec ball */
		case BF_ROD_ELEC_BALL:
		{
			rad = 2;
			dam = 32;
			return (borg_attack_aux_rod_bolt(SV_ROD_ELEC_BALL, rad, dam, GF_ELEC));
		}

		/* Wand -- acid ball */
		case BF_ROD_COLD_BALL:
		{
			rad = 2;
			dam = 48;
			return (borg_attack_aux_rod_bolt(SV_ROD_COLD_BALL, rad, dam, GF_COLD));
		}

		/* Wand -- acid ball */
		case BF_ROD_ACID_BALL:
		{
			rad = 2;
			dam = 60;
			return (borg_attack_aux_rod_bolt(SV_ROD_ACID_BALL, rad, dam, GF_ACID));
		}

		/* Wand -- fire ball */
		case BF_ROD_FIRE_BALL:
		{
			rad = 2;
			dam = 72;
			return (borg_attack_aux_rod_bolt(SV_ROD_FIRE_BALL, rad, dam, GF_FIRE));
		}


		/* Wand -- magic missile */
		case BF_WAND_MAGIC_MISSILE:
		{
			dam = (2*(6+1)/2);
			return (borg_attack_aux_wand_bolt(SV_WAND_MAGIC_MISSILE, rad, dam, GF_MISSILE));
		}

		/* Wand -- elec bolt */
		case BF_WAND_ELEC_BOLT:
		{
			dam = (3*(8+1)/2);
			return (borg_attack_aux_wand_bolt(SV_WAND_ELEC_BOLT, rad, dam, GF_ELEC));
		}

		/* Wand -- cold bolt */
		case BF_WAND_COLD_BOLT:
		{
			dam = (3*(8+1)/2);
			return (borg_attack_aux_wand_bolt(SV_WAND_COLD_BOLT, rad, dam, GF_COLD));
		}

		/* Wand -- acid bolt */
		case BF_WAND_ACID_BOLT:
		{
			dam = (5*(8+1)/2);
			return (borg_attack_aux_wand_bolt(SV_WAND_ACID_BOLT, rad, dam, GF_ACID));
		}

		/* Wand -- fire bolt */
		case BF_WAND_FIRE_BOLT:
		{
			dam = (6*(8+1)/2);
			return (borg_attack_aux_wand_bolt(SV_WAND_FIRE_BOLT, rad, dam, GF_FIRE));
		}


		/* Spell -- light beam */
		case BF_WAND_LITE_BEAM:
		{
			rad = -1;
			dam = (6*(8+1)/2);
			return (borg_attack_aux_wand_bolt(SV_WAND_LITE, rad, dam, GF_LITE_WEAK));
		}


		/* Wand -- stinking cloud */
		case BF_WAND_STINKING_CLOUD:
		{
			rad = 2;
			dam = 12;
			return (borg_attack_aux_wand_bolt(SV_WAND_STINKING_CLOUD, rad, dam, GF_POIS));
		}

		/* Wand -- elec ball */
		case BF_WAND_ELEC_BALL:
		{
			rad = 2;
			dam = 32;
			return (borg_attack_aux_wand_bolt(SV_WAND_ELEC_BALL, rad, dam, GF_ELEC));
		}

		/* Wand -- acid ball */
		case BF_WAND_COLD_BALL:
		{
			rad = 2;
			dam = 48;
			return (borg_attack_aux_wand_bolt(SV_WAND_COLD_BALL, rad, dam, GF_COLD));
		}

		/* Wand -- acid ball */
		case BF_WAND_ACID_BALL:
		{
			rad = 2;
			dam = 60;
			return (borg_attack_aux_wand_bolt(SV_WAND_ACID_BALL, rad, dam, GF_ACID));
		}

		/* Wand -- fire ball */
		case BF_WAND_FIRE_BALL:
		{
			rad = 2;
			dam = 72;
			return (borg_attack_aux_wand_bolt(SV_WAND_FIRE_BALL, rad, dam, GF_FIRE));
		}

		/* Wand -- dragon cold */
		case BF_WAND_DRAGON_COLD:
		{
			rad = 3;
			dam = 80;
			return (borg_attack_aux_wand_bolt(SV_WAND_DRAGON_COLD, rad, dam, GF_COLD));
		}

		/* Wand -- dragon fire */
		case BF_WAND_DRAGON_FIRE:
		{
			rad = 3;
			dam = 100;
			return (borg_attack_aux_wand_bolt(SV_WAND_DRAGON_FIRE, rad, dam, GF_FIRE));
		}


		/* Dragon scale mail -- lightning */
		case BF_DRAGON_BLUE:
		{
			rad = 2;
			dam = 100;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_BLUE, rad, dam, GF_ELEC));
		}

		/* Dragon scale mail -- frost */
		case BF_DRAGON_WHITE:
		{
			rad = 2;
			dam = 110;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_WHITE, rad, dam, GF_COLD));
		}

		/* Dragon scale mail -- acid */
		case BF_DRAGON_BLACK:
		{
			rad = 2;
			dam = 130;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_BLACK, rad, dam, GF_ACID));
		}

		/* Dragon scale mail -- poison */
		case BF_DRAGON_GREEN:
		{
			rad = 2;
			dam = 150;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_GREEN, rad, dam, GF_POIS));
		}

		/* Dragon scale mail -- fire */
		case BF_DRAGON_RED:
		{
			rad = 2;
			dam = 200;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_RED, rad, dam, GF_FIRE));
		}

#if 0
		/* Dragon scale mail -- lightning or frost or acid or poison or fire */
		case BF_DRAGON_MULTIHUED:
		{
			rad = 2;
			dam = 250;
			ouch = GF_ELEC = GF_COLD = GF_ACID = GF_POIS = GF_FIRE = 0;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_MULTIHUED, rad, dam, ouch));
		}
#endif

		/* Dragon scale mail -- confusion */
		case BF_DRAGON_BRONZE:
		{
			rad = 2;
			dam = 120;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_BRONZE, rad, dam, GF_CONFUSION));
		}

		/* Dragon scale mail -- sound */
		case BF_DRAGON_GOLD:
		{
			rad = 2;
			dam = 130;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_GOLD, rad, dam, GF_SOUND));
		}

#if 0
		/* Dragon scale mail -- chaos or disenchantment */
		case BF_DRAGON_CHAOS:
		{
			rad = 2;
			dam = 220;
			ouch = GF_CHAOS = GF_DISENCHANT = 0;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_CHAOS, rad, dam, ouch));
		}
#endif

#if 0
		/* Dragon scale mail -- sound or shards */
		case BF_DRAGON_LAW:
		{
			rad = 2;
			dam = 230;
			ouch = GF_SOUND = GF_SHARD = 0;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_LAW, rad, dam, ouch));
		}
#endif

#if 0
		/* Dragon scale mail -- sound or shards or chaos or disenchantment */
		case BF_DRAGON_BALANCE:
		{
			rad = 2;
			dam = 250;
			ouch = GF_SOUND = GF_SHARD = GF_CHAOS = GF_DISENCHANT = 0;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_BALANCE, rad, dam, ouch));
		}
#endif

#if 0
		/* Dragon scale mail -- light or darkness */
		case BF_DRAGON_SHINING:
		{
			rad = 2;
			dam = 200;
			ouch = GF_LITE = GF_DARK = 0;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_SHINING, rad, dam, ouch));
		}
#endif

		/* Dragon scale mail -- the elements */
		case BF_DRAGON_POWER:
		{
			rad = 2;
			dam = 300;
			return (borg_attack_aux_dragon_bolt(SV_DRAGON_POWER, rad, dam, GF_MISSILE));
		}


		/* Artifact ring */
		case BF_ART_NARYA:
		{
			rad = 3;
			dam = 120;
			return (borg_attack_aux_artifact_bolt(ART_NARYA, rad, dam, GF_FIRE));
		}

		/* Artifact ring */
		case BF_ART_NENYA:
		{
			rad = 3;
			dam = 200;
			return (borg_attack_aux_artifact_bolt(ART_NENYA, rad, dam, GF_COLD));
		}

		/* Artifact ring */
		case BF_ART_VILYA:
		{
			rad = 3;
			dam = 250;
			return (borg_attack_aux_artifact_bolt(ART_VILYA, rad, dam, GF_ELEC));
		}

		/* Artifact gloves */
		case BF_ART_CAMMITHRIM:
		{
			rad = 1;
			dam = (2*(6+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_CAMMITHRIM, rad, dam, GF_MISSILE));
		}

		/* Artifact gloves */
		case BF_ART_PAURHACH:
		{
			rad = 1;
			dam = (9*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_PAURHACH, rad, dam, GF_FIRE));
		}

		/* Artifact gloves */
		case BF_ART_PAURNIMMEN:
		{
			rad = 1;
			dam = (6*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_PAURNIMMEN, rad, dam, GF_COLD));
		}

		/* Artifact gloves */
		case BF_ART_PAURAEGEN:
		{
			rad = 1;
			dam = (4*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_PAURAEGEN, rad, dam, GF_ELEC));
		}

		/* Artifact gloves */
		case BF_ART_PAURNEN:
		{
			rad = 1;
			dam = (5*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_PAURNEN, rad, dam, GF_ACID));
		}

		/* Artifact gloves */
		case BF_ART_FINGOLFIN:
		{
			rad = 1;
			dam = 150;
			return (borg_attack_aux_artifact_bolt(ART_FINGOLFIN, rad, dam, GF_ARROW));
		}

		/* Artifact weapon */
		case BF_ART_NARTHANC:
		{
			rad = 3;
			dam = (9*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_NARTHANC, rad, dam, GF_FIRE));
		}

		/* Artifact weapon */
		case BF_ART_NIMTHANC:
		{
			rad = 3;
			dam = (6*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_NIMTHANC, rad, dam, GF_COLD));
		}

		/* Artifact weapon */
		case BF_ART_DETHANC:
		{
			rad = 3;
			dam = (4*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_DETHANC, rad, dam, GF_ELEC));
		}

		/* Artifact weapon */
		case BF_ART_RILIA:
		{
			rad = 3;
			dam = 12;
			return (borg_attack_aux_artifact_bolt(ART_RILIA, rad, dam, GF_POIS));
		}

		/* Artifact weapon */
		case BF_ART_BELANGIL:
		{
			rad = 2;
			dam = 48;
			return (borg_attack_aux_artifact_bolt(ART_BELANGIL, rad, dam, GF_COLD));
		}

		/* Artifact weapon */
		case BF_ART_ARUNRUTH:
		{
			rad = 1;
			dam = (12*(8+1)/2);
			return (borg_attack_aux_artifact_bolt(ART_ARUNRUTH, rad, dam, GF_COLD));
		}

		/* Artifact weapon */
		case BF_ART_RINGIL:
		{
			rad = 2;
			dam = 100;
			return (borg_attack_aux_artifact_bolt(ART_RINGIL, rad, dam, GF_COLD));
		}

		/* Artifact weapon */
		case BF_ART_ANDURIL:
		{
			rad = 2;
			dam = 72;
			return (borg_attack_aux_artifact_bolt(ART_ANDURIL, rad, dam, GF_FIRE));
		}

		/* Artifact weapon */
		case BF_ART_THEODEN:
		{
			rad = 1;
			dam = 120;
			return (borg_attack_aux_artifact_bolt(ART_THEODEN, rad, dam, GF_OLD_DRAIN));
		}

		/* Artifact weapon */
		case BF_ART_AEGLOS:
		{
			rad = 2;
			dam = 100;
			return (borg_attack_aux_artifact_bolt(ART_AEGLOS, rad, dam, GF_COLD));
		}

		/* Artifact weapon */
		case BF_ART_FIRESTAR:
		{
			rad = 3;
			dam = 72;
			return (borg_attack_aux_artifact_bolt(ART_FIRESTAR, rad, dam, GF_FIRE));
		}

		/* Artifact weapon */
		case BF_ART_TURMIL:
		{
			rad = 1;
			dam = 90;
			return (borg_attack_aux_artifact_bolt(ART_TURMIL, rad, dam, GF_OLD_DRAIN));
		}
	}

	/* Oops */
	return (0);
}


/*
 * Attack nearby monsters, in the best possible way, if any.
 *
 * We consider a variety of possible attacks, including physical attacks
 * on adjacent monsters, missile attacks on nearby monsters, spell/prayer
 * attacks on nearby monsters, and wand/rod attacks on nearby monsters.
 *
 * Basically, for each of the known "types" of attack, we "simulate" the
 * "optimal" result of using that attack, and then we "apply" the "type"
 * of attack which appears to have the "optimal" result.
 *
 * When calculating the "result" of using an attack, we only consider the
 * effect of the attack on visible, on-screen, known monsters, which are
 * within 16 grids of the player.  This prevents most "spurious" attacks,
 * but we can still be fooled by situations like creeping coins which die
 * while out of sight, leaving behind a pile of coins, which we then find
 * again, and attack with distance attacks, which have no effect.  Perhaps
 * we should "expect" certain results, and take note of failure to observe
 * those effects.  XXX XXX XXX
 *
 * See above for the "semantics" of each "type" of attack.
 */
bool borg_attack(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, j, x, y;

	int n, b_n = 0;
	int a, b_a = -1;

	int cy, cx;

	/* Track the potential targets */
	bool borg_cave_mark[DUNGEON_HGT][DUNGEON_WID];

	/* No targets yet */
	C_WIPE(borg_cave_mark, DUNGEON_HGT * DUNGEON_WID, bool);

	/* Nobody around */
	if (!borg_kills_cnt) return (FALSE);


	/* Reset list */
	borg_temp_n = 0;

	/* Find "nearby" monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill;

		/* Monster */
		kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Require current knowledge */
		if (kill->when < borg_time) continue;

		/* Ignore multiplying monsters if needed */
		if (borg_ignoring && !borg_base_is_afraid &&
			 (r_info[kill->r_idx].flags2 & RF2_MULTIPLY)) continue;

		/* Acquire location */
		x = kill->x;
		y = kill->y;

		/* Never shoot through walls */
		if (!(borg_cave_info[y][x] & (CAVE_VIEW))) continue;

		/* Check the distance XXX XXX XXX */
		if (distance(py, px, y, x) > 16) continue;

		/* Never shoot off-screen */
		if (!borg_panel_contains(y, x)) continue;

		/* Save the location (careful) */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;

		/* Mark this target */
		borg_cave_mark[y][x] = TRUE;

		/* Save nearby locations */
		for (j = 0; j < 8; j++)
		{
			cy = ddy_ddd[j] + y;
			cx = ddx_ddd[j] + x;

			/* Ignore previous targets */
			if (borg_cave_mark[cy][cx]) continue;

			/* Handle overflow by forgetting new target */
			if (borg_temp_n == BORG_TEMP_MAX) borg_temp_n--;

			/* Save the location (careful) */
			borg_temp_x[borg_temp_n] = cx;
			borg_temp_y[borg_temp_n] = cy;
			borg_temp_n++;

			/* Mark this target */
			borg_cave_mark[cy][cx] = TRUE;
		}
	}

	/* No destinations */
	if (!borg_temp_n) return (FALSE);


	/* Simulate */
	borg_simulate = TRUE;

	/* Analyze the possible attacks */
	for (a = 0; a < BF_MAX; a++)
	{
		/* Simulate */
		n = borg_attack_aux(a);

		/* Track "best" attack */
		if (n <= b_n) continue;

		/* Track best */
		b_a = a;
		b_n = n;
	}

	/* Nothing good */
	if (b_n <= 0) return (FALSE);


	/* Note */
	borg_note(format("# Performing attack type %d with value %d", b_a, b_n));

	/* Instantiate */
	borg_simulate = FALSE;

	/* Instantiate */
	(void)borg_attack_aux(b_a);

	/* Success */
	return (TRUE);
}





/*
 * Attempt to recover from damage and such after a battle
 *
 * Note that resting while in danger is counter-productive, unless
 * the danger is low, in which case it may induce "farming".
 *
 * Note that resting while recall is active will often cause you
 * to lose hard-won treasure from nasty monsters, so we disable
 * resting when waiting for recall in the dungeon near objects.
 *
 * First we try spells/prayers, which are "free", and then we
 * try food, potions, scrolls, staffs, rods, artifacts, etc.
 *
 * XXX XXX XXX
 * Currently, we use healing spells as if they were "free", but really,
 * this is only true if the "danger" is less than the "reward" of doing
 * the healing spell, and if there are no monsters which might soon step
 * around the corner and attack.
 */
bool borg_recover(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int p, q;


	/*** Handle annoying situations ***/

	/* Refuel current torch */
	if ((borg_items[INVEN_LITE].tval == TV_LITE) &&
	    (borg_items[INVEN_LITE].sval == SV_LITE_TORCH))
	{
		/* Refuel the torch if needed */
		if (borg_items[INVEN_LITE].pval < 2500)
		{
			if (borg_refuel_torch()) return (TRUE);
		}
	}

	/* Refuel current lantern */
	if ((borg_items[INVEN_LITE].tval == TV_LITE) &&
	    (borg_items[INVEN_LITE].sval == SV_LITE_LANTERN))
	{
		/* Refuel the lantern if needed */
		if (borg_items[INVEN_LITE].pval < 5000)
		{
			if (borg_refuel_lantern()) return (TRUE);
		}
	}


	/*** Do not recover when in danger ***/

	/* Look around for danger */
	p = borg_danger(py, px, 1);

	/* Never recover in dangerous situations */
	if (p > borg_avoid / 2) return (FALSE);


	/*** Roll for "paranoia" ***/

	/* Base roll */
	q = rand_int(100);

	/* Half dead */
	if (b_ptr->chp < b_ptr->mhp / 2) q = q - 10;

	/* Almost dead */
	if (b_ptr->chp < b_ptr->mhp / 4) q = q - 10;


	/*** Use "cheap" cures ***/

	/* Hack -- cure stun */
	if (borg_base_is_stun && (q < 75))
	{
		if (borg_prayer(2, 7) ||
		    borg_prayer(3, 2) ||
		    borg_prayer(6, 1) ||
		    borg_prayer(6, 2))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure cuts */
	if (borg_base_is_cut && (q < 75))
	{
		if (borg_prayer(2, 2) ||
		    borg_prayer(2, 7) ||
		    borg_prayer(3, 2) ||
		    borg_prayer(6, 0) ||
		    borg_prayer(6, 1) ||
		    borg_prayer(6, 2))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure poison */
	if (borg_base_is_poisoned && (q < 75))
	{
		if (borg_spell(1, 4) ||
		    borg_prayer(2, 0))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure fear */
	if (borg_base_is_afraid && (q < 75))
	{
		if (borg_spell(7, 2) ||
		    borg_spell(7, 0) ||
		    borg_prayer(0, 3))
		{
			return (TRUE);
		}
	}

	/* Hack -- satisfy hunger */
	if (borg_base_is_hungry && (q < 75))
	{
		if (borg_spell(2, 0) ||
		    borg_prayer(1, 5))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal damage */
	if ((b_ptr->chp < b_ptr->mhp / 2) && (q < 75))
	{
		if (borg_spell(0, 5) ||
		    borg_prayer(3, 2) ||
		    borg_prayer(2, 7) ||
		    borg_prayer(2, 2) ||
		    borg_prayer(1, 2) ||
		    borg_prayer(6, 0) ||
		    borg_prayer(6, 1) ||
		    borg_prayer(6, 2))
		{
			return (TRUE);
		}
	}


	/*** Use "expensive" cures ***/

	/* Hack -- cure stun */
	if (borg_base_is_stun && (q < 25))
	{
		if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_use_staff(SV_STAFF_CURING) ||
		    borg_zap_rod(SV_ROD_CURING) ||
		    borg_zap_rod(SV_ROD_HEALING))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure cuts */
	if (borg_base_is_cut && (q < 25))
	{
		if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_use_staff(SV_STAFF_CURING) ||
		    borg_zap_rod(SV_ROD_CURING) ||
		    borg_zap_rod(SV_ROD_HEALING))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure poison */
	if (borg_base_is_poisoned && (q < 25))
	{
		if (borg_quaff_potion(SV_POTION_CURE_POISON) ||
		    borg_quaff_potion(SV_POTION_SLOW_POISON) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_eat_food(SV_FOOD_CURE_POISON) ||
		    borg_eat_food(SV_FOOD_WAYBREAD) ||
		    borg_use_staff(SV_STAFF_CURING) ||
		    borg_zap_rod(SV_ROD_CURING) ||
		    borg_activate_artifact(ART_DAL))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure blindness */
	if (borg_base_is_blind && (q < 25))
	{
		if (borg_eat_food(SV_FOOD_CURE_BLINDNESS) ||
		    borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
		    borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_use_staff(SV_STAFF_CURING) ||
		    borg_use_staff(SV_ROD_CURING))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure confusion */
	if (borg_base_is_confused && (q < 25))
	{
		if (borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
		    borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_use_staff(SV_STAFF_CURING) ||
		    borg_use_staff(SV_ROD_CURING))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure fear */
	if (borg_base_is_afraid && (q < 25))
	{
		if (borg_eat_food(SV_FOOD_CURE_PARANOIA) ||
		    borg_quaff_potion(SV_POTION_BOLDNESS) ||
		    borg_quaff_potion(SV_POTION_HEROISM) ||
		    borg_quaff_potion(SV_POTION_BESERK_STRENGTH) ||
		    borg_activate_artifact(ART_DAL))
		{
			return (TRUE);
		}
	}

	/* Hack -- satisfy hunger */
	if (borg_base_is_hungry && (q < 25))
	{
		if (borg_read_scroll(SV_SCROLL_SATISFY_HUNGER))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal damage */
	if ((b_ptr->chp < b_ptr->mhp / 2) && (q < 25))
	{
		if (borg_zap_rod(SV_ROD_HEALING) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_quaff_potion(SV_POTION_CURE_SERIOUS))
		{
			return (TRUE);
		}
	}


	/*** Just Rest ***/

	/* Hack -- rest until healed (unless in danger) */
	if ((borg_base_is_blind || borg_base_is_confused || borg_base_is_image ||
	     borg_base_is_poisoned || borg_base_is_afraid || borg_base_is_cut || borg_base_is_stun ||
	     (b_ptr->chp < b_ptr->mhp) || (b_ptr->csp < b_ptr->msp)) &&
	    (!b_ptr->depth || !borg_takes_cnt || !borg_recalling) &&
	    (p <= 0) &&
	    (rand_int(100) < 90))
	{
		/* XXX XXX XXX */

		/* Take note */
		borg_note("# Resting...");

		/* Send action (rest until done) */
		borg_keypress('R');
		borg_keypress('&');
		borg_keypress('\n');

		/* Done */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}







/*
 * Take one "step" towards the given location, return TRUE if possible
 *
 * Note that sometimes what we think is a monster is really an object.
 */
static bool borg_play_step(int y2, int x2)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int dir, x, y;

	byte feat;


	/* Get a direction, if possible */
	dir = borg_goto_dir(py, px, y2, x2);

	/* We have arrived */
	if (dir == 5) return (FALSE);


	/* Obtain the destination */
	x = px + ddx[dir];
	y = py + ddy[dir];


	/* Hack -- set goal */
	xb_ptr->gx = x;
	xb_ptr->gy = y;


	/* Monsters -- Attack */
	if (borg_cave_m_idx[y][x])
	{
		auto_kill *kill = &borg_kills[borg_cave_m_idx[y][x]];

		/* Message */
		borg_note(format("# Attacking a '%s' at (%d,%d)",
		                 r_name + r_info[kill->r_idx].name,
		                 kill->y, kill->x));

		/* Normally do a real attack */
		if (rand_int(100) < 90)
		{
			/* Send action (alter) */
			borg_keypress('+');

			/* Send direction */
			borg_keypress(I2D(dir));
		}

		/* Sometimes stagger */
		else
		{
			/* Send action (stagger) */
			borg_keypress(';');

			/* Send direction */
			borg_keypress(I2D(dir));
		}

		/* Done */
		return (TRUE);
	}


	/* Objects -- Take */
	if (borg_cave_o_idx[y][x])
	{
		auto_take *take = &borg_takes[borg_cave_o_idx[y][x]];

		/* Message */
		borg_note(format("# Walking onto a '%s' at (%d,%d)",
		                 k_name + k_info[take->k_idx].name,
		                 take->y, take->x));

		/* Send action (walk onto it) */
		borg_keypress(';');

		/* Send direction */
		borg_keypress(I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Get feature */
	feat = borg_cave_feat[y][x];


	/* Traps -- disarm */
	if ((feat >= FEAT_TRAP_HEAD) &&
	    (feat <= FEAT_TRAP_TAIL))
	{
		/* Message */
		borg_note("# Disarming a trap");

		/* Send action (disarm) */
		borg_keypress('D');

		/* Send direction */
		borg_keypress(I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Closed/Locked Doors -- Open */
	if ((feat >= FEAT_DOOR_HEAD) &&
	    (feat <= FEAT_DOOR_HEAD + 0x07))
	{
		/* Hack -- prevent infinite loops XXX XXX XXX */
		if (!rand_int(100)) return (FALSE);

		/* Message */
		borg_note("# Opening a door");

		/* Send count (9) */
		borg_keypresses("09");

		/* Send action (open) */
		borg_keypress('o');

		/* Send direction */
		borg_keypress(I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Jammed Doors -- Bash or destroy */
	if ((feat >= FEAT_DOOR_HEAD + 0x08) &&
	    (feat <= FEAT_DOOR_TAIL))
	{
		/* Hack -- prevent infinite loops XXX XXX XXX */
		if (!rand_int(100)) return (FALSE);

		/* Mega-Hack -- allow "destroy doors" */
		if (borg_prayer(7, 0))
		{
			borg_note("# Unbarring ways");

			/* Done */
			return (TRUE);
		}

		/* Send "destroy doors" */
		if (borg_spell(1, 2))
		{
			borg_note("# Destroying doors");

			/* Done */
			return (TRUE);
		}

		/* Send "stone to mud" */
		if (borg_spell(1, 8))
		{
			borg_note("# Melting a door");

			/* Send direction */
			borg_keypress(I2D(dir));

			/* Done */
			return (TRUE);
		}

		/* Message */
		borg_note("# Bashing a door");

		/* Send action (bash) */
		borg_keypress('B');

		/* Send direction */
		borg_keypress(I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Rubble, Treasure, Seams, Walls -- Tunnel or Melt */
	if (feat >= FEAT_SECRET)
	{
		/* Hack -- prevent infinite loops XXX XXX XXX */
		if (rand_int(100) < 10) return (FALSE);

		/* Send "stone to mud" */
		if (borg_spell(1, 8))
		{
			borg_note("# Melting a wall/etc");

			/* Send direction */
			borg_keypress(I2D(dir));

			/* Done */
			return (TRUE);
		}

		/* Message */
		borg_note("# Digging through wall/etc");

		/* Send count (99) */
		borg_keypresses("099");

		/* Send action (tunnel) */
		borg_keypress('T');

		/* Send direction */
		borg_keypress(I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Shops -- Enter */
	if ((feat >= FEAT_SHOP_HEAD) &&
	    (feat <= FEAT_SHOP_TAIL))
	{
		int shop = (feat - FEAT_SHOP_HEAD);

		/* Message */
		borg_note(format("# Entering a '%d' shop", shop + 1));

		/* Send action (walk into store) */
		borg_keypress(';');

		/* Send direction */
		borg_keypress(I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Send action (walk) */
	borg_keypress(';');

	/* Send direction */
	borg_keypress(I2D(dir));

	/* Done */
	return (TRUE);
}




/*
 * Act twitchy
 */
bool borg_twitchy(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int dir;

	/* This is a bad thing */
	borg_note("# Twitchy!");

	/* Pick a random direction */
	dir = randint(9);

	/* Hack -- set goal */
	xb_ptr->gx = px + ddx[dir];
	xb_ptr->gy = py + ddy[dir];

	/* Maybe alter */
	if (rand_int(100) < 10)
	{
		/* Send action (alter) */
		borg_keypress('+');

		/* Send direction */
		borg_keypress(I2D(dir));
	}

	/* Normally move */
	else
	{
		/* Send action (walk) */
		borg_keypress(';');

		/* Send direction */
		borg_keypress(I2D(dir));
	}
	
	/* Done */
	return (TRUE);
}





/*
 * Commit the current "flow"
 */
static bool borg_flow_commit(cptr who, int why)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int cost;

	/* Working cost of player grid */
	cost = borg_flow_work[py][px];

	/* Verify the total "cost" */
	if (cost >= 250) return (FALSE);

	/* Message */
	if (who) borg_note(format("# Flowing toward %s at cost %d", who, cost));

	/* Commit the working flow costs */
	C_COPY(borg_flow_cost, borg_flow_work, DUNGEON_HGT, byte_wid);

	/* Set the task */
	borg_task = why;

	/* Success */
	return (TRUE);
}





/*
 * Attempt to take an optimal step towards the current goal location
 *
 * Note that the "borg_update()" routine notices new monsters and objects,
 * and movement of monsters and objects, and cancels any flow in progress.
 *
 * Note that the "borg_update()" routine notices when a grid which was
 * not thought to block motion is discovered to in fact be a grid which
 * blocks motion, and removes that grid from any flow in progress, but
 * does not explicitly cancel any flow in progress.
 *
 * When given multiple alternative steps, this function attempts to choose
 * the "safest" path, by penalizing grids containing embedded gold, monsters,
 * rubble, doors, traps, store doors, and even floors.  This allows the Borg
 * to "step around" dangerous grids, even if this means extending the path
 * by a step or two, and encourages him to prefer grids such as objects and
 * stairs which are not only interesting but which are known not to be
 * invisible traps.
 *
 * This function needs some work.  It should attempt to analyze the "local
 * region" around the player and determine the optimal choice of locations
 * based on some useful computations, such as potential danger.  XXX XXX XXX
 *
 * If it works, return TRUE, otherwise, cancel the goal and return FALSE.
 */
bool borg_flow_old(int why)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int x, y;


	/* Continue */
	if (borg_task == why)
	{
		int b_n = 0;

		int i, b_i = -1;

		int c, b_c;


		/* Acquire cost of current grid */
		b_c = borg_flow_cost[py][px] * 16;


		/* Look around */
		for (i = 0; i < 8; i++)
		{
			byte feat;

			/* Grid in that direction */
			x = px + ddx_ddd[i];
			y = py + ddy_ddd[i];

			/* Acquire feature */
			feat = borg_cave_feat[y][x];

			/* Acquire cost of that grid */
			c = borg_flow_cost[y][x] * 16;

			/* Hack -- Penalize traps */
			if ((feat >= FEAT_TRAP_HEAD) &&
				 (feat <= FEAT_TRAP_TAIL))
			{
				c += 12;
			}

			/* Hack -- Penalize doors */
			else if ((feat >= FEAT_DOOR_HEAD) &&
						(feat <= FEAT_DOOR_TAIL))
			{
				c += 6;
			}

			/* Hack -- Prevent infinite loops */
			else
			{
				c += 1;
			}

			/* Never backtrack */
			if (c > b_c) continue;

			/* Notice new best value */
			if (c < b_c) b_n = 0;

			/* Apply the randomizer to equivalent values */
			if ((++b_n > 1) && (rand_int(b_n) != 0)) continue;

			/* Track it */
			b_i = i; b_c = c;
		}

		/* Try it */
		if (b_i >= 0)
		{
			/* Access the location */
			x = px + ddx_ddd[b_i];
			y = py + ddy_ddd[b_i];

			/* Attempt motion */
			if (borg_play_step(y, x)) return (TRUE);
		}

		/* Forget task */
		borg_task = 0;
	}

	/* Nothing to do */
	return (FALSE);
}




/*
 * Prepare to flee the level via stairs
 */
bool borg_flow_stair_both(int why)
{
	int i;

	/* None to flow to */
	if (!borg_track_less_num && !borg_track_more_num) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_track_less_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_track_less_y[i], borg_track_less_x[i]);
	}

	/* Enqueue useful grids */
	for (i = 0; i < borg_track_more_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_track_more_y[i], borg_track_more_x[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("stairs", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}




/*
 * Prepare to flow towards "up" stairs
 */
bool borg_flow_stair_less(int why)
{
	int i;

	/* None to flow to */
	if (!borg_track_less_num) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_track_less_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_track_less_y[i], borg_track_less_x[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("up-stairs", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to flow towards "down" stairs
 */
bool borg_flow_stair_more(int why)
{
	int i;

	/* None to flow to */
	if (!borg_track_more_num) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_track_more_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_track_more_y[i], borg_track_more_x[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("down-stairs", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards any non-visited shop
 */
bool borg_flow_shop_visit(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, x, y;

	/* Must be in town */
	if (b_ptr->depth) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Visit the shops */
	for (i = 0; i < 8; i++)
	{
		/* Must not be visited */
		if (borg_shops[i].when) continue;

		/* Obtain the location */
		x = borg_track_shop_x[i];
		y = borg_track_shop_y[i];

		/* Hack -- Must be known and not under the player */
		if (!x || !y || ((px == x) && (py == y))) continue;

		/* Enqueue the grid */
		borg_flow_enqueue_grid(y, x);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("shops", GOAL_MISC)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_MISC)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards a specific shop entry
 */
bool borg_flow_shop_entry(int i)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int x, y;

	cptr name = (f_name + f_info[0x08+i].name);

	/* Must be in town */
	if (b_ptr->depth) return (FALSE);

	/* Obtain the location */
	x = borg_track_shop_x[i];
	y = borg_track_shop_y[i];

	/* Hack -- Must be known */
	if (!x || !y) return (FALSE);

	/* Hack -- re-enter a shop */
	if ((x == px) && (y == py))
	{
		/* Note */
		borg_note("# Re-entering a shop");

		/* Send action (enter store) */
		borg_keypress('_');

		/* Success */
		return (TRUE);
	}

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue the grid */
	borg_flow_enqueue_grid(y, x);

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit(name, GOAL_MISC)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_MISC)) return (FALSE);

	/* Success */
	return (TRUE);
}






/*
 * Prepare to "flow" towards monsters to "kill"
 *
 * Note that monsters under the player are always deleted
 */
bool borg_flow_kill(bool viewable)
{
	int i, x, y, p;

	/* Efficiency -- Nothing to kill */
	if (!borg_kills_cnt) return (FALSE);

	/* Nothing found */
	borg_temp_n = 0;

	/* Scan the monster list */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Ignore multiplying monsters */
		if (borg_ignoring && !borg_base_is_afraid &&
			 (r_info[kill->r_idx].flags2 & RF2_MULTIPLY)) continue;

		/* Access the location */
		x = kill->x;
		y = kill->y;

		/* Require line of sight if requested */
		if (viewable && !(borg_cave_info[y][x] & (CAVE_VIEW))) continue;

		/* Calculate danger */
		p = borg_danger_aux(y, x, 1, i);

		/* Hack -- Skip "deadly" monsters */
		if (p > borg_avoid / 2) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing to kill */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Look for something to kill */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_temp_y[i], borg_temp_x[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit(NULL, GOAL_KILL)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_KILL)) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Prepare to "flow" towards objects to "take"
 *
 * Note that objects under the player are always deleted
 */
bool borg_flow_take(bool viewable)
{
	int i, x, y;

	/* Efficiency -- Nothing to take */
	if (!borg_takes_cnt) return (FALSE);


	/* Nothing yet */
	borg_temp_n = 0;

	/* Scan the object list */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		auto_take *take = &borg_takes[i];

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Access the location */
		x = take->x;
		y = take->y;

		/* Require line of sight if requested */
		if (viewable && !(borg_cave_info[y][x] & (CAVE_VIEW))) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing to take */
	if (!borg_temp_n) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Look for something to take */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_temp_y[i], borg_temp_x[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit(NULL, GOAL_TAKE)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_TAKE)) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Determine if a grid is "interesting" (and should be explored)
 *
 * A grid is "interesting" if it is a closed door, rubble, hidden treasure,
 * or a visible trap, or an "unknown" grid.
 */
static bool borg_flow_dark_interesting(int y, int x)
{
	int feat;


	/* Get the feature */
	feat = borg_cave_feat[y][x];


	/* Explore unknown grids */
	if (feat == FEAT_NONE) return (TRUE);


	/* Efficiency -- Ignore "boring" grids */
	if (feat < FEAT_TRAP_HEAD) return (FALSE);


	/* Efficiency -- Ignore secret doors */
	if (feat == FEAT_SECRET) return (FALSE);

	/* Efficiency -- Ignore other walls */
	if (feat >= FEAT_WALL_EXTRA) return (FALSE);


	/* Explore "closed doors" */
	if ((feat >= FEAT_DOOR_HEAD) &&
	    (feat <= FEAT_DOOR_TAIL))
	{
		return (TRUE);
	}


	/* Explore "rubble" */
	if (feat == FEAT_RUBBLE)
	{
		return (TRUE);
	}


	/* Explore "known treasure" */
	if ((feat == FEAT_MAGMA_K) ||
	    (feat == FEAT_QUARTZ_K))
	{
		/* Do not disarm when confused */
		if (borg_base_is_confused) return (FALSE);

		/* Allow "stone to mud" ability */
		if (borg_spell_legal(1, 8)) return (TRUE);

		/* Do not dig unless we appear strong enough to succeed XXX XXX XXX */
		if (b_ptr->skill_dig < ((feat & 0x01) ? 20 : 10) + 5) return (FALSE);

		/* Okay */
		return (TRUE);
	}


	/* Explore "visible traps" */
	if ((feat >= FEAT_TRAP_HEAD) &&
	    (feat <= FEAT_TRAP_TAIL))
	{
		/* Do not disarm when blind */
		if (borg_base_is_blind) return (FALSE);

		/* Do not disarm when confused */
		if (borg_base_is_confused) return (FALSE);

		/* Do not disarm when hallucinating */
		if (borg_base_is_image) return (FALSE);

		/* Do not disarm without lite */
		if (!b_ptr->cur_lite) return (FALSE);

		/* Do not disarm when clumsy */
		if (b_ptr->skill_dis < 15) return (FALSE);

		/* Okay */
		return (TRUE);
	}


	/* Ignore other grids */
	return (FALSE);
}


/*
 * Hack -- Prepare to "flow" directly towards a location
 *
 * Prepare a "flow" in such a way that from any point on the "projection"
 * path from the player to the given grid, as long as the rest of the path
 * is "safe" and "clear", we will walk along that path to the given grid.
 *
 * This function is used by several of the sub-functions called by the
 * "borg_flow_dark()" function to provide an extremely optimized local
 * "flow" during the initial exploration of a level.
 */
static errr borg_flow_direct(int y, int x)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int y1, x1;
	int y2, x2;

	int i, n;

	/* Projection flags */
	int flg = 0;

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	u16b path_g[512];


	/* Initial location */
	y1 = py;
	x1 = px;

	/* Final location */
	y2 = y;
	x2 = x;

	/* Obtain a local projection path (skip one entry) */
	path_n = borg_project_path(path_g+1, MAX_SIGHT, y1, x1, y2, x2, flg);

        /* Paranoia */
        if (!path_n) return (1);

	/* Add starting grid */
	path_g[0] = GRID(y1, x1);

	/* Increase path length */
	path_n++;

        /* Final location */
        y = GRID_Y(path_g[path_n-1]);
        x = GRID_X(path_g[path_n-1]);

	/* Make sure destination is reachable */
	if ((y != y2) || (x != x2)) return (1);

	/* Process the path (backwards) */
	for (i = path_n - 1; i >= 0; --i)
	{
		/* Location */
		y = GRID_Y(path_g[i]);
		x = GRID_X(path_g[i]);

		/* Analyze every grid once */
		if (borg_cave_danger[y][x] < 0)
		{
			borg_cave_danger[y][x] = borg_danger(y, x, 1);
		}

		/* Avoid dangerous grids */
		if (borg_cave_danger[y][x] > borg_avoid / 2) return (1);
	}

	/* Reset flow cost */
	n = 0;

	/* Process the path (backwards) */
	for (i = path_n - 1; i >= 0; --i)
	{
		/* Location */
		y = GRID_Y(path_g[i]);
		x = GRID_X(path_g[i]);

		/* Hack -- Stop path early if possible */
		if (borg_flow_work[y][x] <= n) break;

		/* Save the working flow cost, and advance flow cost */
		borg_flow_work[y][x] = n++;
	}

	/* Success */
	return (0);
}


/*
 * Hack -- mark off the edges of a rectangle as "avoid" or "allow"
 *
 * If the parameter "avoid" is TRUE, we will avoid the border, and
 * if it is FALSE, we will "allow" the border (assuming it is safe).
 */
static void borg_flow_border(int y1, int x1, int y2, int x2, bool avoid)
{
	int x, y;
	
	/* Hack -- Avoidance value */
	u16b v = (avoid ? 30000 : -1);

	/* Scan west/east edges */
	for (y = y1; y <= y2; y++)
	{
		/* Avoid/Clear west edge */
		borg_cave_danger[y][x1] = v;

		/* Avoid/Clear east edge */
		borg_cave_danger[y][x2] = v;
	}

	/* Scan north/south edges */
	for (x = x1; x <= x2; x++)
	{
		/* Avoid/Clear north edge */
		borg_cave_danger[y1][x] = v;

		/* Avoid/Clear south edge */
		borg_cave_danger[y2][x] = v;
	}
}


/*
 * Prepare to "flow" towards local "interesting" grids.
 *
 * This function checks the 8 adjacent grids, which are always legal,
 * and which are always viewable.
 *
 * This function handles the exploration of adjacent grids.
 *
 * This function uses a "direct" flow for efficiency.
 *
 * The "borg_temp_n" array is much larger than 8 grids.
 */
static bool borg_flow_dark_1(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i;

	int x, y;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Reset */
	borg_temp_n = 0;

	/* Scan adjacent grids */
	for (i = 0; i < 8; i++)
	{
		y = py + ddy_ddd[i];
		x = px + ddx_ddd[i];

		/* Skip "boring" grids (assume reachable) */
		if (!borg_flow_dark_interesting(y, x)) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Create paths to useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Create a direct path */
		(void)borg_flow_direct(y, x);
	}


	/* Attempt to Commit the flow */
	if (!borg_flow_commit(NULL, GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards local "interesting" grids.
 *
 * This function checks the 8 grids at the far edges and corners of the
 * local region, which are never lit by the torch, if they are legal, and
 * in line of sight.
 *
 * This function handles the exploration of most dark corridors and rooms.
 *
 * This function uses a "direct" flow for efficiency.
 *
 * The "borg_temp_n" array is much larger than 8 grids.
 */
static bool borg_flow_dark_2(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, r;

	int x, y;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Maximal radius */
	r = b_ptr->cur_lite + 1;


	/* Reset */
	borg_temp_n = 0;

	/* Eight directions */
	for (i = 0; i < 8; i++)
	{
		y = py + ddy_ddd[i] * r;
		x = px + ddx_ddd[i] * r;

		/* Require legal */
		if ((unsigned)(y) >= DUNGEON_HGT) continue;
		if ((unsigned)(x) >= DUNGEON_WID) continue;

		/* Require viewable */
		if (!(borg_cave_info[y][x] & (CAVE_VIEW))) continue;

		/* Skip "boring" grids */
		if (!borg_flow_dark_interesting(y, x)) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Create paths to useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Create a direct path */
		(void)borg_flow_direct(y, x);
	}


	/* Attempt to Commit the flow */
	if (!borg_flow_commit(NULL, GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards local "interesting" grids.
 *
 * This function checks the 81 grids which are "near" the player, if
 * they are fully inside the dungeon, and in line of sight.
 *
 * This function handles the exploration of the local region.
 *
 * This function uses a "direct" flow for efficiency.
 *
 * The "borg_temp_n" array is much larger than 81 grids.
 */
static bool borg_flow_dark_3(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i;

	int x, y;

	int x1, y1, x2, y2;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Local region */
	y1 = py - 4;
	x1 = px - 4;
	y2 = py + 4;
	x2 = px + 4;

	/* Restrict to "legal" grids */
	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;
	if (y2 > DUNGEON_HGT - 1) y2 = DUNGEON_HGT - 1;
	if (x2 > DUNGEON_WID - 1) x2 = DUNGEON_WID - 1;


	/* Reset */
	borg_temp_n = 0;

	/* Examine the region */
	for (y = y1; y <= y2; y++)
	{
		/* Examine the region */
		for (x = x1; x <= x2; x++)
		{
			/* Require viewable */
			if (!(borg_cave_info[y][x] & (CAVE_VIEW))) continue;

			/* Skip "boring" grids */
			if (!borg_flow_dark_interesting(y, x)) continue;

			/* Careful -- Remember it */
			borg_temp_x[borg_temp_n] = x;
			borg_temp_y[borg_temp_n] = y;
			borg_temp_n++;
		}
	}

	/* Nothing interesting */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Create a direct path */
		(void)borg_flow_direct(y, x);
	}


	/* Attempt to Commit the flow */
	if (!borg_flow_commit(NULL, GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards local "interesting" grids.
 *
 * This function checks the 81 grids which are "near" the player, if
 * they are legal, and directly adjacent to a viewable normal floor grid.
 *
 * This function handles the exploration of the local region, including
 * the exploration of most bending corridors, and some rooms.
 *
 * This function uses a "local" flow for efficiency.  This involves limiting
 * the "depth" of the flow to 5, and avoiding "unknown" grids when the flow
 * is being "spread".  In addition, we restrict the destination grids to
 * grids which are adjacent to a viewable floor grid.
 *
 * The "borg_temp_n" array is much larger than 81 grids.
 */
static bool borg_flow_dark_4(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, j;

	int x, y;

	int x1, y1, x2, y2;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Local region */
	y1 = py - 4;
	x1 = px - 4;
	y2 = py + 4;
	x2 = px + 4;

	/* Restrict to "legal" grids */
	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;
	if (y2 > DUNGEON_HGT - 1) y2 = DUNGEON_HGT - 1;
	if (x2 > DUNGEON_WID - 1) x2 = DUNGEON_WID - 1;


	/* Reset */
	borg_temp_n = 0;

	/* Examine the region */
	for (y = y1; y <= y2; y++)
	{
		/* Examine the region */
		for (x = x1; x <= x2; x++)
		{
			/* Skip "boring" grids */
			if (!borg_flow_dark_interesting(y, x)) continue;

			/* Scan simple neighbors */
			for (j = 0; j < 4; j++)
			{
				int y2 = y + ddy_ddd[j];
				int x2 = x + ddx_ddd[j];
		
				/* Require normal floor grids */
				if (borg_cave_feat[y2][x2] != FEAT_FLOOR) continue;

				/* Require line of sight */
				if (!(borg_cave_info[y2][x2] & (CAVE_VIEW))) continue;
				
				/* Careful -- Remember it */
				borg_temp_x[borg_temp_n] = x;
				borg_temp_y[borg_temp_n] = y;
				borg_temp_n++;

				/* Done */
				break;
			}
		}
	}

	/* Nothing interesting */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Enqueue the grid */
		borg_flow_enqueue_grid(y, x);
	}

	/* Spread the flow (limit depth) */
	borg_flow_spread(5, TRUE, TRUE);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit(NULL, GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids
 *
 * This function checks the 31*31 grids which are "near" the player, if
 * they are legal, and directly adjacent to a normal floor grid.
 *
 * We avoid paths that would take us into different panels by setting the
 * "icky" flag for the "border" grids to prevent path construction through
 * those grids, and clearing them when done, to prevent confusion elsewhere.
 * Note that these grids can still be used as the *destination* of the flow,
 * since they are enqueued *before* the borders are prevented.
 *
 * We also limit the flow to a depth of 32, though this probably does not
 * provide any extra efficiency.
 *
 * The "borg_temp_n" array is larger than 31*31 grids.
 */
static bool borg_flow_dark_7(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int y1, x1, y2, x2;

	int y, x, i, j;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Local region */
	y1 = py - 15;
	x1 = px - 15;
	y2 = py + 15;
	x2 = px + 15;

	/* Restrict to "legal" grids */
	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;
	if (y2 > DUNGEON_HGT - 1) y2 = DUNGEON_HGT - 1;
	if (x2 > DUNGEON_WID - 1) x2 = DUNGEON_WID - 1;

	/* Tighten borders */
	y1++; x1++; y2--; x2--;


	/* Nothing yet */
	borg_temp_n = 0;

	/* Examine the panel */
	for (y = y1; y <= y2; y++)
	{
		/* Examine the panel */
		for (x = x1; x <= x2; x++)
		{
			/* Skip "boring" grids */
			if (!borg_flow_dark_interesting(y, x)) continue;

			/* Scan simple neighbors */
			for (j = 0; j < 4; j++)
			{
				int y2 = y + ddy_ddd[j];
				int x2 = x + ddx_ddd[j];
		
				/* Require normal floor grids */
				if (borg_cave_feat[y2][x2] != FEAT_FLOOR) continue;

				/* Careful -- Remember it */
				borg_temp_x[borg_temp_n] = x;
				borg_temp_y[borg_temp_n] = y;
				borg_temp_n++;

				/* Done */
				break;
			}
		}
	}

	/* Nothing useful */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Enqueue the grid */
		borg_flow_enqueue_grid(y, x);
	}


	/* Expand borders */
	y1--; x1--; y2++; x2++;

	/* Avoid the edges */
	borg_flow_border(y1, x1, y2, x2, TRUE);

	/* Spread the flow (limit depth) */
	borg_flow_spread(32, TRUE, TRUE);

	/* Clear the edges */
	borg_flow_border(y1, x1, y2, x2, FALSE);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit("dark-7", GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids
 *
 * This function all the grids, if they are legal, and adjacent to any
 * known floor grid in any direction.
 *
 * This function takes care of any "interesting" grid not explored by
 * the routines above, as long as there is a safe path to the grid of
 * length less than 250.
 *
 * The "borg_temp_n" array may not be large enough to hold all the grids,
 * so we must check it before each insertion.
 */
static bool borg_flow_dark_8(void)
{
	int y, x, i, j;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Nothing yet */
	borg_temp_n = 0;

	/* Examine every "legal" grid */
	for (y = 1; y < DUNGEON_HGT-1; y++)
	{
		for (x = 1; x < DUNGEON_WID-1; x++)
		{
			/* Skip "boring" grids */
			if (!borg_flow_dark_interesting(y, x)) continue;

			/* Scan all neighbors */
			for (j = 0; j < 8; j++)
			{
				int y2 = y + ddy_ddd[j];
				int x2 = x + ddx_ddd[j];

				/* Skip unknown grids (important) */
				if (borg_cave_feat[y2][x2] == FEAT_NONE) continue;

				/* Skip non-floor grids (important) */
				if (!borg_cave_floor_bold(y2, x2)) continue;

				/* Careful -- Remember it */
				borg_temp_x[borg_temp_n] = x;
				borg_temp_y[borg_temp_n] = y;
				borg_temp_n++;

				/* Hack -- Handle overflow */
				if (borg_temp_n == BORG_TEMP_MAX)
				{
					/* Full break */
					y = DUNGEON_HGT;
					x = DUNGEON_WID;
				}

				/* Done */
				break;
			}
		}
	}

	/* Nothing useful */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Enqueue the grid */
		borg_flow_enqueue_grid(y, x);
	}

	/* Spread the flow */
	borg_flow_spread(250, TRUE, TRUE);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit("dark-9", GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids
 *
 * The "exploration" routines are broken into "nearby" and "distant"
 * exploration, and each set is chosen via the flag below.
 */
bool borg_flow_dark(bool nearby)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	/* Paranoia */
	if (borg_flow_dark_interesting(py, px))
	{
		borg_oops("darkness");
		return (FALSE);
	}

	/* Near */
	if (nearby)
	{
		/* Method 1 */
		if (borg_flow_dark_1()) return (TRUE);

		/* Method 2 */
		if (borg_flow_dark_2()) return (TRUE);

		/* Method 3 */
		if (borg_flow_dark_3()) return (TRUE);

		/* Method 4 */
		if (borg_flow_dark_4()) return (TRUE);
	}

	/* Far */
	else
	{
		/* Method 7 */
		if (borg_flow_dark_7()) return (TRUE);

		/* Method 8 */
		if (borg_flow_dark_8()) return (TRUE);
	}

	/* Fail */
	return (FALSE);
}



/*
 * Hack -- spastic searching
 */

static byte spastic_x;
static byte spastic_y;



/*
 * Search carefully for secret doors and such
 */
bool borg_flow_spastic(bool bored)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int cost;

	int i, x, y, v;

	int b_x = px;
	int b_y = py;
	int b_v = -1;


	/* Hack -- not in town */
	if (!b_ptr->depth) return (FALSE);


	/* Not bored */
	if (!bored)
	{
		/* Look around for danger */
		int p = borg_danger(py, px, 1);

		/* Avoid searching when in danger */
		if (p > borg_avoid / 4) return (FALSE);
	}


	/* We have arrived */
	if ((spastic_x == px) && (spastic_y == py))
	{
		/* Cancel */
		spastic_x = 0;
		spastic_y = 0;

		/* Take note */
		borg_note(format("# Spastic Searching at (%d,%d)...", py, px));

		/* Count searching */
		for (i = 0; i < 9; i++)
		{
			/* Extract the location */
			int xx = px + ddx_ddd[i];
			int yy = py + ddy_ddd[i];

			/* Tweak -- Remember the search */
			if (borg_cave_search[yy][xx] < 100)
			{
				borg_cave_search[yy][xx] += 5;
			}
		}

		/* Send count (5) */
		borg_keypresses("05");

		/* Send action (search) */
		borg_keypress('s');

		/* Success */
		return (TRUE);
	}


	/* Reverse flow */
	borg_flow_reverse();

	/* Scan the entire map */
	for (y = 1; y < DUNGEON_HGT-1; y++)
	{
		for (x = 1; x < DUNGEON_WID-1; x++)
		{
			int feats[8];

			int wall = 0;
			int supp = 0;
			int diag = 0;


			/* Skip unknown grids */
			if (borg_cave_feat[y][x] == FEAT_NONE) continue;

			/* Skip walls/doors */
			if (!borg_cave_floor_bold(y, x)) continue;


			/* Get the working flow cost */
			cost = borg_flow_work[y][x];

			/* Skip "unreachable" grids */
			if (cost >= 250) continue;


			/* Tweak -- Limit total searches */
			if (borg_cave_search[y][x] >= 50) continue;

			/* Limit initial searches until bored */
			if (!bored && (borg_cave_search[y][x] > 5)) continue;

			/* Avoid searching detected sectors */
			if (borg_detect_door[y/11][x/33]) continue;


			/* Extract adjacent locations */
			for (i = 0; i < 8; i++)
			{
				int yy = y + ddy_ddd[i];
				int xx = x + ddx_ddd[i];
				
				feats[i] = borg_cave_feat[yy][xx];
			}


			/* Count possible door locations */
			for (i = 0; i < 4; i++)
			{
				if (feats[i] == FEAT_SECRET) wall++;
				if (feats[i] >= FEAT_WALL_EXTRA) wall++;
			}

			/* No possible secret doors */
			if (wall < 1) continue;


			/* Count supporting evidence for secret doors */
			for (i = 0; i < 4; i++)
			{
				/* Rubble */
				if (feats[i] == FEAT_RUBBLE) continue;

				/* Walls, Doors XXX XXX */
				if (((feats[i] >= FEAT_SECRET) && (feats[i] <= FEAT_PERM_SOLID)) ||
				    ((feats[i] == FEAT_OPEN) || (feats[i] == FEAT_BROKEN)) ||
				    ((feats[i] >= FEAT_DOOR_HEAD) && (feats[i] <= FEAT_DOOR_TAIL)))
				{
					supp++;
				}
			}

			/* Count supporting evidence for secret doors */
			for (i = 4; i < 8; i++)
			{
				/* Rubble */
				if (feats[i] == FEAT_RUBBLE) continue;

				/* Walls */
				if (feats[i] >= FEAT_SECRET) diag++;
			}

			/* No possible secret doors */
			if (diag < 2) continue;


			/* Tweak -- Reward walls, punish visitation and distance */
			v = (supp * 500) + (diag * 100) - (borg_cave_search[y][x] * 20) - (cost * 1);

			/* The grid is not searchable */
			if (v <= 0) continue;


			/* Tweak -- Minimal interest until bored */
			if (!bored && (v < 1500)) continue;


			/* Track "best" grid */
			if ((b_v >= 0) && (v < b_v)) continue;

			/* Save the data */
			b_v = v; b_x = x; b_y = y;
		}
	}

	/* Clear the flow codes */
	borg_flow_clear();

	/* Hack -- Nothing found */
	if (b_v < 0) return (FALSE);


	/* Memorize */
	spastic_x = b_x;
	spastic_y = b_y;


	/* Enqueue the grid */
	borg_flow_enqueue_grid(b_y, b_x);

	/* Spread the flow */
	borg_flow_spread(250, TRUE, FALSE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("spastic", GOAL_XTRA)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_XTRA)) return (FALSE);

	/* Success */
	return (TRUE);
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

