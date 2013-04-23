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
 *   ART_CUBRAGOL     simple -- brand bolts
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
			if (borg_cave_danger[y][x] > borg_avoid / borg_avoid_factor ) continue;


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
	if (borg_cave_danger[y][x] > borg_avoid / borg_avoid_factor) return;


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
			/* Write the borg map file if we are in the dungeon */
			if (b_ptr->depth >= dump_level) borg_write_map(FALSE);

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
    BF_LAUNCH_EGO,

	BF_OBJECT,

	BF_SPELL_MAGIC_MISSILE,
	BF_SPELL_ELEC_BOLT,
	BF_SPELL_COLD_BOLT,
	BF_SPELL_FIRE_BOLT,
	BF_SPELL_ACID_BOLT,

	BF_SPELL_LITE_BEAM,
	
	BF_SPELL_SLOW_MONSTER,
	BF_SPELL_CONFUSE_MONSTER,
	BF_SPELL_SLEEP_I,
    BF_SPELL_SLEEP_II,
    BF_SPELL_SLEEP_III,

	BF_SPELL_POISON_BALL,
	BF_SPELL_COLD_BALL,
	BF_SPELL_ACID_BALL,
	BF_SPELL_FIRE_BALL,

	BF_SPELL_POISON_STORM,
	BF_SPELL_COLD_STORM,
	BF_SPELL_METEOR_STORM,
	BF_SPELL_MANA_STORM,

	BF_PRAYER_HOLY_ORB_BALL,

	BF_PRAYER_DISPEL_UNDEAD1,
	BF_PRAYER_DISPEL_EVIL1,

	BF_PRAYER_DISPEL_UNDEAD2,
	BF_PRAYER_DISPEL_EVIL2,

	BF_STAFF_DISPEL_EVIL,
	BF_STAFF_POWER,
	BF_STAFF_HOLINESS,

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

	BF_ART_INGWE,

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
#if 0
	/* Lower the damage for low-level mages DvE*/
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 15))
	{
		dam = dam/5;
	}
#endif

	/* Limit damage to twice maximal hitpoints */
	if (dam > kill->power * 2) dam = kill->power * 2;

	/* Enhance the preceived damgage to summoner in order to influence the
     * choice of targets.
     */
    if ( (r_ptr->flags6 & RF6_S_KIN) ||
		 (r_ptr->flags6 & RF6_S_HI_DEMON) ||
		 (r_ptr->flags6 & RF6_S_MONSTER) ||
         (r_ptr->flags6 & RF6_S_MONSTERS) ||
         (r_ptr->flags6 & RF6_S_ANT) ||
         (r_ptr->flags6 & RF6_S_SPIDER) ||
         (r_ptr->flags6 & RF6_S_HOUND) ||
         (r_ptr->flags6 & RF6_S_HYDRA) ||
         (r_ptr->flags6 & RF6_S_ANGEL) ||
         (r_ptr->flags6 & RF6_S_DEMON) ||
         (r_ptr->flags6 & RF6_S_UNDEAD) ||
         (r_ptr->flags6 & RF6_S_DRAGON) ||
         (r_ptr->flags6 & RF6_S_HI_UNDEAD) ||
         (r_ptr->flags6 & RF6_S_WRAITH) ||
         (r_ptr->flags6 & RF6_S_UNIQUE) )
		   dam += ((dam * 3)/2);
	

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
		if (!kill->awake && (p > borg_avoid / borg_avoid_factor) && (d <= kill->power))
		{
			continue;
		}

		/* Calculate "danger" to player */
		p = borg_danger_aux(py, px, 4, m_idx);

		/* Reduce "bonus" of partial kills */
		if (d <= kill->curhp) p = p / 10;

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


	/* Simulation */
	if (borg_simulate) return (b_d);


	/* Save the location */
	xb_ptr->gx = borg_temp_x[b_i];
	xb_ptr->gy = borg_temp_y[b_i];

	/* Hack -- target the location */
	borg_target(xb_ptr->gy, xb_ptr->gx);

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
	borg_keypress((char)I2D(dir));

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
bool borg_target(int y, int x)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int x1, y1, x2, y2;

	/* Log */
	borg_note(format("# Targetting location (%d,%d)", y, x));

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

	/* Read monster hit points at next opportunity */
	borg_do_health_bar = TRUE;

	/* Done */
	return (TRUE);
}


/* Special arrow damage types -RML */
#define GF_ARROW_EGO_BASE_BORG  200


/*
 * Guess how much damage a spell attack will do to a monster
 *
 * We only handle the "standard" damage types, and the
 * special arrow types.
 *
 * We are paranoid about monster resistances
 *
 * We ignore "special" effects for now
 */
static int borg_launch_damage_one(int i, int dam, int typ)
{
	int p1, p2;

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

        /* Ego-arrow: hurt animal (x2) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_ANIMAL:
		{
            if (r_ptr->flags3 & RF3_ANIMAL) dam *= 2;
			break;
		}

        /* Ego-arrow: hurt evil (x2) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_EVIL:
		{
            if (r_ptr->flags3 & RF3_EVIL) dam *= 2;
			break;
		}

        /* Ego-arrow: hurt undead (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_UNDEAD:
		{
            if (r_ptr->flags3 & RF3_UNDEAD) dam *= 3;
			break;
		}

        /* Ego-arrow: hurt demon (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_DEMON:
		{
            if (r_ptr->flags3 & RF3_DEMON) dam *= 3;
			break;
		}

        /* Ego-arrow: hurt orc (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_ORC:
		{
            if (r_ptr->flags3 & RF3_ORC) dam *= 3;
			break;
		}

        /* Ego-arrow: hurt troll (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_TROLL:
		{
            if (r_ptr->flags3 & RF3_TROLL) dam *= 3;
			break;
		}

        /* Ego-arrow: hurt giant (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_GIANT:
		{
            if (r_ptr->flags3 & RF3_GIANT) dam *= 3;
			break;
		}

        /* Ego-arrow: hurt dragon (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_HURT_DRAGON:
		{
            if (r_ptr->flags3 & RF3_DRAGON) dam *= 3;
			break;
		}

        /* Ego-arrow: flame (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_FLAME:
		{
            if (!(r_ptr->flags3 & RF3_IM_FIRE)) dam *= 3;
			break;
		}

        /* Ego-arrow: frost (x3) */
        case GF_ARROW_EGO_BASE_BORG + EGO_FROST:
		{
            if (!(r_ptr->flags3 & RF3_IM_COLD)) dam *= 3;
			break;
		}

        /* Ego-arrow: wounding (no effect*/
        case GF_ARROW_EGO_BASE_BORG + EGO_WOUNDING:
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

		case GF_OLD_SLEEP:
        borg_sleep_spell = FALSE;
        p1 = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
        borg_sleep_spell = TRUE;
        p2 = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
        borg_sleep_spell = FALSE;
        dam= (p1-p2);
        if (r_ptr->flags3 & RF3_NO_SLEEP) dam = -999;
		if (r_ptr->flags1 & RF1_UNIQUE) dam = -999;
        if (kill->speed < r_ptr->speed ) dam = -0;
        if (kill->afraid) dam = -0;
        if (kill->confused) dam = -0;
        if (!kill->awake) dam = -0;
        if (kill->level > b_ptr->lev) dam = -0;
        break;

		case GF_OLD_SLOW:
        borg_slow_spell = FALSE;
        p1 = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
        borg_slow_spell = TRUE;
        p2 = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
        borg_slow_spell = FALSE;
        dam= (p1-p2);
        if (kill->speed < r_ptr->speed ) dam = -0;
        if (kill->afraid) dam = -0;
        if (kill->confused) dam = -0;
        if (!kill->awake) dam = -0;
        if (kill->level > b_ptr->lev+5) dam = -0;
        break;

		case GF_OLD_CONF:
        borg_confuse_spell = FALSE;
        p1 = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
        borg_confuse_spell = TRUE;
        p2 = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
        borg_confuse_spell = FALSE;
        dam= (p1-p2);
        if (kill->confused) dam = -0;
        if (kill->afraid) dam = -0;
        if (kill->speed < r_ptr->speed ) dam = -0;
        if (r_ptr->flags3 & RF3_NO_CONF) dam = -999;
        if (r_ptr->flags2 & RF2_MULTIPLY) dam = -0;
        if (!kill->awake) dam = -0;
        if (kill->level > b_ptr->lev) dam = -0;
        break;

		/* Banishment-- cast when in extreme danger (checked in borg_defense). */
        case GF_AWAY_EVIL:
        if (r_ptr->flags3 & RF3_EVIL)
        {
            /* damage is the danger of the baddie */
            dam = borg_danger_aux(b_ptr->py,b_ptr->px,2,i);
            /* try not teleport away uniques. */
            if (r_ptr->flags1 & RF1_UNIQUE)
                dam = -1000;
        }
        else
        {
            dam = -0;
        }
        break;


		/* Various */
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

	/* Limit damage to twice maximal hitpoints */
	if (dam > kill->power * 2) dam = kill->power * 2;

    /* Enhance the preceived damgage to summoner in order to influence the
     * choice of targets.
     */
    if ( (r_ptr->flags6 & RF6_S_KIN) ||
		 (r_ptr->flags6 & RF6_S_HI_DEMON) ||
		 (r_ptr->flags6 & RF6_S_MONSTER) ||
         (r_ptr->flags6 & RF6_S_MONSTERS) ||
         (r_ptr->flags6 & RF6_S_ANT) ||
         (r_ptr->flags6 & RF6_S_SPIDER) ||
         (r_ptr->flags6 & RF6_S_HOUND) ||
         (r_ptr->flags6 & RF6_S_HYDRA) ||
         (r_ptr->flags6 & RF6_S_ANGEL) ||
         (r_ptr->flags6 & RF6_S_DEMON) ||
         (r_ptr->flags6 & RF6_S_UNDEAD) ||
         (r_ptr->flags6 & RF6_S_DRAGON) ||
         (r_ptr->flags6 & RF6_S_HI_UNDEAD) ||
         (r_ptr->flags6 & RF6_S_WRAITH) ||
         (r_ptr->flags6 & RF6_S_UNIQUE) )
       dam += ((dam * 3)/2);

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

	int d, p, x, y;

	auto_kill *kill;


	/* Monster */
	kill = &borg_kills[i];

	/* Skip dead monsters */
	if (!kill->r_idx) return (0);

	/* Require current knowledge */
	if (kill->when < borg_time) return (0);

	/* Acquire location */
	x = kill->x;
	y = kill->y;

	/* Never shoot walls/doors */
	if (!borg_cave_floor_bold(y, x)) return (0);

	/* Hack -- Unknown grids should be avoided some of the time */
	if ((borg_cave_feat[y][x] == FEAT_NONE) && ((borg_time % 8) == 0)) return (0);

	/* Hack -- Weird grids should be avoided some of the time */
	if ((borg_cave_feat[y][x] == FEAT_INVIS) && ((borg_time % 8) == 0)) return (0);

	/* Calculate damage */
	d = borg_launch_damage_one(i, dam, typ);

	/* No damage */
	if (d <= 0) return (0);

	/* Calculate danger */
	p = borg_danger_aux(y, x, 1, i);

	/* Hack -- avoid waking most "hard" sleeping monsters */
	if (!kill->awake && (p > borg_avoid / borg_avoid_factor) && (d <= kill->power))
	{
		return (-999);
	}

	/* Hack -- ignore "easy" / "sleeping" town monsters */
	if (!b_ptr->depth && (!kill->awake || (p <= 0)))
	{
		return (0);
	}

	/* Hack -- Low level Mages should not attack town monsters DvE */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 10) && (!b_ptr->depth))
	{
		return (-999);
	}

	/* Calculate "danger" to player */
	p = borg_danger_aux(py, px, 4, i);

	/* Reduce "bonus" of partial kills */
	if (d <= kill->power) p = p / 10;

	/* Add in power */
	d += p;

	/* Result */
	return (d);
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

	int i;

	int x1, y1;
	int x2, y2;

	int r, n;

	int m_idx;

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

		/* Collect damage (bolts/beams) */
		if (rad <= 0) n += borg_launch_bolt_aux_hack(m_idx, dam, typ);

		/* Avoid non-terminal monsters (bolts) */
		if ((rad == 0) && (i != path_n - 1) && m_idx) return (0);

		/* Avoid unknown grids (see above) */
		if (borg_cave_feat[y][x] == FEAT_NONE) return (0);

		/* Avoid weird grids (see above) */
		if (borg_cave_feat[y][x] == FEAT_INVIS) return (0);
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

		/* Collect damage, lowered by distance */
		n += borg_launch_bolt_aux_hack(m_idx, dam / (r + 1), typ);
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


	/* Examine possible destinations */
	for (i = 0; i < borg_temp_n; i++)
	{
		int x = borg_temp_x[i];
		int y = borg_temp_y[i];

		/* Consider it */
		n = borg_launch_bolt_aux(y, x, rad, dam, typ, max);

		/* Skip useless attacks */
		if (n <= 0) continue;

		/* Collect best attack */
		if ((b_i >= 0) && (n < b_n)) continue;

		/* Hack -- reset chooser */
		if ((b_i >= 0) && (n > b_n)) num = 0;

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

        /* Skip ego-missiles */
        if (item->name2) continue;

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
 * Simulate/Apply the optimal result of launching an ego-missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target,
 * for each ego-type  -RML
 */
static int borg_attack_aux_launch_ego(void)
{
    int n, b_n = -1;

	int k, b_k = -1;
    int track_k = -1;
	int d, b_d = -1;
    int track_d = -1;

    int name2, best_name2 = -1;

	auto_item *bow = &borg_items[INVEN_BOW];


	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);


    for (name2 = EGO_HURT_ANIMAL; name2 < EGO_BACKBITING; name2++)
    {
        b_k = -1;
        b_d = -1;

        /* Scan the pack */
        for (k = 0; k < INVEN_PACK; k++)
        {
            auto_item *item = &borg_items[k];
    
            /* Skip empty items */
            if (!item->iqty) continue;

            /* Skip bad missiles */
            if (item->tval != b_ptr->ammo_tval) continue;

            /* Skip ego-missiles of the wrong type */
            if (item->name2 != name2) continue;

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
        if (b_k < 0) continue;
    

        /* Choose optimal location */
        n = borg_launch_bolt(0, b_d, GF_ARROW_EGO_BASE_BORG + name2, MAX_RANGE);

        /* Cost more for better missiles */
        n = n - borg_items[b_k].value / 25;

        /* Ignore worse damage */
        if (n <= b_n) continue;

        /* Track best missile */
        b_n = n; best_name2 = name2;
        track_k = b_k; track_d = b_d;

    }

    /* Nothing to use */
    if (track_k < 0) return (0);

	/* Simulation */
	if (borg_simulate) return (b_n);


	/* Do it */
    borg_note(format("# Firing ego-missile '%s'",
                     borg_items[track_k].desc));

    /* Choose optimal location */
    n = borg_launch_bolt(0, track_d, GF_ARROW_EGO_BASE_BORG + best_name2, MAX_RANGE);

	/* Send action (fire) */
	borg_keypress('f');

	/* Send item index */
    borg_send_item_index(track_k);

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


	/* Paranoia */
	if (borg_simulate && (rand_int(100) < 10)) return (0);


	/* Require ability (right now) */
	if (!borg_spell_okay(book, what)) return (0);


	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Penalize mana usage */
	b_n = b_n - as->power;

	/* Penalize use of reserve mana (not for low level char's) */
	if (b_ptr->lev < 20) 
	{
		if (b_ptr->csp - as->power < b_ptr->msp / 2) b_n = b_n - as->power * 10;
	}

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


	/* Paranoia */
	if (borg_simulate && (rand_int(100) < 10)) return (0);


	/* Require ability */
	if (!borg_prayer_okay(book, what)) return (0);


	/* Choose optimal location */
	b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

	/* Penalize mana usage */
	b_n = b_n - as->power;

	/* Penalize use of reserve mana */
	if (b_ptr->csp - as->power < b_ptr->msp / 2) b_n = b_n - as->power * 10;

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
 * Simulate/Apply the optimal result of using a "dispel" attack
 */
static int borg_attack_aux_dispel(int dam, u32b type)
{
	int b_n = 0;

	int i, ty, tx, num = 0;

	int py = b_ptr->py, px = b_ptr->px;

	/* Check each monster */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];
		monster_race *r_ptr;

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Acquire race pointer */
		r_ptr = &r_info[kill->r_idx];

		/* Require current knowledge */
		if (kill->when < borg_time) continue;

		/* Require correct type */
		if (type && !(r_ptr->flags3 & type)) continue;

		/* Acquire location */
		tx = kill->x;
		ty = kill->y;

		/* Check for projectability */
		if (!(borg_projectable(py, px, ty, tx))) continue;

		/* Add damage to this monster */
		b_n += (dam > kill->curhp) ? (kill->curhp) : (dam);

		/* One more monster affected */
		num++;
	}

	/* Hack -- Try to affect at least 3 monsters */
	if (num < 3) b_n = 0;

	/* Value */
	return (b_n);
}

/*
 * Simulate/Apply the optimal result of a "special" prayer
 *
 * This currently includes "Dispel Undead" and "Dispel Evil"
 */
static int borg_attack_aux_prayer_special(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];

	int dam, b_n;

	/* No firing while blind, confused, or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (0);

	/* Check for availability */
	if (!borg_prayer_okay(book, what)) return (0);

	/* Determine damage */
	if (book == 8)
	{
		/* Copy in Wrath of God */
		dam = b_ptr->lev * 4;
	}
	else
	{
		/* Copy in Exorcism and Dispelling */
		dam = b_ptr->lev * 3;
	}

	/* Determine which spell is being used */
	if ((book == 3 && what == 1) || (book == 8 && what == 0))
	{
		/* Extract damage */
		b_n = borg_attack_aux_dispel(dam, RF3_UNDEAD);
	}
	else
	{
		/* Extract damage */
		b_n = borg_attack_aux_dispel(dam, RF3_EVIL);
	}

	/* Penalize mana usage */
	b_n = b_n - as->power;

	/* Penalize use of reserve mana */
	if (b_ptr->csp - as->power < b_ptr->msp / 2) b_n = b_n - as->power * 10;

	/* Simulation */
	if (borg_simulate) return (b_n);

	/* Instantiate */
	(void)borg_prayer(book, what);

	/* Value */
	return (b_n);
}


/*
 * Simulate/Apply the optimal result of using a "special" attack staff
 *
 * This includes Staves of Dispel Evil and Staves of Power
 */
static int borg_attack_aux_staff(int sval)
{
	int i, b_n;

	/* No firing while hallucinating */
	if (borg_base_is_image) return (0);

	/* Look for that staff */
	i = borg_slot(TV_STAFF, sval);

	/* None available */
	if (i < 0) return (0);

	/* No charges */
	if (!borg_items[i].pval) return (0);

	/* Check staff type */
	if (sval == SV_STAFF_DISPEL_EVIL)
	{
		/* Damage from dispel evil */
		b_n = borg_attack_aux_dispel(60, RF3_EVIL);
	}
	else if ((sval == SV_STAFF_POWER) || (sval == SV_STAFF_HOLINESS))
	{
		/* Damage from dispel all */
		b_n = borg_attack_aux_dispel(120, 0);
	}
	else 
	{
		/* XXX */
		b_n = 0;
	}

	/* Simulation */
	if (borg_simulate) return (b_n);

	/* Instantiate */
	(void)borg_use_staff(sval);

	/* Value */
	return (b_n);
}


/*
 * Simulate/Apply the optimal result of using a "special" artifact activation
 *
 * This currently includes Ingwe (dispel evil with damage 5*level)
 */
static int borg_attack_aux_artifact(int name1)
{
	int i, b_n;

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

	/* Check artifact type */
	if (name1 == ART_INGWE)
	{
		/* Damage from dispel evil */
		b_n = borg_attack_aux_dispel(5 * b_ptr->lev, RF3_EVIL);
	}
	else
	{
		/* XXX */
		b_n = 0;
	}

	/* Simulation */
	if (borg_simulate) return (b_n);

	/* Instantiate */
	(void)borg_activate_artifact(name1);

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


	/* Paranoia */
	if (borg_simulate && (rand_int(100) < 10)) return (0);


	/* Look for that rod */
	i = borg_slot(TV_ROD, sval);

	/* None available */
	if (i < 0) return (0);

	/* Still "charging" */
	if (!borg_items[i].pval) return (0);


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


	/* Paranoia */
	if (borg_simulate && (rand_int(100) < 10)) return (0);


	/* Look for that wand */
	i = borg_slot(TV_WAND, sval);

	/* None available */
	if (i < 0) return (0);

	/* No charges */
	if (!borg_items[i].pval) return (0);


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


	/* Paranoia */
	if (borg_simulate && (rand_int(100) < 10)) return (0);


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


	/* Paranoia */
	if (borg_simulate && (rand_int(100) < 10)) return (0);


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

static int borg_attack_aux_sleep_ii(void)
{
    int p1= 0;
    int p2 = 0;
    int d = 0;
	int method = 0;

    auto_magic *as;

    /* Obtain initial danger */
    borg_sleep_spell = FALSE;
    p1= borg_danger(b_ptr->py,b_ptr->px,4);

    if (borg_spell_okay(2, 2))
	{
		method = 1; /* spell */
		as = &borg_magics[2][2];
	}
	if (borg_prayer_okay(1, 4))
	{
		method = 2; /* prayer */
		as = &borg_magics[1][4];
	}

	if (!method) return 0;

    /* What effect is there? */
    borg_sleep_spell_ii = TRUE;
    p2=borg_danger(b_ptr->py,b_ptr->px,4);
    borg_sleep_spell_ii = FALSE;


    /* value is d, enhance the value for rogues and rangers so that
     * they can use their critical hits.
     */
    d = (p1-p2);

    /* Penalize mana usage */
    d = d - as->power;

    /* Penalize use of reserve mana */
    if (b_ptr->csp - as->power < b_ptr->msp / 2) d = d - (as->power * 10);

    /* Simulation */
    if (borg_simulate) return (d);

	if (method == 1)
	{
		/* Cast the spell */
		if (borg_spell(2, 2)) return (d);
    }
	if (method ==2)
	{
		/* Prayer */
		if (borg_prayer(1, 4)) return (d);
	}
	/* failure */
    return (0);
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


        /* Ego-missile attack */
        case BF_LAUNCH_EGO:
		{
            return (borg_attack_aux_launch_ego());
		}


		/* Object attack */
		case BF_OBJECT:
		{
			return (borg_attack_aux_object());
		}


		/* Spell -- magic missile */
		case BF_SPELL_MAGIC_MISSILE:
		{
			dam = (3+(b_ptr->lev-1)/5)*(4+1)/2;
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
			return (borg_attack_aux_spell_bolt(1, 6, rad, dam, GF_LITE_WEAK));
		}

        /* Spell -- slow monster */
        case BF_SPELL_SLOW_MONSTER:
        dam = 10;
        return (borg_attack_aux_spell_bolt(2,7, rad, dam, GF_OLD_SLOW));

        /* Spell -- confuse monster */
        case BF_SPELL_CONFUSE_MONSTER:
        dam = 10;
        return (borg_attack_aux_spell_bolt(1,0, rad, dam, GF_OLD_CONF));


		/* Spell -- sleep I */
        case BF_SPELL_SLEEP_I:
        dam = 10;
        return (borg_attack_aux_spell_bolt(1, 3, rad, dam, GF_OLD_SLEEP));

        /* Spell -- sleep II and sanctuary */
        case BF_SPELL_SLEEP_II:
        dam = 10;
        return (borg_attack_aux_sleep_ii());

        case BF_SPELL_SLEEP_III:
		rad = 10;
        dam = 10;
        return (borg_attack_aux_spell_bolt(2,5,rad,dam, GF_OLD_SLEEP));

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

		/* Prayer -- dispel undead */
		case BF_PRAYER_DISPEL_UNDEAD1:
		{
			return (borg_attack_aux_prayer_special(3, 1));
		}

		/* Prayer -- dispel evil */
		case BF_PRAYER_DISPEL_EVIL1:
		{
			return (borg_attack_aux_prayer_special(3, 3));
		}

		/* Prayer -- dispel undead */
		case BF_PRAYER_DISPEL_UNDEAD2:
		{
			return (borg_attack_aux_prayer_special(8, 0));
		}

		/* Prayer -- dispel evil */
		case BF_PRAYER_DISPEL_EVIL2:
		{
			return (borg_attack_aux_prayer_special(8, 1));
		}

		/* Staff -- dispel evil */
		case BF_STAFF_DISPEL_EVIL:
		{
			return (borg_attack_aux_staff(SV_STAFF_DISPEL_EVIL));
		}

		/* Staff -- power */
		case BF_STAFF_POWER:
		{
			return (borg_attack_aux_staff(SV_STAFF_POWER));
		}
        /* Staff -- holiness */
        case BF_STAFF_HOLINESS:
		{
			return (borg_attack_aux_staff(SV_STAFF_HOLINESS));
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


		/* Artifact amulet */
		case BF_ART_INGWE:
		{
			return (borg_attack_aux_artifact(ART_INGWE));
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

	int i, x, y;

	int n, b_n = 0;
	int a, b_a = -1;


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
 * Various types of preparation
 * 
 * These are ordered (roughly) from least to most expensive.  This
 * ensures that if two offer the same degree of preparation, the
 * cheaper of the two will be chosen.
 */
enum
{
    BP_WIELD,
	BP_LESSER_HEAL,
	BP_PROT_EVIL,
	BP_HEAL,
	BP_BIG_HEAL, 
	BP_RES_COLD_FIRE,
	BP_RES_COLD,
	BP_RES_FIRE,
	BP_RES_ACID,
	BP_RES_ELEC,
	BP_RES_POIS,
	BP_RESISTANCE,
	BP_TELE_OTHER,
	BP_CREATE_DOOR,
    BP_MASS_GENOCIDE,
    BP_GENOCIDE,
    BP_EARTHQUAKE,
    BP_DESTRUCTION,
    BP_BANISHMENT,
	BP_HASTE,
	BP_GLYPH,
	BP_INVULNERABILITY,
	BP_RESTORE_MANA,
	BP_SHIELD,
	BP_MAX
};

/*
 * Methods of preparation
 */
enum
{
	BPM_NONE,
	BPM_SPELL,
	BPM_PRAYER,
	BPM_POTION,
	BPM_SCROLL,
	BPM_WAND,
	BPM_STAFF,
	BPM_ROD,
	BPM_ARTIFACT,

	BPM_MAX
};

/*
 * Simulate/Apply the result of healing 2000 points of damage
 */
static int borg_prepare_aux_big_heal(int p)
{
	int q, amt;

	int method = BPM_NONE;

	/*** Check availability ***/

	/* Check for Soulkeeper */
	if (borg_items[INVEN_BODY].name1 == ART_SOULKEEPER)
	{
		/* Available if not charging */
		if (!borg_items[INVEN_BODY].timeout)
			method = BPM_ARTIFACT;
	}

	/* Check for Heal prayer */
	if (borg_prayer_okay(6, 2))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	/* Check for Healing potion */
	if (borg_slot(TV_POTION, SV_POTION_STAR_HEALING) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* XXX -- Pretend we lessen danger by half healed amount */
	amt = (b_ptr->mhp > b_ptr->chp + 2000) ? 2000 : (b_ptr->mhp - b_ptr->chp);
	q = p - amt / 2;

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 100) q += (100 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_prayer_fail(6, 2, TRUE) * 10;
	}

	/* No negative danger */
	if (q < 0) q = 0;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Big Heal");

	/* Perform healing */
	if (borg_activate_artifact(ART_SOULKEEPER) ||
		borg_quaff_potion(SV_POTION_STAR_HEALING) ||
	    borg_prayer(6, 2))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of healing 300 points of damage
 */
static int borg_prepare_aux_heal(int p)
{
	int q, amt, i;

	int method = BPM_NONE;

	/*** Check availability ***/

	/* Check for Rod of Healing */
	if ((i = borg_slot(TV_ROD, SV_ROD_HEALING)) >= 0)
	{
		/* Check for usability */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_ROD;
		}
	}

	/* Check for Heal prayer */
	if (borg_prayer_okay(3, 2))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	/* Check for Healing potion */
	if (borg_slot(TV_POTION, SV_POTION_HEALING) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Check for Rod of Healing */
	if ((i = borg_slot(TV_ROD, SV_ROD_HEALING)) >= 0)
	{
		/* Check for usability */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_ROD;
		}
	}
	/* Forget if not available */
	if (!method) return (p);

	/* XXX -- Pretend we lessen danger by half healed amount */
	amt = (b_ptr->mhp > b_ptr->chp + 300) ? 300 : (b_ptr->mhp - b_ptr->chp);
	q = p - amt / 2;

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 32) q += (32 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_prayer_fail(3, 2, TRUE) * 5;
	}

	/* No negative danger */
	if (q < 0) q = 0;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Heal");

	/* Perform healing */
	if (borg_zap_rod(SV_ROD_HEALING) ||
		borg_quaff_potion(SV_POTION_HEALING) ||
	    borg_prayer(3, 2))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of cure light wounds
 */
static int borg_prepare_aux_lesser_heal(int p)
{
	int q, amt;

	int method = BPM_NONE;

	/*** Check availability ***/

	/* Check for Heal prayer */
	if (borg_prayer_okay(0, 1))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	if (borg_spell_okay(0, 5))
	{
		/* Avaiable */
		method = BPM_SPELL;
	}

	/* Check for Cure Light Wounds potion */
	if (borg_slot(TV_POTION, SV_POTION_CURE_LIGHT) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* XXX -- Pretend we lessen danger by half healed amount */
	amt = (b_ptr->mhp > b_ptr->chp + 10) ? 10 : (b_ptr->mhp - b_ptr->chp);
	q = p - amt / 2;

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 4) q += (4 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_prayer_fail(0, 1, TRUE) / 2;
	}

	/* No negative danger */
	if (q < 0) q = 0;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Lesser Heal");

	/* Perform healing */
	if (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
		borg_prayer(0, 1) ||
		borg_spell(0, 5))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting all lows
 */
static int borg_prepare_aux_resistance(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_oppose_fire = b_ptr->oppose_fire;
	int old_oppose_cold = b_ptr->oppose_cold;
	int old_oppose_acid = b_ptr->oppose_acid;
	int old_oppose_elec = b_ptr->oppose_elec;
	int old_oppose_pois = b_ptr->oppose_pois;

	/*** Check availability ***/
	
	/* Check for "Resistance" spell */
	if (borg_spell_okay(4, 4))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Check for Colluin */
	if (borg_items[INVEN_OUTER].name1 == ART_COLLUIN)
	{
		/* Available if not charging */
		if (!borg_items[INVEN_OUTER].timeout)
			method = BPM_ARTIFACT;
	}

	/* Check for Bladeturner */
	if (borg_items[INVEN_BODY].name1 == ART_BLADETURNER)
	{
		/* Available if not charging */
		if (!borg_items[INVEN_BODY].timeout)
			method = BPM_ARTIFACT;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_fire = 9999;
	b_ptr->oppose_cold = 9999;
	b_ptr->oppose_acid = 9999;
	b_ptr->oppose_elec = 9999;
	b_ptr->oppose_pois = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* Restore old values */
	b_ptr->oppose_fire = old_oppose_fire;
	b_ptr->oppose_cold = old_oppose_cold;
	b_ptr->oppose_acid = old_oppose_acid;
	b_ptr->oppose_elec = old_oppose_elec;
	b_ptr->oppose_pois = old_oppose_pois;

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 50) q += (50 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(4, 4, TRUE) * 5;
	}


	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resistance");

	/* Cast resistance */
	if (borg_activate_artifact(ART_BLADETURNER) ||
	    borg_activate_artifact(ART_COLLUIN) ||
	    borg_spell(4, 4))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting fire and cold
 */
static int borg_prepare_aux_resist_fire_cold(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_oppose_fire = b_ptr->oppose_fire;
	int old_oppose_cold = b_ptr->oppose_cold;

	/*** Check availability ***/
	
	/* Check for prayer */
	if (borg_prayer_okay(1, 7))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_fire = 9999;
	b_ptr->oppose_cold = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* Restore old values */
	b_ptr->oppose_fire = old_oppose_fire;
	b_ptr->oppose_cold = old_oppose_cold;

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 30) q += (30 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_prayer_fail(1, 7, TRUE) * 5;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resist Fire/Cold");

	/* Cast resist heat and cold */
	if (borg_prayer(1, 7))
		return (q);

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting fire
 */
static int borg_prepare_aux_resist_fire(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_oppose_fire = b_ptr->oppose_fire;

	/*** Check availablity ***/

	/* Check for spell */
	if (borg_spell_okay(4, 0))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Check for potion */
	if (borg_slot(TV_POTION, SV_POTION_RESIST_HEAT) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_fire = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 20) q += (20 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(4, 0, TRUE) * 5;
	}

	/* Restore old values */
	b_ptr->oppose_fire = old_oppose_fire;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resist Fire");

	/* Cast */
	if (borg_quaff_potion(SV_POTION_RESIST_HEAT) ||
	    borg_spell(4, 0))
		return (q);

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting cold
 */
static int borg_prepare_aux_resist_cold(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_oppose_cold = b_ptr->oppose_cold;

	/*** Check availablity ***/

	/* Check for spell */
	if (borg_spell_okay(4, 1))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Check for potion */
	if (borg_slot(TV_POTION, SV_POTION_RESIST_COLD) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_cold = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* Restore old values */
	b_ptr->oppose_cold = old_oppose_cold;

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 20) q += (20 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(4, 1, TRUE) * 5;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resist Cold");

	/* Cast */
	if (borg_quaff_potion(SV_POTION_RESIST_COLD) ||
	    borg_spell(4, 1))
		return (q);

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting acid
 */
static int borg_prepare_aux_resist_acid(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_oppose_acid = b_ptr->oppose_acid;

	/*** Check availablity ***/

	/* Check for spell */
	if (borg_spell_okay(4, 2))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_acid = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* Restore old values */
	b_ptr->oppose_acid = old_oppose_acid;

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 20) q += (20 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(4, 2, TRUE) * 5;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resist Acid");

	/* Cast */
	if (borg_spell(4, 2))
		return (q);

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting electricity
 */
static int borg_prepare_aux_resist_elec(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = FALSE;

	int old_oppose_elec = b_ptr->oppose_elec;

	/*** Check availablity ***/

	/* Check for spell */
	if (borg_spell_okay(4, 3))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_elec = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* Restore old values */
	b_ptr->oppose_elec = old_oppose_elec;

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 20) q += (20 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(4, 3, TRUE) * 5;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resist Elec");

	/* Cast */
	if (borg_spell(4, 3))
		return (q);

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of resisting poison
 */
static int borg_prepare_aux_resist_pois(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_oppose_pois = b_ptr->oppose_pois;

	/*** Check availablity ***/

	/* Check for spell */
	if (borg_spell_okay(4, 4))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->oppose_pois = 9999;

	/* Now analyze danger with resistances */
	q = borg_danger(py, px, 2);

	/* Restore old values */
	b_ptr->oppose_pois = old_oppose_pois;

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 20) q += (20 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(4, 4, TRUE) * 5;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Resist Poison");

	/* Cast */
	if (borg_spell(4, 4))
		return (q);

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of teleporting a monster away
 *
 * Note that we try to NOT teleport uniques away, or we'll never
 * actually fight them.
 *
 * We also try not to teleport away breeders, because this
 * tends to create more of a problem than it solves.
 */
static int borg_prepare_aux_tele_other(int p)
{
	int q, b_q;
	int i, b_i;

	u16b grids[MAX_RANGE];

	int dis, j, m_idx;
	int y, x;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int ty, tx;

	int method = BPM_NONE;

	/*** Check availability ***/

	/* Check for spell */
	if (borg_spell_okay(3, 2))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Check for prayer */
	if (borg_prayer_okay(4, 2))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	/* Check for wand */
	if ((i = borg_slot(TV_WAND, SV_WAND_TELEPORT_AWAY)) >= 0)
	{
		/* Check for usability */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_WAND;
		}
	}

	/* Check for rod */
	if ((i = borg_slot(TV_ROD, SV_ROD_TELEPORT_AWAY)) >= 0)
	{
		/* Check for usability */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_ROD;
		}
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Initial danger */
	b_q = p;
	b_i = -1;

	/* Pick a monster to cast on */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill;

		/* Grab monster */
		kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Require current knowledge */
		if (kill->when < borg_time) continue;

		/* Acquire location */
		tx = kill->x;
		ty = kill->y;

		/* Don't shoot through walls */
		if (!(borg_cave_info[ty][tx] & (CAVE_VIEW))) continue;

		/* Acquire path */
		dis = borg_project_path(grids, MAX_RANGE, py, px, ty, tx, PROJECT_THRU);

		/* Begin with current danger */
		q = p;

		/* See which get teleported */
		for (j = 1; j < dis; j++)
		{
			/* Get location */
			x = GRID_X(grids[j]);
			y = GRID_Y(grids[j]);

			/* Get monster index */
			m_idx = borg_cave_m_idx[y][x];

			/* Skip non-existant monsters */
			if (m_idx <= 0) continue;

			/* Avoid uniques */
			if (r_info[borg_kills[m_idx].r_idx].flags1 & RF1_UNIQUE) q += 9999;

            /* Avoid breeders */
            if (r_info[borg_kills[m_idx].r_idx].flags2 & RF2_MULTIPLY) q += 9999;

			/* Subtract danger from this monster */
			q -= borg_danger_aux(py, px, 2, m_idx);
		}

		/* Teleporting everything away at high health does no good */
		if ((q <= 0) && (b_ptr->chp >= b_ptr->mhp * 7 / 8)) continue;

		/* Track best result */
		if (q >= b_q) continue;

		/* Remember */
		b_q = q;
		b_i = i;
	}

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 100) b_q += (100 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		b_q += borg_spell_fail(3, 2, TRUE) * 5;
	}

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 100) b_q += (100 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		b_q += borg_prayer_fail(4, 2, TRUE) * 5;
	}

	/* No negative danger */
	if (b_q < 0) b_q = 0;

	/* Return new danger if simulating */
	if (borg_simulate) return (b_q);

	borg_note("# Preparation Teleport Away");

	/* Save location */
	xb_ptr->gy = borg_kills[b_i].y;
	xb_ptr->gx = borg_kills[b_i].x;

	/* Target grid */
	(void)borg_target(xb_ptr->gy, xb_ptr->gx);

	/* Fire */
	if (borg_aim_wand(SV_WAND_TELEPORT_AWAY) ||
	    borg_zap_rod(SV_ROD_TELEPORT_AWAY) ||
	    borg_spell(3, 2) ||
	    borg_prayer(4, 2))
	{
		/* Fire at target */
		borg_keypress('5');

		/* Return value */
		return (b_q);
	}

	/* XXX */
	return (p);
}



/*
 * Simulate/Apply the result of laying a glyph of warding
 */
static int borg_prepare_aux_glyph(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	byte old_feat = borg_cave_feat[py][px];

	/*** Check availability ***/

	/* No casting on top of objects */
	if (borg_on_object) return (p);

	/* No casting on open doors and such */
	if (borg_cave_feat[py][px] != FEAT_FLOOR) return (p);

	/* Check for prayer */
	if (borg_prayer_okay(3, 4))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	/* Scroll */
	if (borg_slot(TV_SCROLL, SV_SCROLL_RUNE_OF_PROTECTION) >= 0)
	{
		/* Available */
		method = BPM_SCROLL;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	borg_cave_feat[py][px] = FEAT_GLYPH;

	/* Now analyze danger */
	q = borg_danger(py, px, 2);

	/* Restore old feature */
	borg_cave_feat[py][px] = old_feat;

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 110) q += (110 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_prayer_fail(3, 4, TRUE) * 10;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Glyph/Rune");

	/* XXX -- Assume on top of an object now */
	borg_on_object = TRUE;

	/* XXX -- Assume this will be successful -- XXX */
	/* XXX -- This is probably a bad idea -- XXX */
	borg_cave_feat[py][px] = FEAT_GLYPH;

	/* Lay glyph */
	if (borg_read_scroll(SV_SCROLL_RUNE_OF_PROTECTION) ||
	    borg_prayer(3, 4))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the result of hasting
 */
static int borg_prepare_aux_haste(int p)
{
	int q;

	int i;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	/* Already hasted, this'll do no good */
	if (b_ptr->fast) return (p);

	/*** Check availability ***/

	/* Check for spell first*/
	if (borg_spell_okay(3, 3))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Check for potion */
	if (borg_slot(TV_POTION, SV_POTION_SPEED) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Check for staff */
	if ((i = borg_slot(TV_STAFF, SV_STAFF_SPEED)) >= 0)
	{
		/* Check for charges */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_STAFF;
		}
	}

	/* Check for Feanor */
	if (borg_items[INVEN_FEET].name1 == ART_FEANOR)
	{
		/* Check for usability */
		if (!borg_items[INVEN_FEET].timeout)
		{
			/* Available */
			method = BPM_ARTIFACT;
		}
	}

	/* Check for rod */
	if ((i = borg_slot(TV_ROD, SV_ROD_SPEED)) >= 0)
	{
		/* Check for usability */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_ROD;
		}
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->pspeed += 10;

	/* React to change in speed */
	borg_react_speed();

	/* Now analyze danger */
	q = borg_danger(py, px, 2);

	/* Restore old speed */
	b_ptr->pspeed -= 10;

	/* React to change in speed */
	borg_react_speed();

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 50) q += (50 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(3, 3, TRUE) * 5;
	}

	/* zap the rod allways if not hasted */
	if (method == BPM_ROD)
	{
		q /= 2;
	}

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Haste");

	/* Haste */
	if (borg_zap_rod(SV_ROD_SPEED) ||
		borg_activate_artifact(ART_FEANOR) ||
	    borg_quaff_potion(SV_POTION_SPEED) ||
	    borg_use_staff(SV_STAFF_SPEED) ||
	    borg_spell(3, 3))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the results of using Protection from Evil
 */
static int borg_prepare_aux_prot_evil(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_protevil = b_ptr->protevil;

	/*** Check availability ***/

	/* Already protected */
	if (b_ptr->protevil) return (p);

	/* Check for Carlammas */
	if (borg_items[INVEN_NECK].name1 == ART_CARLAMMAS)
	{
		/* Check for usability */
		if (!borg_items[INVEN_NECK].timeout)
		{
			/* Available */
			method = BPM_ARTIFACT;
		}
	}

	/* Check for prayer */
	if (borg_prayer_okay(2, 4))
	{
		/* Available */
		method = BPM_PRAYER;
	}

	/* Check for scroll */
	if (borg_slot(TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL) >= 0)
	{
		/* Available */
		method = BPM_SCROLL;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->protevil = 9999;

	/* Now analyze danger */
	q = borg_danger(py, px, 2);

	/* XXX -- Penalize prayer */
	if (method == BPM_PRAYER)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 20) q += (20 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_prayer_fail(2, 4, TRUE) * 5;
	}

	/* Restore old value */
	b_ptr->protevil = old_protevil;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Protection From Evil");

	/* Apply protection */
	if (borg_activate_artifact(ART_CARLAMMAS) ||
		borg_read_scroll(SV_SCROLL_PROTECTION_FROM_EVIL) ||
	    borg_prayer(2, 4))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/* Create Door */
static int borg_prepare_aux_create_door( int p1)
{
    int p2 = 0;
    int fail_allowed = 30;
    int door_bad =0;
    int door_x, door_y,x,y;

    /* any summoners near?*/
    if (!borg_fighting_summoner) return (p1);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > borg_avoid)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (borg_avoid*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < borg_avoid/3)
        fail_allowed += 20;

    if (!borg_spell_fail(5, 0, TRUE) < fail_allowed ) return (p1);

    /* Do not cast if surounded by doors or something */
    /* Get grid */
    for (door_x = -1; door_x <= 1; door_x++)
    {
        for (door_y = -1; door_y <= 1; door_y++)
        {
            /* Acquire location */
            x = door_x + b_ptr->px;
            y = door_y + b_ptr->py;

            /* track spaces already protected */
            if ((borg_cave_feat[y][x] == FEAT_GLYPH) ||
               ((borg_cave_feat[y][x] >= FEAT_DOOR_HEAD) && 
			    (borg_cave_feat[y][x] <= FEAT_PERM_SOLID)))
            {
                door_bad++;
            }

            /* track spaces that cannot be protected */
            if ((borg_cave_o_idx[y][x]) ||
               ((borg_cave_feat[y][x] >= FEAT_TRAP_HEAD) && 
			    (borg_cave_feat[y][x] <= FEAT_TRAP_TAIL)) ||
                (borg_cave_feat[y][x] == FEAT_LESS) ||
               (borg_cave_feat[y][x] == FEAT_MORE) ||
               (borg_cave_feat[y][x] == FEAT_OPEN) ||
               (borg_cave_feat[y][x] == FEAT_BROKEN) ||
               (borg_cave_m_idx[y][x]))
            {
                door_bad++;
            }
        }
    }

    /* Track it */
    /* lets make sure that we going to be benifited */
    if (door_bad >= 6)
    {
        /* not really worth it.  Only 2 spaces protected */
        return (p1);
    }

    /* pretend we are protected and look again */
    borg_create_door = TRUE;
    p2 = borg_danger(b_ptr->py, b_ptr->px, 1);
    borg_create_door = FALSE;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= ((borg_unique_near || vault_on_level)?((borg_avoid*2)/3): (borg_avoid/2)) &&
        p1 > (borg_avoid/8))
    {
        /* Simulation */
        if (borg_simulate) return (p2);

        /* do it! */
        if (borg_spell(5, 0))
        {
            /* Value */
            return (p2);
        }
    }

    /* default to can't do it. */
    return (p1);
}

/* This will simulate and cast the mass genocide spell.  There is a concern
 * when the genocide spell prompts us for information on what type of
 * create to genocide.  The mass genocide spell will not be a problem.
 */
static int borg_prepare_aux_mass_genocide(int p1)
{
    int hit = 0, i, y, x,p2;
	int method = BPM_NONE;

    auto_magic *as = &borg_magics[6][4];

	/* Check for Eonwe */
	if (borg_items[INVEN_WIELD].name1 == ART_EONWE)
	{
		/* Check for usability */
		if (!borg_items[INVEN_WIELD].timeout)
		{
			/* Available */
			method = BPM_ARTIFACT;
		}
	}

    /* see if spell is legal */
    if (borg_spell_fail(6, 4, TRUE) < 40)
	{
		/* Avaiable */
		method = BPM_SPELL;
	}

	/* Forget if not available */
	if (!method) return (p1);

    /* See if he is in real danger */
    if (p1 < borg_avoid * 3)
        return (p1);

    /* Penalize mana usage */
	if (method == BPM_SPELL)
		p2 = as->power;

    /* Penalize for loss of HP from casting the spell */
    /* Scan the known monster list */
    for (i = 0; i < borg_kills_nxt; i++)
    {

        auto_kill *kill;
        monster_race *r_ptr;

        /* Monster */
        kill = &borg_kills[i];
        r_ptr = &r_info[kill->r_idx];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Acquire location */
        x = kill->x;
        y = kill->y;

        /* Check the distance */
        if (distance(b_ptr->py, b_ptr->px, y, x) > MAX_SIGHT) continue;

        /* take a hit, add the damage */
        hit = hit + 2;
    }

    /* if strain is greater than hp, don't cast it */
    if (hit >= b_ptr->chp) p2= +1000;

    /* Penalize the strain from casting the spell */
    p2 = p2 + hit;

    /* Penalize use of reserve mana */
	if (method == BPM_SPELL)
		if (b_ptr->csp - as->power < b_ptr->msp / 2) p2 = p2 + (as->power * 10);

    /* Simulation */
    if (borg_simulate) return (p2);

	borg_note("# Preparation Mass Genocide");

    /* Cast the spell */
    if (borg_spell(6, 4) ||
        borg_activate_artifact(ART_EONWE))
        {
            /* Value */
            return (p2);
        }
     else
     return (p1);

}

/* This will simulate and cast the genocide spell.  There is a concern
 * when the genocide spell prompts us for information on what type of
 * create to genocide.
 */
static int borg_prepare_aux_genocide(int p1)
{
    int i, p, b_p=0, b_i = 0, y, x;
    int p2 = 0;
    char genocide_target= ' ';

    bool genocide_spell = FALSE;
    int fail_allowed = 39;

	int py = b_ptr->py;
	int px = b_ptr->px;

    /* See if he is in real danger */
    if (p1 < borg_avoid)
        return (p1);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > borg_avoid)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (borg_avoid * 2) / 3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < borg_avoid / 3)
        fail_allowed += 10;

    if ((borg_spell_fail(3, 6, TRUE) < fail_allowed) ||
        (borg_spell_fail(6, 3, TRUE) < fail_allowed) ||
        ( -1 != borg_slot(TV_SCROLL, SV_SCROLL_GENOCIDE)) ||
        ( -1 != borg_slot(TV_STAFF, SV_STAFF_GENOCIDE) &&
         borg_items[borg_slot(TV_STAFF, SV_STAFF_GENOCIDE)].pval))
    {
        genocide_spell = TRUE;
		borg_note("# We can genocide!!");
    }

	if (borg_items[INVEN_BODY].name1 == ART_CELEBORN)
	{
		/* Check for usability */
		if (!borg_items[INVEN_BODY].timeout)
		{
			/* Available */
			genocide_spell = TRUE;
		}
	}
		if (genocide_spell == FALSE) return (p1);

	    /* two methods to calculate the threat: */
	    /* 1 cycle each character of monsters on screen*/
	    /* 1 collect collective threat of each char */

	    /* 2 select race of most dangerous guy, and choose him */
	    /* We will use method two */

		/* Find a monster and calculate its danger */
		for (i = 0; i < borg_kills_nxt; i++)
		{
	        auto_kill *kill;
			monster_race *r_ptr;

	        /* Monster */
			kill = &borg_kills[i];
			r_ptr = &r_info[kill->r_idx];

			/* Skip dead monsters */
			if (!kill->r_idx) continue;

			/* Require current knowledge */
			if (kill->when < borg_time) continue;

			/* generally Genocide Summoners.  They are the nasty ones */
			if (!(r_ptr->flags6 & RF6_S_KIN) &&
				!(r_ptr->flags6 & RF6_S_HI_DEMON) &&
				!(r_ptr->flags6 & RF6_S_MONSTER) &&
				!(r_ptr->flags6 & RF6_S_MONSTERS) &&
				!(r_ptr->flags6 & RF6_S_ANT) &&
				!(r_ptr->flags6 & RF6_S_SPIDER) &&
				!(r_ptr->flags6 & RF6_S_HOUND) &&
				!(r_ptr->flags6 & RF6_S_HYDRA) &&
				!(r_ptr->flags6 & RF6_S_ANGEL) &&
				!(r_ptr->flags6 & RF6_S_DEMON) &&
				!(r_ptr->flags6 & RF6_S_UNDEAD) &&
				!(r_ptr->flags6 & RF6_S_DRAGON) &&
				!(r_ptr->flags6 & RF6_S_HI_UNDEAD) &&
				!(r_ptr->flags6 & RF6_S_WRAITH) &&
				!(r_ptr->flags6 & RF6_S_UNIQUE) ) continue;

			/* we try not to genocide uniques */
			if (r_ptr->flags1& RF1_UNIQUE) continue;
	
			/* Acquire location */
	        x = kill->x;
			y = kill->y;

			/* Check the distance XXX XXX XXX */
			if (distance(py, px, y, x) > 10) continue;
	
		    /* if he can't see me, then he is no threat */
//         if (!borg_los(y,x, py, px)) continue;

			/* Calculate danger */
			p = borg_danger(y, x, 1);

	        /* Skip useless attacks */
		    if (p <= 0) continue;

			/* Collect best attack */
			if ((b_i >= 0) && (p < b_p)) continue;

			/* track it */
			b_i = i;
			b_p = p;
			/* AND THE ENVELOPE PLEASE......*/
			genocide_target = r_ptr->x_char;
		}

		/* How is it with this guy gone? */
		/* Not entirely true, we take out the baddie, but not the others who
		 * share this character (escorts).  So the effect may be greater.
		 */
	    p2 = p1 - b_p;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= ((borg_unique_near || vault_on_level)? (borg_avoid*2/3):(borg_avoid/2)) &&
		       p1 > (borg_avoid/7))
		{
	        /* Simulation */
		    if (borg_simulate) return (p2);

			borg_note("# Preparation Genocide");

			/* do it! ---use scrolls first since they clutter inventory */
			if ( borg_read_scroll( SV_SCROLL_GENOCIDE))
			{
	            /* "this is a genocide scroll -more- " */
				borg_keypress((genocide_target));
				return (p2);
			}

	        if (borg_spell(6, 3) ||
				borg_spell(3, 6) ||
				borg_activate_artifact(ART_CELEBORN) ||
				borg_use_staff(SV_STAFF_GENOCIDE))
			{
#if 0
			/* Was I sucessfull in casting the spell? */
			byte t_a;
			char buf[10];

			/* Check to see if we are being asked to wax someone. */
			if (!(0 == borg_what_text(38, 0, 9, &t_a, buf) &&
				(buf[0] == 'g') &
				(buf[1] == 'e') &
				(buf[2] == 'n') &
				(buf[3] == 'o') &
				(buf[4] == 'c') &
				(buf[5] == 'i') &
				(buf[6] == 'd') &
				(buf[7] == 'e') &
				(buf[8] == ':')) )
			{
#endif
				/* and the winner is.....*/
				borg_keypress((genocide_target));
#if 0
			}
#endif
		return (p2);
		}

	}
    /* default to can't do it. */
    return (p1);
}

/* Earthquake, priest and mage spells */
static int borg_defend_aux_earthquake(int p1)
{
    int p2 = 0;

    if (!borg_prayer_fail(2, 5, TRUE) < 40 &&
        !borg_spell_fail(5, 3, TRUE) < 40)
        return (p1);

    /* See if he is in real danger */
    if (p1 < borg_avoid* 2)
        return (p1);

    /* What effect is there? */
    p2= p1 / 2;

    if (p1 > p2 &&
           p2 <= ((borg_unique_near || vault_on_level)?((borg_avoid*2)/3): (borg_avoid/2)) &&
           p1 > (borg_avoid/7))
    {
        /* Simulation */
        if (borg_simulate) return (p2);

        /* Cast the spell */
        if (borg_prayer(2, 5) ||
            borg_spell(5,3))
            {
                return (p2);
            }
     }
     return (p1);
}

/* Word of Destruction, priest and mage spells.  Death is right around the
 *  corner, so kill everything.
 */
static int borg_defend_aux_destruction(int p1)
{
    int p2 = 0;
    
    if (!borg_prayer_fail(8, 3, TRUE) < 40 &&
        !borg_spell_fail(3, 5, TRUE) < 40)
        return (p1);

    /* See if he is in real danger */
    if (p1 < borg_avoid * 3)
        return (p1);

    /* What effect is there? */
    p2= 0;

    /* Simulation */
    if (borg_simulate) return (p2);

    /* Cast the spell */
    if (borg_prayer(8, 3) ||
        borg_spell(3,5))
        {
            return (p2);
        }

    /* oops it did not work */
    return (p1);
}

static int borg_defend_aux_banishment(int p1)
{
    int p2= 0, b_n = 0;
    int fail_allowed = 25;

    /* Only tell away if scared and there is a local unique */
    if ( p1 < borg_avoid * 3)
        return (p1);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > borg_avoid* 4)
        fail_allowed -= 15;

    if (!borg_prayer_fail(8, 2, TRUE) < fail_allowed)
        return (p1);

	/* This needs to be checked DvE */
    b_n = borg_attack_aux_prayer_bolt(8,2,20, 500, GF_AWAY_EVIL);

    /* normalize the value */
    p2 = (p1 - b_n);
    if (p2 < 0) p2 = 0;

    /* check to see if I am left better off */
    if (p1 > p2 &&
        p2 <= ((borg_unique_near || vault_on_level)?((borg_avoid*2)/3): (borg_avoid/2)) && 
        p1 > (borg_avoid/8))
    {
        /* Simulation */
        if (borg_simulate) return (p2);

        /* Cast the spell */
        if (borg_prayer(8, 2))
        {
            /* Value */
            return (p2);
        }
    }
        return (p1);
}


/*
 * Simulate/Apply the results of using Globe of Invulnerability
 */
static int borg_prepare_aux_invulnerability(int p)
{
	int q;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int method = BPM_NONE;

	int old_invuln = b_ptr->invuln;

	/*** Check availability ***/

	/* Already protected */
	if (b_ptr->invuln) return (p);

	/* Check for spell */
	if (borg_spell_okay(7, 4))
	{
		/* Available */
		method = BPM_SPELL;
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Pretend to cast */
	b_ptr->invuln = 9999;

	/* Now analyze danger */
	q = borg_danger(py, px, 2);

	/* XXX -- Penalize spell */
	if (method == BPM_SPELL)
	{
		/* Leave us with some mana */
		if (b_ptr->csp < 150) q += (150 - b_ptr->csp) * 5;

		/* Penalize fail rate */
		q += borg_spell_fail(7, 4, TRUE) * 10;
	}

	/* Restore old value */
	b_ptr->invuln = old_invuln;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Globe of Invulnerability");

	/* Apply globe */
	if (borg_spell(7, 4))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

/*
 * Simulate/Apply the results of using Restore Mana
 */
static int borg_prepare_aux_restore_mana(int p)
{
	int q;

	int i;

	int method = BPM_NONE;

	/* Warriors can't use this */
	if (b_ptr->pclass == CLASS_WARRIOR) return (p);

	/*** Check availability ***/

	/* Check for potion */
	if (borg_slot(TV_POTION, SV_POTION_RESTORE_MANA) >= 0)
	{
		/* Available */
		method = BPM_POTION;
	}

	/* Check for staff */
	if ((i = borg_slot(TV_STAFF, SV_STAFF_THE_MAGI)) >= 0)
	{
		/* Check for charges */
		if (borg_items[i].pval)
		{
			/* Available */
			method = BPM_STAFF;
		}
	}

	/* Forget if not available */
	if (!method) return (p);

	/* Assume we lessen danger by half the mana restored */
	q = p - (b_ptr->msp - b_ptr->csp) / 2;

	/* Return new danger if simulating */
	if (borg_simulate) return (q);

	borg_note("# Preparation Restore Mana");

	/* Apply */
	if (borg_quaff_potion(SV_POTION_RESTORE_MANA) ||
	    borg_use_staff(SV_STAFF_THE_MAGI))
	{
		/* Return value */
		return (q);
	}

	/* XXX */
	return (p);
}

static int borg_prepare_aux_shield( int p1)
{
    int p2 = 0;
    int fail_allowed = 39;

    /* if already protected */
    if (b_ptr->shield || b_ptr->invuln)
        return (p1);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > borg_avoid)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (borg_avoid*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < borg_avoid/3)
        fail_allowed += 5;

    if (!borg_spell_fail(7, 1, TRUE) < fail_allowed)
        return (p1);

    /* pretend we are protected and look again */
    b_ptr->shield = 9999;
    p2 = borg_danger(b_ptr->py, b_ptr->px, 1);
    b_ptr->shield = 0;

    /* slightly enhance the value if fighting a unique */
	/* or there is a vault on he level */
    if (borg_unique_near || vault_on_level)  p2=(p2*7/10);


    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= ((borg_unique_near || vault_on_level)?((borg_avoid*2)/3): (borg_avoid/2)) && 
        p1 > (borg_avoid/8))
    {
        /* Simulation */
        if (borg_simulate) return (p2);

        /* do it! */
        borg_spell(7, 1);
        return (p2);
    }

    /* default to can't do it. */
    return (p1);
}

/*
 * Simulate/apply the results of wielding an alternate item.
 */
static int borg_prepare_aux_wield(int p)
{
	int hole = INVEN_PACK - 1;

    int q, b_q;
    int i, b_i;

    int slot;

	int py = b_ptr->py;
	int px = b_ptr->px;

    bool fix = FALSE;

    auto_item *item;

    b_i = -1;
    b_q = borg_danger(px, py, 2);

    for (i = 0; i < INVEN_PACK; i++)
    {
        item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* skip cursed items */
		if (item->flags3 & TR3_LIGHT_CURSE) continue;
		if (item->flags3 & TR3_HEAVY_CURSE) continue;

        /* Find slot */
        slot = borg_wield_slot(item);

        /* Require slot */
        if (slot < 0) continue;

		/* Save the old item */
		COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

		/* Save the new item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

		/* Save the hole */
		COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

		/* Take off old item */
		COPY(&borg_items[hole], &borg_safe_items[slot], auto_item);

		/* Wear new item */
		COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

		/* Only a single item */
		borg_items[slot].iqty = 1;

		/* Reduce the inventory quantity by one */
		borg_items[i].iqty--;

		/* Fix later */
		fix = TRUE;

		/* Examine the inventory was FALSE*/
		borg_notice(TRUE);

        /* Calculate danger */
        q = borg_danger(px, py, 2);

		/* Restore the old item */
		COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

		/* Restore the new item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Restore the hole */
		COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

        /* Ignore bad swaps */
        if (q >= b_q) continue;

        /* Maintain the best */
        b_i = i; b_q = q;
    }

    /* Restore bonuses */
    if (fix) borg_notice(TRUE);

    /* XXX XXX XXX */
    if (b_i < 0) b_q = p;

    /* Return new danger if simulating */
    if (borg_simulate) return (b_q);

	borg_note("# Preparation Wield Alternate");

    /* Apply */
    if (b_i < 0)
    {
        borg_oops("tried to prepare by wielding nothing");
        return (b_q);
    }

    /* Get the item */
    item = &borg_items[b_i];

    /* Log */
    borg_note(format("# Wearing %s (preparing).", item->desc));

    /* Send action (wear) */
    borg_keypress('w');

    /* Send item index */
    borg_send_item_index(b_i);

    /* Return value */
    return (b_q);
}

/*
 * Simulate/Apply the result of using a given type of preparation
 */
static int borg_prepare_aux(int what, int p)
{
	/* Analyze */
	switch (what)
	{
		/* Heal (2000) */
		case BP_BIG_HEAL:
		{
			return (borg_prepare_aux_big_heal(p));
		}

		/* Heal (300) */
		case BP_HEAL:
		{
			return (borg_prepare_aux_heal(p));
		}

		/* Heal (Cure Light Wounds) */
		case BP_LESSER_HEAL:
		{
			return (borg_prepare_aux_lesser_heal(p));
		}

		/* Oppose all lows */
		case BP_RESISTANCE:
		{
			return (borg_prepare_aux_resistance(p));
		}

		/* Oppose fire and cold */
		case BP_RES_COLD_FIRE:
		{
			return (borg_prepare_aux_resist_fire_cold(p));
		}

		/* Oppose cold */
		case BP_RES_COLD:
		{
			return (borg_prepare_aux_resist_cold(p));
		}

		/* Oppose fire */
		case BP_RES_FIRE:
		{
			return (borg_prepare_aux_resist_fire(p));
		}

		/* Oppose acid */
		case BP_RES_ACID:
		{
			return (borg_prepare_aux_resist_acid(p));
		}

		/* Oppose electricity */
		case BP_RES_ELEC:
		{
			return (borg_prepare_aux_resist_elec(p));
		}

		/* Oppose poison */
		case BP_RES_POIS:
		{
			return (borg_prepare_aux_resist_pois(p));
		}

		/* Teleport a monster away */
		case BP_TELE_OTHER:
		{
			return (borg_prepare_aux_tele_other(p));
		}
		/* cast create doors */
		case BP_CREATE_DOOR:
        {
            return (borg_prepare_aux_create_door(p));
        }
		/* Lay glyph of warding */
		case BP_GLYPH:
		{
			return (borg_prepare_aux_glyph(p));
		}

		/* Haste self */
		case BP_HASTE:
		{
			return (borg_prepare_aux_haste(p));
		}

		/* Protection from evil */
		case BP_PROT_EVIL:
		{
			return (borg_prepare_aux_prot_evil(p));
		}
		/* Mass Genocide */
		case BP_MASS_GENOCIDE:
        {
            return (borg_prepare_aux_mass_genocide(p));
            break;
        }
		/* Genocide */
        case BP_GENOCIDE:
        {
            return (borg_prepare_aux_genocide(p));
            break;
        }
        case BP_EARTHQUAKE:
        {
            return (borg_defend_aux_earthquake(p));
            break;
        }
        case BP_DESTRUCTION:
        {
            return (borg_defend_aux_destruction(p));
            break;
        }
        case BP_BANISHMENT:
        {
            return (borg_defend_aux_banishment(p));
            break;
        }
		/* Globe of Invulnerability */
		case BP_INVULNERABILITY:
		{
			return (borg_prepare_aux_invulnerability(p));
		}

		/* Restore mana */
		case BP_RESTORE_MANA:
		{
			return (borg_prepare_aux_restore_mana(p));
		}

        /* Wield an alternate item (temporarily) */
        case BP_WIELD:
        {
            return (borg_prepare_aux_wield(p));
        }
	}

	/* XXX */
	return (p);
}


/*
 * Attempt to prepare for a dangerous situation
 *
 * We are called when the current danger is greater than we would like,
 * but not so great as to induce fleeing.
 *
 * We examine the situation and determine whether one of the following
 * would be beneficial:
 *
 *    Heal
 *    Resist Cold/Fire/Acid/Elec/Pois
 *    Protection from Evil
 *    Glyph of Warding
 *    Speed boost
 *    Globe of Invulnerability
 *    Restore Mana
 *	  Genocide
 *	  Mass genocide
 */
bool borg_prepare(int p)
{
	int q, b_q;
	int a, b_a = 0;

	/*** Evaluate current situation ***/

	/* Blindness, confusion, or hallucination negate these */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (FALSE);

	/* Current danger */
	b_q = p;

	/* Note */
	borg_note(format("# Wanting to prepare, danger %d", p));

	/* Simulate */
	borg_simulate = TRUE;

	/* Try each type of preparation */
	for (a = 0; a < BP_MAX; a++)
	{
		/* Check danger resulting from this */
		q = borg_prepare_aux(a, p);

		/* Track best */
		if (q >= b_q) continue;
		
		/* Remember */
		b_q = q;
		b_a = a;
	}

	/* Note */
	borg_note(format("# Best preparation (%d) reduces danger to %d", b_a, b_q));

	/* Check for worthwhile results */
	if (borg_unique_near || vault_on_level)
	{
		/* Reduce danger by at least 10% */
		if (b_q > (p * 9 / 10)) return (FALSE);

		/* Better to teleport than to die */
		if (b_q > (borg_avoid * 3 / (2 * borg_avoid_factor))) return (FALSE);
	}
	else
	{
		/* Reduce danger by at least 30% */
		if (b_q > (p * 7 / 10)) return (FALSE);

		/* Better to teleport than to die */
		if (b_q > (borg_avoid * 1 / borg_avoid_factor)) return (FALSE);
	}

	/* Note */
	borg_note(format("# Performing preparation type %d", b_a));
	borg_note(format("# Reducing danger from %d to %d", p, b_q));

	/* Instantiate */
	borg_simulate = FALSE;

	/* Instantiate */
	(void)borg_prepare_aux(b_a, p);

	/* Success */
	return (TRUE);
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
 * is there a summoner nearby? (check auto_kills)
 * This is used in borg_defend to incapacitate before melee.
 * This only works for summoners.  If one of the
 * monsters around is misidentified then it may be a summoner
 * and we wouldn't know.
 */
static bool borg_near_summoner(int dist)
{
    auto_kill *kill;
    monster_race *r_ptr;
    int x9, y9, ax, ay, d;
    int i;
	int py = b_ptr->py;
	int px = b_ptr->px;

    /* make sure there is a summoner around */
    for (i = 1; i < borg_kills_nxt; i++)
    {
        kill = &borg_kills[i];
        r_ptr = &r_info[kill->r_idx];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        x9 = kill->x;
        y9 = kill->y;

        /* Distance components */
        ax = (x9 > px) ? (x9 - px) : (px - x9);
        ay = (y9 > py) ? (y9 - py) : (py - y9);

        /* Distance */
        d = MAX(ax, ay);

        /* if the summoner is too far then skip it. */
        if (d > dist) continue;

        /* if he cant see me then forget it */
        if (!borg_los(py, px, y9, x9)) continue;

        /* found one.  Done. */
		if ((r_ptr->flags6 & RF6_S_KIN) ||
			(r_ptr->flags6 & RF6_S_HI_DEMON) ||
			(r_ptr->flags6 & RF6_S_MONSTER) ||
		    (r_ptr->flags6 & RF6_S_MONSTERS) ||
			(r_ptr->flags6 & RF6_S_ANT) ||
			(r_ptr->flags6 & RF6_S_SPIDER) ||
			(r_ptr->flags6 & RF6_S_HOUND) ||
			(r_ptr->flags6 & RF6_S_HYDRA) ||
			(r_ptr->flags6 & RF6_S_ANGEL) ||
			(r_ptr->flags6 & RF6_S_DEMON) ||
			(r_ptr->flags6 & RF6_S_UNDEAD) ||
			(r_ptr->flags6 & RF6_S_DRAGON) ||
			(r_ptr->flags6 & RF6_S_HI_UNDEAD) ||
			(r_ptr->flags6 & RF6_S_WRAITH) ||
			(r_ptr->flags6 & RF6_S_UNIQUE) ) return (TRUE);
    }

    /* no nearby summoners */
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
			/* Better to take a chance */
			/*
			if (borg_cave_feat[y][x] == FEAT_NONE) continue;
			*/

			/* Skip weird grids */
			/* Better to take a chance */
			/*
			if (borg_cave_feat[y][x] == FEAT_INVIS) continue;
			*/

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
		p = borg_max_danger(y, x, 2);

		/* Count "safe" locations */
		if (p <= borg_avoid / borg_avoid_factor) n++;
	}

	/* Too much danger */
	if (n < 35) return (FALSE);

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

	int mp, mq, b_mq = -1;

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

	/* if on level 100 and not ready for Morgoth, run */
    if (b_ptr->depth == 100)
    {
        if (borg_ready_morgoth == 0 && !borg_king)
        {
            /* Start leaving */
            if (!borg_leaving)
            {
                /* Note */
                borg_note("# Leaving (Not ready for Morgoth now)");

                /* Start leaving */
                borg_leaving = TRUE;
            }
        }
    }


	/*** Evaluate local danger ***/
	/* am I fighting a summoner? */
	borg_fighting_summoner = borg_near_summoner(10);

	/* Look around */
	mp = borg_max_danger(py, px, 1);
	p = borg_danger(py, px, 1);

	/* Prevent bouncing borg */
	if (time_this_panel > 500)
	{
		if (!borg_leaving)
		{
			borg_note("# Leaving (bouncing borg)");
			borg_leaving = TRUE;

			/* Recall if we're in town and max_depth is deep enough */
			if (!b_ptr->depth && (b_ptr->max_depth > 4))
			{
				if (borg_recall()) return (TRUE);
			}
		}
	}


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
			mq = borg_max_danger(y, x, 2);

			/* Skip larger danger */
			if ((b_mq >= 0) && (b_mq < mq)) continue;

			/* Track */
			b_mq = mq;
		}

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
	if ((p > borg_avoid / borg_avoid_factor) || (p > borg_fear_region[py/11][px/11]))
	{
		/* Describe (briefly) the current situation */
		borg_note(format("# Loc:%d,%d Dep:%d Lev:%d HP:%d/%d SP:%d/%d Danger:%d/%d Avoid:%d",
		                 py, px, b_ptr->depth, b_ptr->lev,
		                 b_ptr->chp, b_ptr->mhp, b_ptr->csp, b_ptr->msp,
		                 b_q, p, borg_avoid));
		borg_note(format("# Max Danger:%d/%d", b_mq, mp));
	}

	/* No (good) retreat */
	if ((b_q < 0) || (b_q > p)) b_q = p;
	if ((b_mq < 0) || (b_mq > mp)) b_mq = mp;

    /* do some swapping before running away! */
    if (p > (borg_avoid / borg_avoid_factor))
    {
        if (borg_backup_swap(p)) return TRUE;
    }



	/*** Danger ***/

	/* Impending doom */
	borg_notice(TRUE);
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
	else if ((b_mq > b_ptr->mhp) && 
			 (!borg_unique_level) && 
			 (b_ptr->lev < 50) && 
			 (!vault_on_level) &&
		     !((b_ptr->depth == 100) && (borg_ready_morgoth == 1)))
	{
		/* Start leaving */
		if (!borg_leaving)
		{
			/* Note */
			borg_note("# Leaving (excessive danger)");

			/* Start leaving */
			borg_leaving = TRUE;
		}

#if 0
        /* Disabled to stop stupid recalls -RML */
		/* Start fleeing */
		if (!borg_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (excessive danger)");

			/* Start fleeing */
			borg_fleeing = TRUE;
		}
#endif
	}

	/* Moderate danger */
	else if ((b_q > b_ptr->mhp / 2) && !borg_unique_level && !vault_on_level)
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
	else if (!b_ptr->depth && (mp > b_ptr->chp))
	{
		/* Flee now */
		if (!borg_leaving)
		{
			/* Flee! */
			borg_note("# fleeing (potential danger)");

			/* Start leaving */
			borg_fleeing = TRUE;
		}
	}


	/*** Stairs ***/

	/* Fleeing */
	if (borg_fleeing)
	{
		/* Take stairs anywhere */
		borg_stair_less = borg_stair_more = TRUE;
	}

	/* Leaving */
	else if (borg_leaving)
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
	if (borg_stair_less || (b_mq > b_ptr->chp / 2))
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
	if ((b_ptr->depth <= borg_happy_depth + 1) && (borg_stair_more || (b_mq > 2 * b_ptr->mhp)) && !borg_recalling)
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
	if (b_mq > borg_avoid)
	{
		/* XXX XXX XXX Count close calls */

		/* Phase door, if useful */
		if (borg_caution_phase(10) &&
			(borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
		     borg_spell(0, 2) ||
		     borg_prayer(4, 0)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Hack -- assume on object */
			borg_on_object = TRUE;

			/* Success */
			return (TRUE);
		}

		/* Teleport */
		if (TRUE &&
		    (borg_read_scroll(SV_SCROLL_TELEPORT) ||
		     borg_use_staff(SV_STAFF_TELEPORTATION) ||
			 borg_spell(1, 5) ||
		     borg_prayer(4, 1) ||
		     borg_prayer(1, 1)
		     ))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Hack -- assume on object */
			borg_on_object = TRUE;

			/* Success */
			return (TRUE);
		}
	}

	/* Prepare for a dangerous situation */
	if (b_q > borg_avoid / (borg_avoid_factor * 2))
	{
		/* Prepare if possible */
		if (borg_prepare(b_q))
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
	if (b_mq > borg_avoid / borg_avoid_factor)
	{
		/* XXX XXX XXX Count close calls */

		/* Phase door, if useful */
		if (borg_caution_phase(10) &&
		    (borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
			 borg_spell(0, 2) ||
		     borg_prayer(4, 0)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Hack -- assume on object */
			borg_on_object = TRUE;

			/* Success */
			return (TRUE);
		}

		/* Try teleportation */
		if (/* (rand_int(100) < 50) && */
		    (borg_spell(1, 5) ||
		     borg_prayer(4, 1) ||
		     borg_prayer(1, 1)))
		{
			/* Recalculate danger */
			borg_do_wipe_danger = TRUE;

			/* Forget task */
			borg_task = 0;

			/* Hack -- reset bravery */
			borg_boost = 0;

			/* Hack -- reset the "goal" location */
			xb_ptr->gx = xb_ptr->gy = 0;

			/* Hack -- assume on object */
			borg_on_object = TRUE;

			/* Success */
			return (TRUE);
		}
	}


	/*** Deal with critical situations ***/

	/* Hack -- being knocked out is extremely deadly */
	if (b_ptr->stun >= 6000)
	{
		/* Attempt to heal with potions */
		if (borg_quaff_potion(SV_POTION_HEALING) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL))
		{
			/* Success */
			return (TRUE);
		}

		/* Try to teleport */
		if (borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_use_staff(SV_STAFF_TELEPORTATION) ||
			borg_prayer(1, 1) ||
		    borg_spell(1, 5)		    
		    )
		{
			/* Success */
			return (TRUE);
		}

		/* Flee */
		if (b_ptr->depth)
		{
			/* Start leaving */
			if (!borg_leaving)
			{
				/* Flee */
				borg_note("# Leaving (badly stunned)");

				/* Start leaving */
				borg_leaving = TRUE;
			}

			/* Start fleeing */
			if (!borg_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (badly stunned)");

				/* Start fleeing */
				borg_fleeing = TRUE;
			}
		}
	}

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

	/* Confusion is bad when in danger */
	if (borg_base_is_confused && (p > borg_avoid / borg_avoid_factor))
	{
		/* Attempt to cure confusion */
		if (borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
		    borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
		    borg_quaff_potion(SV_POTION_HEALING))
		{
			/* Success */
			return (TRUE);
		}
	}


	/*** Flee on foot ***/

	/* Strategic retreat */
	if (p > borg_avoid / borg_avoid_factor)
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
				if (borg_danger(y1, x1, r+1) >= p) break;


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
			borg_keypress((char)I2D(b_d));

			/* No longer on an object */
			borg_on_object = FALSE;

			/* Done */
			return (TRUE);
		}
	}


	/* Want to back away */
	if (p > borg_avoid / borg_avoid_factor)
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
			if ((b_k == k) && (b_f >= f)) continue;

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
			borg_keypress((char)I2D(dir));

			/* No longer on an object */
			borg_on_object = FALSE;

			/* Done */
			return (TRUE);
		}

		/* Note */
		borg_note(format("# Cornered (danger %d)", p));
	}


	/*** Try healing ***/

	/* Hack -- heal when wounded (prayers) */
	if ((b_ptr->chp <= b_ptr->mhp / 2) && (rand_int(100) < 50))
	{
		/* Use Heal when badly wounded */
		if ((b_ptr->chp <= b_ptr->mhp - 150) &&
		    (b_ptr->csp >= b_ptr->msp - 30))
		{
			if (borg_prayer(3, 2)) return (TRUE);
		}

		if ((b_ptr->csp >= b_ptr->msp - 15) &&
		    (borg_prayer(2, 2) ||
		     borg_prayer(2, 7) ||
		     borg_prayer(6, 0) ||
		     borg_prayer(6, 1)))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal when wounded */
	if ((b_ptr->chp <= b_ptr->mhp / 2) && (rand_int(100) < 50))
	{
		/* Use Heal when badly wounded */
		if (b_ptr->chp <= b_ptr->mhp - 150)
		{
			if (borg_quaff_potion(SV_POTION_HEALING))
			{
				return (TRUE);
			}
		}

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
	    (amt_cure_critical < 3))
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
	if (borg_fleeing && (b_ptr->depth > 1) && (borg_recall()))
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
 * check to make sure there are no monsters around 
 * that should prevent resting
 */
bool borg_check_rest(void)
{
    int i;

    /* Examine all the monsters */
    for (i = 1; i < borg_kills_nxt; i++)
    {
        auto_kill *kill = &borg_kills[i];
        monster_race *r_ptr = &r_info[kill->r_idx];

        int x9 = kill->x;
        int y9 = kill->y;
		int py = b_ptr->py;
		int px = b_ptr->px;
        int ax, ay, d;

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Distance components */
        ax = (x9 > px) ? (x9 - px) : (px - x9);
        ay = (y9 > py) ? (y9 - py) : (py - y9);

        /* Distance */
        d = MAX(ax, ay);

        /* Minimal distance */
        if (d > 20) continue;

        /* should check LOS... monster to me   apw*/
        if (borg_los(y9,x9, py,px)) return FALSE;

        /* should check LOS... me to monster   apw*/
        if (borg_los(py,px,y9,x9)) return FALSE;

        /* if absorbs mana, not safe */
        if ((r_ptr->flags5 & RF5_DRAIN_MANA) && (b_ptr->msp > 1)) return FALSE;

        /* if breeder, not safe */
//        if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;

    }
    return TRUE;
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
		else
		{
			/* Hack -- Panic recall */
			if (borg_recall())
			{
				borg_note("# Panic: no food recalling");
				return(TRUE);
			}
		}
	}


	/*** Do not recover when in danger ***/

	/* Look around for danger */
	p = borg_danger(py, px, 1);

	/* Never recover in dangerous situations */
	if (p > MAX(1,borg_avoid / borg_avoid_factor)) return (FALSE);


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
		    borg_zap_rod(SV_ROD_CURING))
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
		    borg_zap_rod(SV_ROD_CURING))
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
		    borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		    borg_quaff_potion(SV_POTION_CURE_CRITICAL))
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
	    (p <= 0) && borg_check_rest() &&
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

	int dir, x, y, ox, oy, i;

	byte feat;

	int o_y=0, o_x=0, door_found = 0;

	/* close all doors */
	if (borg_close_doors)
	{
		/* scan the adjacent grids */
		for (ox = -1; ox <= 1; ox++)
		{
			for (oy = -1; oy <= 1; oy++)
			{
				/* skip our own spot */
				if ((oy == 0) && (ox == 0)) continue;

				/* skip our orignal goal */
				if ((oy + py == y2) && (ox + px == x2)) continue;

				/* Acquire location */
				feat = borg_cave_feat[oy + py][ox + px];

				/* skip non open doors */
				if (feat != FEAT_OPEN) continue;

				/* skip monster on door */
				if (borg_cave_m_idx[oy + py][ox + px]) continue;

				/* Skip repeatedly closed doors */
				if (track_door_num >= 255) continue;

				/* save this spot */
				o_y = oy;
				o_x = ox;
				door_found = 1;
			}
		}

		/* Is there a door to close? */
		if (door_found)
		{
			/* Get a direction, if possible */
			dir = borg_goto_dir(py, px, py + o_y, px + o_x);

			/* Obtain the destination */
			x = px + ddx[dir];
			y = py + ddy[dir];

			/* Hack -- set goal */
			xb_ptr->gx = x;
			xb_ptr->gy = y;

			/* Close */
			borg_note("# Closing a door");
			borg_keypress('c');
			borg_keypress((char)I2D(dir));

			/* Check for an existing flag */
			for (i = 0; i < track_door_num; i++)
			{
				/* Stop if we already new about this door */
				if ((track_door_x[i] == x) && (track_door_y[i] == y)) return (TRUE);
			}

			/* Track the newly closed door */
			if (i == track_door_num && i < track_door_size)
			{
				borg_note("# Noting the closing of a door.");
				track_door_num++;
				track_door_x[i] = x;
				track_door_y[i] = y;
			}
			return (TRUE);
		}
	}

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
			borg_keypress((char)I2D(dir));
		}

		/* Sometimes stagger */
		else
		{
			/* Send action (stagger) */
			borg_keypress(';');

			/* Send direction */
			borg_keypress((char)I2D(dir));
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
		borg_keypress((char)I2D(dir));

		/* No longer on an object */
		borg_on_object = FALSE;

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
		borg_keypress((char)I2D(dir));

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
		borg_keypress((char)I2D(dir));

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
			borg_keypress((char)I2D(dir));

			/* Done */
			return (TRUE);
		}

		/* Message */
		borg_note("# Bashing a door");

		/* Send action (bash) */
		borg_keypress('B');

		/* Send direction */
		borg_keypress((char)I2D(dir));

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
			borg_keypress((char)I2D(dir));

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
		borg_keypress((char)I2D(dir));

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
		borg_keypress((char)I2D(dir));

		/* Done */
		return (TRUE);
	}


	/* Send action (walk) */
	borg_keypress(';');

	/* Send direction */
	borg_keypress((char)I2D(dir));

	/* No longer on an object */
	borg_on_object = FALSE;

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
		borg_keypress((char)I2D(dir));
	}

	/* Normally move */
	else
	{
		/* Send action (walk) */
		borg_keypress(';');

		/* Send direction */
		borg_keypress((char)I2D(dir));

		/* No longer on an object */
		borg_on_object = FALSE;

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

	/* Don't flee downstairs if we are deep */
	if (why == GOAL_FLEE && b_ptr->depth >= 5) return (FALSE);

	/* Don't go (much) past happy depth */
	if (b_ptr->depth > borg_happy_depth) return (FALSE);

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
 * Prepare to flow towards light
 */
bool borg_flow_light(int why)
{
    int y,x,i;
    

    /* reset counters */
    borg_glow_n = 0;
    i=0;

    /* build the glow array */
    /* Scan map */
    for (y = b_ptr->wy; y < b_ptr->wy + SCREEN_HGT; y++)
    {
        for (x = b_ptr->wx; x < b_ptr->wx + SCREEN_WID; x++)
        {
            /* Not a perma-lit, and not our spot. */
            if (y == b_ptr->py && x == b_ptr->px) continue;
            if (!(borg_cave_info[y][x] & CAVE_GLOW)) continue;

            /* keep count */
            borg_glow_y[borg_glow_n] = y;
            borg_glow_x[borg_glow_n] = x;
            borg_glow_n++;

        }
     }
    /* None to flow to */
    if (!borg_glow_n) return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < borg_glow_n; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(borg_glow_y[i], borg_glow_x[i]);
    }

    /* Spread the flow */
    borg_flow_spread(250, TRUE, FALSE);

    /* Attempt to Commit the flow */
    if (!borg_flow_commit("a lighted area", why)) return (FALSE);

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

    cptr message;


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
		if (p > borg_avoid / borg_avoid_factor) continue;

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


    message = "monsters";
    if (viewable) message = "visible monsters";

	/* Attempt to Commit the flow */
    if (!borg_flow_commit(message, GOAL_KILL)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_KILL)) return (FALSE);

	/* Success */
	return (TRUE);
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
 * Prepare to "flow" towards objects to "take", in the current panel -RML
 *
 * Note that objects under the player are always deleted
 */
bool borg_flow_take_near(bool viewable)
{
	int i, x, y;

	int py = b_ptr->py;
	int px = b_ptr->px;

	int y1, x1, y2, x2;


	/* Efficiency -- Nothing to take */
	if (!borg_takes_cnt) return (FALSE);


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

	/* Scan the object list */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		auto_take *take = &borg_takes[i];

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Access the location */
		x = take->x;
		y = take->y;

        /* Skip takes outside range -RML */
        if (x < x1) continue;
        if (y < y1) continue;
        if (x > x2) continue;
        if (y > y2) continue;

		/* Don't pick up bad objects */
		if (y == borg_bad_location_y && x == borg_bad_location_x) continue;

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

	/* Expand borders */
	y1--; x1--; y2++; x2++;

	/* Avoid the edges */
	borg_flow_border(y1, x1, y2, x2, TRUE);

    /* Spread the flow (limit depth) */
    borg_flow_spread(32, TRUE, FALSE);


	/* Clear the edges */
	borg_flow_border(y1, x1, y2, x2, FALSE);


	/* Attempt to Commit the flow */
    if (!borg_flow_commit("nearby objects", GOAL_TAKE)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_TAKE)) return (FALSE);

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

    cptr message;


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

		/* Don't pick up bad objects */
		if (y == borg_bad_location_y && x == borg_bad_location_x) continue;

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


    message = "objects";
    if (viewable) message = "visible objects";

	/* Attempt to Commit the flow */
    if (!borg_flow_commit(message, GOAL_TAKE)) return (FALSE);

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
	int ox, oy, i;


	/* Get the feature */
	feat = borg_cave_feat[y][x];


	/* Explore unknown grids */
	if (feat == FEAT_NONE) return (TRUE);


	/* Efficiency -- Ignore "boring" grids */
	if (feat < FEAT_TRAP_HEAD) return (FALSE);


	/* Efficiency -- Ignore secret doors */
	if (feat == FEAT_SECRET) return (FALSE);

	/* Efficiency -- Ignore other walls */
//	if (feat >= FEAT_WALL_EXTRA) return (FALSE);

	/* "Vaults" Explore non perma-walls adjacent to a perma wall */
    if (feat == FEAT_WALL_EXTRA || feat == FEAT_MAGMA ||
        feat == FEAT_QUARTZ)
    {
        /* Do not attempt when confused */
        if (borg_base_is_confused) return (FALSE);

        /* hack and cheat.  No vaults  on this level */
    //    if (!vault_on_level) return (FALSE);

        /* Do not attempt when shallow */
        /* if (auto_depth <= 30) return (FALSE);*/

        /* Do not attempt when on boring levels */
        /* if (auto_feeling <= 4) return (FALSE);*/

        /* This code is extremely expensive.  It uses a ton of time.
         * The 3 checks above should help to speed
         * him up a bit.
         */

        /* scan the adjacent grids */
        for (ox = -1; ox <= 1; ox++)
        {
            for (oy = -1; oy <= 1; oy++)
            {
                /* skip non perma grids wall */
                if (borg_cave_feat[oy+y][ox+x] != FEAT_PERM_INNER) continue;

                /* Allow "stone to mud" ability */
                if (borg_spell_legal(1, 8)) return (TRUE);

                /* Do not dig unless we appear strong enough to succeed XXX XXX XXX */
                if (b_ptr->skill_dig < ((borg_cave_feat[oy+y][ox+x] & 0x01) ? 20 : 10) + 5) return (FALSE);

                /* Glove up and dig in */
                return (TRUE);
            }
        }

    }


	/* Explore "closed doors" */
	if ((feat >= FEAT_DOOR_HEAD) &&
	    (feat <= FEAT_DOOR_TAIL))
	{
		/* Did I close this one */
		for (i = 0; i < track_door_num; i++)
		{
			/* mark as icky if I closed this one */
			if ((track_door_x[i] == x) && (track_door_y[i] == y))
			{
				/* not interesting */
				return (FALSE);
			}
		}
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
		if (borg_cave_danger[y][x] > borg_avoid / borg_avoid_factor) return (1);
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
    if (!borg_flow_commit("interesting grids (1-adjacent)", GOAL_DARK)) return (FALSE);

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
    if (!borg_flow_commit("interesting grids (2-untorch)", GOAL_DARK)) return (FALSE);

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
    if (!borg_flow_commit("interesting grids (3-near)", GOAL_DARK)) return (FALSE);

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
    if (!borg_flow_commit("interesting grids (4-near)", GOAL_DARK)) return (FALSE);

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
    if (!borg_flow_commit("interesting grids (7-panel)", GOAL_DARK)) return (FALSE);

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
    if (!borg_flow_commit("interesting grids (9-far)", GOAL_DARK)) return (FALSE);

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
		if (p > borg_avoid / (borg_avoid_factor * 2)) return (FALSE);
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

