/* File: borg6.c */
/* Purpose: Medium level stuff for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "borg1.h"
#include "borg2.h"
#include "borg3.h"
#include "borg4.h"
#include "borg5.h"
#include "borg6.h"



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
 *
 * Bugs:
 *   Currently the "twitchy()" function is not very smart
 *   We get "twitchy" when we are afraid of the monsters
 *   Annoyance and Danger are very different things (!)
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
    if ((ay <= 1) && (ay <= 1)) return (e);


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
 * This function was once a major bottleneck, so we now use several
 * slightly bizarre, but highly optimized, memory copying methods.
 */
static void borg_flow_clear(void)
{
    /* Reset the "cost" fields */
    COPY(auto_data_cost, auto_data_hard, auto_data);

    /* Wipe costs and danger */
    if (auto_danger_wipe)
    {
        /* Wipe the "know" flags */
        WIPE(auto_data_know, auto_data);

        /* Wipe the "icky" flags */
        WIPE(auto_data_icky, auto_data);

        /* Wipe complete */
        auto_danger_wipe = FALSE;
    }

    /* Start over */
    flow_head = 0;
    flow_tail = 0;
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
 * use a priority queue, this function might become unusably slow,
 * unless we reactivated the "room building" code.
 *
 * We handle both "walls" and "danger" by marking every grid which
 * is "impassible", due to either walls, or danger, as "ICKY", and
 * marking every grid which has been "checked" as "KNOW", allowing
 * us to only check the wall/danger status of any grid once.  This
 * provides some important optimization, since many "flows" can be
 * done before the "ICKY" and "KNOW" flags must be reset.
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
 * was currently included in any flow.
 *
 * If a "depth" is given, then the flow will only be spread to that
 * depth, note that the maximum legal value of "depth" is 250.
 */
static void borg_flow_spread(int depth, bool optimize, bool avoid)
{
    int i;
    int n, o = 0;
    int x1, y1;
    int x, y;


    /* Now process the queue */
    while (flow_head != flow_tail)
    {
        /* Extract the next entry */
        x1 = auto_flow_x[flow_tail];
        y1 = auto_flow_y[flow_tail];

        /* Circular queue -- dequeue the next entry */
        if (++flow_tail == AUTO_FLOW_MAX) flow_tail = 0;


        /* Cost (one per movement grid) */
        n = auto_data_cost->data[y1][x1] + 1;

        /* New depth */
        if (n > o)
        {
            /* Optimize (if requested) */
            if (optimize && (n > auto_data_cost->data[c_y][c_x])) break;

            /* Limit depth */
            if (n > depth) break;

            /* Save */
            o = n;
        }

        /* Queue the "children" */
        for (i = 0; i < 8; i++)
        {
            int old_head;

            auto_grid *ag;


            /* Neighbor grid */
            x = x1 + ddx_ddd[i];
            y = y1 + ddy_ddd[i];


            /* Skip "reached" grids */
            if (auto_data_cost->data[y][x] <= n) continue;


            /* Access the grid */
            ag = &auto_grids[y][x];


            /* Avoid "wall" grids (not doors) */
            if (ag->feat >= FEAT_SECRET) continue;


            /* Avoid unknown grids (if requested) */
            if (avoid && (ag->feat == FEAT_NONE)) continue;



            /* Ignore "icky" grids */
            if (auto_data_icky->data[y][x]) continue;


            /* Analyze every grid once */
            if (!auto_data_know->data[y][x])
            {
                int p;


                /* Mark as known */
                auto_data_know->data[y][x] = TRUE;


                /* Get the danger */
                p = borg_danger(y, x, 1);

                /* Dangerous grid */
                if (p > avoidance / 2)
                {
                    /* Mark as icky */
                    auto_data_icky->data[y][x] = TRUE;

                    /* Ignore this grid */
                    continue;
                }
            }


            /* Save the flow cost */
            auto_data_cost->data[y][x] = n;

            /* Enqueue that entry */
            auto_flow_x[flow_head] = x;
            auto_flow_y[flow_head] = y;


            /* Circular queue -- memorize head */
            old_head = flow_head;

            /* Circular queue -- insert with wrap */
            if (++flow_head == AUTO_FLOW_MAX)
                flow_head = 0;

            /* Circular queue -- handle overflow (badly) */
            if (flow_head == flow_tail)
                flow_head = old_head;
        }
    }

    /* Forget the flow info */
    flow_head = flow_tail = 0;
}



/*
 * Enqueue a fresh (legal) starting grid, if it is safe
 */
static void borg_flow_enqueue_grid(int y, int x)
{
    int old_head;

    /* Avoid icky grids */
    if (auto_data_icky->data[y][x]) return;

    /* Unknown */
    if (!auto_data_know->data[y][x])
    {
        /* Mark as known */
        auto_data_know->data[y][x] = TRUE;

        /* Mark dangerous grids as icky */
        if (borg_danger(y, x, 1) > avoidance / 2)
        {
            /* Icky */
            auto_data_icky->data[y][x] = TRUE;

            /* Avoid */
            return;
        }
    }


    /* Only enqueue a grid once */
    if (!auto_data_cost->data[y][x]) return;


    /* Save the flow cost (zero) */
    auto_data_cost->data[y][x] = 0;

    /* Enqueue that entry */
    auto_flow_y[flow_head] = y;
    auto_flow_x[flow_head] = x;


    /* Circular queue -- memorize head */
    old_head = flow_head;

    /* Circular queue -- insert with wrap */
    if (++flow_head == AUTO_FLOW_MAX) flow_head = 0;

    /* Circular queue -- handle overflow */
    if (flow_head == flow_tail) flow_head = old_head;
}



/*
 * Do a "reverse" flow from the player outwards
 */
static void borg_flow_reverse(void)
{
    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue the player's grid */
    borg_flow_enqueue_grid(c_y, c_x);

    /* Spread, but do NOT optimize */
    borg_flow_spread(250, FALSE, FALSE);
}





/*
 * Attempt to induce "word of recall"
 * artifact activations added throughout this code
 */
bool borg_recall(void)
{
    /* Multiple "recall" fails */
    if (!goal_recalling)
    {
        /* Try to "recall" */
        if (borg_spell(5, 4) ||
            borg_prayer(4, 4) ||
            borg_zap_rod(SV_ROD_RECALL) ||
            borg_read_scroll(SV_SCROLL_WORD_OF_RECALL) ||
            borg_activate_artifact(ART_AVAVIR, INVEN_WIELD))
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
        auto_item *item = &auto_items[i];

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
        auto_item *item = &auto_items[i];

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

	/* Scan the inventory for "potions" food */
    for (i = 0; i < INVEN_PACK; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip unknown food */
        if (!item->kind) continue;

        /* Skip non-food */
        if (item->tval != TV_POTION) continue;

        /* Consume in order, when hurting */
        if (auto_chp < 4 &&
            (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_quaff_potion(SV_POTION_HEALING) ||
            borg_quaff_potion(SV_POTION_STAR_HEALING) ||
            borg_quaff_potion(SV_POTION_LIFE) ))
        {
            return (TRUE);
        }
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
    if (!auto_depth && track_more_num)
    {
        /* Love the stairs! */
        d = double_distance(y, x, track_more_y[0], track_more_x[0]);

        /* Proximity is good */
        f += (1000 - d);

        /* Close proximity is great */
        if (d < 4) f += (2000 - (d * 500));
    }

    /* Hack -- chase Up Stairs in dungeon */
    if (auto_depth && track_less_num)
    {
        /* Love the stairs! */
        d = double_distance(y, x, track_less_y[0], track_less_x[0]);

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
 * to pillars, or where we have previously stepped.
 * Stairs are good because they can be used to leave
 * the level.  Corridors are good because you can back into them
 * to avoid groups of monsters and because they can be used for
 * escaping.  Pillars are good because while standing next to a
 * pillar, you can walk "around" it in two different directions,
 * allowing you to retreat from a single normal monster forever.
 * Previous steps are good to avoid traps and it also tends to be
 * clear of bad guys.
 */
static bool borg_happy_grid_bold(int y, int x)
{
    int i;

    auto_grid *ag = &auto_grids[y][x];


    /* Accept stairs */
    if (ag->feat == FEAT_LESS) return (TRUE);
    if (ag->feat == FEAT_MORE) return (TRUE);
    if (ag->feat == FEAT_GLYPH) return (TRUE);

    /* Hack -- weak/dark is very unhappy */
    if (do_weak || !my_cur_lite) return (FALSE);

    /* Apply a control effect so that he does not get stuck in a loop */
    if ((c_t - auto_began) >= 2000)  return (FALSE);

    /* Case 1a: north-south corridor */
    if (borg_cave_floor_bold(y-1, x) && borg_cave_floor_bold(y+1, x) &&
        !borg_cave_floor_bold(y, x-1) && !borg_cave_floor_bold(y, x+1) &&
        !borg_cave_floor_bold(y+1, x-1) && !borg_cave_floor_bold(y+1, x+1) &&
        !borg_cave_floor_bold(y-1, x-1) && !borg_cave_floor_bold(y-1, x+1))
    {
        /* Happy */
        return (TRUE);
    }

    /* Case 1b: east-west corridor */
    if (borg_cave_floor_bold(y, x-1) && borg_cave_floor_bold(y, x+1) &&
        !borg_cave_floor_bold(y-1, x) && !borg_cave_floor_bold(y+1, x) &&
        !borg_cave_floor_bold(y+1, x-1) && !borg_cave_floor_bold(y+1, x+1) &&
        !borg_cave_floor_bold(y-1, x-1) && !borg_cave_floor_bold(y-1, x+1))
    {
        /* Happy */
        return (TRUE);
    }

    /* Case 1aa: north-south doorway */
    if (borg_cave_floor_bold(y-1, x) && borg_cave_floor_bold(y+1, x) &&
        !borg_cave_floor_bold(y, x-1) && !borg_cave_floor_bold(y, x+1))
    {
        /* Happy */
        return (TRUE);
    }

    /* Case 1ba: east-west doorway */
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

    /* check for grids that have been stepped on before */
    for (i = 0; i < track_step_num; i++)
    {
        /* Enqueue the grid */
        if ((track_step_y[i] == y) &&
            (track_step_x[i] == x))
        {
            /* Recent step is good */
            if (i < 25)
            {
                return (TRUE);
            }
        }
     }


    /* Not happy */
    return (FALSE);
}
/*
 * Hack -- evaluate the likelihood of the borg getting surrounded
 * by a bunch of monsters.  This is called from borg_danger() when
 * he looking for a strategic retreat.  It is hopeful that the borg
 * will see that several monsters are approaching him and he may
 * become surrouned then die.  This routine looks at near by monsters
 * as determines the likelyhood of him getting surrouned.
 */
static bool borg_surrounded(void)
{
    auto_kill *kill;
    monster_race *r_ptr;

    int safe_grids = 8;
    int non_safe_grids = 0;
    int monsters = 0;
    int adjacent_monsters = 0;

    int x9, y9, ax, ay, d;
    int i;

    /* Evaluate the local monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        kill = &auto_kills[i];
        r_ptr = &r_info[kill->r_idx];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        x9 = kill->x;
        y9 = kill->y;

        /* Distance components */
        ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
        ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

        /* Distance */
        d = MAX(ax, ay);

        /* if the monster is too far then skip it. */
        if (d > 3) continue;

        /* if he cant see me then forget it.*/
        if (!borg_los(c_y, c_x, y9, x9)) continue;

        /* if asleep, don't consider this one */
        if (!kill->awake) continue;

        /* Monsters with Pass Wall are dangerous, no escape from them */
        if (r_ptr->flags2 & RF2_PASS_WALL) continue;
        if (r_ptr->flags2 & RF2_KILL_WALL) continue;

        /* Monsters who never move cant surround */
        if (r_ptr->flags1 & RF1_NEVER_MOVE) continue;

        /* keep track of monsters touching me */
        if (d == 1) adjacent_monsters ++;

        /* Add them up. */
        monsters ++;

    }

    /* Evaluate the Non Safe Grids, (walls, closed doors, traps, monsters) */
    for (i = 0; i < 8; i++)
    {
        int x = c_x + ddx_ddd[i];
        int y = c_y + ddy_ddd[i];

        /* Access the grid */
        auto_grid *ag = &auto_grids[y][x];

        /* Skip walls/doors */
        if (!borg_cave_floor_grid(ag)) non_safe_grids ++;

        /* Skip unknown grids */
        if (ag->feat == FEAT_NONE) non_safe_grids ++;

        /* Skip monster grids */
        if (ag->kill) non_safe_grids ++;

        /* Mega-Hack -- skip stores XXX XXX XXX */
        if ((ag->feat >= FEAT_SHOP_HEAD) && (ag->feat <= FEAT_SHOP_TAIL)) non_safe_grids ++;

        /* Mega-Hack -- skip traps XXX XXX XXX */
        if ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL)) non_safe_grids ++;

    }

    /* Safe grids are decreased */
    safe_grids = safe_grids - non_safe_grids;

    /* Am I in hallway? If so don't worry about it */
    if (safe_grids == 1 && adjacent_monsters == 1) return (FALSE);

    /* I am likely to get surrouned */
    if (monsters > safe_grids)
    {
        borg_note(format("# Possibility of being surrounded (%d/%d)",
        monsters, safe_grids));

        /* The borg can get trapped by breeders by continueing to flee
         * into a dead-end.  So he needs to be able to trump this
         * routine.
         */
        if (goal_ignoring) return (FALSE);
        else return (TRUE);
    }

    /* Probably will not be surrouned */
    return (FALSE);
}

/* This will look down a hallway and possibly light it up using
 * the Light Beam mage spell.  This spell is mostly used when
 * the borg is moving through the dungeon under boosted bravery.
 * This will allow him to "see" if anyone is there.
 *
 * It might also come in handy if he's in a hallway and gets shot, or
 * if resting in a hallway.  He may want to cast it to make
 * sure no previously unknown monsters are in the hall.
 * NOTE:  ESP will alter the value of this spell.
 *
 * Borg has a problem when not on map centering mode and casting the beam
 * repeatedly, down or up when at the edge of a panel.
 */
bool borg_lite_beam(bool simulation)
{
    int dir = 5;
    bool spell_ok = FALSE;

    auto_grid *ag = &auto_grids[c_y][c_x];

    /* Hack -- weak/dark is very unhappy */
    if (do_weak || !my_cur_lite) return (FALSE);

    /* Apply a control effect so that he does not get stuck in a loop */
    if ((c_t - auto_began) >= 2000)  return (FALSE);

    /* Require the ability */
    if (borg_spell_okay_fail(1,5, 20) ||
        (-1 != borg_slot(TV_WAND, SV_WAND_LITE) &&
            auto_items[borg_slot(TV_WAND, SV_WAND_LITE)].pval) ||
        borg_equips_rod(SV_ROD_LITE))
        spell_ok = TRUE;

    /* North */
    switch (my_cur_lite)
    {

    /* Torch */
    case 1:
        ag = &auto_grids[c_y- (my_cur_lite +1)][c_x];
        if (borg_cave_floor_bold(c_y - 1,c_x) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 8;
            break;
        }
    /* Lantern */
    case 2:
        ag = &auto_grids[c_y- (my_cur_lite +1)][c_x];
        if (borg_cave_floor_bold(c_y - 1,c_x) &&
        borg_cave_floor_bold(c_y - 2,c_x) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 8;
            break;
        }
    /* Artifact */
    case 3:
        ag = &auto_grids[c_y- (my_cur_lite +1)][c_x];

        if (borg_cave_floor_bold(c_y - 1,c_x) &&
        borg_cave_floor_bold(c_y - 2,c_x) &&
        borg_cave_floor_bold(c_y - 3,c_x) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 8;
            break;
        }
    }

    /* South */
    switch (my_cur_lite)
    {
    case 1:
        ag = &auto_grids[c_y + (my_cur_lite +1)][c_x];
        if (borg_cave_floor_bold(c_y + 1,c_x) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 2;
            break;
        }
    /* Lantern */
    case 2:
        ag = &auto_grids[c_y + (my_cur_lite +1)][c_x];
        if (borg_cave_floor_bold(c_y + 1,c_x) &&
        borg_cave_floor_bold(c_y + 2,c_x) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 2;
            break;
        }
    /* Artifact */
    case 3:
        ag = &auto_grids[c_y + (my_cur_lite +1)][c_x];
        if (borg_cave_floor_bold(c_y + 1,c_x) &&
        borg_cave_floor_bold(c_y + 2,c_x) &&
        borg_cave_floor_bold(c_y + 3,c_x) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 2;
            break;
        }
    }

    /* East */
    switch (my_cur_lite)
    {
    /* Torch */
    case 1:
        ag = &auto_grids[c_y][c_x+(my_cur_lite +1)];
        if (borg_cave_floor_bold(c_y ,c_x +1) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 6;
            break;
        }
    /* Lantern */
    case 2:
        ag = &auto_grids[c_y][c_x+(my_cur_lite +1)];
        if (borg_cave_floor_bold(c_y ,c_x + 1) &&
        borg_cave_floor_bold(c_y,c_x + 2) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 6;
            break;
        }
    /* Artifact */
    case 3:
        ag = &auto_grids[c_y][c_x+(my_cur_lite +1)];
        if (borg_cave_floor_bold(c_y ,c_x + 1) &&
        borg_cave_floor_bold(c_y ,c_x + 2) &&
        borg_cave_floor_bold(c_y ,c_x + 3) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 6;
            break;
        }
    }

    /* West */
    switch (my_cur_lite)
    {
    /* Torch */
    case 1:
        ag = &auto_grids[c_y][c_x-(my_cur_lite +1)];
        if (borg_cave_floor_bold(c_y ,c_x -1) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 4;
            break;
        }
    /* Lantern */
    case 2:
        ag = &auto_grids[c_y][c_x-(my_cur_lite +1)];
        if (borg_cave_floor_bold(c_y ,c_x - 1) &&
        borg_cave_floor_bold(c_y,c_x - 2) &&
        !ag->feat == FEAT_FLOOR  && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 4;
            break;
        }
    /* Artifact */
    case 3:
        ag = &auto_grids[c_y][c_x-(my_cur_lite +1)];
        if (borg_cave_floor_bold(c_y ,c_x - 1) &&
        borg_cave_floor_bold(c_y ,c_x - 2) &&
        borg_cave_floor_bold(c_y ,c_x - 3) &&
        !ag->feat == FEAT_FLOOR && ag->feat < FEAT_DOOR_HEAD)
        {
            /* note the direction */
            dir = 4;
            break;
        }
    }

    /* Dont do it if: */
    if (dir == 5 || spell_ok == FALSE ||
       (dir == 2 && (c_y == 18 || c_y == 19  ||
                     c_y == 29 || c_y == 30  ||
                     c_y == 40 || c_y == 41  ||
                     c_y == 51 || c_y == 52))||
       (dir == 8 && (c_y == 13 || c_y == 14  ||
                     c_y == 24 || c_y == 25  ||
                     c_y == 35 || c_y == 36  ||
                     c_y == 46 || c_y == 47)))
       return (FALSE);

    /* simulation */
    if (simulation) return (TRUE);

    /* cast the light beam */
    if (borg_spell(1,5) ||
         borg_zap_rod(SV_ROD_LITE) ||
         borg_aim_wand(SV_WAND_LITE))
        {   /* apply the direction */
            borg_keypress(I2D(dir));
            borg_note("# Illuminating this hallway");
            return(TRUE);
        }

    /* cant do it */
    return (FALSE);
}

/*
 * is there a unique nearby? (check auto_kills)
 * This is used to keep us in fights and make us use all
 * our 'spare' equiptment to kill uniques
 * This only works for uniques we know about.  If one of the
 * monsters around is misidentified then it may be a unique
 * and we wouldn't know.
 */
static int borg_near_unique(int dist)
{
    auto_kill *kill;
    monster_race *r_ptr;
    int x9, y9, ax, ay, d;
    int i;

    /* make sure there is a unique around */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        kill = &auto_kills[i];
        r_ptr = &r_info[kill->r_idx];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /*  hack -- forget about Maggot */
        if (!auto_depth) continue;

        x9 = kill->x;
        y9 = kill->y;

        /* Distance components */
        ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
        ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

        /* Distance */
        d = MAX(ax, ay);

        /* if the unique is too far then skip it. */
        if (d > dist) continue;

        /* if he cant see me then forget it */
        if (kill->r_idx != 547 && !borg_projectable(c_y, c_x, y9, x9)) continue;

        /* Mark our flag that there is one on the level */
        unique_on_level = TRUE;

        /* found one.  Done. */
        if (kill->r_idx != 547 && r_ptr->flags1 & RF1_UNIQUE) return (1);
        if (kill->r_idx == 547 && r_ptr->flags1 & RF1_UNIQUE) return (2);

    }

    /* no nearby uniques */
    return (0);
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

    /* make sure there is a summoner around */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        kill = &auto_kills[i];
        r_ptr = &r_info[kill->r_idx];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        x9 = kill->x;
        y9 = kill->y;

        /* Distance components */
        ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
        ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

        /* Distance */
        d = MAX(ax, ay);

        /* if the summoner is too far then skip it. */
        if (d > dist) continue;

        /* if he cant see me then forget it */
        if (!borg_projectable(c_y, c_x, y9, x9)) continue;

        /* found one.  Done. */
    if ( (r_ptr->flags6 & RF6_S_MONSTER) ||
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
    return FALSE;
}


/*
 * Help determine if "phase door" seems like a good idea
 */
bool borg_caution_phase(int emergency)
{
    int n, k, i, d, x, y, p;

    int dis = 10;
    int min = dis / 2;

    auto_grid *ag;

    /* No phase with Ethereal Lock */
    if (borg_lock) return (FALSE);

    /* Simulate 100 attempts */
    for (n = k = 0; k < 100; k++)
    {
        /* Pick a location */
        for (i = 0; i < 100; i++)
        {
            /* Pick a (possibly illegal) location */
            while (1)
            {
                y = rand_spread(c_y, dis);
                x = rand_spread(c_x, dis);
                d = distance(c_y, c_x, y, x);
                if ((d >= min) && (d <= dis)) break;
            }

            /* Ignore illegal locations */
            if ((y <= 0) || (y >= AUTO_MAX_Y - 1)) continue;
            if ((x <= 0) || (x >= AUTO_MAX_X - 1)) continue;

            /* Access */
            ag = &auto_grids[y][x];

            /* Skip unknown grids */
            if (ag->feat == FEAT_NONE) continue;

            /* Skip weird grids */
            if (ag->feat == FEAT_INVIS) continue;

            /* Skip walls */
            if (!borg_cave_floor_bold(y, x)) continue;

            /* Skip monsters */
            if (ag->kill) continue;

            /* Stop looking */
            break;
        }

        /* No location */
        /* in the real code it would keep trying but here we should */
        /* assume that there is unknown spots that you would be able */
        /* to go but may be dangerious. */
        if (i >= 100)
        {
            n++;
            continue;
        }

        /* Examine */
        p = borg_danger(y, x, 2);

        /* if *very* scary, do not allow jumps at all */
        if (p > auto_chp) n++;
    }

    /* Too much danger */
    /* in an emergency try with extra danger allowed */
    if  (n > emergency)
    {
        borg_note(format("# No Phase. scary squares: %d", n));
        return (FALSE);
    }
    else
        borg_note(format("# Safe to Phase. scary squares: %d", n));

    /* Okay */
    return (TRUE);
}



/*
 * Try to phase door or teleport or by using Yeek Flee in Terror
 * b_q is the danger of the least dangerious square around us.
 */
bool borg_escape(int b_q)
{

    /* no phase with Ethereal Lock */
    if (borg_lock) return (FALSE);

    /* Borg has a nasty habit of teleporting right before he gets to his goal shop
     * when he is deathly ill.  So sadly we have to trump this.
     */
     if ((do_poisoned || do_cut || do_weak) && auto_depth ==0) return (FALSE);

    /* 1. really scary, I'm about to die */
    /* Try an emergency teleport, or phase door as last resort */
    if ( do_heavy_stun ||
         ((b_q >= avoidance * 20/10) && borg_fighting_unique ==2 ) ||
         ((b_q >= avoidance * 17/10) && borg_fighting_unique ==1 ) ||
         ((b_q >= avoidance * 15/10) && !borg_fighting_unique) )
    {
        /* only escape with spell if fail is low */
        int allow_fail = 5;

        /* if very healthy, allow extra fail */
        if ((auto_chp*100)/auto_mhp > 70)
            allow_fail = 10;

        /* comprimised, do not allow fail */
        if (do_heavy_stun)
            allow_fail = 1;

        if (borg_spell_fail(2, 6, allow_fail) ||
            borg_spell_fail(5, 1, allow_fail) ||
            borg_prayer_fail(1, 1, allow_fail) ||
            borg_prayer_fail(4, 1, allow_fail) ||
            borg_read_scroll(SV_SCROLL_TELEPORT) ||
            borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
            borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
            (amt_phase && borg_caution_phase(65) &&
            (borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
             borg_activate_artifact(ART_BELEGENNON,INVEN_BODY)||
             borg_spell_fail(1, 4, allow_fail)  ||
             borg_prayer_fail(4, 0, allow_fail))))
        {
            /* Flee! */
           borg_note("# Danger Level 1.");
           return (TRUE);
        }
    }
    /* 1a. really scary, I'm about to die Critical check*/
    /* Try an emergency teleport (This will take us to negative mana but, */
    /* what the hell, I am about to die anyway. */
    if ( do_heavy_stun ||
         ((b_q >= avoidance * 20/10) && borg_fighting_unique == 2 ) ||
         ((b_q >= avoidance * 17/10) && borg_fighting_unique == 1 ) ||
         ((b_q >= avoidance * 15/10) && !borg_fighting_unique) )
    {
        int sv_mana = auto_csp;

        auto_csp = auto_msp;

        /* try to teleport, get far away from here */
        if (borg_prayer_fail(1, 1, 15) ||
            borg_prayer_fail(4, 1, 15) ||
            borg_spell_fail(2, 6, 15) ||
            borg_spell_fail(5, 1, 15))
        {
            /* verify use of spell */
            borg_keypress('y');

            /* Flee! */
            borg_note("# Danger Level 1.  Critical Attempt");
            return (TRUE);
        }
        /* emergency phase spell */
        else if (amt_phase && borg_caution_phase(80) &&
                 (borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
                 borg_activate_artifact(ART_BELEGENNON,INVEN_BODY)))
        {
                /* Flee! */
                borg_note("# Danger Level 1.  Critical Phase");
                return (TRUE);
        }
        /* emergency phase spell */
        else if (borg_caution_phase(80) &&
                 (borg_spell_fail(1, 4, 15)  ||
                  borg_prayer_fail(4, 0, 15)))
        {
            /* verify use of spell */
            borg_keypress('y');

            /* Flee! */
            borg_note("# Danger Level 1.  Critical Attempt");
            return (TRUE);
        }
        auto_csp = sv_mana;
    }

    /* 2 - a bit more scary */
    /* Attempt to teleport (usually) */
    /* do not escape from uniques so quick */
    if ( do_heavy_stun ||
         ((b_q >= avoidance * 15/10) && borg_fighting_unique ==1 ) ||
         ((b_q >= avoidance * 13/10) && !borg_fighting_unique) )
    {
        /* only escape with spell if fail is low */
        int allow_fail = 7;

        /* if very healthy, allow extra fail */
        if ((auto_chp*100)/auto_mhp > 70)
            allow_fail = 20;

        /* very scary, do not allow fail */
        if (do_heavy_stun)
            allow_fail = 1;

        /* Try teleportation */
        if ( borg_spell_fail(2, 6, allow_fail) ||
             borg_spell_fail(5, 1, allow_fail) ||
             borg_prayer_fail(4, 1, allow_fail) ||
             borg_prayer_fail(1, 1, allow_fail) ||
             borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
             borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
             borg_read_scroll(SV_SCROLL_TELEPORT))
        {
            /* Flee! */
            borg_note("# Danger Level 2.");

            /* Success */
            return (TRUE);
        }
        /* Phase door, if useful */
        if (amt_phase && borg_caution_phase(50) &&
            (borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
             borg_spell_fail(1, 4, allow_fail) ||
             borg_prayer_fail(4, 0, allow_fail) ||
             borg_activate_artifact(ART_BELEGENNON,INVEN_BODY)||
             borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ))
        {
            /* Flee! */
            borg_note("# Danger Level 2.");
            /* Success */
            return (TRUE);
        }

    }

    /* 3- not too bad */
    /* also run if stunned or it is scary here */
    if ( do_heavy_stun ||
         ((b_q >= avoidance * 13/10) && borg_fighting_unique ==1) ||
         ((b_q >= avoidance * 10/10) && !borg_fighting_unique) ||
         ((b_q >= avoidance) && do_afraid && (amt_missile <=0 &&
           auto_class == CLASS_WARRIOR) ))
    {
        /* only escape with spell if fail is low */
        int allow_fail = 7;

        /* if very healthy, allow extra fail */
        if ((auto_chp*100)/auto_mhp > 70)
            allow_fail = 20;

        /* very scary, do not allow fail */
        if (do_heavy_stun)
            allow_fail = 2;

        /* Phase door, if useful */
        if (amt_phase && borg_caution_phase(25) &&
             (borg_spell_fail(1, 4, allow_fail) ||
              borg_prayer_fail(4, 0, allow_fail) ||
              borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
              borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
              borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
        {
            /* Flee! */
            borg_note("# Danger Level 3.");

            /* Success */
            return (TRUE);
        }

        /* Teleport via spell */
        if ( borg_spell_fail(2, 6, allow_fail) ||
             borg_spell_fail(5, 1, allow_fail) ||
             borg_prayer_fail(1, 1, allow_fail) ||
             borg_prayer_fail(4, 1, allow_fail) ||
             borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
             borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
             borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
             borg_read_scroll(SV_SCROLL_TELEPORT))
        {
            /* Flee! */
            borg_note("# Danger Level 3.");

            /* Success */
            return (TRUE);
        }
        /* Phase door, if useful */
        if (amt_phase && borg_caution_phase(65) &&
             (borg_spell_fail(1, 4, allow_fail) ||
              borg_prayer_fail(4, 0, allow_fail) ||
              borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
              borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
              borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
        {
            /* Flee! */
            borg_note("# Danger Level 3.");

            /* Success */
            return (TRUE);
        }

        /* if we got this far we tried to escape but couldn't... */
        /* time to flee */
        if (!goal_fleeing && (!borg_fighting_unique || auto_level < 35) && !vault_on_level)
        {
            /* Note */
            borg_note("# Fleeing (failed to teleport)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }

        /* Flee now */
        if (!goal_leaving && (!borg_fighting_unique || auto_level < 35) && !vault_on_level)
        {
            /* Flee! */
            borg_note("# Leaving (failed to teleport)");

            /* Start leaving */
            goal_leaving = TRUE;
        }

    }
    /* 4- not too scary but I'm comprimized */
    if ( (b_q >= avoidance * 8 /10 && auto_chp < auto_mhp /3) ||
         (b_q >= avoidance * 12/10 && borg_fighting_unique ==1 &&
           auto_chp <= auto_mhp / 3) ||
         (b_q >= avoidance * 6 /10 && auto_level <= 20 && !borg_fighting_unique) ||
         (b_q >= avoidance * 6 /10 && auto_class == CLASS_MAGE && auto_level <= 35))
    {
        /* only escape with spell if fail is low */
        int allow_fail = 10;

        /* if very healthy, allow extra fail */
        if ((auto_chp*100)/auto_mhp > 70)
            allow_fail = 30;

        /* very scary, do not allow fail */
        if ((b_q > avoidance) || do_heavy_stun)
            allow_fail = 5;

        /* Phase door, if useful */
        if (amt_phase && borg_caution_phase(20) &&
             (borg_spell_fail(1, 4, allow_fail) ||
              borg_prayer_fail(4, 0, allow_fail) ||
              borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
              borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
              borg_read_scroll(SV_SCROLL_PHASE_DOOR) ))
        {
            /* Flee! */
            borg_note("# Danger Level 4.");
            /* Success */
            return (TRUE);
        }

        /* Teleport via spell */
        if ( borg_spell_fail(2, 6, allow_fail) ||
             borg_prayer_fail(1, 1, allow_fail) ||
             borg_prayer_fail(4, 1, allow_fail) ||
             borg_spell_fail(5, 1, allow_fail) ||
             borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
             borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
             borg_read_scroll(SV_SCROLL_TELEPORT) ||
             borg_use_staff_fail(SV_STAFF_TELEPORTATION) )
        {
            /* Flee! */
            borg_note("# Danger Level 4.");

            /* Success */
            return (TRUE);
        }

        /* if we got this far we tried to escape but couldn't... */
        /* time to flee */
        if (!goal_fleeing && !borg_fighting_unique && auto_level < 35 && !vault_on_level)
        {
            /* Note */
            borg_note("# Fleeing (failed to teleport)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }

        /* Flee now */
        if (!goal_leaving && !borg_fighting_unique && !vault_on_level)
        {
            /* Flee! */
            borg_note("# Leaving (failed to teleport)");

            /* Start leaving */
            goal_leaving = TRUE;
        }
        /* Emergency Phase door if a weak mage */
        if ((auto_class == CLASS_MAGE && auto_level <=35 ) &&
            amt_phase && borg_caution_phase(65) &&
             (borg_spell_fail(1, 4, allow_fail) ||
              borg_prayer_fail(4, 0, allow_fail) ||
              borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
              borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
              borg_read_scroll(SV_SCROLL_PHASE_DOOR) ))
        {
            /* Flee! */
            borg_note("# Danger Level 4.");
            /* Success */
            return (TRUE);
        }

    }

    /* 5- not too scary but I'm comprimized */
    if (auto_level < 10 &&
        (b_q >= avoidance * 6 /10 ||
        (b_q >= avoidance * 8/10 && borg_fighting_unique ==1 )))
    {
        /* only escape with spell if fail is low */
        int allow_fail = 10;

        /* if very healthy, allow extra fail */
        if ((auto_chp*100)/auto_mhp > 70)
            allow_fail = 30;

        /* very scary, do not allow fail */
        if ((b_q > avoidance) || do_heavy_stun)
            allow_fail = 5;

        /* Phase door, if useful */
        if (amt_phase && borg_caution_phase(20) &&
             (borg_spell_fail(1, 4, allow_fail) ||
              borg_prayer_fail(4, 0, allow_fail) ||
              borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
              borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
              borg_read_scroll(SV_SCROLL_PHASE_DOOR) ))
        {
            /* Flee! */
            borg_note("# Danger Level 5.");
            /* Success */
            return (TRUE);
        }

        /* Teleport via spell */
        if ( borg_spell_fail(2, 6, allow_fail) ||
             borg_prayer_fail(1, 1, allow_fail) ||
             borg_prayer_fail(4, 1, allow_fail) ||
             borg_spell_fail(5, 1, allow_fail) ||
             borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
             borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
             borg_read_scroll(SV_SCROLL_TELEPORT) ||
             borg_use_staff_fail(SV_STAFF_TELEPORTATION) )
        {
            /* Flee! */
            borg_note("# Danger Level 5.");

            /* Success */
            return (TRUE);
        }

        /* if we got this far we tried to escape but couldn't... */
        /* time to flee */
        if (!goal_fleeing && !borg_fighting_unique && auto_level < 35 && !vault_on_level)
        {
            /* Note */
            borg_note("# Fleeing (failed to teleport)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }

        /* Flee now */
        if (!goal_leaving && !borg_fighting_unique && !vault_on_level)
        {
            /* Flee! */
            borg_note("# Leaving (failed to teleport)");

            /* Start leaving */
            goal_leaving = TRUE;
        }
        /* Emergency Phase door if a weak mage */
        if ((auto_class == CLASS_MAGE && auto_level <=35 ) &&
            amt_phase && borg_caution_phase(65) &&
             (borg_spell_fail(1, 4, allow_fail) ||
              borg_prayer_fail(4, 0, allow_fail) ||
              borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
              borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
              borg_read_scroll(SV_SCROLL_PHASE_DOOR) ))
        {
            /* Flee! */
            borg_note("# Danger Level 5.");
            /* Success */
            return (TRUE);
        }

    }

    /* Yeek Flee in Terror */
    if ( auto_race == RACE_YEEK && (!borg_speed) &&
        (((b_q >= avoidance * 8 /10) && auto_chp <= auto_mhp / 3) ||
         ((b_q >= avoidance * 12/10) && borg_fighting_unique) ||
         ((b_q >= avoidance * 8 /10) && auto_level <= 20 && !borg_fighting_unique) ||
         ((b_q >= avoidance * 8 /10) && auto_class == CLASS_MAGE && auto_level <= 35)))
    {
        /* Make sure we have enough hit points to do it! */
        if (auto_chp > 3 && !do_confused)
        {
            borg_note("# Danger Level 4.  Yeek Flee.");
            borg_keypress('U');
            borg_keypress('a');
            return (TRUE);
        }
    }

    return (FALSE);
}


/*
 * ** Try healing **
 * this function tries to heal the borg before trying to flee.
 */
static bool borg_heal(int danger )
{
    int hp_down;
    int allow_fail = 20;
    int chance;

    hp_down = auto_mhp - auto_chp;


    /*  Hack -- heal when confused. This is deadly.*/
    /* This is checked twice, once, here, to see if he is in low danger
     * and again at the end of borg_caution, when all other avenues have failed */
    if (do_confused && (rand_int(100) < 85))
    {
        if ((hp_down >= 300) && danger - 300 < auto_chp &&
            borg_quaff_potion(SV_POTION_HEALING))
        {
            return (TRUE);
        }
        if (danger - 20 < auto_chp  &&
           (borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_quaff_potion(SV_POTION_HEALING) ||
            borg_use_staff_fail(SV_STAFF_HEALING)))
        {
            return (TRUE);
        }
    }
    /*  Hack -- heal when blind. This is deadly.*/
    if (do_blind && (rand_int(100) < 85))
    {
        if ((hp_down >= 300) && borg_quaff_potion(SV_POTION_HEALING))
        {
            return (TRUE);
        }
        if (borg_eat_food(SV_FOOD_CURE_BLINDNESS) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_quaff_potion(SV_POTION_HEALING) ||
            borg_use_staff_fail(SV_STAFF_HEALING))
        {
            return (TRUE);
        }
    }

    /* Generall try to conserve ez-heal potions */
    if ((do_blind || do_confused) &&
        (hp_down >= 400 || (danger > auto_chp *4 && hp_down > 100)) &&
        borg_quaff_potion(SV_POTION_STAR_HEALING))
    {
        return (TRUE);
    }

    /* restore Mana */
    /* note, blow the staff charges easy because the staff will not last. */
    if (auto_csp < (auto_msp / 5) && (rand_int(100) < 50))
    {
        if (borg_use_staff_fail(SV_STAFF_THE_MAGI))
        {
            borg_note("# Use Magi Staff");
            return (TRUE);
        }
    }
    /* blowing potions is harder */
    /* NOTE: must have enough mana to keep up GOI or do a HEAL */
    if (auto_csp < (auto_msp / 10) ||
       ((auto_csp < 70) && (borg_goi == 3)) )
    {
            /*  use the potion if battling a unique and not too dangerous
             *  or I am out of teleports and in danger
             */
            if ((borg_fighting_unique && danger < avoidance *2) ||
                (amt_teleport == 0 && danger > avoidance))
            {
                if (borg_use_staff_fail(SV_STAFF_THE_MAGI) ||
                    borg_quaff_potion(SV_POTION_RESTORE_MANA))
                {
                    borg_note("# Restored my mana");
                    return (TRUE);
                }
            }
    }
    /*  Hack -- rest until healed */
    if ( (!do_blind && !do_poisoned && !do_cut && !borg_goi &&
          !borg_shield && !borg_bless &&
          !do_weak && !do_hungry && danger < avoidance/5) &&
         (do_confused || do_image || do_afraid || do_stun || do_heavy_stun ||
          auto_chp < auto_mhp || auto_csp < auto_msp*3/4)  &&
         borg_check_rest() &&
         danger <= auto_fear_region[c_y/11][c_x/11])
    {
        /* check for then call lite in dark room before resting */
        if (!borg_check_lite_only())
        {
            /* Take note */
            borg_note(format("# Resting to restore HP/SP..."));

            /* Rest until done */
            borg_keypress('R');
            borg_keypress('&');
            borg_keypress('\n');

            /* Reset our panel clock, we need to be here */
            time_this_panel =0;

            /* Done */
            return (TRUE);
        }
        else
        {
            /* Must have been a dark room */
            borg_note(format("# Lighted the darkened room instead of resting."));
            return (TRUE);
        }
     }

    /* if unhurt no healing needed */
    if (hp_down == 0)
        return FALSE;

    /* Healing and fighting Morgoth.  Do before "chance"*/
    if (auto_chp <= 550 && borg_fighting_unique == 2 &&
        (borg_prayer_fail(3,5, 5) ||
         borg_use_staff_fail(SV_STAFF_HOLINESS) ||
         borg_use_staff_fail(SV_STAFF_HEALING) ||
         borg_zap_rod(SV_ROD_HEALING) ||
         borg_quaff_potion(SV_POTION_HEALING) ||
         borg_quaff_potion(SV_POTION_STAR_HEALING) ||
         borg_prayer_fail(6, 2, 15) ||
         borg_prayer_fail(3, 2, 15) ||
         borg_prayer_fail(3,5, 20) ||
         borg_quaff_potion(SV_POTION_LIFE) ||
         borg_activate_artifact(ART_SOULKEEPER,INVEN_BODY) ||
         borg_activate_artifact(ART_GONDOR,INVEN_HEAD)) )
    {
        return (TRUE);
    }

    /* Hack -- heal when wounded a percent of the time */
    /* down 4/5 hp 0%                      */
    /* 3/4 hp 2%                           */
    /* 2/3 hp 20%                          */
    /* 1/2 hp 50%                          */
    /* 1/3 hp 75%                          */
    /* 1/4 hp 100%                         */

    chance = rand_int(100);

    /* if we are fighting a unique increase the odds of healing */
    if (borg_fighting_unique) chance -= 10;

    /* if Ethereal Locked we cant teleport, increase the odds */
    if (borg_lock) chance -=10;

    /* if danger is close to the hp and healing will help, do it */
    if (danger >= auto_chp && danger < auto_mhp )  chance -= 25;

    if (!(((auto_chp <= ((auto_mhp * 4) / 5)) && (chance < 0)) ||
            ((auto_chp <= ((auto_mhp * 3) / 4)) && (chance < 2)) ||
            ((auto_chp <= ((auto_mhp * 2) / 3)) && (chance < 20)) ||
            ((auto_chp <= (auto_mhp / 2)) && (chance < 50)) ||
            ((auto_chp <= (auto_mhp / 3)) && (chance < 75)) ||
             (auto_chp <= (auto_mhp / 4)) ||
             do_heavy_stun || do_stun || do_poisoned || do_cut))
            return FALSE;


    /* Cure light Wounds (2d10) */
    if ( hp_down < 10 &&
         ((danger - 6) < auto_chp) &&
         (borg_prayer_fail(0, 1, allow_fail) ||
          borg_spell_fail(0,4,allow_fail) ||
          borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
          borg_activate_artifact(ART_LOTHARANG,INVEN_WIELD) ) )
    {
        return (TRUE);
    }
    /* Cure Serious Wounds (4d10) */
    if ( hp_down < 20 &&
         ((danger - 18) < auto_chp) &&
         (borg_prayer_fail(1, 2, allow_fail) ||
          borg_quaff_potion(SV_POTION_CURE_SERIOUS)))
    {
        return (TRUE);
    }

    /* Cure Critical Wounds (6d10) */
    if ( hp_down < 50 &&
         ((danger -  40) < auto_chp) &&
         (borg_prayer_fail(2, 2, allow_fail) ||
          borg_prayer_fail(6, 0, allow_fail) ||
          borg_quaff_potion(SV_POTION_CURE_CRITICAL)))
    {
        return (TRUE);
    }

    /* Cure Mortal Wounds (8d10) */
    if ( hp_down < 120 &&
         ((danger - 100) < auto_chp) &&
         (borg_prayer_fail(2, 7, allow_fail) ||
          borg_prayer_fail(6, 1, allow_fail) ||
          borg_quaff_potion(SV_POTION_CURE_CRITICAL) ))
    {
        return (TRUE);
    }

    /* Generally continue to heal.  But if we are preparing for the end
     * game uniques, then bail out here in order to save our heal pots.
     * Priests wont need to bail, they have ez heals.
     */
    if (auto_max_depth >=98 && !borg_fighting_unique &&
        auto_class != CLASS_PRIEST)
        /* Bail out to save the heal pots for Morgoth*/
        {
            return (FALSE);
        }

    /* Heal step one (200hp) */
    if (hp_down < 250 &&
        ((danger - 180) < auto_chp) &&
        (borg_zap_rod(SV_ROD_HEALING) ||
         borg_activate_artifact(ART_SOULKEEPER,INVEN_BODY) ||
         borg_activate_artifact(ART_GONDOR,INVEN_HEAD) ||
         borg_use_staff_fail(SV_STAFF_HOLINESS) ||
         borg_use_staff_fail(SV_STAFF_HEALING) ||
         borg_prayer_fail(3, 2, allow_fail) ||
         borg_quaff_potion(SV_POTION_HEALING) ))
    {
        return (TRUE);
    }

    /* Heal step two (300hp) */
    if (hp_down < 350 &&
        ((danger - 300) < auto_chp) &&
        (borg_use_staff_fail(SV_STAFF_HEALING) ||
         (borg_fighting_unique && borg_prayer_fail(3,5, allow_fail)) ||
         borg_use_staff_fail(SV_STAFF_HOLINESS) ||
         borg_prayer_fail(3, 2, allow_fail) ||
         borg_zap_rod(SV_ROD_HEALING) ||
         borg_quaff_potion(SV_POTION_HEALING)))
    {
        return (TRUE);
    }

    /* Healing step three (300hp).  */
    if ((hp_down < 650 && danger < auto_chp + 300) &&
         ((borg_fighting_unique && borg_prayer_fail(3,5, allow_fail)) ||
         borg_zap_rod(SV_ROD_HEALING) ||
         borg_prayer_fail(6, 2, allow_fail) ||
         borg_prayer_fail(3, 2, allow_fail) ||
         borg_use_staff_fail(SV_STAFF_HOLINESS) ||
         borg_use_staff_fail(SV_STAFF_HEALING) ||
         borg_quaff_potion(SV_POTION_HEALING) ||
         borg_activate_artifact(ART_SOULKEEPER,INVEN_BODY) ||
         borg_activate_artifact(ART_GONDOR,INVEN_HEAD)) )
    {
        return (TRUE);
    }

    /* Healing final check. stage 1*/
    if ((hp_down >= 650 && danger < auto_chp + 300) &&
        ((borg_fighting_unique && borg_prayer_fail(3,5, allow_fail)) ||
         borg_zap_rod(SV_ROD_HEALING) ||
         borg_prayer_fail(6, 2, allow_fail) ||
         borg_prayer_fail(3, 2, allow_fail) ||
         borg_use_staff_fail(SV_STAFF_HOLINESS) ||
         borg_use_staff_fail(SV_STAFF_HEALING) ||
         borg_quaff_potion(SV_POTION_HEALING) ||
         borg_activate_artifact(ART_SOULKEEPER,INVEN_BODY) ||
         borg_activate_artifact(ART_GONDOR,INVEN_HEAD)) )
    {
        return (TRUE);
    }

    /* nothing to do */
    return (FALSE);

}

/*
 * Be "cautious" and attempt to prevent death or dishonor.
 *
 * Strategy:
 *
 *   (1) Caution
 *   (1a) Analyze the situation
 *   (1a1) try to heal
 *   (1a2) try a defence
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
 * could die in a single round, we set the "goal_fleeing" flag, and if
 * we could die in two rounds, we set the "goal_leaving" flag.
 *
 * In town, whenever we could die in two rounds if we were to stay still,
 * we set the "goal_leaving" flag.  In combination with the "retreat" and
 * the "back away" code, this should allow us to leave town before getting
 * into situations which might be fatal.
 *
 * Flag "goal_fleeing" means get off this level right now, using recall
 * if possible when we get a chance, and otherwise, take stairs, even if
 * it is very dangerous to do so.
 *
 * Flag "goal_leaving" means get off this level when possible, using
 * stairs if possible when we get a chance.
 *
 * We will also take stairs if we happen to be standing on them, and we
 * could die in two rounds.  This is often "safer" than teleportation,
 * and allows the "retreat" code to retreat towards stairs, knowing that
 * once there, we will leave the level.
 *
 * If we can, we should try to hit a monster with an offset spell.
 * A Druj can not move but they are really dangerous.  So we should retreat
 * to a happy grid (meaning we have los and it does not), we should target
 * one space away from the bad guy then blast away with ball spells.
 */
bool borg_caution(void)
{
    int j, p;

    /* if on level 100 and not ready for Morgoth, run */
    if (auto_depth == 100)
    {
        if (borg_ready_morgoth == 0 && !borg_king)
        {
            /* Start leaving */
            if (!goal_leaving)
            {
                /* Note */
                borg_note("# Leaving (Not ready for Morgoth now)");

                /* Start leaving */
                goal_leaving = TRUE;
            }
        }
    }

    /*** Evaluate local danger ***/

    /* am I fighting a unique or a summoner? */
    borg_fighting_unique = borg_near_unique(5);
    borg_fighting_summoner = borg_near_summoner(10);

    /* Look around */
    p = borg_danger(c_y, c_x, 1);

    /* Describe (briefly) the current situation */
    /* Danger (ignore stupid "fear" danger) */
    if ((p > avoidance / 5) || (p > auto_fear_region[c_y/11][c_x/11]))
    {
        /* Describe (briefly) the current situation */
        borg_note(format("# Loc:%d,%d Dep:%d Lev:%d HP:%d/%d SP:%d/%d Danger:p=%d",
                         c_x, c_y, auto_depth, auto_level,
                         auto_chp, auto_mhp, auto_csp, auto_msp,
                         p));
        /* Comment on glyph */
        if (track_glyph_num)
        {
            int i;
            for (i = 0; i < track_glyph_num; i++)
            {
                /* Enqueue the grid */
                if ((track_glyph_y[i] == c_y) &&
                    (track_glyph_x[i] == c_x))
                    {
                        /* if standing on one */
                        borg_note(format("# Standing on Glyph"));
                    }
            }
        }
        if (borg_goi)
        {
        borg_note(format("# Protected by GOI"));
        }
        if (borg_shield)
        {
        borg_note(format("# Protected by Mystic Shield"));
        }
        if (borg_prot_from_evil)
        {
        borg_note(format("# Protected by PFE"));
        }
        if (borg_lock)
        {
        borg_note(format("# Protected by Ethereal Lock"));
        }
    }

    /* try healing before running away */
    if (borg_heal(p))
        return TRUE;

    /* do some defence before running away! */
    if (borg_defend(p))
        return TRUE;

    /* do some swapping before running away! */
    if (p > (avoidance / 3))
    {
        if (borg_backup_swap(p))
            return TRUE;
    }

    /* If I am waiting for recall,  & safe, then stay put. */
    if (goal_recalling && borg_check_rest())
        {
            /* rest here until lift off */
            borg_keypress('R');
            borg_keypress('5');
            borg_keypress('0');
            borg_keypress('0');
            borg_keypress('\n');
        }

    /*** Danger ***/

    /* Impending doom */
    /* Don't take off in the middle of a fight */
    /* just to restock and it is useless to restock */
    /* if you have just left town. */
    if (borg_restock(auto_depth) &&
        !borg_fighting_unique &&
        (auto_time_town + (c_t - auto_began)) > 200)
    {
        /* Start leaving */
        if (!goal_leaving)
        {
            /* Note */
            borg_note(format("# Leaving (restock) %s", borg_restock(auto_depth)));

            /* Start leaving */
            goal_leaving = TRUE;
        }

        /* Start fleeing */
        if (!goal_fleeing)
        {
            /* Note */
            borg_note(format("# Fleeing (restock) %s", borg_restock(auto_depth)));

            /* Start fleeing */
            goal_fleeing = TRUE;
        }
    }
    /* Excessive danger */
    else if (p > (auto_chp * 2))
    {
        /* Start fleeing */
        /* do not flee level if going after Morgoth or fighting a unique */
        if (!goal_fleeing && !borg_fighting_unique && (auto_level < 50) &&
            !vault_on_level && (auto_depth < 100 && borg_ready_morgoth == 1))
        {
            /* Note */
            borg_note("# Fleeing (excessive danger)");

            /* Start fleeing */
            goal_fleeing = TRUE;
        }
    }
    /* Potential danger (near death) in town */
    else if (!auto_depth && (p > auto_chp) && (auto_level < 50) )
    {
        /* Flee now */
        if (!goal_leaving)
        {
            /* Flee! */
            borg_note("# Leaving (potential danger)");

            /* Start leaving */
            goal_leaving = TRUE;
        }
    }


    /*** Stairs ***/

    /* Leaving or Fleeing, take stairs */
    if (goal_leaving || goal_fleeing)
    {
        /* Take next stairs */
        stair_less = goal_fleeing;
        if (borg_ready_morgoth == 0 && !borg_king)
            stair_less = TRUE;

        /* Only go down if fleeing or prepared, but not when starving.
         * or lacking on food */
        if ((cptr)NULL == borg_prepared(auto_depth+1))
            stair_more = TRUE;
        if (do_hungry || do_weak || !my_cur_lite)
              stair_more = FALSE;
        /* if fleeing town, then dive */
        if (!auto_depth) stair_more = TRUE;
    }

    /* Take stairs up */
    if (stair_less || (p > auto_chp / 2))
    {
        /* Current grid */
        auto_grid *ag = &auto_grids[c_y][c_x];

        /* Usable stairs */
        if (ag->feat == FEAT_LESS)
        {
            /* Take the stairs */
            borg_keypress('<');

            /* Success */
            return (TRUE);
        }
    }

    /* Take stairs down */
    if ((stair_more || (p > auto_chp / 2)) && !goal_recalling)
    {
        /* Current grid */
        auto_grid *ag = &auto_grids[c_y][c_x];

        /* Usable stairs */
        if (ag->feat == FEAT_MORE)
        {
            /* Take the stairs */
            borg_keypress('>');

            /* Success */
            return (TRUE);
        }
    }



    /*** Deal with critical situations ***/

    /* Hack -- require light */
    if (!my_lite)
    {
        auto_item *item = &auto_items[INVEN_LITE];

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
        if (auto_depth && (item->pval < 250))
        {
            /* Start leaving */
            if (!goal_leaving)
            {
                /* Flee */
                borg_note("# Leaving (need fuel)");

                /* Start leaving */
                goal_leaving = TRUE;
            }

            /* Start fleeing */
            if (!goal_fleeing)
            {
                /* Flee */
                borg_note("# Fleeing (need fuel)");

                /* Start fleeing */
                goal_fleeing = TRUE;
            }
        }
    }

    /* Hack -- prevent starvation */
    if (do_weak)
    {
        /* Attempt to satisfy hunger */
        if (borg_eat_food_any() ||
            borg_spell(2, 7) ||
            borg_prayer(2, 0))
        {
            /* Success */
            return (TRUE);
        }

        /* Try to restore mana then cast the spell next round */
        if (borg_quaff_potion(SV_POTION_RESTORE_MANA)) return (TRUE);

        /* Flee for food */
        if (auto_depth)
        {
            /* Start leaving */
            if (!goal_leaving)
            {
                /* Flee */
                borg_note("# Leaving (need food)");

                /* Start leaving */
                goal_leaving = TRUE;
            }

            /* Start fleeing */
            if (!goal_fleeing)
            {
                /* Flee */
                borg_note("# Fleeing (need food)");

                /* Start fleeing */
                goal_fleeing = TRUE;
            }
        }
    }


    /*** Flee on foot ***/

    /* Strategic retreat */
    if (p > avoidance / 3 || (borg_surrounded() && p !=0))
    {
        int d, b_d = -1;
        int r, b_r = -1;

        int b_x = c_x;
        int b_y = c_y;


        /* Scan the useful viewable grids */
        for (j = 1; j < auto_view_n; j++)
        {
            int x1 = c_x;
            int y1 = c_y;

            int x2 = auto_view_x[j];
            int y2 = auto_view_y[j];


            /* Not if confused, cant predict the motion */
            if (do_confused) continue;

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
                auto_grid *ag;

                /* Obtain direction */
                d = borg_goto_dir(y1, x1, y2, x2);

                /* Verify direction */
                if ((d == 0) || (d == 5)) break;

                /* Track distance */
                r++;

                /* Simulate the step */
                y1 += ddy[d];
                x1 += ddx[d];

                /* Obtain the grid */
                ag = &auto_grids[y1][x1];

                /* Require floor */
                if (!borg_cave_floor_grid(ag)) break;

                /* Require line of sight */
                if (!borg_projectable(y1, x1, y2, x2)) break;

                /* Check danger (over time) =*/
                if (borg_danger(y1, x1, r+1) > p) break;

                /* make sure it is not dangerous to take the first step. */
                if (r == 1 && borg_danger(y1, x1, 1) >= avoidance / 3)  break;

                /* Skip monsters */
                if (ag->kill) break;

                /* Skip traps */
                if ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL)) break;


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
            b_d = borg_goto_dir(c_y, c_x, b_y, b_x);

            /* Hack -- set goal */
            g_x = c_x + ddx[b_d];
            g_y = c_y + ddy[b_d];

            /* Note */
            borg_note(format("# Retreating to %d,%d (distance %d) via %d,%d (%d >= %d)",
                             b_x, b_y, b_r, g_x, g_y, p, borg_danger(g_y, g_x, 2)));

            /* Strategic retreat */
            borg_keypress(I2D(b_d));

            /* Success */
            return (TRUE);
        }
    }

    /*** Escape if possible ***/

    /* Attempt to escape */
    if (borg_escape(p))
    {
        /* Success */
        return (TRUE);
    }

    /* Back away, we just failed to escape */
    if (p > avoidance / 3 || (borg_surrounded() && p != 0))
    {
        int i, b_i = -1;
        int k, b_k = -1;
        int f, b_f = -1;

        /* Current danger */
        b_k = p;

        /* Check the freedom */
        b_f = borg_freedom(c_y, c_x);

        /* Attempt to find a better grid */
        for (i = 0; i < 8; i++)
        {
            int x = c_x + ddx_ddd[i];
            int y = c_y + ddy_ddd[i];

            /* Access the grid */
            auto_grid *ag = &auto_grids[y][x];

            /* Not if confused, cant predict the motion */
            if (do_confused) continue;

            /* Skip walls/doors */
            if (!borg_cave_floor_grid(ag)) continue;

            /* Skip monster grids */
            if (ag->kill) continue;

            /* Mega-Hack -- skip stores XXX XXX XXX */
            if ((ag->feat >= FEAT_SHOP_HEAD) && (ag->feat <= FEAT_SHOP_TAIL)) continue;

            /* Mega-Hack -- skip traps XXX XXX XXX */
            if ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL)) continue;

            /* Extract the danger there */
            k = borg_danger(y, x, 2);

            /* Skip higher danger */
            if (b_k < k) continue;

            /* Check the freedom there */
            f = borg_freedom(y, x);

            /* Danger is the same */
            if (b_k == k)
            {
                /* If I am low level, reward backing-up if safe */
                if (auto_level < 3 && (auto_chp < auto_mhp ||
                    auto_csp < auto_csp))
                {
                        /* do consider the retreat */
                }
                else
                {
                    /* Freedom of my grid is better than the next grid
                     * so stay put and fight.
                     */
                    if (b_f > f) continue;
                }
            }

            /* Save the info */
            b_i = i; b_k = k; b_f = f;
        }

        /* Back away */
        if (b_i >= 0)
        {
            /* Hack -- set goal */
            g_x = c_x + ddx_ddd[b_i];
            g_y = c_y + ddy_ddd[b_i];

            /* Note */
            borg_note(format("# Backing up to %d,%d (%d > %d)",
                             g_x, g_y, p, borg_danger(g_y, g_x, 2)));

            /* Back away from danger */
            borg_keypress(I2D(ddd[b_i]));

            /* Success */
            return (TRUE);
        }

    }

    /*** Cures ***/

    /* Hack -- cure poison when poisoned */
    if (do_poisoned && auto_chp < auto_mhp /3)
    {
        if (borg_spell_fail(4, 4, 60) ||
            borg_prayer_fail(3, 0, 60) ||
            borg_quaff_potion(SV_POTION_CURE_POISON) ||
            borg_activate_artifact(ART_DAL,INVEN_FEET) ||
            borg_eat_food(SV_FOOD_CURE_POISON) ||
            borg_use_staff(SV_STAFF_CURING) ||
            borg_zap_rod(SV_ROD_CURING))
        {
            borg_note("# Attempting to cure poison.");
            return (TRUE);
        }

        /* buy time */
          if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_eat_food(SV_FOOD_WAYBREAD) ||
            borg_spell_fail(1, 3, 60) ||
            borg_prayer_fail(0,8,60) ||
            borg_prayer_fail(0,1,60) ||
            borg_spell_fail(0,4,60) ||
            borg_use_staff_fail(SV_STAFF_HEALING))
        {
            borg_note("# Attempting to slow poison.");
            return (TRUE);
        }
    }


    /* Hack -- cure poison when poisoned CRITICAL CHECK
     */
    if (do_poisoned && (auto_chp < 5))
    {
        int sv_mana = auto_csp;

        auto_csp = auto_msp;

        if (borg_spell(1, 3) ||
            borg_prayer(0,8) ||
            borg_prayer(0,1) ||
            borg_spell(0,4))
        {
            /* verify use of spell */
            borg_keypress('y');

            /* Flee! */
            borg_note("# Emergency Cure Poison! Gasp!!!....");

            return (TRUE);
        }
        auto_csp = sv_mana;

        /* Quaff healing pots to buy some time- in this emergency.  */
        if (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_HEALING) ||
            borg_quaff_potion(SV_POTION_STAR_HEALING) ||
            borg_quaff_potion(SV_POTION_LIFE)) return (TRUE);

        /* Try to Restore Mana */
        if (borg_quaff_potion(SV_POTION_RESTORE_MANA)) return (TRUE);

        /* Quaff unknown potions in this emergency.  We might get luck */
        if (borg_quaff_unknown()) return (TRUE);

        /* Eat unknown mushroom in this emergency.  We might get luck */
        if (borg_eat_unknown()) return (TRUE);

        /* Use unknown Staff in this emergency.  We might get luck */
        if (borg_use_unknown()) return (TRUE);

    }

    /* Hack -- cure wounds when bleeding, also critical check */
    if (do_cut && (auto_chp < auto_mhp/3 || (rand_int(100) < 30))  &&
        p < avoidance)
    {
        if (borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
            borg_spell(0,4) ||
            borg_prayer(1,2) ||
            borg_prayer(2,7) ||
            borg_prayer(6,1) ||
            borg_prayer(0,1))
        {
            return (TRUE);
        }
    }
    /* bleeding and about to die CRITICAL CHECK*/
    if (do_cut && (auto_chp < 2))
    {
        int sv_mana = auto_csp;

        auto_csp = auto_msp;

        if (borg_spell(0, 4) ||
            borg_prayer(0,1) ||
            borg_prayer(1, 2))
        {
            /* verify use of spell */
            borg_keypress('y');

            /* Flee! */
            borg_note("# Emergency Wound Patch! Gasp!!!....");

            return (TRUE);
        }
        auto_csp = sv_mana;

        /* Quaff healing pots to buy some time- in this emergency.  */
        if (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_HEALING) ||
            borg_quaff_potion(SV_POTION_STAR_HEALING) ||
            borg_quaff_potion(SV_POTION_LIFE)) return (TRUE);


        /* Try to Restore Mana */
        if (borg_quaff_potion(SV_POTION_RESTORE_MANA)) return (TRUE);

        /* Quaff unknown potions in this emergency.  We might get luck */
        if (borg_quaff_unknown()) return (TRUE);

        /* Eat unknown mushroom in this emergency.  We might get luck */
        if (borg_eat_unknown()) return (TRUE);

        /* Use unknown Staff in this emergency.  We might get luck */
        if (borg_use_unknown()) return (TRUE);
    }

    /* cure confusion, second check, first (slightly different) in borg_heal */
    if (do_confused)
    {
        if (auto_mhp-auto_chp >= 300 &&
            (borg_quaff_potion(SV_POTION_HEALING) ||
             borg_quaff_potion(SV_POTION_STAR_HEALING) ||
             borg_quaff_potion(SV_POTION_LIFE)))
        {
            return (TRUE);
        }
        if (borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_quaff_potion(SV_POTION_HEALING) ||
            borg_use_staff(SV_STAFF_HEALING))
        {
            return (TRUE);
        }
    }

    /* Hack -- cure fear when afraid */
    if (do_afraid &&
       (rand_int(100) < 70 ||
        (auto_class == CLASS_WARRIOR && amt_missile <=0)))
    {
        if (borg_prayer(0, 3) ||
            borg_quaff_potion(SV_POTION_BOLDNESS) ||
            borg_quaff_potion(SV_POTION_HEROISM) ||
            borg_quaff_potion(SV_POTION_BESERK_STRENGTH) ||
            borg_spell(7, 0) ||
            borg_activate_artifact(ART_DAL,INVEN_FEET) )
        {
            return (TRUE);
        }
    }


    /*** Note impending death XXX XXX XXX ***/

    /* Flee from low hit-points */
    if (((auto_chp < auto_mhp / 3) ||
        ((auto_chp < auto_mhp / 2) && auto_chp < (auto_level *3) )) &&        (amt_cure_critical < 3) &&
        (amt_heal < 1))
    {
        /* Flee from low hit-points */
        if (auto_depth && (rand_int(100) < 25))
        {
            /* Start leaving */
            if (!goal_leaving)
            {
                /* Flee */
                borg_note("# Leaving (low hit-points)");

                /* Start leaving */
                goal_leaving = TRUE;
            }

            /* Start fleeing */
            if (!goal_fleeing)
            {
                /* Flee */
                borg_note("# Fleeing (low hit-points)");

                /* Start fleeing */
                goal_fleeing = TRUE;
            }
        }
    }

    /* Flee from bleeding wounds or poison and no heals */
    if ((do_cut && auto_chp < auto_mhp / 3) ||
        (do_poisoned && amt_cure_poison <= 0))
    {
        /* Flee from bleeding wounds */
        if (auto_depth && (rand_int(100) < 25))
        {
            /* Start leaving */
            if (!goal_leaving)
            {
                /* Flee */
                borg_note("# Leaving (bleeding/posion)");

                /* Start leaving */
                goal_leaving = TRUE;
            }

            /* Start fleeing */
            if (!goal_fleeing)
            {
                /* Flee */
                borg_note("# Fleeing (bleeding/poison)");

                /* Start fleeing */
                goal_fleeing = TRUE;
            }
        }
    }

    /* Final and emergency heal check.  We generally try to preserve our
     * ez_heal pots, but if we are going to die, then use them up now.
     * All other attempts to save the borg have failed.
     */
    if ((auto_chp < auto_mhp/10 ||
         (amt_teleport + amt_escape ==0 && auto_chp < auto_mhp/4)) &&
        p > auto_chp * 2 &&
        (borg_quaff_potion(SV_POTION_HEALING) ||
         borg_quaff_potion(SV_POTION_STAR_HEALING) ||
         borg_quaff_potion(SV_POTION_LIFE) ))
    {
         return (TRUE);
    }

    /* Hack -- use "recall" to flee if possible */
    if (goal_fleeing && auto_depth && (borg_recall()))
    {
        /* Note */
        borg_note("# Fleeing the level (recall)");

        /* Success */
        return (TRUE);
    }

   /* If I am waiting for recall,and in danger, buy time with
     * phase and cure_anythings.
     */
     if (goal_recalling && (p > avoidance * 3))
     {
		 if (borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
		     borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
             borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
		     borg_quaff_potion(SV_POTION_CURE_LIGHT))
		     {
				 borg_note("# Buying time waiting for Recall.");
				 return (TRUE);
			 }
	 }

    /* if I am gonna die next round, and I have no way to escape
     * use the unknown stuff (if I am low level).
     */
    if (p > (auto_chp * 4) && auto_level < 20 && !auto_msp)
    {
        if (borg_use_unknown()||
           borg_quaff_unknown() ||
           borg_eat_unknown()) return (TRUE);

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
 *   Using wands
 *   Using rods
 *   Using staffs
 *   Using scrolls
 *   Activating Artifacts APW
 *   Activate Dragon Armour APW
 */
enum
{
    BF_LAUNCH_NORMAL,
    BF_LAUNCH_SEEKER,
    BF_LAUNCH_ANIMAL,
    BF_LAUNCH_EVIL,
    BF_LAUNCH_UNDEAD,
    BF_LAUNCH_DEMON,
    BF_LAUNCH_ORC,
    BF_LAUNCH_GIANT,
    BF_LAUNCH_TROLL,
    BF_LAUNCH_DRAGON,
    BF_LAUNCH_FLAME,
    BF_LAUNCH_FROST,
    BF_LAUNCH_WOUNDING,
    BF_OBJECT,

    BF_THRUST,
    BF_SPELL_MAGIC_MISSILE,
    BF_SPELL_MAGIC_MISSILE_RESERVE,
    BF_SPELL_ELEC_BOLT,
    BF_SPELL_COLD_BOLT,
    BF_SPELL_FIRE_BOLT,
    BF_SPELL_ACID_BOLT,
    BF_SPELL_SLOW_MONSTER,
    BF_SPELL_CONFUSE_MONSTER,
    BF_SPELL_STONE_TO_MUD,
    BF_SPELL_POLYMORPH,
    BF_SPELL_LITE_BEAM,
    BF_SPELL_SLEEP_I,
    BF_SPELL_STRIKING,
    BF_SPELL_HOLD_FOE,
    BF_SPELL_MANA_BOLT,

    BF_SPELL_CONFUSION,

    BF_SPELL_POISON_BALL,
    BF_SPELL_COLD_BALL,
    BF_SPELL_ACID_BALL,
    BF_SPELL_FIRE_BALL,
    BF_SPELL_COLD_STORM,
    BF_SPELL_METEOR_STORM,
    BF_SPELL_MANA_STORM,
    BF_SPELL_PLASMA_STORM,

    BF_PRAYER_SPIRIT_HAMMER,
    BF_PRAYER_SPIRIT_HAMMER_RESERVE,
    BF_PRAYER_SCARE_CREATURE,
    BF_PRAYER_BLIND_CREATURE,
    BF_PRAYER_SANCTUARY,
    BF_PRAYER_HOLY_ORB_BALL,
    BF_PRAYER_DISP_UNDEAD1,
    BF_PRAYER_DISP_EVIL1,
    BF_PRAYER_HOLY_WORD,
    BF_PRAYER_DISP_UNDEAD2,
    BF_PRAYER_DISP_EVIL2,
    BF_PRAYER_DRAIN_LIFE,
    BF_PRAYER_DIVINE_INTERVENTION,

    BF_ROD_ELEC_BOLT,
    BF_ROD_COLD_BOLT,
    BF_ROD_ACID_BOLT,
    BF_ROD_FIRE_BOLT,
    BF_ROD_LITE_BEAM,
    BF_ROD_DRAIN_LIFE,
    BF_ROD_ELEC_BALL,
    BF_ROD_COLD_BALL,
    BF_ROD_ACID_BALL,
    BF_ROD_FIRE_BALL,
    BF_ROD_SLOW_MONSTER,
    BF_ROD_SLEEP_MONSTER,

    BF_STAFF_SLEEP_MONSTERS,
    BF_STAFF_SLOW_MONSTERS,
    BF_STAFF_DISPEL_EVIL,
    BF_STAFF_POWER,
    BF_STAFF_HOLINESS,

    BF_WAND_MAGIC_MISSILE,
    BF_WAND_ELEC_BOLT,
    BF_WAND_COLD_BOLT,
    BF_WAND_ACID_BOLT,
    BF_WAND_FIRE_BOLT,
    BF_WAND_SLOW_MONSTER,
    BF_WAND_SLEEP_MONSTER,
    BF_WAND_CONFUSE_MONSTER,
    BF_WAND_FEAR_MONSTER,
    BF_WAND_ANNIHILATION,
    BF_WAND_DRAIN_LIFE,
    BF_WAND_LITE_BEAM,
    BF_WAND_STINKING_CLOUD,
    BF_WAND_ELEC_BALL,
    BF_WAND_COLD_BALL,
    BF_WAND_ACID_BALL,
    BF_WAND_FIRE_BALL,
    BF_WAND_WONDER,
    BF_WAND_DRAGON_COLD,
    BF_WAND_DRAGON_FIRE,

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
    BF_ART_TOTILA,
    BF_ART_FIRESTAR,
    BF_ART_TURMIL,
    BF_ART_RAZORBACK,
    BF_ART_CAMMITHRIM,
    BF_ART_PAURHACH,
    BF_ART_PAURNIMMEN,
    BF_ART_HOLCOLLETH,
    BF_ART_CELEBORN,
    BF_ART_PAURAEGEN,
    BF_ART_PAURNEN,
    BF_ART_FINGOLFIN,
    BF_ART_INGWE,
    BF_ART_NARYA,
    BF_ART_NENYA,
    BF_ART_VILYA,

    BF_DRAGON_BLUE,
    BF_DRAGON_WHITE,
    BF_DRAGON_BLACK,
    BF_DRAGON_GREEN,
    BF_DRAGON_RED,
    BF_DRAGON_MULTIHUED,
    BF_DRAGON_BRONZE,
    BF_DRAGON_GOLD,
    BF_DRAGON_CHAOS,
    BF_DRAGON_LAW,
    BF_DRAGON_BALANCE,
    BF_DRAGON_SHINING,
    BF_DRAGON_POWER,

    BF_RACIAL_PIXIE,
    BF_RACIAL_BOLT_CRYSTAL,
    BF_RACIAL_BOLT_COPPER,
    BF_RACIAL_BOLT_BRONZE,
    BF_RACIAL_BOLT_GOLD,
    BF_RACIAL_BOLT_PSEUDO,

    BF_RACIAL_BALL_CRYSTAL,
    BF_RACIAL_BALL_COPPER,
    BF_RACIAL_BALL_BRONZE,
    BF_RACIAL_BALL_GOLD,
    BF_RACIAL_BALL_PSEUDO,
    BF_RACIAL_BALL_MULTI_FIRE,
    BF_RACIAL_BALL_MULTI_COLD,
    BF_RACIAL_BALL_MULTI_ELEC,
    BF_RACIAL_BALL_MULTI_ACID,
    BF_RACIAL_BALL_MULTI_POIS,


    BF_MAX
};



/*
 * Guess how much damage a physical attack will do to a monster
 */
static int borg_thrust_damage_one(int i)
{
    int dam;
    int mult;

    auto_kill *kill;

    monster_race *r_ptr;

    auto_item *item;

    int chance;

    /* Examine current weapon */
    item = &auto_items[INVEN_WIELD];

    /* Monster record */
    kill = &auto_kills[i];

    /* Monster race */
    r_ptr = &r_info[kill->r_idx];

    /* Damage */
    dam = (item->dd * (item->ds + 1) / 2);

    /* dragons are different */
    if (auto_race >= RACE_MIN_DRAGON)
    {
        dam = p_ptr->lev+adj_drag[p_ptr->stat_ind[A_DEX]] - 128 *
              p_ptr->lev+adj_drag[p_ptr->stat_ind[A_STR]] - 128;
    }
    /* here is the place for slays and such */
    mult = 1;

    if (my_slay_animal && (r_ptr->flags3 & RF3_ANIMAL))
        mult = 2;
    if (my_slay_evil && (r_ptr->flags3 & RF3_EVIL))
        mult = 2;
    if (((my_slay_undead) && (r_ptr->flags3 & RF3_ANIMAL)) ||
       ((my_slay_demon) && (r_ptr->flags3 & RF3_DEMON)) ||
       ((my_slay_orc) && (r_ptr->flags3 & RF3_ORC)) ||
       ((my_slay_troll) && (r_ptr->flags3 & RF3_TROLL)) ||
       ((my_slay_giant) && (r_ptr->flags3 & RF3_GIANT)) ||
       ((my_slay_dragon) && (r_ptr->flags3 & RF3_DRAGON)))
       mult = 3;
    if (((my_brand_acid) && !(r_ptr->flags3 & RF3_IM_ACID)) ||
       ((my_brand_fire) && !(r_ptr->flags3 & RF3_IM_FIRE)) ||
       ((my_brand_cold) && !(r_ptr->flags3 & RF3_IM_COLD)) ||
       ((my_brand_elec) && !(r_ptr->flags3 & RF3_IM_ELEC)))
        mult = 3;

    /* add the multiplier */
        dam *= mult;

	/* Critical Hits ? */

    /* add weapon bonuses */
    dam += item->to_d;

    /* add player bonuses */
    dam += my_to_dam;

    /* multiply the damage for the whole round of attacks */
    dam *= my_num_blow;

    /* reduce for % chance to hit (AC) */
    chance = (my_skill_thn + ((my_to_hit + item->to_h) * 3));
    if ((r_ptr->ac * 3 / 4) > 0)
        chance = (chance * 100) / (r_ptr->ac * 3 / 4);

    /* 5% automatic success/fail */
    if (chance > 95) chance = 95;
    if (chance < 5) chance = 5;

    /* add 20% to chance to give a bit more wieght to weapons */
    if (auto_level > 15) chance += 20;

    dam = (dam * chance) / 100;

    /* Limit damage to twice maximal hitpoints */
    if (dam > kill->power * 2) dam = kill->power * 2;

    /*
     * Enhance the preceived damage on Uniques.  This way we target them
     * Keep in mind that he should hit the uniques but if he has a
     * x5 great bane of dragons, he will tend attack the dragon since the
     * precieved (and actual) damage is higher.  But don't select
     * the town uniques (maggot does no damage)
     *
     */
    if ((r_ptr->flags1 & RF1_UNIQUE) && auto_depth >=1) dam += (dam * 5);

    if ((r_ptr->flags1 & RF1_UNIQUE) && auto_depth ==0)
     {
        dam = dam * 2/3;
        if (auto_level < 5) dam =0;
     }

    /* give a small bonus for whacking a breeder */
    if (r_ptr->flags2 & RF2_MULTIPLY)
        dam = (dam * 3/2);

    /* Enhance the preceived damgage to summoner in order to influence the
     * choice of targets.
     */
    if ( (r_ptr->flags6 & RF6_S_MONSTER) ||
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

    /* try to conserve mana for GOI */
    if (borg_goi)
    {
        dam += (dam*13/10);
    }
    /* Paralyzed Monsters are invulnerable */
    if (kill->invuln) dam = -100;

    /* Damage */
    return (dam);
}



/*
 * Simulate/Apply the optimal result of making a physical attack
 */
static int borg_attack_aux_thrust(void)
{
    int p, dir;

    int i, b_i = -1;
    int d, b_d = -1;

    auto_grid *ag;

    auto_kill *kill;

    /* Too afraid to attack */
    if (do_afraid) return (0);


    /* Examine possible destinations */
    for (i = 0; i < auto_temp_n; i++)
    {
        int x = auto_temp_x[i];
        int y = auto_temp_y[i];

        /* Require "adjacent" */
        if (distance(c_y, c_x, y, x) > 1) continue;

        /* Acquire grid */
        ag = &auto_grids[y][x];

        /* Calculate "average" damage */
        d = borg_thrust_damage_one(ag->kill);

        /* No damage */
        if (d <= 0) continue;

        /* Obtain the monster */
        kill = &auto_kills[ag->kill];

        /* Hack -- avoid waking most "hard" sleeping monsters */
        if (!kill->awake && (d <= kill->power) )
        {
            /* Calculate danger */
            p = borg_danger_aux(y, x, 1, ag->kill);

            if (p > avoidance / 2)
                continue;
        }

        /* Calculate "danger" to player */
        p = borg_danger_aux(c_y, c_x, 2, ag->kill);

        /* Reduce "bonus" of partial kills */
        if (d <= kill->power) p = p / 10;

        /* Add the danger to the damage */
        d += p;

        /* Ignore lower damage */
        if ((b_i >= 0) && (d < b_d)) continue;

        /* Save the info */
        b_i = i;
        b_d = d;
    }

    /* Nothing to attack */
    if (b_i < 0) return (0);

    /* Simulation */
    if (auto_simulate) return (b_d);

    /* Save the location */
    g_x = auto_temp_x[b_i];
    g_y = auto_temp_y[b_i];

    ag = &auto_grids[g_y][g_x];
    kill= &auto_kills[ag->kill];

    /* Note */
    borg_note(format("# Facing %s at (%d,%d) who has %d Hit Points.",(r_name + r_info[kill->r_idx].name), g_y,g_x,kill->power));
    borg_note(format("# Attacking with weapon '%s'",
                     auto_items[INVEN_WIELD].desc));

    /* Get a direction for attacking */
    dir = borg_extract_dir(c_y, c_x, g_y, g_x);

    /* Attack the grid */
    borg_keypress('+');
    borg_keypress(I2D(dir));

    /* Success */
    return (b_d);
}




/*
 * Target a location.  Can be used alone or at "Direction?" prompt.
 *
 * Warning -- This will only work for locations on the current panel
 */
bool borg_target(int y, int x)
{
    int x1, y1, x2, y2;

    auto_grid *ag;
    auto_kill *kill;

    ag = &auto_grids[y][x];
    kill = &auto_kills[ag->kill];


    /* Log */
    /* Report a little bit */
    if (ag->kill)
    {
       borg_note(format("# Targeting %s who has %d Hit Points.",(r_name + r_info[kill->r_idx].name), kill->power));
    }
    else
    {
        borg_note(format("# Targetting location (%d,%d)", y, x));
    }

    /* Target mode */
    borg_keypress('*');

    /* Target a location */
    borg_keypress('p');

    /* Determine "path" */
    x1 = c_x;
    y1 = c_y;
    x2 = x;
    y2 = y;

    /* Move to the location (diagonals) */
    for (; (y1 < y2) && (x1 < x2); y1++, x1++) borg_keypress('3');
    for (; (y1 < y2) && (x1 > x2); y1++, x1--) borg_keypress('1');
    for (; (y1 > y2) && (x1 < x2); y1--, x1++) borg_keypress('9');
    for (; (y1 > y2) && (x1 > x2); y1--, x1--) borg_keypress('7');

    /* Move to the location */
    for (; y1 < y2; y1++) borg_keypress('2');
    for (; y1 > y2; y1--) borg_keypress('8');
    for (; x1 < x2; x1++) borg_keypress('6');
    for (; x1 > x2; x1--) borg_keypress('4');

    /* Select the target */
    borg_keypress('5');

    /* Success */
    return (TRUE);
}



/*
 * Guess how much damage a spell attack will do to a monster
 *
 * We only handle the "standard" damage types.
 *
 * We are paranoid about monster resistances
 *
 * He tends to waste all of his arrows on a monsters immediately adjacent
 * to him.  Then he has no arrows for the rest of the level.  We will
 * decrease the damage if the monster is adjacent and we are getting low
 * on missiles.
 */
int borg_launch_damage_one(int i, int dam, int typ)
{
    int p1, p2 = 0, damex =0;

    auto_kill *kill;

    monster_race *r_ptr;

    /* Monster record */
    kill = &auto_kills[i];

    /* Monster race */
    r_ptr = &r_info[kill->r_idx];


    /* Analyze the damage type */
    switch (typ)
    {
        /* Magic Missile */
        case GF_MISSILE:
        break;

        /* Standard Arrow */
        case GF_ARROW:
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Seeker Arrow */
        case GF_ARROW_SEEKER:
        if (r_ptr->flags1 & RF1_UNIQUE) dam /= 10;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Hurt Animal*/
        case GF_ARROW_ANIMAL:
        if (r_ptr->flags3 & RF3_ANIMAL) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of hurt evil */
        case GF_ARROW_EVIL:
        if (r_ptr->flags3 & RF3_EVIL) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Hurt undead*/
        case GF_ARROW_UNDEAD:
        if (r_ptr->flags3 & RF3_UNDEAD) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of hurt demon */
        case GF_ARROW_DEMON:
        if (r_ptr->flags3 & RF3_DEMON) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Hurt Orc*/
        case GF_ARROW_ORC:
        if (r_ptr->flags3 & RF3_ORC) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of hurt Troll */
        case GF_ARROW_TROLL:
        if (r_ptr->flags3 & RF3_TROLL) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Hurt Giant*/
        case GF_ARROW_GIANT:
        if (r_ptr->flags3 & RF3_GIANT) dam *= 2;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Flame*/
        case GF_ARROW_FLAME:
        if (!(r_ptr->flags3 & RF3_IM_FIRE)) dam *= 3;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Frost*/
        case GF_ARROW_FROST:
        if (!(r_ptr->flags3 & RF3_IM_COLD)) dam *= 3;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;


        /* Arrow of slay dragon*/
        case GF_ARROW_DRAGON:
        if (r_ptr->flags3 & RF3_DRAGON) dam *= 3;
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Arrow of Wounding*/
        case GF_ARROW_WOUNDING:
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        break;

        /* Pure damage */
        case GF_MANA:
        /* only use mana storm against uniques... this */
        /* should cut down on some mana use. */
        if (!borg_fighting_unique)
            dam /= 2;
        break;

        /* Meteor -- powerful magic missile */
        case GF_METEOR:
        break;


        /* Acid */
        case GF_ACID:
        if (r_ptr->flags3 & RF3_IM_ACID) dam /= 9;
        break;

        /* Electricity */
        case GF_ELEC:
        if (r_ptr->flags3 & RF3_IM_ELEC) dam /= 9;
        break;

        /* Fire damage */
        case GF_FIRE:
        if (r_ptr->flags3 & RF3_IM_FIRE) dam /= 9;
        break;

        /* Cold */
        case GF_COLD:
        if (r_ptr->flags3 & RF3_IM_COLD) dam /= 9;
        break;

        /* Poison */
        case GF_POIS:
        if (r_ptr->flags3 & RF3_IM_POIS) dam /= 9;
        break;

        /* Ice */
        case GF_ICE:
        if (r_ptr->flags3 & RF3_IM_COLD) dam /= 9;
        break;


        /* Holy Orb */
        case GF_HOLY_ORB:
        if (r_ptr->flags3 & RF3_EVIL) dam *= 2;
        if (r_ptr->flags1 & RF1_QUESTOR) dam = dam *15/10;
        break;

        /* APW dispel undead */
        case GF_DISP_UNDEAD:
        if (!(r_ptr->flags3 & RF3_UNDEAD)) dam = 0;
        break;

        /*  Dispel Evil */
        case GF_DISP_EVIL:
        if (!(r_ptr->flags3 & RF3_EVIL)) dam = 0;
        break;

        /*  Holy Word */
        case GF_HOLY_WORD:
        if (!(r_ptr->flags3 & RF3_EVIL)) dam = 0;
        break;


        /* Weak Lite */
        case GF_LITE_WEAK:
        if (!(r_ptr->flags3 & RF3_HURT_LITE)) dam = 0;
        break;


        /* Drain Life */
        case GF_OLD_DRAIN:
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam /= 5;
        if ((r_ptr->flags3 & RF3_UNDEAD) ||
            (r_ptr->flags3 & RF3_DEMON) ||
            (strchr("Egv", r_ptr->d_char)))
        {
            dam = 0;
        }
        break;

        /* Stone to Mud */
        case GF_KILL_WALL:
        if (!(r_ptr->flags3 & RF3_HURT_ROCK)) dam = 0;
        break;

        /* New mage spell */
        case GF_NETHER:
		{
			if (r_ptr->flags3 & RF3_UNDEAD)
			{
				dam = 0;
			}
			else if (r_ptr->flags4 & RF4_BR_NETH)
			{
                dam *= 3; dam /= 9;
			}
			else if (r_ptr->flags3 & RF3_EVIL)
			{
				dam /= 2;
			}
        }
			break;

        /* New mage spell */
        case GF_CHAOS:
        if (r_ptr->flags4 & RF4_BR_CHAO)
        {
            dam *=3; dam /= 9;
        }
        break;

        /* New mage spell */
        case GF_GRAVITY:
        if (r_ptr->flags4 & RF4_BR_GRAV)
        {
            dam *= 2; dam /= 9;
        }
        break;

        /* New mage spell */
        case GF_SHARD:
        if (r_ptr->flags4 & RF4_BR_SHAR)
        {
            dam *= 3; dam /= 9;
        }
        break;

        /* New mage spell */
        case GF_SOUND:
        if (r_ptr->flags4 & RF4_BR_SOUN)
        {
                dam *= 2; dam /= 9;
        }
        break;

        /* New mage spell */
        case GF_PLASMA:
        if (r_ptr->flags4 & RF4_BR_PLAS)
        {
                dam *= 2; dam /= 9;
        }
        break;

        /* New mage spell */
        case GF_FORCE:
        if (distance(c_y, c_x,kill->y, kill->x) == 1) dam *= 2;
        if (r_ptr->flags4 & RF4_BR_WALL)
        {
                dam *= 2; dam /= 9;
        }
        break;

        /* Weird attacks */
        case GF_WATER:
        case GF_DISENCHANT:
        case GF_NEXUS:
        case GF_INERTIA:
        case GF_TIME:
        case GF_LITE:
        case GF_DARK:
        break;


        /* Various */
        case GF_OLD_HEAL:
        case GF_OLD_CLONE:
        case GF_OLD_SPEED:
        case GF_DARK_WEAK:
        case GF_KILL_DOOR:
        case GF_KILL_TRAP:
        case GF_MAKE_WALL:
        case GF_MAKE_DOOR:
        case GF_MAKE_TRAP:
        case GF_AWAY_UNDEAD:
        case GF_TURN_EVIL:
        case GF_DISP_ALL:
        dam = 0;
        break;

        /* These spells which put the monster out of commission, we
         * look at the danger of the monster prior to and after being
         * put out of commission.  The difference is the damage.
         * The following factors are considered when we
         * consider the spell:
         *
         * 1. Is it already comprised by that spell?
         * 2. Is it comprimised by another spell?
         * 3. Does it resist the modality?
         * 4. Will it make it's savings throw better than half the time?
         * 5. We generally ignore these spells for breeders.
         *
         * The spell sleep II and sanctuary have a special consideration
         * since the monsters must be adjacent to the player.
         */

        case GF_AWAY_ALL:
        dam = borg_danger_aux(c_y,c_x,2,i);
        /* Try not to teleport away uniques.   These are the guys you are trying */
        /* to kill! */
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            if (dam > avoidance * 6)
            {
                /* get rid of this scary guy */
            }
            else
            {
                dam = -5000;
            }
	   }
        break;

        case GF_OLD_CONF:
        borg_confuse_spell = FALSE;
        p1 = borg_danger_aux(c_y,c_x,2,i);
        borg_confuse_spell = TRUE;
        p2 = borg_danger_aux(c_y,c_x,2,i);
        borg_confuse_spell = FALSE;
        dam= (p1-p2);
        if (kill->confused) dam = 0;
        if (kill->afraid) dam = 0;
        if (kill->speed < r_ptr->speed ) dam = 0;
        if (r_ptr->flags3 & RF3_NO_CONF) dam = -999;
        if (r_ptr->flags2 & RF2_MULTIPLY) dam = 0;
        if (!kill->awake) dam = 0;
        break;

        case GF_CONFUSION:
        borg_confuse_spell = FALSE;
        p1 = borg_danger_aux(c_y,c_x,2,i);
        borg_confuse_spell = TRUE;
        p2 = borg_danger_aux(c_y,c_x,2,i);
        borg_confuse_spell = FALSE;
        damex = (p1-p2);
        if (kill->confused) damex = 0;
        if (kill->afraid) damex = 0;
        if (kill->speed < r_ptr->speed ) damex = 0;
        if (r_ptr->flags3 & RF3_NO_CONF) damex = 0;
        if (r_ptr->flags2 & RF2_MULTIPLY) damex = 0;
        if (!kill->awake) damex= 0;
        dam +=damex;
        break;

        case GF_TURN_ALL:
        borg_fear_mon_spell = FALSE;
        p1 = borg_danger_aux(c_y,c_x,2,i);
        borg_fear_mon_spell = TRUE;
        p2 = borg_danger_aux(c_y,c_x,2,i);
        borg_fear_mon_spell = FALSE;
        dam= (p1-p2);
        if (kill->confused) dam = 0;
        if (kill->afraid) dam = 0;
        if (kill->speed < r_ptr->speed ) dam = 0;
        if (r_ptr->flags3 & RF3_NO_FEAR) dam = -999;
        if (r_ptr->flags2 & RF2_MULTIPLY) dam = 0;
        if (!kill->awake) dam = 0;
        break;

        case GF_OLD_SLOW:
        borg_slow_spell = FALSE;
        p1 = borg_danger_aux(c_y,c_x,2,i);
        borg_slow_spell = TRUE;
        p2 = borg_danger_aux(c_y,c_x,2,i);
        borg_slow_spell = FALSE;
        dam= (p1-p2);
        if (kill->speed < r_ptr->speed ) dam = 0;
        if (kill->afraid) dam = 0;
        if (kill->confused) dam = 0;
        if (!kill->awake) dam = 0;
        break;

        case GF_PARALYSE:
        p1 = borg_danger_aux(c_y,c_x,2,i);
        dam= (p1);
        /* must be sufficiently scary */
        if (dam < auto_chp/3) dam /=2;
        if (kill->speed < r_ptr->speed ) dam = 0;
        if (kill->afraid) dam = 0;
        if (kill->confused) dam = 0;
        if (kill->level > auto_level-5) dam = 0;
        break;

        case GF_OLD_SLEEP:
        borg_sleep_spell = FALSE;
        p1 = borg_danger_aux(c_y,c_x,2,i);
        borg_sleep_spell = TRUE;
        p2 = borg_danger_aux(c_y,c_x,2,i);
        borg_sleep_spell = FALSE;
        dam= (p1-p2);
        if (r_ptr->flags3 & RF3_NO_SLEEP) dam = -999;
        if (kill->speed < r_ptr->speed ) dam = 0;
        if (kill->afraid) dam = 0;
        if (kill->confused) dam = 0;
        if (!kill->awake) dam = 0;
        break;

        case GF_OLD_POLY:
        dam = borg_danger_aux(c_y,c_x,2,i);
        if (r_ptr->flags1 & RF1_UNIQUE) dam = -999;
        /* dont bother unless he is a scary monster */
        if (dam < auto_chp) dam = 0;
        break;

        case GF_TURN_UNDEAD:
        if (r_ptr->flags3 & RF3_UNDEAD)
        {
            borg_fear_mon_spell = FALSE;
            p1 = borg_danger_aux(c_y,c_x,2,i);
            borg_fear_mon_spell = TRUE;
            p2 = borg_danger_aux(c_y,c_x,2,i);
            borg_fear_mon_spell = FALSE;
            dam= (p1-p2);
            if (kill->confused) dam = 0;
            if (kill->afraid) dam = 0;
            if (kill->speed < r_ptr->speed ) dam = 0;
            if (!kill->awake) dam = 0;
        }
        else
        {
            dam = 0;
        }
        break;

        /* Banishment-- cast when in extreme danger (checked in borg_defense). */
        case GF_AWAY_EVIL:
        if (r_ptr->flags3 & RF3_EVIL)
        {
            /* damage is the danger of the baddie */
            dam = borg_danger_aux(c_y,c_x,2,i);
            /* try not teleport away uniques. */
            if (r_ptr->flags1 & RF1_UNIQUE)
                dam = -1000;
        }
        else
        {
            dam = 0;
        }
        break;

    }

    /* Limit damage to twice maximal hitpoints */
    if (dam > kill->power * 2) dam = kill->power * 2;

    /* give a small bonus for whacking a unique */
    /* this should be just enough to give prefrence to wacking uniques */
    if ((r_ptr->flags1 & RF1_UNIQUE) && auto_depth >=1)
        dam = (dam * 5);

    if ((r_ptr->flags1 & RF1_UNIQUE) && auto_depth ==0)
     {
        dam = dam * 2/3;
        if (auto_level < 5) dam =0;
     }

    /* give a small bonus for whacking a breeder */
    if (r_ptr->flags2 & RF2_MULTIPLY)
        dam = (dam * 3/2);

    /* Enhance the preceived damgage to summoner in order to influence the
     * choice of targets.
     */
    if ( (r_ptr->flags6 & RF6_S_MONSTER) ||
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


    /* Paralyzed Monsters are invulnerable */
    if (kill->invuln) dam = 0;

    /* High clevel borgs town scumming should not waste arrows on lower
     * dlevels.  Try to conserve them.
     */
    if (auto_level > 25 && typ < BF_THRUST &&
        ((auto_depth < auto_level / 5) ||         /* shallow dungeon */
         (kill->level < auto_level -15)))           /* weak monster */
    {
        /* set damage to zero, force borg to melee attack */
        dam = 0;
    }

    /* Damage */
    return (dam);
}



/*
 * Simulate / Invoke the launching of a bolt at a monster
 */
static int borg_launch_bolt_aux_hack(int i, int dam, int typ)
{
    int d, p, x, y;
    int o_y = 0;
    int o_x = 0;
    int walls =0;
    int unknown =0;

    auto_grid *ag;

    auto_kill *kill;

    monster_race *r_ptr;

    /* Monster */
    kill = &auto_kills[i];

    /* monster race */
    r_ptr = &r_info[kill->r_idx];


    /* Skip dead monsters */
    if (!kill->r_idx) return (0);

    /* Require current knowledge */
    if (kill->when < c_t) return (0);

    /* Acquire location */
    x = kill->x;
    y = kill->y;

    /* Acquire the grid */
    ag = &auto_grids[y][x];

    /* Never shoot walls/doors */
    if (!borg_cave_floor_grid(ag)) return (0);

    /* Hack -- Unknown grids should be avoided some of the time */
    if ((ag->feat == FEAT_NONE) && ((c_t % 8) == 0)) return (0);

    /* Hack -- Weird grids should be avoided some of the time */
    if ((ag->feat == FEAT_INVIS) && ((c_t % 8) == 0)) return (0);

    /* dont shoot at ghosts in walls, not perfect */
    if (r_ptr->flags2 & RF2_PASS_WALL)
    {
        /* if 2 walls and 1 unknown skip this monster */
        /* Acquire location */
        x = kill->x;
        y = kill->y;

        /* Get grid */
        for (o_x = -1; o_x <= 1; o_x++)
        {
            for (o_y = -1; o_y <= 1; o_y++)
            {
                /* Acquire location */
                x = kill->x + o_x;
                y = kill->y + o_y;

                ag = &auto_grids[y][x];

                if (ag->feat >= FEAT_MAGMA &&
                    ag->feat <= FEAT_PERM_SOLID) walls++;
                if (ag->feat & FEAT_INVIS) unknown++;
            }
        }
        /* Is the ghost likely in a wall? */
        if (walls >=2 && unknown >=1) return (0);
    }

    /* Calculate damage */
    d = borg_launch_damage_one(i, dam, typ);

    /* Calculate danger */
    p = borg_danger_aux(y, x, 1, i);

     /* Hack -- avoid waking most "hard" sleeping monsters */
    if (!kill->awake &&
        (p > avoidance / 2) &&
        (d < kill->power) )
    {
        return (-999);
    }

    /* Hack -- ignore "sleeping" town monsters */
    if (!auto_depth && !kill->awake)
    {
        return (0);
    }

    /* Calculate "danger" to player */
    p = borg_danger_aux(c_y, c_x, 2, i);

    /* Reduce "bonus" of partial kills */
    if (d < kill->power) p = p / 10;

    /* Add in power */
    d += p;

    /* Result */
    return (d);
}


/*
 * Determine the "reward" of launching a beam/bolt/ball at a location
 *
 * An "unreachable" location always has zero reward.
 *
 * Basically, we sum the "rewards" of doing the appropriate amount of
 * damage to each of the "affected" monsters.
 *
 * We will attempt to apply the offset attack here
 */
static int borg_launch_bolt_aux(int y, int x, int rad, int dam, int typ, int max)
{
    int i;

    int x1, y1;
    int x2, y2;

    int dist;

    int r, n;

    auto_grid *ag;
    monster_race *r_ptr;
    auto_kill *kill;

    int q_x, q_y;

    /* Extract panel */
    q_x = w_x / 33;
    q_y = w_y / 11;

    /* Reset damage */
    n = 0;

    /* Initial location */
    x1 = c_x; y1 = c_y;

    /* Final location */
    x2 = x; y2 = y;

    /* Start over */
    x = x1; y = y1;

    /* Simulate the spell/missile path */
    for (dist = 0; dist < max; dist++)
    {
        /* Get the grid */
        ag = &auto_grids[y2][x2];
        kill = &auto_kills[ag->kill];
        r_ptr = &r_info[kill->r_idx];

        ag = &auto_grids[y][x];

        /* Stop at walls */
        /* note: beams end at walls.  */
        if (dist)
        {
            /* Stop at walls */
            /* note if beam, this is the end of the beam */
            /* dispel spells act like beams (sort of) APW*/
            if (!borg_cave_floor_grid(ag))
            {
                if (rad != -1 && rad !=15)
                    return (0);
                else
                    return (n);
            }
        }

        /* Collect damage (bolts/beams/dispel) */
        if (rad <= 0 || rad != 15) n += borg_launch_bolt_aux_hack(ag->kill, dam, typ);

        /* Check for arrival at "final target" */
        /* except beams, which keep going. */
        if ( (rad != -1 && rad !=15)  && ((x == x2) && (y == y2))) break;

        /* Stop bolts at monsters  */
        if (!rad && ag->kill) return (0);

        /* Check for shooting into situations. Dont do the check if esp */
        if (!my_telepathy)
        {
            /* Check the missile path--not if I have Infra */
            if (dist && (my_see_infra <= 0) && !(r_ptr->flags2 & RF2_HAS_LITE))
            {
                /* Stop at unknown grids (see above) */
                /* note if beam, dispel, this is the end of the beam */
                if (ag->feat == FEAT_NONE)
                {
                    if (rad != -1 && rad !=15)
                        return (0);
                    else
                        return (n);
                }

                /* Stop at weird grids (see above) */
                /* note if beam, this is the end of the beam */
                if (ag->feat == FEAT_INVIS)
                {
                    if (rad != -1 && rad !=15)
                        return (0);
                    else
                        return (n);
                }

                /* Stop at unseen walls */
                /* We just shot and missed, this is our next shot */
                if (successful_target == -1)
                {
                    if (rad != -1 && rad !=15)
                        return (0);
                    else
                        return (n);
                }
            }
            else  /* I do have infravision */
            {
                /* Stop at unseen walls */
                /* We just shot and missed, this is our next shot */
                if (successful_target == -1)
                {
                    if (rad != -1 && rad !=15)
                        return (0);
                    else
                        return (n);
                }
            }
         }
        else /* I do have ESP */
         {
            /* Check the missile path */
            if (dist )
            {
                /* if this area has been magic mapped,
                * ok to shoot in the dark
                */
                if (!auto_detect_wall[q_y+0][q_x+0] &&
                    !auto_detect_wall[q_y+0][q_x+1] &&
                    !auto_detect_wall[q_y+1][q_x+0] &&
                    !auto_detect_wall[q_y+1][q_x+1])
                {

                    /* Stop at unknown grids (see above) */
                    /* note if beam, dispel, this is the end of the beam */
                    if (ag->feat == FEAT_NONE)
                    {
                        if (rad != -1 && rad !=15)
                            return (0);
                        else
                            return (n);
                    }

                    /* Stop at unseen walls */
                    /* We just shot and missed, this is our next shot */
                    if (successful_target == -1)
                    {
                        if (rad != -1 && rad !=15)
                            return (0);
                        else
                           return (n);
                    }
                }

                /* Stop at weird grids (see above) */
                /* note if beam, this is the end of the beam */
                if (ag->feat == FEAT_INVIS)
                {
                    if (rad != -1 && rad !=15)
                        return (0);
                    else
                        return (n);
                }

                /* Stop at unseen walls */
                /* We just shot and missed, this is our next shot */
                if (successful_target == -1)
                {
                    if (rad != -1 && rad !=15)
                        return (0);
                    else
                        return (n);
                }
            }
        }

        /* Calculate the new location */
        mmove2(&y, &x, y1, x1, y2, x2);
    }

    /* Bolt/Beam attack */
    if (rad <= 0) return (n);

    /* Excessive distance */
    if (dist >= MAX_RANGE) return (0);

    /* Check monsters in blast radius */
    for (i = 0; i < auto_temp_n; i++)
    {
        /* Acquire location */
        x = auto_temp_x[i];
        y = auto_temp_y[i];

        /* Get the grid */
        ag = &auto_grids[y][x];

        /* Check distance */
        r = distance(y2, x2, y, x);

        /* Maximal distance */
        if (r > rad) continue;

        /* Never pass through walls */
        if (!borg_projectable(y2, x2, y, x)) continue;

        /* dispel spells should hurt the same no matter the rad: make r= y  and x */
        if (rad == 15) r = 0;

        /* Collect damage, lowered by distance */
        n += borg_launch_bolt_aux_hack(ag->kill, dam / (r + 1), typ);

        /* check destroyed stuff. */
        if (ag->take)
        {
            auto_take *take = &auto_takes[ag->take];
            object_kind *k_ptr = &k_info[take->k_idx];

            switch (typ)
            {
                case GF_ACID:
                {
                    /* rings/boots cost extra (might be speed!) */
                    if (k_ptr->tval == TV_BOOTS)
                    {
                        n -= 2000;
                    }
                    break;
                }
                case GF_ELEC:
                {
                    /* rings/boots cost extra (might be speed!) */
                    if (k_ptr->tval == TV_RING)
                    {
                        n -= 2000;
                    }
                    break;
                }

                case GF_FIRE:
                {
                    /* rings/boots cost extra (might be speed!) */
                    if (k_ptr->tval == TV_BOOTS)
                    {
                        n -= 2000;
                    }
                    break;
                }
                case GF_COLD:
                {
                    if (k_ptr->tval == TV_POTION)
                    {
                        n -= 2000;
                    }
                    break;
                }
                case GF_MANA:
                {
                    /* rings/boots cost extra (might be speed!) */
                    if (k_ptr->tval == TV_RING ||
                        k_ptr->tval == TV_BOOTS ||
                        k_ptr->tval == TV_POTION)
                    {
                        n -= 2000;
                    }
                    break;
                }
            }
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
 *  dispel spells have a rad  of 15
 */
static int borg_launch_bolt(int rad, int dam, int typ, int max)
{
    int num = 0;

    int i, b_i = -1;
    int n, b_n = 0;
    int b_o_x = 0, b_o_y = 0;
    int o_y = 0, o_x = 0;

    /* If its a Dispel, then the epicenter is the player */
    if (rad >=6)
    {
        b_n = borg_launch_bolt_aux(c_y, c_x, rad, dam, typ, max);

        /* Simulation */
        if (auto_simulate) return (b_n);

        /* Result */
        return (b_n);
    }

    /* This will allow the borg to target places adjacent to a monster
     * in order to exploit and abuse a feature of the game.  Whereas,
     * the borg, while targeting a monster will not score d/t walls, he
     * could land a success hit by targeting adjacent to the monster.
     * For example:
     * ######################
     * #####...@.......######
     * ############Px........
     * ######################
     * In order to hit the P, the borg must target the x and not the P.
     *
     * This is a massive exploitation.  But it ranks with Farming, Scumming,
     * Savefile abuse.
     */

    /* Examine possible (offset) destinations */
    for (i = 0; i < auto_temp_n; i++)
    {
        int x = auto_temp_x[i];
        int y = auto_temp_y[i];

        /* Consider each spot, adjacent and on top of the monster */
        for (o_x = -1; o_x <= 1; o_x++)
        {
            for (o_y = -1; o_y <= 1; o_y++)
            {
                /* Acquire location */
                x = auto_temp_x[i] + o_x;
                y = auto_temp_y[i] + o_y;

                /* Consider it */
                n = borg_launch_bolt_aux(y, x, rad, dam, typ, max);

                /* Skip useless attacks */
                if (n <= 0) continue;

                /* Hack -- game forbids targetting of outside walls */
                if (x ==0 || y ==0 || x == DUNGEON_WID -1 || y == DUNGEON_HGT-1)
                    continue;

                /* Collect best attack */
                if ((b_i >= 0) && (n < b_n)) continue;

                /* Hack -- reset chooser */
                if ((b_i >= 0) && (n > b_n)) num = 0;

                /* Apply the randomizer */
                if ((num > 1) && (rand_int(num) != 0)) continue;

                /* Track it */
                b_i = i;
                b_n = n;
                b_o_y = o_y;
                b_o_x = o_x;
            }
        }
    }

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Save the location */
    g_x = auto_temp_x[b_i] + b_o_x;
    g_y = auto_temp_y[b_i] + b_o_y;

    /* Target the location */
    (void)borg_target(g_y, g_x);

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
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;


    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {

        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip Ego branded items--they are looked at later */
        if (item->name2) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d <= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing standard missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of launching a seeker missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_seeker(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;


    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {

        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip non seekers--they are looked at later */
        if (item->sval != SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d <= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_SEEKER, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing seeker missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_flame(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_FLAME) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d <= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_FLAME, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing flame branded missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_frost(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_FROST) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;

        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_FROST, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing frost branded missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_animal(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_ANIMAL) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_ANIMAL, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing animal missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_undead(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_UNDEAD) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_UNDEAD, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing undead missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_demon(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_DEMON) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_DEMON, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing demon missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_orc(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_ORC) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_ORC, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing orc missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_troll(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_TROLL) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_TROLL, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing troll missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_giant(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_GIANT) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_GIANT, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing giant missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_evil(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_EVIL) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_EVIL, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing evil branded missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_dragon(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_HURT_DRAGON) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_DRAGON, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing dragon branded missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of launching a branded missile
 *
 * First, pick the "optimal" ammo, then pick the optimal target
 */
static int borg_attack_aux_launch_wounding(void)
{
    int b_n = 0;

    int k , b_k = -1;
    int d , b_d = -1;

    auto_item *bow = &auto_items[INVEN_BOW];

    /* Scan the pack */
    for (k = 0; k < INVEN_PACK; k++)
    {
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        if (item->name2 != EGO_WOUNDING) continue;
        if (item->sval == SV_AMMO_HEAVY) continue;

        /* Skip bad missiles */
        if (item->tval != my_ammo_tval) continue;

        /* Skip worthless missiles */
        if (item->value <= 0) continue;

        /* Skip un-identified, non-average, missiles */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Determine average damage */
        d = (item->dd * (item->ds + 1) / 2);
        d = d + item->to_d + bow->to_d;
        d = d * my_ammo_power * my_num_fire;


        /* Paranoia */
        if (d <= 0) continue;

        if ((b_k >=0) && (d<= b_d)) continue;

        b_k = k;
        b_d = d;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Choose optimal type of bolt */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW_WOUNDING, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Firing wounding branded missile '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('f');

    /* Use the missile */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

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
        auto_item *item = &auto_items[k];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip un-identified, non-average, objects */
        if (!item->able && !streq(item->note, "{average}")) continue;

        /* Skip "equipment" items (not ammo) */
        if (borg_wield_slot(item) >= 0) continue;

        /* Determine average damage from object */
        d = (k_info[item->kind].dd * (k_info[item->kind].ds + 1) / 2);

        /* Skip useless stuff */
        if (d <= 0) continue;

        /* Skip "expensive" stuff */
        if (d < item->value) continue;

	    /* Hack -- Save last five flasks for fuel, if needed */
        if (item->tval == TV_FLASK &&
            (amt_fuel <= 5 && !borg_fighting_unique)) continue;

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
        b_r = (adj_str_blow[my_stat_ind[A_STR]] + 20) * mul / div;

        /* Max distance of 10 */
        if (b_r > 10) b_r = 10;
    }

    /* Nothing to use */
    if (b_k < 0) return (0);


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Choose optimal location */
    b_n = borg_launch_bolt(0, b_d, GF_ARROW, b_r);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Do it */
    borg_note(format("# Throwing painful object '%s'",
                     auto_items[b_k].desc));

    /* Fire */
    borg_keypress('v');

    /* Use the object */
    borg_keypress(I2A(b_k));

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

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
    int penalty = 0;

    auto_magic *as = &auto_magics[book][what];


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* make sure I am powerfull enough to do another goi if this one falls */
    if (borg_goi && ((auto_csp - as->power) < 70)) return (0);

    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);

    /* Require ability (right now) */
    if (!borg_spell_okay_fail(book, what, 25)) return (0);


    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* weak mages need MM spell, they dont get penalized, plus a bonus */
    if (auto_level <= 25 && book == 0 && what == 0)
    {
		/* slight bonus */
		if (auto_level < 10 && b_n >= 1) b_n= b_n + 10;

        /* Simulation */
        if (auto_simulate) return (b_n);
    }

    /* Penalize mana usage */
    b_n = b_n - as->power;

    /* Penalize use of reserve mana */
    if (auto_csp - as->power < auto_msp / 2) b_n = b_n - (as->power * 20);

    /* Penalize use of deep reserve mana */
    if (auto_csp - as->power < auto_msp / 3) b_n = b_n - (as->power * 30);

    /* Really penalize use of mana needed for final teleport */
    if (auto_class == CLASS_MAGE) penalty =8;
    if (auto_class == CLASS_RANGER) penalty = 22;
    if (auto_class == CLASS_ROGUE) penalty = 20;
    if ((auto_msp > 30) && (auto_csp - as->power) < penalty)
            b_n = b_n - (as->power * 750);

    /* Simulation */
    if (auto_simulate) return (b_n);


    /* Cast the spell */
    (void)borg_spell(book, what);

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/* This routine is the same as the one above only in an emergency case.
 * The borg will enter negative mana casting this
 */
static int borg_attack_aux_spell_bolt_reserve(int book, int what, int rad, int dam, int typ)
{
    int b_n;
    int penalty =0;

    auto_magic *as = &auto_magics[book][what];

    /* Fake our Mana */
    int sv_mana = auto_csp;


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Must be dangerous */
    if (borg_danger(c_y, c_x,1) < avoidance * 2) return (0);

    /* Require ability (with faked mana) */
    auto_csp = auto_msp;
    if (!borg_spell_okay_fail(book, what, 25))
    {
		auto_csp=sv_mana;
		return (0);
	}

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

/* We should check to make sure the borg is not surrounded by guys,
 * but only one guy and that it is likely to die from this spell.
 * Perhaps check his hit points.
 */

    /* Only Weak guys should try this */
    if (auto_level >=15) b_n = 0;

    /* return the value */
    if (auto_simulate)
    {
		auto_csp = sv_mana;
		return (b_n);
	}

    /* Cast the spell */
    if (borg_spell_fail(book, what, 25))
    {
        /* Note the use of the emergency spell */
        borg_note("# Emergency use of an Attack Spell.");

        /* verify use of spell */
        borg_keypress('y');
    }

    /* Use target */
    borg_keypress('5');

    /* Set our shooting flag */
    successful_target = -1;

    /* restore true mana */
    auto_csp = 0;

    /* Value */
    return (b_n);
}


/* This routine is the same as the one above only in an emergency case.
 * The borg will enter negative mana casting this
 */
static int borg_attack_aux_prayer_bolt_reserve(int book, int what, int rad, int dam, int typ)
{
    int b_n;
    int penalty =0;

    auto_magic *as = &auto_magics[book][what];

    /* Fake our Mana */
    int sv_mana = auto_csp;


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);

    /* Must be dangerous */
    if (borg_danger(c_y, c_x,1) < avoidance * 2) return (0);

    /* Require ability (with faked mana) */
    auto_csp = auto_msp;
    if (!borg_prayer_okay_fail(book, what, 25))
    {
		auto_csp = sv_mana;
		return (0);
	}

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

/* We should check to make sure the borg is not surrounded by guys,
 * but only one guy and that it is likely to die from this spell.
 * Perhaps check his hit points.
 */

    /* Only Weak guys should try this */
    if (auto_level >=15) b_n = 0;

    /* return the value */
    if (auto_simulate) return (b_n);

    /* Cast the spell */
    if (borg_prayer_fail(book, what, 25))
    {
        /* Note the use of the emergency spell */
        borg_note("# Emergency use of an Attack Prayer.");

        /* verify use of spell */
        borg_keypress('y');
    }

    /* Use target */
    borg_keypress('5');

    /* Set our shooting flag */
    successful_target = -1;

    /* restore true mana */
    auto_csp = 0;

    /* Value */
    return (b_n);
}


/*
 * Simulate/Apply the optimal result of using a "normal" attack prayer
 */
static int borg_attack_aux_prayer_bolt(int book, int what, int rad, int dam, int typ)
{
    int b_n;
    int penalty=0;

    auto_magic *as = &auto_magics[book][what];


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Require ability */
    if (!borg_prayer_okay_fail(book, what, 25)) return (0);


    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* Penalize mana usage */
    b_n = b_n - as->power;

    /* Penalize use of reserve mana */
    if (auto_csp - as->power < auto_msp / 2) b_n = b_n - (as->power * 20);

    /* Penalize use of deep reserve mana */
    if (auto_csp - as->power < auto_msp / 3) b_n = b_n - (as->power * 30);

    /* Really penalize use of mana needed for final teleport */
        if (auto_class == CLASS_PRIEST) penalty =8;
        if (auto_class == CLASS_PALADIN) penalty = 20;
        if ((auto_msp > 30) && (auto_csp - as->power) < penalty)
                b_n = b_n - (as->power * 750);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Cast the prayer */
    (void)borg_prayer(book, what);

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of using a "racial" attack
 *
 */
static int borg_attack_aux_racial_bolt(char num, int rad, int dam, int typ)
{
    int b_n;
    int cost=0;

    /* No firing while blind, confused, or hallucinating
     * and not when poisoned, must conserve hp */
    if (do_blind || do_confused || do_image || do_poisoned) return (0);

    /* Obtain cost of each bolt */
    if (auto_race == RACE_PIXIE) cost = 5;
    if (auto_race >= RACE_MIN_DRAGON && rad < 1) cost = (2*auto_level)/3;
    if (auto_race >= RACE_MIN_DRAGON && rad >= 1) cost = (3*auto_level)/2;

    /* make sure I am powerfull enough to do it */
    if ((auto_chp - cost) <= (auto_mhp /10)) return (0);

    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* Penalize HP usage */
    b_n = b_n - (cost*2);

    /* Penalize use of HP when fighting a unique */
    if (borg_fighting_unique) b_n = b_n - (cost * 2);

    /* Penalize use of reserve HP */
    if (auto_chp - cost < auto_mhp / 2) b_n = b_n - (cost * 5);

    /* Penalize use of deep reserve mana */
    if (auto_chp - cost < auto_mhp / 3) b_n = b_n - (cost * 10);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Wonder Twin Powers Activate! */
    borg_keypress('U');
    borg_keypress(num);

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}
/*
 * Simulate/Apply the optimal result of using a "racial" attack
 * Right now only Psuedo Dragons use this one.  (lite spell).
 */
static int borg_attack_aux_racial_dispel(char num, int rad, int dam, int typ)
{
    int b_n;
    int cost=0;

    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image || do_poisoned) return (0);

    /* Obtain cost of each bolt */
    if (auto_race >= RACE_MIN_DRAGON) cost = (3*auto_level)/2;

    /* make sure I am powerfull enough to do it */
    if ((auto_chp - cost) <= (auto_mhp /10)) return (0);

    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);

    /* Require ability (right now) */

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, rad);

    /* Penalize HP usage */
    b_n = b_n - (cost*2);

    /* Penalize use of HP when fighting a unique */
    if (borg_fighting_unique) b_n = b_n - (cost * 2);

    /* Penalize use of reserve HP */
    if (auto_chp - cost < auto_mhp / 3) b_n = b_n - (cost * 5);

    /* Penalize use of deep reserve mana */
    if (auto_chp - cost < auto_mhp / 5) b_n = b_n - (cost * 10);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Wonder Twin Powers Activate! */
    borg_keypress('U');
    borg_keypress('b');

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * APW Simulate/Apply the optimal result of using a "dispel" attack prayer
 */
static int borg_attack_aux_prayer_dispel(int book, int what, int dam, int typ)
{
    int b_n;

    auto_magic *as = &auto_magics[book][what];


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Require ability */
    if (!borg_prayer_okay_fail(book, what, 25)) return (0);

    /* Choose optimal location--radius defined as 10 */
    b_n = borg_launch_bolt(15, dam, typ, MAX_RANGE);

    /* Penalize mana usage */
    b_n = b_n - as->power;

    /* Penalize use of reserve mana */
    if (auto_csp - as->power < auto_msp / 2) b_n = b_n - (as->power * 20);

    /* Penalize use of deep reserve mana */
    if (auto_csp - as->power < auto_msp / 3) b_n = b_n - (as->power * 30);

    /* Really penalize use of mana needed for final teleport */
    if ((auto_msp > 30) && (auto_csp - as->power) < 6)
        b_n = b_n - (as->power * 750);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Cast the prayer */
    (void)borg_prayer(book, what);


    /* Value */
    return (b_n);
}




/*
 * APW Simulate/Apply the optimal result of using a "dispel" attack spell
 */
static int borg_attack_aux_spell_dispel(int book, int what, int dam, int typ)
{
    int b_n;

    auto_magic *as = &auto_magics[book][what];


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Require ability */
    if (!borg_spell_okay_fail(book, what, 25)) return (0);

    /* Choose optimal location--radius defined as 10 */
    b_n = borg_launch_bolt(15, dam, typ, MAX_RANGE);

    /* Penalize mana usage */
    b_n = b_n - as->power;

    /* Penalize use of reserve mana */
    if (auto_csp - as->power < auto_msp / 2) b_n = b_n - (as->power * 20);

    /* Penalize use of deep reserve mana */
    if (auto_csp - as->power < auto_msp / 3) b_n = b_n - (as->power * 30);

    /* Really penalize use of mana needed for final teleport */
    /* (6 pts for mage) */
    if ((auto_msp > 30) && (auto_csp - as->power) < 6)
        b_n = b_n - (as->power * 750);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Cast the prayer */
    (void)borg_spell(book, what);


    /* Value */
    return (b_n);
}

/*
 * APW Simulate/Apply the optimal result of using a "dispel" staff
 * Which would be dispel evil, power, holiness.  Genocide-type handeled later.
 */
static int borg_attack_aux_staff_dispel(int sval, int rad, int dam, int typ)
{
    int i, b_n;

    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);

    /* look for the staff */
    i =  borg_slot(TV_STAFF, sval);

    /* Require ability */
    if (i < 0) return (0);

    /* No charges */
    if (!auto_items[i].pval) return (0);

    /* Choose optimal location--radius defined as 10 */
    b_n = borg_launch_bolt(15, dam, typ, MAX_RANGE);

    /* Big Penalize charge usage */
    b_n = b_n - 50;

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Cast the prayer */
    (void)borg_use_staff(sval);


    /* Value */
    return (b_n);
}



/*
 * Simulate/Apply the optimal result of using a "normal" attack rod
 */
static int borg_attack_aux_rod_bolt(int sval, int rad, int dam, int typ)
{
    int b_n;


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Look for that rod */
    if (!borg_equips_rod(sval)) return (0);

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Zap the rod */
    (void)borg_zap_rod(sval);

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

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
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Look for that wand */
    i = borg_slot(TV_WAND, sval);

    /* None available */
    if (i < 0) return (0);

    /* No charges */
    if (!auto_items[i].pval) return (0);


    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* Penalize charge usage */
    b_n = b_n - 5;

    /* Wands of wonder are used in last ditch efforts.  They behave
     * randomly, so the best use of them is an emergency.  I have seen
     * borgs die from hill orcs with fully charged wonder wands.  Odds
     * are he could have taken the orcs with the wand.  So use them in
     * an emergency after all the borg_caution() steps have failed
     */
    if (sval == SV_WAND_WONDER)
    {
        /* check the danger */
        if (b_n > 0 && borg_danger(c_y,c_x,1) >= (avoidance * 2) )
        {
            /* make the wand appear deadly */
            b_n = 999;

            /* note the use of the wand in the emergency */
            borg_note(format("# Emergency use of a Wand of Wonder."));
        }
        else
        {
            b_n = 0;
        }
    }

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Aim the wand */
    (void)borg_aim_wand(sval);

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of ACTIVATING an attack artifact
 *
 */
static int borg_attack_aux_artifact(int art_name, int art_loc, int rad, int dam, int typ)
{
    int b_n;

    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Look for that artifact and to see if it is charged */
    if (!borg_equips_artifact(art_name,art_loc)) return (0);

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Activate the artifact */
    (void)borg_activate_artifact(art_name, art_loc);

    /* Use target */
    if (art_name !=ART_INGWE || art_name !=ART_RAZORBACK)
    {
        borg_keypress('5');
    /* Set our targetting flag */
    successful_target = -1;
    }

    /* Value */
    return (b_n);
}

/*
 * Simulate/Apply the optimal result of ACTIVATING a DRAGON ARMOUR
 *
 */
static int borg_attack_aux_dragon(int sval, int rad, int dam, int typ)
{
    int b_n;


    /* No firing while blind, confused, or hallucinating */
    if (do_blind || do_confused || do_image) return (0);


    /* Paranoia */
    if (auto_simulate && (rand_int(100) < 5)) return (0);


    /* Look for that scale mail and charged*/
    if (!borg_equips_dragon(sval)) return (0);

    /* Choose optimal location */
    b_n = borg_launch_bolt(rad, dam, typ, MAX_RANGE);

    /* Simulation */
    if (auto_simulate) return (b_n);

    /* Activate the scale mail */
     (void)borg_activate_dragon(sval);

    /* Use target */
    borg_keypress('5');

    /* Set our targetting flag */
    successful_target = -1;

    /* Value */
    return (b_n);
}

static int borg_attack_aux_artifact_holcolleth(void)
{
    int p1= 0;
    int p2 = 0;
    int d = 0;

    /* Obtain initial danger */
    borg_sleep_spell = FALSE;
    p1= borg_danger(c_y,c_x,4);

    if (!borg_equips_artifact(ART_HOLCOLLETH, INVEN_OUTER))
        return (0);

    /* What effect is there? */
    borg_sleep_spell_ii = TRUE;
    p2=borg_danger(c_y,c_x,4);
    borg_sleep_spell_ii = FALSE;

    /* value is d, enhance the value for rogues and rangers so that
     * they can use their critical hits.
     */
    d = (p1-p2);

    /* Simulation */
    if (auto_simulate) return (d);

    /* Cast the spell */
    if (borg_activate_artifact(ART_HOLCOLLETH, INVEN_OUTER))
    /* Value */
    {
        return (d);
    }
    else
    return (0);
}


/*
 * Simulate/Apply the optimal result of using the given "type" of attack
 */
static int borg_attack_aux(int what)
{
    int dam = 0, chance, rad = 0;

    /* Analyze */
    switch (what)
    {
        /* Physical attack */
        case BF_THRUST:
        return (borg_attack_aux_thrust());

        /* Missile attack */
        case BF_LAUNCH_NORMAL:
        return (borg_attack_aux_launch());

        /* Missile attack */
        case BF_LAUNCH_SEEKER:
        return (borg_attack_aux_launch_seeker());

        /* Missile attack */
        case BF_LAUNCH_FLAME:
        return (borg_attack_aux_launch_flame());

        /* Missile attack */
        case BF_LAUNCH_FROST:
        return (borg_attack_aux_launch_frost());

        /* Missile attack */
        case BF_LAUNCH_ANIMAL:
        return (borg_attack_aux_launch_animal());

        /* Missile attack */
        case BF_LAUNCH_UNDEAD:
        return (borg_attack_aux_launch_undead());

        /* Missile attack */
        case BF_LAUNCH_DEMON:
        return (borg_attack_aux_launch_demon());

        /* Missile attack */
        case BF_LAUNCH_ORC:
        return (borg_attack_aux_launch_orc());

        /* Missile attack */
        case BF_LAUNCH_GIANT:
        return (borg_attack_aux_launch_giant());

        /* Missile attack */
        case BF_LAUNCH_TROLL:
        return (borg_attack_aux_launch_troll());

        /* Missile attack */
        case BF_LAUNCH_EVIL:
        return (borg_attack_aux_launch_evil());

        /* Missile attack */
        case BF_LAUNCH_DRAGON:
        return (borg_attack_aux_launch_dragon());

        /* Missile attack */
        case BF_LAUNCH_WOUNDING:
        return (borg_attack_aux_launch_wounding());

        /* Object attack */
        case BF_OBJECT:
        return (borg_attack_aux_object());



        /* Spell -- slow monster */
        case BF_SPELL_SLOW_MONSTER:
        dam = 10;
        return (borg_attack_aux_spell_bolt(7,1, rad, dam, GF_OLD_SLOW));

        /* Spell -- confuse monster */
        case BF_SPELL_CONFUSE_MONSTER:
        dam = 10;
        return (borg_attack_aux_spell_bolt(1,7, rad, dam, GF_OLD_CONF));

        /* Spell -- sleep I */
        case BF_SPELL_SLEEP_I:
        dam = 10;
        return (borg_attack_aux_spell_bolt(1, 0, rad, dam, GF_OLD_SLEEP));

        /* Spell -- Polymorph Monster */
        case BF_SPELL_POLYMORPH:
        dam = 10;
        return (borg_attack_aux_spell_bolt(7, 4, rad, dam, GF_OLD_POLY));

        /* Spell -- magic missile */
        case BF_SPELL_MAGIC_MISSILE:
        dam = (3+((auto_level)/4))*(4+1)/2;
        return (borg_attack_aux_spell_bolt(0, 0, rad, dam, GF_MISSILE));

        /* Spell -- magic missile */
        case BF_SPELL_MAGIC_MISSILE_RESERVE:
        dam = (3+((auto_level)/4))*(4+1)/2;
        return (borg_attack_aux_spell_bolt_reserve(0, 0, rad, dam, GF_MISSILE));

        /* Spell -- electric bolt */
        case BF_SPELL_ELEC_BOLT:
        dam = (3+((auto_level-5)/4))*(8+1)/2;
        return (borg_attack_aux_spell_bolt(1, 2, rad, dam, GF_ELEC));

        /* Spell -- cold bolt */
        case BF_SPELL_COLD_BOLT:
        dam = (20+auto_level);
        return (borg_attack_aux_spell_bolt(1, 6, rad, dam, GF_COLD));

        /* Spell -- fire bolt */
        case BF_SPELL_FIRE_BOLT:
        dam = (8+(auto_level/4))*(8+1)/2;
        return (borg_attack_aux_spell_bolt(2, 4, rad, dam, GF_FIRE));

        /* Spell -- acid bolt */
        case BF_SPELL_ACID_BOLT:
        dam = (8+(auto_level/4))*(8+1)/2;
        return (borg_attack_aux_spell_bolt(8, 0, rad, dam, GF_ACID));

        /* Spell -- kill wall */
        case BF_SPELL_STONE_TO_MUD:
        dam = (20+(30/2));
        return (borg_attack_aux_spell_bolt(7, 3, rad, dam, GF_KILL_WALL));

        /* Spell -- light beam */
        case BF_SPELL_LITE_BEAM:
        rad = -1;
        dam = (6*(8+1)/2);
        return (borg_attack_aux_spell_bolt(1, 5, rad, dam, GF_LITE_WEAK));

        /* Spell -- Striking */
        case BF_SPELL_STRIKING:
        dam = ((3+(auto_level / 4)) *7)/2;
        return (borg_attack_aux_spell_bolt(2, 0, rad, dam, GF_FORCE));

        /* Spell -- Mana Bolt */
        case BF_SPELL_MANA_BOLT:
        dam = ((auto_level / 2) *10)/2;
        return (borg_attack_aux_spell_bolt(6, 4, rad, dam, GF_MANA));

        /* Spell -- Hold Foe */
        case BF_SPELL_HOLD_FOE:
        dam = 10;
        return (borg_attack_aux_spell_bolt(3, 2, rad, dam, GF_PARALYSE));

        /* Spell -- stinking cloud */
        case BF_SPELL_POISON_BALL:
        rad = 2;
        dam = (10 + (auto_level/2));
        return (borg_attack_aux_spell_bolt(0, 7, rad, dam, GF_POIS));

        /* Spell -- cold ball */
        case BF_SPELL_COLD_BALL:
        rad = 2;
        dam = (30 + (2*auto_level));
        return (borg_attack_aux_spell_bolt(3, 1, rad, dam, GF_COLD));

        /* Spell -- acid ball */
        case BF_SPELL_ACID_BALL:
        rad = 2;
        dam = (4*auto_level);
        return (borg_attack_aux_spell_bolt(8, 1, rad, dam, GF_ACID));

        /* Spell -- fire ball */
        case BF_SPELL_FIRE_BALL:
        rad = 2;
        dam = (75 + 2*auto_level);
        return (borg_attack_aux_spell_bolt(3, 5, rad, dam, GF_FIRE));


        /* Spell -- cold storm */
        case BF_SPELL_COLD_STORM:
        rad = 3;
        dam = (5* auto_level);
        return (borg_attack_aux_spell_bolt(8, 2, rad, dam, GF_COLD));

        /* Spell -- meteor storm */
        case BF_SPELL_METEOR_STORM:
        rad = 3;
        dam = (7 * auto_level);
        return (borg_attack_aux_spell_bolt(8, 3, rad, dam, GF_METEOR));

        /* Spell -- mana storm */
        case BF_SPELL_MANA_STORM:
        rad = 3;
        dam = (300 + (auto_level * 4));
        return (borg_attack_aux_spell_bolt(8, 5, rad, dam, GF_MANA));

        /* Spell -- Confusion */
        case BF_SPELL_CONFUSION:
        dam = ((auto_level * 3)/2);
        return (borg_attack_aux_spell_dispel(5,2, dam, GF_CONFUSION));

        /* Spell -- Plasma Storm */
        case BF_SPELL_PLASMA_STORM:
        rad = 3;
        dam = (auto_level * 3);
        return (borg_attack_aux_spell_bolt(3,7, rad, dam, GF_PLASMA));



        /* Prayer -- blind creature */
        case BF_PRAYER_SPIRIT_HAMMER:
        dam = ((3*3)+1)/2;
        return (borg_attack_aux_prayer_bolt(0,5, rad, dam, GF_HOLY_ORB));

        /* Prayer -- blind creature */
        case BF_PRAYER_SPIRIT_HAMMER_RESERVE:
        dam = ((3*3)+1)/2;
        return (borg_attack_aux_prayer_bolt_reserve(0,5, rad, dam, GF_HOLY_ORB));


        /* Prayer -- scare creature */
        case BF_PRAYER_SCARE_CREATURE:
        dam = ((3*3)+1)/2;
        return (borg_attack_aux_prayer_bolt(1,0, rad, dam, GF_TURN_ALL));

        /* Prayer -- orb of draining */
        case BF_PRAYER_HOLY_ORB_BALL:
        rad = ((auto_level >= 30) ? 3 : 2);
        dam = ((auto_class == CLASS_PRIEST) ? 2 : 4);
        dam = (3*(8+1)/2 + auto_level + (auto_level/dam));
        return (borg_attack_aux_prayer_bolt(2, 1, rad, dam, GF_ELEC));

        /* Prayer -- blind creature */
        case BF_PRAYER_BLIND_CREATURE:
        dam = 10;
        return (borg_attack_aux_prayer_bolt(3,3, rad, dam, GF_OLD_CONF));

        /* Prayer -- and sanctuary LOS*/
        case BF_PRAYER_SANCTUARY:
        dam = 10;
        return (borg_attack_aux_prayer_dispel(1, 4, dam, GF_OLD_SLEEP));

        /* Prayer -- Dispel Undead */
        case BF_PRAYER_DISP_UNDEAD1:
        dam = ((auto_level * 3)/2);
        return (borg_attack_aux_prayer_dispel(3,1, dam, GF_DISP_UNDEAD));

        /* Prayer -- Dispel Evil */
        case BF_PRAYER_DISP_EVIL1:
        dam = ((auto_level * 4)/2);
        return (borg_attack_aux_prayer_dispel(3,2, dam, GF_DISP_EVIL));

        /* Prayer -- Dispel Undead2 Wrath of God */
        case BF_PRAYER_DISP_UNDEAD2:
        dam = ((auto_level * 4)/2);
        return (borg_attack_aux_prayer_dispel(8,0, dam, GF_DISP_UNDEAD));

        /* Prayer -- Dispel EVIL2 Wrath of God */
        case BF_PRAYER_DISP_EVIL2:
        dam = ((auto_level * 4)/2);
        return (borg_attack_aux_prayer_dispel(8,1, dam, GF_DISP_EVIL));

        /* Prayer -- Banishment (teleport evil away)*/
        /* This is a defense spell:  done in borg_defense() */

        /* Prayer -- Holy Word also has heal effect*/
        case BF_PRAYER_HOLY_WORD:
        if (auto_mhp - auto_chp > 300)
         /*  force him to think the spell is more deadly to get him to
          * cast it.  This will provide some healing for him.
          */
        {
         dam = ((auto_level * 10));
         return (borg_attack_aux_prayer_dispel(3,7, dam, GF_DISP_EVIL));
        }
        else /* If he is not wounded dont cast this, use Disp Evil instead. */
        {
         dam = ((auto_level * 3)/2) -50;
         return (borg_attack_aux_prayer_dispel(3,7, dam, GF_DISP_EVIL));
        }

        /* Prayer -- Drain Life Wrath of God */
        case BF_PRAYER_DRAIN_LIFE:
        dam = (300);
        return (borg_attack_aux_prayer_bolt(8,4, rad, dam, GF_OLD_DRAIN));

        /* Prayer -- Divine Intervention--paralyse ball */
        case BF_PRAYER_DIVINE_INTERVENTION:
        dam = (10);
        rad = 2;
        return (borg_attack_aux_prayer_bolt(8,4, rad, dam, GF_PARALYSE));


        /* ROD -- slow monster */
        case BF_ROD_SLOW_MONSTER:
        dam = 10;
        return (borg_attack_aux_rod_bolt(SV_ROD_SLOW_MONSTER, rad, dam, GF_OLD_SLOW));

        /* ROD -- sleep monster */
        case BF_ROD_SLEEP_MONSTER:
        dam = 10;
        return (borg_attack_aux_rod_bolt(SV_ROD_SLEEP_MONSTER, rad, dam, GF_OLD_SLEEP));

        /* Rod -- elec bolt */
        case BF_ROD_ELEC_BOLT:
        dam = 3*(8+1)/2;
        return (borg_attack_aux_rod_bolt(SV_ROD_ELEC_BOLT, rad, dam, GF_ELEC));

        /* Rod -- cold bolt */
        case BF_ROD_COLD_BOLT:
        dam = 5*(8+1)/2;
        return (borg_attack_aux_rod_bolt(SV_ROD_COLD_BOLT, rad, dam, GF_COLD));

        /* Rod -- acid bolt */
        case BF_ROD_ACID_BOLT:
        dam = 6*(8+1)/2;
        return (borg_attack_aux_rod_bolt(SV_ROD_ACID_BOLT, rad, dam, GF_ACID));

        /* Rod -- fire bolt */
        case BF_ROD_FIRE_BOLT:
        dam = 8*(8+1)/2;
        return (borg_attack_aux_rod_bolt(SV_ROD_FIRE_BOLT, rad, dam, GF_FIRE));

        /* Spell -- light beam */
        case BF_ROD_LITE_BEAM:
        rad = -1;
        dam = (6*(8+1)/2);
        return (borg_attack_aux_rod_bolt(SV_ROD_LITE, rad, dam, GF_LITE_WEAK));

        /* Spell -- drain life */
        case BF_ROD_DRAIN_LIFE:
        dam = (75);
        return (borg_attack_aux_rod_bolt(SV_ROD_DRAIN_LIFE, rad, dam, GF_OLD_DRAIN));

        /* Rod -- elec ball */
        case BF_ROD_ELEC_BALL:
        rad = 2;
        dam = 32;
        return (borg_attack_aux_rod_bolt(SV_ROD_ELEC_BALL, rad, dam, GF_ELEC));

        /* Rod -- acid ball */
        case BF_ROD_COLD_BALL:
        rad = 2;
        dam = 48;
        return (borg_attack_aux_rod_bolt(SV_ROD_COLD_BALL, rad, dam, GF_COLD));

        /* Rod -- acid ball */
        case BF_ROD_ACID_BALL:
        rad = 2;
        dam = 60;
        return (borg_attack_aux_rod_bolt(SV_ROD_ACID_BALL, rad, dam, GF_ACID));

        /* Rod -- fire ball */
        case BF_ROD_FIRE_BALL:
        rad = 2;
        dam = 72;
        return (borg_attack_aux_rod_bolt(SV_ROD_FIRE_BALL, rad, dam, GF_FIRE));


        /* Wand -- magic missile */
        case BF_WAND_MAGIC_MISSILE:
        dam = 2*(6+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_MAGIC_MISSILE, rad, dam, GF_MISSILE));

        /* Wand -- slow monster */
        case BF_WAND_SLOW_MONSTER:
        dam = 10;
        return (borg_attack_aux_wand_bolt(SV_WAND_SLOW_MONSTER, rad, dam, GF_OLD_SLOW));

        /* Wand -- sleep monster */
        case BF_WAND_SLEEP_MONSTER:
        dam = 10;
        return (borg_attack_aux_wand_bolt(SV_WAND_SLEEP_MONSTER, rad, dam, GF_OLD_SLEEP));

        /* Wand -- fear monster */
        case BF_WAND_FEAR_MONSTER:
        dam = 2*(6+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_FEAR_MONSTER, rad, dam, GF_TURN_ALL));

       /* Wand -- conf monster */
        case BF_WAND_CONFUSE_MONSTER:
        dam = 2*(6+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_CONFUSE_MONSTER, rad, dam, GF_OLD_CONF));

        /* Wand -- elec bolt */
        case BF_WAND_ELEC_BOLT:
        dam = 3*(8+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_ELEC_BOLT, rad, dam, GF_ELEC));

        /* Wand -- cold bolt */
        case BF_WAND_COLD_BOLT:
        dam = 3*(8+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_COLD_BOLT, rad, dam, GF_COLD));

        /* Wand -- acid bolt */
        case BF_WAND_ACID_BOLT:
        dam = 5*(8+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_ACID_BOLT, rad, dam, GF_ACID));

        /* Wand -- fire bolt */
        case BF_WAND_FIRE_BOLT:
        dam = 6*(8+1)/2;
        return (borg_attack_aux_wand_bolt(SV_WAND_FIRE_BOLT, rad, dam, GF_FIRE));

        /* Spell -- light beam */
        case BF_WAND_LITE_BEAM:
        rad = -1;
        dam = (6*(8+1)/2);
        return (borg_attack_aux_wand_bolt(SV_WAND_LITE, rad, dam, GF_LITE_WEAK));

        /* Wand -- stinking cloud */
        case BF_WAND_STINKING_CLOUD:
        rad = 2;
        dam = 12;
        return (borg_attack_aux_wand_bolt(SV_WAND_STINKING_CLOUD, rad, dam, GF_POIS));

        /* Wand -- elec ball */
        case BF_WAND_ELEC_BALL:
        rad = 2;
        dam = 32;
        return (borg_attack_aux_wand_bolt(SV_WAND_ELEC_BALL, rad, dam, GF_ELEC));

        /* Wand -- acid ball */
        case BF_WAND_COLD_BALL:
        rad = 2;
        dam = 48;
        return (borg_attack_aux_wand_bolt(SV_WAND_COLD_BALL, rad, dam, GF_COLD));

        /* Wand -- acid ball */
        case BF_WAND_ACID_BALL:
        rad = 2;
        dam = 60;
        return (borg_attack_aux_wand_bolt(SV_WAND_ACID_BALL, rad, dam, GF_ACID));

        /* Wand -- fire ball */
        case BF_WAND_FIRE_BALL:
        rad = 2;
        dam = 72;
        return (borg_attack_aux_wand_bolt(SV_WAND_FIRE_BALL, rad, dam, GF_FIRE));

        /* Wand -- dragon cold */
        case BF_WAND_DRAGON_COLD:
        rad = 3;
        dam = 80;
        return (borg_attack_aux_wand_bolt(SV_WAND_DRAGON_COLD, rad, dam, GF_COLD));

        /* Wand -- dragon fire */
        case BF_WAND_DRAGON_FIRE:
        rad = 3;
        dam = 100;
        return (borg_attack_aux_wand_bolt(SV_WAND_DRAGON_FIRE, rad, dam, GF_FIRE));

        /* Wand -- annihilation */
        case BF_WAND_ANNIHILATION:
        dam = 125;
        return (borg_attack_aux_wand_bolt(SV_WAND_ANNIHILATION, rad, dam, GF_OLD_DRAIN));

        /* Wand -- drain life */
        case BF_WAND_DRAIN_LIFE:
        dam = 75;
        return (borg_attack_aux_wand_bolt(SV_WAND_DRAIN_LIFE, rad, dam, GF_OLD_DRAIN));

        /* Wand -- wand of wonder */
        case BF_WAND_WONDER:
        dam = 35;
        return (borg_attack_aux_wand_bolt(SV_WAND_WONDER, rad, dam, GF_MISSILE));

        /* Staff -- Sleep Monsters */
        case BF_STAFF_SLEEP_MONSTERS:
        dam = 60;
        return (borg_attack_aux_staff_dispel(SV_STAFF_SLEEP_MONSTERS, rad, dam, GF_OLD_SLEEP));

        /* Staff -- Slow Monsters */
        case BF_STAFF_SLOW_MONSTERS:
        dam = 60;
        rad = 10;
        return (borg_attack_aux_staff_dispel(SV_STAFF_SLOW_MONSTERS, rad, dam, GF_OLD_SLOW));

        /* Staff -- Dispel Evil */
        case BF_STAFF_DISPEL_EVIL:
        dam = 60;
        return (borg_attack_aux_staff_dispel(SV_STAFF_DISPEL_EVIL, rad, dam, GF_DISP_EVIL));

        /* Staff -- Power */
        case BF_STAFF_POWER:
        dam = 120;
        return (borg_attack_aux_staff_dispel(SV_STAFF_POWER, rad, dam, GF_TURN_ALL));

        /* Staff -- holiness, dispel evil, heal */
        case BF_STAFF_HOLINESS:
        if (auto_chp < auto_mhp/2) dam = 500;
        else dam = 120;
        return (borg_attack_aux_staff_dispel(SV_STAFF_HOLINESS, rad, dam, GF_DISP_EVIL));


        /* Artifact -- Narthanc- fire bolt 9d8*/
        case BF_ART_NARTHANC:
        rad = 0;
        dam = (9*(8+1)/2);
        return (borg_attack_aux_artifact(ART_NARTHANC, INVEN_WIELD,rad, dam, GF_FIRE));

        /* Artifact -- Nimthanc- frost bolt 6d8*/
        case BF_ART_NIMTHANC:
        rad = 0;
        dam = (6*(8+1)/2);
        return (borg_attack_aux_artifact(ART_NIMTHANC, INVEN_WIELD, rad, dam, GF_COLD));

        /* Artifact -- Dethanc- electric bolt 4d8*/
        case BF_ART_DETHANC:
        rad = 0;
        dam = (4*(8+1)/2);
        return (borg_attack_aux_artifact(ART_DETHANC, INVEN_WIELD, rad, dam, GF_ELEC));

        /* Artifact -- Rilia- poison gas 12*/
        case BF_ART_RILIA:
        rad = 2;
        dam = 12;
        return (borg_attack_aux_artifact(ART_RILIA, INVEN_WIELD, rad, dam, GF_POIS));

        /* Artifact -- Belangil- frost ball 48*/
        case BF_ART_BELANGIL:
        rad = 2;
        dam = 48;
        return (borg_attack_aux_artifact(ART_BELANGIL,  INVEN_WIELD,rad, dam, GF_COLD));

        /* Artifact -- Arunruth- frost bolt 12d8*/
        case BF_ART_ARUNRUTH:
        rad = 0;
        dam = (12*(8+1)/2);
        return (borg_attack_aux_artifact(ART_ARUNRUTH,  INVEN_WIELD,rad, dam, GF_COLD));

        /* Artifact -- Ringil- frost ball 100*/
        case BF_ART_RINGIL:
        rad = 2;
        dam = 100;
        return (borg_attack_aux_artifact(ART_RINGIL,  INVEN_WIELD,rad, dam, GF_COLD));

        /* Artifact -- Anduril- fire ball 72*/
        case BF_ART_ANDURIL:
        rad = 2;
        dam = 72;
        return (borg_attack_aux_artifact(ART_ANDURIL, INVEN_WIELD, rad, dam, GF_FIRE));

        /* Artifact -- Theoden- drain Life 120*/
        case BF_ART_THEODEN:
        rad = 0;
        dam = 120;
        return (borg_attack_aux_artifact(ART_THEODEN, INVEN_WIELD, rad, dam, GF_OLD_DRAIN));

        /* Artifact -- Aeglos- frost ball 100*/
        case BF_ART_AEGLOS:
        rad = 2;
        dam = 100;
        return (borg_attack_aux_artifact(ART_AEGLOS,  INVEN_WIELD,rad, dam, GF_COLD));

        /* Artifact -- Totila- confustion */
        case BF_ART_TOTILA:
        rad = 0;
        dam = 10;
        return (borg_attack_aux_artifact(ART_TOTILA,  INVEN_WIELD,rad, dam, GF_OLD_CONF));

        /* Artifact -- Holcolleth -- sleep ii*/
        case BF_ART_HOLCOLLETH:
        dam = 10;
        return (borg_attack_aux_artifact_holcolleth());

        /* Artifact -- Firestar- fire ball 72 */
        case BF_ART_FIRESTAR:
        rad = 2;
        dam = 72;
        return (borg_attack_aux_artifact(ART_FIRESTAR,  INVEN_WIELD,rad, dam, GF_FIRE));

        /* Artifact -- TURMIL- drain life 90 */
        case BF_ART_TURMIL:
        rad = 0;
        dam = 90;
        return (borg_attack_aux_artifact(ART_TURMIL,  INVEN_WIELD,rad, dam, GF_OLD_DRAIN));

        /* Artifact -- Razorback- spikes 150 */
        case BF_ART_RAZORBACK:
        rad = 2;
        dam = 150;
        return (borg_attack_aux_artifact(ART_RAZORBACK,  INVEN_BODY,rad, dam, GF_MISSILE));

        /* Artifact -- Cammithrim- Magic Missile 2d6 */
        case BF_ART_CAMMITHRIM:
        rad = 0;
        dam = (2*(6+1)/2);
        return (borg_attack_aux_artifact(ART_CAMMITHRIM,  INVEN_HANDS,rad, dam, GF_MISSILE));

        /* Artifact -- Paurhach- fire bolt 9d8 */
        case BF_ART_PAURHACH:
        rad = 0;
        dam = (9*(8+1)/2);
        return (borg_attack_aux_artifact(ART_PAURHACH,  INVEN_HANDS,rad, dam, GF_FIRE));

        /* Artifact -- Paurnimmen- frost bolt 6d8 */
        case BF_ART_PAURNIMMEN:
        rad = 0;
        dam = (6*(8+1)/2);
        return (borg_attack_aux_artifact(ART_PAURNIMMEN, INVEN_HANDS, rad, dam, GF_COLD));

        /* Artifact -- Celeborn- great storm 200 */
        case BF_ART_CELEBORN:
        rad = 10;
        dam = 200;
        return (borg_attack_aux_artifact(ART_CELEBORN, INVEN_HANDS, rad, dam, GF_MISSILE));

        /* Artifact -- Pauraegen- lightning bolt 4d8 */
        case BF_ART_PAURAEGEN:
        rad = 0;
        dam = (4*(8+1)/2);
        return (borg_attack_aux_artifact(ART_PAURAEGEN,  INVEN_HANDS,rad, dam, GF_ELEC));

        /* Artifact -- PaurNEN- ACID bolt 5d8 */
        case BF_ART_PAURNEN:
        rad = 0;
        dam = (5*(8+1)/2);
        return (borg_attack_aux_artifact(ART_PAURNEN,  INVEN_HANDS,rad, dam, GF_ACID));

        /* Artifact -- FINGOLFIN- MISSILE 150 */
        case BF_ART_FINGOLFIN:
        rad = 0;
        dam = 150;
        return (borg_attack_aux_artifact(ART_FINGOLFIN, INVEN_HANDS, rad, dam, GF_ARROW));

        /* Artifact -- INGWE- DISPEL EVIL X5 */
        case BF_ART_INGWE:
        rad = 10;
        dam = (10 + (auto_level*5)/2);
        return (borg_attack_aux_artifact(ART_INGWE,  INVEN_NECK,rad, dam, GF_DISP_EVIL));

        /* Artifact -- NARYA- FIRE BALL 120 */
        case BF_ART_NARYA:
        rad = 2;
        dam = 120;
        return (borg_attack_aux_artifact(ART_NARYA,  INVEN_RIGHT,rad, dam, GF_FIRE));

        /* Artifact -- NENYA- COLD BALL 200 */
        case BF_ART_NENYA:
        rad = 2;
        dam = 200;
        return (borg_attack_aux_artifact(ART_NENYA,  INVEN_RIGHT,rad, dam, GF_COLD));

        /* Artifact -- VILYA- ELEC BALL 250 */
        case BF_ART_VILYA:
        rad = 2;
        dam = 250;
        return (borg_attack_aux_artifact(ART_VILYA,  INVEN_RIGHT,rad, dam, GF_ELEC));

    /* Hack -- Dragon Scale Mail can be activated as well */
            case BF_DRAGON_BLUE:
            rad =2;
            dam=100;
            return (borg_attack_aux_dragon(SV_DRAGON_BLUE, rad, dam, GF_ELEC));

            case BF_DRAGON_WHITE:
            rad =2;
            dam=110;
            return (borg_attack_aux_dragon(SV_DRAGON_WHITE, rad, dam, GF_COLD));

            case BF_DRAGON_BLACK:
            rad =2;
            dam=130;
            return (borg_attack_aux_dragon(SV_DRAGON_BLACK, rad, dam, GF_ACID));

            case BF_DRAGON_GREEN:
            rad =2;
            dam=150;
            return (borg_attack_aux_dragon(SV_DRAGON_GREEN, rad, dam, GF_POIS));

            case BF_DRAGON_RED:
            rad =2;
            dam=200;
            return (borg_attack_aux_dragon(SV_DRAGON_RED, rad, dam, GF_FIRE));

            case BF_DRAGON_MULTIHUED:
				chance = rand_int(5);
            rad =2;
            dam=200;
            return (borg_attack_aux_dragon(SV_DRAGON_MULTIHUED, rad, dam,
                    (((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
                             ((chance == 4) ? GF_POIS : GF_FIRE)))) )) );

            case BF_DRAGON_BRONZE:
            rad =2;
            dam=120;
            return (borg_attack_aux_dragon(SV_DRAGON_BRONZE, rad, dam, GF_CONFUSION));

            case BF_DRAGON_GOLD:
            rad =2;
            dam=150;
            return (borg_attack_aux_dragon(SV_DRAGON_GOLD, rad, dam, GF_SOUND));

            case BF_DRAGON_CHAOS:
            chance = rand_int(2);
            rad =2;
            dam=220;
            return (borg_attack_aux_dragon(SV_DRAGON_CHAOS, rad, dam,
                (chance == 1 ? GF_CHAOS : GF_DISENCHANT)) );

            case BF_DRAGON_LAW:
            chance = rand_int(2);
            rad =2;
            dam=230;
            return (borg_attack_aux_dragon(SV_DRAGON_LAW, rad, dam,
                (chance == 1 ? GF_SOUND : GF_SHARD)) );

            case BF_DRAGON_BALANCE:
            chance = rand_int(4);
            rad =2;
            dam=230;
            return (borg_attack_aux_dragon(SV_DRAGON_BALANCE, rad, dam,
              ( ((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
                            ((chance == 3) ? GF_SOUND : GF_SHARD))) )) );

            case BF_DRAGON_SHINING:
            chance = rand_int(2);
            rad =2;
            dam=200;
            return (borg_attack_aux_dragon(SV_DRAGON_SHINING, rad, dam,
                (chance == 0 ? GF_LITE : GF_DARK)) );

            case BF_DRAGON_POWER:
            rad =2;
            dam=300;
            return (borg_attack_aux_dragon(SV_DRAGON_POWER, rad, dam, GF_MISSILE));


            case BF_RACIAL_PIXIE:
            if (auto_race != RACE_PIXIE) return (0);
            dam = 1;
            rad = 0;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_CONFUSION));

            case BF_RACIAL_BOLT_CRYSTAL:
            if (auto_level < 5 || auto_race != RACE_CRYSTALDRAG) return (0);
            dam = auto_chp/4;
            rad = 0;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_SHARD));

            case BF_RACIAL_BOLT_COPPER:
            if (auto_level < 5 || auto_race != RACE_COPPERDRAG) return (0);
            dam = auto_chp/4;
            rad = 0;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_DISENCHANT));

            case BF_RACIAL_BOLT_BRONZE:
            if (auto_level < 5 || auto_race != RACE_BRONZEDRAG) return (0);
            dam = auto_chp/4;
            rad = 0;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_CONFUSION));

            case BF_RACIAL_BOLT_GOLD:
            if (auto_level < 5 || auto_race != RACE_GOLDDRAG) return (0);
            dam = auto_chp/4;
            rad = 0;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_SOUND));

            case BF_RACIAL_BOLT_PSEUDO:
            if (auto_level < 5 || auto_race != RACE_PSEUDODRAG) return (0);
            dam = auto_chp/4;
            rad = -1;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_LITE));

            case BF_RACIAL_BALL_CRYSTAL:
            if (auto_level < 25 || auto_race != RACE_CRYSTALDRAG) return (0);
            dam = auto_chp/3;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('b', rad, dam, GF_SHARD));

            case BF_RACIAL_BALL_COPPER:
            if (auto_level < 25 || auto_race != RACE_COPPERDRAG) return (0);
            dam = auto_chp/3;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('b', rad, dam, GF_DISENCHANT));

            case BF_RACIAL_BALL_BRONZE:
            if (auto_level < 25 || auto_race != RACE_BRONZEDRAG) return (0);
            dam = auto_chp/3;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('b', rad, dam, GF_CONFUSION));

            case BF_RACIAL_BALL_GOLD:
            if (auto_level < 25 || auto_race != RACE_GOLDDRAG) return (0);
            dam = auto_chp/3;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('b', rad, dam, GF_SOUND));

            case BF_RACIAL_BALL_PSEUDO:
            if (auto_level < 25 || auto_race != RACE_PSEUDODRAG) return (0);
            dam = auto_chp/3;
            rad = (auto_level/5) +1;
            return (borg_attack_aux_racial_dispel('b', rad, dam, GF_LITE));

            case BF_RACIAL_BALL_MULTI_POIS:
            if (auto_level < 45 || auto_race != RACE_MULTIHUEDDRAG) return (0);
            dam = auto_chp*10/36;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('e', rad, dam, GF_POIS));

            case BF_RACIAL_BALL_MULTI_FIRE:
            if (auto_level < 35 || auto_race != RACE_MULTIHUEDDRAG) return (0);
            dam = auto_chp*10/30;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('d', rad, dam, GF_FIRE));

            case BF_RACIAL_BALL_MULTI_COLD:
            if (auto_level < 25 || auto_race != RACE_MULTIHUEDDRAG) return (0);
            dam = auto_chp*10/35;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('c', rad, dam, GF_COLD));

            case BF_RACIAL_BALL_MULTI_ELEC:
            if (auto_level < 15 || auto_race != RACE_MULTIHUEDDRAG) return (0);
            dam = auto_chp*10/40;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('b', rad, dam, GF_ELEC));

            case BF_RACIAL_BALL_MULTI_ACID:
            if (auto_level < 5 || auto_race != RACE_MULTIHUEDDRAG) return (0);
            dam = auto_chp*10/45;
            rad = (auto_level/10) +1;
            return (borg_attack_aux_racial_bolt('a', rad, dam, GF_ACID));
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
    int i, x, y;

    int n, b_n = 0;
    int g, b_g = -1;

    auto_grid *ag;

    /* Nobody around */
    if (!auto_kills_cnt) return (FALSE);

    /* Set the attacking flag so that danger is boosted for monsters */
    /* we want to attack first. */
    borg_attacking = TRUE;

    /* Reset list */
    auto_temp_n = 0;

    /* Find "nearby" monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {

        auto_kill *kill;

        /* Monster */
        kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Require current knowledge */
        if (kill->when < c_t) continue;

        /* Ignore multiplying monsters */
        if (goal_ignoring && !do_afraid &&
            (r_info[kill->r_idx].flags2 & RF2_MULTIPLY)) continue;

        /* Acquire location */
        x = kill->x;
        y = kill->y;

        /* Get grid */
        ag = &auto_grids[y][x];

        /* Never shoot off-screen */
        if (!(ag->info & BORG_OKAY)) continue;

        /* Never shoot through walls */
        if (!(ag->info & BORG_VIEW)) continue;

        /* Check the distance XXX XXX XXX */
        if (distance(c_y, c_x, y, x) > 16) continue;

        /* Save the location (careful) */
        auto_temp_x[auto_temp_n] = x;
        auto_temp_y[auto_temp_n] = y;
        auto_temp_n++;
    }

    /* No destinations */
    if (!auto_temp_n)
    {
        borg_attacking = FALSE;
        return (FALSE);
    }

    /* Simulate */
    auto_simulate = TRUE;

    /* Analyze the possible attacks */
    for (g = 0; g < BF_MAX; g++)
    {
        /* Simulate */
        n = borg_attack_aux(g);

        /* Track "best" attack  <= */
        if (n <= b_n) continue;

        /* Track best */
        b_g = g;
        b_n = n;
    }

    /* Nothing good */
    if (b_n <= 0)
    {
        borg_attacking = FALSE;
        return (FALSE);
    }


    /* Note */
    borg_note(format("# Performing attack type %d with value %d.", b_g, b_n));

    /* Instantiate */
    auto_simulate = FALSE;

    /* Instantiate */
    (void)borg_attack_aux(b_g);

    borg_attacking = FALSE;

    /* Success */
    return (TRUE);
}

/*
 * try to make this look like borg_attack stuff
 *
 * There are several types of seup moves:
 *
 *   Temporary speed
 *   Protect From Evil
 *   Bless\Prayer
 *   Heroism
 *   Temp Resist (either all or just cold/fire?)
 *   Shield
 *   Teleport away
 *   Glyph of Warding
 *   See inviso
 *
 * * and many others
 *
 */
enum
{
    BD_SPEED,
    BD_PROT_FROM_EVIL,
    BD_BLESS,
    BD_HERO,
    BD_RESIST1,
    BD_RESIST2,
    BD_RESIST3,
    BD_RESIST4,
    BD_RESIST5,
    BD_RESIST6,
    BD_SHIELD,
    BD_GOI,

    BD_ETHEREAL_LOCK,
    BD_ARMOR_OF_LIGHT,
    BD_CREATE_DOOR_WALL,
    BD_INVISO,
    BD_GLYPH,
    BD_TELL_AWAY,
    BD_CREATE_DOORS,
    BD_SCATTER_FOE,
    BD_EARTHQUAKE,
    BD_DESTRUCTION,
    BD_BANISHMENT,
    BD_DETECT_INVISO,
    BD_LIGHT_BEAM,
    BD_SHIFT_PANEL,

    BD_MAX
};

/*
 * Bless/Prayer to prepare for battle
 */
static int borg_defend_aux_bless( int p1 )
{
    int fail_allowed = 10;

    /* already blessed */
    if (borg_bless)
        return (0);

    if ( !borg_prayer_okay_fail(0, 2, fail_allowed) &&
         !borg_prayer_okay_fail(3, 1, fail_allowed))
        return (0);

    /* if we are in some danger but not much, go for a quick bless */
    if (p1 > avoidance/12 && p1 < avoidance/2)
    {
        /* Simulation */
        /* bless is a low priority */
        if (auto_simulate) return (1);

        /* do it! */
        if (borg_prayer(0, 2 ) || borg_prayer(3,1) )
             return 1;
    }

    return (0);
}

/*
 * Speed to prepare for battle
 */
static int borg_defend_aux_speed( int p1 )
{
    int p2 = 0;
    bool good_speed = FALSE;
    bool speed_spell = FALSE;
    bool speed_staff = FALSE;
    bool speed_rod = FALSE;
    int fail_allowed = 39;

    /* already fast */
    if (borg_speed)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    /* only cast defence spells if fail rate is not too high */
    if ( borg_spell_okay_fail( 3, 3, fail_allowed ))
        speed_spell = TRUE;

    /* staff must have charges */
    if ( borg_equips_staff_fail (SV_STAFF_SPEED))
        speed_staff = TRUE;

    /* rod can't be charging */
    if (borg_equips_rod(SV_ROD_SPEED)) speed_rod = TRUE;

    if (0 > borg_slot(TV_POTION, SV_POTION_SPEED) &&
        !speed_staff &&
        !speed_rod &&
        !speed_spell &&
        !borg_equips_artifact(ART_FEANOR, INVEN_FEET) &&
        !borg_equips_artifact(ART_TARATOL, INVEN_WIELD) &&
        !borg_equips_artifact(ART_TULKAS, INVEN_LEFT))
        return (0);

    /* if we have an infinite/large suppy of speed we can */
    /* be generious with our use */
    if (speed_rod || speed_spell || speed_staff ||
       borg_equips_artifact(ART_TULKAS, INVEN_RIGHT) ||
       borg_equips_artifact(ART_FEANOR, INVEN_FEET) ||
       borg_equips_artifact(ART_TARATOL, INVEN_WIELD))
       good_speed = TRUE;

    /* pretend we are protected and look again */
    borg_speed = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_speed = FALSE;

    /* if we are fighting a unique cast it. */
    if (good_speed && borg_fighting_unique)
    {
        /* HACK pretend that it was scary and will be safer */
        p2 = p2 * 7/10;
    }
    /* if we are fighting a unique and a summoner cast it. */
    if (borg_fighting_summoner && borg_fighting_unique)
    {
        /* HACK pretend that it was scary and will be safer */
        p2 = p2 * 7/10;
    }

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if ( ((p1 > p2) &&
           p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
           (p1 > (avoidance/7)) && good_speed) ||
         ((p1 > p2) &&
         p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/3)) &&
         (p1 > (avoidance/8))))
    {
        /* if we just did GOI or LOCK do a speed right after. */
        if (good_speed && (borg_goi || borg_lock))
        {
            /* HACK pretend that it was scary and will be very safe */
            /* This is done because GOI messes up our calculations */
            p1 = 10000;
            p2 = 1;
        }

        /* Simulation */
        if (auto_simulate) return (p1-p2 + borg_goi * 50);

        /* do it! */
        if (borg_spell_fail( 3, 3, fail_allowed))
            return (p1-p2);

        if ( borg_zap_rod( SV_ROD_SPEED ) ||
             borg_use_staff( SV_STAFF_SPEED) ||
             borg_quaff_potion(SV_POTION_SPEED) ||
             borg_activate_artifact(ART_FEANOR, INVEN_FEET) ||
             borg_activate_artifact(ART_TARATOL, INVEN_WIELD) ||
             borg_activate_artifact(ART_TULKAS, INVEN_RIGHT) )

            /* Value */
            return (p1-p2 + borg_goi * 50);
    }
    /* default to can't do it. */
    return (0);
}

/*
 * Globe of Invulnurability
 */
static int borg_defend_aux_goi(int p1)
{
    int p2 = 0;
    int fail_allowed = 39;

    if (borg_goi)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* a bit scary */
    if ( p1 > (avoidance/2))
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/4)
        fail_allowed += 10;

    if  (!borg_spell_okay_fail(7, 5, fail_allowed))
        return (0);

    /* pretend we are protected and look again */
    borg_goi = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_goi = FALSE;

    /*  if we are fighting a unique enhance the value by reducing p2*/
    if (borg_fighting_unique)
    {
        p2 = p2 / 2;
    }

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_spell_fail(7, 5, fail_allowed))
        return (p1-p2);

    }

    /* default to can't do it. */
    return (0);
}

/* cold/fire */
static int borg_defend_aux_resist1( p1 )
{
    int p2 = 0;
    int fail_allowed = 39;
    bool    save_fire,
            save_cold;

    if (my_oppose_fire &&
        my_oppose_cold)
        return (0);

    if (my_resist_fire &&
        my_resist_cold)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    if (!borg_prayer_okay_fail(1, 7, fail_allowed) &&
        !borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    /* pretend we are protected and look again */
    save_fire = my_oppose_fire;
    save_cold = my_oppose_cold;
    my_oppose_fire = TRUE;
    my_oppose_cold = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_oppose_fire = save_fire;
    my_oppose_cold = save_cold;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) ||
            borg_prayer_fail(1, 7, fail_allowed) )

        /* Value */
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}

/* all resists */
static int borg_defend_aux_resist2( int p1)
{
    int p2 = 0;
    int fail_allowed = 39;
    bool    save_fire,
            save_acid,
            save_poison,
            save_elec,
            save_cold;

    if (my_oppose_fire &&
        my_oppose_acid &&
        my_oppose_pois &&
        my_oppose_elec &&
        my_oppose_cold)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    if (!borg_spell_okay_fail(4, 5, fail_allowed) &&
        !borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    /* pretend we are protected and look again */
    save_fire = my_oppose_fire;
    save_acid = my_oppose_acid;
    save_poison =  my_oppose_pois;
    save_elec = my_oppose_elec;
    save_cold = my_oppose_cold;
    my_oppose_fire = TRUE;
    my_oppose_cold = TRUE;
    my_oppose_acid = TRUE;
    my_oppose_pois = TRUE;
    my_oppose_elec = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_oppose_fire = save_fire;
    my_oppose_cold = save_cold;
    my_oppose_acid = save_acid;
    my_oppose_pois = save_poison;
    my_oppose_elec = save_elec;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return ((p1-p2) -1);

        /* do it! */
        if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) ||
            borg_spell_fail(4, 5, fail_allowed) )

        /* Value */
        return ((p1-p2)-1);
    }

    /* default to can't do it. */
    return (0);
}
/* fire */
static int borg_defend_aux_resist3( p1 )
{

    int p2 = 0;
    int fail_allowed = 39;
    bool    save_fire;

    if (my_oppose_fire)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    if (!borg_spell_okay_fail(4, 0, fail_allowed) &&
        !borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    save_fire = my_oppose_fire;
    /* pretend we are protected and look again */
    my_oppose_fire = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_oppose_fire = save_fire;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) ||
            borg_spell_fail(4, 0, fail_allowed) )

        /* Value */
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}

 /* cold */
static int borg_defend_aux_resist4( p1 )
{

    int p2 = 0;
    int fail_allowed = 39;
    bool    save_cold;

    if ( my_oppose_cold )
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
       fail_allowed += 10;

    if (!borg_spell_okay_fail(4, 1, fail_allowed) &&
        !borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    save_cold = my_oppose_cold;
    /* pretend we are protected and look again */
    my_oppose_cold = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_oppose_cold = save_cold;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

       /* do it! */
        if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) ||
            borg_spell_fail(4, 1, fail_allowed) )

        /* Value */
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}


 /* Acid - Elec*/
static int borg_defend_aux_resist5( p1 )
{

    int p2 = 0;
    int fail_allowed = 39;
    bool    save_acid;
    bool    save_elec;

    if ( my_oppose_acid && my_oppose_elec)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
       fail_allowed += 10;

    if (!borg_spell_okay_fail(4, 2, fail_allowed) &&
        !borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    save_acid = my_oppose_acid;
    save_elec = my_oppose_elec;
    /* pretend we are protected and look again */
    my_oppose_acid = TRUE;
    my_oppose_elec = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_oppose_acid = save_acid;
    my_oppose_elec = save_elec;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) ||
            borg_spell_fail(4, 2, fail_allowed) )

        /* Value */
        return (p1-p2);
    }
    /* default to can't do it. */
    return (0);
}


/* poison */
static int borg_defend_aux_resist6( p1 )
{
    int p2 = 0;
    int fail_allowed = 39;
    bool    save_poison;

    if (my_oppose_pois)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    if (!borg_spell_okay_fail(4, 3, fail_allowed) &&
        !borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    save_poison = my_oppose_pois;
    /* pretend we are protected and look again */
    my_oppose_pois = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_oppose_pois = save_poison;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) ||
            borg_spell_fail(4, 3, fail_allowed) )

        /* Value */
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}

static int borg_defend_aux_prot_evil( int p1)
{
    int p2 = 0;
    int fail_allowed = 39;
    bool pfe_spell = FALSE;
    auto_grid *ag = &auto_grids[c_y][c_x];

    /* if already protected */
    if (borg_prot_from_evil)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    if (borg_prayer_okay_fail(2,4,fail_allowed)) pfe_spell= TRUE;

    if ( 0 <= borg_slot(TV_SCROLL,SV_SCROLL_PROTECTION_FROM_EVIL)) pfe_spell = TRUE;

    if (do_blind || do_confused || do_image)
        pfe_spell = FALSE;

    /* cant cast if dark */
    if (!(ag->info & BORG_GLOW) && !my_cur_lite)
        pfe_spell = FALSE;

    if (borg_equips_artifact(ART_CARLAMMAS,INVEN_NECK)) pfe_spell = TRUE;

    if (!pfe_spell) return (0);


    /* pretend we are protected and look again */
    borg_prot_from_evil = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_prot_from_evil = FALSE;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */

    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_prayer_fail(2, 4, fail_allowed) ||
           borg_activate_artifact(ART_CARLAMMAS, INVEN_NECK) ||
           borg_read_scroll(SV_SCROLL_PROTECTION_FROM_EVIL) )

        /* Value */
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}

static int borg_defend_aux_shield( int p1)
{
    int p2 = 0;
    int fail_allowed = 39;

    /* if already protected */
    if (borg_shield || borg_goi)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 5;

    if (!borg_spell_okay_fail(7, 2, fail_allowed))
        return (0);

    /* pretend we are protected and look again */
    borg_shield = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_shield = FALSE;

    /* slightly enhance the value if fighting a unique */
    if (borg_fighting_unique)  p2=(p2*7/10);


    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        borg_spell_fail(7, 2, fail_allowed);
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}

static int borg_defend_aux_inviso( int p1)
{
    int p2 = 0;
    int fail_allowed = 39;

    /* if already protected */
    if (my_inviso)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 5;

    if (!borg_spell_okay_fail(3, 0, fail_allowed))
        return (0);

    /* pretend we are protected and look again */
    my_inviso = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    my_inviso = FALSE;

    /* slightly enhance the value if fighting a unique */
    if (borg_fighting_unique)  p2=(p2*7/10);


    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        borg_spell_fail(3, 0, fail_allowed);
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}
/*
 * Ethereal Lock
 *
 */
static int borg_defend_aux_ethereal_lock(int p1)
{
    int p2 = 0;
    int fail_allowed = 39;
    if (borg_lock)
        return 0;

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 10;
    else
    /* a bit scary */
    if ( p1 > (avoidance/2))
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/4)
        fail_allowed += 10;

    if (!borg_prayer_okay_fail(4, 5, fail_allowed))
        return -1;

    /* pretend we are protected and look again */
    borg_lock = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_lock = FALSE;

    /*  if we are fighting a unique enhance the value by reducing p2*/
    if (borg_fighting_unique)
    {
        p2 = p2 / 2;
    }

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_prayer_fail(4, 5,fail_allowed))
        return (p1-p2);
    }
   if (!auto_simulate)
    {
        if (borg_prayer_fail(4, 5,fail_allowed))
        borg_note ("# Had to use the back up action step.");
        return (p1-p2);
    }

    /* default to can't do it. */
    return 0;
}


static int borg_defend_aux_armor_of_light( int p1)
{
    int p2 = 0;
    int fail_allowed = 39;

    /* if already protected */
    if (borg_prot_from_evil)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 10;

    if (!borg_prayer_okay_fail(6,3,fail_allowed)) return (FALSE);

    /* pretend we are protected and look again */
    borg_prot_from_evil = TRUE;
    borg_shield = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_prot_from_evil = FALSE;
    borg_shield = FALSE;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_prayer_fail(6, 3, fail_allowed))

        /* Value */
        return (p1-p2);
    }

    /* default to can't do it. */
    return (0);
}

/*
 * Try to get rid of all of the non-uniques around so you can go at it
 * 'mano-e-mano' with the unique.
 */
static int borg_defend_aux_tell_away( int p1)
{
    int p2= 0, b_n = 0;
    int fail_allowed = 30;
    bool  spell_ok;

    /* Only tell away if scared */
    if ( p1 < avoidance)
        return (0);

    spell_ok = FALSE;

	/* not with Ethereal Lock */
	if (borg_lock) return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance*4)
        fail_allowed -= 18;
    else
    /* scary */
    if ( p1 > avoidance*3)
        fail_allowed -= 12;
    else
    /* a little scary */
    if ( p1 > (avoidance*5)/2)
        fail_allowed += 5;

    if (borg_spell_okay_fail(3, 4, fail_allowed) ||
        borg_prayer_okay_fail(4, 2, fail_allowed) ||
        borg_equips_artifact(ART_ULMO, INVEN_WIELD) ||
        ( -1 != borg_slot(TV_WAND, SV_WAND_TELEPORT_AWAY) &&
         auto_items[borg_slot(TV_WAND, SV_WAND_TELEPORT_AWAY)].pval))
         spell_ok = TRUE;

        if (!spell_ok) return (0);

    /* chose then target a bad guy */
    b_n = borg_launch_bolt(-1, p1, GF_AWAY_ALL, MAX_RANGE);

    /* normalize the value */
    p2 = (p1 - b_n);
    if (p2 < 0) p2 = 0;

    /* check to see if I am left better off */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p2);

        /* Cast the spell */
        if (borg_spell_fail(3, 4, fail_allowed) ||
            borg_prayer_fail(4, 2, fail_allowed) ||
            borg_activate_artifact(ART_ULMO, INVEN_WIELD)||
            borg_aim_wand(SV_WAND_TELEPORT_AWAY))
        {
            /* Use target */
            borg_keypress('5');

            /* Set our shooting flag */
            successful_target = -1;

            /* Value */
            return (p2);
        }
    }
        return (0);
}

/*
 * Hero to prepare for battle
 */
static int borg_defend_aux_hero( int p1 )
{
    int fail_allowed = 10;

    /* already hero */
    if (borg_hero || borg_berserk)
        return (0);

    if ( !borg_spell_okay_fail(7, 0, fail_allowed ))
        return (0);

    /* if we are in some danger but not much, go for a quick bless */
    if ((p1 > avoidance/12 && p1 < avoidance/2) ||
        (borg_fighting_unique && p1 < avoidance *13/10))
    {
        /* Simulation */
        /* hero is a low priority */
        if (auto_simulate) return (1);

        /* do it! */
        if (borg_spell(7, 0 ))
             return 1;
    }

    return (0);
}

/* Glyph of Warding and Rune of Protection */
static int borg_defend_aux_glyph( int p1)
{
    int p2 = 0, i;
    int fail_allowed = 30;
    bool glyph_spell = FALSE;

    auto_grid *ag = &auto_grids[c_y][c_x];

    /* He should not cast it while on an object.  There is a chance that
     * he will enter a level via stairs, then immediately cast a glyph while
     * still standing on the stairway.  Having not scanned the screen, he
     * would not be aware of the stairs under him.  So he would cast the spell.
     *
     * I have addressed this inadequately in borg9.c when dealing with
     * messages.  The message "the object resists" will delete the glyph
     * from the array.  Then I set a broken door on that spot, the borg ignores
     * broken doors, so he won't loop.
     */

    if ( (ag->take) ||
         (ag->feat == FEAT_GLYPH) ||
         ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL)) ||
         ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_TAIL)) ||
         (ag->feat == FEAT_LESS) ||
         (ag->feat == FEAT_MORE) ||
         (ag->feat == FEAT_OPEN) ||
         (ag->feat == FEAT_BROKEN) )
        {
            return (0);
        }

        /* dont use this on Morgoth, he breaks them too easily */
        if (borg_fighting_unique ==2) return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 20;

    if (borg_prayer_okay_fail(3,6,fail_allowed)) glyph_spell = TRUE;

    if ( 0 <= borg_slot(TV_SCROLL,SV_SCROLL_RUNE_OF_PROTECTION)) glyph_spell = TRUE;

    if ((do_blind || do_confused || do_image) && glyph_spell)
        glyph_spell = FALSE;

    /* Can't cast if dark */
    if (!(ag->info & BORG_GLOW) && !my_cur_lite)
        glyph_spell = FALSE;

    if (!glyph_spell) return (0);

    /* pretend we are protected and look again */
    borg_on_glyph = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_on_glyph = FALSE;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_prayer_fail(3, 6, fail_allowed) ||
            borg_read_scroll(SV_SCROLL_RUNE_OF_PROTECTION))
        {
            /* Check for an existing glyph */
            for (i = 0; i < track_glyph_num; i++)
            {
                /* Stop if we already new about this glyph */
                if ((track_glyph_x[i] == c_x) && (track_glyph_y[i] == c_y)) return (p1-p2);
            }

            /* Track the newly discovered glyph */
            if ((i == track_glyph_num) && (track_glyph_size))
            {
                borg_note("# Noting the creation of a glyph.");
                track_glyph_num++;
                track_glyph_x[i] = c_x;
                track_glyph_y[i] = c_y;
            }

            /* setting a flag in case the spell fails. */
            borg_casted_glyph = TRUE;

            return (p1-p2);
        }

    }

    /* default to can't do it. */
    return (0);
}

/* Create Door or Wall */
static int borg_defend_aux_create_door_wall( int p1)
{
    int p2 = 0;
    int fail_allowed = 30;
    int b_n=0;

    /* any summoners near?*/
    if (!borg_fighting_summoner) return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 20;

    if (!borg_spell_okay_fail(5, 5, fail_allowed) &&
        !borg_prayer_okay_fail(4,3, fail_allowed))
        return (0);

    /* chose then target a summoning guy, we chose AWAY so that we dont
     * block off uniques
     */
    b_n = borg_launch_bolt(-1, p1, GF_AWAY_ALL, MAX_RANGE);

    /* normalize the value */
    p2 = (p1 - b_n);
    if (p2 < 0) p2 = 0;


    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_spell_fail(5, 5, fail_allowed) ||
            borg_prayer_fail(4, 3, fail_allowed))
        {
            /* Use target */
            borg_keypress('5');

            /* Value */
            return (p1-p2);
        }
    }

    /* default to can't do it. */
    return (0);
}

/* Create Doors */
static int borg_defend_aux_create_doors( int p1)
{
    int p2 = 0;
    int fail_allowed = 30;
    int door_bad =0;
    int door_x, door_y,x,y;

    auto_grid *ag;


    /* any summoners near?*/
    if (!borg_fighting_summoner) return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance)
        fail_allowed -= 19;
    else
    /* a little scary */
    if ( p1 > (avoidance*2)/3)
        fail_allowed -= 5;
    else
    /* not very scary, allow lots of fail */
    if ( p1 < avoidance/3)
        fail_allowed += 20;

    if (!borg_spell_okay_fail(2, 2, fail_allowed ))
        return (0);

    /* Do not cast if surounded by doors or something */
    /* Get grid */
    for (door_x = -1; door_x <= 1; door_x++)
    {
        for (door_y = -1; door_y <= 1; door_y++)
        {
            /* Acquire location */
            x = door_x + c_x;
            y = door_y + c_y;

            ag = &auto_grids[y][x];

            /* track spaces already protected */
            if ( (ag->feat == FEAT_GLYPH) || ag->kill ||
               ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_PERM_SOLID)))
            {
                door_bad++;
            }

            /* track spaces that cannot be protected */
            if ( (ag->take) ||
               ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL)) ||
               (ag->feat == FEAT_LESS) ||
               (ag->feat == FEAT_MORE) ||
               (ag->feat == FEAT_OPEN) ||
               (ag->feat == FEAT_BROKEN) ||
               (ag->kill))
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
        return (0);
    }

    /* pretend we are protected and look again */
    borg_create_door = TRUE;
    p2 = borg_danger(c_y, c_x, 1);
    borg_create_door = FALSE;

    /* if this is an improvement and we may not avoid monster now and */
    /* we may have before */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p1-p2);

        /* do it! */
        if (borg_spell_fail(2, 2, fail_allowed))
        {
            /* Set the breeder flag so that doors stay closed. */
            breeder_level = TRUE;

            /* Value */
            return (p1-p2);
        }
    }

    /* default to can't do it. */
    return (0);
}
/* This will simulate and cast the Scatter Foe spell and Mass Genocide-types.
 */
static int borg_defend_aux_scatter_foe(void)
{
    int p1= 0, hit = 0, i, y, x,p2;

    /* see if spell is legal */
    if (!borg_spell_okay_fail(6, 5, 40) &&
        !borg_equips_artifact(ART_EONWE, INVEN_WIELD))
        return (0);

	/* not with Ethereal Lock */
	if (borg_lock) return (0);

    /* Obtain initial danger, measured over time*/
    p1= borg_danger(c_y,c_x,1);

    /* See if he is in real danger */
    if (p1 < avoidance * 2)
        return (0);

    /* Ending danger */
    p2 = 0;

    /* Penalize for loss of HP from casting the spell */
    /* Scan the known monster list */
    for (i = 0; i < auto_kills_nxt; i++)
    {

        auto_kill *kill;
        monster_race *r_ptr;

        /* Monster */
        kill = &auto_kills[i];
        r_ptr = &r_info[kill->r_idx];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Acquire location */
        x = kill->x;
        y = kill->y;

        /* Check the distance */
        if (distance(c_y, c_x, y, x) > 16) continue;

        /* take a hit, add the damage */
        hit = hit + 2;
    }

    /* if strain is greater than hp, don't cast it */
    if (hit >= auto_chp) p2= p1 + 1000;

    /* Penalize the strain from casting the spell */
    p2 = p2 + hit;


    /* Simulation */
    if (auto_simulate) return (p1-p2);

    /* Cast the spell */
    if (borg_spell(6, 5) ||
        borg_activate_artifact(ART_EONWE, INVEN_WIELD))
        {
            /* Value */
            return (p1-p2);
        }
     else
     return (0);

}

/* Earthquake, priest and mage spells */
static int borg_defend_aux_earthquake(void)
{
    int p1= 0;
    int p2 = 0;
    int d = 0;
    int y,x, door_y, door_x;
    int door_bad =0;
    auto_grid *ag;

    /* Obtain initial danger */
    p1= borg_danger(c_y,c_x,1);

    if (!borg_prayer_okay_fail(2, 5, 35) &&
        !borg_spell_okay_fail(5, 3, 35))
        return (0);

    /* Do not cast if surounded by doors or something */
    /* Get grid */
    for (door_x = -1; door_x <= 1; door_x++)
    {
        for (door_y = -1; door_y <= 1; door_y++)
        {
            /* Acquire location */
            x = door_x + c_x;
            y = door_y + c_y;

            ag = &auto_grids[y][x];

            /* track spaces already protected */
            if ( (ag->feat == FEAT_GLYPH) || ag->kill ||
               ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_PERM_SOLID)))
            {
                door_bad++;
            }

            /* track spaces that cannot be protected */
            if ( (ag->take) ||
               ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL)) ||
               (ag->feat == FEAT_LESS) ||
               (ag->feat == FEAT_MORE) ||
               (ag->feat == FEAT_OPEN) ||
               (ag->feat == FEAT_BROKEN) ||
               (ag->kill))
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
        return (0);
    }

    /* See if he is in real danger or modest danger and summoners near */
    if ((p1 < avoidance*15/10) || (p1 < (avoidance*8/10) && borg_fighting_summoner))
        return (0);

    /* What effect is there? */
    /* this is a total guess */
    p2= p1 / 4;

    /* value is d */
    d = (p1-p2);

    if (p1 > p2 &&
           p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
           p1 > (avoidance/7))
    {
        /* Simulation */
        if (auto_simulate) return (d);

        /* Cast the spell */
        if (borg_prayer(2, 5) ||
            borg_spell(5,3))
            {
                return (d);
            }
     }
     return (0);
}

/* Word of Destruction, priest and mage spells.  Death is right around the
 *  corner, so kill everything.
 */
static int borg_defend_aux_destruction(void)
{
    int p1= 0;
    int p2 = 0;
    int d = 0;
    bool spell= FALSE;

    /* Obtain initial danger */
    p1= borg_danger(c_y,c_x,1);

    if (borg_prayer_okay_fail(8, 3, 55) ||
        borg_spell_okay_fail(8, 4, 55) ||
        borg_spell_okay_fail(3, 5, 55) ||
        borg_equips_staff_fail(SV_STAFF_DESTRUCTION))
        spell = TRUE;

	/* Borg_defend() is called before borg_escape().  He may have some
     * easy ways to escape (teleport scroll) but he may attempt this spell
     * instead of using the scrolls.
     */
    /* Use teleport scrolls instead of WoD */
    if (amt_escape && !do_blind && !do_confused) return (0);

    /* Obtain initial danger */
    p1= borg_danger(c_y,c_x,1);

    /* Special check for super danger--no fail check */
    if (p1 > (avoidance * 4) && borg_equips_staff_fail(SV_STAFF_DESTRUCTION))
        spell= TRUE;

    if (spell == FALSE) return (0);

    /* See if he is in real danger */
    if (p1 < avoidance * 3)
        return (0);

    /* What effect is there? */
    p2= 0;

    /* value is d */
    d = (p1-p2);

    /* Try not to use this on uniques */
    if (borg_fighting_unique && p1 < avoidance * 5) d = 0;

    /* Simulation */
    if (auto_simulate) return (d);

    /* Cast the spell */
    if (borg_prayer(8, 3) ||
        borg_spell(8,4) ||
        borg_spell(3,5) ||
        borg_use_staff(SV_STAFF_DESTRUCTION))
        {
            return (d);
        }

    /* oops it did not work */
    return (0);
}
static int borg_defend_aux_banishment( int p1)
{
    int p2= 0;
    int dam = 0;
    int fail_allowed = 25;

    /* Only tell away if scared */
    if ( p1 < avoidance * 3)
        return (0);

    /* if very scary, do not allow for much chance of fail */
    if ( p1 > avoidance * 3)
        fail_allowed -= 15;

    if (!borg_prayer_okay_fail(8, 2, fail_allowed))
        return (0);

    /* Value */
    dam = borg_launch_bolt(10, 1000, GF_AWAY_EVIL, MAX_RANGE);


	/* Normalize the Value */
    p2= (p1 - dam);
    if (p2 <=0 ) p2 =0;

    /* Try not to use them on uniques */
    if (p1 < avoidance * 4 && borg_fighting_unique ==2) p2=0;

    /* check to see if I am left better off */
    if (p1 > p2 &&
        p2 <= (borg_fighting_unique?((avoidance*2)/3): (avoidance/2)) &&
        p1 > (avoidance/8))
    {
        /* Simulation */
        if (auto_simulate) return (p2);

        /* Cast the spell */
        if (borg_prayer_fail(8, 2, fail_allowed))
        {
            /* Value */
            return (p2);
        }
    }
        return (0);
}



/*
 * Detect Inviso/Monsters
 * Used only if I am hit by an unseen guy.
 * Casts detect invis.
 */
static int borg_defend_aux_detect_inviso(p1)
{
    int fail_allowed = 25;
    /* no need */
    if (do_blind)
        return (0);

    /* not recent, dont bother */
    if (c_t > (need_see_inviso+5))
        return (0);

    /* Do I have anything that will work? */
    if (-1 == borg_slot(TV_POTION,SV_POTION_DETECT_INVIS)  &&
        -1 == borg_slot(TV_SCROLL,SV_SCROLL_DETECT_INVIS) &&
       (-1 == borg_slot(TV_STAFF, SV_STAFF_DETECT_INVIS) &&
             !auto_items[borg_slot(TV_STAFF,SV_STAFF_DETECT_INVIS)].pval) &&
       (-1 == borg_slot(TV_STAFF, SV_STAFF_DETECT_EVIL) &&
             !auto_items[borg_slot(TV_STAFF,SV_STAFF_DETECT_EVIL)].pval) &&
        !borg_prayer_okay_fail(2, 3, fail_allowed))
        return (0);

/*  not working correctly no value */
    /* No real value known, but lets cast it to find the bad guys. */
    if (auto_simulate) return (0);


    /* smoke em if you got em */
    if (!my_see_inv &&
       (borg_quaff_potion(SV_POTION_DETECT_INVIS) ||
        borg_read_scroll(SV_SCROLL_DETECT_INVIS) ||
        borg_use_staff(SV_STAFF_DETECT_INVIS) ||
        borg_prayer_fail(2, 3, fail_allowed) ||
        borg_use_staff(SV_STAFF_DETECT_EVIL)))
        {
            need_see_inviso = (c_t + 25);
            return (10);
        }

    /* ah crap, I guess I wont be able to see them */
    return (0);

}
/*
 * Light Beam to spot lurkers
 * Used only if I am hit by an unseen guy.
 * Lights up a hallway.
 */
static int borg_defend_aux_lbeam(p1)
{
    bool hallway = FALSE;
    int x=c_x;
    int y=c_y;


    /* no need */
    if (do_blind)
        return (0);

    /* Light Beam section to spot non seen guys */
        /* not recent, dont bother */
        if (c_t > (need_see_inviso+2))
            return (0);

        /* Check to see if I am in a hallway */
        /* Case 1a: north-south corridor */
        if (borg_cave_floor_bold(y-1, x) && borg_cave_floor_bold(y+1, x) &&
            !borg_cave_floor_bold(y, x-1) && !borg_cave_floor_bold(y, x+1) &&
            !borg_cave_floor_bold(y+1, x-1) && !borg_cave_floor_bold(y+1, x+1) &&
            !borg_cave_floor_bold(y-1, x-1) && !borg_cave_floor_bold(y-1, x+1))
        {
            /* ok to light up */
            hallway = TRUE;
        }

        /* Case 1b: east-west corridor */
        if (borg_cave_floor_bold(y, x-1) && borg_cave_floor_bold(y, x+1) &&
            !borg_cave_floor_bold(y-1, x) && !borg_cave_floor_bold(y+1, x) &&
            !borg_cave_floor_bold(y+1, x-1) && !borg_cave_floor_bold(y+1, x+1) &&
            !borg_cave_floor_bold(y-1, x-1) && !borg_cave_floor_bold(y-1, x+1))
        {
            /* ok to light up */
            hallway = TRUE;
        }

        /* Case 1aa: north-south doorway */
        if (borg_cave_floor_bold(y-1, x) && borg_cave_floor_bold(y+1, x) &&
            !borg_cave_floor_bold(y, x-1) && !borg_cave_floor_bold(y, x+1))
        {
            /* ok to light up */
            hallway = TRUE;
        }

        /* Case 1ba: east-west doorway */
        if (borg_cave_floor_bold(y, x-1) && borg_cave_floor_bold(y, x+1) &&
            !borg_cave_floor_bold(y-1, x) && !borg_cave_floor_bold(y+1, x))
        {
            /* ok to light up */
            hallway = TRUE;
        }

        /* not in a hallway */
        if (!hallway) return (0);

        /* if too dangerous, forget it */
        if (auto_simulate && p1 > avoidance *3/4) return (0);

        /* test the beam function */
        if (!borg_lite_beam(TRUE)) return (0);

        /* return some value */
        if (auto_simulate) return (10);

        /* if in a hallway call the Light Beam routine */
        if (borg_lite_beam(FALSE))
        {
            return (10);
        }
        return (0);
}

/* Shift the panel to locate offscreen monsters */
static int borg_defend_aux_panel_shift()
{
    int dir=0;

    /* no need */
    if (!need_shift_panel)
        return (0);

    /* Determine our current panel */
    /* Send action (view panel info) */
     borg_keypress('L');

    /* Which direction do we need to move? */
    /* Shift panel to the right */
    if (c_x >= 52 && c_x <= 60 && p_ptr->wx == 0) dir = 6;
    if (c_x >= 84 && c_x <= 94 && p_ptr->wx == 1) dir = 6;
    if (c_x >= 116 && c_x <= 123 && p_ptr->wx == 2) dir = 6;
    if (c_x >= 148 && c_x <= 159 && p_ptr->wx == 3) dir = 6;
    /* Shift panel to the left */
    if (c_x <= 142 && c_x >= 136 && p_ptr->wx == 4) dir = 4;
    if (c_x <= 110 && c_x >= 103 && p_ptr->wx == 3) dir = 4;
    if (c_x <= 78 && c_x >= 70 && p_ptr->wx == 2) dir = 4;
    if (c_x <= 46 && c_x >= 37 && p_ptr->wx == 1) dir = 4;

    if (dir) borg_keypress(I2D(dir));

    /* reset dir */
    dir = 0;

    /* Shift panel down */
    if (c_y >= 15 && c_y <= 19 && p_ptr->wy == 0) dir = 2;
    if (c_y >= 25 && c_y <= 30 && p_ptr->wy == 1) dir = 2;
    if (c_y >= 36 && c_y <= 41 && p_ptr->wy == 2) dir = 2;
    if (c_y >= 48 && c_y <= 52 && p_ptr->wy == 3) dir = 2;
    /* Shift panel up */
    if (c_y <= 51 && c_y >= 47 && p_ptr->wy == 4) dir = 8;
    if (c_y <= 39 && c_y >= 35 && p_ptr->wy == 3) dir = 8;
    if (c_y <= 28 && c_y >= 24 && p_ptr->wy == 2) dir = 8;
    if (c_y <= 17 && c_y >= 13 && p_ptr->wy == 1) dir = 8;

    if (dir) borg_keypress(I2D(dir));
    borg_keypress(ESCAPE);

    /* note it,  reset the flag */
    borg_note("# Shifted panel to locate offscreen monster.");
    need_shift_panel = FALSE;

    /* This uses no energy */
    return (0);
}

/*
 * Simulate/Apply the optimal result of using the given "type" of defence
 * p1 is the current danger level (passed in for effiency)
 */
static int borg_defend_aux(int what, int p1)
{

    /* Analyze */
    switch (what)
    {
        case BD_SPEED:
        {
            return (borg_defend_aux_speed(p1));
        }

        case BD_PROT_FROM_EVIL:
        {
            return (borg_defend_aux_prot_evil(p1));
        }
        case BD_ARMOR_OF_LIGHT:
        {
            return (borg_defend_aux_armor_of_light(p1));
        }
        case BD_ETHEREAL_LOCK:
        {
            return (borg_defend_aux_ethereal_lock(p1));
        }
        case BD_RESIST1:
        {
            return (borg_defend_aux_resist1(p1));
        }
        case BD_RESIST2:
        {
            return (borg_defend_aux_resist2(p1));
        }
        case BD_RESIST3:
        {
            return (borg_defend_aux_resist3(p1));
        }
        case BD_RESIST4:
        {
            return (borg_defend_aux_resist4(p1));
        }
        case BD_RESIST5:
        {
            return (borg_defend_aux_resist5(p1));
        }
        case BD_RESIST6:
        {
            return (borg_defend_aux_resist6(p1));
        }
        case BD_BLESS:
        {
            return (borg_defend_aux_bless(p1));
        }

        case BD_HERO:
        {
          return (borg_defend_aux_hero(p1));
        }
        case BD_SHIELD:
        {
            return (borg_defend_aux_shield(p1));
        }
        case BD_GOI:
        {
            return (borg_defend_aux_goi(p1));
        }

        case BD_TELL_AWAY:
        {
            return (borg_defend_aux_tell_away(p1));
        }
        case BD_GLYPH:
        {
            return (borg_defend_aux_glyph(p1));
        }
        case BD_CREATE_DOOR_WALL:
        {
            return (borg_defend_aux_create_door_wall(p1));
        }
        case BD_CREATE_DOORS:
        {
            return (borg_defend_aux_create_doors(p1));
        }
        case BD_EARTHQUAKE:
        {
            return (borg_defend_aux_earthquake());
        }
        case BD_DESTRUCTION:
        {
            return (borg_defend_aux_destruction());
        }
        case BD_BANISHMENT:
        {
            return (borg_defend_aux_banishment(p1));
        }
        case BD_SCATTER_FOE:
        {
            return (borg_defend_aux_scatter_foe());
        }
        case BD_INVISO:
        {
            return (borg_defend_aux_inviso(p1));
        }
        case BD_DETECT_INVISO:
        {
            return (borg_defend_aux_detect_inviso(p1));
        }
        case BD_LIGHT_BEAM:
        {
            return (borg_defend_aux_lbeam());
        }
        case BD_SHIFT_PANEL:
        {
            return (borg_defend_aux_panel_shift());
        }
    }
    return (0);
}

/*
 * prepare to attack... this is setup for a battle.
 */
bool borg_defend(int p1)
{
    int n, b_n = 0;
    int g, b_g = -1;

    /* Simulate */
    auto_simulate = TRUE;

    /* if you have a globe up and it is about to drop, */
    /* refresh it (if you can) */
    if (borg_goi && borg_goi < 2)
    {
        int p;

        /* check 'true' danger. This will make sure we do not */
        /* refresh our GOI if no-one is around */
        borg_attacking = TRUE;
        p = p1;
        borg_attacking = FALSE;
        if (p > auto_fear_region[c_y/11][c_x/11])
        {
            if (borg_spell(7, 5))
            {
                borg_note("# refreshing GOI ");

                borg_goi += 10;
                return (TRUE);
            }
        }
    }

    /* Analyze the possible setup moves */
    for (g = 0; g < BD_MAX; g++)
    {
        /* Simulate */
        n = borg_defend_aux(g, p1);

        /* Track "best" attack */
        if (n <= b_n) continue;

        /* Track best */
        b_g = g;
        b_n = n;
    }

    /* Nothing good */
    if (b_n <= 0)
    {
        return (FALSE);
    }

    /* Note */
    borg_note(format("# Performing defence type %d with value %d", b_g, b_n));

    /* Instantiate */
    auto_simulate = FALSE;

    /* Instantiate */
    (void)borg_defend_aux(b_g, p1);
    /* Success */
    return (TRUE);

}
/*
 * Perma spells.  Some are cool to have on all the time, so long as their
 * mana cost is not too much.
 * There are several types of setup moves:
 *
 *   Temporary speed
 *   Protect From Evil
 *   Prayer
 *   Temp Resist (either all or just cold/fire?)
 *   Shield
 *
 */
enum
{
    BP_SPEED,
    BP_PROT_FROM_EVIL,
    BP_BLESS,
    BP_HERO,
    BP_RESIST_ALL,
    BP_RESIST_ALL_COLLUIN,
    BP_RESIST_F,
    BP_RESIST_C,
    BP_RESIST_AE,
    BP_RESIST_P,

    BP_SHIELD,

    BP_MAX
};

/*
 * Bless/Prayer to prepare for battle
 */
static int borg_perma_aux_bless()
{
    int fail_allowed = 5, cost;

    auto_magic *as;

    /* already blessed */
    if (borg_bless)
        return (0);

	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;


    /* Cant when Blind */
    if (do_blind || do_confused) return (0);


    if ( !borg_prayer_okay_fail(3, 1, fail_allowed))
        return (0);

    /* XXX Dark */

    /* Obtain the cost of the spell */
    as = &auto_magics[3][1];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= (unique_on_level ? auto_csp /10 : auto_csp /20)) return (0);

    /* Simulation */
    /* bless is a low priority */
    if (auto_simulate) return (1);

    /* do it! */
    if (borg_prayer(3,1))
          return 1;


    return (0);
}
/*
 * Hero/Berserk to prepare for battle
 */
static int borg_perma_aux_hero()
{
    int fail_allowed = 5, cost;

    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    /* already blessed */
    if (borg_hero || borg_berserk)
        return (0);

    /* Cant when Blind */
    if (do_blind || do_confused) return (0);


    if ( !borg_spell_okay_fail(7, 0, fail_allowed))
        return (0);

    /* XXX Dark */

    /* Obtain the cost of the spell */
    as = &auto_magics[7][0];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= (unique_on_level ? auto_csp /10 : auto_csp /20)) return (0);

    /* Simulation */
    /* bless is a low priority */
    if (auto_simulate) return (1);

    /* do it! */
    if (borg_spell(7,0))
          return 1;


    return (0);
}
/* all resists */
static int borg_perma_aux_resist()
{
    int cost = 0;
    int fail_allowed = 5;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    if (my_oppose_fire &&
        my_oppose_acid &&
        my_oppose_pois &&
        my_oppose_elec &&
        my_oppose_cold)
        return (0);

    if (!borg_spell_okay_fail(4, 5, fail_allowed))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[4][5];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= (unique_on_level ? auto_csp /10 : auto_csp /20)) return (0);

    /* Simulation */
    if (auto_simulate) return (2);

    /* do it! */
    if (borg_spell_fail(4, 5, fail_allowed) )

        /* Value */
        return (2);


    /* default to can't do it. */
    return (0);
}

/* all resists from the cloak */
static int borg_perma_aux_resist_colluin()
{
    if (my_oppose_fire &&
        my_oppose_acid &&
        my_oppose_pois &&
        my_oppose_elec &&
        my_oppose_cold)
        return (0);

	/* use it when unique is near */
	if (!borg_fighting_unique) return (0);

    if (!borg_equips_artifact(ART_COLLUIN, INVEN_OUTER))
        return (0);

    /* Simulation */
    if (auto_simulate) return (2);

    /* do it! */
    if (borg_activate_artifact(ART_COLLUIN, INVEN_OUTER) )

        /* Value */
        return (2);


    /* default to can't do it. */
    return (0);
}


/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_f()
{
    int cost = 0;
    int fail_allowed = 5;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    if (my_oppose_fire || my_resist_fire >=2 || !unique_on_level)
        return (0);

    if (!borg_spell_okay_fail(4, 0, fail_allowed))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[4][0];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= auto_csp /20) return (0);

    /* Simulation */
    if (auto_simulate) return (1);

    /* do it! */
    if (borg_spell_fail(4, 0, fail_allowed) )

    {

        /* Value */
        return (1);
	}

    /* default to can't do it. */
    return (0);
}
/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_c()
{
    int cost = 0;
    int fail_allowed = 5;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    if (my_oppose_cold || my_resist_cold >=2 || !unique_on_level)
        return (0);

    if (!borg_spell_okay_fail(4, 1, fail_allowed))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[4][1];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= auto_csp /20) return (0);

    /* Simulation */
    if (auto_simulate) return (1);

    /* do it! */
    if (borg_spell_fail(4, 1, fail_allowed) )

        /* Value */
        return (1);


    /* default to can't do it. */
    return (0);
}

/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_ae()
{
    int cost = 0;
    int fail_allowed = 5;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    if ((my_oppose_elec && my_oppose_acid) || (my_resist_elec >=2 && my_resist_acid) || !unique_on_level)
        return (0);

    if (!borg_spell_okay_fail(4, 2, fail_allowed))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[4][2];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= auto_csp /20) return (0);

    /* Simulation */
    if (auto_simulate) return (1);

    /* do it! */
    if (borg_spell_fail(4, 2, fail_allowed) )

        /* Value */
        return (1);


    /* default to can't do it. */
    return (0);
}

/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_p()
{
    int cost = 0;
    int fail_allowed = 5;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    if (my_oppose_pois || my_resist_pois >=2 || !unique_on_level)
        return (0);

    if (!borg_spell_okay_fail(4, 3, fail_allowed))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[4][3];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= auto_csp /20) return (0);

    /* Simulation */
    if (auto_simulate) return (1);

    /* do it! */
    if (borg_spell_fail(4, 3, fail_allowed) )

        /* Value */
        return (1);


    /* default to can't do it. */
    return (0);
}


/*
 * Speed to prepare for battle
 */
static int borg_perma_aux_speed()
{
    int fail_allowed = 7;
    int cost;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    /* already fast */
    if (borg_speed)
        return (0);

    /* only cast defence spells if fail rate is not too high */
    if ( !borg_spell_okay_fail( 3, 3, fail_allowed ))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[3][3];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= auto_csp /20) return (0);

    /* Simulation */
    if (auto_simulate) return (5);

    /* do it! */
    if (borg_spell_fail( 3, 3, fail_allowed))
        return (5);

    /* default to can't do it. */
    return (0);
}
static int borg_perma_aux_shield()
{
    int fail_allowed = 3;
    int cost;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    /* if already protected */
    if (borg_shield || borg_goi)
        return (0);

    if (!borg_spell_okay_fail(7, 2, fail_allowed))
        return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[7][2];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= (unique_on_level ? auto_csp /10 : auto_csp /20)) return (0);

    /* Simulation */
    if (auto_simulate) return (2);

    /* do it! */
    borg_spell_fail(7, 2, fail_allowed);
        return (2);

    /* default to can't do it. */
    return (0);
}
static int borg_perma_aux_prot_evil()
{
    int cost = 0;
    int fail_allowed = 5;
    auto_magic *as;


	/* increase the threshold */
	if (unique_on_level) fail_allowed = 10;
	if (borg_fighting_unique) fail_allowed = 15;

    /* if already protected */
    if (borg_prot_from_evil)
        return (0);

    if (!borg_prayer_okay_fail(2,4,fail_allowed)) return (0);

    /* Obtain the cost of the spell */
    as = &auto_magics[2][4];
    cost = as->power;

    /* If its cheap, go ahead */
    if (cost >= (unique_on_level ? auto_csp /10 : auto_csp /20)) return (0);

    /* Simulation */
    if (auto_simulate) return (3);

    /* do it! */
    if (borg_prayer_fail(2, 4, fail_allowed))

        /* Value */
        return (3);

    /* default to can't do it. */
    return (0);
}

/*
 * Simulate/Apply the optimal result of using the given "type" of set-up
 */
static int borg_perma_aux(int what)
{

    /* Analyze */
    switch (what)
    {
        case BP_SPEED:
        {
            return (borg_perma_aux_speed());
        }

        case BP_PROT_FROM_EVIL:
        {
            return (borg_perma_aux_prot_evil());
        }
        case BP_RESIST_ALL:
        {
            return (borg_perma_aux_resist());
        }
        case BP_RESIST_ALL_COLLUIN:
        {
            return (borg_perma_aux_resist_colluin());
        }
        case BP_RESIST_F:
        {
            return (borg_perma_aux_resist_f());
        }
        case BP_RESIST_C:
        {
            return (borg_perma_aux_resist_c());
        }
        case BP_RESIST_AE:
        {
            return (borg_perma_aux_resist_ae());
        }
        case BP_RESIST_P:
        {
            return (borg_perma_aux_resist_p());
        }
        case BP_BLESS:
        {
            return (borg_perma_aux_bless());
        }
        case BP_HERO:
        {
            return (borg_perma_aux_hero());
        }
        case BP_SHIELD:
        {
            return (borg_perma_aux_shield());
        }
    }
    return (0);
}

/*
 * prepare to attack... this is setup for a battle.
 */
bool borg_perma_spell()
{
    int n, b_n = 0;
    int g, b_g = -1;

    /* Simulate */
    auto_simulate = TRUE;

    /* Not in town */
    if (!auto_depth) return (FALSE);

    /* Analyze the possible setup moves */
    for (g = 0; g < BP_MAX; g++)
    {
        /* Simulate */
        n = borg_perma_aux(g);

        /* Track "best" move */
        if (n <= b_n) continue;

        /* Track best */
        b_g = g;
        b_n = n;
    }

    /* Nothing good */
    if (b_n <= 0)
    {
        return (FALSE);
    }

    /* Note */
    borg_note(format("# Performing perma-spell type %d with value %d", b_g, b_n));

    /* Instantiate */
    auto_simulate = FALSE;

    /* Instantiate */
    (void)borg_perma_aux(b_g);
    /* Success */
    return (TRUE);

}
/*
 * check to make sure there are no monsters around
 * that should prevent resting
 */
bool borg_check_rest(void)
{
    int i;
    int o_y, o_x;
    int walls = 0;
    int x,y;

    auto_grid *ag;

    /* if I am surrounded on all sides, rest here */
    for (o_x = -1; o_x <= 1; o_x++)
    {
        for (o_y = -1; o_y <= 1; o_y++)
        {
            /* Acquire location */
            x = c_x + o_x;
            y = c_y + o_y;

            ag = &auto_grids[y][x];

            if (ag->feat >= FEAT_MAGMA &&
                ag->feat <= FEAT_PERM_SOLID) walls++;
            if ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_TAIL)) walls++;
        }
    }
    /* Is there protection all around? */
    if (walls >=8) return TRUE;

    /* Examine all the monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        auto_kill *kill = &auto_kills[i];
        monster_race *r_ptr = &r_info[kill->r_idx];

        int x9 = kill->x;
        int y9 = kill->y;
        int ax, ay, d;

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Distance components */
        ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
        ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

        /* Distance */
        d = MAX(ax, ay);

        /* Minimal distance */
        if (d > 16) continue;

        /* Minimal distance */
        if (d < 3) return (FALSE);

        /* Real scary guys pretty close */
        if (d < 13 && (borg_danger_aux(y9, x9, 1, i) > avoidance) &&
            avoidance < auto_mhp/2) return (FALSE);
        if (d < 8 && (borg_danger_aux(y9, x9, 1, i) > avoidance/2)) return (FALSE);
        if (d < 5 && (borg_danger_aux(y9, x9, 1, i) > avoidance/3)) return (FALSE);

        /* should check LOS... monster to me */
        if (borg_projectable(y9,x9, c_y,c_x)) return FALSE;

        /* should check LOS... me to monster */
        if (borg_projectable( c_y,c_x,y9,x9)) return FALSE;

        /* if absorbs mana, not safe */
        if ((r_ptr->flags5 & RF5_DRAIN_MANA) && (auto_msp > 1)) return FALSE;

        /* if breeder, not safe */
        if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;

        /* if it walks through walls, not safe */
        if (r_ptr->flags2 & RF2_PASS_WALL) return FALSE;
        if (r_ptr->flags2 & RF2_KILL_WALL) return FALSE;

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
    int p = 0;
    int q, i;

    /*** Handle annoying situations ***/

    /* Refuel current torch */
    if ((auto_items[INVEN_LITE].tval == TV_LITE) &&
        (auto_items[INVEN_LITE].sval == SV_LITE_TORCH))
    {
        /* Refuel the torch if needed */
        if (auto_items[INVEN_LITE].pval < 2500)
        {
            if (borg_refuel_torch()) return (TRUE);

            /* Take note */
            borg_note(format("# Need to refuel but cant!", p));
        }
    }

    /* Refuel current lantern */
    if ((auto_items[INVEN_LITE].tval == TV_LITE) &&
        (auto_items[INVEN_LITE].sval == SV_LITE_LANTERN))
    {
        /* Refuel the lantern if needed */
        if (auto_items[INVEN_LITE].pval < 5000)
        {
            if (borg_refuel_lantern()) return (TRUE);

            /* Take note */
            borg_note(format("# Need to refuel but cant!", p));
        }
    }


    /*** Do not recover when in danger ***/

    /* Look around for danger */
    p = borg_danger(c_y, c_x, 1);

    /* Never recover in dangerous situations */
    if (p > avoidance / 4) return (FALSE);


    /*** Roll for "paranoia" ***/

    /* Base roll */
    q = rand_int(100);

    /* Half dead */
    if (auto_chp < auto_mhp / 2) q = q - 10;

    /* Almost dead */
    if (auto_chp < auto_mhp / 4) q = q - 10;


    /*** Use "cheap" cures ***/

    /* Hack -- cure stun */
    if (do_stun && (q < 75))
    {
        if (borg_activate_artifact(ART_LOTHARANG, INVEN_WIELD) ||
            borg_prayer(2, 7) ||
            borg_prayer(3, 4) ||
            borg_prayer(6, 1) ||
            borg_prayer(6, 2))

        {
            /* Take note */
            borg_note(format("# Cure Stun", p));

            return (TRUE);
        }
    }

    /* Hack -- cure stun */
    if (do_heavy_stun)
    {
        if (borg_activate_artifact(ART_LOTHARANG, INVEN_WIELD) ||
            borg_prayer(2, 7) ||
            borg_prayer(3, 4) ||
            borg_prayer(6, 1) ||
            borg_prayer(6, 2))
        {
            /* Take note */
            borg_note(format("# Cure Heavy Stun", p));

            return (TRUE);
        }
    }

    /* Hack -- cure cuts */
    if (do_cut && (q < 75))
    {
        if (borg_activate_artifact(ART_LOTHARANG, INVEN_WIELD) ||
            borg_prayer(2, 2) ||
            borg_prayer(2, 7) ||
            borg_prayer(3, 4) ||
            borg_prayer(6, 0) ||
            borg_prayer(6, 1) ||
            borg_prayer(6, 2))
        {
            /* Take note */
            borg_note(format("# Cure Cuts", p));

            return (TRUE);
        }
    }

    /* Hack -- cure poison */
    if (do_poisoned && (q < 75))
    {
        if (borg_activate_artifact(ART_DAL, INVEN_FEET) ||
            borg_spell(4, 4) ||
            borg_prayer(3, 0))
        {
            /* Take note */
            borg_note(format("# Cure poison", p));

            return (TRUE);
        }
    }

    /* Hack -- cure fear */
    if (do_afraid && (q < 75))
    {
        if (borg_activate_artifact(ART_DAL, INVEN_FEET) ||
            borg_spell(7, 0) ||
            borg_prayer(0, 3))
        {
            /* Take note */
            borg_note(format("# Cure fear", p));

            return (TRUE);
        }
    }

    /* Hack -- satisfy hunger */
    if ((do_hungry || do_weak) && (q < 75))
    {
        if (borg_spell(2, 7) ||
            borg_prayer(2, 0))
        {
            return (TRUE);
        }
    }

    /* Hack -- heal damage */
    if ((auto_chp < auto_mhp / 2) && (q < 75) && p == 0 &&
        (auto_csp > auto_msp /4))
    {
        if (borg_activate_artifact(ART_SOULKEEPER, INVEN_BODY) ||
            borg_prayer(3, 4) ||
            borg_prayer(6, 2) ||
            borg_prayer(2, 7) ||
            borg_prayer(6, 1) )
        {
            /* Take note */
            borg_note(format("# heal damage (recovering)"));

            return (TRUE);
        }
    }

    /* cure experience loss with prayer */
    if (do_fix_exp && (borg_activate_artifact(ART_LUTHIEN, INVEN_OUTER) ||
            borg_prayer(6, 5)) )
    {
        return (TRUE);
    }

    /* cure stat drain with prayer */
    for (i = 0; i < 6; i++)
    {
        if (do_fix_stat[i] && borg_prayer(6, 4))
        {
            return (TRUE);
        }
    }

    /*** Use "expensive" cures ***/

    /* Hack -- cure stun */
    if (do_stun && (q < 25))
    {
        if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_use_staff(SV_STAFF_CURING) ||
            borg_zap_rod(SV_ROD_CURING) ||
            borg_zap_rod(SV_ROD_HEALING) ||
            borg_activate_artifact(ART_SOULKEEPER, INVEN_BODY) ||
            borg_activate_artifact(ART_GONDOR, INVEN_HEAD))
        {
            return (TRUE);
        }
    }

    /* Hack -- cure heavy stun */
    if (do_heavy_stun && (q < 95))
    {
        if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_use_staff(SV_STAFF_CURING) ||
            borg_zap_rod(SV_ROD_CURING) ||
            borg_zap_rod(SV_ROD_HEALING) ||
            borg_activate_artifact(ART_SOULKEEPER, INVEN_BODY) ||
            borg_activate_artifact(ART_GONDOR, INVEN_HEAD))
        {
            return (TRUE);
        }
    }

    /* Hack -- cure cuts */
    if (do_cut && (q < 25))
    {
        if (borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_use_staff(SV_STAFF_CURING) ||
            borg_zap_rod(SV_ROD_CURING) ||
            borg_zap_rod(SV_ROD_HEALING) ||
            borg_activate_artifact(ART_SOULKEEPER, INVEN_BODY) ||
            borg_activate_artifact(ART_GONDOR, INVEN_HEAD))
        {
                return (TRUE);
        }
    }

    /* Hack -- cure poison */
    if (do_poisoned && (q < 25))
    {
        if (borg_quaff_potion(SV_POTION_CURE_POISON) ||
            borg_eat_food(SV_FOOD_CURE_POISON) ||
            borg_use_staff(SV_STAFF_CURING) ||
            borg_zap_rod(SV_ROD_CURING) ||
            borg_quaff_potion(SV_POTION_SLOW_POISON) ||
            borg_eat_food(SV_FOOD_WAYBREAD) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_activate_artifact(ART_DAL, INVEN_FEET))
        {
            borg_note("# Cure Poison.");
            return (TRUE);
        }
    }

    /* Hack -- cure blindness */
    if (do_blind && (q < 25))
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
    if (do_confused && (q < 25))
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
    if (do_afraid && (q < 25))
    {
        if (borg_eat_food(SV_FOOD_CURE_PARANOIA) ||
            borg_quaff_potion(SV_POTION_BOLDNESS) ||
            borg_quaff_potion(SV_POTION_HEROISM) ||
            borg_quaff_potion(SV_POTION_BESERK_STRENGTH) ||
            borg_activate_artifact(ART_DAL, INVEN_FEET))
        {
            return (TRUE);
        }
    }

    /* Hack -- satisfy hunger */
    if ((do_hungry || do_weak) && (q < 25))
    {
        if (borg_read_scroll(SV_SCROLL_SATISFY_HUNGER))
        {
            return (TRUE);
        }
    }

    /* Hack -- heal damage */
    if ((auto_chp < auto_mhp / 2) && (q < 25))
    {
        if (borg_zap_rod(SV_ROD_HEALING) ||
            borg_quaff_potion(SV_POTION_CURE_CRITICAL) ||
            borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
            borg_activate_artifact(ART_LOTHARANG, INVEN_WIELD))
        {
            return (TRUE);
        }
    }

    /* Hack -- Allow Rod of Healing to recharge */
    if (amt_rod_heal)
    {
        /* Step 1.  Recharge when I have only 1 rod. */
        if (!auto_items[borg_slot(TV_ROD, SV_ROD_HEALING)].pval)
        {
            /* Rest, when safe, until at least one recharges */
            if (!do_weak && !do_cut && !do_hungry && !do_poisoned &&
                borg_check_rest() && !borg_spell_okay(6,3))
            {
                /* Take note */
                borg_note("# Resting to recharge a rod...");

                /*  Reset the Bouncing-borg timer */
                time_this_panel = 0;

                /* Rest until done */
                borg_keypress('R');
                borg_keypress('1');
                borg_keypress('0');
                borg_keypress('0');
                borg_keypress('\n');

                /* Done */
                return (TRUE);
            }
        }

        /* Step 2.  Recharge all healing rods. */
        if (auto_items[borg_slot(TV_ROD, SV_ROD_HEALING)].pval !=
            auto_items[borg_slot(TV_ROD, SV_ROD_HEALING)].iqty)
        {
            /* Rest until all recharge */
            if (!do_weak && !do_cut && !do_hungry && !do_poisoned &&
                borg_check_rest() && auto_depth == 0)
            {
                /* Take note */
                borg_note("# Resting to recharge all rods...");

                /*  Reset the Bouncing-borg timer */
                time_this_panel = 0;

                /* Rest until done */
                borg_keypress('R');
                borg_keypress('1');
                borg_keypress('0');
                borg_keypress('0');
                borg_keypress('\n');

                /* Done */
                return (TRUE);
            }
        }
    }

    /*** Just Rest ***/

    /* Hack -- rest until healed */
    if ((do_blind || do_confused || do_image ||
         do_afraid || do_stun || do_heavy_stun ||
         (auto_chp < auto_mhp) || (auto_csp < auto_msp)) &&
         (!auto_takes_cnt || !goal_recalling) && !borg_goi && !borg_shield &&
        (rand_int(100) < 90) && borg_check_rest( ) &&
        p <= auto_fear_region[c_y/11][c_x/11])
    {
        /* XXX XXX XXX */
        if (!do_weak && !do_cut && !do_hungry && !do_poisoned)
        {
            /* Take note */
            borg_note(format("# Resting (danger %d)...", p));

            /* Rest until done */
            borg_keypress('R');
            borg_keypress('&');
            borg_keypress('\n');

            /* Done */
            return (TRUE);
        }
    }


    /* Nope */
    return (FALSE);
}


/*
 * Take one "step" towards the given location, return TRUE if possible
 */
static bool borg_play_step(int y2, int x2)
{
    auto_grid *ag;

    int dir, x, y, ox, oy, i;

    int o_y=0, o_x=0, door_found = 0;

    /* Breeder levels, close all doors */
    if (breeder_level)
    {
        /* scan the adjacent grids */
        for (ox = -1; ox <= 1; ox++)
        {
                for (oy = -1; oy <= 1; oy++)
                {
                    /* skip our own spot */
                    if ((oy+c_y == c_y) && (ox+c_x == c_x)) continue;

                    /* skip our orignal goal */
                    if ((oy+c_y == y2) && (ox+c_x == x2)) continue;

                    /* Acquire location */
                    ag = &auto_grids[oy+c_y][ox+c_x];

                    /* skip non open doors */
                    if (ag->feat != FEAT_OPEN) continue;

                    /* skip monster on door */
                    if (ag->kill) continue;

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
            dir = borg_goto_dir(c_y, c_x, c_y+o_y, c_x+o_x);

            /* Obtain the destination */
            x = c_x + ddx[dir];
            y = c_y + ddy[dir];

            /* Hack -- set goal */
            g_x = x;
            g_y = y;

            /* Close */
            borg_note("# Closing a door");
            borg_keypress('c');
            borg_keypress(I2D(dir));

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
    dir = borg_goto_dir(c_y, c_x, y2, x2);

    /* We have arrived */
    if (dir == 5) return (FALSE);


    /* Obtain the destination */
    x = c_x + ddx[dir];
    y = c_y + ddy[dir];

    /* Access the grid we are stepping on */
    ag = &auto_grids[y][x];

    /* Hack -- set goal */
    g_x = x;
    g_y = y;


    /* Monsters -- Attack */
    if (ag->kill)
    {
        auto_kill *kill = &auto_kills[ag->kill];

        /* can't attack someone if afraid! */
        if (do_afraid)
            return (FALSE);

        /* Message */
        borg_note(format("# Walking into a '%s' at (%d,%d)",
                         r_name + r_info[kill->r_idx].name,
                         kill->x, kill->y));

        /* Walk into it */
        if (my_no_alter)
        {
            borg_keypress(';');
            my_no_alter = FALSE;
        }
        else
        {
            borg_keypress('+');
        }
        borg_keypress(I2D(dir));
        return (TRUE);
    }


    /* Objects -- Take */
    if (ag->take)
    {
        auto_take *take = &auto_takes[ag->take];

        /* Message */
        borg_note(format("# Walking onto a '%s' at (%d,%d)",
                         k_name + k_info[take->k_idx].name,
                         take->x, take->y));

        /* Walk onto it */
        borg_keypress(I2D(dir));
        return (TRUE);
    }

    /* Glyph of Warding */
    if (ag->feat == FEAT_GLYPH)
    {
        /* Message */
        borg_note(format("# Walking onto a glyph of warding."));

        /* Walk onto it */
        borg_keypress(I2D(dir));
        return (TRUE);
    }


    /* Traps -- disarm */
    if ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL))
    {
        /* not when blind or confused */
        if (!my_cur_lite || do_blind || do_confused) return (FALSE);

        /* Mega-Hack -- allow "destroy doors" */
        if (borg_prayer(7, 0))
        {
            borg_note("# Unbarring ways");
            return (TRUE);
        }
        /* Disarm */
        borg_note("# Disarming a trap");
        borg_keypress('D');
        borg_keypress(I2D(dir));
        return (TRUE);
    }


    /* Closed Doors -- Open */
    if ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_HEAD + 0x07))
    {
        /* Paranoia XXX XXX XXX */
        if (!rand_int(100)) return (FALSE);

        /* Open */
        if (my_need_alter)
        {
            borg_keypress('+');
            my_need_alter = FALSE;
        }
        else
        {

            borg_note("# Opening a door");
            borg_keypress('0');
            borg_keypress('9');
            borg_keypress('o');
        }
        borg_keypress(I2D(dir));
        return (TRUE);
    }



    /* Jammed Doors -- Bash or destroy */
    if ((ag->feat >= FEAT_DOOR_HEAD + 0x08) && (ag->feat <= FEAT_DOOR_TAIL))
    {
        /* Paranoia XXX XXX XXX */
        if (!rand_int(100)) return (FALSE);

        /* Mega-Hack -- allow "destroy doors" */
        if (borg_prayer(7, 0))
        {
            borg_note("# Unbarring ways");
            return (TRUE);
        }

#if 0 /* this is a bug in drangband */
        /* Mega-Hack -- allow "destroy doors" */
        if (borg_spell(1, 1))
        {
            borg_note("# Destroying doors");
            borg_keypress(I2D(dir));
            return (TRUE);
        }
#endif
        /* Mega-Hack -- allow "stone to mud" */
        if (borg_spell(7, 3) ||
            borg_activate_artifact(ART_OROME, INVEN_WIELD))
        {
            borg_note("# Melting a door");
            borg_keypress(I2D(dir));
            return (TRUE);
        }

        /* Bash */
        borg_note("# Bashing a door");
        borg_keypress('B');
        borg_keypress(I2D(dir));
        return (TRUE);
    }

    /* Rubble, Treasure, Seams, Walls -- Tunnel or Melt */
    if (ag->feat >= FEAT_SECRET)
    {
        /* Mega-Hack -- prevent infinite loops */
        if (rand_int(100) < 10) return (FALSE);

        /* Mega-Hack -- allow "stone to mud" */
        if (borg_spell(7, 3) ||
            borg_activate_artifact(ART_OROME, INVEN_WIELD))
        {
            borg_note("# Melting a wall/etc");
            borg_keypress(I2D(dir));
            return (TRUE);
        }

		/* Walk into it */
        if (my_no_alter)
        {
            borg_keypress(';');
	        borg_keypress(I2D(dir));
            my_no_alter = FALSE;
	        return (TRUE);
        }
        else
		{
    	    /* Tunnel */
    	    borg_note("# Digging through wall/etc");
    	    borg_keypress('0');
    	    borg_keypress('9');
    	    borg_keypress('9');
    	    borg_keypress('T');
    	    borg_keypress(I2D(dir));
    	    return (TRUE);
		}
    }


    /* Shops -- Enter */
    if ((ag->feat >= FEAT_SHOP_HEAD) && (ag->feat <= FEAT_SHOP_TAIL))
    {
        /* Message */
        borg_note(format("# Entering a '%d' shop", (ag->feat - FEAT_SHOP_HEAD) + 1));

        /* Enter the shop */
        borg_keypress(I2D(dir));
        return (TRUE);
    }


    /* Walk in that direction */
    if (my_need_alter)
    {
        borg_keypress('+');
        my_need_alter = FALSE;
    }
    borg_keypress(I2D(dir));


    /* Did something */
    return (TRUE);
}




/*
 * Act twitchy
 */
bool borg_twitchy(void)
{
    int dir;

    /* This is a bad thing */
    borg_note("# Twitchy!");

    /* try to phase out of it */
    if (amt_phase && borg_caution_phase(TRUE) &&
       (borg_spell_fail(1, 4, 40) ||
        borg_prayer_fail(4, 0, 40) ||
        borg_activate_artifact(ART_BELEGENNON, INVEN_BODY)||
        borg_activate_artifact(ART_COLANNON,INVEN_OUTER) ||
        borg_read_scroll(SV_SCROLL_PHASE_DOOR) ))
    {
        /* We did something */
        return (TRUE);
    }
    /* Pick a random direction */
    dir = randint(9);

    /* Hack -- set goal */
    g_x = c_x + ddx[dir];
    g_y = c_y + ddy[dir];

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

		/* Send direction */
		borg_keypress(I2D(dir));
	}

    /* We did something */
    return (TRUE);
}





/*
 * Commit the current "flow"
 */
static bool borg_flow_commit(cptr who, int why)
{
    int cost;

    /* Cost of current grid */
    cost = auto_data_cost->data[c_y][c_x];

    /* Verify the total "cost" */
    if (cost >= 250) return (FALSE);

    /* Message */
    if (who) borg_note(format("# Flowing toward %s at cost %d", who, cost));

    /* Obtain the "flow" information */
    COPY(auto_data_flow, auto_data_cost, auto_data);

    /* Save the goal type */
    goal = why;

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
 * blocks motion, and removes that grid from any flow in progress.
 *
 * When given multiple alternative steps, this function attempts to choose
 * the "safest" path, by penalizing grids containing embedded gold, monsters,
 * rubble, doors, traps, store doors, and even floors.  This allows the Borg
 * to "step around" dangerous grids, even if this means extending the path by
 * a step or two, and encourages him to prefer grids such as objects and stairs
 * which are not only interesting but which are known not to be invisible traps.
 *
 * XXX XXX XXX XXX This function needs some work.  It should attempt to
 * analyze the "local region" around the player and determine the optimal
 * choice of locations based on some useful computations.
 *
 * If it works, return TRUE, otherwise, cancel the goal and return FALSE.
 */
bool borg_flow_old(int why)
{
    int x, y;

    auto_grid *ag;


    /* Continue */
    if (goal == why)
    {
        int b_n = 0;

        int i, b_i = -1;

        int c, b_c;


        /* Flow cost of current grid */
        b_c = auto_data_flow->data[c_y][c_x] * 10;

        /* Prevent loops */
        b_c = b_c - 5;


        /* Look around */
        for (i = 0; i < 8; i++)
        {
            /* Grid in that direction */
            x = c_x + ddx_ddd[i];
            y = c_y + ddy_ddd[i];

            /* Access the grid */
            ag = &auto_grids[y][x];

            /* Flow cost at that grid */
            c = auto_data_flow->data[y][x] * 10;

            /* Never backtrack */
            if (c > b_c) continue;


            /* Notice new best value */
            if (c < b_c) b_n = 0;

            /* Apply the randomizer to equivalent values */
            if ((++b_n >= 2) && (rand_int(b_n) != 0)) continue;

            /* Track it */
            b_i = i; b_c = c;
        }

        /* Try it */
        if (b_i >= 0)
        {
            /* Access the location */
            x = c_x + ddx_ddd[b_i];
            y = c_y + ddy_ddd[b_i];

            /* Attempt motion */
            if (borg_play_step(y, x)) return (TRUE);
        }

        /* Cancel goal */
        goal = 0;
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
    if (!track_less_num && !track_more_num) return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < track_less_num; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(track_less_y[i], track_less_x[i]);
    }

    /* Enqueue useful grids */
    for (i = 0; i < track_more_num; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(track_more_y[i], track_more_x[i]);
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
    if (!track_less_num) return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < track_less_num; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(track_less_y[i], track_less_x[i]);
    }

    if (auto_level > 35 || !my_cur_lite)
    {
        /* Spread the flow */
        borg_flow_spread(250, TRUE, FALSE);
    }
    else
    {
        /* Spread the flow, No Optimize, Avoid */
        borg_flow_spread(250, FALSE, TRUE);
    }

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
    if (!track_more_num) return (FALSE);

    /* if not fleeing do not go down unless safe */
    if (!goal_fleeing && (cptr)NULL != borg_prepared(auto_depth + 1))
        return (FALSE);

    /* dont go down if hungry or low on food */
    if (!my_cur_lite || do_weak || do_hungry)
        return (FALSE);

    /* don't head for the stairs if you are recalling,  */
    /* even if you are fleeing. */
    if (goal_recalling)
        return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < track_more_num; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(track_more_y[i], track_more_x[i]);
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
 * Prepare to flow towards glyph of warding
 */
bool borg_flow_glyph(int why)
{
    int i;

    /* None to flow to */
    if (!track_glyph_num) return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < track_glyph_num; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(track_glyph_y[i], track_glyph_x[i]);
    }

    /* Spread the flow */
    borg_flow_spread(250, TRUE, FALSE);

    /* Attempt to Commit the flow */
    if (!borg_flow_commit("glyph of warding", why)) return (FALSE);

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
    auto_glow_n = 0;
    i=0;

    /* build the glow array */
    /* Scan map */
    for (y = w_y; y < w_y + SCREEN_HGT; y++)
    {
        for (x = w_x; x < w_x + SCREEN_WID; x++)
        {
            auto_grid *ag = &auto_grids[y][x];

            /* Not a perma-lit, and not our spot. */
            if (!(ag->info & BORG_GLOW)) continue;

            /* keep count */
            auto_glow_y[auto_glow_n] = y;
            auto_glow_x[auto_glow_n] = x;
            auto_glow_n++;

        }
     }

    /* None to flow to */
    if (!auto_glow_n) return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < auto_glow_n; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(auto_glow_y[i], auto_glow_x[i]);
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
    int i, x, y;

    /* Must be in town */
    if (auto_depth) return (FALSE);

    /* Clear the flow codes */
    borg_flow_clear();

    /* Visit the shops */
    for (i = 0; i < MAX_STORES; i++)
    {
        /* Must not be visited */
        if (auto_shops[i].when) continue;

        /* if poisoned or bleeding skip non temples */
        if ( (do_cut || do_poisoned) &&
             (i != 3 && i !=7) ) continue;

        /* if starving--skip non food places */
        if ( do_weak &&
             (i != 0 && i !=7) ) continue;

        /* if dark--skip non food places */
        if ( !my_cur_lite && (i != 0 ) && auto_level >= 2) continue;

        /* Obtain the location */
        x = track_shop_x[i];
        y = track_shop_y[i];

        /* Hack -- Must be known and not under the player */
        if (!x || !y || ((c_x == x) && (c_y == y))) continue;

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
    int x, y;

    cptr name = (f_name + f_info[0x08+i].name);

    /* Must be in town */
    if (auto_depth) return (FALSE);

    /* Obtain the location */
    x = track_shop_x[i];
    y = track_shop_y[i];

    /* Hack -- Must be known */
    if (!x || !y) return (FALSE);

    /* Hack -- re-enter a shop if needed */
    if ((x == c_x) && (y == c_y))
    {
        /* Note */
        borg_note("# Re-entering a shop");

        /* Enter the store */
        borg_keypress('5');

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

    auto_grid *ag;


    /* Efficiency -- Nothing to kill */
    if (!auto_kills_cnt) return (FALSE);


    /* Nothing found */
    auto_temp_n = 0;

    /* Scan the monster list */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        auto_kill *kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Ignore multiplying monsters */
        if (goal_ignoring && !do_afraid &&
            (r_info[kill->r_idx].flags2 & RF2_MULTIPLY)) continue;

        /* Avoid multiplying monsters */
        if (auto_level < 10 &&
            (r_info[kill->r_idx].flags2 & RF2_MULTIPLY)) continue;

        /* Dont chase towns people when starting out */
        if (auto_level < 5 && auto_depth ==0) continue;

        /* YOU ARE NOT A WARRIOR!! DON'T ACT LIKE ONE!! */
        if (auto_class == CLASS_MAGE && auto_level < 35) continue;


		/* Do not chase Invulnerable monsters */
		if (kill->invuln) continue;

        /* Access the location */
        x = kill->x;
        y = kill->y;

        /* Get the grid */
        ag = &auto_grids[y][x];

        /* Require line of sight if requested */
        if (viewable && !(ag->info & BORG_VIEW)) continue;

        /* Calculate danger */
        p = borg_danger_aux(y, x, 1, i);

        /* Hack -- Skip "deadly" monsters */
        if (p > avoidance / 2) continue;

        /* Careful -- Remember it */
        auto_temp_x[auto_temp_n] = x;
        auto_temp_y[auto_temp_n] = y;
        auto_temp_n++;
    }

    /* Nothing to kill */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Look for something to kill */
    for (i = 0; i < auto_temp_n; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(auto_temp_y[i], auto_temp_x[i]);
    }

    /* Spread the flow */
    /* if we are not flowing toward monsters that we can see, make sure they */
    /* are at least easily reachable.  The second flag is weather or not */
    /* to avoid unkown squares.  This was for performance when we have ESP. */
    borg_flow_spread(250, TRUE, !viewable);

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

    auto_grid *ag;


    /* Efficiency -- Nothing to take */
    if (!auto_takes_cnt) return (FALSE);


    /* Nothing yet */
    auto_temp_n = 0;

    /* Scan the object list */
    for (i = 1; i < auto_takes_nxt; i++)
    {
        auto_take *take = &auto_takes[i];

        int a;
        bool item_bad;

        /* Skip dead objects */
        if (!take->k_idx) continue;

        /* Access the location */
        x = take->x;
        y = take->y;

        /* look to see if this is on the bad items list */
        item_bad = FALSE;
        for (a = 0; a < 10; a++)
        {
            if (x == bad_obj_x[a] && y == bad_obj_y[a])
                item_bad = TRUE;
        }

        /* it is a bad item, do not track it */
        if (item_bad) continue;

        /* Get the grid */
        ag = &auto_grids[y][x];

        /* Require line of sight if requested */
        if (viewable && !(ag->info & BORG_VIEW)) continue;

        /* Careful -- Remember it */
        auto_temp_x[auto_temp_n] = x;
        auto_temp_y[auto_temp_n] = y;
        auto_temp_n++;
    }

    /* Nothing to take */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Look for something to take */
    for (i = 0; i < auto_temp_n; i++)
    {
        /* Enqueue the grid */
        borg_flow_enqueue_grid(auto_temp_y[i], auto_temp_x[i]);
    }

    /* Spread the flow */
    /* if we are not flowing toward items that we can see, make sure they */
    /* are at least easily reachable.  The second flag is weather or not  */
    /* to avoid unkown squares.  This was for performance. */
    borg_flow_spread(250, TRUE, !viewable);


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
 * or a non-perma-wall adjacent to a perma-wall. (GCV)
 */
static bool borg_flow_dark_interesting(int y, int x)
{
    int oy;
    int ox, i;

    auto_grid *ag;


    /* Get the auto_grid */
    ag = &auto_grids[y][x];


    /* Explore unknown grids */
    if (ag->feat == FEAT_NONE) return (TRUE);


    /* Efficiency -- Ignore "boring" grids */
    if (ag->feat < FEAT_TRAP_HEAD) return (FALSE);


    /* Explore "known treasure" */
    if ((ag->feat == FEAT_MAGMA_K) || (ag->feat == FEAT_QUARTZ_K))
    {
        /* Do not disarm when confused or dark*/
        if (!my_cur_lite || do_confused) return (FALSE);

        /* Allow "stone to mud" ability */
        if (borg_spell_legal(7, 3) ||
            borg_activate_artifact(ART_OROME, INVEN_WIELD)) return (TRUE);

        /* Do not dig unless we appear strong enough to succeed XXX XXX XXX */
        if (my_skill_dig < ((ag->feat & 0x01) ? 20 : 10) + 5) return (FALSE);

        /* Okay */
        return (TRUE);
    }

    /* "Vaults" Explore non perma-walls adjacent to a perma wall */
    if (ag->feat == FEAT_WALL_EXTRA || ag->feat == FEAT_MAGMA ||
        ag->feat == FEAT_QUARTZ)
    {
        /* Do not attempt when confused */
        if (do_confused) return (FALSE);

        /* hack and cheat.  No vaults  on this level */
        if (!vault_on_level) return (FALSE);

        /* AJG Do not attempt on the edge */
        if(x < AUTO_MAX_X-1
        && y < AUTO_MAX_Y-1
        && x > 1
        && y > 1)
        {
            /* scan the adjacent grids */
            for (ox = -1; ox <= 1; ox++)
            {
                for (oy = -1; oy <= 1; oy++)
                {

                    /* Acquire location */
                    ag = &auto_grids[oy+y][ox+x];

                    /* skip non perma grids wall */
                    if (ag->feat != FEAT_PERM_INNER) continue;

                    /* Allow "stone to mud" ability */
                    if (borg_spell_legal(7, 3) ||
                        borg_equips_artifact(ART_OROME, INVEN_WIELD)) return (TRUE);

                    /* Do not dig unless we appear strong enough to succeed XXX XXX XXX */
                    if (my_skill_dig < ((ag->feat & 0x01) ? 20 : 10) + 5) return (FALSE);

                    /* Glove up and dig in */
                    return (TRUE);
                }
            }
        }
    /* not adjacent to a GCV,  Restore Grid */
    ag = &auto_grids[y][x];

    }

    /* Explore "rubble" */
    if (ag->feat == FEAT_RUBBLE)
    {
        return (TRUE);
    }


    /* Explore "closed doors" */
    if ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_TAIL))
    {
            /* some closed doors leave alone */
            if (breeder_level)
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

            }
        /* this door should be ok to open */
        return (TRUE);
    }


    /* Explore "visible traps" */
    if ((ag->feat >= FEAT_TRAP_HEAD) && (ag->feat <= FEAT_TRAP_TAIL))
    {
        /* Do not disarm when blind */
        if (do_blind) return (FALSE);

        /* Do not disarm when confused */
        if (do_confused) return (FALSE);

        /* Do not disarm when hallucinating */
        if (do_image) return (FALSE);

        /* Do not disarm without lite */
        if (!my_cur_lite) return (FALSE);

        /* Do not disarm when clumsy */
        if (my_skill_dis < 20 && auto_level < 20 ) return (FALSE);
        if (my_skill_dis < 40 && auto_level < 10 ) return (FALSE);

        /* Okay */
        return (TRUE);
    }


    /* Ignore other grids */
    return (FALSE);
}


/*
 * Determine if a grid is "reachable" (and can be explored)
 */
static bool borg_flow_dark_reachable(int y, int x)
{
    int j;

    auto_grid *ag;

    /* Scan neighbors */
    for (j = 0; j < 8; j++)
    {
        int y2 = y + ddy_ddd[j];
        int x2 = x + ddx_ddd[j];

        /* Get the grid */
        ag = &auto_grids[y2][x2];

        /* Skip unknown grids (important) */
        if (ag->feat == FEAT_NONE) continue;

        /* Accept known floor grids */
        if (borg_cave_floor_grid(ag)) return (TRUE);
    }

    /* Failure */
    return (FALSE);
}


/*
 * Place a "direct path" into the flow array, checking danger
 *
 * Modify the "cost" array in such a way that from any point on
 * one "direct" path from the player to the given grid, as long
 * as the rest of the path is "safe" and "clear", the Borg will
 * walk along the path to the given grid.
 *
 * This function is used by "borg_flow_dark_1()" to provide an
 * optimized "flow" during the initial exploration of a level.
 */
static void borg_flow_direct(int y, int x)
{
    int n = 0;

    int x1, y1, x2, y2;

    int ay, ax;

    int shift;

    auto_grid *ag;


    /* Avoid icky grids */
    if (auto_data_icky->data[y][x]) return;

    /* Unknown */
    if (!auto_data_know->data[y][x])
    {
        /* Mark as known */
        auto_data_know->data[y][x] = TRUE;

        /* Mark dangerous grids as icky */
        if (borg_danger(y, x, 1) > avoidance / 2)
        {
            /* Icky */
            auto_data_icky->data[y][x] = TRUE;

            /* Avoid */
            return;
        }
    }


    /* Save the flow cost (zero) */
    auto_data_cost->data[y][x] = 0;


    /* Save "origin" */
    y1 = y;
    x1 = x;

    /* Save "destination" */
    y2 = c_y;
    x2 = c_x;

    /* Calculate distance components */
    ay = (y2 < y1) ? (y1 - y2) : (y2 - y1);
    ax = (x2 < x1) ? (x1 - x2) : (x2 - x1);

    /* Path */
    while (1)
    {
        /* Check for arrival at player */
        if ((x == x2) && (y == y2)) return;

        /* Next */
        n++;

        /* Move mostly vertically */
        if (ay > ax)
        {
            /* Extract a shift factor XXX */
            shift = (n * ax + (ay-1) / 2) / ay;

            /* Sometimes move along the minor axis */
            x = (x2 < x1) ? (x1 - shift) : (x1 + shift);

            /* Always move along major axis */
            y = (y2 < y1) ? (y1 - n) : (y1 + n);
        }

        /* Move mostly horizontally */
        else
        {
            /* Extract a shift factor XXX */
            shift = (n * ay + (ax-1) / 2) / ax;

            /* Sometimes move along the minor axis */
            y = (y2 < y1) ? (y1 - shift) : (y1 + shift);

            /* Always move along major axis */
            x = (x2 < x1) ? (x1 - n) : (x1 + n);
        }


        /* Access the grid */
        ag = &auto_grids[y][x];


        /* Ignore "wall" grids */
        if (!borg_cave_floor_grid(ag)) return;


        /* Abort at "icky" grids */
        if (auto_data_icky->data[y][x]) return;

        /* Analyze every grid once */
        if (!auto_data_know->data[y][x])
        {
            /* Mark as known */
            auto_data_know->data[y][x] = TRUE;

            /* Avoid dangerous grids (forever) */
            if (borg_danger(y, x, 1) > avoidance / 2)
            {
                /* Mark as icky */
                auto_data_icky->data[y][x] = TRUE;

                /* Abort */
                return;
            }
        }

        /* Abort "pointless" paths if possible */
        if (auto_data_cost->data[y][x] <= n) break;

        /* Save the new flow cost */
        auto_data_cost->data[y][x] = n;
    }
}


/*
 * Hack -- mark off the edges of a rectangle as "avoid" or "clear"
 */
static void borg_flow_border(int y1, int x1, int y2, int x2, bool stop)
{
    int x, y;

    /* Scan west/east edges */
    for (y = y1; y <= y2; y++)
    {
        /* Avoid/Clear west edge */
        auto_data_know->data[y][x1] = stop;
        auto_data_icky->data[y][x1] = stop;

        /* Avoid/Clear east edge */
        auto_data_know->data[y][x2] = stop;
        auto_data_icky->data[y][x2] = stop;
    }

    /* Scan north/south edges */
    for (x = x1; x <= x2; x++)
    {
        /* Avoid/Clear north edge */
        auto_data_know->data[y1][x] = stop;
        auto_data_icky->data[y1][x] = stop;

        /* Avoid/Clear south edge */
        auto_data_know->data[y2][x] = stop;
        auto_data_icky->data[y2][x] = stop;
    }
}


/*
 * Prepare to "flow" towards "interesting" grids (method 1)
 *
 * This function examines the torch-lit grids for "interesting" grids.
 */
static bool borg_flow_dark_1(void)
{
    int i;

    int x, y;


    /* Hack -- not in town */
    if (!auto_depth) return (FALSE);


    /* Reset */
    auto_temp_n = 0;

    /* Scan torch-lit grids */
    for (i = 0; i < auto_lite_n; i++)
    {
        y = auto_lite_y[i];
        x = auto_lite_x[i];

        /* Skip "boring" grids (assume reachable) */
        if (!borg_flow_dark_interesting(y, x)) continue;

        /* Careful -- Remember it */
        auto_temp_x[auto_temp_n] = x;
        auto_temp_y[auto_temp_n] = y;
        auto_temp_n++;
    }

    /* Nothing */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Create paths to useful grids */
    for (i = 0; i < auto_temp_n; i++)
    {
        y = auto_temp_y[i];
        x = auto_temp_x[i];

        /* Create a path */
        borg_flow_direct(y, x);
    }


    /* Attempt to Commit the flow */
    if (!borg_flow_commit(NULL, GOAL_DARK)) return (FALSE);

    /* Take one step */
    if (!borg_flow_old(GOAL_DARK)) return (FALSE);

    /* Forget goal */
    goal = 0;

    /* Success */
    return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids (method 2)
 *
 * This function is only used when the player is at least 4 grids away
 * from the outer dungeon wall, to prevent any nasty memory errors.
 *
 * This function examines the grids just outside the torch-lit grids
 * for "unknown" grids, and flows directly towards them (one step).
 */
static bool borg_flow_dark_2(void)
{
    int i, r;

    int x, y;

    auto_grid *ag;


    /* Hack -- not in town */
    if (!auto_depth) return (FALSE);


    /* Maximal radius */
    r = my_cur_lite + 1;


    /* Reset */
    auto_temp_n = 0;

    /* Four directions */
    for (i = 0; i < 4; i++)
    {
        y = c_y + ddy_ddd[i] * r;
        x = c_x + ddx_ddd[i] * r;

        /* Check legality */
        if (y < 1) continue;
        if (x < 1) continue;
        if (y > AUTO_MAX_Y - 2) continue;
        if (x > AUTO_MAX_X - 2) continue;

        /* Acquire grid */
        ag = &auto_grids[y][x];

        /* Require unknown */
        if (ag->feat != FEAT_NONE) continue;

        /* Require viewable */
        if (!(ag->info & BORG_VIEW)) continue;

        /* Careful -- Remember it */
        auto_temp_x[auto_temp_n] = x;
        auto_temp_y[auto_temp_n] = y;
        auto_temp_n++;
    }

    /* Nothing */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Create paths to useful grids */
    for (i = 0; i < auto_temp_n; i++)
    {
        y = auto_temp_y[i];
        x = auto_temp_x[i];

        /* Create a path */
        borg_flow_direct(y, x);
    }


    /* Attempt to Commit the flow */
    if (!borg_flow_commit(NULL, GOAL_DARK)) return (FALSE);

    /* Take one step */
    if (!borg_flow_old(GOAL_DARK)) return (FALSE);

    /* Forget goal */
    goal = 0;

    /* Success */
    return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids (method 3)
 *
 * Note the use of a limit on the "depth" of the flow, and of the flag
 * which avoids "unknown" grids when calculating the flow, both of which
 * help optimize this function to only handle "easily reachable" grids.
 *
 * The "auto_temp" array is much larger than any "local region".
 */
static bool borg_flow_dark_3(void)
{
    int i;

    int x, y;

    int x1, y1, x2, y2;


    /* Hack -- not in town */
    if (!auto_depth) return (FALSE);


    /* Local region */
    y1 = c_y - 4;
    x1 = c_x - 4;
    y2 = c_y + 4;
    x2 = c_x + 4;

    /* Restrict to "legal" grids */
    if (y1 < 1) y1 = 1;
    if (x1 < 1) x1 = 1;
    if (y2 > AUTO_MAX_Y - 2) y2 = AUTO_MAX_Y - 2;
    if (x2 > AUTO_MAX_X - 2) x2 = AUTO_MAX_X - 2;


    /* Reset */
    auto_temp_n = 0;

    /* Examine the region */
    for (y = y1; y <= y2; y++)
    {
        /* Examine the region */
        for (x = x1; x <= x2; x++)
        {
            /* Skip "boring" grids */
            if (!borg_flow_dark_interesting(y, x)) continue;

            /* Skip "unreachable" grids */
            if (!borg_flow_dark_reachable(y, x)) continue;

            /* Careful -- Remember it */
            auto_temp_x[auto_temp_n] = x;
            auto_temp_y[auto_temp_n] = y;
            auto_temp_n++;
        }
    }

    /* Nothing interesting */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < auto_temp_n; i++)
    {
        y = auto_temp_y[i];
        x = auto_temp_x[i];

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
 * Prepare to "flow" towards "interesting" grids (method 4)
 *
 * Note that we avoid grids close to the edge of the panel, since they
 * induce panel scrolling, which is "expensive" in terms of CPU usage,
 * and because this allows us to "expand" the border by several grids
 * to lay down the "avoidance" border in known legal grids.
 *
 * We avoid paths that would take us into different panels by setting
 * the "icky" flag for the "border" grids to prevent path construction,
 * and then clearing them when done, to prevent confusion elsewhere.
 *
 * The "auto_temp" array is large enough to hold one panel full of grids.
 */
static bool borg_flow_dark_4(void)
{
    int i, x, y;

    int x1, y1, x2, y2;


    /* Hack -- not in town */
    if (!auto_depth) return (FALSE);


    /* Local region */
    y1 = c_y - 11;
    x1 = c_x - 11;
    y2 = c_y + 11;
    x2 = c_x + 11;

    /* Restrict to "legal" grids */
    if (y1 < 1) y1 = 1;
    if (x1 < 1) x1 = 1;
    if (y2 > AUTO_MAX_Y - 2) y2 = AUTO_MAX_Y - 2;
    if (x2 > AUTO_MAX_X - 2) x2 = AUTO_MAX_X - 2;


    /* Nothing yet */
    auto_temp_n = 0;

    /* Examine the panel */
    for (y = y1; y <= y2; y++)
    {
        /* Examine the panel */
        for (x = x1; x <= x2; x++)
        {
            /* Skip "boring" grids */
            if (!borg_flow_dark_interesting(y, x)) continue;

            /* Skip "unreachable" grids */
            if (!borg_flow_dark_reachable(y, x)) continue;

            /* Careful -- Remember it */
            auto_temp_x[auto_temp_n] = x;
            auto_temp_y[auto_temp_n] = y;
            auto_temp_n++;
        }
    }

    /* Nothing useful */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < auto_temp_n; i++)
    {
        y = auto_temp_y[i];
        x = auto_temp_x[i];

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
    if (!borg_flow_commit("dark-4", GOAL_DARK)) return (FALSE);

    /* Take one step */
    if (!borg_flow_old(GOAL_DARK)) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids (method 5)
 */
static bool borg_flow_dark_5(void)
{
    int i, x, y;


    /* Hack -- not in town */
    if (!auto_depth) return (FALSE);


    /* Nothing yet */
    auto_temp_n = 0;

    /* Examine every "legal" grid */
    for (y = 1; y < AUTO_MAX_Y-1; y++)
    {
        for (x = 1; x < AUTO_MAX_X-1; x++)
        {
            /* Skip "boring" grids */
            if (!borg_flow_dark_interesting(y, x)) continue;

            /* Skip "unreachable" grids */
            if (!borg_flow_dark_reachable(y, x)) continue;

            /* Careful -- Remember it */
            auto_temp_x[auto_temp_n] = x;
            auto_temp_y[auto_temp_n] = y;
            auto_temp_n++;

            /* Paranoia -- Check for overflow */
            if (auto_temp_n == AUTO_TEMP_MAX)
            {
                /* Hack -- Double break */
                y = AUTO_MAX_Y;
                x = AUTO_MAX_X;
                break;
            }
        }
    }

    /* Nothing useful */
    if (!auto_temp_n) return (FALSE);


    /* Clear the flow codes */
    borg_flow_clear();

    /* Enqueue useful grids */
    for (i = 0; i < auto_temp_n; i++)
    {
        y = auto_temp_y[i];
        x = auto_temp_x[i];

        /* Enqueue the grid */
        borg_flow_enqueue_grid(y, x);
    }

    /* Spread the flow */
    borg_flow_spread(250, TRUE, TRUE);


    /* Attempt to Commit the flow */
    if (!borg_flow_commit("dark-5", GOAL_DARK)) return (FALSE);

    /* Take one step */
    if (!borg_flow_old(GOAL_DARK)) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids
 *
 * The "exploration" routines are broken into "near" and "far"
 * exploration, and each set is chosen via the flag below.
 */
bool borg_flow_dark(bool neer)
{
    /* Paranoia */
    if (borg_flow_dark_interesting(c_y, c_x))
    {
        return (FALSE);
    }

    /* Near */
    if (neer)
    {
        /* Method 1 */
        if (borg_flow_dark_1()) return (TRUE);

        /* Method 2 */
        if (borg_flow_dark_2()) return (TRUE);

        /* Method 3 */
        if (borg_flow_dark_3()) return (TRUE);
    }

    /* Far */
    else
    {
        /* Method 4 */
        if (borg_flow_dark_4()) return (TRUE);

        /* Method 5 */
        if (borg_flow_dark_5()) return (TRUE);
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
    int cost;

    int i, x, y, v;

    int b_x = c_x;
    int b_y = c_y;
    int b_v = -1;

    auto_grid *ag;


    /* Hack -- not in town */
    if (!auto_depth) return (FALSE);


    /* Not bored */
    if (!bored)
    {
        /* Look around for danger */
        int p = borg_danger(c_y, c_x, 1);

        /* Avoid searching when in danger */
        if (p > avoidance / 4) return (FALSE);
    }


    /* We have arrived */
    if ((spastic_x == c_x) && (spastic_y == c_y))
    {
        /* Cancel */
        spastic_x = 0;
        spastic_y = 0;

        /* Take note */
        borg_note(format("# Spastic Searching at (%d,%d)...", c_x, c_y));

        /* Count searching */
        for (i = 0; i < 9; i++)
        {
            /* Extract the location */
            int xx = c_x + ddx_ddd[i];
            int yy = c_y + ddy_ddd[i];

            /* Current grid */
            ag = &auto_grids[yy][xx];

            /* Tweak -- Remember the search */
            if (ag->xtra < 100) ag->xtra += 5;
        }

        /* Tweak -- Search a little */
        borg_keypress('0');
        borg_keypress('5');
        borg_keypress('s');

        /* Success */
        return (TRUE);
    }


    /* Reverse flow */
    borg_flow_reverse();

    /* Scan the entire map */
    for (y = 1; y < AUTO_MAX_Y-1; y++)
    {
        for (x = 1; x < AUTO_MAX_X-1; x++)
        {
            auto_grid *ag_ptr[8];

            int wall = 0;
            int supp = 0;
            int diag = 0;


            /* Acquire the grid */
            ag = &auto_grids[y][x];

            /* Skip unknown grids */
            if (ag->feat == FEAT_NONE) continue;

            /* Skip walls/doors */
            if (!borg_cave_floor_grid(ag)) continue;


            /* Acquire the cost */
            cost = auto_data_cost->data[y][x];

            /* Skip "unreachable" grids */
            if (cost >= 250) continue;


            /* Tweak -- Limit total searches */
            if (ag->xtra >= 50) continue;

            /* Limit initial searches until bored */
            if (!bored && (ag->xtra > 5)) continue;

            /* Avoid searching detected sectors */
            if (auto_detect_door[y/11][x/33]) continue;


            /* Extract adjacent locations */
            for (i = 0; i < 8; i++)
            {
                /* Extract the location */
                int xx = x + ddx_ddd[i];
                int yy = y + ddy_ddd[i];

                /* Get the grid contents */
                ag_ptr[i] = &auto_grids[yy][xx];
            }


            /* Count possible door locations */
            for (i = 0; i < 4; i++)
            {
                ag = ag_ptr[i];
                if (ag->feat >= FEAT_WALL_EXTRA) wall++;
            }

            /* No possible secret doors */
            if (wall < 1) continue;


            /* Count supporting evidence for secret doors */
            for (i = 0; i < 4; i++)
            {
                ag = ag_ptr[i];

                /* Rubble */
                if (ag->feat == FEAT_RUBBLE) continue;

                /* Walls, Doors */
                if (((ag->feat >= FEAT_SECRET) && (ag->feat <= FEAT_PERM_SOLID)) ||
                    ((ag->feat == FEAT_OPEN) || (ag->feat == FEAT_BROKEN)) ||
                    ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_TAIL)))
                {
                    supp++;
                }
            }

            /* Count supporting evidence for secret doors */
            for (i = 4; i < 8; i++)
            {
                ag = ag_ptr[i];

                /* Rubble */
                if (ag->feat == FEAT_RUBBLE) continue;

                /* Walls */
                if (ag->feat >= FEAT_SECRET)
                {
                    diag++;
                }
            }

            /* No possible secret doors */
            if (diag < 2) continue;


            /* Tweak -- Reward walls, punish visitation and distance */
            v = (supp * 500) + (diag * 100) - (ag->xtra * 20) - (cost * 1);

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


    /* Access grid */
    ag = &auto_grids[b_y][b_x];

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




/*
 * Initialize this file
 */
void borg_init_6(void)
{
    /* Nothing */
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
