/*
 * File: cave-view.c
 * Purpose: Line-of-sight and view calculations
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


#include "s-angband.h"


/*
 * Approximate Distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
int distance(struct loc *grid1, struct loc *grid2)
{
    /* Find the absolute y/x distance components */
    int ay = abs(grid2->y - grid1->y);
    int ax = abs(grid2->x - grid1->x);

    /* Approximate the distance */
    return (ay > ax)? (ay + (ax >> 1)): (ax + (ay >> 1));
}


/*
 * A simple, fast, integer-based line-of-sight algorithm.  By Joseph Hall,
 * 4116 Brewster Drive, Raleigh NC 27606.  Email to jnh@ecemwl.ncsu.edu.
 *
 * This function returns true if a "line of sight" can be traced from the
 * center of the grid (x1,y1) to the center of the grid (x2,y2), with all
 * of the grids along this path (except for the endpoints) being non-wall
 * grids.  Actually, the "chess knight move" situation is handled by some
 * special case code which allows the grid diagonally next to the player
 * to be obstructed, because this yields better gameplay semantics.  This
 * algorithm is totally reflexive, except for "knight move" situations.
 *
 * Because this function uses (short) ints for all calculations, overflow
 * may occur if dx and dy exceed 90.
 *
 * Once all the degenerate cases are eliminated, we determine the "slope"
 * ("m"), and we use special "fixed point" mathematics in which we use a
 * special "fractional component" for one of the two location components
 * ("qy" or "qx"), which, along with the slope itself, are "scaled" by a
 * scale factor equal to "abs(dy*dx*2)" to keep the math simple.  Then we
 * simply travel from start to finish along the longer axis, starting at
 * the border between the first and second tiles (where the y offset is
 * thus half the slope), using slope and the fractional component to see
 * when motion along the shorter axis is necessary.  Since we assume that
 * vision is not blocked by "brushing" the corner of any grid, we must do
 * some special checks to avoid testing grids which are "brushed" but not
 * actually "entered".
 *
 * Angband three different "line of sight" type concepts, including this
 * function (which is used almost nowhere), the "project()" method (which
 * is used for determining the paths of projectables and spells and such),
 * and the "update_view()" concept (which is used to determine which grids
 * are "viewable" by the player, which is used for many things, such as
 * determining which grids are illuminated by the player's torch, and which
 * grids and monsters can be "seen" by the player, etc).
 */
bool los(struct chunk *c, struct loc *grid1, struct loc *grid2)
{
    /* Delta */
    int dx, dy;

    /* Absolute */
    int ax, ay;

    /* Signs */
    int sx, sy;

    /* Fractions */
    int qx, qy;

    /* Scanners */
    struct loc scan;

    /* Scale factors */
    int f1, f2;

    /* Slope, or 1/Slope, of LOS */
    int m;

    /* Extract the offset */
    dy = grid2->y - grid1->y;
    dx = grid2->x - grid1->x;

    /* Extract the absolute offset */
    ay = ABS(dy);
    ax = ABS(dx);

    /* Handle adjacent (or identical) grids */
    if ((ax < 2) && (ay < 2)) return true;

    /* Directly South/North */
    if (!dx)
    {
        /* South -- check for walls */
        if (dy > 0)
        {
            scan.x = grid1->x;
            for (scan.y = grid1->y + 1; scan.y < grid2->y; scan.y++)
            {
                if (!square_isprojectable(c, &scan)) return false;
            }
        }

        /* North -- check for walls */
        else
        {
            scan.x = grid1->x;
            for (scan.y = grid1->y - 1; scan.y > grid2->y; scan.y--)
            {
                if (!square_isprojectable(c, &scan)) return false;
            }
        }

        /* Assume los */
        return true;
    }

    /* Directly East/West */
    if (!dy)
    {
        /* East -- check for walls */
        if (dx > 0)
        {
            scan.y = grid1->y;
            for (scan.x = grid1->x + 1; scan.x < grid2->x; scan.x++)
            {
                if (!square_isprojectable(c, &scan)) return false;
            }
        }

        /* West -- check for walls */
        else
        {
            scan.y = grid1->y;
            for (scan.x = grid1->x - 1; scan.x > grid2->x; scan.x--)
            {
                if (!square_isprojectable(c, &scan)) return false;
            }
        }

        /* Assume los */
        return true;
    }

    /* Extract some signs */
    sx = (dx < 0) ? -1 : 1;
    sy = (dy < 0) ? -1 : 1;

    /* Vertical and horizontal "knights" */
    loc_init(&scan, grid1->x, grid1->y + sy);
    if ((ax == 1) && (ay == 2) && square_isprojectable(c, &scan))
        return true;
    loc_init(&scan, grid1->x + sx, grid1->y);
    if ((ay == 1) && (ax == 2) && square_isprojectable(c, &scan))
        return true;

    /* Calculate scale factor div 2 */
    f2 = (ax * ay);

    /* Calculate scale factor */
    f1 = f2 << 1;

    /* Travel horizontally */
    if (ax >= ay)
    {
        /* Let m = dy / dx * 2 * (dy * dx) = 2 * dy * dy */
        qy = ay * ay;
        m = qy << 1;

        scan.x = grid1->x + sx;

        /* Consider the special case where slope == 1. */
        if (qy == f2)
        {
            scan.y = grid1->y + sy;
            qy -= f1;
        }
        else
            scan.y = grid1->y;

        /* Note (below) the case (qy == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (grid2->x - scan.x)
        {
            if (!square_isprojectable(c, &scan)) return false;

            qy += m;

            if (qy < f2)
                scan.x += sx;
            else if (qy > f2)
            {
                scan.y += sy;
                if (!square_isprojectable(c, &scan)) return false;
                qy -= f1;
                scan.x += sx;
            }
            else
            {
                scan.y += sy;
                qy -= f1;
                scan.x += sx;
            }
        }
    }

    /* Travel vertically */
    else
    {
        /* Let m = dx / dy * 2 * (dx * dy) = 2 * dx * dx */
        qx = ax * ax;
        m = qx << 1;

        scan.y = grid1->y + sy;

        if (qx == f2)
        {
            scan.x = grid1->x + sx;
            qx -= f1;
        }
        else
            scan.x = grid1->x;

        /* Note (below) the case (qx == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (grid2->y - scan.y)
        {
            if (!square_isprojectable(c, &scan)) return false;

            qx += m;

            if (qx < f2)
                scan.y += sy;
            else if (qx > f2)
            {
                scan.x += sx;
                if (!square_isprojectable(c, &scan)) return false;
                qx -= f1;
                scan.y += sy;
            }
            else
            {
                scan.x += sx;
                qx -= f1;
                scan.y += sy;
            }
        }
    }

    /* Assume los */
    return true;
}


/*
 * Some comments on the dungeon related data structures and functions...
 *
 * Angband is primarily a dungeon exploration game, and it should come as
 * no surprise that the internal representation of the dungeon has evolved
 * over time in much the same way as the game itself, to provide semantic
 * changes to the game itself, to make the code simpler to understand, and
 * to make the executable itself faster or more efficient in various ways.
 *
 * There are a variety of dungeon related data structures, and associated
 * functions, which store information about the dungeon, and provide methods
 * by which this information can be accessed or modified.
 *
 * Some of this information applies to the dungeon as a whole, such as the
 * list of unique monsters which are still alive.  Some of this information
 * only applies to the current dungeon level, such as the current depth, or
 * the list of monsters currently inhabiting the level.  And some of the
 * information only applies to a single grid of the current dungeon level,
 * such as whether the grid is illuminated, or whether the grid contains a
 * monster, or whether the grid can be seen by the player.  If Angband was
 * to be turned into a multi-player game, some of the information currently
 * associated with the dungeon should really be associated with the player,
 * such as whether a given grid is viewable by a given player.
 *
 * Currently, a lot of the information about the dungeon is stored in ways
 * that make it very efficient to access or modify the information, while
 * still attempting to be relatively conservative about memory usage, even
 * if this means that some information is stored in multiple places, or in
 * ways which require the use of special code idioms.  For example, each
 * monster record in the monster array contains the location of the monster,
 * and each cave grid has an index into the monster array, or a zero if no
 * monster is in the grid.  This allows the monster code to efficiently see
 * where the monster is located, while allowing the dungeon code to quickly
 * determine not only if a monster is present in a given grid, but also to
 * find out which monster.  The extra space used to store the information
 * twice is inconsequential compared to the speed increase.
 *
 * Several pieces of information about each cave grid are stored in the
 * info field of the "cave->squares" array, which is a special array of
 * bitflags.
 *
 * The "SQUARE_ROOM" flag is used to determine which grids are part of "rooms",
 * and thus which grids are affected by "illumination" spells.
 *
 * The "SQUARE_VAULT" flag is used to determine which grids are part of
 * "vaults", and thus which grids cannot serve as the destinations of player
 * teleportation.
 *
 * The "SQUARE_NO_TELEPORT" flag is used to determine which grids are part of "pits",
 * and thus which grids cannot serve as origin for player/monster teleportation.
 *
 * The "SQUARE_GLOW" flag is used to determine which grids are "permanently
 * illuminated". This flag is used by the update_view() function to help
 * determine which viewable flags may be "seen" by the player. This flag
 * is used by the "map_info" function to determine if a grid is only lit by
 * the player's torch. This flag has special semantics for wall grids
 * (see "update_view()").
 *
 * The "SQUARE_VIEW" flag is used to determine which grids are currently in
 * line of sight of the player.  This flag is set by (and used by) the
 * "update_view()" function.  This flag is used by any code which needs to
 * know if the player can "view" a given grid.  This flag is used by the
 * "map_info()" function for some optional special lighting effects.  The
 * "square_isview()" function wraps an abstraction around this flag, but
 * certain code idioms are much more efficient.  This flag is used to check
 * if a modification to a terrain feature might affect the player's field of
 * view.  This flag is used to see if certain monsters are "visible" to the
 * player.  This flag is used to allow any monster in the player's field of
 * view to "sense" the presence of the player.
 *
 * The "SQUARE_SEEN" flag is used to determine which grids are currently in
 * line of sight of the player and also illuminated in some way.  This flag
 * is set by the "update_view()" function, using computations based on the
 * "SQUARE_VIEW" and "SQUARE_GLOW" flags and terrain of various grids.
 * This flag is used by any code which needs to know if the player can "see" a
 * given grid.  This flag is used by the "map_info()" function both to see
 * if a given "boring" grid can be seen by the player, and for some optional
 * special lighting effects.  The "square_isseen()" function wraps an
 * abstraction around this flag, but certain code idioms are much more
 * efficient.  This flag is used to see if certain monsters are "visible" to
 * the player.  This flag is never set for a grid unless "SQUARE_VIEW" is also
 * set for the grid.  Whenever the terrain or "SQUARE_GLOW" flag changes
 * for a grid which has the "SQUARE_VIEW" flag set, the "SQUARE_SEEN" flag must
 * be recalculated.  The simplest way to do this is to call "update_view()"
 * whenever the terrain or "SQUARE_GLOW" flags change for a grid which has
 * "SQUARE_VIEW" set.
 *
 * The "SQUARE_WASSEEN" flag is used to determine if the "SQUARE_SEEN" flag for a
 * grid has changed during the "update_view()" function. This flag must always
 * be cleared by any code which sets it.
 *
 * The "update_view()" function is an extremely important function.  It is
 * called only when the player moves, significant terrain changes, or the
 * player's blindness or torch radius changes.  Note that when the player
 * is resting, or performing any repeated actions (like digging, disarming,
 * farming, etc), there is no need to call the "update_view()" function, so
 * even if it was not very efficient, this would really only matter when the
 * player was "running" through the dungeon.  It sets the "SQUARE_VIEW" flag
 * on every cave grid in the player's field of view.  It also checks the torch
 * radius of the player, and sets the "SQUARE_SEEN" flag for every grid which
 * is in the "field of view" of the player and which is also "illuminated",
 * either by the players torch (if any) or by any permanent light source.
 * It could use and help maintain information about multiple light sources,
 * which would be helpful in a multi-player version of Angband.
 *
 * Note that the "update_view()" function allows, among other things, a room
 * to be "partially" seen as the player approaches it, with a growing cone
 * of floor appearing as the player gets closer to the door.  Also, by not
 * turning on the "memorize perma-lit grids" option, the player will only
 * "see" those floor grids which are actually in line of sight.  And best
 * of all, you can now activate the special lighting effects to indicate
 * which grids are actually in the player's field of view by using dimmer
 * colors for grids which are not in the player's field of view, and/or to
 * indicate which grids are illuminated only by the player's torch by using
 * the color yellow for those grids.
 *
 * It seems as though slight modifications to the "update_view()" functions
 * would allow us to determine "reverse" line-of-sight as well as "normal"
 * line-of-sight", which would allow monsters to have a more "correct" way
 * to determine if they can "see" the player, since right now, they "cheat"
 * somewhat and assume that if the player has "line of sight" to them, then
 * they can "pretend" that they have "line of sight" to the player.  But if
 * such a change was attempted, the monsters would actually start to exhibit
 * some undesirable behavior, such as "freezing" near the entrances to long
 * hallways containing the player, and code would have to be added to make
 * the monsters move around even if the player was not detectable, and to
 * "remember" where the player was last seen, to avoid looking stupid.
 *
 * Note that the "SQUARE_GLOW" flag means that a grid is permanently lit in
 * some way.  However, for the player to "see" the grid, as determined by
 * the "SQUARE_SEEN" flag, the player must not be blind, the grid must have
 * the "SQUARE_VIEW" flag set, and if the grid is a "wall" grid, and it is
 * not lit by the player's torch, then it must touch a projectable grid
 * which has both the "SQUARE_GLOW"
 * and "SQUARE_VIEW" flags set.  This last part about wall grids is induced
 * by the semantics of "SQUARE_GLOW" as applied to wall grids, and checking
 * the technical requirements can be very expensive, especially since the
 * grid may be touching some "illegal" grids.  Luckily, it is more or less
 * correct to restrict the "touching" grids from the eight "possible" grids
 * to the (at most) three grids which are touching the grid, and which are
 * closer to the player than the grid itself, which eliminates more than
 * half of the work, including all of the potentially "illegal" grids, if
 * at most one of the three grids is a "diagonal" grid.  In addition, in
 * almost every situation, it is possible to ignore the "SQUARE_VIEW" flag
 * on these three "touching" grids, for a variety of technical reasons.
 * Finally, note that in most situations, it is only necessary to check
 * a single "touching" grid, in fact, the grid which is strictly closest
 * to the player of all the touching grids, and in fact, it is normally
 * only necessary to check the "SQUARE_GLOW" flag of that grid, again, for
 * various technical reasons.  However, one of the situations which does
 * not work with this last reduction is the very common one in which the
 * player approaches an illuminated room from a dark hallway, in which the
 * two wall grids which form the "entrance" to the room would not be marked
 * as "SQUARE_SEEN", since of the three "touching" grids nearer to the player
 * than each wall grid, only the farthest of these grids is itself marked
 * "SQUARE_GLOW".
 *
 *
 * Here are some pictures of the legal "light source" radius values, in
 * which the numbers indicate the "order" in which the grids could have
 * been calculated, if desired.  Note that the code will work with larger
 * radiuses, though currently yields such a radius, and the game would
 * become slower in some situations if it did.
 *
 *       Rad=0     Rad=1      Rad=2        Rad=3
 *      No-Light Torch,etc   Lantern     Artifacts
 *
 *                                          333
 *                             333         43334
 *                  212       32123       3321233
 *         @        1@1       31@13       331@133
 *                  212       32123       3321233
 *                             333         43334
 *                                          333
 */


static void mark_wasseen(struct player *p, struct chunk *c)
{
    struct loc begin, end;
    struct loc_iterator iter;

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* Save the old "view" grids for later */
    do
    {
        if (square_isseen(p, &iter.cur))
            sqinfo_on(square(c, &iter.cur)->info, SQUARE_WASSEEN);
        sqinfo_off(square_p(p, &iter.cur)->info, SQUARE_VIEW);
        sqinfo_off(square_p(p, &iter.cur)->info, SQUARE_SEEN);
    }
    while (loc_iterator_next_strict(&iter));
}


static void add_monster_lights(struct player *p, struct chunk *c)
{
    int d, k;

    /* Scan monster list and add monster lights */
    for (k = 1; k < cave_monster_max(c); k++)
    {
        /* Check the k'th monster */
        struct monster *mon = cave_monster(c, k);
        bool in_los = los(c, &p->grid, &mon->grid);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Skip monsters not carrying light */
        if (!rf_has(mon->race->flags, RF_HAS_LIGHT)) continue;

        /* Light a 3x3 box centered on the monster */
        for (d = 0; d < 9; d++)
        {
            struct loc grid;

            loc_sum(&grid, &mon->grid, &ddgrid_ddd[d]);

            /* If the monster isn't visible we can only light open tiles */
            if (!in_los && !square_isprojectable(c, &grid)) continue;

            /* If the tile is too far away we won't light it */
            if (distance(&p->grid, &grid) > z_info->max_sight) continue;

            /* If the tile itself isn't in LOS, don't light it */
            if (!los(c, &p->grid, &grid)) continue;

            /* Mark the square lit and seen */
            sqinfo_on(square_p(p, &grid)->info, SQUARE_VIEW);
            sqinfo_on(square_p(p, &grid)->info, SQUARE_SEEN);
        }
    }
}


static void add_player_lights(struct player *p, struct chunk *c)
{
    int i, j, k;

    /* Scan player list and add player lights */
    for (k = 1; k <= NumPlayers; k++)
    {
        /* Check the k'th player */
        struct player *q = player_get(k);
        bool in_los;

        /* Ignore the player that we're updating */
        if (q == p) continue;

        /* Skip players not on this level */
        if (!wpos_eq(&q->wpos, &p->wpos)) continue;

        /* Skip players not carrying light */
        if (!q->state.cur_light) continue;

        in_los = los(c, &p->grid, &q->grid);

        /* Light a cur_light radius area centered on the player */
        for (i = 0 - q->state.cur_light; i <= q->state.cur_light; i++)
        {
            for (j = 0 - q->state.cur_light; j <= q->state.cur_light; j++)
            {
                struct loc grid;

                loc_init(&grid, q->grid.x + j, q->grid.y + i);

                /* Oops */
                if (!square_in_bounds(c, &grid)) continue;

                /* Skip grids out of radius */
                if (distance(&q->grid, &grid) > q->state.cur_light) continue;

                /* If the player isn't visible we can only light open tiles */
                if (!in_los && !square_isprojectable(c, &grid)) continue;

                /* If the tile is too far away we won't light it */
                if (distance(&p->grid, &grid) > z_info->max_sight) continue;

                /* If the tile itself isn't in LOS, don't light it */
                if (!los(c, &p->grid, &grid)) continue;

                /* If the tile isn't in LOS of the other player, don't light it either */
                if (!los(c, &q->grid, &grid)) continue;

                /* Mark the square lit and seen */
                sqinfo_on(square_p(p, &grid)->info, SQUARE_VIEW);
                sqinfo_on(square_p(p, &grid)->info, SQUARE_SEEN);
            }
        }
    }
}


static void update_one(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Remove view if blind, check visible squares for traps */
    if (p->timed[TMD_BLIND])
        sqinfo_off(square_p(p, grid)->info, SQUARE_SEEN);
    else if (square_isseen(p, grid) && square_issecrettrap(c, grid))
        square_reveal_trap(p, grid, false, true);

    /* Square went from unseen -> seen */
    if (square_isseen(p, grid) && !square_wasseen(c, grid))
    {
        if (square_isfeel(c, grid) && square_ispfeel(p, grid))
        {
            p->cave->feeling_squares++;
            sqinfo_off(square_p(p, grid)->info, SQUARE_FEEL);
            if (p->cave->feeling_squares == z_info->feeling_need)
            {
                display_feeling(p, true);
                p->upkeep->redraw |= (PR_STATE);
            }
        }

        square_note_spot_aux(p, c, grid);
        square_light_spot_aux(p, c, grid);
    }

    /* Square went from seen -> unseen */
    if (!square_isseen(p, grid) && square_wasseen(c, grid))
        square_light_spot_aux(p, c, grid);

    sqinfo_off(square(c, grid)->info, SQUARE_WASSEEN);
}


static void become_viewable(struct player *p, struct chunk *c, struct loc *grid, bool lit)
{
    if (square_isview(p, grid)) return;

    sqinfo_on(square_p(p, grid)->info, SQUARE_VIEW);

    if (lit) sqinfo_on(square_p(p, grid)->info, SQUARE_SEEN);

    if (square_isglow(c, grid))
    {
        struct loc gridc;

        loc_copy(&gridc, grid);

        /* For walls, move a bit towards the player */
        if (square_iswall(c, grid))
        {
            loc_init(&gridc,
                ((grid->x < p->grid.x)? (grid->x + 1): (grid->x > p->grid.x)? (grid->x - 1): grid->x),
                ((grid->y < p->grid.y)? (grid->y + 1): (grid->y > p->grid.y)? (grid->y - 1): grid->y));
        }

        if (square_isglow(c, &gridc))
            sqinfo_on(square_p(p, grid)->info, SQUARE_SEEN);
    }
}


static void update_view_one(struct player *p, struct chunk *c, struct loc *grid, int radius)
{
    int dir;
    int d = distance(grid, &p->grid);
    bool lit = ((d < radius)? true: false);
    struct loc cgrid;

    if (d > z_info->max_sight) return;

    /* Light squares with bright terrain, or squares adjacent */
    if (square_isbright(c, grid)) lit = true;
    for (dir = 0; dir < 8; dir++)
    {
        struct loc adjacent;

        loc_sum(&adjacent, grid, &ddgrid_ddd[dir]);

        if (!square_in_bounds(c, &adjacent)) continue;
        if (square_isbright(c, &adjacent)) lit = true;
    }

    loc_copy(&cgrid, grid);

    /*
     * Special case for wall lighting. If we are a wall and the square in
     * the direction of the player is in LOS, we are in LOS. This avoids
     * situations like:
     *
     * #1#############
     * #............@#
     * ###############
     *
     * where the wall cell marked '1' would not be lit because the LOS
     * algorithm runs into the adjacent wall cell.
     */
    if (square_iswall(c, grid))
    {
        int dx = grid->x - p->grid.x;
        int dy = grid->y - p->grid.y;
        int ax = ABS(dx);
        int ay = ABS(dy);
        int sx = (dx > 0)? 1: -1;
        int sy = (dy > 0)? 1: -1;

        loc_init(&cgrid,
            ((grid->x < p->grid.x)? (grid->x + 1): (grid->x > p->grid.x)? (grid->x - 1): grid->x),
            ((grid->y < p->grid.y)? (grid->y + 1): (grid->y > p->grid.y)? (grid->y - 1): grid->y));

        /*
         * Check that the cell we're trying to steal LOS from isn't a
         * wall. If we don't do this, double-thickness walls will have
         * both sides visible.
         */
        if (square_iswall(c, &cgrid)) loc_copy(&cgrid, grid);

        /* Check that we got here via the 'knight's move' rule. If so, don't steal LOS. */
        if ((ax == 2) && (ay == 1))
        {
            struct loc grid1, grid2;

            loc_init(&grid1, grid->x - sx, grid->y);
            loc_init(&grid2, grid->x - sx, grid->y - sy);
            if (square_in_bounds(c, &grid1) && !square_iswall(c, &grid1) &&
                square_in_bounds(c, &grid2) && square_iswall(c, &grid2))
            {
                loc_copy(&cgrid, grid);
            }
        }
        else if ((ax == 1) && (ay == 2))
        {
            struct loc grid1, grid2;

            loc_init(&grid1, grid->x, grid->y - sy);
            loc_init(&grid2, grid->x - sx, grid->y - sy);
            if (square_in_bounds(c, &grid1) && !square_iswall(c, &grid1) &&
                square_in_bounds(c, &grid2) && square_iswall(c, &grid2))
            {
                loc_copy(&cgrid, grid);
            }
        }
    }

    if (los(c, &p->grid, &cgrid))
        become_viewable(p, c, grid, lit);
}


/*
 * Calculate the complete field of view.
 */
void update_view(struct player *p, struct chunk *c)
{
    int radius;
    struct loc begin, end;
    struct loc_iterator iter;

    mark_wasseen(p, c);

    /* Extract "radius" value */
    radius = p->state.cur_light;

    /* Handle real light */
    if (radius > 0) ++radius;

    add_monster_lights(p, c);

    add_player_lights(p, c);

    /* Assume we can view the player grid */
    sqinfo_on(square_p(p, &p->grid)->info, SQUARE_VIEW);
    if ((radius > 0) || square_isglow(c, &p->grid))
        sqinfo_on(square_p(p, &p->grid)->info, SQUARE_SEEN);

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* View squares we have LOS to */
    do
    {
        update_view_one(p, c, &iter.cur, radius);
    }
    while (loc_iterator_next_strict(&iter));

    loc_iterator_first(&iter, &begin, &end);

    do
    {
        update_one(p, c, &iter.cur);
    }
    while (loc_iterator_next_strict(&iter));
}


/*
 * Returns true if the player's grid is dark
 */
bool no_light(struct player *p)
{
    return (!square_isseen(p, &p->grid));
}