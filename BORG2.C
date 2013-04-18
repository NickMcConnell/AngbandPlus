/* File: borg2.c */
/* Purpose: Low level dungeon mapping skills -BEN- */

#include "angband.h"


#ifdef ALLOW_BORG

#include "borg1.h"
#include "borg2.h"


/*
 * This file helps the Borg understand mapping the dungeon.
 *
 * Currently, this includes general routines involving dungeon grids,
 * including calculating "flow" values from place to place, determining
 * "line of sight", plus "field of view" and "torch-lit grids", setting
 * the target to a given location, and extracting the optimal direction
 * for "movement" from place to place.
 *
 * Note that the dungeon is assumed smaller than 256 by 256.
 *
 * This file also supplies the (compiled out) support for "automatic
 * room extraction".  This code will automatically group regions of
 * the dungeon into rooms, and do the "flow" navigation on those rooms
 * instead of on grids.  Often, this takes less space, and is faster,
 * howver, it is more complicated, and does not allow "specialized"
 * flow calculations that penalize grids by variable amounts.
 */



#ifdef BORG_ROOMS

/*
 * Hack -- access the "set" of rooms containing a given point
 *
 * We should probably build a "list" of "used rooms", where the "tail"
 * of that list is all the "crossed rooms", and also keep a pointer to
 * that tail for "fast" access to the "crossed rooms" set.
 *
 * This function is necessary because some grids (though not many)
 * can be contained in multiple rooms, because we allow "crossing"
 * rooms.  The Borg uses crossing rooms not only for actual "cross"
 * and "overlap" rooms in the dungeon, but also weird double width
 * corridors, the corners of "ragged edge" rooms, and various regions
 * in the town.  Actually, the worst "offenders" come from the town...
 *
 * Note that this function should be valid even if rooms are destroyed
 * inside a single "search" through the room list.  This is because
 * only the "first" call to this function checks the "room" field.
 */
auto_room *room(bool go, int gy, int gx)
{
    static int x = 0, y = 0, i = 0;

    /* We just got a new grid */
    if (go)
    {
        auto_grid *ag = &auto_grids[gy][gx];

        /* Default to no rooms */
        i = AUTO_ROOMS;

        /* Paranoia -- no rooms */
        if (!ag->room) return (NULL);

        /* Efficiency -- Single room */
        if (ag->room < AUTO_ROOMS) return (&auto_rooms[ag->room]);

        /* Scan through multiple rooms */
        x = gx; y = gy; i = 0;
    }

    /* Look for a room */
    for (i++; i < auto_room_max; i++)
    {
        /* Access the room */
        auto_room *ar = &auto_rooms[i];

        /* Skip "free" rooms */
        if (ar->free) continue;

        /* If the room contains it, use it */
        if ((x >= ar->x1) && (x <= ar->x2) &&
            (y >= ar->y1) && (y <= ar->y2))
        {
            return (ar);
        }
    }

    /* Default */
    return (NULL);
}






/*
 * Mega-Hack -- clean up the "free room" list
 * First, destroy some of the "fake" rooms
 * Then, attempt to compress the list
 */
bool borg_free_room_update(void)
{
    int x, y, n = 0;

    auto_grid *ag;
    auto_room *ar;


    /* Scan the dungeon */
    for (y = 0; y < AUTO_MAX_Y; y++)
    {
        for (x = 0; x < AUTO_MAX_X; x++)
        {
            /* Extract the "grid" */
            ag = &auto_grids[y][x];

            /* Skip "known" grids */
            if (ag->feat != FEAT_NONE) continue;

            /* Skip "viewable" grids */
            if (ag->info & BORG_VIEW) continue;

            /* Only process "roomed" grids */
            if (!ag->room) continue;

            /* Paranoia -- skip cross rooms */
            if (ag->room >= AUTO_ROOMS) continue;

            /* Get the (solitary) room */
            ar = &auto_rooms[ag->room];

            /* Paranoia -- Skip "big" rooms */
            if ((ar->x1 < x) || (ar->x2 > x)) continue;
            if ((ar->y1 < y) || (ar->y2 > y)) continue;

            /* That room is now "gone" */
            ar->when = 0L;
            ar->x1 = ar->x2 = 0;
            ar->y1 = ar->y2 = 0;

            /* Add it to the "free list" */
            auto_rooms[ag->room].free = auto_rooms[0].free;

            /* Drop the room into the "free list" */
            auto_rooms[0].free = ag->room;

            /* No room here */
            ag->room = 0;

            /* Count changes */
            n++;
        }
    }

    /* Nothing done */
    if (!n) return (FALSE);


    /* Message */
    borg_note(format("# Destroyed %d fake rooms.", n));

    /* Rooms destroyed */
    return (TRUE);
}



/*
 * Mega-Hack -- purge the "free room" list
 * Could be a little more "clever", I suppose
 * For example, first nuke all rooms not in view
 */
static bool borg_free_room_purge(void)
{
    int x, y, n;


    /* Purge every room */
    for (n = 1; n < auto_room_max; n++)
    {
        auto_room *ar = &auto_rooms[n];

        /* That room is now "gone" */
        ar->when = 0L;
        ar->x1 = ar->x2 = 0;
        ar->y1 = ar->y2 = 0;

        /* Reset the "free list" */
        ar->free = n + 1;
    }

    /* Reset the free list */
    auto_room_head->free = 1;

    /* Maximum room index */
    auto_room_max = 1;


    /* Scan the dungeon */
    for (y = 0; y < AUTO_MAX_Y; y++)
    {
        for (x = 0; x < AUTO_MAX_X; x++)
        {
            /* Extract the "grid" */
            auto_grid *ag = &auto_grids[y][x];

            /* No room here */
            ag->room = 0;
        }
    }


    /* Message */
    borg_note(format("# Purged all %d rooms.", n));

    /* Rooms destroyed */
    return (TRUE);
}



/*
 * Grab a room from the free list and return it
 */
auto_room *borg_free_room(void)
{
    int i;

    auto_room *ar;


    /* Running out of free rooms */
    if (auto_room_head->free == auto_room_tail->self)
    {
        /* Message */
        borg_note("# Updating room list!");

        /* Try to free some rooms */
        if (!borg_free_room_update())
        {
            /* Oops */
            borg_note("# Purging room list!");

            /* Try to free some rooms */
            if (!borg_free_room_purge())
            {
                /* Oops */
                borg_oops("broken rooms");

                /* Hack -- Attempt to prevent core dumps */
                return (&auto_rooms[1]);
            }
        }
    }


    /* Acquire a "free" room */
    i = auto_room_head->free;

    /* Access the new room */
    ar = &auto_rooms[i];

    /* Remove the room from the free list */
    auto_room_head->free = ar->free;

    /* Take note of new "maximum" room index */
    if (auto_room_max < i + 1) auto_room_max = i + 1;

    /* The new room is not free */
    ar->free = 0;

    /* Paranoia */
    ar->when = 0L;
    ar->x1 = ar->x1 = 0;
    ar->y1 = ar->y2 = 0;

    /* Return the room */
    return (ar);
}





/*
 * Determine if the given grid is a "snake" grid
 * A "snake" is a "normal" section of corridor, with no
 * bends or intersections.  Only the "center" counts.
 * Note that a "1x1" grid (or "diagonal" grid) is a "snake".
 * A "snake" grid cannot touch "unknown" grids.
 */
static bool borg_build_snake(int y, int x)
{
    auto_grid *ag;

    /* Access the center */
    ag = &auto_grids[y][x];

    /* Central grid must be known */
    if (ag->feat == FEAT_NONE) return (FALSE);

    /* Central grid must be a non wall/door */
    if (!borg_cave_floor_bold(y, x)) return (FALSE);

    /* South/North blockage induces a snake */
    if (!borg_cave_floor_bold(y+1, x) && !borg_cave_floor_bold(y-1, x)) return (TRUE);

    /* East/West blockage induces a snake */
    if (!borg_cave_floor_bold(y, x+1) && !borg_cave_floor_bold(y, x-1)) return (TRUE);

    /* No good */
    return (FALSE);
}



/*
 * Determine if the given "box" in the world is fully made of "floor"
 * Currently, we refuse to accept "unknown" grids as "floor" grids
 */
static bool borg_build_room_floor(int y1, int x1, int y2, int x2)
{
    int x, y;

    /* Check for "floors" */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            auto_room *ar;

            /* Access that grid */
            auto_grid *ag = &auto_grids[y][x];

            /* Refuse to accept walls/doors */
            if (!borg_cave_floor_grid(ag)) return (FALSE);

            /* Refuse to accept unknown grids */
            if (ag->feat == FEAT_NONE) return (FALSE);

            /* Mega-Hack -- Refuse to accept stores */
            if ((ag->feat >= FEAT_SHOP_HEAD) && (ag->feat <= FEAT_SHOP_TAIL)) return (FALSE);

#ifndef CROSS_ROOMS

            /* Hack -- Do not "cross" other (large) rooms */
            for (ar = room(1, y, x); ar; ar = room(0, 0, 0))
            {
                if (ar->x1 != ar->x2) return (FALSE);
                if (ar->y1 != ar->y2) return (FALSE);
            }

#endif

        }
    }

    /* Must be okay */
    return (TRUE);
}


/*
 * Determine if the given "box" in the world is fully "known"
 */
static bool borg_build_room_known(int y1, int x1, int y2, int x2)
{
    int x, y;

    /* Check for "unknown" grids */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Access that grid */
            auto_grid *ag = &auto_grids[y][x];

            /* Refuse to accept unknown grids */
            if (ag->feat == FEAT_NONE) return (FALSE);
        }
    }

    /* Must be okay */
    return (TRUE);
}




/*
 * Attempt to build a "bigger" room for the given location
 * Return TRUE if a "new" room was created, else FALSE
 *
 * Possibly problematic rooms:
 *
 *   #########
 *   ##.....##      Currently this room is parsed as two
 *   #.......#      large overlapping rectangles.  We should
 *   #.......#      probably consider finding the intersection
 *   #.......#      of those rooms, and subtracting out the rest
 *   ##.....##      resulting in four small and one large room.
 *   #########
 *
 * Note that the "room builder" will automatically find the largest
 * rectangle, and will automatically "absorb" enclosed rooms.  Now
 * all we need to do is "notice" when we are "crossing" another room.
 * As long as we do not cross TWO rooms, we should be fine (?).  But
 * observe the following room:
 *
 *   #########
 *   ###...###      Assume that (somehow) the two "horizontal" rooms
 *   ###...###      are constructed first, and then the "vertical"
 *   #.......#      room is discovered.  Now the room crosses TWO
 *   #.......#      other rooms, and we would like to "extract"
 *   ###...###      from this union all NINE resulting rectangles...
 *   ###...###
 *   #.......#      Note that crossing corridors do NOT have this
 *   #.......#      problem, except when "new holes" are made in
 *   ###...###      the middle of existing corridors.  That is
 *   ###...###      something we might consider noticing.
 *   #########
 *
 * Another question to ask is whether we would rather have:
 *   (1) rooms that cross each other
 *   (2) no crossing rooms, but some "touching" rooms
 *   (3) no crossing rooms and no touching rooms
 *
 * Note that #2 seems to allow for the "nicest" treatment of corridors.
 *
 * Note that a room is NEVER built including "unknown" grids.
 *
 * Also note that corridors are handled by a special routine, and rooms
 * are only constructed out of at least a 2x2 block of grids.  Corridors
 * are never allowed to overlap any other rooms.
 *
 * See the "CROSS_ROOM" compile-time define which determine whether the
 * Borg is allowed to generate "crossing" rooms.  This option allows for
 * "better" room generation, but at the cost of efficiency.
 */
bool borg_build_room(int y, int x)
{
    uint i, j;

    int x1, y1, x2, y2;

    auto_grid *ag;
    auto_room *ar;

    bool change = FALSE;


    /* Attempt to expand a 3x3 room */
    if (borg_build_room_floor(y-1, x-1, y+1, x+1))
    {
        x1 = x - 1; y1 = y - 1; x2 = x + 1; y2 = y + 1;
    }

    /* Or attempt to expand a 3x2 room (south) */
    else if (borg_build_room_floor(y, x-1, y+1, x+1))
    {
        x1 = x - 1; y1 = y; x2 = x + 1; y2 = y + 1;
    }

    /* Or attempt to expand a 3x2 room (north) */
    else if (borg_build_room_floor(y-1, x-1, y, x+1))
    {
        x1 = x - 1; y1 = y - 1; x2 = x + 1; y2 = y;
    }

    /* Or attempt to expand a 2x3 room (east) */
    else if (borg_build_room_floor(y-1, x, y+1, x+1))
    {
        x1 = x; y1 = y - 1; x2 = x + 1; y2 = y + 1;
    }

    /* Or attempt to expand a 2x3 room (west) */
    else if (borg_build_room_floor(y-1, x-1, y+1, x))
    {
        x1 = x - 1; y1 = y - 1; x2 = x; y2 = y + 1;
    }

    /* Or attempt to expand a 2x2 room (south east) */
    else if (borg_build_room_floor(y, x, y+1, x+1))
    {
        x1 = x; y1 = y; x2 = x + 1; y2 = y + 1;
    }

    /* Or attempt to expand a 2x2 room (south west) */
    else if (borg_build_room_floor(y, x-1, y+1, x))
    {
        x1 = x - 1; y1 = y; x2 = x; y2 = y + 1;
    }

    /* Or attempt to expand a 2x2 room (north east) */
    else if (borg_build_room_floor(y-1, x, y, x+1))
    {
        x1 = x; y1 = y - 1; x2 = x + 1; y2 = y;
    }

    /* Or attempt to expand a 2x2 room (north west) */
    else if (borg_build_room_floor(y-1, x-1, y, x))
    {
        x1 = x - 1; y1 = y - 1; x2 = x; y2 = y;
    }

    /* Hack -- only "snake" grids can grow corridors */
    else if (!borg_build_snake(y, x))
    {
        x1 = x; y1 = y; x2 = x; y2 = y;
    }

    /* Or attempt to extend a corridor (south) */
    else if (borg_build_snake(y+1, x))
    {
        x1 = x; y1 = y; x2 = x; y2 = y + 1;
    }

    /* Or attempt to extend a corridor (north) */
    else if (borg_build_snake(y-1, x))
    {
        x1 = x; y1 = y - 1; x2 = x; y2 = y;
    }

    /* Or attempt to extend a corridor (east) */
    else if (borg_build_snake(y, x+1))
    {
        x1 = x; y1 = y; x2 = x + 1; y2 = y;
    }

    /* Or attempt to extend a corridor (west) */
    else if (borg_build_snake(y, x-1))
    {
        x1 = x - 1; y1 = y; x2 = x; y2 = y;
    }


    /* Default to 1x1 grid */
    else
    {
        x1 = x; y1 = y; x2 = x; y2 = y;
    }


    /* Hack -- Single grid (1x1) rooms are boring */
    if ((x1 == x2) && (y1 == y2)) return (FALSE);


    /* Expand a north/south corridor */
    if (x1 == x2)
    {
        /* Grow south/north */
        while (borg_build_snake(y2+1, x)) y2++;
        while (borg_build_snake(y1-1, x)) y1--;
    }

    /* Expand a east/west corridor */
    else if (y1 == y2)
    {
        /* Grow east/west */
        while (borg_build_snake(y, x2+1)) x2++;
        while (borg_build_snake(y, x1-1)) x1--;
    }

    /* Expand a rectangle -- try south/north first */
    else if (rand_int(100) < 50)
    {
        /* Grow south/north */
        while (borg_build_room_floor(y2+1, x1, y2+1, x2)) y2++;
        while (borg_build_room_floor(y1-1, x1, y1-1, x2)) y1--;

        /* Grow east/west */
        while (borg_build_room_floor(y1, x2+1, y2, x2+1)) x2++;
        while (borg_build_room_floor(y1, x1-1, y2, x1-1)) x1--;
    }

    /* Expand a rectangle -- try east/west first */
    else
    {
        /* Grow east/west */
        while (borg_build_room_floor(y1, x2+1, y2, x2+1)) x2++;
        while (borg_build_room_floor(y1, x1-1, y2, x1-1)) x1--;

        /* Grow south/north */
        while (borg_build_room_floor(y2+1, x1, y2+1, x2)) y2++;
        while (borg_build_room_floor(y1-1, x1, y1-1, x2)) y1--;
    }


    /* Hack -- refuse to build rooms touching unknowns */
    if (!borg_build_room_known(y2+1, x1-1, y2+1, x2+1)) return (FALSE);
    if (!borg_build_room_known(y1-1, x1-1, y1-1, x2+1)) return (FALSE);
    if (!borg_build_room_known(y1-1, x2+1, y2+1, x2+1)) return (FALSE);
    if (!borg_build_room_known(y1-1, x1-1, y2+1, x1-1)) return (FALSE);


    /* Make sure this room does not exist and is not contained */
    for (ar = room(1, y, x); ar; ar = room(0, 0, 0))
    {
        /* Never make a room "inside" another room */
        if ((ar->x1 <= x1) && (x2 <= ar->x2) &&
            (ar->y1 <= y1) && (y2 <= ar->y2))
        {
            /* The room already exists */
            return (FALSE);
        }
    }


    /* Message */
    borg_note(format("# Room (%d,%d to %d,%d)", x1, y1, x2, y2));

    /* Access a free room */
    ar = borg_free_room();

    /* Initialize the new room */
    ar->x1 = x1; ar->x2 = x2;
    ar->y1 = y1; ar->y2 = y2;

    /* Save the room index */
    i = ar->self;

    /* Absorb old rooms */
    for (j = 1; j < auto_room_max; j++)
    {
        /* Skip the "current" room! */
        if (i == j) continue;

        /* Get the room */
        ar = &auto_rooms[j];

        /* Skip "free" rooms */
        if (ar->free) continue;

        /* Skip non-contained rooms */
        if ((ar->x1 < x1) || (ar->y1 < y1)) continue;
        if ((x2 < ar->x2) || (y2 < ar->y2)) continue;

        /* Scan the "contained" room */
        for (y = ar->y1; y <= ar->y2; y++)
        {
            for (x = ar->x1; x <= ar->x2; x++)
            {
                /* Get the "contained" grid */
                ag = &auto_grids[y][x];

                /* Normal grids "lose" their parents. */
                if (ag->room < AUTO_ROOMS) ag->room = 0;

                /* Cross-rooms lose one parent */
                if (ag->room > AUTO_ROOMS) ag->room--;
            }
        }

        /* That room is now "gone" */
        ar->when = 0L;
        ar->x1 = ar->x2 = 0;
        ar->y1 = ar->y2 = 0;

        /* Add it to the "free list" */
        auto_rooms[j].free = auto_rooms[0].free;
        auto_rooms[0].free = j;
    }


    /* Access the new room */
    ar = &auto_rooms[i];

    /* Scan the grids contained in the new room */
    for (y = ar->y1; y <= ar->y2; y++)
    {
        for (x = ar->x1; x <= ar->x2; x++)
        {
            /* Get the "contained" grid */
            ag = &auto_grids[y][x];

            /* Steal "absorbed" grids */
            if (ag->room == AUTO_ROOMS) ag->room = ar->self;

            /* Steal "fresh" grids */
            if (ag->room == 0) ag->room = ar->self;

            /* Skip grids owned by this room */
            if (ag->room == i) continue;

            /* Normal grids become "cross-grids" (one parent) */
            if (ag->room < AUTO_ROOMS) ag->room = AUTO_ROOMS + 1;

            /* All cross-grids now have another parent */
            ag->room++;
        }
    }

    /* Changed */
    change = TRUE;

    /* Changed */
    return (change);
}



/*
 * Destroy all rooms containing the given location
 */
bool borg_clear_room(int y, int x)
{
    uint xx, yy;

    auto_grid *ag;
    auto_room *ar;

    bool res = FALSE;


    /* Absorb old rooms */
    for (ar = room(1, y, x); ar; ar = room(0, 0, 0))
    {
        /* Scan the "contained" room */
        for (yy = ar->y1; yy <= ar->y2; yy++)
        {
            for (xx = ar->x1; xx <= ar->x2; xx++)
            {
                /* Get the "contained" grid */
                ag = &auto_grids[yy][xx];

                /* Normal grids "lose" their parents. */
                if (ag->room < AUTO_ROOMS) ag->room = 0;

                /* Cross-rooms lose one parent */
                if (ag->room > AUTO_ROOMS) ag->room--;
            }
        }

        /* That room is now "gone" */
        ar->when = 0L;
        ar->x1 = ar->x2 = 0;
        ar->y1 = ar->y2 = 0;

        /* Add the room to the "free list" */
        auto_rooms[ar->self].free = auto_rooms[0].free;

        /* Add the room to the "free list" */
        auto_rooms[0].free = ar->self;
    }


    /* Result */
    return (res);
}




/*
 * Clear out the "room" array
 */
void borg_wipe_rooms(void)
{
    int i, x, y;

    auto_grid *ag;
    auto_room *ar;


    /* Clean up the old used rooms */
    for (i = 1; i < auto_room_max; i++)
    {
        /* Access the room */
        ar = &auto_rooms[i];

        /* Place the room in the free room list */
        ar->free = i + 1;

        /* No location */
        ar->x1 = ar->x2 = ar->y1 = ar->y2 = 0;

        /* Never seen it */
        ar->when = 0L;
    }

    /* Reset the free list */
    auto_room_head->free = 1;

    /* Maximum room index */
    auto_room_max = 1;
}

#endif



/*
 * A simple, fast, integer-based line-of-sight algorithm.
 *
 * See "los()" in "cave.c" for complete documentation
 */
bool borg_los(int y1, int x1, int y2, int x2)
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
    int tx, ty;

    /* Scale factors */
    int f1, f2;

    /* Slope, or 1/Slope, of LOS */
    int m;


    /* Extract the offset */
    dy = y2 - y1;
    dx = x2 - x1;

    /* Extract the absolute offset */
    ay = ABS(dy);
    ax = ABS(dx);


    /* Handle adjacent (or identical) grids */
    if ((ax < 2) && (ay < 2)) return (TRUE);


    /* Paranoia -- require "safe" origin */
    /* if (!in_bounds(y1, x1)) return (FALSE); */


    /* Directly South/North */
    if (!dx)
    {
        /* South -- check for walls */
        if (dy > 0)
        {
            for (ty = y1 + 1; ty < y2; ty++)
            {
                if (!borg_cave_floor_bold(ty, x1)) return (FALSE);
            }
        }

        /* North -- check for walls */
        else
        {
            for (ty = y1 - 1; ty > y2; ty--)
            {
                if (!borg_cave_floor_bold(ty, x1)) return (FALSE);
            }
        }

        /* Assume los */
        return (TRUE);
    }

    /* Directly East/West */
    if (!dy)
    {
        /* East -- check for walls */
        if (dx > 0)
        {
            for (tx = x1 + 1; tx < x2; tx++)
            {
                if (!borg_cave_floor_bold(y1, tx)) return (FALSE);
            }
        }

        /* West -- check for walls */
        else
        {
            for (tx = x1 - 1; tx > x2; tx--)
            {
                if (!borg_cave_floor_bold(y1, tx)) return (FALSE);
            }
        }

        /* Assume los */
        return (TRUE);
    }


    /* Extract some signs */
    sx = (dx < 0) ? -1 : 1;
    sy = (dy < 0) ? -1 : 1;


    /* Vertical "knights" */
    if (ax == 1)
    {
        if (ay == 2)
        {
            if (borg_cave_floor_bold(y1 + sy, x1)) return (TRUE);
        }
    }

    /* Horizontal "knights" */
    else if (ay == 1)
    {
        if (ax == 2)
        {
            if (borg_cave_floor_bold(y1, x1 + sx)) return (TRUE);
        }
    }


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

        tx = x1 + sx;

        /* Consider the special case where slope == 1. */
        if (qy == f2)
        {
            ty = y1 + sy;
            qy -= f1;
        }
        else
        {
            ty = y1;
        }

        /* Note (below) the case (qy == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (x2 - tx)
        {
            if (!borg_cave_floor_bold(ty, tx)) return (FALSE);

            qy += m;

            if (qy < f2)
            {
                tx += sx;
            }
            else if (qy > f2)
            {
                ty += sy;
                if (!borg_cave_floor_bold(ty, tx)) return (FALSE);
                qy -= f1;
                tx += sx;
            }
            else
            {
                ty += sy;
                qy -= f1;
                tx += sx;
            }
        }
    }

    /* Travel vertically */
    else
    {
        /* Let m = dx / dy * 2 * (dx * dy) = 2 * dx * dx */
        qx = ax * ax;
        m = qx << 1;

        ty = y1 + sy;

        if (qx == f2)
        {
            tx = x1 + sx;
            qx -= f1;
        }
        else
        {
            tx = x1;
        }

        /* Note (below) the case (qx == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (y2 - ty)
        {
            if (!borg_cave_floor_bold(ty, tx)) return (FALSE);

            qx += m;

            if (qx < f2)
            {
                ty += sy;
            }
            else if (qx > f2)
            {
                tx += sx;
                if (!borg_cave_floor_bold(ty, tx)) return (FALSE);
                qx -= f1;
                ty += sy;
            }
            else
            {
                tx += sx;
                qx -= f1;
                ty += sy;
            }
        }
    }

    /* Assume los */
    return (TRUE);
}

/*
 * Check the projection from (x1,y1) to (x2,y2).
 * Assume that there is no monster in the way.
 * Hack -- we refuse to assume that unknown grids are floors
 * Adapted from "projectable()" in "spells1.c".
 */
bool borg_projectable(int y1, int x1, int y2, int x2)
{
    int dist, y, x;

    auto_grid *ag;

    /* Start at the initial location */
    y = y1; x = x1;

    /* Simulate the spell/missile path */
    for (dist = 0; dist <= MAX_RANGE; dist++)
    {
        /* Get the grid */
        ag = &auto_grids[y][x];

        if (auto_chp < auto_mhp / 2)
        {
            /* Assume all unknow grids more than distance 10 from you */
            /* are walls--when I am wounded. This will make me more fearful */
            if ((dist > 10) && (ag->feat == FEAT_NONE)) break;
        }
        else
        {
            /* Assume all unknow grids more than distance 3 from you */
            /* are walls. This will make me brave. */
            if ((dist > 2) && (ag->feat == FEAT_NONE)) break;
        }
        /* Never pass through walls/doors */
        if (dist && (!borg_cave_floor_grid(ag))) break;

        /* Check for arrival at "final target" */
        if ((x == x2) && (y == y2)) return (TRUE);

        /* Calculate the new location */
        mmove2(&y, &x, y1, x1, y2, x2);
    }

    /* Assume obstruction */
    return (FALSE);
}


/*
 * Check the projection from (x1,y1) to (x2,y2).
 * Assume that there is no monster in the way.
 * Hack -- we refuse to assume that unknown grids are floors
 * Adapted from "projectable()" in "spells1.c".
 * This is used by borg_offset()
 */
bool borg_offset_projectable(int y1, int x1, int y2, int x2)
{
    int dist, y, x;

    auto_grid *ag;

    /* Start at the initial location */
    y = y1; x = x1;

    /* Simulate the spell/missile path */
    for (dist = 0; dist <= MAX_RANGE; dist++)
    {
        /* Get the grid */
        ag = &auto_grids[y][x];

        /* Assume all unknown grids are walls. */
        if ((dist) && (ag->feat == FEAT_NONE)) break;

        /* Never pass through walls/doors */
        if (dist && (!borg_cave_floor_grid(ag))) break;

        /* Check for arrival at "final target" */
        if ((x == x2) && (y == y2)) return (TRUE);

        /* Calculate the new location */
        mmove2(&y, &x, y1, x1, y2, x2);
    }

    /* Assume obstruction */
    return (FALSE);
}


/*
 * Check the projection from (x1,y1) to (x2,y2).
 * Assume that monsters in the way will stop the projection
 * Hack -- we refuse to assume that unknown grids are floors
 * Adapted from "projectable()" in "spells1.c".
 */
bool borg_projectable_pure(int y1, int x1, int y2, int x2)
{
    int dist, y, x;
    auto_grid *ag;

    /* Start at the initial location */
    y = y1; x = x1;

    /* Simulate the spell/missile path */
    for (dist = 0; dist <= MAX_RANGE; dist++)
    {
        /* Get the grid */
        ag = &auto_grids[y][x];

        /* Hack -- assume unknown grids are walls */
        if (dist && (ag->feat == FEAT_NONE)) break;

        /* Never pass through walls/doors */
        if (dist && (!borg_cave_floor_grid(ag))) break;

        /* Check for arrival at "final target" */
        if ((x == x2) && (y == y2)) return (TRUE);

        /* Stop at monsters */
        if (ag->kill) break;

        /* Calculate the new location */
        mmove2(&y, &x, y1, x1, y2, x2);
    }

    /* Assume obstruction */
    return (FALSE);
}




/*
 * Clear the lite grids
 */
void borg_forget_lite(void)
{
    int i;

    /* None to forget */
    if (!auto_lite_n) return;

    /* Clear them all */
    for (i = 0; i < auto_lite_n; i++)
    {
        int y = auto_lite_y[i];
        int x = auto_lite_x[i];

        /* Forget that the grid is lit */
        auto_grids[y][x].info &= ~BORG_LITE;
    }

    /* None left */
    auto_lite_n = 0;
}



/*
 * XXX XXX XXX
 *
 * This macro allows us to efficiently add a grid to the "lite" array,
 * note that we are never called for illegal grids, or for grids which
 * have already been placed into the "lite" array, and we are never
 * called when the "lite" array is full.
 */
#define borg_cave_lite_hack(Y,X) \
    auto_grids[Y][X].info |= BORG_LITE; \
    auto_lite_y[auto_lite_n] = (Y); \
    auto_lite_x[auto_lite_n] = (X); \
    auto_lite_n++



/*
 * Update the set of grids "illuminated" by the player's lite.
 *
 * See "update_lite" in "cave.c" for complete documentation
 *
 * It is very important that the "player grid" be the first grid in the
 * array of "BORG_LITE" grids, since this is assumed in several places.
 */
void borg_update_lite(void)
{
    int i, x, y, min_x, max_x, min_y, max_y;


    /*** Clear old grids ***/

    /* Clear them all */
    for (i = 0; i < auto_lite_n; i++)
    {
        y = auto_lite_y[i];
        x = auto_lite_x[i];

        /* Mark the grid as not "lite" */
        auto_grids[y][x].info &= ~BORG_LITE;
    }

    /* None left */
    auto_lite_n = 0;

    /* Hack -- Player has no lite */
    if (my_cur_lite <= 0) return;


    /*** Collect the new "lite" grids ***/

    /* Player grid */
    borg_cave_lite_hack(c_y, c_x);

    /* Radius 1 -- torch radius */
    if (my_cur_lite >= 1)
    {
        /* Adjacent grid */
        borg_cave_lite_hack(c_y+1, c_x);
        borg_cave_lite_hack(c_y-1, c_x);
        borg_cave_lite_hack(c_y, c_x+1);
        borg_cave_lite_hack(c_y, c_x-1);

        /* Diagonal grids */
        borg_cave_lite_hack(c_y+1, c_x+1);
        borg_cave_lite_hack(c_y+1, c_x-1);
        borg_cave_lite_hack(c_y-1, c_x+1);
        borg_cave_lite_hack(c_y-1, c_x-1);
    }

    /* Radius 2 -- lantern radius */
    if (my_cur_lite >= 2)
    {
        /* South of the player */
        if (borg_cave_floor_bold(c_y+1, c_x))
        {
            borg_cave_lite_hack(c_y+2, c_x);
            borg_cave_lite_hack(c_y+2, c_x+1);
            borg_cave_lite_hack(c_y+2, c_x-1);
        }

        /* North of the player */
        if (borg_cave_floor_bold(c_y-1, c_x))
        {
            borg_cave_lite_hack(c_y-2, c_x);
            borg_cave_lite_hack(c_y-2, c_x+1);
            borg_cave_lite_hack(c_y-2, c_x-1);
        }

        /* East of the player */
        if (borg_cave_floor_bold(c_y, c_x+1))
        {
            borg_cave_lite_hack(c_y, c_x+2);
            borg_cave_lite_hack(c_y+1, c_x+2);
            borg_cave_lite_hack(c_y-1, c_x+2);
        }

        /* West of the player */
        if (borg_cave_floor_bold(c_y, c_x-1))
        {
            borg_cave_lite_hack(c_y, c_x-2);
            borg_cave_lite_hack(c_y+1, c_x-2);
            borg_cave_lite_hack(c_y-1, c_x-2);
        }
    }

    /* Radius 3+ -- artifact radius */
    if (my_cur_lite >= 3)
    {
        int d, p;

        /* Maximal radius */
        p = my_cur_lite;

        /* Paranoia -- see "LITE_MAX" */
        if (p > 5) p = 5;

        /* South-East of the player */
        if (borg_cave_floor_bold(c_y+1, c_x+1))
        {
            borg_cave_lite_hack(c_y+2, c_x+2);
        }

        /* South-West of the player */
        if (borg_cave_floor_bold(c_y+1, c_x-1))
        {
            borg_cave_lite_hack(c_y+2, c_x-2);
        }

        /* North-East of the player */
        if (borg_cave_floor_bold(c_y-1, c_x+1))
        {
            borg_cave_lite_hack(c_y-2, c_x+2);
        }

        /* North-West of the player */
        if (borg_cave_floor_bold(c_y-1, c_x-1))
        {
            borg_cave_lite_hack(c_y-2, c_x-2);
        }

        /* Maximal north */
        min_y = c_y - p;
        if (min_y < 0) min_y = 0;

        /* Maximal south */
        max_y = c_y + p;
        if (max_y > AUTO_MAX_Y-1) max_y = AUTO_MAX_Y-1;

        /* Maximal west */
        min_x = c_x - p;
        if (min_x < 0) min_x = 0;

        /* Maximal east */
        max_x = c_x + p;
        if (max_x > AUTO_MAX_X-1) max_x = AUTO_MAX_X-1;

        /* Scan the maximal box */
        for (y = min_y; y <= max_y; y++)
        {
            for (x = min_x; x <= max_x; x++)
            {
                int dy = (c_y > y) ? (c_y - y) : (y - c_y);
                int dx = (c_x > x) ? (c_x - x) : (x - c_x);

                /* Skip the "central" grids (above) */
                if ((dy <= 2) && (dx <= 2)) continue;

                /* Hack -- approximate the distance */
                d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

                /* Skip distant grids */
                if (d > p) continue;

                /* Viewable, nearby, grids get "torch lit" */
                if (auto_grids[y][x].info & BORG_VIEW)
                {
                    /* This grid is "torch lit" */
                    borg_cave_lite_hack(y, x);
                }
            }
        }
    }
}





/*
 * Clear the viewable space
 */
void borg_forget_view(void)
{
    int i;

    auto_grid *ag;

    /* None to forget */
    if (!auto_view_n) return;

    /* Clear them all */
    for (i = 0; i < auto_view_n; i++)
    {
        int y = auto_view_y[i];
        int x = auto_view_x[i];

        /* Access the grid */
        ag = &auto_grids[y][x];

        /* Forget that the grid is viewable */
        ag->info &= ~BORG_VIEW;
    }

    /* None left */
    auto_view_n = 0;
}



/*
 * This macro allows us to efficiently add a grid to the "view" array,
 * note that we are never called for illegal grids, or for grids which
 * have already been placed into the "view" array, and we are never
 * called when the "view" array is full.
 */
#define borg_cave_view_hack(A,Y,X) \
    (A)->info |= BORG_VIEW; \
    auto_view_y[auto_view_n] = (Y); \
    auto_view_x[auto_view_n] = (X); \
    auto_view_n++



/*
 * Helper function for "borg_update_view()" below
 *
 * See "update_view_aux()" in "cave.c" for complete documentation.
 */
static bool borg_update_view_aux(int y, int x, int y1, int x1, int y2, int x2)
{
    bool f1, f2, v1, v2, z1, z2, wall;

    auto_grid *ag;

    auto_grid *g1_ag;
    auto_grid *g2_ag;


    /* Access the grids */
    g1_ag = &auto_grids[y1][x1];
    g2_ag = &auto_grids[y2][x2];


    /* Check for walls */
    f1 = (borg_cave_floor_grid(g1_ag));
    f2 = (borg_cave_floor_grid(g2_ag));

    /* Totally blocked by physical walls */
    if (!f1 && !f2) return (TRUE);


    /* Check for visibility */
    v1 = (f1 && (g1_ag->info & BORG_VIEW));
    v2 = (f2 && (g2_ag->info & BORG_VIEW));

    /* Totally blocked by "unviewable neighbors" */
    if (!v1 && !v2) return (TRUE);


    /* Access the grid */
    ag = &auto_grids[y][x];


    /* Check for walls */
    wall = (!borg_cave_floor_grid(ag));


    /* Check the "ease" of visibility */
    z1 = (v1 && (g1_ag->info & BORG_XTRA));
    z2 = (v2 && (g2_ag->info & BORG_XTRA));

    /* Hack -- "easy" plus "easy" yields "easy" */
    if (z1 && z2)
    {
        ag->info |= BORG_XTRA;

        borg_cave_view_hack(ag, y, x);

        return (wall);
    }

    /* Hack -- primary "easy" yields "viewed" */
    if (z1)
    {
        borg_cave_view_hack(ag, y, x);

        return (wall);
    }


    /* Hack -- "view" plus "view" yields "view" */
    if (v1 && v2)
    {
        /* ag->info |= BORG_XTRA; */

        borg_cave_view_hack(ag, y, x);

        return (wall);
    }


    /* Mega-Hack -- the "borg_los()" function works poorly on walls */
    if (wall)
    {
        borg_cave_view_hack(ag, y, x);

        return (wall);
    }


    /* Hack -- check line of sight */
    if (borg_los(c_y, c_x, y, x))
    {
        borg_cave_view_hack(ag, y, x);

        return (wall);
    }


    /* Assume no line of sight. */
    return (TRUE);
}



/*
 * Calculate the region currently "viewable" by the player
 *
 * See "update_view()" in "cave.c" for complete documentation
 *
 * It is very important that the "player grid" be the first grid in the
 * array of "BORG_VIEW" grids, since this is assumed in several places.
 */
void borg_update_view(void)
{
    int n, m, d, k, y, x, z;

    int se, sw, ne, nw, es, en, ws, wn;

    int full, over;

    auto_grid *ag;


    /*** Initialize ***/

    /* Optimize */
    if (view_reduce_lite && !auto_depth)
    {
        /* Full radius (10) */
        full = MAX_SIGHT / 2;

        /* Octagon factor (15) */
        over = MAX_SIGHT * 3 / 4;
    }

    /* Normal */
    else
    {
        /* Full radius (20) */
        full = MAX_SIGHT;

        /* Octagon factor (30) */
        over = MAX_SIGHT * 3 / 2;
    }


    /*** Step 0 -- Begin ***/

    /* Save the old "view" grids for later */
    for (n = 0; n < auto_view_n; n++)
    {
        y = auto_view_y[n];
        x = auto_view_x[n];

        /* Access the grid */
        ag = &auto_grids[y][x];

        /* Mark the grid as not in "view" */
        ag->info &= ~(BORG_VIEW);
    }

    /* Start over with the "view" array */
    auto_view_n = 0;


    /*** Step 1 -- adjacent grids ***/

    /* Now start on the player */
    y = c_y;
    x = c_x;

    /* Access the grid */
    ag = &auto_grids[y][x];

    /* Assume the player grid is easily viewable */
    ag->info |= BORG_XTRA;

    /* Assume the player grid is viewable */
    borg_cave_view_hack(ag, y, x);


    /*** Step 2 -- Major Diagonals ***/

    /* Hack -- Limit */
    z = full * 2 / 3;

    /* Scan south-east */
    for (d = 1; d <= z; d++)
    {
        ag = &auto_grids[y+d][x+d];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y+d, x+d);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Scan south-west */
    for (d = 1; d <= z; d++)
    {
        ag = &auto_grids[y+d][x-d];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y+d, x-d);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Scan north-east */
    for (d = 1; d <= z; d++)
    {
        ag = &auto_grids[y-d][x+d];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y-d, x+d);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Scan north-west */
    for (d = 1; d <= z; d++)
    {
        ag = &auto_grids[y-d][x-d];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y-d, x-d);
        if (!borg_cave_floor_grid(ag)) break;
    }


    /*** Step 3 -- major axes ***/

    /* Scan south */
    for (d = 1; d <= full; d++)
    {
        ag = &auto_grids[y+d][x];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y+d, x);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Initialize the "south strips" */
    se = sw = d;

    /* Scan north */
    for (d = 1; d <= full; d++)
    {
        ag = &auto_grids[y-d][x];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y-d, x);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Initialize the "north strips" */
    ne = nw = d;

    /* Scan east */
    for (d = 1; d <= full; d++)
    {
        ag = &auto_grids[y][x+d];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y, x+d);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Initialize the "east strips" */
    es = en = d;

    /* Scan west */
    for (d = 1; d <= full; d++)
    {
        ag = &auto_grids[y][x-d];
        ag->info |= BORG_XTRA;
        borg_cave_view_hack(ag, y, x-d);
        if (!borg_cave_floor_grid(ag)) break;
    }

    /* Initialize the "west strips" */
    ws = wn = d;


    /*** Step 4 -- Divide each "octant" into "strips" ***/

    /* Now check each "diagonal" (in parallel) */
    for (n = 1; n <= over / 2; n++)
    {
        int ypn, ymn, xpn, xmn;


        /* Acquire the "bounds" of the maximal circle */
        z = over - n - n;
        if (z > full - n) z = full - n;
        while ((z + n + (n>>1)) > full) z--;


        /* Access the four diagonal grids */
        ypn = y + n;
        ymn = y - n;
        xpn = x + n;
        xmn = x - n;


        /* South strip */
        if (ypn < AUTO_MAX_Y-1)
        {
            /* Maximum distance */
            m = MIN(z, (AUTO_MAX_Y-1) - ypn);

            /* East side */
            if ((xpn <= AUTO_MAX_X-1) && (n < se))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ypn+d, xpn, ypn+d-1, xpn-1, ypn+d-1, xpn))
                    {
                        if (n + d >= se) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                se = k + 1;
            }

            /* West side */
            if ((xmn >= 0) && (n < sw))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ypn+d, xmn, ypn+d-1, xmn+1, ypn+d-1, xmn))
                    {
                        if (n + d >= sw) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                sw = k + 1;
            }
        }


        /* North strip */
        if (ymn > 0)
        {
            /* Maximum distance */
            m = MIN(z, ymn);

            /* East side */
            if ((xpn <= AUTO_MAX_X-1) && (n < ne))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ymn-d, xpn, ymn-d+1, xpn-1, ymn-d+1, xpn))
                    {
                        if (n + d >= ne) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                ne = k + 1;
            }

            /* West side */
            if ((xmn >= 0) && (n < nw))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ymn-d, xmn, ymn-d+1, xmn+1, ymn-d+1, xmn))
                    {
                        if (n + d >= nw) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                nw = k + 1;
            }
        }


        /* East strip */
        if (xpn < AUTO_MAX_X-1)
        {
            /* Maximum distance */
            m = MIN(z, (AUTO_MAX_X-1) - xpn);

            /* South side */
            if ((ypn <= AUTO_MAX_Y-1) && (n < es))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ypn, xpn+d, ypn-1, xpn+d-1, ypn, xpn+d-1))
                    {
                        if (n + d >= es) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                es = k + 1;
            }

            /* North side */
            if ((ymn >= 0) && (n < en))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ymn, xpn+d, ymn+1, xpn+d-1, ymn, xpn+d-1))
                    {
                        if (n + d >= en) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                en = k + 1;
            }
        }


        /* West strip */
        if (xmn > 0)
        {
            /* Maximum distance */
            m = MIN(z, xmn);

            /* South side */
            if ((ypn <= AUTO_MAX_Y-1) && (n < ws))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ypn, xmn-d, ypn-1, xmn-d+1, ypn, xmn-d+1))
                    {
                        if (n + d >= ws) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                ws = k + 1;
            }

            /* North side */
            if ((ymn >= 0) && (n < wn))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (borg_update_view_aux(ymn, xmn-d, ymn+1, xmn-d+1, ymn, xmn-d+1))
                    {
                        if (n + d >= wn) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                wn = k + 1;
            }
        }
    }


    /*** Step 5 -- Complete the algorithm ***/

    /* Update all the new grids */
    for (n = 0; n < auto_view_n; n++)
    {
        y = auto_view_y[n];
        x = auto_view_x[n];

        /* Access the grid */
        ag = &auto_grids[y][x];

        /* Clear the "BORG_XTRA" flag */
        ag->info &= ~BORG_XTRA;
    }
}





/*
 * Init this file.
 */
void borg_init_2(void)
{
    /* Nothing */
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
