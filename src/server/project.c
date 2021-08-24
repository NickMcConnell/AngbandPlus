/*
 * File: project.c
 * Purpose: The project() function and helpers
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


struct projection *projections;


/*
 * Specify attr/char pairs for visual special effects for project()
 */
byte proj_to_attr[PROJ_MAX][BOLT_MAX];
char proj_to_char[PROJ_MAX][BOLT_MAX];


/*
 * PROJ type info needed for projections
 *
 * Note that elements come first, so PROJ_ACID == ELEM_ACID, etc
 */
static const char *proj_name_list[] =
{
    #define ELEM(a, b, c, d) #a,
    #include "../common/list-elements.h"
    #undef ELEM
    #define PROJ(a) #a,
    #include "../common/list-projections.h"
    #undef PROJ
    NULL
};


int proj_name_to_idx(const char *name)
{
    int i;

    for (i = 0; proj_name_list[i]; i++)
    {
        if (!my_stricmp(name, proj_name_list[i])) return i;
    }

    return -1;
}


const char *proj_idx_to_name(int type)
{
    my_assert(type >= 0);
    my_assert(type < PROJ_MAX);

    return proj_name_list[type];
}


/*
 * Projection paths
 */


/*
 * Determine the path taken by a projection.
 *
 * The projection will always start from grid1, and will travel
 * towards grid2, touching one grid per unit of distance along
 * the major axis, and stopping when it enters the finish grid or a
 * wall grid, or has travelled the maximum legal distance of "range".
 *
 * Note that "distance" in this function (as in the "update_view()" code)
 * is defined as "MAX(dy,dx) + MIN(dy,dx)/2", which means that the player
 * actually has an "octagon of projection" not a "circle of projection".
 *
 * The path grids are saved into the grid array pointed to by "gp", and
 * there should be room for at least "range" grids in "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * The "flg" flags can be used to modify the behavior of this function.
 *
 * In particular, the "PROJECT_STOP" and "PROJECT_THRU" flags have the same
 * semantics as they do for the "project" function, namely, that the path
 * will stop as soon as it hits a monster, or that the path will continue
 * through the finish grid, respectively.
 *
 * The "PROJECT_JUMP" flag, which for the "project()" function means to
 * start at a special grid (which makes no sense in this function), means
 * that the path should be "angled" slightly if needed to avoid any wall
 * grids, allowing the player to "target" any grid which is in "view".
 *
 * This function returns the number of grids (if any) in the path.  This
 * function will return zero if and only if grid1 and grid2 are equal.
 *
 * This algorithm is similar to, but slightly different from, the one used
 * by "update_view_los()", and very different from the one used by "los()".
 */
int project_path(struct player *p, struct loc *gp, int range, struct chunk *c, struct loc *grid1,
    struct loc *grid2, int flg)
{
    int y, x;
    int n = 0;
    int k = 0;

    /* Absolute */
    int ay, ax;

    /* Offsets */
    int sy, sx;

    /* Fractions */
    int frac;

    /* Scale factors */
    int full, half;

    /* Slope */
    int m;

    /* No path necessary (or allowed) */
    if (loc_eq(grid1, grid2)) return (0);

    /* Analyze "dy" */
    if (grid2->y < grid1->y)
    {
        ay = (grid1->y - grid2->y);
        sy = -1;
    }
    else
    {
        ay = (grid2->y - grid1->y);
        sy = 1;
    }

    /* Analyze "dx" */
    if (grid2->x < grid1->x)
    {
        ax = (grid1->x - grid2->x);
        sx = -1;
    }
    else
    {
        ax = (grid2->x - grid1->x);
        sx = 1;
    }

    /* Number of "units" in one "half" grid */
    half = (ay * ax);

    /* Number of "units" in one "full" grid */
    full = half << 1;

    /* Vertical */
    if (ay > ax)
    {
        /* Start at tile edge */
        frac = ax * ax;

        /* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
        m = frac << 1;

        /* Start */
        y = grid1->y + sy;
        x = grid1->x;

        /* Create the projection path */
        while (1)
        {
            /* Save grid */
            loc_init(&gp[n], x, y);
            n++;

            /* Hack -- check maximum range */
            if ((n + (k >> 1)) >= range) break;

            /* Sometimes stop at finish grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if (loc_eq(&gp[n - 1], grid2)) break;
            }

            /* Stop at non-initial wall grids, except where that would leak info during targeting */
            if (!(flg & (PROJECT_INFO)))
            {
                if ((n > 0) && !square_isprojectable(c, &gp[n - 1])) break;
            }
            else
                if ((n > 0) && square_isbelievedwall(p, c, &gp[n - 1])) break;

            /* Sometimes stop at non-initial targets */
            if (flg & (PROJECT_STOP))
            {
                if ((n > 0) && square(c, &gp[n - 1])->mon) break;
            }

            /* Slant */
            if (m)
            {
                /* Advance (X) part 1 */
                frac += m;

                /* Horizontal change */
                if (frac >= half)
                {
                    /* Advance (X) part 2 */
                    x += sx;

                    /* Advance (X) part 3 */
                    frac -= full;

                    /* Track distance */
                    k++;
                }
            }

            /* Advance (Y) */
            y += sy;
        }
    }

    /* Horizontal */
    else if (ax > ay)
    {
        /* Start at tile edge */
        frac = ay * ay;

        /* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
        m = frac << 1;

        /* Start */
        y = grid1->y;
        x = grid1->x + sx;

        /* Create the projection path */
        while (1)
        {
            /* Save grid */
            loc_init(&gp[n], x, y);
            n++;

            /* Hack -- check maximum range */
            if ((n + (k >> 1)) >= range) break;

            /* Sometimes stop at finish grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if (loc_eq(&gp[n - 1], grid2)) break;
            }

            /* Stop at non-initial wall grids, except where that would leak info during targeting */
            if (!(flg & (PROJECT_INFO)))
            {
                if ((n > 0) && !square_isprojectable(c, &gp[n - 1])) break;
            }
            else
                if ((n > 0) && square_isbelievedwall(p, c, &gp[n - 1])) break;

            /* Sometimes stop at non-initial targets */
            if (flg & (PROJECT_STOP))
            {
                if ((n > 0) && square(c, &gp[n - 1])->mon) break;
            }

            /* Slant */
            if (m)
            {
                /* Advance (Y) part 1 */
                frac += m;

                /* Vertical change */
                if (frac >= half)
                {
                    /* Advance (Y) part 2 */
                    y += sy;

                    /* Advance (Y) part 3 */
                    frac -= full;

                    /* Track distance */
                    k++;
                }
            }

            /* Advance (X) */
            x += sx;
        }
    }

    /* Diagonal */
    else
    {
        /* Start */
        y = grid1->y + sy;
        x = grid1->x + sx;

        /* Create the projection path */
        while (1)
        {
            /* Save grid */
            loc_init(&gp[n], x, y);
            n++;

            /* Hack -- check maximum range */
            if ((n + (n >> 1)) >= range) break;

            /* Sometimes stop at finish grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if (loc_eq(&gp[n - 1], grid2)) break;
            }

            /* Stop at non-initial wall grids, except where that would leak info during targeting */
            if (!(flg & (PROJECT_INFO)))
            {
                if ((n > 0) && !square_isprojectable(c, &gp[n - 1])) break;
            }
            else
                if ((n > 0) && square_isbelievedwall(p, c, &gp[n - 1])) break;

            /* Sometimes stop at non-initial targets */
            if (flg & (PROJECT_STOP))
            {
                if ((n > 0) && square(c, &gp[n - 1])->mon) break;
            }

            /* Advance (Y) */
            y += sy;

            /* Advance (X) */
            x += sx;
        }
    }

    /* Length */
    return (n);
}


/*
 * Determine if a bolt spell cast from grid1 to grid2 will arrive
 * at the final destination, assuming that no monster gets in the way,
 * using the "project_path()" function to check the projection path.
 *
 * Note that no grid is ever "projectable()" from itself.
 *
 * This function is used to determine if the player can (easily) target
 * a given grid.
 *
 * If 'nowall' is false, we allow targets to be in walls, otherwise wraithed players/ghosts
 * would be safe from monster spells!
 */
bool projectable(struct chunk *c, struct loc *grid1, struct loc *grid2, int flg, bool nowall)
{
    struct loc grid_g[512];
    int grid_n = 0;

    /* Check the projection path */
    grid_n = project_path(NULL, grid_g, z_info->max_range, c, grid1, grid2, flg);

    /* No grid is ever projectable from itself */
    if (!grid_n) return false;

    /* May not end in a wall grid */
    if (nowall && !square_ispassable(c, &grid_g[grid_n - 1])) return false;

    /* May not end in an unrequested grid */
    if (!loc_eq(&grid_g[grid_n - 1], grid2)) return false;

    /* Assume okay */
    return true;
}


/*
 * Get a legal "multi-hued" color for drawing "spells"
 */
static byte mh_attr(void)
{
    switch (randint1(9))
    {
        case 1: return (COLOUR_RED);
        case 2: return (COLOUR_GREEN);
        case 3: return (COLOUR_BLUE);
        case 4: return (COLOUR_YELLOW);
        case 5: return (COLOUR_ORANGE);
        case 6: return (COLOUR_VIOLET);
        case 7: return (COLOUR_L_RED);
        case 8: return (COLOUR_L_GREEN);
        case 9: return (COLOUR_L_BLUE);
    }

    return (COLOUR_WHITE);
}


byte proj_color(int type)
{
    if (type < 0 || type >= PROJ_MAX) return COLOUR_WHITE;

    /* Hack -- magic missile and chaos are multihued */
    if ((type == PROJ_MISSILE) || (type == PROJ_CHAOS))
        return mh_attr();

    return projections[type].color;
}


/*
 * The main project() function and its helpers
 */


static bool stop_project(struct source *who, struct loc *grid, struct chunk *cv, int typ)
{
    s16b p1_id, p2_id;
    int m_idx;

    /* Possible decoy */
    struct loc *decoy = cave_find_decoy(cv);

    if (who->monster && loc_eq(decoy, grid)) return true;
    m_idx = square(cv, grid)->mon;
    if (m_idx == 0) return false;

    /* Get caster info */
    if (who->player) p1_id = who->player->id;
    else p1_id = who->monster->master;

    /* Get target info */
    if (m_idx < 0) p2_id = player_get(0 - m_idx)->id;
    else p2_id = cave_monster(cv, m_idx)->master;

    /* Skip the dungeon master */
    if ((m_idx < 0) && is_dm_p(player_get(0 - m_idx)))
    {
        /* Let all spells pass through the dungeon master */
        return false;
    }

    /* Stop at anything that isn't party-friendly */
    if (!master_in_party(p1_id, p2_id)) return true;

    /* Stop if a useful spell hits a friendly player */
    if ((typ == PROJ_PROJECT) && (m_idx < 0))
        return true;

    /* Stop if an order hits a friendly monster */
    if ((typ == PROJ_COMMAND) && (m_idx > 0))
        return true;

    /* Let everything else pass through */
    return false;
}


/*
 * Given an origin, find its coordinates and return them
 *
 * If there is no origin, return (-1, -1)
 */
void origin_get_loc(struct loc *ploc, struct source *origin)
{
    if (origin->monster) loc_copy(ploc, &origin->monster->grid);
    else if (origin->trap) loc_copy(ploc, &origin->trap->grid);
    else if (origin->player) loc_copy(ploc, &origin->player->grid);
    else loc_init(ploc, -1, -1);
}


/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * origin: Origin of the projection
 * rad: Radius of explosion (0 = beam/bolt, 1 to 20 = ball), or maximum
 *      length of arc from the source.
 * cv: Current cave
 * finish: Target location (or location to travel towards)
 * dam: Base damage to apply to monsters, terrain, objects, or players
 * typ: Type of projection (fire, frost, dispel demons etc.)
 * flg: Extra bit flags that control projection behavior
 * degrees_of_arc: How wide an arc spell is (in degrees).
 * diameter_of_source: how wide the source diameter is.
 * what: message to other players if the target dies
 *
 * Returns true if any effects of the projection were observed, else false
 *
 *
 * At present, there are five major types of projections:
 *
 * Point-effect projection: (no PROJECT_BEAM flag, radius of zero, and either
 *   jumps directly to target or has a single source and target grid)
 * A point-effect projection has no line of projection, and only affects one 
 *   grid. It is used for most area-effect spells (like dispel evil) and
 *   pinpoint strikes like the monster Holding prayer.
 * 
 * Bolt: (no PROJECT_BEAM flag, radius of zero, has to travel from source to
 *   target)
 * A bolt travels from source to target and affects only the final grid in its
 *   projection path. If given the PROJECT_STOP flag, it is stopped by any
 *   monster or character in its path (at present, all bolts use this flag).
 *
 * Beam: (PROJECT_BEAM)
 * A beam travels from source to target, affecting all grids passed through 
 *   with full damage. It is never stopped by monsters in its path. Beams
 *   may never be combined with any other projection type.
 *
 * Ball: (positive radius, unless the PROJECT_ARC flag is set)
 * A ball travels from source towards the target, and always explodes. Unless
 *   specified, it does not affect wall grids, but otherwise affects any grids 
 *   in LOS from the center of the explosion.
 * If used with a direction, a ball will explode on the first occupied grid in 
 *   its path. If given a target, it will explode on that target. If a
 *   wall is in the way, it will explode against the wall. If a ball reaches
 *   z_info->max_range without hitting anything or reaching its target, it will
 *   explode at that point.
 *
 * Arc: (positive radius, with the PROJECT_ARC flag set)
 * An arc is a portion of a source-centered ball that explodes outwards 
 *   towards the target grid. Like a ball, it affects all non-wall grids in
 *   LOS of the source in the explosion area. The width of arc spells is con-
 *   trolled by degrees_of_arc.
 * An arc is created by rejecting all grids that form the endpoints of lines 
 *   whose angular difference (in degrees) from the centerline of the arc is 
 *   greater than one-half the input "degrees_of_arc". See the table "get_
 *   angle_to_grid" in "gen-util.c" for more information.
 * Note: An arc with a value for degrees_of_arc of zero is actually a beam of
 *   defined length.
 *
 * Projections that effect all monsters in LOS are handled through the use 
 *   of the PROJECT_LOS effect, which applies a single-grid projection to
 *   individual monsters. Projections that light up rooms or affect all
 *   monsters on the level are more efficiently handled through special
 *   functions.
 *
 * Variations:
 *
 * PROJECT_STOP forces a path of projection to stop at the first occupied grid 
 *   it hits. This is used with bolts, and also by ball spells travelling in
 *   a specific direction rather than towards a target.
 *
 * PROJECT_THRU allows a path of projection towards a target to continue 
 *   past that target. It also allows a spell to affect wall grids adjacent
 *   to a grid in LOS of the center of the explosion.
 * 
 * PROJECT_JUMP allows a projection to immediately set the source of the pro-
 *   jection to the target. This is used for all area effect spells (like
 *   dispel evil), and can also be used for bombardments.
 * 
 * PROJECT_HIDE erases all graphical effects, making the projection invisible.
 *
 * PROJECT_GRID allows projections to affect terrain features.
 *
 * PROJECT_ITEM allows projections to affect objects on the ground.
 *
 * PROJECT_KILL allows projections to affect monsters.
 *
 * PROJECT_PLAY allows projections to affect the player.
 *
 * degrees_of_arc controls the width of arc spells. With a value for
 *   degrees_of_arc of zero, arcs act like beams of defined length.
 *
 * diameter_of_source controls how quickly explosions lose strength with dis-
 *   tance from the target. Most ball spells have a source diameter of 10,
 *   which means that they do 1/2 damage at range 1, 1/3 damage at range 2, 
 *   and so on. Caster-centered balls usually have a source diameter of 20,
 *   which allows them to do full damage to all adjacent grids. Arcs have
 *   source diameters ranging up from 20, which allows the spell designer to
 *   fine-tune how quickly a breath loses strength outwards from the breather.
 *
 * Implementation notes:
 *
 * If the source grid is not the same as the target, we project along the path 
 *   between them. Bolts stop if they hit anything, beams stop if they hit a
 *   wall, and balls and arcs may exhibit either bahavior. When they reach
 *   the final grid in the path, balls and arcs explode. We do not allow
 *   beams to be combined with explosions.
 * Balls affect all floor grids in LOS (optionally, also wall grids adjacent 
 *   to a grid in LOS) within their radius. Arcs do the same, but only within
 *   their cone of projection.
 * Because affected grids are only scanned once, and it is really helpful to 
 *   have explosions that travel outwards from the source, they are sorted by 
 *   distance. For each distance, an adjusted damage is calculated.
 * In successive passes, the code then displays explosion graphics, erases 
 *   these graphics, marks terrain for possible later changes, affects 
 *   objects, monsters, the character, and finally changes features and 
 *   teleports monsters and characters in marked grids.
 *
 * Usage and graphics notes:
 *
 * Only 256 grids can be affected per projection, limiting the effective 
 * radius of standard ball attacks to nine units (diameter nineteen). Arcs
 * can have larger radii; an arc capable of going out to range 20 should not 
 * be wider than 70 degrees.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters on 
 * both sides of a wall. 
 *
 * Note that for consistency, we pretend that the bolt actually takes time
 * to move from point A to point B, even if the player cannot see part of the
 * projection path. Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the illumination of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(struct source *origin, int rad, struct chunk *cv, struct loc *finish, int dam, int typ,
    int flg, int degrees_of_arc, byte diameter_of_source, const char *what)
{
    int i, j, k, dist_from_centre;
    u32b dam_temp;
    struct loc centre;
    struct loc start;
    int n1y = 0;
    int n1x = 0;

    /* Assume the player sees nothing */
    bool notice = false;

    /* Notify the UI if it can draw this projection */
    bool drawing[MAX_PLAYERS];

    /* Number of grids in the "path" */
    int num_path_grids = 0;

    /* Actual grids in the "path" */
    struct loc path_grid[512];

    /* Number of grids in the "blast area" (including the "beam" path) */
    int num_grids = 0;

    /* Coordinates of the affected grids */
    struct loc blast_grid[256];

    /* Distance to each of the affected grids. */
    int distance_to_grid[256];

    /* Precalculated damage values for each distance. */
    int *dam_at_dist = malloc((z_info->max_range + 1) * sizeof(*dam_at_dist));

    /* Assume the player has seen nothing */
    for (i = 0; i < MAX_PLAYERS; ++i) drawing[i] = false;

    /* No projection path - jump to target */
    if (flg & PROJECT_JUMP)
    {
        loc_copy(&start, finish);

        /* Clear the flag */
        flg &= ~(PROJECT_JUMP);
    }
    else
    {
        origin_get_loc(&start, origin);

        /* Default to finish grid */
        if ((start.y == -1) && (start.x == -1))
            loc_copy(&start, finish);
    }

    /* Default center of explosion (if any) */
    loc_copy(&centre, &start);

    /*
     * An arc spell with no width and a non-zero radius is actually a
     * beam of defined length. Mark it as such.
     */
    if ((flg & PROJECT_ARC) && (degrees_of_arc == 0) && (rad != 0))
    {
        /* No longer an arc */
        flg &= ~(PROJECT_ARC);

        /* Now considered a beam */
        flg |= (PROJECT_BEAM);
        flg |= (PROJECT_THRU);
    }

    /*
     * If a single grid is both start and finish (for example
     * if PROJECT_JUMP is set), store it; otherwise, travel along the
     * projection path.
     */
    if (loc_eq(&start, finish))
    {
        loc_copy(&blast_grid[num_grids], finish);
        loc_copy(&centre, finish);
        distance_to_grid[num_grids] = 0;
        sqinfo_on(square(cv, finish)->info, SQUARE_PROJECT);
        num_grids++;
    }
    else
    {
        struct loc grid;

        /* Start from caster */
        loc_copy(&grid, &start);

        /* Calculate the projection path */
        /* Hack -- remove PROJECT_STOP flag to handle friendly targets separately */
        num_path_grids = project_path(NULL, path_grid, z_info->max_range, cv, &start, finish,
            (flg & ~PROJECT_STOP));

        /* Some beams have limited length. */
        if (flg & PROJECT_BEAM)
        {
            /* Use length limit, if any is given. */
            if ((rad > 0) && (rad < num_path_grids))
                num_path_grids = rad;
        }

        /* Project along the path (except for arcs) */
        if (!(flg & PROJECT_ARC))
        {
            for (i = 0; i < num_path_grids; ++i)
            {
                bool collected = false;
                struct loc ogrid;

                /* PWMAngband: BALL attacks should also be applied to wraithed players */
                bool proj_wall = (origin->target && loc_eq(&origin->target->grid, &path_grid[i]));

                /* Hack -- balls explode before reaching walls */
                if (!square_ispassable(cv, &path_grid[i]) && (rad > 0) && !proj_wall) break;

                /* Advance */
                loc_copy(&ogrid, &grid);
                loc_copy(&grid, &path_grid[i]);

                /*
                 * Beams collect all grids in the path, all other methods
                 * collect only the final grid in the path.
                 */
                if (flg & PROJECT_BEAM)
                {
                    loc_copy(&blast_grid[num_grids], &grid);
                    distance_to_grid[num_grids] = 0;
                    sqinfo_on(square(cv, &grid)->info, SQUARE_PROJECT);
                    num_grids++;
                    collected = true;
                }
                else if (i == num_path_grids - 1)
                {
                    loc_copy(&blast_grid[num_grids], &grid);
                    distance_to_grid[num_grids] = 0;
                    sqinfo_on(square(cv, &grid)->info, SQUARE_PROJECT);
                    num_grids++;
                    collected = true;
                }

                /* Only do visuals if requested and within range limit. */
                if (!(flg & PROJECT_HIDE))
                {
                    struct bolt data;

                    data.proj_type = typ;
                    data.beam = ((flg & PROJECT_BEAM)? true: false);
                    loc_copy(&data.ogrid, &ogrid);
                    loc_copy(&data.grid, &grid);

                    /* Tell the UI to display the bolt */
                    display_bolt(cv, &data, drawing);
                }

                /* Sometimes stop at non-initial monsters/players */
                if ((flg & PROJECT_STOP) && stop_project(origin, &grid, cv, typ))
                {
                    /* Store the grid if necessary */
                    if (!collected)
                    {
                        loc_copy(&blast_grid[num_grids], &grid);
                        distance_to_grid[num_grids] = 0;
                        sqinfo_on(square(cv, &grid)->info, SQUARE_PROJECT);
                        num_grids++;
                    }

                    break;
                }
            }
        }

        /* Save the "blast epicenter" */
        loc_copy(&centre, &grid);
    }

    /*
     * Now check for explosions. Beams have already stored all the grids they
     * will affect; all non-beam projections with positive radius explode in
     * some way
     */
    if ((rad > 0) && !(flg & PROJECT_BEAM))
    {
        struct loc begin, end;
        struct loc_iterator iter;

        /* Pre-calculate some things for arcs. */
        if ((flg & PROJECT_ARC) && (num_path_grids != 0))
        {
            /* Explosion centers on the caster. */
            loc_copy(&centre, &start);

            /* The radius of arcs cannot be more than 20 */
            if (rad > 20) rad = 20;

            /* Ensure legal access into get_angle_to_grid table */
            if (num_path_grids < 21)
                i = num_path_grids - 1;
            else
                i = 20;

            /* Reorient the grid forming the end of the arc's centerline. */
            n1y = path_grid[i].y - centre.y + 20;
            n1x = path_grid[i].x - centre.x + 20;
        }

        /* If the center of the explosion hasn't been saved already, save it now. */
        if (num_grids == 0)
        {
            loc_copy(&blast_grid[num_grids], &centre);
            distance_to_grid[num_grids] = 0;
            sqinfo_on(square(cv, &centre)->info, SQUARE_PROJECT);
            num_grids++;
        }

        loc_init(&begin, centre.x - rad, centre.y - rad);
        loc_init(&end, centre.x + rad, centre.y + rad);
        loc_iterator_first(&iter, &begin, &end);

        /* Scan every grid that might possibly be in the blast radius. */
        do
        {
            /* PWMAngband: BREATH attacks should also be applied to wraithed players */
            bool proj_wall = (origin->target && loc_eq(&origin->target->grid, &iter.cur));

            /* Center grid has already been stored. */
            if (loc_eq(&iter.cur, &centre)) continue;

            /* Precaution: Stay within area limit. */
            if (num_grids >= 255) break;

            /* Ignore "illegal" locations */
            if (!square_in_bounds(cv, &iter.cur)) continue;

            /*
             * Most explosions are immediately stopped by walls. If
             * PROJECT_THRU is set, walls can be affected if adjacent to
             * a grid visible from the explosion centre.
             * All explosions can affect one layer of terrain which is
             * passable but not projectable.
             */
            if ((flg & PROJECT_THRU) || square_ispassable(cv, &iter.cur) || proj_wall)
            {
                /* If this is a wall grid, ... */
                if (!square_isprojectable(cv, &iter.cur))
                {
                    /* Check neighbors */
                    for (i = 0, k = 0; i < 8; i++)
                    {
                        struct loc ngrid;

                        loc_sum(&ngrid, &iter.cur, &ddgrid_ddd[i]);
                        if (los(cv, &centre, &ngrid))
                        {
                            k++;
                            break;
                        }
                    }

                    /* Require at least one adjacent grid in LOS. */
                    if (!k) continue;
                }
            }
            else if (!square_isprojectable(cv, &iter.cur))
                continue;
            /*if (!los(cv, &centre, &iter.cur)) continue;*/

            /* Must be within maximum distance. */
            dist_from_centre = distance(&centre, &iter.cur);
            if (dist_from_centre > rad) continue;

            /* Do we need to consider a restricted angle? */
            if (flg & PROJECT_ARC)
            {
                /* Use angle comparison to delineate an arc. */
                int n2y, n2x, tmp, rotate, diff;

                /* Reorient current grid for table access. */
                n2y = iter.cur.y - start.y + 20;
                n2x = iter.cur.x - start.x + 20;

                /*
                 * Find the angular difference (/2) between
                 * the lines to the end of the arc's center-
                 * line and to the current grid.
                 */
                rotate = 90 - get_angle_to_grid[n1y][n1x];
                tmp = ABS(get_angle_to_grid[n2y][n2x] + rotate) % 180;
                diff = ABS(90 - tmp);

                /*
                 * If difference is not greater then that
                 * allowed, and the grid is in LOS, accept it.
                 */
                if (diff < (degrees_of_arc + 6) / 4)
                {
                    if (los(cv, &centre, &iter.cur))
                    {
                        loc_copy(&blast_grid[num_grids], &iter.cur);
                        distance_to_grid[num_grids] = dist_from_centre;
                        sqinfo_on(square(cv, &iter.cur)->info, SQUARE_PROJECT);
                        num_grids++;
                    }
                }
            }

            /* Accept all grids in LOS */
            else if (los(cv, &centre, &iter.cur))
            {
                loc_copy(&blast_grid[num_grids], &iter.cur);
                distance_to_grid[num_grids] = dist_from_centre;
                sqinfo_on(square(cv, &iter.cur)->info, SQUARE_PROJECT);
                num_grids++;
            }
        }
        while (loc_iterator_next(&iter));
    }

    /* Calculate and store the actual damage at each distance. */
    for (i = 0; i <= z_info->max_range; i++)
    {
        /* No damage outside the radius. */
        if (i > rad)
            dam_temp = 0;

        /* Effect is constant */
        else if (flg & PROJECT_CONST)
            dam_temp = dam;

        /* Standard damage calc. for 10' source diameters, or at origin. */
        else if (!diameter_of_source || (i == 0))
            dam_temp = (dam + i) / (i + 1);

        /*
         * If a particular diameter for the source of the explosion's energy is
         * given, it is full strength to that diameter and then reduces.
         */
        else
        {
            dam_temp = (diameter_of_source * dam) / ((i + 1) * 10);
            if (dam_temp > (u32b)dam) dam_temp = dam;
        }

        /* Store it. */
        dam_at_dist[i] = dam_temp;
    }

    /* Sort the blast grids by distance from the centre. */
    for (i = 0, k = 0; i <= rad; i++)
    {
        /* Collect all the grids of a given distance together. */
        for (j = k; j < num_grids; j++)
        {
            if (distance_to_grid[j] == i)
            {
                struct loc tmp;
                int tmp_d;

                loc_copy(&tmp, &blast_grid[k]);
                tmp_d = distance_to_grid[k];

                loc_copy(&blast_grid[k], &blast_grid[j]);
                distance_to_grid[k] = distance_to_grid[j];

                loc_copy(&blast_grid[j], &tmp);
                distance_to_grid[j] = tmp_d;

                /* Write to next slot */
                k++;
            }
        }
    }

    /* Display the blast area if allowed. */
    if (!(flg & PROJECT_HIDE))
    {
        struct explosion data;

        data.proj_type = typ;
        data.num_grids = num_grids;
        data.distance_to_grid = distance_to_grid;
        data.blast_grid = blast_grid;

        /* Tell the UI to display the blast */
        display_explosion(cv, &data, drawing, ((flg & PROJECT_ARC)? true: false));
    }

    /* Hack -- count how many projections we have seen */
    for (j = 1; j <= NumPlayers; j++)
    {
        struct player *p = player_get(j);

        /* Skip irrelevant players */
        if (!wpos_eq(&p->wpos, &cv->wpos)) continue;
        if (p->timed[TMD_BLIND]) continue;

        /* Add one to the count */
        if (drawing[j]) p->did_visuals = true;
    }

    /* Affect objects on every relevant grid */
    if (flg & PROJECT_ITEM)
    {
        for (i = 0; i < num_grids; i++)
        {
            if (project_o(origin, distance_to_grid[i], cv, &blast_grid[i],
                dam_at_dist[distance_to_grid[i]], typ))
            {
                notice = true;
            }
        }
    }

    /* Check monsters */
    if (flg & PROJECT_KILL)
    {
        bool was_obvious = false;
        bool did_hit = false;
        int num_hit = 0;
        struct loc last_hit;

        loc_init(&last_hit, 0, 0);

        /* Scan for monsters */
        for (i = 0; i < num_grids; i++)
        {
            struct loc grid;

            /* Check this monster hasn't been processed already */
            if (!square_isproject(cv, &blast_grid[i])) continue;

            /* Affect the monster in the grid */
            project_m(origin, distance_to_grid[i], cv, &blast_grid[i],
                dam_at_dist[distance_to_grid[i]], typ, flg, &did_hit, &was_obvious, &grid.y, &grid.x);
            if (was_obvious) notice = true;
            if (did_hit)
            {
                num_hit++;

                /* Monster location may have been updated by project_m() */
                loc_copy(&last_hit, &grid);
            }
        }

        /* Player affected one monster (without "jumping") */
        if (origin->player && (num_hit == 1) && !(flg & (PROJECT_JUMP)))
        {
            /* Location */
            int m_idx = square(cv, &last_hit)->mon;

            /* Track if possible */
            if (m_idx > 0)
            {
                struct source mon_body;
                struct source *mon = &mon_body;

                source_monster(mon, cave_monster(cv, m_idx));

                /* Recall and track */
                if (monster_is_visible(origin->player, m_idx))
                {
                    monster_race_track(origin->player->upkeep, mon);
                    health_track(origin->player->upkeep, mon);
                }
            }
        }
    }

    /* Look for players, affect them when found */
    if (flg & PROJECT_PLAY)
    {
        bool was_obvious = false;
        bool did_hit = false;
        int num_hit = 0;
        struct loc last_hit;

        loc_init(&last_hit, 0, 0);

        for (i = 0; i < num_grids; i++)
        {
            struct loc grid;

            /* Check this player hasn't been processed already */
            if (!square_isproject(cv, &blast_grid[i])) continue;

            /* Affect the player in the grid */
            project_p(origin, distance_to_grid[i], cv, &blast_grid[i],
                dam_at_dist[distance_to_grid[i]], typ, what, &did_hit, &was_obvious, &grid.y, &grid.x);
            if (was_obvious) notice = true;
            if (did_hit)
            {
                num_hit++;

                /* Player location may have been updated by project_p() */
                loc_copy(&last_hit, &grid);
            }
        }

        /* Player affected one player (without "jumping") */
        if (origin->player && (num_hit == 1) && !(flg & (PROJECT_JUMP)))
        {
            /* Location */
            int m_idx = square(cv, &last_hit)->mon;

            /* Track if possible */
            if (m_idx < 0)
            {
                struct source act_body;
                struct source *p_act = &act_body;

                source_player(p_act, 0 - m_idx, player_get(0 - m_idx));

                /* Hack -- auto-track */
                if (player_is_visible(origin->player, 0 - m_idx))
                    health_track(origin->player->upkeep, p_act);
            }
        }
    }

    /* Affect features in every relevant grid */
    if (flg & PROJECT_GRID)
    {
        for (i = 0; i < num_grids; i++)
        {
            if (project_f(origin, distance_to_grid[i], cv, &blast_grid[i],
                dam_at_dist[distance_to_grid[i]], typ))
            {
                notice = true;
            }
        }
    }

    /* Clear all the processing marks. */
    for (i = 0; i < num_grids; i++)
    {
        /* Clear the mark */
        sqinfo_off(square(cv, &blast_grid[i])->info, SQUARE_PROJECT);
    }

    free(dam_at_dist);

    /* Return "something was noticed" */
    return (notice);
}
