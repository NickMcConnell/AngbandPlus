/*
 * File: cave.c
 * Purpose: Chunk allocation and utility functions
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


int FEAT_NONE;
int FEAT_FLOOR;
int FEAT_CLOSED;
int FEAT_OPEN;
int FEAT_BROKEN;
int FEAT_LESS;
int FEAT_MORE;
int FEAT_SECRET;
int FEAT_RUBBLE;
int FEAT_PASS_RUBBLE;
int FEAT_MAGMA;
int FEAT_QUARTZ;
int FEAT_MAGMA_K;
int FEAT_QUARTZ_K;
int FEAT_GRANITE;
int FEAT_PERM;
int FEAT_LAVA;
int FEAT_PERM_STATIC;
int FEAT_PERM_HOUSE;
int FEAT_PERM_FAKE;
int FEAT_PERM_ARENA;
int FEAT_WATER;
int FEAT_MUD;
int FEAT_DRAWBRIDGE;
int FEAT_FOUNTAIN;
int FEAT_FNT_DRIED;
int FEAT_LOOSE_DIRT;
int FEAT_DIRT;
int FEAT_FLOOR_SAFE;
int FEAT_LAVA_STREAM;
int FEAT_STREET;
int FEAT_FLOOR_PIT;
int FEAT_GRASS;
int FEAT_CROP;
int FEAT_TREE;
int FEAT_EVIL_TREE;
int FEAT_MOUNTAIN;
int FEAT_LOGS;
int FEAT_SAND;
int FEAT_SANDWALL;
int FEAT_ICE;
int FEAT_ICEWALL;
int FEAT_SWAMP;
int FEAT_TOWN;
int FEAT_DEEP_WATER;
int FEAT_HILL;
int FEAT_SHORE;
int FEAT_PERM_CLEAR;
int FEAT_HOME_OPEN;
int FEAT_HOME_CLOSED;


struct feature *f_info;


/*
 * Global array for looping through the "keypad directions"
 */
s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };


/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]"
 *
 * This means that each entry in this array corresponds to the direction
 * with the same array index in ddd[].
 */
s16b ddx_ddd[9] =
{ 0, 0, 1, -1, 1, -1, 1, -1, 0 };

s16b ddy_ddd[9] =
{ 1, -1, 0, 0, 1, 1, -1, -1, 0 };


/*
 * Hack -- precompute a bunch of calls to distance().
 *
 * The pair of arrays dist_offsets_y[n] and dist_offsets_x[n] contain the
 * offsets of all the locations with a distance of n from a central point,
 * with an offset of (0,0) indicating no more offsets at this distance.
 *
 * This is, of course, fairly unreadable, but it eliminates multiple loops
 * from the previous version.
 *
 * It is probably better to replace these arrays with code to compute
 * the relevant arrays, even if the storage is pre-allocated in hard
 * coded sizes.  At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 */

static const int d_off_y_0[] =
{ 0 };

static const int d_off_x_0[] =
{ 0 };

static const int d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static const int d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };

static const int d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static const int d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };

static const int d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static const int d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };

static const int d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static const int d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };

static const int d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static const int d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };

static const int d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static const int d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };

static const int d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static const int d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };

static const int d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static const int d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };

static const int d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static const int d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };

const int *dist_offsets_y[10] =
{
    d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
    d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

const int *dist_offsets_x[10] =
{
    d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
    d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};


/*
 * Given a central direction at position [dir #][0], return a series
 * of directions radiating out on both sides from the central direction
 * all the way back to its rear.
 *
 * Side directions come in pairs; for example, directions '1' and '3'
 * flank direction '2'. The code should know which side to consider
 * first. If the left, it must add 10 to the central direction to
 * access the second part of the table.
 */
const byte side_dirs[20][8] =
{
    /* bias right */
    {0, 0, 0, 0, 0, 0, 0, 0},
    {1, 4, 2, 7, 3, 8, 6, 9},
    {2, 1, 3, 4, 6, 7, 9, 8},
    {3, 2, 6, 1, 9, 4, 8, 7},
    {4, 7, 1, 8, 2, 9, 3, 6},
    {5, 5, 5, 5, 5, 5, 5, 5},
    {6, 3, 9, 2, 8, 1, 7, 4},
    {7, 8, 4, 9, 1, 6, 2, 3},
    {8, 9, 7, 6, 4, 3, 1, 2},
    {9, 6, 8, 3, 7, 2, 4, 1},

    /* bias left */
    {0, 0, 0, 0, 0, 0, 0, 0},
    {1, 2, 4, 3, 7, 6, 8, 9},
    {2, 3, 1, 6, 4, 9, 7, 8},
    {3, 6, 2, 9, 1, 8, 4, 7},
    {4, 1, 7, 2, 8, 3, 9, 6},
    {5, 5, 5, 5, 5, 5, 5, 5},
    {6, 9, 3, 8, 2, 7, 1, 4},
    {7, 4, 8, 1, 9, 2, 6, 3},
    {8, 7, 9, 4, 6, 1, 3, 2},
    {9, 8, 6, 7, 3, 4, 2, 1}
};


/*
 * Find a terrain feature index by name
 */
int lookup_feat(const char *name)
{
    int i;

    /* Look for it */
    for (i = 0; i < z_info->f_max; i++)
    {
        struct feature *feat = &f_info[i];

        if (!feat->name) continue;

        /* Test for equality */
        if (streq(name, feat->name)) return i;
    }

    quit_fmt("Failed to find terrain feature %s", name);
    return -1;
}


/*
 * Set terrain constants to the indices from terrain.txt
 */
void set_terrain(void)
{
    FEAT_NONE = lookup_feat("unknown grid");
    FEAT_FLOOR = lookup_feat("open floor");
    FEAT_CLOSED = lookup_feat("closed door");
    FEAT_OPEN = lookup_feat("open door");
    FEAT_BROKEN = lookup_feat("broken door");
    FEAT_LESS = lookup_feat("up staircase");
    FEAT_MORE = lookup_feat("down staircase");
    FEAT_SECRET = lookup_feat("secret door");
    FEAT_RUBBLE = lookup_feat("pile of rubble");
    FEAT_PASS_RUBBLE = lookup_feat("pile of passable rubble");
    FEAT_MAGMA = lookup_feat("magma vein");
    FEAT_QUARTZ = lookup_feat("quartz vein");
    FEAT_MAGMA_K = lookup_feat("magma vein with treasure");
    FEAT_QUARTZ_K = lookup_feat("quartz vein with treasure");
    FEAT_GRANITE = lookup_feat("granite wall");
    FEAT_PERM = lookup_feat("permanent wall");
    FEAT_LAVA = lookup_feat("lava");
    FEAT_PERM_STATIC = lookup_feat("static dungeon town wall");
    FEAT_PERM_HOUSE = lookup_feat("house wall");
    FEAT_PERM_FAKE = lookup_feat("fake permanent wall");
    FEAT_PERM_ARENA = lookup_feat("arena wall");
    FEAT_WATER = lookup_feat("water");
    FEAT_MUD = lookup_feat("mud");
    FEAT_DRAWBRIDGE = lookup_feat("drawbridge");
    FEAT_FOUNTAIN = lookup_feat("fountain");
    FEAT_FNT_DRIED = lookup_feat("fountain (dried out)");
    FEAT_LOOSE_DIRT = lookup_feat("loose dirt");
    FEAT_DIRT = lookup_feat("packed dirt");
    FEAT_FLOOR_SAFE = lookup_feat("safe floor");
    FEAT_LAVA_STREAM = lookup_feat("lava stream");
    FEAT_STREET = lookup_feat("street");
    FEAT_FLOOR_PIT = lookup_feat("pit floor");
    FEAT_GRASS = lookup_feat("grass");
    FEAT_CROP = lookup_feat("crops");
    FEAT_TREE = lookup_feat("tree");
    FEAT_EVIL_TREE = lookup_feat("dark tree");
    FEAT_MOUNTAIN = lookup_feat("mountain");
    FEAT_LOGS = lookup_feat("log");
    FEAT_SAND = lookup_feat("sand");
    FEAT_SANDWALL = lookup_feat("sandwall");
    FEAT_ICE = lookup_feat("ice");
    FEAT_ICEWALL = lookup_feat("ice wall");
    FEAT_SWAMP = lookup_feat("swamp");
    FEAT_TOWN = lookup_feat("town");
    FEAT_DEEP_WATER = lookup_feat("deep water");
    FEAT_HILL = lookup_feat("hill");
    FEAT_SHORE = lookup_feat("shore");
    FEAT_PERM_CLEAR = lookup_feat("border permawall");
    FEAT_HOME_OPEN = lookup_feat("open house door");
    FEAT_HOME_CLOSED = lookup_feat("closed house door");
}


/*
 * Allocate a new chunk of the world
 */
struct chunk *cave_new(int height, int width)
{
    int y, x;
    struct chunk *c = mem_zalloc(sizeof(*c));

    c->height = height;
    c->width = width;

    c->feat_count = mem_zalloc(z_info->f_max * sizeof(int));

    c->squares = mem_zalloc(c->height * sizeof(struct square*));
    for (y = 0; y < c->height; y++)
    {
        c->squares[y] = mem_zalloc(c->width * sizeof(struct square));
        for (x = 0; x < c->width; x++)
            c->squares[y][x].info = mem_zalloc(SQUARE_SIZE * sizeof(bitflag));
    }

    c->monsters = mem_zalloc(z_info->level_monster_max * sizeof(struct monster));
    c->mon_max = 1;

    c->o_gen = mem_zalloc(MAX_OBJECTS * sizeof(bool));
    c->join = mem_zalloc(sizeof(struct connector));

    return c;
}


/*
 * Free a chunk
 */
void cave_free(struct chunk *c)
{
    int y, x;

    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            mem_free(c->squares[y][x].info);
            if (c->squares[y][x].trap)
                square_free_trap(c, y, x);
            if (c->squares[y][x].obj)
                object_pile_free(c->squares[y][x].obj);
        }
        mem_free(c->squares[y]);
    }
    mem_free(c->squares);

    mem_free(c->feat_count);
    mem_free(c->monsters);
    mem_free(c->o_gen);
    mem_free(c->join);
    mem_free(c);
}


/*
 * Standard "find me a location" function, now with all legal outputs!
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location.
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 *
 * need_los determines whether line of sight is needed
 */
bool scatter(struct chunk *c, int *yp, int *xp, int y, int x, int d, bool need_los)
{
    int nx, ny;
    int tries = 0;

    /* Pick a location, try many times */
    while (tries < 1000)
    {
        /* Pick a new location */
        ny = rand_spread(y, d);
        nx = rand_spread(x, d);
        tries++;

        /* Ignore annoying locations */
        if (!square_in_bounds_fully(c, ny, nx)) continue;

        /* Ignore "excessively distant" locations */
        if ((d > 1) && (distance(y, x, ny, nx) > d)) continue;

        /* Require "line of sight" if set */
        if (need_los && !los(c, y, x, ny, nx)) continue;

        /* Set the location and return */
        (*yp) = ny;
        (*xp) = nx;
        return true;
    }

    return false;
}


/*
 * Get a monster on the current level by its index.
 */
struct monster *cave_monster(struct chunk *c, int idx)
{
    /* Index MUST be valid */
    my_assert((idx >= 0) && (idx < c->mon_max));

    return &c->monsters[idx];
}


/*
 * The maximum number of monsters allowed in the level.
 */
int cave_monster_max(struct chunk *c)
{
    return c->mon_max;
}


/*
 * The current number of monsters present on the level.
 */
int cave_monster_count(struct chunk *c)
{
    return c->mon_cnt;
}


/*
 * Return the number of doors/traps around (or under) the character.
 */
int count_feats(struct player *p, struct chunk *c, int *y, int *x,
    bool (*test)(struct chunk *c, int y, int x), bool under)
{
    int d;
    int xx, yy;

    /* Count how many matches */
    int count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* If not searching under player continue */
        if ((d == 8) && !under) continue;

        /* Extract adjacent (legal) location */
        yy = p->py + ddy_ddd[d];
        xx = p->px + ddx_ddd[d];

        /* Paranoia */
        if (!square_in_bounds_fully(c, yy, xx)) continue;

        /* Must have knowledge */
        if (!square_isknown(p, yy, xx)) continue;

        /* Not looking for this feature */
        if (!((*test)(c, yy, xx))) continue;

        /* Count it */
        ++count;

        /* Remember the location of the last feature found */
        if (x && y)
        {
            *y = yy;
            *x = xx;
        }
    }

    /* All done */
    return count;
}


/*
 * Update the visuals
 */
void update_visuals(struct worldpos *wpos)
{
    int i;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!COORDS_EQUAL(&p->wpos, wpos)) continue;

        /* Update the visuals */
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }
}


/*
 * Note changes to viewable region
 */
void note_viewable_changes(struct worldpos *wpos, int y, int x)
{
    int i;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!COORDS_EQUAL(&p->wpos, wpos)) continue;

        /* Note changes to viewable region */
        if (square_isview(p, y, x)) p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }
}


/*
 * Fully update the flow
 */
void fully_update_flow(struct worldpos *wpos)
{
    int i;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!COORDS_EQUAL(&p->wpos, wpos)) continue;
    }
}


/*
 * Display the full map of the dungeon in the active Term.
 */
void display_fullmap(struct player *p)
{
    int x, y;
    struct chunk *cv = chunk_get(&p->wpos);

    /* Dump the map */
    for (y = 0; y < z_info->dungeon_hgt; y++)
    {
        /* First clear the old stuff */
        for (x = 0; x < z_info->dungeon_wid; x++)
        {
            p->scr_info[y][x].c = 0;
            p->scr_info[y][x].a = 0;
            p->trn_info[y][x].c = 0;
            p->trn_info[y][x].a = 0;
        }

        /* Scan the columns of row "y" */
        for (x = 0; x < z_info->dungeon_wid; x++)
        {
            u16b a, ta;
            char c, tc;
            struct grid_data g;

            /* Check bounds */
            if (!square_in_bounds(cv, y, x)) continue;

            /* Determine what is there */
            map_info(p, cv, y, x, &g);
            grid_data_as_text(p, cv, false, &g, &a, &c, &ta, &tc);

            p->scr_info[y][x].c = c;
            p->scr_info[y][x].a = a;
            p->trn_info[y][x].c = tc;
            p->trn_info[y][x].a = ta;
        }

        /* Send that line of info */
        Send_fullmap(p, y);
    }

    /* Reset the line counter */
    Send_fullmap(p, -1);
}


/*  
 * Update the cursors for anyone tracking a monster (or player)
 */
void update_cursor(struct source *who)
{
    int i;

    /* Each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);
        struct source *cursor_who = &p->cursor_who;

        /* See if he is tracking this monster (or player) */
        if (source_equal(cursor_who, who))
        {
            /* Redraw */
            p->upkeep->redraw |= (PR_CURSOR);
        }
    }
}


/*
 * Update the health bars for anyone tracking a monster (or player)
 */
void update_health(struct source *who)
{
    int i;

    /* Each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);
        struct source *health_who = &p->upkeep->health_who;

        /* See if he is tracking this monster (or player) */
        if (source_equal_player_or_monster(health_who, who))
        {
            /* Redraw */
            p->upkeep->redraw |= (PR_HEALTH);
        }
    }
}


static void place_feature(struct player *p, struct chunk *c, byte cur_feat)
{
    /* Can only place a staircase once */
    if ((cur_feat == FEAT_LESS) && (c->join->down.y || c->join->down.x))
    {
        msg(p, "There is already an up staircase on this level!");
        return;
    }
    if ((cur_feat == FEAT_MORE) && (c->join->up.y || c->join->up.x))
    {
        msg(p, "There is already a down staircase on this level!");
        return;
    }

    /* Remove a staircase */
    if (square_isupstairs(c, p->py, p->px))
        square_set_join_down(c, 0, 0);
    if (square_isdownstairs(c, p->py, p->px))
        square_set_join_up(c, 0, 0);

    /* Place a staircase */
    if (cur_feat == FEAT_LESS) square_set_upstairs(c, p->py, p->px);
    if (cur_feat == FEAT_MORE) square_set_downstairs(c, p->py, p->px);
}


static void get_rectangle(struct chunk *c, int y0, int x0, int *ymax, int *xmax)
{
    int y, x;

    /* Find the width of the rectangle to fill */
    for (x = x0; x < *xmax; x++)
    {
        /* Require a "clean" floor grid */
        if (!square_canputitem(c, y0, x))
        {
            if (x < *xmax) *xmax = x;
            break;
        }
    }

    /* Find the height of the rectangle to fill */
    for (y = y0; y < *ymax; y++)
    {
        /* Require a "clean" floor grid */
        if (!square_canputitem(c, y, x0))
        {
            if (y < *ymax) *ymax = y;
            break;
        }
    }
}


/*
 * The dungeon master movement hook, called whenever he moves
 * (to make building large buildings / summoning hoards of monsters easier)
 */
void (*master_move_hook)(struct player *p, char *args) = NULL;


static int get_feat_byfuzzyname(char *name)
{
    int i;
    char buf[NORMAL_WID];
    char *str;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    for (i = 0; i < z_info->f_max; i++)
    {
        struct feature *feat = &f_info[i];

        /* Clean up name */
        clean_name(buf, feat->name);

        /* If cleaned name matches our search string, return it */
        if (strstr(buf, name)) return i;
    }

    return -1;
}


/*
 * Build walls and such
 */
void master_build(struct player *p, char *parms)
{
    static byte cur_feat = 0;
    struct chunk *c = chunk_get(&p->wpos);

    /* Paranoia -- make sure the player is on a valid level */
    if (!c) return;

    if (!cur_feat) cur_feat = FEAT_FLOOR;

    /* Place a feature at the player's location */
    if (!parms)
    {
        place_feature(p, c, cur_feat);
        return;
    }

    switch (parms[0])
    {
        /* Set Feature */
        case 'i':
        {
            int feat = get_feat_byfuzzyname(&parms[1]);

            /* Unknown or unauthorized features */
            if (feat == -1) break;
            if (feat_ismetamap(feat)) break;

            cur_feat = (byte)feat;
            break;
        }

        /* Place Feature */
        case 'f':
        {
            place_feature(p, c, cur_feat);
            break;
        }

        /* Draw Line */
        case 'l':
        {
            int dir = (int)parms[1];

            /* No lines of staircases */
            if ((cur_feat == FEAT_LESS) || (cur_feat == FEAT_MORE)) break;

            /* No lines of shops */
            if (feat_is_shop(cur_feat)) break;

            /* No lines of house doors */
            if (feat_ishomedoor(cur_feat)) break;

            /* Draw a line if we have a valid direction */
            if (dir && (dir != 5) && VALID_DIR(dir))
            {
                s16b x = p->px, y = p->py;

                /* Require a "clean" floor grid */
                while (square_canputitem(c, y, x))
                {
                    /* Set feature */
                    square_set_feat(c, y, x, cur_feat);

                    /* Update the visuals */
                    update_visuals(&p->wpos);

                    /* Use the given direction */
                    x += ddx[dir];
                    y += ddy[dir];
                }
            }

            break;
        }

        /* Fill Rectangle */
        case 'r':
        {
            int y = p->py;
            int x = p->px;
            int y_max = c->height - 1;
            int x_max = c->width - 1;

            /* No rectangles of staircases */
            if ((cur_feat == FEAT_LESS) || (cur_feat == FEAT_MORE)) break;

            /* No rectangles of shops */
            if (feat_is_shop(cur_feat)) break;

            /* No rectangles of house doors */
            if (feat_ishomedoor(cur_feat)) break;

            /* Find the width and height of the rectangle to fill */
            while ((y < y_max) && (x < x_max))
            {
                get_rectangle(c, y, x, &y_max, &x_max);
                y++; x++;
            }

            /* Fill rectangle */
            for (y = p->py; y < y_max; y++)
            {
                for (x = p->px; x < x_max; x++)
                {
                    /* Set feature */
                    square_set_feat(c, y, x, cur_feat);

                    /* Update the visuals */
                    update_visuals(&p->wpos);
                }
            }

            break;
        }

        /* Build mode on */
        case 'm':
        {
            master_move_hook = master_build;
            break;
        }

        /* Build mode off */
        case 'x':
        {
            master_move_hook = NULL;
            break;
        }
    }
}


void fill_dirt(struct chunk *c, int y1, int x1, int y2, int x2)
{
    int y, x;

    for (y = y1; y <= y2; y++)
        for (x = x1; x <= x2; x++)
            square_set_feat(c, y, x, FEAT_LOOSE_DIRT);
}


void add_crop(struct chunk *c, int y1, int x1, int y2, int x2, int orientation)
{
    int y, x;

    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Different orientations */
            if ((!orientation && (y % 2)) || (orientation && (x % 2)))
            {
                /* Set to crop */
                square_set_feat(c, y, x, FEAT_CROP);
            }
        }
    }
}


byte add_building(struct chunk *c, int y1, int x1, int y2, int x2, int type)
{
    byte floor_feature = FEAT_FLOOR, wall_feature = 0, door_feature = 0;
    bool lit_room = true;
    int y, x;

    /* Select features */
    switch (type)
    {
        case WILD_LOG_CABIN:
        {
            wall_feature = FEAT_LOGS;
            door_feature = FEAT_CLOSED;
            lit_room = false;

            break;
        }

        case WILD_TOWN_HOME:
        {
            wall_feature = FEAT_PERM_HOUSE;
            door_feature = FEAT_HOME_CLOSED;
            floor_feature = FEAT_FLOOR_SAFE;

            break;
        }

        case WILD_ARENA:
        {
            wall_feature = FEAT_PERM_ARENA;
            door_feature = FEAT_PERM_ARENA;

            break;
        }
    }

    /* Build a rectangular building */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Clear previous contents, add "basic" wall */
            square_set_feat(c, y, x, wall_feature);
        }
    }

    /* Make it hollow */
    for (y = y1 + 1; y < y2; y++)
    {
        for (x = x1 + 1; x < x2; x++)
        {
            /* Fill with floor */
            square_set_feat(c, y, x, floor_feature);

            /* Make it "icky" */
            sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);

            /* Make it glowing */
            if (lit_room)
            {
                sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
            }
        }
    }

    return door_feature;
}


void add_moat(struct chunk *c, int y1, int x1, int y2, int x2, int drawbridge_y[3],
    int drawbridge_x[3])
{
    int y, x;

    /* North / South */
    for (x = x1 - 2; x <= x2 + 2; x++)
    {
        square_set_feat(c, y1 - 2, x, FEAT_WATER);
        sqinfo_on(c->squares[y1 - 2][x].info, SQUARE_VAULT);
        square_set_feat(c, y1 - 3, x, FEAT_WATER);
        sqinfo_on(c->squares[y1 - 3][x].info, SQUARE_VAULT);
        square_set_feat(c, y2 + 2, x, FEAT_WATER);
        sqinfo_on(c->squares[y2 + 2][x].info, SQUARE_VAULT);
        square_set_feat(c, y2 + 3, x, FEAT_WATER);
        sqinfo_on(c->squares[y2 + 3][x].info, SQUARE_VAULT);
    }

    /* East / West */
    for (y = y1 - 2; y <= y2 + 2; y++)
    {
        square_set_feat(c, y, x1 - 2, FEAT_WATER);
        sqinfo_on(c->squares[y][x1 - 2].info, SQUARE_VAULT);
        square_set_feat(c, y, x1 - 3, FEAT_WATER);
        sqinfo_on(c->squares[y][x1 - 3].info, SQUARE_VAULT);
        square_set_feat(c, y, x2 + 2, FEAT_WATER);
        sqinfo_on(c->squares[y][x2 + 2].info, SQUARE_VAULT);
        square_set_feat(c, y, x2 + 3, FEAT_WATER);
        sqinfo_on(c->squares[y][x2 + 3].info, SQUARE_VAULT);
    }
    square_set_feat(c, drawbridge_y[0], drawbridge_x[0], FEAT_DRAWBRIDGE);
    sqinfo_on(c->squares[drawbridge_y[0]][drawbridge_x[0]].info, SQUARE_VAULT);
    square_set_feat(c, drawbridge_y[1], drawbridge_x[1], FEAT_DRAWBRIDGE);
    sqinfo_on(c->squares[drawbridge_y[1]][drawbridge_x[1]].info, SQUARE_VAULT);
    square_set_feat(c, drawbridge_y[2], drawbridge_x[2], FEAT_DRAWBRIDGE);
    sqinfo_on(c->squares[drawbridge_y[2]][drawbridge_x[2]].info, SQUARE_VAULT);
}


/* Player images for graphic mode */
cave_view_type player_presets[MAX_PRESETS][MAX_XTRA_CLASSES][MAX_XTRA_RACES][MAX_SEXES];
cave_view_type player_numbers[MAX_PRESETS][8];
