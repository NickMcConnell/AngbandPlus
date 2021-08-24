/*
 * File: cave-square.c
 * Purpose: Functions for dealing with individual squares
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
 * FEATURE PREDICATES
 *
 * These functions test a terrain feature index for the obviously described
 * type. They are used in the square feature predicates below, and
 * occasionally on their own
 */


/*
 * True if the square is a magma wall.
 */
bool feat_is_magma(int feat)
{
    return tf_has(f_info[feat].flags, TF_MAGMA);
}


/*
 * True if the square is a quartz wall.
 */
bool feat_is_quartz(int feat)
{
    return tf_has(f_info[feat].flags, TF_QUARTZ);
}


/*
 * True if the square is a mineral wall with treasure (magma/quartz).
 */
bool feat_is_treasure(int feat)
{
    return (tf_has(f_info[feat].flags, TF_GOLD) && tf_has(f_info[feat].flags, TF_INTERESTING));
}


/*
 * True is the feature is a solid wall (not rubble).
 */
bool feat_is_wall(int feat)
{
    return tf_has(f_info[feat].flags, TF_WALL);
}


/*
 * True is the feature is a floor.
 */
bool feat_is_floor(int feat)
{
    return tf_has(f_info[feat].flags, TF_FLOOR);
}


/*
 * True is the feature can hold a trap.
 */
bool feat_is_trap_holding(int feat)
{
    return tf_has(f_info[feat].flags, TF_TRAP);
}


/*
 * True is the feature can hold an object.
 */
bool feat_is_object_holding(int feat)
{
    return tf_has(f_info[feat].flags, TF_OBJECT);
}


/*
 * True if a monster can walk through the feature.
 */
bool feat_is_monster_walkable(int feat)
{
    return tf_has(f_info[feat].flags, TF_PASSABLE);
}


/*
 * True if the feature is a shop entrance.
 */
bool feat_is_shop(int feat)
{
    return tf_has(f_info[feat].flags, TF_SHOP);
}


/*
 * True if the feature is a vendor.
 */
bool feat_is_vendor(int feat)
{
    return tf_has(f_info[feat].flags, TF_VENDOR);
}


/*
 * True if the feature is passable by the player.
 */
bool feat_is_passable(int feat)
{
    return tf_has(f_info[feat].flags, TF_PASSABLE);
}


/*
 * True if any projectable can pass through the feature.
 */
bool feat_is_projectable(int feat)
{
    return tf_has(f_info[feat].flags, TF_PROJECT);
}


/*
 * True if the feature can be lit by light sources.
 */
bool feat_is_torch(int feat)
{
    return tf_has(f_info[feat].flags, TF_TORCH);
}


/*
 * True if the feature is internally lit.
 */
bool feat_is_bright(int feat)
{
    return tf_has(f_info[feat].flags, TF_BRIGHT);
}


/*
 * True if the feature is fire-based.
 */
bool feat_is_fiery(int feat)
{
    return tf_has(f_info[feat].flags, TF_FIERY);
}


/*
 * True if the feature doesn't carry monster flow information.
 */
bool feat_is_no_flow(int feat)
{
    return tf_has(f_info[feat].flags, TF_NO_FLOW);
}


/*
 * True if the feature doesn't carry player scent.
 */
bool feat_is_no_scent(int feat)
{
    return tf_has(f_info[feat].flags, TF_NO_SCENT);
}


/*
 * True if the feature should have smooth boundaries (for dungeon generation).
 */
bool feat_is_smooth(int feat)
{
    return tf_has(f_info[feat].flags, TF_SMOOTH);
}


bool feat_issafefloor(int feat)
{
    return tf_has(f_info[feat].flags, TF_FLOOR_SAFE);
}


bool feat_isterrain(int feat)
{
    return !tf_has(f_info[feat].flags, TF_FLOOR);
}


bool feat_isprefixed(int feat)
{
    return (tf_has(f_info[feat].flags, TF_DOOR_CLOSED) || tf_has(f_info[feat].flags, TF_ROCK) ||
        tf_has(f_info[feat].flags, TF_PREFIXED));
}


int feat_order_special(int feat)
{
    if ((feat == FEAT_WATER) || (feat == FEAT_MUD) || (feat == FEAT_DRAWBRIDGE) ||
        (feat == FEAT_LOOSE_DIRT) || (feat == FEAT_CROP) || (feat == FEAT_MOUNTAIN))
    {
        return 0;
    }

    if ((feat == FEAT_TREE) || (feat == FEAT_EVIL_TREE))
        return 5;

    if ((feat == FEAT_SWAMP) || (feat == FEAT_DEEP_WATER) || (feat == FEAT_HILL) ||
        (feat == FEAT_SHORE))
    {
        return 7;
    }

    return -1;
}


int feat_pseudo(char d_char)
{
    switch (d_char)
    {
        case '+': return FEAT_CLOSED;
        case '<': return FEAT_LESS;
        case '>': return FEAT_MORE;
    }

    return FEAT_FLOOR;
}


bool feat_ishomedoor(int feat)
{
    return tf_has(f_info[feat].flags, TF_DOOR_HOME);
}


bool feat_isperm(int feat)
{
    return (tf_has(f_info[feat].flags, TF_PERMANENT) && tf_has(f_info[feat].flags, TF_ROCK));
}


bool feat_ismetamap(int feat)
{
    return tf_has(f_info[feat].flags, TF_METAMAP);
}


/*
 * SQUARE FEATURE PREDICATES
 *
 * These functions are used to figure out what kind of square something is,
 * via c->squares[y][x].feat (preferably accessed via square(c, grid)).
 * All direct testing of square(c, grid).feat should be rewritten
 * in terms of these functions.
 *
 * It's often better to use square behavior predicates (written in terms of
 * these functions) instead of these functions directly. For instance,
 * square_isrock() will return false for a secret door, even though it will
 * behave like a rock wall until the player determines it's a door.
 *
 * Use functions like square_isdiggable, square_iswall, etc. in these cases.
 */


/*
 * True if the square is normal open floor.
 */
bool square_isfloor(struct chunk *c, struct loc *grid)
{
    return feat_is_floor(square(c, grid)->feat);
}


/*
 * True if the square is safe floor.
 */
bool square_issafefloor(struct chunk *c, struct loc *grid)
{
    return feat_issafefloor(square(c, grid)->feat);
}


/*
 * True if the square is pit floor.
 */
bool square_ispitfloor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_FLOOR) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_PIT));
}


/*
 * True if the square is other floor.
 */
bool square_isotherfloor(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_FLOOR_OTHER);
}


bool square_isanyfloor(struct chunk *c, struct loc *grid)
{
    return (square_isfloor(c, grid) || square_issafefloor(c, grid) || square_isotherfloor(c, grid));
}


/*
 * True if the square can hold a trap.
 */
bool square_istrappable(struct chunk *c, struct loc *grid)
{
    return feat_is_trap_holding(square(c, grid)->feat);
}


/*
 * True if the square can hold an object.
 */
bool square_isobjectholding(struct chunk *c, struct loc *grid)
{
    return feat_is_object_holding(square(c, grid)->feat);
}


/*
 * True if the square is a normal granite rock wall.
 */
bool square_isrock(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_GRANITE) &&
        !tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_ANY));
}


/*
 * True if the square is a permanent wall.
 */
bool square_isperm(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_PERMANENT) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_ROCK));
}


bool square_isborder(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_BORDER);
}


bool square_ispermarena(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_ARENA);
}


bool square_ispermhouse(struct chunk *c, struct loc *grid)
{
    return (square(c, grid)->feat == FEAT_PERM_HOUSE);
}


bool square_ispermstatic(struct chunk *c, struct loc *grid)
{
    return (square(c, grid)->feat == FEAT_PERM_STATIC);
}


bool square_ispermfake(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_PERMANENT) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_PIT));
}


/*
 * True if the square is a magma wall.
 */
bool square_ismagma(struct chunk *c, struct loc *grid)
{
    return feat_is_magma(square(c, grid)->feat);
}


/*
 * True if the square is a quartz wall.
 */
bool square_isquartz(struct chunk *c, struct loc *grid)
{
    return feat_is_quartz(square(c, grid)->feat);
}


/*
 * True if the square is a mineral wall (other)
 */
bool square_ismineral_other(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_SAND) ||
        tf_has(f_info[square(c, grid)->feat].flags, TF_ICE) ||
        tf_has(f_info[square(c, grid)->feat].flags, TF_DARK));
}


/*
 * True if the square is a mineral wall.
 */
bool square_ismineral(struct chunk *c, struct loc *grid)
{
    return (square_isrock(c, grid) || square_ismagma(c, grid) || square_isquartz(c, grid) ||
        square_ismineral_other(c, grid));
}


bool square_hasgoldvein(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_GOLD);
}


bool square_hasgoldvein_p(struct player *p, struct loc *grid)
{
    return tf_has(f_info[square_p(p, grid)->feat].flags, TF_GOLD);
}


/*
 * True if the square is rubble.
 */
bool square_isrubble(struct chunk *c, struct loc *grid)
{
    return (!tf_has(f_info[square(c, grid)->feat].flags, TF_WALL) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_ROCK));
}


/*
 * True if the square is a hidden secret door.
 *
 * These squares appear as if they were granite -- when detected a secret door
 * is replaced by a closed door.
 */
bool square_issecretdoor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_ANY) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_ROCK));
}


/*
 * True if the square is an open door.
 */
bool square_isopendoor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_CLOSABLE));
}


bool square_home_isopendoor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_HOME) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_CLOSABLE));
}


/*
 * True if the square is a closed door.
 */
bool square_iscloseddoor(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_CLOSED);
}


bool square_basic_iscloseddoor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_CLOSED) &&
        !tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_HOME));
}


bool square_home_iscloseddoor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_CLOSED) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_HOME));
}


bool square_isbrokendoor(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_ANY) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_PASSABLE) &&
        !tf_has(f_info[square(c, grid)->feat].flags, TF_CLOSABLE));
}


/*
 * True if the square is a door.
 *
 * This includes open, closed, broken, and hidden doors.
 */
bool square_isdoor(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_DOOR_ANY);
}


/*
 * True if the square is a door.
 *
 * This includes open, closed, broken, and hidden doors.
 */
bool square_isdoor_p(struct player *p, struct loc *grid)
{
    return tf_has(f_info[square_p(p, grid)->feat].flags, TF_DOOR_ANY);
}


/*
 * True if cave is an up or down stair
 */
bool square_isstairs(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_STAIR);
}


/*
 * True if cave is an up or down stair
 */
bool square_isstairs_p(struct player *p, struct loc *grid)
{
    return tf_has(f_info[square_p(p, grid)->feat].flags, TF_STAIR);
}


/*
 * True if cave is an up stair.
 */
bool square_isupstairs(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_UPSTAIR);
}


/*
 * True if cave is a down stair.
 */
bool square_isdownstairs(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_DOWNSTAIR);
}


/*
 * True if the square is a shop entrance.
 */
bool square_isshop(struct chunk *c, struct loc *grid)
{
    return feat_is_shop(square(c, grid)->feat);
}


bool square_noticeable(struct chunk *c, struct loc *grid)
{
    if (square_isvisibletrap(c, grid)) return true;

    return (tf_has(f_info[square(c, grid)->feat].flags, TF_INTERESTING) ||
        tf_has(f_info[square(c, grid)->feat].flags, TF_NOTICEABLE));
}


/*
 * True if the square contains a player
 */
bool square_isplayer(struct chunk *c, struct loc *grid)
{
    return ((square(c, grid)->mon < 0)? true: false);
}


/*
 * True if the square contains a player or a monster
 */
bool square_isoccupied(struct chunk *c, struct loc *grid)
{
    return ((square(c, grid)->mon != 0)? true: false);
}


/*
 * True if the player knows the terrain of the square
 */
bool square_isknown(struct player *p, struct loc *grid)
{
    return ((square_p(p, grid)->feat != FEAT_NONE) || (p->dm_flags & DM_SEE_LEVEL));
}


/*
 * True if the the player's knowledge of the terrain of the square is wrong
 * or missing
 */
bool square_isnotknown(struct player *p, struct chunk *c, struct loc *grid)
{
    return (square_p(p, grid)->feat != square(c, grid)->feat);
}


/*
 * True if the square is marked
 */
bool square_ismark(struct player *p, struct loc *grid)
{
    my_assert(player_square_in_bounds(p, grid));

    return sqinfo_has(square_p(p, grid)->info, SQUARE_MARK);
}


bool square_istree(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_TREE);
}


bool square_isstrongtree(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_TREE) &&
        !tf_has(f_info[square(c, grid)->feat].flags, TF_WITHERED));
}


bool square_iswitheredtree(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_TREE) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_WITHERED));
}


bool square_isdirt(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_DIRT);
}


bool square_isgrass(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_GRASS);
}


bool square_iscrop(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_CROP);
}


bool square_iswater(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_WATER);
}


bool square_islava(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_LAVA);
}


bool square_isnether(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_NETHER);
}


bool square_ismountain(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_MOUNTAIN);
}


bool square_isdryfountain(struct chunk *c, struct loc *grid)
{
    return (tf_has(f_info[square(c, grid)->feat].flags, TF_FOUNTAIN) &&
        tf_has(f_info[square(c, grid)->feat].flags, TF_DRIED));
}


bool square_isfountain(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_FOUNTAIN);
}


bool square_isweb(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_WEB);
}


/*
 * SQUARE INFO PREDICATES
 *
 * These functions tell whether a square is marked with one of the SQUARE_*
 * flags. These flags are mostly used to mark a square with some information
 * about its location or status.
 */


/*
 * True if the square is lit
 */
bool square_isglow(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_GLOW);
}


/*
 * True if the square is part of a vault.
 *
 * This doesn't say what kind of square it is, just that it is part of a vault.
 */
bool square_isvault(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_VAULT);
}


/*
 * True if the square will be cleared of its trash on every dawn.
 */
bool square_notrash(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_NOTRASH);
}


/*
 * True if the square is part of a room.
 */
bool square_isroom(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_ROOM);
}


/*
 * True if the square has been seen by the player
 */
bool square_isseen(struct player *p, struct loc *grid)
{
    my_assert(player_square_in_bounds(p, grid));

    return sqinfo_has(square_p(p, grid)->info, SQUARE_SEEN);
}


/*
 * True if the square is currently viewable by the player
 */
bool square_isview(struct player *p, struct loc *grid)
{
    my_assert(player_square_in_bounds(p, grid));

    return sqinfo_has(square_p(p, grid)->info, SQUARE_VIEW);
}


/*
 * True if the square was seen before the current update
 */
bool square_wasseen(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_WASSEEN);
}


/*
 * True if the square has been detected for traps
 */
bool square_isdtrap(struct player *p, struct loc *grid)
{
    my_assert(player_square_in_bounds(p, grid));

    return sqinfo_has(square_p(p, grid)->info, SQUARE_DTRAP);
}


/*
 * True if the square is a feeling trigger square
 */
bool square_isfeel(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_FEEL);
}


/*
 * True if the square is a feeling trigger square for the player
 */
bool square_ispfeel(struct player *p, struct loc *grid)
{
    my_assert(player_square_in_bounds(p, grid));

    return sqinfo_has(square_p(p, grid)->info, SQUARE_FEEL);
}


/*
 * True if the square has a known trap
 */
bool square_istrap(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_TRAP);
}


/*
 * True if the square is an inner wall (generation)
 */
bool square_iswall_inner(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_WALL_INNER);
}


/*
 * True if the square is an outer wall (generation)
 */
bool square_iswall_outer(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_WALL_OUTER);
}


/*
 * True if the square is a solid wall (generation)
 */
bool square_iswall_solid(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_WALL_SOLID);
}


/*
 * True if the square has monster restrictions (generation)
 */
bool square_ismon_restrict(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_MON_RESTRICT);
}


/*
 * True if the square is no-teleport.
 */
bool square_isno_teleport(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_NO_TELEPORT);
}


/*
 * True if the square is no-map.
 */
bool square_isno_map(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_NO_MAP);
}


/*
 * True if the square is no-esp.
 */
bool square_isno_esp(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_NO_ESP);
}


/*
 * True if the square is marked for projection processing
 */
bool square_isproject(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_PROJECT);
}


/*
 * True if cave square is inappropriate to place stairs
 */
bool square_isno_stairs(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return sqinfo_has(square(c, grid)->info, SQUARE_NO_STAIRS);
}


/*
 * SQUARE BEHAVIOR PREDICATES
 *
 * These functions define how a given square behaves, e.g. whether it is
 * passable by the player, whether it is diggable, contains items, etc.
 *
 * These functions use the FEATURE PREDICATES (as well as c->info) to make
 * the determination.
 */


/*
 * True if the square is open (a floor square not occupied by a monster/player).
 */
bool square_isopen(struct chunk *c, struct loc *grid)
{
    return (square_isanyfloor(c, grid) && !square(c, grid)->mon);
}


/*
 * True if the square is empty (an open square without any items).
 */
bool square_isempty(struct chunk *c, struct loc *grid)
{
    if (square_isplayertrap(c, grid)) return false;
    return (square_isopen(c, grid) && !square_object(c, grid));
}


/*
 * True if the square is an "empty" floor grid.
 */
bool square_isemptyfloor(struct chunk *c, struct loc *grid)
{
    /* Assume damaging squares are not valid */
    if (square_isdamaging(c, grid)) return false;

    return (square_ispassable(c, grid) && !square(c, grid)->mon);
}


/*
 * True if the square is an untrapped floor square without items.
 */
bool square_canputitem(struct chunk *c, struct loc *grid)
{
    if (!square_isanyfloor(c, grid)) return false;
    if (square_trap_flag(c, grid, TRF_GLYPH) || square_isplayertrap(c, grid))
        return false;
    return !square_object(c, grid);
}


/*
 * True if the square can be dug: this includes rubble and non-permanent walls.
 */
bool square_isdiggable(struct chunk *c, struct loc *grid)
{
    /* PWMAngband: also include trees and webs */
    return (square_ismineral(c, grid) || square_issecretdoor(c, grid) || square_isrubble(c, grid) ||
        square_istree(c, grid) || square_isweb(c, grid));
}


/*
 * True if a square seems diggable: this includes diggable squares as well as permanent walls,
 * closed doors and mountain tiles.
 */
bool square_seemsdiggable(struct chunk *c, struct loc *grid)
{
    return (square_isdiggable(c, grid) || square_basic_iscloseddoor(c, grid) ||
        square_isperm(c, grid) || square_ismountain(c, grid));
}


/*
 * True if a monster can walk through the tile.
 *
 * This is needed for polymorphing. A monster may be on a feature that isn't
 * an empty space, causing problems when it is replaced with a new monster.
 */
bool square_is_monster_walkable(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return feat_is_monster_walkable(square(c, grid)->feat);
}


/*
 * True if the square is passable by the player.
 */
bool square_ispassable(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return feat_is_passable(square(c, grid)->feat);
}


/*
 * True if any projectable can pass through the square.
 *
 * This function is the logical negation of square_iswall().
 */
bool square_isprojectable(struct chunk *c, struct loc *grid)
{
    if (!square_in_bounds(c, grid)) return false;

    return feat_is_projectable(square(c, grid)->feat);
}


/*
 * True if the square is a wall square (impedes the player).
 *
 * This function is the logical negation of square_isprojectable().
 */
bool square_iswall(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return !square_isprojectable(c, grid);
}


/*
 * True if the square is a permanent wall or one of the "stronger" walls.
 *
 * The stronger walls are granite, magma and quartz. This excludes things like
 * secret doors and rubble.
 */
bool square_isstrongwall(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return (square_ismineral(c, grid) || square_isperm(c, grid));
}


/*
 * True if the cave square is internally lit.
 */
bool square_isbright(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return feat_is_bright(square(c, grid)->feat);
}


/*
 * True if the cave square is fire-based.
 */
bool square_isfiery(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return feat_is_fiery(square(c, grid)->feat);
}


/*
 * True if the cave square can damage the inhabitant
 */
bool square_isdamaging(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return (square_iswater(c, grid) || square_islava(c, grid) || square_isfiery(c, grid) ||
        square_isnether(c, grid));
}


/*
 * True if the cave square doesn't allow monster flow information.
 */
bool square_isnoflow(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return feat_is_no_flow(square(c, grid)->feat);
}


/*
 * True if the cave square doesn't carry player scent.
 */
bool square_isnoscent(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));

    return feat_is_no_scent(square(c, grid)->feat);
}


bool square_iswarded(struct chunk *c, struct loc *grid)
{
    struct trap_kind *rune = lookup_trap("glyph of warding");

    return square_trap_specific(c, grid, rune->tidx);
}


bool square_isdecoyed(struct chunk *c, struct loc *grid)
{
    struct trap_kind *glyph = lookup_trap("decoy");

    return square_trap_specific(c, grid, glyph->tidx);
}


bool square_seemslikewall(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_ROCK);
}


bool square_isinteresting(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_INTERESTING);
}


/*
 * True if the square is a closed, locked door.
 */
bool square_islockeddoor(struct chunk *c, struct loc *grid)
{
    return square_door_power(c, grid) > 0;
}


/*
 * True if there is a player trap (known or unknown) in this square.
 */
bool square_isplayertrap(struct chunk *c, struct loc *grid)
{
    return square_trap_flag(c, grid, TRF_TRAP);
}


/*
 * True if there is a visible trap in this square.
 */
bool square_isvisibletrap(struct chunk *c, struct loc *grid)
{
    return square_trap_flag(c, grid, TRF_VISIBLE);
}


/*
 * True if the square is an unknown player trap (it will appear as a floor tile).
 */
bool square_issecrettrap(struct chunk *c, struct loc *grid)
{
    return !square_isvisibletrap(c, grid) && square_isplayertrap(c, grid);
}


/*
 * True if the square is a known, disabled player trap.
 */
bool square_isdisabledtrap(struct chunk *c, struct loc *grid)
{
    return square_isvisibletrap(c, grid) && (square_trap_timeout(c, grid, 0) > 0);
}


/*
 * True if the square is a known, disarmable player trap.
 */
bool square_isdisarmabletrap(struct chunk *c, struct loc *grid)
{
    if (square_isdisabledtrap(c, grid)) return false;
    return square_isvisibletrap(c, grid) && square_isplayertrap(c, grid);
}


/*
 * Checks if a square is at the (inner) edge of a trap detect area
 */
bool square_dtrap_edge(struct player *p, struct chunk *c, struct loc *grid)
{
    struct loc next;

    /* Only on random levels */
    if (!random_level(&p->wpos)) return false;

    /* Check if the square is a dtrap in the first place */
    if (!square_isdtrap(p, grid)) return false;

    /* Check for non-dtrap adjacent grids */
    next_grid(&next, grid, DIR_S);
    if (square_in_bounds_fully(c, &next) && !square_isdtrap(p, &next))
        return true;
    next_grid(&next, grid, DIR_E);
    if (square_in_bounds_fully(c, &next) && !square_isdtrap(p, &next))
        return true;
    next_grid(&next, grid, DIR_N);
    if (square_in_bounds_fully(c, &next) && !square_isdtrap(p, &next))
        return true;
    next_grid(&next, grid, DIR_W);
    if (square_in_bounds_fully(c, &next) && !square_isdtrap(p, &next))
        return true;

    return false;
}


/*
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool square_changeable(struct chunk *c, struct loc *grid)
{
    struct object *obj;

    /* Forbid perma-grids */
    if (square_isperm(c, grid) || square_isstairs(c, grid) || square_isshop(c, grid) ||
        square_home_iscloseddoor(c, grid))
    {
        return false;
    }

    /* Check objects */
    for (obj = square_object(c, grid); obj; obj = obj->next)
    {
        /* Forbid artifact grids */
        if (obj->artifact) return false;
    }

    /* Accept */
    return true;
}


bool square_in_bounds(struct chunk *c, struct loc *grid)
{
    my_assert(c);

    return ((grid->x >= 0) && (grid->x < c->width) && (grid->y >= 0) && (grid->y < c->height));
}


bool square_in_bounds_fully(struct chunk *c, struct loc *grid)
{
    my_assert(c);

    return ((grid->x > 0) && (grid->x < c->width - 1) && (grid->y > 0) && (grid->y < c->height - 1));
}


/*
 * Checks if a square is thought by the player to block projections
 */
bool square_isbelievedwall(struct player *p, struct chunk *c, struct loc *grid)
{
    /* The edge of the world is definitely gonna block things */
    if (!square_in_bounds_fully(c, grid)) return true;

    /* If we dont know assume its projectable */
    if (!square_isknown(p, grid)) return false;

    /* Report what we think (we may be wrong) */
    return !feat_is_projectable(square_p(p, grid)->feat);
}


/*
 * OTHER SQUARE FUNCTIONS
 *
 * Below are various square-specific functions which are not predicates
 */


struct square *square(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));
    return &c->squares[grid->y][grid->x];
}


struct player_square *square_p(struct player *p, struct loc *grid)
{
    my_assert(player_square_in_bounds(p, grid));
    return &p->cave->squares[grid->y][grid->x];
}


struct feature *square_feat(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds(c, grid));
    return &f_info[square(c, grid)->feat];
}


/*
 * Get a monster on the current level by its position.
 */
struct monster *square_monster(struct chunk *c, struct loc *grid)
{
    if (!square_in_bounds(c, grid)) return NULL;
    if (square(c, grid)->mon > 0)
    {
        struct monster *mon = cave_monster(c, square(c, grid)->mon);

        return (mon->race? mon: NULL);
    }

    return NULL;
}


/*
 * Get the top object of a pile on the current level by its position.
 */
struct object *square_object(struct chunk *c, struct loc *grid)
{
    if (!square_in_bounds(c, grid)) return NULL;
    return square(c, grid)->obj;
}


/*
 * Get the first (and currently only) trap in a position on the current level.
 */
struct trap *square_trap(struct chunk *c, struct loc *grid)
{
    if (!square_in_bounds(c, grid)) return NULL;
    return square(c, grid)->trap;
}


/*
 * Return true if the given object is on the floor at this grid
 */
bool square_holds_object(struct chunk *c, struct loc *grid, struct object *obj)
{
    my_assert(square_in_bounds(c, grid));
    return pile_contains(square_object(c, grid), obj);
}


/*
 * Remove an object from a floor pile, leaving it unattached
 */
void square_excise_object(struct chunk *c, struct loc *grid, struct object *obj)
{
    my_assert(square_in_bounds(c, grid));
    pile_excise(&square(c, grid)->obj, obj);

    /* Hack -- excise object index */
    c->o_gen[0 - (obj->oidx + 1)] = false;
    obj->oidx = 0;

    /* Delete the mimicking monster if necessary */
    if (obj->mimicking_m_idx)
    {
        struct monster *mon = cave_monster(c, obj->mimicking_m_idx);

        /* Clear the mimicry */
        mon->mimicked_obj = NULL;
        mon->camouflage = false;
    }

    /* Redraw */
    redraw_floor(&c->wpos, grid);

    /* Visual update */
    square_light_spot(c, grid);
}


/*
 * Excise an entire floor pile.
 */
void square_excise_pile(struct chunk *c, struct loc *grid)
{
    struct object *obj;

    my_assert(c);
    my_assert(square_in_bounds(c, grid));

    for (obj = square_object(c, grid); obj; obj = obj->next)
    {
        /* Preserve unseen artifacts */
        preserve_artifact(obj);

        /* Hack -- excise object index */
        c->o_gen[0 - (obj->oidx + 1)] = false;
        obj->oidx = 0;

        /* Delete the mimicking monster if necessary */
        if (obj->mimicking_m_idx)
        {
            struct monster *mon = cave_monster(c, obj->mimicking_m_idx);

            /* Clear the mimicry */
            mon->mimicked_obj = NULL;

            delete_monster_idx(c, obj->mimicking_m_idx);
        }
    }

    object_pile_free(square_object(c, grid));
    square_set_obj(c, grid, NULL);

    /* Redraw */
    redraw_floor(&c->wpos, grid);

    /* Visual update */
    square_light_spot(c, grid);
}


/*
 * Sense the existence of objects on a grid in the current level
 */
void square_sense_pile(struct player *p, struct chunk *c, struct loc *grid)
{
    struct object *obj;

    if (!wpos_eq(&p->wpos, &c->wpos)) return;

    /* Make new sensed objects where necessary */
    if (square_p(p, grid)->obj) return;

    /* Sense every item on this grid */
    for (obj = square_object(c, grid); obj; obj = obj->next)
        object_sense(p, obj);
}


static bool object_equals(const struct object *obj1, const struct object *obj2)
{
    struct object *test;

    /* Objects are strictly equal */
    if (obj1 == obj2) return true;

    /* Objects are strictly different */
    if (!(obj1 && obj2)) return false;

    /* Make a writable identical copy of the second object */
    test = object_new();
    memcpy(test, obj2, sizeof(struct object));

    /* Make prev and next strictly equal since they are irrelevant */
    test->prev = obj1->prev;
    test->next = obj1->next;

    /* Known part must be equal */
    if (!object_equals(obj1->known, test->known))
    {
        mem_free(test);
        return false;
    }

    /* Make known strictly equal since they are now irrelevant */
    test->known = obj1->known;

    /* Brands must be equal */
    if (!brands_are_equal(obj1, test))
    {
        mem_free(test);
        return false;
    }

    /* Make brands strictly equal since they are now irrelevant */
    test->brands = obj1->brands;

    /* Slays must be equal */
    if (!slays_are_equal(obj1, test))
    {
        mem_free(test);
        return false;
    }

    /* Make slays strictly equal since they are now irrelevant */
    test->slays = obj1->slays;

    /* Make attr strictly equal since they are irrelevant */
    test->attr = obj1->attr;

    /* All other fields must be equal */
    if (memcmp(obj1, test, sizeof(struct object)) != 0)
    {
        mem_free(test);
        return false;
    }

    /* Success */
    mem_free(test);
    return true;
}


static void square_update_pile(struct player *p, struct chunk *c, struct loc *grid)
{
    struct object *obj;

    /* Know every item on this grid */
    for (obj = square_object(c, grid); obj; obj = obj->next)
    {
        /* Make the new object */
        struct object *new_obj = object_new();

        object_copy(new_obj, obj);

        /* Attach it to the current floor pile */
        pile_insert_end(&square_p(p, grid)->obj, new_obj);
    }
}


/*
 * Update the player's knowledge of the objects on a grid in the current level
 */
void square_know_pile(struct player *p, struct chunk *c, struct loc *grid)
{
    struct object *obj, *known_obj;

    obj = square_object(c, grid);
    known_obj = square_p(p, grid)->obj;

    if (!wpos_eq(&p->wpos, &c->wpos)) return;

    /* Object is not known: update knowledge */
    if (!known_obj)
    {
        square_update_pile(p, c, grid);
        return;
    }

    /* Object is absent: wipe knowledge */
    if (!obj)
    {
        square_forget_pile(p, grid);
        return;
    }

    /* Object is known: wipe and update knowledge if something changed */
    while (obj || known_obj)
    {
        if (!object_equals(obj, known_obj))
        {
            square_forget_pile(p, grid);
            square_update_pile(p, c, grid);
            return;
        }
        if (obj) obj = obj->next;
        if (known_obj) known_obj = known_obj->next;
    }
}


void square_forget_pile(struct player *p, struct loc *grid)
{
    struct object *current, *next;

    current = square_p(p, grid)->obj;
    while (current)
    {
        next = current->next;

        /* Stop tracking item */
        if (tracked_object_is(p->upkeep, current)) track_object(p->upkeep, NULL);

        object_delete(&current);
        current = next;
    }
    square_p(p, grid)->obj = NULL;
}


struct object *square_known_pile(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Hack -- DM has full knowledge */
    if (p->dm_flags & DM_SEE_LEVEL) return square_object(c, grid);

    return square_p(p, grid)->obj;
}


/*
 * Return how many cardinal directions around (x, y) contain (real) walls.
 *
 * c current chunk
 * y, x co-ordinates
 *
 * Returns the number of walls
 */
int square_num_walls_adjacent(struct chunk *c, struct loc *grid)
{
    int k = 0;
    struct loc next;

    my_assert(square_in_bounds_fully(c, grid));

    next_grid(&next, grid, DIR_S);
    if (feat_is_wall(square(c, &next)->feat)) k++;
    next_grid(&next, grid, DIR_N);
    if (feat_is_wall(square(c, &next)->feat)) k++;
    next_grid(&next, grid, DIR_E);
    if (feat_is_wall(square(c, &next)->feat)) k++;
    next_grid(&next, grid, DIR_W);
    if (feat_is_wall(square(c, &next)->feat)) k++;

    return k;
}


/*
 * Set the terrain type for a square.
 *
 * This should be the only function that sets terrain, apart from the savefile
 * loading code.
 */
void square_set_feat(struct chunk *c, struct loc *grid, int feat)
{
    int current_feat;

    my_assert(square_in_bounds(c, grid));
    current_feat = square(c, grid)->feat;

    /* Track changes */
    if (current_feat) c->feat_count[current_feat]--;
    if (feat) c->feat_count[feat]++;

    /* Make the change */
    square(c, grid)->feat = feat;

    /* Light bright terrain */
    if (feat_is_bright(feat)) sqinfo_on(square(c, grid)->info, SQUARE_GLOW);

    /* Make the new terrain feel at home */
    if (!ht_zero(&c->generated))
    {
        /* Remove traps if necessary */
        if (!square_player_trap_allowed(c, grid))
            square_destroy_trap(c, grid);

        square_note_spot(c, grid);
        square_light_spot(c, grid);
    }

    /* Make sure no incorrect wall flags set for dungeon generation */
    else
    {
        sqinfo_off(square(c, grid)->info, SQUARE_WALL_INNER);
        sqinfo_off(square(c, grid)->info, SQUARE_WALL_OUTER);
        sqinfo_off(square(c, grid)->info, SQUARE_WALL_SOLID);
    }
}


/*
 * Set the player-"known" terrain type for a square.
 */
static void square_set_known_feat(struct player *p, struct loc *grid, int feat)
{
    square_p(p, grid)->feat = feat;
}


/*
 * Set the occupying monster for a square.
 */
void square_set_mon(struct chunk *c, struct loc *grid, int midx)
{
    square(c, grid)->mon = midx;
}


/*
 * Set the (first) object for a square.
 */
void square_set_obj(struct chunk *c, struct loc *grid, struct object *obj)
{
    square(c, grid)->obj = obj;
}


/*
 * Set the (first) trap for a square.
 */
void square_set_trap(struct chunk *c, struct loc *grid, struct trap *trap)
{
    square(c, grid)->trap = trap;
}


void square_add_trap(struct chunk *c, struct loc *grid)
{
    my_assert(square_in_bounds_fully(c, grid));
    place_trap(c, grid, -1, c->wpos.depth);
}


void square_add_glyph(struct chunk *c, struct loc *grid, int type)
{
    struct trap_kind *glyph = NULL;

    switch (type)
    {
        case GLYPH_WARDING: glyph = lookup_trap("glyph of warding"); break;
        case GLYPH_DECOY: glyph = lookup_trap("decoy"); loc_copy(&c->decoy, grid); break;
        default: return;
    }

    place_trap(c, grid, glyph->tidx, 0);
}


void square_add_stairs(struct chunk *c, struct loc *grid, byte feat_stairs)
{
    static byte count = 0xFF;
    static u16b feat = 0;
    u16b desired_feat;

    if (!feat) feat = FEAT_MORE;

    /* Choose staircase direction */
    if (feat_stairs != FEAT_NONE)
        desired_feat = feat_stairs;
    else if (cfg_limit_stairs >= 2)
    {
        /* Always down */
        desired_feat = FEAT_MORE;
    }
    else if (count == 0)
    {
        /* Un-bias the RNG: no more creation of 10 up staircases in a row... */
        count = rand_range(3, 5);
        feat = ((feat == FEAT_MORE)? FEAT_LESS: FEAT_MORE);
        desired_feat = feat;
    }
    else
    {
        /* Random choice */
        desired_feat = (magik(50)? FEAT_MORE: FEAT_LESS);

        /* Check current feature */
        if ((count == 0xFF) || (desired_feat != feat)) count = rand_range(3, 5);
        if (desired_feat == feat) count--;
    }

    /* Create a staircase */
    square_set_feat(c, grid, desired_feat);
}


void square_open_door(struct chunk *c, struct loc *grid)
{
    square_remove_all_traps(c, grid);
    square_set_feat(c, grid, FEAT_OPEN);
}


void square_open_homedoor(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_HOME_OPEN);
}


void square_close_door(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_CLOSED);
}


void square_smash_door(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_BROKEN);
}


void square_unlock_door(struct chunk *c, struct loc *grid)
{
    square_set_door_lock(c, grid, 0);
}


void square_destroy_door(struct chunk *c, struct loc *grid)
{
    u16b feat = ((c->wpos.depth > 0)? FEAT_FLOOR: FEAT_DIRT);

    square_remove_all_traps(c, grid);
    square_set_feat(c, grid, feat);
}


void square_destroy_trap(struct chunk *c, struct loc *grid)
{
    square_remove_all_traps(c, grid);
}


void square_disable_trap(struct player *p, struct chunk *c, struct loc *grid)
{
    if (!square_isplayertrap(c, grid)) return;
    square_set_trap_timeout(p, c, grid, false, 0, 10);
}


void square_destroy_decoy(struct player *p, struct chunk *c, struct loc *grid)
{
    square_remove_all_traps(c, grid);
    loc_init(&c->decoy, 0, 0);

    if (!p) return;
    if (los(c, &p->grid, grid) && !p->timed[TMD_BLIND])
        msg(p, "The decoy is destroyed!");
}


void square_tunnel_wall(struct chunk *c, struct loc *grid)
{
    u16b feat = ((c->wpos.depth > 0)? FEAT_FLOOR: FEAT_DIRT);

    square_set_feat(c, grid, feat);
}


void square_destroy_wall(struct chunk *c, struct loc *grid)
{
    u16b feat = ((c->wpos.depth > 0)? FEAT_FLOOR: FEAT_MUD);

    square_set_feat(c, grid, feat);
}


void square_destroy(struct chunk *c, struct loc *grid)
{
    int feat = FEAT_FLOOR;
    int r = randint0(200);

    if (r < 20)
        feat = FEAT_GRANITE;
    else if (r < 70)
        feat = FEAT_QUARTZ;
    else if (r < 100)
        feat = FEAT_MAGMA;

    square_set_feat(c, grid, feat);
}


void square_earthquake(struct chunk *c, struct loc *grid)
{
    int t = randint0(100);
    int f;

    if (!square_ispassable(c, grid))
    {
        square_clear_feat(c, grid);
        return;
    }

    if (t < 20)
        f = FEAT_GRANITE;
    else if (t < 70)
        f = FEAT_QUARTZ;
    else
        f = FEAT_MAGMA;

    square_set_feat(c, grid, f);
}


/*
 * Add visible treasure to a mineral square.
 */
void square_upgrade_mineral(struct chunk *c, struct loc *grid)
{
    if (square(c, grid)->feat == FEAT_MAGMA)
        square_set_feat(c, grid, FEAT_MAGMA_K);
    if (square(c, grid)->feat == FEAT_QUARTZ)
        square_set_feat(c, grid, FEAT_QUARTZ_K);
}


void square_destroy_rubble(struct chunk *c, struct loc *grid)
{
    u16b feat = ((c->wpos.depth > 0)? FEAT_FLOOR: FEAT_MUD);

    square_set_feat(c, grid, feat);
}


/* Note that this returns the STORE_ index, which is one less than shopnum */
int square_shopnum(struct chunk *c, struct loc *grid)
{
    if (square_isshop(c, grid)) return f_info[square(c, grid)->feat].shopnum - 1;
    return -1;
}


int square_digging(struct chunk *c, struct loc *grid)
{
    return f_info[square(c, grid)->feat].dig;
}


int square_apparent_feat(struct player *p, struct chunk *c, struct loc *grid)
{
    int actual = square_known_feat(p, c, grid);
    char *mimic_name = f_info[actual].mimic;

    return (mimic_name? lookup_feat(mimic_name): actual);
}


const char *square_apparent_name(struct player *p, struct chunk *c, struct loc *grid)
{
    struct feature *f = &f_info[square_apparent_feat(p, c, grid)];

    if (f->shortdesc) return f->shortdesc;
    return f->name;
}


void square_memorize(struct player *p, struct chunk *c, struct loc *grid)
{
    if (!wpos_eq(&p->wpos, &c->wpos)) return;
    square_set_known_feat(p, grid, square(c, grid)->feat);
}


void square_forget(struct player *p, struct loc *grid)
{
    square_set_known_feat(p, grid, FEAT_NONE);
}


void square_mark(struct player *p, struct loc *grid)
{
    sqinfo_on(square_p(p, grid)->info, SQUARE_MARK);
}


void square_unmark(struct player *p, struct loc *grid)
{
    sqinfo_off(square_p(p, grid)->info, SQUARE_MARK);
}


void square_unglow(struct chunk *c, struct loc *grid)
{
    /* Bright tiles are always lit */
    if (square_isbright(c, grid)) return;

    sqinfo_off(square(c, grid)->info, SQUARE_GLOW);
}


bool square_isnormal(struct chunk *c, struct loc *grid)
{
    if (square_isvisibletrap(c, grid)) return true;
    return (!tf_has(f_info[square(c, grid)->feat].flags, TF_FLOOR) &&
        !tf_has(f_info[square(c, grid)->feat].flags, TF_BORING));
}


void square_destroy_tree(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_DIRT);
}


void square_burn_tree(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_EVIL_TREE);
}


void square_burn_grass(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_DIRT);
}


void square_colorize_door(struct chunk *c, struct loc *grid, int power)
{
    square_set_feat(c, grid, FEAT_HOME_CLOSED + power);
}


void square_build_permhouse(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_PERM_HOUSE);
}


void square_dry_fountain(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_FNT_DRIED);
}


void square_clear_feat(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_FLOOR);
}


void square_add_wall(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_GRANITE);
}


void square_add_tree(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_TREE);
}


void square_add_dirt(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_DIRT);
}


void square_add_grass(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_GRASS);
}


void square_add_safe(struct chunk *c, struct loc *grid)
{
    square_set_feat(c, grid, FEAT_FLOOR_SAFE);
}


bool square_isplot(struct chunk *c, struct loc *grid)
{
    return tf_has(f_info[square(c, grid)->feat].flags, TF_PLOT);
}


void square_actor(struct chunk *c, struct loc *grid, struct source *who)
{
    int m_idx;

    memset(who, 0, sizeof(struct source));
    if (!square_in_bounds(c, grid)) return;

    m_idx = square(c, grid)->mon;
    if (!m_idx) return;

    who->idx = abs(m_idx);
    who->player = ((m_idx < 0)? player_get(0 - m_idx): NULL);
    who->monster = ((m_idx > 0)? cave_monster(c, m_idx): NULL);
}


int square_known_feat(struct player *p, struct chunk *c, struct loc *grid)
{
    if (p->dm_flags & DM_SEE_LEVEL) return square(c, grid)->feat;
    return square_p(p, grid)->feat;
}


static bool normal_grid(struct chunk *c, struct loc *grid)
{
    return ((((c->wpos.depth > 0) || town_area(&c->wpos)) && square_isnormal(c, grid)) ||
        square_isroom(c, grid));
}


/*
 * Light or darken a square
 * Also applied for wilderness and special levels
 */
void square_illuminate(struct player *p, struct chunk *c, struct loc *grid, bool daytime)
{
    /* Only interesting grids at night */
    if (normal_grid(c, grid) || daytime || (c->wpos.depth > 0))
    {
        sqinfo_on(square(c, grid)->info, SQUARE_GLOW);
        if (p) square_memorize(p, c, grid);
    }
    else
    {
        square_unglow(c, grid);
        if (p) square_forget(p, grid);
    }
}


struct trap *square_top_trap(struct chunk *c, struct loc *grid)
{
    struct trap *trap = NULL;

    if (square_istrap(c, grid))
    {
        trap = square(c, grid)->trap;

        /* Scan the square trap list */
        while (trap)
        {
            if ((trf_has(trap->flags, TRF_TRAP) && trf_has(trap->flags, TRF_VISIBLE)) ||
                trf_has(trap->flags, TRF_GLYPH))
            {
                /* Accept the trap -- only if not disabled */
                if (!trap->timeout) break;
            }
            trap = trap->next;
        }
    }

    return trap;
}


void square_memorize_trap(struct player *p, struct chunk *c, struct loc *grid)
{
    struct trap *trap;

    if (!wpos_eq(&p->wpos, &c->wpos)) return;

    /* Remove current knowledge */
    square_forget_trap(p, grid);

    /* Memorize first visible trap */
    trap = square_top_trap(c, grid);
    if (trap)
    {
        square_p(p, grid)->trap = mem_zalloc(sizeof(struct trap));
        square_p(p, grid)->trap->kind = trap->kind;
        loc_copy(&square_p(p, grid)->trap->grid, &trap->grid);
        trf_copy(square_p(p, grid)->trap->flags, trap->flags);
    }
}


struct trap *square_known_trap(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Hack -- DM has full knowledge */
    if (p->dm_flags & DM_SEE_LEVEL) return square_top_trap(c, grid);

    return square_p(p, grid)->trap;
}


void square_forget_trap(struct player *p, struct loc *grid)
{
    if (square_p(p, grid)->trap)
    {
        mem_free(square_p(p, grid)->trap);
        square_p(p, grid)->trap = NULL;
    }
}


void square_init_join_up(struct chunk *c)
{
    loc_init(&c->join->up, 0, 0);
}


void square_set_join_up(struct chunk *c, struct loc *grid)
{
    loc_copy(&c->join->up, grid);
}


void square_init_join_down(struct chunk *c)
{
    loc_init(&c->join->down, 0, 0);
}


void square_set_join_down(struct chunk *c, struct loc *grid)
{
    loc_copy(&c->join->down, grid);
}


void square_init_join_rand(struct chunk *c)
{
    loc_init(&c->join->rand, 0, 0);
}


void square_set_join_rand(struct chunk *c, struct loc *grid)
{
    loc_copy(&c->join->rand, grid);
}


/*
 * Set an up staircase at position (y,x)
 */
void square_set_upstairs(struct chunk *c, struct loc *grid)
{
    /* Clear previous contents, add up stairs */
    if (cfg_limit_stairs < 2) square_set_feat(c, grid, FEAT_LESS);

    /* Set this to be the starting location for people going down */
    square_set_join_down(c, grid);
}


/*
 * Set a down staircase at position (y,x)
 */
void square_set_downstairs(struct chunk *c, struct loc *grid, int feat)
{
    /* Clear previous contents, add down stairs */
    square_set_feat(c, grid, feat);

    /* Set this to be the starting location for people going up */
    square_set_join_up(c, grid);
}
