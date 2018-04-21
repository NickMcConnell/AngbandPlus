/*
 * File: rooms.c
 * Purpose: make rooms. Used by generate.c when creating dungeons.
 */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

#include "angband.h"
#include "generate.h"
#include "grid.h"
#include "rooms.h"

/* Only one ROOM_SHOP room per level, please. Due to the
   current inventory implementation (viz, using NO_TOWN),
   multiple shops of the same kind on the same level would
   have the same keeper and stock! */
static bool shop_allowed = TRUE;

/*
 * [from SAngband (originally from OAngband)]
 *
 * Table of values that control how many times each type of room will
 * appear. Each type of room has its own row, and each column
 * corresponds to dungeon levels 0, 10, 20, and so on. The final
 * value is the minimum depth the room can appear at. -LM-
 *
 * Level 101 and below use the values for level 100.
 *
 * Rooms with lots of monsters or loot may not be generated if the
 * object or monster lists are already nearly full. Rooms will not
 * appear above their minimum depth. Tiny levels will not have space
 * for all the rooms you ask for.
 */
static room_info_type room_info_normal[ROOM_T_MAX] =
{
    /* Depth */
    /*  0  10  20  30  40  50  60  70  80  90 100  min limit */

    {{900,800,700,600,500,400,300,200,100,100,100},  0}, /*NORMAL   */
    {{  1, 10, 20, 30, 40, 50, 60, 70, 80, 90,100},  1}, /*OVERLAP  */
    {{  1, 10, 20, 30, 40, 50, 60, 70, 80, 90,100},  3}, /*CROSS    */
    {{  1, 10, 20, 30, 40, 50, 60, 70, 80, 90,100},  3}, /*INNER_F  */
    {{  0,  1,  1,  1,  3,  4,  6,  8,  8,  8,  8}, 10}, /*NEST     */
    {{  0,  1,  1,  2,  3,  4,  6,  8,  8,  8,  8}, 10}, /*PIT      */
    {{  0,  0,  1,  1,  2,  2,  3,  5,  6,  8, 10}, 30}, /*LESSER_V */
    {{  0,  0,  0,  0,  1,  2,  2,  2,  3,  3,  4}, 40}, /*GREATER_V*/
    {{  0,200,300,400,500,600,700,800,900,900,900}, 10}, /*FRACAVE  */
    {{  0,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2}, 10}, /*RANDOM_V */
    {{  0,  4,  8, 12, 16, 20, 24, 28, 32, 36, 40},  3}, /*OVAL     */
    {{  1,  6, 12, 18, 24, 30, 36, 42, 48, 54, 60}, 10}, /*CRYPT    */
    {{  0,  0,  1,  1,  1,  2,  3,  4,  5,  6,  8}, 20}, /*TRAP_PIT */
    {{  0,  0,  1,  1,  1,  2,  3,  4,  5,  6,  8}, 20}, /*TRAP     */
    {{  0,  0,  0,  0,  1,  1,  1,  2,  2,  2,  2}, 40}, /*GLASS    */
    {{  1, 10, 20, 30, 40, 50, 60, 70, 80, 90,100}, 10}, /*TEMPLATE  */
  /*{{100,100,150,200,250,300,350,400,450,450,450},  0}, TEMPLATE */
};

/* Build rooms in descending order of difficulty. */
static byte room_build_order[ROOM_T_MAX] = {
    ROOM_T_GREATER_VAULT,
    ROOM_T_RANDOM_VAULT,
    ROOM_T_LESSER_VAULT,
    ROOM_T_TEMPLATE,
    ROOM_T_TRAP_PIT,
    ROOM_T_PIT,
    ROOM_T_NEST,
    ROOM_T_TRAP,
    ROOM_T_GLASS,
    ROOM_T_INNER_FEAT,
    ROOM_T_OVAL,
    ROOM_T_CRYPT,
    ROOM_T_OVERLAP,
    ROOM_T_CROSS,
    ROOM_T_FRACAVE,
    ROOM_T_NORMAL,
};


static void place_locked_door(int y, int x)
{
    if (d_info[dungeon_type].flags1 & DF1_NO_DOORS)
    {
        place_floor_bold(y, x);
    }
    else
    {
        set_cave_feat(y, x, feat_locked_door_random((d_info[dungeon_type].flags1 & DF1_GLASS_DOOR) ? DOOR_GLASS_DOOR : DOOR_DOOR));
        cave[y][x].info &= ~(CAVE_FLOOR);
        delete_monster(y, x);
    }
}

static void place_secret_door(int y, int x, int type)
{
    if (d_info[dungeon_type].flags1 & DF1_NO_DOORS)
    {
        place_floor_bold(y, x);
    }
    else
    {
        cave_type *c_ptr = &cave[y][x];

        if (type == DOOR_DEFAULT)
        {
            type = ((d_info[dungeon_type].flags1 & DF1_CURTAIN) &&
                    one_in_((d_info[dungeon_type].flags1 & DF1_NO_CAVE) ? 16 : 256)) ? DOOR_CURTAIN :
                    ((d_info[dungeon_type].flags1 & DF1_GLASS_DOOR) ? DOOR_GLASS_DOOR : DOOR_DOOR);
        }

        /* Create secret door */
        place_closed_door(y, x, type);

        if (type != DOOR_CURTAIN)
        {
            /* Hide by inner wall because this is used in rooms only */
            c_ptr->mimic = feat_wall_inner;

            /* Floor type terrain cannot hide a door */
            if (feat_supports_los(c_ptr->mimic) && !feat_supports_los(c_ptr->feat))
            {
                if (have_flag(f_info[c_ptr->mimic].flags, FF_MOVE) || have_flag(f_info[c_ptr->mimic].flags, FF_CAN_FLY))
                {
                    c_ptr->feat = one_in_(2) ? c_ptr->mimic : floor_type[randint0(100)];
                }
                c_ptr->mimic = 0;
            }
        }

        c_ptr->info &= ~(CAVE_FLOOR);
        delete_monster(y, x);
    }
}

/*
 * This funtion makes a very small room centred at (x0, y0)
 * This is used in crypts, and random elemental vaults.
 *
 * Note - this should be used only on allocated regions
 * within another room.
 */
static void build_small_room(int x0, int y0)
{
    int x, y;

    for (y = y0 - 1; y <= y0 + 1; y++)
    {
        place_inner_bold(y, x0 - 1);
        place_inner_bold(y, x0 + 1);
    }

    for (x = x0 - 1; x <= x0 + 1; x++)
    {
        place_inner_bold(y0 - 1, x);
        place_inner_bold(y0 + 1, x);
    }

    /* Place a secret door on one side */
    switch (randint0(4))
    {
        case 0: place_secret_door(y0, x0 - 1, DOOR_DEFAULT); break;
        case 1: place_secret_door(y0, x0 + 1, DOOR_DEFAULT); break;
        case 2: place_secret_door(y0 - 1, x0, DOOR_DEFAULT); break;
        case 3: place_secret_door(y0 + 1, x0, DOOR_DEFAULT); break;
    }

    /* Clear mimic type */
    cave[y0][x0].mimic = 0;

    /* Add inner open space */
    place_floor_bold(y0, x0);
}


/*
 * This function tunnels around a room if
 * it will cut off part of a cave system.
 */
static void check_room_boundary(int x1, int y1, int x2, int y2)
{
    int count, x, y;
    bool old_is_floor, new_is_floor;


    /* Initialize */
    count = 0;

    old_is_floor = get_is_floor(x1 - 1, y1);

    /*
     * Count the number of floor-wall boundaries around the room
     * Note: diagonal squares are ignored since the player can move diagonally
     * to bypass these if needed.
     */

    /* Above the top boundary */
    for (x = x1; x <= x2; x++)
    {
        new_is_floor = get_is_floor(x, y1 - 1);

        /* Increment counter if they are different */
        if (new_is_floor != old_is_floor) count++;

        old_is_floor = new_is_floor;
    }

    /* Right boundary */
    for (y = y1; y <= y2; y++)
    {
        new_is_floor = get_is_floor(x2 + 1, y);

        /* increment counter if they are different */
        if (new_is_floor != old_is_floor) count++;

        old_is_floor = new_is_floor;
    }

    /* Bottom boundary */
    for (x = x2; x >= x1; x--)
    {
        new_is_floor = get_is_floor(x, y2 + 1);

        /* increment counter if they are different */
        if (new_is_floor != old_is_floor) count++;

        old_is_floor = new_is_floor;
    }

    /* Left boundary */
    for (y = y2; y >= y1; y--)
    {
        new_is_floor = get_is_floor(x1 - 1, y);

        /* increment counter if they are different */
        if (new_is_floor != old_is_floor) count++;

        old_is_floor = new_is_floor;
    }

    /* If all the same, or only one connection exit. */
    if (count <= 2) return;


    /* Tunnel around the room so to prevent problems with caves */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            set_floor(x, y);
        }
    }
}


/*
 *  Helper function for find_space().
 *
 *  Is this a good location?
 */
static bool find_space_aux(int blocks_high, int blocks_wide, int block_y, int block_x)
{
    int by1, bx1, by2, bx2, by, bx;

    /* Itty-bitty rooms must shift about within their rectangle */
    if (blocks_wide < 3)
    {
        if ((blocks_wide == 2) && (block_x % 3) == 2)
            return FALSE;
    }

    /* Rooms with width divisible by 3 must be fitted to a rectangle. */
    else if ((blocks_wide % 3) == 0)
    {
        /* Must be aligned to the left edge of a 11x33 rectangle. */
        if ((block_x % 3) != 0)
            return FALSE;
    }

    /*
     * Big rooms that do not have a width divisible by 3 must be
     * aligned towards the edge of the dungeon closest to them.
     */
    else
    {
        /* Shift towards left edge of dungeon. */
        if (block_x + (blocks_wide / 2) <= dun->col_rooms / 2)
        {
            if (((block_x % 3) == 2) && ((blocks_wide % 3) == 2))
                return FALSE;
            if ((block_x % 3) == 1)
                return FALSE;
        }

        /* Shift toward right edge of dungeon. */
        else
        {
            if (((block_x % 3) == 2) && ((blocks_wide % 3) == 2))
                return FALSE;
            if ((block_x % 3) == 1)
                return FALSE;
        }
    }

    /* Extract blocks */
    by1 = block_y + 0;
    bx1 = block_x + 0;
    by2 = block_y + blocks_high;
    bx2 = block_x + blocks_wide;

    /* Never run off the screen */
    if ((by1 < 0) || (by2 > dun->row_rooms)) return FALSE;
    if ((bx1 < 0) || (bx2 > dun->col_rooms)) return FALSE;
    
    /* Verify available space */
    for (by = by1; by < by2; by++)
    {
        for (bx = bx1; bx < bx2; bx++)
        {
            if (dun->room_map[by][bx])
            {
                return FALSE;
            }
        }
    }

    /* This location is okay */
    return TRUE;
}


/*
 * Find a good spot for the next room. -LM-
 *
 * Find and allocate a free space in the dungeon large enough to hold
 * the room calling this function.
 *
 * We allocate space in 11x11 blocks, but want to make sure that rooms
 * align neatly on the standard screen. Therefore, we make them use
 * blocks in few 11x33 rectangles as possible.
 *
 * Be careful to include the edges of the room in height and width!
 *
 * Return TRUE and values for the center of the room if all went well.
 * Otherwise, return FALSE.
 */
static bool find_space(int *y, int *x, int height, int width)
{
    int candidates, pick;
    int by, bx, by1, bx1, by2, bx2;
    int block_y = 0, block_x = 0;


    /* Find out how many blocks we need. */
    int blocks_high = 1 + ((height - 1) / BLOCK_HGT);
    int blocks_wide = 1 + ((width - 1) / BLOCK_WID);

    /* There are no way to allocate such huge space */
    if (dun->row_rooms < blocks_high) return FALSE;
    if (dun->col_rooms < blocks_wide) return FALSE;

    /* Initiallize */
    candidates = 0;

    /* Count the number of valid places */
    for (block_y = dun->row_rooms - blocks_high; block_y >= 0; block_y--)
    {
        for (block_x = dun->col_rooms - blocks_wide; block_x >= 0; block_x--)
        {
            if (find_space_aux(blocks_high, blocks_wide, block_y, block_x))
            {
                /* Find a valid place */
                candidates++;
            }
        }
    }

    /* No place! */
    if (!candidates)
    {
        return FALSE;
    }

    /* Normal dungeon */
    if (!(d_info[dungeon_type].flags1 & DF1_NO_CAVE))
    {
        /* Choose a random one */
        pick = randint1(candidates);
    }

    /* NO_CAVE dungeon (Castle) */
    else
    {
        /* Always choose the center one */
        pick = candidates/2 + 1;
    }

    /* Pick up the choosen location */
    for (block_y = dun->row_rooms - blocks_high; block_y >= 0; block_y--)
    {
        for (block_x = dun->col_rooms - blocks_wide; block_x >= 0; block_x--)
        {
            if (find_space_aux(blocks_high, blocks_wide, block_y, block_x))
            {
                pick--;

                /* This one is picked? */
                if (!pick) break;
            }
        }

        if (!pick) break;
    }

    /* Extract blocks */
    by1 = block_y + 0;
    bx1 = block_x + 0;
    by2 = block_y + blocks_high;
    bx2 = block_x + blocks_wide;

    /*
     * It is *extremely* important that the following calculation
     * be *exactly* correct to prevent memory errors
     */

    /* Acquire the location of the room */
    (*y) = ((by1 + by2) * BLOCK_HGT) / 2;
    (*x) = ((bx1 + bx2) * BLOCK_WID) / 2;

    /* Save the room location */
    if (dun->cent_n < CENT_MAX)
    {
        dun->cent[dun->cent_n].y = *y;
        dun->cent[dun->cent_n].x = *x;
        dun->cent_n++;
    }

    /* Reserve some blocks. */
    for (by = by1; by < by2; by++)
    {
        for (bx = bx1; bx < bx2; bx++)
        {
            dun->room_map[by][bx] = TRUE;
        }
    }


    /*
     * Hack- See if room will cut off a cavern.
     *
     * If so, fix by tunneling outside the room in such a
     * way as to connect the caves.
     */
    check_room_boundary(*x - width / 2 - 1, *y - height / 2 - 1, *x + (width - 1) / 2 + 1, *y + (height - 1) / 2 + 1);


    /* Success. */
    return TRUE;
}



/*
 * Room building routines.
 *
 * Room types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster nests
 *   6 -- monster pits
 *   7 -- simple vaults
 *   8 -- greater vaults
 *   9 -- fractal caves
 *  10 -- random vaults
 *  11 -- circular rooms
 *  12 -- crypts
 *  13 -- trapped monster pits
 *  14 -- trapped room
 */

static bool build_room_template(int type, int subtype)
{
    room_template_t *room_ptr;
    int x, y;
    int xval, yval;
    int xoffset = 0;
    int yoffset = 0;
    int transno;

    /* Pick a room template */
    room_ptr = choose_room_template(type, subtype);
    if (!room_ptr) return FALSE;
    if (room_ptr->flags & ROOM_SHOP)
        shop_allowed = FALSE;

    /* pick type of transformation (0-7) */
    if (room_ptr->flags & ROOM_NO_ROTATE)
        transno = 0;
    else if (type == ROOM_VAULT)
        transno = randint0(8);
    else 
    {
        int n = randint0(100);
        if (n < 45)
            transno = 0;
        else if (n < 90)
            transno = 2;
        else if (n < 95)
            transno = 1;
        else
            transno = 3;

        if (one_in_(2))
            transno |= 0x04;
    }

    /* calculate offsets */
    x = room_ptr->width;
    y = room_ptr->height;

    /* Some huge vault cannot be rotated to fit in the dungeon */
    if (x+2 > cur_hgt-2)
    {
        /* Forbid 90 or 270 degree rotation */
        transno &= ~1;
    }

    coord_trans(&x, &y, 0, 0, transno);
    if (x < 0) xoffset = -x - 1;
    if (y < 0) yoffset = -y - 1;

    /* Find and reserve some space in the dungeon. Get center of room.
     * Hack -- Prepare a bit larger space (+2, +2) to 
     * prevent generation of vaults with no-entrance. */
    if (!find_space(&yval, &xval, abs(y) + 2, abs(x) + 2))
        return FALSE;

    /* Message */
    if (cheat_room) msg_format("%s", room_name + room_ptr->name);

    build_room_template_aux(room_ptr, yval, xval, xoffset, yoffset, transno);
    return TRUE;
}

/*
 * Type 1 -- normal rectangular rooms
 */
static bool build_type1(void)
{
    int y, x, y2, x2, yval, xval;
    int y1, x1, xsize, ysize;

    bool light;

    cave_type *c_ptr;

    bool curtain = (d_info[dungeon_type].flags1 & DF1_CURTAIN) &&
        one_in_((d_info[dungeon_type].flags1 & DF1_NO_CAVE) ? 48 : 512);

    /* Pick a room size */
    y1 = 1+randint1(4);
    x1 = 2+randint1(11);
    y2 = 1+randint1(3);
    x2 = 2+randint1(11);

    xsize = x1 + x2 + 1;
    ysize = y1 + y2 + 1;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if (!find_space(&yval, &xval, ysize + 2, xsize + 2))
        return FALSE;

    /* Choose lite or dark */
    light = ((dun_level <= randint1(25)) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS));


    /* Get corner values */
    y1 = yval - ysize / 2;
    x1 = xval - xsize / 2;
    y2 = yval + (ysize - 1) / 2;
    x2 = xval + (xsize - 1) / 2;


    /* Place a full floor under the room */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            c_ptr = &cave[y][x];
            place_floor_grid(c_ptr);
            c_ptr->info |= (CAVE_ROOM);
            if (light) c_ptr->info |= (CAVE_GLOW);
        }
    }

    /* Walls around the room */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        c_ptr = &cave[y][x1 - 1];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y][x2 + 1];
        place_outer_grid(c_ptr);
    }
    for (x = x1 - 1; x <= x2 + 1; x++)
    {
        c_ptr = &cave[y1 - 1][x];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y2 + 1][x];
        place_outer_grid(c_ptr);
    }


    /* Hack -- Occasional curtained room */
    if (curtain && (y2 - y1 > 2) && (x2 - x1 > 2))
    {
        for (y = y1; y <= y2; y++)
        {
            c_ptr = &cave[y][x1];
            c_ptr->feat = feat_door[DOOR_CURTAIN].closed;
            c_ptr->info &= ~(CAVE_MASK);
            c_ptr = &cave[y][x2];
            c_ptr->feat = feat_door[DOOR_CURTAIN].closed;
            c_ptr->info &= ~(CAVE_MASK);
        }
        for (x = x1; x <= x2; x++)
        {
            c_ptr = &cave[y1][x];
            c_ptr->feat = feat_door[DOOR_CURTAIN].closed;
            c_ptr->info &= ~(CAVE_MASK);
            c_ptr = &cave[y2][x];
            c_ptr->feat = feat_door[DOOR_CURTAIN].closed;
            c_ptr->info &= ~(CAVE_MASK);
        }
    }


    /* Hack -- Occasional pillar room */
    if (one_in_(20))
    {
        for (y = y1; y <= y2; y += 2)
        {
            for (x = x1; x <= x2; x += 2)
            {
                c_ptr = &cave[y][x];
                place_inner_grid(c_ptr);
            }
        }
    }

    /* Hack -- Occasional room with four pillars */
    else if (one_in_(20))
    {
        if ((y1 + 4 < y2) && (x1 + 4 < x2))
        {
            c_ptr = &cave[y1 + 1][x1 + 1];
            place_inner_grid(c_ptr);

            c_ptr = &cave[y1 + 1][x2 - 1];
            place_inner_grid(c_ptr);

            c_ptr = &cave[y2 - 1][x1 + 1];
            place_inner_grid(c_ptr);

            c_ptr = &cave[y2 - 1][x2 - 1];
            place_inner_grid(c_ptr);
        }
    }

    /* Hack -- Occasional ragged-edge room */
    else if (one_in_(50))
    {
        for (y = y1 + 2; y <= y2 - 2; y += 2)
        {
            c_ptr = &cave[y][x1];
            place_inner_grid(c_ptr);
            c_ptr = &cave[y][x2];
            place_inner_grid(c_ptr);
        }
        for (x = x1 + 2; x <= x2 - 2; x += 2)
        {
            c_ptr = &cave[y1][x];
            place_inner_grid(c_ptr);
            c_ptr = &cave[y2][x];
            place_inner_grid(c_ptr);
        }
    }
    /* Hack -- Occasional divided room */
    else if (one_in_(50))
    {
        bool curtain2 = (d_info[dungeon_type].flags1 & DF1_CURTAIN) &&
            one_in_((d_info[dungeon_type].flags1 & DF1_NO_CAVE) ? 2 : 128);

        if (randint1(100) < 50)
        {
            /* Horizontal wall */
            for (x = x1; x <= x2; x++)
            {
                place_inner_bold(yval, x);
                if (curtain2) cave[yval][x].feat = feat_door[DOOR_CURTAIN].closed;
            }

            /* Prevent edge of wall from being tunneled */
            place_solid_bold(yval, x1 - 1);
            place_solid_bold(yval, x2 + 1);
        }
        else
        {
            /* Vertical wall */
            for (y = y1; y <= y2; y++)
            {
                place_inner_bold(y, xval);
                if (curtain2) cave[y][xval].feat = feat_door[DOOR_CURTAIN].closed;
            }

            /* Prevent edge of wall from being tunneled */
            place_solid_bold(y1 - 1, xval);
            place_solid_bold(y2 + 1, xval);
        }

        place_random_door(yval, xval, TRUE);
        if (curtain2) cave[yval][xval].feat = feat_door[DOOR_CURTAIN].closed;
    }

    return TRUE;
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static bool build_type2(void)
{
    int            y, x, xval, yval;
    int            y1a, x1a, y2a, x2a;
    int            y1b, x1b, y2b, x2b;
    bool        light;
    cave_type   *c_ptr;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if (!find_space(&yval, &xval, 11, 25)) return FALSE;

    /* Choose lite or dark */
    light = ((dun_level <= randint1(25)) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS));

    /* Determine extents of the first room */
    y1a = yval - randint1(4);
    y2a = yval + randint1(3);
    x1a = xval - randint1(11);
    x2a = xval + randint1(10);

    /* Determine extents of the second room */
    y1b = yval - randint1(3);
    y2b = yval + randint1(4);
    x1b = xval - randint1(10);
    x2b = xval + randint1(11);


    /* Place a full floor for room "a" */
    for (y = y1a - 1; y <= y2a + 1; y++)
    {
        for (x = x1a - 1; x <= x2a + 1; x++)
        {
            c_ptr = &cave[y][x];
            place_floor_grid(c_ptr);
            c_ptr->info |= (CAVE_ROOM);
            if (light) c_ptr->info |= (CAVE_GLOW);
        }
    }

    /* Place a full floor for room "b" */
    for (y = y1b - 1; y <= y2b + 1; y++)
    {
        for (x = x1b - 1; x <= x2b + 1; x++)
        {
            c_ptr = &cave[y][x];
            place_floor_grid(c_ptr);
            c_ptr->info |= (CAVE_ROOM);
            if (light) c_ptr->info |= (CAVE_GLOW);
        }
    }


    /* Place the walls around room "a" */
    for (y = y1a - 1; y <= y2a + 1; y++)
    {
        c_ptr = &cave[y][x1a - 1];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y][x2a + 1];
        place_outer_grid(c_ptr);
    }
    for (x = x1a - 1; x <= x2a + 1; x++)
    {
        c_ptr = &cave[y1a - 1][x];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y2a + 1][x];
        place_outer_grid(c_ptr);
    }

    /* Place the walls around room "b" */
    for (y = y1b - 1; y <= y2b + 1; y++)
    {
        c_ptr = &cave[y][x1b - 1];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y][x2b + 1];
        place_outer_grid(c_ptr);
    }
    for (x = x1b - 1; x <= x2b + 1; x++)
    {
        c_ptr = &cave[y1b - 1][x];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y2b + 1][x];
        place_outer_grid(c_ptr);
    }



    /* Replace the floor for room "a" */
    for (y = y1a; y <= y2a; y++)
    {
        for (x = x1a; x <= x2a; x++)
        {
            c_ptr = &cave[y][x];
            place_floor_grid(c_ptr);
        }
    }

    /* Replace the floor for room "b" */
    for (y = y1b; y <= y2b; y++)
    {
        for (x = x1b; x <= x2b; x++)
        {
            c_ptr = &cave[y][x];
            place_floor_grid(c_ptr);
        }
    }

    return TRUE;
}




/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters.
 */


/*
 * Monster validation macro
 *
 * Line 1 -- forbid town monsters
 * Line 2 -- forbid uniques
 * Line 3 -- forbid aquatic monsters
 */
#define vault_monster_okay(I) \
    (mon_hook_dungeon(I) && \
     !(r_info[I].flags1 & RF1_UNIQUE) && \
     !(r_info[I].flags7 & RF7_UNIQUE2) && \
     !(r_info[I].flagsr & RFR_RES_ALL) && \
     !(r_info[I].flags7 & RF7_AQUATIC))


/* Race index for "monster pit (clone)" */
static int vault_aux_race;

/* Race index for "monster pit (symbol clone)" */
static char vault_aux_char;

/* Breath mask for "monster pit (dragon)" */
static u32b vault_aux_dragon_mask4;

/*
 * Helper monster selection function
 */
static bool vault_aux_simple(int r_idx)
{
    /* Okay */
    return (vault_monster_okay(r_idx));
}


/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);
    if (r_idx == MON_CAAWS || r_idx == MON_SHOGGOTH) return TRUE;

    if ((r_ptr->flags2 & RF2_KILL_BODY) && !(r_ptr->flags1 & RF1_NEVER_BLOW)) return (FALSE);

    /* Also decline evil jellies (like death molds and shoggoths) */
    /*if (r_ptr->flags3 & (RF3_EVIL)) return (FALSE);*/

    /* Require icky thing, jelly, mold, or mushroom */
    if (!my_strchr("ijm,", r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (animal)"
 */
static bool vault_aux_animal(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!vault_monster_okay(r_idx)) return FALSE;
    if (!(r_ptr->flags3 & RF3_ANIMAL)) return FALSE;
    if (r_idx == MON_DEATH_BEAST) return FALSE;

    return TRUE;
}


/*
 * Helper function for "monster nest (undead)"
 */
static bool vault_aux_undead(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    /* Require Undead */
    if (!(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (chapel)"
 */
bool vault_aux_chapel_g(int r_idx)
{
    static int chapel_list[] = {
        MON_NOV_PRIEST, MON_NOV_PALADIN, MON_NOV_PRIEST_G, MON_NOV_PALADIN_G, 
        MON_PRIEST, MON_JADE_MONK, MON_IVORY_MONK, MON_ULTRA_PALADIN, 
        MON_EBONY_MONK, MON_W_KNIGHT, MON_KNI_TEMPLAR, MON_PALADIN,
        MON_TOPAZ_MONK, 0};

    int i;

    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    if (r_ptr->flags3 & (RF3_EVIL)) return (FALSE);
    if ((r_idx == MON_A_GOLD) || (r_idx == MON_A_SILVER)) return (FALSE);

    /* Require "priest" or Angel */

    if (r_ptr->d_char == 'A') return TRUE;

    for (i = 0; chapel_list[i]; i++)
        if (r_idx == chapel_list[i]) return TRUE;

    return FALSE;
}

bool vault_aux_chapel_e(int r_idx)
{
    static int chapel_list[] = {
        MON_FALLEN_ANGEL, 
        MON_HIGH_PRIEST, 
        MON_ARCHPRIEST,
        MON_BLACK_KNIGHT,
        MON_DEATH_KNIGHT,
        MON_HELL_KNIGHT,
        MON_ANTI_PALADIN,
        MON_IPSISSIMUS,
        MON_WYRD_SISTER,
        0
    };

    int i;

    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    if (r_ptr->flags3 & (RF3_GOOD)) return (FALSE);

    if (r_ptr->d_char == 'U') return TRUE;

    for (i = 0; chapel_list[i]; i++)
        if (r_idx == chapel_list[i]) return TRUE;

    return FALSE;
}

/*
 * Helper function for "monster nest (kennel)"
 */
static bool vault_aux_kennel(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!vault_monster_okay(r_idx)) return FALSE;
    if (!my_strchr("CZ", r_ptr->d_char)) return FALSE;
    if (r_idx == MON_DEATH_BEAST) return FALSE;
  
    return TRUE;
}


/*
 * Helper function for "monster nest (mimic)"
 */
static bool vault_aux_mimic(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    /* Require mimic */
    if (!my_strchr("!$&(/=?[\\|", r_ptr->d_char)) return (FALSE);

    /* Okay */
    return (TRUE);
}

/*
 * Helper function for "monster nest (clone)"
 */
static bool vault_aux_clone(int r_idx)
{
    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    return (r_idx == vault_aux_race);
}


/*
 * Helper function for "monster nest (symbol clone)"
 */
static bool vault_aux_symbol_e(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    if ((r_ptr->flags2 & RF2_KILL_BODY) && !(r_ptr->flags1 & RF1_NEVER_BLOW)) return (FALSE);

    if (r_ptr->flags3 & (RF3_GOOD)) return (FALSE);

    /* Decline incorrect symbol */
    if (r_ptr->d_char != vault_aux_char) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster nest (symbol clone)"
 */
static bool vault_aux_symbol_g(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    if ((r_ptr->flags2 & RF2_KILL_BODY) && !(r_ptr->flags1 & RF1_NEVER_BLOW)) return (FALSE);

    if (r_ptr->flags3 & (RF3_EVIL)) return (FALSE);

    /* Decline incorrect symbol */
    if (r_ptr->d_char != vault_aux_char) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    /* Require orc */
    if (!(r_ptr->flags3 & RF3_ORC)) return (FALSE);

    /* Decline undead */
    if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    /* Require troll */
    if (!(r_ptr->flags3 & RF3_TROLL)) return (FALSE);

    /* Decline undead */
    if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!vault_monster_okay(r_idx)) return (FALSE);
    if (!(r_ptr->flags3 & RF3_GIANT)) return (FALSE);
    if (r_ptr->flags4 & RF4_THROW) return FALSE;
    if (r_ptr->flags3 & RF3_GOOD) return (FALSE);
    if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

    return (TRUE);
}


/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    /* Require dragon */
    if (!(r_ptr->flags3 & RF3_DRAGON)) return (FALSE);

    /* Hack -- Require correct "breath attack" */
    if (r_ptr->flags4 != vault_aux_dragon_mask4) return (FALSE);

    /* Decline undead */
    if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_demon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    if ((r_ptr->flags2 & RF2_KILL_BODY) && !(r_ptr->flags1 & RF1_NEVER_BLOW)) return (FALSE);

    /* Require demon */
    if (!(r_ptr->flags3 & RF3_DEMON)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (lovecraftian)"
 */
static bool vault_aux_cthulhu(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return (FALSE);

    if ((r_ptr->flags2 & RF2_KILL_BODY) && !(r_ptr->flags1 & RF1_NEVER_BLOW)) return (FALSE);

    /* Require eldritch horror */
    if (!(r_ptr->flags2 & (RF2_ELDRITCH_HORROR))) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "monster pit (clone)"
 */
static void vault_prep_clone(void)
{
    /* Apply the monster restriction */
    get_mon_num_prep(vault_aux_simple, NULL);

    /* Pick a race to clone */
    vault_aux_race = get_mon_num(dun_level + 10);

    /* Remove the monster restriction */
    get_mon_num_prep(NULL, NULL);
}


/*
 * Helper function for "monster pit (symbol clone)"
 */
static void vault_prep_symbol(void)
{
    int r_idx;

    /* Apply the monster restriction */
    get_mon_num_prep(vault_aux_simple, NULL);

    /* Pick a race to clone */
    r_idx = get_mon_num(dun_level + 10);

    /* Remove the monster restriction */
    get_mon_num_prep(NULL, NULL);

    /* Extract the symbol */
    vault_aux_char = r_info[r_idx].d_char;
}


/*
 * Helper function for "monster pit (dragon)"
 */
static void vault_prep_dragon(void)
{
    /* Pick dragon type */
    switch (randint0(6))
    {
        /* Black */
        case 0:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_ACID;

            /* Done */
            break;
        }

        /* Blue */
        case 1:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_ELEC;

            /* Done */
            break;
        }

        /* Red */
        case 2:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_FIRE;

            /* Done */
            break;
        }

        /* White */
        case 3:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_COLD;

            /* Done */
            break;
        }

        /* Green */
        case 4:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_POIS;

            /* Done */
            break;
        }

        /* Multi-hued */
        default:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = (RF4_BR_ACID | RF4_BR_ELEC |
                                              RF4_BR_FIRE | RF4_BR_COLD |
                                              RF4_BR_POIS);

            /* Done */
            break;
        }
    }
}


/*
 * Helper function for "monster pit (dark elf)"
 */
static bool vault_aux_dark_elf(int r_idx)
{
    int i;
    static int dark_elf_list[] =
    {
        MON_D_ELF, MON_D_ELF_MAGE, MON_D_ELF_WARRIOR, MON_D_ELF_PRIEST,
        MON_D_ELF_LORD, MON_D_ELF_WARLOCK, MON_D_ELF_DRUID, MON_NIGHTBLADE,
        MON_D_ELF_SORC, MON_D_ELF_SHADE, 0,
    };

    /* Validate the monster */
    if (!vault_monster_okay(r_idx)) return FALSE;

    /* Require dark elves */
    for (i = 0; dark_elf_list[i]; i++)
        if (r_idx == dark_elf_list[i]) return TRUE;

    /* Assume not */
    return FALSE;
}


typedef struct vault_aux_type vault_aux_type;


struct vault_aux_type
{
    cptr name;
    bool (*hook_func)(int r_idx);
    void (*prep_func)(void);
    int level;
    int chance;
};


static int pick_vault_type(vault_aux_type *l_ptr, s16b allow_flag_mask)
{
    int tmp, total, count;

    vault_aux_type *n_ptr;

    /* Calculate the total possibilities */
    for (n_ptr = l_ptr, total = 0, count = 0; TRUE; n_ptr++, count++)
    {
        /* Note end */
        if (!n_ptr->name) break;

        /* Ignore excessive depth */
        if (n_ptr->level > dun_level) continue;

        /* Not matched with pit/nest flag */
        if (!(allow_flag_mask & (1L << count))) continue;

        /* Count this possibility */
        total += n_ptr->chance * MAX_DEPTH / (MIN(dun_level, MAX_DEPTH - 1) - n_ptr->level + 5);
    }

    /* Pick a random type */
    tmp = randint0(total);

    /* Find this type */
    for (n_ptr = l_ptr, total = 0, count = 0; TRUE; n_ptr++, count++)
    {
        /* Note end */
        if (!n_ptr->name) break;

        /* Ignore excessive depth */
        if (n_ptr->level > dun_level) continue;

        /* Not matched with pit/nest flag */
        if (!(allow_flag_mask & (1L << count))) continue;

        /* Count this possibility */
        total += n_ptr->chance * MAX_DEPTH / (MIN(dun_level, MAX_DEPTH - 1) - n_ptr->level + 5);

        /* Found the type */
        if (tmp < total) break;
    }

    return n_ptr->name ? count : -1;
}

static vault_aux_type nest_types[] =
{
    {"clone",        vault_aux_clone,    vault_prep_clone,   5, 3},
    {"jelly",        vault_aux_jelly,    NULL,               5, 6},
    {"symbol good",  vault_aux_symbol_g, vault_prep_symbol, 25, 2},
    {"symbol evil",  vault_aux_symbol_e, vault_prep_symbol, 25, 2},
    {"mimic",        vault_aux_mimic,    NULL,              30, 4},
    {"lovecraftian", vault_aux_cthulhu,  NULL,              70, 2},
    {"kennel",       vault_aux_kennel,   NULL,              45, 4},
    {"animal",       vault_aux_animal,   NULL,              35, 5},
    {"chapel",       vault_aux_chapel_g, NULL,              75, 4},
    {"undead",       vault_aux_undead,   NULL,              75, 5},
    {NULL,           NULL,               NULL,               0, 0},
};

static vault_aux_type pit_types[] =
{
    {"orc",          vault_aux_orc,      NULL,               5, 6},
    {"troll",        vault_aux_troll,    NULL,              20, 6},
    {"giant",        vault_aux_giant,    NULL,              20, 6},
    {"lovecraftian", vault_aux_cthulhu,  NULL,              80, 2},
    {"symbol good",  vault_aux_symbol_g, vault_prep_symbol, 70, 1},
    {"symbol evil",  vault_aux_symbol_e, vault_prep_symbol, 70, 1},
    {"chapel",       vault_aux_chapel_g, NULL,              65, 2},
    {"dragon",       vault_aux_dragon,   vault_prep_dragon, 60, 9},
    {"demon",        vault_aux_demon,    NULL,              80, 6},
    {"dark elf",     vault_aux_dark_elf, NULL,              45, 4},
    {NULL,           NULL,               NULL,               0, 0},
};


/* Nest types code */
#define NEST_TYPE_CLONE        0
#define NEST_TYPE_JELLY        1
#define NEST_TYPE_SYMBOL_GOOD  2
#define NEST_TYPE_SYMBOL_EVIL  3
#define NEST_TYPE_MIMIC        4
#define NEST_TYPE_LOVECRAFTIAN 5
#define NEST_TYPE_KENNEL       6
#define NEST_TYPE_ANIMAL       7
#define NEST_TYPE_CHAPEL       8
#define NEST_TYPE_UNDEAD       9

/* Pit types code */
#define PIT_TYPE_ORC           0
#define PIT_TYPE_TROLL         1
#define PIT_TYPE_GIANT         2
#define PIT_TYPE_LOVECRAFTIAN  3
#define PIT_TYPE_SYMBOL_GOOD   4
#define PIT_TYPE_SYMBOL_EVIL   5
#define PIT_TYPE_CHAPEL        6
#define PIT_TYPE_DRAGON        7
#define PIT_TYPE_DEMON         8
#define PIT_TYPE_DARK_ELF      9

/* A struct for nest monster information with cheat_hear */
typedef struct
{
    s16b r_idx;
    bool used;
}
nest_mon_info_type;



#define NUM_NEST_MON_TYPE 64


/* coordinate translation code */
void coord_trans(int *x, int *y, int xoffset, int yoffset, int transno)
{
    int i;
    int temp;

    /*
     * transno specifies what transformation is required. (0-7)
     * The lower two bits indicate by how much the vault is rotated,
     * and the upper bit indicates a reflection.
     * This is done by using rotation matrices... however since
     * these are mostly zeros for rotations by 90 degrees this can
     * be expressed simply in terms of swapping and inverting the
     * x and y coordinates.
     */
    for (i = 0; i < transno % 4; i++)
    {
        /* rotate by 90 degrees */
        temp = *x;
        *x = -(*y);
        *y = temp;
    }

    if (transno / 4)
    {
        /* Reflect depending on status of 3rd bit. */
        *x = -(*x);
    }

    /* Add offsets so vault stays in the first quadrant */
    *x += xoffset;
    *y += yoffset;
}


static const room_grid_t *_find_room_grid(const room_template_t *room_ptr, char letter)
{
    int i;
    for (i = 0; i < ROOM_MAX_LETTERS; i++)
    {
        if (room_ptr->letters[i].letter == letter)
            return &room_ptr->letters[i];
    }
    return NULL;
}

static bool _obj_kind_is_good = FALSE;
static int _obj_kind_hack = 0;
static bool _obj_kind_hook(int k_idx)
{
    if (_obj_kind_is_good && !kind_is_good(k_idx))
        return FALSE;

    switch (_obj_kind_hack)
    {
    case OBJ_TYPE_DEVICE:       return kind_is_device(k_idx);
    case OBJ_TYPE_JEWELRY:      return kind_is_jewelry(k_idx);
    case OBJ_TYPE_BOOK:         return kind_is_book(k_idx);
    case OBJ_TYPE_BODY_ARMOR:   return kind_is_body_armor(k_idx);
    case OBJ_TYPE_OTHER_ARMOR:  return kind_is_other_armor(k_idx);
    case OBJ_TYPE_WEAPON:       return kind_is_weapon(k_idx);
    case OBJ_TYPE_BOW_AMMO:     return kind_is_bow_ammo(k_idx);
    case OBJ_TYPE_MISC:         return kind_is_misc(k_idx);
    default:                    return k_info[k_idx].tval == _obj_kind_hack;
    }
}

static void _apply_room_grid1(int x, int y, const room_grid_t *grid_ptr, u16b room_flags)
{
    cave_type *c_ptr = &cave[y][x];
    bool       trapped = FALSE;

    /* Feature */
    if (grid_ptr->cave_feat)
    {
        c_ptr->feat = conv_dungeon_feat(grid_ptr->cave_feat);
        c_ptr->info = (c_ptr->info & (CAVE_MASK | CAVE_TEMP)) | grid_ptr->cave_info;

        if (grid_ptr->flags & ROOM_GRID_SPECIAL)
            c_ptr->special = grid_ptr->extra;

        if (have_flag(f_info[c_ptr->feat].flags, FF_STORE))
            store_init(NO_TOWN, f_info[c_ptr->feat].subtype);
    }

    /* Traps */
    if (grid_ptr->cave_trap)
    {
        if (!grid_ptr->trap_pct || randint0(100) < grid_ptr->trap_pct)
        {
            c_ptr->mimic = c_ptr->feat;
            c_ptr->feat = conv_dungeon_feat(grid_ptr->cave_trap);
            trapped = TRUE;
        }
    }
    else if (grid_ptr->flags & ROOM_GRID_TRAP_RANDOM)
    {
        if (!grid_ptr->trap_pct || randint0(100) < grid_ptr->trap_pct)
        {
            place_trap(y, x);
            trapped = TRUE;
        }
    }

    if (trapped)
        return;

    /* Object */
    object_level = base_level + grid_ptr->object_level;
    if (grid_ptr->flags & ROOM_GRID_OBJ_ARTIFACT)
    {
        int a_idx = grid_ptr->object;
        if (no_artifacts)
        {
            int k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_ACQUIREMENT);
            object_type forge;

            object_prep(&forge, k_idx);
            drop_here(&forge, y, x);                
        }
        else if (a_info[a_idx].generated)
        {
            object_type forge;
            create_replacement_art(a_idx, &forge);
            drop_here(&forge, y, x);
        }
        else if (create_named_art(a_idx, y, x))
            a_info[a_idx].generated = TRUE;
    }
    else if (grid_ptr->flags & ROOM_GRID_OBJ_RANDOM)
    {
        int mode = 0;
        if (grid_ptr->flags & ROOM_GRID_ART_RANDOM)
            mode = AM_GOOD | AM_GREAT | AM_SPECIAL | AM_NO_FIXED_ART;
        else if (grid_ptr->flags & ROOM_GRID_EGO_RANDOM)
            mode = AM_GOOD | AM_GREAT | AM_NO_FIXED_ART;
        else if (grid_ptr->object_level)
            mode = AM_GOOD;
        place_object(y, x, mode);
    }
    else if ((grid_ptr->flags & ROOM_GRID_OBJ_TYPE) && grid_ptr->object == TV_GOLD)
    {
        object_type forge;
        if (make_gold(&forge, FALSE))
            drop_here(&forge, y, x);
    }
    else if (grid_ptr->object)
    {
        int k_idx;

        if (grid_ptr->flags & ROOM_GRID_OBJ_TYPE)
        {
            if (grid_ptr->object == TV_JUNK || grid_ptr->object == TV_SKELETON)
                object_level = 1;

            _obj_kind_is_good = FALSE;
            if (grid_ptr->object_level > 0)
                _obj_kind_is_good = TRUE;
            _obj_kind_hack = grid_ptr->object;
            get_obj_num_hook = _obj_kind_hook;
            get_obj_num_prep();
            k_idx = get_obj_num(object_level);
            get_obj_num_hook = NULL;
            get_obj_num_prep();
        }
        else
            k_idx = grid_ptr->object;

        if (k_idx)
        {
            int         mode = 0;
            object_type forge;

            if (grid_ptr->flags & ROOM_GRID_ART_RANDOM)
            {
                mode = AM_GOOD | AM_GREAT | AM_SPECIAL | AM_NO_FIXED_ART;
            }
            else if (grid_ptr->flags & ROOM_GRID_EGO_RANDOM)
            {
                mode = AM_GOOD | AM_GREAT | AM_NO_FIXED_ART;
            }
            else if (grid_ptr->flags & ROOM_GRID_OBJ_EGO)
            {
                mode |= AM_GOOD | AM_GREAT | AM_FORCE_EGO;
                apply_magic_ego = grid_ptr->extra;
            }
            else if (grid_ptr->object_level)
                mode = AM_GOOD;

            object_prep(&forge, k_idx);
            apply_magic(&forge, object_level, mode);
            mass_produce(&forge);
            drop_here(&forge, y, x);
        }
    }
    object_level = base_level;
}

static const room_grid_t *_room_grid_hack = 0;
static u16b _room_flags_hack = 0;
static bool _room_grid_mon_hook(int r_idx)
{
    if (_room_flags_hack & ROOM_THEME_GOOD)
    {
        if (r_info[r_idx].flags3 & RF3_EVIL)
            return FALSE;
    }

    if (_room_flags_hack & ROOM_THEME_EVIL)
    {
        if (r_info[r_idx].flags3 & RF3_GOOD)
            return FALSE;
    }

    if (_room_grid_hack->flags & ROOM_GRID_MON_NO_UNIQUE)
    {
        if (r_info[r_idx].flags1 & RF1_UNIQUE)
            return FALSE;
    }

    if (_room_grid_hack->flags & ROOM_GRID_MON_TYPE)
    {
        if (!mon_is_type(r_idx, _room_grid_hack->monster))
            return FALSE;
    }
    else if (_room_grid_hack->flags & ROOM_GRID_MON_CHAR)
    {
        if (_room_grid_hack->monster != r_info[r_idx].d_char)
            return FALSE;
    }
    else if (_room_grid_hack->flags & ROOM_GRID_MON_RANDOM)
    {
        monster_hook_type hook;
        if (!dun_level && wilderness_mon_hook) /* Hack: get_monster_hook() often returns the incorrect terrain hook for a scrolled wilderness tile.*/
            hook = wilderness_mon_hook;
        else
            hook = get_monster_hook();
        if (hook && !hook(r_idx))
            return FALSE;
    }
    return TRUE;
}


static void _apply_room_grid2(int x, int y, const room_grid_t *grid_ptr, u16b room_flags)
{
    int mode = 0;

    if (!(grid_ptr->flags & ROOM_GRID_MON_NO_GROUP))
        mode |= PM_ALLOW_GROUP;
    if (!(grid_ptr->flags & ROOM_GRID_MON_NO_SLEEP))
        mode |= PM_ALLOW_SLEEP;
    /*if (!(grid_ptr->flags & ROOM_GRID_MON_NO_UNIQUE))
        mode |= PM_ALLOW_UNIQUE;  Note: This flag does not work! */
    if (grid_ptr->flags & ROOM_GRID_MON_HASTE)
        mode |= PM_HASTE;

    if (grid_ptr->flags & ROOM_GRID_MON_FRIENDLY)
        mode |= PM_FORCE_FRIENDLY;
    if (room_flags & ROOM_THEME_FRIENDLY)
        mode |= PM_FORCE_FRIENDLY;

    /* Monsters are allocated on pass 2 to handle group placement, 
       which requires the terrain to be properly laid out. Note that 
       quest files (process_dungeon_file) are broken in this respect. */


    if (!(grid_ptr->flags & ROOM_GRID_MON_RANDOM) && !grid_ptr->monster)
        return;

    /* The NIGHT theme is designed for wilderness cemeteries and 
       such, which should be populated with foul undead, but only
       in the deep, dark hours of night! */
    if ((room_flags & ROOM_THEME_NIGHT) && !dun_level && !p_ptr->inside_quest)
    {
        int day, hour, min;
        extract_day_hour_min(&day, &hour, &min);
        if (hour > 3 && hour < 22)
            return;
    }

    /* Added for symmetry with ROOM_THEME_NIGHT ... any ideas? */
    if ((room_flags & ROOM_THEME_DAY) && !dun_level && !p_ptr->inside_quest)
    {
        int day, hour, min;
        extract_day_hour_min(&day, &hour, &min);
        if (hour < 8 || hour > 18)
            return;
    }

    if (grid_ptr->flags & (ROOM_GRID_MON_TYPE | ROOM_GRID_MON_RANDOM | ROOM_GRID_MON_CHAR))
    {
        int r_idx;
        monster_level = base_level + grid_ptr->monster_level;
        _room_grid_hack = grid_ptr;
        _room_flags_hack = room_flags;
        get_mon_num_prep(_room_grid_mon_hook, get_monster_hook2(y, x));
        r_idx = get_mon_num(monster_level);
        place_monster_aux(0, y, x, r_idx, mode);
        monster_level = base_level;
    }
    else if (grid_ptr->monster)
    {
        place_monster_aux(0, y, x, grid_ptr->monster, mode | PM_NO_KAGE);
    }

    /* TODO: Letters in quest files need extra handling for cloned uniques,
       as well as resurrecting uniques already slain. See process_dungeon_file_aux
       in init1.c for gory details. */
}

#define _MAX_FORMATION 10
static int _formation_monsters[_MAX_FORMATION];

static bool _init_formation(const room_template_t *room_ptr, int x, int y)
{
    const room_grid_t *grid_ptr = _find_room_grid(room_ptr, '0');
    int i, j, n, which;
    monster_type align;

    for (i = 0; i < _MAX_FORMATION; i++)
        _formation_monsters[i] = 0;

    if (!grid_ptr) 
        return FALSE;

    monster_level = base_level + grid_ptr->monster_level;

    if (grid_ptr->flags & (ROOM_GRID_MON_TYPE | ROOM_GRID_MON_CHAR))
    {
        _room_grid_hack = grid_ptr;
        _room_flags_hack = room_ptr->flags;
        get_mon_num_prep(_room_grid_mon_hook, get_monster_hook2(y, x));
    }    
    else if (grid_ptr->flags & ROOM_GRID_MON_RANDOM)
    {
        n = randint0(100);
        if (!dun_level) n = 99; /* Hack: Most nests/pits won't allocate on the surface! */
        if (n < 5)
        {
            which = pick_vault_type(nest_types, d_info[dungeon_type].nest);

            if (which < 0) 
                return FALSE;

            if (nest_types[which].prep_func)
                nest_types[which].prep_func();

            get_mon_num_prep(nest_types[which].hook_func, NULL);
        }
        else if (n < 30)
        {
            which = pick_vault_type(pit_types, d_info[dungeon_type].pit);

            if (which < 0) 
                return FALSE;

            if (pit_types[which].prep_func)
                pit_types[which].prep_func();

            get_mon_num_prep(pit_types[which].hook_func, NULL);
        }
        else if (n < 50)
        {
            room_grid_t grid;
            grid.flags = ROOM_GRID_MON_TYPE;

            switch (randint1(5))
            {
            case 1: grid.monster = SUMMON_KAMIKAZE; break;
            case 2: grid.monster = SUMMON_KNIGHT; break;
            case 3: grid.monster = SUMMON_HUMAN; break;
            case 4: grid.monster = SUMMON_DRAGON; break;
            case 5: grid.monster = SUMMON_THIEF; break;
            }
            _room_grid_hack = &grid;
            _room_flags_hack = room_ptr->flags;
            get_mon_num_prep(_room_grid_mon_hook, get_monster_hook2(y, x));
        }
        else 
        {
            room_grid_t grid;
            int r_idx;

            _room_grid_hack = &grid;
            _room_flags_hack = room_ptr->flags;

            grid.flags = ROOM_GRID_MON_RANDOM;
            get_mon_num_prep(_room_grid_mon_hook, get_monster_hook2(y, x));
            r_idx = get_mon_num(monster_level);

            grid.flags = ROOM_GRID_MON_CHAR;
            grid.monster = r_info[r_idx].d_char;
            get_mon_num_prep(_room_grid_mon_hook, get_monster_hook2(y, x));
            r_idx = get_mon_num(monster_level);
        }
    }

    align.sub_align = SUB_ALIGN_NEUTRAL;
    for (i = 0; i < _MAX_FORMATION; i++)
    {
        int r_idx = 0, attempts = 100;

        while (attempts--)
        {
            r_idx = get_mon_num(monster_level);
            if (!r_idx) continue;
            if (monster_has_hostile_align(&align, 0, 0, &r_info[r_idx])) continue;
            if (r_info[r_idx].flags1 & RF1_UNIQUE) continue;
            if (r_info[r_idx].flags7 & RF7_UNIQUE2) continue;
            if (r_idx == MON_NAZGUL) continue;
            break;
        }

        if (!r_idx || !attempts)
        { 
            /*r_idx = get_mon_num(monster_level);*/
            return FALSE;
        }

        if (r_info[r_idx].flags3 & RF3_EVIL) align.sub_align |= SUB_ALIGN_EVIL;
        if (r_info[r_idx].flags3 & RF3_GOOD) align.sub_align |= SUB_ALIGN_GOOD;

        _formation_monsters[i] = r_idx;
    }
    monster_level = base_level;

    /* (Bubble) Sort the entries */
    for (i = 0; i < _MAX_FORMATION - 1; i++)
    {
        for (j = 0; j < _MAX_FORMATION - 1; j++)
        {
            int i1 = j;
            int i2 = j + 1;

            int p1 = r_info[_formation_monsters[i1]].level;
            int p2 = r_info[_formation_monsters[i2]].level;

            if (p1 < p2)
            {
                int tmp = _formation_monsters[i1];
                _formation_monsters[i1] = _formation_monsters[i2];
                _formation_monsters[i2] = tmp;
            }
        }
    }
    return TRUE;
}

/*
 * Build Rooms from Templates (e.g. Vaults)
 * TODO: Can we clean up the interface a bit?
 */
void build_room_template_aux(const room_template_t *room_ptr, int yval, int xval, int xoffset, int yoffset, int transno)
{
    int dx, dy, x, y, i, j;

    cptr t;
    cptr data = room_text + room_ptr->text;
    int  ymax = room_ptr->height;
    int  xmax = room_ptr->width;

    cave_type *c_ptr;
    const room_grid_t *grid_ptr;
    bool initialized_formation = FALSE;

    /* Place dungeon features and objects */
    for (t = data, dy = 0; dy < ymax; dy++)
    {
        for (dx = 0; dx < xmax; dx++, t++)
        {
            /* prevent loop counter from being overwritten */
            i = dx;
            j = dy;

            /* Flip / rotate */
            coord_trans(&i, &j, xoffset, yoffset, transno);

            /* Extract the location */
            if (transno % 2 == 0)
            {
                /* no swap of x/y */
                x = xval - (xmax / 2) + i;
                y = yval - (ymax / 2) + j;
            }
            else
            {
                /* swap of x/y */
                x = xval - (ymax / 2) + i;
                y = yval - (xmax / 2) + j;
            }

            /* Hack -- skip "non-grids" */
            if (*t == ' ') continue;
            if (!in_bounds2(y, x)) continue;

            /* Access the grid */
            c_ptr = &cave[y][x];

            /* Lay down a floor */
            if (room_ptr->type != ROOM_WILDERNESS && room_ptr->type != ROOM_AMBUSH)
                place_floor_grid(c_ptr);

            /* Remove any mimic */
            c_ptr->mimic = 0;

            /* Part of a vault? */
            if (room_ptr->type == ROOM_VAULT)
                c_ptr->info |= CAVE_ROOM | CAVE_ICKY;
            else if (room_ptr->type != ROOM_WILDERNESS && room_ptr->type != ROOM_AMBUSH)
                c_ptr->info |= CAVE_ROOM;

            grid_ptr = _find_room_grid(room_ptr, *t);
            if (!grid_ptr && (room_ptr->flags & ROOM_THEME_FORMATION) && '0' <= *t && *t <= '9')
                grid_ptr = _find_room_grid(room_ptr, '.'); /* TODO ... */

            if (grid_ptr)
            {
                _apply_room_grid1(x, y, grid_ptr, room_ptr->flags);
                continue;
            }

            /* Analyze the grid */
            switch (*t)
            {
                /* Granite wall (outer) */
            case '%':
                place_outer_noperm_grid(c_ptr);
                break;

                /* Granite wall (inner) */
            case '#':
                place_inner_grid(c_ptr);
                break;

                /* Glass wall (inner) */
            case '$':
                place_inner_grid(c_ptr);
                c_ptr->feat = feat_glass_wall;
                break;

                /* Permanent wall (inner) */
            case 'X':
                place_inner_perm_grid(c_ptr);
                break;

                /* Permanent glass wall (inner) */
            case 'Y':
                place_inner_perm_grid(c_ptr);
                c_ptr->feat = feat_permanent_glass_wall;
                break;

                /* Treasure/trap */
            case '*':
                if (randint0(100) < 75)
                {
                    place_object(y, x, 0L);
                }
                else
                {
                    place_trap(y, x);
                }
                break;

                /* Secret doors */
            case '+':
                if (room_ptr->type == ROOM_WILDERNESS)
                    place_locked_door(y, x);
                else
                    place_secret_door(y, x, DOOR_DEFAULT);
                break;

                /* Secret glass doors */
            case '-':
                place_secret_door(y, x, DOOR_GLASS_DOOR);
                if (is_closed_door(c_ptr->feat)) c_ptr->mimic = feat_glass_wall;
                break;

                /* Curtains */
            case '\'':
                place_secret_door(y, x, DOOR_CURTAIN);
                break;

                /* Trap */
            case '^':
                place_trap(y, x);
                break;

                /* The Pattern */
            case 'p':
                set_cave_feat(y, x, feat_pattern_start);
                break;

            case 'a':
                set_cave_feat(y, x, feat_pattern_1);
                break;

            case 'b':
                set_cave_feat(y, x, feat_pattern_2);
                break;

            case 'c':
                set_cave_feat(y, x, feat_pattern_3);
                break;

            case 'd':
                set_cave_feat(y, x, feat_pattern_4);
                break;

            case 'P':
                set_cave_feat(y, x, feat_pattern_end);
                break;

            case 'B':
                set_cave_feat(y, x, feat_pattern_exit);
                break;

            case 'A':
                /* Reward for Pattern walk */
                object_level = base_level + 12;
                place_object(y, x, AM_GOOD | AM_GREAT);
                object_level = base_level;
                break;
            }
        }
    }

    /* Place dungeon monsters and objects */
    for (t = data, dy = 0; dy < ymax; dy++)
    {
        for (dx = 0; dx < xmax; dx++, t++)
        {
            /* prevent loop counter from being overwritten */
            i = dx;
            j = dy;

            /* Flip / rotate */
            coord_trans(&i, &j, xoffset, yoffset, transno);

            /* Extract the location */
            if (transno % 2 == 0)
            {
                /* no swap of x/y */
                x = xval - (xmax / 2) + i;
                y = yval - (ymax / 2) + j;
            }
            else
            {
                /* swap of x/y */
                x = xval - (ymax / 2) + i;
                y = yval - (xmax / 2) + j;
            }

            /* Hack -- skip "non-grids" */
            if (*t == ' ') continue;
            if (!in_bounds(y, x)) continue;

            /* Monster Formations are a huge, but worthwhile hack 
               '0' to '9' index into the formation array, initialized above.
               User specifies a '0' index in v_info to indicate the type of
               formation (e.g. L:0:MON(ORC, 10))
             */
            if ( (room_ptr->flags & ROOM_THEME_FORMATION)
              && '0' <= *t && *t <= '9' )
            {
                if ((room_ptr->flags & ROOM_THEME_FORMATION) && !initialized_formation)
                {
                    int k;
                    for (k = 0; k < 100; k++)
                    {
                        /* The Old Monster Pits/Nest fail fairly often. For example,
                           when trying to generate a good chapel of vampires! More 
                           commonly, Orc Pits won't work after a certain depth ... */
                        if (_init_formation(room_ptr, x, y)) 
                            break;
                    }
                    initialized_formation = TRUE;
                }
                {
                    int idx = *t - '0';
                    int r_idx = _formation_monsters[idx];

                    if (r_idx)
                    {
                        place_monster_aux(0, y, x, r_idx, PM_NO_KAGE);
                    }
                    continue;
                }
            }

            grid_ptr = _find_room_grid(room_ptr, *t);
            if (grid_ptr)
            {
                _apply_room_grid2(x, y, grid_ptr, room_ptr->flags);
                continue;
            }

            /* Analyze the symbol */
            switch (*t)
            {
                /* Monster */
                case '&':
                {
                    monster_level = base_level + 5;
                    place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                    monster_level = base_level;
                    break;
                }

                /* Meaner monster */
                case '@':
                {
                    if (room_ptr->type == ROOM_AMBUSH)
                    {
                        p_ptr->oldpx = x;
                        p_ptr->oldpy = y;
                    }
                    else
                    {
                        monster_level = base_level + 11;
                        place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                        monster_level = base_level;
                    }
                    break;
                }

                /* Meaner monster, plus treasure */
                case '9':
                {
                    monster_level = base_level + 9;
                    place_monster(y, x, PM_ALLOW_SLEEP);
                    monster_level = base_level;
                    object_level = base_level + 9;
                    place_object(y, x, AM_GOOD);
                    object_level = base_level;
                    break;
                }

                /* Nasty monster and treasure */
                case '8':
                {
                    monster_level = base_level + 40;
                    place_monster(y, x, PM_ALLOW_SLEEP);
                    monster_level = base_level;
                    object_level = base_level + 40;
                    place_object(y, x, AM_GOOD | AM_GREAT);
                    object_level = base_level;
                    break;
                }

                /* Monster and/or object */
                case ',':
                {
                    if (randint0(100) < 50)
                    {
                        monster_level = base_level + 3;
                        place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                        monster_level = base_level;
                    }
                    if (randint0(100) < 50)
                    {
                        object_level = base_level + 7;
                        place_object(y, x, 0L);
                        object_level = base_level;
                    }
                    break;
                }

            }
        }
    }
}

static bool _room_is_allowed(const room_template_t * room_ptr, int type, int subtype)
{
    if (!ironman_rooms && base_level < room_ptr->level) return FALSE;  /* Note: dun_level is 0 for wilderness encounters! */
    if (room_ptr->max_level && room_ptr->max_level < base_level) return FALSE;
    if (room_ptr->type != type) return FALSE;
    if (subtype && room_ptr->subtype != subtype) return FALSE;
    if (!room_ptr->rarity) return FALSE;
    if ((room_ptr->flags & ROOM_SHOP) && !shop_allowed) return FALSE;

    if (!dun_level)
    {
        if ((room_ptr->flags & ROOM_THEME_DAY) && !is_daytime()) return FALSE;
        if ((room_ptr->flags & ROOM_THEME_NIGHT) && is_daytime()) return FALSE;
    }

    switch (dungeon_type)
    {
    case DUNGEON_HEAVEN:
        if (room_ptr->flags & ROOM_THEME_EVIL) return FALSE;
        break;
    case DUNGEON_HELL:
        if (room_ptr->flags & ROOM_THEME_GOOD) return FALSE;
        break;
    }

    return TRUE;
}

room_template_t *choose_room_template(int type, int subtype)
{
    int total = 0;
    int i, n;

    for (i = 0; i < max_room_idx; i++)
    {
        room_template_t *room_ptr = &room_info[i];
        if (!_room_is_allowed(room_ptr, type, subtype)) continue;
        if (room_ptr->flags & ROOM_DEBUG) return room_ptr;
        total += 1000 / room_ptr->rarity;
    }

    if (!total)
        return NULL;

    n = randint1(total);
    for (i = 0; i < max_room_idx; i++)
    {
        room_template_t *room_ptr = &room_info[i];
        if (!_room_is_allowed(room_ptr, type, subtype)) continue;
        n -= 1000 / room_ptr->rarity;
        if (n <= 0)
            return room_ptr;
    }

    return NULL;
}

/*
 * Structure to hold all "fill" data
 */

typedef struct fill_data_type fill_data_type;

struct fill_data_type
{
    /* area size */
    int xmin;
    int ymin;
    int xmax;
    int ymax;

    /* cutoffs */
    int c1;
    int c2;
    int c3;

    /* features to fill with */
    int feat1;
    int feat2;
    int feat3;

    int info1;
    int info2;
    int info3;

    /* number of filled squares */
    int amount;
};

static fill_data_type fill_data;


/* Store routine for the fractal cave generator */
/* this routine probably should be an inline function or a macro. */
static void store_height(int x, int y, int val)
{
    /* if on boundary set val > cutoff so walls are not as square */
    if (((x == fill_data.xmin) || (y == fill_data.ymin) ||
         (x == fill_data.xmax) || (y == fill_data.ymax)) &&
        (val <= fill_data.c1)) val = fill_data.c1 + 1;

    /* store the value in height-map format */
    cave[y][x].feat = val;

    return;
}


/*
* Explanation of the plasma fractal algorithm:
*
* A grid of points is created with the properties of a 'height-map'
* This is done by making the corners of the grid have a random value.
* The grid is then subdivided into one with twice the resolution.
* The new points midway between two 'known' points can be calculated
* by taking the average value of the 'known' ones and randomly adding
* or subtracting an amount proportional to the distance between those
* points. The final 'middle' points of the grid are then calculated
* by averaging all four of the originally 'known' corner points. An
* random amount is added or subtracted from this to get a value of the
* height at that point. The scaling factor here is adjusted to the
* slightly larger distance diagonally as compared to orthogonally.
*
* This is then repeated recursively to fill an entire 'height-map'
* A rectangular map is done the same way, except there are different
* scaling factors along the x and y directions.
*
* A hack to change the amount of correlation between points is done using
* the grd variable. If the current step size is greater than grd then
* the point will be random, otherwise it will be calculated by the
* above algorithm. This makes a maximum distance at which two points on
* the height map can affect each other.
*
* How fractal caves are made:
*
* When the map is complete, a cut-off value is used to create a cave.
* Heights below this value are "floor", and heights above are "wall".
* This also can be used to create lakes, by adding more height levels
* representing shallow and deep water/ lava etc.
*
* The grd variable affects the width of passages.
* The roug variable affects the roughness of those passages
*
* The tricky part is making sure the created cave is connected. This
* is done by 'filling' from the inside and only keeping the 'filled'
* floor. Walls bounding the 'filled' floor are also kept. Everything
* else is converted to the normal _extra_.
 */


/*
 *  Note that this uses the cave.feat array in a very hackish way
 *  the values are first set to zero, and then each array location
 *  is used as a "heightmap"
 *  The heightmap then needs to be converted back into the "feat" format.
 *
 *  grd=level at which fractal turns on. smaller gives more mazelike caves
 *  roug=roughness level. 16=normal. higher values make things more convoluted
 *    small values are good for smooth walls.
 *  size=length of the side of the square cave system.
 */
static void generate_hmap(int y0, int x0, int xsiz, int ysiz, int grd, int roug, int cutoff)
{
    int xhsize, yhsize, xsize, ysize, maxsize;

    /*
     * fixed point variables- these are stored as 256 x normal value
     * this gives 8 binary places of fractional part + 8 places of normal part
     */

    u16b xstep, xhstep, ystep, yhstep;
    u16b xstep2, xhstep2, ystep2, yhstep2;
    u16b i, j, ii, jj, diagsize, xxsize, yysize;
    
    /* Cache for speed */
    u16b xm, xp, ym, yp;

    /* redefine size so can change the value if out of range */
    xsize = xsiz;
    ysize = ysiz;

    /* Paranoia about size of the system of caves */
    if (xsize > 254) xsize = 254;
    if (xsize < 4) xsize = 4;
    if (ysize > 254) ysize = 254;
    if (ysize < 4) ysize = 4;

    /* get offsets to middle of array */
    xhsize = xsize / 2;
    yhsize = ysize / 2;

    /* fix rounding problem */
    xsize = xhsize * 2;
    ysize = yhsize * 2;

    /* get limits of region */
    fill_data.xmin = x0 - xhsize;
    fill_data.ymin = y0 - yhsize;
    fill_data.xmax = x0 + xhsize;
    fill_data.ymax = y0 + yhsize;

    /* Store cutoff in global for quick access */
    fill_data.c1 = cutoff;

    /*
    * Scale factor for middle points:
    * About sqrt(2) * 256 - correct for a square lattice
    * approximately correct for everything else.
     */
    diagsize = 362;

    /* maximum of xsize and ysize */
    maxsize = (xsize > ysize) ? xsize : ysize;

    /* Clear the section */
    for (i = 0; i <= xsize; i++)
    {
        for (j = 0; j <= ysize; j++)
        {
            /* -1 is a flag for "not done yet" */
            cave[(int)(fill_data.ymin + j)][(int)(fill_data.xmin + i)].feat = -1;
            /* Clear icky flag because may be redoing the cave */
            cave[(int)(fill_data.ymin + j)][(int)(fill_data.xmin + i)].info &= ~(CAVE_ICKY);
        }
    }

    /* Boundaries are walls */
    cave[fill_data.ymin][fill_data.xmin].feat = maxsize;
    cave[fill_data.ymax][fill_data.xmin].feat = maxsize;
    cave[fill_data.ymin][fill_data.xmax].feat = maxsize;
    cave[fill_data.ymax][fill_data.xmax].feat = maxsize;

    /* Set the middle square to be an open area. */
    cave[y0][x0].feat = 0;

    /* Initialize the step sizes */
    xstep = xhstep = xsize * 256;
    ystep = yhstep = ysize * 256;
    xxsize = xsize * 256;
    yysize = ysize * 256;

    /*
     * Fill in the rectangle with fractal height data -
     * like the 'plasma fractal' in fractint.
     */
    while ((xhstep > 256) || (yhstep > 256))
    {
        /* Halve the step sizes */
        xstep = xhstep;
        xhstep /= 2;
        ystep = yhstep;
        yhstep /= 2;

        /* cache well used values */
        xstep2 = xstep / 256;
        ystep2 = ystep / 256;

        xhstep2 = xhstep / 256;
        yhstep2 = yhstep / 256;

        /* middle top to bottom. */
        for (i = xhstep; i <= xxsize - xhstep; i += xstep)
        {
            for (j = 0; j <= yysize; j += ystep)
            {
                /* cache often used values */
                ii = i / 256 + fill_data.xmin;
                jj = j / 256 + fill_data.ymin;

                /* Test square */
                if (cave[jj][ii].feat == -1)
                {
                    if (xhstep2 > grd)
                    {
                        /* If greater than 'grid' level then is random */
                        store_height(ii, jj, randint1(maxsize));
                    }
                    else
                    {
                        /* Average of left and right points +random bit */
                        store_height(ii, jj,
                            (cave[jj][fill_data.xmin + (i - xhstep) / 256].feat
                             + cave[jj][fill_data.xmin + (i + xhstep) / 256].feat) / 2
                             + (randint1(xstep2) - xhstep2) * roug / 16);
                    }
                }
            }
        }


        /* middle left to right. */
        for (j = yhstep; j <= yysize - yhstep; j += ystep)
        {
            for (i = 0; i <= xxsize; i += xstep)
            {
                /* cache often used values */
                ii = i / 256 + fill_data.xmin;
                jj = j / 256 + fill_data.ymin;

                /* Test square */
                if (cave[jj][ii].feat == -1)
                {
                    if (xhstep2 > grd)
                    {
                        /* If greater than 'grid' level then is random */
                        store_height(ii, jj, randint1(maxsize));
                    }
                    else
                    {
                        /* Average of up and down points +random bit */
                        store_height(ii, jj,
                            (cave[fill_data.ymin + (j - yhstep) / 256][ii].feat
                            + cave[fill_data.ymin + (j + yhstep) / 256][ii].feat) / 2
                            + (randint1(ystep2) - yhstep2) * roug / 16);
                    }
                }
            }
        }

        /* center. */
        for (i = xhstep; i <= xxsize - xhstep; i += xstep)
        {
            for (j = yhstep; j <= yysize - yhstep; j += ystep)
            {
                /* cache often used values */
                ii = i / 256 + fill_data.xmin;
                jj = j / 256 + fill_data.ymin;

                /* Test square */
                if (cave[jj][ii].feat == -1)
                {
                    if (xhstep2 > grd)
                    {
                        /* If greater than 'grid' level then is random */
                        store_height(ii, jj, randint1(maxsize));
                    }
                    else
                    {
                        /* Cache reused values. */
                        xm = fill_data.xmin + (i - xhstep) / 256;
                        xp = fill_data.xmin + (i + xhstep) / 256;
                        ym = fill_data.ymin + (j - yhstep) / 256;
                        yp = fill_data.ymin + (j + yhstep) / 256;

                        /* 
                         * Average over all four corners + scale by diagsize to
                         * reduce the effect of the square grid on the shape of the fractal
                         */
                        store_height(ii, jj,
                            (cave[ym][xm].feat + cave[yp][xm].feat
                            + cave[ym][xp].feat + cave[yp][xp].feat) / 4
                            + (randint1(xstep2) - xhstep2) * (diagsize / 16) / 256 * roug);
                    }
                }
            }
        }
    }
}


static bool hack_isnt_wall(int y, int x, int c1, int c2, int c3, int feat1, int feat2, int feat3, int info1, int info2, int info3)
{
    /*
     * function used to convert from height-map back to the
     *  normal angband cave format
     */
    if (cave[y][x].info & CAVE_ICKY)
    {
        /* already done */
        return FALSE;
    }
    else
    {
        /* Show that have looked at this square */
        cave[y][x].info|= (CAVE_ICKY);

        /* Use cutoffs c1-c3 to allocate regions of floor /water/ lava etc. */
        if (cave[y][x].feat <= c1)
        {
            /* 25% of the time use the other tile : it looks better this way */
            if (randint1(100) < 75)
            {
                cave[y][x].feat = feat1;
                cave[y][x].info &= ~(CAVE_MASK);
                cave[y][x].info |= info1;
                return TRUE;
            }
            else
            {
                cave[y][x].feat = feat2;
                cave[y][x].info &= ~(CAVE_MASK);
                cave[y][x].info |= info2;
                return TRUE;
            }
        }
        else if (cave[y][x].feat <= c2)
        {
            /* 25% of the time use the other tile : it looks better this way */
            if (randint1(100) < 75)
            {
                cave[y][x].feat = feat2;
                cave[y][x].info &= ~(CAVE_MASK);
                cave[y][x].info |= info2;
                return TRUE;
            }
            else
            {
                cave[y][x].feat = feat1;
                cave[y][x].info &= ~(CAVE_MASK);
                cave[y][x].info |= info1;
                return TRUE;
            }
        }
        else if (cave[y][x].feat <= c3)
        {
            cave[y][x].feat = feat3;
            cave[y][x].info &= ~(CAVE_MASK);
            cave[y][x].info |= info3;
            return TRUE;
        }
        /* if greater than cutoff then is a wall */
        else
        {
            place_outer_bold(y, x);
            return FALSE;
        }
    }
}




/*
 * Quick and nasty fill routine used to find the connected region
 * of floor in the middle of the cave
 */
static void cave_fill(byte y, byte x)
{
    int i, j, d;
    int ty, tx;

    int flow_tail = 1;
    int flow_head = 0;


    /*** Start Grid ***/

    /* Enqueue that entry */
    temp_y[0] = y;
    temp_x[0] = x;


    /* Now process the queue */
    while (flow_head != flow_tail)
    {
        /* Extract the next entry */
        ty = temp_y[flow_head];
        tx = temp_x[flow_head];

        /* Forget that entry */
        if (++flow_head == TEMP_MAX) flow_head = 0;

        /* Add the "children" */
        for (d = 0; d < 8; d++)
        {
            int old_head = flow_tail;

            /* Child location */
            j = ty + ddy_ddd[d];
            i = tx + ddx_ddd[d];

            /* Paranoia Don't leave the cave */
            if (!in_bounds(j, i))
            {
                /* affect boundary */
                cave[j][i].info |= CAVE_ICKY;
/*                return; */
            }

            /* If within bounds */
            else if ((i > fill_data.xmin) && (i < fill_data.xmax)
                && (j > fill_data.ymin) && (j < fill_data.ymax))
            {
                /* If not a wall or floor done before */
                if (hack_isnt_wall(j, i,
                    fill_data.c1, fill_data.c2, fill_data.c3,
                    fill_data.feat1, fill_data.feat2, fill_data.feat3,
                    fill_data.info1, fill_data.info2, fill_data.info3))
                {
                    /* Enqueue that entry */
                    temp_y[flow_tail] = j;
                    temp_x[flow_tail] = i;

                    /* Advance the queue */
                    if (++flow_tail == TEMP_MAX) flow_tail = 0;

                    /* Hack -- Overflow by forgetting new entry */
                    if (flow_tail == flow_head)
                    {
                        flow_tail = old_head;
                    }
                    else
                    {
                        /* keep tally of size of cave system */
                        (fill_data.amount)++;
                    }
                }
            }
            else
            {
                /* affect boundary */
                cave[j][i].info |= CAVE_ICKY;
            }
        }
    }
}


static bool generate_fracave(int y0, int x0, int xsize, int ysize, int cutoff, bool light, bool room)
{
    int x, y, i, xhsize, yhsize;

    /* offsets to middle from corner */
    xhsize = xsize / 2;
    yhsize = ysize / 2;


    /*
     * select region connected to center of cave system
     * this gets rid of alot of isolated one-sqaures that
     * can make teleport traps instadeaths...
     */

    /* cutoffs */
    fill_data.c1 = cutoff;
    fill_data.c2 = 0;
    fill_data.c3 = 0;

    /* features to fill with */
    fill_data.feat1 = floor_type[randint0(100)];
    fill_data.feat2 = floor_type[randint0(100)];
    fill_data.feat3 = floor_type[randint0(100)];

    fill_data.info1 = CAVE_FLOOR;
    fill_data.info2 = CAVE_FLOOR;
    fill_data.info3 = CAVE_FLOOR;

    /* number of filled squares */
    fill_data.amount = 0;

    cave_fill((byte)y0, (byte)x0);

    /* if tally too small, try again */
    if (fill_data.amount < 10)
    {
        /* too small - clear area and try again later */
        for (x = 0; x <= xsize; ++x)
        {
            for (y = 0; y <= ysize; ++y)
            {
                place_extra_bold(y0 + y - yhsize, x0 + x - xhsize);
                cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~(CAVE_ICKY | CAVE_ROOM);
            }
        }
        return FALSE;
    }

    /*
     * Do boundarys-check to see if they are next to a filled region
     * If not then they are set to normal granite
     * If so then they are marked as room walls.
     */
    for (i = 0; i <= xsize; ++i)
    {
        /* top boundary */
        if ((cave[0 + y0 - yhsize][i + x0 - xhsize].info & CAVE_ICKY) && (room))
        {
            /* Next to a 'filled' region? - set to be room walls */
            place_outer_bold(y0 + 0 - yhsize, x0 + i - xhsize);
            if (light) cave[y0 + 0 - yhsize][x0 + i - xhsize].info |= (CAVE_GLOW);
            cave[y0 + 0 - yhsize][x0 + i - xhsize].info |= (CAVE_ROOM);
            place_outer_bold(y0 + 0 - yhsize, x0 + i - xhsize);
        }
        else
        {
            /* set to be normal granite */
            place_extra_bold(y0 + 0 - yhsize, x0 + i - xhsize);
        }

        /* bottom boundary */
        if ((cave[ysize + y0 - yhsize][i + x0 - xhsize].info & CAVE_ICKY) && (room))
        {
            /* Next to a 'filled' region? - set to be room walls */
            place_outer_bold(y0 + ysize - yhsize, x0 + i - xhsize);
            if (light) cave[y0 + ysize - yhsize][x0 + i - xhsize].info|=(CAVE_GLOW);
            cave[y0 + ysize - yhsize][x0 + i - xhsize].info|=(CAVE_ROOM);
            place_outer_bold(y0 + ysize - yhsize, x0 + i - xhsize);
        }
        else
        {
            /* set to be normal granite */
            place_extra_bold(y0 + ysize - yhsize, x0 + i - xhsize);
        }

        /* clear the icky flag-don't need it any more */
        cave[y0 + 0 - yhsize][x0 + i - xhsize].info &= ~(CAVE_ICKY);
        cave[y0 + ysize - yhsize][x0 + i - xhsize].info &= ~(CAVE_ICKY);
    }

    /* Do the left and right boundaries minus the corners (done above) */
    for (i = 1; i < ysize; ++i)
    {
        /* left boundary */
        if ((cave[i + y0 - yhsize][0 + x0 - xhsize].info & CAVE_ICKY) && room)
        {
            /* room boundary */
            place_outer_bold(y0 + i - yhsize, x0 + 0 - xhsize);
            if (light) cave[y0 + i - yhsize][x0 + 0 - xhsize].info |= (CAVE_GLOW);
            cave[y0 + i - yhsize][x0 + 0 - xhsize].info |= (CAVE_ROOM);
            place_outer_bold(y0 + i - yhsize, x0 + 0 - xhsize);
        }
        else
        {
            /* outside room */
            place_extra_bold(y0 + i - yhsize, x0 + 0 - xhsize);
        }
        /* right boundary */
        if ((cave[i + y0 - yhsize][xsize + x0 - xhsize].info & CAVE_ICKY) && room)
        {
            /* room boundary */
            place_outer_bold(y0 + i - yhsize, x0 + xsize - xhsize);
            if (light) cave[y0 + i - yhsize][x0 + xsize - xhsize].info |= (CAVE_GLOW);
            cave[y0 + i - yhsize][x0 + xsize - xhsize].info |= (CAVE_ROOM);
            place_outer_bold(y0 + i - yhsize, x0 + xsize - xhsize);
        }
        else
        {
            /* outside room */
            place_extra_bold(y0 + i - yhsize, x0 + xsize - xhsize);
        }

        /* clear icky flag -done with it */
        cave[y0 + i - yhsize][x0 + 0 - xhsize].info &= ~(CAVE_ICKY);
        cave[y0 + i - yhsize][x0 + xsize - xhsize].info &= ~(CAVE_ICKY);
    }


    /* Do the rest: convert back to the normal format */
    for (x = 1; x < xsize; ++x)
    {
        for (y = 1; y < ysize; ++y)
        {
            if (is_floor_bold(y0 + y - yhsize, x0 + x - xhsize) &&
                (cave[y0 + y - yhsize][x0 + x - xhsize].info & CAVE_ICKY))
            {
                /* Clear the icky flag in the filled region */
                cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~CAVE_ICKY;

                /* Set appropriate flags */
                if (light) cave[y0 + y - yhsize][x0 + x - xhsize].info |= (CAVE_GLOW);
                if (room) cave[y0 + y - yhsize][x0 + x - xhsize].info |= (CAVE_ROOM);
            }
            else if (is_outer_bold(y0 + y - yhsize, x0 + x - xhsize) &&
                 (cave[y0 + y - yhsize][x0 + x - xhsize].info & CAVE_ICKY))
            {
                /* Walls */
                cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~(CAVE_ICKY);
                if (light) cave[y0 + y - yhsize][x0 + x - xhsize].info |= (CAVE_GLOW);
                if (room)
                {
                    cave[y0 + y - yhsize][x0 + x - xhsize].info |= (CAVE_ROOM);
                }
                else
                {

                    place_extra_bold(y0 + y - yhsize, x0 + x - xhsize);
                    cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~(CAVE_ROOM);
                }
            }
            else
            {
                /* Clear the unconnected regions */
                place_extra_bold(y0 + y - yhsize, x0 + x - xhsize);
                cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~(CAVE_ICKY | CAVE_ROOM);
            }
        }
    }

    /*
     * XXX XXX XXX There is a slight problem when tunnels pierce the caves:
     * Extra doors appear inside the system. (Its not very noticeable though.)
     * This can be removed by "filling" from the outside in. This allows a separation
     * from _outer_ with _inner_. (Internal walls are  _outer_ instead.)
     * The extra effort for what seems to be only a minor thing (even non-existant if you
     * think of the caves not as normal rooms, but as holes in the dungeon), doesn't seem
     * worth it.
     */

    return TRUE;
}


/*
 * Driver routine to create fractal cave system
 */
static bool build_type9(void)
{
    int grd, roug, cutoff, xsize, ysize, y0, x0;

    bool done, light, room;

    /* get size: note 'Evenness'*/
    xsize = randint1(22) * 2 + 8;
    ysize = randint1(15) * 2 + 6;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if (!find_space(&y0, &x0, ysize + 1, xsize + 1))
    {
        /* Limit to the minimum room size, and retry */
        xsize = 8;
        ysize = 8;

        /* Find and reserve some space in the dungeon. Get center of room. */
        if (!find_space(&y0, &x0, ysize + 1, xsize + 1))
        {
            /*
             * Still no space?!
             * Try normal room
             */
            return build_type1();
        }
    }

    light = done = FALSE;
    room = TRUE;

    if ((dun_level <= randint1(25)) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS)) light = TRUE;

    while (!done)
    {
        /* Note: size must be even or there are rounding problems
        * This causes the tunnels not to connect properly to the room */

        /* testing values for these parameters feel free to adjust */
        grd = 1 << (randint0(4));

        /* want average of about 16 */
        roug = randint1(8) * randint1(4);

        /* about size/2 */
        cutoff = randint1(xsize / 4) + randint1(ysize / 4) +
             randint1(xsize / 4) + randint1(ysize / 4);

        /* make it */
        generate_hmap(y0, x0, xsize, ysize, grd, roug, cutoff);

        /* Convert to normal format + clean up */
        done = generate_fracave(y0, x0, xsize, ysize, cutoff, light, room);
    }

    return TRUE;
}

#ifdef ALLOW_CAVERNS_AND_LAKES
/*
 * Builds a cave system in the center of the dungeon.
 */
void build_cavern(void)
{
    int grd, roug, cutoff, xsize, ysize, x0, y0;
    bool done, light;

    light = done = FALSE;
    if ((dun_level <= randint1(50)) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS)) light = TRUE;

    /* Make a cave the size of the dungeon */
    xsize = cur_wid - 1;
    ysize = cur_hgt - 1;
    x0 = xsize / 2;
    y0 = ysize / 2;

    /* Paranoia: make size even */
    xsize = x0 * 2;
    ysize = y0 * 2;

    while (!done)
    {
        /* testing values for these parameters: feel free to adjust */
        grd = randint1(4) + 4;

        /* want average of about 16 */
        roug = randint1(8) * randint1(4);

        /* about size/2 */
        cutoff = xsize / 2;

         /* make it */
        generate_hmap(y0 + 1, x0 + 1, xsize, ysize, grd, roug, cutoff);

        /* Convert to normal format+ clean up */
        done = generate_fracave(y0 + 1, x0 + 1, xsize, ysize, cutoff, light, FALSE);
    }
}

static bool generate_lake(int y0, int x0, int xsize, int ysize, int c1, int c2, int c3, int type)
{
    int x, y, i, xhsize, yhsize;
    int feat1, feat2, feat3;

    /* offsets to middle from corner */
    xhsize = xsize / 2;
    yhsize = ysize / 2;

    /* Get features based on type */
    switch (type)
    {
    case LAKE_T_LAVA: /* Lava */
        feat1 = feat_deep_lava;
        feat2 = feat_shallow_lava;
        feat3 = floor_type[randint0(100)];
        break;
    case LAKE_T_WATER: /* Water */
        feat1 = feat_deep_water;
        feat2 = feat_shallow_water;
        feat3 = floor_type[randint0(100)];
        break;
    case LAKE_T_CAVE: /* Collapsed cave */
        feat1 = floor_type[randint0(100)];
        feat2 = floor_type[randint0(100)];
        feat3 = feat_rubble;
        break;
    case LAKE_T_EARTH_VAULT: /* Earth vault */
        feat1 = feat_rubble;
        feat2 = floor_type[randint0(100)];
        feat3 = feat_rubble;
        break;
    case LAKE_T_AIR_VAULT: /* Air vault */
        feat1 = feat_grass;
        feat2 = feat_tree;
        feat3 = feat_grass;
        break;
    case LAKE_T_WATER_VAULT: /* Water vault */
        feat1 = feat_shallow_water;
        feat2 = feat_deep_water;
        feat3 = feat_shallow_water;
        break;
    case LAKE_T_FIRE_VAULT: /* Fire Vault */
        feat1 = feat_shallow_lava;
        feat2 = feat_deep_lava;
        feat3 = feat_shallow_lava;
        break;

    /* Paranoia */
    default: return FALSE;
    }

    /*
     * select region connected to center of cave system
     * this gets rid of alot of isolated one-sqaures that
     * can make teleport traps instadeaths...
     */

    /* cutoffs */
    fill_data.c1 = c1;
    fill_data.c2 = c2;
    fill_data.c3 = c3;

    /* features to fill with */
    fill_data.feat1 = feat1;
    fill_data.feat2 = feat2;
    fill_data.feat3 = feat3;

    fill_data.info1 = 0;
    fill_data.info2 = 0;
    fill_data.info3 = 0;

    /* number of filled squares */
    fill_data.amount = 0;

    /* select region connected to center of cave system
    * this gets rid of alot of isolated one-sqaures that
    * can make teleport traps instadeaths... */
    cave_fill((byte)y0, (byte)x0);

    /* if tally too small, try again */
    if (fill_data.amount < 10)
    {
        /* too small -clear area and try again later */
        for (x = 0; x <= xsize; ++x)
        {
            for (y = 0; y <= ysize; ++y)
            {
                place_floor_bold(y0 + y - yhsize, x0 + x - xhsize);
                cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~(CAVE_ICKY);
            }
        }
        return FALSE;
    }

    /* Do boundarys- set to normal granite */
    for (i = 0; i <= xsize; ++i)
    {
        place_extra_bold(y0 + 0 - yhsize, x0 + i - xhsize);
        place_extra_bold(y0 + ysize - yhsize, x0 + i - xhsize);

        /* clear the icky flag-don't need it any more */
        cave[y0 + 0 - yhsize][x0 + i - xhsize].info &= ~(CAVE_ICKY);
        cave[y0 + ysize - yhsize][x0 + i - xhsize].info &= ~(CAVE_ICKY);
    }

    /* Do the left and right boundaries minus the corners (done above) */

    for (i = 1; i < ysize; ++i)
    {
        place_extra_bold(y0 + i - yhsize, x0 + 0 - xhsize);
        place_extra_bold(y0 + i - yhsize, x0 + xsize - xhsize);

        /* clear icky flag -done with it */
        cave[y0 + i - yhsize][x0 + 0 - xhsize].info &= ~(CAVE_ICKY);
        cave[y0 + i - yhsize][x0 + xsize - xhsize].info &= ~(CAVE_ICKY);
    }


    /* Do the rest: convert back to the normal format */
    for (x = 1; x < xsize; ++x)
    {
        for (y = 1; y < ysize; ++y)
        {
            /* Fill unconnected regions with granite */
            if ((!(cave[y0 + y - yhsize][x0 + x - xhsize].info & CAVE_ICKY)) ||
                is_outer_bold(y0 + y - yhsize, x0 + x - xhsize))
                place_extra_bold(y0 + y - yhsize, x0 + x - xhsize);

            /* turn off icky flag (no longer needed.) */
            cave[y0 + y - yhsize][x0 + x - xhsize].info &= ~(CAVE_ICKY | CAVE_ROOM);

            /* Light lava */
            if (cave_have_flag_bold(y0 + y - yhsize, x0 + x - xhsize, FF_LAVA))
            {
                if (!(d_info[dungeon_type].flags1 & DF1_DARKNESS)) cave[y0 + y - yhsize][x0 + x - xhsize].info |= CAVE_GLOW;
            }
        }
    }

    return TRUE;
}


/*
 * makes a lake/collapsed cave system in the center of the dungeon
 */
void build_lake(int type)
{
    int grd, roug, xsize, ysize, x0, y0;
    bool done = FALSE;
    int c1, c2, c3;

    /* paranoia - exit if lake type out of range. */
    if ((type < LAKE_T_LAVA) || (type > LAKE_T_FIRE_VAULT))
    {
        msg_format("Invalid lake type (%d)", type);
        return;
    }

    /* Make the size of the dungeon */
    xsize = cur_wid - 1;
    ysize = cur_hgt - 1;
    x0 = xsize / 2;
    y0 = ysize / 2;

    /* Paranoia: make size even */
    xsize = x0 * 2;
    ysize = y0 * 2;

    while (!done)
    {
        /* testing values for these parameters: feel free to adjust */
        grd = randint1(3) + 4;

        /* want average of about 16 */
        roug = randint1(8) * randint1(4);

        /* Make up size of various componants */
        /* Floor */
        c3 = 3 * xsize / 4;

        /* Deep water/lava */
        c1 = randint0(c3 / 2) + randint0(c3 / 2) - 5;

        /* Shallow boundary */
        c2 = (c1 + c3) / 2;

        /* make it */
        generate_hmap(y0 + 1, x0 + 1, xsize, ysize, grd, roug, c3);

        /* Convert to normal format+ clean up */
        done = generate_lake(y0 + 1, x0 + 1, xsize, ysize, c1, c2, c3, type);
    }
}
#endif /* ALLOW_CAVERNS_AND_LAKES */

/*
 * Routine that fills the empty areas of a room with treasure and monsters.
 */
static void fill_treasure(int x1, int x2, int y1, int y2, int difficulty)
{
    int x, y, cx, cy, size;
    s32b value;

    /* center of room:*/
    cx = (x1 + x2) / 2;
    cy = (y1 + y2) / 2;

    /* Rough measure of size of vault= sum of lengths of sides */
    size = abs(x2 - x1) + abs(y2 - y1);

    for (x = x1; x <= x2; x++)
    {
        for (y = y1; y <= y2; y++)
        {
            /* Thing added based on distance to center of vault
             * Difficulty is 1-easy to 10-hard */
            value = ((((s32b)(distance(cx, cy, x, y))) * 100) / size) + randint1(10) - difficulty;

            /* hack- empty square part of the time */
            if ((randint1(100) - difficulty * 3) > 50) value = 20;

             /* if floor, shallow water and lava */
            if (is_floor_bold(y, x) ||
                (cave_have_flag_bold(y, x, FF_PLACE) && cave_have_flag_bold(y, x, FF_DROP)))
            {
                /* The smaller 'value' is, the better the stuff */
                if (value < 0)
                {
                    /* Meanest monster + treasure */
                    monster_level = base_level + 40;
                    place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                    monster_level = base_level;
                    object_level = base_level + 20;
                    place_object(y, x, AM_GOOD);
                    object_level = base_level;
                }
                else if (value < 5)
                {
                    /* Mean monster +treasure */
                    monster_level = base_level + 20;
                    place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                    monster_level = base_level;
                    object_level = base_level + 10;
                    place_object(y, x, AM_GOOD);
                    object_level = base_level;
                }
                else if (value < 10)
                {
                    /* Monster */
                    monster_level = base_level + 9;
                    place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                    monster_level = base_level;
                }
                else if (value < 17)
                {
                    /* Intentional Blank space */

                    /*
                     * (Want some of the vault to be empty
                     * so have room for group monsters.
                     * This is used in the hack above to lower
                     * the density of stuff in the vault.)
                     */
                }
                else if (value < 23)
                {
                    /* Object or trap */
                    if (randint0(100) < 25)
                    {
                        place_object(y, x, 0L);
                    }
                    else
                    {
                        place_trap(y, x);
                    }
                }
                else if (value < 30)
                {
                    /* Monster and trap */
                    monster_level = base_level + 5;
                    place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                    monster_level = base_level;
                    place_trap(y, x);
                }
                else if (value < 40)
                {
                    /* Monster or object */
                    if (randint0(100) < 50)
                    {
                        monster_level = base_level + 3;
                        place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                        monster_level = base_level;
                    }
                    if (randint0(100) < 50)
                    {
                        object_level = base_level + 7;
                        place_object(y, x, 0L);
                        object_level = base_level;
                    }
                }
                else if (value < 50)
                {
                    /* Trap */
                    place_trap(y, x);
                }
                else
                {
                    /* Various Stuff */

                    /* 20% monster, 40% trap, 20% object, 20% blank space */
                    if (randint0(100) < 20)
                    {
                        place_monster(y, x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
                    }
                    else if (randint0(100) < 50)
                    {
                        place_trap(y, x);
                    }
                    else if (randint0(100) < 50)
                    {
                        place_object(y, x, 0L);
                    }
                }

            }
        }
    }
}

/*
 * maze vault -- rectangular labyrinthine rooms
 *
 * maze vault uses two routines:
 *    r_visit - a recursive routine that builds the labyrinth
 *    build_maze_vault - a driver routine that calls r_visit and adds
 *                   monsters, traps and treasure
 *
 * The labyrinth is built by creating a spanning tree of a graph.
 * The graph vertices are at
 *    (x, y) = (2j + x1, 2k + y1)   j = 0,...,m-1    k = 0,...,n-1
 * and the edges are the vertical and horizontal nearest neighbors.
 *
 * The spanning tree is created by performing a suitably randomized
 * depth-first traversal of the graph. The only adjustable parameter
 * is the randint0(3) below; it governs the relative density of
 * twists and turns in the labyrinth: smaller number, more twists.
 */
static void r_visit(int y1, int x1, int y2, int x2,
            int node, int dir, int *visited)
{
    int i, j, m, n, temp, x, y, adj[4];

    /* dimensions of vertex array */
    m = (x2 - x1) / 2 + 1;
    n = (y2 - y1) / 2 + 1;

    /* mark node visited and set it to a floor */
    visited[node] = 1;
    x = 2 * (node % m) + x1;
    y = 2 * (node / m) + y1;
    place_floor_bold(y, x);

    /* setup order of adjacent node visits */
    if (one_in_(3))
    {
        /* pick a random ordering */
        for (i = 0; i < 4; i++)
            adj[i] = i;
        for (i = 0; i < 4; i++)
        {
            j = randint0(4);
            temp = adj[i];
            adj[i] = adj[j];
            adj[j] = temp;
        }
        dir = adj[0];
    }
    else
    {
        /* pick a random ordering with dir first */
        adj[0] = dir;
        for (i = 1; i < 4; i++)
            adj[i] = i;
        for (i = 1; i < 4; i++)
        {
            j = 1 + randint0(3);
            temp = adj[i];
            adj[i] = adj[j];
            adj[j] = temp;
        }
    }

    for (i = 0; i < 4; i++)
    {
        switch (adj[i])
        {
            case 0:
                /* (0,+) - check for bottom boundary */
                if ((node / m < n - 1) && (visited[node + m] == 0))
                {
                    place_floor_bold(y + 1, x);
                    r_visit(y1, x1, y2, x2, node + m, dir, visited);
                }
                break;
            case 1:
                /* (0,-) - check for top boundary */
                if ((node / m > 0) && (visited[node - m] == 0))
                {
                    place_floor_bold(y - 1, x);
                    r_visit(y1, x1, y2, x2, node - m, dir, visited);
                }
                break;
            case 2:
                /* (+,0) - check for right boundary */
                if ((node % m < m - 1) && (visited[node + 1] == 0))
                {
                    place_floor_bold(y, x + 1);
                    r_visit(y1, x1, y2, x2, node + 1, dir, visited);
                }
                break;
            case 3:
                /* (-,0) - check for left boundary */
                if ((node % m > 0) && (visited[node - 1] == 0))
                {
                    place_floor_bold(y, x - 1);
                    r_visit(y1, x1, y2, x2, node - 1, dir, visited);
                }
        } /* end switch */
    }
}


void build_maze_vault(int x0, int y0, int xsize, int ysize, bool is_vault)
{
    int y, x, dy, dx;
    int y1, x1, y2, x2;
    int m, n, num_vertices, *visited;
    bool light;
    cave_type *c_ptr;


    if (cheat_room && is_vault) msg_print("Maze Vault");

    /* Choose lite or dark */
    light = ((dun_level <= randint1(25)) && is_vault && !(d_info[dungeon_type].flags1 & DF1_DARKNESS));

    /* Pick a random room size - randomized by calling routine */
    dy = ysize / 2 - 1;
    dx = xsize / 2 - 1;

    y1 = y0 - dy;
    x1 = x0 - dx;
    y2 = y0 + dy;
    x2 = x0 + dx;

    /* generate the room */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            c_ptr = &cave[y][x];
            c_ptr->info |= CAVE_ROOM;
            if (is_vault) c_ptr->info |= CAVE_ICKY;
            if ((x == x1 - 1) || (x == x2 + 1) || (y == y1 - 1) || (y == y2 + 1))
            {
                place_outer_grid(c_ptr);
            }
            else if (!is_vault)
            {
                place_extra_grid(c_ptr);
            }
            else
            {
                place_inner_grid(c_ptr);
            }
            if (light) c_ptr->info |= (CAVE_GLOW);
        }
    }

    /* dimensions of vertex array */
    m = dx + 1;
    n = dy + 1;
    num_vertices = m * n;

    /* initialize array of visited vertices */
    C_MAKE(visited, num_vertices, int);

    /* traverse the graph to create a spaning tree, pick a random root */
    r_visit(y1, x1, y2, x2, randint0(num_vertices), 0, visited);

    /* Fill with monsters and treasure, low difficulty */
    if (is_vault) fill_treasure(x1, x2, y1, y2, randint1(5));

    C_KILL(visited, num_vertices, int);
}

/*
 * Add outer wall to a floored region
 * Note: no range checking is done so must be inside dungeon
 * This routine also stomps on doors
 */
static void add_outer_wall(int x, int y, int light, int x1, int y1, int x2, int y2)
{
    cave_type *c_ptr;
    feature_type *f_ptr;
    int i, j;

    if (!in_bounds(y, x)) return;

    c_ptr = &cave[y][x];

    /* hack- check to see if square has been visited before
    * if so, then exit (use room flag to do this) */
    if (c_ptr->info & CAVE_ROOM) return;

    /* set room flag */
    c_ptr->info |= CAVE_ROOM;

    f_ptr = &f_info[c_ptr->feat];

    if (is_floor_bold(y, x))
    {
        for (i = -1; i <= 1; i++)
        {
            for (j = -1; j <= 1; j++)
            {
                if ((x + i >= x1) && (x + i <= x2) &&
                     (y + j >= y1) && (y + j <= y2))
                {
                    add_outer_wall(x + i, y + j, light, x1, y1, x2, y2);
                    if (light) c_ptr->info |= CAVE_GLOW;
                }
            }
        }
    }
    else if (is_extra_bold(y, x))
    {
        /* Set bounding walls */
        place_outer_bold(y, x);
        if (light) c_ptr->info |= CAVE_GLOW;
    }
    else if (permanent_wall(f_ptr))
    {
        /* Set bounding walls */
        if (light) c_ptr->info |= CAVE_GLOW;
    }
}


/*
 * Hacked distance formula - gives the 'wrong' answer.
 * Used to build crypts
 */
static int dist2(int x1, int y1, int x2, int y2,
         int h1, int h2, int h3, int h4)
{
    int dx, dy;
    dx = abs(x2 - x1);
    dy = abs(y2 - y1);

    /* Basically this works by taking the normal pythagorean formula
     * and using an expansion to express this in a way without the
     * square root. This approximate formula is then perturbed to give
     * the distorted results. (I found this by making a mistake when I was
     * trying to fix the circular rooms.)
     */

    /* h1-h4 are constants that describe the metric */
    if (dx >= 2 * dy) return (dx + (dy * h1) / h2);
    if (dy >= 2 * dx) return (dy + (dx * h1) / h2);
    return (((dx + dy) * 128) / 181 +
        (dx * dx / (dy * h3) + dy * dy / (dx * h3)) * h4);
    /* 128/181 is approx. 1/sqrt(2) */
}

/*
 * Build crypt room.
 * For every grid in the possible square, check the (fake) distance.
 * If it's less than the radius, make it a room square.
 *
 * When done fill from the inside to find the walls,
 */
static bool build_type12(void)
{
    int rad, x, y, x0, y0;
    int light = FALSE;
    bool emptyflag = TRUE;

    /* Make a random metric */
    int h1, h2, h3, h4;
    h1 = randint1(32) - 16;
    h2 = randint1(16);
    h3 = randint1(32);
    h4 = randint1(32) - 16;

    /* Occasional light */
    if ((randint1(dun_level) <= 5) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS)) light = TRUE;

    rad = randint1(9);

    /* Find and reserve some space in the dungeon. Get center of room. */
    if (!find_space(&y0, &x0, rad * 2 + 3, rad * 2 + 3)) return FALSE;

    /* Make floor */
    for (x = x0 - rad; x <= x0 + rad; x++)
    {
        for (y = y0 - rad; y <= y0 + rad; y++)
        {
            /* clear room flag */
            cave[y][x].info &= ~(CAVE_ROOM);

            if (dist2(y0, x0, y, x, h1, h2, h3, h4) <= rad - 1)
            {
                /* inside - so is floor */
                place_floor_bold(y, x);
            }
            else if (distance(y0, x0, y, x) < 3)
            {
                place_floor_bold(y, x);
            }
            else
            {
                /* make granite outside so arena works */
                place_extra_bold(y, x);
            }

            /* proper boundary for arena */
            if (((y + rad) == y0) || ((y - rad) == y0) ||
                ((x + rad) == x0) || ((x - rad) == x0))
            {
                place_extra_bold(y, x);
            }
        }
    }

    /* Find visible outer walls and set to be FEAT_OUTER */
    add_outer_wall(x0, y0, light, x0 - rad - 1, y0 - rad - 1,
               x0 + rad + 1, y0 + rad + 1);

    /* Check to see if there is room for an inner vault */
    for (x = x0 - 2; x <= x0 + 2; x++)
    {
        for (y = y0 - 2; y <= y0 + 2; y++)
        {
            if (!is_floor_bold(y, x))
            {
                /* Wall in the way */
                emptyflag = FALSE;
            }
        }
    }

    if (emptyflag && one_in_(2))
    {
        /* Build the vault */
        build_small_room(x0, y0);

        /* Place a treasure in the vault */
        place_object(y0, x0, 0L);

        /* Let's guard the treasure well */
        vault_monsters(y0, x0, randint0(2) + 3);

        /* Traps naturally */
        vault_traps(y0, x0, 4, 4, randint0(3) + 2);
    }

    return TRUE;
}


/*
 * Type 14 -- trapped rooms
 *
 * A special trap is placed at center of the room
 */
static bool build_type14(void)
{
    int y, x, y2, x2, yval, xval;
    int y1, x1, xsize, ysize;

    bool light;

    cave_type *c_ptr;
    s16b trap;

    /* Pick a room size */
    y1 = randint1(4);
    x1 = randint1(11);
    y2 = randint1(3);
    x2 = randint1(11);

    xsize = x1 + x2 + 1;
    ysize = y1 + y2 + 1;

    /* Find and reserve some space in the dungeon. Get center of room. */
    if (!find_space(&yval, &xval, ysize + 2, xsize + 2)) return FALSE;

    /* Choose lite or dark */
    light = ((dun_level <= randint1(25)) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS));


    /* Get corner values */
    y1 = yval - ysize / 2;
    x1 = xval - xsize / 2;
    y2 = yval + (ysize - 1) / 2;
    x2 = xval + (xsize - 1) / 2;


    /* Place a full floor under the room */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            c_ptr = &cave[y][x];
            place_floor_grid(c_ptr);
            c_ptr->info |= (CAVE_ROOM);
            if (light) c_ptr->info |= (CAVE_GLOW);
        }
    }

    /* Walls around the room */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        c_ptr = &cave[y][x1 - 1];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y][x2 + 1];
        place_outer_grid(c_ptr);
    }
    for (x = x1 - 1; x <= x2 + 1; x++)
    {
        c_ptr = &cave[y1 - 1][x];
        place_outer_grid(c_ptr);
        c_ptr = &cave[y2 + 1][x];
        place_outer_grid(c_ptr);
    }

    if (dun_level < 30 + randint1(30))
        trap = feat_trap_piranha;
    else
        trap = feat_trap_armageddon;

    /* Place a special trap */
    c_ptr = &cave[rand_spread(yval, ysize/4)][rand_spread(xval, xsize/4)];
    c_ptr->mimic = c_ptr->feat;
    c_ptr->feat = trap;

    /* Message */
    if (cheat_room)
    {
        msg_format("Room of %s", f_name + f_info[trap].name);
    }

    return TRUE;
}

static bool build_type16(void)
{
    int rad, x, y, x0, y0, r_idx;
    int light = FALSE;

    /* Occasional light */
    if ((randint1(dun_level) <= 15) && !(d_info[dungeon_type].flags1 & DF1_DARKNESS)) light = TRUE;

    rad = 3+randint0(5);

    /* Find and reserve some space in the dungeon. Get center of room. */
    if (!find_space(&y0, &x0, rad * 2 + 1, rad * 2 + 1)) return FALSE;

    /* Make circular floor */
    for (x = x0 - rad; x <= x0 + rad; x++)
    {
        for (y = y0 - rad; y <= y0 + rad; y++)
        {
            if (distance(y0, x0, y, x) <= rad - 1)
            {
                /* inside- so is floor */
                place_floor_bold(y, x);
            }
            else if (distance(y0, x0, y, x) <= rad + 1)
            {
                /* make granite outside so arena works */
                place_extra_bold(y, x);
            }
        }
    }

    r_idx = get_mon_num(dun_level);
    if (r_idx) place_monster_aux(0, y0, x0, r_idx, PM_ALLOW_SLEEP);

    /* Find visible outer walls and set to be FEAT_OUTER */
    add_outer_wall(x0, y0, light, x0 - rad, y0 - rad, x0 + rad, y0 + rad);

    return TRUE;
}

/*
 * Attempt to build a room of the given type at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
static bool room_build(int typ)
{
    if (dungeon_type == DUNGEON_ARENA)
        return build_type16();

    /* Build a room */
    switch (typ)
    {
    case ROOM_T_FRACAVE:       return build_type9();
    case ROOM_T_NORMAL:        return one_in_(3) ? build_type2() : build_type1();

    case ROOM_T_TRAP:          return build_type14();
    case ROOM_T_CRYPT:         return build_type12();

    /* I think rooms should be specified with templates where possible ... */
    case ROOM_T_TRAP_PIT:      /*return build_type13(); cf N:509*/
    case ROOM_T_GLASS:         /*return build_type15(); I never liked these ... */
    case ROOM_T_OVERLAP:       /*return build_type2();*/
    case ROOM_T_CROSS:         /*return build_type3();*/
    case ROOM_T_INNER_FEAT:    /*return build_type4();*/
    case ROOM_T_NEST:          /*return build_type5(); ROOM_THEME_FORMATION*/
    case ROOM_T_PIT:           /*return build_type6(); ROOM_THEME_FORMATION*/
    case ROOM_T_OVAL:          /*return build_type11();*/
    case ROOM_T_TEMPLATE:      return build_room_template(ROOM_NORMAL, 0);

    /* Of course, vaults have always been templates! */
    case ROOM_T_RANDOM_VAULT:  /*return build_type10();*/
    case ROOM_T_LESSER_VAULT:  return build_room_template(ROOM_VAULT, VAULT_LESSER);
    case ROOM_T_GREATER_VAULT: return build_room_template(ROOM_VAULT, VAULT_GREATER);
    }

    /* Paranoia */
    return FALSE;
}


#define MOVE_PLIST(dst, src) (prob_list[dst] += prob_list[src], prob_list[src] = 0)

/*
 * [from SAngband (originally from OAngband)]
 * 
 * Generate rooms in dungeon. Build bigger rooms at first.
 */
bool generate_rooms(void)
{
    int i;
    bool remain;
    int crowded = 0;
    int total_prob;
    int prob_list[ROOM_T_MAX];
    int rooms_built = 0;
    int area_size = 100 * (cur_hgt*cur_wid) / (MAX_HGT*MAX_WID);
    int level_index = MIN(10, div_round(dun_level, 10));

    /* Number of each type of room on this level */
    s16b room_num[ROOM_T_MAX];

    /* Limit number of rooms */
    int dun_rooms = rand_range(10, 25) * area_size / 100;

    /* Assume normal cave */
    room_info_type *room_info_ptr = room_info_normal;

    shop_allowed = TRUE;

    if (dun_rooms < 5)
        dun_rooms = 5;

    /*
     * Initialize probability list.
     */
    for (i = 0; i < ROOM_T_MAX; i++)
    {
        /* No rooms allowed above their minimum depth. */
        if (dun_level < room_info_ptr[i].min_level)
        {
            prob_list[i] = 0;
        }
        else
        {
            prob_list[i] = room_info_ptr[i].prob[level_index];
        }
    }

    /*
     * XXX -- Various dungeon types and options.
     */

    /* Ironman sees only Greater Vaults */
    if (ironman_rooms && !((d_info[dungeon_type].flags1 & (DF1_BEGINNER | DF1_CHAMELEON | DF1_SMALLEST))))
    {
        for (i = 0; i < ROOM_T_MAX; i++)
        {
            if (i == ROOM_T_GREATER_VAULT) prob_list[i] = 1;
            else prob_list[i] = 0;
        }
    }

    /* Forbidden vaults */
    else if (d_info[dungeon_type].flags1 & DF1_NO_VAULT)
    {
        prob_list[ROOM_T_LESSER_VAULT] = 0;
        prob_list[ROOM_T_GREATER_VAULT] = 0;
        prob_list[ROOM_T_RANDOM_VAULT] = 0;
    }


    /* NO_CAVE dungeon (Castle)*/
    if (d_info[dungeon_type].flags1 & DF1_NO_CAVE)
    {
        MOVE_PLIST(ROOM_T_NORMAL, ROOM_T_FRACAVE);
        MOVE_PLIST(ROOM_T_INNER_FEAT, ROOM_T_CRYPT);
        MOVE_PLIST(ROOM_T_INNER_FEAT, ROOM_T_OVAL);
    }

    /* CAVE dungeon (Orc cave etc.) */
    else if (d_info[dungeon_type].flags1 & DF1_CAVE)
    {
        MOVE_PLIST(ROOM_T_FRACAVE, ROOM_T_NORMAL);
    }

    /* No caves when a (random) cavern exists: they look bad */
    else if (dun->cavern || dun->empty_level)
    {
        prob_list[ROOM_T_FRACAVE] = 0;
    }

    /* Forbidden glass rooms */
    if (!(d_info[dungeon_type].flags1 & DF1_GLASS_ROOM))
    {
        prob_list[ROOM_T_GLASS] = 0;
    }

    /*
     * Initialize number of rooms,
     * And calcurate total probability.
     */
    for (total_prob = 0, i = 0; i < ROOM_T_MAX; i++)
    {
        room_num[i] = 0;
        total_prob += prob_list[i];
    }

    /*
     * Prepare the number of rooms, of all types, we should build
     * on this level.
     */
    for (i = dun_rooms; i > 0; i--)
    {
        int room_type;
        int rand = randint0(total_prob);

        /* Get room_type randomly */
        for (room_type = 0; room_type < ROOM_T_MAX; room_type++)
        {
            if (rand < prob_list[room_type]) break;
            else rand -= prob_list[room_type];
        }

        /* Paranoia */
        if (room_type >= ROOM_T_MAX) room_type = ROOM_T_NORMAL;

        /* Increase the number of rooms of that type we should build. */
        room_num[room_type]++;
    }

    /*
     * Build each type of room one by one until we cannot build any more.
     * [from SAngband (originally from OAngband)]
     */
    while (TRUE)
    {
        /* Assume no remaining rooms */
        remain = FALSE;

        for (i = 0; i < ROOM_T_MAX && rooms_built < dun_rooms; i++)
        {
            /* What type of room are we building now? */
            int room_type = room_build_order[i];

            /* Go next if none available */
            if (!room_num[room_type]) continue;

            /* Use up one unit */
            room_num[room_type]--;

            /* Build the room. */
            if (room_build(room_type))
            {
                /* Increase the room built count. */
                rooms_built++;

                /* Mark as there was some remaining rooms */
                remain = TRUE;

                switch (room_type)
                {
                case ROOM_T_PIT:
                case ROOM_T_NEST:
                case ROOM_T_TRAP_PIT:

                    /* Avoid too many monsters */
                    if (++crowded >= 2)
                    {
                        room_num[ROOM_T_PIT] = 0;
                        room_num[ROOM_T_NEST] = 0;
                        room_num[ROOM_T_TRAP_PIT] = 0;
                    }
                }
            }
        }

        /* End loop if no room remain */
        if (!remain) break;
    }

    /* Hack: Wilderness also picks room templates ... */
    shop_allowed = TRUE;

    if (rooms_built < 1) return FALSE;

    if (cheat_room)
    {
        msg_format("Number of Rooms: %d (%d)", rooms_built, dun_rooms);
    }

    return TRUE;
}
