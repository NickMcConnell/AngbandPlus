/* File: cave.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/utilities.h"
#include "src/init.h"
#include "tilebag.h"
#include "src/hotkeys.h"
#include <QtCore/qmath.h>

/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@thangorodrim.net)
 */

/*
 * Hack -- function hook to point to the right "get_energy_*flow*" function
 */
static int (*get_energy_to_move)(int y, int x, byte which_flow, u32b elem_flag);


/*
 * Approximate distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
int distance(int y1, int x1, int y2, int x2)
{
    int ay, ax;

    /* Find the absolute y/x distance components */
    ay = (y1 > y2) ? (y1 - y2) : (y2 - y1);
    ax = (x1 > x2) ? (x1 - x2) : (x2 - x1);

    /* Hack -- approximate the distance */
    return ((ay > ax) ? (ay + (ax>>1)) : (ax + (ay>>1)));
}

/*
 * Distance between two squares using Pythagorean theorum
 */
int distance_pythagorean(int y1, int x1, int y2, int x2)
{
    qreal distance = qSqrt(qPow((y1 - y2), 2) + qPow((x1 - x2), 2));

    int int_distance = distance;

    /* Hack -- approximate the distance */
    return (int_distance);
}


/*
 * A simple, fast, integer-based line-of-sight algorithm.  By Joseph Hall,
 * 4116 Brewster Drive, Raleigh NC 27606.  Email to jnh@ecemwl.ncsu.edu.
 *
 * This function returns TRUE if a "line of sight" can be traced from the
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
bool generic_los(int y1, int x1, int y2, int x2, u16b flg)
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

    /* Paranoia */
    if (!flg) flg = CAVE_LOS;

    /* Extract the offset */
    dy = y2 - y1;
    dx = x2 - x1;

    /* Extract the absolute offset */
    ay = ABS(dy);
    ax = ABS(dx);

    /* Handle adjacent (or identical) grids */
    if ((ax < 2) && (ay < 2)) return (TRUE);

    /* Directly South/North */
    if (!dx)
    {
        /* South -- check for the flag */
        if (dy > 0)
        {
            for (ty = y1 + 1; ty < y2; ty++)
            {
                if (!cave_flag_bold(ty, x1, flg)) return (FALSE);
            }
        }

        /* North -- check for the flag  */
        else
        {
            for (ty = y1 - 1; ty > y2; ty--)
            {
                if (!cave_flag_bold(ty, x1, flg)) return (FALSE);
            }
        }

        /* Assume los */
        return (TRUE);
    }

    /* Directly East/West */
    if (!dy)
    {
        /* East -- check for the flag  */
        if (dx > 0)
        {
            for (tx = x1 + 1; tx < x2; tx++)
            {
                if (!cave_flag_bold(y1, tx, flg)) return (FALSE);
            }
        }

        /* West -- check for the flag  */
        else
        {
            for (tx = x1 - 1; tx > x2; tx--)
            {
                if (!cave_flag_bold(y1, tx, flg)) return (FALSE);
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
            if (cave_flag_bold(y1 + sy, x1, flg)) return (TRUE);
        }
    }

    /* Horizontal "knights" */
    else if (ay == 1)
    {
        if (ax == 2)
        {
            if (cave_flag_bold(y1, x1 + sx, flg)) return (TRUE);
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
            if (!cave_flag_bold(ty, tx, flg)) return (FALSE);

            qy += m;

            if (qy < f2)
            {
                tx += sx;
            }
            else if (qy > f2)
            {
                ty += sy;
                if (!cave_flag_bold(ty, tx, flg)) return (FALSE);
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
            if (!cave_flag_bold(ty, tx, flg)) return (FALSE);

            qx += m;

            if (qx < f2)
            {
                ty += sy;
            }
            else if (qx > f2)
            {
                tx += sx;
                if (!cave_flag_bold(ty, tx, flg)) return (FALSE);
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
 * Returns true if the player's grid is dark
 */
bool no_light(void)
{
    return (!player_can_see_bold(p_ptr->py, p_ptr->px));
}


/*
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool cave_valid_bold(int y, int x)
{
    object_type *o_ptr;

    /* Forbid perma-grids */
    if (cave_perma_bold(y, x)) return (FALSE);

    /* Check objects */
    for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
    {
        /* Forbid artifact grids */
        if (o_ptr->is_artifact()) return (FALSE);
    }

    /* Accept */
    return (TRUE);
}


/*
 * Table of breath colors.  Must match listings in a single set of
 * monster spell flags.
 *
 * The value "255" is special.  Monsters with that kind of breath
 * may be any color.
 */
static byte breath_to_attr[32][2] =
{
    {  0,  0 },
    {  0,  0 },
    {  0,  0 },
    {  0,  0 },
    {  0,  0 },
    {  0,  0 },
    {  0,  0 },
    {  0,  0 },
    {  TERM_SLATE, TERM_L_DARK },       /* RF4_BRTH_ACID */
    {  TERM_BLUE,  TERM_L_BLUE },       /* RF4_BRTH_ELEC */
    {  TERM_RED,  TERM_L_RED },         /* RF4_BRTH_FIRE */
    {  TERM_WHITE,  TERM_IVORY },     /* RF4_BRTH_COLD */
    {  TERM_GREEN,  TERM_L_GREEN },     /* RF4_BRTH_POIS */
    {  TERM_ORANGE,  TERM_RED },        /* RF4_BRTH_PLAS */
    {  TERM_YELLOW,  TERM_ORANGE },     /* RF4_BRTH_LIGHT */
    {  TERM_L_DARK,  TERM_SLATE },      /* RF4_BRTH_DARK */
    {  TERM_BRONZE,  TERM_UMBER },     /* RF4_BRTH_CONFU */
    {  TERM_YELLOW,  TERM_L_UMBER },    /* RF4_BRTH_SOUND */
    {  TERM_UMBER,  TERM_L_UMBER },     /* RF4_BRTH_SHARD */
    {  TERM_SNOW_WHITE,  TERM_SLATE },  /* RF4_BRTH_INER */
    {  TERM_SNOW_WHITE,  TERM_SLATE },  /* RF4_BRTH_GRAV */
    {  TERM_WHITE,  TERM_L_BLUE },      /* RF4_BRTH_WIND */
    {  TERM_UMBER,  TERM_L_UMBER },     /* RF4_BRTH_FORCE */
    {  TERM_L_RED,  TERM_VIOLET },      /* RF4_BRTH_NEXUS */
    {  TERM_L_GREEN,  TERM_GREEN },     /* RF4_BRTH_NETHR */
    {  255,  255 },   /* (any color) */ /* RF4_BRTH_CHAOS */
    {  TERM_VIOLET,  TERM_VIOLET },     /* RF4_BRTH_DISEN */
    {  TERM_VIOLET,  TERM_BLUE },       /* RF4_BRTH_MANA */
    {  0,  0 },     /*  */
    {  0,  0 },     /*  */
    {  0,  0 }      /*  */
};


// start with a color, return a similar color for shimmering effects.
// This should use Rand_simple so it doesn't affect the main random thread.
static byte similar_color(byte color)
{
    switch (color)
    {
        case TERM_RED:
        case TERM_L_RED:
        case TERM_RED_LAVA:
        case TERM_RED_RUST:
        case TERM_RASPBERRY:
        case TERM_PINK:
        {
            int x = Rand_simple(6);
            if (x == 1) return TERM_RED;
            if (x == 2) return TERM_L_RED;
            if (x == 3) return TERM_RED_LAVA;
            if (x == 4)	return TERM_RASPBERRY;
            if (x == 5)	return TERM_PINK;
            return TERM_RED_RUST;
        }
        case TERM_BLUE:
        case TERM_L_BLUE:
        case TERM_NAVY_BLUE:
        case TERM_SKY_BLUE:
        {
            int x = Rand_simple(4);
            if (x == 1) return TERM_BLUE;
            if (x == 2) return TERM_L_BLUE;
            if (x == 3) return TERM_NAVY_BLUE;
            return TERM_SKY_BLUE;
        }

        case TERM_GREEN:
        case TERM_L_GREEN:
        case TERM_JUNGLE_GREEN:
        case TERM_LIME_GREEN:
        {
            int x = Rand_simple(4);
            if (x == 1) return TERM_GREEN;
            if (x == 2) return TERM_L_GREEN;
            if (x == 3) return TERM_JUNGLE_GREEN;
            return TERM_LIME_GREEN;
        }
        case TERM_UMBER:
        case TERM_L_UMBER:
        case TERM_AUBURN:
        case TERM_L_BROWN:
        {
            int x = Rand_simple(4);
            if (x == 1) return TERM_UMBER;
            if (x == 2) return TERM_L_UMBER;
            if (x == 3) return TERM_AUBURN;
            return TERM_L_BROWN;
        }
        case TERM_ORANGE:
        case TERM_ORANGE_PEEL:
        case TERM_MAHAGONY:
        {
            int x = Rand_simple(3);
            if (x == 1) return TERM_ORANGE;
            if (x == 2) return TERM_ORANGE_PEEL;
            return TERM_MAHAGONY;
        }
        case TERM_YELLOW:
        case TERM_MAIZE:
        case TERM_EARTH_YELLOW:
        {
            int x = Rand_simple(3);
            if (x == 1) return TERM_YELLOW;
            if (x == 2) return TERM_MAIZE;
            return TERM_EARTH_YELLOW;
        }
        case TERM_SLATE:
        case TERM_L_DARK:
        case TERM_SLATE_GRAY:
        case TERM_TAUPE:
        {
            int x = Rand_simple(4);
            if (x == 1) return TERM_SLATE;
            if (x == 2) return TERM_L_DARK;
            if (x == 3) return TERM_SLATE_GRAY;
            return TERM_TAUPE;
        }
        case TERM_VIOLET:
        case TERM_PURPLE:
        {
            int x = Rand_simple(2);
            if (x == 1) return TERM_VIOLET;
            return TERM_PURPLE;
        }
        case TERM_GOLD:
        case TERM_SILVER:
        case TERM_COPPER:
        {
            int x = Rand_simple(3);
            if (x == 1) return TERM_GOLD;
            if (x == 2) return TERM_SILVER;
            return TERM_COPPER;
        }
        case TERM_WHITE:
        case TERM_SNOW_WHITE:
        case TERM_LIGHT_GRAY:
        default:
        {
            int x = Rand_simple(3);
            if (x == 1) return TERM_WHITE;
            if (x == 2) return TERM_SNOW_WHITE;
            return TERM_LIGHT_GRAY;
        }
    }
};

/*
 * Multi-hued monsters shimmer according to their default attr or to their
 * breaths.  -LM-
 *
 * If a monster has an attr other than 'v', it uses both colors associated
 * with that attr.
 * If a monster has only one kind of breath, it uses both colors
 * associated with that breath.  Otherwise, it just uses the first
 * color for any of its breaths.
 *
 * If a monster does not breath anything, it can be any color.
 * This function needs to use Rand_simple to preserve
 * the consistency of the RNG seed used in the game.
 *
 */
byte multi_hued_color(monster_race *r_ptr)
{
    QVector<byte> allowed_attrs;

    int i, j;

    int breaths = 0;
    int first_color = 0;
    int second_color = 0;
    allowed_attrs.clear();


    /* Monsters with an attr other than 'v' choose colors according to attr */
    if ((r_ptr->color_num != TERM_VIOLET) && (r_ptr->color_num != TERM_PURPLE))
    {
        return (similar_color(r_ptr->color_num));
    }

    /* Monsters with no ranged attacks can be any color */
    if (!r_ptr->freq_ranged) return (Rand_simple(MAX_COLORS));

    /* Check breaths */
    for (i = 0; i < 32; i++)
    {
        bool stored = FALSE;

        /* Don't have that breath */
        if (!(r_ptr->flags4 & (1L << i))) continue;

        /* Get the first color of this breath */
        first_color = breath_to_attr[i][0];

        /* Breath has no color associated with it */
        if (first_color == 0) continue;

        /* Monster can be of any color */
        if (first_color == 255) return (Rand_simple(MAX_COLORS));

        /* Increment the number of breaths */
        breaths++;

        /* Monsters with lots of breaths may be any color. */
        if (breaths == 6) return (Rand_simple(MAX_COLORS));

        /* Always store the first color */
        for (j = 0; j < allowed_attrs.size(); j++)
        {
            /* Already stored */
            if (allowed_attrs[j] == first_color) stored = TRUE;
        }
        if (!stored)
        {
            allowed_attrs.append(first_color);
        }

        /*
         * Remember (but do not immediately store) the second color
         * of the first breath.
         */
        if (breaths == 1)
        {
            second_color = breath_to_attr[i][1];
        }
    }

    /* Monsters with no breaths may be of any color. */
    if (breaths == 0) return (Rand_simple(MAX_COLORS));

    /* If monster has one breath, store the second color too. */
    if (breaths == 1)
    {
        allowed_attrs.append(second_color);
    }

    /* Pick a color at random */
    return (allowed_attrs[Rand_simple(allowed_attrs.size())]);
}


/*
 * Hack -- Hallucinatory monster
 */
static u16b image_monster(void)
{
    monster_race *r_ptr;

    while (1)
    {
        int i = Rand_simple(z_info->r_max);

        /* Select a random monster */
        r_ptr = &r_info[i];

        /* Skip non-entries */
        if (!r_ptr->r_speed) continue;

        /* Encode */
        return (i);
    }

    /*just to avoid compiler warnings*/
    return (TRUE);
}


/*
 * Hack -- Hallucinatory object
 */
static u16b image_object(void)
{
    object_kind *k_ptr;

    while (1)
    {

        int i = Rand_simple(z_info->k_max - 1) + 1;
        /* Select a random object */
        k_ptr = &k_info[i];

        /* Skip non-entries */
        if (k_ptr->k_name.isEmpty()) continue;

        /* Encode */
        return (i);
    }

    /*just to avoid compiler warnings*/
    return (TRUE);
}


static void special_lighting_floor(dungeon_type *dun_ptr)
{
    /* Handle "seen" grids */
    if (dun_ptr->cave_info & (CAVE_SEEN))
    {
        /* Only lit by "torch" lite */
        if (view_yellow_light && !(dun_ptr->cave_info & (CAVE_GLOW | CAVE_HALO)))
        {
            dun_ptr->ui_flags |= UI_LIGHT_TORCH;
            object_type *o_ptr = &inventory[INVEN_LIGHT];
            // Hack - Special light effect for the phial
            if ((o_ptr->k_idx > 0) && (k_info[o_ptr->k_idx].k_name.contains("Phial~"))) {
                dun_ptr->ui_flags |= UI_COSMIC_TORCH;
            }

            if (IS_GRAY(dun_ptr->dun_color)) {
                dun_ptr->dun_color = defined_colors[TERM_YELLOW];
            }
        }                
        return;
    }

    /* Handle "dark" grids and "blindness" */
    else if (p_ptr->timed[TMD_BLIND] || !(dun_ptr->cave_info & (CAVE_GLOW | CAVE_HALO)))
    {             
        dun_ptr->ui_flags |= UI_LIGHT_DIM;

        dun_ptr->dun_color = defined_colors[TERM_L_DARK];

        return;
    }

    else if (view_bright_light) {
        dun_ptr->ui_flags |= UI_LIGHT_BRIGHT;

        dun_ptr->dun_color = dun_ptr->dun_color.darker(150);

        return;
    }
}


static void special_lighting_wall(dungeon_type *dun_ptr)
{
    /* Handle "seen" grids */
    if (dun_ptr->cave_info & (CAVE_SEEN))
    {        
        return;
    }

    /* Handle "blind" */
    else if (p_ptr->timed[TMD_BLIND])
    {
        dun_ptr->ui_flags |= UI_LIGHT_DIM;

        dun_ptr->dun_color = defined_colors[TERM_L_DARK];

        return;
    }

    else if (view_bright_light) {
        dun_ptr->ui_flags |= UI_LIGHT_BRIGHT;

        dun_ptr->dun_color = dun_ptr->dun_color.darker(150);

        return;
    }
}


/*
 * Returns the given color or a complementary color (shimmering effects)
 */
static byte shimmer_color(byte color)
{
    /* Use the given color 50% of the time */
    if (one_in_(2))
    {
        return (similar_color(color));
    }

    /* Done */
    return (color);
}


/*
 * Checks if a square is at the (inner) edge of a trap detect area
 */
bool dtrap_edge(int y, int x)
{
    /* Check if the square is a dtrap in the first place */
    if (!(dungeon_info[y][x].cave_info & (CAVE_DTRAP))) return FALSE;

    /* Check for non-dtrap adjacent grids */
    if (in_bounds_fully(y + 1, x    ) && (!(dungeon_info[y + 1][    x].cave_info & (CAVE_DTRAP)))) return TRUE;
    if (in_bounds_fully(y    , x + 1) && (!(dungeon_info[y    ][x + 1].cave_info & (CAVE_DTRAP)))) return TRUE;
    if (in_bounds_fully(y - 1, x    ) && (!(dungeon_info[y - 1][    x].cave_info & (CAVE_DTRAP)))) return TRUE;
    if (in_bounds_fully(y    , x - 1) && (!(dungeon_info[y    ][x - 1].cave_info & (CAVE_DTRAP)))) return TRUE;

    return FALSE;
}

static void map_terrain(s16b y, s16b x)
{
    dungeon_type *dun_ptr = &dungeon_info[y][x];
    s16b feat = dun_ptr->feature_idx;
    bool det_trap_edge = dtrap_edge(y, x);
    u16b info = dun_ptr->cave_info;
    feature_type *f_ptr;
    bool do_dtrap = FALSE;
    dun_ptr->dtrap = FALSE;

    //Assume som things normal;
    dun_ptr->special_lighting = FLOOR_LIGHT_NORMAL;   

    /* Boring grids (floors, etc) */
    if (!(f_info[feat].f_flags1 & (FF1_REMEMBER)))
    {
        /* Trap detection edge */
        do_dtrap = TRUE;

        /* Memorized (or seen) floor */
        if ((info & (CAVE_MARK)) || (info & (CAVE_SEEN)))
        {
            /* Apply "mimic" field */
            feat = f_info[feat].f_mimic;

            /* Get the feature */
            f_ptr = &f_info[feat];

            /* We have seen the feature */
            f_ptr->f_everseen = TRUE;

            /* Get attribute and color */
            dun_ptr->dun_char = f_ptr->d_char;
            dun_ptr->dun_color = f_ptr->d_color;
            dun_ptr->dun_tile = f_ptr->tile_id;

            /* Special lighting effects */
            if (view_special_light)
            {
                special_lighting_floor(dun_ptr);
            }
        }

        /* Unknown */
        else
        {
            /* Get the darkness feature color and character */
            f_ptr = &f_info[FEAT_NONE];
            dun_ptr->dun_color = f_ptr->d_color;
            dun_ptr->dun_char = f_ptr->d_char;
            dun_ptr->dun_tile = f_ptr->tile_id;
        }
    }

    /* Interesting grids (non-floors) */
    else
    {
        /* Memorized grids */
        if (info & (CAVE_MARK))
        {
            /* Apply "mimic" field */
            feat = f_info[feat].f_mimic;

            /* Get the darkness feature color and character */
            f_ptr = &f_info[feat];
            dun_ptr->dun_color = f_ptr->d_color;
            dun_ptr->dun_char = f_ptr->d_char;
            dun_ptr->dun_tile = f_ptr->tile_id;

            /* We have seen the feature */
            f_ptr->f_everseen = TRUE;

            /* Special lighting effects (walls only) */
            if (view_granite_light)
            {
                special_lighting_wall(dun_ptr);
            }
        }

        /* Unknown */
        else
        {
            /* Trap detection edge */
            do_dtrap = TRUE;

            /* Get the darkness feature */
            f_ptr = &f_info[FEAT_NONE];
            dun_ptr->dun_color = f_ptr->d_color;
            dun_ptr->dun_char = f_ptr->d_char;            
            dun_ptr->dun_tile = f_ptr->tile_id;            
        }
    }

    /* Now add the dtrap edge characteristic as an overlay*/
    if ((do_dtrap) && (det_trap_edge))
    {
        dun_ptr->dtrap = TRUE;
        QString single_char = QString("●");
        dun_ptr->dun_color = defined_colors[TERM_GREEN];
        dun_ptr->dun_char = single_char[0];
    }
}

static void map_objects (s16b y, s16b x)
{
    object_type *o_ptr;
    dungeon_type *dun_ptr = &dungeon_info[y][x];
    bool sq_flag = FALSE;
    bool do_purple_dot = TRUE;
    int floor_num = 0;

    // Start with a clean slate
    dun_ptr->object_char = ' ';
    dun_ptr->object_color = add_preset_color(TERM_DARK);
    dun_ptr->obj_special_symbol = OBJ_SYMBOL_NONE;
    dun_ptr->object_tile.clear();

    //nothing there
    if (!dun_ptr->has_object()) return;

    /* Objects */
    for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
    {
        /* Memorized objects */
        if (!o_ptr->marked) continue;

        /* Hack -- object hallucination */
        if (p_ptr->timed[TMD_IMAGE])
        {
            int i = image_object();

            dun_ptr->object_color = k_info[i].d_color;
            dun_ptr->object_char =  k_info[i].d_char;
            dun_ptr->object_tile = k_info[i].get_tile_id();

            return;
        }

        /*autosquelch insert*/
        sq_flag = ((mark_squelch_items) &&
            (k_info[o_ptr->k_idx].squelch == SQUELCH_ALWAYS) && (k_info[o_ptr->k_idx].aware));

        /*hack - never allow quest items to appear as dot*/
        if ((!sq_flag) || o_ptr->is_quest_object())
        {
            dun_ptr->object_color = o_ptr->get_color();
            dun_ptr->object_char = o_ptr->get_char();
            dun_ptr->object_tile = o_ptr->get_tile_id();

            /*found a non-squelchable item, unless showing piles, display this one*/
            if (!show_piles) break;

            /*if only one item in a pile is not squelchable, show that one*/
            do_purple_dot = FALSE;
        }

        if (do_purple_dot)
        {
            QString single_char = QString("●");
            dun_ptr->object_color = defined_colors[TERM_VIOLET];
            dun_ptr->object_char = single_char[0];
            dun_ptr->object_tile.clear();
        }

        /* Special stack symbol, unless everything in the pile is squelchable */
        else if (++floor_num > 1)
        {
            if (use_graphics && current_tiles->has_tile("obj_pile"))
            {
                dun_ptr->object_tile = "obj_pile";
            }
            else
            {
                dun_ptr->object_tile.clear();
            }
            dun_ptr->object_char = k_info[0].d_char;
            dun_ptr->object_color = k_info[0].d_color;

            // We are done
            return;
        }
    }
}

static void map_effects(s16b y, s16b x)
{
    feature_type *f_ptr;
    effect_type *x_ptr = NULL;
    dungeon_type *dun_ptr = &dungeon_info[y][x];

    dun_ptr->effect_char = ' ';
    dun_ptr->effect_color = add_preset_color(TERM_DARK);
    dun_ptr->effect_tile.clear();

    if (!dun_ptr->has_effect()) return;
    if (!(dun_ptr->cave_info & (CAVE_SEEN | CAVE_MARK))) return;

    bool has_object = dun_ptr->has_visible_object();

    /* Handle effects */

    /* Visual ordering of the effects, biggest number first */
    static byte priority_table[] =
    {
        0,	/* nothing */
        30,	/* EFFECT_LINGERING_CLOUD */
        40,	/* EFFECT_SHIMMERING_CLOUD */
        10,	/* EFFECT_PERMANENT_CLOUD */
        20,	/* EFFECT_TRAP_SMART  */
        20,	/* EFFECT_TRAP_DUMB  */
        20,	/* EFFECT_TRAP_PLAYER  */
        20,	/* EFFECT_GLYPH */
        50,	/* EFFECT_GLACIER */
        9,	/* EFFECT_INSCRIPTION */
        10,	/* EFFECT_ROCKS */
    };

    int max_priority = -1;

    /* Get the first effect */
    u16b x_idx = dungeon_info[y][x].effect_idx;

    /* Scan the effects on that grid */
    while (x_idx)
    {
        u16b feat;

        /* Default priority */
        byte priority = 0;

        /* Get the effect data */
        effect_type *tmp_x_ptr = &x_list[x_idx];

        /* Point to the next effect */
        x_idx = tmp_x_ptr->next_x_idx;

        /* Ignore hidden effects */
        if (!(tmp_x_ptr->x_f_idx) || (tmp_x_ptr->x_flags & (EF1_HIDDEN))) continue;

        /* Get the mimic field */
        feat = f_info[tmp_x_ptr->x_f_idx].f_mimic;

        /* We have seen this effect */
        f_info[feat].f_everseen = TRUE;

        /* Hack - Permanent clouds and inscriptions shouldn't override objects */
        if (has_object && ((tmp_x_ptr->x_type == EFFECT_PERMANENT_CLOUD) ||
            (tmp_x_ptr->x_type == EFFECT_INSCRIPTION) ||
                (tmp_x_ptr->x_type == EFFECT_ROCKS))) continue;

        /* Get the priority of the effect, if possible */
        if (tmp_x_ptr->x_type < N_ELEMENTS(priority_table))
        {
            priority = priority_table[tmp_x_ptr->x_type];
        }

        /* We have a candidate */
        if (priority > max_priority)
        {
            /* Remember the effect */
            x_ptr = tmp_x_ptr;

            /* Remember priority */
            max_priority = priority;
        }
    }

    /* Do we have a visible effect? */
    if (x_ptr)
    {
        u16b feat;

        /* Apply "mimic" field to the feature effect. */
        feat = f_info[x_ptr->x_f_idx].f_mimic;

        /* Get the feature */
        f_ptr = &f_info[feat];

        /* We have seen the feature */
        f_ptr->f_everseen = TRUE;

        /* No special effects in graphics mode */
        if (use_graphics)
        {
            dun_ptr->effect_color = f_ptr->d_color;
            dun_ptr->effect_tile = f_ptr->tile_id;
            if (x_ptr->x_type == EFFECT_PERMANENT_CLOUD || x_ptr->x_type == EFFECT_LINGERING_CLOUD
                    || x_ptr->x_type == EFFECT_SHIMMERING_CLOUD)
            {
                dun_ptr->ui_flags |= UI_TRANSPARENT_EFFECT;
            }
        }
        /* Permanent clouds */
        else if (x_ptr->x_type == EFFECT_PERMANENT_CLOUD)
        {
            /* Get the feature under the cloud */
            u16b feat2 = dungeon_info[y][x].feature_idx;

            /* Not boring floor? */
            if ((feat2 != FEAT_FLOOR) && (feat2 != FEAT_COBBLESTONE_FLOOR))
            {
                /* Get mimiced feature */
                feat2 = f_info[feat2].f_mimic;

                /* Use the color of that feature */
                dun_ptr->effect_color = add_preset_color(shimmer_color(f_info[feat2].color_num));
            }
            /* Harmless terrain */
            else
            {
                /* Use the default color of the cloud */
                dun_ptr->effect_color = add_preset_color(gf_color(f_ptr->x_gf_type));
            }
        }
        /* Lingering clouds */
        else if (x_ptr->x_type == EFFECT_LINGERING_CLOUD)
        {
            dun_ptr->effect_color = add_preset_color(gf_color(f_ptr->x_gf_type));
        }
        /* Smart traps */
        else if (x_ptr->x_type == EFFECT_TRAP_SMART)
        {
            dun_ptr->effect_color = add_preset_color(shimmer_color(f_ptr->color_num));
        }
        /* Glacier */
        else if (x_ptr->x_type == EFFECT_GLACIER)
        {
            dun_ptr->effect_color = add_preset_color(shimmer_color(f_ptr->color_num));
        }
        /* Traps, glyphs, etc. */
        else
        {
            dun_ptr->effect_color = f_ptr->d_color;
        }

        /* Normal char */
        dun_ptr->effect_char = f_ptr->d_char;
    }
}

/*
 * Map all dungeon information for a given square.
 *
 */
static void map_monster (s16b y, s16b x)
{
    dungeon_type *dun_ptr = &dungeon_info[y][x];
    dun_ptr->monster_char = ' ';
    dun_ptr->monster_color = add_preset_color(TERM_DARK);
    dun_ptr->monster_tile.clear();

    dun_ptr->double_height_monster = FALSE;

    // Nothing there
    if (!dun_ptr->has_monster()) return;

    /* Monsters */
    if (dun_ptr->monster_idx > 0)
    {
        monster_type *m_ptr = &mon_list[dun_ptr->monster_idx];

        /* Visible monster*/        
        if (m_ptr->ml)
        {
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Desired attr */

            dun_ptr->monster_color = r_ptr->d_color;
            dun_ptr->monster_char = r_ptr->d_char;
            dun_ptr->monster_tile = r_ptr->tile_id;
            if (r_ptr->flags2 & RF2_PASS_WALL)
            {
                // In this tileset the monsters are alredy drawn transparent
                if (use_graphics != GRAPHICS_RAYMOND_GAUSTADNES) dun_ptr->ui_flags |= UI_TRANSPARENT_MONSTER;
            }

            if (r_ptr->double_height_tile) dun_ptr->double_height_monster = TRUE;

            /* Hack -- monster hallucination */
            if (p_ptr->timed[TMD_IMAGE])
            {
                int i = image_monster();

                dun_ptr->monster_color = r_info[i].d_color;
                dun_ptr->monster_char =   r_info[i].d_char;
                dun_ptr->monster_tile = r_info[i].tile_id;

                return;

            }

            /* Multi-hued monster */
            else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
            {
                /* Multi-hued attr */
                dun_ptr->monster_color = add_preset_color(multi_hued_color(r_ptr));
            }

            /* Normal attr, Clear char, monster */
            else if (r_ptr->flags1 & (RF1_CHAR_CLEAR))
            {
                if (dun_ptr->effect_char != ' ') dun_ptr->monster_char = dun_ptr->effect_char;
                else if (dun_ptr->object_char != ' ') dun_ptr->monster_char = dun_ptr->object_char;
                else if (dun_ptr->dun_char != ' ') dun_ptr->monster_char = dun_ptr->dun_char;
            }

            /* Normal attr, Clear char, monster */
            else if (r_ptr->flags1 & (RF1_ATTR_CLEAR))
            {
                if (dun_ptr->effect_char != ' ') dun_ptr->monster_color = dun_ptr->effect_color;
                else if (dun_ptr->object_char != ' ') dun_ptr->monster_color = dun_ptr->object_color;
                else if (dun_ptr->dun_char != ' ') dun_ptr->monster_color = dun_ptr->dun_color;
            }
        }
    }

    /* Handle "player" */
    else if (dun_ptr->monster_idx < 0)
    {
        monster_race *r_ptr = &r_info[0];

        /* Get the "player" attr */
        /*  DSV:  I've chosen the following sequence of colors to indicate
                the player's current HP.  There are colors are left over, but I
                left them in this comment for easy reference, in the likely case
                that I decide to change the order of color changes.

            TERM_WHITE		90-100% of HP remaining
            TERM_YELLOW		70- 89% of HP remaining
            TERM_ORANGE		50- 69% of HP remaining
            TERM_L_RED		30- 49% of HP remaining
            TERM_RED		 0- 29% of HP remaining

        */

        if (hp_changes_color)
        {
            byte a;
            /* check for hunger first */
            a = TERM_WHITE;
            if (p_ptr->food < PY_FOOD_ALERT)
            {
                a = TERM_L_UMBER;
            }

            /* overwrite with health check */
            switch(p_ptr->chp * 10 / p_ptr->mhp)
            {
                case  8:
                case  7:	a = TERM_YELLOW ;	break;
                case  6:
                case  5:	a = TERM_ORANGE ;	break;
                case  4:
                case  3:	a = TERM_L_RED  ;	break;
                case  2:
                case  1:
                case  0:	a = TERM_RED    ;	break;
            }

            dun_ptr->monster_color = add_preset_color(a);
        }

        else
        {
            dun_ptr->monster_color = r_ptr->d_color;
        }

        dun_ptr->monster_tile = p_ptr->tile_id;

        /* Get the "player" char */
        dun_ptr->monster_char = r_ptr->d_char;
    }
}

static void set_priority(int y, int x)
{
    dungeon_type *dun_ptr = &dungeon_info[y][x];

    feature_type *f_ptr = &f_info[dun_ptr->feature_idx];

    byte priority = 0;

    if (dun_ptr->has_visible_terrain()) priority = f_ptr->f_priority;

    if (dun_ptr->has_visible_effect())
    {
        feature_type *f2_ptr = &f_info[dun_ptr->effect_idx];
        priority += f2_ptr->f_priority;
    }
    if (dun_ptr->has_visible_object()) priority += 10;
    if (dun_ptr->has_visible_monster())
    {
        // Player square has the hightest priority
        if ((y == p_ptr->py) && (x == p_ptr->px))
        {
            priority = MAX_BYTE;
        }
        else
        {
            monster_type *m_ptr = &mon_list[dun_ptr->monster_idx];
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            priority += MAX(10,(r_ptr->level - (2 * p_ptr->lev / 3)));
        }
    }

    dun_ptr->priority = priority;
}



void map_info(s16b y, s16b x)
{
    dungeon_info[y][x].ui_flags = 0;

    // Map each layer of the dungeon
    map_terrain (y, x);

    map_objects (y, x);

    map_effects (y, x);

    map_monster (y, x);

    set_priority(y, x);
}


/*
 * Memorize interesting viewable object/features in the given grid
 *
 * This function should only be called on "legal" grids.
 *
 * This function will memorize the object and/or feature in the given grid,
 * if they are (1) see-able and (2) interesting.  Note that all objects are
 * interesting, all terrain features except floors (and invisible traps) are
 * interesting, and floors (and invisible traps) are interesting sometimes
 * (depending on various options involving the illumination of floor grids).
 *
 * The automatic memorization of all objects and non-floor terrain features
 * as soon as they are displayed allows incredible amounts of optimization
 * in various places, especially "map_info()" and this function itself.
 *
 * Note that the memorization of objects is completely separate from the
 * memorization of terrain features, preventing annoying floor memorization
  * when a detected object is picked up from a dark floor, and object
 * memorization when an object is dropped into a floor grid which is
 * memorized but out-of-sight.
 *
 * This function should be called every time the "memorization" of a grid
 * (or the object in a grid) is called into question, such as when an object
 * is created in a grid, when a terrain feature "changes" from "floor" to
 * "non-floor", and when any grid becomes "see-able" for any reason.
 *
 * This function is called primarily from the "update_view()" function, for
 * each grid which becomes newly "see-able".
 */
void note_spot(int y, int x)
{
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    object_type *o_ptr;

    /* Get cave info */
    u16b info = dungeon_info[y][x].cave_info;

    /* Require "seen" flag */
    if (!(info & (CAVE_SEEN))) return;

    /* Hack -- memorize objects */
    for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
    {
        /* Memorize objects */
        o_ptr->mark_object();
    }

    /* Hack -- memorize grids */
    if (!(info & (CAVE_MARK)))
    {
        /* Memorize some "boring" grids */
        if (!_feat_ff1_match(f_ptr, FF1_REMEMBER))
        {
            /* Option -- memorize certain floors */
            if ((view_perma_grids &&
                (info & (CAVE_GLOW | CAVE_HALO))) ||
                (view_torch_grids))
            {
                /* Memorize */
                dungeon_info[y][x].mark_square();
            }

            /* Hack -- Emulate the REMEMBER flag for certain effects */
            else
            {
                /* Get the first effect on that grid */
                int x_idx = dungeon_info[y][x].effect_idx;

                /* Iterate through the effects */
                while (x_idx)
                {
                    /* Get the effect data */
                    effect_type *x_ptr = &x_list[x_idx];

                    /* Prepare the next effect */
                    x_idx = x_ptr->next_x_idx;

                    /* Ignore hidden effects */
                    if (x_ptr->x_flags & (EF1_HIDDEN)) continue;

                    /* We'll remember traps and glyphs for now */
                    if (!(x_ptr->x_flags & (EF1_TRAP_DUMB |
                            EF1_TRAP_SMART | EF1_TRAP_PLAYER |
                            EF1_GLYPH))) continue;

                    /* Remember the grid */
                    dungeon_info[y][x].mark_square();

                    /* Done */
                    break;
                }
            }

        }

        /* Memorize all "interesting" grids */
        else
        {
            /* Memorize */
            dungeon_info[y][x].mark_square();
        }
    }
}



/*
 * Redraw a dungeon square.
 * first update it, then print it(on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 *
 */
void light_spot(int y, int x)
{
    dungeon_type *d_ptr = &dungeon_info[y][x];
    bool redraw_above = d_ptr->double_height_monster;

    /* Hack -- redraw the grid */
    map_info(y, x);

    // Possibly draw the square above it
    if (redraw_above || d_ptr->double_height_monster)
    {
        if (in_bounds(y-1, x)) map_info(y-1, x);
    }

    // print the square onscreen
    ui_redraw_grid(y, x);

    // Possibly draw the square above it
    if (redraw_above || d_ptr->double_height_monster)
    {
        if (in_bounds(y-1, x)) redraw_coords.append(make_coords(y-1, x));
        p_ptr->redraw |= PR_DRAW;
    }
}

static bool coords_sort(coord first, coord second)
{
    if (first.y > second.y) return (TRUE);
    if (first.y < second.y)return (FALSE);

    // Y coords are equal
    if (first.x > second.x) return (TRUE);
    return (FALSE);
}

coord make_coords(int y, int x)
{
    coord this_coord;
    this_coord.y = y;
    this_coord.x = x;
    return (this_coord);
}

void draw_coords(void)
{
    if (!redraw_coords.size()) return;

    qSort(redraw_coords.begin(), redraw_coords.end(), coords_sort);

    int y = -1;
    int x = -1;

    for (int i = 0; i < redraw_coords.size(); i++)
    {
           coord *co_ptr = &redraw_coords[i];

           // Don't do duplicates
           if (y == co_ptr->y && x == co_ptr->x) continue;

           y = co_ptr->y;
           x = co_ptr->x;

           ui_redraw_grid(y, x);
    }

    redraw_coords.clear();

    p_ptr->redraw &= ~(PR_DRAW);
}


/*
 * Maximum number of grids in a single octant
 */
#define VINFO_MAX_GRIDS 161


/*
 * Maximum number of slopes in a single octant
 */
#define VINFO_MAX_SLOPES 126


/*
 * Mask of bits used in a single octant
 */
#define VINFO_BITS_3 0x3FFFFFFF
#define VINFO_BITS_2 0xFFFFFFFF
#define VINFO_BITS_1 0xFFFFFFFF
#define VINFO_BITS_0 0xFFFFFFFF


/*
 * Forward declare
 */
typedef struct vinfo_type vinfo_type;


/*
 * The 'vinfo_type' structure
 */
struct vinfo_type
{
    u16b grid[8];

    /* LOS slopes (up to 128) */
    u32b bits_3;
    u32b bits_2;
    u32b bits_1;
    u32b bits_0;

    /* Index of the first LOF slope */
    byte slope_fire_index1;

    /* Index of the (possible) second LOF slope */
    byte slope_fire_index2;

    vinfo_type *next_0;
    vinfo_type *next_1;

    byte y;
    byte x;
    byte d;
    byte r;
};


/*
 * The array of "vinfo" objects, initialized by "vinfo_init()"
 */
static vinfo_type vinfo[VINFO_MAX_GRIDS];


/*
 * Slope scale factor
 */
#define SCALE 100000L


/*
 * The actual slopes (for reference)
 */

/* Bit :     Slope   Grids */
/* --- :     -----   ----- */
/*   0 :      2439      21 */
/*   1 :      2564      21 */
/*   2 :      2702      21 */
/*   3 :      2857      21 */
/*   4 :      3030      21 */
/*   5 :      3225      21 */
/*   6 :      3448      21 */
/*   7 :      3703      21 */
/*   8 :      4000      21 */
/*   9 :      4347      21 */
/*  10 :      4761      21 */
/*  11 :      5263      21 */
/*  12 :      5882      21 */
/*  13 :      6666      21 */
/*  14 :      7317      22 */
/*  15 :      7692      20 */
/*  16 :      8108      21 */
/*  17 :      8571      21 */
/*  18 :      9090      20 */
/*  19 :      9677      21 */
/*  20 :     10344      21 */
/*  21 :     11111      20 */
/*  22 :     12000      21 */
/*  23 :     12820      22 */
/*  24 :     13043      22 */
/*  25 :     13513      22 */
/*  26 :     14285      20 */
/*  27 :     15151      22 */
/*  28 :     15789      22 */
/*  29 :     16129      22 */
/*  30 :     17241      22 */
/*  31 :     17647      22 */
/*  32 :     17948      23 */
/*  33 :     18518      22 */
/*  34 :     18918      22 */
/*  35 :     20000      19 */
/*  36 :     21212      22 */
/*  37 :     21739      22 */
/*  38 :     22580      22 */
/*  39 :     23076      22 */
/*  40 :     23809      22 */
/*  41 :     24137      22 */
/*  42 :     24324      23 */
/*  43 :     25714      23 */
/*  44 :     25925      23 */
/*  45 :     26315      23 */
/*  46 :     27272      22 */
/*  47 :     28000      23 */
/*  48 :     29032      23 */
/*  49 :     29411      23 */
/*  50 :     29729      24 */
/*  51 :     30434      23 */
/*  52 :     31034      23 */
/*  53 :     31428      23 */
/*  54 :     33333      18 */
/*  55 :     35483      23 */
/*  56 :     36000      23 */
/*  57 :     36842      23 */
/*  58 :     37142      24 */
/*  59 :     37931      24 */
/*  60 :     38461      24 */
/*  61 :     39130      24 */
/*  62 :     39393      24 */
/*  63 :     40740      24 */
/*  64 :     41176      24 */
/*  65 :     41935      24 */
/*  66 :     42857      23 */
/*  67 :     44000      24 */
/*  68 :     44827      24 */
/*  69 :     45454      23 */
/*  70 :     46666      24 */
/*  71 :     47368      24 */
/*  72 :     47826      24 */
/*  73 :     48148      24 */
/*  74 :     48387      24 */
/*  75 :     51515      25 */
/*  76 :     51724      25 */
/*  77 :     52000      25 */
/*  78 :     52380      25 */
/*  79 :     52941      25 */
/*  80 :     53846      25 */
/*  81 :     54838      25 */
/*  82 :     55555      24 */
/*  83 :     56521      25 */
/*  84 :     57575      26 */
/*  85 :     57894      25 */
/*  86 :     58620      25 */
/*  87 :     60000      23 */
/*  88 :     61290      25 */
/*  89 :     61904      25 */
/*  90 :     62962      25 */
/*  91 :     63636      25 */
/*  92 :     64705      25 */
/*  93 :     65217      25 */
/*  94 :     65517      25 */
/*  95 :     67741      26 */
/*  96 :     68000      26 */
/*  97 :     68421      26 */
/*  98 :     69230      26 */
/*  99 :     70370      26 */
/* 100 :     71428      25 */
/* 101 :     72413      26 */
/* 102 :     73333      26 */
/* 103 :     73913      26 */
/* 104 :     74193      27 */
/* 105 :     76000      26 */
/* 106 :     76470      26 */
/* 107 :     77777      25 */
/* 108 :     78947      26 */
/* 109 :     79310      26 */
/* 110 :     80952      26 */
/* 111 :     81818      26 */
/* 112 :     82608      26 */
/* 113 :     84000      26 */
/* 114 :     84615      26 */
/* 115 :     85185      26 */
/* 116 :     86206      27 */
/* 117 :     86666      27 */
/* 118 :     88235      27 */
/* 119 :     89473      27 */
/* 120 :     90476      27 */
/* 121 :     91304      27 */
/* 122 :     92000      27 */
/* 123 :     92592      27 */
/* 124 :     93103      28 */
/* 125 :    100000      13 */


/*
 * Forward declare
 */
typedef struct vinfo_hack vinfo_hack;


/*
 * Temporary data used by "vinfo_init()"
 *
 *	- Number of line of sight slopes
 *
 *	- Slope values
 *
 *	- Slope range for each grid
 */
struct vinfo_hack
{
    int num_slopes;

    long slopes[VINFO_MAX_SLOPES];

    long slopes_min[MAX_SIGHT+1][MAX_SIGHT+1];
    long slopes_max[MAX_SIGHT+1][MAX_SIGHT+1];
};


/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
static bool ang_sort_comp_hook_longs(const void *u, const void *v, int a, int b)
{
    long *x = (long*)(u);

    /* Unused parameter */
    (void)v;

    return (x[a] <= x[b]);
}


/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
static void ang_sort_swap_hook_longs(void *u, void *v, int a, int b)
{
    long *x = (long*)(u);

    long temp;

    /* Unused parameter */
    (void)v;

    /* Swap */
    temp = x[a];
    x[a] = x[b];
    x[b] = temp;
}


/*
 * Save a slope
 */
static void vinfo_init_aux(vinfo_hack *hack, int y, int x, long m)
{
    int i;

    /* Handle "legal" slopes */
    if ((m > 0) && (m <= SCALE))
    {
        /* Look for that slope */
        for (i = 0; i < hack->num_slopes; i++)
        {
            if (hack->slopes[i] == m) break;
        }

        /* New slope */
        if (i == hack->num_slopes)
        {
            /* Paranoia */
            if (hack->num_slopes >= VINFO_MAX_SLOPES)
            {
                quit_npp_games(QString("Too many LOS slopes (%1)!") .arg(VINFO_MAX_SLOPES));
            }

            /* Save the slope, increment count */
            hack->slopes[hack->num_slopes++] = m;
        }
    }

    /* Track slope range */
    if (hack->slopes_min[y][x] > m) hack->slopes_min[y][x] = m;
    if (hack->slopes_max[y][x] < m) hack->slopes_max[y][x] = m;

}


/*
 * Initialize the "vinfo" array
 *
 * Full Octagon (radius 20), Grids=1149
 *
 * Quadrant (south east), Grids=308, Slopes=251
 *
 * Octant (east then south), Grids=161, Slopes=126
 *
 * This function assumes that VINFO_MAX_GRIDS and VINFO_MAX_SLOPES
 * have the correct values, which can be derived by setting them to
 * a number which is too high, running this function, and using the
 * error messages to obtain the correct values.
 */
int vinfo_init(void)
{
    int i;
    int y, x;
    u16b g;

    long m;

    vinfo_hack *hack;

    int num_grids = 0;

    int queue_head = 0;
    int queue_tail = 0;
    vinfo_type *queue[VINFO_MAX_GRIDS*2];

    /* Make hack */
    hack = ZNEW(vinfo_hack);

    /* Analyze grids */
    for (y = 0; y <= MAX_SIGHT; ++y)
    {
        for (x = y; x <= MAX_SIGHT; ++x)
        {
            /* Skip grids which are out of sight range */
            if (distance(0, 0, y, x) > MAX_SIGHT) continue;

            /* Default slope range */
            hack->slopes_min[y][x] = 999999999;
            hack->slopes_max[y][x] = 0;

            /* Paranoia */
            if (num_grids >= VINFO_MAX_GRIDS)
            {
                quit_npp_games(QString("Too many grids (%1 >= %2)!") .arg(num_grids) .arg(VINFO_MAX_GRIDS));
            }

            /* Count grids */
            num_grids++;

            /* Slope to the top right corner */
            m = SCALE * (1000L * y - 500) / (1000L * x + 500);

            /* Handle "legal" slopes */
            vinfo_init_aux(hack, y, x, m);

            /* Slope to top left corner */
            m = SCALE * (1000L * y - 500) / (1000L * x - 500);

            /* Handle "legal" slopes */
            vinfo_init_aux(hack, y, x, m);

            /* Slope to bottom right corner */
            m = SCALE * (1000L * y + 500) / (1000L * x + 500);

            /* Handle "legal" slopes */
            vinfo_init_aux(hack, y, x, m);

            /* Slope to bottom left corner */
            m = SCALE * (1000L * y + 500) / (1000L * x - 500);

            /* Handle "legal" slopes */
            vinfo_init_aux(hack, y, x, m);
        }
    }

    /* Enforce maximal efficiency (grids) */
    if (num_grids < VINFO_MAX_GRIDS)
    {
        quit_npp_games(QString("Too few grids (%1 < %2)!") .arg(num_grids) .arg(VINFO_MAX_GRIDS));
    }

    /* Enforce maximal efficiency (line of sight slopes) */
    if (hack->num_slopes < VINFO_MAX_SLOPES)
    {
        quit_npp_games(QString("Too few LOS slopes (%1 < %2)!") .arg(hack->num_slopes) .arg(VINFO_MAX_SLOPES));
    }

    /* Sort slopes numerically */
    ang_sort_comp = ang_sort_comp_hook_longs;

    /* Sort slopes numerically */
    ang_sort_swap = ang_sort_swap_hook_longs;

    /* Sort the (unique) LOS slopes */
    ang_sort(hack->slopes, NULL, hack->num_slopes);

    /* Enqueue player grid */
    queue[queue_tail++] = &vinfo[0];

    /* Process queue */
    while (queue_head < queue_tail)
    {
        int e;

        /* Index */
        e = queue_head++;

        /* Main Grid */
        g = vinfo[e].grid[0];

        /* Location */
        y = GRID_Y(g);
        x = GRID_X(g);

        /* Compute grid offsets */
        vinfo[e].grid[0] = GRID(+y,+x);
        vinfo[e].grid[1] = GRID(+x,+y);
        vinfo[e].grid[2] = GRID(+x,-y);
        vinfo[e].grid[3] = GRID(+y,-x);
        vinfo[e].grid[4] = GRID(-y,-x);
        vinfo[e].grid[5] = GRID(-x,-y);
        vinfo[e].grid[6] = GRID(-x,+y);
        vinfo[e].grid[7] = GRID(-y,+x);


        /* Skip player grid */
        if (e > 0)
        {
            long slope_fire;

            long tmp0 = 0;
            long tmp1 = 0;
            long tmp2 = 999999L;

            /* Determine LOF slope for this grid */
            if (x == 0) slope_fire = SCALE;
            else slope_fire = SCALE * (1000L * y) / (1000L * x);

            /* Analyze LOS slopes */
            for (i = 0; i < hack->num_slopes; ++i)
            {
                m = hack->slopes[i];

                /* Memorize intersecting slopes */
                if ((hack->slopes_min[y][x] < m) &&
                    (hack->slopes_max[y][x] > m))
                {
                    /* Add it to the LOS slope set */
                    switch (i / 32)
                    {
                        case 3: vinfo[e].bits_3 |= (1L << (i % 32)); break;
                        case 2: vinfo[e].bits_2 |= (1L << (i % 32)); break;
                        case 1: vinfo[e].bits_1 |= (1L << (i % 32)); break;
                        case 0: vinfo[e].bits_0 |= (1L << (i % 32)); break;
                    }

                    /* Check for exact match with the LOF slope */
                    if (m == slope_fire) tmp0 = i;

                    /* Remember index of nearest LOS slope < than LOF slope */
                    else if ((m < slope_fire) && (m > tmp1)) tmp1 = i;

                    /* Remember index of nearest LOS slope > than LOF slope */
                    else if ((m > slope_fire) && (m < tmp2)) tmp2 = i;
                }
            }

            /* There is a perfect match with one of the LOS slopes */
            if (tmp0)
            {
                /* Save the (unique) slope */
                vinfo[e].slope_fire_index1 = tmp0;
                vinfo[e].slope_fire_index2 = tmp0;
            }

            /* The LOF slope lies between two LOS slopes */
            else
            {
                /* Save the first slope */
                vinfo[e].slope_fire_index1 = tmp1;

                /* Save the second slope */
                vinfo[e].slope_fire_index2 = tmp2;
            }
        }

        /* Default */
        vinfo[e].next_0 = &vinfo[0];

        /* Grid next child */
        if (distance(0, 0, y, x+1) <= MAX_SIGHT)
        {
            g = GRID(y,x+1);

            if (queue[queue_tail-1]->grid[0] != g)
            {
                vinfo[queue_tail].grid[0] = g;
                queue[queue_tail] = &vinfo[queue_tail];
                queue_tail++;
            }

            vinfo[e].next_0 = &vinfo[queue_tail - 1];
        }


        /* Default */
        vinfo[e].next_1 = &vinfo[0];

        /* Grid diag child */
        if (distance(0, 0, y+1, x+1) <= MAX_SIGHT)
        {
            g = GRID(y+1,x+1);

            if (queue[queue_tail-1]->grid[0] != g)
            {
                vinfo[queue_tail].grid[0] = g;
                queue[queue_tail] = &vinfo[queue_tail];
                queue_tail++;
            }

            vinfo[e].next_1 = &vinfo[queue_tail - 1];
        }


        /* Hack -- main diagonal has special children */
        if (y == x) vinfo[e].next_0 = vinfo[e].next_1;


        /* Grid coordinates, approximate distance  */
        vinfo[e].y = y;
        vinfo[e].x = x;
        vinfo[e].d = ((y > x) ? (y + x/2) : (x + y/2));
        vinfo[e].r = ((!y) ? x : (!x) ? y : (y == x) ? y : 0);
    }

    /* Verify maximal bits XXX XXX XXX */
    if (((vinfo[1].bits_3 | vinfo[2].bits_3) != VINFO_BITS_3) ||
        ((vinfo[1].bits_2 | vinfo[2].bits_2) != VINFO_BITS_2) ||
        ((vinfo[1].bits_1 | vinfo[2].bits_1) != VINFO_BITS_1) ||
        ((vinfo[1].bits_0 | vinfo[2].bits_0) != VINFO_BITS_0))
    {
        quit_npp_games("Incorrect bit masks!");
    }


    /* Kill hack */
    FREE(hack);


    /* Success */
    return (0);
}


/*
 * Forget the "CAVE_VIEW" and "CAVE_FIRE" grids, redrawing as needed
 */
void forget_view(void)
{

    /* Clear them all */
    for (int i = 0; i < view_grids.size(); i++)
    {

        /* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
        dungeon_info[view_grids[i].y][view_grids[i].x].cave_info &= ~(CAVE_VIEW | CAVE_SEEN);

        /* Redraw */
        light_spot(view_grids[i].y, view_grids[i].x);
    }

    /* None left */
    view_grids.clear();

    /* Clear the CAVE_FIRE flag */
    for (int i = 0; i < fire_grids.size(); i++)
    {
        /* Clear */
        dungeon_info[fire_grids[i].y][fire_grids[i].x].cave_info &= ~(CAVE_FIRE);
    }

    /* None left */
    fire_grids.clear();
}


/*
 * Calculate the complete field of view using a new algorithm
 *
 * Note the following idiom, which is used in the function below.
 * This idiom processes each "octant" of the field of view, in a
 * clockwise manner, starting with the east strip, south side,
 * and for each octant, allows a simple calculation to set "g"
 * equal to the proper grids, relative to "pg", in the octant.
 *
 *   for (o2 = 0; o2 < 8; o2++)
 *   ...
 *         g = pg + p->grid[o2];
 *   ...
 *
 *
 * Normally, vision along the major axes is more likely than vision
 * along the diagonal axes, so we check the bits corresponding to
 * the lines of sight near the major axes first.
 *
 * We use the "temp_g" array (and the "CAVE_TEMP" flag) to keep track of
 * which grids were previously marked "CAVE_SEEN", since only those grids
 * whose "CAVE_SEEN" value changes during this routine must be redrawn.
 *
 * This function is now responsible for maintaining the "CAVE_SEEN"
 * flags as well as the "CAVE_VIEW" flags, which is good, because
 * the only grids which normally need to be memorized and/or redrawn
 * are the ones whose "CAVE_SEEN" flag changes during this routine.
 *
 * Basically, this function divides the "octagon of view" into octants of
 * grids (where grids on the main axes and diagonal axes are "shared" by
 * two octants), and processes each octant one at a time, processing each
 * octant one grid at a time, processing only those grids which "might" be
 * viewable, and setting the "CAVE_VIEW" flag for each grid for which there
 * is an (unobstructed) line of sight from the center of the player grid to
 * any internal point in the grid (and collecting these "CAVE_VIEW" grids
 * into the "view_g" array), and setting the "CAVE_SEEN" flag for the grid
 * if, in addition, the grid is "illuminated" in some way.
 *
 * This function relies on a theorem (suggested and proven by Mat Hostetter)
 * which states that in each octant of a field of view, a given grid will
 * be "intersected" by one or more unobstructed "lines of sight" from the
 * center of the player grid if and only if it is "intersected" by at least
 * one such unobstructed "line of sight" which passes directly through some
 * corner of some grid in the octant which is not shared by any other octant.
 * The proof is based on the fact that there are at least three significant
 * lines of sight involving any non-shared grid in any octant, one which
 * intersects the grid and passes though the corner of the grid closest to
 * the player, and two which "brush" the grid, passing through the "outer"
 * corners of the grid, and that any line of sight which intersects a grid
 * without passing through the corner of a grid in the octant can be "slid"
 * slowly towards the corner of the grid closest to the player, until it
 * either reaches it or until it brushes the corner of another grid which
 * is closer to the player, and in either case, the existanc of a suitable
 * line of sight is thus demonstrated.
 *
 * It turns out that in each octant of the radius 20 "octagon of view",
 * there are 161 grids (with 128 not shared by any other octant), and there
 * are exactly 126 distinct "lines of sight" passing from the center of the
 * player grid through any corner of any non-shared grid in the octant.  To
 * determine if a grid is "viewable" by the player, therefore, you need to
 * simply show that one of these 126 lines of sight intersects the grid but
 * does not intersect any wall grid closer to the player.  So we simply use
 * a bit vector with 126 bits to represent the set of interesting lines of
 * sight which have not yet been obstructed by wall grids, and then we scan
 * all the grids in the octant, moving outwards from the player grid.  For
 * each grid, if any of the lines of sight which intersect that grid have not
 * yet been obstructed, then the grid is viewable.  Furthermore, if the grid
 * is a wall grid, then all of the lines of sight which intersect the grid
 * should be marked as obstructed for future reference.  Also, we only need
 * to check those grids for whom at least one of the "parents" was a viewable
 * non-wall grid, where the parents include the two grids touching the grid
 * but closer to the player grid (one adjacent, and one diagonal).  For the
 * bit vector, we simply use 4 32-bit integers.  All of the static values
 * which are needed by this function are stored in the large "vinfo" array
 * (above), which is machine generated by another program.  XXX XXX XXX
 *
 * Hack -- The queue must be able to hold more than VINFO_MAX_GRIDS grids
 * because the grids at the edge of the field of view use "grid zero" as
 * their children, and the queue must be able to hold several of these
 * special grids.  Because the actual number of required grids is bizarre,
 * we simply allocate twice as many as we would normally need.  XXX XXX XXX
 */
void update_view(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    u16b pg = GRID(py,px);

    int i, j, o2;
    u16b g;

    int radius;

    /*used for monster lite patch*/
    int fy,fx,k;

    QVector<coord> prev_view_list;
    prev_view_list.clear();

    QVector<coord> prev_seen_list;
    prev_seen_list.clear();

    /*** Step 0 -- Begin ***/

    /* Save the old "view" grids for later */
    for (i = 0; i < view_grids.size(); i++)
    {
        prev_view_list.append(view_grids[i]);

        /* Save "CAVE_SEEN" grids */
        if (dungeon_info[view_grids[i].y][view_grids[i].x].cave_info & (CAVE_SEEN))
        {
            prev_seen_list.append(view_grids[i]);

            dungeon_info[view_grids[i].y][view_grids[i].x].cave_info |= (CAVE_TEMP);
        }

        /* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
        dungeon_info[view_grids[i].y][view_grids[i].x].cave_info &= ~(CAVE_VIEW | CAVE_SEEN);
    }

    /* Reset the "view" array */
    view_grids.clear();

    /* Clear the CAVE_FIRE flag */
    for (i = 0; i < fire_grids.size(); i++)
    {
        /* Clear */
        dungeon_info[fire_grids[i].y][fire_grids[i].x].cave_info&= ~(CAVE_FIRE);
    }
    fire_grids.clear();

    /* Extract "radius" value */
    radius = p_ptr->state.cur_light;

    /* Handle real light */
    if (radius > 0) ++radius;    

    /*** Step 2 -- Monster Lights  ***/

    /* Scan monster list and add monster lites */
    for ( k = 1; k < mon_max; k++)
    {
        /* Check the k'th monster */
        monster_type *m_ptr = &mon_list[k];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Access the location */
        fx = m_ptr->fx;
        fy = m_ptr->fy;

        /* Carrying lite */
        if (r_ptr->flags2 & (RF2_HAS_LIGHT))
        {
            for (i = -1; i <= 1; i++)
            {
                for (j = -1; j <= 1; j++)
                {
                    /*
                     * Compute distance, so you don't have an empty lite
                     * floating around past the max_site range.
                     */

                    int dy = (p_ptr->py > (fy+i)) ? (p_ptr->py - (fy+i)) : ((fy+i) - p_ptr->py);
                    int dx = (p_ptr->px > (fx+j)) ? (p_ptr->px - (fx+j)) : ((fx+j) - p_ptr->px);

                    /* Approximate distance */
                    int d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

                    if ((d <= MAX_SIGHT) && (los(p_ptr->py,p_ptr->px,fy+i,fx+j)))
                    {
                        dungeon_type *dun_ptr = &dungeon_info[fy+i][fx+j];

                        dun_ptr->cave_info |= (CAVE_SEEN | CAVE_VIEW | CAVE_EXPLORED);

                        view_grids.append(make_coords(fy+i, fx+j));
                    }
                }
            }
        }
    }


    /*** Step 2 -- player grid ***/

    /* Player grid */
    g = pg;

    /* Assume viewable */
    dungeon_info[p_ptr->py][p_ptr->px].cave_info |= (CAVE_VIEW | CAVE_FIRE);

    /* Torch-lit grid */
    if (0 < radius)
    {
        /* Mark as "CAVE_SEEN" */
        dungeon_info[p_ptr->py][p_ptr->px].cave_info |= (CAVE_SEEN | CAVE_EXPLORED);
    }

    /* Perma-lit grid */
    else if (dungeon_info[p_ptr->py][p_ptr->px].cave_info & (CAVE_GLOW | CAVE_HALO))
    {
        /* Mark as "CAVE_SEEN" */
        dungeon_info[p_ptr->py][p_ptr->px].cave_info |= (CAVE_SEEN | CAVE_EXPLORED);
    }

    /* Save in the "view" array */
    view_grids.append(make_coords(p_ptr->py, p_ptr->px));

    /* Save in the "fire" array */
    fire_grids.append(make_coords(p_ptr->py, p_ptr->px));

    /*** Step 2b -- octants (CAVE_VIEW + CAVE_SEEN) ***/

    /* Scan each octant */
    for (o2 = 0; o2 < 8; o2++)
    {
        vinfo_type *p;

        /* Last added */
        vinfo_type *last = &vinfo[0];

        /* Grid queue */
        int queue_head = 0;
        int queue_tail = 0;
        vinfo_type *queue[VINFO_MAX_GRIDS*2];

        /* Slope bit vector */
        u32b bits0 = VINFO_BITS_0;
        u32b bits1 = VINFO_BITS_1;
        u32b bits2 = VINFO_BITS_2;
        u32b bits3 = VINFO_BITS_3;

        /* Reset queue */
        queue_head = queue_tail = 0;

        /* Initial grids */
        queue[queue_tail++] = &vinfo[1];
        queue[queue_tail++] = &vinfo[2];

        /* Process queue */
        while (queue_head < queue_tail)
        {
            /* Dequeue next grid */
            p = queue[queue_head++];            

            /* Check bits */
            if ((bits0 & (p->bits_0)) ||
                (bits1 & (p->bits_1)) ||
                (bits2 & (p->bits_2)) ||
                (bits3 & (p->bits_3)))
            {
                /* Extract grid value XXX XXX XXX */
                g = pg + p->grid[o2];

                /* Get grid info */
                dungeon_type *dun_ptr = &dungeon_info[GRID_Y(g)][GRID_X(g)];

                /* Handle opaque grids */
                if (!(dun_ptr->cave_info & (CAVE_LOS)))
                {                    
                    /* Clear bits */
                    bits0 &= ~(p->bits_0);
                    bits1 &= ~(p->bits_1);
                    bits2 &= ~(p->bits_2);
                    bits3 &= ~(p->bits_3);

                    /* Newly viewable grid */
                    if (!(dun_ptr->cave_info & (CAVE_VIEW)))
                    {
                        /* Mark as viewable */
                        dun_ptr->cave_info |= (CAVE_VIEW);

                        /* Torch-lit grids */
                        if (p->d < radius)
                        {
                            /* Mark as "CAVE_SEEN" */
                            dun_ptr->cave_info |= (CAVE_SEEN | CAVE_EXPLORED);
                        }

                        /* Perma-lit grids */
                        else if (dun_ptr->cave_info & (CAVE_GLOW))
                        {
                            int y = GRID_Y(g);
                            int x = GRID_X(g);

                            /* Hack -- move towards player */
                            int yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
                            int xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

                            /* Check for "complex" illumination */
                            if (((dungeon_info[yy][xx].cave_info & (CAVE_LOS)) &&
                                 (dungeon_info[yy][xx].cave_info & (CAVE_GLOW))) ||
                                ((dungeon_info[y][xx].cave_info & (CAVE_LOS)) &&
                                 (dungeon_info[y][xx].cave_info & (CAVE_GLOW))) ||
                                ((dungeon_info[yy][x].cave_info & (CAVE_LOS)) &&
                                 (dungeon_info[yy][x].cave_info & (CAVE_GLOW))))
                            {
                                /* Mark as seen */
                                dun_ptr->cave_info |= (CAVE_SEEN | CAVE_EXPLORED);
                            }
                        }

                        view_grids.append(make_coords(GRID_Y(g), GRID_X(g)));
                    }
                }

                /* Handle transparent grids */
                else
                {                    
                    /* Enqueue child */
                    if (last != p->next_0)
                    {
                        queue[queue_tail++] = last = p->next_0;
                    }

                    /* Enqueue child */
                    if (last != p->next_1)
                    {
                        queue[queue_tail++] = last = p->next_1;
                    }

                    /* Newly viewable grid */
                    if (!(dun_ptr->cave_info & (CAVE_VIEW)))
                    {
                        /* Mark as "viewable" */
                        dun_ptr->cave_info |= (CAVE_VIEW);

                        /* Torch-lit grids */
                        if (p->d < radius)
                        {
                            /* Mark as "CAVE_SEEN" */
                            dun_ptr->cave_info |= (CAVE_SEEN | CAVE_EXPLORED);
                        }

                        /* Perma-lit grids */
                        else if (dun_ptr->cave_info & (CAVE_GLOW | CAVE_HALO))
                        {
                            /* Mark as "CAVE_SEEN" */
                            dun_ptr->cave_info |= (CAVE_SEEN | CAVE_EXPLORED);
                        }

                        /* Save in array */
                        view_grids.append(make_coords(GRID_Y(g), GRID_X(g)));
                    }
                }
            }
        }
    }

    /*** Step 3b -- octants (CAVE_FIRE) ***/

    /* Scan each octant */
    for (o2 = 0; o2 < 8; o2++)
    {
        vinfo_type *p;

        /* Last added */
        vinfo_type *last = &vinfo[0];

        /* Grid queue */
        int queue_head = 0;
        int queue_tail = 0;
        vinfo_type *queue[VINFO_MAX_GRIDS*2];

        /* Slope bit vector */
        u32b bits0 = VINFO_BITS_0;
        u32b bits1 = VINFO_BITS_1;
        u32b bits2 = VINFO_BITS_2;
        u32b bits3 = VINFO_BITS_3;

        /* Reset queue */
        queue_head = queue_tail = 0;

        /* Initial grids */
        queue[queue_tail++] = &vinfo[1];
        queue[queue_tail++] = &vinfo[2];

        /* Process queue */
        while (queue_head < queue_tail)
        {
            /* Assume no line of fire */
            bool line_fire = FALSE;

            /* Dequeue next grid */
            p = queue[queue_head++];

            /* Check bits */
            if ((bits0 & (p->bits_0)) ||
                (bits1 & (p->bits_1)) ||
                (bits2 & (p->bits_2)) ||
                (bits3 & (p->bits_3)))
            {
                /* Extract grid value XXX XXX XXX */
                g = pg + p->grid[o2];

                /* Get grid info */
                dungeon_type *dun_ptr = &dungeon_info[GRID_Y(g)][GRID_X(g)];

                /* Check for first possible line of fire */
                i = p->slope_fire_index1;

                /* Check line(s) of fire */
                while (TRUE)
                {
                    switch (i / 32)
                    {
                        case 3:
                        {
                            if (bits3 & (1L << (i % 32))) line_fire = TRUE;
                            break;
                        }
                        case 2:
                        {
                            if (bits2 & (1L << (i % 32))) line_fire = TRUE;
                            break;
                        }
                        case 1:
                        {
                            if (bits1 & (1L << (i % 32))) line_fire = TRUE;
                            break;
                        }
                        case 0:
                        {
                            if (bits0 & (1L << (i % 32))) line_fire = TRUE;
                            break;
                        }
                    }

                    /* Check second LOF slope only if necessary */
                    if ((line_fire) ||
                        (p->slope_fire_index2 == p->slope_fire_index1) ||
                        (i == p->slope_fire_index2))
                    {
                        break;
                    }

                    /* Check second possible line of fire */
                    i = p->slope_fire_index2;
                }

                /* Note line of fire */
                if (line_fire)
                {
                    dun_ptr->cave_info |= (CAVE_FIRE);

                    /* Save in array */
                    fire_grids.append(make_coords(GRID_Y(g), GRID_X(g)));
                }

                /* Handle non-projectable grids */
                if (!(dun_ptr->cave_info & (CAVE_PROJECT)))
                {
                    /* Clear bits */
                    bits0 &= ~(p->bits_0);
                    bits1 &= ~(p->bits_1);
                    bits2 &= ~(p->bits_2);
                    bits3 &= ~(p->bits_3);
                }

                /* Handle projectable grids */
                else
                {
                    /* Enqueue child */
                    if (last != p->next_0)
                    {
                        queue[queue_tail++] = last = p->next_0;
                    }

                    /* Enqueue child */
                    if (last != p->next_1)
                    {
                        queue[queue_tail++] = last = p->next_1;
                    }
                }
            }
        }
    }


    /*** Step 4 -- Complete the algorithm ***/

    /* Handle blindness */
    if (p_ptr->timed[TMD_BLIND])
    {
        /* Process "new" grids */
        for (i = 0; i < view_grids.size(); i++)
        {
            dungeon_type *dun_ptr = &dungeon_info[view_grids[i].y][view_grids[i].x];

            /* Grid cannot be "CAVE_SEEN" */
            dun_ptr->cave_info &= ~(CAVE_SEEN);
        }
    }

    /* Process "new" grids */
    for (i = 0; i < view_grids.size(); i++)
    {
        dungeon_type *dun_ptr = &dungeon_info[view_grids[i].y][view_grids[i].x];

        /* Was not "CAVE_SEEN", is now "CAVE_SEEN" */
        if ((dun_ptr->cave_info & (CAVE_SEEN)) && !(dun_ptr->cave_info & (CAVE_TEMP)))
        {
            /* Note */
            note_spot(view_grids[i].y, view_grids[i].x);

            /* Redraw */
            light_spot(view_grids[i].y, view_grids[i].x);
        }
    }

    /* Process "old" grids */
    for (i = 0; i < prev_seen_list.size(); i++)
    {
        dungeon_type *dun_ptr = &dungeon_info[prev_seen_list[i].y][prev_seen_list[i].x];

        /* Was "CAVE_SEEN", is now not "CAVE_SEEN" */
        if (!(dun_ptr->cave_info & (CAVE_SEEN)))
        {
            /* Redraw */
            light_spot(prev_seen_list[i].y, prev_seen_list[i].x);
        }

        dun_ptr->cave_info &= ~(CAVE_TEMP);
    }
}


static int get_energy_no_doors(int y, int x, byte which_flow, u32b elem_flag)
{
    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /*Unused*/
    (void)elem_flag;

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x)) return (0);
    }

    /*This is an elemental terrain*/
    if (_feat_ff3_match(f_ptr, TERRAIN_MASK))
    {
        /*Monsters don't want to suffer damage going to this square*/
        if (f_ptr->dam_non_native) return (0);
    }

    /*Use non-native energy movement*/
    return(f_ptr->non_native_energy_move);
}


static int get_energy_pass_doors(int y, int x, byte which_flow, u32b elem_flag)
{

    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /*Use the elem_flag for a completey different purpose. */
    elem_flag = _feat_ff3_match(f_ptr, TERRAIN_MASK);

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x))
        {
             /* An effect is preventing movement. Ignore grid */
            if (dungeon_info[y][x].has_effect()) return (0);

            /* Closed doors are allowed */
            if (!_feat_ff1_match(f_ptr, FF1_DOOR)) return (0);
        }
    }

    /*This is an elemental terrain*/
    if (elem_flag)
    {
        /* Mark any flows that need doing*/
        p_ptr->update |= elem_flag;

        /*Monsters don't want to suffer damage going to this square*/
        if (f_ptr->dam_non_native) return (0);
    }

    /*Use non-native energy movement*/
    return(f_ptr->non_native_energy_move);
}


static int get_energy_elemental_flow(int y, int x, byte which_flow, u32b elem_flag)
{
    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x))
        {
            /* An effect is preventing movement. Ignore grid */
            if (dungeon_info[y][x].has_effect()) return (0);

            /* Closed doors are allowed */
            if (!_feat_ff1_match(f_ptr, FF1_DOOR)) return (0);
        }
    }

    /*If a match, return the native movement energy*/
    if (_feat_ff3_match(f_ptr, elem_flag))
    {
        return (f_ptr->native_energy_move);
    }

    /*Not native*/
    /*Monsters don't want to suffer damage going to this square*/
    if (f_ptr->dam_non_native) return (0);

    /*Use non-native energy movement*/
    return(f_ptr->non_native_energy_move);
}


static int get_energy_elemental_flow_no_doors(int y, int x, byte which_flow, u32b elem_flag)
{
    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x)) return 0;
    }

    /*If a match, return the native movement energy*/
    if (_feat_ff3_match(f_ptr, elem_flag))
    {
        return (f_ptr->native_energy_move);
    }

    /*Not native*/
    /*Monsters don't want to suffer damage going to this square*/
    if (f_ptr->dam_non_native) return (0);

    /*Use non-native energy movement*/
    return(f_ptr->non_native_energy_move);

}


static int get_energy_flying(int y, int x, byte which_flow, u32b elem_flag)
{

    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /* Use the elem_flag for a completey different purpose */
    elem_flag = _feat_ff2_match(f_ptr, FF2_CAN_FLY);

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x))
        {
            /* An effect is preventing movement. Ignore */
            if (dungeon_info[y][x].has_effect()) return 0;

            /* Feature doesn't have the CAN_FLY flag and it isn't a door. Ignore */
            if (!elem_flag && !_feat_ff1_match(f_ptr, FF1_DOOR)) return 0;
        }
    }

    /*We can fly here, non-native damage doesn't matter*/
    if (elem_flag)
    {
        return (BASE_ENERGY_MOVE);
    }

    /*Monsters don't want to suffer damage going to this square*/
    if (f_ptr->dam_non_native) return (0);

    /*Use non-native energy movement*/
    return (f_ptr->non_native_energy_move);
}


static int get_energy_flying_no_doors(int y, int x, byte which_flow, u32b elem_flag)
{

    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /* Use the elem_flag for a completey different purpose */
    elem_flag = _feat_ff2_match(f_ptr, FF2_CAN_FLY);

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /* Can't move here */
        if (!cave_passable_bold(y, x))
        {
            /* An effect is preventing movement. Ignore */
            if (dungeon_info[y][x].has_effect()) return 0;

            /* Feature doesn't have the CAN_FLY. Ignore */
            if (!elem_flag) return 0;
        }
    }

    /*We can fly here, non-native damage doesn't matter*/
    if (elem_flag)
    {
        return (BASE_ENERGY_MOVE);
    }

    /*Monsters don't want to suffer damage going to this square*/
    if (f_ptr->dam_non_native) return (0);

    /*Use non-native energy movement*/
    return (f_ptr->non_native_energy_move);
}

/*Movement through walls*/
static int get_energy_pass_walls(int y, int x, byte which_flow, u32b elem_flag)
{

    /*Quick pointer*/
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /*Unused*/
    (void)elem_flag;

    /*We have not done this flow yet*/
    if (cave_cost[which_flow][y][x] == 0)
    {
        /*The monster can't go here*/
        if (_feat_ff1_match(f_ptr, FF1_PERMANENT | FF1_MOVE) == (FF1_PERMANENT)) return (0);
    }

    /*Monsters don't want to suffer damage going to this square*/
    if (f_ptr->dam_non_native) return (0);

    /*Use non-native energy movement*/
    return(MIN(BASE_ENERGY_MOVE, f_ptr->non_native_energy_move));
}


/*
 *
 *
 *Partial update of the monster movement flow code.
 */
static void update_flow_partial(byte which_flow, u32b elem_flag)
{

    int i, d, k;
    byte y, x, y2, x2;
    int last_index;
    int grid_count = 0;


    /* Note where we get information from, and where we overwrite */
    int this_cycle = 0;
    int next_cycle = 1;

    byte flow_table[2][2][15 * FLOW_LENGTH_PARTIAL];

    /*Reduce the base flow*/
    cost_at_center[which_flow] -= (ENERGY_TO_MOVE * 2);

    /*Point to the right function for the flows*/
    if (which_flow == FLOW_NO_DOORS) get_energy_to_move = get_energy_no_doors;
    else if (which_flow == FLOW_PASS_DOORS) get_energy_to_move = get_energy_pass_doors;
    else if (which_flow == FLOW_FLYING) get_energy_to_move = get_energy_flying;
    else if (which_flow == FLOW_FLYING_NO_DOORS) get_energy_to_move = get_energy_flying_no_doors;
    else if (which_flow == FLOW_PASS_WALLS) get_energy_to_move = get_energy_pass_walls;
    /* All the native elements*/
    else if (which_flow >= ELEM_FLOW_BASE_NO_DOORS) get_energy_to_move = get_energy_elemental_flow_no_doors;
    else get_energy_to_move = get_energy_elemental_flow;

    /* Store base cost at the character location */
    cave_cost[which_flow][p_ptr->py][p_ptr->px] = cost_at_center[which_flow];

    /* Store this first grid in the flow table, note that we've done so */
    flow_table[this_cycle][0][0] = p_ptr->py;
    flow_table[this_cycle][1][0] = p_ptr->px;
    grid_count = 1;

    /*** Partial Update of the flow ***/

    /* Extend the noise burst out to its limits */
    for (k = 0; k < FLOW_LENGTH_PARTIAL; k++)
    {
        /* Get the number of grids we'll be looking at */
        last_index = grid_count;

        /* Stop if we've run out of work to do */
        if (last_index == 0) break;

        /* Clear the grid count */
        grid_count = 0;

        /* Get each valid entry in the flow table in turn. */
        for (i = 0; i < last_index; i++)
        {
            /* Get this grid */
            y = flow_table[this_cycle][0][i];
            x = flow_table[this_cycle][1][i];

            /* Look at all adjacent grids */
            for (d = 0; d < 8; d++)
            {
                int new_cave_cost;
                int energy_to_move;

                /* Child location */
                y2 = y + ddy_ddd[d];
                x2 = x + ddx_ddd[d];

                /* Check Bounds */
                if (!in_bounds(y2, x2)) continue;

                /*Use a functional pointer for speed to get the proper energy*/
                energy_to_move = (*get_energy_to_move)(y2, x2, which_flow, elem_flag);

                /*Not a useable square*/
                if (energy_to_move == 0) continue;

                /*Calculate the energy required to move to this square*/
                new_cave_cost = cave_cost[which_flow][y][x] + energy_to_move;

                /*This is not a quicker route*/
                if ((cave_cost[which_flow][y2][x2] > 0) &&
                         (cave_cost[which_flow][y2][x2] <= new_cave_cost)) continue;

                /* Store cost at this location */
                cave_cost[which_flow][y2][x2] = new_cave_cost;

                /*Don't store the same grid twice*/
                if (dungeon_info[y2][x2].path_flow) continue;

                /*mark that we stored this grid for the next cycle*/
                dungeon_info[y2][x2].path_flow = TRUE;

                /* Store this grid in the flow table */
                flow_table[next_cycle][0][grid_count] = y2;
                flow_table[next_cycle][1][grid_count] = x2;

                /* Increment number of grids stored */
                grid_count++;
            }
        }

        /*Clear the CAVE_FLOW marks*/
        for (i = 0; i < grid_count; i++)
        {
            /* Not very legible, but this is using the y and x coordinates from the nest cycle*/
            dungeon_info[flow_table[next_cycle][0][i]][flow_table[next_cycle][1][i]].path_flow = FALSE;
        }

        /* Swap write and read portions of the table */
        if (this_cycle == 0)
        {
            this_cycle = 1;
            next_cycle = 0;
        }
        else
        {
            this_cycle = 0;
            next_cycle = 1;
        }
    }
}


/*
 *Clear the monster_flow for partial updating
 */
static void update_flows_partial_prep(void)
{
    int i, j, d, k;
    byte y, x, y2, x2;
    int last_index;
    int grid_count = 0;

    /* Note where we get information from, and where we overwrite */
    int this_cycle = 0;
    int next_cycle = 1;

    byte flow_table[2][2][12 * FLOW_LENGTH_PARTIAL];

    byte clear_flows[MAX_FLOWS];
    byte active_flows = 0;

    /*Clear all the cave_cost at the player center*/
    /*Also record the active flows*/
    for (i = FLOW_NO_DOORS; i < MAX_FLOWS; i++)
    {
        /*We have an active flow*/
        if (cost_at_center[i] > 0)
        {
            clear_flows[active_flows] = i;
            active_flows++;

            /* Store cost at this location */
            cave_cost[i][p_ptr->py][p_ptr->px] = 0;
        }
    }

    /* Save the last update noise epicenter */
    p_ptr->update_center_y = p_ptr->py;
    p_ptr->update_center_x = p_ptr->px;

    /* Store this first grid in the flow table, note that we've done so */
    flow_table[this_cycle][0][0] = p_ptr->py;
    flow_table[this_cycle][1][0] = p_ptr->px;
    grid_count = 1;

    /*** Partial Update of the flow ***/

    /* Extend the noise burst out to its limits */
    for (k = 0; k < FLOW_LENGTH_PARTIAL; k++)
    {
        /* Get the number of grids we'll be looking at */
        last_index = grid_count;

        /* Stop if we've run out of work to do */
        if (last_index == 0) break;

        /* Clear the grid count */
        grid_count = 0;

        /* Get each valid entry in the flow table in turn. */
        for (i = 0; i < last_index; i++)
        {
            /* Get this grid */
            y = flow_table[this_cycle][0][i];
            x = flow_table[this_cycle][1][i];

            /* Look at all adjacent grids */
            for (d = 0; d < 8; d++)
            {
                /* Child location */
                y2 = y + ddy_ddd[d];
                x2 = x + ddx_ddd[d];

                /* Check Bounds */
                if (!in_bounds(y2, x2)) continue;

                /*Ignore updated features already*/

                /* Ignore walls. */
                if (!cave_passable_bold(y2, x2)) continue;

                /*We have done this one already*/
                if (cave_cost[FLOW_PASS_DOORS][y2][x2] == 0) continue;

                /*Clear all the active flows*/
                for (j = 0; j < active_flows; j++)
                {
                    /* Store cost at this location */
                    cave_cost[clear_flows[j]][y2][x2] = 0;
                }

                /* Store this grid in the flow table */
                flow_table[next_cycle][0][grid_count] = y2;
                flow_table[next_cycle][1][grid_count] = x2;

                /* Increment number of grids stored */
                grid_count++;
            }
        }

        /* Swap write and read portions of the table */
        if (this_cycle == 0)
        {
            this_cycle = 1;
            next_cycle = 0;
        }
        else
        {
            this_cycle = 0;
            next_cycle = 1;
        }
    }
}


/*
 * Fill in the "cave_cost" field of every grid that the player can
 * reach with the amount of time needed to reach that grid.
 * Monsters use this information by moving to adjacent grids with
 * lower flow costs, thereby homing in on the player even though
 * twisty tunnels and mazes.  Monsters can also run away.
 *
 *
 * The flow table is three-dimensional.  The first dimension allows the
 * table to both store and overwrite grids safely.  The second indicates
 * whether this value is that for x or for y.  The third is the number
 * of grids able to be stored at any flow distance.
 */

static void update_flow_full(byte which_flow, u32b elem_flag)
{
    int i, d, k;
    byte y, x, y2, x2;
    int last_index;
    int grid_count = 0;


    /* Note where we get information from, and where we overwrite */
    int this_cycle = 0;
    int next_cycle = 1;

    byte flow_table[2][2][10 * FLOW_LENGTH_FULL];

    /*
     * Set the initial cost to 10000 updates will progressively
     * lower this value.  When it reaches zero, another full
     * rebuild has to be done.
     */
    cost_at_center[which_flow] = BASE_FLOW_CENTER;

    /*Point to the right function for the flows*/
    if (which_flow == FLOW_NO_DOORS) get_energy_to_move = get_energy_no_doors;
    else if (which_flow == FLOW_PASS_DOORS) get_energy_to_move = get_energy_pass_doors;
    else if (which_flow == FLOW_FLYING) get_energy_to_move = get_energy_flying;
    else if (which_flow == FLOW_FLYING_NO_DOORS) get_energy_to_move = get_energy_flying_no_doors;
    else if (which_flow == FLOW_PASS_WALLS) get_energy_to_move = get_energy_pass_walls;
    /* All the native elements*/
    else if (which_flow >= ELEM_FLOW_BASE_NO_DOORS) get_energy_to_move = get_energy_elemental_flow_no_doors;
    else get_energy_to_move = get_energy_elemental_flow;


    /*** Create the flow ***/

    /* Store base cost at the character location */
    cave_cost[which_flow][p_ptr->py][p_ptr->px] = BASE_FLOW_CENTER;

    /* Store this first grid in the flow table, note that we've done so */
    flow_table[this_cycle][0][0] = p_ptr->py;
    flow_table[this_cycle][1][0] = p_ptr->px;
    grid_count = 1;

    /* Extend the flow information out to its limits */
    for (k = 0; k < FLOW_LENGTH_FULL; k++)
    {
        /* Get the number of grids we'll be looking at */
        last_index = grid_count;

        /* Stop if we've run out of work to do */
        if (last_index == 0) break;

        /* Clear the grid count */
        grid_count = 0;

        /* Get each valid entry in the flow table in turn. */
        for (i = 0; i < last_index; i++)
        {
            /* Get this grid */
            y = flow_table[this_cycle][0][i];
            x = flow_table[this_cycle][1][i];

            /* Look at all adjacent grids */
            for (d = 0; d < 8; d++)
            {
                int new_cave_cost;
                int energy_to_move;

                /* Child location */
                y2 = y + ddy_ddd[d];
                x2 = x + ddx_ddd[d];

                /* Check Bounds */
                if (!in_bounds(y2, x2)) continue;

                /*Use a functional pointer for speed to get the proper energy*/
                energy_to_move = (*get_energy_to_move)(y2, x2, which_flow, elem_flag);

                /*Not a usable square*/
                if (energy_to_move == 0) continue;

                /*
                 * Calculate the energy required to move to this square
                 * from the previous square.
                 */
                new_cave_cost = cave_cost[which_flow][y][x] + energy_to_move;

                /*This is not a quicker route*/
                if ((cave_cost[which_flow][y2][x2] > 0) &&
                         (cave_cost[which_flow][y2][x2] <= new_cave_cost)) continue;


                /* Store cost at this location */
                cave_cost[which_flow][y2][x2] = new_cave_cost;

                /*Don't store the same grid twice*/
                if (dungeon_info[y2][x2].path_flow) continue;

                /*mark that we stored this grid for the next cycle*/
                dungeon_info[y2][x2].path_flow = TRUE;

                /* Store this grid in the flow table */
                flow_table[next_cycle][0][grid_count] = y2;
                flow_table[next_cycle][1][grid_count] = x2;

                /* Increment number of grids stored */
                grid_count++;
            }
        }

        /*Clear the CAVE_FLOW marks*/
        for (i = 0; i < grid_count; i++)
        {
            /* Not very legible, but this is using the y and x coordinates from the nest cycle*/
            dungeon_info[flow_table[next_cycle][0][i]][flow_table[next_cycle][1][i]].path_flow = FALSE;
        }

        /* Swap write and read portions of the table */
        if (this_cycle == 0)
        {
            this_cycle = 1;
            next_cycle = 0;
        }
        else
        {
            this_cycle = 0;
            next_cycle = 1;
        }
    }
}


/*
 * Clear the flow information and prepare to build completely new flow information
 */
static void update_flows_full_prep(void)
{
    int i;

    /* Clear the cost at centers */
    /* Clear the cost at centers and all flow (noise) information */
    for (i = FLOW_NO_DOORS; i < MAX_FLOWS; i++)
    {
        if (cost_at_center[i] > 0)
        {
            WIPE(&cave_cost[i][0][0], u16b_dungeon);
        }

        cost_at_center[i] = 0;
    }

    /* Save the new noise epicenter */
    p_ptr->flow_center_y = p_ptr->py;
    p_ptr->flow_center_x = p_ptr->px;
    p_ptr->update_center_y = p_ptr->py;
    p_ptr->update_center_x = p_ptr->px;

    /*Clear the flows*/

    p_ptr->update &= ~(TERRAIN_MASK);

    /*
     * All monsters need to re-consider their targets and flow.
     * Townsman AI or fleeing monsters are the exception
     */

    /* Process the monsters */
    for (i = 1; i < mon_max; i++)
    {
        /* Get the monster */
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Ignore dead monsters */
        if (!m_ptr->r_idx) continue;

        /*re-evaluate the flow the monster is using*/
        m_ptr->using_flow = NEED_FLOW;

        /*Do we need the flying flow*/
        if (r_ptr->flags3 & RF3_FLYING)
        {
            /*If we are in range, we need this flow*/
            if (m_ptr->mflag & (MFLAG_ACTV)) p_ptr->update |= (PU_FLOW_FLYING);
        }

        /*Do we need the pass_walls flow*/
        if ((r_ptr->flags2 & RF2_KILL_WALL) ||
            (r_ptr->flags2 & RF2_PASS_WALL))
        {
            /*Are we in range?*/
            if (m_ptr->mflag & (MFLAG_ACTV)) p_ptr->update |= (PU_FLOW_PASS_WALLS);
        }

         /* The monster cannot handle doors so we must use the doorless versions of the flows */
        if (MONSTER_HATES_DOORS(r_ptr))
        {
            /*Are we in range?*/
            if (m_ptr->mflag & (MFLAG_ACTV)) p_ptr->update |= (PU_FLOW_NO_DOORS_SPECIAL);
        }

        /*
         * Always a target y for each target x.
         * Don't clear the townman AI or fleeing monsters
         */
        if ((m_ptr->target_x) && (!(m_ptr->mflag & (MFLAG_TOWN))) &&
            (m_ptr->min_range != FLEE_RANGE))
        {
            /*We need to re-evaluate target*/
            m_ptr->target_x = m_ptr->target_y = 0;
        }
    }
}


/*
 * Fill in the "cave_cost" field of every grid that the player can
 * reach with the number of steps needed to reach that grid.  This
 * also yields the route distance of the player from every grid.
 * Monsters use this information by moving to adjacent grids with
 * lower flow costs, thereby homing in on the player even though
 * twisty tunnels and mazes.  Monsters can also run away from loud
 * noises.
 *
 *
 * The flow table is three-dimensional.  The first dimension allows the
 * table to both store and overwrite grids safely.  The second indicates
 * whether this value is that for x or for y.  The third is the number
 * of grids able to be stored at any flow distance.
 */

void update_flows(bool full)
{
    /*First check if we need a full update*/
    if (!full)
    {
        if (p_ptr->update & (PU_FLOW_DOORS | PU_FLOW_NO_DOORS)) full = TRUE;

        /*We have done something to move more than two squares in one turn*/
        else if (distance(p_ptr->py, p_ptr->px,
                p_ptr->update_center_y, p_ptr->update_center_x) > 1) full = TRUE;

        /*we have moved too far since the last full update*/
        else if (distance(p_ptr->flow_center_y, p_ptr->flow_center_x,
                p_ptr->update_center_y, p_ptr->update_center_x) >
                            MAX_DISTANCE_BETWEEN_UPDATES) full = TRUE;
        /*We can't do any more partial updates*/
        else if (cost_at_center[FLOW_NO_DOORS] < 500) full = TRUE;

        /* Arena levels are too small to have partial updates */
        else if (p_ptr->dungeon_type == DUNGEON_TYPE_ARENA) full = TRUE;
    }

    if (!full)
    {
        u32b elemental_flows = ELEM_BASE;

        int i;

        /*clear all the old information*/
        update_flows_partial_prep();

        /*Partial Update the two main flows, and the two non-elemental flows if applicable*/
        update_flow_partial(FLOW_PASS_DOORS, 0L);
        update_flow_partial(FLOW_NO_DOORS, 0L);
        if (cost_at_center[FLOW_FLYING] > 0)		update_flow_partial(FLOW_FLYING, 0L);
        if (cost_at_center[FLOW_FLYING_NO_DOORS] > 0)   update_flow_partial(FLOW_FLYING_NO_DOORS, 0L);
        if (cost_at_center[FLOW_PASS_WALLS] > 0)	update_flow_partial(FLOW_PASS_WALLS, 0L);

        p_ptr->update &= ~(PU_FLOW_DOORS | PU_FLOW_NO_DOORS | PU_FLOW_FLYING | PU_FLOW_PASS_WALLS);

        /*Check all the optional flows*/
        for (i = ELEM_FLOW_BASE; i <= ELEM_FLOW_TAIL; i++)
        {
            int j;

            /*Update if necessary*/
            if (cost_at_center[i] > 0)
            {
                /*Update the flow*/
                update_flow_partial(i, elemental_flows);
            }

            /* Get the respective NO_DOORS flow */
            j = i - ELEM_FLOW_BASE + ELEM_FLOW_BASE_NO_DOORS;

            /* Update if necessary */
            if (cost_at_center[j] > 0)
            {
                /* Update the flow */
                update_flow_partial(j, elemental_flows);
            }

            /*Clear the flow marker*/
            p_ptr->update &= ~(elemental_flows);

            /*Shift to the next flow*/
            elemental_flows = (elemental_flows << 1);
        }
    }
    else
    {
        int flow;
        u32b element;

        /*clear all the old information*/
        update_flows_full_prep();

        /*Update the two main flows, and the flying or pass walls flow as needed*/
        update_flow_full(FLOW_PASS_DOORS, 0L);
        update_flow_full(FLOW_NO_DOORS, 0L);
        if (p_ptr->update & (PU_FLOW_FLYING))
        {
            update_flow_full(FLOW_FLYING, 0L);

            /* Update the doorless version of the FLYING flow if necessary */
            if (p_ptr->update & (PU_FLOW_NO_DOORS_SPECIAL))
            {
                update_flow_full(FLOW_FLYING_NO_DOORS, 0L);
            }
        }

        if (p_ptr->update & (PU_FLOW_PASS_WALLS)) update_flow_full(FLOW_PASS_WALLS, 0L);
        p_ptr->update &= ~(PU_FLOW_DOORS | PU_FLOW_NO_DOORS | PU_FLOW_FLYING | PU_FLOW_PASS_WALLS);

        element = ELEM_BASE;

        /* Update the elemental flows if necessary */
        for (flow = ELEM_FLOW_BASE; flow <= ELEM_FLOW_TAIL; flow++)
        {
            /* Only if we are allowed */
            if (p_ptr->update & element)
            {
                /* Clear the mark */
                p_ptr->update &= ~element;

                /* Do it */
                update_flow_full(flow, element);

                /* Update the doorless version of the flow if requested */
                if (p_ptr->update & (PU_FLOW_NO_DOORS_SPECIAL))
                {
                    /* Do it */
                    update_flow_full(flow - ELEM_FLOW_BASE + ELEM_FLOW_BASE_NO_DOORS, element);
                }
            }

            /* Get the next element flag */
            element <<= 1;
        }

        /* Clear any mark */
        p_ptr->update &= ~(PU_FLOW_NO_DOORS_SPECIAL);
    }
}


/*
 * Light up the dungeon using "claravoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", memorizes all grids as with magic mapping, and, under the
 * standard option settings (view_perma_grids but not view_torch_grids)
 * memorizes all floor grids too.
 *
 * Note that if "view_perma_grids" is not set, we do not memorize floor
 * grids, since this would defeat the purpose of "view_perma_grids", not
 * that anyone seems to play without this option.
 *
 * Note that if "view_torch_grids" is set, we do not memorize floor grids,
 * since this would prevent the use of "view_torch_grids" as a method to
 * keep track of what grids have been observed directly.
 */
void wiz_light(void)
{
    int i, y, x;

    /* Memorize objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Skip held objects */
        if (o_ptr->held_m_idx) continue;

        /* Memorize */
        o_ptr->mark_object();
    }

    /* Scan all normal grids */
    for (y = 0; y < p_ptr->cur_map_hgt-1; y++)
    {
        /* Scan all normal grids */
        for (x = 0; x < p_ptr->cur_map_wid-1; x++)
        {
            /* Process all non-walls, but don't count rubble */
            if (!(f_info[dungeon_info[y][x].feature_idx].f_flags1 & (FF1_WALL)))
            {
                /* Scan all neighbors */
                for (i = 0; i < 9; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Perma-lite the grid */
                    dungeon_info[yy][xx].cave_info |= (CAVE_GLOW);

                    /* Memorize normal features */
                    if (f_info[dungeon_info[yy][xx].feature_idx].f_flags1 & (FF1_REMEMBER))
                    {
                        /* Memorize the grid */
                        dungeon_info[yy][xx].mark_square();
                    }

                    /* Normally, memorize floors (see above) */
                    if (view_perma_grids && !view_torch_grids)
                    {
                        /* Memorize the grid */
                        dungeon_info[yy][xx].mark_square();
                    }
                }
            }
        }
    }

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP | PR_WIN_MONLIST | PR_WIN_OBJLIST);

}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
    int i, y, x;


    /* Forget every grid */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Process the grid */
            dungeon_info[y][x].cave_info &= ~(CAVE_MARK | CAVE_DTRAP | CAVE_EXPLORED);
        }
    }

    /* Forget all objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Skip held objects */
        if (o_ptr->held_m_idx) continue;

        /* Forget the object */
        o_ptr->marked = FALSE;
    }

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP | PR_WIN_MONLIST | PR_WIN_OBJLIST);
}



/*
 * Light or Darken the town
 */
void town_illuminate(bool daytime)
{
    int y, x, i;

    /* Apply light or darkness */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            bool always_lit = FALSE;

            /* Obvious */
            if (dungeon_info[y][x].feature_idx != FEAT_COBBLESTONE_FLOOR) always_lit = TRUE;

            /* Glyphs and visible traps */
            else if (cave_any_trap_bold(y, x) &&
                    !(x_list[dungeon_info[y][x].effect_idx].x_flags & (EF1_HIDDEN))) always_lit = TRUE;

            /* Interesting grids */
            if (always_lit)
            {
                /* Illuminate the grid */
                dungeon_info[y][x].cave_info |= (CAVE_GLOW);

                /* Memorize the grid */
                dungeon_info[y][x].mark_square();
            }

            /* Boring grids (light) */
            else if (daytime)
            {
                /* Illuminate the grid */
                dungeon_info[y][x].cave_info |= (CAVE_GLOW);

                /* Hack -- Memorize grids */
                if (view_perma_grids)
                {
                    dungeon_info[y][x].mark_square();
                }
            }

            /* Boring grids (dark) */
            else
            {
                /* Darken the grid */
                dungeon_info[y][x].cave_info &= ~(CAVE_GLOW);

                /* Hack -- Forget grids */
                if (view_perma_grids)
                {
                    dungeon_info[y][x].cave_info &= ~(CAVE_MARK | CAVE_EXPLORED);
                }
            }
        }
    }

    /* Handle shop doorways */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Track shop doorways */
            if (dungeon_info[y][x].is_store())
            {
                for (i = 0; i < 8; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Illuminate the grid */
                    dungeon_info[yy][xx].cave_info |= (CAVE_GLOW);

                    /* Hack -- Memorize grids */
                    if (view_perma_grids)
                    {
                        dungeon_info[yy][xx].mark_square();
                    }
                }
            }
        }
    }

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_MONLIST | PR_MAP |  PR_WIN_OBJLIST);
}


/*
 * Set or unset the CAVE_LOS, CAVE_PROJECT and CAVE_MOVE flags of the given
 * grid depending on the terrain feature and effects contained in such grid.
 * This function is very IMPORTANT. It must be called whenever the content of
 * a grid changes (cave_set_feat_aux), we add/remove effects or we load a
 * savefile.
 *
 * CAVE_LOS controls the CAVE_VIEW and CAVE_SEEN flags.
 * CAVE_PROJECT controls the CAVE_FIRE flag.
 * CAVE_MOVE controls monster/player movement and noise flows.
 *
 * Note on CAVE_MOVE. This flag is actually equal to FF1_MOVE plus effects.
 * Some places in the sources still need to check the presence of the FF1_MOVE
 * flag sans effects. You'll see checks for CAVE_MOVE or FF1_MOVE depending
 * on the context.
 */
void update_los_proj_move(int y, int x)
{
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];
    u16b x_idx;
    u32b mask = 0;
    u32b old_flags;

    /* Save the old flags */
    old_flags = dungeon_info[y][x].cave_info;

    /* Paranoia */
    old_flags &= (CAVE_LOS | CAVE_PROJECT | CAVE_MOVE);

    /* Turn off those flags by default */
    dungeon_info[y][x].cave_info &= ~(CAVE_LOS | CAVE_PROJECT | CAVE_MOVE);

    /* Feature allows LOS */
    if (_feat_ff1_match(f_ptr, FF1_LOS)) mask |= (CAVE_LOS);

    /* Feature allows projections */
    if (_feat_ff1_match(f_ptr, FF1_PROJECT)) mask |= (CAVE_PROJECT);

    /* Feature allows movement */
    if (_feat_ff1_match(f_ptr, FF1_MOVE)) mask |= (CAVE_MOVE);

    /* Check effects if necessary */
    x_idx = dungeon_info[y][x].effect_idx;

    /* Traverse the effects applied to that grid */
    while (x_idx)
    {
        /* Get the effect */
        effect_type *x_ptr = &x_list[x_idx];

        /* Get the next effect (for the next iteration) */
        x_idx = x_ptr->next_x_idx;

        /* Ignore certain effects */
        if (!(x_ptr->x_f_idx) || (x_ptr->x_flags & (EF1_HIDDEN))) continue;

        /* Get the pseudo-feature associated to the effect */
        f_ptr = &f_info[x_ptr->x_f_idx];

        /* Disable los if necessary */
        if (!_feat_ff1_match(f_ptr, FF1_LOS)) mask &= ~(CAVE_LOS);

        /* Disable projections if necessary */
        if (!_feat_ff1_match(f_ptr, FF1_PROJECT)) mask &= ~(CAVE_PROJECT);

        /* Disable movement if necessary */
        if (!_feat_ff1_match(f_ptr, FF1_MOVE)) mask &= ~(CAVE_MOVE);
    }

    /* Turn on the collected flags */
    dungeon_info[y][x].cave_info |= mask;

    /* Request view update if necessary */
    if ((old_flags & (CAVE_LOS | CAVE_PROJECT)) !=
        (mask & (CAVE_LOS | CAVE_PROJECT)))
    {
        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }

    /* Request flow update if necessary */
    if ((old_flags & (CAVE_MOVE)) != (mask & (CAVE_MOVE)))
    {
        /* Full update of the flows */
        p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
    }
}


/*
 * Hack -- Really change the feature
 * This is the only function that should actually change a floor feature, outside of wiping
 * the dungeon feature in the class method.
 */
static void cave_set_feat_aux(int y, int x, u16b feat)
{
    /* Old feature */
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /* New feature */
    feature_type *f2_ptr = &f_info[feat];

    /* We aren't changing anything */
    if (f2_ptr == f_ptr) return;

    /* Check hidden monsters exposed by change */
    if (dungeon_info[y][x].monster_idx > 0)
    {
        /* Get the monster */
        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

        /* Get the race */
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Check if the monster is currently hidden and if the new terrain can't hide it anymore */
        if ((m_ptr->mflag & (MFLAG_HIDE)) &&
            !(feat_ff2_match(feat, FF2_COVERED) &&
                    is_monster_native_aux(feat, r_ptr->r_native)))
        {
            /* Show the monster */
            // todo unhide monster monster_unhide(m_ptr);
        }
    }

    /* Really set the feature */
    dungeon_info[y][x].feature_idx = feat;

    /* Remove inscriptions */
    if (dungeon_info[y][x].effect_idx > 0)
    {
        /* Get the index */
        s16b x_idx = dungeon_info[y][x].effect_idx;

        /* Scan all the effects on that grid */
        while (x_idx)
        {
            /* Get the effect */
            effect_type *x_ptr = &x_list[x_idx];

            /* Prepare the next index */
            x_idx = x_ptr->next_x_idx;

            /* Check inscriptions */
            if (x_ptr->x_type == EFFECT_INSCRIPTION)
            {
                /* Remove it */
                delete_effect_idx((int)(x_ptr - x_list));
            }
        }
    }

    /* Update CAVE_LOS, CAVE_PROJECT and CAVE_MOVE flags */
    update_los_proj_move(y, x);

    /* Update noise information if certain things change */
    /* EXPERIMENTAL -DG- */
    if ((f2_ptr->dam_non_native != f_ptr->dam_non_native) ||
        (f2_ptr->native_energy_move != f_ptr->native_energy_move) ||
        (f2_ptr->non_native_energy_move !=
            f_ptr->non_native_energy_move) ||
        (_feat_ff3_match(f2_ptr, TERRAIN_MASK) !=
            _feat_ff3_match(f_ptr, TERRAIN_MASK)))
    {
        /* Full update of the flows */
        p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
    }

    /* Remove the old feature from dyna_g if necessary */
    if (_feat_ff3_match(f_ptr, FF3_DYNAMIC))
    {
        remove_dynamic_terrain(y, x);
    }

    /* Add the new feature to dyna_g if necessary */
    if (_feat_ff3_match(f2_ptr, FF3_DYNAMIC))
    {
        (void)add_dynamic_terrain(y, x);
    }

    /* Update the flags of the current level */
    if (_feat_ff3_match(f2_ptr, TERRAIN_MASK))
    {
        /* Get the LF1_* flag */
        u32b flag = get_level_flag(feat);

        /* Debug message */
        if (cheat_room && !(level_flag & flag))
        {

            QString name = feature_desc(feat, TRUE, TRUE);

            message(QString("Adding %1 to the level.") .arg(name));
        }

        /* Update the level flags */
        level_flag |= flag;
    }

    /* Check if we have to reactivate some adjacent dynamic grid */
    /* We only do this when we have a generated dungeon (efficiency) */
    if (character_dungeon)
    {
        int d;

        /* Check adjacent grids */
        for (d = 0; d < 8; d++)
        {
            /* Get coordinates */
            int yy = y + ddy_ddd[d];
            int xx = x + ddx_ddd[d];

            /* Ignore annoying locations */
            if (!in_bounds(yy, xx)) continue;

            /* Ignore non-dynamic grids */
            if (!cave_ff3_match(yy, xx, FF3_DYNAMIC)) continue;

            /* The dynamic grid already is in dyna_g. Ignore */
            if (get_dynamic_terrain(yy, xx)) continue;

            /* Try to put the grid in dyna_g */
            (void)add_dynamic_terrain(yy, xx);
        }
    }

    /* Check if location changed under player */
    if ((p_ptr->py == y) && (p_ptr->px == x))
    {
        /*Track the new feature*/
        feature_kind_track(dungeon_info[y][x].feature_idx);

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);
    }

     /* Don't allow effects where players can't move. */
    if (!_feat_ff1_match(f2_ptr, FF1_MOVE) && dungeon_info[y][x].has_effect()) delete_effects(y, x);

    /* This is a generated dungeon*/
    if (character_dungeon)
    {
        /* Notice */
        note_spot(y, x);

        /* Redraw */
        light_spot(y, x);
    }
}


/*
 * Takes a location and action and changes the feature at that
 * location through applying the given action.
 */
void cave_alter_feat(int y, int x, int action)
{
    int newfeat;

    /* Set old feature */
    int oldfeat = dungeon_info[y][x].feature_idx;

    /*Mark the feature lore*/
    feature_type *f_ptr = &f_info[oldfeat];

    /*Mark the feature lore*/
    feature_lore *f_l_ptr = &f_l_list[oldfeat];

    /* Get the new feat */
    newfeat = feat_state(dungeon_info[y][x].feature_idx, action);

    /* Mark the transition in the feature_lore */
    if ((player_can_see_bold(y, x)) && player_can_observe())
    {
        int i;

        /* Check an specific entry for the transition first */
        for (i = 0; i < MAX_FEAT_STATES; i++)
        {
            /* Found the action */
            if (f_ptr->state[i].fs_action == action)
            {
                /*Count the number of times this transition has been seen*/
                if (f_l_ptr->f_l_state[i] < UCHAR_MAX) f_l_ptr->f_l_state[i]++;

                /* Done */
                break;
            }
        }

        /* Check the default if we can't find the transition */
        if ((i >= MAX_FEAT_STATES) && (f_ptr->defaults == newfeat))
        {
            /*Count the number of times this transition has been seen*/
            if (f_l_ptr->f_l_defaults < UCHAR_MAX) f_l_ptr->f_l_defaults++;
        }

    }


    /* Invisible trap */
    if (newfeat == oldfeat)
    {
        if (feat_ff3_match(oldfeat, FF3_PICK_TRAP))
        {
            /*Mark the lore*/
            if (player_can_see_bold(y, x)) f_l_ptr->f_l_flags3 |= (FF3_PICK_TRAP);

            /* Pick a trap */
            pick_and_set_trap(y, x, 0);

            /* Disturb */
            disturb(FALSE, FALSE);
        }
        else if (feat_ff3_match(oldfeat, FF3_PICK_DOOR))
        {

            /*Mark the lore*/
            if (player_can_see_bold(y, x)) f_l_ptr->f_l_flags3 |= (FF3_PICK_DOOR);

            /* Pick a trap */
            place_closed_door(y, x);

            /* Disturb */
            disturb(FALSE, FALSE);
        }
        else
        {
            return;
        }

        /* This is a generated dungeon*/
        if (character_dungeon)
        {
            /* Notice */
            note_spot(y, x);

            /* Redraw */
            light_spot(y, x);
        }
    }

    /* Other stuff */
    else
    {
        /* Set the new feature */
        cave_set_feat(y, x, newfeat);
    }
}


/*
 * Change the "feature_idx" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(int y, int x, u16b feat)
{
    int i, j;

    /* Old Feature */
    u16b old_feat = dungeon_info[y][x].feature_idx;
    feature_type *f_ptr = &f_info[old_feat];

    /* New Feature */
    feature_type *f2_ptr = &f_info[feat];

    /* We aren't changing anything */
    if (f2_ptr == f_ptr) return;

    /* Take damage if this is the player square*/
    if (character_dungeon)
    {
        if ((p_ptr->py == y) && (p_ptr->px == x))
        {

            /* Take any accumulated damage from terrain */
            process_player_terrain_damage();
        }
    }     

    /* Change the feature */
    cave_set_feat_aux(y, x, feat);

    /* The new feature is radiating its own light */
    if (_feat_ff2_match(f2_ptr, FF2_GLOW))
    {
        /* Turn on super glow */
        dungeon_info[y][x].cave_info |= (CAVE_HALO);

        /* Spread the super glow through adjacent grids */
        for (i = 0; i < 8; i++)
        {
            /* Get coordinates */
            int yy = y + ddy_ddd[i];
            int xx = x + ddx_ddd[i];

            /* Ignore annoying locations */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Turn on super glow */
            dungeon_info[yy][xx].cave_info |= (CAVE_HALO);
        }

        /* Fully update the visuals */
        if (player_has_los_bold(y, x))
        {
            p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW |	PU_MONSTERS);
        }
    }

    /* The grid has lost its own light */
    else if (_feat_ff2_match(f_ptr, FF2_GLOW))
    {
        /* Turn off super glow */
        dungeon_info[y][x].cave_info &= ~(CAVE_HALO);

        /* Check adjacent grids */
        for (i = 0; i < 8; i++)
        {
            /* Get coordinates */
            int yy = y + ddy_ddd[i];
            int xx = x + ddx_ddd[i];

            /* Ignore annoying locations */
            if (!in_bounds_fully(yy, xx)) continue;

            /* An adjacent grid is glowing */
            if (cave_ff2_match(yy, xx, FF2_GLOW))
            {
                /* Turn on super glow again */
                dungeon_info[y][x].cave_info |= (CAVE_HALO);

                /* Jump to the next adjacent grid */
                continue;
            }

            /* Regular grid. Turn off super glow (temporarily) */
            dungeon_info[yy][xx].cave_info &= ~(CAVE_HALO);

            /* Check adjacent grids */
            for (j = 0; j < 8; j++)
            {
                /* Get coordinates */
                int yyy = yy + ddy_ddd[j];
                int xxx = xx + ddx_ddd[j];

                /* Ignore annoying locations */
                if (!in_bounds_fully(yyy, xxx)) continue;

                /* Found glowing grid */
                if (cave_ff2_match(yyy, xxx, FF2_GLOW))
                {
                    /* Turn on super glow again */
                    dungeon_info[yy][xx].cave_info |= (CAVE_HALO);

                    /* Done */
                    break;
                }
            }

            /* We have to darken the grid, for real */
            if (!(dungeon_info[yy][xx].cave_info & (CAVE_HALO | CAVE_GLOW)) &&
                !cave_ff1_match(yy, xx, FF1_REMEMBER))
            {
                /* Forget */
                dungeon_info[yy][xx].cave_info &= ~(CAVE_MARK);
            }
        }

        /* We have to darken the grid, for real */
        if (!(dungeon_info[y][x].cave_info & (CAVE_HALO | CAVE_GLOW)) &&
            !cave_ff1_match(y, x, FF1_REMEMBER))
        {
            /* Forget */
            dungeon_info[y][x].cave_info &= ~(CAVE_MARK);
        }

        /* Fully update the visuals */
        if (player_has_los_bold(y, x))
        {
            p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW |	PU_MONSTERS);
        }
    }

    /* Handle gold */
    if (character_dungeon && _feat_ff1_match(f_ptr, FF1_HAS_GOLD) &&
        !_feat_ff1_match(f2_ptr, FF1_HAS_GOLD))
    {
        /* Mark the lore if seen*/
        if (player_can_see_bold(y,x))
        {
            /*Mark the feature lore*/
            feature_lore *f_l_ptr = &f_l_list[old_feat];
            f_l_ptr->f_l_flags1 |= (FF1_HAS_GOLD);
        }

        place_gold(y, x);
    }
}


/*
 * Determine the path taken by a projection.  -BEN-, -LM-
 *
 * The projection will always start one grid from the grid (y1,x1), and will
 * travel towards the grid (y2,x2), touching one grid per unit of distance
 * along the major axis, and stopping when it satisfies certain conditions
 * or has traveled the maximum legal distance of "range".  Projections
 * cannot extend further than MAX_SIGHT (at least at present).
 *
 * A projection only considers those grids which contain the line(s) of fire
 * from the start to the end point.  Along any step of the projection path,
 * either one or two grids may be valid options for the next step.  When a
 * projection has a choice of grids, it chooses that which offers the least
 * resistance.  Given a choice of clear grids, projections prefer to move
 * orthogonally.
 *
 * Also, projections to or from the character must stay within the pre-
 * calculated field of fire ("cave_info & (CAVE_FIRE)").  This is a hack.
 * XXX XXX
 *
 * The path grids are saved into the grid array "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid (y1,x1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * We modify y2 and x2 if they are too far away, or (for PROJECT_PASS only)
 * if the projection threatens to leave the dungeon.
 *
 * The "flg" flags can be used to modify the behavior of this function:
 *    PROJECT_STOP:  projection stops when it cannot bypass a monster.
 *    PROJECT_CHCK:  projection notes when it cannot bypass a monster.
 *    PROJECT_THRU:  projection extends past destination grid
 *    PROJECT_PASS:  projection passes through walls
 *
 * This function returns the number of grids (if any) in the path.  This
 * may be zero if no grids are legal except for the starting one.
 */
int project_path(u16b *path_g, u16b *path_gx, int range,
        int y1, int x1, int *y2, int *x2, u32b flg)
{
    int i, j;
    int dy, dx;
    int num, dist, octant;
    int grids = 0;
    bool line_fire;
    bool full_stop = FALSE;

    int y_a, x_a, y_b, x_b;
    int y = 0;
    int x = 0;

    int path_n;

    /* Start with all lines of sight unobstructed */
    u32b bits0 = VINFO_BITS_0;
    u32b bits1 = VINFO_BITS_1;
    u32b bits2 = VINFO_BITS_2;
    u32b bits3 = VINFO_BITS_3;

    int slope_fire1 = -1, slope_fire2 = -1;

    /* Projections are either vertical or horizontal */
    bool vertical;

    /* Assume our target is in-bounds (we can use ordinary grid math) */
    bool y2_neg = FALSE, y2_large = FALSE;
    bool x2_neg = FALSE, x2_large = FALSE;

    /* Optionally require grids to be strictly in line of fire */
    bool require_strict_lof = FALSE;

    /* Count of grids in LOF */
    u16b tmp_grids[160];

    /* Line of fire slope(s) to each legal grid */
    byte tmp_slope_fire1[160];
    byte tmp_slope_fire2[160];

    /* Count of grids in projection path */
    int step;

    /* Remember whether and how a grid is blocked */
    int blockage[2];
    int blockage_tmp;

    /* Assume no monsters in way */
    bool monster_in_way = FALSE;

    /* Initial grid */
    u16b g0 = GRID(y1, x1);

    u16b g;

    /* Pointer to vinfo data */
    vinfo_type *p;

    /* Assume no path */
    path_n = 0;

    /* Handle projections of zero length */
    if ((range <= 0) || ((*y2 == y1) && (*x2 == x1))) return (0);

    /* The character is the source or target of the projection */
    if ((( y1 == p_ptr->py) && ( x1 == p_ptr->px)) ||
        ((*y2 == p_ptr->py) && (*x2 == p_ptr->px)))
    {
        /* Require strict LOF */
        require_strict_lof = TRUE;
    }

    /* Get position change (signed) */
    dy = *y2 - y1;
    dx = *x2 - x1;

    /* Get distance from start to finish */
    dist = distance(y1, x1, *y2, *x2);

    /* Rescale Large Distances */
    if (dist > MAX_SIGHT)
    {
        /* Always watch your (+/-) when doing rounded integer math. */
        int round_y = (dy < 0 ? -(dist / 2) : (dist / 2));
        int round_x = (dx < 0 ? -(dist / 2) : (dist / 2));

        /* Rescale the endpoints */
        dy = ((dy * (MAX_SIGHT - 1)) + round_y) / dist;
        dx = ((dx * (MAX_SIGHT - 1)) + round_x) / dist;
        *y2 = y1 + dy;
        *x2 = x1 + dx;
    }

    /* Get the correct octant */
    if (dy < 0)
    {
        /* Up and to the left */
        if (dx < 0)
        {
            /* More upwards than to the left - octant 4 */
            if (ABS(dy) > ABS(dx)) octant = 5;

            /* At least as much left as upwards - octant 3 */
            else                   octant = 4;
        }
        else
        {
            if (ABS(dy) > ABS(dx)) octant = 6;
            else                   octant = 7;
        }
    }
    else
    {
        if (dx < 0)
        {
            if (ABS(dy) > ABS(dx)) octant = 2;
            else                   octant = 3;
        }
        else
        {
            if (ABS(dy) > ABS(dx)) octant = 1;
            else                   octant = 0;
        }
    }

    /* Determine whether the major axis is vertical or horizontal */
    if ((octant == 5) || (octant == 6) || (octant == 2) || (octant == 1))
    {
        vertical = TRUE;
    }
    else
    {
        vertical = FALSE;
    }

    /* Is our target out-of-bounds (y or x < 0 or > 255)? */
    if      (*y2 <   0) y2_neg   = TRUE;
    else if (*y2 > 255) y2_large = TRUE;

    if      (*x2 <   0) x2_neg   = TRUE;
    else if (*x2 > 255) x2_large = TRUE;


    /* Scan the octant, find the grid corresponding to the end point */
    for (j = 1; j < VINFO_MAX_GRIDS; j++)
    {
        int vy, vx;

        /* Point to this vinfo record */
        p = &vinfo[j];

        /* Extract grid value */
        g = g0 + p->grid[octant];

        /* Get axis coordinates */
        vy = GRID_Y(g);
        vx = GRID_X(g);

        /* Translate out-of-bounds y-values */
        if ((y2_neg) && (vy >= 128)) vy -= 256;
        else if ((y2_large) && (vy < 128)) vy += 256;

        /* Translate out-of-bounds x-values */
        if ((x2_neg) && (vx >= 128))
        {
            vx -= 256;
            vy++;
        }
        else if ((x2_large) && (vx < 128))
        {
            vx += 256;
            vy--;
        }

        /* Require that grid be correct */
        if ((vy != *y2) || (vx != *x2)) continue;

        /* Store slopes of fire */
        slope_fire1 = p->slope_fire_index1;
        slope_fire2 = p->slope_fire_index2;

        break;
    }

    /* Note failure XXX XXX */
    if (slope_fire1 == -1) return (0);

    /* Scan the octant, collect all grids having the correct line of fire */
    for (j = 1; j < VINFO_MAX_GRIDS; j++)
    {
        line_fire = FALSE;

        /* Point to this vinfo record */
        p = &vinfo[j];

        /* See if any lines of sight pass through this grid */
        if (!((bits0 & (p->bits_0)) ||
              (bits1 & (p->bits_1)) ||
              (bits2 & (p->bits_2)) ||
              (bits3 & (p->bits_3))))
        {
            continue;
        }

        /*
         * Extract grid value.  Use pointer shifting to get the
         * correct grid offset for this octant.
         */
        g = g0 + *((u16b*)(((byte*)(p)) + (octant * 2)));

        y = GRID_Y(g);
        x = GRID_X(g);

        /* Must be legal (this is important) */
        if (!in_bounds_fully(y, x)) continue;

        /* Check for first possible slope of fire */
        i = slope_fire1;

        /* Check slope(s) of fire */
        while (TRUE)
        {
            switch (i / 32)
            {
                case 3:
                {
                    if (bits3 & (1L << (i % 32)))
                    {
                        if (p->bits_3 & (1L << (i % 32))) line_fire = TRUE;
                    }
                    break;
                }
                case 2:
                {
                    if (bits2 & (1L << (i % 32)))
                    {
                        if (p->bits_2 & (1L << (i % 32))) line_fire = TRUE;
                    }
                    break;
                }
                case 1:
                {
                    if (bits1 & (1L << (i % 32)))
                    {
                        if (p->bits_1 & (1L << (i % 32))) line_fire = TRUE;
                    }
                    break;
                }
                case 0:
                {
                    if (bits0 & (1L << (i % 32)))
                    {
                        if (p->bits_0 & (1L << (i % 32))) line_fire = TRUE;
                    }
                    break;
                }
            }

            /* We're done if no second SOF exists, or when we've checked it */
            if (i == slope_fire2) break;

            /* Check second possible slope of fire */
            i = slope_fire2;
        }

        /* This grid contains at least one of the slopes of fire */
        if (line_fire)
        {
            /* Store grid value (always) */
            tmp_grids[grids] = g;

            /* Store the LOF slope(s) to this grid */
            tmp_slope_fire1[grids] = p->slope_fire_index1;
            tmp_slope_fire2[grids] = p->slope_fire_index2;

            /* Go to next grid */
            grids++;
        }

        /*
         * Check for all terrain that won't support projection.  They can be in
         * a projection path, but the path usually cannot pass through them.
         */
        if (!(flg & (PROJECT_PASS)) && !cave_project_bold(y, x))
        {
            /* Clear any lines of sight passing through this grid */
            bits0 &= ~(p->bits_0);
            bits1 &= ~(p->bits_1);
            bits2 &= ~(p->bits_2);
            bits3 &= ~(p->bits_3);
        }
    }

    /* Scan the grids along the slope(s) of fire */
    for (step = 0, j = 0; j < grids;)
    {
        /* Get the coordinates of this grid */
        y_a = GRID_Y(tmp_grids[j]);
        x_a = GRID_X(tmp_grids[j]);

        /* Get the coordinates of the next grid, if legal */
        if (j < grids - 1)
        {
            y_b = GRID_Y(tmp_grids[j+1]);
            x_b = GRID_X(tmp_grids[j+1]);
        }
        else
        {
            y_b = -1;
            x_b = -1;
        }

        /*
         * We always have at least one legal grid, and may have two.  Allow
         * the second grid if its position differs only along the minor axis.
         */
        if (vertical ? y_a == y_b : x_a == x_b) num = 2;
        else                                    num = 1;

        /* We choose the diagonal grid if it is closer to the true path */
        if (num == 2)
        {
            int d_slope_fire_a =
                MIN(ABS(slope_fire1 - tmp_slope_fire1[j]),
                    ABS(slope_fire2 - tmp_slope_fire2[j]));

            int d_slope_fire_b =
                MIN(ABS(slope_fire1 - tmp_slope_fire1[j+1]),
                    ABS(slope_fire2 - tmp_slope_fire2[j+1]));

            /* Swap the grids  XXX */
            if (d_slope_fire_b < d_slope_fire_a)
            {
                int y_c = y_b;
                int x_c = x_b;
                u16b tmp_grids_swap = tmp_grids[j];

                y_b = y_a;
                x_b = x_a;
                y_a = y_c;
                x_a = x_c;
                tmp_grids[j] = tmp_grids[j+1];
                tmp_grids[j+1] = tmp_grids_swap;
            }
        }

        /* Scan one or both grids */
        for (i = 0; i < num; i++)
        {
            /*Assume no blockage*/
            blockage[i] = PATH_G_FULL;

            /* Get the coordinates of this grid */
            y = (i == 0 ? y_a : y_b);
            x = (i == 0 ? x_a : x_b);

            /* Check projection range */
            if ((step > range - 2) &&
                (distance(y1, x1, y, x) >= range))
            {
                /* End of projection */
                full_stop = TRUE;
            }

            /* Usually stop at destination grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if ((y == *y2) && (x == *x2))
                {
                    /* End of projection */
                    full_stop = TRUE;
                }
            }

            /* Usually stop at wall grids */
            if (!(flg & (PROJECT_PASS)))
            {
                if (!cave_project_bold(y, x)) blockage[i] = PATH_G_WALL;
            }

            /* If we don't stop at wall grids, we explicitly check legality */
            else if (!in_bounds_fully(y, x))
            {
                /* End of projection */
                break;
            }

            /* Hidden monsters cannot stop projections */
            if ((dungeon_info[y][x].effect_idx > 0) && (mon_list[dungeon_info[y][x].monster_idx].mflag & (MFLAG_HIDE)))
            {
                /* Blank test */
            }

            /* Try to avoid occupied grids */
            else if ((dungeon_info[y][x].monster_idx != 0) && (blockage[i] < PATH_G_WALL))
            {
                if      (flg & (PROJECT_STOP)) blockage[i] = PATH_G_WALL;
                else if (flg & (PROJECT_CHCK)) blockage[i] = PATH_G_BLCK;
            }

            /* Usual case:  we are requiring strict LOF */
            if (require_strict_lof)
            {
                /* This grid does not qualify; it will be skipped */
                if (!dungeon_info[y][x].projectable())
                {
                    blockage[i] += PATH_G_NONE;
                }
            }
        }/* Grids have been scanned */

        /* Prefer the 1st grid */
        if ((num == 1) || (blockage[0] <= blockage[1]))
        {
            path_gx[step] = blockage_tmp = blockage[0];
            path_g[step] = tmp_grids[j];
        }

        /* Accept second if necessary */
        else
        {
            path_gx[step] = blockage_tmp = blockage[1];
            path_g[step] = tmp_grids[j+1];
        }

        /* Get the grid coordinates */
        y = GRID_Y(path_g[step]);
        x = GRID_X(path_g[step]);

        /* Take a step */
        step++;

        /* Handle end of projection */
        if (full_stop) break;

        /* The projection ends at walls (usually) */
        if (blockage_tmp == PATH_G_WALL) break;

        /* Blockage of 1 means a creature bars the path */
        if (blockage_tmp == PATH_G_BLCK)
        {
            /* If not the start or endpoint, note the blockage */
            if ((y != *y2) || (x != *x2))

                monster_in_way = TRUE;
        }

        /* Grid should be skipped, but a creature blocks it */
        if ((blockage_tmp >= PATH_G_NONE) && (dungeon_info[y][x].monster_idx != 0))
        {
            /* Any sort of bolt projection stops immediately */
            if (!(flg & (PROJECT_BEAM | PROJECT_BOOM | PROJECT_CHCK)))
                break;

            /* If we don't stop, we note the potential blockage */
            else monster_in_way = TRUE;
        }

        /*
         * Hack -- If we require orthogonal movement, but are moving
         * diagonally, we have to plot an extra grid.  XXX XXX
         */
        if ((flg & (PROJECT_ORTH)) && (step >= 2))
        {
            /* Get grids for this projection step and the last */
            y_a = GRID_Y(path_g[step-1]);
            x_a = GRID_X(path_g[step-1]);

            y_b = GRID_Y(path_g[step-2]);
            x_b = GRID_X(path_g[step-2]);

            /* The grids differ along both axis -- we moved diagonally */
            if ((y_a != y_b) && (x_a != x_b))
            {
                /* Get locations for the connecting grids */
                int y_c = y_a;
                int x_c = x_b;
                int y_d = y_b;
                int x_d = x_a;

                /* Back up one step */
                step--;

                /* Assume both grids are available */
                blockage[0] = 0;
                blockage[1] = 0;

                /* Hack -- Check legality */
                if (!in_bounds_fully(y_c, x_c)) blockage[0] = PATH_G_WALL;
                if (!in_bounds_fully(y_d, x_d)) blockage[1] = PATH_G_WALL;

                /* Usually stop at wall grids */
                if (!(flg & (PROJECT_PASS)))
                {
                    if (!cave_project_bold(y_c, x_c)) blockage[0] = PATH_G_WALL;
                    if (!cave_project_bold(y_d, x_d)) blockage[1] = PATH_G_WALL;
                }

                /* Try to avoid non-initial monsters/players */
                if (dungeon_info[y_c][x_c].monster_idx != 0)
                {
                    if      (flg & (PROJECT_STOP)) blockage[0] = PATH_G_WALL;
                    else if (flg & (PROJECT_CHCK)) blockage[0] = PATH_G_BLCK;
                }
                if (dungeon_info[y_d][x_d].monster_idx != 0)
                {
                    if      (flg & (PROJECT_STOP)) blockage[1] = PATH_G_WALL;
                    else if (flg & (PROJECT_CHCK)) blockage[1] = PATH_G_BLCK;
                }

                /* Both grids are blocked -- we have to stop now */
                if ((blockage[0] >= 2) && (blockage[1] >= PATH_G_WALL)) break;

                /* Accept the first grid if possible, the second if necessary */
                if (blockage[0] <= blockage[1]) path_g[step++] = GRID(y_c, x_c);
                else 							path_g[step++] = GRID(y_d, x_d);

                /* Re-insert the original grid, take an extra step */
                path_g[step++] = GRID(y_a, x_a);

                /* Increase range to accommodate this extra step */
                range++;
            }
        }

        /* Advance to the next unexamined LOF grid */
        j += num;
    }

    /* Accept last grid as the new endpoint */
    *y2 = GRID_Y(path_g[step - 1]);
    *x2 = GRID_X(path_g[step - 1]);

    /* Remember number of grids in projection path */
    path_n = step;

    /* Return number of grids in path, negative if blocked */
    if (monster_in_way) return (-path_n);
    else return (path_n);
}


/*
 * Determine if a spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination.
 *
 * This function is used to determine if the player can (easily) target
 * a given grid, if a monster can target the player, and if a clear shot
 * exists from monster to player.
 *
 * In cases where the character is either the source or the target of the
 * projection (which is most of the time), we now can save much time by
 * forbidding all projections that do not remain in line of fire.  -LM-
 *
 * Those projections that survive this check use the "project_path()"
 * function to determine the projection path and find any obstacles.
 * What qualifies as an obstacle depends on the projection flags.
 *
 * Note that no grid is ever "projectable()" from itself.
 */
byte projectable(int y1, int x1, int y2, int x2, u32b flg)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int y, x;

    int path_n;
    u16b path_g[PATH_SIZE];
    u16b path_gx[PATH_SIZE];

    int old_y2 = y2;
    int old_x2 = x2;

    /* We do not have permission to pass through walls */
    if (!(flg & (PROJECT_WALL | PROJECT_PASS)))
    {
        /* The character is the source of the projection */
        if ((y1 == py) && (x1 == px))
        {
            /* Require that destination be in line of fire */
            if (!dungeon_info[y2][x2].projectable()) return (PROJECT_NO);
        }

        /* The character is the target of the projection */
        else if ((y2 == py) && (x2 == px))
        {
            /* Require that source be in line of fire */
            if (!dungeon_info[y1][x1].projectable()) return (PROJECT_NO);
        }
    }

    /* Check the projection path */
    path_n = project_path(path_g, path_gx, MAX_RANGE, y1, x1, &y2, &x2, flg);

    /* No grid is ever projectable from itself */
    if (!path_n) return (PROJECT_NO);

    /* Final grid  */
    y = GRID_Y(path_g[path_n - 1]);
    x = GRID_X(path_g[path_n - 1]);

    /* May not end in an unrequested grid, unless PROJECT_THRU */
    if (!(flg & (PROJECT_THRU)))
    {
        if ((y != old_y2) || (x != old_x2)) return (PROJECT_NO);
    }

    /* Usually, cannot pass through walls */
    if (!(flg & (PROJECT_PASS)))
    {
        /* May not end in a non-projectable grid*/
        if (!cave_project_bold(y, x) && !cave_passable_bold(y, x)) return (PROJECT_NO);
    }

    /* Promise a clear bolt shot if we have verified that there is one */
    if ((flg & (PROJECT_STOP)) || (flg & (PROJECT_CHCK)))
    {
        /* Positive value for projectable mean no obstacle was found. */
        if (path_n > 0) return (PROJECT_CLEAR);
    }

    /* Assume projectable, but make no promises about clear shots */
    return (PROJECT_NOT_CLEAR);
}


/*
 * Standard "find me a location" function
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location.
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 *
 * Currently the "m" parameter is unused.
 */
void scatter(int *yp, int *xp, int y, int x, int d, int m)
{
    int nx, ny;


    /* Unused parameter */
    (void)m;

    /* Pick a location */
    while (TRUE)
    {
        /* Pick a new location */
        ny = rand_spread(y, d);
        nx = rand_spread(x, d);

        /* Ignore annoying locations */
        if (!in_bounds_fully(ny, nx)) continue;

        /* Ignore "excessively distant" locations */
        if ((d > 1) && (distance(y, x, ny, nx) > d)) continue;

        /* Require "line of sight" */
        if (los(y, x, ny, nx)) break;
    }

    /* Save the location */
    (*yp) = ny;
    (*xp) = nx;
}


/*
 * Track a new monster
 */
void health_track(int m_idx)
{
    /* Track a new guy */
    p_ptr->health_who = m_idx;

    /* Redraw (later) */
    p_ptr->redraw |= (PR_SIDEBAR_MON);
}


/*
 * Hack -- track the given object kind
 */
void track_object(int item)
{
    p_ptr->object_kind_idx = 0;
    if (p_ptr->object_idx == item) return;
    p_ptr->object_idx = item;
    p_ptr->redraw |= (PR_WIN_OBJ_RECALL);
}

void track_object_kind(int k_idx)
{
    p_ptr->object_idx = 0;

    if (p_ptr->object_kind_idx == k_idx) return;
    p_ptr->object_kind_idx = k_idx;
    p_ptr->redraw |= (PR_WIN_OBJ_RECALL);
}


/*
 * Hack -- track the given monster race
 */
void monster_race_track(int r_idx)
{
    if (p_ptr->monster_race_idx == r_idx) return;

    /* Save this monster ID */
    p_ptr->monster_race_idx = r_idx;

    /* Window stuff */
    p_ptr->redraw |= (PR_WIN_MON_RECALL);
}


/*
 * Hack -- track the given object kind
 */
void feature_kind_track(int f_idx)
{
    if (p_ptr->feature_kind_idx == f_idx) return;

    /* Save this object ID */
    p_ptr->feature_kind_idx = f_idx;

    /* Window stuff */
    p_ptr->redraw |= (PR_WIN_FEAT_RECALL);
}


/*
 * Something has happened to disturb the player.
 *
 * The first arg indicates a major disturbance, which affects search.
 *
 * The second arg wipes the current hotkey.
 *
 * All disturbance cancels repeated commands, hotkeys, resting, and running.
 */
void disturb(bool stop_search, bool wipe_hotkey)
{
    /* Cancel Resting */
    if (p_ptr->is_resting())
    {
        /* Redraw the state (later) */
        p_ptr->redraw |= (PR_STATUSBAR | PR_SIDEBAR_PL);
    }

    /* Cancel running */
    if (p_ptr->is_running())
    {
        /* Check for new panel if appropriate */
        if (center_player) ui_center(p_ptr->py, p_ptr->px);

        /* Calculate torch radius */
        p_ptr->update |= (PU_TORCH);

    }

    /* Cancel repeated commands */
    p_ptr->player_command_wipe();

    //Clear any running hotkey
    if (wipe_hotkey)
    {
            if (running_hotkey.hotkey_steps.size()) running_hotkey.clear_hotkey_steps();
    }

    /* Cancel searching if requested */
    if (stop_search && p_ptr->searching)
    {
        /* Cancel */
        p_ptr->searching = FALSE;

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Redraw the state */
        p_ptr->redraw |= (PR_SIDEBAR_PL | PR_STATUSBAR);
    }

    light_spot(p_ptr->py, p_ptr->px);
}

