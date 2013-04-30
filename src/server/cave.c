/*
 * File: cave.c
 * Purpose: Lighting and update functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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
#include "../common/tvalsval.h"
#include "cmds.h"
#include "monster/mon-spell.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "s-spells.h"
#include "squelch.h"
#include "wilderness.h"


static int vinfo_grids;
static int vinfo_slopes;
static u32b vinfo_bits_3;
static u32b vinfo_bits_2;
static u32b vinfo_bits_1;
static u32b vinfo_bits_0;


/*
 * Approximate Distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
int distance(int y1, int x1, int y2, int x2)
{
    /* Find the absolute y/x distance components */
    int ay = abs(y2 - y1);
    int ax = abs(x2 - x1);

    /* Approximate the distance */
    return (ay > ax)? (ay + (ax >> 1)): (ax + (ay >> 1));
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
bool los(int depth, int y1, int x1, int y2, int x2)
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

    /* Directly South/North */
    if (!dx)
    {
        /* South -- check for walls */
        if (dy > 0)
        {
            for (ty = y1 + 1; ty < y2; ty++)
            {
                if (!cave_floor_bold(depth, ty, x1)) return (FALSE);
            }
        }

        /* North -- check for walls */
        else
        {
            for (ty = y1 - 1; ty > y2; ty--)
            {
                if (!cave_floor_bold(depth, ty, x1)) return (FALSE);
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
                if (!cave_floor_bold(depth, y1, tx)) return (FALSE);
            }
        }

        /* West -- check for walls */
        else
        {
            for (tx = x1 - 1; tx > x2; tx--)
            {
                if (!cave_floor_bold(depth, y1, tx)) return (FALSE);
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
            if (cave_floor_bold(depth, y1 + sy, x1)) return (TRUE);
        }
    }

    /* Horizontal "knights" */
    else if (ay == 1)
    {
        if (ax == 2)
        {
            if (cave_floor_bold(depth, y1, x1 + sx)) return (TRUE);
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
            ty = y1;

        /* Note (below) the case (qy == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (x2 - tx)
        {
            if (!cave_floor_bold(depth, ty, tx)) return (FALSE);

            qy += m;

            if (qy < f2)
                tx += sx;
            else if (qy > f2)
            {
                ty += sy;
                if (!cave_floor_bold(depth, ty, tx)) return (FALSE);
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
            tx = x1;

        /* Note (below) the case (qx == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (y2 - ty)
        {
            if (!cave_floor_bold(depth, ty, tx)) return (FALSE);

            qx += m;

            if (qx < f2)
                ty += sy;
            else if (qx > f2)
            {
                tx += sx;
                if (!cave_floor_bold(depth, ty, tx)) return (FALSE);
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
bool no_light(struct player *p)
{
    return (!player_can_see_bold(p, p->py, p->px));
}


/*
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool cave_valid_bold(int depth, int y, int x)
{
    object_type *o_ptr;

    /* Forbid perma-grids */
    if (cave_perma_bold(depth, y, x)) return (FALSE);

    /* Check objects */
    for (o_ptr = get_first_object(depth, y, x); o_ptr; o_ptr = get_next_object(o_ptr))
    {
        /* Forbid artifact grids */
        if (o_ptr->artifact) return (FALSE);
    }

    /* Accept */
    return (TRUE);
}


/*
 * Hack -- Hallucinatory monster
 */
static void hallucinatory_monster(struct player *p, byte *a, char *c)
{
    while (1)
    {
        /* Select a random monster */
        int i = randint0(z_info->r_max);
        monster_race *r_ptr = &r_info[i];

        /* Skip non-entries */
        if (!r_ptr->name) continue;

        /* Retrieve attr/char */
        *a = p->r_attr[i];
        *c = p->r_char[i];

        return;
    }
}


/*
 * Hack -- Hallucinatory object
 */
static void hallucinatory_object(struct player *p, byte *a, char *c)
{
    while (1)
    {
        /* Select a random object */
        int i = randint0(z_info->k_max - 1) + 1;
        object_kind *k_ptr = &k_info[i];

        /* Skip non-entries */
        if (!k_ptr->name) continue;

        /* Retrieve attr/char (Hack - without flavors) */
        *a = p->k_attr[i];
        *c = p->k_char[i];

        /* Hack - Skip empty entries */
        if ((*a == 0) || (*c == 0)) continue;

        return;
    }
}


/*
 * Return the correct "color" of another player
 */
static byte player_color(struct player *p)
{
    /* Ghosts are black */
    if (p->ghost) return TERM_L_DARK;

    /* Cloaked rogues */
    if (p->timed[TMD_MIMIC]) return player_id2class(p->tim_mimic_what)->attr;

    /* Color is based off of class */
    return p->clazz->attr;
}


typedef struct
{
    int flag;
    byte first_color;
    byte second_color;
} breath_attr_struct;


/*
 * Table of breath colors.  Must match listings in a single set of
 * monster spell flags.
 *
 * The value "255" is special.  Monsters with that kind of breath
 * may be any color.
 */
static breath_attr_struct breath_to_attr[] =
{
    {RSF_BR_ACID, TERM_SLATE, TERM_L_DARK},
    {RSF_BR_ELEC, TERM_BLUE, TERM_L_BLUE},
    {RSF_BR_FIRE, TERM_RED, TERM_L_RED},
    {RSF_BR_COLD, TERM_WHITE, TERM_L_WHITE},
    {RSF_BR_POIS, TERM_GREEN, TERM_L_GREEN},
    {RSF_BR_NETH, TERM_L_GREEN, TERM_GREEN},
    {RSF_BR_LIGHT, TERM_ORANGE, TERM_YELLOW},
    {RSF_BR_DARK, TERM_L_DARK, TERM_SLATE},
    {RSF_BR_SOUN, TERM_YELLOW, TERM_L_UMBER},
    {RSF_BR_CHAO, 255, 255},
    {RSF_BR_DISE, TERM_VIOLET, TERM_L_BLUE},
    {RSF_BR_NEXU, TERM_VIOLET, TERM_L_RED},
    {RSF_BR_TIME, TERM_L_BLUE, TERM_BLUE},
    {RSF_BR_INER, TERM_L_WHITE, TERM_SLATE},
    {RSF_BR_GRAV, TERM_L_WHITE, TERM_SLATE},
    {RSF_BR_SHAR, TERM_UMBER, TERM_L_UMBER},
    {RSF_BR_PLAS, TERM_ORANGE, TERM_RED},
    {RSF_BR_WALL, TERM_UMBER, TERM_L_UMBER},
    {RSF_BR_MANA, TERM_L_DARK, TERM_SLATE},
    {RSF_BR_WATE, TERM_BLUE, TERM_SLATE}
};


/*
 * Multi-hued monsters shimmer according to their breaths.
 *
 * If a monster has only one kind of breath, it uses both colors 
 * associated with that breath.  Otherwise, it just uses the first 
 * color for any of its breaths.
 *
 * If a monster does not breath anything, it can be any color.
 */
static byte multi_hued_attr_breath(monster_race *r_ptr)
{
    bitflag mon_breath[RSF_SIZE];
    size_t i;
    int j, breaths = 0, stored_colors = 0;
    byte allowed_attrs[15];
    byte second_color = 0;

    /* Monsters with no ranged attacks can be any color */
    if (!r_ptr->freq_spell) return randint1(BASIC_COLORS - 1);

    /* Hack -- Require correct "breath attack" */
    rsf_copy(mon_breath, r_ptr->spell_flags);
    set_spells(mon_breath, RST_BREATH);

    /* Check breaths */
    for (i = 0; i < N_ELEMENTS(breath_to_attr); i++)
    {
        bool stored = FALSE;
        byte first_color;

        /* Don't have that breath */
        if (!rsf_has(mon_breath, breath_to_attr[i].flag)) continue;

        /* Get the first color of this breath */
        first_color = breath_to_attr[i].first_color;

        /* Monster can be of any color */
        if (first_color == 255) return randint1(BASIC_COLORS - 1);

        /* Increment the number of breaths */
        breaths++;

        /* Monsters with lots of breaths may be any color. */
        if (breaths == 6) return randint1(BASIC_COLORS - 1);

        /* Check if already stored */
        for (j = 0; j < stored_colors; j++)
        {
            /* Already stored */
            if (allowed_attrs[j] == first_color) stored = TRUE;
        }

        /* If not, store the first color */
        if (!stored)
        {
            allowed_attrs[stored_colors] = first_color;
            stored_colors++;
        }

        /* 
         * Remember (but do not immediately store) the second color 
         * of the first breath.
         */
        if (breaths == 1) second_color = breath_to_attr[i].second_color;
    }

    /* Monsters with no breaths may be of any color. */
    if (breaths == 0) return randint1(BASIC_COLORS - 1);

    /* If monster has one breath, store the second color too. */
    if (breaths == 1)
    {
        allowed_attrs[stored_colors] = second_color;
        stored_colors++;
    }

    /* Pick a color at random */
    return (allowed_attrs[randint0(stored_colors)]);
}


/*
 * Table of flickering colors.
 */
static byte color_flicker[MAX_COLORS][3] =
{
    {TERM_DARK, TERM_L_DARK, TERM_L_RED},
    {TERM_WHITE, TERM_L_WHITE, TERM_L_BLUE},
    {TERM_SLATE, TERM_WHITE, TERM_L_DARK},
    {TERM_ORANGE, TERM_YELLOW, TERM_L_RED},
    {TERM_RED, TERM_L_RED, TERM_L_PINK},
    {TERM_GREEN, TERM_L_GREEN, TERM_L_TEAL},
    {TERM_BLUE, TERM_L_BLUE, TERM_SLATE},
    {TERM_UMBER, TERM_L_UMBER, TERM_MUSTARD},
    {TERM_L_DARK, TERM_SLATE, TERM_L_VIOLET},
    {TERM_L_WHITE, TERM_WHITE, TERM_SLATE},
    {TERM_L_PURPLE, TERM_PURPLE, TERM_L_VIOLET},
    {TERM_YELLOW, TERM_L_YELLOW, TERM_MUSTARD},
    {TERM_L_RED, TERM_RED, TERM_L_PINK},
    {TERM_L_GREEN, TERM_L_TEAL, TERM_GREEN},
    {TERM_L_BLUE, TERM_DEEP_L_BLUE, TERM_BLUE_SLATE},
    {TERM_L_UMBER, TERM_UMBER, TERM_MUD},
    {TERM_PURPLE, TERM_VIOLET, TERM_MAGENTA},
    {TERM_VIOLET, TERM_L_VIOLET, TERM_MAGENTA},
    {TERM_TEAL, TERM_L_TEAL, TERM_L_GREEN},
    {TERM_MUD, TERM_YELLOW, TERM_UMBER},
    {TERM_L_YELLOW, TERM_WHITE, TERM_L_UMBER},
    {TERM_MAGENTA, TERM_L_PINK, TERM_L_RED},
    {TERM_L_TEAL, TERM_L_WHITE, TERM_TEAL},
    {TERM_L_VIOLET, TERM_L_PURPLE, TERM_VIOLET},
    {TERM_L_PINK, TERM_L_RED, TERM_L_WHITE},
    {TERM_MUSTARD, TERM_YELLOW, TERM_UMBER},
    {TERM_BLUE_SLATE, TERM_BLUE, TERM_SLATE},
    {TERM_DEEP_L_BLUE, TERM_L_BLUE, TERM_BLUE},
};


/*
 * Multi-hued monsters shimmer according to their flickering colors.
 */
static byte get_flicker(byte a)
{
    return color_flicker[a][randint0(3)];
}


/*
 * Return the correct attr/char pair for any player
 */
static void player_pict(struct player *p, struct player *q, byte *a, char *c)
{
    int life, timefactor;
    bool show_as_number = TRUE;

    /* Get the "player" attr */
    if (q == p)
    {
        /* Handle himself */
        *a = p->r_attr[0];
    }
    else
    {
        /* Handle other */
        *a = player_color(q);
        if (p->use_graphics)
            *a = player_presets[p->use_graphics - 1][q->clazz->cidx][q->race->ridx][q->psex].a;

        /* Hack -- Elementalists */
        if (!p->use_graphics && (*a == TERM_MULTI))
        {
            /* Set default attr */
            *a = TERM_VIOLET;

            /* Shimmer the player */
            if (allow_shimmer(p))
            {
                switch (randint0(5))
                {
                    case 0: *a = TERM_WHITE; break;
                    case 1: *a = TERM_RED; break;
                    case 2: *a = TERM_GREEN; break;
                    case 3: *a = TERM_BLUE; break;
                    case 4: *a = TERM_SLATE; break;
                }
            }
        }
    }

    /* Get the "player" char */
    if (q == p)
    {
        /* Handle himself */
        *c = p->r_char[0];
    }
    else
    {
        /* Handle other */
        *c = p->r_char[0];
        if (p->use_graphics)
            *c = player_presets[p->use_graphics - 1][q->clazz->cidx][q->race->ridx][q->psex].c;
    }

    /* Handle ghosts in graphical mode */
    if (p->use_graphics && q->ghost)
    {
        s16b r_idx = get_r_idx("Ghost");

        *a = p->r_attr[r_idx];
        *c = p->r_char[r_idx];
    }

    /* Handle polymorphed players: use monster attr/char */
    if (q->r_idx)
    {
        monster_race *r_ptr = &r_info[q->r_idx];

        /* Desired attr */
        *a = p->r_attr[q->r_idx];

        /* Desired char */
        *c = p->r_char[q->r_idx];

        /* Multi-hued monster */
        if (!p->use_graphics && monster_shimmer(r_ptr) && allow_shimmer(p))
        {
            if (rf_has(r_ptr->flags, RF_ATTR_MULTI))
                *a = multi_hued_attr_breath(r_ptr);
            else if (rf_has(r_ptr->flags, RF_ATTR_FLICKER))
                *a = get_flicker(*a);
        }
    }

    /* Handle mimic form: use object attr/char (don't shimmer) */
    if (q->k_idx > 0)
    {
        object_kind *kind = &k_info[q->k_idx];

        /* Normal attr and char */
        *a = object_kind_attr(p, kind);
        *c = object_kind_char(p, kind);

        /* Set default attr */
        if (!p->use_graphics && (*a == TERM_MULTI)) *a = TERM_VIOLET;
    }

    /* Give interesting visual effects in non-graphical mode for the player */
    if (!p->use_graphics && (q == p))
    {
        /* Give a visual effect to some spells */
        if (p->timed[TMD_MANASHIELD] || p->timed[TMD_INVULN] || p->timed[TMD_DEADLY])
            *a = TERM_VIOLET;

        /* Handle hp_changes_color option */
        else if (OPT_P(p, hp_changes_color))
        {
            *a = TERM_WHITE;
            life = ((p->chp * 95) / (p->mhp * 10));
            if (life < 9) *a = TERM_YELLOW;
            if (life < 7) *a = TERM_ORANGE;
            if (life < 5) *a = TERM_L_RED;
            if (life < 3) *a = TERM_RED;
            show_as_number = FALSE;
        }

        /* If we are in a slow time bubble, give a visual warning */
        timefactor = base_time_factor(p, 0);
        if (timefactor < NORMAL_TIME)
        {
            /* Initialize bubble color */
            if (p->bubble_colour == TERM_DARK) p->bubble_colour = *a;

            /* Switch between normal and bubble color every (10 + slowdown) turns */
            if (ht_diff(&turn, &p->bubble_change) > (10 + (NORMAL_TIME - timefactor)))
            {
                /* Normal -> bubble color */
                if (p->bubble_colour == *a)
                {
                    if (*a == TERM_WHITE) *a = TERM_VIOLET;
                    else *a = TERM_WHITE;
                }

                /* Set bubble turn/color values */
                /* This also handles the case bubble -> normal color */
                ht_copy(&p->bubble_change, &turn);
                p->bubble_colour = *a;
            }

            /* Use bubble color */
            *a = p->bubble_colour;
        }
    }

    /* Hack -- Highlight party leader! */
    else
    {
        if (!p->use_graphics && is_party_owner(p, q) && OPT_P(p, highlight_leader) && magik(50))
        {
            if (*a == TERM_YELLOW) *a = TERM_L_DARK;
            else *a = TERM_YELLOW;
        }
    }

    /* Display the player as a number if hp/mana is low (60% or less) */
    if (show_as_number)
    {
        /* Sorcerors protected by disruption shield get % of mana */
        if (q->timed[TMD_MANASHIELD])
            life = (q->csp * 95) / (q->msp * 10);

        /* Other players get % of hps */
        else
            life = (q->chp * 95) / (q->mhp * 10);

        /* Paranoia */
        if (life < 0) life = 0;

        /* Display a number if hp/mana is 60% or less */
        if (life < 7)
        {
            /* Desired char */
            *c = I2D(life);

            /* Use presets in gfx mode */
            if (p->use_graphics)
            {
                *a = player_numbers[p->use_graphics - 1][life].a;
                *c = player_numbers[p->use_graphics - 1][life].c;
            }
        }
    }
}


/*
 * Translate text colours.
 *
 * This translates a color based on the attribute. We use this to set terrain to
 * be lighter or darker, make metallic monsters shimmer, highlight text under the
 * mouse, and reduce the colours on mono colour or 16 colour terms to the correct
 * colour space.
 *
 * TODO: Honour the attribute for the term (full color, mono, 16 color) but ensure
 * that e.g. the lighter version of yellow becomes white in a 16 color term, but
 * light yellow in a full colour term.
 */
byte get_color(byte a, int attr, int n)
{
    /* Accept any graphical attr (high bit set) */
    if (a & (0x80)) return (a);

    /* TODO: Honour the attribute for the term (full color, mono, 16 color) */
    if (!attr) return (a);

    /* Translate the color N times */
    while (n > 0)
    {
        a = color_table[a].color_translate[attr];
        n--;
    }

    /* Return the modified color */
    return (a);
}


/*
 * Checks if a square is at the (inner) edge of a trap detect area
 */
bool dtrap_edge(struct player *p, int y, int x)
{
    /* Only on random levels */
    if (!random_level(p->depth)) return FALSE;

    /* Check if the square is a dtrap in the first place */
    if (!(p->cave->info[y][x] & CAVE_DTRAP)) return FALSE;

    /* Check for non-dtrap adjacent grids */
    if (in_bounds_fully(y + 1, x) && !(p->cave->info[y + 1][x] & CAVE_DTRAP)) return TRUE;
    if (in_bounds_fully(y, x + 1) && !(p->cave->info[y][x + 1] & CAVE_DTRAP)) return TRUE;
    if (in_bounds_fully(y - 1, x) && !(p->cave->info[y - 1][x] & CAVE_DTRAP)) return TRUE;
    if (in_bounds_fully(y, x - 1) && !(p->cave->info[y][x - 1] & CAVE_DTRAP)) return TRUE;

    return FALSE;
}


static bool feat_is_known_trap(int feat)
{
    return ((feat >= FEAT_TRAP_HEAD) && (feat <= FEAT_TRAP_TAIL));
}


static bool feat_is_treasure(int feat)
{
    return ((feat == FEAT_MAGMA_K) || (feat == FEAT_QUARTZ_K));
}


/*
 * Apply text lighting effects
 */
static void grid_get_attr(struct player *p, grid_data *g, byte *a)
{
    /* Save the high-bit, since it's used for attr inversion. */
    byte a0 = (*a & 0x80);

    /* We will never tint traps or treasure */
    if (feat_is_known_trap(g->f_idx) || feat_is_treasure(g->f_idx)) return;

    /* Tint the trap detection edge green */
    if (g->trapborder)
    {
        *a = (a0 | (g->in_view? TERM_L_GREEN: TERM_GREEN));
        return;
    }

    /* If the square isn't white we won't apply any other lighting effects. */
    if (((*a & 0x7F) != TERM_WHITE) && (g->f_idx != FEAT_GRASS)) return;

    /* If it's a floor tile then we'll tint based on lighting. */
    if (cave_floor_basic(g->f_idx))
    {
        switch (g->lighting)
        {
            case FEAT_LIGHTING_BRIGHT:
                *a = (a0 | (OPT_P(p, view_orange_light)? TERM_ORANGE: TERM_YELLOW));
                break;
            case FEAT_LIGHTING_DARK:
                if ((*a & 0x7F) == TERM_WHITE) *a = (a0 | TERM_L_DARK);
                break;
        }
        return;
    }

    /* If it's another kind of tile, only tint when unlit. */
    if (((*a & 0x7F) == TERM_WHITE) && (g->f_idx != FEAT_SHOP_HEAD + STORE_WEAPON) &&
        (g->lighting == FEAT_LIGHTING_DARK))
    {
        *a = (a0 | TERM_SLATE);
    }
}


/*
 * This function takes a pointer to a grid info struct describing the
 * contents of a grid location (as obtained through the function map_info)
 * and fills in the character and attr pairs for display.
 *
 * ap and cp are filled with the attr/char pair for the monster, object or
 * floor tile that is at the "top" of the grid (monsters covering objects, 
 * which cover floor, assuming all are present).
 *
 * tap and tcp are filled with the attr/char pair for the floor, regardless
 * of what is on it.  This can be used by graphical displays with
 * transparency to place an object onto a floor tile, is desired.
 *
 * Any lighting effects are also applied to these pairs, clear monsters allow
 * the underlying colour or feature to show through (ATTR_CLEAR and
 * CHAR_CLEAR), multi-hued colour-changing (ATTR_MULTI) is applied, and so on.
 *
 * NOTES:
 * This is called pretty frequently, whenever a grid on the map display
 * needs updating, so don't overcomplicate it.
 *
 * The "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "pile"
 * attr/char, and "feature zero" being used for the "darkness" attr/char.
 *
 * TODO:
 * The transformations for tile colors, or brightness for the 16x16
 * tiles should be handled differently.  One possibility would be to
 * extend feature_type with attr/char definitions for the different states.
 * This will probably be done outside of the current text->graphics mappings
 * though.
 */
void grid_data_as_text(struct player *p, bool server, grid_data *g, byte *ap, char *cp,
    byte *tap, char *tcp)
{
    byte a;
    char c;
    char (*f_char_ptr)[FEAT_LIGHTING_MAX];
    byte (*f_attr_ptr)[FEAT_LIGHTING_MAX];
    char *r_char_ptr;
    byte *r_attr_ptr;
    bool use_graphics;

    /* Should we override the clients attr/char settings? */
    if (server)
    {
        /* We have initialised a global array of server char/attr elsewhere for speed */
        f_attr_ptr = f_attr_s;
        f_char_ptr = f_char_s;
        r_attr_ptr = r_attr_s;
        r_char_ptr = r_char_s;
    }
    else
    {
        f_attr_ptr = p->f_attr;
        f_char_ptr = p->f_char;
        r_attr_ptr = p->r_attr;
        r_char_ptr = p->r_char;
    }

    /* Normal attr and char */
    a = f_attr_ptr[g->f_idx][g->lighting];
    c = f_char_ptr[g->f_idx][g->lighting];

    /* Hack -- Use basic lighting for unmapped tiles */
    use_graphics = ((p->use_graphics != GRAPHICS_NONE) && (a & 0x80));

    /* Apply text lighting effects */
    if (!use_graphics) grid_get_attr(p, g, &a);

    /* Check for trap detection boundaries */
    else if (g->trapborder && (g->f_idx == FEAT_FLOOR) && (g->m_idx || g->first_o_idx))
    {
        /*
         * If there is an object or monster here, and this is a plain floor
         * display the border here rather than an overlay below
         */
        a = f_attr_ptr[FEAT_FLOOR_TRAP][g->lighting];
        c = f_char_ptr[FEAT_FLOOR_TRAP][g->lighting];
    }

    /* Save the terrain info for the transparency effects */
    (*tap) = a;
    (*tcp) = c;

    /* If there's an object, deal with that. */
    if (g->unseen_money)
    {
        /* $$$ gets an orange star */
        a = object_kind_attr(p, &k_info[7]);
        c = object_kind_char(p, &k_info[7]);
    }
    if (g->unseen_object)
    {
        /* Everything else gets a red star */
        a = object_kind_attr(p, &k_info[6]);
        c = object_kind_char(p, &k_info[6]);
    }
    else if (g->first_o_idx)
    {
        if (g->hallucinate)
        {
            /* Just pick a random object to display. */
            hallucinatory_object(p, &a, &c);
        }
        else if (g->multiple_objects)
        {
            /* Get the "pile" feature instead */
            a = p->k_attr[0];
            c = p->k_char[0];
        }
        else
        {
            object_type *o_ptr = object_byid(g->first_o_idx);

            /* Normal attr and char */
            a = object_attr(p, o_ptr);
            c = object_char(p, o_ptr);

            /* Multi-hued object */
            if (object_shimmer(o_ptr))
            {
                /* Set default attr */
                if (a == TERM_MULTI) a = TERM_VIOLET;

                /* Shimmer the object */
                if (allow_shimmer(p)) a = randint1(BASIC_COLORS - 1);
            }
        }
    }

    /* If there's a monster */
    if (g->m_idx > 0)
    {
        monster_type *m_ptr = (g->hallucinate? NULL: cave_monster(cave_get(p->depth), g->m_idx));

        if (g->hallucinate)
        {
            /* Just pick a random monster to display. */
            hallucinatory_monster(p, &a, &c);
        }
        else if (!is_mimicking(m_ptr))
        {
            monster_race *r_ptr = &r_info[m_ptr->r_idx];
            byte da;
            char dc;

            /* Desired attr & char */
            da = r_attr_ptr[m_ptr->r_idx];
            dc = r_char_ptr[m_ptr->r_idx];

            /* Special attr/char codes */
            if (da & 0x80)
            {
                /* Use attr */
                a = da;

                /* Use char */
                c = dc;
            }

            /* Turn uniques purple if desired (violet, actually) */
            else if (OPT_P(p, purple_uniques) && rf_has(r_ptr->flags, RF_UNIQUE))
            {
                /* Use violet attr */
                a = TERM_VIOLET;

                /* Use char */
                c = dc;
            }

            /* Multi-hued monster */
            else if (monster_shimmer(r_ptr))
            {
                /* Use attr */
                a = da;

                /* Use char */
                c = dc;

                /* Shimmer the monster */
                if (allow_shimmer(p))
                {
                    /* Multi-hued attr */
                    if (rf_has(r_ptr->flags, RF_ATTR_MULTI))
                        a = multi_hued_attr_breath(r_ptr);
                    else if (rf_has(r_ptr->flags, RF_ATTR_FLICKER))
                        a = get_flicker(da);

                    /* Redraw monster list if needed */
                    if (m_ptr->attr != a) p->redraw |= PR_MONLIST;
                }
            }

            /* Normal monster (not "clear" in any way) */
            else if (!flags_test(r_ptr->flags, RF_SIZE, RF_ATTR_CLEAR, RF_CHAR_CLEAR, FLAG_END))
            {
                /* Use attr */
                a = da;

                /* Use char */
                c = dc;
            }

            /* Hack -- Bizarre grid under monster */
            else if (a & 0x80)
            {
                /* Use attr */
                a = da;

                /* Use char */
                c = dc;
            }

            /* Normal char, Clear attr, monster */
            else if (!rf_has(r_ptr->flags, RF_CHAR_CLEAR))
            {
                /* Normal char */
                c = dc;
            }

            /* Normal attr, Clear char, monster */
            else if (!rf_has(r_ptr->flags, RF_ATTR_CLEAR))
            {
                /* Normal attr */
                a = da;
            }

            /* Hack -- Random mimics */
            if (m_ptr->mimicked_k_idx)
            {
                c = p->k_char[m_ptr->mimicked_k_idx];
                if (p->use_graphics) a = p->k_attr[m_ptr->mimicked_k_idx];
            }

            /* Store the drawing attr so we can use it elsewhere */
            m_ptr->attr = a;
        }
    }

    /* Handle "player" */
    else if (g->is_player)
    {
        player_pict(p, p, &a, &c);
        Send_player_pos(p);
    }

    /* Handle other players */
    else if (g->m_idx < 0)
    {
        if (g->hallucinate)
        {
            s16b k_idx = player_get(0 - g->m_idx)->k_idx;

            /* Player mimics an object -- Just pick a random object to display. */
            if (k_idx > 0)
                hallucinatory_object(p, &a, &c);

            /* Player mimics a feature -- Display him normally. */
            else if (k_idx < 0)
                player_pict(p, player_get(0 - g->m_idx), &a, &c);

            /* Just pick a random monster to display. */
            else
                hallucinatory_monster(p, &a, &c);
        }
        else
            player_pict(p, player_get(0 - g->m_idx), &a, &c);
    }

    /* Check for trap detection boundaries */
    else if (g->trapborder && g->f_idx && !g->first_o_idx && use_graphics)
    {
        /* No overlay is used, so we can use the trap border overlay */
        a = f_attr_ptr[FEAT_OVER_TRAP][g->lighting];
        c = f_char_ptr[FEAT_OVER_TRAP][g->lighting];
    }

    /* Result */
    (*ap) = a;
    (*cp) = c;
}


/*
 * This function takes a grid location (x, y) and extracts information the
 * player is allowed to know about it, filling in the grid_data structure
 * passed in 'g'.
 *
 * The information filled in is as follows:
 *  - g->f_idx is filled in with the terrain's feature type, or FEAT_NONE
 *    if the player doesn't know anything about the grid.  The function
 *    makes use of the "mimic" field in terrain in order to allow one
 *    feature to look like another (hiding secret doors, invisible traps,
 *    etc).  This will return the terrain type the player "Knows" about,
 *    not necessarily the real terrain.
 *  - g->m_idx is set to the monster index, or 0 if there is none (or the
 *    player doesn't know it).
 *  - g->first_o_idx is set to the index of the first object in a grid
 *    that the player knows about, or zero for no object in the grid.
 *  - g->muliple_objects is TRUE if there is more than one object in the
 *    grid that the player knows and cares about (to facilitate any special
 *    floor stack symbol that might be used).
 *  - g->in_view is TRUE if the player can currently see the grid - this can
 *    be used to indicate field-of-view.
 *  - g->lighting is set to indicate the lighting level for the grid:
 *    FEAT_LIGHTING_DARK for unlit grids, FEAT_LIGHTING_BRIGHT for those lit by the player's
 *    light source, and FEAT_LIGHTING_LIT for inherently light grids (lit rooms, etc).
 *    Note that lighting is always FEAT_LIGHTING_LIT for known "interesting" grids
 *    like walls.
 *  - g->is_player is TRUE if the player is on the given grid.
 *  - g->hallucinate is TRUE if the player is hallucinating something "strange"
 *    for this grid - this should pick a random monster to show if the m_idx
 *    is non-zero, and a random object if first_o_idx is non-zero.
 *
 * NOTES:
 * This is called pretty frequently, whenever a grid on the map display
 * needs updating, so don't overcomplicate it.
 *
 * Terrain is remembered separately from objects and monsters, so can be
 * shown even when the player can't "see" it.  This leads to things like
 * doors out of the player's view still change from closed to open and so on.
 */
void map_info(struct player *p, unsigned y, unsigned x, grid_data *g)
{
    object_type *o_ptr;
    byte info;
    s16b this_o_idx, next_o_idx = 0;
    player_type *q_ptr;

    my_assert(x < DUNGEON_WID);
    my_assert(y < DUNGEON_HGT);

    info = p->cave->info[y][x];

    /* Default "clear" values, others will be set later where appropriate. */
    g->first_o_idx = 0;
    g->multiple_objects = FALSE;
    g->lighting = FEAT_LIGHTING_DARK;
    g->unseen_object = FALSE;
    g->unseen_money = FALSE;

    q_ptr = player_get(0 - cave_get(p->depth)->m_idx[y][x]);

    g->f_idx = cave_get(p->depth)->feat[y][x];
    if (f_info[g->f_idx].mimic) g->f_idx = f_info[g->f_idx].mimic;
    g->in_view = ((info & CAVE_SEEN)? TRUE: FALSE);
    g->is_player = ((q_ptr == p)? TRUE: FALSE);
    g->m_idx = ((g->is_player)? 0: cave_get(p->depth)->m_idx[y][x]);
    g->hallucinate = (p->timed[TMD_IMAGE]? TRUE: FALSE);
    g->trapborder = ((info & CAVE_DEDGE)? TRUE: FALSE);

    if (g->in_view)
    {
        g->lighting = FEAT_LIGHTING_LIT;

        if (!(cave_get(p->depth)->info[y][x] & CAVE_GLOW) && OPT_P(p, view_yellow_light))
            g->lighting = FEAT_LIGHTING_BRIGHT;
    }
    else if (!is_memorized(p, info))
        g->f_idx = FEAT_NONE;

    /* Objects */
    for (this_o_idx = cave_get(p->depth)->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        if (p->obj_marked[this_o_idx] == MARK_AWARE)
        {
            /* Distinguish between unseen money and objects */
            if (o_ptr->tval == TV_GOLD)
                g->unseen_money = TRUE;
            else
                g->unseen_object = TRUE;
        }
        else if (object_seen(p, this_o_idx) && !squelch_item_ok(p, o_ptr))
        {
            if (g->first_o_idx == 0)
                g->first_o_idx = this_o_idx;
            else
            {
                g->multiple_objects = TRUE;
                break;
            }
        }
    }

    /* Monsters */
    if (g->m_idx > 0)
    {
        /* If the monster isn't "visible", make sure we don't list it.*/
        if (!p->mon_vis[g->m_idx]) g->m_idx = 0;
    }

    /* Players */
    else if (g->m_idx < 0)
    {
        /* If the player isn't "visible", make sure we don't list it.*/
        if (!p->play_vis[0 - g->m_idx]) g->m_idx = 0;
    }

    /* Rare random hallucination on non-outer walls */
    if (g->hallucinate && !g->m_idx && !g->first_o_idx)
    {
        if (one_in_(128) && (g->f_idx != FEAT_PERM_SOLID)) g->m_idx = 1;
        else if (one_in_(128) && (g->f_idx != FEAT_PERM_SOLID)) g->first_o_idx = 1;
        else g->hallucinate = FALSE;
    }

    my_assert(g->f_idx <= FEAT_HOME_TAIL);
    if (!g->hallucinate) {my_assert((int)g->m_idx < cave_get(p->depth)->mon_max);}
    if (!g->hallucinate) {my_assert(g->first_o_idx < o_max);}
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
static void cave_note_spot_aux(struct player *p, struct cave *c, int y, int x)
{
    object_type *o_ptr;
    s16b this_o_idx, next_o_idx = 0;

    /* Require "seen" flag */
    if (!(p->cave->info[y][x] & CAVE_SEEN)) return;

    /* Hack -- memorize objects */
    for (this_o_idx = c->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Memorize */
        p->obj_marked[this_o_idx] = MARK_SEEN;

        /* Redraw */
        p->redraw |= PR_ITEMLIST;
    }

    /* Memorize this grid */
    p->cave->info[y][x] |= CAVE_MARK;
}


void cave_note_spot(struct cave *c, int y, int x)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* If he's not here, skip him */
        if (p_ptr->depth != c->depth) continue;

        /* Memorize interesting viewable object/features in the given grid for that player */
        cave_note_spot_aux(p_ptr, c, y, x);
    }
}


/*
 * Redraw (on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 */
void cave_light_spot_aux(struct player *p, struct cave *cv, int y, int x)
{
    int dispx, dispy;

    /* Paranoia (to avoid division by zero) */
    if (!p->tile_hgt || !p->tile_wid) return;

    /* Redraw if on screen */
    if (panel_contains(p, y, x))
    {
        byte a, ta;
        char c, tc;
        grid_data g;

        /* Examine the grid */
        map_info(p, y, x, &g);
        grid_data_as_text(p, FALSE, &g, &a, &c, &ta, &tc);

        dispx = x - p->offset_x;
        dispy = y - p->offset_y + 1;

        /* Only draw if different than buffered */
        if ((p->scr_info[dispy][dispx].c != c) || (p->scr_info[dispy][dispx].a != a) ||
            (p->trn_info[dispy][dispx].a != ta) || (p->trn_info[dispy][dispx].c != tc) ||
            ((x == p->px) && (y == p->py)))
        {
            /* Modify internal buffer */
            p->scr_info[dispy][dispx].c = c;
            p->scr_info[dispy][dispx].a = a;
            p->trn_info[dispy][dispx].c = tc;
            p->trn_info[dispy][dispx].a = ta;

            /* Tell client to redraw this grid */
            Send_char(p, dispx, dispy, a, c, ta, tc);
        }
    }
}


void cave_light_spot(struct cave *c, int y, int x)
{
    int i;

    /* Paranoia */
    if (!c) return;

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* If he's not here, skip him */
        if (p_ptr->depth != c->depth) continue;

        /* Actually light that spot for that player */
        cave_light_spot_aux(p_ptr, c, y, x);
    }
}


/*
 * Redraw (on the screen) the current map panel
 *
 * Note the inline use of "cave_light_spot()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void prt_map(struct player *p)
{
    byte a;
    char c;
    byte ta;
    char tc;
    grid_data g;
    int y, x;
    int vy, vx;
    int ty, tx;
    int screen_hgt, screen_wid;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    /* Assume screen */
    ty = p->offset_y + screen_hgt;
    tx = p->offset_x + screen_wid;

    /* Dump the map */
    for (y = p->offset_y, vy = 1; y < ty; vy++, y++)
    {
        /* First clear the old stuff */
        for (x = 0; x < DUNGEON_WID; x++)
        {
            p->scr_info[vy][x].c = 0;
            p->scr_info[vy][x].a = 0;
            p->trn_info[vy][x].c = 0;
            p->trn_info[vy][x].a = 0;
        }

        /* Scan the columns of row "y" */
        for (x = p->offset_x, vx = 0; x < tx; vx++, x++)
        {
            /* Determine what is there */
            map_info(p, y, x, &g);
            grid_data_as_text(p, FALSE, &g, &a, &c, &ta, &tc);

            p->scr_info[vy][vx].c = c;
            p->scr_info[vy][vx].a = a;
            p->trn_info[vy][vx].c = tc;
            p->trn_info[vy][vx].a = ta;
        }

        /* Send that line of info */
        Send_line_info(p, vy);
    }

    /* Reset the line counter */
    Send_line_info(p, -1);
}


static bool feat_is_rock(int feat)
{
    switch (feat)
    {
        case FEAT_WALL_EXTRA:
        case FEAT_WALL_INNER:
        case FEAT_WALL_OUTER:
        case FEAT_WALL_SOLID: return TRUE;
        default: return FALSE;
    }
}


static int feat_mimic(int feat)
{
    if (f_info[feat].mimic) return f_info[feat].mimic;
    return feat;
}


static int feat_pushable(int feat1, int feat2)
{
    if (feat_is_rock(feat_mimic(feat1)) && (feat_mimic(feat2) == FEAT_FLOOR))
        return feat1;
    if (feat_is_rock(feat_mimic(feat2)) && (feat_mimic(feat1) == FEAT_FLOOR))
        return feat2;
    return FEAT_NONE;
}


static int feat_dir(int yg, int xg, int yf, int xf)
{
    if (yg > yf)
    {
        if (xg > xf) return 3;
        if (xg < xf) return 1;
        return 2;
    }
    if (yg < yf)
    {
        if (xg > xf) return 9;
        if (xg < xf) return 7;
        return 8;
    }
    if (xg > xf) return 6;
    if (xg < xf) return 4;
    return 5;
}


/*
 * Display a "small-scale" map of the dungeon in the active Term.
 *
 * Note the use of a specialized "priority" function to allow this function
 * to work with any graphic attr/char mappings, and the attempts to optimize
 * this function where possible.
 */
void display_map(struct player *p, bool subwindow)
{
    int py = p->py;
    int px = p->px;
    int map_hgt, map_wid;
    int dungeon_hgt, dungeon_wid;
    int row, col;
    int x, y;
    grid_data g;
    byte a, ta;
    char c, tc;
    byte tp;
    byte mp[DUNGEON_HGT][DUNGEON_WID];
    byte ma[DUNGEON_HGT][DUNGEON_WID];
    char mc[DUNGEON_HGT][DUNGEON_WID];
    byte mf[DUNGEON_HGT][DUNGEON_WID];
    int f_idx, push_f_idx;

    /* Desired map size */
    map_hgt = p->max_hgt - ROW_MAP - 1;
    map_wid = p->screen_cols;

    /* Hack -- Classic mini-map */
    if (subwindow)
    {
        map_hgt = NORMAL_HGT;
        map_wid = NORMAL_WID;
    }

    dungeon_hgt = DUNGEON_HGT;
    dungeon_wid = DUNGEON_WID;

    /* Prevent accidents */
    if (map_hgt > dungeon_hgt) map_hgt = dungeon_hgt;
    if (map_wid > dungeon_wid) map_wid = dungeon_wid;

    /* Prevent accidents */
    if ((map_wid < 1) || (map_hgt < 1)) return;

    /* Clear chars, attributes, priorities, features */
    for (y = 0; y < map_hgt; ++y)
    {
        for (x = 0; x < map_wid; ++x)
        {
            /* Nothing here */
            ma[y][x] = TERM_WHITE;
            mc[y][x] = ' ';
            mf[y][x] = FEAT_NONE;

            /* No priority */
            mp[y][x] = 0;
        }
    }

    /* Analyze the actual map */
    for (y = 0; y < dungeon_hgt; y++)
    {
        for (x = 0; x < dungeon_wid; x++)
        {
            row = (y * map_hgt / dungeon_hgt);
            col = (x * map_wid / dungeon_wid);

            /* Get the feature at that map location */
            map_info(p, y, x, &g);
            f_idx = g.f_idx;

            /*
             * Hack --  When a floor tile takes priority over a dungeon wall, push that wall tile
             * to the closest dark tile
             */
            push_f_idx = feat_pushable(f_idx, mf[row][col]);
            if (push_f_idx != FEAT_NONE)
            {
                int yy, xx;
                int dir = ((push_f_idx == f_idx)? feat_dir(y, x, row, col): feat_dir(row, col, y, x));

                /* Push wall tile */
                yy = row + ddy[dir];
                xx = col + ddx[dir];
                if ((yy >= 0) && (yy < map_hgt) && (xx >= 0) && (xx < map_wid) &&
                    (mf[yy][xx] == FEAT_NONE))
                {
                    ma[yy][xx] = p->f_attr[push_f_idx][FEAT_LIGHTING_LIT];
                    mc[yy][xx] = p->f_char[push_f_idx][FEAT_LIGHTING_LIT];
                    mf[yy][xx] = push_f_idx;
                    mp[yy][xx] = f_info[push_f_idx].priority;
                }
            }

            /* Get the priority of that feature */
            tp = f_info[f_idx].priority;

            /* Save "best" */
            if (mp[row][col] < tp)
            {
                /* Hack -- Make every grid on the map lit */
                g.lighting = FEAT_LIGHTING_LIT;
                grid_data_as_text(p, FALSE, &g, &a, &c, &ta, &tc);

                /* Save the char */
                mc[row][col] = tc;

                /* Save the attr */
                ma[row][col] = ta;

                /* Save priority */
                mp[row][col] = tp;

                /* Save feature */
                mf[row][col] = f_idx;
            }
        }
    }

    /* Make sure the player is visible in main window */
    if (!subwindow)
    {
        /* Player location */
        row = (py * map_hgt / dungeon_hgt);
        col = (px * map_wid / dungeon_wid);

        player_pict(p, p, &ta, &tc);

        /* Set the "player" attr */
        ma[row][col] = ta;

        /* Set the "player" char */
        mc[row][col] = tc;
    }

    /* Activate mini-map window */
    if (subwindow) Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_MAP);

    /* Display each map line in order */
    for (y = 0; y < map_hgt; ++y)
    {
        /* Display the line */
        for (x = 0; x < map_wid; ++x)
        {
            ta = ma[y][x];
            tc = mc[y][x];

            /* Add the character */
            p->scr_info[y][x].c = tc;
            p->scr_info[y][x].a = ta;
        }

        /* Send that line of info */
        Send_mini_map(p, y, map_wid);

        /* Throw some nonsense into the "screen_info" so it gets cleared */
        for (x = 0; x < map_wid; x++)
        {
            p->scr_info[y][x].c = 0;
            p->scr_info[y][x].a = 255;
            p->trn_info[y][x].c = 0;
            p->trn_info[y][x].a = 0;
        }
    }

    /* Reset the line counter */
    Send_mini_map(p, -1, 0);

    /* Restore main window */
    if (subwindow) Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
}


static int get_wilderness_type(int Ind, int wild_idx)
{
    player_type *p_ptr = player_get(Ind);

    /* If off the map, set to unknown type */
    if (wild_idx < 0 - MAX_WILD) return -1;

    /* Hack -- the town is always known */
    if (!wild_idx) return WILD_TOWN;

    /* If the player hasnt been here, dont show him the terrain */
    if (!wild_is_explored(p_ptr, 0 - wild_idx)) return -1;

    /* Determine wilderness type */
    return determine_wilderness_type(wild_idx);
}


static void wild_display_map(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int map_hgt, map_wid;
    int dungeon_hgt, dungeon_wid;
    int col;
    int x, y;
    grid_data g;
    byte a, ta;
    char c, tc;
    byte ma[DUNGEON_HGT][DUNGEON_WID];
    char mc[DUNGEON_HGT][DUNGEON_WID];
    char buf[NORMAL_WID];

    /* Desired map size */
    map_hgt = p_ptr->max_hgt - ROW_MAP - 1;
    map_wid = p_ptr->screen_cols;

    dungeon_hgt = DUNGEON_HGT;
    dungeon_wid = DUNGEON_WID;

    /* Prevent accidents */
    if (map_hgt > dungeon_hgt) map_hgt = dungeon_hgt;
    if (map_wid > dungeon_wid) map_wid = dungeon_wid;

    /* Prevent accidents */
    if ((map_wid < 1) || (map_hgt < 1)) return;

    /* Clear the chars and attributes */
    for (y = 0; y < map_hgt; ++y)
    {
        for (x = 0; x < map_wid; ++x)
        {
            /* Nothing here */
            ma[y][x] = TERM_WHITE;
            mc[y][x] = ' ';
        }
    }

    /* Analyze the actual map */
    for (y = 0; y < map_hgt; y++)
    {
        for (x = 0; x < map_wid; x++)
        {
            int world_y, world_x, wild_idx, type;

            /* Location */
            world_y = p_ptr->world_y + map_hgt / 2 - y;
            world_x = p_ptr->world_x - map_wid / 2 + x;
            wild_idx = world_index(world_x, world_y);

            /* Get wilderness type */
            type = get_wilderness_type(Ind, wild_idx);

            /* Initialize our grid_data structure */
            WIPE(&g, grid_data);
            g.lighting = FEAT_LIGHTING_LIT;
            g.in_view = TRUE;

            /* Set meta terrain feature */
            switch (type)
            {
                case WILD_SHORE: g.f_idx = FEAT_WATER; break;
                case WILD_GRASS: g.f_idx = FEAT_GRASS; break;
                case WILD_WOOD: g.f_idx = FEAT_TREE; break;
                case WILD_SWAMP: g.f_idx = FEAT_SWAMP; break;
                case WILD_WASTE: g.f_idx = FEAT_DIRT; break;
                case WILD_MOUNTAIN: g.f_idx = FEAT_MOUNTAIN; break;
                case WILD_VOLCANO: g.f_idx = FEAT_LAVA; break;
                case WILD_TOWN: g.f_idx = FEAT_TOWN; break;
            }

            /* Extract the current attr/char at that map location */
            grid_data_as_text(p_ptr, FALSE, &g, &a, &c, &ta, &tc);

            /* Put the player in the center */
            if ((y == map_hgt / 2) && (x == map_wid / 2))
                player_pict(p_ptr, p_ptr, &ta, &tc);

            /* Save the char */
            mc[y][x] = tc;

            /* Save the attr */
            ma[y][x] = ta;
        }
    }

    /* Prepare bottom string */
    buf[0] = '\0';
    my_strcat(buf, " ", sizeof(buf));
    wild_cat_depth(p_ptr->depth, buf, sizeof(buf));
    my_strcat(buf, " ", sizeof(buf));

    /* Print string at the bottom */
    col = map_wid - strlen(buf);
    for (x = col; x < map_wid; x++) mc[y][x] = buf[x - col];

    /* Display each map line in order */
    for (y = 0; y < map_hgt; ++y)
    {
        /* Display the line */
        for (x = 0; x < map_wid; ++x)
        {
            /* Add the character */
            p_ptr->scr_info[y][x].c = mc[y][x];
            p_ptr->scr_info[y][x].a = ma[y][x];
        }

        /* Send that line of info */
        Send_mini_map(p_ptr, y, map_wid);

        /* Throw some nonsense into the "screen_info" so it gets cleared */
        for (x = 0; x < map_wid; x++)
        {
            p_ptr->scr_info[y][x].c = 0;
            p_ptr->scr_info[y][x].a = 255;
            p_ptr->trn_info[y][x].c = 0;
            p_ptr->trn_info[y][x].a = 0;
        }
    }

    /* Reset the line counter */
    Send_mini_map(p_ptr, -1, 0);
}


/*
 * Display a "small-scale" map of the dungeon.
 *
 * Note that the "player" is always displayed on the map.
 */
void do_cmd_view_map(int Ind)
{
    display_map(player_get(Ind), FALSE);
}


/*
 * Display a "small-scale" map of the wilderness.
 *
 * Note that the "player" is always displayed on the map.
 */
void do_cmd_wild_map(int Ind)
{
    wild_display_map(Ind);
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
 * One of the major bottlenecks in ancient versions of Angband was in the
 * calculation of "line of sight" from the player to various grids, such
 * as those containing monsters, using the relatively expensive "los()"
 * function.  This was such a nasty bottleneck that a lot of silly things
 * were done to reduce the dependancy on "line of sight", for example, you
 * could not "see" any grids in a lit room until you actually entered the
 * room, at which point every grid in the room became "illuminated" and
 * all of the grids in the room were "memorized" forever.  Other major
 * bottlenecks involved the determination of whether a grid was lit by the
 * player's torch, and whether a grid blocked the player's line of sight.
 * These bottlenecks led to the development of special new functions to
 * optimize issues involved with "line of sight" and "torch lit grids".
 * These optimizations led to entirely new additions to the game, such as
 * the ability to display the player's entire field of view using different
 * colors than were used for the "memorized" portions of the dungeon, and
 * the ability to memorize dark floor grids, but to indicate by the way in
 * which they are displayed that they are not actually illuminated.  And
 * of course many of them simply made the game itself faster or more fun.
 * Also, over time, the definition of "line of sight" has been relaxed to
 * allow the player to see a wider "field of view", which is slightly more
 * realistic, and only slightly more expensive to maintain.
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
 * Some of the information about the dungeon is used by functions which can
 * constitute the "critical efficiency path" of the game itself, and so the
 * way in which they are stored and accessed has been optimized in order to
 * optimize the game itself.  For example, the "update_view()" function was
 * originally created to speed up the game itself (when the player was not
 * running), but then it took on extra responsibility as the provider of the
 * new "special effects lighting code", and became one of the most important
 * bottlenecks when the player was running.  So many rounds of optimization
 * were performed on both the function itself, and the data structures which
 * it uses, resulting eventually in a function which not only made the game
 * faster than before, but which was responsible for even more calculations
 * (including the determination of which grids are "viewable" by the player,
 * which grids are illuminated by the player's torch, and which grids can be
 * "seen" in some way by the player), as well as for providing the guts of
 * the special effects lighting code, and for the efficient redisplay of any
 * grids whose visual representation may have changed.
 *
 * Several pieces of information about each cave grid are stored in various
 * two dimensional arrays, with one unit of information for each grid in the
 * dungeon.  Some of these arrays have been intentionally expanded by a small
 * factor to make the two dimensional array accesses faster by allowing the
 * use of shifting instead of multiplication.
 *
 * Several pieces of information about each cave grid are stored in the
 * "cave->info" array, which is a special two dimensional array of bytes,
 * one for each cave grid, each containing eight separate "flags" which
 * describe some property of the cave grid.  These flags can be checked and
 * modified extremely quickly, especially when special idioms are used to
 * force the compiler to keep a local register pointing to the base of the
 * array.  Special location offset macros can be used to minimize the number
 * of computations which must be performed at runtime.  Note that using a
 * byte for each flag set may be slightly more efficient than using a larger
 * unit, so if another flag (or two) is needed later, and it must be fast,
 * then the two existing flags which do not have to be fast should be moved
 * out into some other data structure and the new flags should take their
 * place.  This may require a few minor changes in the savefile code.
 *
 * The "CAVE_ROOM" flag is saved in the savefile and is used to determine
 * which grids are part of "rooms", and thus which grids are affected by
 * "illumination" spells.  This flag does not have to be very fast.
 *
 * The "CAVE_ICKY" flag is saved in the savefile and is used to determine
 * which grids are part of "vaults", and thus which grids cannot serve as
 * the destinations of player teleportation.  This flag does not have to
 * be very fast.
 *
 * The "CAVE_NOTELE" flag is saved in the savefile and is used to determine
 * which grids are part of "pits", and thus which grids cannot serve as
 * origin for player/monster teleportation.  This flag does not have to
 * be very fast.
 *
 * The "CAVE_MARK" flag is saved in the savefile and is used to determine
 * which grids have been "memorized" by the player.  This flag is used by
 * the "map_info()" function to determine if a grid should be displayed.
 * This flag is used in a few other places to determine if the player can
 * "know" about a given grid.  This flag must be very fast.
 *
 * The "CAVE_GLOW" flag is saved in the savefile and is used to determine
 * which grids are "permanently illuminated".  This flag is used by the
 * "update_view()" function to help determine which viewable flags may
 * be "seen" by the player.  This flag is used by the "map_info" function
 * to determine if a grid is only lit by the player's torch.  This flag
 * has special semantics for wall grids (see "update_view()").  This flag
 * must be very fast.
 *
 * The "CAVE_WALL" flag is used to determine which grids block the player's
 * line of sight.  This flag is used by the "update_view()" function to
 * determine which grids block line of sight, and to help determine which
 * grids can be "seen" by the player.  This flag must be very fast.
 *
 * The "CAVE_VIEW" flag is used to determine which grids are currently in
 * line of sight of the player.  This flag is set by (and used by) the
 * "update_view()" function.  This flag is used by any code which needs to
 * know if the player can "view" a given grid.  This flag is used by the
 * "map_info()" function for some optional special lighting effects.  The
 * "player_has_los_bold()" macro wraps an abstraction around this flag, but
 * certain code idioms are much more efficient.  This flag is used to check
 * if a modification to a terrain feature might affect the player's field of
 * view.  This flag is used to see if certain monsters are "visible" to the
 * player.  This flag is used to allow any monster in the player's field of
 * view to "sense" the presence of the player.  This flag must be very fast.
 *
 * The "CAVE_SEEN" flag is used to determine which grids are currently in
 * line of sight of the player and also illuminated in some way.  This flag
 * is set by the "update_view()" function, using computations based on the
 * "CAVE_VIEW" and "CAVE_WALL" and "CAVE_GLOW" flags of various grids.  This
 * flag is used by any code which needs to know if the player can "see" a
 * given grid.  This flag is used by the "map_info()" function both to see
 * if a given "boring" grid can be seen by the player, and for some optional
 * special lighting effects.  The "player_can_see_bold()" macro wraps an
 * abstraction around this flag, but certain code idioms are much more
 * efficient.  This flag is used to see if certain monsters are "visible" to
 * the player.  This flag is never set for a grid unless "CAVE_VIEW" is also
 * set for the grid.  Whenever the "CAVE_WALL" or "CAVE_GLOW" flag changes
 * for a grid which has the "CAVE_VIEW" flag set, the "CAVE_SEEN" flag must
 * be recalculated.  The simplest way to do this is to call "forget_view()"
 * and "update_view()" whenever the "CAVE_WALL" or "CAVE_GLOW" flags change
 * for a grid which has "CAVE_VIEW" set.  This flag must be very fast.
 *
 * The "CAVE_TEMP" flag is used for a variety of temporary purposes.  This
 * flag is used to determine if the "CAVE_SEEN" flag for a grid has changed
 * during the "update_view()" function.  This flag is used to "spread" light
 * or darkness through a room.  This flag is used by the "monster flow code".
 * This flag must always be cleared by any code which sets it, often, this
 * can be optimized by the use of the special "temp_g" array.  This flag must
 * be very fast.
 *
 * Note that the "CAVE_MARK" flag is used for many reasons, some of which
 * are strictly for optimization purposes.  The "CAVE_MARK" flag means that
 * even if the player cannot "see" the grid, he "knows" about the terrain in
 * that grid.  This is used to "memorize" grids when they are first "seen" by
 * the player, and to allow certain grids to be "detected" by certain magic.
 *
 * Objects are "memorized" in a different way, using a special "marked" flag
 * on the object itself, which is set when an object is observed or detected.
 * This allows objects to be "memorized" independant of the terrain features.
 *
 * The "update_view()" function is an extremely important function.  It is
 * called only when the player moves, significant terrain changes, or the
 * player's blindness or torch radius changes.  Note that when the player
 * is resting, or performing any repeated actions (like digging, disarming,
 * farming, etc), there is no need to call the "update_view()" function, so
 * even if it was not very efficient, this would really only matter when the
 * player was "running" through the dungeon.  It sets the "CAVE_VIEW" flag
 * on every cave grid in the player's field of view, and maintains an array
 * of all such grids in the global "view_g" array.  It also checks the torch
 * radius of the player, and sets the "CAVE_SEEN" flag for every grid which
 * is in the "field of view" of the player and which is also "illuminated",
 * either by the players torch (if any) or by any permanent light source.
 * It could use and help maintain information about multiple light sources,
 * which would be helpful in a multi-player version of Angband.
 *
 * The "update_view()" function maintains the special "view_g" array, which
 * contains exactly those grids which have the "CAVE_VIEW" flag set.  This
 * array is used by "update_view()" to (only) memorize grids which become
 * newly "seen", and to (only) redraw grids whose "seen" value changes, which
 * allows the use of some interesting (and very efficient) "special lighting
 * effects".  In addition, this array could be used elsewhere to quickly scan
 * through all the grids which are in the player's field of view.
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
 * The old "update_view()" algorithm uses the special "CAVE_EASY" flag as a
 * temporary internal flag to mark those grids which are not only in view,
 * but which are also "easily" in line of sight of the player.  This flag
 * is actually just the "CAVE_SEEN" flag, and the "update_view()" function
 * makes sure to clear it for all old "CAVE_SEEN" grids, and then use it in
 * the algorithm as "CAVE_EASY", and then clear it for all "CAVE_EASY" grids,
 * and then reset it as appropriate for all new "CAVE_SEEN" grids.  This is
 * kind of messy, but it works.  The old algorithm may disappear eventually.
 *
 * The new "update_view()" algorithm uses a faster and more mathematically
 * correct algorithm, assisted by a large machine generated static array, to
 * determine the "CAVE_VIEW" and "CAVE_SEEN" flags simultaneously.  See below.
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
 * Note that the "CAVE_GLOW" flag means that a grid is permanently lit in
 * some way.  However, for the player to "see" the grid, as determined by
 * the "CAVE_SEEN" flag, the player must not be blind, the grid must have
 * the "CAVE_VIEW" flag set, and if the grid is a "wall" grid, and it is
 * not lit by the player's torch, then it must touch a grid which does not
 * have the "CAVE_WALL" flag set, but which does have both the "CAVE_GLOW"
 * and "CAVE_VIEW" flags set.  This last part about wall grids is induced
 * by the semantics of "CAVE_GLOW" as applied to wall grids, and checking
 * the technical requirements can be very expensive, especially since the
 * grid may be touching some "illegal" grids.  Luckily, it is more or less
 * correct to restrict the "touching" grids from the eight "possible" grids
 * to the (at most) three grids which are touching the grid, and which are
 * closer to the player than the grid itself, which eliminates more than
 * half of the work, including all of the potentially "illegal" grids, if
 * at most one of the three grids is a "diagonal" grid.  In addition, in
 * almost every situation, it is possible to ignore the "CAVE_VIEW" flag
 * on these three "touching" grids, for a variety of technical reasons.
 * Finally, note that in most situations, it is only necessary to check
 * a single "touching" grid, in fact, the grid which is strictly closest
 * to the player of all the touching grids, and in fact, it is normally
 * only necessary to check the "CAVE_GLOW" flag of that grid, again, for
 * various technical reasons.  However, one of the situations which does
 * not work with this last reduction is the very common one in which the
 * player approaches an illuminated room from a dark hallway, in which the
 * two wall grids which form the "entrance" to the room would not be marked
 * as "CAVE_SEEN", since of the three "touching" grids nearer to the player
 * than each wall grid, only the farthest of these grids is itself marked
 * "CAVE_GLOW".
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
 *
 *
 * Here is an illustration of the two different "update_view()" algorithms,
 * in which the grids marked "%" are pillars, and the grids marked "?" are
 * not in line of sight of the player.
 *
 *
 *                    Sample situation
 *
 *                  #####################
 *                  ############.%.%.%.%#
 *                  #...@..#####........#
 *                  #............%.%.%.%#
 *                  #......#####........#
 *                  ############........#
 *                  #####################
 *
 *
 *          New Algorithm             Old Algorithm
 *
 *      ########?????????????    ########?????????????
 *      #...@..#?????????????    #...@..#?????????????
 *      #...........?????????    #.........???????????
 *      #......#####.....????    #......####??????????
 *      ########?????????...#    ########?????????????
 *
 *      ########?????????????    ########?????????????
 *      #.@....#?????????????    #.@....#?????????????
 *      #............%???????    #...........?????????
 *      #......#####........?    #......#####?????????
 *      ########??????????..#    ########?????????????
 *
 *      ########?????????????    ########?????%???????
 *      #......#####........#    #......#####..???????
 *      #.@..........%???????    #.@..........%???????
 *      #......#####........#    #......#####..???????
 *      ########?????????????    ########?????????????
 *
 *      ########??????????..#    ########?????????????
 *      #......#####........?    #......#####?????????
 *      #............%???????    #...........?????????
 *      #.@....#?????????????    #.@....#?????????????
 *      ########?????????????    ########?????????????
 *
 *      ########?????????%???    ########?????????????
 *      #......#####.....????    #......####??????????
 *      #...........?????????    #.........???????????
 *      #...@..#?????????????    #...@..#?????????????
 *      ########?????????????    ########?????????????
 */


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
    s16b grid[8];
    u32b bits_3;
    u32b bits_2;
    u32b bits_1;
    u32b bits_0;
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
 *  - Number of grids
 *  - Number of slopes
 *  - Slope values
 *  - Slope range per grid
 */
struct vinfo_hack
{
    int num_slopes;
    long slopes[VINFO_MAX_SLOPES];
    long slopes_min[MAX_SIGHT_LGE + 1][MAX_SIGHT_LGE + 1];
    long slopes_max[MAX_SIGHT_LGE + 1][MAX_SIGHT_LGE + 1];
};


static int cmp_longs(const void *a, const void *b)
{
    long x = *(const long *)a;
    long y = *(const long *)b;

    if (x < y)
        return -1;
    if (x > y)
        return 1;

    return 0;
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
            if (hack->num_slopes >= vinfo_slopes)
                quit_fmt("Too many slopes (%d)!", vinfo_slopes);

            /* Save the slope, and advance */
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
errr vinfo_init(void)
{
    int i, g;
    int y, x;
    long m;
    vinfo_hack *hack;
    int num_grids = 0;
    int queue_head = 0;
    int queue_tail = 0;
    vinfo_type *queue[VINFO_MAX_GRIDS * 2];

    /* Set the variables for the grids, bits and slopes actually used */
    vinfo_grids = (cfg_small_range? 48: VINFO_MAX_GRIDS);
    vinfo_slopes = (cfg_small_range? 36: VINFO_MAX_SLOPES);
    vinfo_bits_3 = (cfg_small_range? 0x00000000: VINFO_BITS_3);
    vinfo_bits_2 = (cfg_small_range? 0x00000000: VINFO_BITS_2);
    vinfo_bits_1 = (cfg_small_range? 0x0000000F: VINFO_BITS_1);
    vinfo_bits_0 = (cfg_small_range? 0xFFFFFFFF: VINFO_BITS_0);

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
            if (num_grids >= vinfo_grids)
                quit_fmt("Too many grids (%d >= %d)!", num_grids, vinfo_grids);

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

    /* Enforce maximal efficiency */
    if (num_grids < vinfo_grids)
        quit_fmt("Too few grids (%d < %d)!", num_grids, vinfo_grids);

    /* Enforce maximal efficiency */
    if (hack->num_slopes < vinfo_slopes)
        quit_fmt("Too few slopes (%d < %d)!", hack->num_slopes, vinfo_slopes);

    sort(hack->slopes, hack->num_slopes, sizeof(*hack->slopes), cmp_longs);

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
        vinfo[e].grid[0] = GRID(+y, +x);
        vinfo[e].grid[1] = GRID(+x, +y);
        vinfo[e].grid[2] = GRID(+x, -y);
        vinfo[e].grid[3] = GRID(+y, -x);
        vinfo[e].grid[4] = GRID(-y, -x);
        vinfo[e].grid[5] = GRID(-x, -y);
        vinfo[e].grid[6] = GRID(-x, +y);
        vinfo[e].grid[7] = GRID(-y, +x);

        /* Analyze slopes */
        for (i = 0; i < hack->num_slopes; ++i)
        {
            m = hack->slopes[i];

            /* Memorize intersection slopes (for non-player-grids) */
            if ((e > 0) && (hack->slopes_min[y][x] < m) && (m < hack->slopes_max[y][x]))
            {
                switch (i / 32)
                {
                    case 3: vinfo[e].bits_3 |= (1L << (i % 32)); break;
                    case 2: vinfo[e].bits_2 |= (1L << (i % 32)); break;
                    case 1: vinfo[e].bits_1 |= (1L << (i % 32)); break;
                    case 0: vinfo[e].bits_0 |= (1L << (i % 32)); break;
                }
            }
        }

        /* Default */
        vinfo[e].next_0 = &vinfo[0];

        /* Grid next child */
        if (distance(0, 0, y, x + 1) <= MAX_SIGHT)
        {
            g = GRID(y, x + 1);

            if (queue[queue_tail - 1]->grid[0] != g)
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
        if (distance(0, 0, y + 1, x + 1) <= MAX_SIGHT)
        {
            g = GRID(y + 1, x + 1);

            if (queue[queue_tail - 1]->grid[0] != g)
            {
                vinfo[queue_tail].grid[0] = g;
                queue[queue_tail] = &vinfo[queue_tail];
                queue_tail++;
            }

            vinfo[e].next_1 = &vinfo[queue_tail - 1];
        }

        /* Hack -- main diagonal has special children */
        if (y == x) vinfo[e].next_0 = vinfo[e].next_1;

        /* Extra values */
        vinfo[e].y = y;
        vinfo[e].x = x;
        vinfo[e].d = ((y > x)? (y + x / 2): (x + y / 2));
        vinfo[e].r = ((!y)? x: (!x)? y: (y == x)? y: 0);
    }

    /* Verify maximal bits XXX XXX XXX */
    if (((vinfo[1].bits_3 | vinfo[2].bits_3) != vinfo_bits_3) ||
        ((vinfo[1].bits_2 | vinfo[2].bits_2) != vinfo_bits_2) ||
        ((vinfo[1].bits_1 | vinfo[2].bits_1) != vinfo_bits_1) ||
        ((vinfo[1].bits_0 | vinfo[2].bits_0) != vinfo_bits_0))
    {
        quit_fmt("Incorrect bit masks: %x %x %x %x", (vinfo[1].bits_3 | vinfo[2].bits_3),
            (vinfo[1].bits_2 | vinfo[2].bits_2), (vinfo[1].bits_1 | vinfo[2].bits_1),
            (vinfo[1].bits_0 | vinfo[2].bits_0));
    }

    /* Kill hack */
    mem_free(hack);

    /* Success */
    return (0);
}


/*
 * Forget the "CAVE_VIEW" grids, redrawing as needed
 */
void forget_view(struct player *p)
{
    int i, g;
    int fast_view_n = p->view_n;
    u16b *fast_view_g = p->view_g;
    byte *fast_cave_info = &p->cave->info[0][0];

    /* None to forget */
    if (!fast_view_n) return;

    /* Clear them all */
    for (i = 0; i < fast_view_n; i++)
    {
        int y, x;

        /* Grid */
        g = fast_view_g[i];

        /* Location */
        y = GRID_Y(g);
        x = GRID_X(g);

        /* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
        fast_cave_info[g] &= ~(CAVE_VIEW | CAVE_SEEN);

        /* Redraw */
        cave_light_spot_aux(p, cave_get(p->depth), y, x);
    }

    /* None left */
    fast_view_n = 0;

    /* Save 'view_n' */
    p->view_n = fast_view_n;
}


/*
 * Calculate the complete field of view using a new algorithm
 *
 * If "view_g" and "temp_g" were global pointers to arrays of grids, as
 * opposed to actual arrays of grids, then we could be more efficient by
 * using "pointer swapping".
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
void update_view(struct player *p)
{
    int py = p->py;
    int px = p->px;
    int pg = GRID(py, px);
    int i, j, k, g, o2;
    int radius;
    int fast_view_n = p->view_n;
    u16b *fast_view_g = p->view_g;
    int fast_temp_n = 0;
    u16b *fast_temp_g = p->temp_g;
    byte *fast_cave_info = &p->cave->info[0][0];
    byte info;
    int y, x;

    /*** Step 0 -- Begin ***/

    /* Save the old "view" grids for later */
    for (i = 0; i < fast_view_n; i++)
    {
        /* Grid */
        g = fast_view_g[i];
        y = GRID_Y(g);
        x = GRID_X(g);

        /* Get grid info */
        info = fast_cave_info[g];

        /* Save "CAVE_SEEN" grids */
        if (info & (CAVE_SEEN))
        {
            /* Set "CAVE_TEMP" flag */
            cave_get(p->depth)->info[y][x] |= (CAVE_TEMP);

            /* Save grid for later */
            fast_temp_g[fast_temp_n++] = g;
        }

        /* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
        info &= ~(CAVE_VIEW | CAVE_SEEN);

        /* Save cave info */
        fast_cave_info[g] = info;
    }

    /* Reset the "view" array */
    fast_view_n = 0;

    /* Extract "radius" value */
    radius = p->cur_light;

    /* Handle real light */
    if (radius > 0) ++radius;

    /* Scan monster list and add monster lights */
    for (k = 1; k < cave_monster_max(cave_get(p->depth)); k++)
    {
        bool in_los;

        /* Check the k'th monster */
        monster_type *m_ptr = cave_monster(cave_get(p->depth), k);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Access the location */
        int fx = m_ptr->fx;
        int fy = m_ptr->fy;

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip monsters not carrying light */
        if (!rf_has(r_ptr->flags, RF_HAS_LIGHT)) continue;

        in_los = los(p->depth, p->py, p->px, fy, fx);

        /* Light a 3x3 box centered on the monster */
        for (i = -1; i <= 1; i++)
        {
            for (j = -1; j <= 1; j++)
            {
                int sy = fy + i;
                int sx = fx + j;

                /* If the monster isn't visible we can only light open tiles */
                if (!in_los && !cave_floor_bold(p->depth, sy, sx)) continue;

                /* If the tile is too far away we won't light it */
                if (distance(p->py, p->px, sy, sx) > MAX_SIGHT) continue;

                /* If the tile itself isn't in LOS, don't light it */
                if (!los(p->depth, p->py, p->px, sy, sx)) continue;

                g = GRID(sy, sx);

                /* Mark the square lit and seen */
                fast_cave_info[g] |= (CAVE_VIEW | CAVE_SEEN);

                /* Save in array */
                fast_view_g[fast_view_n++] = g;
            }
        }
    }

    /* Scan player list and add player lights */
    for (k = 1; k < NumPlayers + 1; k++)
    {
        bool in_los;

        /* Check the k'th player */
        player_type *q_ptr = player_get(k);

        /* Access the location */
        int fx = q_ptr->px;
        int fy = q_ptr->py;

        /* Ignore the player that we're updating */
        if (q_ptr == p) continue;

        /* Skip players not on this depth */
        if (q_ptr->depth != p->depth) continue;

        /* Skip players not carrying light */
        if (!q_ptr->cur_light) continue;

        in_los = los(p->depth, p->py, p->px, fy, fx);

        /* Light a cur_light radius area centered on the player */
        for (i = 0 - q_ptr->cur_light; i <= q_ptr->cur_light; i++)
        {
            for (j = 0 - q_ptr->cur_light; j <= q_ptr->cur_light; j++)
            {
                int sy = fy + i;
                int sx = fx + j;

                /* Oops */
                if (!in_bounds(sy, sx)) continue;

                /* Skip grids out of radius */
                if (distance(fy, fx, sy, sx) > q_ptr->cur_light) continue;

                /* If the player isn't visible we can only light open tiles */
                if (!in_los && !cave_floor_bold(p->depth, sy, sx)) continue;

                /* If the tile is too far away we won't light it */
                if (distance(p->py, p->px, sy, sx) > MAX_SIGHT) continue;

                /* If the tile itself isn't in LOS, don't light it */
                if (!los(p->depth, p->py, p->px, sy, sx)) continue;

                /* If the tile isn't in LOS of the other player, don't light it either */
                if (!los(p->depth, fy, fx, sy, sx)) continue;

                g = GRID(sy, sx);

                /* Mark the square lit and seen */
                fast_cave_info[g] |= (CAVE_VIEW | CAVE_SEEN);

                /* Save in array */
                fast_view_g[fast_view_n++] = g;
            }
        }
    }

    /*** Step 1 -- player grid ***/

    /* Player grid */
    g = pg;
    y = GRID_Y(g);
    x = GRID_X(g);

    /* Get grid info */
    info = fast_cave_info[g];

    /* Assume viewable */
    info |= (CAVE_VIEW);

    /* Torch-lit grid */
    if (0 < radius)
    {
        /* Mark as "CAVE_SEEN" */
        info |= (CAVE_SEEN);
    }

    /* Perma-lit grid */
    else if (cave_get(p->depth)->info[y][x] & (CAVE_GLOW))
    {
        /* Mark as "CAVE_SEEN" */
        info |= (CAVE_SEEN);
    }

    /* Save cave info */
    fast_cave_info[g] = info;

    /* Save in array */
    fast_view_g[fast_view_n++] = g;

    /*** Step 2 -- octants ***/

    /* Scan each octant */
    for (o2 = 0; o2 < 8; o2++)
    {
        vinfo_type *pv;

        /* Last added */
        vinfo_type *last = &vinfo[0];

        /* Grid queue */
        int queue_head = 0;
        int queue_tail = 0;
        vinfo_type *queue[VINFO_MAX_GRIDS * 2];

        /* Slope bit vector */
        u32b bits0 = vinfo_bits_0;
        u32b bits1 = vinfo_bits_1;
        u32b bits2 = vinfo_bits_2;
        u32b bits3 = vinfo_bits_3;

        /* Reset queue */
        queue_head = queue_tail = 0;

        /* Initial grids */
        queue[queue_tail++] = &vinfo[1];
        queue[queue_tail++] = &vinfo[2];

        /* Process queue */
        while (queue_head < queue_tail)
        {
            /* Dequeue next grid */
            pv = queue[queue_head++];

            /* Check bits */
            if ((bits0 & (pv->bits_0)) || (bits1 & (pv->bits_1)) ||
                (bits2 & (pv->bits_2)) || (bits3 & (pv->bits_3)))
            {
                /* Extract grid value XXX XXX XXX */
                g = pg + pv->grid[o2];
                y = GRID_Y(g);
                x = GRID_X(g);

                /* Get grid info */
                info = fast_cave_info[g];

                /* Handle wall */
                if (cave_get(p->depth)->info[y][x] & (CAVE_WALL))
                {
                    /* Clear bits */
                    bits0 &= ~(pv->bits_0);
                    bits1 &= ~(pv->bits_1);
                    bits2 &= ~(pv->bits_2);
                    bits3 &= ~(pv->bits_3);

                    /* Newly viewable wall */
                    if (!(info & (CAVE_VIEW)))
                    {
                        /* Mark as viewable */
                        info |= (CAVE_VIEW);

                        /* Torch-lit grids */
                        if (pv->d < radius)
                        {
                            /* Mark as "CAVE_SEEN" */
                            info |= (CAVE_SEEN);
                        }

                        /* Perma-lit grids */
                        else if (cave_get(p->depth)->info[y][x] & (CAVE_GLOW))
                        {
                            /* Hack -- move towards player */
                            int yy = ((y < py)? (y + 1): (y > py)? (y - 1): y);
                            int xx = ((x < px)? (x + 1): (x > px)? (x - 1): x);

                            /* Check for "simple" illumination */
                            if (cave_get(p->depth)->info[yy][xx] & (CAVE_GLOW))
                            {
                                /* Mark as seen */
                                info |= (CAVE_SEEN);
                            }
                        }

                        /* Save cave info */
                        fast_cave_info[g] = info;

                        /* Save in array */
                        fast_view_g[fast_view_n++] = g;
                    }
                }

                /* Handle non-wall */
                else
                {
                    /* Enqueue child */
                    if (last != pv->next_0)
                        queue[queue_tail++] = last = pv->next_0;

                    /* Enqueue child */
                    if (last != pv->next_1)
                        queue[queue_tail++] = last = pv->next_1;

                    /* Newly viewable non-wall */
                    if (!(info & (CAVE_VIEW)))
                    {
                        /* Mark as "viewable" */
                        info |= (CAVE_VIEW);

                        /* Torch-lit grids */
                        if (pv->d < radius)
                        {
                            /* Mark as "CAVE_SEEN" */
                            info |= (CAVE_SEEN);
                        }

                        /* Perma-lit grids */
                        else if (cave_get(p->depth)->info[y][x] & (CAVE_GLOW))
                        {
                            /* Mark as "CAVE_SEEN" */
                            info |= (CAVE_SEEN);
                        }

                        /* Save cave info */
                        fast_cave_info[g] = info;

                        /* Save in array */
                        fast_view_g[fast_view_n++] = g;
                    }
                }
            }
        }
    }

    /*** Step 3 -- Complete the algorithm ***/

    /* Handle blindness */
    if (p->timed[TMD_BLIND])
    {
        /* Process "new" grids */
        for (i = 0; i < fast_view_n; i++)
        {
            /* Grid */
            g = fast_view_g[i];

            /* Grid cannot be "CAVE_SEEN" */
            fast_cave_info[g] &= ~(CAVE_SEEN);
        }
    }

    /* Process "new" grids */
    for (i = 0; i < fast_view_n; i++)
    {
        /* Grid */
        g = fast_view_g[i];
        y = GRID_Y(g);
        x = GRID_X(g);

        /* Get grid info */
        info = fast_cave_info[g];

        /* Was not "CAVE_SEEN", is now "CAVE_SEEN" */
        if ((info & (CAVE_SEEN)) && !(cave_get(p->depth)->info[y][x] & (CAVE_TEMP)))
        {
            /* Handle feeling squares */
            if ((cave_get(p->depth)->info[y][x] & CAVE_FEEL) && (p->cave->info[y][x] & CAVE_FEEL))
            {
                p->cave->feeling_squares++;

                /* Erase the square so you can't 'resee' it */
                p->cave->info[y][x] &= ~(CAVE_FEEL);

                /* Display feeling if necessary */
                if (p->cave->feeling_squares == FEELING1) display_feeling(p, TRUE);
            }

            /* Note */
            cave_note_spot_aux(p, cave_get(p->depth), y, x);

            /* Redraw */
            cave_light_spot_aux(p, cave_get(p->depth), y, x);
        }
    }

    /* Process "old" grids */
    for (i = 0; i < fast_temp_n; i++)
    {
        /* Grid */
        g = fast_temp_g[i];
        y = GRID_Y(g);
        x = GRID_X(g);

        /* Get grid info */
        info = fast_cave_info[g];

        /* Clear "CAVE_TEMP" flag */
        cave_get(p->depth)->info[y][x] &= ~(CAVE_TEMP);

        /* Save cave info */
        fast_cave_info[g] = info;

        /* Was "CAVE_SEEN", is now not "CAVE_SEEN" */
        if (!(info & (CAVE_SEEN)))
        {
            /* Redraw */
            cave_light_spot_aux(p, cave_get(p->depth), y, x);
        }
    }

    /* Save 'view_n' */
    p->view_n = fast_view_n;
}


/*
 * Size of the circular queue used by "cave_update_flow()"
 */
#define FLOW_MAX 2048


/*
 * Hack -- forget the "flow" information
 */
void cave_forget_flow(struct player *p)
{
    int x, y;

    /* Nothing to forget */
    if (!p->flow_save) return;

    /* Check the entire dungeon */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        for (x = 0; x < DUNGEON_WID; x++)
        {
            /* Forget the old data */
            p->cave->cost[y][x] = 0;
            p->cave->when[y][x] = 0;
        }
    }

    /* Start over */
    p->flow_save = 0;
}


/*
 * Hack -- fill in the "cost" field of every grid that the player can
 * "reach" with the number of steps needed to reach that grid.  This
 * also yields the "distance" of the player from every grid.
 *
 * In addition, mark the "when" of the grids that can reach the player
 * with the incremented value of "flow_save".
 *
 * Hack -- use the local "flow_y" and "flow_x" arrays as a "circular
 * queue" of cave grids.
 *
 * We do not need a priority queue because the cost from grid to grid
 * is always "one" (even along diagonals) and we process them in order.
 */
void cave_update_flow(struct player *p, struct cave *c)
{
    int py = p->py;
    int px = p->px;
    int ty, tx;
    int y, x;
    int n, d;
    int flow_n;
    int flow_tail = 0;
    int flow_head = 0;
    byte flow_y[FLOW_MAX];
    byte flow_x[FLOW_MAX];

    /*** Cycle the flow ***/

    /* Cycle the flow */
    if (p->flow_save++ == 255)
    {
        /* Cycle the flow */
        for (y = 0; y < DUNGEON_HGT; y++)
        {
            for (x = 0; x < DUNGEON_WID; x++)
            {
                int w = p->cave->when[y][x];
                p->cave->when[y][x] = ((w >= 128)? (w - 128): 0);
            }
        }

        /* Restart */
        p->flow_save = 128;
    }

    /* Local variable */
    flow_n = p->flow_save;

    /*** Player Grid ***/

    /* Save the time-stamp */
    p->cave->when[py][px] = flow_n;

    /* Save the flow cost */
    p->cave->cost[py][px] = 0;

    /* Enqueue that entry */
    flow_y[flow_head] = py;
    flow_x[flow_head] = px;

    /* Advance the queue */
    ++flow_tail;

    /*** Process Queue ***/

    /* Now process the queue */
    while (flow_head != flow_tail)
    {
        /* Extract the next entry */
        ty = flow_y[flow_head];
        tx = flow_x[flow_head];

        /* Forget that entry (with wrap) */
        if (++flow_head == FLOW_MAX) flow_head = 0;

        /* Child cost */
        n = p->cave->cost[ty][tx] + 1;

        /* Hack -- Limit flow depth */
        if (n == MONSTER_FLOW_DEPTH) continue;

        /* Add the "children" */
        for (d = 0; d < 8; d++)
        {
            int old_head = flow_tail;

            /* Child location */
            y = ty + ddy_ddd[d];
            x = tx + ddx_ddd[d];

            /* Oops */
            if (!in_bounds(y, x)) continue;

            /* Ignore "pre-stamped" entries */
            if (p->cave->when[y][x] == flow_n) continue;

            /* Ignore "walls" and "rubble" */
            if (cave_isrubble(c, y, x) || cave_isstrongwall(c, y, x)) continue;

            /* Save the time-stamp */
            p->cave->when[y][x] = flow_n;

            /* Save the flow cost */
            p->cave->cost[y][x] = n;

            /* Enqueue that entry */
            flow_y[flow_tail] = y;
            flow_x[flow_tail] = x;

            /* Advance the queue */
            if (++flow_tail == FLOW_MAX) flow_tail = 0;

            /* Hack -- Overflow by forgetting new entry */
            if (flow_tail == flow_head) flow_tail = old_head;
        }
    }
}


/*
 * Light up the dungeon using "clairvoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", and memorizes all grids as with magic mapping.
 */
void wiz_light(struct player *p, bool full)
{
    int i, y, x;

    /* Hack -- DM has full detection */
    if (p->dm_flags & DM_SEE_LEVEL) full = TRUE;

    /* Memorize objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!o_ptr->kind) continue;

        /* Skip held objects */
        if (o_ptr->held_m_idx) continue;

        /* Skip objects not on this depth */
        if (o_ptr->depth != p->depth) continue;

        /* Memorize */
        if (p->obj_marked[i] < MARK_SEEN)
            p->obj_marked[i] = (full? MARK_SEEN: MARK_AWARE);
    }

    /* Scan all normal grids */
    for (y = 1; y < DUNGEON_HGT - 1; y++)
    {
        /* Scan all normal grids */
        for (x = 1; x < DUNGEON_WID - 1; x++)
        {
            /* Process all non-walls */
            if (!cave_wall_basic(cave_get(p->depth)->feat[y][x]))
            {
                /* Scan all neighbors */
                for (i = 0; i < 9; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Perma-light the grid */
                    cave_get(p->depth)->info[yy][xx] |= (CAVE_GLOW);

                    /* Memorize normal features */
                    if (!cave_floor_basic(cave_get(p->depth)->feat[yy][xx]))
                    {
                        /* Memorize the grid */
                        p->cave->info[yy][xx] |= CAVE_MARK;
                    }
                }
            }
        }
    }

    /* Fully update the visuals */
    p->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    p->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
static void wiz_dark(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i, y, x;

    /* Forget every grid */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        for (x = 0; x < DUNGEON_WID; x++)
        {
            /* Process the grid */
            p_ptr->cave->info[y][x] &= ~(CAVE_MARK | CAVE_DTRAP | CAVE_DEDGE);

            /* PWMAngband: unlight all permalit grids */
            cave_get(p_ptr->depth)->info[y][x] &= ~CAVE_GLOW;
        }
    }

    /* Forget all objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!o_ptr->kind) continue;

        /* Skip held objects */
        if (o_ptr->held_m_idx) continue;

        /* Skip objects not on this depth */
        if (o_ptr->depth != p_ptr->depth) continue;

        /* Forget the object */
        p_ptr->obj_marked[i] = MARK_UNAWARE;
    }

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    p_ptr->redraw |= (PR_MAP | PR_DTRAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Light or Darken the town
 * Also applied for wilderness and special levels
 */
void cave_illuminate(struct player *p, struct cave *c, bool daytime)
{
    int y, x, i;

    /* Not on random levels */
    if (random_level(c->depth)) return;

    /* Make sure we're not in a store */
    if (p && in_store(p)) return;

    /* Apply light or darkness */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            /* Interesting grids */
            if (((town_area(c->depth) || (c->depth > 0)) &&
                !cave_floor_basic(c->feat[y][x])) || (c->info[y][x] & CAVE_ROOM))
            {
                /* Illuminate the grid */
                c->info[y][x] |= CAVE_GLOW;

                /* Memorize the grid */
                if (p) p->cave->info[y][x] |= CAVE_MARK;
            }

            /* Boring grids (light) */
            else if (daytime || (c->depth > 0))
            {
                /* Illuminate the grid */
                c->info[y][x] |= CAVE_GLOW;

                /* Hack -- Memorize grids */
                if (p) p->cave->info[y][x] |= CAVE_MARK;
            }

            /* Boring grids (dark) */
            else
            {
                /* Darken the grid */
                c->info[y][x] &= ~CAVE_GLOW;

                /* Hack -- Forget grids */
                if (p) p->cave->info[y][x] &= ~CAVE_MARK;
            }
        }
    }

    /* Handle shop doorways */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            /* Track shop doorways */
            if (cave_isshop(c, y, x))
            {
                for (i = 0; i < 8; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Illuminate the grid */
                    c->info[yy][xx] |= CAVE_GLOW;

                    /* Hack -- Memorize grids */
                    if (p) p->cave->info[yy][xx] |= CAVE_MARK;
                }
            }
        }
    }

    /* Fully update the visuals */
    if (p) p->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    if (p) p->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Change the "feat" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(struct cave *c, int y, int x, int feat)
{
    my_assert(c);
    my_assert(y >= 0 && y < DUNGEON_HGT);
    my_assert(x >= 0 && x < DUNGEON_WID);

    /* Change the feature */
    c->feat[y][x] = feat;

    /* Handle "wall/door" grids */
    if (((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_PERM_SOLID)) ||
        ((feat >= FEAT_TREE) && (feat <= FEAT_PERM_CLEAR)) ||
        ((feat >= FEAT_HOME_HEAD) && (feat <= FEAT_HOME_TAIL)))
    {
        c->info[y][x] |= CAVE_WALL;
    }

    /* Handle "floor"/etc grids */
    else c->info[y][x] &= ~CAVE_WALL;

    /* Notice */
    cave_note_spot(c, y, x);

    /* Redraw */
    cave_light_spot(c, y, x);
}


bool cave_in_bounds(struct cave *c, int y, int x)
{
    return ((x >= 0) && (x < c->width) && (y >= 0) && (y < c->height));
}


bool cave_in_bounds_fully(struct cave *c, int y, int x)
{
    return ((x > 0) && (x < c->width - 1) && (y > 0) && (y < c->height - 1));
}


/*
 * Determine the path taken by a projection.
 *
 * The projection will always start from the grid (y1,x1), and will travel
 * towards the grid (y2,x2), touching one grid per unit of distance along
 * the major axis, and stopping when it enters the destination grid or a
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
 * that the initial grid (y1,x1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * The "flg" flags can be used to modify the behavior of this function.
 *
 * In particular, the "PROJECT_STOP" and "PROJECT_THRU" flags have the same
 * semantics as they do for the "project" function, namely, that the path
 * will stop as soon as it hits a monster, or that the path will continue
 * through the destination grid, respectively.
 *
 * The "PROJECT_JUMP" flag, which for the "project()" function means to
 * start at a special grid (which makes no sense in this function), means
 * that the path should be "angled" slightly if needed to avoid any wall
 * grids, allowing the player to "target" any grid which is in "view".
 * This flag is non-trivial and has not yet been implemented, but could
 * perhaps make use of the "vinfo" array (above).  XXX XXX XXX
 *
 * This function returns the number of grids (if any) in the path.  This
 * function will return zero if and only if (y1,x1) and (y2,x2) are equal.
 *
 * This algorithm is similar to, but slightly different from, the one used
 * by "update_view_los()", and very different from the one used by "los()".
 */
int project_path(u16b *gp, int range, int depth, int y1, int x1, int y2, int x2, int flg)
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
    if ((x1 == x2) && (y1 == y2)) return (0);

    /* Analyze "dy" */
    if (y2 < y1)
    {
        ay = (y1 - y2);
        sy = -1;
    }
    else
    {
        ay = (y2 - y1);
        sy = 1;
    }

    /* Analyze "dx" */
    if (x2 < x1)
    {
        ax = (x1 - x2);
        sx = -1;
    }
    else
    {
        ax = (x2 - x1);
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
        y = y1 + sy;
        x = x1;

        /* Create the projection path */
        while (1)
        {
            /* Save grid */
            gp[n++] = GRID(y, x);

            /* Hack -- Check maximum range */
            if ((n + (k >> 1)) >= range) break;

            /* Sometimes stop at destination grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if ((x == x2) && (y == y2)) break;
            }

            /* Always stop at non-initial wall grids */
            if ((n > 0) && !cave_floor_bold(depth, y, x)) break;

            /* Sometimes stop at non-initial monsters/players */
            if (flg & (PROJECT_STOP))
            {
                if ((n > 0) && cave_get(depth)->m_idx[y][x]) break;
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
        y = y1;
        x = x1 + sx;

        /* Create the projection path */
        while (1)
        {
            /* Save grid */
            gp[n++] = GRID(y, x);

            /* Hack -- Check maximum range */
            if ((n + (k >> 1)) >= range) break;

            /* Sometimes stop at destination grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if ((x == x2) && (y == y2)) break;
            }

            /* Always stop at non-initial wall grids */
            if ((n > 0) && !cave_floor_bold(depth, y, x)) break;

            /* Sometimes stop at non-initial monsters/players */
            if (flg & (PROJECT_STOP))
            {
                if ((n > 0) && cave_get(depth)->m_idx[y][x]) break;
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
        y = y1 + sy;
        x = x1 + sx;

        /* Create the projection path */
        while (1)
        {
            /* Save grid */
            gp[n++] = GRID(y, x);

            /* Hack -- Check maximum range */
            if ((n + (n >> 1)) >= range) break;

            /* Sometimes stop at destination grid */
            if (!(flg & (PROJECT_THRU)))
            {
                if ((x == x2) && (y == y2)) break;
            }

            /* Always stop at non-initial wall grids */
            if ((n > 0) && !cave_floor_bold(depth, y, x)) break;

            /* Sometimes stop at non-initial monsters/players */
            if (flg & (PROJECT_STOP))
            {
                if ((n > 0) && cave_get(depth)->m_idx[y][x]) break;
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
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming that no monster gets in the way,
 * using the "project_path()" function to check the projection path.
 *
 * Note that no grid is ever "projectable()" from itself.
 *
 * This function is used to determine if the player can (easily) target
 * a given grid.
 */
bool projectable(int depth, int y1, int x1, int y2, int x2, int flg)
{
    int y, x;
    int grid_n = 0;
    u16b grid_g[512];

    /* Check the projection path */
    grid_n = project_path(grid_g, MAX_RANGE, depth, y1, x1, y2, x2, flg);

    /* No grid is ever projectable from itself */
    if (!grid_n) return (FALSE);

    /* Final grid */
    y = GRID_Y(grid_g[grid_n - 1]);
    x = GRID_X(grid_g[grid_n - 1]);

    /* May not end in a wall grid */
    if (!cave_floor_bold(depth, y, x)) return (FALSE);

    /* May not end in an unrequested grid */
    if ((y != y2) || (x != x2)) return (FALSE);

    /* Assume okay */
    return (TRUE);
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
 * The "skip_los" parameter specifies whether "los" is necessary.
 */
void scatter(int depth, int *yp, int *xp, int y, int x, int d, bool skip_los)
{
    int nx, ny;

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
        if (skip_los || los(depth, y, x, ny, nx)) break;
    }

    /* Save the location */
    (*yp) = ny;
    (*xp) = nx;
}


/*
 * Track a new monster
 */
void health_track(struct player *p, int who)
{
    /* Track a new guy */
    p->health_who = who;

    /* Redraw (later) */
    p->redraw |= (PR_HEALTH);
}


/*
 * Hack -- track the given monster race
 */
void monster_race_track(int Ind, int r_idx)
{
    player_type *p_ptr = player_get(Ind);

    /* Redraw */
    if (p_ptr->monster_race_idx != r_idx) p_ptr->redraw |= (PR_MONSTER);

    /* Save this monster ID */
    p_ptr->monster_race_idx = r_idx;
}


/*
 * Something has happened to disturb the player.
 *
 * The first arg indicates a major disturbance, which affects search.
 *
 * The second arg is currently unused, but could induce output flush.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(struct player *p, int stop_search, int unused_flag)
{
    /* Dungeon Master is never disturbed */
    /*if (p->dm_flags & DM_NEVER_DISTURB) return;*/

    /* Cancel repeated commands */
    p->search_request = 0;

    /* Cancel Resting */
    if (p->resting)
    {
        p->resting = 0;
        p->redraw |= (PR_STATE);
    }

    /* Cancel running */
    if (p->running)
    {
        p->running = FALSE;

        /* Check for new panel if appropriate */
        if (OPT_P(p, center_player)) verify_panel(p);

        p->update |= (PU_TORCH);
    }

    /* Cancel searching if requested */
    if (stop_search && p->searching)
    {
        p->searching = FALSE;
        p->update |= (PU_BONUS);
        p->redraw |= (PR_STATE);
    }

    /* Get out of icky screen if requested */
    if (stop_search && p->screen_icky && OPT_P(p, disturb_icky)) Send_term_info(p, NTERM_HOLD, 0);

    /* Cancel looking around if requested */
    if (stop_search &&
        (((p->offset_y != p->offset_y_old) && (p->offset_y_old != -1)) ||
        ((p->offset_x != p->offset_x_old) && (p->offset_x_old != -1))))
    {
        /* Cancel input */
        Send_term_info(p, NTERM_HOLD, 0);

        /* Stop locating */
        do_cmd_locate(p, 0);
    }
}


/*
 * Hack -- Check if a level is a "quest" level
 */
bool is_quest(int depth)
{
    return ((depth == 99) || (depth == 100) || (depth == 126) || (depth == 127));
}


struct cave *world[MAX_DEPTH + MAX_WILD];
struct cave **cave = &world[MAX_WILD];


struct cave *cave_new(s16b depth)
{
    struct cave *c = mem_zalloc(sizeof(*c));

    c->info = C_ZNEW(DUNGEON_HGT, byte_wid);
    c->feat = C_ZNEW(DUNGEON_HGT, byte_wid);
    c->m_idx = C_ZNEW(DUNGEON_HGT, s16b_wid);
    c->o_idx = C_ZNEW(DUNGEON_HGT, s16b_wid);
    c->monsters = C_ZNEW(z_info->m_max, struct monster);
    c->mon_max = 1;

    c->depth = depth;

    return c;
}


void cave_free(struct cave *c)
{
    mem_free(c->info);
    mem_free(c->feat);
    mem_free(c->m_idx);
    mem_free(c->o_idx);
    mem_free(c->monsters);
    mem_free(c);
}


/*
 * FEATURE PREDICATES
 *
 * These functions are used to figure out what kind of square something is,
 * via c->feat[y][x]. All direct testing of c->feat[y][x] should be rewritten
 * in terms of these functions.
 *
 * It's often better to use feature behavior predicates (written in terms of
 * these functions) instead of these functions directly. For instance,
 * cave_isrock() will return false for a secret door, even though it will
 * behave like a rock wall until the player determines it's a door.
 *
 * Use functions like cave_isdiggable, cave_iswall, etc. in these cases.
 */


/*
 * True if the square is normal open floor.
 */
bool cave_isfloor(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_FLOOR);
}


bool cave_issafefloor(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_FLOOR_SAFE);
}


bool cave_isotherfloor(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_LOOSE_DIRT) && (feat <= FEAT_CROP) && (feat != FEAT_LAVA));
}


bool cave_isanyfloor(struct cave *c, int y, int x)
{
    return (cave_isfloor(c, y, x) || cave_issafefloor(c, y, x) || cave_isotherfloor(c, y, x));
}


/*
 * True if the square is a normal granite rock wall.
 *
 * FEAT_WALL_SOLID is the normal feature type. The others are weird byproducts
 * of cave generation (and should be avoided).
 */
bool cave_isrock(struct cave *c, int y, int x)
{
    switch (c->feat[y][x])
    {
        case FEAT_WALL_EXTRA:
        case FEAT_WALL_INNER:
        case FEAT_WALL_OUTER:
        case FEAT_WALL_SOLID: return TRUE;
        default: return FALSE;
    }
}


/*
 * True if the square is a permanent wall.
 *
 * FEAT_PERM_SOLID is the normal feature type. The others are weird byproducts
 * of cave generation (and should be avoided).
 */
bool cave_isperm(struct cave *c, int y, int x)
{
    switch (c->feat[y][x])
    {
        case FEAT_PERM_EXTRA:
        case FEAT_PERM_BASIC:
        case FEAT_PERM_FAKE:
        case FEAT_PERM_ARENA:
        case FEAT_PERM_SOLID: return TRUE;
        default: return FALSE;
    }
}


/*
 * True if the square is a magma wall.
 */
bool cave_ismagma(struct cave *c, int y, int x)
{
    switch (c->feat[y][x])
    {
        case FEAT_MAGMA:
        case FEAT_MAGMA_H:
        case FEAT_MAGMA_K: return TRUE;
        default: return FALSE;
    }
}


/*
 * True if the square is a quartz wall.
 */
bool cave_isquartz(struct cave *c, int y, int x)
{
    switch (c->feat[y][x])
    {
        case FEAT_QUARTZ:
        case FEAT_QUARTZ_H:
        case FEAT_QUARTZ_K: return TRUE;
        default: return FALSE;
    }
}


/*
 * True if the square is a mineral wall (magma/quartz).
 */
bool cave_ismineral(struct cave *c, int y, int x)
{
    return (cave_isrock(c, y, x) || cave_ismagma(c, y, x) || cave_isquartz(c, y, x));
}


/*
 * True if the square is rubble.
 */
bool cave_isrubble(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_RUBBLE);
}


/*
 * True if the square is a hidden secret door.
 *
 * These squares appear as if they were granite -- when detected a secret door
 * is replaced by a closed door.
 */
bool cave_issecretdoor(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_SECRET);
}


/*
 * True if the square is an open door.
 */
bool cave_isopendoor(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_OPEN);
}


bool cave_isbasicopen(struct cave *c, int y, int x)
{
    return (cave_isopendoor(c, y, x) || (c->feat[y][x] == FEAT_HOME_OPEN));
}


/*
 * True if the square is a closed door (possibly locked or jammed).
 */
bool cave_iscloseddoor(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL));
}


/*
 * True if the square is a closed, locked door.
 */
bool cave_islockeddoor(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_DOOR_HEAD + 0x01) && (feat <= FEAT_DOOR_TAIL));
}


/*
 * True if the square is a closed, jammed door.
 */
bool cave_isjammeddoor(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_DOOR_HEAD + 0x08) && (feat <= FEAT_DOOR_TAIL));
}


bool cave_ishomedoor(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_HOME_HEAD) && (feat <= FEAT_HOME_TAIL));
}


bool cave_isbasicdoor(struct cave *c, int y, int x)
{
    return (cave_iscloseddoor(c, y, x) || cave_ishomedoor(c, y, x));
}


/*
 * True if the square is a door.
 *
 * This includes open, closed, and hidden doors.
 */
bool cave_isdoor(struct cave *c, int y, int x)
{
    return (cave_isopendoor(c, y, x) || cave_issecretdoor(c, y, x) || cave_iscloseddoor(c, y, x));
}


/*
 * True if the square is an unknown trap (it will appear as a floor tile).
 */
bool cave_issecrettrap(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_INVIS);
}


/*
 * True if the square is a known trap.
 */
bool cave_isknowntrap(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_TRAP_HEAD) && (feat <= FEAT_TRAP_TAIL));
}


/*
 * True if the square contains a trap, known or unknown.
 */
bool cave_istrap(struct cave *c, int y, int x)
{
    return (cave_issecrettrap(c, y, x) || cave_isknowntrap(c, y, x));
}


/*
 * True if cave is an up stair.
 */
bool cave_isupstairs(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_LESS);
}


/*
 * True if cave is a down stair.
 */
bool cave_isdownstairs(struct cave *c, int y, int x)
{
    return (c->feat[y][x] == FEAT_MORE);
}


/*
 * True if cave is an up or down stair
 */
bool cave_isstairs(struct cave *c, int y, int x)
{
    return (cave_isupstairs(c, y, x) || cave_isdownstairs(c, y, x));
}


bool cave_istree(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_TREE) && (feat <= FEAT_EVIL_TREE));
}


bool cave_isshop(struct cave *c, int y, int x)
{
    int feat = c->feat[y][x];

    return ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL));
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
 * True if the square is open (a floor square not occupied by a monster).
 */
bool cave_isopen(struct cave *c, int y, int x)
{
    return ((cave_isfloor(c, y, x) || cave_isotherfloor(c, y, x)) && !c->m_idx[y][x]);
}


/*
 * True if the square is empty (an open square without any items).
 */
bool cave_isempty(struct cave *c, int y, int x)
{
    return (cave_isopen(c, y, x) && !c->o_idx[y][x]);
}


/*
 * True if the square is a floor square without items.
 */
bool cave_canputitem(struct cave *c, int y, int x)
{
    return ((cave_isfloor(c, y, x) || cave_isotherfloor(c, y, x)) && !c->o_idx[y][x]);
}


/*
 * True if the square can be dug: this includes rubble and non-permanent walls.
 */
bool cave_isdiggable(struct cave *c, int y, int x)
{
    return (cave_ismineral(c, y, x) || cave_issecretdoor(c, y, x) || cave_isrubble(c, y, x));
}


/*
 * True if the square is passable by the player.
 *
 * This function is the logical negation of cave_iswall().
 */
bool cave_ispassable(struct cave *c, int y, int x)
{
    return !(c->info[y][x] & CAVE_WALL);
}


/*
 * True if the square is a wall square (impedes the player).
 *
 * This function is the logical negation of cave_ispassable().
 */
bool cave_iswall(struct cave *c, int y, int x)
{
    return (c->info[y][x] & CAVE_WALL);
}


/*
 * True if the square is a permanent wall or one of the "stronger" walls.
 *
 * The stronger walls are granite, magma and quartz. This excludes things like
 * secret doors and rubble.
 */
bool cave_isstrongwall(struct cave *c, int y, int x)
{
    return (cave_ismineral(c, y, x) || cave_isperm(c, y, x));
}


/*
 * True if the square is part of a vault.
 *
 * This doesn't say what kind of square it is, just that it is part of a vault.
 */
bool cave_isvault(struct cave *c, int y, int x)
{
    return (c->info[y][x] & CAVE_ICKY);
}


/*
 * True if the square is part of a room.
 */
bool cave_isroom(struct cave *c, int y, int x)
{
    return (c->info[y][x] & CAVE_ROOM);
}


/*
 * True if cave square is a feeling trigger square
 */
bool cave_isfeel(struct cave *c, int y, int x)
{
    return (c->info[y][x] & CAVE_FEEL);
}


/*
 * Get a monster on the current level by its index.
 */
struct monster *cave_monster(struct cave *c, int idx)
{
    /* Index MUST be valid */
    my_assert((idx >= 0) && (idx < c->mon_max));

    return &c->monsters[idx];
}


/*
 * Get a monster on the current level by its position.
 */
struct monster *cave_monster_at(struct cave *c, int y, int x)
{
    return cave_monster(c, c->m_idx[y][x]);
}


/*
 * The maximum number of monsters allowed in the level.
 */
int cave_monster_max(struct cave *c)
{
    return c->mon_max;
}


/*
 * The current number of monsters present on the level.
 */
int cave_monster_count(struct cave *c)
{
    return c->mon_cnt;
}


/*
 * Add visible treasure to a mineral square.
 */
void upgrade_mineral(struct cave *c, int y, int x)
{
    switch (c->feat[y][x])
    {
        case FEAT_MAGMA: cave_set_feat(c, y, x, FEAT_MAGMA_K); break;
        case FEAT_QUARTZ: cave_set_feat(c, y, x, FEAT_QUARTZ_K); break;
    }
}


struct cave *cave_get(s16b depth)
{
    /* Depth MUST be valid */
    my_assert((depth >= 0 - MAX_WILD) && (depth < MAX_DEPTH));

    return cave[depth];
}


/* PWMAngband */


/*
 * Update the visuals
 */
void update_visuals(int depth)
{
    int i;

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* If he's not here, skip him */
        if (p_ptr->depth != depth) continue;

        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }
}


/*
 * Fully update the flow
 */
void fully_update_flow(int depth)
{
    int i;

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* If he's not here, skip him */
        if (p_ptr->depth != depth) continue;

        /* Fully update the flow */
        p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);
    }
}


/*
 * Wipe the "CAVE_MARK" bit in everyone's array
 */
void forget_spot(int depth, int y, int x)
{
    int i;

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* If he's not here, skip him */
        if (p_ptr->depth != depth) continue;

        /* Forget the spot */
        p_ptr->cave->info[y][x] &= ~CAVE_MARK;
    }
}


/*
 * Display the full map of the dungeon in the active Term.
 */
void display_fullmap(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int x, y;

    /* Dump the map */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        /* First clear the old stuff */
        for (x = 0; x < DUNGEON_WID; x++)
        {
            p_ptr->scr_info[y][x].c = 0;
            p_ptr->scr_info[y][x].a = 0;
            p_ptr->trn_info[y][x].c = 0;
            p_ptr->trn_info[y][x].a = 0;
        }

        /* Scan the columns of row "y" */
        for (x = 0; x < DUNGEON_WID; x++)
        {
            byte a, ta;
            char c, tc;
            grid_data g;

            /* Determine what is there */
            map_info(p_ptr, y, x, &g);
            grid_data_as_text(p_ptr, FALSE, &g, &a, &c, &ta, &tc);

            p_ptr->scr_info[y][x].c = c;
            p_ptr->scr_info[y][x].a = a;
            p_ptr->trn_info[y][x].c = tc;
            p_ptr->trn_info[y][x].a = ta;
        }

        /* Send that line of info */
        Send_fullmap(Ind, y);
    }

    /* Reset the line counter */
    Send_fullmap(Ind, -1);
}


/*
 * Unlight the dungeon map.
 */
void deep_nights(struct player *p)
{
    int i;

    /* No effect outside of the dungeon during day */
    if ((p->depth <= 0) && is_daytime()) return;

    /* No effect on special levels */
    if (check_special_level(p->depth)) return;

    /* Check for every other player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);
        object_type *o_ptr;

        /* Only works for players on the level */
        if (p->depth != p_ptr->depth) continue;

        /* Get the light source */
        o_ptr = &p_ptr->inventory[INVEN_LIGHT];

        /* Bye bye light */
        if (o_ptr->kind)
        {
            bitflag f[OF_SIZE];

            /* Extract the flags */
            object_flags(o_ptr, f);

            if ((o_ptr->timeout > 0) && !of_has(f, OF_NO_FUEL))
            {
                msg(p_ptr, "Your light suddently empty.");

                /* No more light, it's Rogues day today :) */
                o_ptr->timeout = 0;

                /* Redraw */
                p_ptr->redraw |= (PR_EQUIP);
            }
        }

        /* Forget every grid */
        wiz_dark(i);
    }
}


/*
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming that no monster gets in the way,
 * using the "project_path()" function to check the projection path.
 *
 * Note that no grid is ever "projectable()" from itself.
 *
 * This function is used to determine if a monster can target the player.
 * The target can be in a wall... otherwise wraithed players/ghosts would be safe
 * from monster spells!
 */
bool projectable_wall(int depth, int y1, int x1, int y2, int x2)
{
    int y, x;
    int grid_n = 0;
    u16b grid_g[512];

    /* Check the projection path */
    grid_n = project_path(grid_g, MAX_RANGE, depth, y1, x1, y2, x2, 0);

    /* No grid is ever projectable from itself */
    if (!grid_n) return (FALSE);

    /* Final grid */
    y = GRID_Y(grid_g[grid_n - 1]);
    x = GRID_X(grid_g[grid_n - 1]);

    /* May not end in an unrequested grid */
    if ((y != y2) || (x != x2)) return (FALSE);

    /* Assume okay */
    return (TRUE);
}


/*
 * Cursor-track a new monster
 */
void cursor_track(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);

    /* Track a new guy */
    p_ptr->cursor_who = m_idx;
}


/*  
 * Update the cursors for anyone tracking a monster
 */
void update_cursor(int m_idx)
{
    int i;

    /* Each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* See if he is tracking this monster */
        if (p_ptr->cursor_who == m_idx)
        {
            /* Redraw */
            p_ptr->redraw |= (PR_CURSOR);
        }
    }
}


/*
 * Update the health bars for anyone tracking a monster
 */
void update_health(int m_idx)
{
    int i;

    /* Each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* See if he is tracking this monster */
        if (p_ptr->health_who == m_idx)
        {
            /* Redraw */
            p_ptr->redraw |= (PR_HEALTH);
        }
    }
}
