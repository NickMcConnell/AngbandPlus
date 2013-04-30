/*
 * File: spells1.c
 * Purpose: Some spell effects, and the project() function
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
#include "generate.h"
#include "monster/mon-make.h"
#include "monster/mon-msg.h"
#include "monster/mon-spell.h"
#include "monster/mon-timed.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "s-spells.h"
#include "squelch.h"
#include "target.h"
#include "wilderness.h"


/*
 * Details of the different projectable attack types in the game.
 * See spells.h for structure
 */
static const struct gf_type gf_table[] =
{
    #define GF(a, b, c, d, e, f, g, h, i, j, k, l, m) \
        {GF_##a, b, c, d, e, f, g, h, i, j, k, l, m},
    #define RV(b, x, y, m) {b, x, y, m}
    #include "../common/list-gf-types.h"
    #undef GF
    #undef RV
    {0}
};


/*
 * Check for resistance to a GF_ attack type. Return codes:
 * -1 = vulnerability
 * 0 = no resistance (or resistance plus vulnerability)
 * 1 = single resistance or opposition (or double resist plus vulnerability)
 * 2 = double resistance (including opposition)
 * 3 = total immunity
 *
 * type is the attack type we are trying to resist
 * flags is the set of flags we're checking
 * real is whether this is a real attack
 */
int check_for_resist_aux(struct player *p, int type, bitflag flags[OF_SIZE], bool real)
{
    const struct gf_type *gf_ptr = &gf_table[type];
    int result = 0;

    if (gf_ptr->vuln && of_has(flags, gf_ptr->vuln)) result--;

    /* If it's not a real attack, we don't check timed status explicitly */
    if (real && gf_ptr->opp && p->timed[gf_ptr->opp]) result++;

    if (gf_ptr->resist && of_has(flags, gf_ptr->resist)) result++;
    if (gf_ptr->immunity && of_has(flags, gf_ptr->immunity)) result = 3;

    /* Notice flags, if it's a real attack */
    if (real && gf_ptr->immunity) wieldeds_notice_flag(p, gf_ptr->immunity);
    if (real && gf_ptr->resist) wieldeds_notice_flag(p, gf_ptr->resist);
    if (real && gf_ptr->vuln) wieldeds_notice_flag(p, gf_ptr->vuln);

    /* Hack -- Space-time anchor lets us resist time and gravity */
    if (((type == GF_TIME) || (type == GF_GRAVITY)) && p->timed[TMD_ANCHOR])
        result = 1;

    return result;
}


int check_for_resist(struct player *p, int type, bool real)
{
    return check_for_resist_aux(p, type, p->state.flags, real);
}


/*
 * Check whether the player is immune to side effects of a GF_ type.
 *
 * type is the GF_ type we are checking.
 */
bool check_side_immune(struct player *p, int type)
{
    const struct gf_type *gf_ptr = &gf_table[type];

    if (gf_ptr->immunity)
    {
        if (gf_ptr->side_immune && check_state(p, gf_ptr->immunity)) return TRUE;
    }
    else if ((gf_ptr->resist && of_has(p->state.flags, gf_ptr->resist)) ||
        (gf_ptr->opp && p->timed[gf_ptr->opp]))
    {
        return TRUE;
    }
    return FALSE;
}


/*
 * Update monster knowledge of player resists.
 *
 * m_idx is the monster who is learning
 * type is the GF_ type to which it's learning about the player's resistance (or lack of)
 */
void monster_learn_resists(struct monster *m, struct player *p, int type)
{
    const struct gf_type *gf_ptr = &gf_table[type];

    update_smart_learn(m, p, gf_ptr->resist);
    update_smart_learn(m, p, gf_ptr->immunity);
    update_smart_learn(m, p, gf_ptr->vuln);
}


/*
 * Strip the HATES_ flags out of a flagset for any IGNORE_ flags that are
 * present
 */
void dedup_hates_flags(bitflag *f)
{
    size_t i;

    for (i = 0; i < GF_MAX; i++)
    {
        const struct gf_type *gf_ptr = &gf_table[i];

        if (gf_ptr->obj_imm && of_has(f, gf_ptr->obj_imm) && gf_ptr->obj_hates &&
            of_has(f, gf_ptr->obj_hates))
        {
            of_off(f, gf_ptr->obj_hates);
        }
    }
}


static const char *dragon_format[6][2] =
{
    {"Baby %s dragon", "Baby %s drake"},
    {"Young %s dragon", "Young %s drake"},
    {"Mature %s dragon", "Mature %s drake"},
    {"Ancient %s dragon", "Great %s drake"},
    {"Great %s Wyrm", "Great Wyrm of %s"},
    {"Ancient %s Wyrm", "Ancient Wyrm of %s"}
};


struct dragon_name
{
    const char *d_name;
    byte d_fmt;
    const char *w_name;
    byte w_fmt;
    byte commonness;
};


static struct dragon_name dragon_names[] =
{
    {"blue", 0, "Storm", 0, 6},
    {"white", 0, "Ice", 0, 6},
    {"green", 0, "Swamp", 0, 6},
    {"black", 0, "Bile", 0, 5},
    {"red", 0, "Hell", 0, 5},
    {"gold", 0, "Thunder", 1, 5},
    {"silver", 0, "Radiance", 1, 5},
    {"shadow", 1, "Shadow", 0, 4},
    {"crystal", 1, "Crystal", 0, 4},
    {"ethereal", 1, "Ethereal", 0, 4},
    {"water", 0, "the Deep", 1, 4},
    {"chaos", 1, "Chaos", 1, 3},
    {"law", 1, "Law", 1, 3},
    {"multi-hued", 0, "Many Colours", 1, 2},
    {"balance", 1, "Balance", 1, 2},
    {"power", 0, "Power", 1, 1}
};


static void get_dragon_name(int lvl_idx, int form_idx, char *name, size_t len)
{
    struct dragon_name *dn = &dragon_names[form_idx];

    /* Dragon */
    if (lvl_idx < 4)
        strnfmt(name, len, dragon_format[lvl_idx][dn->d_fmt], dn->d_name);

    /* Wyrm */
    else
        strnfmt(name, len, dragon_format[lvl_idx][dn->w_fmt], dn->w_name);
}


static s16b get_dragon_race(int lvl_idx, int form_idx)
{
    char name[NORMAL_WID];

    /* Name */
    get_dragon_name(lvl_idx, form_idx, name, sizeof(name));

    return get_r_idx(name);
}


static int get_dragon_form(int r_idx)
{
    int lvl_idx, form_idx;
    char name[NORMAL_WID];
    monster_race *r_ptr = &r_info[r_idx];

    for (lvl_idx = 0; lvl_idx < 6; lvl_idx++)
    {
        for (form_idx = 0; form_idx < N_ELEMENTS(dragon_names); form_idx++)
        {
            /* Name */
            get_dragon_name(lvl_idx, form_idx, name, sizeof(name));

            if (streq(r_ptr->name, name)) return form_idx;
        }
    }

    return -1;
}


static s16b get_dragon_random(void)
{
    int form_idx = 0, i, j;
    int options = 0;

    for (i = 0; i < N_ELEMENTS(dragon_names); i++)
    {
        struct dragon_name *dn = &dragon_names[i];

        for (j = 0; j < dn->commonness; j++)
        {
            if (one_in_(++options)) form_idx = i;
        }
    }

    return get_dragon_race(0, form_idx);
}


void poly_dragon(struct player *p, bool msg)
{
    int r_idx = 0;
    s16b race_newborn = get_r_idx("Newborn dragon");

    /* Character birth */
    if (p->r_idx == 0) r_idx = race_newborn;

    /* Keep current form at low level */
    else if (p->lev < 5) r_idx = p->r_idx;

    /* Random choice of race at level 5 */
    else if ((p->lev == 5) && (p->r_idx == race_newborn))
        r_idx = get_dragon_random();

    /* New form */
    else
    {
        int lvl_idx, form_idx;

        /* Level index */
        if (p->lev == PY_MAX_LEVEL) lvl_idx = 5;
        else lvl_idx = (p->lev - 5) / 10;

        /* Form index */
        form_idx = get_dragon_form(p->r_idx);

        /* New form */
        r_idx = get_dragon_race(lvl_idx, form_idx);
    }

    /* Polymorph into that dragon */
    if (r_idx && (r_idx != p->r_idx)) do_cmd_poly(p, r_idx, FALSE, msg);
}


void do_cmd_poly(struct player *p, int number, bool check_kills, bool domsg)
{
    monster_race *r_ptr;
    monster_lore *l_ptr;
    const char *s;

    /* Restrict ghosts */
    if (p->ghost)
    {
        if (domsg)
            msg(p, "You need a tangible body to polymorph!");
        else
            plog("You need a tangible body to polymorph!");
        return;
    }

    /* Check boundaries */
    if ((number < 0) || (number > z_info->r_max - 1))
    {
        if (domsg)
            msg(p, "This monster race doesn't exist.");
        else
            plog("This monster race doesn't exist.");
        return;
    }

    /* Nothing to do */
    if (number == p->r_idx)
    {
        if (domsg)
            msg(p, "You are already using that form.");
        else
            plog("You are already using that form.");
        return;
    }

    /* Polymorph into normal form */
    if (number == 0)
    {
        if (domsg)
            msg(p, "You polymorph back into your normal form.");
        else
            plog("You polymorph back into your normal form.");

        /* Wraithform */
        player_clear_timed(p, TMD_WRAITH, TRUE);

        /* Invisibility */
        player_clear_timed(p, TMD_INVIS, TRUE);

        /* Normal form */
        p->r_idx = 0;
        p->k_idx = 0;
        if (domsg) Send_poly(p, 0);

        /* Notice */
        p->update |= (PU_BONUS | PU_HP | PU_MONSTERS);

        /* Redraw */
        p->redraw |= (PR_MAP | PR_EQUIP | PR_SPELL);

        return;
    }

    r_ptr = &r_info[number];

    /* Skip non-entries */
    if (!r_ptr->name)
    {
        if (domsg)
            msg(p, "This monster race doesn't exist.");
        else
            plog("This monster race doesn't exist.");
        return;
    }

    /* Must not be unique */
    if (rf_has(r_ptr->flags, RF_UNIQUE))
    {
        if (domsg)
            msg(p, "This monster race is unique.");
        else
            plog("This monster race is unique.");
        return;
    }

    /* Check required kill count */
    l_ptr = &p->lore[number];
    if (check_kills && (!l_ptr->pkills || (l_ptr->pkills < r_ptr->level)))
    {
        if (domsg)
            msg(p, "You have not learned that form yet.");
        else
            plog("You have not learned that form yet.");
        return;
    }

    /* Polymorph into that monster */
    s = r_ptr->name;
    if (domsg)
        msg(p, "You polymorph into %s %s.", (is_a_vowel(tolower(s[0]))? "an": "a"), s);
    else
        plog_fmt("You polymorph into %s %s.", (is_a_vowel(tolower(s[0]))? "an": "a"), s);

    /* Wraithform */
    if (rf_has(r_ptr->flags, RF_PASS_WALL))
    {
        p->timed[TMD_WRAITH] = -1;
        p->redraw |= PR_STATUS;
        handle_stuff(p);
    }
    else
        player_clear_timed(p, TMD_WRAITH, TRUE);

    /* Invisibility */
    if (rf_has(r_ptr->flags, RF_INVISIBLE))
    {
        p->timed[TMD_INVIS] = -1;
        p->update |= PU_MONSTERS;
        p->redraw |= PR_STATUS;
        handle_stuff(p);
    }
    else
        player_clear_timed(p, TMD_INVIS, TRUE);

    /* New form */
    p->r_idx = number;
    if (domsg) Send_poly(p, number);

    /* Unaware players */
    p->k_idx = 0;
    if (rf_has(r_ptr->flags, RF_UNAWARE)) p->k_idx = -1;

    /* Hack -- Random mimics */
    if (r_ptr->base == lookup_monster_base("random mimic"))
    {
        /* Random symbol from object set */
        while (1)
        {
            /* Select a random object */
            p->k_idx = randint0(z_info->k_max - 1) + 1;

            /* Skip non-entries */
            if (!k_info[p->k_idx].name) continue;

            /* Skip empty entries */
            if (!k_info[p->k_idx].d_attr || !k_info[p->k_idx].d_char)
                continue;

            /* Force race attr */
            if (k_info[p->k_idx].d_attr != r_ptr->d_attr) continue;

            /* Success */
            break;
        }
    }

    /* Hack -- Object mimics */
    else if (r_ptr->mimic_kinds)
    {
        struct monster_mimic *mimic_kind;
        int i = 1;

        /* Pick a random object kind to mimic */
        for (mimic_kind = r_ptr->mimic_kinds; mimic_kind; mimic_kind = mimic_kind->next, i++)
        {
            if (one_in_(i)) p->k_idx = mimic_kind->kind->kidx;
        }
    }

    /* Notice */
    p->update |= (PU_BONUS | PU_HP | PU_MONSTERS);

    /* Redraw */
    p->redraw |= (PR_MAP | PR_EQUIP | PR_SPELL);
}


/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * Note that this function is one of the more "dangerous" ones...
 */
s16b poly_r_idx(int depth, int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    int i, r, lev1, lev2;

    /* Paranoia -- Uniques never polymorph */
    if (rf_has(r_ptr->flags, RF_UNIQUE)) return (r_idx);

    /* Allowable range of "levels" for resulting monster */
    lev1 = r_ptr->level - ((randint1(20) / randint1(9)) + 1);
    lev2 = r_ptr->level + ((randint1(20) / randint1(9)) + 1);

    /* Pick a (possibly new) non-unique race */
    for (i = 0; i < 1000; i++)
    {
        /* Pick a new race, using a level calculation */
        r = get_mon_num(depth, r_ptr->level + 5);

        /* Handle failure */
        if (!r) break;

        /* Obtain race */
        r_ptr = &r_info[r];

        /* Ignore unique monsters */
        if (rf_has(r_ptr->flags, RF_UNIQUE)) continue;

        /* Ignore monsters with incompatible levels */
        if ((r_ptr->level < lev1) || (r_ptr->level > lev2)) continue;

        /* Use that index */
        r_idx = r;

        /* Done */
        break;
    }

    /* Result */
    return (r_idx);
}


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
bool teleport_away(struct monster *m, int dis)
{
    int ny = 0, nx = 0, oy, ox, d, i, min;
    bool look = TRUE;

    /* Paranoia */
    if (!m->r_idx) return FALSE;

    /* Space-time anchor */
    if (check_st_anchor(m->depth, m->fy, m->fx)) return FALSE;

    /* No teleporting from inside pits */
    if (is_notele(m->depth, m->fy, m->fx)) return FALSE;

    /* Save the old location */
    oy = m->fy;
    ox = m->fx;

    /* Minimum distance */
    min = dis / 2;

    /* Look until done */
    while (look)
    {
        /* Verify max distance */
        if (dis > 200) dis = 200;

        /* Try several locations */
        for (i = 0; i < 500; i++)
        {
            /* Pick a (possibly illegal) location */
            while (1)
            {
                ny = rand_spread(oy, dis);
                nx = rand_spread(ox, dis);
                d = distance(oy, ox, ny, nx);
                if ((d >= min) && (d <= dis)) break;
            }

            /* Ignore illegal locations */
            if (!in_bounds_fully(ny, nx)) continue;

            /* Require "empty" floor space */
            if (!cave_empty_bold(m->depth, ny, nx)) continue;

            /* No teleport onto glyph of warding */
            if (cave_get(m->depth)->feat[ny][nx] == FEAT_GLYPH) continue;

            /* No teleporting into vaults and such */
            if (is_icky(m->depth, ny, nx)) continue;

            /* This grid looks good */
            look = FALSE;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;
    }

    /* Move the monster */
    monster_swap(m->depth, oy, ox, ny, nx);

    return TRUE;
}


/*
 * Teleport a monster to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
bool teleport_to(int depth, int m_idx, int ny, int nx)
{
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    int dis = 0, tries = 200, y, x, ctr = 0;

    /* Paranoia */
    if (!m_ptr->r_idx) return FALSE;

    /* Space-time anchor */
    if (check_st_anchor(depth, m_ptr->fy, m_ptr->fx)) return FALSE;

    /* No teleporting from inside pits */
    if (is_notele(depth, m_ptr->fy, m_ptr->fx)) return FALSE;

    /* Find a usable location */
    while (--tries)
    {
        /* Pick a nearby legal location */
        while (1)
        {
            y = rand_spread(ny, dis);
            x = rand_spread(nx, dis);
            if (in_bounds_fully(y, x)) break;
        }

        /* Accept "naked" floor grids... */
        /* ... but only accept grids in LOS of the caster */
        if (cave_naked_bold(depth, y, x) && los(depth, ny, nx, y, x)) break;

        /* Occasionally advance the distance */
        if (++ctr > (4 * dis * dis + 4 * dis + 1))
        {
            ctr = 0;
            dis++;
        }
    }

    /* No usable location */
    if (!tries) return FALSE;

    /* Move the monster */
    monster_swap(depth, m_ptr->fy, m_ptr->fx, y, x);

    return TRUE;
}


/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 *
 * PWMAngband hack: try very hard to move the player away from its
 * previous location; this should ensure that a player teleporting
 * twice by mistake doesn't land again in the same trouble...
 */
bool teleport_player_aux(struct player *p, int dis, bool safe_ghost)
{
    int d, i, min, x = p->py, y = p->px;
    bool look = TRUE;

    /* Space-time anchor */
    if (check_st_anchor(p->depth, p->py, p->px))
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* No teleporting from inside pits */
    if (is_notele(p->depth, p->py, p->px))
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* Hack -- Hijack teleport in Arena */
    if (p->arena_num != -1)
    {
        i = p->arena_num;
        return teleport_player_to(p,
            arenas[i].y_1 + 1 + randint1(arenas[i].y_2 - arenas[i].y_1 - 2),
            arenas[i].x_1 + 1 + randint1(arenas[i].x_2 - arenas[i].x_1 - 2));
    }

    /* Minimum distance */
    min = dis / 2;

    /* Look until done */
    while (look)
    {
        /* Verify max distance */
        if (dis > 200) dis = 200;

        /* Try several locations */
        for (i = 0; i < 500; i++)
        {
            /* Pick a (possibly illegal) location */
            while (1)
            {
                y = rand_spread(p->py, dis);
                x = rand_spread(p->px, dis);
                d = distance(p->py, p->px, y, x);

                /* Hack -- Not too close from previous player location */
                if (distance(p->old_py, p->old_px, y, x) < min) continue;

                if ((d >= min) && (d <= dis)) break;
            }

            /* Ignore illegal locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Just require empty space if teleporting a ghost to safety */
            if (safe_ghost)
            {
                if (cave_get(p->depth)->m_idx[y][x]) continue;
            }

            /* Require "naked" floor space */
            else
            {
                if (!cave_naked_bold(p->depth, y, x)) continue;
            }

            /* No teleporting into vaults and such */
            if (is_icky(p->depth, y, x)) continue;

            /* This grid looks good */
            look = FALSE;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;
    }

    /* Sound */
    sound(p, MSG_TELEPORT);

    /* Move the player */
    monster_swap(p->depth, p->py, p->px, y, x);

    /* Handle stuff */
    handle_stuff(p);

    /* Hack -- Fix store */
    if (in_store(p)) Send_store_leave(p);

    return TRUE;
}


bool teleport_player(struct player *p, int dis)
{
    return teleport_player_aux(p, dis, FALSE);
}


/*
 * Teleport player to a grid near the given location
 *
 * This function is very obsessive about correctness.
 */
bool teleport_player_to(struct player *p, int ny, int nx)
{
    int y, x, dis = 0, ctr = 0;
    int tries = 200;

    /* Space-time anchor */
    if (check_st_anchor(p->depth, ny, nx))
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* No teleporting from inside pits */
    if (is_notele(p->depth, ny, nx))
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* Find a usable location */
    while (--tries)
    {
        /* Pick a nearby legal location */
        while (1)
        {
            y = rand_spread(ny, dis);
            x = rand_spread(nx, dis);
            if (in_bounds_fully(y, x)) break;
        }

        /* Accept "naked" floor grids... */
        /* ... but only accept grids in LOS of the caster */
        if (cave_naked_bold(p->depth, y, x) && los(p->depth, ny, nx, y, x)) break;

        /* Occasionally advance the distance */
        if (++ctr > (4 * dis * dis + 4 * dis + 1))
        {
            ctr = 0;
            dis++;
        }
    }

    /* No usable location */
    if (!tries)
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* Move the player */
    monster_swap(p->depth, p->py, p->px, y, x);

    /* Handle stuff */
    handle_stuff(p);

    /* Hack -- Fix store */
    if (in_store(p)) Send_store_leave(p);

    return TRUE;
}


/*
 * Teleport the player one level up or down (random when legal)
 *
 * Note that keeping the "players_on_depth" array correct is VERY important,
 * otherwise levels with players still on them might be destroyed, or empty
 * levels could be kept around, wasting memory.
 *
 * In the wilderness, teleport to a neighboring wilderness level.
 */
bool teleport_player_level(struct player *p)
{
    int new_world_x = p->world_x, new_world_y = p->world_y, new_depth;
    byte new_level_method;
    char *message;

    /* Space-time anchor */
    if (check_st_anchor(p->depth, p->py, p->px))
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* Arena fighters don't teleport level */
    if (p->arena_num != -1)
    {
        msg(p, "The teleporting attempt fails.");
        return FALSE;
    }

    /* If in the wilderness, teleport to a random neighboring level */
    if (p->depth < 0)
    {
        /* Get a valid neighbor */
        do
        {
            switch (randint0(4))
            {
                case DIR_NORTH:
                    message = "A gust of wind blows you north.";
                    new_world_x = p->world_x;
                    new_world_y = p->world_y + 1;
                    break;
                case DIR_EAST:
                    message = "A gust of wind blows you east.";
                    new_world_x = p->world_x + 1;
                    new_world_y = p->world_y;
                    break;
                case DIR_SOUTH:
                    message = "A gust of wind blows you south.";
                    new_world_x = p->world_x;
                    new_world_y = p->world_y - 1;
                    break;
                case DIR_WEST:
                    message = "A gust of wind blows you west.";
                    new_world_x = p->world_x - 1;
                    new_world_y = p->world_y;
                    break;
            }
            new_depth = world_index(new_world_x, new_world_y);
        }
        while (new_depth < 0 - MAX_WILD);
        new_level_method = LEVEL_OUTSIDE_RAND;
    }

    /* Go up or down a level */
    else
    {
        bool down = TRUE, up = TRUE;

        /* Only down from town or when restricted */
        if (!p->depth || (cfg_limit_stairs == 2) || OPT_P(p, birth_ironman))
            up = FALSE;

        /* Only up from bottom level or quest level when quest not completed */
        if (!quest_done(p, p->depth) || (p->depth == MAX_DEPTH - 1))
            down = FALSE;

        /* Hack -- DM redesigning the level */
        if (players_on_depth[p->depth - 1] == INHIBIT_DEPTH)
            up = FALSE;
        if (players_on_depth[p->depth + 1] == INHIBIT_DEPTH)
            down = FALSE;

        /* Random choice */
        if (up && down)
        {
            if (magik(50)) up = FALSE;
            else down = FALSE;
        }

        /* Sometimes go down */
        if (down)
        {
            message = "You sink through the floor.";
            new_depth = p->depth + 1;
        }

        /* Sometimes go up */
        else if (up)
        {
            message = "You rise up through the ceiling.";
            new_depth = p->depth - 1;
        }

        /* Failure */
        else
        {
            msg(p, "The teleporting attempt fails.");
            return FALSE;
        }

        new_level_method = LEVEL_RAND;
    }

    /* Tell the player */
    msgt(p, MSG_TPLEVEL, message);

    /* Change location */
    dungeon_change_level(p, new_depth, new_level_method);

    /* Hack -- Replace the player */
    p->world_x = new_world_x;
    p->world_y = new_world_y;

    /* Update the wilderness map */
    if (p->depth < 0) wild_set_explored(p, 0 - p->depth);

    return TRUE;
}


/*
 * Teleports 5 dungeon levels down (from max_depth)
 */
bool deep_descent(struct player *p, bool apply)
{
    int i, target_depth = p->max_depth;

    /* Special case: wilderness level */
    if (p->depth < 0)
    {
        /* Don't apply effect while in the wilderness */
        if (apply) return FALSE;

        /* Set the timer */
        msg(p, "The air around you starts to swirl...");
        msg_misc(p, " is surrounded by a swirling aura...");
        p->deep_descent = 3 + randint1(4);
        return TRUE;
    }

    /* Calculate target depth */
    for (i = 5; i > 0; i--)
    {
        if (!quest_done(p, target_depth)) break;
        if (players_on_depth[target_depth + 1] == INHIBIT_DEPTH) break;
        if (target_depth >= MAX_DEPTH - 1) break;

        target_depth++;
    }

    /* Determine the level */
    if (target_depth > p->depth)
    {
        /* Set the timer */
        if (!apply)
        {
            msg(p, "The air around you starts to swirl...");
            msg_misc(p, " is surrounded by a swirling aura...");
            p->deep_descent = 3 + randint1(4);
            return TRUE;
        }

        /* Change location */
        disturb(p, 0, 0);
        msgt(p, MSG_TPLEVEL, "The floor opens beneath you!");
        msg_misc(p, " sinks through the floor!");
        dungeon_change_level(p, target_depth, LEVEL_RAND);
        return TRUE;
    }

    /* Just print a message when unable to set the timer */
    if (!apply)
    {
        msg(p, "You sense a malevolent presence blocking passage to the levels below.");
        return FALSE;
    }

    /* Otherwise do something disastrous */
    msg(p, "You are thrown back in an explosion!");
    destroy_area(NULL, p->depth, p->py, p->px, 5, TRUE);
    return TRUE;
}


/*
 * Get a legal "multi-hued" color for drawing "spells"
 */
static byte mh_attr(void)
{
    switch (randint1(9))
    {
        case 1: return (TERM_RED);
        case 2: return (TERM_GREEN);
        case 3: return (TERM_BLUE);
        case 4: return (TERM_YELLOW);
        case 5: return (TERM_ORANGE);
        case 6: return (TERM_VIOLET);
        case 7: return (TERM_L_RED);
        case 8: return (TERM_L_GREEN);
        case 9: return (TERM_L_BLUE);
    }

    return (TERM_WHITE);
}


/*
 * Return a color to use for the bolt/ball spells
 */
byte spell_color(int type)
{
    /* Analyze */
    switch (type)
    {
        case GF_MISSILE:    return (mh_attr());
        case GF_ACID:       return (TERM_SLATE);
        case GF_ELEC:       return (TERM_BLUE);
        case GF_FIRE:       return (TERM_RED);
        case GF_COLD:       return (TERM_WHITE);
        case GF_POIS:       return (TERM_GREEN);
        case GF_HOLY_ORB:   return (TERM_L_DARK);
        case GF_MANA:       return (TERM_L_DARK);
        case GF_WATER:      return (TERM_SLATE);
        case GF_NETHER:     return (TERM_L_GREEN);
        case GF_CHAOS:      return (mh_attr());
        case GF_DISEN:      return (TERM_VIOLET);
        case GF_NEXUS:      return (TERM_L_RED);
        case GF_SOUND:      return (TERM_YELLOW);
        case GF_SHARD:      return (TERM_UMBER);
        case GF_FORCE:      return (TERM_UMBER);
        case GF_INERT:      return (TERM_L_WHITE);
        case GF_GRAVITY:    return (TERM_L_WHITE);
        case GF_TIME:       return (TERM_L_BLUE);
        case GF_LIGHT_WEAK: return (TERM_ORANGE);
        case GF_LIGHT:      return (TERM_ORANGE);
        case GF_DARK_WEAK:  return (TERM_L_DARK);
        case GF_DARK:       return (TERM_L_DARK);
        case GF_PLASMA:     return (TERM_RED);
        case GF_METEOR:     return (TERM_RED);
        case GF_ICE:        return (TERM_WHITE);
        case GF_DEATH:      return (TERM_L_DARK);
    }

    /* Standard "color" */
    return (TERM_WHITE);
}


/*
 * Decreases players hit points and sets death flag if necessary
 */
bool take_hit(struct player *p, int damage, const char *hit_from, bool non_physical)
{
    int old_chp = p->chp;
    int warning = (p->mhp * p->other.hitpoint_warn / 10);
    bool death_spell = (!strcmp(p->died_from, "the spell of Undead Form") ||
        !strcmp(p->died_from, "the Death spell"));
    int old_num = get_player_num(p);

    /* Paranoia */
    if (p->is_dead) return TRUE;

    /* Become aware of player's presence */
    if (p->k_idx) aware_player(p, p);

    /* Disturb */
    if (strcmp(hit_from, "fading")) disturb(p, 1, 0);

    /* Hack -- Apply "invulnerability" */
    if (p->timed[TMD_INVULN] == -1)
    {
        /* Permanent invulnerability */
        damage = 0;
    }
    else if (p->timed[TMD_INVULN] && non_physical)
    {
        /* Globe of invulnerability protects against non-physical attacks only */
        damage -= damage * p->lev / 100;
    }

    /* Disruption shield: damage is substracted from mana first */
    if (p->timed[TMD_MANASHIELD] && (p->csp > 0))
    {
        /* Disruption shield fully absorbed the damage */
        if (p->csp > damage)
        {
            /* Substract from mana and set to zero */
            p->csp -= damage;
            damage = 0;
        }

        /* Disruption shield partially absorbed the damage */
        else
        {
            damage -= p->csp;
            p->csp = 0;
            p->csp_frac = 0;

            /* No more mana shield... */
            player_clear_timed(p, TMD_MANASHIELD, TRUE);
        }

        /* Display the spellpoints */
        p->redraw |= (PR_MANA);
    }

    /* Hurt the player */
    p->chp -= damage;

    /* Hack -- Redraw picture */
    redraw_picture(p, old_num);

    /* Display the hitpoints */
    p->redraw |= (PR_HP);

    /* Dead player */
    if (p->chp < 0)
    {
        /* Note cause of death */
        my_strcpy(p->died_from, hit_from, sizeof(p->died_from));

        /* Record the original (pre-ghost) cause of death */
        if (!p->ghost) player_death_info(p, hit_from);

        /* No longer a winner (except when using the spells of Undead Form or Death) */
        if (!death_spell) p->total_winner = FALSE;

        /* Note death */
        p->is_dead = TRUE;

        /* Dead */
        return TRUE;
    }

    /* Hitpoint warning */
    if (warning && (p->chp <= warning))
    {
        /* Message (only the first time) */
        if (old_chp > warning)
        {
            msgt(p, MSG_HITPOINT_WARN, "*** LOW HITPOINT WARNING! ***");
            message_flush(p);
        }
    }

    /* Alive */
    return FALSE;
}


/*
 * Destroys a type of item on a given percent chance.
 * The chance 'cperc' is in hundredths of a percent (1-in-10000)
 * Note that missiles are no longer necessarily all destroyed
 *
 * Returns number of items destroyed.
 */
int inven_damage(struct player *p, int type, int cperc)
{
    const struct gf_type *gf_ptr = &gf_table[type];
    int i, j, k, amt;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    bool damage;
    bitflag f[OF_SIZE];

    /* Count the casualties */
    k = 0;

    /* Scan through the slots backwards */
    for (i = QUIVER_END - 1; i >= 0; i--)
    {
        if ((i >= INVEN_PACK) && (i < QUIVER_START)) continue;

        /* Get the item in that slot */
        o_ptr = &p->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Hack -- for now, skip artifacts */
        if (o_ptr->artifact) continue;

        of_wipe(f);
        object_flags(o_ptr, f);

        /* Give this item slot a shot at death if it is vulnerable */
        if (of_has(f, gf_ptr->obj_hates) && !of_has(f, gf_ptr->obj_imm))
        {
            /* Chance to destroy this item */
            int chance = cperc;

            /* Track if it is damaged instead of destroyed */
            damage = FALSE;

            /*
             * Analyze the type to see if we just damage it
             * We also check for rods to reduce chance
             */
            switch (o_ptr->tval)
            {
                /* Weapons */
                case TV_BOW:
                case TV_SWORD:
                case TV_HAFTED:
                case TV_POLEARM:
                {
                    /* Chance to damage it */
                    if (CHANCE(cperc, 10000))
                    {
                        /* Damage the item */
                        o_ptr->to_h--;
                        o_ptr->to_d--;

                        /* Damaged! */
                        damage = TRUE;
                    }
                    else continue;

                    break;
                }

                /* Wearable items */
                case TV_HELM:
                case TV_CROWN:
                case TV_SHIELD:
                case TV_BOOTS:
                case TV_GLOVES:
                case TV_CLOAK:
                case TV_SOFT_ARMOR:
                case TV_HARD_ARMOR:
                case TV_DRAG_ARMOR:
                {
                    /* Chance to damage it */
                    if (CHANCE(cperc, 10000))
                    {
                        /* Damage the item */
                        o_ptr->to_a--;

                        /* Damaged! */
                        damage = TRUE;
                    }
                    else continue;

                    break;
                }

                /* Rods are tough */
                case TV_ROD:
                {
                    chance = (chance / 4);

                    break;
                }
            }

            /* Damage instead of destroy */
            if (damage)
            {
                p->update |= (PU_BONUS);
                p->redraw |= (PR_EQUIP);

                /* Casualty count */
                amt = o_ptr->number;
            }

            /* ... or count the casualties */
            else for (amt = j = 0; j < o_ptr->number; ++j)
            {
                if (CHANCE(chance, 10000)) amt++;
            }

            /* Some casualties */
            if (amt)
            {
                /* Get a description */
                object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

                /* Message */
                msgt(p, MSG_DESTROY, "%sour %s (%c) %s %s!",
                    ((o_ptr->number > 1) ? ((amt == o_ptr->number) ? "All of y" :
                    (amt > 1 ? "Some of y" : "One of y")) : "Y"), o_name,
                    index_to_label(i), ((amt > 1) ? "were" : "was"),
                    (damage? "damaged": "destroyed"));

                /* Damage already done? */
                if (damage) continue;

                /* Reduce charges if some devices are destroyed */
                reduce_charges(o_ptr, amt);

                /* Destroy "amt" items */
                inven_item_increase(p, i, 0 - amt);
                inven_item_optimize(p, i);

                /* Count the casualties */
                k += amt;
            }
        }
    }

    /* Return the casualty count */
    return (k);
}


/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(struct player *p)
{
    object_type *o_ptr = NULL;
    bitflag f[OF_SIZE];
    char o_name[NORMAL_WID];

    /* Pick a (possibly empty) inventory slot */
    switch (randint1(6))
    {
        case 1: o_ptr = &p->inventory[INVEN_BODY]; break;
        case 2: o_ptr = &p->inventory[INVEN_ARM]; break;
        case 3: o_ptr = &p->inventory[INVEN_OUTER]; break;
        case 4: o_ptr = &p->inventory[INVEN_HANDS]; break;
        case 5: o_ptr = &p->inventory[INVEN_HEAD]; break;
        case 6: o_ptr = &p->inventory[INVEN_FEET]; break;
    }

    /* Nothing to damage */
    if (!o_ptr->kind) return (FALSE);

    /* No damage left to be done */
    if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);

    /* Describe */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

    /* Extract the flags */
    object_flags(o_ptr, f);

    /* Object resists */
    if (of_has(f, OF_IGNORE_ACID))
    {
        msg(p, "Your %s is unaffected!", o_name);

        return (TRUE);
    }

    /* Message */
    msg(p, "Your %s is damaged!", o_name);

    /* Damage the item */
    o_ptr->to_a--;

    p->update |= (PU_BONUS);
    p->redraw |= (PR_EQUIP);

    /* Item was damaged */
    return (TRUE);
}


/*
 * Adjust damage according to resistance or vulnerability.
 *
 * type is the attack type we are checking.
 * dam is the unadjusted damage.
 * dam_aspect is the calc we want (min, avg, max, random).
 * resist is the degree of resistance (-1 = vuln, 3 = immune).
 */
int adjust_dam(struct player *p, int type, int dam, aspect dam_aspect, int resist)
{
    const struct gf_type *gf_ptr = &gf_table[type];
    int i, denom;

    if (dam <= 0) return 0;

    /* Immune */
    if (resist == 3) return 0;

    /* Hack -- Acid damage is halved by armour, holy orb is halved */
    if (((type == GF_ACID) && p && minus_ac(p)) || (type == GF_HOLY_ORB))
        dam = (dam + 1) / 2;

    /* Hack -- Biofeedback halves "sharp" damage */
    if (p && p->timed[TMD_BIOFEEDBACK])
    {
        switch (type)
        {
            case GF_MISSILE:
            case GF_ARROW_X:
            case GF_ARROW_1:
            case GF_ARROW_2:
            case GF_ARROW_3:
            case GF_ARROW_4:
            case GF_BOULDER:
            case GF_SHARD:
            case GF_SOUND:
                dam = (dam + 1) / 2;
                break;
        }
    }

    /* Handle polymorphed players */
    if (p && p->r_idx)
    {
        monster_race *r_ptr = &r_info[p->r_idx];

        if ((type == GF_FIRE) && rf_has(r_ptr->flags, RF_HURT_FIRE)) dam = dam * 4 / 3;
        if ((type == GF_COLD) && rf_has(r_ptr->flags, RF_HURT_COLD)) dam = dam * 4 / 3;
        if ((type == GF_ICE) && rf_has(r_ptr->flags, RF_HURT_COLD)) dam = dam * 4 / 3;
        if ((type == GF_LIGHT) && rf_has(r_ptr->flags, RF_HURT_LIGHT)) dam = dam * 4 / 3;
        if ((type == GF_LIGHT_WEAK) && !rf_has(r_ptr->flags, RF_HURT_LIGHT)) dam = 0;
    }

    /* Vulnerable */
    if (resist == -1) return (dam * 4 / 3);

    /*
     * Variable resists vary the denominator, so we need to invert the logic
     * of dam_aspect. (m_bonus is unused)
     */
    switch (dam_aspect)
    {
        case MINIMISE:
            denom = randcalc(gf_ptr->denom, 0, MAXIMISE);
            break;
        case MAXIMISE:
            denom = randcalc(gf_ptr->denom, 0, MINIMISE);
            break;
        default:
            denom = randcalc(gf_ptr->denom, 0, dam_aspect);
    }

    for (i = resist; i > 0; i--)
    {
        if (denom) dam = dam * gf_ptr->num / denom;
    }

    return dam;
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(struct player *p, int stat)
{
    /* Restore if needed */
    if (p->stat_cur[stat] != p->stat_max[stat])
    {
        /* Restore */
        p->stat_cur[stat] = p->stat_max[stat];

        /* Recalculate bonuses */
        p->update |= (PU_BONUS);

        /* Success */
        return (TRUE);
    }

    /* Nothing to restore */
    return (FALSE);
}


/*
 * Apply disenchantment to the player's stuff
 *
 * This function is also called from the "melee" code
 *
 * The "mode" is currently unused.
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(struct player *p, int mode)
{
    int t = 0;
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Pick a random slot */
    switch (randint1(8))
    {
        case 1: t = INVEN_WIELD; break;
        case 2: t = INVEN_BOW; break;
        case 3: t = INVEN_BODY; break;
        case 4: t = INVEN_OUTER; break;
        case 5: t = INVEN_ARM; break;
        case 6: t = INVEN_HEAD; break;
        case 7: t = INVEN_HANDS; break;
        case 8: t = INVEN_FEET; break;
    }

    /* Get the item */
    o_ptr = &p->inventory[t];

    /* No item, nothing happens */
    if (!o_ptr->kind) return (FALSE);

    /* Nothing to disenchant */
    if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0))
    {
        /* Nothing to notice */
        return (FALSE);
    }

    /* Describe the object */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

    /* Artifacts have 60% chance to resist */
    if (o_ptr->artifact && magik(60))
    {
        /* Message */
        msg(p, "Your %s (%c) resist%s disenchantment!",
            o_name, index_to_label(t), SINGULAR(o_ptr->number));

        /* Notice */
        return (TRUE);
    }

    /* Apply disenchantment, depending on which kind of equipment */
    if ((t == INVEN_WIELD) || (t == INVEN_BOW))
    {
        /* Disenchant to-hit */
        if (o_ptr->to_h > 0) o_ptr->to_h--;
        if ((o_ptr->to_h > 5) && magik(20)) o_ptr->to_h--;

        /* Disenchant to-dam */
        if (o_ptr->to_d > 0) o_ptr->to_d--;
        if ((o_ptr->to_d > 5) && magik(20)) o_ptr->to_d--;
    }
    else
    {
        /* Disenchant to-ac */
        if (o_ptr->to_a > 0) o_ptr->to_a--;
        if ((o_ptr->to_a > 5) && magik(20)) o_ptr->to_a--;
    }

    /* Message */
    msg(p, "Your %s (%c) %s disenchanted!", o_name, index_to_label(t),
        ((o_ptr->number != 1)? "were": "was"));

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    /* Redraw */
    p->redraw |= (PR_EQUIP);

    /* Notice */
    return (TRUE);
}


void poly_bat(struct player *p, int chance, char *killer)
{
    char buf[MSG_LEN];
    s16b race_fruit_bat = get_r_idx("Fruit bat");

    /* Not in fruit bat mode! */
    if (OPT_P(p, birth_fruit_bat))
    {
        msg(p, "Nothing happens.");
        return;
    }

    if (p->r_idx != race_fruit_bat)
    {
        /* Attempt a saving throw */
        if (p->ghost || player_has(p, PF_DRAGON) || CHANCE(p->state.skills[SKILL_SAVE], chance))
            msg(p, "You resist the effects!");
        else
        {
            char desc[NORMAL_WID];

            my_strcpy(desc, p->name, sizeof(desc));
            my_strcap(desc);

            /* Turned into a fruit bat */
            if (killer)
                strnfmt(buf, sizeof(buf), "%s was turned into a fruit bat by %s!", desc, killer);
            else
                strnfmt(buf, sizeof(buf), "%s was turned into a fruit bat!", desc);
            msg_broadcast(p, buf);
            do_cmd_poly(p, race_fruit_bat, FALSE, TRUE);
        }
    }
    else
    {
        /* No saving throw for being restored... */
        do_cmd_poly(p, 0, FALSE, TRUE);
    }
}


bool poly_race(struct player *p, int r_idx)
{
    monster_race *r_ptr;

    /* Restrict */
    if (p->ghost || player_has(p, PF_DRAGON) || OPT_P(p, birth_fruit_bat) || (p->r_idx == r_idx))
    {
        msg(p, "Nothing happens.");
        return FALSE;
    }

    /* Restrict if too powerful */
    r_ptr = &r_info[r_idx];
    if (p->lev < r_ptr->level / 2)
    {
        msg(p, "Nothing happens.");
        return FALSE;
    }

    /* Non-Shapechangers get a huge penalty for using Rings of Polymorphing */
    if (!player_has(p, PF_MONSTER_SPELLS))
    {
        msg(p, "Your nerves and muscles feel weak and lifeless!");
        take_hit(p, damroll(10, 10), "the strain of polymorphing", FALSE);
        player_stat_dec(p, A_DEX, TRUE);
        player_stat_dec(p, A_WIS, TRUE);
        player_stat_dec(p, A_CON, TRUE);
        player_stat_dec(p, A_STR, TRUE);
        player_stat_dec(p, A_CHR, TRUE);
        player_stat_dec(p, A_INT, TRUE);

        /* Fail if too powerful */
        if (magik(r_ptr->level)) return TRUE;
    }

    do_cmd_poly(p, r_idx, FALSE, TRUE);

    return TRUE;
}


/*
 * Hack -- Track "affected" monsters
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;


/*
 * We are called from "project()" to "damage" terrain features
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 * Hack -- We also "see" grids which are "memorized".
 *
 * Perhaps we should affect doors and/or walls.
 */
static bool project_f(int who, int r, int depth, int y, int x, int dam, int typ, bool obvious)
{
    player_type *p_ptr = player_get(0 - who);
    bool line_sight = FALSE;
    bool line_sound = FALSE;
    bool is_blind = FALSE;

    /* Set the player info */
    if (p_ptr)
    {
        line_sight = player_has_los_bold(p_ptr, y, x);
        line_sound = ((p_ptr->cave->info[y][x] & CAVE_MARK)? TRUE: FALSE);
        is_blind = p_ptr->timed[TMD_BLIND];
    }

    /* Analyze the type */
    switch (typ)
    {
        /* Ignore most effects */
        case GF_ACID:
        case GF_ELEC:
        case GF_COLD:
        case GF_PLASMA:
        case GF_METEOR:
        case GF_ICE:
        case GF_SHARD:
        case GF_FORCE:
        case GF_SOUND:
        case GF_MANA:
        case GF_HOLY_ORB: break;

        /* Place a wall */
        case GF_STONE_WALL:
        {
            /* Require a "naked" floor grid */
            if (!cave_naked_bold(depth, y, x)) break;

            /* Check line of sight */
            if (line_sight) obvious = TRUE;

            /* Place a wall */
            cave_set_feat(cave_get(depth), y, x, FEAT_WALL_EXTRA);

            /* Update the visuals */
            update_visuals(depth);

            break;
        }

        /* Burn trees and grass */
        case GF_FIRE:
        {
            /* No effect on special levels */
            if (check_special_level(depth)) break;

            /* Burn trees */
            if (cave_get(depth)->feat[y][x] == FEAT_TREE)
            {
                /* Check line of sight */
                if (line_sight)
                {
                    msg(p_ptr, "The tree burns!");
                    obvious = TRUE;
                }

                /* Burn the tree */
                cave_set_feat(cave_get(depth), y, x, FEAT_EVIL_TREE);
            }

            /* Destroy trees */
            else if (cave_get(depth)->feat[y][x] == FEAT_EVIL_TREE)
            {
                /* Check line of sight */
                if (line_sight)
                {
                    msg(p_ptr, "The tree burns to the ground!");
                    obvious = TRUE;
                }

                /* Destroy the tree */
                if (!depth) trees_in_town--;
                cave_set_feat(cave_get(depth), y, x, FEAT_DIRT);
            }

            /* Burn grass */
            if (cave_get(depth)->feat[y][x] == FEAT_GRASS)
            {
                /* Check line of sight */
                if (line_sight) obvious = TRUE;

                /* Destroy the grass */
                cave_set_feat(cave_get(depth), y, x, FEAT_DIRT);
            }

            break;
        }

        /* Destroy Traps (and Locks) */
        case GF_KILL_TRAP:
        {
            /* Reveal secret doors */
            if (cave_issecretdoor(cave_get(depth), y, x))
            {
                place_closed_door(cave_get(depth), y, x);

                /* Check line of sight */
                if (line_sight) obvious = TRUE;
            }

            /* Destroy traps */
            if (cave_istrap(cave_get(depth), y, x))
            {
                /* Check line of sight */
                if (line_sight)
                {
                    msg(p_ptr, "There is a bright flash of light!");
                    obvious = TRUE;
                }

                /* Forget the trap */
                forget_spot(depth, y, x);

                /* Destroy the trap */
                cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
            }

            /* Locked doors are unlocked */
            else if ((cave_get(depth)->feat[y][x] >= FEAT_DOOR_HEAD + 0x01) &&
                (cave_get(depth)->feat[y][x] <= FEAT_DOOR_HEAD + 0x07))
            {
                /* Unlock the door */
                cave_set_feat(cave_get(depth), y, x, FEAT_DOOR_HEAD);

                /* Check line of sight */
                if (line_sight)
                {
                    msg(p_ptr, "Click!");
                    obvious = TRUE;
                }
            }

            break;
        }

        /* Destroy Doors (and traps) */
        case GF_KILL_DOOR:
        {
            /* Destroy all doors and traps */
            if (cave_isopendoor(cave_get(depth), y, x) ||
                (cave_get(depth)->feat[y][x] == FEAT_BROKEN) ||
                cave_istrap(cave_get(depth), y, x) ||
                cave_iscloseddoor(cave_get(depth), y, x))
            {
                /* Check line of sight */
                if (line_sight)
                {
                    msg(p_ptr, "There is a bright flash of light!");
                    obvious = TRUE;
                }

                /* Visibility change */
                if (cave_iscloseddoor(cave_get(depth), y, x))
                {
                    int i;

                    /* Check everyone */
                    for (i = 1; i < NumPlayers + 1; i++)
                    {
                        player_type *q_ptr = player_get(i);

                        /* If he's not here, skip him */
                        if (q_ptr->depth != depth) continue;

                        /* Check line of sight */
                        if (!player_has_los_bold(q_ptr, y, x)) continue;

                        /* Update the visuals */
                        q_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
                    }
                }

                /* Forget the door */
                forget_spot(depth, y, x);

                /* Destroy the feature */
                cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
            }

            break;
        }

        /* Destroy walls (and doors) */
        case GF_KILL_WALL:
        {
            /* No effect on special levels */
            if (check_special_level(depth)) break;

            /* Non-walls (etc) */
            if (cave_floor_bold(depth, y, x)) break;

            /* Permanent walls */
            if (cave_isperm(cave_get(depth), y, x) ||
                (cave_get(depth)->feat[y][x] == FEAT_PERM_CLEAR))
            {
                break;
            }

            /* Granite */
            if (cave_isrock(cave_get(depth), y, x))
            {
                /* Message */
                if (line_sound)
                {
                    msg(p_ptr, "The wall turns into mud!");
                    obvious = TRUE;
                }

                /* Forget the wall */
                forget_spot(depth, y, x);

                /* Destroy the wall */
                if (depth > 0)
                    cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
                else
                    cave_set_feat(cave_get(depth), y, x, FEAT_MUD);
            }

            /* Quartz / Magma with treasure */
            else if ((cave_get(depth)->feat[y][x] >= FEAT_MAGMA_H) &&
                (cave_get(depth)->feat[y][x] <= FEAT_QUARTZ_K))
            {
                /* Message */
                if (line_sound)
                {
                    msg(p_ptr, "The vein turns into mud!");
                    msg(p_ptr, "You have found something!");
                    obvious = TRUE;
                }

                /* Forget the wall */
                forget_spot(depth, y, x);

                /* Destroy the wall */
                if (depth > 0)
                    cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
                else
                    cave_set_feat(cave_get(depth), y, x, FEAT_MUD);

                /* Place some gold */
                place_gold(p_ptr, cave_get(depth), y, x, object_level(depth), SV_GOLD_ANY,
                    ORIGIN_FLOOR);
            }

            /* Quartz / Magma */
            else if ((cave_get(depth)->feat[y][x] >= FEAT_MAGMA) &&
                (cave_get(depth)->feat[y][x] <= FEAT_QUARTZ))
            {
                /* Message */
                if (line_sound)
                {
                    msg(p_ptr, "The vein turns into mud!");
                    obvious = TRUE;
                }

                /* Forget the wall */
                forget_spot(depth, y, x);

                /* Destroy the wall */
                if (depth > 0)
                    cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
                else
                    cave_set_feat(cave_get(depth), y, x, FEAT_MUD);
            }

            /* Rubble */
            else if (cave_isrubble(cave_get(depth), y, x))
            {
                /* Message */
                if (line_sound)
                {
                    msg(p_ptr, "The rubble turns into mud!");
                    obvious = TRUE;
                }

                /* Forget the wall */
                forget_spot(depth, y, x);

                /* Destroy the rubble */
                if (depth > 0)
                    cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
                else
                    cave_set_feat(cave_get(depth), y, x, FEAT_MUD);

                /* Hack -- place an object */
                if (magik(10))
                {
                    /* Found something */
                    if (line_sight)
                    {
                        msg(p_ptr, "There was something buried in the rubble!");
                        obvious = TRUE;
                    }

                    /* Place object */
                    place_object(p_ptr, cave_get(depth), y, x, object_level(depth), FALSE,
                        FALSE, ORIGIN_RUBBLE, 0);
                }
            }

            /* House doors are immune */
            else if (cave_ishomedoor(cave_get(depth), y, x))
            {
                /* Message */
                if (line_sound)
                {
                    msg(p_ptr, "The door resists.");
                    obvious = TRUE;
                }
            }

            /* Destroy doors (and secret doors) */
            else if (cave_iscloseddoor(cave_get(depth), y, x) ||
                cave_issecretdoor(cave_get(depth), y, x))
            {
                /* Hack -- Special message */
                if (line_sound)
                {
                    msg(p_ptr, "The door crumbles to dust!");
                    obvious = TRUE;
                }

                /* Forget the wall */
                forget_spot(depth, y, x);

                /* Destroy the rubble */
                if (depth > 0)
                    cave_set_feat(cave_get(depth), y, x, FEAT_FLOOR);
                else
                    cave_set_feat(cave_get(depth), y, x, FEAT_DIRT);
            }

            /* Update the visuals */
            update_visuals(depth);

            /* Fully update the flow */
            fully_update_flow(depth);

            break;
        }

        /* Make doors */
        case GF_MAKE_DOOR:
        {
            /* Require a grid without monsters */
            if (cave_get(depth)->m_idx[y][x]) break;

            /* Require a floor grid */
            if (!cave_isanyfloor(cave_get(depth), y, x)) break;

            /* Push objects off the grid */
            if (cave_get(depth)->o_idx[y][x]) push_object(p_ptr, y, x);

            /* Create a closed door */
            cave_set_feat(cave_get(depth), y, x, FEAT_DOOR_HEAD);

            /* Observe */
            if (line_sound) obvious = TRUE;

            /* Update the visuals */
            update_visuals(depth);

            break;
        }

        /* Make traps */
        case GF_MAKE_TRAP:
        {
            /* Require an "empty" floor grid */
            if (!cave_isopen(cave_get(depth), y, x)) break;

            /* Create a trap */
            create_trap(cave_get(depth), y, x);

            break;
        }

        /* Light up the grid */
        case GF_LIGHT_WEAK:
        case GF_LIGHT:
        {
            int i;

            /* Turn on the light */
            cave_get(depth)->info[y][x] |= CAVE_GLOW;

            /* Grid is in line of sight */
            if (line_sight && !is_blind) obvious = TRUE;

            /* Check everyone */
            for (i = 1; i < NumPlayers + 1; i++)
            {
                player_type *q_ptr = player_get(i);

                /* If he's not here, skip him */
                if (q_ptr->depth != depth) continue;

                /* Check line of sight */
                if (!player_has_los_bold(q_ptr, y, x)) continue;

                /* Fully update the visuals */
                q_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
            }

            break;
        }

        /* Darken the grid */
        case GF_DARK_WEAK:
        case GF_DARK:
        {
            int i;

            /* No effect outside of the dungeon during day */
            if ((depth <= 0) && is_daytime()) break;

            /* No effect on special levels */
            if (check_special_level(depth)) break;

            /* Turn off the light. */
            cave_get(depth)->info[y][x] &= ~CAVE_GLOW;

            /* Hack -- Forget "boring" grids */
            if (cave_floor_basic(cave_get(depth)->feat[y][x]))
                forget_spot(depth, y, x);

            /* Grid is in line of sight */
            if (line_sight) obvious = TRUE;

            /* Check everyone */
            for (i = 1; i < NumPlayers + 1; i++)
            {
                player_type *q_ptr = player_get(i);

                /* If he's not here, skip him */
                if (q_ptr->depth != depth) continue;

                /* Check line of sight */
                if (!player_has_los_bold(q_ptr, y, x)) continue;

                /* Fully update the visuals */
                q_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
            }

            /* All done */
            break;
        }
    }

    /* Return "Anything seen?" */
    return (obvious);
}


s16b get_r_idx(const char *name)
{
    s16b r_idx;

    for (r_idx = 1; r_idx < z_info->r_max; r_idx++)
    {
        monster_race *r_ptr = &r_info[r_idx];
        if (!r_ptr->name) continue;
        if (!strcmp(r_ptr->name, name)) return r_idx;
    }
    return -1;
}


/*
 * We are called from "project()" to "damage" objects
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * Perhaps we should only SOMETIMES damage things on the ground.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * Hack -- We also "see" objects which are "memorized".
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int r, int depth, int y, int x, int dam, int typ, bool obvious)
{
    player_type *p_ptr = player_get(0 - who);
    s16b this_o_idx, next_o_idx = 0;
    bitflag f[OF_SIZE];
    char o_name[NORMAL_WID];
    monster_type *who_ptr = NULL;

    /* Set the monster pointer */
    if (who > 0) who_ptr = cave_monster(cave_get(depth), who);

    /* Scan all objects in the grid */
    for (this_o_idx = cave_get(depth)->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;
        bool is_art = FALSE;
        bool ignore = FALSE;
        bool plural = FALSE;
        bool do_kill = FALSE;
        const char *note_kill = NULL;
        bool observed = FALSE;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        if (who < 0)
            observed = (object_marked(p_ptr, this_o_idx) && !squelch_item_ok(p_ptr, o_ptr));

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Extract the flags */
        object_flags(o_ptr, f);

        /* Get the "plural"-ness */
        if (o_ptr->number > 1) plural = TRUE;

        /* Check for artifact */
        if (o_ptr->artifact) is_art = TRUE;

        /* Analyze the type */
        switch (typ)
        {
            /* Identify */
            case GF_IDENTIFY:
            {
                /* Identify it fully */
                if (who < 0) object_notice_everything(p_ptr, o_ptr, FALSE);
                obvious = TRUE;
                break;
            }

            /* Raise dead */
            case GF_RAISE:
            {
                monster_race *r_ptr;
                s16b r_idx = -1;

                /* Skeletons can be raised */
                if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval >= SV_SKELETON_KOBOLD))
                {
                    /* Choose new animated skeleton */
                    switch (o_ptr->sval)
                    {
                        case SV_SKELETON_KOBOLD:
                            r_idx = get_r_idx("Skeleton kobold");
                            break;
                        case SV_SKELETON_ORC:
                            r_idx = get_r_idx("Skeleton orc");
                            break;
                        case SV_SKELETON_HUMAN:
                            r_idx = get_r_idx("Skeleton human");
                            break;
                        case SV_SKELETON_SKULL:
                            r_idx = get_r_idx("Flying skull");
                            break;
                        case SV_SKELETON_TROLL:
                            r_idx = get_r_idx("Skeleton troll");
                            break;
                        case SV_SKELETON_ETTIN:
                            r_idx = get_r_idx("Skeleton ettin");
                            break;
                    }
                    r_ptr = &r_info[r_idx];

                    /* Handle PWMAngband base monsters (Flying skull) */
                    if (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
                        r_idx = -1;
                }

                /* Humanoid corpses can be raised too */
                else if ((o_ptr->tval == TV_CORPSE) && (o_ptr->sval == SV_CORPSE_HUMAN))
                {
                    /* Choose new animated corpse */
                    if (o_ptr->pval[DEFAULT_PVAL + 1] <= o_ptr->timeout / 5)
                    {
                        /* Rotten corpses only produce... Rotting corpses :) */
                        r_idx = get_r_idx("Rotting corpse");
                        r_ptr = &r_info[r_idx];

                        /* Handle PWMAngband base monsters (Rotting corpse) */
                        if (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
                            r_idx = -1;
                    }
                    else do
                    {
                        r_idx = randint1(z_info->r_max - 1);
                        r_ptr = &r_info[r_idx];

                        if (!r_ptr->name) r_idx = -1;

                        /* Animated corpses can be any of non unique z, W or L */
                        if ((r_ptr->base != lookup_monster_base("zombie")) &&
                            (r_ptr->base != lookup_monster_base("wraith")) &&
                            (r_ptr->base != lookup_monster_base("lich")))
                        {
                            r_idx = -1;
                        }
                        if (rf_has(r_ptr->flags, RF_UNIQUE)) r_idx = -1;

                        /* Handle PWMAngband base monsters */
                        if (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
                            r_idx = -1;

                        /* Handle PWMAngband extra monsters */
                        if (rf_has(r_ptr->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters)
                            r_idx = -1;
                    }
                    while (r_idx == -1);
                }

                /* Check monster */
                if (r_idx != -1)
                {
                    /* Raise dead prohibited in town and special levels */
                    if (forbid_special(depth)) break;

                    /* Raised monsters can't be too powerful */
                    if (who < 0)
                    {
                        /* Check player level */
                        if (p_ptr->lev < 3 * r_ptr->level / 4) break;
                    }
                    else
                    {
                        /* Check monster level */
                        if (who_ptr->level < r_ptr->level) break;
                    }
                    if (monster_level(depth) < r_ptr->level / 2) break;

                    /* Raising dead costs mana */
                    if ((who < 0) && (r_ptr->level > (p_ptr->csp - p_ptr->spell_cost)))
                        break;

                    /* Place a new monster */
                    if (place_new_monster(p_ptr, cave_get(depth), y, x, r_idx, 0, ORIGIN_DROP_SUMMON))
                    {
                        do_kill = TRUE;

                        /* Handle monsters raised by the player */
                        if (who < 0)
                        {
                            monster_type *m_ptr;

                            msg(p_ptr, "A monster rises from the grave!");

                            /* Hack -- Get new monster */
                            m_ptr = cave_monster_at(cave_get(depth), y, x);

                            /* Raised monsters are mostly neutral */
                            if (magik(80)) monster_set_master(m_ptr, p_ptr, MSTATUS_GUARD);

                            /* Use some mana */
                            p_ptr->spell_cost += r_ptr->level;
                        }
                    }
                }

                break;
            }

            /* Acid -- Lots of things */
            case GF_ACID:
            {
                if (of_has(f, OF_HATES_ACID))
                {
                    do_kill = TRUE;
                    note_kill = (plural ? " melt!" : " melts!");
                    if (of_has(f, OF_IGNORE_ACID)) ignore = TRUE;
                }
                break;
            }

            /* Elec -- Rings and Wands */
            case GF_ELEC:
            {
                if (of_has(f, OF_HATES_ELEC))
                {
                    do_kill = TRUE;
                    note_kill = (plural ? " are destroyed!" : " is destroyed!");
                    if (of_has(f, OF_IGNORE_ELEC)) ignore = TRUE;
                }
                break;
            }

            /* Fire -- Flammable objects */
            case GF_FIRE:
            {
                if (of_has(f, OF_HATES_FIRE))
                {
                    do_kill = TRUE;
                    note_kill = (plural ? " burn up!" : " burns up!");
                    if (of_has(f, OF_IGNORE_FIRE)) ignore = TRUE;
                }
                break;
            }

            /* Cold -- potions and flasks */
            case GF_COLD:
            {
                if (of_has(f, OF_HATES_COLD))
                {
                    note_kill = (plural ? " shatter!" : " shatters!");
                    do_kill = TRUE;
                    if (of_has(f, OF_IGNORE_COLD)) ignore = TRUE;
                }
                break;
            }

            /* Fire + Elec */
            case GF_PLASMA:
            {
                if (of_has(f, OF_HATES_FIRE))
                {
                    do_kill = TRUE;
                    note_kill = (plural ? " burn up!" : " burns up!");
                    if (of_has(f, OF_IGNORE_FIRE)) ignore = TRUE;
                }
                if (of_has(f, OF_HATES_ELEC))
                {
                    ignore = FALSE;
                    do_kill = TRUE;
                    note_kill = (plural ? " are destroyed!" : " is destroyed!");
                    if (of_has(f, OF_IGNORE_ELEC)) ignore = TRUE;
                }
                break;
            }

            /* Fire + Cold */
            case GF_METEOR:
            {
                if (of_has(f, OF_HATES_FIRE))
                {
                    do_kill = TRUE;
                    note_kill = (plural ? " burn up!" : " burns up!");
                    if (of_has(f, OF_IGNORE_FIRE)) ignore = TRUE;
                }
                if (of_has(f, OF_HATES_COLD))
                {
                    ignore = FALSE;
                    do_kill = TRUE;
                    note_kill = (plural ? " shatter!" : " shatters!");
                    if (of_has(f, OF_IGNORE_COLD)) ignore = TRUE;
                }
                break;
            }

            /* Hack -- break potions and such */
            case GF_ICE:
            case GF_SHARD:
            case GF_FORCE:
            case GF_SOUND:
            {
                if (of_has(f, OF_HATES_COLD))
                {
                    note_kill = (plural ? " shatter!" : " shatters!");
                    do_kill = TRUE;
                }
                break;
            }

            /* Mana -- destroys everything */
            case GF_MANA:
            {
                do_kill = TRUE;
                note_kill = (plural ? " are destroyed!" : " is destroyed!");
                break;
            }

            /* Holy Orb -- destroys cursed non-artifacts */
            case GF_HOLY_ORB:
            {
                if (cursed_p(o_ptr->flags))
                {
                    do_kill = TRUE;
                    note_kill = (plural ? " are destroyed!" : " is destroyed!");
                }
                break;
            }

            /* Unlock chests */
            case GF_KILL_TRAP:
            case GF_KILL_DOOR:
            {
                /* Chests are noticed only if trapped or locked */
                if (is_locked_chest(o_ptr))
                {
                    /* Disarm or Unlock */
                    unlock_chest(o_ptr);

                    /* Identify */
                    if (who < 0) object_notice_everything(p_ptr, o_ptr, TRUE);

                    /* Notice */
                    if (observed)
                    {
                        msg(p_ptr, "Click!");
                        obvious = TRUE;
                    }
                }

                break;
            }
        }

        /* Attempt to destroy the object */
        if (do_kill)
        {
            /* Effect "observed" */
            if (observed)
            {
                obvious = TRUE;
                object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
            }

            /* Artifacts, and other objects, get to resist */
            if (is_art || ignore)
            {
                /* Observe the resist */
                if (observed)
                    msg(p_ptr, "The %s %s unaffected!", o_name, (plural? "are": "is"));
            }

            /* Kill it */
            else
            {
                /* Describe if needed */
                if (observed && note_kill)
                    msgt(p_ptr, MSG_DESTROY, "The %s%s", o_name, note_kill);

                /* Delete the object */
                delete_object_idx(this_o_idx);

                /* Redraw */
                cave_light_spot(cave_get(depth), y, x);
            }
        }
    }

    /* Return "Anything seen?" */
    return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to a monster.
 *
 * This routine takes a "source monster" (by index) which is mostly used to
 * determine if the player is causing the damage, and a "radius" (see below),
 * which is used to decrease the power of explosions with distance, and a
 * location, via integers which are modified by certain types of attacks
 * (polymorph and teleport being the obvious ones), a default damage, which
 * is modified as needed based on various properties, and finally a "damage
 * type" (see below).
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.  Note that the "damage" parameter is
 * divided by the radius, so monsters not at the "epicenter" will not take
 * as much damage (or whatever)...
 *
 * Note that "polymorph" is dangerous, since a failure in "place_monster()"'
 * may result in a dereference of an invalid pointer.  XXX XXX XXX
 *
 * Various messages are produced, and damage is applied.
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" big balls of it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.  If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint1(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint1(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint1(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint1(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint1(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "obvious" happen.
 */
static bool project_m(int who, int r, int depth, int y, int x, int dam, int typ, bool obvious)
{
    player_type *p_ptr = player_get(0 - who);
    monster_type *m_ptr;
    monster_race *r_ptr;
    monster_lore *l_ptr = NULL;
    u16b flag = MON_TMD_FLG_NOTIFY;
    bool do_gravity = FALSE;
    bool do_delete = FALSE;

    /* Is the monster "seen"? */
    bool seen = FALSE;

    /* Were the effects "irrelevant"? */
    bool skipped = FALSE;

    /* Did the monster die? */
    bool mon_died = FALSE;

    /* Polymorph setting (true or false) */
    int do_poly = 0;

    /* Teleport setting (max distance) */
    int do_dist = 0;

    /* Confusion setting (amount to confuse) */
    int do_conf = 0;

    /* Stunning setting (amount to stun) */
    int do_stun = 0;

    /* Slow setting (amount to slow) */
    int do_slow = 0;

    /* Haste setting (amount to haste) */
    int do_haste = 0;

    /* Sleep amount (amount to sleep) */
    bool do_sleep = FALSE;

    /* Fear amount (amount to fear) */
    int do_fear = 0;

    /* Are we trying to id the source of this effect? */
    bool id = ((who < 0)? !obvious: FALSE);

    /* Blindness amount (amount to blind) */
    int do_blind = 0;

    /* Hold the monster name */
    char m_name[NORMAL_WID];
    char m_poss[NORMAL_WID];

    int m_idx = cave_get(depth)->m_idx[y][x];

    /* Assume no note */
    int m_note = MON_MSG_NONE;

    /* Assume a default death */
    byte note_dies = MON_MSG_DIE;

    /* Walls protect monsters */
    if (!cave_floor_bold(depth, y, x)) return FALSE;

    /* No monster here */
    if (m_idx <= 0) return FALSE;

    /* Never affect projector */
    if (m_idx == who) return FALSE;

    /* Obtain monster info */
    m_ptr = cave_monster(cave_get(depth), m_idx);
    r_ptr = &r_info[m_ptr->r_idx];

    /* Obtain player info */
    if (who < 0)
    {
        l_ptr = &p_ptr->lore[m_ptr->r_idx];
        seen = p_ptr->mon_vis[m_idx];
    }

    /* Reduce damage by distance */
    dam = (dam + r) / (r + 1);

    /* Get the monster name (BEFORE polymorphing) */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(p_ptr, m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);

    /* Some monsters get "destroyed" */
    if (monster_is_unusual(r_ptr))
    {
        /* Special note at death */
        note_dies = MON_MSG_DESTROYED;
    }

    /* The caster is a player */
    if (p_ptr)
    {
        bool threat = FALSE;

        /* Is this type of attack a threat? */
        switch (typ)
        {
            case GF_ELEC: /* IM_XXX only strongly resist */
            case GF_POIS:
            case GF_ACID:
            case GF_COLD:
            case GF_FIRE:
            case GF_MISSILE: /* no resistance */
            case GF_ARROW_X:
            case GF_ARROW_1:
            case GF_ARROW_2:
            case GF_ARROW_3:
            case GF_ARROW_4:
            case GF_BOULDER:
            case GF_PLASMA: /* RES_PLAS only resist */
            case GF_HOLY_ORB: /* no resistance */
            case GF_LIGHT: /* BR_LIGHT only resist */
            case GF_DARK: /* BR_DARK, ORC, HURT_LIGHT only resist */
            case GF_SHARD: /* BR_SHAR only resist */
            case GF_SOUND: /* BR_SOUN only resist */
            case GF_FORCE: /* BR_WALL only resist */
            case GF_INERT: /* BR_INER only resist */
            case GF_MANA: /* BR_MANA only resist */
            case GF_METEOR:
            case GF_ICE: /* IM_COLD only strongly resist */
            case GF_CHAOS: /* BR_CHAO only resist */
            case GF_DISEN: /* RES_DISE only resist */
            case GF_NEXUS: /* RES_NEXUS only resist */
            case GF_TIME: /* BR_TIME only resist */
            case GF_GRAVITY: /* BR_GRAV only resist */
            case GF_DISP_ALL: /* no resistance */
            case GF_CURSE:
            case GF_CURSE2:
                threat = TRUE;
                break;
            case GF_WATER: /* IM_WATER are immune */
                if (!rf_has(r_ptr->flags, RF_IM_WATER)) threat = TRUE;
                break;
            case GF_LIGHT_WEAK: /* all except HURT_LIGHT are immune */
                if (rf_has(r_ptr->flags, RF_HURT_LIGHT)) threat = TRUE;
                break;
            case GF_DARK_WEAK: /* irrelevant */
            case GF_KILL_DOOR:
            case GF_KILL_TRAP:
            case GF_MAKE_DOOR:
            case GF_MAKE_TRAP:
            case GF_OLD_CLONE: /* no real damage */
            case GF_OLD_POLY:
            case GF_OLD_HEAL:
            case GF_OLD_SPEED:
            case GF_OLD_SLOW:
            case GF_OLD_CONF:
            case GF_BLIND:
            case GF_FORGET:
            case GF_OLD_SLEEP:
            case GF_AWAY_EVIL:
            case GF_AWAY_ALL:
            case GF_TURN_UNDEAD:
            case GF_TURN_ALL:
            case GF_STONE_WALL: /* irrelevant */
            case GF_MAGE_PROJECT:
            case GF_PRIEST_PROJECT:
            case GF_SORC_PROJECT:
            case GF_SHAD_PROJECT:
            case GF_HUNT_PROJECT:
            case GF_PSI_PROJECT:
            case GF_DEATH_PROJECT:
            case GF_GHOST_PROJECT:
            case GF_MIMIC_PROJECT:
            case GF_ELEM_PROJECT:
            case GF_SUMMON_PROJECT:
            case GF_IDENTIFY:
            case GF_STUN: /* no real damage */
            case GF_RAISE: /* irrelevant */
            case GF_GUARD: /* no real damage */
            case GF_FOLLOW:
            case GF_TELE_TO:
            case GF_TELE_LEVEL:
            case GF_DRAIN_MANA:
            case GF_ATTACK:
            case GF_CONTROL:
                break;
            case GF_NETHER: /* UNDEAD are immune */
                if (!rf_has(r_ptr->flags, RF_UNDEAD)) threat = TRUE;
                break;
            case GF_KILL_WALL: /* all except HURT_ROCK are immune */
                if (rf_has(r_ptr->flags, RF_HURT_ROCK)) threat = TRUE;
                break;
            case GF_OLD_DRAIN: /* UNDEAD, DEMON, 'E', 'g', 'v' are immune */
            case GF_DRAIN:
            case GF_PSI_DRAIN:
                if (!monster_is_nonliving(r_ptr)) threat = TRUE;
                break;
            case GF_DISP_UNDEAD: /* all except UNDEAD are immune */
                if (rf_has(r_ptr->flags, RF_UNDEAD)) threat = TRUE;
                break;
            case GF_DISP_EVIL: /* all except EVIL are immune */
                if (rf_has(r_ptr->flags, RF_EVIL)) threat = TRUE;
                break;
            case GF_PSI: /* EMPTY_MIND are immune */
            case GF_BLAST:
            case GF_SMASH:
                if (!rf_has(r_ptr->flags, RF_EMPTY_MIND)) threat = TRUE;
                break;
            case GF_DEATH: /* UNIQUE are immune */
                if (!rf_has(r_ptr->flags, RF_UNIQUE)) threat = TRUE;
                break;
        }

        /* Check hostility for threatening spells */
        if (threat && !pvm_check(0 - who, m_idx)) return FALSE;
    }

    /* Analyze the damage type */
    switch (typ)
    {
        /* Magic Missile -- pure damage */
        case GF_MISSILE:
        {
            if (seen) obvious = TRUE;

            break;
        }

        /* Acid */
        case GF_ACID:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_IM_ACID);
            if (rf_has(r_ptr->flags, RF_IM_ACID))
            {
                m_note = MON_MSG_RESIST_A_LOT;
                dam /= 9;
            }

            break;
        }

        /* Electricity */
        case GF_ELEC:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_IM_ELEC);
            if (rf_has(r_ptr->flags, RF_IM_ELEC))
            {
                m_note = MON_MSG_RESIST_A_LOT;
                dam /= 9;
            }

            break;
        }

        /* Fire damage */
        case GF_FIRE:
        {
            if (seen) obvious = TRUE;
            if (seen)
            {
                rf_on(l_ptr->flags, RF_IM_FIRE);
                rf_on(l_ptr->flags, RF_HURT_FIRE);
            }
            if (rf_has(r_ptr->flags, RF_IM_FIRE))
            {
                m_note = MON_MSG_RESIST_A_LOT;
                dam /= 9;
            }
            else if (rf_has(r_ptr->flags, RF_HURT_FIRE))
            {
                m_note = MON_MSG_CATCH_FIRE;
                note_dies = MON_MSG_DISENTEGRATES;
                dam *= 2;
            }

            break;
        }

        /* Cold */
        case GF_COLD:

        /* Ice -- Cold + Cuts + Stun */
        case GF_ICE:
        {
            if (seen)
            {
                obvious = TRUE;
                rf_on(l_ptr->flags, RF_IM_COLD);
                rf_on(l_ptr->flags, RF_HURT_COLD);
            }

            if (typ == GF_ICE)
            {
                if (who > 0)
                {
                    do_stun = (randint1(15) + 1) / (r + 1);
                    flag |= MON_TMD_MON_SOURCE;
                }
                else
                    do_stun = (randint1(15) + r + p_ptr->lev / 5) / (r + 1);

                if (rsf_has(r_ptr->spell_flags, RSF_BR_SHAR))
                {
                    /* Learn about breathers through resistance */
                    if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_SHAR);
                }
                else if (magik(50))
                {
                    add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_ICE, FALSE);

                    /* Apply bleeding */
                    mon_inc_timed(p_ptr, m_ptr, MON_TMD_CUT, damroll(5, 8), flag, id);
                }
            }

            if (rf_has(r_ptr->flags, RF_IM_COLD))
            {
                m_note = MON_MSG_RESIST_A_LOT;
                dam /= 9;
            }
            else if (rf_has(r_ptr->flags, RF_HURT_COLD))
            {
                m_note = MON_MSG_BADLY_FROZEN;
                note_dies = MON_MSG_FREEZE_SHATTER;
                dam *= 2;
            }

            break;
        }

        /* Poison */
        case GF_POIS:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_IM_POIS);
            if (rf_has(r_ptr->flags, RF_IM_POIS))
            {
                m_note = MON_MSG_RESIST_A_LOT;
                dam /= 9;
            }
            else if (magik(50))
            {
                /* Apply poison */
                mon_inc_timed(p_ptr, m_ptr, MON_TMD_POIS, randint0(dam) + 10, flag, id);
            }

            break;
        }

        /* Holy Orb -- hurts Evil */
        case GF_HOLY_ORB:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_EVIL);
            if (rf_has(r_ptr->flags, RF_EVIL))
            {
                dam *= 2;
                m_note = MON_MSG_HIT_HARD;
            }

            break;
        }

        /* "Missile" spells */
        case GF_ARROW_X:
        case GF_ARROW_1:
        case GF_ARROW_2:
        case GF_ARROW_3:
        case GF_ARROW_4:
        case GF_BOULDER:
        {
            if (seen) obvious = TRUE;

            break;
        }

        /* Plasma */
        case GF_PLASMA:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_RES_PLAS);

            /* Immune to stunning */
            if (rf_has(r_ptr->flags, RF_RES_PLAS))
            {
                if (seen) rf_on(l_ptr->flags, RF_NO_STUN);
            }
            else if (who > 0)
            {
                do_stun = (10 + randint1(15) + r) / (r + 1);
                flag |= MON_TMD_MON_SOURCE;
            }
            else
                do_stun = (10 + randint1(15) + r + p_ptr->lev / 5) / (r + 1);

            if (rf_has(r_ptr->flags, RF_RES_PLAS))
            {
                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Nether -- see above */
        case GF_NETHER:
        {
            if (seen) obvious = TRUE;

            /* Update the lore */
            if (seen)
            {
                /* Acquire knowledge of undead type and nether resistance */
                rf_on(l_ptr->flags, RF_UNDEAD);
                rf_on(l_ptr->flags, RF_RES_NETH);

                /* If it isn't undead, acquire extra knowledge */
                if (!rf_has(r_ptr->flags, RF_UNDEAD))
                {
                    /* Learn this creature breathes nether if true */
                    if (rsf_has(r_ptr->spell_flags, RSF_BR_NETH))
                        rsf_on(l_ptr->spell_flags, RSF_BR_NETH);

                    /* Otherwise learn about evil type */
                    else
                        rf_on(l_ptr->flags, RF_EVIL);
                }
            }

            if (rf_has(r_ptr->flags, RF_UNDEAD))
            {
                m_note = MON_MSG_IMMUNE;
                dam = 0;
            }
            else if (rf_has(r_ptr->flags, RF_RES_NETH) ||
                rsf_has(r_ptr->spell_flags, RSF_BR_NETH))
            {
                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }
            else if (rf_has(r_ptr->flags, RF_EVIL))
            {
                dam /= 2;
                m_note = MON_MSG_RESIST_SOMEWHAT;
            }

            break;
        }

        /* Water damage */
        case GF_WATER:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_IM_WATER);

            /* Immune to stunning and confusion */
            if (rf_has(r_ptr->flags, RF_IM_WATER))
            {
                if (seen)
                {
                    rf_on(l_ptr->flags, RF_NO_STUN);
                    rf_on(l_ptr->flags, RF_NO_CONF);
                }
            }
            else if (who > 0)
            {
                do_stun = (10 + randint1(15) + r) / (r + 1);
                do_conf = (5 + randint1(11) + r) / (r + 1);
                flag |= MON_TMD_MON_SOURCE;
            }
            else
            {
                do_stun = (10 + randint1(15) + r + p_ptr->lev / 5) / (r + 1);
                do_conf = (5 + randint1(11) + r + p_ptr->lev / 5) / (r + 1);
            }

            if (rf_has(r_ptr->flags, RF_IM_WATER))
            {
                m_note = MON_MSG_IMMUNE;
                dam = 0;
            }

            break;
        }

        /* Chaos -- Chaos breathers resist */
        case GF_CHAOS:
        {
            if (seen) obvious = TRUE;

            do_poly = TRUE;

            /* Forbid in the town and on special levels */
            if (forbid_special(depth)) do_poly = FALSE;

            if (who > 0)
            {
                do_conf = (5 + randint1(11) + r) / (r + 1);
                flag |= MON_TMD_MON_SOURCE;
            }
            else
                do_conf = (5 + randint1(11) + r + p_ptr->lev / 5) / (r + 1);

            if (rsf_has(r_ptr->spell_flags, RSF_BR_CHAO))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_CHAO);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
                do_poly = FALSE;
            }

            break;
        }

        /* Shards -- Shard breathers resist */
        case GF_SHARD:
        {
            if (seen) obvious = TRUE;

            if (rsf_has(r_ptr->spell_flags, RSF_BR_SHAR))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_SHAR);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }
            else if (magik(50))
            {
                /* Apply bleeding */
                if (dam) mon_inc_timed(p_ptr, m_ptr, MON_TMD_CUT, dam, flag, id);
            }

            break;
        }

        /* Sound -- Sound breathers resist */
        case GF_SOUND:
        {
            if (seen) obvious = TRUE;

            if (who > 0)
            {
                do_stun = (10 + randint1(15) + r) / (r + 1);
                flag |= MON_TMD_MON_SOURCE;
            }
            else
                do_stun = (10 + randint1(15) + r + p_ptr->lev / 5) / (r + 1);

            if (rsf_has(r_ptr->spell_flags, RSF_BR_SOUN))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_SOUN);

                m_note = MON_MSG_RESIST;
                dam *= 2; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Disenchantment */
        case GF_DISEN:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_RES_DISE);
            if (rf_has(r_ptr->flags, RF_RES_DISE))
            {
                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Nexus */
        case GF_NEXUS:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_RES_NEXUS);
            if (rf_has(r_ptr->flags, RF_RES_NEXUS))
            {
                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Force */
        case GF_FORCE:
        {
            if (seen) obvious = TRUE;

            if (who > 0)
            {
                do_stun = (randint1(15) + r) / (r + 1);
                flag |= MON_TMD_MON_SOURCE;
            }
            else
                do_stun = (randint1(15) + r + p_ptr->lev / 5) / (r + 1);

            if (rsf_has(r_ptr->spell_flags, RSF_BR_WALL))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_WALL);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Inertia -- breathers resist */
        case GF_INERT:
        {
            if (seen) obvious = TRUE;

            if (rsf_has(r_ptr->spell_flags, RSF_BR_INER))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_INER);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            /* Slow effect */
            else do_slow = dam;

            break;
        }

        /* Time -- breathers resist */
        case GF_TIME:
        {
            if (seen) obvious = TRUE;
            if (rsf_has(r_ptr->spell_flags, RSF_BR_TIME))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_TIME);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Gravity -- breathers resist */
        case GF_GRAVITY:
        {
            if (seen) obvious = TRUE;
            do_dist = 10;

            /* Higher level monsters can resist the teleportation better */
            if (CHANCE(m_ptr->level, MAX_DEPTH)) do_dist = 0;

            if (rsf_has(r_ptr->spell_flags, RSF_BR_GRAV))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_GRAV);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
                do_dist = 0;
            }

            /* Gravity effect */
            else do_gravity = TRUE;

            break;
        }

        /* Mana -- breathers resist */
        case GF_MANA:
        {
            if (seen) obvious = TRUE;
            if (rsf_has(r_ptr->spell_flags, RSF_BR_MANA))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_MANA);

                m_note = MON_MSG_RESIST;
                dam *= 3; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Meteor -- powerful magic missile */
        case GF_METEOR:
        {
            if (seen) obvious = TRUE;

            break;
        }

        /* Drain Life */
        case GF_OLD_DRAIN:
        {
            if (seen) obvious = TRUE;
            if (seen)
            {
                rf_on(l_ptr->flags, RF_UNDEAD);
                rf_on(l_ptr->flags, RF_DEMON);
            }
            if (monster_is_nonliving(r_ptr))
            {
                m_note = MON_MSG_UNAFFECTED;
                obvious = FALSE;
                dam = 0;
            }

            break;
        }

        /* Polymorph monster (Use "dam" as "power") */
        case GF_OLD_POLY:
        {
            /* Polymorph later */
            do_poly = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Clone monsters (Ignore "dam") */
        case GF_OLD_CLONE:
        {
            if (seen) obvious = TRUE;

            /* Heal fully */
            m_ptr->hp = m_ptr->maxhp;

            /* Speed up */
            mon_inc_timed(p_ptr, m_ptr, MON_TMD_FAST, 50, MON_TMD_FLG_NOTIFY, id);

            /* Attempt to clone */
            if (clone_mon(0 - who, depth, m_idx)) m_note = MON_MSG_SPAWN;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Heal Monster (use "dam" as amount of healing) */
        case GF_OLD_HEAL:
        {
            if (seen) obvious = TRUE;

            /* Wake up */
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, id);

            /* Heal */
            m_ptr->hp += dam;

            /* No overflow */
            if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

            /* Redraw (later) if needed */
            update_health(m_idx);

            /* Message */
            m_note = MON_MSG_HEALTHIER;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Speed Monster (Ignore "dam") */
        case GF_OLD_SPEED:
        {
            if (seen) obvious = TRUE;

            /* Speed up */
            do_haste = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Slow Monster (Use "dam" as "power") */
        case GF_OLD_SLOW:
        {
            /* Get slowed later */
            do_slow = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Sleep (Use "dam" as "power") */
        case GF_OLD_SLEEP:
        {
            /* Go to sleep later */
            do_sleep = TRUE;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Blindness (Use "dam" as "power") */
        case GF_BLIND:
        {
            /* Get blinded later */
            do_blind = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Confusion (Use "dam" as "power") */
        case GF_OLD_CONF:
        case GF_FORGET:
        {
            /* Get confused later */
            do_conf = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Light, but only hurts susceptible creatures */
        case GF_LIGHT_WEAK:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_HURT_LIGHT);

            /* Hurt by light */
            if (rf_has(r_ptr->flags, RF_HURT_LIGHT))
            {
                /* Special effect */
                m_note = MON_MSG_CRINGE_LIGHT;
                note_dies = MON_MSG_SHRIVEL_LIGHT;
            }

            /* Normally no damage */
            else
            {
                /* No damage */
                dam = 0;
            }

            break;
        }

        /* Light -- opposite of Dark */
        case GF_LIGHT:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_HURT_LIGHT);

            if (rsf_has(r_ptr->spell_flags, RSF_BR_LIGHT))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_LIGHT);

                m_note = MON_MSG_RESIST;
                dam *= 2; dam /= (randint1(6) + 6);
            }
            else if (rf_has(r_ptr->flags, RF_HURT_LIGHT))
            {
                m_note = MON_MSG_CRINGE_LIGHT;
                note_dies = MON_MSG_SHRIVEL_LIGHT;
                dam *= 2;
            }

            break;
        }

        /* Dark -- opposite of Light */
        case GF_DARK:
        {
            if (seen) obvious = TRUE;
            if (rsf_has(r_ptr->spell_flags, RSF_BR_DARK))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_DARK);

                m_note = MON_MSG_RESIST;
                dam *= 2; dam /= (randint1(6) + 6);
            }

            break;
        }

        /* Stone to Mud */
        case GF_KILL_WALL:
        {
            if (seen) obvious = TRUE;
            if (seen) rf_on(l_ptr->flags, RF_HURT_ROCK);

            /* Hurt by rock remover */
            if (rf_has(r_ptr->flags, RF_HURT_ROCK))
            {
                /* Cute little message */
                m_note = MON_MSG_LOSE_SKIN;
                note_dies = MON_MSG_DISSOLVE;
            }

            /* Usually, ignore the effects */
            else
            {
                /* No damage */
                dam = 0;
            }

            break;
        }

        /* Teleport evil (Use "dam" as "power") */
        case GF_AWAY_EVIL:
        {
            if (seen) rf_on(l_ptr->flags, RF_EVIL);

            /* Only affect evil */
            if (rf_has(r_ptr->flags, RF_EVIL))
            {
                if (seen) obvious = TRUE;
                do_dist = dam;
            }

            /* Others ignore */
            else
            {
                /* Irrelevant */
                skipped = TRUE;
            }

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Teleport monster (Use "dam" as "power") */
        case GF_AWAY_ALL:
        {
            /* Obvious */
            if (seen) obvious = TRUE;

            /* Prepare to teleport */
            do_dist = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Turn undead (Use "dam" as "power") */
        case GF_TURN_UNDEAD:
        {
            /* Only affect undead */
            if (rf_has(r_ptr->flags, RF_UNDEAD))
            {
                /* Obvious */
                if (seen) obvious = TRUE;

                /* Apply some fear */
                do_fear = dam;
            }
            else
                skipped = TRUE;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Turn monster (Use "dam" as "power") */
        case GF_TURN_ALL:
        {
            /* Get frightened later */
            do_fear = dam;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Dispel undead */
        case GF_DISP_UNDEAD:
        {
            if (seen) rf_on(l_ptr->flags, RF_UNDEAD);

            /* Only affect undead */
            if (rf_has(r_ptr->flags, RF_UNDEAD))
            {
                /* Obvious */
                if (seen) obvious = TRUE;

                /* Message */
                m_note = MON_MSG_SHUDDER;
                note_dies = MON_MSG_DISSOLVE;
            }

            /* Ignore other monsters */
            else
            {
                /* Irrelevant */
                skipped = TRUE;

                /* No damage */
                dam = 0;
            }

            break;
        }

        /* Dispel evil */
        case GF_DISP_EVIL:
        {
            if (seen) rf_on(l_ptr->flags, RF_EVIL);

            /* Only affect evil */
            if (rf_has(r_ptr->flags, RF_EVIL))
            {
                /* Obvious */
                if (seen) obvious = TRUE;

                /* Message */
                m_note = MON_MSG_SHUDDER;
                note_dies = MON_MSG_DISSOLVE;
            }

            /* Ignore other monsters */
            else
            {
                /* Irrelevant */
                skipped = TRUE;

                /* No damage */
                dam = 0;
            }

            break;
        }

        /* Dispel monster */
        case GF_DISP_ALL:
        {
            /* Obvious */
            if (seen) obvious = TRUE;

            /* Message */
            m_note = MON_MSG_SHUDDER;
            note_dies = MON_MSG_DISSOLVE;

            break;
        }

        /* Stunning (Use "dam" as "power") */
        case GF_STUN:
        {
            /* Get stunned later */
            do_stun = damroll(3, dam / 2) + 1;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Mind Blast */
        case GF_PSI:
        case GF_BLAST:
        case GF_SMASH:
        {
            bool effect = magik(50);
            if (seen) obvious = TRUE;

            if (rf_has(r_ptr->flags, RF_EMPTY_MIND))
            {
                dam = 0;
                m_note = MON_MSG_IMMUNE;
                obvious = FALSE;
            }
            else if (rf_has(r_ptr->flags, RF_STUPID) || rf_has(r_ptr->flags, RF_WEIRD_MIND) ||
                rf_has(r_ptr->flags, RF_ANIMAL) || (m_ptr->level > randint1(3 * dam)))
            {
                dam /= 3;
                m_note = MON_MSG_RESIST;
                effect = magik(25);
            }
            if ((dam > 0) && effect)
            {
                /* Mind Blast (psi spell): random conf/stun/fear/sleep effect */
                if (typ == GF_PSI)
                {
                    switch (randint1(4))
                    {
                        case 1: do_conf = randint0(4) + 4; break;
                        case 2: do_stun = randint0(4) + 4; break;
                        case 3: do_fear = randint0(4) + 4; break;
                        default: do_sleep = TRUE; break;
                    }
                }

                /* Mind Blast (monster spell): conf effect */
                else if (typ == GF_BLAST)
                    do_conf = randint0(4) + 4;

                /* Brain Smash: random blind/conf/sleep/slow effect */
                else
                {
                    switch (randint1(4))
                    {
                        case 1: do_blind = 8 + randint0(8); break;
                        case 2: do_conf = randint0(4) + 4; break;
                        case 3: do_sleep = TRUE; break;
                        default: do_slow = dam; break;
                    }
                }
            }
            note_dies = MON_MSG_DROP_DEAD;

            break;
        }

        /* Drain Life + replenish HP */
        case GF_DRAIN:
        {
            if (seen) obvious = TRUE;
            if (seen)
            {
                rf_on(l_ptr->flags, RF_UNDEAD);
                rf_on(l_ptr->flags, RF_DEMON);
            }
            if (monster_is_nonliving(r_ptr))
            {
                m_note = MON_MSG_UNAFFECTED;
                obvious = FALSE;
                dam = 0;
            }
            else
            {
                int drain = dam;

                if (drain > m_ptr->hp) drain = m_ptr->hp;
                hp_player_safe(p_ptr, 1 + drain / 2);
            }

            break;
        }

        /* Drain Life + replenish SP */
        case GF_PSI_DRAIN:
        {
            if (seen) obvious = TRUE;
            if (seen)
            {
                rf_on(l_ptr->flags, RF_UNDEAD);
                rf_on(l_ptr->flags, RF_DEMON);
            }
            if (monster_is_nonliving(r_ptr))
            {
                m_note = MON_MSG_UNAFFECTED;
                obvious = FALSE;
                dam = 0;
            }
            else
            {
                int drain = dam;

                if (drain > m_ptr->hp) drain = m_ptr->hp;
                sp_player(p_ptr, 1 + drain / 2);
            }

            break;
        }

        /* Death -- instant death */
        case GF_DEATH:
        {
            if (seen) obvious = TRUE;

            /* Unique monsters resist */
            /* You don't think you gonna kill Morgoth this way, do you? :) */
            if (rf_has(r_ptr->flags, RF_UNIQUE))
            {
                m_note = MON_MSG_UNAFFECTED;
                dam = 0;
                obvious = FALSE;
            }

            /* Normal monsters die instantly */
            else
                dam = 30000;

            break;
        }

        /* Order undead monsters to stay still (Use "dam" as "power") */
        case GF_GUARD:
        {
            if (seen) rf_on(l_ptr->flags, RF_UNDEAD);

            /* Only if the undead monster is not already under the spell */
            if (!((m_ptr->master == p_ptr->id) && (m_ptr->status == MSTATUS_GUARD)) &&
                rf_has(r_ptr->flags, RF_UNDEAD))
            {
                /* Obvious */
                if (seen) obvious = TRUE;

                /* Attempt a saving throw */
                if (rf_has(r_ptr->flags, RF_UNIQUE) ||
                    CHANCE(m_ptr->level - 10, (dam < 11)? 1: (dam - 10)))
                {
                    /* No obvious effect */
                    m_note = MON_MSG_UNAFFECTED;
                    obvious = FALSE;
                }
                else if (check_state(p_ptr, OF_AGGRAVATE))
                {
                    /* Too enraged to be controlled */
                    m_note = MON_MSG_HATE;
                    obvious = FALSE;
                }
                else
                {
                    /* Order monster to stay still */
                    m_note = MON_MSG_REACT;
                    monster_set_master(m_ptr, p_ptr, MSTATUS_GUARD);
                }
            }

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Order undead monsters to follow (Use "dam" as "power") */
        case GF_FOLLOW:
        {
            if (seen) rf_on(l_ptr->flags, RF_UNDEAD);

            /* Only if the undead monster is not already under the spell */
            if (!((m_ptr->master == p_ptr->id) && (m_ptr->status == MSTATUS_FOLLOW)) &&
                rf_has(r_ptr->flags, RF_UNDEAD))
            {
                /* Obvious */
                if (seen) obvious = TRUE;

                /* Attempt a saving throw */
                if (rf_has(r_ptr->flags, RF_UNIQUE) ||
                    CHANCE(m_ptr->level - 10, (dam < 11)? 1: (dam - 10)))
                {
                    /* No obvious effect */
                    m_note = MON_MSG_UNAFFECTED;
                    obvious = FALSE;
                }
                else if (check_state(p_ptr, OF_AGGRAVATE))
                {
                    /* Too enraged to be controlled */
                    m_note = MON_MSG_HATE;
                    obvious = FALSE;
                }
                else
                {
                    /* Order monster to follow */
                    m_note = MON_MSG_REACT;
                    monster_set_master(m_ptr, p_ptr, MSTATUS_FOLLOW);
                }
            }

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Teleport monster to player */
        case GF_TELE_TO:
        {
            /* Obvious */
            if (seen) obvious = TRUE;

            /* Teleport */
            if (teleport_to(depth, m_idx, p_ptr->py, p_ptr->px))
                m_note = MON_MSG_RETURN;
            else
                obvious = FALSE;

            /* Hack -- get new location */
            y = m_ptr->fy;
            x = m_ptr->fx;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Teleport monster to another level */
        case GF_TELE_LEVEL:
        {
            /* Obvious */
            if (seen) obvious = TRUE;

            /* Unique monsters resist */
            if (rf_has(r_ptr->flags, RF_UNIQUE))
            {
                m_note = MON_MSG_UNAFFECTED;
                obvious = FALSE;
            }

            /* Normal monsters are banished */
            else
            {
                m_note = MON_MSG_DISAPPEAR;
                do_delete = TRUE;
            }

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Curse -- pure damage */
        case GF_CURSE:
        {
            if (seen) obvious = TRUE;

            break;
        }

        /* Heavy curse -- damage + cuts */
        case GF_CURSE2:
        {
            if (seen) obvious = TRUE;

            /* Cut effect */
            if (rsf_has(r_ptr->spell_flags, RSF_BR_SHAR))
            {
                /* Learn about breathers through resistance */
                if (seen) rsf_on(l_ptr->spell_flags, RSF_BR_SHAR);
            }
            else if (magik(50))
            {
                /* Apply bleeding */
                mon_inc_timed(p_ptr, m_ptr, MON_TMD_CUT, CAUSE_4_CUT, flag, id);
            }

            break;
        }

        /* Drain mana (Use "dam" as "power") */
        case GF_DRAIN_MANA:
        {
            if (seen) obvious = TRUE;

            /* Affects only casters */
            if (r_ptr->freq_spell)
            {
                int r1;

                /* Attack power, capped vs monster level */
                r1 = (randint1(dam) / 2) + 1;
                if (r1 > (m_ptr->level / 6) + 1) r1 = (m_ptr->level / 6) + 1;

                /* Heal player */
                hp_player(p_ptr, r1 * 6);
            }
            else
            {
                /* No obvious effect */
                m_note = MON_MSG_UNAFFECTED;
                obvious = FALSE;
            }

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Order undead monsters to attack (Use "dam" as "power") */
        case GF_ATTACK:
        {
            if (seen) rf_on(l_ptr->flags, RF_UNDEAD);

            /* Only if the undead monster is not already under the spell */
            if (!((m_ptr->master == p_ptr->id) && (m_ptr->status == MSTATUS_ATTACK)) &&
                rf_has(r_ptr->flags, RF_UNDEAD))
            {
                /* Obvious */
                if (seen) obvious = TRUE;

                /* Attempt a saving throw */
                if (rf_has(r_ptr->flags, RF_UNIQUE) ||
                    CHANCE(m_ptr->level - 10, (dam < 11)? 1: (dam - 10)))
                {
                    /* No obvious effect */
                    m_note = MON_MSG_UNAFFECTED;
                    obvious = FALSE;
                }
                else if (check_state(p_ptr, OF_AGGRAVATE))
                {
                    /* Too enraged to be controlled */
                    m_note = MON_MSG_HATE;
                    obvious = FALSE;
                }
                else
                {
                    /* Order monster to attack */
                    m_note = MON_MSG_REACT;
                    monster_set_master(m_ptr, p_ptr, MSTATUS_ATTACK);
                }
            }

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Order summoned monsters to attack */
        case GF_CONTROL:
        {
            /* Obvious */
            if (seen) obvious = TRUE;

            /* Try to charm the monster */
            m_note = charm_monster(m_ptr, p_ptr, dam);

            /* No obvious effect */
            if (m_note != MON_MSG_REACT) obvious = FALSE;

            /* No "real" damage */
            dam = 0;

            break;
        }

        /* Default */
        default:
        {
            /* Irrelevant */
            skipped = TRUE;

            /* No damage */
            dam = 0;

            break;
        }
    }

    /* Absolutely no effect */
    if (skipped) return FALSE;

    /* Check for death */
    if (dam > m_ptr->hp)
    {
        /* Extract method of death */
        m_note = note_dies;
    }

    /* Handle polymorph */
    else if (do_poly)
    {
        /* Default -- assume no polymorph */
        m_note = MON_MSG_UNAFFECTED;

        /* Uniques cannot be polymorphed */
        if (!rf_has(r_ptr->flags, RF_UNIQUE))
        {
            if (seen) obvious = TRUE;

            /* Saving throws are allowed */
            if (CHANCE(m_ptr->level, 90) || ((typ == GF_OLD_POLY) &&
                CHANCE(m_ptr->level - 10, (dam < 11)? 1: (dam - 10))))
            {
                if (typ == GF_OLD_POLY) m_note = MON_MSG_MAINTAIN_SHAPE;
            }
            else
            {
                /* Pick a "new" monster race */
                int tmp = poly_r_idx(depth, m_ptr->r_idx);

                /* Handle polymorph */
                if (tmp != m_ptr->r_idx)
                {
                    /* Monster polymorphs */
                    m_note = MON_MSG_CHANGE;

                    /* Add the message now before changing the monster race */
                    add_monster_message(p_ptr, m_name, m_ptr, m_note, FALSE);

                    /* No more messages */
                    m_note = MON_MSG_NONE;

                    /* Turn off the damage */
                    dam = 0;

                    /* "Kill" the "old" monster */
                    delete_monster_idx(cave_get(depth), m_idx);

                    /* Create a new monster (no groups) */
                    place_new_monster(p_ptr, cave_get(depth), y, x, tmp, 0, ORIGIN_DROP_POLY);

                    /* Hack -- Assume success XXX XXX XXX */

                    /* Hack -- Get new monster */
                    m_idx = cave_get(depth)->m_idx[y][x];
                    m_ptr = cave_monster(cave_get(depth), m_idx);

                    /* Hack -- Get new race */
                    r_ptr = &r_info[m_ptr->r_idx];
                }
            }
        }
    }

    /* Handle gravity effect */
    else if (do_gravity)
    {
        /* Message */
        add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_TORN, FALSE);

        /* Obvious */
        if (seen) obvious = TRUE;

        /* Teleport */
        if (do_dist) teleport_away(m_ptr, do_dist);

        /* Hack -- Get new location */
        y = m_ptr->fy;
        x = m_ptr->fx;

        if (who > 0)
        {
            do_stun = (10 + randint1(15) + r) / (r + 1);
            flag |= MON_TMD_MON_SOURCE;
        }
        else
            do_stun = (10 + randint1(15) + r + p_ptr->lev / 5) / (r + 1);

        if (m_ptr->m_timed[MON_TMD_STUN]) do_stun /= 2;

        /* Apply stunning and slowing */
        if (do_stun) mon_inc_timed(p_ptr, m_ptr, MON_TMD_STUN, do_stun, flag, id);
        if (dam) mon_inc_timed(p_ptr, m_ptr, MON_TMD_SLOW, dam, flag, id);
    }

    /* Handle "teleport" */
    else if (do_dist)
    {
        /* Obvious */
        if (seen) obvious = TRUE;

        /* Teleport */
        if (teleport_away(m_ptr, do_dist)) m_note = MON_MSG_DISAPPEAR;
        else obvious = FALSE;

        /* Hack -- Get new location */
        y = m_ptr->fy;
        x = m_ptr->fx;
    }

    /* Handle stunning, confusion, blindness, slowing, hasting and fear */
    else if (do_stun)
    {
        if (m_ptr->m_timed[MON_TMD_STUN]) do_stun /= 2;

        if (do_stun && mon_inc_timed(p_ptr, m_ptr, MON_TMD_STUN, do_stun, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    else if (do_conf)
    {
        int tmp = damroll(3, do_conf / 2) + 1;

        /* Mind effects */
        if ((typ == GF_PSI) || (typ == GF_BLAST) || (typ == GF_SMASH)) tmp = do_conf;

        if (tmp && mon_inc_timed(p_ptr, m_ptr, MON_TMD_CONF, tmp, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    else if (do_blind)
    {
        if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_BLIND, do_blind, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    else if (do_slow)
    {
        if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_SLOW, do_slow, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    else if (do_haste)
    {
        if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_FAST, do_haste, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    if (do_fear)
    {
        if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_FEAR, do_fear, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    /* Handle sleep */
    if (do_sleep)
    {
        int tmp = 500;

        if (who > 0) flag |= MON_TMD_MON_SOURCE;
        else tmp += p_ptr->lev * 10;

        if (mon_inc_timed(p_ptr, m_ptr, MON_TMD_SLEEP, tmp, flag, id))
        {
            if (seen) obvious = TRUE;
        }
    }

    /* If another monster did the damage, hurt the monster by hand */
    else if (who > 0)
        mon_died = take_hit_MvM(who, m_ptr, dam, note_dies);

    /* If the player did it, give experience, check fear */
    else
    {
        bool fear = FALSE;

        /* The monster is going to be killed */
        if (dam > m_ptr->hp)
        {
            /* Adjust message for unseen monsters */
            if (!seen) note_dies = MON_MSG_MORIA_DEATH;

            /* Save the death notification for later */
            add_monster_message(p_ptr, m_name, m_ptr, note_dies, TRUE);
        }

        /* Hurt the monster, check for fear and death */
        if (mon_take_hit(0 - who, m_ptr, dam, &fear, ""))
            mon_died = TRUE;
        else
        {
            /* Give detailed messages if visible or destroyed */
            if ((m_note != MON_MSG_NONE) && seen)
                add_monster_message(p_ptr, m_name, m_ptr, m_note, FALSE);

            /* Hack -- Pain message */
            else if (dam > 0) message_pain(0 - who, m_ptr, dam);

            if (fear && seen)
                add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_FLEE_IN_TERROR, TRUE);
        }
    }

    /* Banish the monster */
    if (do_delete) delete_monster_idx(cave_get(depth), m_idx);

    /* Update the monster */
    if (!mon_died) update_mon(depth, m_idx, FALSE);

    /* Redraw the monster grid */
    cave_light_spot(cave_get(depth), y, x);

    /* Update monster recall window */
    if (p_ptr && (p_ptr->monster_race_idx == m_ptr->r_idx))
    {
        /* Redraw */
        p_ptr->redraw |= (PR_MONSTER);
    }

    /* Track it */
    project_m_n++;
    project_m_x = x;
    project_m_y = y;

    /* Return "Anything seen?" */
    return (obvious);
}


#define ATT_SAVE        0x01
#define ATT_DAMAGE      0x02
#define ATT_NON_PHYS    0x04
#define ATT_RAW         0x08


/* Map an attack type for each spell type */
static struct
{
    int spell;
    int attack;
    byte flags;
    const char *msg;
} map_type[] =
{
    {GF_ACID, RSF_BR_ACID, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_FIRE, RSF_BR_FIRE, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_COLD, RSF_BR_COLD, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_ELEC, RSF_BR_ELEC, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_POIS, RSF_BR_POIS, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_MISSILE, 0, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_HOLY_ORB, 0, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_ARROW_X, 0, ATT_DAMAGE, NULL},
    {GF_ARROW_1, 0, ATT_DAMAGE, NULL},
    {GF_ARROW_2, 0, ATT_DAMAGE, NULL},
    {GF_ARROW_3, 0, ATT_DAMAGE, NULL},
    {GF_ARROW_4, 0, ATT_DAMAGE, NULL},
    {GF_BOULDER, 0, ATT_DAMAGE, NULL},
    {GF_PLASMA, RSF_BR_PLAS, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_NETHER, RSF_BR_NETH, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_WATER, RSF_BR_WATE, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_CHAOS, RSF_BR_CHAO, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_SHARD, RSF_BR_SHAR, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_SOUND, RSF_BR_SOUN, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_DISEN, RSF_BR_DISE, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_NEXUS, RSF_BR_NEXU, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_FORCE, RSF_BR_WALL, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_INERT, RSF_BR_INER, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_LIGHT, RSF_BR_LIGHT, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_DARK, RSF_BR_DARK, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_TIME, RSF_BR_TIME, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_GRAVITY, RSF_BR_GRAV, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_MANA, RSF_BR_MANA, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_METEOR, 0, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_ICE, RSF_BO_ICEE, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_CURSE, 0, ATT_SAVE | ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_CURSE2, RSF_CAUSE_4, ATT_SAVE | ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_LIGHT_WEAK, 0, ATT_DAMAGE | ATT_NON_PHYS, NULL},
    {GF_AWAY_ALL, RSF_TELE_AWAY, 0, "commands you to go away"},
    {GF_TELE_TO, RSF_TELE_TO, 0, "commands you to return"},
    {GF_TELE_LEVEL, RSF_TELE_LEVEL, ATT_SAVE, "gestures at your feet"},
    {GF_MAGE_PROJECT, RSF_MAGE_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_PRIEST_PROJECT, RSF_PRIEST_PROJECT, ATT_RAW, "projects a prayer on you"},
    {GF_SORC_PROJECT, RSF_SORC_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_SHAD_PROJECT, RSF_SHAD_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_HUNT_PROJECT, RSF_HUNT_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_PSI_PROJECT, RSF_PSI_PROJECT, ATT_RAW, "projects a mental power on you"},
    {GF_DEATH_PROJECT, RSF_DEATH_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_GHOST_PROJECT, RSF_GHOST_PROJECT, ATT_RAW, "projects a ghostly ability on you"},
    {GF_MIMIC_PROJECT, RSF_MIMIC_PROJECT, ATT_RAW, "projects a monster spell on you"},
    {GF_ELEM_PROJECT, RSF_ELEM_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_SUMMON_PROJECT, RSF_SUMMON_PROJECT, ATT_RAW, "projects a spell on you"},
    {GF_OLD_CONF, RSF_CONF, ATT_SAVE, "creates a mesmerising illusion"},
    {GF_BLIND, RSF_BLIND, ATT_SAVE, "casts a spell, burning your eyes"},
    {GF_FORGET, RSF_FORGET, ATT_SAVE, "tries to blank your mind"},
    {GF_DRAIN_MANA, RSF_DRAIN_MANA, 0, "draws psychic energy from you"},
    {GF_OLD_SLOW, RSF_SLOW, ATT_SAVE, "drains power from your muscles"},
    {GF_OLD_SLEEP, RSF_HOLD, ATT_SAVE, "stares deep into your eyes"},
    {GF_TURN_ALL, RSF_SCARE, ATT_SAVE, "casts a fearful illusion"},
    {GF_STUN, RSF_STUN, ATT_SAVE, "casts a stunning spell"},
    {GF_OLD_POLY, RSF_POLY, 0, "polymorphs you"},
    {GF_PSI, RSF_PSI, ATT_SAVE | ATT_DAMAGE, NULL},
    {GF_BLAST, RSF_MIND_BLAST, ATT_SAVE | ATT_DAMAGE, NULL},
    {GF_SMASH, RSF_BRAIN_SMASH, ATT_SAVE | ATT_DAMAGE, NULL}
};


/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * This routine takes a "source monster" (by index), a "distance", a default
 * "damage", and a "damage type".  See "project_m()" above.
 *
 * If "rad" is non-zero, then the blast was centered elsewhere, and the damage
 * is reduced (see "project_m()" above).  This can happen if a monster breathes
 * at the player and hits a wall instead.
 *
 * We return "TRUE" if any "obvious" effects were observed.
 *
 * Actually, for historical reasons, we just assume that the effects were
 * obvious.  XXX XXX XXX
 */
static bool project_p(int who, int r, int depth, int y, int x, int dam, int typ, const char *what,
    bool obvious)
{
    player_type *p_ptr;
    bool blind, seen;
    bool dead = FALSE;

    /* Get the damage type details */
    const struct gf_type *gf_ptr = &gf_table[typ];

    /* Monster name (for attacks) */
    char m_name[NORMAL_WID];

    /* Monster name (for damage) */
    char killer[NORMAL_WID];

    /* Projected spell */
    int index = dam;

    /* No player here */
    int Ind = 0 - cave_get(depth)->m_idx[y][x];
    if (Ind <= 0) return (FALSE);

    /* Never affect projector */
    if (0 - who == Ind) return (FALSE);

    /* Obtain player info */
    p_ptr = player_get(Ind);
    blind = (p_ptr->timed[TMD_BLIND]? TRUE: FALSE);
    if (who > 0)
        seen = (!blind && p_ptr->mon_vis[who]);
    else
        seen = (!blind && p_ptr->play_vis[0 - who]);

    /* The damage has already been applied! */
    if (p_ptr->project_hurt) return FALSE;

    /* Limit maximum damage XXX XXX XXX */
    if (dam > 1600) dam = 1600;

    /* Reduce damage by distance */
    dam = (dam + r) / (r + 1);

    /* The caster is a monster */
    if (who > 0)
    {
        monster_type *m_ptr = cave_monster(cave_get(depth), who);

        /* Get the monster name */
        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

        /* Get the monster's real name */
        monster_desc(p_ptr, killer, sizeof(killer), m_ptr, MDESC_SHOW | MDESC_IND2);

        /* Check hostility for threatening spells */
        if (!pvm_check(Ind, who)) return FALSE;
    }

    /* The caster is a player */
    else if (who < 0)
    {
        bool threat = FALSE;

        /* Is this type of attack a threat? */
        switch (typ)
        {
            case GF_AWAY_ALL:
            case GF_MAGE_PROJECT:
            case GF_PRIEST_PROJECT:
            case GF_SORC_PROJECT:
            case GF_SHAD_PROJECT:
            case GF_HUNT_PROJECT:
            case GF_PSI_PROJECT:
            case GF_DEATH_PROJECT:
            case GF_GHOST_PROJECT:
            case GF_MIMIC_PROJECT:
            case GF_ELEM_PROJECT:
            case GF_SUMMON_PROJECT:
            case GF_TELE_TO:
            case GF_TELE_LEVEL: break;
            default: threat = TRUE; break;
        }

        /* Check hostility for threatening spells */
        if (threat)
        {
            int mode = ((target_get_monster(0 - who) == (0 - Ind))? PVP_DIRECT: PVP_INDIRECT);

            if (!pvp_check(player_get(0 - who), p_ptr, mode, TRUE, cave_get(depth)->feat[y][x]))
                return (FALSE);
        }

        player_desc(p_ptr, m_name, sizeof(m_name), player_get(0 - who), TRUE);
        my_strcpy(killer, player_get(0 - who)->name, sizeof(killer));
    }

    /* Let player know what is going on */
    if (!seen && gf_ptr->desc) msg(p_ptr, "You %s!", gf_ptr->desc);

    if (typ == GF_GRAVITY) msg(p_ptr, "Gravity warps around you.");

    /* MvP */
    if (who > 0)
    {
        /* Adjust damage for resistance, immunity or vulnerability, and apply it */
        dam = adjust_dam(p_ptr, typ, dam, RANDOMISE, check_for_resist(p_ptr, typ, TRUE));
        if (dam) take_hit(p_ptr, dam, killer, TRUE);
    }

    /* PvP */
    else
    {
        int i;

        /* Get the attack type for this spell type */
        for (i = 0; i < N_ELEMENTS(map_type); i++)
        {
            if (map_type[i].spell == typ) break;
        }

        /* Adjust damage for resistance, immunity or vulnerability, and apply it */
        if (i < N_ELEMENTS(map_type))
        {
            if (seen && map_type[i].msg) msg(p_ptr, "%s %s!", m_name, map_type[i].msg);
            if ((map_type[i].flags & ATT_SAVE) && magik(p_ptr->state.skills[SKILL_SAVE]))
                msg(p_ptr, "You resist the effects!");
            else
            {
                bool non_physical = ((map_type[i].flags & ATT_NON_PHYS)? TRUE: FALSE);

                strnfmt(p_ptr->died_flavor, sizeof(p_ptr->died_flavor), "was %s by %s", what, killer);
                dam = adjust_dam(p_ptr, typ, dam, RANDOMISE, check_for_resist(p_ptr, typ, TRUE));
                if (dam && (map_type[i].flags & ATT_DAMAGE))
                    dead = take_hit(p_ptr, dam, killer, non_physical);
                if (!dead && map_type[i].attack)
                {
                    do_side_effects(p_ptr, map_type[i].attack,
                        ((map_type[i].flags & ATT_RAW)? index: dam), who, seen, TRUE);
                }

                /* Give a message */
                if (dam && (map_type[i].flags & ATT_DAMAGE) && !dead)
                    player_pain(0 - who, Ind, dam);
            }
        }
    }

    /* The damage has been applied */
    p_ptr->project_hurt = TRUE;

    /* Disturb */
    disturb(p_ptr, 1, 0);

    /* Track this player */
    project_m_n++;
    project_m_x = x;
    project_m_y = y;

    /* Return "Anything seen?" */
    return (obvious);
}


/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
void bolt_pict(int Ind, int y, int x, int ny, int nx, int typ, byte *a, char *c)
{
    player_type *p_ptr = player_get(Ind);
    int motion;

    /* Convert co-ordinates into motion */
    if ((ny == y) && (nx == x))
        motion = BOLT_NO_MOTION;
    else if (nx == x)
        motion = ((ny < y)? BOLT_0: BOLT_180);
    else if ((ny - y) == (x - nx))
        motion = ((ny < y)? BOLT_45: BOLT_225);
    else if (ny == y)
        motion = ((nx > x)? BOLT_90: BOLT_270);
    else if ((ny - y) == (nx - x))
        motion = ((nx > x)? BOLT_135: BOLT_315);
    else
        motion = BOLT_NO_MOTION;

    /* Decide on output char */
    if (!p_ptr->use_graphics)
    {
        /* ASCII is simple */
        char chars[] = "*|/-\\|/-\\";

        *c = chars[motion];
        *a = spell_color(typ);
    }
    else
    {
        *a = p_ptr->gf_attr[typ][motion];
        *c = p_ptr->gf_char[typ][motion];
    }
}


/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * Input:
 *   who: Index of "source" monster (negative for "player")
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
 *   y,x: Target location (or location to travel "towards")
 *   dam: Base damage roll to apply to affected monsters (or player)
 *   typ: Type of damage to apply to monsters (and objects)
 *   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
 *
 * Return:
 *   TRUE if any "effects" of the projection were observed, else FALSE
 *
 * Allows a monster (or player) to project a beam/bolt/ball of a given kind
 * towards a given location (optionally passing over the heads of interposing
 * monsters), and have it do a given amount of damage to the monsters (and
 * optionally objects) within the given radius of the final location.
 *
 * A "bolt" travels from source to target and affects only the target grid.
 * A "beam" travels from source to target, affecting all grids passed through.
 * A "ball" travels from source to the target, exploding at the target, and
 *   affecting everything within the given radius of the target location.
 *
 * Traditionally, a "bolt" does not affect anything on the ground, and does
 * not pass over the heads of interposing monsters, much like a traditional
 * missile, and will "stop" abruptly at the "target" even if no monster is
 * positioned there, while a "ball", on the other hand, passes over the heads
 * of monsters between the source and target, and affects everything except
 * the source monster which lies within the final radius, while a "beam"
 * affects every monster between the source and target, except for the casting
 * monster (or player), and rarely affects things on the ground.
 *
 * Two special flags allow us to use this function in special ways, the
 * "PROJECT_HIDE" flag allows us to perform "invisible" projections, while
 * the "PROJECT_JUMP" flag allows us to affect a specific grid, without
 * actually projecting from the source monster (or player).
 *
 * The player will only get "experience" for monsters killed by himself
 * Unique monsters can only be destroyed by attacks from the player
 *
 * Only 256 grids can be affected per projection, limiting the effective
 * "radius" of standard ball attacks to nine units (diameter nineteen).
 *
 * One can project in a given "direction" by combining PROJECT_THRU with small
 * offsets to the initial location (see "line_spell()"), or by calculating
 * "virtual targets" far away from the player.
 *
 * One can also use PROJECT_THRU to send a beam/bolt along an angled path,
 * continuing until it actually hits somethings (useful for "stone to mud").
 *
 * Bolts and Beams explode INSIDE walls, so that they can destroy doors.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters
 * on both sides of a wall.  Some bug reports indicate that this is still
 * happening in 2.7.8 for Windows, though it appears to be impossible.
 *
 * We "pre-calculate" the blast area only in part for efficiency.
 * More importantly, this lets us do "explosions" from the "inside" out.
 * This results in a more logical distribution of "blast" treasure.
 * It also produces a better (in my opinion) animation of the explosion.
 * It could be (but is not) used to have the treasure dropped by monsters
 * in the middle of the explosion fall "outwards", and then be damaged by
 * the blast as it spreads outwards towards the treasure drop location.
 *
 * Walls and doors are included in the blast area, so that they can be
 * "burned" or "melted" in later versions.
 *
 * This algorithm is intended to maximize simplicity, not necessarily
 * efficiency, since this function is not a bottleneck in the code.
 *
 * We apply the blast effect from ground zero outwards, in several passes,
 * first affecting features, then objects, then monsters, then the player.
 * This allows walls to be removed before checking the object or monster
 * in the wall, and protects objects which are dropped by monsters killed
 * in the blast, and allows the player to see all affects before he is
 * killed or teleported away.  The semantics of this method are open to
 * various interpretations, but they seem to work well in practice.
 *
 * We process the blast area from ground-zero outwards to allow for better
 * distribution of treasure dropped by monsters, and because it provides a
 * pleasing visual effect at low cost.
 *
 * Note that the damage done by "ball" explosions decreases with distance.
 * This decrease is rapid, grids at radius "dist" take "1/dist" damage.
 *
 * Notice the "napalm" effect of "beam" weapons.  First they "project" to
 * the target, and then the damage "flows" along this beam of destruction.
 * The damage at every grid is the same as at the "center" of a "ball"
 * explosion, since the "beam" grids are treated as if they ARE at the
 * center of a "ball" explosion.
 *
 * Currently, specifying "beam" plus "ball" means that locations which are
 * covered by the initial "beam", and also covered by the final "ball", except
 * for the final grid (the epicenter of the ball), will be "hit twice", once
 * by the initial beam, and once by the exploding ball.  For the grid right
 * next to the epicenter, this results in 150% damage being done.  The center
 * does not have this problem, for the same reason the final grid in a "beam"
 * plus "bolt" does not -- it is explicitly removed.  Simply removing "beam"
 * grids which are covered by the "ball" will NOT work, as then they will
 * receive LESS damage than they should.  Do not combine "beam" with "ball".
 *
 * The array "gy[],gx[]" with current size "grids" is used to hold the
 * collected locations of all grids in the "blast area" plus "beam path".
 *
 * Note the rather complex usage of the "gm[]" array.  First, gm[0] is always
 * zero.  Second, for N>1, gm[N] is always the index (in gy[],gx[]) of the
 * first blast grid (see above) with radius "N" from the blast center.  Note
 * that only the first gm[1] grids in the blast area thus take full damage.
 * Also, note that gm[rad+1] is always equal to "grids", which is the total
 * number of blast grids.
 *
 * Note that once the projection is complete, (y2,x2) holds the final location
 * of bolts/beams, and the "epicenter" of balls.
 *
 * Note also that "rad" specifies the "inclusive" radius of projection blast,
 * so that a "rad" of "one" actually covers 5 or 9 grids, depending on the
 * implementation of the "distance" function.  Also, a bolt can be properly
 * viewed as a "ball" with a "rad" of "zero".
 *
 * Note that if no "target" is reached before the beam/bolt/ball travels the
 * maximum distance allowed (MAX_RANGE), no "blast" will be induced.  This
 * may be relevant even for bolts, since they have a "1x1" mini-blast.
 *
 * Note that for consistency, we "pretend" that the bolt actually takes "time"
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that all projections now "explode" at their final destination, even
 * if they were being projected at a more distant destination.  This means
 * that "ball" spells will *always* explode.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the "illumination" of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(int who, int rad, int depth, int y, int x, int dam, int typ, int flg, const char *what)
{
    int y1, x1;
    int y2, x2;
    int dist;
    int i, t, j;
    bool visual[MAX_PLAYERS];
    bool drawn[MAX_PLAYERS];

    /* Assume the player sees nothing */
    bool notice = FALSE;

    /* Number of grids in the "path" */
    int path_n = 0;

    /* Actual grids in the "path" */
    u16b path_g[512];

    /* Number of grids in the "blast area" (including the "beam" path) */
    int grids = 0;

    /* Coordinates of the affected grids */
    byte gx[256], gy[256];

    /* Encoded "radius" info (see above) */
    byte gm[16];

    /* Assume the player has seen nothing */
    for (i = 0; i < MAX_PLAYERS; ++i) visual[i] = FALSE;

    /* Assume the player has seen no blast grids */
    for (i = 0; i < MAX_PLAYERS; ++i) drawn[i] = FALSE;

    /* Hack -- Jump to target */
    if (flg & PROJECT_JUMP)
    {
        x1 = x;
        y1 = y;

        /* Clear the flag */
        flg &= ~(PROJECT_JUMP);
    }

    /* Start at player */
    else if (who < 0)
    {
        x1 = player_get(0 - who)->px;
        y1 = player_get(0 - who)->py;
    }

    /* Start at monster */
    else if (who > 0)
    {
        x1 = cave_monster(cave_get(depth), who)->fx;
        y1 = cave_monster(cave_get(depth), who)->fy;
    }

    /* Oops */
    else
    {
        x1 = x;
        y1 = y;
    }

    /* Default "destination" */
    y2 = y;
    x2 = x;

    /* Hack -- verify stuff */
    if (flg & PROJECT_THRU)
    {
        if ((x1 == x2) && (y1 == y2)) flg &= ~PROJECT_THRU;
    }

    /* Hack -- Assume there will be no blast (max radius 16) */
    for (dist = 0; dist < 16; dist++) gm[dist] = 0;

    /* Initial grid */
    y = y1;
    x = x1;

    /* Calculate the projection path */
    /* Hack -- Remove PROJECT_STOP flag to handle friendly targets separately */
    path_n = project_path(path_g, MAX_RANGE, depth, y1, x1, y2, x2, (flg & ~PROJECT_STOP));

    /* Project along the path */
    for (i = 0; i < path_n; ++i)
    {
        int oy = y;
        int ox = x;
        int ny = GRID_Y(path_g[i]);
        int nx = GRID_X(path_g[i]);
        int m_idx;

        /* Hack -- Balls explode before reaching walls */
        if (!cave_floor_bold(depth, ny, nx) && (rad > 0)) break;

        /* Advance */
        y = ny;
        x = nx;

        /* Collect beam grids */
        if (flg & PROJECT_BEAM)
        {
            gy[grids] = y;
            gx[grids] = x;
            grids++;
        }

        /* Only do visuals if requested */
        if (!(flg & PROJECT_HIDE))
        {
            /* Do visuals for all players that can "see" the bolt */
            for (j = 1; j < NumPlayers + 1; j++)
            {
                player_type *p_ptr = player_get(j);

                /* Skip irrelevant players */
                if (p_ptr->depth != depth) continue;
                if (p_ptr->timed[TMD_BLIND]) continue;
                if (!panel_contains(p_ptr, y, x)) continue;
                if (p_ptr->did_visuals) continue;

                /* Only do visuals if the player can "see" the bolt */
                if (player_has_los_bold(p_ptr, y, x))
                {
                    byte a;
                    char c;

                    /* Obtain the bolt pict */
                    bolt_pict(j, oy, ox, y, x, typ, &a, &c);

                    /* Draw, Highlight, Fresh, Pause, Erase */
                    flush_path_grid(p_ptr, depth, y, x, a, c);

                    /* Display "beam" grids */
                    if (flg & (PROJECT_BEAM))
                    {
                        /* Obtain the explosion pict */
                        bolt_pict(j, y, x, y, x, typ, &a, &c);

                        /* Draw, Highlight, Fresh, Pause, Erase */
                        draw_path_grid(p_ptr, y, x, a, c);
                    }

                    /* Hack -- Activate delay */
                    visual[j] = TRUE;
                }

                /* Hack -- Delay anyway for consistency */
                else if (visual[j])
                {
                    /* Delay for consistency */
                    Send_flush(p_ptr, FALSE, TRUE);
                }
            }
        }

        /* Sometimes stop at non-initial monsters/players */
        m_idx = cave_get(depth)->m_idx[y][x];
        if ((m_idx != 0) && (flg & PROJECT_STOP))
        {
            s16b p1_id, p2_id;

            /* Get caster info */
            if (who < 0) p1_id = player_get(0 - who)->id;
            else p1_id = cave_monster(cave_get(depth), who)->master;

            /* Get target info */
            if (m_idx < 0) p2_id = player_get(0 - m_idx)->id;
            else p2_id = cave_monster(cave_get(depth), m_idx)->master;

            /* Skip the dungeon master */
            if ((m_idx < 0) && is_dm(0 - m_idx))
            {
                /* Let all spells pass through the dungeon master */
            }

            /* Stop at anything that isn't party-friendly */
            else if (!master_in_party(p1_id, p2_id)) break;

            /* Stop if a useful spell hits a friendly player */
            else if ((typ >= GF_MAGE_PROJECT) && (typ <= GF_MIMIC_PROJECT) && (m_idx < 0))
                break;

            /* Stop if an order hits a friendly monster */
            else if (((typ == GF_GUARD) || (typ == GF_FOLLOW) || (typ == GF_ATTACK)) && (m_idx > 0))
                break;

            /* Let everything else pass through */
        }
    }

    /* Save the "blast epicenter" */
    y2 = y;
    x2 = x;

    /* Start the "explosion" */
    gm[0] = 0;

    /* Hack -- make sure beams get to "explode" */
    gm[1] = grids;

    /* Explode */
    /* Hack -- remove final beam grid */
    if (flg & (PROJECT_BEAM)) grids--;

    /* Determine the blast area, work from the inside out */
    for (dist = 0; dist <= rad; dist++)
    {
        /* Scan the maximal blast area of radius "dist" */
        for (y = y2 - dist; y <= y2 + dist; y++)
        {
            for (x = x2 - dist; x <= x2 + dist; x++)
            {
                /* Ignore "illegal" locations */
                if (!in_bounds(y, x)) continue;

                /* Enforce a "circular" explosion */
                if (distance(y2, x2, y, x) != dist) continue;

                /* Ball explosions are stopped by walls */
                if (!los(depth, y2, x2, y, x)) continue;

                /* Save this grid */
                gy[grids] = y;
                gx[grids] = x;
                grids++;
            }
        }

        /* Encode some more "radius" info */
        gm[dist + 1] = grids;
    }

    /* Speed -- ignore "non-explosions" */
    if (!grids) return (FALSE);

    /* Display the "blast area" if requested */
    if (!(flg & PROJECT_HIDE))
    {
        /* Then do the "blast", from inside out */
        for (t = 0; t <= rad; t++)
        {
            /* Dump everything with this radius */
            for (i = gm[t]; i < gm[t + 1]; i++)
            {
                /* Extract the location */
                y = gy[i];
                x = gx[i];

                /* Do visuals for all players that can "see" the blast */
                for (j = 1; j < NumPlayers + 1; j++)
                {
                    player_type *p_ptr = player_get(j);

                    /* Skip irrelevant players */
                    if (p_ptr->depth != depth) continue;
                    if (p_ptr->timed[TMD_BLIND]) continue;
                    if (!panel_contains(p_ptr, y, x)) continue;
                    if (p_ptr->did_visuals) continue;

                    /* Only do visuals if the player can "see" the blast */
                    if (player_has_los_bold(p_ptr, y, x))
                    {
                        byte a;
                        char c;

                        drawn[j] = TRUE;

                        /* Obtain the explosion pict */
                        bolt_pict(j, y, x, y, x, typ, &a, &c);

                        /* Draw, Highlight, Fresh, Pause, Erase */
                        draw_path_grid(p_ptr, y, x, a, c);
                    }
                }
            }

            /* Flush each "radius" separately for all players that can "see" the blast */
            for (j = 1; j < NumPlayers + 1; j++)
            {
                player_type *p_ptr = player_get(j);

                /* Skip irrelevant players */
                if (p_ptr->depth != depth) continue;
                if (p_ptr->timed[TMD_BLIND]) continue;

                /* Flush each "radius" separately */
                if (visual[j] || drawn[j])
                {
                    /* Delay (efficiently) */
                    Send_flush(p_ptr, TRUE, TRUE);
                }
                else
                    Send_flush(p_ptr, TRUE, FALSE);
            }
        }

        /* Flush the erasing for all players that can "see" the blast */
        for (j = 1; j < NumPlayers + 1; j++)
        {
            player_type *p_ptr = player_get(j);

            /* Skip irrelevant players */
            if (p_ptr->depth != depth) continue;
            if (p_ptr->timed[TMD_BLIND]) continue;

            /* Flush the erasing */
            if (drawn[j])
            {
                /* Erase the explosion drawn above */
                for (i = 0; i < grids; i++)
                {
                    /* Extract the location */
                    y = gy[i];
                    x = gx[i];

                    /* Skip irrelevant players */
                    if (!panel_contains(p_ptr, y, x)) continue;

                    /* Hack -- Erase if needed */
                    if (player_has_los_bold(p_ptr, y, x))
                        cave_light_spot_aux(p_ptr, cave_get(depth), y, x);
                }

                /* Flush the explosion */
                Send_flush(p_ptr, TRUE, FALSE);
            }
        }
    }

    /* Hack -- Count how many blasts we have seen */
    for (j = 1; j < NumPlayers + 1; j++)
    {
        player_type *p_ptr = player_get(j);

        /* Skip irrelevant players */
        if (p_ptr->depth != depth) continue;
        if (p_ptr->timed[TMD_BLIND]) continue;

        /* Add one to the count */
        if (visual[j] || drawn[j]) p_ptr->did_visuals = TRUE;
    }

    /* Check features */
    if (flg & PROJECT_GRID)
    {
        /* Start with "dist" of zero */
        dist = 0;

        /* Scan for features */
        for (i = 0; i < grids; i++)
        {
            /* Hack -- Notice new "dist" values */
            if (gm[dist + 1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the feature in that grid */
            if (project_f(who, dist, depth, y, x, dam, typ, FALSE))
                notice = TRUE;
        }
    }

    /* Check objects */
    if (flg & PROJECT_ITEM)
    {
        /* Start with "dist" of zero */
        dist = 0;

        /* Scan for objects */
        for (i = 0; i < grids; i++)
        {
            /* Hack -- Notice new "dist" values */
            if (gm[dist + 1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the object */
            if (project_o(who, dist, depth, y, x, dam, typ, FALSE))
                notice = TRUE;
        }
    }

    /* Check monsters */
    if (flg & PROJECT_KILL)
    {
        /* Mega-Hack */
        project_m_n = 0;
        project_m_x = 0;
        project_m_y = 0;

        /* Start with "dist" of zero */
        dist = 0;

        /* Scan for monsters */
        for (i = 0; i < grids; i++)
        {
            /* Hack -- Notice new "dist" values */
            if (gm[dist + 1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the monster in the grid */
            if (project_m(who, dist, depth, y, x, dam, typ, ((flg & PROJECT_AWARE)? TRUE: FALSE)))
                notice = TRUE;
        }

        /* Player affected one monster (without "jumping") */
        if ((who < 0) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
        {
            int m_idx;

            /* Location */
            x = project_m_x;
            y = project_m_y;

            /* Track if possible */
            m_idx = cave_get(depth)->m_idx[y][x];
            if (m_idx > 0)
            {
                player_type *p_ptr = player_get(0 - who);
                monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);

                /* Hack -- auto-recall */
                if (p_ptr->mon_vis[m_idx]) monster_race_track(0 - who, m_ptr->r_idx);

                /* Hack - auto-track */
                if (p_ptr->mon_vis[m_idx]) health_track(p_ptr, m_idx);
            }
        }
    }

    /* Check players */
    if (flg & PROJECT_KILL)
    {
        /* Mega-Hack */
        project_m_n = 0;
        project_m_x = 0;
        project_m_y = 0;

        /* Start with "dist" of zero */
        dist = 0;

        /* Players didn't get hurt yet */
        for (j = 1; j < NumPlayers + 1; j++)
        {
            player_type *p_ptr = player_get(j);
            if (p_ptr->depth != depth) continue;
            p_ptr->project_hurt = FALSE;
        }

        /* Scan for players */
        for (i = 0; i < grids; i++)
        {
            /* Hack -- Notice new "dist" values */
            if (gm[dist + 1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the player (assume obvious) */
            if (project_p(who, dist, depth, y, x, dam, typ, what, TRUE))
                notice = TRUE;
        }

        /* Player affected one player (without "jumping") */
        if ((who < 0) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
        {
            int m_idx;

            /* Location */
            x = project_m_x;
            y = project_m_y;

            /* Track if possible */
            m_idx = cave_get(depth)->m_idx[y][x];
            if (m_idx < 0)
            {
                player_type *p_ptr = player_get(0 - who);

                /* Hack - auto-track */
                if (p_ptr->play_vis[0 - m_idx]) health_track(p_ptr, m_idx);
            }
        }
    }

    /* Return "something was noticed" */
    return (notice);
}


bool check_st_anchor(int depth, int y, int x)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *q_ptr = player_get(i);

        /* Skip players not on this depth */
        if (q_ptr->depth != depth) continue;

        /* Skip players too far */
        if (distance(q_ptr->py, q_ptr->px, y, x) > ANCHOR_RADIUS) continue;

        if (!q_ptr->timed[TMD_ANCHOR]) continue;

        return TRUE;
    }

    /* Assume no st_anchor */
    return FALSE;
}


bool has_bowbrand(struct player *p, byte brand)
{
    return (p->timed[TMD_BOWBRAND] && (p->bow_brand_t == brand));
}


bool can_swim(struct player *p)
{
    return (p->r_idx && rf_has(r_info[p->r_idx].flags, RF_IM_WATER));
}


void monster_set_master(struct monster *m, struct player *p, byte status)
{
    /* A high charisma will allow more slaves to be controlled */
    if (p && (m->status <= MSTATUS_SUMMONED))
    {
        int maxslaves = 1 + (1 + p->state.stat_ind[A_CHR]) / 4;

        if (p->slaves == maxslaves)
        {
            msg(p, "You cannot control that many monsters!");
            return;
        }
    }

    m->master = (p? p->id: 0);
    m->lifespan = (p? m->level + 5 + randint1(5): 0);
    if (p && (m->status <= MSTATUS_SUMMONED)) p->slaves++;
    m->status = status;
}


/*
 * Stat Table (CHR) -- Chance of getting a friendly summon
 */
static const byte summon_friendly[STAT_RANGE] =
{
    0   /* 3 */,
    4   /* 4 */,
    8   /* 5 */,
    12  /* 6 */,
    16  /* 7 */,
    20  /* 8 */,
    23  /* 9 */,
    26  /* 10 */,
    29  /* 11 */,
    32  /* 12 */,
    35  /* 13 */,
    38  /* 14 */,
    41  /* 15 */,
    44  /* 16 */,
    47  /* 17 */,
    50  /* 18/00-18/09 */,
    52  /* 18/10-18/19 */,
    54  /* 18/20-18/29 */,
    56  /* 18/30-18/39 */,
    58  /* 18/40-18/49 */,
    60  /* 18/50-18/59 */,
    62  /* 18/60-18/69 */,
    64  /* 18/70-18/79 */,
    66  /* 18/80-18/89 */,
    68  /* 18/90-18/99 */,
    70  /* 18/100-18/109 */,
    72  /* 18/110-18/119 */,
    74  /* 18/120-18/129 */,
    76  /* 18/130-18/139 */,
    78  /* 18/140-18/149 */,
    80  /* 18/150-18/159 */,
    83  /* 18/160-18/169 */,
    86  /* 18/170-18/179 */,
    89  /* 18/180-18/189 */,
    92  /* 18/190-18/199 */,
    95  /* 18/200-18/209 */,
    98  /* 18/210-18/219 */,
    100 /* 18/220+ */
};


bool can_charm_monster(struct player *p)
{
    return (magik(summon_friendly[p->state.stat_ind[A_CHR]]));
}


int charm_monster(struct monster *m, struct player *p, byte status)
{
    monster_race *r_ptr = &r_info[m->r_idx];
    bool charmed = FALSE;

    /* Only if the monster has been summoned */
    if (m->status == MSTATUS_HOSTILE) return MON_MSG_UNAFFECTED;

    /* Uniques are unaffected */
    if (rf_has(r_ptr->flags, RF_UNIQUE)) return MON_MSG_UNAFFECTED;

    /* Too enraged to be controlled */
    if (check_state(p, OF_AGGRAVATE)) return MON_MSG_HATE;

    /* A high level will help a lot */
    if (!CHANCE(MAX(m->level - 5, 1), p->lev * 5)) charmed = TRUE;

    /* A high charisma will help a lot, and will always yield useful summons */
    if (can_charm_monster(p))
    {
        charmed = TRUE;
        status = MSTATUS_ATTACK;
    }

    /* Only if the monster is not already under the spell */
    if (m->status >= status) return MON_MSG_UNAFFECTED;

    /* Monster is pacified */
    if (charmed)
    {
        monster_set_master(m, p, status);
        return MON_MSG_REACT;
    }

    /* Monster stays hostile */
    return MON_MSG_UNAFFECTED;
}
