/*
 * File: mon-util.c
 * Purpose: Monster manipulation utilities.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


static const struct monster_flag monster_flag_table[] =
{
    #define RF(a, b, c) {RF_##a, b, c},
    #include "../common/list-mon-race-flags.h"
    #undef RF
    {RF_MAX, RFT_NONE, NULL}
};


/*
 * Return a description for the given monster race flag.
 *
 * Returns an empty string for an out-of-range flag.
 *
 * flag is one of the RF_ flags.
 */
const char *describe_race_flag(int flag)
{
    if ((flag <= RF_NONE) || (flag >= RF_MAX)) return "";
    return monster_flag_table[flag].desc;
}


/*
 * Create a mask of monster flags of a specific type.
 *
 * f is the flag array we're filling
 * ... is the list of flags we're looking for
 *
 * N.B. RFT_MAX must be the last item in the ... list
 */
void create_mon_flag_mask(bitflag *f, ...)
{
    const struct monster_flag *rf;
    int i;
    va_list args;

    rf_wipe(f);

    va_start(args, f);

    /* Process each type in the va_args */
    for (i = va_arg(args, int); i != RFT_MAX; i = va_arg(args, int))
    {
        for (rf = monster_flag_table; rf->index < RF_MAX; rf++)
        {
            if (rf->type == i)
                rf_on(f, rf->index);
        }
    }

    va_end(args);
}


/*
 * Return whether the given base matches any of the names given.
 *
 * Accepts a variable-length list of name strings. The list must end with NULL.
 */
bool match_monster_bases(const struct monster_base *base, ...)
{
    bool ok = false;
    va_list vp;
    char *name;

    va_start(vp, base);
    while (!ok && ((name = va_arg(vp, char *)) != NULL))
        ok = (base == lookup_monster_base(name));
    va_end(vp);

    return ok;
}


/*
 * Analyse the path from player to infravision-seen monster and forget any
 * grids which would have blocked line of sight
 */
static void path_analyse(struct player *p, struct chunk *c, struct loc *grid)
{
    int path_n, i;
    struct loc path_g[256];

    /* Plot the path. */
    path_n = project_path(NULL, path_g, z_info->max_range, c, &p->grid, grid, PROJECT_NONE);

    /* Project along the path */
    for (i = 0; i < path_n; ++i)
    {
        /* Skip target */
        if (loc_eq(&path_g[i], grid)) continue;

        /* Forget grids which would block los */
        if (square_isprojectable(c, &path_g[i]) &&
            !feat_is_projectable(square_p(p, &path_g[i])->feat))
        {
            sqinfo_off(square_p(p, &path_g[i])->info, SQUARE_SEEN);
            square_forget(p, &path_g[i]);
            square_light_spot_aux(p, c, &path_g[i]);
        }
    }
}


void player_desc(struct player *p, char *desc, size_t max, struct player *q, bool capitalize)
{
    int who = get_player_index(get_connection(q->conn));

    if (player_is_visible(p, who))
        my_strcpy(desc, q->name, max);
    else
        my_strcpy(desc, "someone", max);
    if (capitalize) my_strcap(desc);
}


static bool is_detected_m(struct player *p, const bitflag mflags[RF_SIZE], int d_esp)
{
    /* Full ESP */
    if (player_of_has(p, OF_ESP_ALL)) return true;

    /* Partial ESP */
    if (rf_has(mflags, RF_ORC) && player_of_has(p, OF_ESP_ORC)) return true;
    if (rf_has(mflags, RF_TROLL) && player_of_has(p, OF_ESP_TROLL)) return true;
    if (rf_has(mflags, RF_GIANT) && player_of_has(p, OF_ESP_GIANT)) return true;
    if (rf_has(mflags, RF_DRAGON) && player_of_has(p, OF_ESP_DRAGON)) return true;
    if (rf_has(mflags, RF_DEMON) && player_of_has(p, OF_ESP_DEMON)) return true;
    if (rf_has(mflags, RF_UNDEAD) && player_of_has(p, OF_ESP_UNDEAD)) return true;
    if (rf_has(mflags, RF_EVIL) && player_of_has(p, OF_ESP_EVIL)) return true;
    if (rf_has(mflags, RF_ANIMAL) && player_of_has(p, OF_ESP_ANIMAL)) return true;

    /* Radius ESP */
    if (player_of_has(p, OF_ESP_RADIUS)) return (d_esp <= z_info->max_sight);

    /* No ESP */
    return false;
}


/*
 * Returns true if the given monster is currently mimicking an ignored item.
 */
static bool is_mimicking_ignored(struct player *p, struct monster *mon)
{
    my_assert(mon != NULL);

    if (!monster_is_mimicking(mon)) return false;

    return ignore_item_ok(p, mon->mimicked_obj);
}


/*
 * This function updates the monster record of the given monster
 *
 * This involves extracting the distance to the player (if requested),
 * and then checking for visibility (natural, infravision, see-invis,
 * telepathy), updating the monster visibility flag, redrawing (or
 * erasing) the monster when its visibility changes, and taking note
 * of any interesting monster flags (cold-blooded, invisible, etc).
 *
 * Note the new "mflag" field which encodes several monster state flags,
 * including "view" for when the monster is currently in line of sight,
 * and "mark" for when the monster is currently visible via detection.
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "ml" (visible to the player), and
 * "mflag" (to maintain the "MFLAG_VIEW" flag).
 *
 * Note the special "update_monsters()" function which can be used to
 * call this function once for every monster.
 *
 * Note the "full" flag which requests that the "cdis" field be updated;
 * this is only needed when the monster (or the player) has moved.
 *
 * Every time a monster moves, we must call this function for that
 * monster, and update the distance, and the visibility.  Every time
 * the player moves, we must call this function for every monster, and
 * update the distance, and the visibility.  Whenever the player "state"
 * changes in certain ways ("blindness", "infravision", "telepathy",
 * and "see invisible"), we must call this function for every monster,
 * and update the visibility.
 *
 * Routines that change the "illumination" of a grid must also call this
 * function for any monster in that grid, since the "visibility" of some
 * monsters may be based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves.  When the player is running, this function is one
 * of the primary bottlenecks, along with "update_view()" and the
 * "process_monsters()" code, so efficiency is important.
 *
 * A monster is "visible" to the player if (1) it has been detected
 * by the player, (2) it is close to the player and the player has
 * telepathy, or (3) it is close to the player, and in line of sight
 * of the player, and it is "illuminated" by some combination of
 * infravision, torch light, or permanent light (invisible monsters
 * are only affected by "light" if the player can see invisible).
 *
 * Monsters which are not on the current panel may be "visible" to
 * the player, and their descriptions will include an "offscreen"
 * reference.  Currently, offscreen monsters cannot be targeted
 * or viewed directly, but old targets will remain set.  XXX XXX
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way).  Note that "moves" includes "appears" and "disappears".
 */
static void update_mon_aux(struct player *p, struct monster *mon, struct chunk *c, bool full,
    bool *blos, int *dis_to_closest, struct player **closest, int *lowhp)
{
    struct monster_lore *lore;
    int d, d_esp;
    struct source who_body;
    struct source *who = &who_body;
    struct loc grid1, grid2, grid;

    /* Seen at all */
    bool flag = false;

    /* Seen by vision */
    bool easy = false;

    /* ESP permitted */
    bool telepathy_ok = true;

    /* Basic telepathy */
    bool basic = false;

    bool isDM = ((p->dm_flags & DM_SEE_MONSTERS)? true: false);

    /* If still generating the level, measure distances from the middle */
    grid.y = (!ht_zero(&c->generated)? p->grid.y: c->height / 2);
    grid.x = (!ht_zero(&c->generated)? p->grid.x: c->width / 2);

    my_assert(mon != NULL);
    source_monster(who, mon);

    lore = get_lore(p, mon->race);

    /* Compute distance */
    d = distance(&grid, &mon->grid);

    /* Restrict distance */
    if (d > 255) d = 255;

    /* PWMAngband: telepathic awareness */
    loc_init(&grid1, grid.x / 3, grid.y);
    loc_init(&grid2, mon->grid.x / 3, mon->grid.y);
    d_esp = distance(&grid1, &grid2);
    if (d_esp > 255) d_esp = 255;

    /* Find the closest player */
    if (full)
    {
        /* Hack -- skip him if he's shopping */
        /* Hack -- make the dungeon master invisible to monsters */
        /* Skip player if dead or gone */
        if (!in_store(p) && !(p->dm_flags & DM_MONSTER_FRIEND) &&
            p->alive && !p->is_dead && !p->upkeep->new_level_method)
        {
            /* Check if monster has LOS to the player */
            bool new_los = los(c, &mon->grid, &grid);

            /* Remember this player if closest */
            if (is_closest(p, mon, *blos, new_los, d, *dis_to_closest, *lowhp))
            {
                *blos = new_los;
                *dis_to_closest = d;
                *closest = p;
                *lowhp = p->chp;
            }
        }
    }

    /* Detected */
    if (p->mon_det[mon->midx]) flag = true;

    /* Check if telepathy works */
    if (square_isno_esp(c, &mon->grid) || square_isno_esp(c, &grid))
        telepathy_ok = false;

    /* Nearby */
    if ((d <= z_info->max_sight) || !cfg_limited_esp || isDM)
    {
        bool hasESP = is_detected_m(p, mon->race->flags, d_esp);
        bool isTL = (player_has(p, PF_THUNDERLORD) &&
            (d_esp <= (p->lev * z_info->max_sight / PY_MAX_LEVEL)));

        basic = (isDM || ((hasESP || isTL) && telepathy_ok));

        /* Basic telepathy */
        if (basic)
        {
            /* Empty mind, no telepathy */
            if (rf_has(mon->race->flags, RF_EMPTY_MIND)) {}

            /* Weird mind, one in ten individuals are detectable */
            else if (rf_has(mon->race->flags, RF_WEIRD_MIND))
            {
                if ((mon->midx % 10) == 5)
                {
                    /* Detectable */
                    flag = true;

                    /* Check for LOS so that MFLAG_VIEW is set later */
                    if (square_isview(p, &mon->grid)) easy = true;
                }
            }

            /* Normal mind, allow telepathy */
            else
            {
                flag = true;

                /* Check for LOS so that MFLAG_VIEW is set later */
                if (square_isview(p, &mon->grid)) easy = true;
            }

            /* DM has perfect ESP */
            if (isDM)
            {
                flag = true;

                /* Check for LOS so that MFLAG_VIEW is set later */
                if (square_isview(p, &mon->grid)) easy = true;
            }
        }

        /* Normal line of sight and player is not blind */
        if (square_isview(p, &mon->grid) && !p->timed[TMD_BLIND])
        {
            /* Use "infravision" */
            if (d <= p->state.see_infra)
            {
                /* Learn about warm/cold blood */
                rf_on(lore->flags, RF_COLD_BLOOD);

                /* Handle "warm blooded" monsters */
                if (!rf_has(mon->race->flags, RF_COLD_BLOOD))
                {
                    /* Easy to see */
                    easy = flag = true;
                }
            }

            /* Use illumination */
            if (square_isseen(p, &mon->grid))
            {
                /* Learn it emits light */
                rf_on(lore->flags, RF_HAS_LIGHT);

                /* Learn about invisibility */
                rf_on(lore->flags, RF_INVISIBLE);

                /* Handle invisibility */
                if (monster_is_invisible(mon))
                {
                    /* See invisible */
                    if (player_of_has(p, OF_SEE_INVIS))
                    {
                        /* Easy to see */
                        easy = flag = true;
                    }
                }
                else
                {
                    /* Easy to see */
                    easy = flag = true;
                }
            }

            /* Learn about intervening squares */
            path_analyse(p, c, &mon->grid);
        }
    }

    /* If a mimic looks like an ignored item, it's not seen */
    if (is_mimicking_ignored(p, mon))
        easy = flag = false;

    /* The monster is now visible */
    if (flag)
    {
        /* Learn about the monster's mind */
        if (basic)
        {
            flags_set(lore->flags, RF_SIZE, RF_EMPTY_MIND, RF_WEIRD_MIND, RF_SMART, RF_STUPID,
                FLAG_END);
        }

        /* It was previously unseen */
        if (!monster_is_visible(p, mon->midx))
        {
            /* Mark as visible */
            mflag_on(p->mflag[mon->midx], MFLAG_VISIBLE);

            /* Draw the monster */
            square_light_spot_aux(p, c, &mon->grid);

            /* Update health bar as needed */
            update_health(who);

            /* Hack -- count "fresh" sightings */
            mon->race->lore.seen = 1;
            lore->pseen = 1;

            /* Redraw */
            p->upkeep->redraw |= (PR_MONLIST);
        }

        /* Efficiency -- notice multi-hued monsters */
        if (monster_shimmer(mon->race) && allow_shimmer(p))
            c->scan_monsters = true;
    }

    /* Not visible but was previously seen */
    else if (monster_is_visible(p, mon->midx))
    {
        /* Treat mimics differently */
        if (!mon->mimicked_obj || ignore_item_ok(p, mon->mimicked_obj))
        {
            /* Mark as not visible */
            mflag_off(p->mflag[mon->midx], MFLAG_VISIBLE);

            /* Erase the monster */
            square_light_spot_aux(p, c, &mon->grid);

            /* Update health bar as needed */
            update_health(who);

            /* Redraw */
            p->upkeep->redraw |= (PR_MONLIST);
        }
    }

    /* Is the monster now easily visible? */
    if (easy)
    {
        /* Change */
        if (!monster_is_in_view(p, mon->midx))
        {
            /* Mark as easily visible */
            mflag_on(p->mflag[mon->midx], MFLAG_VIEW);

            /* Disturb on appearance (except townies, friendlies and hidden mimics) */
            if (OPT(p, disturb_near) && (mon->level > 0) && pvm_check(p, mon) &&
                !monster_is_camouflaged(mon))
            {
                disturb(p, (p->firing_request? 3: 1));
            }

            /* Redraw */
            p->upkeep->redraw |= (PR_MONLIST);
        }
    }
    else
    {
        /* Change */
        if (monster_is_in_view(p, mon->midx))
        {
            /* Mark as not easily visible */
            mflag_off(p->mflag[mon->midx], MFLAG_VIEW);

            /* Redraw */
            p->upkeep->redraw |= (PR_MONLIST);
        }
    }
}


void update_mon(struct monster *mon, struct chunk *c, bool full)
{
    int i;
    bool blos = false;
    struct player *closest = NULL;
    int dis_to_closest = 9999, lowhp = 9999;
    struct source who_body;
    struct source *who = &who_body;

    my_assert(mon != NULL);
    source_monster(who, mon);

    /* Check for each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Make sure he's on the same dungeon level */
        if (!wpos_eq(&p->wpos, &mon->wpos)) continue;

        update_mon_aux(p, mon, c, full, &blos, &dis_to_closest, &closest, &lowhp);
    }

    /* Track closest player */
    if (full)
    {
        /* Forget player status */
        if (closest != mon->closest_player)
        {
            of_wipe(mon->known_pstate.flags);
            pf_wipe(mon->known_pstate.pflags);
            for (i = 0; i < ELEM_MAX; i++)
                mon->known_pstate.el_info[i].res_level = 0;
        }

        /* Always track closest player */
        mon->closest_player = closest;

        /* Paranoia -- make sure we found a closest player */
        if (closest) mon->cdis = dis_to_closest;
    }

    /* Update the cursor */
    update_cursor(who);
}


/*
 * Updates all the (non-dead) monsters via update_mon().
 */
void update_monsters(struct chunk *c, bool full)
{
    int i;

    /* Efficiency -- clear multi-hued flag */
    c->scan_monsters = false;

    /* Update each (live) monster */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);

        /* Update the monster if alive */
        if (mon->race) update_mon(mon, c, full);
    }
}


/*  
 * See if a monster can carry an object (it will pick up either way)
 */
static bool monster_can_carry(struct monster *mon, struct object *obj, bool force)
{
    int total_number = 0;
    struct object *held_obj;

    /* Always carry artifacts */
    if (obj->artifact) return true;

    /* Clones don't carry stuff */
    if (mon->clone) return false;

    /* Force to carry monster drops */
    if (force) return true;

    /* Only carry stuff in the dungeon */
    if (mon->wpos.depth == 0) return false;

#if !defined(MAX_MONSTER_BAG)
    return true;
#else
    /* Scan objects already being held for combination */
    for (held_obj = mon->held_obj; held_obj; held_obj = held_obj->next)
        total_number++;

    /*
     * Chance-based response. The closer monster to his limit, the less the chance is.
     * If he reached the limit, he will not pick up
     * XXX XXX XXX -- double chance && strict limit
     */
    if ((randint0(MAX_MONSTER_BAG) * 2 > total_number) && (total_number < MAX_MONSTER_BAG))
        return true;

    return false;
#endif
}


/*
 * Add the given object to the given monster's inventory.
 *
 * Returns true if the object is successfully added, false otherwise.
 */
bool monster_carry(struct monster *mon, struct object *obj, bool force)
{
    struct object *held_obj;

    /* See if the monster can carry the object */
    if (!monster_can_carry(mon, obj, force)) return false;

    /* Scan objects already being held for combination */
    for (held_obj = mon->held_obj; held_obj; held_obj = held_obj->next)
    {
        /* Check for combination */
        if (object_similar(NULL, held_obj, obj, OSTACK_MONSTER))
        {
            /* Combine the items */
            object_absorb(held_obj, obj);

            /* Result */
            return true;
        }
    }

    /* Forget location */
    loc_init(&obj->grid, 0, 0);

    /* Hack -- reset index */
    obj->oidx = 0;

    /* Link the object to the monster */
    obj->held_m_idx = mon->midx;
    memcpy(&obj->wpos, &mon->wpos, sizeof(struct worldpos));

    /* Add the object to the monster's inventory */
    pile_insert(&mon->held_obj, obj);

    /* Result */
    return true;
}


/*
 * Swap the players/monsters (if any) at two locations.
 */
void monster_swap(struct chunk *c, struct loc *grid1, struct loc *grid2)
{
    struct player *p;
    int m1, m2;
    struct monster *mon;
    struct loc *decoy = cave_find_decoy(c);

    /* Hack -- don't use grid1 and grid2 directly, they may refer to current player/monster grids */
    struct loc from, to;

    loc_copy(&from, grid1);
    loc_copy(&to, grid2);

    /* Monsters */
    m1 = square(c, &from)->mon;
    m2 = square(c, &to)->mon;

    /* Update grids */
    square_set_mon(c, &from, m2);
    square_set_mon(c, &to, m1);

    /* Monster 1 */
    if (m1 > 0)
    {
        mon = cave_monster(c, m1);

        /* Hack -- save previous monster location */
        loc_copy(&mon->old_grid, &mon->grid);

        /* Move monster */
        loc_copy(&mon->grid, &to);

        /* Update monster */
        update_mon(mon, c, true);

        /* Radiate light? */
        if (rf_has(mon->race->flags, RF_HAS_LIGHT)) update_view_all(&c->wpos, 0);
    }

    /* Player 1 */
    else if (m1 < 0)
    {
        p = player_get(0 - m1);

        /* Hack -- save previous player location */
        loc_copy(&p->old_grid, &p->grid);

        /* Move player */
        loc_copy(&p->grid, &to);

        /* Decoys get destroyed if player is too far away */
        if (!loc_is_zero(decoy) && (distance(decoy, &p->grid) > z_info->max_sight))
            square_destroy_decoy(p, c, decoy);

        /* Update the trap detection status */
        p->upkeep->redraw |= (PR_DTRAP);

        /* Redraw */
        p->upkeep->redraw |= (PR_FLOOR | PR_MONLIST | PR_ITEMLIST);
        p->upkeep->redraw |= (PR_SPELL | PR_STUDY);

        /* Updates */
        verify_panel(p);
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

        /* Radiate light? */
        if (p->state.cur_light) update_view_all(&c->wpos, 0 - m1);
    }

    /* Monster 2 */
    if (m2 > 0)
    {
        mon = cave_monster(c, m2); 

        /* Hack -- save previous monster location */
        loc_copy(&mon->old_grid, &mon->grid);

        /* Move monster */
        loc_copy(&mon->grid, &from);

        /* Update monster */
        update_mon(mon, c, true);

        /* Radiate light? */
        if (rf_has(mon->race->flags, RF_HAS_LIGHT)) update_view_all(&c->wpos, 0);
    }

    /* Player 2 */
    else if (m2 < 0)
    {
        p = player_get(0 - m2);

        /* Hack -- save previous player location */
        loc_copy(&p->old_grid, &p->grid);

        /* Move player */
        loc_copy(&p->grid, &from);

        /* Decoys get destroyed if player is too far away */
        if (!loc_is_zero(decoy) && (distance(decoy, &p->grid) > z_info->max_sight))
            square_destroy_decoy(p, c, decoy);

        /* Update the trap detection status */
        p->upkeep->redraw |= (PR_DTRAP);

        /* Redraw */
        p->upkeep->redraw |= (PR_FLOOR | PR_MONLIST | PR_ITEMLIST);
        p->upkeep->redraw |= (PR_SPELL | PR_STUDY);

        /* Updates */
        verify_panel(p);
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

        /* Radiate light? */
        if (p->state.cur_light) update_view_all(&c->wpos, 0 - m2);
    }

    /* Redraw */
    square_light_spot(c, &from);
    square_light_spot(c, &to);
}


/*
 * Make player fully aware of the given player.
 */
void aware_player(struct player *p, struct player *q)
{
    if (!q->k_idx) return;

    q->k_idx = 0;
    p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}


/*
 * Make player fully aware of the given mimic.
 *
 * When a player becomes aware of a mimic, we update the monster memory
 * and delete the "fake item" that the monster was mimicking.
 */
void become_aware(struct player *p, struct chunk *c, struct monster *mon)
{
    struct monster_lore *lore = (p? get_lore(p, mon->race): NULL);

    if (!monster_is_camouflaged(mon)) return;

    mon->camouflage = false;

    /* Learn about mimicry */
    if (lore && rf_has(mon->race->flags, RF_UNAWARE)) rf_on(lore->flags, RF_UNAWARE);

    /* Delete any false items */
    if (mon->mimicked_obj)
    {
        struct object *obj = mon->mimicked_obj;

        /* Print a message */
        if (p && square_isseen(p, &obj->grid))
        {
            char o_name[NORMAL_WID];

            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
            msg(p, "The %s was really a monster!", o_name);
        }

        /* Clear the mimicry */
        obj->mimicking_m_idx = 0;
        mon->mimicked_obj = NULL;

        square_excise_object(c, &obj->grid, obj);

        /* Give the object to the monster if appropriate */
        /* Otherwise delete the mimicked object */
        if (!rf_has(mon->race->flags, RF_MIMIC_INV) || !monster_carry(mon, obj, true))
            object_delete(&obj);
    }

    /* Delete any false features */
    if (mon->race->base == lookup_monster_base("feature mimic"))
    {
        /* Print a message */
        if (p) msg(p, "The %s was really a monster!", f_info[square(c, &mon->grid)->feat].name);

        /* Clear the feature */
        square_set_feat(c, &mon->grid, mon->feat);
    }

    /* Update monster and item lists */
    if (p)
    {
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
        p->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);
    }

    square_note_spot(c, &mon->grid);
    square_light_spot(c, &mon->grid);
}


/*
 * The given monster learns about an "observed" resistance or other player
 * state property, or lack of it.
 *
 * Note that this function is robust to being called with `element` as an
 * arbitrary PROJ_ type.
 */
void update_smart_learn(struct monster *mon, struct player *p, int flag, int pflag,
    int element)
{
    bool element_ok = ((element >= 0) && (element < ELEM_MAX));

    /* Sanity check */
    if (!flag && !element_ok) return;

    /* Anything a monster might learn, the player should learn */
    if (flag) equip_learn_flag(p, flag);
    if (element_ok) equip_learn_element(p, element);

    /* Not allowed to learn */
    if (!cfg_ai_learn) return;

    /* Too stupid to learn anything */
    if (monster_is_stupid(mon->race)) return;

    /* Not intelligent, only learn sometimes */
    if (!monster_is_smart(mon->race) && one_in_(2)) return;

    /* Analyze the knowledge; fail very rarely */
    if (one_in_(100)) return;

    /* Learn the flag */
    if (flag)
    {
        if (player_of_has(p, flag))
            of_on(mon->known_pstate.flags, flag);
        else
            of_off(mon->known_pstate.flags, flag);
    }

    /* Learn the pflag */
    if (pflag)
    {
        if (pf_has(p->state.pflags, pflag))
            of_on(mon->known_pstate.pflags, pflag);
        else
            of_off(mon->known_pstate.pflags, pflag);
    }

    /* Learn the element */
    if (element_ok)
        mon->known_pstate.el_info[element].res_level = p->state.el_info[element].res_level;
}


#define MAX_KIN_RADIUS      5
#define MAX_KIN_DISTANCE    5


/*
 * Given a dungeon chunk, a monster, and a location, see if there is
 * an injured monster with the same base kind in LOS and less than
 * MAX_KIN_DISTANCE away.
 */
static struct monster *get_injured_kin(struct chunk *c, const struct monster *mon, struct loc *grid)
{
    struct monster *kin = square_monster(c, grid);

    /* Ignore the monster itself */
    if (loc_eq(grid, &((struct monster *)mon)->grid)) return NULL;

    /* Check kin */
    if (!kin) return NULL;
    if (kin->race->base != mon->race->base) return NULL;

    /* Check line of sight */
    if (!los(c, &((struct monster *)mon)->grid, grid)) return NULL;

    /* Check injury */
    if (kin->hp == kin->maxhp) return NULL;

    /* Check distance */
    if (distance(&((struct monster *)mon)->grid, grid) > MAX_KIN_DISTANCE) return NULL;

    return kin;
}


/*
 * Find out if there are any injured monsters nearby.
 *
 * See get_injured_kin() above for more details on what monsters qualify.
 */
bool find_any_nearby_injured_kin(struct chunk *c, const struct monster *mon)
{
    struct loc grid;

    for (grid.y = mon->grid.y - MAX_KIN_RADIUS; grid.y <= mon->grid.y + MAX_KIN_RADIUS; grid.y++)
    {
        for (grid.x = mon->grid.x - MAX_KIN_RADIUS; grid.x <= mon->grid.x + MAX_KIN_RADIUS; grid.x++)
        {
            if (get_injured_kin(c, mon, &grid) != NULL) return true;
        }
    }

    return false;
}


/*
 * Choose one injured monster of the same base in LOS of the provided monster.
 *
 * Scan MAX_KIN_RADIUS grids around the monster to find potential grids,
 * make a list of kin, and choose a random one.
 */
struct monster *choose_nearby_injured_kin(struct chunk *c, const struct monster *mon)
{
    struct set *set = set_new();
    struct loc grid;
    struct monster *found;

    for (grid.y = mon->grid.y - MAX_KIN_RADIUS; grid.y <= mon->grid.y + MAX_KIN_RADIUS; grid.y++)
    {
        for (grid.x = mon->grid.x - MAX_KIN_RADIUS; grid.x <= mon->grid.x + MAX_KIN_RADIUS; grid.x++)
        {
            struct monster *kin = get_injured_kin(c, mon, &grid);

            if (kin != NULL) set_add(set, kin);
        }
    }

    found = set_choose(set);
    set_free(set);

    return found;
}


/*
 * Ends the game for players who killed the final boss Melkor.
 */
static void end_game(struct player *p, const struct monster *m)
{
    int i;

    /* Game over */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);
        bool party_win = (party_share_with(p, p->party, player) &&
            mflag_has(player->mflag[m->midx], MFLAG_HURT));

        /* Ends the game for the killer */
        /* Ends the game for party members on the same level... */
        /* ... only if they participated in the fight! */
        if ((player == p) || party_win)
        {
            /* Change the "title" */
            player->total_winner = 3;

            /* Redraw the "title" */
            player->upkeep->redraw |= (PR_TITLE);

            /* Congratulations */
            msg(player, "*** CONGRATULATIONS ***");
            msg(player, "You have finished the game!");

            /* Know inventory and home items */
            death_knowledge(player);

            /* Dump */
            my_strcpy(player->death_info.died_from, "winner", sizeof(player->death_info.died_from));
            player_dump(player, true);

            /* Retire */
            do_cmd_suicide(player);
        }
    }
}


/*
 * Give a better title to players who conquered the Nether Realm.
 */
static void better_title(struct player *p, const struct monster *m)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);
        bool party_win = (party_share_with(p, p->party, player) &&
            mflag_has(player->mflag[m->midx], MFLAG_HURT));

        /* Killer and party members who participated in the fight */
        if ((player == p) || party_win)
        {
            /* Change the "title" */
            player->total_winner = 2;

            /* Redraw the "title" */
            player->upkeep->redraw |= (PR_TITLE);

            /* Congratulations */
            msg(player, "*** CONGRATULATIONS ***");
            msg(player, "You have conquered the Nether Realm!");
        }
    }
}


/*
 * Handles the "death" of a monster.
 *
 * Disperses treasures carried by the monster centered at the monster location.
 * Note that objects dropped may disappear in crowded rooms.
 *
 * Checks for "Quest" completion when a quest monster is killed.
 */
void monster_death(struct player *p, struct chunk *c, struct monster *mon)
{
    int dump_item = 0;
    int dump_gold = 0;
    bool visible = (monster_is_visible(p, mon->midx) || monster_is_unique(mon->race));
    int winners;

    /* Reward the player with experience */
    monster_give_xp(p, c, mon, false);

    /* Hack -- killing Melkor ends the game */
    if (rf_has(mon->race->flags, RF_PWMANG_FINAL)) end_game(p, mon);

    /* Hack -- killing Xakaze gives a better title */
    if (mon->race->base == lookup_monster_base("nether")) better_title(p, mon);

    /* Hack -- killing Morgoth marks some players as "winners" */
    winners = quest_check(p, c, mon);

    /* Drop objects being carried */
    monster_drop_carried(p, c, mon, winners, visible, &dump_item, &dump_gold);

    /* Take note of any dropped treasure */
    if (visible && (dump_item || dump_gold))
        lore_treasure(p, mon, dump_item, dump_gold);

    /* Process quest monsters */
    end_quest(p, c, mon);

    /* Drop a corpse */
    monster_drop_corpse(p, c, mon);
}


/*
 * Handle the consequences of the killing of a monster by the player
 */
static void player_kill_monster(struct player *p, struct chunk *c, struct source *who, int note)
{
    struct monster *mon = who->monster;
    struct monster_lore *lore = get_lore(p, mon->race);
    char buf[MSG_LEN];
    char logbuf[MSG_LEN];
    int i;
    const char *title = get_title(p);
    bool cheeze;
    char m_name[NORMAL_WID];

    /* Assume normal death sound */
    int soundfx = MSG_KILL;

    /* Play a special sound if the monster was unique */
    if (monster_is_unique(mon->race))
    {
        if (mon->race->base == lookup_monster_base("Morgoth"))
            soundfx = MSG_KILL_KING;
        else
            soundfx = MSG_KILL_UNIQUE;
    }

    /* Extract monster name */
    monster_desc(p, m_name, sizeof(m_name), mon, MDESC_DEFAULT);

    /* Death message */
    switch (note)
    {
        /* Death by physical attack */
        case -2:
        {
            /* Make sure to flush any monster messages first */
            notice_stuff(p);

            /* Death by physical attack -- invisible monster */
            if (!monster_is_visible(p, mon->midx))
            {
                msg_format_near(p, MSG_GENERIC, " has killed %s.", m_name);
                msgt(p, soundfx, "You have killed %s.", m_name);
            }

            /* Death by physical attack -- unusual monster */
            else if (monster_is_destroyed(mon->race))
            {
                msg_format_near(p, MSG_GENERIC, " has destroyed %s.", m_name);
                msgt(p, soundfx, "You have destroyed %s.", m_name);
            }

            /* Death by physical attack -- normal monster */
            else
            {
                msg_format_near(p, MSG_GENERIC, " has slain %s.", m_name);
                msgt(p, soundfx, "You have slain %s.", m_name);
            }

            break;
        }

        /* Death by spell attack - messages handled by project_m() */
        case -1: break;

        /* Death by ranged attack */
        default:
        {
            char name[NORMAL_WID];

            monster_desc(p, name, sizeof(name), mon, MDESC_CAPITAL);
            switch (note)
            {
                case MON_MSG_DESTROYED:
                    msg_format_complex_near(p, MSG_GENERIC, "%s is destroyed.", name);
                    break;
                default:
                    msg_format_complex_near(p, MSG_GENERIC, "%s dies.", name);
            }
            add_monster_message(p, mon, note, true);
        }
    }

    /* Take note of the killer (only the first time!) */
    if (monster_is_unique(mon->race) && !lore->pkills)
    {
        /* Give credit to the killer */
        strnfmt(buf, sizeof(buf), "%s was slain by %s %s.", mon->race->name, title, p->name);

        /* Tell every player */
        msg_broadcast(p, buf, soundfx);

        /* Message for event history */
        strnfmt(logbuf, sizeof(logbuf), "Killed %s", mon->race->name);

        /* Record this kill in the event history */
        history_add_unique(p, logbuf, HIST_SLAY_UNIQUE);

        /* Give credit to party members who took part of the fight */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *q = player_get(i);

            if (q == p) continue;

            /* Same party, same level, helped to kill */
            if (party_share_with(p, p->party, q) && mflag_has(q->mflag[mon->midx], MFLAG_HURT))
            {
                /* Message for event history */
                strnfmt(logbuf, sizeof(logbuf), "Helped to kill %s", mon->race->name);

                /* Record this kill in the event history */
                history_add(q, logbuf, HIST_HELP_UNIQUE);
            }
        }
    }

    /* Killing an unique multiple times is cheezy! */
    /* Adding clones here to avoid cloning/breeding abuse */
    cheeze = ((monster_is_unique(mon->race) && lore->pkills) || mon->clone);

    /* Take note of the kill */
    if (lore->pkills < SHRT_MAX)
    {
        /* Remember */
        lore->pkills++;
    }

    /* Count kills in all lives */
    if (mon->race->lore.tkills < SHRT_MAX) mon->race->lore.tkills++;

    /* Update lore and tracking */
    lore_update(mon->race, lore);
    monster_race_track(p->upkeep, who);

    /* Should we absorb its soul? */
    if (p->timed[TMD_SOUL] && monster_is_living(mon))
    {
        int drain = 1 + (mon->level / 2) + p->lev * 4 / 5;
        if (drain > mon->maxhp) drain = mon->maxhp;
        msg(p, "You absorb the life of the dying soul.");
        hp_player_safe(p, 1 + drain / 2);
    }

    /* Cheezy kills give neither xp nor loot! */
    if (!cheeze) monster_death(p, c, mon);

    /* Bloodlust bonus */
    if (p->timed[TMD_BLOODLUST])
    {
        player_inc_timed(p, TMD_BLOODLUST, 10, false, false);
        player_over_exert(p, PY_EXERT_CONF, 5, 3);
        player_over_exert(p, PY_EXERT_HALLU, 5, 10);
    }

    /* Redraw */
    p->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);

    /* Radiate light? */
    if (rf_has(mon->race->flags, RF_HAS_LIGHT)) update_view_all(&c->wpos, 0);

    /* Delete the monster */
    delete_monster_idx(c, mon->midx);
}


/*
 * See how a monster reacts to damage
 */
static void monster_scared_by_damage(struct player *p, struct monster *mon, int dam, bool *fear)
{
    /* Pain can reduce or cancel existing fear, or cause fear */
    if (!(*fear) && mon->m_timed[MON_TMD_FEAR] && (dam > 0))
    {
        int tmp = randint1(dam);

        /* Cure a little or all fear */
        if (tmp < mon->m_timed[MON_TMD_FEAR])
        {
            /* Reduce fear */
            mon_dec_timed(p, mon, MON_TMD_FEAR, tmp, MON_TMD_FLG_NOMESSAGE);
        }
        else
        {
            /* Cure fear */
            mon_clear_timed(p, mon, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE);

            /* No more fear */
            (*fear) = false;
        }
    }

    /* Sometimes a monster gets scared by damage */
    if (!mon->m_timed[MON_TMD_FEAR] && !rf_has(mon->race->flags, RF_NO_FEAR))
    {
        int percentage;

        /* Percentage of fully healthy */
        percentage = (100L * mon->hp) / mon->maxhp;

        /*
         * Run (sometimes) if at 10% or less of max hit points,
         * or (usually) when hit for half its current hit points
         */
        if (((percentage <= 10) && CHANCE(percentage, 10)) || ((dam >= mon->hp) && magik(80)))
        {
            int timer = randint1(10) +
                (((dam >= mon->hp) && (percentage > 7))? 20: ((11 - percentage) * 5));

            /* Hack -- note fear */
            (*fear) = true;

            mon_inc_timed(p, mon, MON_TMD_FEAR, timer, MON_TMD_FLG_NOMESSAGE | MON_TMD_FLG_NOFAIL);
        }
    }
}


/*
 * Decreases a monster's hit points by `dam` and handle monster death.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * We announce monster death using an optional "death message" (`note`)
 * if given, and a otherwise a generic killed/destroyed message.
 *
 * Returns true if the monster has been killed (and deleted).
 */
bool mon_take_hit(struct player *p, struct chunk *c, struct monster *mon, int dam, bool *fear,
    int note)
{
    struct source who_body;
    struct source *who = &who_body;

    /* Redraw (later) if needed */
    source_monster(who, mon);
    update_health(who);

    /* Wake it up */
    mon_clear_timed(p, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
    mon_clear_timed(p, mon, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY);

    /* Become aware of its presence */
    if (monster_is_camouflaged(mon)) become_aware(p, c, mon);

    /* No damage, we're done */
    if (dam == 0) return false;

    /* Hurt it */
    mon->hp -= dam;
    mflag_on(p->mflag[mon->midx], MFLAG_HURT);

    /* It is dead now */
    if (mon->hp < 0)
    {
        player_kill_monster(p, c, who, note);

        /* Not afraid */
        (*fear) = false;

        /* Monster is dead */
        return true;
    }

    /* Did it get frightened? */
    monster_scared_by_damage(p, mon, dam, fear);

    /* Not dead yet */
    return false;
}


/*
 * Terrain damages monster
 */
void monster_take_terrain_damage(struct chunk *c, struct monster *mon)
{
    /* Damage the monster */
    if (monster_hates_grid(c, mon, &mon->grid))
    {
        int note_dies = MON_MSG_DIE;

        if (monster_is_destroyed(mon->race))
            note_dies = MON_MSG_DESTROYED;

        if (square_islava(c, &mon->grid) || square_isfiery(c, &mon->grid))
            note_dies = MON_MSG_BURN;

        if (square_iswater(c, &mon->grid))
            note_dies = MON_MSG_DROWN;

        if (square_isnether(c, &mon->grid))
            note_dies = MON_MSG_DRAIN;

        project_m_monster_attack_aux(NULL, c, mon, 100 + randint1(100), note_dies);
    }
}


static bool is_detected_p(struct player *p, struct player *q, int dis_esp)
{
    /* Full ESP */
    if (player_of_has(p, OF_ESP_ALL)) return true;

    /* Partial ESP */
    if (player_has(q, PF_ORC) && player_of_has(p, OF_ESP_ORC)) return true;
    if (player_has(q, PF_TROLL) && player_of_has(p, OF_ESP_TROLL)) return true;
    if (player_has(q, PF_GIANT) && player_of_has(p, OF_ESP_GIANT)) return true;
    if (player_has(q, PF_THUNDERLORD) && player_of_has(p, OF_ESP_DRAGON)) return true;
    if (player_has(q, PF_ANIMAL) && player_of_has(p, OF_ESP_ANIMAL)) return true;
    if (player_has(q, PF_DRAGON) && player_of_has(p, OF_ESP_DRAGON)) return true;
    if (q->ghost && player_of_has(p, OF_ESP_UNDEAD)) return true;

    /* Radius ESP */
    if (player_of_has(p, OF_ESP_RADIUS)) return (dis_esp <= z_info->max_sight);

    /* No ESP */
    return false;
}


static void update_player_aux(struct player *p, struct player *q, struct chunk *c)
{
    int d, d_esp;
    int id = get_player_index(get_connection(q->conn));
    struct loc grid1, grid2;

    /* Seen at all */
    bool flag = false;

    /* Seen by vision */
    bool easy = false;

    /* ESP permitted */
    bool telepathy_ok = true;

    bool isDM = ((p->dm_flags & DM_SEE_PLAYERS)? true: false);

    /* Compute distance */
    d = distance(&q->grid, &p->grid);

    /* Restrict distance */
    if (d > 255) d = 255;

    /* PWMAngband: telepathic awareness */
    loc_init(&grid1, q->grid.x / 3, q->grid.y);
    loc_init(&grid2, p->grid.x / 3, p->grid.y);
    d_esp = distance(&grid1, &grid2);
    if (d_esp > 255) d_esp = 255;

    /* Detected */
    if (p->play_det[id]) flag = true;

    /* Check if telepathy works */
    if (square_isno_esp(c, &q->grid) || square_isno_esp(c, &p->grid))
        telepathy_ok = false;

    /* Nearby */
    if ((d <= z_info->max_sight) || !cfg_limited_esp || isDM)
    {
        bool hasESP = is_detected_p(p, q, d_esp);
        bool isTL = (player_has(p, PF_THUNDERLORD) &&
            (d_esp <= (p->lev * z_info->max_sight / PY_MAX_LEVEL)));

        /* Basic telepathy */
        if (isDM || ((hasESP || isTL) && telepathy_ok))
        {
            /* Empty mind, no telepathy */
            if (q->poly_race && rf_has(q->poly_race->flags, RF_EMPTY_MIND)) {}

            /* Weird mind, occasional telepathy */
            else if (q->poly_race && rf_has(q->poly_race->flags, RF_WEIRD_MIND))
            {
                /* One in ten individuals are detectable */
                if ((id % 10) == 5)
                {
                    /* Detectable */
                    flag = true;

                    /* Check for LOS so that MFLAG_VIEW is set later */
                    if (square_isview(p, &q->grid)) easy = true;
                }
            }

            /* Normal mind, allow telepathy */
            else
            {
                /* Detectable */
                flag = true;

                /* Check for LOS so that MFLAG_VIEW is set later */
                if (square_isview(p, &q->grid)) easy = true;
            }

            /* DM has perfect ESP */
            if (isDM)
            {
                /* Detectable */
                flag = true;

                /* Check for LOS so that MFLAG_VIEW is set later */
                if (square_isview(p, &q->grid)) easy = true;
            }
        }

        /* Normal line of sight, and not blind */
        if (square_isview(p, &q->grid) && !p->timed[TMD_BLIND])
        {
            /* Use "infravision" */
            if (d <= p->state.see_infra)
            {
                /* Handle "cold blooded" players */
                if (q->poly_race && rf_has(q->poly_race->flags, RF_COLD_BLOOD)) {}

                /* Handle "warm blooded" players */
                else
                {
                    /* Easy to see */
                    easy = flag = true;
                }
            }

            /* Use "illumination" */
            if (square_isseen(p, &q->grid))
            {
                /* Handle "invisible" players */
                if (q->timed[TMD_INVIS])
                {
                    /* See invisible */
                    if (player_of_has(p, OF_SEE_INVIS))
                    {
                        /* Easy to see */
                        easy = flag = true;
                    }
                }

                /* Handle "normal" monsters */
                else
                {
                    /* Easy to see */
                    easy = flag = true;
                }
            }

            /* Learn about intervening squares */
            path_analyse(p, c, &q->grid);
        }

        /* Players in the same party are always visible */
        if (in_party(p, q->party)) easy = flag = true;

        /* Hack -- dungeon masters are invisible */
        if (q->dm_flags & DM_SECRET_PRESENCE) easy = flag = false;
    }

    /* Player is now visible */
    if (flag)
    {
        /* It was previously unseen */
        if (!player_is_visible(p, id))
        {
            /* Mark as visible */
            mflag_on(p->pflag[id], MFLAG_VISIBLE);

            /* Draw the player */
            square_light_spot_aux(p, c, &q->grid);
        }
        else
        {
            /* Player color may have changed! */
            square_light_spot_aux(p, c, &q->grid);
        }

        /* Efficiency -- notice multi-hued players */
        if (q->poly_race && monster_shimmer(q->poly_race) && allow_shimmer(p))
            q->shimmer = true;

        /* Efficiency -- notice party leaders */
        if (is_party_owner(p, q) && OPT(p, highlight_leader))
            q->shimmer = true;

        /* Efficiency -- notice elementalists */
        if (player_has(q, PF_ELEMENTAL_SPELLS) && allow_shimmer(p))
            q->shimmer = true;
    }

    /* The player is not visible */
    else
    {
        /* It was previously seen */
        if (player_is_visible(p, id))
        {
            /* Mark as not visible */
            mflag_off(p->pflag[id], MFLAG_VISIBLE);

            /* Erase the player */
            square_light_spot_aux(p, c, &q->grid);
        }
    }

    /* The player is now easily visible */
    if (easy)
    {
        /* Change */
        if (!player_is_in_view(p, id))
        {
            /* Mark as easily visible */
            mflag_on(p->pflag[id], MFLAG_VIEW);

            /* Disturb on appearance (except friendlies and hidden mimics) */
            if (OPT(p, disturb_near) && pvp_check(p, q, PVP_CHECK_ONE, true, 0x00) && !q->k_idx &&
                 !p->firing_request)
            {
                /* Disturb */
                disturb(p, 1);
            }
        }
    }

    /* The player is not easily visible */
    else
    {
        /* Change */
        if (player_is_in_view(p, id))
        {
            /* Mark as not easily visible */
            mflag_off(p->pflag[id], MFLAG_VIEW);
        }
    }
}


/*
 * This function updates the visibility flags for everyone who may see
 * this player.
 */
void update_player(struct player *q)
{
    int i, id = get_player_index(get_connection(q->conn));
    struct source who_body;
    struct source *who = &who_body;

    /* Efficiency -- clear "shimmer" flag */
    q->shimmer = false;

    /* Check for every other player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Skip players not on this level */
        if (!wpos_eq(&p->wpos, &q->wpos))
        {
            mflag_wipe(p->pflag[id]);
            p->play_det[id] = 0;
            continue;
        }

        /* Player can always see himself */
        if (q == p) continue;

        update_player_aux(p, q, chunk_get(&p->wpos));
    }

    source_player(who, 0, q);
    update_cursor(who);
}

/*
 * This function simply updates all the players (see above).
 */
void update_players(void)
{
    int i;

    /* Update each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Update the player */
        update_player(player_get(i));
    }
}


bool is_humanoid(const struct monster_race *race)
{
    return rf_has(race->flags, RF_HUMANOID);
}


/*
 * Half humanoid monsters: nagas (half snake/half human), hybrids,
 * driders (half spider/half human), human metamorphs
 */
bool is_half_humanoid(const struct monster_race *race)
{
    if ((race->base == lookup_monster_base("naga")) || strstr(race->name, "harpy") ||
        strstr(race->name, "taur") || streq(race->name, "Sphinx") ||
        streq(race->name, "Gorgon") || streq(race->name, "Drider") ||
        strstr(race->name, "Were")) return true;

    return false;
}


void update_monlist(struct monster *mon)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (wpos_eq(&p->wpos, &mon->wpos))
            p->upkeep->redraw |= PR_MONLIST;
    }
}


bool resist_undead_attacks(struct player *p, struct monster_race *race)
{
    return (rf_has(race->flags, RF_UNDEAD) && player_has(p, PF_UNDEAD_POWERS) &&
        (p->lev >= 6) && magik(40 + p->lev));
}


void update_view_all(struct worldpos *wpos, int skip)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        if (wpos_eq(&player->wpos, wpos) && (i != skip))
            player->upkeep->update |= PU_UPDATE_VIEW;
    }
}

