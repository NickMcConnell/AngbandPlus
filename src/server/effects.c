/*
 * File: effects.c
 * Purpose: Handler and auxiliary functions for every effect in the game
 *
 * Copyright (c) 2007 Andi Sidwell
 * Copyright (c) 2014 Ben Semmler, Nick McConnell
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


typedef struct effect_handler_context_s
{
    struct player *player;
    struct monster *mon;
    struct chunk *cave;
    effect_index effect;
    bool aware;
    int dir;
    int beam;
    int boost;
    random_value value;
    int p1, p2, p3;
    const char *self_msg;
    bool ident;
    quark_t note;
    int spell_power, elem_power;
    int flag;
    struct monster *target_m_ptr;
} effect_handler_context_t;


typedef bool (*effect_handler_f)(effect_handler_context_t *);


/*
 * Structure for effects
 */
struct effect_kind
{
    u16b index;                 /* Effect index */
    bool aim;                   /* Whether the effect requires aiming */
    const char *info;           /* Effect info (for spell tips) */
    effect_handler_f handler;   /* Function to perform the effect */
    const char *desc;           /* Effect description */
};


/*
 * Array of stat adjectives
 */
static const char *desc_stat_pos[] =
{
    #define STAT(a, b, c, d, e, f, g, h) f,
    #include "../common/list-stats.h"
    #undef STAT
    NULL
};


/*
 * Array of stat opposite adjectives
 */
const char *desc_stat_neg[] =
{
    #define STAT(a, b, c, d, e, f, g, h) g,
    #include "../common/list-stats.h"
    #undef STAT
    NULL
};


static int effect_calculate_value(effect_handler_context_t *context, bool use_boost)
{
    int final = 0;

    if (context->value.base > 0 || (context->value.dice > 0 && context->value.sides > 0))
        final = context->value.base + damroll(context->value.dice, context->value.sides);

    if (use_boost) final = final * (100 + context->boost) / 100;

    /* Hack -- elementalists */
    final = final * (20 + context->elem_power) / 20;

    return final;
}


/*
 * Apply the project() function in a direction, or at a target
 */
bool project_aimed(struct player *p, struct monster *mon, int typ, int dir, int dam, int flg,
    const char *what)
{
    int ty, tx;
    struct chunk *c = chunk_get(p->depth);
    struct actor source_body;
    struct actor *source = &source_body;

    /* Pass through the target if needed */
    flg |= (PROJECT_THRU);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    /* Use the given direction */
    tx = p->px + ddx[dir];
    ty = p->py + ddy[dir];

    /* Player or monster? */
    if (mon)
    {
        ACTOR_MONSTER(source, mon);
    }
    else
    {
        ACTOR_PLAYER(source, get_player_index(get_connection(p->conn)), p);

        /* Hack -- use an actual "target" */
        if ((dir == 5) && target_okay(p)) target_get(p, &tx, &ty);
    }

    /* Analyze the "dir" and the "target", do NOT explode */
    return (project(source, 0, c, ty, tx, dam, typ, flg, 0, 0, what));
}


/*
 * Apply the project() function to grids the player is touching
 */
static bool project_touch(struct player *p, int dam, int typ, bool aware)
{
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY | PROJECT_HIDE | PROJECT_THRU;
    struct actor who_body;
    struct actor *who = &who_body;

    ACTOR_PLAYER(who, get_player_index(get_connection(p->conn)), p);

    if (aware) flg |= PROJECT_AWARE;
    return (project(who, 1, chunk_get(p->depth), py, px, dam, typ, flg, 0, 0, "killed"));
}


/* Helpers */


/*
 * Set magical detection counter for a monster/player.
 *
 * Since the counter will be decremented at the end of the same turn, we set the counter to 2
 * to allow detection to last for one turn.
 */
static void give_detect(struct player *p, struct actor *who)
{
    /* Players */
    if (who->player) p->play_det[who->idx] = 2;

    /* Monsters */
    else p->mon_det[who->idx] = 2;
}


/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] =
{
    0, 10, 20, 40, 80, 160, 280, 400,
    550, 700, 800, 900, 950, 970, 990, 1000
};


/*
 * Tries to increase an items bonus score, if possible
 *
 * Returns true if the bonus was increased
 */
static bool enchant_score(s16b *score, bool is_artifact)
{
    int chance;

    /* Artifacts resist enchantment half the time */
    if (is_artifact && magik(50)) return false;

    /* Figure out the chance to enchant */
    if (*score < 0) chance = 0;
    else if (*score > 15) chance = 1000;
    else chance = enchant_table[*score];

    /* If we roll less-than-or-equal to chance, it fails */
    if (CHANCE(chance, 1000)) return false;

    /* Increment the score */
    ++*score;

    return true;
}


/*
 * Tries to uncurse a cursed item, if possible
 *
 * Returns true if a curse was broken
 */
static bool enchant_curse(struct player *p, struct object *obj, bool is_artifact)
{
    bitflag f[OF_SIZE];

    /* If the item isn't cursed (or is perma-cursed) this doesn't work */
    if (!cursed_p(obj->flags) || of_has(obj->flags, OF_PERMA_CURSE)) return false;

    /* Artifacts resist enchanting curses away half the time */
    if (is_artifact && magik(50)) return false;

    /* Normal items are uncursed 25% of the time */
    if (magik(75)) return false;

    /* Uncurse the item */
    msg(p, "The curse is broken!");
    create_mask(f, false, OFT_CURSE, OFT_MAX);
    of_diff(obj->flags, f);
    return true;
}


/*
 * Helper function for enchant() which tries to do the two things that
 * enchanting an item does, namely increasing its bonuses and breaking curses
 *
 * Returns true if a bonus was increased or a curse was broken
 */
static bool enchant_aux(struct player *p, struct object *obj, s16b *score)
{
    bool result = false;
    bool is_artifact = (obj->artifact? true: false);

    if (enchant_score(score, is_artifact)) result = true;
    if (enchant_curse(p, obj, is_artifact)) result = true;
    return result;
}


/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT   0x01
#define ENCH_TODAM   0x02
#define ENCH_TOBOTH  0x03
#define ENCH_TOAC    0x04


/*
 * Enchant an item
 *
 * Revamped! Now takes item pointer, number of times to try enchanting, and a
 * flag of what to try enchanting. Artifacts resist enchantment some of the
 * time. Also, any enchantment attempt (even unsuccessful) kicks off a parallel
 * attempt to uncurse a cursed item.
 *
 * Note that an item can technically be enchanted all the way to +15 if you
 * wait a very, very, long time.  Going from +9 to +10 only works about 5% of
 * the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and the larger
 * the pile, the lower the chance of success.
 *
 * Returns true if the item was changed in some way
 */
static bool enchant(struct player *p, struct object *obj, int n, int eflag)
{
    int i, prob;
    bool res = false;

    /* Magic ammo are always +0 +0 */
    if (magic_ammo_p(obj)) return false;

    /* Artifact ammo cannot be enchanted */
    if (tval_is_ammo(obj) && obj->artifact) return false;

    /* Mage weapons are always +0 +0 */
    if (tval_is_mstaff(obj)) return false;

    /* Large piles resist enchantment */
    prob = obj->number * 100;

    /* Missiles are easy to enchant */
    if (tval_is_ammo(obj)) prob = prob / 20;

    /* Try "n" times */
    for (i = 0; i < n; i++)
    {
        /* Roll for pile resistance */
        if (!CHANCE(100, prob)) continue;

        /* Try the three kinds of enchantment we can do */
        if ((eflag & ENCH_TOHIT) && enchant_aux(p, obj, &obj->to_h))
            res = true;
        if ((eflag & ENCH_TODAM) && enchant_aux(p, obj, &obj->to_d))
            res = true;
        if ((eflag & ENCH_TOAC) && enchant_aux(p, obj, &obj->to_a))
            res = true;
    }

    /* Failure */
    if (!res) return false;

    /* Recalculate bonuses, gear */
    p->upkeep->update |= (PU_BONUS | PU_INVEN);

    /* Combine the pack (later) */
    p->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);

    /* Success */
    return true;
}


/*
 * Apply a "project()" directly to all viewable monsters
 */
static bool project_los(struct player *p, int typ, int dam, bool obvious)
{
    int i, x, y;
    int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_PLAY | PROJECT_HIDE;
    struct actor who_body;
    struct actor *who = &who_body;
    struct chunk *c = chunk_get(p->depth);

    ACTOR_PLAYER(who, get_player_index(get_connection(p->conn)), p);

    if (obvious) flg |= PROJECT_AWARE;

    p->current_sound = -2;

    /* Affect all (nearby) monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Require line of sight */
        if (!square_isview(p, y, x)) continue;

        /* Jump directly to the target monster */
        if (project(who, 0, c, y, x, dam, typ, flg, 0, 0, "killed"))
            obvious = true;
    }

    /* Affect all (nearby) players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (p->depth != q->depth) continue;

        /* Skip ourself */
        if (q == p) continue;

        /* Location */
        y = q->py;
        x = q->px;

        /* Require line of sight */
        if (!square_isview(p, y, x)) continue;

        /* Jump directly to the target player */
        if (project(who, 0, c, y, x, dam, typ, flg, 0, 0, "killed"))
            obvious = true;
    }

    p->current_sound = -1;

    /* Result */
    return (obvious);
}


/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
static bool fire_beam(struct player *p, struct monster *mon, int typ, int dir, int dam,
    bool obvious)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_PLAY;

    if (obvious) flg |= PROJECT_AWARE;
    p->current_sound = -2;
    result = project_aimed(p, mon, typ, dir, dam, flg, "annihilated");
    p->current_sound = -1;
    return result;
}


/*  
 * Brand weapons (or ammo)
 *
 * Turns the (non-magical) object into an ego-item of type 'brand'.
 */
static void brand_object(struct player *p, struct object *obj, const char *brand,
    const char *name)
{
    int i;
    struct ego_item *ego;
    bool ok = false;

    /* You can never modify artifacts/ego-items */
    /* You can never modify worthless/cursed items */
    if (obj && !cursed_p(obj->flags) && obj->kind->cost && !obj->artifact && !obj->ego)
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

        /* Describe */
        msg(p, "The %s %s surrounded with an aura of %s.", o_name,
            ((obj->number > 1)? "are": "is"), name);

        /* Get the right ego type for the object */
        for (i = 0; i < z_info->e_max; i++)
        {
            ego = &e_info[i];

            /* Match the name */
            if (!ego->name) continue;
            if (streq(ego->name, brand))
            {
                struct ego_poss_item *poss;

                for (poss = ego->poss_items; poss; poss = poss->next)
                {
                    if (poss->kidx == obj->kind->kidx)
                        ok = true;
                }
            }
            if (ok) break;
        }

        /* Hack -- BRAND_COOL */
        if (streq(brand, "BRAND_COOL")) i = EGO_ELEMENTAL;

        /* Make it an ego item */
        obj->ego = &e_info[i];
        ego_apply_magic(obj, 0);
        object_notice_ego(p, obj);

        /* Update the gear */
        p->upkeep->update |= (PU_INVEN);

        /* Combine the pack (later) */
        p->upkeep->notice |= (PN_COMBINE);

        /* Redraw */
        p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

        /* Enchant */
        enchant(p, obj, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Hack -- BRAND_COOL */
        if (streq(brand, "BRAND_COOL"))
        {
            /* Brand the object */
            append_fixed_brand(&obj->brands, ELEM_COLD, 2);

            if (object_was_sensed(obj) || object_is_known(p, obj))
                object_notice_brands(p, obj, NULL);
        }

        /* Endless source of cash? No way... make them worthless */
        set_origin(obj, ORIGIN_WORTHLESS, p->depth, 0);
        if (object_was_sensed(obj) || object_is_known(p, obj))
            p->upkeep->notice |= PN_IGNORE;
    }
    else
        msg(p, "The branding failed.");
}


static bool light_line_aux(struct player *p, struct monster *mon, int dir, int typ, int dam)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL | PROJECT_PLAY;

    p->current_sound = -2;
    result = project_aimed(p, mon, typ, dir, dam, flg, "killed");
    p->current_sound = -1;
    return result;
}


/*
 * Detect "invisible" monsters around the player.
 */
static bool detect_monsters_invis(struct player *p, int y_dist, int x_dist, bool pause,
    bool aware)
{
    int i, y, x;
    int x1, x2, y1, y2;
    bool flag = false;
    struct actor who_body;
    struct actor *who = &who_body;
    struct chunk *c = chunk_get(p->depth);

    /* Pick an area to map */
    y1 = p->py - y_dist;
    y2 = p->py + y_dist;
    x1 = p->px - x_dist;
    x2 = p->px + x_dist;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);
        struct monster_lore *lore;

        /* Skip dead monsters */
        if (!mon->race) continue;

        lore = get_lore(p, mon->race);

        /* Skip visible monsters */
        if (mflag_has(p->mflag[i], MFLAG_VISIBLE)) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all invisible monsters */
        if (rf_has(mon->race->flags, RF_INVISIBLE))
        {
            struct actor_race *monster_race = &p->upkeep->monster_race;

            /* Take note that they are invisible */
            rf_on(lore->flags, RF_INVISIBLE);

            /* Update monster recall window */
            if (ACTOR_RACE_EQUAL(monster_race, mon)) p->upkeep->redraw |= (PR_MONSTER);

            /* Increment detection counter */
            ACTOR_MONSTER(who, mon);
            give_detect(p, who);

            /* Detect */
            flag = true;
        }
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip visible players */
        if (mflag_has(p->pflag[i], MFLAG_VISIBLE)) continue;

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (p->depth != q->depth) continue;

        /* Skip ourself */
        if (q == p) continue;

        /* Location */
        y = q->py;
        x = q->px;

        /* Only detect nearby players */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all invisible players */
        if (q->timed[TMD_INVIS])
        {
            /* Increment detection counter */
            ACTOR_PLAYER(who, i, q);
            give_detect(p, who);

            /* Detect */
            flag = true;
        }
    }

    /* Describe result, and clean up */
    if (flag && pause)
    {
        /* Hack -- fix the monsters and players */
        update_monsters(c, false);
        update_players();

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(p, "You sense the presence of invisible creatures!");
        party_msg_near(p, " senses the presence of invisible creatures!");

        /* Hack -- pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }
    else if (aware && !flag)
        msg(p, "You sense no invisible creatures.");

    /* Result */
    return (flag);
}


/*
 * Detect "normal" monsters around the player.
 */
static bool detect_monsters_normal(struct player *p, int y_dist, int x_dist, bool pause,
    bool aware)
{
    int i, y, x;
    int x1, x2, y1, y2;
    bool flag = false;
    struct actor who_body;
    struct actor *who = &who_body;
    struct chunk *c = chunk_get(p->depth);

    /* Pick an area to map */
    y1 = p->py - y_dist;
    y2 = p->py + y_dist;
    x1 = p->px - x_dist;
    x2 = p->px + x_dist;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Skip visible monsters */
        if (mflag_has(p->mflag[i], MFLAG_VISIBLE)) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all non-invisible, obvious monsters */
        if (!rf_has(mon->race->flags, RF_INVISIBLE) && !mon->unaware)
        {
            /* Increment detection counter */
            ACTOR_MONSTER(who, mon);
            give_detect(p, who);

            /* Detect */
            flag = true;
        }
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip visible players */
        if (mflag_has(p->pflag[i], MFLAG_VISIBLE)) continue;

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (p->depth != q->depth) continue;

        /* Skip ourself */
        if (q == p) continue;

        /* Location */
        y = q->py;
        x = q->px;

        /* Only detect nearby players */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all non-invisible, obvious players */
        if (!q->timed[TMD_INVIS] && !q->k_idx)
        {
            /* Increment detection counter */
            ACTOR_PLAYER(who, i, q);
            give_detect(p, who);

            /* Detect */
            flag = true;
        }
    }

    /* Describe and clean up */
    if (flag && pause)
    {
        /* Hack -- fix the monsters and players */
        update_monsters(c, false);
        update_players();

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(p, "You sense the presence of creatures!");
        party_msg_near(p, " senses the presence of creatures!");

        /* Hack -- pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }
    else if (aware && !flag)
        msg(p, "You sense no monsters.");

    /* Result */
    return (flag);
}


static struct player *get_inscribed_player(struct player *p, quark_t note)
{
    struct player *q = NULL;
    char* inscription = (char*)quark_str(note);

    /* Check for a valid inscription */
    if (inscription == NULL)
    {
        msg(p, "Nobody to use the power with.");
        return NULL;
    }

    /* Scan the inscription for @P */
    while ((*inscription != '\0') && !q)
    {
        if (*inscription == '@')
        {
            inscription++;

            /* A valid @P has been located */
            if (*inscription == 'P')
            {
                inscription++;
                q = player_lookup(inscription);
            }
        }
        inscription++;
    }

    if (!q) msg(p, "Player is not on.");

    return q;
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
static bool fire_bolt(struct player *p, struct monster *mon, int typ, int dir, int dam,
    bool obvious)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

    if (obvious) flg |= PROJECT_AWARE;
    return (project_aimed(p, mon, typ, dir, dam, flg, "annihilated"));
}


/* Effect handlers */


static bool effect_handler_ACQUIRE(effect_handler_context_t *context)
{
    int num = effect_calculate_value(context, false);

    acquirement(context->player, context->cave, num, 0);
    context->ident = true;
    return true;
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
static bool effect_handler_AGGRAVATE(effect_handler_context_t *context)
{
    int i;
    bool sleep = false;

    /* MvM -- disable */
    if (context->target_m_ptr) return true;

    /* Immediately obvious if the player did it */
    if (!context->mon) msg(context->player, "There is a high pitched humming noise.");

    /* Aggravate everyone nearby */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);
        int d;

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Skip aggravating monster (or player) */
        if (mon == context->mon) continue;

        /* Wake up nearby sleeping monsters */
        d = distance(context->player->py, context->player->px, mon->fy, mon->fx);
        if ((d < z_info->max_sight * 2) && mon->m_timed[MON_TMD_SLEEP])
        {
            mon_clear_timed(context->player, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, false);
            sleep = true;
        }

        /* Speed up monsters in line of sight */
        if (square_isview(context->player, mon->fy, mon->fx))
        {
            mon_inc_timed(context->player, mon, MON_TMD_FAST, 25, MON_TMD_FLG_NOTIFY, false);
            if (is_mimicking(mon)) become_aware(context->player, context->cave, mon);
        }
    }

    /* Messages */
    if (sleep) msg(context->player, "You hear a sudden stirring in the distance!");

    context->ident = true;
    return true;
}


/*
 * Cast an alter spell
 * Affect objects and grids (not monsters)
 */
static bool effect_handler_ALTER(effect_handler_context_t *context)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;

    if (project_aimed(context->player, context->mon, context->p1, context->dir, 0, flg, "killed"))
        context->ident = true;
    return true;
}


static bool effect_handler_ALTER_REALITY(effect_handler_context_t *context)
{
    int i;

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        msg(context->player, "You cannot alter this level...");
        return false;
    }

    /* Search for players on this depth */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Only players on this depth */
        if (q->depth != context->player->depth) continue;

        /* Tell the player about it */
        msg(q, "The world changes!");

        /* Generate a new level (later) */
        q->upkeep->new_level_method = LEVEL_RAND;
    }

    /* Deallocate the level */
    cave_wipe(context->cave);
    chunk_list_remove(context->player->depth);
    return true;
}


/*
 * Cast a ball spell
 * Stop if we hit a monster or the player, act as a ball
 * Allow target mode to pass over monsters
 * Affect grids, objects, and monsters
 */
static bool effect_handler_BALL(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int dam = effect_calculate_value(context, true);
    int rad = (context->p2? context->p2: 2);
    struct actor source_body;
    struct actor *source = &source_body;
    int ty, tx;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    ty = py + ddy[context->dir];
    tx = px + ddx[context->dir];

    /* Player or monster? */
    if (context->mon)
    {
        ACTOR_MONSTER(source, context->mon);
        if (rf_has(context->mon->race->flags, RF_POWERFUL)) rad++;
        flg &= ~(PROJECT_STOP | PROJECT_THRU);

        /* MvM */
        if (context->target_m_ptr)
        {
            ty = context->target_m_ptr->fy;
            tx = context->target_m_ptr->fx;
        }
    }
    else
    {
        if (context->p3) rad += context->player->lev / context->p3;

        /* Hack -- mimics */
        if (context->player->poly_race && rf_has(context->player->poly_race->flags, RF_POWERFUL))
            rad++;

        /* Hack -- elementalists */
        rad = rad + context->spell_power / 2;
        rad = rad * (20 + context->elem_power) / 20;

        ACTOR_PLAYER(source, get_player_index(get_connection(context->player->conn)),
            context->player);

        /* Ask for a target if no direction given */
        if ((context->dir == 5) && target_okay(context->player))
        {
            flg &= ~(PROJECT_STOP | PROJECT_THRU);
            target_get(context->player, &tx, &ty);
        }
    }

    /* Aim at the target, explode */
    context->player->current_sound = -2;
    if (project(source, rad, context->cave, ty, tx, dam, context->p1, flg, 0, 0, "annihilated"))
        context->ident = true;
    context->player->current_sound = -1;

    return true;
}


/*
 * Cast a ball spell which effect is obvious.
 * If context->p3 is negative, allow only on random levels.
 */
static bool effect_handler_BALL_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int rad = context->p2 + ((context->p3 > 0)? (context->player->lev / context->p3): 0);

    /* Only on random levels */
    if ((context->p3 < 0) && !random_level(context->player->depth))
    {
        msg(context->player, "You cannot create traps here...");
        return false;
    }

    if (fire_ball(context->player, context->p1, context->dir, dam, rad, true))
        context->ident = true;
    return true;
}


/*
 * Delete all non-unique monsters of a given "type" from the level
 */
static bool effect_handler_BANISH(effect_handler_context_t *context)
{
    int i;
    unsigned dam = 0;
    char typ;
    int d = 999, tmp;
    const char *pself = player_self(context->player);

    /* Search all monsters and find the closest */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip Unique Monsters */
        if (rf_has(mon->race->flags, RF_UNIQUE)) continue;

        /* Check distance */
        if ((tmp = distance(context->player->py, context->player->px, mon->fy, mon->fx)) < d)
        {
            /* Set closest distance */
            d = tmp;

            /* Set char */
            typ = mon->race->d_char;
        }
    }

    /* Check to make sure we found a monster */
    if (d == 999)
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* Delete the monsters of that "type" */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip Unique Monsters */
        if (rf_has(mon->race->flags, RF_UNIQUE)) continue;

        /* Skip "wrong" monsters */
        if (mon->race->d_char != typ) continue;

        /* Delete the monster */
        delete_monster_idx(context->cave, i);

        /* Take some damage */
        dam += randint1(4);
    }

    /* Hurt the player */
    strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
        "exhausted %s with Banishment", pself);
    take_hit(context->player, dam, "the strain of casting Banishment", false);

    /* Update monster list window */
    if (dam > 0)
    {
        context->player->upkeep->redraw |= (PR_MONLIST);
        context->ident = true;
    }

    /* Success */
    return true;
}


/*
 * Cast a beam spell
 * Pass through monsters, as a beam
 * Affect monsters (not grids or objects)
 */
static bool effect_handler_BEAM(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    fire_beam(context->player, context->mon, context->p1, context->dir, dam, false);
    if (!context->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Cast a beam spell which effect is obvious
 */
static bool effect_handler_BEAM_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    if (fire_beam(context->player, context->mon, context->p1, context->dir, dam, true))
        context->ident = true;
    return true;
}


/*
 * One Ring activation
 */
static bool effect_handler_BIZARRE(effect_handler_context_t *context)
{
    context->ident = true;

    /* Pick a random effect */
    switch (randint1(10))
    {
        case 1:
        case 2:
        {
            /* Message */
            msg(context->player, "You are surrounded by a malignant aura.");

            /* Decrease all stats (permanently) */
            player_stat_dec(context->player, STAT_STR, true);
            player_stat_dec(context->player, STAT_INT, true);
            player_stat_dec(context->player, STAT_WIS, true);
            player_stat_dec(context->player, STAT_DEX, true);
            player_stat_dec(context->player, STAT_CON, true);

            /* Lose some experience (permanently) */
            player_exp_lose(context->player, context->player->exp / 4, true);

            break;
        }

        case 3:
        {
            /* Message */
            msg(context->player, "You are surrounded by a powerful aura.");

            /* Dispel monsters */
            project_los(context->player, GF_DISP_ALL, 1000, false);

            break;
        }

        case 4:
        case 5:
        case 6:
        {
            /* Mana Ball */
            fire_ball(context->player, GF_MANA, context->dir, 300, 3, false);

            break;
        }

        case 7:
        case 8:
        case 9:
        case 10:
        {
            /* Mana Bolt */
            fire_bolt(context->player, context->mon, GF_MANA, context->dir, 250, false);

            break;
        }
    }
    return true;
}


/*
 * Cast a ball spell centered on the character
 */
static bool effect_handler_BLAST(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = context->p2 + (context->p3? (context->player->lev / context->p3): 0);

    /* Hack -- elementalists */
    rad = rad + context->spell_power / 2;
    rad = rad * (20 + context->elem_power) / 20;

    if (fire_ball(context->player, context->p1, 0, dam, rad, false))
        context->ident = true;
    return true;
}


/*
 * Cast a ball spell centered on the character (with obvious effects)
 */
static bool effect_handler_BLAST_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = context->p2 + (context->p3? (context->player->lev / context->p3): 0);

    /* Monster */
    if (context->mon)
    {
        int rlev = ((context->mon->race->level >= 1)? context->mon->race->level: 1);
        struct actor who_body;
        struct actor *who = &who_body;

        rad = context->p2 + (context->p3? (rlev / context->p3): 0);

        ACTOR_MONSTER(who, context->mon);
        project(who, rad, context->cave, context->mon->fy, context->mon->fx, 0, context->p1,
            PROJECT_ITEM | PROJECT_HIDE, 0, 0, "killed");
        update_smart_learn(context->mon, context->player, 0, 0, context->p1);
    }

    /* Player */
    else if (fire_ball(context->player, context->p1, 0, dam, rad, true))
        context->ident = true;

    return true;
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a bolt
 * Affect monsters (not grids or objects)
 *
 * PWMAngband: setting context->p2 is a hack for teleport other
 */
static bool effect_handler_BOLT(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    /* Hack -- teleport other */
    if (context->p2)
    {
        sound(context->player, MSG_TPOTHER);

        context->player->current_sound = -2;
        if (fire_bolt(context->player, context->mon, context->p1, context->dir, dam, false))
            context->ident = true;
        context->player->current_sound = -1;
    }

    /* MvM */
    else if (context->target_m_ptr)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_MONSTER(who, context->mon);
        project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, dam,
            context->p1, flag, 0, 0, "annihilated");
    }

    /* Normal case */
    else if (fire_bolt(context->player, context->mon, context->p1, context->dir, dam, false))
        context->ident = true;

    return true;
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a bolt
 * Affect monsters (not grids or objects)
 * Notice stuff based on awareness of the effect
 *
 * PWMAngband: if context->p2 is set, forbid on static levels
 */
static bool effect_handler_BOLT_AWARE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    /* Forbid in the town and on special levels */
    if (context->p2 && forbid_special(context->player->depth))
    {
        msg(context->player, "You cannot polymorph monsters here...");
        context->ident = true;
        return false;
    }

    if (fire_bolt(context->player, context->mon, context->p1, context->dir, dam, context->aware))
        context->ident = true;
    return true;
}


/*
 * Cast a melee range spell
 * Affect monsters (not grids or objects)
 */
static bool effect_handler_BOLT_MELEE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int ty, tx;
    struct actor who_body;
    struct actor *who = &who_body;

    ACTOR_PLAYER(who, get_player_index(get_connection(context->player->conn)), context->player);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    tx = context->player->px + ddx[context->dir];
    ty = context->player->py + ddy[context->dir];

    /* Hack -- use an actual "target" */
    if ((context->dir == 5) && target_okay(context->player))
    {
        target_get(context->player, &tx, &ty);

        /* Check distance */
        if (distance(context->player->py, context->player->px, ty, tx) > 1)
        {
            msg(context->player, "Target out of range.");
            return true;
        }
    }

    /* Analyze the "dir" and the "target", do NOT explode */
    if (project(who, 0, context->cave, ty, tx, dam, context->p1, PROJECT_KILL | PROJECT_PLAY, 0, 0,
        "annihilated"))
    {
        context->ident = true;
    }

    return true;
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 * context->p2 is any adjustment to the regular beam chance
 * context->p3 being set means to divide by the adjustment instead of adding
 */
static bool effect_handler_BOLT_OR_BEAM(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int beam = context->beam;

    if (context->p3) beam /= context->p2;
    else beam += context->p2;

    /* Hack -- space/time anchor */
    if (context->player->timed[TMD_ANCHOR] && (context->p1 == GF_TIME))
    {
        if (one_in_(3))
        {
            msg(context->player, "The space/time anchor stops your time bolt!");
            return true;
        }
        if (one_in_(3))
            player_clear_timed(context->player, TMD_ANCHOR, true);
    }

    if (magik(beam))
        fire_beam(context->player, context->mon, context->p1, context->dir, dam, false);
    else
        fire_bolt(context->player, context->mon, context->p1, context->dir, dam, false);
    if (!context->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a bolt
 * Affect monsters (not grids or objects)
 *
 * Like BOLT, but only identifies on noticing an effect
 */
static bool effect_handler_BOLT_STATUS(effect_handler_context_t *context)
{
    return effect_handler_BOLT(context);
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a bolt
 * Affect monsters (not grids or objects)
 *
 * The same as BOLT_STATUS, but done as a separate function to aid descriptions
 */
static bool effect_handler_BOLT_STATUS_DAM(effect_handler_context_t *context)
{
    return effect_handler_BOLT(context);
}


static bool effect_handler_BOW_BRAND(effect_handler_context_t *context)
{
    bitflag old_type = context->player->brand.type;
    bool old_blast = context->player->brand.blast;

    /* Set brand type and damage */
    context->player->brand.type = (bitflag)context->p1;
    context->player->brand.blast = (context->p2? true: false);
    context->player->brand.dam = (context->p2? effect_calculate_value(context, false): 0);

    /* Branding of the same type stacks */
    if (has_bowbrand(context->player, old_type, old_blast))
        player_inc_timed(context->player, TMD_BOWBRAND, context->player->lev, true, true);

    /* Apply new branding */
    else
    {
        /* Force the message display */
        context->player->timed[TMD_BOWBRAND] = 0;
        player_set_timed(context->player, TMD_BOWBRAND, context->player->lev + randint1(20), true);
    }

    return true;
}


static bool effect_handler_BOW_BRAND_SHOT(effect_handler_context_t *context)
{
    bitflag old_type = context->player->brand.type;
    bool old_blast = context->player->brand.blast;

    /* Set brand type and damage */
    context->player->brand.type = (bitflag)context->p1;
    context->player->brand.blast = false;
    context->player->brand.dam = effect_calculate_value(context, false);

    /* Branding of the same type stacks */
    if (has_bowbrand(context->player, old_type, old_blast))
        player_inc_timed(context->player, TMD_BOWBRAND, context->player->lev, true, true);

    /* Apply new branding */
    else
    {
        /* Force the message display */
        context->player->timed[TMD_BOWBRAND] = 0;
        player_set_timed(context->player, TMD_BOWBRAND, context->player->lev + randint1(20), true);
    }

    return true;
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(const struct object *obj)
{
    /* Ammo */
    if (!tval_is_ammo(obj)) return false;

    /* Magic ammo are always +0 +0 */
    if (kf_has(obj->kind->kind_flags, KF_AMMO_MAGIC)) return false;

    return true;
}


/*
 * Brand some (non-magical) ammo
 */
static bool effect_handler_BRAND_AMMO(effect_handler_context_t *context)
{
    struct object *obj;

    /* Get an item */
    if (context->player->current_value == ITEM_REQUEST)
    {
        get_item(context->player, HOOK_AMMO);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->player, context->player->current_value, true, true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->player, obj) && !is_owner(context->player, obj))
    {
        msg(context->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires ammo */
    if (!item_tester_hook_ammo(obj)) return false;

    /* Select the brand */
    if (one_in_(3))
        brand_object(context->player, obj, "of Flame", "flames");
    else if (one_in_(2))
        brand_object(context->player, obj, "of Frost", "frost");
    else
        brand_object(context->player, obj, "of Venom", "venom");

    /* Redraw */
    if (!object_is_carried(context->player, obj))
        redraw_floor(obj->depth, obj->iy, obj->ix);

    context->ident = true;
    return true;
}


/*
 * Brand the current weapon
 *
 * PWMAngband: if context->p2 is set, brand with weak frost instead of fire
 */
static bool effect_handler_BRAND_WEAPON(effect_handler_context_t *context)
{
    /* Hack -- branded with fire? */
    bool with_fire = (context->p2? false: true);

    struct object *obj = equipped_item_by_slot_name(context->player, "weapon");

    /* Select the brand */
    if (obj)
    {
        if (with_fire)
        {
            if (one_in_(2))
                brand_object(context->player, obj, "of Flame", "flames");
            else
                brand_object(context->player, obj, "of Frost", "frost");
        }
        else
        {
            if (one_in_(2))
                brand_object(context->player, obj, "BRAND_COOL", "weak frost");
            else
                brand_object(context->player, obj, "of Frost", "frost");
        }
    }

    context->ident = true;
    return true;
}


/*
 * Breathe an element, in a cone from the breath
 * Affect grids, objects, and monsters
 * context->p1 is element, context->p2 degrees of arc
 */
static bool effect_handler_BREATH(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int dam = effect_calculate_value(context, false);
    int type = context->p1;
    int rad = 0;
    struct actor source_body;
    struct actor *source = &source_body;
    int ty, tx;

    /*
     * Diameter of source starts at 20, so full strength only adjacent to
     * the breather.
     */
    int diameter_of_source = 20;
    int degrees_of_arc = context->p2;

    int flg = PROJECT_ARC | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

    /* Hack -- already used up */
    bool used = (context->p3 == 1);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    ty = py + ddy[context->dir];
    tx = px + ddx[context->dir];

    /* Radius of zero means no fixed limit. */
    if (rad == 0) rad = z_info->max_range;

    /* Player or monster? */
    if (context->mon)
    {
        ACTOR_MONSTER(source, context->mon);

        /* Breath parameters for monsters are monster-dependent */
        dam = breath_dam(type, context->mon->hp);

        /* Powerful monsters breathe wider arcs */
        if (rf_has(context->mon->race->flags, RF_POWERFUL))
        {
            diameter_of_source *= 2;
            degrees_of_arc *= 2;
        }

        /* MvM */
        if (context->target_m_ptr)
        {
            ty = context->target_m_ptr->fy;
            tx = context->target_m_ptr->fx;
        }
    }
    else
    {
        /* PWMAngband: let Power Dragon Scale Mails breathe a random element */
        if (type == GF_MISSILE)
        {
            bitflag mon_breath[RSF_SIZE];

            /* Allow all elements */
            rsf_wipe(mon_breath);
            init_spells(mon_breath);
            set_breath(mon_breath);

            /* Get breath effect */
            type = breath_effect(context->player, mon_breath);
        }

        /* Handle polymorphed players */
        else if (context->player->poly_race && (dam == 0))
        {
            const char *pself = player_self(context->player);

            /* Damage */
            dam = breath_dam(type, context->player->chp);

            /* Boost damage to take into account player hp vs monster hp */
            dam = (dam * 6) / 5;

            /* Breathing damages health instead of costing mana */
            strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
                "exhausted %s with breathing", pself);
            take_hit(context->player, context->player->mhp / 20, "the strain of breathing", false);
            if (context->player->is_dead) return !used;

            /* Powerful breath */
            if (rf_has(context->player->poly_race->flags, RF_POWERFUL))
            {
                diameter_of_source *= 2;
                degrees_of_arc *= 2;
            }
        }

        ACTOR_PLAYER(source, get_player_index(get_connection(context->player->conn)),
            context->player);

        /* Ask for a target if no direction given */
        if (context->dir == 5)
        {
            if (target_okay(context->player))
                target_get(context->player, &tx, &ty);

            /* Hack -- no target available, default to random direction */
            else
                context->dir = 0;
        }

        /* Hack -- no direction given, default to random direction */
        if (!context->dir)
        {
            context->dir = ddd[randint0(8)];
            ty = py + ddy[context->dir];
            tx = px + ddx[context->dir];
        }
    }

    /* Diameter of the energy source. */
    if (degrees_of_arc < 60)
    {
        /* This handles finite length beams */
        if (degrees_of_arc == 0)
            diameter_of_source = rad * 10;
        /*
         * Narrower cone means energy drops off less quickly. 30 degree
         * breaths are still full strength 3 grids from the breather,
         * and 20 degree breaths are still full strength at 5 grids.
         */
        else
            diameter_of_source = diameter_of_source * 60 / degrees_of_arc;
    }

    /* Max */
    if (diameter_of_source > 250) diameter_of_source = 250;

    /* Breathe at the target */
    context->player->current_sound = -2;
    if (project(source, rad, context->cave, ty, tx, dam, type, flg, degrees_of_arc,
        diameter_of_source, "vaporized"))
    {
        context->ident = true;
    }
    context->player->current_sound = -1;

    return !used;
}


static bool effect_handler_CLOAK_CHANGT(effect_handler_context_t *context)
{
    int what;
    int i, tries = 200;
    int dur = effect_calculate_value(context, false);

    while (--tries)
    {
        struct player *q;

        /* 1 < i < NumPlayers */
        i = randint1(NumPlayers);

        q = player_get(i);

        /* Disguising into a rogue is .. mhh ... stupid */
        if (q->clazz->cidx == context->player->clazz->cidx) continue;

        /* Ok we found a good class lets mimic */
        what = q->clazz->cidx;
        break;
    }

    /* Arg nothing .. bah be a warrior */
    if (!tries) what = 0;

    context->player->tim_mimic_what = what;
    player_set_timed(context->player, TMD_MIMIC, dur, true);
    return true;
}


static bool effect_handler_CONFUSING(effect_handler_context_t *context)
{
    if (!context->player->confusing)
    {
        msg(context->player, "Your hands begin to glow.");
        context->player->confusing = true;
        context->ident = true;
    }
    return true;
}


static bool effect_handler_CREATE_HOUSE(effect_handler_context_t *context)
{
    context->ident = true;

    /* MAngband house creation code disabled for now */
    /*return create_house(p);*/

    return build_house(context->player);
}


/*
 * Create potions of poison from any potion
 */
static bool effect_handler_CREATE_POISON(effect_handler_context_t *context)
{
    struct object *obj, *poison;
    int amt;

    /* Get an item */
    if (context->player->current_value == ITEM_REQUEST)
    {
        get_item(context->player, HOOK_POISON);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->player, context->player->current_value, true, true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->player, obj) && !is_owner(context->player, obj))
    {
        msg(context->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires a potion */
    if (!tval_is_potion(obj)) return false;

    /* Amount */
    amt = obj->number;

    /* Message */
    msg(context->player, "You create %d potions of poison.", amt);

    /* Eliminate the item */
    use_object(context->player, obj, amt, false);

    /* Create the potions */
    poison = object_new();
    object_prep(context->player, poison, lookup_kind_by_name(TV_POTION, "Poison"), 0, MINIMISE);
    poison->number = amt;

    /* Set origin */
    set_origin(poison, ORIGIN_ACQUIRE, context->player->depth, 0);

    drop_near(context->player, context->cave, poison, 0, context->player->py, context->player->px,
        true, DROP_FADE);

    return true;
}


/*
 * Create stairs at the player location
 */
static bool effect_handler_CREATE_STAIRS(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;

    context->ident = true;

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        msg(context->player, "You cannot create stairs here...");
        return false;
    }

    /* Only allow stairs to be created on empty floor */
    if (!square_isfloor(context->cave, py, px))
    {
        msg(context->player, "There is no empty floor here.");
        return false;
    }

    /* Forbidden */
    if ((context->cave->depth == z_info->max_depth - 1) && (cfg_limit_stairs >= 2))
    {
        msg(context->player, "You cannot create stairs here...");
        return false;
    }

    /* Push objects off the grid */
    if (square_object(context->cave, py, px)) push_object(context->player, context->cave, py, px);

    square_add_stairs(context->cave, py, px, context->cave->depth);

    return true;
}


static bool effect_handler_CREATE_WALLS(effect_handler_context_t *context)
{
    int num = effect_calculate_value(context, false);

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        msg(context->player, "You cannot create walls here...");
        return false;
    }

    if (num)
    {
        int y;
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_PLAYER(who, get_player_index(get_connection(context->player->conn)), context->player);

        for (y = 0; y < num; y++)
        {
            int dir = ddd[randint0(8)];

            project(who, 0, context->cave, context->player->py + ddy[dir],
                context->player->px + ddx[dir], 0, GF_STONE_WALL, PROJECT_GRID, 0, 0, "killed");
        }

        return true;
    }

    fire_ball(context->player, GF_STONE_WALL, 0, 1, 1, false);
    return true;
}


static bool effect_handler_CRUNCH(effect_handler_context_t *context)
{
    if (!player_undead(context->player))
    {
        if (one_in_(2))
            msg(context->player, "It's crunchy.");
        else
            msg(context->player, "It nearly breaks your tooth!");
    }
    context->ident = true;
    return true;
}


/*
 * Cure a player status condition.
 */
static bool effect_handler_CURE(effect_handler_context_t *context)
{
    int type = context->p1;

    if (player_clear_timed(context->player, type, true)) context->ident = true;
    return true;
}


#if 0
/*
 * Curse the player's armor
 */
static bool effect_handler_CURSE_ARMOR(effect_handler_context_t *context)
{
    struct object *obj;
    char o_name[NORMAL_WID];

    /* Curse the body armor */
    obj = equipped_item_by_slot_name(context->player, "body");

    /* Nothing to curse */
    if (!obj)
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* Describe */
    object_desc(context->player, o_name, sizeof(o_name), obj, ODESC_FULL);

    /* Attempt a saving throw for artifacts */
    if (obj->artifact && magik(50))
    {
        /* Cool */
        msg(context->player,
            "A terrible black aura tries to surround your armor, but your %s resists the effects!",
            o_name);
    }

    /* Not artifact or failed save... */
    else
    {
        /* Oops */
        msg(context->player, "A terrible black aura blasts your %s!", o_name);

        /* Damage the armor */
        obj->to_a -= randint1(3);

        /* Curse it */
        flags_set(obj->flags, OF_SIZE, OF_LIGHT_CURSE, OF_HEAVY_CURSE, FLAG_END);

        /* Recalculate bonuses */
        context->player->upkeep->update |= (PU_BONUS);

        /* Redraw */
        context->player->upkeep->redraw |= (PR_EQUIP);
    }

    /* Notice */
    context->ident = true;
    return true;
}


/*
 * Curse the player's weapon
 */
static bool effect_handler_CURSE_WEAPON(effect_handler_context_t *context)
{
    struct object *obj;
    char o_name[NORMAL_WID];

    /* Curse the weapon */
    obj = equipped_item_by_slot_name(context->player, "weapon");

    /* Nothing to curse */
    if (!obj)
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* Describe */
    object_desc(context->player, o_name, sizeof(o_name), obj, ODESC_FULL);

    /* Attempt a saving throw */
    if (obj->artifact && magik(50))
    {
        /* Cool */
        msg(context->player,
            "A terrible black aura tries to surround your weapon, but your %s resists the effects!",
            o_name);
    }

    /* Not artifact or failed save... */
    else
    {
        /* Oops */
        msg(context->player, "A terrible black aura blasts your %s!", o_name);

        /* Damage the weapon */
        obj->to_h -= randint1(3);
        obj->to_d -= randint1(3);

        /* Curse it */
        flags_set(obj->flags, OF_SIZE, OF_LIGHT_CURSE, OF_HEAVY_CURSE, FLAG_END);

        /* Recalculate bonuses */
        context->player->upkeep->update |= (PU_BONUS);

        /* Redraw */
        context->player->upkeep->redraw |= (PR_EQUIP);
    }

    /* Notice */
    context->ident = true;
    return true;
}
#endif


/*
 * Deal damage from the current monster to the player
 */
static bool effect_handler_DAMAGE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    char ddesc[NORMAL_WID];
    const char *what = "annihilated";

    if (!context->mon) return true;

    /* MvM */
    if (context->target_m_ptr)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_MONSTER(who, context->mon);
        project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, dam,
            context->p1, flag, 0, 0, "annihilated");

        return true;
    }

    /* Get the "died from" name in case this attack kills @ */
    monster_desc(context->player, ddesc, sizeof(ddesc), context->mon, MDESC_DIED_FROM);

    if ((context->p1 == GF_BLAST) || (context->p1 == GF_SMASH))
        what = "turned into an unthinking vegetable";
    strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor), "was %s by %s",
        what, ddesc);

    /* Hit the player */
    take_hit(context->player, dam, ddesc, true);

    return true;
}


/*
 * Call darkness around the player
 * Affect all monsters in the projection radius (context->p2)
 */
static bool effect_handler_DARKEN_AREA(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int dam = effect_calculate_value(context, false);
    int rad = context->p2;
    int flg = PROJECT_GRID | PROJECT_KILL | PROJECT_PLAY;
    struct actor who_body;
    struct actor *who = &who_body;

    /* No effect outside of the dungeon during day */
    if ((context->cave->depth <= 0) && is_daytime())
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* No effect on special levels */
    if (special_level(context->cave->depth))
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* MvM */
    if (context->target_m_ptr)
    {
        py = context->target_m_ptr->fy;
        px = context->target_m_ptr->fx;
        ACTOR_MONSTER(who, context->mon);
    }
    else
    {
        /* Message */
        if (!context->player->timed[TMD_BLIND])
            msg(context->player, "Darkness surrounds you.");

        ACTOR_PLAYER(who, get_player_index(get_connection(context->player->conn)), context->player);
    }

    /* Hook into the "project()" function */
    project(who, rad, context->cave, py, px, dam, GF_DARK_WEAK, flg, 0, 0, "killed");

    /* Darken the room */
    light_room(context->player, context->cave, py, px, false);

    /* Hack -- blind the player directly if player-cast */
    if (!context->mon)
    {
        if (!player_resists(context->player, ELEM_DARK))
            player_inc_timed(context->player, TMD_BLIND, 3 + randint1(5), true, true);
        equip_notice_element(context->player, ELEM_DARK);
    }

    /* Assume seen */
    context->ident = true;
    return true;
}


/*
 * Turn a player into an undead being
 */
static void player_turn_undead(struct player *p)
{
    int i;

    /* Hack -- note "death" */
    msgt(p, MSG_DEATH, "You turn into an undead being.");
    message_flush(p);

    /* Handle polymorphed players */
    if (p->poly_race) do_cmd_poly(p, NULL, false, true);

    /* Cancel current effects */
    for (i = 0; i < TMD_MAX; i++) player_clear_timed(p, i, true);

    /* Turn him into an undead being */
    set_ghost_flag(p, 2, true);

    /* Give him his hit points and mana points back */
    restore_hp(p);
    restore_sp(p);

    /* Feed him */
    player_set_food(p, PY_FOOD_MAX - 1);

    /* Cancel any WOR spells */
    p->word_recall = 0;
    p->deep_descent = 0;

    /* Notice, update and redraw */
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->update |= (PU_BONUS | PU_INVEN);
    p->upkeep->redraw |= (PR_STATE | PR_BASIC | PR_PLUSSES | PR_INVEN | PR_SPELL);
}


static bool effect_handler_DEATH(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    /* Not when already undead */
    if (context->player->ghost)
    {
        msg(context->player, "You call upon Death... but nothing happens.");
        return false;
    }

    /* Kill opponent */
    fire_bolt(context->player, context->mon, GF_DEATH, context->dir, dam, false);

    /* Turn him into an undead being */
    player_turn_undead(context->player);

    return true;
}


static void set_descent(struct player *p)
{
    /* Set the timer */
    msg(p, "The air around you starts to swirl...");
    msg_misc(p, " is surrounded by a swirling aura...");
    p->deep_descent = 3 + randint1(4);

    /* Redraw the state (later) */
    p->upkeep->redraw |= (PR_STATE);
}


/*
 * Teleports 5 dungeon levels down (from max_depth)
 *
 * PWMAngband: set context->p2 to activate the descent
 */
static bool effect_handler_DEEP_DESCENT(effect_handler_context_t *context)
{
    int target_increment, target_depth;
    bool apply = (context->p2? true: false);

    /* Special case: wilderness level */
    if (context->player->depth < 0)
    {
        /* Don't apply effect while in the wilderness */
        if (apply) return false;

        /* Set the timer */
        set_descent(context->player);
        context->ident = true;

        return true;
    }

    /* Calculate target depth */
    target_increment = (4 / z_info->stair_skip) + 1;
    target_depth = dungeon_get_next_level(context->player, context->player->max_depth,
        target_increment);

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(target_depth))
    {
        /* Don't apply effect while DM is redesigning the level */
        if (apply) return false;

        /* Set the timer */
        set_descent(context->player);
        context->ident = true;

        return true;
    }

    /* Determine the level */
    if (target_depth > context->player->depth)
    {
        /* Set the timer */
        if (!apply)
        {
            set_descent(context->player);
            context->ident = true;

            return true;
        }

        /* Change location */
        disturb(context->player, 0);
        msgt(context->player, MSG_TPLEVEL, "The floor opens beneath you!");
        msg_misc(context->player, " sinks through the floor!");
        dungeon_change_level(context->player, context->cave, target_depth, LEVEL_RAND);
        return true;
    }

    /* Just print a message when unable to set the timer */
    if (!apply)
    {
        msg(context->player, "You sense a malevolent presence blocking passage to the levels below.");
        context->ident = true;
        return true;
    }

    /* Otherwise do something disastrous */
    msg(context->player, "You are thrown back in an explosion!");
    effect_simple(context->player, EF_DESTRUCTION, "0", 0, 5, 1, NULL, NULL);
    return true;
}


/*
 * Unlight the dungeon map.
 */
static bool effect_handler_DEEP_NIGHTS(effect_handler_context_t *context)
{
    int i;

    /* No effect outside of the dungeon during day */
    if ((context->player->depth <= 0) && is_daytime())
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* No effect on special levels */
    if (special_level(context->player->depth))
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* Check for every other player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);
        struct object *obj;

        /* Only works for players on the level */
        if (context->player->depth != player->depth) continue;

        /* Get the light source */
        obj = equipped_item_by_slot_name(player, "light");

        /* Bye bye light */
        if (obj && (obj->timeout > 0) && !of_has(obj->flags, OF_NO_FUEL))
        {
            msg(player, "Your light suddently empty.");

            /* No more light, it's Rogues day today :) */
            obj->timeout = 0;

            /* Redraw */
            player->upkeep->redraw |= (PR_EQUIP);
        }

        /* Forget every grid */
        wiz_dark(player);
    }

    return true;
}


/*
 * The destruction effect
 *
 * This effect "deletes" monsters (instead of killing them).
 *
 * This is always an effect centred on the player; it is similar to the
 * earthquake effect.
 *
 * PWMAngband: the radius is set in context->value.base (unless specified directly); if
 * context->p3 is set, destroy the area silently.
 */
static bool effect_handler_DESTRUCTION(effect_handler_context_t *context)
{
    int y, x, k, r = effect_calculate_value(context, false);
    int y1 = context->player->py;
    int x1 = context->player->px;
    int hurt[MAX_PLAYERS];
    int count = 0;

    if (context->p2) r = context->p2;
    context->ident = true;

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        if (!context->p3) msg(context->player, "The ground shakes for a moment.");
        return true;
    }

    if (!context->p3) msg_misc(context->player, " unleashes great power!");

    /* Big area of affect */
    for (y = (y1 - r); y <= (y1 + r); y++)
    {
        for (x = (x1 - r); x <= (x1 + r); x++)
        {
            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Extract the distance */
            k = distance(y1, x1, y, x);

            /* Stay in the circle of death */
            if (k > r) continue;

            /* Lose room and vault */
            sqinfo_off(context->cave->squares[y][x].info, SQUARE_ROOM);
            sqinfo_off(context->cave->squares[y][x].info, SQUARE_VAULT);
            sqinfo_off(context->cave->squares[y][x].info, SQUARE_NO_TELEPORT);
            if (square_ispitfloor(context->cave, y, x)) square_clear_feat(context->cave, y, x);

            /* Lose light */
            square_unglow(context->cave, y, x);
            square_light_spot(context->cave, y, x);

            /* Hack -- notice player affect */
            if (context->cave->squares[y][x].mon < 0)
            {
                /* Hurt the player later */
                hurt[count] = 0 - context->cave->squares[y][x].mon;
                count++;

                /* Do not hurt this grid */
                continue;
            }

            /* Hack -- skip the epicenter */
            if ((y == y1) && (x == x1)) continue;

            /* Delete the monster (if any) */
            delete_monster(context->cave, y, x);
            if (square_ispitfloor(context->cave, y, x)) square_clear_feat(context->cave, y, x);

            /* Don't remove stairs */
            if (square_isstairs(context->cave, y, x)) continue;

            /* Destroy any grid that isn't a permanent wall */
            if (!square_isperm(context->cave, y, x))
            {
                /* Delete objects */
                square_excise_pile(context->cave, y, x);
                square_destroy(context->cave, y, x);
            }
        }
    }

    /* Hack -- affect players */
    for (k = 0; k < count; k++)
    {
        struct player *p = player_get(hurt[k]);

        /* Message */
        msg(p, "There is a searing blast of light!");

        /* Blind the player */
        equip_notice_element(p, ELEM_LIGHT);
        if (!player_resists(p, ELEM_LIGHT))
            player_inc_timed(p, TMD_BLIND, 10 + randint1(10), true, true);

        /* Fully update the visuals */
        p->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow */
        p->upkeep->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

        /* Redraw */
        p->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);
    }

    return true;
}


/*
 * Detect all monsters on the level.
 */
static bool effect_handler_DETECT_ALL_MONSTERS(effect_handler_context_t *context)
{
    int i;
    bool detect = false;
    struct actor who_body;
    struct actor *who = &who_body;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Increment detection counter */
        ACTOR_MONSTER(who, mon);
        give_detect(context->player, who);

        /* Detect */
        detect = true;
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (context->player->depth != q->depth) continue;

        /* Skip ourself */
        if (q == context->player) continue;

        /* Increment detection counter */
        ACTOR_PLAYER(who, i, q);
        give_detect(context->player, who);

        /* Detect */
        detect = true;
    }

    /* Describe result, and clean up */
    if (detect)
    {
        /* Hack -- fix the monsters and players */
        update_monsters(context->cave, false);
        update_players();

        /* Full refresh (includes monster/object lists) */
        context->player->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(context->player);

        /* Normal refresh (without monster/object lists) */
        context->player->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(context->player, "An image of all nearby life-forms appears in your mind!");
        party_msg_near(context->player, " senses the presence of all nearby life-forms!");

        /* Hack -- pause */
        if (OPT_P(context->player, pause_after_detect)) Send_pause(context->player);
    }
    else
        msg(context->player, "The level is devoid of life.");

    /* Result */
    if (detect)
        context->ident = true;
    return true;
}


/*
 * Detect evil monsters around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_EVIL(effect_handler_context_t *context)
{
    int i, x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;
    bool monsters = false;
    struct actor who_body;
    struct actor *who = &who_body;

    /* Pick an area to map */
    y1 = context->player->py - y_dist;
    y2 = context->player->py + y_dist;
    x1 = context->player->px - x_dist;
    x2 = context->player->px + x_dist;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);
        struct monster_lore *lore;

        /* Skip dead monsters */
        if (!mon->race) continue;

        lore = get_lore(context->player, mon->race);

        /* Skip visible monsters */
        if (mflag_has(context->player->mflag[i], MFLAG_VISIBLE)) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect evil monsters */
        if (rf_has(mon->race->flags, RF_EVIL))
        {
            struct actor_race *monster_race = &context->player->upkeep->monster_race;

            /* Take note that they are evil */
            rf_on(lore->flags, RF_EVIL);

            /* Update monster recall window */
            if (ACTOR_RACE_EQUAL(monster_race, mon))
                context->player->upkeep->redraw |= (PR_MONSTER);

            /* Increment detection counter */
            ACTOR_MONSTER(who, mon);
            give_detect(context->player, who);

            /* Detect */
            monsters = true;
            context->ident = true;
        }
    }

    /* Note effects and clean up */
    if (monsters)
    {
        /* Hack -- fix the monsters */
        update_monsters(context->cave, false);

        /* Full refresh (includes monster/object lists) */
        context->player->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(context->player);

        /* Normal refresh (without monster/object lists) */
        context->player->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(context->player, "You sense the presence of evil creatures!");
        party_msg_near(context->player, " senses the presence of evil creatures!");

        /* Hack -- pause */
        if (OPT_P(context->player, pause_after_detect)) Send_pause(context->player);
    }
    else if (context->aware)
        msg(context->player, "You sense no evil creatures.");

    return true;
}


/*
 * Detect doors and stairs around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_FEATURES(effect_handler_context_t *context)
{
    int x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;
    bool doors = false, stairs = false;

    /* Pick an area to map */
    y1 = context->player->py - y_dist;
    y2 = context->player->py + y_dist;
    x1 = context->player->px - x_dist;
    x2 = context->player->px + x_dist;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Detect secret doors */
            if (square_issecretdoor(context->cave, y, x))
            {
                /* Pick a door */
                place_closed_door(context->cave, y, x);
            }

            /* Detect doors */
            if (square_isdoor(context->cave, y, x))
            {
                /* Memorize the door */
                square_memorize(context->player, context->cave, y, x);

                /* Obvious */
                doors = true;
            }

            /* Detect stairs */
            if (square_isstairs(context->cave, y, x))
            {
                /* Memorize the stairs */
                square_memorize(context->player, context->cave, y, x);

                /* Obvious */
                stairs = true;
            }

            /* Forget unknown doors/stairs in the mapping area */
            if ((tf_has(f_info[context->player->cave->squares[y][x].feat].flags, TF_DOOR_ANY) ||
                tf_has(f_info[context->player->cave->squares[y][x].feat].flags, TF_STAIR)) &&
                square_isnotknown(context->player, context->cave, y, x))
            {
                square_forget(context->player, y, x);
            }
        }
    }

    /* Describe */
    if (doors && !stairs)
    {
        msg(context->player, "You sense the presence of doors!");
        party_msg_near(context->player, " senses the presence of doors!");
    }
    else if (!doors && stairs)
    {
        msg(context->player, "You sense the presence of stairs!");
        party_msg_near(context->player, " senses the presence of stairs!");
    }
    else if (doors && stairs)
    {
        msg(context->player, "You sense the presence of doors and stairs!");
        party_msg_near(context->player, " senses the presence of doors and stairs!");
    }
    else if (context->aware)
        msg(context->player, "You sense no doors or stairs.");

    /* Fully update the visuals */
    context->player->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    context->player->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

    if (doors || stairs)
        context->ident = true;
    return true;
}


/*
 * Detect buried gold around the player. The height to detect above and below
 * the player is context->value.dice, the width either side of the player
 * context->value.sides.
 */
static bool effect_handler_DETECT_GOLD(effect_handler_context_t *context)
{
    int x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;

    /* Pick an area to map */
    y1 = context->player->py - y_dist;
    y2 = context->player->py + y_dist;
    x1 = context->player->px - x_dist;
    x2 = context->player->px + x_dist;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Magma/Quartz + Known Gold */
            if (square_hasgoldvein(context->cave, y, x))
            {
                /* Memorize */
                square_memorize(context->player, context->cave, y, x);

                /* Detect */
                context->ident = true;
            }

            /* Forget unknown gold in the mapping area */
            if (tf_has(f_info[context->player->cave->squares[y][x].feat].flags, TF_GOLD) &&
                square_isnotknown(context->player, context->cave, y, x))
            {
                square_forget(context->player, y, x);
            }
        }
    }

    /* Fully update the visuals */
    context->player->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    context->player->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

    return true;
}


/*
 * Detect invisible monsters around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_INVISIBLE_MONSTERS(effect_handler_context_t *context)
{
    if (detect_monsters_invis(context->player, context->value.dice, context->value.sides, true,
        context->aware))
    {
        context->ident = true;
    }
    return true;
}


/*
 * Detect all monsters around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_MONSTERS(effect_handler_context_t *context)
{
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;

    /* Reveal monsters */
    bool detected_creatures = detect_monsters_normal(context->player, y_dist, x_dist, false,
        context->aware);
    bool detected_invis = detect_monsters_invis(context->player, y_dist, x_dist, false,
        context->aware);

    /* Describe result, and clean up */
    if (detected_creatures || detected_invis)
    {
        /* Hack -- fix the monsters and players */
        update_monsters(context->cave, false);
        update_players();

        /* Full refresh (includes monster/object lists) */
        context->player->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(context->player);

        /* Normal refresh (without monster/object lists) */
        context->player->full_refresh = false;

        context->ident = true;

        /* Describe, and wait for acknowledgement */
        msg(context->player, "You sense the presence of creatures!");
        party_msg_near(context->player, " senses the presence of creatures!");

        /* Hack -- pause */
        if (OPT_P(context->player, pause_after_detect)) Send_pause(context->player);
    }

    return true;
}


/*
 * Detect traps around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_TRAPS(effect_handler_context_t *context)
{
    int x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;
    bool detect = false;
    struct object *obj;

    /* Pick an area to map */
    y1 = context->player->py - y_dist;
    y2 = context->player->py + y_dist;
    x1 = context->player->px - x_dist;
    x2 = context->player->px + x_dist;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Detect traps */
            if (square_isplayertrap(context->cave, y, x))
            {
                /* Reveal trap */
                if (square_reveal_trap(context->player, y, x, 100, false))
                {
                    /* We found something to detect */
                    detect = true;
                }
            }

            /* Forget unknown traps in the mapping area */
            if (!square_top_trap(context->cave, y, x))
                square_forget_trap(context->player, y, x);

            /* Scan all objects in the grid to look for traps on chests */
            for (obj = square_object(context->cave, y, x); obj; obj = obj->next)
            {
                /* Skip anything not a trapped chest */
                if (!is_trapped_chest(obj)) continue;

                /* Identify once */
                if (!object_is_known(context->player, obj))
                {
                    /* Hack -- know the pile */
                    floor_pile_know(context->player, context->cave, y, x);

                    /* Know the trap */
                    object_notice_everything_aux(context->player, obj, true, false);

                    /* Notice */
                    if (!ignore_item_ok(context->player, obj))
                    {
                        /* Notice it */
                        disturb(context->player, 0);

                        /* We found something to detect */
                        detect = true;
                    }
                }
            }

            /* Mark as trap-detected */
            sqinfo_on(context->player->cave->squares[y][x].info, SQUARE_DTRAP);
        }
    }

    /* Rescan the map for the new dtrap edge */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* See if this grid is on the edge */
            if (square_dtrap_edge(context->player, context->cave, y, x))
                sqinfo_on(context->player->cave->squares[y][x].info, SQUARE_DEDGE);
            else
                sqinfo_off(context->player->cave->squares[y][x].info, SQUARE_DEDGE);
        }
    }

    /* Describe */
    if (detect)
    {
        msg(context->player, "You sense the presence of traps!");
        party_msg_near(context->player, " senses the presence of traps!");
    }

    /* Trap detection always makes you aware, even if no traps are present */
    else
        msg(context->player, "You sense no traps.");

    /* Fully update the visuals */
    context->player->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    context->player->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST | PR_DTRAP);

    /* Notice */
    context->ident = true;

    return true;
}


/*
 * Detect treasures around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 *
 * PWMAngband: set context->p2 for full detection
 */
static bool effect_handler_DETECT_TREASURES(effect_handler_context_t *context)
{
    int x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;
    bool gold_buried = false;
    bool objects = false;
    bool full = (context->p2? true: false);

    /* Hack -- DM has full detection */
    if (context->player->dm_flags & DM_SEE_LEVEL) full = true;

    /* Pick an area to map */
    y1 = context->player->py - y_dist;
    y2 = context->player->py + y_dist;
    x1 = context->player->px - x_dist;
    x2 = context->player->px + x_dist;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Magma/Quartz + Known Gold */
            if (square_hasgoldvein(context->cave, y, x))
            {
                /* Memorize */
                square_memorize(context->player, context->cave, y, x);

                /* Detect */
                gold_buried = true;
            }

            /* Forget unknown gold in the mapping area */
            if (tf_has(f_info[context->player->cave->squares[y][x].feat].flags, TF_GOLD) &&
                square_isnotknown(context->player, context->cave, y, x))
            {
                square_forget(context->player, y, x);
            }
        }
    }

    /* Scan the area for objects */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            struct object *obj;

            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            obj = square_object(context->cave, y, x);

            /* Skip empty grids */
            if (!obj) continue;

            /* Detect */
            if (!ignore_item_ok(context->player, obj) || !full) objects = true;

            /* Memorize the pile */
            if (full) floor_pile_know(context->player, context->cave, y, x);
            else floor_pile_sense(context->player, context->cave, y, x);
        }
    }

    if (gold_buried)
    {
        msg(context->player, "You sense the presence of buried treasure!");
        party_msg_near(context->player, " senses the presence of buried treasure!");
    }
    if (objects)
    {
        msg(context->player, "You sense the presence of objects!");
        party_msg_near(context->player, " senses the presence of objects!");
    }
    if (context->aware && !gold_buried && !objects)
        msg(context->player, "You sense no treasure or objects.");

    /* Fully update the visuals */
    context->player->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    context->player->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

    if (gold_buried || objects) context->ident = true;
    return true;
}


/*
 * Detect visible monsters around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_VISIBLE_MONSTERS(effect_handler_context_t *context)
{
    if (detect_monsters_normal(context->player, context->value.dice, context->value.sides, true,
        context->aware))
    {
        context->ident = true;
    }
    return true;
}


static bool effect_handler_DETONATE(effect_handler_context_t *context)
{
    int i;
    struct monster *mon;
    int p_flag = PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

    /* Make all controlled jellies and vortices explode */
    for (i = cave_monster_max(context->cave) - 1; i >= 1; i--)
    {
        mon = cave_monster(context->cave, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Skip non slaves */
        if (context->player->id != mon->master) continue;

        /* Jellies explode with a slowing effect */
        if (match_monster_bases(mon->race->base, "jelly", "mold", NULL))
        {
            struct actor who_body;
            struct actor *who = &who_body;

            ACTOR_MONSTER(who, mon);
            project(who, 2, context->cave, mon->fy, mon->fx, 20, GF_OLD_SLOW, p_flag, 0, 0,
                "killed");

            /* Delete the monster */
            delete_monster_idx(context->cave, i);
        }

        /* Vortices explode with a ball effect */
        else if (match_monster_bases(mon->race->base, "vortex", NULL))
        {
            bitflag f[RSF_SIZE];
            int num = 0, j;
            byte spells[RSF_MAX];
            struct actor who_body;
            struct actor *who = &who_body;

            /* Extract the racial spell flags */
            rsf_copy(f, mon->race->spell_flags);

            /* Require breath attacks */
            set_breath(f);

            /* Extract spells */
            for (j = FLAG_START; j < RSF_MAX; j++)
            {
                if (rsf_has(f, j)) spells[num++] = j;
            }

            /* Pick at random */
            ACTOR_MONSTER(who, mon);
            project(who, 2, context->cave, mon->fy, mon->fx, mon->level,
                spell_effect(spells[randint0(num)]), p_flag, 0, 0, "killed");

            /* Delete the monster */
            delete_monster_idx(context->cave, i);
        }
    }
    return true;
}


/*
 * Apply disenchantment to the player's stuff.
 */
static bool effect_handler_DISENCHANT(effect_handler_context_t *context)
{
    int i, count = 0;
    struct object *obj;
    char o_name[NORMAL_WID];

    /* Count slots */
    for (i = 0; i < context->player->body.count; i++)
    {
        /* Ignore rings, amulets and lights (and tools) */
        if (slot_type_is(context->player, i, EQUIP_RING)) continue;
        if (slot_type_is(context->player, i, EQUIP_AMULET)) continue;
        if (slot_type_is(context->player, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(context->player, i, EQUIP_TOOL)) continue;

        /* Count disenchantable slots */
        count++;
    }

    /* Pick one at random */
    for (i = context->player->body.count - 1; i >= 0; i--)
    {
        /* Ignore rings, amulets and lights (and tools) */
        if (slot_type_is(context->player, i, EQUIP_RING)) continue;
        if (slot_type_is(context->player, i, EQUIP_AMULET)) continue;
        if (slot_type_is(context->player, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(context->player, i, EQUIP_TOOL)) continue;

        if (one_in_(count--)) break;
    }

    /* Get the item */
    obj = slot_object(context->player, i);

    /* No item, nothing happens */
    if (!obj) return true;

    /* Nothing to disenchant */
    if ((obj->to_h <= 0) && (obj->to_d <= 0) && (obj->to_a <= 0))
        return true;

    /* Describe the object */
    object_desc(context->player, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Artifacts have 60% chance to resist */
    if (obj->artifact && magik(60))
    {
        /* Message */
        msg(context->player, "Your %s (%c) resist%s disenchantment!", o_name, I2A(i),
            SINGULAR(obj->number));

        /* Notice */
        context->ident = true;

        return true;
    }

    /* Apply disenchantment, depending on which kind of equipment */
    if (slot_type_is(context->player, i, EQUIP_WEAPON) || slot_type_is(context->player, i, EQUIP_BOW))
    {
        /* Disenchant to-hit */
        if (obj->to_h > 0) obj->to_h--;
        if ((obj->to_h > 5) && magik(20)) obj->to_h--;

        /* Disenchant to-dam */
        if (obj->to_d > 0) obj->to_d--;
        if ((obj->to_d > 5) && magik(20)) obj->to_d--;
    }
    else
    {
        /* Disenchant to-ac */
        if (obj->to_a > 0) obj->to_a--;
        if ((obj->to_a > 5) && magik(20)) obj->to_a--;
    }

    /* Message */
    msg(context->player, "Your %s (%c) %s disenchanted!", o_name, I2A(i),
        ((obj->number != 1)? "were": "was"));

    /* Recalculate bonuses */
    context->player->upkeep->update |= (PU_BONUS);

    /* Redraw */
    context->player->upkeep->redraw |= (PR_EQUIP);

    /* Notice */
    context->ident = true;

    return true;
}


/*
 * Drain mana from the player, healing the caster.
 */
static bool effect_handler_DRAIN_MANA(effect_handler_context_t *context)
{
    int drain = effect_calculate_value(context, false);
    struct actor who_body;
    struct actor *who = &who_body;
    bool seen;

    if (!context->mon) return true;

    seen = (!context->player->timed[TMD_BLIND] &&
        mflag_has(context->player->mflag[context->mon->midx], MFLAG_VISIBLE));
    ACTOR_MONSTER(who, context->mon);

    /* MvM */
    if (context->target_m_ptr)
    {
        char m_name[NORMAL_WID];

        /* Affects only casters */
        if (!context->target_m_ptr->race->freq_spell) return true;

        monster_desc(context->player, m_name, sizeof(m_name), context->mon, MDESC_STANDARD);

        /* Attack power, capped vs monster level */
        if (drain > (context->target_m_ptr->level / 6) + 1)
            drain = (context->target_m_ptr->level / 6) + 1;

        /* Heal the monster */
        if (context->mon->hp < context->mon->maxhp)
        {
            /* Heal */
            context->mon->hp += (6 * drain);
            if (context->mon->hp > context->mon->maxhp) context->mon->hp = context->mon->maxhp;

            /* Redraw (later) if needed */
            update_health(who);

            /* Special message */
            if (seen) msg(context->player, "%s appears healthier.", m_name);
        }

        return true;
    }

    if (resist_undead_attacks(context->player, context->mon->race))
    {
        msg(context->player, "You resist the effects!");
        return true;
    }

    drain_mana(context->player, who, drain, seen);

    return true;
}


/*
 * Drain a stat temporarily. The stat index is context->p1.
 */
static bool effect_handler_DRAIN_STAT(effect_handler_context_t *context)
{
    int stat = context->p1;
    int flag = sustain_flag(stat);

    /* Bounds check */
    if (flag < 0) return true;

    /* Notice effect */
    equip_notice_flag(context->player, flag);

    /* Sustain */
    if (player_of_has(context->player, flag))
    {
        /* Message */
        msg(context->player, "You feel very %s for a moment, but the feeling passes.",
            desc_stat_neg[stat]);

        /* Notice */
        context->ident = true;

        return true;
    }

    /* Attempt to reduce the stat */
    if (player_stat_dec(context->player, stat, false))
    {
        /* Message */
        msgt(context->player, MSG_DRAIN_STAT, "You feel very %s.", desc_stat_neg[stat]);

        /* Notice */
        context->ident = true;
    }

    return true;
}


/*
 * Induce an earthquake of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and jump into a safe grid if possible,
 * otherwise, he will tunnel through the rubble instantaneously.
 *
 * Monsters will take damage, and jump into a safe grid if possible,
 * otherwise they will be buried in the rubble, disappearing from
 * the level in the same way that they do when banished.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 *
 * PWMAngband: the radius is set in context->value.base (unless specified directly); if
 * context->mon is set, quake the area silently around the monster
 */
static bool effect_handler_EARTHQUAKE(effect_handler_context_t *context)
{
    int r = effect_calculate_value(context, false);
    int i, y, x, yy, xx, dy, dx, cy, cx, j;
    int damage = 0;
    int safe_grids = 0, safe_y = 0, safe_x = 0;
    int hurt[MAX_PLAYERS];
    bool map[32][32];
    int count = 0;

    if (context->p2) r = context->p2;
    context->ident = true;

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        if (!context->mon) msg(context->player, "The ground shakes for a moment.");
        return true;
    }

    /* Determine the epicentre */
    if (context->mon)
    {
        cy = context->mon->fy;
        cx = context->mon->fx;
    }
    else
    {
        cy = context->player->py;
        cx = context->player->px;
    }

    if (!context->mon) msg_misc(context->player, " causes the ground to shake!");

    /* Paranoia -- enforce maximum range */
    if (r > 12) r = 12;

    /* Clear the "maximal blast" area */
    for (y = 0; y < 32; y++)
    {
        for (x = 0; x < 32; x++) map[y][x] = false;
    }

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Take note of any player */
            if (context->cave->squares[yy][xx].mon < 0)
            {
                hurt[count] = context->cave->squares[yy][xx].mon;
                count++;
            }

            /* Lose room and vault */
            sqinfo_off(context->cave->squares[yy][xx].info, SQUARE_ROOM);
            sqinfo_off(context->cave->squares[yy][xx].info, SQUARE_VAULT);
            sqinfo_off(context->cave->squares[yy][xx].info, SQUARE_NO_TELEPORT);
            if (square_ispitfloor(context->cave, yy, xx)) square_clear_feat(context->cave, yy, xx);

            /* Lose light */
            square_unglow(context->cave, yy, xx);

            /* Skip the epicenter */
            if (!dx && !dy) continue;

            /* Skip most grids */
            if (magik(85)) continue;

            /* Damage this grid */
            map[16 + dy][16 + dx] = true;

            /* Hack -- take note of player damage */
            if (context->cave->squares[yy][xx].mon < 0) hurt[count - 1] = 0 - hurt[count - 1];
        }
    }

    /* First, affect the players (if necessary) */
    for (j = 0; j < count; j++)
    {
        struct player *player;

        /* Skip undamaged players */
        if (hurt[j] < 0) continue;

        player = player_get(hurt[j]);

        safe_grids = 0; safe_y = 0; safe_x = 0; damage = 0;

        /* Check around the player */
        for (i = 0; i < 8; i++)
        {
            /* Get the location */
            y = player->py + ddy_ddd[i];
            x = player->px + ddx_ddd[i];

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Skip non-empty grids */
            if (!square_isemptyfloor(context->cave, y, x)) continue;

            /* Important -- skip "quake" grids */
            if (map[16 + y - cy][16 + x - cx]) continue;

            /* Count "safe" grids, apply the randomizer */
            if ((++safe_grids > 1) && randint0(safe_grids)) continue;

            /* Save the safe location */
            safe_y = y; safe_x = x;
        }

        /* Random message */
        switch (randint1(3))
        {
            case 1:
            {
                msg(player, "The cave ceiling collapses!");
                break;
            }
            case 2:
            {
                msg(player, "The cave floor twists in an unnatural way!");
                break;
            }
            default:
            {
                msg(player, "The cave quakes!");
                msg(player, "You are pummeled with debris!");
                break;
            }
        }

        /* Hurt the player a lot */
        if (!safe_grids)
        {
            /* Message and damage */
            msg(player, "You are severely crushed!");
            damage = 300;
        }

        /* Destroy the grid, and push the player to safety */
        else
        {
            /* Calculate results */
            switch (randint1(3))
            {
                case 1:
                {
                    msg(player, "You nimbly dodge the blast!");
                    damage = 0;
                    break;
                }
                case 2:
                {
                    msg(player, "You are bashed by rubble!");
                    damage = damroll(10, 4);
                    player_inc_timed(player, TMD_STUN, randint1(50), true, true);
                    break;
                }
                case 3:
                {
                    msg(player, "You are crushed between the floor and ceiling!");
                    damage = damroll(10, 4);
                    player_inc_timed(player, TMD_STUN, randint1(50), true, true);
                    break;
                }
            }

            /* Move player */
            monster_swap(context->cave, player->py, player->px, safe_y, safe_x);
        }

        /* Take some damage */
        if (damage)
        {
            my_strcpy(player->died_flavor, "was crushed by tons of falling rocks",
                sizeof(player->died_flavor));
            take_hit(player, damage, "an earthquake", false);
        }
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, yy, xx)) continue;

            /* Skip unaffected grids */
            if (!map[16 + dy][16 + dx]) continue;

            /* Process monsters */
            if (context->cave->squares[yy][xx].mon > 0)
            {
                struct monster *mon = square_monster(context->cave, yy, xx);

                /* Most monsters cannot co-exist with rock */
                if (!rf_has(mon->race->flags, RF_KILL_WALL) &&
                    !rf_has(mon->race->flags, RF_PASS_WALL))
                {
                    char m_name[NORMAL_WID];

                    /* Assume not safe */
                    safe_grids = 0;

                    /* Monster can move to escape the wall */
                    if (!rf_has(mon->race->flags, RF_NEVER_MOVE))
                    {
                        /* Look for safety */
                        for (i = 0; i < 8; i++)
                        {
                            /* Get the grid */
                            y = yy + ddy_ddd[i];
                            x = xx + ddx_ddd[i];

                            /* Skip illegal grids */
                            if (!square_in_bounds_fully(context->cave, y, x)) continue;

                            /* Skip non-empty grids */
                            if (!square_isemptyfloor(context->cave, y, x)) continue;

                            /* No safety on glyph of warding */
                            if (square_iswarded(context->cave, y, x)) continue;

                            /* Important -- skip "quake" grids */
                            if (map[16 + y - cy][16 + x - cx]) continue;

                            /* Count "safe" grids, apply the randomizer */
                            if ((++safe_grids > 1) && randint0(safe_grids)) continue;

                            /* Save the safe grid */
                            safe_y = y;
                            safe_x = x;
                        }
                    }

                    /* Give players a message */
                    for (j = 0; j < count; j++)
                    {
                        /* Get player */
                        struct player *player = player_get(abs(hurt[j]));

                        /* Describe the monster */
                        monster_desc(player, m_name, sizeof(m_name), mon, MDESC_DEFAULT);

                        /* Scream in pain */
                        add_monster_message(player, m_name, mon, MON_MSG_WAIL, true);
                    }

                    /* Take damage from the quake */
                    damage = (safe_grids? damroll(4, 8): (mon->hp + 1));

                    /* Monster is certainly awake */
                    mon_clear_timed(context->player, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE,
                        false);

                    /* If the quake finished the monster off, show message */
                    if ((mon->hp < damage) && (mon->hp >= 0))
                    {
                        /* Give players a message */
                        for (j = 0; j < count; j++)
                        {
                            /* Get player */
                            struct player *player = player_get(abs(hurt[j]));

                            /* Describe the monster */
                            monster_desc(player, m_name, sizeof(m_name), mon, MDESC_DEFAULT);

                            /* Message */
                            add_monster_message(player, m_name, mon, MON_MSG_EMBEDDED, true);
                        }
                    }

                    /* Apply damage directly */
                    mon->hp -= damage;

                    /* Delete (not kill) "dead" monsters */
                    if (mon->hp < 0)
                    {
                        /* Delete the monster */
                        delete_monster(context->cave, yy, xx);
                        if (square_ispitfloor(context->cave, yy, xx))
                            square_clear_feat(context->cave, yy, xx);

                        /* No longer safe */
                        safe_grids = 0;
                    }

                    /* Escape from the rock */
                    if (safe_grids)
                    {
                        /* Move the monster */
                        monster_swap(context->cave, yy, xx, safe_y, safe_x);
                    }
                }
            }
        }
    }

    /* Important -- no wall on players */
    for (j = 0; j < count; j++)
    {
        /* Get player */
        struct player *player = player_get(abs(hurt[j]));

        map[16 + player->py - cy][16 + player->px - cx] = false;
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, yy, xx)) continue;

            /* Note unaffected grids for light changes, etc. */
            if (!map[16 + dy][16 + dx]) square_light_spot(context->cave, yy, xx);

            /* Destroy location and all objects (if valid) */
            else if (square_changeable(context->cave, yy, xx))
            {
                square_excise_pile(context->cave, yy, xx);
                square_earthquake(context->cave, yy, xx);
            }
        }
    }

    for (j = 0; j < count; j++)
    {
        /* Get player */
        struct player *player = player_get(abs(hurt[j]));

        /* Fully update the visuals */
        player->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow */
        player->upkeep->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

        /* Redraw */
        player->upkeep->redraw |= (PR_HEALTH | PR_MONLIST | PR_ITEMLIST);
    }

    return true;
}


static bool effect_handler_ELEM_BRAND(effect_handler_context_t *context)
{
    int tries = effect_calculate_value(context, false);
    struct object *obj = equipped_item_by_slot_name(context->player, "weapon");
    int i;
    const char *act;
    int brand;
    bool chosen[5];

    memset(chosen, 0, sizeof(chosen));

    /* You can never modify artifacts/ego-items */
    /* You can never modify worthless/cursed items */
    if (obj && !cursed_p(obj->flags) && obj->kind->cost && !obj->artifact && !obj->ego)
    {
        char o_name[NORMAL_WID];

        object_desc(context->player, o_name, sizeof(o_name), obj, ODESC_BASE);

        /* Brand the object */
        obj->ego = &e_info[EGO_ELEMENTAL];
        ego_apply_magic(obj, 0);
        object_notice_ego(context->player, obj);

        /* Enchant */
        enchant(context->player, obj, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Add some brands */
        for (i = 0; i < tries; i++)
        {
            int what = randint0(5);

            /* Select a brand */
            switch (what)
            {
                case 0: act = "flames"; brand = ELEM_FIRE; break;
                case 1: act = "frost"; brand = ELEM_COLD; break;
                case 2: act = "lightning"; brand = ELEM_ELEC; break;
                case 3: act = "acid"; brand = ELEM_ACID; break;
                case 4: act = "venom"; brand = ELEM_POIS; break;
            }

            /* Check brand */
            if (chosen[what]) continue;
            chosen[what] = true;

            /* Describe */
            msg(context->player, "The %s %s surrounded with an aura of %s.", o_name,
                ((obj->number > 1)? "are": "is"), act);

            /* Brand the object */
            append_fixed_brand(&obj->brands, brand, 3);

            if (object_was_sensed(obj) || object_is_known(context->player, obj))
                object_notice_brands(context->player, obj, NULL);
        }

        /* Endless source of cash? No way... make them worthless */
        set_origin(obj, ORIGIN_WORTHLESS, context->player->depth, 0);
        if (object_was_sensed(obj) || object_is_known(context->player, obj))
            context->player->upkeep->notice |= PN_IGNORE;
    }
    else
        msg(context->player, "The branding failed.");

    return true;
}


/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(const struct object *obj)
{
    return tval_is_weapon(obj);
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(const struct object *obj)
{
    return tval_is_armor(obj);
}


/*
 * Enchant an item (in the inventory or on the floor)
 * Note that armour, to hit or to dam is controlled by context->p2
 *
 * PWMAngband: set context->p3 to prevent items from becoming "worthless" (anti-cheeze)
 */
static bool effect_handler_ENCHANT(effect_handler_context_t *context)
{
    int value = effect_calculate_value(context, false);
    struct object *obj;
    bool (*item_tester)(const struct object *obj);
    char o_name[NORMAL_WID];

    context->ident = true;

    /* Get an item */
    if (context->player->current_value == ITEM_REQUEST)
    {
        get_item(context->player, ((context->p2 == ENCH_TOAC)? HOOK_ARMOR: HOOK_WEAPON));
        return false;
    }

    /* Use current */
    obj = object_from_index(context->player, context->player->current_value, true, true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->player, obj) && !is_owner(context->player, obj))
    {
        msg(context->player, "This item belongs to someone else!");
        return false;
    }

    /* Assume enchant weapon */
    item_tester = item_tester_hook_weapon;

    /* Enchant armor if requested */
    if (context->p2 == ENCH_TOAC) item_tester = item_tester_hook_armour;

    /* Paranoia: requires proper item */
    if (!item_tester(obj)) return false;

    /* Description */
    object_desc(context->player, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Describe */
    msg(context->player, "%s %s glow%s brightly!",
        (object_is_carried(context->player, obj) ? "Your" : "The"), o_name,
        SINGULAR(obj->number));

    /* Enchant */
    if (!enchant(context->player, obj, value, context->p2))
    {
        /* Failure */
        msg(context->player, "The enchantment failed.");
    }

    /* Endless source of cash? No way... make them worthless */
    else if (!context->p3)
    {
        set_origin(obj, ORIGIN_WORTHLESS, context->player->depth, 0);
        if (object_was_sensed(obj) || object_is_known(context->player, obj))
            context->player->upkeep->notice |= PN_IGNORE;
    }

    /* Redraw */
    if (!object_is_carried(context->player, obj))
        redraw_floor(obj->depth, obj->iy, obj->ix);

    /* Something happened */
    return true;
}


/*
 * Dummy effect, to tell the effect code to stop appending info (for spells).
 */
static bool effect_handler_END_INFO(effect_handler_context_t *context)
{
    return true;
}


static bool effect_handler_ENLIGHTENMENT(effect_handler_context_t *context)
{
    bool full = (context->p2? true: false);

    if (full)
        msg(context->player, "An image of your surroundings forms in your mind...");
    wiz_light(context->player, context->cave, full);
    context->ident = true;
    return true;
}


static bool effect_handler_GAIN_EXP(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->player->exp < PY_MAX_EXP)
    {
        s32b ee = (context->player->exp / 2) + 10;

        if (ee > amount) ee = amount;
        msg(context->player, "You feel more experienced.");
        player_exp_gain(context->player, context->p1? ee: amount);
        context->ident = true;
    }
    return true;
}


/*
 * Gain a stat point. The stat index is context->p1.
 */
static bool effect_handler_GAIN_STAT(effect_handler_context_t *context)
{
    int stat = context->p1;

    /* Attempt to increase */
    if (player_stat_inc(context->player, stat))
    {
        /* Message */
        msg(context->player, "You feel very %s!", desc_stat_pos[stat]);

        /* Notice */
        context->ident = true;
    }

    return true;
}


/*
 * Heal the player by a given percentage of their wounds, or a minimum
 * amount, whichever is larger.
 *
 * context->value.base should be the minimum, and
 * context->value.m_bonus the percentage
 */
static bool effect_handler_HEAL_HP(effect_handler_context_t *context)
{
    int num, amount;

    /* Paranoia */
    if ((context->value.m_bonus <= 0) && (context->value.base <= 0)) return true;

    /* Slight hack to ID !Life */
    if (context->value.base >= 5000) context->ident = true;

    /* No healing needed */
    if (context->player->chp >= context->player->mhp) return true;

    /* Figure healing level */
    num = ((context->player->mhp - context->player->chp) * context->value.m_bonus) / 100;

    /* PWMAngband: Cell Adjustment heals a variable amount of hps */
    amount = context->value.base + damroll(context->value.dice, context->value.sides);

    /* Enforce minimums */
    if (num < amount) num = amount;

    if (context->self_msg) msg(context->player, context->self_msg);
    if (hp_player(context->player, num))
        context->ident = true;
    return true;
}


/*
 * Identify an unknown item
 */
static bool effect_handler_IDENTIFY(effect_handler_context_t *context)
{
    struct object *obj;

    context->ident = true;

    /* Get an item */
    if (context->player->current_value == ITEM_REQUEST)
    {
        get_item(context->player, HOOK_IDENTIFY);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->player, context->player->current_value, true, true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->player, obj) && !is_owner(context->player, obj))
    {
        msg(context->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires identifiable item */
    if (object_is_known(context->player, obj)) return false;

    /* Identify the object */
    do_ident_item(context->player, obj);
    if (!object_is_carried(context->player, obj))
        redraw_floor(obj->depth, obj->iy, obj->ix);

    /* Something happened */
    return true;
}


/*
 * Identify everything worn or carried by the player
 */
static bool effect_handler_IDENTIFY_PACK(effect_handler_context_t *context)
{
    struct object *obj;

    if (context->self_msg) msg(context->player, context->self_msg);
    context->ident = true;

    /* Simply identify and know every item */
    for (obj = context->player->gear; obj; obj = obj->next)
    {
        /* Aware and Known */
        if (object_is_known(context->player, obj)) continue;

        /* Identify it */
        do_ident_item(context->player, obj);
    }

    return true;
}


/*
 * Call light around the player
 * Affect all monsters in the projection radius (context->p2)
 */
static bool effect_handler_LIGHT_AREA(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int dam = effect_calculate_value(context, false);
    int rad = context->p2 + (context->p3? (context->player->lev / context->p3): 0);
    int flg = PROJECT_GRID;
    struct actor who_body;
    struct actor *who = &who_body;

    ACTOR_PLAYER(who, get_player_index(get_connection(context->player->conn)), context->player);

    /* Hack -- elementalists */
    if (context->spell_power)
    {
        rad = dam;
        dam = 0;
    }

    /* Hurt monsters/players in the projection radius */
    if (dam > 0) flg |= (PROJECT_KILL | PROJECT_PLAY);

    /* Message */
    if (!context->player->timed[TMD_BLIND])
        msg(context->player, "You are surrounded by a white light.");

    /* Hook into the "project()" function */
    context->player->current_sound = -2;
    project(who, rad, context->cave, py, px, dam, GF_LIGHT_WEAK, flg, 0, 0, "killed");
    context->player->current_sound = -1;

    /* Light up the room */
    light_room(context->player, context->cave, py, px, true);

    /* Assume seen */
    context->ident = true;
    return true;
}


/*
 * Cast a line spell
 * Pass through monsters, as a beam
 * Affect monsters and grids (not objects)
 *
 * PWMAngband: setting context->value.m_bonus is a hack for elementalists to
 * get multiple lines
 */
static bool effect_handler_LINE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int y, num = (context->value.m_bonus? context->value.m_bonus: 1);

    if (context->self_msg && !context->player->timed[TMD_BLIND])
        msg(context->player, context->self_msg);
    for (y = 0; y < num; y++)
    {
        if (light_line_aux(context->player, context->mon, context->dir, context->p1, dam))
            context->ident = true;
    }
    return true;
}


static bool effect_handler_LOSE_EXP(effect_handler_context_t *context)
{
    if (!player_of_has(context->player, OF_HOLD_LIFE) && context->player->exp)
    {
        msg(context->player, "You feel your memories fade.");
        player_exp_lose(context->player, context->player->exp / 4, false);
    }
    context->ident = true;
    equip_notice_flag(context->player, OF_HOLD_LIFE);
    return true;
}


/*
 * Lose a stat point permanently, in a stat other than the one specified
 * in context->p1.
 */
static bool effect_handler_LOSE_RANDOM_STAT(effect_handler_context_t *context)
{
    int safe_stat = context->p1;
    int loss_stat = safe_stat;

    /* Pick a random stat to decrease other than "stat" */
    while (loss_stat == safe_stat) loss_stat = randint0(STAT_MAX);

    /* Attempt to reduce the stat */
    if (player_stat_dec(context->player, loss_stat, true))
    {
        /* Notice */
        context->ident = true;

        /* Message */
        msgt(context->player, MSG_DRAIN_STAT, "You feel very %s.", desc_stat_neg[loss_stat]);
    }

    return true;
}


/*
 * Map an area around the player. The height to map above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_MAP_AREA(effect_handler_context_t *context)
{
    int i, x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;

    /* Pick an area to map */
    y1 = context->player->py - y_dist;
    y2 = context->player->py + y_dist;
    x1 = context->player->px - x_dist;
    x2 = context->player->px + x_dist;

    /* Drag the co-ordinates into the dungeon */
    if (y1 < 0) y1 = 0;
    if (y2 > context->cave->height - 1) y2 = context->cave->height - 1;
    if (x1 < 0) x1 = 0;
    if (x2 > context->cave->width - 1) x2 = context->cave->width - 1;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Some squares can't be mapped */
            if (square_isno_map(context->cave, y, x)) continue;

            /* All non-walls are "checked" */
            if (!square_seemslikewall(context->cave, y, x))
            {
                if (!square_in_bounds_fully(context->cave, y, x)) continue;

                /* Memorize normal features, mark grids as processed */
                if (square_isnormal(context->cave, y, x))
                {
                    square_memorize(context->player, context->cave, y, x);
                    square_mark(context->player, y, x);
                }

                /* Memorize known walls */
                for (i = 0; i < 8; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Memorize walls (etc), mark grids as processed */
                    if (square_seemslikewall(context->cave, yy, xx))
                    {
                        square_memorize(context->player, context->cave, yy, xx);
                        square_mark(context->player, yy, xx);
                    }
                }
            }

            /* Forget unprocessed, unknown grids in the mapping area */
            if (!square_ismark(context->player, y, x) &&
                square_isnotknown(context->player, context->cave, y, x))
            {
                square_forget(context->player, y, x);
            }
        }
    }

    /* Unmark grids */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            if (!square_in_bounds(context->cave, y, x)) continue;
            square_unmark(context->player, y, x);
        }
    }

    /* Fully update the visuals */
    context->player->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw whole map, monster list */
    context->player->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

    /* Notice */
    context->ident = true;

    return true;
}


/*
 * Delete all nearby (non-unique) monsters. The radius of effect is
 * context->p2 if passed, otherwise the player view radius.
 */
static bool effect_handler_MASS_BANISH(effect_handler_context_t *context)
{
    int i;
    int radius = (context->p2? context->p2: z_info->max_sight);
    unsigned dam = 0;
    bool result = false;
    const char *pself = player_self(context->player);

    /* Delete the (nearby) monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);
        int d;

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip unique monsters */
        if (rf_has(mon->race->flags, RF_UNIQUE)) continue;

        /* Skip distant monsters */
        d = distance(context->player->py, context->player->px, mon->fy, mon->fx);
        if (d > radius) continue;

        /* Delete the monster */
        delete_monster_idx(context->cave, i);

        /* Take some damage */
        dam += randint1(3);
    }

    /* Hurt the player */
    strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
        "exhausted %s with Mass Banishment", pself);
    take_hit(context->player, dam, "the strain of casting Mass Banishment", false);

    /* Calculate result */
    result = (dam > 0)? true: false;

    /* Redraw */
    if (result)
    {
        context->player->upkeep->redraw |= (PR_MONLIST);
        context->ident = true;
    }

    return true;
}


static bool effect_handler_MIND_VISION(effect_handler_context_t *context)
{
    struct player *q = get_inscribed_player(context->player, context->note);

    if (!q) return true;

    if (context->player == q)
    {
        msg(context->player, "You cannot link to your own mind.");
        return false;
    }
    if (context->player->esp_link)
    {
        msg(context->player, "Your mind is already linked.");
        return false;
    }
    if (q->esp_link)
    {
        msg(context->player, "%s's mind is already linked.", q->name);
        return false;
    }

    /* Not if hostile */
    if (pvp_check(context->player, q, PVP_CHECK_ONE, true, 0x00))
    {
        /* Message */
        msg(context->player, "%s's mind is not receptive.", q->name);
        return false;
    }

    msg(q, "%s infiltrates your mind.", context->player->name);
    msg(context->player, "You infiltrate %s's mind.", q->name);
    context->player->esp_link = q->id;
    context->player->esp_link_type = LINK_DOMINANT;

    q->esp_link = context->player->id;
    q->esp_link_type = LINK_DOMINATED;
    q->upkeep->redraw |= PR_MAP;

    return true;
}


/*
 * Monster self-healing.
 */
static bool effect_handler_MON_HEAL_HP(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);
    char m_name[NORMAL_WID], m_poss[NORMAL_WID];
    bool seen;
    struct actor who_body;
    struct actor *who = &who_body;

    if (!context->mon) return true;

    /* No stupid message when at full health */
    if (context->mon->hp == context->mon->maxhp) return true;

    /* Get the monster name (or "it") */
    monster_desc(context->player, m_name, sizeof(m_name), context->mon, MDESC_STANDARD);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(context->player, m_poss, sizeof(m_poss), context->mon, MDESC_PRO_VIS | MDESC_POSS);

    seen = (!context->player->timed[TMD_BLIND] &&
        mflag_has(context->player->mflag[context->mon->midx], MFLAG_VISIBLE));

    /* Heal some */
    context->mon->hp += amount;

    /* Fully healed */
    if (context->mon->hp >= context->mon->maxhp)
    {
        context->mon->hp = context->mon->maxhp;

        if (seen)
            msg(context->player, "%s looks REALLY healthy!", m_name);
        else
            msg(context->player, "%s sounds REALLY healthy!", m_name);
    }

    /* Partially healed */
    else if (seen)
        msg(context->player, "%s looks healthier.", m_name);
    else
        msg(context->player, "%s sounds healthier.", m_name);

    /* Redraw (later) if needed */
    ACTOR_MONSTER(who, context->mon);
    update_health(who);

    /* Cancel fear */
    if (context->mon->m_timed[MON_TMD_FEAR])
    {
        mon_clear_timed(context->player, context->mon, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE, false);
        msg(context->player, "%s recovers %s courage.", m_name, m_poss);
    }

    /* Cancel poison */
    if (context->mon->m_timed[MON_TMD_POIS])
    {
        mon_clear_timed(context->player, context->mon, MON_TMD_POIS, MON_TMD_FLG_NOMESSAGE, false);
        msg(context->player, "%s is no longer poisoned.", m_name);
    }  

    /* Cancel bleeding */
    if (context->mon->m_timed[MON_TMD_CUT])
    {
        mon_clear_timed(context->player, context->mon, MON_TMD_CUT, MON_TMD_FLG_NOMESSAGE, false);
        msg(context->player, "%s is no longer bleeding.", m_name);
    }

    return true;
}


/*
 * Extend a (positive or negative) monster status condition.
 */
static bool effect_handler_MON_TIMED_INC(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (!context->mon) return true;

    mon_inc_timed(context->player, context->mon, context->p1, amount, 0, false);
    context->ident = true;
    return true;
}


/*
 * Feed the player.
 */
static bool effect_handler_NOURISH(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->self_msg && !player_undead(context->player))
        msg(context->player, context->self_msg);
    player_set_food(context->player, context->player->food + amount);
    return true;
}


static bool effect_handler_POLY_RACE(effect_handler_context_t *context)
{
    struct monster_race *race = &r_info[context->boost];

    /* Restrict */
    if (context->player->ghost || player_has(context->player, PF_DRAGON) ||
        OPT_P(context->player, birth_fruit_bat) || (context->player->poly_race == race))
    {
        msg(context->player, "Nothing happens.");
        return false;
    }

    /* Restrict if too powerful */
    if (context->player->lev < race->level / 2)
    {
        msg(context->player, "Nothing happens.");
        return false;
    }

    /* Useless ring */
    if (race->ridx == 0)
    {
        msg(context->player, "Nothing happens.");
        return false;
    }

    /* Non-Shapechangers get a huge penalty for using rings of polymorphing */
    if (!player_has(context->player, PF_MONSTER_SPELLS))
    {
        const char *pself = player_self(context->player);

        msg(context->player, "Your nerves and muscles feel weak and lifeless!");
        strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
            "exhausted %s with polymorphing", pself);
        take_hit(context->player, damroll(10, 10), "the strain of polymorphing", false);
        player_stat_dec(context->player, STAT_DEX, true);
        player_stat_dec(context->player, STAT_WIS, true);
        player_stat_dec(context->player, STAT_CON, true);
        player_stat_dec(context->player, STAT_STR, true);
        player_stat_dec(context->player, STAT_INT, true);

        /* Fail if too powerful */
        if (magik(race->level))
        {
            context->ident = true;
            return true;
        }
    }

    do_cmd_poly(context->player, race, false, true);

    context->ident = true;
    return true;
}


/*
 * Probe nearby monsters
 */
static bool effect_handler_PROBE(effect_handler_context_t *context)
{
    int i;
    bool probe = false;
    bool blows;

    /* Probe all (nearby) monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);
        int d;

        blows = false;

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Skip monsters too far */
        d = distance(context->player->py, context->player->px, mon->fy, mon->fx);
        if (d > z_info->max_sight)
            continue;

        /* Probe visible monsters */
        if (mflag_has(context->player->mflag[i], MFLAG_VISIBLE))
        {
            char m_name[NORMAL_WID];
            char buf[NORMAL_WID];
            int j;

            /* Start the message */
            if (!probe) msg(context->player, "Probing...");

            /* Get "the monster" or "something" */
            monster_desc(context->player, m_name, sizeof(m_name), mon,
                MDESC_IND_HID | MDESC_CAPITAL);

            strnfmt(buf, sizeof(buf), "blows");
            for (j = 0; j < z_info->mon_blows_max; j++)
            {
                if (mon->blow[j].dice.dice)
                {
                    if (!blows) blows = true;
                    my_strcat(buf,
                        format(" %dd%d", mon->blow[j].dice.dice, mon->blow[j].dice.sides),
                        sizeof(buf));
                }
            }

            /* Describe the monster */
            msg(context->player, "%s (%d) has %d hp, %d ac, %d speed.", m_name, mon->level,
                mon->hp, mon->ac, mon->mspeed);
            if (blows)
                msg(context->player, "%s (%d) %s.", m_name, mon->level, buf);

            /* Learn all of the non-spell, non-treasure flags */
            lore_do_probe(context->player, mon);

            /* Probe worked */
            probe = true;
        }
    }

    /* Done */
    if (probe)
    {
        msg(context->player, "That's all.");
        context->ident = true;
    }

    return true;
}


/*
 * Dummy effect, to tell the effect code to apply a "project()" on a monster (for MvM mode).
 */
static bool effect_handler_PROJECT(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
    struct actor who_body;
    struct actor *who = &who_body;

    if (!context->mon) return true;

    /* MvM only */
    if (!context->target_m_ptr) return true;

    ACTOR_MONSTER(who, context->mon);
    project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, dam,
        context->p1, flag, 0, 0, "annihilated");

    return true;
}


/*
 * Apply a "project()" directly to all viewable monsters. If context->p2 is
 * set, the effect damage boost is applied.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool effect_handler_PROJECT_LOS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, context->p2? true: false);
    int typ = context->p1;

    if (project_los(context->player, typ, dam, false))
        context->ident = true;
    return true;
}


/*
 * Apply a "project()" directly to all viewable monsters. If context->p2 is
 * set, the effect damage boost is applied.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool effect_handler_PROJECT_LOS_AWARE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, context->p2? true: false);
    int typ = context->p1;

    if (project_los(context->player, typ, dam, context->aware))
    {
        context->ident = true;
        if (context->self_msg) msg(context->player, context->self_msg);
    }
    return true;
}


/*
 * Dummy effect, to tell the effect code to pick one of the next
 * context->value.base effects at random.
 */
static bool effect_handler_RANDOM(effect_handler_context_t *context)
{
    return true;
}


/*
 * Selects the recall depth.
 * Setting negative levels is now legal, assuming that the player has explored
 * the respective wilderness level.
 */
static void set_recall_depth(struct player *p, quark_t note)
{
    unsigned char* inscription = (unsigned char*)quark_str(note);

    /* Default to the players maximum depth */
    int target_depth = p->max_depth;

    /* Check for a valid inscription */
    if (inscription)
    {
        /* Scan the inscription for @R */
        while (*inscription != '\0')
        {
            if (*inscription == '@')
            {
                inscription++;

                if (*inscription == 'R')
                {
                    /* A valid @R has been located */
                    inscription++;

                    /* Convert the inscription into a level index */
                    target_depth = atoi(inscription);

                    /* Help avoid typos */
                    if (target_depth % 50)
                        target_depth = 0;
                    else
                        target_depth /= 50;

                    break;
                }
            }
            inscription++;
        }

        /* Do some bounds checking/sanity checks */
        if ((target_depth < 0 - MAX_WILD) || (target_depth > p->max_depth)) target_depth = 0;

        /* If a wilderness level, verify that the player has visited here before */
        if (target_depth < 0)
        {
            /* If the player has not visited here, set the recall depth to the town */
            if (!wild_is_explored(p, 0 - target_depth)) target_depth = 0;
        }
    }

    /* Force descent to a lower level if allowed */
    if (((cfg_limit_stairs == 3) || OPT_P(p, birth_force_descend)) && (target_depth > 0) &&
        (p->max_depth < z_info->max_depth - 1))
    {
        target_depth = dungeon_get_next_level(p, p->max_depth, 1);
    }

    p->recall_depth = target_depth;
}


/*
 * Set word of recall as appropriate.
 *
 * PWMAngband: context->value gives the delay.
 */
static bool effect_handler_RECALL(effect_handler_context_t *context)
{
    int delay = effect_calculate_value(context, false);

    context->ident = true;

    /* No recall */
    if ((cfg_no_recall || OPT_P(context->player, birth_no_recall)) && !context->player->total_winner)
    {
        msg(context->player, "Nothing happens.");
        return false;
    }

    /* No recall from quest levels with force_descend while the quest is active */
    if (((cfg_limit_stairs == 3) || OPT_P(context->player, birth_force_descend)) &&
        is_quest_active(context->player, context->player->depth))
    {
        msg(context->player, "Nothing happens.");
        return false;
    }

    /* Activate recall */
    if (!context->player->word_recall)
    {
        /* Select the recall depth */
        set_recall_depth(context->player, context->note);

        /* Warn the player if they're descending to an unrecallable level */
        if (((cfg_limit_stairs == 3) || OPT_P(context->player, birth_force_descend)) &&
            !context->player->depth &&
            is_quest_active(context->player, context->player->recall_depth) &&
            (context->player->current_value == ITEM_REQUEST))
        {
            get_item(context->player, HOOK_DOWN);
            return false;
        }

        /* Activate recall */
        context->player->word_recall = delay;
        msg(context->player, "The air around you becomes charged...");
        msg_misc(context->player, " is surrounded by a charged aura...");

        /* Redraw the state (later) */
        context->player->upkeep->redraw |= (PR_STATE);
    }

    /* Deactivate recall */
    else
    {
        /* Ask for confirmation */
        if (context->player->current_value == ITEM_REQUEST)
        {
            get_item(context->player, HOOK_CONFIRM);
            return false;
        }

        /* Deactivate recall */
        context->player->word_recall = 0;
        msg(context->player, "A tension leaves the air around you...");
        msg_misc(context->player, "'s charged aura disappears...");

        /* Redraw the state (later) */
        context->player->upkeep->redraw |= (PR_STATE);
    }

    return true;
}


/*
 * Hook to specify rechargeable items
 */
static bool item_tester_hook_recharge(const struct object *obj)
{
    /* Recharge staves and wands */
    if (tval_can_have_charges(obj)) return true;

    return false;
}


/*
 * Recharge a wand or staff from the pack or on the floor. Recharge strength
 * is context->value.base.
 *
 * It is harder to recharge high level, and highly charged wands.
 */
static bool effect_handler_RECHARGE(effect_handler_context_t *context)
{
    int i, t, lev;
    int strength = context->value.base;
    struct object *obj;
    bool carried;
    int depth, y, x;

    /* Immediately obvious */
    context->ident = true;

    /* Get an item */
    if (context->player->current_value == ITEM_REQUEST)
    {
        get_item(context->player, HOOK_RECHARGE);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->player, context->player->current_value, true, true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Save object info (backfire may destroy it) */
    carried = object_is_carried(context->player, obj);
    depth = obj->depth;
    y = obj->iy;
    x = obj->ix;

    /* Restricted by choice */
    if (!carried && !is_owner(context->player, obj))
    {
        msg(context->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires rechargeable item */
    if (!item_tester_hook_recharge(obj)) return false;

    /* Extract the object "level" */
    lev = obj->kind->level;

    /*
     * Chance of failure = 1 time in
     * [Spell_strength + 100 - item_level - 10 * charge_per_item] / 15
     */
    i = (strength + 100 - lev - (10 * (obj->pval / obj->number))) / 15;

    /* Back-fire */
    if ((i <= 1) || one_in_(i))
    {
        msg(context->player, "The recharge backfires!");
        msg(context->player, "There is a bright flash of light.");

        /* Safe recharge: drain all charges */
        if (cfg_safe_recharge)
            obj->pval = 0;

        /* Normal recharge: destroy one item */
        else
        {
            /* Reduce and describe inventory */
            use_object(context->player, obj, 1, true);
        }
    }

    /* Recharge */
    else
    {
        /* Extract a "power" */
        t = (strength / (lev + 2)) + 1;

        /* Recharge based on the power */
        if (t > 0) obj->pval += 2 + randint1(t);
    }

    /* Combine the pack (later) */
    context->player->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    context->player->upkeep->redraw |= (PR_INVEN);
    if (!carried) redraw_floor(depth, y, x);

    /* Something was done */
    return true;
}


#if 0
/*
 * Removes curses from one item in inventory.
 *
 * "heavy" removes heavy curses if true
 *
 * Returns true if uncursed, false otherwise
 */
static int remove_curse_aux(struct player *p, struct object *obj, bool heavy)
{
    bitflag f[OF_SIZE];

    /* Skip non-objects and uncursed items */
    if (!obj) return false;
    if (!cursed_p(obj->flags)) return false;

    /* Heavily cursed items need a special spell */
    if (of_has(obj->flags, OF_HEAVY_CURSE) && !heavy) return false;

    /* Perma-cursed items can never be uncursed */
    if (of_has(obj->flags, OF_PERMA_CURSE)) return false;

    /* Uncurse, and update things */
    create_mask(f, false, OFT_CURSE, OFT_MAX);
    of_diff(obj->flags, f);
    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    return true;
}


/*
 * Removes light curses from worn items.
 */
static bool effect_handler_REMOVE_CURSE(effect_handler_context_t *context)
{
    int i, cnt = 0;

    /* Attempt to uncurse items being worn */
    for (i = 0; i < context->player->body.count; i++)
    {
        struct object *obj = slot_object(context->player, i);

        /* Count the uncursings */
        if (remove_curse_aux(context->player, obj, false)) cnt++;
    }

    /* Return "something uncursed" */
    if (cnt)
    {
        if (!context->player->timed[TMD_BLIND])
            msg(context->player, "The air around your body glows blue for a moment...");
        else
            msg(context->player, "You feel as if someone is watching over you.");
        context->ident = true;
    }
    return true;
}


/*
 * Remove all curses
 */
static bool effect_handler_REMOVE_ALL_CURSE(effect_handler_context_t *context)
{
    int cnt = 0;
    struct object *obj;

    /* Attempt to uncurse all items */
    for (obj = context->player->gear; obj; obj = obj->next)
    {
        /* Count the uncursings */
        if (remove_curse_aux(context->player, obj, true)) cnt++;
    }

    /* Return "something uncursed" */
    if (cnt)
    {
        if (!context->player->timed[TMD_BLIND])
            msg(context->player, "The air around your body glows blue for a moment...");
        else
            msg(context->player, "You feel as if someone is watching over you.");
        context->ident = true;
    }
    return true;
}
#endif


static bool effect_handler_RESILIENCE(effect_handler_context_t *context)
{
    int i;
    struct monster *mon;
    char m_name[NORMAL_WID];
    bool seen;

    /* Expand the lifespan of slaves */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        mon = cave_monster(context->cave, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Skip non slaves */
        if (context->player->id != mon->master) continue;

        /* Acquire the monster name */
        monster_desc(context->player, m_name, sizeof(m_name), mon, MDESC_STANDARD);

        seen = (!context->player->timed[TMD_BLIND] &&
            mflag_has(context->player->mflag[mon->midx], MFLAG_VISIBLE));

        /* Skip already resilient slaves */
        if (mon->resilient)
        {
            if (seen) msg(context->player, "%s is unaffected.", m_name);
        }

        /* Double the lifespan (cap the value depending on monster level) */
        else
        {
            mon->lifespan = mon->level * 2 + 20;
            mon->resilient = 1;
            if (seen) msg(context->player, "%s looks more resilient.", m_name);
        }
    }

    return true;
}


/*
 * Restores any drained experience
 */
static bool effect_handler_RESTORE_EXP(effect_handler_context_t *context)
{
    if (context->self_msg && !player_undead(context->player))
        msg(context->player, context->self_msg);

    /* Restore experience */
    if (context->player->exp < context->player->max_exp)
    {
        /* Message */
        msg(context->player, "You feel your life energies returning.");

        /* Restore the experience */
        player_exp_gain(context->player, context->player->max_exp - context->player->exp);

        /* Did something */
        context->ident = true;
    }

    return true;
}


static bool effect_handler_RESTORE_MANA(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (!amount) amount = context->player->msp;

    /* Healing needed */
    if (context->player->csp < context->player->msp)
    {
        int old_num = get_player_num(context->player);

        /* Gain mana */
        context->player->csp += amount;

        /* Enforce maximum */
        if (context->player->csp >= context->player->msp)
        {
            context->player->csp = context->player->msp;
            context->player->csp_frac = 0;
        }

        /* Hack -- redraw picture */
        redraw_picture(context->player, old_num);

        /* Redraw */
        context->player->upkeep->redraw |= (PR_MANA);

        /* Print a nice message */
        msg(context->player, "You feel your head clear.");

        /* Notice */
        context->ident = true;
    }

    return true;
}


/*
 * Restore a stat. The stat index is context->p1.
 */
static bool effect_handler_RESTORE_STAT(effect_handler_context_t *context)
{
    int stat = context->p1;

    /* Check bounds */
    if ((stat < 0) || (stat >= STAT_MAX)) return true;

    /* Not needed */
    if (context->player->stat_cur[stat] == context->player->stat_max[stat]) return true;

    /* Restore */
    context->player->stat_cur[stat] = context->player->stat_max[stat];

    /* Recalculate bonuses */
    context->player->upkeep->update |= (PU_BONUS);

    /* Message */
    msg(context->player, "You feel less %s.", desc_stat_neg[stat]);

    /* Success */
    context->ident = true;
    return true;
}


/* Try to resurrect someone */
static bool effect_handler_RESURRECT(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int x, y;

    for (y = -1; y <= 1; y++)
    {
        for (x = -1; x <= 1; x++)
        {
            int m_idx = context->cave->squares[py + y][px + x].mon;

            if ((x == 0) && (y == 0)) continue;

            if ((m_idx < 0) && square_ispassable(context->cave, py + y, px + x))
            {
                struct player *q = player_get(0 - m_idx);

                if (q->ghost && !player_can_undead(q))
                {
                    resurrect_player(q, context->cave);
                    context->ident = true;
                    return true;
                }
            }
        }
    }

    /* We did not resurrect anyone */
    return true;
}


/*
 * Create a "glyph of warding".
 */
static bool effect_handler_RUNE(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;

    /* Hack -- already used up */
    bool used = (context->p2 == 1);

    /* Always notice */
    context->ident = true;

    /* Only on random levels */
    if (!random_level(context->cave->depth))
    {
        msg(context->player, "You cannot create glyphs here...");
        return false;
    }

    /* Require clean space */
    if (!square_canward(context->cave, py, px))
    {
        msg(context->player, "There is no clear floor on which to cast the spell.");
        return false;
    }

    /* Push objects off the grid */
    if (square_object(context->cave, py, px)) push_object(context->player, context->cave, py, px);

    /* Create a glyph of warding */
    square_add_ward(context->cave, py, px);
    msg_misc(context->player, " lays down a glyph of protection.");

    return !used;
}


static bool effect_handler_SAFE_GUARD(effect_handler_context_t *context)
{
    int x, y, rad = 2 + (context->player->lev / 20);

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        msg(context->player, "You cannot create glyphs here...");
        return false;
    }

    msg_misc(context->player, " lays down some glyphs of protection.");

    for (x = context->player->px - rad; x <= context->player->px + rad; x++)
    {
        for (y = context->player->py - rad; y <= context->player->py + rad; y++)
        {
            /* First we must be in the dungeon */
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Is it a naked grid? */
            if (!square_isempty(context->cave, y, x)) continue;

            /* Now we want a circle */
            if (distance(y, x, context->player->py, context->player->px) != rad) continue;

            /* Everything ok... then put a glyph */
            square_add_ward(context->cave, y, x);
        }
    }

    return true;
}


/*
 * Set player food counter
 */
static bool effect_handler_SET_NOURISH(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->self_msg) msg(context->player, context->self_msg);
    if (player_set_food(context->player, amount)) context->ident = true;
    return true;
}


/*
 * Cast a line spell in every direction
 * Stop if we hit a monster, act as a ball
 * Affect grids, objects, and monsters
 *
 * PWMAngband: if context->p2 is set, divide the damage by that amount
 */
static bool effect_handler_STAR(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int i;

    if (context->p2) dam /= context->p2;

    if (context->self_msg && !context->player->timed[TMD_BLIND])
        msg(context->player, context->self_msg);
    for (i = 0; i < 8; i++)
        light_line_aux(context->player, context->mon, ddd[i], context->p1, dam);
    if (!context->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Cast a ball spell in every direction
 * Stop if we hit a monster, act as a ball
 * Affect grids, objects, and monsters
 */
static bool effect_handler_STAR_BALL(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int i;

    if (context->self_msg && !context->player->timed[TMD_BLIND])
        msg(context->player, context->self_msg);
    for (i = 0; i < 8; i++)
        fire_ball(context->player, context->p1, ddd[i], dam, context->p2, false);
    if (!context->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Summon context->value monsters of context->p1 type.
 *
 * PWMAngband: set context->p2 to get delayed summons; context->p3 is the chance
 * to get friendly summons (set to -1 to bypass friendly summons)
 */
static bool effect_handler_SUMMON(effect_handler_context_t *context)
{
    int summon_max = effect_calculate_value(context, false);
    int summon_type = (context->p1? context->p1: S_ANY);
    int message_type = summon_message_type(summon_type);
    int count = 0;
    int y, x, depth;

    /* Summoners may get friendly summons */
    int chance = context->p3;

    if (chance == -1) chance = 0;
    else if (player_has(context->player, PF_SUMMON_SPELLS)) chance = 100;

    /* Hack -- no summons in Arena */
    if (context->mon)
    {
        y = context->mon->fy;
        x = context->mon->fx;
        depth = context->mon->depth;
    }
    else
    {
        y = context->player->py;
        x = context->player->px;
        depth = context->player->depth;
    }
    if (pick_arena(depth, y, x) != -1) return true;

    sound(context->player, message_type);

    /* Monster summon */
    if (context->mon)
    {
        int rlev = ((context->mon->race->level >= 1)? context->mon->race->level: 1);

        /* Set the kin_base if necessary */
        if (summon_type == S_KIN) kin_base = context->mon->race->base;

        /* Summon them */
        count = summon_monster_aux(context->player, context->cave, context->mon->fy,
            context->mon->fx, summon_type, rlev, summon_max, chance);
    }

    /* Delayed summon */
    else if (context->p2)
    {
        int i;

        if (check_antisummon(context->player, NULL)) return true;

        /* Summon them */
        for (i = 0; i < summon_max; i++)
        {
            count += summon_specific(context->player, context->cave, context->player->py,
                context->player->px, context->player->depth, summon_type, true, false, chance);
        }
    }

    /* Player summon */
    else
    {
        if (check_antisummon(context->player, NULL)) return true;

        /* Set the kin_base if necessary */
        if (summon_type == S_KIN) kin_base = context->player->poly_race->base;

        /* Summon them */
        count = summon_monster_aux(context->player, context->cave, context->player->py,
            context->player->px, summon_type, context->player->depth, summon_max, chance);
    }

    /* Identify if some monsters arrive */
    if (count) context->ident = true;

    /* Message for the blind */
    if (count && context->player->timed[TMD_BLIND])
    {
        msgt(context->player, message_type, "You hear %s appear nearby.",
            ((count > 1)? "many things": "something"));
    }

    /* Summoner failed */
    if (context->mon && !count) msg(context->player, "But nothing comes.");

    return true;
}


/*
 * Cast multiple non-jumping ball spells at the same target.
 *
 * Targets absolute coordinates instead of a specific monster, so that
 * the death of the monster doesn't change the target's location.
 */
static bool effect_handler_SWARM(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int dam = effect_calculate_value(context, true);
    int num = context->value.m_bonus;
    int ty, tx;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    struct actor who_body;
    struct actor *who = &who_body;

    ACTOR_PLAYER(who, get_player_index(get_connection(context->player->conn)), context->player);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    ty = py + ddy[context->dir];
    tx = px + ddx[context->dir];

    /* Hack -- use an actual "target" */
    if ((context->dir == 5) && target_okay(context->player))
    {
        flg &= ~(PROJECT_STOP | PROJECT_THRU);
        target_get(context->player, &tx, &ty);
    }

    context->player->current_sound = -2;
    while (num--)
    {
        /* Aim at the target. Hurt items on floor. */
        if (project(who, context->p2, context->cave, ty, tx, dam, context->p1, flg, 0, 0,
            "annihilated"))
        {
            context->ident = true;
        }
    }
    context->player->current_sound = -1;

    return true;
}


static bool effect_handler_TELE_OBJECT(effect_handler_context_t *context)
{
    struct object *obj, *teled;
    struct player *q;

    /* Get an item */
    if (context->player->current_value == ITEM_REQUEST)
    {
        get_item(context->player, HOOK_SEND);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->player, context->player->current_value, true, true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->player, obj) && !is_owner(context->player, obj))
    {
        msg(context->player, "This item belongs to someone else!");
        return false;
    }

    /* Forbid artifacts */
    if (obj->artifact)
    {
        msg(context->player, "The object is too powerful to be sent...");
        return false;
    }

    q = get_inscribed_player(context->player, context->note);
    if (!q) return true;

    /* Note that the pack is too full */
    if (!inven_carry_okay(q, obj))
    {
        msg(context->player, "%s has no room for another object.", q->name);
        return false;
    }

    /* Note that the pack is too heavy */
    if (!weight_okay(q, obj))
    {
        msg(context->player, "%s is already too burdened to carry another object.", q->name);
        return false;
    }

    /* Restricted by choice */
    if (OPT_P(q, birth_no_stores))
    {
        msg(context->player, "%s cannot be reached.", q->name);
        return false;
    }

    /* Actually teleport the object to the player inventory */
    teled = object_new();
    object_copy(teled, obj);
    inven_carry(q, teled, true, false);

    /* Combine the pack */
    q->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    q->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    /* Wipe it */
    use_object(context->player, obj, obj->number, false);

    /* Combine the pack */
    context->player->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    context->player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    msg(q, "You are hit by a powerful magic wave from %s.", context->player->name);
    return true;
}


/*
 * Teleport player or monster up to context->value.base grids away.
 *
 * If no spaces are readily available, the distance may increase.
 * Try very hard to move the player/monster at least a quarter that distance.
 * Setting context->p2 allows monsters to teleport the player away
 */
static bool effect_handler_TELEPORT(effect_handler_context_t *context)
{
    int y_start, x_start;
    int dis = context->value.base;
    int d, i, min, y, x;
    bool look = true;
    bool is_player = (!context->mon || context->p2);
    bool safe_ghost = false;
    int depth;

    /* Hack -- already used up */
    bool used = (context->p3 == 1);

    context->ident = true;

    /* MvM */
    if (context->target_m_ptr && context->p2)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_MONSTER(who, context->mon);
        project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, dis,
            context->p1, flag, 0, 0, "annihilated");

        return !used;
    }

    /* Establish the coordinates to teleport from */
    if (is_player)
    {
        y_start = context->player->py;
        x_start = context->player->px;
        safe_ghost = context->player->ghost;
        depth = context->player->depth;
    }
    else
    {
        y_start = context->mon->fy;
        x_start = context->mon->fx;
        depth = context->mon->depth;
    }

    /* Space-time anchor */
    if (check_st_anchor(depth, y_start, x_start) && !safe_ghost)
    {
        if (context->player) msg(context->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, y_start, x_start) && !safe_ghost)
    {
        if (context->player) msg(context->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Hack -- hijack teleport in Arena */
    if (context->player && (context->player->arena_num != -1))
    {
        int arena_num = context->player->arena_num;

        effect_simple(context->player, EF_TELEPORT_TO, "0",
            arenas[arena_num].y_1 + 1 + randint1(arenas[arena_num].y_2 - arenas[arena_num].y_1 - 2),
            arenas[arena_num].x_1 + 1 + randint1(arenas[arena_num].x_2 - arenas[arena_num].x_1 - 2),
            0, NULL, NULL);
        return !used;
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
                y = rand_spread(y_start, dis);
                x = rand_spread(x_start, dis);
                d = distance(y_start, x_start, y, x);

                /* Hack -- not too close from previous player location */
                if (is_player &&
                    distance(context->player->old_py, context->player->old_px, y, x) < min)
                {
                    continue;
                }

                if ((d >= min) && (d <= dis)) break;
            }

            /* Ignore illegal locations */
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Just require empty space if teleporting a ghost to safety */
            if (safe_ghost)
            {
                if (context->cave->squares[y][x].mon) continue;
            }

            /* Require "naked" floor space */
            else
            {
                if (!square_isempty(context->cave, y, x)) continue;
            }

            /* No teleporting into vaults and such */
            if (square_isvault(context->cave, y, x)) continue;

            /* No monster teleport onto glyph of warding */
            if (!is_player && square_iswarded(context->cave, y, x)) continue;

            /* This grid looks good */
            look = false;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;
    }

    /* Sound */
    if (context->player) sound(context->player, (is_player? MSG_TELEPORT: MSG_TPOTHER));

    /* Move the target */
    monster_swap(context->cave, y_start, x_start, y, x);

    /* Clear any projection marker to prevent double processing */
    sqinfo_off(context->cave->squares[y][x].info, SQUARE_PROJECT);

    /* Handle stuff */
    if (context->player) handle_stuff(context->player);

    /* Hack -- fix store */
    if (is_player && in_store(context->player)) Send_store_leave(context->player);

    return !used;
}


/*
 * Teleport the player one level up or down (random when legal)
 *
 * Note that keeping the player count correct is VERY important,
 * otherwise levels with players still on them might be destroyed, or empty
 * levels could be kept around, wasting memory.
 *
 * In the wilderness, teleport to a neighboring wilderness level.
 */
static bool effect_handler_TELEPORT_LEVEL(effect_handler_context_t *context)
{
    int target_depth;
    int new_world_x = context->player->world_x, new_world_y = context->player->world_y;
    byte new_level_method;
    char *message;

    /* Hack -- already used up */
    bool used = (context->p2 == 1);

    context->ident = true;

    /* MvM */
    if (context->target_m_ptr)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_MONSTER(who, context->mon);
        project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, 0,
            context->p1, flag, 0, 0, "annihilated");

        return !used;
    }

    /* Resist hostile teleport */
    if (context->mon && player_resists(context->player, ELEM_NEXUS))
    {
        msg(context->player, "You resist the effect!");
        return !used;
    }

    /* Space-time anchor */
    if (check_st_anchor(context->player->depth, context->player->py, context->player->px))
    {
        msg(context->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Arena fighters don't teleport level */
    if (context->player->arena_num != -1)
    {
        msg(context->player, "The teleporting attempt fails.");
        return !used;
    }

    /* If in the wilderness, teleport to a random neighboring level */
    if (context->player->depth < 0)
    {
        /* Get a valid neighbor */
        do
        {
            switch (randint0(4))
            {
                case DIR_NORTH:
                    message = "A gust of wind blows you north.";
                    new_world_x = context->player->world_x;
                    new_world_y = context->player->world_y + 1;
                    break;
                case DIR_EAST:
                    message = "A gust of wind blows you east.";
                    new_world_x = context->player->world_x + 1;
                    new_world_y = context->player->world_y;
                    break;
                case DIR_SOUTH:
                    message = "A gust of wind blows you south.";
                    new_world_x = context->player->world_x;
                    new_world_y = context->player->world_y - 1;
                    break;
                case DIR_WEST:
                    message = "A gust of wind blows you west.";
                    new_world_x = context->player->world_x - 1;
                    new_world_y = context->player->world_y;
                    break;
            }
            target_depth = world_index(new_world_x, new_world_y);
        }
        while ((target_depth < 0 - MAX_WILD) || chunk_inhibit_players(target_depth));
        new_level_method = LEVEL_OUTSIDE_RAND;
    }

    /* Go up or down a level */
    else
    {
        bool up = true, down = true;
        int base_depth = context->player->depth;

        /* No going up with force_descend or in the town */
        if ((cfg_limit_stairs >= 2) || OPT_P(context->player, birth_force_descend) || !base_depth)
            up = false;

        /* No forcing player down to quest levels if they can't leave */
        if ((cfg_limit_stairs == 3) || OPT_P(context->player, birth_force_descend))
        {
            target_depth = dungeon_get_next_level(context->player, context->player->max_depth, 1);
            if (is_quest_active(context->player, target_depth))
            {
                msg(context->player, "The teleporting attempt fails.");
                return !used;
            }

            /* Descend one level deeper */
            base_depth = context->player->max_depth;
        }

        /* Can't leave quest levels or go down deeper than the dungeon */
        if (is_quest_active(context->player, context->player->depth) ||
            (base_depth == z_info->max_depth - 1))
        {
            down = false;
        }

        /* Hack -- DM redesigning the level */
        target_depth = dungeon_get_next_level(context->player, context->player->depth, -1);
        if (chunk_inhibit_players(target_depth))
            up = false;
        target_depth = dungeon_get_next_level(context->player, base_depth, 1);
        if (chunk_inhibit_players(target_depth))
            down = false;

        /* Determine up/down if not already done */
        if (up && down)
        {
            if (magik(50)) up = false;
            else down = false;
        }

        /* Now actually do the level change */
        if (up)
        {
            message = "You rise up through the ceiling.";
            target_depth = dungeon_get_next_level(context->player, context->player->depth, -1);
        }
        else if (down)
        {
            message = "You sink through the floor.";
            target_depth = dungeon_get_next_level(context->player, base_depth, 1);
        }
        else
        {
            msg(context->player, "The teleporting attempt fails.");
            return !used;
        }

        new_level_method = LEVEL_RAND;
    }

    /* Tell the player */
    msgt(context->player, MSG_TPLEVEL, message);

    /* Change location */
    dungeon_change_level(context->player, context->cave, target_depth, new_level_method);

    /* Hack -- replace the player */
    context->player->world_x = new_world_x;
    context->player->world_y = new_world_y;

    /* Update the wilderness map */
    if (context->player->depth < 0) wild_set_explored(context->player, 0 - context->player->depth);

    return !used;
}


/*
 * Teleport player/monster to a grid near the given location
 * Setting context->p1 and context->p2 treats them as y and x coordinates
 *
 * This function is slightly obsessive about correctness.
 */
static bool effect_handler_TELEPORT_TO(effect_handler_context_t *context)
{
    int py = context->player->py;
    int px = context->player->px;
    int ny = py, nx = px;
    int y, x, dis = 0, ctr = 0;
    int tries = 200;
    bool is_player = true;

    context->ident = true;

    /* MvM */
    if (context->target_m_ptr)
    {
        py = context->target_m_ptr->fy;
        px = context->target_m_ptr->fx;
        is_player = false;
    }

    /* Where are we going? */
    if (context->p1 && context->p2)
    {
        /* Teleport to player */
        ny = context->p1;
        nx = context->p2;
        if (context->mon)
        {
            py = context->mon->fy;
            px = context->mon->fx;
            is_player = false;
        }
    }
    else if (context->mon)
    {
        /* Teleport to monster */
        ny = context->mon->fy;
        nx = context->mon->fx;
    }
    else
    {
        /* Teleport to target */
        if ((context->dir == 5) && target_okay(context->player))
        {
            int rad = effect_calculate_value(context, false);

            target_get(context->player, &nx, &ny);

            if (distance(ny, nx, py, px) > rad)
            {
                msg(context->player, "You cannot blink that far.");
                return true;
            }
        }
        else
        {
            msg(context->player, "You must have a target.");
            return true;
        }
    }

    /* Space-time anchor */
    if (check_st_anchor(context->player->depth, ny, nx))
    {
        msg(context->player, "The teleporting attempt fails.");
        return true;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, ny, nx))
    {
        msg(context->player, "The teleporting attempt fails.");
        return true;
    }

    /* Find a usable location */
    while (--tries)
    {
        bool legal = true;

        /* Pick a nearby legal location */
        while (1)
        {
            y = rand_spread(ny, dis);
            x = rand_spread(nx, dis);
            if (square_in_bounds_fully(context->cave, y, x)) break;
        }

        /* No teleporting into vaults and such if the target is outside the vault */
        if (square_isvault(context->cave, y, x) && !square_isvault(context->cave, py, px))
            legal = false;

        /* Only accept grids in LOS of the caster */
        if (!los(context->cave, ny, nx, y, x)) legal = false;

        /* Accept legal "naked" floor grids... */
        if (square_isempty(context->cave, y, x) && legal) break;

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
        msg(context->player, "The teleporting attempt fails.");
        return true;
    }

    /* Move the player */
    monster_swap(context->cave, py, px, y, x);

    /* Clear any projection marker to prevent double processing */
    sqinfo_off(context->cave->squares[y][x].info, SQUARE_PROJECT);

    /* Handle stuff */
    if (is_player) handle_stuff(context->player);

    /* Hack -- fix store */
    if (is_player && in_store(context->player)) Send_store_leave(context->player);

    return true;
}


/*
 * Thrust the player or a monster away from the source of a projection.
 *
 * PWMAngband: context->p1 and context->p2 contain the coordinates of the caster
 *
 * PWMAngband: completely rewritten using project_path() and stopping at the first monster,
 *             player or wall
 */
static bool effect_handler_THRUST_AWAY(effect_handler_context_t *context)
{
    int y, x, i, c_y, c_x, t_y, t_x;
    int grids_away = effect_calculate_value(context, false);
    int path_n;
    struct loc path_g[256];
    bool skip = true, moved = false;

    /* Caster */
    c_y = context->p1;
    c_x = context->p2;

    /* Target */
    if (context->mon)
    {
        t_y = context->mon->fy;
        t_x = context->mon->fx;
    }
    else
    {
        t_y = context->player->py;
        t_x = context->player->px;
    }

    /* Nothing to do if the target is already in a wall */
    if (!square_isprojectable(context->cave, t_y, t_x))
    {
        sqinfo_off(context->cave->squares[t_y][t_x].info, SQUARE_PROJECT);
        return true;
    }

    /* Start at the target grid. */
    y = t_y;
    x = t_x;

    /* Calculate the path */
    path_n = project_path(path_g, z_info->max_range, context->cave, c_y, c_x, t_y, t_x, PROJECT_THRU);

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        int ny = path_g[i].y;
        int nx = path_g[i].x;

        /* Skip target, start processing afterwards */
        if ((ny == t_y) && (nx == t_x))
        {
            skip = false;
            continue;
        }

        /* Skip grids up to target */
        if (skip) continue;

        /* Stop before hitting unpassable terrain */
        if (!square_ispassable(context->cave, ny, nx)) break;
        if (!square_isprojectable(context->cave, ny, nx)) break;

        /* Stop before hitting a monster or player */
        if (context->cave->squares[ny][nx].mon) break;

        /* Jump to new location. */
        y = ny;
        x = nx;
        moved = true;

        /* We can't travel any more. */
        grids_away--;
        if (!grids_away) break;
    }

    /* Move target */
    if (moved) monster_swap(context->cave, t_y, t_x, y, x);

    /* Clear the projection mark. */
    sqinfo_off(context->cave->squares[y][x].info, SQUARE_PROJECT);

    return true;
}


/*
 * Reduce a (positive or negative) player status condition.
 * If context->p2 is set, decrease by the current value / context->p2
 */
static bool effect_handler_TIMED_DEC(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->p2) amount = context->player->timed[context->p1] / context->p2;
    if (player_dec_timed(context->player, context->p1, amount, true)) context->ident = true;
	return true;
}


/*
 * Extend a (positive or negative) player status condition.
 * If context->p2 is set, increase by that amount if the status exists already
 */
static bool effect_handler_TIMED_INC(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    /* MvM -- irrelevant */
    if (context->target_m_ptr) return true;

    /* Increase by that amount if the status exists already */
    if (context->p2 && context->player->timed[context->p1]) amount = context->p2;

    if (player_inc_timed_aux(context->player, context->mon, context->p1, amount, true, true))
        context->ident = true;

	return true;
}


/*
 * Extend a (positive or negative) player status condition unresistably.
 * If context->p2 is set, increase by that amount if the status exists already
 */
static bool effect_handler_TIMED_INC_NO_RES(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    /* Increase by that amount if the status exists already */
    if (context->p2 && context->player->timed[context->p1]) amount = context->p2;

    if (player_inc_timed_aux(context->player, context->mon, context->p1, amount, true, false))
        context->ident = true;

	return true;
}


/*
 * Set a (positive or negative) player status condition.
 */
static bool effect_handler_TIMED_SET(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    /* Hack -- Day of the Misrule */
    if (context->self_msg)
    {
        const char *pm;

        switch (context->player->psex)
        {
            case SEX_FEMALE: pm = "Daughter"; break;
            case SEX_MALE: pm = "Son"; break;
            default: pm = "Creature"; break;
        }

        msg(context->player, context->self_msg, pm);
    }

    /* Hack -- Touch of Death */
    if (context->p1 == TMD_DEADLY)
    {
        if (context->player->state.stat_use[STAT_STR] < 18+120)
        {
            msg(context->player, "You're not strong enough to use the Touch of Death.");
            return false;
        }
        if (context->player->state.stat_use[STAT_DEX] < 18+120)
        {
            msg(context->player, "You're not dextrous enough to use the Touch of Death.");
            return false;
        }
    }

    if (player_set_timed(context->player, context->p1, amount, true))
        context->ident = true;
    return true;
}


/*
 * Affect adjacent grids (radius 1 ball attack)
 *
 * PWMAngband: set context->p2 to 1 to prevent the effect on static levels
 */
static bool effect_handler_TOUCH(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    /* Only on random levels */
    if ((context->p2 == 1) && !random_level(context->player->depth))
    {
        msg(context->player, "Nothing happens.");
        return true;
    }

    /* MvM */
    if (context->target_m_ptr)
    {
        int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
        struct actor who_body;
        struct actor *who = &who_body;

        ACTOR_MONSTER(who, context->mon);
        project(who, 1, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, 0,
            context->p1, flg, 0, 0, "killed");
        return true;
    }

    if (project_touch(context->player, dam, context->p1, false))
    {
        context->ident = true;
        if (context->self_msg) msg(context->player, context->self_msg);
    }
    return true;
}


/*
 * Affect adjacent grids (radius 1 ball attack)
 * Notice stuff based on awareness of the effect
 */
static bool effect_handler_TOUCH_AWARE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    if (project_touch(context->player, dam, context->p1, context->aware))
        context->ident = true;
    return true;
}


static bool effect_handler_UNDEAD_FORM(effect_handler_context_t *context)
{
    /* Not when already undead */
    if (context->player->ghost)
    {
        msg(context->player, "You are already undead.");
        return false;
    }

    /* Requirement not met */
    if (context->player->state.stat_use[STAT_INT] < 18+70)
    {
        msg(context->player, "You're not smart enough to turn into an undead being.");
        return false;
    }

    /* Turn him into an undead being */
    player_turn_undead(context->player);

    return true;
}


/* Hack -- recalculate max. hitpoints between CON and HP restoration */
static bool effect_handler_UPDATE_STUFF(effect_handler_context_t *context)
{
    context->player->upkeep->update |= (PU_BONUS);
    update_stuff(context->player, context->cave);
    return true;
}


/* Wipe everything */
static bool effect_handler_WIPE_AREA(effect_handler_context_t *context)
{
    int r = context->p2;
    int yy, xx, dy, dx, cy, cx, k;
    int hurt[MAX_PLAYERS];
    int count = 0;

    /* Only on random levels */
    if (!random_level(context->player->depth))
    {
        msg(context->player, "The ground shakes for a moment.");
        return true;
    }

    /* Determine the epicentre */
    cy = context->player->py;
    cx = context->player->px;

    /* Paranoia -- enforce maximum range */
    if (r > 12) r = 12;

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Take note of any player */
            if (context->cave->squares[yy][xx].mon < 0)
            {
                hurt[count] = 0 - context->cave->squares[yy][xx].mon;
                count++;
            }

            /* Lose room and vault */
            sqinfo_off(context->cave->squares[yy][xx].info, SQUARE_VAULT);
            sqinfo_off(context->cave->squares[yy][xx].info, SQUARE_ROOM);
            sqinfo_off(context->cave->squares[yy][xx].info, SQUARE_NO_TELEPORT);
            if (square_ispitfloor(context->cave, yy, xx)) square_clear_feat(context->cave, yy, xx);

            /* Lose light */
            square_unglow(context->cave, yy, xx);
            square_light_spot(context->cave, yy, xx);

            /* Delete monsters */
            delete_monster(context->cave, yy, xx);
            if (square_ispitfloor(context->cave, yy, xx)) square_clear_feat(context->cave, yy, xx);

            /* Destroy "valid" grids */
            if (square_changeable(context->cave, yy, xx))
            {
                /* Delete objects */
                square_excise_pile(context->cave, yy, xx);

                /* Turn into basic floor */
                square_clear_feat(context->cave, yy, xx);
            }
        }
    }

    /* Hack -- affect players */
    for (k = 0; k < count; k++)
    {
        struct player *p = player_get(hurt[k]);

        /* Message */
        msg(p, "There is a searing blast of light!");

        /* Fully update the visuals */
        p->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow */
        p->upkeep->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

        /* Redraw */
        p->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);
    }

    return true;
}


/*
 * The "wonder" effect.
 */
static bool effect_handler_WONDER(effect_handler_context_t *context)
{
    /*
     * This spell should become more useful (more controlled) as the player
     * gains experience levels. Thus, add 1/5 of the player's level to the die roll.
     * This eliminates the worst effects later on, while keeping the results quite
     * random. It also allows some potent effects only at high level.
     */

    int plev = context->player->lev;
    int die = effect_calculate_value(context, false);
    effect_handler_f handler = NULL;
    effect_handler_context_t new_context;

    memset(&new_context, 0, sizeof(new_context));
    new_context.player = context->player;
    new_context.cave = context->cave;
    new_context.aware = context->aware;
    new_context.dir = context->dir;
    new_context.beam = context->beam;
    new_context.boost = context->boost;
    new_context.ident = context->ident;

    if (die > 100) msg(context->player, "You feel a surge of power!");

    if (die < 8)
    {
        msg_misc(context->player, " mumbles.");
        new_context.p1 = GF_OLD_CLONE;
        handler = effect_handler_BOLT;
    }
    else if (die < 14)
    {
        msg_misc(context->player, " mumbles.");
        new_context.value.base = 100;
        new_context.p1 = GF_OLD_SPEED;
        handler = effect_handler_BOLT;
    }
    else if (die < 26)
    {
        msg_misc(context->player, " mumbles.");
        new_context.value.dice = 4;
        new_context.value.sides = 6;
        new_context.p1 = GF_OLD_HEAL;
        handler = effect_handler_BOLT;
    }
    else if (die < 31)
    {
        msg_misc(context->player, " discharges an everchanging blast of energy.");
        new_context.aware = false;
        new_context.value.base = plev;
        new_context.p1 = GF_OLD_POLY;
        new_context.p2 = 1;
        handler = effect_handler_BOLT_AWARE;
    }
    else if (die < 36)
    {
        msg_misc(context->player, " fires a magic missile.");
        new_context.value.dice = 3 + (plev - 1) / 5;
        new_context.value.sides = 4;
        new_context.p1 = GF_MISSILE;
        new_context.p2 = -10;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 41)
    {
        msg_misc(context->player, " makes a complicated gesture.");
        new_context.aware = false;
        new_context.value.base = plev;
        new_context.p1 = GF_OLD_CONF;
        handler = effect_handler_BOLT_AWARE;
    }
    else if (die < 46)
    {
        msg_misc(context->player, " fires a stinking cloud.");
        new_context.value.base = 20 + plev / 2;
        new_context.p1 = GF_POIS;
        new_context.p2 = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 51)
    {
        msg_misc(context->player, "'s hands project a line of shimmering blue light.");
        new_context.value.dice = 6;
        new_context.value.sides = 8;
        new_context.p1 = GF_LIGHT_WEAK;
        new_context.self_msg = "A line of shimmering blue light appears.";
        handler = effect_handler_LINE;
    }
    else if (die < 56)
    {
        msg_misc(context->player, " fires a lightning bolt.");
        new_context.value.dice = 3 + (plev - 5) / 6;
        new_context.value.sides = 6;
        new_context.p1 = GF_ELEC;
        handler = effect_handler_BEAM;
    }
    else if (die < 61)
    {
        msg_misc(context->player, " fires a frost bolt.");
        new_context.value.dice = 5 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.p1 = GF_COLD;
        new_context.p2 = -10;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 66)
    {
        msg_misc(context->player, " fires an acid bolt.");
        new_context.value.dice = 6 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.p1 = GF_ACID;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 71)
    {
        msg_misc(context->player, " fires a fire bolt.");
        new_context.value.dice = 8 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.p1 = GF_FIRE;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 76)
    {
        msg_misc(context->player, " fires a bolt filled with pure energy!");
        new_context.value.base = 75;
        new_context.p1 = GF_OLD_DRAIN;
        handler = effect_handler_BOLT;
    }
    else if (die < 81)
    {
        msg_misc(context->player, " fires a lightning ball.");
        new_context.value.base = 30 + plev / 2;
        new_context.p1 = GF_ELEC;
        new_context.p2 = 2;
        handler = effect_handler_BALL;
    }
    else if (die < 86)
    {
        msg_misc(context->player, " fires an acid ball.");
        new_context.value.base = 40 + plev;
        new_context.p1 = GF_ACID;
        new_context.p2 = 2;
        handler = effect_handler_BALL;
    }
    else if (die < 91)
    {
        msg_misc(context->player, " fires an ice ball.");
        new_context.value.base = 70 + plev;
        new_context.p1 = GF_ICE;
        new_context.p2 = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 96)
    {
        msg_misc(context->player, " fires a fire ball.");
        new_context.value.base = 80 + plev;
        new_context.p1 = GF_FIRE;
        new_context.p2 = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 101)
    {
        msg_misc(context->player, " fires a massive bolt filled with pure energy!");
        new_context.value.base = 100 + plev;
        new_context.p1 = GF_OLD_DRAIN;
        handler = effect_handler_BOLT;
    }
    else if (die < 104)
    {
        msg_misc(context->player, " mumbles.");
        new_context.value.base = 12;
        handler = effect_handler_EARTHQUAKE;
    }
    else if (die < 106)
    {
        msg_misc(context->player, " mumbles.");
        new_context.value.base = 15;
        handler = effect_handler_DESTRUCTION;
    }
    else if (die < 108)
    {
        msg_misc(context->player, " mumbles.");
        handler = effect_handler_BANISH;
    }
    else if (die < 110)
    {
        msg_misc(context->player, " mumbles.");
        new_context.value.base = 120;
        new_context.p1 = GF_DISP_ALL;
        new_context.p2 = 1;
        handler = effect_handler_PROJECT_LOS;
    }

    if (handler != NULL)
    {
        bool handled = handler(&new_context);

        context->ident = new_context.ident;
        return handled;
    }

    /* RARE */
    msg_misc(context->player, " mumbles.");
    effect_simple(context->player, EF_PROJECT_LOS, "150", GF_DISP_ALL, 1, 0, &context->ident, NULL);
    effect_simple(context->player, EF_PROJECT_LOS, "20", GF_OLD_SLOW, 0, 0, &context->ident, NULL);
    effect_simple(context->player, EF_PROJECT_LOS, format("%d", plev), GF_OLD_SLEEP, 0, 0,
        &context->ident, NULL);
    effect_simple(context->player, EF_HEAL_HP, "300", 0, 0, 0, &context->ident, NULL);

    return true;
}


static bool effect_handler_TRAP_DOOR(effect_handler_context_t *context)
{
    const char *poss = player_poss(context->player);
    int target_depth = dungeon_get_next_level(context->player, context->player->depth, 1);

    /* Verify basic quests */
    if (is_quest_active(context->player, context->player->depth))
    {
        msg(context->player, "You feel quite certain something really awful just happened...");
        return true;
    }

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(target_depth))
    {
        msg(context->player, "You feel quite certain something really awful just happened...");
        return true;
    }

    msg(context->player, "You fall through a trap door!");
    if (player_of_has(context->player, OF_FEATHER))
        msg(context->player, "You float gently down to the next level.");
    else
    {
        int dam = effect_calculate_value(context, false);

        strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
            "broke %s neck after falling from 50ft high", poss);
        take_hit(context->player, dam, "a trap", false);
    }
    equip_notice_flag(context->player, OF_FEATHER);

    dungeon_change_level(context->player, context->cave, target_depth, LEVEL_RAND);
    return true;
}


static bool effect_handler_TRAP_PIT(effect_handler_context_t *context)
{
    const char *poss = player_poss(context->player);

    msg(context->player, "You fall into a pit!");
    if (player_of_has(context->player, OF_FEATHER))
        msg(context->player, "You float gently to the bottom of the pit.");
    else
    {
        int dam = effect_calculate_value(context, false);

        strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
            "broke %s neck after falling into a pit", poss);
        take_hit(context->player, dam, "a trap", false);
    }
    equip_notice_flag(context->player, OF_FEATHER);

    return true;
}


static bool effect_handler_TRAP_PIT_SPIKES(effect_handler_context_t *context)
{
    const char *pself = player_self(context->player);

    msg(context->player, "You fall into a spiked pit!");

    if (player_of_has(context->player, OF_FEATHER))
    {
        msg(context->player, "You float gently to the floor of the pit.");
        msg(context->player, "You carefully avoid touching the spikes.");
    }
    else
    {
        int dam = effect_calculate_value(context, false);

        /* Extra spike damage */
        if (one_in_(2))
        {
            msg(context->player, "You are impaled!");
            dam *= 2;
            player_inc_timed(context->player, TMD_CUT, randint1(dam), true, true);
        }

        strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
            "impaled %s on sharp spikes", pself);
        take_hit(context->player, dam, "a trap", false);
    }
    equip_notice_flag(context->player, OF_FEATHER);

    return true;
}


static bool effect_handler_TRAP_PIT_POISON(effect_handler_context_t *context)
{
    const char *pself = player_self(context->player);

    msg(context->player, "You fall into a spiked pit!");

    if (player_of_has(context->player, OF_FEATHER))
    {
        msg(context->player, "You float gently to the floor of the pit.");
        msg(context->player, "You carefully avoid touching the spikes.");
    }
    else
    {
        int dam = effect_calculate_value(context, false);

        /* Extra spike damage */
        if (one_in_(2))
        {
            msg(context->player, "You are impaled on poisonous spikes!");
            player_inc_timed(context->player, TMD_CUT, randint1(dam * 2), true, true);
            player_inc_timed(context->player, TMD_POISONED, randint1(dam * 4), true, true);
        }

        strnfmt(context->player->died_flavor, sizeof(context->player->died_flavor),
            "impaled %s on poisonous spikes", pself);
        take_hit(context->player, dam, "a trap", false);
    }
    equip_notice_flag(context->player, OF_FEATHER);

    return true;
}


static bool effect_handler_TRAP_RUNE_SUMMON(effect_handler_context_t *context)
{
    int i;
    int num = effect_calculate_value(context, false);

    msgt(context->player, MSG_SUM_MONSTER, "You are enveloped in a cloud of smoke!");
    if (check_antisummon(context->player, NULL)) num = 0;

    /* Remove trap */
    square_destroy_trap(context->player, context->cave, context->player->py, context->player->px);

    for (i = 0; i < num; i++)
    {
        summon_specific(context->player, context->cave, context->player->py, context->player->px,
            context->player->depth, S_ANY, true, false, 0);
    }

    return true;
}


static bool effect_handler_TRAP_RUNE_TELEPORT(effect_handler_context_t *context)
{
    int radius = effect_calculate_value(context, false);
    char dist[5];

    strnfmt(dist, sizeof(dist), "%d", radius);
    msg(context->player, "You hit a teleport trap!");
    effect_simple(context->player, EF_TELEPORT, dist, 0, 0, 0, NULL, NULL);
    return true;
}


static bool effect_handler_TRAP_SPOT_FIRE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    msg(context->player, "You are enveloped in flames!");
    my_strcpy(context->player->died_flavor, "was fried by a fire trap",
        sizeof(context->player->died_flavor));
    dam = adjust_dam(context->player, GF_FIRE, dam, RANDOMISE, 0);
    if (dam)
    {
        if (!take_hit(context->player, dam, "a fire trap", false))
            inven_damage(context->player, GF_FIRE, MIN(dam * 5, 300));
    }
    return true;
}


static bool effect_handler_TRAP_SPOT_ACID(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    msg(context->player, "You are splashed with acid!");
    my_strcpy(context->player->died_flavor, "was dissolved by an acid trap",
        sizeof(context->player->died_flavor));
    dam = adjust_dam(context->player, GF_ACID, dam, RANDOMISE, 0);
    if (dam)
    {
        if (!take_hit(context->player, dam, "an acid trap", false))
            inven_damage(context->player, GF_ACID, MIN(dam * 5, 300));
    }
    return true;
}


static bool effect_handler_TRAP_DART_SLOW(effect_handler_context_t *context)
{
    if (trap_check_hit(context->player, 125))
    {
        msg(context->player, "A small dart hits you!");
        my_strcpy(context->player->died_flavor, "was shot by a slowing dart",
            sizeof(context->player->died_flavor));
        if (take_hit(context->player, damroll(1, 4), "a trap", false)) return true;
        player_inc_timed(context->player, TMD_SLOW, randint0(20) + 20, true, false);
    }
    else
        msg(context->player, "A small dart barely misses you.");
    return true;
}


static bool effect_handler_TRAP_DART_LOSE_STR(effect_handler_context_t *context)
{
    if (trap_check_hit(context->player, 125))
    {
        msg(context->player, "A small dart hits you!");
        my_strcpy(context->player->died_flavor, "was shot by a weakening dart",
            sizeof(context->player->died_flavor));
        if (take_hit(context->player, damroll(1, 4), "a trap", false)) return true;
        effect_simple(context->player, EF_DRAIN_STAT, "0", STAT_STR, 0, 0, NULL, NULL);
    }
    else
        msg(context->player, "A small dart barely misses you.");
    return true;
}


static bool effect_handler_TRAP_DART_LOSE_DEX(effect_handler_context_t *context)
{
    if (trap_check_hit(context->player, 125))
    {
        msg(context->player, "A small dart hits you!");
        my_strcpy(context->player->died_flavor, "was shot by a small dart",
            sizeof(context->player->died_flavor));
        if (take_hit(context->player, damroll(1, 4), "a trap", false)) return true;
        effect_simple(context->player, EF_DRAIN_STAT, "0", STAT_DEX, 0, 0, NULL, NULL);
    }
    else
        msg(context->player, "A small dart barely misses you.");
    return true;
}


static bool effect_handler_TRAP_DART_LOSE_CON(effect_handler_context_t *context)
{
    if (trap_check_hit(context->player, 125))
    {
        msg(context->player, "A small dart hits you!");
        my_strcpy(context->player->died_flavor, "was shot by an exhausting dart",
            sizeof(context->player->died_flavor));
        if (take_hit(context->player, damroll(1, 4), "a trap", false)) return true;
        effect_simple(context->player, EF_DRAIN_STAT, "0", STAT_CON, 0, 0, NULL, NULL);
    }
    else
        msg(context->player, "A small dart barely misses you.");
    return true;
}


static bool effect_handler_TRAP_GAS_BLIND(effect_handler_context_t *context)
{
    msg(context->player, "You are surrounded by a black gas!");
    player_inc_timed(context->player, TMD_BLIND, randint0(50) + 25, true, true);
    return true;
}


static bool effect_handler_TRAP_GAS_CONFUSE(effect_handler_context_t *context)
{
    msg(context->player, "You are surrounded by a gas of scintillating colors!");
    player_inc_timed(context->player, TMD_CONFUSED, randint0(20) + 10, true, true);
    return true;
}


static bool effect_handler_TRAP_GAS_POISON(effect_handler_context_t *context)
{
    msg(context->player, "You are surrounded by a pungent green gas!");
    player_inc_timed(context->player, TMD_POISONED, randint0(20) + 10, true, true);
    return true;
}


static bool effect_handler_TRAP_GAS_SLEEP(effect_handler_context_t *context)
{
    msg(context->player, "You are surrounded by a strange white mist!");
    player_inc_timed(context->player, TMD_PARALYZED, randint0(10) + 5, true, true);
    return true;
}


/*
 * Useful things about effects.
 */
static const struct effect_kind effects[] =
{
    {EF_NONE, false, NULL, NULL, NULL},
    #define EFFECT(x, a, b, c, d, e) {EF_##x, a, b, effect_handler_##x, e},
    #include "list-effects.h"
    #undef EFFECT
    {EF_MAX, false, NULL, NULL, NULL}
};


static const char *effect_names[] =
{
    NULL,
    #define EFFECT(x, a, b, c, d, e) #x,
    #include "list-effects.h"
    #undef EFFECT
    NULL
};


/* Utility functions */


static bool effect_valid(struct effect *effect)
{
    if (!effect) return false;
    return ((effect->index > EF_NONE) && (effect->index < EF_MAX));
}


bool effect_aim(struct effect *effect)
{
    struct effect *e = effect;

    if (!effect_valid(effect)) return false;

    while (e)
    {
        if (effects[e->index].aim) return true;
        e = e->next;
    }

    return false;
}


const char *effect_info(struct effect *effect)
{
    if (!effect_valid(effect)) return NULL;

    return effects[effect->index].info;
}


const char *effect_desc(struct effect *effect)
{
    if (!effect_valid(effect)) return NULL;

    return effects[effect->index].desc;
}


effect_index effect_lookup(const char *name)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(effect_names); i++)
    {
        const char *effect_name = effect_names[i];

        /* Test for equality */
        if ((effect_name != NULL) && streq(name, effect_name)) return i;
    }

    return EF_MAX;
}


/*
 * Translate a string to an effect parameter index
 */
int effect_param(int index, const char *type)
{
    int val = -1;

    /* Assign according to effect index */
    switch (index)
    {
        /* Projection name */
        case EF_ALTER:
        case EF_BALL:
        case EF_BALL_OBVIOUS:
        case EF_BEAM:
        case EF_BEAM_OBVIOUS:
        case EF_BLAST:
        case EF_BLAST_OBVIOUS:
        case EF_BOLT:
        case EF_BOLT_AWARE:
        case EF_BOLT_MELEE:
        case EF_BOLT_OR_BEAM:
        case EF_BOLT_STATUS:
        case EF_BOLT_STATUS_DAM:
        case EF_BOW_BRAND:
        case EF_BOW_BRAND_SHOT:
        case EF_BREATH:
        case EF_DAMAGE:
        case EF_LINE:
        case EF_PROJECT:
        case EF_PROJECT_LOS:
        case EF_PROJECT_LOS_AWARE:
        case EF_STAR:
        case EF_STAR_BALL:
        case EF_SWARM:
        case EF_TELEPORT:
        case EF_TELEPORT_LEVEL:
        case EF_TOUCH:
        case EF_TOUCH_AWARE:
        {
            val = gf_name_to_idx(type);
            break;
        }

        /* Timed effect name */
        case EF_CURE:
        case EF_TIMED_DEC:
        case EF_TIMED_INC:
        case EF_TIMED_INC_NO_RES:
        case EF_TIMED_SET:
        {
            val = timed_name_to_idx(type);
            break;
        }

        /* Monster timed effect name */
        case EF_MON_TIMED_INC:
        {
            val = mon_timed_name_to_idx(type);
            break;
        }

        /* Summon name */
        case EF_SUMMON:
        {
            val = summon_name_to_idx(type);
            break;
        }

        /* Stat name */
        case EF_DRAIN_STAT:
        case EF_GAIN_STAT:
        case EF_LOSE_RANDOM_STAT:
        case EF_RESTORE_STAT:
        {
            val = stat_name_to_idx(type);
            break;
        }

        /* Anything else shouldn't be calling this */
        default: break;
    }

    return val;
}


/*
 * Do an effect (usually from a given object).
 * Boost is the extent to which skill surpasses difficulty, used as % boost. It
 * ranges from 0 to 138.
 */
bool effect_do(struct player *p, struct effect *effect, bool *ident, bool aware, int dir,
    struct beam_info *beam, int boost, quark_t note, struct monster *mon,
    struct monster *target_m_ptr)
{
    bool completed = false;
    effect_handler_f handler;
    struct actor actor_body;
    struct actor *data = &actor_body;
    int depth;

    /* Paranoia */
    if (p) depth = p->depth;
    else if (mon) depth = mon->depth;
    else if (target_m_ptr) depth = target_m_ptr->depth;
    else quit_fmt("No valid actor in effect_do(). Please report this bug.");

    data->idx = 0;
    data->player = p;
    data->mon = mon;

    do
    {
        int random_choices = 0, leftover = 0;
        random_value value;

        if (!effect_valid(effect))
            quit_fmt("Bad effect passed to effect_do(). Please report this bug.");

        memset(&value, 0, sizeof(value));
        if (effect->dice != NULL)
            random_choices = dice_roll(effect->dice, (void *)data, &value);

        /* Deal with special random effect */
        if (effect->index == EF_RANDOM)
        {
            int choice = randint0(random_choices);

            leftover = random_choices - choice;

            /* Skip to the chosen effect */
            effect = effect->next;
            while (choice--) effect = effect->next;

            /* Roll the damage, if needed */
            memset(&value, 0, sizeof(value));
            if (effect->dice != NULL)
                dice_roll(effect->dice, (void *)data, &value);
        }

        /* Handle the effect */
        handler = effects[effect->index].handler;
        if (handler != NULL)
        {
            effect_handler_context_t context;

            context.player = p;
            context.mon = mon;
            context.cave = chunk_get(depth);
            context.effect = effect->index;
            context.aware = aware;
            context.dir = dir;
            context.beam = (beam? beam->beam: 0);
            context.boost = boost;
            context.value = value;
            context.p1 = effect->params[0];
            context.p2 = effect->params[1];
            context.p3 = effect->params[2];
            context.self_msg = effect->self_msg;
            context.ident = *ident;
            context.note = note;
            context.spell_power = (beam? beam->spell_power: 0);
            context.elem_power = (beam? beam->elem_power: 0);
            context.flag = effect->flag;
            context.target_m_ptr = target_m_ptr;

            completed = handler(&context);
            *ident = context.ident;

            /* PWMAngband: stop at the first non-handled effect */
            if (!completed) return false;
        }
        else
            quit_fmt("Effect not handled. Please report this bug.");

        /* Get the next effect, if there is one */
        if (leftover)
        {
            /* Skip the remaining non-chosen effects */
            while (leftover--) effect = effect->next;
        }
        else
            effect = effect->next;
    }
    while (effect);

    return completed;
}


/*
 * Perform a single effect with a simple dice string and parameters
 * Calling with ident a valid pointer will (depending on effect) give success
 * information; ident = NULL will ignore this
 */
bool effect_simple(struct player *p, int index, const char* dice_string, int p1, int p2, int p3,
    bool *ident, struct monster *mon)
{
    struct effect *effect = mem_zalloc(sizeof(*effect));
    int dir = 0;
    bool dummy_ident = false, result;

    /* Set all the values */
    effect->index = index;
    effect->dice = dice_new();
    dice_parse_string(effect->dice, dice_string);
    effect->params[0] = p1;
    effect->params[1] = p2;
    effect->params[2] = p3;

    /* Direction if needed (PWMAngband: simply use actual target) */
    if (effect_aim(effect)) dir = 5;

    /* Do the effect */
    if (ident)
        result = effect_do(p, effect, ident, true, dir, NULL, 0, 0, mon, NULL);
    else
        result = effect_do(p, effect, &dummy_ident, true, dir, NULL, 0, 0, mon, NULL);

    free_effect(effect);
    return result;
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(struct player *p, int typ, int dir, int dam, int rad, bool obvious)
{
    int ty, tx;
    bool result;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    struct actor who_body;
    struct actor *who = &who_body;

    ACTOR_PLAYER(who, get_player_index(get_connection(p->conn)), p);

    if (obvious) flg |= PROJECT_AWARE;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    /* Use the given direction */
    tx = p->px + ddx[dir];
    ty = p->py + ddy[dir];

    /* Hack -- use an actual "target" */
    if ((dir == 5) && target_okay(p))
    {
        flg &= ~(PROJECT_STOP | PROJECT_THRU);
        target_get(p, &tx, &ty);
    }

    /* Analyze the "dir" and the "target".  Hurt items on floor. */
    p->current_sound = -2;
    result = project(who, rad, chunk_get(p->depth), ty, tx, dam, typ, flg, 0, 0, "annihilated");
    p->current_sound = -1;
    return result;
}
