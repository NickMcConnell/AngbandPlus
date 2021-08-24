/*
 * File: effects.c
 * Purpose: Handler and auxiliary functions for every effect in the game
 *
 * Copyright (c) 2007 Andi Sidwell
 * Copyright (c) 2016 Ben Semmler, Nick McConnell
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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
 * Structures and helper functions for effects
 */


typedef struct effect_handler_context_s
{
    effect_index effect;
    struct source *origin;
    struct chunk *cave;
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
 * Stat adjectives
 */
const char *desc_stat(int stat, bool positive)
{
    struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_STAT, stat);

    if (positive) return prop->adjective;
    return prop->neg_adj;
}


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


static void get_target(struct source *origin, int dir, int *ty, int *tx)
{
    /* MvX */
    if (origin->monster)
    {
        if (origin->monster->m_timed[MON_TMD_CONF] && one_in_(CONF_RANDOM_CHANCE))
        {
            dir = ddd[randint0(8)];
            *ty = origin->monster->fy + ddy[dir];
            *tx = origin->monster->fx + ddx[dir];
        }
        else
        {
            *ty = origin->player->py;
            *tx = origin->player->px;
        }
    }

    /* Ask for a target if no direction given */
    else if ((dir == 5) && target_okay(origin->player))
        target_get(origin->player, tx, ty);

    /* Use the adjacent grid in the given direction as target */
    else
    {
        *ty = origin->player->py + ddy[dir];
        *tx = origin->player->px + ddx[dir];
    }
}


/*
 * Apply the project() function in a direction, or at a target
 */
bool project_aimed(struct source *origin, int typ, int dir, int dam, int flg, const char *what)
{
    int ty, tx;
    struct chunk *c = chunk_get(&origin->player->wpos);
    struct source who_body;
    struct source *who = &who_body;

    /* Pass through the target if needed */
    flg |= (PROJECT_THRU);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    get_target(origin, dir, &ty, &tx);

    /* Hack -- only one source */
    if (origin->monster)
        source_monster(who, origin->monster);
    else
        source_player(who, get_player_index(get_connection(origin->player->conn)), origin->player);

    /* Aim at the target, do NOT explode */
    return (project(who, 0, c, ty, tx, dam, typ, flg, 0, 0, what));
}


/*
 * Apply the project() function to grids around the player
 */
static bool project_touch(struct player *p, int dam, int rad, int typ, bool aware)
{
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY | PROJECT_HIDE | PROJECT_THRU;
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(p->conn)), p);

    if (aware) flg |= PROJECT_AWARE;
    return (project(who, rad, chunk_get(&p->wpos), py, px, dam, typ, flg, 0, 0, "killed"));
}


/*
 * Selects items that have at least one removable curse.
 */
static bool item_tester_uncursable(const struct object *obj)
{
    struct curse_data *c = obj->known->curses;
    size_t i;

    for (i = 0; c && (i < (size_t)z_info->curse_max); i++)
    {
        if (c[i].power == 0) continue;
        if (c[i].power < 100) return true;
    }

    return false;
}


/*
 * Removes an individual curse from an object.
 */
static void remove_object_curse(struct player *p, struct object *obj, int index, bool message)
{
    struct curse_data *c = &obj->curses[index];
    char *name = curses[index].name;
    int i;

    memset(c, 0, sizeof(struct curse_data));
    if (message) msg(p, "The %s curse is removed!", name);

    /* Check to see if that was the last one */
    for (i = 0; i < z_info->curse_max; i++)
    {
        if (obj->curses[i].power) return;
    }

    mem_free(obj->curses);
    obj->curses = NULL;
}


/*
 * Attempts to remove a curse from an object.
 */
static bool uncurse_object(struct player *p, struct object *obj, int strength)
{
    int index = p->current_action;
    struct curse_data *curse;
    bool carried;
    int y, x;

    /* Paranoia */
    if ((index < 0) || (index >= z_info->curse_max)) return false;

    curse = &obj->curses[index];

    /* Save object info (backfire may destroy it) */
    carried = object_is_carried(p, obj);
    y = obj->iy;
    x = obj->ix;

    /* Curse is permanent */
    if (curse->power >= 100) return false;

    /* Successfully removed this curse */
    if (strength >= curse->power)
    {
        remove_object_curse(p, obj->known, index, false);
        remove_object_curse(p, obj, index, true);
    }

    /* Failure to remove, object is now fragile */
    else if (!of_has(obj->flags, OF_FRAGILE))
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_FULL);
        msgt(p, MSG_CURSED, "The removal fails. Your %s is now fragile.", o_name);

        of_on(obj->flags, OF_FRAGILE);
        player_learn_flag(p, OF_FRAGILE);
    }

    /* Failure - unlucky fragile object is destroyed */
    else if (one_in_(4))
    {
        msg(p, "There is a bang and a flash!");
        take_hit(p, damroll(5, 5), "a failed attempt at uncursing", false,
            "was killed by a failed attempt at uncursing");

        /* Preserve any artifact */
        preserve_artifact_aux(obj);
        if (obj->artifact) history_lose_artifact(p, obj);

        use_object(p, obj, 1, false);
    }

    /* Non-destructive failure */
    else
        msg(p, "The removal fails.");

    /* Housekeeping */
    p->upkeep->update |= (PU_BONUS);
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_EQUIP | PR_INVEN);
    if (!carried) redraw_floor(&p->wpos, y, x);

    return true;
}


/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT   0x01
#define ENCH_TODAM   0x02
#define ENCH_TOBOTH  0x03
#define ENCH_TOAC    0x04


/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] =
{
    0, 10, 20, 40, 80, 160, 280, 400,
    550, 700, 800, 900, 950, 970, 990, 1000
};


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
 * Helper function for enchant() which tries increasing an item's bonuses
 *
 * Returns true if a bonus was increased
 */
static bool enchant_aux(struct player *p, struct object *obj, s16b *score)
{
    bool result = false;
    bool is_artifact = (obj->artifact? true: false);

    if (enchant_score(score, is_artifact)) result = true;
    return result;
}


/*
 * Enchant an item
 *
 * Revamped! Now takes item pointer, number of times to try enchanting, and a
 * flag of what to try enchanting. Artifacts resist enchantment some of the
 * time.
 *
 * Note that an item can technically be enchanted all the way to +15 if you
 * wait a very, very, long time. Going from +9 to +10 only works about 5% of
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

    /* Magic ammo cannot be enchanted */
    if (tval_is_ammo(obj) && of_has(obj->flags, OF_AMMO_MAGIC)) return false;

    /* Artifact ammo cannot be enchanted */
    if (tval_is_ammo(obj) && obj->artifact) return false;

    /* Mage weapons and dark swords are always +0 +0 */
    if (tval_is_mstaff(obj) || tval_is_dark_sword(obj)) return false;

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


static struct ego_item *ego_elemental(void)
{
    int i;

    for (i = 0; i < z_info->e_max; i++)
    {
        if (streq(e_info[i].name, "(Elemental)")) return &e_info[i];
    }

    return NULL;
}


/*  
 * Brand weapons (or ammo)
 *
 * Turns the (non-magical) object into an ego-item of type 'brand'.
 */
static void brand_object(struct player *p, struct object *obj, const char *brand, const char *name)
{
    int i;
    struct ego_item *ego;
    bool ok = false;

    /* You can never modify artifacts, ego items or worthless items */
    if (obj && obj->kind->cost && !obj->artifact && !obj->ego)
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
                struct poss_item *poss;

                for (poss = ego->poss_items; poss; poss = poss->next)
                {
                    if (poss->kidx == obj->kind->kidx)
                        ok = true;
                }
            }
            if (ok) break;
        }

        /* Hack -- BRAND_COOL */
        if (streq(brand, "BRAND_COOL")) ego = ego_elemental();

        /* Make it an ego item */
        obj->ego = ego;
        ego_apply_magic(obj, 0);

        /* Hack -- BRAND_COOL */
        if (streq(brand, "BRAND_COOL"))
        {
            /* Brand the object */
            append_brand(&obj->brands, get_brand("cold", 2));
        }

        object_notice_ego(p, obj);

        /* Update the gear */
        p->upkeep->update |= (PU_INVEN);

        /* Combine the pack (later) */
        p->upkeep->notice |= (PN_COMBINE);

        /* Redraw */
        p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

        /* Enchant */
        enchant(p, obj, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Endless source of cash? No way... make them worthless */
        set_origin(obj, ORIGIN_WORTHLESS, p->wpos.depth, NULL);
        if (object_was_sensed(obj) || object_is_known(p, obj))
            p->upkeep->notice |= PN_IGNORE;
    }
    else
        msg(p, "The branding failed.");
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
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(const struct object *obj)
{
    /* Ammo */
    if (!tval_is_ammo(obj)) return false;

    /* Normal magic ammo cannot be branded */
    if (of_has(obj->flags, OF_AMMO_MAGIC)) return false;

    return true;
}


/*
 * Set magical detection counter for a monster/player.
 *
 * Since the counter will be decremented at the end of the same turn, we set the counter to 2
 * to allow detection to last for one turn.
 */
static void give_detect(struct player *p, struct source *who)
{
    /* Players */
    if (who->player) p->play_det[who->idx] = 2;

    /* Monsters */
    else p->mon_det[who->idx] = 2;
}


/*
 * Apply a "project()" directly to all viewable monsters
 */
static bool project_los(struct player *p, int typ, int dam, bool obvious)
{
    int i, x, y;
    int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_PLAY | PROJECT_HIDE;
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);

    source_player(who, get_player_index(get_connection(p->conn)), p);

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

        /* Skip players not on this level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

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
static bool fire_beam(struct source *origin, int typ, int dir, int dam, bool obvious)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_PLAY;

    if (obvious) flg |= PROJECT_AWARE;
    origin->player->current_sound = -2;
    result = project_aimed(origin, typ, dir, dam, flg, "annihilated");
    origin->player->current_sound = -1;
    return result;
}


static bool light_line_aux(struct source *origin, int dir, int typ, int dam)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL | PROJECT_PLAY;

    origin->player->current_sound = -2;
    result = project_aimed(origin, typ, dir, dam, flg, "killed");
    origin->player->current_sound = -1;
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
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);

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
        if (monster_is_visible(p, i)) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all invisible monsters */
        if (monster_is_invisible(mon->race))
        {
            struct actor_race *monster_race = &p->upkeep->monster_race;

            /* Take note that they are invisible */
            rf_on(lore->flags, RF_INVISIBLE);

            /* Update monster recall window */
            if (ACTOR_RACE_EQUAL(monster_race, mon)) p->upkeep->redraw |= (PR_MONSTER);

            /* Increment detection counter */
            source_monster(who, mon);
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
        if (player_is_visible(p, i)) continue;

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

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
            source_player(who, i, q);
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
        if (OPT(p, pause_after_detect)) Send_pause(p);
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
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);

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
        if (monster_is_visible(p, i)) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all non-invisible, obvious monsters */
        if (!monster_is_invisible(mon->race) && !monster_is_camouflaged(mon))
        {
            /* Increment detection counter */
            source_monster(who, mon);
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
        if (player_is_visible(p, i)) continue;

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

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
            source_player(who, i, q);
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
        if (OPT(p, pause_after_detect)) Send_pause(p);
    }
    else if (aware && !flag)
        msg(p, "You sense no monsters.");

    /* Result */
    return (flag);
}


static struct player *get_inscribed_player(struct player *p, quark_t note)
{
    struct player *q = NULL;
    char *inscription = (char*)quark_str(note);

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
static bool fire_bolt(struct source *origin, int typ, int dir, int dam, bool obvious)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

    if (obvious) flg |= PROJECT_AWARE;
    return (project_aimed(origin, typ, dir, dam, flg, "annihilated"));
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
 * Selects the recall depth.
 *
 * Inscribe @Rdepth to recall to a specific depth.
 * Inscribe @Rx,y to recall to a specific wilderness level (this assumes
 * that the player has explored the respective wilderness level).
 */
static void set_recall_depth(struct player *p, quark_t note)
{
    unsigned char *inscription = (unsigned char *)quark_str(note);
    struct wild_type *w_ptr = get_wt_info_at(p->wpos.wy, p->wpos.wx);

    /* Default to the players maximum depth */
    COORDS_SET(&p->recall_wpos, p->wpos.wy, p->wpos.wx, p->max_depth);

    /* PWMAngband: check minimum/maximum depth of current dungeon */
    if (p->recall_wpos.depth > 0)
    {
        p->recall_wpos.depth = MAX(p->recall_wpos.depth, w_ptr->min_depth);
        p->recall_wpos.depth = MIN(p->recall_wpos.depth, w_ptr->max_depth - 1);
    }

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
                    int x, y;
                    char buf[NORMAL_WID];

                    /* A valid @R has been located */
                    inscription++;

                    /* Convert the inscription into wilderness coordinates */
                    if ((3 == sscanf(inscription, "%d,%d%s", &x, &y, buf)) ||
                        (2 == sscanf(inscription, "%d,%d", &x, &y)))
                    {
                        /* Forbid if no wilderness */
                        if (cfg_diving_mode || OPT(p, birth_no_recall))
                        {
                            /* Deactivate recall */
                            memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                            return;
                        }

                        /* Do some bounds checking/sanity checks */
                        w_ptr = get_wt_info_at(y, x);
                        if (w_ptr)
                        {
                            /* Verify that the player has visited here before */
                            if (wild_is_explored(p, &w_ptr->wpos))
                            {
                                memcpy(&p->recall_wpos, &w_ptr->wpos, sizeof(struct worldpos));
                                return;
                            }
                        }

                        /* Deactivate recall */
                        memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                        return;
                    }

                    /* Convert the inscription into a level index */
                    if ((2 == sscanf(inscription, "%d%s", &x, buf)) ||
                        (1 == sscanf(inscription, "%d", &x)))
                    {
                        /* Help avoid typos */
                        if (x % 50)
                        {
                            /* Deactivate recall */
                            memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                            return;
                        }

                        /* Convert from ft to index */
                        x /= 50;

                        /* Do some bounds checking/sanity checks */
                        if ((x >= w_ptr->min_depth) && (x <= p->recall_wpos.depth))
                        {
                            p->recall_wpos.depth = x;
                            break;
                        }

                        /* Deactivate recall */
                        memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                        return;
                    }

                    /* Deactivate recall */
                    memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                    return;
                }
            }
            inscription++;
        }
    }

    /* Force descent to a lower level if allowed */
    if (((cfg_limit_stairs == 3) || OPT(p, birth_force_descend)) &&
        (p->max_depth < z_info->max_depth - 1))
    {
        p->recall_wpos.depth = dungeon_get_next_level(p, p->max_depth, 1);
    }
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
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(p->conn)), p);

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
    result = project(who, rad, chunk_get(&p->wpos), ty, tx, dam, typ, flg, 0, 0, "annihilated");
    p->current_sound = -1;
    return result;
}


/*
 * Effect handlers
 */


static bool effect_handler_ACQUIRE(effect_handler_context_t *context)
{
    int num = effect_calculate_value(context, false);

    acquirement(context->origin->player, context->cave, num, 0);
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

    if (project_aimed(context->origin, context->p1, context->dir, 0, flg, "killed"))
        context->ident = true;
    return true;
}


static bool effect_handler_ALTER_REALITY(effect_handler_context_t *context)
{
    int i;

    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot alter this level...");
        return false;
    }

    /* Search for players on this level */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Only players on this level */
        if (!COORDS_EQUAL(&q->wpos, &context->origin->player->wpos)) continue;

        /* Tell the player about it */
        msg(q, "The world changes!");

        /* Generate a new level (later) */
        q->upkeep->new_level_method = LEVEL_RAND;
    }

    /* Deallocate the level */
    chunk_list_remove(context->cave);
    cave_wipe(context->cave);
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
    int dam = effect_calculate_value(context, true);
    int rad = (context->p2? context->p2: 2);
    struct source who_body;
    struct source *who = &who_body;
    int ty, tx;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    const char *what = "annihilated";

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Player or monster? */
    if (context->origin->monster)
    {
        source_monster(who, context->origin->monster);
        if (monster_is_powerful(context->origin->monster->race)) rad++;
        flg &= ~(PROJECT_STOP | PROJECT_THRU);

        /* Handle confusion */
        if (context->origin->monster->m_timed[MON_TMD_CONF] && one_in_(CONF_RANDOM_CHANCE))
        {
            int dir = ddd[randint0(8)];

            ty = context->origin->monster->fy + ddy[dir];
            tx = context->origin->monster->fx + ddx[dir];
        }

        /* MvM */
        else if (context->target_m_ptr)
        {
            ty = context->target_m_ptr->fy;
            tx = context->target_m_ptr->fx;
        }

        /* MvP */
        else
        {
            ty = context->origin->player->py;
            tx = context->origin->player->px;
        }
    }
    else
    {
        struct trap *trap = context->origin->trap;

        if (trap)
        {
            ty = trap->fy;
            tx = trap->fx;
            source_trap(who, trap);
        }
        else
        {
            if (context->p3) rad += context->origin->player->lev / context->p3;

            /* Hack -- mimics */
            if (context->origin->player->poly_race &&
                monster_is_powerful(context->origin->player->poly_race))
            {
                rad++;
            }

            /* Hack -- elementalists */
            rad = rad + context->spell_power / 2;
            rad = rad * (20 + context->elem_power) / 20;

            source_player(who, get_player_index(get_connection(context->origin->player->conn)),
                context->origin->player);

            /* Ask for a target if no direction given */
            if ((context->dir == 5) && target_okay(context->origin->player))
            {
                flg &= ~(PROJECT_STOP | PROJECT_THRU);
                target_get(context->origin->player, &tx, &ty);
            }

            /* Use the given direction */
            else
            {
                ty = context->origin->player->py + ddy[context->dir];
                tx = context->origin->player->px + ddx[context->dir];
            }
        }
    }

    /* Aim at the target, explode */
    context->origin->player->current_sound = -2;
    if (project(who, rad, context->cave, ty, tx, dam, context->p1, flg, 0, 0, what))
        context->ident = true;
    context->origin->player->current_sound = -1;

    return true;
}


/*
 * Cast a ball spell which effect is obvious.
 * If context->p3 is negative, allow only on random levels.
 */
static bool effect_handler_BALL_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int rad = context->p2 + ((context->p3 > 0)? (context->origin->player->lev / context->p3): 0);

    /* Only on random levels */
    if ((context->p3 < 0) && !random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create traps here...");
        context->ident = true;
        return false;
    }

    if (fire_ball(context->origin->player, context->p1, context->dir, dam, rad, true))
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
    const char *pself = player_self(context->origin->player);
    char df[160];

    context->ident = true;

    /* Search all monsters and find the closest */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip Unique Monsters */
        if (monster_is_unique(mon->race)) continue;

        /* Check distance */
        if ((tmp = distance(context->origin->player->py, context->origin->player->px,
            mon->fy, mon->fx)) < d)
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
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* Delete the monsters of that "type" */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip Unique Monsters */
        if (monster_is_unique(mon->race)) continue;

        /* Skip "wrong" monsters */
        if (mon->race->d_char != typ) continue;

        /* Delete the monster */
        delete_monster_idx(context->cave, i);

        /* Take some damage */
        dam += randint1(4);
    }

    /* Hurt the player */
    strnfmt(df, sizeof(df), "exhausted %s with Banishment", pself);
    take_hit(context->origin->player, dam, "the strain of casting Banishment", false, df);

    /* Update monster list window */
    if (dam > 0) context->origin->player->upkeep->redraw |= (PR_MONLIST);

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

    fire_beam(context->origin, context->p1, context->dir, dam, false);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Cast a beam spell which effect is obvious
 */
static bool effect_handler_BEAM_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    if (fire_beam(context->origin, context->p1, context->dir, dam, true))
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
            msg(context->origin->player, "You are surrounded by a malignant aura.");

            /* Decrease all stats (permanently) */
            player_stat_dec(context->origin->player, STAT_STR, true);
            player_stat_dec(context->origin->player, STAT_INT, true);
            player_stat_dec(context->origin->player, STAT_WIS, true);
            player_stat_dec(context->origin->player, STAT_DEX, true);
            player_stat_dec(context->origin->player, STAT_CON, true);

            /* Lose some experience (permanently) */
            player_exp_lose(context->origin->player, context->origin->player->exp / 4, true);

            break;
        }

        case 3:
        {
            /* Message */
            msg(context->origin->player, "You are surrounded by a powerful aura.");

            /* Dispel monsters */
            project_los(context->origin->player, PROJ_DISP_ALL, 1000, false);

            break;
        }

        case 4:
        case 5:
        case 6:
        {
            /* Mana Ball */
            fire_ball(context->origin->player, PROJ_MANA, context->dir, 300, 3, false);

            break;
        }

        case 7:
        case 8:
        case 9:
        case 10:
        {
            /* Mana Bolt */
            fire_bolt(context->origin, PROJ_MANA, context->dir, 250, false);

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
    int rad = context->p2 + (context->p3? (context->origin->player->lev / context->p3): 0);

    /* Hack -- elementalists */
    rad = rad + context->spell_power / 2;
    rad = rad * (20 + context->elem_power) / 20;

    if (fire_ball(context->origin->player, context->p1, 0, dam, rad, false))
        context->ident = true;
    return true;
}


/*
 * Cast a ball spell centered on the character (with obvious effects)
 */
static bool effect_handler_BLAST_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = context->p2 + (context->p3? (context->origin->player->lev / context->p3): 0);

    /* Monster */
    if (context->origin->monster)
    {
        int rlev = ((context->origin->monster->race->level >= 1)?
            context->origin->monster->race->level: 1);
        struct source who_body;
        struct source *who = &who_body;

        rad = context->p2 + (context->p3? (rlev / context->p3): 0);

        source_monster(who, context->origin->monster);
        project(who, rad, context->cave, context->origin->monster->fy, context->origin->monster->fx,
            0, context->p1, PROJECT_ITEM | PROJECT_HIDE, 0, 0, "killed");
        update_smart_learn(context->origin->monster, context->origin->player, 0, 0, context->p1);
    }

    /* Player */
    else if (fire_ball(context->origin->player, context->p1, 0, dam, rad, true))
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
        context->origin->player->current_sound = -2;
        sound(context->origin->player, MSG_TPOTHER);
        if (fire_bolt(context->origin, context->p1, context->dir, dam, false))
            context->ident = true;
        context->origin->player->current_sound = -1;
    }

    /* MvM */
    else if (context->target_m_ptr)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct source who_body;
        struct source *who = &who_body;
        int ty, tx;

        if (context->origin->monster->m_timed[MON_TMD_CONF] && one_in_(CONF_RANDOM_CHANCE))
        {
            int dir = ddd[randint0(8)];

            ty = context->origin->monster->fy + ddy[dir];
            tx = context->origin->monster->fx + ddx[dir];
        }
        else
        {
            ty = context->target_m_ptr->fy;
            tx = context->target_m_ptr->fx;
        }

        source_monster(who, context->origin->monster);
        project(who, 0, context->cave, ty, tx, dam, context->p1, flag, 0, 0, "annihilated");
    }

    /* Normal case */
    else if (fire_bolt(context->origin, context->p1, context->dir, dam, false))
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

    /* Forbid in the towns and on special levels */
    if (context->p2 && forbid_special(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot polymorph monsters here...");
        context->ident = true;
        return false;
    }

    if (fire_bolt(context->origin, context->p1, context->dir, dam, context->aware))
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
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(context->origin->player->conn)),
        context->origin->player);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    tx = context->origin->player->px + ddx[context->dir];
    ty = context->origin->player->py + ddy[context->dir];

    /* Hack -- use an actual "target" */
    if ((context->dir == 5) && target_okay(context->origin->player))
    {
        target_get(context->origin->player, &tx, &ty);

        /* Check distance */
        if (distance(context->origin->player->py, context->origin->player->px, ty, tx) > 1)
        {
            msg(context->origin->player, "Target out of range.");
            context->ident = true;
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
    if (context->origin->player->timed[TMD_ANCHOR] && (context->p1 == PROJ_TIME))
    {
        if (one_in_(3))
        {
            msg(context->origin->player, "The space/time anchor stops your time bolt!");
            context->ident = true;
            return true;
        }
        if (one_in_(3))
            player_clear_timed(context->origin->player, TMD_ANCHOR, true);
    }

    if (magik(beam))
        fire_beam(context->origin, context->p1, context->dir, dam, false);
    else
        fire_bolt(context->origin, context->p1, context->dir, dam, false);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;
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
    bitflag old_type = context->origin->player->brand.type;
    bool old_blast = context->origin->player->brand.blast;

    /* Set brand type and damage */
    context->origin->player->brand.type = (bitflag)context->p1;
    context->origin->player->brand.blast = (context->p2? true: false);
    context->origin->player->brand.dam = (context->p2? effect_calculate_value(context, false): 0);

    /* Branding of the same type stacks */
    if (has_bowbrand(context->origin->player, old_type, old_blast))
    {
        player_inc_timed(context->origin->player, TMD_BOWBRAND, context->origin->player->lev, true,
            true);
    }

    /* Apply new branding */
    else
    {
        /* Force the message display */
        context->origin->player->timed[TMD_BOWBRAND] = 0;
        player_set_timed(context->origin->player, TMD_BOWBRAND,
            context->origin->player->lev + randint1(20), true);
    }

    return true;
}


static bool effect_handler_BOW_BRAND_SHOT(effect_handler_context_t *context)
{
    bitflag old_type = context->origin->player->brand.type;
    bool old_blast = context->origin->player->brand.blast;

    /* Set brand type and damage */
    context->origin->player->brand.type = (bitflag)context->p1;
    context->origin->player->brand.blast = false;
    context->origin->player->brand.dam = effect_calculate_value(context, false);

    /* Branding of the same type stacks */
    if (has_bowbrand(context->origin->player, old_type, old_blast))
    {
        player_inc_timed(context->origin->player, TMD_BOWBRAND, context->origin->player->lev, true,
            true);
    }

    /* Apply new branding */
    else
    {
        /* Force the message display */
        context->origin->player->timed[TMD_BOWBRAND] = 0;
        player_set_timed(context->origin->player, TMD_BOWBRAND,
            context->origin->player->lev + randint1(20), true);
    }

    return true;
}


/*
 * Brand some (non-magical) ammo
 */
static bool effect_handler_BRAND_AMMO(effect_handler_context_t *context)
{
    struct object *obj;

    context->ident = true;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_AMMO, "");
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->origin->player, obj) && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires ammo */
    if (!item_tester_hook_ammo(obj)) return false;

    /* Select the brand */
    if (one_in_(3))
        brand_object(context->origin->player, obj, "of Flame", "flames");
    else if (one_in_(2))
        brand_object(context->origin->player, obj, "of Frost", "frost");
    else
        brand_object(context->origin->player, obj, "of Venom", "venom");

    /* Redraw */
    if (!object_is_carried(context->origin->player, obj))
        redraw_floor(&context->origin->player->wpos, obj->iy, obj->ix);

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

    struct object *obj = equipped_item_by_slot_name(context->origin->player, "weapon");

    /* Select the brand */
    if (obj)
    {
        if (with_fire)
        {
            if (one_in_(2))
                brand_object(context->origin->player, obj, "of Flame", "flames");
            else
                brand_object(context->origin->player, obj, "of Frost", "frost");
        }
        else
        {
            if (one_in_(2))
                brand_object(context->origin->player, obj, "BRAND_COOL", "weak frost");
            else
                brand_object(context->origin->player, obj, "of Frost", "frost");
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
    int dam = effect_calculate_value(context, false);
    int type = context->p1;
    int rad = 0;
    struct source who_body;
    struct source *who = &who_body;
    int ty, tx;

    /*
     * Diameter of source starts at 40, so full strength up to 3 grids from
     * the breather.
     */
    int diameter_of_source = 40;
    int degrees_of_arc = context->p2;

    int flg = PROJECT_ARC | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

    /* Hack -- already used up */
    bool used = (context->p3 == 1);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Radius of zero means no fixed limit. */
    if (rad == 0) rad = z_info->max_range;

    /* Player or monster? */
    if (context->origin->monster)
    {
        source_monster(who, context->origin->monster);

        /* Breath parameters for monsters are monster-dependent */
        dam = breath_dam(type, context->origin->monster->hp);

        /* Powerful monsters' breath is now full strength at 5 grids */
        if (monster_is_powerful(context->origin->monster->race))
        {
            diameter_of_source *= 3;
            diameter_of_source /= 2;
        }

        /* MvM */
        if (context->target_m_ptr)
        {
            ty = context->target_m_ptr->fy;
            tx = context->target_m_ptr->fx;
        }

        /* MvP */
        else
        {
            ty = context->origin->player->py;
            tx = context->origin->player->px;
        }
    }
    else
    {
        /* PWMAngband: let Power Dragon Scale Mails breathe a random element */
        if (type == PROJ_MISSILE)
        {
            bitflag mon_breath[RSF_SIZE];

            /* Allow all elements */
            rsf_wipe(mon_breath);
            init_spells(mon_breath);
            set_breath(mon_breath);

            /* Get breath effect */
            type = breath_effect(context->origin->player, mon_breath);
        }

        /* Handle polymorphed players */
        else if (context->origin->player->poly_race && (dam == 0))
        {
            const char *pself = player_self(context->origin->player);
            char df[160];

            /* Damage */
            dam = breath_dam(type, context->origin->player->chp);

            /* Boost damage to take into account player hp vs monster hp */
            dam = (dam * 6) / 5;

            /* Breathing damages health instead of costing mana */
            strnfmt(df, sizeof(df), "exhausted %s with breathing", pself);
            take_hit(context->origin->player, context->origin->player->mhp / 20,
                "the strain of breathing", false, df);
            if (context->origin->player->is_dead) return !used;

            /* Powerful breath */
            if (monster_is_powerful(context->origin->player->poly_race))
            {
                diameter_of_source *= 3;
                diameter_of_source /= 2;
            }
        }

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        /* Ask for a target if no direction given */
        if ((context->dir == 5) && target_okay(context->origin->player))
            target_get(context->origin->player, &tx, &ty);
        else
        {
            /* Hack -- no target available, default to random direction */
            if (context->dir == 5) context->dir = 0;

            /* Hack -- no direction given, default to random direction */
            if (!context->dir) context->dir = ddd[randint0(8)];

            /* Use the given direction */
            ty = context->origin->player->py + ddy[context->dir];
            tx = context->origin->player->px + ddx[context->dir];
        }
    }

    /* Diameter of the energy source. */
    if (degrees_of_arc < 60)
    {
        /* This handles finite length beams */
        if (degrees_of_arc == 0)
            diameter_of_source = rad * 10;
        /*
         * Narrower cone means energy drops off less quickly. We now have:
         * - 30 degree regular breath  | full strength at 5 grids
         * - 30 degree powerful breath | full strength at 9 grids
         * - 20 degree regular breath  | full strength at 11 grids
         * - 20 degree powerful breath | full strength at 17 grids
         * where grids are measured from the breather.
         */
        else
            diameter_of_source = diameter_of_source * 60 / degrees_of_arc;
    }

    /* Max */
    if (diameter_of_source > 250) diameter_of_source = 250;

    /* Breathe at the target */
    context->origin->player->current_sound = -2;
    if (project(who, rad, context->cave, ty, tx, dam, type, flg, degrees_of_arc,
        diameter_of_source, "vaporized"))
    {
        context->ident = true;
    }
    context->origin->player->current_sound = -1;

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
        if (q->clazz->cidx == context->origin->player->clazz->cidx) continue;

        /* Ok we found a good class lets mimic */
        what = q->clazz->cidx;
        break;
    }

    /* Arg nothing .. bah be a warrior */
    if (!tries) what = 0;

    context->origin->player->tim_mimic_what = what;
    player_set_timed(context->origin->player, TMD_MIMIC, dur, true);
    return true;
}


static bool effect_handler_CONFUSING(effect_handler_context_t *context)
{
    if (!context->origin->player->confusing)
    {
        msg(context->origin->player, "Your hands begin to glow.");
        context->origin->player->confusing = true;
    }
    context->ident = true;
    return true;
}


static bool effect_handler_CREATE_HOUSE(effect_handler_context_t *context)
{
    context->ident = true;

    /* MAngband house creation code disabled for now */
    /*return create_house(p);*/

    return build_house(context->origin->player);
}


/*
 * Create potions of poison from any potion
 */
static bool effect_handler_CREATE_POISON(effect_handler_context_t *context)
{
    struct object *obj, *poison;
    int amt;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_POISON, "");
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->origin->player, obj) && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires a potion */
    if (!tval_is_potion(obj)) return false;

    /* Amount */
    amt = obj->number;

    /* Message */
    msg(context->origin->player, "You create %d potions of poison.", amt);

    /* Eliminate the item */
    use_object(context->origin->player, obj, amt, false);

    /* Create the potions */
    poison = object_new();
    object_prep(context->origin->player, poison, lookup_kind_by_name(TV_POTION, "Poison"), 0,
        MINIMISE);
    poison->number = amt;

    /* Set origin */
    set_origin(poison, ORIGIN_ACQUIRE, context->origin->player->wpos.depth, NULL);

    drop_near(context->origin->player, context->cave, &poison, 0, context->origin->player->py,
        context->origin->player->px, true, DROP_FADE);

    return true;
}


/*
 * Create stairs at the player location
 */
static bool effect_handler_CREATE_STAIRS(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;
    struct wild_type *w_ptr = get_wt_info_at(context->origin->player->wpos.wy,
        context->origin->player->wpos.wx);

    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create stairs here...");
        return false;
    }

    /* Only allow stairs to be created on empty floor */
    if (!square_isfloor(context->cave, py, px))
    {
        msg(context->origin->player, "There is no empty floor here.");
        return false;
    }

    /* Forbidden */
    if ((context->cave->wpos.depth == w_ptr->max_depth - 1) && (cfg_limit_stairs >= 2))
    {
        msg(context->origin->player, "You cannot create stairs here...");
        return false;
    }

    /* Push objects off the grid */
    if (square_object(context->cave, py, px))
        push_object(context->origin->player, context->cave, py, px);

    /* Surface: always down */
    if (context->cave->wpos.depth == 0)
        square_add_stairs(context->cave, py, px, FEAT_MORE);

    /* Bottom: always up */
    else if (context->cave->wpos.depth == w_ptr->max_depth - 1)
        square_add_stairs(context->cave, py, px, FEAT_LESS);

    /* Random */
    else
        square_add_stairs(context->cave, py, px, FEAT_NONE);

    return true;
}


static bool effect_handler_CREATE_WALLS(effect_handler_context_t *context)
{
    int num = effect_calculate_value(context, false);

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create walls here...");
        return false;
    }

    if (num)
    {
        int y;
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        for (y = 0; y < num; y++)
        {
            int dir = ddd[randint0(8)];

            project(who, 0, context->cave, context->origin->player->py + ddy[dir],
                context->origin->player->px + ddx[dir], 0, PROJ_STONE_WALL, PROJECT_GRID, 0, 0,
                "killed");
        }

        return true;
    }

    fire_ball(context->origin->player, PROJ_STONE_WALL, 0, 1, 1, false);
    return true;
}


static bool effect_handler_CRUNCH(effect_handler_context_t *context)
{
    if (!player_undead(context->origin->player))
    {
        if (one_in_(2))
            msg(context->origin->player, "It's crunchy.");
        else
            msg(context->origin->player, "It nearly breaks your tooth!");
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

    player_clear_timed(context->origin->player, type, true);
    context->ident = true;
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

    /* Notice */
    context->ident = true;

    /* Curse the body armor */
    obj = equipped_item_by_slot_name(context->origin->player, "body");

    /* Nothing to curse */
    if (!obj)
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* Describe */
    object_desc(context->origin->player, o_name, sizeof(o_name), obj, ODESC_FULL);

    /* Attempt a saving throw for artifacts */
    if (obj->artifact && magik(50))
    {
        msg(context->origin->player,
            "A terrible black aura tries to surround your armor, but your %s resists the effects!",
            o_name);
    }
    else
    {
        msg(context->origin->player, "A terrible black aura blasts your %s!", o_name);

        /* Damage the armor */
        obj->to_a -= randint1(3);

        /* Curse it */
        append_object_curse(obj, object_level(&context->origin->player->wpos), obj->tval);

        /* Recalculate bonuses */
        context->origin->player->upkeep->update |= (PU_BONUS);

        /* Redraw */
        context->origin->player->upkeep->redraw |= (PR_EQUIP);
    }

    return true;
}


/*
 * Curse the player's weapon
 */
static bool effect_handler_CURSE_WEAPON(effect_handler_context_t *context)
{
    struct object *obj;
    char o_name[NORMAL_WID];

    /* Notice */
    context->ident = true;

    /* Curse the weapon */
    obj = equipped_item_by_slot_name(context->origin->player, "weapon");

    /* Nothing to curse */
    if (!obj)
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* Describe */
    object_desc(context->origin->player, o_name, sizeof(o_name), obj, ODESC_FULL);

    /* Attempt a saving throw */
    if (obj->artifact && magik(50))
    {
        msg(context->origin->player,
            "A terrible black aura tries to surround your weapon, but your %s resists the effects!",
            o_name);
    }
    else
    {
        msg(context->origin->player, "A terrible black aura blasts your %s!", o_name);

        /* Damage the weapon */
        obj->to_h -= randint1(3);
        obj->to_d -= randint1(3);

        /* Curse it */
        append_object_curse(obj, object_level(&context->origin->player->wpos), obj->tval);

        /* Recalculate bonuses */
        context->origin->player->upkeep->update |= (PU_BONUS);

        /* Redraw */
        context->origin->player->upkeep->redraw |= (PR_EQUIP);
    }

    return true;
}
#endif


/*
 * Deal damage from the current monster or trap to the player
 */
static bool effect_handler_DAMAGE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    char killer[NORMAL_WID];
    struct monster *mon = context->origin->monster;
    struct trap *trap = context->origin->trap;
    int slot = slot_by_name(context->origin->player, "weapon");
    struct object *obj = context->origin->player->body.slots[slot].obj;
    bool non_physical;
    char df[160];

    if (mon)
    {
        const char *what = "annihilated";

        /* MvM */
        if (context->target_m_ptr)
        {
            int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
            struct source who_body;
            struct source *who = &who_body;

            source_monster(who, mon);
            project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx,
                dam, context->p1, flag, 0, 0, what);

            return true;
        }

        /* Get the "died from" name in case this attack kills @ */
        monster_desc(context->origin->player, killer, sizeof(killer), mon, MDESC_DIED_FROM);

        if ((context->p1 == PROJ_BLAST) || (context->p1 == PROJ_SMASH))
            what = "turned into an unthinking vegetable";

        non_physical = true;

        strnfmt(df, sizeof(df), "was %s by %s", what, killer);
    }

    /* A trap */
    else if (trap)
    {
        char *article = (is_a_vowel(trap->kind->desc[0])? "an ": "a ");

        strnfmt(killer, sizeof(killer), "%s%s", article, trap->kind->desc);

        non_physical = false;

        trap_msg_death(context->origin->player, trap, df, sizeof(df));
    }

    /* Must be a cursed weapon */
    else
    {
        my_assert(obj);
        object_desc(context->origin->player, killer, sizeof(killer), obj, ODESC_PREFIX | ODESC_BASE);

        non_physical = false;

        strnfmt(df, sizeof(df), "was killed by %s", killer);
    }

    /* Always ID */
    context->ident = true;

    /* Hit the player */
    take_hit(context->origin->player, dam, killer, non_physical, df);

    return true;
}


/*
 * Call darkness around the player
 * Affect all monsters in the projection radius (context->p2)
 */
static bool effect_handler_DARKEN_AREA(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;
    int dam = effect_calculate_value(context, false);
    int rad = context->p2;
    int flg = PROJECT_GRID | PROJECT_KILL | PROJECT_PLAY;
    struct source who_body;
    struct source *who = &who_body;

    /* Assume seen */
    context->ident = true;

    /* No effect outside of the dungeon during day */
    if ((context->cave->wpos.depth == 0) && is_daytime())
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* No effect on special levels */
    if (special_level(&context->cave->wpos))
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* MvM */
    if (context->target_m_ptr)
    {
        py = context->target_m_ptr->fy;
        px = context->target_m_ptr->fx;
        source_monster(who, context->origin->monster);
    }
    else
    {
        /* Message */
        if (!context->origin->player->timed[TMD_BLIND])
            msg(context->origin->player, "Darkness surrounds you.");

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);
    }

    /* Hook into the "project()" function */
    project(who, rad, context->cave, py, px, dam, PROJ_DARK_WEAK, flg, 0, 0, "killed");

    /* Darken the room */
    light_room(context->origin->player, context->cave, py, px, false);

    /* Hack -- blind the player directly if player-cast */
    if (!context->origin->monster)
    {
        if (!player_resists(context->origin->player, ELEM_DARK))
            player_inc_timed(context->origin->player, TMD_BLIND, 3 + randint1(5), true, true);
        equip_learn_element(context->origin->player, ELEM_DARK);
    }

    return true;
}


static bool effect_handler_DEATH(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    /* Not when already undead */
    if (context->origin->player->ghost)
    {
        msg(context->origin->player, "You call upon Death... but nothing happens.");
        return false;
    }

    /* Kill opponent */
    fire_bolt(context->origin, PROJ_DEATH, context->dir, dam, false);

    /* Turn him into an undead being */
    player_turn_undead(context->origin->player);

    return true;
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
    struct wild_type *w_ptr = get_wt_info_at(context->origin->player->wpos.wy,
        context->origin->player->wpos.wx);
    struct worldpos wpos;

    context->ident = true;

    /* Special case: no dungeon or winner-only dungeon */
    if ((w_ptr->max_depth == 1) || forbid_entrance(context->origin->player))
    {
        /* Don't apply effect while in the wilderness */
        if (apply) return false;

        /* Set the timer */
        set_descent(context->origin->player);

        return true;
    }

    /* Calculate target depth */
    target_increment = (4 / z_info->stair_skip) + 1;
    target_depth = dungeon_get_next_level(context->origin->player,
        context->origin->player->max_depth, target_increment);

    COORDS_SET(&wpos, context->origin->player->wpos.wy, context->origin->player->wpos.wx,
        target_depth);

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(&wpos))
    {
        /* Don't apply effect while DM is redesigning the level */
        if (apply) return false;

        /* Set the timer */
        set_descent(context->origin->player);

        return true;
    }

    /* Determine the level */
    if (target_depth > context->origin->player->wpos.depth)
    {
        /* Set the timer */
        if (!apply)
        {
            set_descent(context->origin->player);

            return true;
        }

        /* Change location */
        disturb(context->origin->player, 0);
        msgt(context->origin->player, MSG_TPLEVEL, "The floor opens beneath you!");
        msg_misc(context->origin->player, " sinks through the floor!");
        dungeon_change_level(context->origin->player, context->cave, &wpos, LEVEL_RAND);
        return true;
    }

    /* Just print a message when unable to set the timer */
    if (!apply)
    {
        msg(context->origin->player,
            "You sense a malevolent presence blocking passage to the levels below.");
        return true;
    }

    /* Otherwise do something disastrous */
    msg(context->origin->player, "You are thrown back in an explosion!");
    effect_simple(EF_DESTRUCTION, context->origin, "0", 0, 5, 1, NULL);
    return true;
}


/*
 * Unlight the dungeon map.
 */
static bool effect_handler_DEEP_NIGHTS(effect_handler_context_t *context)
{
    int i;

    /* No effect outside of the dungeon during day */
    if ((context->origin->player->wpos.depth == 0) && is_daytime())
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* No effect on special levels */
    if (special_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* Check for every other player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);
        struct object *obj;

        /* Only works for players on the level */
        if (!COORDS_EQUAL(&player->wpos, &context->origin->player->wpos)) continue;

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
    int y1 = context->origin->player->py;
    int x1 = context->origin->player->px;
    int hurt[MAX_PLAYERS];
    int count = 0;

    if (context->p2) r = context->p2;
    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        if (!context->p3) msg(context->origin->player, "The ground shakes for a moment.");
        return true;
    }

    if (!context->p3) msg_misc(context->origin->player, " unleashes great power!");

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

            /* Forget completely */
            square_unglow(context->cave, y, x);
            square_forget_all(context->cave, y, x);
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
                square_forget_pile_all(context->cave, y, x);
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
        equip_learn_element(p, ELEM_LIGHT);
        if (!player_resists(p, ELEM_LIGHT))
            player_inc_timed(p, TMD_BLIND, 10 + randint1(10), true, true);

        /* Fully update the visuals */
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

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
    struct source who_body;
    struct source *who = &who_body;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Increment detection counter */
        source_monster(who, mon);
        give_detect(context->origin->player, who);

        /* Detect */
        detect = true;
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this level */
        if (!COORDS_EQUAL(&q->wpos, &context->origin->player->wpos)) continue;

        /* Skip ourself */
        if (q == context->origin->player) continue;

        /* Increment detection counter */
        source_player(who, i, q);
        give_detect(context->origin->player, who);

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
        context->origin->player->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(context->origin->player);

        /* Normal refresh (without monster/object lists) */
        context->origin->player->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(context->origin->player, "An image of all nearby life-forms appears in your mind!");
        party_msg_near(context->origin->player, " senses the presence of all nearby life-forms!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }
    else
        msg(context->origin->player, "The level is devoid of life.");

    /* Result */
    context->ident = true;
    return true;
}


/*
 * Detect doors around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_DOORS(effect_handler_context_t *context)
{
    int x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;
    bool doors = false, redraw = false;

    /* Pick an area to map */
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Detect secret doors */
            if (square_issecretdoor(context->cave, y, x))
            {
                /* Put an actual door */
                place_closed_door(context->cave, y, x);

                /* Memorize */
                square_memorize(context->origin->player, context->cave, y, x);
                square_light_spot(context->cave, y, x);

                /* Obvious */
                doors = true;

                redraw = true;
            }

            /* Forget unknown doors in the mapping area */
            if (square_isdoor_p(context->origin->player, y, x) &&
                square_isnotknown(context->origin->player, context->cave, y, x))
            {
                square_forget(context->origin->player, y, x);
                square_light_spot(context->cave, y, x);

                redraw = true;
            }
        }
    }

    /* Describe */
    if (doors)
    {
        msg(context->origin->player, "You sense the presence of doors!");
        party_msg_near(context->origin->player, " senses the presence of doors!");
    }
    else if (context->aware)
        msg(context->origin->player, "You sense no doors.");

    /* Redraw minimap */
    if (redraw) context->origin->player->upkeep->redraw |= PR_MAP;

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
    struct source who_body;
    struct source *who = &who_body;

    /* Pick an area to map */
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);
        struct monster_lore *lore;

        /* Skip dead monsters */
        if (!mon->race) continue;

        lore = get_lore(context->origin->player, mon->race);

        /* Skip visible monsters */
        if (monster_is_visible(context->origin->player, i)) continue;

        /* Location */
        y = mon->fy;
        x = mon->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect evil monsters */
        if (monster_is_evil(mon->race))
        {
            struct actor_race *monster_race = &context->origin->player->upkeep->monster_race;

            /* Take note that they are evil */
            rf_on(lore->flags, RF_EVIL);

            /* Update monster recall window */
            if (ACTOR_RACE_EQUAL(monster_race, mon))
                context->origin->player->upkeep->redraw |= (PR_MONSTER);

            /* Increment detection counter */
            source_monster(who, mon);
            give_detect(context->origin->player, who);

            /* Detect */
            monsters = true;
        }
    }

    /* Note effects and clean up */
    if (monsters)
    {
        /* Hack -- fix the monsters */
        update_monsters(context->cave, false);

        /* Full refresh (includes monster/object lists) */
        context->origin->player->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(context->origin->player);

        /* Normal refresh (without monster/object lists) */
        context->origin->player->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(context->origin->player, "You sense the presence of evil creatures!");
        party_msg_near(context->origin->player, " senses the presence of evil creatures!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }
    else if (context->aware)
        msg(context->origin->player, "You sense no evil creatures.");

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
    bool redraw = false;

    /* Pick an area to map */
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

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
                square_memorize(context->origin->player, context->cave, y, x);
                square_light_spot(context->cave, y, x);

                redraw = true;
            }

            /* Forget unknown gold in the mapping area */
            if (square_hasgoldvein_p(context->origin->player, y, x) &&
                square_isnotknown(context->origin->player, context->cave, y, x))
            {
                square_forget(context->origin->player, y, x);
                square_light_spot(context->cave, y, x);

                redraw = true;
            }
        }
    }

    /* Redraw minimap */
    if (redraw) context->origin->player->upkeep->redraw |= PR_MAP;

    context->ident = true;
    return true;
}


/*
 * Detect invisible monsters around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_INVISIBLE_MONSTERS(effect_handler_context_t *context)
{
    detect_monsters_invis(context->origin->player, context->value.dice, context->value.sides, true,
        context->aware);

    context->ident = true;
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
    bool detected_creatures = detect_monsters_normal(context->origin->player, y_dist, x_dist, false,
        context->aware);
    bool detected_invis = detect_monsters_invis(context->origin->player, y_dist, x_dist, false,
        context->aware);

    /* Describe result, and clean up */
    if (detected_creatures || detected_invis)
    {
        /* Hack -- fix the monsters and players */
        update_monsters(context->cave, false);
        update_players();

        /* Full refresh (includes monster/object lists) */
        context->origin->player->full_refresh = true;

        /* Handle Window stuff */
        handle_stuff(context->origin->player);

        /* Normal refresh (without monster/object lists) */
        context->origin->player->full_refresh = false;

        /* Describe, and wait for acknowledgement */
        msg(context->origin->player, "You sense the presence of creatures!");
        party_msg_near(context->origin->player, " senses the presence of creatures!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }

    context->ident = true;
    return true;
}


/*
 * Detect stairs around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_STAIRS(effect_handler_context_t *context)
{
    int x, y;
    int x1, x2, y1, y2;
    int y_dist = context->value.dice;
    int x_dist = context->value.sides;
    bool stairs = false, redraw = false;

    /* Pick an area to map */
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Detect stairs */
            if (square_isstairs(context->cave, y, x))
            {
                /* Memorize */
                square_memorize(context->origin->player, context->cave, y, x);
                square_light_spot(context->cave, y, x);

                /* Obvious */
                stairs = true;

                redraw = true;
            }

            /* Forget unknown stairs in the mapping area */
            if (square_isstairs_p(context->origin->player, y, x) &&
                square_isnotknown(context->origin->player, context->cave, y, x))
            {
                square_forget(context->origin->player, y, x);
                square_light_spot(context->cave, y, x);

                redraw = true;
            }
        }
    }

    /* Describe */
    if (stairs)
    {
        msg(context->origin->player, "You sense the presence of stairs!");
        party_msg_near(context->origin->player, " senses the presence of stairs!");
    }
    else if (context->aware)
        msg(context->origin->player, "You sense no stairs.");

    /* Redraw minimap */
    if (redraw) context->origin->player->upkeep->redraw |= PR_MAP;

    context->ident = true;
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
    bool detect = false, redraw = false;
    struct object *obj;

    /* Pick an area to map */
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

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
                if (square_reveal_trap(context->origin->player, y, x, true, false))
                {
                    /* We found something to detect */
                    detect = true;

                    redraw = true;
                }
            }

            /* Forget unknown traps in the mapping area */
            if (!square_top_trap(context->cave, y, x))
            {
                square_forget_trap(context->origin->player, y, x);

                redraw = true;
            }

            /* Scan all objects in the grid to look for traps on chests */
            for (obj = square_object(context->cave, y, x); obj; obj = obj->next)
            {
                /* Skip anything not a trapped chest */
                if (!is_trapped_chest(obj)) continue;

                /* Identify once */
                if (!object_is_known(context->origin->player, obj))
                {
                    /* Hack -- know the pile */
                    square_know_pile(context->origin->player, context->cave, y, x);

                    /* Know the trap */
                    object_notice_everything_aux(context->origin->player, obj, true, false);

                    /* Notice */
                    if (!ignore_item_ok(context->origin->player, obj))
                    {
                        /* Notice it */
                        disturb(context->origin->player, 0);

                        /* We found something to detect */
                        detect = true;
                    }
                }
            }

            /* Mark as trap-detected */
            sqinfo_on(context->origin->player->cave->squares[y][x].info, SQUARE_DTRAP);
        }
    }

    /* Describe */
    if (detect)
    {
        msg(context->origin->player, "You sense the presence of traps!");
        party_msg_near(context->origin->player, " senses the presence of traps!");
    }

    /* Trap detection always makes you aware, even if no traps are present */
    else
        msg(context->origin->player, "You sense no traps.");

    /* Redraw minimap */
    if (redraw) context->origin->player->upkeep->redraw |= PR_MAP;
    context->origin->player->upkeep->redraw |= PR_DTRAP;

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
    bool gold_buried = false, objects = false;
    bool full = (context->p2? true: false);

    /* Hack -- DM has full detection */
    if (context->origin->player->dm_flags & DM_SEE_LEVEL) full = true;

    /* Pick an area to map */
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

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
                square_memorize(context->origin->player, context->cave, y, x);
                square_light_spot(context->cave, y, x);

                /* Detect */
                gold_buried = true;
            }

            /* Forget unknown gold in the mapping area */
            if (square_hasgoldvein_p(context->origin->player, y, x) &&
                square_isnotknown(context->origin->player, context->cave, y, x))
            {
                square_forget(context->origin->player, y, x);
                square_light_spot(context->cave, y, x);
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
            if (!ignore_item_ok(context->origin->player, obj) || !full) objects = true;

            /* Memorize the pile */
            if (full) square_know_pile(context->origin->player, context->cave, y, x);
            else square_sense_pile(context->origin->player, context->cave, y, x);
            square_light_spot(context->cave, y, x);
        }
    }

    if (gold_buried)
    {
        msg(context->origin->player, "You sense the presence of buried treasure!");
        party_msg_near(context->origin->player, " senses the presence of buried treasure!");
    }
    if (objects)
    {
        msg(context->origin->player, "You sense the presence of objects!");
        party_msg_near(context->origin->player, " senses the presence of objects!");
    }
    if (context->aware && !gold_buried && !objects)
        msg(context->origin->player, "You sense no treasure or objects.");

    /* Redraw minimap, monster list */
    context->origin->player->upkeep->redraw |= (PR_MAP | PR_ITEMLIST);

    context->ident = true;
    return true;
}


/*
 * Detect visible monsters around the player. The height to detect above and below the player
 * is context->value.dice, the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_VISIBLE_MONSTERS(effect_handler_context_t *context)
{
    detect_monsters_normal(context->origin->player, context->value.dice, context->value.sides, true,
        context->aware);

    context->ident = true;
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
        if (context->origin->player->id != mon->master) continue;

        /* Jellies explode with a slowing effect */
        if (match_monster_bases(mon->race->base, "jelly", "mold", NULL))
        {
            struct source who_body;
            struct source *who = &who_body;

            source_monster(who, mon);
            project(who, 2, context->cave, mon->fy, mon->fx, 20, PROJ_MON_SLOW, p_flag, 0, 0,
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
            struct source who_body;
            struct source *who = &who_body;

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
            source_monster(who, mon);
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
    for (i = 0; i < context->origin->player->body.count; i++)
    {
        /* Ignore rings, amulets and lights (and tools) */
        if (slot_type_is(context->origin->player, i, EQUIP_RING)) continue;
        if (slot_type_is(context->origin->player, i, EQUIP_AMULET)) continue;
        if (slot_type_is(context->origin->player, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(context->origin->player, i, EQUIP_TOOL)) continue;

        /* Count disenchantable slots */
        count++;
    }

    /* Pick one at random */
    for (i = context->origin->player->body.count - 1; i >= 0; i--)
    {
        /* Ignore rings, amulets and lights (and tools) */
        if (slot_type_is(context->origin->player, i, EQUIP_RING)) continue;
        if (slot_type_is(context->origin->player, i, EQUIP_AMULET)) continue;
        if (slot_type_is(context->origin->player, i, EQUIP_LIGHT)) continue;
        if (slot_type_is(context->origin->player, i, EQUIP_TOOL)) continue;

        if (one_in_(count--)) break;
    }

    /* Notice */
    context->ident = true;

    /* Get the item */
    obj = slot_object(context->origin->player, i);

    /* No item, nothing happens */
    if (!obj) return true;

    /* Nothing to disenchant */
    if ((obj->to_h <= 0) && (obj->to_d <= 0) && (obj->to_a <= 0))
        return true;

    /* Describe the object */
    object_desc(context->origin->player, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Artifacts have 60% chance to resist */
    if (obj->artifact && magik(60))
    {
        /* Message */
        msg(context->origin->player, "Your %s (%c) resist%s disenchantment!", o_name, I2A(i),
        SINGULAR(obj->number));

        return true;
    }

    /* Apply disenchantment, depending on which kind of equipment */
    if (slot_type_is(context->origin->player, i, EQUIP_WEAPON) ||
        slot_type_is(context->origin->player, i, EQUIP_BOW))
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
    msg(context->origin->player, "Your %s (%c) %s disenchanted!", o_name, I2A(i),
        ((obj->number != 1)? "were": "was"));

    /* Recalculate bonuses */
    context->origin->player->upkeep->update |= (PU_BONUS);

    /* Redraw */
    context->origin->player->upkeep->redraw |= (PR_EQUIP);

    return true;
}


/*
 * Drain some light from the player's light source, if possible
 */
static bool effect_handler_DRAIN_LIGHT(effect_handler_context_t *context)
{
    int drain = effect_calculate_value(context, false);
    int light_slot = slot_by_name(context->origin->player, "light");
    struct object *obj = slot_object(context->origin->player, light_slot);

    if (obj && !of_has(obj->flags, OF_NO_FUEL) && (obj->timeout > 0))
    {
        /* Reduce fuel */
        obj->timeout -= drain;
        if (obj->timeout < 1) obj->timeout = 1;

        /* Notice */
        if (!context->origin->player->timed[TMD_BLIND])
        {
            msg(context->origin->player, "Your light dims.");
            context->ident = true;
        }

        /* Redraw */
        context->origin->player->upkeep->redraw |= (PR_EQUIP);
    }

    return true;
}


/*
 * Drain mana from the player, healing the caster.
 */
static bool effect_handler_DRAIN_MANA(effect_handler_context_t *context)
{
    int drain = effect_calculate_value(context, false);
    struct source who_body;
    struct source *who = &who_body;
    struct monster *mon = context->origin->monster;
    bool seen = false;

    if (mon)
    {
        seen = (!context->origin->player->timed[TMD_BLIND] &&
            monster_is_visible(context->origin->player, mon->midx));
        source_monster(who, mon);

        /* MvM */
        if (context->target_m_ptr)
        {
            char m_name[NORMAL_WID];

            /* Affects only casters */
            if (!context->target_m_ptr->race->freq_spell) return true;

            monster_desc(context->origin->player, m_name, sizeof(m_name), mon, MDESC_STANDARD);

            /* Attack power, capped vs monster level */
            if (drain > (context->target_m_ptr->level / 6) + 1)
                drain = (context->target_m_ptr->level / 6) + 1;

            /* Heal the monster */
            if (mon->hp < mon->maxhp)
            {
                /* Heal */
                mon->hp += (6 * drain);
                if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;

                /* Redraw (later) if needed */
                update_health(context->origin);

                /* Special message */
                if (seen) msg(context->origin->player, "%s appears healthier.", m_name);
            }

            return true;
        }

        if (resist_undead_attacks(context->origin->player, mon->race))
        {
            msg(context->origin->player, "You resist the effects!");
            return true;
        }
    }
    else
        source_trap(who, context->origin->trap);

    drain_mana(context->origin->player, who, drain, seen);

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

    /* Notice */
    context->ident = true;

    /* Notice effect */
    equip_learn_flag(context->origin->player, flag);

    /* Sustain */
    if (player_of_has(context->origin->player, flag))
    {
        /* Message */
        msg(context->origin->player, "You feel very %s for a moment, but the feeling passes.",
            desc_stat(stat, false));

        return true;
    }

    /* Attempt to reduce the stat */
    if (player_stat_dec(context->origin->player, stat, false))
    {
        /* Message */
        msgt(context->origin->player, MSG_DRAIN_STAT, "You feel very %s.", desc_stat(stat, false));
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
 * context->origin->monster is set, quake the area silently around the monster
 */
static bool effect_handler_EARTHQUAKE(effect_handler_context_t *context)
{
    int r = effect_calculate_value(context, false);
    int i, y, x, yy, xx, dy, dx, j;
    int damage = 0;
    int safe_grids = 0, safe_y = 0, safe_x = 0;
    int hurt[MAX_PLAYERS];
    bool map[32][32];
    struct loc centre;
    int count = 0;

    if (context->p2) r = context->p2;
    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        if (!context->origin->monster)
            msg(context->origin->player, "The ground shakes for a moment.");
        return true;
    }

    /* Determine the epicentre */
    origin_get_loc(&centre, context->origin);

    if (!context->origin->monster)
        msg_misc(context->origin->player, " causes the ground to shake!");

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
            yy = centre.y + dy;
            xx = centre.x + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, yy, xx)) continue;

            /* Skip distant grids */
            if (distance(centre.y, centre.x, yy, xx) > r) continue;

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

            /* Forget completely */
            square_unglow(context->cave, yy, xx);
            square_forget_all(context->cave, yy, xx);
            square_light_spot(context->cave, yy, xx);

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
            if (!square_isempty(context->cave, y, x)) continue;

            /* Important -- skip "quake" grids */
            if (map[16 + y - centre.y][16 + x - centre.x]) continue;

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
            take_hit(player, damage, "an earthquake", false, "was crushed by tons of falling rocks");
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = centre.y + dy;
            xx = centre.x + dx;

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
                            if (!square_isempty(context->cave, y, x)) continue;

                            /* No safety on glyph of warding */
                            if (square_iswarded(context->cave, y, x)) continue;

                            /* Important -- skip "quake" grids */
                            if (map[16 + y - centre.y][16 + x - centre.x]) continue;

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

                        /* Scream in pain */
                        add_monster_message(player, mon, MON_MSG_WAIL, true);
                    }

                    /* Take damage from the quake */
                    damage = (safe_grids? damroll(4, 8): (mon->hp + 1));

                    /* Monster is certainly awake */
                    mon_clear_timed(context->origin->player, mon, MON_TMD_SLEEP,
                        MON_TMD_FLG_NOMESSAGE);
                    mon_clear_timed(context->origin->player, mon, MON_TMD_HOLD,
                        MON_TMD_FLG_NOTIFY);

                    /* If the quake finished the monster off, show message */
                    if ((mon->hp < damage) && (mon->hp >= 0))
                    {
                        /* Give players a message */
                        for (j = 0; j < count; j++)
                        {
                            /* Get player */
                            struct player *player = player_get(abs(hurt[j]));

                            /* Message */
                            add_monster_message(player, mon, MON_MSG_EMBEDDED, true);
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

        map[16 + player->py - centre.y][16 + player->px - centre.x] = false;
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = centre.y + dy;
            xx = centre.x + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, yy, xx)) continue;

            /* Note unaffected grids for light changes, etc. */
            if (!map[16 + dy][16 + dx]) square_light_spot(context->cave, yy, xx);

            /* Destroy location and all objects (if valid) */
            else if (square_changeable(context->cave, yy, xx))
            {
                square_forget_pile_all(context->cave, yy, xx);
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
        player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

        /* Redraw */
        player->upkeep->redraw |= (PR_HEALTH | PR_MONLIST | PR_ITEMLIST);
    }

    return true;
}


static bool effect_handler_ELEM_BRAND(effect_handler_context_t *context)
{
    int tries = effect_calculate_value(context, false);
    struct object *obj = equipped_item_by_slot_name(context->origin->player, "weapon");
    int i;
    const char *act;
    int brand;
    bool chosen[5];

    memset(chosen, 0, 5 * sizeof(bool));

    /* You can never modify artifacts, ego items or worthless items */
    if (obj && obj->kind->cost && !obj->artifact && !obj->ego)
    {
        char o_name[NORMAL_WID];

        object_desc(context->origin->player, o_name, sizeof(o_name), obj, ODESC_BASE);

        /* Make it an ego item */
        obj->ego = ego_elemental();
        ego_apply_magic(obj, 0);

        /* Add some brands */
        for (i = 0; i < tries; i++)
        {
            int what = randint0(5);

            /* Select a brand */
            switch (what)
            {
                case 0: act = "flames"; brand = get_brand("fire", 3); break;
                case 1: act = "frost"; brand = get_brand("cold", 3); break;
                case 2: act = "lightning"; brand = get_brand("lightning", 3); break;
                case 3: act = "acid"; brand = get_brand("acid", 3); break;
                case 4: act = "venom"; brand = get_brand("poison", 3); break;
            }

            /* Check brand */
            if (chosen[what]) continue;
            chosen[what] = true;

            /* Describe */
            msg(context->origin->player, "The %s %s surrounded with an aura of %s.", o_name,
                ((obj->number > 1)? "are": "is"), act);

            /* Brand the object */
            append_brand(&obj->brands, brand);
        }

        object_notice_ego(context->origin->player, obj);

        /* Enchant */
        enchant(context->origin->player, obj, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Endless source of cash? No way... make them worthless */
        set_origin(obj, ORIGIN_WORTHLESS, context->origin->player->wpos.depth, NULL);
        if (object_was_sensed(obj) || object_is_known(context->origin->player, obj))
            context->origin->player->upkeep->notice |= PN_IGNORE;
    }
    else
        msg(context->origin->player, "The branding failed.");

    return true;
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
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, ((context->p2 == ENCH_TOAC)? HOOK_ARMOR: HOOK_WEAPON), "");
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->origin->player, obj) && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Assume enchant weapon */
    item_tester = item_tester_hook_weapon;

    /* Enchant armor if requested */
    if (context->p2 == ENCH_TOAC) item_tester = item_tester_hook_armour;

    /* Paranoia: requires proper item */
    if (!item_tester(obj)) return false;

    /* Description */
    object_desc(context->origin->player, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Describe */
    msg(context->origin->player, "%s %s glow%s brightly!",
        (object_is_carried(context->origin->player, obj) ? "Your" : "The"), o_name,
        SINGULAR(obj->number));

    /* Enchant */
    if (!enchant(context->origin->player, obj, value, context->p2))
    {
        /* Failure */
        msg(context->origin->player, "The enchantment failed.");
    }

    /* Endless source of cash? No way... make them worthless */
    else if (!context->p3)
    {
        set_origin(obj, ORIGIN_WORTHLESS, context->origin->player->wpos.depth, NULL);
        if (object_was_sensed(obj) || object_is_known(context->origin->player, obj))
            context->origin->player->upkeep->notice |= PN_IGNORE;
    }

    /* Redraw */
    if (!object_is_carried(context->origin->player, obj))
        redraw_floor(&context->origin->player->wpos, obj->iy, obj->ix);

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


static bool effect_handler_LIGHT_LEVEL(effect_handler_context_t *context)
{
    bool full = (context->p2? true: false);

    if (full)
        msg(context->origin->player, "An image of your surroundings forms in your mind...");
    wiz_light(context->origin->player, context->cave, full);
    context->ident = true;
    return true;
}


static bool effect_handler_GAIN_EXP(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->origin->player->exp < PY_MAX_EXP)
    {
        s32b ee = (context->origin->player->exp / 2) + 10;

        if (ee > amount) ee = amount;
        msg(context->origin->player, "You feel more experienced.");
        player_exp_gain(context->origin->player, context->p1? ee: amount);
    }
    context->ident = true;

    return true;
}


/*
 * Gain a stat point. The stat index is context->p1.
 */
static bool effect_handler_GAIN_STAT(effect_handler_context_t *context)
{
    int stat = context->p1;

    /* Attempt to increase */
    if (player_stat_inc(context->origin->player, stat))
    {
        /* Message */
        msg(context->origin->player, "You feel very %s!", desc_stat(stat, true));
    }

    /* Notice */
    context->ident = true;

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

    /* Always ID */
    context->ident = true;

    /* No healing needed */
    if (context->origin->player->chp >= context->origin->player->mhp) return true;

    /* Figure healing level */
    num = ((context->origin->player->mhp - context->origin->player->chp) *
        context->value.m_bonus) / 100;

    /* PWMAngband: Cell Adjustment heals a variable amount of hps */
    amount = context->value.base + damroll(context->value.dice, context->value.sides);

    /* Enforce minimums */
    if (num < amount) num = amount;

    if (context->self_msg) msg(context->origin->player, context->self_msg);
    hp_player(context->origin->player, num);
    return true;
}


/*
 * Identify an unknown rune of an item.
 */
static bool effect_handler_IDENTIFY(effect_handler_context_t *context)
{
    struct object *obj;

    context->ident = true;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_IDENTIFY, "");
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->origin->player, obj) && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires identifiable item */
    if (object_runes_known(obj)) return false;

    /* Identify the object */
    object_learn_unknown_rune(context->origin->player, obj);
    if (!object_is_carried(context->origin->player, obj))
        redraw_floor(&context->origin->player->wpos, obj->iy, obj->ix);

    /* Something happened */
    return true;
}


/*
 * Call light around the player
 * Affect all monsters in the projection radius (context->p2)
 */
static bool effect_handler_LIGHT_AREA(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;
    int dam = effect_calculate_value(context, false);
    int rad = context->p2 + (context->p3? (context->origin->player->lev / context->p3): 0);
    int flg = PROJECT_GRID;
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(context->origin->player->conn)),
        context->origin->player);

    /* Hack -- elementalists */
    if (context->spell_power)
    {
        rad = dam;
        dam = 0;
    }

    /* Hurt monsters/players in the projection radius */
    if (dam > 0) flg |= (PROJECT_KILL | PROJECT_PLAY);

    /* Message */
    if (!context->origin->player->timed[TMD_BLIND])
        msg(context->origin->player, "You are surrounded by a white light.");

    /* Hook into the "project()" function */
    context->origin->player->current_sound = -2;
    project(who, rad, context->cave, py, px, dam, PROJ_LIGHT_WEAK, flg, 0, 0, "killed");
    context->origin->player->current_sound = -1;

    /* Light up the room */
    light_room(context->origin->player, context->cave, py, px, true);

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

    if (context->self_msg && !context->origin->player->timed[TMD_BLIND])
        msg(context->origin->player, context->self_msg);
    for (y = 0; y < num; y++)
    {
        if (light_line_aux(context->origin, context->dir, context->p1, dam))
            context->ident = true;
    }
    return true;
}


static bool effect_handler_LOSE_EXP(effect_handler_context_t *context)
{
    if (!player_of_has(context->origin->player, OF_HOLD_LIFE) && context->origin->player->exp)
    {
        msg(context->origin->player, "You feel your memories fade.");
        player_exp_lose(context->origin->player, context->origin->player->exp / 4, false);
    }
    context->ident = true;
    equip_learn_flag(context->origin->player, OF_HOLD_LIFE);
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
    if (player_stat_dec(context->origin->player, loss_stat, true))
    {
        /* Message */
        msgt(context->origin->player, MSG_DRAIN_STAT, "You feel very %s.",
            desc_stat(loss_stat, false));
    }

    /* Notice */
    context->ident = true;

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
    y1 = context->origin->player->py - y_dist;
    y2 = context->origin->player->py + y_dist;
    x1 = context->origin->player->px - x_dist;
    x2 = context->origin->player->px + x_dist;

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
                    square_memorize(context->origin->player, context->cave, y, x);
                    square_mark(context->origin->player, y, x);
                }

                /* Memorize known walls */
                for (i = 0; i < 8; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Memorize walls (etc), mark grids as processed */
                    if (square_seemslikewall(context->cave, yy, xx))
                    {
                        square_memorize(context->origin->player, context->cave, yy, xx);
                        square_mark(context->origin->player, yy, xx);
                    }
                }
            }

            /* Forget unprocessed, unknown grids in the mapping area */
            if (!square_ismark(context->origin->player, y, x) &&
                square_isnotknown(context->origin->player, context->cave, y, x))
            {
                square_forget(context->origin->player, y, x);
            }
        }
    }

    /* Unmark grids */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            if (!square_in_bounds(context->cave, y, x)) continue;
            square_unmark(context->origin->player, y, x);
        }
    }

    /* Fully update the visuals */
    context->origin->player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw minimap, monster list, item list */
    context->origin->player->upkeep->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

    /* Notice */
    context->ident = true;

    return true;
}


/*
 * Reveals the location of a random wilderness area.
 */
static bool effect_handler_MAP_WILD(effect_handler_context_t *context)
{
    int x, y, xx, yy;
    struct worldpos wpos;
    char buf[NORMAL_WID];

    int max_radius = radius_wild - 1;

    /* Default to magic map if no wilderness */
    if (cfg_diving_mode || OPT(context->origin->player, birth_no_recall))
    {
        effect_handler_MAP_AREA(context);
        return true;
    }

    /* Pick an area to map */
    y = randint0(2 * max_radius + 1) - max_radius;
    x = randint0(2 * max_radius + 1) - max_radius;

    /* Update the wilderness map around that area */
    for (yy = y - 1; yy <= y + 1; yy++)
    {
        for (xx = x - 1; xx <= x + 1; xx++)
        {
            COORDS_SET(&wpos, yy, xx, 0);
            wild_set_explored(context->origin->player, &wpos);
        }
    }
    wild_cat_depth(&wpos, buf, sizeof(buf));
    msg(context->origin->player, "You suddenly know more about the area around %s.", buf);

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
    const char *pself = player_self(context->origin->player);
    char df[160];

    context->ident = true;

    /* Delete the (nearby) monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);
        int d;

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip unique monsters */
        if (monster_is_unique(mon->race)) continue;

        /* Skip distant monsters */
        d = distance(context->origin->player->py, context->origin->player->px, mon->fy, mon->fx);
        if (d > radius) continue;

        /* Delete the monster */
        delete_monster_idx(context->cave, i);

        /* Take some damage */
        dam += randint1(3);
    }

    /* Hurt the player */
    strnfmt(df, sizeof(df), "exhausted %s with Mass Banishment", pself);
    take_hit(context->origin->player, dam, "the strain of casting Mass Banishment", false, df);

    /* Calculate result */
    result = (dam > 0)? true: false;

    /* Redraw */
    if (result) context->origin->player->upkeep->redraw |= (PR_MONLIST);

    return true;
}


static bool effect_handler_MIND_VISION(effect_handler_context_t *context)
{
    struct player *q = get_inscribed_player(context->origin->player, context->note);

    if (!q) return true;

    if (context->origin->player == q)
    {
        msg(context->origin->player, "You cannot link to your own mind.");
        return false;
    }
    if (context->origin->player->esp_link)
    {
        msg(context->origin->player, "Your mind is already linked.");
        return false;
    }
    if (q->esp_link)
    {
        msg(context->origin->player, "%s's mind is already linked.", q->name);
        return false;
    }

    /* Not if hostile */
    if (pvp_check(context->origin->player, q, PVP_CHECK_ONE, true, 0x00))
    {
        /* Message */
        msg(context->origin->player, "%s's mind is not receptive.", q->name);
        return false;
    }

    msg(q, "%s infiltrates your mind.", context->origin->player->name);
    msg(context->origin->player, "You infiltrate %s's mind.", q->name);
    context->origin->player->esp_link = q->id;
    context->origin->player->esp_link_type = LINK_DOMINANT;

    q->esp_link = context->origin->player->id;
    q->esp_link_type = LINK_DOMINATED;
    q->upkeep->redraw |= PR_MAP;

    return true;
}


static void heal_monster(struct player *p, struct monster *mon, struct source *origin, int amount)
{
    char m_name[NORMAL_WID], m_poss[NORMAL_WID];
    bool seen;

    /* Get the monster name (or "it") */
    monster_desc(p, m_name, sizeof(m_name), mon, MDESC_STANDARD);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(p, m_poss, sizeof(m_poss), mon, MDESC_PRO_VIS | MDESC_POSS);

    seen = (!p->timed[TMD_BLIND] && monster_is_visible(p, mon->midx));

    /* Heal some */
    mon->hp += amount;

    /* Fully healed */
    if (mon->hp >= mon->maxhp)
    {
        mon->hp = mon->maxhp;

        if (seen)
            msg(p, "%s looks REALLY healthy!", m_name);
        else
            msg(p, "%s sounds REALLY healthy!", m_name);
    }

    /* Partially healed */
    else if (seen)
        msg(p, "%s looks healthier.", m_name);
    else
        msg(p, "%s sounds healthier.", m_name);

    /* Redraw (later) if needed */
    update_health(origin);

    /* Cancel fear */
    if (mon->m_timed[MON_TMD_FEAR])
    {
        mon_clear_timed(p, mon, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE);
        msg(p, "%s recovers %s courage.", m_name, m_poss);
    }

    /* Cancel poison */
    if (mon->m_timed[MON_TMD_POIS])
    {
        mon_clear_timed(p, mon, MON_TMD_POIS, MON_TMD_FLG_NOMESSAGE);
        msg(p, "%s is no longer poisoned.", m_name);
    }  

    /* Cancel bleeding */
    if (mon->m_timed[MON_TMD_CUT])
    {
        mon_clear_timed(p, mon, MON_TMD_CUT, MON_TMD_FLG_NOMESSAGE);
        msg(p, "%s is no longer bleeding.", m_name);
    }
}


/*
 * Monster self-healing.
 */
static bool effect_handler_MON_HEAL_HP(effect_handler_context_t *context)
{
    struct monster *mon = context->origin->monster;
    int amount = effect_calculate_value(context, false);

    if (!mon) return true;

    /* No stupid message when at full health */
    if (mon->hp == mon->maxhp) return true;

    heal_monster(context->origin->player, mon, context->origin, amount);

    /* ID */
    context->ident = true;

    return true;
}


/*
 * Monster healing of kin.
 */
static bool effect_handler_MON_HEAL_KIN(effect_handler_context_t *context)
{
    struct monster *mon = context->origin->monster;
    int amount = effect_calculate_value(context, false);

    if (!mon) return true;

    /* Find a nearby monster */
    mon = choose_nearby_injured_kin(context->cave, mon);
    if (!mon) return true;

    /* No stupid message when at full health */
    if (mon->hp == mon->maxhp) return true;

    heal_monster(context->origin->player, mon, context->origin, amount);

    /* ID */
    context->ident = true;

    return true;
}


/*
 * Extend a (positive or negative) monster status condition.
 */
static bool effect_handler_MON_TIMED_INC(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (!context->origin->monster) return true;

    mon_inc_timed(context->origin->player, context->origin->monster, context->p1, amount, 0);
    context->ident = true;
    return true;
}


/*
 * Feed the player.
 */
static bool effect_handler_NOURISH(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->self_msg && !player_undead(context->origin->player))
        msg(context->origin->player, context->self_msg);
    player_set_food(context->origin->player, context->origin->player->food + amount);
    context->ident = true;
    return true;
}


static bool effect_handler_POLY_RACE(effect_handler_context_t *context)
{
    struct monster_race *race = &r_info[context->boost];

    context->ident = true;

    /* Restrict */
    if (context->origin->player->ghost || player_has(context->origin->player, PF_DRAGON) ||
        OPT(context->origin->player, birth_fruit_bat) ||
        (context->origin->player->poly_race == race))
    {
        msg(context->origin->player, "Nothing happens.");
        return false;
    }

    /* Restrict if too powerful */
    if (context->origin->player->lev < race->level / 2)
    {
        msg(context->origin->player, "Nothing happens.");
        return false;
    }

    /* Useless ring */
    if (race->ridx == 0)
    {
        msg(context->origin->player, "Nothing happens.");
        return false;
    }

    /* Non-Shapechangers get a huge penalty for using rings of polymorphing */
    if (!player_has(context->origin->player, PF_MONSTER_SPELLS))
    {
        const char *pself = player_self(context->origin->player);
        char df[160];

        msg(context->origin->player, "Your nerves and muscles feel weak and lifeless!");
        strnfmt(df, sizeof(df), "exhausted %s with polymorphing", pself);
        take_hit(context->origin->player, damroll(10, 10), "the strain of polymorphing", false, df);
        player_stat_dec(context->origin->player, STAT_DEX, true);
        player_stat_dec(context->origin->player, STAT_WIS, true);
        player_stat_dec(context->origin->player, STAT_CON, true);
        player_stat_dec(context->origin->player, STAT_STR, true);
        player_stat_dec(context->origin->player, STAT_INT, true);

        /* Fail if too powerful */
        if (magik(race->level)) return true;
    }

    do_cmd_poly(context->origin->player, race, false, true);

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
        d = distance(context->origin->player->py, context->origin->player->px, mon->fy, mon->fx);
        if (d > z_info->max_sight)
            continue;

        /* Probe visible monsters */
        if (monster_is_visible(context->origin->player, i))
        {
            char m_name[NORMAL_WID];
            char buf[NORMAL_WID];
            int j;

            /* Start the message */
            if (!probe) msg(context->origin->player, "Probing...");

            /* Get "the monster" or "something" */
            monster_desc(context->origin->player, m_name, sizeof(m_name), mon,
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
            msg(context->origin->player, "%s (%d) has %d hp, %d ac, %d speed.", m_name, mon->level,
                mon->hp, mon->ac, mon->mspeed);
            if (blows)
                msg(context->origin->player, "%s (%d) %s.", m_name, mon->level, buf);

            /* Learn all of the non-spell, non-treasure flags */
            lore_do_probe(context->origin->player, mon);

            /* Probe worked */
            probe = true;
        }
    }

    /* Done */
    if (probe)
    {
        msg(context->origin->player, "That's all.");
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
    struct source who_body;
    struct source *who = &who_body;

    if (!context->origin->monster) return true;

    /* MvM only */
    if (!context->target_m_ptr) return true;

    source_monster(who, context->origin->monster);
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

    project_los(context->origin->player, typ, dam, false);
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

    if (project_los(context->origin->player, typ, dam, context->aware) && context->self_msg)
        msg(context->origin->player, context->self_msg);
    context->ident = true;
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
 * Set word of recall as appropriate.
 *
 * PWMAngband: context->value gives the delay.
 */
static bool effect_handler_RECALL(effect_handler_context_t *context)
{
    int delay = effect_calculate_value(context, false);

    context->ident = true;

    /* No recall */
    if (((cfg_diving_mode == 2) || OPT(context->origin->player, birth_no_recall)) &&
        !context->origin->player->total_winner)
    {
        msg(context->origin->player, "Nothing happens.");
        return false;
    }

    /* No recall from quest levels with force_descend while the quest is active */
    if (((cfg_limit_stairs == 3) || OPT(context->origin->player, birth_force_descend)) &&
        is_quest_active(context->origin->player, context->origin->player->wpos.depth))
    {
        msg(context->origin->player, "Nothing happens.");
        return false;
    }

    /* Activate recall */
    if (!context->origin->player->word_recall)
    {
        /* Select the recall depth */
        set_recall_depth(context->origin->player, context->note);

        /* Warn the player if they're descending to an unrecallable level */
        if (((cfg_limit_stairs == 3) || OPT(context->origin->player, birth_force_descend)) &&
            surface_of_dungeon(&context->origin->player->wpos) &&
            is_quest_active(context->origin->player, context->origin->player->recall_wpos.depth) &&
            (context->origin->player->current_value == ITEM_REQUEST))
        {
            get_item(context->origin->player, HOOK_DOWN, "");
            return false;
        }

        /* Activate recall */
        context->origin->player->word_recall = delay;
        msg(context->origin->player, "The air around you becomes charged...");
        msg_misc(context->origin->player, " is surrounded by a charged aura...");

        /* Redraw the state (later) */
        context->origin->player->upkeep->redraw |= (PR_STATE);
    }

    /* Deactivate recall */
    else
    {
        /* Ask for confirmation */
        if (context->origin->player->current_value == ITEM_REQUEST)
        {
            get_item(context->origin->player, HOOK_CONFIRM, "");
            return false;
        }

        /* Deactivate recall */
        context->origin->player->word_recall = 0;
        msg(context->origin->player, "A tension leaves the air around you...");
        msg_misc(context->origin->player, "'s charged aura disappears...");

        /* Redraw the state (later) */
        context->origin->player->upkeep->redraw |= (PR_STATE);
    }

    return true;
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
    int strength = effect_calculate_value(context, false);
    struct object *obj;
    bool carried;
    int y, x;

    /* Immediately obvious */
    context->ident = true;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_RECHARGE, "");
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Save object info (backfire may destroy it) */
    carried = object_is_carried(context->origin->player, obj);
    y = obj->iy;
    x = obj->ix;

    /* Restricted by choice */
    if (!carried && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
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
        msg(context->origin->player, "The recharge backfires!");
        msg(context->origin->player, "There is a bright flash of light.");

        /* Safe recharge: drain all charges */
        if (cfg_safe_recharge)
            obj->pval = 0;

        /* Normal recharge: destroy one item */
        else
        {
            /* Reduce and describe inventory */
            use_object(context->origin->player, obj, 1, true);
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
    context->origin->player->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    context->origin->player->upkeep->redraw |= (PR_INVEN);
    if (!carried) redraw_floor(&context->origin->player->wpos, y, x);

    /* Something was done */
    return true;
}


/*
 * Attempt to uncurse an object
 */
static bool effect_handler_REMOVE_CURSE(effect_handler_context_t *context)
{
    int strength = effect_calculate_value(context, false);
    struct object *obj;
    char dice_string[20];

    context->ident = true;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        /* Get the dice string */
        strnfmt(dice_string, sizeof(dice_string), "%d+d%d", context->value.base,
            context->value.sides);

        get_item(context->origin->player, HOOK_UNCURSE, dice_string);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->origin->player, obj) && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Paranoia: requires uncursable item */
    if (!item_tester_uncursable(obj)) return false;

    return uncurse_object(context->origin->player, obj, strength);
}


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
        if (context->origin->player->id != mon->master) continue;

        /* Acquire the monster name */
        monster_desc(context->origin->player, m_name, sizeof(m_name), mon, MDESC_STANDARD);

        seen = (!context->origin->player->timed[TMD_BLIND] &&
            monster_is_visible(context->origin->player, mon->midx));

        /* Skip already resilient slaves */
        if (mon->resilient)
        {
            if (seen) msg(context->origin->player, "%s is unaffected.", m_name);
        }

        /* Double the lifespan (cap the value depending on monster level) */
        else
        {
            mon->lifespan = mon->level * 2 + 20;
            mon->resilient = 1;
            if (seen) msg(context->origin->player, "%s looks more resilient.", m_name);
        }
    }

    return true;
}


/*
 * Restores any drained experience
 */
static bool effect_handler_RESTORE_EXP(effect_handler_context_t *context)
{
    if (context->self_msg && !player_undead(context->origin->player))
        msg(context->origin->player, context->self_msg);

    /* Restore experience */
    if (context->origin->player->exp < context->origin->player->max_exp)
    {
        /* Message */
        msg(context->origin->player, "You feel your life energies returning.");

        /* Restore the experience */
        player_exp_gain(context->origin->player,
            context->origin->player->max_exp - context->origin->player->exp);
    }

    /* Did something */
    context->ident = true;

    return true;
}


static bool effect_handler_RESTORE_MANA(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (!amount) amount = context->origin->player->msp;

    /* Healing needed */
    if (context->origin->player->csp < context->origin->player->msp)
    {
        int old_num = get_player_num(context->origin->player);

        /* Gain mana */
        context->origin->player->csp += amount;

        /* Enforce maximum */
        if (context->origin->player->csp >= context->origin->player->msp)
        {
            context->origin->player->csp = context->origin->player->msp;
            context->origin->player->csp_frac = 0;
        }

        /* Hack -- redraw picture */
        redraw_picture(context->origin->player, old_num);

        /* Redraw */
        context->origin->player->upkeep->redraw |= (PR_MANA);

        /* Print a nice message */
        msg(context->origin->player, "You feel your head clear.");
    }

    /* Notice */
    context->ident = true;

    return true;
}


/*
 * Restore a stat. The stat index is context->p1.
 */
static bool effect_handler_RESTORE_STAT(effect_handler_context_t *context)
{
    int stat = context->p1;

    /* Success */
    context->ident = true;

    /* Check bounds */
    if ((stat < 0) || (stat >= STAT_MAX)) return true;

    /* Not needed */
    if (context->origin->player->stat_cur[stat] == context->origin->player->stat_max[stat])
        return true;

    /* Restore */
    context->origin->player->stat_cur[stat] = context->origin->player->stat_max[stat];

    /* Recalculate bonuses */
    context->origin->player->upkeep->update |= (PU_BONUS);

    /* Message */
    msg(context->origin->player, "You feel less %s.", desc_stat(stat, false));

    return true;
}


/*
 * Try to resurrect someone
 */
static bool effect_handler_RESURRECT(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;
    int x, y;

    context->ident = true;

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
                    return true;
                }
            }
        }
    }

    /* We did not resurrect anyone */
    return true;
}


/*
 * The rubble effect
 *
 * This causes rubble to fall into empty squares.
 */
static bool effect_handler_RUBBLE(effect_handler_context_t *context)
{
    /*
     * First we work out how many grids we want to fill with rubble. Then we
     * check that we can actually do this, by counting the number of grids
     * available, limiting the number of rubble grids to this number if
     * necessary.
     */
    int rubble_grids = randint1(3);
    int open_grids = count_feats(context->origin->player, context->cave, NULL, NULL, square_isempty,
        false);

    /* Avoid infinite loops */
    int iterations = 0;

    if (rubble_grids > open_grids) rubble_grids = open_grids;

    while ((rubble_grids > 0) && (iterations < 10))
    {
        int d;

        /* Look around the player */
        for (d = 0; d < 9; d++)
        {
            int yy, xx;

            /* Ignore the player's location */
            if (d == 8) continue;

            /* Extract adjacent (legal) location */
            yy = context->origin->player->py + ddy_ddd[d];
            xx = context->origin->player->px + ddx_ddd[d];

            if (square_in_bounds_fully(context->cave, yy, xx) &&
                square_isempty(context->cave, yy, xx) && one_in_(3))
            {
                if (one_in_(2))
                    square_set_feat(context->cave, yy, xx, FEAT_PASS_RUBBLE);
                else
                    square_set_feat(context->cave, yy, xx, FEAT_RUBBLE);
                rubble_grids--;
            }
        }

        iterations++;
    }

    context->ident = true;

    /* Fully update the visuals */
    context->origin->player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw monster list */
    context->origin->player->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);

    return true;
}


/*
 * Create a "glyph of warding".
 */
static bool effect_handler_RUNE(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;

    /* Hack -- already used up */
    bool used = (context->p2 == 1);

    /* Always notice */
    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->cave->wpos))
    {
        msg(context->origin->player, "You cannot create glyphs here...");
        return false;
    }

    /* Require clean space */
    if (!square_canward(context->cave, py, px))
    {
        msg(context->origin->player, "There is no clear floor on which to cast the spell.");
        return false;
    }

    /* Push objects off the grid */
    if (square_object(context->cave, py, px))
        push_object(context->origin->player, context->cave, py, px);

    /* Create a glyph of warding */
    square_add_ward(context->cave, py, px);
    msg_misc(context->origin->player, " lays down a glyph of protection.");

    return !used;
}


static bool effect_handler_SAFE_GUARD(effect_handler_context_t *context)
{
    int x, y, rad = 2 + (context->origin->player->lev / 20);

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create glyphs here...");
        return false;
    }

    msg_misc(context->origin->player, " lays down some glyphs of protection.");

    for (x = context->origin->player->px - rad; x <= context->origin->player->px + rad; x++)
    {
        for (y = context->origin->player->py - rad; y <= context->origin->player->py + rad; y++)
        {
            /* First we must be in the dungeon */
            if (!square_in_bounds_fully(context->cave, y, x)) continue;

            /* Is it a naked grid? */
            if (!square_isempty(context->cave, y, x)) continue;

            /* Now we want a circle */
            if (distance(y, x, context->origin->player->py, context->origin->player->px) != rad)
                continue;

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

    if (context->self_msg) msg(context->origin->player, context->self_msg);
    player_set_food(context->origin->player, amount);
    context->ident = true;
    return true;
}


/*
 * Project from the player's grid at the player, act as a ball
 * Affect the player, grids, objects, and monsters
 */
static bool effect_handler_SPOT(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;
    int dam = effect_calculate_value(context, false);
    int rad = context->p2;
    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    bool result;
    struct source who_body;
    struct source *who = &who_body;

    source_trap(who, context->origin->trap);

    /* Aim at the target, explode */
    context->origin->player->current_sound = -2;
    result = project(who, rad, chunk_get(&context->origin->player->wpos), py, px, dam, context->p1,
        flg, 0, 0, "annihilated");
    context->origin->player->current_sound = -1;
    if (result) context->ident = true;

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

    if (context->self_msg && !context->origin->player->timed[TMD_BLIND])
        msg(context->origin->player, context->self_msg);
    for (i = 0; i < 8; i++)
        light_line_aux(context->origin, ddd[i], context->p1, dam);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;
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

    if (context->self_msg && !context->origin->player->timed[TMD_BLIND])
        msg(context->origin->player, context->self_msg);
    for (i = 0; i < 8; i++)
        fire_ball(context->origin->player, context->p1, ddd[i], dam, context->p2, false);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Summon context->value monsters of context->p1 type.
 *
 * Set context->p2 to add an out of depth element.
 *
 * PWMAngband: set context->p3 to a negative value to get delayed summons (-2 to bypass friendly
 * summons); set context->p3 to a positive value to set the chance to get friendly summons
 */
static bool effect_handler_SUMMON(effect_handler_context_t *context)
{
    int summon_max = effect_calculate_value(context, false);
    int summon_type = context->p1;
    int level_boost = context->p2;
    int message_type = summon_message_type(summon_type);
    int count = 0;
    int y, x;
    struct worldpos *wpos;
    struct monster *mon = context->origin->monster;

    /* Hack -- no summons in Arena */
    if (mon)
    {
        y = mon->fy;
        x = mon->fx;
        wpos = &mon->wpos;
    }
    else
    {
        y = context->origin->player->py;
        x = context->origin->player->px;
        wpos = &context->origin->player->wpos;
    }
    if (pick_arena(wpos, y, x) != -1) return true;

    sound(context->origin->player, message_type);

    /* Monster summon */
    if (mon)
    {
        int rlev = ((mon->race->level >= 1)? mon->race->level: 1);

        /* Set the kin_base if necessary */
        if (summon_type == summon_name_to_idx("KIN")) kin_base = mon->race->base;

        /* Summon them */
        count = summon_monster_aux(context->origin->player, context->cave, mon->fy, mon->fx,
            summon_type, rlev + level_boost, summon_max, 0);

        /* Summoner failed */
        if (!count) msg(context->origin->player, "But nothing comes.");
    }

    /* Delayed summon */
    else if (context->p3 < 0)
    {
        int i, chance = 0, mlvl;

        if (check_antisummon(context->origin->player, NULL)) return true;

        /* Summoners may get friendly summons */
        if (player_has(context->origin->player, PF_SUMMON_SPELLS) && (context->p3 != -2))
            chance = 100;

        /* Summon them */
        mlvl = monster_level(&context->origin->player->wpos);
        for (i = 0; i < summon_max; i++)
        {
            count += summon_specific(context->origin->player, context->cave,
                context->origin->player->py, context->origin->player->px, mlvl + level_boost,
                summon_type, true, false, chance);
        }
    }

    /* Player summon */
    else
    {
        int mlvl;

        if (check_antisummon(context->origin->player, NULL)) return true;

        /* Set the kin_base if necessary */
        if (summon_type == summon_name_to_idx("KIN"))
            kin_base = context->origin->player->poly_race->base;

        /* Summon them */
        mlvl = monster_level(&context->origin->player->wpos);
        count = summon_monster_aux(context->origin->player, context->cave,
            context->origin->player->py, context->origin->player->px, summon_type,
            mlvl + level_boost, summon_max, context->p3);
    }

    /* Identify */
    context->ident = true;

    /* Message for the blind */
    if (count && context->origin->player->timed[TMD_BLIND])
    {
        msgt(context->origin->player, message_type, "You hear %s appear nearby.",
            ((count > 1)? "many things": "something"));
    }

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
    int py = context->origin->player->py;
    int px = context->origin->player->px;
    int dam = effect_calculate_value(context, true);
    int num = context->value.m_bonus;
    int ty, tx;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(context->origin->player->conn)),
        context->origin->player);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    ty = py + ddy[context->dir];
    tx = px + ddx[context->dir];

    /* Hack -- use an actual "target" */
    if ((context->dir == 5) && target_okay(context->origin->player))
    {
        flg &= ~(PROJECT_STOP | PROJECT_THRU);
        target_get(context->origin->player, &tx, &ty);
    }

    context->origin->player->current_sound = -2;
    while (num--)
    {
        /* Aim at the target. Hurt items on floor. */
        if (project(who, context->p2, context->cave, ty, tx, dam, context->p1, flg, 0, 0,
            "annihilated"))
        {
            context->ident = true;
        }
    }
    context->origin->player->current_sound = -1;

    return true;
}


static bool effect_handler_TELE_OBJECT(effect_handler_context_t *context)
{
    struct object *obj, *teled;
    struct player *q;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_SEND, "");
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Restricted by choice */
    if (!object_is_carried(context->origin->player, obj) && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Forbid artifacts */
    if (obj->artifact)
    {
        msg(context->origin->player, "The object is too powerful to be sent...");
        return false;
    }

    q = get_inscribed_player(context->origin->player, context->note);
    if (!q) return true;

    /* Note that the pack is too full */
    if (!inven_carry_okay(q, obj))
    {
        msg(context->origin->player, "%s has no room for another object.", q->name);
        return false;
    }

    /* Note that the pack is too heavy */
    if (!weight_okay(q, obj))
    {
        msg(context->origin->player, "%s is already too burdened to carry another object.", q->name);
        return false;
    }

    /* Restricted by choice */
    if (OPT(q, birth_no_stores))
    {
        msg(context->origin->player, "%s cannot be reached.", q->name);
        return false;
    }

    /* Actually teleport the object to the player inventory */
    teled = object_new();
    object_copy(teled, obj);
    assess_object(q, teled);
    inven_carry(q, teled, true, false);

    /* Combine the pack */
    q->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    q->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    /* Wipe it */
    use_object(context->origin->player, obj, obj->number, false);

    /* Combine the pack */
    context->origin->player->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    context->origin->player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    msg(q, "You are hit by a powerful magic wave from %s.", context->origin->player->name);
    return true;
}


static bool allow_teleport(struct chunk *c, int y, int x, bool safe_ghost, bool is_player)
{
    /* Just require empty space if teleporting a ghost to safety */
    if (safe_ghost)
    {
        if (c->squares[y][x].mon) return false;
    }

    /* Require "naked" floor space */
    else
    {
        if (!square_isempty(c, y, x)) return false;
    }

    /* No monster teleport onto glyph of warding */
    if (!is_player && square_iswarded(c, y, x)) return false;

    /* No teleporting into vaults and such */
    if (square_isvault(c, y, x)) return false;

    return true;
}


/*
 * Teleport player or monster up to context->value.base grids away.
 *
 * If no spaces are readily available, the distance may increase.
 * Try very hard to move the player/monster at least a quarter that distance.
 * Setting context->p2 allows monsters to teleport the player away.
 */
static bool effect_handler_TELEPORT(effect_handler_context_t *context)
{
    int y_start, x_start;
    int dis = context->value.base;
    int y, x, pick;
    int num_spots = 0;
    bool is_player = (!context->origin->monster || context->p2);
    bool safe_ghost = false;
    struct worldpos *wpos;
    int d_min = 0, d_max = 0;
    bool far_location = false;

    /* Hack -- already used up */
    bool used = (context->p3 == 1);

    context->ident = true;

    /* MvM */
    if (context->target_m_ptr && context->p2)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->origin->monster);
        project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, dis,
            context->p1, flag, 0, 0, "annihilated");

        return !used;
    }

    /* Establish the coordinates to teleport from */
    if (is_player)
    {
        y_start = context->origin->player->py;
        x_start = context->origin->player->px;
        safe_ghost = context->origin->player->ghost;
        wpos = &context->origin->player->wpos;
    }
    else
    {
        y_start = context->origin->monster->fy;
        x_start = context->origin->monster->fx;
        wpos = &context->origin->monster->wpos;
    }

    /* Space-time anchor */
    if (check_st_anchor(wpos, y_start, x_start) && !safe_ghost)
    {
        if (context->origin->player) msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, y_start, x_start) && !safe_ghost)
    {
        if (context->origin->player) msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport curse */
    if (context->origin->player && player_of_has(context->origin->player, OF_NO_TELEPORT))
    {
        equip_learn_flag(context->origin->player, OF_NO_TELEPORT);
        msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Hack -- hijack teleport in Arena */
    if (context->origin->player && (context->origin->player->arena_num != -1))
    {
        int arena_num = context->origin->player->arena_num;
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        effect_simple(EF_TELEPORT_TO, who, "0",
            arenas[arena_num].y_1 + 1 + randint1(arenas[arena_num].y_2 - arenas[arena_num].y_1 - 2),
            arenas[arena_num].x_1 + 1 + randint1(arenas[arena_num].x_2 - arenas[arena_num].x_1 - 2),
            0, NULL);
        return !used;
    }

    /* Get min/max teleporting distances */
    for (y = 1; y < context->cave->height - 1; y++)
    {
        for (x = 1; x < context->cave->width - 1; x++)
        {
            int d = distance(y, x, y_start, x_start);

            /* Must move */
            if (d == 0) continue;

            if (!allow_teleport(context->cave, y, x, safe_ghost, is_player)) continue;

            if ((d_min == 0) || (d < d_min)) d_min = d;
            if ((d_max == 0) || (d > d_max)) d_max = d;
        }
    }

    /* Report failure (very unlikely) */
    if ((d_min == 0) && (d_max == 0))
    {
        if (context->origin->player)
            msg(context->origin->player, "Failed to find teleport destination!");
         return !used;
    }

    /* Randomise the distance a little */
    if (one_in_(2)) dis -= randint0(dis / 4);
    else dis += randint0(dis / 4);

    /* Try very hard to move the player/monster between dis / 4 and dis grids away */
    if (dis <= d_min) d_max = d_min;
    else if (dis / 4 >= d_max) d_min = d_max;
    else
    {
        if (dis / 4 > d_min) d_min = dis / 4;
        if (dis < d_max) d_max = dis;
    }

    /* See if we can find a location not too close from previous player location */
    if (is_player)
    {
        for (y = 1; y < context->cave->height - 1; y++)
        {
            for (x = 1; x < context->cave->width - 1; x++)
            {
                int d = distance(y, x, y_start, x_start);
                int d_old = distance(y, x, context->origin->player->old_py,
                    context->origin->player->old_px);

                /* Enforce distance */
                if ((d < d_min) || (d > d_max)) continue;

                if (!allow_teleport(context->cave, y, x, safe_ghost, is_player)) continue;

                /* Not too close from previous player location */
                if (d_old < d_min) continue;

                far_location = true;
                break;
            }
            if (far_location) break;
        }
    }

    /* Count valid teleport locations */
    for (y = 1; y < context->cave->height - 1; y++)
    {
        for (x = 1; x < context->cave->width - 1; x++)
        {
            int d = distance(y, x, y_start, x_start);

            /* Enforce distance */
            if ((d < d_min) || (d > d_max)) continue;
            if (far_location)
            {
                int d_old = distance(y, x, context->origin->player->old_py,
                    context->origin->player->old_px);

                if (d_old < d_min) continue;
            }

            if (!allow_teleport(context->cave, y, x, safe_ghost, is_player)) continue;

            num_spots++;
        }
    }

    /* Pick a spot */
    pick = randint0(num_spots);
    for (y = 1; y < context->cave->height - 1; y++)
    {
        for (x = 1; x < context->cave->width - 1; x++)
        {
            int d = distance(y, x, y_start, x_start);

            /* Enforce distance */
            if ((d < d_min) || (d > d_max)) continue;
            if (far_location)
            {
                int d_old = distance(y, x, context->origin->player->old_py,
                    context->origin->player->old_px);

                if (d_old < d_min) continue;
            }

            if (!allow_teleport(context->cave, y, x, safe_ghost, is_player)) continue;

            pick--;
            if (pick == -1) break;
        }
        if (pick == -1) break;
    }

    /* Sound */
    if (context->origin->player)
        sound(context->origin->player, (is_player? MSG_TELEPORT: MSG_TPOTHER));

    /* Report the teleporting before moving the monster */
    if (!is_player)
    {
        add_monster_message(context->origin->player, context->origin->monster, MON_MSG_DISAPPEAR,
            false);
    }

    /* Move the target */
    monster_swap(context->cave, y_start, x_start, y, x);

    /* Clear any projection marker to prevent double processing */
    sqinfo_off(context->cave->squares[y][x].info, SQUARE_PROJECT);

    /* Handle stuff */
    if (context->origin->player) handle_stuff(context->origin->player);

    /* Hack -- fix store */
    if (is_player && in_store(context->origin->player)) Send_store_leave(context->origin->player);

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
    struct wild_type *w_ptr = get_wt_info_at(context->origin->player->wpos.wy,
        context->origin->player->wpos.wx);
    char *message;
    struct worldpos wpos;
    byte new_level_method;

    /* Hack -- already used up */
    bool used = (context->p2 == 1);

    context->ident = true;

    /* MvM */
    if (context->target_m_ptr)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->origin->monster);
        project(who, 0, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, 0,
            context->p1, flag, 0, 0, "annihilated");

        return !used;
    }

    /* Resist hostile teleport */
    if (context->origin->monster && player_resists(context->origin->player, ELEM_NEXUS))
    {
        msg(context->origin->player, "You resist the effect!");
        return !used;
    }

    /* Space-time anchor */
    if (check_st_anchor(&context->origin->player->wpos, context->origin->player->py,
        context->origin->player->px))
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport curse */
    if (player_of_has(context->origin->player, OF_NO_TELEPORT))
    {
        equip_learn_flag(context->origin->player, OF_NO_TELEPORT);
        msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Arena fighters don't teleport level */
    if (context->origin->player->arena_num != -1)
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* If no dungeon or winner-only dungeon, teleport to a random neighboring level */
    if ((w_ptr->max_depth == 1) || forbid_entrance(context->origin->player))
    {
        struct wild_type *neighbor = NULL;
        int tries = 20;

        /* Get a valid neighbor */
        while (tries--)
        {
            char dir = randint0(4);

            switch (dir)
            {
                case DIR_NORTH: message = "A gust of wind blows you north."; break;
                case DIR_EAST: message = "A gust of wind blows you east."; break;
                case DIR_SOUTH: message = "A gust of wind blows you south."; break;
                case DIR_WEST: message = "A gust of wind blows you west."; break;
            }

            neighbor = get_neighbor(w_ptr, dir);
            if (neighbor && !chunk_inhibit_players(&neighbor->wpos)) break;
            neighbor = NULL;
        }

        if (!neighbor)
        {
            msg(context->origin->player, "The teleporting attempt fails.");
            return !used;
        }

        COORDS_SET(&wpos, neighbor->wpos.wy, neighbor->wpos.wx, 0);
        new_level_method = LEVEL_OUTSIDE_RAND;
    }

    /* Go up or down a level */
    else
    {
        bool up = true, down = true;
        int target_depth;
        int base_depth = context->origin->player->wpos.depth;

        /* No going up with force_descend or on the surface */
        if ((cfg_limit_stairs >= 2) || OPT(context->origin->player, birth_force_descend) ||
            !base_depth)
        {
            up = false;
        }

        /* No forcing player down to quest levels if they can't leave */
        if ((cfg_limit_stairs == 3) || OPT(context->origin->player, birth_force_descend))
        {
            target_depth = dungeon_get_next_level(context->origin->player,
                context->origin->player->max_depth, 1);
            if (is_quest_active(context->origin->player, target_depth))
            {
                msg(context->origin->player, "The teleporting attempt fails.");
                return !used;
            }

            /* Descend one level deeper */
            base_depth = context->origin->player->max_depth;
        }

        /* Can't leave quest levels or go down deeper than the dungeon */
        if (is_quest_active(context->origin->player, context->origin->player->wpos.depth) ||
            (base_depth == w_ptr->max_depth - 1))
        {
            down = false;
        }

        /* Hack -- DM redesigning the level */
        target_depth = dungeon_get_next_level(context->origin->player,
            context->origin->player->wpos.depth, -1);
        COORDS_SET(&wpos, context->origin->player->wpos.wy, context->origin->player->wpos.wx,
            target_depth);
        if (chunk_inhibit_players(&wpos))
            up = false;
        target_depth = dungeon_get_next_level(context->origin->player, base_depth, 1);
        COORDS_SET(&wpos, context->origin->player->wpos.wy, context->origin->player->wpos.wx,
            target_depth);
        if (chunk_inhibit_players(&wpos))
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
            target_depth = dungeon_get_next_level(context->origin->player,
                context->origin->player->wpos.depth, -1);
        }
        else if (down)
        {
            message = "You sink through the floor.";
            target_depth = dungeon_get_next_level(context->origin->player, base_depth, 1);
        }
        else
        {
            msg(context->origin->player, "The teleporting attempt fails.");
            return !used;
        }

        COORDS_SET(&wpos, context->origin->player->wpos.wy, context->origin->player->wpos.wx,
            target_depth);
        new_level_method = LEVEL_RAND;
    }

    /* Tell the player */
    msgt(context->origin->player, MSG_TPLEVEL, message);

    /* Change location */
    dungeon_change_level(context->origin->player, context->cave, &wpos, new_level_method);

    /* Update the wilderness map */
    if (wpos.depth == 0) wild_set_explored(context->origin->player, &wpos);

    return !used;
}


/*
 * Teleport player/monster to a grid near the given location
 * Setting context->p1 and context->p2 treats them as y and x coordinates
 * Hack: setting context->p3 means we are about to enter an arena
 *
 * This function is slightly obsessive about correctness.
 */
static bool effect_handler_TELEPORT_TO(effect_handler_context_t *context)
{
    int py = context->origin->player->py;
    int px = context->origin->player->px;
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
        if (context->origin->monster)
        {
            py = context->origin->monster->fy;
            px = context->origin->monster->fx;
            is_player = false;
        }
    }
    else if (context->origin->monster)
    {
        /* Teleport to monster */
        ny = context->origin->monster->fy;
        nx = context->origin->monster->fx;
    }
    else
    {
        /* Teleport to target */
        if ((context->dir == 5) && target_okay(context->origin->player))
        {
            int rad = effect_calculate_value(context, false);

            target_get(context->origin->player, &nx, &ny);

            if (distance(ny, nx, py, px) > rad)
            {
                msg(context->origin->player, "You cannot blink that far.");
                return true;
            }
        }
        else
        {
            msg(context->origin->player, "You must have a target.");
            return true;
        }
    }

    /* Space-time anchor */
    if (check_st_anchor(&context->origin->player->wpos, ny, nx))
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return true;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, ny, nx))
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return true;
    }

    /* Check for a no teleport curse */
    if (player_of_has(context->origin->player, OF_NO_TELEPORT))
    {
        equip_learn_flag(context->origin->player, OF_NO_TELEPORT);
        msg(context->origin->player, "The teleporting attempt fails.");
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
        {
            /* Hack -- we enter an arena by teleporting into it, so allow that */
            if (!context->p3) legal = false;
        }

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
        msg(context->origin->player, "The teleporting attempt fails.");
        return true;
    }

    /* Move the player */
    monster_swap(context->cave, py, px, y, x);

    /* Clear any projection marker to prevent double processing */
    sqinfo_off(context->cave->squares[y][x].info, SQUARE_PROJECT);

    /* Handle stuff */
    if (is_player) handle_stuff(context->origin->player);

    /* Hack -- fix store */
    if (is_player && in_store(context->origin->player)) Send_store_leave(context->origin->player);

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

    context->ident = true;

    /* Caster */
    c_y = context->p1;
    c_x = context->p2;

    /* Target */
    if (context->origin->monster)
    {
        t_y = context->origin->monster->fy;
        t_x = context->origin->monster->fx;
    }
    else
    {
        struct trap *trap = context->origin->trap;

        t_y = context->origin->player->py;
        t_x = context->origin->player->px;

        /* Player gets pushed in a random direction if on the trap */
        if (trap)
        {
            int d = randint0(8);

            t_y += ddy_ddd[d];
            t_x += ddx_ddd[d];
        }
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
    path_n = project_path(NULL, path_g, z_info->max_range, context->cave, c_y, c_x, t_y, t_x,
        PROJECT_THRU);

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
    if (moved)
    {
        monster_swap(context->cave, t_y, t_x, y, x);
        if (!context->origin->monster) player_know_floor(context->origin->player, context->cave);
    }

    /* Some special messages or effects for player or monster. */
    if (square_isfiery(context->cave, y, x))
    {
        if (!context->origin->monster && !player_passwall(context->origin->player))
            msg(context->origin->player, "You are thrown into molten lava!");
    }

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

    if (context->p2) amount = context->origin->player->timed[context->p1] / context->p2;
    player_dec_timed(context->origin->player, context->p1, amount, true);
    context->ident = true;
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
    if (context->p2 && context->origin->player->timed[context->p1]) amount = context->p2;

    player_inc_timed_aux(context->origin->player, context->origin->monster, context->p1, amount,
        true, true);
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
    if (context->p2 && context->origin->player->timed[context->p1]) amount = context->p2;

    player_inc_timed_aux(context->origin->player, context->origin->monster, context->p1, amount,
        true, false);
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

        switch (context->origin->player->psex)
        {
            case SEX_FEMALE: pm = "Daughter"; break;
            case SEX_MALE: pm = "Son"; break;
            default: pm = "Creature"; break;
        }

        msg(context->origin->player, context->self_msg, pm);
    }

    /* Hack -- Touch of Death */
    if (context->p1 == TMD_DEADLY)
    {
        if (context->origin->player->state.stat_use[STAT_STR] < 18+120)
        {
            msg(context->origin->player, "You're not strong enough to use the Touch of Death.");
            return false;
        }
        if (context->origin->player->state.stat_use[STAT_DEX] < 18+120)
        {
            msg(context->origin->player, "You're not dextrous enough to use the Touch of Death.");
            return false;
        }
    }

    player_set_timed(context->origin->player, context->p1, amount, true);
    context->ident = true;
    return true;
}


/*
 * Affect adjacent grids
 *
 * PWMAngband: set context->p3 to 1 to prevent the effect on static levels
 */
static bool effect_handler_TOUCH(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = (context->p2? context->p2: 1);

    /* Only on random levels */
    if ((context->p3 == 1) && !random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* MvM */
    if (context->target_m_ptr)
    {
        int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->origin->monster);
        project(who, rad, context->cave, context->target_m_ptr->fy, context->target_m_ptr->fx, 0,
            context->p1, flg, 0, 0, "killed");
        return true;
    }

    if (project_touch(context->origin->player, dam, rad, context->p1, false))
    {
        context->ident = true;
        if (context->self_msg) msg(context->origin->player, context->self_msg);
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

    if (project_touch(context->origin->player, dam, 1, context->p1, context->aware))
        context->ident = true;
    return true;
}


static bool effect_handler_UNDEAD_FORM(effect_handler_context_t *context)
{
    /* Not when already undead */
    if (context->origin->player->ghost)
    {
        msg(context->origin->player, "You are already undead.");
        return false;
    }

    /* Requirement not met */
    if (context->origin->player->state.stat_use[STAT_INT] < 18+70)
    {
        msg(context->origin->player, "You're not smart enough to turn into an undead being.");
        return false;
    }

    /* Turn him into an undead being */
    player_turn_undead(context->origin->player);

    return true;
}


/* Hack -- recalculate max. hitpoints between CON and HP restoration */
static bool effect_handler_UPDATE_STUFF(effect_handler_context_t *context)
{
    context->origin->player->upkeep->update |= (PU_BONUS);
    update_stuff(context->origin->player, context->cave);
    return true;
}


/*
 * Wake up all monsters in line of sight
 */
static bool effect_handler_WAKE(effect_handler_context_t *context)
{
    int i;
    bool woken = false;
    struct loc origin_loc;

    /* MvM -- disable */
    if (context->target_m_ptr) return true;

    origin_get_loc(&origin_loc, context->origin);

    /* Wake everyone nearby */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        if (mon->race)
        {
            int radius = z_info->max_sight * 2;

            /* Skip monsters too far away */
            if ((distance(origin_loc.y, origin_loc.x, mon->fy, mon->fx) < radius) &&
                mon->m_timed[MON_TMD_SLEEP])
            {
                mon_clear_timed(context->origin->player, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
                woken = true;

                /* XXX */
                if (monster_is_camouflaged(mon))
                    become_aware(context->origin->player, context->cave, mon);
            }
        }
    }

    /* Messages */
    if (woken) msg(context->origin->player, "You hear a sudden stirring in the distance!");

    context->ident = true;

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
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "The ground shakes for a moment.");
        return true;
    }

    /* Determine the epicentre */
    cy = context->origin->player->py;
    cx = context->origin->player->px;

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

            /* Forget completely */
            square_unglow(context->cave, yy, xx);
            square_forget_all(context->cave, yy, xx);
            square_light_spot(context->cave, yy, xx);

            /* Delete monsters */
            delete_monster(context->cave, yy, xx);
            if (square_ispitfloor(context->cave, yy, xx)) square_clear_feat(context->cave, yy, xx);

            /* Destroy "valid" grids */
            if (square_changeable(context->cave, yy, xx))
            {
                /* Delete objects */
                square_forget_pile_all(context->cave, yy, xx);
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
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

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

    int plev = context->origin->player->lev;
    int die = effect_calculate_value(context, false);
    effect_handler_f handler = NULL;
    effect_handler_context_t new_context;

    memset(&new_context, 0, sizeof(new_context));
    new_context.origin = context->origin;
    new_context.cave = context->cave;
    new_context.aware = context->aware;
    new_context.dir = context->dir;
    new_context.beam = context->beam;
    new_context.boost = context->boost;
    new_context.ident = context->ident;

    if (die > 100) msg(context->origin->player, "You feel a surge of power!");

    if (die < 8)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.p1 = PROJ_MON_CLONE;
        handler = effect_handler_BOLT;
    }
    else if (die < 14)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.base = 100;
        new_context.p1 = PROJ_MON_SPEED;
        handler = effect_handler_BOLT;
    }
    else if (die < 26)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.dice = 4;
        new_context.value.sides = 6;
        new_context.p1 = PROJ_MON_HEAL;
        handler = effect_handler_BOLT;
    }
    else if (die < 31)
    {
        msg_misc(context->origin->player, " discharges an everchanging blast of energy.");
        new_context.aware = false;
        new_context.value.base = plev;
        new_context.p1 = PROJ_MON_POLY;
        new_context.p2 = 1;
        handler = effect_handler_BOLT_AWARE;
    }
    else if (die < 36)
    {
        msg_misc(context->origin->player, " fires a magic missile.");
        new_context.value.dice = 3 + (plev - 1) / 5;
        new_context.value.sides = 4;
        new_context.p1 = PROJ_MISSILE;
        new_context.p2 = -10;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 41)
    {
        msg_misc(context->origin->player, " makes a complicated gesture.");
        new_context.aware = false;
        new_context.value.base = 5;
        new_context.value.dice = 1;
        new_context.value.sides = 5;
        new_context.p1 = PROJ_MON_CONF;
        handler = effect_handler_BOLT_AWARE;
    }
    else if (die < 46)
    {
        msg_misc(context->origin->player, " fires a stinking cloud.");
        new_context.value.base = 20 + plev / 2;
        new_context.p1 = PROJ_POIS;
        new_context.p2 = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 51)
    {
        msg_misc(context->origin->player, "'s hands project a line of shimmering blue light.");
        new_context.value.dice = 6;
        new_context.value.sides = 8;
        new_context.p1 = PROJ_LIGHT_WEAK;
        new_context.self_msg = "A line of shimmering blue light appears.";
        handler = effect_handler_LINE;
    }
    else if (die < 56)
    {
        msg_misc(context->origin->player, " fires a lightning bolt.");
        new_context.value.dice = 3 + (plev - 5) / 6;
        new_context.value.sides = 6;
        new_context.p1 = PROJ_ELEC;
        handler = effect_handler_BEAM;
    }
    else if (die < 61)
    {
        msg_misc(context->origin->player, " fires a frost bolt.");
        new_context.value.dice = 5 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.p1 = PROJ_COLD;
        new_context.p2 = -10;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 66)
    {
        msg_misc(context->origin->player, " fires an acid bolt.");
        new_context.value.dice = 6 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.p1 = PROJ_ACID;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 71)
    {
        msg_misc(context->origin->player, " fires a fire bolt.");
        new_context.value.dice = 8 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.p1 = PROJ_FIRE;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 76)
    {
        msg_misc(context->origin->player, " fires a bolt filled with pure energy!");
        new_context.value.base = 75;
        new_context.p1 = PROJ_MON_DRAIN;
        handler = effect_handler_BOLT;
    }
    else if (die < 81)
    {
        msg_misc(context->origin->player, " fires a lightning ball.");
        new_context.value.base = 30 + plev / 2;
        new_context.p1 = PROJ_ELEC;
        new_context.p2 = 2;
        handler = effect_handler_BALL;
    }
    else if (die < 86)
    {
        msg_misc(context->origin->player, " fires an acid ball.");
        new_context.value.base = 40 + plev;
        new_context.p1 = PROJ_ACID;
        new_context.p2 = 2;
        handler = effect_handler_BALL;
    }
    else if (die < 91)
    {
        msg_misc(context->origin->player, " fires an ice ball.");
        new_context.value.base = 70 + plev;
        new_context.p1 = PROJ_ICE;
        new_context.p2 = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 96)
    {
        msg_misc(context->origin->player, " fires a fire ball.");
        new_context.value.base = 80 + plev;
        new_context.p1 = PROJ_FIRE;
        new_context.p2 = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 101)
    {
        msg_misc(context->origin->player, " fires a massive bolt filled with pure energy!");
        new_context.value.base = 100 + plev;
        new_context.p1 = PROJ_MON_DRAIN;
        handler = effect_handler_BOLT;
    }
    else if (die < 104)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.base = 12;
        handler = effect_handler_EARTHQUAKE;
    }
    else if (die < 106)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.base = 15;
        handler = effect_handler_DESTRUCTION;
    }
    else if (die < 108)
    {
        msg_misc(context->origin->player, " mumbles.");
        handler = effect_handler_BANISH;
    }
    else if (die < 110)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.base = 120;
        new_context.p1 = PROJ_DISP_ALL;
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
    msg_misc(context->origin->player, " mumbles.");
    effect_simple(EF_PROJECT_LOS, context->origin, "150", PROJ_DISP_ALL, 1, 0, &context->ident);
    effect_simple(EF_PROJECT_LOS, context->origin, "20", PROJ_MON_SLOW, 0, 0, &context->ident);
    effect_simple(EF_PROJECT_LOS, context->origin, "0", PROJ_MON_SLEEP, 0, 0, &context->ident);
    effect_simple(EF_HEAL_HP, context->origin, "300", 0, 0, 0, &context->ident);

    return true;
}


/*
 * Properties of effects
 */


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


/*
 * Utility functions
 */


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
        case EF_SPOT:
        case EF_STAR:
        case EF_STAR_BALL:
        case EF_SWARM:
        case EF_TELEPORT:
        case EF_TELEPORT_LEVEL:
        case EF_TOUCH:
        case EF_TOUCH_AWARE:
        {
            val = proj_name_to_idx(type);
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
 * Execution of effects
 */


/*
 * Execute an effect chain.
 *
 * effect is the effect chain
 * origin is the origin of the effect (player, monster etc.)
 * ident  will be updated if the effect is identifiable
 *        (NB: no effect ever sets *ident to false)
 * aware  indicates whether the player is aware of the effect already
 * dir    is the direction the effect will go in
 * beam   is the base chance out of 100 that a BOLT_OR_BEAM effect will beam
 * boost  is the extent to which skill surpasses difficulty, used as % boost. It
 *        ranges from 0 to 138.
 */
bool effect_do(struct effect *effect, struct source *origin, bool *ident, bool aware, int dir,
    struct beam_info *beam, int boost, quark_t note, struct monster *target_m_ptr)
{
    bool completed = false;
    effect_handler_f handler;
    struct worldpos wpos;

    /* Paranoia */
    if (origin->player) memcpy(&wpos, &origin->player->wpos, sizeof(wpos));
    else if (origin->monster) memcpy(&wpos, &origin->monster->wpos, sizeof(wpos));
    else if (target_m_ptr) memcpy(&wpos, &target_m_ptr->wpos, sizeof(wpos));
    else quit_fmt("No valid source in effect_do(). Please report this bug.");

    do
    {
        int random_choices = 0, leftover = 0;
        random_value value;

        if (!effect_valid(effect))
            quit_fmt("Bad effect passed to effect_do(). Please report this bug.");

        memset(&value, 0, sizeof(value));
        if (effect->dice != NULL)
            random_choices = dice_roll(effect->dice, (void *)origin, &value);

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
                dice_roll(effect->dice, (void *)origin, &value);
        }

        /* Handle the effect */
        handler = effects[effect->index].handler;
        if (handler != NULL)
        {
            effect_handler_context_t context;

            context.effect = effect->index;
            context.origin = origin;
            context.cave = chunk_get(&wpos);
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
bool effect_simple(int index, struct source *origin, const char *dice_string, int p1, int p2,
    int p3, bool *ident)
{
    struct effect effect;
    int dir = 0;
    bool dummy_ident = false, result;

    /* Set all the values */
    memset(&effect, 0, sizeof(effect));
    effect.index = index;
    effect.dice = dice_new();
    dice_parse_string(effect.dice, dice_string);
    effect.params[0] = p1;
    effect.params[1] = p2;
    effect.params[2] = p3;

    /* Direction if needed (PWMAngband: simply use actual target) */
    if (effect_aim(&effect)) dir = 5;

    /* Do the effect */
    if (ident)
        result = effect_do(&effect, origin, ident, true, dir, NULL, 0, 0, NULL);
    else
        result = effect_do(&effect, origin, &dummy_ident, true, dir, NULL, 0, 0, NULL);

    dice_free(effect.dice);
    return result;
}
