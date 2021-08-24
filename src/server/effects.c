/*
 * File: effects.c
 * Purpose: Handler and auxiliary functions for every effect in the game
 *
 * Copyright (c) 2007 Andi Sidwell
 * Copyright (c) 2016 Ben Semmler, Nick McConnell
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
    struct beam_info beam;
    int boost;
    random_value value;
    int subtype, radius, other, y, x;
    const char *self_msg;
    bool ident;
    quark_t note;
    int flag;
    struct monster *target_mon;
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

    /* Device boost */
    if (use_boost) final = final * (100 + context->boost) / 100;

    /* Hack -- elementalists */
    final = final * (20 + context->beam.elem_power) / 20;

    return final;
}


static void get_target(struct chunk *c, struct source *origin, int dir, struct loc *grid)
{
    /* MvX */
    if (origin->monster)
    {
        int accuracy = monster_effect_accuracy(origin->monster, MON_TMD_CONF, CONF_RANDOM_CHANCE);

        if (randint1(100) > accuracy)
        {
            dir = ddd[randint0(8)];
            next_grid(grid, &origin->monster->grid, dir);
        }
        else
        {
            struct loc *decoy = cave_find_decoy(c);

            if (!loc_is_zero(decoy))
                loc_copy(grid, decoy);
            else
                loc_copy(grid, &origin->player->grid);
        }
    }

    /* Ask for a target if no direction given */
    else if ((dir == DIR_TARGET) && target_okay(origin->player))
        target_get(origin->player, grid);

    /* Use the adjacent grid in the given direction as target */
    else
        next_grid(grid, &origin->player->grid, dir);
}


/*
 * Apply the project() function in a direction, or at a target
 */
bool project_aimed(struct source *origin, int typ, int dir, int dam, int flg, const char *what)
{
    struct chunk *c = chunk_get(&origin->player->wpos);
    struct source who_body;
    struct source *who = &who_body;
    struct loc grid;

    /* Pass through the target if needed */
    flg |= (PROJECT_THRU);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    get_target(c, origin, dir, &grid);

    /* Hack -- only one source */
    if (origin->monster)
        source_monster(who, origin->monster);
    else
        source_player(who, get_player_index(get_connection(origin->player->conn)), origin->player);

    /* Aim at the target, do NOT explode */
    return (project(who, 0, c, &grid, dam, typ, flg, 0, 0, what));
}


/*
 * Apply the project() function to grids around the player
 */
static bool project_touch(struct player *p, int dam, int rad, int typ, bool aware, bool mon)
{
    struct loc pgrid;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY | PROJECT_HIDE | PROJECT_THRU;
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc *decoy = cave_find_decoy(c);

    if (!loc_is_zero(decoy) && mon)
    {
        loc_copy(&pgrid, decoy);
        flg |= PROJECT_JUMP;
    }
    else
        loc_copy(&pgrid, &p->grid);
    source_player(who, get_player_index(get_connection(p->conn)), p);

    if (aware) flg |= PROJECT_AWARE;
    return (project(who, rad, c, &pgrid, dam, typ, flg, 0, 0, "killed"));
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
    struct loc grid;

    /* Paranoia */
    if ((index < 0) || (index >= z_info->curse_max)) return false;

    curse = &obj->curses[index];

    /* Save object info (backfire may destroy it) */
    carried = object_is_carried(p, obj);
    loc_copy(&grid, &obj->grid);

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
    if (!carried) redraw_floor(&p->wpos, &grid, NULL);

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


static struct ego_item *ego_brand(struct object *obj, const char *brand)
{
    int i;

    for (i = 0; i < z_info->e_max; i++)
    {
        struct ego_item *ego = &e_info[i];

        /* Match the name */
        if (!ego->name) continue;
        if (streq(ego->name, brand))
        {
            struct poss_item *poss;

            for (poss = ego->poss_items; poss; poss = poss->next)
            {
                if (poss->kidx == obj->kind->kidx)
                    return ego;
            }
        }
    }

    return NULL;
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
    struct ego_item *ego;

    /* You can never modify artifacts, ego items or worthless items */
    if (obj && obj->kind->cost && !obj->artifact && !obj->ego)
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

        /* Describe */
        msg(p, "The %s %s surrounded with an aura of %s.", o_name,
            ((obj->number > 1)? "are": "is"), name);

        /* Get the right ego type for the object */
        ego = ego_brand(obj, brand);

        /* Hack -- BRAND_COOL */
        if (streq(brand, "BRAND_COOL")) ego = ego_elemental();

        if (!ego)
        {
            msg(p, "The branding failed.");
            return;
        }

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
 * Hook to specify a staff
 */
static bool item_tester_hook_staff(const struct object *obj)
{
    return tval_is_staff(obj);
}


/*
 * Increment magical detection counter for a monster/player.
 */
static void give_detect(struct player *p, struct source *who)
{
    int power = 2 + (p->lev + 2) / 5;

    /* Players */
    if (who->player)
    {
        if (p->play_det[who->idx]) power = 1;
        p->play_det[who->idx] = MIN((int)p->play_det[who->idx] + power, 255);
    }

    /* Monsters */
    else
    {
        if (p->mon_det[who->idx]) power = 1;
        p->mon_det[who->idx] = MIN((int)p->mon_det[who->idx] + power, 255);
    }
}


/*
 * Apply a "project()" directly to all viewable monsters
 */
static bool project_los(effect_handler_context_t *context, int typ, int dam, bool obvious)
{
    int i;
    struct loc origin;
    int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_PLAY | PROJECT_HIDE;
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = context->cave;
    struct player *p = context->origin->player;

    origin_get_loc(&origin, context->origin);

    if (context->origin->monster)
        source_monster(who, context->origin->monster);
    else
        source_player(who, get_player_index(get_connection(p->conn)), p);

    if (obvious) flg |= PROJECT_AWARE;

    p->current_sound = -2;

    /* Affect all (nearby) monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Require line of sight */
        if (!los(c, &origin, &mon->grid)) continue;

        /* Jump directly to the monster */
        if (project(who, 0, c, &mon->grid, dam, typ, flg, 0, 0, "killed"))
            obvious = true;
    }

    /* Affect all (nearby) players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this level */
        if (!wpos_eq(&q->wpos, &p->wpos)) continue;

        /* Skip ourself */
        if (q == p) continue;

        /* Require line of sight */
        if (!los(c, &origin, &q->grid)) continue;

        /* Jump directly to the player */
        if (project(who, 0, c, &q->grid, dam, typ, flg, 0, 0, "killed"))
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
 * Detect monsters which satisfy the given predicate around the player.
 * The height to detect above and below the player is y_dist,
 * the width either side of the player x_dist.
 */
static bool detect_monsters(struct player *p, int y_dist, int x_dist, monster_predicate pred,
    int flag, player_predicate ppred)
{
    int i;
    int x1, x2, y1, y2;
    bool monsters = false;
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);

    /* Set the detection area */
    y1 = p->grid.y - y_dist;
    y2 = p->grid.y + y_dist;
    x1 = p->grid.x - x_dist;
    x2 = p->grid.x + x_dist;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);
        struct monster_lore *lore;

        /* Skip dead monsters */
        if (!mon->race) continue;

        lore = get_lore(p, mon->race);

        /* Only detect nearby monsters */
        if ((mon->grid.x < x1) || (mon->grid.y < y1) || (mon->grid.x > x2) || (mon->grid.y > y2))
            continue;

        /* Detect all appropriate, obvious monsters */
        if (pred(mon))
        {
            struct actor_race *monster_race = &p->upkeep->monster_race;

            /* Increment detection counter */
            source_monster(who, mon);
            give_detect(p, who);

            /* Skip visible monsters */
            if (monster_is_visible(p, i)) continue;

            /* Take note that they are detectable */
            if (flag) rf_on(lore->flags, flag);

            /* Update monster recall window */
            if (ACTOR_RACE_EQUAL(monster_race, mon)) p->upkeep->redraw |= (PR_MONSTER);

            /* Detect */
            monsters = true;
        }
    }

    /* Scan players */
    for (i = 1; ((i <= NumPlayers) && ppred); i++)
    {
        struct player *q = player_get(i);

        /* Skip ourself */
        if (q == p) continue;

        /* Skip players not on this level */
        if (!wpos_eq(&q->wpos, &p->wpos)) continue;

        /* Only detect nearby players */
        if ((q->grid.x < x1) || (q->grid.y < y1) || (q->grid.x > x2) || (q->grid.y > y2)) continue;

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Detect all appropriate, obvious players */
        if (ppred(q))
        {
            /* Increment detection counter */
            source_player(who, i, q);
            give_detect(p, who);

            /* Skip visible players */
            if (player_is_visible(p, i)) continue;

            /* Detect */
            monsters = true;
        }
    }

    return monsters;
}


/*
 * Detect "invisible" monsters around the player.
 */
static bool detect_monsters_invis(struct player *p, int y_dist, int x_dist, bool pause,
    bool aware)
{
    bool monsters = detect_monsters(p, y_dist, x_dist, monster_is_invisible, RF_INVISIBLE,
        player_is_invisible);
    struct chunk *c = chunk_get(&p->wpos);

    /* Describe result, and clean up */
    if (monsters && pause)
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
    else if (aware && !monsters)
        msg(p, "You sense no invisible creatures.");

    /* Result */
    return monsters;
}


/*
 * Detect "normal" monsters around the player.
 */
static bool detect_monsters_normal(struct player *p, int y_dist, int x_dist, bool pause,
    bool aware)
{
    bool monsters = detect_monsters(p, y_dist, x_dist, monster_is_not_invisible, 0,
        player_is_not_invisible);
    struct chunk *c = chunk_get(&p->wpos);

    /* Describe and clean up */
    if (monsters && pause)
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
    else if (aware && !monsters)
        msg(p, "You sense no monsters.");

    /* Result */
    return monsters;
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

    /* Scan the inscription for #P */
    while ((*inscription != '\0') && !q)
    {
        if (*inscription == '#')
        {
            inscription++;

            /* A valid #P has been located */
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
    player_set_timed(p, TMD_FOOD, PY_FOOD_FULL - 1, false);

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
 * Checks if an inscription is valid.
 *
 * Returns 0 if valid, 1 if invalid, 2 if asking for recall depth
 */
static int valid_inscription(struct player *p, unsigned char *inscription, int current_value,
    const char *where)
{
    struct wild_type *w_ptr = get_wt_info_at(&p->wpos.grid);

    /* Scan the inscription for #R */
    while (*inscription != '\0')
    {
        if (*inscription == '#')
        {
            inscription++;

            if (*inscription == 'R')
            {
                int depth;
                struct loc grid;
                char buf[NORMAL_WID];

                /* A valid #R has been located */
                inscription++;

                /* Generic #R inscription: ask for recall depth */
                if (*inscription == '\0')
                {
                    /* Ask for recall depth */
                    if (current_value == ITEM_REQUEST)
                    {
                        get_item(p, HOOK_RECALL, "");
                        return 2;
                    }

                    /* Default recall depth */
                    if (STRZERO(where)) return 0;

                    /* Use recall depth */
                    inscription = (unsigned char *)where;
                }

                /* Convert the inscription into wilderness coordinates */
                if ((3 == sscanf(inscription, "%d,%d%s", &grid.x, &grid.y, buf)) ||
                    (2 == sscanf(inscription, "%d,%d", &grid.x, &grid.y)))
                {
                    /* Forbid if no wilderness */
                    if ((cfg_diving_mode > 1) || OPT(p, birth_no_recall))
                    {
                        /* Deactivate recall */
                        memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                        return 1;
                    }

                    /* Do some bounds checking/sanity checks */
                    w_ptr = get_wt_info_at(&grid);
                    if (w_ptr)
                    {
                        /* Verify that the player has visited here before */
                        if (wild_is_explored(p, &w_ptr->wpos))
                        {
                            memcpy(&p->recall_wpos, &w_ptr->wpos, sizeof(struct worldpos));
                            return 1;
                        }
                    }

                    /* Deactivate recall */
                    memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                    return 1;
                }

                /* Convert the inscription into a level index */
                if ((2 == sscanf(inscription, "%d%s", &depth, buf)) ||
                    (1 == sscanf(inscription, "%d", &depth)))
                {
                    /* Help avoid typos */
                    if (depth % 50)
                    {
                        /* Deactivate recall */
                        memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                        return 1;
                    }

                    /* Convert from ft to index */
                    depth /= 50;

                    /* Do some bounds checking/sanity checks */
                    if ((depth >= w_ptr->min_depth) && (depth <= p->recall_wpos.depth))
                    {
                        p->recall_wpos.depth = depth;
                        break;
                    }

                    /* Deactivate recall */
                    memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                    return 1;
                }

                /* Deactivate recall */
                memcpy(&p->recall_wpos, &p->wpos, sizeof(struct worldpos));
                return 1;
            }
        }

        inscription++;
    }

    return 0;
}


/*
 * Selects the recall depth.
 *
 * Inscribe #Rdepth to recall to a specific depth.
 * Inscribe #Rx,y to recall to a specific wilderness level (this assumes
 * that the player has explored the respective wilderness level).
 */
static bool set_recall_depth(struct player *p, quark_t note, int current_value, const char *where)
{
    unsigned char *inscription = (unsigned char *)quark_str(note);
    struct wild_type *w_ptr = get_wt_info_at(&p->wpos.grid);

    /* Default to the players maximum depth */
    wpos_init(&p->recall_wpos, &p->wpos.grid, p->max_depth);

    /* PWMAngband: check minimum/maximum depth of current dungeon */
    if (p->recall_wpos.depth > 0)
    {
        p->recall_wpos.depth = MAX(p->recall_wpos.depth, w_ptr->min_depth);
        p->recall_wpos.depth = MIN(p->recall_wpos.depth, w_ptr->max_depth - 1);
    }

    /* Check for a valid inscription */
    if (inscription)
    {
        int result = valid_inscription(p, inscription, current_value, where);

        if (result == 1) return true;
        if (result == 2) return false;
    }

    /* Force descent to a lower level if allowed */
    if (((cfg_limit_stairs == 3) || OPT(p, birth_force_descend)) &&
        (p->max_depth < z_info->max_depth - 1))
    {
        p->recall_wpos.depth = dungeon_get_next_level(p, p->max_depth, 1);
    }

    return true;
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(struct player *p, int typ, int dir, int dam, int rad, bool obvious)
{
    struct loc target;
    bool result;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(p->conn)), p);

    if (obvious) flg |= PROJECT_AWARE;

    /* Hack -- heal self */
    if (typ == PROJ_MON_HEAL) flg |= PROJECT_CONST;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    /* Use the given direction */
    next_grid(&target, &p->grid, dir);

    /* Hack -- use an actual "target" */
    if ((dir == DIR_TARGET) && target_okay(p))
    {
        flg &= ~(PROJECT_STOP | PROJECT_THRU);
        target_get(p, &target);
    }

    /* Analyze the "dir" and the "target".  Hurt items on floor. */
    p->current_sound = -2;
    result = project(who, rad, chunk_get(&p->wpos), &target, dam, typ, flg, 0, 0, "annihilated");
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

    if (project_aimed(context->origin, context->subtype, context->dir, 0, flg, "killed"))
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
        if (!wpos_eq(&q->wpos, &context->origin->player->wpos)) continue;

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


static bool handler_breath(effect_handler_context_t *context, bool use_boost)
{
    int dam = effect_calculate_value(context, use_boost);
    int type = context->subtype;
    struct loc target;
    struct source who_body;
    struct source *who = &who_body;

    /*
     * Diameter of source starts at 4, so full strength up to 3 grids from
     * the breather.
     */
    int diameter_of_source = 4;

    /* Minimum breath width is 20 degrees */
    int degrees_of_arc = MAX(context->other, 20);

    int flg = PROJECT_ARC | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

    /* Distance breathed has no fixed limit. */
    int rad = z_info->max_range;

    /* Hack -- already used up */
    bool used = (context->radius == 1);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Player or monster? */
    if (context->origin->monster)
    {
        int accuracy = monster_effect_accuracy(context->origin->monster, MON_TMD_CONF,
            CONF_RANDOM_CHANCE);

        source_monster(who, context->origin->monster);

        /* Breath parameters for monsters are monster-dependent */
        dam = breath_dam(type, context->origin->monster->hp);

        /* Powerful monster */
        if (monster_is_powerful(context->origin->monster->race))
        {
            /* Breath is now full strength at 5 grids */
            diameter_of_source *= 3;
            diameter_of_source /= 2;
        }

        /* Target player or monster? */
        if (randint1(100) > accuracy)
        {
            /* Confused direction. */
            int dir = ddd[randint0(8)];

            next_grid(&target, &context->origin->monster->grid, dir);
        }
        else if (context->target_mon)
        {
            /* Target monster. */
            loc_copy(&target, &context->target_mon->grid);
        }
        else
        {
            /* Target player. */
            struct loc *decoy = cave_find_decoy(context->cave);

            if (!loc_is_zero(decoy))
                loc_copy(&target, decoy);
            else
                loc_copy(&target, &context->origin->player->grid);
            who->target = context->origin->player;
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

            /* Breathing also consumes food */
            if (!context->origin->player->ghost)
                player_dec_timed(context->origin->player, TMD_FOOD, 50, false);

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
        if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
            target_get(context->origin->player, &target);
        else
        {
            /* Hack -- no target available, default to random direction */
            if (context->dir == DIR_TARGET) context->dir = 0;

            /* Hack -- no direction given, default to random direction */
            if (!context->dir) context->dir = ddd[randint0(8)];

            /* Use the given direction */
            next_grid(&target, &context->origin->player->grid, context->dir);
        }
    }

    /* Adjust the diameter of the energy source */
    if (degrees_of_arc < 60)
    {
        /*
         * Narrower cone means energy drops off less quickly. We now have:
         * - 30 degree regular breath  | full strength at 5 grids
         * - 30 degree powerful breath | full strength at 9 grids
         * - 20 degree regular breath  | full strength at 11 grids
         * - 20 degree powerful breath | full strength at 17 grids
         * where grids are measured from the breather.
         */
        diameter_of_source = diameter_of_source * 60 / degrees_of_arc;

        /* Max */
        if (diameter_of_source > 25) diameter_of_source = 25;
    }

    /* Breathe at the target */
    context->origin->player->current_sound = -2;
    if (project(who, rad, context->cave, &target, dam, type, flg, degrees_of_arc, diameter_of_source,
        "vaporized"))
    {
        context->ident = true;
    }
    context->origin->player->current_sound = -1;

    return !used;
}


/*
 * Breathe an element, in a cone from the breath
 * Affect grids, objects, and monsters
 * context->subtype is element, context->other degrees of arc
 *
 * PWMAngband: if context->radius is set, object is already used up; use device boost
 */
static bool effect_handler_ARC(effect_handler_context_t *context)
{
    return handler_breath(context, true);
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
    int rad = (context->radius? context->radius: 2);
    struct loc target;
    struct source who_body;
    struct source *who = &who_body;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    const char *what = "annihilated";

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Player or monster? */
    if (context->origin->monster)
    {
        int accuracy = monster_effect_accuracy(context->origin->monster, MON_TMD_CONF,
            CONF_RANDOM_CHANCE);

        source_monster(who, context->origin->monster);

        /* Powerful monster */
        if (monster_is_powerful(context->origin->monster->race)) rad++;

        flg &= ~(PROJECT_STOP | PROJECT_THRU);

        /* Handle confusion */
        if (randint1(100) > accuracy)
        {
            int dir = ddd[randint0(8)];

            next_grid(&target, &context->origin->monster->grid, dir);
        }

        /* Target monster */
        else if (context->target_mon)
            loc_copy(&target, &context->target_mon->grid);

        /* Target player */
        else
        {
            struct loc *decoy = cave_find_decoy(context->cave);

            if (!loc_is_zero(decoy))
                loc_copy(&target, decoy);
            else
                loc_copy(&target, &context->origin->player->grid);
            who->target = context->origin->player;
        }
    }
    else
    {
        struct trap *trap = context->origin->trap;

        if (trap)
        {
            loc_copy(&target, &trap->grid);
            source_trap(who, trap);
        }
        else
        {
            if (context->other) rad += context->origin->player->lev / context->other;

            /* Hack -- mimics */
            if (context->origin->player->poly_race &&
                monster_is_powerful(context->origin->player->poly_race))
            {
                rad++;
            }

            /* Hack -- elementalists */
            rad = rad + context->beam.spell_power / 2;
            rad = rad * (20 + context->beam.elem_power) / 20;

            source_player(who, get_player_index(get_connection(context->origin->player->conn)),
                context->origin->player);

            /* Ask for a target if no direction given */
            if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
            {
                flg &= ~(PROJECT_STOP | PROJECT_THRU);
                target_get(context->origin->player, &target);
            }

            /* Use the given direction */
            else
                next_grid(&target, &context->origin->player->grid, context->dir);
        }
    }

    /* Aim at the target, explode */
    context->origin->player->current_sound = -2;
    if (project(who, rad, context->cave, &target, dam, context->subtype, flg, 0, 0, what))
        context->ident = true;
    context->origin->player->current_sound = -1;

    return true;
}


/*
 * Cast a ball spell which effect is obvious.
 * If context->other is negative, allow only on random levels.
 */
static bool effect_handler_BALL_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int rad = context->radius + ((context->other > 0)? (context->origin->player->lev / context->other): 0);

    /* Only on random levels */
    if ((context->other < 0) && !random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create traps here...");
        context->ident = true;
        return false;
    }

    if (fire_ball(context->origin->player, context->subtype, context->dir, dam, rad, true))
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
        if ((tmp = distance(&context->origin->player->grid, &mon->grid)) < d)
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
 * Turns the player into a fruit bat
 */
static bool effect_handler_BATTY(effect_handler_context_t *context)
{
    poly_bat(context->origin->player, 100, NULL);
    context->ident = true;
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

    fire_beam(context->origin, context->subtype, context->dir, dam, false);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;
    return true;
}


/*
 * Cast a beam spell which effect is obvious
 */
static bool effect_handler_BEAM_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    if (fire_beam(context->origin, context->subtype, context->dir, dam, true))
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
            project_los(context, PROJ_DISP_ALL, 1000, false);

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
    int rad = context->radius + (context->other? (context->origin->player->lev / context->other): 0);

    /* Hack -- elementalists */
    rad = rad + context->beam.spell_power / 2;
    rad = rad * (20 + context->beam.elem_power) / 20;

    if (fire_ball(context->origin->player, context->subtype, 0, dam, rad, false))
        context->ident = true;
    return true;
}


/*
 * Cast a ball spell centered on the character (with obvious effects)
 */
static bool effect_handler_BLAST_OBVIOUS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = context->radius + (context->other? (context->origin->player->lev / context->other): 0);

    /* Monster */
    if (context->origin->monster)
    {
        int rlev = ((context->origin->monster->race->level >= 1)?
            context->origin->monster->race->level: 1);
        struct source who_body;
        struct source *who = &who_body;

        rad = context->radius + (context->other? (rlev / context->other): 0);

        source_monster(who, context->origin->monster);
        project(who, rad, context->cave, &context->origin->monster->grid, 0, context->subtype,
            PROJECT_ITEM | PROJECT_HIDE, 0, 0, "killed");
        update_smart_learn(context->origin->monster, context->origin->player, 0, 0, context->subtype);
    }

    /* Player */
    else if (fire_ball(context->origin->player, context->subtype, 0, dam, rad, true))
        context->ident = true;

    return true;
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a bolt
 * Affect monsters (not grids or objects)
 *
 * PWMAngband: setting context->radius is a hack for teleport other
 */
static bool effect_handler_BOLT(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    /* Hack -- teleport other */
    if (context->radius)
    {
        context->origin->player->current_sound = -2;
        sound(context->origin->player, MSG_TPOTHER);
        if (fire_bolt(context->origin, context->subtype, context->dir, dam, false))
            context->ident = true;
        context->origin->player->current_sound = -1;
    }

    /* MvM */
    else if (context->target_mon)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct source who_body;
        struct source *who = &who_body;
        struct loc target;
        int accuracy = monster_effect_accuracy(context->origin->monster, MON_TMD_CONF,
            CONF_RANDOM_CHANCE);

        if (randint1(100) > accuracy)
        {
            int dir = ddd[randint0(8)];

            next_grid(&target, &context->origin->monster->grid, dir);
        }
        else
            loc_copy(&target, &context->target_mon->grid);

        source_monster(who, context->origin->monster);
        project(who, 0, context->cave, &target, dam, context->subtype, flag, 0, 0, "annihilated");
    }

    /* Normal case */
    else if (fire_bolt(context->origin, context->subtype, context->dir, dam, false))
        context->ident = true;

    return true;
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a bolt
 * Affect monsters (not grids or objects)
 * Notice stuff based on awareness of the effect
 *
 * PWMAngband: if context->radius is set, forbid on static levels
 */
static bool effect_handler_BOLT_AWARE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);

    /* Forbid in the towns and on special levels */
    if (context->radius && forbid_special(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot polymorph monsters here...");
        context->ident = true;
        return false;
    }

    if (fire_bolt(context->origin, context->subtype, context->dir, dam, context->aware))
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
    struct loc target;
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(context->origin->player->conn)),
        context->origin->player);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    next_grid(&target, &context->origin->player->grid, context->dir);

    /* Hack -- use an actual "target" */
    if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
    {
        target_get(context->origin->player, &target);

        /* Check distance */
        if (distance(&context->origin->player->grid, &target) > 1)
        {
            msg(context->origin->player, "Target out of range.");
            context->ident = true;
            return true;
        }
    }

    /* Analyze the "dir" and the "target", do NOT explode */
    if (project(who, 0, context->cave, &target, dam, context->subtype,
        PROJECT_GRID | PROJECT_KILL | PROJECT_PLAY, 0, 0, "annihilated"))
    {
        context->ident = true;
    }

    return true;
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 * context->other is used as any adjustment to the regular beam chance
 */
static bool effect_handler_BOLT_OR_BEAM(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int beam = context->beam.beam + context->other;

    /* Hack -- space/time anchor */
    if (context->origin->player->timed[TMD_ANCHOR] && (context->subtype == PROJ_TIME))
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
        fire_beam(context->origin, context->subtype, context->dir, dam, false);
    else
        fire_bolt(context->origin, context->subtype, context->dir, dam, false);
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
    context->origin->player->brand.type = (bitflag)context->subtype;
    context->origin->player->brand.blast = (context->radius? true: false);
    context->origin->player->brand.dam = (context->radius? effect_calculate_value(context, false): 0);

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
    context->origin->player->brand.type = (bitflag)context->subtype;
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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
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
        redraw_floor(&context->origin->player->wpos, &obj->grid, NULL);

    return true;
}


/*
 * Brand the current weapon
 *
 * PWMAngband: if context->radius is set, brand with weak frost instead of fire
 */
static bool effect_handler_BRAND_WEAPON(effect_handler_context_t *context)
{
    /* Hack -- branded with fire? */
    bool with_fire = (context->radius? false: true);

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
 * context->subtype is element, context->other degrees of arc
 *
 * PWMAngband: if context->radius is set, object is already used up; don't use device boost
 */
static bool effect_handler_BREATH(effect_handler_context_t *context)
{
    return handler_breath(context, false);
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


static bool effect_handler_COOKIE(effect_handler_context_t *context)
{
    struct hint *v, *r = NULL;
    int n;

    msg(context->origin->player, "Suddenly a thought comes to your mind:");

    /* Get a random hint from the global hints list */
    for (v = hints, n = 1; v; v = v->next, n++)
    {
        if (one_in_(n)) r = v;
    }

    msg(context->origin->player, r->hint);

    context->ident = true;
    return true;
}


/*
 * Turn a staff into arrows
 */
static bool effect_handler_CREATE_ARROWS(effect_handler_context_t *context)
{
    int lev;
    struct object *obj, *arrows;
    bool good = false, great = false;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_STAFF, "");
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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
        return false;
    }

    /* Paranoia: requires a staff */
    if (!item_tester_hook_staff(obj)) return false;

    /* Extract the object "level" */
    lev = obj->kind->level;

    /* Roll for good */
    if (randint1(lev) > 25)
    {
        good = true;

        /* Roll for great */
        if (randint1(lev) > 50) great = true;
    }

    /* Destroy the staff */
    use_object(context->origin->player, obj, 1, true);

    /* Make some arrows */
    arrows = make_object(context->origin->player, context->cave, context->origin->player->lev, good,
        great, false, NULL, TV_ARROW);
    if (!arrows) return true;
    set_origin(arrows, ORIGIN_ACQUIRE, context->origin->player->wpos.depth, NULL);

    drop_near(context->origin->player, context->cave, &arrows, 0, &context->origin->player->grid,
        true, DROP_FADE, true);

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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
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

    drop_near(context->origin->player, context->cave, &poison, 0, &context->origin->player->grid,
        true, DROP_FADE, true);

    return true;
}


/*
 * Create stairs at the player location
 */
static bool effect_handler_CREATE_STAIRS(effect_handler_context_t *context)
{
    struct wild_type *w_ptr = get_wt_info_at(&context->origin->player->wpos.grid);

    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create stairs here...");
        return false;
    }

    /* Only allow stairs to be created on empty floor */
    if (!square_isfloor(context->cave, &context->origin->player->grid))
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
    push_object(context->origin->player, context->cave, &context->origin->player->grid);

    /* Surface: always down */
    if (context->cave->wpos.depth == 0)
        square_add_stairs(context->cave, &context->origin->player->grid, FEAT_MORE);

    /* Bottom: always up */
    else if (context->cave->wpos.depth == w_ptr->max_depth - 1)
        square_add_stairs(context->cave, &context->origin->player->grid, FEAT_LESS);

    /* Random */
    else
        square_add_stairs(context->cave, &context->origin->player->grid, FEAT_NONE);

    return true;
}


static bool effect_handler_CREATE_TREES(effect_handler_context_t *context)
{
    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create trees here...");
        return false;
    }

    fire_ball(context->origin->player, PROJ_TREES, 0, 1, 3, false);
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
            struct loc target;

            next_grid(&target, &context->origin->player->grid, dir);
            project(who, 0, context->cave, &target, 0, PROJ_STONE_WALL, PROJECT_GRID, 0, 0, "killed");
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
    int type = context->subtype;

    player_clear_timed(context->origin->player, type, true);
    context->ident = true;
    return true;
}


/*
 * Curse a monster for direct damage
 */
static bool effect_handler_CURSE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    bool fear = false;
    bool dead = false;
    struct source *target_who = &context->origin->player->target.target_who;

    context->ident = true;

    /* Need to choose a monster, not just point */
    if (!target_able(context->origin->player, target_who))
    {
        msg(context->origin->player, "No target selected!");
        return false;
    }

    if (target_who->monster)
    {
        dead = mon_take_hit(context->origin->player, context->cave, target_who->monster, dam, &fear,
            MON_MSG_DIE);
        if (!dead && monster_is_visible(context->origin->player, target_who->monster->midx))
        {
            if (dam > 0) message_pain(context->origin->player, target_who->monster, dam);
            if (fear)
            {
                add_monster_message(context->origin->player, target_who->monster,
                    MON_MSG_FLEE_IN_TERROR, true);
            }
        }
    }
    else
    {
        char killer[NORMAL_WID];
        char df[160];

        my_strcpy(killer, context->origin->player->name, sizeof(killer));
        strnfmt(df, sizeof(df), "was killed by %s", killer);
        dead = take_hit(target_who->player, dam, killer, false, df);
        if ((dam > 0) && !dead)
            player_pain(context->origin->player, target_who->player, dam);
    }

    return true;
}


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
        object_know_curses(obj);
        object_check_for_ident(context->origin->player, obj);

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
        object_know_curses(obj);
        object_check_for_ident(context->origin->player, obj);

        /* Recalculate bonuses */
        context->origin->player->upkeep->update |= (PU_BONUS);

        /* Redraw */
        context->origin->player->upkeep->redraw |= (PR_EQUIP);
    }

    return true;
}


/*
 * Deal damage from the current monster or trap to the player
 */
static bool effect_handler_DAMAGE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    char killer[NORMAL_WID];
    struct monster *mon = context->origin->monster;
    struct trap *trap = context->origin->trap;
    struct object *obj = context->origin->obj;
    struct chest_trap *chest_trap = context->origin->chest_trap;
    bool non_physical;
    char df[160];

    /* Always ID */
    context->ident = true;

    /* A monster */
    if (mon)
    {
        const char *what = "annihilated";
        struct loc *decoy = cave_find_decoy(context->cave);

        /* Damage another monster */
        if (context->target_mon)
        {
            int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
            struct source who_body;
            struct source *who = &who_body;

            source_monster(who, mon);
            project(who, 0, context->cave, &context->target_mon->grid, dam, context->subtype, flag,
                0, 0, what);

            return true;
        }

        /* Destroy a decoy */
        if (!loc_is_zero(decoy))
        {
            square_destroy_decoy(context->origin->player, context->cave, decoy);
            return true;
        }

        /* Get the "died from" name in case this attack kills @ */
        monster_desc(context->origin->player, killer, sizeof(killer), mon, MDESC_DIED_FROM);

        if ((context->subtype == PROJ_BLAST) || (context->subtype == PROJ_SMASH))
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

    /* A cursed weapon */
    else if (obj)
    {
        object_desc(context->origin->player, killer, sizeof(killer), obj, ODESC_PREFIX | ODESC_BASE);
        non_physical = false;
        strnfmt(df, sizeof(df), "was killed by %s", killer);
    }

    /* A chest */
    else if (chest_trap)
    {
        non_physical = false;
        strnfmt(df, sizeof(df), "was killed by %s", chest_trap->msg_death);
    }

    /* The player */
    else
    {
        if (context->self_msg) my_strcpy(killer, context->self_msg, sizeof(killer));
        else my_strcpy(killer, "self-inflicted wounds", sizeof(killer));
        non_physical = true;
        strnfmt(df, sizeof(df), "was killed by %s", killer);
    }

    /* Hit the player */
    take_hit(context->origin->player, dam, killer, non_physical, df);

    context->self_msg = NULL;
    return true;
}


/*
 * Call darkness around the player or target monster
 */
static bool effect_handler_DARKEN_AREA(effect_handler_context_t *context)
{
    struct loc target;
    bool decoy_unseen = false;

    loc_copy(&target, &context->origin->player->grid);

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

    /* Check for monster targeting another monster */
    if (context->target_mon)
    {
        loc_copy(&target, &context->target_mon->grid);
        if (!context->origin->player->timed[TMD_BLIND])
        {
            char m_name[NORMAL_WID];

            monster_desc(context->origin->player, m_name, sizeof(m_name), context->target_mon,
                MDESC_TARG);
            msg(context->origin->player, "Darkness surrounds %s.", m_name);
        }
    }
    else
    {
        struct loc *decoy = cave_find_decoy(context->cave);

        /* Check for decoy */
        if (!loc_is_zero(decoy) && context->origin->monster)
        {
            loc_copy(&target, decoy);
            if (!los(context->cave, &context->origin->player->grid, decoy) ||
                context->origin->player->timed[TMD_BLIND])
            {
                decoy_unseen = true;
            }
            else
                msg(context->origin->player, "Darkness surrounds the decoy.");
        }

        else if (!context->origin->player->timed[TMD_BLIND])
            msg(context->origin->player, "Darkness surrounds you.");
    }

    /* Darken the room */
    light_room(context->origin->player, context->cave, &target, false);

    /* Hack -- blind the player directly if player-cast */
    if (!context->origin->monster)
    {
        if (!player_resists(context->origin->player, ELEM_DARK))
            player_inc_timed(context->origin->player, TMD_BLIND, 3 + randint1(5), true, true);
        equip_learn_element(context->origin->player, ELEM_DARK);
    }

    /* Assume seen */
    context->ident = !decoy_unseen;

    return true;
}


static bool effect_handler_DARKEN_LEVEL(effect_handler_context_t *context)
{
    bool full = (context->other? true: false);
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

    if (full)
        msg(context->origin->player, "A great blackness rolls through the dungeon...");
    wiz_dark(context->origin->player, context->cave, full);
    context->ident = true;

    /* Check for every other player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Only works for players on the level */
        if (!wpos_eq(&player->wpos, &context->origin->player->wpos)) continue;

        /* Only works on hostile players */
        if (pvp_check(context->origin->player, player, PVP_CHECK_ONE, true, 0x00))
        {
            /* Get the light source */
            struct object *obj = equipped_item_by_slot_name(player, "light");

            /* Bye bye light */
            if (obj && (obj->timeout > 0) && !of_has(obj->flags, OF_NO_FUEL))
            {
                msg(player, "Your light suddenly empty.");

                /* No more light, it's Rogues day today :) */
                obj->timeout = 0;

                /* Redraw */
                player->upkeep->redraw |= (PR_EQUIP);
            }
        }
    }

    return true;
}


/*
 * Teleports 5 dungeon levels down (from max_depth)
 *
 * PWMAngband: set context->radius to activate the descent
 */
static bool effect_handler_DEEP_DESCENT(effect_handler_context_t *context)
{
    int target_increment, target_depth;
    bool apply = (context->radius? true: false);
    struct wild_type *w_ptr = get_wt_info_at(&context->origin->player->wpos.grid);
    struct worldpos wpos;

    context->ident = true;

    /* Special case: no dungeon or winner-only/shallow dungeon */
    if ((w_ptr->max_depth == 1) || forbid_entrance_weak(context->origin->player) ||
        forbid_entrance_strong(context->origin->player))
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

    wpos_init(&wpos, &context->origin->player->wpos.grid, target_depth);

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
        disturb(context->origin->player);
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
    effect_simple(EF_DESTRUCTION, context->origin, "0", ELEM_LIGHT, 5, 1, 0, 0, NULL);
    return true;
}


/*
 * The destruction effect
 *
 * This effect "deletes" monsters (instead of killing them).
 *
 * This is always an effect centered on the player; it is similar to the
 * earthquake effect.
 *
 * PWMAngband: the radius can be set in context->value.base (Major Havoc); if
 * context->other is set, destroy the area silently.
 */
static bool effect_handler_DESTRUCTION(effect_handler_context_t *context)
{
    int k, r = effect_calculate_value(context, false);
    int elem = context->subtype;
    int hurt[MAX_PLAYERS];
    int count = 0;
    struct loc begin, end;
    struct loc_iterator iter;

    if (context->radius) r = context->radius;
    context->ident = true;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        if (!context->other) msg(context->origin->player, "The ground shakes for a moment.");
        return true;
    }

    if (!context->other) msg_misc(context->origin->player, " unleashes great power!");

    loc_init(&begin, context->origin->player->grid.x - r, context->origin->player->grid.y - r);
    loc_init(&end, context->origin->player->grid.x + r, context->origin->player->grid.y + r);
    loc_iterator_first(&iter, &begin, &end);

    /* Big area of affect */
    do
    {
        /* Skip illegal grids */
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Extract the distance */
        k = distance(&context->origin->player->grid, &iter.cur);

        /* Stay in the circle of death */
        if (k > r) continue;

        /* Lose room and vault */
        sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_ROOM);
        sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_VAULT);
        sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_NO_TELEPORT);
        if (square_ispitfloor(context->cave, &iter.cur))
            square_clear_feat(context->cave, &iter.cur);

        /* Forget completely */
        square_unglow(context->cave, &iter.cur);
        square_forget_all(context->cave, &iter.cur);
        square_light_spot(context->cave, &iter.cur);

        /* Hack -- notice player affect */
        if (square(context->cave, &iter.cur)->mon < 0)
        {
            /* Hurt the player later */
            hurt[count] = 0 - square(context->cave, &iter.cur)->mon;
            count++;

            /* Do not hurt this grid */
            continue;
        }

        /* Hack -- skip the epicenter */
        if (loc_eq(&iter.cur, &context->origin->player->grid)) continue;

        /* Delete the monster (if any) */
        delete_monster(context->cave, &iter.cur);
        if (square_ispitfloor(context->cave, &iter.cur))
            square_clear_feat(context->cave, &iter.cur);

        /* Don't remove stairs */
        if (square_isstairs(context->cave, &iter.cur)) continue;

        /* Destroy any grid that isn't a permanent wall */
        if (!square_isunpassable(context->cave, &iter.cur))
        {
            /* Delete objects */
            square_forget_pile_all(context->cave, &iter.cur);
            square_excise_pile(context->cave, &iter.cur);
            square_destroy(context->cave, &iter.cur);
        }
    }
    while (loc_iterator_next(&iter));

    /* Hack -- affect players */
    for (k = 0; k < count; k++)
    {
        struct player *p = player_get(hurt[k]);

        /* Message */
        if (elem == ELEM_LIGHT) msg(p, "There is a searing blast of light!");
        else msg(p, "Darkness seems to crush you!");

        /* Blind the player */
        equip_learn_element(p, elem);
        if (!player_resists(p, elem))
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

        /* Skip ourself */
        if (q == context->origin->player) continue;

        /* Skip players not on this level */
        if (!wpos_eq(&q->wpos, &context->origin->player->wpos)) continue;

        /* Skip the dungeon master if hidden */
        if (q->dm_flags & DM_SECRET_PRESENCE) continue;

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
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_DOORS(effect_handler_context_t *context)
{
    int x1, x2, y1, y2;
    bool doors = false, redraw = false;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Pick an area to map */
    y1 = context->origin->player->grid.y - context->y;
    y2 = context->origin->player->grid.y + context->y;
    x1 = context->origin->player->grid.x - context->x;
    x2 = context->origin->player->grid.x + context->x;

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan the dungeon */
    do
    {
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Detect secret doors */
        if (square_issecretdoor(context->cave, &iter.cur))
        {
            /* Put an actual door */
            place_closed_door(context->cave, &iter.cur);

            /* Memorize */
            square_memorize(context->origin->player, context->cave, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            /* Obvious */
            doors = true;

            redraw = true;
        }

        /* Forget unknown doors in the mapping area */
        if (square_isdoor_p(context->origin->player, &iter.cur) &&
            square_isnotknown(context->origin->player, context->cave, &iter.cur))
        {
            square_forget(context->origin->player, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            redraw = true;
        }
    }
    while (loc_iterator_next(&iter));

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
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_EVIL(effect_handler_context_t *context)
{
    bool monsters = detect_monsters(context->origin->player, context->y, context->x,
        monster_is_evil, RF_EVIL, NULL);

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
 * Detect lmonsters susceptible to fear around the player. The height to detect
 * above and below the player is context->y, the width either side of
 * the player context->x.
 */
static bool effect_handler_DETECT_FEARFUL_MONSTERS(effect_handler_context_t *context)
{
    bool monsters = detect_monsters(context->origin->player, context->y, context->x,
        monster_is_fearful, 0, NULL);

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
        msg(context->origin->player, "These monsters could provide good sport.");
        party_msg_near(context->origin->player, " senses the presence of fearful creatures!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }
    else if (context->aware)
        msg(context->origin->player, "You smell no fear in the air.");

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
    int x1, x2, y1, y2;
    bool redraw = false;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Pick an area to map */
    y1 = context->origin->player->grid.y - context->y;
    y2 = context->origin->player->grid.y + context->y;
    x1 = context->origin->player->grid.x - context->x;
    x2 = context->origin->player->grid.x + context->x;

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan the dungeon */
    do
    {
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Magma/Quartz + Known Gold */
        if (square_hasgoldvein(context->cave, &iter.cur))
        {
            /* Memorize */
            square_memorize(context->origin->player, context->cave, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            redraw = true;
        }

        /* Forget unknown gold in the mapping area */
        if (square_hasgoldvein_p(context->origin->player, &iter.cur) &&
            square_isnotknown(context->origin->player, context->cave, &iter.cur))
        {
            square_forget(context->origin->player, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            redraw = true;
        }
    }
    while (loc_iterator_next(&iter));

    /* Redraw minimap */
    if (redraw) context->origin->player->upkeep->redraw |= PR_MAP;

    context->ident = true;
    return true;
}


/*
 * Detect invisible monsters around the player. The height to detect above and below the player
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_INVISIBLE_MONSTERS(effect_handler_context_t *context)
{
    detect_monsters_invis(context->origin->player, context->y, context->x, true, context->aware);

    context->ident = true;
    return true;
}


/*
 * Detect living monsters around the player. The height to detect above and below the player
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_LIVING_MONSTERS(effect_handler_context_t *context)
{
    bool monsters = detect_monsters(context->origin->player, context->y, context->x,
        monster_is_living, 0, player_is_living);

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
        msg(context->origin->player, "You sense life!");
        party_msg_near(context->origin->player, " senses life!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }
    else if (context->aware)
        msg(context->origin->player, "You sense no life.");

    context->ident = true;
    return true;
}


/*
 * Detect all monsters around the player. The height to detect above and below the player
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_MONSTERS(effect_handler_context_t *context)
{
    /* Reveal monsters */
    bool detected_creatures = detect_monsters_normal(context->origin->player, context->y,
        context->x, false, context->aware);
    bool detected_invis = detect_monsters_invis(context->origin->player, context->y,
        context->x, false, context->aware);

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
 * Detect non-evil monsters around the player. The height to detect above and below the player
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_NONEVIL(effect_handler_context_t *context)
{
    bool monsters = detect_monsters(context->origin->player, context->y, context->x,
        monster_is_nonevil, 0, NULL);

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
        msg(context->origin->player, "You sense the presence of non-evil creatures!");
        party_msg_near(context->origin->player, " senses the presence of non-evil creatures!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }
    else if (context->aware)
        msg(context->origin->player, "You sense no non-evil creatures.");

    context->ident = true;
    return true;
}


/*
 * Detect monsters possessing a spirit around the player.
 * The height to detect above and below the player is context->value.dice,
 * the width either side of the player context->value.sides.
 */
static bool effect_handler_DETECT_SOUL(effect_handler_context_t *context)
{
    bool monsters = detect_monsters(context->origin->player, context->y, context->x,
        monster_has_spirit, RF_SPIRIT, NULL);

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
        msg(context->origin->player, "You sense the presence of spirits!");
        party_msg_near(context->origin->player, " senses the presence of spirits!");

        /* Hack -- pause */
        if (OPT(context->origin->player, pause_after_detect)) Send_pause(context->origin->player);
    }
    else if (context->aware)
        msg(context->origin->player, "You sense no spirits.");

    context->ident = true;
    return true;
}


/*
 * Detect stairs around the player. The height to detect above and below the player
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_STAIRS(effect_handler_context_t *context)
{
    int x1, x2, y1, y2;
    bool stairs = false, redraw = false;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Pick an area to map */
    y1 = context->origin->player->grid.y - context->y;
    y2 = context->origin->player->grid.y + context->y;
    x1 = context->origin->player->grid.x - context->x;
    x2 = context->origin->player->grid.x + context->x;

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan the dungeon */
    do
    {
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Detect stairs */
        if (square_isstairs(context->cave, &iter.cur))
        {
            /* Memorize */
            square_memorize(context->origin->player, context->cave, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            /* Obvious */
            stairs = true;

            redraw = true;
        }

        /* Forget unknown stairs in the mapping area */
        if (square_isstairs_p(context->origin->player, &iter.cur) &&
            square_isnotknown(context->origin->player, context->cave, &iter.cur))
        {
            square_forget(context->origin->player, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            redraw = true;
        }
    }
    while (loc_iterator_next(&iter));

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
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_TRAPS(effect_handler_context_t *context)
{
    int x1, x2, y1, y2;
    bool detect = false, redraw = false;
    struct object *obj;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Pick an area to map */
    y1 = context->origin->player->grid.y - context->y;
    y2 = context->origin->player->grid.y + context->y;
    x1 = context->origin->player->grid.x - context->x;
    x2 = context->origin->player->grid.x + context->x;

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan the dungeon */
    do
    {
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Detect traps */
        if (square_isplayertrap(context->cave, &iter.cur))
        {
            /* Reveal trap */
            if (square_reveal_trap(context->origin->player, &iter.cur, true, false))
            {
                /* We found something to detect */
                detect = true;

                redraw = true;
            }
        }

        /* Forget unknown traps in the mapping area */
        if (!square_top_trap(context->cave, &iter.cur))
        {
            square_forget_trap(context->origin->player, &iter.cur);

            redraw = true;
        }

        /* Scan all objects in the grid to look for traps on chests */
        for (obj = square_object(context->cave, &iter.cur); obj; obj = obj->next)
        {
            /* Skip anything not a trapped chest */
            if (!is_trapped_chest(obj)) continue;

            /* Identify once */
            if (!object_is_known(context->origin->player, obj))
            {
                /* Hack -- know the pile */
                square_know_pile(context->origin->player, context->cave, &iter.cur);

                /* Know the trap */
                object_notice_everything_aux(context->origin->player, obj, true, false);

                /* Notice */
                if (!ignore_item_ok(context->origin->player, obj))
                {
                    /* Notice it */
                    disturb(context->origin->player);

                    /* We found something to detect */
                    detect = true;
                }
            }
        }

        /* Mark as trap-detected */
        sqinfo_on(square_p(context->origin->player, &iter.cur)->info, SQUARE_DTRAP);
    }
    while (loc_iterator_next(&iter));

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
 * is context->y, the width either side of the player context->x.
 *
 * PWMAngband: set context->radius for full detection
 */
static bool effect_handler_DETECT_TREASURES(effect_handler_context_t *context)
{
    int x1, x2, y1, y2;
    bool gold_buried = false, objects = false;
    bool full = (context->radius? true: false);
    struct loc begin, end;
    struct loc_iterator iter;

    /* Hack -- DM has full detection */
    if (context->origin->player->dm_flags & DM_SEE_LEVEL) full = true;

    /* Pick an area to map */
    y1 = context->origin->player->grid.y - context->y;
    y2 = context->origin->player->grid.y + context->y;
    x1 = context->origin->player->grid.x - context->x;
    x2 = context->origin->player->grid.x + context->x;

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan the dungeon */
    do
    {
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Magma/Quartz + Known Gold */
        if (square_hasgoldvein(context->cave, &iter.cur))
        {
            /* Memorize */
            square_memorize(context->origin->player, context->cave, &iter.cur);
            square_light_spot(context->cave, &iter.cur);

            /* Detect */
            gold_buried = true;
        }

        /* Forget unknown gold in the mapping area */
        if (square_hasgoldvein_p(context->origin->player, &iter.cur) &&
            square_isnotknown(context->origin->player, context->cave, &iter.cur))
        {
            square_forget(context->origin->player, &iter.cur);
            square_light_spot(context->cave, &iter.cur);
        }
    }
    while (loc_iterator_next(&iter));

    loc_iterator_first(&iter, &begin, &end);

    /* Scan the area for objects */
    do
    {
        struct object *obj;

        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        obj = square_object(context->cave, &iter.cur);

        /* Skip empty grids */
        if (!obj)
        {
            square_forget_pile(context->origin->player, &iter.cur);
            continue;
        }

        /* Detect */
        if (!ignore_item_ok(context->origin->player, obj) || !full) objects = true;

        /* Memorize the pile */
        if (full) square_know_pile(context->origin->player, context->cave, &iter.cur);
        else square_sense_pile(context->origin->player, context->cave, &iter.cur);
        square_light_spot(context->cave, &iter.cur);
    }
    while (loc_iterator_next(&iter));

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
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_DETECT_VISIBLE_MONSTERS(effect_handler_context_t *context)
{
    detect_monsters_normal(context->origin->player, context->y, context->x, true, context->aware);

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
            project(who, 2, context->cave, &mon->grid, 20, PROJ_MON_SLOW, p_flag, 0, 0, "killed");

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
            project(who, 2, context->cave, &mon->grid, mon->level,
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
        struct loc *decoy = cave_find_decoy(context->cave);

        seen = (!context->origin->player->timed[TMD_BLIND] &&
            monster_is_visible(context->origin->player, mon->midx));
        source_monster(who, mon);

        /* Target is another monster - disenchant it */
        if (context->target_mon)
        {
            char m_name[NORMAL_WID];

            /* Affects only casters */
            if (!context->target_mon->race->freq_spell) return true;

            monster_desc(context->origin->player, m_name, sizeof(m_name), mon, MDESC_STANDARD);

            /* Attack power, capped vs monster level */
            if (drain > (context->target_mon->level / 6) + 1)
                drain = (context->target_mon->level / 6) + 1;

            mon_inc_timed(context->origin->player, context->target_mon, MON_TMD_DISEN,
                MAX(drain, 0), 0);

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

        /* Target was a decoy - destroy it */
        if (!loc_is_zero(decoy))
        {
            square_destroy_decoy(context->origin->player, context->cave, decoy);
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
 * Drain a stat temporarily. The stat index is context->subtype.
 */
static bool effect_handler_DRAIN_STAT(effect_handler_context_t *context)
{
    int stat = context->subtype;
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
 * PWMAngband: the radius can be set in context->value.base (Minor Havoc); if
 * context->origin->monster is set, quake the area silently around the monster
 */
static bool effect_handler_EARTHQUAKE(effect_handler_context_t *context)
{
    int r = effect_calculate_value(context, false);
    bool targeted = (context->subtype? true: false);
    int i, y, x, j;
    struct loc offset, centre, safe_grid;
    int safe_grids = 0;
    int damage = 0;
    int hurt[MAX_PLAYERS];
    bool map[32][32];
    int count = 0;

    loc_init(&safe_grid, 0, 0);

    if (context->radius) r = context->radius;
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
    {
        msg(context->origin->player, "The ground shakes! The ceiling caves in!");
        msg_misc(context->origin->player, " causes the ground to shake!");
    }

    /* Sometimes ask for a target */
    if (targeted)
    {
        /* Ensure "dir" is in ddx/ddy array bounds */
        if (!VALID_DIR(context->dir)) return false;

        /* Ask for a target if no direction given */
        if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
            target_get(context->origin->player, &centre);
    }

    /* Paranoia -- enforce maximum range */
    if (r > 15) r = 15;

    /* Initialize a map of the maximal blast area */
    for (y = 0; y < 32; y++)
    {
        for (x = 0; x < 32; x++) map[y][x] = false;
    }

    /* Check around the epicenter */
    for (offset.y = -r; offset.y <= r; offset.y++)
    {
        for (offset.x = -r; offset.x <= r; offset.x++)
        {
            struct loc grid;

            /* Extract the location */
            loc_sum(&grid, &centre, &offset);

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, &grid)) continue;

            /* Skip distant grids */
            if (distance(&centre, &grid) > r) continue;

            /* Take note of any player */
            if (square(context->cave, &grid)->mon < 0)
            {
                hurt[count] = square(context->cave, &grid)->mon;
                count++;
            }

            /* Lose room and vault */
            sqinfo_off(square(context->cave, &grid)->info, SQUARE_ROOM);
            sqinfo_off(square(context->cave, &grid)->info, SQUARE_VAULT);
            sqinfo_off(square(context->cave, &grid)->info, SQUARE_NO_TELEPORT);
            if (square_ispitfloor(context->cave, &grid)) square_clear_feat(context->cave, &grid);

            /* Forget completely */
            square_unglow(context->cave, &grid);
            square_forget_all(context->cave, &grid);
            square_light_spot(context->cave, &grid);

            /* Skip the epicenter */
            if (loc_is_zero(&offset)) continue;

            /* Skip most grids */
            if (magik(85)) continue;

            /* Damage this grid */
            map[16 + offset.y][16 + offset.x] = true;

            /* Take note of player damage */
            if (square(context->cave, &grid)->mon < 0) hurt[count - 1] = 0 - hurt[count - 1];
        }
    }

    /* First, affect the players (if necessary) */
    for (j = 0; j < count; j++)
    {
        struct player *player;

        /* Skip undamaged players */
        if (hurt[j] < 0) continue;

        player = player_get(hurt[j]);

        safe_grids = 0; damage = 0;
        loc_init(&safe_grid, 0, 0);

        /* Check around the player */
        for (i = 0; i < 8; i++)
        {
            struct loc grid;

            /* Get the location */
            loc_sum(&grid, &player->grid, &ddgrid_ddd[i]);

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, &grid)) continue;

            /* Skip non-empty grids - allow pushing into traps and webs */
            if (!square_isopen(context->cave, &grid)) continue;

            /* Important -- skip grids marked for damage */
            if (map[16 + grid.y - centre.y][16 + grid.x - centre.x]) continue;

            /* Count "safe" grids, apply the randomizer */
            if ((++safe_grids > 1) && randint0(safe_grids)) continue;

            /* Save the safe location */
            loc_copy(&safe_grid, &grid);
        }

        /* Random message */
        switch (randint1(3))
        {
            case 1:
            {
                msg(player, "The cave ceiling collapses on you!");
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
            monster_swap(context->cave, &player->grid, &safe_grid);
        }

        /* Take some damage */
        if (damage)
            take_hit(player, damage, "an earthquake", false, "was crushed by tons of falling rocks");
    }

    /* Examine the quaked region */
    for (offset.y = -r; offset.y <= r; offset.y++)
    {
        for (offset.x = -r; offset.x <= r; offset.x++)
        {
            struct loc grid;

            /* Extract the location */
            loc_sum(&grid, &centre, &offset);

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, &grid)) continue;

            /* Skip unaffected grids */
            if (!map[16 + offset.y][16 + offset.x]) continue;

            /* Process monsters */
            if (square(context->cave, &grid)->mon > 0)
            {
                struct monster *mon = square_monster(context->cave, &grid);

                /* Most monsters cannot co-exist with rock */
                if (!monster_passes_walls(mon->race))
                {
                    /* Assume not safe */
                    safe_grids = 0;

                    /* Monster can move to escape the wall */
                    if (!rf_has(mon->race->flags, RF_NEVER_MOVE))
                    {
                        /* Look for safety */
                        for (i = 0; i < 8; i++)
                        {
                            struct loc safe;

                            /* Get the grid */
                            loc_sum(&safe, &grid, &ddgrid_ddd[i]);

                            /* Skip illegal grids */
                            if (!square_in_bounds_fully(context->cave, &safe)) continue;

                            /* Skip non-empty grids */
                            if (!square_isempty(context->cave, &safe)) continue;

                            /* No safety on glyph of warding */
                            if (square_iswarded(context->cave, &safe)) continue;

                            /* Important -- skip "quake" grids */
                            if (map[16 + safe.y - centre.y][16 + safe.x - centre.x]) continue;

                            /* Count "safe" grids, apply the randomizer */
                            if ((++safe_grids > 1) && randint0(safe_grids)) continue;

                            /* Save the safe grid */
                            loc_copy(&safe_grid, &safe);
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

                    /* Monster is certainly awake, not thinking about player */
                    monster_wake(context->origin->player, mon, false, 0);
                    mon_clear_timed(context->origin->player, mon, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY);

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
                        delete_monster(context->cave, &grid);
                        if (square_ispitfloor(context->cave, &grid))
                            square_clear_feat(context->cave, &grid);

                        /* No longer safe */
                        safe_grids = 0;
                    }

                    /* Escape from the rock */
                    if (safe_grids)
                    {
                        /* Move the monster */
                        monster_swap(context->cave, &grid, &safe_grid);
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

        map[16 + player->grid.y - centre.y][16 + player->grid.x - centre.x] = false;
    }

    /* Examine the quaked region and damage marked grids if possible */
    for (offset.y = -r; offset.y <= r; offset.y++)
    {
        for (offset.x = -r; offset.x <= r; offset.x++)
        {
            struct loc grid;

            /* Extract the location */
            loc_sum(&grid, &centre, &offset);

            /* Skip illegal grids */
            if (!square_in_bounds_fully(context->cave, &grid)) continue;

            /* Note unaffected grids for light changes, etc. */
            if (!map[16 + offset.y][16 + offset.x]) square_light_spot(context->cave, &grid);

            /* Destroy location and all objects (if valid) */
            else if (square_changeable(context->cave, &grid))
            {
                square_forget_pile_all(context->cave, &grid);
                square_excise_pile(context->cave, &grid);
                square_earthquake(context->cave, &grid);
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
 * Note that armour, to hit or to dam is controlled by context->subtype
 *
 * PWMAngband: set context->radius to prevent items from becoming "worthless" (anti-cheeze)
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
        get_item(context->origin->player,
            ((context->subtype == ENCH_TOAC)? HOOK_ARMOR: HOOK_WEAPON), "");
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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
        return false;
    }

    /* Assume enchant weapon */
    item_tester = item_tester_hook_weapon;

    /* Enchant armor if requested */
    if (context->subtype == ENCH_TOAC) item_tester = item_tester_hook_armour;

    /* Paranoia: requires proper item */
    if (!item_tester(obj)) return false;

    /* Description */
    object_desc(context->origin->player, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Describe */
    msg(context->origin->player, "%s %s glow%s brightly!",
        (object_is_carried(context->origin->player, obj) ? "Your" : "The"), o_name,
        SINGULAR(obj->number));

    /* Enchant */
    if (!enchant(context->origin->player, obj, value, context->subtype))
    {
        /* Failure */
        msg(context->origin->player, "The enchantment failed.");
    }

    /* Endless source of cash? No way... make them worthless */
    else if (!context->radius)
    {
        set_origin(obj, ORIGIN_WORTHLESS, context->origin->player->wpos.depth, NULL);
        if (object_was_sensed(obj) || object_is_known(context->origin->player, obj))
            context->origin->player->upkeep->notice |= PN_IGNORE;
    }

    /* Redraw */
    if (!object_is_carried(context->origin->player, obj))
        redraw_floor(&context->origin->player->wpos, &obj->grid, NULL);

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


static bool effect_handler_GAIN_EXP(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->origin->player->exp < PY_MAX_EXP)
    {
        s32b ee = (context->origin->player->exp / 2) + 10;

        if (ee > amount) ee = amount;
        msg(context->origin->player, "You feel more experienced.");
        player_exp_gain(context->origin->player, context->subtype? ee: amount);
    }
    context->ident = true;

    return true;
}


/*
 * Gain a stat point. The stat index is context->subtype.
 */
static bool effect_handler_GAIN_STAT(effect_handler_context_t *context)
{
    int stat = context->subtype;

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


static bool effect_handler_GRANITE(effect_handler_context_t *context)
{
    struct trap *trap = context->origin->trap;

    square_add_wall(context->cave, &trap->grid);

    context->origin->player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    context->origin->player->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);

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

    context->self_msg = NULL;
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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
        return false;
    }

    /* Paranoia: requires identifiable item */
    if (object_runes_known(obj)) return false;

    /* Identify the object */
    object_learn_unknown_rune(context->origin->player, obj);
    if (!object_is_carried(context->origin->player, obj))
        redraw_floor(&context->origin->player->wpos, &obj->grid, NULL);

    /* Something happened */
    return true;
}


/*
 * Crack a whip, or spit at the player; actually just a finite length beam
 * Affect grids, objects, and monsters
 * context->radius is length of beam
 */
static bool effect_handler_LASH(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = context->radius;
    int flg = PROJECT_ARC | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    int i, type;
    struct loc target;
    struct source who_body;
    struct source *who = &who_body;
    int diameter_of_source;
    const struct monster_race *race = NULL;

    /* Paranoia */
    if (rad > z_info->max_range) rad = z_info->max_range;

    /*
     * Diameter of source is the same as the radius, so the effect is
     * essentially full strength for its entire length.
     */
    diameter_of_source = rad;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Player or monster? */
    if (context->origin->monster)
    {
        source_monster(who, context->origin->monster);

        /* Target player or monster? */
        if (context->target_mon)
            loc_copy(&target, &context->target_mon->grid);
        else
        {
            struct loc *decoy = cave_find_decoy(context->cave);

            if (!loc_is_zero(decoy))
                loc_copy(&target, decoy);
            else
                loc_copy(&target, &context->origin->player->grid);
            who->target = context->origin->player;
        }

        race = context->origin->monster->race;
    }
    else if (context->origin->player->poly_race)
    {
        /* Handle polymorphed players */
        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        /* Ask for a target if no direction given */
        if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
            target_get(context->origin->player, &target);
        else
        {
            /* Hack -- no target available, default to random direction */
            if (context->dir == DIR_TARGET) context->dir = 0;

            /* Hack -- no direction given, default to random direction */
            if (!context->dir) context->dir = ddd[randint0(8)];

            /* Use the given direction */
            next_grid(&target, &context->origin->player->grid, context->dir);
        }

        race = context->origin->player->poly_race;
    }

    if (!race) return false;

    /* Get the type (default is PROJ_MISSILE) */
    type = race->blow[0].effect->lash_type;

    /* Scan through all blows for damage */
    for (i = 0; i < z_info->mon_blows_max; i++)
    {
        /* Extract the attack infomation */
        random_value dice = race->blow[i].dice;

        /* Full damage of first blow, plus half damage of others */
        dam += randcalc(dice, race->level, RANDOMISE) / (i? 2: 1);
    }

    /* No damaging blows */
    if (!dam) return false;

    /* Check bounds */
    if (diameter_of_source > 25) diameter_of_source = 25;

    /* Lash the target */
    context->origin->player->current_sound = -2;
    if (project(who, rad, context->cave, &target, dam, type, flg, 0, diameter_of_source,
        "lashed"))
    {
        context->ident = true;
    }
    context->origin->player->current_sound = -1;

    return true;
}


/*
 * Call light around the player
 */
static bool effect_handler_LIGHT_AREA(effect_handler_context_t *context)
{
    /* Message */
    if (!context->origin->player->timed[TMD_BLIND])
        msg(context->origin->player, "You are surrounded by a white light.");

    /* Hack -- elementalists */
    if (context->beam.spell_power)
    {
        int rad = effect_calculate_value(context, false);
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        /* Hook into the "project()" function */
        context->origin->player->current_sound = -2;
        project(who, rad, context->cave, &context->origin->player->grid, 0, PROJ_LIGHT_WEAK,
            PROJECT_GRID, 0, 0, "killed");
        context->origin->player->current_sound = -1;
    }

    /* Light up the room */
    light_room(context->origin->player, context->cave, &context->origin->player->grid, true);

    /* Assume seen */
    context->ident = true;
    return true;
}


static bool effect_handler_LIGHT_LEVEL(effect_handler_context_t *context)
{
    bool full = (context->radius? true: false);

    if (full)
        msg(context->origin->player, "An image of your surroundings forms in your mind...");
    wiz_light(context->origin->player, context->cave, full);
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
        if (light_line_aux(context->origin, context->dir, context->subtype, dam))
            context->ident = true;
    }

    context->self_msg = NULL;
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
 * in context->subtype.
 */
static bool effect_handler_LOSE_RANDOM_STAT(effect_handler_context_t *context)
{
    int safe_stat = context->subtype;
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
 * is context->y, the width either side of the player context->x.
 */
static bool effect_handler_MAP_AREA(effect_handler_context_t *context)
{
    int i;
    int x1, x2, y1, y2;
    struct loc centre;
    struct loc begin, end;
    struct loc_iterator iter;

    origin_get_loc(&centre, context->origin);

    /* Pick an area to map */
    y1 = centre.y - context->y;
    y2 = centre.y + context->y;
    x1 = centre.x - context->x;
    x2 = centre.x + context->x;

    /* Drag the co-ordinates into the dungeon */
    if (y1 < 0) y1 = 0;
    if (y2 > context->cave->height - 1) y2 = context->cave->height - 1;
    if (x1 < 0) x1 = 0;
    if (x2 > context->cave->width - 1) x2 = context->cave->width - 1;

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Scan the dungeon */
    do
    {
        /* Some squares can't be mapped */
        if (square_isno_map(context->cave, &iter.cur)) continue;

        /* All non-walls are "checked" */
        if (!square_seemslikewall(context->cave, &iter.cur))
        {
            if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

            /* Memorize normal features, mark grids as processed */
            if (square_isnormal(context->cave, &iter.cur))
            {
                square_memorize(context->origin->player, context->cave, &iter.cur);
                square_mark(context->origin->player, &iter.cur);
            }

            /* Memorize known walls */
            for (i = 0; i < 8; i++)
            {
                struct loc a_grid;

                loc_sum(&a_grid, &iter.cur, &ddgrid_ddd[i]);

                /* Memorize walls (etc), mark grids as processed */
                if (square_seemslikewall(context->cave, &a_grid))
                {
                    square_memorize(context->origin->player, context->cave, &a_grid);
                    square_mark(context->origin->player, &a_grid);
                }
            }
        }

        /* Forget unprocessed, unknown grids in the mapping area */
        if (!square_ismark(context->origin->player, &iter.cur) &&
            square_isnotknown(context->origin->player, context->cave, &iter.cur))
        {
            square_forget(context->origin->player, &iter.cur);
        }
    }
    while (loc_iterator_next(&iter));

    loc_init(&begin, x1 - 1, y1 - 1);
    loc_init(&end, x2 + 1, y2 + 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Unmark grids */
    do
    {
        if (!square_in_bounds(context->cave, &iter.cur)) continue;
        square_unmark(context->origin->player, &iter.cur);
    }
    while (loc_iterator_next(&iter));

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
    int x, y;
    struct worldpos wpos;
    char buf[NORMAL_WID];
    struct loc begin, end;
    struct loc_iterator iter;

    int max_radius = radius_wild - 1;

    /* Default to magic map if no wilderness */
    if ((cfg_diving_mode > 1) || OPT(context->origin->player, birth_no_recall))
    {
        effect_handler_MAP_AREA(context);
        return true;
    }

    /* Pick an area to map */
    y = randint0(2 * max_radius + 1) - max_radius;
    x = randint0(2 * max_radius + 1) - max_radius;

    loc_init(&begin, x - 1, y - 1);
    loc_init(&end, x + 1, y + 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Update the wilderness map around that area */
    do
    {
        wpos_init(&wpos, &iter.cur, 0);
        wild_set_explored(context->origin->player, &wpos);
    }
    while (loc_iterator_next(&iter));
    wild_cat_depth(&wpos, buf, sizeof(buf));
    msg(context->origin->player, "You suddenly know more about the area around %s.", buf);

    /* Notice */
    context->ident = true;

    return true;
}


/*
 * Delete all nearby (non-unique) monsters. The radius of effect is
 * context->radius if passed, otherwise the player view radius.
 */
static bool effect_handler_MASS_BANISH(effect_handler_context_t *context)
{
    int i;
    int radius = (context->radius? context->radius: z_info->max_sight);
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
        d = distance(&context->origin->player->grid, &mon->grid);
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


static bool py_attack_grid(struct player *p, struct chunk *c, struct loc *grid)
{
    struct source who_body;
    struct source *who = &who_body;
    int oldhp, newhp;

    square_actor(c, grid, who);

    /* Monster */
    if (who->monster)
    {
        /* Reveal mimics */
        if (monster_is_camouflaged(who->monster))
        {
            become_aware(p, c, who->monster);

            /* Mimic wakes up and becomes aware */
            if (pvm_check(p, who->monster))
                monster_wake(p, who->monster, false, 100);
        }

        oldhp = who->monster->hp;

        /* Attack */
        if (pvm_check(p, who->monster)) py_attack(p, c, grid);

        newhp = who->monster->hp;
    }

    /* Player */
    else if (who->player)
    {
        /* Reveal mimics */
        if (who->player->k_idx)
            aware_player(p, who->player);

        oldhp = who->player->chp;

        /* Attack */
        if (pvp_check(p, who->player, PVP_DIRECT, true, square(c, grid)->feat))
            py_attack(p, c, grid);

        newhp = who->player->chp;
    }

    /* Nobody */
    else
        return false;

    /* Lame test for hitting the target */
    return ((newhp > 0) && (newhp != oldhp));
}


static bool effect_handler_MELEE_BLOWS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    struct loc target;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    next_grid(&target, &context->origin->player->grid, context->dir);

    /* Hack -- use an actual "target" */
    if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
    {
        target_get(context->origin->player, &target);

        /* Check distance */
        if (distance(&context->origin->player->grid, &target) > 1)
        {
            msg(context->origin->player, "Target out of range.");
            context->ident = true;
            return true;
        }
    }

    if (py_attack_grid(context->origin->player, context->cave, &target))
    {
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        /* Analyze the "dir" and the "target", do NOT explode */
        if (project(who, 0, context->cave, &target, dam, context->subtype,
            PROJECT_GRID | PROJECT_KILL | PROJECT_PLAY, 0, 0, "annihilated"))
        {
            context->ident = true;
        }
    }

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

    mon_inc_timed(context->origin->player, context->origin->monster, context->subtype,
        MAX(amount, 0), 0);
    context->ident = true;
    return true;
}


/*
 * Feed the player, or set their satiety level.
 */
static bool effect_handler_NOURISH(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->self_msg && !player_undead(context->origin->player))
        msg(context->origin->player, context->self_msg);

    amount *= z_info->food_value;

    /* Increase food level by amount */
    if (context->subtype == 0)
        player_inc_timed(context->origin->player, TMD_FOOD, MAX(amount, 0), false, false);

    /* Decrease food level by amount */
    else if (context->subtype == 1)
        player_dec_timed(context->origin->player, TMD_FOOD, MAX(amount, 0), false);

    /* Set food level to amount, vomiting if necessary */
    else if (context->subtype == 2)
    {
        bool message = (context->origin->player->timed[TMD_FOOD] > amount);

        if (message) msg(context->origin->player, "You vomit!");
        player_set_timed(context->origin->player, TMD_FOOD, MAX(amount, 0), false);
    }

    /* Increase food level to amount if needed */
    else if ((context->subtype == 3) && (context->origin->player->timed[TMD_FOOD] < amount))
        player_set_timed(context->origin->player, TMD_FOOD, MAX(amount + 1, 0), false);

    context->ident = true;
    context->self_msg = NULL;
    return true;
}


static bool effect_handler_POLY_RACE(effect_handler_context_t *context)
{
    struct monster_race *race = &r_info[context->boost];

    context->ident = true;

    /* Restrict */
    if (context->origin->player->ghost || player_has(context->origin->player, PF_DRAGON) ||
        player_has(context->origin->player, PF_HYDRA) ||
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
        d = distance(&context->origin->player->grid, &mon->grid);
        if (d > z_info->max_sight) continue;

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
    if (!context->target_mon) return true;

    source_monster(who, context->origin->monster);
    project(who, 0, context->cave, &context->target_mon->grid, dam, context->subtype, flag, 0, 0,
        "annihilated");

    return true;
}


/*
 * Apply a "project()" directly to all viewable monsters. If context->other is
 * set, the effect damage boost is applied.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool effect_handler_PROJECT_LOS(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, context->other? true: false);
    int typ = context->subtype;

    project_los(context, typ, dam, false);
    context->ident = true;
    return true;
}


/*
 * Apply a "project()" directly to all viewable monsters. If context->other is
 * set, the effect damage boost is applied.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool effect_handler_PROJECT_LOS_AWARE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, context->other? true: false);
    int typ = context->subtype;

    if (context->self_msg && project_los(context, typ, dam, context->aware))
        msg(context->origin->player, context->self_msg);

    context->ident = true;
    context->self_msg = NULL;
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
 * Map an area around the recently detected monsters.
 * The height to map above and below each monster is context->y,
 * the width either side of each monster context->x.
 * For player level dependent areas, we use the hack of applying value dice
 * and sides as the height and width.
 */
static bool effect_handler_READ_MINDS(effect_handler_context_t *context)
{
    int i;
    int dist_y = (context->y? context->y: context->value.dice);
    int dist_x = (context->x? context->x: context->value.sides);
    bool found = false;
    struct source who_body;
    struct source *who = &who_body;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Detect all appropriate monsters */
        if (context->origin->player->mon_det[i])
        {
            source_both(who, context->origin->player, mon);
            effect_simple(EF_MAP_AREA, who, "0", 0, 0, 0, dist_y, dist_x, NULL);
            found = true;
        }
    }

    if (found)
    {
        msg(context->origin->player, "Images form in your mind!");
        context->ident = true;
        return true;
    }

    return false;
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
    if (((cfg_diving_mode == 3) || OPT(context->origin->player, birth_no_recall)) &&
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
        if (!set_recall_depth(context->origin->player, context->note,
            context->origin->player->current_value, context->beam.inscription))
        {
            return false;
        }

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
    int i, t;
    int strength = effect_calculate_value(context, false);
    struct object *obj;
    bool carried;
    struct loc grid;
    char dice_string[20];

    /* Immediately obvious */
    context->ident = true;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        /* Get the dice string (to show recharge failure rates) */
        strnfmt(dice_string, sizeof(dice_string), "%d", strength);

        get_item(context->origin->player, HOOK_RECHARGE, dice_string);
        return false;
    }

    /* Use current */
    obj = object_from_index(context->origin->player, context->origin->player->current_value, true,
        true);

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Save object info (backfire may destroy it) */
    carried = object_is_carried(context->origin->player, obj);
    loc_copy(&grid, &obj->grid);

    /* Restricted by choice */
    if (!carried && !is_owner(context->origin->player, obj))
    {
        msg(context->origin->player, "This item belongs to someone else!");
        return false;
    }

    /* Must meet level requirement */
    if (!carried && !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
        return false;
    }

    /* Paranoia: requires rechargeable item */
    if (!item_tester_hook_recharge(obj)) return false;

    /* Chance of failure */
    i = recharge_failure_chance(obj, strength);

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
        int ease_of_recharge = (100 - obj->kind->level) / 10;

        t = (strength / (10 - ease_of_recharge)) + 1;

        /* Recharge based on the power */
        if (t > 0) obj->pval += 2 + randint1(t);
    }

    /* Combine the pack (later) */
    context->origin->player->upkeep->notice |= (PN_COMBINE);

    /* Redraw */
    context->origin->player->upkeep->redraw |= (PR_INVEN);
    if (!carried) redraw_floor(&context->origin->player->wpos, &grid, NULL);

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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
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

    context->self_msg = NULL;
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
 * Restore a stat. The stat index is context->subtype.
 */
static bool effect_handler_RESTORE_STAT(effect_handler_context_t *context)
{
    int stat = context->subtype;

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
    struct loc begin, end;
    struct loc_iterator iter;

    context->ident = true;

    loc_init(&begin, context->origin->player->grid.x - 1, context->origin->player->grid.y - 1);
    loc_init(&end, context->origin->player->grid.x + 1, context->origin->player->grid.y + 1);
    loc_iterator_first(&iter, &begin, &end);

    do
    {
        int m_idx;
        struct player *q;

        if (loc_eq(&iter.cur, &context->origin->player->grid)) continue;

        m_idx = square(context->cave, &iter.cur)->mon;
        if (m_idx >= 0) continue;
        if (!square_ispassable(context->cave, &iter.cur)) continue;

        q = player_get(0 - m_idx);
        if (q->ghost && !player_can_undead(q))
        {
            resurrect_player(q, context->cave);
            return true;
        }
    }
    while (loc_iterator_next(&iter));

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
    int open_grids = count_feats(context->origin->player, context->cave, NULL, square_isempty, false);

    /* Avoid infinite loops */
    int iterations = 0;

    if (rubble_grids > open_grids) rubble_grids = open_grids;

    while ((rubble_grids > 0) && (iterations < 10))
    {
        int d;

        /* Look around the player */
        for (d = 0; d < 8; d++)
        {
            struct loc grid;

            /* Extract adjacent (legal) location */
            loc_sum(&grid, &context->origin->player->grid, &ddgrid_ddd[d]);

            if (!square_in_bounds_fully(context->cave, &grid)) continue;
            if (!square_isempty(context->cave, &grid)) continue;

            if (one_in_(3))
            {
                if (one_in_(2))
                    square_set_rubble(context->cave, &grid, FEAT_PASS_RUBBLE);
                else
                    square_set_rubble(context->cave, &grid, FEAT_RUBBLE);
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
 * Create a glyph.
 */
static bool effect_handler_GLYPH(effect_handler_context_t *context)
{
    struct loc *decoy = cave_find_decoy(context->cave);

    /* Hack -- already used up */
    bool used = (context->radius == 1);

    /* Always notice */
    context->ident = true;

    /* Only one decoy at a time */
    if (!loc_is_zero(decoy) && (context->subtype == GLYPH_DECOY))
    {
        msg(context->origin->player, "You can only deploy one decoy at a time.");
        return false;
    }

    /* Only on random levels */
    if (!random_level(&context->cave->wpos))
    {
        msg(context->origin->player, "You cannot create glyphs here...");
        return false;
    }

    /* Require clean space */
    if (!square_istrappable(context->cave, &context->origin->player->grid))
    {
        msg(context->origin->player, "There is no clear floor on which to cast the spell.");
        return false;
    }

    /* Push objects off the grid */
    push_object(context->origin->player, context->cave, &context->origin->player->grid);

    /* Create a glyph */
    square_add_glyph(context->cave, &context->origin->player->grid, context->subtype);
    msg_misc(context->origin->player, " lays down a glyph.");

    return !used;
}


static bool effect_handler_SAFE_GUARD(effect_handler_context_t *context)
{
    int rad = 2 + (context->origin->player->lev / 20);
    struct loc begin, end;
    struct loc_iterator iter;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "You cannot create glyphs here...");
        return false;
    }

    msg_misc(context->origin->player, " lays down some glyphs of protection.");

    loc_init(&begin, context->origin->player->grid.x - rad, context->origin->player->grid.y - rad);
    loc_init(&end, context->origin->player->grid.x + rad, context->origin->player->grid.y + rad);
    loc_iterator_first(&iter, &begin, &end);

    do
    {
        /* First we must be in the dungeon */
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Is it a naked grid? */
        if (!square_isempty(context->cave, &iter.cur)) continue;

        /* Now we want a circle */
        if (distance(&iter.cur, &context->origin->player->grid) != rad) continue;

        /* Everything ok... then put a glyph */
        square_add_glyph(context->cave, &iter.cur, GLYPH_WARDING);
    }
    while (loc_iterator_next(&iter));

    return true;
}


/*
 * Project from the player's grid at the player, act as a ball
 * Affect the player, grids, objects, and monsters
 */
static bool effect_handler_SPOT(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = context->radius;
    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    bool result;
    struct source who_body;
    struct source *who = &who_body;

    source_trap(who, context->origin->trap);

    /* Aim at the target, explode */
    context->origin->player->current_sound = -2;
    result = project(who, rad, chunk_get(&context->origin->player->wpos),
        &context->origin->player->grid, dam, context->subtype, flg, 0, 0, "annihilated");
    context->origin->player->current_sound = -1;
    if (result) context->ident = true;

    return true;
}


/*
 * Cast a line spell in every direction
 * Stop if we hit a monster, act as a ball
 * Affect grids, objects, and monsters
 *
 * PWMAngband: if context->radius is set, divide the damage by that amount
 */
static bool effect_handler_STAR(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    int i;

    if (context->radius) dam /= context->radius;

    if (context->self_msg && !context->origin->player->timed[TMD_BLIND])
        msg(context->origin->player, context->self_msg);
    for (i = 0; i < 8; i++)
        light_line_aux(context->origin, ddd[i], context->subtype, dam);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;

    context->self_msg = NULL;
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
        fire_ball(context->origin->player, context->subtype, ddd[i], dam, context->radius, false);
    if (!context->origin->player->timed[TMD_BLIND]) context->ident = true;

    context->self_msg = NULL;
    return true;
}


/*
 * Strike the target with a ball from above
 */
static bool effect_handler_STRIKE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, true);
    struct loc target;
    struct source who_body;
    struct source *who = &who_body;
    int flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    const char *what = "annihilated";

    loc_copy(&target, &context->origin->player->grid);
    source_player(who, get_player_index(get_connection(context->origin->player->conn)),
        context->origin->player);

    /* Ask for a target; if no direction given, the player is struck  */
    if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
        target_get(context->origin->player, &target);
    else
    {
        msg(context->origin->player, "You must have a target.");
        return false;
    }

    /* Enforce line of sight */
    if (!projectable(context->origin->player, context->cave, &context->origin->player->grid,
        &target, PROJECT_NONE, true) || !square_isknown(context->origin->player, &target))
    {
        return false;
    }

    /* Aim at the target. Hurt items on floor. */
    context->origin->player->current_sound = -2;
    if (project(who, context->radius, context->cave, &target, dam, context->subtype, flg, 0, 0, what))
        context->ident = true;
    context->origin->player->current_sound = -1;

    return true;
}


/*
 * Summon context->value monsters of context->subtype type.
 *
 * Set context->radius to add an out of depth element.
 *
 * PWMAngband: set context->other to a negative value to get delayed summons (-2 to bypass friendly
 * summons); set context->other to a positive value to set the chance to get friendly summons
 */
static bool effect_handler_SUMMON(effect_handler_context_t *context)
{
    int summon_max = effect_calculate_value(context, false);
    int summon_type = context->subtype;
    int level_boost = context->radius;
    int message_type = summon_message_type(summon_type);
    int count = 0;
    struct loc grid;
    struct worldpos *wpos;
    struct monster *mon = context->origin->monster;

    /* Hack -- no summons in Arena */
    if (mon)
    {
        loc_copy(&grid, &mon->grid);
        wpos = &mon->wpos;
    }
    else
    {
        loc_copy(&grid, &context->origin->player->grid);
        wpos = &context->origin->player->wpos;
    }
    if (pick_arena(wpos, &grid) != -1) return true;

    sound(context->origin->player, message_type);

    /* Monster summon */
    if (mon)
    {
        int rlev = ((mon->race->level >= 1)? mon->race->level: 1);

        /* Set the kin_base if necessary */
        if (summon_type == summon_name_to_idx("KIN")) kin_base = mon->race->base;

        /* Summon them */
        count = summon_monster_aux(context->origin->player, context->cave, &mon->grid, summon_type,
            rlev + level_boost, summon_max, 0, mon);

        /* Summoner failed */
        if (!count) msg(context->origin->player, "But nothing comes.");
    }

    /* Delayed summon */
    else if (context->other < 0)
    {
        int i, chance = 0, mlvl;

        if (check_antisummon(context->origin->player, NULL)) return true;

        /* Summoners may get friendly summons */
        if (player_has(context->origin->player, PF_SUMMON_SPELLS) && (context->other != -2))
            chance = 100;

        /* Summon them */
        mlvl = monster_level(&context->origin->player->wpos);
        for (i = 0; i < summon_max; i++)
        {
            count += summon_specific(context->origin->player, context->cave,
                &context->origin->player->grid, mlvl + level_boost, summon_type, true, one_in_(4),
                chance, NULL);
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
            &context->origin->player->grid, summon_type, mlvl + level_boost, summon_max,
            context->other, NULL);
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
    int dam = effect_calculate_value(context, true);
    int num = context->value.m_bonus;
    struct loc target;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
    struct source who_body;
    struct source *who = &who_body;

    source_player(who, get_player_index(get_connection(context->origin->player->conn)),
        context->origin->player);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(context->dir)) return false;

    /* Use the given direction */
    next_grid(&target, &context->origin->player->grid, context->dir);

    /* Hack -- use an actual "target" */
    if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
        target_get(context->origin->player, &target);

    context->origin->player->current_sound = -2;
    while (num--)
    {
        /* Aim at the target. Hurt items on floor. */
        if (project(who, context->radius, context->cave, &target, dam, context->subtype, flg, 0, 0,
            "annihilated"))
        {
            context->ident = true;
        }
    }
    context->origin->player->current_sound = -1;

    return true;
}


static bool effect_handler_SWEEP(effect_handler_context_t *context)
{
    int d;

    for (d = 0; d < 8; d++)
    {
        struct loc adjacent;

        loc_sum(&adjacent, &context->origin->player->grid, &ddgrid_ddd[d]);
        py_attack_grid(context->origin->player, context->cave, &adjacent);
    }

    return true;
}


/*
 * Draw energy from a magical device
 */
static bool effect_handler_TAP_DEVICE(effect_handler_context_t *context)
{
    int lev;
    int energy = 0;
    struct object *obj;
    bool used = false;

    /* Get an item */
    if (context->origin->player->current_value == ITEM_REQUEST)
    {
        get_item(context->origin->player, HOOK_DRAIN, "");
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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
        return false;
    }

    /* Paranoia: requires rechargeable item */
    if (!item_tester_hook_recharge(obj)) return false;

    /* Extract the object "level" */
    lev = obj->kind->level;

    /* Extract the object's energy */
    energy = (5 + lev) * 3 * obj->pval / 2;

    /* Turn energy into mana. */
    if (energy < 36)
    {
        /* Require a resonable amount of energy */
        msg(context->origin->player, "That item had no useable energy.");
    }
    else
    {
        /* If mana below maximum, increase mana and drain object. */
        if (context->origin->player->csp < context->origin->player->msp)
        {
            int old_num = get_player_num(context->origin->player);

            /* Drain the object. */
            obj->pval = 0;

            /* Combine the pack (later) */
            context->origin->player->upkeep->notice |= (PN_COMBINE);

            /* Redraw */
            context->origin->player->upkeep->redraw |= (PR_INVEN);
            if (!object_is_carried(context->origin->player, obj))
                redraw_floor(&context->origin->player->wpos, &obj->grid, NULL);

            /* Increase mana. */
            context->origin->player->csp += energy / 6;
            if (context->origin->player->csp >= context->origin->player->msp)
            {
                context->origin->player->csp = context->origin->player->msp;
                context->origin->player->csp_frac = 0;
            }

            msg(context->origin->player, "You feel your head clear.");
            used = true;

            /* Hack -- redraw picture */
            redraw_picture(context->origin->player, old_num);

            context->origin->player->upkeep->redraw |= (PR_MANA);
        }
        else
            msg(context->origin->player, "Your mana was already at its maximum. Item not drained.");
    }

    return used;
}


/*
 * Draw energy from a nearby undead
 */
static bool effect_handler_TAP_UNLIFE(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);
    int drain = 0;
    bool fear = false;
    bool dead = false;
    struct source *target_who = &context->origin->player->target.target_who;
    char dice[5];

    context->ident = true;

    /* Need to choose a monster, not just point */
    if (!target_able(context->origin->player, target_who))
    {
        msg(context->origin->player, "No target selected!");
        return false;
    }

    if (target_who->monster)
    {
        char m_name[NORMAL_WID];

        /* Must be undead */
        if (!monster_is_undead(target_who->monster->race))
        {
            msg(context->origin->player, "Nothing happens.");
            return false;
        }

        /* Hurt the monster */
        monster_desc(context->origin->player, m_name, sizeof(m_name), target_who->monster,
            MDESC_DEFAULT);
        msg(context->origin->player, "You draw power from the %s.", m_name);
        drain = MIN(target_who->monster->hp, amount) / 4;
        dead = mon_take_hit(context->origin->player, context->cave, target_who->monster, amount,
            &fear, MON_MSG_DESTROYED);
        if (!dead && monster_is_visible(context->origin->player, target_who->monster->midx))
        {
            if (amount > 0) message_pain(context->origin->player, target_who->monster, amount);
            if (fear)
            {
                add_monster_message(context->origin->player, target_who->monster,
                    MON_MSG_FLEE_IN_TERROR, true);
            }
        }
    }
    else
    {
        char killer[NORMAL_WID];
        char df[160];

        /* Must be undead */
        if (!target_who->player->poly_race ||
            !rf_has(target_who->player->poly_race->flags, RF_UNDEAD))
        {
            msg(context->origin->player, "Nothing happens.");
            return false;
        }

        /* Hurt the player */
        msg(context->origin->player, "You draw power from %s.", target_who->player->name);
        drain = MIN(target_who->player->chp, amount) / 4;
        my_strcpy(killer, context->origin->player->name, sizeof(killer));
        strnfmt(df, sizeof(df), "was killed by %s", killer);
        dead = take_hit(target_who->player, amount, killer, false, df);
        if ((amount > 0) && !dead)
            player_pain(context->origin->player, target_who->player, amount);
    }

    /* Gain mana */
    strnfmt(dice, sizeof(dice), "%d", drain);
    effect_simple(EF_RESTORE_MANA, context->origin, dice, 0, 0, 0, 0, 0, NULL);

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

    /* Must meet level requirement */
    if (!object_is_carried(context->origin->player, obj) &&
        !has_level_req(context->origin->player, obj))
    {
        msg(context->origin->player, "You don't have the required level!");
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
    if (cfg_no_stores || OPT(q, birth_no_stores))
    {
        msg(context->origin->player, "%s cannot be reached.", q->name);
        return false;
    }

    /* Actually teleport the object to the player inventory */
    teled = object_new();
    object_copy(teled, obj);
    assess_object(q, teled, true);
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


static bool allow_teleport(struct chunk *c, struct loc *grid, bool safe_ghost, bool is_player)
{
    /* Just require empty space if teleporting a ghost to safety */
    if (safe_ghost)
    {
        if (square(c, grid)->mon) return false;
    }

    /* Require "naked" floor space */
    else
    {
        if (!square_isempty(c, grid)) return false;
    }

    /* No monster teleport onto glyph of warding */
    if (!is_player && square_iswarded(c, grid)) return false;

    /* No teleporting into vaults and such */
    if (square_isvault(c, grid) || !square_is_monster_walkable(c, grid)) return false;

    return true;
}


/*
 * Teleport player or monster up to context->value.base grids away.
 *
 * If no spaces are readily available, the distance may increase.
 * Try very hard to move the player/monster at least a quarter that distance.
 * Setting context->subtype allows monsters to teleport the player away.
 */
static bool effect_handler_TELEPORT(effect_handler_context_t *context)
{
    struct loc start;
    int dis = context->value.base;
    int pick;
    int num_spots = 0;
    bool is_player = (!context->origin->monster || context->subtype);
    bool safe_ghost = false;
    struct worldpos *wpos;
    int d_min = 0, d_max = 0;
    bool far_location = false;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Hack -- already used up */
    bool used = (context->other == 1);

    context->ident = true;

    /* Monster targeting another monster */
    if (context->target_mon && context->subtype)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->origin->monster);
        project(who, 0, context->cave, &context->target_mon->grid, dis, context->subtype, flag,
            0, 0, "annihilated");

        return !used;
    }

    /* Establish the coordinates to teleport from */
    if (is_player)
    {
        struct loc *decoy = cave_find_decoy(context->cave);

        /* Decoys get destroyed */
        if (!loc_is_zero(decoy) && context->subtype)
        {
            square_destroy_decoy(context->origin->player, context->cave, decoy);
            return !used;
        }

        loc_copy(&start, &context->origin->player->grid);
        safe_ghost = context->origin->player->ghost;
        wpos = &context->origin->player->wpos;
    }
    else
    {
        loc_copy(&start, &context->origin->monster->grid);
        wpos = &context->origin->monster->wpos;
    }

    /* Space-time anchor */
    if (check_st_anchor(wpos, &start) && !safe_ghost)
    {
        if (context->origin->player) msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, &start) && !safe_ghost)
    {
        if (context->origin->player) msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport curse */
    if (is_player && player_of_has(context->origin->player, OF_NO_TELEPORT))
    {
        equip_learn_flag(context->origin->player, OF_NO_TELEPORT);
        msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Don't teleport NO_DEATH monsters */
    if (!is_player && rf_has(context->origin->monster->race->flags, RF_NO_DEATH))
    {
        if (context->origin->player) msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Hack -- hijack teleport in Arena */
    if (is_player && (context->origin->player->arena_num != -1))
    {
        int arena_num = context->origin->player->arena_num;
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);

        effect_simple(EF_TELEPORT_TO, who, "0", 0, 0, 0,
            arenas[arena_num].grid_1.y + 1 + randint1(arenas[arena_num].grid_2.y - arenas[arena_num].grid_1.y - 2),
            arenas[arena_num].grid_1.x + 1 + randint1(arenas[arena_num].grid_2.x - arenas[arena_num].grid_1.x - 2),
            NULL);
        return !used;
    }

    loc_init(&begin, 1, 1);
    loc_init(&end, context->cave->width - 1, context->cave->height - 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Get min/max teleporting distances */
    do
    {
        int d = distance(&iter.cur, &start);

        /* Must move */
        if (d == 0) continue;

        if (!allow_teleport(context->cave, &iter.cur, safe_ghost, is_player)) continue;

        if ((d_min == 0) || (d < d_min)) d_min = d;
        if ((d_max == 0) || (d > d_max)) d_max = d;
    }
    while (loc_iterator_next_strict(&iter));

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
        loc_iterator_first(&iter, &begin, &end);

        do
        {
            int d = distance(&iter.cur, &start);
            int d_old = distance(&iter.cur, &context->origin->player->old_grid);

            /* Enforce distance */
            if ((d < d_min) || (d > d_max)) continue;

            if (!allow_teleport(context->cave, &iter.cur, safe_ghost, is_player))
                continue;

            /* Not too close from previous player location */
            if (d_old < d_min) continue;

            far_location = true;
            break;
        }
        while (loc_iterator_next_strict(&iter));
    }

    loc_iterator_first(&iter, &begin, &end);

    /* Count valid teleport locations */
    do
    {
        int d = distance(&iter.cur, &start);

        /* Enforce distance */
        if ((d < d_min) || (d > d_max)) continue;
        if (far_location)
        {
            int d_old = distance(&iter.cur, &context->origin->player->old_grid);

            if (d_old < d_min) continue;
        }

        if (!allow_teleport(context->cave, &iter.cur, safe_ghost, is_player)) continue;

        num_spots++;
    }
    while (loc_iterator_next_strict(&iter));

    loc_iterator_first(&iter, &begin, &end);

    /* Pick a spot */
    pick = randint0(num_spots);
    do
    {
        int d = distance(&iter.cur, &start);

        /* Enforce distance */
        if ((d < d_min) || (d > d_max)) continue;
        if (far_location)
        {
            int d_old = distance(&iter.cur, &context->origin->player->old_grid);

            if (d_old < d_min) continue;
        }

        if (!allow_teleport(context->cave, &iter.cur, safe_ghost, is_player)) continue;

        pick--;
        if (pick == -1) break;
    }
    while (loc_iterator_next_strict(&iter));

    /* Sound */
    if (context->origin->player)
        sound(context->origin->player, (is_player? MSG_TELEPORT: MSG_TPOTHER));

    /* Report the teleporting before moving the monster */
    if (!is_player)
    {
        add_monster_message(context->origin->player, context->origin->monster, MON_MSG_DISAPPEAR,
            false);
    }

    /* Reveal mimics */
    if (is_player)
    {
        if (context->origin->player->k_idx)
            aware_player(context->origin->player, context->origin->player);
    }
    else
    {
        if (monster_is_camouflaged(context->origin->monster))
            become_aware(context->origin->player, context->cave, context->origin->monster);
    }

    /* Move the target */
    monster_swap(context->cave, &start, &iter.cur);

    /* Clear any projection marker to prevent double processing */
    sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_PROJECT);

    /* Clear monster target if it's no longer visible */
    if (context->origin->player && !is_player &&
        !los(context->cave, &context->origin->player->grid, &iter.cur))
    {
        target_set_monster(context->origin->player, NULL);
    }

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
    struct wild_type *w_ptr = get_wt_info_at(&context->origin->player->wpos.grid);
    char *message;
    struct worldpos wpos;
    byte new_level_method;
    struct loc *decoy = cave_find_decoy(context->cave);

    /* Hack -- already used up */
    bool used = (context->radius == 1);

    context->ident = true;

    /* MvM */
    if (context->target_mon)
    {
        int flag = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->origin->monster);
        project(who, 0, context->cave, &context->target_mon->grid, 0, context->subtype, flag, 0, 0,
            "annihilated");

        return !used;
    }

    /* Targeted decoys get destroyed */
    if (!loc_is_zero(decoy) && context->origin->monster)
    {
        square_destroy_decoy(context->origin->player, context->cave, decoy);
        return !used;
    }

    /* Resist hostile teleport */
    if (context->origin->monster && player_resists(context->origin->player, ELEM_NEXUS))
    {
        msg(context->origin->player, "You resist the effect!");
        return !used;
    }

    /* Space-time anchor */
    if (check_st_anchor(&context->origin->player->wpos, &context->origin->player->grid))
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return !used;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, &context->origin->player->grid))
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

    /* If no dungeon or winner-only/shallow dungeon, teleport to a random neighboring level */
    if ((w_ptr->max_depth == 1) || forbid_entrance_weak(context->origin->player) ||
        forbid_entrance_strong(context->origin->player))
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

        wpos_init(&wpos, &neighbor->wpos.grid, 0);
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
        wpos_init(&wpos, &context->origin->player->wpos.grid, target_depth);
        if (chunk_inhibit_players(&wpos))
            up = false;
        target_depth = dungeon_get_next_level(context->origin->player, base_depth, 1);
        wpos_init(&wpos, &context->origin->player->wpos.grid, target_depth);
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

        wpos_init(&wpos, &context->origin->player->wpos.grid, target_depth);
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
 * Teleport player or target monster to a grid near the given location
 * Setting context->y and context->x treats them as y and x coordinates
 * Setting context->subtype allows monsters to teleport toward a target.
 * Hack: setting context->other means we are about to enter an arena
 *
 * This function is slightly obsessive about correctness.
 */
static bool effect_handler_TELEPORT_TO(effect_handler_context_t *context)
{
    struct loc start, aim, land;
    int dis = 0, ctr = 0;
    int tries = 200;
    bool is_player;

    context->ident = true;

    /* Where are we coming from? */
    if (context->subtype)
    {
        /* Monster teleporting */
        loc_copy(&start, &context->origin->monster->grid);
        is_player = false;
    }
    else if (context->target_mon)
    {
        /* Monster being teleported */
        loc_copy(&start, &context->target_mon->grid);
        is_player = false;
    }
    else
    {
        struct loc *decoy = cave_find_decoy(context->cave);

        /* Targeted decoys get destroyed */
        if (!loc_is_zero(decoy) && context->origin->monster)
        {
            square_destroy_decoy(context->origin->player, context->cave, decoy);
            return true;
        }

        /* Player being teleported */
        loc_copy(&start, &context->origin->player->grid);
        is_player = true;
    }

    /* Where are we going? */
    if (context->y && context->x)
    {
        /* Teleport to player */
        loc_init(&aim, context->x, context->y);
        if (context->origin->monster)
        {
            loc_copy(&start, &context->origin->monster->grid);
            is_player = false;
        }
    }
    else if (context->origin->monster)
    {
        /* Monster teleporting */
        if (context->subtype)
        {
            if (context->target_mon) loc_copy(&aim, &context->target_mon->grid);
            else loc_copy(&aim, &context->origin->player->grid);
        }

        /* Teleport to monster */
        else loc_copy(&aim, &context->origin->monster->grid);
    }
    else
    {
        /* Teleport to target */
        if ((context->dir == DIR_TARGET) && target_okay(context->origin->player))
        {
            int rad = effect_calculate_value(context, false);

            target_get(context->origin->player, &aim);

            if (distance(&aim, &start) > rad)
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
    if (check_st_anchor(&context->origin->player->wpos, &start))
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return true;
    }

    /* Check for a no teleport grid */
    if (square_isno_teleport(context->cave, &start))
    {
        msg(context->origin->player, "The teleporting attempt fails.");
        return true;
    }

    /* Check for a no teleport curse */
    if (is_player && player_of_has(context->origin->player, OF_NO_TELEPORT))
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
            rand_loc(&land, &aim, dis, dis);
            if (square_in_bounds_fully(context->cave, &land)) break;
        }

        /* No teleporting into vaults and such if the target is outside the vault */
        if (square_isvault(context->cave, &land) && !square_isvault(context->cave, &start))
        {
            /* Hack -- we enter an arena by teleporting into it, so allow that */
            if (!context->other) legal = false;
        }

        /* Only accept grids in LOS of the caster */
        if (!los(context->cave, &aim, &land)) legal = false;

        /* Accept legal "naked" floor grids... */
        if (square_isempty(context->cave, &land) && legal) break;

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

    /* Move player or monster */
    monster_swap(context->cave, &start, &land);

    /* Clear any projection marker to prevent double processing */
    sqinfo_off(square(context->cave, &land)->info, SQUARE_PROJECT);

    /* Handle stuff */
    if (is_player) handle_stuff(context->origin->player);

    /* Hack -- fix store */
    if (is_player && in_store(context->origin->player)) Send_store_leave(context->origin->player);

    return true;
}


/*
 * Reduce a (positive or negative) player status condition.
 * If context->other is set, decrease by the current value / context->other
 */
static bool effect_handler_TIMED_DEC(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    if (context->other) amount = context->origin->player->timed[context->subtype] / context->other;
    player_dec_timed(context->origin->player, context->subtype, MAX(amount, 0), true);
    context->ident = true;
	return true;
}


/*
 * Extend a (positive or negative) player status condition.
 * If context->other is set, increase by that amount if the player already
 * has the status
 */
static bool effect_handler_TIMED_INC(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);
    struct loc *decoy = cave_find_decoy(context->cave);

    /* MvM -- irrelevant */
    if (context->target_mon) return true;

    /* Destroy decoy if it's a monster attack */
    if (context->origin->monster && !loc_is_zero(decoy))
    {
        square_destroy_decoy(context->origin->player, context->cave, decoy);
        return true;
    }

    /* Increase by that amount if the status exists already */
    if (context->other && context->origin->player->timed[context->subtype])
        amount = context->other;

    player_inc_timed_aux(context->origin->player, context->origin->monster, context->subtype,
        MAX(amount, 0), true, true);
    context->ident = true;
	return true;
}


/*
 * Extend a (positive or negative) player status condition unresistably.
 * If context->other is set, increase by that amount if the status exists already
 */
static bool effect_handler_TIMED_INC_NO_RES(effect_handler_context_t *context)
{
    int amount = effect_calculate_value(context, false);

    /* Increase by that amount if the status exists already */
    if (context->other && context->origin->player->timed[context->subtype])
        amount = context->other;

    player_inc_timed_aux(context->origin->player, context->origin->monster, context->subtype,
        MAX(amount, 0), true, false);
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
    if (context->subtype == TMD_DEADLY)
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

    player_set_timed(context->origin->player, context->subtype, MAX(amount, 0), true);
    context->ident = true;
    context->self_msg = NULL;
    return true;
}


/*
 * Affect adjacent grids
 *
 * PWMAngband: set context->other to 1 to prevent the effect on static levels
 */
static bool effect_handler_TOUCH(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);
    int rad = (context->radius? context->radius: 1);

    /* Only on random levels */
    if ((context->other == 1) && !random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "Nothing happens.");
        return true;
    }

    /* Monster cast at monster */
    if (context->target_mon)
    {
        int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
        struct source who_body;
        struct source *who = &who_body;

        source_monster(who, context->origin->monster);
        project(who, rad, context->cave, &context->target_mon->grid, 0, context->subtype, flg, 0, 0,
            "killed");
        return true;
    }

    if (project_touch(context->origin->player, dam, rad, context->subtype, false,
        context->origin->monster? true: false))
    {
        context->ident = true;
        if (context->self_msg) msg(context->origin->player, context->self_msg);
    }

    context->self_msg = NULL;
    return true;
}


/*
 * Affect adjacent grids (radius 1 ball attack)
 * Notice stuff based on awareness of the effect
 */
static bool effect_handler_TOUCH_AWARE(effect_handler_context_t *context)
{
    int dam = effect_calculate_value(context, false);

    if (project_touch(context->origin->player, dam, 1, context->subtype, context->aware, false))
        context->ident = true;
    return true;
}


static bool effect_handler_UNDEAD_FORM(effect_handler_context_t *context)
{
    /* Restrict */
    if (context->origin->player->ghost || player_has(context->origin->player, PF_DRAGON) ||
        player_has(context->origin->player, PF_HYDRA) ||
        OPT(context->origin->player, birth_fruit_bat))
    {
        msg(context->origin->player, "You try to turn into an undead being... but nothing happens.");
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
    struct loc origin;

    origin_get_loc(&origin, context->origin);

    /* Wake everyone nearby */
    for (i = 1; i < cave_monster_max(context->cave); i++)
    {
        struct monster *mon = cave_monster(context->cave, i);

        if (mon->race)
        {
            int radius = z_info->max_sight * 2;
            int dist = distance(&origin, &mon->grid);

            /* Skip monsters too far away */
            if ((dist < radius) && mon->m_timed[MON_TMD_SLEEP])
            {
                /* Monster wakes, closer means likelier to become aware */
                monster_wake(context->origin->player, mon, false, 100 - 2 * dist);
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


/*
 * Create a web.
 */
static bool effect_handler_WEB(effect_handler_context_t *context)
{
    int rad = 1;
    struct monster *mon = context->origin->monster;
    struct loc begin, end, grid;
    struct loc_iterator iter;
    int spell_power;

    if (mon)
    {
        spell_power = mon->race->spell_power;
        loc_copy(&grid, &mon->grid);
    }
    else
    {
        spell_power = context->origin->player->lev * 2;
        loc_copy(&grid, &context->origin->player->grid);
    }

    /* Always notice */
    context->ident = true;

    /* Increase the radius for higher spell power */
    if (spell_power > 40) rad++;
    if (spell_power > 80) rad++;

    loc_init(&begin, grid.x - rad, grid.y - rad);
    loc_init(&end, grid.x + rad, grid.y + rad);
    loc_iterator_first(&iter, &begin, &end);

    /* Check within the radius for clear floor */
    do
    {
        /* Skip illegal grids */
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Skip distant grids */
        if (distance(&iter.cur, &grid) > rad) continue;

        /* Require a floor grid with no existing traps or glyphs */
        if (!square_iswebbable(context->cave, &iter.cur)) continue;

        /* Create a web */
        square_add_web(context->cave, &iter.cur);
    }
    while (loc_iterator_next(&iter));

    return true;
}


/* Wipe everything */
static bool effect_handler_WIPE_AREA(effect_handler_context_t *context)
{
    int r = context->radius;
    int k;
    int hurt[MAX_PLAYERS];
    int count = 0;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Only on random levels */
    if (!random_level(&context->origin->player->wpos))
    {
        msg(context->origin->player, "The ground shakes for a moment.");
        return true;
    }

    /* Paranoia -- enforce maximum range */
    if (r > 12) r = 12;

    loc_init(&begin, context->origin->player->grid.x - r, context->origin->player->grid.y - r);
    loc_init(&end, context->origin->player->grid.x + r, context->origin->player->grid.y + r);
    loc_iterator_first(&iter, &begin, &end);

    /* Check around the epicenter */
    do
    {
        /* Skip illegal grids */
        if (!square_in_bounds_fully(context->cave, &iter.cur)) continue;

        /* Skip distant grids */
        if (distance(&context->origin->player->grid, &iter.cur) > r) continue;

        /* Take note of any player */
        if (square(context->cave, &iter.cur)->mon < 0)
        {
            hurt[count] = 0 - square(context->cave, &iter.cur)->mon;
            count++;
        }

        /* Lose room and vault */
        sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_VAULT);
        sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_ROOM);
        sqinfo_off(square(context->cave, &iter.cur)->info, SQUARE_NO_TELEPORT);
        if (square_ispitfloor(context->cave, &iter.cur))
            square_clear_feat(context->cave, &iter.cur);

        /* Forget completely */
        square_unglow(context->cave, &iter.cur);
        square_forget_all(context->cave, &iter.cur);
        square_light_spot(context->cave, &iter.cur);

        /* Delete monsters */
        delete_monster(context->cave, &iter.cur);
        if (square_ispitfloor(context->cave, &iter.cur))
            square_clear_feat(context->cave, &iter.cur);

        /* Destroy "valid" grids */
        if (square_changeable(context->cave, &iter.cur))
        {
            /* Delete objects */
            square_forget_pile_all(context->cave, &iter.cur);
            square_excise_pile(context->cave, &iter.cur);

            /* Turn into basic floor */
            square_clear_feat(context->cave, &iter.cur);
        }
    }
    while (loc_iterator_next(&iter));

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
        new_context.subtype = PROJ_MON_CLONE;
        handler = effect_handler_BOLT;
    }
    else if (die < 14)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.base = 100;
        new_context.subtype = PROJ_MON_SPEED;
        handler = effect_handler_BOLT;
    }
    else if (die < 26)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.value.dice = 4;
        new_context.value.sides = 6;
        new_context.subtype = PROJ_MON_HEAL;
        handler = effect_handler_BOLT;
    }
    else if (die < 31)
    {
        msg_misc(context->origin->player, " discharges an everchanging blast of energy.");
        new_context.aware = false;
        new_context.value.base = plev;
        new_context.subtype = PROJ_MON_POLY;
        new_context.radius = 1;
        handler = effect_handler_BOLT_AWARE;
    }
    else if (die < 36)
    {
        msg_misc(context->origin->player, " fires a magic missile.");
        new_context.value.dice = 3 + (plev - 1) / 5;
        new_context.value.sides = 4;
        new_context.subtype = PROJ_MISSILE;
        new_context.other = -10;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 41)
    {
        msg_misc(context->origin->player, " makes a complicated gesture.");
        new_context.aware = false;
        new_context.value.base = 5;
        new_context.value.dice = 1;
        new_context.value.sides = 5;
        new_context.subtype = PROJ_MON_CONF;
        handler = effect_handler_BOLT_AWARE;
    }
    else if (die < 46)
    {
        msg_misc(context->origin->player, " fires a stinking cloud.");
        new_context.value.base = 20 + plev / 2;
        new_context.subtype = PROJ_POIS;
        new_context.radius = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 51)
    {
        msg_misc(context->origin->player, "'s hands project a line of shimmering blue light.");
        new_context.value.dice = 6;
        new_context.value.sides = 8;
        new_context.subtype = PROJ_LIGHT_WEAK;
        new_context.self_msg = "A line of shimmering blue light appears.";
        handler = effect_handler_LINE;
    }
    else if (die < 56)
    {
        msg_misc(context->origin->player, " fires a lightning bolt.");
        new_context.value.dice = 3 + (plev - 5) / 6;
        new_context.value.sides = 6;
        new_context.subtype = PROJ_ELEC;
        handler = effect_handler_BEAM;
    }
    else if (die < 61)
    {
        msg_misc(context->origin->player, " fires a frost bolt.");
        new_context.value.dice = 5 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.subtype = PROJ_COLD;
        new_context.other = -10;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 66)
    {
        msg_misc(context->origin->player, " fires an acid bolt.");
        new_context.value.dice = 6 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.subtype = PROJ_ACID;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 71)
    {
        msg_misc(context->origin->player, " fires a fire bolt.");
        new_context.value.dice = 8 + (plev - 5) / 4;
        new_context.value.sides = 8;
        new_context.subtype = PROJ_FIRE;
        handler = effect_handler_BOLT_OR_BEAM;
    }
    else if (die < 76)
    {
        msg_misc(context->origin->player, " fires a bolt filled with pure energy!");
        new_context.value.base = 75;
        new_context.subtype = PROJ_MON_DRAIN;
        handler = effect_handler_BOLT;
    }
    else if (die < 81)
    {
        msg_misc(context->origin->player, " fires a lightning ball.");
        new_context.value.base = 30 + plev / 2;
        new_context.subtype = PROJ_ELEC;
        new_context.radius = 2;
        handler = effect_handler_BALL;
    }
    else if (die < 86)
    {
        msg_misc(context->origin->player, " fires an acid ball.");
        new_context.value.base = 40 + plev;
        new_context.subtype = PROJ_ACID;
        new_context.radius = 2;
        handler = effect_handler_BALL;
    }
    else if (die < 91)
    {
        msg_misc(context->origin->player, " fires an ice ball.");
        new_context.value.base = 70 + plev;
        new_context.subtype = PROJ_ICE;
        new_context.radius = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 96)
    {
        msg_misc(context->origin->player, " fires a fire ball.");
        new_context.value.base = 80 + plev;
        new_context.subtype = PROJ_FIRE;
        new_context.radius = 3;
        handler = effect_handler_BALL;
    }
    else if (die < 101)
    {
        msg_misc(context->origin->player, " fires a massive bolt filled with pure energy!");
        new_context.value.base = 100 + plev;
        new_context.subtype = PROJ_MON_DRAIN;
        handler = effect_handler_BOLT;
    }
    else if (die < 104)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.radius = 12;
        handler = effect_handler_EARTHQUAKE;
    }
    else if (die < 106)
    {
        msg_misc(context->origin->player, " mumbles.");
        new_context.radius = 15;
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
        new_context.subtype = PROJ_DISP_ALL;
        new_context.other = 1;
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
    effect_simple(EF_PROJECT_LOS, context->origin, "150", PROJ_DISP_ALL, 0, 1, 0, 0, &context->ident);
    effect_simple(EF_PROJECT_LOS, context->origin, "20", PROJ_MON_SLOW, 0, 0, 0, 0, &context->ident);
    effect_simple(EF_PROJECT_LOS, context->origin, "0", PROJ_SLEEP_ALL, 0, 0, 0, 0, &context->ident);
    effect_simple(EF_HEAL_HP, context->origin, "300", 0, 0, 0, 0, 0, &context->ident);

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


static bool effect_valid(const struct effect *effect)
{
    if (!effect) return false;
    return ((effect->index > EF_NONE) && (effect->index < EF_MAX));
}


bool effect_aim(const struct effect *effect)
{
    const struct effect *e = effect;

    if (!effect_valid(effect)) return false;

    while (e)
    {
        if (effects[e->index].aim) return true;
        e = e->next;
    }

    return false;
}


const char *effect_info(const struct effect *effect)
{
    if (!effect_valid(effect)) return NULL;

    return effects[effect->index].info;
}


const char *effect_desc(const struct effect *effect)
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
 * Translate a string to an effect parameter subtype index
 */
int effect_subtype(int index, const char *type)
{
    int val = -1;

    /* Assign according to effect index */
    switch (index)
    {
        /* Projection name */
        case EF_ALTER:
        case EF_ARC:
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
        case EF_DESTRUCTION:
        case EF_LASH:
        case EF_LINE:
        case EF_MELEE_BLOWS:
        case EF_PROJECT:
        case EF_PROJECT_LOS:
        case EF_PROJECT_LOS_AWARE:
        case EF_SPOT:
        case EF_STAR:
        case EF_STAR_BALL:
        case EF_STRIKE:
        case EF_SWARM:
        case EF_TOUCH:
        case EF_TOUCH_AWARE:
        {
            val = proj_name_to_idx(type);
            break;
        }

        /* Inscribe a glyph */
        case EF_GLYPH:
        {
            if (streq(type, "WARDING")) val = GLYPH_WARDING;
            else if (streq(type, "DECOY")) val = GLYPH_DECOY;
            break;
        }

        case EF_TELEPORT:
        case EF_TELEPORT_LEVEL:
        {
            if (streq(type, "NONE")) val = 0;
            else val = proj_name_to_idx(type);
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

        /* Nourishment types */
        case EF_NOURISH:
        {
            if (streq(type, "INC_BY")) val = 0;
            else if (streq(type, "DEC_BY")) val = 1;
            else if (streq(type, "SET_TO")) val = 2;
            else if (streq(type, "INC_TO")) val = 3;
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

        /* Enchant type name - not worth a separate function */
        case EF_ENCHANT:
        {
            if (streq(type, "TOBOTH")) val = ENCH_TOBOTH;
            else if (streq(type, "TOHIT")) val = ENCH_TOHIT;
            else if (streq(type, "TODAM")) val = ENCH_TODAM;
            else if (streq(type, "TOAC")) val = ENCH_TOAC;
            break;
        }

        /* Targeted earthquake */
        case EF_EARTHQUAKE:
        {
            if (streq(type, "TARGETED")) val = 1;
            else if (streq(type, "NONE")) val = 0;
            break;
        }

        /* Allow monster teleport toward */
        case EF_TELEPORT_TO:
        {
            if (streq(type, "SELF")) val = 1;
            break;
        }

        /* Some effects only want a radius, so this is a dummy */
        default:
        {
            if (streq(type, "NONE")) val = 0;
            break;
        }
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
    struct beam_info *beam, int boost, quark_t note, struct monster *target_mon)
{
    bool completed = false;
    effect_handler_f handler;
    struct worldpos wpos;

    /* Paranoia */
    if (origin->player) memcpy(&wpos, &origin->player->wpos, sizeof(wpos));
    else if (origin->monster) memcpy(&wpos, &origin->monster->wpos, sizeof(wpos));
    else if (target_mon) memcpy(&wpos, &target_mon->wpos, sizeof(wpos));
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
            context.boost = boost;
            context.value = value;
            context.subtype = effect->subtype;
            context.radius = effect->radius;
            context.other = effect->other;
            context.y = effect->y;
            context.x = effect->x;
            context.self_msg = effect->self_msg;
            context.ident = *ident;
            context.note = note;
            context.flag = effect->flag;
            context.target_mon = target_mon;

            memset(&context.beam, 0, sizeof(context.beam));
            if (beam)
            {
                context.beam.beam = beam->beam;
                context.beam.spell_power = beam->spell_power;
                context.beam.elem_power = beam->elem_power;
                my_strcpy(context.beam.inscription, beam->inscription,
                    sizeof(context.beam.inscription));
            }

            completed = handler(&context);
            *ident = context.ident;

            /* PWMAngband: stop at the first non-handled effect */
            if (!completed) return false;

            /* PWMAngband: message if not already displayed */
            if (context.self_msg) msg(context.origin->player, context.self_msg);
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
bool effect_simple(int index, struct source *origin, const char *dice_string, int subtype,
    int radius, int other, int y, int x, bool *ident)
{
    struct effect effect;
    int dir = 0;
    bool dummy_ident = false, result;

    /* Set all the values */
    memset(&effect, 0, sizeof(effect));
    effect.index = index;
    effect.dice = dice_new();
    dice_parse_string(effect.dice, dice_string);
    effect.subtype = subtype;
    effect.radius = radius;
    effect.other = other;
    effect.y = y;
    effect.x = x;

    /* Direction if needed (PWMAngband: simply use actual target) */
    if (effect_aim(&effect)) dir = DIR_TARGET;

    /* Do the effect */
    if (ident)
        result = effect_do(&effect, origin, ident, true, dir, NULL, 0, 0, NULL);
    else
        result = effect_do(&effect, origin, &dummy_ident, true, dir, NULL, 0, 0, NULL);

    dice_free(effect.dice);
    return result;
}
