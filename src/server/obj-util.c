/*
 * File: obj-util.c
 * Purpose: Object utilities
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


struct object_base *kb_info;
struct artifact *a_info;
struct flavor *flavors;


/*
 * Hold the titles of scrolls, 6 to 14 characters each, plus quotes
 */
static char scroll_adj[MAX_TITLES][18];


static void flavor_assign_fixed(void)
{
    int i;
    struct flavor *f;

    for (f = flavors; f; f = f->next)
    {
        /* Skip random flavors */
        if (f->sval == SV_UNKNOWN) continue;

        for (i = 0; i < z_info->k_max; i++)
        {
            struct object_kind *k = &k_info[i];

            /* Skip other objects */
            if ((k->tval == f->tval) && (k->sval == f->sval))
            {
                /* Store the flavor */
                k->flavor = f;
            }
        }
    }
}


static void flavor_assign_random(byte tval)
{
    int i;
    int flavor_count = 0;
    int choice;
    struct flavor *f;

    /* Count the random flavors for the given tval */
    for (f = flavors; f; f = f->next)
    {
        if ((f->tval == tval) && (f->sval == SV_UNKNOWN))
            flavor_count++;
    }

    for (i = 0; i < z_info->k_max; i++)
    {
        /* Skip other object types */
        /* Skip objects that already are flavored */
        if ((k_info[i].tval != tval) || k_info[i].flavor) continue;

        if (!flavor_count) quit_fmt("Not enough flavors for tval %d.", tval);

        /* Select a flavor */
        choice = randint0(flavor_count);

        /* Find and store the flavor */
        for (f = flavors; f; f = f->next)
        {
            /* Skip other tvals */
            /* Skip assigned svals */
            if ((f->tval != tval) || (f->sval != SV_UNKNOWN)) continue;

            if (choice == 0)
            {
                /* Store the flavor */
                k_info[i].flavor = f;

                /* Mark the flavor as used */
                f->sval = k_info[i].sval;

                /* Hack -- set the scroll name if it's a scroll */
                if (tval_is_scroll_k(&k_info[i]))
                    f->text = string_make(scroll_adj[k_info[i].sval]);

                /* One less flavor to choose from */
                flavor_count--;

                break;
            }

            choice--;
        }
    }
}


/*
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, Staffs, Wands, Rods, Mushrooms, Potions, Scrolls.
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 2 to 8 letters long, and that no scroll is finished
 * until it attempts to grow beyond 15 letters.  The first time this
 * can happen is when the current title has 6 letters and the new word
 * has 8 letters, which would result in a 6 letter scroll title.
 *
 * Hack -- make sure everything stays the same for each saved game
 * This is accomplished by the use of a saved "random seed", as in
 * "town_gen()".  Since no other functions are called while the special
 * seed is in effect, so this function is pretty "safe".
 */
void flavor_init(void)
{
    int i, j;

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Hack -- induce consistant flavors */
    Rand_value = seed_flavor;

    flavor_assign_fixed();

    flavor_assign_random(TV_RING);
    flavor_assign_random(TV_AMULET);
    flavor_assign_random(TV_STAFF);
    flavor_assign_random(TV_WAND);
    flavor_assign_random(TV_ROD);
    flavor_assign_random(TV_MUSHROOM);
    flavor_assign_random(TV_POTION);

    /* Scrolls (random titles, always white) */
    for (i = 0; i < MAX_TITLES; i++)
    {
        char buf[26];
        char *end = buf + 1;
        int titlelen = 0;
        int wordlen;
        bool okay = true;

        my_strcpy(buf, "\"", sizeof(buf));
        wordlen = randname_make(RANDNAME_SCROLL, 2, 8, end, 24, name_sections);
        while (titlelen + wordlen < (int)(sizeof(scroll_adj[0]) - 3))
        {
            end[wordlen] = ' ';
            titlelen += wordlen + 1;
            end += wordlen + 1;
            wordlen = randname_make(RANDNAME_SCROLL, 2, 8, end, 24 - titlelen, name_sections);
        }
        buf[titlelen] = '"';
        buf[titlelen + 1] = '\0';

        /* Check the scroll name hasn't already been generated */
        for (j = 0; j < i; j++)
        {
            if (streq(buf, scroll_adj[j]))
            {
                okay = false;
                break;
            }
        }

        if (okay)
            my_strcpy(scroll_adj[i], buf, sizeof(scroll_adj[0]));

        /* Have another go at making a name */
        else
            i--;
    }

    flavor_assign_random(TV_SCROLL);

    /* Hack -- use the "complex" RNG */
    Rand_quick = false;
}


/*
 * Hack -- remove redundant bitflags
 */
static void remove_redundant_flags(bitflag flags[OF_SIZE])
{
    /*
     * ESP evil bypasses ESP undead/demon
     *
     * Note: although orcs/trolls/giants/dragons are evil, ESP evil cannot
     * bypass ESP orc/troll/giant/dragon because of the corresponding player
     * races (a player of the Half-Orc race can be detected by ESP orc, but
     * not by ESP evil)
     */
    if (of_has(flags, OF_ESP_EVIL))
    {
        of_off(flags, OF_ESP_UNDEAD);
        of_off(flags, OF_ESP_DEMON);
    }

    /* ESP all bypasses all other ESPs */
    if (of_has(flags, OF_ESP_ALL))
    {
        bitflag f2[OF_SIZE];

        create_obj_flag_mask(f2, false, OFT_ESP, OFT_MAX);
        of_diff(flags, f2);
        of_on(flags, OF_ESP_ALL);
    }
}


void object_flags_aux(const struct object *obj, bitflag flags[OF_SIZE])
{
    size_t i;

    /* Add object flags */
    of_copy(flags, obj->flags);

    /* Add curse flags */
    for (i = 0; obj->curses && (i < (size_t)z_info->curse_max); i++)
    {
        if (obj->curses[i].power == 0) continue;
        of_union(flags, curses[i].obj->flags);
    }
}


/*
 * Obtain the flags for an item
 */
void object_flags(const struct object *obj, bitflag flags[OF_SIZE])
{
    of_wipe(flags);

    if (!obj) return;

    object_flags_aux(obj, flags);

    /* Hack -- remove redundant bitflags */
    remove_redundant_flags(flags);
}


/*
 * Obtain the flags for an item which are known to the player
 */
void object_flags_known(const struct object *obj, bitflag flags[OF_SIZE], bool aware)
{
    bitflag obj_flags[OF_SIZE], known_flags[OF_SIZE];

    of_wipe(flags);

    if (!obj) return;

    /* Get object flags */
    of_wipe(obj_flags);
    object_flags_aux(obj, obj_flags);

    /* Get known flags */
    of_wipe(known_flags);
    object_flags_aux(obj->known, known_flags);

    /* Add object flags */
    of_copy(flags, obj_flags);

    of_inter(flags, known_flags);

    if (aware) of_union(flags, obj->kind->flags);

    if (obj->ego && easy_know(obj, aware))
        of_union(flags, obj->ego->flags);

    /* Hack -- remove redundant bitflags */
    remove_redundant_flags(flags);

    /* Make sure all flags are present on the object */
    of_inter(flags, obj_flags);
}


/*
 * Obtain the modifiers for an item
 */
void object_modifiers(const struct object *obj, s32b modifiers[OBJ_MOD_MAX])
{
    int i, j;

    memset(modifiers, 0, OBJ_MOD_MAX * sizeof(s32b));

    if (!obj) return;

    /* Add object modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
        modifiers[i] = obj->modifiers[i];

    /* Add curse modifiers */
    for (i = 0; obj->curses && (i < z_info->curse_max); i++)
    {
        if (obj->curses[i].power == 0) continue;
        for (j = 0; j < OBJ_MOD_MAX; j++)
            modifiers[j] += obj->curses[i].modifiers[j];
    }
}


/*
 * Obtain the to-hit for an item
 */
void object_to_h(const struct object *obj, s16b *to_h)
{
    size_t i;

    *to_h = 0;

    if (!obj) return;

    /* Add object to-hit */
    *to_h = obj->to_h;

    /* Add curse to-hit */
    for (i = 0; obj->curses && (i < (size_t)z_info->curse_max); i++)
    {
        if (obj->curses[i].power == 0) continue;
        *to_h += obj->curses[i].to_h;
    }
}


/*
 * Obtain the to-dam for an item
 */
void object_to_d(const struct object *obj, s16b *to_d)
{
    size_t i;

    *to_d = 0;

    if (!obj) return;

    /* Add object to-dam */
    *to_d = obj->to_d;

    /* Add curse to-dam */
    for (i = 0; obj->curses && (i < (size_t)z_info->curse_max); i++)
    {
        if (obj->curses[i].power == 0) continue;
        *to_d += obj->curses[i].to_d;
    }
}


/*
 * Obtain the to-ac for an item
 */
void object_to_a(const struct object *obj, s16b *to_a)
{
    size_t i;

    *to_a = 0;

    if (!obj) return;

    /* Add object to-ac */
    *to_a = obj->to_a;

    /* Add curse to-ac */
    for (i = 0; obj->curses && (i < (size_t)z_info->curse_max); i++)
    {
        if (obj->curses[i].power == 0) continue;
        *to_a += obj->curses[i].to_a;
    }
}


/*
 * Obtain the elements for an item
 */
void object_elements(const struct object *obj, struct element_info el_info[ELEM_MAX])
{
    int i, j;
    bool vuln[ELEM_MAX];

    memset(el_info, 0, ELEM_MAX * sizeof(struct element_info));

    if (!obj) return;

    /* Add object elements */
    memcpy(el_info, obj->el_info, ELEM_MAX * sizeof(struct element_info));
    for (i = 0; i < ELEM_MAX; i++)
    {
        vuln[i] = false;
        if (el_info[i].res_level == -1)
        {
            vuln[i] = true;
            el_info[i].res_level = 0;
        }
    }

    /* Add curse elements */
    for (i = 0; obj->curses && (i < z_info->curse_max); i++)
    {
        if (obj->curses[i].power == 0) continue;
        for (j = 0; j < ELEM_MAX; j++)
        {
            if (curses[i].obj->el_info[j].res_level == -1)
                vuln[j] = true;
            if (curses[i].obj->el_info[j].res_level > el_info[j].res_level)
                el_info[j].res_level = curses[i].obj->el_info[j].res_level;
        }
    }

    for (i = 0; i < ELEM_MAX; i++)
    {
        if (vuln[i] && (el_info[i].res_level < 3))
            el_info[i].res_level--;
    }
}


/*
 * Return true if the item is unknown (has yet to be seen by the player).
 */
bool is_unknown(const struct object *obj)
{
    if (!obj) return false;
    return (obj->kind == unknown_item_kind);
}


/*
 * Return true if the item is unknown money (has yet to be seen by the player).
 */
bool is_unknown_money(const struct object *obj)
{
    if (!obj) return false;
    return (obj->kind == unknown_gold_kind);
}


/*
 * Sort comparator for objects using only tval and sval
 * -1 if o1 should be first
 *  1 if o2 should be first
 *  0 if it doesn't matter
 */
static int compare_types(const struct object *o1, const struct object *o2)
{
    if (o1->tval == o2->tval) return CMP(o1->sval, o2->sval);
    return CMP(o1->tval, o2->tval);
}


/*
 * Sort comparator for objects
 * -1 if o1 should be first
 *  1 if o2 should be first
 *  0 if it doesn't matter
 *
 * The sort order is designed with the "list items" command in mind.
 */
int compare_items(struct player *p, const struct object *o1, const struct object *o2)
{
    /* Unknown objects go at the end, order doesn't matter */
    if (is_unknown(o1) || is_unknown(o2))
    {
        if (!is_unknown(o1)) return -1;
        return 1;
    }

    /* Known artifacts will sort first */
    if (object_is_known_artifact(o1) && object_is_known_artifact(o2))
        return compare_types(o1, o2);
    if (object_is_known_artifact(o1)) return -1;
    if (object_is_known_artifact(o2)) return 1;

    /* Unknown objects will sort next */
    if (!object_flavor_is_aware(p, o1) && !object_flavor_is_aware(p, o2))
        return compare_types(o1, o2);
    if (!object_flavor_is_aware(p, o1)) return -1;
    if (!object_flavor_is_aware(p, o2)) return 1;

    /* If only one of them is worthless, the other comes first */
    if ((o1->kind->cost == 0) && (o2->kind->cost != 0)) return 1;
    if ((o1->kind->cost != 0) && (o2->kind->cost == 0)) return -1;

    /* Otherwise, just compare tvals and svals */
    /* NOTE: arguably there could be a better order than this */
    return compare_types(o1, o2);
}


/*** Generic utility functions ***/


/*
 * Return an object's effect.
 */
struct effect *object_effect(const struct object *obj)
{
    if (obj->activation) return obj->activation->effect;
    if (obj->effect) return obj->effect;
    return NULL;
}


/*
 * Can the object fail if used?
 */
bool obj_can_fail(struct player *p, const struct object *o)
{
    if (tval_can_have_failure(o)) return true;

    return ((wield_slot(p, o) == -1)? false: true);
}


/*
 * Returns the number of times in 1000 that @ will FAIL
 */
int get_use_device_chance(struct player *p, const struct object *obj)
{
    int lev, fail, numerator, denominator;
    int skill = p->state.skills[SKILL_DEVICE];
    int skill_min = 10;
    int skill_max = 141;
    int diff_min  = 1;
    int diff_max  = 100;

    /* Extract the item level, which is the difficulty rating */
    if (obj->artifact)
        lev = get_artifact_level(obj);
    else
        lev = obj->kind->level;

    numerator   = (skill - lev) - (skill_max - diff_min);
    denominator = (lev - skill) - (diff_max - skill_min);

    /* Make sure that we don't divide by zero */
    if (denominator == 0) denominator = ((numerator > 0)? 1: -1);

    fail = (100 * numerator) / denominator;

    /* Ensure failure rate is between 1% and 75% */
    if (fail > 750) fail = 750;
    if (fail < 10) fail = 10;

    return fail;
}


/*
 * Distribute charges of rods, staves, or wands.
 *
 * source = source item
 * dest = target item, must be of the same type as source
 * amt   = number of items that are transfered
 */
void distribute_charges(struct object *source, struct object *dest, int amt)
{
    int charge_time = randcalc(source->time, 0, AVERAGE), max_time;

    /*
     * Hack -- if staves or wands are dropped, the total maximum
     * charges need to be allocated between the two stacks.
     * If all the items are being dropped, it makes for a neater message
     * to leave the original stack's charges alone.
     */
    if (tval_can_have_charges(source))
    {
        dest->pval = source->pval * amt / source->number;

        if (amt < source->number) source->pval -= dest->pval;
    }

    /*
     * Hack -- rods also need to have their timeouts distributed.
     *
     * The dropped stack will accept all time remaining to charge up to
     * its maximum.
     */
    if (tval_can_have_timeout(source))
    {
        max_time = charge_time * amt;

        if (source->timeout > max_time) dest->timeout = max_time;
        else dest->timeout = source->timeout;

        if (amt < source->number) source->timeout -= dest->timeout;
    }
}


/*
 * Number of items (usually rods) charging
 */
int number_charging(const struct object *obj)
{
    int charge_time, num_charging;

    charge_time = randcalc(obj->time, 0, AVERAGE);

    /* Item has no timeout */
    if (charge_time <= 0) return 0;

    /* No items are charging */
    if (obj->timeout <= 0) return 0;

    /* Calculate number charging based on timeout */
    num_charging = (obj->timeout + charge_time - 1) / charge_time;

    /* Number charging cannot exceed stack size */
    if (num_charging > obj->number) num_charging = obj->number;

    return num_charging;
}


/*
 * Allow a stack of charging objects to charge by one unit per charging object.
 * Return true if something recharged
 */
bool recharge_timeout(struct object *obj)
{
    int charging_before, charging_after;

    /* Find the number of charging items */
    charging_before = number_charging(obj);

    /* Nothing to charge */
    if (charging_before == 0) return false;

    /* Decrease the timeout */
    obj->timeout -= MIN(charging_before, obj->timeout);

    /* Find the new number of charging items */
    charging_after = number_charging(obj);

    /* Return true if at least 1 item obtained a charge */
    if (charging_after < charging_before) return true;

    return false;
}


/* Can only take off non-stuck items */
bool obj_can_takeoff(const struct object *obj)
{
    bitflag f[OF_SIZE];

    object_flags(obj, f);

    return !of_has(f, OF_STICKY);
}


/*
 * Does the given object need to be aimed?
 */
int obj_needs_aim(struct player *p, const struct object *obj)
{
    bool aim = effect_aim(object_effect(obj));
    bool aware = object_flavor_is_aware(p, obj);

    /* Determine whether we know an item needs to be aimed */
    bool known_aim = (aware || object_effect_is_known(obj, aware));

    /* Wands and ammo */
    if (tval_is_ammo(obj) || tval_is_wand(obj)) return AIM_NORMAL;

    /* Rods that require aiming or unknown rods */
    if (tval_can_have_timeout(obj) && (aim || !aware)) return AIM_NORMAL;

    /* Unknown things with no obvious aim get a random direction */
    if (aim) return (known_aim? AIM_NORMAL: AIM_RANDOM);

    return AIM_NONE;
}


/*** PWMAngband ***/


void get_object_info(struct player *p, struct object *obj, byte equipped,
    struct object_xtra *info_xtra)
{
    struct curse_data *c = obj->known->curses;
    int i;
    bool activatable, charging;

    /* Get the color */
    info_xtra->attr = obj->kind->base->attr;

    /* Get the "activation" flag */
    activatable = ((equipped && object_effect(obj)) || (!equipped && tval_can_have_timeout(obj)));
    if (activatable)
    {
        info_xtra->act = ACT_NORMAL;

        /* Check the recharge */
        charging = ((equipped && obj->timeout) ||
            (!equipped && (number_charging(obj) == obj->number)));
        if (charging) info_xtra->act = ACT_TIMEOUT;
    }

    /* Get direction choice */
    if (obj_needs_aim(p, obj) == AIM_NORMAL) info_xtra->aim = 1;

    /* Get the "fuelable" flag */
    if (tval_is_light(obj) && of_has(obj->flags, OF_TAKES_FUEL))
    {
        /* Non-empty, non-everburning lamps are okay */
        if ((equipped || (obj->timeout > 0)) && !of_has(obj->flags, OF_NO_FUEL))
            info_xtra->fuel = 1;
    }

    /* Get the "fail" flag */
    info_xtra->fail = 255;
    if (obj_can_fail(p, obj))
    {
        info_xtra->fail--;
        if (object_effect_is_known(obj, object_flavor_is_aware(p, obj)))
            info_xtra->fail = (9 + get_use_device_chance(p, obj)) / 10;
    }

    /* Get the "cursed" flag */
    for (i = 0; c && (i < z_info->curse_max); i++)
    {
        if (c[i].power == 0) continue;
        if (c[i].power >= 100) continue;
        if (!player_knows_curse(p, i)) continue;
        if (!STRZERO(info_xtra->name_curse))
        {
            my_strcat(info_xtra->name_curse, "|", sizeof(info_xtra->name_curse));
            my_strcat(info_xtra->name_power, "|", sizeof(info_xtra->name_power));
        }
        my_strcat(info_xtra->name_curse, format("%d", i), sizeof(info_xtra->name_curse));
        my_strcat(info_xtra->name_power, format("%d", c[i].power), sizeof(info_xtra->name_power));
    }

    /* Get the "stuck" flag */
    if (equipped) info_xtra->stuck = !obj_can_takeoff(obj);

    /* Get the "known" flag */
    info_xtra->known = object_runes_known(obj);

    /* Get the "slot" flag */
    if (equipped) info_xtra->slot = equipped_item_slot(p->body, obj);
    else info_xtra->slot = wield_slot(p, obj);
}


int get_owner_id(const struct object *obj)
{
    int ind;

    if (!obj->owner) return 0;

    for (ind = 1; ind <= NumPlayers; ind++)
    {
        struct player *p = player_get(ind);

        if (p->id == obj->owner) return ind;
    }

    return 0;
}


void set_artifact_info(struct player *p, const struct object *obj, byte info)
{
    byte *pinfo;

    /* Paranoia */
    if (!p) return;

    /* Not an artifact */
    if (!obj->artifact) return;

    /* True artifacts */
    if (true_artifact_p(obj)) pinfo = p->art_info;
    else pinfo = p->randart_info;

    /* Add history entry */
    switch (info)
    {
        case ARTS_GENERATED:
            history_generate_artifact(p, obj);
            break;
        case ARTS_FOUND:
            history_find_artifact(p, obj);
            break;
        case ARTS_ABANDONED:
        case ARTS_SOLD:
            history_lose_artifact(p, obj);
            break;
    }

    /* Only once */
    if (pinfo[obj->artifact->aidx] >= info) return;

    /* Register info */
    pinfo[obj->artifact->aidx] = info;
}


/*
 * Other "kind" values for "good" templates.
 * Used by the auto-ignore system to avoid ignoring "good" items at
 * "average" level.
 */
bool kind_is_good_other(const struct object_kind *kind)
{
    /* Dragon Scale Mails are good */
    if (kind->tval == TV_DRAG_ARMOR) return true;

    /* Anything with the GOOD flag */
    if (kf_has(kind->kind_flags, KF_GOOD)) return true;

    /* Assume not good */
    return false;
}


void set_origin(struct object *obj, byte origin, s16b origin_depth, struct monster_race *origin_race)
{
    obj->origin = origin;
    obj->origin_depth = origin_depth;
    obj->origin_race = origin_race;
}


/*
 * Shimmer multi-hued objects
 */
void shimmer_objects(struct player *p, struct chunk *c)
{
    int y, x;

    /* Shimmer multi-hued objects */
    for (y = 1; y < c->height; y++)
    {
        for (x = 1; x < c->width; x++)
        {
            struct object *obj, *first_obj = NULL;

            /* Need to be the first object on the pile that is not ignored */
            for (obj = square_known_pile(p, c, y, x); obj; obj = obj->next)
            {
                if (!ignore_item_ok(p, obj))
                {
                    if (!first_obj)
                        first_obj = obj;
                    else
                    {
                        first_obj = NULL;
                        break;
                    }
                }
            }

            /* Light that spot */
            if (first_obj && object_shimmer(first_obj))
                square_light_spot_aux(p, c, y, x);
        }
    }
}


/*
 * Hack -- process the objects
 */
void process_objects(struct chunk *c)
{
    int i, y, x;

    /* Every 10 game turns */
    if ((turn.turn % 10) != 5) return;

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Skip irrelevant players */
        if (!COORDS_EQUAL(&p->wpos, &c->wpos)) continue;
        if (p->upkeep->new_level_method || p->upkeep->funeral) continue;
        if (!allow_shimmer(p)) continue;

        /* Shimmer multi-hued objects */
        shimmer_objects(p, c);
    }

    /* Recharge other level objects */
    for (y = 1; y < c->height; y++)
    {
        for (x = 1; x < c->width; x++)
        {
            struct object *obj = square_object(c, y, x), *next;
            bool redraw = false;

            while (obj)
            {
                next = obj->next;

                /* Recharge rods */
                if (tval_can_have_timeout(obj) && recharge_timeout(obj))
                    redraw = true;

                /* Corpses slowly decompose */
                if (tval_is_corpse(obj))
                {
                    obj->decay--;

                    /* Notice changes */
                    if (obj->decay == obj->timeout / 5)
                        redraw = true;

                    /* No more corpse... */
                    else if (!obj->decay)
                    {
                        square_excise_object(c, y, x, obj);
                        object_delete(&obj);
                    }
                }

                obj = next;
            }

            if (redraw) redraw_floor(&c->wpos, y, x);
        }
    }
}


bool is_owner(struct player *p, struct object *obj)
{
    /* Free object */
    if (!obj->owner) return true;

    /* No restriction */
    if (!OPT(p, birth_no_stores)) return true;

    /* Must be the owner */
    return (obj->owner == p->id);
}


void object_own(struct player *p, struct object *obj)
{
    obj->askprice = 0;

    /* Check ownership */
    if (obj->owner && (p->id != obj->owner))
    {
        char o_name[NORMAL_WID];
        char buf[512];
        const char *owner_name;
        hash_entry *ptr;
        struct effect *effect = object_effect(obj);

        /* Owner name */
        ptr = lookup_player(obj->owner);
        owner_name = ((ptr && ht_zero(&ptr->death_turn))? ptr->name: "(deceased)");

        /* Log transaction */
        strnfmt(buf, sizeof(buf), "TR %s-%d | %s-%d $ %ld", owner_name, (int)obj->owner,
            p->name, (int)p->id, (long)object_value(p, obj, 1));
        audit(buf);

        /* Object name */
        object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
        strnfmt(buf, sizeof(buf), "TR+%s", o_name);
        audit(buf);

        /* Hack -- potion of experience */
        if (effect && (effect->index == EF_GAIN_EXP)) obj->askprice = 1;
    }

    /* Set ownership */
    obj->owner = p->id;

    /* Artifact is now owned */
    if (true_artifact_p(obj)) obj->artifact->owned = 1;
}


static struct player *get_creator(const struct object *obj)
{
    int i;

    if (!obj->creator) return NULL;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (p->id == obj->creator) return p;
    }

    return NULL;
}


void preserve_artifact_aux(const struct object *obj)
{
    /* Not an artifact */
    if (!obj->artifact) return;

    /* True artifacts */
    if (true_artifact_p(obj))
    {
        if (obj->artifact->created) obj->artifact->created--;
        obj->artifact->owned = 0;
    }

    /* Randarts */
    else
    {
        /* Only works when creator is ingame */
        struct player *p = get_creator(obj);

        if (p) p->randart_created[obj->artifact->aidx] = 0;
    }
}


void preserve_artifact(const struct object *obj)
{
    struct player *p;

    /* Not an artifact */
    if (!obj->artifact) return;

    /* Only works when owner is ingame */
    p = player_get(get_owner_id(obj));

    /* Preserve any artifact */
    preserve_artifact_aux(obj);
    if (p) history_lose_artifact(p, obj);
}


/*
 * Destroy an item in the pack or on the floor. Returns true if the item has been completely
 * used up, false otherwise.
 */
bool use_object(struct player *p, struct object *obj, int amount, bool describe)
{
    struct object *used_obj;
    bool none_left = false;
    struct chunk *c = chunk_get(&p->wpos);

    /* Destroy an item in the pack */
    if (object_is_carried(p, obj))
        used_obj = gear_object_for_use(p, obj, amount, describe, &none_left);

    /* Destroy an item on the floor */
    else
        used_obj = floor_object_for_use(p, c, obj, amount, describe, &none_left);

    object_delete(&used_obj);

    return none_left;
}


/*
 * Redraw changes occured on floor items
 *
 * Note: this is similar to square_note_spot(), but we don't memorize the grid -- we redraw
 * the floor instead.
 */
void redraw_floor(struct worldpos *wpos, int y, int x)
{
    int i;

    /* Redraw changes for all players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Require "seen" flag and the current level */
        /* PWMAngband: consider player spot as "seen" */
        if (!COORDS_EQUAL(&p->wpos, wpos)) continue;
        if (p->upkeep->new_level_method || p->upkeep->funeral) continue;
        if (!square_isseen(p, y, x) && !player_is_at(p, y, x)) continue;

        /* Make the player know precisely what is on this grid */
        square_know_pile(p, chunk_get(wpos), y, x);

        /* Redraw */
        p->upkeep->redraw |= PR_ITEMLIST;

        /* Under a player */
        if (!player_is_at(p, y, x)) continue;

        /* Redraw */
        p->upkeep->redraw |= PR_FLOOR;
    }
}


bool object_marked_aware(struct player *p, const struct object *obj)
{
    if (p)
    {
        int none = tval_find_idx("none");
        int item = lookup_sval(none, "<unknown item>");
        int gold = lookup_sval(none, "<unknown treasure>");

        return ((obj->tval == none) && ((obj->sval == item) || (obj->sval == gold)));
    }

    return false;
}


/* Hack -- get an item from its index */
struct object *object_from_index(struct player *p, int item, bool prompt, bool check_ignore)
{
    struct object *obj;
    struct chunk *c = chunk_get(&p->wpos);

    /* Get the item (from pack or equipment) */
    if (item >= 0)
    {
        for (obj = p->gear; obj; obj = obj->next)
        {
            /* Check item (from pack or equipment) */
            if (obj->oidx == item) return obj;
        }

        return NULL;
    }

    /* Get the item (on the floor) */
    for (obj = square_object(c, p->py, p->px); obj; obj = obj->next)
    {
        /* Ignore all non-objects */
        if (!obj->kind) continue;

        /* Ignore all hidden objects */
        if (check_ignore && ignore_item_ok(p, obj)) continue;

        /* Check item (on the floor) */
        if (obj->oidx == item) return obj;
    }

    /* Nothing */
    if (prompt) msg(p, "There's nothing on the floor.");
    return NULL;
}


/*
 * Find an ego item from its name.
 *
 * name ego type name
 * kind object kind
 */
struct ego_item *lookup_ego_item(const char *name, struct object_kind *kind)
{
    int i;

    /* Paranoia */
    if (!kind) return NULL;

    /* Look for it */
    for (i = 0; i < z_info->e_max; i++)
    {
        struct ego_item *ego = &e_info[i];
        struct poss_item *poss_item = ego->poss_items;

        /* Reject nameless and wrong names */
        if (!ego->name) continue;
        if (strcmp(name, ego->name)) continue;

        /* Check kind */
        while (poss_item)
        {
            if (kind->kidx == poss_item->kidx) return ego;
            poss_item = poss_item->next;
        }
    }

    return NULL;
}


/*
 * Return the artifact with the given name
 */
struct artifact *lookup_artifact_name(const char *name)
{
    int i;
    struct artifact *match = NULL;

    /* Look for it */
    for (i = 0; i < z_info->a_max; i++)
    {
        struct artifact *art = &a_info[i];

        /* Test for equality */
        if (art->name && streq(name, art->name)) return art;

        /* Test for close matches */
        if ((strlen(name) >= 3) && art->name && my_stristr(art->name, name) && !match)
            match = art;
    }

    /* Return our best match */
    return match;
}
