/*
 * File: obj-info.c
 * Purpose: Object description code.
 *
 * Copyright (c) 2004 Robert Ruehlmann
 * Copyright (c) 2010 Andi Sidwell
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
 * Describes the number of blows possible for given stat bonuses
 */
struct blow_info
{
    int str_plus;
    int dex_plus;
    int centiblows;
};


/*
 * Data tables
 */


static const struct origin_type
{
    int type;
    int args;
    const char *desc;
} origins[] =
{
    #define ORIGIN(a, b, c) {ORIGIN_##a, b, c},
    #include "list-origins.h"
    #undef ORIGIN

    {ORIGIN_MAX, -1, ""}
};


/*
 * List-writing utility code
 */


/*
 * Given an array of strings, as so:
 *  { "intelligence", "fish", "lens", "prime", "number" },
 *
 * ... output a list like "intelligence, fish, lens, prime, number.\n".
 */
static void info_out_list(struct player *p, const char *list[], size_t count)
{
    size_t i;

    for (i = 0; i < count; i++)
    {
        text_out(p, list[i]);
        if (i != (count - 1)) text_out(p, ", ");
    }

    text_out(p, ".\n");
}


/*
 * Fills recepticle with all the elements that correspond to the given `list`.
 */
static size_t element_info_collect(const bool list[], const char *recepticle[])
{
    int i, count = 0;

    for (i = 0; i < ELEM_MAX; i++)
    {
        if (list[i]) recepticle[count++] = projections[i].name;
    }

    return count;
}


/*
 * Code that makes use of the data tables to describe aspects of an
 * object's information
 */


/*
 * Describe an item's curses.
 */
static bool describe_curses(struct player *p, const struct object *obj)
{
    int i;
    struct curse_data *c = (obj->curses? obj->known->curses: NULL);

    if (!c) return false;

    for (i = 0; i < z_info->curse_max; i++)
    {
        if (c[i].power == 0) continue;
        text_out(p, "It ");
        text_out_c(p, COLOUR_L_RED, curses[i].desc);
        if (c[i].power == 100) text_out(p, "; this curse cannot be removed");
        text_out(p, ".\n");
    }

    /* Say if curse removal has been tried */
    if (of_has(obj->flags, OF_FRAGILE)) text_out(p, "Attempting to uncurse it may destroy it.\n");

    return true;
}


/*
 * Describe stat modifications.
 */
static bool describe_stats(struct player *p, const struct object *obj, bool aware)
{
    size_t count = 0, i;
    s32b modifiers[OBJ_MOD_MAX];

    /* Fact of but not size of mods is known for egos and flavoured items the player is aware of */
    bool known_effect = false;

    if (obj->ego && obj->known->ego) known_effect = true;
    if (tval_can_have_flavor(obj) && aware) known_effect = true;

    object_modifiers(obj, modifiers);

    /* See what we've got */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        const char *desc = lookup_obj_property(OBJ_PROPERTY_MOD, i)->name;
        int val = modifiers[i];

        /* Hack -- don't show pointless "poly race" modifier */
        if (i == OBJ_MOD_POLY_RACE) continue;

        /* Hack -- antimagic is described elsewhere */
        if (i == OBJ_MOD_ANTI_MAGIC) continue;

        if (!val) continue;
        if (obj->known->modifiers[i] || known_effect) count++;

        /* Either all mods are visible, or none are */
        if (obj->known->modifiers[i])
            text_out_c(p, ((val > 0)? COLOUR_L_GREEN: COLOUR_RED), "%+i %s.\n", val, desc);
        else if (known_effect)
            text_out(p, "Affects your %s.\n", desc);
    }

    return (count > 0);
}


/*
 * Describe immunities, resistances and vulnerabilities granted by an object.
 */
static bool describe_elements(struct player *p, const struct element_info el_info[])
{
    const char *i_descs[ELEM_MAX];
    const char *r_descs[ELEM_MAX];
    const char *v_descs[ELEM_MAX];
    size_t i, count;
    bool list[ELEM_MAX], prev = false;

    /* Immunities */
    for (i = 0; i < ELEM_MAX; i++)
        list[i] = (el_info[i].res_level == 3);
    count = element_info_collect(list, i_descs);
    if (count)
    {
        text_out(p, "Provides immunity to ");
        info_out_list(p, i_descs, count);
        prev = true;
    }

    /* Resistances */
    for (i = 0; i < ELEM_MAX; i++)
        list[i] = (el_info[i].res_level == 1);
    count = element_info_collect(list, r_descs);
    if (count)
    {
        text_out(p, "Provides resistance to ");
        info_out_list(p, r_descs, count);
        prev = true;
    }

    /* Vulnerabilities */
    for (i = 0; i < ELEM_MAX; i++)
        list[i] = (el_info[i].res_level == -1);
    count = element_info_collect(list, v_descs);
    if (count)
    {
        text_out(p, "Makes you vulnerable to ");
        info_out_list(p, v_descs, count);
        prev = true;
    }

    return prev;
}


/*
 * Describe protections granted by an object.
 */
static bool describe_protects(struct player *p, const bitflag flags[OF_SIZE])
{
    const char *p_descs[OF_MAX];
    int i, count = 0;

    /* Protections */
    for (i = 1; i < OF_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_FLAG, i);

        if (prop->subtype != OFT_PROT) continue;
        if (of_has(flags, prop->index)) p_descs[count++] = prop->desc;
    }
    if (!count) return false;

    text_out(p, "Provides protection from ");
    info_out_list(p, p_descs, count);

    return true;
}


/*
 * Describe elements an object ignores.
 */
static bool describe_ignores(struct player *p, const struct element_info el_info[])
{
    const char *descs[ELEM_MAX];
    size_t i, count;
    bool list[ELEM_MAX];

    for (i = 0; i < ELEM_MAX; i++)
        list[i] = ((el_info[i].flags & EL_INFO_IGNORE)? true: false);
    count = element_info_collect(list, descs);

    if (!count) return false;

    text_out(p, "Cannot be harmed by ");
    info_out_list(p, descs, count);

    return true;
}


/*
 * Describe elements that damage or destroy an object.
 */
static bool describe_hates(struct player *p, const struct element_info el_info[])
{
    const char *descs[ELEM_MAX];
    size_t i, count;
    bool list[ELEM_MAX];

    for (i = 0; i < ELEM_MAX; i++)
        list[i] = ((el_info[i].flags & EL_INFO_HATES)? true: false);
    count = element_info_collect(list, descs);

    if (!count) return false;

    text_out(p, "Can be destroyed by ");
    info_out_list(p, descs, count);

    return true;
}


/*
 * Describe stat sustains.
 */
static bool describe_sustains(struct player *p, const bitflag flags[OF_SIZE])
{
    const char *descs[STAT_MAX];
    int i, count = 0;

    for (i = 0; i < STAT_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_STAT, i);

        if (of_has(flags, sustain_flag(prop->index))) descs[count++] = prop->name;
    }

    if (!count) return false;

    text_out(p, "Sustains ");
    info_out_list(p, descs, count);

    return true;
}


/*
 * Describe miscellaneous powers.
 */
static bool describe_misc_magic(struct player *p, const bitflag flags[OF_SIZE])
{
    int i;
    bool printed = false;

    for (i = 1; i < OF_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_FLAG, i);

        if ((prop->subtype != OFT_MISC) && (prop->subtype != OFT_MELEE) &&
            (prop->subtype != OFT_BAD) && (prop->subtype != OFT_OTHER))
        {
            continue;
        }
        if (of_has(flags, prop->index) && prop->desc)
        {
            text_out(p, "%s. ", prop->desc);
            printed = true;
        }
    }

    if (printed) text_out(p, "\n");

    return printed;
}


/*
 * Describe slays granted by an object.
 */
static bool describe_slays(struct player *p, const struct object *obj, bool fulldesc)
{
    int i, count = 0;
    bool *s = obj->known->slays;

    if (!s) return false;

    for (i = 0; i < z_info->slay_max; i++)
    {
        if (s[i]) count++;
    }

    my_assert(count >= 1);

    if (fulldesc) text_out(p, "It causes your melee attacks to slay ");
    else text_out(p, "Slays ");

    for (i = 0; i < z_info->slay_max; i++)
    {
        if (!s[i]) continue;
        text_out(p, slays[i].name);
        if (slays[i].multiplier > 3) text_out(p, " (powerfully)");
        if (count > 1) text_out(p, ", ");
        else text_out(p, ".\n");
        count--;
    }

    return true;
}


/*
 * Describe brands granted by an object.
 */
static bool describe_brands(struct player *p, const struct object *obj, bool fulldesc)
{
    int i, count = 0;
    bool *b = obj->known->brands;

    if (!b) return false;

    for (i = 0; i < z_info->brand_max; i++)
    {
        if (b[i]) count++;
    }

    my_assert(count >= 1);

    if (fulldesc) text_out(p, "It brands your melee attacks with ");
    else text_out(p, "Branded with ");

    for (i = 0; i < z_info->brand_max; i++)
    {
        if (!b[i]) continue;
        if (brands[i].desc_adjective)
        {
            text_out(p, brands[i].desc_adjective);
            text_out(p, " ");
        }
        text_out(p, brands[i].name);
        if (count > 1) text_out(p, ", ");
        else text_out(p, ".\n");
        count--;
    }

    return true;
}


/*
 * Describe monster ESP.
 */
static bool describe_esp(struct player *p, const bitflag flags[OF_SIZE])
{
    int i;
    bool printed = false;

    for (i = 1; i < OF_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_FLAG, i);

        if (prop->subtype != OFT_ESP) continue;
        if (of_has(flags, prop->index))
        {
            text_out(p, "%s.\n", prop->desc);
            printed = true;
        }
    }

    return printed;
}


/*
 * Account for criticals in the calculation of melee prowess.
 */
static int calculate_melee_crits(struct player *p, struct player_state *state, int weight,
    int plus, int dam)
{
    int k, to_crit = weight + 5 * (state->to_h + plus) + (state->skills[SKILL_TO_HIT_MELEE] - 60);
    int crit_dam = 0;

    to_crit = MIN(5000, MAX(0, to_crit));

    /* Extract average critical damage */
    for (k = weight + 1; k <= weight + 650; k++)
    {
        if (k < 400) crit_dam += dam * 2 + 5;
        else if (k < 700) crit_dam += dam * 2 + 10;
        else if (k < 900) crit_dam += dam * 3 + 15;
        else if (k < 1300) crit_dam += dam * 3 + 20;
        else crit_dam += dam * 4 + 20;
    }
    crit_dam /= 650;

    /* Chance */
    crit_dam = (crit_dam * to_crit + dam * (5000 - to_crit)) / 5000;

    /* Apply Touch of Death */
    if (p->timed[TMD_DEADLY])
        crit_dam = (crit_dam * 3 + (dam * 4 + 30)) / 4;

    return crit_dam;
}


/*
 * Missile crits follow the same approach as melee crits.
 */
static int calculate_missile_crits(struct player *p, struct player_state *state, int weight,
    int plus, int dam)
{
    int k, to_crit = weight + 4 * (state->to_h + plus) + 2 * p->lev;
    int crit_dam = 0;

    to_crit = MIN(5000, MAX(0, to_crit));

    /* Extract average critical damage */
    for (k = weight + 1; k <= weight + 500; k++)
    {
        if (k < 500) crit_dam += dam * 2 + 5;
        else if (k < 1000) crit_dam += dam * 2 + 10;
        else crit_dam += dam * 3 + 15;
    }
    crit_dam /= 500;

    /* Chance */
    crit_dam = (crit_dam * to_crit + dam * (5000 - to_crit)) / 5000;

    return crit_dam;
}


/*
 * Get the object flags the player should know about for the given object/viewing mode combination.
 */
static void get_known_flags(const struct object *obj, int mode, bitflag flags[OF_SIZE], bool aware)
{
    /* Grab the object flags */
    object_flags_known(obj, flags, aware);

    /* Don't include base flags when terse */
    if (mode & OINFO_TERSE) of_diff(flags, obj->kind->base->flags);
}


/*
 * Get the object element info the player should know about for the given
 * object/viewing mode combination.
 */
static void get_known_elements(const struct object *obj, int mode, struct element_info el_info[],
    bool aware)
{
    size_t i;

    object_elements(obj, el_info);

    /* Grab the element info */
    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Report known element info */
        if (!object_element_is_known(obj, i, aware))
        {
            el_info[i].res_level = 0;
            el_info[i].flags = 0;
            if (obj->known->el_info[i].flags & EL_INFO_IGNORE)
                el_info[i].flags |= EL_INFO_IGNORE;
        }

        /* Ignoring an element */
        if (obj->el_info[i].flags & EL_INFO_IGNORE)
        {
            /* If the object is usually destroyed, mention the ignoring */
            if (obj->el_info[i].flags & EL_INFO_HATES)
                el_info[i].flags &= ~(EL_INFO_HATES);

            /* Otherwise don't say anything */
            else
                el_info[i].flags &= ~(EL_INFO_IGNORE);
        }

        /* Don't include hates flag when terse */
        if (mode & OINFO_TERSE)
            el_info[i].flags &= ~(EL_INFO_HATES);
    }
}


/*
 * Gets information about the number of blows possible for the player with
 * the given object.
 *
 * Fills in the possible_blows[] information of .str_plus and .dex_plus needed
 * to achieve the approximate number of blows in centiblows.
 *
 * `max_blows` must be at least 1 to hold the current number of blows
 * `possible_blows` must be at least [`max_blows`] in size, and will be limited
 * to that number of entries. The theoretical maximum is STAT_RANGE * 2 if
 * an extra blow/speed boost was given for each combination of STR and DEX.
 *
 * Returns the number of entries made in the possible_blows[] table, or 0
 * if the object is not a weapon.
 */
static int obj_known_blows(struct player *p, const struct object *obj, int max_num,
    struct blow_info possible_blows[])
{
    int str_plus, dex_plus, old_blows;
    int str_faster = -1, str_done = -1;
    int dex_plus_bound;
    int str_plus_bound;
    struct player_state state;
    int weapon_slot = slot_by_name(p, "weapon");
    struct object *current_weapon = slot_object(p, weapon_slot);
    int num = 0;
    bool weapon = (tval_is_melee_weapon(obj) || tval_is_mstaff(obj));

    /* Not a weapon - no blows! */
    if (!weapon) return 0;

    /* Pretend we're wielding the object */
    p->body.slots[weapon_slot].obj = (struct object *)obj;

    /* Calculate the player's hypothetical state */
    memset(&state, 0, sizeof(state));
    calc_bonuses(p, &state, true, false);

    /* First entry is always the current num of blows. */
    possible_blows[num].str_plus = 0;
    possible_blows[num].dex_plus = 0;
    possible_blows[num].centiblows = state.num_blows;
    num++;

    /* Check to see if extra STR or DEX would yield extra blows */
    old_blows = state.num_blows;
    dex_plus_bound = STAT_RANGE - state.stat_ind[STAT_DEX];
    str_plus_bound = STAT_RANGE - state.stat_ind[STAT_STR];

    /* Re-calculate with increased stats */
    for (dex_plus = 0; dex_plus < dex_plus_bound; dex_plus++)
    {
        for (str_plus = 0; str_plus < str_plus_bound; str_plus++)
        {
            int new_blows;
            struct player_state tmpstate;

            /* Unlikely */
            if (num == max_num)
            {
                p->body.slots[weapon_slot].obj = current_weapon;
                return num;
            }

            memset(&tmpstate, 0, sizeof(tmpstate));
            tmpstate.stat_add[STAT_STR] = str_plus;
            tmpstate.stat_add[STAT_DEX] = dex_plus;
            calc_bonuses(p, &tmpstate, true, false);
            new_blows = tmpstate.num_blows;

            /* Test to make sure that this extra blow is a new str/dex combination, not a repeat */
            if (((new_blows - new_blows % 10) > (old_blows - old_blows % 10)) &&
                ((str_plus < str_done) || (str_done == -1)))
            {
                possible_blows[num].str_plus = str_plus;
                possible_blows[num].dex_plus = dex_plus;
                possible_blows[num].centiblows = new_blows;
                num++;

                str_done = str_plus;
                break;
            }

            /*
             * If the combination doesn't increment the displayed blows number, it might still
             * take a little less energy
             */
            if ((new_blows > old_blows) && ((str_plus < str_faster) || (str_faster == -1)) &&
                ((str_plus < str_done) || (str_done == -1)))
            {
                possible_blows[num].str_plus = str_plus;
                possible_blows[num].dex_plus = dex_plus;
                possible_blows[num].centiblows = -1;
                num++;

                str_faster = str_plus;
                continue;
            }
        }
    }

    /* Stop pretending */
    p->body.slots[weapon_slot].obj = current_weapon;

    return num;
}


/*
 * Describe blows.
 */
static bool describe_blows(struct player *p, const struct object *obj)
{
    int i;
    struct blow_info blow_info[STAT_RANGE * 2];
    int num_entries = 0;

    num_entries = obj_known_blows(p, obj, STAT_RANGE * 2, blow_info);
    if (num_entries == 0) return false;

    /* First entry is always current blows (+0, +0) */
    text_out_c(p, COLOUR_L_GREEN, "%d.%d ", blow_info[0].centiblows / 100,
        ((blow_info[0].centiblows / 10) % 10));
    text_out(p, "blow%s/round.\n", ((blow_info[0].centiblows > 100)? "s": ""));

    /* Then list combinations that give more blows / speed boost */
    for (i = 1; i < num_entries; i++)
    {
        struct blow_info entry = blow_info[i];

        if (entry.centiblows != -1)
        {
            text_out(p, "With +%d STR and +%d DEX you would get ", entry.str_plus,
                entry.dex_plus);
            text_out(p, "%d.%d ", entry.centiblows / 100, (entry.centiblows / 10) % 10);
            text_out(p, "blow%s/round.\n", ((entry.centiblows > 100)? "s": ""));
        }
        else
        {
            text_out(p, "With +%d STR and +%d DEX you would attack a bit faster.\n",
                entry.str_plus, entry.dex_plus);
        }
    }

    return true;
}


static int calc_damage(struct player *p, struct player_state *state, const struct object *obj,
    const struct object *bow, bool weapon, bool ammo, int mult)
{
    int dice, sides, dam, plus = 0;
    int multiplier = 1;

    /* Use displayed dice if real dice not known */
    if (obj->known->dd && obj->known->ds)
    {
        dice = obj->dd;
        sides = obj->ds;
    }
    else
    {
        dice = obj->kind->dd;
        sides = obj->kind->ds;
    }

    /* Calculate damage */
    dam = ((sides + 1) * dice * 5);

    /* Apply melee slays & brands */
    if (weapon) dam *= mult;

    /* Add object to-dam (x10) */
    if (obj->known->to_d)
    {
        s16b to_d;

        object_to_d(obj, &to_d);
        dam += to_d * 10;
    }

    /* Apply melee critical hits */
    if (weapon)
    {
        if (obj->known->to_h)
        {
            s16b to_h;

            object_to_h(obj, &to_h);
            plus = to_h;
        }
        dam = calculate_melee_crits(p, state, obj->weight, plus, dam);
    }

    /* Add player to-dam (x10) for melee weapons */
    if (weapon) dam += state->to_d * 10;

    /* Add shooter to-dam (x10) for missile weapons */
    if (ammo && bow && bow->known->to_d)
    {
        s16b to_d;

        object_to_d(bow, &to_d);
        dam += to_d * 10;
    }

    /* Apply throwing multiplier */
    if (!weapon && !ammo)
    {
        int might = 2 + obj->weight / 12;

        /* Good at throwing */
        if (player_has(p, PF_FAST_THROW)) might = 2 + (obj->weight + p->lev) / 12;

        dam *= might;
    }

    /* Calculate missile multiplier from launcher multiplier, slays & brands */
    if (!weapon)
    {
        if (ammo) multiplier = p->state.ammo_mult;
        if (mult > 1)
        {
            if (multiplier > 1) multiplier += mult;
            else multiplier = mult;
        }
    }

    /* Apply missile multiplier */
    if (!weapon) dam *= multiplier;

    /* Apply missile to-dam from temp branding (x10) */
    if (tval_is_ammo(obj) && p->timed[TMD_BOWBRAND] && !p->brand.blast) dam += p->brand.dam * 10;

    /* Apply missile critical hits */
    if (!weapon)
    {
        if (obj->known->to_h) plus = obj->to_h;
        dam = calculate_missile_crits(p, &p->state, obj->weight, plus, dam);
    }

    /* Don't show negative damage */
    if (dam < 0) dam = 0;

    /* Apply number of blows/shots per round */
    if (weapon) dam = (dam * state->num_blows) / 100;
    else if (ammo) dam = (dam * p->state.num_shots) / 10;

    return dam;
}


/*
 * Gets information about the average damage/turn that can be inflicted if
 * the player wields the given weapon.
 *
 * Fills in the damage against normal adversaries in `normal_damage`, as well
 * as the slays/brands on the weapon in slay_damage/brand_list.
 * `nonweap_slay` is set to whether other items being worn could add to the
 * damage done by branding attacks.
 */
static bool obj_known_damage(struct player *p, const struct object *obj, int *normal_damage,
    int *brand_damage, int *slay_damage, bool *nonweap_slay, bool thrown)
{
    int i;
    bool *total_brands;
    bool *total_slays;
    bool has_brands_or_slays = false;
    struct object *bow = equipped_item_by_slot_name(p, "shooting");
    bool weapon = ((tval_is_melee_weapon(obj) || tval_is_mstaff(obj)) && !thrown);
    bool ammo = ((p->state.ammo_tval == obj->tval) && !thrown);
    struct player_state state;
    int weapon_slot = slot_by_name(p, "weapon");
    struct object *current_weapon = slot_object(p, weapon_slot);
    struct object *known_bow = (bow? bow->known: NULL);

    /* Pretend we're wielding the object if it's a weapon */
    if (weapon) p->body.slots[weapon_slot].obj = (struct object *)obj;

    /* Calculate the player's hypothetical state */
    memset(&state, 0, sizeof(state));
    calc_bonuses(p, &state, true, false);

    /* Stop pretending */
    if (weapon) p->body.slots[weapon_slot].obj = current_weapon;

    /* Get the brands */
    total_brands = mem_zalloc(z_info->brand_max * sizeof(bool));
    copy_brands(&total_brands, obj->known->brands);
    if (ammo && known_bow)
        copy_brands(&total_brands, known_bow->brands);

    /* Brands */
    for (i = 0; i < z_info->brand_max; i++)
    {
        /* Handle class brands */
        if (p->clazz->brands && p->clazz->brands[i].brand && (p->lev >= p->clazz->brands[i].lvl))
            append_brand(&total_brands, i);

        /* Only for melee attacks */
        if (!weapon) continue;

        /* Handle racial brands */
        if (p->race->brands && p->race->brands[i].brand && (p->lev >= p->race->brands[i].lvl))
            append_brand(&total_brands, i);

        /* Temporary brands */
        if (player_has_temporary_brand(p, i))
            append_brand(&total_brands, i);
    }

    /* Temporary branding (missile attacks) */
    if (tval_is_ammo(obj) && p->timed[TMD_BOWBRAND])
        append_brand(&total_brands, get_bow_brand(&p->brand));

    /* Handle polymorphed players (melee attacks) */
    if (weapon && p->poly_race)
    {
        for (i = 0; i < z_info->mon_blows_max; i++)
            append_brand(&total_brands, get_poly_brand(p->poly_race, i));
    }

    /* Get the slays */
    total_slays = mem_zalloc(z_info->slay_max * sizeof(bool));
    copy_slays(&total_slays, obj->known->slays);
    if (ammo && known_bow)
        copy_slays(&total_slays, known_bow->slays);

    /* Slays */
    for (i = 0; i < z_info->slay_max; i++)
    {
        /* Only for melee attacks */
        if (!weapon) continue;

        /* Handle racial/class slays */
        if (p->race->slays && p->race->slays[i].slay && (p->lev >= p->race->slays[i].lvl))
            append_slay(&total_slays, i);
        if (p->clazz->slays && p->clazz->slays[i].slay && (p->lev >= p->clazz->slays[i].lvl))
            append_slay(&total_slays, i);

        /* Temporary slays */
        if (player_has_temporary_slay(p, i))
            append_slay(&total_slays, i);
    }

    /* Melee weapons may get slays and brands from other items */
    *nonweap_slay = false;
    if (weapon)
    {
        for (i = 2; i < p->body.count; i++)
        {
            struct object *slot_obj = slot_object(p, i);

            if (!slot_obj) continue;

            if (copy_brands(&total_brands, slot_obj->known->brands)) *nonweap_slay = true;
            if (copy_slays(&total_slays, slot_obj->known->slays)) *nonweap_slay = true;
        }
    }

    /* Get damage for each brand on the objects */
    for (i = 0; i < z_info->brand_max; i++)
    {
        int index = -1, multiplier = brands[i].multiplier;

        /* Must have the brand */
        if (total_brands[i]) has_brands_or_slays = true;
        else continue;

        brand_damage[i] = calc_damage(p, &state, obj, bow, weapon, ammo, multiplier);

        /* Hack -- double damage if vulnerable to fire or cold */
        if (streq(brands[i].name, "fire"))
            index = z_info->brand_max + multiplier - 2;
        if (streq(brands[i].name, "cold"))
            index = z_info->brand_max + multiplier;
        if (index > 0)
            brand_damage[index] = calc_damage(p, &state, obj, bow, weapon, ammo, multiplier * 2);
    }

    /* Get damage for each slay on the objects */
    for (i = 0; i < z_info->slay_max; i++)
    {
        /* Must have the slay */
        if (total_slays[i]) has_brands_or_slays = true;
        else continue;

        slay_damage[i] = calc_damage(p, &state, obj, bow, weapon, ammo, slays[i].multiplier);
    }

    /* Normal damage, not considering brands or slays */
    *normal_damage = calc_damage(p, &state, obj, bow, weapon, ammo, 1);

    mem_free(total_brands);
    mem_free(total_slays);
    return has_brands_or_slays;
}


/*
 * Describe damage.
 */
static void describe_damage(struct player *p, const struct object *obj, bool thrown)
{
    int i;
    bool nonweap_slay = false;
    int normal_damage = 0;
    int *brand_damage = mem_zalloc((z_info->brand_max + 4) * sizeof(int));
    int *slay_damage = mem_zalloc(z_info->slay_max * sizeof(int));

    /* Collect brands and slays */
    bool has_brands_or_slays = obj_known_damage(p, obj, &normal_damage, brand_damage, slay_damage,
        &nonweap_slay, thrown);

    /* Mention slays and brands from other items */
    if (nonweap_slay)
        text_out(p, "This weapon may benefit from one or more off-weapon brands or slays.\n");

    if (thrown) text_out(p, "Average thrown damage: ");
    else text_out(p, "Average damage/round: ");

    if (has_brands_or_slays)
    {
        /* Sort by decreasing damage so entries with the same damage can be printed together */
        int *sortind = mem_alloc((z_info->brand_max + 4 + z_info->slay_max) * sizeof(int));
        int nsort = 0;
        const char *lastnm;
        int lastdam, groupn;
        bool last_is_brand, last_is_fire, last_is_cold;

        /* Assemble the indices. Do the slays first so, if tied for damage, they'll appear first. */
        for (i = 0; i < z_info->slay_max; i++)
        {
            if (slay_damage[i] > 0)
            {
                sortind[nsort] = i + z_info->brand_max + 4;
                ++nsort;
            }
        }
        for (i = 0; i < z_info->brand_max + 4; i++)
        {
            if (brand_damage[i] > 0)
            {
                sortind[nsort] = i;
                ++nsort;
            }
        }

        if (nsort > 0)
        {
            /* Sort. Since the number is small, insertion sort is fine. */
            for (i = 0; i < nsort - 1; i++)
            {
                int maxdam = ((sortind[i] < z_info->brand_max + 4)? brand_damage[sortind[i]]:
                    slay_damage[sortind[i] - z_info->brand_max - 4]);
                int maxind = i;
                int j;

                for (j = i + 1; j < nsort; j++)
                {
                    int dam = ((sortind[j] < z_info->brand_max + 4)? brand_damage[sortind[j]]:
                        slay_damage[sortind[j] - z_info->brand_max - 4]);

                    if (maxdam < dam)
                    {
                        maxdam = dam;
                        maxind = j;
                    }
                }
                if (maxind != i)
                {
                    int tmp = sortind[maxind];

                    sortind[maxind] = sortind[i];
                    sortind[i] = tmp;
                }
            }

            /* Output */
            lastdam = 0;
            groupn = 0;
            lastnm = NULL;
            last_is_brand = false;
            last_is_fire = false;
            last_is_cold = false;
            for (i = 0; i < nsort; i++)
            {
                const char *tgt;
                int dam;
                bool is_brand, is_fire = false, is_cold = false;

                if (sortind[i] < z_info->brand_max + 4)
                {
                    is_brand = true;
                    if (sortind[i] < z_info->brand_max)
                        tgt = brands[sortind[i]].name;
                    else if (sortind[i] < z_info->brand_max + 2)
                    {
                        is_fire = true;
                        tgt = "fire";
                    }
                    else
                    {
                        is_cold = true;
                        tgt = "cold";
                    }
                    dam = brand_damage[sortind[i]];
                }
                else
                {
                    is_brand = false;
                    tgt = slays[sortind[i] - z_info->brand_max - 4].name;
                    dam = slay_damage[sortind[i] - z_info->brand_max - 4];
                }

                if (groupn > 0)
                {
                    if (dam != lastdam)
                    {
                        if (groupn > 2) text_out(p, ", and");
                        else if (groupn == 2) text_out(p, " and");
                    }
                    else if (groupn > 1) text_out(p, ",");
                    if (last_is_brand)
                    {
                        if (last_is_fire || last_is_cold)
                            text_out(p, " creatures susceptible to");
                        else
                            text_out(p, " creatures not resistant to");
                    }
                    text_out(p, " %s", lastnm);
                }
                if (dam != lastdam)
                {
                    if (i != 0) text_out(p, ", ");
                    if (dam % 10) text_out_c(p, COLOUR_L_GREEN, "%d.%d vs", dam / 10, dam % 10);
                    else text_out_c(p, COLOUR_L_GREEN, "%d vs", dam / 10);
                    groupn = 1;
                    lastdam = dam;
                }
                else
                {
                    my_assert(groupn > 0);
                    ++groupn;
                }
                lastnm = tgt;
                last_is_brand = is_brand;
                last_is_fire = is_fire;
                last_is_cold = is_cold;
            }
            if (groupn > 0)
            {
                if (groupn > 2) text_out(p, ", and");
                else if (groupn == 2) text_out(p, " and");
                if (last_is_brand)
                {
                    if (last_is_fire || last_is_cold)
                        text_out(p, " creatures susceptible to");
                    else
                        text_out(p, " creatures not resistant to");
                }
                text_out(p, " %s", lastnm);
            }

            text_out(p, ((nsort == 1)? " and ": ", and "));
        }
        else
            has_brands_or_slays = false;

        mem_free(sortind);
    }

    /* Normal damage, not considering brands or slays */
    if (!normal_damage)
        text_out_c(p, COLOUR_L_RED, "0");
    else if (normal_damage % 10)
        text_out_c(p, COLOUR_L_GREEN, "%d.%d", normal_damage / 10, normal_damage % 10);
    else
        text_out_c(p, COLOUR_L_GREEN, "%d", normal_damage / 10);
    if (has_brands_or_slays) text_out(p, " vs. others");
    text_out(p, ".\n");

    mem_free(brand_damage);
    mem_free(slay_damage);
}


/*
 * Gets miscellaneous combat information about the given object.
 *
 * Fills in whether there is a special effect when thrown in `thrown effect`,
 * the `range` in ft (or zero if not ammo), the percentage chance of breakage
 * and whether it is too heavy to be wielded effectively at the moment.
 */
static void obj_known_misc_combat(struct player *p, const struct object *obj,
    bool *thrown_effect, int *range, int *break_chance, bool *heavy, bool *two_handed)
{
    bool weapon = (tval_is_melee_weapon(obj) || tval_is_mstaff(obj));
    bool ammo = (p->state.ammo_tval == obj->tval);
    bool aware = object_flavor_is_aware(p, obj);

    *thrown_effect = *heavy = *two_handed = false;
    *range = *break_chance = 0;

    if (!weapon && !ammo)
    {
        /* Potions can have special text */
        if (tval_is_potion(obj) && obj->dd && obj->ds && aware)
            *thrown_effect = true;
    }

    if (ammo)
    {
        /* Range of the weapon */
        *range = MIN(6 + 2 * p->state.ammo_mult, z_info->max_range);

        /* Temporary "Farsight" */
        if (p->timed[TMD_FARSIGHT]) *range += (p->lev - 7) / 10;
    }

    /* Add breakage chance */
    *break_chance = breakage_chance(obj, true);

    /* Is the weapon too heavy? */
    if (weapon)
    {
        struct player_state state;
        int weapon_slot = slot_by_name(p, "weapon");
        struct object *current_weapon = slot_object(p, weapon_slot);

        /* Pretend we're wielding the object */
        p->body.slots[weapon_slot].obj = (struct object *)obj;

        /* Calculate the player's hypothetical state */
        memset(&state, 0, sizeof(state));
        calc_bonuses(p, &state, true, false);

        /* Stop pretending */
        p->body.slots[weapon_slot].obj = current_weapon;

        /* Warn about heavy weapons */
        *heavy = state.heavy_wield;

        /* Special -- two-handed weapons */
        if (kf_has(obj->kind->kind_flags, KF_TWO_HANDED)) *two_handed = true;
    }
}


/*
 * Describe combat advantages
 */
static bool describe_combat(struct player *p, const struct object *obj)
{
    bool weapon = (tval_is_melee_weapon(obj) || tval_is_mstaff(obj));
    bool ammo = (p->state.ammo_tval == obj->tval);
    bool throwing_weapon = (weapon && of_has(obj->flags, OF_THROWING));
    bool rock = (tval_is_ammo(obj) && of_has(obj->flags, OF_THROWING));
    int range, break_chance;
    bool thrown_effect, heavy, two_handed;

    obj_known_misc_combat(p, obj, &thrown_effect, &range, &break_chance, &heavy, &two_handed);

    /* Abort if we've nothing to say */
    if (!weapon && !ammo && !rock)
    {
        /* Potions can have special text */
        if (thrown_effect)
        {
            text_out(p, "It can be thrown at creatures with damaging effect.\n");
            return true;
        }

        return false;
    }

    text_out_c(p, COLOUR_L_WHITE, "Combat info:\n");

    /* Special -- two-handed weapons */
    if (two_handed)
        text_out_c(p, COLOUR_L_RED, "This weapon should be wielded with both hands.\n");

    /* Warn about heavy weapons */
    if (heavy)
        text_out_c(p, COLOUR_L_RED, "You are too weak to use this weapon.\n");

    /* Weapon: blows/round */
    if (weapon)
        describe_blows(p, obj);

    /* Missile: shots/round and range */
    else if (ammo)
    {
        text_out_c(p, COLOUR_L_GREEN, "%d.%d ", p->state.num_shots / 10, p->state.num_shots % 10);
        text_out(p, "shot%s/round.\n", ((p->state.num_shots > 10)? "s": ""));
        text_out(p, "When fired, hits targets up to ");
        text_out_c(p, COLOUR_L_GREEN, "%d", range * 10);
        text_out(p, " feet away.\n");
    }

    /* Describe damage */
    if (weapon || ammo) describe_damage(p, obj, false);
    if (throwing_weapon || rock) describe_damage(p, obj, true);

    /* Add breakage chance */
    if (ammo && !of_has(obj->flags, OF_AMMO_MAGIC) && !obj->artifact)
    {
        text_out_c(p, COLOUR_L_GREEN, "%d%%", break_chance);
        text_out(p, " chance of breaking upon contact.\n");
    }

    /* Something has been said */
    return true;
}


/*
 * Returns information about objects that can be used for digging.
 *
 * `deciturns` will be filled in with the avg number of deciturns it will
 * take to dig through each type of diggable terrain, and must be at least
 * [DIGGING_MAX].
 *
 * Returns false if the object has no effect on digging.
 */
static bool obj_known_digging(struct player *p, const struct object *obj, int deciturns[])
{
    struct player_state state;
    int i;
    int chances[DIGGING_MAX];
    int slot = wield_slot(p, obj);
    struct object *current;
    bool equipped = object_is_equipped(p->body, obj);
    s32b modifiers[OBJ_MOD_MAX];

    object_modifiers(obj, modifiers);

    /* Doesn't remotely resemble a digger */
    if (!tval_is_digger(obj) && (modifiers[OBJ_MOD_TUNNEL] <= 0)) return false;

    /* Player has no digging info */
    if ((modifiers[OBJ_MOD_TUNNEL] || !tval_is_digger(obj)) &&
        !obj->known->modifiers[OBJ_MOD_TUNNEL])
    {
        return false;
    }

    /* Pretend we're wielding the object, unless already equipped */
    if (!equipped)
    {
        current = slot_object(p, slot);
        p->body.slots[slot].obj = (struct object *)obj;
    }

    /* Calculate the player's hypothetical state */
    memset(&state, 0, sizeof(state));
    calc_bonuses(p, &state, true, false);

    /* Stop pretending */
    if (!equipped) p->body.slots[slot].obj = current;

    calc_digging_chances(p, &state, chances);

    /* Digging chance is out of 1600 */
    for (i = DIGGING_TREE; i < DIGGING_MAX; i++)
    {
        int chance = MIN(1600, chances[i]);

        deciturns[i] = (chance? (16000 / chance): 0);
    }

    return true;
}


/*
 * Describe objects that can be used for digging
 */
static bool describe_digger(struct player *p, const struct object *obj)
{
    int i;
    int deciturns[DIGGING_MAX];
    static const char *names[DIGGING_MAX] =
    {
        "vegetation", "rubble", "magma veins", "quartz veins", "granite", ""
    };

    /* Get useful info or print nothing */
    if (!obj_known_digging(p, obj, deciturns)) return false;

    for (i = DIGGING_TREE; i < DIGGING_DOORS; i++)
    {
        if ((i == 0) && (deciturns[0] > 0))
        {
            if (tval_is_melee_weapon(obj) || tval_is_mstaff(obj))
                text_out(p, "Clears ");
            else
                text_out(p, "With this item, your current weapon clears ");
        }
        if ((i == 4) || ((i != 0) && (deciturns[i] == 0))) text_out(p, "and ");
        if (deciturns[i] == 0)
        {
            text_out_c(p, COLOUR_L_RED, "doesn't affect ");
            text_out(p, "%s.\n", names[i]);
            break;
        }
        text_out(p, "%s in ", names[i]);
        if (deciturns[i] == 10) text_out_c(p, COLOUR_L_GREEN, "1 ");
        else if (deciturns[i] < 100)
            text_out_c(p, COLOUR_GREEN, "%d.%d ", deciturns[i] / 10, deciturns[i] % 10);
        else
        {
            text_out_c(p, ((deciturns[i] < 1000)? COLOUR_YELLOW: COLOUR_RED), "%d ",
                (deciturns[i] + 5) / 10);
        }
        text_out(p, "turn%s%s", ((deciturns[i] == 10)? "": "s"), ((i == 4)? ".\n": ", "));
    }

    /* You always have something to say... */
    return true;
}


/*
 * Gives the known light-sourcey characteristics of the given object.
 *
 * Fills in the intensity of the light in `intensity`, whether it uses fuel and
 * how many turns light it can refuel in similar items.
 *
 * Return false if the object is not known to be a light source (which
 * includes it not actually being a light source).
 */
static bool obj_known_light(struct player *p, const struct object *obj, int mode, int *intensity,
    bool *uses_fuel, int *refuel_turns)
{
    bool no_fuel;
    bool is_light = tval_is_light(obj);
    s32b modifiers[OBJ_MOD_MAX];
    bool known = object_is_known(p, obj);
    bool known_light = (known || obj->known->modifiers[OBJ_MOD_LIGHT]);
    bitflag flags[OF_SIZE];
    bool artifact = (obj->artifact? true: false);

    object_modifiers(obj, modifiers);
    if (!is_light && (modifiers[OBJ_MOD_LIGHT] <= 0)) return false;
    if (modifiers[OBJ_MOD_LIGHT] && !known_light) return false;

    /* Work out intensity */
    if (of_has(obj->flags, OF_LIGHT_2)) *intensity = 2;
    else if (of_has(obj->flags, OF_LIGHT_3)) *intensity = 3;
    else if (of_has(obj->flags, OF_LIGHT_4)) *intensity = 4;
    if (known_light) *intensity += modifiers[OBJ_MOD_LIGHT];

    /*
     * Prevent unidentified objects (especially artifact lights) from showing
     * bad intensity and refueling info.
     */
    if (*intensity == 0) return false;

    get_known_flags(obj, mode, flags, object_flavor_is_aware(p, obj));
    no_fuel = of_has(flags, OF_NO_FUEL);

    if (no_fuel && !artifact)
        *uses_fuel = false;
    else
        *uses_fuel = true;

    /* Only show refueling info for known objects */
    if (known)
    {
        if (is_light && !no_fuel && of_has(obj->flags, OF_TAKES_FUEL))
            *refuel_turns = z_info->fuel_lamp;
        else
            *refuel_turns = 0;
    }
    else
        *refuel_turns = -1;

    return true;
}


/*
 * Describe things that look like lights.
 */
static bool describe_light(struct player *p, const struct object *obj, int mode)
{
    int intensity = 0;
    bool uses_fuel = false;
    int refuel_turns = 0;
    bool terse = ((mode & OINFO_TERSE)? true: false);

    if (!obj_known_light(p, obj, mode, &intensity, &uses_fuel, &refuel_turns))
        return false;

    if (!tval_is_light(obj)) return false;

    text_out(p, "Intensity ");
    text_out_c(p, COLOUR_L_GREEN, "%d", intensity);
    text_out(p, " light.");

    if (!uses_fuel)
        text_out(p, " No fuel required.");

    /* Lamps can refill other lamps */
    if (!terse)
    {
        if (refuel_turns > 0)
            text_out(p, " Refills other lamps up to %d turns of fuel.", refuel_turns);
        else if (refuel_turns == 0)
            text_out(p, " Cannot be refueled.");
    }

    text_out(p, "\n");

    return true;
}


/*
 * Describe readable books.
 */
static bool describe_book(struct player *p, const struct object *obj)
{
    if (!obj_can_browse(p, obj)) return false;

    text_out(p, "You can read this book.\n");

    return true;
}


/*
 * Can the effect be aimed?
 *
 * Based on obj_needs_aim().
 */
bool obj_can_aim(struct player *p, const struct object *obj)
{
    bool aim = effect_aim(object_effect(obj));
    bool aware = object_flavor_is_aware(p, obj);

    /* Wands */
    if (tval_is_wand(obj)) return true;

    /* Rods that require aiming or unknown rods */
    if (tval_can_have_timeout(obj) && (aim || !aware)) return true;

    /* Aware flavored items or unflavored flavored with known effect */
    if (aim) return ((aware && obj->kind->flavor) || object_effect_is_known(obj, aware));

    return false;
}


/*
 * Gives the known effects of using the given item.
 *
 * Fills in:
 *  - the effect, or NULL if there is an effect but details are unknown
 *  - whether the effect can be aimed
 *  - the minimum and maximum time in game turns for the item to recharge
 *    (or zero if it does not recharge)
 *  - the percentage chance of the effect failing when used
 *
 * Return false if the object has no effect.
 */
static bool obj_known_effect(struct player *p, const struct object *obj, struct effect **effect,
    bool *aimed, int *min_recharge, int *max_recharge, int *failure_chance)
{
    random_value timeout = obj->time;

    *effect = object_effect(obj);
    *min_recharge = 0;
    *max_recharge = 0;
    *failure_chance = 0;
    *aimed = obj_can_aim(p, obj);

    /* No effect - no info */
    if (!*effect) return false;

    /* Don't know much - be vague */
    if (!object_effect_is_known(obj, object_flavor_is_aware(p, obj)))
    {
        *effect = NULL;
        return true;
    }

    if (randcalc(timeout, 0, MAXIMISE) > 0)
    {
        *min_recharge = randcalc(timeout, 0, MINIMISE);
        *max_recharge = randcalc(timeout, 0, MAXIMISE);
    }

    if (tval_can_have_nourishment(obj) || tval_is_scroll(obj))
        *failure_chance = 0;
    else
        *failure_chance = get_use_device_chance(p, obj);

    return true;
}


/*
 * Describe an object's effect, if any.
 */
static bool describe_effect(struct player *p, const struct object *obj, bool only_artifacts)
{
    struct effect *effect = NULL;
    bool aimed = false, has_desc = false;
    int min_time, max_time, failure_chance;

    /* Sometimes only print artifact activation info */
    /* PWMAngband: also print ego activation info */
    if (only_artifacts && !(obj->artifact || obj->ego)) return false;

    if (!obj_known_effect(p, obj, &effect, &aimed, &min_time, &max_time, &failure_chance))
        return false;

    /* Effect not known, mouth platitudes */
    if (!effect && object_effect(obj))
    {
        if (tval_is_edible(obj))
            text_out(p, "It can be eaten.\n");
        else if (tval_is_potion(obj))
            text_out(p, "It can be quaffed.\n");
        else if (tval_is_scroll(obj))
            text_out(p, "It can be read.\n");
        else if (aimed)
            text_out(p, "It can be aimed.\n");
        else
            text_out(p, "It can be activated.\n");

        return true;
    }

    /* Activations get a special message */
    if (obj->activation && obj->activation->desc)
    {
        if (aimed)
            text_out(p, "When aimed, it ");
        else
            text_out(p, "When activated, it ");
        print_effect(p, obj->activation->desc);
    }
    else
    {
        /* Get descriptions for all the effects */
        effect = object_effect(obj);
        while (effect)
        {
            if (effect_desc(effect))
            {
                has_desc = true;
                break;
            }
            effect = effect->next;
        }
        if (!has_desc) return false;
        effect = object_effect(obj);

        if (tval_is_edible(obj))
            text_out(p, "When eaten, it ");
        else if (tval_is_potion(obj))
            text_out(p, "When quaffed, it ");
        else if (tval_is_scroll(obj))
            text_out(p, "When read, it ");
        else if (aimed)
            text_out(p, "When aimed, it ");
        else
            text_out(p, "When activated, it ");

        /* Print a colourised description */
        if (!effect_describe(p, obj, effect)) return false;
    }

    if (min_time || max_time)
    {
        /* Adjust for player speed */
        int multiplier = turn_energy(p->state.speed);

        text_out(p, ".\nTakes ");

        /* Correct for player speed */
        min_time = (min_time * multiplier) / 10;
        max_time = (max_time * multiplier) / 10;

        text_out_c(p, COLOUR_L_GREEN, "%d", min_time);

        if (min_time != max_time)
        {
            text_out(p, " to ");
            text_out_c(p, COLOUR_L_GREEN, "%d", max_time);
        }

        text_out(p, " turns to recharge");
        if (p->state.speed != 110) text_out(p, " at your current speed");
    }

    text_out(p, ".\n");

    if (failure_chance > 0)
    {
        text_out(p, "Your chance of success is %d.%d%%\n", (1000 - failure_chance) / 10,
            (1000 - failure_chance) % 10);
    }

    return true;
}


/*
 * Describe an item's origin
 */
static bool describe_origin(struct player *p, const struct object *obj, bool terse)
{
    char loot_spot[NORMAL_WID];
    char name[NORMAL_WID];
    int origin;
    const char *dropper = NULL;
    const char *article;
    bool unique = false;

    /* Only give this info in chardumps if wieldable */
    if (terse && !obj_can_wear(p, obj)) return false;

    /* Set the origin - care needed for mimics */
    if ((obj->origin == ORIGIN_DROP_MIMIC) && (obj->mimicking_m_idx != 0))
        origin = ORIGIN_FLOOR;
    else
        origin = obj->origin;

    /* Name the place of origin */
    if (obj->origin_depth)
    {
        strnfmt(loot_spot, sizeof(loot_spot), "at %d feet (level %d)", obj->origin_depth * 50,
            obj->origin_depth);
    }
    else
        my_strcpy(loot_spot, "on the surface", sizeof(loot_spot));

    /* Name the monster of origin */
    if (obj->origin_race)
    {
        dropper = obj->origin_race->name;
        if (monster_is_unique(obj->origin_race)) unique = true;
    }
    else
        dropper = "monster lost to history";
    article = (is_a_vowel(dropper[0])? "an ": "a ");
    if (unique)
        my_strcpy(name, dropper, sizeof(name));
    else
    {
        my_strcpy(name, article, sizeof(name));
        my_strcat(name, dropper, sizeof(name));
    }

    /* Print an appropriate description */
    switch (origins[origin].args)
    {
        case -1: return false;
        case 0: text_out(p, origins[origin].desc); break;
        case 1: text_out(p, origins[origin].desc, loot_spot); break;
        case 2: text_out(p, origins[origin].desc, name, loot_spot); break;
    }

    text_out(p, "\n\n");

    return true;
}


/*
 * Print an item's flavour text.
 *
 * obj is the object we are describing
 */
static bool describe_flavor_text(struct player *p, const struct object *obj)
{
    /* Display the known artifact or object description */
    if (true_artifact_p(obj) && obj->known->artifact && obj->artifact->text)
    {
        text_out(p, "%s\n\n", obj->artifact->text);
        return true;
    }
    if (object_flavor_is_aware(p, obj))
    {
        bool did_desc = false;

        if (obj->kind->text)
        {
            text_out(p, "%s", obj->kind->text);
            did_desc = true;
        }

        /* Display an additional ego-item description */
        if (obj->ego && obj->known->ego && obj->ego->text)
        {
            if (did_desc) text_out(p, " ");
            text_out(p, "%s", obj->ego->text);
            did_desc = true;
        }

        if (did_desc) text_out(p, "\n\n");
        return did_desc;
    }

    return false;
}


/*
 * Output code
 */


/*
 * Output object information
 */
static void object_info_out(struct player *p, const struct object *obj, int mode)
{
    bitflag flags[OF_SIZE], flags_misc[OF_SIZE];
    struct element_info el_info[ELEM_MAX];
    bool something = false, origin_or_desc = false;
    bool aware = object_flavor_is_aware(p, obj);
    bool terse = ((mode & OINFO_TERSE)? true: false);
    int am, i;

    /* Hack -- "wearable" items other than weapons/ammo add slays/brands to melee attacks */
    bool fulldesc = (tval_has_variable_power(obj) && !tval_is_enchantable_weapon(obj));

    /* Unaware objects get simple descriptions */
    if (object_marked_aware(p, obj))
    {
        text_out(p, "You do not know what this is.\n");
        return;
    }

    /* Grab the object flags */
    get_known_flags(obj, mode, flags, aware);

    /* Grab the element info */
    get_known_elements(obj, mode, el_info, aware);

    /* Print origin and descriptive text for a given object */
    if (describe_origin(p, obj, terse)) origin_or_desc = true;
    if (!terse)
    {
        if (describe_flavor_text(p, obj)) origin_or_desc = true;
    }

    /* Unidentified item */
    if (!object_fully_known(p, obj) && object_was_sensed(obj))
    {
        text_out(p, "You do not know the full extent of this item's powers.\n");
        something = true;
    }

    /* Hack -- don't display misc magic flags from curses (description is enough) */
    of_copy(flags_misc, flags);
    for (i = 0; obj->known->curses && (i < z_info->curse_max); i++)
    {
        if (obj->known->curses[i].power == 0) continue;
        of_diff(flags_misc, curses[i].obj->flags);
    }

    /* Describe bits */
    if (describe_curses(p, obj)) something = true;
    if (describe_stats(p, obj, aware)) something = true;
    if (describe_slays(p, obj, fulldesc)) something = true;
    if (describe_brands(p, obj, fulldesc)) something = true;
    if (describe_elements(p, el_info)) something = true;
    if (describe_protects(p, flags)) something = true;
    if (describe_ignores(p, el_info)) something = true;
    if (describe_hates(p, el_info)) something = true;
    if (describe_sustains(p, flags)) something = true;
    if (describe_misc_magic(p, flags_misc)) something = true;
    if (describe_light(p, obj, mode)) something = true;
    if (describe_book(p, obj)) something = true;
    if (describe_esp(p, flags)) something = true;
    if (something) text_out(p, "\n");

    /* PWMAngband: dark swords have an antimagic field */
    am = antimagic_field(obj, flags);
    if (am)
    {
        something = true;
        text_out_c(p, COLOUR_L_DARK, "Has a %d%% chance of suppressing magic.\n", am);
    }

    if (describe_effect(p, obj, terse))
    {
        something = true;
        text_out(p, "\n");
    }

    /* Describe combat bits */
    if (describe_combat(p, obj))
    {
        something = true;
        text_out(p, "\n");
    }

    /* Describe boring bits */
    if (!terse && describe_digger(p, obj)) something = true;

    /* Don't append anything in terse (for character dump), since that seems to cause extra linebreaks */
    if (!something)
    {
        if (!terse)
            text_out(p, "\n\nThis item does not seem to possess any special abilities.");
        else if (!origin_or_desc)
            text_out(p, "\n");
    }

    /* Ownership and level requirement */
    if (obj->level_req && !terse)
    {
        text_out(p, "\n");
        if (obj->owner > 0)
        {
            hash_entry *ptr = lookup_player(obj->owner);
            const char *owner_name = ((ptr && ht_zero(&ptr->death_turn))? ptr->name: "(deceased)");

            text_out(p, "Owner: %s\n", owner_name);
        }
        text_out_c(p, COLOUR_L_RED, "Level requirement: %d", obj->level_req);
    }

    /* Preferred price */
    if (object_fully_known(p, obj) && !terse)
    {
        /* Use black market price */
        double price = (double)object_value(p, obj, 1) * 2;

        text_out(p, "\n");
        text_out_c(p, COLOUR_YELLOW, "Preferred price per unit: %ld au", (s32b)price);
    }
}


/*
 * Provide information on an item, including how it would affect the current
 * player's state.
 */
void object_info(struct player *p, const struct object *obj, int mode)
{
    object_info_out(p, obj, mode);
}


/*
 * Provide information on an item suitable for writing to the character dump - keep it brief.
 */
void object_info_chardump(struct player *p, ang_file *f, const struct object *obj)
{
    int i, j;
    char buf[NORMAL_WID + 1];
    char *s;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Dump info into player */
    object_info_out(p, obj, OINFO_TERSE);

    /* Restore height and width of current dungeon level */
    text_out_done_no_newline(p);

    /* Dump info into file */
    for (i = 0; i < p->last_info_line; i++)
    {
        /* Initialize buffer */
        memset(buf, 0, sizeof(buf));

        /* Copy buffer, indent by 5 */
        for (j = 0; j < 5; j++) buf[j] = ' ';
        for (j = 5; j < NORMAL_WID; j++) buf[j] = p->info[i][j - 5].c;

        /* Back up over spaces */
        s = buf + strlen(buf);
        while ((s > buf) && (s[-1] == ' ')) --s;
        *s = '\0';
        
        /* Dump buffer */
        file_putf(f, "%s\n", buf);
    }
}
