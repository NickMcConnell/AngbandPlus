/*
 * File: obj-info.c
 * Purpose: Object description code.
 *
 * Copyright (c) 2004 Robert Ruehlmann
 * Copyright (c) 2010 Andi Sidwell
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


/*
 * Describes a flag-name pair
 */
struct flag_type
{
    int flag;
    const char *name;
};


/*
 * Describes the number of blows possible for given stat bonuses
 */
struct blow_info
{
    int str_plus;
    int dex_plus;
    int centiblows;
};


/*** Big fat data tables ***/


static const struct flag_type elements[] =
{
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) {ELEM_##a, b},
    #include "../common/list-elements.h"
    #undef ELEM
    {ELEM_MAX, NULL}
};


static const struct flag_type mod_flags[] =
{
    #define STAT(a, b, c, d, e, f, g, h) {OBJ_MOD_##a, h},
    #include "../common/list-stats.h"
    #undef STAT
    #define OBJ_MOD(a, b, c, d) {OBJ_MOD_##a, d},
    #include "../common/list-object-modifiers.h"
    #undef OBJ_MOD
    {OBJ_MOD_MAX, ""}
};


static const struct flag_type protect_flags[] =
{
    {OF_PROT_FEAR, "fear"},
    {OF_PROT_BLIND, "blindness"},
    {OF_PROT_CONF, "confusion"},
    {OF_PROT_STUN, "stunning"}
};


static const struct flag_type sustain_flags[] =
{
    #define STAT(a, b, c, d, e, f, g, h) {OF_##c, h},
    #include "../common/list-stats.h"
    #undef STAT
    {0, ""}
};


static const struct flag_type misc_flags[] =
{
    {OF_BLESSED, "Blessed by the gods"},
    {OF_SLOW_DIGEST, "Slows your metabolism"},
    {OF_IMPAIR_HP, "Impairs hitpoint recovery"},
    {OF_IMPAIR_MANA, "Impairs mana recovery"},
    {OF_AFRAID, "Makes you afraid of melee, and worse at shooting and casting spells"},
    {OF_FEATHER, "Makes you fall like a feather"},
    {OF_REGEN, "Speeds regeneration"},
    {OF_FREE_ACT, "Prevents paralysis"},
    {OF_HOLD_LIFE, "Sustains your life force"},
    {OF_SEE_INVIS, "Grants the ability to see invisible things"},
    {OF_KNOWLEDGE, "Identifies all items for you"},
    {OF_ANTI_MAGIC, "Creates an anti-magic field"},
    {OF_AGGRAVATE, "Aggravates nearby creatures"},
    {OF_DRAIN_EXP, "Drains experience"},
    {OF_TELEPORT, "Induces random teleportation"}
};


static const struct flag_type esp_flags[] =
{
    {OF_ESP_ALL, "Grants telepathy"},
    {OF_ESP_ANIMAL, "Grants the ability to sense animals"},
    {OF_ESP_EVIL, "Grants the ability to sense evil"},
    {OF_ESP_UNDEAD, "Grants the ability to sense undead"},
    {OF_ESP_DEMON, "Grants the ability to sense demons"},
    {OF_ESP_ORC, "Grants the ability to sense orcs"},
    {OF_ESP_TROLL, "Grants the ability to sense trolls"},
    {OF_ESP_GIANT, "Grants the ability to sense giants"},
    {OF_ESP_DRAGON, "Grants the ability to sense dragons"},
    {OF_ESP_RADIUS, "Grants telepathic awareness"}
};


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


static struct
{
    int index;
    int args;
    int efinfo_flag;
    const char *desc;
} base_descs[] =
{
    {EF_NONE, 0, EFINFO_NONE, ""},
    #define EFFECT(x, a, b, c, d, e) {EF_##x, c, d, e},
    #include "list-effects.h"
    #undef EFFECT
    {EF_MAX, 0, EFINFO_NONE, ""}
};


/*** Utility code ***/


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
 * Fills recepticle with all the flags in `flags` that are in the given `list`.
 */
static size_t flag_info_collect(const struct flag_type list[], size_t max,
    const bitflag flags[OF_SIZE], const char *recepticle[])
{
    size_t i, count = 0;

    for (i = 0; i < max; i++)
    {
        if (of_has(flags, list[i].flag)) recepticle[count++] = list[i].name;
    }

    return count;
}


/*
 * Fills recepticle with all the elements that correspond to the given `list`.
 */
static size_t element_info_collect(const bool list[], const char *recepticle[])
{
    size_t i, count = 0;

    for (i = 0; i < ELEM_MAX; i++)
    {
        if (list[i]) recepticle[count++] = elements[i].name;
    }

    return count;
}


/*** Code that makes use of the data tables ***/


/*
 * Describe an item's curses.
 */
static bool describe_curses(struct player *p, const struct object *obj,
    const bitflag flags[OF_SIZE])
{
    if (of_has(flags, OF_PERMA_CURSE))
        text_out_c(p, COLOUR_L_RED, "Permanently cursed.\n");
    else if (of_has(flags, OF_HEAVY_CURSE))
        text_out_c(p, COLOUR_L_RED, "Heavily cursed.\n");
    else if (of_has(flags, OF_LIGHT_CURSE))
        text_out_c(p, COLOUR_L_RED, "Cursed.\n");
    else
        return false;

    return true;
}


/*
 * Describe stat modifications.
 */
static bool describe_stats(struct player *p, const struct object *obj, bool aware)
{
    size_t count = 0, i;

    /* Fact of but not size of mods is known for egos and flavoured items the player is aware of */
    bool known_effect = false;

    if (object_ego_is_visible(obj)) known_effect = true;
    if (tval_can_have_flavor(obj) && aware) known_effect = true;

    /* See what we've got */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        const char *desc = mod_flags[i].name;
        int val = obj->modifiers[mod_flags[i].flag];

        if (!val) continue;
        if (!mod_flags[i].name[0]) continue;
        count++;

        /* Either all mods are visible, or none are */
        if (object_this_mod_is_visible(obj, mod_flags[i].flag))
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
    const char *p_descs[N_ELEMENTS(protect_flags)];
    size_t count;

    /* Protections */
    count = flag_info_collect(protect_flags, N_ELEMENTS(protect_flags), flags, p_descs);
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
    size_t count = flag_info_collect(sustain_flags, STAT_MAX, flags, descs);

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
    size_t i;
    bool printed = false;

    for (i = 0; i < N_ELEMENTS(misc_flags); i++)
    {
        if (of_has(flags, misc_flags[i].flag))
        {
            text_out(p, "%s. ", misc_flags[i].name);
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
    struct slay *s = obj->known->slays;

    if (!s) return false;

    if (fulldesc) text_out(p, "It causes your melee attacks to slay ");
    else text_out(p, "Slays ");

    while (s)
    {
        text_out(p, s->name);
        if (s->multiplier > 3) text_out(p, " (powerfully)");
        if (s->next) text_out(p, ", ");
        else text_out(p, ".\n");
        s = s->next;
    }

    return true;
}


/*
 * Describe brands granted by an object.
 */
static bool describe_brands(struct player *p, const struct object *obj, bool fulldesc)
{
    struct brand *b = obj->known->brands;
    struct brand *known_brands = brand_collect(b, NULL);
    bool weapon = (tval_is_melee_weapon(obj) || tval_is_mstaff(obj));
    bool ammo = (p->state.ammo_tval == obj->tval);
    int i;

    /* Handle polymorphed players */
    if (weapon && p->poly_race)
    {
        for (i = 0; i < z_info->mon_blows_max; i++)
            append_fixed_brand(&known_brands, get_poly_brand(p->poly_race, i), 3);
    }

    /* Hack -- extract temp branding */
    if (ammo && p->timed[TMD_BOWBRAND])
        append_fixed_brand(&known_brands, get_bow_brand(&p->brand), 3);

    /* Hack -- extract temp branding */
    if (weapon && p->timed[TMD_SGRASP])
        append_fixed_brand(&known_brands, ELEM_ELEC, 3);

    if (!known_brands) return false;

    if (fulldesc) text_out(p, "It brands your melee attacks with ");
    else text_out(p, "Branded with ");

    b = known_brands;
    while (b)
    {
        if (b->multiplier < 3) text_out(p, "weak ");
        text_out(p, b->name);
        if (b->next) text_out(p, ", ");
        else text_out(p, ".\n");
        b = b->next;
    }

    free_brand(known_brands);
    return true;
}


/*
 * Describe monster ESP.
 */
static bool describe_esp(struct player *p, const bitflag flags[OF_SIZE])
{
    size_t i;
    bool printed = false;

    for (i = 0; i < N_ELEMENTS(esp_flags); i++)
    {
        if (of_has(flags, esp_flags[i].flag))
        {
            text_out(p, "%s.\n", esp_flags[i].name);
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
    int k, to_crit = weight + 5 * (state->to_h + plus) + 3 * p->lev;
    int crit_dam = 0;

    to_crit = MIN(5000, MAX(0, to_crit));

    /* Extract average critical damage */
    for (k = weight + 1; k <= weight + 650; k++)
    {
        if (k < 400) crit_dam += dam * 2 + 5;
        else if (k < 700) crit_dam += dam * 2 + 10;
        else if (k < 900) crit_dam += dam * 3 + 15;
        else if (k < 1300) crit_dam += dam * 3 + 20;
        else crit_dam += dam * 7 / 2 + 25;
    }
    crit_dam /= 650;

    /* Chance */
    crit_dam = (crit_dam * to_crit + dam * (5000 - to_crit)) / 5000;

    /* Apply Touch of Death */
    if (p->timed[TMD_DEADLY])
        crit_dam = (crit_dam * 3 + (dam * 7 / 2 + 30)) / 4;

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

    /* Grab the element info */
    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Report known element info */
        if (object_element_is_known(obj, i, aware))
        {
            el_info[i].res_level = obj->el_info[i].res_level;
            el_info[i].flags = obj->el_info[i].flags;
        }
        else
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
    int str_plus, dex_plus, old_blows, new_blows;
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

    dex_plus_bound = STAT_RANGE - state.stat_ind[STAT_DEX];
    str_plus_bound = STAT_RANGE - state.stat_ind[STAT_STR];

    /* Check to see if extra STR or DEX would yield extra blows */
    for (dex_plus = 0; dex_plus < dex_plus_bound; dex_plus++)
    {
        for (str_plus = 0; str_plus < str_plus_bound; str_plus++)
        {
            struct player_state tmpstate;

            if (num == max_num)
            {
                p->body.slots[weapon_slot].obj = current_weapon;
                return num;
            }

            memset(&tmpstate, 0, sizeof(tmpstate));

            tmpstate.stat_add[STAT_STR] = str_plus;
            tmpstate.stat_add[STAT_DEX] = dex_plus;

            calc_bonuses(p, &tmpstate, true, false);

            old_blows = state.num_blows;
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
    if (object_attack_plusses_are_visible(obj))
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
    if (object_attack_plusses_are_visible(obj))
        dam += obj->to_d * 10;

    /* Apply melee critical hits */
    if (weapon)
    {
        if (object_attack_plusses_are_visible(obj))
            plus = obj->to_h;
        dam = calculate_melee_crits(p, state, obj->weight, plus, dam);
    }

    /* Add player to-dam (x10) for melee weapons */
    if (weapon) dam += state->to_d * 10;

    /* Add shooter to-dam (x10) for missile weapons */
    if (ammo && bow && object_attack_plusses_are_visible(bow))
        dam += bow->to_d * 10;

    /* Calculate missile multiplier from launcher multiplier, slays & brands */
    if (ammo)
    {
        multiplier = p->state.ammo_mult;
        if (mult > 1)
        {
            if (multiplier > 1) multiplier += mult;
            else multiplier = mult;
        }
    }

    /* Apply missile multiplier */
    if (ammo) dam *= multiplier;

    /* Apply missile to-dam from temp branding (x10) */
    if (ammo && p->timed[TMD_BOWBRAND] && !p->brand.blast) dam += p->brand.dam * 10;

    /* Apply missile critical hits */
    if (ammo)
    {
        if (object_attack_plusses_are_visible(obj))
            plus = obj->to_h;
        dam = calculate_missile_crits(p, &p->state, obj->weight, plus, dam);
    }

    /* Don't show negative damage */
    if (dam < 0) dam = 0;

    /* Apply number of blows/shots per round */
    if (weapon) dam = (dam * state->num_blows) / 100;
    else dam *= p->state.num_shots;

    return dam;
}


/*
 * Gets information about the average damage/turn that can be inflicted if
 * the player wields the given weapon.
 *
 * Fills in the damage against normal adversaries in `normal_damage`, as well
 * as the slays/brands on the weapon in slay_list/brand_list.
 * `nonweap_slay` is set to whether other items being worn could add to the
 * damage done by branding attacks.
 *
 * Returns the number of slays populated in slay_list[] and slay_damage[].
 */
static bool obj_known_damage(struct player *p, const struct object *obj, int *normal_damage,
    struct brand **brand_list, struct slay **slay_list, bool *nonweap_slay)
{
    int i;
    struct object *bow = equipped_item_by_slot_name(p, "shooting");
    bool weapon = (tval_is_melee_weapon(obj) || tval_is_mstaff(obj));
    bool ammo = (p->state.ammo_tval == obj->tval);
    struct player_state state;
    int weapon_slot = slot_by_name(p, "weapon");
    struct object *current_weapon = slot_object(p, weapon_slot);
    struct brand *brand;
    struct slay *slay;
    struct object *known_bow = (bow? bow->known: NULL);

    /* Pretend we're wielding the object if it's a weapon */
    if (weapon)
        p->body.slots[weapon_slot].obj = (struct object *)obj;

    /* Calculate the player's hypothetical state */
    memset(&state, 0, sizeof(state));
    calc_bonuses(p, &state, true, false);

    /* Stop pretending */
    p->body.slots[weapon_slot].obj = current_weapon;

    /* Get the brands */
    *brand_list = brand_collect(obj->known->brands, (ammo? known_bow: NULL));

    /* Handle polymorphed players */
    if (weapon && p->poly_race)
    {
        for (i = 0; i < z_info->mon_blows_max; i++)
            append_fixed_brand(brand_list, get_poly_brand(p->poly_race, i), 3);
    }

    /* Hack -- extract temp branding */
    if (ammo && p->timed[TMD_BOWBRAND])
        append_fixed_brand(brand_list, get_bow_brand(&p->brand), 3);

    /* Hack -- extract temp branding */
    if (weapon && p->timed[TMD_SGRASP])
        append_fixed_brand(brand_list, ELEM_ELEC, 3);

    /* Get the slays */
    *slay_list = slay_collect(obj->known->slays, (ammo? known_bow: NULL));

    /* Melee weapons may get slays and brands from other items */
    *nonweap_slay = false;
    if (weapon)
    {
        for (i = 2; i < p->body.count; i++)
        {
            struct object *slot_obj = slot_object(p, i);

            if (!slot_obj) continue;

            for (slay = slot_obj->known->slays; slay; slay = slay->next)
            {
                *nonweap_slay = true;
                append_fixed_slay(slay_list, slay->name, slay->race_flag, slay->multiplier);
            }
            for (brand = slot_obj->known->brands; brand; brand = brand->next)
            {
                *nonweap_slay = true;
                append_fixed_brand(brand_list, brand->element, brand->multiplier);
            }
        }
    }

    /* Hack -- double damage if vulnerable to fire or cold */
    for (brand = *brand_list; brand; brand = brand->next)
    {
        if ((brand->element == ELEM_FIRE) || (brand->element == ELEM_COLD))
        {
            struct brand *b = mem_zalloc(sizeof(*b));

            b->name = string_make(brand->name);
            b->element = brand->element;
            b->multiplier = brand->multiplier * 2;
            b->next = brand->next;
            brand->next = b;
            brand = b;
        }
    }

    /* Get damage for each brand on the objects */
    for (brand = *brand_list; brand; brand = brand->next)
        brand->damage = calc_damage(p, &state, obj, bow, weapon, ammo, brand->multiplier);

    /* Get damage for each slay on the objects */
    for (slay = *slay_list; slay; slay = slay->next)
        slay->damage = calc_damage(p, &state, obj, bow, weapon, ammo, slay->multiplier);

    /* Normal damage, not considering brands or slays */
    *normal_damage = calc_damage(p, &state, obj, bow, weapon, ammo, 1);

    return (*slay_list || *brand_list);
}


/*
 * Describe damage.
 */
static void describe_damage(struct player *p, const struct object *obj)
{
    bool nonweap_slay = false;
    int normal_damage;
    struct brand *brand, *brands = NULL;
    struct slay *slay, *slays = NULL;
    bool has_brands_or_slays;

    /* Collect brands and slays */
    has_brands_or_slays = obj_known_damage(p, obj, &normal_damage, &brands, &slays,
        &nonweap_slay);

    /* Mention slays and brands from other items */
    if (nonweap_slay)
        text_out(p, "This weapon may benefit from one or more off-weapon brands or slays.\n");

    text_out(p, "Average damage/round: ");

    /* Output damage for creatures effected by the brands */
    for (brand = brands; brand; brand = brand->next)
    {
        if (!brand->damage)
            text_out_c(p, COLOUR_L_RED, "0");
        else if (brand->damage % 10)
            text_out_c(p, COLOUR_L_GREEN, "%d.%d", brand->damage / 10, brand->damage % 10);
        else
            text_out_c(p, COLOUR_L_GREEN, "%d", brand->damage / 10);
        if (brand->multiplier <= 3)
            text_out(p, " vs. creatures not resistant to %s, ", brand->name);
        else
            text_out(p, " vs. creatures susceptible to %s, ", brand->name);
    }

    /* Output damage for creatures effected by the slays */
    for (slay = slays; slay; slay = slay->next)
    {
        if (!slay->damage)
            text_out_c(p, COLOUR_L_RED, "0");
        else if (slay->damage % 10)
            text_out_c(p, COLOUR_L_GREEN, "%d.%d", slay->damage / 10, slay->damage % 10);
        else
            text_out_c(p, COLOUR_L_GREEN, "%d", slay->damage / 10);
        text_out(p, " vs. %s, ", slay->name);
    }

    if (has_brands_or_slays) text_out(p, "and ");

    /* Normal damage, not considering brands or slays */
    if (!normal_damage)
        text_out_c(p, COLOUR_L_RED, "0");
    else if (normal_damage % 10)
        text_out_c(p, COLOUR_L_GREEN, "%d.%d", normal_damage / 10, normal_damage % 10);
    else
        text_out_c(p, COLOUR_L_GREEN, "%d", normal_damage / 10);
    if (has_brands_or_slays) text_out(p, " vs. others");
    text_out(p, ".\n");

    free_brand(brands);
    free_slay(slays);
}


/*
 * Gets miscellaneous combat information about the given object.
 *
 * Fills in whether there is a special effect when thrown in `thrown effect`,
 * the `range` in ft (or zero if not ammo), whether the weapon has the
 * impact flag set, the percentage chance of breakage and whether it is
 * too heavy to be weilded effectively at the moment.
 */
static void obj_known_misc_combat(struct player *p, const struct object *obj,
    bool *thrown_effect, int *range, bool *impactful, int *break_chance, bool *too_heavy,
    bool *two_handed)
{
    bool weapon = (tval_is_melee_weapon(obj) || tval_is_mstaff(obj));
    bool ammo = (p->state.ammo_tval == obj->tval);
    bitflag f[OF_SIZE];
    bool aware = object_flavor_is_aware(p, obj);

    *thrown_effect = *impactful = *too_heavy = *two_handed = false;
    *range = *break_chance = 0;

    get_known_flags(obj, 0, f, aware);

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

    /* Note the impact flag */
    *impactful = of_has(f, OF_IMPACT);

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
        *too_heavy = state.heavy_wield;

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
    int range, break_chance;
    bool impactful, thrown_effect, too_heavy, two_handed;

    obj_known_misc_combat(p, obj, &thrown_effect, &range, &impactful, &break_chance,
        &too_heavy, &two_handed);

    /* Abort if we've nothing to say */
    if (!weapon && !ammo)
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
    if (too_heavy)
        text_out_c(p, COLOUR_L_RED, "You are too weak to use this weapon.\n");

    /* Weapon: blows/round */
    if (weapon)
        describe_blows(p, obj);

    /* Missile: shots/round and range */
    else
    {
        text_out_c(p, COLOUR_L_GREEN, "%d ", p->state.num_shots);
        text_out(p, "shot%s/round.\n", PLURAL(p->state.num_shots));
        text_out(p, "Hits targets up to ");
        text_out_c(p, COLOUR_L_GREEN, "%d", range * 10);
        text_out(p, " feet away.\n");
    }

    /* Describe damage */
    describe_damage(p, obj);

    /* Note the impact flag */
    if (impactful)
        text_out(p, "Sometimes creates earthquakes on impact.\n");

    /* Add breakage chance */
    if (ammo && !magic_ammo_p(obj) && !obj->artifact)
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

    /* Doesn't remotely resemble a digger */
    if ((slot < 0) || (obj->modifiers[OBJ_MOD_TUNNEL] <= 0)) return false;

    /* Player has no digging info */
    if (!object_this_mod_is_visible(obj, OBJ_MOD_TUNNEL))
        return false;

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
 * Fills in the radius of the light in `rad`, whether it uses fuel and
 * how many turns light it can refuel in similar items.
 *
 * Return false if the object is not known to be a light source (which
 * includes it not actually being a light source).
 */
static bool obj_known_light(struct player *p, const struct object *obj, int mode, int *rad,
    bool *uses_fuel, int *refuel_turns)
{
    bitflag flags[OF_SIZE];
    bool no_fuel;
    bool is_light = (tval_is_light(obj)? true: false);
    bool artifact = (obj->artifact? true: false);
    bool known = object_is_known(p, obj);

    get_known_flags(obj, mode, flags, object_flavor_is_aware(p, obj));

    if (!is_light && (obj->modifiers[OBJ_MOD_LIGHT] <= 0)) return false;

    /*
     * Prevent unidentified objects (especially artifact lights) from showing
     * bad radius and refueling info.
     */
    if (!known && !object_this_mod_is_visible(obj, OBJ_MOD_LIGHT))
        return false;

    /* Work out radius */
    *rad = obj->modifiers[OBJ_MOD_LIGHT];

    no_fuel = of_has(flags, OF_NO_FUEL);

    if (no_fuel && !artifact)
        *uses_fuel = false;
    else
        *uses_fuel = true;

    /* Only show refueling info for known objects */
    if (is_light && !no_fuel && of_has(obj->flags, OF_TAKES_FUEL) && known)
        *refuel_turns = z_info->fuel_lamp;
    else
        *refuel_turns = 0;

    return true;
}


/*
 * Describe things that look like lights.
 */
static bool describe_light(struct player *p, const struct object *obj, int mode)
{
    int rad = 0;
    bool uses_fuel = false;
    int refuel_turns = 0;
    bool terse = ((mode & OINFO_TERSE)? true: false);

    if (!obj_known_light(p, obj, mode, &rad, &uses_fuel, &refuel_turns))
        return false;

    text_out(p, "Radius ");
    text_out_c(p, COLOUR_L_GREEN, "%d", rad);
    text_out(p, " light.");

    if (tval_is_light(obj))
    {
        if (!uses_fuel)
            text_out(p, " No fuel required.");

        /* Lamps can refill other lamps */
        if (!terse)
        {
            if (refuel_turns)
                text_out(p, " Refills other lamps up to %d turns of fuel.", refuel_turns);
            else
                text_out(p, " Cannot be refueled.");
        }
    }

    text_out(p, "\n");

    return true;
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
    struct effect *eff = object_effect(obj);

    *effect = NULL;
    *min_recharge = 0;
    *max_recharge = 0;
    *failure_chance = 0;
    *aimed = false;

    /* Don't know much - be vague */
    if (eff && !object_effect_is_known(obj, object_flavor_is_aware(p, obj)))
    {
        *aimed = effect_aim(eff);

        return true;
    }

    /* No effect - no info */
    if (!eff) return false;

    *effect = eff;
    *aimed = effect_aim(eff);

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


static void print_effect(struct player *p, const char *d)
{
    char desc[MSG_LEN];
    char *t;
    bool colored = false;

    /* Print a colourised description */
    my_strcpy(desc, d, sizeof(desc));
    t = strtok(desc, "{}");
    while (t)
    {
        if (colored) text_out_c(p, COLOUR_L_GREEN, t);
        else text_out(p, t);
        colored = !colored;
        t = strtok(NULL, "{}");
    }
}


/*
 * Describe an object's effect, if any.
 */
static bool describe_effect(struct player *p, const struct object *obj, bool only_artifacts)
{
    char desc[MSG_LEN];
    struct effect *effect = NULL;
    bool aimed = false, has_desc = false;
    int min_time, max_time, failure_chance;
    struct actor actor_body;
    struct actor *data = &actor_body;

    data->idx = 0;
    data->player = p;
    data->mon = NULL;

    /* Sometimes only print artifact activation info */
    /* PWMAngband: also print ego activation info */
    if (only_artifacts && !(obj->artifact || obj->ego)) return false;

    if (!obj_known_effect(p, obj, &effect, &aimed, &min_time, &max_time, &failure_chance))
        return false;

    /* Effect not known, mouth platitudes */
    if (!effect && object_effect(obj))
    {
        if (aimed)
            text_out(p, "It can be aimed.\n");
        else if (tval_is_edible(obj))
            text_out(p, "It can be eaten.\n");
        else if (tval_is_potion(obj))
            text_out(p, "It can be drunk.\n");
        else if (tval_is_scroll(obj))
            text_out(p, "It can be read.\n");
        else
            text_out(p, "It can be activated.\n");

        return true;
    }

    /* Activations get a special message */
    if (obj->activation && obj->activation->desc)
    {
        text_out(p, "When activated, it ");
        print_effect(p, obj->activation->desc);
    }
    else
    {
        int random_choices = 0;

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

        if (aimed)
            text_out(p, "When aimed, it ");
        else if (tval_is_edible(obj))
            text_out(p, "When eaten, it ");
        else if (tval_is_potion(obj))
            text_out(p, "When quaffed, it ");
        else if (tval_is_scroll(obj))
            text_out(p, "When read, it ");
        else
            text_out(p, "When activated, it ");

        /* Print a colourised description */
        while (effect)
        {
            int roll = 0;
            random_value value;
            char dice_string[20];
            int boost, level;

            /* Skip blank descriptions */
            if (!effect_desc(effect))
            {
                effect = effect->next;
                continue;
            }

            /* Get the level */
            if (obj->artifact)
                level = get_artifact_level(obj);
            else
                level = obj->kind->level;

            /* Get the boost */
            boost = MAX(p->state.skills[SKILL_DEVICE] - level, 0);

            memset(&value, 0, sizeof(value));
            if (effect->dice != NULL)
                dice_roll(effect->dice, (void *)data, &value);

            /* Deal with special random effect */
            if (effect->index == EF_RANDOM) random_choices = roll + 1;

            /* Get the possible dice strings */
            if (value.dice && value.base)
            {
                strnfmt(dice_string, sizeof(dice_string), "{%d+%dd%d}", value.base, value.dice,
                    value.sides);
            }
            else if (value.dice)
                strnfmt(dice_string, sizeof(dice_string), "{%dd%d}", value.dice, value.sides);
            else
                strnfmt(dice_string, sizeof(dice_string), "{%d}", value.base);

            /* Check all the possible types of description format */
            switch (base_descs[effect->index].efinfo_flag)
            {
                /* Straight copy */
                case EFINFO_NONE:
                {
                    my_strcpy(desc, effect_desc(effect), sizeof(desc));
                    break;
                }

                /* Healing sometimes has a minimum percentage */
                case EFINFO_HEAL:
                {
                    char min_string[50];

                    if (value.m_bonus)
                    {
                        strnfmt(min_string, sizeof(min_string),
                            " (or {%d%%} of max HP, whichever is greater)", value.m_bonus);
                    }
                    else
                        strnfmt(min_string, sizeof(min_string), "");
                    strnfmt(desc, sizeof(desc), effect_desc(effect), dice_string, min_string);
                    break;
                }

                /* Use dice string */
                case EFINFO_CONST:
                /* PWMAngband: just use dice string since it's only used for objects */
                case EFINFO_TELE:
                /* PWMAngband: using dice string as radius because it's not always a constant */
                case EFINFO_QUAKE:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect), dice_string);
                    break;
                }

                /* Timed effect description */
                case EFINFO_CURE:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect),
                        timed_idx_to_desc(effect->params[0]));
                    break;
                }

                /* Timed effect description + duration */
                case EFINFO_TIMED:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect),
                        timed_idx_to_desc(effect->params[0]), dice_string);
                    break;
                }

                /* Stat name */
                case EFINFO_STAT:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect),
                        mod_flags[effect->params[0]].name);
                    break;
                }

                /* PWMAngband: restore original description (spell effect description + dice string) */
                case EFINFO_SEEN:
                {
                    char los_string[100];

                    strnfmt(los_string, sizeof(los_string), gf_desc(effect->params[0]), dice_string);
                    strnfmt(desc, sizeof(desc), effect_desc(effect), los_string);
                    break;
                }

                /* Summon effect description */
                case EFINFO_SUMM:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect), summon_desc(effect->params[0]));
                    break;
                }

                /* PWMAngband: restore original description (reverse radius and dice string) */
                case EFINFO_LIGHT:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect), effect->params[1], dice_string);
                    break;
                }

                /* Object generated balls are elemental */
                /* PWMAngband: restore original description (reverse radius and description) */
                case EFINFO_BALL:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect), effect->params[1],
                        gf_desc(effect->params[0]), dice_string);
                    if (boost)
                    {
                        my_strcat(desc,
                            format(", which your device skill increases by {%d} percent", boost),
                            sizeof(desc));
                    }
                    break;
                }

                /* Object generated breaths are elemental */
                /* PWMAngband: restore original description (effect + damage) */
                case EFINFO_BREATH:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect), gf_desc(effect->params[0]),
                        dice_string);
                    break;
                }

                /* Bolts that inflict status */
                case EFINFO_BOLT:
                case EFINFO_TOUCH:
                {
                    /* Hack -- some effects have a duration */
                    if (strstr(gf_desc(effect->params[0]), "%s"))
                    {
                        char tmp[100];

                        strnfmt(tmp, sizeof(tmp), gf_desc(effect->params[0]), dice_string);
                        strnfmt(desc, sizeof(desc), effect_desc(effect), tmp);
                    }
                    else
                        strnfmt(desc, sizeof(desc), effect_desc(effect), gf_desc(effect->params[0]));
                    break;
                }

                /* Bolts and beams that damage */
                case EFINFO_BOLTD:
                {
                    strnfmt(desc, sizeof(desc), effect_desc(effect), gf_desc(effect->params[0]),
                        dice_string);
                    if (boost)
                    {
                        my_strcat(desc,
                            format(", which your device skill increases by {%d} percent", boost),
                            sizeof(desc));
                    }
                    break;
                }

                /* PWMAngband: restore mana can restore a fixed amount of mana points, or all of them */
                case EFINFO_MANA:
                {
                    if (!value.base) my_strcpy(dice_string, "all your", sizeof(dice_string));
                    strnfmt(desc, sizeof(desc), effect_desc(effect), dice_string);
                    break;
                }

                /* PWMAngband: restore original description */
                case EFINFO_ENCHANT:
                {
                    const char *what;

                    switch (effect->params[1])
                    {
                        case 1: what = "a weapon's to-hit bonus"; break;
                        case 2: what = "a weapon's to-dam bonus"; break;
                        case 4: what = "a piece of armor"; break;
                        default: what = "something"; /* XXX */
                    }
                    strnfmt(desc, sizeof(desc), effect_desc(effect), what, dice_string);
                    break;
                }

                /* PWMAngband: use dice string and apply digestion rate */
                case EFINFO_FOOD:
                {
                    /* Basic digestion rate based on speed */
                    int rate = player_digest(p);

                    /* Adjust for player speed */
                    int multiplier = turn_energy(p->state.speed);

                    strnfmt(dice_string, sizeof(dice_string), "{%d}", value.base * multiplier / rate);
                    strnfmt(desc, sizeof(desc), effect_desc(effect), dice_string);
                    break;
                }

                default:
                {
                    msg(p, "Bad effect description passed to describe_effect(). Please report this bug.");
                    return false;
                }
            }
            print_effect(p, desc);

            /*
             * Random choices need special treatment - note that this code
             * assumes that RANDOM and the random choices will be the last
             * effect in the object/activation description
             */
            if (random_choices >= 1)
            {
                if (effect->index == EF_RANDOM) {}
                else if (random_choices > 2) text_out(p, ", ");
                else if (random_choices == 2) text_out(p, " or ");
                random_choices--;
            }
            else if (effect->next)
            {
                if (effect->next->next && (effect->next->index != EF_RANDOM)) text_out(p, ", ");
                else text_out(p, " and ");
            }
            effect = effect->next;
        }
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
    const char *droppee;
    const char *article;

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
        my_strcpy(loot_spot, "in town", sizeof(loot_spot));

    /* Name the monster of origin */
    if (r_info[obj->origin_xtra].ridx)
        droppee = r_info[obj->origin_xtra].name;
    else
        droppee = "monster lost to history";
    article = (is_a_vowel(droppee[0])? "an ": "a ");
    if (rf_has(r_info[obj->origin_xtra].flags, RF_UNIQUE))
        my_strcpy(name, droppee, sizeof(name));
    else
    {
        my_strcpy(name, article, sizeof(name));
        my_strcat(name, droppee, sizeof(name));
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
static void describe_flavor_text(struct player *p, const struct object *obj)
{
    /* Display the known artifact or object description */
    if (true_artifact_p(obj) && object_is_known(p, obj) && obj->artifact->text)
        text_out(p, "%s\n\n", obj->artifact->text);
    else if (object_flavor_is_aware(p, obj))
    {
        bool did_desc = false;

        if (obj->kind->text)
        {
            text_out(p, "%s", obj->kind->text);
            did_desc = true;
        }

        /* Display an additional ego-item description */
        if (object_ego_is_visible(obj) && obj->ego->text)
        {
            if (did_desc) text_out(p, " ");
            text_out(p, "%s", obj->ego->text);
            did_desc = true;
        }

        if (did_desc) text_out(p, "\n\n");
    }
}


/*
 * Output object information
 */
static void object_info_out(struct player *p, const struct object *obj, int mode)
{
    bitflag flags[OF_SIZE];
    struct element_info el_info[ELEM_MAX];
    bool something = false;
    bool known = object_is_known(p, obj);
    bool aware = object_flavor_is_aware(p, obj);
    bool terse = ((mode & OINFO_TERSE)? true: false);

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
    describe_origin(p, obj, terse);
    if (!terse) describe_flavor_text(p, obj);

    /* Unidentified item */
    if (!known)
    {
        text_out(p, "You do not know the full extent of this item's powers.\n");
        something = true;
    }

    /* Describe bits */
    if (describe_curses(p, obj, flags)) something = true;
    if (describe_stats(p, obj, aware)) something = true;
    if (describe_slays(p, obj, fulldesc)) something = true;
    if (describe_brands(p, obj, fulldesc)) something = true;
    if (describe_elements(p, el_info)) something = true;
    if (describe_protects(p, flags)) something = true;
    if (describe_ignores(p, el_info)) something = true;
    if (describe_hates(p, el_info)) something = true;
    if (describe_sustains(p, flags)) something = true;
    if (describe_misc_magic(p, flags)) something = true;
    if (describe_light(p, obj, mode)) something = true;
    if (describe_esp(p, flags)) something = true;
    if (something) text_out(p, "\n");

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
    if (!something && !terse)
        text_out(p, "\n\nThis item does not seem to possess any special abilities.");
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
