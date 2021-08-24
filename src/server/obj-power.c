/*
 * File: obj-power.c
 * Purpose: Calculation of object power
 *
 * Copyright (c) 2001 Chris Carr, Chris Robertson
 * Revised in 2009-11 by Chris Carr, Peter Denison
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


struct power_calc *calculations;


/*
 * Individual object power calculations
 */


struct power_calc_data
{
    struct object *obj;
    bool known;
    bool aware;
    int best_power;
    int num_brands;
    int num_slays;
    int num_kills;
    int iter;
};


static int object_power_calculation_TO_DAM(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s16b to_d = 0;

    if (power_obj->known->to_d) object_to_d(power_obj, &to_d);
    return to_d;
}


static int object_power_calculation_DICE(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s32b modifiers[OBJ_MOD_MAX];

    if (tval_is_melee_weapon(power_obj) || tval_is_mstaff(power_obj) || tval_is_ammo(power_obj))
        return power_obj->dd * (power_obj->ds + 1);

    if (tval_is_launcher(power_obj))
        return 0;

    object_modifiers(power_obj, modifiers);

    if (power_obj->brands || power_obj->slays || (modifiers[OBJ_MOD_BLOWS] > 0) ||
        (modifiers[OBJ_MOD_SHOTS] > 0) || (modifiers[OBJ_MOD_MIGHT] > 0))
    {
        return 48;
    }

    return 0;
}


static int object_power_calculation_DICE_DICE(void *data)
{
    int dice_pwr = (object_power_calculation_DICE(data) * 5) / 4;

    return dice_pwr * dice_pwr;
}


static int object_power_calculation_IS_EGO(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    return (power_obj->ego? 1: 0);
}


static int object_power_calculation_EXTRA_BLOWS(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s32b modifiers[OBJ_MOD_MAX];

    object_modifiers(power_obj, modifiers);

    if (calc->known || power_obj->known->modifiers[OBJ_MOD_BLOWS])
        return modifiers[OBJ_MOD_BLOWS];

    return 0;
}


static int object_power_calculation_EXTRA_SHOTS(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s32b modifiers[OBJ_MOD_MAX];

    object_modifiers(power_obj, modifiers);

    if (calc->known || power_obj->known->modifiers[OBJ_MOD_SHOTS])
        return modifiers[OBJ_MOD_SHOTS];

    return 0;
}


static int object_power_calculation_EXTRA_MIGHT(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s32b modifiers[OBJ_MOD_MAX];

    object_modifiers(power_obj, modifiers);

    if (calc->known || power_obj->known->modifiers[OBJ_MOD_MIGHT])
        return modifiers[OBJ_MOD_MIGHT];

    return 0;
}


static int object_power_calculation_BOW_MULTIPLIER(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    return (tval_is_launcher(power_obj)? power_obj->pval: 1);
}


static int object_power_calculation_BEST_SLAY(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;

    return calc->best_power;
}


static int object_power_calculation_SLAY_SLAY(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;

    if (calc->num_slays <= 1) return 0;
    return calc->num_slays * calc->num_slays;
}


static int object_power_calculation_BRAND_BRAND(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;

    if (calc->num_brands <= 1) return 0;
    return calc->num_brands * calc->num_brands;
}


static int object_power_calculation_SLAY_BRAND(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;

    return calc->num_slays * calc->num_brands;
}


static int object_power_calculation_KILL_KILL(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;

    if (calc->num_kills <= 1) return 0;
    return calc->num_kills * calc->num_kills;
}


static int object_power_calculation_ALL_SLAYS(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    int i, count = 0;

    for (i = 0; i < z_info->slay_max; i++)
    {
        struct slay *slay = &slays[i];

        if (slay->name && (slay->multiplier <= 3)) count++;
    }

    return ((calc->num_slays == count)? 1: 0);
}


static int object_power_calculation_ALL_BRANDS(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    int i, count = 0;

    for (i = 0; i < z_info->brand_max; i++)
    {
        struct brand *brand = &brands[i];

        if (brand->name && (brand->multiplier == 3)) count++;
    }

    return ((calc->num_brands == count)? 1: 0);
}


static int object_power_calculation_ALL_KILLS(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    int i, count = 0;

    for (i = 0; i < z_info->slay_max; i++)
    {
        struct slay *slay = &slays[i];

        if (slay->name && (slay->multiplier > 3)) count++;
    }

    return ((calc->num_kills == count)? 1: 0);
}


static int object_power_calculation_TO_HIT(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s16b to_h = 0;

    if (power_obj->known->to_h) object_to_h(power_obj, &to_h);
    return to_h;
}


static int object_power_calculation_BASE_AC(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    return ((power_obj->ac || tval_is_armor(power_obj))? 1: 0);
}


static int object_power_calculation_AC(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    return power_obj->ac;
}


static int object_power_calculation_TOTAL_ARMOR(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s16b to_a;

    object_to_a(power_obj, &to_a);
    return power_obj->ac + to_a;
}


static int object_power_calculation_WEIGHT(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    return MAX(power_obj->weight, 20);
}


static int object_power_calculation_TO_ARMOR(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    s16b to_a = 0;

    if (power_obj->known->to_a) object_to_a(power_obj, &to_a);
    return to_a;
}


static int object_power_calculation_MODIFIER(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    if (calc->known || power_obj->known->modifiers[calc->iter])
    {
        s32b modifiers[OBJ_MOD_MAX];

        object_modifiers(power_obj, modifiers);
        return modifiers[calc->iter];
    }

    return 0;
}


static int object_power_calculation_MOD_POWER(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct obj_property *prop;

    prop = lookup_obj_property(OBJ_PROPERTY_MOD, calc->iter);
    my_assert(prop);
    return prop->power;
}


static int object_power_calculation_MOD_TYPE_MULT(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;

    prop = lookup_obj_property(OBJ_PROPERTY_MOD, calc->iter);
    my_assert(prop);
    return prop->type_mult[power_obj->tval];
}


static int object_power_calculation_MOD_MULT(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct obj_property *prop;

    prop = lookup_obj_property(OBJ_PROPERTY_MOD, calc->iter);
    my_assert(prop);
    return prop->mult;
}


static int object_power_calculation_FLAG_POWER(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;
    bitflag flags[OF_SIZE];

    /* Extract the flags */
    if (calc->known)
        object_flags(power_obj, flags);
    else
        object_flags_known(power_obj, flags, calc->aware);

    prop = lookup_obj_property(OBJ_PROPERTY_FLAG, calc->iter);
    my_assert(prop);
    return (of_has(flags, calc->iter)? prop->power: 0);
}


static int object_power_calculation_FLAG_TYPE_MULT(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;

    prop = lookup_obj_property(OBJ_PROPERTY_FLAG, calc->iter);
    my_assert(prop);
    return prop->type_mult[power_obj->tval];
}


static int object_power_calculation_NUM_TYPE(void *data, int type)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    bitflag f[OF_SIZE];
    bitflag flags[OF_SIZE];

    /* Extract the flags */
    if (calc->known)
        object_flags(power_obj, flags);
    else
        object_flags_known(power_obj, flags, calc->aware);

    of_wipe(f);
    create_obj_flag_mask(f, 0, type, OFT_MAX);
    of_inter(f, flags);
    return ((of_count(f) > 1)? of_count(f): 0);
}


static int object_power_calculation_NUM_SUSTAINS(void *data)
{
    return object_power_calculation_NUM_TYPE(data, OFT_SUST);
}


static int object_power_calculation_ALL_TYPE(void *data, int type)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    bitflag f[OF_SIZE];
    bitflag flags[OF_SIZE];

    /* Extract the flags */
    if (calc->known)
        object_flags(power_obj, flags);
    else
        object_flags_known(power_obj, flags, calc->aware);

    of_wipe(f);
    create_obj_flag_mask(f, 0, type, OFT_MAX);
    return (of_is_subset(flags, f)? 1: 0);
}


static int object_power_calculation_ALL_SUSTAINS(void *data)
{
    return object_power_calculation_ALL_TYPE(data, OFT_SUST);
}


static int object_power_calculation_NUM_PROTECTS(void *data)
{
    return object_power_calculation_NUM_TYPE(data, OFT_PROT);
}


static int object_power_calculation_ALL_PROTECTS(void *data)
{
    return object_power_calculation_ALL_TYPE(data, OFT_PROT);
}


static int object_power_calculation_NUM_MISC(void *data)
{
    return object_power_calculation_NUM_TYPE(data, OFT_MISC);
}


static int object_power_calculation_ALL_MISC(void *data)
{
    return object_power_calculation_ALL_TYPE(data, OFT_MISC);
}


static int object_power_calculation_NUM_ESP(void *data)
{
    return object_power_calculation_NUM_TYPE(data, OFT_ESP);
}


static int object_power_calculation_ALL_ESP(void *data)
{
    return object_power_calculation_ALL_TYPE(data, OFT_ESP);
}


static int object_power_calculation_IGNORE(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    prop = lookup_obj_property(OBJ_PROPERTY_IGNORE, calc->iter);
    my_assert(prop);
    if ((calc->known || object_element_is_known(power_obj, calc->iter, calc->aware)) &&
        (el_info[calc->iter].flags & EL_INFO_IGNORE))
    {
        return prop->power;
    }

    return 0;
}


static int object_power_calculation_VULN(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    prop = lookup_obj_property(OBJ_PROPERTY_VULN, calc->iter);
    my_assert(prop);
    if ((calc->known || object_element_is_known(power_obj, calc->iter, calc->aware)) &&
        (el_info[calc->iter].res_level == -1))
    {
        return prop->power;
    }

    return 0;
}


static int object_power_calculation_RESIST(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    prop = lookup_obj_property(OBJ_PROPERTY_RESIST, calc->iter);
    my_assert(prop);
    if ((calc->known || object_element_is_known(power_obj, calc->iter, calc->aware)) &&
        (el_info[calc->iter].res_level == 1))
    {
        return prop->power;
    }

    return 0;
}


static int object_power_calculation_IMM(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct obj_property *prop;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    prop = lookup_obj_property(OBJ_PROPERTY_IMM, calc->iter);
    my_assert(prop);
    if ((calc->known || object_element_is_known(power_obj, calc->iter, calc->aware)) &&
        (el_info[calc->iter].res_level == 3))
    {
        return prop->power;
    }

    return 0;
}


static int object_power_calculation_NUM_BASE_RES(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i, count = 0;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_BASE_MIN; i <= ELEM_BASE_MAX; i++)
    {
        if (el_info[i].res_level >= 1) count++;
    }

    return ((count > 1)? count: 0);
}


static int object_power_calculation_ALL_BASE_RES(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_BASE_MIN; i <= ELEM_BASE_MAX; i++)
    {
        if (el_info[i].res_level < 1) return 0;
    }

    return 1;
}


static int object_power_calculation_NUM_HIGH_RES(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i, count = 0;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_HIGH_MIN; i <= ELEM_HIGH_MAX; i++)
    {
        if (el_info[i].res_level == 1) count++;
    }

    return ((count > 1)? count: 0);
}


static int object_power_calculation_ALL_HIGH_RES(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_HIGH_MIN; i <= ELEM_HIGH_MAX; i++)
    {
        if (el_info[i].res_level != 1) return 0;
    }

    return 1;
}


static int object_power_calculation_NUM_XHIGH_RES(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i, count = 0;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_HIGH_MAX + 1; i <= ELEM_XHIGH_MAX; i++)
    {
        if (el_info[i].res_level == 1) count++;
    }

    return ((count > 1)? count: 0);
}


static int object_power_calculation_ALL_XHIGH_RES(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_HIGH_MAX + 1; i <= ELEM_XHIGH_MAX; i++)
    {
        if (el_info[i].res_level != 1) return 0;
    }

    return 1;
}


static int object_power_calculation_NUM_IMM(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i, count = 0;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_BASE_MIN; i <= ELEM_BASE_MAX; i++)
    {
        if (el_info[i].res_level == 3) count++;
    }

    return ((count > 1)? count: 0);
}


static int object_power_calculation_ALL_IMM(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    int i;
    struct element_info el_info[ELEM_MAX];

    object_elements(power_obj, el_info);

    for (i = ELEM_BASE_MIN; i <= ELEM_BASE_MAX; i++)
    {
        if (el_info[i].res_level != 3) return 0;
    }

    return 1;
}


static int object_power_calculation_EFFECT_POWER(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;

    /* Get activation */
    if (calc->known || object_effect_is_known(power_obj, calc->aware))
    {
        /* Add power for activation */
        if (power_obj->activation && randcalc(power_obj->time, 0, MAXIMISE))
            return power_obj->activation->power;
    }

    return 0;
}


static int object_power_calculation_CURSE_POWER(void *data)
{
    struct power_calc_data *calc = (struct power_calc_data *)data;
    struct object *power_obj = calc->obj;
    struct curse_data *known_curses = (calc->known? power_obj->curses: power_obj->known->curses);
    int i, curse_power = 0;

    /* Get the curse object power */
    for (i = 0; known_curses && (i < z_info->curse_max); i++)
    {
        if (known_curses[i].power)
            curse_power -= known_curses[i].power / 10;
    }

    return curse_power;
}


expression_base_value_f power_calculation_by_name(const char *name)
{
    static const struct power_calc_s
    {
        const char *name;
        expression_base_value_f function;
    } power_calcs[] =
    {
        {"OBJ_POWER_TO_DAM", object_power_calculation_TO_DAM},
        {"OBJ_POWER_DICE", object_power_calculation_DICE},
        {"OBJ_POWER_DICE_DICE", object_power_calculation_DICE_DICE},
        {"OBJ_POWER_IS_EGO", object_power_calculation_IS_EGO},
        {"OBJ_POWER_EXTRA_BLOWS", object_power_calculation_EXTRA_BLOWS},
        {"OBJ_POWER_EXTRA_SHOTS", object_power_calculation_EXTRA_SHOTS},
        {"OBJ_POWER_EXTRA_MIGHT", object_power_calculation_EXTRA_MIGHT},
        {"OBJ_POWER_BOW_MULTIPLIER", object_power_calculation_BOW_MULTIPLIER},
        {"OBJ_POWER_BEST_SLAY", object_power_calculation_BEST_SLAY},
        {"OBJ_POWER_SLAY_SLAY", object_power_calculation_SLAY_SLAY},
        {"OBJ_POWER_BRAND_BRAND", object_power_calculation_BRAND_BRAND},
        {"OBJ_POWER_SLAY_BRAND", object_power_calculation_SLAY_BRAND},
        {"OBJ_POWER_KILL_KILL", object_power_calculation_KILL_KILL},
        {"OBJ_POWER_ALL_SLAYS", object_power_calculation_ALL_SLAYS},
        {"OBJ_POWER_ALL_BRANDS", object_power_calculation_ALL_BRANDS},
        {"OBJ_POWER_ALL_KILLS", object_power_calculation_ALL_KILLS},
        {"OBJ_POWER_TO_HIT", object_power_calculation_TO_HIT},
        {"OBJ_POWER_BASE_AC", object_power_calculation_BASE_AC},
        {"OBJ_POWER_AC", object_power_calculation_AC},
        {"OBJ_POWER_TOTAL_ARMOR", object_power_calculation_TOTAL_ARMOR},
        {"OBJ_POWER_WEIGHT", object_power_calculation_WEIGHT},
        {"OBJ_POWER_TO_ARMOR", object_power_calculation_TO_ARMOR},
        {"OBJ_POWER_MODIFIER", object_power_calculation_MODIFIER},
        {"OBJ_POWER_MOD_POWER", object_power_calculation_MOD_POWER},
        {"OBJ_POWER_MOD_TYPE_MULT", object_power_calculation_MOD_TYPE_MULT},
        {"OBJ_POWER_MOD_MULT", object_power_calculation_MOD_MULT},
        {"OBJ_POWER_FLAG_POWER", object_power_calculation_FLAG_POWER},
        {"OBJ_POWER_FLAG_TYPE_MULT", object_power_calculation_FLAG_TYPE_MULT},
        {"OBJ_POWER_NUM_SUSTAINS", object_power_calculation_NUM_SUSTAINS},
        {"OBJ_POWER_ALL_SUSTAINS", object_power_calculation_ALL_SUSTAINS},
        {"OBJ_POWER_NUM_PROTECTS", object_power_calculation_NUM_PROTECTS},
        {"OBJ_POWER_ALL_PROTECTS", object_power_calculation_ALL_PROTECTS},
        {"OBJ_POWER_NUM_MISC", object_power_calculation_NUM_MISC},
        {"OBJ_POWER_ALL_MISC", object_power_calculation_ALL_MISC},
        {"OBJ_POWER_NUM_ESP", object_power_calculation_NUM_ESP},
        {"OBJ_POWER_ALL_ESP", object_power_calculation_ALL_ESP},
        {"OBJ_POWER_IGNORE", object_power_calculation_IGNORE},
        {"OBJ_POWER_VULN", object_power_calculation_VULN},
        {"OBJ_POWER_RESIST", object_power_calculation_RESIST},
        {"OBJ_POWER_IMM", object_power_calculation_IMM},
        {"OBJ_POWER_NUM_BASE_RES", object_power_calculation_NUM_BASE_RES},
        {"OBJ_POWER_ALL_BASE_RES", object_power_calculation_ALL_BASE_RES},
        {"OBJ_POWER_NUM_HIGH_RES", object_power_calculation_NUM_HIGH_RES},
        {"OBJ_POWER_ALL_HIGH_RES", object_power_calculation_ALL_HIGH_RES},
        {"OBJ_POWER_NUM_XHIGH_RES", object_power_calculation_NUM_XHIGH_RES},
        {"OBJ_POWER_ALL_XHIGH_RES", object_power_calculation_ALL_XHIGH_RES},
        {"OBJ_POWER_NUM_IMM", object_power_calculation_NUM_IMM},
        {"OBJ_POWER_ALL_IMM", object_power_calculation_ALL_IMM},
        {"OBJ_POWER_EFFECT_POWER", object_power_calculation_EFFECT_POWER},
        {"OBJ_POWER_CURSE_POWER", object_power_calculation_CURSE_POWER},
        {NULL, NULL}
    };
    const struct power_calc_s *current = power_calcs;

    while (current->name != NULL && current->function != NULL)
    {
        if (my_stricmp(name, current->name) == 0)
            return current->function;

        current++;
    }

    return NULL;
}


/*
 * Overall object power calculations
 */


/*
 * Run an individual power calculation
 *
 * Dice are used in power calculations sometimes as an easy way of encoding
 * multiplication, so the MAXIMISE aspect is always used in their evaluation.
 */
static int run_power_calculation(struct power_calc *calc, void *data)
{
    struct object *power_obj = ((struct power_calc_data *)data)->obj;
    random_value rv = {0, 0, 0, 0};
    struct poss_item *poss = calc->poss_items;

    /* Ignore null calculations */
    if (!calc->dice) return 0;

    /* Check whether this calculation applies to this item */
    if (poss)
    {
        while (poss)
        {
            if (power_obj->kind->kidx == poss->kidx) break;
            poss = poss->next;
        }
        if (!poss) return 0;
    }

    return dice_evaluate(calc->dice, 1, MAXIMISE, data, &rv);
}


static void apply_op(int operation, int *current, int newval)
{
    switch (operation)
    {
        case POWER_CALC_NONE: break;
        case POWER_CALC_ADD:
        {
            *current += newval;
            break;
        }
        case POWER_CALC_ADD_IF_POSITIVE:
        {
            if (newval > 0) *current += newval;
            break;
        }
        case POWER_CALC_SQUARE_ADD_IF_POSITIVE:
        {
            if (newval > 0) *current += newval * newval;
            break;
        }
        case POWER_CALC_MULTIPLY:
        {
            *current *= newval;
            break;
        }
        case POWER_CALC_DIVIDE:
        {
            *current /= newval;
            break;
        }
        default: break;
    }
}


/*
 * Calculate stats on slays and brands up front
 */
static void collect_slay_brand_stats(struct power_calc_data *data)
{
    int i;
    bool *known_brands = (data->known? data->obj->brands: data->obj->known->brands);
    bool *known_slays = (data->known? data->obj->slays: data->obj->known->slays);

    data->num_brands = 0;
    data->num_slays = 0;
    data->num_kills = 0;
    data->best_power = 100;

    /* If there are no slays or brands return */
    if ((brand_count(data->obj->brands) + slay_count(data->obj->slays)) == 0) return;

    /* Count the known brands and slays */
    for (i = 0; known_brands && (i < z_info->brand_max); i++)
    {
        if (!known_brands[i]) continue;
        data->num_brands++;
        if (brands[i].power > data->best_power) data->best_power = brands[i].power;
    }
    for (i = 0; known_slays && (i < z_info->slay_max); i++)
    {
        if (!known_slays[i]) continue;
        if (slays[i].multiplier <= 3) data->num_slays++;
        else data->num_kills++;
        if (slays[i].power > data->best_power) data->best_power = slays[i].power;
    }
}


static int object_power_aux(struct power_calc_data *data, int **current_value)
{
    int i;
    int power = 0;

    /* Preprocess the power calculations for intermediate results */
    for (i = 0; i < z_info->calculation_max; i++)
    {
        struct power_calc *calc = &calculations[i];
        int j;

        /* Discard OF_NONE */
        int iter_min = ((calc->iterate.property_type == OBJ_PROPERTY_FLAG)? 1: 0);

        /* Run the calculation... */
        for (data->iter = iter_min; data->iter < calc->iterate.max; data->iter++)
            current_value[i][data->iter] = run_power_calculation(calc, (void *)data);

        /* ...and apply to an earlier one if needed */
        if (calc->apply_to)
        {
            for (j = 0; j < i; j++)
            {
                if (!calculations[j].name) continue;
                if (streq(calculations[j].name, calc->apply_to)) break;
            }

            /* Ignore this calculation if no name found, otherwise apply it */
            if (i == j)
                plog_fmt("No target %s for %s to apply to", calc->apply_to, calc->name);

            /* Both the same size */
            else if (calculations[j].iterate.max == calc->iterate.max)
            {
                for (data->iter = iter_min; data->iter < calc->iterate.max; data->iter++)
                {
                    apply_op(calc->operation, &current_value[j][data->iter],
                        current_value[i][data->iter]);
                }
            }

            /* Many values applying to one */
            else if (calculations[j].iterate.max == 1)
            {
                for (data->iter = iter_min; data->iter < calc->iterate.max; data->iter++)
                    apply_op(calc->operation, &current_value[j][0], current_value[i][data->iter]);
            }

            else
                plog_fmt("Size mismatch applying %s to %s", calc->name, calculations[j].name);
        }
    }

    /* Put all the power calculations together */
    for (i = 0; i < z_info->calculation_max; i++)
    {
        struct power_calc *calc = &calculations[i];
        struct poss_item *poss = calc->poss_items;

        /* Discard OF_NONE */
        int iter_min = ((calc->iterate.property_type == OBJ_PROPERTY_FLAG)? 1: 0);

        /* Check whether this calculation applies to this item */
        if (poss)
        {
            while (poss)
            {
                if (data->obj->kind->kidx == poss->kidx) break;
                poss = poss->next;
            }
            if (!poss) continue;
        }

        if (calc->apply_to == NULL)
        {
            for (data->iter = iter_min; data->iter < calc->iterate.max; data->iter++)
            {
                apply_op(calc->operation, &power, current_value[i][data->iter]);
                if (power >= INHIBIT_POWER) return INHIBIT_POWER;
            }
        }
    }

    return power;
}


/*
 * Run all the power calculations on an object to find its power
 */
int object_power(struct player *p, const struct object* obj)
{
    int i;
    int **current_value;
    int power;
    struct power_calc_data data;

    data.known = object_is_known(p, obj);
    data.aware = object_flavor_is_aware(p, obj);

    /* Set the power evaluation object and collect slay and brand stats */
    data.obj = (struct object *)obj;
    collect_slay_brand_stats(&data);

    /* Set up arrays for each power calculation (most of them length 1) */
    current_value = mem_zalloc(z_info->calculation_max * sizeof(int*));
    for (i = 0; i < z_info->calculation_max; i++)
    {
        struct power_calc *calc = &calculations[i];

        if (calc->iterate.max) current_value[i] = mem_zalloc(calc->iterate.max * sizeof(int));
    }

    power = object_power_aux(&data, current_value);

    /* Free the current value arrays */
    for (i = 0; i < z_info->calculation_max; i++)
        mem_free(current_value[i]);
    mem_free(current_value);

    return power;
}


/*
 * Object pricing
 */


/*
 * Return the "value" of an "unknown" non-wearable item
 * Make a guess at the value of non-aware items
 */
static int object_value_base(struct player *p, const struct object *obj)
{
    /* Use template cost for aware objects */
    if (object_flavor_is_aware(p, obj)) return obj->kind->cost;

    /* Analyze the type */
    switch (obj->tval)
    {
        case TV_MUSHROOM: return 5;
        case TV_POTION:
        case TV_SCROLL: return 20;
        case TV_WAND: return 50;
        case TV_STAFF: return 70;
        case TV_ROD: return 90;
    }

    /* Paranoia (should never come here) */
    return 0;
}


/*
 * Return the real price of a known (or partly known) item.
 *
 * Wands and staves get cost for each charge.
 *
 * Wearable items (weapons, launchers, jewelry, lights, armour, tools, ammo)
 * are priced according to their power rating. All ammo, and normal (non-ego)
 * torches are scaled down by AMMO_RESCALER to reflect their impermanence.
 *
 * PWMAngband: artifacts are always sellable
 */
int object_value_real(struct player *p, const struct object *obj, int qty)
{
    int value = 0, total_value;
    int power;
    int a = 1;
    int b = 5;
    int min_value = (obj->artifact? 1: 0);

    /* Hack -- worthless objects */
    if (obj->origin == ORIGIN_WORTHLESS) return min_value;

    /* Wearables and ammo have prices that vary by individual item properties */
    if (tval_has_variable_power(obj))
    {
        bool normal_ammo = (tval_is_ammo(obj) && !obj->artifact);
        bool normal_torch = (tval_is_light(obj) && of_has(obj->flags, OF_BURNS_OUT) && !obj->ego);

        /* Calculate power and value */
        power = object_power(p, obj);
        if (power > 0) value = a * power * power + b * power;

        /* Rescale for expendables */
        if ((normal_ammo || normal_torch) && (value > 0))
            value = MAX(value / AMMO_RESCALER, 1);

        /* PWMAngband -- boost artifact missiles */
        if (tval_is_ammo(obj) && obj->artifact) value += 10000L;

        /* PWMAngband -- boost rings of polymorphing */
        if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
        {
            struct monster_race *race = &r_info[obj->modifiers[OBJ_MOD_POLY_RACE]];

            value += MAX(race->level, 1) * MAX(race->mexp, 100);
        }

        /* Get the total value */
        total_value = value * qty;
    }
    else
    {
        /* Worthless items */
        if (!obj->kind->cost) return min_value;

        /* Base cost */
        value = obj->kind->cost;

        /* Calculate total value */
        total_value = value * qty;

        /* Wands/Staffs */
        if (tval_can_have_charges(obj))
        {
            int charges = obj->pval * qty;

            /* Calculate number of charges, rounded up */
            if (obj->number)
            {
                charges = obj->pval * qty / obj->number;
                if ((obj->pval * qty) % obj->number != 0) charges++;
            }

            /* Pay extra for charges, depending on standard number of charges */
            total_value += value * charges / 20;
        }
    }

    /* No negative value */
    if (total_value < min_value) total_value = min_value;

    /* Return the value */
    return total_value;
}


/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item
 *
 * Never notice unknown bonuses or properties, including curses,
 * since that would give players information they did not have.
 */
int object_value(struct player *p, const struct object *obj, int qty)
{
    int value;

    /* Gold */
    if (tval_is_money(obj)) return obj->pval;

    /* Variable power items are assessed by what is known about them */
    /* Known items use the actual value */
    if (tval_has_variable_power(obj) || object_is_known(p, obj))
        value = object_value_real(p, obj, qty);

    /* Unknown constant-price items just get a base value */
    else
        value = object_value_base(p, obj) * qty;

    /* Return the final value */
    return value;
}


