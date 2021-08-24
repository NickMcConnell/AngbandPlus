/*
 * File: obj-properties.c
 * Purpose: Functions to deal with object flags and modifiers
 *
 * Copyright (c) 2014 Chris Carr, Nick McConnell
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
 * Details of the different object flags in the game.
 * See obj-properties.h for structure
 *
 * Note that sustain stat flags are included first, so that the index into
 * the flag table for a sustain is the stat index + 1
 *
 * Note that any strings in the last position must have exactly one %s
 */
static const struct object_flag object_flag_table[] =
{
    {OF_NONE, 0, OFT_NONE, 0, ""},
    #define STAT(a, b, c, d, e, f, g, h) {OF_##c, OFID_NORMAL, OFT_SUST, d, "Your %s glows."},
    #include "../common/list-stats.h"
    #undef STAT
    #define OF(a, b, c, d, e) {OF_##a, b, c, d, e},
    #include "../common/list-object-flags.h"
    #undef OF
    {OF_MAX, 0, OFT_NONE, 0, ""}
};


/*
 * Return the sustain flag of a given stat.
 */
int sustain_flag(int stat)
{
    if ((stat < 0) || (stat >= STAT_MAX)) return -1;

    return object_flag_table[stat + 1].index;
}


/*
 * Details of the different object modifiers in the game.
 * See obj-properties.h for structure
 */
static const struct object_mod object_mod_table[] =
{
    #define STAT(a, b, c, d, e, f, g, h) {OBJ_MOD_##a, b, e, h},
    #include "../common/list-stats.h"
    #undef STAT
    #define OBJ_MOD(a, b, c, d) {OBJ_MOD_##a, b, c, d},
    #include "../common/list-object-modifiers.h"
    #undef OBJ_MOD
    {OBJ_MOD_MAX, 0, 0, ""}
};


/*
 * Create a "mask" of flags of a specific type or ID threshold.
 *
 * f is the flag array we're filling
 * id is whether we're masking by ID level
 * ... is the list of flags or ID types we're looking for
 *
 * N.B. OFT_MAX must be the last item in the ... list
 */
void create_mask(bitflag *f, bool id, ...)
{
    const struct object_flag *of;
    int i;
    va_list args;

    of_wipe(f);

    va_start(args, id);

    /* Process each type in the va_args */
    for (i = va_arg(args, int); i != OFT_MAX; i = va_arg(args, int))
    {
        for (of = object_flag_table; of->index < OF_MAX; of++)
        {
            if ((id && of->id == i) || (!id && of->type == i))
                of_on(f, of->index);
        }
    }

    va_end(args);
}


/*
 * Determine whether a flagset includes any curse flags.
 */
bool cursed_p(const bitflag *f)
{
    bitflag f2[OF_SIZE];

    of_wipe(f2);
    create_mask(f2, false, OFT_CURSE, OFT_MAX);

    return of_is_inter(f, f2);
}


/*
 * Get the slot multiplier for a flag's power rating
 *
 * flag is the flag in question.
 * slot is the wield_slot it's in.
 */
s16b flag_slot_mult(struct player *p, int flag, int slot)
{
    const struct object_flag *f = &object_flag_table[flag];

    switch (f->type)
    {
        /* Many flags are equally good (or bad) in any slot */
        case OFT_SUST:
        case OFT_PROT:
        case OFT_BAD:
        case OFT_CURSE: return 1;

        /* Some flags have no effect */
        case OFT_NONE: return 0;

        /* Light-specific */
        case OFT_LIGHT: return (slot_type_is(p, slot, EQUIP_LIGHT)? 1: 0);

        /* Melee weapon specific */
        case OFT_MELEE: return (slot_type_is(p, slot, EQUIP_WEAPON)? 1: 0);

        /* Miscellaneous flags are a mixed bag */
        case OFT_MISC:
        case OFT_KNOW:
        case OFT_ESP:
        {
            /* Hack -- ESP_POWER */
            if (flag == OF_ESP_POWER) return 0;

            /* Weapon and bow slot are more useful for other purposes */
            if (slot_type_is(p, slot, EQUIP_WEAPON) || slot_type_is(p, slot, EQUIP_BOW))
                return 1;

            /* SD and FF are a bit lame */
            if ((flag == OF_FEATHER) || (flag == OF_SLOW_DIGEST)) return 1;

            /* FA on gloves is really nice */
            if ((flag == OF_FREE_ACT) && slot_type_is(p, slot, EQUIP_GLOVES)) return 5;

            /* All the major powers are good */
            return 2;
        }

        default: return 1;
    }
}


/*
 * Return the base power rating for a flag.
 */
s32b flag_power(int flag)
{
    const struct object_flag *of = &object_flag_table[flag];

    return of->power;
}


/*
 * Return the OFT_ type of a flag.
 */
int obj_flag_type(int flag)
{
    const struct object_flag *of = &object_flag_table[flag];

    return of->type;
}


/*
 * Print a message when an object flag is identified by use.
 *
 * flag is the flag being noticed
 * name is the object name
 */
void flag_message(struct player *p, int flag, char *name)
{
    const struct object_flag *of = &object_flag_table[flag];

    if (!streq(of->message, ""))
        msg(p, of->message, name);
}


/*
 * Return the base power rating for a mod.
 */
s32b mod_power(int mod)
{
    const struct object_mod *om = &object_mod_table[mod];

    return om->power;
}


/*
 * Return the mod weighting of a mod.
 */
int mod_mult(int mod)
{
    const struct object_mod *om = &object_mod_table[mod];

    return om->mod_mult;
}


/*
 * Get the slot multiplier for a mod's power rating
 *
 * mod is the mod in question.
 * slot is the wield_slot it's in.
 */
s16b mod_slot_mult(struct player *p, int mod, int slot)
{
    /* Ammo gets -1 as a slot, and always has multiplier 1 */
    if (slot == -1) return 1;

    /* Gloves with DEX are good */
    if ((mod == OBJ_MOD_DEX) && slot_type_is(p, slot, EQUIP_GLOVES)) return 2;

    /* Extra blows are silly on a bow, powerful off-weapon */
    if (mod == OBJ_MOD_BLOWS)
    {
        if (slot_type_is(p, slot, EQUIP_BOW)) return 0;
        if (slot_type_is(p, slot, EQUIP_WEAPON)) return 1;
        return 3;
    }

    /* Extra shots are silly on a melee weapon, powerful off-weapon */
    if (mod == OBJ_MOD_SHOTS)
    {
        if (slot_type_is(p, slot, EQUIP_WEAPON)) return 0;
        if (slot_type_is(p, slot, EQUIP_BOW)) return 1;
        return 4;
    }

    /* Extra might only works on bows */
    if (mod == OBJ_MOD_MIGHT)
    {
        if (slot_type_is(p, slot, EQUIP_BOW)) return 1;
        return 0;
    }

    /* Light is best on, well, lights */
    if (mod == OBJ_MOD_LIGHT)
    {
        if (slot_type_is(p, slot, EQUIP_LIGHT)) return 3;
        return 1;
    }

    /* Hack -- POLY_RACE */
    if (mod == OBJ_MOD_POLY_RACE)
    {
        if (slot_type_is(p, slot, EQUIP_RING)) return 1;
        return 0;
    }

    /* Others are all easy */
    return 1;
}
