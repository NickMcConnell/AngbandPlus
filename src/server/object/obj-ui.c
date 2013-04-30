/*
 * File: obj-ui.c
 * Purpose: Mainly object descriptions and generic UI functions
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


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "../effects.h"
#include "../netserver.h"


/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(struct player *p)
{
    register int i, z = 0;
    object_type *o_ptr;
    byte attr, act, fuel, fail;
    char o_name[NORMAL_WID];
    int wgt, slot;
    s32b price;

    /* Have the final slot be the FINAL slot */
    z = INVEN_WIELD;

    /* Display the pack */
    for (i = 0; i < z; i++)
    {
        /* Examine the item */
        o_ptr = &p->inventory[i];

        /* Paranoia */
        if (!o_ptr->kind)
        {
            object_type dummy;

            o_ptr = &dummy;
            object_prep(o_ptr, &k_info[0], 0, MINIMISE);
        }

        /* Obtain an item description */
        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Display the weight if needed */
        wgt = o_ptr->weight * o_ptr->number;

        /* Display the price if needed */
        price = (in_store(p) && (p->store_num <= STORE_XBM))?
            price_item(p, o_ptr, TRUE, o_ptr->number): 0;

        /* Get the info */
        get_object_info(p, o_ptr, &attr, &act, &fuel, &fail, &slot);

        /* Send the info to the client */
        Send_inven(p, index_to_label(i), attr, wgt, price, o_ptr->number,
            o_ptr->tval, o_ptr->sval, act, fuel, fail, slot, o_name);
    }
}


/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(struct player *p)
{
    register int i;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    byte attr, act, fuel, fail;
    int wgt;
    s32b price;
    int effect;

    /* Display the equipment */
    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        /* Examine the item */
        o_ptr = &p->inventory[i];

        /* Paranoia */
        if (!o_ptr->kind)
        {
            object_type dummy;

            o_ptr = &dummy;
            object_prep(o_ptr, &k_info[0], 0, MINIMISE);
        }

        /* Obtain an item description */
        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Get the color */
        attr = p->tval_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

        /* Display the weight (if needed) */
        wgt = o_ptr->tval? o_ptr->weight * o_ptr->number: 0;

        /* Display the price if needed */
        price = ((in_store(p) && (p->store_num <= STORE_XBM))?
            price_item(p, o_ptr, TRUE, o_ptr->number): 0);

        /* Get the "activation" flag */
        act = 0;

        /* Get the effect */
        effect = o_ptr->effect;

        /* Activatable effects */
        if (effect)
        {
            /* Get direction choice */
            if (effect_aim(effect)) act = 2;
            else act = 1;
        }

        /* Get the "fuelable" flag */
        fuel = 0;
        if ((o_ptr->tval == TV_LIGHT) && is_lamp(o_ptr))
        {
            bitflag f[OF_SIZE];

            /* Get flags */
            object_flags(o_ptr, f);

            /* Non-everburning lamps are okay */
            if (!of_has(f, OF_NO_FUEL)) fuel = 1;
        }

        /* Get the "fail" flag */
        fail = 255;
        if (object_effect_is_known(p, o_ptr))
            fail = (9 + get_use_device_chance(p, o_ptr)) / 10;

        /* Send the info off */
        Send_equip(p, index_to_label(i), attr, wgt, price, o_ptr->number, o_ptr->tval, o_ptr->sval,
            act, fuel, fail, -1, o_name);
    }
}


/*
 * Choice window "shadow" of the "show_floor()" function
 */
void display_floor(struct player *p, const int *floor_list, int floor_num)
{
    int i;
    object_type *o_ptr;
    byte attr, act, fuel, fail;
    int slot;
    char o_name[NORMAL_WID];

    /* Limit displayed floor items to MAX_FLOOR_STACK */
    if (floor_num > MAX_FLOOR_STACK) floor_num = MAX_FLOOR_STACK;

    /* Effectiveness */
    if (!floor_num && !p->delta_floor_item) return;
    p->delta_floor_item = floor_num;

    /* Display the floor */
    for (i = 0; i < floor_num; i++)
    {
        /* Examine the item */
        o_ptr = object_byid(floor_list[i]);

        /* Obtain an item description */
        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Get the info */
        get_object_info(p, o_ptr, &attr, &act, &fuel, &fail, &slot);

        /* Send the info to the client */
        Send_floor(p, i, floor_list[i], attr, o_ptr->number, o_ptr->tval,
            o_ptr->sval, act, fuel, fail, slot, o_name);
    }

    /* Clear */
    for (i = floor_num; i < MAX_FLOOR_STACK; i++)
        Send_floor(p, i, -1, 0, 0, 0, 0, 0, 0, 0, -1, "");
}


/*
 * Display the floor.
 */
void show_floor(int Ind, olist_detail_t mode)
{
    Send_show_floor(Ind, (byte)mode);
}


bool get_item(struct player *p, byte tester_tval, byte tester_hook)
{
    /* Pending */
    p->current_value = ITEM_PENDING;

    Send_item_request(p, tester_tval, tester_hook);

    return (TRUE);
}
