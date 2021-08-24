/*
 * File: cmd-obj.c
 * Purpose: Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007-9 Andrew Sidwell, Chris Carr, Ed Graham, Erik Osheim
 *                       Jeff Greene, Diego Gonzalez
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include <src/npp.h>
#include "src/player_command.h"
#include "src/object_settings.h"
#include "src/cmds.h"
#include <QPushButton>



// Miscellaneous hooks to support the universal object handler below
static bool obj_has_inscrip(object_type *o_ptr) {return (o_ptr->has_inscription());}
static bool obj_can_takeoff(object_type *o_ptr) {return (o_ptr->can_takeoff());}
static bool obj_is_scroll(object_type *o_ptr)   {return (o_ptr->is_scroll());}
static bool obj_is_wand(object_type *o_ptr)     {return (o_ptr->is_wand());}
static bool obj_is_rod(object_type *o_ptr)      {return (o_ptr->is_rod());}
static bool obj_is_food(object_type *o_ptr)     {return (o_ptr->is_food());}
static bool obj_is_potion(object_type *o_ptr)   {return (o_ptr->is_potion());}
static bool obj_is_staff(object_type *o_ptr)    {return (o_ptr->is_staff());}
static bool obj_is_ring(object_type *o_ptr)     {return (o_ptr->is_ring());}
static bool obj_is_ammo(object_type *o_ptr)     {return (o_ptr->is_ammo());}
static bool obj_is_usable(object_type *o_ptr)   {return (o_ptr->is_usable_item());}

/*** Examination ***/
cmd_arg obj_examine(object_type *o_ptr, cmd_arg args)
{
    (void)o_ptr;
    track_object(args.item);

    object_info_screen(o_ptr);

    args.verify = TRUE;
    return (args);
}


cmd_arg obj_drop(object_type *o_ptr, cmd_arg args)
{
    int amt = get_quantity("Please enter an amount to drop.", o_ptr->number, o_ptr->number, FALSE);
    if (amt <= 0)
    {
        args.verify = FALSE;
        return (args);
    }

    args.verify = TRUE;
    args.number = amt;

    return (args);
}

cmd_arg obj_destroy(object_type *o_ptr, cmd_arg args)
{
    int amt = get_quantity("Please enter an amount to destroy.", o_ptr->number, o_ptr->number, FALSE);
    if (amt <= 0)
    {
        args.verify = FALSE;
        return (args);
    }

    args.verify = TRUE;
    args.number = amt;

    return (args);
}



/*** A universal object handler to avoid repetitive code ***/

/* Item "action" type */
typedef struct
{
    cmd_arg (*action)(object_type *, cmd_arg);
    bool needs_aim;
    QString desc;

    QString prompt;
    QString noop;

    bool (*filter)(object_type *o_ptr);
    int mode;
    bool (*prereq)(void);
} item_act_t;


/* All possible item actions */
static item_act_t item_actions[] =
{

    /* ACTION_UNINSCRIBE */
    { NULL, FALSE, "uninscribe",
      "Un-inscribe which item? ", "You have nothing to un-inscribe.",
      obj_has_inscrip, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER), NULL },

    /* ACTION_INSCRIBE */
    { NULL, FALSE, "inscribe",
      "Inscribe which item? ", "You have nothing to inscribe.",
      NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | IS_HARMLESS | USE_QUIVER), NULL },

    /* ACTION_EXAMINE */
    { obj_examine, FALSE, "examine",
    "Examine which item? ", "You have nothing to examine.",
    NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | IS_HARMLESS | USE_QUIVER), NULL },

    /* ACTION_TAKEOFF */
    { NULL, FALSE, "takeoff",
    "Take off which item? ", "You are not wearing anything you can take off.",
    obj_can_takeoff, (USE_EQUIP | USE_QUIVER), NULL },

    /* ACTION_WIELD */
    { NULL, FALSE, "wield",
    "Wear/Wield which item? ", "You have nothing you can wear or wield.",
    obj_can_wear, (USE_INVEN | USE_FLOOR), NULL },

    /* ACTION_DROP */
    { obj_drop, FALSE, "drop",
    "Drop which item? ", "You have nothing to drop.",
    NULL, (USE_EQUIP | USE_INVEN | USE_QUIVER), NULL },

    /* ACTION_USE_STAFF */
    { NULL,  TRUE, "use",
      "Use which staff? ", "You have no staff to use.",
      obj_is_staff, (USE_INVEN | USE_FLOOR), NULL },

      /* ACTION_AIM_WAND */
    { NULL, TRUE, "aim",
      "Aim which wand? ", "You have no wand to aim.",
      obj_is_wand, (USE_INVEN | USE_FLOOR), NULL },

      /* ACTION_ZAP_ROD */
    { NULL, TRUE, "zap",
      "Zap which rod? ", "You have no charged rods to zap.",
      obj_is_rod, (USE_INVEN | USE_FLOOR), NULL },

      /* ACTION_ACTIVATE */
    { NULL, TRUE, "activate",
      "Activate which item? ", "You have nothing to activate.",
      obj_is_activatable, (USE_EQUIP), NULL },

      /* ACTION_EAT_FOOD */
    { NULL, FALSE, "eat",
      "Eat which item? ", "You have nothing to eat.",
      obj_is_food, (USE_INVEN | USE_FLOOR), NULL },

      /* ACTION_QUAFF_POTION */
    { NULL, FALSE, "quaff",
      "Quaff which potion? ", "You have no potions to quaff.",
      obj_is_potion, (USE_INVEN | USE_FLOOR), NULL },

      /* ACTION_READ_SCROLL */
    { NULL, TRUE, "read",
      "Read which scroll? ", "You have no scrolls to read.",
      obj_is_scroll, (USE_INVEN | USE_FLOOR), player_can_read },

    /* ACTION_REFILL */
   { NULL, FALSE, "refill",
    "Refuel with what fuel source? ", "You have nothing to refuel with.",
    obj_can_refill, (USE_INVEN | USE_FLOOR), NULL },

    /* ACTION_DESTROY */
   { obj_destroy, FALSE, "destroy",
    "Destroy which item? ", "You have nothing to destroy.",
    NULL, (USE_INVEN | USE_FLOOR | USE_QUIVER), NULL },

    /* ACTION_USE_ITEM */
   { NULL, TRUE, "use",
    "Use which item? ", "You have no items to use.",
    obj_is_usable, (USE_INVEN | USE_FLOOR), NULL },

};


/* List matching up to item_actions[] */
typedef enum
{
    ACTION_UNINSCRIBE = 0,
    ACTION_INSCRIBE,
    ACTION_EXAMINE,
    ACTION_TAKEOFF,
    ACTION_WIELD,
    ACTION_DROP,

    ACTION_USE_STAFF,
    ACTION_AIM_WAND,
    ACTION_ZAP_ROD,
    ACTION_ACTIVATE,
    ACTION_EAT_FOOD,
    ACTION_QUAFF_POTION,
    ACTION_READ_SCROLL,
    ACTION_REFILL,
    ACTION_DESTROY,
    ACTION_USE_ITEM
} item_act;

bool trap_related_object(object_type *o_ptr)
{
    if (o_ptr->is_wand() && o_ptr->is_aware())
    {
        if ((o_ptr->sval == SV_WAND_DISARMING) ||
            (o_ptr->sval == SV_WAND_TRAP_DOOR_DEST)) return (TRUE);
    }
    else if (o_ptr->is_rod() && o_ptr->is_aware())
    {
        if (o_ptr->sval == SV_ROD_DISARMING) return (TRUE);
    }

    return (FALSE);
}

/*** Old-style noun-verb functions ***/


/*
 * Generic "select item action" function
 */
static cmd_arg select_item(item_act act)
{
    object_type *o_ptr;

    QString q, s;

    cmd_arg args;
    args.wipe();

    if (item_actions[act].prereq)
    {
        if (!item_actions[act].prereq())
        {
            args.verify = FALSE;
            return (args);
        }
    }

    /* Don't allow activation of swap weapons */
    if (birth_swap_weapons)
    {
        if (act == ACTION_ACTIVATE)  item_tester_swap = TRUE;
    }

    /* Get item */
    q = item_actions[act].prompt;
    s = item_actions[act].noop;
    item_tester_hook = item_actions[act].filter;

    if (!get_item(&args.item, q, s, item_actions[act].mode))
    {
        args.verify = FALSE;
        return (args);
    }

    /* Get the item */
    o_ptr = object_from_item_idx(args.item);

    /* Execute the item command */
    if (item_actions[act].action != NULL)
        args = item_actions[act].action(o_ptr, args);
    else if (item_actions[act].needs_aim && obj_needs_aim(o_ptr))
    {
        bool trap_related = trap_related_object(o_ptr);
        if (!get_aim_dir(&args.direction, trap_related))
        {
            args.verify = FALSE;
            return (args);
        }
    }

    // Success
    args.verify = TRUE;
    return(args);
}




/*** Utility bits and bobs ***/




/*** Inscriptions ***/
void command_uninscribe(cmd_arg args)
{
    bool check_autoinscribe = FALSE;
    object_type *o_ptr = object_from_item_idx(args.item);
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    // Command cancelled
    if(!args.verify) return;

    if (!item_is_available(args.item, NULL, USE_FLOOR | USE_INVEN | USE_EQUIP | USE_QUIVER))
    {
        pop_up_message_box("You do not have that item to uninscribe.");
        return;
    }

    if (!o_ptr->has_inscription())
    {
        pop_up_message_box("That item had no inscription to remove.");
        return;
    }

    // See if the inscriptions match
    if (!k_ptr->autoinscribe.isEmpty())
    {
        if (operator==(k_ptr->autoinscribe, o_ptr->inscription)) check_autoinscribe = TRUE;
    }

    /* Remove the inscription */
    o_ptr->inscription.clear();

    message(QString("Inscription removed."));

    /*The object kind has an autoinscription*/
    if (check_autoinscribe)
    {
        QString tmp_val;
        QString o_name2;

        /*now describe with correct amount*/
        o_name2 = object_desc(o_ptr->k_idx, ODESC_PLURAL | ODESC_FULL);

        /* Prompt */
        tmp_val = (QString("Remove automatic inscription for %1?") .arg(o_name2));

        /* Clear Auto-Inscribe if they want that */
        if (get_check(tmp_val)) k_ptr->autoinscribe.clear();
    }

    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
}

/*
 * Remove inscription
 */
void do_cmd_uninscribe(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_UNINSCRIBE);

    command_uninscribe(args);
}

void command_inscribe(cmd_arg args)
{
    object_type *o_ptr = object_from_item_idx(args.item);
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    bool check_autoinscribe = FALSE;

    QString o_name;
    QString new_inscription;

    // Command cancelled
    if(!args.verify) return;

    if (!item_is_available(args.item, NULL, USE_FLOOR | USE_INVEN | USE_EQUIP | USE_QUIVER))
    {
        pop_up_message_box("You do not have that item to inscribe.");
        return;
    }

    /* Describe the activity */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    QString tmp2 = (QString("Inscribing %1.") .arg(o_name));

    new_inscription = get_string("Please enter an inscription", QString(tmp2), o_ptr->inscription);

    // Nothing inscribed
    if (new_inscription.isEmpty()) return;

    if (!o_ptr->is_artifact() && operator!=(k_ptr->autoinscribe, new_inscription)) check_autoinscribe = TRUE;

    if (check_autoinscribe)
    {
        /* Get a new inscription */
        QString tmp_val;
        QString o_name2;

        /*now describe with correct amount*/
        o_name2 = object_desc(o_ptr->k_idx, ODESC_PLURAL | ODESC_FULL);

        /* Prompt */
        tmp_val = (QString("Automatically inscribe all %1 with %2?") .arg(o_name2) .arg(new_inscription));

        /* Auto-Inscribe if they want that */
        if (get_check(tmp_val)) k_ptr->autoinscribe = new_inscription;
    }

    /* Save the inscription */
    o_ptr->inscription = new_inscription;

    /* Combine / Reorder the pack */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

}

/*
 * Add inscription
 */
void do_cmd_inscribe(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_INSCRIBE);

    command_inscribe(args);
}

void command_examine(cmd_arg args)
{
    object_type *o_ptr = object_from_item_idx(args.item);

    obj_examine(o_ptr, args);
}


//Inspect an item
void do_cmd_examine(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    select_item(ACTION_EXAMINE);
}

/*** Taking off/putting on ***/

//Actually take off the item.

void command_takeoff(cmd_arg args)
{
    // Command cancelled
    if(!args.verify) return;

    if (!item_is_available(args.item, NULL, USE_EQUIP | USE_QUIVER))
    {
        pop_up_message_box("You are not wielding that item.");
        return;
    }

    if (!obj_can_takeoff(object_from_item_idx(args.item)))
    {
        message(QString("You cannot take off that item."));
        return;
    }

    if (!get_item_allow(args.item, VERIFY_TAKEOFF)) return;

    p_ptr->message_append_start();

    (void)inven_takeoff(args.item, 255);
    pack_overflow();
    if(!p_ptr->in_store) process_player_energy(BASE_ENERGY_MOVE / 2);
    return;
}

/*
 * Take off an item (keyboard command)
 */
void do_cmd_takeoff(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_TAKEOFF);

    (void)command_takeoff(args);
}

// Handle wielding the item.
void command_wield(cmd_arg args)
{
    object_type *equip_o_ptr;
    QString o_name;

    // Command cancelled
    if(!args.verify) return;

    object_type *o_ptr = object_from_item_idx(args.item);

    args.slot = wield_slot(o_ptr);

    if (!item_is_available(args.item, NULL, USE_INVEN | USE_FLOOR))
    {
        pop_up_message_box("You do not have that item to wield.");
        return;
    }

    /* Usually if the slot is taken we'll just replace the item in the slot,
     * but in some cases we need to ask the user which slot they actually
     * want to replace */
    if (inventory[args.slot].k_idx)
    {
        if (o_ptr->is_ring())
        {
            QString q = "Replace which ring? ";
            QString s = "Error in obj_wield, please report";
            item_tester_hook = obj_is_ring;
            if (!get_item(&args.slot, q, s, USE_EQUIP))
            {
                return;
            }
        }

        if (o_ptr->is_ammo() && !object_similar(&inventory[args.slot], o_ptr))
        {
            QString q = "Replace which ammunition? ";
            QString s = "Error in obj_wield, please report";
            item_tester_hook = obj_is_ammo;
            if (!get_item(&args.slot, q, s, USE_QUIVER))
            {
                return;
            }
        }
    }

    /* Check the slot */
    if (!slot_can_wield_item(args.slot, o_ptr))
    {
        o_name = object_desc(o_ptr,  ODESC_PREFIX | ODESC_FULL);

        pop_up_message_box("You cannot wield that item there.");
        return;
    }

    /*Hack - don't allow quest items to be worn*/
    if(o_ptr->is_quest_object())
    {
        pop_up_message_box("You cannot wield quest items.");
        return;
    }

    if (!get_item_allow(args.item, VERIFY_WIELD)) return;

    /* Hack - Throwing weapons can be wielded in the quiver too. */
    if (is_throwing_weapon(o_ptr) && !IS_QUIVER_SLOT(args.slot))
    {
        if (o_ptr->use_verify[AUTO_WIELD_QUIVER]) args.slot = QUIVER_START;
    }

    equip_o_ptr = &inventory[args.slot];

    /* If the slot is open, wield and be done */
    if (!equip_o_ptr->k_idx)
    {
        wield_item(o_ptr, args.item, args.slot);
        if (!p_ptr->in_store) process_player_energy(BASE_ENERGY_MOVE);
        return;
    }

    /* If the slot is in the quiver and objects can be combined */
    if (((obj_is_ammo(equip_o_ptr)) || (is_throwing_weapon(o_ptr)))
                          && object_similar(equip_o_ptr, o_ptr))
    {
        wield_item(o_ptr, args.item, args.slot);
        if (!p_ptr->in_store) process_player_energy(BASE_ENERGY_MOVE);
        return;
    }

    /* Prevent wielding into a cursed slot */
    if (equip_o_ptr->is_cursed())
    {
        o_name = object_desc(equip_o_ptr,  ODESC_BASE);
        pop_up_message_box(QString("The %1 you are %2 appears to be cursed.") .arg(o_name) .arg(describe_use(args.slot)));
        return;
    }

    /* checks for taking off old equipment*/
    if (!get_item_allow(args.slot, VERIFY_TAKEOFF)) return;

    p_ptr->message_append_start();

    wield_item(o_ptr, args.item, args.slot);
    if (!p_ptr->in_store) process_player_energy(BASE_ENERGY_MOVE);
    return;
}


/*
 * Wield or wear an item - (keyboard command)
 */
void do_cmd_wield()
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_WIELD);

    (void)command_wield(args);
}

void command_drop(cmd_arg args)
{

    int item = args.item;
    object_type *o_ptr = object_from_item_idx(item);
    int amt = args.number;

    // Command cancelled
    if(!args.verify) return;

    if (!item_is_available(item, NULL, USE_INVEN | USE_EQUIP | USE_QUIVER))
    {
        message(QString("You do not have that item to drop."));
        return;
    }

    /* Hack -- Cannot remove cursed items */
    if ((item >= INVEN_WIELD) && o_ptr->is_cursed())
    {
        pop_up_message_box("Hmmm, it seems to be cursed.");
        return;
    }

    /* Cursed quiver */
    else if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver)
    {
        /* Oops */
        pop_up_message_box("Your quiver is cursed!");

        /* Nope */
        return;
    }

    if (!get_item_allow(args.item, VERIFY_DROP)) return;

    p_ptr->message_append_start();

    inven_drop(item, amt);
    process_player_energy(BASE_ENERGY_MOVE / 2);

    return;
}

/*
 * Drop an item
 */
void do_cmd_drop(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_DROP);

    command_drop(args);
}

static void refill_lamp(object_type *j_ptr, object_type *o_ptr, int item)
{

    /* Refuel from a lantern */
    if (o_ptr->sval == SV_LIGHT_LANTERN)
    {
        j_ptr->timeout += o_ptr->timeout;
    }
    /* Refuel from a flask */
    else
    {
        j_ptr->timeout += o_ptr->pval;
    }

    /* Message */
    message(QString("You fuel your lamp."));

    /* Comment */
    if (j_ptr->timeout >= FUEL_LAMP)
    {
        j_ptr->timeout = FUEL_LAMP;
        message(QString("Your lamp is full."));
    }

    /* Refilled from a lantern */
    if (o_ptr->sval == SV_LIGHT_LANTERN)
    {

        /* Unstack if necessary */
        if (o_ptr->number > 1)
        {
            object_type *i_ptr;
            object_type object_type_body;

            /* Get local object */
            i_ptr = &object_type_body;

            /* Obtain a local object */
            i_ptr->object_copy(o_ptr);

            /* Modify quantity */
            i_ptr->number = 1;

            /* Remove fuel */
            i_ptr->timeout = 0;

            /* Unstack the used item */
            o_ptr->number--;

            /* Carry or drop */
            if (item >= 0)
                item = inven_carry(i_ptr);
            else
                drop_near(i_ptr, 0, p_ptr->py, p_ptr->px);
        }

        /* Empty a single lantern */
        else
        {
            /* No more fuel */
            o_ptr->timeout = 0;
        }

        /* Combine / Reorder the pack (later) */
        p_ptr->update |= (PU_BONUS);
        p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY);
    }

    /* Refilled from a flask */
    else
    {
        /* Decrease the item (from the pack) */
        if (item >= 0)
        {
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }

        /* Decrease the item (from the floor) */
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
    }

    /* Recalculate torch */
    p_ptr->update |= (PU_TORCH);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_EQUIPMENT);
}


static void refuel_torch(object_type *j_ptr, object_type *o_ptr, int item)
{
    /* Refuel */
    j_ptr->timeout += o_ptr->timeout + 5;

    /* Message */
    message(QString("You combine the torches."));

    /* Over-fuel message */
    if (j_ptr->timeout >= FUEL_TORCH)
    {
        j_ptr->timeout = FUEL_TORCH;
        message(QString("Your torch is fully fueled."));
    }

    /* Refuel message */
    else
    {
        message(QString("Your torch glows more brightly."));
    }

    /* Decrease the item (from the pack) */
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }

    /* Decrease the item (from the floor) */
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    /* Recalculate torch */
    p_ptr->update |= (PU_TORCH);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_EQUIPMENT);
}


/*
 * Lanterns can be filled with oil on the dungeon's floor.
 * Returns TRUE if the amount of fuel could be filled in this way.
 */
static bool do_cmd_refill_lamp_from_terrain(void)
{
    object_type *j_ptr;

    /* Get player coordinates */
    int y = p_ptr->py;
    int x = p_ptr->px;

    /* Assume that the oil can be dry */
    bool can_dry = TRUE;

    /* Check presence of oil */
    if (!(cave_ff3_match(y, x, FF3_OIL) && cave_passable_bold(y, x))) return (FALSE);

    /* Ask the user if he/she wants to use the oil on the grid */
    if (!get_check("Do you want to refill your lamp from the oil on the floor? ")) return (FALSE);

    /* Get the lantern */
    j_ptr = &inventory[INVEN_LIGHT];

    /* Increase fuel amount (not too much) */
    j_ptr->timeout += (200 + rand_int(3) * 50);

    /* Increase fuel amount even more if there is more oil on that grid */
    if (cave_ff2_match(y, x, FF2_DEEP)) j_ptr->timeout += (100 + rand_int(2) * 50);

    /* Message */
    message(QString("You fuel your lamp."));

    /* Comment */
    if (j_ptr->timeout >= FUEL_LAMP)
    {
        /* Some oil was unused */
        if (j_ptr->timeout > FUEL_LAMP)
        {
            /* Set oil to the max. value */
            j_ptr->timeout = FUEL_LAMP;

            /* Give the remaining oil to the grid */
            can_dry = FALSE;
        }

        /* Message */
        message(QString("Your lamp is full."));
    }

    /* Sometimes we remove the oil grid to stop oil-abuse */
    if (can_dry && !f_info[FEAT_FLOOR_EARTH].f_name.length() && one_in_(7))
    {
        /* Transform to earth */
        cave_set_feat(y, x, FEAT_FLOOR_EARTH);

        /* Message */
        message(QString("The oil patch dries."));
    }

    /* Recalculate torch */
    p_ptr->update |= (PU_TORCH);

    /* Window stuff */
    p_ptr->redraw |= (PR_WIN_EQUIPMENT);

    /* Success */
    return (TRUE);
}

/*
 * Refill the players lamp, or restock his torches
 */
void command_refuel(cmd_arg args)
{
    object_type *j_ptr = &inventory[INVEN_LIGHT];

    // Command cancelled
    if(!args.verify) return;

    if (j_ptr->sval == SV_LIGHT_LANTERN)
    {
        p_ptr->message_append_start();
        if (do_cmd_refill_lamp_from_terrain())
        {
            p_ptr->player_previous_command_update(CMD_REFUEL, args);
            process_player_energy(BASE_ENERGY_MOVE / 2);
            return;
        }
    }

    int item = args.item;
    object_type *o_ptr = object_from_item_idx(item);

    if (!item_is_available(item, NULL, USE_INVEN | USE_FLOOR))
    {
        pop_up_message_box("You do not have that item to refill with it.");
        return;
    }

    if (!get_item_allow(item, VERIFY_REFILL)) return;

    /* It is nothing */
    if (j_ptr->tval != TV_LIGHT)
    {
        pop_up_message_box("You are not wielding a light.");
        return;
    }

    /* It's a lamp */
    else if (j_ptr->sval == SV_LIGHT_LANTERN)
    {
        p_ptr->message_append_start();
        refill_lamp(j_ptr, o_ptr, item);
    }

    /* It's a torch */
    else if (j_ptr->sval == SV_LIGHT_TORCH)
    {
        p_ptr->message_append_start();
        refuel_torch(j_ptr, o_ptr, item);
    }

    p_ptr->player_previous_command_update(CMD_REFUEL, args);
    p_ptr->command_previous_args.k_idx = o_ptr->k_idx;

    if (!p_ptr->in_store) process_player_energy(BASE_ENERGY_MOVE / 2);
}


/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refuel(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_REFILL);

    (void) command_refuel(args);

}

static void swap_weapons(void)
{
    object_type *o_ptr = &inventory[INVEN_MAIN_WEAPON];
    object_type *j_ptr = &inventory[INVEN_SWAP_WEAPON];
    object_type object_type_body;
    object_type *i_ptr = & object_type_body;
    QString o_name;
    QString act;

    /* Not holding anything */
    if ((!o_ptr->k_idx) && (!j_ptr->k_idx))
    {
        pop_up_message_box(QString("But you are wielding no weapons."));
        return;
    }

    /* Can't swap because of a cursed weapon */
    if (o_ptr->is_cursed())
    {
        o_name = object_desc(o_ptr,  ODESC_BASE);
        pop_up_message_box(QString("The %1 you are %2 appears to be cursed.") .arg(o_name) .arg(describe_use(INVEN_MAIN_WEAPON)));
        return;
    }

    /* Give the player a message for the item they are taking off */
    if (o_ptr->k_idx)
    {
        /* The player took off a bow */
        if (o_ptr->is_bow())
        {
            act = "You were shooting";
        }

        /* Took off weapon */
        else act = "You were wielding";

        /* Describe the object */
        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
        message(QString("%1 %2 (%3).") .arg(act) .arg(o_name) .arg(index_to_label(INVEN_SWAP_WEAPON)));
    }

    /* Make a record of the primary weapon, and wipe it */
    i_ptr->object_copy(o_ptr);
    o_ptr->object_wipe();

    /* Insert the swap weapon if there is one */
    if (j_ptr->k_idx)
    {
        wield_item(j_ptr, INVEN_SWAP_WEAPON, INVEN_MAIN_WEAPON);
    }

    /* if a previous weapon, place it in the secondary weapon slot */
    if (i_ptr->k_idx)
    {
        j_ptr->object_copy(i_ptr);
    }

    /* Recalculate bonuses, torch, mana */
    p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

    process_player_energy(BASE_ENERGY_MOVE);
}

/* Search the backpack for a weapon with @x and wield it*/
static void wield_swap_weapon(void)
{
    int i;

    object_type *o_ptr = &inventory[INVEN_MAIN_WEAPON];

    /* Can't swap because of a cursed weapon */
    if (o_ptr->is_cursed())
    {
        QString o_name;

        o_name = object_desc(o_ptr,  ODESC_BASE);
        pop_up_message_box(QString("The %1 you are %2 appears to be cursed.") .arg(o_name) .arg(describe_use(INVEN_MAIN_WEAPON)));
        return;
    }

    /* Check every object */
    for (i = 0; i < INVEN_MAX_PACK; ++i)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Skip empty inscriptions */
        if (o_ptr->inscription.isEmpty()) continue;

        if(!o_ptr->is_weapon()) continue;

        if (!o_ptr->use_verify[AUTO_SWAP]) continue;

        p_ptr->message_append_start();

        /* Wield it */
        wield_item(o_ptr, i, INVEN_MAIN_WEAPON);

        /* Recalculate bonuses, torch, mana */
        p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

        /* We are done */
        process_player_energy(BASE_ENERGY_MOVE);
        return;
    }

    /* Didn't find anything */
    pop_up_message_box("Please inscribe a weapon with '@x' in order to swap it.");
}

/*
 * Depending on game options, either swap weapons between the main weapon
 * slot and the swap weapon slot, or search for weapon with the @x inscription and wield it
 */
void command_swap(cmd_arg args)
{
    // Paranoia
    if (!p_ptr->playing) return;

    p_ptr->player_previous_command_update(CMD_SWAP, args);

    if (birth_swap_weapons) swap_weapons();
    else wield_swap_weapon();
}

/*
 * Depending on game options, either swap weapons between the main weapon
 * slot and the swap weapon slot, or search for weapon with the swap flag inscription and wield it
 */
void do_cmd_swap_weapon()
{
    cmd_arg args;
    args.wipe();
    command_swap(args);
}

ObjectDestroyDialog::ObjectDestroyDialog(s16b o_idx)
{
    QVBoxLayout *main_layout = new QVBoxLayout;

    o_ptr = object_from_item_idx(o_idx);
    k_ptr = &k_info[o_ptr->k_idx];
    squelch_type = squelch_type_of(o_ptr);

    QLabel *header_main = new QLabel("<b><h2>Confirm Object Destruction</b></h2>");
    header_main->setAlignment(Qt::AlignCenter);
    main_layout->addWidget(header_main);

    setLayout(main_layout);
    setWindowTitle(tr("Object Menu"));

    main_layout->addStretch(1);

    // Add squelch settings, except for the instant artifacts
    if (!o_ptr->art_num)
    {
        QVBoxLayout *squelch_buttons = new QVBoxLayout;
        main_layout->addLayout(squelch_buttons);
        add_squelch_buttons(squelch_buttons);

        QVBoxLayout *quality_buttons = new QVBoxLayout;
        main_layout->addLayout(quality_buttons);
        add_quality_buttons(quality_buttons);

        QVBoxLayout *ego_buttons = new QVBoxLayout;
        main_layout->addLayout(ego_buttons);
        add_ego_buttons(ego_buttons);
    }

    QLabel *object_name = new QLabel(QString("<br><h2>Really destroy %1?</h2><br>") .arg(object_desc(o_ptr, ODESC_FULL)));
    object_name->setAlignment(Qt::AlignCenter);
    main_layout->addWidget(object_name);

    main_layout->addStretch(1);

    //Add a close buttons
    QDialogButtonBox *buttons = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    connect(buttons, SIGNAL(accepted()), this, SLOT(accept()));
    main_layout->addWidget(buttons);

    setLayout(main_layout);
    setWindowTitle(tr("Verify Destroy"));

}

/*
 * Destroy an item
 */
void command_destroy(cmd_arg args)
{
    // Command cancelled
    if(!args.verify) return;

    int item = args.item;
    object_type *o_ptr = object_from_item_idx(item);

    if (!item_is_available(item, NULL, USE_INVEN | USE_FLOOR | USE_QUIVER))
    {
        pop_up_message_box("You do not have that item to destroy.");
        return;
    }

    int amt = args.number;
    int old_number;
    int old_charges = 0;

    QString o_name;

    QString out_val;

    /* Can't destroy cursed items we're wielding. */
    if ((item >= INVEN_WIELD) && o_ptr->is_cursed())
    {
        message(QString("You cannot destroy the cursed item."));
        return;
    }

    /* Cursed quiver */
    else if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver)
    {
        /* Oops */
        message(QString("Your quiver is cursed!"));
        return;
    }

    /* Allow user abort */
    if (amt <= 0) return;

    /* Describe the object */
    old_number = o_ptr->number;

    /*Hack, state the correct number of charges to be destroyed if wand, rod or staff*/
    if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF) || o_ptr->tval == TV_ROD)
        && (amt < o_ptr->number))
    {
        /*save the number of charges*/
        old_charges = o_ptr->pval;

        /*distribute the charges*/
        o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;

        o_ptr->pval = old_charges - o_ptr->pval;
    }

    /*hack -  make sure we get the right amount displayed*/
    o_ptr->number = amt;

    /*now describe with correct amount*/
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /*reverse the hack*/
    o_ptr->number = old_number;

    /* Verify destruction */
    if (verify_destroy)
    {
        ObjectDestroyDialog *dlg = new ObjectDestroyDialog(item);
        bool result = dlg->exec();
        delete dlg;
        if (!result) return;
    }

    if (!get_item_allow(item, VERIFY_DESTROY)) return;

    /* Artifacts cannot be destroyed */
    if (o_ptr->is_artifact())
    {
        /* Message */
        message(QString("You cannot destroy %1.") .arg(o_name));

        /* Don't mark id'ed objects */
        if (o_ptr->is_known()) return;

        /* It has already been sensed */
        if (o_ptr->ident & (IDENT_SENSE))
        {
            /* Already sensed objects always get improved feelings */
            if (o_ptr->is_cursed() || o_ptr->is_broken())
                o_ptr->discount = INSCRIP_TERRIBLE;
            else
                o_ptr->discount = INSCRIP_SPECIAL;
        }
        else
        {
            /* Mark the object as indestructible */
            o_ptr->discount = INSCRIP_INDESTRUCTIBLE;
        }

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        p_ptr->update |= (PU_PLAYER_SCORE);

        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT | PR_STATUSBAR);

        /* Done */
        return;
    }

    p_ptr->message_append_start();

    /* Message */
    message(QString("You destroy %1.") .arg(o_name));

    /*hack, restore the proper number of charges after the messages have printed
     * so the proper number of charges are destroyed*/
    if (old_charges) o_ptr->pval = old_charges;

    /* Reduce the charges of rods/wands */
    reduce_charges(o_ptr, amt);

    /* Eliminate the item (from the pack) */
    if (item >= 0)
    {
        inven_item_increase(item, -amt);
        inven_item_describe(item);
        inven_item_optimize(item);
    }

    /* Eliminate the item (from the floor) */
    else
    {
        floor_item_increase(0 - item, -amt);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    process_player_energy(BASE_ENERGY_MOVE);
}


/*
 * Handle the user command to destroy an item
 */
void do_cmd_destroy(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_DESTROY);

    command_destroy(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_activate(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_ACTIVATE);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_use_item(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_USE_ITEM);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_aim_wand(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_AIM_WAND);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_use_staff(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_USE_STAFF);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_zap_rod(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_ZAP_ROD);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_eat_food(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_EAT_FOOD);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_quaff_potion(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_QUAFF_POTION);

    command_use(args);
}

/*
 * Handle the user command to destroy an item
 */
void do_cmd_read_scroll(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    cmd_arg args = select_item(ACTION_READ_SCROLL);

    command_use(args);
}

