/* File: cmd3.c */

/* Purpose: Inventory commands */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "int-map.h"
#include <assert.h>

/*
 * Display inventory
 */
void do_cmd_inven(void)
{
    char out_val[160];


    /* Note that we are in "inventory" mode */
    command_wrk = FALSE;

#ifdef ALLOW_EASY_FLOOR

    /* Note that we are in "inventory" mode */
    if (easy_floor) command_wrk = (USE_INVEN);

#endif /* ALLOW_EASY_FLOOR */

    /* Save screen */
    screen_save();

    /* Hack -- show empty slots */
    item_tester_full = TRUE;

    /* Display the inventory */
    (void)show_inven(0, 0);

    /* Hack -- hide empty slots */
    item_tester_full = FALSE;

    sprintf(out_val, "Inventory: carrying %d.%d pounds (%d%% of capacity). Command: ",
        (int)(p_ptr->total_weight / 10), (int)(p_ptr->total_weight % 10),
        (p_ptr->total_weight * 100) / weight_limit());


    /* Get a command */
    prt(out_val, 0, 0);

    /* Get a new command */
    command_new = inkey();

    /* Load screen */
    screen_load();


    /* Process "Escape" */
    if (command_new == ESCAPE)
    {
        int wid, hgt;

        /* Get size */
        Term_get_size(&wid, &hgt);

        /* Reset stuff */
        command_new = 0;
        command_gap = wid - 30;
    }

    /* Process normal keys */
    else
    {
        /* Hack -- Use "display" mode */
        command_see = TRUE;
    }
}


/*
 * Display equipment
 */
void do_cmd_equip(void)
{
    char out_val[160];


    /* Note that we are in "equipment" mode */
    command_wrk = TRUE;

#ifdef ALLOW_EASY_FLOOR

    /* Note that we are in "equipment" mode */
    if (easy_floor) command_wrk = (USE_EQUIP);

#endif /* ALLOW_EASY_FLOOR  */

    /* Save the screen */
    screen_save();

    /* Hack -- show empty slots */
    item_tester_full = TRUE;

    /* Display the equipment */
    (void)show_equip(0, 0);

    /* Hack -- undo the hack above */
    item_tester_full = FALSE;

    /* Build a prompt */
    sprintf(out_val, "Equipment: carrying %d.%d pounds (%d%% of capacity). Command: ",
        (int)(p_ptr->total_weight / 10), (int)(p_ptr->total_weight % 10),
        (p_ptr->total_weight * 100) / weight_limit());

    /* Get a command */
    prt(out_val, 0, 0);

    /* Get a new command */
    command_new = inkey();

    /* Restore the screen */
    screen_load();


    /* Process "Escape" */
    if (command_new == ESCAPE)
    {
        int wid, hgt;

        /* Get size */
        Term_get_size(&wid, &hgt);

        /* Reset stuff */
        command_new = 0;
        command_gap = wid - 30;
    }

    /* Process normal keys */
    else
    {
        /* Enter "display" mode */
        command_see = TRUE;
    }
}


void kamaenaoshi(int item)
{
}

/*
 * Drop an item
 */
void do_cmd_drop(void)
{
    int item, amt = 1;

    object_type *o_ptr;

    cptr q, s;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    item_tester_no_ryoute = TRUE;
    /* Get an item */
    q = "Drop which item? ";
    s = "You have nothing to drop.";

    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];

        /* Ugly hack! */
        if ( object_is_melee_weapon(o_ptr) 
          && equip_is_valid_slot(item)
          && p_ptr->pclass == CLASS_PSION
          && psion_weapon_graft() )
        {
            msg_print("Failed!  Your weapon is currently grafted to your arm!");
            return;
        }
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }


    /* Hack -- Cannot remove cursed items */
    if (equip_is_valid_slot(item))
    {
        if (object_is_cursed(o_ptr))
        {
            msg_print("Hmmm, it seems to be cursed.");
            return;
        }
        if (have_flag(o_ptr->art_flags, TR_NO_REMOVE))
        {
            msg_print("You can't drop yourself, silly!");
            return;
        }
    }

    if (o_ptr->tval == TV_POTION && o_ptr->sval == SV_POTION_BLOOD)
    {
        msg_print("You can't do that!  Your blood will go sour!");
        return;
    }

    /* See how many items */
    if (o_ptr->number > 1)
    {
        /* Get a quantity */
        amt = get_quantity(NULL, o_ptr->number);

        /* Allow user abort */
        if (amt <= 0) return;
    }


    /* Take a partial turn */
    energy_use = 50;

    /* Drop (some of) the item */
    inven_drop(item, amt);

    if (equip_is_valid_slot(item))
        calc_android_exp();

    p_ptr->redraw |= (PR_EQUIPPY);
}


static bool high_level_book(object_type *o_ptr)
{
    if ((o_ptr->tval == TV_LIFE_BOOK) ||
        (o_ptr->tval == TV_SORCERY_BOOK) ||
        (o_ptr->tval == TV_NATURE_BOOK) ||
        (o_ptr->tval == TV_CHAOS_BOOK) ||
        (o_ptr->tval == TV_DEATH_BOOK) ||
        (o_ptr->tval == TV_TRUMP_BOOK) ||
        (o_ptr->tval == TV_CRAFT_BOOK) ||
        (o_ptr->tval == TV_DAEMON_BOOK) ||
        (o_ptr->tval == TV_CRUSADE_BOOK) ||
        (o_ptr->tval == TV_NECROMANCY_BOOK) ||
        (o_ptr->tval == TV_ARMAGEDDON_BOOK) ||
        (o_ptr->tval == TV_MUSIC_BOOK) ||
        (o_ptr->tval == TV_HEX_BOOK))
    {
        if (o_ptr->sval > 1)
            return TRUE;
        else
            return FALSE;
    }

    return FALSE;
}


/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
    int          item, amt = 1;
    int          old_number;
    bool         force = FALSE;
    object_type *o_ptr;
    object_type  forge;
    object_type *q_ptr = &forge;
    bool         is_equipped = FALSE;
    char         o_name[MAX_NLEN];
    char         out_val[MAX_NLEN+40];

    cptr q, s;
    int mode = USE_INVEN | USE_FLOOR;

    if (p_ptr->pclass == CLASS_RUNE_KNIGHT)
        mode |= USE_EQUIP;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Hack -- force destruction */
    if (command_arg > 0) force = TRUE;


    /* Get an item */
    q = "Destroy which item? ";
    s = "You have nothing to destroy.";

    if (!get_item(&item, q, s, mode)) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
        is_equipped = equip_is_valid_slot(item);
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Hack for Rune Knight: They can destroy worn equipment, but only
       if it has the Sacrifice rune.  get_item() is not smart enough
       to handle this restriction ... */
    if (is_equipped && o_ptr->rune != RUNE_SACRIFICE)
    {
        msg_print("You must first remove that item before destroying it.");
        return;
    }

    /* Verify unless quantity given beforehand */
    if (!force && (confirm_destroy || (object_value(o_ptr) > 0)))
    {
        object_desc(o_name, o_ptr, OD_OMIT_PREFIX);

        /* Make a verification */
        sprintf(out_val, 
            "Really destroy %s? [y/n/Auto]",
            o_name);

        msg_print(NULL);

        /* HACK : Add the line to message buffer */
        message_add(out_val);
        p_ptr->window |= (PW_MESSAGE);
        window_stuff();

        /* Get an acceptable answer */
        while (TRUE)
        {
            char i;

            /* Prompt */
            prt(out_val, 0, 0);

            i = inkey();

            /* Erase the prompt */
            prt("", 0, 0);


            if (i == 'y' || i == 'Y')
            {
                break;
            }
            if (i == ESCAPE || i == 'n' || i == 'N')
            {
                /* Cancel */
                return;
            }
            if (i == 'A')
            {
                /* Add an auto-destroy preference line */
                if (autopick_autoregister(o_ptr))
                {
                    /* Auto-destroy it */
                    autopick_alter_item(item, TRUE);
                }
                else
                    msg_print("Initialize the auto-pick preferences first (Type '_').");

                /* The object is already destroyed. */
                return;
            }
        } /* while (TRUE) */
    }

    /* See how many items */
    if (o_ptr->number > 1)
    {
        /* Get a quantity */
        amt = get_quantity(NULL, o_ptr->number);

        /* Allow user abort */
        if (amt <= 0) return;
    }


    /* Describe the object */
    old_number = o_ptr->number;
    o_ptr->number = amt;
    object_desc(o_name, o_ptr, 0);
    o_ptr->number = old_number;

    /* Take a turn */
    energy_use = 100;

    /* Artifacts cannot be destroyed */
    if (!can_player_destroy_object(o_ptr))
    {
        energy_use = 0;

        /* Message */
        msg_format("You cannot destroy %s.", o_name);

        /* Done */
        return;
    }

    object_copy(q_ptr, o_ptr);

    stats_on_destroy(o_ptr, amt);

    if (prace_is_(RACE_MON_JELLY))
        jelly_eat_object(o_ptr);
    else if (prace_is_(RACE_MON_SWORD) && object_is_melee_weapon(o_ptr))
        sword_absorb_object(o_ptr);
    else if (prace_is_(RACE_MON_RING) && object_is_jewelry(o_ptr))
        ring_absorb_object(o_ptr);
    else
        msg_format("You destroy %s.", o_name);

    if (o_ptr->rune == RUNE_SACRIFICE)
    {
        int add_hp = is_equipped ? p_ptr->mhp : p_ptr->mhp/3;
        int add_sp = is_equipped ? p_ptr->msp : p_ptr->msp/3;

        msg_print("You feel a surge of wondrous power enter your body.");
        
        p_ptr->chp = MIN(p_ptr->mhp, p_ptr->chp + add_hp);
        p_ptr->chp_frac = 0;
        p_ptr->csp = MIN(p_ptr->msp, p_ptr->csp + add_sp);
        p_ptr->csp_frac = 0;

        p_ptr->redraw |= (PR_MANA);
        p_ptr->window |= (PW_PLAYER);
        p_ptr->window |= (PW_SPELL);
        p_ptr->redraw |= (PR_HP);

        if (is_equipped)
        {
            blast_object(o_ptr);
            o_ptr->curse_flags = TRC_HEAVY_CURSE;
        }
    }
    else if (is_equipped)
        blast_object(o_ptr);

    sound(SOUND_DESTITEM);

    /* Reduce the charges of rods/wands */
    reduce_charges(o_ptr, amt);

    /* Eliminate the item (from the pack) */
    if (item >= 0)
    {
        if (!is_equipped)
        {
            inven_item_increase(item, -amt);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
    }

    /* Eliminate the item (from the floor) */
    else
    {
        floor_item_increase(0 - item, -amt);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    if ( p_ptr->pclass == CLASS_NECROMANCER
      && (q_ptr->tval == TV_LIFE_BOOK || q_ptr->tval == TV_CRUSADE_BOOK) )
    {
        int sp = 0;
        int osp = p_ptr->csp;
        switch (q_ptr->sval)
        {
        case 0: sp = 10; break;
        case 1: sp = 25; break;
        case 2: sp = 100; break;
        case 3: sp = 666; break;
        }

        p_ptr->csp += sp;
        if (p_ptr->csp >= p_ptr->msp)
        {
            p_ptr->csp = p_ptr->msp;
            p_ptr->csp_frac = 0;
        }

        if (p_ptr->csp > osp)
            msg_print("You feel your head clear.");

        p_ptr->redraw |= (PR_MANA);
    }

    if (high_level_book(q_ptr))
    {
        bool gain_expr = FALSE;

        if (p_ptr->prace == RACE_ANDROID)
        {
        }
        else if ((p_ptr->pclass == CLASS_WARRIOR) || (p_ptr->pclass == CLASS_BERSERKER))
        {
            gain_expr = TRUE;
        }
        else if (p_ptr->pclass == CLASS_PALADIN)
        {
            if (is_good_realm(p_ptr->realm1))
            {
                if (!is_good_realm(tval2realm(q_ptr->tval))) gain_expr = TRUE;
            }
            else
            {
                if (is_good_realm(tval2realm(q_ptr->tval))) gain_expr = TRUE;
            }
        }

        if (gain_expr && (p_ptr->exp < PY_MAX_EXP))
        {
            s32b tester_exp = p_ptr->max_exp / 20;
            if (tester_exp > 10000) tester_exp = 10000;
            if (q_ptr->sval < 3) tester_exp /= 4;
            if (tester_exp<1) tester_exp = 1;

            msg_print("You feel more experienced.");
            gain_exp(tester_exp * amt);
        }
    }

    if (high_level_book(q_ptr) && q_ptr->tval == TV_LIFE_BOOK)
    {
        virtue_add(VIRTUE_UNLIFE, 1);
        virtue_add(VIRTUE_VITALITY, -1);
    }
    else if ( high_level_book(q_ptr) 
           && (q_ptr->tval == TV_DEATH_BOOK || q_ptr->tval == TV_NECROMANCY_BOOK) )
    {
        virtue_add(VIRTUE_UNLIFE, -1);
        virtue_add(VIRTUE_VITALITY, 1);
    }    

    if (q_ptr->to_a || q_ptr->to_h || q_ptr->to_d)
        virtue_add(VIRTUE_ENCHANTMENT, -1);
    
    if (object_value_real(q_ptr) > 30000)
        virtue_add(VIRTUE_SACRIFICE, 2);
    
    else if (object_value_real(q_ptr) > 10000)
        virtue_add(VIRTUE_SACRIFICE, 1);

    if (q_ptr->to_a != 0 || q_ptr->to_d != 0 || q_ptr->to_h != 0)
        virtue_add(VIRTUE_HARMONY, 1);

    if (equip_is_valid_slot(item)) 
        calc_android_exp();
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
    int            item;

    object_type        *o_ptr;

    char        o_name[MAX_NLEN];

    cptr q, s;

    item_tester_no_ryoute = TRUE;
    /* Get an item */
    q = "Examine which item? ";
    s = "You have nothing to examine.";

    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }


    /* Note, descriptions for potions, scrolls, wands, staves and rods all spoil 
       the object's effects. Some of the light and jewelry descriptions are also TMI.
       Descriptions for weapons and armor should always be displayed. */
    if (!object_is_weapon_armour_ammo(o_ptr) && !object_is_known(o_ptr))
    {
        msg_print("You have no special knowledge about that item.");
        return;
    }

    /* Description */
    object_desc(o_name, o_ptr, 0);

    /* Describe */
    msg_format("Examining %s...", o_name);

    /* Describe it fully */
    if (!screen_object(o_ptr, SCROBJ_FORCE_DETAIL)) msg_print("You see nothing special.");
}



/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
    int   item;

    object_type *o_ptr;

    cptr q, s;

    item_tester_no_ryoute = TRUE;
    /* Get an item */
    q = "Un-inscribe which item? ";
    s = "You have nothing to un-inscribe.";

    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Nothing to remove */
    if (!o_ptr->inscription)
    {
        msg_print("That item had no inscription to remove.");

        return;
    }

    /* Message */
    msg_print("Inscription removed.");


    /* Remove the incription */
    o_ptr->inscription = 0;

    /* Combine the pack */
    p_ptr->notice |= (PN_COMBINE);

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP);

    /* .や$の関係で, 再計算が必要なはず -- henkma */
    p_ptr->update |= (PU_BONUS);

}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
    int            item;

    object_type        *o_ptr;

    char        o_name[MAX_NLEN];

    char        out_val[80];

    cptr q, s;

    item_tester_no_ryoute = TRUE;
    /* Get an item */
    q = "Inscribe which item? ";
    s = "You have nothing to inscribe.";

    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Describe the activity */
    object_desc(o_name, o_ptr, OD_OMIT_INSCRIPTION);

    /* Message */
    msg_format("Inscribing %s.", o_name);

    msg_print(NULL);

    /* Start with nothing */
    strcpy(out_val, "");

    /* Use old inscription */
    if (o_ptr->inscription)
    {
        /* Start with the old inscription */
        strcpy(out_val, quark_str(o_ptr->inscription));
    }

    /* Get a new inscription (possibly empty) */
    if (get_string("Inscription: ", out_val, 80))
    {
        /* Save the inscription */
        o_ptr->inscription = quark_add(out_val);

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* .や$の関係で, 再計算が必要なはず -- henkma */
        p_ptr->update |= (PU_BONUS);
    }
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(object_type *o_ptr)
{
    /* Flasks of oil are okay */
    if (o_ptr->tval == TV_FLASK) return (TRUE);

    /* Laterns are okay */
    if ((o_ptr->tval == TV_LITE) &&
        (o_ptr->sval == SV_LITE_LANTERN)) return (TRUE);

    /* Assume not okay */
    return (FALSE);
}

static bool _lite_is_darkness(object_type *lite)
{
    if (lite->name2 == EGO_LITE_DARKNESS || have_flag(lite->art_flags, TR_DARKNESS))
        return TRUE;
    return FALSE;
}

/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(object_type *lantern)
{
    int item;
    object_type *o_ptr;

    item_tester_hook = item_tester_refill_lantern;
    if (!get_item(&item, "Refill with which flask? ", "You have no flasks of oil.", USE_INVEN | USE_FLOOR)) return;
    if (item >= 0)
        o_ptr = &inventory[item];
    else
        o_ptr = &o_list[0 - item];

    energy_use = 50;
    lantern->xtra4 += o_ptr->xtra4;
    msg_print("You fuel your lamp.");
    if ( _lite_is_darkness(o_ptr) && lantern->xtra4 > 0)
    {
        lantern->xtra4 = 0;
        msg_print("Your lamp has gone out!");
    }
    else if (_lite_is_darkness(o_ptr) || _lite_is_darkness(lantern))
    {
        lantern->xtra4 = 0;
        msg_print("Curiously, your lamp doesn't light.");
    }
    else if (lantern->xtra4 >= FUEL_LAMP)
    {
        lantern->xtra4 = FUEL_LAMP;
        msg_print("Your lamp is full.");
    }

    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    p_ptr->update |= PU_TORCH;
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static bool _is_torch(object_type *o_ptr) {
    return object_is_(o_ptr, TV_LITE, SV_LITE_TORCH);
}
static void do_cmd_refill_torch(object_type *torch)
{
    int item;
    object_type *o_ptr;

    item_tester_hook = _is_torch;
    if (!get_item(&item, "Refuel with which torch? ", "You have no extra torches.", USE_INVEN | USE_FLOOR)) return;
    if (item >= 0)
        o_ptr = &inventory[item];
    else
        o_ptr = &o_list[0 - item];

    energy_use = 50;
    torch->xtra4 += o_ptr->xtra4 + 5;

    msg_print("You combine the torches.");
    if (_lite_is_darkness(o_ptr) && torch->xtra4 > 0)
    {
        torch->xtra4 = 0;
        msg_print("Your torch has gone out!");
    }
    else if (_lite_is_darkness(o_ptr) || _lite_is_darkness(torch))
    {
        torch->xtra4 = 0;
        msg_print("Curiously, your torch does not light.");
    }
    else if (torch->xtra4 >= FUEL_TORCH)
    {
        torch->xtra4 = FUEL_TORCH;
        msg_print("Your torch is fully fueled.");
    }
    else
        msg_print("Your torch glows more brightly.");

    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    p_ptr->update |= PU_TORCH;
}


/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
    int slot = equip_find_object(TV_LITE, SV_ANY);

    if (slot)
    {
        object_type *o_ptr = equip_obj(slot);

        if (p_ptr->special_defense & KATA_MUSOU)
            set_action(ACTION_NONE);

        switch (o_ptr->sval)
        {
        case SV_LITE_LANTERN:
            do_cmd_refill_lamp(o_ptr);
            break;
        case SV_LITE_TORCH:
            do_cmd_refill_torch(o_ptr);
            break;
        default:
            msg_print("Your light cannot be refilled.");
        }
    }
    else
        msg_print("You are not wielding a light.");
}

/*
 * Target command
 */
void do_cmd_target(void)
{
    /* Target set */
    if (target_set(TARGET_KILL))
    {
        msg_print("Target Selected.");

    }

    /* Target aborted */
    else
    {
        msg_print("Target Aborted.");

    }
}



/*
 * Look command
 */
void do_cmd_look(void)
{
    /* Look around */
    if (target_set(TARGET_LOOK))
    {
        msg_print("Target Selected.");

    }
}



/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
    int        dir, y1, x1, y2, x2;

    char    tmp_val[80];

    char    out_val[160];

    int wid, hgt;

    /* Get size */
    get_screen_size(&wid, &hgt);


    /* Start at current panel */
    y2 = y1 = panel_row_min;
    x2 = x1 = panel_col_min;

    /* Show panels until done */
    while (1)
    {
        /* Describe the location */
        if ((y2 == y1) && (x2 == x1))
        {
            tmp_val[0] = '\0';

        }
        else
        {
            sprintf(tmp_val, "%s%s of",
                ((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
                ((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));

        }

        /* Prepare to ask which way to look */
        sprintf(out_val,
            "Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",

            y2 / (hgt / 2), y2 % (hgt / 2),
            x2 / (wid / 2), x2 % (wid / 2), tmp_val);

        /* Assume no direction */
        dir = 0;

        /* Get a direction */
        while (!dir)
        {
            char command;

            /* Get a command (or Cancel) */
            if (!get_com(out_val, &command, TRUE)) break;
            if (command == '5') break;

            /* Extract the action (if any) */
            dir = get_keymap_dir(command);

            /* Error */
            if (!dir) bell();
        }

        /* No direction */
        if (!dir) break;

        /* Apply the motion */
        if (change_panel(ddy[dir], ddx[dir]))
        {
            y2 = panel_row_min;
            x2 = panel_col_min;
        }
    }


    /* Recenter the map around the player */
    verify_panel();

    /* Update stuff */
    p_ptr->update |= (PU_MONSTERS);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    /* Window stuff */
    p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

    /* Handle stuff */
    handle_stuff();
}



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
    u16b *who = (u16b*)(u);

    u16b *why = (u16b*)(v);

    int w1 = who[a];
    int w2 = who[b];

    int z1, z2;

    /* Sort by player kills */
    if (*why >= 4)
    {
        /* Extract player kills */
        z1 = r_info[w1].r_pkills;
        z2 = r_info[w2].r_pkills;

        /* Compare player kills */
        if (z1 < z2) return (TRUE);
        if (z1 > z2) return (FALSE);
    }


    /* Sort by total kills */
    if (*why >= 3)
    {
        /* Extract total kills */
        z1 = r_info[w1].r_tkills;
        z2 = r_info[w2].r_tkills;

        /* Compare total kills */
        if (z1 < z2) return (TRUE);
        if (z1 > z2) return (FALSE);
    }


    /* Sort by monster level */
    if (*why >= 2)
    {
        /* Extract levels */
        z1 = r_info[w1].level;
        z2 = r_info[w2].level;

        /* Compare levels */
        if (z1 < z2) return (TRUE);
        if (z1 > z2) return (FALSE);
    }


    /* Sort by monster experience */
    if (*why >= 1)
    {
        /* Extract experience */
        z1 = r_info[w1].mexp;
        z2 = r_info[w2].mexp;

        /* Compare experience */
        if (z1 < z2) return (TRUE);
        if (z1 > z2) return (FALSE);
    }


    /* Compare indexes */
    return (w1 <= w2);
}


/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
    u16b *who = (u16b*)(u);

    u16b holder;

    /* Unused */
    (void)v;

    /* Swap */
    holder = who[a];
    who[a] = who[b];
    who[b] = holder;
}



/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "multiple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored. XXX XXX XXX
 */
void do_cmd_query_symbol(void)
{
    int        i, n, r_idx;
    char    sym, query;
    char    buf[128];

    bool    all = FALSE;
    bool    uniq = FALSE;
    bool    norm = FALSE;
    bool    ride = FALSE;
    char    temp[80] = "";

    bool    recall = FALSE;

    u16b    why = 0;
    u16b    *who;

    /* Get a character, or abort */
    if (!get_com("Enter character to be identified(^A:All,^U:Uniqs,^N:Non uniqs,^M:Name): ", &sym, FALSE)) return;

    /* Find that character info, and describe it */
    for (i = 0; ident_info[i]; ++i)
    {
        if (sym == ident_info[i][0]) break;
    }

    /* Describe */
    if (sym == KTRL('A'))
    {
        all = TRUE;
        strcpy(buf, "Full monster list.");
    }
    else if (sym == KTRL('U'))
    {
        all = uniq = TRUE;
        strcpy(buf, "Unique monster list.");
    }
    else if (sym == KTRL('N'))
    {
        all = norm = TRUE;
        strcpy(buf, "Non-unique monster list.");
    }
    else if (sym == KTRL('R'))
    {
        all = ride = TRUE;
        strcpy(buf, "Ridable monster list.");
    }
    /* XTRA HACK WHATSEARCH */
    else if (sym == KTRL('M'))
    {
        all = TRUE;
        if (!get_string("Enter name:",temp, 70))
        {
            temp[0]=0;
            return;
        }
        sprintf(buf, "Monsters with a name \"%s\"",temp);
    }
    else if (ident_info[i])
    {
        sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
    }
    else
    {
        sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
    }

    /* Display the result */
    prt(buf, 0, 0);

    /* Allocate the "who" array */
    C_MAKE(who, max_r_idx, u16b);

    /* Collect matching monsters */
    for (n = 0, i = 1; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Nothing to recall */
        if (!(cheat_know || p_ptr->wizard) && !r_ptr->r_sights) continue;

        /* Require non-unique monsters if needed */
        if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

        /* Require unique monsters if needed */
        if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

        /* Require ridable monsters if needed */
        if (ride && !(r_ptr->flags7 & (RF7_RIDING))) continue;

        /* XTRA HACK WHATSEARCH */
        if (temp[0])
        {
          int xx;
          char temp2[80];
  
          for (xx=0; temp[xx] && xx<80; xx++)
          {
            if (isupper(temp[xx])) temp[xx]=tolower(temp[xx]);
          }
  
          strcpy(temp2, r_name+r_ptr->name);
          for (xx=0; temp2[xx] && xx<80; xx++)
            if (isupper(temp2[xx])) temp2[xx]=tolower(temp2[xx]);
  
          if (my_strstr(temp2, temp))
              who[n++]=i;
        }

        /* Collect "appropriate" monsters */
        else if (all || (r_ptr->d_char == sym)) who[n++] = i;
    }

    /* Nothing to recall */
    if (!n)
    {
        /* Free the "who" array */
        C_KILL(who, max_r_idx, u16b);

        return;
    }


    /* Prompt XXX XXX XXX */
    put_str("Recall details? (k/y/n): ", 0, 40);


    /* Query */
    query = inkey();

    /* Restore */
    prt(buf, 0, 0);

    why = 2;

    /* Select the sort method */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    /* Sort the array */
    ang_sort(who, &why, n);

    /* Sort by kills (and level) */
    if (query == 'k')
    {
        why = 4;
        query = 'y';
    }

    /* Catch "escape" */
    if (query != 'y')
    {
        /* Free the "who" array */
        C_KILL(who, max_r_idx, u16b);

        return;
    }

    /* Sort if needed */
    if (why == 4)
    {
        /* Select the sort method */
        ang_sort_comp = ang_sort_comp_hook;
        ang_sort_swap = ang_sort_swap_hook;

        /* Sort the array */
        ang_sort(who, &why, n);
    }


    /* Start at the end */
    i = n - 1;

    /* Scan the monster memory */
    while (1)
    {
        /* Extract a race */
        r_idx = who[i];

        /* Hack -- Auto-recall */
        monster_race_track(r_idx);

        /* Hack -- Handle stuff */
        handle_stuff();

        /* Interact */
        while (1)
        {
            /* Recall */
            if (recall)
            {
                /* Save the screen */
                screen_save();

                /* Recall on screen */
                screen_roff(who[i], 0);
            }

            /* Hack -- Begin the prompt */
            roff_top(r_idx);

            /* Hack -- Complete the prompt */
            Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");

            /* Command */
            query = inkey();

            /* Unrecall */
            if (recall)
            {
                /* Restore */
                screen_load();
            }

            /* Normal commands */
            if (query != 'r') break;

            /* Toggle recall */
            recall = !recall;
        }

        /* Stop scanning */
        if (query == ESCAPE) break;

        /* Move to "prev" monster */
        if (query == '-')
        {
            if (++i == n)
            {
                i = 0;
                if (!expand_list) break;
            }
        }

        /* Move to "next" monster */
        else
        {
            if (i-- == 0)
            {
                i = n - 1;
                if (!expand_list) break;
            }
        }
    }

    /* Free the "who" array */
    C_KILL(who, max_r_idx, u16b);

    /* Re-display the identity */
    prt(buf, 0, 0);
}

struct _mon_list_info_s 
{
    int r_idx;
    int ct_total;
    int ct_awake;
    int ct_los;
};

typedef struct _mon_list_info_s _mon_list_info_t;
typedef _mon_list_info_t *_mon_list_info_ptr;

static bool _compare_r_level(vptr u, vptr v, int a, int b)
{
    int *order = (int*)u;
    int left = order[a];
    int right = order[b];
    return r_info[left].level >= r_info[right].level;
}

static void _swap_int(vptr u, vptr v, int a, int b)
{
    int *order = (int*)u;
    int tmp = order[a];
    order[a] = order[b];
    order[b] = tmp;
}

/* Idea borrowed from Vanilla 3.5, but recoded from scratch ... */
void do_cmd_list_monsters(void)
{
    int         i, ct_types, ct_total = 0;
    int_map_ptr info = int_map_alloc(free);
    
    /* Collect */
    for (i = 0; i < max_m_idx; i++)
    {
        const monster_type *m_ptr = &m_list[i];
        _mon_list_info_ptr  info_ptr;
        
        if (!m_ptr->ap_r_idx) continue;
        if (!m_ptr->ml) continue;

        info_ptr = int_map_find(info, m_ptr->ap_r_idx);
        if (!info_ptr)
        {
            info_ptr = malloc(sizeof(_mon_list_info_t));
            info_ptr->r_idx = m_ptr->ap_r_idx;
            info_ptr->ct_total = 0;
            info_ptr->ct_awake = 0;
            info_ptr->ct_los = 0;
            
            int_map_add(info, m_ptr->ap_r_idx, info_ptr);
        }

        assert(info_ptr);
        info_ptr->ct_total++;
        ct_total++;

        if (!MON_CSLEEP(m_ptr)) info_ptr->ct_awake++;
        if (projectable(py, px, m_ptr->fy, m_ptr->fx)) info_ptr->ct_los++;
    }
    
    ct_types = int_map_count(info);
    if (ct_types)
    {
        int_map_iter_ptr  iter;
        int              *order;
        int               cx, cy, row = 1, col;

        /* Sort */
        order = C_MAKE(order, ct_types, int);

        i = 0;
        for (iter = int_map_iter_alloc(info); 
                int_map_iter_is_valid(iter); 
                int_map_iter_next(iter))
        {
            _mon_list_info_ptr info_ptr = int_map_iter_current(iter);
            order[i++] = info_ptr->r_idx;
        }
        int_map_iter_free(iter);

        ang_sort_comp = _compare_r_level;
        ang_sort_swap = _swap_int;
        ang_sort(order, NULL, ct_types);

        /* Display */
        Term_get_size(&cx, &cy);
        col = cx - 52;
        screen_save();
        c_prt(TERM_WHITE, format("You see %d monster%s", ct_total, ct_total != 1 ? "s" : ""), 0, col);
        for (i = 0; i < ct_types; i++)
        {
            int                 r_idx = order[i];
            const monster_race *r_ptr = &r_info[r_idx];
            byte                attr = TERM_WHITE;
            _mon_list_info_ptr  info_ptr = int_map_find(info, r_idx);
            char                buf[100];

            assert(info_ptr);

            Term_erase(col - 1, row, 53);

            if (row >= cy - 2) 
            {
                c_prt(TERM_YELLOW, "...", row++, col+2);
                break;
            }

            if (r_ptr->flags1 & RF1_UNIQUE)
                attr = TERM_VIOLET;
            else if (r_ptr->level > base_level)
                attr = TERM_RED;                
            else if (!info_ptr->ct_awake)
                attr = TERM_L_UMBER;

            if (info_ptr->ct_total == 1)
                sprintf(buf, "%s", r_name + r_ptr->name);
            else if (!info_ptr->ct_awake)
                sprintf(buf, "%s (%d sleeping)", r_name + r_ptr->name, info_ptr->ct_total);
            else if (info_ptr->ct_awake == info_ptr->ct_total)
                sprintf(buf, "%s (%d awake)", r_name + r_ptr->name, info_ptr->ct_total);
            else
                sprintf(buf, "%s (%d awake, %d sleeping)", r_name + r_ptr->name, 
                    info_ptr->ct_awake, info_ptr->ct_total - info_ptr->ct_awake);

            Term_queue_bigchar(col, row, r_ptr->x_attr, r_ptr->x_char, 0, 0);
            c_put_str(attr, format(" %-50.50s", buf), row++, col+1);
        }
        Term_erase(col - 1, row, 53);
        c_prt(TERM_YELLOW, "Hit any key.", row, col+2);
        inkey();
        prt("", 0, 0);

        screen_load();

        C_KILL(order, ct_types, int);
    }
    else
        msg_print("You see no visible monsters.");

    int_map_free(info);
}

static bool _compare_obj_list_info(vptr u, vptr v, int a, int b)
{
    int         *vec = (int*)u;
    int          left_idx = vec[a];
    int          right_idx = vec[b];
    object_type *left_obj = &o_list[left_idx];
    object_type *right_obj = &o_list[right_idx];

    /* Hack -- readable books always come first */
    if (left_obj->tval == REALM1_BOOK && right_obj->tval != REALM1_BOOK) return TRUE;
    if (right_obj->tval == REALM1_BOOK && left_obj->tval != REALM1_BOOK) return FALSE;

    if (left_obj->tval == REALM2_BOOK && right_obj->tval != REALM2_BOOK) return TRUE;
    if (right_obj->tval == REALM2_BOOK && left_obj->tval != REALM2_BOOK) return FALSE;

    /* Objects sort by decreasing type */
    if (left_obj->tval > right_obj->tval) return TRUE;
    if (left_obj->tval < right_obj->tval) return FALSE;

    /* Objects sort by increasing sval */
    if (left_obj->sval < right_obj->sval) return TRUE;
    if (left_obj->sval > right_obj->sval) return FALSE;

    return TRUE;
}

#define _MAX_OBJ_LIST 100

void do_cmd_list_objects(void)
{
    int list[_MAX_OBJ_LIST];
    int i, ct = 0;
    int cx, cy, row = 1, col;

    for (i = 0; i < max_o_idx; i++)
    {
        object_type *o_ptr = &o_list[i];
        int          auto_pick_idx;

        if (!o_ptr->k_idx) continue;
        if (!(o_ptr->marked & OM_FOUND)) continue;
        if (o_ptr->tval == TV_GOLD) continue;
        if (ct >= _MAX_OBJ_LIST) break;

        auto_pick_idx = is_autopick(o_ptr);

        if (!p_ptr->wizard)
        {
            if (auto_pick_idx < 0) continue;
            if (!(autopick_list[auto_pick_idx].action & DO_DISPLAY)) continue;
            if (!(autopick_list[auto_pick_idx].action & (DO_AUTOPICK | DO_QUERY_AUTOPICK))) continue;
        }
        list[ct++] = i;
    }

    if (!ct)
    {
        msg_print("No objects match your pickup preferences.");
        return;
    }

    ang_sort_comp = _compare_obj_list_info;
    ang_sort_swap = _swap_int;
    ang_sort(list, NULL, ct);

    Term_get_size(&cx, &cy);
    col = cx - 52;
    screen_save();
    c_prt(TERM_WHITE, format("%d object%s match your pickup preferences", ct, ct != 1 ? "s" : ""), 0, col);
    for (i = 0; i < ct; i++)
    {
        char         o_name[MAX_NLEN];
        object_type *o_ptr = &o_list[list[i]];
        byte         a = object_attr(o_ptr);
        char         c = object_char(o_ptr);
        byte         attr = TERM_WHITE;

        Term_erase(col - 1, row, 53);

        if (row >= cy - 2) 
        {
            c_prt(TERM_YELLOW, "...", row++, col+2);
            break;
        }

        object_desc(o_name, o_ptr, 0);
        attr = tval_to_attr[o_ptr->tval % 128];

        Term_queue_bigchar(col, row, a, c, 0, 0);
        c_put_str(attr, format(" %-50.50s", o_name), row++, col+1);
    }
    Term_erase(col - 1, row, 53);
    c_prt(TERM_YELLOW, "Hit any key.", row, col+2);
    inkey();
    prt("", 0, 0);

    screen_load();
}
