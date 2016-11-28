/* File: cmd3.c */

/* Purpose: Inventory commands */

/* Inventory and equipment display and management interface, observing an
 * object, inscribing, refuelling, (l)ooking around the screen and
 * (L)ooking around the dungeon, help info on textual chars ("8" is the home,
 * etc.), monster memory interface, stealing.
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* Hack -- possible victim outcry. -LM- */
static cptr desc_victim_outcry[] =
{
        "'My money, where's my money?'",
        "'Thief! Thief! Thief! Baggins! We hates it forever!'",
        "'Tell me, have you seen a purse wandering around?'",
        "'Thieves, Fire, Murder!'",
        "''Ere, 'oo are you?'",
        "'Hey, look what I've copped!'",
        "'How dare you!'",
        "'Help yourself again, thief, there is plenty and to spare!'",
        "'All the world detests a thief.'",
        "'Catch me this thief!'",
        "'Hi! Ho! Robbery!'",
        "'My gold, my precious gold!'",
        "'My gold is costly, thief!'",
        "'Your blood for my gold?  Agreed!'",
        "'I scrimp, I save, and now it's gone!'",
        "'Robbers like you are part of the problem!'",
        "'Banditti!  This dungeon's just not safe anymore!'",
        "'Ruined!  I'm ruined!'",
        "'Where, where is the common decency?'",
        "'Your knavish tricks end here and now!'",
};


/*
 * Display inventory
 */
void do_cmd_inven(void)
{

        char out_val[160];
        int wgt;
        char unit[10];

        /* Note that we are in "inventory" mode */
        command_wrk = FALSE;

#ifdef ALLOW_EASY_FLOOR

        /* Note that we are in "inventory" mode */
        if (easy_floor) command_wrk = (USE_INVEN);

#endif /* ALLOW_EASY_FLOOR */

        /* Save the screen */
        screen_save();

        /* Hack -- show empty slots */
        item_tester_full = TRUE;

        /* Display the inventory */
        show_inven();

        /* Hack -- hide empty slots */
        item_tester_full = FALSE;

        wgt = total_weight * ((metric) ? 0.5 : 1);
        strcpy (unit, ((metric) ? "kilos" : "pounds"));

    sprintf(out_val, "Inventory: carrying %d.%d %s (%d%% of capacity). Command: ",
           wgt / 10, wgt % 10, unit,
       (total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2));

        /* Get a command */
        prt(out_val, 0, 0);

        /* Get a new command */
        command_new = inkey();

        /* Restore the screen */
        screen_load();


        /* Process "Escape" */
        if (command_new == ESCAPE)
        {
                /* Reset stuff */
                command_new = 0;
                command_gap = 50;
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
        char unit[10];
        int wgt;

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
        show_equip();

        /* Hack -- undo the hack above */
        item_tester_full = FALSE;

        wgt = total_weight * ((metric) ? 0.5 : 1);
        strcpy (unit, ((metric) ? "kilos" : "pounds"));

        /* Build a prompt */
   sprintf(out_val, "Equipment: carrying %d.%d %s (%d%% of capacity). Command: ",
           wgt / 10, wgt % 10, unit,
       (total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2));

        /* Get a command */
        prt(out_val, 0, 0);

        /* Get a new command */
        command_new = inkey();

        /* Restore the screen */
        screen_load();


        /* Process "Escape" */
        if (command_new == ESCAPE)
        {
                /* Reset stuff */
                command_new = 0;
                command_gap = 50;
        }

        /* Process normal keys */
        else
        {
                /* Enter "display" mode */
                command_see = TRUE;
        }
}


/*
 * The "wearable" tester
 */
static bool item_tester_hook_wear(object_type *o_ptr)
{
        /* Check for a usable slot */
        if (wield_slot(o_ptr) >= INVEN_WIELD) return (TRUE);

        /* Assume not wearable */
        return (FALSE);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
        int i, item, slot, num = 1;

        object_type forge;
        object_type *q_ptr;

        object_type *o_ptr;

        object_type *i_ptr;

        cptr act;

        char o_name[80];

        cptr q, s;

        u64b f1, f2, f3;


        /* Restrict the choices */
        item_tester_hook = item_tester_hook_wear;

        /* Get an item */
        q = "Wear/Wield which item? ";
        s = "You have nothing you can wear or wield.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


        /* Check the slot */
        slot = wield_slot(o_ptr);

        if (p_ptr->polymon)
        {
           monster_race *r_ptr = &r_info[p_ptr->polymon];

           if (((slot == INVEN_ARM) || (slot == INVEN_WIELD) || (slot ==
                 INVEN_BOW) || (slot == INVEN_HANDS)) && !(r_ptr->r_polyinfo
                 & HAS_HANDS))
              {
                 msg_format ("You cannot %s this without hands.", (slot ==
                   INVEN_HANDS) ? "wear" : "wield");
                 return;
              }

           if (((slot == INVEN_BODY) || (slot == INVEN_OUTER))
                 && !(r_ptr->r_polyinfo & HAS_TORSO))
              {
                 msg_format ("You cannot wear this without a torso.");
                 return;
              }

           if ((slot == INVEN_FEET)
                 && !(r_ptr->r_polyinfo & HAS_FEET))
              {
                 msg_format ("You cannot wear this without having feet.");
                 return;
              }

           if (((slot == INVEN_LEFT) || (slot == INVEN_RIGHT))
                 && !(r_ptr->r_polyinfo & HAS_FINGERS))
              {
                 msg_format ("You cannot wear this without having fingers.");
                 return;
              }

           if (((slot == INVEN_HEAD) || (slot == INVEN_NECK))
                 && !(r_ptr->r_polyinfo & HAS_HEAD))
              {
                 msg_format ("You cannot wear this without a head.");
                 return;
              }
        }

        /* Prevent wielding into a cursed slot */
        if (cursed_p(&inventory[slot]))
        {
                /* Describe it */
                object_desc(o_name, &inventory[slot], FALSE, 0);

                /* Message */
                msg_format("The %s you are %s appears to be cursed.",
                           o_name, describe_use(slot));

                o_ptr->note = quark_add("cursed");

                o_ptr->ident |= (IDENT_SENSE);

                sc_time += 3;

                /* Cancel the command */
                return;
        }

    if ((cursed_p(o_ptr)) && (wear_confirm) &&
#ifdef ALLOW_EASY_SENSE /* TNB */
                (object_known_p(o_ptr) || sensed_p(o_ptr)))
#else /* ALLOW_EASY_SENSE -- TNB */
                (object_known_p(o_ptr) || (o_ptr->ident & IDENT_SENSE)))
#endif /* ALLOW_EASY_SENSE -- TNB */
    {
        char dummy[512];

                /* Describe it */
        object_desc(o_name, o_ptr, FALSE, 0);

        sprintf(dummy, "Really use the %s {cursed}? ", o_name);
        if (!(get_check(dummy)))
            return;
    }

        /* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3);

        /* Two handed weapons can't be wielded with a shield */

        if ((inventory[INVEN_ARM].k_idx != 0) && (f1 & TR1_MUST2H))
        {
           object_desc(o_name, o_ptr, FALSE, 0);
           msg_format("You cannot wield your %s with a shield.", o_name);
           return;
        }

        i_ptr = &inventory[INVEN_WIELD];

        /* Extract the flags */
        object_flags(i_ptr, &f1, &f2, &f3);

        /* Prevent shield from being put on if wielding 2H */

        if ((slot == INVEN_ARM) && (f1 & TR1_MUST2H))
        {
           object_desc(o_name, o_ptr, FALSE, 0);
           msg_format("You cannot wield your %s with a two-handed weapon.", o_name);
/*         sc_time += 2;  ? */
           return;
        }

        if ((slot == INVEN_ARM) && (f1 & TR1_COULD2H))
        {
           if (!get_check("Are you sure you want to restrict your fighting? "))
            return;
        }


        /* Check if completed a quest */
        for (i = 0; i < max_quests; i++)
        {
                if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) &&
                   (quest[i].status == QUEST_STATUS_TAKEN) &&
                        (quest[i].k_idx == o_ptr->name1) &&
                        (o_ptr->name1 > 0))
                {
                        quest[i].status = QUEST_STATUS_COMPLETED;
                        msg_print("You completed your quest!");
                        msg_print(NULL);
                }
        }

        /* Take a turn */
        energy_use = 100;

        /* Get local object */
        q_ptr = &forge;

        /* Obtain local object */
        object_copy(q_ptr, o_ptr);

        if (slot == INVEN_AMMO) num = o_ptr->number;

        /* Modify quantity */
        q_ptr->number = num;

        /* Decrease the item (from the pack) */
        if (item >= 0)
        {
                inven_item_increase(item, -num);
                inven_item_optimize(item);
        }

        /* Decrease the item (from the floor) */
        else
        {
                floor_item_increase(0 - item, -num);
                floor_item_optimize(0 - item);
        }

        /* Access the wield slot */
        o_ptr = &inventory[slot];

        /* Take off existing item */
        if (slot != INVEN_AMMO)
        {
                if (o_ptr->k_idx)
                {
                        /* Take off existing item */
                        (void)inven_takeoff(slot, 255);
                }
        }
        else
        {
                if (o_ptr->k_idx)
                {
                        if (!object_similar(o_ptr, q_ptr))
                        {
                                /* Take off existing item */
                                (void)inven_takeoff(slot, 255);
                        }
                        else
                        {
                                q_ptr->number += o_ptr->number;
                        }
                }
        }

        /* Wear the new stuff */
        object_copy(o_ptr, q_ptr);

        /* Increase the weight */
        total_weight += q_ptr->weight;

        /* Increment the equip counter by hand */
        equip_cnt++;

        /* Where is the item now */
        if (slot == INVEN_WIELD)
        {
                act = "You are wielding";
        }
        else if (slot == INVEN_BOW)
        {
                act = "You are shooting with";
        }
        else if (slot == INVEN_LITE)
        {
                act = "Your light source is";
        }
        else if (slot == INVEN_AMMO)
        {
                act = "In your quiver you have";
        }
        else
        {
                act = "You are wearing";
        }

        /* Describe the result */
        object_desc(o_name, o_ptr, TRUE, 3);

        /* Message */
        msg_format("%s %s (%c).", act, o_name, index_to_label(slot));

        sc_time += randint(20);

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Recalculate torch */
        p_ptr->update |= (PU_TORCH);

        /* Recalculate hitpoint */
        p_ptr->update |= (PU_HP);

        /* Recalculate mana */
        p_ptr->update |= (PU_MANA);

        p_ptr->redraw |= (PR_EQUIPPY);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}



/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
        int item, slot;

        object_type *o_ptr;

        cptr q, s;

        char o_name[80];

        /* Get an item */
        q = "Take off which item? ";
        s = "You are not wearing anything to take off.";
        if (!get_item(&item, q, s, (USE_EQUIP))) return;

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

        /* Check the slot */
        slot = wield_slot(o_ptr);

        /* Item is cursed */
        if (cursed_p(o_ptr))
        {
                /* Describe it */
                object_desc(o_name, &inventory[slot], TRUE, 3);

                /* Oops */
                msg_format("You cannot bring yourself to remove %s.", o_name);

                sc_time += 3;

#if 1 /* TNB */

#ifdef ALLOW_EASY_SENSE

                /* The object was sensed */
                if ((o_ptr->ident & (IDENT_SENSE)) && o_ptr->note)
                {
                        /* Get the inscription */
                        s = quark_str(o_ptr->note);

                        /* Look for the automatic inscription */
                        if (streq(s, "?"))
                        {
                                /* Forget the insription */
                                o_ptr->note = 0;
                        }
                }

#endif /* ALLOW_EASY_SENSE */

                /* Note the curse */
                o_ptr->ident |= (IDENT_SENSE);

#endif /* TNB */

                /* Nope */
                return;
        }


        /* Take a partial turn */
        energy_use = 50;

        /* Take off the item */
        (void)inven_takeoff(item, 255);

        /* Recalculate hitpoint */
        p_ptr->update |= (PU_HP);

    p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
        int item, amt = 1;

        object_type *o_ptr;

        cptr q, s;

        /* Get an item */
        q = "Drop which item? ";
        s = "You have nothing to drop.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

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


        /* Hack -- Cannot remove cursed items */
        if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
        {
                /* Oops */
                msg_print("Hmmm, it seems to be cursed.");

#if 1 /* TNB */

#ifdef ALLOW_EASY_SENSE

                /* The object was sensed */
                if ((o_ptr->ident & (IDENT_SENSE)) && o_ptr->note)
                {
                        /* Get the inscription */
                        s = quark_str(o_ptr->note);

                        /* Look for the automatic inscription */
                        if (streq(s, "?"))
                        {
                                /* Forget the insription */
                                o_ptr->note = 0;
                        }
                }

#endif /* ALLOW_EASY_SENSE */

                /* Note the curse */
                o_ptr->ident |= (IDENT_SENSE);

#endif /* TNB */

                /* Nope */
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

    p_ptr->redraw |= (PR_EQUIPPY);
}


static bool high_level_book(object_type * o_ptr)
{
    if ((o_ptr->tval == TV_LIFE_BOOK) || (o_ptr->tval == TV_AIR_BOOK) ||
        (o_ptr->tval == TV_EARTH_BOOK) || (o_ptr->tval == TV_FIRE_BOOK) ||
        (o_ptr->tval == TV_DEATH_BOOK) || (o_ptr->tval == TV_WATER_BOOK))
        {
            if (o_ptr->sval>1) return TRUE;
            else return FALSE;
        }
        return FALSE;
}


/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
        int                     item, amt = 1;
        int                     old_number;

        bool            force = FALSE;

        object_type             *o_ptr;

        char            o_name[80];

        char            out_val[160];

        cptr q, s;

        /* Hack -- force destruction */
        if (command_arg > 0) force = TRUE;


        /* Get an item */
        q = "Destroy which item? ";
        s = "You have nothing to destroy.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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
        object_desc(o_name, o_ptr, TRUE, 3);
        o_ptr->number = old_number;

        /* Verify unless quantity given */
        if (!force)
        {
                if (!((auto_destroy) && (object_value(o_ptr)<1)))
                {
                        /* Make a verification */
                        sprintf(out_val, "Really destroy %s? ", o_name);
                        if (!get_check(out_val)) return;
                }
        }

        /* Take a turn */
        energy_use = 100;

        /* Artifacts cannot be destroyed */
        if (artifact_p(o_ptr) || o_ptr->art_name)
        {
                cptr feel = "special";

                energy_use = 0;

                /* Message */
                msg_format("You cannot destroy %s.", o_name);

                /* Hack -- Handle icky artifacts */
                if (cursed_p(o_ptr) || broken_p(o_ptr)) feel = "terrible";

                /* Hack -- inscribe the artifact */
                o_ptr->note = quark_add(feel);

                /* We have "felt" it (again) */
                o_ptr->ident |= (IDENT_SENSE);

                /* Combine the pack */
                p_ptr->notice |= (PN_COMBINE);

      p_ptr->redraw |= (PR_EQUIPPY);

                /* Window stuff */
                p_ptr->window |= (PW_INVEN | PW_EQUIP);

                /* Done */
                return;
        }

        /* Message */
        msg_format("You destroy %s.", o_name);
        sc_time += (randint(20) * (o_ptr->weight/100) + 1);
        sound(SOUND_DESTITEM);

    if (high_level_book(o_ptr))
    {
        bool gain_expr = FALSE;

        if (p_ptr->pclass == CLASS_WARRIOR)
                {
                        gain_expr = TRUE;
                }
        else if (p_ptr->pclass == CLASS_PALADIN)
        {
            if (p_ptr->realm1 == 1)
            {
                if (o_ptr->tval != TV_LIFE_BOOK) gain_expr = TRUE;
            }
            else
            {
                if (o_ptr->tval == TV_LIFE_BOOK) gain_expr = TRUE;
            }
        }

        if ((gain_expr) && (p_ptr->exp < PY_MAX_EXP))

           {
            s32b tester_exp = p_ptr->max_exp / 20;
            if (tester_exp > 10000) tester_exp = 10000;
            if (o_ptr->sval < 3) tester_exp /= 4;
            if (tester_exp<1) tester_exp = 1;

            msg_print("You feel more experienced.");
            gain_exp(tester_exp * amt);
           }
        }

        /*
         * Hack -- If rods or wand are destroyed, the total maximum timeout or
         * charges of the stack needs to be reduced, unless all the items are
         * being destroyed. -LM-
         */
        if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)) &&
                (amt < o_ptr->number))
        {
                o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
        }

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
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
        int                     item;

        object_type             *o_ptr;

        char            o_name[80];

        cptr q, s;

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

        /* Description */
        object_desc(o_name, o_ptr, TRUE, 3);

        /* Describe */
        msg_format("Examining %s...", o_name);

        /* Require full knowledge */
        if (o_ptr->ident & (IDENT_MENTAL))
        {
            if (!identify_fully_aux(o_ptr)) msg_print("You see nothing special.");
        }
        else
        {
            cptr foo = damage_status(o_ptr);
            if (foo)
            {
              msg_format("It looks %s.", foo);
            }
        }

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
        if (!o_ptr->note)
        {
                msg_print("That item had no inscription to remove.");
                return;
        }

        /* Message */
        msg_print("Inscription removed.");

        /* Remove the incription */
        o_ptr->note = 0;

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
        int                     item;

        object_type             *o_ptr;

        char            o_name[80];

        char            out_val[80];

        cptr q, s;

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
        object_desc(o_name, o_ptr, TRUE, 3);

        /* Message */
        msg_format("Inscribing %s.", o_name);
        msg_print(NULL);

        /* Start with nothing */
        strcpy(out_val, "");

        /* Use old inscription */
        if (o_ptr->note)
        {
                /* Start with the old inscription */
                strcpy(out_val, quark_str(o_ptr->note));
        }

        /* Get a new inscription (possibly empty) */
        if (get_string("Inscription: ", out_val, 80))
        {
                /* Save the inscription */
                o_ptr->note = quark_add(out_val);

                /* Combine the pack */
                p_ptr->notice |= (PN_COMBINE);

                /* Window stuff */
                p_ptr->window |= (PW_INVEN | PW_EQUIP);
        }
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(object_type *o_ptr)
{
        /* Flasks of oil are okay */
        if (o_ptr->tval == TV_FLASK) return (TRUE);

        /* Lanterns are okay */
        if ((o_ptr->tval == TV_LITE) &&
            (o_ptr->sval == SV_LITE_LANTERN)) return (TRUE);

        /* Assume not okay */
        return (FALSE);
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(void)
{
        int item;

        object_type *o_ptr;
        object_type *j_ptr;

        cptr q, s;


        /* Restrict the choices */
        item_tester_hook = item_tester_refill_lantern;

        /* Get an item */
        q = "Refill with which flask? ";
        s = "You have no flasks of oil.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


        /* Take a partial turn */
        energy_use = 50;

        /* Access the lantern */
        j_ptr = &inventory[INVEN_LITE];

        /* Refuel */
        j_ptr->pval += o_ptr->pval;

        sc_time += 10;

        /* Message */
        msg_print("You fuel your lamp.");

        /* Comment */
        if (j_ptr->pval >= FUEL_LAMP)
        {
                j_ptr->pval = FUEL_LAMP;
                msg_print("Your lamp is full.");
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
}


/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(object_type *o_ptr)
{
        /* Torches are okay */
        if ((o_ptr->tval == TV_LITE) &&
            (o_ptr->sval == SV_LITE_TORCH)) return (TRUE);

        /* Assume not okay */
        return (FALSE);
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(void)
{
        int item;

        object_type *o_ptr;
        object_type *j_ptr;

        cptr q, s;


        /* Restrict the choices */
        item_tester_hook = item_tester_refill_torch;

        /* Get an item */
        q = "Refuel with which torch? ";
        s = "You have no extra torches.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


        /* Take a partial turn */
        energy_use = 50;

        /* Access the primary torch */
        j_ptr = &inventory[INVEN_LITE];

        /* Refuel */
        j_ptr->pval += o_ptr->pval + 5;

        sc_time += 3;

        /* Message */
        msg_print("You combine the torches.");

        /* Over-fuel message */
        if (j_ptr->pval >= FUEL_TORCH)
        {
                j_ptr->pval = FUEL_TORCH;
                msg_print("Your torch is fully fueled.");
        }

        /* Refuel message */
        else
        {
                msg_print("Your torch glows more brightly.");
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
}


/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
        object_type *o_ptr;

        /* Get the light */
        o_ptr = &inventory[INVEN_LITE];

        /* It is nothing */
        if (o_ptr->tval != TV_LITE)
        {
                msg_print("You are not wielding a light.");
        }

        /* It's a lamp */
        else if (o_ptr->sval == SV_LITE_LANTERN)
        {
                do_cmd_refill_lamp();
        }

        /* It's a torch */
        else if (o_ptr->sval == SV_LITE_TORCH)
        {
                do_cmd_refill_torch();
        }

        /* No torch to refill */
        else
        {
                msg_print("Your light cannot be refilled.");
        }
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
        int             dir, y1, x1, y2, x2;

        char    tmp_val[80];

        char    out_val[160];


        /* Start at current panel */
        y2 = y1 = panel_row;
        x2 = x1 = panel_col;

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
                        "Map sector [%d,%d], which is%s your sector.  Direction?",
                        y2, x2, tmp_val);

                /* Assume no direction */
                dir = 0;

                /* Get a direction */
                while (!dir)
                {
                        char command;

                        /* Get a command (or Cancel) */
                        if (!get_com(out_val, &command)) break;

                        /* Extract the action (if any) */
                        dir = get_keymap_dir(command);

                        /* Error */
                        if (!dir) bell();
                }

                /* No direction */
                if (!dir) break;

                /* Apply the motion */
                y2 += ddy[dir];
                x2 += ddx[dir];

                /* Verify the row */
                if (y2 > max_panel_rows) y2 = max_panel_rows;
                else if (y2 < 0) y2 = 0;

                /* Verify the col */
                if (x2 > max_panel_cols) x2 = max_panel_cols;
                else if (x2 < 0) x2 = 0;

                /* Handle "changes" */
                if ((y2 != panel_row) || (x2 != panel_col))
                {
                        /* Save the new panel info */
                        panel_row = y2;
                        panel_col = x2;

                        /* Recalculate the boundaries */
                        panel_bounds();

                        /* Update stuff */
                        p_ptr->update |= (PU_MONSTERS);

                        /* Redraw map */
                        p_ptr->redraw |= (PR_MAP);

                        /* Handle stuff */
                        handle_stuff();
                }
        }


        /* Recenter the map around the player */
        verify_panel();

        /* Update stuff */
        p_ptr->update |= (PU_MONSTERS);

        /* Redraw map */
        p_ptr->redraw |= (PR_MAP);

        /* Window stuff */
        p_ptr->window |= (PW_OVERHEAD);

        /* Handle stuff */
        handle_stuff();
}






/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info[] =
{
        " :A dark grid",
        "!:A potion (or oil)",
        "\":An amulet (or necklace)",
        "#:A wall (or secret door)",
        "$:Treasure (gold or gems)",
        "%:A vein (magma or quartz)",
        /* "&:unused", */
        "':An open door",
        "(:Soft armor",
        "):A shield",
        "*:A vein with treasure",
        "+:A closed door",
        ",:Food (or mushroom patch)",
        "-:A wand (or rod)",
        ".:Floor",
        "/:A polearm (Axe/Pike/etc)",
        "0:An altar",
        "1:Entrance to General Store",
        "2:Entrance to Armory",
        "3:Entrance to Weaponsmith",
        "4:Entrance to Temple",
        "5:Entrance to Alchemy shop",
        "6:Entrance to Magic store",
        "7:Entrance to Black Market",
        "8:Entrance to your home",
        "9:Entrance to the bookstore",
        "::Rubble",
        ";:A glyph of warding / explosive rune",
        "<:An up staircase",
        "=:A ring",
        ">:A down staircase",
        "?:A scroll",
        "@:You",
        "A:Angel",
        "B:Bird",
        "C:Canine",
        "D:Ancient Dragon/Wyrm",
        "E:Elemental",
        "F:Dragon Fly",
        "G:Ghost",
        "H:Hybrid",
        "I:Insect",
        "J:Snake",
        "K:Killer Beetle",
        "L:Lich",
        "M:Multi-Headed Reptile",
        /* "N:unused", */
        "O:Ogre",
        "P:Giant Humanoid",
        "Q:Quylthulg (Pulsing Flesh Mound)",
        "R:Reptile/Amphibian",
        "S:Spider/Scorpion/Tick",
        "T:Troll",
        "U:Major Demon",
        "V:Vampire",
        "W:Wight/Wraith/etc",
        "X:Xorn/Xaren/etc",
        "Y:Yeti",
        "Z:Zephyr Hound",
        "[:Hard armor",
        "\\:A hafted weapon (mace/whip/etc)",
        "]:Misc. armor",
        "^:A trap",
        "_:A staff",
        /* "`:unused", */
        "a:Ant",
        "b:Bat",
        "c:Centipede",
        "d:Dragon",
        "e:Floating Eye",
        "f:Feline",
        "g:Golem",
        "h:Hobbit/Elf/Dwarf",
        "i:Icky Thing",
        "j:Jelly",
        "k:Kobold",
        "l:Lizard?",
        "m:Mold",
        "n:Naga",
        "o:Orc",
        "p:Person/Human",
        "q:Quadruped",
        "r:Rodent",
        "s:Skeleton",
        "t:Townsperson",
        "u:Minor Demon",
        "v:Vortex",
        "w:Worm/Worm-Mass",
        "x:eXtraterrestrial aliens",
        "y:Yeek",
        "z:Zombie/Mummy",
        "{:A missile (arrow/bolt/shot)",
        "|:An edged weapon (sword/dagger/etc)",
        "}:A launcher (bow/crossbow/sling)",
        "~:Aquatic monster or misc item",
        NULL
};



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
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
static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
        u16b *who = (u16b*)(u);

        u16b holder;

        /* XXX XXX */
        v = v ? v : 0;

        /* Swap */
        holder = who[a];
        who[a] = who[b];
        who[b] = holder;
}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
        monster_race    *r_ptr = &r_info[r_idx];

        byte            a1, a2;
        char            c1, c2;


        /* Access the chars */
        c1 = r_ptr->d_char;
        c2 = r_ptr->x_char;

        /* Access the attrs */
        a1 = r_ptr->d_attr;
        a2 = r_ptr->x_attr;

        /* Hack -- fake monochrome */
        if (!use_color) a1 = TERM_WHITE;
        if (!use_color) a2 = TERM_WHITE;


        /* Clear the top line */
        Term_erase(0, 0, 255);

        /* Reset the cursor */
        Term_gotoxy(0, 0);

        /* A title (use "The" for non-uniques) */
        if (!(r_ptr->flags1 & (RF1_UNIQUE)))
        {
                Term_addstr(-1, TERM_WHITE, "The ");
        }

        /* Dump the name */
        Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

        /* Append the "standard" attr/char info */
        Term_addstr(-1, TERM_WHITE, " ('");
        Term_addch(a1, c1);
        Term_addstr(-1, TERM_WHITE, "')");

        /* Append the "optional" attr/char info */
        Term_addstr(-1, TERM_WHITE, "/('");
        Term_addch(a2, c2);
        Term_addstr(-1, TERM_WHITE, "'):");
}

/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "mulitple" monsters:
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
        int     i, n, a, r_idx, k_idx;
        char    sym, query;
        char    buf[128];

        bool    all = FALSE;
        bool    uniq = FALSE;
        bool    norm = FALSE;

        bool    recall = FALSE;

        u16b    why = 0;
        u16b    *who;
        int     row;
        char    *chr_ptr;

        /* Allocate the "who" array */
        C_MAKE(who, max_r_idx, u16b);

        /* Get a character, or abort */
        if (!get_com("Enter character to be identified: ", &sym)) return;

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


        /* First recall monsters */
        for (n = 0, i = 1; i < max_r_idx-1; i++)
        {
                monster_race *r_ptr = &r_info[i];

                /* Nothing to recall */
                if (!cheat_know && !r_ptr->r_sights) continue;

                /* Require non-unique monsters if needed */
                if (norm && (r_ptr->flags1 & RF1_UNIQUE)) continue;

                /* Require unique monsters if needed */
                if (uniq && !(r_ptr->flags1 & RF1_UNIQUE)) continue;

                /* Collect "appropriate" monsters */
                if (all || (r_ptr->d_char == sym)) who[n++] = i;
        }

        /* The following code is almost identical to the
         * existing monster identification code.  The changes
         * occur later.
         */

        if (n)
          {

            /* Prompt XXX XXX XXX */
            put_str("Recall monster details? (k/p/y/n): ", 0, 40);

            /* Query */
            query = inkey();

            /* Restore */
            prt(buf, 0, 0);


            /* Sort by kills (and level) */
            if (query == 'k')
              {
                why = 4;
                query = 'y';
              }

            /* Sort by level */
            if (query == 'p')
              {
                why = 2;
                query = 'y';
              }

            /* Catch "escape" */
            if (query == 'y')
              {

                /* Sort if needed */
                if (why)
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

                    /* Hack -- Begin the prompt */
                    roff_top(r_idx);

                    /* Hack -- Complete the prompt */
                    Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");

                    /* Interact */
                    while (1)
                      {
                        /* Recall */
                        if (recall)
                          {
                            /* Save the screen */
                            screen_save();

                            /* Recall on screen */
                            screen_roff(who[i], i);

                            /* Hack -- Complete the prompt (again) */
                            Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
                          }

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
              }

            /* Re-display the identity */
            prt(buf, 0, 0);
          }

        /*
         * Now that monster recall is finished, let's do object
         * recall.  The following code is essentially new.
         */

        /*
         * First we scan k_info to see which objects have the
         * same symbol as the requested object.  Note that we
         * only include those objects which we are "aware" of.
         */

        for (n = 0, i = 1; i < max_k_idx-1; i++)
        {
                object_kind *k_ptr = &k_info[i];

                if ((k_ptr->aware) &&
                    (k_ptr->d_char == sym)) who[n++] = i;
        }

        if (!n) return;  /* No objects to recall */

        /* Ask if user wants to see known objects */
        put_str("Recall known objects? (y/n): ", 0, 40);
        query = inkey();

        /* Restore */
        prt(buf, 0, 0);

        /* Return if not 'y' */
        if (query != 'y') return;

        /* Save the screen */
        screen_save();

        row = 0;
        for (i=0; i<n; i++)
          {
            /* Get the object index */
            k_idx = who[i];

            /* Two objects per row... */
            if (i%2 == 0)
              {
                row++;
                Term_erase(0, row, 255);
              }

            /* Position cursor */
            Term_gotoxy(40*(i%2), row);

            /* And start printing out string */
            Term_addstr(-1, TERM_WHITE, " [");

            /* Hack in case there is no color. */
            if (use_color)
              Term_addch(k_info[k_idx].d_attr, k_info[k_idx].d_char);
            else
              Term_addch(TERM_WHITE, k_info[k_idx].d_char);

            Term_addstr(-1, TERM_WHITE, "]  ");



            /*
             * Some objects have the same symbol and must be
             * told apart.  These are the ones that I thought
             * were the most important.
             * Note the hack for mushrooms.  Only mushrooms
             * among the food items have "flavor"
             */
            switch (k_info[k_idx].tval)
              {
              case TV_WAND:
                Term_addstr(-1, TERM_WHITE, "Wand of ");
                break;
              case TV_ROD:
                Term_addstr(-1, TERM_WHITE, "Rod of ");
                break;
              case TV_SCROLL:
                Term_addstr(-1, TERM_WHITE, "Scroll of ");
                break;
              case TV_FOOD:
                if (k_info[k_idx].flavor)
                  Term_addstr(-1, TERM_WHITE, "Mushroom of ");
                break;
              }


            /*
             * Mega-hack to parse the "name" of the object ...
             * In this code I strip out the '~' and '&' from the
             * object name.  In the case of '&', I also remove
             * an additional space.
             */
            for (chr_ptr = k_name+k_info[k_idx].name; *(chr_ptr); chr_ptr++)
              {
                if ((*(chr_ptr) != '~') && (*(chr_ptr) != '&'))
                  Term_addch(TERM_WHITE, *(chr_ptr));
                if (*(chr_ptr) == '&') chr_ptr++;
              }

            /*
             * Another hack...  For the special objects (Phial, etc.)
             * I put the full artifact name if it has been generated.
             */
            if (k_info[k_idx].flags3 & TR3_INSTA_ART)
              {
                Term_addstr(-1, TERM_WHITE, " ");
                /* Now find the name of the artifact */
                for (a=1; a<max_a_idx-1; a++)
                  {
                    /* If this is the artifact, print it */
                    if ((a_info[a].tval == k_info[k_idx].tval) &&
                        (a_info[a].sval == k_info[k_idx].sval) &&
                        (a_info[a].cur_num == 1))
                      Term_addstr(-1, TERM_WHITE, a_name+a_info[a].name);
                  }
              }



            /* Check to see if we are at bottom of screen */
            if  ((i%44 == 43) && (i<n-1))
              {
                Term_erase(0, 23, 255);
                Term_gotoxy(0, 23);
                Term_addstr(-1, TERM_WHITE, " ---More--- (Press a key)");
                query = inkey();
                screen_load();
                row = 0;
              }
          }

        row++;
        Term_erase(0, row, 255);
        Term_gotoxy(0, row);
        Term_addstr(-1, TERM_WHITE, " ---Done---  (Press a key)");
        query = inkey();
        screen_load();
        return;
}

/*
 *  research_mon
 *  -KMW-
 */
bool research_mon(void)
{
        int i, n, r_idx;
        char sym, query;
        char buf[128];

        s16b oldkills;
        byte oldwake;
        bool oldcheat;

        bool notpicked;

        bool recall = FALSE;

        u16b why = 0;

        monster_race *r2_ptr;

        u16b    *who;

        oldcheat = cheat_know;

        /* Save the screen */
        screen_save();

        /* Get a character, or abort */
        if (!get_com("Enter character of monster: ", &sym)) return (FALSE);
        if (!get_com("Enter character of monster: ", &sym))
        {
                /* Restore */
                screen_load();

                return (FALSE);
        }

        /* Allocate the "who" array */
        C_MAKE(who, max_r_idx, u16b);

        /* Find that character info, and describe it */
        for (i = 0; ident_info[i]; ++i)
        {
                if (sym == ident_info[i][0]) break;
        }

        if (ident_info[i])
        {
                sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
        }
        else
        {
                sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
        }

        /* Display the result */
        prt(buf, 16, 10);


        /* Collect matching monsters */
        for (n = 0, i = 1; i < max_r_idx; i++)
        {
                monster_race *r_ptr = &r_info[i];

                cheat_know = TRUE;

                /* Collect "appropriate" monsters */
                if (r_ptr->d_char == sym) who[n++] = i;
        }

        /* Nothing to recall */
        if (!n)
        {
                cheat_know = oldcheat;

                /* Free the "who" array */
                C_KILL(who, max_r_idx, u16b);

                /* Restore */
                screen_load();

                return (FALSE);
        }

        /* Sort by level */
        why = 2;
        query = 'y';

        /* Sort if needed */
        if (why)
        {
                /* Select the sort method */
                ang_sort_comp = ang_sort_comp_hook;
                ang_sort_swap = ang_sort_swap_hook;

                /* Sort the array */
                ang_sort(who, &why, n);
        }


        /* Start at the end */
        i = n - 1;

        notpicked = TRUE;

        /* Scan the monster memory */
        while (notpicked)
        {
                /* Extract a race */
                r_idx = who[i];

                /* Save this monster ID */
                monster_race_idx = r_idx;

                /* Hack -- Handle stuff */
                handle_stuff();

                /* Hack -- Begin the prompt */
                roff_top(r_idx);

                /* Hack -- Complete the prompt */
                Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC, space to continue]");

                /* Interact */
                while (1)
                {
                        /* Recall */
                        if (recall)
                        {

                                /* Recall on screen */
                                r2_ptr = &r_info[r_idx];

                                oldkills = r2_ptr->r_tkills;
                                oldwake = r2_ptr->r_wake;
                                screen_roff(who[i], 1);
                                r2_ptr->r_tkills = oldkills;
                                r2_ptr->r_wake = oldwake;
                                cheat_know = oldcheat;
                                notpicked = FALSE;
                        }

                        /* Command */
                        query = inkey();

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


        /* Re-display the identity */
        /* prt(buf, 5, 5);*/

        cheat_know = oldcheat;

        /* Free the "who" array */
        C_KILL(who, max_r_idx, u16b);

        /* Restore */
        screen_load();

        return (!notpicked);
}

/*
 * Rogues may steal gold from monsters.  The monster needs to have
 * something to steal (it must drop some form of loot), and should
 * preferably be asleep.  Humanoids and dragons are a rogue's favourite
 * targets.  Steal too often on a level, and monsters will be more wary,
 * and the hue and cry will be eventually be raised.  Having every
 * monster on the level awake and aggravated is not pleasant. -LM-
 */

void py_steal(int y, int x)
{
        cptr act = NULL;

        cave_type *c_ptr = &cave[y][x];

        monster_type *m_ptr = &m_list[c_ptr->m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        char m_name[80];

        int i;
        int effect, theft_protection;
        int filching_power = 0;
        int purse = 0;

        bool thief = FALSE;
        bool success = FALSE;

        /* Hard limit on theft. */
        if (number_of_thefts_on_level > p_ptr->lev/2)
        {
                msg_print("Everyone is keeping a lookout for you.  You can steal nothing here.");
                return;
        }

        /* Determine the cunning of the thief. */
        filching_power = 2 * p_ptr->lev;


        /* Determine how much protection the monster has. */
        theft_protection = (7 * (r_ptr->level + 2) / 4);
        theft_protection += (m_ptr->mspeed - p_ptr->pspeed);

        /* Send a thief to catch a thief. */
        for (i = 0; i < 4; i++)
        {
                /* Extract infomation about the blow effect */
                effect = r_ptr->blow[i].effect;
                if (effect == RBE_EAT_GOLD) thief = TRUE;
                if (effect == RBE_EAT_ITEM) thief = TRUE;
        }
        if (thief) theft_protection += 30;

        if ((m_ptr->csleep) && (theft_protection > 0)) theft_protection = 3 * theft_protection / 5;

        /* The more you steal on a level, the more wary the monsters. */
        theft_protection += number_of_thefts_on_level * 15;

        /* Did the theft succeed?  */
        if ((theft_protection > 0) && (randint(theft_protection) < filching_power)) success = TRUE;


        /* If the theft succeeded, determine the value of the purse. */
        if (success)
        {
                purse = randint(3 * (r_ptr->level + 2) / 2);

                /* Uniques are juicy targets. */
                if (r_ptr->flags1 & (RF1_UNIQUE)) purse *= 3;

                /* But some monsters are dirt poor. */
                if (!((r_ptr->flags1 & (RF1_DROP_60)) ||
                    (r_ptr->flags1 & (RF1_DROP_90)) ||
                    (r_ptr->flags1 & (RF1_DROP_1D2)) ||
                    (r_ptr->flags1 & (RF1_DROP_2D2)) ||
                    (r_ptr->flags1 & (RF1_DROP_3D2)) ||
                    (r_ptr->flags1 & (RF1_DROP_4D2)))) purse = 0;

                /* Some monster races are far better to steal from than others. */
                if ((r_ptr->d_char == 'D') || (r_ptr->d_char == 'd') ||
                        (r_ptr->d_char == 'p') || (r_ptr->d_char == 'h'))
                        purse *= 2 + randint(3) + randint(r_ptr->level / 20);
                else if ((r_ptr->d_char == 'P') || (r_ptr->d_char == 'o') ||
                        (r_ptr->d_char == 'O') || (r_ptr->d_char == 'T') ||
                        (r_ptr->d_char == 'n') || (r_ptr->d_char == 'W') ||
                        (r_ptr->d_char == 'k') || (r_ptr->d_char == 'L') ||
                        (r_ptr->d_char == 'V') || (r_ptr->d_char == 'y'))
                        purse *= 1 + randint(3) + randint(r_ptr->level / 30);

                /* Pickings are scarce in a land of many thieves. */
                purse *= (dun_level + 5) / (p_ptr->max_dlv + 5);

                /* Increase player gold. */
                p_ptr->au += purse;

                /* Redraw gold */
                p_ptr->redraw |= (PR_GOLD);

                /* Announce the good news. */
                if (purse) msg_format("You burgle %d gold.", purse);

                /* Pockets are empty. */
                else msg_print("You burgle only dust.");
        }

        /* The victim normally, but not always, wakes up and is aggravated. */
        if (randint(4) != 1)
        {
                m_ptr->csleep = 0;
                if (m_ptr->mspeed < r_ptr->speed + 3) m_ptr->mspeed += 10;


                /* Occasionally, amuse the player with a message. */
                if ((randint(5) == 1) && (purse) && (r_ptr->flags2 & (RF2_SMART)))
                {
                        monster_desc(m_name, m_ptr, 0);
                        act = desc_victim_outcry[rand_int(20)];
                        msg_format("%^s cries out %s", m_name, act);
                }
                /* Otherwise, simply explain what happened. */
                else
                {
                        monster_desc(m_name, m_ptr, 0);
                        msg_format("You have aroused %^s.", m_name);
                }
        }

        /* The thief also speeds up, but only for just long enough to escape. */
        if (!p_ptr->fast) p_ptr->fast += 2;

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Handle stuff */
        handle_stuff();


        /* Increment the number of thefts, and possibly raise the hue and cry. */
        number_of_thefts_on_level++;

        if (number_of_thefts_on_level > p_ptr->lev/2)
        {
                /* Notify the player of the trouble he's in. */
                msg_print("All the level is in an uproar over your misdeeds!");

                /* Aggravate and speed up all monsters on level. */
                aggravate_monsters(1);
        }
}
