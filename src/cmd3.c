/* File: cmd3.c */

/* Purpose: Inventory commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Move an item from equipment list to pack
 * Note that only one item at a time can be wielded per slot.
 * Note that taking off an item when "full" will cause that item
 * to fall to the ground.
 */
static void inven_takeoff(s16b item, s16b amt)
{
   object_type         *i_ptr;
   object_type         tmp_obj;

   cptr                act;

   char                i_name[80];

   /* Get the item to take off */
   i_ptr = &inventory[item];

   /* Paranoia */
   if (amt <= 0) return;

   /* Verify */
   if (amt > i_ptr->number) amt = i_ptr->number;

   /* Make a copy to carry */
   tmp_obj = *i_ptr;
   tmp_obj.number = amt;

   /* What are we "doing" with the object */
   if ((item == INVEN_WIELD) && (p_ptr->pclass != CLASS_HIGHPRST))
   {
      if (p_ptr->twohands) act = "Was wielding 2H";
      else act = "Was wielding";
/* jk */
      p_ptr->twohands=FALSE;
   }
   else if ((item == INVEN_BOW) && (p_ptr->pclass != CLASS_HIGHPRST))
   {
      act = "Was shooting with";
   }
   else if (item == INVEN_AMMO)
   {
      if (amt == i_ptr->number)
      {
         act = "Was shooting with";
      }
      else
      {
         act = "Took off";
      }
   }

   else if (item == INVEN_LITE)
   {
      act = "Light source was";
   }
   else
   {
      act = "Was wearing";
   }

   /* Describe the result */
   if (amt < i_ptr->number)
   {
      s16b old_number = i_ptr->number;
      i_ptr->number = amt;
      object_desc(i_name, i_ptr, TRUE, 3);
      i_ptr->number = old_number;
   }
   else
   {
      object_desc(i_name, i_ptr, TRUE, 3);
   }

   /* Message */
   msg_format("%^s %s.", act, i_name);

   if ((i_ptr->tval == TV_HAFTED) && (i_ptr->sval == SV_QUARTERSTAFF))
   {
      if ( (i_ptr->name2 == EGO_MAGE) ||
           (i_ptr->name2 == EGO_ADEPT) ||
           (i_ptr->name2 == EGO_ARCHMAGE))
      {
         if ( (!i_ptr->timeout) && (bit_class(p_ptr->pclass) & MAGE_MAGIC_CLASS) )
         {
            msg_print("Your staff lets out an angry hiss.");
            tmp_obj.timeout = 1000;
         }
      }
   }

   (void)inven_carry(&tmp_obj,amt);

   /* Delete (part of) it */
   item_increase(item, -amt, px, py);
   item_optimize(item, px, py);
}

/*
 * Drops (some of) an item from inventory to "near" the current location
 */
static void inven_drop(s16b item, s16b amt)
{
   object_type         *i_ptr;
   object_type         tmp_obj;

   cptr                act;

   char                i_name[80];

   /* Access the slot to be dropped */
   i_ptr = &inventory[item];

   /* Error check */
   if (amt <= 0) return;

   /* Not too many */
   if (amt > i_ptr->number) amt = i_ptr->number;

   /* Nothing done? */
   if (amt <= 0) return;

   /* Make a "fake" object */
   tmp_obj = *i_ptr;
   tmp_obj.number = amt;
   if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val>0))
   {
      /* the dropped rods cannot recharge longer than amt * max_charge */
      tmp_obj.p1val = min(tmp_obj.p1val, amt * get_rod_charge(&tmp_obj));
      /* prevent negative charges on the remaining items if we drop 1 partially charged rod */
      i_ptr->p1val = max(0, i_ptr->p1val - tmp_obj.p1val);
   }
   if ((i_ptr->tval == TV_WAND) || (i_ptr->tval == TV_STAFF))
   {
      /* if we have 4 drop 1, 25% of charges in dropped, 75% in kept */
      tmp_obj.p1val = (amt * i_ptr->p1val) / i_ptr->number;
      i_ptr->p1val = i_ptr->p1val - tmp_obj.p1val;
   }

   /* What are we "doing" with the object */
   if (amt < i_ptr->number)
   {
      act = "Dropped";
   }
   else if ((item == INVEN_WIELD) && (p_ptr->pclass != CLASS_HIGHPRST))
   {
      if (p_ptr->twohands) act = "Was wielding 2H";
      else act = "Was wielding";
   }
   else if ( ((item == INVEN_BOW) || (item == INVEN_AMMO)) && (p_ptr->pclass != CLASS_HIGHPRST))
   {
      act = "Was shooting with";
   }
   else if (item == INVEN_LITE)
   {
      act = "Light source was";
   }
   else if (item >= INVEN_WIELD)
   {
      act = "Was wearing";
   }
   else
   {
      act = "Dropped";
   }

   /* Message */
   object_desc(i_name, &tmp_obj, TRUE, 3);

   /* Message */
   msg_format("%^s %s (%c).", act, i_name, index_to_label(item));

   /* Drop it (carefully) near the player */
   (void)drop_near(&tmp_obj, 0, px, py, 0, FALSE, FALSE);

   /* Decrease the item, optimize. */
   item_increase(item, -amt, px, py);
   item_describe(item, px, py);
   item_optimize(item, px, py);
}

/*
 * this prints a prompt, with a colored representation of the
 * current weight carried
 */
static void print_inven_command_prompt(cptr type)
{
   char out_val1[80], out_val2[80], out_val3[80];

   /* Build a prompt */
   /* Insert the total burden and character capacity into a string. -LM- */

   sprintf(out_val1, "(%s) burden %d.%d lb (",
           type, p_ptr->total_weight / 10, p_ptr->total_weight % 10);
   sprintf(out_val2, "%d%% of capacity", p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);
   sprintf(out_val3, "). Command: ");
   c_put_str(TERM_WHITE,out_val1, 0, MESSAGE_ROW);
   c_put_str(weight_color(), out_val2, 0+strlen(out_val1), MESSAGE_ROW);
   c_put_str(TERM_WHITE,out_val3, 0+strlen(out_val1)+strlen(out_val2), MESSAGE_ROW);
}

/*
 * Display inventory
 */
void do_cmd_inven(void)
{

   /* Note that we are in "inventory" mode */

   p_ptr->command_wrk = FALSE;

   /* Save the screen */
   Term_save();

   /* Hack -- show empty slots */
   item_tester_full = TRUE;

   /* Display the inventory */
   show_inven();

   /* Hack -- hide empty slots */
   item_tester_full = FALSE;

   print_inven_command_prompt("Inventory");

   /* Get a new command */
   p_ptr->command_new = inkey();

   /* Restore the screen */
   Term_load();

   /* Process "Escape" */
   if (p_ptr->command_new == ESCAPE)
   {
      /* Reset stuff */
      p_ptr->command_new = 0;
   }

   /* Process normal keys */
   else
   {
      /* Hack -- Use "display" mode */
      p_ptr->command_see = TRUE;
   }
}

/*
 * Display equipment
 */
void do_cmd_equip(void)
{
   /* Note that we are in "equipment" mode */
   p_ptr->command_wrk = TRUE;

   /* Save the screen */
   Term_save();

   /* Hack -- show empty slots */
   item_tester_full = TRUE;

   /* Display the equipment */
   show_equip();

   /* Hack -- undo the hack above */
   item_tester_full = FALSE;

   print_inven_command_prompt("Equipment");

   /* Get a new command */
   p_ptr->command_new = inkey();

   /* Restore the screen */
   Term_load();

   /* Process "Escape" */
   if (p_ptr->command_new == ESCAPE)
   {
      /* Reset stuff */
      p_ptr->command_new = 0;
   }

   /* Process normal keys */
   else
   {
      /* Enter "display" mode */
      p_ptr->command_see = TRUE;
   }
}

/*
 * The "wearable" tester
 */
static bool item_tester_hook_wear(object_type *i_ptr)
{
   /* Check for a usable slot */
   if (wield_slot(i_ptr) >= INVEN_WIELD) return (TRUE);

   /* Assume not wearable */
   return (FALSE);
}

/*
 * must this weapon be wielded by two hands?
 */
bool must_2h(object_type *i_ptr)
{
   bool result = FALSE;

   if (i_ptr->name1)
   {
      result |= ( (a_info[i_ptr->name1].flags3 & TR3_MUST2H) ? TRUE : FALSE);
   }
   if (i_ptr->name2)
   {
      result |= ( (e_info[i_ptr->name1].flags3 & TR3_MUST2H) ? TRUE : FALSE);
   }
   result |= ( (k_info[i_ptr->k_idx].flags3 & TR3_MUST2H) ? TRUE : FALSE);
   return (result);
}

/*
 * could this weapon be wielded by two hands?
 */
bool could_2h(object_type *i_ptr)
{
   bool result = FALSE;

   if (i_ptr->name1)
   {
      result |= ( (a_info[i_ptr->name1].flags3 & TR3_COULD2H) ? TRUE : FALSE);
   }
   if (i_ptr->name2)
   {
      result |= ( (e_info[i_ptr->name1].flags3 & TR3_COULD2H) ? TRUE : FALSE);
   }
   result |= ( (k_info[i_ptr->k_idx].flags3 & TR3_COULD2H) ? TRUE : FALSE);
   return (result);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
   s16b item, slot;
   object_type tmp_obj;
   object_type *i_ptr = NULL;
   cptr act;
   char i_name[80];
/* jk */
/* for the two-handed routines */
   bool shield_present;
   bool wield_new_twoh = FALSE;
   bool gladiator_weapon = TRUE;

   s16b amt = -1;

   /* Restrict the choices */
   item_tester_hook = item_tester_hook_wear;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Wear/Wield which item? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing you can wear or wield.");
      if (item == -1) msg_print("Aborted!");
      item_tester_hook = NULL;
      return;
   }
   if (amt == 0) /* did we abort choosing a quantity for ammo? */
   {
      item_tester_hook = NULL;
      return;
   }

   /* reset the item-selection hook */
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Check the slot */
   slot = wield_slot(i_ptr);

   if ((p_ptr->pclass == CLASS_GLADIATR) && (slot==INVEN_ARM))
   {
      msg_print("As a Gladiator, you can't use this shield.");
      return;
   }

   /* gladiators are difficult :-) */
   if ((p_ptr->pclass == CLASS_GLADIATR) && (slot == INVEN_WIELD) &&
       (inventory[INVEN_WIELD].k_idx))
   {
      /* we are already wearing a secondary weapon */
      if (inventory[INVEN_ARM].k_idx != 0)
      {
         /* so 2h weapons are out */
         if (must_2h(i_ptr))
         {
            msg_print("You cannot wield this 2-hands weapon with a secondary weapon.");
            return;
         }
         /* else we check */
         else if (get_check("Wield this weapon as secondary weapon?"))
         {
            slot = INVEN_ARM;
            gladiator_weapon = TRUE;
            if (could_2h(i_ptr))
            {
               msg_print("As primary weapon, you could wield this weapon two-handed");
            }
         }
      }
      /* we are not wearing a secondary weapon */
      else
      {
         /* 2h weapons are primary weapons by definition */
         if (must_2h(i_ptr))
         {
            slot = INVEN_WIELD;
         }
         /* a possible 2h weapon as secondary weapon may be possible */
         else if (p_ptr->twohands)
         {
            /* we can wield the secondary weapon 2h, but we are already wearing a 2h primary weapon */
            if (must_2h(&inventory[INVEN_WIELD]))
            {
               slot = INVEN_WIELD;
               gladiator_weapon = TRUE;
            }
            else if (could_2h(&inventory[INVEN_WIELD]) && p_ptr->twohands)
            {
               wield_new_twoh=get_check("Continue wielding your primary weapon with 2 hands?");
               if (wield_new_twoh)
               {
                  if (!get_check("Switch this weapon with your current primary weapon?"))
                  {
                     return;
                  }
               }
               else
               {
                  p_ptr->twohands=FALSE;
                  if (could_2h(i_ptr))
                  {
                     msg_print("As primary weapon, you could wield this weapon two-handed");
                  }
               }
               slot = INVEN_ARM;
               gladiator_weapon = TRUE;
            }
            else if ( could_2h(i_ptr) &&
                     get_check("Wield this weapon as secondary weapon? Primary 2-hands is possible"))
            {
               slot = INVEN_ARM;
               gladiator_weapon = TRUE;
            }
            /* if this is a plain and simple one-handed weapon, we switch it with the primary */
            else if (!could_2h(i_ptr) && !must_2h(i_ptr))
            {
               slot = INVEN_ARM;
               gladiator_weapon = TRUE;
            }
            /* if not, we switch it with the primary */
         }
         else
         {
            slot = INVEN_ARM;
            gladiator_weapon = TRUE;
            if (could_2h(i_ptr))
            {
               msg_print("As primary weapon, you could wield this weapon two-handed");
            }
         }
      }
   }


/* jk - no shield wearing while two-handed weapons are there .... */
   if ((slot==INVEN_ARM) && p_ptr->twohands)
   {
      if (p_ptr->pclass == CLASS_GLADIATR)
      {
         msg_format("You can't wield a %s while wielding a two-handed weapon",
                    i_ptr->tval==TV_SHIELD?"shield":"second weapon");
      }
      else
      {
         msg_print("You can't wield a shield while wielding a two-handed weapon");
      }
      return;
   }

   if ( (p_ptr->pclass == CLASS_HIGHPRST) &&
        ((slot == INVEN_WIELD) || (slot == INVEN_BOW) || (slot == INVEN_AMMO) ) &&
        (i_ptr->tval != TV_RING) )
   {
      msg_print("As a High Priest, you can't bring yourself to wield this or any weapon.");
      return;
   }

   /* if we are a gladiator, and the special test hasn't succeeded (gladiator_weapon==FALSE) */
   /* and we try to wear any armour or shield, forbid that                                   */
   if (p_ptr->pclass == CLASS_GLADIATR)
   {
      if ( (slot==INVEN_BODY) || ( (!gladiator_weapon) && (slot==INVEN_ARM)))
      {
         msg_format("As a gladiator, you can't wear this %s.",
                    slot==INVEN_BODY?"armour":"shield");
         return;
      }
   }

   if (slot==INVEN_WIELD)
   {
      shield_present=inventory[INVEN_ARM].k_idx;

      if (must_2h(i_ptr))
      {
         if (p_ptr->pclass==CLASS_MAGE)
         {
            msg_print("As a mage, you can't wield this weapon.");
            return;
         }
         if (p_ptr->pclass==CLASS_PRIEST)
         {
            msg_print("As a priest, you can't wield this weapon.");
            return;
         }
         if (shield_present)
         {
            if (p_ptr->pclass == CLASS_GLADIATR)
            {
               msg_print("You can't wield this two-handed weapon and another weapon.");
            }
            else
            {
               msg_print("You can't wield this two-handed weapon with a shield.");
            }
            return;
         }
         else
         {
            wield_new_twoh=TRUE;
         }
      }
      else if (could_2h(i_ptr))
      {
         if (shield_present)
         {
            if (p_ptr->pclass == CLASS_GLADIATR)
            {
               msg_print("Without a second weapon, you could wield this weapon two-handed.");
            }
            else if ((p_ptr->pclass!=CLASS_MAGE) && (p_ptr->pclass!=CLASS_PRIEST))
            {
               msg_print("Without a shield, you could wield this weapon two-handed");
            }
         }
         else if ((p_ptr->pclass!=CLASS_MAGE) && (p_ptr->pclass!=CLASS_PRIEST))
         {
            object_desc(i_name, i_ptr, FALSE, 0);
            wield_new_twoh=get_check(format("Wield %s two-handed?",i_name));
         }
      }
   }

   /* Prevent wielding into a cursed slot */
   if (cursed_p(&inventory[slot]))
   {

       /* Describe it */
       object_desc(i_name, &inventory[slot], FALSE, 0);

       /* Message */
       msg_format("The %s you are %s appears to be cursed.",
                  i_name, describe_use(slot));

       /* Cancel the command */
       return;
   }

   /* Verify potential overflow */
   if ((inven_cnt >= INVEN_PACK) &&
       ((item >INVEN_TOTAL) || (i_ptr->number > 1)))
   {
       /* Verify with the player */
       if (other_query_flag &&
           !get_check("Your pack may overflow.  Continue? ")) return;
   }

   /* Take a turn */
   energy_use = 100;

   /* Get a copy of the object to wield */
   tmp_obj = *i_ptr;
   tmp_obj.number = amt;

   /* Decrease the item (from the pack) */
   item_increase(item, -amt, px, py);
   item_optimize(item, px, py);

   /* Access the wield slot */
   i_ptr = &inventory[slot];
   if (object_similar(i_ptr, &tmp_obj) && (slot == INVEN_AMMO))
   {
      item_increase(slot, amt, px, py);
      item_optimize(slot, px, py);
   }
   else
   {
      /* Take off the "entire" item if something is there */
      if (inventory[slot].k_idx) inven_takeoff(slot, 255);
      /*** Could make procedure "inven_wield()" ***/

      /* Wear the new stuff */
      *i_ptr = tmp_obj;
   }

   /* Increment the equip counter by hand */
   equip_cnt++;

   /* Where is the item now */
   if ((slot == INVEN_WIELD) && (p_ptr->pclass != CLASS_HIGHPRST))
   {
      p_ptr->twohands = wield_new_twoh;
      if (p_ptr->twohands) act = "You are wielding 2H";
      else act = "You are wielding";
   }
   else if ( ((slot == INVEN_BOW) || (slot == INVEN_AMMO)) &&
             (p_ptr->pclass != CLASS_HIGHPRST))
   {
      act = "You are shooting with";
   }
   else if (slot == INVEN_LITE)
   {
      act = "Your light source is";
   }
   else if ((slot == INVEN_ARM) && (p_ptr->pclass == CLASS_GLADIATR) &&
            (i_ptr->tval != TV_SHIELD))
   {
      act = "Your secondary weapon is";
   }
   else
   {
      act = "You are wearing";
   }

   /* Describe the result */
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Message */
   msg_format("%^s %s (%c).", act, i_name, index_to_label(slot));

/* jk - on wielding the spear of melkor, it becomes heavy_cursed again */
/* and poisons you to */
   if (i_ptr->name1==ART_MELKOR)
   {
      set_poisoned(10);
/* supposedly the HEAVY_CURSE flag in a_info makes this that making */
/* i_ptr->ident |=ID_CURSED acts as making it heavy_cursed */
      i_ptr->ident |=ID_CURSED;
   }

   /* Cursed! */
   if (cursed_p(i_ptr))
   {
      /* Warn the player */
      msg_print("Oops! It feels deathly cold!");

      /* Note the curse */
      i_ptr->ident |= ID_SENSE;
   }

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Recalculate torch */
   p_ptr->update |= (PU_TORCH);

   /* Recalculate mana */
   p_ptr->update |= (PU_MANA);

   p_ptr->redraw2 |= PR2_EQUIPPY;

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
   s16b item;

   object_type *i_ptr;

/* jk */
   s16b amt = -1;

   /* Verify potential overflow */
   if (inven_cnt >= INVEN_PACK)
   {
      /* Verify with the player */
      if (other_query_flag &&
          !get_check("Your pack may overflow.  Continue? ")) return;
   }

   /* Get an item (from equip) */
   if (!get_item(&item, &amt, "Take off which item? ", TRUE, FALSE, FALSE))
   {
      if (item == -2) msg_print("You are not wearing anything to take off.");
      return;
   }

   /* Get the item (in the pack) */
   i_ptr = &inventory[item];

   /* Item is cursed */
   if (cursed_p(i_ptr))
   {
      /* Oops */
      msg_print("Hmmm, it seems to be cursed.");

      /* Nope */
      return;
   }


   /* Take a partial turn */
   energy_use = 50;

   /* Take off the item */
   inven_takeoff(item, amt);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
   s16b         item, amt = 0;
   object_type *i_ptr;

   /* Get an item (from equip or inven) */
   if (!get_item(&item, &amt, "Drop which item? ", TRUE, TRUE, FALSE))
   {
      if (item == -2) msg_print("You have nothing to drop.");
      return;
   }
/* this is really absurd: there was code to provide for dropping items */
/* off the floor to the floor? */
   if (item == -1)
   {
      msg_print("You can't drop something that's already on the floor.");
      return;
   }
   /* Get the item (in the pack) */
   i_ptr = &inventory[item];
   if (i_ptr->number==1) amt=1;
   /* Cannot remove cursed items */
   if ((item >= INVEN_WIELD) && cursed_p(i_ptr))
   {
      /* Oops */
      msg_print("Hmmm, it seems to be cursed.");

      /* Nope */
      return;
   }

   if (amt<=0) return;

   /* Take a partial turn */
   energy_use = 50;

   /* Drop (some of) the item */
   inven_drop(item, amt);
/* jk - this may seem appropriate if dropping out of equip is allowed */
   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Recalculate torch */
   p_ptr->update |= (PU_TORCH);

   /* Recalculate mana */
   p_ptr->update |= (PU_MANA);
}

/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
/* jk - amt =1, so get_item won't ask how many, because we will crush */
/* all cursed items if so desired before asking how many */
   s16b                 item, amt = 1;
   s16b                 old_number;
   object_type         *i_ptr = NULL;
   char                 i_name[80];
   char                 out_val[160];

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Destroy which item? ", FALSE, TRUE, TRUE))
   {
       if (item == -2) msg_print("You have nothing to destroy.");
       return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

/* jk - determine flags */

   /* See how many items */
   if (i_ptr->number > 1)
   {
/* jk - if we know it's cursed and want no confirmation - kill all */
      if (worthless(i_ptr) && kill_cursed_pack)
      {
         amt= i_ptr->number;
      }
      else
      {
         /* Get a quantity */
         amt = get_quantity(NULL, i_ptr->number,i_ptr->number);
      }
      /* Allow user abort */
      if (amt <= 0) return;
   }

   /* Describe the object */
   old_number = i_ptr->number;
   i_ptr->number = amt;
   object_desc(i_name, i_ptr, TRUE, 1);
   i_ptr->number = old_number;

   /* Make a verification */
   if (!worthless(i_ptr) && !kill_cursed_pack)
   {
      sprintf(out_val, "Really destroy %s? ", i_name);
      if (!get_check(out_val)) return;
   }

   /* Take a turn */
   energy_use = 100;

   /* Artifacts cannot be destroyed */
   if (artifact_p(i_ptr))
   {
      cptr feel = "special";

      /* Message */
      msg_format("You cannot destroy %s.", i_name);

      /* Hack -- Handle icky artifacts */
      if (cursed_p(i_ptr) || broken_p(i_ptr)) feel = "terrible";

      /* Hack -- inscribe the artifact */
      i_ptr->note = quark_add(feel);

      /* We have "felt" it (again) */
      i_ptr->ident |= (ID_SENSE);

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP);

      /* Combine the pack */
      p_ptr->notice |= (PN_COMBINE);

      /* Done */
      return;
   }

   /* Message */
   if (show_full_name_on_destroy && !object_has_flavor(i_ptr->k_idx))
   {
      char i_name2[80];
      i_ptr->ident |= (ID_KNOWN | ID_EQUIPTESTED | ID_MENTAL);
      object_desc(i_name2, i_ptr, TRUE, 3);
      msg_format("You destroy %s.", i_name2);
   }
   else
   {
      msg_format("You destroy %s.", i_name);
   }

   /* Eliminate the item (from the pack) */
   item_increase(item, -amt, px, py);
   item_describe(item, px, py);
   item_optimize(item, px, py);
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
   s16b                 item;
   object_type         *i_ptr = NULL;
   char                i_name[80];

/* jk */
   s16b amt = 1;

   /* Get an item (from equip or inven or floor) */
   if (!get_item(&item, &amt, "Examine which item? ", TRUE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to examine.");
      return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);


   /* Require full knowledge */
   if (!(i_ptr->ident & ID_MENTAL))
   {
      (void)identify_fully_aux(i_ptr, TRUE);
      return;
   }

   /* Description */
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Describe */
   msg_format("Examining %s...", i_name);

   /* Describe it fully */
   if (!identify_fully_aux(i_ptr, FALSE)) msg_print("You see nothing special.");
}

/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
   s16b   item;

   object_type *i_ptr = NULL;
/* jk */
   s16b amt = 1;

   /* Get an item (from equip or inven or floor) */
   if (!get_item(&item, &amt, "Un-inscribe which item? ", TRUE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to un-inscribe.");
      return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Nothing to remove */
   if (!i_ptr->note)
   {
      msg_print("That item had no inscription to remove.");
      return;
   }

   /* Message */
   msg_print("Inscription removed.");

   /* Remove the incription */
   i_ptr->note = 0;

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Combine the pack */
   p_ptr->notice |= (PN_COMBINE);
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
   s16b                 item;
   object_type         *i_ptr = NULL;
   char                 i_name[80];
   char                 out_val[80];
/* jk */
   s16b                 amt = 1;

   /* Get an item (from equip or inven or floor) */
   if (!get_item(&item, &amt, "Inscribe which item? ", TRUE, TRUE, TRUE))
   {
       if (item == -2) msg_print("You have nothing to inscribe.");
       return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Describe the activity */
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Message */
   msg_format("Inscribing %s.", i_name);
   msg_print(NULL);

   /* Start with nothing */
   strcpy(out_val, "");

   /* Use old inscription */
   if (i_ptr->note)
   {
      /* Start with the old inscription */
      strcpy(out_val, quark_str(i_ptr->note));
   }

   /* Get a new inscription (possibly empty) */
   if (get_string("Inscription: ", out_val, 80))
   {
      /* Save the inscription */
      i_ptr->note = quark_add(out_val);

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP);

      /* Combine the pack */
      p_ptr->notice |= (PN_COMBINE);
   }
}

/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(object_type *i_ptr)
{
   /* Flasks of oil are okay */
   if (i_ptr->tval == TV_FLASK) return (TRUE);

   /* Other lanterns are okay */
   if ((i_ptr->tval == TV_LITE) &&
       (i_ptr->sval == SV_LITE_LANTERN)) return (TRUE);
   if ((i_ptr->tval == TV_LITE) &&
       (i_ptr->sval == SV_LITE_NOLDOR)) return (TRUE);

   /* Assume not okay */
   return (FALSE);
}

/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(void)
{
   s16b item;
   object_type *i_ptr = NULL;
   object_type *j_ptr;
/* jk */
/* refilling from more flasks is possible now */
   s16b amt;
   u32b max_fuel = FUEL_LAMP;

   /* Restrict the choices */
   item_tester_hook = item_tester_refill_lantern;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Refill with which flask? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have no flasks of oil.");
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   if (amt>0)
   {
      /* Take a partial turn */
      energy_use = 50;

      /* Access the lantern */
      j_ptr = &inventory[INVEN_LITE];

      /* Refuel */
      j_ptr->p1val += (u32b)i_ptr->p1val * (u32b)amt;

      /* Message */
      msg_print("You fuel your lamp.");

      if ( (inventory[INVEN_LITE].tval == TV_LITE) && (inventory[INVEN_LITE].sval == SV_LITE_NOLDOR) )
      {
         max_fuel *= 3;
      }

      /* Comment */
      if (j_ptr->p1val >= max_fuel)
      {
          j_ptr->p1val = max_fuel;
          msg_print("Your lamp is full.");
      }

      /* Decrease the item (from the pack) */
      if ( (i_ptr->tval == TV_LITE) &&
          ( (i_ptr->sval == SV_LITE_LANTERN) || (i_ptr->sval == SV_LITE_NOLDOR)))
      {
         /* jk - if you refuel by using another lamp, it gets empty - */
         /* it doesn't disappear */
         i_ptr->p1val = 0;
      }
      else
      {
         item_increase(item, -amt, px, py);
      }

      item_describe(item, px, py);
      item_optimize(item, px, py);

      /* Recalculate torch */
      p_ptr->update |= (PU_TORCH);
   }
}

/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(object_type *i_ptr)
{
   /* Torches are okay */
   if ((i_ptr->tval == TV_LITE) &&
       (i_ptr->sval == SV_LITE_TORCH)) return (TRUE);

   /* Assume not okay */
   return (FALSE);
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(void)
{
   s16b item;
   object_type *i_ptr = NULL;
   object_type *j_ptr;
/* jk */
   s16b amt;

   /* Restrict the choices */
   item_tester_hook = item_tester_refill_torch;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Refuel with which torch? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have no extra torches.");
      return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   if (amt>0)
   {
      /* Take a partial turn */
      energy_use = 50;

      /* Access the primary torch */
      j_ptr = &inventory[INVEN_LITE];

      /* Refuel */
      j_ptr->p1val += i_ptr->p1val*amt + 5;

      /* Message */
      msg_print("You combine the torches.");

      /* Over-fuel message */
      if (j_ptr->p1val >= FUEL_TORCH)
      {
          j_ptr->p1val = FUEL_TORCH;
          msg_print("Your torch is fully fueled.");
      }

      /* Refuel message */
      else
      {
          msg_print("Your torch glows more brightly.");
      }

      /* Decrease the item (from the pack) */
      item_increase(item, -amt, px, py);
      item_describe(item, px, py);
      item_optimize(item, px, py);

      /* Recalculate torch */
      p_ptr->update |= (PU_TORCH);
   }
}

/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
   object_type *i_ptr;

   /* Get the light */
   i_ptr = &inventory[INVEN_LITE];

   /* It is nothing */
   if (i_ptr->tval != TV_LITE)
   {
      msg_print("You are not wielding a light.");
   }

   /* It's a lamp */
   else if ((i_ptr->sval == SV_LITE_LANTERN) || (i_ptr->sval == SV_LITE_NOLDOR))
   {
      do_cmd_refill_lamp();
   }

   /* It's a torch */
   else if (i_ptr->sval == SV_LITE_TORCH)
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
   /* Set the target */
   if (target_set())
   {
      msg_print("Target Selected.");
   }
   else
   {
      msg_print("Target Aborted.");
   }
}

/*
 * Look at a monster
 */
static cptr look_mon_desc(s16b m_idx)
{
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   bool          living = TRUE;
   s16b          perc;

   /* Determine if the monster is "living" (vs "undead") */
   if (r_ptr->flags3 & RF3_UNDEAD) living = FALSE;
   if (r_ptr->flags3 & RF3_DEMON) living = FALSE;
   if (strchr("Egv", r_ptr->d_char)) living = FALSE;


   /* Healthy monsters */
   if (m_ptr->hp >= m_ptr->maxhp)
   {
      /* No damage */
      return (living ? "unhurt" : "undamaged");
   }

   /* Calculate a health "percentage" */
   perc = 100L * m_ptr->hp / m_ptr->maxhp;

   if (perc >= 60)
   {
      return (living ? "somewhat wounded" : "somewhat damaged");
   }

   if (perc >= 25)
   {
      return (living ? "wounded" : "damaged");
   }

   if (perc >= 10)
   {
      return (living ? "badly wounded" : "badly damaged");
   }

   return (living ? "almost dead" : "almost destroyed");
}

/*
 * describe trapped floor / door square
 */
static char do_cmd_look_aux_trap(bool floor, s16b x, s16b y)
{
   char            name[128];
   char            query;
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
   trap_type      *t_ptr = &t_info[first_trap(tr_ptr)];

dlog(DEBUGTRAPS,"cmd3.c: do_cmd_look_aux_trap: trapped %s found\n",
                   floor?"floor":"door");

   if (describe_trap(name, tr_ptr))
   {
      prt(format("You see %s%s. %s--space--",floor?"a floor ":"a door ",
                 name,t_ptr->ident?"[(r)emember] ":""), 0, MESSAGE_ROW);
dlog(DEBUGTRAPS,"cmd3.c: do_cmd_look_aux_trap: you see %s\n", name);
      move_cursor_relative(x,y);
      query = inkey();
      msg_print(NULL);
      if ((query=='r') && t_ptr->ident)
      {
         s16b  k;
         trap_type *t_ptr = &t_info[first_trap(tr_ptr)];
         Term_save();
         if (num_traps_ptr(tr_ptr,TRAP_EXISTS)==1)
         {
            prt(format("This is a trap you %s.",
                       knowstr[trap_experience(t_ptr)] ), 0, MESSAGE_ROW);
         }
         else
         {
            s16b num = num_traps_ptr(tr_ptr,TRAP_FOUND);
            s16b cur = 0;
            if (num>20)
            {
               msg_print("More than 20 traps - phew!");
            }
            else
            {
               s16b len = 0;
               /* get maximum line length */
               for (k = 0, cur = 0; k < num; k++, cur++)
               {
                  /* skip empty traps in this tr_ptr */
                  if (tr_ptr->type[cur]==0) continue;
                  /* skip unfound traps in this tr_ptr */
                  if (tr_ptr->found[cur] == FALSE) continue;

                  t_ptr = &t_info[tr_ptr->type[cur]];

                  sprintf(name,"%s",
                          format("%c %s - a trap you %s.",index_to_label(k),
                                 t_name+t_ptr->name,
                                 knowstr[trap_experience(t_ptr)]));
dlog(DEBUGTRAPS,"cmd3.c: do_cmd_look_aux_trap: phase 1 trap %d name %s\n",
                k, name);
                  if (strlen(name)>len) len=strlen(name);
               }
dlog(DEBUGTRAPS,"cmd3.c: do_cmd_look_aux_trap: phase 1 max len %d\n", len);
               if (len > 78) len = 78;

               /* now print the info, using this length */
               cur=0;
               for (k = 0, cur = 0; k < num; k++, cur++)
               {
                  /* skip empty traps in this tr_ptr */
                  if (tr_ptr->type[cur]==0) continue;
                  /* skip unfound traps in this tr_ptr */
                  if (tr_ptr->found[cur] == FALSE) continue;
                  t_ptr = &t_info[tr_ptr->type[k]];
                  prt(format("%c %s - a trap you %s.",index_to_label(k),
                             t_name+t_ptr->name,
                             knowstr[trap_experience(t_ptr)]),78-len,k+2);
dlog(DEBUGTRAPS,"cmd3.c: do_cmd_look_aux_trap: phase 2 trap %d\n", k);
                  cur++;
               }
            }
         }
         query=inkey();
         Term_load();
      }
      return (query);
   }
   else
   {
      prt("Error describing trap", 0, MESSAGE_ROW);
      return (0);
   }
}

/*
 * Assume the player is not blind or hallucinating.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * XXX XXX XXX We should allow the use of a "set target" command.
 *
 * XXX XXX XXX Note new terrain features for 2.8.0, and note that
 * eventually, we may allow objects and terrain features in the
 * same grid, or multiple objects per grid.
 *
 * We may assume that the grid is supposed to be "interesting".
 */
static s16b do_cmd_look_aux(s16b y, s16b x)
{
   cave_cell_type           *c_ptr = &dungeon.level[sublevel][y][x];

/* jk */
   s16b j;
/* cnt is the number of items on the floor */
   s16b cnt = 0;
   s16b index[MAX_FLOOR_ITEMS];

   monster_type        *m_ptr = &mn_list[c_ptr->m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];
   object_type         *i_ptr;

   cptr                s1 = "You see ", s2 = "";

   bool                prep = FALSE;

   s16b                mtyp = c_ptr->mtyp;
   s16b                styp = c_ptr->styp;

   s16b                query = ' ';

   char                m_name[80];
   char                i_name[80];
   char                out_val[160];

   msg_print(NULL);

   /* Hack -- looking under the player */
   if ((y == py) && (x == px))
   {
       s1 = "You are ";
       prep = TRUE;
   }

   /* Hack -- Convert secret doors to walls */
   if ((mtyp==DUNG_DOOR) && (styp==DUNG_DOOR_SECRET))
   {
      mtyp=DUNG_WALL;
      styp=DUNG_WALL_GRANITE;
   }

   /* Hack -- Convert invisible traps to floors */
   if ((mtyp==DUNG_FLOOR) && (styp==DUNG_FLOOR_TRAP))
   {
      styp=DUNG_FLOOR_NORMAL;
   }
   /* Actual monsters */
   if (c_ptr->m_idx && m_ptr->ml)
   {
      /* Get the monster name ("a kobold") */
      monster_desc(m_name, m_ptr, 0x08);

      /* Hack -- track this monster race */
      monster_race_track(m_ptr->r_idx);

      /* Hack -- health bar for this monster */
      health_track(c_ptr->m_idx);

      /* Hack -- handle stuff */
      handle_stuff();

      /* Describe, and prompt for recall */
      if (wizard && (r_ptr->flags2 & RF2_MULTIPLY))
      {
         sprintf(out_val,"%s%s%s (%s) [(r)ecall (l)ook]",
              s1, s2, m_name, look_mon_desc(c_ptr->m_idx));
      }
      else
      {
         sprintf(out_val, "%s%s%s (%s) [(r)ecall (l)ook]",
                 s1, s2, m_name, look_mon_desc(c_ptr->m_idx));
      }
      prt(out_val, 0, MESSAGE_ROW);

      /* Get a command */
      move_cursor_relative(x, y);
      query = inkey();

      /* Recall as needed */
      while (query == 'r')
      {
         /* Recall */
         Term_save();
         screen_roff(m_ptr->r_idx);
         Term_addstr(-1, TERM_WHITE, "  --space--");
         query = inkey();
         Term_load();

         /* Continue if desired */
         if (query != ' ') return (query);

         /* Get a new command */
         move_cursor_relative(x, y);
         query = inkey();
      }

      /* Continue if allowed */
      if ((query != ' ') && (query!='l')) return (query);

      /* Change the intro */
      s1 = "It is ";

      /* Hack -- take account of gender */
      if (r_ptr->flags1 & RF1_FEMALE)
         s1 = "She is ";
      else if (r_ptr->flags1 & RF1_MALE)
         s1 = "He is ";

      /* Use a preposition with objects */
      s2 = "on ";

      /* Use a preposition with terrain */
      prep = TRUE;

      if (query=='l')
      {
         if (mn_list[c_ptr->m_idx].has_drop)
         {
            s16b is_idx = item_set_this_monster(c_ptr->m_idx);
            s16b num    = items_in_set(is_idx);
            prt(format("%scarrying %d objects. [(l)ook]",s1,num),0,MESSAGE_ROW);
            query=inkey();
            if (query=='l')
            {
               Term_save();
               show_monster_inventory(is_idx);
               query = inkey();
               Term_load();
            }
            if (query != ' ') return (query);
         }
         else
         {
            monster_desc(m_name, m_ptr, 0x01);
            m_name[0]=FORCEUPPER(m_name[0]);
            prt(format("%s seems not to carry anything.       --space--",m_name),0,MESSAGE_ROW);
            query=inkey();
            if (query != ' ') return (query);
         }
      }

      /* Ignore floors */
      if ((mtyp==DUNG_FLOOR) && (styp==DUNG_FLOOR_NORMAL)) mtyp = 0;
   }

   /* Actual items */
   for (j = objects_on_floor(x,y)-1;j>=0;j--)
   {
      i_ptr=get_item_pointer_floor_xy(j,x,y);
      if (i_ptr->marked)
      {
         index[cnt++]=j;
      }
   }
   if (cnt==1)
   {
      cptr t_name;
      t_name = f_name + f_info[get_f_idx(mtyp, styp)].name;

      i_ptr = get_item_pointer_floor_xy(0,x,y);
      /* Obtain an object description */
      object_desc(i_name, i_ptr, TRUE, 3);

      /* Describe the object */
      sprintf(out_val, "%s%s%s on the %s.  --space--", s1, s2, i_name, t_name);
      prt(out_val, 0, MESSAGE_ROW);
      move_cursor_relative(x, y);
      query = inkey();
      if (query != ' ') return (query);
   }
   else if (cnt>1)
   {
      cptr t_name;
      t_name = f_name + f_info[get_f_idx(mtyp, styp)].name;

      prt(format("On the %s you see %d objects. [(l)ook]", t_name, cnt), 0, MESSAGE_ROW);
      move_cursor_relative(x, y);
      query=inkey();
      if (query=='l')
      {
         Term_save();
         show_floor(x,y);
         prt("The pile consists of:           --space--",0 , MESSAGE_ROW);
         query = inkey();
         Term_load();
      }
      if (query != ' ') return (query);
      /* Ignore floors */
      if ((mtyp=DUNG_FLOOR) && (styp==DUNG_FLOOR_NORMAL)) mtyp = 0;
   }

   if ((c_ptr->fdat && CAVE_MARK) && (mtyp==DUNG_TRAP))
   {
      query = do_cmd_look_aux_trap(TRUE, x, y);
      if (query != ' ') return(query);
   }

   /* seeing door trap */
dlog(DEBUGTRAPS,"cmd3.c: do_cmd_look_aux: testing for door trap @ %d,%d mtyp %d, traps found %d\n",
                mtyp, trap_found_xy(x, y, TRAP_FOUND));
   if (num_traps_xy(x, y, TRAP_FOUND) && (mtyp == DUNG_DOOR))
   {
      query = do_cmd_look_aux_trap(FALSE, x, y);
      if (query != ' ') return(query);
   }

   /* Describe terrain (if memorized) */
   if (mtyp && (c_ptr->fdat & CAVE_MARK))
   {
      cptr p1 = "";
      cptr p2 = "a ";
      cptr name;

      name = f_name + f_info[get_f_idx(mtyp, styp)].name;

      /* Pick a prefix */
      if (prep)
      {
         switch (c_ptr->mtyp)
         {
            case DUNG_DOOR:
            case DUNG_WALL:
            case DUNG_PERWALL: p1 = "in ";
                 break;

            case DUNG_SHRUB : p1 = "under ";
                 break;

            case DUNG_TRAP:
            case DUNG_NOTHING:
            case DUNG_FLOOR:
            case DUNG_ENTR:
            case DUNG_WILDN:
            case DUNG_WATER:
            case DUNG_STAIR: p1 = "on ";
                 break;
         }
      }
#if 0
      else
      {
         if (c_ptr->mtyp==DUNG_SHRUB)
         {
            s1 = "You are under ";
            p2 = "a ";
         }
      }
#endif

      /* Note leading vowel */
      if (is_a_vowel(name[0])) p2 = "an ";

      /* Hack -- store doors */
      if (mtyp==DUNG_ENTR) p2 = "the entrance to the ";

      /* no "you see a lava" please */
      if (mtyp == DUNG_LAVA) p2 = "";

      /* Display a message */
      sprintf(out_val, "%s%s%s%s. --space--", s1, p1, p2, name);
      prt(out_val, 0, MESSAGE_ROW);
      move_cursor_relative(x, y);
      query = inkey();

      /* Continue if allowed */
      if (query != ' ') return (query);
   }

   /* Keep going */
   return (query);
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool look_accept(s16b x, s16b y, bool find_terrain)
{
   cave_cell_type *c_ptr;
/* jk */
   s16b j;

   /* Examine the grid */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Visible monsters */
   if (c_ptr->m_idx)
   {
       monster_type *m_ptr = &mn_list[c_ptr->m_idx];

       /* Visible monsters */
       if (m_ptr->ml)
       {
          return (TRUE);
       }
   }

   /* Objects */
/* jk */
   for (j = objects_on_floor(x,y)-1;j>=0;j--)
   {
      object_type *i_ptr = get_item_pointer_floor_xy(j,x,y);
      /* Memorized object */

      if (i_ptr->marked) return (TRUE);
   }

   /* Always say something about shops and signs */

   if ((c_ptr->mtyp == DUNG_ENTR) || (c_ptr->mtyp == DUNG_SIGN)) return(TRUE);

   if (c_ptr->mtyp == DUNG_TRAP) return (TRUE);

   if ((c_ptr->mtyp == DUNG_DOOR) && (num_traps_xy(x, y, TRAP_FOUND)>0))
   {
      return (TRUE);
   }
   if (known_treasure(x, y)) return (TRUE);

   /* Memorized features (no floors) */
   if (find_terrain)
   {
      return (TRUE);
   }
   /* Nope */
   return (FALSE);
}

/*
 * A new "look" command, similar to the "target" command.
 */
void do_cmd_look(void)
{
   s16b        i, d, m;
   bool        done = FALSE;
   char        query;

   /* Blind */
   if (p_ptr->blind)
   {
      msg_print("You can't see a damn thing!");
      return;
   }

   /* Hallucinating */
   if (p_ptr->image)
   {
      msg_print("You can't believe what you are seeing!");
      return;
   }

   /* Reset "temp" array */
   temp_n = 0;

   /* Collect viewable grids */
   for (i = 0; i < view_n; i++)
   {
      s16b x = GRID_X(view_g[i]);
      s16b y = GRID_Y(view_g[i]);

      /* Skip off-screen locations */
      if (!panel_contains(x,y)) continue;

      /* Skip invalid locations */
      if (!look_accept(x,y, view_all_squares)) continue;

      /* Save the location */
      temp_x[temp_n] = x;
      temp_y[temp_n] = y;
      temp_n++;
   }

   /* Nothing to see */
   if (!temp_n)
   {
      msg_print("You see nothing special.");
      return;
   }

   /* Set the sort hooks */
   ang_sort_comp = ang_sort_comp_distance;
   ang_sort_swap = ang_sort_swap_distance;

   /* Sort the positions */
   ang_sort(temp_x, temp_y, temp_n);

   /* Start near the player */
   m = 0;

   /* Interact */
   while (!done)
   {
      msg_print(NULL);
      query = do_cmd_look_aux(temp_y[m], temp_x[m]);

      /* Assume no "direction" */
      d = 0;

      /* Analyze (non "recall") command */
      switch (query)
      {
         case ESCAPE:
         case 'q':
             done = TRUE;
             break;

         case ' ':
             if (++m == temp_n) m = 0;
             break;

         case '-':
             if (m-- == 0) m = temp_n - 1;
             break;

         case '1': case 'b': d = 1; break;
         case '2': case 'j': d = 2; break;
         case '3': case 'n': d = 3; break;
         case '4': case 'h': d = 4; break;
         case '6': case 'l': d = 6; break;
         case '7': case 'y': d = 7; break;
         case '8': case 'k': d = 8; break;
         case '9': case 'u': d = 9; break;

         default:
             bell("Unrecognized key");
      }

      /* Hack -- move around */
      if (d)
      {
         /* Find a new grid if possible */
         i = target_pick(temp_x[m], temp_y[m], ddx[d], ddy[d]);

         /* Use that grid */
         if (i >= 0) m = i;
      }
   }

   /* Clear the prompt */
   prt("", 0, MESSAGE_ROW);
}

/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate()
{
   s16b    dir, x1, y1, x2, y2;
   char    tmp_val[80];
   char    out_val[160];

   bool    old_scroll_panels = smooth_scroll_panels;

   /*
    * If smooth scrolling is turned on, we must choose a valid
    * p_ptr->wy and p_ptr->wx. We must also turn off smooth
    * scrolling because panel_bounds() does not use p_ptr->wy
    * or p_ptr->wx when smooth scrolling is in use.
    */
   if (smooth_scroll_panels)
   {
      /* Choose a panel col */
      p_ptr->wx = ((px - SCREEN_WID / 4) / (SCREEN_WID / 2));
      if (p_ptr->wx > panel_max_cols) p_ptr->wx = panel_max_cols;
      else if (p_ptr->wx < 0) p_ptr->wx = 0;

      /* Choose a panel row */
      p_ptr->wy = ((py - SCREEN_HGT / 4) / (SCREEN_HGT / 2));
      if (p_ptr->wy > panel_max_rows) p_ptr->wy = panel_max_rows;
      else if (p_ptr->wy < 0) p_ptr->wy = 0;

      /* Turn off smooth scrolling */
      smooth_scroll_panels = FALSE;
   }

   /* Start at current panel */
   x2 = x1 = p_ptr->wx;
   y2 = y1 = p_ptr->wy;

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
              x2, y2, tmp_val);

      /* Assume no direction */
      dir = 0;

      /* Get a direction */
      while (!dir)
      {
         char command;

         /* Get a command (or Cancel) */
         if (!get_com(out_val, &command)) break;

         /* Analyze the keypress */
         switch (command)
         {
            /* Convert roguelike directions */
            case 'B': case 'b': case '1': dir = 1; break;
            case 'J': case 'j': case '2': dir = 2; break;
            case 'N': case 'n': case '3': dir = 3; break;
            case 'H': case 'h': case '4': dir = 4; break;
            case 'L': case 'l': case '6': dir = 6; break;
            case 'Y': case 'y': case '7': dir = 7; break;
            case 'K': case 'k': case '8': dir = 8; break;
            case 'U': case 'u': case '9': dir = 9; break;
         }

         /* Error */
         if (!dir) bell("Unrecognized key");
      }

      /* No direction */
      if (!dir) break;

      /* Apply the motion */
      y2 += ddy[dir];
      x2 += ddx[dir];

      /* Verify the row */
      if (y2 > panel_max_rows) y2 = panel_max_rows;
      else if (y2 < 0) y2 = 0;

      /* Verify the col */
      if (x2 > panel_max_cols) x2 = panel_max_cols;
      else if (x2 < 0) x2 = 0;

      /* Handle "changes" */
      if ((y2 != p_ptr->wy) || (x2 != p_ptr->wx))
      {
         /* Save the new panel info */
         p_ptr->wy = y2;
         p_ptr->wx = x2;

         /* Recalculate the boundaries */
         panel_bounds();

         /* Update stuff */
         p_ptr->update |= (PU_MONSTERS);

         /* Redraw map */
         p_ptr->redraw1 |= (PR1_MAP);

         /* Handle stuff */
         handle_stuff();
      }
   }

   /* Restore smooth scrolling */
   smooth_scroll_panels = old_scroll_panels;

   /* Recenter the map around the player */
   verify_panel();

   /* Update stuff */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

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
   "&:A corpse",
   "':An open door",
   "(:Soft armor",
   "):A shield",
   "*:a pile of items",
   "+:A closed door",
   ",:Food (or mushroom patch)",
   "-:A wand (or rod)",
   ".:Floor",
   "/:A polearm (Axe/Pike/etc)",
       /* "0:unused", */
   "1:Entrance to General Store",
   "2:Entrance to Armory",
   "3:Entrance to Weaponsmith",
   "4:Entrance to Temple",
   "5:Entrance to Alchemy shop",
   "6:Entrance to Magic store",
   "7:Entrance to Black Market",
   "8:Entrance to your home",
       /* "9:unused", */
   "::Rubble",
   ";:A glyph of warding",
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
   "l:Louse",
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
   "w:Worm/Worm-Mass/Dragon-Worm",
       /* "x:unused", */
   "y:Yeek",
   "z:Zombie/Mummy",
   "{:A missile (arrow/bolt/shot)",
   "|:An edged weapon (sword/dagger/etc)",
   "}:A launcher (bow/crossbow/sling)",
   "~:A tool (or miscellaneous item)",
   NULL
};

/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
static bool ang_sort_comp_hook(vptr u, vptr v, s16b a, s16b b)
{
   u16b *who = (u16b*)(u);

   u16b *why = (u16b*)(v);

   s16b w1 = who[a];
   s16b w2 = who[b];

   s16b z1, z2;


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
static void ang_sort_swap_hook(vptr u, vptr v, s16b a, s16b b)
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
static void roff_top(s16b r_idx)
{
   monster_race        *r_ptr = &r_info[r_idx];

   byte                a1, a2;
   char                c1, c2;


   /* Access the chars */
   c1 = r_ptr->d_char;
   c2 = r_ptr->x_char;

   /* Assume white */
   a1 = TERM_WHITE;
   a2 = TERM_WHITE;

#ifdef USE_COLOR

   /* Access the attrs */
   if (use_color)
   {
      a1 = r_ptr->d_attr;
      a2 = r_ptr->x_attr;
   }

#endif


   /* Clear the top line */
   Term_erase(0, 0, 80);

   /* Reset the cursor */
   Term_gotoxy(0, 0);

   /* A title (use "The" for non-uniques) */
   if (!(r_ptr->flags1 & RF1_UNIQUE))
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
   s16b         i, n, r_idx;
   char        sym, query;
   char        buf[128];
/* jk */
   char        searchname[80]; /* to hold name to search for in */
   bool        search = FALSE;

   bool        all = FALSE;
   bool        uniq = FALSE;
   bool        norm = FALSE;

   bool        recall = FALSE;

   u16b        why = 0;
   u16b        who[r_number];


   /* Get a character, or abort */
   if (!get_com("Identify what character or ^A(ll)/^N(on-uniques)/^U(niques)/^S(earch): ", &sym)) return;

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
/* jk - searching on */
   else if (sym == KTRL('S'))
   {
      search = TRUE;
      strcpy(searchname,"");
      prt("Enter (part of) name to search for:",1,0);
      askfor_aux(searchname,40);
      sprintf(buf,"Searching for \"%s\"",searchname);
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
   prt(buf, 0, MESSAGE_ROW);

   /* Collect matching monsters */
   for (n = 0, i = 1; i < r_number; i++)
   {
      monster_race *r_ptr = &r_info[i];

      /* Nothing to recall */
      if (!cheat_know && !r_ptr->r_sights) continue;

      /* Require non-unique monsters if needed */
      if (norm && (r_ptr->flags1 & RF1_UNIQUE)) continue;

      /* Require unique monsters if needed */
      if (uniq && !(r_ptr->flags1 & RF1_UNIQUE)) continue;

      /* Collect "appropriate" monsters */
      if (search)
      {
         if (cmp_strngs(r_name + r_ptr->name,searchname)==TRUE) who[n++]=i;
      }
      else if (all || (r_ptr->d_char == sym)) who[n++] = i;
   }

   /* Nothing to recall */
   if (!n) return;


   /* Prompt XXX XXX XXX */
   put_str("Recall details? (k/p/y/n): ", 40, MESSAGE_ROW);

   /* Query */
   query = inkey();

   /* Restore */
   prt(buf, 0, MESSAGE_ROW);


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
   if (query != 'y') return;


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

   /* Scan the monster memory. */
   while ((0 <= i) && (i <= n - 1))
   {
      /* Validate index */
      for (i = i % n; i < 0; i += n) ;

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
            Term_save();

            /* Recall on screen */
            screen_roff(who[i]);

            /* Hack -- Complete the prompt (again) */
            Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
         }

         /* Command */
         query = inkey();

         /* Unrecall */
         if (recall)
         {
            /* Restore */
            Term_load();
         }

         /* Normal commands */
         if (query != 'r') break;

         /* Toggle recall */
         recall = !recall;
      }

      /* Stop scanning */
      if (query == ESCAPE) break;

      /* Move to the "next" or "prev" monster */
      i = i + ((query == '-') ? 1 : -1);
   }

   /* Re-display the identity */
   prt(buf, 0, MESSAGE_ROW);
}

extern void do_cmd_change_tactic(bool up)
{
   char c;
   if (up)
   {
      p_ptr->tactic++;
      if (p_ptr->tactic>8) p_ptr->tactic = 0;
   }
   else
   {
      if (p_ptr->tactic==0) p_ptr->tactic = 9;
      p_ptr->tactic--;
   }
   prt(format("During your adventures in Angband, you behave %s. -- more --",
       tactic_info[p_ptr->tactic].name),0,MESSAGE_ROW);
   c = inkey();
   p_ptr->update |= (PU_BONUS);
   update_stuff();
   prt("",0,MESSAGE_ROW);
}

extern void do_cmd_change_movement(bool up)
{
   char c;
   if (up)
   {
      p_ptr->movement++;
      if (p_ptr->movement>8) p_ptr->movement = 0;
   }
   else
   {
      if (p_ptr->movement==0) p_ptr->movement = 9;
      p_ptr->movement--;
   }
   prt(format("During your adventures in Angband, you explore %s. -- more --",
              move_info[p_ptr->movement].name),0,MESSAGE_ROW);
   c = inkey();
   p_ptr->update |= (PU_BONUS);
   update_stuff();
   prt("",0,MESSAGE_ROW);
}

