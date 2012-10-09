/* File: object2.c */

/* Purpose: misc code for objects */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Apply a "object restriction function" to the "object allocation table"
 *
 * code by Leon Marrick, copied from oangband
 */
errr get_obj_num_prep(void)
{
   s16b i, total = 0;

   /* Get the entry */
   alloc_entry *table = alloc_kind_table;

   /* Scan the allocation table */
   for (i = 0; i < alloc_kind_size; i++)
   {
      /* Accept objects which pass the restriction, if any */
      if (!get_obj_num_hook || (*get_obj_num_hook)(table[i].index))
      {
         /* Accept this object */
         table[i].prob2 = table[i].prob1;
         total++;
      }

      /* Do not use this object */
      else
      {
         /* Decline this object */
         table[i].prob2 = 0;
      }
   }
dlog(DEBUGITEMS,"object2.c: get_obj_num_prep: accepted %d of %d\n", total, alloc_kind_size);

   /* Success */
   return (0);
}

/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 *
 * code by Leon Marrick, copied from oangband
 *
 * This function uses the "allocation table" built in "init.c".
 *
 * There is a small chance (1/20) of "boosting" the given depth by
 * a potentially large amount (see below).
 *
 * It is (slightly) more likely to acquire an object of the given level
 * than one of a lower level.  This is done by choosing several objects
 * appropriate to the given level and keeping the "hardest" one.
 *
 * XXX XXX XXX Note that this function may fail (very rarely).
 */
s16b get_obj_num(s16b level)
{
   s16b i, j, p;

   s16b k_idx;

   long value, total;

   object_kind *k_ptr;

   alloc_entry *table = alloc_kind_table;

   /* Boost level */
   if (level > 0)
   {
      /* Occasional "boost" */
      if (rand_int(GREAT_OBJ) == 0)
      {
         /* What a bizarre calculation */
         level = 1 + (level * MAX_LEVEL / randint(MAX_LEVEL));
      }
   }

   /* Reset total */
   total = 0L;
dlog(DEBUGITEMS,"object2.c: get_obj_num: level %d\n", level);
   /* Process probabilities */
   for (i = 0; i < alloc_kind_size; i++)
   {
      /* Objects are sorted by depth */
      if (table[i].level > level) break;

      /* Default */
      table[i].prob3 = 0;

      /* Access the index */
      k_idx = table[i].index;

      /* Access the actual kind */
      k_ptr = &k_info[k_idx];

      /* Hack -- prevent embedded chests */
      if (opening_chest && (k_ptr->tval == TV_CHEST)) continue;

      /* Accept */
      table[i].prob3 = table[i].prob2;

      /* Total */
      total += table[i].prob3;
   }
dlog(DEBUGITEMS,"object2.c: get_obj_num: total %d, tested %d of %d\n", total, i, alloc_kind_size);

   /* No legal objects */
   if (total <= 0) return (0);

   /* Pick an object */
   value = rand_int(total);

   /* Find the object */
   for (i = 0; i < alloc_kind_size; i++)
   {
      /* Found the entry */
      if (value < table[i].prob3) break;

      /* Decrement */
      value = value - table[i].prob3;
   }


   /* Power boost */
   p = rand_int(100);

   /* Hack -- chests should have decent stuff, and chests should themselves
    * be decent, so we guarantee two more tries for better objects. -LM-
    */
   if ((opening_chest) || (required_tval == TV_CHEST))  p = 0;

   /* Try for a "better" object once (60%) or twice (10%) */
   if (p < 60)
   {
      /* Save old */
      j = i;

      /* Pick a object */
      value = rand_int(total);

      /* Find the monster */
      for (i = 0; i < alloc_kind_size; i++)
      {
         /* Found the entry */
         if (value < table[i].prob3) break;

         /* Decrement */
         value = value - table[i].prob3;
      }

      /* Keep the "best" one */
      if (table[i].level < table[j].level) i = j;
   }

   /* Try for a "better" object twice (10%) */
   if (p < 10)
   {
      /* Save old */
      j = i;

      /* Pick a object */
      value = rand_int(total);

      /* Find the object */
      for (i = 0; i < alloc_kind_size; i++)
      {
         /* Found the entry */
         if (value < table[i].prob3) break;

         /* Decrement */
         value = value - table[i].prob3;
      }

      /* Keep the "best" one */
      if (table[i].level < table[j].level) i = j;
   }

   /* Result */
   return (table[i].index);
}

/*
 * Known is true when the "attributes" of an object are "known".
 * These include tohit, todam, toac, cost, and p1val (charges).
 *
 * Note that "knowing" an object gives you everything that an "awareness"
 * gives you, and much more.  In fact, the player is always "aware" of any
 * item of which he has full "knowledge".
 *
 * But having full knowledge of, say, one "wand of wonder", does not, by
 * itself, give you knowledge, or even awareness, of other "wands of wonder".
 * It happens that most "identify" routines (including "buying from a shop")
 * will make the player "aware" of the object as well as fully "know" it.
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void object_known(object_type *i_ptr)
{
   if (i_ptr->name1)
   {
dlog(DEBUGITEMS,"object2.c: object_known: getting to know artifact %d (%s)\n",
                 i_ptr->name1, a_name + a_info[i_ptr->name1].name);
   }
   /* Remove "default inscriptions" */
   if (i_ptr->note && (i_ptr->ident & ID_SENSE))
   {
      /* Access the inscription */
      cptr q = quark_str(i_ptr->note);

      /* Hack -- Remove auto-inscriptions */
      if ((streq(q, "cursed")) ||
          (streq(q, "broken")) ||
          (streq(q, "good")) ||
          (streq(q, "average")) ||
          (streq(q, "excellent")) ||
          (streq(q, "worthless")) ||
          (streq(q, "special")) ||
          (streq(q, "terrible")))
      {
         /* Forget the inscription */
         i_ptr->note = 0;
      }
   }

   /* Clear the "Felt" info */
   i_ptr->ident &= ~ID_SENSE;

   /* Clear the "Empty" info */
   i_ptr->ident &= ~ID_EMPTY;

   /* Now we know about the item */
   i_ptr->ident |= ID_KNOWN;

   /* remember artifacts especially */
   if (i_ptr->name1) a_info[i_ptr->name1].ident |= ID_KNOWN;
}

/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *i_ptr)
{
   /* Fully aware of the effects */
   k_info[i_ptr->k_idx].aware = TRUE;
}

bool need_tries(object_type *i_ptr)
{
   switch(i_ptr->tval)
   {
      case TV_SHOT:
      case TV_BOLT:
      case TV_ARROW:
      case TV_BOW:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_SWORD:
      case TV_DIGGING:
      case TV_BOOTS:
      case TV_GLOVES:
      case TV_CLOAK:
      case TV_CROWN:
      case TV_HELM:
      case TV_SHIELD:
      case TV_SOFT_ARMOR:
      case TV_HARD_ARMOR:
      case TV_DRAG_ARMOR:
          return (TRUE);
          break;
   }
   return(FALSE);
}

/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static cptr value_check_aux1(object_type *i_ptr)
{
   /* Artifacts */
   if (artifact_p(i_ptr))
   {
      /* Cursed/Broken */
      if (cursed_p(i_ptr) || broken_p(i_ptr)) return "terrible";

      /* Normal */
      return "special";
   }

   /* Ego-Items */
   if (ego_item_p(i_ptr))
   {
      /* Cursed/Broken */
      if (cursed_p(i_ptr) || broken_p(i_ptr)) return "worthless";

      /* Normal */
      return "excellent";
   }

   /* Cursed items */
   if (cursed_p(i_ptr)) return "cursed";

   /* Broken items */
   if (broken_p(i_ptr)) return "broken";

   /* Good "armor" bonus */
   if (i_ptr->to_a > 0) return "good";

   /* Good "weapon" bonus */
   if (i_ptr->to_h + i_ptr->to_d > 0) return "good";

   /* Default to "average" */
   return "average";
}

/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static cptr value_check_aux2(object_type *i_ptr)
{
   /* Cursed items (all of them) */
   if (cursed_p(i_ptr)) return "cursed";

   /* Broken items (all of them) */
   if (broken_p(i_ptr)) return "broken";

   /* Artifacts -- except cursed/broken ones */
   if (artifact_p(i_ptr)) return "good";

   /* Ego-Items -- except cursed/broken ones */
   if (ego_item_p(i_ptr)) return "good";

   /* Good armor bonus */
   if (i_ptr->to_a > 0) return "good";

   /* Good weapon bonuses */
   if (i_ptr->to_h + i_ptr->to_d > 0) return "good";

   /* No feeling */
   return (NULL);
}

/*
 * Sense the inventory
 *
 *   Class 0 = Warrior --> fast and heavy
 *   Class 1 = Mage    --> slow and light
 *   Class 2 = Priest  --> fast but light
 *   Class 3 = Rogue   --> okay and heavy
 *   Class 4 = Ranger  --> slow and light
 *   Class 5 = Paladin --> slow but heavy
 */
void sense_inventory(void)
{
   s16b         i;
   s16b         plev = p_ptr->lev;
   bool         heavy = FALSE;
   cptr         feel;
   object_type *i_ptr;

   char         i_name[80];

   /*** Check for "sensing" ***/

   /* No sensing when confused */
   if (p_ptr->confused) return;

   /* Analyze the class */
   switch (p_ptr->pclass)
   {
      /* Warriors & Gladiators */
      case CLASS_GLADIATR:
      case CLASS_WARRIOR:

         /* Good sensing */
         if (0 != rand_int(9000L / (plev * plev + 40))) return;
         heavy = TRUE; /* Heavy sensing */
         break;

      /* Mages */
      case CLASS_MAGE:

         /* Very bad (light) sensing */
         if (0 != rand_int(240000L / (plev + 5))) return;

         /* Done */
         break;

      /* Priests */
      case CLASS_PRIEST:

         /* Good (light) sensing */
         if (0 != rand_int(10000L / (plev * plev + 40))) return;

         /* Done */
         break;

      /* Rogues */
      case CLASS_ROGUE:

         /* Okay sensing */
         if (0 != rand_int(20000L / (plev * plev + 40))) return;

         /* Heavy sensing */
         heavy = TRUE;

         /* Done */
         break;

      /* Rangers */
      case CLASS_RANGER:

         /* Very bad (light) sensing */
         if (0 != rand_int(120000L / (plev + 5))) return;

         /* Done */
         break;

      /* Paladins */
      case CLASS_PALADIN:

         /* Bad sensing */
         if (0 != rand_int(80000L / (plev * plev + 40))) return;

         /* Heavy sensing */
         heavy = TRUE;

         /* Done */
         break;

      /* War Mage and High Priests */
      case CLASS_HIGHPRST:
      case CLASS_WARMAGE:

         /* Good sensing, but slow */
         if (0 != rand_int(90000L / (plev * plev + 40))) return;
         heavy = TRUE; /* Heavy sensing */
         break;
   }

   /*** Sense everything ***/

   /* Check everything */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      bool okay = FALSE;

      i_ptr = &inventory[i];

      /* Skip empty slots */
      if (!i_ptr->k_idx) continue;

      /* if it isn't tried yet, do that first */
dlog(DEBUGITEMS,"object2.c: sense_inventory: need_tries %d ident %016x ident & ID_EQUIPTESTED %016x\n",
                need_tries(i_ptr), i_ptr->ident, i_ptr->ident & ID_EQUIPTESTED);

      if (need_tries(i_ptr) && !(i_ptr->ident & ID_EQUIPTESTED) && (randint(20)==1))
      {
dlog(DEBUGITEMS,"object2.c: sense_inventory: about to call test_equipment\n");
         test_equipment(i_ptr, FALSE);
         return;
      }

      /* Valid "tval" codes */
      switch (i_ptr->tval)
      {
         case TV_SHOT:
         case TV_ARROW:
         case TV_BOLT:
         case TV_BOW:
         case TV_DIGGING:
         case TV_HAFTED:
         case TV_POLEARM:
         case TV_SWORD:
         case TV_BOOTS:
         case TV_GLOVES:
         case TV_HELM:
         case TV_CROWN:
         case TV_SHIELD:
         case TV_CLOAK:
         case TV_SOFT_ARMOR:
         case TV_HARD_ARMOR:
         case TV_DRAG_ARMOR:
            okay = TRUE;
            break;
      }

      /* Skip non-sense machines */
      if (!okay) continue;

      /* We know about it already, do not tell us again */
      if (i_ptr->ident & ID_SENSE) continue;

      /* It is fully known, no information needed */
      if (object_known_p(i_ptr)) continue;

      /* Occasional failure on inventory items */
      if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;


      /* Check for a feeling */
      feel = (heavy ? value_check_aux1(i_ptr) : value_check_aux2(i_ptr));

      /* Skip non-feelings */
      if (!feel) continue;

      /* Stop everything */
      if (disturb_other)
      {
         disturb(0, 0);
      }

      /* Get an object description */
      object_desc(i_name, i_ptr, FALSE, 0);

      /* Message (equipment) */
      if (i >= INVEN_WIELD)
      {
         msg_format("You feel the %s (%c) you are %s %s %s...",
                  i_name, index_to_label(i), describe_use(i),
                  ((i_ptr->number == 1) ? "is" : "are"), feel);
      }

      /* Message (inventory) */
      else
      {
         msg_format("You feel the %s (%c) in your pack %s %s...",
                  i_name, index_to_label(i),
                  ((i_ptr->number == 1) ? "is" : "are"), feel);
      }

      /* We have "felt" it */
      i_ptr->ident |= (ID_SENSE);

      /* Inscribe it textually */
      if (!i_ptr->note) i_ptr->note = quark_add(feel);

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP);

      /* Combine / Reorder the pack (later) */
      p_ptr->notice |= (PN_COMBINE | PN_REORDER);
   }
}


void test_equipment(object_type *i_ptr, bool easy)
{
   char iname1[80];
   s16b i, power;
   bool worn = FALSE;

   object_desc(iname1, i_ptr, FALSE, 0);
   if (!easy)
   {
/* we can differentiate between weapon and bow skills here */
      if (wield_slot(i_ptr)==INVEN_WIELD)
      {
         if (p_ptr->skill_thn < randint(50)) return;
      }
      else if (wield_slot(i_ptr)==INVEN_BOW)
      {
         if (p_ptr->skill_thn < randint(50)) return;
      }
      /* else it's armour */
   }

   i_ptr->ident |= ID_EQUIPTESTED;

   /* check if this item is worn or is in the inventory */
   for (i=INVEN_WIELD; i<INVEN_TOTAL; i++)
   {
      if (&inventory[i]==i_ptr) worn=TRUE;
   }

   switch(i_ptr->tval)
   {
      case TV_SHOT:
      case TV_BOLT:
      case TV_ARROW:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_SWORD:
      case TV_DIGGING:
         msg_format("You feel that the %s %s exact%s %dd%d damage.", iname1,
                    worn?"you are holding":"in your inventory",
                    i_ptr->number>1?"":"s",i_ptr->dd, i_ptr->ds);
         break;
      case TV_BOW:
         power = i_ptr->sval % 10;

         msg_format("You feel the %s %s does x%d damage.", iname1,
                    worn?"you are holding":"in your inventory",
                    power);
         break;

      case TV_BOOTS:
      case TV_GLOVES:
      case TV_CLOAK:
      case TV_CROWN:
      case TV_HELM:
      case TV_SHIELD:
      case TV_SOFT_ARMOR:
      case TV_HARD_ARMOR:
      case TV_DRAG_ARMOR:
         if (i_ptr->to_h!=0)
            msg_format("You feel the %s %s adds %d ac and %d to-hit.", iname1,
                        worn?"you are wearing":"in your inventory", i_ptr->ac, i_ptr->to_h);
         else
            msg_format("You feel the %s %s adds %d ac.", iname1,
                        worn?"you are wearing":"in your inventory", i_ptr->ac);
         break;
   }
   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);
}

void burn_light(void)
{
   object_type *i_ptr;

   /* Check for light being wielded */
   i_ptr = &inventory[INVEN_LITE];

   /* Burn some fuel in the current lite */
   if (i_ptr->tval == TV_LITE)
   {
      /* Hack -- Use some fuel (except on artifacts) */
      if (!artifact_p(i_ptr) && (i_ptr->p1val > 0))
      {
         /* Decrease life-span */
         i_ptr->p1val--;

         /* powerful lamps burn faster! */
         /* burning to -1 leads to endless loops in object2.c:object_desc_num() */
         if ((i_ptr->sval == SV_LITE_NOLDOR) && (i_ptr->p1val>1)) i_ptr->p1val -= 5;
         if (i_ptr->p1val < 0) i_ptr->p1val = 0;

         /* Hack -- notice interesting fuel steps */
         if ((i_ptr->p1val < 100) || (!(i_ptr->p1val % 100)))
         {
            /* Window stuff */
            p_ptr->window |= (PW_EQUIP);
         }

         /* Hack -- Special treatment when blind */
         if (p_ptr->blind)
         {
            /* Hack -- save some light for later */
            if (i_ptr->p1val == 0) i_ptr->p1val++;
         }

         /* The light is now out */
         else if (i_ptr->p1val == 0)
         {
             disturb(0, 0);
             msg_print("Your light has gone out!");
         }

         /* The light is getting dim */
         else if ((i_ptr->p1val < 100) && (!(i_ptr->p1val % 5)))
         {
            if (disturb_other)
            {
               disturb(0, 0);
            }
            msg_print("Your light is growing faint.");
         }
      }
   }

   /* Calculate torch radius */
   p_ptr->update |= (PU_TORCH);
}

/*
 * If player has inscribed the object with "!!", let him know when it's 
 * recharged. -LM-
 */
static void recharged_notice(object_type *i_ptr)
{
   char i_name[120];

   cptr s;

   /* No inscription */
   if (!i_ptr->note) return;

   /* Find a '!' */
   s = strchr(quark_str(i_ptr->note), '!');

   /* Process notification request. */
   while (s)
   {
      /* Find another '!' */
      if (s[1] == '!')
      {
         /* Describe (briefly) */
         object_desc(i_name, i_ptr, FALSE, 0);

         /* Notify the player */
         if (i_ptr->number > 1)    
         {
            msg_format("Your %s are recharged.", i_name);
         }
         else
         {
            msg_format("Your %s is recharged.", i_name);
         }

         /* Done. */
         return;
      }

      /* Keep looking for '!'s */
      s = strchr(s + 1, '!');
   }
}

/*
 * handle the recharging of stuff in the inventory
 */
void recharge_inventory(void)
{
   s16b         i, j;
   object_type *i_ptr;

   /* Note changes */
   j = 0;

   /* Process equipment */
   for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      /* Get the object */
      i_ptr = &inventory[i];

      /* Skip non-objects */
      if (!i_ptr->k_idx) continue;

      /* Recharge activatable objects */
      if (i_ptr->timeout > 0)
      {
         /* Recharge */

         i_ptr->timeout--;

         /* Notice changes */
         if (!(i_ptr->timeout))
         {
            recharged_notice(i_ptr);
            j++;
         }
      }
   }

   /* Recharge rods */
   for (i = 0; i < INVEN_PACK; i++)
   {
      i_ptr = &inventory[i];

/* note that staffs of the mage are only recharged when they are wielded */
/* and thus are not used here! */
      /* Examine all charging rods */
      if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val))
      {
         /* Charge it */
         i_ptr->p1val--;

         /* Notice changes */
         if (!(i_ptr->p1val))
         {
            recharged_notice(i_ptr);
            j++;
         }
      }
   }
/* jk - some special effects of gothmog here */
   i_ptr = &inventory[INVEN_WIELD];
   if ((i_ptr->name1==ART_GOTHMOG) && (rand_int(50)==1))
   {
/* 30 is the division between less and more damage to the inventory */
      fire_dam(rand_int(35),"The Black Lochaber Axe of Gothmog");
   }

   /* Notice changes */
   if (j)
   {
      /* Window stuff */
      p_ptr->window |= (PW_EQUIP);

      /* Combine pack */
      p_ptr->notice |= (PN_COMBINE);
   }
}

/*
 * Something has been "sampled"
 */
void object_tried(object_type *i_ptr)
{
   i_ptr->ident |= ID_EQUIPTESTED;
   k_info[i_ptr->k_idx].tried = TRUE;
}

bool has_spell(object_type *i_ptr, s16b i)
{
   s16b spell_set = (i / 16);
   s16b spell_bit = (i % 16);
   bool found = FALSE;

   if (!i_ptr->spell_set) return(FALSE);

   if ( (s_list[i_ptr->spell_set].spells[spell_set] & (1<<spell_bit) ) > 0)
   {
dlog(DEBUGITEMS,"object2.c: has_spell: i_ptr->spellset %d i %d looking @ set %d bit %d value %x looking for %x\n",
                i_ptr->spell_set, i, spell_set, spell_bit,
                s_list[i_ptr->spell_set].spells[spell_set], (1<<spell_bit));
      found = TRUE;
dlog(DEBUGITEMS,"object2.c: has_spell: %s found %d\n", s_name + s_info[i].name, found);
   }
   return found;
}

void set_spell(object_type *i_ptr, s16b i)
{
dlog(DEBUGITEMS,"object2.c: set_spell: tv %d sv %d pv %ld spell %d\n",
                i_ptr->tval, i_ptr->sval, i_ptr->p1val, i);
   if (i_ptr->spell_set == 0)
   {
      i_ptr->spell_set=s_pop();

dlog(DEBUGITEMS,"object2.c: set_spell: adding spell_set %d\n", i_ptr->spell_set);

      if (!i_ptr->spell_set)
      {
         quit("Error: not enough spell_sets available - check MAX_S_IDX in defines.h");
      }
      s_list[i_ptr->spell_set].inuse=TRUE;

dlog(DEBUGITEMS,"object2.c: set_spell: spell_set %d inuse TRUE\n", i_ptr->spell_set);

   }

dlog(DEBUGITEMS,"object2.c: set_spell: before set %d spells[%d] %016x flag %016x\n",
                i_ptr->spell_set, i/16, s_list[i_ptr->spell_set].spells[i/16],
                (i<<(i % 16)));

   s_list[i_ptr->spell_set].spells[i / 16] |= (1<<(i % 16));

dlog(DEBUGITEMS,"object2.c: set_spell: after         spells[%d] %016x\n",
                i/16, s_list[i_ptr->spell_set].spells[i/16]);

}

void clr_spell(object_type *i_ptr, s16b i)
{
   s16b j;
   bool still_used = FALSE;

   if (!i_ptr->spell_set)
      quit("Error: trying to clear a spell from an item without spells!");
   s_list[i_ptr->spell_set].spells[i / 16] &= ~(1<<(i % 16));
   for (j=0; j<MAX_SPELLS_PER_ITEM/16+1; j++)
   {
      if (s_list[i_ptr->spell_set].spells[j]!=0) still_used = TRUE;
   }
   if (!still_used)
   {
dlog(DEBUGITEMS,"object2.c: clr_spell: wiping last spell %d in item_set %d\n",
                i, i_ptr->spell_set);
      s_list[i_ptr->spell_set].inuse = FALSE;
      i_ptr->spell_set = 0;
   }
}

/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(object_type *i_ptr)
{
   object_kind *k_ptr = &k_info[i_ptr->k_idx];

   /* Aware item -- use template cost */
   if (object_aware_p(i_ptr)) return (k_ptr->cost);

   /* Analyze the type */
   switch (i_ptr->tval)
   {
       /* Un-aware Food */
       case TV_FOOD: return (5L);

       /* Un-aware Potions */
       case TV_POTION: return (20L);

       /* Un-aware Scrolls */
       case TV_SCROLL: return (20L);

       /* Un-aware Staffs */
       case TV_STAFF: return (70L);

       /* Un-aware Wands */
       case TV_SPELL:
       case TV_WAND: return (50L);

       /* Un-aware Rods */
       case TV_ROD: return (90L);

       /* Un-aware Rings */
       case TV_RING: return (45L);

       /* Un-aware Amulets */
       case TV_AMULET: return (45L);
   }

   /* Paranoia -- Oops */
   return (0L);
}

/*
 * this computes the value of an to_a bonus in items
 * this can be called for known or tried items, not for unknown!
 */
static s32b armor_bonus_cost(object_type *i_ptr)
{
   s16b item_cost = 10L;
   object_kind *k_ptr = &k_info[i_ptr->k_idx];
   s16b ac_cost = 10L;
   s32b value = 0L;

   /* first determine the item base cost */
   if (i_ptr->name1 != 0)
   {
      item_cost += a_info[i_ptr->name1].cost;
   }
   if (i_ptr->name2 != 0)
   {
      item_cost += e_info[i_ptr->name2].cost;
   }
   else
   {
      item_cost += k_ptr->cost;
   }
   /* then determine the cost per bonus point */
   /* ac_cost can vary between 10 and 500 */
   if (k_ptr->ac>0)
   {
      ac_cost = min(500,max(10, item_cost / k_ptr->ac));
   }
   else /* crowns have base AC of 0 */
   {
      ac_cost=500;
   }

   /* Give credit for bonuses */
   /* an armour with k_ptr->cost 1000, i_ptr->to_a -4 and k_ptr->ac 14 would give */
   /* value += 2*-4*1000/14 -> += -570 which seems fair                           */
   value +=  2 * i_ptr->to_a * ac_cost;

   return (value);
}

/*
 * Return the "real" price of a "known" item, not including discounts
 *
 * Wand and staffs get cost for each charge
 *
 * Armor is worth an extra 100 gold per bonus point to armor class.
 *
 * Weapons are worth an extra 100 gold per bonus point (AC,TH,TD).
 *
 * Missiles are only worth 5 gold per bonus point, since they
 * usually appear in groups of 20, and we want the player to get
 * the same amount of cash for any "equivalent" item.  Note that
 * missiles never have any of the "p1val" flags, and in fact, they
 * only have a few of the available flags, primarily of the "slay"
 * and "brand" and "ignore" variety.
 *
 * Armor with a negative armor bonus is worthless.
 * Weapons with negative hit+damage bonuses are worthless.
 *
 * Every wearable item with a "p1val" bonus is worth extra (see below).
 */
s32b object_value_real(object_type *i_ptr)
{
   s32b value;

   u64b f1, f2, f3;

   object_kind *k_ptr = &k_info[i_ptr->k_idx];

   /* Base cost */
   value = k_ptr->cost;

   /* Extract some flags */
   object_flags(i_ptr, &f1, &f2, &f3);

   /* Artifact */
   if (i_ptr->name1)
   {
       artifact_type *a_ptr = &a_info[i_ptr->name1];

       /* Hack -- Use the artifact cost instead */
       value = a_ptr->cost;
   }

   /* Ego-Item */
   else if (i_ptr->name2)
   {
       ego_item_type *e_ptr = &e_info[i_ptr->name2];

       /* Hack -- Reward the ego-item with a bonus */
       value += e_ptr->cost;
   }

   /* Analyze p1val bonus */
   switch (i_ptr->tval)
   {
       case TV_SHOT:
       case TV_ARROW:
       case TV_BOLT:
       case TV_BOW:
       case TV_DIGGING:
       case TV_HAFTED:
       case TV_POLEARM:
       case TV_SWORD:
       case TV_BOOTS:
       case TV_GLOVES:
       case TV_HELM:
       case TV_CROWN:
       case TV_SHIELD:
       case TV_CLOAK:
       case TV_SOFT_ARMOR:
       case TV_HARD_ARMOR:
       case TV_DRAG_ARMOR:
       case TV_LITE:
       case TV_AMULET:
       case TV_RING:

           /* p1val */
           /* Give credit for stat bonuses */
           if (f1 & TR1_STR1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);
           if (f1 & TR1_INT1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);
           if (f1 & TR1_WIS1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);
           if (f1 & TR1_DEX1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);
           if (f1 & TR1_CON1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);
           if (f1 & TR1_CHR1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);

           /* Give credit for stealth and searching */
           if (f1 & TR1_STEALTH1) value += ((i_ptr->p1val + i_ptr->p2val) * 100L);
           if (f1 & TR1_SEARCH1) value += ((i_ptr->p1val + i_ptr->p2val) * 100L);

           /* Give credit for infra-vision and tunneling */
           if (f1 & TR1_INFRA1) value += ((i_ptr->p1val + i_ptr->p2val) * 50L);
           if (f1 & TR1_TUNNEL1) value += ((i_ptr->p1val + i_ptr->p2val) * 50L);

           /* Give credit for extra attacks */
           if (f1 & TR1_BLOWS1) value += ((i_ptr->p1val + i_ptr->p2val) * 2000L);

           /* Give credit for extra lite */
           if (f1 & TR1_LITE1) value += ((i_ptr->p1val + i_ptr->p2val) * 200L);

           /* Give credit for vampirism */
           if (f1 & TR1_VAMPIRIC1) value += ((i_ptr->p1val + i_ptr->p2val) * 250L);

           /* Give credit for magic skill */
           if (f1 & TR1_MAGIC1) value += ((i_ptr->p1val + i_ptr->p2val) * 150L);

           /* Give credit for speed bonus */
           if (f1 & TR1_SPEED1)
           {
              if (i_ptr->p1val>0)
              {
                 value += (i_ptr->p1val * 30000L);
              }
              else
              {
                 value += (i_ptr->p1val * 500L);
              }
           }

           if (f1 & TR1_SPEED2)
           {
              if (i_ptr->p2val>0)
              {
                 value += (i_ptr->p2val * 30000L);
              }
              else
              {
                 value += (i_ptr->p2val * 500L);
              }
           }

           if (f3 & TR3_SOMELITE) value += 200L;
           break;
   }


   /* Analyze the item */
   switch (i_ptr->tval)
   {
       /* Wands/Staffs */
       case TV_WAND:
       case TV_STAFF:

           /* Pay extra for charges */
           value += ((value / 20) * i_ptr->p1val);

           /* Done */
           break;

       /* Rings/Amulets */
       case TV_RING:
       case TV_AMULET:

           /* Give credit for bonuses */
           value += ((i_ptr->to_h + i_ptr->to_d + i_ptr->to_a) * 100L);

           /* Done */
           break;

       /* Armor */
       case TV_BOOTS:
       case TV_GLOVES:
       case TV_CLOAK:
       case TV_CROWN:
       case TV_HELM:
       case TV_SHIELD:
       case TV_SOFT_ARMOR:
       case TV_HARD_ARMOR:
       case TV_DRAG_ARMOR:

           value += armor_bonus_cost(i_ptr);

           value += ((i_ptr->to_h + i_ptr->to_d) * 100L);

           /* Hack -- Factor in ac rating */
           if (i_ptr->ac != k_ptr->ac)
           {
               /* each base +1 to ac means more than bonus +1 to ac! */
               s32b newvalue = value + (i_ptr->ac - k_ptr->ac) * 150L;
               if (newvalue <=0) newvalue = value / 4;
               value = newvalue;
           }


           /* Done */
           break;

       /* Bows/Weapons */
       case TV_BOW:
       case TV_DIGGING:
       case TV_HAFTED:
       case TV_SWORD:
       case TV_POLEARM:

           /* Factor in the bonuses */
           value += ((i_ptr->to_h + i_ptr->to_d + i_ptr->to_a) * 100L);

           /* Hack -- Factor in extra damage dice */
           if (i_ptr->dd != k_ptr->dd)
           {
               s32b newvalue = value + (i_ptr->dd - k_ptr->dd) * i_ptr->ds * 100L;
               if (newvalue <=0) newvalue = value / 4;
               value = newvalue;
           }

           /* Hack -- Factor in extra damage side */
           if (i_ptr->ds != k_ptr->ds)
           {
               s32b newvalue = value + (i_ptr->ds - k_ptr->ds) * i_ptr->dd * 20L;
               if (newvalue <=0) newvalue = value / 2;
               value = newvalue;
           }


           /* Done */
           break;

       /* Ammo */
       case TV_SHOT:
       case TV_ARROW:
       case TV_BOLT:

           /* Factor in the bonuses */
           value += ((i_ptr->to_h + i_ptr->to_d) * 5L);

           /* Hack -- Factor in extra damage dice */
           if ((i_ptr->dd > k_ptr->dd) && (i_ptr->ds == k_ptr->ds))
           {
               value += (i_ptr->dd - k_ptr->dd) * i_ptr->ds * 5L;
           }

           /* Done */
           break;
   }


   /* Return the value */
   if (value < 0L) value = 0L;
   return (value);
}

/*
 * this is an addition to the value for tried items, where
 * we know *something* about to_a for armour, and dd/ds for weapons
 */
static s32b object_value_tried(object_type *i_ptr)
{
   s32b value = 0;

   object_kind *k_ptr = &k_info[i_ptr->k_idx];

   /* Hack -- "worthless" items */
   if (!k_ptr->cost) return (0L);

   /* Analyze the item */
   switch (i_ptr->tval)
   {
       /* Armor */
       case TV_BOOTS:
       case TV_GLOVES:
       case TV_CLOAK:
       case TV_CROWN:
       case TV_HELM:
       case TV_SHIELD:
       case TV_SOFT_ARMOR:
       case TV_HARD_ARMOR:
       case TV_DRAG_ARMOR:

           /* you sense an armour gives you 4 ac for a leather armour[4,+0] also */
           /* for [4,+2] so there is nothing to add here, the i_ptr->ac factor   */
           /* is already computed */
           /* Done */
           break;

       /* Bows/Weapons */
       case TV_BOW:
       case TV_DIGGING:
       case TV_HAFTED:
       case TV_SWORD:
       case TV_POLEARM:

           /* Hack -- Factor in extra damage dice */
           if (i_ptr->dd != k_ptr->dd)
           {
               s32b newvalue = value + (i_ptr->dd - k_ptr->dd) * i_ptr->ds * 100L;
               if (newvalue <=0) newvalue = value / 4;
               value = newvalue;
           }

           /* Hack -- Factor in extra damage side */
           if (i_ptr->ds != k_ptr->ds)
           {
               s32b newvalue = value + (i_ptr->ds - k_ptr->ds) * i_ptr->dd * 20L;
               if (newvalue <=0) newvalue = value / 2;
               value = newvalue;
           }

           /* Done */
           break;

       /* Ammo */
       case TV_SHOT:
       case TV_ARROW:
       case TV_BOLT:

           /* Hack -- Factor in extra damage dice */
           if ((i_ptr->dd > k_ptr->dd) && (i_ptr->ds == k_ptr->ds))
           {
               value += (i_ptr->dd - k_ptr->dd) * i_ptr->ds * 5L;
           }

           /* Done */
           break;
   }

   /* Return the value */
   return (value);
}



/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item (qty one)
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 *
 * Note that discounted items stay discounted forever, even if
 * the discount is "forgotten" by the player via memory loss.
 */
s32b object_value(object_type *i_ptr)
{
   s32b value;

   /* Unknown items -- acquire a base value */
   if (object_known_p(i_ptr))
   {
      /* Broken items -- worthless */
      if (broken_p(i_ptr)) return (0L);

      /* Cursed items -- worthless */
      if (cursed_p(i_ptr)) return (0L);

      /* Real value (see above) */
      value = object_value_real(i_ptr);
   }

   /* Known items -- acquire the actual value */
   else
   {
      /* Hack -- Felt broken items */
      if ((i_ptr->ident & ID_SENSE) && broken_p(i_ptr)) return (0L);

      /* Hack -- Felt cursed items */
      if ((i_ptr->ident & ID_SENSE) && cursed_p(i_ptr)) return (0L);

      /* Base value (see above) */
      value = object_value_base(i_ptr);

      if ((i_ptr->ident & ID_EQUIPTESTED) ||
          (k_info[i_ptr->k_idx].tried))
      {
         value += object_value_tried(i_ptr);
      }
   }

   /* Apply discount (if any) */
   if (i_ptr->discount) value -= (value * i_ptr->discount / 100L);

   /* Return the final value */
   return (value);
}

/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed.  It uses "randnor()" to choose values from
 * a normal distribution, whose mean moves from zero towards the max as the
 * level increases, and whose standard deviation is equal to 1/4 of the max,
 * and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since the "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the "full" enchantment on an object, even a deep levels.
 *
 * It is always possible (albeit unlikely) to get the "full" enchantment.
 *
 * A sample distribution of values from "m_bonus(10, N)" is shown below:
 *
 *   N       0     1     2     3     4     5     6     7     8     9    10
 * ---    ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
 *   0   66.37 13.01  9.73  5.47  2.89  1.31  0.72  0.26  0.12  0.09  0.03
 *   8   46.85 24.66 12.13  8.13  4.20  2.30  1.05  0.36  0.19  0.08  0.05
 *  16   30.12 27.62 18.52 10.52  6.34  3.52  1.95  0.90  0.31  0.15  0.05
 *  24   22.44 15.62 30.14 12.92  8.55  5.30  2.39  1.63  0.62  0.28  0.11
 *  32   16.23 11.43 23.01 22.31 11.19  7.18  4.46  2.13  1.20  0.45  0.41
 *  40   10.76  8.91 12.80 29.51 16.00  9.69  5.90  3.43  1.47  0.88  0.65
 *  48    7.28  6.81 10.51 18.27 27.57 11.76  7.85  4.99  2.80  1.22  0.94
 *  56    4.41  4.73  8.52 11.96 24.94 19.78 11.06  7.18  3.68  1.96  1.78
 *  64    2.81  3.07  5.65  9.17 13.01 31.57 13.70  9.30  6.04  3.04  2.64
 *  72    1.87  1.99  3.68  7.15 10.56 20.24 25.78 12.17  7.52  4.42  4.62
 *  80    1.02  1.23  2.78  4.75  8.37 12.04 27.61 18.07 10.28  6.52  7.33
 *  88    0.70  0.57  1.56  3.12  6.34 10.06 15.76 30.46 12.58  8.47 10.38
 *  96    0.27  0.60  1.25  2.28  4.30  7.60 10.77 22.52 22.51 11.37 16.53
 * 104    0.22  0.42  0.77  1.36  2.62  5.33  8.93 13.05 29.54 15.23 22.53
 * 112    0.15  0.20  0.56  0.87  2.00  3.83  6.86 10.06 17.89 27.31 30.27
 * 120    0.03  0.11  0.31  0.46  1.31  2.48  4.60  7.78 11.67 25.53 45.72
 * 128    0.02  0.01  0.13  0.33  0.83  1.41  3.24  6.17  9.57 14.22 64.07
 */
static s16b m_bonus(s16b max, s16b level)
{
   s16b bonus, stand, extra, value;
   s16b my_MAX_LEVEL;

   /* this can be called from birth.c, when MAX_LEVEL is not yet known */
   my_MAX_LEVEL = MAX_LEVEL;
   if (MAX_LEVEL==0) my_MAX_LEVEL=127;

   /* Paranoia -- enforce maximal "level" */
   if (level > my_MAX_LEVEL - 1) level = my_MAX_LEVEL - 1;

   /* The "bonus" moves towards the max */
   bonus = ((max * level) / my_MAX_LEVEL);

   /* Hack -- determine fraction of error */
   extra = ((max * level) % my_MAX_LEVEL);

   /* Hack -- simulate floating point computations */
   if (rand_int(my_MAX_LEVEL) < extra) bonus++;

   /* The "stand" is equal to one quarter of the max */
   stand = (max / 4);

   /* Hack -- determine fraction of error */
   extra = (max % 4);

   /* Hack -- simulate floating point computations */
   if (rand_int(4) < extra) stand++;

   /* Choose an "interesting" value */
   value = randnor(bonus, stand);

   /* Enforce the minimum value */
   if (value < 0) return (0);

   /* Enforce the maximum value */
   if (value > max) return (max);

   /* Result */
   return (value);
}

/*
 * Cheat -- describe a created object for the user
 */
static void object_mention(object_type *i_ptr)
{
   char i_name[80];

   /* Describe */
   object_desc_store(i_name, i_ptr, FALSE, 0);

   /* Artifact */
   if (artifact_p(i_ptr))
   {
      /* Silly message */
      msg_format("Artifact (%s)", i_name);
   }

   /* Ego-item */
   else if (ego_item_p(i_ptr))
   {
      /* Silly message */
      msg_format("Ego-item (%s)", i_name);
   }

   /* Normal item */
   else
   {
      /* Silly message */
      msg_format("Object (%s)", i_name);
   }
}

/*
 * Mega-Hack -- Attempt to create one of the "Special Objects"
 *
 * We are only called from "place_object()", and we assume that
 * "apply_magic()" is called immediately after we return.
 *
 * Note -- see "make_artifact()" and "apply_magic()"
 */
bool make_artifact_special(object_type *i_ptr, bool wizard)
{
   s16b                 i, art_num, offset;

   s16b                 k_idx = 0;
   artifact_type *a_ptr;

   /* No artifacts in the town */
   if (!p_ptr->mdepth) return (FALSE);

dlog(DEBUGITEMS, "object2.c: make_artifact_special: starting\n");

   offset=rand_int(ART_MIN_NORMAL);
   /* Check the artifact list (just the "specials") */
   for (i = 0; i < ART_MIN_NORMAL; i++)
   {
      art_num = ( offset + i ) % ART_MIN_NORMAL;
      a_ptr = &a_info[art_num];

      /* Skip "empty" artifacts */
      if (!a_ptr->name) continue;

dlog(DEBUGITEMS,"object2.c: make_artifact_special: artifact %d = %s (cur_num %d, i_ptr %s)\n",
                art_num, a_name + a_ptr->name, a_ptr->cur_num, k_name + k_info[i_ptr->k_idx].name);
      /* Cannot make an artifact twice */
      if (a_ptr->cur_num && !wizard)
      {
dlog(DEBUGITEMS,"object2.c: make_artifact_special: already exists, not wizard\n");
         continue;
      }

#if 0
      /* making random items is not what wizards want! */
      if (wizard && (a_ptr->tval != i_ptr->tval)) continue;
#endif

      /* XXX XXX Enforce minimum "depth" (loosely) */
      if (a_ptr->level > p_ptr->mdepth)
      {
         /* Acquire the "out-of-depth factor" */
         s16b d = (a_ptr->level - p_ptr->mdepth) * 2;

         /* Roll for out-of-depth creation */
         if (rand_int(d) != 0)
         {
dlog(DEBUGITEMS,"object2.c: make_artifact_special: failed minimum depth (%d vs %d)\n",
                a_ptr->level, p_ptr->mdepth);
            continue;
         }
      }
dlog(DEBUGITEMS,"object2.c: make_artifact_special: trying for %s rarity %d\n",
                             a_name + a_ptr->name, a_ptr->rarity);
      /* Artifact "rarity roll" */
      if (rand_int(a_ptr->rarity) != 0)
      {
         return (0);
      }
dlog(DEBUGITEMS,"object2.c: make_artifact_special: %s succeeded1 rarity %d tv %d sv %d\n",
                a_name + a_ptr->name, a_ptr->rarity, a_ptr->tval, a_ptr->sval);

      /* Find the base object */
      k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

      /* XXX XXX Enforce minimum "object" level (loosely) */
      if (k_info[k_idx].level > object_level)
      {
         /* Acquire the "out-of-depth factor" */
         s16b d = (k_info[k_idx].level - object_level) * 5;

         /* Roll for out-of-depth creation */
         if (rand_int(d) != 0) continue;
      }
dlog(DEBUGITEMS,"object2.c: make_artifact_special: %s succeeded2 k_idx %d\n",
                             a_name + a_ptr->name, k_idx);

      /* Assign the template */
      invcopy(i_ptr, k_idx);
dlog(DEBUGITEMS,"object2.c: make_artifact_special: i_ptr tv %d sv %d k_idx %d name %s\n",
                i_ptr->tval, i_ptr->sval, i_ptr->k_idx, k_name + k_info[i_ptr->k_idx].name);

      /* Mega-Hack -- mark the item as an artifact */
      i_ptr->name1 = art_num;

      /* Success */
      return (TRUE);
   }

   /* Failure */
   return (FALSE);
}

/*
 * Attempt to change an object into an artifact
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
/* jk - static removed */
bool make_artifact(object_type *i_ptr, bool wizard)
{
   s16b i;

   /* No artifacts in the town */
   if (!p_ptr->mdepth) return (FALSE);

   /* Paranoia -- no "plural" artifacts */
   if (i_ptr->number != 1) return (FALSE);

   /* Check the artifact list (skip the "specials") */
   for (i = ART_MIN_NORMAL; i < a_number; i++)
   {
      artifact_type *a_ptr = &a_info[i];

      /* Skip "empty" items */
      if (!a_ptr->name) continue;

      /* Cannot make an artifact twice */
      if (a_ptr->cur_num && !wizard) continue;

      /* Must have the correct fields */
      if (a_ptr->tval != i_ptr->tval) continue;
      if (a_ptr->sval != i_ptr->sval) continue;

      /* XXX XXX Enforce minimum "depth" (loosely) */
      if (a_ptr->level > p_ptr->mdepth)
      {
         /* Acquire the "out-of-depth factor" */
         s16b d = (a_ptr->level - p_ptr->mdepth) * 2;

         /* Roll for out-of-depth creation */
         if (rand_int(d) != 0) continue;
      }

      /* We must make the "rarity roll" */
      if (rand_int(a_ptr->rarity) != 0) continue;

      /* Hack -- mark the item as an artifact */
      i_ptr->name1 = i;

      /* Success */
      return (TRUE);
   }

   /* Failure */
   return (FALSE);
}

/*
 * Charge a new wand.
 */
void charge_wand(object_type *i_ptr)
{
   switch (i_ptr->sval)
   {
      case SV_WAND_HEAL_MONSTER:      i_ptr->p1val = randint(20) + 8; break;
      case SV_WAND_HASTE_MONSTER:     i_ptr->p1val = randint(20) + 8; break;
      case SV_WAND_CLONE_MONSTER:     i_ptr->p1val = randint(5)  + 3; break;
      case SV_WAND_TELEPORT_AWAY:     i_ptr->p1val = randint(5)  + 6; break;
      case SV_WAND_DISARMING:         i_ptr->p1val = randint(5)  + 4; break;
      case SV_WAND_TRAP_DOOR_DEST:    i_ptr->p1val = randint(8)  + 6; break;
      case SV_WAND_STONE_TO_MUD:      i_ptr->p1val = randint(4)  + 3; break;
      case SV_WAND_LITE:              i_ptr->p1val = randint(10) + 6; break;
      case SV_WAND_SLEEP_MONSTER:     i_ptr->p1val = randint(15) + 8; break;
      case SV_WAND_SLOW_MONSTER:      i_ptr->p1val = randint(10) + 6; break;
      case SV_WAND_CONFUSE_MONSTER:   i_ptr->p1val = randint(12) + 6; break;
      case SV_WAND_FEAR_MONSTER:      i_ptr->p1val = randint(5)  + 3; break;
      case SV_WAND_DRAIN_LIFE:        i_ptr->p1val = randint(3)  + 3; break;
      case SV_WAND_POLYMORPH:         i_ptr->p1val = randint(8)  + 6; break;
      case SV_WAND_STINKING_CLOUD:    i_ptr->p1val = randint(8)  + 6; break;
      case SV_WAND_MAGIC_MISSILE:     i_ptr->p1val = randint(10) + 6; break;
      case SV_WAND_ACID_BOLT:         i_ptr->p1val = randint(8)  + 6; break;
      case SV_WAND_ELEC_BOLT:         i_ptr->p1val = randint(8)  + 6; break;
      case SV_WAND_FIRE_BOLT:         i_ptr->p1val = randint(8)  + 6; break;
      case SV_WAND_COLD_BOLT:         i_ptr->p1val = randint(5)  + 6; break;
      case SV_WAND_ACID_BALL:         i_ptr->p1val = randint(5)  + 2; break;
      case SV_WAND_ELEC_BALL:         i_ptr->p1val = randint(8)  + 4; break;
      case SV_WAND_FIRE_BALL:         i_ptr->p1val = randint(4)  + 2; break;
      case SV_WAND_COLD_BALL:         i_ptr->p1val = randint(6)  + 2; break;
      case SV_WAND_WONDER:            i_ptr->p1val = randint(15) + 8; break;
      case SV_WAND_ANNIHILATION:      i_ptr->p1val = randint(2)  + 1; break;
      case SV_WAND_DRAGON_FIRE:       i_ptr->p1val = randint(3)  + 1; break;
      case SV_WAND_DRAGON_COLD:       i_ptr->p1val = randint(3)  + 1; break;
      case SV_WAND_DRAGON_BREATH:     i_ptr->p1val = randint(3)  + 1; break;
   }
}

/*
 * Charge a new staff.
 */
void charge_staff(object_type *i_ptr)
{
   switch (i_ptr->sval)
   {
      case SV_STAFF_DARKNESS:         i_ptr->p1val = randint(8)  + 8; break;
      case SV_STAFF_SLOWNESS:         i_ptr->p1val = randint(8)  + 8; break;
      case SV_STAFF_HASTE_MONSTERS:   i_ptr->p1val = randint(8)  + 8; break;
      case SV_STAFF_SUMMONING:        i_ptr->p1val = randint(3)  + 1; break;
      case SV_STAFF_TELEPORTATION:    i_ptr->p1val = randint(4)  + 5; break;
      case SV_STAFF_IDENTIFY:         i_ptr->p1val = randint(15) + 5; break;
      case SV_STAFF_REMOVE_CURSE:     i_ptr->p1val = randint(3)  + 4; break;
      case SV_STAFF_STARLITE:         i_ptr->p1val = randint(5)  + 6; break;
      case SV_STAFF_LITE:             i_ptr->p1val = randint(20) + 8; break;
      case SV_STAFF_MAPPING:          i_ptr->p1val = randint(5)  + 5; break;
      case SV_STAFF_DETECT_GOLD:      i_ptr->p1val = randint(20) + 8; break;
      case SV_STAFF_DETECT_ITEM:      i_ptr->p1val = randint(15) + 6; break;
      case SV_STAFF_DETECT_TRAP:      i_ptr->p1val = randint(5)  + 6; break;
      case SV_STAFF_DETECT_DOOR:      i_ptr->p1val = randint(8)  + 6; break;
      case SV_STAFF_DETECT_INVIS:     i_ptr->p1val = randint(15) + 8; break;
      case SV_STAFF_DETECT_EVIL:      i_ptr->p1val = randint(15) + 8; break;
      case SV_STAFF_CURE_LIGHT:       i_ptr->p1val = randint(5)  + 6; break;
      case SV_STAFF_CURING:           i_ptr->p1val = randint(3)  + 4; break;
      case SV_STAFF_HEALING:          i_ptr->p1val = randint(2)  + 1; break;
      case SV_STAFF_THE_MAGI:         i_ptr->p1val = randint(2)  + 2; break;
      case SV_STAFF_SLEEP_MONSTERS:   i_ptr->p1val = randint(5)  + 6; break;
      case SV_STAFF_SLOW_MONSTERS:    i_ptr->p1val = randint(5)  + 6; break;
      case SV_STAFF_SPEED:            i_ptr->p1val = randint(3)  + 4; break;
      case SV_STAFF_PROBING:          i_ptr->p1val = randint(6)  + 2; break;
      case SV_STAFF_DISPEL_EVIL:      i_ptr->p1val = randint(3)  + 4; break;
      case SV_STAFF_POWER:            i_ptr->p1val = randint(3)  + 1; break;
      case SV_STAFF_HOLINESS:         i_ptr->p1val = randint(2)  + 2; break;
      case SV_STAFF_GENOCIDE:         i_ptr->p1val = randint(2)  + 1; break;
      case SV_STAFF_EARTHQUAKES:      i_ptr->p1val = randint(5)  + 3; break;
      case SV_STAFF_DESTRUCTION:      i_ptr->p1val = randint(3)  + 1; break;
   }
}

/* jk - this computes a bonus between min & max */
/* cursed or broken items get negative bonuses */
static s16b get_bonus(s16b min, s16b max, bool negative)
{
   s16b len;
   s16b result;
   s16b tmp;
   if (min>max)
   {
      tmp = min; min = max; max = tmp;
   }

   len = (max - min);

   if (negative)
   {
      result= -(min + rand_int(len));
   }
   else
   {
      result= min + rand_int(len);
dlog(DEBUGITEMS,"object2.c: get_bonus: min %d max %d negative %d return %d\n",
                min, max, negative, result);
   }
   return (result);
}

/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 * Hack -- note special rating boost for dragon scale mail
 */
static void a_m_aux_1(object_type *i_ptr, s16b level, s16b power)
{
   s16b tohit1 = randint(5) + m_bonus(5, level);
   s16b todam1 = randint(5) + m_bonus(5, level);

   s16b tohit2 = m_bonus(10, level);
   s16b todam2 = m_bonus(10, level);
   s16b chance;

dlog(DEBUGITEMS,"object2.c: a_m_aux1: tv %d sv %d pv %ld dd %d ds %d lev %d pow %d\n",
                i_ptr->tval, i_ptr->sval, i_ptr->p1val, i_ptr->dd, i_ptr->ds, level,
                power);

   /* Good */
   if (power > 0)
   {
       /* Enchant */
       i_ptr->to_h += tohit1;
       i_ptr->to_d += todam1;

       /* Very good */
       if (power > 1)
       {
           /* Enchant again */
           i_ptr->to_h += tohit2;
           i_ptr->to_d += todam2;
       }
   }

   /* Cursed */
   else if (power < 0)
   {
       /* Penalize */
       i_ptr->to_h -= tohit1;
       i_ptr->to_d -= todam1;

       /* Very cursed */
       if (power < -1)
       {
           /* Penalize again */
           i_ptr->to_h -= tohit2;
           i_ptr->to_d -= todam2;
       }

       /* Cursed (if "bad") */
       if (i_ptr->to_h + i_ptr->to_d < 0) i_ptr->ident |= ID_CURSED;
   }
dlog(DEBUGITEMS,"object2.c: a_m_aux1: step 2\n");

   /* Analyze type */
   switch (i_ptr->tval)
   {
       case TV_DIGGING:

           /* Very good */
           if (power > 1)
           {
               /* Special Ego-item */
               i_ptr->name2 = EGO_DIGGING;
           }

           /* Bad */
           else if (power < 0)
           {
               /* Hack -- Reverse digging bonus */
               i_ptr->p1val = 0 - (i_ptr->p1val);
           }

           /* Very bad */
           else if (power < -1)
           {
               /* Hack -- Horrible digging bonus */
               i_ptr->p1val = 0 - (5 + randint(5));
           }

           break;


       case TV_HAFTED:
       case TV_POLEARM:
       case TV_SWORD:

           /* Very Good */
           if (power > 1)
           {
              if ((i_ptr->tval == TV_HAFTED) && (i_ptr->sval == SV_QUARTERSTAFF) && (randint(3)==1))
              {
                 switch(randint(10))
                 {
                    case 1:
                    case 2:
                    case 3:
                    case 4:
                    case 5:
                    case 6: i_ptr->name2 = EGO_MAGE;
                            break;
                    case 7:
                    case 8:
                    case 9: i_ptr->name2 = EGO_ADEPT;
                            break;
                    case 10: i_ptr->name2 = EGO_ARCHMAGE;
                             break;
                 }
                 return;
              }

              /* Roll for an ego-item */
/* jk - was 29 */
              chance = randint(34);

              switch (chance)
              {
                  case 1:
                      i_ptr->name2 = EGO_HA;
                      break;

                  case 2:
                      i_ptr->name2 = EGO_DF;
                      break;

                  case 3:
                      i_ptr->name2 = EGO_BRAND_ACID;
                      break;

                  case 4:
                      i_ptr->name2 = EGO_BRAND_ELEC;
                      break;

                  case 5:
                      i_ptr->name2 = EGO_BRAND_FIRE;
                      break;

                  case 6:
                      i_ptr->name2 = EGO_BRAND_COLD;
                      break;

                  case 7: case 8:
                      i_ptr->name2 = EGO_SLAY_ANIMAL;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_ANIMAL;
                      }
                      break;

                  case 9: case 10:
                      i_ptr->name2 = EGO_SLAY_DRAGON;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_DRAGON;
                      }
                      break;

                  case 11: case 12:
                      i_ptr->name2 = EGO_SLAY_EVIL;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_EVIL;
                      }
                      break;

                  case 13: case 14:
                      i_ptr->name2 = EGO_SLAY_UNDEAD;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_UNDEAD;
                      }
                      break;

                  case 15: case 16: case 17:
                      i_ptr->name2 = EGO_SLAY_ORC;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_ORC;
                      }
                      break;

                  case 18: case 19: case 20:
                      i_ptr->name2 = EGO_SLAY_TROLL;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_TROLL;
                      }
                      break;

                  case 21: case 22: case 23:
                      i_ptr->name2 = EGO_SLAY_GIANT;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_GIANT;
                      }
                      break;

                  case 24: case 25: case 26:
                      i_ptr->name2 = EGO_SLAY_DEMON;
                      if (rand_int(100) < 20)
                      {
                          i_ptr->name2 = EGO_KILL_DEMON;
                      }
                      break;

                  case 27:
                      i_ptr->name2 = EGO_WEST;
                      break;

                  case 28:
                      i_ptr->name2 = EGO_BLESS_BLADE;
                      break;

                  case 29:
                      i_ptr->name2 = EGO_ATTACKS;
                      break;
/* jk */
                  case 30:
                      i_ptr->name2 = EGO_VAMPIRIC;
                      break;
                  case 31: case 32:
                      i_ptr->name2 = EGO_SIZE;
                      break;
                  case 33: case 34:
                      i_ptr->name2 = EGO_JOY;
                      break;
              }

              /* Hack -- Super-charge the damage dice */

dlog(DEBUGITEMS,"object2.c: a_m_aux1: supercharging: dd %d ds %d\n",
                i_ptr->dd, i_ptr->ds);

              /* jk - not sure why cases with ds=0 exist.... */
              /* but they do and hang the game here :-( */
              if (i_ptr->ds)
              {
                 while (rand_int(10L * i_ptr->dd * i_ptr->ds) == 0) i_ptr->dd++;
              }

              /* Hack -- Lower the damage dice */
              if (i_ptr->dd > 9) i_ptr->dd = 9;
           }

           /* Very cursed */
           else if (power < -1)
           {
              /* Roll for ego-item */
              if (rand_int(MAX_LEVEL) < level)
              {
                 i_ptr->name2 = EGO_MORGUL;
              }
           }

           break;


       case TV_BOW:

           /* Very good */
           if (power > 1)
           {
               s16b chance;
               chance=randint(11);
               /* Roll for ego-item */
               switch (chance)
               {
                   case 1:
                       i_ptr->name2 = EGO_EXTRA_MIGHT;
                       break;

                   case 2:
                       i_ptr->name2 = EGO_EXTRA_SHOTS;
                       break;

                   case 3: case 4: case 5: case 6:
                       i_ptr->name2 = EGO_VELOCITY;
                       break;

                   case 7: case 8: case 9: case 10:
                       i_ptr->name2 = EGO_ACCURACY;
                       break;
                   case 11:
                   {
                       char i_name[80];
                       object_desc(i_name, i_ptr, TRUE, 3);
                       i_ptr->name2 = EGO_HOLLOW_STEEL;

                       break;
                   }
               }
           }
           break;

       case TV_BOLT:
       case TV_ARROW:
       case TV_SHOT:

           /* Very good */
           if (power > 1)
           {
               bool again = TRUE;
               while (again)
               {
                  again = FALSE;
                  /* Roll for ego-item */
                  switch (randint(16))
                  {
                      case 1:
                      case 2:
                      case 3:  i_ptr->name2 = EGO_WOUNDING;
                               break;

                      case 4:  i_ptr->name2 = EGO_FLAME;
                               break;

                      case 5:  i_ptr->name2 = EGO_FROST;
                               break;

                      case 6:
                      case 7:  i_ptr->name2 = EGO_HURT_ANIMAL;
                               break;

                      case 8:
                      case 9:  i_ptr->name2 = EGO_HURT_EVIL;
                               break;

                      case 10: i_ptr->name2 = EGO_HURT_DRAGON;
                               break;
                      case 11:
                      case 12: if (i_ptr->tval == TV_BOLT)
                                  i_ptr->name2 = EGO_ORCISH;
                               else
                                  again = TRUE;
                               break;

                      case 13:
                      case 14: if (i_ptr->tval == TV_SHOT)
                                  i_ptr->name2 = EGO_HOBBIT;
                               else
                                  again = TRUE;
                               break;


                      case 15: if (i_ptr->tval == TV_ARROW)
                                  i_ptr->name2 = EGO_ELVEN;
                               else
                                  again = TRUE;
                               break;
                      case 16: if (i_ptr->tval == TV_ARROW)
                               {
                                  i_ptr->name2 = EGO_BLACKFEATHER;
                                  i_ptr->ds = 2;
                               }
                               else
                                  again = TRUE;
                               break;
                  }
               }

               /* Hack -- super-charge the damage dice */
dlog(DEBUGITEMS,"object2.c: a_m_aux1: supercharging: dd %d ds %d\n",
                i_ptr->dd, i_ptr->ds);
               if (i_ptr->ds)
               {
                  while (rand_int(10L * i_ptr->dd * i_ptr->ds) == 0) i_ptr->dd++;
               }

               /* Hack -- restrict the damage dice */
               if (i_ptr->dd > 9) i_ptr->dd = 9;
           }

           /* Very cursed */
           else if (power < -1)
           {
               /* Roll for ego-item */
               if (rand_int(MAX_LEVEL) < level)
               {
                   i_ptr->name2 = EGO_BACKBITING;
               }
           }
           break;
   }
dlog(DEBUGITEMS,"object2.c: a_m_aux1: returning\n");

}

/*
 * Apply magic to an item known to be "armor"
 *
 * Hack -- note special processing for crown/helm
 * Hack -- note special processing for robe of permanence
 */
static void a_m_aux_2(object_type *i_ptr, s16b level, s16b power)
{
   s16b toac1, toac2;
   toac1 = randint(5) + m_bonus(5, level);
   toac2 = m_bonus(10, level);
   /* Good */
   if (power > 0)
   {
      /* Enchant */
      i_ptr->to_a += toac1;

      /* Very good */
      if (power > 1)
      {
         /* Enchant again */
         i_ptr->to_a += toac2;
      }
   }

   /* Cursed */
   else if (power < 0)
   {
      /* Penalize */
      i_ptr->to_a -= toac1;

      /* Very cursed */
      if (power < -1)
      {
         /* Penalize again */
         i_ptr->to_a -= toac2;
      }

      /* Cursed (if "bad") */
      if (i_ptr->to_a < 0) i_ptr->ident |= ID_CURSED;
   }


   /* Analyze type */
   switch (i_ptr->tval)
   {
      case TV_DRAG_ARMOR:

         /* Rating boost */
         rating += 30;

         /* Mention the item */
         if (cheat_peek) object_mention(i_ptr);

         break;


      case TV_HARD_ARMOR:
      case TV_SOFT_ARMOR:

         /* Very good */
         if (power > 1)
         {
            s16b chance = randint(24);
            /* Hack -- Try for "Robes of the Magi" */
            if ((i_ptr->tval == TV_SOFT_ARMOR) &&
               (i_ptr->sval == SV_ROBE) &&
               (rand_int(100) < 10))
            {
               i_ptr->name2 = EGO_PERMANENCE;
               break;
            }

            /* Roll for ego-item */
            if (chance<5) i_ptr->name2 = EGO_RESIST_ACID;
            else if (chance<9) i_ptr->name2 = EGO_RESIST_ELEC;
            else if (chance<13) i_ptr->name2 = EGO_RESIST_FIRE;
            else if (chance<17) i_ptr->name2 = EGO_RESIST_COLD;
            else if (chance<19) i_ptr->name2 = EGO_RESISTANCE;
            else if (chance<22) i_ptr->name2 = EGO_QUALITY;
            else i_ptr->name2 = EGO_ELVENKIND;
         }

         break;

      case TV_SHIELD:

         /* Very good */
         if (power > 1)
         {
            /* Roll for ego-item */
            switch (randint(12))
            {
               case 1:
                  i_ptr->name2 = EGO_ENDURE_ACID;
                  break;

               case 2: case 3: case 4:
                  i_ptr->name2 = EGO_ENDURE_ELEC;
                  break;

               case 5: case 6:
                  i_ptr->name2 = EGO_ENDURE_FIRE;
                  break;

               case 7: case 8: case 9:
                  i_ptr->name2 = EGO_ENDURE_COLD;
                  break;

               case 10: case 11:
                  i_ptr->name2 = EGO_DWARVEN;
                  break;

               default:
                  i_ptr->name2 = EGO_ENDURANCE;
                  break;
            }
         }

         break;


      case TV_GLOVES:

         /* Very good */
         if (power > 1)
         {
            /* Roll for ego-item */
/* jk - was 10 */
            switch (randint(11))
            {
               case 1: case 2: case 3: case 4:
                  i_ptr->name2 = EGO_FREE_ACTION;
                  break;

               case 5: case 6: case 7:
                  i_ptr->name2 = EGO_SLAYING;
                  break;

               case 8: case 9:
                  i_ptr->name2 = EGO_AGILITY;
                  break;

               case 10:
                  i_ptr->name2 = EGO_POWER;
                  break;

/* jk */
               case 11:
                  i_ptr->name2 = EGO_MAGIC;
                  break;
            }
         }

         /* Very cursed */
         else if (power < -1)
         {
            /* Roll for ego-item */
            switch (randint(2))
            {
               case 1:
                  i_ptr->name2 = EGO_CLUMSINESS;
                  break;
               default:
                  i_ptr->name2 = EGO_WEAKNESS;
                  break;
            }
         }

         break;


      case TV_BOOTS:

         /* Very good */
         if (power > 1)
         {
            /* Roll for ego-item */
            s16b k = randint(30);
/* chances:
 *
 * 2 in 30 : boots of speed
 * 4 in 30 : boots of free action
 * 8 in 30 : boots of stealth
 * 6 in 30 : boots of jumping
 * 5 in 30 : boots of heavy step
 * 5 in 30 : boots of slow descent
 */
            if (k<2) i_ptr->name2 = EGO_SPEED;
            else if (k<6) i_ptr->name2 =  EGO_MOTION;
            else if (k<14) i_ptr->name2 = EGO_QUIET;
            else if (k<20) i_ptr->name2 = EGO_JUMP;
            else if (k<25) i_ptr->name2 = EGO_HSTEP;
            else        i_ptr->name2 = EGO_SLOW_DESCENT;
         }

         /* Very cursed */
         else if (power < -1)
         {
            /* Roll for ego-item */
            switch (randint(3))
            {
               case 1:
                  i_ptr->name2 = EGO_NOISE;
                  break;
               case 2:
                  i_ptr->name2 = EGO_SLOWNESS;
                  break;
               case 3:
                  i_ptr->name2 = EGO_ANNOYANCE;
                  break;
            }
         }

         break;


      case TV_CROWN:

         /* Very good */
         if (power > 1)
         {
            /* Roll for ego-item */
            switch (randint(8))
            {
               case 1:
                  i_ptr->name2 = EGO_MAGI;
                  break;

               case 2:
                  i_ptr->name2 = EGO_MIGHT;
                  break;

               case 3:
                  i_ptr->name2 = EGO_TELEPATHY;
                  break;

               case 4:
                  i_ptr->name2 = EGO_REGENERATION;
                  break;

               case 5: case 6:
                  i_ptr->name2 = EGO_LORDLINESS;
                  break;

               default:
                  i_ptr->name2 = EGO_SEEING;
                  break;
            }
         }

         /* Very cursed */
         else if (power < -1)
         {
            /* Roll for ego-item */
            switch (randint(9))
            {
               case 1: case 2:
                  i_ptr->name2 = EGO_STUPIDITY;
                  break;
               case 3: case 4:
                  i_ptr->name2 = EGO_NAIVETY;
                  break;
               case 5:
                  i_ptr->name2 = EGO_UGLINESS;
                  break;
               case 6:
                  i_ptr->name2 = EGO_SICKLINESS;
                  break;
               case 7:
                  i_ptr->name2 = EGO_TELEPORTATION;
                  break;
/* jk */
               case 8:
                  i_ptr->name2 = EGO_CRUSH;
                  break;
            }
         }

         break;


      case TV_HELM:

         /* Very good */
         if (power > 1)
         {
            /* Roll for ego-item */
            switch (randint(15))
            {
               case 1: case 2:
                  i_ptr->name2 = EGO_INTELLIGENCE;
                  break;

               case 3: case 4:
                  i_ptr->name2 = EGO_WISDOM;
                  break;

               case 5: case 6:
                  i_ptr->name2 = EGO_BEAUTY;
                  break;

               case 7: case 8:
                  i_ptr->name2 = EGO_SEEING;
                  break;

               case 9: case 10:
                  i_ptr->name2 = EGO_LITE;
                  break;

/* jk */
               case 11:
                  i_ptr->name2 = EGO_NOLDOR;
                  break;

               default:
                  i_ptr->name2 = EGO_INFRAVISION;
                  break;
            }
         }

         /* Very cursed */
         else if (power < -1)
         {
            /* Roll for ego-item */
            switch (randint(9))
            {
               case 1: case 2:
                  i_ptr->name2 = EGO_STUPIDITY;
                  break;
               case 3: case 4:
                  i_ptr->name2 = EGO_NAIVETY;
                  break;
               case 5:
                  i_ptr->name2 = EGO_UGLINESS;
                  break;
               case 6:
                  i_ptr->name2 = EGO_SICKLINESS;
                  break;
               case 7:
                  i_ptr->name2 = EGO_TELEPORTATION;
                  break;
/* jk */
               case 8: case 9:
                  i_ptr->name2 = EGO_CRUSH;
                  break;
            }
         }

         break;


      case TV_CLOAK:

         /* Very good */
         if (power > 1)
         {
            /* Roll for ego-item */
            switch (randint(17))
            {
               case 1: case 2: case 3: case 4:
               case 5: case 6: case 7: case 8:
                  i_ptr->name2 = EGO_PROTECTION;
                  break;

               case 9: case 10: case 11: case 12:
               case 13: case 14: case 15: case 16:
                  i_ptr->name2 = EGO_STEALTH;
                  break;

               case 17:
                  i_ptr->name2 = EGO_AMAN;
                  break;
            }
         }

         /* Very cursed */
         else if (power < -1)
         {
            /* Choose some damage */
            switch (randint(3))
            {
               case 1:
                  i_ptr->name2 = EGO_IRRITATION;
                  break;
               case 2:
                  i_ptr->name2 = EGO_VULNERABILITY;
                  break;
               case 3:
                  i_ptr->name2 = EGO_ENVELOPING;
                  break;
            }
         }

         break;
   }
}

/*
 * try to make an ego-item ring
 */
static void make_ego_item_ring(object_type *i_ptr, s16b level, s16b power)
{
  if (power > 1)
  {
     switch randint(9)
     {
        case  1:
           i_ptr->name2 = EGO_STALKING;
           break;
        case  2:
           i_ptr->name2 = EGO_MINDLESSRAGE;
           break;
        case  3:
           i_ptr->name2 = EGO_MEDITATION;
           break;
        case  4:
           i_ptr->name2 = EGO_RUSHING;
           break;
        case  5:
           i_ptr->name2 = EGO_NUMENOR;
           break;
        case  6:
           i_ptr->name2 = EGO_MORIA;
           break;
        case  7:
           i_ptr->name2 = EGO_LOTHLORIEN;
           break;
        case  8:
           i_ptr->name2 = EGO_GONDOR;
           break;
        case  9:
           i_ptr->name2 = EGO_ROHAN;
           break;
     }
  }
  else if (power < 0)
  {
    switch randint(30)
    {
       case  1:
          i_ptr->name2 = EGO_WITHERING1;
          break;
       case  2:
          i_ptr->name2 = EGO_WITHERING2;
          break;
       case  3:
          i_ptr->name2 = EGO_SHAKING;
          break;
       case  4:
          i_ptr->name2 = EGO_DULLEDMIND1;
          break;
       case  5:
          i_ptr->name2 = EGO_DULLEDMIND2;
          break;
       case  6:
          i_ptr->name2 = EGO_SLOTHFULNESS1;
          break;
       case  7:
          i_ptr->name2 = EGO_SLOTHFULNESS2;
          break;
    }
 }
 return;
}

/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special rating boost for ring of speed
 * Hack -- note special rating boost for amulet of the magi
 * Hack -- note special "p1val boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
static void a_m_aux_3(object_type *i_ptr, s16b level, s16b power)
{
   /* Apply magic (good or bad) according to type */
   switch (i_ptr->tval)
   {
      case TV_RING:
      {
         /* it this one of the special rings, reserved for ego-items? */
         /* and do we want an ego-item? */
         if (k_info[i_ptr->k_idx].flags3 & TR3_NOFLAVOR)
         {
            make_ego_item_ring(i_ptr, level, power);
         }
         else
         {
            /* Analyze */
            switch (i_ptr->sval)
            {
               /* Strength, Constitution, Dexterity, Intelligence */
               case SV_RING_STR:
               case SV_RING_CON:
               case SV_RING_DEX:
               case SV_RING_INT:
               {
                  /* Stat bonus */
                  i_ptr->p1val = 1 + m_bonus(5, level);

                  /* Cursed */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse p1val */
                     i_ptr->p1val = 0 - (i_ptr->p1val);
                  }

                  break;
               }

               /* Ring of Speed! */
               case SV_RING_SPEED:
               {
                  /* Base speed (1 to 10) */
                  i_ptr->p1val = randint(5) + m_bonus(5, level);

                  /* Super-charge the ring */
                  while (rand_int(100) < 50) i_ptr->p1val++;

                  /* Cursed Ring */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse p1val */
                     i_ptr->p1val = 0 - (i_ptr->p1val);

                     break;
                  }

                  /* Rating boost */
                  rating += 25;

                  /* Mention the item */
                  if (cheat_peek) object_mention(i_ptr);

                  break;
               }

               /* Searching */
               case SV_RING_SEARCHING:
               {
                  /* Bonus to searching */
                  i_ptr->p1val = 1 + m_bonus(5, level);

                  /* Cursed */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse p1val */
                     i_ptr->p1val = 0 - (i_ptr->p1val);
                  }

                  break;
               }

               /* Flames, Acid, Ice */
               case SV_RING_FLAMES:
               case SV_RING_ACID:
               case SV_RING_ICE:
               {
                  /* Bonus to armor class */
                  i_ptr->to_a = 5 + randint(5) + m_bonus(10, level);
                  break;
               }

               /* Weakness, Stupidity */
               case SV_RING_WEAKNESS:
               case SV_RING_STUPIDITY:
               {
                  /* Broken */
                  i_ptr->ident |= ID_BROKEN;

                  /* Cursed */
                  i_ptr->ident |= ID_CURSED;

                  /* Penalize */
                  i_ptr->p1val = 0 - (1 + m_bonus(5, level));

                  break;
               }

               /* WOE, Stupidity */
               case SV_RING_WOE:
               {
                  /* Broken */
                  i_ptr->ident |= ID_BROKEN;

                  /* Cursed */
                  i_ptr->ident |= ID_CURSED;

                  /* Penalize */
                  i_ptr->to_a = 0 - (5 + m_bonus(10, level));
                  i_ptr->p1val = 0 - (1 + m_bonus(5, level));

                  break;
               }

               /* Ring of damage */
               case SV_RING_DAMAGE:
               {
                  /* Bonus to damage */
                  i_ptr->to_d = 5 + randint(5) + m_bonus(10, level);

                  /* Cursed */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse bonus */
                     i_ptr->to_d = 0 - (i_ptr->to_d);
                  }

                  break;
               }

               /* Ring of Accuracy */
               case SV_RING_ACCURACY:
               {
                  /* Bonus to hit */
                  i_ptr->to_h = 5 + randint(5) + m_bonus(10, level);

                  /* Cursed */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse tohit */
                     i_ptr->to_h = 0 - (i_ptr->to_h);
                  }

                  break;
               }

               /* Ring of Protection */
               case SV_RING_PROTECTION:
               {
                  /* Bonus to armor class */
                  i_ptr->to_a = 5 + randint(5) + m_bonus(10, level);

                  /* Cursed */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse toac */
                     i_ptr->to_a = 0 - (i_ptr->to_a);
                  }

                  break;
               }

               /* Ring of Slaying */
               case SV_RING_SLAYING:
               {
                  /* Bonus to damage and to hit */
                  i_ptr->to_d = randint(5) + m_bonus(10, level);
                  i_ptr->to_h = randint(5) + m_bonus(10, level);

                  /* Cursed */
                  if (power < 0)
                  {
                     /* Broken */
                     i_ptr->ident |= ID_BROKEN;

                     /* Cursed */
                     i_ptr->ident |= ID_CURSED;

                     /* Reverse bonuses */
                     i_ptr->to_h = 0 - (i_ptr->to_h);
                     i_ptr->to_d = 0 - (i_ptr->to_d);
                  }

                  break;
               }
            } /* switch on tval of flavor-ring */
         } /* flavorless ring or not? */

         break;
      } /* case TV_RING */

      case TV_AMULET:
      {
         /* Analyze */
         switch (i_ptr->sval)
         {
            /* Amulet of wisdom/charisma */
            case SV_AMULET_WISDOM:
            case SV_AMULET_CHARISMA:
            {
               i_ptr->p1val = 1 + m_bonus(5, level);

               /* Cursed */
               if (power < 0)
               {
                  /* Broken */
                  i_ptr->ident |= ID_BROKEN;

                  /* Cursed */
                  i_ptr->ident |= ID_CURSED;

                  /* Reverse bonuses */
                  i_ptr->p1val = 0 - (i_ptr->p1val);
               }

               break;
            }

            /* Amulet of searching */
            case SV_AMULET_SEARCHING:
            {
               i_ptr->p1val = randint(5) + m_bonus(5, level);

               /* Cursed */
               if (power < 0)
               {
                  /* Broken */
                  i_ptr->ident |= ID_BROKEN;

                  /* Cursed */
                  i_ptr->ident |= ID_CURSED;

                  /* Reverse bonuses */
                  i_ptr->p1val = 0 - (i_ptr->p1val);
               }

               break;
            }

            /* Amulet of the Magi -- never cursed */
            case SV_AMULET_THE_MAGI:
            {
               i_ptr->p1val = randint(5) + m_bonus(5, level);
               i_ptr->to_a = randint(5) + m_bonus(5, level);

               /* Boost the rating */
               rating += 25;

               /* Mention the item */
               if (cheat_peek) object_mention(i_ptr);

               break;
            }

            /* Amulet of Doom -- always cursed */
            case SV_AMULET_DOOM:
            {
               /* Broken */
               i_ptr->ident |= ID_BROKEN;

               /* Cursed */
               i_ptr->ident |= ID_CURSED;

               /* Penalize */
               i_ptr->p1val = 0 - (randint(5) + m_bonus(5, level));
               i_ptr->to_a = 0 - (randint(5) + m_bonus(5, level));

               break;
            }
         }

         break;
      }
   }
}

/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static void a_m_aux_4(object_type *i_ptr, s16b level, s16b power)
{
   /* Apply magic (good or bad) according to type */
   switch (i_ptr->tval)
   {
      case TV_LITE:

         /* Hack -- Torches -- random fuel */
         if (i_ptr->sval == SV_LITE_TORCH)
         {
            if (i_ptr->p1val) i_ptr->p1val = randint(i_ptr->p1val);
         }

         /* Hack -- Lanterns -- random fuel */
         if (i_ptr->sval == SV_LITE_LANTERN)
         {
            if (i_ptr->p1val) i_ptr->p1val = randint(i_ptr->p1val);
         }

         break;

      case TV_WAND:

         /* Hack -- charge wands */
         charge_wand(i_ptr);

         break;

      case TV_STAFF:

         /* Hack -- charge staffs */
         charge_staff(i_ptr);

         break;

      case TV_CHEST:
         /* Hack -- skip ruined chests */
dlog(DEBUGITEMS,"object2.c: a_m_aux_4: chest, p1val %d, k_info[].level %d\n",
                i_ptr->p1val, k_info[i_ptr->k_idx].level);
         if (k_info[i_ptr->k_idx].level <= 0) break;
         i_ptr->xtra2 = 0; /* signify untrapped */
         i_ptr->p1val = 1;  /* if p1val == 0, the chest is empty */
         set_traps_chest(i_ptr, p_ptr->mdepth, 25, 5);
dlog(DEBUGITEMS,"object2.c: a_m_aux_4: chest, p1val %d after setting traps\n", i_ptr->p1val);
         /* if we didn't succeed for whatever reason */
         if (!i_ptr->xtra2)
         {
            trap_item_type *tr_ptr = &t_list[i_ptr->xtra2];
            tr_ptr->inuse = FALSE;
         }
         break;

      case TV_BOOK:
         /* we have ego-item books, now! */
         {
            if (randint(10)==1)
            {
               /* EGO_BOOK_FIRE                 enough variety?
                  EGO_BOOK_ACID
                  EGO_BOOK_FIRE_ACID
               */

               switch(randint(11))
               {
                  case  1:
                  case  2:
                  case  3:
                  case  4:
                  case  5:
                          i_ptr->name2 = EGO_BOOK_FIRE;
                          break;
                  case  6:
                  case  7:
                  case  8:
                  case  9:
                  case 10:
                          i_ptr->name2 = EGO_BOOK_ACID;
                          break;
                  case 11: 
                          i_ptr->name2 = EGO_BOOK_FIRE_ACID;
                          break;
               }
            }
         } /* case TV_BOOK */
   }
}

/*
 * this changes the dd, ds values on weapons and such
 * in 40% of cases, leading to more varied games
 */
static void change_damage_dice(object_type *i_ptr)
{
   switch (randint(10))
   {
      case 1: i_ptr->dd++;
              break;
      case 2: if (!(--i_ptr->dd))
              {
                 i_ptr->dd = 1;
                 if (!--i_ptr->ds) i_ptr->ds = 1;
              }
              break;
      case 3: i_ptr->ds++;
              break;
      case 4: if (!(--i_ptr->ds))
              {
                 i_ptr->ds = 1;
                 if (!--i_ptr->dd) i_ptr->dd = 1;
              }
              break;
   }
}

/*
 * this changes the ac values on armor and such
 * in 20% of cases, leading to more varied games
 */
static void change_armor_dice(object_type *i_ptr)
{
   switch (randint(10))
   {
      case 1: i_ptr->ac++;
              if (randint(10)==5) i_ptr->ac++;
              break;
      case 2: if (i_ptr->ac > 0) i_ptr->ac--;
              if ((randint(10)==5) && (i_ptr->ac > 0)) i_ptr->ac--;
              break;
   }
}

/*
 * Complete the "creation" of an object by applying "magic" to the item
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs, giving fuel to lites, and placing traps on chests.
 *
 * In particular, note that "Instant Artifacts", if "created" by an external
 * routine, must pass through this function to complete the actual creation.
 *
 * The base "chance" of the item being "good" increases with the "level"
 * parameter, which is usually derived from the dungeon level, being equal
 * to the level plus 10, up to a maximum of 75.  If "good" is true, then
 * the object is guaranteed to be "good".  If an object is "good", then
 * the chance that the object will be "great" (ego-item or artifact), also
 * increases with the "level", being equal to half the level, plus 5, up to
 * a maximum of 20.  If "great" is true, then the object is guaranteed to be
 * "great".  At dungeon level 65 and below, 15/100 objects are "great".
 *
 * If the object is not "good", there is a chance it will be "cursed", and
 * if it is "cursed", there is a chance it will be "broken".  These chances
 * are related to the "good" / "great" chances above.
 *
 * Otherwise "normal" rings and amulets will be "good" half the time and
 * "cursed" half the time, unless the ring/amulet is always good or cursed.
 *
 * If "okay" is true, and the object is going to be "great", then there is
 * a chance that an artifact will be created.  This is true even if both the
 * "good" and "great" arguments are false.  As a total hack, if "great" is
 * true, then the item gets 3 extra "attempts" to become an artifact.
 */
void apply_magic(object_type *i_ptr, s16b lev, bool okay, bool good, bool great)
{
   s16b i, rolls, f1, f2, power;
   s16b min_to_h, max_to_h, min_to_d, max_to_d, min_to_a, max_to_a;
   s16b min_p1val, max_p1val, min_p2val, max_p2val;

   /* Maximum "level" for various things */
   if (lev > MAX_LEVEL - 1) lev = MAX_LEVEL - 1;

   /* Base chance of being "good" */
   f1 = lev + 10;

   /* Maximal chance of being "good" */
   if (f1 > 75) f1 = 75;

   /* Base chance of being "great" */
   f2 = f1 / 2;

   /* Maximal chance of being "great" */
   if (f2 > 20) f2 = 20;

   /* Assume normal */
   power = 0;

   /* Roll for "good" */
   if (good || magik(f1))
   {
      /* Assume "good" */
      power = 1;

      /* Roll for "great" */
      if (great || magik(f2)) power = 2;
   }

   /* Roll for "cursed" */
   else if (magik(f1))
   {
      /* Assume "cursed" */
      power = -1;

      /* Roll for "broken" */
      if (magik(f2)) power = -2;
   }
   /* Assume no rolls */
   rolls = 0;

   /* Get one roll if excellent */
   if (power >= 2) rolls = 1;

   /* Hack -- Get four rolls if forced great */
   if (great) rolls = 4;

   /* Hack -- Get no rolls if not allowed */
   if (!okay || i_ptr->name1) rolls = 0;
   /* Roll for artifacts if allowed */
   for (i = 0; i < rolls; i++)
   {
      /* Roll for an artifact */
      if (make_artifact(i_ptr, FALSE)) break;
   }

   /* Hack -- analyze artifacts */
   if (i_ptr->name1)
   {
      artifact_type *a_ptr = &a_info[i_ptr->name1];

      /* Hack -- Mark the artifact as "created" */
      a_ptr->cur_num = 1;

      /* Extract the other fields */
      i_ptr->p1val = a_ptr->p1val;
      i_ptr->p2val = a_ptr->p2val;
      i_ptr->ac = a_ptr->ac;
      i_ptr->dd = a_ptr->dd;
      i_ptr->ds = a_ptr->ds;
      i_ptr->to_a = a_ptr->to_a;
      i_ptr->to_h = a_ptr->to_h;
      i_ptr->to_d = a_ptr->to_d;
      i_ptr->weight = a_ptr->weight;

      /* Hack -- extract the "broken" flag */
      if (!a_ptr->cost) i_ptr->ident |= ID_BROKEN;

      /* Hack -- extract the "cursed" flag */
      if (a_ptr->flags3 & TR3_CURSED) i_ptr->ident |= ID_CURSED;

      /* Mega-Hack -- increase the rating */
      rating += 10;

      /* Mega-Hack -- increase the rating again */
      if (a_ptr->cost > 50000L) rating += 10;

      /* Set the good item flag */
      good_item_flag = TRUE;

      /* Cheat -- peek at the item */
      if (cheat_peek) object_mention(i_ptr);

      /* Done */
dlog(DEBUGITEMS,"object2.c: apply_magic: created artifact %d (%s) object_known_p now %d\n",
                 i_ptr->name1, a_name + a_ptr->name, object_known_p(i_ptr));
      return;
   }

   /* Apply magic */
   switch (i_ptr->tval)
   {
      case TV_DIGGING:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_SWORD:
      case TV_BOW:
      case TV_SHOT:
      case TV_ARROW:
      case TV_BOLT:

         if (power) a_m_aux_1(i_ptr, lev, power);
         change_damage_dice(i_ptr);
         break;

      case TV_DRAG_ARMOR:
      case TV_HARD_ARMOR:
      case TV_SOFT_ARMOR:
      case TV_SHIELD:
      case TV_HELM:
      case TV_CROWN:
      case TV_CLOAK:
      case TV_GLOVES:
      case TV_BOOTS:
         if (power) a_m_aux_2(i_ptr, lev, power);
         change_armor_dice(i_ptr);
         break;

      case TV_RING:
      case TV_AMULET:
         if (!power && (rand_int(100) < 50)) power = -1;
         a_m_aux_3(i_ptr, lev, power);
         break;

      default:
         a_m_aux_4(i_ptr, lev, power);
         break;
   }

   /* Hack -- analyze ego-items */
   if (i_ptr->name2)
   {
      ego_item_type *e_ptr = &e_info[i_ptr->name2];

dlog(DEBUGITEMS,"object2.c: apply_magic: ego-item %s\n", e_name + e_ptr->name);

      /* Hack -- extra powers */
      switch (i_ptr->name2)
      {
         /* Quarterstaff of the Mage */
         case EGO_MAGE:
            i_ptr->xtra1 = EGO_XTRA_SUSTAIN;
            break;

         case EGO_ADEPT:
             i_ptr->xtra1 = EGO_XTRA_LOWRESIST;
             break;

         case EGO_ARCHMAGE:
            i_ptr->xtra1 = EGO_XTRA_HIGHRESIST;
            break;

         /* Weapon (Holy Avenger) */
         case EGO_HA:
            i_ptr->xtra1 = EGO_XTRA_SUSTAIN;
            break;

         /* Weapon (Defender) */
         case EGO_DF:
            i_ptr->xtra1 = EGO_XTRA_SUSTAIN;
            break;

         /* Weapon (Blessed) */
         case EGO_BLESS_BLADE:
            i_ptr->xtra1 = EGO_XTRA_ABILITY;
            break;

         /* Robe of Permanance */
         case EGO_PERMANENCE:
            i_ptr->xtra1 = EGO_XTRA_HIGHRESIST;
            break;

         /* Armor of Elvenkind */
         case EGO_ELVENKIND:
            i_ptr->xtra1 = EGO_XTRA_HIGHRESIST;
            break;

         /* Crown of the Magi */
         case EGO_MAGI:
            i_ptr->xtra1 = EGO_XTRA_ABILITY;
            break;

         /* Cloak of Aman */
         case EGO_AMAN:
            i_ptr->xtra1 = EGO_XTRA_HIGHRESIST;
            break;
      }

      /* Randomize the "xtra" power */
      if (i_ptr->xtra1) i_ptr->xtra2 = randint(256);

      /* Hack -- acquire "broken" flag */
      if (!e_ptr->cost) i_ptr->ident |= ID_BROKEN;

      /* Hack -- acquire "cursed" flag */
      if (e_ptr->flags3 & TR3_CURSED) i_ptr->ident |= ID_CURSED;

/* jk - factor in the weight option! */
      if (e_ptr->weightfactor!=100)
      {
         s32b temp = (s32b)i_ptr->weight * (s32b)e_ptr->weightfactor;
         temp = temp / 100L;
         i_ptr->weight = (s16b)temp;
         if (i_ptr->weight <= 0) i_ptr->weight = 1;
      }

      /* in what range should the bonuses be? */
      if (e_ptr->min_to_h>128)
      {
         min_to_h = -(256 - e_ptr->min_to_h);
      }
      else
      {
         min_to_h = e_ptr->min_to_h;
      }
      if (e_ptr->min_to_d>128)
      {
         min_to_d = -(256 - e_ptr->min_to_d);
      }
      else
      {
         min_to_d = e_ptr->min_to_d;
      }
      if (e_ptr->min_to_a>128)
      {
         min_to_a = -(256 - e_ptr->min_to_a);
      }
      else
      {
         min_to_a = e_ptr->min_to_a;
      }
      if (e_ptr->min_p1val>128)
      {
         min_p1val = -(256 - e_ptr->min_p1val);
      }
      else
      {
         min_p1val = e_ptr->min_p1val;
      }
      if (e_ptr->min_p2val>128)
      {
         min_p2val = -(256 - e_ptr->min_p2val);
      }
      else
      {
         min_p2val = e_ptr->min_p2val;
      }

      if (e_ptr->max_to_h>128)
      {
         max_to_h = -(256 - e_ptr->max_to_h);
      }
      else
      {
         max_to_h = e_ptr->max_to_h;
      }
      if (e_ptr->max_to_d>128)
      {
         max_to_d = -(256 - e_ptr->max_to_d);
      }
      else
      {
         max_to_d = e_ptr->max_to_d;
      }
      if (e_ptr->max_to_a>128)
      {
         max_to_a = -(256 - e_ptr->max_to_a);
      }
      else
      {
         max_to_a = e_ptr->max_to_a;
      }
      if (e_ptr->max_p1val>128)
      {
         max_p1val = -(256 - e_ptr->max_p1val);
      }
      else
      {
         max_p1val = e_ptr->max_p1val;
      }
      if (e_ptr->max_p2val>128)
      {
         max_p2val = -(256 - e_ptr->max_p2val);
      }
      else
      {
         max_p2val = e_ptr->max_p2val;
      }

dlog(DEBUGITEMS,"object2.c: apply_magic: e_ptr: min_to_h %d max_to_h %d real: min_to_h %d max_to_h %d\n",
                e_ptr->min_to_h, e_ptr->max_to_h, min_to_h, max_to_h);
dlog(DEBUGITEMS,"                        e_ptr: min_to_d %d max_to_d %d real: min_to_d %d max_to_d %d\n",
                e_ptr->min_to_d, e_ptr->max_to_d, min_to_d, max_to_d);
dlog(DEBUGITEMS,"                        e_ptr: min_to_a %d max_to_a %d real: min_to_a %d max_to_a %d\n",
                e_ptr->min_to_a, e_ptr->max_to_a, min_to_a, max_to_a);
dlog(DEBUGITEMS,"                        e_ptr: min_p1val %d max_p1val %d real: min_p1val %d max_p1val %d\n",
                e_ptr->min_p1val, e_ptr->max_p1val, min_p1val, max_p1val);
dlog(DEBUGITEMS,"                        e_ptr: min_p2val %d max_p2val %d real: min_p2val %d max_p2val %d\n",
                e_ptr->min_p2val, e_ptr->max_p2val, min_p2val, max_p2val);

dlog(DEBUGITEMS,"object2.c: apply_magic: before i_ptr->to_h %d to_d %d to_a %d p1val %ld p2val %d cursed %d broken %d\n",
                i_ptr->to_h, i_ptr->to_d, i_ptr->to_a, i_ptr->p1val, i_ptr->p2val, cursed_p(i_ptr), broken_p(i_ptr));

      /* Hack -- obtain bonuses */
      i_ptr->to_h += get_bonus(min_to_h, max_to_h,
                               cursed_p(i_ptr) || broken_p(i_ptr));
      i_ptr->to_d += get_bonus(min_to_d, max_to_d,
                               cursed_p(i_ptr) || broken_p(i_ptr));
      i_ptr->to_a += get_bonus(min_to_a, max_to_a,
                               cursed_p(i_ptr) || broken_p(i_ptr));
      i_ptr->p1val += get_bonus(min_p1val, max_p1val,
                               cursed_p(i_ptr) || broken_p(i_ptr));
      i_ptr->p2val += get_bonus(min_p2val, max_p2val,
                               cursed_p(i_ptr) || broken_p(i_ptr));

dlog(DEBUGITEMS,"object2.c: apply_magic: after i_ptr->to_h %d to_d %d to_a %d p1val %ld p2val %d\n",
               i_ptr->to_h, i_ptr->to_d, i_ptr->to_a, i_ptr->p1val, i_ptr->p2val);

      /* Hack -- apply rating bonus */
      rating += e_ptr->rating;

      /* Cheat -- describe the item */
      if (cheat_peek) object_mention(i_ptr);

      /* Done */
      return;
   }

   /* Examine real objects */
   if (i_ptr->k_idx)
   {
      object_kind *k_ptr = &k_info[i_ptr->k_idx];

      /* Hack -- acquire "broken" flag */
      if (!k_ptr->cost) i_ptr->ident |= ID_BROKEN;

      /* Hack -- acquire "cursed" flag */
      if (k_ptr->flags3 & TR3_CURSED) i_ptr->ident |= ID_CURSED;
   }
}

/*
 * Hack -- determine if a template is the right kind of object. -LM-
 */
static bool kind_is_right_kind(s16b k_idx)
{
   object_kind *k_ptr = &k_info[k_idx];

   /* We are only looking for non-worthless items of the tval asked for. */
   if ((k_ptr->tval == required_tval) && (k_ptr->cost > 0)) return (TRUE);
   else return (FALSE);
}

/*
 * Hack -- determine if a template is "good".  This concept has been consider-
 * ably expanded in Oangband. -LM-
 */
static bool kind_is_good(s16b k_idx)
{
   object_kind *k_ptr = &k_info[k_idx];

   /* Analyze the item type */
   switch (k_ptr->tval)
   {
      /* Armor -- Good unless damaged */
      case TV_HARD_ARMOR:
      case TV_SOFT_ARMOR:
      case TV_DRAG_ARMOR:
      case TV_SHIELD:
      case TV_CLOAK:
      case TV_BOOTS:
      case TV_GLOVES:
      case TV_HELM:
      case TV_CROWN:
      {
         if (k_ptr->to_a < 0) return (FALSE);
         return (TRUE);
      }

      /* Weapons -- Good unless damaged.  Diggers are no longer good. -LM- */
      case TV_BOW:
      case TV_SWORD:
      case TV_HAFTED:
      case TV_POLEARM:
      {
         if (k_ptr->to_h < 0) return (FALSE);
         if (k_ptr->to_d < 0) return (FALSE);
         return (TRUE);
      }

      /* Ammo -- Shots/Arrows/Bolts are good. -LM- */
      case TV_SHOT:
      case TV_BOLT:
      case TV_ARROW:
      {
         return (TRUE);
      }

      /* Books -- High level books are good */
      case TV_BOOK:
      {
         if (k_ptr->sval != SV_BOOK_SMALL) return (TRUE);
         return (FALSE);
      }

      /* Rings -- Rings of Speed and of Telepathy are good, as are any with a high enough value. -LM-
       */
      case TV_RING:
      {

         if (k_ptr->sval == SV_RING_SPEED) return (TRUE);
         if (k_ptr->sval == SV_RING_ESP) return (TRUE);

         if (k_ptr->cost >= 500 + p_ptr->mdepth * 90) return (TRUE);

         return (FALSE);
      }

      /* Amulets -- Amulets of the Magi are good, as are
       * any with a high enough value. -LM-
       */
      case TV_AMULET:
      {
         if (k_ptr->sval == SV_AMULET_THE_MAGI) return (TRUE);
         if (k_ptr->cost >= 500 + p_ptr->mdepth * 90) return (TRUE);

         return (FALSE);
      }

      /* Chests are always good. -LM- */
      case TV_CHEST:
      {
         return (TRUE);
      }

      /* Wands of Dragon Fire, Cold, and Breath, and of Annihilation
       * are good, as are any with a high enough value. -LM-
       */
      case TV_WAND:
      {
         if (k_ptr->sval == SV_WAND_DRAGON_FIRE) return (TRUE);
         if (k_ptr->sval == SV_WAND_DRAGON_COLD) return (TRUE);
         if (k_ptr->sval == SV_WAND_DRAGON_BREATH) return (TRUE);
         if (k_ptr->sval == SV_WAND_ANNIHILATION) return (TRUE);

         if (k_ptr->cost >= 400 + p_ptr->mdepth * 60) return (TRUE);

         return (FALSE);
      }

      case TV_STAFF:
      {
         if (k_ptr->sval == SV_STAFF_POWER) return (TRUE);
         if (k_ptr->sval == SV_STAFF_HOLINESS) return (TRUE);
         if (k_ptr->sval == SV_STAFF_GENOCIDE) return (TRUE);

         if (k_ptr->cost >= 500 + p_ptr->mdepth * 60) return (TRUE);

         return (FALSE);
      }

      case TV_ROD:
      {
         if (k_ptr->sval == SV_ROD_IDENTIFY) return (TRUE);
         if (k_ptr->sval == SV_ROD_CURING) return (TRUE);
         if (k_ptr->sval == SV_ROD_RESTORATION) return (TRUE);
         if (k_ptr->sval == SV_ROD_SPEED) return (TRUE);
         if (k_ptr->sval == SV_ROD_HEALING) return (TRUE);

         if (k_ptr->cost >= 500 + p_ptr->mdepth * 100) return (TRUE);

         return (FALSE);
      }

      /*  Any potion or scroll that costs a certain amount or more must be good. -LM- */
      case TV_POTION:
      case TV_SCROLL:
      case TV_SPELL:
      {
         if (k_ptr->cost >= 10000) return (TRUE);
         if (k_ptr->cost >= 500 + p_ptr->mdepth * 110) return (TRUE);
         return (FALSE);
      }
   }

   /* Assume not good */
   return (FALSE);
}

void create_item(object_type *i_ptr, bool good, bool great, bool exact_kind)
{
   s16b         prob = (good ? 10 : 1000);
   bool         ok1 = FALSE, ok2 = FALSE;

dlog(DEBUGITEMS,"object2.c: create_item: good %d great %d starting\n", good, great);
   while (!ok1)
   {
      object_kind *k_ptr = NULL; /* egcs complains - what can I do :-) */

      /* Base level for the object */
      s16b base = (good ? (object_level + 10) : object_level);

      /* Hack -- clear out the forgery */
dlog(DEBUGITEMS,"object2.c: create_item: about to call invwipe prob %d\n", prob);
      invwipe(i_ptr);

      ok2 = FALSE;
      while (!ok2)
      {
         /* Generate a special object, or a normal object */
         if ((rand_int(prob) == 0) && make_artifact_special(i_ptr, FALSE))
         {
            /* artifacts are always all right */
            ok2 = TRUE;
         }
         else
         {
            s16b k_idx;

            /* Good objects */
            if (good)
            {
               /* Activate restriction */
               get_obj_num_hook = kind_is_good;

               /* Prepare allocation table */
               get_obj_num_prep();
            }

            /* Objects with a particular tval.  Only activate if there is a
             * valid tval to restrict to. -LM- */
            if ((exact_kind) && (required_tval))
            {
               /* Activate restriction */
               get_obj_num_hook = kind_is_right_kind;

               /* Prepare allocation table */
               get_obj_num_prep();
            }

            /* get an object index */
            k_idx = get_obj_num(base);

/* wewe  possible return an object of (nothing) here! */
            /* Handle failure */
            if (!k_idx) return;

            /* Prepare the object */
            invcopy(i_ptr, k_idx);
            k_ptr = &k_info[k_idx];

            /* reset things - good objects */
            if (good)
            {
               /* Clear restriction */
               get_obj_num_hook = NULL;

               /* Prepare allocation table */
               get_obj_num_prep();
            }

            /* reset things - objects with a particular tval. -LM- */
            if (exact_kind)
            {
               /* Clear restriction */
               get_obj_num_hook = NULL;

               /* Prepare allocation table */
               get_obj_num_prep();
            }

            ok2 = TRUE; /* use this object */

            /* Usually (67% chance) forbid very low-level objects that are
             * intrinsically worthless. -LM-
             */
            if ((k_ptr->level < object_level / 4) &&
               (k_ptr->cost < 1) && (randint(3) != 1)) ok2 = FALSE;

            /* Sometimes (33% chance) forbid very low-level objects that aren't
             * worth very much. -LM-
             */
            else if ((k_ptr->level < object_level / 4) &&
               (k_ptr->cost < object_level - 20) &&
               (randint(3) == 1))
            {
               ok2 = FALSE;
            }

            /* Hack -- If a chest is specifically asked for, almost always
             * winnow out any not sufficiently high-level. -LM-
             */
            if (required_tval == TV_CHEST)
            {
               /* Need to avoid asking for the impossible. */
               s16b tmp = object_level > 100 ? 100 : object_level;

               if ((k_ptr->level < 2 * (tmp + 3) / 4 + randint(tmp / 4)) &&
                  (randint(10) != 1))
               {
                  ok2 = FALSE;
               }
            }
         }
      } /* created an object */

      /* Apply magic (allow artifacts) */
      apply_magic(i_ptr, object_level, TRUE, good, great);

      ok1 = TRUE; /* we have a good object we think */

      /* watching farmer maggot drop a broken stick when he has */
      /* drop_great is no fun, so we demand that a great object has some */
      /* value */
      if ((great) && (object_value(i_ptr)==0)) ok1 = FALSE;
   }

   /* Hack -- generate multiple spikes/missiles */
   /* but not for artifact arrows or other artifacts! */
   switch (i_ptr->tval)
   {
      case TV_SPIKE:
      case TV_SHOT:
      case TV_ARROW:
      case TV_BOLT:
          if (i_ptr->name1 == 0) i_ptr->number = damroll(6,7);
   }
dlog(DEBUGITEMS,"object2.c: create_item: exiting\n");
}

/* some functions, like the dungeon creation, don't mind where exactly the */
/* object is placed, or know beforehand that the floor is empty */
void place_object(s16b x, s16b y, bool good, bool great, bool exact_kind)
{
   place_object_known(&x, &y, good, great, exact_kind, FALSE);
}

/*
 * Attempt to place an object (normal or good/great) at the given location.
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "object_level" for the "generation level".
 *
 * This routine requires a clean floor grid destination.
 * it returns the value where it is actually placed in x and y
 */
void place_object_known(s16b *x, s16b *y, bool good, bool great,
                        bool exact_kind, bool wizard)
{
   s16b                 old = rating;
   object_type          forge;
   s16b                 nx, ny;

   /* Paranoia -- check bounds */
   if (!in_bounds((*x), (*y))) return;

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   create_item(&forge,good,great, exact_kind);

   /* Chance of "special object" */
   if (new_scatter(&forge, (*x), (*y), &nx, &ny))
   {
      (*x) = nx;
      (*y) = ny;
   }
   else
   {
      dlog(DEBUGITEMS,"Error - new_scatter couldn't place object in place_object");
      return;
   }

   if (!wizard)
   {
      forge.log.mlevel = p_ptr->mdepth;
      forge.log.slevel = p_ptr->sdepth;
      forge.log.whose = 0;
      forge.log.where = OBJ_FOUND_TUNNEL;
      if (dungeon.level[sublevel][(*y)][(*x)].fdat & CAVE_MAZE)  forge.log.where = OBJ_FOUND_MAZE;
      if (dungeon.level[sublevel][(*y)][(*x)].fdat & CAVE_ROOM)  forge.log.where = OBJ_FOUND_ROOM;
      if (dungeon.level[sublevel][(*y)][(*x)].fdat & CAVE_VAULT) forge.log.where = OBJ_FOUND_VAULT;
      if (dungeon.level[sublevel][(*y)][(*x)].fdat & CAVE_PIT)   forge.log.where = OBJ_FOUND_PIT;
      if (dungeon.level[sublevel][(*y)][(*x)].fdat & CAVE_NEST)  forge.log.where = OBJ_FOUND_NEST;
   }
   else
   {
      forge.log.where = OBJ_FOUND_WIZARD;
      forge.log.whose = 0;
      forge.log.mlevel = p_ptr->mdepth;
      forge.log.slevel = p_ptr->sdepth;
   }

   if (floor_carry(&forge,(*x),(*y))!=-1)
   {
      /* Notice "okay" out-of-depth objects (unless already noticed) */
      object_type *i_ptr = &forge;

      if (!cursed_p(i_ptr) && !broken_p(i_ptr) &&
          (rating == old) && (k_info[i_ptr->k_idx].level > p_ptr->mdepth))
      {
         /* Rating increase */
         rating += (k_info[i_ptr->k_idx].level - p_ptr->mdepth);

         /* Cheat -- peek at items */
         if (cheat_peek) object_mention(i_ptr);
      }
      optimize_floor((*x),(*y));
   }
   else
     quit("Location found in place_object_known was already full!");
}

/*
 * Scatter some "great" objects near the player
 */
void acquirement(s16b x1, s16b y1, s16b num, bool great, bool wizard)
{
   s16b        x, y;

   /* Scatter some objects */
   for (; num > 0; --num)
   {
      x = x1;
      y = y1;
      place_object_known(&x, &y, TRUE, great, wizard, FALSE);

      /* Notice */
      note_spot(x, y);

      /* Redraw */
      lite_spot(x, y);

      /* Under the player */
      if ((y == py) && (x == px))
      {
         /* Message */
         msg_print ("You feel something roll beneath your feet.");
      }
      /* Placement accomplished */
   }
}

void create_gold_item(object_type *i_ptr)
{
   s16b base, k_idx;
   s16b i     = ((randint(object_level + 2) + 2) / 2) - 1;

   /* Apply "extra" magic */
   if (rand_int(GREAT_OBJ) == 0) i += randint(object_level + 1);

   /* Hack -- Creeping Coins only generate "themselves" */
   if (coin_type) i = coin_type;

   /* Do not create "illegal" Treasure Types */
   if (i > SV_GOLD_END) i = SV_GOLD_END;

   /* Make an object */
   k_idx = lookup_kind(TV_GOLD, SV_GOLD_START+i);

   invcopy(i_ptr, k_idx);

   /* Hack -- Base coin cost */
   base = k_info[k_idx].cost;

   /* Determine how much the treasure is "worth" */
   i_ptr->p1val = (base + (8L * randint(base)) + randint(8));
/* jk - treasure can be 1 piece worth 42, or 10 pieces worth 4 or 6x7 etc */
   while (i_ptr->p1val > (base+randint(base*3)))
   {
      s16b f = randint(5);
      if ((s16b)(i_ptr->p1val / f)<=(base/4)) break;
      i_ptr->number *= f;
      i_ptr->p1val /= f;
   }
}

void give_monster_gold(s16b m_idx, s32b gold)
{
   object_type forge;
   bool        go_on = TRUE;
   s32b        amount = gold;
   s32b        base;

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   while (go_on)
   {
      create_gold_item(&forge);
      forge.number = 1;
      forge.p1val = amount;
dlog(DEBUGITEMS,"object2.c: give_monster_gold starting loop with amount %ld number %d\n", amount, forge.number);
      base = (s32b)k_info[forge.k_idx].cost;

      while ((forge.p1val>10) && (forge.p1val > (base+randint(base*3))))
      {
         s32b f = randint(5);
dlog(DEBUGITEMS,"object2.c: give_monster_gold: in loop num %d p1val %ld, base %ld f %ld\n",
             forge.number, forge.p1val, base, f);
         if ((forge.p1val / f)<=(base/4)) break;
         if (((s32b)forge.number*(s32b)f)>255)
         {
            /* number is only a byte, so prevent any overflow by getting out */
            /* of the loop here. Make sure p1val contains a moderate value per */
            /* piece, pieces of copper each worth 6134 AU seem silly.        */
            /* we just make more gold-items if we have any amount left, and  */
            /* hope we can make enough.....*/
            forge.p1val = (base + randint(base*3));
            break;
         }
         forge.number *= (byte)f;
         forge.p1val /= f;
      }
dlog(DEBUGITEMS,"object2.c: give_monster_gold: after loop num %d p1val %ld, base %ld\n", forge.number, forge.p1val, base);
      if (monster_inven_carry(m_idx,&forge)==-1)
      {
         /* the monster inventory was full - object lost */
         msg_print("You have a bad feeling about this theft.");
         /* we need not give any more gold now */
         go_on = FALSE;
      }
      else
      {
         amount -= forge.p1val * forge.number;
         /* don't bother with amounts below 10 + 10% of the original amount */
         if (amount<randint(10+(gold/10))) go_on = FALSE;
dlog(DEBUGITEMS,"object2.c: give_monster_gold: continuing with amount %ld\n",
             amount);

      }
   }
}

/*
 * Places a treasure (Gold or Gems) at given location
 * The location must be a valid, floor grid.
 * jk - empty not necessary
 */
void place_gold(s16b x, s16b y)
{
   place_gold_known(&x, &y);
}

void place_gold_known(s16b *x, s16b *y)
{
   s16b         nx, ny;
   object_type forge;

   /* Paranoia -- check bounds */
   if (!in_bounds((*x), (*y))) return;

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   /* Hack -- Pick a Treasure variety */
   create_gold_item(&forge);

   /* just x and y here, because drop_near expects *x and *y */
   if (new_scatter(&forge, (*x), (*y), &nx, &ny))
   {
      (*x) = nx;
      (*y) = ny;
   }
   else
   {
      dlog(DEBUGITEMS,"Error - new_scatter couldn't place object in place_object");
      return;
   }
   if (floor_carry(&forge,(*x),(*y))==-1)
   {
      quit("Location found in place_gold was already full");
   }
}

/*
 * Let an item 'i_ptr' fall to the ground at or near (y,x).
 * The initial location is assumed to be "in_bounds()".
 *
 * This function takes a parameter "chance".  This is the percentage
 * chance that the item will "disappear" instead of drop.  If the object
 * has been thrown, then this is the chance of disappearance on contact.
 *
 * Hack -- this function uses "chance" to determine if it should produce
 * some form of "description" of the drop event (under the player).
 *
 * This function should probably be broken up into a function to determine
 * a "drop location", and several functions to actually "drop" an object.
 *
 * XXX XXX XXX Consider allowing objects to combine on the ground.
 */
bool new_scatter(object_type *i_ptr, s16b x, s16b y, s16b *nx, s16b *ny)
{
   /* we look at a 7 by 7 square */
   const s16b   hsize = 3; /* meaning we start at -3 from x to x+3 etc */
   bool         flag = FALSE;
   u16b         objs[7*7]; /* Watcom 10.6 complains if you put size*size */
   s16b         best_x[7*7];
   s16b         best_y[7*7];
   s16b         best_o, cnt, best_cnt, dst, curdst, curobj;
   s16b         x1, y1, k, dis = 0;
   char         i_name[80];

/* we follow this strategy:
 *
 * we first look at a square block from x,y-1 to x,y+1
 * if we found a 0 objects-solution from that block, we continue, else we
 * we look at the full square block size by size,
 * and first we determine where in that square dropping is allowed
 * allowed means we can see it, it's in bounds, and there's no door etc. there
 * then we look for the square within our reach that has the least items already
 * there.
 * if there's more than one of those, we find the closest one
 * if there's more than one of those, we pick a random one
 * if the item's an artifact and we have no space, we kill something
 * if we still have no space, the item is lost
 */
   /* quick exit if no hassle needed: */
   if (objects_on_floor_absorb(i_ptr,x,y)==1)
   {
      (*nx) = x;
      (*ny) = y;
      return (TRUE);
   }
   /* search the area around the player */
   cnt = 0;
   best_o = MAX_FLOOR_ITEMS;
   best_cnt = 0;

/* first we search a small block, for speed reasons! */
   for (x1=(x-1);x1<=(x+1);x1++)
   {
      for (y1=(y-1);y1<=(y+1);y1++)
      {
         objs[cnt]=256*dis+255;
         if (in_bounds(x1,y1))
         {
            dis = 1; /* this is also for speed! */
            if (cnt==4) dis = 0; /* the middle 0,1,2,3 4 */
            /* 255 is a signal the square is unusable
             * if the square is within bounds
             * this implementation depends on distance never being greater than
             * 255, and items also <256 at all times - that's safe!
             */
            /* and the terrain allows objects & we are in a correct circle */
            if (floor_grid_bold(x1,y1) && (dis<=hsize))
            {
               /* and the drop origin can see it */
               if (los(x,y,x1,y1))
               {
                  objs[cnt]-=255; /* now it's usable */
                  objs[cnt]+=objects_on_floor_absorb(i_ptr,x1,y1); /* cnt was increased */
               }
            }
         }
         curobj = objs[cnt]%256;
         if (curobj==255) continue; /* square isn't usable */
         if (curobj<best_o)        /* store a new best solution */
         {
            best_cnt = 0;
            best_x[best_cnt] = x1;
            best_y[best_cnt++] = y1;
            best_o = curobj;
         }
         else if (curobj==best_o)  /* a new as good as solution */
         {
            best_x[best_cnt] = x1;
            best_y[best_cnt++] = y1;
         }
         cnt++; /* now for the next square */
      }
   }
/* best_o ==1 means 0 objects on floor, or 1 that can absorb */
   if (best_o>1)
   {
      cnt = 0;
      best_o = MAX_FLOOR_ITEMS;
      best_cnt = 0;
      for (x1=(x-hsize);x1<=(x+hsize);x1++)
      {
         for (y1=(y-hsize);y1<=(y+hsize);y1++)
         {
            objs[cnt]=256*dis+255;
            if (in_bounds(x1,y1))
            {
               dis = distance(x,y,x1,y1);
               /* 255 is a signal the square is unusable
                * if the square is within bounds
                * this implementation depends on distance never being greater than
                * 255, and items also <256 at all times - that's safe!
                *
                * and the terrain allows objects & we are in a correct circle */

               if (floor_grid_bold(x1,y1) && (dis<=hsize))
               {
                  /* and we can see it */
                  if (los(x,y,x1,y1))
                  {
                     objs[cnt]-=255; /* now it's usable */
                     objs[cnt]+=objects_on_floor_absorb(i_ptr,x1,y1); /* cnt was increased */
                  }
               }
            }
            curobj = objs[cnt]%256;
            if (curobj==255) continue; /* square isn't usable */
            if (curobj<best_o)        /* store a new best solution */
            {
               best_cnt = 0;
               best_x[best_cnt] = x1;
               best_y[best_cnt++] = y1;
               best_o = curobj;
            }
            else if (curobj==best_o)  /* a new as good as solution */
            {
               best_x[best_cnt] = x1;
               best_y[best_cnt++] = y1;
            }
            cnt++; /* now for the next square */
         } /* end y loop */
      } /* end x loop */
   } /* end searching in big block */
   if (best_cnt>1) /* if we have more best locations */
   /* we find the best solution in terms of distance */
   {
      cnt = 0;
      dst = hsize+1;     /* we hope to get closer than this */
      for (k=0;k<best_cnt;k++)
      {
         curdst = distance(best_x[k],best_y[k],x,y); /* a closer one found? */
         if (curdst<dst)
         {
            cnt = 0;
            best_x[cnt]=best_x[k];
            best_y[cnt++]=best_y[k];
            dst = curdst;                             /* use that one */
         }
         else if (curdst==dst)                  /* to be sure we use a random */
         {                                      /* best one */
            best_x[cnt]=best_x[k];
            best_y[cnt++]=best_y[k];
         }
      }
      /* we now have cnt best items in 0..cnt-1 */
   }
   else
   {
      cnt = best_cnt; /* we found only one */
   }
   (*nx) = 0; /* only to stop gcc from complaining about these being un */
   (*ny) = 0; /* initialized */
   flag = (cnt>0);   /* if we found none, there's a problem */
   if (flag)
   {
      best_o = rand_int(cnt);
      (*nx) = best_x[best_o];
      (*ny) = best_y[best_o];
      flag = (objects_on_floor_absorb(i_ptr,(*nx),(*ny))<=ITEM_SET_SIZE);
   }
   if (!flag && artifact_p(i_ptr))
   {
      best_o = rand_int(cnt);
      (*nx) = best_x[best_o];
      (*ny) = best_y[best_o];
      object_desc(i_name, i_ptr, FALSE, 0);
      /* Message */
      msg_format("The %s crashes to the floor.", i_name);
      delete_object((*nx),(*ny),rand_int(ITEM_SET_SIZE));
      flag = TRUE;
   }
   return (flag);
}

static void print_message_drop(object_type *i_ptr, s16b actionflag, bool visible, s16b number)
{
   char        i_name[80];
   char        prefix[2][15];

   object_desc(i_name, i_ptr, FALSE, 0x40);
   if (i_ptr->number>1)
   {
      strcpy(prefix[1],""); /* plural added by object_desc */
      if (number==1)
         strcpy(prefix[0],"one of the ");
      else if (number==i_ptr->number)
         strcpy(prefix[0],"all of the ");
      else
         strcpy(prefix[0],"some of the ");
   }
   else
   {
      strcpy(prefix[1],""); /* no plural */
      strcpy(prefix[0],is_a_vowel(i_name[0])?"an ":"a ");
   }

/* jk - is this right when dealing with artifacts? The messages may not be */
/* right, but they don't disappear either */
dlog(DEBUGITEMS,"object2.c: print_message_drop actionflag %d visib %d num %d\n",
            actionflag, visible, number);
dlog(DEBUGITEMS,"                               tval %d sval %d p1val %ld name %s\n",
           i_ptr->tval, i_ptr->sval, i_ptr->p1val, i_name);
dlog(DEBUGITEMS,"                               prefix[0] %s prefix[1] %s\n",
            prefix[0], prefix[1]);

   if (!drop_messages) return;

   if (p_ptr->blind) visible = FALSE;

   switch (actionflag)
   {
      case DROP_SINK:
         if (visible)
         {
             msg_format("You see %s%s sink%s to the bottom.",
                        prefix[0],
                        i_name, prefix[1]);
             break;
         }
      case DROP_BURN:
         if (visible)
         {
             msg_format("You see %s%s sink%s burning into the lava.",
                        prefix[0],
                        i_name, prefix[1]);
             break;
         }

      case DROP_DISAPPEAR:
         switch (randint(4))
         {
            case 0:
               i_name[0] = FORCEUPPER(i_name[0]);
               if (visible)
               {
                   msg_format("%s%s disappear%s.",
                              prefix[0],
                              i_name, prefix[1]);
                   break;
               }
            case 1:
               if (visible)
               {
                  msg_format("You hear %s%s%s drop down a very deep hole.",
                             prefix[0], i_name,prefix[1]);
               }
               else
               {
                  msg_print("You hear something drop down a very deep hole.");
               }
               break;
            case 2:
               if (visible)
                {
                  msg_format("You see %s%s%s bounce out of sight.",
                             prefix[0],i_name, prefix[1]);
               }
               else
               {
                  msg_print("You hear a series of diminishing thuds.");
               }
            case 3:
               if (visible)
               {
                  msg_format("You hear %s%s%s falling with a curious sound.",
                             prefix[0],i_name,prefix[1]);
               }
               else
               {
                  msg_print("You hear a something falling with a curious sound.");
               }
               break;
         }
         break;
      case DROP_SHATTER:
         switch (randint(3))
         {
            case 0:
               if (visible)
                  msg_format("You see %s%s%s shatter into tiny pieces.",
                             prefix[0],i_name,prefix[1]);
               else
                  msg_print("You hear something shatter into tiny pieces.");
               break;
            case 1:
               if (visible)
               {
                  msg_format("You hear the sound of %s%s%s breaking on the ground.",
                             prefix[0],i_name,prefix[1]);
               }
               else
               {
                  msg_print("You hear something breaking.");
               }
               break;
            case 2:
               if (visible)
               {
                  msg_format("You see %s%s%s land in a dozen pieces.",
                            prefix[0], i_name, prefix[1]);
               }
               else
               {
                  msg_print("You hear something land in a dozen pieces.");
               }
               break;
         }
         break;
      case DROP_BREAK:
         if (visible)
         {
            msg_format("On falling, %s%s%s break%s into small fragments.",
                        prefix[0], i_name, prefix[1], number>1 ? "":"s");
         }
         else
         {
            msg_print("You hear a lot of small parts fall rattling on the floor.");
         }
         break;
      case DROP_CRUMBLE:
         if (visible)
         {
            msg_format("You see %s%s%s drop and crumble to dust.",
                       prefix[0], i_name, prefix[1]);
         }
         else
         {
            msg_print("You hear a strange crumbling sound.");
         }
         break;
      case DROP_FISSURE:
         if (visible)
         {
            msg_format("You see %s%s%s disappear into a small fissure in the floor.",
                       prefix[0], i_name, prefix[1]);
         }
         else
         {
            msg_print("You hear a small sound from far, far below.");
         }
         break;
   }
dlog(DEBUGITEMS,"object2.c: print_message_drop returning\n");
}

static void print_message_hit(object_type *i_ptr, s16b actionflag, bool visible, s16b number,
                              bool hit_wall)
{
   char        i_name[80];
   char        prefix[15];

   object_desc(i_name, i_ptr, FALSE, 0x40);
   strcpy(prefix,is_a_vowel(i_name[0])?"an ":"a ");

/* jk - is this right when dealing with artifacts? The messages may not be */
/* right, but they don't disappear either */
dlog(DEBUGITEMS,"object2.c: print_message_hit actionflag %d visib %d num %d\n",
           actionflag, visible, number);
dlog(DEBUGITEMS,"                             tval %d sval %d p1val %ld name %s\n",
          i_ptr->tval, i_ptr->sval, i_ptr->p1val, i_name);
dlog(DEBUGITEMS,"                             prefix %s\n", prefix);

   if (!drop_messages) return;

   switch (actionflag)
   {
      case DROP_DISAPPEAR:
         msg_format("The %s disappears.", i_name);
         break;
      case DROP_SHATTER:
         msg_format("The %s shatters.", i_name);
         break;
      case DROP_BREAK:
         msg_format("The %s breaks.", i_name);
         break;
      case DROP_CRUMBLE:
         msg_format("The %s crumbles to dust on impact.", i_name);
         break;
      case DROP_FISSURE:
         msg_format("The %s bounces away and disappears into a small fissure.", i_name);
         break;
   }
dlog(DEBUGITEMS,"object2.c: print_message_hit returning\n");
}


bool drop_near(object_type *i_ptr, s16b break_number, s16b x, s16b y,
               s16b actionflag, bool hit_body, bool hit_wall)
{
   s16b         ny, nx;
   char         i_name[80];

   bool visible = ( (distance(px,py,x,y)<MAX_SIGHT) &&
                    (dungeon.level[sublevel][y][x].fdat & CAVE_VIEW) );
dlog(DEBUGEXTRA,"object2.c: drop_near: i_ptr->number now %d\n", i_ptr->number);


   if (!artifact_p(i_ptr) && (break_number>0))
   {
      if (player_has_los_bold(x,y))
      {
         if (hit_body || hit_wall)
         {
            print_message_hit(i_ptr, actionflag, visible, break_number, hit_wall);
         }
         else
         {
            print_message_drop(i_ptr,actionflag,visible, break_number);
         }
      }
      i_ptr->number -= break_number;
      if (!i_ptr->number) return(FALSE); /* object has ceased to exist */
   }

   object_desc(i_name, i_ptr, TRUE, 0x40);

   if (new_scatter(i_ptr, x, y, &nx, &ny))
   {
      cave_cell_type *c_ptr = &dungeon.level[sublevel][ny][nx];
      if (!artifact_p(i_ptr))
      {
         if (c_ptr->mtyp == DUNG_WATER)
         {
            print_message_drop(i_ptr, DROP_SINK, visible, i_ptr->number);
            i_ptr->number = 0;
            return(FALSE); /* object has ceased to exist */
         }

         if (c_ptr->mtyp == DUNG_LAVA)
         {
            print_message_drop(i_ptr, DROP_BURN, visible, i_ptr->number);
            i_ptr->number = 0;
            return(FALSE); /* object has ceased to exist */
         }
      }

      /* new_scatter finds the optimal place, so TRUE */
      if (floor_carry(i_ptr,nx,ny)!=-1)
      {
dlog(DEBUGEXTRA,"object2.c: drop_near: after floor_carry i_ptr->number now %d\n", i_ptr->number);
         optimize_floor(nx,ny);
dlog(DEBUGEXTRA,"object2.c: drop_near: after floor_carry optimize i_ptr->number now %d\n", i_ptr->number);

         if (player_has_los_bold(nx,ny))
         {
            visible = ( (distance(px,py,nx,ny)<MAX_SIGHT) &&
                        (dungeon.level[sublevel][ny][nx].fdat & CAVE_VIEW) );
            if (visible)
            {
               note_spot(nx, ny);

               /* Draw the spot */
               lite_spot(nx, ny);

               /* messages for the player's square are handled below */
               if ((nx != px) || (ny != py))
               {
                  if (drop_messages) msg_format("You see %s drop.",i_name);
               }
            }
            else
            {
               if (drop_messages)
               {
                  msg_print("You hear something fall to the floor.");
               }
            }

         }
dlog(DEBUGEXTRA,"object2.c: drop_near: after messages1\n");

         /* Sound'*/
         sound(SOUND_DROP);

         /* Mega-Hack -- no message if "dropped" by player */
         /* jk - why not? */
         /* Message when an object falls under the player */
         if ((ny == py) && (nx == px))
         {
            if (objects_on_floor(nx,ny)==1)
            {
               if (drop_messages)
               {
                  msg_print("You feel something roll beneath your feet.");
               }
            }
            else
            {
               if (drop_messages)
               {
                  msg_print("You feel a pile build beneath your feet.");
               }
            }
         }
dlog(DEBUGEXTRA,"object2.c: drop_near: after messages2\n");
         /* Success */
         return (TRUE);
      }
      else /* this is strange: new_scatter said OK, while floor_carry didn't */
      {
         quit(format("object2.c: drop_near: new_scatter %d,%d vs. floor_carry discordancy", nx,ny));
         return(FALSE); /* else gcc complains */
      }
   }
   else
   {
      /* we can't scatter it, so display a message ALL of it is broken */
      print_message_drop(i_ptr,actionflag,visible, i_ptr->number);
      return (FALSE);
   }
}

/*
 * Describe the charges on an item in the inventory.
 */
/* jk - function made compatible with both inven & floor_items */
void item_charges(s16b item)
{
   object_type *i_ptr = NULL;
   char buf[10];

   i_ptr=get_item_pointer(item);

   if (item<INVEN_TOTAL)
   {
      strcpy(buf,"You have");
   }
   else
   {
      strcpy(buf,"There are");
   }

   /* Require staff/wand */
   if ((i_ptr->tval != TV_STAFF) && (i_ptr->tval != TV_WAND)) return;
   /* Require known item */
   if (!object_known_p(i_ptr)) return;
   /* Multiple charges */
   if (i_ptr->p1val != 1)
   {
      msg_format("%s %d charges remaining.", buf, i_ptr->p1val); }
   else
   {
      msg_format("%s %d charge remaining.", buf, i_ptr->p1val);
   }
}

/*
 * Describe an item in the inventory.
 */
/* jk - reworked to allow for floor-items */
/* x,y are necessary - seeing can be elsewhere! */
/* it's assumed y,x point to a valid location with an object in it */
void item_describe(s16b item, s16b x, s16b y)
{
   object_type *i_ptr = NULL;
   char        i_name[80];
   char        buf[10];

   i_ptr=get_item_pointer_xy(item, x, y);

   if (item<INVEN_TOTAL)
   {
      strcpy(buf,"You have");
   }
   else
   {
      strcpy(buf,"You see");
   }

   /* Get a description */
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Print a message */
   msg_format("%s %s.", buf, i_name);
}

/*
 * Prepare an object based on an object kind.
 */
void object_prep(object_type *o_ptr, int k_idx)
{
   object_kind *k_ptr = &k_info[k_idx];

   /* Clear the record */
   WIPE(o_ptr, object_type);

   /* Save the kind index */
   o_ptr->k_idx = k_idx;

   /* Efficiency -- tval/sval */
   o_ptr->tval = k_ptr->tval;
   o_ptr->sval = k_ptr->sval;

   /* Default "p1val" */
   o_ptr->p1val = k_ptr->p1val;

   /* Default number */
   o_ptr->number = 1;

   /* Default weight */
   o_ptr->weight = k_ptr->weight;

   /* Default magic */
   o_ptr->to_h = k_ptr->to_h;
   o_ptr->to_d = k_ptr->to_d;
   o_ptr->to_a = k_ptr->to_a;

   /* Default power */
   o_ptr->ac = k_ptr->ac;
   o_ptr->dd = k_ptr->dd;
   o_ptr->ds = k_ptr->ds;

   /* Hack -- worthless items are always "broken" */
   if (k_ptr->cost <= 0) o_ptr->ident |= (ID_BROKEN);

   /* Hack -- cursed items are always "cursed" */
   if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (ID_CURSED);
}
