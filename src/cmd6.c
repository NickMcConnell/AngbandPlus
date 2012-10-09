/* File: cmd6.c */

/* Purpose: Object commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * This file includes code for eating food, drinking potions,
 * reading scrolls, aiming wands, using staffs, zapping rods,
 * and activating artifacts.
 *
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up.  If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * This code now correctly handles the unstacking of wands, staffs,
 * and rods.  Note the overly paranoid warning about potential pack
 * overflow, which allows the player to use and drop a stacked item.
 *
 * In all "unstacking" scenarios, the "used" object is "carried" as if
 * the player had just picked it up.  In particular, this means that if
 * the use of an item induces pack overflow, that item will be dropped.
 *
 * For simplicity, these routines induce a full "pack reorganization"
 * which not only combines similar items, but also reorganizes various
 * items to obey the current "sorting" method.  This may require about
 * 400 item comparisons, but only occasionally.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "i_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */
static bool item_tester_hook_food(object_type *i_ptr)
{
   return ((i_ptr->tval == TV_FOOD) || (i_ptr->tval == TV_CORPSE));
}

bool handle_food_gives(object_type *i_ptr, s16b fresh, u32b flag)
{
   char i_name[80];
   bool ident = FALSE;
   object_desc(i_name, i_ptr, FALSE, 3);

dlog(DEBUGITEMS,"cmd6.c: handle_food_gives flag %08lx fresh %d\n", flag, fresh);
   if (flag & CORPSE_GIVES_ACID) ident=set_acid(p_ptr->acid+rand_int(20)+20);
   if (flag & CORPSE_GIVES_FIRE) ident=set_fire(p_ptr->fire+rand_int(20)+20);
   if (flag & CORPSE_GIVES_COLD) ident=set_cold(p_ptr->cold+rand_int(20)+20);
   if (flag & CORPSE_GIVES_ELEC) ident=set_elec(p_ptr->elec+rand_int(20)+20);
   if (flag & CORPSE_GIVES_STR) ident=do_inc_stat(A_STR, FALSE);
   if (flag & CORPSE_GIVES_DEX) ident=do_inc_stat(A_DEX, FALSE);
   if (flag & CORPSE_GIVES_INT) ident=do_inc_stat(A_INT, FALSE);
   if (flag & CORPSE_GIVES_WIS) ident=do_inc_stat(A_WIS, FALSE);
   if (flag & CORPSE_GIVES_CON) ident=do_inc_stat(A_CON, FALSE);
   if (flag & CORPSE_GIVES_CHR) ident=do_inc_stat(A_CHR, FALSE);
   if (flag & CORPSE_GIVES_BLINDNESS)
      ident=set_blind(p_ptr->blind+rand_int(200)+200);
   if (flag & CORPSE_GIVES_CONFUSION)
      ident=set_confused(p_ptr->confused + rand_int(200) + 200);
   if (flag & CORPSE_GIVES_VOMIT)
   {
      msg_print("This is very very bad to eat!");
      (void)set_food(PY_FOOD_STARVE - 1);
      (void)set_poisoned(10);
      if (!p_ptr->free_act)
          (void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
      ident = TRUE;
   }
   if (flag & CORPSE_GIVES_TELEPORT)
   {
      msg_print("You feel dizzy for a moment.");
      teleport_player(67*RATIO);
      ident = TRUE;
   }
   if (flag & CORPSE_GIVES_BERSERKER)
   {
      hp_player(30);
      set_afraid(0);
      set_shero(p_ptr->shero + randint(25) + 25);
   }
   if (flag & CORPSE_GIVES_PEARL)
   {
       s16b k_idx = lookup_kind(TV_CORPSE, SV_GOLD_PEARL);
       object_type item;
       invcopy(&item, k_idx);
       item.p1val = 1;
       if (drop_near(&item, 0, px, py, 0, FALSE, FALSE))
       {
          msg_print("You see a pearl drop from this corpse!");
          ident = TRUE;
       }
    }
   if (flag & CORPSE_GIVES_POISON)
   {
      cptr adj[15] = { " smelling", " a little", " a bit", "", " rather",
                      " badly", " very", " dangerously", " highly", " deadly" };
      if (fresh>0)
      {
         ident=set_poisoned(fresh);
         msg_format("This %s is%s poisonous.",i_name, adj[fresh/10]);
      }
   }
   if (flag & CORPSE_GIVES_BADPOISON)
   {
      cptr adj[15] = { " smelling", " a little", " a bit", "", " rather",
                      " badly", " very", " dangerously", " highly", " deadly" };
      ident=set_poisoned(fresh*2);
      msg_format("This %s is%s poisonous.",i_name,
                 adj[((fresh/5)>9)?9:(fresh/5)]);
   }
   if (flag & CORPSE_GIVES_PARALYZE)
   {
      msg_print("Your stomach rumbles heavily; you rest a few turns.");
      ident=set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
   }
   if (flag & CORPSE_GIVES_SLEEP)
   {
      msg_print("You suddenly feel very sleepy.");
      ident=set_paralyzed(p_ptr->paralyzed + rand_int(20) + 10);
   }
   if (flag & CORPSE_GIVES_MANA)
   {
      if (p_ptr->csp < p_ptr->msp)
      {
         p_ptr->csp = p_ptr->msp;
         p_ptr->csp_frac = 0;
         msg_print("Your feel your head clear.");
         p_ptr->redraw1 = (PR1_MANA);
         ident = TRUE;
      }
   }
   if (flag & CORPSE_GIVES_SCREAM)
   { 
      project_who_type who;
      msg_print("This food makes you burp very loud - you feel uneasy about the noise.");
      who.type = WHO_PLAYER;
      ident = aggravate_monsters(&who, -1);
   }
   if (flag & CORPSE_GIVES_BADSCREAM)
   {
      msg_print("An anguished cry escapes your throat as this food passes.");
      (void)do_player_trap_call_out();
   }
   if (flag & CORPSE_GIVES_INFLATE)
   {
      msg_print("This seems to blow up in your stomach...");
      (void)set_food(p_ptr->food*5);
   }
   if (flag & CORPSE_GIVES_FORGET)
   {
      msg_print("You suddenly think of ages past and have trouble remembering what you are doing.");
      (void)lose_some_info(100);
   }

   if (flag & CORPSE_GIVES_THROAT)
      ident=set_throat(p_ptr->throat+rand_int(25)+25);
   if (flag & CORPSE_GIVES_BADTHROAT)
      ident=set_throat(p_ptr->throat + rand_int(75) + 75);
   if (flag & CORPSE_GIVES_NEXUS)
   {
      if (p_ptr->resist_nexus)
      {
         msg_print("You bowels scramble wildly.");
         take_hit(damroll((fresh/10)+1, 6),
                  format("%s which scrambled your bowels.", i_name));
      }
      else
      {
         s16b max1, cur1, max2, cur2, ii, jj;
         switch (randint(7))
         {
            case 1:
            case 2:
            case 3: teleport_player(67*RATIO);
                    break;

            case 4:
            case 5: teleport_player(10);
                    break;

            case 6: if (rand_int(100) < p_ptr->skill_sav)
                    {
                       msg_print("You resist the effects!");
                       break;
                    }
                    /* Teleport Level */
                    teleport_player_level();
                    break;

            case 7: if (rand_int(100) < p_ptr->skill_sav)
                    {
                        msg_print("You resist the effects!");
                        break;
                    }

                    msg_print("Your body starts to scramble...");

                    /* Pick a pair of stats */
                    ii = rand_int(6);
                    for (jj = ii; jj == ii; jj = rand_int(6)) ;

                    max1 = p_ptr->stat_max[ii];
                    cur1 = p_ptr->stat_cur[ii];
                    max2 = p_ptr->stat_max[jj];
                    cur2 = p_ptr->stat_cur[jj];

                    p_ptr->stat_max[ii] = max2;
                    p_ptr->stat_cur[ii] = cur2;
                    p_ptr->stat_max[jj] = max1;
                    p_ptr->stat_cur[jj] = cur1;

                    p_ptr->update = (PU_BONUS);
                    ident = TRUE;
                    break;
         } /* switch */
         ident = TRUE;
      }
   }
   if (flag & CORPSE_GIVES_BLINK)
   {
      msg_print("You suddenly stand somewhere else.");
      teleport_player(10);
      ident = TRUE;
   }
   if (flag & CORPSE_GIVES_SPEED)
   {
      if (!p_ptr->fast)
      {
          ident=(set_fast(randint(25) + 15));
      }
      else
      {
          (void)set_fast(p_ptr->fast + 5);
      }
   }
   return (ident);
}

static bool handle_food_takes(object_type *i_ptr, s16b fresh, u32b flag)
{
   char i_name[80];
   bool ident = FALSE;
   s16b dec_stat_mode;

   if (randint(200)<fresh)
      dec_stat_mode = STAT_DEC_TEMPORARY;
   else
      dec_stat_mode = STAT_DEC_NORMAL;

   object_desc(i_name, i_ptr, FALSE, 3);

dlog(DEBUGITEMS,"cmd6.c: handle_food_takes flag %08lx fresh %d\n", flag, fresh);
   if (flag & CORPSE_TAKES_ACID) ident=set_acid(0);
   if (flag & CORPSE_TAKES_FIRE) ident=set_fire(0);
   if (flag & CORPSE_TAKES_COLD) ident=set_cold(0);
   if (flag & CORPSE_TAKES_ELEC) ident=set_elec(0);
   if (flag & CORPSE_TAKES_STR) ident=do_dec_stat(A_STR, dec_stat_mode);
   if (flag & CORPSE_TAKES_DEX) ident=do_dec_stat(A_DEX, dec_stat_mode);
   if (flag & CORPSE_TAKES_INT) ident=do_dec_stat(A_INT, dec_stat_mode);
   if (flag & CORPSE_TAKES_WIS) ident=do_dec_stat(A_WIS, dec_stat_mode);
   if (flag & CORPSE_TAKES_CON) ident=do_dec_stat(A_CON, dec_stat_mode);
   if (flag & CORPSE_TAKES_CHR) ident=do_dec_stat(A_CHR, dec_stat_mode);
   if (flag & CORPSE_TAKES_BLINDNESS) ident=set_blind(0);
   if (flag & CORPSE_TAKES_CONFUSION) ident=set_confused(0);
   if (flag & CORPSE_TAKES_POISON)
      if (randint(100)<fresh) ident=set_poisoned(p_ptr->poisoned-50);
   if (flag & CORPSE_TAKES_BADPOISON)
      if (randint(50)<fresh) ident=set_poisoned(0);
   if (flag & CORPSE_TAKES_MANA)
   {
      if (p_ptr->csp>0)
      {
         p_ptr->csp = 0;
         p_ptr->csp_frac = 0;
         p_ptr->redraw1 = (PR1_MANA);
         msg_print("You sense a great loss.");
         ident=TRUE;
      }
   }
   if (flag & CORPSE_TAKES_SPEED)
      if (set_slow(p_ptr->slow + randint(25) + 15)) ident=TRUE;
   return (ident);
}

/* jk - chooses a random set bit out of flag, and returns that one */
static u32b random_set_bit(u32b flag)
{
   byte index[32];
   s16b i, cnt = 0;

dlog(DEBUGITEMS,"cmd6.c: random_set_bit entered with flag %08lx\n", flag);
   if (!flag) return 0L;   /* no bits set? return 0    */

   for (i = 0; i<32; i++)
   {
      if (flag & (1<<i)) index[cnt++]=i;  /* bit found? save index  */
   }
   i = randint(cnt)-1;     /* choose random bit number */
dlog(DEBUGITEMS,"cmd6.c: random_set_bit exiting with bit %d value %08lx\n",
                i, 1L<<index[i]);
   return (1L<<index[i]);  /* return that bit          */
}

/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
   s16b                 item, ident, lev;

   object_type         *i_ptr = NULL;
/* jk */
   s16b amt = 1;

   if (p_ptr->throat)
   {
      msg_print("Your throat is swelled shut.");
      return;
   }

   /* Restrict choices to food */
   item_tester_hook = item_tester_hook_food;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Eat which item? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to eat.");
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Take a turn */
   energy_use = 100;

   /* Identity not known yet */
   ident = FALSE;

   /* Object level */
   lev = k_info[i_ptr->k_idx].level;

   if (i_ptr->tval == TV_FOOD)
   {
      /* Analyze the food */
      switch (i_ptr->sval)
      {
         case SV_FOOD_POISON:
            if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
            {
               if (set_poisoned(p_ptr->poisoned + rand_int(10) + 10))
               {
                  ident = TRUE;
               }
            }
            break;

         case SV_FOOD_BLINDNESS:
            if (!p_ptr->resist_blind)
            {
               if (set_blind(p_ptr->blind + rand_int(200) + 200))
               {
                  ident = TRUE;
               }
            }
            break;

         case SV_FOOD_PARANOIA:
            if (!p_ptr->resist_fear)
            {
               if (set_afraid(p_ptr->afraid + rand_int(10) + 10))
               {
                  ident = TRUE;
               }
            }
            break;

         case SV_FOOD_CONFUSION:
            if (!p_ptr->resist_conf)
            {
               if (set_confused(p_ptr->confused + rand_int(10) + 10))
               {
                  ident = TRUE;
               }
            }
            break;

         case SV_FOOD_HALLUCINATION:
            if (!p_ptr->resist_chaos)
            {
               if (set_image(p_ptr->image + rand_int(250) + 250))
               {
                  ident = TRUE;
               }
            }
            break;

         case SV_FOOD_PARALYSIS:
            if (!p_ptr->free_act)
            {
               if (set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10))
               {
                  ident = TRUE;
               }
            }
            break;

         case SV_FOOD_WEAKNESS:
            take_hit(damroll(6, 6), "poisonous food.");
            (void)do_dec_stat(A_STR, STAT_DEC_NORMAL);
            ident = TRUE;
            break;

         case SV_FOOD_SICKNESS:
            take_hit(damroll(6, 6), "poisonous food.");
            (void)do_dec_stat(A_CON, STAT_DEC_NORMAL);
            ident = TRUE;
            break;

         case SV_FOOD_STUPIDITY:
            take_hit(damroll(8, 8), "poisonous food.");
            (void)do_dec_stat(A_INT, STAT_DEC_NORMAL);
            ident = TRUE;
            break;

         case SV_FOOD_NAIVETY:
            take_hit(damroll(8, 8), "poisonous food.");
            (void)do_dec_stat(A_WIS, STAT_DEC_NORMAL);
            ident = TRUE;
            break;

         case SV_FOOD_UNHEALTH:
            take_hit(damroll(10, 10), "poisonous food.");
            (void)do_dec_stat(A_CON, STAT_DEC_NORMAL);
            ident = TRUE;
            break;

         case SV_FOOD_DISEASE:
            take_hit(damroll(10, 10), "poisonous food.");
            (void)do_dec_stat(A_STR, STAT_DEC_NORMAL);
            ident = TRUE;
            break;

         case SV_FOOD_CURE_POISON:
            if (set_poisoned(0)) ident = TRUE;
            break;

         case SV_FOOD_CURE_BLINDNESS:
            if (set_blind(0)) ident = TRUE;
            break;

         case SV_FOOD_CURE_PARANOIA:
            if (set_afraid(0)) ident = TRUE;
            break;

         case SV_FOOD_CURE_CONFUSION:
            if (set_confused(0)) ident = TRUE;
            break;

         case SV_FOOD_CURE_SERIOUS:
            if (hp_player(damroll(4, 8))) ident = TRUE;
            break;

         case SV_FOOD_RESTORE_STR:
            if (do_res_stat(A_STR)) ident = TRUE;
            break;

         case SV_FOOD_RESTORE_CON:
            if (do_res_stat(A_CON)) ident = TRUE;
            break;

         case SV_FOOD_RESTORING:
            if (do_res_stat(A_STR)) ident = TRUE;
            if (do_res_stat(A_INT)) ident = TRUE;
            if (do_res_stat(A_WIS)) ident = TRUE;
            if (do_res_stat(A_DEX)) ident = TRUE;
            if (do_res_stat(A_CON)) ident = TRUE;
            if (do_res_stat(A_CHR)) ident = TRUE;
            break;

         case SV_FOOD_RATION:
         case SV_FOOD_BISCUIT:
         case SV_FOOD_JERKY:
         case SV_FOOD_SLIME_MOLD:
            msg_print("That tastes good.");
            ident = TRUE;
            break;

         case SV_FOOD_WAYBREAD:
            msg_print("That tastes good.");
            (void)set_poisoned(0);
            (void)hp_player(damroll(4, 8));
            ident = TRUE;
            break;

         case SV_FOOD_PINT_OF_ALE:
         case SV_FOOD_PINT_OF_WINE:
            msg_print("That tastes good.");
            ident = TRUE;
            break;

         default:
            msg_print("Oops.  Undefined food effect.");
            break;
      }
   } /* normal food */
   else
   { /* corpses */
      s16b r_idx = i_ptr->sval;
      u32b flag_ga   = r_info[r_idx].corpse_gives_alw;
      u32b flag_gs   = r_info[r_idx].corpse_gives_smt;
      u32b flag_ta   = r_info[r_idx].corpse_takes_alw;
      u32b flag_ts   = r_info[r_idx].corpse_takes_smt;
      byte chance_g  = r_info[r_idx].corpse_chance_gives;
      byte chance_t  = r_info[r_idx].corpse_chance_takes;

      /* spoilt ranges from 0 to 100, 0 means spoilt, 100 means fresh */
      u32b spoilt = (r_info[i_ptr->sval].corpse_spoiling - i_ptr->xtra2)
                       * 100 / r_info[i_ptr->sval].corpse_spoiling;
      /* fresh: 0 means fresh, 100 means utterly spoilt */
      u32b fresh = 100 - spoilt;

dlog(DEBUGITEMS,"cmd6.c: eat corpse ga %08lx gs %08lx ta %08lx ts %08lx\n",
      flag_ga, flag_gs, flag_ta, flag_ts);
dlog(DEBUGITEMS,"cmd6.c: chance g %d chance t %d spoilt %ld fresh %ld r_idx %d\n",
      chance_g, chance_t, spoilt, fresh, r_idx);

      /* benefits of this food */
      if (flag_ga) ident = handle_food_gives(i_ptr, fresh, flag_ga);
      if ((flag_gs) && (randint(100)<chance_g))
      {
dlog(DEBUGITEMS,"cmd6.c: randint(100)<chance_g\n");
         ident = handle_food_gives(i_ptr, fresh, random_set_bit(flag_gs));
      }
      /* and losses */
      if (flag_ta) ident = handle_food_takes(i_ptr, fresh, flag_ta);
      if ((flag_ts) && (randint(100)<chance_t))
      {
dlog(DEBUGITEMS,"cmd6.c: randint(100)<chance_t\n");
         ident = handle_food_takes(i_ptr, fresh, random_set_bit(flag_ts));
      }
   }
   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* We have tried it */
   object_tried(i_ptr);

   /* The player is now aware of the object */
   if (ident && !object_aware_p(i_ptr))
   {
       object_aware(i_ptr);
       gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Food can feed the player */
   (void)set_food(p_ptr->food + (u16b)i_ptr->p1val);

   /* Destroy a food in the pack */
   item_increase(item, -1, px, py);
   item_describe(item, px, py);
   item_optimize(item, px, py);
}

/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(void)
{
   s16b         item, ident, lev;

   object_type *i_ptr = NULL;

/* jk */
   s16b amt = 1;

   if (p_ptr->throat)
   {
      msg_print("Your throat is swelled shut.");
      return;
   }

   /* Restrict choices to potions */
   item_tester_tval = TV_POTION;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Quaff which potion? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have no potions to quaff.");
      item_tester_tval = 0;
      return;
   }
   item_tester_tval = 0;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Take a turn */
   energy_use = 100;

   /* Not identified yet */
   ident = FALSE;

   /* Object level */
   lev = k_info[i_ptr->k_idx].level;

   /* Analyze the potion */
   switch (i_ptr->sval)
   {
       case SV_POTION_WATER:
       case SV_POTION_APPLE_JUICE:
       case SV_POTION_SLIME_MOLD:
           msg_print("You feel less thirsty.");
           ident = TRUE;
           break;

       case SV_POTION_SLOWNESS:
           if (set_slow(p_ptr->slow + randint(25) + 15)) ident = TRUE;
           break;

       case SV_POTION_SALT_WATER:
           msg_print("The potion makes you vomit!");
           (void)set_food(PY_FOOD_STARVE - 1);
           (void)set_poisoned(0);
           (void)set_paralyzed(p_ptr->paralyzed + 4);
           ident = TRUE;
           break;

       case SV_POTION_POISON:
           if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
           {
               if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
               {
                   ident = TRUE;
               }
           }
           break;

       case SV_POTION_BLINDNESS:
           if (!p_ptr->resist_blind)
           {
               if (set_blind(p_ptr->blind + rand_int(100) + 100))
               {
                   ident = TRUE;
               }
           }
           break;

       case SV_POTION_FIRE:
           if (set_fire(p_ptr->fire + rand_int(100) + 100))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_COLD:
           if (set_cold(p_ptr->cold + rand_int(100) + 100))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_ACID:
           if (set_acid(p_ptr->acid + rand_int(100) + 100))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_ELEC:
           if (set_elec(p_ptr->elec + rand_int(100) + 100))
           {
               ident = TRUE;
           }
           break;


       case SV_POTION_CONFUSION:
           if (!p_ptr->resist_conf)
           {
               if (set_confused(p_ptr->confused + rand_int(20) + 15))
               {
                   ident = TRUE;
               }
           }
           break;

       case SV_POTION_SLEEP:
           if (!p_ptr->free_act)
           {
              ident=set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
           }
           break;

       case SV_POTION_LIFT:
           if (p_ptr->lift)
           {
              take_hit(damroll(3, 10), "a potion of Mule Legs");
           }
           else
             ident=set_lift(rand_int(50) + 50);
           break;

       case SV_POTION_LOSE_MEMORIES:
           if (!p_ptr->hold_life && (p_ptr->exp > 0))
           {
               msg_print("You feel your memories fade.");
               lose_exp(p_ptr->exp / 4);
               ident = TRUE;
           }
           break;

       case SV_POTION_RUINATION:
           msg_print("Your nerves and muscles feel weak and lifeless!");
           take_hit(damroll(10, 10), "a potion of Ruination");
           (void)dec_stat(A_DEX, 25, STAT_DEC_PERMANENT);
           (void)dec_stat(A_WIS, 25, STAT_DEC_PERMANENT);
           (void)dec_stat(A_CON, 25, STAT_DEC_PERMANENT);
           (void)dec_stat(A_STR, 25, STAT_DEC_PERMANENT);
           (void)dec_stat(A_CHR, 25, STAT_DEC_PERMANENT);
           (void)dec_stat(A_INT, 25, STAT_DEC_PERMANENT);
           ident = TRUE;
           break;

       case SV_POTION_DEC_STR:
           if (do_dec_stat(A_STR, STAT_DEC_NORMAL)) ident = TRUE;
           break;

       case SV_POTION_DEC_INT:
           if (do_dec_stat(A_INT, STAT_DEC_NORMAL)) ident = TRUE;
           break;

       case SV_POTION_DEC_WIS:
           if (do_dec_stat(A_WIS, STAT_DEC_NORMAL)) ident = TRUE;
           break;

       case SV_POTION_DEC_DEX:
           if (do_dec_stat(A_DEX, STAT_DEC_NORMAL)) ident = TRUE;
           break;

       case SV_POTION_DEC_CON:
           if (do_dec_stat(A_CON, STAT_DEC_NORMAL)) ident = TRUE;
           break;

       case SV_POTION_DEC_CHR:
           if (do_dec_stat(A_CHR, STAT_DEC_NORMAL)) ident = TRUE;
           break;

       case SV_POTION_DETONATIONS:
           msg_print("Massive explosions rupture your body!");
           take_hit(damroll(50, 20), "a potion of Detonation");
           (void)set_stun(p_ptr->stun + 75);
           (void)set_cut(p_ptr->cut + 5000);
           ident = TRUE;
           break;

       case SV_POTION_DEATH:
           msg_print("A feeling of Death flows through your body.");
           take_hit(5000, "a potion of Death");
           ident = TRUE;
           break;

       case SV_POTION_INFRAVISION:
           if (set_tim_infra(p_ptr->tim_infra + 100 + randint(100)))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_DETECT_INVIS:
           if (set_tim_invis(p_ptr->tim_invis + 12 + randint(12)))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_SLOW_POISON:
           if (set_poisoned(p_ptr->poisoned / 2)) ident = TRUE;
           break;

       case SV_POTION_CURE_POISON:
           if (set_poisoned(0)) ident = TRUE;
           break;

       case SV_POTION_BOLDNESS:
           if (set_afraid(0)) ident = TRUE;
           break;

       case SV_POTION_SPEED:
           if (!p_ptr->fast)
           {
               if (set_fast(randint(25) + 15)) ident = TRUE;
           }
           else
           {
               (void)set_fast(p_ptr->fast + 5);
           }
           break;

       case SV_POTION_RESIST_HEAT:
           if (set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_RESIST_COLD:
           if (set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10))
           {
               ident = TRUE;
           }
           break;

       case SV_POTION_HEROISM:
           if (hp_player(10)) ident = TRUE;
           if (set_afraid(0)) ident = TRUE;
           if (set_hero(p_ptr->hero + randint(25) + 25)) ident = TRUE;
           break;

       case SV_POTION_BESERK_STRENGTH:
           if (hp_player(30)) ident = TRUE;
           if (set_afraid(0)) ident = TRUE;
           if (set_shero(p_ptr->shero + randint(25) + 25)) ident = TRUE;
           break;

       case SV_POTION_CURE_LIGHT:
           if (hp_player(damroll(2, 8))) ident = TRUE;
           if (set_blind(0)) ident = TRUE;
           if (set_cut(p_ptr->cut - 10)) ident = TRUE;
           break;

       case SV_POTION_CURE_SERIOUS:
           if (hp_player(damroll(4, 8))) ident = TRUE;
           if (set_blind(0)) ident = TRUE;
           if (set_confused(0)) ident = TRUE;
           if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
           break;

       case SV_POTION_CURE_CRITICAL:
           if (hp_player(damroll(6, 8))) ident = TRUE;
           if (set_blind(0)) ident = TRUE;
           if (set_confused(0)) ident = TRUE;
           if (set_poisoned(0)) ident = TRUE;
           if (set_stun(0)) ident = TRUE;
           if (set_cut(0)) ident = TRUE;
           break;

       case SV_POTION_HEALING:
           if (hp_player(300)) ident = TRUE;
           if (set_blind(0)) ident = TRUE;
           if (set_confused(0)) ident = TRUE;
           if (set_poisoned(0)) ident = TRUE;
           if (set_stun(0)) ident = TRUE;
           if (set_cut(0)) ident = TRUE;
           break;

       case SV_POTION_STAR_HEALING:
           if (hp_player(1200)) ident = TRUE;
           if (set_blind(0)) ident = TRUE;
           if (set_confused(0)) ident = TRUE;
           if (set_poisoned(0)) ident = TRUE;
           if (set_stun(0)) ident = TRUE;
           if (set_cut(0)) ident = TRUE;
           break;

       case SV_POTION_LIFE:
           msg_print("You feel life flow through your body!");
           restore_level();
           hp_player(5000);
           (void)set_poisoned(0);
           (void)set_blind(0);
           (void)set_confused(0);
           (void)set_image(0);
           (void)set_stun(0);
           (void)set_cut(0);
           (void)do_res_stat(A_STR);
           (void)do_res_stat(A_CON);
           (void)do_res_stat(A_DEX);
           (void)do_res_stat(A_WIS);
           (void)do_res_stat(A_INT);
           (void)do_res_stat(A_CHR);
           ident = TRUE;
           break;

       case SV_POTION_RESTORE_MANA:
           if (p_ptr->csp < p_ptr->msp)
           {
               p_ptr->csp = p_ptr->msp;
               p_ptr->csp_frac = 0;
               msg_print("Your feel your head clear.");
               p_ptr->redraw1 |= (PR1_MANA);
               ident = TRUE;
           }
           break;

       case SV_POTION_RESTORE_EXP:
           if (restore_level()) ident = TRUE;
           break;

       case SV_POTION_RES_STR:
           if (do_res_stat(A_STR)) ident = TRUE;
           break;

       case SV_POTION_RES_INT:
           if (do_res_stat(A_INT)) ident = TRUE;
           break;

       case SV_POTION_RES_WIS:
           if (do_res_stat(A_WIS)) ident = TRUE;
           break;

       case SV_POTION_RES_DEX:
           if (do_res_stat(A_DEX)) ident = TRUE;
           break;

       case SV_POTION_RES_CON:
           if (do_res_stat(A_CON)) ident = TRUE;
           break;

       case SV_POTION_RES_CHR:
           if (do_res_stat(A_CHR)) ident = TRUE;
           break;

       case SV_POTION_INC_STR:
           if (do_inc_stat(A_STR, FALSE)) ident = TRUE;
           break;

       case SV_POTION_INC_INT:
           if (do_inc_stat(A_INT, FALSE)) ident = TRUE;
           break;

       case SV_POTION_INC_WIS:
           if (do_inc_stat(A_WIS, FALSE)) ident = TRUE;
           break;

       case SV_POTION_INC_DEX:
           if (do_inc_stat(A_DEX, FALSE)) ident = TRUE;
           break;

       case SV_POTION_INC_CON:
           if (do_inc_stat(A_CON, FALSE)) ident = TRUE;
           break;

       case SV_POTION_INC_CHR:
           if (do_inc_stat(A_CHR, FALSE)) ident = TRUE;
           break;

       case SV_POTION_AUGMENTATION:
           if (do_inc_stat(A_STR, FALSE)) ident = TRUE;
           if (do_inc_stat(A_INT, FALSE)) ident = TRUE;
           if (do_inc_stat(A_WIS, FALSE)) ident = TRUE;
           if (do_inc_stat(A_DEX, FALSE)) ident = TRUE;
           if (do_inc_stat(A_CON, FALSE)) ident = TRUE;
           if (do_inc_stat(A_CHR, FALSE)) ident = TRUE;
           break;

       case SV_POTION_ENLIGHTENMENT:
           msg_print("A glowing fog spreads through the dungeon...");
           wiz_lite();
           ident = TRUE;
           break;

       case SV_POTION_STAR_ENLIGHTENMENT:
           msg_print("A glowing for spreads through the dungeon...");
           msg_print(NULL);
           wiz_lite();
           msg_print("You feel a strange sensation in your head.");
           (void)do_inc_stat(A_INT, FALSE);
           (void)do_inc_stat(A_WIS, FALSE);
           (void)detect_treasure();
           (void)detect_object();
           (void)detect_sdoor();
           (void)detect_trap();
           identify_pack();
           self_knowledge();
           ident = TRUE;
           break;

       case SV_POTION_SELF_KNOWLEDGE:
           msg_print("You begin to know yourself a little better...");
           msg_print(NULL);
           self_knowledge();
           ident = TRUE;
           break;

       case SV_POTION_EXPERIENCE:
           restore_level();
           if (p_ptr->exp < PY_MAX_EXP)
           {
               s32b ee = (p_ptr->exp / 2) + 10;
               if (ee > 100000L) ee = 100000L;
               msg_print("You feel more experienced.");
               gain_exp(ee);
               ident = TRUE;
           }
           break;

       default:
           msg_print("Oops.  Undefined potion.");
           break;
   }

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* The item has been tried */
   object_tried(i_ptr);

   /* An identification was made */
   if (ident && !object_aware_p(i_ptr))
   {
       object_aware(i_ptr);
       gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Potions can feed the player */
   (void)set_food(p_ptr->food + i_ptr->p1val);

   /* Destroy a potion in the pack or on the floor */
   item_increase(item, -1, px, py);
   item_describe(item, px, py);
   item_optimize(item, px, py);
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
   object_type *j_ptr;

   char i_name[80];
   s16b  item = rand_int(INVEN_FEET-INVEN_BODY)+INVEN_BODY;

/* if no item there, there's a 40% chance to get another one */
   while (!inventory[item].k_idx && (rand_int(10)<4))
      item = rand_int(INVEN_FEET-INVEN_BODY)+INVEN_BODY;

   /* Curse the body armor */
   j_ptr = &inventory[item];

   /* Nothing to curse */
   if (!j_ptr->k_idx) return (FALSE);

   /* Describe */
   object_desc(i_name, j_ptr, FALSE, 3);

   /* Attempt a saving throw for artifacts */
   if (artifact_p(j_ptr) && (rand_int(100) < 50))
   {
      /* Cool */
      msg_format("A %s tries to %s, but your %s resists the effects!",
                 "terrible black aura", "surround your armor", i_name);
   }

   /* not artifact or failed save... */
   else
   {
      /* Oops */
      msg_format("A terrible black aura blasts your %s!", i_name);

      /* Blast the armor */
      j_ptr->name1 = 0;
      j_ptr->name2 = EGO_BLASTED;
      j_ptr->to_a = 0 - randint(5) - randint(5);
      j_ptr->to_h = 0;
      j_ptr->to_d = 0;
      j_ptr->ac = 0;
      j_ptr->dd = 0;
      j_ptr->ds = 0;
/* this seems appropriate as well */
      j_ptr->p1val = 0 - randint(8);

      /* Curse it */
      j_ptr->ident |= ID_CURSED;

      /* Break it */
      j_ptr->ident |= ID_BROKEN;

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Recalculate mana */
      p_ptr->update |= (PU_MANA);

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
   }

   return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
   object_type *j_ptr;

   char i_name[80];

   if (p_ptr->pclass == CLASS_HIGHPRST)
   {
      msg_print("You hear an angry buzzing, but nothing seems to happen.");
      return (TRUE);
   }

   if (rand_int(2)==1)
   {
      /* Curse the weapon */
      j_ptr = &inventory[INVEN_WIELD];

      if ((p_ptr->pclass == CLASS_GLADIATR) &&
          ( (inventory[INVEN_ARM].k_idx!=0) && (inventory[INVEN_ARM].tval != TV_SHIELD)))
      {
         if (randint(2)==1) j_ptr = &inventory[INVEN_ARM];
      }
      /* Nothing to curse */
      else
      {
         if (!j_ptr->k_idx) j_ptr = &inventory[INVEN_BOW];
      }
   }
   else
   {
      j_ptr = &inventory[INVEN_BOW];
      if (!j_ptr->k_idx) j_ptr = &inventory[INVEN_WIELD];
   }
   if (!j_ptr->k_idx) return (FALSE);

   /* Describe */
   object_desc(i_name, j_ptr, FALSE, 3);

   /* Attempt a saving throw */
   if (artifact_p(j_ptr) && (rand_int(100) < 50))
   {
      /* Cool */
      msg_format("A %s tries to %s, but your %s resists the effects!",
                 "terrible black aura", "surround your weapon", i_name);
   }

   /* not artifact or failed save... */
   else
   {
      /* Oops */
      msg_format("A terrible black aura blasts your %s!", i_name);

      /* Shatter the weapon */
      j_ptr->name1 = 0;
      j_ptr->name2 = EGO_SHATTERED;
      j_ptr->to_h = 0 - randint(5) - randint(5);
      j_ptr->to_d = 0 - randint(5) - randint(5);
      j_ptr->to_a = 0;
      j_ptr->ac = 0;
      j_ptr->dd = 0;
      j_ptr->ds = 0;

      /* Curse it */
      j_ptr->ident |= ID_CURSED;

      /* Break it */
      j_ptr->ident |= ID_BROKEN;

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Recalculate mana */
      p_ptr->update |= (PU_MANA);

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
   }

   /* Notice */
   return (TRUE);
}

static void read_scroll(object_type *i_ptr, s16b item)
{
   bool ident   = FALSE;
   bool used_up = FALSE;
   s16b lev,k;

   /* Object level */
   lev = k_info[i_ptr->k_idx].level;

   /* Assume the scroll will get used up */
   used_up = TRUE;

   /* Analyze the scroll */
   switch (i_ptr->sval)
   {
      case SV_SCROLL_DARKNESS:
         if (unlite_area(10, 3)) ident = TRUE;
         if (!p_ptr->resist_blind)
         {
            (void)set_blind(p_ptr->blind + 3 + randint(5));
         }
         break;

      case SV_SCROLL_AGGRAVATE_MONSTER:
         {  
            project_who_type who;
            msg_print("There is a high pitched humming noise.");
            who.type = WHO_PLAYER;
            aggravate_monsters(&who, -1);
            ident = TRUE;
            break;
         }

      case SV_SCROLL_CURSE_ARMOR:
         if (curse_armor()) ident = TRUE;
         break;

      case SV_SCROLL_TREASURE:
         {
            s16b x = px, y = py;
            place_gold_known(&x, &y);
            note_spot(x, y);
            lite_spot(x, y);
            if ((x==px) && (y==py))
            {
               msg_print("You feel some treasure appear between your feet.");
            }
            else
            {
               if (p_ptr->blind)
                  msg_print("You hear something trinkle on the floor.");
               else
                  msg_print("You see some treasure appear.");
            }

            ident=TRUE;
            break;
         }

      case SV_SCROLL_CURSE_WEAPON:
         if (curse_weapon()) ident = TRUE;
         break;

      case SV_SCROLL_SUMMON_MONSTER:
         for (k = 0; k < randint(3); k++)
         {
            if (summon_specific(px, py, p_ptr->mdepth, 0, 0))
            {
               ident = TRUE;
            }
         }
         break;

      case SV_SCROLL_SUMMON_UNDEAD:
         for (k = 0; k < randint(3); k++)
         {
            if (summon_specific(px, py, p_ptr->mdepth, SUMMON_UNDEAD, 0))
            {
               ident = TRUE;
            }
         }
         break;

      case SV_SCROLL_TRAP_CREATION:
      {
         project_who_type who;
         who.type = WHO_PLAYER;
         if (trap_creation(&who, px, py)) ident = TRUE;
         break;
      }
      case SV_SCROLL_PHASE_DOOR:
         teleport_player(10);
         ident = TRUE;
         break;

      case SV_SCROLL_TELEPORT:
         teleport_player(33*RATIO);
         ident = TRUE;
         break;

      case SV_SCROLL_TELEPORT_LEVEL:
         (void)teleport_player_level();
         ident = TRUE;
         break;

      case SV_SCROLL_WORD_OF_RECALL:
         if ( (p_ptr->word_recall == 0) &&
              !(dungeon.level[sublevel][py][px].fdat & CAVE_AREN))
         {
            p_ptr->word_recall = randint(20) + 15;
            msg_print("The air about you becomes charged...");
         }
         else if (dungeon.level[sublevel][py][px].fdat & CAVE_AREN)
         {
            msg_print("The crowd definitely cheers for the other guy now!");
         }
         else
         {
            p_ptr->word_recall = 0;
            msg_print("A tension leaves the air around you...");
         }
         ident = TRUE;
         break;

      case SV_SCROLL_IDENTIFY:
         if (!object_aware_p(i_ptr)) msg_print("This is an identify scroll.");
         ident = TRUE;
         if (!ident_spell()) used_up = FALSE;
         break;

      case SV_SCROLL_STAR_IDENTIFY:
         if (!object_aware_p(i_ptr)) msg_print("This is an *identify* scroll.");
         ident = TRUE;
         if (!identify_fully()) used_up = FALSE;
         break;

      case SV_SCROLL_REMOVE_CURSE:
         if (remove_curse())
         {
             msg_print("You feel as if someone is watching over you.");
             ident = TRUE;
         }
         break;

      case SV_SCROLL_STAR_REMOVE_CURSE:
         remove_all_curse();
         ident = TRUE;
         break;

      case SV_SCROLL_ENCHANT_ARMOR:
         if (!object_aware_p(i_ptr)) msg_print("This is a scroll of enchant armor.");
         ident = TRUE;
         if (!enchant_spell(0, 0, 1)) used_up = FALSE;
         break;

      case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
         if (!object_aware_p(i_ptr)) msg_print("This is a scroll of enchant weapon to-hit.");
         if (!enchant_spell(1, 0, 0)) used_up = FALSE;
         ident = TRUE;
         break;

      case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
         if (!object_aware_p(i_ptr)) msg_print("This is a scroll of enchant weapon to-dam.");
         if (!enchant_spell(0, 1, 0)) used_up = FALSE;
         ident = TRUE;
         break;

      case SV_SCROLL_STAR_ENCHANT_ARMOR:
         if (!object_aware_p(i_ptr)) msg_print("This is a scroll of *enchant* armor.");
         if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;
         ident = TRUE;
         break;

      case SV_SCROLL_STAR_ENCHANT_WEAPON:
         if (!object_aware_p(i_ptr)) msg_print("This is a scroll of *enchant* weapon.");
         if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;
         ident = TRUE;
         break;

      case SV_SCROLL_RECHARGING:
         if (!object_aware_p(i_ptr)) msg_print("This is a scroll of recharging.");
         if (!recharge(60)) used_up = FALSE;
         ident = TRUE;
         break;

      case SV_SCROLL_LIGHT:
         if (lite_area(damroll(2, 8), 2)) ident = TRUE;
         break;

      case SV_SCROLL_MAPPING:
         map_area();
         ident = TRUE;
         break;

      case SV_SCROLL_DETECT_GOLD:
         if (detect_treasure()) ident = TRUE;
         break;

      case SV_SCROLL_DETECT_ITEM:
         if (detect_object()) ident = TRUE;
         break;

      case SV_SCROLL_DETECT_TRAP:
         if (detect_trap()) ident = TRUE;
         break;

      case SV_SCROLL_DETECT_DOOR:
         if (detect_sdoor()) ident = TRUE;
         break;

      case SV_SCROLL_DETECT_INVIS:
         if (detect_invisible()) ident = TRUE;
         break;

      case SV_SCROLL_SATISFY_HUNGER:
         if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
         break;

      case SV_SCROLL_BLESSING:
         if (set_blessed(p_ptr->blessed + randint(12) + 6)) ident = TRUE;
         break;

      case SV_SCROLL_HOLY_CHANT:
         if (set_blessed(p_ptr->blessed + randint(24) + 12)) ident = TRUE;
         break;

      case SV_SCROLL_HOLY_PRAYER:
         if (set_blessed(p_ptr->blessed + randint(48) + 24)) ident = TRUE;
         break;

      case SV_SCROLL_MONSTER_CONFUSION:
         if (p_ptr->confusing == 0)
         {
             msg_print("Your hands begin to glow.");
             p_ptr->confusing = TRUE;
             ident = TRUE;
         }
         break;

      case SV_SCROLL_PROTECTION_FROM_EVIL:
         k = 3 * p_ptr->lev;
         if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
         break;

      case SV_SCROLL_RUNE_OF_PROTECTION:
         warding_glyph();
         ident = TRUE;
         break;

      case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
         if (destroy_doors_touch()) ident = TRUE;
         break;

      case SV_SCROLL_STAR_DESTRUCTION:
         destroy_area(px, py, 15, TRUE);
         ident = TRUE;
         break;

      case SV_SCROLL_DISPEL_UNDEAD:
         if (dispel_undead(60)) ident = TRUE;
         break;

      case SV_SCROLL_GENOCIDE:
         if (!object_aware_p(i_ptr)) msg_print("This is a genocide scroll.");
         (void)genocide();
         ident = TRUE;
         break;

      case SV_SCROLL_MASS_GENOCIDE:
         if (!object_aware_p(i_ptr)) msg_print("This is a mass genocide scroll.");
         (void)mass_genocide();
         ident = TRUE;
         break;

      case SV_SCROLL_ACQUIREMENT:
         acquirement(px, py, 1, TRUE, FALSE);
         ident = TRUE;
         break;

      case SV_SCROLL_STAR_ACQUIREMENT:
         acquirement(px, py, randint(2) + 1, TRUE, FALSE);
         ident = TRUE;
         break;

      default:
         msg_print("Oops.  Undefined scroll.");
         break;
   }


   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* The item was tried */
   object_tried(i_ptr);

   /* An identification was made */
   if (ident && !object_aware_p(i_ptr))
   {
      object_aware(i_ptr);
      gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Hack -- allow certain scrolls to be "preserved" */
   if (!used_up) return;

   /* Destroy a scroll in the pack */
   if (item < INVEN_TOTAL)
   {
      item_increase(item, -1, px, py);
      item_describe(item, px, py);
      item_optimize(item, px, py);
   }
   /* Destroy a scroll on the floor */
   else
   {
      item_increase(item, -1, i_ptr->ix, i_ptr->iy);
      /* it's no use describing 'you see nothing' if the scroll is gone */
      if (i_ptr->number > 0 ) item_describe(item, i_ptr->ix, i_ptr->iy);
      item_optimize(item, i_ptr->ix, i_ptr->iy);
   }
}

bool item_tester_hook_read(object_type *i_ptr)
{
   /* Read scrolls */
   if (i_ptr->tval == TV_SCROLL) return (TRUE);

   /* Read spells */
   if (i_ptr->tval == TV_SPELL) return (TRUE);

   /* Nope */
   return (FALSE);
}

/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
void do_cmd_read_scroll(void)
{
   s16b                 item;

   object_type         *i_ptr = NULL;

/* jk */
   s16b amt = 1;

   /* Check some conditions */
   if (p_ptr->blind)
   {
      msg_print("You can't see anything.");
      return;
   }
   if (no_lite())
   {
      msg_print("You have no light to read by.");
      return;
   }
   if (p_ptr->confused)
   {
      msg_print("You are too confused!");
      return;
   }

   /* Restrict choices to scrolls */
   item_tester_hook = item_tester_hook_read;

   /* Get an item (from inven) */
   if (!get_item(&item, &amt, "Read what? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to read.");
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Take a turn */
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE) || 
       (p_ptr->pclass == CLASS_PRIEST) || (p_ptr->pclass == CLASS_HIGHPRST))
   {
      energy_use = 75;
      if (p_ptr->lev>=45) energy_use = 25;
      else if (p_ptr->lev>=35) energy_use = 33;
      else if (p_ptr->lev>=15) energy_use = 50;
   }
   else if (p_ptr->pclass == CLASS_WARMAGE)
   {
      energy_use = 100;
      if (p_ptr->lev>=40) energy_use = 33;
      else if (p_ptr->lev>=25) energy_use = 50;
   }
   else
      energy_use = 100;

   /* Not identified yet */
   switch(i_ptr->tval)
   {
      case TV_SPELL:  read_spell(i_ptr, item);
                      break;
      case TV_SCROLL: read_scroll(i_ptr, item);
                      break;
   }
}

/*
 * check if a mage-type can expect an extra response
 */
static bool get_mage_extra(void)
{
   if ((p_ptr->pclass == CLASS_MAGE) ||
       (p_ptr->pclass == CLASS_WARMAGE))
   {
      return (randint(35-adj_int_dev[p_ptr->stat_ind[A_INT]]) < randint(20));
   }
   else if (p_ptr->pclass == CLASS_HIGHPRST)
   {
      return (randint(45-adj_int_dev[p_ptr->stat_ind[A_INT]]) < randint(20));
   }
   else
   {
      return FALSE;
   }
}

/*
 * check if a mage-type can expect a super response (where super > extra!)
 */
static bool get_mage_super(bool mage_extra)
{
   return (mage_extra && randint(10-(p_ptr->lev/10))==1);
}

/*
 * what effect has mage_super? (super > extra!) 
 */
static s16b do_mage_super(s16b value)
{
   s16b factor = 1, divisor = 1;

   switch(randint(10))
   {
      case 1:
      case 2:
      case 3:
      case 4:
      case 5:  factor = 5; divisor = 2; /* 2.5 */
               break;
      case 6:
      case 7:
      case 8:  factor = 3; divisor = 1; /* 3.0 */
               break;
      case 9:  factor = 4; divisor = 1; /* 4.0 */
               break;
      case 10: factor = 6; divisor = 1; /* 6.0 */
               break;
   }
   return ((factor*value) / divisor);
}

/*
 * what effect has mage_extra?
 */
static s16b do_mage_extra(s16b value)
{
   s16b factor = 1, divisor = 1;

   switch(randint(10))
   {
      case 1:
      case 2:
      case 3:
      case 4:
      case 5:  factor = 3; divisor = 2; /* 1.5 */
               break;
      case 6:
      case 7:
      case 8:  factor = 7; divisor = 4; /* 1.75 */
               break;
      case 9: 
      case 10: factor = 2; divisor = 1; /* 2.00 */
               break;
   }
   return ((factor*value) / divisor);
}

/*
 * Use a staff.                 -RAK-
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
static void activate_staff(object_type *i_ptr, s16b item)
{
   s16b                 ident, chance, k, lev;

/* jk */
   bool destroyed = FALSE;
   bool mage_extra = FALSE;
   bool mage_super = FALSE;

   /* Hack -- let staffs of identify get aborted */
   bool use_charge = TRUE;

   /* Take a turn */
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE))
   {
      energy_use = 75;
      if (p_ptr->lev>=35) energy_use = 33;
      else if (p_ptr->lev>=15) energy_use = 50;
   }
   else
   {
      energy_use = 100;
   }

   /* Not identified yet */
   ident = FALSE;

   /* Extract the item level */
   lev = k_info[i_ptr->k_idx].level;

   /* Base chance of success */
   chance = p_ptr->skill_dev;

   /* Confusion hurts skill */
   if (p_ptr->confused) chance = chance / 2;

   /* Hight level objects are harder */
   chance = chance - ((lev > 50) ? 50 : lev);

   /* Give everyone a (slight) chance */
   if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
   {
       chance = USE_DEVICE;
   }

   /* Roll for usage */
   if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
   {
       if (flush_failure) flush();
       if (rand_int(100)<(USE_DEVICE/2))
       {
          object_kind *k_ptr = &k_info[i_ptr->k_idx];
          char        name[80];
          object_desc(name,i_ptr,0,1);
          msg_print("The staff glows in a strange way!");
          fire_dam(randint(k_ptr->level/2)+(k_ptr->level/2),name);
          i_ptr->p1val = max(0, i_ptr->p1val-1);
       }
       else
          msg_print("You failed to use the staff properly.");

       return;
   }

   mage_extra = get_mage_extra();
   mage_super = get_mage_super(mage_extra);

   /* The staff is already empty! */
   if (i_ptr->p1val <= 0)
   {
       if (mage_extra)
       {
          switch(randint(2))
          {
             case 1:
                msg_print("The staff creaks, but obeys your mage-wishes.");
                i_ptr->p1val = 1;
                mage_extra = FALSE;
                break;
             case 2:
                msg_print("Alas! The uncharged staff explodes.");
                item_increase(item, -1, px, py);
                item_describe(item, px, py);
                item_optimize(item, px, py);
                return;
           }
       }
       else if (mage_super)
       {
          msg_print("The staff groans, but obeys your mage-wishes.");
          i_ptr->p1val = 1;
          mage_super = FALSE;
       }
       else
       {
          if (flush_failure) flush();
          if (i_ptr->number>1)
          {
             msg_print("None of the staffs has any charges left.");
          }
          else
          {
             msg_print("The staff has no charges left.");
          }
          i_ptr->ident |= ID_EMPTY;
          return;
       }
   }

   /* Analyze the staff */
   switch (i_ptr->sval)
   {
      case SV_STAFF_DARKNESS:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (unlite_area(do_mage_super(10), do_mage_super(3))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (unlite_area(do_mage_extra(10), do_mage_extra(3))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
         {
            if (unlite_area(10, 3)) ident = TRUE;
         }

         if (!p_ptr->resist_blind)
         {
            if (set_blind(p_ptr->blind + 3 + randint(5))) ident = TRUE;
         }
         break;

      case SV_STAFF_SLOWNESS:
         if (set_slow(p_ptr->slow + randint(30) + 15)) ident = TRUE;
         break;

      case SV_STAFF_HASTE_MONSTERS:
         if (speed_monsters()) ident = TRUE;
         break;

      case SV_STAFF_SUMMONING:
         for (k = 0; k < randint(4); k++)
         {
            if (summon_specific(px, py, p_ptr->mdepth, 0, 0))
            {
               ident = TRUE;
            }
         }
         break;

      case SV_STAFF_TELEPORTATION:
         teleport_player(33*RATIO);
         ident = TRUE;
         break;

      case SV_STAFF_IDENTIFY:
         if (!ident_spell()) use_charge = FALSE;
         ident = TRUE;
         break;

      case SV_STAFF_REMOVE_CURSE:
         if (mage_super)
         {
            if (remove_all_curse())
            {
               if (!p_ptr->blind)
               {
                  msg_print("The staff glows deep blue for a moment...");
               }
               ident = TRUE;
            }
            mage_super = FALSE;
         }
         else if (remove_curse())
         {
            if (!p_ptr->blind)
            {
               msg_print("The staff glows blue for a moment...");
            }
            ident = TRUE;
         }
         break;

      case SV_STAFF_STARLITE:
         if (!p_ptr->blind)
         {
            msg_print("The end of the staff glows brightly...");
         }
         for (k = 0; k < 8; k++) lite_line(ddd[k]);
         ident = TRUE;
         break;

      case SV_STAFF_LITE:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (lite_area(damroll(do_mage_super(2), do_mage_super(8)), do_mage_super(2))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (lite_area(damroll(do_mage_extra(2),do_mage_extra(8)), do_mage_extra(2))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
         {
            if (lite_area(damroll(2, 8), 2)) ident = TRUE;
         }
         break;

      case SV_STAFF_MAPPING:
         map_area();
         ident = TRUE;
         break;

      case SV_STAFF_DETECT_GOLD:
         if (detect_treasure()) ident = TRUE;
         break;

      case SV_STAFF_DETECT_ITEM:
         if (detect_object()) ident = TRUE;
         break;

      case SV_STAFF_DETECT_TRAP:
         if (detect_trap()) ident = TRUE;
         break;

      case SV_STAFF_DETECT_DOOR:
         if (detect_sdoor()) ident = TRUE;
         break;

      case SV_STAFF_DETECT_INVIS:
         if (detect_invisible()) ident = TRUE;
         break;

      case SV_STAFF_DETECT_EVIL:
         if (detect_evil()) ident = TRUE;
         break;

      case SV_STAFF_CURE_LIGHT:
         if (mage_super)
         {
            if (hp_player(do_mage_super(8)))
            {
               msg_print("You feel the staff come to life in your hands.");
               ident = TRUE;
               mage_super = FALSE;
            }
         }
         else if (mage_extra)
         {
            if (hp_player(do_mage_extra(8)))
            {
               msg_print("You feel the staff warm to your touch.");
               ident = TRUE;
               mage_extra = FALSE;
            }
         }
         else
         {
            if (hp_player(randint(8))) ident = TRUE;
         }
         break;

      case SV_STAFF_CURING:
         if (set_blind(0)) ident = TRUE;
         if (set_poisoned(0)) ident = TRUE;
         if (set_confused(0)) ident = TRUE;
         if (set_stun(0)) ident = TRUE;
         if (set_cut(0)) ident = TRUE;
         break;

      case SV_STAFF_HEALING:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (do_mage_super(300)) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (hp_player(do_mage_extra(300))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
            if (hp_player(300)) ident = TRUE;

         if (set_stun(0)) ident = TRUE;
         if (set_cut(0)) ident = TRUE;
         break;

      case SV_STAFF_THE_MAGI:
         if (do_res_stat(A_INT)) ident = TRUE;
         if (p_ptr->csp < p_ptr->msp)
         {
            p_ptr->csp = p_ptr->msp;
            p_ptr->csp_frac = 0;
            ident = TRUE;
            msg_print("Your feel your head clear.");
            p_ptr->redraw1 |= (PR1_MANA);
         }
         break;

      case SV_STAFF_SLEEP_MONSTERS:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (sleep_monsters_extra(do_mage_super(p_ptr->lev))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (sleep_monsters_extra(do_mage_extra(p_ptr->lev))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
         {
            if (sleep_monsters()) ident = TRUE;
         }
         break;

      case SV_STAFF_SLOW_MONSTERS:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (slow_monsters_extra(do_mage_super(p_ptr->lev))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (slow_monsters_extra(do_mage_extra(p_ptr->lev))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
            if (slow_monsters()) ident = TRUE;
         break;

      case SV_STAFF_SPEED:
         if (!p_ptr->fast)
         {
            if (mage_super)
            {
               msg_print("You feel the staff come to life in your hands.");
               if (set_fast(randint(do_mage_super(30))+do_mage_super(15))) ident=TRUE;
               mage_super = FALSE;
            }
            else if (mage_extra)
            {
               msg_print("You feel the staff warm to your touch.");
               if (set_fast(randint(do_mage_extra(30))+do_mage_extra(15))) ident=TRUE;
               mage_extra = FALSE;
            }
            else
            {
               if (set_fast(randint(30) + 15)) ident = TRUE;
            }
         }
         else
         {
            if (mage_super)
            {
               msg_print("You feel the staff come to life in your hands.");
               (void)set_fast(p_ptr->fast + do_mage_super(5));
               mage_super = FALSE;
            }
            if (mage_extra)
            {
               msg_print("You feel the staff warm to your touch.");
               (void)set_fast(p_ptr->fast + do_mage_extra(5));
               mage_extra = FALSE;
            }
            else
            {
               (void)set_fast(p_ptr->fast + 5);
            }
         }
         break;

      case SV_STAFF_PROBING:
         ident = probing();
         break;

      case SV_STAFF_DISPEL_EVIL:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (dispel_evil(do_mage_super(60))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (dispel_evil(do_mage_extra(60))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
         {
            if (dispel_evil(60)) ident = TRUE;
         }
         break;

      case SV_STAFF_POWER:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (dispel_monsters(do_mage_super(120))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (dispel_monsters(do_mage_extra(120))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
         {
            if (dispel_monsters(120)) ident = TRUE;
         }
         break;

      case SV_STAFF_HOLINESS:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            if (dispel_evil(do_mage_super(120))) ident = TRUE;
            k = do_mage_super(3) * p_ptr->lev;
            if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
            if (hp_player(do_mage_super(50))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            if (dispel_evil(do_mage_extra(120))) ident = TRUE;
            k = do_mage_extra(3) * p_ptr->lev;
            if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
            if (hp_player(do_mage_extra(50))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
         {
            if (dispel_evil(120)) ident = TRUE;
            k = 3 * p_ptr->lev;
            if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
            if (hp_player(50)) ident = TRUE;
         }
         if (set_poisoned(0)) ident = TRUE;
         if (set_afraid(0)) ident = TRUE;
         if (set_stun(0)) ident = TRUE;
         if (set_cut(0)) ident = TRUE;
         break;

      case SV_STAFF_GENOCIDE:
         (void)genocide();
         ident = TRUE;
         break;

      case SV_STAFF_EARTHQUAKES:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            earthquake(px, py, do_mage_super(10));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            earthquake(px, py, do_mage_extra(10));
            mage_extra = FALSE;
         }
         else
         {
            earthquake(px, py, 10);
         }
         ident = TRUE;
         break;

      case SV_STAFF_DESTRUCTION:
         if (mage_super)
         {
            msg_print("You feel the staff come to life in your hands.");
            destroy_area(px, py, do_mage_super(15), TRUE);
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the staff warm to your touch.");
            destroy_area(px, py, do_mage_extra(15), TRUE);
            mage_extra = FALSE;
         }
         else
         {
            destroy_area(px, py, 15, TRUE);
         }
         ident = TRUE;
         break;


      default:
         msg_print("Oops.  Undefined staff.");
         break;
   }

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Tried the item */
   object_tried(i_ptr);

   /* An identification was made */
   if (ident && !object_aware_p(i_ptr))
   {
      object_aware(i_ptr);
      gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Hack -- some uses are "free" */
   if (!use_charge) return;

   /* mage can use a staff without decharging it */
   if (mage_super || (mage_extra && (randint(2)==1)))
   {
      msg_print("The staff vibrates strongly as you release it.");
      mage_super = FALSE;
      mage_extra = FALSE;
   }
   else
   {
      /* Use a single charge */
      i_ptr->p1val--;
   }

   /* Describe charges in the pack or on the floor */
   if (!destroyed) item_charges(item);
}

/*
 * Aim a wand (from the pack or floor).
 *
 * Use a single charge from a single item.
 * Handle "unstacking" in a logical manner.
 *
 * For simplicity, you cannot use a stack of items from the
 * ground.  This would require too much nasty code.
 *
 * There are no wands which can "destroy" themselves, in the inventory
 * or on the ground, so we can ignore this possibility.  Note that this
 * required giving "wand of wonder" the ability to ignore destruction
 * by electric balls.
 *
 * All wands can be "cancelled" at the "Direction?" prompt for free.
 *
 * Note that the basic "bolt" wands do slightly less damage than the
 * basic "bolt" rods, but the basic "ball" wands do the same damage
 * as the basic "ball" rods.
 */

static void activate_wand(object_type *i_ptr, s16b item)
{
   s16b  lev, ident, chance, dir, sval;

/* jk */
   bool  destroyed = FALSE;
   bool  mage_extra = FALSE;
   bool  mage_super = FALSE;


   /* Allow direction to be cancelled for free */
   if (!get_aim_dir(&dir)) return;

   /* Take a turn */
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE))
   {
      energy_use = 75;
      if (p_ptr->lev>=35) energy_use = 33;
      else if (p_ptr->lev>=15) energy_use = 50;
   }
   else
      energy_use = 100;

   /* Not identified yet */
   ident = FALSE;

   /* Get the level */
   lev = k_info[i_ptr->k_idx].level;

   /* Base chance of success */
   chance = p_ptr->skill_dev;

   /* Confusion hurts skill */
   if (p_ptr->confused) chance = chance / 2;

   /* Hight level objects are harder */
   chance = chance - ((lev > 50) ? 50 : lev);

   /* Give everyone a (slight) chance */
   if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
   {
       chance = USE_DEVICE;
   }

   /* Roll for usage */
   if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
   {
       if (flush_failure) flush();
       if (rand_int(100)<(USE_DEVICE/2))
       {
          object_kind *k_ptr = &k_info[i_ptr->k_idx];
          char        name[80];
          object_desc(name,i_ptr,0,1);
          msg_print("The wand makes you shiver in a strange way!");
          cold_dam(randint(k_ptr->level/2)+(k_ptr->level/2),name);
          i_ptr->p1val = max(0, i_ptr->p1val-1);
       }
       else
       {
          msg_print("You failed to use the wand properly.");
       }
       return;
   }

   mage_extra = get_mage_extra();
   mage_super = get_mage_super(mage_extra);

   /* The wand is already empty! */
   if (i_ptr->p1val <= 0)
   {
       if (mage_extra && (randint(2)==1))
       {
          switch(randint(2))
          {
             case 1:
                msg_print("The wand creaks, but obeys your mage-wishes.");
                i_ptr->p1val = 1;
                mage_extra = FALSE;
                break;
             case 2:
                msg_print("Alas! The uncharged wand explodes.");
                item_increase(item, -1, px, py);
                item_describe(item, px, py);
                item_optimize(item, px, py);
                return;
                break;
          }
       }
       else if (mage_super)
       {
          msg_print("The wand groans, but obeys your mage-wishes.");
          i_ptr->p1val = 1;
          mage_super = FALSE;
       }
       else
       {
          if (flush_failure) flush();
          if (i_ptr->number>1)
          {
             msg_print("None of the wands has any charges left.");
          }
          else
          {
             msg_print("The wand has no charges left.");
          }
          i_ptr->ident |= ID_EMPTY;
          return;
       }
   }

   /* XXX Hack -- Extract the "sval" effect */
   sval = i_ptr->sval;

   /* XXX Hack -- Wand of wonder can do anything before it */
   if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

   /* Analyze the wand */
   switch (sval)
   {
      case SV_WAND_HEAL_MONSTER:
          if (heal_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_HASTE_MONSTER:
          if (speed_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_CLONE_MONSTER:
          if (clone_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_TELEPORT_AWAY:
          if (teleport_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_DISARMING:
          if (disarm_trap(dir)) ident = TRUE;
          break;

      case SV_WAND_TRAP_DOOR_DEST:
          if (destroy_door(dir)) ident = TRUE;
          break;

      case SV_WAND_STONE_TO_MUD:
          if (wall_to_mud(dir)) ident = TRUE;
          break;

      case SV_WAND_LITE:
          msg_print("A line of blue shimmering light appears.");
          lite_line(dir);
          ident = TRUE;
          break;

      case SV_WAND_SLEEP_MONSTER:
          if (sleep_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_SLOW_MONSTER:
          if (slow_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_CONFUSE_MONSTER:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             if (confuse_monster(dir, do_mage_super(10))) ident = TRUE;
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             if (confuse_monster(dir, do_mage_extra(10))) ident = TRUE;
             mage_extra = FALSE;
          }
          else if (confuse_monster(dir, 10)) ident = TRUE;
          break;

      case SV_WAND_FEAR_MONSTER:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             if (fear_monster(dir, do_mage_super(10))) ident = TRUE;
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             if (fear_monster(dir, do_mage_extra(10))) ident = TRUE;
             mage_extra = FALSE;
          }
          else if (fear_monster(dir, 10)) ident = TRUE;
          break;

      case SV_WAND_DRAIN_LIFE:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             if (drain_life(dir, do_mage_super(75))) ident = TRUE;
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             if (drain_life(dir, do_mage_extra(75))) ident = TRUE;
             mage_extra = FALSE;
          }
          else if (drain_life(dir, 75)) ident = TRUE;
          break;

      case SV_WAND_POLYMORPH:
          if (poly_monster(dir)) ident = TRUE;
          break;

      case SV_WAND_STINKING_CLOUD:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_POIS, dir, do_mage_super(12), do_mage_super(2));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_POIS, dir, do_mage_extra(12), do_mage_extra(2));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_POIS, dir, 12, 2);

          ident = TRUE;
          break;

      case SV_WAND_MAGIC_MISSILE:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(do_mage_super(2),do_mage_super(6)));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(do_mage_extra(2),do_mage_extra(6)));
             mage_extra = FALSE;
          }
          else
             fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2,6));
          ident = TRUE;
          break;

      case SV_WAND_ACID_BOLT:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_bolt_or_beam(20, GF_ACID, dir, damroll(do_mage_super(5),do_mage_super(8)));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_bolt_or_beam(20, GF_ACID, dir, damroll(do_mage_extra(5),do_mage_extra(8)));
             mage_extra = FALSE;
          }
          else
             fire_bolt_or_beam(20, GF_ACID, dir, damroll(5,8));
          ident = TRUE;
          break;

      case SV_WAND_ELEC_BOLT:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_bolt_or_beam(20, GF_ELEC, dir, damroll(do_mage_super(3),do_mage_super(8)));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_bolt_or_beam(20, GF_ELEC, dir, damroll(do_mage_extra(3), do_mage_extra(8)));
             mage_extra = FALSE;
          }
          else
             fire_bolt_or_beam(20, GF_ELEC, dir, damroll(3,8));
          ident = TRUE;
          break;

      case SV_WAND_FIRE_BOLT:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_bolt_or_beam(20, GF_FIRE, dir, damroll(do_mage_super(6), do_mage_super(8)));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_bolt_or_beam(20, GF_FIRE, dir, damroll(do_mage_extra(6), do_mage_extra(8)));
             mage_extra = FALSE;
          }
          else
             fire_bolt_or_beam(20, GF_FIRE, dir, damroll(6,8));
          ident = TRUE;
          break;

      case SV_WAND_COLD_BOLT:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_bolt_or_beam(20, GF_COLD, dir, damroll(do_mage_super(3),do_mage_super(8)));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_bolt_or_beam(20, GF_COLD, dir, damroll(do_mage_extra(3),do_mage_extra(8)));
             mage_extra = FALSE;
          }
          else
             fire_bolt_or_beam(20, GF_COLD, dir, damroll(3,8));
          ident = TRUE;
          break;

      case SV_WAND_ACID_BALL:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_ACID, dir, do_mage_super(60), do_mage_super(2));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_ACID, dir, do_mage_extra(60), do_mage_extra(2));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_ACID, dir, 60, 2);
          ident = TRUE;
          break;

      case SV_WAND_ELEC_BALL:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_ELEC, dir, do_mage_super(32), do_mage_super(2));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_ELEC, dir, do_mage_extra(32), do_mage_extra(2));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_ELEC, dir, 32, 2);
          ident = TRUE;
          break;

      case SV_WAND_FIRE_BALL:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_FIRE, dir, do_mage_super(72), do_mage_super(2));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_FIRE, dir, do_mage_extra(72), do_mage_extra(2));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_FIRE, dir, 72, 2);
          ident = TRUE;
          break;

      case SV_WAND_COLD_BALL:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_COLD, dir, do_mage_super(48), do_mage_super(2));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_COLD, dir, do_mage_extra(48), do_mage_extra(2));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_COLD, dir, 48, 2);
          ident = TRUE;
          break;

      case SV_WAND_WONDER:
          msg_print("Oops.  Wand of wonder activated.");
          break;

      case SV_WAND_DRAGON_FIRE:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_FIRE, dir, do_mage_super(100), do_mage_super(3));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_FIRE, dir, do_mage_extra(100), do_mage_extra(3));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_FIRE, dir, 100, 3);
          ident = TRUE;
          break;

      case SV_WAND_DRAGON_COLD:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             fire_ball(GF_COLD, dir, do_mage_super(80), do_mage_super(3));
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             fire_ball(GF_COLD, dir, do_mage_extra(80), do_mage_extra(3));
             mage_extra = FALSE;
          }
          else
             fire_ball(GF_COLD, dir, 80, 3);
          ident = TRUE;
          break;

      case SV_WAND_DRAGON_BREATH:
          switch (randint(5))
          {
             case 1:
                if (mage_super)
                {
                   msg_print("You feel the wand come to life in your hands.");
                   fire_ball(GF_COLD, dir, do_mage_super(100), do_mage_super(3));
                   mage_super = FALSE;
                }
                else if (mage_extra)
                {
                   msg_print("You feel the wand warm to your touch.");
                   fire_ball(GF_ACID, dir, do_mage_extra(100), do_mage_extra(3));
                   mage_extra = FALSE;
                }
                else
                   fire_ball(GF_ACID, dir, 100, 3);
                break;
             case 2:
                if (mage_super)
                {
                   msg_print("You feel the wand come to life in your hands.");
                   fire_ball(GF_ELEC, dir, do_mage_super(80), do_mage_super(3));
                   mage_super = FALSE;
                }
                else if (mage_extra)
                {
                   msg_print("You feel the wand warm to your touch.");
                   fire_ball(GF_ELEC, dir, do_mage_extra(80), do_mage_extra(3));
                   mage_extra = FALSE;
                }
                else
                   fire_ball(GF_ELEC, dir, 80, 3);
                break;
             case 3:
                if (mage_super)
                {
                   msg_print("You feel the wand come to life in your hands.");
                   fire_ball(GF_FIRE, dir, do_mage_super(100), do_mage_super(3));
                   mage_super = FALSE;
                }
                else if (mage_extra)
                {
                   msg_print("You feel the wand warm to your touch.");
                   fire_ball(GF_FIRE, dir, do_mage_extra(100), do_mage_extra(3));
                   mage_extra = FALSE;
                }
                else
                   fire_ball(GF_FIRE, dir, 100, 3);
                break;
             case 4:
                if (mage_super)
                {
                   msg_print("You feel the wand come to life in your hands.");
                   fire_ball(GF_COLD, dir, do_mage_super(80), do_mage_super(3));
                   mage_super = FALSE;
                }
                else if (mage_extra)
                {
                   msg_print("You feel the wand warm to your touch.");
                   fire_ball(GF_COLD, dir, do_mage_extra(80), do_mage_extra(3));
                   mage_extra = FALSE;
                }
                else
                   fire_ball(GF_COLD, dir, 80, 3);
                break;
             default:
                if (mage_super)
                {
                   msg_print("You feel the wand come to life in your hands.");
                   fire_ball(GF_POIS, dir, do_mage_super(60), do_mage_super(3));
                   mage_super = FALSE;
                }
                else if (mage_extra)
                {
                   msg_print("You feel the wand warm to your touch.");
                   fire_ball(GF_POIS, dir, do_mage_extra(60), do_mage_extra(3));
                   mage_extra = FALSE;
                }
                else
                 fire_ball(GF_POIS, dir, 60, 3);
                 break;
          }
          ident = TRUE;
          break;

      case SV_WAND_ANNIHILATION:
          if (mage_super)
          {
             msg_print("You feel the wand come to life in your hands.");
             if (drain_life(dir, do_mage_super(125))) ident = TRUE;
             mage_super = FALSE;
          }
          else if (mage_extra)
          {
             msg_print("You feel the wand warm to your touch.");
             if (drain_life(dir, do_mage_extra(125))) ident = TRUE;
             mage_extra = FALSE;
          }
          else
             if (drain_life(dir, 125)) ident = TRUE;
          break;

      default:
          msg_print("Oops.  Undefined wand.");
          break;
   }

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Mark it as tried */
   object_tried(i_ptr);

   /* Apply identification */
   if (ident && !object_aware_p(i_ptr))
   {
       object_aware(i_ptr);
       gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* mage can use a wand without decharging it */
   if (mage_super || (mage_extra && (randint(2)==1)))
   {
      msg_print("The wand vibrates strongly as you release it.");
      mage_super = FALSE;
      mage_extra = FALSE;
   }
   else
   {
      /* Use a single charge */
      i_ptr->p1val--;
   }

   /* Describe charges in the pack or on the floor */
   if (!destroyed) item_charges(item);
}

/* jk */
s16b get_rod_charge(object_type *i_ptr)
{
   s16b result = 0;

   switch (i_ptr->sval)
   {
      case SV_ROD_DETECT_TRAP:   result = 50;  break;
      case SV_ROD_DETECT_DOOR:   result = 70;  break;
      case SV_ROD_IDENTIFY:      result = 10;  break;
      case SV_ROD_RECALL:        result = 60;  break;
      case SV_ROD_ILLUMINATION:  result = 30;  break;
      case SV_ROD_MAPPING:       result = 99;  break;
      case SV_ROD_DETECTION:     result = 99;  break;
      case SV_ROD_PROBING:       result = 50;  break;
      case SV_ROD_CURING:        result = 999; break;
      case SV_ROD_HEALING:       result = 999; break;
      case SV_ROD_RESTORATION:   result = 999; break;
      case SV_ROD_SPEED:         result = 99;  break;
      case SV_ROD_TELEPORT_AWAY: result = 25;  break;
      case SV_ROD_DISARMING:     result = 30;  break;
      case SV_ROD_LITE:          result = 9;   break;
      case SV_ROD_SLEEP_MONSTER: result = 18;  break;
      case SV_ROD_SLOW_MONSTER:  result = 20;  break;
      case SV_ROD_DRAIN_LIFE:    result = 23;  break;
      case SV_ROD_POLYMORPH:     result = 25;  break;
      case SV_ROD_ACID_BOLT:     result = 12;  break;
      case SV_ROD_ELEC_BOLT:     result = 11;  break;
      case SV_ROD_FIRE_BOLT:     result = 15;  break;
      case SV_ROD_COLD_BOLT:     result = 13;  break;
      case SV_ROD_ACID_BALL:     result = 27;  break;
      case SV_ROD_ELEC_BALL:     result = 23;  break;
      case SV_ROD_FIRE_BALL:     result = 30;  break;
      case SV_ROD_COLD_BALL:     result = 25;  break;
      case SV_ROD_IDENT_TRAP:    result = 25;  break;

      /* fall-through */
      case SV_ROD_CLEARTHINKING:
      case SV_ROD_DREAMS:
      case SV_ROD_ESCAPE:
      case SV_ROD_PLENTY:
      case SV_ROD_KNOWLEDGE:     result = 150;  break;

      default:
          msg_print("Oops.  Undefined rod.");
          break;
   }
   return (result);
}

/*
 * this function activates a rod
 */
static void activate_rod(object_type *i_ptr, s16b item)
{
   s16b             ident, chance, dir, lev;
   bool             mage_extra = FALSE;
   bool             mage_super = FALSE;

   /* Hack -- let perception get aborted */
   bool             use_charge = TRUE;
   project_who_type who;

   /* Get a direction for unknown rods, and for (not-artifact) rods that need it */
   if ( ( (i_ptr->sval >= SV_ROD_MIN_DIRECTION) && (i_ptr->name1 == 0) ) ||
        !object_aware_p(i_ptr) )
   {
      /* Get a direction, allow cancel */
      if (!get_aim_dir(&dir)) return;
   }

   /* Take a turn */
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE))
   {
      energy_use = 75;
      if (p_ptr->lev>=35) energy_use = 33;
      else if (p_ptr->lev>=15) energy_use = 50;
   }
   else
   {
      energy_use = 100;
   }

   /* Not identified yet */
   ident = FALSE;

   /* Extract the item level */
   lev = k_info[i_ptr->k_idx].level;

   /* Base chance of success */
   chance = p_ptr->skill_dev;

   /* Confusion hurts skill */
   if (p_ptr->confused) chance = chance / 2;

   /* Hight level objects are harder */
   chance = chance - ((lev > 50) ? 50 : lev);

   /* Give everyone a (slight) chance */
   if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
   {
      chance = USE_DEVICE;
   }

   /* Roll for usage */
   if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
   {
      if (flush_failure) flush();
      if (rand_int(100)<(USE_DEVICE/2))
      {
         object_kind *k_ptr = &k_info[i_ptr->k_idx];
         char     name[80];
         object_desc(name,i_ptr,0,1);
         msg_print("The rod shocks you!");
         elec_dam(randint(k_ptr->level/2)+(k_ptr->level/2),name);

         /* add a small extra timeout, but don't go over the maximum */
         i_ptr->p1val += get_rod_charge(i_ptr)/(randint(3)+1);
         i_ptr->p1val = min(i_ptr->p1val, i_ptr->number * get_rod_charge(i_ptr));
      }
      else
      {
         msg_print("You failed to use the rod properly.");
      }

      return;
   }
   /* if we are a mage, test for some extra action */

   mage_extra = get_mage_extra();
   mage_super = get_mage_super(mage_extra);

   /* are all of the rods still charging? */
   if (i_ptr->p1val > (get_rod_charge(i_ptr) * (i_ptr->number-1) ) )
   {
      if (mage_extra && (randint(4)==1))
      {
         switch(randint(4))
         {
            case 1:
               msg_print("The rod creaks, but obeys your mage-wishes.");
               i_ptr->p1val = max(0, i_ptr->p1val - get_rod_charge(i_ptr));
               mage_extra = FALSE;
               break;
            case 2:
               msg_print("Alas! The uncharged rod explodes.");
               item_increase(item,-1, px, py);
               item_describe(item, px, py);
               item_optimize(item, px, py);
               return;
               break;
         }
      }
      else if (mage_super)
      {
         msg_print("The rod groans,  but obeys your mage-wishes.");
         i_ptr->p1val = max(0, i_ptr->p1val - get_rod_charge(i_ptr));
         mage_super = FALSE;
      }
      else
      {
         if (flush_failure) flush();
         if (i_ptr->number>1)
         {
            msg_print("The rods are all still charging.");
         }
         else
         {
            msg_print("The rod is still charging.");
         }
         return;
      }
   }

   /* Analyze the rod */
   switch (i_ptr->sval)
   {
      case SV_ROD_DETECT_TRAP:
         if (detect_trap()) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_DETECT_DOOR:
         if (detect_sdoor()) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_IDENTIFY:
         ident = TRUE;
         if (!ident_spell()) use_charge = FALSE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_RECALL:
         if (p_ptr->word_recall == 0)
         {
            msg_print("The air about you becomes charged...");
            p_ptr->word_recall = 15 + randint(20);
         }
         else
         {
            msg_print("A tension leaves the air around you...");
            p_ptr->word_recall = 0;
         }
         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_ILLUMINATION:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            if (lite_area(damroll(do_mage_super(2),do_mage_super(8)), do_mage_super(2))) ident=TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            if (lite_area(damroll(do_mage_extra(2),do_mage_extra(8)), do_mage_extra(2))) ident=TRUE;
            mage_extra = FALSE;
         }
         else
            if (lite_area(damroll(2, 8), 2)) ident = TRUE;

         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_MAPPING:
         map_area();
         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_DETECTION:
         detection();
         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_PROBING:
         probing();
         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_CURING:
         if (set_blind(0)) ident = TRUE;
         if (set_poisoned(0)) ident = TRUE;
         if (set_confused(0)) ident = TRUE;
         if (set_stun(0)) ident = TRUE;
         if (set_cut(0)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_HEALING:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            if (hp_player(do_mage_super(500))) ident=TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            if (hp_player(do_mage_extra(500))) ident=TRUE;
            mage_extra = FALSE;
         }
         else
            if (hp_player(500)) ident = TRUE;

         if (set_stun(0)) ident = TRUE;
         if (set_cut(0)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_RESTORATION:
         if (restore_level()) ident = TRUE;
         if (do_res_stat(A_STR)) ident = TRUE;
         if (do_res_stat(A_INT)) ident = TRUE;
         if (do_res_stat(A_WIS)) ident = TRUE;
         if (do_res_stat(A_DEX)) ident = TRUE;
         if (do_res_stat(A_CON)) ident = TRUE;
         if (do_res_stat(A_CHR)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_SPEED:
         if (!p_ptr->fast)
         {
            if (mage_super)
            {
               msg_print("You feel the rod come to life in your hands.");
               if (set_fast(randint(do_mage_super(30))+do_mage_super(15))) ident=TRUE;
               mage_super = FALSE;
            }
            else if (mage_extra)
            {
               msg_print("You feel the rod warm to your touch.");
               if (set_fast(randint(do_mage_extra(30))+do_mage_extra(15))) ident=TRUE;
               mage_extra = FALSE;
            }
            else
               if (set_fast(randint(30) + 15)) ident = TRUE;
         }
         else
         {
            if (mage_super)
            {
               msg_print("You feel the rod come to life in your hands.");
               (void)set_fast(p_ptr->fast + do_mage_super(5));
               mage_super = FALSE;
            }
            else if (mage_extra)
            {
               msg_print("You feel the rod warm to your touch.");
               (void)set_fast(p_ptr->fast + do_mage_extra(5));
               mage_extra = FALSE;
            }
            else
               (void)set_fast(p_ptr->fast + 5);
         }
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_TELEPORT_AWAY:
         if (teleport_monster(dir)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_DISARMING:
         if (disarm_trap(dir)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_LITE:
         msg_print("A line of blue shimmering light appears.");
         lite_line(dir);
         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_SLEEP_MONSTER:
         if (sleep_monster(dir)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_SLOW_MONSTER:
         if (slow_monster(dir)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_DRAIN_LIFE:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            if (drain_life(dir, do_mage_super(75))) ident = TRUE;
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            if (drain_life(dir, do_mage_extra(75))) ident = TRUE;
            mage_extra = FALSE;
         }
         else
            if (drain_life(dir, 75)) ident = TRUE;

         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_POLYMORPH:
         if (poly_monster(dir)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_ACID_BOLT:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_bolt_or_beam(20, GF_ACID, dir, damroll(do_mage_super(6),do_mage_super(8)));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            fire_bolt_or_beam(20, GF_ACID, dir, damroll(do_mage_extra(6),do_mage_extra(8)));
            mage_extra = FALSE;
         }
         else
            fire_bolt_or_beam(10, GF_ACID, dir, damroll(6,8));

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_ELEC_BOLT:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_bolt_or_beam(20, GF_ELEC, dir, damroll(do_mage_super(3),do_mage_super(8)));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            fire_bolt_or_beam(20, GF_ELEC, dir, damroll(do_mage_extra(3),do_mage_extra(8)));
            mage_extra = FALSE;
         }
         else
            fire_bolt_or_beam(10, GF_ELEC, dir, damroll(3,8));

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_FIRE_BOLT:
         who.type = WHO_PLAYER;
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_bolt_or_beam(20, GF_FIRE, dir, damroll(do_mage_super(8),do_mage_super(8)));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod glow to your touch.");
            fire_bolt_or_beam(10, GF_FIRE, dir, damroll(do_mage_extra(8),do_mage_extra(8)));
            mage_extra = FALSE;
         }
         else
         {
            fire_bolt_or_beam(10, GF_FIRE, dir, damroll(8,8));
         }

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_COLD_BOLT:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_bolt_or_beam(20, GF_COLD, dir, damroll(do_mage_super(5),do_mage_super(8)));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod glow to your touch.");
            fire_bolt_or_beam(10, GF_COLD, dir, damroll(do_mage_extra(5),do_mage_extra(8)));
            mage_extra = FALSE;
         }
         else
            fire_bolt_or_beam(10, GF_COLD, dir, damroll(5,8));

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_ACID_BALL:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_ball(GF_ACID, dir, do_mage_super(60), do_mage_super(2));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            fire_ball(GF_ACID, dir, do_mage_extra(60), do_mage_extra(2));
            mage_extra = FALSE;
         }
         else
            fire_ball(GF_ACID, dir, 60, 2);

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_ELEC_BALL:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_ball(GF_ELEC, dir, do_mage_super(32), do_mage_super(2));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            fire_ball(GF_ELEC, dir, do_mage_extra(32), do_mage_extra(2));
            mage_extra = FALSE;
         }
         else
            fire_ball(GF_ELEC, dir, 32, 2);

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_FIRE_BALL:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_ball(GF_FIRE, dir, do_mage_super(72), do_mage_super(2));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            fire_ball(GF_FIRE, dir, do_mage_extra(72), do_mage_extra(2));
            mage_extra = FALSE;
         }
         else
            fire_ball(GF_FIRE, dir, 72, 2);

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_COLD_BALL:
         if (mage_super)
         {
            msg_print("You feel the rod come to life in your hands.");
            fire_ball(GF_COLD, dir, do_mage_super(48), do_mage_super(2));
            mage_super = FALSE;
         }
         else if (mage_extra)
         {
            msg_print("You feel the rod warm to your touch.");
            fire_ball(GF_COLD, dir, do_mage_super(48), do_mage_super(2));
            mage_extra = FALSE;
         }
         else
            fire_ball(GF_COLD, dir, 48, 2);

         ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_IDENT_TRAP:
         if (ident_trap(dir)) ident = TRUE;
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_CLEARTHINKING:
         ident = set_confused(0);
         who.type = WHO_PLAYER;
         ident |= project(&who, 2+randint(2), px, py, 0, GF_CONF_STRONG, PROJECT_KILL_MONSTER);
         if (p_ptr->csp < p_ptr->msp)
         {
            p_ptr->csp += (p_ptr->msp-p_ptr->csp) / 2;
            p_ptr->csp_frac = 0;
            p_ptr->redraw1 = (PR1_MANA);
            ident = TRUE;
         }
         if (ident)
         {
            msg_print("You feel your head clear.");
         }
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_DREAMS:
         ident = detection();
         who.type = WHO_PLAYER;
         ident |= project(&who, 2+randint(2), px, py, 0, GF_SLEEP_STRONG, PROJECT_KILL_MONSTER);
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_ESCAPE:
         ident = fire_ball(GF_DARK, dir, 150, 2+randint(2));
         ident |= unlite_area(50, 10);
         teleport_player(10);
         if (!p_ptr->fast)
         {
            if (set_fast(randint(25) + 15)) ident = TRUE;
         }
         else
         {
            (void)set_fast(p_ptr->fast + 5);
         }
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      case SV_ROD_PLENTY:
         {
            s16b food_kind[5] = {SV_FOOD_BISCUIT, SV_FOOD_JERKY, SV_FOOD_RATION,
                                 SV_FOOD_SLIME_MOLD, SV_FOOD_WAYBREAD};
            s16b k_idx;
            object_type forge;

            if (hp_player(200)) ident = TRUE;
            if (set_poisoned(0)) ident = TRUE;
            if (set_stun(0)) ident = TRUE;
            /* Get object base type */
            k_idx = lookup_kind(TV_FOOD, food_kind[rand_int(5)]);

            /* strange things happen otherwise, you have been warned! */
            forge.spell_set = 0;
            invwipe(&forge);

            /* Create the item */
            invcopy(&forge, k_idx);

            forge.log.where = OBJ_FOUND_PLENTY;
            forge.log.whose = 0;
            forge.log.mlevel = p_ptr->mdepth;
            forge.log.slevel = p_ptr->sdepth;

            /* Drop the object from heaven */
            if (!drop_near(&forge, 0, px, py, 0, FALSE, FALSE))
            {
               msg_print("You feel a need for a light snack.");
            }
            else
            {
               ident = TRUE;
            }
            i_ptr->p1val += get_rod_charge(i_ptr);
            break;
         }

      case SV_ROD_KNOWLEDGE:
         ident = fire_ball(GF_LITE, dir, 150, 2+randint(2));
         ident |= lite_area(50, 10);
         ident |= dispel_evil(300);
         (void)ident_spell();
         i_ptr->p1val += get_rod_charge(i_ptr);
         break;

      default:
         msg_print("Oops.  Undefined rod.");
         break;
   }

   if (use_charge)
   {
      if (mage_super && (i_ptr->p1val>0))
      {
         msg_print("The rod vibrates strongly as you release it.");
         i_ptr->p1val = max(0, i_ptr->p1val - 3*(get_rod_charge(i_ptr) / 4));
         mage_super = FALSE;
      }
      else if (mage_extra && (i_ptr->p1val>0))
      {
         msg_print("The rod vibrates as you release it.");
         i_ptr->p1val = max(0, i_ptr->p1val - (get_rod_charge(i_ptr) / 2));
         mage_extra = FALSE;
      }
   }

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Tried the object */
   object_tried(i_ptr);

   /* Successfully determined the object function */
   if (ident && !object_aware_p(i_ptr))
   {
      object_aware(i_ptr);
      gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);
}

/*
 * Activate (zap) a Rod
 *
 * Unstack fully charged rods as needed.
 *
 * Hack -- rods of perception/genocide can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 */
/* jk - we need some keys for other thing, so now 'z' works with */
/* staffs, wands and rods */
void do_cmd_zap(void)
{
   s16b        item;
   object_type *i_ptr;

/* jk */
   s16b amt = 1;

   item_tester_hook = item_tester_hook_recharge;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Zap what? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to zap.");
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr = get_item_pointer(item);

dlog(DEBUGITEMS,"cmd6.c: do_cmd_zap: gotten item %d\n", item);

   /* Verify potential overflow */
   if ((inven_cnt >= INVEN_PACK) &&
       (i_ptr->number > 1) && (item<INVEN_TOTAL))
   {
      /* Verify with the player */
      if (other_query_flag &&
          !get_check("Your pack might overflow.  Continue? ")) return;
   }

   if (i_ptr->tval == TV_STAFF) activate_staff(i_ptr, item);
   if (i_ptr->tval == TV_WAND) activate_wand(i_ptr, item);
   if (i_ptr->tval == TV_ROD) activate_rod(i_ptr, item);

   item_tester_hook = NULL;
}

/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *i_ptr)
{
   u64b f1, f2, f3;

   /* Not known */
   if (!object_known_p(i_ptr)) return (FALSE);

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

dlog(DEBUGITEMS,"cmd6.c: item_tester_hook_activate: name %s activate %d\n",
                k_name + k_info[i_ptr->k_idx].name, (f3 & TR3_ACTIVATE)?1:0);
   /* Check activation flag */
   if (f3 & TR3_ACTIVATE) return (TRUE);

   /* Assume not */
   return (FALSE);
}

/*
 * Hack -- activate the ring of power
 */
static void ring_of_power(s16b dir)
{
   /* Pick a random effect */
   switch (randint(10))
   {
      case 1:
      case 2:

         /* Message */
         msg_print("You are surrounded by a malignant aura.");

         /* Decrease all stats (permanently) */
         (void)dec_stat(A_STR, 50, STAT_DEC_PERMANENT);
         (void)dec_stat(A_INT, 50, STAT_DEC_PERMANENT);
         (void)dec_stat(A_WIS, 50, STAT_DEC_PERMANENT);
         (void)dec_stat(A_DEX, 50, STAT_DEC_PERMANENT);
         (void)dec_stat(A_CON, 50, STAT_DEC_PERMANENT);
         (void)dec_stat(A_CHR, 50, STAT_DEC_PERMANENT);

         /* Lose some experience (permanently) */
         p_ptr->exp -= (p_ptr->exp / 4);
         p_ptr->max_exp -= (p_ptr->exp / 4);
         check_experience();

         break;

      case 3:

         /* Message */
         msg_print("You are surrounded by a powerful aura.");

         /* Dispel monsters */
         dispel_monsters(1000);

         break;

      case 4:
      case 5:
      case 6:

         /* Mana Ball */
         fire_ball(GF_MANA, dir, 300, 3);

         break;

      default:

         /* Mana Bolt */
         fire_bolt(GF_MANA, dir, 250);

         break;
   }
}

void add_spells_mage_staff(object_type *i_ptr)
{
   bool found[MAX_SPELLS_PER_ITEM];
   s16b index[MAX_SPELLS_PER_ITEM];
   char buf;
   s16b i, j, cnt=0, cntlow=0, spells=0, spellno, unbound_spells = 0;
   bool add_spells = FALSE;

   for (i=0; i<s_number; i++)
   {
      if (has_spell(i_ptr, i)) index[spells++]=i;
   }
dlog(DEBUGITEMS,"cmd6.c: add_spells_mage_staff: %d spells\n", spells);
   if ((spells>0) && (i_ptr->timeout))
   {
      msg_print("Your staff vibrates, but nothing happens.");
      return;
   }

   if ((i_ptr->name2 == EGO_MAGE) && (spells==1))
   {
dlog(DEBUGITEMS,"cmd6.c: add_spells_mage_staff: EGO_MAGE & 1 spell - executing spell %d\n", index[0]);
      msg_print("Your staff comes to life!");
      exec_spell(index[0]);
      i_ptr->timeout = 100 + randint(200);
      return;
   }

   if (spells)
   {
      if ( ((i_ptr->name2 == EGO_ADEPT) && (spells<2)) ||
           ((i_ptr->name2 == EGO_ARCHMAGE) && (spells<3)) )
      {
         prt("", 0, MESSAGE_ROW);
         buf='x';
         while ((buf != 'A') && (buf != 'U'))
         {
            add_spells=get_com("Do you want to try to Add a spell to the staff or Use it?(A/U):", &buf);
            if ((buf=='a')) buf='A';
            if ((buf=='u')) buf='U';
            add_spells=(buf=='A');
         }
      }
   }
   else /* no spells yet */
   {
      add_spells = TRUE;
   }

   if (!add_spells)
   {
      /* Save the screen */
      Term_save();

      print_spells(index, spells);

      /* Clear the top line */
      prt("", 0, MESSAGE_ROW);

      /* Prompt user */
      if (!get_com("Which spell? ", &buf))
      {
         Term_load();
         return;
      }

      spellno=A2I(buf);
      if ((spellno<0) || (spellno >= spells))
      {
         Term_load();
         return;
      }
      Term_load();
      msg_print("Your staff comes to life!");
      exec_spell(index[spellno]);
      i_ptr->timeout = 100 + randint(200);
   }
   else /* add spells */
   {
      for (j=0; j<s_number; j++) found[j]=FALSE;

      for (i=0; i<INVEN_PACK; i++)
      {
         if (inventory[i].tval == TV_SPELL)
         {
            unbound_spells++;
         }
         if (inventory[i].tval == TV_BOOK)
         {
dlog(DEBUGITEMS,"cmd6.c: add_spells_mage_staff: inven %d book found\n", i);
            for (j=0; j<s_number; j++)
            {
               if (has_spell(&inventory[i], j))
               {
dlog(DEBUGITEMS,"cmd6.c: add_spells_mage_staff: inven %d spell %d found, found[%d]=%d\n",
                i, j, j, found[j]);
                  if (!found[j])
                  {
                     index[cnt++]=j;
dlog(DEBUGITEMS,"cmd6.c: add_spells_mage_staff: new spell, scale %d\n", s_info[j].scale);
                     if (s_info[j].scale <= 2) cntlow++;
                  }
                  found[j]=TRUE;
               }
            }
         }
      }
dlog(DEBUGITEMS,"cmd6.c: add_spells_mage_staff: cnt %d cntlow %d\n",
                cnt, cntlow);
      if (!cnt)
      {
         if (!unbound_spells)
         {
            msg_print("You have no spells for the staff to adsorb!");
         }
         else
         {
            msg_print("You can only absorb spells bound in a book in your staff!");
         }
         return;
      }
      if ( ((i_ptr->name2 == EGO_MAGE) && (cnt<4)) ||
           ((i_ptr->name2 == EGO_ADEPT) && ((cnt<8) || (cntlow<3))) ||
           ((i_ptr->name2 == EGO_ARCHMAGE) && ((cnt<12) || (cntlow<5))) )
      {
         msg_print("The staff flickers for a moment, but nothing happens.");
         return;
      }
      j=rand_int(cnt);
      set_spell(i_ptr, index[j]);
      msg_format("The staff hums as it absorbs the spell of %s.",
                 s_info[index[j]].name + s_name);
   }
}

/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
void do_cmd_activate(void)
{
   s16b                  item, lev, dir, a, i, k, chance;
   project_who_type      who;
   object_type          *i_ptr = NULL;

/* jk */
   s16b amt = 1;

   /* Prepare the hook */
   item_tester_hook = item_tester_hook_activate;

   /* Get an item (from equip) */
   if (!get_item(&item, &amt, "Activate which item? ", TRUE, FALSE, FALSE))
   {
      if (item == -2) msg_print("You have nothing to activate.");
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   if ((bit_class(p_ptr->pclass) & MAGE_MAGIC_CLASS) && (i_ptr->tval==TV_HAFTED) && (i_ptr->sval==SV_QUARTERSTAFF))
   {
      if ((i_ptr->name2==EGO_MAGE) ||
         (i_ptr->name2==EGO_ADEPT) ||
         (i_ptr->name2==EGO_ARCHMAGE))
      {
         add_spells_mage_staff(i_ptr);
         return;
      }
   }

   /* Take a turn */
   energy_use = 100;

   /* Extract the item level */
   lev = k_info[i_ptr->k_idx].level;

   /* Hack -- use artifact level instead */
   if (artifact_p(i_ptr)) lev = a_info[i_ptr->name1].level;

   /* Base chance of success */
   chance = p_ptr->skill_dev;

   /* Confusion hurts skill */
   if (p_ptr->confused) chance = chance / 2;

   /* Hight level objects are harder */
   chance = chance - ((lev > 50) ? 50 : lev);

   /* Give everyone a (slight) chance */
   if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
   {
      chance = USE_DEVICE;
   }

   /* Roll for usage */
   if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
   {
      if (flush_failure) flush();
      msg_print("You failed to activate it properly.");
      return;
   }

   /* Check the recharge */
   if (i_ptr->timeout)
   {
      msg_print("It whines, glows and fades...");
      return;
   }

   /* Wonder Twin Powers... Activate! */
   msg_print("You activate it...");

   /* Artifacts activate by name */
   if (i_ptr->name1)
   {
      /* This needs to be changed */
      switch (i_ptr->name1)
      {
         case ART_NARTHANC:
            msg_print("Your dagger is covered in fire...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_FIRE, dir, damroll(9, 8));
            i_ptr->timeout = rand_int(8) + 8;
            break;

         case ART_NIMTHANC:
            msg_print("Your dagger is covered in frost...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_COLD, dir, damroll(6, 8));
            i_ptr->timeout = rand_int(7) + 7;
            break;

         case ART_DETHANC:
            msg_print("Your dagger is covered in sparks...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_ELEC, dir, damroll(4, 8));
            i_ptr->timeout = rand_int(6) + 6;
            break;

         case ART_RILIA:
            msg_print("Your dagger throbs deep green...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_POIS, dir, 12, 3);
            i_ptr->timeout = rand_int(4) + 4;
            break;

         case ART_BELANGIL:
            msg_print("Your dagger is covered in frost...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_COLD, dir, 48, 2);
            i_ptr->timeout = rand_int(5) + 5;
            break;

         case ART_DAL:
            msg_print("You feel energy flow through your feet...");
            (void)set_afraid(0);
            (void)set_poisoned(0);
            i_ptr->timeout = 5;
            break;

         case ART_RINGIL:
            msg_print("Your sword glows an intense blue...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_COLD, dir, 100, 2);
            i_ptr->timeout = 300;
            break;

         case ART_ANDURIL:
            msg_print("Your sword glows an intense red...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_FIRE, dir, 72, 2);
            i_ptr->timeout = 400;
            break;

         case ART_FIRESTAR:
            msg_print("Your morningstar rages in fire...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_FIRE, dir, 72, 3);
            i_ptr->timeout = 100;
            break;

         case ART_FEANOR:
            if (!p_ptr->fast)
            {
               (void)set_fast(randint(20) + 20);
            }
            else
            {
               (void)set_fast(p_ptr->fast + 5);
            }
            i_ptr->timeout = 200;
            break;

         case ART_THEODEN:
            msg_print("The blade of your axe glows black...");
            if (!get_aim_dir(&dir)) return;
            drain_life(dir, 120);
            i_ptr->timeout = 400;
            break;

         case ART_SUN:
            msg_print("The shield shines bright...");
            (void)lite_area(damroll(20, 10), 8);
            set_afraid(0);
            set_shero(p_ptr->shero + randint(25) + 25);
            i_ptr->timeout = 400;
            break;

         case ART_TURMIL:
            msg_print("The head of your hammer glows white...");
            if (!get_aim_dir(&dir)) return;
            drain_life(dir, 90);
            i_ptr->timeout = 70;
            break;

         case ART_CASPANION:
            msg_print("Your armor glows bright red...");
            destroy_doors_touch();
            i_ptr->timeout = 10;
            break;

         case ART_AVAVIR:
            if (p_ptr->word_recall == 0)
            {
               p_ptr->word_recall = randint(20) + 15;
               msg_print("The air about you becomes charged...");
            }
            else
            {
               p_ptr->word_recall = 0;
               msg_print("A tension leaves the air around you...");
            }
            i_ptr->timeout = 200;
            break;

/* jk - the axe of Hurin */
         case ART_HURIN:
            if (!p_ptr->fast)
            {
               (void)set_fast(randint(50) + 50);
            }
            else
            {
               (void)set_fast(p_ptr->fast + 5);
            }
            hp_player(30);
            set_afraid(0);
            set_shero(p_ptr->shero + randint(50) + 50);
            i_ptr->timeout = rand_int(200) + 100;
            break;

         case ART_GOTHMOG:
            msg_print("Your lochaber axe erupts in fire...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_FIRE, dir, 300, 4);
            i_ptr->timeout = 200+rand_int(200);
            break;


         case ART_TARATOL:
            if (!p_ptr->fast)
            {
               (void)set_fast(randint(20) + 20);
            }
            else
            {
               (void)set_fast(p_ptr->fast + 5);
            }
            i_ptr->timeout = rand_int(100) + 100;
            break;

         case ART_ERIRIL:
            /* Identify and combine pack */
            (void)ident_spell();
            /* XXX Note that the artifact is always de-charged */
            i_ptr->timeout = 10;
            break;

         case ART_OLORIN:
            probing();
            i_ptr->timeout = 20;
            break;

         case ART_EONWE:
            msg_print("Your axe lets out a long, shrill note...");
            (void)mass_genocide();
            i_ptr->timeout = 1000;
            break;

         case ART_LOTHARANG:
            msg_print("Your battle axe radiates deep purple...");
            hp_player(damroll(4, 8));
            (void)set_cut((p_ptr->cut / 2) - 50);
            i_ptr->timeout = rand_int(3) + 3;
            break;

         case ART_CUBRAGOL:

            /* Use the first (XXX) acceptable bolts */
            for (a = 0; a < INVEN_PACK; a++)
            {
               object_type *j_ptr = &inventory[a];
               if ((j_ptr->tval == TV_BOLT) &&
                  (!artifact_p(j_ptr)) && (!ego_item_p(j_ptr)) &&
                  (!cursed_p(j_ptr) && !broken_p(j_ptr))) break;
            }

            /* Enchant the bolts (or fail) */
            if ((a < INVEN_PACK) && (rand_int(100) < 25))
            {
               object_type *j_ptr = &inventory[a];
               msg_print("Your bolts are covered in a fiery aura!");
               j_ptr->name2 = EGO_FLAME;
               enchant(j_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
            }
            else
            {
               if (flush_failure) flush();
               msg_print("The fiery enchantment failed.");
            }

            i_ptr->timeout = 999;
            break;

         case ART_ARUNRUTH:
            msg_print("Your sword glows a pale blue...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_COLD, dir, damroll(12, 8));
            i_ptr->timeout = 500;
            break;

         case ART_AEGLOS:
            msg_print("Your spear glows a bright white...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_COLD, dir, 100, 2);
            i_ptr->timeout = 500;
            break;

         case ART_OROME:
            msg_print("Your spear pulsates...");
            if (!get_aim_dir(&dir)) return;
            wall_to_mud(dir);
            i_ptr->timeout = 5;
            break;

         case ART_SOULKEEPER:
            msg_print("Your armor glows a bright white...");
            msg_print("You feel much better...");
            (void)hp_player(1000);
            (void)set_cut(0);
            i_ptr->timeout = 888;
            break;

         case ART_BELEGENNON:
            teleport_player(10);
            i_ptr->timeout = 2;
            break;

         case ART_CELEBORN:
            (void)genocide();
            i_ptr->timeout = 500;
            break;

         case ART_LUTHIEN:
            restore_level();
            i_ptr->timeout = 450;
            break;

         case ART_ULMO:
            msg_print("Your trident glows deep red...");
            if (!get_aim_dir(&dir)) return;
            teleport_monster(dir);
            i_ptr->timeout = 150;
            break;

         case ART_COLLUIN:
            msg_print("Your cloak glows many colours...");
            (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
            (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
            (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
            (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
            (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
            i_ptr->timeout = 111;
            break;

         case ART_HOLCOLLETH:
            msg_print("Your cloak glows deep blue...");
            sleep_monsters_touch();
            i_ptr->timeout = 55;
            break;

         case ART_THINGOL:
            msg_print("You hear a low humming noise...");
            recharge(60);
            i_ptr->timeout = 70;
            break;

         case ART_COLANNON:
            teleport_player(33*RATIO);
            i_ptr->timeout = 45;
            break;

         case ART_TOTILA:
            msg_print("Your flail glows in scintillating colours...");
            if (!get_aim_dir(&dir)) return;
            confuse_monster(dir, 20);
            i_ptr->timeout = 15;
            break;

         case ART_CAMMITHRIM:
            msg_print("Your gloves glow extremely brightly...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_MISSILE, dir, damroll(2, 6));
            i_ptr->timeout = 2;
            break;

         case ART_PAURHACH:
            msg_print("Your gauntlets are covered in fire...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_FIRE, dir, damroll(9,8));
            i_ptr->timeout = rand_int(8) + 8;
            break;

         case ART_PAURNIMMEN:
            msg_print("Your gauntlets are covered in frost...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_COLD, dir, damroll(6, 8));
            i_ptr->timeout = rand_int(7) + 7;
            break;

         case ART_PAURAEGEN:
            msg_print("Your gauntlets are covered in sparks...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_ELEC, dir, damroll(4, 8));
            i_ptr->timeout = rand_int(6) + 6;
            break;

         case ART_PAURNEN:
            msg_print("Your gauntlets look very acidic...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_ACID, dir, damroll(5, 8));
            i_ptr->timeout = rand_int(5) + 5;
            break;

         case ART_FINGOLFIN:
            msg_print("Magical spikes appear on your cesti...");
            if (!get_aim_dir(&dir)) return;
            fire_bolt(GF_ARROW, dir, 150);
            i_ptr->timeout = rand_int(90) + 90;
            break;

         case ART_HOLHENNETH:
            msg_print("An image forms in your mind...");
            detection();
            i_ptr->timeout = rand_int(55) + 55;
            break;

         case ART_GONDOR:
            msg_print("You feel a warm tingling inside...");
            (void)hp_player(500);
            (void)set_cut(0);
            i_ptr->timeout = 500;
            break;

         case ART_RAZORBACK:
            msg_print("You are surrounded by lightning!");
            for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
            i_ptr->timeout = 1000;
            break;

         case ART_BLADETURNER:
            msg_print("Your armor glows many colours...");
            (void)hp_player(30);
            (void)set_afraid(0);
            (void)set_shero(p_ptr->shero + randint(50) + 50);
            (void)set_blessed(p_ptr->blessed + randint(50) + 50);
            (void)set_oppose_acid(p_ptr->oppose_acid + randint(50) + 50);
            (void)set_oppose_elec(p_ptr->oppose_elec + randint(50) + 50);
            (void)set_oppose_fire(p_ptr->oppose_fire + randint(50) + 50);
            (void)set_oppose_cold(p_ptr->oppose_cold + randint(50) + 50);
            (void)set_oppose_pois(p_ptr->oppose_pois + randint(50) + 50);
            i_ptr->timeout = 400;
            break;

         case ART_GALADRIEL:
            msg_print("The phial wells with clear light...");
            lite_area(damroll(2, 15), 3);
            i_ptr->timeout = rand_int(10) + 10;
            break;

         case ART_ELENDIL:
            msg_print("The star shines brightly...");
            map_area();
            i_ptr->timeout = rand_int(50) + 50;
            break;

         case ART_THRAIN:
            msg_print("The stone glows a deep green...");
            wiz_lite();
            (void)detect_sdoor();
            (void)detect_trap();
            i_ptr->timeout = rand_int(100) + 100;
            break;

         case ART_INGWE:
            msg_print("An aura of good floods the area...");
            dispel_evil(p_ptr->lev * 5);
            i_ptr->timeout = rand_int(300) + 300;
            break;

         case ART_CARLAMMAS:
            msg_print("The amulet lets out a shrill wail...");
            k = 3 * p_ptr->lev;
            (void)set_protevil(p_ptr->protevil + randint(25) + k);
            i_ptr->timeout = rand_int(225) + 225;
            break;

         case ART_TULKAS:
            msg_print("The ring glows brightly...");
            if (!p_ptr->fast)
            {
               (void)set_fast(randint(75) + 75);
            }
            else
            {
               (void)set_fast(p_ptr->fast + 5);
            }
            i_ptr->timeout = rand_int(150) + 150;
            break;

         case ART_NARYA:
            msg_print("The ring glows deep red...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_FIRE, dir, 120, 3);
            i_ptr->timeout = rand_int(225) + 225;
            break;

         case ART_NENYA:
            msg_print("The ring glows bright white...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_COLD, dir, 200, 3);
            i_ptr->timeout = rand_int(325) + 325;
            break;

         case ART_VILYA:
            msg_print("The ring glows deep blue...");
            if (!get_aim_dir(&dir)) return;
            fire_ball(GF_ELEC, dir, 250, 3);
            i_ptr->timeout = rand_int(425) + 425;
            break;

         case ART_POWER:
            msg_print("The ring glows intensely black...");
            if (!get_aim_dir(&dir)) return;
            ring_of_power(dir);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case ART_SARUMAN:
             msg_print("The staffs emits a humming sound that tears at your soul...");
             who.type=WHO_PLAYER;
             project(&who, 2+randint(2), px, py, 0, GF_CONF_STRONG, PROJECT_KILL_MONSTER);
             i_ptr->timeout = rand_int(100) + 100;
             break;

         default:
            msg_print("Oops.  Non-Activatable Artifact.");
      }

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP);

      /* Done */
      return;
   }

   /* Hack -- Dragon Scale Mail can be activated as well */
   if (i_ptr->tval == TV_DRAG_ARMOR)
   {
      /* Get a direction for breathing (or abort) */
      if (!get_aim_dir(&dir)) return;

      /* Branch on the sub-type */
      switch (i_ptr->sval)
      {
         case SV_DRAGON_BLUE:
            msg_print("You breathe lightning.");
            fire_ball(GF_ELEC, dir, 100, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_WHITE:
            msg_print("You breathe frost.");
            fire_ball(GF_COLD, dir, 110, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_BLACK:
            msg_print("You breathe acid.");
            fire_ball(GF_ACID, dir, 130, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_GREEN:
            msg_print("You breathe poison gas.");
            fire_ball(GF_POIS, dir, 150, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_RED:
            msg_print("You breathe fire.");
            fire_ball(GF_FIRE, dir, 200, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_MULTIHUED:
            chance = rand_int(5);
            msg_format("You breathe %s.",
                     ((chance == 1) ? "lightning" :
                     ((chance == 2) ? "frost" :
                      ((chance == 3) ? "acid" :
                       ((chance == 4) ? "poison gas" : "fire")))));
            fire_ball(((chance == 1) ? GF_ELEC :
                     ((chance == 2) ? GF_COLD :
                     ((chance == 3) ? GF_ACID :
                      ((chance == 4) ? GF_POIS : GF_FIRE)))),
                    dir, 250, 2);
            i_ptr->timeout = rand_int(225) + 225;
            break;

         case SV_DRAGON_BRONZE:
            msg_print("You breathe confusion.");
            fire_ball(GF_CONFUSION, dir, 120, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_GOLD:
            msg_print("You breathe sound.");
            fire_ball(GF_SOUND, dir, 130, 2);
            i_ptr->timeout = rand_int(450) + 450;
            break;

         case SV_DRAGON_CHAOS:
            chance = rand_int(2);
            msg_format("You breathe %s.",
                     ((chance == 1 ? "chaos" : "disenchantment")));
            fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
                    dir, 220, 2);
            i_ptr->timeout = rand_int(300) + 300;
            break;

         case SV_DRAGON_LAW:
            chance = rand_int(2);
            msg_format("You breathe %s.",
                     ((chance == 1 ? "sound" : "shards")));
            fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
                    dir, 230, 2);
            i_ptr->timeout = rand_int(300) + 300;
            break;

         case SV_DRAGON_BALANCE:
            chance = rand_int(4);
            msg_format("You breathe %s.",
                     ((chance == 1) ? "chaos" :
                     ((chance == 2) ? "disenchantment" :
                      ((chance == 3) ? "sound" : "shards"))));
            fire_ball(((chance == 1) ? GF_CHAOS :
                     ((chance == 2) ? GF_DISENCHANT :
                     ((chance == 3) ? GF_SOUND : GF_SHARDS))),
                    dir, 250, 2);
            i_ptr->timeout = rand_int(300) + 300;
            break;

         case SV_DRAGON_SHINING:
            chance = rand_int(2);
            msg_format("You breathe %s.",
                     ((chance == 0 ? "light" : "darkness")));
            fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, 2);
            i_ptr->timeout = rand_int(300) + 300;
            break;

         case SV_DRAGON_POWER:
            msg_print("You breathe the elements.");
            fire_ball(GF_MISSILE, dir, 300, 2);
            i_ptr->timeout = rand_int(300) + 300;
            break;

         default:
            msg_print("Oops.  You have bad breath.");
      }

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP);

      /* Success */
      return;
   }

   /* jk - some other items can be activated as well */

   if ((i_ptr->tval == TV_BOOTS) && (i_ptr->name2 == EGO_JUMP) )
   {
      teleport_player(10);
      i_ptr->timeout = rand_int(10) + 10;
      return;
   }

   if ((i_ptr->tval == TV_HELM) && (i_ptr->name2 == EGO_NOLDOR) )
   {
      if (detect_treasure())
      {
         msg_print("You detect treasure.");
      }
      i_ptr->timeout = rand_int(20) + 10;
      return;
   }

   /* Mistake */
   msg_print("Oops.  That object cannot be activated.");
}


