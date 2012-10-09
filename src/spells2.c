/* File: spells2.c */

/* Purpose: Spell code (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Increase players hit points, notice effects
 */
bool hp_player(s16b num)
{
   if (p_ptr->chp < p_ptr->mhp)
   {
      p_ptr->chp += num;

      if (p_ptr->chp > p_ptr->mhp)
      {
         p_ptr->chp = p_ptr->mhp;
         p_ptr->chp_frac = 0;
      }

      p_ptr->redraw1 |= (PR1_HP);

      num = num / 5;
      if (num < 3)
      {
         if (num == 0)
         {
            msg_print("You feel a little better.");
         }
         else
         {
            msg_print("You feel better.");
         }
      }
      else
      {
         if (num < 7)
         {
            msg_print("You feel much better.");
         }
         else
         {
            msg_print("You feel very good.");
         }
      }

      return (TRUE);
   }

   return (FALSE);
}

/*
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
   /* Require clean space */
   if (!clean_grid_bold(py, px)) return;

   if (dungeon.level[sublevel][py][px].fdat & CAVE_AREN)
   {
      msg_print("The floors vibrates in a strange way. You hear howling laughter.");
      return;
   }

   /* Create a glyph of warding */
   (void)set_grid_type(px, py, DUNG_FLOOR, DUNG_FLOOR_GLYPH, GRID_KEEP, 0);
}

/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] =
{
   "strong",
   "smart",
   "wise",
   "dextrous",
   "healthy",
   "cute"
};

/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
   "weak",
   "stupid",
   "naive",
   "clumsy",
   "sickly",
   "ugly"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(s16b stat, s16b mode)
{
   bool sust = FALSE;

   /* Access the "sustain" */
   switch (stat)
   {
      case A_STR: if (p_ptr->sustain_str) sust = TRUE; break;
      case A_INT: if (p_ptr->sustain_int) sust = TRUE; break;
      case A_WIS: if (p_ptr->sustain_wis) sust = TRUE; break;
      case A_DEX: if (p_ptr->sustain_dex) sust = TRUE; break;
      case A_CON: if (p_ptr->sustain_con) sust = TRUE; break;
      case A_CHR: if (p_ptr->sustain_chr) sust = TRUE; break;
   }

   /* Sustain */
   if (sust)
   {
      /* Message */
      msg_format("You feel %s for a moment, but the feeling passes.",
                 desc_stat_neg[stat]);

      /* Notice effect */
      return (TRUE);
   }

   /* Attempt to reduce the stat */
   if (dec_stat(stat, 10, mode))
   {
      /* Message */
      if (mode==STAT_DEC_TEMPORARY)
      {
         msg_format("You feel strangely %s.", desc_stat_neg[stat]);
      }
      else if (mode == STAT_DEC_PERMANENT)
      {
         msg_format("You feel very %s.", desc_stat_neg[stat]);
      }
      else
      {
         msg_format("You feel %s.", desc_stat_neg[stat]);
      }

      /* Notice effect */
      p_ptr->redraw1 |= (PR1_STATS);
      return (TRUE);
   }

   /* Nothing obvious */
   return (FALSE);
}

/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(s16b stat)
{
   /* Attempt to increase */
   if (res_stat(stat))
   {
      /* Message */
      msg_format("You feel less %s.", desc_stat_neg[stat]);

      /* Notice */
      p_ptr->redraw1 |= (PR1_STATS);
      return (TRUE);
   }

   /* Nothing obvious */
   return (FALSE);
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(s16b stat, bool only_temporary)
{
   bool res = FALSE;

   /* Restore strength */
   if (!only_temporary) res = res_stat(stat);

   /* Attempt to increase */
   if (inc_stat(stat, only_temporary))
   {
       /* Message */
       if (only_temporary)
       {
          if (p_ptr->stat_cur[stat]<p_ptr->stat_max[stat])
          {
             msg_format("You feel less %s.", desc_stat_neg[stat]);
          }
          else
          {
             msg_format("You feel %s again!", desc_stat_pos[stat]);
          }
       }
       else
       {
          msg_format("Wow!  You feel very %s!", desc_stat_pos[stat]);
       }

       /* Notice */
       p_ptr->redraw1 |= (PR1_STATS);
       return (TRUE);
   }

   /* Restoration worked */
   if (res)
   {
       /* Message */
       msg_format("You feel less %s.", desc_stat_neg[stat]);

       /* Notice */
       p_ptr->redraw1 |= (PR1_STATS);
       return (TRUE);
   }

   /* Nothing obvious */
   return (FALSE);
}

/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
   s16b                 i;
   object_type        *i_ptr;

   /* Simply identify and know every item */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      i_ptr = &inventory[i];
      if (i_ptr->k_idx)
      {
         object_tried(i_ptr);
         object_aware(i_ptr);
         object_known(i_ptr);
      }
   }
}

/*
 * Used by the "enchant" function (chance of failure)
 */
static s16b enchant_table[16] =
{
      0,  10,  50, 100, 200,
    300, 400, 500, 700, 950,
    990, 992, 995, 997, 999,
   1000
};

/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static s16b remove_curse_aux(s16b all)
{
   s16b         i, cnt = 0;

   /* Attempt to uncurse items being worn */
   for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      u64b f1, f2, f3;

      object_type *i_ptr = &inventory[i];

      /* Uncursed already */
      if (!cursed_p(i_ptr)) continue;

      /* Extract the flags */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Heavily Cursed Items need a special spell */
      if (!all && (f3 & TR3_HEAVY_CURSE)) continue;

      /* Perma-Cursed Items can NEVER be uncursed */
      if (f3 & TR3_PERMA_CURSE) continue;

      /* Uncurse it */
      i_ptr->ident &= ~ID_CURSED;

      /* Hack -- Assume felt */
      i_ptr->ident |= ID_SENSE;

      /* Take note */
      i_ptr->note = quark_add("uncursed");
 /* jk - scrolls of *remove curse* have a 1 in (55-level chance to */
 /* reverse the curse effects - a ring of damage(-15) {cursed} then */
 /* becomes a ring of damage (+15) */
 /* this does not go for artifacts - a sword of mormegil +40,+60 would */
 /* be somewhat unbalancing */
 /* due to the nature of this procedure, it only works on cursed items */
 /* ie you get only one chance! */
      if ((randint(55-p_ptr->lev)==1) && !artifact_p(i_ptr))
      {
         if (i_ptr->to_a<0) i_ptr->to_a=-i_ptr->to_a;
         if (i_ptr->to_h<0) i_ptr->to_h=-i_ptr->to_h;
         if (i_ptr->to_d<0) i_ptr->to_d=-i_ptr->to_d;
         if (i_ptr->p1val<0) i_ptr->p1val=-i_ptr->p1val;
      }

      /* Window stuff */
      p_ptr->window |= (PW_EQUIP);

      /* Recalculate the bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Count the uncursings */
      cnt++;
   }

   /* Return "something uncursed" */
   return (cnt);
}

/*
 * Remove most curses
 */
bool remove_curse()
{
   return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse()
{
   return (remove_curse_aux(TRUE));
}

/*
 * Restores any drained experience
 */
bool restore_level()
{
   /* Restore experience */
   if (p_ptr->exp < p_ptr->max_exp)
   {
      /* Message */
      msg_print("You feel your life energies returning.");

      /* Restore the experience */
      p_ptr->exp = p_ptr->max_exp;

      /* Check the experience */
      check_experience();

      /* Did something */
      return (TRUE);
   }

   /* No effect */
   return (FALSE);
}

/*
 * self-knowledge... idea from nethack.  Useful for determining powers and
 * resistences of items.  It saves the screen, clears it, then starts listing
 * attributes, a screenful at a time.  (There are a LOT of attributes to
 * list.  It will probably take 2 or 3 screens for a powerful character whose
 * using several artifacts...) -CFT
 *
 * It is now a lot more efficient. -BEN-
 *
 * See also "identify_fully()".
 *
 * XXX XXX XXX Use the "show_file()" method, perhaps.
 */
void self_knowledge()
{
   s16b         i = 0, j, k;

   u64b         f1 = 0LL, f2 = 0LL, f3 = 0LL;
   object_type *i_ptr;

   cptr         info[128];

   /* Acquire item flags from equipment */
   for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
   {
      u64b t1, t2, t3;

      i_ptr = &inventory[k];

      /* Skip empty items */
      if (!i_ptr->k_idx) continue;

      /* Extract the flags */
      object_flags(i_ptr, &t1, &t2, &t3);

      /* Extract flags */
      f1 |= t1;
      f2 |= t2;
      f3 |= t3;
   }


   if (p_ptr->blind)
   {
      info[i++] = "You cannot see.";
   }
   if (p_ptr->confused)
   {
      info[i++] = "You are confused.";
   }
   if (p_ptr->afraid)
   {
      info[i++] = "You are terrified.";
   }
   if (p_ptr->cut)
   {
      info[i++] = "You are bleeding.";
   }
   if (p_ptr->stun)
   {
      info[i++] = "You are stunned.";
   }
   if (p_ptr->poisoned)
   {
      info[i++] = "You are poisoned.";
   }
   if (p_ptr->image)
   {
      info[i++] = "You are hallucinating.";
   }

   if (p_ptr->aggravate)
   {
      info[i++] = "You aggravate monsters.";
   }
   if (p_ptr->teleport)
   {
      info[i++] = "Your position is very uncertain.";
   }

   if (p_ptr->blessed)
   {
      info[i++] = "You feel rightous.";
   }
   if (p_ptr->hero)
   {
      info[i++] = "You feel heroic.";
   }
   if (p_ptr->shero)
   {
      info[i++] = "You are in a battle rage.";
   }
   if (p_ptr->protevil)
   {
      info[i++] = "You are protected from evil.";
   }
   if (p_ptr->shield)
   {
      info[i++] = "You are protected by a mystic shield.";
   }
   if (p_ptr->invuln)
   {
      u16b max = 2000 + p_ptr->lev * 60;
      if (p_ptr->invuln == max)
         info[i++] = "You are temporarily protected by a very powerfull magical shield.";
      else if (p_ptr->invuln >= (max - (max/4)))
         info[i++] = "You are temporarily protected by a powerfull magical shield.";
      else if (p_ptr->invuln >= (max - (max/3)))
         info[i++] = "You are temporarily protected by a strong magical shield.";
      else if (p_ptr->invuln >= (max/3 ))
         info[i++] = "You are temporarily protected by a magical shield.";
      else
         info[i++] = "You are temporarily protected by a weakening magical shield.";
   }
   if (p_ptr->confusing)
   {
      info[i++] = "Your hands are glowing dull red.";
   }
   if (p_ptr->searching)
   {
      info[i++] = "You are looking around very carefully.";
   }
   if (p_ptr->new_spells)
   {
      info[i++] = "You can learn some more spells.";
   }
   if (p_ptr->word_recall)
   {
      info[i++] = "You will soon be recalled.";
   }
   if (p_ptr->see_infra)
   {
      info[i++] = "Your eyes are sensitive to infrared light.";
   }
   if (p_ptr->see_inv)
   {
      info[i++] = "You can see invisible creatures.";
   }
   if (p_ptr->ffall)
   {
      info[i++] = "You land gently.";
   }
   if (p_ptr->free_act)
   {
      info[i++] = "You have free action.";
   }
   if (p_ptr->regenerate)
   {
      info[i++] = "You regenerate quickly.";
   }
   if (p_ptr->slow_digest)
   {
      info[i++] = "Your appetite is small.";
   }
   if (p_ptr->telepathy>0)
   {
      if (p_ptr->telepathy < 10)
      {
         info[i++] = "You have limited ESP.";
      }
      else
      {
         info[i++] = "You have ESP.";
      }
   }
   if (p_ptr->hold_life)
   {
      info[i++] = "You have a firm hold on your life force.";
   }
   if (p_ptr->lite)
   {
      info[i++] = "You are carrying at least one permanent light.";
   }
   if (p_ptr->immune_acid)
   {
      info[i++] = "You are completely immune to acid.";
   }
   else if ((p_ptr->resist_acid) && (p_ptr->oppose_acid))
   {
      info[i++] = "You resist acid exceptionally well.";
   }
   else if ((p_ptr->resist_acid) || (p_ptr->oppose_acid))
   {
      info[i++] = "You are resistant to acid.";
   }

   if (p_ptr->immune_elec)
   {
      info[i++] = "You are completely immune to lightning.";
   }
   else if ((p_ptr->resist_elec) && (p_ptr->oppose_elec))
   {
      info[i++] = "You resist lightning exceptionally well.";
   }
   else if ((p_ptr->resist_elec) || (p_ptr->oppose_elec))
   {
      info[i++] = "You are resistant to lightning.";
   }

   if (p_ptr->immune_fire)
   {
      info[i++] = "You are completely immune to fire.";
   }
   else if ((p_ptr->resist_fire) && (p_ptr->oppose_fire))
   {
      info[i++] = "You resist fire exceptionally well.";
   }
   else if ((p_ptr->resist_fire) || (p_ptr->oppose_fire))
   {
      info[i++] = "You are resistant to fire.";
   }

   if (p_ptr->immune_cold)
   {
      info[i++] = "You are completely immune to cold.";
   }
   else if ((p_ptr->resist_cold) && (p_ptr->oppose_cold))
   {
      info[i++] = "You resist cold exceptionally well.";
   }
   else if ((p_ptr->resist_cold) || (p_ptr->oppose_cold))
   {
      info[i++] = "You are resistant to cold.";
   }

   if ((p_ptr->resist_pois) && (p_ptr->oppose_pois))
   {
      info[i++] = "You resist poison exceptionally well.";
   }
   else if ((p_ptr->resist_pois) || (p_ptr->oppose_pois))
   {
      info[i++] = "You are resistant to poison.";
   }

   if (p_ptr->resist_lite)
   {
      info[i++] = "You are resistant to bright light.";
   }
   if (p_ptr->resist_dark)
   {
      info[i++] = "You are resistant to darkness.";
   }
   if (p_ptr->resist_conf)
   {
      info[i++] = "You are resistant to confusion.";
   }
   if (p_ptr->resist_sound)
   {
      info[i++] = "You are resistant to sonic attacks.";
   }
   if (p_ptr->resist_disen)
   {
      info[i++] = "You are resistant to disenchantment.";
   }
   if (p_ptr->resist_chaos)
   {
      info[i++] = "You are resistant to chaos.";
   }
   if (p_ptr->resist_shard)
   {
      info[i++] = "You are resistant to blasts of shards.";
   }
   if (p_ptr->resist_nexus)
   {
      info[i++] = "You are resistant to nexus attacks.";
   }
   if (p_ptr->resist_neth)
   {
      info[i++] = "You are resistant to nether forces.";
   }
   if (p_ptr->resist_fear)
   {
      info[i++] = "You are completely fearless.";
   }
   if (p_ptr->resist_blind)
   {
      info[i++] = "Your eyes are resistant to blindness.";
   }

   if (p_ptr->sustain_str)
   {
      info[i++] = "Your strength is sustained.";
   }
   if (p_ptr->sustain_int)
   {
      info[i++] = "Your intelligence is sustained.";
   }
   if (p_ptr->sustain_wis)
   {
      info[i++] = "Your wisdom is sustained.";
   }
   if (p_ptr->sustain_con)
   {
      info[i++] = "Your constitution is sustained.";
   }
   if (p_ptr->sustain_dex)
   {
      info[i++] = "Your dexterity is sustained.";
   }
   if (p_ptr->sustain_chr)
   {
      info[i++] = "Your charisma is sustained.";
   }

   if ( (f1 & TR1_STR1) || (f1 & TR1_STR2) )
   {
      info[i++] = "Your strength is affected by your equipment.";
   }
   if ( (f1 & TR1_INT1) || (f1 & TR1_INT2) )
   {
      info[i++] = "Your intelligence is affected by your equipment.";
   }
   if ( (f1 & TR1_WIS1) || (f1 & TR1_WIS2) )
   {
      info[i++] = "Your wisdom is affected by your equipment.";
   }
   if ( (f1 & TR1_DEX1) || (f1 & TR1_DEX2) )
   {
      info[i++] = "Your dexterity is affected by your equipment.";
   }
   if ( (f1 & TR1_CON1) || (f1 & TR1_CON2) )
   {
      info[i++] = "Your constitution is affected by your equipment.";
   }
   if ( (f1 & TR1_CHR1) || (f1 & TR1_CHR2) )
   {
      info[i++] = "Your charisma is affected by your equipment.";
   }

   if ( (f1 & TR1_STEALTH1) || (f1 & TR1_STEALTH2) )
   {
      info[i++] = "Your stealth is affected by your equipment.";
   }
   if ( (f1 & TR1_SEARCH1) || (f1 & TR1_SEARCH2) )
   {
      info[i++] = "Your searching ability is affected by your equipment.";
   }
   if ( (f1 & TR1_INFRA1) || (f1 & TR1_INFRA2) )
   {
      info[i++] = "Your infravision is affected by your equipment.";
   }
   if ( (f1 & TR1_TUNNEL1) || (f1 & TR1_TUNNEL2) )
   {
      info[i++] = "Your digging ability is affected by your equipment.";
   }
   if ( (f1 & TR1_SPEED1) || (f1 & TR1_SPEED2) )
   {
      info[i++] = "Your speed is affected by your equipment.";
   }
   if ( (f1 & TR1_BLOWS1) || (f1 & TR1_BLOWS2) )
   {
      info[i++] = "Your attack speed is affected by your equipment.";
   }
   if ( (f1 & TR1_SHOTS1) || (f1 & TR1_SHOTS2) )
   {
      info[i++] = "Your shooting speed is affected by your equipment.";
   }
   if ( (f1 & TR1_MIGHT1) || (f1 & TR1_MIGHT2) )
   {
      info[i++] = "Your shooting might is affected by your equipment.";
   }


   /* Access the current weapon */
   i_ptr = &inventory[INVEN_WIELD];

   /* Analyze the weapon */
   if (i_ptr->k_idx)
   {
      /* Indicate Blessing */
      if (f3 & TR3_BLESSED)
      {
         info[i++] = "Your weapon has been blessed by the gods.";
      }

      /* Hack */
      if (f1 & TR1_IMPACT)
      {
         info[i++] = "The impact of your weapon can cause earthquakes.";
      }

      /* Special "Attack Bonuses" */
      if (f1 & TR1_BRAND_ACID)
      {
         info[i++] = "Your weapon melts your foes.";
      }
      if (f1 & TR1_BRAND_ELEC)
      {
         info[i++] = "Your weapon shocks your foes.";
      }
      if (f1 & TR1_BRAND_FIRE)
      {
         info[i++] = "Your weapon burns your foes.";
      }
      if (f1 & TR1_BRAND_COLD)
      {
         info[i++] = "Your weapon freezes your foes.";
      }

      /* Special "slay" flags */
      if (f1 & TR1_SLAY_ANIMAL)
      {
         info[i++] = "Your weapon strikes at animals with extra force.";
      }
      if (f1 & TR1_SLAY_EVIL)
      {
         info[i++] = "Your weapon strikes at evil with extra force.";
      }
      if (f1 & TR1_SLAY_UNDEAD)
      {
         info[i++] = "Your weapon strikes at undead with holy wrath.";
      }
      if (f1 & TR1_SLAY_DEMON)
      {
         info[i++] = "Your weapon strikes at demons with holy wrath.";
      }
      if (f1 & TR1_SLAY_ORC)
      {
         info[i++] = "Your weapon is especially deadly against orcs.";
      }
      if (f1 & TR1_SLAY_TROLL)
      {
         info[i++] = "Your weapon is especially deadly against trolls.";
      }
      if (f1 & TR1_SLAY_GIANT)
      {
         info[i++] = "Your weapon is especially deadly against giants.";
      }
      if (f1 & TR1_SLAY_DRAGON)
      {
         info[i++] = "Your weapon is especially deadly against dragons.";
      }

      /* Special "kill" flags */
      if (f1 & TR1_KILL_DRAGON)
      {
         info[i++] = "Your weapon is a great bane of dragons.";
      }
   }

   /* Save the screen */
   Term_save();

   /* Erase the screen */
   for (k = 1; k < 24; k++) prt("", 13, k);

   /* Label the information */
   prt("   Your Attributes:", 15, 1);

   /* We will print on top of the map (column 13) */
   for (k = 2, j = 0; j < i; j++)
   {
      /* Show the info */
      prt(info[j], 15, k++);

      /* Every 20 entries (lines 2 to 21), start over */
      if ((k == 22) && (j+1 < i))
      {
         prt("-- more --", 15, k);
         inkey();
         for ( ; k > 2; k--) prt("", 15, k);
      }
   }

   /* Pause */
   prt("[Press any key to continue]", 13, k);
   inkey();

   /* Restore the screen */
   Term_load();
}

void forget_item(object_type *i_ptr)
{
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

   /* Hack -- Clear the "empty" flag */
   i_ptr->ident &= ~ID_EMPTY;

   /* Hack -- Clear the "known" flag */
   i_ptr->ident &= ~ID_KNOWN;
   if (i_ptr->name1) a_info[i_ptr->name1].ident |= ~ID_KNOWN;

   /* Hack -- Clear the "felt" flag */
   i_ptr->ident &= ~ID_SENSE;
}

/*
 * Forget everything
 */
bool lose_all_info(void)
{
   s16b                 i;

   /* Forget info about objects */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      object_type *i_ptr = &inventory[i];

      /* Skip non-items */
      if (!i_ptr->k_idx) continue;

      /* Allow "protection" by the MENTAL flag */
      if (i_ptr->ident & ID_MENTAL) continue;

      forget_item(i_ptr);
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Mega-Hack -- Forget the map */
   wiz_dark();

   /* It worked */
   return (TRUE);
}

/* jk
 * Forget some things
 * chance = 0-100 % chance of forgetting items.
 * take special care to return true only if something actually happened
 */
bool lose_some_info(s16b chance)
{
   s16b                 i;
   bool                result = FALSE;

   /* Forget info about objects */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      object_type *i_ptr = &inventory[i];

      /* Skip non-items */
      if (!i_ptr->k_idx) continue;

      /* Allow "protection" by the MENTAL flag */
      if (i_ptr->ident & ID_MENTAL) continue;

      if (rand_int(100)>chance) continue;

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
            result=TRUE;
         }
      }

      /* Hack -- Clear the "empty" flag */
      result |= (i_ptr->ident & ID_EMPTY);
      i_ptr->ident &= ~ID_EMPTY;

      /* Hack -- Clear the "known" flag */
      result |= (i_ptr->ident & ID_KNOWN);
      i_ptr->ident &= ~ID_KNOWN;
      if (i_ptr->name1) a_info[i_ptr->name1].ident |= ~ID_KNOWN;

      /* Hack -- Clear the "felt" flag */
      result |= (i_ptr->ident & ID_SENSE);
      i_ptr->ident &= ~ID_SENSE;

      /* Hack -- Clear the "tried" flag */
      if (need_tries(i_ptr))
      {
         result |= (i_ptr->ident & ID_EQUIPTESTED);
         i_ptr->ident &= ~ID_EQUIPTESTED;
      }
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* It worked */
   return (result);
}

/*
 * detect any treasure at given coordinates
 */
bool detect_treasure_xy(s16b x, s16b y)
{
   s16b             j;
   object_type     *i_ptr;
   bool             detect = FALSE;
   cave_cell_type  *c_ptr;

   /* jk - total hack, assume gold is always in [0] */
   for (j=objects_on_floor(x,y)-1;j>=0;j--)
   {
      i_ptr = get_item_pointer_floor_xy(j,x,y);
      /* Notice gold */
      if (i_ptr->tval == TV_GOLD)
      {
          /* Notice new items */
          if (!(i_ptr->marked))
          {
              /* Detect */
              detect = TRUE;

              /* Hack -- memorize the item */
              i_ptr->marked = TRUE;

              /* Redraw */
              lite_spot(x, y);
              break;
          }
      }
   }
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Magma/Quartz + Known Gold */
   if (treasure(x,y))
   {
      /* Notice detected gold */
      if (!(c_ptr->fdat & CAVE_MARK))
      {
         switch (wall_art(c_ptr->styp))
         {
            case DUNG_WALL_ART_GRANITE:
                 (void)set_grid_type(x, y, DUNG_WALL,
                                     DUNG_WALL_GRTREAS, GRID_ADD, 0);
                 break;
            case DUNG_WALL_ART_CHALK:
                 (void)set_grid_type(x, y, DUNG_WALL,
                                     DUNG_WALL_CHTREAS, GRID_ADD, 0);
                 break;
            case DUNG_WALL_ART_QUARTZ:
                 (void)set_grid_type(x, y, DUNG_WALL,
                                     DUNG_WALL_QUTREAS, GRID_ADD, 0);
                 break;
            case DUNG_WALL_ART_MAGMA:
                 (void)set_grid_type(x, y, DUNG_WALL,
                                     DUNG_WALL_MGTREAS, GRID_ADD, 0);
         }
         c_ptr->fdat &= ~CAVE_MIMIC; /* this we do not anymore */
         detect = TRUE;            /* Detect */
         c_ptr->fdat |= CAVE_MARK; /* Hack -- memorize the feature */
         lite_spot(x, y);          /* Redraw */
      }
   }
   return (detect);
}

/*
 * Detect any treasure on the current panel             -RAK-
 *
 * We do not yet create any "hidden gold" features XXX XXX XXX
 */
bool detect_treasure(void)
{
   s16b         x, y;
   bool         detect = FALSE;

   /* Scan the current panel */
   for (y = panel_min_row; y <= panel_max_row; y++)
   {
      for (x = panel_min_col; x <= panel_max_col; x++)
      {
         detect |= detect_treasure_xy(x, y);
      }
   }
   return (detect);
}

/*
 * detect magic items at the given coordinates
 */
bool detect_magic_xy(s16b x, s16b y)
{
   s16b         tv;
   s16b         k;
   bool        detect = FALSE;
   object_type *i_ptr;
   /* Access the grid and object */
   for (k=objects_on_floor(x,y)-1;k>=0;k--)
   {
      i_ptr = get_item_pointer_floor_xy(k,x,y);

      /* Examine the tval */
      tv = i_ptr->tval;

      /* Artifacts, misc magic items, or enchanted wearables */
      if (artifact_p(i_ptr) || ego_item_p(i_ptr) ||
          (tv == TV_AMULET) || (tv == TV_RING) ||
          (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
          (tv == TV_SCROLL) || (tv == TV_POTION) || (tv == TV_BOOK) ||
          ((i_ptr->to_a > 0) || (i_ptr->to_h + i_ptr->to_d > 0)))
      {
         /* Note new items */
         if (!(i_ptr->marked))
         {
            /* Detect */
            detect = TRUE;

            /* Memorize the item */
            i_ptr->marked = TRUE;

            /* Redraw */
            lite_spot(x, y);
         }
      }
   }
   return (detect);
}

/*
 * Detect magic items.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_magic()
{
   s16b         x, y;
   bool        detect = FALSE;


   /* Scan the current panel */
   for (y = panel_min_row; y <= panel_max_row; y++)
   {
      for (x = panel_min_col; x <= panel_max_col; x++)
      {
         detect |= detect_magic_xy(x, y);
      }
   }

   /* Return result */
   return (detect);
}

/*
 * Locates and displays all invisible creatures on current panel -RAK-
 */
bool detect_invisible()
{
   s16b         i;
   bool        flag = FALSE;


   /* Detect all invisible monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];
      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      s16b fx = m_ptr->fx;
      s16b fy = m_ptr->fy;
      s16b fz = m_ptr->fz;

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      if (fz != sublevel) continue;

      /* Skip visible monsters */
      if (m_ptr->ml) continue;

      /* Detect all invisible monsters */
      if (panel_contains(fx, fy) && (r_ptr->flags2 & RF2_INVISIBLE))
      {
         /* Take note that they are invisible */
         r_ptr->r_flags2 |= RF2_INVISIBLE;

         /* Mega-Hack -- Show the monster */
         m_ptr->ml = TRUE;
         lite_spot(fx, fy);
         flag = TRUE;
      }
   }

   /* Describe result, and clean up */
   if (flag)
   {
      /* Describe, and wait for acknowledgement */
      msg_print("You sense the presence of invisible creatures!");
      msg_print(NULL);

      /* Mega-Hack -- Fix the monsters */
      update_monsters(FALSE);
   }

   /* Result */
   return (flag);
}



/*
 * Display evil creatures on current panel              -RAK-
 */
bool detect_evil(void)
{
   s16b         i;
   bool        flag = FALSE;


   /* Display all the evil monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];
      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      s16b fy = m_ptr->fy;
      s16b fx = m_ptr->fx;
      s16b fz = m_ptr->fz;

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      if (fz != sublevel) continue;

      /* Skip visible monsters */
      if (m_ptr->ml) continue;

      /* Detect evil monsters */
      if (panel_contains(fx, fy) && (r_ptr->flags3 & RF3_EVIL))
      {
         /* Mega-Hack -- Show the monster */
         m_ptr->ml = TRUE;
         lite_spot(fx, fy);
         flag = TRUE;
      }
   }

   /* Note effects and clean up */
   if (flag)
   {
      /* Describe, and wait for acknowledgement */
      msg_print("You sense the presence of evil!");
      msg_print(NULL);

      /* Mega-Hack -- Fix the monsters */
      update_monsters(FALSE);
   }

   /* Result */
   return (flag);
}



/*
 * Display all non-invisible monsters on the current panel
 */
bool detect_monsters(void)
{
   s16b         i;
   bool        flag = FALSE;


   /* Detect non-invisible monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];
      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      s16b fx = m_ptr->fx;
      s16b fy = m_ptr->fy;
      s16b fz = m_ptr->fz;

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      if (fz != sublevel) continue;

      /* Skip visible monsters */
      /* jk - monsters in a forest have ml, but still can't be seen */

      if ((dungeon.level[sublevel][fy][fx].mtyp!=DUNG_SHRUB) && m_ptr->ml) continue;

      /* Detect all non-invisible monsters */
      if (panel_contains(fx, fy) && (!(r_ptr->flags2 & RF2_INVISIBLE)))
      {
         /* Mega-Hack -- Show the monster */
         m_ptr->ml = TRUE;
         lite_spot(fx, fy);
         flag = TRUE;
      }
   }

   /* Describe and clean up */
   if (flag)
   {
      /* Describe, and wait for acknowledgement */
      msg_print("You sense the presence of monsters!");
      msg_print(NULL);

      /* Mega-Hack -- Fix the monsters */
      update_monsters(FALSE);
   }

   /* Result */
   return (flag);
}


/*
 * Detect everything
 */
bool detection(void)
{
   s16b         i;
   bool        flag = FALSE;
   bool        detect = FALSE;


   /* Detect the easy things */
   if (detect_treasure()) detect = TRUE;
   if (detect_object()) detect = TRUE;
   if (detect_sdoor()) detect = TRUE;
   if (detect_trap()) detect = TRUE;

   /* Detect all monsters in the current panel */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      s16b fx = m_ptr->fx;
      s16b fy = m_ptr->fy;
      s16b fz = m_ptr->fz;

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      if (fz != sublevel) continue;

      /* Skip visible monsters */

      if ((dungeon.level[sublevel][fy][fx].mtyp!=DUNG_SHRUB) && m_ptr->ml) continue;

      /* Detect all monsters */
      if (panel_contains(fx, fy))
      {
          /* Mega-Hack -- Show the monster */
          m_ptr->ml = TRUE;
          lite_spot_detect(fx, fy);
          flag = detect = TRUE;
      }
   }
   /* Describe the result, then fix the monsters */
   if (flag)
   {
      /* Describe, and wait for acknowledgement */
      msg_print("You sense the presence of monsters!");
      msg_print(NULL);

      /* Mega-Hack -- Fix the monsters */
      update_monsters(FALSE);
   }

   /* Result */
   return (detect);
}

/*
 * detect all objects at given coordinates
 */
bool detect_object_xy(s16b x, s16b y)
{
   s16b         k;
   bool        detect = FALSE;
   object_type *i_ptr;

   for (k=objects_on_floor(x,y)-1;k>=0;k--)
   {
      i_ptr = get_item_pointer_floor_xy(k,x,y);

      /* Do not detect "gold" */
      if (i_ptr->tval == TV_GOLD) continue;

      /* Note new objects */
      if (!(i_ptr->marked))
      {
         /* Detect */
         detect = TRUE;

         /* Hack -- memorize it */
         i_ptr->marked = TRUE;

         /* Redraw */
         lite_spot(x, y);
      }
   }
   return (detect);
}

/*
 * Detect all objects on the current panel              -RAK-
 */
bool detect_object(void)
{
   s16b         x, y;
   bool        detect = FALSE;

   /* Scan the current panel */
   for (y = panel_min_row; y <= panel_max_row; y++)
   {
      for (x = panel_min_col; x <= panel_max_col; x++)
      {
         detect |= detect_object_xy(x, y);
      }
   }

   return (detect);
}

/*
 * detect traps at given coordinates
 */
bool detect_trap_xy(s16b x, s16b y)
{
   bool             detect = FALSE;
   cave_cell_type  *c_ptr;
   /* Access the grid */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Detect invisible traps */
   if (test_grid_ptr(c_ptr,DUNG_FLOOR,DUNG_FLOOR_TRAP))
   {
dlog(DEBUGTRAPS,"detect_trap: %d,%d hidden found t_idx %d\n",x,y,c_ptr->t_idx);
      /* Hack -- memorize it */
      c_ptr->fdat |= CAVE_MARK;

      find_one_trap(x, y);
      lite_spot(x, y);

      /* Obvious */
      detect = TRUE;
   }
   else if ( (c_ptr->mtyp == DUNG_TRAP) &&
        (c_ptr->styp == DUNG_TRAP_FNDONE))
   {
      /* this makes all traps there known */
      find_all_other_traps(x, y);

      c_ptr->fdat |= CAVE_MARK;
      lite_spot(x, y);

      /* Obvious */
      detect = TRUE;
   }
   /* find traps on doors, but not on secret doors !*/
   if ((c_ptr->mtyp == DUNG_DOOR) &&
       (c_ptr->t_idx) && (c_ptr->styp != DUNG_DOOR_SECRET))
   {
      /* Hack -- memorize it */
      s16b           i, cnt = 0;
      trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
      for (i=1; i<t_number; i++)
      {
         if (get_trap(tr_ptr,i) && !trap_found_ptr(tr_ptr,i))
         {
            set_trap_found_ptr(tr_ptr, i);
            cnt++;
         }
      }
      if (cnt==1)
      {
         msg_print("You have found a booby-trapped door.");
      }
      else
      {
         msg_print("You have found a multiple-trap door.");
      }

      c_ptr->fdat |= CAVE_MARK;

      /* Redraw */
      lite_spot(x, y);

      /* Obvious */
      detect = TRUE;
   }
   return (detect);
}

/*
 * Locates and displays traps on current panel
 */
bool detect_trap(void)
{
   s16b             x, y;
   bool             detect = FALSE;

   /* Scan the current panel */
   for (y = panel_min_row; y <= panel_max_row; y++)
   {
      for (x = panel_min_col; x <= panel_max_col; x++)
      {
         detect |= detect_trap_xy(x, y);
      }
   }
   return (detect);
}

/*
 * detect secret doors at given coordinates
 */
bool detect_sdoor_xy(s16b x, s16b y)
{
   bool        detect = FALSE;
   cave_cell_type *c_ptr;

   /* Access the grid and object */
   c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGTRAPS,"spells2.c: detect_sdoor: trying %d,%d mt %d st %d feat %s\n",
                x, y, c_ptr->mtyp, c_ptr->styp, f_name + f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name);
   /* Hack -- detect secret doors */
   if (test_grid_ptr(c_ptr, DUNG_DOOR, DUNG_DOOR_SECRET))
   {
      s16b doorchance;
      /* Find the door XXX XXX XXX */
      doorchance = randint(150-p_ptr->mdepth);
      if (c_ptr->fdat & CAVE_VAULT) doorchance = doorchance/4;

      if (doorchance<5)
      {
         (void)set_grid_type(x,y,DUNG_DOOR,
                             DUNG_DOOR_JAMMED, GRID_ADD, 0);
      }
      else if (doorchance<10)
      {
         (void)set_grid_type(x,y,DUNG_DOOR,
                             DUNG_DOOR_LOCKED, GRID_ADD, 0);
      }
      else
      {
         (void)set_grid_type(x,y,DUNG_DOOR,
                             DUNG_DOOR_CLOSED, GRID_ADD, 0);
      }

      /* Memorize the door */
      c_ptr->fdat |= CAVE_MARK;

      /* Redraw */
      lite_spot(x, y);

      /* Obvious */
      detect = TRUE;
   }
   else if ((c_ptr->mtyp == DUNG_DOOR) || (c_ptr->mtyp == DUNG_ENTR))
   {
      /* Memorize the door */
      c_ptr->fdat |= CAVE_MARK;

      /* Redraw */
      lite_spot(x, y);

      /* Obvious */
      detect = TRUE;
   }

   /* Ignore known grids */
   if (c_ptr->fdat & CAVE_MARK) return (detect);

   /* Hack -- detect stairs */
   if (c_ptr->mtyp == DUNG_STAIR)
   {
      /* Memorize the stairs */
      c_ptr->fdat |= CAVE_MARK;

      /* Redraw */
      lite_spot(x, y);

      /* Obvious */
      detect = TRUE;
   }
   return (detect);
}

/*
 * Locates and displays all stairs and secret doors on current panel -RAK-
 */
bool detect_sdoor()
{
   s16b        x, y;
   bool        detect = FALSE;

   /* Scan the panel */
   for (y = panel_min_row; y <= panel_max_row; y++)
   {
      for (x = panel_min_col; x <= panel_max_col; x++)
      {
         detect |= detect_sdoor_xy(x, y);
      }
   }
   return (detect);
}

/*
 * Create stairs at the player location
 */
void stair_creation()
{
   /* Access the grid */
   cave_cell_type *c_ptr;

   /* Access the player grid */
   c_ptr = &dungeon.level[sublevel][py][px];

   /* don't indulge naughty hackers */
   if (!valid_grid(px, py) || (dungeon.level[sublevel][py][px].fdat & CAVE_AREN))
   {
      msg_print("The object resists the spell.");
      return;
   }

   /* Hack -- Delete old contents */
   delete_object(px, py, -1);

   /* Town -- must go down */
   if (!p_ptr->mdepth)
   {
      /* Clear previous contents, add down stairs */
      place_main_down_stair(px,py,TRUE);
   }

   /* Quest -- must go up */
   else if (is_quest(p_ptr->mdepth) || (p_ptr->mdepth>= MAX_LEVEL-1))
   {
      /* Clear previous contents, add up stairs */
      place_main_up_stair(px,py,TRUE);
   }

   /* Requested type */
   else
   {
      /* Clear previous contents, add stairs */
      if (randint(2)==1)
      {
         place_main_up_stair(px,py,TRUE);
      }
      else
      {
         place_main_down_stair(px,py,TRUE);
      }
   }

   /* Notice */
   note_spot(px, py);

   /* Redraw */
   lite_spot(px, py);
}

/*
 * Hook to specify "weapon"
 */
static bool test_weapon(object_type *i_ptr, bool tohit_allowed, bool todam_allowed)
{
   switch (i_ptr->tval)
   {
      case TV_SWORD:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_DIGGING:
      case TV_BOW:
      case TV_BOLT:
      case TV_ARROW:
      case TV_SHOT:
           return (TRUE);
   }
/* jwk - some rings count also */
   if (i_ptr->tval==TV_RING)
   {
      switch (i_ptr->sval)
      {
         case SV_RING_DAMAGE:
            return (todam_allowed);
         case SV_RING_ACCURACY:
            return (tohit_allowed);
         case SV_RING_SLAYING:
            return (TRUE);
      }
   }
   if ((i_ptr->tval==TV_GLOVES) && (i_ptr->name2==EGO_SLAYING)) return (TRUE);
/* the two hard-hitting gloves */
   if (i_ptr->name1==ART_CAMBELEG) return (TRUE);
   if (i_ptr->name1==ART_FINGOLFIN) return (TRUE);

   return (FALSE);
}

/*
 * Hook to specify "weapon tohit only"
 */
static bool item_tester_hook_weapon_tohit(object_type *i_ptr)
{
   return (test_weapon(i_ptr, TRUE, FALSE));
}

/*
 * Hook to specify "weapon todam only"
 */
static bool item_tester_hook_weapon_todam(object_type *i_ptr)
{
   return (test_weapon(i_ptr, FALSE, TRUE));
}

/*
 * Hook to specify "weapon in general"
 */
static bool item_tester_hook_weapon(object_type *i_ptr)
{
   return (test_weapon(i_ptr, TRUE, TRUE));
}

/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(object_type *i_ptr)
{
   switch (i_ptr->tval)
   {
      case TV_DRAG_ARMOR:
      case TV_HARD_ARMOR:
      case TV_SOFT_ARMOR:
      case TV_SHIELD:
      case TV_CLOAK:
      case TV_CROWN:
      case TV_HELM:
      case TV_BOOTS:
      case TV_GLOVES:
          return (TRUE);
   }
/* jwk - some other things count also */
   if (i_ptr->tval==TV_RING)
   {
      switch (i_ptr->sval)
      {
         case SV_RING_PROTECTION:
         case SV_RING_FLAMES:
         case SV_RING_ICE:
         case SV_RING_ACID:
            return (TRUE);
      }
   }
   if ((i_ptr->tval==TV_AMULET) && (i_ptr->sval == SV_AMULET_THE_MAGI))
   {
      return (TRUE);
   }
/* Defender/Holy Avenger also has innate armour capabilities */
   if ( (i_ptr->name2==EGO_HA) || (i_ptr->name2==EGO_DF) ) return (TRUE);

/* now for the artifacts which influence AC */
   if (i_ptr->name1==ART_ANGRIST) return (TRUE);
   if (i_ptr->name1==ART_MORMEGIL) return (TRUE);
   if (i_ptr->name1==ART_ANDURIL) return (TRUE);
   if (i_ptr->name1==ART_DOOMCALLER) return (TRUE);
   if (i_ptr->name1==ART_TIL) return (TRUE);
   if (i_ptr->name1==ART_AEGLOS) return (TRUE);
   if (i_ptr->name1==ART_DURIN) return (TRUE);
   if (i_ptr->name1==ART_EONWE) return (TRUE);
   if (i_ptr->name1==ART_BALLI) return (TRUE);
   if (i_ptr->name1==ART_AVAVIR) return (TRUE);
   if (i_ptr->name1==ART_GROND) return (TRUE);
   if (i_ptr->name1==ART_FIRESTAR) return (TRUE);
   if (i_ptr->name1==ART_AULE) return (TRUE);
   if (i_ptr->name1==ART_TURMIL) return (TRUE);
   if (i_ptr->name1==ART_SUN) return (TRUE);


   return (FALSE);
}

/*
 * Brand the current weapon
 */
void brand_weapon(void)
{
   object_type *i_ptr;

   i_ptr = &inventory[INVEN_WIELD];

   /* you can never modify artifacts / ego-items */
   /* you can never modify broken / cursed items */
   if ((i_ptr->k_idx) &&
       (!artifact_p(i_ptr)) && (!ego_item_p(i_ptr)) &&
       (!broken_p(i_ptr)) && (!cursed_p(i_ptr)))
   {
      cptr act = NULL;

      char i_name[80];

      if (rand_int(100) < 25)
      {
          act = "is covered in a fiery shield!";
          i_ptr->name2 = EGO_BRAND_FIRE;
      }

      else
      {
          act = "glows deep, icy blue!";
          i_ptr->name2 = EGO_BRAND_COLD;
      }

      object_desc(i_name, i_ptr, FALSE, 0);

      msg_format("Your %s %s", i_name, act);

/* jk - the 100 signals that it shouldn't work like *ENCHANT* */
      enchant(i_ptr, 100+rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
   }

   else
   {
      if (flush_failure) flush();
      msg_print("The Branding failed.");
   }
}

/*
 * Enchants a plus onto an item.                        -RAK-
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *i_ptr, s16b n, s16b eflag)
{
   s16b i, chance, prob;

   bool res = FALSE;

   bool is_artifact = artifact_p(i_ptr);

/* jk */
   s16b star_extra = 0;
   s16b plusval;
   object_kind *k_ptr = &k_info[i_ptr->k_idx];

   u64b f1, f2, f3;

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

/* jk */
   if (n>100)
   {
      n-=100;
   }
   else if ((n>1) && (randint(100-p_ptr->lev)<30))
   {
      star_extra=1; /* extra enchantment past +15 possible */
      n=1;
   }
   /* enchant an extra plus onto a weapon, if every test works out */
   /* first, we have to have a *enchant* scroll */
   else if ((n>1) &&
   /* then we have to test if is *enchant weapon*, not armor */
   /* if we test tohit || todam, it may happen twice in one scroll.... */
            (eflag & ENCH_TODAM) &&
   /* if we have already boosted the damage dice, chances become less... */
   /* first 'extra' dd 30%, next 5%, then 1% */
            (rand_int(1000)>enchant_table[((i_ptr->dd+8-k_ptr->dd)>15)?15:(i_ptr->dd+8-k_ptr->dd)]) &&
   /* it makes only sense on weapons, not on bows or rings of slaying etc */
            (wield_slot(i_ptr)==INVEN_WIELD) &&
   /* artifacts are immune against this */
            !is_artifact)
   {
      star_extra=2; /* supercharge damage dice */
      i_ptr->dd++;
      res=TRUE;
      if (i_ptr->dd>9)   /* that's a little too much */
      {
         i_ptr->dd=9;    /* pc just wasted a scroll! */
         res=FALSE;
      }
      n=1;
   }

   /* Large piles resist enchantment */
   prob = i_ptr->number * 100;

   /* Missiles are easy to enchant */
   if ((i_ptr->tval == TV_BOLT) ||
       (i_ptr->tval == TV_ARROW) ||
       (i_ptr->tval == TV_SHOT))
   {
      prob = prob / 20;
   }

   /* Try "n" times */
   for (i=0; i<n; i++)
   {
      /* Hack -- Roll for pile resistance */
      if (rand_int(prob) >= 100) continue;

      /* Enchant to hit */
      if (eflag & ENCH_TOHIT)
      {
         if (i_ptr->to_h < 0) chance = 0;
         else if (i_ptr->to_h > 15) chance = 1000;
         else chance = enchant_table[i_ptr->to_h];

         if (star_extra==1)
         {
            plusval = i_ptr->to_h-(p_ptr->lev/10)-3;
            if (plusval < 0) chance = 0;
            else if (plusval > 15) chance = 1000;
            else chance = enchant_table[plusval];
         }

         if ((randint(1000) > chance) && (!is_artifact || (rand_int(100) < 50)))
         {
            i_ptr->to_h++;
            res = TRUE;

            /* only when you get it above -1 -CFT */
            if (cursed_p(i_ptr) &&
                (!(f3 & TR3_PERMA_CURSE)) &&
                (i_ptr->to_h >= 0) && (rand_int(100) < 25))
            {
               msg_print("The curse is broken!");
               i_ptr->ident &= ~ID_CURSED;
               i_ptr->ident |= ID_SENSE;
               i_ptr->note = quark_add("uncursed");
            }
         }
      }

      /* Enchant to damage */
      if (eflag & ENCH_TODAM)
      {
         if (i_ptr->to_d < 0) chance = 0;
         else if (i_ptr->to_d > 15) chance = 1000;
         else chance = enchant_table[i_ptr->to_d];

         if (star_extra==1)
         {
            plusval = i_ptr->to_d-(p_ptr->lev/10)-3;
            if (plusval < 0) chance = 0;
            else if (plusval > 15) chance = 1000;
            else chance = enchant_table[plusval];
         }

         if ((randint(1000) > chance) && (!is_artifact || (rand_int(100) < 50)))
         {
            i_ptr->to_d++;
            res = TRUE;

            /* only when you get it above -1 -CFT */
            if (cursed_p(i_ptr) &&
                (!(f3 & TR3_PERMA_CURSE)) &&
                (i_ptr->to_d >= 0) && (rand_int(100) < 25))
            {
               msg_print("The curse is broken!");
               i_ptr->ident &= ~ID_CURSED;
               i_ptr->ident |= ID_SENSE;
               i_ptr->note = quark_add("uncursed");
            }
         }
      }

      /* Enchant to armor class */
      if (eflag & ENCH_TOAC)
      {
         if (i_ptr->to_a < 0) chance = 0;
         else if (i_ptr->to_a > 15) chance = 1000;
         else chance = enchant_table[i_ptr->to_a];

         if (star_extra==1)
         {
            plusval = i_ptr->to_d-(p_ptr->lev/10)-3;
            if (plusval < 0) chance = 0;
            else if (plusval > 15) chance = 1000;
            else chance = enchant_table[plusval];
         }

         if ((randint(1000) > chance) && (!is_artifact || (rand_int(100) < 50)))
         {
            i_ptr->to_a++;
            res = TRUE;

            /* only when you get it above -1 -CFT */
            if (cursed_p(i_ptr) &&
                (!(f3 & TR3_PERMA_CURSE)) &&
                (i_ptr->to_a >= 0) && (rand_int(100) < 25))
            {
               msg_print("The curse is broken!");
               i_ptr->ident &= ~ID_CURSED;
               i_ptr->ident |= ID_SENSE;
               i_ptr->note = quark_add("uncursed");
            }
         }
      }
   }

   if ( (star_extra==1) && res)
   {
      msg_format("The glow dims slowly");
   }

   if ( (star_extra==2) && res)
   {
      msg_format("It feels different");
   }

   /* Failure */
   if (!res) return (FALSE);

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Success */
   return (TRUE);
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(s16b num_hit, s16b num_dam, s16b num_ac)
{
   s16b                 item;
   bool                okay = FALSE;

   object_type         *i_ptr = NULL;
   char                i_name[80];

/* jk */
   s16b amt = 1;

   /* Assume enchant weapon */
   item_tester_hook = item_tester_hook_weapon;
   if ((num_hit > 0) && (num_dam == 0)) item_tester_hook = item_tester_hook_weapon_tohit;
   if ((num_hit == 0) && (num_dam > 0)) item_tester_hook = item_tester_hook_weapon_todam;

   /* Enchant armor if requested */
   if (num_ac) item_tester_hook = item_tester_hook_armour;

   /* Get an item (from equip or inven or floor) */
   if (!get_item(&item, &amt, "Enchant which item? ", TRUE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to enchant.");
      item_tester_hook = NULL;
      return (FALSE);
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

/* jk - hack to prevent sv_scroll_start_enchant_weapon to enchant */
/* the to-hit of damage rings, or the to-dam of accuracy rings */
   if (i_ptr->tval==TV_RING)
   {
      if ((num_hit>0) && (i_ptr->sval == SV_RING_DAMAGE))
      {
         num_hit=0;
      }
      if ((num_dam>0) && (i_ptr->sval == SV_RING_ACCURACY))
      {
         num_dam=0;
      }
   }

   /* Description */
   object_desc(i_name, i_ptr, FALSE, 0);

   /* Describe */
   msg_format("%s %s glow%s brightly!",
              ((item >= 0) ? "Your" : "The"), i_name,
              ((i_ptr->number > 1) ? "" : "s"));

   /* Enchant */
   if (enchant(i_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
   if (enchant(i_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
   if (enchant(i_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

   /* Failure */
   if (!okay)
   {
      /* Flush */
      if (flush_failure) flush();

      /* Message */
      msg_print("The enchantment failed.");
   }

   /* Something happened */
   return (TRUE);
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell()
{
   s16b                item;
   object_type        *i_ptr = NULL;
   char                i_name[80];
/* jk */
   s16b amt = 1;

   /* Get an item (from equip or inven or floor) */
   if (!get_item(&item, &amt, "Identify which item? ", TRUE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to identify.");
      return (FALSE);
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Identify it fully */
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);

   if ( (i_ptr->name1) && (a_info[i_ptr->name1].log.found_when == 0) )
   {
      a_info[i_ptr->name1].log.found_when = turn;
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Description */
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Describe */
   if ((item >= INVEN_WIELD) && (item<INVEN_TOTAL))
   {
      msg_format("%^s: %s (%c).",
                 describe_use(item), i_name, index_to_label(item));
   }
   else if (item <INVEN_WIELD)
   {
      msg_format("In your pack: %s (%c).",
                 i_name, index_to_label(item));
   }
   else
   {
      msg_format("On the ground: %s.",
                 i_name);
   }

   /* Something happened */
   return (TRUE);
}

/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully()
{
   s16b                 item;

   object_type         *i_ptr = NULL;

   char                i_name[80];

/* jk */
   s16b amt = 1;

   /* Get an item (from equip or inven or floor) */
   if (!get_item(&item, &amt, "Identify which item? ", TRUE, TRUE, TRUE))
   {
       if (item == -2) msg_print("You have nothing to identify.");
       return (FALSE);
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Identify it fully */
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);

   /* Mark the item as fully known */
   i_ptr->ident |= (ID_MENTAL);

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Handle stuff */
   handle_stuff();

   /* Description */
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Describe */
   if ((item >= INVEN_WIELD) && (item<INVEN_TOTAL))
   {
      msg_format("%^s: %s (%c).",
                 describe_use(item), i_name, index_to_label(item));
   }
   else if (item <INVEN_WIELD)
   {
      msg_format("In your pack: %s (%c).",
                 i_name, index_to_label(item));
   }
   else
   {
      msg_format("On the ground: %s.",
                 i_name);
   }

   /* Describe it fully */
   identify_fully_aux(i_ptr, FALSE);

   /* Success */
   return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool item_tester_hook_recharge(object_type *i_ptr)
{
   /* Recharge staffs */
   if (i_ptr->tval == TV_STAFF) return (TRUE);

   /* Recharge wands */
   if (i_ptr->tval == TV_WAND) return (TRUE);

   /* Hack -- Recharge rods */
   if (i_ptr->tval == TV_ROD) return (TRUE);

   /* Nope */
   return (FALSE);
}

/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 * Mage -- Recharge I --> recharge(5)
 * Mage -- Recharge II --> recharge(40)
 * Mage -- Recharge III --> recharge(100)
 *
 * Priest -- Recharge --> recharge(15)
 *
 * Scroll of recharging --> recharge(60)
 *
 * recharge(20) = 1/6 failure for empty 10th level wand
 * recharge(60) = 1/10 failure for empty 10th level wand
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staffs/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 * XXX XXX XXX Perhaps we should auto-unstack recharging stacks.
 */
bool recharge(s16b num)
{
   s16b                 i = 0, t, item, lev, amt = 1;
   s16b                 total_chance = 100, charged;
   object_type         *i_ptr = NULL;

   /* Only accept legal items */
   item_tester_hook = item_tester_hook_recharge;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Recharge which item? ", FALSE, TRUE, TRUE))
   {
       if (item == -2) msg_print("You have nothing to recharge.");
       item_tester_hook = NULL;
       return (FALSE);
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* Extract the object "level" */
   lev = k_info[i_ptr->k_idx].level;

   i_ptr = get_item_pointer(item);

   total_chance += num;

   /* Recharge a rod */
   if (i_ptr->tval == TV_ROD)
   {

      /* get number of charged rods in this stack */
      /* if we have a stack of 17 rods, with 1 uncharged, it's very difficult to charge that one */
      /* if we have a stack of 17 rods, with 16 uncharged, it should be easy to charge one       */
      charged = (i_ptr->number - ( (i_ptr->p1val+get_rod_charge(i_ptr)-1) / get_rod_charge(i_ptr) ) );
      /* Extract a recharge power */
      i = (total_chance - (lev + charged)) / 5;

      /* Paranoia -- prevent crashes */
      if (i < 1) i = 1;

      /* Back-fire */
      if ( rand_int(i) == 0 )
      {
         /* Hack -- backfire */
         if (num > 2)
         {
            msg_print("The recharge backfires, draining the rods further!");
         }
         else if (num == 1)
         {
            msg_print("The recharge backfires, draining the rod further!");
         }
         else /* all rods completely uncharged */
         {
            msg_print("Nothing seems to happen");
         }

         /* Hack -- decharge the rod */

         i_ptr->p1val = min( i_ptr->number * get_rod_charge(i_ptr), i_ptr->p1val + get_rod_charge(i_ptr));
      }

      /* Recharge */
      else
      {
         /* Rechange amount */
         t = (num * damroll(2, 4));

         /* fully recharge one rod */
         i_ptr->p1val = max(0, i_ptr->p1val - get_rod_charge(i_ptr));
      }
   }

   /* Recharge wand/staff */
   else
   {
      /* Recharge power */
      i = (total_chance - lev - ((10 * i_ptr->p1val) / i_ptr->number)) / 15;

      /* now for the penalty for large stacks: */
      if ((i_ptr->number * lev) > 200 )
      {
         i = i - (s16b)(((s32b)i_ptr->number * (s32b)i_ptr->number * (s32b)lev * (s32b)lev) / 40000L);
      }

      /* Paranoia -- prevent crashes */
      if (i < 1) i = 1;

      /* Back-fire */
      if (rand_int(i) == 0)
      {
         msg_print("There is a bright flash of light.");

         /* if we have 5, and destroy one, we keep 80% of the charges */
         i_ptr->p1val = ((i_ptr->number - 1) * i_ptr->p1val ) / i_ptr->number;

         /* Reduce and describe inventory */
         item_increase(item, -1, px, py);
         item_describe(item, px, py);
         item_optimize(item, px, py);
      }

      /* Recharge */
      else
      {
         /* Extract a "power" */
         t = (num / (lev + 2)) + 1;

         /* Recharge based on the power */
         if (t > 0) i_ptr->p1val += 2 + randint(t);

         /* Hack -- we no longer "know" the item */
         i_ptr->ident &= ~ID_KNOWN;

         /* Hack -- we no longer think the item is empty */
         i_ptr->ident &= ~ID_EMPTY;
      }
   }

   /* Window stuff */
   p_ptr->window |= (PW_INVEN);

   /* Combine / Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Something was done */
   return (TRUE);
}

/*
 * Apply a "project()" directly to all viewable monsters
 */
static bool project_hack(s16b typ, s16b dam)
{
   s16b              i, x, y;
   s16b              flg = PROJECT_JUMP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_HIDE;
   bool             obvious = FALSE;
   project_who_type who;

   who.type = WHO_PLAYER;

   /* Affect all (nearby) monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Location */
      y = m_ptr->fy;
      x = m_ptr->fx;

      /* Require line of sight */
      if (!player_has_los_bold(x, y)) continue;

      /* Jump directly to the target monster */
      if (project(&who, 0, x, y, dam, typ, flg)) obvious = TRUE;
   }

   /* Result */
   return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
   return (project_hack(GF_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
   return (project_hack(GF_SLOW, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters_extra(s16b level)
{
   return (project_hack(GF_SLOW, level));
}


/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
   return (project_hack(GF_SLEEP, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters_extra(s16b level)
{
   return (project_hack(GF_SLEEP, level));
}



/*
 * Banish evil monsters
 */
bool banish_evil(s16b dist)
{
   return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
   return (project_hack(GF_TURN_UNDEAD, p_ptr->lev));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(s16b dam)
{
   return (project_hack(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(s16b dam)
{
   return (project_hack(GF_DISP_EVIL, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(s16b dam)
{
   return (project_hack(GF_DISP_ALL, dam));
}

/*
 * Wake up all monsters, and speed up "los" monsters if how_much == -1
 * If how_much != -1, it is meant as a measure of how much monsters
 * wake up because their neighbour 'screams in pain' etc.
 *
 * XXX XXX note that to be fair, this routine should center on the screaming
 * monster, not on the player. The difference is small, however.
 */
bool aggravate_monsters(project_who_type *who, s16b how_much)
{
   s16b i;

   bool sleep = FALSE;
   bool speed = FALSE;
   bool invisible_wokenup = FALSE;

   /* Aggravate everyone nearby */
   for (i = 1; i < mn_max; i++)
   {
      monster_type   *m_ptr = &mn_list[i];
      monster_race   *r_ptr = &r_info[m_ptr->r_idx];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Skip aggravating monster (or player) */
      if ((who->type == WHO_PLAYER) && (i == 0)) continue;
      if ((who->type == WHO_TRAPBYPLAYER) && (i == 0)) continue;
      if ((who->type == WHO_MONSTER) && (i == who->index)) continue;
      if ((who->type == WHO_TRAPBYMONSTER) && (i == who->index)) continue;

      /* Wake up nearby sleeping monsters */
      if (m_ptr->cdis < MAX_SIGHT * 2)
      {
         /* Wake up */
         if (how_much == -1)
         {
            if (m_ptr->csleep)
            {
               /* Wake up */
               m_ptr->csleep = 0;
               sleep = TRUE;
            }
         }
         else
         {
            if (m_ptr->csleep)
            {
               monster_type *m_ptr2 = &mn_list[who->index];

               if (distance(m_ptr->fx, m_ptr->fy, m_ptr2->fx, m_ptr2->fy) == 1) how_much *= 10;

               m_ptr->csleep -= how_much;

               /* Wake up */
               if (m_ptr->csleep <= 0)
               {
                  char m_name[80];

                  /* Acquire the monster name */
                  monster_desc(m_name, m_ptr, 0);

                  m_ptr->csleep = 0;
                  if (m_ptr->ml)
                  {
                     msg_format("%s wakes up.", m_name);
                  }
                  else
                  {
                     invisible_wokenup = TRUE;
                  } 
               }
               sleep = TRUE;
            }
         }   
      }

      /* Speed up monsters in line of sight */
      if (player_has_los_bold(m_ptr->fx, m_ptr->fy) && (how_much == -1))
      {
         /* Speed up (instantly) to racial base + 10 */
         if (m_ptr->mspeed < r_ptr->speed + 10)
         {
            /* Speed up */
            m_ptr->mspeed = r_ptr->speed + 10;
            speed = TRUE;
         }
      }
   }

   /* Messages */
   if (how_much == -1)
   {
      if (speed)
      {
         msg_print("You feel a sudden stirring nearby!");
      }
      else if (sleep)
      {
         msg_print("You hear a sudden stirring in the distance!");
      }
   }
   else
   {
      if (invisible_wokenup == TRUE)
      {
         msg_print("You hear noises from nearby.");
      }
   }
   return (sleep|speed); /* did something happen? */
}



/*
 * Delete all non-unique monsters of a given "type" from the level
 * jk - no way - only from sight.
 */
bool genocide(void)
{
   s16b      i;

   char     typ;

   bool     result = FALSE;

   /* Mega-Hack -- Get a monster symbol */
   (void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

   /* Delete the monsters of that "type" */
   for (i = 1; i < mn_max; i++)
   {
      monster_type   *m_ptr = &mn_list[i];
      monster_race   *r_ptr = &r_info[m_ptr->r_idx];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Hack -- Skip Unique Monsters */
      if (r_ptr->flags1 & RF1_UNIQUE) continue;

      /* Skip "wrong" monsters */
      if (r_ptr->d_char != typ) continue;

      /* wipe all the items the monster has */
      if (m_ptr->has_drop)
      {
         s16b is_idx, j, number;
         object_type *i_ptr;

         is_idx = item_set_this_monster(i);
         number = items_in_set(is_idx);
         for (j=0;j<number;j++)
         {
            i_ptr = &i_list[is_list[is_idx].index[j]];
            /* preserve any artifacts */
            if (i_ptr->name1)
            {
               a_info[i_ptr->name1].cur_num = 0;
            }
            invwipe(&i_list[is_list[is_idx].index[j]]);
         }
         is_list[is_idx].inuse = FALSE;
         is_list[is_idx].x = 0;
         is_list[is_idx].y = 0;
         is_list[is_idx].z = 0;
         /* this is probably paranoia - can delete_monster_idx fail? */
         m_ptr->has_drop = FALSE;
      }

      /* Delete the monster */
      delete_monster_idx(i);

      /* Take damage */
      take_hit(randint(r_ptr->level/10), "the strain of casting Genocide");

      /* Visual feedback */
      move_cursor_relative(px, py);
      p_ptr->redraw1 |= (PR1_HP);
      handle_stuff();
      Term_fresh();
      delay(20 * delay_spd);

      /* Take note */
      result = TRUE;
   }

   return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(void)
{
   s16b      i;

   bool     result = FALSE;

   /* Delete the (nearby) monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type   *m_ptr = &mn_list[i];
      monster_race   *r_ptr = &r_info[m_ptr->r_idx];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Hack -- Skip unique monsters */
      if (r_ptr->flags1 & RF1_UNIQUE) continue;

      /* Skip distant monsters */
      if (m_ptr->cdis > MAX_SIGHT) continue;

      /* wipe all the items the monster has */
      if (m_ptr->has_drop)
      {
         s16b is_idx, j, number;
         object_type *i_ptr;

         is_idx = item_set_this_monster(i);
         number = items_in_set(is_idx);
         for (j=0;j<number;j++)
         {
            i_ptr = &i_list[is_list[is_idx].index[j]];
            /* preserve any artifacts */
            if (i_ptr->name1)
            {
               a_info[i_ptr->name1].cur_num = 0;
            }
            invwipe(&i_list[is_list[is_idx].index[j]]);
         }
         is_list[is_idx].inuse = FALSE;
         is_list[is_idx].x = 0;
         is_list[is_idx].y = 0;
         is_list[is_idx].z = 0;
         /* this is probably paranoia - can delete_monster_idx fail? */
         m_ptr->has_drop = FALSE;
      }

      /* Delete the monster */
      delete_monster_idx(i);

      /* Hack -- visual feedback */
      
      take_hit(damroll(4,8), "the strain of casting Mass Genocide");
      move_cursor_relative(px, py);
      p_ptr->redraw1 |= (PR1_HP);
      handle_stuff();
      Term_fresh();
      delay(20 * delay_spd);

      /* Note effect */
      result = TRUE;
   }

   return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
   s16b        i;

   bool     probe = FALSE;

   /* Probe all (nearby) monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Require line of sight */
      if (!player_has_los_bold(m_ptr->fx, m_ptr->fy)) continue;

      /* Probe visible monsters */
      if (m_ptr->ml)
      {
         char m_name[80];

         /* Start the message */
         if (!probe) msg_print("Probing...");

         /* Get "the monster" or "something" */
         monster_desc(m_name, m_ptr, 0x04);

         /* Describe the monster */
         msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

         /* Learn all of the non-spell, non-treasure flags */
         lore_do_probe(i);

         /* Probe worked */
         probe = TRUE;
      }
   }

   /* Done */
   if (probe)
   {
      msg_print("That's all.");
   }

   /* Result */
   return (probe);
}

/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(s16b x1, s16b y1, s16b r, bool full)
{
   s16b y, x, k;

   cave_cell_type *c_ptr;

   bool flag = FALSE;


   /* XXX XXX */
   full = full ? full : 0;

   /* Big area of affect */
   for (y = (y1 - r); y <= (y1 + r); y++)
   {
      for (x = (x1 - r); x <= (x1 + r); x++)
      {
         /* Skip illegal grids */
         if (!in_bounds(x, y)) continue;

         /* Extract the distance */
         k = distance(x1, y1, x, y);

         /* Stay in the circle of death */
         if (k > r) continue;

         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][y][x];

         /* Lose room and vault */
         c_ptr->fdat &= ~(CAVE_ROOM | CAVE_VAULT);

         /* Lose light and knowledge */
         c_ptr->fdat &= ~(CAVE_MARK | CAVE_GLOW);

         /* Hack -- Notice player affect */
         if ((x == px) && (y == py))
         {
            /* Hurt the player later */
            flag = TRUE;

            /* Do not hurt this grid */
            continue;
         }

         /* Hack -- Skip the epicenter */
         if ((y == y1) && (x == x1)) continue;

         /* Delete the monster (if any) */
         delete_monster(x, y);

         /* Destroy "valid" grids */
         if (valid_grid(x, y))
         {

            /* Delete the object (if any) */
            delete_object(x, y, -1);

            if (rand_int(2)==1)
               place_wall(x, y);
            else
            {
               /* Clear previous contents, add floor */
               (void)set_grid_type(x, y, DUNG_FLOOR,
                              DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }
         }
      }
   }

   /* Hack -- Affect player */
   if (flag)
   {
      /* Message */
      msg_print("There is a searing blast of light!");

      /* Blind the player */
      if (!p_ptr->resist_blind && !p_ptr->resist_lite)
      {
         /* Become blind */
         (void)set_blind(p_ptr->blind + 10 + randint(10));
      }
   }

   /* Mega-Hack -- Forget the view and lite */
   p_ptr->update |= PU_UN_VIEW;

   /* Update stuff */
   p_ptr->update |= (PU_VIEW | PU_FLOW);

   /* Update the monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that thus the player and monsters (except eaters of walls and
 * passers through walls) will never occupy the same grid as a wall.
 * Note that as of now (2.7.8) no monster may occupy a "wall" grid, even
 * for a single turn, unless that monster can pass_walls or kill_walls.
 * This has allowed massive simplification of the "monster" code.
 */
void earthquake(s16b cx, s16b cy, s16b r)
{
   s16b      i, x, y, xx, yy, dx, dy, ox, oy;

   s16b      damage = 0;

   s16b      sn = 0, sy = 0, sx = 0;

   bool     hurt = FALSE;

   cave_cell_type   *c_ptr;

   bool     map[32][32];


   /* Paranoia -- Enforce maximum range */
   if (r > 12) r = 12;

   /* Clear the "maximal blast" area */
   for (x = 0; x < 32; x++)
   {
      for (y = 0; y < 32; y++)
      {
         map[x][y] = FALSE;
      }
   }
dlog(DEBUGTRAPS,"spells2.c: earthquake: step 1 r %d\n", r);

   /* Check around the epicenter */
   for (dy = -r; dy <= r; dy++)
   {
      for (dx = -r; dx <= r; dx++)
      {
         /* Extract the location */
         xx = cx + dx;
         yy = cy + dy;
dlog(DEBUGTRAPS,"spells2.c: earthquake: center %d,%d rel %d,%d real %d,%d testing:\n",
                cx, cy, dx, dy, xx, yy);

dlog(DEBUGTRAPS,"spells2.c: earthquake: %d,%d max x,y %d,%d in_bounds %d\n",
                xx, yy, cur_hgt-1, cur_wid-1, in_bounds(xx, yy));

         /* Skip illegal grids */
         if (!in_bounds(xx, yy)) continue;

dlog(DEBUGTRAPS,"spells2.c: earthquake: in bounds\n");
         /* Skip distant grids */
         if (distance(cx, cy, xx, yy) > r) continue;

dlog(DEBUGTRAPS,"spells2.c: earthquake: close\n");

         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][yy][xx];

         /* Lose room and vault */
         c_ptr->fdat &= ~(CAVE_ROOM | CAVE_VAULT);

         /* Lose light and knowledge */
         c_ptr->fdat &= ~(CAVE_GLOW | CAVE_MARK);

         /* Skip the epicenter */
         if (!dx && !dy) continue;
dlog(DEBUGTRAPS,"spells2.c: earthquake: not center\n");

         /* Skip most grids */
         if (rand_int(100) < 85) continue;
dlog(DEBUGTRAPS,"spells2.c: earthquake: randomness done\n");

         /* Damage this grid */
         map[16+dx][16+dy] = TRUE;
dlog(DEBUGTRAPS,"spells2.c: earthquake: center %d,%d rel %d,%d real %d,%d damaged\n",
                cx, cy, dx, dy, xx, yy);
         /* Hack -- Take note of player damage */
         if ((yy == py) && (xx == px)) hurt = TRUE;
      }
   }
dlog(DEBUGTRAPS,"spells2.c: earthquake: step 2, hurt %d\n", hurt);

   /* First, affect the player (if necessary) */
   if (hurt)
   {
      /* Check around the player */
      for (i = 0; i < 8; i++)
      {
         /* Access the location */
         x = px + ddx[i];
         y = py + ddy[i];

         /* Skip non-empty grids */
         if (!empty_grid_bold(x, y)) continue;

         /* Important -- Skip "quake" grids         */
         /* but only if it is in range of the quake */
         if ( (distance(cx, cy, x, y) <= r) &&
              (map[16+(x-cx)][16+(y-cy)]) )
         {
            continue;
         }

         /* Count "safe" grids */
         sn++;

         /* Randomize choice */
         if (rand_int(sn) > 0) continue;

         /* Save the safe location */
         sx = x; sy = y;
      }

      /* Random message */
      switch (randint(3))
      {
         case 1:
            msg_print("The cave ceiling collapses!");
            break;
         case 2:
            msg_print("The cave floor twists in an unnatural way!");
            break;
         default:
            msg_print("The cave quakes!  You are pummeled with debris!");
            break;
      }

      /* Hurt the player a lot */
      if (!sn)
      {
         /* Message and damage */
         msg_print("You are severely crushed!");
         damage = 300;
      }

      /* Destroy the grid, and push the player to safety */
      else
      {
         /* Calculate results */
         switch (randint(3))
         {
            case 1:
               msg_print("You nimbly dodge the blast!");
               damage = 0;
               break;
            case 2:
               msg_print("You are bashed by rubble!");
               damage = damroll(10, 4);
               (void)set_stun(p_ptr->stun + randint(50));
               break;
            case 3:
               msg_print("You are crushed between the floor and ceiling!");
               damage = damroll(10, 4);
               (void)set_stun(p_ptr->stun + randint(50));
               break;
         }

         /* Save the old location */
         oy = py;
         ox = px;

         /* Move the player to the safe location */
         py = sy;
         px = sx;

         /* Redraw the old spot */
         lite_spot(ox, oy);

         /* Redraw the new spot */
         lite_spot(px, py);

         /* Check for new panel */
         verify_panel();
      }

      /* Important -- no wall on player */
      map[16+px-cx][16+py-cy] = FALSE;

      /* Take some damage */
      if (damage) take_hit(damage, "an earthquake");
   }

dlog(DEBUGTRAPS,"spells2.c: earthquake: step 3\n");

   /* Examine the quaked region */
   for (dy = -r; dy <= r; dy++)
   {
      for (dx = -r; dx <= r; dx++)
      {
         /* Extract the location */
         xx = cx + dx;
         yy = cy + dy;

         /* Skip unaffected grids */
         if (!map[16+xx-cx][16+yy-cy]) continue;

         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][yy][xx];

         /* Process monsters */
         if (c_ptr->m_idx)
         {
            monster_type *m_ptr = &mn_list[c_ptr->m_idx];
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Most monsters cannot co-exist with rock */
            if (!(r_ptr->flags2 & RF2_KILL_WALL) &&
               !(r_ptr->flags2 & RF2_PASS_WALL))
            {
               char m_name[80];

               /* Assume not safe */
               sn = 0;

               /* Monster can move to escape the wall */
               if (!(r_ptr->flags1 & RF1_NEVER_MOVE))
               {
                  /* Look for safety */
                  for (i = 0; i < 8; i++)
                  {
                     /* Access the grid */
                     y = yy + ddy[i];
                     x = xx + ddx[i];

                     /* Skip non-empty grids */
                     if (!empty_grid_bold(x, y)) continue;

                     /* Hack -- no safety on glyph of warding */
                     if (test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_GLYPH))
                          continue;

                     /* Important -- Skip "quake" grids */
                     if (map[16+x-cx][16+y-cy]) continue;

                     /* Count "safe" grids */
                     sn++;

                     /* Randomize choice */
                     if (rand_int(sn) > 0) continue;

                     /* Save the safe grid */
                     sy = y; sx = x;
                  }
               }

               /* Describe the monster */
               monster_desc(m_name, m_ptr, 0);

               /* Scream in pain */
               msg_format("%^s wails out in pain!", m_name);

               /* Take damage from the quake */
               damage = (sn ? damroll(4, 8) : 200);

               /* Monster is certainly awake */
               m_ptr->csleep = 0;

               /* Apply damage directly */
               m_ptr->hp -= damage;

               /* Delete (not kill) "dead" monsters */
               if (m_ptr->hp < 0)
               {
                  /* Message */
                  msg_format("%^s is embedded in the rock!", m_name);

                  /* Delete the monster */
                  delete_monster(xx, yy);

                  /* No longer safe */
                  sn = 0;
               }

               /* Hack -- Escape from the rock */
               if (sn)
               {
                  s16b m_idx = dungeon.level[sublevel][yy][xx].m_idx;

                  /* Update the new location */
                  dungeon.level[sublevel][sy][sx].m_idx = m_idx;

                  /* Update the old location */
                  dungeon.level[sublevel][yy][xx].m_idx = 0;

                  /* Move the monster */
                  m_ptr->fx = sx;
                  m_ptr->fy = sy;
                  /* stay on this level, fz doesn't change */

                  /* Update the monster (new location) */
                  update_mon(m_idx, TRUE);

                  /* Redraw the old grid */
                  lite_spot(xx, yy);

                  /* Redraw the new grid */
                  lite_spot(sx, sy);
               }
            }
         }
      }
   }
dlog(DEBUGTRAPS,"spells2.c: earthquake: step 4\n");

   /* Examine the quaked region */
   for (dy = -r; dy <= r; dy++)
   {
      for (dx = -r; dx <= r; dx++)
      {
         /* Extract the location */
         xx = cx + dx;
         yy = cy + dy;
         /* Skip unaffected grids */
         if (!map[16+xx-cx][16+yy-cy]) continue;

         /* Access the cave grid */
         c_ptr = &dungeon.level[sublevel][yy][xx];

         /* Paranoia -- never affect player */
         if ((yy == py) && (xx == px)) continue;

         /* Destroy location (if valid) */
         if (valid_grid(xx, yy))
         {
            bool floor;
            floor = floor_grid_bold(xx, yy);

            /* Delete any object that is still there */
            delete_object(xx, yy, -1);

            if (floor==TRUE)
            {
               place_wall(xx, yy);

            }
            else
            {
               /* Clear previous contents, add floor */
               (void)set_grid_type(xx, yy, DUNG_FLOOR,
                              DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }
         }
      }
   }

   /* Mega-Hack -- Forget the view and lite */
   p_ptr->update |= PU_UN_VIEW;

   /* Update stuff */
   p_ptr->update |= (PU_VIEW | PU_FLOW);

   /* Update the monsters */
   p_ptr->update |= (PU_DISTANCE);

   /* Update the health bar */
   p_ptr->redraw1 |= (PR1_HEALTH);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
   s16b i;

   /* Clear them all */
   for (i = 0; i < temp_n; i++)
   {
      s16b x = temp_x[i];
      s16b y = temp_y[i];

      cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

      /* No longer in the array */
      c_ptr->fdat &= ~CAVE_TEMP;

      /* Update only non-CAVE_GLOW grids */
      /* if (c_ptr->fdat & CAVE_GLOW) continue; */

      /* Perma-Lite */
      c_ptr->fdat |= CAVE_GLOW;

      /* Process affected monsters */
      if (c_ptr->m_idx)
      {
         monster_type      *m_ptr = &mn_list[c_ptr->m_idx];

         monster_race      *r_ptr = &r_info[m_ptr->r_idx];

         /* Update the monster */
         update_mon(c_ptr->m_idx, FALSE);

         /* Sometimes monsters wake up */
         if (m_ptr->csleep &&
            (((r_ptr->flags2 & RF2_STUPID) && (rand_int(100) < 10)) ||
             (rand_int(100) < 75) ||
             (r_ptr->flags2 & RF2_SMART)))
         {
            /* Wake up! */
            m_ptr->csleep = 0;

            /* Notice the "waking up" */
            if (m_ptr->ml)
            {
               char m_name[80];

               /* Acquire the monster name */
               monster_desc(m_name, m_ptr, 0);

               /* Dump a message */
               msg_format("%^s wakes up.", m_name);
            }
         }
      }

      /* Note */
      note_spot(x, y);

      /* Redraw */
      lite_spot(x, y);
   }

   /* None left */
   temp_n = 0;

   /* Mega-Hack -- Forget the view and lite */
   p_ptr->update |= PU_UN_VIEW;

   /* Update stuff */
   p_ptr->update |= (PU_VIEW | PU_FLOW);

   /* Update the monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
 *
 * Also, process all affected monsters
 */
static void cave_temp_room_unlite(void)
{
   s16b i;

   /* Clear them all */
   for (i = 0; i < temp_n; i++)
   {
      s16b y = temp_y[i];
      s16b x = temp_x[i];

      cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

      /* No longer in the array */
      c_ptr->fdat &= ~CAVE_TEMP;

      /* Darken the grid */
      c_ptr->fdat &= ~CAVE_GLOW;

      /* Hack -- Forget "boring" grids */
      if ( (c_ptr->mtyp == DUNG_FLOOR) &&
          ( (c_ptr->styp == DUNG_FLOOR_NORMAL) ||
            (c_ptr->styp == DUNG_FLOOR_TRAP) ) )
      {
         /* Forget the grid */
         c_ptr->fdat &= ~CAVE_MARK;

         /* Notice */
         note_spot(x, y);
      }

      /* Process affected monsters */
      if (c_ptr->m_idx)
      {
         /* Update the monster */
         update_mon(c_ptr->m_idx, FALSE);
      }

      /* Redraw */
      lite_spot(x, y);
   }

   /* None left */
   temp_n = 0;
}

/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(s16b x, s16b y, bool room)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

   /* Avoid infinite recursion */
   if (c_ptr->fdat & CAVE_TEMP) return;

   /* Do not "leave" the current room */
   if (!(c_ptr->fdat & CAVE_ROOM) && (room == TRUE) && !is_wall(x, y)) return;
   if ((room == FALSE) && (distance(px, py, x, y) > 6)) return;

   /* Paranoia -- verify space */
   if (temp_n == TEMP_MAX) return;

   /* Mark the grid as "seen" */
   c_ptr->fdat |= CAVE_TEMP;

   /* Add it to the "seen" set */
   temp_x[temp_n] = x;
   temp_y[temp_n] = y;
   temp_n++;
}

/*
 * Illuminate any room containing the given location.
 */
void lite_room(s16b x1, s16b y1)
{
   s16b i, x, y;
   bool in_room = FALSE;

   if (dungeon.level[sublevel][y1][x1].fdat & CAVE_ROOM)
   {
      in_room = TRUE;
   }

   /* Add the initial grid */
   cave_temp_room_aux(x1, y1, in_room);

   /* While grids are in the queue, add their neighbors */
   for (i = 0; i < temp_n; i++)
   {
      x = temp_x[i], y = temp_y[i];

      /* Walls get lit, but stop light */
      if (!floor_grid_bold(x, y))
      {
         continue;
      }

      /* Spread adjacent */
      cave_temp_room_aux(x, y + 1, in_room);
      cave_temp_room_aux(x, y - 1, in_room);
      cave_temp_room_aux(x + 1, y, in_room);
      cave_temp_room_aux(x - 1, y, in_room);

      /* Spread diagonal */
      cave_temp_room_aux(x + 1, y + 1, in_room);
      cave_temp_room_aux(x - 1, y - 1, in_room);
      cave_temp_room_aux(x + 1, y - 1, in_room);
      cave_temp_room_aux(x - 1, y + 1, in_room);
   }

   /* Now, lite them all up at once */
   cave_temp_room_lite();

   /* Update the view & the monsters */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(s16b x1, s16b y1)
{
   s16b i, x, y;
   bool in_room;

   in_room =  (dungeon.level[sublevel][y1][x1].fdat & CAVE_ROOM);

   /* Add the initial grid */
   cave_temp_room_aux(x1, y1, in_room);

   /* Spread, breadth first */
   for (i = 0; i < temp_n; i++)
   {
      x = temp_x[i], y = temp_y[i];

      /* Walls get dark, but stop darkness */
      if (!floor_grid_bold(x, y)) continue;

      /* Spread adjacent */
      cave_temp_room_aux(x, y + 1, in_room);
      cave_temp_room_aux(x, y - 1, in_room);
      cave_temp_room_aux(x + 1, y, in_room);
      cave_temp_room_aux(x - 1, y, in_room);

      /* Spread diagonal */
      cave_temp_room_aux(x + 1, y + 1, in_room);
      cave_temp_room_aux(x - 1, y - 1, in_room);
      cave_temp_room_aux(x + 1, y - 1, in_room);
      cave_temp_room_aux(x - 1, y + 1, in_room);
   }

   /* Now, darken them all at once */
   cave_temp_room_unlite();

   /* Update the view & the monsters */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(s16b dam, s16b rad)
{
   s16b flg = PROJECT_GRID | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   project_who_type who;
   
   /* Hack -- Message */
   if (!p_ptr->blind)
   {
      msg_print("You are surrounded by a white light.");
   }

   who.type = WHO_PLAYER;

   /* Hook into the "project()" function */
   (void)project(&who, rad, px, py, dam, GF_LITE_WEAK, flg);

   /* Lite up the room */
   lite_room(px, py);

   /* Update the view & the monsters */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);

   /* Assume seen */
   return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(s16b dam, s16b rad)
{
   s16b flg = PROJECT_GRID | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   project_who_type who;

   /* Hack -- Message */
   if (!p_ptr->blind)
   {
      msg_print("Darkness surrounds you.");
   }

   who.type = WHO_PLAYER;

   /* Hook into the "project()" function */
   (void)project(&who, rad, px, py, dam, GF_DARK_WEAK, flg);

   /* Lite up the room */
   unlite_room(px, py);

   /* Assume seen */
   return (TRUE);
}



/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(s16b typ, s16b dir, s16b dam, s16b rad)
{
   s16b tx, ty;
   s16b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   project_who_type who;
 
   /* Use the given direction */
   tx = px + 99 * ddx[dir];
   ty = py + 99 * ddy[dir];

   /* Hack -- Use an actual "target" */
   if ((dir == 5) && target_okay())
   {
      flg &= ~PROJECT_STOP;
      tx = target_col;
      ty = target_row;
   }

   /* Analyze the "dir" and the "target". Hurt items on floor. */
dlog(DEBUGPROJ, "spells2.c: fire_ball: typ %d dir %d dam %d rad %d flg %016x\n",
            typ, dir, dam, rad, flg);
   who.type = WHO_PLAYER;
   return (project(&who, rad, tx, ty, dam, typ, flg));
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(s16b typ, s16b dir, s16b dam, s16b flg)
{
   s16b tx, ty;
   project_who_type who;

   /* Pass through the target if needed */
   flg |= (PROJECT_THRU);

   /* Use the given direction */
   tx = px + ddx[dir];
   ty = py + ddy[dir];

   /* Hack -- Use an actual "target" */
   if ((dir == 5) && target_okay())
   {
      tx = target_col;
      ty = target_row;
   }

   /* Analyze the "dir" and the "target", do NOT explode */
dlog(DEBUGPROJ,"spells2.c: project_hook: about to call project with tx,ty %d,%d dam %d typ %d flg %d\n",
     tx,ty,dam,typ,flg);
   who.type = WHO_PLAYER;
   return (project(&who, 0, tx, ty, dam, typ, flg));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(s16b typ, s16b dir, s16b dam)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(typ, dir, dam, flg));
}

/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(s16b typ, s16b dir, s16b dam)
{
   s16b flg = PROJECT_BEAM | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(typ, dir, dam, flg));
}

/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(s16b prob, s16b typ, s16b dir, s16b dam)
{
dlog(DEBUGPROJ,"spells2.c: fire_bolt_or_beam: prob %d typ %d dir %d dam %d\n",
     prob, typ, dir, dam);
   if (rand_int(100) < prob)
   {
dlog(DEBUGPROJ,"spells2.c: fire_bolt_or_beam: beam\n");
      return (fire_beam(typ, dir, dam));
   }
   else
   {
dlog(DEBUGPROJ,"spells2.c: fire_bolt_or_beam: bolt\n");
      return (fire_bolt(typ, dir, dam));
   }
}


/*
 * Some of the old functions
 */

bool lite_line(s16b dir)
{
   s16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   p_ptr->update |= (PU_VIEW);
   return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}

bool drain_life(s16b dir, s16b dam)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_DRAIN, dir, dam, flg));
}

bool wall_to_mud(s16b dir)
{
   s16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), flg));
}

bool destroy_door(s16b dir)
{
   s16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
   return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}

bool disarm_trap(s16b dir)
{
   s16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
   return (project_hook(GF_KILL_TRAP, dir, 0, flg));
}

bool heal_monster(s16b dir)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_HEAL, dir, damroll(4, 6), flg));
}

bool speed_monster(s16b dir)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_SPEED, dir, p_ptr->lev, flg));
}

bool slow_monster(s16b dir)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_SLOW, dir, p_ptr->lev, flg));
}

bool sleep_monster(s16b dir)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_SLEEP, dir, p_ptr->lev, flg));
}

bool confuse_monster(s16b dir, s16b plev)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_CONF, dir, plev, flg));
}

bool poly_monster(s16b dir)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_POLY, dir, p_ptr->lev, flg));
}

bool clone_monster(s16b dir)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_CLONE, dir, 0, flg));
}

bool fear_monster(s16b dir, s16b plev)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_TURN_ALL, dir, plev, flg));
}

bool teleport_monster(s16b dir)
{
   s16b flg = PROJECT_BEAM | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg));
}

/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation()
{
   s16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
   project_who_type who;
   who.type = WHO_PLAYER;
   return (project(&who, 1, px, py, 0, GF_MAKE_DOOR, flg));
}

bool trap_creation(project_who_type *who, s16b x, s16b y)
{
   s16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
   return (project(who, 1, x, y, 0, GF_MAKE_TRAP, flg));
}

bool destroy_doors_touch()
{
   s16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
   project_who_type who;
   who.type = WHO_PLAYER;
   return (project(&who, 1, px, py, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(void)
{
   s16b flg = PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_HIDE;
   project_who_type who;
   who.type = WHO_PLAYER;
   return (project(&who, 1, px, py, p_ptr->lev, GF_SLEEP, flg));
}


/* JK */
bool ident_trap(s16b dir)
{
   s16b           x = px;
   s16b           y = py;
   s16b           i, max, trap = 0, item = -1;
   cave_cell_type      *c_ptr;
   trap_item_type *tr_ptr = NULL;
   object_type    *i_ptr;
   bool           obvious = FALSE;
   bool           found;

   max=(p_ptr->lev/5)+3;

   for (i=0; i<=max; i++)
   {
      x+=ddx[dir];
      y+=ddy[dir];
      found = FALSE;
      if (!in_bounds(x,y)) break;       /* don't look outside cave  */
      if (!floor_grid_bold(x,y)) break; /* don't look through walls */
      c_ptr = &dungeon.level[sublevel][y][x];

      /* unknown traps don't get noted here */
      if ( (c_ptr->mtyp == DUNG_TRAP) &&
           (c_ptr->styp == DUNG_TRAP_FNDONE))
      {
         tr_ptr = &t_list[c_ptr->t_idx];
         trap = get_random_trap(tr_ptr, TRAP_FOUND);
         found = TRUE;
      }

      else if (c_ptr->mtyp == DUNG_DOOR)
      {
         if (c_ptr->t_idx)
         {
            tr_ptr = &t_list[c_ptr->t_idx];
            trap = get_random_trap(tr_ptr, TRAP_FOUND);
            found=TRUE;
         }
      }

      else if (objects_on_floor(x, y)>0)
      {
         /* test for trapped and known chests */
         for (i=0; i<objects_on_floor(x,y); i++)
         {
            /* Get the object (if any) */
            i_ptr = get_item_pointer_floor_xy(i,x,y);
            /* don't count empty chests if we know them */
            if ((i_ptr->tval==TV_CHEST) &&
                (object_known_p(i_ptr)) && (i_ptr->p1val>0))
            {
               tr_ptr = &t_list[i_ptr->xtra2];
               trap = get_random_trap(tr_ptr, TRAP_FOUND);
               found=TRUE;
               item = i;
               break;
            }
         }
      }

      if (found)
      {
         trap_type *t_ptr = &t_info[trap];
         if (t_ptr->flags & TRAP_IDENTIFIED)
         {
            msg_print("You have already studied this trap.");
            return (TRUE);
         }
      }

      if (found)
      {
         trap_type       *t_ptr = &t_info[trap];

         if (item==-1)     /* is it on the floor */
         {
            note_spot(x,y); /* at least, we now know it's there */
            lite_spot(x,y);
         }

         set_trap_found_ptr(tr_ptr, trap);

         /* guess a trap identity, better if higher level character */
         /*                        better if higher s16b */
         /*                        better if easy trap */
         if (randint(100- (p_ptr->lev/3)- (adj_mag_stat[p_ptr->stat_ind[A_INT]])) <t_ptr->difficulty)
         /* gain some knowledge */
         /* gain knowledge about easy traps quickly */
         /* gain knowledge about difficult traps slower */
         /* higher s16b helps */
         {
            s16b value     = ( (32768-t_ptr->known)/1000* /* anywhere between 32 and 1 */
                              (12-t_ptr->difficulty)*    /* anywhere between 2 and 11 */
                              (adj_mag_stat[p_ptr->stat_ind[A_INT]])); /* anywhere between 1 and 20 */
                              /* result: += anywhere between 2 and 1200 */
            if (value<100)
            {
               msg_format("You suddenly get an idea about the %s.",t_name+t_ptr->name);
            }
            else if (value<300)
            {
               msg_format("You suddenly get a bright idea about the %s.",t_name+t_ptr->name);
            }
            else if (value<500)
            {
               msg_format("You suddenly get a luminous idea about the %s.",t_name+t_ptr->name);
            }
            else if (value<900)
            {
               msg_format("You suddenly get an insight into the working of the %s.",t_name+t_ptr->name);
            }
            else
            {
               msg_format("You feel enlightened about the %s.",t_name+t_ptr->name);
            }
            t_ptr->known+=value;
            t_ptr->flags |= TRAP_IDENTIFIED;
            obvious = TRUE;
         }
         else
         {
            msg_print("Something flashes through your head.");
         }
         break; /* no use further searching */
      } /* if (found) */
   } /* for - next */
   return (obvious);
}

