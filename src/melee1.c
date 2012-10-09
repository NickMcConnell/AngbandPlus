/* File: melee1.c */

/* Purpose: Monster attacks */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static s16b monster_critical(s16b dice, s16b sides, s16b dam)
{
   s16b max = 0;
   s16b total = dice * sides;

   /* Must do at least 95% of perfect */
   if (dam < total * 19 / 20) return (0);

   /* Weak blows rarely work */
   if ((dam < 20) && (rand_int(100) >= dam)) return (0);

   /* Perfect damage */
   if (dam == total) max++;

   /* Super-charge */
   if (dam >= 20)
   {
      while (rand_int(100) < 2) max++;
   }

   /* Critical damage */
   if (dam > 45) return (6 + max);
   if (dam > 33) return (5 + max);
   if (dam > 25) return (4 + max);
   if (dam > 18) return (3 + max);
   if (dam > 11) return (2 + max);
   return (1 + max);
}

/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static s16b check_hit(s16b power, s16b level)
{
   s16b i, k, ac;

   /* Percentile dice */
   k = rand_int(100);

   /* Hack -- Always miss or hit */
   if (k < 10) return (k < 5);

   /* Calculate the "attack quality" */
   i = (power + (level * 3));

   /* Total armor */
   ac = p_ptr->ac + p_ptr->to_a;

   /* Power and Level compete against Armor */
   if ((i > 0) && (randint(i) > ((ac * 3) / 4))) return (TRUE);

   /* Assume miss */
   return (FALSE);
}

/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] =
{
   "insults you!",
   "insults your mother!",
   "gives you the finger!",
   "humiliates you!",
   "defiles you!",
   "dances around you!",
   "makes obscene gestures!",
   "moons you!!!"
};

/*
 * Hack -- possible "insult" messages
 */
static cptr desc_moan[] =
{
   "seems sad about something.",
   "asks if you have seen his dogs.",
   "tells you to get off his land.",
   "mumbles something about mushrooms."
};

/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(s16b m_idx)
{
   monster_type       *m_ptr = &mn_list[m_idx];
   monster_race       *r_ptr = &r_info[m_ptr->r_idx];
   s16b                ap_cnt;
   s16b                i, j, k, tmp, ac, rlev;
   s16b                do_cut, do_stun;
   s32b                gold;
   object_type        *i_ptr;

   char                i_name[80];
   char                m_name[80];
   char                ddesc[80];
   bool                small_blinked, escaping;
   bool                fear = FALSE;
   bool                do_reflect = FALSE;

   /* Not allowed to attack */
   if (r_ptr->flags1 & RF1_NEVER_BLOW) return (FALSE);

   /* Total armor */
   ac = p_ptr->ac + p_ptr->to_a;

   /* Extract the effective monster level */
   rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

   /* Get the monster name (or "it") */
   monster_desc(m_name, m_ptr, 0);

   /* Get the "died from" information (i.e. "a kobold") */
   monster_desc(ddesc, m_ptr, 0x88);

   /* Assume no blink */
   small_blinked = FALSE;
   escaping      = FALSE;

   /* Scan through all four blows */
   for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
   {
      bool visible = FALSE;
      bool obvious = FALSE;

      s16b power = 0;
      s16b damage = 0;

      cptr act = NULL, method_str = NULL;

      /* Extract the attack infomation */
      s16b effect = r_ptr->blow[ap_cnt].effect;
      s16b method = r_ptr->blow[ap_cnt].method;
      s16b d_dice = r_ptr->blow[ap_cnt].d_dice;
      s16b d_side = r_ptr->blow[ap_cnt].d_side;

      /* Hack -- no more attacks */
      if (!method) break;

      /* Stop if player is dead or gone */
      if (!alive || death || new_level_flag) break;

      /* Extract visibility (before blink) */
      if (m_ptr->ml) visible = TRUE;

      /* Extract the attack "power" */
      switch (effect)
      {
         case RBE_HURT:      power = 60; break;
         case RBE_POISON:    power =  5; break;
         case RBE_UN_BONUS:  power = 20; break;
         case RBE_UN_POWER:  power = 15; break;
         case RBE_EAT_GOLD:  power =  5; break;
         case RBE_EAT_ITEM:  power =  5; break;
         case RBE_EAT_FOOD:  power =  5; break;
         case RBE_EAT_LITE:  power =  5; break;
         case RBE_ACID:      power =  0; break;
         case RBE_ELEC:      power = 10; break;
         case RBE_FIRE:      power = 10; break;
         case RBE_COLD:      power = 10; break;
         case RBE_BLIND:     power =  2; break;
         case RBE_CONFUSE:   power = 10; break;
         case RBE_TERRIFY:   power = 10; break;
         case RBE_PARALYZE:  power =  2; break;
         case RBE_LOSE_STR:  power =  0; break;
         case RBE_LOSE_DEX:  power =  0; break;
         case RBE_LOSE_CON:  power =  0; break;
         case RBE_LOSE_INT:  power =  0; break;
         case RBE_LOSE_WIS:  power =  0; break;
         case RBE_LOSE_CHR:  power =  0; break;
         case RBE_LOSE_ALL:  power =  2; break;
         case RBE_SHATTER:   power = 60; break;
         case RBE_EXP_10:    power =  5; break;
         case RBE_EXP_20:    power =  5; break;
         case RBE_EXP_40:    power =  5; break;
         case RBE_EXP_80:    power =  5; break;
      }

      /* Monster hits player */
      if (!effect || check_hit(power, rlev))
      {
         /* Always disturbing */
         disturb(1, 0);

         /* Hack -- Apply "protection from evil" */
         if ((p_ptr->protevil > 0) &&
             (r_ptr->flags3 & RF3_EVIL) &&
             (p_ptr->lev >= rlev) &&
             ((rand_int(100) + p_ptr->lev) > 50))
         {
             /* Remember the Evil-ness */
             if (m_ptr->ml) r_ptr->r_flags3 |= RF3_EVIL;

             /* Message */
             msg_format("%^s is repelled.", m_name);

             /* Hack -- Next attack */
             continue;
         }

         /* Assume no cut or stun */
         do_cut = do_stun = 0;

         /* Describe the attack method */
         switch (method)
         {
             case RBM_HIT:
                 act = "hits you.";
                 method_str = "hit";
                 do_cut = do_stun = 1;
                 break;

             case RBM_TOUCH:
                 act = "touches you.";
                 break;

             case RBM_PUNCH:
                 act = "punches you.";
                 method_str = "punch";
                 do_stun = 1;
                 break;

             case RBM_KICK:
                 act = "kicks you.";
                 method_str = "kick";
                 do_stun = 1;
                 break;

             case RBM_CLAW:
                 act = "claws you.";
                 method_str = "claw";
                 do_cut = 1;
                 break;

             case RBM_BITE:
                 act = "bites you.";
                 method_str = "bite";
                 do_cut = 1;
                 break;

             case RBM_STING:
                 act = "stings you.";
                 method_str = "sting";
                 break;

             case RBM_XXX1:
                 act = "XXX1's you.";
                 break;

             case RBM_BUTT:
                 act = "butts you.";
                 method_str = "butt";
                 do_stun = 1;
                 break;

             case RBM_CRUSH:
                 act = "crushes you.";
                 method_str = "crush";
                 do_stun = 1;
                 break;

             case RBM_ENGULF:
                 act = "engulfs you.";
                 break;

             case RBM_XXX2:
                 act = "XXX2's you.";
                 break;

             case RBM_CRAWL:
                 act = "crawls on you.";
                 method_str = "crawl";
                 break;

             case RBM_DROOL:
                 act = "drools on you.";
                 method_str = "drool";
                 break;

             case RBM_SPIT:
                 act = "spits on you.";
                 method_str = "spit";
                 break;

             case RBM_XXX3:
                 act = "XXX3's on you.";
                 break;

             case RBM_GAZE:
                 act = "gazes at you.";
                 break;

             case RBM_WAIL:
                 act = "wails at you.";
                 break;

             case RBM_SPORE:
                 act = "releases spores at you.";
                 break;

             case RBM_XXX4:
                 act = "projects XXX4's at you.";
                 break;

             case RBM_BEG:
                 act = "begs you for money.";
                 break;

             case RBM_INSULT:
                 act = desc_insult[rand_int(8)];
                 break;

             case RBM_MOAN:
                 act = desc_moan[rand_int(4)];
                 break;

             case RBM_XXX5:
                 act = "XXX5's you.";
                 break;
         }

         if (p_ptr->reflecting && (randint(5)!=1) && (
             (method==RBM_HIT)   || (method==RBM_SPORE) || (method==RBM_PUNCH) ||
             (method==RBM_KICK)  || (method==RBM_CLAW)  || (method==RBM_BITE)  ||
             (method==RBM_STING) || (method==RBM_BUTT)  || (method==RBM_CRUSH) ||
             (method==RBM_CRAWL) || (method==RBM_DROOL) || (method==RBM_SPIT)))
         {

            static cptr reflect_str[] =
                { "stumble","stagger","jerk","convulse","obey an inner need",
                  "fall", "shudder", "twist in pain", "be confused", "be dizzy",
                  "blink", "doze off"
                };
            s16b msgnum = rand_int(12);
            char reflex[80];

            do_reflect = TRUE;

            monster_desc(reflex, m_ptr, 0x23);

            msg_format("%^s tries to %s you, but seems to %s and %s %s.",
                       m_name, method_str, reflect_str[msgnum], method_str, reflex);
         }
         else
         {
            /* Message */
            if (act) msg_format("%^s %s", m_name, act);
         }

         /* Hack -- assume all attacks are obvious */
         obvious = TRUE;

         /* Roll out the damage */
         damage = damroll(d_dice, d_side);

         /* Apply appropriate damage */
         switch (effect)
         {
            case 0:

               /* Hack -- Assume obvious */
               obvious = TRUE;

               /* Hack -- No damage */
               damage = 0;

               break;

            case RBE_HURT:

               /* Obvious */
               obvious = TRUE;

               /* Take damage */
               if (do_reflect)
               {
                  project_who_type who;
                  message_pain(m_idx, damage);
                  who.type = WHO_PLAYER;
                  mon_take_hit(&who, m_idx, damage, &fear, NULL);
               }
               else
               {
                  /* Hack -- Player armor reduces total damage */
                  s16b old_damage = damage;
                  damage -= (damage * ((ac < 150) ? ac : 150) / 250);

                  take_hit(damage, ddesc);
                  if (wizard)
                  {
                     msg_format("%^s does %d (before ac:%d) damage by hurting.", m_name,damage, old_damage);
                  }
               }

               break;

            case RBE_POISON:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by poisoning.", m_name,damage);
               }

               /* Take "poison" effect */
               if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
               {
                  if (set_poisoned(p_ptr->poisoned + randint(rlev) + 5))
                  {
                     obvious = TRUE;
                  }
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_POIS);

               break;

            case RBE_UN_BONUS:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by disenchanting.", m_name,damage);
               }


               /* Allow complete resist */
               if (!p_ptr->resist_disen)
               {
                  /* Apply disenchantment */
                  if (apply_disenchant(0)) obvious = TRUE;
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_DISEN);

               break;

            case RBE_UN_POWER:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by draining.", m_name,damage);
               }

               /* Find an item */
               for (k = 0; k < 10; k++)
               {
                  /* Pick an item */
                  i = rand_int(INVEN_PACK);

                  /* Obtain the item */
                  i_ptr = &inventory[i];

                  /* Drain charged wands/staffs */
                  if (((i_ptr->tval == TV_STAFF) ||
                       (i_ptr->tval == TV_WAND)) &&
                      (i_ptr->p1val))
                  {
                     s16b tmp;

                     /* Message */
                     msg_print("Energy drains from your pack!");

                     /* Obvious */
                     obvious = TRUE;

                     /* Uncharge */
                     i_ptr->p1val = 0;

                     /* Window stuff */
                     p_ptr->window |= (PW_INVEN);

                     /* Combine / Reorder the pack */
                     p_ptr->notice |= (PN_COMBINE | PN_REORDER);

                     /* Heal */
                     j = rlev;
                     tmp = j * i_ptr->p1val * i_ptr->number;
                     m_ptr->hp += tmp;
                     msg_format("%^s gains %d hp by draining.", m_name,tmp);
                     if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

                     /* Redraw (later) if needed */
                     if (health_who == m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

                     /* Done */
                     break;
                  }
               }

               break;

            case RBE_EAT_GOLD:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by eating gold.", m_name,damage);
               }

               /* Obvious */
               obvious = TRUE;

               /* Saving throw (unless paralyzed) based on dex and level */
               if (!p_ptr->paralyzed &&
                   (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
                                     p_ptr->lev)))
               {
                  /* Saving throw message */
                  msg_print("You quickly protect your money pouch!");

                  /* Occasional blink anyway */
                  if (rand_int(3)) small_blinked = TRUE;
               }

               /* Eat gold */
               else
               {
                  gold = (p_ptr->au / 10) + randint(25);
                  if (gold < 2) gold = 2;
                  if (gold > 5000) gold = (p_ptr->au / 20) + randint(3000);
                  if (gold > p_ptr->au) gold = p_ptr->au;
                  p_ptr->au -= gold;
                  if (gold <= 0)
                  {
                     msg_print("Nothing was stolen.");
                  }
                  else
                  {
                     if (p_ptr->au)
                     {
                        msg_print("Your purse feels lighter.");
                        msg_format("%ld coins were stolen!", (long)gold);
                     }
                     else
                     {
                        msg_print("Your purse feels lighter.");
                        msg_print("All of your coins were stolen!");
                     }
                     p_ptr->redraw1 |= (PR1_GOLD);
                     escaping = TRUE;
                     /* now give this monster some gold! */
                     give_monster_gold(m_idx, gold);
                     msg_print("You burn with desire to avenge this theft.");
                  }
               }

               break;

            case RBE_EAT_ITEM:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by eating an item.", m_name,damage);
               }

               /* Saving throw (unless paralyzed) based on dex and level */
               if (!p_ptr->paralyzed &&
                   (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
                                     p_ptr->lev)))
               {
                  /* Saving throw message */
                  msg_print("You grab hold of your backpack!");

                  /* Occasional "blink" anyway */
                  small_blinked = TRUE;

                  /* Obvious */
                  obvious = TRUE;

                  /* Done */
                  break;
               }

               /* Find an item */
               for (k = 0; k < 10; k++)
               {
                  object_type forge;
                  s16b amt;

                  /* strange things happen otherwise, you have been warned! */
                  forge.spell_set = 0;
                  invwipe(&forge);
                  
                  /* Pick an item */
                  i = rand_int(INVEN_PACK);

                  /* Obtain the item */
                  i_ptr = &inventory[i];

                  /* Accept real items */
                  if (!i_ptr->k_idx) continue;

                  /* Don't steal artifacts  -CFT */
                  if (artifact_p(i_ptr)) continue;

                  /* Get a description */
                  object_desc(i_name, i_ptr, FALSE, 3);

                  amt = randint(i_ptr->number);

                  /* Message */
                  msg_format("%sour %s %s stolen!",
                             ((amt == 1) ? "One of y" : "Y"),
                             i_name, (amt > 1) ? "were":"was");

                  /* create a monster item */
                  forge = (*i_ptr);
                  forge.number = amt;
                  if (monster_inven_carry(m_idx,&forge)==-1)
                  {
                     msg_print("You have a bad feeling about this theft.");
                  }
                  else
                  {
                     msg_print("You burn with desire to avenge this theft.");
                  }

                  /* Steal the items */
                  item_increase(i, -amt, px, py);
                  item_optimize(i, px, py);

                  /* Obvious */
                  obvious = TRUE;

                  /* Blink away */
                  escaping = TRUE;

                  /* Done */
                  break;
               }

               break;

            case RBE_EAT_FOOD:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by eating food.", m_name,damage);
               }

               /* Steal some food */
               for (k = 0; k < 10; k++)
               {
                  /* Pick an item from the pack */
                  i = rand_int(INVEN_PACK);

                  /* Get the item */
                  i_ptr = &inventory[i];

                  /* Accept real items */
                  if (!i_ptr->k_idx) continue;

                  /* Only eat food */
                  if (i_ptr->tval != TV_FOOD) continue;

                  /* Get a description */
                  object_desc(i_name, i_ptr, FALSE, 0);

                  /* Message */
                  msg_format("%sour %s (%c) was eaten!",
                             ((i_ptr->number > 1) ? "One of y" : "Y"),
                             i_name, index_to_label(i));

                  /* Steal the items */
                  item_increase(i, -1, px, py);
                  item_optimize(i, px, py);

                  /* Obvious */
                  obvious = TRUE;

                  /* Done */
                  break;
               }

               break;

            case RBE_EAT_LITE:

               /* Take some damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by draining fuel.", m_name,damage);
               }

               /* Access the lite */
               i_ptr = &inventory[INVEN_LITE];

               /* Drain fuel */
               if ((i_ptr->p1val > 0) && (!artifact_p(i_ptr)))
               {
                  /* Reduce fuel */
                  i_ptr->p1val -= (250 + randint(250));
                  if (i_ptr->p1val < 1) i_ptr->p1val = 1;

                  /* Notice */
                  if (!p_ptr->blind)
                  {
                     msg_print("Your light dims.");
                     obvious = TRUE;
                  }

                  /* Window stuff */
                  p_ptr->window |= (PW_EQUIP);
               }

               break;

            case RBE_ACID:

               /* Obvious */
               obvious = TRUE;

               /* Message */
               msg_print("You are covered in acid!");

               /* Special damage */
               acid_dam(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by acid.", m_name,damage);
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_ACID);

               break;

            case RBE_ELEC:

               /* Obvious */
               obvious = TRUE;

               /* Message */
               msg_print("You are struck by electricity!");

               /* Special damage */
               elec_dam(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by electricity.", m_name,damage);
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_ELEC);

               break;

            case RBE_FIRE:

               /* Obvious */
               obvious = TRUE;

               /* Message */
               msg_print("You are enveloped in flames!");

               /* Special damage */
               fire_dam(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by fire.", m_name,damage);
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_FIRE);

               break;

            case RBE_COLD:

               /* Obvious */
               obvious = TRUE;

               /* Message */
               msg_print("You are covered with frost!");

               /* Special damage */
               cold_dam(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by cold.", m_name,damage);
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_COLD);

               break;

            case RBE_BLIND:

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by blinding.", m_name,damage);
               }

               /* Increase "blind" */
               if (!p_ptr->resist_blind)
               {
                  if (set_blind(p_ptr->blind + 10 + randint(rlev)))
                  {
                     obvious = TRUE;
                  }
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_BLIND);

               break;

            case RBE_CONFUSE:

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by confusing.", m_name,damage);
               }

               /* Increase "confused" */
               if (!p_ptr->resist_conf)
               {
                  if (set_confused(p_ptr->confused + 3 + randint(rlev)))
                  {
                     obvious = TRUE;
                  }
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_CONF);

               break;

            case RBE_TERRIFY:

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by terrifying.", m_name,damage);
               }

               /* Increase "afraid" */
               if (p_ptr->resist_fear)
               {
                  msg_print("You stand your ground!");
                  obvious = TRUE;
               }
               else if (rand_int(100) < p_ptr->skill_sav)
               {
                  msg_print("You stand your ground!");
                  obvious = TRUE;
               }
               else
               {
                  if (set_afraid(p_ptr->afraid + 3 + randint(rlev)))
                  {
                     obvious = TRUE;
                  }
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_FEAR);

               break;

            case RBE_PARALYZE:

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by paralyzing.", m_name,damage);
               }

               /* Increase "paralyzed" */
               if (p_ptr->free_act)
               {
                  msg_print("You are unaffected!");
                  obvious = TRUE;
               }
               else if (rand_int(100) < p_ptr->skill_sav)
               {
                  msg_print("You resist the effects!");
                  obvious = TRUE;
               }
               else
               {
                  if (set_paralyzed(p_ptr->paralyzed + 3 + randint(rlev)))
                  {
                     obvious = TRUE;
                  }
               }

               /* Learn about the player */
               update_smart_learn(m_idx, DRS_FREE);

               break;

            case RBE_LOSE_STR:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering strength.", m_name,damage);
               }

               /* Damage (stat) */
               if (do_dec_stat(A_STR, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_LOSE_INT:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering intelligence.", m_name,damage);
               }

               /* Damage (stat) */
               if (do_dec_stat(A_INT, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_LOSE_WIS:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering wisdom.", m_name,damage);
               }

               /* Damage (stat) */
               if (do_dec_stat(A_WIS, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_LOSE_DEX:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering dexterity.", m_name,damage);
               }

               /* Damage (stat) */
               if (do_dec_stat(A_DEX, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_LOSE_CON:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering constitution.", m_name,damage);
               }

               /* Damage (stat) */
               if (do_dec_stat(A_CON, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_LOSE_CHR:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering charisma.", m_name,damage);
               }

               /* Damage (stat) */
               if (do_dec_stat(A_CHR, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_LOSE_ALL:

               /* Damage (physical) */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering all stats.", m_name,damage);
               }

               /* Damage (stats) */
               if (do_dec_stat(A_STR, STAT_DEC_NORMAL)) obvious = TRUE;
               if (do_dec_stat(A_DEX, STAT_DEC_NORMAL)) obvious = TRUE;
               if (do_dec_stat(A_CON, STAT_DEC_NORMAL)) obvious = TRUE;
               if (do_dec_stat(A_INT, STAT_DEC_NORMAL)) obvious = TRUE;
               if (do_dec_stat(A_WIS, STAT_DEC_NORMAL)) obvious = TRUE;
               if (do_dec_stat(A_CHR, STAT_DEC_NORMAL)) obvious = TRUE;

               break;

            case RBE_SHATTER:

               /* Obvious */
               obvious = TRUE;

               /* Hack -- Reduce damage based on the player armor class */
               damage -= (damage * ((ac < 150) ? ac : 150) / 250);

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by releasing an earthquake.", m_name,damage);
               }

               /* Radius 8 earthquake centered at the monster */
               if (damage > 23) earthquake(m_ptr->fx, m_ptr->fy, 8);

               break;

            case RBE_EXP_10:

               /* Obvious */
               obvious = TRUE;

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering experience(10).", m_name,damage);
               }

               if (p_ptr->hold_life && (rand_int(100) < 95))
               {
                  msg_print("You keep hold of your life force!");
               }
               else
               {
                  s32b d = damroll(10,6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
                  if (p_ptr->hold_life)
                  {
                     msg_print("You feel your life slipping away!");
                     lose_exp(d/10);
                  }
                  else
                  {
                     msg_print("You feel your life draining away!");
                     lose_exp(d);
                  }
               }
               break;

            case RBE_EXP_20:

               /* Obvious */
               obvious = TRUE;

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering experience(20).", m_name,damage);
               }

               if (p_ptr->hold_life && (rand_int(100) < 90))
               {
                  msg_print("You keep hold of your life force!");
               }
               else
               {
                  s32b d = damroll(20,6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
                  if (p_ptr->hold_life)
                  {
                     msg_print("You feel your life slipping away!");
                     lose_exp(d/10);
                  }
                  else
                  {
                     msg_print("You feel your life draining away!");
                     lose_exp(d);
                  }
               }
               break;

            case RBE_EXP_40:

               /* Obvious */
               obvious = TRUE;

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering experience(40).", m_name,damage);
               }

               if (p_ptr->hold_life && (rand_int(100) < 75))
               {
                  msg_print("You keep hold of your life force!");
               }
               else
               {
                  s32b d = damroll(40,6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
                  if (p_ptr->hold_life)
                  {
                     msg_print("You feel your life slipping away!");
                     lose_exp(d/10);
                  }
                  else
                  {
                     msg_print("You feel your life draining away!");
                     lose_exp(d);
                  }
               }
               break;

            case RBE_EXP_80:

               /* Obvious */
               obvious = TRUE;

               /* Take damage */
               take_hit(damage, ddesc);
               if (wizard)
               {
                  msg_format("%^s does %d damage by lowering experience(80).", m_name,damage);
               }

               if (p_ptr->hold_life && (rand_int(100) < 50))
               {
                  msg_print("You keep hold of your life force!");
               }
               else
               {
                  s32b d = damroll(80,6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
                  if (p_ptr->hold_life)
                  {
                     msg_print("You feel your life slipping away!");
                     lose_exp(d/10);
                  }
                  else
                  {
                     msg_print("You feel your life draining away!");
                     lose_exp(d);
                  }
               }
               break;
         }


         /* Hack -- only one of cut or stun */
         if (do_cut && do_stun)
         {
            /* Cancel cut */
            if (rand_int(100) < 50)
            {
               do_cut = 0;
            }

            /* Cancel stun */
            else
            {
               do_stun = 0;
            }
         }

         /* Handle cut */
         if (do_cut && !do_reflect)
         {
            s16b k = 0;

            /* Critical hit (zero if non-critical) */
            tmp = monster_critical(d_dice, d_side, damage);

            /* Roll for damage */
            switch (tmp)
            {
                case 0: k = 0; break;
                case 1: k = randint(5); break;
                case 2: k = randint(5) + 5; break;
                case 3: k = randint(20) + 20; break;
                case 4: k = randint(50) + 50; break;
                case 5: k = randint(100) + 100; break;
                case 6: k = 300; break;
                default: k = 500; break;
            }

            /* Apply the cut */
            if (k)
            {
               (void)set_cut(p_ptr->cut + k);
               if (wizard)
               {
                  msg_format("%^s increases your cuts by %d.", m_name,k);
               }
            }
         }

         /* Handle stun */
         if (do_stun && !do_reflect)
         {
             s16b k = 0;

             /* Critical hit (zero if non-critical) */
             tmp = monster_critical(d_dice, d_side, damage);

             /* Roll for damage */
             switch (tmp)
             {
                 case 0: k = 0; break;
                 case 1: k = randint(5); break;
                 case 2: k = randint(10) + 10; break;
                 case 3: k = randint(20) + 20; break;
                 case 4: k = randint(30) + 30; break;
                 case 5: k = randint(40) + 40; break;
                 case 6: k = 100; break;
                 default: k = 200; break;
             }

             /* Apply the stun */
             if (k)
             {
                (void)set_stun(p_ptr->stun + k);
                if (wizard)
                {
                    msg_format("%^s increases your stunning by %d.", m_name,k);
                }
             }
         }

         /* Hack -- delay fear messages */
         /* fear can only occur when reflecting */
         if (fear && m_ptr->ml)
         {
             /* Sound */
             sound(SOUND_FLEE);

             /* Message */
             msg_format("%^s turns away from you in terror.", m_name);
         }
      }

      /* Monster missed player */
      else
      {
         /* Analyze failed attacks */
         switch (method)
         {
            case RBM_HIT:
            case RBM_TOUCH:
            case RBM_PUNCH:
            case RBM_KICK:
            case RBM_CLAW:
            case RBM_BITE:
            case RBM_STING:
            case RBM_XXX1:
            case RBM_BUTT:
            case RBM_CRUSH:
            case RBM_ENGULF:
            case RBM_XXX2:

               /* Visible monsters */
               if (m_ptr->ml)
               {
                  /* Disturbing */
                  disturb(1, 0);

                  /* Message */
                  if (p_ptr->invuln)
                     msg_format("Your magical shield protects you from %^s.", m_name);
                  else
                     msg_format("%^s misses you.", m_name);
               }

               break;
         }
      }

      /* Analyze "visible" monsters only */
      if (visible)
      {
         /* Count "obvious" attacks (and ones that cause damage) */
         if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
         {
            /* Count attacks of this type */
            if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
            {
                r_ptr->r_blows[ap_cnt]++;
            }
         }
      }
   }

   /* jk - escaping means a "short" blink, followed by a very big increase */
   /* in speed */
   if (escaping)
   {
      msg_format("%^s sprints away very fast.", m_name);

      if (!escaping)
      {
         /* an already escaping monster shouldn't strike again, but who knows */
         /* what goes on in a monsters' mind? - anyway, don't give them too   */
         /* much speed - increasing it once is enough */
         m_ptr->oldspeed = m_ptr->mspeed;
         m_ptr->mspeed += 10 + (r_ptr->level / 10) +
                          randint(10+(r_ptr->level / 10));
      }
      /* this may be increased! */
      m_ptr->escaping += r_ptr->level + 5 + randint(10);

      small_blinked = TRUE;
   }

   if (small_blinked)
   {
      teleport_away(m_idx, 5);
   }

   /* Always notice cause of death */
   if (death && (r_ptr->r_deaths < MAX_SHORT)) r_ptr->r_deaths++;

   /* Assume we attacked */
   return (TRUE);
}


