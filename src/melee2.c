/* File: melee2.c */

/* Purpose: Monster spells and movement */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#ifdef DRS_SMART_OPTIONS

/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeves Sward).
 * Major modifications by "BEN" (Ben Harrison).
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.  This will require
 * much less space, if I am not mistaken.  Thus, each monster gets a
 * set of 32 bit flags, "smart", build from the various "SM_*" flags.
 *
 * This has the added advantage that attacks and spells are related.
 * The "smart_learn" option means that the monster "learns" the flags
 * that should be set, and "smart_cheat" means that he "knows" them.
 * So "smart_cheat" means that the "smart" field is always up to date,
 * while "smart_learn" means that the "smart" field is slowly learned.
 * Both of them have the same effect on the "choose spell" routine.
 */

/*
 * Internal probablility routine
 */
static bool int_outof(monster_race *r_ptr, s16b prob)
{
   /* Non-Smart monsters are half as "smart" */
   if (!(r_ptr->flags2 & RF2_SMART)) prob = prob / 2;

   /* Roll the dice */
   return (rand_int(100) < prob);
}

/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(s16b m_idx, u64b *f4p, u64b *f5p, u64b *f6p)
{
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   u64b f4 = (*f4p);
   u64b f5 = (*f5p);
   u64b f6 = (*f6p);

   u32b smart = 0L;

   /* Too stupid to know anything */
   if (r_ptr->flags2 & RF2_STUPID) return;

   /* Must be cheating or learning */
   if (!smart_cheat && !smart_learn) return;

   /* Update acquired knowledge */
   if (smart_learn)
   {
      /* Hack -- Occasionally forget player status */
      if (m_ptr->smart && (rand_int(100) < 1)) m_ptr->smart = 0L;

      /* Use the memorized flags */
      smart = m_ptr->smart;
   }

   /* Cheat if requested */
   if (smart_cheat)
   {
      /* Know basic info */
      if (p_ptr->resist_acid) smart |= SM_RES_ACID;
      if (p_ptr->oppose_acid) smart |= SM_OPP_ACID;
      if (p_ptr->immune_acid) smart |= SM_IMM_ACID;
      if (p_ptr->resist_elec) smart |= SM_RES_ELEC;
      if (p_ptr->oppose_elec) smart |= SM_OPP_ELEC;
      if (p_ptr->immune_elec) smart |= SM_IMM_ELEC;
      if (p_ptr->resist_fire) smart |= SM_RES_FIRE;
      if (p_ptr->oppose_fire) smart |= SM_OPP_FIRE;
      if (p_ptr->immune_fire) smart |= SM_IMM_FIRE;
      if (p_ptr->resist_cold) smart |= SM_RES_COLD;
      if (p_ptr->oppose_cold) smart |= SM_OPP_COLD;
      if (p_ptr->immune_cold) smart |= SM_IMM_COLD;

      /* Know poison info */
      if (p_ptr->resist_pois) smart |= SM_RES_POIS;
      if (p_ptr->oppose_pois) smart |= SM_OPP_POIS;

      /* Know special resistances */
      if (p_ptr->resist_neth) smart |= SM_RES_NETH;
      if (p_ptr->resist_lite) smart |= SM_RES_LITE;
      if (p_ptr->resist_dark) smart |= SM_RES_DARK;
      if (p_ptr->resist_fear) smart |= SM_RES_FEAR;
      if (p_ptr->resist_conf) smart |= SM_RES_CONF;
      if (p_ptr->resist_chaos) smart |= SM_RES_CHAOS;
      if (p_ptr->resist_disen) smart |= SM_RES_DISEN;
      if (p_ptr->resist_blind) smart |= SM_RES_BLIND;
      if (p_ptr->resist_nexus) smart |= SM_RES_NEXUS;
      if (p_ptr->resist_sound) smart |= SM_RES_SOUND;
      if (p_ptr->resist_shard) smart |= SM_RES_SHARD;

      /* Know bizarre "resistances" */
      if (p_ptr->free_act) smart |= SM_IMM_FREE;
      if (!p_ptr->msp) smart |= SM_IMM_MANA;
   }

   /* Nothing known */
   if (!smart) return;

   if (smart & SM_IMM_ACID)
   {
      if (int_outof(r_ptr, 100)) f4 &= ~RF4_BR_ACID;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BA_ACID;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BO_ACID;
   }
   else if ((smart & SM_OPP_ACID) && (smart & SM_RES_ACID))
   {
      if (int_outof(r_ptr, 80)) f4 &= ~RF4_BR_ACID;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BA_ACID;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BO_ACID;
   }
   else if ((smart & SM_OPP_ACID) || (smart & SM_RES_ACID))
   {
      if (int_outof(r_ptr, 30)) f4 &= ~RF4_BR_ACID;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BA_ACID;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BO_ACID;
   }


   if (smart & SM_IMM_ELEC)
   {
      if (int_outof(r_ptr, 100)) f4 &= ~RF4_BR_ELEC;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BA_ELEC;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BO_ELEC;
   }
   else if ((smart & SM_OPP_ELEC) && (smart & SM_RES_ELEC))
   {
      if (int_outof(r_ptr, 80)) f4 &= ~RF4_BR_ELEC;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BA_ELEC;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BO_ELEC;
   }
   else if ((smart & SM_OPP_ELEC) || (smart & SM_RES_ELEC))
   {
      if (int_outof(r_ptr, 30)) f4 &= ~RF4_BR_ELEC;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BA_ELEC;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BO_ELEC;
   }


   if (smart & SM_IMM_FIRE)
   {
      if (int_outof(r_ptr, 100)) f4 &= ~RF4_BR_FIRE;
      if (int_outof(r_ptr, 100)) f4 &= ~RF4_BR_PFIRE;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BA_FIRE;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BO_FIRE;
   }
   else if ((smart & SM_OPP_FIRE) && (smart & SM_RES_FIRE))
   {
      if (int_outof(r_ptr, 80)) f4 &= ~RF4_BR_FIRE;
      if (int_outof(r_ptr, 80)) f4 &= ~RF4_BR_PFIRE;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BA_FIRE;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BO_FIRE;
   }
   else if ((smart & SM_OPP_FIRE) || (smart & SM_RES_FIRE))
   {
      if (int_outof(r_ptr, 30)) f4 &= ~RF4_BR_FIRE;
      if (int_outof(r_ptr, 30)) f4 &= ~RF4_BR_PFIRE;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BA_FIRE;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BO_FIRE;
   }

   if (smart & SM_IMM_COLD)
   {
      if (int_outof(r_ptr, 100)) f4 &= ~RF4_BR_COLD;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BA_COLD;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BO_COLD;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BO_ICEE;
   }
   else if ((smart & SM_OPP_COLD) && (smart & SM_RES_COLD))
   {
      if (int_outof(r_ptr, 80)) f4 &= ~RF4_BR_COLD;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BA_COLD;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BO_COLD;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BO_ICEE;
   }
   else if ((smart & SM_OPP_COLD) || (smart & SM_RES_COLD))
   {
      if (int_outof(r_ptr, 30)) f4 &= ~RF4_BR_COLD;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BA_COLD;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BO_COLD;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BO_ICEE;
   }

   if ((smart & SM_OPP_POIS) && (smart & SM_RES_POIS))
   {
      if (int_outof(r_ptr, 80)) f4 &= ~RF4_BR_POIS;
      if (int_outof(r_ptr, 80)) f5 &= ~RF5_BA_POIS;
   }
   else if ((smart & SM_OPP_POIS) || (smart & SM_RES_POIS))
   {
      if (int_outof(r_ptr, 30)) f4 &= ~RF4_BR_POIS;
      if (int_outof(r_ptr, 30)) f5 &= ~RF5_BA_POIS;
   }


   if (smart & SM_RES_NETH)
   {
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_NETH;
      if (int_outof(r_ptr, 50)) f5 &= ~RF5_BA_NETH;
      if (int_outof(r_ptr, 50)) f5 &= ~RF5_BO_NETH;
   }

   if (smart & SM_RES_LITE)
   {
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_LITE;
   }

   if (smart & SM_RES_DARK)
   {
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_DARK;
      if (int_outof(r_ptr, 50)) f5 &= ~RF5_BA_DARK;
   }

   if (smart & SM_RES_FEAR)
   {
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_SCARE;
   }

   if (smart & SM_RES_CONF)
   {
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_CONF;
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_CONF;
   }

   if (smart & SM_RES_CHAOS)
   {
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_CONF;
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_CONF;
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_CHAO;
   }

   if (smart & SM_RES_DISEN)
   {
      if (int_outof(r_ptr, 100)) f4 &= ~RF4_BR_DISE;
   }

   if (smart & SM_RES_BLIND)
   {
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_BLIND;
   }

   if (smart & SM_RES_NEXUS)
   {
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_NEXU;
      if (int_outof(r_ptr, 50)) f6 &= ~RF6_TELE_LEVEL;
   }

   if (smart & SM_RES_SOUND)
   {
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_SOUN;
   }

   if (smart & SM_RES_SHARD)
   {
      if (int_outof(r_ptr, 50)) f4 &= ~RF4_BR_SHAR;
   }


   if (smart & SM_IMM_FREE)
   {
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_HOLD;
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_SLOW;
   }

   if (smart & SM_IMM_MANA)
   {
      if (int_outof(r_ptr, 100)) f5 &= ~RF5_DRAIN_MANA;
   }


   /* XXX XXX XXX No spells left? */
   /* if (!f4 && !f5 && !f6) ... */


   (*f4p) = f4;
   (*f5p) = f5;
   (*f6p) = f6;
}


#endif

/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */

static bool summon_possible(int x1, int y1)
{
   int x, y;

   /* Start at the player's location, and check 2 grids in each dir */

   for (y= y1-2; y<= y1+2; y++)
   {
     for (x = x1-2; x<=x1+2; x++)
     {
       /* Ignore illegal locations */
       if (!in_bounds(x,y)) continue;

       /* Only check a circular area */
       if (distance(x1,y1,x,y)>2) continue;

       /* Hack: no summon on glyph of warding */
       if (test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) continue;

       /* Require empty floor grid in line of sight */
       if (naked_grid_bold(x,y) && los(x1,y1,x,y)) return (TRUE);
     }
   }
   return FALSE;
}

/*
 * copied from Oangband 0.4.0:
 * Determine if a bolt spell will hit the player.  From Keldon Jones' AI.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 */
static bool clean_shot(s16b x1, s16b y1, s16b x2, s16b y2, s16b rad)
{
   s16b x, y;

   s16b grid_n = 0;
   u16b grid_g[512];

   /* Check the projection path */
   grid_n = project_path(grid_g, MAX_RANGE, x1, y1, x2, y2, PROJECT_STOP);

   /* Source and target the same */
   if (!grid_n) return (FALSE);

   /* Final grid */
   x = GRID_X(grid_g[grid_n-1]);
   y = GRID_Y(grid_g[grid_n-1]);

   /* May not end in a wall grid, unless trees or rubble. -LM- */

   if ( (dungeon.level[sublevel][y][x].mtyp != DUNG_FLOOR) &&
        (dungeon.level[sublevel][y][x].mtyp != DUNG_WILDN) &&
        (rad == 0) )
   {
      return (FALSE);
   }

   /* May not end in an unrequested grid */
   if ( (rad == 0 ) && ( (x != x2) || (y != y2) ) ) return (FALSE);

   /* are we close enough? */
   if (rad > 0)
   {
      if ( (distance(x, y, x2, y2) <= rad) && (los(x, y, x2, y2)) )
      {
         return (TRUE);
      }
   }

   /* Assume okay */
   return (TRUE);
}

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(s16b m_idx, s16b typ, s16b dam_hp)
{
   s16b flg = PROJECT_STOP | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;
   project_who_type who;

   /* Target the player with a bolt attack */
   who.type = WHO_MONSTER;
   who.index = m_idx;
   who.index_race = mn_list[m_idx].r_idx;

   (void)project(&who, 0, px, py, dam_hp, typ, flg);
}

/*
 * Return TRUE if a spell is good for hurting the player (directly).
 */
static bool spell_ball_attack(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_BALL_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_BALL_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_BALL_MASK)) return (TRUE);

   /* Doesn't hurt */
   return (FALSE);
}

/*
 * Return TRUE if a spell is good for hurting the player (directly).
 */
static bool spell_attack(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_ATTACK_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_ATTACK_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_ATTACK_MASK)) return (TRUE);

   /* Doesn't hurt */
   return (FALSE);
}

/*
 * Return TRUE if a spell is good for escaping.
 */
static bool spell_escape(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_ESCAPE_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_ESCAPE_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_ESCAPE_MASK)) return (TRUE);

   /* Isn't good for escaping */
   return (FALSE);
}

/*
 * Return TRUE if a spell is good for annoying the player.
 */
static bool spell_annoy(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_ANNOY_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_ANNOY_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_ANNOY_MASK)) return (TRUE);

   /* Doesn't annoy */
   return (FALSE);
}

/*
 * Return TRUE if a spell summons help.
 */
static bool spell_summon(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_SUMMON_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_SUMMON_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_SUMMON_MASK)) return (TRUE);

   /* Doesn't summon */
   return (FALSE);
}

/*
 * Return TRUE if a spell is good in a tactical situation.
 */
static bool spell_tactic(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_TACTIC_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_TACTIC_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_TACTIC_MASK)) return (TRUE);

   /* Not good */
   return (FALSE);
}

/*
 * Return TRUE if a spell hastes.
 */
static bool spell_haste(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_SPEED_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_SPEED_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_SPEED_MASK)) return (TRUE);

   /* Not a haste spell */
   return (FALSE);
}

 /*
  * + * Return TRUE if a spell is good for healing.
  * + */
static bool spell_heal(cast_spell_type spell)
{
   if ((spell.flag == 4) && (spell.spell & RF4_HEAL_MASK)) return (TRUE);
   if ((spell.flag == 5) && (spell.spell & RF5_HEAL_MASK)) return (TRUE);
   if ((spell.flag == 6) && (spell.spell & RF6_HEAL_MASK)) return (TRUE);

   /* No healing */
   return (FALSE);
}

/*
 * Have a monster choose a spell from a list of "useful" spells.
 *
 * Note that this list does NOT include spells that will just hit
 * other monsters, and the list is restricted when the monster is
 * "desperate".  Should that be the job of this function instead?
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * Use the helper functions above to put spells into categories.
 *
 * This function may well be an efficiency bottleneck.
 */
static s16b choose_attack_spell(int m_idx, cast_spell_type cast_spell[], byte num)
{
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   byte escape[96], escape_num = 0;
   byte ball_attack[96], ball_attack_num = 0;
   byte attack[96], attack_num = 0;
   byte summon[96], summon_num = 0;
   byte tactic[96], tactic_num = 0;
   byte annoy[96], annoy_num = 0;
   byte haste[96], haste_num = 0;
   byte heal[96], heal_num = 0;

   s16b i;

dlog(DEBUGMONAI, "melee2.c: choose_attack_spell: monster %d (%s) @ %d,%d 1:%08lx 2:%08lx 3:%08lx 4:%08lx 5:%08lx 6:%08lx\n",
                 m_idx, r_name + r_ptr->name, m_ptr->fx, m_ptr->fy, r_ptr->flags1, r_ptr->flags2, r_ptr->flags3,
                 r_ptr->flags4, r_ptr->flags5, r_ptr->flags6);
   /* Stupid monsters choose randomly */
   if (r_ptr->flags2 & (RF2_STUPID))
   {
      /* Pick at random */
      return rand_int(num);
   }

   /* Categorize spells */
   for (i = 0; i < num; i++)
   {
      /* Escape spell? */
      if (spell_escape(cast_spell[i])) escape[escape_num++] = i;

      /* Ball Attack spell? */
      if (spell_ball_attack(cast_spell[i])) ball_attack[ball_attack_num++] = i;

      /* Attack spell? */
      if (spell_attack(cast_spell[i])) attack[attack_num++] = i;

      /* Summon spell? */
      if (spell_summon(cast_spell[i])) summon[summon_num++] = i;

      /* Tactical spell? */
      if (spell_tactic(cast_spell[i])) tactic[tactic_num++] = i;

      /* Annoyance spell? */
      if (spell_annoy(cast_spell[i])) annoy[annoy_num++] = i;

      /* Haste spell? */
      if (spell_haste(cast_spell[i])) haste[haste_num++] = i;

      /* Heal spell? */
      if (spell_heal(cast_spell[i])) heal[heal_num++] = i;
   }

   /*** Try to pick an appropriate spell type ***/
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: escape_num %d ball_attack_num %d attack_num %d summon_num %d tactic_num %d\n",
                escape_num, ball_attack_num, attack_num, summon_num, tactic_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: annoy_num %d haste_num %d heal_num %d\n",
                annoy_num, haste_num, heal_num);
   /* Hurt badly or afraid, attempt to flee */
   if ((m_ptr->hp < m_ptr->maxhp / 3) || m_ptr->afraid)
   {
      /* Choose escape spell if possible */
      if (escape_num)
      {
         s16b result = rand_int(escape_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: escape_num %d, returning %d spell %d\n", escape_num, result, escape[result]);
         return escape[result];
      }
   }

   /* Still hurt badly, couldn't flee, attempt to heal */
   if (m_ptr->hp < m_ptr->maxhp / 3)
   {
      /* Choose heal spell if possible */
      if (heal_num)
      {
         s16b result = rand_int(heal_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: heal_num %d, returning %d spell %d\n", heal_num, result, heal[result]);
         return heal[result];
      }
   }

   /* Player is close and we have attack spells, blink away */
   if ((distance(px, py, m_ptr->fx, m_ptr->fy) < 4) && attack_num &&
      (rand_int(100) < 75))
   {
      /* Choose tactical spell */
      if (tactic_num)
      {
         s16b result = rand_int(tactic_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: tactic_num %d, returning %d spell %d\n", tactic_num, result, tactic[result]);
         return tactic[result];
      }
   }

   /* We're hurt (not badly), try to heal */
   if ((m_ptr->hp < m_ptr->maxhp * 3 / 4) && (rand_int(100) < 75))
   {
      /* Choose heal spell if possible */
      if (heal_num)
      {
         s16b result = rand_int(heal_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: heal_num %d, returning %d spell %d\n", heal_num, result, heal[result]);
         return heal[result];
      }
   }

   /* Summon if possible (sometimes) */
   if (summon_num && (rand_int(100) < 50))
   {
      /* Choose summon spell */
      s16b result;
      result = rand_int(summon_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: summon_num %d, returning %d spell %d\n", summon_num, result, summon[result]);
      return summon[result];
   }

   /* smart monsters try to breath around the corner too */
   if ( (r_ptr->flags2 & RF2_SMART) && (ball_attack_num > 0) )
   {
      if ( (randint(100)<50) &&
           clean_shot(m_ptr->fx, m_ptr->fy, px, py, (r_ptr->flags2 & RF2_POWERFUL)?3:2) &&
           !clean_shot(m_ptr->fx, m_ptr->fy, px, py, 0) )
      {
         s16b result;
         result = rand_int(ball_attack_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spell: ball_attack_num %d, returning %d spell %d\n", ball_attack_num, result, ball_attack[result]);
         return ball_attack[result];
      }
   }

   /* Attack spell (most of the time) */
   if (attack_num && (rand_int(100) < 85))
   {
      /* Choose attack spell */
      s16b result;
      result = rand_int(attack_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spel: attack_num %d, returning %d spell %d\n", attack_num, result, attack[result]);
      return attack[result];
   }

   /* Try another tactical spell (sometimes) */
   if (tactic_num && (rand_int(100) < 50))
   {
      /* Choose tactic spell */
      s16b result;
      result = rand_int(tactic_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spel: tactic_num %d, returning %d spell %d\n", tactic_num, result, tactic[result]);
      return tactic[result];
   }

   /* Haste self if we aren't already somewhat hasted (rarely) */
   if (haste_num && (rand_int(100) < (20 + r_ptr->speed - m_ptr->mspeed)))
   {
      /* Choose haste spell */
      s16b result;
      result = rand_int(haste_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spel: haste_num %d, returning %d spell %d\n", haste_num, result, haste[result]);
      return haste[result];
   }

   /* Annoy player (most of the time) */
   if (annoy_num && (rand_int(100) < 85))
   {
      /* Choose annoyance spell */
      s16b result;
      result = rand_int(annoy_num);
dlog(DEBUGMONAI,"melee2.c: choose_attack_spel: annoy_num %d, returning %d spell %d\n", annoy_num, result, annoy[result]);
      return annoy[result];
   }

   /* Choose no spell */
   return (-1);
}

/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(s16b m_idx, s16b typ, s16b dam_hp)
{
   s16b                 rad;

   s16b                 flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER;

   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];
   project_who_type     who;

   /* Determine the radius of the blast */
   rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

   /* Target the player with a bolt attack */
   who.type = WHO_MONSTER;
   who.index = m_idx;
   who.index_race = m_ptr->r_idx;

   (void)project(&who, rad, px, py, dam_hp, typ, flg);
}

/* 
 * this function determines how many monster another monster tries
 * to summon
 */
static s16b summon_number(s16b m_idx)
{
   s16b          result = 0;
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   if (r_ptr->flags6 & RF6_S_NUMBER2) result += 2;
   if (r_ptr->flags6 & RF6_S_NUMBER4) result += 4;
   if (r_ptr->flags6 & RF6_S_NUMBER8) result += 8;
   if (r_ptr->flags6 & RF6_S_NUMBER16) result += 16;

   /* summoning a single monster is always possible */
   if (result == 0) result = 1;
   return result;
}

/*
 * this make a monster execute a spell from r_info[].flags4
 */
static void make_attack_spell4(s16b m_idx, u64b thrown_spell)
{
   monster_type *m_ptr = &mn_list[m_idx];
   char       m_name[80];
   char       m_poss[80];
   char       ddesc[80];
   bool       blind = (p_ptr->blind ? TRUE : FALSE);

   /* Assume "normal" target */

   /* Get the monster name (or "it") */
   monster_desc(m_name, m_ptr, 0x00);

   /* Get the monster possessive ("his"/"her"/"its") */
   monster_desc(m_poss, m_ptr, 0x22);

   /* Hack -- Get the "died from" name */
   monster_desc(ddesc, m_ptr, 0x88);

   /* Cast the spell. */
   switch (thrown_spell)
   {
      case RF4_SHRIEK:
         {
            project_who_type who;

            disturb(1, 0);
            msg_format("%^s makes a high pitched shriek.", m_name);
            who.type = WHO_MONSTER;
            who.index = m_idx;
            who.index_race = m_ptr->r_idx;
         
            (void)aggravate_monsters(&who, -1);
            break;
         }

      case RF4_ARROW_1:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires an arrow.", m_name);
         bolt(m_idx, GF_ARROW, damroll(1, 6));
         break;

      case RF4_ARROW_2:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires an arrow.", m_name);
         bolt(m_idx, GF_ARROW, damroll(3, 6));
         break;

      case RF4_ARROW_3:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires an arrow.", m_name);
         bolt(m_idx, GF_ARROW, damroll(5, 6));
         break;

      case RF4_ARROW_4:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires an arrow.", m_name);
         bolt(m_idx, GF_ARROW, damroll(7, 6));
         break;

      case RF4_ARROW_5:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires an arrow.", m_name);
         bolt(m_idx, GF_ARROW, damroll(12, 6));
         break;

      case RF4_BOLT_1:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a bolt.", m_name);
         bolt(m_idx, GF_ARROW, damroll(1, 8));
         break;

      case RF4_BOLT_2:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a bolt.", m_name);
         bolt(m_idx, GF_ARROW, damroll(3, 8));
         break;

      case RF4_BOLT_3:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a bolt.", m_name);
         bolt(m_idx, GF_ARROW, damroll(5, 8));
         break;

      case RF4_BOLT_4:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a bolt.", m_name);
         bolt(m_idx, GF_ARROW, damroll(7, 8));
         break;

      case RF4_BOLT_5:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a bolt.", m_name);
         bolt(m_idx, GF_ARROW, damroll(12, 8));
         break;

      case RF4_SHOT_1:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a shot.", m_name);
         bolt(m_idx, GF_ARROW, damroll(1, 5));
         break;

      case RF4_SHOT_2:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a shot.", m_name);
         bolt(m_idx, GF_ARROW, damroll(3, 5));
         break;

      case RF4_SHOT_3:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a shot.", m_name);
         bolt(m_idx, GF_ARROW, damroll(5, 5));
         break;

      case RF4_SHOT_4:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a shot.", m_name);
         bolt(m_idx, GF_ARROW, damroll(7, 5));
         break;

      case RF4_SHOT_5:
         disturb(1, 0);
         if (blind) msg_format("%^s makes a strange noise.", m_name);
         else msg_format("%^s fires a shot.", m_name);
         bolt(m_idx, GF_ARROW, damroll(12, 5));
         break;

      case RF4_BR_ACID:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes acid.", m_name);
         breath(m_idx, GF_ACID,
               ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
         update_smart_learn(m_idx, DRS_ACID);
         break;

      case RF4_BR_ELEC:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes lightning.", m_name);
         breath(m_idx, GF_ELEC,
               ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
         update_smart_learn(m_idx, DRS_ELEC);
         break;

      case RF4_BR_FIRE:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes fire.", m_name);
         breath(m_idx, GF_FIRE,
               ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
         update_smart_learn(m_idx, DRS_FIRE);
         break;

      case RF4_BR_COLD:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes frost.", m_name);
         breath(m_idx, GF_COLD,
               ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3)));
         update_smart_learn(m_idx, DRS_COLD);
         break;

      case RF4_BR_POIS:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes gas.", m_name);
         breath(m_idx, GF_POIS,
               ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3)));
         update_smart_learn(m_idx, DRS_POIS);
         break;

      case RF4_BR_NETH:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes nether.", m_name);
         breath(m_idx, GF_NETHER,
               ((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6)) );
         update_smart_learn(m_idx, DRS_NETH);
         break;

      case RF4_BR_LITE:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes light.", m_name);
         breath(m_idx, GF_LITE,
            ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_LITE);
         break;

      case RF4_BR_DARK:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes darkness.", m_name);
         breath(m_idx, GF_DARK,
            ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_DARK);
         break;

      case RF4_BR_CONF:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes confusion.", m_name);
         breath(m_idx, GF_CONFUSION,
            ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_CONF);
         break;

      case RF4_BR_SOUN:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes sound.", m_name);
         breath(m_idx, GF_SOUND,
            ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_SOUND);
         break;

      case RF4_BR_CHAO:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes chaos.", m_name);
         breath(m_idx, GF_CHAOS,
               ((m_ptr->hp / 6) > 600 ? 600 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_CHAOS);
         break;

      case RF4_BR_DISE:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes disenchantment.", m_name);
         breath(m_idx, GF_DISENCHANT,
            ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_DISEN);
         break;

      case RF4_BR_NEXU:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes nexus.", m_name);
         breath(m_idx, GF_NEXUS,
            ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)));
         update_smart_learn(m_idx, DRS_NEXUS);
         break;

      case RF4_BR_TIME:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes time.", m_name);
         breath(m_idx, GF_TIME,
            ((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3)));
         break;

      case RF4_BR_INER:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes inertia.", m_name);
         breath(m_idx, GF_INERTIA,
               ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
         break;

      case RF4_BR_GRAV:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes gravity.", m_name);
         breath(m_idx, GF_GRAVITY,
            ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)));
         break;

      case RF4_BR_SHAR:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes shards.", m_name);
         breath(m_idx, GF_SHARDS,
            ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
         update_smart_learn(m_idx, DRS_SHARD);
         break;

      case RF4_BR_PLAS:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes plasma.", m_name);
         breath(m_idx, GF_PLASMA,
            ((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6)));
         break;

      case RF4_BR_WALL:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes force.", m_name);
         breath(m_idx, GF_FORCE,
               ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6)));
         break;

      case RF4_BR_MANA:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes mana.", m_name);
         breath(m_idx, GF_FORCE,
               ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6)));
         break;

      case RF4_BR_PFIRE:
         disturb(1, 0);
         if (blind) msg_format("%^s breathes.", m_name);
         else msg_format("%^s breathes a large gust of fire.", m_name);
         breath(m_idx, GF_FIRE,
               ((m_ptr->hp / 2) > 2000 ? 2000 : (m_ptr->hp / 2)));
         update_smart_learn(m_idx, DRS_FIRE);
         break;
   }
}

/*
 * this make a monster execute a spell from r_info[].flags5
 */
static void make_attack_spell5(s16b m_idx, u64b thrown_spell)
{
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
   char       m_name[80];
   char       m_poss[80];
   char       ddesc[80];
   bool       blind = (p_ptr->blind ? TRUE : FALSE);
   bool       seen = (!blind && m_ptr->ml);
   s16b       rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

   /* Get the monster name (or "it") */
   monster_desc(m_name, m_ptr, 0x00);

   /* Get the monster possessive ("his"/"her"/"its") */
   monster_desc(m_poss, m_ptr, 0x22);

   /* Hack -- Get the "died from" name */
   monster_desc(ddesc, m_ptr, 0x88);

   /* Cast the spell. */
   switch (thrown_spell)
   {
      case RF5_BA_ACID:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts an acid ball.", m_name);
         breath(m_idx, GF_ACID,
               randint(rlev * 3) + 15);
         update_smart_learn(m_idx, DRS_ACID);
         break;

      case RF5_BA_ELEC:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a lightning ball.", m_name);
         breath(m_idx, GF_ELEC,
            randint(rlev * 3 / 2) + 8);
         update_smart_learn(m_idx, DRS_ELEC);
         break;

      case RF5_BA_FIRE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a fire ball.", m_name);
         breath(m_idx, GF_FIRE,
               randint(rlev * 7 / 2) + 10);
         update_smart_learn(m_idx, DRS_FIRE);
         break;

      case RF5_BA_COLD:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a frost ball.", m_name);
         breath(m_idx, GF_COLD,
               randint(rlev * 3 / 2) + 10);
         update_smart_learn(m_idx, DRS_COLD);
         break;

      case RF5_BA_POIS:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a stinking cloud.", m_name);
         breath(m_idx, GF_POIS,
               damroll(12, 2));
         update_smart_learn(m_idx, DRS_POIS);
         break;

      case RF5_BA_NETH:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a nether ball.", m_name);
         breath(m_idx, GF_NETHER,
               (50 + damroll(10, 10) + rlev));
         update_smart_learn(m_idx, DRS_NETH);
         break;

      case RF5_BA_WATE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s gestures fluidly.", m_name);
         msg_print("You are engulfed in a whirlpool.");
         breath(m_idx, GF_WATER,
               randint(rlev * 5 / 2) + 50);
         break;

      case RF5_BA_MANA:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles powerfully.", m_name);
         else msg_format("%^s invokes a mana storm.", m_name);
         breath(m_idx, GF_MANA,
               (rlev * 5) + damroll(10, 10));
         break;

      case RF5_BA_DARK:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles powerfully.", m_name);
         else msg_format("%^s invokes a darkness storm.", m_name);
         breath(m_idx, GF_DARK,
               (rlev * 5) + damroll(10, 10));
         update_smart_learn(m_idx, DRS_DARK);
         break;

      case RF5_DRAIN_MANA:
         if (p_ptr->csp)
         {
            s16b r1;

            /* Disturb if legal */
            disturb(1, 0);

            /* Basic message */
            msg_format("%^s draws psychic energy from you!", m_name);

            /* Attack power */
            r1 = (randint(rlev) / 2) + 1;

            /* Full drain */
            if (r1 >= p_ptr->csp)
            {
               r1 = p_ptr->csp;
               p_ptr->csp = 0;
               p_ptr->csp_frac = 0;
            }

            /* Partial drain */
            else
            {
               p_ptr->csp -= r1;
            }

            /* Redraw mana */
            p_ptr->redraw1 |= (PR1_MANA);

            /* Heal the monster */
            if (m_ptr->hp < m_ptr->maxhp)
            {
               /* Heal */
               m_ptr->hp += (6 * r1);
               if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

               /* Redraw (later) if needed */
               if (health_who == m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

               /* Special message */
               if (seen)
               {
                  msg_format("%^s appears healthier.", m_name);
               }
            }
         }
         update_smart_learn(m_idx, DRS_MANA);
         break;

      case RF5_MIND_BLAST:
         disturb(1, 0);
         if (!seen)
         {
            msg_print("You feel something focusing on your mind.");
         }
         else
         {
            msg_format("%^s gazes deep into your eyes.", m_name);
         }

         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            msg_print("Your mind is blasted by psionic energy.");
            if (!p_ptr->resist_conf)
            {
               (void)set_confused(p_ptr->confused + rand_int(4) + 4);
            }
            take_hit(damroll(8, 8), ddesc);
         }
         break;

      case RF5_BRAIN_SMASH:
         disturb(1, 0);
         if (!seen)
         {
            msg_print("You feel something focusing on your mind.");
         }
         else
         {
            msg_format("%^s looks deep into your eyes.", m_name);
         }
         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            msg_print("Your mind is blasted by psionic energy.");
            take_hit(damroll(12, 15), ddesc);
            if (!p_ptr->resist_blind)
            {
               (void)set_blind(p_ptr->blind + 8 + rand_int(8));
            }
            if (!p_ptr->resist_conf)
            {
               (void)set_confused(p_ptr->confused + rand_int(4) + 4);
            }
            if (!p_ptr->free_act)
            {
               (void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
            }
            (void)set_slow(p_ptr->slow + rand_int(4) + 4);
         }
         break;

      case RF5_CAUSE_1:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s points at you and curses.", m_name);
         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            take_hit(damroll(3, 8), ddesc);
         }
         break;

      case RF5_CAUSE_2:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s points at you and curses horribly.", m_name);
         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            take_hit(damroll(8, 8), ddesc);
         }
         break;

      case RF5_CAUSE_3:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles loudly.", m_name);
         else msg_format("%^s points at you, incanting terribly!", m_name);
         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            take_hit(damroll(10, 15), ddesc);
         }
         break;

      case RF5_CAUSE_4:
         disturb(1, 0);
         if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
         else msg_format("%^s points at you, screaming the word DIE!", m_name);
         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            take_hit(damroll(15, 15), ddesc);
            (void)set_cut(p_ptr->cut + damroll(10, 10));
         }
         break;

      case RF5_BO_ACID:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a acid bolt.", m_name);
         bolt(m_idx, GF_ACID,
             damroll(7, 8) + (rlev / 3));
         update_smart_learn(m_idx, DRS_ACID);
         break;

      case RF5_BO_ELEC:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a lightning bolt.", m_name);
         bolt(m_idx, GF_ELEC,
             damroll(4, 8) + (rlev / 3));
         update_smart_learn(m_idx, DRS_ELEC);
         break;

      case RF5_BO_FIRE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a fire bolt.", m_name);
         bolt(m_idx, GF_FIRE,
             damroll(9, 8) + (rlev / 3));
         update_smart_learn(m_idx, DRS_FIRE);
         break;

      case RF5_BO_COLD:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a frost bolt.", m_name);
         bolt(m_idx, GF_COLD,
             damroll(6, 8) + (rlev / 3));
         update_smart_learn(m_idx, DRS_COLD);
         break;

      case RF5_BO_POIS:
         break;

      case RF5_BO_NETH:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a nether bolt.", m_name);
         bolt(m_idx, GF_NETHER,
             30 + damroll(5, 5) + (rlev * 3) / 2);
         update_smart_learn(m_idx, DRS_NETH);
         break;

      case RF5_BO_WATE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a water bolt.", m_name);
         bolt(m_idx, GF_WATER,
             damroll(10, 10) + (rlev));
         break;

      case RF5_BO_MANA:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a mana bolt.", m_name);
         bolt(m_idx, GF_MANA,
             randint(rlev * 7 / 2) + 50);
         break;

      case RF5_BO_PLAS:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a plasma bolt.", m_name);
         bolt(m_idx, GF_PLASMA,
             10 + damroll(8, 7) + (rlev));
         break;

      case RF5_BO_ICEE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts an ice bolt.", m_name);
         bolt(m_idx, GF_ICE,
             damroll(6, 6) + (rlev));
         update_smart_learn(m_idx, DRS_COLD);
         break;

      case RF5_MISSILE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a magic missile.", m_name);
         bolt(m_idx, GF_MISSILE,
             damroll(2, 6) + (rlev / 3));
         break;

      case RF5_SCARE:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
         else msg_format("%^s casts a fearful illusion.", m_name);
         if (p_ptr->resist_fear)
         {
            msg_print("You refuse to be frightened.");
         }
         else if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You refuse to be frightened.");
         }
         else
         {
            (void)set_afraid(p_ptr->afraid + rand_int(4) + 4);
         }
         update_smart_learn(m_idx, DRS_FEAR);
         break;

      case RF5_BLIND:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s casts a spell, burning your eyes!", m_name);
         if (p_ptr->resist_blind)
         {
            msg_print("You are unaffected!");
         }
         else if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            (void)set_blind(12 + rand_int(4));
         }
         update_smart_learn(m_idx, DRS_BLIND);
         break;

      case RF5_CONF:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
         else msg_format("%^s creates a mesmerising illusion.", m_name);
         if (p_ptr->resist_conf)
         {
            msg_print("You disbelieve the feeble spell.");
         }
         else if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You disbelieve the feeble spell.");
         }
         else
         {
            (void)set_confused(p_ptr->confused + rand_int(4) + 4);
         }
         update_smart_learn(m_idx, DRS_CONF);
         break;

      case RF5_SLOW:
         disturb(1, 0);
         msg_format("%^s drains power from your muscles!", m_name);
         if (p_ptr->free_act)
         {
            msg_print("You are unaffected!");
         }
         else if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            (void)set_slow(p_ptr->slow + rand_int(4) + 4);
         }
         update_smart_learn(m_idx, DRS_FREE);
         break;

      case RF5_HOLD:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s stares deep into your eyes!", m_name);
         if (p_ptr->free_act)
         {
            msg_print("You are unaffected!");
         }
         else if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_format("You resist the effects!");
         }
         else
         {
            (void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
         }
         update_smart_learn(m_idx, DRS_FREE);
         break;
   }
}

/*
 * this make a monster execute a spell from r_info[].flags6
 */
static void make_attack_spell6(s16b m_idx, u64b thrown_spell)
{
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
   char       m_name[80];
   char       m_poss[80];
   char       ddesc[80];
   bool       blind = (p_ptr->blind ? TRUE : FALSE);
   bool       seen = (!blind && m_ptr->ml);
   s16b       rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
   s16b       k, count = 0;
   s16b       x = px, y = py;

   /* Get the monster name (or "it") */
   monster_desc(m_name, m_ptr, 0x00);

   /* Get the monster possessive ("his"/"her"/"its") */
   monster_desc(m_poss, m_ptr, 0x22);

   /* Hack -- Get the "died from" name */
   monster_desc(ddesc, m_ptr, 0x88);

   /* Cast the spell. */
   switch (thrown_spell)
   {
      case RF6_HASTE:
         disturb(1, 0);
         if (blind)
         {
            msg_format("%^s mumbles.", m_name);
         }
         else
         {
            msg_format("%^s concentrates on %s body.", m_name, m_poss);
         }

         /* Allow quick speed increases to base+10 */
         if (m_ptr->mspeed < r_ptr->speed + 10)
         {
            msg_format("%^s starts moving faster.", m_name);
            m_ptr->mspeed += 10;
         }

         /* Allow small speed increases to base+20 */
         else if (m_ptr->mspeed < r_ptr->speed + 20)
         {
            msg_format("%^s starts moving faster.", m_name);
            m_ptr->mspeed += 2;
         }

         break;

      case RF6_HEAL:
         disturb(1, 0);
         /* Message */
         if (blind)
         {
            msg_format("%^s mumbles.", m_name);
         }
         else
         {
            msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
         }

  /* Heal some */
         m_ptr->hp += (rlev * 6);

         /* Fully healed */
         if (m_ptr->hp >= m_ptr->maxhp)
         {
            /* Fully healed */
            m_ptr->hp = m_ptr->maxhp;

            /* Message */
            if (seen)
            {
               msg_format("%^s looks REALLY healthy!", m_name);
            }
            else
            {
               msg_format("%^s sounds REALLY healthy!", m_name);
            }
         }

         /* Partially healed */
         else
         {
            /* Message */
            if (seen)
            {
               msg_format("%^s looks healthier.", m_name);
            }
            else
            {
               msg_format("%^s sounds healthier.", m_name);
            }
         }

         /* Redraw (later) if needed */
         if (health_who == m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

         /* Cancel fear */
         if (m_ptr->afraid)
         {
            /* Cancel fear */
            m_ptr->afraid = 0;

            /* Message */
            if (fear_messages)
               msg_format("%^s recovers %s courage.", m_name, m_poss);
         }

         break;

      case RF6_BLINK:
         disturb(1, 0);
         msg_format("%^s blinks away.", m_name);
         teleport_away(m_idx, 10);
         break;

      case RF6_TPORT:
         disturb(1, 0);
         msg_format("%^s teleports away.", m_name);
         teleport_away(m_idx, MAX_SIGHT * 2 + 5);
         break;

      case RF6_TELE_TO:
         disturb(1, 0);
         msg_format("%^s commands you to return.", m_name);
         teleport_player_to(m_ptr->fx, m_ptr->fy);
         break;

      case RF6_TELE_AWAY:
         disturb(1, 0);
         msg_format("%^s teleports you away.", m_name);
         teleport_player(67*RATIO);
         break;

      case RF6_TELE_LEVEL:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles strangely.", m_name);
         else msg_format("%^s gestures at your feet.", m_name);
         if (p_ptr->resist_nexus)
         {
            msg_print("You are unaffected!");
         }
         else if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else
         {
            teleport_player_level();
         }
         update_smart_learn(m_idx, DRS_NEXUS);
         break;

      case RF6_DARKNESS:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s gestures in shadow.", m_name);
         (void)unlite_area(0, 3);
         break;

      case RF6_TRAPS:
      {
         project_who_type who;
         disturb(1, 0);
         msg_format("%^s %s cackles evilly.", m_name, blind?"mumbles something and":"casts a spell and");
         who.type = WHO_MONSTER;
         (void)trap_creation(&who, px, py);
         break;
      }
      case RF6_FORGET:
         disturb(1, 0);
         msg_format("%^s tries to blank your mind.", m_name);

         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
         }
         else if (lose_all_info())
         {
            msg_print("Your memories fade away.");
         }
         break;

      case RF6_S_TROLLS:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons trolls!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_TROLL, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear many things appear nearby.");
         break;

      case RF6_S_MONSTER:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons help!", m_name);
         for (k = 1; k > 0; k--)
         {
            count += summon_specific(x, y, rlev, 0, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear something appear nearby.");
         break;

      case RF6_S_MONSTERS:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons monsters!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, 0, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear many things appear nearby.");
         break;

      case RF6_S_ANT:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons ants.", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_ANT, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear many things appear nearby.");
         break;

      case RF6_S_SPIDER:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons spiders.", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_SPIDER, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear many things appear nearby.");
         break;

      case RF6_S_HOUND:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons hounds.", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_HOUND, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear many things appear nearby.");
         break;

      case RF6_S_HYDRA:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons hydras.", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_HYDRA, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear many things appear nearby.");
         break;

      case RF6_S_ANGEL:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons an angel!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_ANGEL, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear something appear nearby.");
         break;

      case RF6_S_DEMON:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons a hellish adversary!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_DEMON, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear something appear nearby.");
         break;

      case RF6_S_UNDEAD:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons an undead adversary!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_UNDEAD, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear something appear nearby.");
         break;

      case RF6_S_DRAGON:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons a dragon!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_DRAGON, summon_number(m_idx));
         }
         if (blind && count) msg_print("You hear something appear nearby.");
         break;

      case RF6_S_HI_UNDEAD:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons greater undead!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_HI_UNDEAD, summon_number(m_idx));
         }
         if (blind && count)
         {
            msg_print("You hear many creepy things appear nearby.");
         }
         break;

      case RF6_S_HI_DRAGON:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons ancient dragons!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_HI_DRAGON, summon_number(m_idx));
         }
         if (blind && count)
         {
            msg_print("You hear many powerful things appear nearby.");
         }
         break;

      case RF6_S_WRAITH:
         disturb(1, 0);
         if (blind) msg_format("%^s mumbles.", m_name);
         else msg_format("%^s magically summons mighty undead opponents!", m_name);
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_WRAITH, summon_number(m_idx));
         }
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_HI_UNDEAD, summon_number(m_idx));
         }
         if (blind && count)
         {
            msg_print("You hear many creepy things appear nearby.");
         }
         break;

      case RF6_S_UNIQUE:
         disturb(1, 0);
         /* don't summon uniques in the arena! */
         if (dungeon.level[sublevel][m_ptr->fy][m_ptr->fx].fdat & CAVE_AREN)
         {
            msg_print("%^s shrieks against the Arena Master!");
         }
         else
         {
            if (blind)
               msg_format("%^s mumbles.", m_name);
            else
               msg_format("%^s magically summons special opponents!", m_name);
            for (k = summon_number(m_idx); k > 0; k--)
            {
               count += summon_specific(x, y, rlev, SUMMON_UNIQUE, summon_number(m_idx));
            }
         }
         for (k = summon_number(m_idx); k > 0; k--)
         {
            count += summon_specific(x, y, rlev, SUMMON_HI_UNDEAD, summon_number(m_idx));
         }
         if (blind && count)
         {
            msg_print("You hear many powerful things appear nearby.");
         }
         break;
   }
}

/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind. See also "effects.c" for other "mistakes".
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * Note that, to allow the use of the "track_target" option at some
 * later time, certain non-optimal things are done in the code below,
 * including explicit checks against the "direct" variable, which is
 * currently always true by the time it is checked, but which should
 * really be set according to an explicit "projectable()" test, and
 * the use of generic "x,y" locations instead of the player location,
 * with those values being initialized with the player location.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that certain spell attacks do not use the "project()" function
 * but "simulate" it via the "direct" variable, which is always at least
 * as restrictive as the "project()" function.  This is necessary to
 * prevent "blindness" attacks and such from bending around walls, etc,
 * and to allow the use of the "track_target" option in the future.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 */
bool make_attack_spell(s16b m_idx)
{
   s16b            k, chance, thrown_spell, rlev, num = 0;
   cast_spell_type spells[96]; /* 3 times 32 different spells maximum */
   u64b            f4, f5, f6;

   monster_type  *m_ptr = &mn_list[m_idx];
   monster_race  *r_ptr = &r_info[m_ptr->r_idx];

   char       m_name[80];
   char       m_poss[80];

   char       ddesc[80];

   bool       no_inate = FALSE;

   /* Extract the blind-ness */
   bool       blind = (p_ptr->blind ? TRUE : FALSE);

   /* Extract the "see-able-ness" */
   bool       seen = (!blind && m_ptr->ml);

   /* Assume "normal" target */
   bool       normal = TRUE;

   /* Hack -- Extract the spell probability */
   chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;
dlog(DEBUGMONAI,"melee2.c: make_attack_spell: chance %d\n", chance);

   /* Not allowed to cast spells */
   if (!chance) return (FALSE);

   /* Cannot cast spells when confused */
   if (m_ptr->confused) return (FALSE);

   if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

   /* Only do spells occasionally */
   if (rand_int(100) >= chance) return (FALSE);

   /* Sometimes forbid inate attacks (breaths) */
   if (rand_int(100) >= (chance * 2)) no_inate = TRUE;

   /* XXX XXX XXX Handle "track_target" option (?) */

  /* Hack -- require projectable player */
   if (normal)
   {
      /* Check range */
      if (m_ptr->cdis > MAX_RANGE) return (FALSE);

      /* Check path */
      if (!projectable(m_ptr->fx, m_ptr->fy, px, py)) return (FALSE);
   }

   /* Extract the monster level */
   rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

   /* Extract the racial spell flags */
   f4 = r_ptr->flags4;
   f5 = r_ptr->flags5;
   f6 = r_ptr->flags6;

   /* Forbid inate attacks sometimes */
   if (no_inate) f4 = 0L;
dlog(DEBUGMONAI,"melee2.c: make_attack_spell: chance granted\n");

   /* Hack -- allow "desperate" spells */
   if ((r_ptr->flags2 & RF2_SMART) &&
      (m_ptr->hp < m_ptr->maxhp / 10) &&
      (rand_int(100) < 50))
   {
      /* Require intelligent spells */
      f4 &= RF4_INT_MASK;
      f5 &= RF5_INT_MASK;
      f6 &= RF6_INT_MASK;

      /* No spells left */
      if (!f4 && !f5 && !f6) return (FALSE);
   }

#ifdef DRS_SMART_OPTIONS

   /* Remove the "ineffective" spells */
   remove_bad_spells(m_idx, &f4, &f5, &f6);

   /* No spells left */
   if (!f4 && !f5 && !f6) return (FALSE);

#endif
   /* Check for a clean bolt shot */
   if ((f4&(RF4_BOLT_MASK) || f5 & (RF5_BOLT_MASK) ||
       f6&(RF6_BOLT_MASK))
    && !(r_ptr->flags2 & (RF2_STUPID)) &&
      !clean_shot(m_ptr->fx, m_ptr->fy, px, py, 0))
   {
      /* Remove spells that will only hurt friends */
      f4 &= ~(RF4_BOLT_MASK);
      f5 &= ~(RF5_BOLT_MASK);
      f6 &= ~(RF6_BOLT_MASK);
   }

   /* Check for a possible summon */
   if ((f4 & (RF4_SUMMON_MASK) || f5 & (RF5_SUMMON_MASK) ||
      f6 & (RF6_SUMMON_MASK)) &&
      !(r_ptr->flags2 & (RF2_STUPID)) &&
      !(summon_possible(px,py)))
   {
      /* Remove summoning spells */
      f4 &= ~(RF4_SUMMON_MASK);
      f5 &= ~(RF5_SUMMON_MASK);
      f6 &= ~(RF6_SUMMON_MASK);
   }

   /* No spells left */
   if (!f4 && !f5 && !f6) return (FALSE);

dlog(DEBUGMONAI,"melee2.c: make_attack_spell: chance granted, left %08lx %08lx %08lx\n", f4, f5, f6);

   /* Extract the "inate" spells */
   for (k = 0; k < 64; k++)
   {
      if (f4 & (1L << k))
      {
         spells[num].flag = 4;
         spells[num++].spell = (1L << k);
      }
   }

   /* Extract the "normal" spells */
   for (k = 0; k < 64; k++)
   {
      if (f5 & (1L << k))
      {
         spells[num].flag = 5;
         spells[num++].spell = (1L << k);
      }
   }

   /* Extract the "bizarre" spells */
   for (k = 0; k < 64; k++)
   {
      if (f6 & (1L << k))
      {
         spells[num].flag = 6;
         spells[num++].spell = (1L << k);
      }
   }

   /* No spells left */
   if (!num) return (FALSE);
dlog(DEBUGMONAI,"melee2.c: make_attack_spell: chance granted, %d spells left\n", num);
   for (k=0; k < num; k++)
   {
dlog(DEBUGMONAI,"melee2.c: make_attack_spell: chance granted, spell %d = flag %d spell %16Lx\n",
                k, spells[k].flag, spells[k].spell);
   }

   /* Stop if player is dead or gone */
   if (!alive || death || new_level_flag) return (FALSE);

   /* Get the monster name (or "it") */
   monster_desc(m_name, m_ptr, 0x00);

   /* Get the monster possessive ("his"/"her"/"its") */
   monster_desc(m_poss, m_ptr, 0x22);

   /* Hack -- Get the "died from" name */
   monster_desc(ddesc, m_ptr, 0x88);

   if (r_ptr->flags2 & RF2_STUPID)
   {
      /* Choose a spell to cast */

      thrown_spell = rand_int(num);
   }
   else
   {
      s16b failrate;

      thrown_spell = choose_attack_spell(m_idx, spells, num);

dlog(DEBUGMONAI,"melee2.c: make_attack_spell: choose_attack_spell recommended %d\n", thrown_spell);

      /* Abort if no spell was chosen */
      if (thrown_spell == -1) return (FALSE);

      /* Calculate spell failure rate */
      failrate = 25 - (rlev + 3) / 4;

      /* Hack -- Stupid monsters will never fail (for jellies and such) */
      if (r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

      /* Check for spell failure (inate attacks never fail) */
      if ((thrown_spell >= 128) && (rand_int(100) < failrate))
      {
         /* Message */
         msg_format("%^s tries to cast a spell, but fails.", m_name);

         return (TRUE);
      }
   }
dlog(DEBUGMONAI,"melee2.c: make_attack_spell: using spell %d\n", thrown_spell);

   switch (spells[thrown_spell].flag)
   {
      case 4: make_attack_spell4(m_idx, spells[thrown_spell].spell);
              break;
      case 5: make_attack_spell5(m_idx, spells[thrown_spell].spell);
              break;
      case 6: make_attack_spell6(m_idx, spells[thrown_spell].spell);
              break;
      default: msg_format("monster %d (%s) @ %d,%d tried unknown spell %d flag %d spell %16Lx",
                          m_idx, r_name + r_ptr->name, m_ptr->fx, m_ptr->fy, thrown_spell, 
                          spells[thrown_spell].flag, spells[thrown_spell].spell);
   }

   /* Remember what the monster did to us */
   if (seen)
   {
      /* Inate spell */
      if (spells[thrown_spell].flag == 4)
      {
         r_ptr->r_flags4 |= spells[thrown_spell].spell;
         if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
      }

      /* Bolt or Ball */
      if (spells[thrown_spell].flag == 5)
      {
         r_ptr->r_flags5 |= spells[thrown_spell].spell;
         if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
      }

      /* Special spell */
      if (spells[thrown_spell].flag == 6)
      {
         r_ptr->r_flags6 |= spells[thrown_spell].spell;
         if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
      }
   }


   /* Always take note of monsters that kill you */
   if (death && (r_ptr->r_deaths < MAX_SHORT)) r_ptr->r_deaths++;


   /* A spell was cast */
   return (TRUE);
}

/*
 * jk - to give summoners a natural dislike to narrow corridors
 */
s16b count_walls(s16b x, s16b y)
{
   s16b result = 8;
   if (in_bounds(x-1,y-1)) result -=floor_grid_bold(x-1,y-1);
   if (in_bounds(x-1,y))   result -=floor_grid_bold(x-1,y);
   if (in_bounds(x-1,y+1)) result -=floor_grid_bold(x-1,y+1);

   if (in_bounds(x+1,y-1)) result -=floor_grid_bold(x+1,y-1);
   if (in_bounds(x+1,y))   result -=floor_grid_bold(x+1,y);
   if (in_bounds(x+1,y+1)) result -=floor_grid_bold(x+1,y+1);

   if (in_bounds(x,y-1))   result -=floor_grid_bold(x,y-1);
   if (in_bounds(x,y+1))   result -=floor_grid_bold(x,y+1);
   return (result);
}

/*
 * jk - to give summoners a natural dislike to narrow corridors
 */
s16b count_walkables(s16b x, s16b y)
{
   s16b result = 0;
   if (in_bounds(x-1,y-1)) result +=grid_walkable(x-1,y-1);
   if (in_bounds(x-1,y))   result +=grid_walkable(x-1,y);
   if (in_bounds(x-1,y+1)) result +=grid_walkable(x-1,y+1);

   if (in_bounds(x+1,y-1)) result +=grid_walkable(x+1,y-1);
   if (in_bounds(x+1,y))   result +=grid_walkable(x+1,y);
   if (in_bounds(x+1,y+1)) result +=grid_walkable(x+1,y+1);

   if (in_bounds(x,y-1))   result +=grid_walkable(x,y-1);
   if (in_bounds(x,y+1))   result +=grid_walkable(x,y+1);
   return (result);
}

/*
 * this function finds out how many squares next to the player are
 * empty and viewable from the monster. If you are in a narrow
 * tunnel, that should be one. If you are in the open or at the entrance
 * of a room, it's 3:
 *
 * ......1XXXX
 * .Z....2@...
 * ......3XXXX
 */
s16b count_walkables_in_los(s16b sx, s16b sy, s16b tx, s16b ty)
{
   /* coords relative to px, py given the direction of the player from the monster */
   static s16b opposite [10] =
   {
      0,      /* no direction */
      DIR_NE, /* SW */
      DIR_N,  /* S */
      DIR_NW, /* SE */ 
      DIR_W,  /* E */
      0,      /* no direction */
      DIR_E,  /* W */
      DIR_SE, /* NW */
      DIR_S,  /* N */
      DIR_SW  /* NE */
   };

   s16b dir,i,result,x,y;

   dir=what_dir(sx, sy, tx, ty);
   result=0;

   for (i=1; i<10; i++)
   {
      if ((i == 5) || (i == opposite[dir])) continue;

      x=tx+ddx[i];
      y=ty+ddy[i];
      if (in_bounds(x, y) && los(sx, sy, x, y))
      {
         result +=grid_walkable(x, y);
      }
   }
   return (result);
}

/*
 * are we at the entrance to a room seen from x,y?
 */
s16b player_next_to_room(s16b x,s16b y)
{
   s16b dir;

   dir=what_dir(x, y, px, py);

   return (dungeon.level[sublevel][py+ddy[dir]][px+ddx[dir]].fdat & CAVE_ROOM);
}

/*
 * helper function for find_exits
 */
void look_exits_updown(s16b x, s16b y, s16b *num_found, bool can_use_doors, s16b dy)
{
   cave_cell_type *c_ptr;

   while (in_bounds(x, y))
   {
      c_ptr = &dungeon.level[sublevel][y][x];

      /* we've met a wall - exit now */
      /* this doesn't like inner pillars in a room, but it does */
      /* the right thing with inner walls                       */
      if (is_wall_ptr(c_ptr))
      {
         break;
      }
      /* we found a door */
      if (c_ptr->mtyp == DUNG_DOOR)
      {
         /* we cannot open doors and found a closed one. No exit */
         if (!can_use_doors && ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
                                 (c_ptr->styp == DUNG_DOOR_JAMMED) ||
                                 (c_ptr->styp == DUNG_DOOR_LOCKED) ) )
         {
            break;
         }
         /* fall through to the case below: the door shouldn't be in the room */
         /* anymore for it to be considered an exit.                          */
      }

      /* we are not in the room anymore - this is an exit */
      if (!is_room(x, y))
      {
         if ( in_bounds(x,y+dy) && (!is_wall(x,y+dy)) )
         {
            tmp_x[(*num_found)] = x;
            tmp_y[(*num_found)] = y;
            (*num_found) = (*num_found)+1;
            break;
         }
      }
      /* go further */
      y = y + dy;
   }
   return;
}

/*
 * helper function for find_exits
 */
void look_exits_leftright(s16b x, s16b y, s16b *num_found, bool can_use_doors, s16b dx)
{
   cave_cell_type *c_ptr;

   while (in_bounds(x, y))
   {
      c_ptr = &dungeon.level[sublevel][y][x];

      /* we've met a wall - exit now */
      /* this doesn't like inner pillars in a room, but it does */
      /* the right thing with inner walls                       */
      if (is_wall_ptr(c_ptr))
      {
         break;
      }

      /* we found a door */
      if (c_ptr->mtyp == DUNG_DOOR)
      {
         /* we cannot open doors and found a closed one. No exit */
         if (!can_use_doors && ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
                                 (c_ptr->styp == DUNG_DOOR_JAMMED) ||
                                 (c_ptr->styp == DUNG_DOOR_LOCKED) ) )
         {
            break;
         }
         /* fall through to the case below: the door shouldn't be in the room */
         /* anymore for it to be considered an exit.                          */
      }

      /* we are not in the room anymore - this is an exit if the next square */
      /* in the same direction is not a wall                                 */
      if (!is_room(x, y))
      {
         if ( in_bounds(x+dx,y) && (!is_wall(x+dx,y)))
         {
            tmp_x[(*num_found)] = x;
            tmp_y[(*num_found)] = y;
            (*num_found) = (*num_found)+1;
            break;
         }
      }

      /* go further */
      x=x+dx;
   }
   return;
}

/*
 * helper function for find_exits
 */
void find_exits_leftright(s16b x, s16b y, s16b *num_found, bool can_use_doors, s16b dx)
{
   /* this will be confused by passable walls within the room, like pillars */
   while (in_bounds(x, y) && !is_wall(x, y))
   {
      /* now first up */
      look_exits_updown(x, y-1, num_found, can_use_doors, -1);
      /* and then down */
      look_exits_updown(x, y+1, num_found, can_use_doors, 1);
      x=x+dx;
   }
   return;
}

/*
 * helper function for find_exits
 */
void find_exits_updown(s16b x, s16b y, s16b *num_found, bool can_use_doors, s16b dy)
{
   /* this will be confused by passable walls within the room, like pillars */
   while (in_bounds(x, y) && !is_wall(x, y))
   {
      /* now first left */
      look_exits_leftright(x-1, y, num_found, can_use_doors, -1);
      /* and then right */
      look_exits_leftright(x+1, y, num_found, can_use_doors, 1);
      y=y+dy;
   }
   return;
}

/*
 * this function gives the coordinates of places in / next to the room that are not in LOS of the
 * player and are exits from the room.
 *
 * it returns the number of locations, and their coordinates in tmp_x, tmp_y
 * in tmp_i, it returns the shortest distance to the player along the path to this exit.
 *
 * the routine works like this:
 * walk left from the player and look up/down if there is a square that isn't a wall and hasn't got
 * the CAVE_ROOM flag set. If the monster can open/kill doors, a door also qualifies, of course.
 * if such a square is found, it is added to the list of exits.
 * after that, walk right, then walk up and down while looking left and right.
 *
 * this function will stop on any wall, even those within the room. This means monsters in
 * inner rooms will find the correct exit, but monsters in rooms with pillars will not always
 * find the correct exit.
 *
 * this may be a slow computation - optimizing it would be nice XXX XXX XXX XXX XXX
 * starting points for optimization: don't check squares twice.....
 */
s16b find_exits(s16b m_idx)
{
   s16b            num_found = 0;
   s16b            mx, my, i, j, path_n;
   u16b            path_g[512];
   monster_type   *m_ptr = &mn_list[m_idx];
   monster_race   *r_ptr = &r_info[m_ptr->r_idx];
   cave_cell_type *c_ptr;
   bool            can_use_doors = FALSE;


   mx=m_ptr->fx;
   my=m_ptr->fy;
dlog(DEBUGMONAI,"melee2.c: find_exits: starting for monster %d @ %d,%d\n", m_idx, mx, my);
   c_ptr = &dungeon.level[sublevel][my][mx];
   /* if the monster currently isn't in a room , just exit. */
   if ( !(dungeon.level[sublevel][my][mx].fdat & CAVE_ROOM))
   {
dlog(DEBUGMONAI,"melee2.c: find_exits: monster not in room\n");
      return 0;
   }

   /* can the monster do something with (known) doors? */
   if ( (r_ptr->flags2 & RF2_OPEN_DOOR) ||(r_ptr->flags2 & RF2_BASH_DOOR) )
   {
      can_use_doors = TRUE;
   }

   /* now find out where the exits from this room are */
   /* up, down, left, right */
   find_exits_updown(mx, my, &num_found, can_use_doors, -1);
   find_exits_updown(mx, my, &num_found, can_use_doors, 1);
   find_exits_leftright(mx, my, &num_found, can_use_doors, -1);
   find_exits_leftright(mx, my, &num_found, can_use_doors, 1);

   /* now trace how close the route to each of the exits passes the player */
   for (i=0; i < num_found; i++)
   {
      /* Calculate the projection path, no flags */
      path_n = project_path(path_g, 99, mx, my, tmp_x[i], tmp_y[i], 0x0);

      tmp_i[i]=9999;

      /* Project along the path */
      for (j = 0; j < path_n; j++)
      {
         s16b nx = GRID_X(path_g[j]);
         s16b ny = GRID_Y(path_g[j]);
         s16b dist = distance(px, py, nx, ny);

         if (dist < tmp_i[i]) tmp_i[i] = dist;
      }
   }

   return (num_found);
}

/*
 * this function finds out how many identical monsters the current monster
 * can see within a certain range. If a group is large, it will be much more
 * likely to try to overwhelm the player than if a group is small.
 *
 * it's likely that this function will slow things down when range is too big
 */
s16b count_seen_friends(s16b m_idx, s16b range)
{
   s16b                 mx, my, x, y, count;
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_type        *m_ptr2;
   cave_cell_type      *c_ptr;

   count=0;
   mx=m_ptr->fx;
   my=m_ptr->fy;

   for (x = (mx-range); x < (mx+range); x++)
   {
      for (y = (my-range); y < (my+range); y++)
      {
         if (!in_bounds(x, y)) continue;
         if ((x==mx) && (y==my)) continue; /* don't count myself */
         /* only if the monster can see each other */
         if (!los(mx, my, x, y)) continue;
         c_ptr = &dungeon.level[sublevel][y][x];
         if (c_ptr->m_idx)
         {
            m_ptr2 = &mn_list[c_ptr->m_idx];
            if ( (r_info[m_ptr->r_idx].d_char == r_info[m_ptr2->r_idx].d_char) &&
                 (r_info[m_ptr2->r_idx].flags1 & RF1_FRIENDS) && !(m_ptr2->afraid))
            {
               count++;
            }
         }
      }
   }
   return (count);
}

/*
 * some monsters don't like to leave the trees' protection
 */
s16b count_trees(s16b x, s16b y)
{
   s16b result = 0;
   if (in_bounds(x-1,y-1)) result += (dungeon.level[sublevel][y-1][x-1].mtyp == DUNG_SHRUB)?1:0;
   if (in_bounds(x,y-1))   result += (dungeon.level[sublevel][y-1][x].mtyp == DUNG_SHRUB)?1:0;
   if (in_bounds(x+1,y-1)) result += (dungeon.level[sublevel][y-1][x+1].mtyp == DUNG_SHRUB)?1:0;

   if (in_bounds(x-1,y+1)) result += (dungeon.level[sublevel][y+1][x-1].mtyp == DUNG_SHRUB)?1:0;
   if (in_bounds(x,y+1))   result += (dungeon.level[sublevel][y+1][x].mtyp == DUNG_SHRUB)?1:0;
   if (in_bounds(x+1,y+1)) result += (dungeon.level[sublevel][y+1][x+1].mtyp == DUNG_SHRUB)?1:0;

   if (in_bounds(x+1,y))   result += (dungeon.level[sublevel][y][x+1].mtyp == DUNG_SHRUB)?1:0;
   if (in_bounds(x-1,y))   result += (dungeon.level[sublevel][y][x-1].mtyp == DUNG_SHRUB)?1:0;
   return (result);
}


/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static s16b mon_will_run(s16b m_idx)
{
   monster_type *m_ptr = &mn_list[m_idx];

#ifdef ALLOW_TERROR

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   u16b p_lev, m_lev;
   u16b p_chp, p_mhp;
   u16b m_chp, m_mhp;
   u32b p_val, m_val;

#endif

   if (m_ptr->escaping) return (TRUE);

   /* Keep monsters from running too far away */
   if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

   /* All "afraid" monsters will run away */
   if (m_ptr->afraid) return (TRUE);

#ifdef ALLOW_TERROR

   /* Nearby monsters will not become terrified */
   if (m_ptr->cdis <= 5) return (FALSE);

   /* Examine player power (level) */
   p_lev = p_ptr->lev;

   /* Examine monster power (level plus morale) */
   m_lev = r_ptr->level + (m_idx & 0x08) + 25;

   /* Optimize extreme cases below */
   if (m_lev > p_lev + 4) return (FALSE);
   if (m_lev + 4 <= p_lev) return (TRUE);

   /* Examine player health */
   p_chp = p_ptr->chp;
   p_mhp = p_ptr->mhp;

   /* Examine monster health */
   m_chp = m_ptr->hp;
   m_mhp = m_ptr->maxhp;

   /* Prepare to optimize the calculation */
   p_val = (p_lev * p_mhp) + (p_chp << 2);     /* div p_mhp */
   m_val = (m_lev * m_mhp) + (m_chp << 2);     /* div m_mhp */

   /* Strong players scare strong monsters */
   if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

#endif

   /* Assume no terror */
   return (FALSE);
}

/*
 * this function tries to find the closest exit for a fleeing monster
 */
static bool find_closest_exit(s16b m_idx, s16b *exitx, s16b *exity)
{
   s16b          num_exits, exit, dist, minexitdist;
   monster_type *m_ptr = &mn_list[m_idx];
   bool          knows_exit = FALSE;

   /* XXX XXX XXX this stores coordinates in tmp_x, tmp_y !! */
   num_exits = find_exits(m_idx);

   minexitdist = 999;

dlog(DEBUGMONAI,"melee2.c: find_closest_exit: %d exits found\n", num_exits);
   for (exit=0; exit < num_exits; exit++)
   {
     dist = distance(m_ptr->fx, m_ptr->fy, tmp_x[exit], tmp_y[exit]);
dlog(DEBUGMONAI,"melee2.c: find_closest_exit: exit %d of %d @ %d,%d - dist from me %d min dist to pc %d mindist %d\n",
                exit, num_exits, tmp_x[exit], tmp_y[exit], dist, tmp_i[exit], minexitdist);

     /* is the minimum distance to the player in tmp_i >= the distance to this exit? */
     if (tmp_i[exit] <= dist)
     {
dlog(DEBUGMONAI,"melee2.c: find_closest_exit: exit @ %d,%d passes very close to the player\n", tmp_x[exit], tmp_y[exit]);
        continue;
     }
     /* XXX XXX some monsters should be able to flee to unseen exits */
     /* use monster_flow? use RF2_SMART? */
     if ( !los(m_ptr->fx, m_ptr->fy, tmp_x[exit], tmp_y[exit]) )
     {
dlog(DEBUGMONAI,"melee2.c: find_closest_exit: exit @ %d,%d not in LOS\n", tmp_x[exit], tmp_y[exit]);
        continue;
     }

     /* is this the closest exit yet? if so, save */
     if ( dist < minexitdist)
     {
        minexitdist = dist;
        (*exitx)=tmp_x[exit];
        (*exity)=tmp_y[exit];
        knows_exit=TRUE;
     }
  }
  return (knows_exit);
}

/*
 * this function evaluates the current move in mmove
 * to check if the monster can move physically there; 
 * can the monster open the door there, etc.
 * return FALSE if we cannot deal with it and this move should no be considered further
 */
static bool weed_moves_feature(monster_type *m_ptr, monster_move_type *mmove, s16b this_move)
{
   s16b nx, ny;
   cave_cell_type *c_ptr;
   monster_race   *r_ptr = &r_info[m_ptr->r_idx];

   nx = mmove[this_move].x;
   ny = mmove[this_move].y;
   c_ptr = &dungeon.level[sublevel][ny][nx];
/*
 * first we test if a square is possible at all (walls, doors & other monsters
 * could be in the way).
 */
   /* monsters that do not move, can not move.... */
   if ( r_ptr->flags1 & RF1_NEVER_MOVE)
   {
      /* we can "move" into the player and thus attack him */
      if ((nx != px) || (ny != py))
      {
         mmove[this_move].possible = FALSE;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d monster never moves\n", this_move, nx, ny);
         return FALSE;
      }
   }
   /* permanent walls are not open to anyone */
   if (c_ptr->mtyp == DUNG_PERWALL)
   {
      mmove[this_move].possible = FALSE;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d permanent wall\n", this_move, nx, ny);
      return FALSE;
   }

   /* can we move in wall squares */
   if (c_ptr->mtyp == DUNG_WALL)
   {
      if ( !(r_ptr->flags2 & RF2_PASS_WALL) &&
           !(r_ptr->flags2 & RF2_KILL_WALL))
      {
         if ((c_ptr->styp == DUNG_WALL_RUBBLE) && (randint(2)==1))
         {
            mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d inpassable wall\n", this_move, nx, ny);
            return FALSE;
         }
         else if (c_ptr->styp != DUNG_WALL_RUBBLE)
         {
            mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d inpassable wall\n", this_move, nx, ny);
            return FALSE;
         }

      }
      else if (r_ptr->flags2 & RF2_KILL_WALL)
      {
         mmove[this_move].result |= MOVE_KILL_WALL;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d killable wall\n", this_move, nx, ny);
      }
      else if (r_ptr->flags2 & RF2_PASS_WALL)
      {
         mmove[this_move].result |= MOVE_PASS_WALL;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d passable wall\n", this_move, nx, ny);
      }
   }

   /* can we move in lava squares */
   if (c_ptr->mtyp == DUNG_LAVA)
   {
      if ( r_ptr->flags3 & RF3_HURT_FIRE) mmove[this_move].suitable -= 750;
      if ( !(r_ptr->flags3 & RF3_IM_FIRE) ) mmove[this_move].suitable -= 500;
   }

   /* if we never hit, we can't try to move into the player! */
   if ( (ny == py) && (nx == px) &&
        (r_ptr->flags1 & RF1_NEVER_BLOW))
   {
      mmove[this_move].possible = FALSE;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d never attacks\n", this_move, nx, ny);
      return FALSE;
   }

   /* can we move in door squares */
   /* we can if we can either open or bash that door */
   if (test_grid_ptr(c_ptr, DUNG_DOOR, DUNG_DOOR_CLOSED))
   {
      if ( !(r_ptr->flags2 & RF2_OPEN_DOOR) ||
           !(r_ptr->flags2 & RF2_BASH_DOOR) )
      {
         mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unopenalble1 closed door\n", this_move, nx, ny);
         return FALSE;
      }
      else
      {
         /* is the monster willing to activate known traps? */
         if (rand_int(m_ptr->hp / 10) >
            ((num_traps_xy(nx,ny,TRAP_FOUND)?3:1)*c_ptr->extra))
         {
            if (r_ptr->flags2 & RF2_OPEN_DOOR)
            {
               mmove[this_move].result |= MOVE_OPEN_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d openable closed door\n", this_move, nx, ny);
            }
            else if (r_ptr->flags2 & RF2_BASH_DOOR)
            {
               mmove[this_move].result |= MOVE_BASH_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d bashable closed door\n", this_move, nx, ny);
            }
         }
         /* if we are unwilling to face the traps, do not go there */
         else
         {
            mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unopenable2 closed door\n", this_move, nx, ny);
            return FALSE;
         }
      }
   }
   /* can we move in locked door squares */
   if (test_grid_ptr(c_ptr, DUNG_DOOR, DUNG_DOOR_LOCKED))
   {
      /* if we cannot open doors and cannot bash them */
      /* or we are stupid, do not pass here */
      if ( ( !(r_ptr->flags2 & RF2_OPEN_DOOR) &&
             !(r_ptr->flags2 & RF2_BASH_DOOR) ) ||
           (r_ptr->flags2 & RF2_STUPID))
      {
         mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unopenable1 locked door\n", this_move, nx, ny);
         return FALSE;
      }
      else
      {
         /* is the monster willing to activate traps? */
         if (rand_int(m_ptr->hp / 10) >
            ((num_traps_xy(nx,ny,TRAP_FOUND)?3:1)*c_ptr->extra))
         {
            if (r_ptr->flags2 & RF2_OPEN_DOOR)
            {
               mmove[this_move].result |= MOVE_OPEN_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d openable locked door\n", this_move, nx, ny);
            }
            else if (r_ptr->flags2 & RF2_BASH_DOOR)
            {
               mmove[this_move].result |= MOVE_BASH_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d bashable locked door\n", this_move, nx, ny);
            }
         }
         else
         {
            mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unopenable2 locked door\n", this_move, nx, ny);
            return FALSE;
         }
      }
   }
   /* can we move in jammed door squares */
   /* intelligence not required :-) */
   if (test_grid_ptr(c_ptr, DUNG_DOOR, DUNG_DOOR_JAMMED))
   {
      /* if we cannot open doors and cannot bash them */
      /* or we are stupid, do not pass here */
      if ( !(r_ptr->flags2 & RF2_BASH_DOOR))
      {
         mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unpassable1 jammed door\n", this_move, nx, ny);
         return FALSE;
      }
      else
      {
         /* is the monster willing to activate traps? */
         if (rand_int(m_ptr->hp / 10) >
            ((num_traps_xy(nx,ny,TRAP_FOUND)?3:1)*c_ptr->extra))
         {
            mmove[this_move].result |= MOVE_BASH_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d bashable jammed door\n", this_move, nx, ny);
         }
         else
         {
            mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unpassable2 jammed door\n", this_move, nx, ny);
            return FALSE;
         }
      }
   }
   /* can we move in secret door squares */
   if (test_grid_ptr(c_ptr, DUNG_DOOR, DUNG_DOOR_SECRET))
   {
      /* if we cannot open doors and cannot bash them */
      /* or we are not smart, do not pass here */
      if ( ( !(r_ptr->flags2 & RF2_OPEN_DOOR) &&
             !(r_ptr->flags2 & RF2_BASH_DOOR) ) ||
             !(r_ptr->flags2 & RF2_SMART))
      {
         mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unopenable1 secret door\n", this_move, nx, ny);
         return FALSE;
      }
      else
      {
         /* is the monster willing to activate traps? */
         if (rand_int(m_ptr->hp / 10) >
            ((num_traps_xy(nx,ny,TRAP_FOUND)?3:1)*c_ptr->extra))
         {
            if (r_ptr->flags2 & RF2_OPEN_DOOR)
            {
               mmove[this_move].result |= MOVE_OPEN_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d openable secret door\n", this_move, nx, ny);
            }
            else if (r_ptr->flags2 & RF2_BASH_DOOR)
            {
               mmove[this_move].result |= MOVE_BASH_DOOR;
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d bashable secret door\n", this_move, nx, ny);
            }
         }
         else
         {
            mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d unopenable2 secret door\n", this_move, nx, ny);
            return FALSE;
         }
      }
   }
   /* do not move out of the arena if we are in it */
   /* nor in if we're out */
   if ( (dungeon.level[sublevel][m_ptr->fy][m_ptr->fx].fdat & CAVE_AREN) != (c_ptr->fdat & CAVE_AREN) )
   {
      mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_feature: sq %d @ %d,%d cannot enter or leave the arena\n", this_move, nx, ny);
      return FALSE;
   }
   return TRUE;
}

/*
 * see if we can deal with a monster occupying the move we could take
 * return FALSE if we cannot deal with it and this move should not be considered further
 */
static bool weed_moves_monster(monster_type *m_ptr, monster_move_type *mmove, s16b this_move)
{
   s16b nx, ny;
   cave_cell_type *c_ptr;
   monster_race  *z_ptr;
   monster_race  *r_ptr = &r_info[m_ptr->r_idx];

   nx = mmove[this_move].x;
   ny = mmove[this_move].y;
   c_ptr = &dungeon.level[sublevel][ny][nx];

   z_ptr = &r_info[mn_list[c_ptr->m_idx].r_idx];
dlog(DEBUGMONAI,"melee2.c: weed_moves_monster: sq %d @ %d,%d monster m_idx %d found kill %d move %d\n",
                 this_move, nx, ny, c_ptr->m_idx, 
                !(r_ptr->flags2 & RF2_MOVE_BODY), !(r_ptr->flags2 & RF2_KILL_BODY)  );
   /* if we cannot move or kill monsters, do not go here */
   if ( !(r_ptr->flags2 & RF2_MOVE_BODY) &&
        !(r_ptr->flags2 & RF2_KILL_BODY) )
   {
      mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_monster: sq %d @ %d,%d unable to displace monster m_idx %d\n",
          this_move, nx, ny, c_ptr->m_idx);
      return FALSE;
   }
   /* kill it if we are worth more than he! - uniques can't be killed this way! */
   if ((r_ptr->flags2 & RF2_KILL_BODY) &&
       (r_ptr->mexp > z_ptr->mexp) &&
       (!(z_ptr->flags1 & RF1_UNIQUE)))
   {
      mmove[this_move].result |= MOVE_KILL_BODY;
dlog(DEBUGMONAI,"melee2.c: weed_moves_monster: sq %d @ %d,%d killable monster\n", this_move, nx, ny);
   }
   /* if we must move it, and we don't come out of a wall, do so */
   else if ((r_ptr->flags2 & RF2_MOVE_BODY) &&
            (r_ptr->mexp > z_ptr->mexp) &&
            (floor_grid_bold(m_ptr->fx, m_ptr->fy)))
   {
      mmove[this_move].result |= MOVE_MOVE_BODY;
dlog(DEBUGMONAI,"melee2.c: weed_moves_monster: sq %d @ %d,%d m_idx %d moveable monster\n", this_move,
          dungeon.level[sublevel][ny][nx].m_idx, nx, ny);
   }
   /* suppose we have RF2_KILL_BODY or RF2_MOVE_BODY but the monster is */
   /* higher-level than we, we still cannot move there                  */
   else
   {
      mmove[this_move].possible = FALSE; /* we really cannot go there */
dlog(DEBUGMONAI,"melee2.c: weed_moves_monster: sq %d @ %d,%d unable to displace high-level monster m_idx %d\n",
          this_move, nx, ny, c_ptr->m_idx);
      return FALSE;
   }
dlog(DEBUGMONAI,"melee2.c: weed_moves_monster: sq %d @ %d,%d monster m_idx %d found, result %08x\n",
          mmove[this_move].result);
   return TRUE;
}

/*
 * this function determines if the monster moves randomly
 * and if so, skips this move if the dice roll true
 */
static bool weed_moves_random(monster_type *m_ptr, monster_move_type *mmove, s16b this_move)
{
   monster_race *r_ptr = &r_info[m_ptr->r_idx];
   s16b nx, ny;

   nx = mmove[this_move].x;
   ny = mmove[this_move].y;

/*
 * Now we test if the monster can move on it's own, or may skip random
 * squares
 */
   /* 75% random movement */
   if ((r_ptr->flags1 & RF1_RAND_50) &&
       (r_ptr->flags1 & RF1_RAND_25) &&
       (rand_int(100) < 75))
   {
      /* Memorize flags */
      if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50 | RF1_RAND_25);
dlog(DEBUGMONAI,"melee2.c: weed_moves_random: sq %d @ %d,%d 75% random\n", this_move, nx, ny);
      return FALSE;
   }
   /* 50% random movement */
   else if ((r_ptr->flags1 & RF1_RAND_50) && (rand_int(100) < 50))
   {
      /* Memorize flags */
      if (m_ptr->ml) r_ptr->r_flags1 |= RF1_RAND_50;
dlog(DEBUGMONAI,"melee2.c: weed_moves_random: sq %d @ %d,%d 50% random\n", this_move, nx, ny);
      return FALSE;
   }
   /* 25% random movement */
   else if ((r_ptr->flags1 & RF1_RAND_25) && (rand_int(100) < 25))
   {
      /* Memorize flags */
      if (m_ptr->ml) r_ptr->r_flags1 |= RF1_RAND_25;
dlog(DEBUGMONAI,"melee2.c: weed_moves_random: sq %d @ %d,%d 25% random\n", this_move, nx, ny);
      return FALSE;
   }
   return (TRUE);
}

/* can we annoy the player by crushing or picking up items along the way? */
static void pickup_possible(s16b m_idx, monster_move_type *mmove, s16b this_move, s16b objs)
{
   monster_type      *m_ptr = &mn_list[m_idx];
   monster_race      *r_ptr = &r_info[m_ptr->r_idx];
   /* are there any items to be crushed so as to annoy the player? */
   if (r_ptr->flags2 & RF2_KILL_ITEM)
   {
      /* found one? crush! crush! crush! */
      mmove[this_move].suitable += 50 + objs;
      mmove[this_move].result |= MOVE_KILL_ITEM;
   }

   /* are there any items to be picked up so as to annoy the player? */
   if (r_ptr->flags2 & RF2_TAKE_ITEM)
   {
      if (mn_list[m_idx].has_drop)
      {
         s16b is_idx, number;
         /* because of the need for speedy calculations, we don't check
          * if the monster inventory already has a similar object, we
          * just check if there is room for another object
          */
         is_idx = item_set_this_monster(m_idx);
         number = items_in_set(is_idx);
         if (number < ITEM_SET_SIZE)
         {
            mmove[this_move].suitable += 50+objs;
            mmove[this_move].result |= MOVE_TAKE_ITEM;
         }
      }
      else
      {
         /* the poor monster doesn't have any items yet... */
         mmove[this_move].suitable += 75 + objs;
         mmove[this_move].result |= MOVE_TAKE_ITEM;
      }
   }
}

/*
 * crushing a glyph along the way doesn't hurt either 
 */
static void test_kill_glyph(monster_type *m_ptr, monster_move_type *mmove, s16b this_move)
{
   s16b nx, ny;
   cave_cell_type *c_ptr;
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   nx = mmove[this_move].x;
   ny = mmove[this_move].y;
   c_ptr = &dungeon.level[sublevel][ny][nx];

   /* if it has a Glyph of Warding, we want especially to break it! */
   if (test_grid_ptr(c_ptr,DUNG_FLOOR,DUNG_FLOOR_GLYPH))
   {
      mmove[this_move].result |= MOVE_BREAK_GLYPH;
      /* can we break it easily? */
      if (BREAK_GLYPH/2 < (3*r_ptr->level))
      {
         /* then annoy the player by moving there */
         /* also making space for summons! */
         mmove[this_move].suitable += 1000;
dlog(DEBUGMONAI,"melee2.c: test_kill_glyph: sq %d @ %d,%d suit %d easy glyph\n", this_move, nx, ny, mmove[this_move].suitable);
      }
      else
      {
         mmove[this_move].suitable -= 200;
dlog(DEBUGMONAI,"melee2.c: test_kill_glyph: sq %d @ %d,%d suit %d glyph\n", this_move, nx, ny, mmove[this_move].suitable);
      }
   }
}

/*
 * this function returns 0 if the player is in good health (hp/mana)
 *                       1 if the player is troubled (hp < 50% or mana < 50%)
 *                       2 if the player is really troubled (hp < 25% or mana < 25%)
 * note that mana is only tested if the player has > 15 max mana to weed out warriors etc.
 * this could be considered cheating... but then, everybody knows the RNG is out to get you
 */
static s16b player_in_trouble(void)
{
   if (p_ptr->chp < (p_ptr->mhp / 4)) return (2);
   if ((p_ptr->msp > 15) && ( p_ptr->csp < (p_ptr->msp / 4)) ) return (2);
   if (p_ptr->chp < (p_ptr->mhp / 2)) return (1);
   if ((p_ptr->msp > 15) && ( p_ptr->csp < (p_ptr->msp / 2)) ) return (1);
   return (0);
}

/*
 * try to find out a logical move for this monster
 *
 * this function should probably be rewritten, if not for efficiency, for clarity.
 * there are three basic strategies for a monster:
 *
 * attack melee
 * attack distance
 * flee
 *
 * we should decide what the monster needs then call that option.
 */
static s16b find_moves(s16b m_idx)
{
   monster_type      *m_ptr = &mn_list[m_idx];
   monster_race      *r_ptr = &r_info[m_ptr->r_idx];
   cave_cell_type    *c_ptr;
   s16b               j, mindist, bestmoves[8], cnt, objs;
   s16b               this_move, blow;
   s16b               mx, my; /* current monster location */
   s16b               nx = 0, ny = 0; /* possible new monster location */
   s16b               exitx = 0, exity = 0;
   s16b               maxsuit, start, tactic, total_dam = 0, friends;
   bool               terror, afraid, in_arena, fleepack, knows_exit, find_exit, player_in_room, in_tunnel;

#if (debuglevel & DEBUGMONAI)
   char               mdesc[80];
#endif

   afraid = (m_ptr->afraid>0) ? TRUE : FALSE;
   terror = mon_will_run(m_idx);
   fleepack = FALSE;

   mx = m_ptr->fx;
   my = m_ptr->fy;
   /* paranoia! */
   if (!in_bounds(mx, my)) return (-1);

   mindist = -30000;
   in_arena = (dungeon.level[sublevel][my][mx].fdat & CAVE_AREN) ? TRUE : FALSE;
   player_in_room = (dungeon.level[sublevel][py][px].fdat & CAVE_ROOM) ? TRUE : FALSE;
   if (!player_in_room) player_in_room = player_next_to_room(mx, my);
   in_tunnel = (dungeon.level[sublevel][my][mx].fdat & CAVE_ROOM) ? FALSE : TRUE;

#if (debuglevel & DEBUGMONAI)
   monster_desc(mdesc, m_ptr, 0x88);
dlog(DEBUGMONAI,"melee2.c: find_moves: m_idx %d %s @ %d,%d player %d,%d pck %d afr %d terr %d arena %d\n",
                m_idx, mdesc, mx, my, px, py, fleepack, afraid, terror, in_arena);
#endif
dlog(DEBUGMONAI,"melee2.c: find_moves: m_idx %d @ monster, m_idx %d @ player\n",
                dungeon.level[sublevel][my][mx].m_idx, dungeon.level[sublevel][py][px].m_idx);

   mindist = 999;

   /* initialize all fields */
   for (this_move=7; this_move >= 0; this_move--)
   {
      mmove[this_move].x = mx + ddx_ddd[this_move];
      mmove[this_move].y = my + ddy_ddd[this_move];
      mmove[this_move].result = 0L;
      if (!in_bounds(mmove[this_move].x, mmove[this_move].y))
      {
         mmove[this_move].possible = FALSE;
      }
      else
      {
         mmove[this_move].dist = distance(px, py, mmove[this_move].x, mmove[this_move].y);
         if (mmove[this_move].dist < mindist) mindist = mmove[this_move].dist;
         mmove[this_move].suitable = 0;
         mmove[this_move].player_view = (dungeon.level[sublevel][mmove[this_move].y][mmove[this_move].x].fdat & CAVE_VIEW)?TRUE:FALSE;
         mmove[this_move].in_room = (dungeon.level[sublevel][mmove[this_move].y][mmove[this_move].x].fdat & CAVE_ROOM)?TRUE:FALSE;
         mmove[this_move].possible = TRUE;
      }
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d dist %d mindist now %d plr_vw %d m_idx %d room %d\n",
                this_move, mmove[this_move].x, mmove[this_move].y,
                mmove[this_move].dist, mindist, mmove[this_move].player_view,
                dungeon.level[sublevel][mmove[this_move].y][mmove[this_move].x].m_idx,
                (dungeon.level[sublevel][my][mx].fdat & CAVE_ROOM)?1:0);
   }

   /*
    * now for what tactic the monster should follow:
    */
   tactic = TACTIC_ATTACK_MELEE;
   
   for (blow=0; blow < 4; blow++)
   {
      if (!r_ptr->blow[blow].method) continue;
      total_dam += r_ptr->blow[blow].d_dice * r_ptr->blow[blow].d_side;
   }
dlog(DEBUGMONAI,"melee2.c: find_moves: total_dam %d\n", total_dam);

   /* determine if we are in the company of friends   */
   /* if we kill walls, we don't need to have friends */
   friends = -1;
   if ( (r_ptr->flags1 & ( RF1_ESCORT | RF1_ESCORTS | RF1_FRIEND | RF1_FRIENDS)) &&
        ! (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL)) )
   {
      friends = count_seen_friends(m_idx, 3);
      /* if there is room around the player, our friends can do damage too */
      if (count_walkables_in_los(mx, my, px, py) >= 3 )
      {
         total_dam *= (friends / 2); /* assume half of them are able to attack the player */
      }
      /* we don't rush blindly out of rooms, for example */
      tactic = TACTIC_ATTACK_MELEE_PACK;

dlog(DEBUGMONAI,"melee2.c: find_moves: pack monster, player_in_trouble %d, monster health %d/%d friends %d\n",
                player_in_trouble(), m_ptr->hp, m_ptr->maxhp, friends);
      /* all monsters rush in if the player is below 25% */
      if ( player_in_trouble() == 2)
      {
         tactic = TACTIC_ATTACK_MELEE;
      }
      /* non-stupid monsters rush in (50% chance) if the player is below 50% */
      if ( (player_in_trouble() == 1) &&
           !(r_ptr->flags2 & RF2_STUPID) && 
           (randint(10) < 5) )
      {
         tactic = TACTIC_ATTACK_MELEE;
      }
      /* smart monsters rush in (always) if the player is below 50% and they are with many and healty */
      if ( (player_in_trouble() == 1) &&
           (r_ptr->flags2 & RF2_SMART) && 
           (friends > 2) &&
           (m_ptr->hp > (m_ptr->maxhp / 2)) )
      {
         tactic = TACTIC_ATTACK_MELEE;
      }
      /* if the player is in a room, rush him */
      if  (dungeon.level[sublevel][py][px].fdat & CAVE_ROOM)
      {
         tactic = TACTIC_ATTACK_MELEE;
      }
   }
dlog(DEBUGMONAI,"melee2.c: find_moves: count_seen_friends %d count_walkables_in_los %d friends %d total_dam %d\n",
               count_seen_friends(m_idx, 3), count_walkables_in_los(mx, my, px, py), friends, total_dam);

   if (terror || afraid)
   {
      if (total_dam <= (p_ptr->mdepth * 2))
      {
         tactic = TACTIC_FLEE;
dlog(DEBUGMONAI, "melee2.c: find_moves: tactic == TACTIC_FLEE\n");
      }
   }
   /* determine if we want to attack in melee or using spells */
   if ( (r_ptr->flags6 & RF6_SUMMON_MASK) ||
        (r_ptr->flags4 & RF4_ATTACK_MASK) ||
        (r_ptr->flags5 & RF5_ATTACK_MASK) ||
        (r_ptr->flags6 & RF6_ATTACK_MASK) )
   {
      s16b pct = (s16b) (100L * m_ptr->hp / m_ptr->maxhp);
      /* if we are healthy */ 
dlog(DEBUGMONAI, "melee2.c: find_moves: pct %d terror %d afraid %d freq_spell %d\n", pct, terror, afraid, r_ptr->freq_spell);
      if ( (pct > 80) && (!terror) && (!afraid) )
      {
         /* do we do spells often on our own or with just one friend? */
         if ( (friends <= 2) && (r_ptr->freq_spell >= 33) )
         {
            tactic = TACTIC_ATTACK_SPELL;
         }
         /* do we do spells often in a group */
         if ( (friends >= 2) && (r_ptr->freq_spell >= (33 / (friends / 2)) ) )
         {
            tactic = TACTIC_ATTACK_SPELL;
         }
      }
   }
dlog(DEBUGMONAI, "melee2.c: find_moves: final tactic %d = %s%s%s%s\n", tactic,
                (tactic == TACTIC_ATTACK_MELEE)?"TACTIC_ATTACK_MELEE":"",
                (tactic == TACTIC_ATTACK_MELEE_PACK)?"TACTIC_ATTACK_MELEE_PACK":"",
                (tactic == TACTIC_ATTACK_SPELL)?"TACTIC_ATTACK_SPELL":"",
                (tactic == TACTIC_FLEE)?"TACTIC_FLEE":"");

   knows_exit = FALSE;
   find_exit  = FALSE;

   /* are we the fleeing type of monster?                                                  */
   /* this is a potentially expensive computation, so check beforehand if it is necessary  */
   /* note that being an unhappy pack monster (fleepack=TRUE) is not enough to flee a room */
   if ( (monster_flee_exits) && (!( r_ptr->flags1 & RF1_NEVER_MOVE)) &&
        (dungeon.level[sublevel][my][mx].fdat & CAVE_ROOM) && (terror || afraid) )
   {
      if ( (r_ptr->flags2 & RF2_SMART) || ( !(r_ptr->flags2 & RF2_STUPID)  && (randint(10)>5) ) )
      {
         knows_exit = find_closest_exit(m_idx, &exitx, &exity);

         if (knows_exit)
         {
            dlog(DEBUGMONAI,"melee2.c: find_moves: chosen exit @ %d,%d\n", exitx, exity);
         }
         else
         {
            dlog(DEBUGMONAI,"melee2.c: find_moves: no suitable exit found\n");
         }
      }
      else
      {
         dlog(DEBUGMONAI,"melee2.c: find_moves: monster isn't smart enough to find exit\n");
      }
   }
   else
   {
      dlog(DEBUGMONAI,"melee2.c: find_moves: monster doesn't need to find exits\n");
   }

   /* Check nearby grids, starting with a random one */
   start = rand_int(8);
   for (j = start; j <= start+7; j++)
   {
      this_move=(j % 8);
dlog(DEBUGMONAI,"melee2.c: find_moves: start %d j %d i %d\n", start, j, this_move);
      nx=mmove[this_move].x;
      ny=mmove[this_move].y;
      /* are we moving out of the dungeon? */
      if (!mmove[this_move].possible)
      {
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d out of bounds\n", this_move, nx, ny);
         continue;
      }
      c_ptr = &dungeon.level[sublevel][ny][nx];
      /* initialize relative distance */
      mmove[this_move].reldist = mmove[this_move].dist - mindist;
      if (knows_exit)
      {
         mmove[this_move].exitdist = distance(nx, ny, exitx, exity);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d exitdist %d\n", this_move, nx, ny,
                mmove[this_move].exitdist);
      }
      
      /* first weed out the moves that cannot happen because of walls, doors etc. */
      if (!weed_moves_feature(m_ptr, mmove, this_move)) continue;

      if (c_ptr->m_idx)
      {
         if (!weed_moves_monster(m_ptr, mmove, this_move)) continue;
      }
      /* until now, we have not really determined the best move         */
      /* just determined which move is possible and which isn't         */
      /* confused monsters don't care about that, so they skip the rest */ 
      if (m_ptr->confused) continue;

      if (!weed_moves_random(m_ptr, mmove, this_move)) continue;

/*
 * and finally we check for intelligent moves
 */
      objs = objects_on_floor(nx, ny);

      /* Check for a clean bolt shot */
      if ( (r_ptr->flags4 & (RF4_BOLT_MASK)) ||
           (r_ptr->flags5 & (RF5_BOLT_MASK)) ||
           (r_ptr->flags6 & (RF6_BOLT_MASK)) )
      {
         /* a reasonable smart monster like a place from where he can hit the player */
         if (!(r_ptr->flags2 & (RF2_STUPID)) && clean_shot(nx, ny, px, py, 0))
         {
            mmove[this_move].suitable += 500;
            /* and an extra bonus if the player cannot see us */
            mmove[this_move].suitable += (mmove[this_move].player_view)?0:250;
         }
         /* a very smart monster like a place from where he can hit the player 'around the corner' also */
         if ( (r_ptr->flags2 & RF2_SMART) && clean_shot(nx, ny, px, py, (r_ptr->flags2 & RF2_POWERFUL)?3:2))
         {
            mmove[this_move].suitable += 500;
            /* and an extra bonus if the player cannot see us */
            mmove[this_move].suitable += (mmove[this_move].player_view)?0:250;
         }
      }
      switch (tactic)
      {
         case TACTIC_ATTACK_MELEE:
            {
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE start %d friends %d\n",
                this_move, nx, ny, mmove[this_move].suitable, friends);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE after room %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* can we annoy the player by picking up stuff along the way? */
               if (objs > 0) pickup_possible(m_idx, mmove, this_move, objs);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE after items %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* can we annoy the player by crushing a glyph along the way? */
               test_kill_glyph(m_ptr, mmove, this_move);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE after glyph %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* move in to the player! */
               mmove[this_move].suitable += 100*(10-mmove[this_move].reldist);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE after distance %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* if we attack the player by moving here, do so! */
               if (mmove[this_move].dist==0) mmove[this_move].suitable +=500;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE after attack now %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* if we can do nothing else, stay in the wall */
               if ((r_ptr->flags2 & RF2_PASS_WALL) && (is_wall(nx, ny)))
               {
                  mmove[this_move].suitable += 50;
               }
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE after walls %d\n", this_move, nx, ny, mmove[this_move].suitable);
               break;
            }
         case TACTIC_ATTACK_MELEE_PACK:
            {
/*
 * pack monsters are the most difficult to handle:
 *
 * 1 if we are in a room, we stay there unless we are really powerfull and/or the player is hurt
 * 2a player in room, monster in room - we attack him
 * 2b player in tunnel, monster in tunnel - we attack him
 * 2c player in room, monster in tunnel - we attack him
 * 2d player in tunnel, monster in room - we stay in the room
 * 3 if the player is not in a room, but we are, we try to keep out of sight
 *    3a if we can keep out of sight, we creep towards the player, remaining out of sight.
 *    3b if we cannot keep out of sight, we flee as far as possible (within the room) 
 * 4 if we are in a tunnel and we cannot immediately attack the player, flee and hope for a room.
 */

dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE_PACK in_tunnel %d player_in_room %d\n", this_move, nx, ny, in_tunnel, player_in_room);
               /* rule 1 */
               /* whatever happens, pack monsters stay in the room */
               /* if we are now in a room                          */
               /* unless we are *really, really* powerfull         */
               /* And the player is in bad health                  */
               if ( (friends >=0) && (dungeon.level[sublevel][my][mx].fdat & CAVE_ROOM) )
               {
                  if ( (total_dam < (p_ptr->mdepth * 5) ) || (p_ptr-> chp > (p_ptr->mhp / 2) ) )
                  {
                     mmove[this_move].suitable += (c_ptr->fdat & CAVE_ROOM)?0:-2500;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE_PACK after testing for 'stay-in-room', suitable %d\n", mmove[this_move].suitable);
                  }
               }
               /* can we annoy the player by picking up stuff along the way? */
               if (objs > 0) pickup_possible(m_idx, mmove, this_move, objs);
               /* can we annoy the player by crushing a glyph along the way? */
               test_kill_glyph(m_ptr, mmove, this_move);

               /* rule 2 & 2a */
               /* player in tunnel, monster in room - hide */
               if (!player_in_room && !in_tunnel)
               {
                  /* unviewable spots are *much* better */
                  mmove[this_move].suitable += (mmove[this_move].player_view)?0:25000;
                  /* have a weak preference for squares close to the player; to ambush him */
                  if (mmove[this_move].player_view == FALSE)
                  {
                     mmove[this_move].suitable -= mmove[this_move].reldist;
                  }
                  else
                  {
                     /* if we are currently not visible to the player, and we would be if we moved this way, disallow that */
                     if ( !(dungeon.level[sublevel][my][mx].fdat & CAVE_VIEW) )
                     {
                        mmove[this_move].possible = FALSE;
                     }
                     /* we are in a room, and this move would be visible, move away */
                     else
                     {
                        mmove[this_move].suitable += mmove[this_move].reldist;
                     }
                  }
               }
               /* player in room, monster in room - attack */
               else if (!in_tunnel)
               {
                  mmove[this_move].suitable += 10*(10-mmove[this_move].reldist);

                  /* if we attack the player by moving here, do so! */
                  if (mmove[this_move].dist==0) mmove[this_move].suitable +=500;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE_PACK after player_in_room or both in tunnel, suitable %d\n", mmove[this_move].suitable);
               }
               /* player in tunnel, monster in tunnel - attack if possible */
               /* player in room, monster in tunnel - attack if possible */
               else
               {
                  /* if we can attack immediately, do so */
                  if (mmove[this_move].dist==0) mmove[this_move].suitable +=500;
                  /* if we can move into any room, do so */
                  if (mmove[this_move].in_room) mmove[this_move].suitable +=2500;
                  /* weak preference for moving away (hoping to find a room */
                  mmove[this_move].suitable += mmove[this_move].reldist;
               }
                   

               /* whatever else we do, staying in the wall is always good */
               if ((r_ptr->flags2 & RF2_PASS_WALL) && (is_wall(nx, ny)))
               {
                  mmove[this_move].suitable += 50;
               }
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_MELEE_PACK result %d\n", this_move, nx, ny, mmove[this_move].suitable);
               break;
            }
         case TACTIC_ATTACK_SPELL:
            {
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL start %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* grids in walls are much preferred by monsters who can use them */
               if ((r_ptr->flags2 & RF2_PASS_WALL) && (is_wall(nx, ny)))
               {
                  mmove[this_move].suitable += 400;
               }
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL wall %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* can we annoy the player by picking up stuff along the way? */
               if (objs > 0) pickup_possible(m_idx, mmove, this_move, objs);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL after items %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* can we annoy the player by crushing a glyph along the way? */
               test_kill_glyph(m_ptr, mmove, this_move);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL after glyph %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* can we hit the player from here? */
               if (!(r_ptr->flags2 & (RF2_STUPID)) && clean_shot(nx, ny, px, py, 0))
               {
                  mmove[this_move].suitable += 500;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL shot %d\n", this_move, nx, ny, mmove[this_move].suitable);
                  /* and an extra bonus if the player cannot see us */
                  mmove[this_move].suitable += (mmove[this_move].player_view)?0:250;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL viewable %d\n", this_move, nx, ny, mmove[this_move].suitable);
               }
               if ( (r_ptr->flags2 & RF2_SMART) && clean_shot(nx, ny, px, py, (r_ptr->flags2 & RF2_POWERFUL)?3:2))
               {
                  mmove[this_move].suitable += 500;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL smart shot %d\n", this_move, nx, ny, mmove[this_move].suitable);
                  /* and an extra bonus if the player cannot see us */
                  mmove[this_move].suitable += (mmove[this_move].player_view)?0:250;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL smart viewable %d\n", this_move, nx, ny, mmove[this_move].suitable);
               }

               /* don't come closer than 3 squares */
               if (mmove[this_move].reldist < 3) mmove[this_move].suitable -=200;
               if (mmove[this_move].reldist >=3) mmove[this_move].suitable +=200;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_ATTACK_SPELL dist %d\n", this_move, nx, ny, mmove[this_move].suitable);
               break;
            }
         case TACTIC_FLEE:
            {
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE start %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* if this leads to an exit, prefer it very much */
               mmove[this_move].suitable += 500*(100 - mmove[this_move].exitdist);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after exit %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* further spots are better */
               mmove[this_move].suitable += 50*mmove[this_move].reldist;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after dist %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* unviewable spots are *much* better */
               mmove[this_move].suitable += (mmove[this_move].player_view)?0:500;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after view %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* being touchable by the player is to be avoided */
               mmove[this_move].suitable += (mmove[this_move].reldist == 1)?0:300;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after touch %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* some walls may lead to better places.... */
               mmove[this_move].suitable += count_walls(nx, ny);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after walls %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* can we annoy the player by picking up stuff along the way? */
               if (objs > 0) pickup_possible(m_idx, mmove, this_move, objs);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after items %d\n", this_move, nx, ny, mmove[this_move].suitable);
               /* grids in walls are much preferred by monsters who can use them */
               if ((r_ptr->flags2 & RF2_PASS_WALL) && (is_wall(nx, ny)))
               {
                  mmove[this_move].suitable += 400;
               }
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after in walls %d\n", this_move, nx, ny, mmove[this_move].suitable);

               /* don't attack the player if possible */
               if ((nx == px) && (ny == py)) mmove[this_move].suitable -= 3000;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d TACTIC_FLEE after attack %d\n", this_move, nx, ny, mmove[this_move].suitable);
               break;
            }
         default: msg_format("monster (m_idx %d %s) @ %d,%d has unknown tactic?",
                             m_idx, r_name + r_info[mn_list[m_idx].r_idx].name, mx, my);
      }

      /* a not stupid monster that can move */
      if (!(r_ptr->flags2 & RF2_STUPID))
      {
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d test trees\n", this_move, nx, ny, mmove[this_move].suitable);
         /* moving from under a tree to out in plain sight */
         if ( (dungeon.level[sublevel][ny][nx].mtyp != DUNG_SHRUB) &&
              (dungeon.level[sublevel][my][mx].mtyp == DUNG_SHRUB))
         {
            /* which hasn't been attacked in a while or is afraid */
            if ((m_ptr->attacked>5) || (m_ptr->afraid))
            {
               /* not often leave the trees */
               if (randint(10)>2)
               {
                  mmove[this_move].suitable -= 250;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d stay in trees\n", this_move, nx, ny, mmove[this_move].suitable);
               }
            }
         }
      }
#ifdef MONSTER_FLOW
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d test flow\n", this_move, nx, ny, mmove[this_move].suitable);
      /* The player is not currently near the monster grid */
      /* and either the player has never been near the monster grid */
      /* or the monster is not allowed to track the player */
      if ( (c_ptr->when >= dungeon.level[sublevel][py][px].when) &&
           (!c_ptr->when || !flow_by_smell) ) continue;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d flow possible cost %d aaf %d\n",
                this_move, nx, ny, mmove[this_move].suitable, c_ptr->cost, r_ptr->aaf);
      /* Monster is too far away to notice the player */
      if (c_ptr->cost > MONSTER_FLOW_DEPTH) continue;
      if (c_ptr->cost > r_ptr->aaf) continue;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d flow close\n", this_move, nx, ny, mmove[this_move].suitable);

      /* Hack -- Player can see us, run towards him */
      if (mmove[this_move].player_view) continue;
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d flow player unviewable\n", this_move, nx, ny, mmove[this_move].suitable);

      /* optimize for high when and small cost */
      mmove[this_move].suitable += c_ptr->when * (200-c_ptr->cost);
dlog(DEBUGMONAI,"melee2.c: find_moves: sq %d @ %d,%d suit %d flow done\n", this_move, nx, ny, mmove[this_move].suitable);
#endif
   }

   maxsuit = -30000;
   cnt = 0;
   for (this_move = 7; this_move >= 0; this_move--)
   {
dlog(DEBUGMONAI,"melee2.c: find_moves: testing sq %d @ %d,%d suit %d possible %d maxsuit %ld\n",
                this_move, mmove[this_move].x, mmove[this_move].y,
                mmove[this_move].suitable, mmove[this_move].possible, maxsuit);
      if (mmove[this_move].possible == FALSE) continue;
      if (mmove[this_move].suitable > maxsuit)
      {
         maxsuit = mmove[this_move].suitable;
         bestmoves[0]=this_move; /* it's better, so start the best_moves from the start */
         cnt=1;
      }
      else if (mmove[this_move].suitable == maxsuit)
      {
         bestmoves[cnt++] = this_move; /* it's equally good, so add to the best_moves */
      }
   }
   if (cnt>0)
   {
      this_move = bestmoves[rand_int(cnt)];
dlog(DEBUGMONAI,"melee2.c: find_moves: returning move %d @ %d,%d suit %d\n",
               this_move, mmove[this_move].x, mmove[this_move].y, maxsuit);
      if (mmove[this_move].suitable < 0)
      {
         return (-1);
dlog(DEBUGMONAI,"melee2.c: find_moves: returning move -1\n");
      }
   }
   else
   {
      this_move=-1;
dlog(DEBUGMONAI,"melee2.c: find_moves: returning move -1\n");
   }
   return (this_move);
}

/*
 * try to multiply a monster
 * returns 0 for not possible (no space)
 * returns -1 for not possible (no more generations allowed)
 * returns -2 for maximum number of breeders reached
 * returns 1 for done
 */
static s16b try_multiply(s16b m_idx)
{
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];

   s16b ox = m_ptr->fx, oy = m_ptr->fy;

   /* Attempt to "mutiply" if able and allowed */
   if (num_repro < MAX_REPRO)
   {
      s16b k, x, y;

      if (r_info[m_ptr->r_idx].flags2 & RF2_MULTIPLY)
      {
         /* do we have any generations left */
         if (!m_ptr->breed_counter) return (-1);
      }

      /* Count the adjacent monsters */
      for (k = 0, y = oy - 1; y <= oy + 1; y++)
      {
          for (x = ox - 1; x <= ox + 1; x++)
         {
            if (dungeon.level[sublevel][y][x].m_idx) k++;
         }
      }

      /* Hack -- multiply slower in crowded areas */
      if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
      {
         /* Try to multiply */
         if (multiply_monster(m_idx))
         {
            /* Take note if visible */
            if (m_ptr->ml) r_ptr->r_flags2 |= RF2_MULTIPLY;

            /* Multiplying takes energy */
            return (1);
         }
         else
         {
            return (0);
         }
      }
   }
   return (-2);
}

/*
 * do we wake up this monster?
 */
static bool try_wake_up(s16b m_idx, u32b player_noise)
{
   monster_type  *m_ptr = &mn_list[m_idx];
   monster_race  *r_ptr = &r_info[m_ptr->r_idx];
   u32b           notice = 0;

   /* Hack -- handle non-aggravation */
   if (!p_ptr->aggravate) notice = rand_int(1024);

dlog(DEBUGMONAI,"melee2.c: try_wake_up: notice %lu ^3=%lu, noise %lu csleep %d agg %d test %d\n",
                notice, notice*notice*notice, player_noise, m_ptr->csleep, p_ptr->aggravate,
                ((notice*notice*notice)<=player_noise) );

   /* Hack -- See if monster "notices" player */
   if ((notice * notice * notice) <= player_noise)
   {
      /* Hack -- amount of "waking" */
      s16b d = 1;

dlog(DEBUGMONAI,"melee2.c: try_wake_up: waking up the monster\n");

      /* Wake up faster near the player */
      if (m_ptr->cdis < 50)
      {
         d = (100 / m_ptr->cdis);
      }
dlog(DEBUGMONAI,"melee2.c: try_wake_up: cdis %d, d now %d\n",
                m_ptr->cdis, d);

      /* Hack -- handle aggravation */
      if (p_ptr->aggravate) d = m_ptr->csleep;

      /* Still asleep */
      if (m_ptr->csleep > d)
      {
         /* Monster wakes up "a little bit" */
         m_ptr->csleep -= d;

         /* Notice the "not waking up" */
         if (m_ptr->ml)
         {
            /* Hack -- Count the ignores */
            if (r_ptr->r_ignore < MAX_UCHAR) r_ptr->r_ignore++;
         }
      }

      /* Just woke up */
      else
      {
         /* Reset sleep counter */
         m_ptr->csleep = 0;

         /* Notice the "waking up" */
         if (m_ptr->ml)
         {
            char m_name[80];

            /* Acquire the monster name */
            monster_desc(m_name, m_ptr, 0);

            /* Dump a message */
            msg_format("%^s wakes up.", m_name);

            /* Hack -- Count the wakings */
            if (r_ptr->r_wake < MAX_UCHAR) r_ptr->r_wake++;
         }
      }
   }
   /* do make sure that we cannot rest for 3000 turns in front of even the */
   /* most sleepy monster                                                  */
   else
   {
      if (m_ptr->cdis < 3)
      {
         m_ptr->csleep -= (3-m_ptr->cdis);
      }
   }

   if (m_ptr->csleep) return (FALSE);

   return (TRUE);
}

/*
 * does the monster heal from stunning?
 */
static bool recover_stun(s16b m_idx)
{
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];

   s16b d = 1;

   /* Make a "saving throw" against stun */
   if (rand_int(5000) <= r_ptr->level * r_ptr->level)
   {
      /* Recover fully */
      d = m_ptr->stun;
   }

   /* Hack -- Recover from stun */
   if (m_ptr->stun > d)
   {
      /* Recover somewhat */
      m_ptr->stun -= d;
   }

   /* Fully recover */
   else
   {
      /* Recover fully */
      m_ptr->stun = 0;

      /* Message if visible */
      if (m_ptr->ml)
      {
         char m_name[80];

         /* Acquire the monster name */
         monster_desc(m_name, m_ptr, 0);

         /* Dump a message */
         msg_format("%^s is no longer stunned.", m_name);
      }
   }

   /* Still stunned */
   if (m_ptr->stun) return (FALSE);
   return (TRUE);
}

/*
 * does the monster recover from confusion?
 */
static void recover_confused(s16b m_idx)
{
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];

   /* Amount of "boldness" */
   s16b d = randint(r_ptr->level / 10 + 1);

   /* Still confused */
   if (m_ptr->confused > d)
   {
      /* Reduce the confusion */
      m_ptr->confused -= d;
   }

   /* Recovered */
   else
   {
      /* No longer confused */
      m_ptr->confused = 0;

      /* Message if visible */
      if (m_ptr->ml)
      {
         char m_name[80];

         /* Acquire the monster name */
         monster_desc(m_name, m_ptr, 0);

         /* Dump a message */
         msg_format("%^s is no longer confused.", m_name);
      }
   }
}

/*
 * recover from fear
 */
static void recover_fear(s16b m_idx)
{
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];
   /* Amount of "boldness" */
   s16b d = randint(r_ptr->level / 10 + 1);

   /* Still afraid */
   if (m_ptr->afraid > d)
   {
      /* Reduce the fear */
      m_ptr->afraid -= d;
   }

   /* Recover from fear, take note if seen */
   else
   {
      /* No longer afraid */
      m_ptr->afraid = 0;

      /* Visual note */
      if (m_ptr->ml)
      {
         char m_name[80];
         char m_poss[80];

         /* Acquire the monster name/poss */
         monster_desc(m_name, m_ptr, 0);
         monster_desc(m_poss, m_ptr, 0x22);

         /* Dump a message */
         if (fear_messages)
            msg_format("%^s recovers %s courage.", m_name, m_poss);
      }
   }
}

/*
 * handle an "escaping" thief
 */
static void handle_escaping(s16b m_idx)
{
   monster_type        *m_ptr = &mn_list[m_idx];

   m_ptr->escaping--;

   if (!m_ptr->escaping && m_ptr->ml)
   {
      if (m_ptr->ml && player_has_los_bold(m_ptr->fx, m_ptr->fy))
      {
         /* Visual note */
         char m_name[80];

         /* Acquire the monster name/poss */
         monster_desc(m_name, m_ptr, 0);

         /* Dump a message */
         msg_format("%^s stops and looks at your backpack.", m_name);
      }
      m_ptr->mspeed = m_ptr->oldspeed;
   }
}

/*
 * this function tests if a monster will build a wall in the
 * location it just left.
 */
void test_build_wall(m_idx, ox, oy)
{
   monster_type        *m_ptr = &mn_list[m_idx];

   if ((randint(10)==1) || (m_ptr->afraid))
   {
      s16b chance = randint(10);

      switch(chance)
      {
         case 1:
         case 2:
         case 3:
         case 4:
         case 5:
         case 6:
         case 7:  (void)set_grid_type(ox, oy, DUNG_WALL, DUNG_WALL_RUBBLE, GRID_KEEP, 0);
                  break;
         case 8:
         case 9:  place_wall(ox, oy);
                  break;
      }
   }
}

/*
 * Process a monster
 *
 * The monster is known to be within 100 grids of the player
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * XXX XXX XXX In addition, monsters which *cannot* open or bash
 * down a door will still stand there trying to open it...
 *
 * XXX Technically, need to check for monster in the way
 * combined with that monster being in a wall (or door?)
 *
 * A "direction" of "5" means "pick a random direction".
 */
static void process_monster(s16b m_idx, u32b player_noise)
{
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];

   s16b                 ox, oy, nx = 0, ny = 0, move, tmp_hp, player_hp;

   cave_cell_type      *c_ptr;
   object_type         *i_ptr;
   char                 m_name[80];

   bool                 do_turn;
   bool                 do_move;
   bool                 do_view;
   bool                 player_can_see_old = FALSE;
   bool                 player_can_see_new = FALSE;
   bool                 previous_ml = m_ptr->ml;

dlog(DEBUGMONAI,"melee2.c: process_monster: m_idx %d %s @ %d,%d player %d,%d\n",
                m_idx, r_name+r_ptr->name, m_ptr->fx, m_ptr->fy, px, py);
dlog(DEBUGMONAI,"melee2.c: process_monster: cslp %d stun %d conf %d afraid %d escaping %d\n",
                m_ptr->csleep, m_ptr->stun, m_ptr->confused, m_ptr->afraid, m_ptr->escaping);

   /* Handle "sleep" - do nothing if the monster stays put */
   if (m_ptr->csleep)
   {
      if (!try_wake_up(m_idx, player_noise)) return;
   }

   /* Handle "stun" - stunned monsters don't move */
   if (m_ptr->stun)
   {
      if (!recover_stun(m_idx)) return;
   }

   /* Handle confusion - confused monsters do move */
   if (m_ptr->confused) recover_confused(m_idx);

   /* Handle "fear" - fearing monsters do move */
   if (m_ptr->afraid) recover_fear(m_idx);

   /* Handle "escaping" */
   if (m_ptr->escaping) handle_escaping(m_idx);

   /* Get the origin */
   ox = m_ptr->fx;
   oy = m_ptr->fy;

   /* Attempt to "multiply" if able and allowed */
   if (r_ptr->flags2 & RF2_MULTIPLY)
   {
      move = try_multiply(m_idx);
      /*
       * returns 0 for not possible (no space)
       * returns -1 for not possible (no more generations allowed)
       * returns -2 for maximum number of breeders reached
       * returns 1 for done
       *
       * messages for cases -1 or -2 seem to fill up the screen too much,
       * so they are omitted!
       */
      if (move==1)
      {
         if (player_has_los_bold(m_ptr->fx, m_ptr->fy) && m_ptr->ml)
         {
            monster_desc(m_name, m_ptr, 0x04);
            msg_format("%^s breeds another generation.", m_name);
            if (wizard)
            {
               msg_format("This is generation %d of %d.",
                          r_ptr->max_gen - m_ptr->breed_counter, r_ptr->max_gen);
            }
            return;
         }
      }
   }

   /* XXX XXX XXX this is a gross hack. */
   player_hp = p_ptr->chp;

   /* Attempt to cast a spell */
dlog(DEBUGMONAI,"melee2.c: process_monster: trying for attack spell\n");
   if (make_attack_spell(m_idx))
   {
      /* if we didn't do anything, last_attack is 0 */
      /* if we did 10% damage, it is 10             */
      /* if we did 50% damage, it is 2              */
      tmp_hp = (player_hp == p_ptr->chp) ? 0 : (p_ptr->mhp / (player_hp - p_ptr->chp));
      if (tmp_hp > 100) tmp_hp = 100;
      m_ptr->last_hit = (byte)tmp_hp;

      return;
   }

   move = find_moves(m_idx);
dlog(DEBUGMONAI,"melee2.c: process_monster: find_moves returned %d\n", move);
   /* Assume nothing */
   do_turn = FALSE;
   do_move = FALSE;
   do_view = FALSE;

   if (move != -1)
   {
dlog(DEBUGMONAI,"melee2.c: process_monster: find_moves returned %d flags %08x\n",
                move, mmove[move].result);
      nx = mmove[move].x;
      ny = mmove[move].y;

      c_ptr = &dungeon.level[sublevel][ny][nx];

      if (p_ptr->blind == 0)
      {
         player_can_see_old = los(px, py, ox, oy);
dlog(DEBUGMONAI,"melee2.c: process_monster: can_see_old from %d,%d to %d,%d = %d\n",
                px, py, ox, oy, player_can_see_old);
         if (! (dungeon.level[sublevel][oy][ox].fdat & CAVE_VIEW) )
         {
dlog(DEBUGMONAI,"melee2.c: process_monster: can_see_old no CAVE_VIEW\n");
            player_can_see_old = FALSE;
         }

         player_can_see_new = los(px, py, nx, ny);
dlog(DEBUGMONAI,"melee2.c: process_monster: can_see_new from %d,%d to %d,%d = %d cave: %08lx\n",
                px, py, nx, ny, player_can_see_new, c_ptr->fdat);
         if (! (dungeon.level[sublevel][ny][nx].fdat & CAVE_VIEW) )
         {
dlog(DEBUGMONAI,"melee2.c: process_monster: can_see_new no CAVE_VIEW\n");
            player_can_see_new = FALSE;
         }
      }

      do_move = TRUE;
      if (mmove[move].result & MOVE_KILL_WALL)
      {
         u32b old_fdat = 0L;

         (void)set_grid_type(nx, ny, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);

         if (c_ptr->t_idx) /* if it was trapped, remove and invalidate it */
         {
            trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];

            tr_ptr->inuse = FALSE;
            c_ptr->t_idx = 0;
         }

         /* Note changes to viewable region */
         if (player_can_see_new)
         {
            c_ptr->fdat |= CAVE_MARK;
            if (old_fdat & CAVE_LITE) c_ptr->fdat |= CAVE_GLOW;
            if (old_fdat & CAVE_VIEW) c_ptr->fdat |= CAVE_VIEW;

            /* the monster is visible */
            m_ptr->ml = TRUE;
            m_ptr->los = TRUE;
            do_view = TRUE;
            r_ptr->r_flags2 |= RF2_KILL_WALL;

         }
      }

      /* passing though a wall requires no special actions other than noting it */
      if (mmove[move].result & MOVE_PASS_WALL)
      {
         if (player_can_see_new && m_ptr->ml)
         {
            r_ptr->r_flags2 |= RF2_PASS_WALL;
         }
         do_view = TRUE;
      }

      if (mmove[move].result & MOVE_OPEN_DOOR)
      {
         (void)set_grid_type(nx, ny, DUNG_DOOR, DUNG_DOOR_OPEN,
                             GRID_ADD, CAVE_WALK|CAVE_LIGHT|CAVE_MAGIC);

         if (player_can_see_new)
         {
            p_ptr->update |= PU_VIEW;
         }

         /* if it was trapped, execute the trap */
         if (c_ptr->t_idx)
         {
            trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
            monster_execute_trap(m_idx, tr_ptr, -2, nx, ny, FALSE);
            /* monster killed? */
            if (!m_ptr->r_idx) return;
         }

         /* Handle viewable doors */
         if (player_can_see_new)
         {
            msg_print("You see a door open!");
            do_view = TRUE;
            if (m_ptr->ml)
            {
               r_ptr->r_flags2 |= RF2_OPEN_DOOR;
            }
         }
         else
         {
            if (randint(100)<p_ptr->skill_pcp)
            {
               msg_print("You hear a door open!");
               if (m_ptr->ml)
               {
                  r_ptr->r_flags2 |= RF2_OPEN_DOOR;
               }
            }
         }
         do_view = TRUE;
      }

      if (mmove[move].result & MOVE_BASH_DOOR)
      {

         /* Disturb (sometimes) */
         if (disturb_other) disturb(0, 0);

         if (rand_int(100) < 50)
         {
            (void)set_grid_type(nx, ny, DUNG_DOOR, DUNG_DOOR_BROKEN,
                                GRID_KEEP, CAVE_WALK|CAVE_LIGHT|CAVE_MAGIC);
         }
         else
         {
            (void)set_grid_type(nx, ny, DUNG_DOOR, DUNG_DOOR_OPEN,
                                GRID_KEEP, CAVE_WALK|CAVE_LIGHT|CAVE_MAGIC);
         }

         p_ptr->update |= PU_VIEW;
         do_view = TRUE;

         if (c_ptr->t_idx)
         /* if it was trapped, execute it */
         {
            trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
            monster_execute_trap(m_idx, tr_ptr, -2, nx, ny, TRUE);
            if (!m_ptr->r_idx) return;
         }
         /* Handle viewable doors */
         if (player_has_los_bold(nx, ny))
         {
            msg_print("You see a door burst open!");
         }
         else
         {
            msg_print("You hear a door burst open!");
         }

         if (m_ptr->ml)
         {
            r_ptr->r_flags2 |= RF2_OPEN_DOOR;
         }
      }

      if (mmove[move].result & MOVE_BREAK_GLYPH)
      {
         /* Describe observable breakage */
         if (c_ptr->fdat & CAVE_MARK)
         {
            msg_print("The rune of protection is broken!");
         }

         /* Break the rune */
         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
         p_ptr->update |= PU_VIEW;

      }

      /* The player is in the way.  Attack him. */
      if ((nx == px) && (ny == py))
      {
         /* XXX XXX XXX this is a gross hack. */
         s16b player_hp = p_ptr->chp;
         s16b tmp;

         /* Do the attack */
         (void)make_attack_normal(m_idx);

         /* if we didn't do anything, last_attack is 0 */
         /* if we did 10% damage, it is 10             */
         /* if we did 50% damage, it is 2              */
         tmp = (player_hp == p_ptr->chp) ? 0 : (p_ptr->mhp / (player_hp - p_ptr->chp));
         if (tmp > 100) tmp = 100;
         m_ptr->last_hit = (byte)tmp;

         /* we don't move */
         do_turn = TRUE;
         do_move = FALSE;
      }
dlog(DEBUGMONAI,"melee2.c: process_monster: mmoves[].result parsed, do_move %d can_see_old %d can_see_new %d\n",
                do_move, player_can_see_old, player_can_see_new);

      /* Creature has been allowed move */
      if (do_move)
      {
         /* Take a turn */
         do_turn = TRUE;

         /* did we move a monster */
         if (mmove[move].result & MOVE_MOVE_BODY)
         {
            monster_type *m_ptr2 = &mn_list[dungeon.level[sublevel][ny][nx].m_idx];
            s16b tmp;
            tmp = dungeon.level[sublevel][ny][nx].m_idx;
            dungeon.level[sublevel][ny][nx].m_idx = dungeon.level[sublevel][oy][ox].m_idx;
            dungeon.level[sublevel][oy][ox].m_idx = tmp;
            m_ptr2->fx = ox;
            m_ptr2->fy = oy;
            /* update the switched monster */
            update_mon(dungeon.level[sublevel][ny][nx].m_idx, TRUE);
            m_ptr->fx = nx;
            m_ptr->fy = ny;
            /* we don't move off this sublevel! */
            /* update the current monster */
            update_mon(m_idx, TRUE);

            if (c_ptr->t_idx)
            {
               trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];

               /* it could happen that you saw messages about some monster */
               /* hitting a trap while on-screen you didn't see anything   */
               /* Redraw the old grid */
               lite_spot(ox, oy);

               /* Redraw the new grid */
               lite_spot(nx, ny);

               monster_execute_trap(m_idx, tr_ptr, -1, nx, ny, FALSE);
            }
            if (player_can_see_new && m_ptr->ml)
            {
                r_ptr->r_flags2 |= RF2_MOVE_BODY;
            }
         }
         /* normal move */
         else
         {
            /* did we kill a monster */
            if (mmove[move].result & MOVE_KILL_BODY)
            {
               project_who_type who;
dlog(DEBUGMONST,"melee2.c: process_monster: m_idx %d (%s) just killed m_idx %d (%s) @ %d,%d\n",
                m_idx, r_name+r_ptr->name, c_ptr->m_idx,
                r_name + r_info[mn_list[c_ptr->m_idx].r_idx].name, nx, ny);

               if (player_has_los_bold(nx, ny) && m_ptr->ml)
               {
                  char m_name1[256], m_name2[256];
                  monster_desc(m_name1, m_ptr, 0x80);
                  monster_desc(m_name2, &mn_list[c_ptr->m_idx], 0x80);

                  msg_format("You see %s kill %s.", m_name1, m_name2);
                  r_ptr->r_flags2 |= RF2_KILL_BODY;
               }
               who.type = WHO_MONSTER;
               monster_death(&who, dungeon.level[sublevel][ny][nx].m_idx);
               delete_monster(nx, ny);
            }
            c_ptr->m_idx = m_idx;
            dungeon.level[sublevel][oy][ox].m_idx = 0;
            m_ptr->fx = nx;
            m_ptr->fy = ny;
            /* we don't move off this sublevel! */
            update_mon(m_idx, TRUE);

            if (c_ptr->t_idx)
            /* if it was trapped, execute it */
            {
               trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
               monster_execute_trap(m_idx, tr_ptr, -1, nx, ny, FALSE);
            }
            if (c_ptr->styp == DUNG_LAVA)
            {
               s16b               dam = 0;
               project_who_type   who;

               /* undead float */
               if (r_ptr->flags3 & RF3_NO_FLOOR)
               {
                  dam = 25 + randint(50);
               }
               else
               {
                  dam = 200 + randint(400);
               }
               who.type = WHO_MONSTER;
               who.index = m_idx;  
               who.index_race = m_ptr->r_idx;  

               (void)project(&who, 1, m_ptr->fx, m_ptr->fy, dam, GF_FIRE,
                             PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
            }
         }

         if (r_ptr->flags2 & RF2_BUILD_WALL)
         {
            test_build_wall(m_idx, ox, oy);
         }

         /* if we cannot see where the monster was, but we can see where it goes */
         /* assume we know it has moved. This is sub-optimal XXX                 */
         /* in particular with monsters that were seen at their old location by  */
         /* telepathy or infravision: they don't always are seen at their new    */
         /* location yet are not redrawn on their old location.....              */
         /* can we see the old location, then redraw that */
         /* can we see the monster some other way (telepathy) then use that */
         if (player_can_see_old || m_ptr->ml || player_can_see_new)
         {
            do_view = TRUE;
         }

         /* Possible disturb */
         if (m_ptr->ml && (disturb_move || (m_ptr->los && disturb_near)))
         {
            /* Disturb */
            disturb(0, 0);
         }

         /* we could have a dead monster now, after it hit a trap - r_idx == 0 */
         if (m_ptr->r_idx && (mmove[move].result & (MOVE_TAKE_ITEM | MOVE_KILL_ITEM)))
         {
            char m_name[80];
            char i_name[80];
            s16b j;

            j = rand_int(objects_on_floor(nx,ny));
            i_ptr = get_item_pointer_floor_xy(j,nx,ny);
dlog(DEBUGMONAI,"melee2.c: process_monster: item %d @ %d,%d requested: tv %d sv %d pv %d name %s\n",
                j, nx, ny, i_ptr->tval, i_ptr->sval, i_ptr->p1val, k_name + k_info[i_ptr->k_idx].name);
            /* Acquire the object name */
            object_desc(i_name, i_ptr, TRUE, 0x32);

            /* Acquire the monster name */
            monster_desc(m_name, m_ptr, 0x04);

            /* The object cannot be picked up by the monster */
            if (!correct_item(m_ptr->r_idx,i_ptr))
            {
               /* Only give a message for "take_item" */
               if (r_ptr->flags2 & RF2_TAKE_ITEM)
               {
                  /* Describe observable situations */
                  if (m_ptr->ml && player_has_los_bold(nx, ny))
                  {
                     /* Dump a message */
                     if (m_ptr->ml)
                     {
                        r_ptr->r_flags2 |= RF2_TAKE_ITEM;
                        /* Dump a message */
                        msg_format("%^s tries to pick up %s, but fails.", m_name, i_name);
                     }
                     else
                     {
                        msg_format("You see %s move a few times.", i_name);
                     }
                  }
               }
            }

            /* Pick up the item */
            else if (r_ptr->flags2 & RF2_TAKE_ITEM)
            {
               if (monster_inven_carry(m_idx,i_ptr)==-1)
               {
                  /* monster cannot carry any more.... */
                  if (player_has_los_bold(nx, ny))
                  {
                     if (m_ptr->ml)
                     {
                        /* Dump a message */
                        msg_format("%^s looks intensely at %s.", m_name, i_name);
                     }
                     else
                     {
                        msg_print("You feel a strange anxiety.");
                     }
                  }
               }
               else
               {
                  if (player_has_los_bold(nx, ny))
                  {
                     if (m_ptr->ml)
                     {
                        r_ptr->r_flags2 |= RF2_TAKE_ITEM;
                        /* Dump a message */
                        msg_format("%^s picks up %s.", m_name, i_name);
                     }
                     else
                     {
                        msg_format("You see %s being picked up.", i_name);
                     }
                  }
                  delete_object(nx, ny, j);
               }
            }

            /* Destroy the item */
            else
            {
               /* Describe observable situations */
               if (player_has_los_bold(nx, ny))
               {
                  if (m_ptr->ml)
                  {
                     r_ptr->r_flags2 |= RF2_KILL_ITEM;
                     /* Dump a message */
                     msg_format("%^s crushes %s.", m_name, i_name);
                  }
                  else
                  {
                     msg_format("You see %s being crushed.", i_name);
                  }
               }

               /* Delete the object */
               delete_object(nx, ny, j);
            }
         } /* item taken/crushed */
      } /* actual movement */
   } /* valid move suggested */

   /* Notice changes in view */

   if (player_can_see_old | player_can_see_new | do_view || m_ptr->ml || previous_ml)
   {
      /* Update some things */
      p_ptr->update |= (PU_VIEW);
   }
   if (player_can_see_old || previous_ml)
   {
      lite_spot(ox, oy);
   }
   if (player_can_see_new || m_ptr->ml)
   {
      note_spot(nx, ny);
      lite_spot(nx, ny);
   }

   /* Hack -- get "bold" if out of options */
   if (!do_turn && !do_move && m_ptr->afraid)
   {
      /* No longer afraid */
      m_ptr->afraid = 0;

      /* Message if seen */
      if (m_ptr->ml)
      {
         char m_name[80];

         /* Acquire the monster name */
         monster_desc(m_name, m_ptr, 0);

         /* Dump a message */
         if (fear_messages)
            msg_format("%^s turns to fight!", m_name);
      }
      /* XXX XXX XXX Actually do something now (?) */
   }
}

/*
 * provided the player can see it and isn't blind or confused,
 * this function mentions to the player that a monster is carrying
 * an artifact
 */
void mention_monster_artifact(s16b m_idx)
{
   monster_type *m_ptr = &mn_list[m_idx];
   s16b         is_idx = item_set_this_monster(m_idx);
   s16b         j, number;
   object_type *i_ptr;
   char         m_name[80];

   monster_desc(m_name, m_ptr, 0x04);

   /* once in a while make the player notice monsters with artifacts */
   if ( (player_has_los_bold(m_ptr->fx, m_ptr->fy)==TRUE) &&
        (p_ptr->blind == 0) &&
        (p_ptr->confused == 0) )
   {
      bool has_artifact = FALSE;

      if (m_ptr->has_drop)
      {
         /* find the correct item_set belonging to this monster */
         item_set_type *is_ptr = &is_list[is_idx];
         number = items_in_set (is_idx);

         for (j = 0; j < number; j++)
         {
            i_ptr = &i_list[is_ptr->index[j]];
            /* more paranoia! */
            if (!i_ptr->k_idx)
            {
               continue;
            }
            if (i_ptr->name1) has_artifact = TRUE;
         }
         if (has_artifact)
         {
            msg_format("You see an aura of power surrounding %s.", m_name);
         }
      }
   }
}

/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 *
 * Note the use of the new special "mn_fast" array, which allows us to only
 * process monsters which are alive (or at least recently alive), which may
 * provide some optimization, especially when resting.   Note that monsters
 * which are only recently alive are excised, using a simple "excision"
 * method relying on the fact that the array is processed backwards.
 *
 * Note that "new" monsters are always added by "mn_pop()" and they are
 * always added at the end of the "mn_fast" array.
 */
void process_monsters(void)
{
   s16b           k, m_idx, e, noise_idx;
   s16b           fx, fy, aaf;
   u32b           player_noise;

   bool           test, test_artifact = FALSE;

   monster_type  *m_ptr;
   monster_race  *r_ptr;

   /* Hack -- calculate the "player noise" */
   noise_idx = 30 - p_ptr->skill_stl + p_ptr->cur_lite;
   if (noise_idx > 31) noise_idx = 31;

   player_noise = (u32b)(1L << (u32b)noise_idx);
dlog(DEBUGMONAI,"melee2.c: process_monsters: skill_stl %d, lite %d, noise %lu noise_idx %d\n",
                p_ptr->skill_stl, p_ptr->cur_lite, player_noise, noise_idx);

   /* Process the monsters */
   for (k = mn_top - 1; k >= 0; k--)
   {
      /* Access the index */
      m_idx = mn_fast[k];

      /* Access the monster */
      m_ptr = &mn_list[m_idx];

      /* Excise "dead" monsters */
      if (!m_ptr->r_idx)
      {
         /* Excise the monster */
         mn_fast[k] = mn_fast[--mn_top];

         /* Skip */
         continue;
      }
      if (m_ptr->fz != sublevel) continue;

      /* Obtain the energy boost */
      e = extract_energy[m_ptr->mspeed];

      /* Give this monster some energy */
      m_ptr->energy += e;

      /* Not enough energy to move */
      if (m_ptr->energy < 100) continue;

      /* Use up "some" energy */
      m_ptr->energy -= 100;

      /* Hack -- Require proximity */
      if (m_ptr->cdis >= 100) continue;

      /* Access the race */
      r_ptr = &r_info[m_ptr->r_idx];

      /* Access the location */
      fx = m_ptr->fx;
      fy = m_ptr->fy;
      aaf = r_ptr->aaf;

      /* Assume no move */
      test = FALSE;

      /* make sure monsters don't see too far in the wilderness */
      if (dungeon.level[sublevel][fy][fx].mtyp == DUNG_SHRUB)
      {
         aaf = (aaf<10)?1:(aaf / 10);
      }


      /* Handle "sensing radius" */
      if (m_ptr->cdis <= aaf)
      {
         /* We can "sense" the player */
         test = TRUE;
         test_artifact = TRUE;
      }

      /* Handle "sight" and "aggravation" */
      else if (m_ptr->cdis <= MAX_SIGHT)
      {
         if (player_has_los_bold(fx, fy))
         {
            test_artifact = TRUE;
            /* We can "see" or "feel" the player */
            test = TRUE;
         }
         if (p_ptr->aggravate)
         {
            test = TRUE;
         }
      }

#ifdef MONSTER_FLOW
      /* Hack -- Monsters can "smell" the player from far away */
      /* Note that most monsters have "aaf" of "20" or so */
      else if (flow_by_sound &&
            (dungeon.level[sublevel][py][px].when == dungeon.level[sublevel][fy][fx].when) &&
            (dungeon.level[sublevel][fy][fx].cost < MONSTER_FLOW_DEPTH) &&
            (dungeon.level[sublevel][fy][fx].cost < aaf))
      {
         /* We can "smell" the player */
         test = TRUE;
      }
#endif

      /* Do nothing */
      if (!test) continue;


      if (test_artifact && (randint(50)==1))
      {
         (void)mention_monster_artifact(m_idx);
      }

      /* Process the monster */
      process_monster(m_idx, player_noise);
      /* Hack -- notice death or departure */
      if (!alive || death || new_level_flag) break;
   }


#ifdef SHIMMER_MONSTERS

   /* Only when needed, every ten game turns */
   if (scan_monsters && (!(turn % 10)))
   {
      /* Shimmer multi-hued monsters */
      for (m_idx = 1; m_idx < mn_max; m_idx++)
      {
         monster_race *r_ptr;

         m_ptr = &mn_list[m_idx];

         /* Skip dead monsters */
         if (!m_ptr->r_idx) continue;

         /* Skip unseen monsters */
         if (!m_ptr->ml) continue;

         /* Access the monster race */
         r_ptr = &r_info[m_ptr->r_idx];

         /* Skip non-multi-hued monsters */
         if (!(r_ptr->flags1 & RF1_ATTR_MULTI)) continue;

         /* Shimmer Multi-Hued Monsters */
         lite_spot(m_ptr->fx, m_ptr->fy);
      }

      /* Clear the flag */
      scan_monsters = FALSE;
   }

#endif

}



