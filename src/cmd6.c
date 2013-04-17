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


static bool activate_random_artifact(object_type * o_ptr);
static bool activate_spell(object_type * o_ptr, byte choice);

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
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */


/*
 * Determine the effects of eating a corpse. A corpse can be
 * eaten whole or cut into pieces for later.
 */
static void corpse_effect(object_type *o_ptr, bool cutting)
{
        monster_race *r_ptr = &r_info[o_ptr->pval2];

	/* Assume no bad effects */
	bool harmful = FALSE;

	byte method, effect, d_dice, d_side;

        int i, dam, idam = 0, mdam, brpow, brdam = 0;

	/* How much of the monster's breath attack remains */
        brpow = (o_ptr->pval > r_ptr->weight ? (o_ptr->pval - r_ptr->weight) / 5 : 0);
        brpow = (brpow > r_ptr->weight / 5 ? r_ptr->weight / 5 : brpow);

        if(o_ptr->weight <= 0) o_ptr->weight = 1;
        if(o_ptr->pval <= 0) o_ptr->pval = 1;

	/*
	 * The breath is only discharged by accident or by slicing off pieces
	 * of meat, and only by corpses.
	 */
        if (o_ptr->sval != SV_CORPSE_CORPSE || (rand_int(o_ptr->weight / 5) && !cutting)) brpow = 0;

	/* Immediate effects - poison, acid, fire, etc. */
	if (!cutting)
	{
		for (i = 0; i < 4; i++)
		{
			/* skip empty blow slot */
			if (!r_ptr->blow[i].method) continue;

                        method = r_ptr->blow[i].method;
                        effect = r_ptr->blow[i].effect;
                        d_dice = r_ptr->blow[i].d_dice;
                        d_side = r_ptr->blow[i].d_side;
                        dam = damroll(d_dice, d_side) * o_ptr->pval / o_ptr->weight / 2;
                        idam = damroll(d_dice, d_side) * ((o_ptr->weight / o_ptr->pval > 2) ?
                                         o_ptr->weight / o_ptr->pval : 2);
			mdam = maxroll(d_dice, d_side) * 2;

			switch (method)
			{
				/* Methods that are meaningless after death */
				case RBM_BITE:
				case RBM_STING:
				case RBM_ENGULF:
				case RBM_DROOL:
				case RBM_SPIT:
				case RBM_GAZE:
				case RBM_WAIL:
				case RBM_BEG:
				case RBM_INSULT:
				case RBM_MOAN:
				continue;
			}
			switch (effect)
         {
            /* Effects that are meaningless after death */
			   case RBE_HURT:
			   case RBE_UN_BONUS:
				case RBE_UN_POWER:
			   case RBE_EAT_GOLD:
			   case RBE_EAT_ITEM:
			   case RBE_EAT_FOOD:
			   case RBE_EAT_LITE:
			   case RBE_ELEC:
			   case RBE_COLD:
			   case RBE_SHATTER:
            {
			   	break;
            }
			   case RBE_POISON:
            {
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
				   {
					   set_poisoned(p_ptr->poisoned + dam + idam + 10);
                  harmful = TRUE;
				   }
               break;
            }
			   case RBE_ACID:
				{
			   	/* Total Immunity */
					if (!(p_ptr->immune_acid || (dam <= 0)))
               {
	   				/* Resist the damage */
		   			if (p_ptr->resist_acid) dam = (dam + 2) / 3;
			   		if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

				   	/* Take damage */
						take_hit(dam, "acidic food");
						harmful = TRUE;
               }
               else
               {
               	set_oppose_acid(p_ptr->oppose_acid + idam);
					}
               break;
            }
			   case RBE_FIRE:
            {
            	/* Totally immune */
               if (p_ptr->immune_fire || (dam <= 0))
               {
				   	/* Resist the damage */
					   if (p_ptr->resist_fire) dam = (dam + 2) / 3;
   					if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

   					/* Take damage */
						take_hit(dam, "a fiery meal");
                  harmful = TRUE;
               }
               else
               {
               	set_oppose_fire(p_ptr->oppose_fire + idam);
               }
               break;
				}
	   		case RBE_BLIND:
				{
			   	if (!p_ptr->resist_blind)
				   {
					   set_blind(p_ptr->blind + dam * 2 + idam * 2 + 20);
   				}
               break;
            }
                                case RBE_CONFUSE:
				{
                                        if (!p_ptr->resist_conf)
                                        {
                                                set_confused(p_ptr->confused + dam + idam + 10);
                                        }
                                        if (!p_ptr->resist_chaos && rand_int(mdam - dam))
                                        {
                                                set_image(p_ptr->image + dam * 10 + idam * 10 + 100);
                                        }
                                        break;
                                }
                                case RBE_HALLU:
				{
                                        if (!p_ptr->resist_chaos && rand_int(mdam - dam))
                                        {
                                                set_image(p_ptr->image + dam * 10 + idam * 10 + 50);
                                        }
                                        break;
                                }
	   		case RBE_TERRIFY:
            {
			   	if (!p_ptr->resist_fear)
				   {
					   set_afraid(p_ptr->afraid + dam + idam + 10);
   				}
               break;
				}
			   case RBE_PARALYZE:
            {
	      		if (!p_ptr->free_act)
		      	{
			      	set_paralyzed(p_ptr->paralyzed + dam + idam + 10);
               }
            }
				case RBE_LOSE_STR:
            {
                                        do_dec_stat(A_STR, STAT_DEC_NORMAL);
               break;
            }
	   		case RBE_LOSE_INT:
            {
                do_dec_stat(A_INT, STAT_DEC_NORMAL);
               break;
            }
				case RBE_LOSE_WIS:
            {
                do_dec_stat(A_WIS, STAT_DEC_NORMAL);
               break;
            }
				case RBE_LOSE_DEX:
            {
                do_dec_stat(A_DEX, STAT_DEC_NORMAL);
               break;
            }
	   		case RBE_LOSE_CON:
            {
                do_dec_stat(A_CON, STAT_DEC_NORMAL);
               break;
   			}
	   		case RBE_LOSE_CHR:
            {
                do_dec_stat(A_CHR, STAT_DEC_NORMAL);
					break;
            }
            /* Don't eat Morgoth's corpse :) */
		   	case RBE_LOSE_ALL:
            {
		    do_dec_stat(A_STR, STAT_DEC_NORMAL);
		    do_dec_stat(A_INT, STAT_DEC_NORMAL);
		    do_dec_stat(A_WIS, STAT_DEC_NORMAL);
		    do_dec_stat(A_DEX, STAT_DEC_NORMAL);
		    do_dec_stat(A_CON, STAT_DEC_NORMAL);
		    do_dec_stat(A_CHR, STAT_DEC_NORMAL);
               o_ptr->pval = 0;
               break;
            }
                                case RBE_SANITY:
                                 {
                                   msg_print("You feel your sanity slipping away!");
                                   take_sanity_hit(dam, "eating an insane monster");
                                   break;
                                 }
            /* Unlife is bad to eat */
			   case RBE_EXP_10:
            {
               msg_print("A black aura surrounds the corpse!");

            	if (p_ptr->hold_life && (rand_int(100) < 50))
               {
               	msg_print("You keep hold of your life force!");
               }
					else
               {
               	s32b d = damroll(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
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
               o_ptr->pval = 0;
               break;
            }
			   case RBE_EXP_20:
            {
               msg_print("A black aura surrounds the corpse!");

					if (p_ptr->hold_life && (rand_int(100) < 50))
               {
						msg_print("You keep hold of your life force!");
               }
               else
               {
               	s32b d = damroll(20, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
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
               o_ptr->pval = 0;
               break;
            }
			   case RBE_EXP_40:
            {
               msg_print("A black aura surrounds the corpse!");

            	if (p_ptr->hold_life && (rand_int(100) < 50))
               {
               	msg_print("You keep hold of your life force!");
					}
               else
               {
	   				s32b d = damroll(40, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
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
               o_ptr->pval = 0;
               break;
            }
				case RBE_EXP_80:
            {
               msg_print("A black aura surrounds the corpse!");

					if (p_ptr->hold_life && (rand_int(100) < 50))
					{
               	msg_print("You keep hold of your life force!");
               }
               else
               {
               	s32b d = damroll(80, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
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
               o_ptr->pval = 0;
	   			break;
		   	}
   		}
		}
   } /* if (!cutting) */

	/*
	 * The organ that supplies breath attacks is not
	 * immediately emptied upon death, although some types
	 * of breath have no effect.
	 * AMHD's make rather risky meals, and deadly snacks.
	 */
	if (r_ptr->flags4 & RF4_BR_ACID && brpow > 0)
	{
		brdam = ((brpow / 3) > 1600 ? 1600 : (brpow / 3));

		msg_print("You are hit by a gush of acid!");

		/* Total Immunity */
		if (!(p_ptr->immune_acid || (brdam <= 0)))
		{
			/* Take damage */
			acid_dam(brdam, "a gush of acid");
			harmful = TRUE;
		}
		o_ptr->pval = 0;
	}
	else if (r_ptr->flags4 & RF4_BR_ACID)
	{
		set_oppose_acid(p_ptr->oppose_acid + rand_int(10) + 10);
	}
	if (r_ptr->flags4 & RF4_BR_ELEC && brpow > 0)
	{
		brdam = ((brpow / 3) > 1600 ? 1600 : (brpow / 3));

		msg_print("You receive a heavy shock!");

		/* Total Immunity */
		if (!(p_ptr->immune_elec || (brdam <= 0)))
		{
			/* Take damage */
			elec_dam(brdam, "an electric shock");
			harmful = TRUE;
		}
                o_ptr->weight = o_ptr->weight - brpow;
                o_ptr->pval = o_ptr->weight;
	}
	else if (r_ptr->flags4 & RF4_BR_ELEC)
	{
		set_oppose_elec(p_ptr->oppose_elec + rand_int(10) + 10);
	}
	if (r_ptr->flags4 & RF4_BR_FIRE && brpow > 0)
	{
		brdam = ((brpow / 3) > 1600 ? 1600 : (brpow / 3));

                msg_print("Roaring flames engulf you!");

		/* Total Immunity */
		if (!(p_ptr->immune_fire || (brdam <= 0)))
                {
			/* Take damage */
			fire_dam(brdam, "an explosion");
                        harmful = TRUE;
                }
 		o_ptr->pval = 0;
   }
   else if (r_ptr->flags4 & RF4_BR_FIRE)
   {
     	set_oppose_fire(p_ptr->oppose_fire + rand_int(10) + 10);
   }
   if (r_ptr->flags4 & RF4_BR_COLD && brpow > 0)
   {
		brdam = ((brpow / 3) > 1600 ? 1600 : (brpow / 3));

      msg_print("You are caught in a freezing liquid!");

		/* Total Immunity */
		if (!(p_ptr->immune_cold || (brdam <= 0)))
      {
			/* Take damage */
			cold_dam(brdam, "a chilling blast");
         harmful = TRUE;
      }
      o_ptr->weight = o_ptr->weight - brpow;
      o_ptr->pval = o_ptr->weight;
	}
   else if (r_ptr->flags4 & RF4_BR_COLD)
   {
     	set_oppose_cold(p_ptr->oppose_cold + rand_int(10) + 10);
	}
	if (r_ptr->flags4 & RF4_BR_POIS && brpow > 0)
   {
		brdam = ((brpow / 3) > 800 ? 800 : (brpow / 3));

      msg_print("You are surrounded by toxic gases!");

      /* Resist the damage */
      if (p_ptr->resist_pois) brdam = (brdam + 2) / 3;
		if (p_ptr->oppose_pois) brdam = (brdam + 2) / 3;

		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)set_poisoned(p_ptr->poisoned + rand_int(brdam) + 10);
		}

      /* Take damage */
      take_hit(brdam, "toxic gases");
      o_ptr->weight = o_ptr->weight - brpow;
      o_ptr->pval = o_ptr->weight;
      harmful = TRUE;
   }
	if (r_ptr->flags4 & RF4_BR_NETH && brpow > 0)
   {
		brdam = ((brpow / 6) > 550 ? 550 : (brpow / 6));

      msg_print("A black aura surrounds the corpse!");

                if (p_ptr->resist_neth)
		{
			brdam *= 6; brdam /= (randint(6) + 6);
		}
		else
		{
			if (p_ptr->hold_life && (rand_int(100) < 75))
			{
				msg_print("You keep hold of your life force!");
			}
			else if (p_ptr->hold_life)
			{
				msg_print("You feel your life slipping away!");
				lose_exp(200 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);
			}
			else
			{
				msg_print("You feel your life draining away!");
				lose_exp(200 + (p_ptr->exp/100) * MON_DRAIN_LIFE);
			}
		}

		/* Take damage */
		take_hit(brdam, "an unholy blast");
      harmful = TRUE;
      o_ptr->weight = o_ptr->weight - brpow;
      o_ptr->pval = o_ptr->weight;
   }
   if (r_ptr->flags4 & RF4_BR_CONF && brpow > 0)
   {
		msg_print("A strange liquid splashes on you!");
                if (!p_ptr->resist_conf)
				set_confused(p_ptr->confused + brdam + idam + 10);
      o_ptr->weight = o_ptr->weight - brpow;
      o_ptr->pval = o_ptr->weight;
	}
   if (r_ptr->flags4 & RF4_BR_CHAO && brpow > 0)
   {
		brdam = ((brpow / 6) > 600 ? 600 : (brpow / 6));

		msg_print("A swirling cloud surrounds you!");

		if (p_ptr->resist_chaos)
		{
			brdam *= 6; brdam /= (randint(6) + 6);
		}
                if (!p_ptr->resist_conf)
		{
			(void)set_confused(p_ptr->confused + rand_int(20) + 10);
		}
		if (!p_ptr->resist_chaos)
		{
			(void)set_image(p_ptr->image + randint(10));
		}
                if (!p_ptr->resist_neth && !p_ptr->resist_chaos)
		{
			if (p_ptr->hold_life && (rand_int(100) < 75))
			{
				msg_print("You keep hold of your life force!");
			}
			else if (p_ptr->hold_life)
			{
				msg_print("You feel your life slipping away!");
				lose_exp(500 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);
			}
			else
			{
				msg_print("You feel your life draining away!");
				lose_exp(5000 + (p_ptr->exp/100) * MON_DRAIN_LIFE);
			}
		}

		/* Take damage */
		take_hit(brdam, "chaotic forces");
      o_ptr->pval = 0;
	}
	if (r_ptr->flags4 & RF4_BR_DISE && brpow > 0)
   {
		brdam = ((brpow / 6) > 500 ? 500 : (brpow / 6));

		msg_print("You are blasted by raw mana!");

		if (p_ptr->resist_disen)
		{
			brdam *= 6; brdam /= (randint(6) + 6);
		}
		else
		{
			(void)apply_disenchant(0);
		}

		/* Take damage */
		take_hit(brdam, "raw mana");
      o_ptr->pval = 0;
	}
	if (r_ptr->flags4 & RF4_BR_PLAS && brpow > 0)
	{
		brdam = ((brpow / 6) > 150 ? 150 : (brpow / 6));

      msg_print("Searing flames engulf the corpse!");

		/* Resist the damage */
		if (p_ptr->resist_fire || p_ptr->oppose_fire) brdam = (brdam + 2) / 3;

		if (!p_ptr->resist_sound)
		{
			int k = (randint((brdam > 40) ? 35 : (brdam * 3 / 4 + 5)));
			(void)set_stun(p_ptr->stun + k);
		}

		/* Take damage */
		take_hit(brdam, "an explosion");
		harmful = TRUE;
      o_ptr->pval = 0;
	}

	/* Jellies are immune to acid only if they are already acidic. */
	if (strchr("j", r_ptr->d_char) && (r_ptr->flags3 & RF3_IM_ACID))
	{
		dam = damroll(8, 8);

		/* Total Immunity */
		if (!(p_ptr->immune_acid || (dam <= 0)))
		{
			/* Resist the damage */
			if (p_ptr->resist_acid) dam = (dam + 2) / 3;
			if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

			/* Take damage */
			take_hit(dam, "acidic food");
		}
		harmful = TRUE;
	}

	/*
	 * Jellies, kobolds, spiders, icky things, molds, and mushrooms
	 * are immune to poison because their body already contains
	 * poisonous chemicals.
	 */
	if (strchr("ijkmS,", r_ptr->d_char) && (r_ptr->flags3 & RF3_IM_POIS))
	{
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			set_poisoned(p_ptr->poisoned + rand_int(15) + 10);
		}
		harmful = TRUE;
	}

	/*
	 * Bad effects override good effects
	 * and hacked-up corpses lose intrinsics.
	 */
        if (!harmful && !cutting && (o_ptr->sval != SV_CORPSE_MEAT))
	{
		if (r_ptr->flags3 & RF3_IM_ACID)
			set_oppose_acid(p_ptr->oppose_acid + rand_int(10) + 10);
		if (r_ptr->flags3 & RF3_IM_ELEC)
			set_oppose_elec(p_ptr->oppose_elec + rand_int(10) + 10);
		if (r_ptr->flags3 & RF3_IM_FIRE)
			set_oppose_fire(p_ptr->oppose_fire + rand_int(10) + 10);
		if (r_ptr->flags3 & RF3_IM_COLD)
			set_oppose_cold(p_ptr->oppose_cold + rand_int(10) + 10);
		if (r_ptr->flags3 & RF3_IM_POIS)
			set_oppose_pois(p_ptr->oppose_pois + rand_int(10) + 10);
		if (r_ptr->flags3 & RF3_RES_NETH)
			set_protevil(p_ptr->protevil + rand_int(25) + 3 * r_ptr->level);
		if (r_ptr->flags3 & RF3_RES_PLAS)
			set_oppose_fire(p_ptr->oppose_fire + rand_int(20) + 20);
                if(r_ptr->flags2 & RF2_SHAPECHANGER)
                        (void)set_mimic(20 , rand_int(MIMIC_VALAR));
                if(r_ptr->flags3 & RF3_DEMON)
                        (void)set_mimic(30 , MIMIC_DEMON);
                if(r_ptr->flags3 & RF3_UNDEAD)
                        (void)set_mimic(30 , MIMIC_VAMPIRE);
                if(r_ptr->flags3 & RF3_NO_FEAR)
                        (void)set_afraid(0);
                if(r_ptr->flags3 & RF3_NO_STUN)
                        (void)set_stun(0);
                if(r_ptr->flags3 & RF3_NO_CONF)
                        (void)set_confused(0);
                if(r_ptr->flags6 & RF6_S_DRACONIAN)
                        summon_specific_friendly(py,px,dun_level,SUMMON_DRACONIAN,FALSE);
                if(r_ptr->flags6 & RF6_S_DEMON)
                        summon_specific_friendly(py,px,dun_level,SUMMON_DEMON,FALSE);
                if(r_ptr->flags6 & RF6_S_DEMON)
                        summon_specific_friendly(py,px,dun_level,SUMMON_KIN,FALSE);
                if(r_ptr->flags6 & RF6_S_HI_DEMON)
                        summon_specific_friendly(py,px,dun_level,SUMMON_HI_DEMON,FALSE);
                if(r_ptr->flags6 & RF6_S_MONSTER)
                        summon_specific_friendly(py,px,dun_level,0,FALSE);
                if(r_ptr->flags6 & RF6_S_MONSTERS)
                {
                        int k;
                        for (k = 0; k < 8; k++)
                                summon_specific_friendly(py,px,dun_level,0,FALSE);
                }
                if(r_ptr->flags6 & RF6_S_UNDEAD)
                        summon_specific_friendly(py,px,dun_level,SUMMON_UNDEAD,FALSE);
                if(r_ptr->flags6 & RF6_S_DRAGON)
                        summon_specific_friendly(py,px,dun_level,SUMMON_DRAGON,FALSE);
                if(r_ptr->flags6 & RF6_S_ANT)
                        summon_specific_friendly(py,px,dun_level,SUMMON_ANT,FALSE);
                if(r_ptr->flags6 & RF6_S_SPIDER)
                        summon_specific_friendly(py,px,dun_level,SUMMON_SPIDER,FALSE);
                if(r_ptr->flags6 & RF6_S_HOUND)
                        summon_specific_friendly(py,px,dun_level,SUMMON_HOUND,FALSE);
                if(r_ptr->flags6 & RF6_S_HYDRA)
                        summon_specific_friendly(py,px,dun_level,SUMMON_HYDRA,FALSE);
                if(r_ptr->flags6 & RF6_S_ANGEL)
                        summon_specific_friendly(py,px,dun_level,SUMMON_ANGEL,FALSE);
                if(r_ptr->flags6 & RF6_S_HI_DRAGON)
                        summon_specific_friendly(py,px,dun_level,SUMMON_HI_DRAGON,FALSE);
                if(r_ptr->flags6 & RF6_S_HI_UNDEAD)
                        summon_specific_friendly(py,px,dun_level,SUMMON_HI_UNDEAD,FALSE);
                if(r_ptr->flags6 & RF6_S_WRAITH)
                        summon_specific_friendly(py,px,dun_level,SUMMON_WRAITH,FALSE);
                if(r_ptr->flags6 & RF6_S_UNIQUE)
                        summon_specific_friendly(py,px,dun_level,SUMMON_UNIQUE,FALSE);
	}
}

/*
 * Hook to determine if an object is eatable
 */
static bool item_tester_hook_eatable(object_type *o_ptr)
{
        if (((o_ptr->tval==TV_FIRESTONE)&&(PRACE_FLAG(PR1_TP)))||
        (o_ptr->tval==TV_FOOD)||(o_ptr->tval==TV_CORPSE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
        int             item, ident, lev, fval = 0;

	object_type     *o_ptr;

        monster_race* r_ptr;

	cptr q, s;

        bool destroy = TRUE;

        /* Restrict choices to food and firestone */
        item_tester_hook = item_tester_hook_eatable;

	/* Get an item */
	q = "Eat which item? ";
	s = "You have nothing to eat.";
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


	/* Sound */
	sound(SOUND_EAT);


	/* Take a turn */
	energy_use = 100;

	/* Identity not known yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Analyze the food */
        if(o_ptr->tval==TV_FOOD){
	switch (o_ptr->sval)
	{
                case SV_FOOD_GREAT_HEALTH:
                {
                        p_ptr->hp_mod += 70;
                        msg_print("As you eat it you begin to feel your life flow getting stronger.");
                        ident = TRUE;
                        p_ptr->update |= (PU_HP);
                        break;
                }
		case SV_FOOD_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_BLINDNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(200) + 200))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			if (!p_ptr->resist_fear)
			{
				if (set_afraid(p_ptr->afraid + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (!p_ptr->resist_conf)
			{
				if (set_confused(p_ptr->confused + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_HALLUCINATION:
		{
			if (!p_ptr->resist_chaos)
			{
				if (set_image(p_ptr->image + rand_int(250) + 250))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_WEAKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food.");
			(void)do_dec_stat(A_STR, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food.");
			(void)do_dec_stat(A_CON, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_STUPIDITY:
		{
			take_hit(damroll(8, 8), "poisonous food.");
			(void)do_dec_stat(A_INT, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_NAIVETY:
		{
			take_hit(damroll(8, 8), "poisonous food.");
			(void)do_dec_stat(A_WIS, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_UNHEALTH:
		{
			take_hit(damroll(10, 10), "poisonous food.");
			(void)do_dec_stat(A_CON, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_DISEASE:
		{
			take_hit(damroll(10, 10), "poisonous food.");
			(void)do_dec_stat(A_STR, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_POISON:
		{
			if (set_poisoned(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			if (set_blind(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_PARANOIA:
		{
			if (set_afraid(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			if (set_confused(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_STR:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_CON:
		{
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORING:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_FOOD_FORTUNE_COOKIE:
		{
			char Rumor[80];

			msg_print("That tastes good.");
			msg_print("There is message in the cookie. It says:");
			msg_print(NULL);
			switch(randint(20))
			{
				case 1:
					get_rnd_line("chainswd.txt", Rumor);
					break;
				case 2:
					get_rnd_line("error.txt", Rumor);
					break;
				case 3:
				case 4:
				case 5:
					get_rnd_line("death.txt", Rumor);
					break;
				default:
					get_rnd_line("rumors.txt", Rumor);
			}
			msg_format("%s", Rumor);
			msg_print(NULL);
			ident = TRUE;
			break;
		}


		case SV_FOOD_RATION:
		case SV_FOOD_BISCUIT:
		case SV_FOOD_JERKY:
		{
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}

		case SV_FOOD_SLIME_MOLD:
		{
			msg_print("That tastes good.");

                        /* 2% chance of getting the mold power */
                        if (magik(2))
                        {
                                ADD_POWER(p_ptr->powers_mod, PWR_GROW_MOLD);
                                p_ptr->update |= PU_POWERS;
                        }

			ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
                        msg_print("That tastes very good.");
			(void)set_poisoned(0);
			(void)hp_player(damroll(4, 8));
                        set_food(PY_FOOD_MAX - 1);
			ident = TRUE;
			break;
		}

		case SV_FOOD_PINT_OF_ALE:
		case SV_FOOD_PINT_OF_WINE:
		{
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}

                case SV_FOOD_ATHELAS:
                {
                        msg_print("A fresh, clean essence rises, driving away wounds and poison.");
                        (void)set_poisoned(0);
                        (void)set_stun(0);
                        (void)set_cut(0);
                        if (p_ptr->black_breath)
                        {
                                msg_print("The hold of the Black Breath on you is broken!");
                                p_ptr->black_breath = FALSE;
                        }
                        ident = TRUE;
                        break;
                }
	}
        }else if(o_ptr->tval==TV_FIRESTONE){
        switch(o_ptr->sval){
                case SV_FIRE_SMALL:
		{
                        if(p_ptr->ctp<p_ptr->mtp){
                                msg_print("Grrrmfff ...");
                                p_ptr->ctp+=4;
                                if(p_ptr->ctp > p_ptr->mtp )
					p_ptr->ctp=p_ptr->mtp;
                                p_ptr->redraw |= (PR_TANK);
                                ident = TRUE;
                        }else msg_print("You can't eat more firestones, you vomit!");
			break;
		}
                case SV_FIRESTONE:
		{
                        if(p_ptr->ctp<p_ptr->mtp){
                                msg_print("Grrrrmmmmmmfffffff ...");
                                p_ptr->ctp+=10;
                                if(p_ptr->ctp > p_ptr->mtp )
					p_ptr->ctp=p_ptr->mtp;
                                p_ptr->redraw |= (PR_TANK);
                                ident = TRUE;
                        }else msg_print("You can't eat more firestones, you vomit!");
			break;
		}
        }
        }else{
                r_ptr = &r_info[o_ptr->pval2];
		switch (o_ptr->sval)
      {
        case SV_CORPSE_CORPSE:
      	{
         	/* Not all is edible. Apologies if messy. */
                if (((r_ptr->flags9 & RF9_DROP_SKELETON) && (o_ptr->weight <= (r_ptr->weight * 3) / 5)) ||
                                        (!(r_ptr->flags9 & RF9_DROP_SKELETON) && (o_ptr->weight <= (r_ptr->weight * 7) / 20)))
         	{
         		msg_print("There is not enough meat.");
            	return;
         	}
				if (!o_ptr->timeout) msg_print("Ugh! Raw meat!");
				else msg_print("That tastes good.");

				/* A pound of raw meat */
				o_ptr->pval -= 10;
                                o_ptr->weight -= 10;

				/* Corpses still have meat on them */
				destroy = FALSE;

				ident = TRUE;
				break;
			}
                        case SV_CORPSE_HEAD:
			{
				msg_print("You feel rather sick.");

				/* A pound of raw meat */
				o_ptr->pval -= 10;
                                o_ptr->weight -= 10;

				/* Corpses still have meat on them */
				destroy = FALSE;

				ident = TRUE;
				break;
			}
                        case SV_CORPSE_MEAT:
			{
				/* Just meat */
				if (!o_ptr->timeout) msg_print("You quickly swallow the meat.");
				else msg_print("That tastes good.");

				ident = TRUE;

				/* Those darn microorganisms */
                                if (!o_ptr->timeout && (o_ptr->weight > o_ptr->pval) &&
					 !(p_ptr->resist_pois || p_ptr->oppose_pois))
				{
                                        set_poisoned(p_ptr->poisoned + rand_int(o_ptr->weight - o_ptr->pval) +
                                        (o_ptr->weight - o_ptr->pval));
				}
				break;
			}
		}

		corpse_effect(o_ptr, FALSE);

		/* Less nutritious than food rations, but much more of it. */
		fval = (o_ptr->timeout) ? 2000 : 2500;

		/* Those darn microorganisms */
                if (!o_ptr->timeout && (o_ptr->weight - o_ptr->pval > 10) &&
			 !(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
                        set_poisoned(p_ptr->poisoned + rand_int(o_ptr->weight - o_ptr->pval) +
                        (o_ptr->weight - o_ptr->pval));
		}

		/* Partially cured */
                if (o_ptr->weight > o_ptr->timeout)
		{
			/* Adjust the "timeout" without overflowing */
                        o_ptr->timeout = (o_ptr->timeout * ((100 * o_ptr->timeout) / o_ptr->weight)) / 100;
		}
        }


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* We have tried it */
	object_tried(o_ptr);

	/* The player is now aware of the object */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

        if(!fval) fval = o_ptr->pval;

	/* Food can feed the player */
        if ((PRACE_FLAG(PR1_VAMPIRE)) || (p_ptr->mimic_form == MIMIC_VAMPIRE))
	{
		/* Reduced nutritional benefit */
                (void)set_food(p_ptr->food + (fval / 10));
		msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
		if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
			msg_print("Your hunger can only be satisfied with fresh blood!");
	}
        else if (PRACE_FLAG(PR1_NO_FOOD))
	{
		msg_print("The food of mortals is poor sustenance for you.");
                set_food(p_ptr->food + ((fval) / 20));
	}
        else if ((PRACE_FLAG(PR1_NO_STUN)) && (PRACE_FLAG(PR1_AC_LEVEL)) && (PRACE_FLAG(PR1_PASS_TREE)))
	{
                msg_print("Food is poor sustenance for you.");
                set_food(p_ptr->food + ((fval) / 20));
	}
	else
	{
                (void)set_food(p_ptr->food + fval);
	}


	/* Destroy a food in the pack */
        if(destroy)
        {
                if (item >= 0)
                {
                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                }

                /* Destroy a food on the floor */
                else
                {
                        floor_item_increase(0 - item, -1);
                        floor_item_describe(0 - item);
                        floor_item_optimize(0 - item);
                }
        }
}


/*
 * Cut a corpse up for convenient storage
 */
void do_cmd_cut_corpse(void)
{
        int item, meat = 0, not_meat = 0;

	object_type *o_ptr;
	object_type *i_ptr;

	object_type object_type_body;

	monster_race *r_ptr;

	cptr q, s;


	/* Restrict choices to corpses */
        item_tester_tval = TV_CORPSE;

	/* Get an item */
	q = "Hack up which corpse? ";
	s = "You have no corpses.";
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

        r_ptr = &r_info[o_ptr->pval2];

        if (o_ptr->sval != SV_CORPSE_CORPSE && o_ptr->sval != SV_CORPSE_HEAD)
        {
		msg_print ("You cannot split that.");
		return;
	}

	switch (o_ptr->sval)
        {
                case SV_CORPSE_CORPSE:
                {
                        if (r_ptr->flags9 & RF9_DROP_SKELETON)
			{
                               not_meat = (r_ptr->weight * 3) / 5;
			}
                        else
			{
                               not_meat = (r_ptr->weight * 7) / 20;
			}
                        meat = r_ptr->weight + r_ptr->weight / 10 - not_meat;
                        break;
                }
      case SV_CORPSE_HEAD:
      {
                                not_meat = r_ptr->weight / 150;
         meat = r_ptr->weight / 30 + r_ptr->weight / 300 - not_meat;
         break;
      }
   }

   if (o_ptr->weight <= not_meat || meat < 10)
   {
		msg_print("There is not enough meat.");
		return;
	}

	/* Hacking 10 pounds off */
   if (meat > 100) meat = 100;

	/* Take a turn */
        energy_use = 100;

   o_ptr->pval -= meat;
        o_ptr->weight -= meat;

	msg_print("You hack some meat off the corpse.");

	corpse_effect(o_ptr, TRUE);

	/* Get local object */
	i_ptr = &object_type_body;

   /* Make some meat */
        object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_MEAT));

	i_ptr->number = meat / 10;
        i_ptr->pval2 = o_ptr->pval2;

   /* Length of time before decay */
	i_ptr->pval = 1000 + rand_int(1000);

	if (inven_carry_okay(i_ptr))
	{
                inven_carry(i_ptr,TRUE);
	}
	else
	{
                drop_near(i_ptr, 0, py, px);
	}
}


/*
 * Use a potion to cure some meat
 *
 * Salt water works well.
 */
void do_cmd_cure_meat(void)
{
	int item, num, cure;

	object_type *o_ptr;
	object_type *i_ptr;

	cptr q, s;

	/* Restrict choices to corpses */
        item_tester_tval = TV_CORPSE;
        item_tester_hook = item_tester_hook_eatable;

	/* Get some meat */
	q = "Cure which meat? ";
	s = "You have no meat to cure.";
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

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get a potion */
	q = "Use which potion? ";
	s = "You have no potions to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		i_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		i_ptr = &o_list[0 - item];
	}

	if (i_ptr->number > 1)
	{
		/* Get a number */
		get_count(1, i_ptr->number);

		/* Save it */
                num = command_arg;
	}
	else
	{
		num = 1;
	}

	if (num == 0) return;

	/* Take a turn */
        energy_use = 100;

	q = "You soak the meat.";
	s = "You soak the meat.";

	switch (i_ptr->sval)
	{
		case SV_POTION_SALT_WATER:
		{
			q = "You salt the meat.";
			cure = 200 * num;
			break;
		}
		case SV_POTION_POISON:
		{
			q = "You poison the meat.";
			cure = 0;
			o_ptr->pval /= 2;
                        if (o_ptr->pval > o_ptr->weight) o_ptr->pval = o_ptr->weight;

			break;
		}
                case SV_POTION_CONFUSION:
		{
			cure = 80 * num;
			break;
		}
		case SV_POTION_SLOW_POISON:
		{
			cure = 20 * num;
			break;
		}
		case SV_POTION_CURE_POISON:
		{
			cure = 45 * num;
			break;
		}
		case SV_POTION_DEATH:
		{
			q = "You ruin the meat.";
			cure = 0;
			o_ptr->pval /= 10;
                        if (o_ptr->pval > o_ptr->weight) o_ptr->pval = o_ptr->weight / 2;

			break;
		}
		default:
		{
			cure = 0;
			break;
		}
	}

	/* Message */
	if (object_known_p(i_ptr)) msg_print(q);
	else msg_print(s);

	/* The meat is already spoiling */
        if (((o_ptr->sval == SV_CORPSE_MEAT) && (o_ptr->weight > o_ptr->pval)) ||
                 o_ptr->weight - o_ptr->pval > 10)
	{
                cure = (cure * o_ptr->pval) / (o_ptr->weight * 20);
	}

	/* Cure the meat */
	o_ptr->timeout += cure / o_ptr->number;

	if (o_ptr->timeout > o_ptr->pval) o_ptr->timeout = o_ptr->pval;

	/* Use up the potions in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, 0 - num);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Use up the potions on the floor */
	else
	{
		floor_item_increase(0 - item, 0 - num);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}


/*
 * Hook to determine if an object is quaffable
 */
static bool item_tester_hook_quaffable(object_type *o_ptr)
{
        if ((o_ptr->tval==TV_POTION)||(o_ptr->tval==TV_POTION2)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool quaff_potion(int tval, int sval, int pval)
{
	int ident = FALSE;

        if(tval==TV_POTION)
	switch (sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		case SV_POTION_SLIME_MOLD:
		{
			msg_print("You feel less thirsty.");
			ident = TRUE;
			break;
		}

		case SV_POTION_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint(25) + 15)) ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			msg_print("The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)set_poisoned(0);
			(void)set_paralyzed(p_ptr->paralyzed + 4);
			ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_BLINDNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(100) + 100))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_CONFUSION: /* Booze */
		{
			if (!((p_ptr->resist_conf) || (p_ptr->resist_chaos)))
			{
				if (set_confused(p_ptr->confused + rand_int(20) + 15))
				{
					ident = TRUE;
				}
				if (randint(2)==1)
				{
					if (set_image(p_ptr->image + rand_int(150) + 150))
					{
						ident = TRUE;
					}
				}
				if (randint(13)==1)
				{
					ident = TRUE;
					if(randint(3)==1) lose_all_info();
					else wiz_dark();
					teleport_player(100);
					wiz_dark();
					msg_print("You wake up elsewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
				}
			}
			break;
		}

		case SV_POTION_SLEEP:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_LOSE_MEMORIES:
		{
			if (!p_ptr->hold_life && (p_ptr->exp > 0))
			{
				msg_print("You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RUINATION:
		{
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination");
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
			ident = TRUE;
			break;
		}

		case SV_POTION_DEC_STR:
		{
			if (do_dec_stat(A_STR, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death");
			ident = TRUE;
			break;
		}

		case SV_POTION_INFRAVISION:
		{
			if (set_tim_infra(p_ptr->tim_infra + 100 + randint(100)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if (set_tim_invis(p_ptr->tim_invis + 12 + randint(12)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_SLOW_POISON:
		{
			if (set_poisoned(p_ptr->poisoned / 2)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_POISON:
		{
			if (set_poisoned(0)) ident = TRUE;
			break;
		}

		case SV_POTION_BOLDNESS:
		{
			if (set_afraid(0)) ident = TRUE;
			break;
		}

		case SV_POTION_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(25) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_POTION_RESIST_HEAT:
		{
			if (set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (set_afraid(0)) ident = TRUE;
			if (set_hero(p_ptr->hero + randint(25) + 25)) ident = TRUE;
			if (hp_player(10)) ident = TRUE;
			break;
		}

		case SV_POTION_BESERK_STRENGTH:
		{
			if (set_afraid(0)) ident = TRUE;
			if (set_shero(p_ptr->shero + randint(25) + 25)) ident = TRUE;
			if (hp_player(30)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			if (hp_player(damroll(6, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
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
                        if (p_ptr->black_breath)
                        {
                                msg_print("The hold of the Black Breath on you is broken!");
                        }
                        p_ptr->black_breath = FALSE;
			ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			if (restore_level()) ident = TRUE;
			break;
		}

		case SV_POTION_RES_STR:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_INT:
		{
			if (do_res_stat(A_INT)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_WIS:
		{
			if (do_res_stat(A_WIS)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_DEX:
		{
			if (do_res_stat(A_DEX)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_CON:
		{
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

		case SV_POTION_RES_CHR:
		{
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_STR:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_INT:
		{
			if (do_inc_stat(A_INT)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_WIS:
		{
			if (do_inc_stat(A_WIS)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_DEX:
		{
			if (do_inc_stat(A_DEX)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_CON:
		{
			if (do_inc_stat(A_CON)) ident = TRUE;
			break;
		}

		case SV_POTION_INC_CHR:
		{
			if (do_inc_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			if (do_inc_stat(A_INT)) ident = TRUE;
			if (do_inc_stat(A_WIS)) ident = TRUE;
			if (do_inc_stat(A_DEX)) ident = TRUE;
			if (do_inc_stat(A_CON)) ident = TRUE;
			if (do_inc_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			ident = TRUE;
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			msg_print("You begin to feel more enlightened...");
			msg_print(NULL);
                        wiz_lite_extra();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
                        self_knowledge(NULL);
			ident = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			msg_print("You begin to know yourself a little better...");
			msg_print(NULL);
                        self_knowledge(NULL);
			ident = TRUE;
			break;
		}

		case SV_POTION_EXPERIENCE:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESISTANCE:
		{
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
			ident = TRUE;
			break;
		}

		case SV_POTION_CURING:
		{
			if (hp_player(50)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
                        if (heal_insanity(50)) ident = TRUE;
			break;
		}

		case SV_POTION_INVULNERABILITY:
		{
			(void)set_invuln(p_ptr->invuln + randint(7) + 7);
			ident = TRUE;
			break;
		}

		case SV_POTION_NEW_LIFE:
		{
			do_cmd_rerate();
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
                                msg_print("You are cured of all corruptions.");
				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
			ident = TRUE;
			break;
		}
                case SV_POTION_BLOOD:
                      {
                        msg_print("You feel the blood of life running through your veins!");
                        ident = TRUE;
                        p_ptr->allow_one_death++;
                        break;
                      }
                case SV_POTION_MUTATION:
                {
                        msg_print("You feel the dark corruptions of Morgoth coming over you !");
                        gain_random_corruption(0);
                        ident = TRUE;
                        break;
                }
                case SV_POTION_INVIS:
		{
			int t = 30 + randint(30);

                        if (set_invis(p_ptr->tim_invis + t, 35))
			{
				ident = TRUE;
			}
                        set_tim_invis(p_ptr->tim_invis + t);
			break;
		}
                case SV_POTION_LEARNING:
                {
                        if((p_ptr->realm1) && (!PRACE_FLAGS(PR1_INNATE_SPELLS)))
                        {
                                int i = p_ptr->new_spells;

                                ident = TRUE;

                                /* Hack force to learn */
                                p_ptr->new_spells++;
                                do_cmd_study();
                                p_ptr->new_spells = i;

                                /* This indicate the number of extra spell the player has learned */
                                p_ptr->xtra_spells++;
                        }
                        else if (!PRACE_FLAGS(PR1_INNATE_SPELLS))
                        {
                                msg_print("You don't have to learn spells.");
                        }
                        else
                        {
                                msg_print("You can't learn any spell.");
                        }
                        break;
                }
                default:
                        process_hooks(HOOK_QUAFF, "(d,d,d)", tval, sval, pval);
                        break;
	}
        else
	switch (sval)
	{
                case SV_POTION2_MIMIC_ABOMINATION:
                case SV_POTION2_MIMIC_WOLF:
                case SV_POTION2_MIMIC_APE:
                case SV_POTION2_MIMIC_GOAT:
                case SV_POTION2_MIMIC_INSECT:
                case SV_POTION2_MIMIC_SPARROW:
                case SV_POTION2_MIMIC_STATUE:
                case SV_POTION2_MIMIC_VAMPIRE:
                case SV_POTION2_MIMIC_SPIDER:
                case SV_POTION2_MIMIC_MANA_BALL:
                case SV_POTION2_MIMIC_FIRE_CLOUD:
                case SV_POTION2_MIMIC_COLD_CLOUD:
                case SV_POTION2_MIMIC_CHAOS_CLOUD:
                        if(!p_ptr->mimic_form)
                        {
                                set_mimic(pval,sval);
                                /* Redraw title */
                                p_ptr->redraw |= (PR_TITLE);
                                /* Recalculate bonuses */
                                p_ptr->update |= (PU_BONUS);
                                ident=TRUE;
                        }
                        break;
                case SV_POTION2_CURE_LIGHT_SANITY:
                        if (heal_insanity(damroll(4,8))) ident = TRUE;
                        break;
                case SV_POTION2_CURE_SERIOUS_SANITY:
                        if (heal_insanity(damroll(8,8))) ident = TRUE;
                        break;
                case SV_POTION2_CURE_CRITICAL_SANITY:
                        if (heal_insanity(damroll(12,8))) ident = TRUE;
                        break;
                case SV_POTION2_CURE_SANITY:
                        if (heal_insanity(damroll(10,100))) ident = TRUE;
                        break;
                default:
                        process_hooks(HOOK_QUAFF, "(d,d,d)", tval, sval, pval);
                        break;
        }

	return(ident);
}

/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(void)
{
	int		item, ident, lev;

	object_type	*o_ptr;
        object_type     *q_ptr,forge;

	cptr q, s;

	/* Restrict choices to potions */
        item_tester_hook = item_tester_hook_quaffable;

	/* Get an item */
	q = "Quaff which potion? ";
	s = "You have no potions to quaff.";
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


	/* Sound */
	sound(SOUND_QUAFF);


	/* Take a turn */
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Analyze the potion */
	ident = quaff_potion(o_ptr->tval, o_ptr->sval, o_ptr->pval);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

        if (cp_ptr->magic_key == MKEY_ALCHEMY)
        {
                if (item >= 0)
                {
                        q_ptr = &forge;
                        object_prep(q_ptr, lookup_kind(TV_BOTTLE, 1));
                        q_ptr->number = 1;
                        object_aware(q_ptr);
                        object_known(q_ptr);

                        q_ptr->ident |= IDENT_STOREB;

                        (void)inven_carry(q_ptr, FALSE);
                }
        }

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Potions can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);


	/* Destroy a potion in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}


}

/*
 * Drink from a fountain
 */
void do_cmd_drink_fountain(void)
{
	cave_type *c_ptr = &cave[py][px];
	bool ident;
	int tval, sval, pval = 0;
	int i;

        /* We quaff or we fill ? */
        if (!get_check("Do you want to quaff from the fountain ?"))
        {
                do_cmd_fill_bottle();
                return;
        }

	if (c_ptr->special2 <= 0)
	{
              msg_print("The fountain is dried out.");
		return;
	}

	if (c_ptr->special <= SV_POTION_LAST) {
		tval = TV_POTION;
		sval = c_ptr->special;
	}
	else {
		tval = TV_POTION2;
		sval = c_ptr->special - SV_POTION_LAST;
	}

	for (i = 0; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		if (k_ptr->tval != tval) continue;
		if (k_ptr->sval != sval) continue;

		pval = k_ptr->pval;
		break;
	}

	ident = quaff_potion(tval, sval, pval);

	c_ptr->special2--;

	if (c_ptr->special2 == 0)
	{
		cave_set_feat(py, px, FEAT_EMPTY_FOUNTAIN);
	}

	if (ident) c_ptr->info |= CAVE_IDNT;
}

/*
 * Hook to determine if an object is fillable
 */
static bool item_tester_hook_fillable(object_type *o_ptr)
{
        if (o_ptr->tval==TV_BOTTLE) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/*
 * Fill an empty bottle
 */
void do_cmd_fill_bottle(void)
{
	cave_type *c_ptr = &cave[py][px];
	int tval, sval, item;
	object_type *q_ptr, forge;
	cptr q, s;

	/* Is the fountain empty? */
	if (c_ptr->special2 <= 0)
	{
		msg_print("The fountain is dried out");
		return;
	}

	/* Determine the tval/sval of the potion */
	if (c_ptr->special <= SV_POTION_LAST) {
		tval = TV_POTION;
		sval = c_ptr->special;
	}
	else {
		tval = TV_POTION2;
		sval = c_ptr->special - SV_POTION_LAST;
	}

	/* Restrict choices to bottles */
        item_tester_hook = item_tester_hook_fillable;

	/* Get an item */
	q = "Fill which bottle? ";
	s = "You have no bottles to fill.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Destroy a bottle in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Create the potion */
	q_ptr = &forge;
	object_prep(q_ptr, lookup_kind(tval, sval));
	q_ptr->number = 1;

	if (c_ptr->info & CAVE_IDNT)
	{
		object_aware(q_ptr);
		object_known(q_ptr);
	}

	inven_carry(q_ptr, FALSE);

	c_ptr->special2--;

	if (c_ptr->special2 == 0)
	{
		cave_set_feat(py, px, FEAT_EMPTY_FOUNTAIN);
	}
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if (((o_ptr->art_name) || artifact_p(o_ptr)) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
                o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;
                o_ptr->art_flags4 = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if ((artifact_p(o_ptr) || o_ptr->art_name) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
                o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;
                o_ptr->art_flags4 = 0;


		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	/* Notice */
	return (TRUE);
}


/*
 * Hook to determine if an object is readable
 */
static bool item_tester_hook_readable(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_SCROLL) || (o_ptr->tval == TV_PARCHEMENT)) return (TRUE);

	/* Assume not */
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
        int                     item, k, used_up, ident, lev;

	object_type		*o_ptr;
        object_type     *q_ptr,forge;

        char  Rumor[80];

	cptr q, s;

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
        item_tester_hook = item_tester_hook_readable;

	/* Get an item */
	q = "Read which scroll? ";
	s = "You have no scrolls to read.";
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


	/* Take a turn */
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Assume the scroll will get used up */
	used_up = TRUE;

	/* Analyze the scroll */
        if (o_ptr->tval == TV_SCROLL)
        {
	switch (o_ptr->sval)
	{
                case SV_SCROLL_MASS_RESURECTION:
                {
                        int k;

                        ident = TRUE;
                        msg_print("You feel the souls of the dead coming back from the Halls of Mandos.");

                        for (k = 0; k < max_r_idx; k++)
                        {
                                monster_race *r_ptr = &r_info[k];

                                if (r_ptr->flags1 & RF1_UNIQUE)
                                {
                                        r_ptr->max_num = 1;
                                }
                        }
                        break;
                }
                case SV_SCROLL_SPELL:
                        cast_spell(o_ptr->pval, o_ptr->pval2, 0);
                break;

                case SV_SCROLL_DEINCARNATION:
                {
                        if(!get_check("Do you realy want to leave your body(beware, it'll be destroyed) ?"))
                        {
                                used_up = FALSE;
                                break;
                        }

                        do_cmd_leave_body(FALSE);
                        ident = TRUE;

                        used_up = TRUE;
                        break;
                }
                case SV_SCROLL_RESET_RECALL:
                {
                        if (reset_recall())
                                msg_format("Recall reset to %s at level %d.",
                                           d_info[p_ptr->recall_dungeon].name + d_name, max_dlv[p_ptr->recall_dungeon]);
                        ident = TRUE;
                        break;
                }
                case SV_SCROLL_DIVINATION:
                {
                        int i, count = 0;
			char buf[120];

                        while(count < 1000)
                        {
                                count++;
                                i = rand_int(MAX_FATES);
                                if(!fates[i].fate) continue;
                                if(fates[i].know) continue;

				msg_print("A massage appeared on the scroll. It says:");
				msg_print(NULL);

				fate_desc(buf, i);
				msg_format("%s", buf);

				msg_print(NULL);
				msg_print("The scroll disappears in a puff of smoke!");

                                fates[i].know = TRUE;
                                ident = TRUE;
                                break;
                        }
                        break;
                }
		case SV_SCROLL_DARKNESS:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				(void)set_blind(p_ptr->blind + 3 + randint(5));
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(1);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
			if (curse_armor()) ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_WEAPON:
		{
			if (curse_weapon()) ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(py, px, dun_level, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

                case SV_SCROLL_SUMMON_MINE:
		{
                        if (summon_specific_friendly(py, px, dun_level, SUMMON_MINE, FALSE))
                        {
                                ident = TRUE;
                        }
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(py, px, dun_level, SUMMON_UNDEAD))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation()) ident = TRUE;
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			teleport_player(10);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			teleport_player(100);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			recall_player();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			ident = TRUE;
			if (!identify_fully()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			remove_all_curse();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_ARMOR:
		{
			ident = TRUE;
                        if (!enchant_spell(0, 0, 1, 0)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
                        if (!enchant_spell(1, 0, 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
                        if (!enchant_spell(0, 1, 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

                case SV_SCROLL_ENCHANT_WEAPON_PVAL:
		{
                        if (!enchant_spell(0, 0, 0, 1)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
                        if (!enchant_spell(0, 0, randint(3) + 2, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
                        if (!enchant_spell(randint(3), randint(3), 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (!recharge(60)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_GOLD:
		{
			if (detect_treasure()) ident = TRUE;
			if (detect_objects_gold()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_ITEM:
		{
			if (detect_objects_normal()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
			if (detect_monsters_invis()) ident = TRUE;
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (set_blessed(p_ptr->blessed + randint(12) + 6)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (set_blessed(p_ptr->blessed + randint(24) + 12)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (set_blessed(p_ptr->blessed + randint(48) + 24)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			warding_glyph();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
				destroy_area(py, px, 15, TRUE);
			else
				msg_print("The dungeon trembles...");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(60)) ident = TRUE;
			break;
		}

		case SV_SCROLL_GENOCIDE:
		{
			(void)genocide(TRUE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_GENOCIDE:
		{
			(void)mass_genocide(TRUE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(py, px, 1, TRUE, FALSE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(py, px, randint(2) + 1, TRUE, FALSE);
			ident = TRUE;
			break;
		}

		/* New Zangband scrolls */
		case SV_SCROLL_FIRE:
		{
			fire_ball(GF_FIRE, 0, 150, 4);
			/* Note: "Double" damage since it is centered on the player ... */
			if (!(p_ptr->oppose_fire || p_ptr->resist_fire || p_ptr->immune_fire))
                                take_hit(50+randint(50)+(p_ptr->sensible_fire)?20:0, "a Scroll of Fire");
			ident = TRUE;
			break;
		}


		case SV_SCROLL_ICE:
		{
			fire_ball(GF_ICE, 0, 175, 4);
			if (!(p_ptr->oppose_cold || p_ptr->resist_cold || p_ptr->immune_cold))
				take_hit(100+randint(100), "a Scroll of Ice");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_CHAOS:
		{
			fire_ball(GF_CHAOS, 0, 222, 4);
			if (!p_ptr->resist_chaos)
                                take_hit(111+randint(111), "a Scroll of Chaos");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_RUMOR:
		{
			msg_print("There is message on the scroll. It says:");
			msg_print(NULL);
			switch(randint(20))
			{
				case 1:
					get_rnd_line("chainswd.txt", Rumor);
					break;
				case 2:
					get_rnd_line("error.txt", Rumor);
					break;
				case 3:
				case 4:
				case 5:
					get_rnd_line("death.txt", Rumor);
					break;
				default:
					get_rnd_line("rumors.txt", Rumor);
			}
			msg_format("%s", Rumor);
			msg_print(NULL);
			msg_print("The scroll disappears in a puff of smoke!");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ARTIFACT:
		{
			(void) artifact_scroll();
			ident = TRUE;
			break;
		}
                default:
                        process_hooks(HOOK_READ, "(d,d)", o_ptr->tval, o_ptr->sval);
                        break;
	}
        }
        else
        {
                /* Maps */
                if (o_ptr->sval >= 200)
                {
                        int i, n;
                        char path[1024], fil[20];

                        sprintf(path, "%s", ANGBAND_DIR_FILE);
                        sprintf(fil, "book-%d.txt",o_ptr->sval);

                        n = atoi(get_line(fil, path, -1));

                        /* Parse all the fields */
                        for (i = 0; i < n; i += 4)
                        {
                                /* Grab the fields */
                                int x = atoi(get_line(fil, path, i + 0));
                                int y = atoi(get_line(fil, path, i + 1));
                                int w = atoi(get_line(fil, path, i + 2));
                                int h = atoi(get_line(fil, path, i + 3));

                                reveal_wilderness_around_player(y, x, h, w);
                        }
                }
                /* Normal parchements */
                else
                {
			/* Save screen */
			screen_save();

                        /* Get the filename */
                        q = format("book-%d.txt",o_ptr->sval);

                        /* Peruse the help file */
                        (void)show_file(q, NULL, 0, 0);

			/* Load screen */
			screen_load();

                        if (o_ptr->sval >= 100)
                        {
                                inscription_info[o_ptr->sval - 100].know = TRUE;
                        }

                        used_up = FALSE;
                }
        }


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Hack -- allow certain scrolls to be "preserved" */
	if (!used_up) return;

	sound(SOUND_SCROLL);

	/* Destroy a scroll in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a scroll on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

        if (cp_ptr->magic_key == MKEY_ALCHEMY)
        {
                if (item >= 0)
                {
                        q_ptr = &forge;
                        object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_NOTHING));
                        object_aware(q_ptr);
                        object_known(q_ptr);

                        q_ptr->ident |= IDENT_STOREB;

                        (void)inven_carry(q_ptr, FALSE);
                }
        }
}







/*
 * Use a staff.			-RAK-
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
void do_cmd_use_staff(void)
{
	int			item, ident, chance, k, lev;

	object_type		*o_ptr;
        u32b f1, f2, f3, f4, f5, esp;

	cptr q, s;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
	q = "Use which staff? ";
	s = "You have no staff to use.";
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


	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the staffs.");
		return;
	}


	/* Take a turn */
        if (PRACE_FLAGS(PR1_ZERO_FAIL) && (cp_ptr->spell_book == TV_MAGERY_BOOK))
        {
                energy_use = 75;
                if (p_ptr->lev>=35) energy_use = 33;
                else if (p_ptr->lev>=15) energy_use = 50;
        }
        else energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

        /* Extract object flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* Is it simple to use ? */
        if (f4 & TR4_EASY_USE)
        {
                chance *= 10;
        }

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the staff properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_print("The staff has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}


	/* Sound */
	sound(SOUND_ZAP);


	/* Analyze the staff */
	switch (o_ptr->sval)
	{
                case SV_STAFF_GANDALF:
		{
                        project_hack(GF_HOLY_FIRE, 150);
                        ident = TRUE;
			break;
		}

                case SV_STAFF_WISHING:
		{
                        make_wish();
                        ident = TRUE;
			break;
		}

		case SV_STAFF_DARKNESS:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				if (set_blind(p_ptr->blind + 3 + randint(5))) ident = TRUE;
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint(30) + 15)) ident = TRUE;
			break;
		}

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters()) ident = TRUE;
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			for (k = 0; k < randint(4); k++)
			{
				if (summon_specific(py, px, dun_level, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_TELEPORTATION:
		{
			teleport_player(100);
			ident = TRUE;
			break;
		}

		case SV_STAFF_IDENTIFY:
		{
			if (!ident_spell()) use_charge = FALSE;
			ident = TRUE;
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				if (!p_ptr->blind)
				{
					msg_print("The staff glows blue for a moment...");
				}
				ident = TRUE;
			}
			break;
		}

		case SV_STAFF_STARLITE:
		{
			if (!p_ptr->blind)
			{
				msg_print("The end of the staff glows brightly...");
			}
			for (k = 0; k < 8; k++) lite_line(ddd[k]);
			ident = TRUE;
			break;
		}

		case SV_STAFF_LITE:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_STAFF_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_GOLD:
		{
			if (detect_treasure()) ident = TRUE;
			if (detect_objects_gold()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_ITEM:
		{
			if (detect_objects_normal()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
			if (detect_monsters_invis()) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (detect_monsters_evil()) ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(randint(8))) ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_THE_MAGI:
		{
			if (do_res_stat(A_INT)) ident = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				ident = TRUE;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			if (sleep_monsters()) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			if (slow_monsters()) ident = TRUE;
			break;
		}

		case SV_STAFF_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_STAFF_PROBING:
		{
			probing();
			ident = TRUE;
			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			if (dispel_evil(60)) ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(120)) ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(120)) ident = TRUE;
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_GENOCIDE:
		{
			(void)genocide(TRUE);
			ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
				earthquake(py, px, 10);
			else
				msg_print("The dungeon trembles...");
			ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
			{
				destroy_area(py, px, 15, TRUE);
				ident = TRUE;
			}
			break;
		}
                default:
                        process_hooks(HOOK_USE, "(d,d)", o_ptr->tval, o_ptr->sval);
                        break;
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Hack -- some uses are "free" */
	if (!use_charge) return;


	/* Use a single charge */
	o_ptr->pval--;

	/* XXX Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity */
		q_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		item = inven_carry(q_ptr, FALSE);

		/* Message */
		msg_print("You unstack your staff.");
	}

	/* Describe charges in the pack */
	if (item >= 0)
	{
		inven_item_charges(item);
	}

	/* Describe charges on the floor */
	else
	{
		floor_item_charges(0 - item);
	}
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
void do_cmd_aim_wand(void)
{
	int			item, lev, ident, chance, dir, sval;

	object_type		*o_ptr;

	cptr q, s;

        u32b f1, f2, f3, f4, f5, esp;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item */
	q = "Aim which wand? ";
	s = "You have no wand to aim.";
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


	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the wands.");
		return;
	}


	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;


	/* Take a turn */
        if (PRACE_FLAGS(PR1_ZERO_FAIL) && (cp_ptr->spell_book == TV_MAGERY_BOOK))
        {
                energy_use = 75;
                if (p_ptr->lev>=35) energy_use = 33;
                else if (p_ptr->lev>=15) energy_use = 50;
        }
        else energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Get the level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

        /* Extract object flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* Is it simple to use ? */
        if (f4 & TR4_EASY_USE)
        {
                chance *= 10;
        }

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the wand properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* The wand is already empty! */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_print("The wand has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}


	/* Sound */
	sound(SOUND_ZAP);


	/* XXX Hack -- Extract the "sval" effect */
	sval = o_ptr->sval;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);


	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			if (clone_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_TRAP_DOOR_DEST:
		{
			if (destroy_door(dir)) ident = TRUE;
			break;
		}

                case SV_WAND_THRAIN:
		case SV_WAND_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			ident = TRUE;
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			if (slow_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			if (confuse_monster(dir, 10)) ident = TRUE;
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			if (fear_monster(dir, 10)) ident = TRUE;
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			if (drain_life(dir, 75)) ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 12, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, 6));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(3, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_CHARM_MONSTER:
		{
			if (charm_monster(dir, 45))
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(6, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(3, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 32, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 72, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 48, 2);
			ident = TRUE;
			break;
		}

                case SV_WAND_WALL_CREATION:
		{
                        project_hook(GF_STONE_WALL, dir, 1, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        ident = TRUE;
			break;
		}

		case SV_WAND_WONDER:
		{
			msg_print("Oops.  Wand of wonder activated.");
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 100, 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 80, 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
					fire_ball(GF_ACID, dir, 100, 3);
					break;
				}

				case 2:
				{
					fire_ball(GF_ELEC, dir, 80, 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 100, 3);
					break;
				}

				case 4:
				{
					fire_ball(GF_COLD, dir, 80, 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 60, 3);
					break;
				}
			}

			ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 125)) ident = TRUE;
			break;
		}

		case SV_WAND_ROCKETS:
		{
			msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir, 75 + (randint(50)), 2);
			ident = TRUE;
			break;
		}
                default:
                        process_hooks(HOOK_AIM, "(d,d)", o_ptr->tval, o_ptr->sval);
                        break;
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Use a single charge */
	o_ptr->pval--;

	/* Describe the charges in the pack */
	if (item >= 0)
	{
		inven_item_charges(item);
	}

	/* Describe the charges on the floor */
	else
	{
		floor_item_charges(0 - item);
	}
}





/*
 * Activate (zap) a Rod
 *
 * Unstack fully charged rods as needed.
 *
 * Hack -- rods of perception/genocide can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 */

/*
 * Hook to determine if an object is zapable
 */
static bool item_tester_hook_zapable(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_ROD_MAIN)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Hook to determine if an object is attachable
 */
static bool item_tester_hook_attachable(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_ROD_MAIN) && (o_ptr->pval == SV_ROD_NOTHING)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Combine a rod and a rod tip */
void zap_combine_rod_tip(object_type *q_ptr, int tip_item)
{
        int item;
        object_type *o_ptr;
        object_kind *k_ptr;
	cptr q, s;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* Restrict choices to rods */
        item_tester_hook = item_tester_hook_attachable;

	/* Get an item */
        q = "Attach the rod tip with which rod? ";
        s = "You have no rod to attach to.";
        if (!get_item(&item, q, s, (USE_INVEN))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
                k_ptr = &k_info[o_ptr->k_idx];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
                k_ptr = &k_info[o_ptr->k_idx];
	}

        /*
         * The rod must have at least the same mana capacity as the
         * rod tip spell needs
         */
        if (o_ptr->pval2 < q_ptr->pval)
        {
                msg_print("This rod doesn't have enough mana for the rod tip.");
                return;
        }

        /* Attach the tip to the rod */
        o_ptr->pval = q_ptr->sval;

        /* Destroy a rod tip in the pack */
        if (tip_item >= 0)
	{
                inven_item_increase(tip_item, -1);
                inven_item_describe(tip_item);
                inven_item_optimize(tip_item);
	}
        /* Destroy a rod tip on the floor */
	else
	{
                floor_item_increase(0 - tip_item, -1);
                floor_item_describe(0 - tip_item);
                floor_item_optimize(0 - tip_item);
	}
}

void do_cmd_zap_rod(void)
{
	int                 item, ident, chance, dir, lev;

	object_type		*o_ptr;
        object_kind *k_ptr;
        object_kind *tip_ptr;
        u32b f1, f2, f3, f4, f5, esp;

	cptr q, s;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }


	/* Restrict choices to rods */
        item_tester_hook = item_tester_hook_zapable;

	/* Get an item */
        q = "Zap which rod? ";
	s = "You have no rod to zap.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
                k_ptr = &k_info[o_ptr->k_idx];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
                k_ptr = &k_info[o_ptr->k_idx];
	}

        /* "Zapping" a Rod Tip on rod of nothing will attach it */
        if (o_ptr->tval == TV_ROD)
        {
                if (item >= 0)
                {
                        zap_combine_rod_tip(o_ptr, item);
                        return;
                }
                else
                {
                        msg_print("You can't a rod tip that's on the floor.");
                        return;
                }
        }

	/* Get a direction (unless KNOWN not to need it) */
        if (((o_ptr->pval >= SV_ROD_MIN_DIRECTION) && !(o_ptr->pval == SV_ROD_DETECT_TRAP) && !(o_ptr->pval == SV_ROD_HAVOC) && !(o_ptr->sval == SV_ROD_HOME)) ||
	     !object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return;
	}

	/* Take a turn */
        if (PRACE_FLAGS(PR1_ZERO_FAIL) && (cp_ptr->spell_book == TV_MAGERY_BOOK))
        {
                energy_use = 75;
                if (p_ptr->lev>=35) energy_use = 33;
                else if (p_ptr->lev>=15) energy_use = 50;
        }
        else energy_use = 100;

        /* Examine the rod */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        if (f4 & TR4_FAST_CAST) energy_use /= 2;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
        tip_ptr = &k_info[lookup_kind(TV_ROD, o_ptr->pval)];
        lev = k_info[lookup_kind(TV_ROD, o_ptr->pval)].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

        /* Is it simple to use ? */
        if (f4 & TR4_EASY_USE)
        {
                chance *= 10;
        }

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the rod properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* A single rod is still charging */
        if (o_ptr->timeout < ((f4 & TR4_CHEAPNESS)?tip_ptr->pval / 2:tip_ptr->pval))
	{
		if (flush_failure) flush();
                msg_print("The rod does not have enough mana yet.");
		return;
	}

        /* Increase the timeout by the rod kind's pval. */
        o_ptr->timeout -= (f4 & TR4_CHEAPNESS)?tip_ptr->pval / 2:tip_ptr->pval;

	/* Sound */
	sound(SOUND_ZAP);

	/* Analyze the rod */
        switch (o_ptr->pval)
	{
                case SV_ROD_HOME:
		{
                        ident = TRUE;
                        do_cmd_home_trump();
			break;
		}

		case SV_ROD_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) use_charge = FALSE;
			break;
		}

		case SV_ROD_RECALL:
		{
			recall_player();
			ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_ROD_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
			detect_all();
			ident = TRUE;
			break;
		}

		case SV_ROD_PROBING:
		{
			probing();
			ident = TRUE;
			break;
		}

		case SV_ROD_CURING:
		{
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(500)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_ROD_RESTORATION:
		{
			if (restore_level()) ident = TRUE;
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_ROD_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			ident = TRUE;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 75)) ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(6, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(3, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(8, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(5, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 32, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 72, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 48, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_HAVOC:
		{
			call_chaos();
			ident = TRUE;
			break;
		}
                default:
                        process_hooks(HOOK_ZAP, "(d,d)", o_ptr->tval, o_ptr->sval);
                        break;
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Hack -- deal with cancelled zap */
	if (!use_charge)
	{
                o_ptr->timeout += (f4 & TR4_CHEAPNESS)?tip_ptr->pval / 2:tip_ptr->pval;
		return;
	}
}




/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
        u32b f1, f2, f3, f4, f5, esp;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}



/*
 * Hack -- activate the ring of power
 */
int ring_of_power()
{
        char ch = 0, p = 0;
        int plev = p_ptr->lev;
        int timeout = 0;

        /* Select power to use */
        while (TRUE)
        {
                if (!get_com("[S]ummon a wraith, [R]ule the world or [C]ast a powerful attack ?", &ch))
                {
                        p = 0;
                        break;
                }
                if (ch == 'S' || ch == 's')
                {
                        p = 1;
                        break;
                }
                if (ch == 'R' || ch == 'r')
                {
                        p = 2;
                        break;
                }
                if (ch == 'C' || ch == 'c')
                {
                        p = 3;
                        break;
                }
        }
        if (p == 1)
        {
                if (summon_specific_friendly(py, px, ((plev * 3) / 2),
                   (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD),
                   (bool)(((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE)))
                {
                        msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
                        msg_print("Ancient, long-dead forms arise from the ground to serve you!");
                }
                timeout = 200 + rand_int(200);
        }
        else if(p == 2)
        {
                msg_print("The power of the ring destroys the world!");
                msg_print("The world changes!");
                if (autosave_l)
                {
                    is_autosave = TRUE;
                    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
                /* Leaving */
                p_ptr->leaving = TRUE;
                timeout = 250 + rand_int(250);
        }
        else if(p == 3)
        {
                int dir;

                if (!get_aim_dir(&dir)) return 0;
                if(rand_int(3) == 0)
                {
                        msg_print("You call the fire of the Mount Doom!");
                        fire_ball(GF_METEOR, dir, 600, 4);
                }else{
                        msg_print("Your ring tries to take possession of your ennemy's mind!");
                        fire_bolt(GF_CHARM, dir, 600);
                }
                timeout = 300 + rand_int(300);
        }
        return timeout;
}




/*
 * Enchant some bolts
 */
bool brand_bolts(void)
{
	int i;

	/* Use the first acceptable bolts */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artifacts and ego-items */
		if (o_ptr->art_name || artifact_p(o_ptr) || ego_item_p(o_ptr))
			continue;

		/* Skip cursed/broken items */
                if (cursed_p(o_ptr)) continue;

		/* Randomize */
		if (rand_int(100) < 75) continue;

		/* Message */
		msg_print("Your bolts are covered in a fiery aura!");

		/* Ego-item */
                o_ptr->name2 = EGO_FLAME;

                /* Apply the ego */
                apply_magic(o_ptr, dun_level, FALSE, FALSE, FALSE);

		/* Enchant */
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* Notice */
		return (TRUE);
	}

	/* Flush */
	if (flush_failure) flush();

	/* Fail */
	msg_print("The fiery enchantment failed.");

	/* Notice */
	return (TRUE);
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
        int             item, i, k, dir, lev, chance,ii,ij,plev=p_ptr->lev;
        char ch,spell_choice;

	object_type     *o_ptr;

        u32b f1, f2, f3, f4, f5, esp;

	cptr q, s;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
        command_see = TRUE;
        command_wrk = USE_EQUIP;
	q = "Activate which item? ";
	s = "You have nothing to activate.";
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


        if((o_ptr->tval != TV_RANDART)&&(o_ptr->tval != TV_EGG))
                if(item < INVEN_WIELD){msg_print("You must wear it to activate it.");return;}

	/* Take a turn */
	energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
        if (artifact_p(o_ptr)){
                if (o_ptr->tval == TV_RANDART) {
                        lev = random_artifacts[o_ptr->sval].level;
                } else {
                        lev = a_info[o_ptr->name1].level;
                }
        }

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

        /* Extract object flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* Is it simple to use ? */
        if (f4 & TR4_EASY_USE)
        {
                chance *= 10;
        }

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
        if ((USE_REALM(REALM_MUSIC)) && (o_ptr->tval == TV_INSTRUMENT))
                ;
        else
                if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
                {
                        if (flush_failure) flush();
                        msg_print("You failed to activate it properly.");
                        sound(SOUND_FAIL);
                        return;
                }

        if ((o_ptr->tval == TV_INSTRUMENT)&&(o_ptr->timeout))
        {
                msg_print("You deactivate it...");
                p_ptr->music = 255;
		return;
        }

        if ((o_ptr->tval == TV_EGG)&&(o_ptr->timeout))
        {
                msg_print("You resume the development of the egg...");
                o_ptr->timeout = 0;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
        }

	/* Check the recharge */
        if ((o_ptr->timeout) && ((!is_ego_p(o_ptr, EGO_MSTAFF_SPELL)) || o_ptr->xtra2))
	{
		msg_print("It whines, glows and fades...");
		return;
	}


	/* Activate the artifact */
	msg_print("You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

        if (is_ego_p(o_ptr, EGO_MSTAFF_SPELL))
        {
                        while (TRUE)
			{
                                if (!get_com("Use Spell [1] or [2] ?", &ch))
				{
                                        spell_choice=0;
					break;
				}

                                if (ch == '1')
				{
                                        spell_choice=1;
					break;
				}

                                if (ch == '2')
				{
                                        spell_choice=2;
					break;
				}
			}
                if ((spell_choice==1) && o_ptr->timeout) {msg_print("The spell 1 is still charging !");return;}
                if ((spell_choice==2) && o_ptr->xtra2) {msg_print("The spell 2 is still charging !");return;}

                if (spell_choice == 1) activate_spell(o_ptr, spell_choice);
                if (spell_choice == 2) activate_spell(o_ptr, spell_choice);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
        }

        else if (o_ptr->tval == TV_EGG)
	{
                msg_print("You stop the development of the egg.");
                o_ptr->timeout = 1;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	/* Artifacts */
	else if (o_ptr->name1)
	{
		/* Choose effect */
		switch (o_ptr->name1)
		{
                        case ART_GILGALAD:
                        {
                                for (k = 1; k < 10; k++)
                                {
                                        if (k - 5) fire_beam(GF_LITE, k, 75);
                                }

                                o_ptr->timeout = rand_int(75) + 75;
                                break;
                        }

                        case ART_CELEBRIMBOR:
                        {
                                set_tim_esp(p_ptr->tim_esp + randint(20) + 20);

                                o_ptr->timeout = rand_int(50) + 20;
                                break;
                        }

                        case ART_SKULLCLEAVER:
                        {
                                destroy_area(py, px, 15, TRUE);
                                o_ptr->timeout = rand_int(200) + 200;
                                break;
                        }

                        case ART_HARADRIM:
                        {
                                set_afraid(0);
                                set_shero(p_ptr->shero + randint(25) + 25);
                                hp_player(30);
                                o_ptr->timeout = rand_int(50) + 50;
                                break;
                        }

                        case ART_FUNDIN:
                        {
                                dispel_evil(p_ptr->lev * 4);
                                o_ptr->timeout = rand_int(100) + 100;
                                break;
                        }

                        case ART_NAIN:
                        {
				if (!get_aim_dir(&dir)) return;
                                wall_to_mud(dir);
                                o_ptr->timeout = rand_int(5) + 7;
                                break;
                        }

                        case ART_EOL:
                        {
				if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_MANA, dir, damroll(9, 8));
                                o_ptr->timeout = rand_int(7) + 7;
                                break;
                        }

                        case ART_UMBAR:
                        {
				if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_MISSILE, dir, damroll(10, 10));
                                o_ptr->timeout = rand_int(20) + 20;
                                break;
                        }

                        case ART_NUMENOR:
                        {
                                /* Give full knowledge */
                                /* Hack -- Maximal info */
                                monster_race *r_ptr;
                                cave_type *c_ptr;
                                int x, y, m;

                                if (!tgt_pt(&x, &y)) break;

                                c_ptr = &cave[y][x];
                                if (!c_ptr->m_idx) break;

                                r_ptr = &r_info[c_ptr->m_idx];

                                /* Observe "maximal" attacks */
                                for (m = 0; m < 4; m++)
                                {
                                        /* Examine "actual" blows */
                                        if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
                                        {
                                                /* Hack -- maximal observations */
                                                r_ptr->r_blows[m] = MAX_UCHAR;
                                        }
                                }

                                /* Hack -- maximal drops */
                                r_ptr->r_drop_gold = r_ptr->r_drop_item =
                                (((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
                                 ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
                                 ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
                                 ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
                                 ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
                                 ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

                                /* Hack -- but only "valid" drops */
                                if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
                                if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

                                /* Hack -- observe many spells */
                                r_ptr->r_cast_inate = MAX_UCHAR;
                                r_ptr->r_cast_spell = MAX_UCHAR;

                                /* Hack -- know all the flags */
                                r_ptr->r_flags1 = r_ptr->flags1;
                                r_ptr->r_flags2 = r_ptr->flags2;
                                r_ptr->r_flags3 = r_ptr->flags3;
                                r_ptr->r_flags4 = r_ptr->flags4;
                                r_ptr->r_flags5 = r_ptr->flags5;
                                r_ptr->r_flags6 = r_ptr->flags6;
                                r_ptr->r_flags4 = r_ptr->flags7;
                                r_ptr->r_flags5 = r_ptr->flags8;
                                r_ptr->r_flags6 = r_ptr->flags9;

                                o_ptr->timeout = rand_int(200) + 500;
                                break;
                        }

                        case ART_KNOWLEDGE:
                        {
                                identify_fully();
                                take_sanity_hit(damroll(10, 7), "the sounds of deads");
                                o_ptr->timeout = rand_int(200) + 100;
                                break;
                        }

			case ART_GALADRIEL:
			{
				msg_print("The phial wells with clear light...");
				lite_area(damroll(2, 15), 3);
                                fire_ball(GF_LITE, 0, 10, 5);
				o_ptr->timeout = rand_int(10) + 10;
				break;
			}

                        case ART_UNDEATH:
			{
                                msg_print("The phial wells with dark light...");
                                unlite_area(damroll(2, 15), 3);
                                take_hit(damroll(10, 10), "activating The Phial of Undeath");
                                (void)dec_stat(A_DEX, 25, STAT_DEC_PERMANENT);
                                (void)dec_stat(A_WIS, 25, STAT_DEC_PERMANENT);
                                (void)dec_stat(A_CON, 25, STAT_DEC_PERMANENT);
                                (void)dec_stat(A_STR, 25, STAT_DEC_PERMANENT);
                                (void)dec_stat(A_CHR, 25, STAT_DEC_PERMANENT);
                                (void)dec_stat(A_INT, 25, STAT_DEC_PERMANENT);
                                o_ptr->timeout = rand_int(10) + 10;
				break;
			}

			case ART_ELENDIL:
			{
				msg_print("The star shines brightly...");
				map_area();
				lite_area(damroll(2, 15), 3);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case ART_THRAIN:
			{
				msg_print("The stone glows a deep green...");
                                detect_all();
                                o_ptr->timeout = rand_int(30) + 30;
                                break;
			}

                        case ART_HIMRING:
			{
				k = 3 * p_ptr->lev;
				(void)set_protevil(p_ptr->protevil + randint(25) + k);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case ART_CARLAMMAS:
			{
				msg_print("The amulet lets out a shrill wail...");
				k = 3 * p_ptr->lev;
				(void)set_protevil(p_ptr->protevil + randint(25) + k);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case ART_INGWE:
			{
				msg_print("The amulet floods the area with goodness...");
				dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}
                        case ART_FLAR:
			{
                                if (dungeon_flags1 & LF1_NO_TELEPORT)
                                {
                                        msg_print("No teleport on special levels...");
                                        break;
                                }

                                if (dungeon_flags1 & LF1_NO_TELEPORT)
                                {
                                        msg_print("Not on special levels!");
                                        break;
                                }

                                msg_print("You open a between gate. Choose a destination.");
                                if (!tgt_pt(&ii,&ij)) return;
                                p_ptr->energy -= 60 - plev;
                                if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                                    (distance(ij,ii,py,px) > plev + 2) ||
                                    (!rand_int(plev * plev / 2)))
                                {
                                        msg_print("You fail to exit the between correctly!");
                                        p_ptr->energy -= 100;
                                        teleport_player(10);
                                }
                                else teleport_player_to(ij,ii);
                                o_ptr->timeout = 100;
                                break;
			}

			case ART_BARAHIR:
			{
                                msg_print("You exterminate small life.");
                                (void)dispel_monsters(4);
                                o_ptr->timeout = rand_int(55) + 55;
                                break;
			}

			case ART_TULKAS:
			{
				msg_print("The ring glows brightly...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(75) + 75);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = rand_int(150) + 150;
				break;
			}

			case ART_NARYA:
			{
				msg_print("The ring glows deep red...");
                                hp_player(500);
                                set_blind(0);
                                set_confused(0);
                                set_poisoned(0);
                                set_stun(0);
                                set_cut(0);
                                o_ptr->timeout = rand_int(100) + 200;
				break;
			}

			case ART_NENYA:
			{
				msg_print("The ring glows bright white...");
				if (!get_aim_dir(&dir)) return;
                                hp_player(800);
                                set_blind(0);
                                set_confused(0);
                                set_poisoned(0);
                                set_stun(0);
                                set_cut(0);
                                o_ptr->timeout = rand_int(200) + 100;
				break;
			}

			case ART_VILYA:
			{
				msg_print("The ring glows deep blue...");
				if (!get_aim_dir(&dir)) return;
                                hp_player(900);
                                set_blind(0);
                                set_confused(0);
                                set_poisoned(0);
                                set_stun(0);
                                set_cut(0);
                                if (p_ptr->black_breath)
                                {
                                        msg_print("The hold of the Black Breath on you is broken!");
                                }
                                o_ptr->timeout = rand_int(200) + 200;
				break;
			}

			case ART_POWER:
			{
				msg_print("The ring glows intensely black...");
                                o_ptr->timeout = ring_of_power();
				break;
			}


                        /* The Stone of Lore is perilous, for the sake of game balance. */
                        case ART_STONE_LORE:
                        {
                                msg_print("The stone reveals hidden mysteries...");
                                if (!ident_spell()) return;

                                if (!p_ptr->realm1)
                                {
                                        /* Sufficient mana */
                                        if (20 <= p_ptr->csp)
                                        {
                                                /* Use some mana */
                                                p_ptr->csp -= 20;
                                        }

                                        /* Over-exert the player */
                                        else
                                        {
                                                int oops = 20 - p_ptr->csp;

                                                /* No mana left */
                                                p_ptr->csp = 0;
                                                p_ptr->csp_frac = 0;

                                                /* Message */
                                                msg_print("You are too weak to control the stone!");

                                                /* Hack -- Bypass free action */
                                                (void)set_paralyzed(p_ptr->paralyzed +
                                                        randint(5 * oops + 1));

                                                /* Confusing. */
                                                (void)set_confused(p_ptr->confused +
                                                        randint(5 * oops + 1));
                                        }

                                        /* Redraw mana */
                                        p_ptr->redraw |= (PR_MANA);
                                }

                                take_hit(damroll(1, 12), "perilous secrets");

                                /* Confusing. */
                                if (rand_int(5) == 0) (void)set_confused(p_ptr->confused +
                                        randint(10));

                                /* Exercise a little care... */
                                if (rand_int(20) == 0) take_hit(damroll(4, 10), "perilous secrets");
                                o_ptr->timeout = 1;
                                break;
                        }

			case ART_RAZORBACK:
			{
				msg_print("Your armor is surrounded by lightning...");
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_BLADETURNER:
			{
                                set_invuln(p_ptr->invuln + randint(8) + 4);
                                o_ptr->timeout = 800;
				break;
			}

                        case ART_MEDIATOR:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 4);
				msg_print("Your armor glows many colours...");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + randint(50) + 50);
				(void)hp_player(30);
				(void)set_blessed(p_ptr->blessed + randint(50) + 50);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(50) + 50);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(50) + 50);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(50) + 50);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(50) + 50);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(50) + 50);
				o_ptr->timeout = 400;
				break;
			}

			case ART_SOULKEEPER:
			{
				msg_print("Your armor glows a bright white...");
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				o_ptr->timeout = 888;
				break;
			}

			case ART_BELEGENNON:
			{
				msg_print("A heavenly choir sings...");
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}

			case ART_CELEBORN:
			{
				msg_print("Your armor glows deep blue...");
				(void)genocide(TRUE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_CASPANION:
			{
				msg_print("Your armor glows bright red...");
				destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}

			case ART_DOR:
			case ART_GORLIM:
			{
				turn_monsters(40 + p_ptr->lev);
				o_ptr->timeout = 3 * (p_ptr->lev + 10);
				break;
			}

			case ART_HOLHENNETH:
			{
				msg_print("Your helm glows bright white...");
				msg_print("An image forms in your mind...");
				detect_all();
				o_ptr->timeout = rand_int(55) + 55;
				break;
			}

			case ART_GONDOR:
			{
				msg_print("Your crown glows deep blue...");
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)set_cut(0);
				o_ptr->timeout = 250;
				break;
			}

			case ART_COLLUIN:
			{
				msg_print("Your cloak glows many colours...");
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				o_ptr->timeout = 111;
				break;
			}

			case ART_HOLCOLLETH:
			{
				msg_print("Your cloak glows deep blue...");
				sleep_monsters_touch();
				o_ptr->timeout = 55;
				break;
			}

			case ART_THINGOL:
			{
				msg_print("Your cloak glows bright yellow...");
				recharge(60);
				o_ptr->timeout = 70;
				break;
			}

			case ART_COLANNON:
			{
                                if (dungeon_flags1 & LF1_NO_TELEPORT)
                                {
                                        msg_print("No teleport on special levels...");
                                        break;
                                }
				msg_print("Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_LUTHIEN:
			{
				msg_print("Your cloak glows a deep red...");
				restore_level();
				o_ptr->timeout = 450;
				break;
			}


			case ART_CAMMITHRIM:
			{
				msg_print("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				o_ptr->timeout = 2;
				break;
			}

			case ART_PAURHACH:
			{
				msg_print("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}

			case ART_PAURNIMMEN:
			{
				msg_print("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}

			case ART_PAURAEGEN:
			{
				msg_print("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}

			case ART_PAURNEN:
			{
				msg_print("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}

			case ART_FINGOLFIN:
			{
				msg_print("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ARROW, dir, 150);
				o_ptr->timeout = rand_int(90) + 90;
				break;
			}


			case ART_FEANOR:
			{
				msg_print("Your boots glow bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = 200;
				break;
			}

			case ART_DAL:
			{
				msg_print("Your boots glow deep blue...");
				(void)set_afraid(0);
				(void)set_poisoned(0);
				o_ptr->timeout = 5;
				break;
			}


			case ART_NARTHANC:
			{
				msg_print("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}

			case ART_NIMTHANC:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}

			case ART_DETHANC:
			{
				msg_print("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}

			case ART_RILIA:
			{
				msg_print("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 12, 3);
				o_ptr->timeout = rand_int(4) + 4;
				break;
			}

			case ART_BELANGIL:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 48, 2);
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}

			case ART_ANGUIREL:
			{
				switch(randint(13))
				{
				case 1: case 2: case 3: case 4: case 5:
					teleport_player(10);
					break;
				case 6: case 7: case 8: case 9: case 10:
					teleport_player(222);
					break;
				case 11: case 12:
					(void)stair_creation();
					break;
				default:
					if(get_check("Leave this level? "))
					{
						if (autosave_l)
						{
							is_autosave = TRUE;
							msg_print("Autosaving the game...");
							do_cmd_save_game();
							is_autosave = FALSE;
						}

						/* Leaving */
						p_ptr->leaving = TRUE;
					}
				}
				o_ptr->timeout = 35;
				break;
			}

			case ART_RINGIL:
			{
				msg_print("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = 300;
				break;
			}

                        case ART_ERU:
			{
                                msg_print("Your sword glows an intense white...");
                                hp_player(7000);
                                heal_insanity(50);
                                set_blind(0);
                                set_poisoned(0);
                                set_confused(0);
                                set_stun(0);
                                set_cut(0);
                                set_image(0);
                                o_ptr->timeout = 500;
				break;
			}

			case ART_DAWN:
			{
				msg_print("You summon the Legion of the Dawn.");
				(void)summon_specific_friendly(py, px, dun_level, SUMMON_DAWN, TRUE);
				o_ptr->timeout = 500 + randint(500);
				break;
			}

			case ART_ANDURIL:
			{
				msg_print("Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 2);
				o_ptr->timeout = 400;
				break;
			}


			case ART_THEODEN:
			{
				msg_print("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 120);
				o_ptr->timeout = 400;
				break;
			}

			case ART_AEGLOS:
			{
				msg_print("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 100, 3);
				o_ptr->timeout = 500;
				break;
			}

			case ART_OROME:
			{
				msg_print("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return;
				wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

			case ART_EONWE:
			{
				msg_print("Your axe lets out a long, shrill note...");
				(void)mass_genocide(TRUE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_LOTHARANG:
			{
				msg_print("Your battle axe radiates deep purple...");
				hp_player(damroll(4, 8));
				(void)set_cut((p_ptr->cut / 2) - 50);
				o_ptr->timeout = rand_int(3) + 3;
				break;
			}

			case ART_ULMO:
			{
				msg_print("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

			case ART_AVAVIR:
			{
                                if (dun_level && (max_dlv[dungeon_type] > dun_level))
				{
					if (get_check("Reset recall depth? "))
                                        max_dlv[dungeon_type] = dun_level;
				}

				msg_print("Your scythe glows soft white...");
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = randint(20) + 15;
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				o_ptr->timeout = 200;
				break;
			}


			case ART_TOTILA:
			{
				msg_print("Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return;
				confuse_monster(dir, 20);
				o_ptr->timeout = 15;
				break;
			}

			case ART_FIRESTAR:
			{
				msg_print("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 3);
				o_ptr->timeout = 100;
				break;
			}

			case ART_TARATOL:
			{
				msg_print("Your mace glows bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}

			case ART_ERIRIL:
			{
				msg_print("Your quarterstaff glows yellow...");
				if (!ident_spell()) return;
				o_ptr->timeout = 10;
				break;
			}

			case ART_OLORIN:
			{
				msg_print("Your quarterstaff glows brightly...");
				detect_all();
				probing();
				identify_fully();
				o_ptr->timeout = 1000;
				break;
			}

			case ART_TURMIL:
			{
				msg_print("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				o_ptr->timeout = 70;
				break;
			}

			case ART_CUBRAGOL:
			{
				msg_print("Your crossbow glows deep red...");
				(void)brand_bolts();
				o_ptr->timeout = 999;
				break;
			}

                        case ART_EVENSTAR:
			{
                                restore_level();
                                (void)do_res_stat(A_STR);
                                (void)do_res_stat(A_DEX);
                                (void)do_res_stat(A_CON);
                                (void)do_res_stat(A_INT);
                                (void)do_res_stat(A_WIS);
                                (void)do_res_stat(A_CHR);
                                o_ptr->timeout = 150;
				break;
			}

                        case ART_ELESSAR:
			{
                                if (p_ptr->black_breath)
                                {
                                        msg_print("The hold of the Black Breath on you is broken!");
                                }
                                p_ptr->black_breath = FALSE;
                                hp_player(100);
                                o_ptr->timeout = 200;
				break;
			}

                        case ART_GANDALF:
                        {
                                msg_print("Your mage staff glows deep blue...");
                                if (p_ptr->csp < p_ptr->msp)
                                {
                                        p_ptr->csp = p_ptr->msp;
                                        p_ptr->csp_frac = 0;
                                        msg_print("Your feel your head clear.");
                                        p_ptr->redraw |= (PR_MANA);
                                        p_ptr->window |= (PW_PLAYER);
                                        p_ptr->window |= (PW_SPELL);
                                }
                                o_ptr->timeout = 666;
                                break;
                        }

                        case ART_MARDA:
                        {
                                if (randint(3) == 1)
                                {
                                        if (summon_specific(py, px, ((plev * 3) / 2), SUMMON_DRACONIAN))
                                        {
                                                msg_print("A Draconian flies in from the Air !");
                                                msg_print("'I will burn you!'");
                                        }
                                }
                                else
                                {
                                        if (summon_specific_friendly(py, px, ((plev * 3) / 2),
                                            SUMMON_DRACONIAN, (bool)(plev == 50 ? TRUE : FALSE)))
                                        {
                                                msg_print("A Draconian flies in from the Air !");
                                                msg_print("'I will help you in your difficult task.'");
                                        }
                                }
                                o_ptr->timeout = 1000;
                                break;
                        }

                        case ART_PALANTIR_ITHIL:
                        case ART_PALANTIR:
                        {
				msg_print("The stone glows a deep green...");
                                wiz_lite_extra();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				o_ptr->timeout = rand_int(100) + 100;
                                break;
                        }
                        case ART_ROBINTON:
                        {
                                msg_format("Your instrument starts %s",music_info[3].desc);
                                p_ptr->music = 3; /* Full ID */
                                o_ptr->timeout = music_info[p_ptr->music].init_recharge;
                                break;
                        }
                        case ART_PIEMUR:
                        {
                                msg_format("Your instrument starts %s",music_info[9].desc);
                                p_ptr->music = 9;
                                o_ptr->timeout = music_info[p_ptr->music].init_recharge;
                                break;
                        }
                        case ART_MENOLLY:
                        {
                                msg_format("Your instrument starts %s",music_info[10].desc);
                                p_ptr->music = 10;
                                o_ptr->timeout = music_info[p_ptr->music].init_recharge;
                                break;
                        }
                        case ART_EREBOR:
                        {
                                msg_print("Your pick twists in your hands.");
                                if (!get_aim_dir(&dir))
                                        return;
                                if (passwall(dir, TRUE))
                                        msg_print("A passage opens, and you step through.");
                                else
                                        msg_print("There is no wall there!");
                                o_ptr->timeout = 75;
                                break;
                        }
                        case ART_DRUEDAIN:
                        {
                                msg_print("Your drum shows you the world.");
                                detect_all();
                                o_ptr->timeout = 99;
                                break;
                        }
                        case ART_ROHAN:
                        {
                                msg_print("Your horn glows deep red.");
                                set_afraid(0);
                                set_shero(p_ptr->shero + damroll(5,10) + 30);
                                set_afraid(0);
                                set_hero(p_ptr->hero + damroll(5,10) + 30);
                                set_fast(p_ptr->fast + damroll(5,10) + 30);
                                hp_player(30);
                                o_ptr->timeout = 250;
                                break;
                        }
                        case ART_HELM:
                        {
                                msg_print("Your horn emits a loud sound.");
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_SOUND, dir, 300, 6);
                                o_ptr->timeout = 300;
                                break;
                        }
                        case ART_BOROMIR:
                        {
                                msg_print("Your horn calls for help.");
                                for(i = 0; i < 15; i++)
                                        summon_specific_friendly(py, px, ((plev * 3) / 2),SUMMON_HUMAN, TRUE);
                                o_ptr->timeout = 1000;
                                break;
                        }
                        case ART_HURIN:
                        {
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
                                o_ptr->timeout = rand_int(200) + 100;
                                break;
                        }
                        case ART_AXE_GOTHMOG:
                        {
                                msg_print("Your lochaber axe erupts in fire...");
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_FIRE, dir, 300, 4);
                                o_ptr->timeout = 200+rand_int(200);
                                break;
                        }
                        case ART_MELKOR:
                        {
                                msg_print("Your spear is covered of darkness...");
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, 150, 3);
                                o_ptr->timeout = 100;
                                break;
                        }
                        case ART_GROND:
                        {
                                msg_print("Your hammer hits the floor...");
                                alter_reality();
                                o_ptr->timeout = 100;
                                break;
                        }
			case ART_NATUREBANE:
			{
				msg_print("Your axe glows blood red...");
                                dispel_monsters(300);
				o_ptr->timeout = 200 + randint(200);
				break;
			}
			case ART_NIGHT:
			{
				msg_print("Your axe emits a black aura...");
				if (!get_aim_dir(&dir)) return;
                                for (i = 0; i < 3; i++)
				{
					if (drain_life(dir, 100))
						hp_player(100);
				}
				o_ptr->timeout = 250;
				break;
			}
                        case ART_ORCHAST:
			{
				msg_print("Your weapon glows brightly...");
				(void)detect_monsters_xxx(RF3_ORC);
				o_ptr->timeout = 10;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

                if (o_ptr->timeout) return;
	}
        else if (is_ego_p(o_ptr, EGO_INST_DRAGONKIND))
	{
                fire_ball(o_ptr->pval2, 5, 300, 7);

                o_ptr->timeout = 100;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (o_ptr->tval == TV_INSTRUMENT)
        {
                if(o_ptr->sval != SV_HORN)
                {
                        msg_format("Your instrument starts %s",music_info[o_ptr->pval2].desc);
                        p_ptr->music = o_ptr->pval2;
                        o_ptr->timeout = music_info[p_ptr->music].init_recharge;
                }
                else
                {
                        msg_format("Your instrument emits a loud sound...");
                        aggravate_monsters(1);
                        o_ptr->timeout = 100;
                }

		/* Success */
		return;
	}
        else if (is_ego_p(o_ptr, EGO_DRAGON))
	{
		teleport_player(100);
		o_ptr->timeout = 50 + randint(50);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (is_ego_p(o_ptr, EGO_JUMP))
	{
                teleport_player(10);
                o_ptr->timeout = 10 + randint(10);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (is_ego_p(o_ptr, EGO_SPINING))
	{
                do_spin();
                o_ptr->timeout = 50 + randint(25);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (is_ego_p(o_ptr, EGO_NOLDOR))
	{
                detect_treasure();
                o_ptr->timeout = 10 + randint(20);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (is_ego_p(o_ptr, EGO_SPECTRAL))
        {
                if (!p_ptr->wraith_form)
                        set_shadow(20 + randint(20));
                else
                        set_shadow(p_ptr->tim_wraith + randint(20));
                o_ptr->timeout = 50 + randint(50);

		/* Window stuff */
                p_ptr->window |= PW_INVEN | PW_EQUIP;

		/* Done */
                return;
        }
        /* Hack -- Amulet of the Serpents can be activated as well */
        if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_SERPENT))
        {
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

                msg_print("You breathe venom...");
                fire_ball(GF_POIS, dir, 100, 2);
                o_ptr->timeout = rand_int(60) + 40;

		/* Window stuff */
                p_ptr->window |= PW_INVEN | PW_EQUIP;

		/* Done */
                return;
        }

	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_DRAGON_BLUE:
			{
				msg_print("You breathe lightning.");
				fire_ball(GF_ELEC, dir, 100, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_WHITE:
			{
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, 110, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_BLACK:
			{
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, 130, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_GREEN:
			{
				msg_print("You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_RED:
			{
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_MULTIHUED:
			{
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
                                o_ptr->timeout = rand_int(60) + 60;
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_GOLD:
			{
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, 2);
                                o_ptr->timeout = rand_int(90) + 90;
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, 220, 2);
                                o_ptr->timeout = rand_int(90) + 60;
				break;
			}

			case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
				          dir, 230, 2);
                                o_ptr->timeout = rand_int(90) + 60;
				break;
			}

			case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				msg_format("You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARDS))),
				          dir, 250, 2);
                                o_ptr->timeout = rand_int(90) + 60;
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, 2);
                                o_ptr->timeout = rand_int(90) + 60;
				break;
			}

			case SV_DRAGON_POWER:
			{
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 3);
                                o_ptr->timeout = rand_int(90) + 60;
				break;
			}
		}



		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

        if (o_ptr->tval == TV_RING)
	{
		switch (o_ptr->sval)
		{
                        case SV_RING_ELEC:
			{
                                /* Get a direction for breathing (or abort) */
                                if (!get_aim_dir(&dir)) return;

                                fire_ball(GF_ELEC, dir, 50, 2);
                                (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_ACID:
			{
                                /* Get a direction for breathing (or abort) */
                                if (!get_aim_dir(&dir)) return;

				fire_ball(GF_ACID, dir, 50, 2);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_ICE:
			{
                                /* Get a direction for breathing (or abort) */
                                if (!get_aim_dir(&dir)) return;

				fire_ball(GF_COLD, dir, 50, 2);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_FLAMES:
			{
                                /* Get a direction for breathing (or abort) */
                                if (!get_aim_dir(&dir)) return;

				fire_ball(GF_FIRE, dir, 50, 2);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

                        /* Yes, this can be activated but at the cost of it's destruction */
                        case SV_RING_TELEPORTATION:
			{
                                if(get_check("This will destroy the ring, do you want to continue ?"))
                                {
                                        msg_print("The ring explode into a space distorsion.");
                                        teleport_player(200);

                                        /* It explodes, doesnt it ? */
                                        take_hit(damroll(2, 10), "an exploding ring");

                                        inven_item_increase(item, -255);
                                        inven_item_optimize(item);
                                }
				break;
			}
		}


		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;

        }
        if ((o_ptr->art_name) || (o_ptr->tval == TV_RANDART))
	{
		(void) activate_random_artifact(o_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

        if (!process_hooks(HOOK_ACTIVATE, "(d)", item))
        {
                /* Mistake */
                msg_print("Oops.  That object cannot be activated.");
        }
}



static bool activate_random_artifact(object_type * o_ptr)
{
	int plev = p_ptr->lev;
	int ii = 0, ij = 0, k, dir, dummy = 0;
        int spell = o_ptr->xtra2;

        if ((!(o_ptr->art_name)) && (o_ptr->tval != TV_RANDART)) return FALSE; /* oops? */

        if(o_ptr->tval == TV_RANDART) spell = activation_info[o_ptr->pval2].spell;

	/* Activate for attack */
        switch (spell)
	{
		case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
			msg_print("It glows extremely brightly...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_MISSILE, dir, damroll(2, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
			msg_print("It throbs deep green...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, 12, 3);
			o_ptr->timeout = rand_int(4) + 4;
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msg_print("It is covered in sparks...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ELEC, dir, damroll(4, 8));
			o_ptr->timeout = rand_int(6) + 6;
			break;
		}

		case ACT_BO_ACID_1:
		{
			msg_print("It is covered in acid...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ACID, dir, damroll(5, 8));
			o_ptr->timeout = rand_int(5) + 5;
			break;
		}

		case ACT_BO_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_COLD, dir, damroll(6, 8));
			o_ptr->timeout = rand_int(7) + 7;
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msg_print("It is covered in fire...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(9, 8));
			o_ptr->timeout = rand_int(8) + 8;
			break;
		}

		case ACT_BA_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 48, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
			msg_print("It glows an intense red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 72, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 100))
			o_ptr->timeout = rand_int(100) + 100;
			break;
		}

		case ACT_BA_COLD_2:
		{
			msg_print("It glows an intense blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
		msg_print("It crackles with electricity...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 100, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			drain_life(dir, 120);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 50))
				hp_player(50);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
			msg_print("It grows magical spikes...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ARROW, dir, 150);
			o_ptr->timeout = rand_int(90) + 90;
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msg_print("It glows deep red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 120, 3);
			o_ptr->timeout = rand_int(225) + 225;
			break;
		}

		case ACT_BA_COLD_3:
		{
			msg_print("It glows bright white...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 200, 3);
			o_ptr->timeout = rand_int(325) + 325;
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msg_print("It glows deep blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 250, 3);
			o_ptr->timeout = rand_int(425) + 425;
			break;
		}

		case ACT_WHIRLWIND:
		{
			{
				int y = 0, x = 0;
				cave_type       *c_ptr;
				monster_type    *m_ptr;

				for (dir = 0; dir <= 9; dir++)
				{
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					/* Get the monster */
					m_ptr = &m_list[c_ptr->m_idx];

					/* Hack -- attack monsters */
					if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
                                                py_attack(y, x, -1);
				}
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_VAMPIRE_2:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 100))
				hp_player(100);
			}

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
			msg_print("It glows in scintillating colours...");
			call_chaos();
			o_ptr->timeout = 350;
			break;
		}

		case ACT_ROCKET:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir, 120 + (plev), 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
			msg_print("It floods the area with goodness...");
			dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_DISP_GOOD:
		{
			msg_print("It floods the area with evil...");
			dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You breathe the elements.");
			fire_ball(GF_MISSILE, dir, 300, 4);
			o_ptr->timeout = 500;
			break;
		}

		/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
			msg_print("It glows in scintillating colours...");
			if (!get_aim_dir(&dir)) return FALSE;
			confuse_monster(dir, 20);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
			msg_print("It glows deep blue...");
			sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
			{
				earthquake(py, px, 10);
				o_ptr->timeout = 50;
			}
			break;
		}

		case ACT_TERROR:
		{
#if 0
			for (i = 0; i < 8; i++) fear_monster(ddd[i], (p_ptr->lev)+10);
#else
			turn_monsters(40 + p_ptr->lev);
#endif
			o_ptr->timeout = 3 * (p_ptr->lev + 10);

			break;
		}

		case ACT_TELE_AWAY:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BANISH_EVIL:
		{
			if (banish_evil(100))
			{
				msg_print("The power of the artifact banishes evil!");
			}
			o_ptr->timeout = 250 + randint(250);
			break;
		}

		case ACT_GENOCIDE:
		{
			msg_print("It glows deep blue...");
			(void)genocide(TRUE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_MASS_GENO:
		{
			msg_print("It lets out a long, shrill note...");
			(void)mass_genocide(TRUE);
			o_ptr->timeout = 1000;
			break;
		}

		/* Activate for summoning / charming */

		case ACT_CHARM_ANIMAL:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void) charm_animal(dir, plev);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_CHARM_UNDEAD:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)control_one_undead(dir, plev);
			o_ptr->timeout = 333;
			break;
		}

		case ACT_CHARM_OTHER:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void) charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_CHARM_ANIMALS:
		{
			(void) charm_animals(plev * 2);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_CHARM_OTHERS:
		{
			charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_ANIMAL:
		{
			(void)summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE);
			o_ptr->timeout = 200 + randint(300);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msg_print("You summon a phantasmal servant.");
			(void)summon_specific_friendly(py, px, dun_level, SUMMON_PHANTOM, TRUE);
			o_ptr->timeout = 200 + randint(200);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			if (randint(3) == 1)
			{
				if (summon_specific(py, px, ((plev * 3) / 2), SUMMON_ELEMENTAL))
				{
					msg_print("An elemental materializes...");
					msg_print("You fail to control it!");
				}
			}
			else
			{
				if (summon_specific_friendly(py, px, ((plev * 3) / 2),
				    SUMMON_ELEMENTAL, (bool)(plev == 50 ? TRUE : FALSE)))
				{
					msg_print("An elemental materializes...");
					msg_print("It seems obedient to you.");
				}
			}
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			if (randint(3) == 1)
			{
				if (summon_specific(py, px, ((plev * 3) / 2), SUMMON_DEMON))
				{
					msg_print("The area fills with a stench of sulphur and brimstone.");
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
				}
			}
			else
			{
				if (summon_specific_friendly(py, px, ((plev * 3) / 2),
				    SUMMON_DEMON, (bool)(plev == 50 ? TRUE : FALSE)))
				{
					msg_print("The area fills with a stench of sulphur and brimstone.");
					msg_print("'What is thy bidding... Master?'");
				}
			}
			o_ptr->timeout = 666 + randint(333);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			if (randint(3) == 1)
			{
				if (summon_specific(py, px, ((plev * 3) / 2),
				    (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD)))
				{
					msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
					msg_print("'The dead arise... to punish you for disturbing them!'");
				}
			}
			else
			{
				if (summon_specific_friendly(py, px, ((plev * 3) / 2),
				    (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD),
				    (bool)(((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE)))
				{
					msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
					msg_print("Ancient, long-dead forms arise from the ground to serve you!");
				}
			}
			o_ptr->timeout = 666 + randint(333);
			break;
		}

		/* Activate for healing */

		case ACT_CURE_LW:
		{
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msg_print("It radiates deep purple...");
			hp_player(damroll(4, 8));
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = rand_int(3) + 3;
			break;
		}

		case ACT_CURE_POISON:
		{
			msg_print("It glows deep blue...");
			(void)set_afraid(0);
			(void)set_poisoned(0);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_REST_LIFE:
		{
			msg_print("It glows a deep red...");
			restore_level();
			o_ptr->timeout = 450;
			break;
		}

		case ACT_REST_ALL:
		{
			msg_print("It glows a deep green...");
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_CURE_700:
		{
			msg_print("It glows deep blue...");
			msg_print("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_cut(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msg_print("It glows a bright white...");
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}

		/* Activate for timed effect */

		case ACT_ESP:
		{
			(void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			(void)set_shero(p_ptr->shero + randint(50) + 50);
			(void)set_blessed(p_ptr->blessed + randint(50) + 50);
			o_ptr->timeout = 100 + randint(100);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msg_print("It lets out a shrill wail...");
			k = 3 * p_ptr->lev;
			(void)set_protevil(p_ptr->protevil + randint(25) + k);
			o_ptr->timeout = rand_int(225) + 225;
			break;
		}

		case ACT_RESIST_ALL:
		{
			msg_print("It glows many colours...");
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(40) + 40);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(40) + 40);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(40) + 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(40) + 40);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(40) + 40);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
			msg_print("It glows bright green...");
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20) + 20);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_XTRA_SPEED:
		{
			msg_print("It glows brightly...");
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(75) + 75);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = rand_int(200) + 200;
			break;
		}

		case ACT_WRAITH:
		{
                        set_shadow(p_ptr->tim_wraith + randint(plev/2) + (plev/2));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
			o_ptr->timeout = 1000;
			break;
		}

		/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
			msg_print("It wells with clear light...");
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_int(10) + 10;
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msg_print("It shines brightly...");
			map_area();
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_int(50) + 50;
			break;
		}

		case ACT_DETECT_ALL:
		{
			msg_print("It glows bright white...");
			msg_print("An image forms in your mind...");
			detect_all();
			o_ptr->timeout = rand_int(55) + 55;
			break;
		}

		case ACT_DETECT_XTRA:
		{
			msg_print("It glows brightly...");
			detect_all();
			probing();
			identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
			msg_print("It glows yellow...");
			identify_fully();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_ID_PLAIN:
		{
			if (!ident_spell()) return FALSE;
			o_ptr->timeout = 10;
			break;
		}

		case ACT_RUNE_EXPLO:
		{
			msg_print("It glows bright red...");
			explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
			msg_print("It glows light blue...");
			warding_glyph();
			o_ptr->timeout = 400;
			break;
		}

		case ACT_SATIATE:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_DEST_DOOR:
		{
			msg_print("It glows bright red...");
			destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
			msg_print("It pulsates...");
			if (!get_aim_dir(&dir)) return FALSE;
			wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
			recharge(60);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
			msg_print("It glows bright yellow...");
			(void) alchemy();
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DIM_DOOR:
		{
             if(dungeon_flags1 & LF1_NO_TELEPORT){msg_print("Not on special levels!");break;}

             msg_print("You open a between gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return FALSE;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the between correctly!");
                 p_ptr->energy -= 100;
                 get_pos_player(10,&ij,&ii);
             }
             cave_set_feat(py,px,FEAT_BETWEEN);
             cave_set_feat(ij,ii,FEAT_BETWEEN);
             cave[py][px].special = ii + (ij << 8);
             cave[ij][ii].special = px + (py << 8);

			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT:
		{
			msg_print("It twists space around you...");
			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}

		case ACT_RECALL:
		{
                        if (dun_level && (max_dlv[dungeon_type] > dun_level))
			{
				if (get_check("Reset recall depth? "))
                                max_dlv[dungeon_type] = dun_level;
			}

			msg_print("It glows soft white...");
			if (!p_ptr->word_recall)
			{
				p_ptr->word_recall = randint(20) + 15;
				msg_print("The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
			o_ptr->timeout = 200;
			break;
		}
                case ACT_DEATH:
                        take_hit(5000,"activating a death spell");
                        break;
                case ACT_RUINATION:
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination");
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
                        break;
                case ACT_DESTRUC:
                        earthquake(py,px,12);
                        break;
                case ACT_UNINT:
                        (void)dec_stat(A_INT, 25, FALSE);
                        break;
                case ACT_UNSTR:
                        (void)dec_stat(A_STR, 25, FALSE);
                        break;
                case ACT_UNCON:
                        (void)dec_stat(A_CON, 25, FALSE);
                        break;
                case ACT_UNCHR:
                        (void)dec_stat(A_CHR, 25, FALSE);
                        break;
                case ACT_UNDEX:
                        (void)dec_stat(A_DEX, 25, FALSE);
                        break;
                case ACT_UNWIS:
                        (void)dec_stat(A_WIS, 25, FALSE);
                        break;
                case ACT_STATLOSS:
                        (void)dec_stat(A_STR, 15, FALSE);
                        (void)dec_stat(A_INT, 15, FALSE);
                        (void)dec_stat(A_WIS, 15, FALSE);
                        (void)dec_stat(A_DEX, 15, FALSE);
                        (void)dec_stat(A_CON, 15, FALSE);
                        (void)dec_stat(A_CHR, 15, FALSE);
                        break;
                case ACT_HISTATLOSS:
                        (void)dec_stat(A_STR, 25, FALSE);
                        (void)dec_stat(A_INT, 25, FALSE);
                        (void)dec_stat(A_WIS, 25, FALSE);
                        (void)dec_stat(A_DEX, 25, FALSE);
                        (void)dec_stat(A_CON, 25, FALSE);
                        (void)dec_stat(A_CHR, 25, FALSE);
                        break;
                case ACT_EXPLOSS:
                        lose_exp(p_ptr->exp/20);
                        break;
                case ACT_HIEXPLOSS:
                        lose_exp(p_ptr->exp/10);
                        break;
                case ACT_SUMMON_MONST:
                        summon_specific(py,px,max_dlv[dungeon_type],0);
                        break;
                case ACT_PARALYZE:
                        set_paralyzed(p_ptr->paralyzed + 20 + randint(10));
                        break;
                case ACT_HALLU:
                        set_image(p_ptr->image + 20 + randint(10));
                        break;
                case ACT_POISON:
                        set_poisoned(p_ptr->poisoned + 20 + randint(10));
                        break;
                case ACT_HUNGER:
                        (void)set_food(PY_FOOD_WEAK);
                        break;
                case ACT_STUN:
                        set_stun(p_ptr->stun + 20 + randint(10));
                        break;
                case ACT_CUTS:
                        set_cut(p_ptr->cut + 20 + randint(10));
                        break;
                case ACT_PARANO:
                        set_confused(p_ptr->confused + 30 + randint(10));
                        break;
                case ACT_CONFUSION:
                        set_confused(p_ptr->confused + 20 + randint(10));
                        break;
                case ACT_BLIND:
                        set_blind(p_ptr->blind + 20 + randint(10));
                        break;
                case ACT_PET_SUMMON:
                        summon_specific_friendly(py,px,max_dlv[dungeon_type],0,FALSE);
                        break;
                case ACT_CURE_PARA:
                        set_confused(0);
                        break;
                case ACT_CURE_HALLU:
                        set_image(0);
                        break;
                case ACT_CURE_POIS:
                        set_poisoned(0);
                        break;
                case ACT_CURE_HUNGER:
                        (void)set_food(PY_FOOD_MAX - 1);
                        break;
                case ACT_CURE_STUN:
                        set_stun(0);
                        break;
                case ACT_CURE_CUTS:
                        set_cut(0);
                        break;
                case ACT_CURE_FEAR:
                        set_afraid(0);
                        break;
                case ACT_CURE_CONF:
                        set_confused(0);
                        break;
                case ACT_CURE_BLIND:
                        set_blind(0);
                        break;
                case ACT_CURING:
                        set_blind(0);
                        set_poisoned(0);
                        set_confused(0);
                        set_stun(0);
                        set_cut(0);
                        set_image(0);
                        break;
                case ACT_DARKNESS:
                        unlite_area(damroll(2,10),10);
                        break;
                case ACT_LEV_TELE:
                        teleport_player_level();
                        break;
                case ACT_ACQUIREMENT:
                        acquirement(py,px,1,FALSE,FALSE);
                        break;
                case ACT_WEIRD:
                        break;
                case ACT_AGGRAVATE:
			aggravate_monsters(1);
                        break;
                case ACT_MUT:
                        gain_random_corruption(0);
                        break;
                case ACT_CURE_INSANITY:
                        heal_insanity(damroll(10,10));
                        break;
                case ACT_CURE_MUT:
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
                                msg_print("You are cured of all corruptions.");
				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
                        break;

                case ACT_LIGHT_ABSORBTION:
                {
                        int y, x, light = 0, dir;
                        cave_type *c_ptr;

                        for(y = py - 6; y <= py + 6; y++)
                        {
                                for(x = px - 6; x <= px + 6; x++)
                                {
                                        if(!in_bounds(y, x)) continue;

                                        c_ptr = &cave[y][x];

                                        if (distance(y, x, py, px) > 6) continue;

                                        if (c_ptr->info & CAVE_GLOW)
                                        {
                                                light++;

                                                /* No longer in the array */
                                                c_ptr->info &= ~(CAVE_TEMP);

                                                /* Darken the grid */
                                                c_ptr->info &= ~(CAVE_GLOW);

                                                /* Hack -- Forget "boring" grids */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
                                                {
                                                        /* Forget the grid */
                                                        c_ptr->info &= ~(CAVE_MARK);

                                                        /* Notice */
                                                        note_spot(y, x);
                                                }

                                                /* Process affected monsters */
                                                if (c_ptr->m_idx)
                                                {
                                                        /* Update the monster */
                                                        update_mon(c_ptr->m_idx, FALSE);
                                                }

                                                /* Redraw */
                                                lite_spot(y, x);
                                        }
                                }
                        }
			if (!get_aim_dir(&dir)) return FALSE;
                        msg_print("The light around you is absorbed... and released in a powerful bolt!");
                        fire_bolt(GF_LITE, dir, damroll(light, p_ptr->lev));
                        break;
                }

		default:
		{
			msg_format("Unknown activation effect: %d.", o_ptr->xtra2);
			return FALSE;
		}
	}

        if(o_ptr->tval == TV_RANDART)
        {
                o_ptr->timeout = activation_info[o_ptr->pval2].cost / 10;
        }

	return TRUE;
}

static bool activate_spell(object_type * o_ptr, byte choice)
{
        int mana = 0, gf = 0, mod = 0;
        rune_spell s_ptr;

        if (choice == 1)
        {
                gf = o_ptr->pval & 0xFFFF;
                mod = o_ptr->pval3 & 0xFFFF;
                mana = o_ptr->pval2 & 0xFF;
        }
        else if (choice == 2)
        {
                gf = o_ptr->pval >> 16;
                mod = o_ptr->pval3 >> 16;
                mana = o_ptr->pval2 >> 8;
        }

        s_ptr.type = gf;
        s_ptr.rune2 = 1 << mod;
        s_ptr.mana = mana;

        /* Execute */
        rune_exec(&s_ptr, 0);

        if (choice == 1) o_ptr->timeout = mana * 10;
        if (choice == 2) o_ptr->xtra2 = mana * 10;

	return TRUE;
}
