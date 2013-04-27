/* File: melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "learn.h"
#include "melee.h"
#include "tvalsval.h"
#include "raceflag.h"

/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 *
 * This must agree with know_damage()/monster1.c .
 */
static int monster_critical(dice_sides d, int dam)
{
	int max = 0;
	int total = d.maxroll();

	/* Must do at least 95% of perfect */
	if (20*dam < 19*total) return (0);

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
 */
static bool check_hit(int power, int level)
{
	int chance;

	/* Calculate the "attack quality" */
	chance = (power + (level * 3));

	/* Check if the player was hit */
	return test_hit(chance, p_ptr->total_ac(), TRUE);
}


/*
 * Hack -- possible "insult" messages
 */
static const char* const desc_insult[] =
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
static const char* const desc_moan[] =
{
	"wants his mushrooms back.", 
	"tells you to get off his land.", 
	"looks for his dogs. ", 
	"says 'Did you kill my Fang?' ", 
	"asks 'Do you want to buy any mushrooms?' ", 
	"seems sad about something.", 
	"asks if you have seen his dogs.", 
	"tells you to get off his land.", 
	"mumbles something about mushrooms." 
};

static int attack_power(byte effect)
{
	/* Extract the attack "power" */
	switch (effect)
	{
		case RBE_HURT:      return 60;
		case RBE_POISON:    return  5;
		case RBE_UN_BONUS:  return 20;
		case RBE_UN_POWER:  return 15;
		case RBE_EAT_GOLD:  return  5;
		case RBE_EAT_ITEM:  return  5;
		case RBE_EAT_FOOD:  return  5;
		case RBE_EAT_LITE:  return  5;
		case RBE_ACID:      return  0;
		case RBE_ELEC:      return 10;
		case RBE_FIRE:      return 10;
		case RBE_COLD:      return 10;
		case RBE_BLIND:     return  2;
		case RBE_CONFUSE:   return 10;
		case RBE_TERRIFY:   return 10;
		case RBE_PARALYZE:  return  2;
		case RBE_LOSE_STR:  return  0;
		case RBE_LOSE_DEX:  return  0;
		case RBE_LOSE_CON:  return  0;
		case RBE_LOSE_INT:  return  0;
		case RBE_LOSE_WIS:  return  0;
		case RBE_LOSE_CHR:  return  0;
		case RBE_LOSE_ALL:  return  2;
		case RBE_SHATTER:   return 60;
		case RBE_EXP_10:    return  5;
		case RBE_EXP_20:    return  5;
		case RBE_EXP_40:    return  5;
		case RBE_EXP_80:    return  5;
		case RBE_HALLU:     return 10;
		default: 			return  0;
	}
}

static void describe_attack_method(byte method,const char*& act,int& sound_msg,int& do_cut, int& do_stun)
{
	/* Describe the attack method */
	switch (method)
	{
		case RBM_HIT:
		{
			act = "hits you.";
			do_cut = do_stun = 1;
			sound_msg = MSG_MON_HIT;
			break;
		}

		case RBM_TOUCH:
		{
			act = "touches you.";
			sound_msg = MSG_MON_TOUCH;
			break;
		}

		case RBM_PUNCH:
		{
			act = "punches you.";
			do_stun = 1;
			sound_msg = MSG_MON_PUNCH;
			break;
		}

		case RBM_KICK:
		{
			act = "kicks you.";
			do_stun = 1;
			sound_msg = MSG_MON_KICK;
			break;
		}

		case RBM_CLAW:
		{
			act = "claws you.";
			do_cut = 1;
			sound_msg = MSG_MON_CLAW;
			break;
		}

		case RBM_BITE:
		{
			act = "bites you.";
			do_cut = 1;
			sound_msg = MSG_MON_BITE;
			break;
		}

		case RBM_STING:
		{
			act = "stings you.";
			sound_msg = MSG_MON_STING;
			break;
		}

		case RBM_BUTT:
		{
			act = "butts you.";
			do_stun = 1;
			sound_msg = MSG_MON_BUTT;
			break;
		}

		case RBM_CRUSH:
		{
			act = "crushes you.";
			do_stun = 1;
			sound_msg = MSG_MON_CRUSH;
			break;
		}

		case RBM_ENGULF:
		{
			act = "engulfs you.";
			sound_msg = MSG_MON_ENGULF;
			break;
		}

		case RBM_CRAWL:
		{
			act = "crawls on you.";
			sound_msg = MSG_MON_CRAWL;
			break;
		}

		case RBM_DROOL:
		{
			act = "drools on you.";
			sound_msg = MSG_MON_DROOL;
			break;
		}

		case RBM_SPIT:
		{
			act = "spits on you.";
			sound_msg = MSG_MON_SPIT; 
			break;
		}

		case RBM_GAZE:
		/* ZaiBand gazes */
		case RBM_PRESBYOPIC_GAZE:
		case RBM_HYPEROPIC_GAZE:
		case RBM_RANGED_GAZE:
		case RBM_CLAIRVOYANT_GAZE:
		{
			act = "gazes at you.";
			sound_msg = MSG_MON_GAZE; 
			break;
		}

		case RBM_WAIL:
		{
			act = "wails at you.";
			sound_msg = MSG_MON_WAIL; 
			break;
		}

		case RBM_SPORE:
		{
			act = "releases spores at you.";
			sound_msg = MSG_MON_SPORE; 
			break;
		}

		case RBM_BEG:
		{
			act = "begs you for money.";
			sound_msg = MSG_MON_BEG;
			break;
		}

		case RBM_INSULT:
		{
			act = desc_insult[rand_int(N_ELEMENTS(desc_insult))];
			sound_msg = MSG_MON_INSULT; 
			break;
		}

		case RBM_MOAN:
		{
			act = desc_moan[rand_int(N_ELEMENTS(desc_moan))];
			sound_msg = MSG_MON_MOAN; 
			break;
		}

		default:
		{
			act = "indescribably attacks you.";
			break;
		}
	}
}

/** 
 * this is for RBE_HURT
 * player armor reduces total damage
 */
int armor_damage_reduction(int damage,int ac)
{
	return damage-(damage * MIN(ac,150) / 250);
}

static void apply_exact_damage(byte effect,int damage,byte rlev,const m_idx_type m_idx,const char* ddesc,bool& obvious,bool& blinked,bool& do_break)
{
	char o_name[80];
	monster_type* const m_ptr = &mon_list[m_idx];
	int k;

	/* Apply appropriate damage */
	switch (effect)
	{
		case RBE_HURT:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage, using armor damage reduction */
			take_hit(armor_damage_reduction(damage,p_ptr->total_ac()), ddesc);

			break;
		}

		case RBE_POISON:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Take "poison" effect */
			if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			{
				if (p_ptr->inc_timed<TMD_POISONED>(randint(rlev) + 5))
				{
					obvious = TRUE;
				}
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_POIS);

			break;
		}

		case RBE_UN_BONUS:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Allow complete resist */
			if (!p_ptr->resist_disen)
			{
				/* Apply disenchantment */
				if (apply_disenchant(0)) obvious = TRUE;
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_DISEN);

			break;
		}

		case RBE_UN_POWER:
		{
			int drained = 0;
			int k;

			assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
			assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

			/* Take damage */
			take_hit(damage, ddesc);

			/* need items in inventory to continue */
			if (0 == p_ptr->inven_cnt) break;

			/* Find an item */
			for (k = 0; k < 10; ++k)
			{
				/* Pick and obtain the item */
				object_type* const o_ptr = &p_ptr->inventory[rand_int(p_ptr->inven_cnt)];

				/* Drain charged wands/staves */
				if ((o_ptr->obj_id.tval == TV_STAFF) ||
				    (o_ptr->obj_id.tval == TV_WAND))
				{
					/* Charged? */
					if (o_ptr->pval)
					{
						drained = o_ptr->pval;

						/* Uncharge */
						o_ptr->pval = 0;
					}
				}

				if (drained)
				{
					int heal = rlev * drained;

					msg_print("Energy drains from your pack!");

					obvious = TRUE;

					/* Don't heal more than max hp */
					heal = MIN(heal, m_ptr->mhp - m_ptr->chp);

					/* Heal */
					m_ptr->chp += heal;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx)
						p_ptr->redraw |= (PR_HEALTH);

					/* Combine / Reorder the pack */
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);

					/* Window stuff */
					p_ptr->redraw |= (PR_INVEN);

					/* Affect only a single inventory slot */
					break;
				}
			}

			break;
		}

		case RBE_EAT_GOLD:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Obvious */
			obvious = TRUE;

			/* Saving throw (unless paralyzed) based on dex and level */
			if (!p_ptr->timed[TMD_PARALYZED] &&
			    (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
			                      p_ptr->lev)))
			{
				/* Saving throw message */
				msg_print("You quickly protect your money pouch!");

				/* Occasional blink anyway */
				if (!one_in_(3)) blinked = TRUE;
			}

			/* Eat gold */
			else
			{
				s32b gold = (p_ptr->au / 10) + randint(25);
				if (gold < 2) gold = 2;
				if (gold > 5000) gold = (p_ptr->au / 20) + randint(3000);
				if (gold > p_ptr->au) gold = p_ptr->au;
				p_ptr->au -= gold;
				if (gold <= 0)
				{
					msg_print("Nothing was stolen.");
				}
				else if (p_ptr->au)
				{
					msg_print("Your purse feels lighter.");
					msg_format("%ld coins were stolen!", (long)gold);
				}
				else
				{
					msg_print("Your purse feels lighter.");
					msg_print("All of your coins were stolen!");
				}

				/* Redraw gold */
				p_ptr->redraw |= (PR_GOLD);

				/* Blink away */
				blinked = TRUE;
			}

			break;
		}

		case RBE_EAT_ITEM:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Saving throw (unless paralyzed) based on dex and level */
			if (!p_ptr->timed[TMD_PARALYZED] &&
			    (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
			                      p_ptr->lev)))
			{
				/* Saving throw message */
				msg_print("You grab hold of your backpack!");

				/* Occasional "blink" anyway */
				blinked = TRUE;

				/* Obvious */
				obvious = TRUE;

				/* Done */
				break;
			}

			assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
			assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

			/* need items in inventory to continue */
			if (0 == p_ptr->inven_cnt) break;

			/* Find an item */
			for (k = 0; k < 10; k++)
			{
				object_type object_type_body;
				object_type *i_ptr = &object_type_body;	/* Get local object */

				/* Pick the item */
				int i = rand_int(p_ptr->inven_cnt);

				/* Obtain the item */
				object_type* const o_ptr = &p_ptr->inventory[i];

				/* Skip artifacts */
				if (o_ptr->is_artifact()) continue;

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_FULL);

				/* Message */
				msg_format("%sour %s (%c) was stolen!",
				           ((o_ptr->number > 1) ? "One of y" : "Y"),
				           o_name, index_to_label(i));

				/* Obtain local object */
				*i_ptr = *o_ptr;

				/* Modify number */
				i_ptr->number = 1;

				/* Hack -- If a rod, staff, or wand, allocate total
				 * maximum timeouts or charges between those
				 * stolen and those missed. -LM-
				 */
				distribute_charges(o_ptr, i_ptr, 1);

				/* Carry the object */
				monster_carry(m_idx, i_ptr);

				/* Steal the items */
				inven_item_increase(i, -1);
				inven_item_optimize(i);

				/* Obvious */
				obvious = TRUE;

				/* Blink away */
				blinked = TRUE;

				/* Done */
				break;
			}

			break;
		}

		case RBE_EAT_FOOD:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
			assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

			/* need items in inventory to continue */
			if (0 == p_ptr->inven_cnt) break;

			/* Steal some food */
			for (k = 0; k < 10; k++)
			{
				/* Pick the item */
				int i = rand_int(p_ptr->inven_cnt);

				/* Obtain the item */
				object_type* const o_ptr = &p_ptr->inventory[i];

				/* Skip non-food objects */
				if (o_ptr->obj_id.tval != TV_FOOD) continue;

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_BASE);

				/* Message */
				msg_format("%sour %s (%c) was eaten!",
				           ((o_ptr->number > 1) ? "One of y" : "Y"),
				           o_name, index_to_label(i));

				/* Steal the items */
				inven_item_increase(i, -1);
				inven_item_optimize(i);

				/* Obvious */
				obvious = TRUE;

				/* Done */
				break;
			}

			break;
		}

		case RBE_EAT_LITE:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Get the lite */
			object_type* const o_ptr = &p_ptr->inventory[INVEN_LITE];

			/* Drain fuel */
			if ((o_ptr->pval > 0) && (!o_ptr->is_artifact()))
			{
				/* Reduce fuel */
				o_ptr->pval -= (250 + randint(250));
				if (o_ptr->pval < 1) o_ptr->pval = 1;

				/* Notice */
				if (!p_ptr->timed[TMD_BLIND] )
				{
					msg_print("Your light dims.");
					obvious = TRUE;
				}

				/* Window stuff */
				p_ptr->redraw |= (PR_EQUIP);
			}

			break;
		}

		case RBE_ACID:
		{
			/* Obvious */
			obvious = TRUE;

			/* Message */
			msg_print("You are covered in acid!");

			/* Special damage */
			acid_dam(damage, ddesc);

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_ACID);

			break;
		}

		case RBE_ELEC:
		{
			/* Obvious */
			obvious = TRUE;

			/* Message */
			msg_print("You are struck by electricity!");

			/* Take damage (special) */
			elec_dam(damage, ddesc);

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_ELEC);

			break;
		}

		case RBE_FIRE:
		{
			/* Obvious */
			obvious = TRUE;

			/* Message */
			msg_print("You are enveloped in flames!");

			/* Take damage (special) */
			fire_dam(damage, ddesc);

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_FIRE);

			break;
		}

		case RBE_COLD:
		{
			/* Obvious */
			obvious = TRUE;

			/* Message */
			msg_print("You are covered with frost!");

			/* Take damage (special) */
			cold_dam(damage, ddesc);

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_COLD);

			break;
		}

		case RBE_BLIND:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Increase "blind" */
			if (!p_ptr->resist_blind)
			{
				if (p_ptr->inc_timed<TMD_BLIND>(10 + randint(rlev)))
				{
					obvious = TRUE;
				}
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_BLIND);

			break;
		}

		case RBE_CONFUSE:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Increase "confused" */
			if (!p_ptr->resist_confu)
			{
				if (p_ptr->inc_timed<TMD_CONFUSED>(3 + randint(rlev)))
				{
					obvious = TRUE;
				}
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_CONFU);

			break;
		}

		case RBE_TERRIFY:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Increase "afraid" */
			if (p_ptr->resist_fear)
			{
				msg_print("You stand your ground!");
				obvious = TRUE;
			}
			else if (p_ptr->std_save())
			{
				msg_print("You stand your ground!");
				obvious = TRUE;
			}
			else
			{
				if (p_ptr->inc_timed<TMD_AFRAID>(3 + randint(rlev)))
				{
					obvious = TRUE;
				}
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_FEAR);

			break;
		}

		case RBE_PARALYZE:
		{
			/* Hack -- Prevent perma-paralysis via damage */
			if (p_ptr->timed[TMD_PARALYZED] && (damage < 1)) damage = 1;

			/* Take damage */
			take_hit(damage, ddesc);

			/* Increase "paralyzed" */
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
				obvious = TRUE;
			}
			else if (p_ptr->std_save())
			{
				msg_print("You resist the effects!");
				obvious = TRUE;
			}
			else
			{
				if (p_ptr->inc_timed<TMD_PARALYZED>(3 + randint(rlev)))
				{
					obvious = TRUE;
				}
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_FREE);

			break;
		}

		case RBE_LOSE_STR:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stat) */
			if (do_dec_stat(A_STR)) obvious = TRUE;

			break;
		}

		case RBE_LOSE_INT:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stat) */
			if (do_dec_stat(A_INT)) obvious = TRUE;

			break;
		}

		case RBE_LOSE_WIS:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stat) */
			if (do_dec_stat(A_WIS)) obvious = TRUE;

			break;
		}

		case RBE_LOSE_DEX:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stat) */
			if (do_dec_stat(A_DEX)) obvious = TRUE;

			break;
		}

		case RBE_LOSE_CON:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stat) */
			if (do_dec_stat(A_CON)) obvious = TRUE;

			break;
		}

		case RBE_LOSE_CHR:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stat) */
			if (do_dec_stat(A_CHR)) obvious = TRUE;

			break;
		}

		case RBE_LOSE_ALL:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Damage (stats) */
			if (do_dec_stat(A_STR)) obvious = TRUE;
			if (do_dec_stat(A_DEX)) obvious = TRUE;
			if (do_dec_stat(A_CON)) obvious = TRUE;
			if (do_dec_stat(A_INT)) obvious = TRUE;
			if (do_dec_stat(A_WIS)) obvious = TRUE;
			if (do_dec_stat(A_CHR)) obvious = TRUE;

			break;
		}

		case RBE_SHATTER:
		{
			/* Obvious */
			obvious = TRUE;

			/* reduce damage by armor */
			damage = armor_damage_reduction(damage,p_ptr->total_ac());

			/* Take damage */
			take_hit(damage, ddesc);

			/* Radius 8 earthquake centered at the monster */
			if (damage > 23)
			{
				const coord p_old_loc(p_ptr->loc);
				earthquake(m_ptr->loc, 8);

				/* Stop the blows if the player is pushed away */
				do_break = (p_old_loc!=p_ptr->loc);
			}
			break;
		}

		case RBE_EXP_10:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(damage, ddesc);

			if (p_ptr->hold_life && (rand_int(100) < 95))
			{
				msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = NdS(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
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

		case RBE_EXP_20:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(damage, ddesc);

			if (p_ptr->hold_life && (rand_int(100) < 90))
			{
				msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = NdS(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

				if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(d / 10);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case RBE_EXP_40:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(damage, ddesc);

			if (p_ptr->hold_life && (rand_int(100) < 75))
			{
				msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = NdS(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

				if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(d / 10);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case RBE_EXP_80:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(damage, ddesc);

			if (p_ptr->hold_life && (rand_int(100) < 50))
			{
				msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = NdS(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

				if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(d / 10);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case RBE_HALLU:
		{
			/* Take damage */
			take_hit(damage, ddesc);

			/* Increase "image" */
			if (!p_ptr->resist_chaos)
			{
				if (p_ptr->inc_timed<TMD_IMAGE>(3 + randint(rlev / 2)))
				{
					obvious = TRUE;
				}
			}

			/* Learn about the player */
			update_smart_learn(m_idx, DRS_RES_CHAOS);

			break;
		}

		default:
		{
			/* Hack -- Assume obvious */
			obvious = TRUE;

			/* Hack -- No damage */
			damage = 0;

			break;
		}
	}
}

/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(const m_idx_type m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	monster_lore *l_ptr = m_ptr->lore();

	/* Not allowed to attack */
	if (r_ptr->flags[0] & RF0_NEVER_BLOW) return (FALSE);

	{	/* blocking brace for C-ish code */
	int ap_cnt;

	int rlev = (r_ptr->level >= 1) ? r_ptr->level : 1;	/* Extract the effective monster level */

	int tmp;
	char m_name[80];
	char ddesc[80];
	bool blinked = FALSE;


	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, MDESC_SHOW | MDESC_IND2);

	/* Scan through all blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		bool visible = m_ptr->ml;	/* Extract visibility (before blink) */
		bool obvious = FALSE;
		bool do_break = FALSE;

		int damage = 0;
		const char* act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		dice_sides d = r_ptr->blow[ap_cnt].d;

		int power = attack_power(effect);
		
		if (!method) break;			/* Hack -- no more attacks */
		if (p_ptr->leaving) break;	/* Handle "leaving" */

		/* ZaiBand: unfortunately, presbyopic and hyperopic gazes cannot focus at range 1 */
		if (   RBM_PRESBYOPIC_GAZE == method
			|| RBM_HYPEROPIC_GAZE  == method)
			continue;

		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			int sound_msg = MSG_GENERIC;	/* Assume no sound */
			int do_cut = 0;					/* Assume no cut or stun */
			int do_stun = 0;

			/* Always disturbing */
			disturb(1, 0);


			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->timed[TMD_PROTEVIL] > 0) &&
			    (r_ptr->flags[2] & RF2_EVIL) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags[2] |= RF2_EVIL;
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Describe the attack method */
			describe_attack_method(method,act,sound_msg,do_cut,do_stun);

			/* Message */
			if (act) message_format(sound_msg, 0, "%^s %s", m_name, act);

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply appropriate damage */
			damage = d.damroll();
			apply_exact_damage(effect,damage,rlev,m_idx,ddesc,obvious,blinked,do_break);

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (one_in_(2))
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
			if (do_cut)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d, damage);

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
				if (k) (void)p_ptr->inc_timed<TMD_CUT>(k);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d, damage);

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
				if (k) (void)p_ptr->inc_timed<TMD_STUN>(k);
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
				case RBM_BUTT:
				case RBM_CRUSH:
				case RBM_ENGULF:

				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1, 0);

					/* Message */
					msg_format("%^s misses you.", m_name);
				}

				break;
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (l_ptr->blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[ap_cnt] < UCHAR_MAX)
				{
					l_ptr->blows[ap_cnt]++;
				}
			}
		}

		/* Skip the other blows if necessary */
		if (do_break) break;
	}


	/* Blink away */
	if (blinked)
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}
	}	/* end blocking brace */


	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_S16B))
	{
		l_ptr->deaths++;
	}


	/* Assume we attacked */
	return (TRUE);
}

void
monster_type::melee_analyze(int& min_dam, int& median_dam, int& max_dam,coord g)
{
	monster_race *r_ptr = race();
	int dis = distance(loc.y,loc.x,g.y,g.x);
	min_dam = median_dam = max_dam = 0;
	
	/* Not allowed to attack */
	if (r_ptr->flags[0] & RF0_NEVER_BLOW) return;

	/* too far away for even a clairvoyant gaze */
	if (MAX_RANGE<dis) return;

	/* don't attack self */
	if (0 == dis) return;

	{	/* blocking brace for C-ish code */
	int ap_cnt;
	int threat_moves;
	int my_moves;
	move_ratio(threat_moves,my_moves,*p_ptr,0,0);

	/* player moves first */
	if (ticks_to_move(1,0) > p_ptr->ticks_to_move(1,0)) return;

	/* Scan through all blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		/* Extract the attack infomation */
		int method = r_ptr->blow[ap_cnt].method;
		dice_sides d = r_ptr->blow[ap_cnt].d;

		if (!method) break;			/* Hack -- no more attacks */

		if (1 == dis)
		{	/* ZaiBand: unfortunately, presbyopic and hyperopic gazes cannot focus at range 1 */
			if (   RBM_PRESBYOPIC_GAZE == method
				|| RBM_HYPEROPIC_GAZE  == method)
				continue;
		}
		else
		{	/* only ranged gazes, for now */
			if 		(RBM_PRESBYOPIC_GAZE == method)
			{
				if (2 > dis || 4 < dis) continue;
				if (!los(loc,g)) continue;
			}
			else if (RBM_HYPEROPIC_GAZE == method)
			{
				if (4 > dis) continue;
				if (!los(loc,g)) continue;
			}
			else if (RBM_RANGED_GAZE == method)
			{
				if (!los(loc,g)) continue;
			}
			else if (RBM_CLAIRVOYANT_GAZE != method) continue;
		}

		switch(method)
		{
		case RBE_HURT:
		case RBE_SHATTER:
			{
			int ac = p_ptr->total_ac();

			min_dam += armor_damage_reduction(d.minroll(),ac);
			median_dam += armor_damage_reduction(d.medianroll(),ac);
			max_dam += armor_damage_reduction(d.maxroll(),ac);
			break;
			}
		default:
			{
			min_dam += d.minroll();
			median_dam += d.medianroll();
			max_dam += d.maxroll();
			}
		}
	}

	/* handle multiple-move on our part */
	min_dam *= my_moves;
	median_dam *= my_moves;
	max_dam *= my_moves;
	}	/* end blocking brace */
}

/*
 * Attack the player via ranged physical attacks (currently, gazes)
 */
bool make_attack_ranged_physical(const m_idx_type m_idx)
{
	monster_type* const m_ptr = &mon_list[m_idx];
	monster_race* const r_ptr = m_ptr->race();
	monster_lore* const l_ptr = m_ptr->lore();
	int attack_count = 0;

	/* Not allowed to attack */
	if (r_ptr->flags[0] & RF0_NEVER_BLOW) return (FALSE);

	{	/* blocking brace for C-ish code */
	int dis = distance(m_ptr->loc.y,m_ptr->loc.x,p_ptr->loc.y,p_ptr->loc.x);
	if (MAX_RANGE<dis) return (FALSE);	/* Too far away to gaze */
	if (1>=dis) return (FALSE);			/* should have handled this as melee */

	{	/* blocking brace for C-ish code */	
	int ap_cnt;

	int rlev = (r_ptr->level >= 1) ? r_ptr->level : 1;	/* Extract the effective monster level */
	/* gazes do not cut or stun */

	char m_name[80];
	char ddesc[80];
	bool blinked = FALSE;

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, MDESC_SHOW | MDESC_IND2);


	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		bool visible = m_ptr->ml;	/* Extract visibility (before blink) */
		bool obvious = FALSE;
		bool do_break = FALSE;

		int damage = 0;
		const char* act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		dice_sides d = r_ptr->blow[ap_cnt].d;

		int power = attack_power(effect);

		if (!method) break;				/* Hack -- no more attacks */
		if (p_ptr->leaving) break;		/* Handle "leaving" */

		/* allow the extended gazes */
		if 		(RBM_CLAIRVOYANT_GAZE==method)
			{	/* OK; clairvoyant gaze ignores LOS */
			}
		else if (los(m_ptr->loc,p_ptr->loc))
			{	/* has LOS (stronger than projectable) */
			if 		(RBM_RANGED_GAZE==method)
				{	/* OK: ranged gaze has arbitrary range */
				}
			else if (RBM_HYPEROPIC_GAZE==method)
				{	/* Hyperopic gaze is worthless at range < 4 */
				if (4>dis) continue;
				}
			else if (RBM_PRESBYOPIC_GAZE==method)
				{	/* Presbyopic gaze only works between ranges 2, 4 inclusive */
				if (2>dis || 4<dis) continue;
				}
			else{
				continue;	/* Not an extended gaze */
				}
			}
		else{
			continue; /* No clairvoyance, no LOS, no gaze */
			}

		attack_count++;

		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			int sound_msg = MSG_GENERIC;	/* Assume no sound */
			int do_cut = 0;					/* Assume no cut or stun */
			int do_stun = 0;

			/* Always disturbing */
			disturb(1, 0);


			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->timed[TMD_PROTEVIL] > 0) &&
			    (r_ptr->flags[2] & RF2_EVIL) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags[2] |= RF2_EVIL;
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}


			/* Describe the attack method */
			describe_attack_method(method,act,sound_msg,do_cut,do_stun);

			/* Message */
			if (act) message_format(sound_msg, 0, "%^s %s", m_name, act);

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply appropriate damage */
			damage = d.damroll();
			apply_exact_damage(effect,damage,rlev,m_idx,ddesc,obvious,blinked,do_break);

			/* gazes should not cut or stun */
		}

		/* Monster missed player */
		else
		{
			/* missed gazes should not be described */
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (l_ptr->blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[ap_cnt] < UCHAR_MAX)
				{
					l_ptr->blows[ap_cnt]++;
				}
			}
		}

		/* Skip the other blows if necessary */
		if (do_break) break;
	}


	/* Blink away */
	if (blinked)
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}
	}	/* end blocking brace */
	}	/* end blocking brace */

	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_S16B))
	{
		l_ptr->deaths++;
	}


	/* Assume we attacked */
	return (attack_count) ? TRUE : FALSE;
}

