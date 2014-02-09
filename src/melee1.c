/* File: melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Leon Marrick, Bahman Rabbi, Diego Gonzalez, Jeff Greene
 *
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


/*
 * Critical blows by monsters can inflict cuts and stuns.
 */
static int monster_critical(int dice, int sides, int dam, int effect)
{
	int max = 0;
	int bonus;
	int total = dice * sides;

	/* Special case -- wounding/battering attack */
	if ((effect == RBE_WOUND) || (effect == RBE_BATTER))
	{
		/* Must do at least 70% of perfect */
		if (dam < total * 7 / 10) return (0);
		max = 1;
	}

	/* Standard attack */
	else
 	{
		/* Weak blows rarely work */
		if ((rand_int(20) >= dam) || (!one_in_(3))) return (0);

		/* Must do at least 90% of perfect */
		if (dam < total * 9 / 10) return (0);
	}

	/* Perfect damage */
	if (dam == total) max++;

	/* Get bonus to critical damage (never greater than 6) */
	bonus = MIN(6, div_round(dam, 8));

	/* Critical damage  (never greater than 6 + max) */
	return (randint(bonus) + max);
}



/*
 * Determine if a monster attack against the player succeeds.
 */
static bool check_hit_player(int power, int level, int m_idx)
{
	int ac, chance;

	monster_type *m_ptr = &mon_list[m_idx];

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	chance = (power + ((m_ptr->m_timed[MON_TMD_STUN]) ? level * 2 : level * 3));

	/*Adjust for player terrain*/
	chance = feat_adjust_combat_for_player(chance, TRUE);

	/*Adjust for monster terrain*/
	chance = feat_adjust_combat_for_monster(m_ptr, chance, FALSE);

	if (m_ptr->mflag & (MFLAG_DESPERATE)) chance += 15;

	/* Total armor */
	ac = p_ptr->state.ac + p_ptr->state.to_a;

	/* Check if the player was hit */
	return test_hit(chance, ac, TRUE);
}


/*
 * Adjust damage for character armour.
 *
 * Use proper rounding.  -LM-
 */
static void ac_dam(int *dam, int ac)
{
	/* The effect of a point of damage decreases as the total rises */
	int d = 120 + ABS(ac);

	/* Adjust damage (whole) */
	*dam -= ((*dam) * ac) / d;

	/* Adjust damage (fractional) */
	if ((((*dam) * ABS(ac)) % d) > rand_int(d))
	{
		*dam -= ((ac > 0) ? 1 : -1);
	}
}

#define MAX_DESC_INSULT   8

/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[MAX_DESC_INSULT] =
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
 * Drain rods wands and staves charges.
 * Return amount to heal monster if applicable.
 */
int drain_charges(object_type *o_ptr, u32b heal)
{

	int counter, old_charges;

	/*get the number of rods/wands/staffs to be drained*/
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		counter = o_ptr->pval;

		/*get the number of charges to be drained*/
		while ((counter > 1) && (!one_in_(counter)))
		{
			/*reduce by one*/
			counter --;
		}

		/*drain the wands/staffs*/
		o_ptr->pval -= counter;

		/*factor healing times the difference*/
		heal*= (counter);
	}

	/*rods*/
	else
	{

		/* Determine how much if left to drain. */
		old_charges = (o_ptr->pval - o_ptr->timeout);

		/*prepare to get the number of charges to drain*/
		counter = old_charges;

		/*get the number of wands/staffs to be drained*/
		while ((counter > 1) && (!one_in_(counter)))
		{
			/*reduce by one*/
			counter --;

		}

		/*see how much timeout to add*/
		old_charges -= counter;

		/*drain the rod(s)*/
		o_ptr->timeout += old_charges;

		/*factor healing times the drained amount*/
		heal *= MAX((old_charges / 30), 1);
	}

	return (heal);

}

/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(monster_type *m_ptr)
{
	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int i, k, tmp, ac, rlev;
	s16b heal;
	int do_cut, do_stun;
	int blows;

	bool alive = TRUE;


	s32b gold;

	object_type *o_ptr;

	char o_name[120];

	char m_name[80];

	char ddesc[80];

	bool blinked;

	int sound_msg;

	bool player_on_glyph = FALSE;

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Remember if the player is standing on a glyph of warding */
	if (cave_player_glyph_bold(p_ptr->py, p_ptr->px)) player_on_glyph = TRUE;

	/* Total armor */
	ac = p_ptr->state.ac + p_ptr->state.to_a;

	/* Extract the effective monster level */
	rlev = MAX(r_ptr->level, 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Get the "died from" information (i.e. "a white worm mass") */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, 0x88);

	/* Assume no blink */
	blinked = FALSE;

	/* Calculate the number of blows this monster gets */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		if (!r_ptr->blow[ap_cnt].method) break;
	}
	blows = ap_cnt;

	/* Scan through all the blows */
	for (ap_cnt = 0; ap_cnt < blows; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;
		bool no_effect = FALSE;
		bool do_break = FALSE;

		int power = 0;
		int dam = 0;

		cptr act = NULL;
		char msg[80];

		/* Extract the attack information */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;

		/* Hack -- no more attacks */
		if (!method) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Assume no cut, stun, or touch */
		do_cut = do_stun = 0;

		/* Assume no sound */
		sound_msg = MSG_GENERIC;

		/* Extract visibility from carrying lite */
		if (r_ptr->flags2 & RF2_HAS_LIGHT) visible = TRUE;

		/* Extract the attack "power".  Elemental attacks upgraded. */
		switch (effect)
		{
			case RBE_HURT:       power = 60;  break;
			case RBE_WOUND:      power = 60;  break;
			case RBE_BATTER:     power = 60;  break;
			case RBE_SHATTER:    power = 60;  break;
			case RBE_UN_BONUS:   power = 20;  break;
			case RBE_UN_POWER:   power = 15;  break;
			case RBE_LOSE_MANA:  power = 25;  break;
			case RBE_EAT_GOLD:   power =  5;  break;
			case RBE_EAT_ITEM:   power =  5;  break;
			case RBE_EAT_FOOD:   power =  5;  break;
			case RBE_EAT_LIGHT:   power =  5;  break;
			case RBE_HUNGER:     power = 15;  break;
			case RBE_POISON:     power = 25;  break;
			case RBE_ACID:       power = 10;  break;
			case RBE_ELEC:       power = 10;  break;
			case RBE_FIRE:       power = 10;  break;
			case RBE_COLD:       power = 10;  break;
			case RBE_BLIND:      power =  5;  break;
			case RBE_CONFUSE:    power = 10;  break;
			case RBE_TERRIFY:    power = 10;  break;
			case RBE_PARALYZE:   power =  5;  break;
			case RBE_HALLU:      power = 10;  break;
			case RBE_DISEASE:    power = 10;  break;
			case RBE_LOSE_STR:   power =  0;  break;
			case RBE_LOSE_DEX:   power =  0;  break;
			case RBE_LOSE_CON:   power =  0;  break;
			case RBE_LOSE_INT:   power =  0;  break;
			case RBE_LOSE_WIS:   power =  0;  break;
			case RBE_LOSE_CHR:   power =  0;  break;
			case RBE_LOSE_ALL:   power =  2;  break;
			case RBE_EXP_10:     power =  5;  break;
			case RBE_EXP_20:     power =  5;  break;
			case RBE_EXP_40:     power =  5;  break;
			case RBE_EXP_80:     power =  5;  break;
		}

		/* Roll out the damage */
		dam = damroll(d_dice, d_side);

		/* Glyphs of warding halve melee damage */
		if (player_on_glyph)
		{
			dam /= 2;

			/* No damage, ignore blow */
			if (!dam) continue;
		}

		/* Describe the attack method, apply special hit chance mods. */
		switch (method)
		{
			case RBM_HIT:
			{

				/* Handle special effect types */
				if (effect == RBE_WOUND)
				{
					if      (dam >= 30) act = "gouges you";
					else if (dam >= 20) act = "slashes you";
					else if (dam >= 5)  act = "cuts you";
					else                act = "scratches you";
				}
				else if (effect == RBE_BATTER)
				{
					if      (dam >= 30) act = "bludgeons you";
					else if (dam >= 20) act = "batters you";
					else if (dam >= 5)  act = "bashes you";
					else                act = "hits you";
				}
				else
				{
					act = "hits you";
				}
				do_cut = do_stun = 1;
				sound_msg = MSG_MON_HIT;
				break;
			}
			case RBM_TOUCH:
			{
				act = "touches you";
				sound_msg = MSG_MON_TOUCH;
				break;
			}

			case RBM_PUNCH:
			{
				act = "punches you";
				do_stun = 1;
				sound_msg = MSG_MON_PUNCH;
				break;
			}

			case RBM_KICK:
			{
				act = "kicks you";
				do_stun = 1;
				sound_msg = MSG_MON_KICK;
				break;
			}
			case RBM_CLAW:
			{
				if      (dam >= 25) act = "slashes you";
				else if (dam >=  5) act = "claws you";
				else                act = "scratches you";
				do_cut = 1;
				sound_msg = MSG_MON_CLAW;
				break;
			}
			case RBM_BITE:
			{
				if (dam >= 5) act = "bites you";
				else          act = "nips you";
				do_cut = 1;
				sound_msg = MSG_MON_BITE;
				break;
			}
			case RBM_PECK:
			{
				act = "pecks you";
				do_cut = 1;
				sound_msg = MSG_MON_BITE;
				break;
			}
			case RBM_BREATHE:
			{
				act = "breathes on you";
				do_cut = 1;
				sound_msg = MSG_BR_ACID;
				break;
			}
			case RBM_STING:
			{
				act = "stings you";
				sound_msg = MSG_MON_STING;
				break;
			}
			case RBM_BUTT:
			{
				if (dam >= rand_range(10, 20)) act = "tramples you";
				else                           act = "butts you";
				do_stun = 1;
				sound_msg = MSG_MON_BUTT;
				break;
			}
			case RBM_CRUSH:
			{
				if (dam >= 10) act = "crushes you";
				else           act = "squeezes you";
				do_stun = 1;
				sound_msg = MSG_MON_CRUSH;
				break;
			}
			case RBM_ENGULF:
			{
				if (dam >= randint(50)) act = "envelops you";
				else                    act = "engulfs you";
				sound_msg = MSG_MON_ENGULF;
				break;
			}
			case RBM_CRAWL:
			{
				act = "crawls on you";
				sound_msg = MSG_MON_CRAWL;
				break;
			}
			case RBM_DROOL:
			{
				act = "drools on you";
				sound_msg = MSG_MON_DROOL;
				break;
			}
			case RBM_SPIT:
			{
				act = "spits on you";
				sound_msg = MSG_MON_SPIT;
				break;
			}
			case RBM_SLIME:
			{
				act = "You've been slimed!";
				sound_msg = MSG_MON_SPIT;
				break;
			}
			case RBM_GAZE:
			{
				if      (dam >= rand_range(20, 30))
					act = "glares at you terribly";
				else if (dam >= rand_range(5, 30))
					act = "gazes upon you";
				else act = "gazes at you";
				sound_msg = MSG_MON_GAZE;
				break;
			}
			case RBM_WAIL:
			{
				act = "makes a horrible wail";
				sound_msg = MSG_MON_WAIL;
				break;
			}
			case RBM_SPORE:
			{
				act = "releases a cloud of spores";
				sound_msg = MSG_MON_SPORE;
				break;
			}
			case RBM_TRAMPLE:
			{
				act = "tramples all over you!";
				sound_msg = MSG_MON_CRUSH;
				break;
			}
			case RBM_XXX5:
			case RBM_XXX6:
			{
				act = "projects XXX's at you";
				break;
			}
			case RBM_BEG:
			{
				act = "begs you for money";
				sound_msg = MSG_MON_BEG;
				break;
			}
			case RBM_INSULT:
			{
				act = desc_insult[rand_int(MAX_DESC_INSULT)];
				sound_msg = MSG_MON_INSULT;
				break;
			}
		}

		/* No effect */
		if (no_effect) continue;

		/* Monster hits player */
		if (!effect || check_hit_player(power, rlev, m_idx))
		{
			/* Always disturbing */
			disturb(1, 0);

			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->timed[TMD_PROTEVIL] > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{

				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Message - special handling for sliming and insult attacks */
			if (act)
			{

				if (method == RBM_SLIME) my_strcpy(msg, format("%s", act), sizeof(msg));
				else if (method == RBM_INSULT)
				{
					my_strcpy(msg, format("%^s %s", m_name, act), sizeof(msg));
				}
				else
				{
					if (dam > p_ptr->chp / 3)
						my_strcpy(msg, format("%^s %s!", m_name, act), sizeof(msg));
					else
						my_strcpy(msg, format("%^s %s.", m_name, act), sizeof(msg));
				}

				/* Message -- sometimes incorrect  XXX XXX */
				if (act) message_format(sound_msg, 0, "%s", msg);
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply appropriate damage */
			switch (effect)
			{

				/* No effect */
				case 0:
				{
					/* Hack -- Assume obvious */
					obvious = TRUE;

					/* Hack -- No damage */
					dam = 0;

					break;
				}

				/* Ordinary hit */
				case RBE_HURT:
				{
					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					break;
				}

				/* Hit with increased chance to wound */
				case RBE_WOUND:
 				{
					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

 					/* Take damage */
					take_hit(dam, ddesc);

					/* Usually don't stun */
					if ((do_stun) && (!one_in_(5))) do_stun = FALSE;

					/* Always give a chance to inflict cuts */
					do_cut = TRUE;

					break;
				}

				/* Hit with increased chance to stun */
				case RBE_BATTER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Usually don't cut */
					if ((do_cut) && (!one_in_(5))) do_cut = FALSE;

					/* Always give a chance to inflict stuns */
					do_stun = TRUE;

					break;
				}

				/* Hit to cause earthquakes */
				case RBE_SHATTER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Radius 6 earthquake centered on the monster */
					if (dam > rand_int(60))
					{
						earthquake(m_ptr->fy, m_ptr->fx, 6, TRUE);

						/*check if the monster & player are still next to each other*/
						if (distance(p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx) > 1)
							do_break = TRUE;

					}

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

 					break;
 				}

				/* Hit to disenchant */
 				case RBE_UN_BONUS:
 				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Allow complete resist */
					if (!p_ptr->state.resist_disen)
					{
						/* Apply disenchantment */
						if (apply_disenchant(0)) obvious = TRUE;
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_DISEN);

					break;
				}

				/* Hit to reduce charges of magical items */
				case RBE_UN_POWER:
				{
					/* Take damage */
					take_hit(dam, ddesc);

					/* Find an item */
					for (k = 0; k < 20; k++)
					{

						/* Blindly hunt ten times for an item. */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* use "tmp" to decide if a item can
						 * be uncharged.  By default, assume it
						 * can't.
						 */
						tmp = 0;

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Drain charged wands/staffs */
						if (item_tester_hook_recharge(o_ptr))

						{
							/* case of charged wands/staffs. */
							if (((o_ptr->tval == TV_STAFF) ||
								(o_ptr->tval == TV_WAND)) &&
								(o_ptr->pval)) tmp = 1;

							/* case of (at least partially) charged rods. */
							else if ((o_ptr->tval == TV_ROD) &&
								(o_ptr->timeout < o_ptr->pval)) tmp = 1;

							if (tmp)
							{

								heal = drain_charges(o_ptr, rlev);

								/* Message */
								msg_print("Energy drains from your pack!");

								/* Obvious */
								obvious = TRUE;

								/* Message */
								if ((m_ptr->hp < m_ptr->maxhp) && (heal))
								{
									if (m_ptr->ml) msg_format("%^s looks healthier.",  m_name);
									else msg_format("%^s sounds healthier.", m_name);
								}

								/*heal is greater than monster wounds, restore mana too*/
								if (heal > (m_ptr->maxhp - m_ptr->hp))
								{

									/*leave some left over for mana*/
									heal -= (m_ptr->maxhp - m_ptr->hp);

									/*fully heal the monster*/
									m_ptr->hp = m_ptr->maxhp;

									/*mana is more powerful than HP*/
									heal /= 10;

									/* if heal was less than 10, make it 1*/
									if (heal < 1) heal = 1;

									/*give message if anything left over*/
									if (m_ptr->mana < r_ptr->mana)
									{
										if (m_ptr->ml) msg_format("%^s looks refreshed.", m_name);
										else msg_format("%^s sounds refreshed.", m_name);
									}

									/*add mana*/
									m_ptr->mana += heal;

									if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;
								}

								/* Simple Heal */
								else m_ptr->hp += heal;

								/* Redraw (later) if needed */
								if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar))
									p_ptr->redraw |= (PR_HEALTH|PR_MON_MANA);

								/* Combine / Reorder the pack */
								p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

								/* Redraw stuff */
								p_ptr->redraw |= (PR_INVEN);

								/* not more than one inventory
								 * slot effected. */
								break;
							}
						}
					}

					break;
				}

				/* Hit to reduce mana */
				case RBE_LOSE_MANA:
				{
					int drain;

					char msg_tmp[80];
					my_strcpy(msg_tmp, msg, sizeof(msg_tmp));

					/* Obvious */
					obvious = TRUE;

					/* Damage (mana) */
					if (p_ptr->csp)
					{
						/* Drain depends on maximum mana */
						drain = 2 + rand_int(p_ptr->msp / 10);

						/* Drain the mana */
						if (drain > p_ptr->csp)
						{
						p_ptr->csp = 0;
							p_ptr->csp_frac = 0;

							my_strcat(msg_tmp, "  Your mana is gone!", sizeof(msg_tmp));
						}
						else
						{
							p_ptr->csp -= drain;
							my_strcat(msg_tmp, "  Your mana drains away.", sizeof(msg_tmp));
						}

						/* Redraw mana */
						p_ptr->redraw |= (PR_MANA);

					}

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Damage (physical) */
					take_hit(dam, ddesc);


					/* Learn about the player */
					update_smart_learn(m_idx, LRN_MANA);
					break;
				}

				/* Hit to steal gold */

				case RBE_EAT_GOLD:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Confused monsters cannot steal successfully. */
					if (m_ptr->m_timed[MON_TMD_CONF]) break;

					/* Obvious */
					obvious = TRUE;

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->timed[TMD_PARALYZED] &&
					    (rand_int(100) < (adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] +
					                      (p_ptr->lev / 10))))
					{
						/* Saving throw message */
						msg_print("You quickly protect your money pouch!");

						/* Occasional blink anyway */
						if (one_in_(3)) blinked = TRUE;
					}

					/* Eat gold */
					else
					{

						/* The more gold you have, the smaller a fraction of it is stolen. */
						int div1 = 8 + div_round(p_ptr->au, 4000L);
						int div2 = div1 * 2;

						/* Steal some gold (at least two gold pieces) */
						gold = rand_range(p_ptr->au / div2, p_ptr->au / div1) +
						       rand_range(2, 25);

						/*sometimes a player doesn't even have two coins*/
						if (p_ptr->au < gold) gold = p_ptr->au;

						/* Reduce character gold */
						p_ptr->au -= gold;

						/* Messages */
						if (gold == 0)
						{
							msg_print("Nothing was stolen.");
						}
						else if (gold == 1)
						{
							msg_print("Your purse feels lighter.");
							msg_format("Your coin was stolen!");
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

						/* While we have gold, put it in objects */
						while (gold > 0)
						{
							int amt;

							/* Create a new temporary object */
							object_type o;
							object_wipe(&o);
							object_prep(&o, lookup_kind(TV_GOLD, SV_GOLD_GOLD));

							/* Amount of gold to put in this object */
							amt = gold > MAX_PVAL ? MAX_PVAL : gold;
							o.pval = amt;
							gold -= amt;

							/* Give the gold to the monster */
							monster_carry(m_idx, &o);
						}


						/* Redraw gold */
						p_ptr->redraw |= (PR_GOLD);

						/* Blink away */
						blinked = TRUE;
					}

					break;
				}

				/* Hit to steal objects from the pack */
				case RBE_EAT_ITEM:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->timed[TMD_PARALYZED] &&
					    (rand_int(100) < (adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] +
					                      (p_ptr->lev / 10))))
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

					/* Blindly scrabble in the backpack ten times */
					for (k = 0; k < 10; k++)
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip artifacts */
						if (artifact_p(o_ptr)) continue;

						/* Get a description */
						object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

						/* Message */
						msg_format("%sour %s (%c) was stolen!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           o_name, index_to_label(i));

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain local object */
						object_copy(i_ptr, o_ptr);

						/* One item is stolen at a time. */
						i_ptr->number = 1;

						/* Hack -- If a rod or wand, allocate total
						 * maximum timeouts or charges between those
						 * stolen and those missed. -LM-
						 */
						distribute_charges(o_ptr, i_ptr, 1);

						/* Carry the object */
						(void)monster_carry(m_idx, i_ptr);

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

				/* Hit to eat food */
				case RBE_EAT_FOOD:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Steal some food */
					for (k = 0; k < 6; k++)
					{
						/* Pick an item from the pack */
						i = rand_int(INVEN_PACK);

						/* Get the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip non-food objects */
						if (o_ptr->tval != TV_FOOD) continue;

						/* Get a description */
						object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

						if ((method != RBM_SLIME) ||
						    (o_ptr->sval < SV_FOOD_MIN_FOOD))
						{
							msg_format("%s %s (%c) was eaten!",
								   ((o_ptr->number > 1) ? "One of your" : "Your last"),
								   o_name, index_to_label(i));
						}

						/* Special message for Green Glutton Ghosts */
						else
						{
							msg_print("It got at your rations!");
						}

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

				/* Hit to reduce nutrition */
				case RBE_HUNGER:
				{
					int resist = 2;

					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* We're not dead yet */
					if (!p_ptr->is_dead)
					{
						/* Allow resistance */
						if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE]) resist++;
						if (p_ptr->state.slow_digest) resist += 2;

						/* Message -- only if appropriate */
						if ((resist > 2) &&
						    (p_ptr->food > PY_FOOD_ALERT))
						{
							msg_print("You resist the effects!");
						}
						else
						{
							msg_print("You feel hungry...");
						}

						/* Reduce food counter, but not too much. */
						set_food(p_ptr->food -
							MIN(500 + p_ptr->food / 5, p_ptr->food / resist));

					}

					break;
				}

				/* Hit to drain light */
				case RBE_EAT_LIGHT:
				{
					u32b f1, f2, f3, fn;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Get the light source */
					o_ptr = &inventory[INVEN_LIGHT];

					/* Get object flags */
					object_flags(o_ptr, &f1, &f2, &f3, &fn);

					/* Drain fuel */
					if ((o_ptr->timeout > 0) && (!artifact_p(o_ptr)))
					{
						/* Reduce fuel */
						o_ptr->timeout -= rand_range(250, 500);
						if (o_ptr->timeout < 1) o_ptr->timeout = 1;

						/* Notice */
						if (!p_ptr->timed[TMD_BLIND])
						{
							msg_print("Your light dims.");
							obvious = TRUE;
						}

						/* Redraw stuff */
						p_ptr->redraw |= (PR_EQUIP);
					}

					break;
				}


				/* Hit to poison */
				case RBE_POISON:
				{
						/* Take damage */
					take_hit(dam, ddesc);

					/* Take "poison" effect */
					if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
					{
						if (inc_timed(TMD_POISONED, randint(rlev) + 5, TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_POIS);

					break;

				}

				/* Hit to inflict acid damage */
				case RBE_ACID:
				{
					/* Some of attack is pure damage, and so
					 * resists should not be allowed to reduce
					 * damage as much as they do normally.
					 * Damage will be reduced later.
					 */
					int dam2 = dam;

					if (p_ptr->state.resist_acid) dam2 *= 2;
					if (p_ptr->timed[TMD_OPP_ACID]) dam2 *= 2;

					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are covered in acid!");

					/* Special damage */
					acid_dam(dam2, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_ACID);

					break;
				}

				/* Hit to electrocute */
				case RBE_ELEC:
				{
					/* Some of attack is pure damage, and so
					 * resists should not be allowed to reduce
					 * damage as much as they do normally.
					 * Damage will be reduced later.
					 */
					int dam2 = dam;

					/* Obvious */
					obvious = TRUE;

					if (p_ptr->state.resist_elec) dam2 *= 2;
					if (p_ptr->timed[TMD_OPP_ELEC]) dam2 *= 2;

					/* Message */
					msg_print("You are struck by electricity!");

					/* Take damage (special) */
					elec_dam(dam2, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_ELEC);

					break;
				}

				/* Hit to burn */
				case RBE_FIRE:
				{
					/* Some of attack is pure damage, and so
					 * resists should not be allowed to reduce
					 * damage as much as they do normally.
					 * Damage will be reduced later.
					 */
					int dam2 = dam;

					if (p_ptr->state.resist_fire) dam2 *= 2;
					if (p_ptr->timed[TMD_OPP_FIRE]) dam2 *= 2;

					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are enveloped in flames!");

					/* Take damage (special) */
					fire_dam(dam2, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FIRE);

					break;
				}

				case RBE_COLD:
				{
					/* Some of attack is pure damage, and so
					 * resists should not be allowed to reduce
					 * damage as much as they do normally.
					 * Damage will be reduced later.
					 */
					int dam2 = dam;

					if (p_ptr->state.resist_cold) dam2 *= 2;
					if (p_ptr->timed[TMD_OPP_COLD]) dam2 *= 2;

					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are covered with frost!");

					/* Take damage (special) */
					cold_dam(dam2, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_COLD);

					break;
				}

				case RBE_BLIND:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Increase blindness */
					if (!p_ptr->state.resist_blind)
					{
						if (inc_timed(TMD_BLIND, 10 + randint(rlev), TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_BLIND);

					break;
				}

				/* Hit to confuse */
				case RBE_CONFUSE:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Increase "confused" */
					if (allow_player_confusion())
					{
						if (inc_timed(TMD_CONFUSED, 3 + randint(rlev), TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_CONFU);

					break;
				}

				/* Hit to frighten */
				case RBE_TERRIFY:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Increase "afraid" */
					if (p_ptr->state.resist_fear)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_AFRAID, 3 + randint(rlev), TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FEAR_SAVE);

					break;
				}

				/* Hit to paralyze (never cumulative) */
				case RBE_PARALYZE:
				{
					/* Hack -- Prevent perma-paralysis via damage */
					if (p_ptr->timed[TMD_PARALYZED] && (dam < 1)) dam = 1;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Increase "paralyzed" */
					if (p_ptr->state.free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
					{
						msg_print("You resist the effects!");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_PARALYZED, 3 + randint(rlev), TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FREE_SAVE);

					break;
				}

				/* Hit to cause disease */
				case RBE_DISEASE:
 				{
					int do_disease = dam;

					/* Player armour reduces raw damage */
					ac_dam(&dam, ac);

					/* Take (adjusted) damage */
					take_hit(dam, ddesc);

					/* Inflict disease (unaffected by armour) */
					disease(&do_disease);

 					break;
 				}

				case RBE_LOSE_STR:
				case RBE_LOSE_INT:
				case RBE_LOSE_WIS:
				case RBE_LOSE_DEX:
				case RBE_LOSE_CON:
				case RBE_LOSE_CHR:
				case RBE_LOSE_ALL:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Reduce strength */
					if ((effect == RBE_LOSE_STR) || (effect == RBE_LOSE_ALL))
					{
						if (do_dec_stat(A_STR)) obvious = TRUE;
					}

					/* Reduce intelligence */
					if ((effect == RBE_LOSE_INT) || (effect == RBE_LOSE_ALL))
					{
						if (do_dec_stat(A_INT)) obvious = TRUE;
					}

					/* Reduce wisdom */
					if ((effect == RBE_LOSE_WIS) || (effect == RBE_LOSE_ALL))
					{
						if (do_dec_stat(A_WIS)) obvious = TRUE;
					}

					/* Reduce dexterity */
					if ((effect == RBE_LOSE_DEX) || (effect == RBE_LOSE_ALL))
					{
						if (do_dec_stat(A_DEX)) obvious = TRUE;
					}

					/* Reduce constitution */
					if ((effect == RBE_LOSE_CON) || (effect == RBE_LOSE_ALL))
 					{
						if (do_dec_stat(A_CON)) obvious = TRUE;
					}

					/* Reduce constitution */
					if ((effect == RBE_LOSE_CHR) || (effect == RBE_LOSE_ALL))
 					{
						if (do_dec_stat(A_CHR)) obvious = TRUE;
					}



					break;
				}

				/* Hit to reduce skills */
				case RBE_EXP_10:
				case RBE_EXP_20:
				case RBE_EXP_40:
				case RBE_EXP_80:
				{
					/* Assume draining */
					bool drain = TRUE;

					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc);

					/* Hold life usually prevents life draining */
					if (p_ptr->state.hold_life)
					{
						int save_chance = 0;

						/*get the saving percentage*/
						if (effect == RBE_EXP_10) save_chance = 95;
						else if (effect == RBE_EXP_20) save_chance = 90;
						else if (effect == RBE_EXP_40) save_chance = 75;
						else if (effect == RBE_EXP_80) save_chance = 50;

						if (rand_int(100) < save_chance)
						{
							msg_print("You keep hold of your life force!");
							drain = FALSE;
						}
					}

					/* Drain life */
					if (drain)
					{
						int d = 0;

						/*go through the 4 strengths of drain_life*/
						if (effect == RBE_EXP_10)
							d = damroll(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;

						else if (effect == RBE_EXP_20)
							d = damroll(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						else if (effect == RBE_EXP_40)
							d = damroll(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						else if (effect == RBE_EXP_80)
							d = damroll(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						/*give the player a message*/
						if (p_ptr->state.hold_life)
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


				case RBE_HALLU:
				{
					/* Take damage */
					take_hit(dam, ddesc);

					/* Increase "image" */
					if (!p_ptr->state.resist_chaos)
					{
						if (inc_timed(TMD_IMAGE, 3 + randint(rlev / 2), TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_CHAOS);

					break;
				}
			}

			/* Handle character death */
			if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
			{
				l_ptr->deaths++;

				/* Leave immediately */
				return (TRUE);
			}

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

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, dam, effect);

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
				if (k) (void)set_cut(p_ptr->timed[TMD_CUT] + k);
			}

			/* Handle stun */
			if (do_stun)
			{

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, dam, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0:  k = 0; break;
					case 1:  k = randint(5); break;
					case 2:  k = rand_range( 8, 16); break;
					case 3:  k = rand_range(15, 30); break;
					case 4:  k = rand_range(25, 50); break;
					case 5:  k = rand_range(35, 70); break;
					case 6:  k = rand_range(45, 90); break;
					default: k = 100; break;

				}

				/* Apply the stun */
				if (k) (void)set_stun(p_ptr->timed[TMD_STUN] + k);
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
				case RBM_PECK:
				case RBM_STING:
				case RBM_BREATHE:
				case RBM_BUTT:
				case RBM_CRUSH:


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
			if (obvious || dam || (l_ptr->blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[ap_cnt] < MAX_UCHAR)
				{
					l_ptr->blows[ap_cnt]++;
				}
			}
		}

		/*hack - stop attacks if monster and player are no longer next to each other*/
		if (do_break) break;

	}


	/* Blink away */
	if ((blinked) && (alive))
	{

		if (teleport_away(m_idx, MAX_SIGHT * 2 + 5))
		{
			msg_print("There is a puff of smoke!");
		}
	}

	/* Assume we attacked */
	return (TRUE);
}







/*********************************************************************/
/*                                                                   */
/*                      Monster Ranged Attacks                       */
/*                                                                   */
/*********************************************************************/


/*
 * Using an input value for average damage, and another that controls
 * variability, return the actual base damage of a monster's attack
 * spell.  The larger the value for "control", the less likely the damage
 * will vary greatly.
 */
int get_dam(monster_race *r_ptr, int attack)
{
	int dam = 0;
	int spread;

	int control, av_dam;

	byte power = r_ptr->spell_power;

	/* Determine mana cost */
	if (attack >= 224) return (FALSE);
	else if (attack >= 192)
	{
		av_dam = power * spell_info_RF7[attack-192][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF7[attack-192][COL_SPELL_DAM_DIV]);
		control = spell_info_RF7[attack-192][COL_SPELL_DAM_VAR];
	}
	else if (attack >= 160)
	{
		av_dam = power * spell_info_RF6[attack-160][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF6[attack-160][COL_SPELL_DAM_DIV]);
		control = spell_info_RF6[attack-160][COL_SPELL_DAM_VAR];
	}
	else if (attack >= 128)
	{
		av_dam = power * spell_info_RF5[attack-128][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF5[attack-128][COL_SPELL_DAM_DIV]);
		control = spell_info_RF5[attack-128][COL_SPELL_DAM_VAR];
	}
	else if (attack >=  96)
	{
		av_dam = power * spell_info_RF4[attack- 96][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF4[attack- 96][COL_SPELL_DAM_DIV]);
		control = spell_info_RF4[attack- 96][COL_SPELL_DAM_VAR];
	}
	else return (FALSE);

	/*Hack - handle Wound spell differently */
	if (attack == 160+20)
	{
		if (power > 75) av_dam = 225 + power - 75;
	}


	/*No point in going through this to return 0*/
	if (av_dam < 1) return (FALSE);

	/* Damage may never differ by more than 50% from the average */
	if (control < 4) control = 4;

	/*
	 * Get the allowable spread (two standard deviations, or 100,
	 * whichever is less).
	 */
	spread = MIN(100, av_dam * 2 / control);

	/* Loop until damage is within the allowable spread */
	while (TRUE)
	{
		/* Randomize damage (average, standard deviation) */
		dam = Rand_normal(av_dam, div_round(av_dam, control));

		/* Forbid too great a variation */
		if (dam > av_dam + spread) continue;
		if (dam < av_dam - spread) continue;

		/* Accept */
		break;
	}

	/* Return randomized damage */
	return (dam);
}

/*
 * Return the max damage by GF type.  It is important to
 * also have a corresponding damage in get_breath_dam and
 * get_ball_dam below, or a damage of zero will be returned.
 */
static int get_max_dam(int gf_type, bool powerful)
{
	switch (gf_type)
	{
		case GF_ACID:
		case GF_ELEC:
		case GF_FIRE:
		case GF_COLD:
		case GF_ICE:
		{
			return (1600);
		}

		case GF_POIS: return (800);

		case GF_PLASMA:
		case GF_INERTIA:
		case GF_FORCE:
		case GF_TIME:
		{
			if (powerful) return (400);
			else return (150);
		}

		case GF_LIGHT:
		case GF_DARK:
		case GF_CONFUSION:
		case GF_HOLY_ORB:
		{
			return (400);
		}

		case GF_SOUND:
		{
			if (powerful) return (500);
			else return (150);
		}

		case GF_SHARD:
		case GF_LAVA:
		case GF_CHAOS:
		case GF_DISENCHANT:
		case GF_METEOR:
		case GF_WATER:
		{
			return (500);
		}

		case GF_GRAVITY:
		{
			if (powerful) return (300);
			else return (150);
		}

		case GF_NEXUS: 	return (450);

		case GF_NETHER:	return (550);

		case GF_MANA:
		{
			if (powerful) return (400);
			else return (250);
		}
	}

	/* Return breath damage */
	return (0);
}


/*
 * Get the breath damage by GF type  It is important to
 * also have a corresponding max damage by GF type in get_max_dam above,
 * or a damage of zero will be returned.
 */
int get_breath_dam(s16b hit_points, int gf_type, bool powerful)
{
	int dam;
	int max_dam = get_max_dam(gf_type, powerful);

	switch (gf_type)
	{
		case GF_ACID:
		case GF_ELEC:
		case GF_FIRE:
		case GF_COLD:
		case GF_POIS:
		case GF_LAVA:
		case GF_TIME:
		case GF_MANA:
		case GF_HOLY_ORB:
		{
			dam = hit_points / 3;
			break;
		}

		case GF_PLASMA:
		case GF_FORCE:
		{
			if (powerful) dam = hit_points / 3;
			else dam = hit_points / 6;
			break;
		}
		case GF_LIGHT:
		case GF_DARK:
		case GF_CONFUSION:
		case GF_SHARD:
		case GF_NEXUS:
		case GF_NETHER:
		case GF_CHAOS:
		case GF_DISENCHANT:
		case GF_METEOR:
		{
			dam = hit_points / 6;
			break;
		}

		case GF_WATER:
		{
			dam = hit_points / 4;
			break;
		}

		case GF_SOUND:
		{
			if (powerful) dam = hit_points / 4;
			else dam = hit_points / 9;
			break;
		}

		case GF_INERTIA:
		case GF_GRAVITY:
		{
			if (powerful) 	dam = hit_points / 4;
			else 			dam = hit_points / 8;
			break;
		}

		/*Whoops!*/
		default: return (FALSE);
	}

	/* Return breath damage, don't exceed max damage */
	if (max_dam <= dam) return (max_dam);
	return (dam);
}

/*
 * Get the ball damage by GF type  It is important to
 * also have a corresponding max damage by GF type in get_max_dam above,
 * or a damage of zero will be returned.
 *
 * An m_idx of (-1) means we want damage based solely on the monster race, and
 * to not factor in the HP of the specific monster.
 *
 */

int get_ball_beam_dam(int m_idx, monster_race *r_ptr, int attack, int gf_type, bool powerful)
{
	int dam = get_dam(r_ptr, attack);
	int max_dam = get_max_dam(gf_type, powerful);

	/* Factor in the hp of the specific monster if called for */
	if (m_idx > 0)
	{
		monster_type *m_ptr = &mon_list[m_idx];

		/* Paranoia - make sure it is a real monster */
		if (m_ptr->r_idx)
		{
			int breath_dam = get_breath_dam(m_ptr->hp, gf_type, powerful);

			/*
			 * Average the damage of a fully healthy creature, and the current monster, so
			 * ball (breath) spells lose some power, but not as much as plain breath spells.
			 */
			dam = (breath_dam + dam) / 2;
		}
	}

	/* Return breath damage, don't exceed max damage */
	if (max_dam <= dam) return (max_dam);
	return (dam);
}

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void mon_bolt(int m_idx, int typ, int dam, u32b flg)
{
	monster_type *m_ptr = &mon_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	flg |= PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY | PROJECT_EFCT;

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, fy, fx, py, px, dam, typ, flg, 0 , 0);
}

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player, but doesn't leave an effect
 */
static void mon_bolt_no_effect(int m_idx, int typ, int dam)
{
	monster_type *m_ptr = &mon_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, fy, fx, py, px, dam, typ, flg, 0 , 0);
}

/*
 * Cast a beam at the player, sometimes with limited range.
 * Do not stop if we hit a monster
 * Affect grids, monsters, and the player
 */
static void mon_beam(int m_idx, int typ, int dam, int range)
{
	monster_type *m_ptr = &mon_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
				PROJECT_PLAY | PROJECT_SAME ;

	/* Target the player with a beam attack */
	(void)project(m_idx, range, fy, fx, py, px, dam, typ, flg ,0 ,0);
}

/*
 * Cast a ball spell at the player
 * Can go in squares next to player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void mon_ball(int m_idx, int typ, int dam, int rad, int py, int px)
{

	monster_type *m_ptr = &mon_list[m_idx];
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
				PROJECT_PLAY | PROJECT_WALL | PROJECT_EFCT | PROJECT_SAME);

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, fy, fx, py, px, dam, typ, flg, 0, 0);
}

/*
 * Release a cloud, which is a ball centered on the monster that does not
 * affect other monsters (mostly to avoid annoying messages).
 *
 * Consider being less graphics-intensive.
 */
void mon_cloud(int m_idx, int typ, int dam, int rad)
{
	monster_type *m_ptr = &mon_list[m_idx];
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg =  (PROJECT_PLAY | PROJECT_EFCT | PROJECT_CLOUD);

	/* Nazgul and Silver Jellies darken the dungeon */
	if ((typ == GF_DARK) || (typ == GF_DARK_WEAK))
	{
		flg |= PROJECT_GRID;
	}

	/* Surround the monster with a cloud */
	(void)project(m_idx, rad, fy, fx, fy, fx, dam, typ, flg, 0, 0);

}


/*
 * Breathe or cast an arc-shaped spell at the player.
 * Use an arc spell of specified range and width.
 * Optionally, do not harm monsters with the same r_idx.
 * Affect grids, objects, monsters, and (specifically) the player
 *
 * Monster breaths do not lose strength with distance at the same rate
 * that normal arc spells do.  If the monster is "powerful", they lose
 * less strength; otherwise, they lose more.
 */
static void mon_arc(int m_idx, int typ, bool noharm, int dam, int rad, int degrees_of_arc)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = (PROJECT_ARC | PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_SAME | \
	           PROJECT_KILL | PROJECT_PLAY | PROJECT_WALL | PROJECT_EFCT);

	/* Diameter of source of energy is at least 20. */
	int diameter_of_source = 20;

	/* XXX XXX -- POWERFUL monster breaths lose less damage with range. */
	int degree_factor = (r_ptr->flags2 & (RF2_POWERFUL)) ? 180 : 90;

	/*unused variable*/
	(void)noharm;

	/* Narrow arcs lose relatively little energy over distance. */
	if (degrees_of_arc < degree_factor)
	{
		if (degrees_of_arc <= 6) diameter_of_source = rad * 10;
		else diameter_of_source = diameter_of_source * degree_factor /
			degrees_of_arc;
	}

	/* Radius of zero means no fixed limit. */
	if (rad == 0) rad = MAX_SIGHT;

	/* Target the player with an arc-shaped attack. */
	(void)project(m_idx, rad, fy, fx, py, px, dam, typ, flg, degrees_of_arc,
		(byte)diameter_of_source);

}



/*
 * Monster attempts to make a ranged (non-melee) attack.
 *
 * Determine if monster can attack at range, then see if it will.  Use
 * the helper function "choose_attack_spell()" to pick a physical ranged
 * attack, magic spell, or summon.  Execute the attack chosen.  Process
 * its effects, and update character knowledge of the monster.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 */
bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px)
{

	int i, k, rlev, spower, rad, manacost;
	int failrate;

	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	/* Summon count */
	int count = 0;

	/* Summon level */
	int summon_lev;

	/* Is the player blind? */
	bool blind = (p_ptr->timed[TMD_BLIND] ? TRUE : FALSE);

	/* Can the player see the monster casting the spell? */
	bool seen = (!blind && m_ptr->ml);

	/* Player is vulnerable */
	bool in_range = player_can_fire_bold(m_ptr->fy, m_ptr->fx);

	bool powerful;

	u16b tmd_flag = (MON_TMD_FLG_NOTIFY);

	if (m_ptr->ml) tmd_flag |= MON_TMD_FLG_SEEN;

	if (r_ptr->flags2 & (RF2_POWERFUL)) powerful = TRUE;
	else powerful = FALSE;

	/* Determine mana cost */
	if (attack >= 224) return (FALSE);
	else if (attack >= 192) manacost = spell_info_RF7[attack-192][COL_SPELL_MANA_COST];
	else if (attack >= 160) manacost = spell_info_RF6[attack-160][COL_SPELL_MANA_COST];
	else if (attack >= 128) manacost = spell_info_RF5[attack-128][COL_SPELL_MANA_COST];
	else if (attack >=  96) manacost = spell_info_RF4[attack- 96][COL_SPELL_MANA_COST];
	else return (FALSE);

	/* Spend mana */
	m_ptr->mana -= manacost;

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_MON_MANA);

	/*Monsters marked as aggressive don't stay that way permanently.  */
	if (m_ptr->mflag & (MFLAG_AGGRESSIVE))
	{
		/* Sometimes remove it */
		if (rand_int(MAX_DEPTH) > r_ptr->level) m_ptr->mflag &= ~(MFLAG_AGGRESSIVE);
	}

	/*** Get some info. ***/

	/* Extract the monster level.  Must be at least 1. */
	rlev = MAX(1, r_ptr->level);

	/* Extract the monster's spell power.  Must be at least 1. */
	spower = MAX(1, r_ptr->spell_power);

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, 0x88);

	/* Get the summon level */
	if (r_ptr->d_char == 'Q') summon_lev = r_ptr->level + 3;
	else                      summon_lev = r_ptr->level - 1;

	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/*stunned monsters always have a chance to fail*/
	if ((m_ptr->m_timed[MON_TMD_STUN]) && (failrate < 10)) failrate = 10;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

	/* Check for spell failure (breath/shot attacks never fail) */
	if ((attack >= 128) && (rand_int(100) < failrate))
	{
		/* Message */
		msg_format("%^s tries to cast a spell, but fails.", m_name);

		return (TRUE);
	}

	/*Monster has cast a spell*/
	m_ptr->mflag &= ~(MFLAG_ALWAYS_CAST);

	/* Hack - Remember the summoner */
	summoner = m_ptr;

	/*** Execute the ranged attack chosen. ***/
	switch (attack)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
			disturb(1, 0);
			sound(MSG_SHRIEK);
			if (r_ptr->flags2 & (RF2_SMART))
				msg_format("%^s shouts for help.", m_name);
			else
				msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

		/* RF4_LASH  (used for whips, but also for spitting) */
		case 96+1:
		{
			/* Attack type and descriptions. */
			int typ;
			cptr desc;
			cptr add_of;
			bool do_blind = FALSE;

			int i;

			/* Get damage and effect of first melee blow. */
			int effect = r_ptr->blow[0].effect;
			int damage = damroll(r_ptr->blow[0].d_dice,
				r_ptr->blow[0].d_side);

			/* Add some more damage for other melee blows. */
			for (i = 1; i < MONSTER_BLOW_MAX; i++)
			{
				damage +=
						damroll(r_ptr->blow[i].d_dice, r_ptr->blow[i].d_side) / 2;
			}

			/* Stop if no damage possible */
			if (!damage) break;

			/* Determine projection type, using effect. */
			switch (effect)
			{
				/* Pure damage with no extras */
				case RBE_HURT:
				{
					typ = GF_ARROW;
					desc = "";
					add_of = "";
					break;
				}
				case RBE_ACID:
				{
					/* Some of attack is pure damage, and so
					 * resists should not be allowed to reduce
					 * damage as much as they do normally.
					 * Damage will be reduced later.
					 */
					if (p_ptr->state.resist_acid) damage *= 2;
					if (p_ptr->timed[TMD_OPP_ACID]) damage *= 2;

					typ = GF_ACID;
					desc = " acid";
					add_of = " of";
					break;
				}
				case RBE_ELEC:
				{
					if (p_ptr->state.resist_elec) damage *= 2;
					if (p_ptr->timed[TMD_OPP_ELEC]) damage *= 2;

					typ = GF_ELEC;
					desc = " lightning";
					add_of = " of";
					break;
				}
				case RBE_FIRE:
				{
					if (p_ptr->state.resist_fire) damage *= 2;
					if (p_ptr->timed[TMD_OPP_FIRE]) damage *= 2;

					typ = GF_FIRE;
					desc = " fire";
					add_of = " of";
					break;
				}
				case RBE_COLD:
				{
					if (p_ptr->state.resist_cold) damage *= 2;
					if (p_ptr->timed[TMD_OPP_COLD]) damage *= 2;

					typ = GF_COLD;
					desc = " frost";
					add_of = " of";
					break;
				}
				case RBE_POISON:
				{
					if (p_ptr->state.resist_pois) damage *= 2;
					if (p_ptr->timed[TMD_OPP_POIS]) damage *= 2;

					typ = GF_POIS;
					desc = " venom";
					add_of = " of";
					break;
				}
				case RBE_BLIND:
				{
					/*hack - some cobras spit poison to blind*/
					if (r_ptr->flags3 & (RF3_ANIMAL))
					{
						typ = GF_POIS;
						desc = " venom";
						add_of = " of";
						do_blind = TRUE;
					}
					/*all other monsters*/
					else
					{
						typ = GF_DARK;
						desc = " blackness";
						add_of = " of";
					}
					break;
				}
				case RBE_CONFUSE:
				case RBE_PARALYZE:
				{
					typ = GF_CONFUSION;
					desc = " confusion";
					add_of = " of";
					break;
				}

				case RBE_UN_BONUS:
				case RBE_UN_POWER:
				{
					typ = GF_DISENCHANT;
					desc = " unmagic";
					add_of = " of";
					break;
				}
				case RBE_LOSE_STR:
				case RBE_LOSE_DEX:
				case RBE_LOSE_CON:
				case RBE_LOSE_INT:
				case RBE_LOSE_WIS:
				case RBE_LOSE_CHR:
				case RBE_LOSE_ALL:
				{
					typ = GF_TIME;
					desc = " ruination";
					add_of = " of";
					break;
				}
				case RBE_EXP_10:
				case RBE_EXP_20:
				case RBE_EXP_40:
				case RBE_EXP_80:
				{
					typ = GF_NETHER;
					desc = " withering";
					add_of = " of";
					break;
				}
				default:
				{
					typ = GF_ARROW;
					desc = "";
					add_of = "";
					break;
				}
			}
			/* XXX -- Animals spit.   Acid-users spit. */
			if ((r_ptr->flags3 & (RF3_ANIMAL)) || (typ == GF_ACID))
			{
				if (blind) msg_print("You hear a soft sound.");
				else if (do_blind)
				{
					msg_format("%^s spits%s straight into your eyes.", m_name, desc);
				}
				else msg_format("%^s spits%s at you.", m_name, desc);
			}
			/* All other creatures use a whip. */
			else
			{
				if (blind) msg_print("You hear a crack.");
				else msg_format("%^s lashes at you with a whip%s%s.",
					m_name, add_of, desc);
			}

			/* Crack the whip, or spit - range 3 */
			mon_beam(m_idx, typ, damage, 3);

			if (do_blind)
			{
				/* Increase blindness */
				if (!p_ptr->state.resist_blind)
				{
					(void)inc_timed(TMD_BLIND, 10 + randint(rlev), TRUE);
				}
			}

			break;
		}
		/* RF4_BOULDER */
		case 96+2:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear something grunt with exertion.");
			else if (spower < 8) msg_format("%^s hurls a rock at you.", m_name);
			else msg_format("%^s hurls a boulder at you.", m_name);
			mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_ROCK);
			break;
		}

		/* RF4_SHOT */
		case 96+3:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear something whirl towards you.");
			else if (spower < 4) msg_format("%^s slings a pebble at you.", m_name);
			else if (spower < 10) msg_format("%^s slings a leaden pellet at you.", m_name);
			else msg_format("%^s slings a seeker shot at you.", m_name);

			mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_SHOT);
			break;
		}

		/* RF4_ARROW */
		case 96+4:
		{
			disturb(1, 0);
			if (spower < 4)
			{
				if (blind) msg_print("You hear a soft twang.");
				else msg_format("%^s fires a small arrow.", m_name);
			}
			else if (spower < 10)
			{
				if (blind) msg_print("You hear a twang.");
				else msg_format("%^s fires an arrow.", m_name);
			}
			else
			{
				if (blind) msg_print("You hear a loud thwang.");
				else msg_format("%^s fires a seeker arrow.", m_name);
			}

			mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_AMMO);
			break;
		}

		/* RF4_BOLT */
		case 96+5:
		{
			disturb(1, 0);
			if (spower < 4)
			{
				if (blind) msg_print("You hear a soft twung.");
				else msg_format("%^s fires a little bolt.", m_name);
			}
			else if (spower < 10)
			{
				if (blind) msg_format("You hear a twung.");
				else msg_format("%^s fires a crossbow bolt.", m_name);
			}
			else
			{
				if (blind) msg_print("You hear a loud thwung.");
				else msg_format("%^s fires a seeker bolt.", m_name);
			}

			mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_AMMO);
			break;
		}

		/* RF4_MISSL */
		case 96+6:
		{
			disturb(1, 0);
			if (spower < 4)
			{
				if (blind) msg_print("You hear something small coming at you.");
				else msg_format("%^s fires a little missile.", m_name);
			}
			else if (spower < 10)
			{
				if (blind) msg_format("You hear something coming at you..");
				else msg_format("%^s fires a missile.", m_name);
			}
			else
			{
				if (blind) msg_print("You hear something powerful coming at you..");
				else msg_format("%^s fires a heavy missile.", m_name);
			}

			mon_bolt(m_idx, GF_MISSILE, get_dam(r_ptr, attack), PROJECT_AMMO);
			break;
		}

		/* RF4_PMISSL */
		case 96+7:
		{
			disturb(1, 0);

			if (blind) msg_print("You hear a soft 'fftt' sound.");
			else msg_format("%^s whips a poisoned dart at you.", m_name);
			mon_bolt_no_effect(m_idx, GF_POIS, get_dam(r_ptr, attack));
			break;
		}

		/* RF4_BRTH_ACID */
		case 96+8:
		{
			disturb(1, 0);
			sound(MSG_BR_ACID);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_ACID, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			/*
			 * Breaths are 40-degree arcs for POWERFUL monsters,
			 * 20 degrees for others.
			 */
			mon_arc(m_idx, GF_ACID, TRUE, get_breath_dam(m_ptr->hp, GF_ACID, powerful),
			        0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_ELEC */
		case 96+9:
		{
			disturb(1, 0);
			sound(MSG_BR_ELEC);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_ELEC, (m_ptr->hp / 4), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_ELEC, TRUE, get_breath_dam(m_ptr->hp, GF_ELEC, powerful),
			        0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_FIRE */
		case 96+10:
		{
			disturb(1, 0);
			sound(MSG_BR_FIRE);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_FIRE, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_FIRE, TRUE, get_breath_dam(m_ptr->hp, GF_FIRE, powerful),
			        0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_COLD */
		case 96+11:
		{
			disturb(1, 0);
			sound(MSG_BR_FROST);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes frost.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_COLD, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_COLD, TRUE, get_breath_dam(m_ptr->hp, GF_COLD, powerful),
			       0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_POIS */
		case 96+12:
		{
			disturb(1, 0);
			sound(MSG_BR_GAS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gas.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_POIS, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_POIS, TRUE, get_breath_dam(m_ptr->hp, GF_POIS, powerful),
			        0, (powerful ? 50 : 30));
			break;
		}

		/* RF4_BRTH_PLAS */
		case 96+13:
		{
			disturb(1, 0);
			sound(MSG_BR_PLASMA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes plasma.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_PLASMA, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_PLASMA, TRUE, get_breath_dam(m_ptr->hp, GF_PLASMA, powerful),
				   0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_LIGHT */
		case 96+14:
		{
			disturb(1, 0);
			sound(MSG_BR_LIGHT);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes light.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_LIGHT, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_LIGHT, TRUE, get_breath_dam(m_ptr->hp, GF_LIGHT, powerful),
			        0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_DARK */
		case 96+15:
		{
			disturb(1, 0);
			sound(MSG_BR_DARK);
			if (blind) msg_format("%^s breathes.", m_name);
			msg_format("%^s breathes darkness.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_DARK, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_DARK, TRUE, get_breath_dam(m_ptr->hp, GF_DARK, powerful),
					 0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_CONFU */
		case 96+16:
		{
			disturb(1, 0);
			sound(MSG_BR_CONF);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes confusion.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_CONFUSION, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_CONFUSION, TRUE, get_breath_dam(m_ptr->hp, GF_CONFUSION, powerful),
			       0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_SOUND */
		case 96+17:
		{
			disturb(1, 0);
			sound(MSG_BR_SOUND);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sound.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_SOUND, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_SOUND, TRUE, get_breath_dam(m_ptr->hp, GF_SOUND, powerful),
			       0, (powerful ? 50 : 30));
			break;
		}

		/* RF4_BRTH_SHARD */
		case 96+18:
		{
			disturb(1, 0);
			sound(MSG_BR_SHARDS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes shards.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_SHARD, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_SHARD, TRUE, get_breath_dam(m_ptr->hp, GF_SHARD, powerful),
			        0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_INER */
		case 96+19:
		{
			disturb(1, 0);
			sound(MSG_BR_INERTIA);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes inertia.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_INERTIA, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_INERTIA, TRUE, get_breath_dam(m_ptr->hp, GF_INERTIA, powerful),
				   0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_GRAV */
		case 96+20:
		{
			disturb(1, 0);
			sound(MSG_BR_GRAVITY);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_GRAVITY, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_GRAVITY, TRUE, get_breath_dam(m_ptr->hp, GF_GRAVITY, powerful),
				   0, (powerful ? 40 : 20));
			break;
		}

		/* Unused */
		case 96+21:
		{
			break;
		}

		/* RF4_BRTH_FORCE */
		case 96+22:
		{
			disturb(1, 0);
			sound(MSG_BR_FORCE);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes force.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_FORCE, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_FORCE, TRUE, get_breath_dam(m_ptr->hp, GF_FORCE, powerful),
				   0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_NEXUS */
		case 96+23:
		{
			disturb(1, 0);
			sound(MSG_BR_NEXUS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nexus.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_NEXUS, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_NEXUS, TRUE, get_breath_dam(m_ptr->hp, GF_GRAVITY, powerful),
			       0, (powerful ? 40 : 20));
			break;
		}
		/* RF4_BRTH_NETHR */
		case 96+24:
		{
			disturb(1, 0);
			sound(MSG_BR_NETHER);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nether.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_NETHER, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_NETHER, TRUE, get_breath_dam(m_ptr->hp, GF_NETHER, powerful),
			       0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_CHAOS */
		case 96+25:
		{
			disturb(1, 0);
			sound(MSG_BR_CHAOS);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes chaos.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_CHAOS, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_CHAOS, TRUE, get_breath_dam(m_ptr->hp, GF_CHAOS, powerful),
			       0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_DISE */
		case 96+26:
		{
			disturb(1, 0);
			sound(MSG_BR_DISENCHANT);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disenchantment.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_DISENCHANT, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_DISENCHANT, TRUE, get_breath_dam(m_ptr->hp, GF_DISENCHANT, powerful),
			       0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_TIME */
		case 96+27:
		{
			disturb(1, 0);
			sound(MSG_BR_TIME);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes time.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_TIME, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_TIME, TRUE, get_breath_dam(m_ptr->hp, GF_TIME, powerful),
				   0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_BRTH_MANA */
		case 96+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes raw mana.", m_name);

			/* Handle Moria breath spells the old fashioned way */
			if (game_mode == GAME_NPPMORIA)
			{
				mon_ball(m_idx, GF_TIME, (m_ptr->hp / 3), 2, py, px);
				break;
			}

			mon_arc(m_idx, GF_MANA, TRUE, get_breath_dam(m_ptr->hp, GF_MANA, powerful),
				    0, (powerful ? 40 : 20));
			break;
		}

		/* RF4_XXX6 */
		case 96+29:
		{
			break;
		}

		/* RF4_XXX7 */
		case 96+30:
		{
			break;
		}

		/* RF4_XXX8 */
		case 96+31:
		{
			break;
		}

		/* RF5_BALL_ACID */
		case 128+0:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_ACID))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of acid.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of acid.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a small acid ball.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts an acid ball.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of acid.", m_name);
				if (spower < 80) rad = 3;
				else rad = 4;
			}
			mon_ball(m_idx, GF_ACID, get_ball_beam_dam(m_idx, r_ptr, attack, GF_ACID, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_ELEC */
		case 128+1:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_ELEC))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of electricity.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of electricity.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a small ball of electricity.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a ball of electricity.", m_name);
			}

			/* Electricity is the most variable of all attacks at high level. */
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);

				if (rand_int(3) != 0)
				{
				msg_format("%^s invokes a storm of electricity.", m_name);
					if (spower < 80) rad = 3;
					else rad = 4;
					spower = 3 * spower / 4;
				}
				else
				{
					msg_format("%^s calls a massive stroke of lightning down upon you!", m_name);
					rad = 0;
					spower = 3 * spower / 2;
				}
			}
			mon_ball(m_idx, GF_ELEC, get_ball_beam_dam(m_idx, r_ptr, attack, GF_ELEC, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_FIRE */
		case 128+2:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_FIRE))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of flames.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of flames.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of fire.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a ball of fire.", m_name);
			}
			else if (spower < 80)
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a firestorm.", m_name);
				rad = 3;
			}
			else
			{
				if (blind) msg_format("%^s intones in rising wrath.", m_name);
				else msg_format("%^s conjures up a maelstrom of fire!", m_name);
				rad = 4;
			}
			mon_ball(m_idx, GF_FIRE, get_ball_beam_dam(m_idx, r_ptr, attack, GF_FIRE, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_COLD */
		case 128+3:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_COLD))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of frost.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of frost.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a small frost ball.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a frost ball.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of frost.", m_name);
				if (spower < 80) rad = 3;
				else rad = 4;
			}
			mon_ball(m_idx, GF_COLD, get_ball_beam_dam(m_idx, r_ptr, attack, GF_COLD, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_POIS */
		case 128+4:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_POIS))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of poison.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of poison.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a stinking cloud.", m_name);
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a venomous cloud.", m_name);
				rad = 3;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of poison.", m_name);
				if (spower < 80) rad = 4;
				else rad = 5;
			}
			mon_ball(m_idx, GF_POIS, get_ball_beam_dam(m_idx, r_ptr, attack, GF_POIS, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_LIGHT */
		case 128+5:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_LIGHT))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of light.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes a brilliant ball of light.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a sphere of light.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s invokes an explosion of light.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a powerful explosion of light.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_LIGHT, get_ball_beam_dam(m_idx, r_ptr, attack, GF_LIGHT, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_DARK */
		case 128+6:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_DARK))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of darkness.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of darkness.", m_name);
					rad = 3;
				}
			}
			else if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of darkness.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a storm of darkness.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a powerful darkness storm.", m_name);
				if (spower < 110) rad = 3;
				else rad = 4;
			}
			mon_ball(m_idx, GF_DARK, get_ball_beam_dam(m_idx, r_ptr, attack, GF_DARK, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_CONFU */
		case 128+7:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_CONFU))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of confusion.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an massive ball of confusion.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of confusion.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a storm of confusion.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a powerful storm of confusion.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_CONFUSION, get_ball_beam_dam(m_idx, r_ptr, attack, GF_CONFUSION, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_SOUND */
		case 128+8:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_SOUND))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of noise.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an ear-splitting ball of noise.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s calls up a blast of sound.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s invokes a thunderclap.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s unleashes a cacophony of sound.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_SOUND, get_ball_beam_dam(m_idx, r_ptr, attack, GF_SOUND, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_SHARD */
		case 128+9:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_SHARD))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of shards.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of shards.", m_name);
					rad = 3;
				}
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s calls up up a blast of shards.", m_name);
				rad = 1;
			}
			else if (spower < 50)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s calls up a whirlwind of shards.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of shards!", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_SHARD, get_ball_beam_dam(m_idx, r_ptr, attack, GF_SHARD, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_METEOR */
		case 128+10:
		{
			disturb(1, 0);
			if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s produces a meteor shower.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s produces a meteor storm.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s murmurs strongly.", m_name);
				else msg_format("%^s produces a violent meteor storm.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_METEOR, get_ball_beam_dam(m_idx, r_ptr, attack, GF_METEOR, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_STORM */
		case 128+11:
		{
			disturb(1, 0);

			if (spower < 22)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s gestures fluidly.", m_name);
				msg_print("You are surrounded by a little storm.");
				rad = 2;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s gestures fluidly.", m_name);
				msg_print("You are engulfed in a whirlpool.");
				rad = 3;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s gestures fluidly.", m_name);
				msg_print("You are lost in a raging tempest of wind and water!");
				rad = 5;
			}
			mon_ball(m_idx, GF_WATER, get_ball_beam_dam(m_idx, r_ptr, attack, GF_WATER, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_NETHR */
		case 128+12:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_NETHR))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a nether ball.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous nether ball.", m_name);
					rad = 3;
				}
			}
			else if (spower < 22)
			{
				if (blind) msg_format("%^s whispers nastily.", m_name);
				else msg_format("%^s casts an orb of nether.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs a deadly word.", m_name);
				else msg_format("%^s casts a nether ball.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s intones with deadly menace.", m_name);
				else msg_format("%^s calls up a storm of nether magics.", m_name);
			rad = 3;
			}
			mon_ball(m_idx, GF_NETHER, get_ball_beam_dam(m_idx, r_ptr, attack, GF_NETHER, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_CHAOS */
		case 128+13:
		{
			disturb(1, 0);

			rad = 2;

			/* Special handling for breathers as opposed to casters */
			if (r_ptr->flags4 & (RF4_BRTH_CHAOS))
			{
				if (spower < 40)
				{
					if (blind) msg_format("%^s breathes.", m_name);
					else msg_format("%^s breathes a ball of chaos.", m_name);
				}
				else
				{
					if (blind) msg_format("%^s breathes forcefully.", m_name);
					msg_format("%^s breathes an enormous ball of chaos.", m_name);
					rad = 3;
				}
			}
			else if (spower < 13)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a sphere of chaos.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts an exlosion of raw chaos.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of chaos.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_CHAOS, get_ball_beam_dam(m_idx, r_ptr, attack, GF_CHAOS, powerful), rad, py, px);
			break;
		}

		/* RF5_BALL_MANA */
		case 128+14:
		{
			disturb(1, 0);
			if (spower < 25)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a mana burst.", m_name);
				rad = 1;
			}
			else if (spower < 50)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a mana ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of mana.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_MANA, get_ball_beam_dam(m_idx, r_ptr, attack, GF_MANA, powerful), rad, py, px);

			break;
		}

		/* RF5_BALL_WATER */
		case 128+15:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a small water ball.", m_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a water ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of water.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
			}
			mon_ball(m_idx, GF_WATER, get_ball_beam_dam(m_idx, r_ptr, attack, GF_WATER, powerful), rad, py, px);
			break;
		}

		/* RF5_BOLT_ACID */
		case 128+16:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an acid bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a bolt of acid.", m_name);
			}
			mon_bolt(m_idx, GF_ACID, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_ELEC */
		case 128+17:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a bolt of electricity.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a bolt of lightning.", m_name);
			}
			mon_bolt(m_idx, GF_ELEC, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_FIRE */
		case 128+18:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a fire bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s throws a fiery sphere at you.", m_name);
			}
			mon_bolt(m_idx, GF_FIRE, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_COLD */
		case 128+19:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a frost bolt.", m_name);
			}
			else
		{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a frost bolt.", m_name);
			}
			mon_bolt(m_idx, GF_COLD, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_POIS */
		case 128+20:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a poison bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a bolt of venom.", m_name);
			}
			mon_bolt(m_idx, GF_POIS, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_PLAS */
		case 128+21:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a plasma bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a bolt of plasma.", m_name);
			}
			mon_bolt(m_idx, GF_PLASMA, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_ICE */
		case 128+22:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an ice bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a bolt of ice.", m_name);
			}
			mon_bolt(m_idx, GF_ICE, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_WATER */
		case 128+23:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a water bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a water bolt.", m_name);
			}
			mon_bolt(m_idx, GF_WATER, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_NETHR */
		case 128+24:
		{
			disturb(1, 0);
			if (spower < 40)
			{
				if (blind) msg_format("%^s whispers nastily.", m_name);
				else msg_format("%^s casts a nether bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs a deadly word.", m_name);
				else msg_format("%^s hurls a black bolt of nether at you.", m_name);
			}
			mon_bolt(m_idx, GF_NETHER, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_MANA */
		case 128+25:
		{
			disturb(1, 0);
			if ((spower < 5) || (spower <= rlev / 10))
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a magic missile.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a mana bolt.", m_name);
			}
			mon_bolt(m_idx, GF_MANA, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BOLT_GRAV */
		case 128+26:
		{
			disturb(1, 0);
			if ((spower < 5) || (spower <= rlev / 10))
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s fires a gravity bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a powerful bolt of gravity.", m_name);
			}
			mon_bolt(m_idx, GF_GRAVITY, get_dam(r_ptr, attack), 0L);
			break;
		}

		/* RF5_BEAM_ELEC */
		case 128+27:
		{
			disturb(1, 0);
			if (blind) msg_print("You feel a crackling in the air.");

			/* Special handling for breathers as opposed to casters */
			else if (r_ptr->flags4 & (RF4_BRTH_ELEC))
			{
				msg_format("%^s breathes a lightning bolt.", m_name);
			}
			else msg_format("%^s shoots a spark of lightning at you.", m_name);

			mon_beam(m_idx, GF_ELEC, get_ball_beam_dam(-1, r_ptr, attack, GF_ELEC, powerful), 10);
			break;
		}

		/* RF5_BEAM_ICE */
		case 128+28:
		{
			disturb(1, 0);
			if (r_ptr->flags4 & (RF4_BRTH_COLD))
			{
				if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes an icy spear", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts an icy lance.", m_name);
			}
			mon_beam(m_idx, GF_ICE, get_ball_beam_dam(-1, r_ptr, attack, GF_ICE, powerful), 12);
			break;
		}

		/* RF5_BEAM_NETHR */
		case 128+29:
		{
			disturb(1, 0);
			if (r_ptr->flags4 & (RF4_BRTH_NETHR))
			{
				if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes a beam of nether", m_name);
			}
			else if (spower < 25)
			{
				if (blind) msg_format("%^s whispers nastily.", m_name);
				else msg_format("%^s casts a beam of nether.", m_name);
			}
			else if (spower < 50)
			{
				if (blind) msg_format("%^s murmurs a deadly word.", m_name);
				else msg_format("%^s hurls a nether lance.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s intones with deadly menace.", m_name);
				else msg_format("%^s unleashes a ray of death.", m_name);
			}
			mon_beam(m_idx, GF_NETHER, get_ball_beam_dam(-1, r_ptr, attack, GF_NETHER, powerful), 10);
			break;
		}

		/* RF5_BEAM_LAVA */
		case 128+30:
		{
			disturb(1, 0);
			/* SLightly different message for breathers */
			if (r_ptr->flags4 & (RF4_BRTH_ALL))
			{
				if (blind) msg_format("%^s breathes.", m_name);
				else msg_format("%^s breathes a stream of fiery lava.", m_name);
			}
			else if (spower < 25)
			{
				if (blind) msg_format("%^s begins murmuring.", m_name);
				else msg_format("%^s shoots a beam of molten magma.", m_name);
			}
			else if (spower < 50)
			{
				if (blind) msg_format("%^s mubles something.", m_name);
				else msg_format("%^s shoots a jet of lava.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s mubles something.", m_name);
				else msg_format("%^s shoots a searing jet of lava.", m_name);
			}
			mon_beam(m_idx, GF_LAVA, get_ball_beam_dam(-1, r_ptr, attack, GF_LAVA, powerful), 10);
			break;
		}

		/* RF5_HOLY_ORB */
		case 128+31:
		{
			disturb(1, 0);
			if (spower < 40)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an orb of draining.", m_name);
				rad = 1;
			}
			else if (spower < 90)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a powerful orb of draining.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s casts a large orb of holy might.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_HOLY_ORB, get_ball_beam_dam(-1, r_ptr, attack, GF_HOLY_ORB, powerful), rad, py, px);

			break;
		}


		/* RF6_HASTE */
		case 160+0:
		{
			if (in_range)
			{
				disturb(1, 0);

				if (!seen)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s concentrates on %s body.", m_name, m_poss);
				}
			}

			/*add to the haste counter*/
			mon_inc_timed(m_idx, MON_TMD_FAST, r_ptr->level + rand_int(r_ptr->level), MON_TMD_FLG_NOTIFY);

			break;
		}

		/* RF6_ADD_MANA */
		case 160+1:
		{
			if (in_range)
			{
				disturb(1, 0);

				if (!seen)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s gathers %s power.", m_name, m_poss);
				}
			}

			/* Increase current mana.  Do not exceed maximum. */
			m_ptr->mana += (spower / 15) + 1;
			if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
			int gain, cost;

			if (in_range)
			{
				disturb(1, 0);

				/* Message */
				if (!seen)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
				}
			}

			/* We regain lost hitpoints (up to spower * 3) */
			gain = MIN(m_ptr->maxhp - m_ptr->hp, spower * 3);

			/* We do not gain more than mana * 15 HPs at a time */
			gain = MIN(gain, m_ptr->mana * 15);

			/* Regain some hitpoints */
			m_ptr->hp += gain;

			/* Lose some mana (high-level monsters are more efficient) */
			cost = 1 + gain / (5 + 2 * r_ptr->level / 5);

			/* Reduce mana (do not go negetive) */
			m_ptr->mana -= MIN(cost, m_ptr->mana);

			/* Fully healed */
			if (m_ptr->hp >= m_ptr->maxhp)
			{
				/* Fully healed */
				m_ptr->hp = m_ptr->maxhp;

				/* Message */
				if (in_range)
				{
					if (seen) msg_format("%^s looks very healthy!",  m_name);
					else      msg_format("%^s sounds very healthy!", m_name);
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (in_range)
				{
					if (seen) msg_format("%^s looks healthier.",  m_name);
					else      msg_format("%^s sounds healthier.", m_name);
				}
			}


			/* Redraw (later) if needed */
			if (((p_ptr->health_who == m_idx) && (m_ptr->ml)) ||  (m_ptr->sidebar))
				p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (m_ptr->m_timed[MON_TMD_FEAR])
			{
				/* Cancel fear */
				mon_clear_timed(m_idx, MON_TMD_FEAR , tmd_flag);

			}

			/* Recalculate combat range later */
			m_ptr->min_range = 0;

			break;
		}

		/* RF6_CURE */
		case 160+3:
		{
			if (seen) msg_format("%^s concentrates on %s ailments.", m_name, m_poss);

			/* Cancel stunning */
			if (m_ptr->m_timed[MON_TMD_STUN])
			{
				/* Cancel stunning */
				mon_clear_timed(m_idx, MON_TMD_STUN , tmd_flag);

			}

			/* Cancel fear */
			if (m_ptr->m_timed[MON_TMD_FEAR])
			{
				/* Cancel fear */
				mon_clear_timed(m_idx, MON_TMD_FEAR , tmd_flag);
			}

			/* Cancel slowing */
			if (m_ptr->m_timed[MON_TMD_SLOW])
			{
				/* Cancel fear */
				mon_clear_timed(m_idx, MON_TMD_SLOW , tmd_flag);
			}

			/* Redraw (later) if needed */
			if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			if (teleport_away(m_idx, 10))
			{

				if (seen) disturb(1, 0);

				/*
				 * If it comes into view from around a corner (unlikely)
				 * give a message and learn about the casting
				 */
				if (!seen && m_ptr->ml)
				{
					msg_format("%^s blinks into view.", ddesc);
					disturb(1, 0);
					seen = TRUE;
				}

				/* Normal message */
				else
				{
					if (seen) msg_format("%^s blinks away.", m_name);
				}
			}
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			if (teleport_away(m_idx, MAX_SIGHT * 2 + 5))
			{

				if (seen)
				{
					disturb(1, 0);
					msg_format("%^s teleports away.", m_name);
				}

				/*
				 * if it comes into view from around a corner (VERY unlikely)
				 * give a message and learn about the casting
				 */
				else if (m_ptr->ml)
				{
					msg_format("%^s teleports.", ddesc);
					disturb(1, 0);
					seen = TRUE;
				}
			}

			break;
		}

		/* RF6_XXX3 */
		case 160+6:
		{
			break;
		}

		/* RF6_TELE_SELF_TO */
		case 160+7:
		{
			int old_cdis = m_ptr->cdis;

			if (seen) disturb(1, 0);

			/* Move monster near player (also updates "m_ptr->ml"). */
			teleport_towards(m_ptr->fy, m_ptr->fx, py, px);

			/* Use up a little bit more energy than a standard move */
			m_ptr->m_energy -= BASE_ENERGY_MOVE / 2;

			/* Monster is now visible, but wasn't before. */
			if ((!seen) && (m_ptr->ml))
			{
				disturb(1, 0);
				/* Message */
				msg_format("%^s suddenly appears.", ddesc);
			}

			/* Monster was visible before, but isn't now. */
			else if ((seen) && (!m_ptr->ml))
			{
				/* Message */
				msg_format("%^s blinks away.", m_name);
			}

			/* Monster is visible both before and after. */
			else if ((seen) && (m_ptr->ml))
			{
				if (distance(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px) < (old_cdis - 1))
				{
					msg_format("%^s blinks toward you.", m_name);
				}
				else
				{
					msg_format("%^s blinks.", m_name);
				}
			}

			/* Have we seen them at any point?  If so, we will learn about the spell. */
			if (m_ptr->ml) seen = TRUE;

			break;
		}

		/* RF6_TELE_TO */
		case 160+8:
		{
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			teleport_player_to(m_ptr->fy, m_ptr->fx);

			/* Use up a little bit more energy than a standard move */
			m_ptr->m_energy -= BASE_ENERGY_MOVE * 3 / 4;
			break;
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			teleport_player(100, FALSE);
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles strangely.", m_name);
			else msg_format("%^s gestures at your feet.", m_name);

			if (p_ptr->state.resist_nexus)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void)teleport_player_level(m_idx);
			}
			break;
		}

		/* RF6_XXX5 */
		case 160+11:
		{
			break;
		}

		/* RF6_DARKNESS */
		case 160+12:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s gestures in shadow.", m_name);
			(void)unlight_area(0, 3);
			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
			disturb(1, 0);
			sound(MSG_CREATE_TRAP);
			if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
			else msg_format("%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation(SOURCE_OTHER);
			break;
		}

		/* Unused - Was Amnesia attack */
		case 160+14:
		{
			break;
		}

		/* RF6_DRAIN_MANA */
		case 160+15:
		{
			if (p_ptr->csp)
			{
				int r1;

				/* Disturb if legal */
				disturb(1, 0);

				/* Basic message */
				msg_format("%^s draws psychic energy from you!", m_name);

				/* Attack power */
				r1 = (randint(spower) / 20) + 1;

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
				p_ptr->redraw |= (PR_MANA);

				/* Replenish monster mana */
				if (m_ptr->mana < r_ptr->mana)
				{
					if ( r1 > r_ptr->mana - m_ptr->mana)
					{
						 r1 -= r_ptr->mana - m_ptr->mana;
						 m_ptr->mana = r_ptr->mana;
					}
					else
					{
						 m_ptr->mana += r1;
						 r1 = 0;
					}

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_MON_MANA);

				}

				/* Heal the monster with remaining energy */
				if ((m_ptr->hp < m_ptr->maxhp) && (r1))
				{
					/* Heal */
					m_ptr->hp += (30 * (r1 + 1));
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

					/* Special message */
					if (seen)
					{
						msg_format("%^s appears healthier.", m_name);
					}
				}
			}
			break;
		}

		/* RF6_XXX6 */
		case 160+16:
		{
			break;
		}

		/* RF6_XXX7 */
		case 160+17:
		{
			break;
		}

		/* RF6_MIND_BLAST */
		case 160+18:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");
			}
			else
			{
				msg_format("%^s gazes deep into your eyes.", m_name);
			}

			if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				if (allow_player_confusion())
				{
					(void)inc_timed(TMD_CONFUSED, rand_int(4) + 4, TRUE);
				}

				take_hit(get_dam(r_ptr, attack), ddesc);
			}
			break;
		}

		/* RF6_BRAIN_SMASH */
		case 160+19:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");

			}
			else
			{
				msg_format("%^s looks deep into your eyes.", m_name);
			}
			if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				msg_print("Your mind is blasted by psionic energy.");
				take_hit(get_dam(r_ptr, attack), ddesc);
				if (!p_ptr->state.resist_blind)
				{
					(void)inc_timed(TMD_BLIND, 8 + rand_int(8), TRUE);
				}
				if (allow_player_confusion())
				{
					(void)inc_timed(TMD_CONFUSED, rand_int(4) + 4, TRUE);
				}
				if (!p_ptr->state.free_act)
				{
					(void)inc_timed(TMD_PARALYZED, rand_int(4) + 4, TRUE);
				}
				(void)inc_timed(TMD_SLOW, rand_int(4) + 4, TRUE);
			}
			break;
		}

		/* RF6_WOUND */
		case 160+20:
		{
			disturb(1, 0);

			if (spower < 4)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at you and curses.", m_name);
				k = 1;
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles deeply.", m_name);
				else msg_format("%^s points at you and curses horribly.", m_name);
				k = 2;
			}
			else if (spower < 20)
			{
				if (blind) msg_format("%^s murmurs loudly.", m_name);
				else msg_format("%^s points at you, incanting terribly.", m_name);
				k = 3;
			}
			else if (spower < 35)
			{
				if (blind) msg_format("%^s cries out wrathfully.", m_name);
				else msg_format("%^s points at you, screaming words of peril!", m_name);
				k = 4;
			}
			else
			{
				if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
				else msg_format("%^s points at you, screaming the word DIE!", m_name);
				k = 5;
			}

			if (rand_int(rlev / 2 + 70) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_format("You resist the effects%c",
				      (spower < 30 ?  '.' : '!'));
			}
			else
			{
				/*
				 * Inflict damage. Note this spell has a hack
				 * than handles damage differently in get_dam.
				 */
				take_hit(get_dam(r_ptr, attack), ddesc);

				/* Cut the player depending on strength of spell. */
				if (k == 1) (void)set_cut(p_ptr->timed[TMD_CUT] + 8 + damroll(2, 4));
				if (k == 2) (void)set_cut(p_ptr->timed[TMD_CUT] + 23 + damroll(3, 8));
				if (k == 3) (void)set_cut(p_ptr->timed[TMD_CUT] + 46 + damroll(4, 12));
				if (k == 4) (void)set_cut(p_ptr->timed[TMD_CUT] + 95 + damroll(8, 15));
				if (k == 5) (void)set_cut(1200);
			}
			break;
		}

		/* RF6_XXX6 */
		case 160+21:
		{
			break;
		}

		/* RF6_XXX7 */
		case 160+22:
		{
			break;
		}

		/* RF6_XXX8 */
		case 160+23:
		{
			break;
		}

		/* RF6_XXX9 */
		case 160+24:
		{
			break;
		}

		/* RF6_HUNGER */
		case 160+25:
		{
			if (blind) msg_print("You are commanded to feel hungry.");
			else msg_format("%^s gestures at you, and commands that you feel hungry.", m_name);

			if (rand_int(rlev / 2 + 70) > p_ptr->state.skills[SKILL_SAVE])
			{
				/* Reduce food abruptly.  */
				(void)set_food(p_ptr->food - (p_ptr->food/4));

			}

			else msg_print ("You resist the effects!");

			break;
		}

		/* RF6_XX11 */
		case 160+26:
		{
			break;
		}

		/* RF6_SCARE */
		case 160+27:
		{
			disturb(1, 0);
			sound(MSG_CAST_FEAR);
			if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			else msg_format("%^s casts a fearful illusion.", m_name);
			if (p_ptr->state.resist_fear)
			{
				msg_print("You refuse to be frightened.");
			}
			else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You refuse to be frightened.");
			}
			else
			{
				i = div_round(r_ptr->level, 10);
				(void)inc_timed(TMD_AFRAID, i + rand_range(3, 6), TRUE);
			}
			break;
		}

		/* RF6_BLIND */
		case 160+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);

			/* Must not already be blind */
			else if (!p_ptr->timed[TMD_BLIND])
			{
				msg_format("%^s casts a spell, burning your eyes!", m_name);
				if (p_ptr->state.resist_blind)
				{
					msg_print("You are unaffected!");
				}
				else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
				{
					msg_print("You blink, and your vision clears.");
				}
				else
				{
					i = div_round(r_ptr->level, 4);
					(void)inc_timed(TMD_BLIND, i + rand_range(5, 10), TRUE);
				}
			}

			break;
		}

		/* RF6_CONF */
		case 160+29:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s creates a mesmerising illusion.", m_name);
			if (!allow_player_confusion())
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else
			{
				i = div_round(r_ptr->level, 8);
				(void)inc_timed(TMD_CONFUSED, i + rand_range(4, 8), TRUE);
			}
			break;
		}

		/* RF6_SLOW */
		case 160+30:
		{
			disturb(1, 0);
			msg_format("%^s drains power from your muscles!", m_name);
			if (p_ptr->state.free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You resist the effects!");
			}
			else
			{
				i = div_round(r_ptr->level, 25);
				(void)inc_timed(TMD_SLOW, i + rand_range(5, 10), TRUE);
			}
			break;
		}

		/* RF6_HOLD */
		case 160+31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s stares deep into your eyes!", m_name);
			if (p_ptr->state.free_act)
			{
				if (!p_ptr->timed[TMD_PARALYZED]) msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				if (!p_ptr->timed[TMD_PARALYZED]) msg_print("You stare back unafraid!");
			}

			/* Must not already be paralyzed */
			else if (!p_ptr->timed[TMD_PARALYZED])
			{
				(void)inc_timed(TMD_PARALYZED, rand_range(4, 8), TRUE);
			}
			break;
		}

		/* RF7_S_KIN */
		case 192 + 0:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons %s %s.", m_name,
				m_poss, ((r_ptr->flags1) & RF1_UNIQUE ?
				"minions" : "kin"));

			/* Hack -- Set the letter of the monsters to summon */
			summon_kin_type = r_ptr->d_char;
			for (k = 0; k < (r_ptr->level > 40 ? 4 : 3); k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_KIN, 0L);
			}

			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_KIN);

			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF7_XXX */
		case 192 + 1:  break;
		case 192 + 2:  break;


		/* RF7_S_MONSTER */
		case 192 + 3:
		{
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, 0, 0L);
			}

			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_MONSTER);

			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF7_S_MONSTERS */
		case 192 + 4:
		{
			disturb(1, 0);
			sound(MSG_SUM_MONSTER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, 0, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_MONSTERS);

			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF7_XXX */
		case 192 + 5:  break;
		case 192 + 6:  break;
		case 192 + 7:  break;

		/* RF7_S_ANT */
		case 192 + 8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ants.", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_ANT, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_ANT);

			if (blind && count) msg_print("You hear chittering and skittering.");
			break;
		}

		/* RF7_S_SPIDER */
		case 192 + 9:
		{
			disturb(1, 0);
			sound(MSG_SUM_SPIDER);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons spiders.", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_SPIDER, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_SPIDER);

			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF7_S_HOUND */
		case 192 + 10:
		{
			disturb(1, 0);
			sound(MSG_SUM_HOUND);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hounds.", m_name);
			for (k = 0; k < 2; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_HOUND, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_HOUND);

			if (blind && count) msg_print("You hear snarling.");
			break;
		}

		/* RF7_S_ANIMAL */
		case 192 + 11:
		{
			disturb(1, 0);
			sound(MSG_SUM_ANIMAL);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons natural creatures.", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_ANIMAL, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_ANIMAL);

			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF7_S_HYDRA */
		case 192 + 12:
		{
			disturb(1, 0);
			sound(MSG_SUM_HYDRA);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hydras.", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HYDRA, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_HYDRA);

			if (blind && count)
			{
				msg_print("You hear hissing and slithering.");
			}
			break;
		}

		/* RF7_XXX */
		case 192 + 13:	break;

		/* RF7_S_THIEF */
		case 192 + 14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s whistles.", m_name);
			else msg_format("%^s whistles up a den of thieves!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_THIEF, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_THIEF);

			if (blind && count) msg_print("You hear footsteps and the whetting of knives.");
			break;
		}

		/* Summon Bert, Bill, and Tom */
		/* No messages unless sucessful */
		/* RF7_S_BERTBILLTOM */
		case 192 + 15:
		{
			for (k = 0; k < 2; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev + 5, SUMMON_BERTBILLTOM, 0L);
			}

			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_BERTBILLTOM);

			/* No messages unless successful */
			if (count)
			{
				if (blind) msg_print("Heavy footsteps approach!");
				else       msg_format("%^s calls up his friends!", m_name);
			}
			break;
		}

		/* RF7_XXX */
		case 192 + 16:	break;

		/* RF6_S_AINU */
		case 192 + 17:
		{
			disturb(1, 0);
			sound(MSG_SUM_AINU);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a maia!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_AINU, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_AINU);

			if (blind && count) msg_print("Majestic songs of the Ainur fill the dungeon!");
			break;
		}

		case 192 + 18:	break;
		case 192 + 19:	break;

		/* RF7_S_DRAGON */
		case 192 + 20:
		{
			disturb(1, 0);
			sound(MSG_SUM_DRAGON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_DRAGON, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_DRAGON);

			if (blind && count) msg_print("You feel something breathing on you...");
			break;
		}

		/* RF7_S_HI_DRAGON */
		case 192 + 21:
		{
			disturb(1, 0);
			sound(MSG_SUM_HI_DRAGON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ancient dragons!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_HI_DRAGON, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_HI_DRAGON);

			if (blind && count)
			{
				msg_print("You feel the breath of great wyrms.");
			}
			break;
		}

		/* RF7_XXX */
		case 192 + 22:	break;
		case 192 + 23:	break;

		/* RF7_S_DEMON */
		case 192 + 24:
		{
			disturb(1, 0);
			sound(MSG_SUM_DEMON);
			if (blind) msg_format("%^s mumbles.", m_name);
			{
				if (!(blind)) msg_format
					("%^s magically summons a hellish adversary!", m_name);
				for (k = 0; k < 1; k++)
				{
					count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_DEMON, 0L);
				}
				/* No suitable matches, don't try this spell again */
				if (!count) dungeon_summon_mask_f7 |= (RF7_S_DEMON);

				if (blind && count) msg_print("You smell fire and brimstone.");
			}
			break;
		}

		/* RF7_S_HI_DEMON */
		case 192 + 25:
		{
			disturb(1, 0);
			sound(MSG_SUM_HI_DEMON);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater demons!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_HI_DEMON, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_HI_DEMON);

			if (blind && count)
			{
				msg_print("You hear many evil things appear nearby.");
			}
			break;
		}

		/* RF7_XXX */
		case 192 + 26:	break;

		/* Summon Uniques */
		/* RF7_S_UNIQUE */
		case 192 + 27:
		{
			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if (blind) msg_format("%^s mumbles.", m_name);

			for (k = 0; k < 3
			; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_UNIQUE, 0L);
			}
			if (count)
			{
				if (blind) msg_print("You've got a bad feeling about this...");
				else       msg_format("%^s magically summons mighty opponents!", m_name);
			}
			else
			{
				/* No suitable matches, don't try this spell again */
				dungeon_summon_mask_f7 |= (RF7_S_UNIQUE);

				if (!blind)
					msg_format("%^s gestures imperiously ... and looks puzzled for a moment.", m_name);
			}
			break;
		}

		/* Summon Uniques */
		/* RF7_S_HI_UNIQUE */
		case 192 + 28:
		{
			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if (blind) msg_format("%^s mumbles.", m_name);

			for (k = 0; k < 3
			; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_HI_UNIQUE, 0L);
			}
			if (count)
			{
				if (blind) msg_print("You've got a bad feeling about this...");
				else       msg_format("%^s magically summons legendary opponents!", m_name);
			}
			else
			{
				/* No suitable matches, don't try this spell again */
				dungeon_summon_mask_f7 |= (RF7_S_HI_UNIQUE);

				if (!blind)
					msg_format("%^s gestures imperiously ... and looks puzzled for a moment.",
						m_name);
			}
			break;
		}

		/* RF7_S_UNDEAD */
		case 192 + 29:
		{
			disturb(1, 0);
			sound(MSG_SUM_UNDEAD);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an undead adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_UNDEAD, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_UNDEAD);

			if (blind && count) msg_print("You hear something creepy appear nearby.");
			break;
		}

		/* RF7_S_HI_UNDEAD */
		case 192 + 30:
		{
			disturb(1, 0);
			sound(MSG_SUM_HI_UNDEAD);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater undead!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_HI_UNDEAD, 0L);
			}

			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_HI_UNDEAD);

			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}


		/* Summon the Ringwraiths */
		/* RF7_S_WRAITH */
		case 192 + 31:
		{
			int old_count;
			disturb(1, 0);
			sound(MSG_SUM_WRAITH);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons mighty undead opponents!", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_WRAITH, 0L);
			}
			/* No suitable matches, don't try this spell again */
			if (!count) dungeon_summon_mask_f7 |= (RF7_S_WRAITH);
			old_count = count;
			for (k = 0; (k < 6) && (count < 6); k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
							 summon_lev, SUMMON_HI_UNDEAD, 0L);
			}
			if (count == old_count)
			{
				/* No suitable matches, don't try this spell again */
				dungeon_summon_mask_f7 |= (RF7_S_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}



		/* Paranoia */
		default:
		{
			msg_print("A monster tried to cast a spell that has not yet been defined.");
		}
	}

	/* Hack - Forget the summoner */
	summoner = NULL;

	/* Learn Player Resists */
	if (attack < 128)
	{
		  update_smart_learn(m_idx, spell_desire_RF4[attack-96][D_RES]);
	}
	else if (attack < 160)
	{
		  update_smart_learn(m_idx, spell_desire_RF5[attack-128][D_RES]);
	}
	else if (attack < 192)
	{
		  update_smart_learn(m_idx, spell_desire_RF6[attack-160][D_RES]);
	}
	else if (attack < 224)
	{
		  update_smart_learn(m_idx, spell_desire_RF7[attack-192][D_RES]);
	}

	/* Mark minimum desired range for recalculation */
	m_ptr->min_range = 0;

	/* Remember what the monster did to us */
	if (seen)
	{
		/* Innate spell */
		if (attack < 32*4)
		{
		l_ptr->r_l_flags4 |= (1L << (attack - 32*3));
		if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Bolt or Ball */
		else if (attack < 32*5)
		{
		l_ptr->r_l_flags5 |= (1L << (attack - 32*4));
		if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Special spell */
		else if (attack < 32*6)
		{
		l_ptr->r_l_flags6 |= (1L << (attack - 32*5));
		if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Summon spell */
		else if (attack < 32*7)
		{
		l_ptr->r_l_flags7 |= (1L << (attack - 32*6));
		if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

	}

	if (seen && p_ptr->wizard)
		msg_format("%^s has %i mana remaining.", m_name, m_ptr->mana);

	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}

	/* A spell was cast */
 	return (TRUE);
}

/* Helpers for the cloud_surround function */

#define MAX_CLOUD_CHOICES 22

struct breath_gf
{
	u32b flag;
	int gf_value;
};

/*
 * Events triggered by the various flags.
 */
static const struct breath_gf breath_to_gf[] =
{
	{ RF4_BRTH_ACID,  GF_ACID },
	{ RF4_BRTH_ELEC,  GF_ELEC },
	{ RF4_BRTH_FIRE,  GF_FIRE },
	{ RF4_BRTH_COLD,  GF_COLD },
	{ RF4_BRTH_POIS,  GF_POIS },
	{ RF4_BRTH_LIGHT, GF_LIGHT },
	{ RF4_BRTH_DARK,  GF_DARK },
	{ RF4_BRTH_CONFU, GF_CONFUSION },
	{ RF4_BRTH_SHARD, GF_SHARD },
	{ RF4_BRTH_INER,  GF_INERTIA },
	{ RF4_BRTH_GRAV,  GF_GRAVITY },
	{ RF4_BRTH_NEXUS, GF_NEXUS },
	{ RF4_BRTH_NETHR, GF_NETHER },
	{ RF4_BRTH_CHAOS, GF_CHAOS },
	{ RF4_BRTH_DISEN, GF_DISENCHANT },
	{ RF4_BRTH_TIME,  GF_TIME },
};

/*
 * Some monsters are surrounded by gas, terrible heat, loud noises, spores,
 * etc.  Process any such affects.
 *
 * The Nazgul surround themselves with darkness, so they all have IS_LIT.
 */
void cloud_surround(int r_idx, int *typ, int *dam, int *rad)
{
	u32b i;
	monster_race *r_ptr = &r_info[r_idx];
	int choices[MAX_CLOUD_CHOICES];
	int count = 0;

	*typ = 0;
	*dam = div_round(r_ptr->level, 4);
	*rad = 1 + r_ptr->level / 30;

	/* Unique monsters have especially strong effects */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		*rad += 1;
		*dam *= 2;
	}

	/*** Determine the kind of cloud we're supposed to be giving off ***/

	/* If breaths and attrs match, the choice is clear. */
	if (r_ptr->flags4 & RF4_BRTH_ALL)
	{
		/* For each listed flag, add it to the possible effects */
		for (i = 0; i < N_ELEMENTS(breath_to_gf); i++)
		{
			const struct breath_gf *brth = &breath_to_gf[i];

			/* If they have the breath, add it to the list */
			if (r_ptr->flags4 & brth->flag)
			{
				choices[count++] = brth->flag;
			}

		}
	}

	/* Molds release spores */
	if (r_ptr->d_char == 'm')
	{
		choices[count++] = GF_SPORE;
	}

	/* The Nazgul darken everything nearby, and possibly blind the player  */
	if (r_ptr->d_char == 'W')
	{
		choices[count++] = GF_DARK;
	}
	/* So do silver jellies*/
	if (r_ptr->d_char == 'j')
	{
		choices[count++] = GF_DARK_WEAK;
	}

	if (count)
	{
		i = randint0(count);
		*typ = choices[i];
	}
}


