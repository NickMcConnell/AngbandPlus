/*
 * File: melee1.c
 * Purpose: Monster attacking code
 *
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

#include "reposband.h"
#include "attack.h"
#include "cave.h"
#include "monster/monster.h"
#include "object/tvalsval.h"
#include "spells.h"

/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, int dam)
{
	int max = 0;
	int total = dice * sides;

	/* Must do at least 95% of perfect */
	if (dam < total * 19 / 20) return (0);

	/* Weak blows rarely work */
	if ((dam < 20) && (randint0(100) >= dam)) return (0);

	/* Perfect damage */
	if (dam == total) max++;

	/* Super-charge */
	if (dam >= 20)
	{
		while (randint0(100) < 2) max++;
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
bool check_hit(int power, int level, int m_idx)
{
	int chance, ac;

	monster_type *m_ptr = &mon_list[m_idx];

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	chance = (power + (m_ptr->stunned ? level * 2 : level * 3));

	/* Total armor */
	ac = p_ptr->state.ac + p_ptr->state.to_a;

	/* if the monster checks vs ac, the player learns ac bonuses */
	/* XXX Eddie should you only learn +ac on miss, -ac on hit?  who knows */
	object_notice_on_defend();

	/* Check if the player was hit */
	return test_hit(chance, ac, TRUE);
}

/*
 * Determine if a monster attack against the monster succeeds.
 */
bool check_hit_monster(int power, int level, int m_idx, int t_idx)
{
	int ac, chance;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_type *t_ptr = &mon_list[t_idx];

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	chance = (power + (m_ptr->stunned ? level * 2 : level * 3));

	/* Total armor */
	ac = r_info[t_ptr->r_idx].ac;

	/* Check if the target was hit */
	return test_hit(chance, ac, TRUE);
}


#define MAX_DESC_INSULT 8


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


#define MAX_DESC_MOAN 8


/*
 * Hack -- possible "insult" messages
 */
static cptr desc_moan[MAX_DESC_MOAN] =
{
	"wants his mushrooms back.",
	"tells you to get off his land.",
	"looks for his dogs. ",
	"says 'Did you kill my Fang?' ",
	"asks 'Do you want to buy any mushrooms?' ",
	"seems sad about something.",
	"asks if you have seen his dogs.",
	"mumbles something about mushrooms."
};


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int i, k, tmp, ac, rlev;
	int do_cut, do_stun;

	s32b gold;

	object_type *o_ptr;

	char o_name[80];

	char m_name[80];

	char ddesc[80];

	bool blinked;

	int sound_msg;


	/* Not allowed to attack */
	if (rf_has(r_ptr->flags, RF_NEVER_BLOW)) return (FALSE);


	/* Total armor */
	ac = p_ptr->state.ac + p_ptr->state.to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, MDESC_SHOW | MDESC_IND2);

	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;
		bool do_break = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
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

		/* Extract visibility from carrying lite */
		if (rf_has(r_ptr->flags, RF_HAS_LIGHT)) visible = TRUE;

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
			case RBE_EAT_LIGHT: power =  5; break;
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
			case RBE_HALLU:     power = 10; break;
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev, m_idx))
		{
			/* Always disturbing */
			disturb(1, 0);


			/* Hack -- Apply "protection from evil" */
			if (p_ptr->timed[TMD_PROTEVIL] > 0)
			{
				/* Learn about the evil flag */
				if (m_ptr->ml)
				{
					rf_on(l_ptr->flags, RF_EVIL);
				}

				if (rf_has(r_ptr->flags, RF_EVIL) &&
				    p_ptr->lev >= rlev &&
				    randint0(100) + p_ptr->lev > 50)
				{
					/* Message */
					msg_format("%^s is repelled.", m_name);

					/* Hack -- Next attack */
					continue;
				}
			}


			/* Assume no cut or stun */
			do_cut = do_stun = 0;

			/* Assume no sound */
			sound_msg = MSG_GENERIC;
			/* Roll out the damage */
			
			if (d_dice > 0 && d_side > 0)
				damage = damroll(d_dice, d_side);
			else
				damage = 0;
			
			/* Describe the attack method */
			switch (method)
			{
				case RBM_HIT:
				{
					/* Handle special effect types */
					if (effect == RBE_WOUND)
					{
						if      (damage >= 30) act = "gouges you.";
						else if (damage >= 20) act = "slashes you.";
						else if (damage >= 5)  act = "cuts you.";
						else                act = "scratches you.";
					}
					else if (effect == RBE_BATTER)
					{
						if      (damage >= 30) act = "bludgeons you.";
						else if (damage >= 20) act = "batters you.";
						else if (damage >= 5)  act = "bashes you.";
						else                act = "hits you.";
					}
					else
					{
						act = "hits you.";
					}
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
					if      (damage >= 25) act = "slashes you.";
					else if (damage >=  5) act = "claws you.";
					else                act = "scratches you.";
					do_cut = 1;
					sound_msg = MSG_MON_CLAW;
					break;
				}

				case RBM_BITE:
				{
					if (damage >= 5) act = "bites you.";
					else          act = "nips you.";
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
					if (damage >= rand_range(10, 20)) act = "tramples you.";
					else                           act = "butts you.";
					do_stun = 1;
					sound_msg = MSG_MON_BUTT;
					break;
				}

				case RBM_CRUSH:
				{
					if (damage >= 10) act = "crushes you.";
					else           act = "squeezes you.";
					do_stun = 1;
					sound_msg = MSG_MON_CRUSH;
					break;
				}

				case RBM_ENGULF:
				{
					if (damage >= randint0(50)) act = "envelops you.";
					else                    act = "engulfs you.";
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
				{
					if      (damage >= rand_range(20, 30))
							act = "glares at you terribly.";
					else if (damage >= rand_range(5, 30))
							act = "gazes upon you.";
					else 	act = "gazes at you.";
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
					act = desc_insult[randint0(MAX_DESC_INSULT)];
					sound_msg = MSG_MON_INSULT;
					break;
				}

				case RBM_MOAN:
				{
					act = desc_moan[randint0(MAX_DESC_MOAN)];
					sound_msg = MSG_MON_MOAN;
					break;
				}
			}

			/* Message */
			if (act) message_format(sound_msg, 0, "%^s %s", m_name, act);


			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply appropriate damage */
			switch (effect)
			{
				case 0:
				{
					/* Hack -- Assume obvious */
					obvious = TRUE;

					/* Hack -- No damage */
					damage = 0;

					break;
				}

				case RBE_HURT:
				{
					/* Obvious */
					obvious = TRUE;

					/* Hack -- Player armor reduces total damage */
					damage -= (damage * ((ac < 240) ? ac : 240) / 400);

					/* Take damage */
					take_hit(damage, ddesc);

					break;
				}

				case RBE_POISON:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Take "poison" effect */
					if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS]))
					{
						if (inc_timed(TMD_POISONED, randint1(rlev) + 5, TRUE))
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
					if (!p_ptr->state.resist_disen)
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

					/* Take damage */
					take_hit(damage, ddesc);

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						i = randint0(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &p_ptr->inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Drain charged wands/staves */
						if ((o_ptr->tval == TV_STAFF) ||
						    (o_ptr->tval == TV_WAND))
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
							heal = MIN(heal, m_ptr->maxhp - m_ptr->hp);

							/* Heal */
							m_ptr->hp += heal;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx)
								p_ptr->redraw |= (PR_HEALTH);

							/* Combine / Reorder the pack */
							p_ptr->notice |= (PN_COMBINE | PN_REORDER);

							/* Redraw stuff */
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
					    (randint0(100) < (adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] +
					                      p_ptr->lev)))
					{
						/* Saving throw message */
						msg_print("You quickly protect your money pouch!");

						/* Occasional blink anyway */
						if (randint0(3)) blinked = TRUE;
					}

					/* Eat gold */
					else
					{
						gold = (p_ptr->au / 10) + randint1(25);
						if (gold < 2) gold = 2;
						if (gold > 5000) gold = (p_ptr->au / 20) + randint1(3000);
						if (gold > p_ptr->au) gold = p_ptr->au;
						p_ptr->au -= gold;
						if (gold <= 0)
						{
							msg_print("Nothing was stolen.");
							break;
						}

						/* Let the player know they were robbed */
						msg_print("Your purse feels lighter.");
						if (p_ptr->au)
							msg_format("%ld coins were stolen!", (long)gold);
						else
							msg_print("All of your coins were stolen!");

						/* While we have gold, put it in objects */
						while (gold > 0)
						{
							int amt;

							/* Create a new temporary object */
							object_type o;
							object_wipe(&o);
							object_prep(&o, objkind_get(TV_GOLD, SV_GOLD), 0, MINIMISE);

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

				case RBE_EAT_ITEM:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->timed[TMD_PARALYZED] &&
					    (randint0(100) < (adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] +
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

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Pick an item */
						i = randint0(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &p_ptr->inventory[i];

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

						/* Modify number */
						i_ptr->number = 1;

						/* Hack -- If a rod, staff, or wand, allocate total
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

				case RBE_EAT_FOOD:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Steal some food */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item from the pack */
						i = randint0(INVEN_PACK);

						/* Get the item */
						o_ptr = &p_ptr->inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip non-food objects */
						if (o_ptr->tval != TV_FOOD) continue;

						/* Get a description */
						object_desc(o_name, sizeof(o_name), o_ptr,
									ODESC_PREFIX | ODESC_BASE);

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

				case RBE_EAT_LIGHT:
				{
					bitflag f[OF_SIZE];

					/* Take damage */
					take_hit(damage, ddesc);

					/* Get the light, and its flags */
					o_ptr = &p_ptr->inventory[INVEN_LIGHT];
					object_flags(o_ptr, f);

					/* Drain fuel where applicable */
					if (!of_has(f, OF_NO_FUEL) && (o_ptr->timeout > 0))
					{
						/* Reduce fuel */
						o_ptr->timeout -= (250 + randint1(250));
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
					if (!p_ptr->state.resist_blind)
					{
						if (inc_timed(TMD_BLIND, 10 + randint1(rlev), TRUE))
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
					if (!p_ptr->state.resist_confu)
					{
						if (inc_timed(TMD_CONFUSED, 3 + randint1(rlev), TRUE))
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
					if (p_ptr->state.resist_fear)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else if (randint0(100) < p_ptr->state.skills[SKILL_SAVE])
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_AFRAID, 3 + randint1(rlev), TRUE))
							obvious = TRUE;
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
					if (p_ptr->state.free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (randint0(100) < p_ptr->state.skills[SKILL_SAVE])
					{
						msg_print("You resist the effects!");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_PARALYZED, 3 + randint1(rlev), TRUE))
							obvious = TRUE;
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
					if (do_dec_stat(A_STR, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_INT:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_INT, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_WIS:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_WIS, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_DEX:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_DEX, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CON:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_CON, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CHR:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_CHR, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_ALL:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stats) */
					if (do_dec_stat(A_STR, FALSE)) obvious = TRUE;
					if (do_dec_stat(A_DEX, FALSE)) obvious = TRUE;
					if (do_dec_stat(A_CON, FALSE)) obvious = TRUE;
					if (do_dec_stat(A_INT, FALSE)) obvious = TRUE;
					if (do_dec_stat(A_WIS, FALSE)) obvious = TRUE;
					if (do_dec_stat(A_CHR, FALSE)) obvious = TRUE;

					break;
				}

				case RBE_SHATTER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Hack -- Reduce damage based on the player armor class */
					damage -= (damage * ((ac < 240) ? ac : 240) / 400);

					/* Take damage */
					take_hit(damage, ddesc);

					/* Radius 8 earthquake centered at the monster */
					if (damage > 23)
					{
						int px_old = p_ptr->px;
						int py_old = p_ptr->py;

						earthquake(m_ptr->fy, m_ptr->fx, 8);

						/* Stop the blows if the player is pushed away */
						if ((px_old != p_ptr->px) ||
						    (py_old != p_ptr->py))
						    do_break = TRUE;
					}
					break;
				}

				case RBE_EXP_10:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					/* XXX Eddie need a DRS for HOLD_LIFE */
					wieldeds_notice_flag(OF_HOLD_LIFE);

					if (p_ptr->state.hold_life && (randint0(100) < 95))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
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

				case RBE_EXP_20:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					wieldeds_notice_flag(OF_HOLD_LIFE);

					if (p_ptr->state.hold_life && (randint0(100) < 90))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->state.hold_life)
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

					wieldeds_notice_flag(OF_HOLD_LIFE);

					if (p_ptr->state.hold_life && (randint0(100) < 75))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->state.hold_life)
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

					wieldeds_notice_flag(OF_HOLD_LIFE);

					if (p_ptr->state.hold_life && (randint0(100) < 50))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->state.hold_life)
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
					if (!p_ptr->state.resist_chaos)
					{
						if (inc_timed(TMD_IMAGE, 3 + randint1(rlev / 2), TRUE))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_CHAOS);

					break;
				}
			}


			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (randint0(100) < 50)
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
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint1(5); break;
					case 2: k = randint1(5) + 5; break;
					case 3: k = randint1(20) + 20; break;
					case 4: k = randint1(50) + 50; break;
					case 5: k = randint1(100) + 100; break;
					case 6: k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if (k) (void)inc_timed(TMD_CUT, k, TRUE);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint1(5); break;
					case 2: k = randint1(10) + 10; break;
					case 3: k = randint1(20) + 20; break;
					case 4: k = randint1(30) + 30; break;
					case 5: k = randint1(40) + 40; break;
					case 6: k = 100; break;
					default: k = 200; break;
				}

				/* Apply the stun */
				if (k) (void)inc_timed(TMD_STUN, k, TRUE);
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
				if (l_ptr->blows[ap_cnt] < MAX_UCHAR)
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


	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}


	/* Assume we attacked */
	return (TRUE);
}

/*
 * Attack a monster via physical attacks.
 */
bool make_attack_normal_mon(monster_type *m_ptr, monster_type *t_ptr)
{
	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];
	int t_idx = cave_m_idx[t_ptr->fy][t_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_race *tr_ptr = &r_info[t_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	monster_lore *lt_ptr = &l_list[t_ptr->r_idx];

	int ap_cnt;

	int k, tmp, ac, rlev;
	int do_cut, do_stun;
	int blows;

	bool alive = TRUE;

	char m_name[80];
	char t_name[80];

	char ddesc[80];

	bool blinked;


	/* Not allowed to attack */
	if (RF_NEVER_BLOW) return (FALSE);
	
	/* Attacker is in wall doesn't attack (because others cannot attack him) */
	if (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_WALL)) return (FALSE);

	/* Hostile will attack each other only in chaotic-vs-lawful fights */
	/* TODO: impliment this -Simon */
	/* if ((m_ptr->align & (AL_HOSTILE_MASK)) && (t_ptr->align & (AL_HOSTILE_MASK)))
	{
		if (m_ptr->align == AL_HOSTILE || t_ptr->align == AL_HOSTILE ||
			m_ptr->align == t_ptr->align) return (FALSE);
	}*/
	
	/* Pets will never attack pets */
	//if ((m_ptr->align & (AL_PET_MASK)) && (t_ptr->align & (AL_PET_MASK))) return (FALSE);
	
	/* Hack -- skip uniques. Probably unrealistic, but needed for the balance */
	/* Hack N2 -- allow uniques if player is a Quylthulg :-) */
	/* Hack N3 -- allow Valar to kill uniques */
	/* Hack N4 -- allow everyone to kill pet uniques */
	/* TODO: impliment this -Simon */
	/*if (tr_ptr->flags & (RF_UNIQUE) && !(r_ptr->flags & (RF_VALA)) &&
		!(tr_ptr->flags & (RF_VALA)) &&
		!(t_ptr->align & (AL_PET_MASK)) &&
		r_info[p_ptr->m_r_idx].d_char != 'Q') return (FALSE);*/

	/* Total armor */
	ac = tr_ptr->ac;

	/* Extract the effective monster level */
	rlev = MAX(r_ptr->level, 1);

	if ((m_ptr->mimic_k_idx) && (m_ptr->ml))
	{
		/*no longer a mimic*/
		m_ptr->mimic_k_idx = 0;

		/* Character notices monster */
		m_ptr->mflag &= ~(MFLAG_MIMIC);

		/* Message  XXX */
		msg_print("There is a mimic!");

		/* Redraw */
		/* TODO: figure this one out, make it work -Simon */
		//lite_spot(m_ptr->fy, m_ptr->fx);

	}

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);
	monster_desc(t_name, sizeof(t_name), t_ptr, 0);

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

        /* Extract visibility from carrying lite */
        if (r_ptr->flags[RF_HAS_LIGHT]) visible = TRUE;

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
			case RBE_EAT_LIGHT:  power =  5;  break;
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

		/* Describe the attack method, apply special hit chance mods. */
		switch (method)
		{
			case RBM_HIT:
			{
				/* Handle special effect types */
				if (effect == RBE_WOUND)
				{
					if      (dam >= 30) act = "gouges %s";
					else if (dam >= 20) act = "slashes %s";
					else if (dam >= 5)  act = "cuts %s";
					else                act = "scratches %s";
				}
				else if (effect == RBE_BATTER)
				{
					if      (dam >= 30) act = "bludgeons %s";
					else if (dam >= 20) act = "batters %s";
					else if (dam >= 5)  act = "bashes %s";
					else                act = "hits %s";
				}
				else
				{
					act = "hits %s";
				}
				do_cut = do_stun = 1;
				break;
			}
			case RBM_TOUCH:
			{
				act = "touches %s";
				break;
			}

			case RBM_PUNCH:
			{
				act = "punches %s";
				do_stun = 1;
				break;
			}

			case RBM_KICK:
			{
				act = "kicks %s";
				do_stun = 1;
				break;
			}
			case RBM_CLAW:
			{
				if      (dam >= 25) act = "slashes %s";
				else if (dam >=  5) act = "claws %s";
				else                act = "scratches %s";
				do_cut = 1;
				break;
			}
			case RBM_BITE:
			{
				if (dam >= 5) act = "bites %s";
				else          act = "nips %s";
				do_cut = 1;
				break;
			}
			case RBM_PECK:
			{
				act = "pecks %s";
				do_cut = 1;
				break;
			}
			case RBM_STING:
			{
				act = "stings %s";
				break;
			}
			case RBM_BUTT:
			{
				if (dam >= rand_range(10, 20)) act = "tramples %s";
				else                           act = "butts %s";
				do_stun = 1;
				break;
			}
			case RBM_CRUSH:
			{
				if (dam >= 10) act = "crushes %s";
				else           act = "squeezes %s";
				do_stun = 1;
				break;
			}
			case RBM_ENGULF:
			{
				if (dam >= randint0(50)) act = "envelops %s";
				else                    act = "engulfs %s";
				break;
			}
			case RBM_CRAWL:
			{
				act = "crawls on %s";
				break;
			}
			case RBM_DROOL:
			{
				act = "drools on %s";
				break;
			}
			case RBM_SPIT:
			{
				act = "spits on %s";
				break;
			}
			case RBM_SLIME:
			{
				act = "%^s has been slimed!";
				break;
			}
			case RBM_GAZE:
			{
				if      (dam >= rand_range(20, 30))
					act = "glares at %s terribly";
				else if (dam >= rand_range(5, 30))
					act = "gazes upon %s";
				else act = "gazes at %s";
				break;
			}
			case RBM_WAIL:
			{
				act = "makes a horrible wail";
				break;
			}
			case RBM_SPORE:
			{
				act = "releases a cloud of spores";
				break;
			}
			case RBM_BEG:
			{
				act = "begs %s for money";
				break;
			}
			case RBM_INSULT:
			{
				act = "insults %s";
				break;
			}
		}

		/* No effect */
		if (no_effect) continue;

		/* Monster hits monster */
		if (!effect || check_hit_monster(power, rlev, m_idx, t_idx))
		{
			/* ToDo: disturb if pets move? */
			
			/* Wake up Neo! */
			t_ptr->csleep = 0;
			t_ptr->mflag |= (MFLAG_ACTV);

			/* Message - special handling for sliming attacks */
			if (act && (m_ptr->ml || t_ptr->ml))
			{
				char msgtmp[80];
				strnfmt(msgtmp, sizeof(msgtmp), act, t_name);
				
				if (method == RBM_SLIME) strcpy(msg, format("%s", msgtmp));
				else
				{
					strcpy(msg, format("%^s %s.", m_name, msgtmp));
				}

				/* Message -- sometimes incorrect  XXX XXX */
				if (act) msg_format("%s", msg);
			}

			/* Hack -- occasional message if neither is seen */
			/* TODO: impliment this -Simon */
			/*if (!(m_ptr->ml || t_ptr->ml) && dam && !mon_mon_nonlos_combat_msg)
			{
				//mon_mon_nonlos_combat_msg = TRUE;
				msg_print("You hear sounds of combat.");
			}*/

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply appropriate damage */
			switch (effect)
			{

				/* No effect */
				case 0:
				{
					/* Message */
					if (act && (m_ptr->ml || t_ptr->ml)) msg_format("%^s %s.", m_name, format(act, t_name));

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

					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

				/* Hit with increased chance to wound */
				case RBE_WOUND:
 				{
					/* Obvious */
					obvious = TRUE;

					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
 					/* Take damage */
					t_ptr->hp -= dam;

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

					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);

					/* Take damage */
					t_ptr->hp -= dam;

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
					if (dam > randint0(60))
						earthquake(m_ptr->fy, m_ptr->fx, 6);

					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

 					break;
 				}

				/* Hit to disenchant */
 				case RBE_UN_BONUS:
 				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Resistance */
					if (tr_ptr->flags[RF_RES_DISE] || r_ptr->flags[RF_BRTH_DISEN])
					{
						dam *= 3; dam /= (randint0(6)+6);
						if (t_ptr->ml)
						{
							lt_ptr->flags[RF_RES_DISE] = TRUE;
							lt_ptr->flags[RF_BRTH_DISEN] = TRUE;
						}
					}

					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

				/* Hit to reduce charges of magical items */
				/* Assume this is disenchantment */
				case RBE_UN_POWER:
				{
					/* Resistance */
					if (tr_ptr->flags[RF_RES_DISE] || r_ptr->flags[RF_BRTH_DISEN])
					{
						dam *= 3; dam /= (randint0(6)+6);
						if (t_ptr->ml)
						{
							lt_ptr->flags[RF_RES_DISE] = TRUE;
							lt_ptr->flags[RF_BRTH_DISEN] = TRUE;
						}
					}

					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

				/* Hit to reduce mana */
				case RBE_LOSE_MANA:
				{
					int drain;

					/* Obvious */
					obvious = TRUE;

					/* Damage (mana) */
					if (t_ptr->mana)
					{
						/* Drain depends on maximum mana */
						drain = 2 + randint0(t_ptr->maxmana / 10);

						/* Drain the mana */
						if (drain > t_ptr->mana)
						{
							t_ptr->mana = 0;
						}
						else
						{
							t_ptr->mana -= drain;
						}

						/* Redraw mana */
						p_ptr->redraw |= (PR_MON_MANA);
					}

					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Damage (physical) */
					t_ptr->hp -= dam;

					break;
				}

				/* Hit to steal gold */
				/* XXX Not implemented */
				case RBE_EAT_GOLD:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					/* (Usually) blink */
					if (randint0(3))
					{
						if (m_ptr->ml)
						{
							msg_format("%^s seems to have stolen something from %s!", m_name, t_name);
						}
						blinked = TRUE;
					}
					
					break;
				}

				/* Hit to steal objects from the pack */
				/* XXX Not implemented */
				case RBE_EAT_ITEM:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					/* (Usually) blink */
					if (randint0(3))
					{
						if (m_ptr->ml)
						{
							msg_format("%^s seems to have stolen something from %s!", m_name, t_name);
						}
						blinked = TRUE;
					}
					
					break;
				}

				/* Hit to eat food */
				/* XXX Not implemented (meaningless) */
				case RBE_EAT_FOOD:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					break;
				}

				/* Hit to reduce nutrition */
				/* XXX Not implemented (meaningless) */
				case RBE_HUNGER:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					break;
				}

				/* Hit to drain light */
				/* XXX Not implemented (how?) */
				case RBE_EAT_LIGHT:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					break;
				}


				/* Hit to poison */
				case RBE_POISON:
				{
					if (tr_ptr->flags[RF_IM_POIS])
					{
						dam /= 9;
						if (t_ptr->ml) lt_ptr->flags[RF_IM_POIS] = TRUE;
					}
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					break;
				}

				/* Hit to inflict acid damage */
				case RBE_ACID:
				{
					if (tr_ptr->flags[RF_IM_ACID])
					{
						dam /= 9;
						if (t_ptr->ml) lt_ptr->flags[RF_IM_ACID] = TRUE;
					}
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					break;
				}

				/* Hit to electrocute */
				case RBE_ELEC:
				{
					if (tr_ptr->flags[RF_IM_ELEC])
					{
						dam /= 9;
						if (t_ptr->ml) lt_ptr->flags[RF_IM_ELEC] = TRUE;
					}
					
					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

				/* Hit to burn */
				case RBE_FIRE:
				{
					if (tr_ptr->flags[RF_IM_FIRE])
					{
						dam /= 9;
						if (t_ptr->ml) lt_ptr->flags[RF_IM_FIRE] = TRUE;
					}
					
					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

				case RBE_COLD:
				{
					if (tr_ptr->flags[RF_IM_COLD])
					{
						dam /= 9;
						if (t_ptr->ml) lt_ptr->flags[RF_IM_COLD] = TRUE;
					}
					
					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

				case RBE_BLIND:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					/* Increase "blindness" */
					if (!(((tr_ptr->flags[RF_UNIQUE]) && (one_in_(2))) ||
		                            (tr_ptr->flags[RF_NO_CONF]) ||
                                            ((tr_ptr->level + randint0(MAX(4, tr_ptr->level / 2))) >
                                            (randint0(dam)))))
					{
						if (t_ptr->blinded)
						{
							t_ptr->blinded += damroll(3, (dam / 4)) + 1;
						}
						else
						{
							t_ptr->blinded += damroll(3, (dam / 2)) + 1;
						}
					}
					
					if (t_ptr->ml) lt_ptr->flags[RF_NO_CONF] = TRUE;
					
					break;
				}

				/* Hit to confuse */
				case RBE_CONFUSE:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					/* Increase "confused" */
					if (!(((tr_ptr->flags[RF_UNIQUE]) && (one_in_(2))) ||
		                            (tr_ptr->flags[RF_NO_CONF]) ||
                                            ((tr_ptr->level + randint0(MAX(4, tr_ptr->level / 2))) >
                                            (randint0(dam)))))
					{
						if (t_ptr->confused)
						{
							t_ptr->confused += damroll(3, (dam / 4)) + 1;
						}
						else
						{
							t_ptr->confused += damroll(3, (dam / 2)) + 1;
						}
					}

					if (t_ptr->ml) lt_ptr->flags[RF_NO_CONF] = TRUE;

					break;
				}

				/* Hit to frighten */
				case RBE_TERRIFY:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					/* Attempt a saving throw */
                    if (!((tr_ptr->flags[RF_UNIQUE]) && (one_in_(2))) || (tr_ptr->flags[RF_NO_FEAR]) || 
						((tr_ptr->level + randint0(MAX(4, tr_ptr->level / 2))) > (randint0(dam))))
                    {
						/* TODO: learn how to make monsters afraid, then do it here -Simon */
						// int tmp = t_ptr->monfear + damroll(3, (dam / 2)) + 1;
						//set_mon_fear(t_ptr, tmp, TRUE);
						t_ptr->mflag &= ~(MFLAG_WARY);
					}
					
					if (t_ptr->ml) lt_ptr->flags[RF_NO_FEAR] = TRUE;

					break;
				}

				/* Hit to paralyze - sleep is bad emulation */
				case RBE_PARALYZE:
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					/* Set sleep */
					if (!(((tr_ptr->flags[RF_UNIQUE]) && (one_in_(2))) || (tr_ptr->flags[RF_NO_SLEEP]) || 
					((tr_ptr->level + randint0(MAX(4, tr_ptr->level / 2))) > (randint0(dam)))))
					{
						t_ptr->csleep = 500;
					}

					if (t_ptr->ml) lt_ptr->flags[RF_NO_SLEEP] = TRUE;

				}

				/* Hit to cause disease - emulate with poison */
				case RBE_DISEASE:
 				{
					if (tr_ptr->flags[RF_IM_POIS])
					{
						dam /= 9;
						if (t_ptr->ml) lt_ptr->flags[RF_IM_POIS] = TRUE;
					}
					
					/* Take damage */
					t_ptr->hp -= dam;
					
					break;
 				}

				case RBE_LOSE_STR:
				case RBE_LOSE_INT:
				case RBE_LOSE_WIS:
				case RBE_LOSE_DEX:
				case RBE_LOSE_CON:
				case RBE_LOSE_CHR:
				case RBE_LOSE_ALL:
				case RBE_EXP_10:
				case RBE_EXP_20:
				case RBE_EXP_40:
				case RBE_EXP_80:
				case RBE_HALLU:
				/* Assume no extra effects */
				{
					/* Monster armor reduces total damage */
					dam -= (dam * ((ac < 240) ? ac : 240) / 400);
					
					/* Take damage */
					t_ptr->hp -= dam;

					break;
				}

			}
			
			/* Hack - uniques cannot be killed by monsters */
			/* Hack N2 -- they can, if a player is Q */
			/* Hack N3 -- They can, if killed by a Vala */
			/* Hack N4 -- They can, if they are player's pets */
			/* TODO: this block of code -Simon */
			/* if (tr_ptr->flags[RF_UNIQUE] && !(r_ptr->flags[RF_VALA]) &&
				!(t_ptr->align[AL_PET_MASK]) && t_ptr->hp < 0)
			{
				if (r_info[p_info[p_ptr->prace].p_monster_index].d_char != 'Q')
					t_ptr->hp = 0;
				else
					t_ptr->hp = -1;
			}*/

			/* Handle monster death */
			if (t_ptr->hp < 0)
			{
				/* Message */
				if (m_ptr->ml || t_ptr->ml)
				{
					/* TODO: implement this -Simon */
					/*if (monster_nonliving(tr_ptr))
					{
						message_format(MSG_KILL, t_ptr->r_idx, "%^s has destroyed %s.", m_name, t_name);
					}
					else
					{*/
						message_format(MSG_KILL, t_ptr->r_idx, "%^s has slain %s.", m_name, t_name);
					//}
				}
				else
				{
					msg_print("You hear a scream of agony!");
				}
				
				/* Death */
				monster_death(t_idx);
				
				/* Remove monster */
				delete_monster_idx(t_idx);
				
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
				/* XXX Do nothing - monsters cannot be cut */
			}

			/* Handle stun XXX */
			if (do_stun)
			{

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, dam);

				/* Roll for damage */
				switch (tmp)
				{
					case 0:  k = 0; break;
					case 1:  k = randint0(5); break;
					case 2:  k = rand_range( 8, 16); break;
					case 3:  k = rand_range(15, 30); break;
					case 4:  k = rand_range(25, 50); break;
					case 5:  k = rand_range(35, 70); break;
					case 6:  k = rand_range(45, 90); break;
					default: k = 100; break;

				}
				
				if (k)
				{
					if (t_ptr->ml)
					{
						if (t_ptr->stunned)
						{
							msg_format("%^s is more dazed.", t_name);
						}
						else
						{
							msg_format("%^s is dazed.", t_name);
						}
					}
					
					/* Apply the stun */
					t_ptr->stunned += k / 2;
				}				
			}
			
			/* Hack -- if something attacked pet, which has no current target,
			 * KILL!
			 */
			//if ((t_ptr->align & (AL_PET_MASK)) && t_ptr->target < 0)
			//{
			//	t_ptr->target = m_idx;
			//}
		}

		/* Monster missed monster */
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
				case RBM_BUTT:
				case RBM_CRUSH:


				/* Visible monsters */
				if (m_ptr->ml || t_ptr->ml)
				{
					/* Message */
					msg_format("%^s misses %s.", m_name, t_name);
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
	}

	/* Blink away */
	if ((blinked) && (alive) && (m_ptr->ml))
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	/* Assume we attacked */
	return (TRUE);
}