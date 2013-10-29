/* File: melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



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
	int chance, ac;

	/* Calculate the "attack quality" */
	chance = (power + (level * 3));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Check if the player was hit */
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
	"tries to stick its finger up your nose!",
	"defiles you!",
	"dances around you!",
	"makes obscene gestures!",
	"moons you!!!"
};


#define MAX_DESC_MOAN 8


/*
 * Hack -- possible "moaning" messages
 */
static cptr desc_moan[MAX_DESC_MOAN] =
{
	"swears loudly 'buggerit!'",
	"mumbles 'millenium hand and shrimp'",
	"looks for his dog. ",
	"mumbles a nonstop flow of swearing",
	"'s stench forms a tangible finger and pokes you in the nose.",
	"reeks to high heaven.",
	"wears a sign that says 'gimmie 100gold and I'll take my stink somewhere else.'",
	"mumbles something about shrimp."
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
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, 0x88);


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



		/* Extract the attack "power" */
		/* what does this do? */
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
			case RBE_HALLU:     power = 10; break;
			case RBE_SILVER:    power = 11; break;
			case RBE_SLIME:     power = 11; break;
			case RBE_CHARM:     power =  2; break;
			case RBE_FRENZY:    power = 11; break;
            case RBE_HUNGER:    power =  2; break;
			case RBE_PIXIEKISS: power =  0; break;
			case RBE_ENTHELP:   power =  0; break;
			case RBE_PURIFY:    power =  0; break;
			case RBE_UNLUCKY:   power =  2; break;
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			/* Always disturbing */
			disturb(1, 0);

			/* Hack -- Apply "protection from lifeless" */
			if ((p_ptr->timed[TMD_PROTDEAD] > 0) &&
			    (r_ptr->flags3 & (RF3_NON_LIVING)) &&
			    (p_ptr->lev >= (rlev-1) + randint(15)) &&
			    ((rand_int(100) + p_ptr->lev) > 40))
			{
				/* Remember the lifelessness */
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_NON_LIVING);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->timed[TMD_PROTEVIL] > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Hack -- Apply strong "protection from evil" */
			if ((p_ptr->timed[TMD_PROTEVIL2] > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(101) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}
				
				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}
			/* can affect monsters higher than the player's level */
			if ((p_ptr->timed[TMD_PROTEVIL2] > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev < rlev) &&
                (p_ptr->lev + randint(p_ptr->lev * 2) > rlev + 6))
			{
                if ((!(r_ptr->flags1 & (RF1_UNIQUE))) ||
                   (rand_int(100) < 5))
                {
                   /* Remember the Evil-ness */
				   if (m_ptr->ml)
				   {
					  l_ptr->flags3 |= (RF3_EVIL);
				   }
				
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

				case RBM_TAIL:
				{
					act = "swings its tail at you.";
					do_stun = 1;
					sound_msg = MSG_MON_HIT;
					break;
				}

				case RBM_BUTT:
				{
					act = "butts you.";
					do_stun = 2;
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

				case RBM_XXX2:
				{
					act = "XXX2's you.";
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
					do_stun = 1;
					sound_msg = MSG_MON_SPIT; 
					break;
				}

				case RBM_KISS: /* XXX3 */
				{
					act = "kisses you.";
					break;
				}

				case RBM_GAZE:
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

				case RBM_XXX4:
				{
					act = "projects XXX4's at you.";
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
					act = desc_insult[rand_int(MAX_DESC_INSULT)];
					sound_msg = MSG_MON_INSULT; 
					break;
				}

				case RBM_MOAN:
				{
					act = desc_moan[rand_int(MAX_DESC_MOAN)];
					sound_msg = MSG_MON_MOAN; 
					break;
				}

				case RBM_XXX5:
				{
					act = "XXX5's you.";
					break;
				}
			}

			/* Message */
			if (act) message_format(sound_msg, 0, "%^s %s", m_name, act);


			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

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
					damage -= (damage * ((ac < 150) ? ac : 150) / 250);

					/* Take damage */
					take_hit(damage, ddesc);

					break;
				}

				case RBE_POISON:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Take "poison" effect */
					if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
					{
						if (inc_timed(TMD_POISONED, randint(rlev) + 5))
						{
                            if (p_ptr->weakresist_pois) (void)dec_timed(TMD_POISONED, randint(6));
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

					/* Take damage */
					take_hit(damage, ddesc);

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

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

							/* Window stuff */
							p_ptr->window |= (PW_INVEN);

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
						if (rand_int(3)) blinked = TRUE;
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
#ifdef EFG
						/* EFGchange stolen gold does not evaporate */
						long maxpval = (u16b) ((s16b) -1);  /* ??? should go elsewhere */
						long maxnum = 99;
						long maxgold = maxnum * maxpval;
						object_type object_type_body;
						object_type *i_ptr = &object_type_body;
						while (gold > 0)
						{
        						object_wipe(i_ptr);
        						make_gold(i_ptr);
							if (gold < maxpval)
								i_ptr->pval = gold;
							else if (gold >= maxgold)
							{
								i_ptr->number = maxnum;
								i_ptr->pval = maxpval;
							}
							else
							{
								/* no big deal to lose a few gold pieces */
								i_ptr->number = gold/maxpval;
								i_ptr->pval = gold/i_ptr->number;
							}
							gold -= i_ptr->number * i_ptr->pval;
							(void)monster_carry(m_idx, i_ptr);
						}
#endif

						/* Redraw gold */
						p_ptr->redraw |= (PR_GOLD);

						/* Window stuff */
						p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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

					/* Find an item */
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
						object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

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
						i = rand_int(INVEN_PACK);

						/* Get the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip non-food objects */
						if (o_ptr->tval != TV_FOOD) continue;

						/* Get a description */
						object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

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
					u32b f1, f2, f3; 

					/* Take damage */
					take_hit(damage, ddesc);

					/* Get the lite, and its flags */
					o_ptr = &inventory[INVEN_LITE];
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Drain fuel where applicable */
					if (!(f3 & TR3_NO_FUEL) && (o_ptr->timeout > 0))
					{
						/* Reduce fuel */
						o_ptr->timeout -= (250 + randint(250));
						if (o_ptr->timeout < 1) o_ptr->timeout = 1;

						/* Notice */
						if (!p_ptr->timed[TMD_BLIND])
						{
							msg_print("Your light dims.");
							obvious = TRUE;
						}

						/* Window stuff */
						p_ptr->window |= (PW_EQUIP);
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
#ifdef ALTDJA
					msg_print("You smell something disgustingly foul!");
#else
					msg_print("You are struck by electricity!");
#endif					

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
						if (inc_timed(TMD_BLIND, 10 + randint(rlev)))
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
						if (inc_timed(TMD_CONFUSED, 3 + randint(rlev)))
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
					if ((p_ptr->resist_fear) || (p_ptr->timed[TMD_FRENZY]))
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->skills[SKILL_SAV])
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_AFRAID, 3 + randint(rlev)))
						{
            			    (void)clear_timed(TMD_CHARM);
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_FEAR);

					break;
				}
				
				case RBE_CHARM:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "charm" */
					if (p_ptr->resist_charm)
					{
						msg_print("You resist its charms.");
						obvious = TRUE;
					}
					else if (rand_int(125) < p_ptr->skills[SKILL_SAV])
					{
						msg_print("You resist its charms.");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_CHARM, 3 + randint(rlev)))
						{
            			    (void)clear_timed(TMD_AFRAID);
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_CHARM);

					break;
				}

				case RBE_PARALYZE:
				{
					/* Hack -- Prevent perma-paralysis via damage */
					if (p_ptr->timed[TMD_PARALYZED] && (damage < 1)) damage = 1;

					/* Take damage */
					take_hit(damage, ddesc);
					
					int savechance = 100 + (badluck/2) - (goodluck/5);
					/* Increase "paralyzed" */
					if (p_ptr->free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(savechance) < p_ptr->skills[SKILL_SAV]) 
					{
						msg_print("You resist the effects!");
						obvious = TRUE;
					}
					else
					{
						if (inc_timed(TMD_PARALYZED, 3 + randint(rlev)))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_FREE);

					break;
				}
				
				case RBE_HUNGER:
                {
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);
					
					/* hunger */
			        if (p_ptr->food > PY_FOOD_WEAK + 240) (void)set_food(PY_FOOD_WEAK);
			        else p_ptr->food = p_ptr->food - (240 + randint(damage * 5));
					msg_print("you feel unsatisfied.");

					break;
                }
				
				case RBE_SILVER:
                {
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);
					
					/* silver poison */
					if (rlev >= 10) p_ptr->silver = p_ptr->silver + randint(rlev / 10);
					if (rlev < 10) p_ptr->silver = p_ptr->silver + 1;
					
					msg_print("you feel silver magic corrupting your mind!");

					break;
                }
                
                case RBE_SLIME:
                {
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					/* slimed */
					if (rlev >= 10) p_ptr->slime = p_ptr->slime + randint(rlev / 9);
					if (rlev < 10) p_ptr->slime = p_ptr->slime + 1;

					msg_print("you are slimed!");

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

					/* Hack -- Reduce damage based on the player armor class */
					damage -= (damage * ((ac < 150) ? ac : 150) / 250);

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

					if (p_ptr->hold_life && (rand_int(100) < 95))
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
						s32b d = damroll(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

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
						s32b d = damroll(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

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
						s32b d = damroll(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

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
					if ((!p_ptr->resist_chaos) && (!p_ptr->timed[TMD_TSIGHT]))
					{
						if (inc_timed(TMD_IMAGE, 3 + randint(rlev / 2)))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_CHAOS);

					break;
				}
				
				case RBE_UNLUCKY:
				{
               		/* Take damage */
					take_hit(damage, ddesc);
					
                    int savechance = 111 + badluck - (goodluck/2);
                    if (r_ptr->flags2 & (RF2_POWERFUL)) savechance += 20;
			        if (rand_int(savechance) < p_ptr->skills[SKILL_SAV])
					{
                       msg_print("You resist the effects.");
                    }
                    else
                    {
                       if (r_ptr->flags2 & (RF2_POWERFUL)) p_ptr->luck -= randint(3);
                       else p_ptr->luck -= randint(2);
                       if (p_ptr->luck > 21) msg_print("You feel less lucky.");
                       else if (p_ptr->luck < 9) msg_print("You feel very unlucky.");
                       else msg_print("You feel unlucky.");
                    }

					break;
				}

                case RBE_FRENZY:
                {
					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "confused" */
					if (!p_ptr->resist_charm)
					{
						if (inc_timed(TMD_FRENZY, 4 + randint((rlev / 2))))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_CHARM);

					break;
				}

				case RBE_PIXIEKISS:
				{
					/* conspicuous lack of damage */
					/* <1d30> */
					int iden = 0;

					if (damage < 10)
					{
                       (void)hp_player(3);
                       (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] - 2);
                       (void)inc_timed(TMD_BLESSED, 3);
                    }
					else if (damage < 20)
					{
                       (void)hp_player(damage);
				       if (clear_timed(TMD_CHARM)) iden = 1;
				       if (clear_timed(TMD_FRENZY)) iden = 1;
                       if (set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2)) iden = 1;
                       if (p_ptr->silver > PY_SILVER_HEALTHY)
                       {
                          p_ptr->silver = p_ptr->silver - 1;
                          iden = 1;
                       }
					   if (iden == 1) msg_print("You feel more pure.");
                    } 
					else 
					{
                       (void)hp_player(damage * 2);
                       (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 4);
                       (void)clear_timed(TMD_BLIND);
                       (void)inc_timed(TMD_SINVIS, 3 + randint(4));
                       (void)inc_timed(TMD_BLESSED, randint(6) + 4);
					   msg_print("You feel blessed.");
                    }
					break;
				}

				case RBE_ENTHELP:
				{
					/* conspicuous lack of damage */
					/* <1d22> */
					msg_print("You take some entdraught..");

					if (damage < 10)
					{
                       (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] - 1);
                       (void)inc_timed(TMD_OPP_COLD, randint(15) + 10);
				       (void)clear_timed(TMD_AFRAID);
   			           msg_print("The entdraught makes you feel warm.");
                    }
					else if (damage < 20)
					{
                       (void)hp_player(damage);
                       (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
                       if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 1;
                       (void)inc_timed(TMD_WSHIELD, randint(20) + 5);
                       spadjust = 1 - 2;
                       (void)inc_timed(TMD_ADJUST, randint(5) + 2);
   			           msg_print("The entdraught makes your body feel more fixed. (kindof like a tree..)");
                    }
					else 
					{
                       (void)do_res_stat(A_CON);
                       (void)inc_timed(TMD_WSHIELD, randint(30) + 10);
                       (void)inc_timed(TMD_OPP_POIS, randint(10) + 10);
   			           msg_print("The entdraught makes you feel tough.");
                    }
					break;
				}
				
				case RBE_PURIFY:
				{
					/* conspicuous lack of damage */
					/* <1d25>  */

					if (damage < 10)
					{
                       (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 3);
				       (void)clear_timed(TMD_CHARM);
				       (void)clear_timed(TMD_AFRAID);
				       (void)clear_timed(TMD_FRENZY);
			           (void)clear_timed(TMD_CONFUSED);
			           (void)clear_timed(TMD_AMNESIA);
                       if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 1;
                       (void)hp_player(damage * damage);
                    }
					else if (damage < 20)
					{
                       (void)hp_player(damage);
                       (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
                       if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 2;
                       spadjust = 1;
                       (void)inc_timed(TMD_ADJUST, randint(4) + 3);
                       (void)inc_timed(TMD_OPP_POIS, randint(10) + 10);
                       (void)inc_timed(TMD_SANCTIFY, randint(14) + 8);
   			           msg_print("You feel purified.");
                    }
					else if (damage < 23)
					{
                       (void)hp_player(damage / 2);
                       (void)do_res_stat(A_CON);
                       (void)do_res_stat(A_WIS);
				       (void)clear_timed(TMD_IMAGE);
                       (void)clear_timed(TMD_POISONED);
                       p_ptr->silver = PY_SILVER_HEALTHY;
                       (void)inc_timed(TMD_OPP_POIS, randint(15) + 10);
			           msg_print("You feel purified and healthier.");
                    }
					else
					{
                       int iden = 0;
                       (void)hp_player(damage * 3);
                       if (clear_timed(TMD_POISONED)) iden = 1;
                       if (p_ptr->silver > PY_SILVER_HEALTHY) 
                       {
                          p_ptr->silver = PY_SILVER_HEALTHY;
                          iden = 1;
                       }
                       if (p_ptr->slime > PY_SLIME_HEALTHY)
                       {
                          p_ptr->slime = p_ptr->slime - 5;
                          if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
                          iden = 1;
                       }
                       if (do_res_stat(A_STR)) iden = 1;
                       if (do_res_stat(A_CON)) iden = 1;
                       if (do_res_stat(A_DEX)) iden = 1;
                       if (do_res_stat(A_WIS)) iden = 1;
                       if (do_res_stat(A_INT)) iden = 1;
                       if (do_res_stat(A_CHR)) iden = 1;
			           if (iden == 1) msg_print("You feel purified and restored.");
                    }
					break;
				}
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
			if (do_cut)
			{
				int k;

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
				if (k) (void)inc_timed(TMD_CUT, k);
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
					case 1: k = randint(5); break;
					case 2: k = randint(10) + 10; break;
					case 3: k = randint(20) + 20; break;
					case 4: k = randint(30) + 30; break;
					case 5: k = randint(40) + 40; break;
					case 6: k = 100; break;
					default: k = 200; break;
				}

				/* Apply the stun */
				if (k) (void)inc_timed(TMD_STUN, k);
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
				case RBM_TAIL:
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

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);
    
	/* Assume we attacked */
	return (TRUE);
}
