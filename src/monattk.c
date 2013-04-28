/* File: monattk.c */

/*
 * Monster melee and ranged attacks.  Monster special effects.
 *
 * Copyright (c) 2001 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
 *
 * Now incorporates the effects of terrain and penalizes stunned monsters.
 * Allow dodging and attacks that always hit unless dodged.
 *
 * The chance to hit increases with attack accuracy and decreases with
 * character armour.  It never rises above 95% and never falls below 5%.
 */
static bool check_hit(int power, int level, int terrain_bonus,
	bool dodging_only, bool *dodged, int m_idx)
{
	int ac;
	monster_type *m_ptr = &m_list[m_idx];

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	power = (power + (m_ptr->stunned ? level * 2 : level * 3));

	/* Characters can dodge some blows. */
	if ((dodging_only) || (one_in_(2)))
	{
		/* Allow dodging */
		if (randint(dodging_ability(150)) >= 10 + level)
		{
			*dodged = TRUE;
			return (FALSE);
		}

		/* Some blows cannot be deflected by character armour */
		if (dodging_only) return (TRUE);
	}

	/* Total armour */
	ac = (p_ptr->ac + p_ptr->to_a + terrain_bonus) / 2;

	/* Return hit or miss */
	return (test_hit_combat(power, ac, TRUE));
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
 * Hack -- possible "sneer" messages
 */
static cptr desc_sneer[] =
{
	"offers you a pony for an outrageous sum.",
	"waits to tell the Black Riders where you've gone.",
	"tells you to clear out, or he'll break your neck.",
	"sneers at the company you keep."
};


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(monster_type *m_ptr, int y, int x)
{
	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int i, j, k, tmp, ac, rlev;
	int do_cut, do_stun;
	int touched;
	int blows;

	bool alive = TRUE;

	int terrain_bonus = 0;

	s32b gold;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[120];

	char m_name[80];

	char ddesc[80];

	bool blinked;


	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Total armour */
	ac = p_ptr->ac + p_ptr->to_a +
		(player_invis(m_ptr) ? p_ptr->invisible : 0);

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a white worm mass") */
	monster_desc(ddesc, m_ptr, 0x88);


	/* Players in rubble can take advantage of cover. */
	if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		terrain_bonus = (ac / 8) + 5;
	}
	/* Players in trees can take advantage of cover. */
	if (cave_feat[y][x] == FEAT_TREE)
	{
		terrain_bonus = (ac / 20) + get_skill(S_NATURE, 0, 20);
	}
	/* Players in water are vulnerable. */
	if (cave_feat[y][x] == FEAT_WATER)
	{
		terrain_bonus = -(ac / 3);
	}


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

		bool cannot_miss = FALSE;
		bool dodging_only = FALSE;
		bool no_effect = FALSE;
		bool dodged = FALSE;

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
		if (mon_fully_visible(m_ptr)) visible = TRUE;

		/* Assume no cut, stun, or touch */
		do_cut = do_stun = touched = 0;

		/* Extract the attack "power".  Elemental attacks upgraded. */
		switch (effect)
		{
			case RBE_HURT:       power = 60;  break;
			case RBE_WOUND:      power = 60;  break;
			case RBE_BATTER:     power = 60;  break;
			case RBE_SHATTER:    power = 60;  break;

			case RBE_UN_BONUS:   power = 20;  break;
			case RBE_UN_POWER:   power = 15;  break;
			case RBE_LOSE_MANA:  power = 45;  dodging_only = TRUE;  break;
			case RBE_EAT_GOLD:   power =  5;  dodging_only = TRUE;  break;
			case RBE_EAT_ITEM:   power =  5;  dodging_only = TRUE;  break;
			case RBE_EAT_FOOD:   power = 45;  dodging_only = TRUE;  break;
			case RBE_EAT_LITE:   power = 45;  dodging_only = TRUE;  break;
			case RBE_HUNGER:     power = 45;  dodging_only = TRUE;  break;

			case RBE_POISON:     power = 25;  break;
			case RBE_ACID:       power = 50;  break;
			case RBE_ELEC:       power = 50;  break;
			case RBE_FIRE:       power = 50;  break;
			case RBE_COLD:       power = 50;  break;

			case RBE_BLIND:      power =  5;  break;
			case RBE_CONFUSE:    power = 10;  break;
			case RBE_TERRIFY:    power = 10;  break;
			case RBE_PARALYZE:   power =  5;  break;
			case RBE_HALLU:      power = 10;  break;

			case RBE_LOSE_STR:   power =  0;  break;
			case RBE_LOSE_DEX:   power =  0;  break;
			case RBE_LOSE_CON:   power =  0;  break;
			case RBE_LOSE_INT:   power =  0;  break;
			case RBE_LOSE_WIS:   power =  0;  break;
			case RBE_LOSE_CHR:   power =  0;  break;
			case RBE_LOSE_LUC:   power =  0;  break;
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

				do_cut = do_stun = touched = 1;
				break;
			}

			case RBM_TOUCH:
			{
				act = "touches you";
				touched = 1;
				break;
			}

			case RBM_PUNCH:
			{
				act = "punches you";
				do_stun = touched = 1;
				break;
			}

			case RBM_KICK:
			{
				act = "kicks you";
				do_stun = touched = 1;
				break;
			}

			case RBM_CLAW:
			{
				if      (dam >= 25) act = "slashes you";
				else if (dam >=  5) act = "claws you";
				else                act = "scratches you";
				do_cut = touched = 1;
				break;
			}

			case RBM_BITE:
			{
				if (dam >= 6) act = "bites you";
				else          act = "nips you";
				do_cut = touched = 1;
				break;
			}

			case RBM_PECK:
			{
				act = "pecks you";
				do_cut = touched = 1;
				break;
			}

			case RBM_STING:
			{
				act = "stings you";
				touched = 1;
				break;
			}

			case RBM_XXX1:
			{
				act = "XXX1's you";
				break;
			}

			case RBM_BUTT:
			{
				if (dam >= rand_range(10, 20)) act = "tramples you";
				else                           act = "butts you";
				do_stun = touched = 1;
				break;
			}

			case RBM_CRUSH:
			{
				if (dam >= 10) act = "crushes you";
				else           act = "squeezes you";
				do_stun = touched = 1;
				break;
			}

			case RBM_ENGULF:
			{
				if (dam >= randint(50)) act = "envelops you";
				else                    act = "engulfs you";
				touched = 1;
				break;
			}

			case RBM_CRAWL:
			{
				act = "crawls on you";
				touched = 1;
				break;
			}

			case RBM_DROOL:
			{
				act = "drools on you";
				break;
			}

			case RBM_SPIT:
			{
				act = "spits on you";
				break;
			}

			case RBM_SLIME:
			{
				act = "You've been slimed!";
				break;
			}

			case RBM_GAZE:
			{
				/* Gaze attacks cannot be dodged or deflected, ... */
				cannot_miss = TRUE;

				/* ... but are of little use unless the target can see. */
				if ((p_ptr->blind) && (!one_in_(5))) no_effect = TRUE;

				if      (dam >= rand_range(20, 30))
					act = "glares at you terribly";
				else if (dam >= rand_range(5, 30))
					act = "gazes upon you";
				else act = "gazes at you";
				break;
			}

			case RBM_WAIL:
			{
				cannot_miss = TRUE;
				act = "makes a horrible wail";
				break;
			}

			case RBM_SPORE:
			{
				act = "releases a cloud of spores";
				break;
			}

			case RBM_XXX4:
			{
				act = "projects XXX4's at you";
				break;
			}

			case RBM_BEG:
			{
				dodging_only = TRUE;
				act = "begs you for money";
				break;
			}

			case RBM_INSULT:
			{
				cannot_miss = TRUE;
				act = desc_insult[rand_int(8)];
				break;
			}

			case RBM_SNEER:
			{
				cannot_miss = TRUE;
				act = desc_sneer[rand_int(4)];
				break;
			}
		}


		/* No effect */
		if (no_effect) continue;

		/* Some monsters always hit */
		if (r_ptr->flags2 & (RF2_NOMISS)) cannot_miss = TRUE;


		/* Monster hits player */
		if (!effect || cannot_miss ||
		    check_hit(power, rlev, terrain_bonus, dodging_only,
		              &dodged, m_idx))
		{
			/* Always disturbing */
			disturb(1, 0);

			/* Hack -- Apply "protection from evil". */
			if ((p_ptr->protevil) && (r_ptr->flags3 & (RF3_EVIL)))
			{
				/* Calculate protection value (from 250 to 300) */
				int protection = get_skill(S_PIETY, 250, 300);

				/* Can (rarely) protect against monsters up to 80th level */
				if (randint(protection - (5 * rlev / 2)) >= 100)
				{
					/* Remember the Evil-ness */
					if (mon_fully_visible(m_ptr))
					{
						l_ptr->flags3 |= (RF3_EVIL);
					}

					/* Message */
					msg_format("%^s is repelled.", m_name);

					/* Hack -- Next attack */
					continue;
				}
			}

			/* Message - special handling for sliming attacks */
			if (act)
			{
				if (method == RBM_SLIME) strcpy(msg, format("%s", act));
				else
				{
					if (dam > p_ptr->chp / 3)
						strcpy(msg, format("%^s %s!", m_name, act));
					else
						strcpy(msg, format("%^s %s.", m_name, act));
				}
			}

			/*
			 * The undead can give the player the Black Breath with a
			 * successful blow. Uniques have a much better chance. -LM-
			 */
			if ((r_ptr->level >= 40) && (r_ptr->flags3 & (RF3_UNDEAD)) &&
			    (r_ptr->flags1 & (RF1_UNIQUE)) &&
			    (one_in_(get_skill(S_DOMINION, 200, 300) - r_ptr->level)))
			{
				msg_print("Your foe calls upon your soul!");
				msg_print("You feel the Black Breath slowly draining you of life...");
				p_ptr->black_breath = TRUE;
			}

			else if ((r_ptr->level >= 50) && (r_ptr->flags3 & (RF3_UNDEAD)) &&
				(one_in_(get_skill(S_DOMINION, 400, 600) - r_ptr->level)))
			{
				msg_print("Your foe calls upon your soul!");
				msg_print("You feel the Black Breath slowly draining you of life...");
				p_ptr->black_breath = TRUE;
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply the best available protective bonus from skills */
			if (TRUE)
			{
				int temp = 100;

				/* Up to 20% protection if you have a realm, 40% otherwise */
				int max = 125;
				if (!p_ptr->realm) max = 167;

				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					temp = MAX(temp, get_skill(S_DOMINION, 100, max));
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					temp = MAX(temp, get_skill(S_PIETY, 100, max));
				}
				if (r_ptr->flags3 & (RF3_ANIMAL))
				{
					temp = MAX(temp, get_skill(S_NATURE, 100, max));
				}

				/* Adjust damage (perfect rounding) */
				dam = div_round(dam * 100, temp);
			}

			/* Apply appropriate damage */
			switch (effect)
			{
				/* No effect */
				case 0:
				{
					/* Message */
					if (act) msg_format("%^s %s.", m_name, act);

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
					take_hit(dam, 0, msg, ddesc);

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
					take_hit(dam, 0, msg, ddesc);

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
					take_hit(dam, 0, msg, ddesc);

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
						earthquake(m_ptr->fy, m_ptr->fx, 6);

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					break;
				}

				/* Hit to disenchant */
				case RBE_UN_BONUS:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Allow complete resist */
					if (!p_ptr->resist_disen)
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
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Blindly hunt ten times for an item. */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];
						k_ptr = &k_info[o_ptr->k_idx];

						/*
						 * use "tmp" to decide if a item can
						 * be uncharged.  By default, assume it
						 * can't.
						 */
						tmp = 0;

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Drain charged wands/staffs/rods */
						if ((o_ptr->tval == TV_STAFF) ||
							(o_ptr->tval == TV_WAND) ||
							(o_ptr->tval == TV_ROD))
						{
							/* Case of charged wands/staffs. */
							if (((o_ptr->tval == TV_STAFF) ||
								(o_ptr->tval == TV_WAND)) &&
								(o_ptr->pval)) tmp = 1;

							/* Case of (at least partially) charged rods. */
							if ((o_ptr->tval == TV_ROD) &&
								(o_ptr->timeout < o_ptr->pval)) tmp = 1;

							if (tmp)
							{
								/* Message */
								msg_print("Energy drains from your pack!");

								/* Obvious */
								obvious = TRUE;

								/* Heal; halved in 0.5.1 for mana gain
								 * Is later doubled if it slips to hps
								 */
								j = 2 + rlev / 20;

								/* Handle new-style wands correctly. */
								if (o_ptr->tval == TV_WAND)
								{
									j *= o_ptr->pval;
								}
								/* Handle new-style rods correctly. */
								else if (o_ptr->tval == TV_ROD)
								{
									j *= (o_ptr->pval - o_ptr->timeout) / 30;
								}
								else
								{
									j *= o_ptr->pval * o_ptr->number;
								}

								/* Replenish monster mana */
								if (r_ptr->mana && (m_ptr->mana < r_ptr->mana))
								{
									if ( j > (r_ptr->mana - m_ptr->mana) * 10)
									{
										 j -= (r_ptr->mana - m_ptr->mana) * 10;
										 m_ptr->mana = r_ptr->mana;
									}
									else
									{
										 m_ptr->mana += (j/10) + 1;
										 j = 0;
									}
								}

								/* Add hps with leftover */
								m_ptr->hp += j * 2;

								if (m_ptr->hp > m_ptr->maxhp)
									m_ptr->hp = m_ptr->maxhp;

								/* Uncharge */
								if ((o_ptr->tval == TV_STAFF) ||
									(o_ptr->tval == TV_WAND))
								{
									o_ptr->pval = 0;
								}

								/* New-style rods. */
								if (o_ptr->tval == TV_ROD)
									o_ptr->timeout = o_ptr->pval;


								/* Combine / Reorder the pack */
								p_ptr->notice |=
									(PN_COMBINE | PN_REORDER);

								/* Redraw (later) if needed */
								if (p_ptr->health_who == m_idx)
									p_ptr->redraw |= (PR_HEALTH);

								/* Window stuff */
								p_ptr->window |= (PW_INVEN);

								/* Only hit one inventory slot. */
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
					strcpy(msg_tmp, msg);

					/* Obvious */
					obvious = TRUE;

					/* Damage (mana) */
					if (p_ptr->csp)
					{
						/* Drain depends on maximum mana */
						drain = 2 + rand_int(p_ptr->msp / 10);
						if (p_ptr->wiz_prot) drain /= 3;

						/* Drain the mana */
						if (drain > p_ptr->csp)
						{
							p_ptr->csp = 0;
							p_ptr->csp_frac = 0;

							strcat(msg_tmp, "  Your mana is gone!");
						}
						else
						{
							p_ptr->csp -= drain;
							strcat(msg_tmp, "  Your mana drains away.");
						}

						/* Redraw mana */
						p_ptr->redraw |= (PR_MANA);

						/* Window stuff */
						p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
					}

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Damage (physical) */
					take_hit(dam, 0, msg_tmp, ddesc);


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
					take_hit(dam, 0, msg, ddesc);

					/* Confused monsters cannot steal successfully. */
					if (m_ptr->confused) break;

					/* Obvious */
					obvious = TRUE;

					/* Saving throw (unless paralyzed) based on dex */
					if (!p_ptr->paralyzed && (rand_int(20 + r_ptr->level / 3) <
					    p_ptr->stat_ind[A_DEX]))
					{
						/* Saving throw message */
						msg_print("You quickly protect your money pouch!");

						/* Occasional blink anyway */
						if (one_in_(2)) blinked = TRUE;
					}

					/* Eat gold */
					else
					{
						/* The more gold you have, the smaller a fraction of it is stolen. */
						int div1 = 8 + rsqrt(p_ptr->au / 400L);
						int div2 = div1 * 2;

						/* Steal some gold (at least two gold pieces) */
						gold = rand_range(p_ptr->au / div2, p_ptr->au / div1) +
						       rand_range(2, 25);

						/* Reduce character gold */
						p_ptr->au -= MIN(p_ptr->au, gold);

						/* Messages */
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

						/* Window stuff */
						p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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
					take_hit(dam, 0, msg, ddesc);

					/* Confused monsters cannot steal successfully. */
					if (m_ptr->confused) break;

					/* Saving throw (unless paralyzed) based on dex */
					if (!p_ptr->paralyzed && (rand_int(20 + r_ptr->level / 4) <
					   p_ptr->stat_ind[A_DEX]))
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
						object_desc(o_name, o_ptr, FALSE, 3);

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

						/*
						 * Hack -- If a rod or wand, allocate total
						 * maximum timeouts or charges between those
						 * stolen and those missed. -LM-
						 */
						if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
						{
							k_ptr = &k_info[o_ptr->k_idx];
							i_ptr->pval = o_ptr->pval / o_ptr->number;
							o_ptr->pval -= i_ptr->pval;
						}

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
					take_hit(dam, 0, msg, ddesc);

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
						object_desc(o_name, o_ptr, FALSE, 0);

						/* Message */
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

						/* Yummy for my tummy */
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
					take_hit(dam, 0, msg, ddesc);

					/* We're not dead yet */
					if (!p_ptr->is_dead)
					{
						/* Allow resistance */
						if (rand_int(100) < p_ptr->skill_sav) resist++;
						if (p_ptr->slow_digest) resist += 2;

						/* Message -- only if appropriate */
						if ((resist > 2) &&
						    (p_ptr->food > p_ptr->food_hungry + 1000))
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
				case RBE_EAT_LITE:
				{
					u32b f1, f2, f3;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Get the light source */
					o_ptr = &inventory[INVEN_LITE];

					/* Get object flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Drain fuel */
					if ((o_ptr->pval > 0) && (!(f3 & (TR3_NOFUEL))))
					{
						/* Reduce fuel */
						o_ptr->pval -= rand_range(250, 500);
						if (o_ptr->pval < 1) o_ptr->pval = 1;

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
				}

				/* Hit to poison */
				case RBE_POISON:
				{
					int resist;
					int d = dam;

					/* Adjust damage for resists */
					resist = 2;
					if (p_ptr->resist_pois) resist++;
					if (p_ptr->oppose_pois) resist++;
					dam = 2 * dam / resist;

					/* Take (adjusted) damage */
					take_hit(dam, 0, msg, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_POIS);

					/* Poison the character, unless resistant */
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
					{
						/* Poisoning is not fully cumulative */
						if (p_ptr->poisoned)
						{
							/* Random damage */
							if (set_poisoned(p_ptr->poisoned + randint(d)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							/* Whole damage to double whole damage */
							if (set_poisoned(p_ptr->poisoned + 3 + rand_range(d, d*2)))
							{
								obvious = TRUE;
							}
						}
					}

					break;
				}

				/* Hit to inflict acid damage */
				case RBE_ACID:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					strcpy(msg, format("%s  You are covered in acid!", msg));

					/* Some guaranteed damage. */
					take_hit(dam / 3 + 1, 0, msg, ddesc);

					/* Special damage, reduced greatly by resists. */
					acid_dam(2 * dam / 3, 0, NULL, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_ACID);

					break;
				}

				/* Hit to electrocute */
				case RBE_ELEC:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					strcpy(msg, format("%s  You are struck by electricity!", msg));

					/* Some guaranteed damage. */
					take_hit(dam / 3 + 1, 0, msg, ddesc);

					/* Special damage, reduced greatly by resists. */
					elec_dam(2 * dam / 3, 0, NULL, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_ELEC);

					break;
				}

				/* Hit to burn */
				case RBE_FIRE:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					strcpy(msg, format("%s  You are enveloped in flames!", msg));

					/* Some guaranteed damage. */
					take_hit(dam / 3 + 1, 0, msg, ddesc);

					/* Special damage, reduced greatly by resists. */
					fire_dam(2 * dam / 3, 0, NULL, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FIRE);

					break;
				}

				/* Hit to freeze */
				case RBE_COLD:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					strcpy(msg, format("%s  You are covered with frost!", msg));

					/* Some guaranteed damage. */
					take_hit(dam / 3 + 1, 0, msg, ddesc);

					/* Special damage, reduced greatly by resists. */
					cold_dam(2 * dam / 3, 0, NULL, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_COLD);

					break;
				}

				/* Hit to blind */
				case RBE_BLIND:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Increase blindness */
					if (!p_ptr->resist_blind)
					{
						if (p_ptr->blind)
						{
							if (set_blind(p_ptr->blind + 2 + randint(rlev / 3), NULL))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_blind(5 + randint(rlev),
								"Your eyes begin to sting."))
							{
								obvious = TRUE;
							}
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
					take_hit(dam, 0, msg, ddesc);

					/* Increase confusion */
					if (!p_ptr->resist_confu)
					{
						if (p_ptr->confused)
						{
							if (set_confused(p_ptr->confused + 2 + randint(rlev / 4)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_confused(5 + randint(rlev / 2)))
							{
								obvious = TRUE;
							}
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
					take_hit(dam, 0, msg, ddesc);

					/* Handle resistance or save */
					if ((p_ptr->resist_fear) ||
					    (rand_int(50 + rlev / 2) < p_ptr->skill_sav))
					{
						if (!p_ptr->afraid) msg_print("You stand your ground!");
						obvious = TRUE;
					}

					/* Increase fear */
					else
					{
						if (p_ptr->afraid)
						{
							if (set_afraid(p_ptr->afraid + 2 + randint(rlev / 3)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_afraid(5 + randint(rlev / 2)))
							{
								obvious = TRUE;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FEAR_SAVE);

					break;
				}

				/* Hit to paralyze (never cumulative) */
				case RBE_PARALYZE:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Increase paralyzation */
					if (p_ptr->free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(50 + rlev / 2) < p_ptr->skill_sav)
					{
						msg_print("You resist the effects!");
						obvious = TRUE;
					}
					else
					{
						if (p_ptr->paralyzed)
						{
							/* No further paralyzation */
						}
						else
						{
							if (set_paralyzed(4 + randint(rlev / 5)))
							{
								obvious = TRUE;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FREE_SAVE);

					break;
				}

				/* Hit to cause hallucinations */
				case RBE_HALLU:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Increase hallucination */
					if (p_ptr->resist_chaos ||
						rand_int(rlev / 2) < p_ptr->skill_sav)
					{
						msg_print("Your vision blurs for a moment, but then clears.");
						obvious = TRUE;
					}
					else
					{
						if (set_image(p_ptr->image + 3 + randint(rlev / 2)))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_CHAOS);

					break;
				}

				/* Hit to cause disease */
				case RBE_DISEASE:
				{
					int do_disease = dam;

					/* Player armour reduces raw damage */
					ac_dam(&dam, ac);

					/* Take (adjusted) damage */
					take_hit(dam, 0, msg, ddesc);

					/* Inflict disease (unaffected by armour) */
					disease(&do_disease);

					break;
				}

				/* Hit to reduce stat(s) */
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
					take_hit(dam, 0, msg, ddesc);

					/* Reduce strength */
					if ((effect == RBE_LOSE_STR) || (effect == RBE_LOSE_ALL))
					{
						(void)do_dec_stat(A_STR, 1, FALSE,
							"You feel weaker.",
							"You feel weaker for a moment, but it passes.");
					}

					/* Reduce intelligence */
					if ((effect == RBE_LOSE_INT) || (effect == RBE_LOSE_ALL))
					{
						(void)do_dec_stat(A_INT, 1, FALSE,
							"You have trouble thinking clearly.",
							"You have trouble thinking clearly, but your mind quickly clears.");
					}

					/* Reduce wisdom */
					if ((effect == RBE_LOSE_WIS) || (effect == RBE_LOSE_ALL))
					{
						(void)do_dec_stat(A_WIS, 1, FALSE,
							"Your wisdom is drained.",
							"Your wisdom is sustained.");
					}

					/* Reduce dexterity */
					if ((effect == RBE_LOSE_DEX) || (effect == RBE_LOSE_ALL))
					{
						(void)do_dec_stat(A_DEX, 1, FALSE,
							"You feel more clumsy.",
							"You feel clumsy for a moment, but it passes.");
					}

					/* Reduce constitution */
					if ((effect == RBE_LOSE_CON) || (effect == RBE_LOSE_ALL))
					{
						(void)do_dec_stat(A_CON, 1, FALSE,
							"Your health is damaged.",
							"Your health is sustained.");
					}

					/* Reduce charisma */
					if ((effect == RBE_LOSE_CHR) || (effect == RBE_LOSE_ALL))
					{
						cptr s = "You keep your good looks.";
						if (p_ptr->stat_ind[A_CHR] < 12)
						     s = "You keep what looks you have.";

						(void)do_dec_stat(A_CHR, 1, FALSE,
							"Your features are twisted.",
							s);
					}

					break;
				}

				/* Hit to reduce luck */
				case RBE_LOSE_LUC:
				{
					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

					/* Reduce luck */
					(void)set_luck(p_ptr->luck - rand_range(5, 10),
						"You feel unlucky...");

					/* Always take note of attack */
					obvious = TRUE;

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
					take_hit(dam, 0, msg, ddesc);


					/* Hold life usually prevents life draining */
					if (p_ptr->hold_life)
					{
						int save_chance = 100;
						if (effect == RBE_EXP_10) save_chance = 95;
						if (effect == RBE_EXP_20) save_chance = 90;
						if (effect == RBE_EXP_40) save_chance = 75;
						if (effect == RBE_EXP_80) save_chance = 50;

						/* Test for save */
						if (rand_int(100) < save_chance)
						{
							msg_print("You keep hold of your life force!");
							drain = FALSE;
						}
					}

					/* Drain life */
					if (drain)
					{
						/* Determine amount of draining */
						s32b d = (calc_spent_exp() * MON_DRAIN_LIFE / 100);

						if (effect == RBE_EXP_10) d += Rand_normal( 20,  4);
						if (effect == RBE_EXP_20) d += Rand_normal( 40,  8);
						if (effect == RBE_EXP_40) d += Rand_normal( 80, 16);
						if (effect == RBE_EXP_80) d += Rand_normal(160, 32);

						/* Hold life greatly reduces any draining */
						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_exp(d / 10, FALSE);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp(d, FALSE);
						}
					}
					break;
				}

				/* Undefined hit */
				default:
				{
					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, 0, msg, ddesc);

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

			/* Handle monster death  XXX */
			if ((m_ptr->hp < 0) || (!m_ptr->maxhp)) return (TRUE);


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
					case 0:  k = 0; break;
					case 1:  k = randint(5); break;
					case 2:  k = rand_range(  5,  10); break;
					case 3:  k = rand_range( 20,  40); break;
					case 4:  k = rand_range( 50, 100); break;
					case 5:  k = rand_range(100, 200); break;
					case 6:  k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if (k) (void)set_cut(p_ptr->cut + k);
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
				if (k) (void)set_stun(p_ptr->stun + k);
			}

			/* Monsters can be hurt by touching the character.  -Zangband- */
			if (touched)
			{
				/* Aura of fire */
				if ((p_ptr->aura_fire) && (alive))
				{
					/* Allow resistance */
					if (r_ptr->flags3 & (RF3_IM_FIRE))
					{
						if (mon_fully_visible(m_ptr))
							l_ptr->flags3 |= (RF3_IM_FIRE);
					}
					else
					{
						bool dummy;

						/* Monster is dead */
						if (mon_take_hit(m_idx, -1, damroll(2, 6), &dummy,
							" turns into a pile of ash."))
						{
							alive = FALSE;
						}

						/* Monster is hurt */
						else if (m_ptr->ml)
							msg_format("%^s is suddenly very hot!", m_name);
					}
				}

				/* Aura of cold */
				if ((p_ptr->aura_cold) && (alive))
				{
					/* Allow resistance */
					if (r_ptr->flags3 & (RF3_IM_COLD))
					{
						if (mon_fully_visible(m_ptr))
							l_ptr->flags3 |= (RF3_IM_COLD);
					}
					else
					{
						bool dummy;

						/* Monster is dead */
						if (mon_take_hit(m_idx, -1, damroll(2, 6), &dummy,
							" freezes solid!"))
						{
							alive = FALSE;
						}

						/* Monster is hurt */
						else if (m_ptr->ml)
							msg_format("%^s is suddenly very cold!", m_name);
					}
				}
			}

			/* Sometimes learn about a piece of worn, unidentified armour */
			if (one_in_(3))
			{
				/* Choose a slot at random */
				int slot = rand_range(INVEN_BODY, INVEN_SUBTOTAL - 1);

				/* Get object in slot */
				o_ptr = &inventory[slot];

				/* Try to learn more - if armour exists */
				if ((o_ptr->k_idx) && (is_any_armour(o_ptr)))
				{
					learn_about_wearable(o_ptr, slot, FALSE);
				}
			}

			/* We have a blink away field active */
			if (p_ptr->blink_away)
			{
				/* Sometimes blink away */
				if (one_in_(1 + blows))
				{
					/* Blink away */
					if (do_blink_away(m_ptr, m_idx))
					{
						/* We cleared some distance -- end attack */
						break;
					}
				}
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
				case RBM_XXX1:
				case RBM_BUTT:
				case RBM_CRUSH:

				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1, 0);

					/* Message */
					if (dodged) msg_format("You dodge %s.", m_name);
					else        msg_format("%^s misses you.", m_name);
				}

				break;
			}
		}

		/* Analyze visible monsters */
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
	if ((blinked) && (alive))
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	/* Assume we attacked */
	return (TRUE);
}




/*
 * Handle a maddened monster's melee attack.  As in Zangband, we use bolt
 * projections because of the pretty graphical effects.  Unlike Zangband,
 * we do not output any messages (except those caused by the projection
 * itself).
 */
void mad_mon_melee(int m_idx, monster_type *m_ptr, int ty, int tx)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	/* Monster being attacked */
	monster_type *m2_ptr;
	monster_race *r2_ptr;

	int m2_idx;
	bool fear;

	int typ, dam,
	ap_cnt = 0;


	int old_delay = op_ptr->delay_factor;

	/* Always display the attacks  XXX */
	if (op_ptr->delay_factor < 2) op_ptr->delay_factor = 2;


	/* Scan through the blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		/* Extract the attack information */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;


		/* Hack -- no more attacks */
		if (!method) break;

		/* Get the index of the creature in this grid for each blow */
		m2_idx = cave_m_idx[ty][tx];

		/* Require that a monster (not a character) be present */
		if (m2_idx <= 0) return;

		/* Get the monster in this grid */
		m2_ptr = &m_list[m2_idx];

		/* Get the monster race */
		r2_ptr = &r_info[m_ptr->r_idx];



		/* Allow misses  XXX XXX */
		if (!(r_ptr->flags2 & (RF2_NOMISS)) &&
		     (rand_int(r_ptr->level) < rand_int(r2_ptr->level)))
		{
			continue;
		}

		/* Assume normal damage */
		dam = damroll(d_dice, d_side);

		/* Assume pure hurt */
		typ = GF_HURT;


		/* Translate the attack into a projection */
		switch (effect)
		{
			/* No effect */
			case 0:
			{
				/* Hack -- No damage */
				typ = 0;
				dam = 0;

				break;
			}

			/* Hit with increased chance to wound */
			case RBE_WOUND:
			{
				/* Target monster is living */
				if (!monster_nonliving(r2_ptr))
				{
					/* chance for extra damage  XXX */
					if (one_in_(2)) dam += dam / 3;
				}

				break;
			}

			/* Hit with increased chance to stun */
			case RBE_BATTER:
			{
				/* Possibly stun the target monster */
				if (!(r2_ptr->flags3 & (RF3_NO_STUN)) && (one_in_(3)))
				{
					m_ptr->stunned += randint(4);
				}

				break;
			}

			/* Hit to cause earthquakes */
			case RBE_SHATTER:
			{
				/* Radius 6 earthquake centered on the monster */
				if (dam > rand_int(60))
					earthquake(m_ptr->fy, m_ptr->fx, 6);

				break;
			}

			/* Hit to disenchant or discharge */
			case RBE_UN_BONUS:
			case RBE_UN_POWER:
			{
				typ = GF_DISENCHANT;
				break;
			}

			/* Hit to reduce mana */
			case RBE_LOSE_MANA:
			{
				int drain;

				/* Damage (mana) */
				if (m_ptr->mana)
				{
					drain = 2 + rand_int(r2_ptr->mana / 10);

					if (drain > m_ptr->mana) m_ptr->mana = 0;
					else m_ptr->mana -= drain;
				}
				break;
			}

			/* Hit to drain light */
			case RBE_EAT_LITE:
			{
				typ = GF_DARK_WEAK;
				break;
			}

			/* Hit to poison */
			case RBE_POISON:
			{
				dam += dam / 3;
				typ = GF_POIS;
				break;
			}

			/* Hit to inflict acid damage */
			case RBE_ACID:
			{
				dam += dam / 4;
				typ = GF_ACID;
				break;
			}

			/* Hit to electrocute */
			case RBE_ELEC:
			{
				dam += dam / 4;
				typ = GF_ELEC;
				break;
			}

			/* Hit to burn */
			case RBE_FIRE:
			{
				dam += dam / 4;
				typ = GF_FIRE;
				break;
			}

			/* Hit to freeze */
			case RBE_COLD:
			{
				dam += dam / 4;
				typ = GF_COLD;
				break;
			}

			/* Hit to blind */
			case RBE_BLIND:
			{
				/* Hurt the target monster immediately */
				(void)mon_take_hit(m2_idx, m_idx, dam, &fear, NULL);

				/* Try to confuse it  XXX */
				typ = GF_DO_CONF;
				dam = 10 + 3 * r_ptr->level / 5;
			}

			/* Hit to confuse */
			case RBE_CONFUSE:
			{
				/* Use strong confusion */
				typ = GF_CONFUSION;
				break;
			}

			/* Hit to frighten */
			case RBE_TERRIFY:
			{
				/* Hurt the target monster immediately */
				(void)mon_take_hit(m2_idx, m_idx, dam, &fear, NULL);

				/* Try to terrify it */
				typ = GF_DO_FEAR;
				dam = 10 + 3 * r_ptr->level / 5;
				break;
			}

			/* Hit to paralyze (never cumulative) */
			case RBE_PARALYZE:
			{
				/* Hurt the target monster immediately */
				(void)mon_take_hit(m2_idx, m_idx, dam, &fear, NULL);

				/* Try to make it go to sleep */
				typ = GF_DO_SLEEP;
				dam = 10 + 3 * r_ptr->level / 5;
				break;
			}

			/* Hit to cause hallucinations */
			case RBE_HALLU:
			{
				typ = GF_CHAOS;
				break;
			}

			/* Hit to cause disease */
			case RBE_DISEASE:
			{
				dam += dam / 2;
				typ = GF_POIS;
				break;
			}

			/* Hit to reduce skills */
			case RBE_EXP_10:
			case RBE_EXP_20:
			case RBE_EXP_40:
			case RBE_EXP_80:
			{
				typ = GF_NETHER;
				break;
			}

			/* All other types of blows */
			default:
			{
				break;
			}
		}

		/* Get target monster's index (again) */
		m2_idx = cave_m_idx[ty][tx];

		/* Require that it be present */
		if (m2_idx <= 0) return;

		/* Get the target monster (again) */
		m2_ptr = &m_list[m2_idx];


		/* Cast a bolt spell */
		if ((typ) && (dam))
		{
			(void)project_bolt(m_idx, 1, m_ptr->fy, m_ptr->fx,
									 m2_ptr->fy, m2_ptr->fx, dam, typ, 0L);
		}

		/* Analyze visible monsters */
		if (m_ptr->ml)
		{
			/* Count attacks of this type */
			if (l_ptr->blows[ap_cnt] < MAX_UCHAR)
			{
				l_ptr->blows[ap_cnt]++;
			}
		}
	}

	/* Restore the old delay factor */
	op_ptr->delay_factor = old_delay;
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
 *
 * Actual damage is normally quite close to the average, but may occas-
 * ionally (about 5% of the time) differ by as much as av_dam * 2 / div,
 * or two standard deviations.
 */
static int get_dam(int av_dam, int control)
{
	int dam = 0;

	/* Damage may never differ by more than 50% from the average */
	if (control < 4) control = 4;

	/* Loop until damage is within two standard deviations */
	while (TRUE)
	{
		/* Randomize damage (average, standard deviation) */
		dam = Rand_normal(av_dam, div_round(av_dam, control));

		/* Forbid variation beyond two standard deviations */
		if (dam > av_dam + (av_dam * 2 / control)) continue;
		if (dam < av_dam - (av_dam * 2 / control)) continue;

		/* Forbid a difference of more than a hundred points of damage */
		if (dam > av_dam + 100) continue;
		if (dam < av_dam - 100) continue;

		/* Accept */
		break;
	}

	/* Return randomized damage */
	return (dam);
}


/*
 * Maddened monsters, like all others, only cast projection spells if the
 * character is projectable.  However, they often go berserk and target a
 * nearby monster instead (which need not be projectable from the char-
 * acter's position).  The end effect is something like that of Nethack's
 * Rings of Conflict.
 *
 * There are a very large number of omissions in this code.  If, as, and
 * when monsters attacking other monsters becomes an important feature of
 * Sangband, a rewrite will become essential.  I direct your attention to
 * Zangband, ToME, Hengband, and other variants with well-developed code.
 */
void mad_mon_retarget(int y0, int x0, int *y, int *x)
{
	int ty, tx;
	int dist_chr, dist_alt;

	/* Get the closest monster in line of sight  XXX XXX */
	get_closest_los_monster(1, *y, *x, &ty, &tx, FALSE);

	/* No such creature */
	if (!in_bounds_fully(ty, tx)) return;

	/* Get distance to character */
	dist_chr = distance(y0, x0, p_ptr->py, p_ptr->px);

	/* Get distance to proposed monster target */
	dist_alt = distance(y0, x0, ty, tx);

	/*
	 * The closer the alternative monster is compared to the character, the
	 * more likely it is that the maddened monster will prefer it as a target.
	 */
	if (rand_int(dist_alt) < rand_int(dist_chr))
	{
		/* Reset target */
		*y = ty;
		*x = tx;
	}
}



/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void mon_bolt(int m_idx, int typ, int dam)
{
	monster_type *m_ptr = &m_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

	/* Maddened monsters may adjust their target */
	if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px);

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, fy, fx, py, px, dam, typ, flg, 0, 0);
}

/*
 * Cast a beam at the player, sometimes with limited range.
 * Do not stop if we hit a monster
 * Affect grids, monsters, and the player
 */
static void mon_beam(int m_idx, int typ, int dam, int range)
{
	monster_type *m_ptr = &m_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
	           PROJECT_PLAY;

	/* Maddened monsters may adjust their target */
	if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px);

	/* Target the player with a beam attack */
	(void)project(m_idx, range, fy, fx, py, px, dam, typ, flg, 0, 0);
}

/*
 * Cast a ball spell at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void mon_ball(int m_idx, int typ, int dam, int rad, bool jump)
{
	monster_type *m_ptr = &m_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
	           PROJECT_PLAY;

	/* Optionally, allow the attack to "jump" to the player */
	if (jump) flg |= (PROJECT_JUMP);

	/* Maddened monsters may adjust their target */
	if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px);

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
	monster_type *m_ptr = &m_list[m_idx];
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY;

	/* Hack -- unlight surrounding area */
	if (typ == GF_DARK_WEAK) flg |= (PROJECT_HIDE);

	/* Surround the monster with a cloud */
	(void)project(m_idx, rad, fy, fx, fy, fx, dam, typ, flg, 0, 0);
}

/*
 * Breathe or cast an arc-shaped spell at the player.
 * Use an arc spell of specified range and width.
 * Optionally, do not harm monsters with the same r_idx.
 * Affect grids, objects, monsters, and (specifically) the player.
 *
 * Monster breaths do not lose strength with distance at the same rate
 * that normal arc spells do.  If the monster is "powerful", they lose
 * less strength; otherwise, they lose more.
 */
static void mon_arc(int m_idx, int typ, bool noharm, int dam, int rad,
	int degrees_of_arc)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_ARC | PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM |
	           PROJECT_KILL | PROJECT_PLAY;


	/* Diameter of source of energy is at least 20. */
	int diameter_of_source = 20;

	/* XXX XXX -- POWERFUL monster breaths lose less damage with range. */
	int degree_factor = (r_ptr->flags2 & (RF2_POWERFUL)) ? 120 : 60;

	/* Narrow arcs lose relatively little energy over distance. */
	if (degrees_of_arc < degree_factor)
	{
		if (degrees_of_arc <= 6) diameter_of_source = rad * 10;
		else diameter_of_source = diameter_of_source * degree_factor /
			degrees_of_arc;
	}


	/* Can optionally ignore monsters with the same r_idx. */
	if (noharm) project_immune = m_ptr->r_idx;

	/* Radius of zero means no fixed limit. */
	if (rad == 0) rad = MAX_SIGHT;

	/* Maddened monsters may adjust their target */
	if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px);

	/* Target the player with an arc-shaped attack. */
	(void)project(m_idx, rad, fy, fx, py, px, dam, typ, flg, degrees_of_arc,
		(byte)diameter_of_source);
}


/*
 * Monsters can concentrate light or conjure up darkness.
 *
 * Weaker monsters affect a smaller radius, but still depend on the full
 * radius of 6 for damage (helps the character guard himself).
 *
 * Return TRUE if the monster did anything.
 */
static int mon_concentrate_light(monster_type *m_ptr, char *desc, int m_idx,
	int spower)
{
	int damage, radius, lit_grids;


	/* Require a clean shot (LOF is reflexive) */
	if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx)) return (0);


	/* Radius of darkness-creation varies depending on spower */
	radius = MIN(6, 3 + spower / 20);

	/* Check to see how much we would gain (use a radius of 6) */
	lit_grids = concentrate_light(m_idx, m_ptr->fy, m_ptr->fx, 6,
		desc, FALSE);

	/* We have enough juice to make it worthwhile (make a hasty guess) */
	if (lit_grids >= rand_range(40, 60))
	{
		/* Actually concentrate the light */
		(void)concentrate_light(m_idx, m_ptr->fy, m_ptr->fx, radius,
			desc, TRUE);

		/* Calculate damage (60 grids => break-even point) */
		damage = lit_grids * spower / 20;

		/* Limit damage, but allow fairly high values */
		if (damage > 9 * spower / 2) damage = 9 * spower / 2;

		/* We did something */
		return (damage);
	}

	/* We decided not to do anything */
	return (0);
}


/*
 * Monster attempts to make a ranged (non-melee) attack.  Handle mana
 * expenditure, update character knowledge.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 */
bool make_attack_ranged(monster_type *m_ptr, int attack)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, k, rlev, spower, rad;

	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	/* Target player */
	int x = px;
	int y = py;

	/* Summon count */
	int count = 0;

	/* Summon level */
	int summon_lev;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Can the player see the monster casting the spell? */
	bool seen = (!blind && m_ptr->ml);


	/* Hack - Spend mana */
	if (attack >= 224) return (FALSE);
	else if (attack >= 192) m_ptr->mana -= mana_cost_RF7[attack-192];
	else if (attack >= 160) m_ptr->mana -= mana_cost_RF6[attack-160];
	else if (attack >= 128) m_ptr->mana -= mana_cost_RF5[attack-128];
	else if (attack >= 96)  m_ptr->mana -= mana_cost_RF4[attack-96];
	else return (FALSE);

	/*** Get some info. ***/

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Extract the monster's spell power.  Must be at least 1. */
	spower = ((r_ptr->spell_power > 1) ? r_ptr->spell_power : 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);


	/* Get the summon level */
	if (r_ptr->d_char == 'Q') summon_lev = r_ptr->level + 3;
	else                      summon_lev = r_ptr->level - 1;


	/* Hack -- a visible monster loses any hidden mimic status */
	if ((m_ptr->ml) && (m_ptr->mflag & (MFLAG_MIME)))
	{
		/* No longer hidden */
		m_ptr->mflag &= ~(MFLAG_MIME);

		/* Get monster name */
		monster_desc(m_name, m_ptr, 0x08);

		/* Notice monster */
		msg_format("%^s appears.", m_name);

		/* Focus on this monster, unless otherwise occupied */
		if (!p_ptr->health_who)
		{
			p_ptr->health_who = cave_m_idx[m_ptr->fy][m_ptr->fx];
			p_ptr->redraw |= (PR_HEALTH);
		}
	}

	/* Hack -- allow spells to be cancelled */
	if (p_ptr->forbid_summoning)
	{
		bool summon = FALSE;

		/* Is the spell a summon spell? */
		if      (attack >= 192)
		{
			if (spell_desire_RF7[attack-192][D_SUMM]) summon = TRUE;
		}
		else if (attack >= 160)
		{
			if (spell_desire_RF6[attack-160][D_SUMM]) summon = TRUE;
		}
		else if (attack >= 128)
		{
			if (spell_desire_RF5[attack-128][D_SUMM]) summon = TRUE;
		}
		else
		{
			if (spell_desire_RF4[attack-192][D_SUMM]) summon = TRUE;
		}

		/* Spell is a summon spell */
		if (summon)
		{
			/* Monster must override the forbid summoning spell to cast */
			if (randint(3 * r_ptr->level / 2) < p_ptr->power)
			{
				/* Monster loses its turn */
				msg_format("%^s attempts to summon, but fails!", m_name);
				return (TRUE);
			}
		}
	}


	/*** Execute the ranged attack chosen. ***/
	switch (attack)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
			disturb(1, 0);
			if (r_ptr->flags2 & (RF2_SMART))
				msg_format("%^s shouts for help.", m_name);
			else
				msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx, FALSE, NULL);
			break;
		}

		/* RF4_LASH  (used for whips, but also for spitting) */
		case 96+1:
		{
			/* Attack type and descriptions. */
			int typ;
			cptr desc;
			cptr add_of;


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

			/* Determine projection type, using effect.
			 * GF_WHIP is used for pure damage with no
			 * extras -- many attacks need this.
			 */
			switch (effect)
			{
				case RBE_HURT:
				{
					typ = GF_WHIP;
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
					if (p_ptr->resist_acid) damage *= 2;
					if (p_ptr->oppose_acid) damage *= 2;
					typ = GF_ACID;
					desc = " acid";
					add_of = " of";
					break;
				}
				case RBE_ELEC:
				{
					if (p_ptr->resist_elec) damage *= 2;
					if (p_ptr->oppose_elec) damage *= 2;

					typ = GF_ELEC;
					desc = " lightning";
					add_of = " of";
					break;
				}
				case RBE_FIRE:
				{
					if (p_ptr->resist_fire) damage *= 2;
					if (p_ptr->oppose_fire) damage *= 2;

					typ = GF_FIRE;
					desc = " fire";
					add_of = " of";
					break;
				}
				case RBE_COLD:
				{
					if (p_ptr->resist_cold) damage *= 2;
					if (p_ptr->oppose_cold) damage *= 2;

					typ = GF_COLD;
					desc = " frost";
					add_of = " of";
					break;
				}
				case RBE_POISON:
				{
					if (p_ptr->resist_pois) damage *= 2;
					if (p_ptr->oppose_pois) damage *= 2;

					typ = GF_POIS;
					desc = " venom";
					add_of = " of";
					break;
				}
				case RBE_BLIND:
				{
					typ = GF_DARK;
					desc = " blackness";
					add_of = " of";
					break;
				}
				case RBE_CONFUSE:
				case RBE_PARALYZE:
				{
					typ = GF_DARK;
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
					typ = GF_WHIP;
					desc = "";
					add_of = "";
					break;
				}
			}

			/* XXX -- Animals spit.   Acid-users spit. */
			if ((r_ptr->flags3 & (RF3_ANIMAL)) || (typ == GF_ACID))
			{
				if (blind) msg_print("You hear a soft sound.");
				else msg_format("%^s spits%s at you.",
					m_name, desc);
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

			break;
		}

		/* RF4_BOULDER */
		case 96+2:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear something grunt with exertion.");
			else msg_format("%^s hurls a boulder at you.", m_name);
			mon_bolt(m_idx, GF_ROCK, get_dam(spower * 4, 10));
			break;
		}

		/* RF4_SHOT */
		case 96+3:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear something whirl towards you.");
			else if (spower < 5) msg_format("%^s slings a pebble at you.", m_name);
			else if (spower < 15) msg_format("%^s slings a leaden pellet at you.", m_name);
			else msg_format("%^s slings a seeker shot at you.", m_name);

			mon_bolt(m_idx, GF_SHOT, get_dam(spower * 4, 10));
			break;
		}

		/* RF4_ARROW */
		case 96+4:
		{
			disturb(1, 0);
			if (spower < 8)
			{
				if (blind) msg_print("You hear a soft twang.");
				else msg_format("%^s fires a small arrow.", m_name);
			}
			else if (spower < 15)
			{
				if (blind) msg_print("You hear a twang.");
				else msg_format("%^s fires an arrow.", m_name);
			}
			else
			{
				if (blind) msg_print("You hear a loud thwang.");
				else msg_format("%^s fires a seeker arrow.", m_name);
			}

			mon_bolt(m_idx, GF_ARROW, get_dam(spower * 4, 10));
			break;
		}

		/* RF4_BOLT */
		case 96+5:
		{
			disturb(1, 0);
			if (spower < 8)
			{
				if (blind) msg_print("You hear a soft twung.");
				else msg_format("%^s fires a little bolt.", m_name);
			}
			else if (spower < 15)
			{
				if (blind) msg_format("You hear a twung.");
				else msg_format("%^s fires a crossbow bolt.", m_name);
			}
			else
			{
				if (blind) msg_print("You hear a loud thwung.");
				else msg_format("%^s fires a seeker bolt.", m_name);
			}

			mon_bolt(m_idx, GF_ARROW, get_dam(spower * 4, 10));
			break;
		}

		/* RF4_MISSL */
		case 96+6:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear something coming at you.");
			else msg_format("%^s fires a missile.", m_name);
			mon_bolt(m_idx, GF_MISSILE, get_dam(spower * 3, 10));
			break;
		}

		/* RF4_PMISSL */
		case 96+7:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear a soft 'fftt' sound.");
			else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
				msg_format("%^s hurls a black dart at you!", m_name);
			else msg_format("%^s whips a poisoned dart at you.", m_name);
			mon_bolt(m_idx, GF_PMISSILE, get_dam(spower * 3, 10));
			break;
		}


		/* RF4_BRTH_ACID */
		case 96+8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);

			/*
			 * Breaths are 40-degree arcs for POWERFUL monsters,
			 * 20 degrees for others.
			 */
			mon_arc(m_idx, GF_ACID, TRUE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_ELEC */
		case 96+9:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);
			mon_arc(m_idx, GF_ELEC, TRUE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_FIRE */
		case 96+10:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);
			mon_arc(m_idx, GF_FIRE, TRUE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_COLD */
		case 96+11:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes frost.", m_name);
			mon_arc(m_idx, GF_COLD, TRUE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_POIS */
		case 96+12:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gas.", m_name);
			mon_arc(m_idx, GF_POIS, TRUE,
			       ((m_ptr->hp / 4) > 800 ? 800 : (m_ptr->hp / 4)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 50 : 30));
			break;
		}

		/* RF4_BRTH_PLAS */
		case 96+13:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes plasma.", m_name);
			mon_arc(m_idx, GF_PLASMA, TRUE,
			       ((m_ptr->hp / 2) > 1600 ? 1600 : (m_ptr->hp / 2)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_LITE */
		case 96+14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes light.", m_name);
			mon_arc(m_idx, GF_LITE, TRUE,
			       ((3 * m_ptr->hp / 10) > 400 ? 400 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_DARK */
		case 96+15:
		{
			disturb(1, 0);

			if (!(r_ptr->flags2 & (RF2_MORGUL_MAGIC)))
			{
				if (blind) msg_format("%^s breathes.", m_name);
				msg_format("%^s breathes darkness.", m_name);
				mon_arc(m_idx, GF_DARK, TRUE,
				       ((3 * m_ptr->hp / 10) > 500 ? 500 : (3 * m_ptr->hp / 10)),
				       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			}
			else
			{
				if (blind) msg_format("%^s breathes.", m_name);
				msg_format("%^s breathes Night.", m_name);
				mon_arc(m_idx, GF_MORGUL_DARK, TRUE,
				       ((3 * m_ptr->hp / 10) > 500 ? 500 : (3 * m_ptr->hp / 10)),
				       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			}
			break;
		}

		/* RF4_BRTH_CONFU */
		case 96+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes confusion.", m_name);
			mon_arc(m_idx, GF_CONFUSION, TRUE,
			       ((3 * m_ptr->hp / 10) > 400 ? 400 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_SOUND */
		case 96+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sound.", m_name);
			mon_arc(m_idx, GF_SOUND, TRUE,
			       ((m_ptr->hp / 4) > 250 ? 250 : (m_ptr->hp / 4)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 50 : 30));
			break;
		}

		/* RF4_BRTH_SHARD */
		case 96+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes shards.", m_name);
			mon_arc(m_idx, GF_SHARD, TRUE,
			       ((3 * m_ptr->hp / 10) > 400 ? 400 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_INER */
		case 96+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes inertia.", m_name);
			mon_arc(m_idx, GF_INERTIA, TRUE,
			       ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_GRAV */
		case 96+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);
			mon_arc(m_idx, GF_GRAVITY, TRUE,
			       ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_WIND */
		case 96+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes winds.", m_name);
			mon_arc(m_idx, GF_WIND, TRUE,
			       ((3 * m_ptr->hp / 10) > 250 ? 250 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_FORCE */
		case 96+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes force.", m_name);
			mon_arc(m_idx, GF_FORCE, TRUE,
			       ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 50 : 30));
			break;
		}

		/* RF4_BRTH_NEXUS */
		case 96+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nexus.", m_name);
			mon_arc(m_idx, GF_NEXUS, TRUE,
			       ((m_ptr->hp / 3) > 300 ? 300 : (m_ptr->hp / 3)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_NETHR */
		case 96+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes nether.", m_name);
			mon_arc(m_idx, GF_NETHER, TRUE,
			       ((3 * m_ptr->hp / 10) > 500 ? 500 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_CHAOS */
		case 96+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes chaos.", m_name);
			mon_arc(m_idx, GF_CHAOS, TRUE,
			       ((3 * m_ptr->hp / 10) > 500 ? 500 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_DISE */
		case 96+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes disenchantment.", m_name);
			mon_arc(m_idx, GF_DISENCHANT, TRUE,
			       ((3 * m_ptr->hp / 10) > 350 ? 350 : (3 * m_ptr->hp / 10)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_TIME */
		case 96+27:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes time.", m_name);
			mon_arc(m_idx, GF_TIME, TRUE,
			       ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BRTH_MANA */
		case 96+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes raw mana.", m_name);
			mon_arc(m_idx, GF_MANA, TRUE,
			       ((m_ptr->hp / 4) > 300 ? 300 : (m_ptr->hp / 4)),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 30 : 15));
			break;
		}

		/* RF4_XXX8 */
		case 96+29:
		{
			break;
		}

		/* RF4_XXX8 */
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
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an acid ball.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts an acid ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of acid.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
			}
			mon_ball(m_idx, GF_ACID, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BALL_ELEC */
		case 128+1:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of electricity.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a ball of electricity.", m_name);
				rad = 2;
			}

			/* Electricity is the most variable of all attacks at high level. */
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);

				if (!one_in_(3))
				{
					msg_format("%^s invokes a storm of electricity.", m_name);
					if (spower < 120) rad = 3;
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
			mon_ball(m_idx, GF_ELEC, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BALL_FIRE */
		case 128+2:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of %sfire.", m_name,
					(r_ptr->flags2 & (RF2_UDUN_MAGIC) ? "hell" : ""));
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a ball of %sfire.", m_name,
					(r_ptr->flags2 & (RF2_UDUN_MAGIC) ? "hell" : ""));
				rad = 2;
			}
			else if (spower < 110)
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
					msg_format("%^s invokes a storm of Udun-fire.", m_name);
				else msg_format("%^s invokes a firestorm.", m_name);
				rad = 3;
			}
			else
			{
				if (blind) msg_format("%^s intones in rising wrath.", m_name);
				else if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
					msg_format("%^s calls upon the fires of Udun!", m_name);

				else msg_format("%^s conjures up a maelstrom of fire!", m_name);
				rad = 4;
			}
			if (r_ptr->flags2 & (RF2_UDUN_MAGIC)) mon_ball(m_idx,
				GF_HELLFIRE, get_dam(spower * 6, 6), rad, TRUE);
			else mon_ball(m_idx, GF_FIRE, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BALL_COLD */
		case 128+3:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a frost ball.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a frost ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of frost.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
			}
			mon_ball(m_idx, GF_COLD, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BALL_POIS */
		case 128+4:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a stinking cloud.", m_name);
				rad = 2;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a venomous cloud.", m_name);
				rad = 3;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of poison.", m_name);
				if (spower < 120) rad = 4;
				else rad = 5;
			}
			mon_ball(m_idx, GF_POIS, get_dam(2 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BALL_LITE */
		case 128+5:
		{
			char desc[30];

			disturb(1, 0);

			/* Sometimes try to concentrate light */
			if (one_in_(2))
			{
				/* Check to see if doing so would be worthwhile */
				int damage = mon_concentrate_light(m_ptr,
					desc, m_idx, spower);

				/* We decided to concentrate light */
				if (damage)
				{
					/* Message */
					msg_format("%^s concentrates light... and releases it %s", m_name, desc);

					/* Fire bolt */
					mon_bolt(m_idx, GF_LITE, damage);

					/* Done */
					break;
				}
			}

			/* Standard ball of light */
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of light.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s invokes a starburst.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a starburst.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_LITE, get_dam(3 * spower, 9), rad, FALSE);
			break;
		}

		/* RF5_BALL_DARK */
		case 128+6:
		{
			char desc[30];

			disturb(1, 0);

			/* Sometimes try to conjure up darkness */
			if (one_in_(2))
			{
				/* Check to see if doing so would be worthwhile */
				int damage = mon_concentrate_light(m_ptr,
					desc, m_idx, spower);

				/* We decided to conjure up darkness */
				if (damage)
				{
					/* Message */
					msg_format("%^s conjures up darkness... and releases it %s", m_name, desc);

					/* Fire bolt */
					mon_bolt(m_idx, GF_DARK, damage);

					/* Done */
					break;
				}
			}

			/* Standard ball of darkness */
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					msg_format("%^s casts a ball of Night.", m_name);
				else msg_format("%^s casts a ball of darkness.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					msg_format("%^s casts a ball of Night.", m_name);
				else msg_format("%^s casts a ball of darkness.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					msg_format("%^s invokes a storm of Night.", m_name);
				else msg_format("%^s invokes a darkness storm.", m_name);
				if (spower < 110) rad = 3;
				else rad = 4;
			}
			if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
				mon_ball(m_idx, GF_MORGUL_DARK,
				   get_dam(3 * spower, 9), rad, FALSE);
			else
				mon_ball(m_idx, GF_DARK,
				   get_dam(3 * spower, 9), rad, FALSE);
			break;
		}

		/* RF5_BALL_CONFU */
		case 128+7:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a ball of confusion.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a ball of confusion.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of confusion.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_CONFUSION, get_dam(3 * spower, 8), rad, FALSE);
			break;
		}

		/* RF5_BALL_SOUND */
		case 128+8:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s calls up a blast of sound.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s invokes a thunderclap.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s unleashes a cacophony of sound.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_SOUND, get_dam(2 * spower, 8), rad, TRUE);
			break;
		}

		/* RF5_BALL_SHARD */
		case 128+9:
		{
			disturb(1, 0);
			if (spower < 15)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s calls up up a blast of shards.", m_name);
				rad = 1;
			}
			else if (spower < 90)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s calls up a whirlwind of shards.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of knives!", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_SHARD, get_dam(3 * spower, 8), rad, TRUE);
			break;
		}


		/* RF5_BALL_WIND */
		case 128+10:
		{
			disturb(1, 0);
			if (spower < 25)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s calls up a blast of wind.", m_name);
				rad = 3;
			}
			else if (spower < 55)
			{
				if (blind) msg_format("%^s calls out.", m_name);
				else msg_format("%^s calls forth a whirlwind.", m_name);
				rad = 4;
			}
			else
			{
				if (blind) msg_format("%^s calls out powerfully.", m_name);
				else msg_format("%^s gestures wildly.", m_name);
				msg_print("You are enveloped in a cyclone!");
				rad = 5;
			}

			mon_ball(m_idx, GF_WIND, get_dam(5 * spower / 2, 4), rad, TRUE);
			break;
		}

		/* RF5_BALL_STORM */
		case 128+11:
		{
			disturb(1, 0);

			if (spower < 30)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s gestures fluidly.", m_name);
				msg_print("You are surrounded by a little storm.");
				rad = 2;
			}
			else if (spower < 70)
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
			mon_ball(m_idx, GF_STORM, get_dam(4 * spower, 4), rad, TRUE);
			break;
		}

		/* RF5_BALL_NETHR */
		case 128+12:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s whispers nastily.", m_name);
				else msg_format("%^s casts an orb of nether.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs a deadly word.", m_name);
				else msg_format("%^s casts a nether ball.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s intones with deadly menace.", m_name);
				else msg_format("%^s calls up a storm of nether magics.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_NETHER, get_dam(3 * spower, 8), rad, FALSE);
			break;
		}

		/* RF5_BALL_CHAOS */
		case 128+13:
		{
			disturb(1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a sphere of chaos.", m_name);
				rad = 1;
			}
			else if (spower < 70)
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a ball of raw chaos.", m_name);
				rad = 2;
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s invokes a storm of chaos.", m_name);
				rad = 3;
			}
			mon_ball(m_idx, GF_CHAOS, get_dam(3 * spower, 4), rad, FALSE);
			break;
		}

		/* RF5_BALL_MANA */
		case 128+14:
		{
			disturb(1, 0);
			if (spower < 40)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a mana burst.", m_name);
				rad = 1;
			}
			else if (spower < 90)
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
			mon_ball(m_idx, GF_MANA, get_dam(2 * spower, 10), rad, FALSE);

			break;
		}

		/* RF5_XXX1 */
		case 128+15:
		{
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
			mon_bolt(m_idx, GF_ACID, get_dam(4 * spower, 6));
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
			mon_bolt(m_idx, GF_ELEC, get_dam(4 * spower, 6));
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
			mon_bolt(m_idx, GF_FIRE, get_dam(4 * spower, 6));
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
			mon_bolt(m_idx, GF_COLD, get_dam(4 * spower, 6));
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
			mon_bolt(m_idx, GF_POIS, get_dam(12 * spower / 5, 6));
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
			mon_bolt(m_idx, GF_PLASMA, get_dam(5 * spower, 6));
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
			mon_bolt(m_idx, GF_ICE, get_dam(5 * spower, 6));
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
			mon_bolt(m_idx, GF_WATER, get_dam(2 * spower, 8));
			break;
		}

		/* RF5_BOLT_NETHR */
		case 128+24:
		{
			disturb(1, 0);
			if (spower < 80)
			{
				if (blind) msg_format("%^s whispers nastily.", m_name);
				else msg_format("%^s casts a nether bolt.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs a deadly word.", m_name);
				else msg_format("%^s hurls a black orb at you.", m_name);
			}
			mon_bolt(m_idx, GF_NETHER, get_dam(5 * spower / 2, 8));
			break;
		}

		/* RF5_BOLT_MANA */
		case 128+25:
		{
			disturb(1, 0);
			if ((spower < 5) || (spower <= rlev / 5))
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts a magic missile.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts a mana bolt.", m_name);
			}
			mon_bolt(m_idx, GF_MANA, get_dam(2 * spower, 10));
			break;
		}

		/* RF5_BOLT_XXX1 */
		case 128+26:
		{
		}

		/* RF5_BEAM_ELEC */
		case 128+27:
		{
			disturb(1, 0);
			if (blind) msg_print("You feel a crackling in the air.");
			else msg_format("%^s shoots a spark of lightning at you.", m_name);

			mon_beam(m_idx, GF_ELEC, get_dam(5 * spower, 6), 6);
			break;
		}

		/* RF5_BEAM_ICE */
		case 128+28:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s casts an icy lance.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s murmurs deeply.", m_name);
				else msg_format("%^s casts an icy lance.", m_name);
			}
			mon_beam(m_idx, GF_ICE, get_dam(5 * spower, 6), 12);
			break;
		}

		/* RF5_BEAM_NETHR */
		case 128+29:
		{
			disturb(1, 0);
			if (spower < 30)
			{
				if (blind) msg_format("%^s whispers nastily.", m_name);
				else msg_format("%^s casts a beam of nether.", m_name);
			}
			else if (spower < 90)
			{
				if (blind) msg_format("%^s murmurs a deadly word.", m_name);
				else msg_format("%^s hurls a nether lance.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s intones with deadly menace.", m_name);
				else msg_format("%^s unleashes a ray of death.", m_name);
			}
			mon_beam(m_idx, GF_NETHER, get_dam(3 * spower, 12), 10);
			break;
		}

		/* RF5_ARC__HFIR */
		case 128+30:
		{
			disturb(1, 0);
			/* Must be powerful and have Udun-magic to get an arc. */
			if ((spower > 50) && (r_ptr->flags2 & (RF2_UDUN_MAGIC)))
			{
				if (blind) msg_format("%^s speaks a word of peril.", m_name);
				else msg_format("%^s invokes a storm of hellfire!", m_name);

				/* Absolutely formidable close up, less so at range. */
				mon_arc(m_idx, GF_HELLFIRE, FALSE, get_dam(7 * spower, 8), 6, 60);
			}

			/* For weak Udun-magic casters, a column of hellfire. */
			else if (r_ptr->flags2 & (RF2_UDUN_MAGIC))
			{
				if (blind) msg_format("%^s murmurs darkly.", m_name);
				else msg_format("%^s gestures, and you are enveloped in hellfire.", m_name);
				mon_ball(m_idx, GF_HELLFIRE, get_dam(5 * spower, 8), 0, TRUE);
			}

			/* Column of fire for everyone else. */
			else
			{
				if (blind) msg_format("%^s mutters.", m_name);
				else msg_format("%^s gestures, and you are enveloped in fire.", m_name);
				mon_ball(m_idx, GF_FIRE, get_dam(5 * spower, 8), 0, TRUE);

			}
			break;
		}

		/* RF5_ARC__WALL */
		case 128+31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mutters.", m_name);
			else msg_format("%^s calls up a wall of force.", m_name);
			mon_arc(m_idx, GF_FORCE, FALSE, get_dam(3 * spower, 18), 8, 60);
			break;
		}

		/* RF6_HASTE */
		case 160+0:
		{
			disturb(1, 0);
			if (blind)
			{
				msg_format("%^s mumbles.", m_name);
			}
			else
			{
				msg_format("%^s concentrates on %s body.", m_name, m_poss);
			}

			/* Allow a quick speed increase if not already greatly hasted. */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				msg_format("%^s starts moving faster.", m_name);
				m_ptr->mspeed += 10;
			}

			/* Allow small speed increases to base + 20 */
			else if (m_ptr->mspeed < r_ptr->speed + 20)
			{
				msg_format("%^s starts moving slightly faster.", m_name);
				m_ptr->mspeed += 2;
			}

			break;
		}

		/* RF6_ADD_MANA */
		case 160+1:
		{
			disturb(1, 0);

			if (m_ptr->ml)
			{
				if (blind)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s gathers %s power.", m_name, m_poss);
				}
			}

			/* Increase current mana.  Do not exceed maximum. */
			m_ptr->mana += (spower / 20) + 5;
			if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
			int gain, cost;

			disturb(1, 0);

			/* Message */
			if (m_ptr->ml)
			{
				if (blind)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else
				{
					msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
				}
			}

			/* We regain lost hitpoints (up to spower * 5) */
			gain = MIN(m_ptr->maxhp - m_ptr->hp, spower * 5);

			/* We do not gain more than mana * 20 HPs at a time */
			gain = MIN(gain, m_ptr->mana * 20);

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
				if (m_ptr->ml)
				{
					if (seen) msg_format("%^s looks very healthy!",  m_name);
					else      msg_format("%^s sounds very healthy!", m_name);
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (m_ptr->ml)
				{
					if (seen) msg_format("%^s looks healthier.",  m_name);
					else      msg_format("%^s sounds healthier.", m_name);
				}
			}


			/* Redraw (later) if needed */
			if ((p_ptr->health_who == m_idx) && (m_ptr->ml))
				p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				set_mon_fear(m_ptr, 0, FALSE);

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s recovers %s courage.", m_name, m_poss);
			}

			/* Recalculate combat range later */
			m_ptr->min_range = 0;

			break;
		}

		/* RF6_CURE */
		case 160+3:
		{
			if (m_ptr->ml)
				msg_format("%^s concentrates on %s ailments.", m_name, m_poss);

			/* Cancel stunning */
			if (m_ptr->stunned)
			{
				/* Cancel stunning */
				m_ptr->stunned = 0;

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s is no longer stunned.", m_name);
			}

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				set_mon_fear(m_ptr, 0, FALSE);

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s recovers %s courage.", m_name, m_poss);

			}

			/* Cancel (major) slowing */
			if (m_ptr->mspeed < r_ptr->speed - 5)
			{
				/* Cancel slowing */
				m_ptr->mspeed = r_ptr->speed;

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s is no longer slowed.", m_name);
			}

			/* Cancel Black Breath sometimes. */
			if ((m_ptr->black_breath) && (rlev + 20 > rand_int(120)))
			{
				/* Cancel Black Breath */
				m_ptr->black_breath = 0;

				/* Message */
				if (m_ptr->ml)
					msg_format("The hold of the Black Breath on %s is broken.", m_name);
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			disturb(1, 0);
			if (m_ptr->ml) msg_format("%^s blinks away.", m_name);
			teleport_away(m_idx, 10);
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			disturb(1, 0);
			if (m_ptr->ml) msg_format("%^s teleports away.", m_name);
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);

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

			/* Move monster near player (also updates "m_ptr->ml"). */
			teleport_towards(m_ptr->fy, m_ptr->fx, py, px);

			/* Monster is now visible, but wasn't before. */
			if ((!seen) && (m_ptr->ml))
			{
				/* Get the name (using "A"/"An") again. */
				monster_desc(ddesc, m_ptr, 0x08);

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
				if (distance(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px) <
				    old_cdis - 1)
				{
					msg_format("%^s blinks toward you.", m_name);
				}
				else
				{
					msg_format("%^s blinks around you.", m_name);
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
			teleport_player_to(m_ptr->fy, m_ptr->fx, 0, TRUE);
			seen = TRUE;
			break;
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			teleport_player(100, TRUE);
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
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
			(void)unlite_area(0, 3);
			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
			else msg_format("%^s casts a spell and cackles evilly.", m_name);
			(void)trap_creation(py, px);
			break;
		}

		/* RF6_FORGET */
		case 160+14:
		{
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				(void)lose_all_info("Your memories fade away.");
			}
			break;
		}

		/* RF6_DRAIN_MANA */
		case 160+15:
		{
			if (p_ptr->csp)
			{
				int r1;
				int resist = get_skill(S_WIZARDRY, 2, 5);

				/* Attack power */
				r1 = (rand_spread(spower, spower / 2) / resist) + 2;

				/* Allow saving throw for wizard-protected characters */
				if ((p_ptr->wiz_prot) && (!one_in_(3))) r1 = 0;

				/* Disturb if legal */
				disturb(1, 0);

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

				/* Note if anything happened */
				if (r1)
				{
					msg_format("%^s draws psychic energy from you!", m_name);
				}

				/* Note immunity */
				else if (p_ptr->csp)
				{
					msg_format("%^s tries to draw psychic energy from you, but fails!", m_name);
					break;
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

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
				}

				/* Heal the monster with remaining energy */
				if ((m_ptr->hp < m_ptr->maxhp) && (r1))
				{
					/* Heal */
					m_ptr->hp += (30 * (r1 + 1));
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

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

		/* RF6_CURSE */
		case 160+17:
		{
			disturb(1, 0);

			/* Weaker monsters can't curse characters with strong minds */
			if (p_ptr->skill_sav > rand_int(r_ptr->level * 2))
			{
				if (seen) msg_format("%^s attempts to curse you, but nothing happens.", m_name);
				break;
			}

			/* Message */
			if (!seen)
			{
				msg_print("You hear curses being directed at you.");
			}
			else
			{
				msg_format("%^s curses you.", m_name);
			}

			/* Cursing a player can have all sorts of effects */
			while (TRUE)
			{
				int choice = randint(100);

				/* Drain away luck (30%) */
				if (choice <= 30)
				{
					(void)set_luck(p_ptr->luck - (2 + spower / 12),
						"You feel strangely unlucky...");
				}

				/* Reduce any stat (10%) */
				else if (choice <= 40)
				{
					/* Damage a stat if magic is not very weak */
					if (spower >= 5)
					{
						(void)do_dec_stat(rand_int(A_MAX), 1, FALSE, NULL, NULL);
					}
				}

				/* Cancel any of various enhancement magics (20%) */
				else if (choice <= 60)
				{
					/* Randomly look for active magics */
					for (i = 0; i < 3; i++)
					{
						int choice2 = randint(2 + MIN(spower / 12, 4));

						if ((choice2 == 6) && (p_ptr->wiz_prot))
						{
							set_wiz_prot(0);   break;
						}
						else if ((choice2 == 6) && (p_ptr->wraithform))
						{
							set_wraithform(0);   break;
						}
						else if ((choice2 == 5) && (p_ptr->holy))
						{
							set_holy(0);   break;
						}
						else if ((choice2 == 5) &&
						         ((p_ptr->shield) || (p_ptr->steelskin)))
						{
							set_shield(0, NULL);
							set_steelskin(0, NULL);   break;
						}
						else if ((choice2 == 4) && (p_ptr->forbid_summoning))
						{
							set_forbid_summoning(0);   break;
						}
						else if ((choice2 == 4) && (p_ptr->tim_invis))
						{
							set_invis(0, 0);   break;
						}
						else if ((choice2 == 3) && (p_ptr->protevil))
						{
							set_protevil(0);   break;
						}
						else if ((choice2 == 3) && (p_ptr->fast))
						{
							set_fast(0);   break;
						}
						else if ((choice2 == 2) && (p_ptr->hero))
						{
							set_hero(0);   break;
						}
						else if ((choice2 == 2) && (p_ptr->blessed))
						{
							set_blessed(0, NULL);   break;
						}
						else if ((choice2 <= 1) && (p_ptr->bold))
						{
							set_bold(0);   break;
						}
					}
				}

				/* Cause disease (5%) */
				else if (choice <= 65)
				{
					/* Damage depends on spell power */
					int dam = spower * 2;

					/* Cause disease */
					disease(&dam);

					/* Hurt (not very much) */
					take_hit(dam / 2, 0, NULL, ddesc);
				}

				/* Suck away some exp (10%) */
				else if (choice <= 75)
				{
					s32b d = (spower * spower);

					/* Drain life */
					if (p_ptr->hold_life)
					{
						if (d >= 20)
						{
							msg_print("You feel your life slipping away!");
							lose_exp(d / 10, FALSE);
						}
					}
					else
					{
						msg_print("You feel your life draining away!");
						lose_exp(d, FALSE);
					}
				}

				/* Curse an item in equipment (25%) */
				else
				{
					curse_equipment(MIN(spower, 100));
				}


				/* Usually, but not always, stop after one effect */
				if (spower < randint(100 + spower)) break;
			}

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

			/* Project a mental attack */
			project(m_idx, 0, p_ptr->py, p_ptr->px, p_ptr->py, p_ptr->px,
			       5 + spower, GF_PSI,
			       PROJECT_HIDE | PROJECT_KILL | PROJECT_PLAY, 0, 0);

			break;
		}

		/* RF6_XXX7 */
		case 160+19:
		{
			break;
		}

		/* RF6_WOUND */
		case 160+20:
		{
			disturb(1, 0);

			if (spower < 7)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at you and mumbles angrily.", m_name);
				k = 1;
			}
			else if (spower < 15)
			{
				if (blind) msg_format("%^s mumbles deeply.", m_name);
				else msg_format("%^s points at you and intones horribly.", m_name);
				k = 2;
			}
			else if (spower < 30)
			{
				if (blind) msg_format("%^s murmurs loudly.", m_name);
				else msg_format("%^s points at you, incanting terribly.", m_name);
				k = 3;
			}
			else if (spower < 55)
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

			if (rand_int(rlev / 2 + 70) < p_ptr->skill_sav)
			{
				msg_format("You resist the effects%c",
				      (spower < 30 ?  '.' : '!'));
			}
			else
			{
				/* Inflict damage. */
				take_hit(get_dam(5 * spower / 2, 6), 0, NULL, ddesc);

				/* Cut the player depending on strength of spell. */
				if (k == 1) (void)set_cut(p_ptr->cut + 8 + damroll(2, 4));
				if (k == 2) (void)set_cut(p_ptr->cut + 23 + damroll(3, 8));
				if (k == 3) (void)set_cut(p_ptr->cut + 46 + damroll(4, 12));
				if (k == 4) (void)set_cut(p_ptr->cut + 95 + damroll(8, 15));
				if (k == 5) (void)set_cut(1200);
			}
			break;
		}

		/* RF6_HELLDARK */
		case 160+21:
		{
			int dam;

			if (blind) msg_format("%^s mumbles.", m_name);
			else
			{
				if (spower < 25)
					msg_format("%^s calls upon the forces of darkness.", m_name);
				else
					msg_format("%^s summons forth dark forces from Hell!", m_name);
			}

			unlite_area(10, 3);

			/* Damage can be quite high if you don't resist darkness */
			dam = spower * 5;
			if ((p_ptr->resist_dark) || (p_ptr->oppose_ethereal)) dam /= 2;

			/* Resist or panic */
			if (rand_int(100) < p_ptr->skill_sav)
			{
				dam /= 2;
				if (spower < 25) msg_print("You resist!");
				else msg_print("You resist the terrors of Hell!");
			}
			else
			{
				if (spower < 25)
				{
					if ((!p_ptr->resist_dark) && (!p_ptr->oppose_ethereal))
						msg_print("The darkness envelops you!");
				}
				else
					msg_print("You succumb to the visions of Hell!");

				/* Very frightening */
				if ((!p_ptr->resist_fear) &&
				    (randint(200) > p_ptr->skill_sav))
				{
					(void)set_afraid(p_ptr->afraid + randint(30) + r_ptr->level * 2);
				}
			}

			mon_ball(m_idx, GF_MISSILE, dam, 4, TRUE);
			update_smart_learn(m_idx, LRN_DARK_SAVE);
			break;

		}

		/* RF6_HOLY_SMITE */
		case 160+22:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s chants.", m_name);
				else msg_format("%^s calls down a holy lance.", m_name);
			}
			else
			{
				if (blind) msg_format("%^s chants powerfully.", m_name);
				else msg_format("%^s calls down a holy lance to smite you!", m_name);
			}

			mon_ball(m_idx, GF_HOLY_ORB, get_dam(4 * spower, 16), 0, TRUE);
			update_smart_learn(m_idx, LRN_HOLY_SAVE);
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
			/* Try a saving throw */
			if (randint(100) > p_ptr->skill_sav)
			{
				int resist = 1;

				/* Allow resistance */
				if (p_ptr->slow_digest) resist++;

				/* Reduce food counter, but don't bash the player too hard. */
				set_food(p_ptr->food -
					MIN(1000 + p_ptr->food / 4, p_ptr->food / resist));

				if (blind) msg_print("You suddenly feel hungry.");
				else msg_format("%^s gestures at you, and you suddenly feel hungry.", m_name);

				if ((resist > 1) && (!blind) && (p_ptr->food > p_ptr->food_hungry))
					msg_print("You resist the effects!");
			}

			else if (!blind)
			{
				msg_format("%^s gestures at you, but nothing seems to happen.",
					m_name);
			}

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
				(void)set_afraid(p_ptr->afraid + rand_range(5, 10));
			}
			break;
		}

		/* RF6_BLIND */
		case 160+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);

			/* Must not already be blind */
			else if (!p_ptr->blind)
			{
				msg_format("%^s casts a spell, burning your eyes!", m_name);
				if (p_ptr->resist_blind)
				{
					msg_print("You are unaffected!");
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You blink, and your vision clears.");
				}
				else
				{
					(void)set_blind(p_ptr->blind + rand_range(5, 10),
						"You go blind!");
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
			if (p_ptr->resist_confu)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You disbelieve the feeble spell.");
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			break;
		}

		/* RF6_SLOW */
		case 160+30:
		{
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
				(void)set_slow(p_ptr->slow + rand_range(6, 12));
			}
			break;
		}

		/* RF6_HOLD */
		case 160+31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s stares deep into your eyes!", m_name);
			if (p_ptr->free_act)
			{
				if (!p_ptr->paralyzed) msg_print("You are unaffected!");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				if (!p_ptr->paralyzed) msg_print("You stare back unafraid!");
			}

			/* Must not already be paralyzed */
			else if (!p_ptr->paralyzed)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_range(4, 8));
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
			for (k = 0; k < (rlev > 40 ? 4 : 3); k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_KIN);
			}

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
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE, summon_lev, 0);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}

		/* RF7_S_MONSTERS */
		case 192 + 4:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE, summon_lev, 0);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF7_XXX */
		case 192 + 5:  break;
		case 192 + 6:  break;

		/* RF7_S_BEETLE */
		case 192 + 7:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a giant beetle.", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_BEETLE);
			}
			if (blind && count) msg_print("You hear chittering and skittering.");
			break;
		}


		/* RF7_S_ANT */
		case 192 + 8:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ants.", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_ANT);
			}
			if (blind && count) msg_print("You hear chittering and skittering.");
			break;
		}

		/* RF7_S_SPIDER */
		case 192 + 9:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons spiders.", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_SPIDER);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}

		/* RF7_S_HOUND */
		case 192 + 10:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hounds.", m_name);
			for (k = 0; k < 2; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_HOUND);
			}
			if (blind && count) msg_print("You hear snarling.");
			break;
		}

		/* RF7_S_ANIMAL */
		case 192 + 11:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons natural creatures.", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_ANIMAL);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}


		/* RF7_XXX */
		case 192 + 12:	break;
		case 192 + 13:	break;

		/* RF7_S_THIEF */
		case 192 + 14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s whistles.", m_name);
			else msg_format("%^s whistles up a den of thieves!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_THIEF);
			}
			if (blind && count) msg_print("You hear footsteps and the whetting of knives.");
			break;
		}

		/* RF7_S_BERTBILLTOM */
		case 192 + 15:
		{
			/* Summon Bert, Bill, and Tom */
			for (k = 0; k < 2; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev + 5, SUMMON_BERTBILLTOM);
			}

			/* No messages unless successful */
			if (count)
			{
				if (blind) msg_print("Heavy footsteps approach!");
				else       msg_format("%^s calls up his friends!", m_name);
			}
			break;
		}

		/* RF7_S_ORC */
		case 192 + 16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s howls.", m_name);
			else msg_format("%^s howls for help.", m_name);

			/* Orc appears, usually at some distance, rarely in a group */
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_ORC);
			}
			break;
		}

		/* RF6_S_ANGEL */
		case 192 + 17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an angel!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE, summon_lev, SUMMON_ANGEL);
			}
			if (blind && count) msg_print("You hear a heavenly choir sing.");
			break;
		}

		case 192 + 18:	break;
		case 192 + 19:	break;

		/* RF7_S_DRAGON */
		case 192 + 20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a dragon!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_DRAGON);
			}
			if (blind && count) msg_print("You feel something breathing on you...");
			break;
		}

		/* RF7_S_HI_DRAGON */
		case 192 + 21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons ancient dragons!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_HI_DRAGON);
			}
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
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons a hellish adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE, summon_lev, SUMMON_DEMON);
			}
			if (blind && count)
			{
				msg_print("You smell fire and brimstone.");
			}
			break;
		}

		/* RF7_S_HI_DEMON */
		case 192 + 25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater demons!", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE, summon_lev,
					SUMMON_HI_DEMON);
			}
			if (blind && count)
			{
				msg_print("You hear many evil things appear nearby.");
			}
			break;
		}

		/* RF7_XXX */
		case 192 + 26:	break;
		case 192 + 27:	break;

		/* RF7_S_UNDEAD */
		case 192 + 28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons an undead adversary!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_UNDEAD);
			}
			if (blind && count) msg_print("You hear something creepy appear nearby.");
			break;
		}

		/* RF7_S_HI_UNDEAD */
		case 192 + 29:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater undead!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* Summon the Ringwraiths */
		/* RF7_S_WRAITH */
		case 192 + 30:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons mighty undead opponents!", m_name);
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_WRAITH);
			}
			for (k = 0; (k < 6) && (count < 6); k++)
			{
				count += summon_specific(y, x, FALSE,
							 summon_lev, SUMMON_HI_UNDEAD);
			}
			if (blind && count)
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* Summon Uniques */
		/* RF7_S_UNIQUE */
		case 192 + 31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);

			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, FALSE,
					summon_lev, SUMMON_UNIQUE);
			}
			if (count)
			{
				if (blind) msg_print("You've got a bad feeling about this...");
				else       msg_format("%^s magically summons legendary opponents!", m_name);
			}
			else
			{
				if (!blind)
					msg_format("%^s gestures imperiously ... and looks puzzled for a moment.",
						m_name);
			}
			break;
		}

		/* Paranoia */
		default:
		{
			msg_print("A monster tried to cast a spell that has not yet been defined.");
		}
	}

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
			l_ptr->flags4 |= (1L << (attack - 32*3));
			if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Bolt or Ball */
		else if (attack < 32*5)
		{
			l_ptr->flags5 |= (1L << (attack - 32*4));
			if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Special spell */
		else if (attack < 32*6)
		{
			l_ptr->flags6 |= (1L << (attack - 32*5));
			if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Summon spell */
		else if (attack < 32*7)
		{
			l_ptr->flags7 |= (1L << (attack - 32*6));
			if (l_ptr->ranged < MAX_UCHAR) l_ptr->ranged++;
		}

		/* Remember special flags */
		if (r_ptr->flags2 & (RF2_ARCHER)) l_ptr->flags2 |= RF2_ARCHER;
		if (r_ptr->flags2 & (RF2_MORGUL_MAGIC)) l_ptr->flags2 |= RF2_MORGUL_MAGIC;
		if (r_ptr->flags2 & (RF2_UDUN_MAGIC)) l_ptr->flags2 |= RF2_UDUN_MAGIC;
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


/*
 * Some monsters are surrounded by gas, terrible heat, loud noises, spores,
 * etc.  Process any such affects.
 *
 * The Nazgul surround themselves with darkness, so they all have IS_LIT.
 */
void cloud_surround(int r_idx, int *typ, int *dam, int *rad)
{
	monster_race *r_ptr = &r_info[r_idx];

	*typ = 0;
	*dam = div_round(r_ptr->level, 4);
	*rad = 1 + r_ptr->level / 60;

	/* Unique monsters have especially strong effects */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		*rad += 1;
		*dam *= 2;
	}

	/*** Determine the kind of cloud we're supposed to be giving off ***/

	/* If breaths and attrs match, the choice is clear. */
	if (r_ptr->flags4)
	{
		/* This is mostly for the dragons (maybe vortexes?) */
		if      ((r_ptr->flags4 & (RF4_BRTH_POIS)) &&
		         (r_ptr->d_attr == TERM_GREEN)) *typ = GF_POIS;
		else if ((r_ptr->flags4 & (RF4_BRTH_ELEC)) &&
		         (r_ptr->d_attr == TERM_BLUE)) *typ = GF_ELEC;
		else if ((r_ptr->flags4 & (RF4_BRTH_ACID)) &&
		         ((r_ptr->d_attr == TERM_SLATE) ||
		          (r_ptr->d_attr == TERM_L_DARK)))
		         *typ = GF_ACID;
		else if ((r_ptr->flags4 & (RF4_BRTH_FIRE)) &&
		         ((r_ptr->d_attr == TERM_RED) ||
		          (r_ptr->d_attr == TERM_L_RED)))
		         *typ = GF_FIRE;
		else if ((r_ptr->flags4 & (RF4_BRTH_COLD)) &&
		         ((r_ptr->d_attr == TERM_WHITE) ||
		          (r_ptr->d_attr == TERM_L_WHITE)))
		         *typ = GF_COLD;
		else if ((r_ptr->flags4 & (RF4_BRTH_SOUND)) &&
		         (r_ptr->d_attr == TERM_YELLOW)) *typ = GF_SOUND;
		else if ((r_ptr->flags4 & (RF4_BRTH_CONFU)) &&
		         (r_ptr->d_attr == TERM_L_UMBER)) *typ = GF_CONFUSION;
	}

	/* Molds release spores */
	if ((!typ) && (r_ptr->d_char == 'm'))
	{
		*typ = GF_SPORE;
		*dam *= 4;
	}

	/* The Nazgul darken everything nearby (so do silver jellies) */
	if ((!typ) && ((r_ptr->d_char == 'W') || (r_ptr->d_char == 'j')))
	{
		*typ = GF_DARK_WEAK;
		*rad = 4;
	}

	/* Leave the rest blank until we make monsters that need it. */
}
