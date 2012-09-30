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
static int monster_critical(int dice, int sides, int dam)
{
	int max = 0;
	int total = dice * sides;

	/* Luck isn't always good for you... */
	if (FLAG(p_ptr, TR_STRANGE_LUCK))
		dam = dam * 3 / 2;

	/* Must do at least 95% of perfect */
	if (dam < total * 19 / 20) return (0);

	/* Weak blows rarely work */
	if ((dam < 20) && (randint0(100) >= dam)) return (0);

	/* Perfect damage */
	if (dam == total) max++;

	/* Super-charge */
	if (dam >= 20)
	{
		while (one_in_(50)) max++;
	}

	/* Critical damage */
	if (dam > 160) return (6 + max);
	if (dam > 80) return (5 + max);
	if (dam > 40) return (4 + max);
	if (dam > 20) return (3 + max);
	if (dam > 10) return (2 + max);
	return (1 + max);
}


/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level)
{
	int i, k, ac;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint1(i) > ((ac * 3) / 4))) return (TRUE);

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
 * The monster wants to flee so print a message.
 */
void flee_message(cptr m_name, u16b r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	
	/* Immobile monsters can never flee */
	if (FLAG(r_ptr, RF_NEVER_MOVE)) return;
	
	/* Sound */
	sound(SOUND_FLEE);

	/* Message */
	msgf(MSGT_FLEE, "%^s flees in terror!", m_name);
	msg_effect(MSG_FLEE, r_idx);
}


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int ap_cnt;

	int k, tmp, ac, rlev;
	bool do_cut, do_stun;

	s32b gold;

	object_type *o_ptr;

	char m_name[80];

	char ddesc[80];

	bool blinked;
	bool touched = FALSE, fear = FALSE, alive = TRUE;
	bool explode = FALSE;
	bool resist_drain = FALSE;

	/* Save visibility */
	bool visible = m_ptr->ml;

	/* Not allowed to attack */
	if (FLAG(r_ptr, RF_NEVER_BLOW)) return (FALSE);

	/* ...nor if friendly */
	if (!is_hostile(m_ptr)) return FALSE;

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->hdice * 2 >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0, 80);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88, 80);


	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;

		/* Stop attacking if the aggressor dies (fire sheath etc.) */
		if (!alive) break;

		/* Hack -- no more attacks */
		if (!method) break;

		/* Stop if player is dead or gone */
		if (!p_ptr->state.playing || p_ptr->state.is_dead) break;

		/* Handle "leaving" */
		if (p_ptr->state.leaving) break;

		/* Extract the attack "power" */
		switch (effect)
		{
			case RBE_HURT:
			{
				power = 60;
				break;
			}
			case RBE_POISON:
			{
				power = 5;
				break;
			}
			case RBE_UN_BONUS:
			{
				power = 20;
				break;
			}
			case RBE_UN_POWER:
			{
				power = 15;
				break;
			}
			case RBE_EAT_GOLD:
			{
				power = 5;
				break;
			}
			case RBE_EAT_ITEM:
			{
				power = 5;
				break;
			}
			case RBE_EAT_FOOD:
			{
				power = 5;
				break;
			}
			case RBE_EAT_LITE:
			{
				power = 5;
				break;
			}
			case RBE_ACID:
			{
				power = 0;
				break;
			}
			case RBE_ELEC:
			{
				power = 10;
				break;
			}
			case RBE_FIRE:
			{
				power = 10;
				break;
			}
			case RBE_COLD:
			{
				power = 10;
				break;
			}
			case RBE_BLIND:
			{
				power = 2;
				break;
			}
			case RBE_CONFUSE:
			{
				power = 10;
				break;
			}
			case RBE_TERRIFY:
			{
				power = 10;
				break;
			}
			case RBE_PARALYZE:
			{
				power = 2;
				break;
			}
			case RBE_LOSE_STR:
			{
				power = 0;
				break;
			}
			case RBE_LOSE_DEX:
			{
				power = 0;
				break;
			}
			case RBE_LOSE_CON:
			{
				power = 0;
				break;
			}
			case RBE_LOSE_INT:
			{
				power = 0;
				break;
			}
			case RBE_LOSE_WIS:
			{
				power = 0;
				break;
			}
			case RBE_LOSE_CHR:
			{
				power = 0;
				break;
			}
			case RBE_LOSE_ALL:
			{
				power = 2;
				break;
			}
			case RBE_SHATTER:
			{
				power = 60;
				break;
			}
			case RBE_EXP_10:
			{
				power = 5;
				break;
			}
			case RBE_EXP_20:
			{
				power = 5;
				break;
			}
			case RBE_EXP_40:
			{
				power = 5;
				break;
			}
			case RBE_EXP_80:
			{
				power = 5;
				break;
			}
			case RBE_DISEASE:
			{
				power = 5;
				break;
			}
			case RBE_TIME:
			{
				power = 5;
				break;
			}
			case RBE_EXP_VAMP:
			{
				power = 5;
				break;
			}
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			int protect = 0;
			
			/* Always disturbing */
			disturb(TRUE);

			if (FLAG(p_ptr, TR_SLAY_DRAGON) &&
					FLAG(r_ptr, RF_DRAGON))
				protect = 5;
			else if (FLAG(p_ptr, TR_SLAY_DEMON) &&
					FLAG(r_ptr, RF_DEMON))
				protect = 5;
			else if (FLAG(p_ptr, TR_SLAY_UNDEAD) &&
					FLAG(r_ptr, RF_UNDEAD))
				protect = 4;
			else if (FLAG(p_ptr, TR_SLAY_ORC) &&
					FLAG(r_ptr, RF_ORC))
				protect = 4;
			else if (FLAG(p_ptr, TR_SLAY_TROLL) &&
					FLAG(r_ptr, RF_TROLL))
				protect = 4;
			else if (FLAG(p_ptr, TR_SLAY_GIANT) &&
					FLAG(r_ptr, RF_GIANT))
				protect = 4;
			else if (FLAG(p_ptr, TR_SLAY_ANIMAL) &&
					FLAG(r_ptr, RF_ANIMAL))
				protect = 3;
			else if (FLAG(p_ptr, TR_SLAY_EVIL) &&
					FLAG(r_ptr, RF_EVIL))
				protect = 3;
			else if ((p_ptr->tim.protevil > 0) &&
					FLAG(r_ptr, RF_EVIL))
				protect = 3;

			/* Protection gets stronger with experience */
			protect *= p_ptr->lev + 20;

			/* Apply "protection" */
			if (protect > 0 && randint1(protect) > 2 * (rlev + 20))
			{
#if 0
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					r_ptr->r_flags[2] |= RF2_EVIL;
				}
#endif

				/* Message */
				msgf("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}


			/* Get action */
			act = format(rbm_info[method].action, "you");

			/* Get flag status */
			touched = rbm_info[method].touched;
			do_cut = rbm_info[method].cut;
			do_stun = rbm_info[method].stun;

			/* Play the sound */
			if (rbm_info[method].sound)
			{
				sound(rbm_info[method].sound);
			}

			/* Special cases */
			if (method == RBM_EXPLODE)
			{
				explode = TRUE;
			}
			else if (method == RBM_INSULT)
			{
				act = desc_insult[randint0(8)];
			}
			else if (method == RBM_MOAN)
			{
				act = desc_moan[randint0(4)];
			}
			else if (method == RBM_SHOW)
			{
				if (one_in_(3))
				{
					act = "sings 'We are a happy family.'";
				}
				else
				{
					act = "sings 'I love you, you love me.'";
				}
			}

			/* Message */
			if (act)
			{
				if ((p_ptr->tim.image) && one_in_(3))
				{
					msgf("%^s %s you.", m_name,
							   silly_attacks[randint0(MAX_SILLY_ATTACK)]);
				}
				else
					msgf("%^s %s", m_name, act);
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

			/*
			 * Skip the effect when exploding, since the explosion
			 * already causes the effect.
			 */
			if (!explode)
			{
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
						/* Message */
						msgf("You receive a dose of poison!");

						/* Special damage */
						obvious = pois_dam(damage, ddesc, randint1(rlev) + 5);
						
						/* Learn about the player */
						update_smart_learn(m_idx, DRS_POIS);

						break;
					}

					case RBE_UN_BONUS:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Allow complete resist */
						if (!(FLAG(p_ptr, TR_RES_DISEN)))
						{
							/* Apply disenchantment */
							if (apply_disenchant()) obvious = TRUE;
						}

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_DISEN);

						break;
					}

					case RBE_UN_POWER:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Find an item */
						OBJ_ITT_START (p_ptr->inventory, o_ptr)
						{
							/* Only work some of the time */
							if (one_in_(2)) continue;

							/* Drain charged wands/staffs */
							if (((o_ptr->tval == TV_STAFF) ||
								 (o_ptr->tval == TV_WAND)) && (o_ptr->pval))
							{
								/* Calculate healed hitpoints */
								int heal = rlev * o_ptr->pval * o_ptr->number;

								/* Don't heal more than max hp */
								heal = MIN(heal, m_ptr->maxhp - m_ptr->hp);

								/* Message */
								msgf("Energy drains from your pack!");

								/* Obvious */
								obvious = TRUE;

								/* Heal the monster */
								m_ptr->hp += heal;

								/* Redraw (later) if needed */
								if (p_ptr->health_who == m_idx) p_ptr->redraw |=
										(PR_HEALTH);

								/* Uncharge */
								if (o_ptr->tval == TV_WAND)
								{
									o_ptr->ac += o_ptr->pval;
								}
								o_ptr->pval = 0;
								
								/* Notice changes */
								notice_inven();

								/* Done */
								break;
							}
						}
						OBJ_ITT_END;

						break;
					}

					case RBE_EAT_GOLD:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Confused monsters cannot steal successfully. -LM- */
						if (m_ptr->confused) break;

						/* Obvious */
						obvious = TRUE;

						/* Saving throw (unless paralyzed) based on dex and level */
						if (!p_ptr->tim.paralyzed &&
							(randint0(100) <
							 (adj_dex_safe[p_ptr->stat[A_DEX].ind] +
							  p_ptr->lev)))
						{
							/* Saving throw message */
							msgf("You quickly protect your money pouch!");

							/* Occasional blink anyway */
							if (!one_in_(3)) blinked = TRUE;
						}

						/* Eat gold */
						else
						{
							gold = (p_ptr->au / 10) + randint1(25);
							if (gold < 2) gold = 2;
							if (gold > 5000) gold =
									(p_ptr->au / 20) + randint1(3000);
							if (gold > p_ptr->au) gold = p_ptr->au;
							p_ptr->au -= gold;
							if (gold <= 0)
							{
								msgf("Nothing was stolen.");
							}
							else if (p_ptr->au)
							{
								msgf("Your purse feels lighter.");
								msgf("%ld coins were stolen!",
										   (long)gold);

								chg_virtue(V_SACRIFICE, 1);
							}
							else
							{
								msgf("Your purse feels lighter.");
								msgf("All of your coins were stolen!");

								chg_virtue(V_SACRIFICE, 2);
							}

							/* Redraw gold */
							p_ptr->redraw |= (PR_GOLD);

							/* Window stuff */
							p_ptr->window |= (PW_PLAYER);

							/* Blink away */
							blinked = TRUE;
						}

						break;
					}

					case RBE_EAT_ITEM:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Confused monsters cannot steal successfully. -LM- */
						if (m_ptr->confused) break;

						/* Saving throw (unless paralyzed) based on dex and level */
						if (!p_ptr->tim.paralyzed &&
							(randint0(100) <
							 (adj_dex_safe[p_ptr->stat[A_DEX].ind] +
							  p_ptr->lev)))
						{
							/* Saving throw message */
							msgf("You grab hold of your backpack!");

							/* Occasional "blink" anyway */
							blinked = TRUE;

							/* Obvious */
							obvious = TRUE;

							/* Done */
							break;
						}

						/* Find an item */
						OBJ_ITT_START (p_ptr->inventory, o_ptr)
						{
							/* Only some of the time */
							if (!one_in_(INVEN_PACK)) continue;

							/* Skip artifacts */
							if (FLAG(o_ptr, TR_INSTA_ART)) continue;

							/* Message */
							msgf("%sour %v was stolen!",
									   ((o_ptr->number > 1) ? "One of y" : "Y"),
									   OBJECT_FMT(o_ptr, FALSE, 3));

							chg_virtue(V_SACRIFICE, 1);

							/* Split object */
							o_ptr = item_split(o_ptr, 1);

							/* Forget mark */
							o_ptr->info &= ~(OB_SEEN);

							/* Give to the monster */
							o_ptr = add_object_list(&m_ptr->hold_o_idx, o_ptr);

							/* Obvious */
							obvious = TRUE;

							/* Blink away */
							blinked = TRUE;

							/* Done */
							break;
						}
						OBJ_ITT_END;


						break;
					}

					case RBE_EAT_FOOD:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Steal some food */
						OBJ_ITT_START (p_ptr->inventory, o_ptr)
						{
							/* Pick an item from the pack */
							if (!one_in_(INVEN_PACK)) continue;

							/* Skip non-food objects */
							if (o_ptr->tval != TV_FOOD) continue;

							/* Message */
							msgf("%sour %v was eaten!",
									   ((o_ptr->number > 1) ? "One of y" : "Y"),
									   OBJECT_FMT(o_ptr, FALSE, 0));

							/* Steal the items */
							item_increase(o_ptr, -1);

							/* Obvious */
							obvious = TRUE;

							/* Done */
							break;
						}
						OBJ_ITT_END;

						break;
					}

					case RBE_EAT_LITE:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Access the lite */
						o_ptr = &p_ptr->equipment[EQUIP_LITE];

						/* Drain fuel */
						if ((o_ptr->pval > 0) &&
							(!(FLAG(o_ptr, TR_INSTA_ART))))
						{
							/* Reduce fuel */
							o_ptr->pval -= (s16b)rand_range(250, 500);
							if (o_ptr->pval < 1) o_ptr->pval = 1;

							/* Notice */
							if (!p_ptr->tim.blind)
							{
								msgf("Your light dims.");
								obvious = TRUE;
							}
							
							/* Notice changes */
							notice_equip();
						}

						break;
					}

					case RBE_ACID:
					{
						/* Message */
						msgf("You are covered in acid!");

						/* Special damage */
						obvious = acid_dam(damage, ddesc);

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_ACID);

						break;
					}

					case RBE_ELEC:
					{
						/* Message */
						msgf("You are struck by electricity!");

						/* Special damage */
						obvious = elec_dam(damage, ddesc);

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_ELEC);

						break;
					}

					case RBE_FIRE:
					{
						/* Message */
						msgf("You are enveloped in flames!");

						/* Special damage */
						obvious = fire_dam(damage, ddesc);

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_FIRE);

						break;
					}

					case RBE_COLD:
					{
						/* Message */
						msgf("You are covered with frost!");

						/* Special damage */
						obvious = cold_dam(damage, ddesc);

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_COLD);

						break;
					}

					case RBE_BLIND:
					{
						/* Take damage */
						take_hit(damage, ddesc);

						/* Increase "blind" */
						if (!(FLAG(p_ptr, TR_RES_BLIND)))
						{
							if (inc_blind(10 + randint1(rlev)))
							{
								obvious = TRUE;
							}
						}

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_BLIND);

						break;
					}

					case RBE_CONFUSE:
					{
						/* Take damage */
						take_hit(damage, ddesc);

						/* Increase "confused" */
						if (!(FLAG(p_ptr, TR_RES_CONF)))
						{
							if (inc_confused(3 + randint1(rlev)))
							{
								obvious = TRUE;
							}
						}

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_CONF);

						break;
					}

					case RBE_TERRIFY:
					{
						/* Saving throw difficulty */
						int power = MAX(r_ptr->hdice * 2, damage);

						/* Take damage */
						take_hit(damage, ddesc);

						/* Increase "afraid" */
						if (FLAG(p_ptr, TR_RES_FEAR))
						{
							msgf("You stand your ground!");
							obvious = TRUE;
						}
						else if (player_save(power))
						{
							msgf("You stand your ground!");
							obvious = TRUE;
						}
						else
						{
							if (inc_afraid(3 + randint1(rlev)))
							{
								obvious = TRUE;
							}
						}

						/* Learn about the player */
						update_smart_learn(m_idx, DRS_FEAR);

						break;
					}

					case RBE_PARALYZE:
					{
						/* Saving throw difficulty */
						int power = MAX(r_ptr->hdice * 2, damage);

						/* Hack -- Prevent perma-paralysis via damage */
						if (p_ptr->tim.paralyzed && (damage < 1)) damage = 1;

						/* Take damage */
						take_hit(damage, ddesc);

						/* Increase "paralyzed" */
						if (FLAG(p_ptr, TR_FREE_ACT))
						{
							msgf("You are unaffected!");
							obvious = TRUE;
						}
						else if (player_save(power))
						{
							msgf("You resist the effects!");
							obvious = TRUE;
						}
						else
						{
							if (inc_paralyzed(3 + randint1(rlev)))
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
						/* Damage (physical) */
						take_hit(damage, ddesc);

						/* Damage (stat) */
						if (do_dec_stat(A_STR)) obvious = TRUE;

						break;
					}

					case RBE_LOSE_INT:
					{
						/* Damage (physical) */
						take_hit(damage, ddesc);

						/* Damage (stat) */
						if (do_dec_stat(A_INT)) obvious = TRUE;

						break;
					}

					case RBE_LOSE_WIS:
					{
						/* Damage (physical) */
						take_hit(damage, ddesc);

						/* Damage (stat) */
						if (do_dec_stat(A_WIS)) obvious = TRUE;

						break;
					}

					case RBE_LOSE_DEX:
					{
						/* Damage (physical) */
						take_hit(damage, ddesc);

						/* Damage (stat) */
						if (do_dec_stat(A_DEX)) obvious = TRUE;

						break;
					}

					case RBE_LOSE_CON:
					{
						/* Damage (physical) */
						take_hit(damage, ddesc);

						/* Damage (stat) */
						if (do_dec_stat(A_CON)) obvious = TRUE;

						break;
					}

					case RBE_LOSE_CHR:
					{
						/* Damage (physical) */
						take_hit(damage, ddesc);

						/* Damage (stat) */
						if (do_dec_stat(A_CHR)) obvious = TRUE;

						break;
					}

					case RBE_LOSE_ALL:
					{
						/* Damage (physical) */
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

						/* Hack Reduce damage based on the player armor class */
						damage -= (damage * ((ac < 150) ? ac : 150) / 250);

						/* Take damage */
						take_hit(damage, ddesc);

						/* Radius 8 earthquake centered at the monster */
						if (damage > 23)
						{
							(void)earthquake(m_ptr->fx, m_ptr->fy, 8);
						}

						break;
					}

					case RBE_EXP_10:
					{
						/* Obvious */
						obvious = TRUE;

						/* Take damage */
						take_hit(damage, ddesc);

						if ((FLAG(p_ptr, TR_HOLD_LIFE)) && (randint0(100) < 95))
						{
							msgf("You keep hold of your life force!");
						}
						else
						{
							s32b d = damroll(10,
											 6) +
								(p_ptr->exp / 100) * MON_DRAIN_LIFE;
							if ((FLAG(p_ptr, TR_HOLD_LIFE)))
							{
								msgf("You feel your life slipping away!");
								lose_exp(d / 10);
							}
							else
							{
								msgf("You feel your life draining away!");
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

						if ((FLAG(p_ptr, TR_HOLD_LIFE)) && (randint0(100) < 90))
						{
							msgf("You keep hold of your life force!");
						}
						else
						{
							s32b d = damroll(20, 6) +
								(p_ptr->exp / 100) * MON_DRAIN_LIFE;
							if (FLAG(p_ptr, TR_HOLD_LIFE))
							{
								msgf("You feel your life slipping away!");
								lose_exp(d / 10);
							}
							else
							{
								msgf("You feel your life draining away!");
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

						if ((FLAG(p_ptr, TR_HOLD_LIFE)) && (randint0(100) < 75))
						{
							msgf("You keep hold of your life force!");
						}
						else
						{
							s32b d = damroll(40, 6) +
								(p_ptr->exp / 100) * MON_DRAIN_LIFE;
							if (FLAG(p_ptr, TR_HOLD_LIFE))
							{
								msgf("You feel your life slipping away!");
								lose_exp(d / 10);
							}
							else
							{
								msgf("You feel your life draining away!");
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

						if ((FLAG(p_ptr, TR_HOLD_LIFE)) && (randint0(100) < 50))
						{
							msgf("You keep hold of your life force!");
						}
						else
						{
							s32b d = damroll(80, 6) +
								(p_ptr->exp / 100) * MON_DRAIN_LIFE;
							if ((FLAG(p_ptr, TR_HOLD_LIFE)))
							{
								msgf("You feel your life slipping away!");
								lose_exp(d / 10);
							}
							else
							{
								msgf("You feel your life draining away!");
								lose_exp(d);
							}
						}
						break;
					}

					case RBE_DISEASE:
					{
						/* Take some damage */
						take_hit(damage, ddesc);

						/* Take "poison" effect */
						obvious = pois_dam(10, "disease", randint1(rlev) + 5);

						/* Damage CON (10% chance) */
						if (randint0(100) < 10)
						{
							/* 1% chance for perm. damage */
							bool perm = (one_in_(10));
							if (dec_stat(A_CON, randint1(10), perm)) obvious =
									TRUE;
						}

						break;
					}
					case RBE_TIME:
					{
						switch (randint1(10))
						{
							case 1:  case 2:  case 3:  case 4:  case 5:
							{
								msgf("You feel life has clocked back.");
								lose_exp(100 +
										 (p_ptr->exp / 100) * MON_DRAIN_LIFE);
								break;
							}

							case 6:  case 7:  case 8:  case 9:
							{
								int stat = randint0(6);

								switch (stat)
								{
									case A_STR:
									{
										act = "strong";
										break;
									}
									case A_INT:
									{
										act = "bright";
										break;
									}
									case A_WIS:
									{
										act = "wise";
										break;
									}
									case A_DEX:
									{
										act = "agile";
										break;
									}
									case A_CON:
									{
										act = "hale";
										break;
									}
									case A_CHR:
									{
										act = "beautiful";
										break;
									}
								}

								msgf
									("You're not as %s as you used to be...",
									 act);

                                /* Note: this is a change from old behavior -RML */
								p_ptr->stat[stat].cur =
									(p_ptr->stat[stat].cur * 3) / 4;
								if (p_ptr->stat[stat].cur <
									30) p_ptr->stat[stat].cur = 30;
								p_ptr->update |= (PU_BONUS);
								break;
							}

							case 10:
							{
								msgf
									("You're not as powerful as you used to be...");

								for (k = 0; k < A_MAX; k++)
								{
									p_ptr->stat[k].cur =
										(p_ptr->stat[k].cur * 3) / 4;
									if (p_ptr->stat[k].cur <
										30) p_ptr->stat[k].cur = 30;
								}
								p_ptr->update |= (PU_BONUS);
								break;
							}
						}
						take_hit(damage, ddesc);

						break;
					}
					case RBE_EXP_VAMP:
					{
						/* Obvious */
						obvious = TRUE;

						/* Take damage */
						take_hit(damage, ddesc);

						if ((FLAG(p_ptr, TR_HOLD_LIFE)) && (randint0(100) < 50))
						{
							msgf("You keep hold of your life force!");
							resist_drain = TRUE;
						}
						else
						{
							s32b d = damroll(60,
											 6) +
								(p_ptr->exp / 100) * MON_DRAIN_LIFE;
							if (FLAG(p_ptr, TR_HOLD_LIFE))
							{
								msgf("You feel your life slipping away!");
								lose_exp(d / 10);
							}
							else
							{
								msgf("You feel your life draining away!");
								lose_exp(d);
							}
						}

						/* Heal the attacker? */
						if (!(p_ptr->rp.prace == RACE_ZOMBIE ||
							  p_ptr->rp.prace == RACE_VAMPIRE ||
							  p_ptr->rp.prace == RACE_SPECTRE ||
							  p_ptr->rp.prace == RACE_SKELETON ||
							  p_ptr->rp.prace == RACE_GOLEM ||
							  p_ptr->rp.prace == RACE_GHOUL) &&
							(damage > 2) && !(resist_drain))
						{
							bool did_heal = FALSE;

							if (m_ptr->hp < m_ptr->maxhp) did_heal = TRUE;

							/* Heal */
							m_ptr->hp += damroll(4, damage / 6);
							if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp =
									m_ptr->maxhp;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx) p_ptr->redraw |=
									(PR_HEALTH);

							/* Special message */
							if ((visible) && (did_heal))
							{
								msgf("%^s appears healthier.", m_name);
							}
						}
					}
				}
			}

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (randint0(100) < 50)
				{
					do_cut = FALSE;
				}

				/* Cancel stun */
				else
				{
					do_stun = FALSE;
				}
			}

			/* Handle cut */
			if (do_cut)
			{
				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0:
					{
						k = 0;
						break;
					}
					case 1:
					{
						k = randint1(5);
						break;
					}
					case 2:
					{
						k = rand_range(5, 10);
						break;
					}
					case 3:
					{
						k = rand_range(20, 40);
						break;
					}
					case 4:
					{
						k = rand_range(50, 100);
						break;
					}
					case 5:
					{
						k = rand_range(100, 200);
						break;
					}
					case 6:
					{
						k = 300;
						break;
					}
					default:
					{
						k = 500;
						break;
					}
				}

				/* Apply the cut */
				if (k) (void)inc_cut(k);
			}

			/* Handle stun */
			if (do_stun)
			{
				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0:
					{
						k = 0;
						break;
					}
					case 1:
					{
						k = randint1(5);
						break;
					}
					case 2:
					{
						k = rand_range(10, 20);
						break;
					}
					case 3:
					{
						k = rand_range(20, 40);
						break;
					}
					case 4:
					{
						k = rand_range(30, 60);
						break;
					}
					case 5:
					{
						k = rand_range(40, 80);
						break;
					}
					case 6:
					{
						k = 100;
						break;
					}
					default:
					{
						k = 200;
						break;
					}
				}

				/* Apply the stun */
				if (k) (void)inc_stun(k);
			}

			if (explode)
			{
				sound(SOUND_EXPLODE);

				if (mon_take_hit(m_idx, m_ptr->hp + 1, &fear, NULL))
				{
					blinked = FALSE;
					alive = FALSE;
				}
			}

			if (touched)
			{
				if ((FLAG(p_ptr, TR_SH_FIRE)) && alive)
				{
					if (!FLAG(r_ptr, RF_IM_FIRE))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, 0);

						msgf("%^s is suddenly very hot!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
										 " turns into a pile of ash."))
						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (visible)
							r_ptr->r_flags[2] |= RF2_IM_FIRE;
					}
				}

				if ((FLAG(p_ptr, TR_SH_ELEC)) && alive)
				{
					if (!FLAG(r_ptr, RF_IM_ELEC))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, 0);

						msgf("%^s gets zapped!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
										 " turns into a pile of cinder."))
						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (visible)
							r_ptr->r_flags[2] |= RF2_IM_ELEC;
					}
				}

				if ((FLAG(p_ptr, TR_SH_ACID)) && alive)
				{
					if (!FLAG(r_ptr, RF_IM_ACID))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, 0);

						msgf("%^s gets melted!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
										 " turns into a puddle of goo."))
						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (visible)
							r_ptr->r_flags[2] |= RF2_IM_ACID;
					}
				}

				if ((FLAG(p_ptr, TR_SH_COLD)) && alive)
				{
					if (!FLAG(r_ptr, RF_IM_COLD))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, 0);

						msgf("%^s gets frozen!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
										 " turns into an icicle."))
						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (visible)
							r_ptr->r_flags[2] |= RF2_IM_COLD;
					}
				}
				touched = FALSE;
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
				case RBM_CHARGE:
				{
					/* Visible monsters */
					if (visible)
					{
						/* Disturbing */
						disturb(TRUE);

						/* Message */
						msgf("%^s misses you.", m_name);
					}

					break;
				}
			}
		}


		/* Analyze "visible" monsters only */
		if (alive && visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					r_ptr->r_blows[ap_cnt]++;
				}

				/* Look to see if we've spotted a mimic */
				if (m_ptr->smart & SM_MIMIC)
				{
					/* Toggle flag */
					m_ptr->smart &= ~(SM_MIMIC);

					/* It is in the monster list now */
					update_mon_vis(m_ptr->r_idx, 1);
				}
			}
		}
	}


	/* Blink away */
	if (blinked && alive)
	{
		msgf("The thief flees laughing!");
		(void)teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}


	/* Always notice cause of death */
	if (p_ptr->state.is_dead && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

	if (alive && visible && fear)
	{
		flee_message(m_name, m_ptr->r_idx);
	}

	/* Assume we attacked */
	return (TRUE);
}
