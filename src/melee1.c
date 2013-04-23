#define MELEE1_C
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
int check_hit(int power, int level)
{
	int i, k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ((p_ptr->ac * 3) / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}

/*
 * Locate a RBM_* index in the blow_methods[] table.
 */
blow_method_type *get_blow_method(byte idx)
{
	/* Subtract 1, as the index for blow_methods[0] is 1. */
	if (idx > 0 && idx <= NUM_BLOW_METHODS)
		return blow_methods+idx-1;

	/* This is because 0 is reserved for "No blow". */
	else
		return NULL;
}

#define MAX_BLOWS_PER_MONSTER 4

/*
 * Return a random blow the specified monster has available.
 */
static monster_blow *choose_blow(monster_type *m_ptr, monster_blow *blows)
{
	int i;
	monster_blow *b_ptr, *b[MAX_BLOWS_PER_MONSTER];

	/* Count the suitable blows. */
	for (b_ptr = blows, i = 0; b_ptr < blows+MAX_BLOWS_PER_MONSTER; b_ptr++)
	{
		blow_method_type *bm_ptr = get_blow_method(b_ptr->method);

		/* No such blow. */
		if (!bm_ptr) continue;

		/* Nice monsters don't use 300 point attacks without warning. */
		if (m_ptr->mflag & MFLAG_NICE && b_ptr->effect == RBE_SHATTER) continue;

		/* Allow this one. */
		b[i++] = b_ptr;
	}

	/* Return a random blow. */
	if (i) return b[rand_int(i)];

	/* Failed to find one. */
	return 0;
}

/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_blow *mb_ptr;
	blow_method_type *b_ptr;

	monster_type	*m_ptr = &m_list[m_idx];

	monster_race	*r_ptr = &r_info[m_ptr->r_idx];

	

	int			i, j, k, tmp, rlev;
	int			do_cut, do_stun;

	s32b		gold;

	object_type		*o_ptr;

	C_TNEW(m_name, MNAME_MAX, char);

	char		ddesc[80];

	bool		blinked;

	bool touched = FALSE, fear = FALSE, living = TRUE;
	bool visible = FALSE;
	bool obvious = FALSE;

	int power = 0;
	int damage = 0;
		cptr act = NULL;

		/* Extract the attack infomation */
		int effect;
		int method;
		int d_dice;
		int d_side;

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW) ||

	/* ...nor if friendly */
	m_ptr->smart & SM_ALLY)
	{
		TFREE(m_name);
		return FALSE;
	}


	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	strnfmt(ddesc, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0x88);


	/* Assume no blink */
	blinked = FALSE;
	
	/* Give back movement energy */	
	m_ptr->energy += extract_energy[m_ptr->mspeed];
	/* And take some attack energy instead */
	m_ptr->energy -= (TURN_ENERGY/r_ptr->num_blows);

		 visible = FALSE;
		 obvious = FALSE;

		 power = 0;
		 damage = 0;

		 act = NULL;

		/* Choose an attack randomly. */
		mb_ptr = choose_blow(m_ptr, r_ptr->blow);

		/* No valid attacks. */
		if (!mb_ptr) return FALSE;

		/* Extract the attack infomation */
		 effect = mb_ptr->effect;
		 method = mb_ptr->method;
		 d_dice = mb_ptr->d_dice;
		 d_side = mb_ptr->d_side;

		/* Extract the attack details. */
		b_ptr = get_blow_method(method);

		/* Paranoia. */
		if (!b_ptr) return FALSE;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;


		/* Extract the attack "power" */
		switch (effect)
		{
			case RBE_HURT:	power = 60; break;
			case RBE_POISON:	power =  5; break;
			case RBE_UN_BONUS:	power = 20; break;
			case RBE_UN_POWER:	power = 15; break;
			case RBE_EAT_GOLD:	power =  5; break;
			case RBE_EAT_ITEM:	power =  5; break;
			case RBE_EAT_FOOD:	power =  5; break;
			case RBE_EAT_LITE:	power =  5; break;
			case RBE_ACID:	power =  0; break;
			case RBE_ELEC:	power = 10; break;
			case RBE_FIRE:	power = 10; break;
			case RBE_COLD:	power = 10; break;
			case RBE_BLIND:	power =  2; break;
			case RBE_CONFUSE:	power = 10; break;
			case RBE_TERRIFY:	power = 10; break;
			case RBE_PARALYZE:	power =  2; break;
			case RBE_LOSE_STR:	power =  0; break;
			case RBE_LOSE_DEX:	power =  0; break;
			case RBE_LOSE_CON:	power =  0; break;
			case RBE_LOSE_INT:	power =  0; break;
			case RBE_LOSE_WIS:	power =  0; break;
			case RBE_LOSE_CHR:	power =  0; break;
			case RBE_LOSE_ALL:	power =  2; break;
			case RBE_SHATTER:	power = 60; break;
			case RBE_EXP_10:	power =  5; break;
			case RBE_EXP_20:	power =  5; break;
			case RBE_EXP_40:	power =  5; break;
			case RBE_EXP_80:	power =  5; break;
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			/* Always disturbing */
			disturb(1);


			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    ((skill_set[SKILL_SAVE].value/2)  >= rlev) &&
			    ((rand_int(100) + (skill_set[SKILL_SAVE].value/2)) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

			}


			/* Assume no cut or stun */
			do_cut = do_stun = 0;

			/* Describe the attack method */
			touched = !!(b_ptr->flags & RBF_TOUCH);
			do_cut = !!(b_ptr->flags & RBF_CUT);
			do_stun = !!(b_ptr->flags & RBF_STUN);

			/* Select a hit message (e.g. "%^s hits %s"). */

			if (b_ptr->hitplayer)
			{
				int z;

				/* Count the valid entries. */
				for (z = 0; b_ptr->hitplayer[z]; z++);

				/* Choose one. */
				act = b_ptr->hitplayer[rand_int(z)];
			}
			else
			{
				/* act is (e.g.) "%^s hits %s". */
				act = b_ptr->hitmsg;
			}

			/* Message */
			if (act) msg_format(act, m_name, "you");

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
					damage -= (damage * MIN(p_ptr->ac, 150) / 250);

					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					break;
				}

				case RBE_POISON:
				{
					/* Take some damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Take "poison" effect */
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
					{
						if (add_flag(TIMED_POISONED, randint(rlev) + 5))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_POIS);

					break;
				}

				case RBE_UN_BONUS:
				{
					/* Take some damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Allow complete resist */
					if (!p_ptr->resist_disen)
					{
						/* Apply disenchantment */
						if (apply_disenchant(0)) obvious = TRUE;
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_DISEN);

					break;
				}

				case RBE_UN_POWER:
				{
					/* Take some damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						do
						{
							i = rand_int(INVEN_TOTAL);
						}
						while (i >= INVEN_WIELD && i <= INVEN_FEET);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Drain charged wands/staffs */
						if (((o_ptr->tval == TV_STAFF) ||
						     (o_ptr->tval == TV_WAND)) &&
						    (o_ptr->pval))
						{
							/* Message */
							msg_print("Energy drains from your pack!");

							/* Obvious */
							obvious = TRUE;

							/* Heal */
							j = rlev;
							m_ptr->hp += j * o_ptr->pval * o_ptr->number;
							if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

							/* Redraw (later) if needed */
							if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

							/* Uncharge */
							o_ptr->pval = 0;

							/* Forget stacking state. */
							set_stack_number(o_ptr);

							/* Recalculate/redraw stuff (later) */
							update_object(o_ptr, 0);

							/* Done */
							break;
						}
					}

					break;
				}

				case RBE_EAT_GOLD:
				{
					/* Take some damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Obvious */
					obvious = TRUE;

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->paralyzed &&
					    (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
					                      (skill_set[SKILL_SAVE].value/2))))
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
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->paralyzed &&
					    (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
					                      (skill_set[SKILL_SAVE].value/2))))
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
						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip artifacts */
                        if (allart_p(o_ptr)) continue;

						/* Message */
						msg_format("%sour %v (%c) was stolen!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           object_desc_f3, o_ptr, FALSE, 3,
								   index_to_label(o_ptr));

						/* Option */
						if (testing_carry)
						{
							object_type *j_ptr;

							/* Make an object */
							j_ptr = o_pop();

							/* Success */
							if (j_ptr)
							{
								/* Copy object */
								object_copy(j_ptr, o_ptr);

								/* Modify number */
								j_ptr->number = 1;

								/* Forget mark */
								j_ptr->marked = FALSE;

								/* Memorize monster */
								j_ptr->held_m_idx = m_idx;

								/* Build stack */
								j_ptr->next_o_idx = m_ptr->hold_o_idx;

								/* Build stack */
								m_ptr->hold_o_idx = j_ptr - o_list;
							}
                        }
						
						/* Steal the items */
						item_increase(o_ptr, -1);
						item_optimize(o_ptr);

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
					/* Take some damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

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

						/* Message */
						msg_format("%sour %v (%c) was eaten!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           object_desc_f3, o_ptr, FALSE, 0,
								   index_to_label(o_ptr));

						/* Steal the items */
						item_increase(o_ptr, -1);
						item_optimize(o_ptr);

						/* Obvious */
						obvious = TRUE;

						/* Done */
						break;
					}

					break;
				}

				case RBE_EAT_LITE:
				{
					/* Take some damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Access the lite */
					o_ptr = &inventory[INVEN_LITE];

					/* Drain fuel */
					if ((o_ptr->pval > 0) && (!allart_p(o_ptr)))
					{
						/* Reduce fuel */
						o_ptr->pval -= (250 + randint(250));
						if (o_ptr->pval < 1) o_ptr->pval = 1;

						/* Forget stacking state. */
						set_stack_number(o_ptr);

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

				case RBE_ACID:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are covered in acid!");

					/* Special damage */
					acid_dam(damage, ddesc, m_ptr->r_idx);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_ACID);

					break;
				}

				case RBE_ELEC:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are struck by electricity!");

					/* Special damage */
					elec_dam(damage, ddesc, m_ptr->r_idx);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_ELEC);

					break;
				}

				case RBE_FIRE:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are enveloped in flames!");

					/* Special damage */
					fire_dam(damage, ddesc, m_ptr->r_idx);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_FIRE);

					break;
				}

				case RBE_COLD:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are covered with frost!");

					/* Special damage */
					cold_dam(damage, ddesc, m_ptr->r_idx);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_COLD);

					break;
				}

				case RBE_BLIND:
				{
					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Increase "blind" */
					if (!p_ptr->resist_blind)
					{
						if (add_flag(TIMED_BLIND, 10 + randint(rlev)))
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
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Increase "confused" */
					if (!p_ptr->resist_conf)
					{
						if (add_flag(TIMED_CONFUSED, 3 + randint(rlev)))
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
					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Increase "afraid" */
					if (p_ptr->resist_fear)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->skill_sav)
					{
						msg_print("You stand your ground!");
						skill_exp(SKILL_SAVE);
						obvious = TRUE;
					}
					else
					{
						if (add_flag(TIMED_AFRAID, 3 + randint(rlev)))
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
					
					/* Make sure at least a point of damage
					    to stop infinite paralysis */
					if(damage == 0) damage = 1;
					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Increase "paralyzed" */
					if (p_ptr->free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->skill_sav)
					{
						msg_print("You resist the effects!");
						skill_exp(SKILL_SAVE);
						obvious = TRUE;
					}
					else
					{
						if (add_flag(TIMED_PARALYZED, 3 + randint(rlev)))
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
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Damage (stat) */
					if (do_dec_stat(A_STR)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_INT:
				{
					/* Damage (physical) */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Damage (stat) */
					if (do_dec_stat(A_INT)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_WIS:
				{
					/* Damage (physical) */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Damage (stat) */
					if (do_dec_stat(A_WIS)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_DEX:
				{
					/* Damage (physical) */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Damage (stat) */
					if (do_dec_stat(A_DEX)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CON:
				{
					/* Damage (physical) */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Damage (stat) */
					if (do_dec_stat(A_CON)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CHR:
				{
					/* Damage (physical) */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Damage (stat) */
					if (do_dec_stat(A_CHR)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_ALL:
				{
					/* Damage (physical) */
					take_hit(damage, ddesc, m_ptr->r_idx);

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
					damage -= (damage * MIN(p_ptr->ac, 150) / 250);

					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					/* Radius 8 earthquake centered at the monster */
					if (damage > 23) earthquake(m_ptr->fy, m_ptr->fx, 8);

					break;
				}

				case RBE_EXP_10:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					if (p_ptr->hold_life && (rand_int(100) < 95))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_skills(1);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_skills(5);
						}
					}
					break;
				}

				case RBE_EXP_20:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					if (p_ptr->hold_life && (rand_int(100) < 90))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_skills(2);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_skills(10);
						}
					}
					break;
				}

				case RBE_EXP_40:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					if (p_ptr->hold_life && (rand_int(100) < 75))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_skills(4);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_skills(20);
						}
					}
					break;
				}

				case RBE_EXP_80:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc, m_ptr->r_idx);

					if (p_ptr->hold_life && (rand_int(100) < 50))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_skills(8);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_skills(40);
						}
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
				int k = 0;

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
				if (k) (void)add_flag(TIMED_CUT, k);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k = 0;

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

				/* Hack - FORCE_SLEEP monsters only knock a properly prepared
				 * character out after a warning. */
				if (m_ptr->mflag & MFLAG_NICE) k = MIN(k, 99-p_ptr->stun);

				/* Apply the stun */
				if (k) (void)add_flag(TIMED_STUN, k);
			}
            if (touched)
            {
                if (p_ptr->sh_fire && living)
                {   if (!(r_ptr->flags3 & RF3_IM_FIRE))
                    {
                        msg_format("%^s is suddenly very hot!", m_name);
                        if (mon_take_hit(m_idx, damroll(2,6), &fear,
                            " turns into a pile of ash."))
                        {
                            blinked = FALSE;
                            living = FALSE;
                        }
                    }
                    else
                    {
                        if (m_ptr->ml)
                            r_ptr->r_flags3 |= RF3_IM_FIRE;

                    }

                }
                if (p_ptr->sh_elec && living)
                {   if (!(r_ptr->flags3 & RF3_IM_ELEC))
                    {
                        msg_format("%^s gets zapped!", m_name);
                        if (mon_take_hit(m_idx, damroll(2,6), &fear,
                            " turns into a pile of cinder."))
                            {
                                blinked = FALSE;
                                living = FALSE;
                            }
                    }
                    else
                    {
                            if (m_ptr->ml)
                            r_ptr->r_flags3 |= RF3_IM_ELEC;
                    }

                            
                }
                touched = FALSE;
            }

            if (r_ptr->flags2 & RF2_RUN_AWAY)
		{
                              r_ptr->r_flags2 |= (RF2_RUN_AWAY);
			msg_format("%^s runs away.", m_name);
			delete_monster(m_ptr->fy, m_ptr->fx);
		}
        }

		/* Monster missed player */
		else
		{
			/* The player will notice a miss. */
			if (b_ptr->missmsg)
			{
				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1);

					/* Message */
					msg_format(b_ptr->missmsg, m_name, "you");
				}
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			int blow = mb_ptr-r_ptr->blow;

			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[blow] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[blow] < MAX_UCHAR)
				{
					r_ptr->r_blows[blow]++;
				}
			}
		}


	/* Blink away */
	if (blinked)
	{
        msg_print("The thief flees laughing!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}


    if (m_ptr->ml && fear)
    {
        sound (SOUND_FLEE);
        msg_format("%^s flees in terror!", m_name);
    }

	TFREE(m_name);

	/* Assume we attacked */
	return (TRUE);
}


