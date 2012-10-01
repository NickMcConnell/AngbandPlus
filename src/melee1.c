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
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_m_hit(int power, int level)
{
	int i, k, ac;

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

#define MAX_DESC_MOAN 4

/*
 * Hack -- possible "insult" messages
 */
static cptr desc_moan[MAX_DESC_MOAN] =
{
	"seems sad about something.",
	"asks if you have seen his dogs.",
	"tells you to get off his land.",
	"mumbles something about mushrooms."
};

/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	int ap_cnt;

	int i, j, k, tmp, ac, rlev;
	int do_cut, do_stun;

	s32b gold;

	object_type *o_ptr;

	char o_name[80];

	char m_name[80];

	char ddesc[80];

	bool blinked;

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

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
        if (r_ptr->flags2 & RF2_HAS_LITE) visible = TRUE;

		/* Extract the attack "power" */
		switch (effect)
		{
			case RBE_HURT:		power = 60; break;
			case RBE_POISON:	power =  5; break;
			case RBE_DISEASE:	power =  2; break;
			case RBE_UN_BONUS:	power = 20; break;
			case RBE_UN_POWER:	power = 15; break;
			case RBE_EAT_GOLD:	power =  5; break;
			case RBE_EAT_ITEM:	power =  5; break;
			case RBE_EAT_FOOD:	power =  5; break;
			case RBE_EAT_LITE:	power =  5; break;
			case RBE_ACID:		power =  0; break;
			case RBE_ELEC:		power = 10; break;
			case RBE_FIRE:		power = 10; break;
			case RBE_COLD:		power = 10; break;
			case RBE_BLIND:		power =  2; break;
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
			case RBE_EXP_1:		power =  5; break;
			case RBE_EXP_2:		power =  5; break;
			case RBE_EXP_3:		power =  5; break;
			case RBE_EXP_4:		power =  5; break;
		}

		/* Monster hits player */
		if (!effect || check_m_hit(power, rlev))
		{
			/* Always disturbing */
			disturb(1);

			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) &&
			    (r_ptr->flags2 & (RF2_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				lore_learn(m_ptr, LRN_FLAG2, RF2_EVIL, FALSE);

				/* Message */
				message_format(MSG_MONSTER, m_ptr->r_idx, "%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Assume no cut or stun */
			do_cut = do_stun = 0;

			/* Describe the attack method */
			switch (method)
			{
				case RBM_HIT:
				{
					act = "hits you.";
					do_cut = do_stun = 1;
					break;
				}

				case RBM_TOUCH:
				{
					act = "touches you.";
					break;
				}

				case RBM_PUNCH:
				{
					act = "punches you.";
					do_stun = 1;
					break;
				}

				case RBM_KICK:
				{
					act = "kicks you.";
					do_stun = 1;
					break;
				}

				case RBM_CLAW:
				{
					act = "claws you.";
					do_cut = 1;
					break;
				}

				case RBM_BITE:
				{
					act = "bites you.";
					do_cut = 1;
					break;
				}

				case RBM_STING:
				{
					act = "stings you.";
					break;
				}

				case RBM_KISS:
				{
					act = "kisses you.";
					break;
				}

				case RBM_BUTT:
				{
					act = "butts you.";
					do_stun = 1;
					break;
				}

				case RBM_CRUSH:
				{
					act = "crushes you.";
					do_stun = 1;
					break;
				}

				case RBM_ENGULF:
				{
					act = "engulfs you.";
					break;
				}

				case RBM_CRAWL:
				{
					act = "crawls on you.";
					break;
				}

				case RBM_DROOL:
				{
					act = "drools on you.";
					break;
				}

				case RBM_SPIT:
				{
					act = "spits on you.";
					break;
				}

				case RBM_GAZE:
				{
					act = "gazes at you.";
					break;
				}

				case RBM_WAIL:
				{
					act = "wails at you.";
					break;
				}

				case RBM_SPORE:
				{
					act = "releases spores at you.";
					break;
				}

				case RBM_BEG:
				{
					act = "begs you for money.";
					break;
				}

				case RBM_INSULT:
				{
					act = desc_insult[rand_int(MAX_DESC_INSULT)];
					break;
				}

				case RBM_MOAN:
				{
					act = desc_moan[rand_int(MAX_DESC_MOAN)];
					break;
				}

			}

			/* Message */
			if (act) message_format(MSG_MONSTER, m_ptr->r_idx, "%^s %s", m_name, act);

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

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
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
					{
						if (set_poisoned(p_ptr->poisoned + randint(rlev) + 5))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_POIS);

					break;
				}

				case RBE_DISEASE:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Take "disease" effect */
					if (!(p_ptr->resist_disease || p_ptr->oppose_disease))
					{
						if (set_diseased(p_ptr->diseased + randint(rlev) + 5))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_DISEASE);

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
						if (apply_disenchant()) obvious = TRUE;
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_RES_DISEN);

					break;
				}

				case RBE_UN_POWER:
				{
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

						/* Drain charged wands/staffs */
						if (((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)) &&
							(o_ptr->pval > 0))
						{
							/* Message */
							message(MSG_ITEM_DAMAGE, 0, "Energy drains from your pack!");

							/* Obvious */
							obvious = TRUE;

							/* Heal */
							j = rlev;
							m_ptr->hp += j * o_ptr->pval * o_ptr->number;
							if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

							/* Uncharge */
							o_ptr->pval = 0;

							/* Combine / Reorder the pack */
							p_ptr->notice |= (PN_COMBINE | PN_REORDER);

							/* Window stuff */
							p_ptr->window |= (PW_INVEN);

							/* Done */
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
					if (!p_ptr->paralyzed &&
					    (rand_int(100) < (adj_dex_safe[p_stat(A_DEX)] + p_ptr->lev)))
					{
						/* Saving throw message */
						message(MSG_RESIST, 0, "You quickly protect your money pouch!");

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
							message(MSG_THEFT, 0, "Nothing was stolen.");
						}
						else if (p_ptr->au)
						{
							message(MSG_THEFT, 0, "Your purse feels lighter.");
							message_format(MSG_THEFT, -1, "%ld coins were stolen!", (long)gold);
						}
						else
						{
							message(MSG_THEFT, 0, "Your purse feels lighter.");
							message(MSG_THEFT, -1, "All of your coins were stolen!");
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

				case RBE_EAT_ITEM:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->paralyzed &&
					    (rand_int(100) < (adj_dex_safe[p_stat(A_DEX)] + p_ptr->lev)))
					{
						/* Saving throw message */
						message(MSG_RESIST, 0, "You grab hold of your backpack!");

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
						object_desc(o_name, o_ptr, FALSE, 3);

						/* Message */
						message_format(MSG_THEFT, 0, "%sour %s (%c) was stolen!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           o_name, index_to_label(i));

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain local object */
						object_copy(i_ptr, o_ptr);

						/* Modify number */
						i_ptr->number = 1;

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
						object_desc(o_name, o_ptr, FALSE, 0);

						/* Message */
						message_format(MSG_THEFT, 0, "%sour %s (%c) was eaten!",
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
					o_ptr = &inventory[INVEN_LITE];

					/* Drain fuel */
					if ((o_ptr->timeout > 0) && (!artifact_p(o_ptr)))
					{
						/* Reduce fuel */
						o_ptr->timeout -= (250 + randint(250));
						if (o_ptr->timeout < 1) o_ptr->timeout = 1;

						/* Notice */
						if (!p_ptr->blind)
						{
							message(MSG_EFFECT, 0, "Your light dims.");
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
					message(MSG_MONSTER, m_ptr->r_idx, "You are covered in acid!");

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
					message(MSG_MONSTER, m_ptr->r_idx, "You are struck by electricity!");

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
					message(MSG_MONSTER, m_ptr->r_idx, "You are enveloped in flames!");

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
					message(MSG_MONSTER, m_ptr->r_idx, "You are covered with frost!");

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
					if (!p_ptr->no_blind)
					{
						if (set_blind(p_ptr->blind + 10 + randint(rlev)))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_NO_BLIND);

					break;
				}

				case RBE_CONFUSE:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "confused" */
					if (!p_ptr->resist_confu)
					{
						if (set_confused(p_ptr->confused + randint(5) + randint((rlev / 5) + 1)))
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
					if (p_ptr->bravery)
					{
						message(MSG_RESIST, 0, "You stand your ground!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->skill[SK_SAV])
					{
						message(MSG_RESIST, 0, "You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (set_afraid(p_ptr->afraid + randint(6) + randint((rlev / 5) + 1)))
						{
							obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_BRAVE);

					break;
				}

				case RBE_PARALYZE:
				{
					/* Hack -- Prevent perma-paralysis via damage */
					if (p_ptr->paralyzed && (damage < 1)) damage = 1;

					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "paralyzed" */
					if (p_ptr->free_act)
					{
						message(MSG_RESIST, 0, "You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->skill[SK_SAV])
					{
						message(MSG_RESIST, 0, "You resist the effects!");
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
				}

				case RBE_LOSE_STR:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_STR, 1, FALSE, TRUE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_INT:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_INT, 1, FALSE, TRUE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_WIS:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_WIS, 1, FALSE, TRUE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_DEX:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_DEX, 1, FALSE, TRUE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CON:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_CON, 1, FALSE, TRUE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CHR:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_CHR, 1, FALSE, TRUE)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_ALL:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stats) */
					if (do_dec_stat(A_STR, 1, FALSE, TRUE)) obvious = TRUE;
					if (do_dec_stat(A_DEX, 1, FALSE, TRUE)) obvious = TRUE;
					if (do_dec_stat(A_CON, 1, FALSE, TRUE)) obvious = TRUE;
					if (do_dec_stat(A_INT, 1, FALSE, TRUE)) obvious = TRUE;
					if (do_dec_stat(A_WIS, 1, FALSE, TRUE)) obvious = TRUE;
					if (do_dec_stat(A_CHR, 1, FALSE, TRUE)) obvious = TRUE;

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
					if (damage > 23) earthquake(m_ptr->fy, m_ptr->fx, 8);

					break;
				}

				case RBE_EXP_1:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					if (p_ptr->hold_life && (rand_int(100) < 90))
					{
						message(MSG_RESIST, 0, "You keep hold of your life force!");
					}
					else
					{
						s32b d = (p_ptr->exp / 100);
						if (p_ptr->hold_life)
						{
							message(MSG_EFFECT, 0, "You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							message(MSG_EFFECT, 0, "You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				case RBE_EXP_2:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					if (p_ptr->hold_life && (rand_int(100) < 80))
					{
						message(MSG_RESIST, 0, "You keep hold of your life force!");
					}
					else
					{
						s32b d = (p_ptr->exp / 50);
						if (p_ptr->hold_life)
						{
							message(MSG_EFFECT, 0, "You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							message(MSG_EFFECT, 0, "You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				case RBE_EXP_3:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					if (p_ptr->hold_life && (rand_int(100) < 70))
					{
						message(MSG_RESIST, 0, "You keep hold of your life force!");
					}
					else
					{
						s32b d = (p_ptr->exp / 33);
						if (p_ptr->hold_life)
						{
							message(MSG_EFFECT, 0, "You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							message(MSG_EFFECT, 0, "You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				case RBE_EXP_4:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(damage, ddesc);

					if (p_ptr->hold_life && (rand_int(100) < 60))
					{
						message(MSG_RESIST, 0, "You keep hold of your life force!");
					}
					else
					{
						s32b d = (p_ptr->exp / 25);
						if (p_ptr->hold_life)
						{
							message(MSG_EFFECT, 0, "You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							message(MSG_EFFECT, 0, "You feel your life draining away!");
							lose_exp(d);
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
				if (k) (void)set_cut(p_ptr->cut + k);
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
				if (k) (void)set_stun(p_ptr->stun + k);
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
				case RBM_KISS:
				case RBM_BUTT:
				case RBM_CRUSH:
				case RBM_ENGULF:

				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1);

					/* Message */
					message_format(MSG_MON_FAIL, m_ptr->r_idx, "%^s misses you.", m_name);
				}

				break;
			}
		}

		/* Count "obvious" attacks (and ones that cause damage) */
		if (visible && (obvious || damage)) lore_learn(m_ptr, LRN_BLOWS, ap_cnt, TRUE);
	}

	/* Blink away */
	if (blinked)
	{
		message(MSG_EFFECT, 0, "There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	/* Always notice cause of death */
	if (p_ptr->is_dead)	lore_learn(m_ptr, LRN_PDEATH, 0, TRUE);

	/* Assume we attacked */
	return (TRUE);
}

