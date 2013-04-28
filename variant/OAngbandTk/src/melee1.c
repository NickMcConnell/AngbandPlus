/* File: melee1.c */

/* Monster melee attacks.  Monster critical blows, whether a monster 
 * attack hits, insult messages.  The code used when a monster attacks 
 * an adjacent player, including descriptions and effects.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


#include "tnb.h" /* TNB */


/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, int dam)
{
	int max = 0;
	int total = dice * sides;

	/* Must do at least 90% of perfect */
	if (dam < total * 19 / 20)
		return (0);

	/* Randomize. */
	if (rand_int(3) == 0)
		return (0);

	/* Weak blows rarely work */
	if ((dam < 20) && (dam < randint(100)))
		return (0);

	/* Perfect damage */
	if (dam == total)
		max++;

	/* Super-charge */
	if (dam > 19)
	{
		while (rand_int(100) < 2)
			max++;
	}

	/* Critical damage */
	if (dam > 45)
		return (6 + max);
	if (dam > 33)
		return (5 + max);
	if (dam > 25)
		return (4 + max);
	if (dam > 18)
		return (3 + max);
	if (dam > 11)
		return (2 + max);
	return (1 + max);
}





/*
 * Determine if a monster attack against the player succeeds.
 * Now incorporates the effects of terrain and penalizes stunned monsters. -LM-
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level, int terrain_bonus, int m_idx)
{
	int i, k, ac;

	monster_type *m_ptr = &m_list[m_idx];

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10)
		return (k < 5);

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	i = (power + (m_ptr->stunned ? level * 2 : level * 3));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a + terrain_bonus;

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ((ac * 3) / 4)))
		return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] = {
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
static cptr desc_sneer[] = {
	"offers you a pony for an outrageous sum.",
	"waits to tell the Black Riders where you've gone.",
	"tells you to clear out, or he'll break your neck.",
	"sneers at the company you keep."
};

/*
 * Mega Hack, but I don't care --- Ask for and take some survival item, 
 * depending on what the player has, then give some money in exchange, 
 * teleport away, and delete the monster.
 */
static void make_request(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	object_type *o_ptr;


	int i;
	int requested_slot = 0;
	int requested_number = 0;
	int sanity_check = 0;
	int offer_price = 0;

	char m_name[80];
	char o_name[O_NAME_MAX];


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Decide what and how much to ask for. */
	while (TRUE)
	{
		/* Increment the loop count. */
		sanity_check++;

		/* Select a random object in the pack. */
		i = rand_int(INVEN_PACK);
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Skip anything that isn't a torch, flask of oil, food ration, 
		 * or scroll of recall.
		 */
		switch (o_ptr->tval)
		{
			case TV_LITE:
			{
				if (o_ptr->sval == SV_LITE_TORCH)
					requested_slot = i;
				break;
			}
			case TV_FLASK:
			{
				requested_slot = i;

				break;
			}
			case TV_FOOD:
			{
				if (o_ptr->sval == SV_FOOD_RATION)
					requested_slot = i;
				break;
			}
			case TV_SCROLL:
			{
				if (o_ptr->sval == SV_SCROLL_WORD_OF_RECALL)
					requested_slot = i;
				break;
			}
			default:
			{
				break;
			}
		}

		/* Found an appropriate item. */
		if (requested_slot)
		{
			object_kind *k_ptr = &k_info[o_ptr->k_idx];

			/* I know what I want.  Now I need to figure how much of it I 
			 * can get away with.
			 */
			requested_number = o_ptr->number;
			if (requested_number > 10)
				requested_number = 9 + (requested_number / 5);

			/* I must offer a fair price, plus some (I'm desperate). */
			offer_price =
				(k_ptr->cost * requested_number) + ((5 +
		  randint(5)) * 100L);

			/* Done. */
			break;
		}

		/* Player doesn't have anything of interest. */
		if (sanity_check > 50)
		{
			/* Let the player know. */
			msg_format
				("%^s looks doleful.  You don't have anything of interest.",
				m_name);

			/* Nothing requested. */
			requested_slot = 0;

			/* Done. */
			break;
		}
	}

	if (requested_slot)
	{
		/* Get the object. */
		o_ptr = &inventory[requested_slot];

		/* Acquire the item name. */
		object_desc(o_name, o_ptr, FALSE, 3);


		/* Try to make a deal for the item. */
		msg_format("%^s looks longingly at your abundant supplies:",
			m_name);
		msg_format
			("'Kind Sir, I desperately need %d %s.  I will gladly give %d gold in exchange.'",
			requested_number, o_name, offer_price);

		/* Make the trade. */
		if (get_check("Accept the offer? "))
		{
			/* Be friendly. */
			msg_print(NULL);
			msg_print("May you have all your heart's desire!");

			/* Take the item. */
			inven_item_increase(i, -requested_number);
			inven_item_describe(i);
			inven_item_optimize(i);

			/* Give gold. */
			p_ptr->au += offer_price;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);
		}

		/* How can you be so stingy! */
		else
		{
			/* Complain bitterly. */
			msg_print(NULL);
			msg_print
				("You scummy excuse for a kobold crook!  May jackals gnaw your bones!");
		}
	}

	/* Hack -- Teleport the monster away and delete it (to prevent the player 
	 * getting rich).
	 */
	teleport_away(m_idx, 50);
	msg_format("%^s runs off.", m_name);

	/* If the monster is a unique, it will never come back. */
	if (r_ptr->flags1 & (RF1_UNIQUE))
		r_ptr->max_num = 0;

	/* Delete the monster. */
	delete_monster_idx(m_idx);
}


/*
 * Attack the player via physical attacks.
 *
 * Support for new features -LM-
 */
bool make_attack_normal(int m_idx, int y, int x)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int i, j, k, tmp, ac, rlev;
	int do_cut, do_stun;

	int terrain_bonus = 0;

	s32b gold;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[O_NAME_MAX];

	char m_name[80];

	char ddesc[80];

	bool blinked;


	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW))
		return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88);


	/* Players in rubble can take advantage of cover. */
	if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		terrain_bonus = ac / 8 + 5;
	}
	/* Players in trees can take advantage of cover, especially rangers and druids. */
	if (cave_feat[y][x] == FEAT_TREE)
	{
		if ((p_ptr->pclass == CLASS_RANGER) ||
			(p_ptr->pclass == CLASS_DRUID))
			terrain_bonus = ac / 8 + 10;
		else
			terrain_bonus = ac / 10 + 2;
	}
	/* Players in water are vulnerable. */
	if (cave_feat[y][x] == FEAT_WATER)
	{
		terrain_bonus = -(ac / 3);
	}


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
		if (!method)
			break;


		/* Handle "leaving" */
		if (p_ptr->leaving)
			break;


		/* Extract visibility (before blink) */
		if (m_ptr->ml)
			visible = TRUE;



		/* Extract the attack "power".  Elemental attacks upgraded. */
		switch (effect)
		{
			case RBE_HURT:
				power = 60;
				break;
			case RBE_POISON:
				power = 25;
				break;
			case RBE_UN_BONUS:
				power = 20;
				break;
			case RBE_UN_POWER:
				power = 15;
				break;
			case RBE_EAT_GOLD:
				power = 5;
				break;
			case RBE_EAT_ITEM:
				power = 5;
				break;
			case RBE_EAT_FOOD:
				power = 5;
				break;
			case RBE_EAT_LITE:
				power = 5;
				break;
			case RBE_ACID:
				power = 50;
				break;
			case RBE_ELEC:
				power = 50;
				break;
			case RBE_FIRE:
				power = 50;
				break;
			case RBE_COLD:
				power = 50;
				break;
			case RBE_BLIND:
				power = 2;
				break;
			case RBE_CONFUSE:
				power = 10;
				break;
			case RBE_TERRIFY:
				power = 10;
				break;
			case RBE_PARALYZE:
				power = 2;
				break;
			case RBE_LOSE_STR:
				power = 0;
				break;
			case RBE_LOSE_DEX:
				power = 0;
				break;
			case RBE_LOSE_CON:
				power = 0;
				break;
			case RBE_LOSE_INT:
				power = 0;
				break;
			case RBE_LOSE_WIS:
				power = 0;
				break;
			case RBE_LOSE_CHR:
				power = 0;
				break;
			case RBE_LOSE_ALL:
				power = 2;
				break;
			case RBE_SHATTER:
				power = 60;
				break;
			case RBE_EXP_10:
				power = 5;
				break;
			case RBE_EXP_20:
				power = 5;
				break;
			case RBE_EXP_40:
				power = 5;
				break;
			case RBE_EXP_80:
				power = 5;
				break;
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev, terrain_bonus, m_idx))
		{
			/* Always disturbing */
			disturb(1, 0);

			/* Hack -- Apply "protection from evil".  Somewhat modi-
			 * fied in Oangband. */
			if ((p_ptr->protevil > 0) && (r_ptr->flags3 & (RF3_EVIL)) &&
				(3 * p_ptr->lev / 2 >= rlev) &&
				((rand_int(100 + p_ptr->lev - 5 * rlev / 4)) > 50))
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

				case RBM_XXX1:
				{
					act = "XXX1's you.";
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

				case RBM_XXX2:
				{
					act = "XXX2's you.";
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

				case RBM_XXX3:
				{
					act = "XXX3's on you.";
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

				case RBM_XXX4:
				{
					act = "projects XXX4's at you.";
					break;
				}

				case RBM_BEG:
				{
					act = "begs you for money.";
					break;
				}

				case RBM_INSULT:
				{
					act = desc_insult[rand_int(8)];
					break;
				}

				case RBM_SNEER:
				{
					act = desc_sneer[rand_int(4)];
					break;
				}

				case RBM_REQUEST:
				{
					make_request(m_idx);
					break;
				}
			}

			/* Message */
			if (act)
				sound(SNDGRP_MONSTER_ATTACK, method, 0); /* ALLOW_SOUND */
			if (act)
				msg_format("%^s %s", m_name, act);

			/* The undead can give the player the Black Breath with a 
			 * sucessful blow. Uniques have a much better chance. -LM-
			 */
			if ((r_ptr->level >= 40) && (r_ptr->flags3 & (RF3_UNDEAD)) &&
				(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(randint(250 - r_ptr->level) == 1))

			{
				msg_print("Your foe calls upon your soul!");
				msg_print
					("You feel the Black Breath slowly draining you of life...");
				p_ptr->black_breath = TRUE;
			}

			else if ((r_ptr->level >= 50) && (r_ptr->flags3 & (RF3_UNDEAD))
				&& (randint(500 - r_ptr->level) == 1))
			{
				msg_print("Your foe calls upon your soul!");
				msg_print
					("You feel the Black Breath slowly draining you of life...");
				p_ptr->black_breath = TRUE;
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage.  Having resistances no longer reduces
			 * elemental attacks quite so much. 
			 */
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

					/* Poison is no longer fully cumulative. */
				case RBE_POISON:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Take "poison" effect */
					if (!(p_ptr->resist_pois && p_ptr->oppose_pois))
					{
						if (p_ptr->resist_pois || p_ptr->oppose_pois)
							damage /= 3;

						if (p_ptr->poisoned)
						{
							/* 1/3 to 2/3 damage. */
							if (set_poisoned(p_ptr->poisoned +
									randint((damage + 2) / 3) +
									(damage / 3)))
								obvious = TRUE;
						}
						else
						{
							/* 1/2 to whole damage, plus 4. */
							if (set_poisoned(p_ptr->poisoned + 4 +
									randint((damage + 1) / 2) +
									(damage / 2)))
								obvious = TRUE;
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_POIS);

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
						if (apply_disenchant(0))
							obvious = TRUE;
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_DISEN);

					break;
				}

					/* now drains rods too. */
				case RBE_UN_POWER:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Blindly hunt ten times for an item. */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];
						k_ptr = &k_info[o_ptr->k_idx];

						/* use "tmp" to decide if a item can 
						 * be uncharged.  By default, assume it 
						 * can't.
						 */
						tmp = 0;

						/* Skip non-objects */
						if (!o_ptr->k_idx)
							continue;

						/* Drain charged wands/staffs/rods */
						if ((o_ptr->tval == TV_STAFF) ||
							(o_ptr->tval == TV_WAND) ||
							(o_ptr->tval == TV_ROD))
						{
							/* case of charged wands/staffs. */
							if (((o_ptr->tval == TV_STAFF) ||
									(o_ptr->tval == TV_WAND)) &&
								(o_ptr->pval)) tmp = 1;

							/* case of (at least partially) charged rods. */
							if ((o_ptr->tval == TV_ROD) &&
								(o_ptr->timeout < o_ptr->pval))
								tmp = 1;

							if (tmp)
							{
								/* Message */
								sound(SNDGRP_EVENT, SND_PACK_DRAIN, 0);	/* ALLOW_SOUND */
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
									j *=
										(o_ptr->pval -
										o_ptr->timeout) / 30;
								}
								else
								{
									j *= o_ptr->pval * o_ptr->number;
								}

								/* Replenish monster mana */
								if (m_ptr->mana < r_ptr->mana)
								{
									if (j >
										(r_ptr->mana - m_ptr->mana) * 10)
									{
										j -=
											(r_ptr->mana -
											m_ptr->mana) * 10;
										m_ptr->mana = r_ptr->mana;
									}
									else
									{
										m_ptr->mana += (j / 10) + 1;
										j = 0;
									}
								}

								/* Add hps with leftover */
								m_ptr->hp += j * 2;

								if (m_ptr->hp > m_ptr->maxhp)
									m_ptr->hp = m_ptr->maxhp;

								/* Redraw (later) if needed */
								if (p_ptr->health_who == m_idx)
									p_ptr->redraw |= (PR_HEALTH);


								/* Uncharge */
								if ((o_ptr->tval == TV_STAFF) ||
									(o_ptr->tval == TV_WAND))
									o_ptr->pval = 0;

								/* New-style rods. */
								if (o_ptr->tval == TV_ROD)
									o_ptr->timeout = o_ptr->pval;


								/* Combine / Reorder the pack */
								p_ptr->notice |= (PN_COMBINE | PN_REORDER);

								/* Window stuff */
								p_ptr->window |= (PW_INVEN);

								/* not more than one inventory 
								 * slot effected. */
								break;
							}
						}
					}

					break;
				}

				case RBE_EAT_GOLD:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Confused monsters cannot steal successfully. */
					if (m_ptr->confused)
						break;

					/* Obvious */
					obvious = TRUE;

					/* Saving throw (unless paralyzed) based on dex and 
					 * relationship between yours and monster's level.
					 */
					if (!p_ptr->paralyzed &&
						(rand_int(100) <
	   (adj_dex_safe[p_ptr->stat_ind[A_DEX]] + p_ptr->lev -
								(r_ptr->level / 2))))
					{
						/* Saving throw message */
						msg_print("You quickly protect your money pouch!");

						/* Occasional blink anyway */
						if (rand_int(3))
							blinked = TRUE;
					}

					/* Eat gold */
					else
					{
						gold = (p_ptr->au / 12) + randint(25);
						if (gold < 2)
							gold = 2;
						if (gold > 5000)
							gold = (p_ptr->au / 20) + randint(2000);
						if (gold > p_ptr->au)
							gold = p_ptr->au;
						p_ptr->au -= gold;
						if (gold <= 0)
						{
							msg_print("Nothing was stolen.");
						}
						else if (p_ptr->au)
						{
							msg_print("Your purse feels lighter.");
							sound(SNDGRP_EVENT, SND_PACK_STEAL, 0);	/* ALLOW_SOUND */
							msg_format("%ld coins were stolen!",
								(long) gold);
						}
						else
						{
							msg_print("Your purse feels lighter.");
							sound(SNDGRP_EVENT, SND_PACK_STEAL, 0);	/* ALLOW_SOUND */
							msg_print("All of your coins were stolen!");
						}

#if 1 /* TNB */
						/* Steal that money! */
						if (gold)
						{
							monster_steal_gold(m_idx, gold);
						}
#endif /* TNB */

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

					/* Confused monsters cannot steal successfully. */
					if (m_ptr->confused)
						break;

					/* Saving throw (unless paralyzed) based on dex and 
					 * relationship between yours and monster's level.
					 */
					if (!p_ptr->paralyzed &&
						(rand_int(100) <
	   (adj_dex_safe[p_ptr->stat_ind[A_DEX]] + p_ptr->lev -
								(r_ptr->level / 2))))
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
						if (!o_ptr->k_idx)
							continue;

						/* Skip artifacts */
						if (artifact_p(o_ptr))
							continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 3);

						/* Message */
						sound(SNDGRP_EVENT, SND_PACK_STEAL, 0);	/* ALLOW_SOUND */
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
						if ((o_ptr->tval == TV_ROD) ||
							(o_ptr->tval == TV_WAND))
						{
							k_ptr = &k_info[o_ptr->k_idx];
							i_ptr->pval = o_ptr->pval / o_ptr->number;
							o_ptr->pval -= i_ptr->pval;
						}

						/* Carry the object */
						(void) monster_carry(m_idx, i_ptr);

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
						if (!o_ptr->k_idx)
							continue;

						/* Skip non-food objects */
						if (o_ptr->tval != TV_FOOD)
							continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 0);

						/* Message */
						sound(SNDGRP_EVENT, SND_PACK_STEAL, 0);	/* ALLOW_SOUND */
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

					/* Access the lite */
					o_ptr = &inventory[INVEN_LITE];

					/* Drain fuel */
					if ((o_ptr->pval > 0) && (!artifact_p(o_ptr)))
					{
						/* Reduce fuel */
						o_ptr->pval -= (250 + randint(250));
						if (o_ptr->pval < 1)
							o_ptr->pval = 1;

						/* Notice */
						if (!p_ptr->blind)
						{
							angtk_flicker(); /* TNB */
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

					/* Some guaranteed damage. */
					take_hit(damage / 3 + 1, ddesc);

					/* Special damage, reduced greatly by resists. */
					acid_dam(2 * damage / 3, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_ACID);

					break;
				}

				case RBE_ELEC:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are struck by electricity!");

					/* Some guaranteed damage. */
					take_hit(damage / 3 + 1, ddesc);

					/* Special damage, reduced greatly by resists. */
					elec_dam(2 * damage / 3, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_ELEC);

					break;
				}

				case RBE_FIRE:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are enveloped in flames!");

					/* Some guaranteed damage. */
					take_hit(damage / 3 + 1, ddesc);

					/* Special damage, reduced greatly by resists. */
					fire_dam(2 * damage / 3, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FIRE);

					break;
				}

				case RBE_COLD:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are covered with frost!");

					/* Some guaranteed damage. */
					take_hit(damage / 3 + 1, ddesc);

					/* Special damage, reduced greatly by resists. */
					cold_dam(2 * damage / 3, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_COLD);

					break;
				}

					/* Blindness is no longer fully cumulative. */
				case RBE_BLIND:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "blind" */
					if (!p_ptr->resist_blind)
					{
						if (p_ptr->blind)
						{
							if (set_blind(p_ptr->blind + 6 +
									randint(rlev / 2)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_blind(12 + randint(rlev)))
							{
								obvious = TRUE;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_BLIND);

					break;
				}

					/* Confusion is no longer fully cumulative. */
				case RBE_CONFUSE:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "confused" */
					if (!p_ptr->resist_confu)
					{
						if (p_ptr->confused)
						{
							if (set_confused(p_ptr->confused + 2 +
									randint(rlev / 2)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_confused(5 + randint(rlev)))
							{
								obvious = TRUE;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_CONFU);

					break;
				}

					/* Fear is no longer fully cumulative. */
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
					else if (rand_int(100) < p_ptr->skill_sav)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (p_ptr->afraid)
						{
							if (set_afraid(p_ptr->afraid + 2 +
									randint(rlev / 2)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_afraid(6 + randint(rlev)))
							{
								obvious = TRUE;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FEAR_SAVE);

					break;
				}

					/* Paralyzation is no longer fully cumulative. */
				case RBE_PARALYZE:
				{
					/* Hack -- Prevent perma-paralysis via damage */
					if (p_ptr->paralyzed && (damage < 1))
						damage = 1;

					/* Take damage */
					take_hit(damage, ddesc);

					/* Increase "paralyzed" */
					if (p_ptr->free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(100) < p_ptr->skill_sav)
					{
						msg_print("You resist the effects!");
						obvious = TRUE;
					}
					else
					{
						if (p_ptr->paralyzed)
						{
							if (set_paralyzed(p_ptr->paralyzed + 2 +
									randint(rlev / 6)))
							{
								obvious = TRUE;
							}
						}
						else
						{
							if (set_paralyzed(4 + randint(rlev / 2)))
							{
								obvious = TRUE;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, LRN_FREE_SAVE);

					break;
				}

				case RBE_LOSE_STR:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_STR))
						obvious = TRUE;

					break;
				}

				case RBE_LOSE_INT:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_INT))
						obvious = TRUE;

					break;
				}

				case RBE_LOSE_WIS:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_WIS))
						obvious = TRUE;

					break;
				}

				case RBE_LOSE_DEX:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_DEX))
						obvious = TRUE;

					break;
				}

				case RBE_LOSE_CON:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_CON))
						obvious = TRUE;

					break;
				}

				case RBE_LOSE_CHR:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stat) */
					if (do_dec_stat(A_CHR))
						obvious = TRUE;

					break;
				}

				case RBE_LOSE_ALL:
				{
					/* Take damage */
					take_hit(damage, ddesc);

					/* Damage (stats) */
					if (do_dec_stat(A_STR))
						obvious = TRUE;
					if (do_dec_stat(A_DEX))
						obvious = TRUE;
					if (do_dec_stat(A_CON))
						obvious = TRUE;
					if (do_dec_stat(A_INT))
						obvious = TRUE;
					if (do_dec_stat(A_WIS))
						obvious = TRUE;
					if (do_dec_stat(A_CHR))
						obvious = TRUE;

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

					/* Radius 6 earthquake centered at the monster */
					if (damage > 23)
						sound(SNDGRP_EVENT, SND_SPELL_SHATTERQUAKE, 0);	/* ALLOW_SOUND */
					if (damage > 23)
						earthquake(m_ptr->fy, m_ptr->fx, 6, FALSE);

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
						s32b d =
							damroll(10,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						sound(SNDGRP_EVENT, SND_EXPER_DRAIN, 0); /* ALLOW_SOUND */
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
						s32b d =
							damroll(20,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						sound(SNDGRP_EVENT, SND_EXPER_DRAIN, 0); /* ALLOW_SOUND */
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
						s32b d =
							damroll(40,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						sound(SNDGRP_EVENT, SND_EXPER_DRAIN, 0); /* ALLOW_SOUND */
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
						s32b d =
							damroll(80,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						sound(SNDGRP_EVENT, SND_EXPER_DRAIN, 0); /* ALLOW_SOUND */
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
					case 0:
						k = 0;
						break;
					case 1:
						k = randint(5);
						break;
					case 2:
						k = randint(5) + 5;
						break;
					case 3:
						k = randint(20) + 20;
						break;
					case 4:
						k = randint(50) + 50;
						break;
					case 5:
						k = randint(100) + 100;
						break;
					case 6:
						k = 300;
						break;
					default:
						k = 500;
						break;
				}

				/* Apply the cut */
				if (k)
					(void) set_cut(p_ptr->cut + k);
			}

			/* Handle stun.  Reduced in Oangband */
			if (do_stun)
			{
				int k = 0;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0:
						k = 0;
						break;
					case 1:
						k = randint(5);
						break;
					case 2:
						k = randint(8) + 8;
						break;
					case 3:
						k = randint(15) + 15;
						break;
					case 4:
						k = randint(25) + 25;
						break;
					case 5:
						k = randint(35) + 35;
						break;
					case 6:
						k = 60;
						break;
					default:
						k = 100;
						break;
				}

				/* Apply the stun */
				if (k)
					(void) set_stun(p_ptr->stun + k);
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
