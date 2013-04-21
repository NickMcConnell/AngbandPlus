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
	if (dam < total * 19 / 20)
		return (0);

	/* Weak blows rarely work */
	if ((dam < 20) && (rand_int(100) >= dam))
		return (0);

	/* Perfect damage */
	if (dam == total)
		max++;

	/* Super-charge */
	if (dam >= 20)
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
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level)
{
	int i, k, ac;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10)
		return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

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
 * Hack -- possible disgusting messages
 */

static cptr desc_disgust[] = {
	"slobbers all over you.",
	"belches loudly.",
	"flatulates.",
	"sucks on a finger.",
	"scratches an armpit.",
	"laughs insanely at a private joke.",
	"picks his nose.",
	"chews on a fingernail.",
	"spits at you.",
	"sniffs himself.",
	"picks crumbs off the dungeon floor.",
	"eats a rotten chunk of kobold meat."
};



/*
 * Select a monster ``saying''.
 */
cptr get_monster_saying(monster_race * r_ptr)
{
	if (!(r_ptr->flags2 & RF2_SMART || r_ptr->sayings_inx ||
			r_ptr->num_sayings) || r_ptr->flags3 & RF3_SILENT)
		return NULL;


	if (r_ptr->sayings_inx)
	{
		return get_monster_saying(&r_info[r_ptr->sayings_inx]);

	}
	else if (r_ptr->num_sayings)
	{
		cptr say = sayings_text + r_ptr->sayings;
		int foo = rand_int(r_ptr->num_sayings);

		while (foo)
		{
			say = strchr(say, '\0') + 1;
			foo--;
		}

		return say;

	}
	else
	{
		return get_random_line("insult.txt");
	}

	return NULL;
}


/*
 * Hack -- convert the RBE_* form of effect to the GF_* form of the 
 * effect. 
 */

static s16b attack_effect_to_spell_type(s16b effect)
{
	switch (effect)
	{
		case RBE_HURT:
			return GF_MANA;
		case RBE_POISON:
			return GF_POIS;
		case RBE_UN_POWER:
		case RBE_UN_BONUS:
			return GF_DISENCHANT;
		case RBE_ACID:
			return GF_ACID;
		case RBE_ELEC:
			return GF_ELEC;
		case RBE_FIRE:
			return GF_FIRE;
		case RBE_COLD:
			return GF_COLD;
		case RBE_BLIND:
			return GF_LITE;
		case RBE_CONFUSE:
			return GF_CONFUSION;
		case RBE_TERRIFY:
			return GF_DISP_ALL;
		case RBE_PARALYZE:
			return GF_SLEEP;
		case RBE_LOSE_STR:
			return GF_DEC_STR;
		case RBE_LOSE_INT:
			return GF_DEC_INT;
		case RBE_LOSE_WIS:
			return GF_DEC_WIS;
		case RBE_LOSE_DEX:
			return GF_DEC_DEX;
		case RBE_LOSE_CON:
			return GF_DEC_CON;
		case RBE_LOSE_CHR:
			return GF_DEC_CHR;
		case RBE_LOSE_ALL:
			return GF_RUINATION;
		case RBE_SHATTER:
			return GF_QUAKE;
		case RBE_EXP_10:
		case RBE_EXP_20:
		case RBE_EXP_40:
		case RBE_EXP_80:
			return GF_DRAIN;
		case RBE_INSANITY:
			return GF_CAUSE_INSANITY;

		default:
			return 0;
	}
}



/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int ap_cnt;

	int j, tmp, ac, rlev;
	int do_cut, do_stun;

	s32b gold;

	object_type *o_ptr;

	char o_name[80];

	char m_name[80];

	char ddesc[80];

	bool blinked;

	bool do_explosion = FALSE;
	bool did_attack = TRUE;

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

	/* ``Smart'' monsters speak insults. */
	/* Monsters with custom sayings will always say something. */
	/* Hack with the stupid brace trick. */
	if (magik(20) && monsters_speak)
	{
		cptr insult = get_monster_saying(r_ptr);

		if (insult)
		{
			msg_format("%^s %s", m_name, insult);
		}
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



		/* Extract the attack "power" */
		switch (effect)
		{
			case RBE_HURT:
				power = 60;
				break;
			case RBE_POISON:
				power = 5;
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
				power = 0;
				break;
			case RBE_ELEC:
				power = 10;
				break;
			case RBE_FIRE:
				power = 10;
				break;
			case RBE_COLD:
				power = 10;
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
			case RBE_INSANITY:
				power = 60;
				break;
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			/* Always disturbing */
			disturb(1, 0);


			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) && (r_ptr->flags3 & (RF3_EVIL)) &&
				(p_ptr->lev >= rlev) &&
				((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_EVIL);
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

				case RBM_RIDDLE:
				{
					act = "speaks in riddles.";
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

				case RBM_DISGUST:
				{
					act = desc_disgust[rand_int(11)];
					break;
				}

				case RBM_PROJECT:
				{
					act = "projects strange thoughts into your head!";
					break;
				}

				case RBM_HALT:
				{
					act = "shouts: ``Halt, Halt!''.";
					break;
				}

				case RBM_EXPLODE:
				{
					act = "explodes!";

					do_explosion = TRUE;

					break;
				}

			}

			/* Message */
			if (act)
				msg_format("%^s %s", m_name, act);

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

			/* Hack -- produce an explosion, turn off other effects. */
			if (do_explosion)
			{
				project(m_idx, 5, m_ptr->fy, m_ptr->fx, damage,
					attack_effect_to_spell_type(effect),
					PROJECT_BLAST | PROJECT_KILL | PROJECT_ITEM |
					PROJECT_GRID);

				effect = 0;
			}


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
						if (set_poisoned(p_ptr->poisoned + randint(rlev) +
								5))
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
						if (apply_disenchant(0))
							obvious = TRUE;
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
					for (o_ptr = inventory; o_ptr != NULL;
						o_ptr = o_ptr->next)
					{

						/* Ugly hack -- try to pass a saving throw. */
						int foo =
							o_ptr->weight * 100 / p_ptr->total_weight;

						if (randint(100) > foo)
							continue;

						/* Skip non-objects */
						if (!o_ptr->k_idx)
							continue;

						/* Drain charged wands/staffs */
						if (((o_ptr->tval == TV_STAFF) ||
								(o_ptr->tval == TV_WAND)) && (o_ptr->pval))
						{
							/* Message */
							mprint(MSG_WARNING,
								"Energy drains from your pack!");

							/* Obvious */
							obvious = TRUE;

							/* Heal */
							j = rlev;
							m_ptr->hp += j * o_ptr->pval * o_ptr->number;

							if (m_ptr->hp > m_ptr->maxhp)
								m_ptr->hp = m_ptr->maxhp;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx)
								p_ptr->redraw |= (PR_HEALTH);

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
						(rand_int(100) <
	   (adj_dex_safe[p_ptr->stat_ind[A_DEX]] + p_ptr->lev)))
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
						gold = (p_ptr->au / 10) + randint(25);

						if (gold < 2)
							gold = 2;

						if (gold > 5000)
							gold = (p_ptr->au / 20) + randint(3000);

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
							msg_format("%ld coins were stolen!",
								(long) gold);
						}
						else
						{
							msg_print("Your purse feels lighter.");
							msg_print("All of your coins were stolen!");
						}

						/* Redraw gold */
						p_ptr->redraw |= (PR_GOLD);

						/* Window stuff */
						p_ptr->window |= (PW_SPELL | PW_PLAYER);

						/* Blink away */
						blinked = TRUE;
					}

					break;
				}

				case RBE_EAT_ITEM:
				{
					object_type *i_ptr;

					/* Take damage */
					take_hit(damage, ddesc);

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->paralyzed &&
						(rand_int(100) <
	   (adj_dex_safe[p_ptr->stat_ind[A_DEX]] + p_ptr->lev)))
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
					for (o_ptr = inventory; o_ptr != NULL;
						o_ptr = o_ptr->next)
					{

						/* Ugly hack -- try to pass a saving throw. */
						int foo =
							o_ptr->weight * 100 / p_ptr->total_weight;
						int i;
						bool skip = FALSE;

						if (randint(100) > foo)
							continue;

						/* Skip non-objects */
						if (!o_ptr->k_idx)
							continue;

						/* Skip artifacts */
						if (artifact_p(o_ptr))
							continue;

						for (i = 0; i < EQUIP_MAX; i++)
						{
							if (equipment[i] == o_ptr)
							{
								if (protect_equipment)
									skip = TRUE;

								if (cursed_p(o_ptr))
									skip = TRUE;
							}
						}

						if (skip)
							continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 3);

						/* Message */
						mformat(MSG_WARNING, "%sour %s was stolen!",
							((o_ptr->number > 1) ? "One of y" : "Y"),
							o_name);

						if (o_ptr->number > 1)
						{
							i_ptr = object_unabsorb(o_ptr, 1);

						}
						else
						{
							remove_from_stack(o_ptr);
							i_ptr = o_ptr;
						}

						/* Carry the object */
						monster_inven_carry(m_ptr, i_ptr);

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
					for (o_ptr = inventory; o_ptr != NULL;
						o_ptr = o_ptr->next)
					{
						/* Ugly hack -- try to pass a saving throw. */
						int foo =
							o_ptr->weight * 100 / p_ptr->total_weight;

						/* Skip non-food objects */
						if (o_ptr->tval != TV_FOOD)
							continue;

						if (randint(100) > foo)
							continue;

						/* Skip non-objects */
						if (!o_ptr->k_idx)
							continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 0);

						/* Message */
						mformat(MSG_WARNING, "Your %s %s eaten!", o_name,
							((o_ptr->number > 1) ? "were" : "was"));

						remove_object(o_ptr);

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
					o_ptr = equipment[EQUIP_LITE];

					if (!o_ptr)
						break;

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
						if (set_blind(p_ptr->blind + 10 + randint(rlev)))
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
						if (set_confused(p_ptr->confused + 3 +
								randint(rlev)))
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
					else if (rand_int(100) < p_ptr->skill_sav)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (set_afraid(p_ptr->afraid + 3 + randint(rlev)))
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
						if (set_paralyzed(p_ptr->paralyzed + 3 +
								randint(rlev)))
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

				case RBE_INSANITY:
				{
					obvious = TRUE;

					take_sanity_hit(damage, ddesc);
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
						fire_mega_blast(m_ptr->fy, m_ptr->fx, GF_QUAKE, 8,
							damroll(2, 5));
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
						s32b d = damroll(10,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							mprint(MSG_WARNING,
								"You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							mprint(MSG_WARNING,
								"You feel your life draining away!");
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
						s32b d = damroll(20,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							mprint(MSG_WARNING,
								"You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							mprint(MSG_WARNING,
								"You feel your life draining away!");
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
						s32b d = damroll(40,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							mprint(MSG_WARNING,
								"You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							mprint(MSG_WARNING,
								"You feel your life draining away!");
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
						s32b d = damroll(80,
							6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							mprint(MSG_WARNING,
								"You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							mprint(MSG_WARNING,
								"You feel your life draining away!");
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
			/* Handle stun */
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
						k = randint(10) + 10;
						break;
					case 3:
						k = randint(20) + 20;
						break;
					case 4:
						k = randint(30) + 30;
						break;
					case 5:
						k = randint(40) + 40;
						break;
					case 6:
						k = 100;
						break;
					default:
						k = 200;
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

			did_attack = FALSE;

		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					r_ptr->r_blows[ap_cnt]++;
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

	/* Kamikaze monsters. */
	if (r_ptr->flags7 & RF7_KAMIKAZE && did_attack)
	{

		/* Note -- no message! */
		delete_monster_idx(m_idx);
	}

	/* Always notice cause of death */
	if (p_ptr->is_dead && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

	/* Assume we attacked */
	return (TRUE);
}
