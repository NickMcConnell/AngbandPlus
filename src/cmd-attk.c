/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Determine if the player "hits" a monster (ranged (firearm) combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* 
	 * 12:00 PM:      Fufie: 10 is 5% + 5%
	 * 12:00 PM:    lemming: if k<5 it returns true
	 * 12:00 PM:      Fufie: 5% to hit and 5% to miss
	 * 12:00 PM:   Mynstral: if the random number is less than 10, it leaves the function
	 * 12:00 PM:    lemming: if not, it's <10 but >5, hence false, and a hit
	 * 12:00 PM:   Mynstral: if it's less than 5, it's true (hit, i assume)
	 */
	/* Thanks for the help guys. I'm retarded. ;-p -CCC */
	/* Note to self, it returns the value (k<5) Because that's what follows */
	/* the return statement - duh -CCC */
	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor */
	if ((chance > 0) && (rand_int(chance) >= (ac))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_norm(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Penalize invisible targets */
	if (!vis) chance = chance / 2;

	/* Power competes against armor */
	if ((chance > 0) && (rand_int(chance) >= (ac))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
sint critical_shot(int weight, int plus, int dam)
{
	int i, k, punch, critplus;
	punch = 1;
	critplus = 1;

	if (p_ptr->skills[SK_INTER_SHOOTING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_INTER_SHOOTING].skill_rank;
	}
	if (p_ptr->skills[SK_ADV_SHOOTING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_ADV_SHOOTING].skill_rank;
	}
	if (p_ptr->skills[SK_MASTER_SHOOTING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_MASTER_SHOOTING].skill_rank;
	}
	if (p_ptr->skills[SK_CRIT_SHOT].skill_max > 0)
	{
		punch *= p_ptr->skills[SK_CRIT_SHOT].skill_rank / 2;
	}
	if (p_ptr->skills[SK_CRIT_SHOT].skill_max > 0)
	{
		critplus = p_ptr->skills[SK_CRIT_SHOT].skill_rank;
	}

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (punch));

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = weight + randint(500) + randint((20 * critplus));

		if (k < 500)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
	}

	return (dam);
}

sint critical_throw(int weight, int plus, int dam)
{
	int i, k, punch, critplus;
	punch = 1;
	critplus = 1;

	if (p_ptr->skills[SK_ADV_THROWING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_ADV_THROWING].skill_rank;
	}
	if (p_ptr->skills[SK_MASTER_THROWING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_MASTER_THROWING].skill_rank;
	}
	if (p_ptr->skills[SK_CRIT_THROW].skill_max > 0)
	{
		punch *= p_ptr->skills[SK_CRIT_THROW].skill_rank / 2;
	}
	if (p_ptr->skills[SK_CRIT_THROW].skill_max > 0)
	{
		critplus = p_ptr->skills[SK_CRIT_THROW].skill_rank;
	}

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (punch));

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = weight + randint(500) + randint((20 * critplus));

		if (k < 500)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
	}

	return (dam);
}


/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
sint critical_norm(int weight, int plus, int dam)
{
	int i, k, punch, critplus;
	punch = 1;
	critplus = 1;
	
	if (p_ptr->skills[SK_INTER_COMBAT].skill_max > 0)
	{
		punch += p_ptr->skills[SK_INTER_COMBAT].skill_rank;
	}
	if (p_ptr->skills[SK_ADV_COMBAT].skill_max > 0)
	{
		punch += p_ptr->skills[SK_ADV_COMBAT].skill_rank;
	}
	if (p_ptr->skills[SK_MASTER_COMBAT].skill_max > 0)
	{
		punch += p_ptr->skills[SK_MASTER_COMBAT].skill_rank;
	}
	if (p_ptr->skills[SK_CRIT_STRIKE].skill_max > 0)
	{
		punch *= p_ptr->skills[SK_CRIT_STRIKE].skill_rank / 2;
	}
	if (p_ptr->skills[SK_CRIT_STRIKE].skill_max > 0)
	{
		critplus = p_ptr->skills[SK_CRIT_STRIKE].skill_rank;
	}

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (punch));

	/* Chance */
	if (randint(5000) <= i)
	{
		k = weight + randint(650) + randint((20 * critplus));

		if (k < 400)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else if (k < 900)
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
		else if (k < 1300)
		{
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam + 20;
		}
		else
		{
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
		}
	}

	return (dam);
}



/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
sint tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_AMMO:
		case TV_BULLET:
		case TV_SHOT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Slay Animal */
			if ((f1 & (TR1_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_ANIMAL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_EVIL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DEMON);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Automata */
			if ((f1 & (TR1_SLAY_AUTOMATA)) &&
			    (r_ptr->flags3 & (RF3_AUTOMATA)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_AUTOMATA);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_TROLL);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_GIANT);
				}

				if (mult < 3) mult = 3;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 3) mult = 3;
			}
			/* Slay Alien */
			if ((f1 & (TR1_SLAY_ALIEN)) &&
			    (r_ptr->flags3 & (RF3_ALIEN)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_ALIEN);
				}

				if (mult < 3) mult = 3;
			}
			/* Slay Beastman */
			if ((f1 & (TR1_SLAY_BEASTMAN)) &&
			    (r_ptr->flags3 & (RF3_BEASTMAN)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_BEASTMAN);
				}

				if (mult < 3) mult = 3;
			}
			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 5) mult = 5;
			}


			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_ACID);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Elec) */
			if (f1 & (TR1_BRAND_ELEC))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ELEC))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Fire) */
			if (f1 & (TR1_BRAND_FIRE))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Cold) */
			if (f1 & (TR1_BRAND_COLD))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_COLD))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			/* Brand (Poison) */
			if (f1 & (TR1_BRAND_POIS))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3) mult = 3;
				}
			}

			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
}




/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static int check_hit(int power)
{
	int k, ac;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if ((power > 0) && (randint(power) >= (ac * 3 / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Handle player hitting a real trap
 */
void hit_trap(int y, int x)
{
	int i, num, dam;

	cptr name = "a trap";


	/* Disturb the player */
	disturb(0, 0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		case FEAT_TRAP_HEAD + 0x00:
		{
			msg_print("You fall through a trap door!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently down to the next level.");
			}
			else
			{
				dam = damroll(2, 8);
				take_hit(dam, name);
			}

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{
			msg_print("You fall into a pit!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently to the bottom of the pit.");
			}
			else
			{
				dam = damroll(2, 6);
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");
					}
					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
			msg_print("You are enveloped in a cloud of smoke!");
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(y, x, p_ptr->depth, 0, FALSE, FALSE);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			msg_print("You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			msg_print("You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)set_slow(p_ptr->slow + rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x09:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_MUS);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0A:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_AGI);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0B:
		{
			if (check_hit(125))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void)do_dec_stat(A_VIG);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("You are surrounded by a black gas!");
			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->resist_confu)
			{
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("You are surrounded by a pungent green gas!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("You are surrounded by a strange white mist!");
			if (!p_ptr->free_act)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
			}
			break;
		}
	}
}

void touch_zap_player(monster_type *m_ptr)
{
	int aura_damage = 0;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags2 & RF2_AURA_FIRE)
	{
		if (!p_ptr->immune_fire)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very hot!");

			if (p_ptr->oppose_fire) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_fire) aura_damage = (aura_damage + 2) / 3;

			take_hit(aura_damage, aura_dam);
			r_ptr->flags2 |= RF2_AURA_FIRE;
			handle_stuff();
		}
	}

	if (r_ptr->flags2 & RF2_AURA_COLD)
	{
		if (!p_ptr->immune_cold)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very cold!");

			if (p_ptr->oppose_cold) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_cold) aura_damage = (aura_damage + 2) / 3;

			take_hit(aura_damage, aura_dam);
			r_ptr->flags3 |= RF2_AURA_COLD;
			handle_stuff();
		}
	}

	if (r_ptr->flags2 & RF2_AURA_ELEC)
	{
		if (!p_ptr->immune_elec)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			if (p_ptr->oppose_elec) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_elec) aura_damage = (aura_damage + 2) / 3;

			msg_print("You get zapped!");
			take_hit(aura_damage, aura_dam);
			r_ptr->flags2 |= RF2_AURA_ELEC;
			handle_stuff();
		}
	}
}


static void natural_attack(s16b m_idx, int attack, bool *fear)
{
	int         k, bonus, chance;
	int         n_weight = 0;
	
	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	

	char       	m_name[80];
	int        	dss, ddd;
	char       	*atk_desc;

	switch (attack)
	{
		case MUT3_SCOR_TAIL:
			dss = 3;
			ddd = 7;
			n_weight = 5;
			atk_desc = "tail";
			p_ptr->energy_use = 30;
			break;
		case MUT3_HORNS:
			dss = 2;
			ddd = 6;
			n_weight = 15;
			p_ptr->energy_use = 20;
			atk_desc = "horns";
			break;
		case MUT3_BEAK:
			dss = 2;
			ddd = 4;
			n_weight = 5;
			p_ptr->energy_use = 20;
			atk_desc = "beak";
			break;
		case MUT3_TUSKS:
			dss = 2;
			ddd = 6;
			n_weight = 30;
			p_ptr->energy_use = 30;
			atk_desc = "tusks";
			break;
		case MUT3_CLAWS:
			dss = 2;
			ddd = 3;
			n_weight = 5;
			p_ptr->energy_use = 10;
			atk_desc = "claws";
			break;
		case MUT3_TENTACLES:
			dss = 3;
			ddd = 3;
			n_weight = 20;
			p_ptr->energy_use = 40;
			atk_desc = "tentacles";
			break;
		default:
			dss = ddd = n_weight = 1;
			atk_desc = "undefined body part";
	}


	/* Get the monster */
	m_ptr = &m_list[m_idx];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h;
	/* Note the basic combat skill doesn't get us very far */
	/* natural chance being no higher than 40 - You won't be hitting very often */
	/* Without other combat skills (forthcoming) */
	chance = ((p_ptr->skills[SK_TOHIT].skill_rank * 5) + (bonus * BTH_PLUS_ADJ));
	if (p_ptr->skills[SK_INTER_COMBAT].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_INTER_COMBAT].skill_rank * 4;
	}
	if(p_ptr->skills[SK_ADV_COMBAT].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_ADV_COMBAT].skill_rank * 3;
	}
	if(p_ptr->skills[SK_MASTER_COMBAT].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_MASTER_COMBAT].skill_rank * 2;
	}

	/* Test for hit */
	if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_HIT);

		msg_format("You hit %s with your %s.", m_name, atk_desc);

		k = damroll(ddd, dss);
		k = critical_norm(n_weight, p_ptr->to_h, k);

		/* Apply the player damage bonuses */
		k += p_ptr->to_d;   

		/* No negative damage */
		if (k < 1) k = 1;

		/* Unusual damage */
		if (p_ptr->muta5 & MUT5_TWISTED)
		{
			if (turn % 2) k *= 2;
			else k /= 2;
		}

		/* Complex message */
		if (p_ptr->wizard)
		{
			msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
		}

		/* Damage, check for fear and mdeath */
		switch (attack)
		{
			case MUT3_SCOR_TAIL:
			{
				project(-1, 0, m_ptr->fy, m_ptr->fx, k, GF_POIS, PROJECT_KILL);
				break;
			}
			case MUT3_HORNS:
			{
				mon_take_hit(m_idx, k, fear, NULL);
				break;
			}
			case MUT3_BEAK:
			{
				mon_take_hit(m_idx, k, fear, NULL);
				break;
			}
			case MUT3_TUSKS:
			{
				mon_take_hit(m_idx, k, fear, NULL);
				break;
			}
			case MUT3_CLAWS:
			{
				mon_take_hit(m_idx, k, fear, NULL);
				break;
			}
			case MUT3_TENTACLES:
			{
				project(-1, 0, m_ptr->fy, m_ptr->fx, k, GF_INERTIA, PROJECT_KILL);
				break;
			}
			default:
			{
				mon_take_hit(m_idx, k, fear, NULL);
				break;
			}
		}
	
		touch_zap_player(m_ptr);
	
	}
	/* Player misses */
	else
	{
		/* Sound */
		sound(SOUND_MISS);

		/* Message */
		message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
	
	}
}


/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	int num = 0, k, bonus, chance, force, forcechance;
	int dammult = 2;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	object_type *o_ptr;

	char m_name[80];

	bool fear = FALSE;	
	bool do_quake = FALSE;
	bool no_extra = FALSE;

	/* Get the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Disturb the player */
	disturb(0, 0);

	/* Disturb the monster */
	m_ptr->csleep = 0;

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);

	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		msg_format("You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}

	/* Get the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;

	/* add in the accurate strike skill to the bonus */
	if (p_ptr->skills[SK_ACC_STRIKE].skill_max > 0)
	{
		bonus += p_ptr->skills[SK_ACC_STRIKE].skill_rank;
	}
	if (bonus < 0) bonus = 0;
	
	/* Calculate the chance to hit */
	chance = 1;
	/* this should always be true, just checking */
	if (p_ptr->skills[SK_TOHIT].skill_max > 0)
	{
		chance = ((p_ptr->skills[SK_TOHIT].skill_rank * 5) + (bonus * BTH_PLUS_ADJ));
	}

	/* add in the relevant combat skills */
	if (p_ptr->skills[SK_INTER_COMBAT].skill_max > 0)
	{
		chance += p_ptr->skills[SK_INTER_COMBAT].skill_rank * 4;
	}
	if(p_ptr->skills[SK_ADV_COMBAT].skill_max > 0)
	{
		chance += p_ptr->skills[SK_ADV_COMBAT].skill_rank * 3;
	}
	if(p_ptr->skills[SK_MASTER_COMBAT].skill_max > 0)
	{
		chance += p_ptr->skills[SK_MASTER_COMBAT].skill_rank * 2;
	}
	if (p_ptr->skills[SK_AXE].skill_max > 0 && o_ptr->tval == TV_POLEARM)
	{
		chance += p_ptr->skills[SK_AXE].skill_rank * 4;
	}
	if (p_ptr->skills[SK_BLUNT].skill_max > 0 && o_ptr->tval == TV_HAFTED)
	{
		chance += p_ptr->skills[SK_BLUNT].skill_rank * 4;
	}
	if (p_ptr->skills[SK_SWORD].skill_max > 0 && o_ptr->tval == TV_SWORD)
	{
		chance += p_ptr->skills[SK_SWORD].skill_rank * 4;
	}
	if (chance < 1) chance = 1;
	forcechance = chance;

	/* Attack once for each legal blow */
/*	while (num++ < p_ptr->num_blow) */
/*	{ */
	/* take a fractional turn */
	p_ptr->energy_use = 100 / p_ptr->num_blow;
	
	/* Test for hit */
	if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
	{
		/* This is a debugging message msg_print("You Hit!");*/
		/* Hack -- bare hands do one damage */
		k = 1;

		/* Handle normal weapon */
		if (o_ptr->k_idx)
		{
			k = damroll(o_ptr->dd, o_ptr->ds);
			k = tot_dam_aux(o_ptr, k, m_ptr);
			/* This looks like a hack for earthquake */
			if (p_ptr->impact && (k > 50)) do_quake = TRUE;
			
			/* check for critical */
			k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			
			/* add in object damage bonus */
			k += o_ptr->to_d;
		}
		
		/* Apply the player damage bonuses */
		k += p_ptr->to_d;
		
		/* Apply the vicious strike bonuses */
		if (p_ptr->skills[SK_VICIOUS_STRIKE].skill_max > 0)
		{
			k += p_ptr->skills[SK_VICIOUS_STRIKE].skill_rank;
		}
		
		/* handle the force of the weapon */
		/* larger heavier weapons have more force behind them */
		/* if you have a good weapons skill, you can get these extra blows */
		if(o_ptr->force > 0)
		{
			if (p_ptr->skills[SK_POWER_STRIKE].skill_max > 0)
			{
				forcechance += p_ptr->skills[SK_POWER_STRIKE].skill_rank * 3;
				dammult += p_ptr->skills[SK_POWER_STRIKE].skill_rank / 6;
			}
			for (force = 0; force < o_ptr->force; force++)
			{
				if (test_hit_norm(forcechance, (r_ptr->ac + r_ptr->level + (r_ptr->ac * (force * (3/2)))), m_ptr->ml))
				{
					dammult++;
					/* this code is to see how often the extra force hits hit */
					/* debugging code msg_format("force #%d hits", force);*/					
				
				}
			}
		}
		/* damnult is 2 and increases by 1 for each force blow */
		k *= dammult;
		/* divide the new k(damage total) by three to increase damage */
		k /= 2;
		/* No negative damage */
		if (k < 1) k = 1;

		/* Message */
		if (dammult < 3) message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.", m_name);
		else if (dammult < 4) message_format(MSG_HIT, m_ptr->r_idx, "You smash %s.", m_name);
		else if (dammult < 5) message_format(MSG_HIT, m_ptr->r_idx, "You crush %s.", m_name);
		else if (dammult < 6) message_format(MSG_HIT, m_ptr->r_idx, "You *bash* %s.", m_name);
		else if (dammult < 7) message_format(MSG_HIT, m_ptr->r_idx, "You *clobber* %s.", m_name);
		else if (dammult < 8) message_format(MSG_HIT, m_ptr->r_idx, "You *mangle* %s.", m_name);
		else if (dammult < 9) message_format(MSG_HIT, m_ptr->r_idx, "You *pulverise* %s.", m_name);
		else if (dammult < 10) message_format(MSG_HIT, m_ptr->r_idx, "You *demolish* %s.", m_name);
		else if (dammult < 11) message_format(MSG_HIT, m_ptr->r_idx, "You *decimate* %s.", m_name);
		else if (dammult < 12) message_format(MSG_HIT, m_ptr->r_idx, "You *annihilate* %s.", m_name);
		else message_format(MSG_HIT, m_ptr->r_idx, "You atomize %s.", m_name);

		/* Complex message */
		if (p_ptr->wizard)
		{
			msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
		}
		
		/* Damage, check for fear and death */
		if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))

		/* Anger the monster */
		if (k > 0) anger_monster(m_ptr);

		/* Damage the player from aura */
		touch_zap_player(m_ptr);

		/* Confusion attack */
		if (p_ptr->confusing)
		{
			/* Cancel glowing hands */
			p_ptr->confusing = FALSE;

			/* Message */
			msg_print("Your hands stop glowing.");

			/* Confuse the monster */
			if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				msg_format("%^s is unaffected.", m_name);
			}
			else if (rand_int(100) < r_ptr->level)
			{
				msg_format("%^s is unaffected.", m_name);
			}
			else
			{
				msg_format("%^s appears confused.", m_name);
				m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
			}
		}
	}

	/* Player misses */
	else
	{
		/* Message */
		message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
	}
/*	} */

	/* Mutations which yield extra 'natural' attacks */
	if (!no_extra)
	{
		if ((p_ptr->muta3 & MUT3_HORNS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT3_HORNS, &fear);
		if ((p_ptr->muta3 & MUT3_CLAWS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT3_CLAWS, &fear);
		if ((p_ptr->muta3 & MUT3_BEAK) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT3_BEAK, &fear);
		if ((p_ptr->muta3 & MUT3_SCOR_TAIL) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT3_SCOR_TAIL, &fear);
		if ((p_ptr->muta3 & MUT3_TUSKS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT3_TUSKS, &fear);
		if ((p_ptr->muta3 & MUT3_TENTACLES) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT3_TENTACLES, &fear);
	}

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand */
	if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10);
}

/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(const object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_TONIC:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_MECHANISM:
		case TV_TEXT:
		case TV_SKELETON:
		{
			return (50);
		}

		/* Always break */
		case TV_AMMO:
		case TV_SHOT:
		case TV_BULLET:
		{
			return (100);
		}

		/* Sometimes break */
		case TV_RAY:
		
		case TV_SPIKE:
		{
			return (25);
		}
	}

	/* Rarely break */
	return (10);
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_cmd_fire(void)
{
	int dir, item;
	int i, j, y, x, ty, tx;
	int tdam, tdis, thits, tmul;
	int bonus, chance;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_GUN];

	/* Require a usable launcher */
	if (!j_ptr->tval || !p_ptr->ammo_tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}


	/* Sound */
	sound(MSG_SHOOT);


	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from thrown object plus launcher bonus */
	tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d + j_ptr->to_d + p_ptr->skills[SK_VICIOUS_SHOT].skill_rank; 

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);

	/* Add in the accurate shot skill to the bonus */
	if (p_ptr->skills[SK_ACC_SHOT].skill_rank > 0)
	{
		bonus += p_ptr->skills[SK_ACC_SHOT].skill_rank;
	}

	chance = ((p_ptr->skills[SK_TOHIT_SHOOTING].skill_rank * 5) + (bonus * BTH_PLUS_ADJ));

	/* add in the relevant combat skills */
	if (p_ptr->skills[SK_INTER_SHOOTING].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_INTER_SHOOTING].skill_rank *4;
	}
	if(p_ptr->skills[SK_ADV_SHOOTING].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_ADV_SHOOTING].skill_rank * 3;
	}
	if(p_ptr->skills[SK_MASTER_SHOOTING].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_MASTER_SHOOTING].skill_rank * 2;
	}
	if (p_ptr->skills[SK_PISTOL].skill_rank > 0 && i_ptr->tval == TV_AMMO)
	{
		chance += p_ptr->skills[SK_PISTOL].skill_rank * 4;
	}
	if (p_ptr->skills[SK_RIFLE].skill_rank > 0 && i_ptr->tval == TV_BULLET)
	{
		chance += p_ptr->skills[SK_RIFLE].skill_rank * 4;
	}
	if (p_ptr->skills[SK_SHOTGUN].skill_rank > 0 && i_ptr->tval == TV_SHOT)
	{
		chance += p_ptr->skills[SK_SHOTGUN].skill_rank * 4;
	}

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base / default range */
	tdis = 5;
	/* Gotten based off the sval of the weapon. */
	switch (j_ptr->sval)
	{
		/* Pistol */
		case SV_DERRINGER:
		{
			tdis = 4;
			break;
		}
		case SV_32_REVOLVER:
		case SV_38_REVOLVER:
		case SV_41_REVOLVER:
		case SV_45_REVOLVER:
		{
			tdis = 8;
			break;
		}
		case SV_22_BOLT_ACTION:
		{
			tdis = 12;
			break;
		}
		/* Light Rifle */
		case SV_30_LEVER_ACTION:
		{
			tdis = 18;
			break;
		}
		/* Heavy Rifle */
		case SV_45_MARTINI_HENRY:
		case SV_COL_MORAN:
		case SV_303_LEE_ENFIELD:
		case SV_ELEPHANT_GUN:
		{
			tdis = 18;
			break;
		}
		/* Light Shotgun */
		case SV_20_GAGUE:
		{
			tdis = 12;
			break;
		}
		/* Heavy Shotgun */
		case SV_16_GAGUE:
		{
			tdis = 10;
			break;
		}
		case SV_12_GAGUE:
		{
			tdis = 8;
			break;
		}
		case SV_10_GAGUE:
		{
			tdis = 6;
			break;
		}
		break;	
	}
		
	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);


	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize distance travelled) */
			if (test_hit_fire(chance2, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_UNDEAD)) ||
				    (r_ptr->flags2 & (RF2_STUPID)) ||
				    (strchr("Evg", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);
				tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
						               "%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}



/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 *
 * Answers: Only if it has the THROW TR3 flag. yes. ...  yes.
 */
void do_cmd_throw(void)
{
	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdam, tdis;
	int mul, div, bonus;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;
	
	u32b f1, f2, f3; 

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;



	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Extract the flags */
	object_flags(i_ptr, &f1, &f2, &f3); 

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}


	/* Description */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	/* @STAT@ */
	tdis = (p_ptr->stat_use[A_MUS] / 4) * mul / div;
	if (p_ptr->skills[SK_ADV_THROWING].skill_rank > 0)
	{
		tdis += p_ptr->skills[SK_ADV_THROWING].skill_rank / 5;
	}
	if (p_ptr->skills[SK_MASTER_THROWING].skill_rank > 0)
	{
		tdis += p_ptr->skills[SK_MASTER_THROWING].skill_rank / 3;
	}

	/* Max distance of 12 */
	if (tdis > 12) tdis = 12;

	/* Hack -- Base damage from thrown object */
	/* The power throw skill should make throwing weapons competative */
	if (p_ptr->skills[SK_POWER_THROW].skill_max > 0)
	{
		tdam = (damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d) * (p_ptr->skills[SK_POWER_THROW].skill_rank / 4 + 1);
	}
	else tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d;	
	
	/* Weapons that aren't throwing weapons don't do a lot of damage */
	if (!(f3 & (TR3_THROW)))
	{
		tdam = (tdam / 10);
		if (tdam < 1) tdam = 1;
	}
	
	/* Chance of hitting */
	bonus = p_ptr->to_h;
	
	/* Throwing weapons get their bonus to hit added in */
	if (f3 & (TR3_THROW))
	{
		bonus += i_ptr->to_h;
	}
	
	/* Accuracy is added to the to hit bonuses, and egro mutiplied by BTH_PLUS_ADJ */
	if (p_ptr->skills[SK_ACC_THROW].skill_rank > 0)
	{
		bonus += p_ptr->skills[SK_ACC_THROW].skill_rank;
	}

	/* Base chance to hit */
	chance = ((p_ptr->skills[SK_TOHIT_THROWING].skill_rank * 5) + (bonus * BTH_PLUS_ADJ));

	/* Additional chance to hit based off of higher level skills */
	if(p_ptr->skills[SK_ADV_THROWING].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_ADV_THROWING].skill_rank * 5;
	}
	if(p_ptr->skills[SK_MASTER_THROWING].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_MASTER_THROWING].skill_rank * 4;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_UNDEAD)) ||
				    (r_ptr->flags2 & (RF2_STUPID)) ||
				    (strchr("Evg", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);
				tdam = critical_throw(i_ptr->weight, i_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
						               "%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	if (f2 & (TR2_RETURN))
	{
		
		msg_format("The %s returns to your hand.", o_name);
		inven_carry(i_ptr);
	}

	/* only items with the THROW flag are immune to destruction */
	else if (f3 & (TR3_THROW))
	{
		/* debugging message */
		msg_print("it drops to the ground.");
		/* Drop near that location */
		drop_near(i_ptr, 0, y, x);
		/* nothing */
	}
	else 
	{
		/* debugging message */
		msg_print("It smashes to the ground.");
		/* Chance of breakage (during attacks) */
		j = (hit_body ? breakage_chance(i_ptr) : 0);

		/* Drop (or break) near that location */
		drop_near(i_ptr, j, y, x);		
	}
	
	
	/* Impliment returning throwing weapons */
}



