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
 * Determine if the player "hits" a monster.
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 * A - Now return -1 if it was a 'near miss' prevented by armor
 */
int test_hit(int chance, int ac, int vis)
{
	int k, roll;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Penalize invisible targets */
	if (!vis) chance = chance / 2;

	/* Power competes against armor */
	if (chance > 0){
		roll = rand_int(chance);
		if (roll >= (ac * 3 / 4)) return 1;
		if (roll >= (ac / 2)) return -1;
	}

	/* Assume miss */
	return 0;
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
int critical_shot(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h_missile + plus) * 4) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = weight + randint(500);

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
int critical_norm(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h_melee + plus) * 5) + (p_ptr->lev * 3));

	if (p_ptr->twoh_weapon){
		i = (i * 11) / 9;
	} else {
		i = (i * 6) / 9;
	}

	/* Chance */
	if (randint(5000) <= i)
	{
		k = weight + randint(650);
		if (p_ptr->twoh_weapon){
			k = k + 100;
		}

		if (k < 400)
		{
			sound(MSG_HIT_GOOD);
			msg_print("It was a good hit!");
			dam = 3 * dam / 2;
		}
		else if (k < 700)
		{
			sound(MSG_HIT_GREAT);
			msg_print("It was a great hit!");
			dam = 2 * dam;
		}
		else if (k < 900)
		{
			sound(MSG_HIT_SUPERB);
			msg_print("It was a superb hit!");
			dam = 5 * dam / 2;
		}
		else if (k < 1300)
		{
			sound(MSG_HIT_HI_GREAT);
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam;
		}
		else
		{
			sound(MSG_HIT_HI_SUPERB);
			msg_print("It was a *SUPERB* hit!");
			dam = 7 * dam / 2;
		}


	}

	else
	{
		sound(MSG_HIT);
	}

	return (dam);
}

/*
 * new version of tot_dam_aux, for melee
 */

int melee_p_increment(const object_type *o_ptr, const monster_type *m_ptr){
	int brand_increment=0, slay_increment=0;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3, fn;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Some "weapons" do extra damage */
	switch (o_ptr->tval)
	{
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
					l_ptr->r_l_flags3 |= (RF3_ANIMAL);
				}

				if (slay_increment < 200) slay_increment = 200;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_EVIL);
				}

				if (slay_increment < 100) slay_increment = 100;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				}

				if (slay_increment < 170) slay_increment = 170;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DEMON);
				}

				if (slay_increment < 170) slay_increment = 170;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_ORC);
				}

				if (slay_increment < 200) slay_increment = 200;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_TROLL);
				}

				if (slay_increment < 200) slay_increment = 200;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_GIANT);
				}

				if (slay_increment < 170) slay_increment = 170;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DRAGON);
				}

				if (slay_increment < 150) slay_increment = 150;
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DRAGON);
				}

				if (slay_increment < 300) slay_increment = 300;
			}

			/* Execute demon */
			if ((f1 & (TR1_KILL_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DEMON);
				}

				if (slay_increment < 300) slay_increment = 300;
			}

			/* Execute undead */
			if ((f1 & (TR1_KILL_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				}

				if (slay_increment < 300) slay_increment = 300;
			}

			if ((p_ptr->slay_elements))
			{
				/*First, Mark all resists in the lore if applicable*/
				if (r_ptr->flags3 & (RF3_IM_ELEM))
				{
					if (m_ptr->ml)
					{
						u32b flags = r_ptr->flags3;

						/*Just the elemental flags*/
						flags &= RF3_IM_ELEM;

						l_ptr->r_l_flags3 |= flags;
					}
				}

				/*Now increase the damage,if they don't resist any of the elements.*/
				if ((r_ptr->flags3 & (RF3_IM_ELEM)) != (RF3_IM_ELEM))
				{
					if (slay_increment < 150) slay_increment = 150;
				}
			}

			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_l_flags3 |= (RF3_IM_ACID);
					}
				} else {

					/* Otherwise, take the damage */
					/*Water reduces acid damage*/
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (brand_increment < 100) brand_increment = 100;
					}
					else
					{
						if (brand_increment < 130) brand_increment = 130;
					}
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
						l_ptr->r_l_flags3 |= (RF3_IM_ELEC);
					}
				} else {

					/* Otherwise, take the damage */
					/* Water increases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (brand_increment < 170) brand_increment = 170;
					}
					else
					{
						if (brand_increment < 130) brand_increment = 130;
					}
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
						l_ptr->r_l_flags3 |= (RF3_IM_FIRE);
					}
				}
				/* Succeptable */
				else if (r_ptr->flags3 & (RF3_HURT_FIRE))
				{
					/* Water decreases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (brand_increment < 170) brand_increment = 170;
					}

					/*Extra Damage*/
					else if (brand_increment < 250) brand_increment = 250;

					if (m_ptr->ml)
					{
						l_ptr->r_l_flags3 |= (RF3_HURT_FIRE);
					}
				} else {

					/* Otherwise, take the damage */
					/* Water decreases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (brand_increment < 80) brand_increment = 80;
					}
					else
					{
						if (brand_increment < 130) brand_increment = 130;
					}
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
						l_ptr->r_l_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Succeptable */
				else if (r_ptr->flags3 & (RF3_HURT_COLD))
				{
					/* Water increases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (brand_increment < 300) brand_increment = 300;
					}

					/*Extra Damage*/
					else if (brand_increment < 250) brand_increment = 250;

					if (m_ptr->ml)
					{
						l_ptr->r_l_flags3 |= (RF3_HURT_COLD);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					/* Water increases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (brand_increment < 170) brand_increment = 170;
					}

					else if (brand_increment < 130) brand_increment = 130;
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
						l_ptr->r_l_flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (brand_increment < 130) brand_increment = 130;
				}
			}

			if (f3 & (TR3_VAMPIRE))
			{
				/* Notice immunity */
				if ((r_ptr->flags3 & (RF3_RES_NETHR)) || (r_ptr->flags3 & (RF3_UNDEAD)))
				{
					// nothing
				}
				else 
				{
					if (brand_increment < 50) brand_increment = 50;
				}
			}
			
			break;
		}
	}


	/* Return the P-increment */
	return brand_increment + slay_increment;
}


/*
 * Extract the "total damage" from a missile hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 */
int tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr, bool is_weapon)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3, fn;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
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
					l_ptr->r_l_flags3 |= (RF3_ANIMAL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_EVIL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DEMON);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_ORC);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_TROLL);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_GIANT);
				}

				if (mult < 2) mult = 2;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DRAGON);
				}

				if (mult < 2) mult = 2;
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DRAGON);
				}

				if (mult < 3) mult = 3;
			}

			/* Execute demon */
			if ((f1 & (TR1_KILL_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_DEMON);
				}

				if (mult < 3) mult = 3;
			}

			/* Execute undead */
			if ((f1 & (TR1_KILL_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				}

				if (mult < 3) mult = 3;
			}

			if ((p_ptr->slay_elements) && (is_weapon))
			{
				/*First, Mark all resists in the lore if applicable*/
				if (r_ptr->flags3 & (RF3_IM_ELEM))
				{
					if (m_ptr->ml)
					{
						u32b flags = r_ptr->flags3;

						/*Just the elemental flags*/
						flags &= RF3_IM_ELEM;

						l_ptr->r_l_flags3 |= flags;
					}
				}

				/*Now increase the damage, but only by two if they don't resist any of the elements.*/
				if ((r_ptr->flags3 & (RF3_IM_ELEM)) != (RF3_IM_ELEM))
				{
					if (mult < 2 ) mult = 2;
				}
			}

			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_l_flags3 |= (RF3_IM_ACID);
					}
				} else {

					/* Otherwise, take the damage */
					if (mult < 2 ) mult = 2;
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
						l_ptr->r_l_flags3 |= (RF3_IM_ELEC);
					}
				} else {

					/* Otherwise, take the damage */
					if (mult < 2 ) mult = 2;
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
						l_ptr->r_l_flags3 |= (RF3_IM_FIRE);
					}
				}
				/* Succeptable */
				else if (r_ptr->flags3 & (RF3_HURT_FIRE))
				{
					/* Water decreases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (mult < 3 ) mult = 3;
					}

					/*Extra Damage*/
					else if (mult < 4) mult = 4;

					if (m_ptr->ml)
					{
						l_ptr->r_l_flags3 |= (RF3_HURT_FIRE);
					}
				} else {

					/* Otherwise, take the damage */
					if (mult < 2 ) mult = 2;
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
						l_ptr->r_l_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Succeptable */
				else if (r_ptr->flags3 & (RF3_HURT_COLD))
				{
					/* Water increases damage */
					if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3 & (FF3_WATER))
					{
						if (mult < 5 ) mult = 5;
					}

					/*Extra Damage*/
					else if (mult < 4) mult = 4;

					if (m_ptr->ml)
					{
						l_ptr->r_l_flags3 |= (RF3_HURT_COLD);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					/* Otherwise, take the damage */
					if (mult < 2 ) mult = 2;
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
						l_ptr->r_l_flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					/* Otherwise, take the damage */
					if (mult < 2 ) mult = 2;
				}
			}

			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
}

cptr tot_dam_aux_verb(const object_type *o_ptr, const monster_type *m_ptr)
{

	u32b f1, f2, f3, fn;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	
	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Slay Animal */
			if ((f1 & (TR1_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				return "smite";
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				return "smite";
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				return "smite";
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				return "smite";
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				return "smite";
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				return "smite";
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				return "smite";
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				return "smite";
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				return "SMITE";
			}

			/* Execute demon */
			if ((f1 & (TR1_KILL_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				return "SMITE";
			}

			/* Execute undead */
			if ((f1 & (TR1_KILL_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				return "SMITE";
			}

			if ((p_ptr->slay_elements))
			{
				return "smite";
			}

			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					// nothing
				} else {
					return "corrode";
				}
			}

			/* Brand (Elec) */
			if (f1 & (TR1_BRAND_ELEC))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ELEC))
				{
					// nothing 
				} else
				{
					return "zap";
				}
			}

			/* Brand (Fire) */
			if (f1 & (TR1_BRAND_FIRE))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					// nothing
				}
				else 
				{
					return "sear";
				}
			}

			/* Brand (Cold) */
			if (f1 & (TR1_BRAND_COLD))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_COLD))
				{
					// nothing
				} 	
				else 
				{
					return "freeze";
				}
			}

			/* Brand (Poison) */
			if (f1 & (TR1_BRAND_POIS))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					// nothing
				}
				else
				{
					return "poison";
				}
			}

			/* Vampiric draining */
			if (f3 & (TR3_VAMPIRE))
			{
				/* Notice immunity */
				if ((r_ptr->flags3 & (RF3_RES_NETHR)) || (r_ptr->flags3 & (RF3_UNDEAD)))
				{
					// nothing
				}
				else 
				{
					return "drain";
				}
			}

			break;
		}
	}
	return "hit";
}

/*
 * Find a secret at the specified location and change it according to
 * the state.
 */
void find_secret(int y, int x)
{
	feature_type *f_ptr;
	feature_lore *f_l_ptr;

	cptr text;

	/* Get feature */
	f_ptr = &f_info[cave_feat[y][x]];
	f_l_ptr = &f_l_list[cave_feat[y][x]];

	if (f_l_ptr->f_l_sights < MAX_UCHAR) f_l_ptr->f_l_sights++;

	/* Get the feature description */
	text = (f_text + f_ptr->f_text);

	if (player_has_los_bold(y, x) && strlen(text))
	{
		/* You have found something */
		msg_format("%s",text);
	}

	/* Change the location */
	cave_alter_feat(y, x, FS_SECRET);


	/*p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);*/

	/* Disturb */
	disturb(0, 0);
}


/*
 * Search for hidden things
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;

	object_type *o_ptr;


	/* Start with base search ability */
	chance = p_ptr->skill_srh;

	/* Penalize various conditions */
	if (p_ptr->blind || no_lite()) chance = chance / 10;
	if (p_ptr->confused || p_ptr->image) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Get the feature */
				int feat = cave_feat[y][x];

				/* Reveal interesting secret features */
				if (feat_ff1_match(feat, FF1_SECRET) &&
					(feat_ff3_match(feat, FF3_PICK_TRAP |
						FF3_PICK_DOOR) ||
					 !feat_ff1_match(feat, FF1_MOVE)))
				{
					find_secret(y, x);
		   		}

				/* Find hidden player traps */
				if (cave_player_trap_bold(y, x))
				{
					/* Get the trap */
					effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

					/* Ignore known traps */
					if (x_ptr->x_flags & (EF1_HIDDEN))
					{
						/* Reveal the trap */
						x_ptr->x_flags &= ~(EF1_HIDDEN);

						/* Show the trap */
						note_spot(y, x);

						lite_spot(y, x);

						/* Message */
						msg_print("You have found a trap!");

						/* Disturb the player */
						disturb(0, 0);
					}
				}

				/* Scan all objects in the grid */
				for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
				{
					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip disarmed chests */
					if (o_ptr->pval <= 0) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
}


/*
 * Objects that combine with items already in the quiver get picked
 * up, placed in the quiver, and combined automatically.
 */
static bool quiver_carry(object_type *o_ptr, int o_idx)
{
	int i;

	object_type *i_ptr, *j_ptr = NULL;

	char name[80];

	/* Must be ammo. */
	if (!ammo_p(o_ptr) && !is_throwing_weapon(o_ptr)) return (FALSE);

	/* Known or sensed cursed ammo is avoided */
	if (cursed_p(o_ptr) && ((o_ptr->ident & (IDENT_SENSE)) || object_known_p(o_ptr))) return (FALSE);

	/* Check quiver space */
	if (!quiver_carry_okay(o_ptr, o_ptr->number, -1)) return (FALSE);

	/* Check quiver for similar objects. */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get object in that slot. */
		i_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!i_ptr->k_idx)
		{
			/* But save first empty slot, see later */
			if (!j_ptr) j_ptr = i_ptr;

			continue;
		}

		/* Look for similar. */
		if (object_similar(i_ptr, o_ptr))
		{

			/* Absorb floor object. */
			object_absorb(i_ptr, o_ptr);

			/* Remember this slot */
			j_ptr = i_ptr;

			/* Done */
			break;
		}
	}

	/* Can't combine the ammo. Search for the "=g" inscription */
	if (i >= END_QUIVER)
	{
		char *s;

		/* Full quiver or no inscription at all */
		if (!j_ptr || !(o_ptr->obj_note)) return (FALSE);

		/* Search the '=' character in the inscription */
		s = strchr(quark_str(o_ptr->obj_note), '=');

		while (TRUE)
		{
			/* We reached the end of the inscription */
			if (!s) return (FALSE);

			/* We found the "=g" inscription */
			if (s[1] == 'g')
			{
				/* Put the ammo in the empty slot */
				object_copy(j_ptr, o_ptr);

				/* Done */
				break;
			}

			/* Keep looking */
			s = strchr(s + 1, '=');
		}
	}

	/*
	 * Increase carried weight.
     * Note that o_ptr has the right number of missiles to add.
	 */
	p_ptr->total_weight += o_ptr->weight * o_ptr->number;

	/* Reorder the quiver, track the index */
	i = reorder_quiver(j_ptr - inventory);

	/* Get the final slot */
	j_ptr = &inventory[i];

	/* Cursed! */
	if (cursed_p(j_ptr))
	{
		/* Warn the player */
		sound(MSG_CURSED);
		msg_print("Oops! It feels untrustworthy...");

		/* Remove special inscription, if any */
		if (j_ptr->discount >= INSCRIP_NULL) j_ptr->discount = 0;

		/* Sense the object if allowed */
		if (j_ptr->discount == 0) j_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		j_ptr->ident |= (IDENT_SENSE);
	}

	/* Describe the object */
	object_desc(name, sizeof(name), j_ptr, TRUE, 3);

	/* Message */
	msg_format("You have %s (%c).", name, index_to_label(i));

	/* Delete the object */
	delete_object_idx(o_idx);

	/* Update "p_ptr->pack_size_reduce" */
	find_quiver_size();

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_NATIVE);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_INVEN);

	return (TRUE);
}


/*
 * Determine if the object should be picked up -- "=g" or autopickup is set
 */
static bool auto_pickup_okay(const object_type *o_ptr)
{
	cptr s;

	/* It can't be carried -- verify elsewhere for warnings. */
	/* if (!inven_carry_okay(o_ptr)) return (FALSE); */

	/*object is marked to not pickup*/
	if ((k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP) &&
	    object_aware_p(o_ptr)) return (FALSE);

	/*object is marked for pickup*/
	if ((k_info[o_ptr->k_idx].squelch == NO_SQUELCH_ALWAYS_PICKUP) &&
	    object_aware_p(o_ptr)) return (TRUE);

	/* No inscription */
	if (!o_ptr->obj_note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->obj_note), '=');

	/* Process inscription */
	while (s)
	{
		/* Auto-pickup on "=g" */
		if (s[1] == 'g') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

	/* Don't auto pickup */
	return (FALSE);
}


/*
 * Helper routine for py_pickup() and py_pickup_floor().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
static void py_pickup_aux(int o_idx)
{
	int slot;

	char o_name[80];
	object_type *o_ptr;

	u16b msgt = MSG_GENERIC;

	o_ptr = &o_list[o_idx];

	/*hack - don't pickup &nothings*/
	if (o_ptr->k_idx)
	{

		/* Carry the object */
		slot = inven_carry(o_ptr);

		/* Get the object again */
		o_ptr = &inventory[slot];

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		if (artifact_known(o_ptr))
		{
			msgt = MSG_NOTICE;
		}

		/* Message */
		msg_c_format(msgt, "You have %s (%c).", o_name, index_to_label(slot));

	}

	/* Delete the object */
	delete_object_idx(o_idx);
}

/*
 * Allow the player to sort through items in a pile and
 * pickup what they want.  This command does not use
 * any energy because it costs a player no extra energy
 * to walk into a grid and automatically pick up items
 */
void do_cmd_pickup_from_pile(void)
{
	bool picked_up_item  = FALSE;

	/*
	 * Loop through and pick up objects until escape is hit or the backpack
	 * can't hold anything else.
	 */
	while (TRUE)
	{
		int item;

		char prompt[80];

		int floor_list[MAX_FLOOR_STACK];

		int floor_num;

		/*start with everything updated*/
		handle_stuff();

		/* Scan for floor objects */
		floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, p_ptr->py, p_ptr->px, 0x01);

		/* No pile */
		if (floor_num < 1)
		{
			cptr player_status = "standing";

			if (p_ptr->flying) player_status = "hovering";

			if (picked_up_item) msg_format("There are no more objects where you are %s.", player_status);
			else msg_format("There are no objects where you are standing %s.", player_status);
			break;
		}

		/* Restrict the choices */
		item_tester_hook = inven_carry_okay;

		/* re-test to see if we can pick any of them up */
		floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, p_ptr->py, p_ptr->px, 0x01);

		/* Nothing can be picked up */
		if (floor_num < 1)
		{
			msg_format("Your backpack is full.");
			break;
		}

		/* Save screen */
		screen_save();

		/* Display */
		show_floor(floor_list, floor_num);

		my_strcpy(prompt, "Pick up which object? (ESC to cancel):", sizeof(prompt));

		/*clear the restriction*/
		item_tester_hook = NULL;

		/* Get the object number to be bought */
		item = get_menu_choice(floor_num, prompt);

		/*player chose escape*/
		if (item == -1)
		{
			screen_load();
			break;
		}

		/* Pick up the object */
		py_pickup_aux(floor_list[item]);

		/*Mark that we picked something up*/
		picked_up_item = TRUE;

		/* Load screen */
		screen_load();
	}

	/*clear the restriction*/
	item_tester_hook = NULL;

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Just be sure all inventory management is done. */
	notice_stuff();

}

/*
 * Make the player carry everything in a grid.
 *
 * If "pickup" is FALSE then only gold will be picked up.
 */
void py_pickup(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;


	int max_lev_for_xp;
			
	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	char o_name[80];
	u16b msgt = MSG_GENERIC;

	int sound_msg;

	int can_pickup = 0;
	int not_pickup = 0;

	int can_pickup_last_o_idx = 0;
	int not_pickup_last_o_idx = 0;

	/* Are we allowed to pick up anything here? */
	if (!(f_info[cave_feat[py][px]].f_flags1 & (FF1_DROP))) return;

 	/* Automatically destroy squelched items in pile if necessary */
	do_squelch_pile(py, px);

	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		bool do_not_pickup = FALSE;

		msgt = MSG_GENERIC;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Prepare a colorful message */
		if (artifact_known(o_ptr))
		{
			msgt = MSG_NOTICE;
		}

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			long gold = (long)o_ptr->pval * o_ptr->number;

			/* Determine which sound to play */
			if (gold < 200) sound_msg = MSG_MONEY1;
			else if (gold < 600) sound_msg = MSG_MONEY2;
			else sound_msg = MSG_MONEY3;

			/* Message */
			message_format(sound_msg, 0, "You have found %ld gold pieces worth of %s.",
			               gold, o_name);

			/* Collect the gold */
			p_ptr->au += gold;

			max_lev_for_xp = 50;

			if (o_ptr->sval <= 3) max_lev_for_xp = 6;
			else if (o_ptr->sval <= 6) max_lev_for_xp = 10;
			else if (o_ptr->sval <= 8) max_lev_for_xp = 14;
			else if (o_ptr->sval <= 11) max_lev_for_xp = 18;
			else if (o_ptr->sval == 12) max_lev_for_xp = 22;
			else if (o_ptr->sval == 13) max_lev_for_xp = 26;
			else if (o_ptr->sval == 14) max_lev_for_xp = 30;
			else if (o_ptr->sval == 15) max_lev_for_xp = 31;
			else if (o_ptr->sval == 16) max_lev_for_xp = 32;
			else if (o_ptr->sval == 17) max_lev_for_xp = 33;
			else if (o_ptr->sval == 18) max_lev_for_xp = 34;

			if (p_ptr->lev < max_lev_for_xp){
			  gain_exp(gold / 10);
			}

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

			/* Delete the gold */
			delete_object_idx(this_o_idx);

			/* Check the next object */
			continue;
		}

		/*some items are marked to never pickup*/
		if ((k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP) && object_aware_p(o_ptr))
		{
			do_not_pickup = TRUE;
		}

		/* Test for quiver auto-pickup */
		if (quiver_carry(o_ptr, this_o_idx)) continue;

		/* Test for auto-pickup */
		if (auto_pickup_okay(o_ptr) && inven_carry_okay(o_ptr))
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* Easy Floor */
		if ((easy_floor) && (!do_not_pickup))
		{
			/* Pickup if possible */
			if (pickup && inven_carry_okay(o_ptr))
			{
				/* Pick up if allowed */
				if (!carry_query_flag)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}

				/* Else count */
				else
				{
					/* Remember */
					can_pickup_last_o_idx = this_o_idx;

					/* Count */
					++can_pickup;
				}
			}

			/* Else count */
			else
			{
				/* Remember */
				not_pickup_last_o_idx = this_o_idx;

				/* Count */
				++not_pickup;
			}

			/* Check the next object */
			continue;
		}

		/* Describe the object */
		if ((!pickup) || (do_not_pickup))
		{
			msg_c_format(msgt, "You see %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Note that the pack is too full */
		if (!inven_carry_okay(o_ptr))
		{
			if (o_ptr->k_idx)
			{
				if(auto_pickup_okay(o_ptr)) msgt = MSG_BELL;
				msg_c_format(msgt, "You have no room for %s.", o_name);
			}

			continue;
		}

		/* Query before picking up */
		if (carry_query_flag)
		{
			char out_val[160];
			int result;

			strnfmt(out_val, sizeof(out_val), "Pick up %s? ", o_name);

			result = get_check_other(out_val, 'k');
			if (!result) continue;
			else if (result == 2)
			{
				bool saved_verify;

				saved_verify = verify_destroy;
				verify_destroy = 0;

				destroy_item(-this_o_idx);

				verify_destroy = saved_verify;
				continue;
			}

		}

		/* Pick up the object */
		py_pickup_aux(this_o_idx);
	}

	/* Easy floor, objects left */
	if (easy_floor && (can_pickup + not_pickup > 0))
	{
		/* Not picking up */
		if (!pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				 /* Get the object */
				o_ptr = &o_list[not_pickup_last_o_idx];

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				msg_c_format(msgt, "You see %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_format("You see a pile of %d objects.", not_pickup);
			}

			/* Done */
			return;
		}

		/* No room */
		if (!can_pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Get the object */
				o_ptr = &o_list[not_pickup_last_o_idx];

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				/* Message */
				if (o_ptr->k_idx) msg_c_format(msgt, "You have no room for %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_print("You have no room for any of the objects on the floor.");
			}

			/* Done */
			return;
		}

		/* Hack - do not make a list for only one object */
		if ((can_pickup == 1) && !always_pickup)
		{

			py_pickup_aux(can_pickup_last_o_idx);

			return;
		}

		/* Pick up objects */
		while (1)
		{
			cptr q, s;

			int item;

			/* Restrict the choices */
			item_tester_hook = inven_carry_okay;

			/* Get an object*/
			q = "Get which item? ";
			s = NULL;
			if (!get_item(&item, q, s, (USE_FLOOR))) break;

			/* Pick up the object */
			py_pickup_aux(0 - item);
		}
	}
}

int to_hit_weight_penalty(const object_type *o_ptr){
	return 2*MAX(0,o_ptr->weight - adj_str_hold[p_ptr->stat_ind[A_STR]])/10;
}

/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
int check_hit(int power)
{
	return test_hit(power, p_ptr->ac + p_ptr->to_a, TRUE)>0;
}

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */

int dam_plus_to_percent_added(int to_d){
  int p = 0;
  if (to_d < 0){
    p = 20 * to_d;
	if (p < -50){
		p = -50;
	}
  } else if (to_d <= 10){
    p = 20 * to_d;
  } else if (to_d <= 20){
    p = 200 + 15 * (to_d - 10);
  } else if (to_d <= 30){
    p = 200 + 150 + 10 * (to_d - 20);
  } else {
    p = 200 + 150 + 100 + 5 * (to_d - 30);
  }
  return p;
}

void py_attack(int y, int x)
{
	int num = 0, k, p, bonus, chance;
	int hits = 0;

	int sleeping_bonus = 0;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	object_type *o_ptr;

	char m_name[80];
	char verb[20];

	bool fear = FALSE;

	bool do_quake = FALSE;

	bool was_asleep = FALSE;

	int blows_this_time, remainder, rand;

	/* Get the monster */
	m_ptr = &mon_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Reveal minics and hidden creatures (note: mimics cannot be sneak-attacked) */
	if (((m_ptr->mimic_k_idx) && (m_ptr->ml)) || (m_ptr->mflag & (MFLAG_HIDE)))
	{
		/* Mimic no longer acts as a detected object */
		m_ptr->mflag &= ~(MFLAG_MIMIC | MFLAG_HIDE);

		/*no longer a mimic*/
		m_ptr->mimic_k_idx = 0;

		/* Get monster name ("a kobold") */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0x88);

		/* And reveal */
		update_mon(cave_m_idx[m_ptr->fy][m_ptr->fx], FALSE);

		/* Redraw */
		lite_spot(m_ptr->fy, m_ptr->fx);

		/* Message */
		if (!p_ptr->afraid)
		{
			msg_format("You find yourself fighting %s!", m_name);

		}
		else
		{

			msg_format("%^s appears, but you are too frightened to fight it!",
				m_name);
			return;
		}

	}

	/*record if monster was sleeping before waking*/
	if (m_ptr->csleep) was_asleep = TRUE;

	/* Disturb the monster */
	m_ptr->csleep = 0;

	/*possibly update the monster health bar*/
	if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

	/* Disturb the player */
	disturb(0, 0);

	/* Extract monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

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

	if (o_ptr){
		/* Calculate the "attack quality" */
		bonus = p_ptr->to_h_melee + o_ptr->to_h;

		bonus = bonus - to_hit_weight_penalty(o_ptr);
	}


	/*
	 * If the monster is sleeping and visible, it can be hit more easily.
	 * Especially by Rogues
	 */

	if ((was_asleep) && (m_ptr->ml))
	{
		sleeping_bonus =  5 + p_ptr->lev / 5;

		if (p_ptr->stat_use[A_STE] >= 18+100)
		{
			/*100 % increase*/
			sleeping_bonus *= 2;
		} else if (p_ptr->stat_use[A_STE] >= 18+50)
		{
			/*75 % increase*/
			sleeping_bonus *= 7;
			sleeping_bonus /= 4;
		} else if (p_ptr->stat_use[A_STE] >= 18)
		{
			/*50 % increase*/
			sleeping_bonus *= 3;
			sleeping_bonus /= 2;
		} else if (p_ptr->stat_use[A_STE] >= 15)
		{
			/*25 % increase*/
			sleeping_bonus *= 5;
			sleeping_bonus /= 4;
		}
	}
	chance = (p_ptr->skill_thn + ((bonus + sleeping_bonus) * BTH_PLUS_ADJ));

	/*Mark the monster as attacked*/
	m_ptr->mflag |= (MFLAG_HIT_BY_MELEE);

	blows_this_time = p_ptr->num_blows_times_ten / 10;
	remainder = p_ptr->num_blows_times_ten - 10*blows_this_time;
	rand = randint(10);
	if (rand<=remainder){
		blows_this_time = blows_this_time + 1;
	}

	/* Attack once for each legal blow */
	while (num++ < blows_this_time)
	{
		/*Adjust for player terrain*/
		chance = feat_adjust_combat_for_player(chance, FALSE);

		/*Adjust for monster terrain*/
		chance = feat_adjust_combat_for_monster(m_ptr, chance, TRUE);

		/* Some monsters are great at dodging  -EZ- */
		if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!was_asleep) &&
			(!m_ptr->stunned) && (!m_ptr->confused) && (!m_ptr->monfear)
			&& (one_in_(2)))
		{
			message_format(MSG_MISS, 0, "%^s evades your blow!",
				m_name);

			/* Learn that monster can dodge */
			l_ptr->r_l_flags2 |= (RF2_EVASIVE);

			continue;
		}

		/* Test for hit */
		else if (test_hit(chance, r_ptr->ac, m_ptr->ml)>0)
		{

			/* If this was the first hit, make some noise */
			hits++;
			if (hits == 1) add_wakeup_chance += p_ptr->base_wakeup_chance;

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				k = damroll(o_ptr->dd, o_ptr->ds);
				p = dam_plus_to_percent_added(p_ptr->to_d_melee + o_ptr->to_d);
				p += melee_p_increment(o_ptr, m_ptr);
				if (p_ptr->impact && (k > 50)) do_quake = TRUE;
				k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
				my_strcpy(verb,tot_dam_aux_verb(o_ptr,m_ptr),20);
			} else {
				/* bare hands */
				k = 1;
				p = dam_plus_to_percent_added(p_ptr->to_d_melee);
				my_strcpy(verb,"hit",20);
			}

			p = p + sleeping_bonus*7;

			k = (k * (p + 100)) / 100;

			/* No negative damage */
			if (k < 0) k = 0;

			if (was_asleep)
			{
				if (cp_ptr->flags & CF_ROGUE_COMBAT)
				{
					message_format(MSG_GENERIC, m_ptr->r_idx,
						"You ruthlessly sneak attack %s!", m_name);
				}
				else
				{
					message_format(MSG_GENERIC, m_ptr->r_idx,
						"You sneak attack %s!", m_name);
				}
			}

			else
			{
				/* Message */
				message_format(MSG_GENERIC, m_ptr->r_idx, "You %s %s.", verb, m_name);
			}

			if (p_ptr->vampire)
			{
				if ((r_ptr->flags3 & (RF3_RES_NETHR)) || (r_ptr->flags3 & (RF3_UNDEAD)))
				{
					// nothing
				}
				else 
				{
					/* gain some HP */
					hp_player(MIN(m_ptr->hp,MAX(1,k/5)));
				}
			}

			/* Complex message */
			if (p_ptr->wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL, SOURCE_PLAYER))
			{
				/*return energy from unused attacks*/
				if (num < blows_this_time)
				{
					p_ptr->p_energy_use -= (((blows_this_time - (num)) * BASE_ENERGY_MOVE ) /
		  									blows_this_time);
				}
				break;
			}

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
						l_ptr->r_l_flags3 |= (RF3_NO_CONF);
					}
							msg_format("%^s is unaffected!", m_name);
				}
				else if (rand_int(100) < r_ptr->level)
				{
					msg_format("%^s is unaffected!", m_name);
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
	}

	/* Mega-Hack -- apply earthquake brand */
	if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10);
}





/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
s16b move_player(int dir, int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b used_energy = BASE_ENERGY_MOVE;

	int y, x;

	char name[80];

	feature_type *f_ptr;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Get the feature */
	f_ptr = &f_info[cave_feat[y][x]];

	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if ((easy_alter) && (!jumping) && (cave_info[y][x] & (CAVE_MARK)) &&
		 	 (_feat_ff1_match(f_ptr, FF1_CAN_OPEN | FF1_CAN_DISARM) ||
			 (cave_player_trap_bold(y, x) &&
			!(x_list[cave_x_idx[y][x]].x_flags & (EF1_HIDDEN)))))

	{
		/* Not already repeating */
		if (!p_ptr->command_rep)
		{
			/* Hack -- Optional auto-repeat */
			if (always_repeat && (p_ptr->command_arg <= 0))
			{
				/* Repeat 99 times */
				p_ptr->command_rep = 99;

				/* Reset the command count */
				p_ptr->command_arg = 0;
			}
		}

		/* Alter */
		do_cmd_alter();

	}

	/* Player can not walk through certain terrain */
	else if (!cave_ff1_match(y, x, FF1_MOVE))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Get hit by traps */
			if (cave_passive_trap_bold(y, x))
			{
				/* Hit the trap */
				hit_trap(x_list[cave_x_idx[y][x]].x_f_idx, y, x, MODE_ACTION, 1);
			}

			/* Get the feature name */
			feature_desc(name, sizeof(name), f_ptr - f_info, TRUE, TRUE);

			/* Tell the player */
			msg_format("You feel %s blocking your way.", name);

			cave_info[y][x] |= (CAVE_MARK);

			lite_spot(y, x);

		}

		/* Mention known obstacles */
		else if (!hit_wall(y, x, TRUE))
		{
			/* Get the feature name */
			feature_desc(name, sizeof(name), f_ptr - f_info,
				TRUE, TRUE);

			/* Tell the player */
			msg_format("There is %s blocking your way.", name);
		}
	}


	/* There is some effect blocking movement */
	else if (!cave_passable_bold(y, x))
	{
		int x_idx;

		/* Discover unknown terrain */
		if (!cave_flag_bold(y, x, CAVE_MARK))
		{
			/* Remember */
			cave_info[y][x] |= (CAVE_MARK);

			/* Redraw */
			lite_spot(y, x);
		}

		/* Get the first effect */
		x_idx = cave_x_idx[y][x];

		/* Find the effect that disables movement */
		while (x_idx)
		{
			u16b feat;

			/* Get the effect */
			effect_type *x_ptr = &x_list[x_idx];

			/* Get the associated feature */
			feat = x_ptr->x_f_idx;

			/* Check the FF1_MOVE flag */
			if (feat && !feat_ff1_match(feat, FF1_MOVE))
			{
				/* Get the feature name */
				feature_desc(name, sizeof(name), feat, TRUE, TRUE);

				/* Tell the player */
				msg_format("There is %s blocking your way.", name);

				/* Done */
				break;
			}

			/* Point to the next effect */
			x_idx = x_ptr->next_x_idx;
		}

		/* Paranoia */
		if (!x_idx) msg_print("You cannot move into this grid.");
	}

	/* Some terrain prevents flying. */
	else if (!(feat_ff2_match(cave_feat[y][x], FF2_CAN_FLY)) && (p_ptr->flying))
	{
		char out_val[80];
		int feat = cave_feat[y][x];

		/* Discover unknown terrain */
		if (!cave_flag_bold(y, x, CAVE_MARK))
		{
			/* Remember */
			cave_info[y][x] |= (CAVE_MARK);

			/* Redraw */
			lite_spot(y, x);
		}


		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		/* Tell the player */
		msg_format("There is %s blocking your way.", name);

		/* Give the player the option to stop flying. */
		sprintf(out_val, "So you want to stop flying? ");
		if (get_check(out_val))
		{
			set_flying(0, FALSE);

			msg_print("You land.");

			/*Little energy used*/
			used_energy = BASE_ENERGY_MOVE / 10;
		}

	}

	/* Normal movement */
	else
	{
		feature_lore *f_l_ptr;

		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;

		/*Get ready to mark lore*/
		f_l_ptr = &f_l_list[cave_feat[p_ptr->py][p_ptr->px]];

		/*Check if the player is native*/
		if (is_player_native(y, x))
		{
			if (!p_ptr->flying)
			{
				/*Mark the lore*/
				if (f_l_ptr->f_l_native_moves < MAX_UCHAR) f_l_ptr->f_l_native_moves ++;

				/*record the energy*/
				used_energy = f_info[cave_feat[p_ptr->py][p_ptr->px]].native_energy_move;
			}
		}
		else
		{
			if (!p_ptr->flying)
			{
				/*Mark the lore*/
				if (f_l_ptr->f_l_non_native_moves < MAX_UCHAR) f_l_ptr->f_l_non_native_moves ++;

				/*record the energy*/
				used_energy = f_info[cave_feat[p_ptr->py][p_ptr->px]].non_native_energy_move;
			}
		}

		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
		    (0 == rand_int(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		py_pickup(jumping != always_pickup);

		/* Handle "store doors" */
		if (cave_shop_bold(y, x))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Free turn XXX XXX XXX */
			p_ptr->p_energy_use = 0;
		}

		/* Hit a trap */
		else if (cave_passive_trap_bold(y, x))
		{
			/* Hit the trap */
 			hit_trap(x_list[cave_x_idx[p_ptr->py][p_ptr->px]].x_f_idx, y, x, MODE_ACTION, 1);

		}

		/* Discover secrets */
		else if (_feat_ff1_match(f_ptr, FF1_SECRET))
		{
			/* Find the secret */
			find_secret(y, x);

			/* Get the feature again */
			f_ptr = &f_info[cave_feat[y][x]];
		}

		/* Record the energy for flying creatures.*/
		if (p_ptr->flying)	used_energy = BASE_ENERGY_MOVE;

		/* Reveal when you are on shallow or deep  terrain */
		else if (!(cave_info[y][x] & (CAVE_MARK)) &&
		     _feat_ff3_match(f_ptr, FF2_SHALLOW | FF2_DEEP))
		{
			/* Get the name */
			feature_desc(name, sizeof(name), f_ptr - f_info,
				FALSE, TRUE);

			/* Tell the player */
			msg_format("You feel you are in %s.", name);

			cave_info[y][x] |= (CAVE_MARK);

			lite_spot(y, x);
		}

		/* Walk on a monster trap */
		else if (cave_monster_trap_bold(y,x))
		{
			msg_print("You inspect your cunning trap.");
		}

	}

	return (used_energy);
}

/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	if (!in_bounds(y, x)) return (FALSE);

	/* Unknown walls are not known walls */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Non-wall grids are not known walls */
	/* Was !cave_ff1_match(y, x, FF1_WALL) -DG- */
	if (cave_ff1_match(y, x, FF1_MOVE)) return (FALSE);

	/* Default */
	return (TRUE);
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown XXX XXX XXX */
	if (!in_bounds(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave_info[y][x] & (CAVE_MARK)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm  -CJS-
 *
 * Basically, once you start running, you keep moving until something
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in any direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###             o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static const byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static const byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. (?)
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;

	/* Save the direction */
	p_ptr->run_cur_dir = dir;

	/* Assume running straight */
	p_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	p_ptr->run_open_area = TRUE;

	/* Assume not looking for breaks */
	p_ptr->run_break_right = FALSE;
	p_ptr->run_break_left = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], py, px))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], py, px))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		p_ptr->run_break_right = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (p_ptr->run_break_left && p_ptr->run_break_right)
	{
		/* Not looking for open area */
		p_ptr->run_open_area = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				p_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				p_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				p_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				p_ptr->run_old_dir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max, inv;
	int option, option2;

	u16b feat;

	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = p_ptr->run_old_dir;

	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;

	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		object_type *o_ptr;

		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];

		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[row][col]];

			/* Visible monster, but not mimics, disturb run */
			if ((m_ptr->ml) && (m_ptr->mimic_k_idx == 0)) return (TRUE);

		}

		/* Visible objects abort running */
		for (o_ptr = get_first_object(row, col); o_ptr; o_ptr = get_next_object(o_ptr))
		{
			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}

		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (cave_info[row][col] & (CAVE_MARK))
		{
			feature_type *f_ptr;

			s16b x_idx;

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].f_mimic;

			/* Get fast pointer */
			f_ptr = &f_info[feat];

			/* Feature doesn't support running. Stop */
			if (!_feat_ff1_match(f_ptr, FF1_RUN)) return (TRUE);

			/* Feature is a door */
			if (_feat_ff1_match(f_ptr, FF1_DOOR))
			{
				/* Doors must not be ignored. Stop */
				if (!(run_ignore_doors)) return (TRUE);

				/* Ignore some doors. Closed doors are always noticeable */
				if (!_feat_ff3_match(f_ptr, FF3_DOOR_OPEN | FF3_DOOR_BROKEN)) return (TRUE);
			}

			/* Check if running should be stopped when reaching stairs */
			if (!(run_ignore_stairs) && _feat_ff1_match(f_ptr, FF1_STAIRS))
			{
				return (TRUE);
			}

			/* Check visible effects */
			x_idx = cave_x_idx[row][col];

			/* Scan effects on that grid */
			while (x_idx)
			{
				/* Get the effect */
				effect_type *x_ptr = &x_list[x_idx];

				/* Found a visible effect, stop */
				if (!(x_ptr->x_flags & (EF1_HIDDEN))) return (TRUE);

				/* Get the index of the next effect */
				x_idx = x_ptr->next_x_idx;
			}

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_los_bold(row, col))
		{
			/* Looking for open area */
			if (p_ptr->run_open_area)
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (p_ptr->run_open_area)
			{
				if (i < 0)
				{
					/* Break to the right */
					p_ptr->run_break_right = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					p_ptr->run_break_left = TRUE;
				}
			}
		}
	}

	/* Looking for open area */
	if (p_ptr->run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].f_mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) || feat_ff1_match(feat, FF1_MOVE))
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].f_mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) || feat_ff1_match(feat, FF1_MOVE))
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* No other options */
			p_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (run_use_corners && !run_cut_corners)
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			p_ptr->run_old_dir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = py + ddy[option];
			col = px + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (run_use_corners &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					p_ptr->run_cur_dir = option;
					p_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (run_cut_corners)
			{
				p_ptr->run_cur_dir = option2;
				p_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				p_ptr->run_cur_dir = option;
				p_ptr->run_old_dir = option2;
			}
		}
	}

	/* About to hit a known wall, stop */
	if (see_wall(p_ptr->run_cur_dir, py, px))
	{
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}

/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);

		/* Hack -- Set the run counter */
		p_ptr->running = (p_ptr->command_arg ? p_ptr->command_arg : 1000);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Continue run */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}
	}

	/* Decrease counter */
	p_ptr->running--;

	/* Move the player */
	p_ptr->p_energy_use = move_player(p_ptr->run_cur_dir, FALSE);

}


