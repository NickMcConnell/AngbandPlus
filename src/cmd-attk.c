/* File: cmd-attack.c */
/* Purpose: Player attack code */
/*
 * cmd-attk.c is a fantastical journey down the players ability to cause 
 * harm and misfortune to his many opponents in three main ways, through
 * striking them in melee, firing firearms, and tossing whatever sharps
 * happen to be handy. Within contains functions to test for a hit from 
 * projectiles and sister function to test for strikes for things held
 * in the characters hand, or perhaps even the hand itself. Then a 
 * trifecta of critical damage functions preceed a function that 
 * modifies damage based on item and monster flags, which itself preceeds
 * a test to see if a trap hits a player. Then for those monsters with 
 * built in defensive mechanisms, a function that hurts the player when 
 * they strike it, followed by a condensed message function for player
 * attacks. Finally player melee attacks are dealt with; a short function 
 * that outlines breakage chances preceeds the final two functions, player
 * ranged attacks. The first one dealing with firearms, and the second
 * dealing with thrown objects.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* Order these is in is important, see brands in to_dam_aux */
#define MODE_MELEE 			 1
#define MODE_MARTIAL_ARTS 	 2
#define MODE_SHOOTING 		 3
#define MODE_THROWING 		 4

/* Three different attack types */
#define TYPE_EDGED		0
#define TYPE_BLUNT		1
#define TYPE_PIERCE		2
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
	int i, k, punch, critplus, chance;
	punch = 1;
	critplus = 1;
	chance = 5000;

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
	if (p_ptr->skills[SK_CRIT_SHOT].skill_max > 1)
	{
		punch *= p_ptr->skills[SK_CRIT_SHOT].skill_rank / 2;
	}
	if (p_ptr->skills[SK_CRIT_SHOT].skill_max > 0)
	{
		critplus = p_ptr->skills[SK_CRIT_SHOT].skill_rank;
	}

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (punch));

	/* Reduce chance for spot weakness */
	if (p_ptr->skills[SK_SPOT_WEAKNESS].skill_max > 0)
	{
		chance -= p_ptr->skills[SK_SPOT_WEAKNESS].skill_rank * 50;
	}

	/* Critical hit */
	if (randint(chance) <= i)
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
	int i, k, punch, critplus, chance;
	punch = 1;
	critplus = 1;
	chance = 3000;

	if (p_ptr->skills[SK_ADV_THROWING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_ADV_THROWING].skill_rank;
	}
	if (p_ptr->skills[SK_MASTER_THROWING].skill_max > 0)
	{
		punch += p_ptr->skills[SK_MASTER_THROWING].skill_rank * 2;
	}
	if (p_ptr->skills[SK_CRIT_THROW].skill_max > 1)
	{
		punch *= p_ptr->skills[SK_CRIT_THROW].skill_rank / 2;
	}
	if (p_ptr->skills[SK_CRIT_THROW].skill_max > 0)
	{
		critplus = p_ptr->skills[SK_CRIT_THROW].skill_rank;
	}

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (punch));
	
	/* Reduce chance for spot weakness */
	if (p_ptr->skills[SK_SPOT_WEAKNESS].skill_max > 0)
	{
		chance -= p_ptr->skills[SK_SPOT_WEAKNESS].skill_rank * 50;
	}

	/* Critical hit */
	if (randint(chance) <= i)
	{
		k = weight + randint(500) + randint((20 * critplus));

		if (k < 500)
		{
			msg_print("It was a good throw!");
			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			msg_print("It was a great throw!");
			dam = 2 * dam + 10;
		}
		else
		{
			msg_print("It was a superb throw!");
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
sint critical_norm(int weight, int plus, int dam, bool martial)
{
	int i, k, punch, critplus;
	int chance;
	punch = 1;
	critplus = 1;
	chance = 5000;
	
	if (martial)
	{
		if (p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 0)
		{
			punch += p_ptr->skills[SK_TOHIT_MARTIAL].skill_rank;
		}
		if (p_ptr->skills[SK_INTER_MARTIAL].skill_max > 0)
		{
			punch += p_ptr->skills[SK_INTER_MARTIAL].skill_rank;
		}
		if (p_ptr->skills[SK_ADV_MARTIAL].skill_max > 0)
		{
			punch += p_ptr->skills[SK_ADV_MARTIAL].skill_rank;
		}
	}
	else
	{
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
			punch *= (p_ptr->skills[SK_CRIT_STRIKE].skill_rank / 2);
		}
		if (p_ptr->skills[SK_CRIT_STRIKE].skill_max > 0)
		{
			critplus = p_ptr->skills[SK_CRIT_STRIKE].skill_rank;
		}
	}
	if (p_ptr->skills[SK_SPOT_WEAKNESS].skill_max > 0)
	{
		chance -= p_ptr->skills[SK_SPOT_WEAKNESS].skill_rank * 50;
	}

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (punch));

	/* Chance */
	if (randint(chance) <= i)
	{
		k = weight + randint(650) + randint((20 * critplus));

		if (k < 400)
		{
		/*	msg_print("It was a good hit!"); */
			dam = (2 * dam) + 15;
		}
		else if (k < 800)
		{
		/*	msg_print("It was a great hit!"); */
			dam = (3 * dam) + 20;
		}
		else if (k < 1000)
		{
		/*	msg_print("It was a superb hit!"); */
			dam = (4 * dam) + 30;
		}
		else if (k < 1400)
		{
		/*	msg_print("It was a *GREAT* hit!"); */
			dam = (5 * dam) + 40;
		}
		else
		{
		/*	msg_print("It was a *SUPERB* hit!"); */
			dam = (6 * dam) + 50;
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
 * Note that most brands are x3, and slays are x5 except
 * Slay Evil (x3).
 *
 * msg_mode being set to 1 is a nifty little hack that only prints 
 * the message if the blow is the first blow, controlling message bloat.
 *
 * I need to correct this function so that slays do less of a *mutipler* of 
 * damage, and resistances do less drastic a divisor, and they end up doing
 * sort of a staged step system.
 *
 */
static sint totplus_dam_aux(const object_type *o_ptr, int tdam, monster_type *m_ptr, int mode)
{
	int mult = 1;
	int div = 1;
	int add = 0;
	int total;
	char m_name[80];
	int pyrokinetics, firelore, firemastery;
	int firebonus = 0;
	
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	if (p_ptr->skills[SK_PYROKINETICS].skill_max)
		pyrokinetics = p_ptr->skills[SK_PYROKINETICS].skill_rank;
	if (p_ptr->skills[SK_FIRE_LORE].skill_max)
		firelore = p_ptr->skills[SK_FIRE_LORE].skill_rank;
	if (p_ptr->skills[SK_FIRE_MASTERY].skill_max)
		firemastery = p_ptr->skills[SK_FIRE_MASTERY].skill_rank;

	if (mode == MODE_MELEE)
	{
		if (firelore > 0) firebonus = firelore * 3;
		if (firemastery > 0) firebonus += firemastery * 2;
	}
	else if (mode == MODE_SHOOTING) firebonus = pyrokinetics * 3;

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_AMMO:
		case TV_BULLET:
		case TV_SHOT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DAGGER:
		case TV_AXES:
		case TV_BLUNT:
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

				if (add < 35) add = 35;
				if (mult < 3) mult = 3;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_EVIL);
				}

				if (add < 15) add = 15;
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

				if (add < 35) add = 35;
				if (mult < 2) mult = 2;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DEMON);
				}

				if (add < 35) add = 35;
				if (mult < 2) mult = 2;
			}

			/* Slay Automata */
			if ((f1 & (TR1_SLAY_AUTOMATA)) &&
			    (r_ptr->flags3 & (RF3_AUTOMATA)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_AUTOMATA);
				}

				if (add < 35) add = 35;
				if (mult < 3) mult = 3;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_DINOSAUR)) &&
			    (r_ptr->flags3 & (RF3_DINOSAUR)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_DINOSAUR);
				}

				if (add < 35) add = 35;
				if (mult < 3) mult = 3;
			}

			/* Slay Construct */
			if ((f1 & (TR1_SLAY_CONSTRUCT)) &&
			    (r_ptr->flags3 & (RF3_CONSTRUCT)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_CONSTRUCT);
				}

				if (add < 35) add = 35;
				if (mult < 3) mult = 3;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_ELEMENTAL)) &&
			    (r_ptr->flags3 & (RF3_ELEMENTAL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_ELEMENTAL);
				}

				if (add < 35) add = 35;
				if (mult < 2) mult = 2;
			}
			/* Slay Alien */
			if ((f1 & (TR1_SLAY_ALIEN)) &&
			    (r_ptr->flags3 & (RF3_ALIEN)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_ALIEN);
				}

				if (add < 35) add = 35;
				if (mult < 2) mult = 2;
			}
			/* Slay Beastman */
			if ((f1 & (TR1_SLAY_BEASTMAN)) &&
			    (r_ptr->flags3 & (RF3_BEASTMAN)))
			{
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_BEASTMAN);
				}

				if (add < 35) add = 35;
				if (mult < 2) mult = 2;
			}

			/* Slay Beastman */
			if ((f1 & (TR1_SLAY_CARDS)) &&
			    (strchr("c", r_ptr->d_char)))
			{
				if (add < 35) add = 35;
				if (mult < 2) mult = 2;
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

				/* Check for vunerablility */
				else if (r_ptr->flags8 & (RF8_VUN_ACID))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_ACID);
					}
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 5) mult = 5;
					}
					if (add < 55) add = 55;
				}

				/* Otherwise, take the damage */
				else
				{
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 3) mult = 3;
					}
					if (add < 35) add = 35;
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

				/* Check for vunerablility */
				else if (r_ptr->flags8 & (RF8_VUN_ELEC))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_ELEC);
					}
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 5) mult = 5;
					}
					if (add < 55) add = 55;
				}

				/* Otherwise, take the damage */
				else
				{
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 3) mult = 3;
					}
					if (add < 35) add = 35;
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

				/* Check for vunerablility */
				else if (r_ptr->flags8 & (RF8_VUN_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_FIRE);
					}
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 5) mult = 5;
					}
					if (add < 55) add = 55;
				}

				/* Otherwise, take the damage */
				else
				{
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 3) mult = 3;
					}
					if (add < 35) add = 35;
				}
			}

			/* Brand (Cold) */
			if (f1 & (TR1_BRAND_ICE))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ICE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_ICE);
					}
				}

				/* Check for vunerablility */
				else if (r_ptr->flags8 & (RF8_VUN_ICE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_ICE);
					}
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 5) mult = 5;
					}
					if (add < 55) add = 55;
				}

				/* Otherwise, take the damage */
				else
				{
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 3) mult = 3;
					}
					if (add < 35) add = 35;
				}
			}

			/* Brand (Poison) */
			if (f1 & (TR1_BRAND_POISON))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_POIS);
					}
				}

				/* Check for vunerablility */
				else if (r_ptr->flags8 & (RF8_VUN_POIS))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_POIS);
					}
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 5) mult = 5;
					}
					if (add < 55) add = 55;
				}

				/* Otherwise, take the damage */
				else
				{
					if (mode >= MODE_SHOOTING) 
					{
						if (mult < 3) mult = 3;
					}
					if (add < 35) add = 35;
				}
			}
			
			/* Brand (Fire) */
			if (firebonus > 0)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Check for vunerablility */
				else if (r_ptr->flags8 & (RF8_VUN_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_FIRE);
					}
					else if (add < (firebonus * 2)) add = (firebonus * 2);
				}

				/* Otherwise, take the damage */
				else
				{
					if (add < firebonus) add = firebonus;
				}
			}

			if (r_ptr->flags8 & (RF8_VUN_EDGED))
			{
				if (f2 & (TR2_EDGED))
				{
					add += 45;
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_EDGED);
					}
				}
			}
			if (r_ptr->flags8 & (RF8_VUN_BLUNT))
			{
				if (f2 & (TR2_BLUNT))
				{
					add += 45;
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_BLUNT);
					}
				}
			}
			if (r_ptr->flags8 & (RF8_VUN_PIERCE))
			{
				if (f2 & (TR2_PIERCE))
				{
					add += 45;
					if (m_ptr->ml)
					{
						l_ptr->r_flags8 |= (RF8_VUN_PIERCE);
					}
				}
			}
			break;
		}
	}
	total = (tdam + add) * mult;
	
	/* Return the total damage */
	return (total);
}

/*
 * Extract the "total damage reduction" from a given weapon impliment 
 * type (edged, blunt, pierce) object hitting a given monster.
 *
 * msg_mode being set to 1 is a nifty little hack that only prints 
 * the message if the blow is the first blow, controlling message bloat.
 *
 */

static sint totminus_dam_aux(const object_type *o_ptr, int tdam, monster_type *m_ptr, int msg_mode, int mode)
{
	int mult = 1;
	int div = 1;
	int total;
	char m_name[80];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	if (mode == MODE_MELEE)
	{
		switch (o_ptr->tval)
		{
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_DAGGER:
			case TV_AXES:
			case TV_BLUNT:
			{
				if (r_ptr->flags3 & (RF3_IM_EDGED))
				{
					if (f2 & (TR2_EDGED))
					{
						if ((f2 & (TR2_PIERCE)) || (f2 & (TR2_BLUNT))) div += 2;
						else div = 5;
						if (msg_mode == 1) msg_format("Your blade glances off %s!", m_name);
						if (m_ptr->ml)
						{
							l_ptr->r_flags3 |= (RF3_IM_EDGED);
						}
					}
				}
				if (r_ptr->flags3 & (RF3_IM_BLUNT))
				{
					if (f2 & (TR2_BLUNT))
					{
						if ((f2 & (TR2_PIERCE)) || (f2 & (TR2_EDGED))) div += 2;
						else div = 5;
						if (msg_mode == 1) msg_format("Your blow glances off %s!", m_name);
						if (m_ptr->ml)
						{
							l_ptr->r_flags3 |= (RF3_IM_BLUNT);
						}
					}
				}
				if (r_ptr->flags3 & (RF3_IM_PIERCE))
				{
					if (f2 & (TR2_PIERCE))
					{
						/* Can we use the weapon a different way? */
						if ((f2 & (TR2_BLUNT)) || (f2 & (TR2_EDGED))) div = +2;
						else div = 5;
						if (msg_mode == 1) msg_format("Your weapon point glances off %s!", m_name);
						if (m_ptr->ml)
						{
							l_ptr->r_flags3 |= (RF3_IM_PIERCE);
						}
					}
				}
				if (r_ptr->flags2 & (RF2_GASEOUS))
				{
						div = 10;
						if (msg_mode == 1) msg_format("Your blow sweeps through %s!", m_name);
						if (m_ptr->ml)
						{
							l_ptr->r_flags2 |= (RF2_GASEOUS);
						}
				}
			}
		}
	}
	else if (mode == MODE_MARTIAL_ARTS)
	{
		if (r_ptr->flags3 & (RF3_IM_BLUNT))
		{
				div = 5;
				if (msg_mode == 1) msg_format("Your blow glances off %s!", m_name);
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_IM_BLUNT);
					// otherwise this flag will not be noticed 
					if (r_ptr->flags2 & (RF2_IMPENT))
						l_ptr->r_flags2 |= (RF2_IMPENT);
				}
		}	
		else if (r_ptr->flags2 & (RF2_IMPENT)) 
		{
				div = 5;
				if (msg_mode == 1) msg_format("Your blow glances off %s!", m_name);
				if (m_ptr->ml)
				{
					l_ptr->r_flags2 |= (RF2_IMPENT);
				}
		}	
		if (r_ptr->flags2 & (RF2_GASEOUS))
		{
				div = 10;
				if (msg_mode == 1) msg_format("Your blow sweeps through %s!", m_name);
				if (m_ptr->ml)
				{
					l_ptr->r_flags2 |= (RF2_GASEOUS);
				}
		}


	}
	else if (mode == MODE_SHOOTING)
	{
		if (r_ptr->flags2 & (RF2_IMPENT)) 
		{
			div = 5;
			msg_format("Your shot bounces off %s!", m_name);
			if (m_ptr->ml)
			{
				l_ptr->r_flags2 |= (RF2_IMPENT);
			}
		}		

		if (r_ptr->flags2 & (RF2_GASEOUS))
		{
				div = 10;
				if (msg_mode == 1) msg_format("Your blow sweeps through %s!", m_name);
				if (m_ptr->ml)
				{
					l_ptr->r_flags2 |= (RF2_GASEOUS);
				}
		}
	
	
	}
	total = tdam / div;
	/* Return the total damage */
	return (total);

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

	/* Hack -- 5% chance of certain hit, 5% chance of certain miss */
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
			msg_print("Click! You step on a steam-powered springboard!");
			if (p_ptr->ffall)
			{
				msg_print("You land gently on your feet.");
			}
			else
			{
				dam = damroll(p_ptr->depth, 8);
				take_hit(dam, name, TRUE);
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
				dam = damroll(p_ptr->depth, 6);
				take_hit(dam, name, TRUE);
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
				dam = damroll(p_ptr->depth, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name, TRUE);
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
				dam = damroll(p_ptr->depth, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));

					if (resist_effect(RS_PSN))
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
				take_hit(dam, name, TRUE);
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
				(void)summon_specific(y, x, p_ptr->depth, 0, FALSE);
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
			dam = damroll(p_ptr->depth * 2, 6);
			fire_dam(dam, GF_FIRE, "a fire trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(p_ptr->depth * 2, 6);
			acid_dam(dam, GF_ACID, "an acid trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(p_ptr->depth * 5))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, TRUE);
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
			if (check_hit(p_ptr->depth * 5))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, TRUE);
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
			if (check_hit(p_ptr->depth * 5))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, TRUE);
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
			if (check_hit(p_ptr->depth * 5))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, TRUE);
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
			if (!(resist_effect(RS_PSN)))
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


/*
	Function performs a natural attack - some of these attacks have side 
	effects slowing/stunning/poisoning the attacked monster. Natural attacks 
	have higher chance to hit then melee attacks. Both weapon and martial 
	skills are used to calculate hit chance and with higher increment values 
	when in case of melee attacks. 

	Added return type showing whether the monster was killed by the attack or 
	not. If the attack type is unknown no attack is made. 
*/
bool natural_attack(s16b m_idx, int attack, bool *fear)
{
	int         k, bonus, chance;
	int         n_weight = 0;
	int					message = 0;
		
	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	

	char       	m_name[80];
	int        	dss, ddd;
	char       	*atk_desc;

	switch (attack)
	{
		case MUT4_SCOR_TAIL:
			dss = 3;
			ddd = 7;
			n_weight = 5;
			atk_desc = "tail";
			p_ptr->energy_use += 30;
			break;
		case MUT4_HORNS:
			dss = 2;
			ddd = 6;
			n_weight = 15;
			p_ptr->energy_use += 20;
			atk_desc = "horns";
			break;
		case MUT4_BEAK:
			dss = 2;
			ddd = 4;
			n_weight = 5;
			p_ptr->energy_use += 20;
			atk_desc = "beak";
			break;
		case MUT4_TUSKS:
			dss = 2;
			ddd = 6;
			n_weight = 30;
			p_ptr->energy_use += 30;
			atk_desc = "tusks";
			break;
		case MUT4_CLAWS:
			dss = 2;
			ddd = 3;
			n_weight = 5;
			p_ptr->energy_use += 10;
			atk_desc = "claws";
			break;
		case MUT4_TENTACLES:
			dss = 3;
			ddd = 3;
			n_weight = 20;
			p_ptr->energy_use += 40;
			atk_desc = "tentacles";
			break;
		case MUT4_ALPHA_SPURS:
			dss = 3;
			ddd = 6;
			n_weight = 20;
			p_ptr->energy_use += 80;
			atk_desc = "spurs";
			break;
		case MUT4_BETA_SPURS:
			dss = 5;
			ddd = 6;
			n_weight = 80;
			p_ptr->energy_use += 60;
			atk_desc = "spurs";
			break;
		case MUT4_GAMMA_SPURS:
			dss = 9;
			ddd = 6;
			n_weight = 100;
			p_ptr->energy_use += 40;
			atk_desc = "spurs";
			break;
		case MUT4_DELTA_SPURS:
			dss = 15;
			ddd = 8;
			n_weight = 400;
			p_ptr->energy_use += 20;
			atk_desc = "spurs";
			break;
		default:
			return FALSE;
/*		dss = ddd = n_weight = 1; */
/*		atk_desc = "undefined body part"; */	
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
	chance = ((p_ptr->skills[SK_TOHIT].skill_rank * 8) + (bonus * BTH_PLUS_ADJ));

	/* Melee and unarmed skill are additive for natural attacks */
	if (p_ptr->skills[SK_INTER_COMBAT].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_INTER_COMBAT].skill_rank * 6;
	}
	if (p_ptr->skills[SK_ADV_COMBAT].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_ADV_COMBAT].skill_rank * 5;
	}
	if (p_ptr->skills[SK_MASTER_COMBAT].skill_rank > 0)
	{
		chance += p_ptr->skills[SK_MASTER_COMBAT].skill_rank * 4;
	}
	if (p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 0)
	{
		chance += p_ptr->skills[SK_TOHIT_MARTIAL].skill_rank * 6;
	}
	if (p_ptr->skills[SK_INTER_MARTIAL].skill_max > 0)
	{
		chance += p_ptr->skills[SK_INTER_MARTIAL].skill_rank * 5;
	}
	if (p_ptr->skills[SK_ADV_MARTIAL].skill_max > 0)
	{
		chance += p_ptr->skills[SK_ADV_MARTIAL].skill_rank * 4;
	}
	
	/* Some monsters are great at dodging  -EZ- */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (!m_ptr->csleep) &&
	    (one_in_(2)) && (r_ptr->d_char == 'G'))
	{
			
			if ((p_ptr->tim_wraith) || (p_ptr->wraith_form) || (p_ptr->blessed))
			{
				/* Nothing - Incoperal players can strike at ghosts */
			}
			else 
			{
				message_format(MSG_MISS, 0, "Your blow passes right through %s!", m_name);

				/* Learn that monster can dodge */
				l_ptr->r_flags2 |= (RF2_PASS_WALL);
				return FALSE;
			}
	}

	
	/* Test for hit */
	if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_HIT);

		/* set the message for the end of the function */
		/* You hit %s with your %s */
		message = 0;


		k = damroll(ddd, dss);
		k = critical_norm(n_weight, p_ptr->to_h, k, FALSE);

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
		
		if (r_ptr->flags3 & (RF3_IM_PIERCE))
		{
					
				k = k/5;
				message = 1; 
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_IM_PIERCE);
				}				
		}
		
		if (r_ptr->flags2 & (RF2_GASEOUS))
		{
				k = k/10;
				message = 2; 
				if (m_ptr->ml)
				{
					l_ptr->r_flags2 |= (RF2_GASEOUS);
				}
		}		

		/* Complex message */
		if (p_ptr->wizard)
		{
			msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
		}
		
		if (message == 0) msg_format("You hit %s with your %s.", m_name, atk_desc);
		/* This has subject verb agreement issues. XCCCX */
		else if (message == 1) msg_format("Your %s glance off %s!", atk_desc, m_name);
		else if (message == 2) msg_format("Your %s sweep through %s!", atk_desc, m_name);

		/* Damage, check for fear and mdeath */
		switch (attack)
		{
			case MUT4_SCOR_TAIL:
			{
				project(-1, 0, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx, 
								k, GF_POISON, PROJECT_KILL, 0, 0);
				if (m_ptr->hp <= 0)
					return TRUE;
				break;
			}
			case MUT4_HORNS:
			case MUT4_BEAK:
			case MUT4_TUSKS:
			case MUT4_CLAWS:
			case MUT4_ALPHA_SPURS:
			{
				return mon_take_hit(m_idx, k, fear, NULL);
				// break;
			}
			case MUT4_TENTACLES:
			{
				if (mon_take_hit(m_idx, k, fear, NULL)) 
					return TRUE;
				project(-1, 0, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx, 
									k, GF_SLOW, PROJECT_KILL, 0, 0);
				break;
			}
			case MUT4_BETA_SPURS:
			case MUT4_GAMMA_SPURS:
			{
				if (mon_take_hit(m_idx, k, fear, NULL)) 
					return TRUE;
				project(-1, 0, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx, 
									k, GF_STUN, PROJECT_KILL, 0, 0);			
				break;
			}
			case MUT4_DELTA_SPURS:
			{
				if (mon_take_hit(m_idx, k, fear, NULL)) 
					return TRUE;
				project(-1, 0, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx, 
									k, GF_STUN, PROJECT_KILL, 0, 0);			
				project(-1, 0, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx, 
									k, GF_SLOW, PROJECT_KILL, 0, 0);			
				break;
			}
			default:
			{
			}
		}
		// default - it was not the killing blow
		return FALSE;
	}
	/* Player misses */
	else
	{
		/* Sound */
		sound(SOUND_MISS);

		/* Message */
		message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
		return FALSE;
	}
}

/* 
 * Handles the rapidly increasing msg code for py_attack.
 * This function was written to avoid message bloat, when
 * characters (steam-mecha) have 9 attacks, just to get all 9 
 * strikes against a monster that you're not doing much damage
 * to (such as a panzer tank) can require hitting the space bar
 * quite a bit. This produces one message for each set of attacks
 * the player makes. 
 *
 * I should spend some time expanding on the number of available
 * messages
 */
 
static void py_attack_msg(char m_name[80], char m_pronoun[80], bool martial, bool living,
								bool death, int dam_type, int crittrack, 
									int forcehits, int blowhits, int damtotal)
{
	char damageadj[60]= "";
	char blowadj[60]= "";
	char forceadj[60]= "";
	char critmsg[60]= "";

	int rand_msg = randint(2);

	/* Message - Perhaps this should be based off of damage */
	if (blowhits == 0)
	{
		/* Message */
		message_format(MSG_MISS, 0, "You miss %s.", m_name);
	}
	else 
	{

		/* critical tracking */
		if (crittrack < 50) strcpy(critmsg, "good");
		else if (crittrack < 100) strcpy(critmsg, "great");
		else if (crittrack < 150) strcpy(critmsg, "superb");
		else if (crittrack < 200) strcpy(critmsg, "first-rate");
		else if (crittrack < 300) strcpy(critmsg, "excellent");		
		else if (crittrack < 400) strcpy(critmsg, "*GREAT*");
		else strcpy(critmsg, "*SUPERB*");
		
		if (forcehits < 2) 	strcpy(forceadj, " weakly");
		else if (forcehits < 4) strcpy(forceadj, "");
		else if (forcehits < 8) strcpy(forceadj, " strongly");
		else if (forcehits < 20) strcpy(forceadj, " powerfully");
		else strcpy(forceadj, " devastatingly");
			
		if (blowhits == 1) strcpy(blowadj, ""); 
		else if (blowhits == 2)(rand_msg == 2)? strcpy(blowadj, "Two"): strcpy(blowadj, " two"); 
		else if (blowhits == 3)(rand_msg == 2)? strcpy(blowadj, "Three"): strcpy(blowadj, " three"); 
		else if (blowhits == 4)(rand_msg == 2)? strcpy(blowadj, "Four"): strcpy(blowadj, " four"); 
		else if (blowhits == 5)(rand_msg == 2)? strcpy(blowadj, "Five"): strcpy(blowadj, " five"); 
		else if (blowhits == 6)(rand_msg == 2)? strcpy(blowadj, "Six"): strcpy(blowadj, " six"); 
		else if (blowhits == 7)(rand_msg == 2)? strcpy(blowadj, "Seven"): strcpy(blowadj, " seven"); 
		else if (blowhits >= 8)(rand_msg == 2)? strcpy(blowadj, "Many"): strcpy(blowadj, " many"); 

		if (martial)
		{
//			int fha;
		//	int rand_msg = randint(3);
			int rand_strike =  rand_range(-50, 100) + damtotal/10;
#if 0			
			fha = forcehits / blowhits;
			if (fha < 1) fha = 1;
			/* critical tracking */
#endif		
			
			if (rand_strike < 31) strcpy(damageadj, "hit");
			else if (rand_strike < 51) strcpy(damageadj, "punch");
			else if (rand_strike < 71) strcpy(damageadj, "kick");
			else if (rand_strike < 91) strcpy(damageadj, "knee strike");
			else if (rand_strike < 101) strcpy(damageadj, "clobber"); 
			else if (rand_strike < 151) strcpy(damageadj, "elbow strike");
			else if (rand_strike < 201) strcpy(damageadj, "jump kick"); 
			else if (rand_strike < 251) strcpy(damageadj, "circle kick"); 
			else if (rand_strike < 301) strcpy(damageadj, "Eagle Claw"); 
			else if (rand_strike < 401) strcpy(damageadj, "Flying Kick"); 
			else strcpy(damageadj, "Dragon Fist");

		}
		else 
		{
			if (damtotal < 21 && dam_type == 2) strcpy(damageadj, "poke");
			else if (damtotal < 21 && dam_type == 1) strcpy(damageadj, "bash");
			else if (damtotal < 21) strcpy(damageadj, "scratch");
			else if (damtotal < 51) strcpy(damageadj, "hit");
			else if (damtotal < 101 && dam_type == 2) strcpy(damageadj, "stab");
			else if (damtotal < 101 && dam_type == 1) strcpy(damageadj, "crush");
			else if (damtotal < 101) strcpy(damageadj, "cut");
			else if (damtotal < 151 && dam_type == 2) strcpy(damageadj, "jab");
			else if (damtotal < 151 && dam_type == 1) strcpy(damageadj, "smash");
			else if (damtotal < 151) strcpy(damageadj, "gash");
			else if (damtotal < 201 && dam_type == 2) strcpy(damageadj, "pierce");
			else if (damtotal < 201 && dam_type == 1) strcpy(damageadj, "maul"); 
			else if (damtotal < 201) strcpy(damageadj, "slash");
			else if (damtotal < 301 && dam_type == 2) strcpy(damageadj, "tear");
			else if (damtotal < 301 && dam_type == 1) strcpy(damageadj, "mangle"); 
			else if (damtotal < 301) strcpy(damageadj, "slice");
			else if (damtotal < 501 && dam_type == 2) strcpy(damageadj, "gouge");
			else if (damtotal < 501 && dam_type == 1) strcpy(damageadj, "pulverise"); 
			else if (damtotal < 501) strcpy(damageadj, "cleave"); 
			else if (damtotal < 1001 && dam_type == 2) strcpy(damageadj, "*impale*");
			else if (damtotal < 1001) strcpy(damageadj, "*demolish*"); 
			else if (damtotal < 3001) strcpy(damageadj, "*eradicate*"); 
			else if (damtotal < 5001) strcpy(damageadj, "*annihilate*"); 
			else strcpy(damageadj, "**atomize**");
		}
		/* Note criticals */
		if (crittrack) message_format(MSG_HIT, 0, "It was a %s %s", 
			critmsg, ((blowhits == 1) ? "hit." : (martial) ? "sequence of strikes." : "sequence of hits."));

		/* msg type one */
		if (rand_msg == 1)
		{
			/* You strongly bash the chicken five times */
			if (death) message_format(MSG_HIT, 0, "You%s %s %s%s%s, %s %s.", 
										forceadj, damageadj, m_name, 
										blowadj, ((blowhits == 1) ? "" : " times"), (living)?"killing":"destroying", m_pronoun);
			else message_format(MSG_HIT, 0, "You%s %s %s%s%s.",
								forceadj, damageadj, m_name, 
									blowadj, ((blowhits == 1) ? "" : " times"));
		}
		else //if (rand_msg == 2)
		{
			char ending[3] = "";
			char *location = NULL;
			// only problematic lines are ending with 'h'-s no 's' ending problems so far
			if((location = strrchr(damageadj, '*')) != NULL)
			{
				strcpy(ending, ""); 
			}
			else if ((location = strrchr(damageadj, 'h')) != NULL)
			{
				if (strlen(location) == 1)
				{
					strcpy(ending, "es"); 
				}
				else strcpy(ending, "s"); 
			}
			else
			{
				strcpy(ending, "s"); 
			}

			if (death)
			{
				/* Two blows bash the chicken powerfully, killing it */
				if (blowhits == 1) message_format(MSG_HIT, 0, "One blow %s%s %s%s, %s %s.", 
									damageadj, ending, m_name, forceadj, (living)?"killing":"destroying", m_pronoun);
				else message_format(MSG_HIT, 0, "%s blows %s %s%s, %s %s.", 
					blowadj, damageadj, m_name, forceadj, (living)?"killing":"destroying", m_pronoun);
			}
			else
			{
				if (blowhits == 1) message_format(MSG_HIT, 0, "One blow %s%s %s%s.", 
									damageadj, ending, m_name, forceadj);
				else message_format(MSG_HIT, 0, "%s blows %s %s%s.", 
										blowadj, damageadj, m_name, forceadj);
			}
		}
	}
}

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then use martial arts
 * skill to do some major damage.
 *
 * Note that melee damage with weapons is somewhat
 * "capped", meaning that often you will do more
 * damage with an equivilant skill in martial arts.
 * This is due to several factors, the primary one 
 * being that with martial arts you forgo having a 
 * weapon equipped, and lose out on the bonuses and
 * resistances. Another factor is the relative rarity
 * of martial arts skills.
 * 
 */
void py_attack(int y, int x, int mode)
{
	/* Function Variables */
	int noise, k, bonus, chance, force, forcechance, attk_mode;
	int critplus, weightplus;
	int nat_attack = 1;
	/* Skills */
	int accstrike, tohit, martialtohit, intermartial, advmartial;
	int intercombat, advcombat, mastercombat;
	int hafted, polearm, sword, dagger, axes, blunt;
	int haftedmaster, polearmmaster, swordmaster;
	int daggermaster, axesmaster, bluntmaster;
	int vicious, dirty, power, knockdown;
	int totalmartial, totaltohit, totaltheft;
	int disable_machine, spirit_infusion;
	int sabotage, anatomy, theology, assassination;
	int theft, mastertheft;
	
	/* Tracking */
	int dammult = 4;
	int num = 0;
	int blowhits = 0;
	int forcehits = 0;
	int damtotal = 0;
	int crittrack = 0;
	u32b f1, f2, f3;
	u32b p1, p2, p3;
	
	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	object_type *o_ptr;

	char m_name[80];
	char m_pronoun[80];

	bool fear = FALSE;	
	bool do_quake = FALSE;
	bool martial = FALSE;
	bool death = FALSE;
	bool living = TRUE;
	bool sleeper = FALSE;
	/* Three different damage types - Assume Edged */ 
	int damage_type = TYPE_EDGED;

	/* Get the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Paranoia */
	accstrike = tohit = martialtohit = intermartial = advmartial = 0;
	intercombat = advcombat = mastercombat = 0;
	hafted = polearm = sword = dagger = axes = blunt = 0;
	haftedmaster = polearmmaster = swordmaster = 0;
	daggermaster = axesmaster = bluntmaster = 0;
	vicious = dirty = power = knockdown = 0;
	totalmartial = totaltohit = totaltheft = 0;
	disable_machine = spirit_infusion = 0;
	sabotage = anatomy = theology = assassination = 0;
	theft = mastertheft = 0;
	critplus = weightplus = 0;
	
	/* assign the skill values */
	if (p_ptr->skills[SK_ACC_STRIKE].skill_max > 0)
		accstrike = p_ptr->skills[SK_ACC_STRIKE].skill_rank;
	if (p_ptr->skills[SK_TOHIT].skill_max > 0)
		tohit = p_ptr->skills[SK_TOHIT].skill_rank;
	if (p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 0)
		martialtohit = p_ptr->skills[SK_TOHIT_MARTIAL].skill_rank;
	if (p_ptr->skills[SK_INTER_MARTIAL].skill_max > 0)
		intermartial = p_ptr->skills[SK_INTER_MARTIAL].skill_rank;
	if (p_ptr->skills[SK_ADV_MARTIAL].skill_max > 0)
		advmartial = p_ptr->skills[SK_ADV_MARTIAL].skill_rank;
	if (p_ptr->skills[SK_INTER_COMBAT].skill_max > 0)
		intercombat = p_ptr->skills[SK_INTER_COMBAT].skill_rank;
	if (p_ptr->skills[SK_ADV_COMBAT].skill_max > 0)
		advcombat = p_ptr->skills[SK_ADV_COMBAT].skill_rank;
	if (p_ptr->skills[SK_MASTER_COMBAT].skill_max > 0)
		mastercombat = p_ptr->skills[SK_MASTER_COMBAT].skill_rank;
	if (p_ptr->skills[SK_HAFTED].skill_max > 0)
		hafted = p_ptr->skills[SK_HAFTED].skill_rank;
	if (p_ptr->skills[SK_HAFTED_MASTER].skill_max > 0)
		haftedmaster = p_ptr->skills[SK_HAFTED_MASTER].skill_rank;
	if (p_ptr->skills[SK_POLEARM].skill_max > 0)
		polearm = p_ptr->skills[SK_POLEARM].skill_rank;
	if (p_ptr->skills[SK_POLEARM_MASTER].skill_max > 0)
		polearmmaster = p_ptr->skills[SK_POLEARM_MASTER].skill_rank;
	if (p_ptr->skills[SK_SWORD].skill_max > 0)
		sword = p_ptr->skills[SK_SWORD].skill_rank;
	if (p_ptr->skills[SK_SWORD_MASTER].skill_max > 0)
		swordmaster = p_ptr->skills[SK_SWORD_MASTER].skill_rank;
	if (p_ptr->skills[SK_DAGGER].skill_max > 0)
		dagger = p_ptr->skills[SK_DAGGER].skill_rank;
	if (p_ptr->skills[SK_DAGGER_MASTER].skill_max > 0)
		daggermaster = p_ptr->skills[SK_DAGGER_MASTER].skill_rank;
	if (p_ptr->skills[SK_AXES].skill_max > 0)
		axes = p_ptr->skills[SK_AXES].skill_rank;
	if (p_ptr->skills[SK_AXES_MASTER].skill_max > 0)
		axesmaster = p_ptr->skills[SK_AXES_MASTER].skill_rank;
	if (p_ptr->skills[SK_BLUNT].skill_max > 0)
		blunt = p_ptr->skills[SK_BLUNT].skill_rank;
	if (p_ptr->skills[SK_BLUNT_MASTER].skill_max > 0)
		bluntmaster = p_ptr->skills[SK_BLUNT_MASTER].skill_rank;
	if (p_ptr->skills[SK_VICIOUS_STRIKE].skill_max > 0)
		vicious = p_ptr->skills[SK_VICIOUS_STRIKE].skill_rank;
	if (p_ptr->skills[SK_DIRTY_FIGHTING].skill_max > 0)
		dirty = p_ptr->skills[SK_DIRTY_FIGHTING].skill_rank;
	if (p_ptr->skills[SK_POWER_STRIKE].skill_max > 0)
		power = p_ptr->skills[SK_POWER_STRIKE].skill_rank;
	if (p_ptr->skills[SK_KNOCK_DOWN].skill_max > 0)
		knockdown = p_ptr->skills[SK_KNOCK_DOWN].skill_rank;
	if (p_ptr->skills[SK_DISABLE_MACHINE].skill_max > 0)
		disable_machine = p_ptr->skills[SK_DISABLE_MACHINE].skill_rank;
	if (p_ptr->skills[SK_SPIRIT_INFUSION].skill_max > 0)
		spirit_infusion = p_ptr->skills[SK_SPIRIT_INFUSION].skill_rank;
	if (p_ptr->skills[SK_ANATOMY].skill_max > 0)
		anatomy = p_ptr->skills[SK_ANATOMY].skill_rank;
	if (p_ptr->skills[SK_SABOTAGE].skill_max > 0)
		sabotage = p_ptr->skills[SK_SABOTAGE].skill_rank;
	if (p_ptr->skills[SK_THEOLOGY].skill_max > 0)
		theology = p_ptr->skills[SK_THEOLOGY].skill_rank;
	if (p_ptr->skills[SK_ASSASSINATION].skill_max > 0)
		assassination = p_ptr->skills[SK_ASSASSINATION].skill_rank;
	if (p_ptr->skills[SK_THEFT].skill_max > 0)
		theft = p_ptr->skills[SK_THEFT].skill_rank;
	if (p_ptr->skills[SK_MASTER_THEFT].skill_max > 0)
		mastertheft = p_ptr->skills[SK_MASTER_THEFT].skill_rank;
		
	/* Total melee, theft, & martial numbers */
	totalmartial = tohit + martialtohit + intermartial + advmartial;
	totaltohit = tohit + intercombat + advcombat + mastercombat;
	totaltheft = theft + mastertheft;
	
	/* Disturb the player */
	disturb(0, 0);

	/* Find out the state of the monster */
	if (m_ptr->csleep > 0) sleeper = TRUE;
	
	/* Disturb the monster */
	m_ptr->csleep = 0;

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Extract monster pronoun */
	monster_desc(m_pronoun, m_ptr, 0x21);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);

	/* Some monsters get "destroyed" */
	if (monster_nonliving(r_ptr))	living = FALSE;

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

	/* Find out if we're using martial arts */
	if (!o_ptr->k_idx) 
	{
		martial = TRUE;
		attk_mode = MODE_MARTIAL_ARTS;
	}
	else attk_mode = MODE_MELEE;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);
	player_flags(&p1, &p2, &p3);

	/* If you can't strike with the weapon - you're kicked out! */
	if (f3 & (TR3_NEVER_BLOW)) 
	{
		/* Message */
		msg_format("Your weapon refuses to attack %s!", m_name);

		return;
	}
	
	/* Determine weapon type for attack message code */
	if (f2 & (TR2_PIERCE)) damage_type = TYPE_PIERCE;
	else if (f2 & (TR2_BLUNT)) damage_type = TYPE_BLUNT;
	
	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;

	/* add in the accurate strike skill to the bonus */
	if (accstrike && !martial) bonus += accstrike;

	/* Weapon penalty bonus is allowed to be less than 0 */
	if (o_ptr->to_h < 0) 
	{
			/* nothing */
	}
	/* otherwise, reduce frustration */
	else if (bonus < 0) bonus = 0;
	
	/* Calculate the chance to hit */
	chance = 1;

	/* this \|/ should always be true, just checking */
	if (tohit) chance = ((tohit * 5) + (bonus * BTH_PLUS_ADJ));

	/* add in the relevant combat skills */
	if (martial)
	{
		if (martialtohit) chance += martialtohit * 6;
		if (intermartial) chance += intermartial * 5;
		if (advmartial) chance += advmartial * 4;
	}
	else
	{
		if (intercombat) chance += intercombat * 4;
		if (advcombat) chance += advcombat * 3;
		if (mastercombat) chance += mastercombat * 2;
	}
	if (hafted && o_ptr->tval == TV_HAFTED) chance += hafted * 4;
	if (haftedmaster && o_ptr->tval == TV_HAFTED) chance += haftedmaster * 6;
	if (polearm && o_ptr->tval == TV_POLEARM) chance += polearm * 4;
	if (polearmmaster && o_ptr->tval == TV_POLEARM) chance += polearmmaster * 6;
	if (sword && o_ptr->tval == TV_SWORD) chance += sword * 4;
	if (swordmaster && o_ptr->tval == TV_SWORD) chance += swordmaster * 6;
	if (dagger && o_ptr->tval == TV_DAGGER) chance += dagger * 4;
	if (daggermaster && o_ptr->tval == TV_DAGGER) chance += daggermaster * 6;
	if (axes && o_ptr->tval == TV_AXES) chance += axes * 4;
	if (axesmaster && o_ptr->tval == TV_AXES) chance += axesmaster * 6;
	if (blunt && o_ptr->tval == TV_BLUNT) chance += blunt * 4;
	if (bluntmaster && o_ptr->tval == TV_BLUNT) chance += bluntmaster * 6;

	/* paranoia */
	if (chance < 1) chance = 1;
	
	
	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
		
		/* Some monsters are great at dodging  -EZ- */
		if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (!m_ptr->csleep) &&
		    (one_in_(2)) && (r_ptr->d_char == 'G') && (!(f2 & (TR2_BLESSED))))
		{
			
			if ((p_ptr->tim_wraith) || (p_ptr->wraith_form) || (p_ptr->blessed))
			{
				/* Nothing - Incoperal players can strike at ghosts */
			}
			else 
			{
				message_format(MSG_MISS, 0, "Your blow passes right through %s!", m_name);

				/* Learn that monster can dodge */
				l_ptr->r_flags2 |= (RF2_PASS_WALL);
				break;
			}
		}
	

		/* Test for hit */
		if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
		{
			/* record the strike */
			blowhits++;
			
			/* Martial Strikes */
			if (martial)
			{
				int die1 = 2;
				int die2 = 4;
				
				if (martialtohit)
				{
					/* Increase the attack die (to 2d14) */
					die2 += martialtohit / 2;
				}
				if (intermartial)
				{
					/* Increase the attack dice  to 6d21 */
					die1 += intermartial / 5;
					die2 += (intermartial + 1) / 3;
				}
				if (advmartial)
				{
					/* Increase the attack dice to 6d41 */
					die2 += 2 + (advmartial / 2);
				}
				
				/* These formulas should take into account MUS and AGI */
				/* Possibly increase Criticals */
				if (randint(640) < randint(totalmartial)) 
					critplus = randint(totalmartial / 2) * randint(totalmartial / 10);
				else critplus = 1;
				
				/* Possibly increase weight */
				if (randint(640) < randint(totalmartial))
					weightplus = randint(totalmartial / 2) * randint(totalmartial / 8);			
				else weightplus = 1;
				
				/* roll for damage */
				k = damroll(die1, die2);
				
				/* Successive martial hits do more damage */
				k =  ((k * (blowhits + 1)) / 3);
			}
			
			/* Handle normal weapon */
			else if (o_ptr->k_idx)
			{
				k = damroll(o_ptr->dd, o_ptr->ds);
	
				/* This looks like a hack for earthquake */
				if (p_ptr->impact && (k > 40)) do_quake = TRUE;
	
				/* critical flag increases weapon weight */
				if (f2 & (TR2_CRITICAL)) weightplus = o_ptr->weight + 400;
				else weightplus = o_ptr->weight;
	
				/* vorpal flag increases critical plus */
				if (f2 & (TR2_VORPAL)) critplus = o_ptr->to_h + rand_range(250, 400);
				else critplus = o_ptr->to_h;
				if (haftedmaster && o_ptr->tval == TV_HAFTED) critplus += (10 * haftedmaster);
				else if (polearmmaster && o_ptr->tval == TV_POLEARM) critplus += (10 * polearmmaster);
				else if (swordmaster && o_ptr->tval == TV_SWORD) critplus += (15 * swordmaster);
				else if (daggermaster && o_ptr->tval == TV_DAGGER) critplus += (20 * daggermaster);
				else if (axesmaster && o_ptr->tval == TV_AXES) critplus += (5 * axesmaster);
				else if (bluntmaster && o_ptr->tval == TV_BLUNT) critplus += (5 * bluntmaster);
			}
			
			/* Should never be triggered */
			else k = 1;
	

			/* Critical blow special attack */
			if (mode == PY_ATTACK_CRITICAL)
			{
				int oldk = k;
				
				/* reset the mode */
				mode = 0;
				
				/* check for critical */
				k = critical_norm(weightplus + 400, critplus + rand_range(450, 900), k * 3, martial);
				
				/* Track Criticals */
				if (k > oldk) crittrack += k - oldk;
			}
			else
			{
				int oldk = k;

				if ((anatomy) && (!(monster_nonliving(r_ptr))))
				{
					if (rand_int(320) < anatomy)
					{
						weightplus += 10 * (randint(anatomy));
						critplus += rand_range(50, 100);
					}
				}				

				if ((sabotage) && (monster_automata(r_ptr)))
				{
					if (rand_int(160) < sabotage)
					{
						weightplus += 10 * (randint(sabotage));
						critplus += rand_range(50, 100);
					}
				}				
				
				/* check for critical */
				k = critical_norm(weightplus, critplus, k, martial);
				
				/* Track Criticals */
				if (k > oldk) crittrack += k - oldk;
			}
			
			/* add in object damage bonus */
			k += o_ptr->to_d;

			/* Apply the player damage bonuses */
			k += p_ptr->to_d;
			
			
			/* handle the force of the weapon */
			/* if you have a good weapon, you can get these (effective) extra blows */
			if ((o_ptr->force > 0) || (f1 & (TR1_BRAND_FORCE)) || (p1 & (TR1_BRAND_FORCE)))
			{
				int forcevalue = o_ptr->force;

				/* forcechance is raised later */
				forcechance = chance;
				dammult = 3;
				
				
				/* Powerstrike increases force strike*/
				if (power > 0)
				{
					forcechance += power * 3;
					dammult += power / 6;
				}
				
				/* Force brand increase force strikes */
				if ((f1 & (TR1_BRAND_FORCE)) || (p1 & (TR1_BRAND_FORCE)))
				{
					forcevalue += 2;
					forcechance += 10;
					dammult +=1;
				}
				if ((anatomy > 1) && (!(monster_nonliving(r_ptr))))
				{
					dammult += (anatomy / 5);
				}
				if ((sabotage > 1) && (monster_automata(r_ptr)))
				{
					dammult += (sabotage / 4);
				}
				for (force = 0; force < forcevalue; force++)
				{
					if (test_hit_norm(forcechance, (r_ptr->ac + (r_ptr->level * 2) + 
											(r_ptr->ac * (force * (3/2)))), m_ptr->ml))
					{
						/* Track the total number of forcehits */
						forcehits++;
						
						/* Increase the damage mutipler */
						dammult++;
					
						/* this code is to see how often the extra force hits hit */
						if (p_ptr->wizard)  msg_format("force #%d hits", forcehits);			
					}
				}
			}

			/* Check for assassination attempt */
			/* Need to add special code for py_attack_msg for assassination XCCCX */
			if (sleeper && assassination)
			{
				dammult += randint(assassination) * randint(4);
			}

			if (p_ptr->wizard)  msg_format("k: %d", k);			
			if (p_ptr->wizard)  msg_format("dammult: %d", dammult);			

			/* damnult is 4 and increases by 1 for each force blow */
			/* Each successful force blow is a 25% increase in damage */
			k *= dammult;

			/* divide the new k(damage total) by three to increase damage */
			k /= 4;

			k = totplus_dam_aux(o_ptr, k, m_ptr, MODE_MELEE);
			
			/* Apply the vicious strike bonuses */
			/* Note does not apply to martial attacks */
			if (vicious && !martial) k += vicious;

			/* Apply the dirty fighting bonuses */
			/* Note applies to martial attacks */
			if (dirty) k += damroll((dirty / 4) + 1, 6);

			k = totminus_dam_aux(o_ptr, k, m_ptr, blowhits, attk_mode);

			if (spirit_infusion) k += (spirit_infusion);
			
			/* No negative damage */
			if (k < 1) k = 1;
			
			/* No flipping damage around to give thousands of hit points */
			/* "will wrap around when damage is over monster HP + 32768" */
			if (k > 32000) k = 32000;
						
			/* Track total damage  */
			damtotal += k;
			
			/* Complex message */
			if (p_ptr->wizard) 
			{ 
				msg_format("You do %d (out of %d) damage. (%d total).", k, m_ptr->hp, damtotal);
			} 

			/* Anger the monster */
			if (k > 0) anger_monster(m_ptr);
			
			if (blowhits == 1)
			{
				/* Make some noise */
				noise = p_ptr->base_wakeup_chance / 2;
			
				/* Burglary skill greatly reduces noise */
				/* Maybe I should replace this with a log function? */
				noise += 901 - (p_ptr->skill_stl * 30);
				
				if (noise < 0) noise = 0;

				if (martial) noise /= 2;
	
				/* Increase the noise level */
				add_wakeup_chance += noise;
			}
			
					
			/* This battle message code should be moved to it's own function */
			if (k > m_ptr->hp) py_attack_msg(m_name, m_pronoun, martial, living,
												TRUE, damage_type, crittrack, 
												forcehits, blowhits, damtotal);

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) 
			{
				death = TRUE;
				break;
			}
			
			/* Heal the player if weapon is vampiric */
			if (f2 & (TR2_VAMPIRIC)) 
			{
				hp_player(rand_range(k / 4, k / 2));
				wp_player(rand_range(1, k / 100));
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
						l_ptr->r_flags3 |= (RF3_NO_CONF);
					}
	
					msg_format("%^s is unaffected.", m_name);
				}
				else if (rand_int(50) < r_ptr->level)
				{
					msg_format("%^s is unaffected.", m_name);
				}
				else
				{
					msg_format("%^s appears confused.", m_name);
					m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
				}
			}

			if ((!death) && mode == PY_ATTACK_STUN)
			{
				/* reset mode */
				mode = 0;
				
				/* Stun the monster */
				project_ball(-1, 0, p_ptr->py, p_ptr->px, y, x, k,
				GF_STUN, 0L, 0);
			}
			
			if ((!death) && (randint(30) < knockdown))
			{
				/* Stun the monster */
				project_ball(-1, 0, p_ptr->py, p_ptr->px, y, x, k,
				GF_STUN, 0L, 0);
			}
					
			if ((!death) && (disable_machine) && 
				((r_ptr->flags3 & (RF3_AUTOMATA)) || 
				 (r_ptr->flags3 & (RF3_CONSTRUCT))))
			{
				if (disable_machine > rand_int(20))
				{
					int drain = disable_machine * 6;
					project_ball(-1, 0, p_ptr->py, p_ptr->px, y, x, drain,
					GF_MPDRAIN_MACHINE, 0L, 0);			
				}
			}


			/* Attempt to steal the money */
			if (randint(50) < totaltheft)
			{
				int totalhaul;
				
				/* Get some gold */
				totalhaul = randint(theft) * randint(theft);
				totalhaul = totalhaul * randint(mastertheft);
				
				/* Collect the gold */
				p_ptr->au += totalhaul;
				
				/* Redraw gold */
				p_ptr->redraw |= (PR_GOLD);
		
				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
		
			}
		}
	}
	
	if (!death) py_attack_msg(m_name, m_pronoun, martial, living, 
								death, damage_type, crittrack, 
								forcehits, blowhits, damtotal);
	
	/* Mutations which yield extra 'natural' attacks */
	if (!death)
	{
		for (nat_attack; nat_attack < MUT4_DELTA_SPURS + 1; nat_attack = nat_attack << 1)
		{
			if(p_ptr->muta4 & nat_attack)
			{
				if (natural_attack(cave_m_idx[y][x], nat_attack, &fear))
				{
					death = TRUE;
					break;
				}
			}
		}
	}
/*
//	if ((!no_extra) && (!death))
	{
		if ((p_ptr->muta4 & MUT4_HORNS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_HORNS, &fear);
		if ((p_ptr->muta4 & MUT4_CLAWS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_CLAWS, &fear);
		if ((p_ptr->muta4 & MUT4_BEAK) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_BEAK, &fear);
		if ((p_ptr->muta4 & MUT4_SCOR_TAIL) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_SCOR_TAIL, &fear);
		if ((p_ptr->muta4 & MUT4_TUSKS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_TUSKS, &fear);
		if ((p_ptr->muta4 & MUT4_TENTACLES) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_TENTACLES, &fear);
		if ((p_ptr->muta4 & MUT4_ALPHA_SPURS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_ALPHA_SPURS, &fear);
		if ((p_ptr->muta4 & MUT4_BETA_SPURS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_BETA_SPURS, &fear);
		if ((p_ptr->muta4 & MUT4_GAMMA_SPURS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_GAMMA_SPURS, &fear);
		if ((p_ptr->muta4 & MUT4_DELTA_SPURS) && (m_ptr->hp > 0))
			natural_attack(cave_m_idx[y][x], MUT4_DELTA_SPURS, &fear);
	}
	if (m_ptr->hp <= 0) death = TRUE;

	/* Knockback effects */
	if ((!death) && ((mode == PY_ATTACK_IMPACT) || 
			((f2 & (TR2_IMPACT)) && one_in_(4))))
	{
		/* reset mode */
		mode = 0;
		
		/* Thrust monster or player away. */
		thrust_away(-1, y, x, 1 + rand_int(3));
	}


	
	/* Hack -- rebate energy for warrior types */
	/* Energy is deducted in cmd-misc.c */
	/* First for hand to hand guys. */
	if (death && martial) 
	{
		if ((totalmartial > rand_range(20, 30)) &&
			(num < p_ptr->num_blow) &&
			(p_ptr->energy_use)) 
		{
			/* Use energy only for blows expended */
			p_ptr->energy_use = (p_ptr->energy_use * num) /
									(p_ptr->num_blow);
		}
	}
	/* Then for warrior types */
	else if (death)
	{
		if ((totaltohit > rand_range(30, 50)) &&
			(num < p_ptr->num_blow) &&
			(p_ptr->energy_use)) 
		{
			/* Use energy only for blows expended */
			p_ptr->energy_use = (p_ptr->energy_use * num) /
									(p_ptr->num_blow);
		}
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
int breakage_chance(const object_type *o_ptr)
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
 * You may only fire items that "match" your gun.
 *
 * You must use pistols + pistol ammo, rifle + rifle ammo,
 * shotguns + shotgun ammo.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that guns of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that guns of "Extra Shots" give an extra shot.
 *
 */
void do_cmd_fire(int gun_kata)
{
	int dir, item, kata, savedir;
	int i, j, y, x, ty, tx;
	int tdam, tdis, thits, tmul, degree;
	int bonus, chance, noise, weightplus, critplus;
	int keeneyes, anatomy, sabotage, rifle, pistol, shotgun;
	int vicious, accurate, tohitshoot, intershoot, advshoot;
	int mastershoot, pyrokinetics;
	u32b f1, f2, f3;


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

	int msec = op_ptr->delay_factor * op_ptr->delay_factor * 2;

	keeneyes = anatomy = sabotage = rifle = pistol = shotgun = 0;
	vicious = accurate = tohitshoot = intershoot = advshoot = 0;
	mastershoot = pyrokinetics = 0;
	weightplus = critplus = degree = 0;

	if (p_ptr->skills[SK_KEEN_EYES].skill_max > 0)
		keeneyes = p_ptr->skills[SK_KEEN_EYES].skill_rank / 4;
	if (p_ptr->skills[SK_ANATOMY].skill_max > 0)
		anatomy = p_ptr->skills[SK_ANATOMY].skill_rank;
	if (p_ptr->skills[SK_SABOTAGE].skill_max > 0)
		sabotage = p_ptr->skills[SK_SABOTAGE].skill_rank;
	if (p_ptr->skills[SK_RIFLE].skill_max > 0)
		rifle = p_ptr->skills[SK_RIFLE].skill_rank;
	if (p_ptr->skills[SK_PISTOL].skill_max > 0)
		pistol = p_ptr->skills[SK_PISTOL].skill_rank;
	if (p_ptr->skills[SK_SHOTGUN].skill_max > 0)
		shotgun = p_ptr->skills[SK_SHOTGUN].skill_rank;
	if (p_ptr->skills[SK_VICIOUS_SHOT].skill_max > 0)
		vicious = p_ptr->skills[SK_VICIOUS_SHOT].skill_rank;
	if (p_ptr->skills[SK_ACC_SHOT].skill_max > 0)
		accurate = p_ptr->skills[SK_ACC_SHOT].skill_rank;
	if (p_ptr->skills[SK_TOHIT_SHOOTING].skill_max > 0)
		tohitshoot = p_ptr->skills[SK_TOHIT_SHOOTING].skill_rank;
	if (p_ptr->skills[SK_INTER_SHOOTING].skill_max > 0)
		intershoot = p_ptr->skills[SK_INTER_SHOOTING].skill_rank;
	if (p_ptr->skills[SK_ADV_SHOOTING].skill_max > 0)
		advshoot = p_ptr->skills[SK_ADV_SHOOTING].skill_rank;
	if (p_ptr->skills[SK_MASTER_SHOOTING].skill_max > 0)
		mastershoot = p_ptr->skills[SK_MASTER_SHOOTING].skill_rank;
	if (p_ptr->skills[SK_PYROKINETICS].skill_max > 0)
		pyrokinetics = p_ptr->skills[SK_PYROKINETICS].skill_rank;
	
	/* Get the "gun" (if any) */
	j_ptr = &inventory[INVEN_GUN];

	/* Extract the flags */
	object_flags(j_ptr, &f1, &f2, &f3);

	/* Require a usable launcher */
	if (!j_ptr->tval || !p_ptr->ammo_tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}

	/* Require proper missile */
	/* item_tester_tval = p_ptr->ammo_tval; */

#if 0
	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}
#endif
	item = INVEN_LOADEDGUN;
	o_ptr = &inventory[item];
	
	if (!o_ptr->k_idx) 
	{
		msg_print("Your firearm is empty!");
		return;
	}

	if (cursed_p(o_ptr))
	{
		msg_format("Click! Your gun has jammed.");
		return;
	}
	
	if (o_ptr->number < gun_kata) 
	{
		msg_format("You don't have enough ammo (%d shots).", gun_kata);
		return;
	}

	/* Get a direction (or cancel) */
	if (gun_kata)
	{
		/* no direction needed */
	}
	else if (!get_aim_dir(&dir)) return;

	/* save the direction for the firebolt */
	savedir = dir;
	
	if (!gun_kata)
	{
		/* Predict the "target" location */
		ty = p_ptr->py + 99 * ddy[dir];
		tx = p_ptr->px + 99 * ddx[dir];

		/* Check for "target request" */
		if ((dir == 5) && target_okay())
		{
			tx = p_ptr->target_col;
			ty = p_ptr->target_row;
		}

		/* Don't shoot at my feet */
		if (tx == p_ptr->px && ty == p_ptr->py)
		{
			p_ptr->energy_use = 0;
			return;
		}
	}


	if (!gun_kata) j = 1;
	else j = gun_kata;

	for (kata = 0; kata < j; kata++)
	{
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
		
		/* Add in the slays on the gun to the bullet */
		i_ptr->flags1 |= f1;
		
		/* Use the proper number of shots */
		thits = p_ptr->num_fire;
	
		/* Base damage from thrown object plus launcher bonus */
		tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d + j_ptr->to_d + vicious; 
	
		/* Actually "fire" the object */
		bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
	
		/* Add in the accurate shot skill to the bonus */
		if (accurate)
		{
			bonus += accurate;
		}
	
		/* Weapon bonus penalty is allowed to be less than 0 */
		if ((i_ptr->to_h < 0) || (j_ptr->to_h < 0)) 
		{
			/* nothing */
		}
		/* Make steam not frustrating */
		else if (bonus < 0) bonus = 0;
	
		chance = ((tohitshoot * 5) + (bonus * BTH_PLUS_ADJ));
	
		/* add in the relevant combat skills */
		if (intershoot) chance += intershoot *4;
		if (advshoot) chance += advshoot * 3;
		if (mastershoot) chance += mastershoot * 2;
		if (pistol && i_ptr->tval == TV_AMMO) chance += pistol * 6;
		if (rifle && i_ptr->tval == TV_BULLET) chance += rifle * 6;
		if (shotgun && i_ptr->tval == TV_SHOT) chance += shotgun * 6;
	
		/* Assume a base multiplier */
		tmul = p_ptr->ammo_mult;
	
	
		/* Base / default range */
		tdis = 5;
	
		/* Range & spread of shotguns */
		tdis = j_ptr->range;
		degree = j_ptr->degree;		
		
		/* add in the keen eyes skill */
		tdis += keeneyes;
		if (tdis > 20) tdis = 20;
		
		/* Start at the player */
		y = p_ptr->py;
		x = p_ptr->px;
	
		if (gun_kata)
		{	
			ty = p_ptr->py + 99 * ddy_cdd[(kata * 3) % 8];
			tx = p_ptr->px + 99 * ddx_cdd[(kata * 3) % 8];
			p_ptr->energy_use = 100;
		}
		else
		{
			/* Take a (partial) turn */
			p_ptr->energy_use = (100 / thits) + randint(25);
		}
	
		
		/* Calculate the path */
		path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, &ty, &tx, PROJECT_THRU);
	
		/* Hack -- Handle stuff */
		handle_stuff();
		
		/* Make some noise. */
		noise = p_ptr->base_wakeup_chance / 2;
	
		/* stealth skill mildly reduces noise */
		/* Guns are loud */
		noise += 8000 - (p_ptr->skill_stl * 100);
	
		/* Increase the noise level */
		add_wakeup_chance += noise;
		
		/* Hack -- shotguns go BOOM */
		if (i_ptr->tval == TV_SHOT && i_ptr->sval == SV_AMMO_BSHOT)
		{
			/* Boost the damage (Boosts damage for arcs only) */
			tdam *= tmul;

			if (pyrokinetics) fire_arc(GF_PYRO_SHOT, dir, tdam, tdis, degree);
			else fire_arc(GF_SHOT, dir, tdam, tdis, degree);
			break;
		}
		
		/* Project along the path */
		else for (i = 0; i < path_n; ++i)
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
	
				char m_name[80];

				/* Note the collision */
				hit_body = TRUE;
	
				/* Get "the monster" or "it" */
				monster_desc(m_name, m_ptr, 0);

				/* Some monsters are great at dodging  -EZ- */
				if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (!m_ptr->csleep) &&
				    (one_in_(2)) && (r_ptr->d_char == 'G') && (!(f2 & (TR2_BLESSED))))
				{
					message_format(MSG_MISS, 0, "Your shot passes right through %^s!", m_name);
					break;
				}

				/* Did we hit it (penalize distance travelled) */
				if (test_hit_fire(chance2, r_ptr->ac, m_ptr->ml))
				{
					bool fear = FALSE;
	
					/* Assume a default death */
					cptr note_dies = " dies.";
	
					/* Some monsters get "destroyed" */
					if (monster_nonliving(r_ptr))
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
	
						/* Message */
						msg_format("The %s hits %s.", o_name, m_name);
	
						/* Hack -- Track this monster race */
						if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
	
						/* Hack -- Track this monster */
						if (m_ptr->ml) health_track(cave_m_idx[y][x]);
					}
					
					if ((anatomy) && (!(monster_nonliving(r_ptr))))
					{
						if (rand_int(320) < anatomy)
						{
							weightplus += 20 * (randint(anatomy));
							critplus += rand_range(150, 300);
							if (anatomy > 9) tmul += (anatomy / 10);
						}
					}				
	
					if ((sabotage) && (monster_automata(r_ptr)))
					{
						if (rand_int(160) < sabotage)
						{
							weightplus += 20 * (randint(sabotage));
							critplus += rand_range(150, 300);
							if (sabotage > 4) tmul += (sabotage / 5);
						}
					}				
					
	
					/* Boost the damage */
					tdam *= tmul;

					/* Apply special damages */
					tdam = totplus_dam_aux(i_ptr, tdam, m_ptr, MODE_SHOOTING);
					tdam = totminus_dam_aux(i_ptr, tdam, m_ptr, 0, MODE_SHOOTING);
					tdam = critical_shot(i_ptr->weight + weightplus, 
											i_ptr->to_h + critplus, tdam);
	
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
				else
				{
					msg_format("The %s misses %s.", o_name, (m_ptr->ml)? m_name: "something");
				}
				/* Stop looking */
				break;
			}
		}
	
		/* No chance of bullets surviving being fired */
	}
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
	int hafted, dagger, polearm, axe, critthrow;
	int advthrow, masterthrow, powerthrow, accthrow, tohitthrow, fastthrow;
	int haftedmaster, polearmmaster, daggermaster, axesmaster;
	int chance, tdam, tdis;
	int mul, div, bonus, noise, weight;

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
	
	u32b f1, f2, f3; 

	int msec = op_ptr->delay_factor * op_ptr->delay_factor * 2;

	hafted = dagger = polearm = axe = 0;
	advthrow = masterthrow = powerthrow = accthrow = tohitthrow = fastthrow = 0;
	haftedmaster = polearmmaster = daggermaster = axesmaster = 0;
	
 	if (p_ptr->skills[SK_HAFTED].skill_max > 0)
 		hafted = p_ptr->skills[SK_HAFTED].skill_rank;
	if (p_ptr->skills[SK_HAFTED_MASTER].skill_max > 0)
		haftedmaster = p_ptr->skills[SK_HAFTED_MASTER].skill_rank;
 	if (p_ptr->skills[SK_DAGGER].skill_max > 0)
 		dagger = p_ptr->skills[SK_DAGGER].skill_rank;
	if (p_ptr->skills[SK_DAGGER_MASTER].skill_max > 0)
		daggermaster = p_ptr->skills[SK_DAGGER_MASTER].skill_rank;
 	if (p_ptr->skills[SK_POLEARM].skill_max > 0)
 		polearm = p_ptr->skills[SK_POLEARM].skill_rank;
	if (p_ptr->skills[SK_POLEARM_MASTER].skill_max > 0)
		polearmmaster = p_ptr->skills[SK_POLEARM_MASTER].skill_rank;
 	if (p_ptr->skills[SK_AXES].skill_max > 0)
 		axe = p_ptr->skills[SK_AXES].skill_rank;
	if (p_ptr->skills[SK_AXES_MASTER].skill_max > 0)
		axesmaster = p_ptr->skills[SK_AXES_MASTER].skill_rank;
	if (p_ptr->skills[SK_TOHIT_THROWING].skill_max > 0)
		tohitthrow = p_ptr->skills[SK_TOHIT_THROWING].skill_rank;
	if (p_ptr->skills[SK_ADV_THROWING].skill_max > 0)
		advthrow = p_ptr->skills[SK_ADV_THROWING].skill_rank;
	if (p_ptr->skills[SK_MASTER_THROWING].skill_max > 0)
		masterthrow = p_ptr->skills[SK_MASTER_THROWING].skill_rank;
	if (p_ptr->skills[SK_POWER_THROW].skill_max > 0)
		powerthrow = p_ptr->skills[SK_POWER_THROW].skill_rank;
	if (p_ptr->skills[SK_CRIT_THROW].skill_max > 0)
		critthrow = p_ptr->skills[SK_CRIT_THROW].skill_rank;
	if (p_ptr->skills[SK_ACC_THROW].skill_max > 0)
		accthrow = p_ptr->skills[SK_ACC_THROW].skill_rank;
	if (p_ptr->skills[SK_FAST_THROW].skill_max > 0)
		fastthrow = p_ptr->skills[SK_FAST_THROW].skill_rank;

	/* Get the equipped weapon */
	j_ptr = &inventory[INVEN_WIELD];
	
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
	mul = 1;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (p_ptr->stat_use[A_MUS] / 4) * mul / div;
	if (advthrow)
	{
		tdis += advthrow / 5;
	}
	if (masterthrow > 0)
	{
		tdis += masterthrow / 3;
	}
	
	/* Throwing weapons get additional range */
	if (f2 & (TR2_THROW))
	{
		tdis += tdis;
	}

	/* Min distance of 1 */
	if (tdis < 1) tdis = 1;
	/* Max distance of 12 */
	if (tdis > 12) tdis = 12;

	/* Hack -- Base damage from thrown object */
	/* The power throw skill should make throwing weapons competative */
	if (powerthrow)
	{
		tdam = (damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d) * (powerthrow / 3 + 1);
	}
	
	
	else tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d;	
	
	if (f2 & (TR2_THROW))
	{
		/* Throwing weapons do additional throwing damage */
		/* Since this just mutiplies the base damage, this value is ok */
		tdam = tdam * 3;
	}
	else
	{
		/* Non throwing weapons do a fraction of their damage */
		tdam = tdam/2;
	}

	/* Weapons that aren't throwing weapons don't do a lot of damage */
	if (f2 & (TR2_PERFECT_BALANCE))
	{
		tdam = tdam * rand_range(3, 6);
	}
	
	/* Chance of hitting */
	bonus = p_ptr->to_h;
	
	/* Throwing weapons get their bonus to hit added in */
	if (f2 & (TR2_THROW))
	{
		if (f2 & (TR2_PERFECT_BALANCE)) bonus += i_ptr->to_h * rand_range(1, 4);
		else bonus += i_ptr->to_h;
	}
	
	/* Accuracy is added to the to hit bonuses, and ergo mutiplied by BTH_PLUS_ADJ */
	if (accthrow)
	{
		bonus += (accthrow * 4);
	}

	/* Base chance to hit */
	chance = ((tohitthrow * 9) + (bonus * BTH_PLUS_ADJ));

	/* Additional chance to hit based off of higher level skills */
	if (advthrow) chance += advthrow * 6;
	if (masterthrow) chance += masterthrow * 4;
	if (hafted && o_ptr->tval == TV_HAFTED) chance += hafted * 4;
	if (haftedmaster && o_ptr->tval == TV_HAFTED) chance += haftedmaster * 6;
	if (dagger && o_ptr->tval == TV_DAGGER) chance += dagger * 4;
	if (daggermaster && o_ptr->tval == TV_DAGGER) chance += daggermaster * 6;
	if (polearm && o_ptr->tval == TV_POLEARM) chance += polearm * 4;
	if (polearmmaster && o_ptr->tval == TV_POLEARM) chance += polearmmaster * 6;
	if (axe && o_ptr->tval == TV_AXES) chance += axe * 4;
	if (axesmaster && o_ptr->tval == TV_AXES) chance += axesmaster * 6;
	
	/* Take a turn - divide by number of blows */
	/* This is very wrong, if they are weilding a weapons that */
	/* gives extra blows, why should it affect their throwing */
	/* Trying the below */
	
	/* If barehanded, gain mutiple throws */
	if (!j_ptr->k_idx) p_ptr->energy_use = 100 / p_ptr->num_blow;
	
	/* Maybe throwing should not be affected if weilding a spellbook */

	/* If weilding a weapon, throwing is slow */
	else p_ptr->energy_use = 100;

	/* Sometimes get some energy back! */
	if (fastthrow)
	{
		p_ptr->energy_use -= fastthrow;
		
		/* Sometimes throw _really_ fast */
		if (rand_int(25) < fastthrow)
			p_ptr->energy_use -= (2 * randint(fastthrow));
			
		if (p_ptr->energy_use < 5) p_ptr->energy_use = 5;
	}

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
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, &ty, &tx, PROJECT_THRU);

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

			char m_name[80];

			/* Note the collision */
			hit_body = TRUE;

			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);

			/* Some monsters are great at dodging  -EZ- */
			if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (!m_ptr->csleep) &&
			    (one_in_(2)) && (r_ptr->d_char == 'G') && (!(f2 & (TR2_BLESSED))))
			{
				message_format(MSG_MISS, 0, "Your %s passes right through %s!", o_name, m_name);
				break;
			}

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

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Throwing weapons get their bonus to hit added in */
				if (f2 & (TR2_THROW))
				{
					if (f2 & (TR2_PERFECT_BALANCE)) weight = i_ptr->weight * rand_range(2, 12);
					else weight = i_ptr->weight * 2;
				}
				else weight = i_ptr->weight;
				
				if (critthrow > 1) weight *= (critthrow/2);

				/* as with melee hit, weapon masterty skill increases critical hit chance */
				/* not sure about the multiplier, for now a 5x improvement should suffice */
				if (haftedmaster && o_ptr->tval == TV_HAFTED) bonus += haftedmaster * 5;
				if (daggermaster && o_ptr->tval == TV_DAGGER) bonus += daggermaster * 5;
				if (polearmmaster && o_ptr->tval == TV_POLEARM) bonus += polearmmaster * 5;
				if (axesmaster && o_ptr->tval == TV_AXES) bonus += axesmaster * 5;

				/* Apply special damage XXX XXX XXX */
				tdam = totplus_dam_aux(i_ptr, tdam, m_ptr, MODE_THROWING);
				tdam = critical_throw(weight, bonus, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;
		
				/* Complex message */
				if (p_ptr->wizard) msg_format("You do %d (out of %d) damage.", tdam, m_ptr->hp);

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
		noise = p_ptr->base_wakeup_chance / 2;
	
		/* Burglary skill greatly reduces noise */
		noise += 201 - (p_ptr->skill_stl * 10);
	
		if (noise < 0) noise = 0;

		/* Increase the noise level */
		add_wakeup_chance += noise;

		msg_format("The %s returns to your hand.", o_name);
		inven_carry(i_ptr);
	}

	/* only items with the THROW flag are immune to destruction */
	else if (f2 & (TR2_THROW))
	{
		noise = p_ptr->base_wakeup_chance / 2;
	
		/* Burglary skill greatly reduces noise */
		noise += 201 - (p_ptr->skill_stl * 10);
	
		if (noise < 0) noise = 0;

		/* Increase the noise level */
		add_wakeup_chance += noise;

		/* debugging message */
		msg_format("The %s drops to the ground.", o_name);
		/* Drop near that location */
		drop_near(i_ptr, 0, y, x);
		/* nothing */
	}
	else 
	{
		noise = p_ptr->base_wakeup_chance / 2;
	
		/* Burglary skill greatly reduces noise */
		noise += 201 - (p_ptr->skill_stl * 5);
	
		if (noise < 0) noise = 0;
		/* Increase the noise level */
		add_wakeup_chance += noise;

		/* debugging message */
		msg_format("The %s smashes to the ground.", o_name);

		/* Chance of breakage (during attacks) */
		j = (hit_body ? breakage_chance(i_ptr) : 0);

		/* Drop (or break) near that location */
		drop_near(i_ptr, j, y, x);		
	}
}



