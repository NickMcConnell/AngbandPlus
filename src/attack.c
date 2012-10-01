/* File: attack.c */

/* All player non-magical attack code.  Hit chance, critical hits when 
 * shooting/throwing and in melee, calculate ego multiplier, druid blows, 
 * bashing, melee attacks.  Chance of object breakage, the shooting code, 
 * the throwing code.
 *
 * Copyright (c) 1999 Leon Marrick, Ben Harrison, James E. Wilson, 
 * Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"


/*
 * Determine if the player "hits" a monster (non-magical combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise use variable "chance" to
 * determine whether the blow lands.
 */
static bool test_hit_combat(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant hit.   Chance to miss removed in Oangband because
	 * of the way monster ACs now work (fewer truly easy targets).
	 */
	if (k < 5) return (TRUE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor. */
	if ((chance > 0) && (rand_int(chance) >= ac)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Calculation of critical hits for objects fired or thrown by the player. -LM-
 */
static sint critical_shot(int chance, int sleeping_bonus, bool thrown_weapon, 
	char o_name[], char m_name[], int visible)
{
	int i, k;
	int mult_a_crit = 10;

	/* Extract missile power.  */
	i = (chance + sleeping_bonus);

	/* Test for critical hit. */
	if (randint(i + 200) <= i)
	{
		/* Encourage the player to throw weapons at sleeping 
		 * monsters.
		 */
		if (sleeping_bonus)
		{ 
			if ((p_ptr->pclass == CLASS_ASSASSIN) && (thrown_weapon))
				message(MSG_HIT, 0, "Assassin Strike.");
			else message(MSG_HIT, 0, "You rudely awaken the monster.");
		}

		/* Determine level of critical hit */
		k = randint(i) + randint(100); 

		/* This portion of the function determines the level of critical hit,
		 * then adjusts the damage dice multiplier and displays an appropriate
		 * combat message.
		 * A distinction is made between visible and invisible monsters.
		 */
		if (k < 125)
		{

				if (!visible)
				{
				/* Invisible monster */
					message_format(MSG_HIT, 0, "The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					message_format(MSG_HIT, 0, "The %s strikes %s.", o_name, m_name);
				}
			mult_a_crit = 15;
		}
		else if (k < 215)
		{
				if (!visible)
				{
				/* Invisible monster */
					message_format(MSG_HIT, 0, "The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					message_format(MSG_HIT, 0, "The %s penetrates %s.", o_name, m_name);
				}
			mult_a_crit = 21;
		}
		else if (k < 275)
		{
				if (!visible)
				{
				/* Invisible monster */
					message_format(MSG_HIT, 0, "The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					message_format(MSG_HIT, 0, "The %s drives into %s.", o_name, m_name);
				}
			mult_a_crit = 28;
		}
		else
		{
				if (!visible)
				{
				/* Invisible monster */
					message_format(MSG_HIT, 0, "The %s finds a mark.", o_name);
				}
				else
				{
				/* Visible monster */
					message_format(MSG_HIT, 0, "The %s transpierces %s.", o_name, m_name);
				}
			mult_a_crit = 35;
		}
	}
	/* If the shot is not a critical hit, then the default message is shown. */
	else
	{
		mult_a_crit = 10; 
		message_format(MSG_HIT, 0, "The %s hits %s.", o_name, m_name);
	}

	return (mult_a_crit);
}



/*
 * Calculation of critical hits by the player in hand-to-hand combat. -LM-
 */
static sint critical_melee(int chance, int sleeping_bonus, char m_name[], object_type *o_ptr)
{
	int i, k;
	int mult_m_crit;

	/* Extract melee attack power.  */
	i = (chance + sleeping_bonus);

	/* Test for critical hit. */
	if (randint(i + 200) <= i)
	{
		/* Encourage the player to make sneak attacks on 
		 * sleeping monsters.
		 */
		if ((sleeping_bonus) && ((p_ptr->pclass == CLASS_ROGUE) || 
			(p_ptr->pclass == CLASS_ASSASSIN)))
			message(MSG_HIT, 0, "You ruthlessly sneak attack!");


		/* Hack - Weapons that normally do little damage benefit most from 
		 * critical hits (10x inflation).
		 */
		mult_m_crit = 120 / (o_ptr->dd * (o_ptr->ds + 1));
		if (mult_m_crit > 20) mult_m_crit = 20;
		if (mult_m_crit < 10) mult_m_crit = 10;


		/* Determine level of critical hit */
		k = randint(i) + randint(100); 

		/* This portion of the function determines the level of critical hit,
		 * the critical mult_m_crit, and displays an appropriate combat 
		 * message.  A distinction is often made between edged and blunt 
		 * weapons.  Unfortunately, whips sometimes display rather odd 
		 * messages... 
		 */
		if (k < 100)
		{
			mult_m_crit *= 15;
			message_format(MSG_HIT, 0, "You strike %s.", m_name);
		}
		else if (k < 160)
		{
			mult_m_crit *= 17;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				message_format(MSG_HIT, 0, "You hack at %s.", m_name);
			else
				message_format(MSG_HIT, 0, "You bash %s.", m_name);
		}
		else if (k < 210)
		{
			mult_m_crit *= 20;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				message_format(MSG_HIT, 0, "You slash %s.", m_name);
			else
				message_format(MSG_HIT, 0, "You pound %s.", m_name);
		}
		else if (k < 250)
		{
			mult_m_crit *= 23;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				message_format(MSG_HIT, 0, "You score %s.", m_name);
			else
				message_format(MSG_HIT, 0, "You batter %s.", m_name);
		}
		else if (k < 280)
		{
			mult_m_crit *= 27;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				message_format(MSG_HIT, 0, "You gouge %s.", m_name);
			else
			       message_format(MSG_HIT, 0, "You bludgeon %s.", m_name);
		}
		else 
		{
			mult_m_crit *= 32;
			message_format(MSG_HIT, 0, "You *smite* %s.", m_name);
		}


		/* Compensate for the weak weapon bonus by deflating the critical 
		 * hit multiplier.
		 */
		mult_m_crit /= 10;
	}

	/* If the blow is not a critical hit, display the default attack 
	 * message and apply the standard multiplier.
	 */
	else
	{
		mult_m_crit = 10; 
		message_format(MSG_HIT, 0, "You hit %s.", m_name);
	}

	return (mult_m_crit);
}



/*
 * Calculate the ego multiplier. 
 *
 * Note that flasks of oil do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Most slays are x2, except Slay Animal (x1.7), and Slay Evil (x1.5).  
 * Weapons of *slaying* now get a larger bonus. All brands are x1.7. -LM-
 *
 * Players may have temporary magic branding.  Paladins do not get to apply 
 * temporary brands to missiles.  A nasty hack, but necessary. -LM-
 */
static sint tot_dam_aux(object_type *o_ptr, monster_type *m_ptr, bool shooting)
{
	int mult_ego = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	/* Assume temporary elemental brand is OK to use. */
	bool allow_t_brand = TRUE;

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Hack -- paladins cannot take advantage of temporary elemental brands 
	 * to rescue their lousy shooting skill.
	 */
	if ((shooting) && (p_ptr->pclass == CLASS_PALADIN)) allow_t_brand = FALSE;


	/* Wielded weapons and diggers and fired missiles may do extra damage. */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Hack -- Special Paladin elemental blows don't effect archery */
			shooting = TRUE;

			/* Fall through. */
		}
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
					l_ptr->flags3 |= (RF3_ANIMAL);
				}

				if ((o_ptr->name2 == EGO_KILL_ANIMAL) && 
					(mult_ego < 20)) mult_ego = 20;

				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Slay Evil */
			if (((f1 & (TR1_SLAY_EVIL)) || 
				(p_ptr->special_attack & (ATTACK_HOLY))) &&
				(r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}

				if ((o_ptr->name2 == EGO_KILL_EVIL) && 
					(mult_ego < 17)) mult_ego = 17;

				else if (mult_ego < 15) mult_ego = 15;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_UNDEAD);
				}

				if ((o_ptr->name2 == EGO_KILL_UNDEAD) && 
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_DEMON);
				}

				if ((o_ptr->name2 == EGO_KILL_DEMON) && 
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_ORC);
				}

				if ((o_ptr->name2 == EGO_KILL_ORC) && 
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_TROLL);
				}

				if ((o_ptr->name2 == EGO_KILL_TROLL) && 
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_GIANT);
				}

				if ((o_ptr->name2 == EGO_KILL_GIANT) && 
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Slay Dragon */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_DRAGON);
				}

				if ((o_ptr->name2 == EGO_KILL_DRAGON) && 
					(mult_ego < 25)) mult_ego = 25;

				else if (mult_ego < 20) mult_ego = 20;
			}

			/* Brand (Acid) */
			if ((f1 & (TR1_BRAND_ACID)) || 
				((p_ptr->special_attack & (ATTACK_ACID)) && 
				(allow_t_brand)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_IM_ACID);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Brand (Elec) */
			if ((f1 & (TR1_BRAND_ELEC)) || 
				((p_ptr->special_attack & (ATTACK_ELEC)) && 
				(allow_t_brand)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ELEC))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Brand (Fire) */
			if ((f1 & (TR1_BRAND_FIRE)) || 
				((p_ptr->special_attack & (ATTACK_FIRE)) && 
				(allow_t_brand)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Otherwise, take extra damage */
				else 
				{
					if ((o_ptr->name2 == EGO_BALROG) && 
						(mult_ego < 30)) mult_ego = 30;
					else if (mult_ego < 17) mult_ego = 17;
				}
			}

			/* Brand (Cold) */
			if ((f1 & (TR1_BRAND_COLD)) || 
				((p_ptr->special_attack & (ATTACK_COLD)) && 
				(allow_t_brand)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_COLD))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_IM_COLD);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			/* Brand (Poison) */
			if ((f1 & (TR1_BRAND_POIS)) || 
				((p_ptr->special_attack & (ATTACK_POIS)) && 
				(allow_t_brand)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take extra damage */
				else if (mult_ego < 17) mult_ego = 17;
			}

			break;
		}
	}

	/* Hack - Sometimes, a temporary Holy Attack becomes exhusted. */
	if ((p_ptr->special_attack & (ATTACK_HOLY)) && (randint(20) == 1))
	{
		p_ptr->special_attack &= ~(ATTACK_HOLY);
		msg_print("Your temporary Holy attack has dissipated.");
	}


	/* Return the final damage multiplier. */
	return (mult_ego);
}

/* Calculate the damage done by a druid fighting barehanded, display an
 * appropriate message, and determine if the blow is capable of causing
 * monster confusion. -LM-
 */
static int get_druid_damage(int plev, char m_name[])
{
	cptr description;
	int dd = 0, ds = 0;
	int chance1, chance2, damage;
	int b_select = 0;

	/* Roll twice and keep the better of the two results, or the second
	 * if equal.
	 */
	chance1 = randint(2 * plev / 5);
	chance2 = randint(2 * plev / 5);
	if (chance1 > chance2) b_select = chance1;
	if (chance2 >= chance1) b_select = chance2;

	/* Just to make sure... */
	if (b_select  > NUM_D_BLOWS) b_select = NUM_D_BLOWS;

	/* Now, get the description and calculate the damage. */
	description = d_blow[b_select - 1].description;
	dd = d_blow[b_select - 1].dd;
	ds = d_blow[b_select - 1].ds;
	damage = damroll(dd, ds);


	/* Druids can also confuse monsters. */
	if (plev > rand_int(300)) 
	{
		/* Use the special druid confusion attack. */
		p_ptr->special_attack |= (ATTACK_DRUID_CONFU);

		/* And display the attack message. */
		message_format(MSG_HIT, 0, "You %s and attempt to confuse %s.", description, m_name);
	}
	else
	{
		/* Basic attack message. */		
		 message_format(MSG_HIT, 0, "You %s %s.", description, m_name);
	}
	return(damage);
}



/*
 * Player attacks a (poor, defenseless) creature in melee. -RAK-, -BEN-, -LM-
 *
 * Determine sleeping bonus, terrain modifiers, and try for a shield bash.
 * For each legal blow calculate base damage, then (if a weapon is wielded)
 * multiply it by the critical hit multiplier, the ego multiplier, and the 
 * Deadliness multiplier (from a table).  If a weapon is not wielded, Druids 
 * get various bare-handed attacks and everyone else gets two fist strikes.  
 * Check for various special attack effects.
 */
void py_attack(int y, int x)
{
	/* Number of dice, also total damage. */
	long k;

	/* The whole and fractional damage dice and their resulting damage. */
	int k_remainder, k_whole;

	/* blow count */
	int num = 0;

	/* hit count. */
	int hits = 0;

	/* Bonus to attack if monster is sleeping, for certain classes. */
	int sleeping_bonus = 0;

	/* Bonus to effective monster ac if it can take cover in terrain. */
	int terrain_bonus = 0;

	int bonus, chance, total_deadliness;

	int bash_chance, bash_quality, bash_dam;

	int blows;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	object_type *o_ptr;

	char m_name[80];

	bool fear = FALSE;

	bool do_quake = FALSE;


	/* Access the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Disturb the player */
	disturb(0, 0);

	/* Initial blows available. */
	blows = p_ptr->num_blow;

	/* If the monster is sleeping and visible, it can be hit more effectively 
	 * by some classes.
	 */
	if ((m_ptr->csleep) && (m_ptr->ml))
	{
		if (p_ptr->pclass == CLASS_ROGUE) 
			sleeping_bonus = 10 + 2 * p_ptr->lev / 5;
		else if (p_ptr->pclass == CLASS_ASSASSIN) 
			sleeping_bonus = 5 + p_ptr->lev / 5;
		else sleeping_bonus = 0;
	}


	/* Disturb the monster */
	m_ptr->csleep = 0;


	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);


	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Handle player fear */
	if (p_ptr->afraid)
	{
		if (m_ptr->ml)
		{
			/* Message */
			msg_format("You are too afraid to attack %s!", m_name);
		}
		else
		{
			/* Special Message. */
			msg_print("You sense something too frightful to combat!");
		}

		/* Done */
		return;
	}


	/* Monsters in rubble can take advantage of cover. */
	if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/* Monsters in trees can take advantage of cover, except 
	 * from players who know nature lore.
	 */
	if ((cave_feat[y][x] == FEAT_TREE) && 
		(mp_ptr->spell_book != TV_DRUID_BOOK))
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/* Monsters in water are vulnerable.  */
	if (cave_feat[y][x] == FEAT_WATER)
	{
		terrain_bonus -= r_ptr->ac / 5;
	}


	/**** The monster bashing code. -LM-  ****/

	/* No shield on arm, no bash.  */
	if ((!inventory[INVEN_ARM].k_idx) || (p_ptr->shield_on_back)) 
	{
		bash_chance = 0;
	}

	/*
	 * Players do not bash if they could otherwise take advantage of special 
	 * bonuses against sleeping monsters, or if the monster is low-level or 
	 * not visible.
	 */
	else if ((sleeping_bonus) || (r_ptr->level < p_ptr->lev / 2) || (!m_ptr->ml))
	{
		bash_chance = 0;
	}

	/* Bashing chance depends on melee Skill, Dex, and a class level bonus. */
	else bash_chance = p_ptr->skill_thn + 
		(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128 
		+ ((p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_PALADIN) ? 
		p_ptr->lev : 0);

	/* Some classes don't bash very often. */
	if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_MAGE) || 
		(p_ptr->pclass == CLASS_DRUID) || (p_ptr->pclass == CLASS_NECRO)) 
	{
		bash_chance /= 3;
	}

	/* Players bash more often when they see a real need. */
	if (bash_chance)
	{
		if ((!inventory[INVEN_WIELD].k_idx) && (p_ptr->pclass != CLASS_DRUID))
			bash_chance *= 3;
		else if ((inventory[INVEN_WIELD].dd * inventory[INVEN_WIELD].ds * blows) 
			< (inventory[INVEN_ARM].dd * inventory[INVEN_ARM].ds * 3))
			bash_chance *= 2;
	}

	/* Try to get in a shield bash. */
	if (bash_chance > rand_int(240 + r_ptr->level * 9))
	{
		message(MSG_HIT, 0, "You get in a shield bash!");

		/* Calculate attack quality, a mix of momentum and accuracy. */
		bash_quality = p_ptr->skill_thn + (p_ptr->wt / 8) + 
			(p_ptr->total_weight / 80) + (inventory[INVEN_ARM].weight / 3);

		/* Calculate damage.  Big shields are deadly. */
		bash_dam = damroll(inventory[INVEN_ARM].dd, inventory[INVEN_ARM].ds);

		/* Multiply by quality and experience factors */
		bash_dam *= bash_quality / 20 + p_ptr->lev / 7;

		/* Strength bonus. */
		bash_dam += (adj_str_td[p_ptr->stat_ind[A_STR]] - 128);

		/* Paranoia. */
		if (bash_dam > 125) bash_dam = 125;

		/* Encourage the player to keep wearing that heavy shield. */
		if (randint(bash_dam) > 30 + randint(bash_dam / 2)) 
		       message(MSG_HIT, 0, "WHAMM!");


		/* Damage, check for fear and death. */
		if (mon_take_hit(cave_m_idx[y][x], bash_dam, &fear, NULL))
		{
			/* Fight's over. */
			return;
		}

		/* Stunning. */
		if (bash_quality + p_ptr->lev > randint(200 + r_ptr->level * 4))
		{
		       message_format(MSG_HIT, 0, "%^s is stunned.", m_name);

			m_ptr->stunned += rand_int(p_ptr->lev / 5) + 4;
			if (m_ptr->stunned > 24) m_ptr->stunned = 24;
		}

		/* Confusion. */
		if (bash_quality + p_ptr->lev > randint(300 + r_ptr->level * 6) && 
			(!r_ptr->flags3 & (RF3_NO_CONF)))
		{
			message_format(MSG_HIT, 0, "%^s appears confused.", m_name);

			m_ptr->confused += rand_int(p_ptr->lev / 5) + 4;
		}

		/* The player will sometimes stumble. */
		if ((30 + adj_dex_th[p_ptr->stat_ind[A_DEX]] - 128) < randint(60))
			blows -= randint(blows);
	}

	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Initialize. */
	total_deadliness = p_ptr->to_d + o_ptr->to_d;

	/* Paranoia.  Ensure legal table access. */
	if (total_deadliness > 150) total_deadliness = 150;

	/* Calculate the "attack quality".  As BTH_PLUS_ADJ has been reduced
	 * to 1, base skill and modifiers to skill are given equal weight.
	 */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Attack once for each legal blow */
	while (num++ < blows)
	{
		/* Test for hit */
		if (test_hit_combat(chance + sleeping_bonus, r_ptr->ac + terrain_bonus, m_ptr->ml))
		{
			/* count hits. */
			hits++;

			/* Sound */
			sound(SOUND_HIT);

			/* If this is the first hit, make some noise. */
			if (hits == 1)
			{
				/* Hack -- Rogues and Assassins are silent melee 
				 * killers. */
				if ((p_ptr->pclass == CLASS_ROGUE) || 
					(p_ptr->pclass == CLASS_ASSASSIN)) 
				add_wakeup_chance = p_ptr->base_wakeup_chance / 4 + 500;

				/* Otherwise, make the standard amount of noise. */
				 else add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 2 + 1000;
			}


			/* If a weapon is wielded, the number of damage dice it rolls 
			 * is multiplied by the slay/brand multiplier, the critical hit 
			 * multiplier (both x10 of actual, for precision), and the 
			 * percentage bonus to damage corresponding to total Deadliness.  
			 * We then deflate and roll the whole number of damage dice, 
			 * roll the fractional die, and add the two damages together.
			 *
			 * This calculation and that for archery are the cornerstones of			 * the reformed combat system. -LM-
			 */
			if (o_ptr->k_idx)
			{
				/* base damage dice. */
				k = o_ptr->dd;

				/* multiply by slays or brands. (10x inflation) */
				k *= tot_dam_aux(o_ptr, m_ptr, FALSE);

				/* multiply by critical hit. (10x inflation) */
				k *= critical_melee(chance, sleeping_bonus, m_name, o_ptr);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					k *= (100 + deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					k *= (100 - 
					deadliness_conversion[ABS(total_deadliness)]);
				else
					k = 0;

				/* Get the whole number of dice by deflating the result. */
				k_whole = k / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				k_remainder = k % 10000;


				/* Calculate and combine the damages of the whole and 
				 * fractional dice.
				 */
				k = damroll(k_whole, o_ptr->ds) + 
					(k_remainder * damroll(1, o_ptr->ds) / 10000);


				/* hack -- check for earthquake. */
				if (p_ptr->impact && (k > 49)) do_quake = TRUE;
			}

			else
			{
				/* Hack - If no weapon is wielded, Druids are able to fight 
				 * effectively.   All other classes only get 1 damage, plus 
				 * whatever bonus their strength gives them. -LM-
				 */
				if (p_ptr->pclass == CLASS_DRUID)
				{
					k = get_druid_damage(p_ptr->lev, m_name);
				}
				else
				{
					k = 1 + ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
					message_format(MSG_HIT, 0, "You punch %s.", m_name);
				}
			}

			/* No negative damage */
			if (k < 0) k = 0;


			/* The verbose wizard message has been moved to mon_take_hit. */


			/* Damage, check for fear and death. */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))
			{
				/* Hack -- High-level warriors can spread their attacks out 
				 * among weaker foes. -LM-
				 */
				if ((p_ptr->pclass == CLASS_WARRIOR) && 
					(p_ptr->lev > 39) && (num < p_ptr->num_blow) && 
					(p_ptr->energy_use))
				{
					p_ptr->energy_use = p_ptr->energy_use * num / p_ptr->num_blow;
				}

				/* Fight's over. */
				break;
			}

			/* Confusion attack */
			if ((p_ptr->special_attack & (ATTACK_CONFUSE)) ||
				(p_ptr->special_attack & (ATTACK_DRUID_CONFU)))
			{
				/* Message */
				if (!( p_ptr->special_attack & (ATTACK_DRUID_CONFU))) 
				  message(MSG_HIT, 0, "Your hands stop glowing.");

				/* Cancel special confusion attack */
				p_ptr->special_attack &= ~(ATTACK_CONFUSE);
				p_ptr->special_attack &= ~(ATTACK_DRUID_CONFU);

				/* Confuse the monster */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_NO_CONF);
					}

					msg_format("%^s is unaffected.", m_name);
				}
				else if (rand_int(110) < r_ptr->level + randint(10))
				{
					msg_format("%^s is unaffected.", m_name);
				}
				else
				{
					message_format(MSG_HIT, 0, "%^s appears confused.", m_name);
					m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
				}
			}

			/* Black Breath attack. -LM- */
			if (p_ptr->special_attack & (ATTACK_BLKBRTH))
			{
				/* Cancel black breath */
				p_ptr->special_attack &= ~(ATTACK_BLKBRTH);

				/* Message */
				msg_print("Your hands stop radiating Night.");

				/* The undead are immune */
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (m_ptr->ml) /* control for visibility */
					{
						l_ptr->flags3 |= (RF3_UNDEAD);
					}

					msg_format("%^s is immune!", m_name);
				}
				/* All other monsters get a saving throw. */
				else if ((rand_int(160)) < (r_ptr->level + rand_int(60)))
				{
					msg_format("%^s wards off your deadly blow.", m_name);
				}
				/* Tasting some of their own medicine... */
				else
				{
					m_ptr->black_breath = TRUE;
					message_format(MSG_HIT, 0, "%^s is stricken with the Black Breath!", m_name);
				}
			}

			/* Rogue "Hit and Run" attack. -LM- */
			if (p_ptr->special_attack & (ATTACK_FLEE))
			{
				/* Cancel the fleeing spell */
				p_ptr->special_attack &= ~(ATTACK_FLEE);

				/* Message */
				msg_print("You escape into the shadows!");

				/* Teleport. */
				teleport_player(6 + p_ptr->lev / 5, TRUE);

				/* Fight's over. */
				return;
			}


		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			/* Message */
			message_format(MSG_MISS, 0, "You miss %s.", m_name);
		}
	}


	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx,
			       "%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand.   Radius reduced in Oangband. */
	if (do_quake)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

		earthquake(py, px, 4, FALSE);
	}
}



/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_SKELETON:
		{
			return (40);
		}
		/* Frequently break */
		case TV_ARROW:
		{
			return (30);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		{
			return (20);
		}
	}

	/* Rarely break */
	return (10);
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.  Project 
 * the missile along the path from player to target.
 * Take terrain and sleeping bonuses into account, make noise,	and check
 * for a hit.  If the missile hits, multiply the base damage die by the 
 * launcher, slay/brand, critical hit, and Deadliness multipliers in turn.
 * Apply any special attack or class bonuses, and check for death.
 * Drop the missile near the target (or end of path), sometimes breaking it.
 */
void do_cmd_fire(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, j, y, x, ty, tx;
	int tdis, thits, tmul;

	int armour, bonus, chance, total_deadliness;

	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	long tdam;
	int tdam_remainder, tdam_whole;

	/* Assume no weapon of velocity or accuracy bonus. */
	int special_dam = 0;
	int special_hit = 0;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[120];
	char m_name[80];

	int path_n = 0;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

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
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

	/* Access the item (if in the pack) */
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

	/* Missile launchers of Velocity and Accuracy sometimes "supercharge" */
	if ((j_ptr->name2 == EGO_VELOCITY) || (j_ptr->name2 == EGO_ACCURACY))
	{
		/* Occasional boost to shot. */
		if (randint(16) == 1)
		{
			if (j_ptr->name2 == EGO_VELOCITY) special_dam = TRUE;
			else if (j_ptr->name2 == EGO_ACCURACY) special_hit = TRUE;

			/* Describe the object */
			object_desc(o_name, j_ptr, FALSE, 0);

			/* Let player know that weapon is activated. */
			msg_format("You feel your %s tremble in your hand.", o_name);
		}
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* sum all the applicable additions to Deadliness. */
	total_deadliness = p_ptr->to_d + i_ptr->to_d + j_ptr->to_d;

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
	sound(SOUND_SHOOT);

	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Use a base distance */
	tdis = 10;

	/* Actually "fire" the object. */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Base range XXX XXX */
	tdis = 10 + 5 * tmul;

	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);


	/* Fire ammo of backbiting, and it will turn on you. -LM- */
	if (i_ptr->name2 == EGO_BACKBITING)
	{
		/* Message. */
		msg_print("Your missile turns in midair and strikes you!");

		/* Calculate damage. */
		tdam = damroll(p_ptr->ammo_mult * 4, i_ptr->ds);

		/* Inflict both normal and wound damage. */
		take_hit(tdam, "ammo of backbiting.");
		set_cut(randint(tdam * 3));

		/* That ends that shot! */
		return;
	}


	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);

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

			int visible = m_ptr->ml;

			int chance2 = chance - distance(py, px, y, x);

			/* Note the (attempted) collision */
			hit_body = TRUE;

			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);


			/* Sleeping, visible monsters are easier to hit. */
			if ((m_ptr->csleep) && (visible)) 
				sleeping_bonus = 5 + p_ptr->lev / 5;


			/* Monsters in rubble can take advantage of cover. */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in trees can take advantage of cover, except from 
			 * players who know nature lore.
			 */
			if ((cave_feat[y][x] == FEAT_TREE) && 
				(mp_ptr->spell_book != TV_DRUID_BOOK))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}

			/* Get effective armour class of monster. */
			armour = r_ptr->ac + terrain_bonus;

			/* Weapons of velocity sometimes almost negate monster armour. */
			if (special_hit) armour /= 3;

			/* Did we hit it (penalize distance travelled) */
			if (test_hit_combat(chance2 + sleeping_bonus, armour, m_ptr->ml))
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


				/* Make some noise.  Hack -- Assassins are silent 
				 * missile weapon killers.
				 */
				if (p_ptr->pclass == CLASS_ASSASSIN) add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 4 + 600;
				else add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 3 + 1200;
				

				/* Hack -- Track this monster race, if critter is visible */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack -- Track this monster, if visible */
				if (m_ptr->ml) health_track(cave_m_idx[y][x]);


				/* The basic damage-determination formula is the same in
				 * archery as it is in melee (apart from the launcher mul-
				 * tiplier).  See formula "py_attack" in "cmd1.c" for more 
				 * details. -LM-
				 */

				/* Base damage dice. */
				tdam = i_ptr->dd;

				/* Multiply by the missile weapon multiplier. */
				tdam *= tmul;


				/* multiply by slays or brands. (10x inflation) */
				tdam *= tot_dam_aux(i_ptr, m_ptr, TRUE);

				/* multiply by critical shot. (10x inflation) */
				tdam *= critical_shot(chance2, sleeping_bonus, FALSE, 
					o_name, m_name, visible);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					tdam *= (100 + 
					deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					tdam *= (100 - 
					deadliness_conversion[ABS(total_deadliness)]);
				else
					tdam = 0;

				/* Get the whole number of dice by deflating the result. */
				tdam_whole = tdam / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				tdam_remainder = tdam % 10000;

				/* Calculate and combine the damages of the whole and 
				 * fractional dice.
				 */
				tdam = damroll(tdam_whole, o_ptr->ds) + 
					(tdam_remainder * damroll(1, o_ptr->ds) / 10000);


				/* If an "enhance missile" spell has been cast, increase 
				 * the damage, and cancel the spell.
				 */
				if (p_ptr->special_attack & (ATTACK_SUPERSHOT))
				{
					tdam = 5 * tdam / 4 + 35;
					p_ptr->special_attack &= ~(ATTACK_SUPERSHOT);
				}

				/* If a weapon of velocity activates, increase damage. */
				if (special_dam)
				{
					tdam += 15;
				}

				/* Hack - Assassins are deadly... */
				if (p_ptr->pclass == CLASS_ASSASSIN)
				{
				/* Increase damage directly (to avoid excessive total
				 * damage by granting too high a Deadliness).
				 */
					if (p_ptr->ammo_tval == TV_SHOT)
					{

						tdam += p_ptr->lev / 2;
					}
					if (p_ptr->ammo_tval == TV_ARROW)
					{
						tdam += 2 * p_ptr->lev / 5;
					}
					if (p_ptr->ammo_tval == TV_BOLT)
					{
						tdam += p_ptr->lev / 3;
					}
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Hit the monster, check for death. */
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
						/* Sound */
						sound(SOUND_FLEE);

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
 * Throw an object from the pack or floor.  Now allows for throwing weapons.  
 * Unlike all other thrown objects, throwing weapons can take advantage of 
 * bonuses to Skill or Deadliness from other equipped items.
 *
 * Note: "unseen" monsters are very hard to hit.
 */
void do_cmd_throw(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdis;
	int mul, div;

	int total_deadliness;
	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	long tdam;
	int tdam_remainder, tdam_whole;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[120];
	char m_name[80];

	int path_n = 0;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	u32b f1, f2, f3;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

	/* Access the item (if in the pack) */
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

	/* Distribute the charges of rods/wands between the stacks */
	distribute_charges(o_ptr, i_ptr, 1);

	/* Extract the thrown object's flags. */
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
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;


	/* Chance of hitting.  Other thrown objects are easier to use, but 
	 * only throwing weapons take advantage of bonuses to Skill from 
	 * other items. -LM-
	 */
	if (f1 & (TR1_THROWING)) chance = ((p_ptr->skill_tht) + 
		((p_ptr->to_h + i_ptr->to_h) * BTH_PLUS_ADJ));
	else chance = ((3 * p_ptr->skill_tht / 2) + 
		(i_ptr->to_h * BTH_PLUS_ADJ));


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);


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

			int visible = m_ptr->ml;

			/* Calculate the projectile accuracy, modified by distance. */
			int chance2 = chance - distance(py, px, y, x);


			/* If the monster is sleeping, it'd better pray there are no 
			 * Assassins with throwing weapons nearby. -LM-
			 */
			if ((m_ptr->csleep) && (visible) && (f1 & (TR1_THROWING)))
			{
				if (p_ptr->pclass == CLASS_ASSASSIN) 
					sleeping_bonus = 15 + p_ptr->lev / 2;
				else sleeping_bonus = 0;
			}

			/* Monsters in rubble can take advantage of cover. */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in trees can take advantage of cover, except from 
			 * players who know nature lore.
			 */
			if ((cave_feat[y][x] == FEAT_TREE) && 
				(mp_ptr->spell_book != TV_DRUID_BOOK))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}


			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);


			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_combat(chance2 + sleeping_bonus, 
				r_ptr->ac + terrain_bonus, m_ptr->ml))
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


				/* Make some noise.  Hack -- Assassins are silent 
				 * missile weapon killers.
				 */
				if (p_ptr->pclass == CLASS_ASSASSIN) add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 4 + 300;
				else add_wakeup_chance = 
					p_ptr->base_wakeup_chance / 3 + 1200;


				/* Hack -- Track this monster race, if critter is visible */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack -- Track this monster, if visible */
				if (m_ptr->ml) health_track(cave_m_idx[y][x]);


				/* sum all the applicable additions to Deadliness. */
				total_deadliness = p_ptr->to_d + i_ptr->to_d;


				/* The basic damage-determination formula is the same in
				 * throwing as it is in melee (apart from the thrown weapon 
				 * multiplier, and the ignoring of non-object bonuses to
				 * Deadliness for objects that are not thrown weapons).   See 
				 * formula "py_attack" in "cmd1.c" for more details. -LM-
				 */

				tdam = i_ptr->dd;

				/* Multiply the number of damage dice by the throwing weapon 
				 * multiplier, if applicable.  This is not the prettiest 
				 * equation, but it does at least try to keep throwing 
				 * weapons competitive.
				 */
				if (f1 & (TR1_THROWING))
				{
					tdam *= 2 + p_ptr->lev / 12;

					/* Perfectly balanced weapons do even more damage. */
					if (f1 & (TR1_PERFECT_BALANCE)) tdam *= 2;
				}


				/* multiply by slays or brands. (10x inflation) */
				tdam *= tot_dam_aux(i_ptr, m_ptr, TRUE);

				/* Only allow critical hits if the object is a throwing 
				 * weapon.  Otherwise, grant the default multiplier.
				 * (10x inflation)
				 */
				if (f1 & (TR1_THROWING)) tdam *= critical_shot
					(chance2, sleeping_bonus, TRUE, o_name, m_name, visible);
				else tdam *= 10;

				/* Convert total or object-only Deadliness into a percen-
				 * tage, and apply it as a bonus or penalty (100x inflation)
				 */
				if (f1 & (TR1_THROWING))
				{
					if (total_deadliness > 0)
						tdam *= (100 + 
						deadliness_conversion[total_deadliness]);
					else if (total_deadliness > -31)
						tdam *= (100 - 
						deadliness_conversion[ABS(total_deadliness)]);
					else
						tdam = 0;
				}

				else 
				{
					if (i_ptr->to_d > 0)
						tdam *= (100 + 
						deadliness_conversion[i_ptr->to_d]);
					else if (i_ptr->to_d > -31)
						tdam *= (100 - 
						deadliness_conversion[ABS(i_ptr->to_d)]);
					else
						tdam = 0;
				}

				/* Get the whole number of dice by deflating the result. */
				tdam_whole = tdam / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				tdam_remainder = tdam % 10000;


				/* Calculate and combine the damages of the whole and 
				 * fractional dice.
				 */
				tdam = damroll(tdam_whole, i_ptr->ds) + 
					(tdam_remainder * damroll(1, i_ptr->ds) / 10000);


				/* No negative damage */
				if (tdam < 0) tdam = 0;

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
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
						msg_format("%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks).   Throwing weapons are designed 
	 * not to break.  -LM-
	 */
	if (f1 & (TR1_PERFECT_BALANCE)) j = 0;
	else if (f1 & (TR1_THROWING)) j = (hit_body ? 2 : 0);
	else j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}
