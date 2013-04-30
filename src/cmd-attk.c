/* File: cmd-attk.c */

/*
 * Commands and routines for moving and attacking
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* 
 * hack - defines for weapons with "special" attacks
 */
#define SPEC_CUT		0x01
#define SPEC_POIS		0x02
#define SPEC_FEAR		0x04
#define SPEC_BLIND		0x08
#define SPEC_STUN		0x10
#define SPEC_CONF		0x20

/*
 * Determine if the player "hits" a monster 
 *
 */
static int test_hit(int skill, int ac, int vis)
{
	int chance;
	int critical_hit_chance = 0;

	/* Increment skill (diminishing returns) */
	if (skill <= 10) skill *= 9;
	else if (skill <= 20) skill = 9 + (skill * 8);
	else if (skill <= 30) skill = 27 + (skill * 7);
	else skill = 54 + (skill * 6);

	/* High ACs are harder to hit */
	if (ac > (skill + 1) / 2) ac += ac / 2;

	/* Invisible monsters are harder to hit */
	if (!vis) ac += ac / 2;

	/* Calculate to-hit percentage */
	if (skill <= 0) chance = 0;
	else chance = 101 - ((ac * 100)/skill);
	if (chance > 100) chance = 100;

	/* Calculate the minimum chance */ 
	if ((p_ptr->command_cmd == 'v') && (chance < 10))
	{
		chance = 10;
	}
	else if ((p_ptr->command_cmd == 'f') && (chance < 10))
	{
		chance = 10;
	}
	else if (chance < 40)
	{
		if (chance < 1) chance = 25;
		else if (chance < 40) chance = 40;
	}

	/* Calculate critical hit chance */
	if (chance > 90)
	{
		critical_hit_chance = (chance - 90) * (chance - 90);
	}

	/* Make the percentage roll. Return negative value in the attack misses. */
	if (!(chance > rand_int(100))) critical_hit_chance = -1;

	return critical_hit_chance;
}

/*
 * Critical hits (from objects fired by player)
 * Factor in item weight, total plusses, and player level.
 */
static int critical_shot(const object_type *o_ptr, byte *special, int dam, bool ambush, int critical_hit_chance, bool deadly_crit)
{
	int i, k;
	object_type *j_ptr;

	if (ambush)
	{
		/* Get the "bow" (if any) */
		j_ptr = &inventory[INVEN_BOW];

		/* Extract "shot" power */
		i = ((p_ptr->to_h_shooting + object_to_h(o_ptr) + object_to_h(j_ptr)) * 2) +10;

		/* Improved ambush chance for some classes */
		if (cp_ptr->flags & CF_AMBUSH) i += p_ptr->lev +10;

		/* Improved ambush chance from item bonuses */
		i += p_ptr->ambush_bonus * 2;
	}
	else i = 0;

	i += (critical_hit_chance * 2);

	/* Critical hit */
	if (rand_int(200) < i)
	{
		k = p_ptr->lev + randint(500);

		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_SHOT) k *= 2;
		else if (cp_ptr->flags & CF_AMBUSH) k = (k*3)/2;

		if (k < 500)
		{
			message(MSG_CRITICAL_HIT, 0, "It was a good hit!");
			dam = 2 * dam;
		}
		else if (k < 900)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a great hit!");
			dam = 2 * dam + 5;
		}
		else
		{
			message(MSG_CRITICAL_HIT, 0,"It was a superb hit!");
			dam = ((5 * dam) / 2) + 10;
		}

		if (deadly_crit) dam = dam * 3;
	
		(*special) |= SPEC_CUT;
	}

	return (dam);
}

/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
static int critical_throw(const object_type *o_ptr, int dam, bool ambush, int critical_hit_chance, bool deadly_crit)
{
	int i, k;

	if (ambush)
	{
		/* Extract "shot" power */
		i = ((p_ptr->to_h_throwing + object_to_h(o_ptr)) * 2) +10;

		/* Improved ambush chance for some classes */
		if (cp_ptr->flags & CF_AMBUSH) i += p_ptr->lev +10;

		/* Improved ambush chance from item bonuses */
		i += p_ptr->ambush_bonus * 2;
	}
	else i = 0;

	i += critical_hit_chance * 2;

	/* Critical hit */
	if (rand_int(200) < i)
	{
		k = p_ptr->lev + randint(500);
		
		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_THROW) k *= 2;
		else if (cp_ptr->flags & CF_AMBUSH) k = (k*3)/2;

		if (k < 400)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a good hit!");
			dam = 2 * dam;
		}
		else if (k < 850)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a great hit!");
			dam = ((5 * dam) / 2) + 5;
		}
		else
		{
			message(MSG_CRITICAL_HIT, 0,"It was a superb hit!");
			dam = 3 * dam + 10;
		}

		if (deadly_crit) dam = 3 * dam;
	}

	return (dam);
}

/*
 * Critical hits (from powder vials thrown by player)
 */
static int critical_powder(bool ambush, int critical_hit_chance)
{
	int i;

	if (ambush)
	{
		/* Extract "shot" power */
		i = (p_ptr->to_h_throwing * 2) +10;

		/* Improved ambush chance for some classes */
		if (cp_ptr->flags & CF_AMBUSH) i += p_ptr->lev +10;

		/* Improved ambush chance from item bonuses */
		i += p_ptr->ambush_bonus * 2;
	}
	else i = 0;

	i += critical_hit_chance * 2;

	/* Critical hit */
	if (rand_int(200) < i)
	{
		if ((cp_ptr->flags & CF_BETTER_THROW) && (rand_int(5) < 2))
		{
			message(MSG_CRITICAL_HIT, 0,"It was a great hit!");
			return 2;
		}
		else
		{
			message(MSG_CRITICAL_HIT, 0,"It was a good hit!");
			return 1;
		}
	}

	return 0;
}

/*
 * Critical hits (unarmed)
 */
static int critical_unarmed(int dam, byte *special, bool ambush, int critical_hit_chance)
{
	int i, k;

	if (ambush)
	{
		/* Extract "blow" power */
		i = (p_ptr->to_h_melee * 2) +10;

		/* Improved ambush chance for some classes */
		if (cp_ptr->flags & CF_AMBUSH) i += p_ptr->lev +10;

		/* Improved ambush chance from item bonuses */
		i += p_ptr->ambush_bonus;
	}
	else i = 0;

	i += critical_hit_chance * 2;

	/* Chance */
	if (rand_int(200) < i)
	{
		k = (p_ptr->to_h_melee * 4) + (p_ptr->lev * 2) + randint(600);

		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_CRITICAL) k *= 2;
		else if (cp_ptr->flags & CF_AMBUSH) k = (k*3)/2;

		if (k < 400)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a good hit!");
			if (p_ptr->shape == SHAPE_HARPY) dam = ((6 * dam) / 5) + 5;
		}
		else if (k < 700)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a great hit!");
			if (p_ptr->shape == SHAPE_HARPY) dam = ((3 * dam) / 2) + 5;
			else dam = dam + 5;
		}
		else if (k < 900)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a superb hit!");
			if (p_ptr->shape == SHAPE_HARPY) dam = ((8 * dam) / 5) + 8;
			else dam = ((3 * dam) / 2) + 8;
		}
		else if (k < 1300)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a *GREAT* hit!");
			if (p_ptr->shape == SHAPE_HARPY) dam = 2 * dam + 13;
			else
			{
				dam = ((8 * dam) / 5) + 13;
				if (rand_int(100) < 10) (*special) |= SPEC_CONF;
			}
		}
		else
		{
			message(MSG_CRITICAL_HIT, 0,"It was a *SUPERB* hit!");
			if (p_ptr->shape == SHAPE_HARPY) dam = ((12 * dam) / 5) + 16;
			else
			{
				dam = 2 * dam + 16;
				if (rand_int(100) < 25) (*special) |= SPEC_CONF;
			}
		}

		if (p_ptr->shape == SHAPE_HARPY) (*special) |= SPEC_CUT;
		else (*special) |= SPEC_STUN;
	}

	return (dam);
}

/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
static int critical_norm(const object_type *o_ptr, byte *special, int dam, bool ambush, int critical_hit_chance, bool deadly_crit)
{
	int i, k;

	/* No critical hit with "bad" weapons */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (p_ptr->icky_wield)) return (dam);

	if (ambush)
	{
		/* Extract "blow" power */
		i = ((p_ptr->to_h_melee + object_to_h(o_ptr)) * 2) +10;

		/* Improved ambush chance for some classes */
		if (cp_ptr->flags & CF_AMBUSH) i += p_ptr->lev +10;

		/* Improved ambush chance from item bonuses */
		i += p_ptr->ambush_bonus;
	}
	else i = 0;

	i += critical_hit_chance * 2;

	/* Chance */
	if (rand_int(200) < i)
	{
		k = ((p_ptr->to_h_melee + object_to_h(o_ptr)) * 4) + (p_ptr->lev * 2) + randint(600);

		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_CRITICAL) k *= 2;
		else if (cp_ptr->flags & CF_AMBUSH) k = (k*3)/2;

		if (k < 400)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a good hit!");
			if (o_ptr->tval == TV_POLEARM) dam = 2 * dam + 5;
			else if (o_ptr->tval == TV_SWORD) dam = ((6 * dam) / 5) + 5;
		}
		else if (k < 700)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a great hit!");
			if (o_ptr->tval == TV_POLEARM) dam = ((11 * dam) / 5) + 10;
			else if (o_ptr->tval == TV_SWORD) dam = ((3 * dam) / 2) + 5;
			else dam = dam + 5;
		}
		else if (k < 900)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a superb hit!");
			if (o_ptr->tval == TV_POLEARM) dam = 3 * dam + 15;
			else if (o_ptr->tval == TV_SWORD) dam = ((8 * dam) / 5) + 8;
			else dam = ((3 * dam) / 2) + 8;
		}
		else if (k < 1300)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a *GREAT* hit!");
			if (o_ptr->tval == TV_POLEARM) dam = ((16 * dam) / 5) + 20;
			else if (o_ptr->tval == TV_SWORD) dam = 2 * dam + 13;
			else dam = ((8 * dam) / 5) + 13;

			if ((o_ptr->tval == TV_BLUNT) && (rand_int(100) < 10)) (*special) |= SPEC_CONF;
		}
		else
		{
			message(MSG_CRITICAL_HIT, 0,"It was a *SUPERB* hit!");
			if (o_ptr->tval == TV_POLEARM) dam = ((7 * dam) / 2) + 25;
			if (o_ptr->tval == TV_SWORD) dam = ((12 * dam) / 5) + 16;
			else dam = 2 * dam + 16;

			if ((o_ptr->tval == TV_BLUNT) && (rand_int(100) < 25)) (*special) |= SPEC_CONF;
		}

		if (deadly_crit) dam = 3 * dam;
	
		if (o_ptr->tval == TV_SWORD) (*special) |= SPEC_CUT;
		if (o_ptr->tval == TV_BLUNT) (*special) |= SPEC_STUN;
	}

	return (dam);
}

/*
 * Apply the special effects of an attack
 */
static void attack_special(int m_idx, byte special, int dam)
{
	char m_name[80];
	
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);
	int tmp;

	/* Extract monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Special - Cut monster */
	if (special & SPEC_CUT)
	{
		/* Cut the monster */
		if (r_ptr->flags3 & (RF3_NO_CUT)) lore_learn(m_ptr, LRN_FLAG3, RF3_NO_CUT, FALSE);
		else if (rand_int(100) >= r_ptr->level)
		{
			/* Already partially poisoned */
			if (m_ptr->bleeding) message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s is bleeding more strongly.", m_name);
			/* Was not poisoned */
			else message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s is bleeding.", m_name);

			tmp = m_ptr->bleeding + dam * 2;

			m_ptr->bleeding = (tmp < 10000) ? tmp : 10000;
		}
	}

	/* Special - Poison monster */
	if (special & SPEC_POIS) 
	{
		/* Poison the monster */
		if (r_ptr->flags3 & (RF3_RES_POIS)) lore_learn(m_ptr, LRN_FLAG3, RF3_RES_POIS, FALSE);
		else if (rand_int(100) >= r_ptr->level)
		{
			/* Already partially poisoned */
			if (m_ptr->poisoned) message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s is more poisoned.", m_name);
			/* Was not poisoned */
			else message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s is poisoned.", m_name);

			tmp = m_ptr->poisoned + dam;

			m_ptr->poisoned = (tmp < 10000) ? tmp : 10000;
		}
	}

	/* Special - Blind monster */
	if (special & SPEC_BLIND) 
	{
		/* Blind the monster */
		if (r_ptr->flags3 & (RF3_NO_BLIND)) lore_learn(m_ptr, LRN_FLAG3, RF3_NO_BLIND, FALSE);
		else if (rand_int(100) >= r_ptr->level)
		{
			/* Already partially blinded */
			if (m_ptr->blinded) message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears more blinded.", m_name);
			/* Was not blinded */
			else message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears blinded.", m_name);

			tmp = m_ptr->blinded + 1 + dam/3 + rand_int(dam)/3;

			m_ptr->blinded = (tmp < 200) ? tmp : 200;

		}
	}

	/* Special - Stun monster */
	if (special & SPEC_STUN)
	{
		/* Stun the monster */
		if (r_ptr->flags3 & (RF3_NO_STUN)) lore_learn(m_ptr, LRN_FLAG3, RF3_NO_STUN, FALSE);
		else if (rand_int(100) >= r_ptr->level)
		{
			/* Already partially stunned */
			if (m_ptr->stunned) message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears more dazed.", m_name);
			/* Was not stunned */
			else message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears dazed.", m_name);

			tmp = m_ptr->stunned + 10 + rand_int(dam) / 5;

			m_ptr->stunned = (tmp < 200) ? tmp : 200;
		}
	}

	/* Special - Confuse monster */
	if (special & SPEC_CONF) 
	{
		/* Confuse the monster */
		if (r_ptr->flags3 & (RF3_NO_CONF)) lore_learn(m_ptr, LRN_FLAG3, RF3_NO_CONF, FALSE);
		else if (rand_int(100) >= r_ptr->level)
		{
			/* Already partially confused */
			if (m_ptr->confused) message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears more confused.", m_name);
			/* Was not stunned */
			else message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears confused.", m_name);

			tmp = m_ptr->confused + 10 + rand_int(dam) / 5;

			m_ptr->confused = (tmp < 200) ? tmp : 200;
		}
	}
}

/*
 * Extract the "total damage" from a given object hitting a given monster.
 */
static int tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr, byte *special)
{
	int mult = 10;

	monster_race *r_ptr = get_monster_real(m_ptr);

	u32b f1, f2, f3;
	byte slays[SL_MAX];	

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	switch (o_ptr->tval)
	{
		/* Flasks do fire damage */
		case TV_FLASK:
		{
			/* Notice immunity */
			if (r_ptr->flags3 & (RF3_RES_FIRE))
			{
				lore_learn(m_ptr, LRN_FLAG3, RF3_RES_FIRE, FALSE);
				mult = 0;
			}
			else if (r_ptr->flags2 & (RF2_HURT_FIRE))
			{
				mult = 15;
				lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_FIRE, FALSE);
			}

			break;
		}

		/* Some "weapons" and "ammo" do extra damage */
		case TV_ARROW:
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Extract slays */
			weapon_slays(o_ptr, slays);

			/* Add slays from bow in case of arrows */
			if (o_ptr->tval == TV_ARROW)
			{
				int i;
				byte slays_bow[SL_MAX];
				object_type *j_ptr = &inventory[INVEN_BOW];

				weapon_slays(j_ptr, slays_bow);

				for (i = 0; i < SL_MAX; i++) 
				{
					if (slays_bow[i] > 10)
					{
						if (slays[i] > 10) slays[i] -= 10;
						slays[i] += slays_bow[i];
					}
				}
			}

			/* Slay Animal */
			if ((slays[SL_ANTI_ANIMAL] > 10) && (r_ptr->flags4 & (RF4_ANIMAL)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_ANIMAL, FALSE);
				if (mult < slays[SL_ANTI_ANIMAL]) mult = slays[SL_ANTI_ANIMAL];
			}

			/* Slay Plant */
			if ((slays[SL_ANTI_PLANT] > 10) && (r_ptr->flags4 & (RF4_PLANT)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_PLANT, FALSE);
				if (mult < slays[SL_ANTI_PLANT]) mult = slays[SL_ANTI_PLANT];
			}

			/* Slay Evil */
			if ((slays[SL_ANTI_EVIL] > 10) && (r_ptr->flags4 & (RF4_EVIL)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_EVIL, FALSE);
				if (mult < slays[SL_ANTI_EVIL]) mult = slays[SL_ANTI_EVIL];
			}

			/* Slay Mythos */
			if ((slays[SL_ANTI_AETHER] > 10) && (r_ptr->flags4 & (RF4_AETHER)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_AETHER, FALSE);
				if (mult < slays[SL_ANTI_AETHER]) mult = slays[SL_ANTI_AETHER];
			}

			/* Slay Skultgard */
			if ((slays[SL_ANTI_SKULTGARD] > 10) && (r_ptr->flags4 & (RF4_SKULTGARD)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_SKULTGARD, FALSE);
				if (mult < slays[SL_ANTI_SKULTGARD]) mult = slays[SL_ANTI_SKULTGARD];
			}

			/* Slay Thornwild */
			if ((slays[SL_ANTI_THORNWILD] > 10) && (r_ptr->flags4 & (RF4_THORNWILD)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_THORNWILD, FALSE);
				if (mult < slays[SL_ANTI_THORNWILD]) mult = slays[SL_ANTI_THORNWILD];
			}

			/* Slay Chaos */
			if ((slays[SL_ANTI_CHAOS] > 10) && (r_ptr->flags4 & (RF4_CHAOS)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_CHAOS, FALSE);
				if (mult < slays[SL_ANTI_CHAOS]) mult = slays[SL_ANTI_CHAOS];
			}

			/* Slay Undead */
			if ((slays[SL_ANTI_UNDEAD] > 10) && (r_ptr->flags4 & (RF4_UNDEAD)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_UNDEAD, FALSE);
				if (mult < slays[SL_ANTI_UNDEAD]) mult = slays[SL_ANTI_UNDEAD];
			}

			/* Slay Demon */
			if ((slays[SL_ANTI_DEMON] > 10) && (r_ptr->flags4 & (RF4_DEMON)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_DEMON, FALSE);
				if (mult < slays[SL_ANTI_DEMON]) mult = slays[SL_ANTI_DEMON];
			}

			/* Slay Humanoid */
			if ((slays[SL_ANTI_HUMANOID] > 10) && (r_ptr->flags4 & (RF4_HUMANOID)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_HUMANOID, FALSE);
				if (mult < slays[SL_ANTI_HUMANOID]) mult = slays[SL_ANTI_HUMANOID];
			}

			/* Slay Person */
			if ((slays[SL_ANTI_PERSON] > 10) && (r_ptr->flags4 & (RF4_PERSON)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_PERSON, FALSE);
				if (mult < slays[SL_ANTI_PERSON]) mult = slays[SL_ANTI_PERSON];
			}

			/* Slay Faery */
			if ((slays[SL_ANTI_FAERY] > 10) && (r_ptr->flags4 & (RF4_FAERY)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_FAERY, FALSE);
				if (mult < slays[SL_ANTI_FAERY]) mult = slays[SL_ANTI_FAERY];
			}

			/* Slay Dragon */
			if ((slays[SL_ANTI_DRAGON] > 10) && (r_ptr->flags4 & (RF4_DRAGON)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_DRAGON, FALSE);
				if (mult < slays[SL_ANTI_DRAGON]) mult = slays[SL_ANTI_DRAGON];
			}

			/* Slay Lycanthrope */
			if ((slays[SL_ANTI_LYCANTHROPE] > 10) && (r_ptr->flags4 & (RF4_LYCANTHROPE)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_LYCANTHROPE, FALSE);
				if (mult < slays[SL_ANTI_LYCANTHROPE]) mult = slays[SL_ANTI_LYCANTHROPE];
			}

			/* Brand (Acid) */
			if (slays[SL_BRAND_ACID] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_ACID)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_ACID, FALSE);

				/* Notice vulnerabilitiy */
				else if (r_ptr->flags2 & (RF2_HURT_ACID)) 
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_ACID, FALSE);
					if (mult < (3 * slays[SL_BRAND_ACID]) / 2)
						mult = (3 * slays[SL_BRAND_ACID]) / 2;
				}

				/* Otherwise, take the damage */
				else if (mult < slays[SL_BRAND_ACID]) mult = slays[SL_BRAND_ACID];
			}

			/* Brand (Elec) */
			if (slays[SL_BRAND_ELEC] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_ELEC)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_ELEC, FALSE);

				/* Notice vulnerabilitiy */
				else if (r_ptr->flags2 & (RF2_HURT_ELEC)) 
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_ELEC, FALSE);
					if (mult < (3 * slays[SL_BRAND_ELEC]) / 2) 
						mult = (3 * slays[SL_BRAND_ELEC]) / 2;
				}

				/* Otherwise, take the damage */
				else if (mult < slays[SL_BRAND_ELEC]) mult = slays[SL_BRAND_ELEC];
			}

			/* Brand (Fire) */
			if (slays[SL_BRAND_FIRE] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_FIRE)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_FIRE, FALSE);

				/* Notice vulnerabilitiy */
				else if (r_ptr->flags2 & (RF2_HURT_FIRE)) 
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_FIRE, FALSE);
					if (mult < (3 * slays[SL_BRAND_FIRE]) / 2)
						mult = (3 * slays[SL_BRAND_FIRE]) / 2;
				}

				/* Otherwise, take the damage */
				else if (mult < slays[SL_BRAND_FIRE]) mult = slays[SL_BRAND_FIRE];
			}

			/* Brand (Cold) */
			if (slays[SL_BRAND_COLD] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_COLD)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_COLD, FALSE);

				/* Notice vulnerabilitiy */
				else if (r_ptr->flags2 & (RF2_HURT_COLD)) 
				{
					lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_COLD, FALSE);
					if (mult < (3 * slays[SL_BRAND_COLD]) / 2) 
						mult = (3 * slays[SL_BRAND_COLD]) / 2;
				}

				/* Otherwise, take the damage */
				else if (mult < slays[SL_BRAND_COLD]) mult = slays[SL_BRAND_COLD];
			}

			/* Brand (Venom) */
			if (slays[SL_BRAND_POIS] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_POIS)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_POIS, FALSE);

				/* Otherwise, take the damage + poison the creature */
				else
				{
					if (mult < slays[SL_BRAND_POIS]) mult = slays[SL_BRAND_POIS];
					if (rand_int(100) < 50) (*special) |= SPEC_POIS;
				}
			}

			/* Brand (light) */
			if (slays[SL_BRAND_LITE] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_LITE)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_LITE, FALSE);

				/* Otherwise, take the damage */
				else if (r_ptr->flags2 & (RF2_HURT_LITE))
				{
					if (mult < slays[SL_BRAND_LITE] * 2) mult = slays[SL_BRAND_LITE] * 2;
					if (rand_int(100) < 50) (*special) |= SPEC_BLIND;

					lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_LITE, FALSE);
				}
				else 
				{
					if (mult < slays[SL_BRAND_LITE]) mult = slays[SL_BRAND_LITE];
					if (rand_int(100) < 20) (*special) |= SPEC_BLIND;
				}
			}

			/* Brand (dark) */
			if (slays[SL_BRAND_DARK] > 10)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_RES_DARK)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_RES_DARK, FALSE);

				/* Otherwise, take the damage */
				else if (r_ptr->flags2 & (RF2_HURT_DARK))
				{
					if (mult < slays[SL_BRAND_DARK] * 2) mult = slays[SL_BRAND_DARK] * 2;
					if (rand_int(100) < 50) (*special) |= SPEC_BLIND;

					lore_learn(m_ptr, LRN_FLAG2, RF2_HURT_DARK, FALSE);
				}
				else 
				{
					if (mult < slays[SL_BRAND_DARK]) mult = slays[SL_BRAND_DARK];
					if (rand_int(100) < 20) (*special) |= SPEC_BLIND;
				}
			}
			
			/* Wounding */
			if (f2 & (TR2_WOUNDING))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_NO_CUT))
					lore_learn(m_ptr, LRN_FLAG3, RF3_NO_CUT, FALSE);
				else if (rand_int(100) < 50) (*special) |= SPEC_CUT;
			}

			/* Terror */
			if (f2 & (TR2_TERROR))
			{
				if (r_ptr->flags3 & (RF3_NO_FEAR)) 
					lore_learn(m_ptr, LRN_FLAG3, RF3_NO_FEAR, FALSE);
				else if (rand_int(100) < 10) (*special) |= SPEC_FEAR;
			}

			break;
		}
	}

	/* Return the total damage (rounded up) */
	return ((tdam * mult) + 9) / 10;
}


/* Create item from a bookshelf, rack, or closet */
void create_shelf_item(int y, int x, int theme)
{
	bool object_created = FALSE;

	object_type *i_ptr;
	object_type object_type_body;

	byte tval = 0;

	while (object_created == FALSE)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Get the actual item from the theme */
		switch (theme)
		{
			/* Rack: warrior theme */
			case 1:
			{
				if (p_ptr->depth >= 37) switch(rand_int(19))
				{
					case 0: case 1: tval = TV_SWORD; break;
					case 2: case 3: tval = TV_POLEARM; break;
					case 4: case 5: tval = TV_BLUNT; break;
					case 6: case 7: tval = TV_BODY_ARMOR; break;
					case 8: case 9: tval = TV_BOW; break;	
					case 10: case 11: tval = TV_HEADGEAR; break;
					case 12: case 13: tval = TV_SHIELD; break;
					case 14: case 15: tval = TV_GLOVES; break;
					case 16: case 17: tval = TV_BOOTS; break;
					case 18: tval = TV_DRAG_ARMOR; break;
				}
				else switch(rand_int(18))
				{
					case 0: case 1: tval = TV_SWORD; break;
					case 2: case 3: tval = TV_POLEARM; break;
					case 4: case 5: tval = TV_BLUNT; break;
					case 6: case 7: tval = TV_BODY_ARMOR; break;
					case 8: case 9: tval = TV_BOW; break;	
					case 10: case 11: tval = TV_HEADGEAR; break;
					case 12: case 13: tval = TV_SHIELD; break;
					case 14: case 15: tval = TV_GLOVES; break;
					case 16: case 17: tval = TV_BOOTS; break;
				}
				break;
			}
			/* Bookshelf: spellcaster theme */
			case 2:
			{
				if (p_ptr->depth >= 20) switch(rand_int(16))
				{
					case 0: case 1: case 2: tval = TV_SCROLL; break;
					case 3: case 4: tval = TV_POTION; break;
					case 5: case 6: tval = TV_WAND; break;
					case 7: case 8: tval = TV_TALISMAN; break;
					case 9: case 10: tval = TV_POWDER; break;
					case 11: case 12: case 13: tval = TV_MAGIC_BOOK; break;
					case 14: tval = TV_ROD; break;	
					case 15: tval = TV_RING; break;	
				}
				else if (p_ptr->depth >= 12) switch(rand_int(14))
				{
					case 0: case 1: case 2: tval = TV_SCROLL; break;
					case 3: case 4: tval = TV_POTION; break;
					case 5: case 6: tval = TV_WAND; break;
					case 7: case 8: tval = TV_POWDER; break;
					case 9: case 10: case 11: tval = TV_MAGIC_BOOK; break;
					case 12: tval = TV_ROD; break;	
					case 13: tval = TV_RING; break;	
				}
				else switch(rand_int(12))
				{
					case 0: case 1: case 2: tval = TV_SCROLL; break;
					case 3: case 4: tval = TV_POTION; break;
					case 5: case 6: tval = TV_WAND; break;
					case 7: case 8: tval = TV_POWDER; break;
					case 9: case 10: tval = TV_MAGIC_BOOK; break;
					case 11: tval = TV_RING; break;	
				}
				break;
			}
			/* Closet: misc theme */
			case 3:
			{
				if (p_ptr->depth >= 12) switch(rand_int(15))
				{
					case 0: case 1: tval = TV_POTION; break;
					case 2: case 3: tval = TV_AMULET; break;
					case 4: case 5: tval = TV_CLOAK; break;
					case 6: case 7: tval = TV_STAFF; break;
					case 8: case 9: case 10: tval = TV_ARROW; break;
					case 11: tval = TV_ROD; break;
					case 12: tval = TV_RING; break;	
					case 13: tval = TV_POWDER; break;
					case 14: tval = TV_WAND; break;
				}
				else switch (rand_int(11))
				{
					case 0: case 1: tval = TV_POTION; break;
					case 2: case 3: tval = TV_CLOAK; break;
					case 4: case 5: tval = TV_STAFF; break;
					case 6: case 7: tval = TV_ARROW; break;
					case 8: tval = TV_AMULET; break;
					case 9: tval = TV_POWDER; break;
					case 10: tval = TV_WAND; break;
				}
				break;
			}
			/* Interesting vegetation */
			case 4:
			{
				switch(rand_int(2))
				{
					case 0: tval = TV_FOOD; message(MSG_GENERIC, 0, "You find some mushrooms."); break;
					case 1: tval = TV_LITE; message(MSG_GENERIC, 0, "You find some dry branches of enchanted wood."); break;
				}
				break;
			}
			
		}

		/* Make a themed object (if possible) */
		if (make_typed(i_ptr, tval, FALSE, FALSE, TRUE))
		{
			/* Mark history */
			object_history(i_ptr, ORIGIN_CHEST + theme, 0, 0, 0);

			/* Drop the object */
			drop_near(i_ptr, -1, y, x, TRUE);

			/* Done */
			object_created = TRUE;
		}
	}
}

/*
 * Search for hidden things
 */
static void search(void)
{
	int x, y;
	int skill;

	/* Search the nearby grids, which are always in bounds */
	for (y = (p_ptr->py - 1); y <= (p_ptr->py + 1); y++)
	{
		for (x = (p_ptr->px - 1); x <= (p_ptr->px + 1); x++)
		{
			skill = p_ptr->skill[SK_PER];

			/* Sometimes, notice traps */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if (trap_detectable(y, x))
			{
				trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

				if ((!t_ptr->visible) && (t_ptr->spot_factor == 1) && (trap_detectable(y, x)))
				{
					/* Alertness helps when detecting traps and runes */
					if (rand_int(100) < skill + (p_ptr->alertness * 25))
					{
						/* Discover invisible traps */

						/* Message */
						message(MSG_FIND, 0, "You found a trap!");
						t_ptr->visible = TRUE;

						/* Show trap */
						lite_spot(y, x);

						/* Disturb */
						disturb(0);
					}

					/* Trap can be searched for only once */
					t_ptr->spot_factor += 1;
				}
			}

			/* Find warding runes */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if (t_list[cave_t_idx[y][x]].w_idx >= WG_WARD_SLUMBER_ACTIVE_HIDDEN)
			{
				trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

				if (t_ptr->spot_factor == 1)
				{
					/* Alertness helps when detecting traps and runes */
					if (rand_int(100) < skill + (p_ptr->alertness * 25))
					{
						place_decoration(y, x, t_list[cave_t_idx[y][x]].w_idx - 30);

						lite_spot(y,x);

						message(MSG_GENERIC, 0, "You notice a warding rune!");

						/* Disturb */
						disturb(0);
					}
				}
			}

			/* Identify fountains */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if ((t_list[cave_t_idx[y][x]].w_idx >= WG_FOUNTAIN_HARPY) && (t_list[cave_t_idx[y][x]].w_idx <= WG_FOUNTAIN_REMOVE_CURSE))
			{
				trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

				if (t_ptr->spot_factor == 1)
				{
					if (rand_int(100) < skill)
					{
						switch (t_list[cave_t_idx[y][x]].w_idx)
						{
							case WG_FOUNTAIN_HARPY:
							{
								message(MSG_GENERIC, 0, "You smell harpy droppings.");
								break;
							}
							case WG_FOUNTAIN_ANGEL:
							{
								message(MSG_GENERIC, 0, "You find an angel's feather.");
								break;
							}
							case WG_FOUNTAIN_APE:
							{
								message(MSG_GENERIC, 0, "You notice ape hair.");
								break;
							}
							case WG_FOUNTAIN_NAGA:
							{
								message(MSG_GENERIC, 0, "You find a piece of shed cobra skin.");
								break;
							}
							case WG_FOUNTAIN_STATUE:
							{
								message(MSG_GENERIC, 0, "You see heavy footprints of a living statue.");
								break;
							}
							case WG_FOUNTAIN_RUINATION:
							{
								message(MSG_GENERIC, 0, "You notice a broken bone and some white hair.");
								break;
							}
							case WG_FOUNTAIN_MONSTERS:
							{
								message(MSG_GENERIC, 0, "You see suspicious faery tracks around the fountain.");
								break;
							}
							case WG_FOUNTAIN_DISEASE:
							{
								message(MSG_GENERIC, 0, "You smell disease.");
								break;
							}
							case WG_FOUNTAIN_CURE_WOUND:
							{
								message(MSG_GENERIC, 0, "You smell healing herbs.");
								break;
							}
							case WG_FOUNTAIN_STAT:
							{
								message(MSG_GENERIC, 0, "You feel energized near the fountain.");
								break;
							}
							case WG_FOUNTAIN_REMOVE_CURSE:
							{
								message(MSG_GENERIC, 0, "You sense a purifying power.");
								break;
							}
						}

						place_decoration(y, x, t_list[cave_t_idx[y][x]].w_idx + 11);
						lite_spot(y,x);

						/* Disturb */
						disturb(0);
					}
				}
			}

			/* Sometimes, notice secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Message */
				message(MSG_FIND, 0, "You have found a secret door.");

				/* Create closed door */
				if (t_list[cave_t_idx[y][x]].w_idx == WG_SHELF_SECRET_DOOR)
				{
					place_decoration(y, x, WG_SHELF_CLOSED_DOOR);
					lite_spot(y,x);
				}

				else if (t_list[cave_t_idx[y][x]].w_idx == WG_PAINTING_SECRET_DOOR)
				{
					place_decoration(y, x, WG_PAINTING_CLOSED_DOOR);
					lite_spot(y,x);
				}

				else if (t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET_SECRET_DOOR)
				{
					place_decoration(y, x, WG_CLOSET_CLOSED_DOOR);
					lite_spot(y,x);
				}

				else if (t_list[cave_t_idx[y][x]].w_idx == WG_RACK_SECRET_DOOR)
				{
					place_decoration(y, x, WG_RACK_CLOSED_DOOR);
					lite_spot(y,x);
				}

				else cave_set_feat(y, x, FEAT_CLOSED);

				if (trap_lock(y, x)) t_list[cave_t_idx[y][x]].visible = TRUE;

				/* Disturb */
				disturb(0);
			}

			/* Find stuff in racks (can only be searched if you are inside the room) */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if ((t_list[cave_t_idx[y][x]].w_idx == WG_RACK) && (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)))
			{
				/* The rack is now empty */
				place_decoration(y, x, WG_RACK_EMPTY);
				lite_spot(y,x);

				/* Create a themed item */
				if (rand_int(5) < 1) create_shelf_item(y, x, 1);
				else if (rand_int(9) < 1)
				{
					if (rand_int(100) < skill)
					{
						message(MSG_GENERIC, 0, "Your sharp eyes spot hidden item.");
						create_shelf_item(y, x, 1);
					}
					else
					{
						message(MSG_GENERIC, 0, "(You miss something.)");
					}
				}

				/* Disturb */
				disturb(0);
			}

			/* Find stuff in bookshelves (can only be searched if you are inside the room) */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if ((t_list[cave_t_idx[y][x]].w_idx == WG_SHELF) && (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)))
			{
				/* The bookshelf is now empty */
				place_decoration(y, x, WG_SHELF_EMPTY);
				lite_spot(y,x);

				/* Create a themed item */
				if (rand_int(11) < 2) create_shelf_item(y, x, 2);
				else if (rand_int(9) < 1)
				{
					if (rand_int(100) < skill)
					{
						message(MSG_GENERIC, 0, "Your sharp eyes spot hidden item.");
						create_shelf_item(y, x, 2);
					}
					else
					{
						message(MSG_GENERIC, 0, "(You miss something.)");
					}
				}

				/* Disturb */
				disturb(0);
			}

			/* Find stuff in closets (can only be searched if you are inside the room) */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if ((t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET) && (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)))
			{
				/* The closet is now empty */
				place_decoration(y, x, WG_CLOSET_EMPTY);
				lite_spot(y,x);

				/* Create a themed item */
				if (rand_int(5) < 1) create_shelf_item(y, x, 3);
				else if (rand_int(9) < 1)
				{
					if (rand_int(100) < skill)
					{
						message(MSG_GENERIC, 0, "Your sharp eyes spot hidden item.");
						create_shelf_item(y, x, 3);
					}
					else
					{
						message(MSG_GENERIC, 0, "(You miss something.)");
					}
				}

				/* Disturb */
				disturb(0);
			}

			/* Find stuff in interesting vegetation squares */
			if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px))
			{
				/* Don't search for traps if the player is blind. */
			}
			else if (p_ptr->confused || p_ptr->image)
			{
				/* Don't search for traps if the player is confused. */
			}
			else if (t_list[cave_t_idx[y][x]].w_idx == WG_INTERESTING_VEGETATION)
			{
				/* Good searchers may find mushrooms, torches or a faery portal */
				if (rand_int(2) < 1)
				{
					if (rand_int(100) < skill)
					{
						if (rand_int(3) < 1)
						{
							place_decoration(y, x, WG_FAERY_PORTAL);
							message(MSG_GENERIC, 0, "You find a hidden faery portal.");
						}
						else
						{
							create_shelf_item(y, x, 4);
							place_decoration(y, x, WG_VEGETATION);
						}
					}
					else
					{
						message(MSG_GENERIC, 0, "(You miss something.)");
						place_decoration(y, x, WG_VEGETATION);
					}
				}
				else 
				{
					place_decoration(y, x, WG_VEGETATION);
					message(MSG_GENERIC, 0, "You see some beautiful flowers.");
				}

				lite_spot(y,x);

				/* Disturb */
				disturb(0);
			}
		}
	}
}

/*
 * Determine if the object can be picked up, and has "=g" in its inscription.
 */
static bool auto_pickup_okay(const object_type *o_ptr)
{
	cptr s;

	/* It can't be carried */
	if (!inven_carry_okay(o_ptr)) return (FALSE);

	/* No inscription */
	if (!o_ptr->note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->note), '=');

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
 * Helper routine for py_pickup().
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

	o_ptr = &o_list[o_idx];

	/* Carry the object */
	slot = inven_carry(o_ptr);

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

	/* Message */
	message_format(MSG_PICKUP, 0, "You have %s (%c).", o_name, index_to_label(slot));

	/* Delete the object */
	delete_object_idx(o_idx);
}

/*
 * Make the player carry everything in a grid.
 *
 * If "pickup" is FALSE then only gold will be picked up.
 */
static void py_pickup(int pickup)
{
	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	char o_name[80];

	int last_o_idx = 0;

	int can_pickup = 0;
	int not_pickup = 0;

	bool pickup_query = FALSE;
	bool heavy = FALSE;

	int squelch = 0;

	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[p_ptr->py][p_ptr->px]; this_o_idx; this_o_idx = next_o_idx)
	{
		heavy = FALSE;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			if (cp_ptr->flags & CF_POVERTY)
			{
				/* Message */
				message_format(MSG_PICKUP, 0, "You find %ld gold pieces worth of %s. (You keep %ld gold.)",
			           	(long)o_ptr->pval, o_name, (o_ptr->pval +2) / 3);

				/* Collect the gold */
				p_ptr->au += (o_ptr->pval +2) / 3;
			}
			else
			{
				/* Message */
				message_format(MSG_PICKUP, 0, "You have found %ld gold pieces worth of %s.",
			           	(long)o_ptr->pval, o_name);

				/* Collect the gold */
				p_ptr->au += o_ptr->pval;
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

		/* Make sure you are properly aware of the object */
		if (object_aware_p(o_ptr)) object_aware(o_ptr);

		/* Test for squelch */
		squelch = squelch_itemp(o_ptr);

		/* Squelch it if necessary */
		if (squelch) 
		{
			do_squelch_item(o_ptr);

			if (auto_squelch) continue;
		}

		/* Test for auto-pickup */
		if (auto_pickup_okay(o_ptr))
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* Query before picking up */
		if (carry_query_flag) pickup_query = TRUE;

		/* Query if object is heavy */
		if (carry_heavy_query)
		{
			int i, j;
			int old_enc = 0;
			int new_enc = 0;

			/* Extract the "weight limit" (in tenth pounds) */
			i = adj_str_wgt[p_stat(A_STR)] * 100;

			/* Calculate current encumbarance */
			j = p_ptr->total_weight;

			/* Apply encumbarance from weight */
			if (j > i/2) old_enc = ((j - (i/2)) / (i / 10));

			/* Increase the weight, recalculate encumbarance */
			j += (o_ptr->number * object_weight(o_ptr));

			/* Apply encumbarance from weight */
			if (j > i / 2) new_enc = ((j - (i / 2)) / (i / 10));

			/* Should we query? */
			if (new_enc>old_enc)
			{
				pickup_query = TRUE;
				heavy = TRUE;
			}
		}

		/* Easy Floor */
		if (easy_floor)
		{
			/* Pickup if possible */
			if (pickup && inven_carry_okay(o_ptr))
			{
				/* Pick up if allowed */
				if (!pickup_query)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}

				/* Else count */
				else
				{
					/* Remember */
					last_o_idx = this_o_idx;

					/* Count */
					++can_pickup;
				}
			}

			/* Else count */
			else
			{
				/* Remember */
				last_o_idx = this_o_idx;

				/* Count */
				++not_pickup;
			}

			/* Check the next object */
			continue;
		}

		/* Describe the object */
		if (!pickup)
		{
			message_format(MSG_DESCRIBE, 0, "You see %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Note that the pack is too full */
		if (!inven_carry_okay(o_ptr))
		{
			message_format(MSG_FAIL, 0, "You have no room for %s.", o_name);

			/* Check the next object */
			continue;
		}

		/*
		 * Query for picking up/destroying floor items
		 * XXX XXX Ugly hack regarding rogue_like_commands
		 */
		if (pickup_query)
		{
			int i;
			char out_val[160];

			cptr keys = (rogue_like_commands) ? "y/n/^d" : "y/n/k";

			/* Hack - describe the object (again, so that the inscription will always appear) */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			if (!heavy) strnfmt(out_val, sizeof(out_val), "Pick up %s? [%s] ", o_name, keys);
			else strnfmt (out_val, sizeof(out_val), "Pick up %s (heavy)? [%s] ", o_name, keys);



			
			/* Prompt for it */
			prt(out_val, 0, 0);

			/* Get an acceptable answer */
			while (TRUE)
			{
				i = inkey();
				if (quick_messages) break;
				if (i == ESCAPE) break;
				if (strchr("YyNn", i)) break;
				if (rogue_like_commands && i == KTRL('D')) break;
				if (!rogue_like_commands && strchr("Kn", i)) break;
				bell("Illegal response to question!");
			}

			/* Erase the prompt */
			prt("", 0, 0);
			
			if ((i == 'Y') || (i == 'y'))
			{
				/* Pick up the object */
				py_pickup_aux(this_o_idx);
			}
			
			if ((rogue_like_commands && (i == KTRL('D'))) ||
				(!rogue_like_commands && ((i == 'K') || (i == 'k'))))
			{
				/* Artifact? */
				if (!destroy_check(o_ptr))
				{
					/* Describe the object (with {terrible/special}) */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

					/* Message */
					message_format(MSG_FAIL, 0, "You cannot destroy the %s.", o_name);
				}
				else
				{
					/* Destroy the object */
					delete_object_idx(this_o_idx);
				}
			}
		}

		/* Pick up the object */
		else py_pickup_aux(this_o_idx);
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
				o_ptr = &o_list[last_o_idx];

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

				/* Message */
				message_format(MSG_DESCRIBE, 0, "You see %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				message_format(MSG_DESCRIBE, 0, "You see a pile of %d objects.", not_pickup);
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
				o_ptr = &o_list[last_o_idx];

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

				/* Message */
				message_format(MSG_FAIL, 0, "You have no room for %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				message(MSG_FAIL, 0, "You have no room for any of the objects on the floor.");
			}

			/* Done */
			return;
		}

		/* Pick up objects */
		while (TRUE)
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

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x, bool show_monster_name)
{
	int num = 0;
	int k, chance;
	int critical_hit_chance = 0;

	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
	monster_race *r_ptr = get_monster_real(m_ptr);

	object_type *o_ptr;

	char m_name[80];

	byte special = 0;

	bool fear = FALSE;
	bool do_quake = FALSE;
	bool deadly_crit = FALSE;
	bool ambush = FALSE;

	/* Disturb the player */
	disturb(0);

	/* Anger the monster */
	m_ptr->calmed = 0;

	/* Nullify invisiblity */
	if (p_ptr->invis) nullify_invis();

	/* Extract monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_track(m_ptr->r_idx, m_ptr->u_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);

	/* Handle player fear */
	if (p_ptr->afraid > PY_FEAR_AFRAID)
	{

		/* Disturb the monster */
		m_ptr->sleep = 0;

		/* Message */
		message_format(MSG_FAIL, 0, "You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}

	/* Burning Hands */
	if (p_ptr->flaming_hands)
	{
		message_format(MSG_HIT, m_ptr->r_idx, "You burn %s.", m_name);
		(void)strike(GF_FIRE, y, x, damroll(12 , apply_sp_mod(3 + p_ptr->lev / 10, p_ptr->sp_dam)), 0);

		/* Attacking ends the spell next turn, even if its duration is made permanent */
		(void)set_flaming_hands(1);
		p_ptr->flaming_hands_perm = 0;

		return;
	}

	/* Claws of Winter */
	if (p_ptr->icy_hands)
	{
		message_format(MSG_HIT, m_ptr->r_idx, "You freeze %s.", m_name);
		(void)strike(GF_COLD, y, x, damroll(12 , apply_sp_mod(3 + p_ptr->lev / 10, p_ptr->sp_dam)), 0);
		(void)set_icy_hands(1);

		/* Attacking ends the spell next turn, even if its duration is made permanent */
		p_ptr->icy_hands_perm = 0;

		return;
	}

	/* Get the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Calculate the "attack quality" */
	chance = p_ptr->skill[SK_THN] + p_ptr->to_h_melee + object_to_h(o_ptr);

	/* Number of blows */
	int blows = p_ptr->num_blow;

	/* Fencing proficiency gives an extra blows against visible person or humanoid */
	if ((m_ptr->ml) && (o_ptr->tval == TV_SWORD))
	{
		if (r_ptr->flags4 & (RF4_PERSON)) blows += p_ptr->fencing;
		if (r_ptr->flags4 & (RF4_HUMANOID)) blows += p_ptr->fencing;
	}

	/* Attack once for each legal blow */
	while (num++ < blows)
	{
		/* Check for evasion */
		if ((r_ptr->flags2 & RF2_EVASIVE) && (rand_int(3) == 0))
		{
			message_format(MSG_MISS, m_ptr->r_idx, "%^s magically evades your blow!", m_name);

			/* Anger it */
			m_ptr->calmed = 0;

			/* Disturb the monster */
			m_ptr->sleep = 0;

			teleport_away(cave_m_idx[y][x], 4);
			lore_learn(m_ptr, LRN_FLAG2, RF2_EVASIVE, FALSE);
			break;
		}

		/* Test for hit */
		critical_hit_chance = (test_hit(chance, r_ptr->ac / (m_ptr->cursed + 1), m_ptr->ml));

		if (critical_hit_chance >= 0)
		{
			/* Long message for invisible monsters */
			if (!m_ptr->ml)
			{
				message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.", m_name);
			}
			/* Short message if this is the first attacked monster this round, or not a first blow */
			else if (!show_monster_name)
			{
				message_format(MSG_HIT, m_ptr->r_idx, "Hit.", m_name);
			}
			else
			{
				message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.", m_name);
			}

			/* Don't repeat the monster name */
			show_monster_name = FALSE;

			/* Roll damage */
			k = damroll(p_ptr->dd, p_ptr->ds);

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				u32b f1, f2, f3;

				/* Extract the flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Apply special damage */
				k = tot_dam_aux(o_ptr, k, m_ptr, &special);
				if ((f2 & TR2_IMPACT) && (k > 50)) do_quake = TRUE;

				/* Tripled critical damage? */
				if (f2 & TR2_DEADLY_CRIT) deadly_crit = TRUE;

				/* Critical hits only against visible monsters */
				if (m_ptr->ml)
				{
					/* You get ambush criticals only against distracted monsters */
					if (m_ptr->blinded || m_ptr->confused || m_ptr->monfear || m_ptr->sleep)
					{
						ambush = TRUE;
					}

					k = critical_norm(o_ptr, &special, k, ambush, critical_hit_chance, FALSE);
				}
			}
			/* Handle unarmed */
			else
			{
				/* Critical hits only against visible monsters */
				if (m_ptr->ml)
				{
					/* You get ambush criticals only against distracted monsters */
					if (m_ptr->blinded || m_ptr->confused || m_ptr->monfear || m_ptr->sleep)
					{
						ambush = TRUE;
					}

					k = critical_unarmed(k, &special, ambush, critical_hit_chance);
				}
			}

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (cheat_wizard)
			{
				message_format(MSG_CHEAT, 0, "You do %d (out of %d) damage.", k, m_ptr->hp);
			}

			/* HACK - Special - Force fear */
			if (special & SPEC_FEAR) 
			{
				fear = TRUE;
				special &= ~SPEC_FEAR;
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) break;

			/* Handle special effects (confusing, stunning, etc */
			if (special) attack_special(cave_m_idx[y][x], special, k);
		}

		/* Player misses */
		else
		{
			/* Message only if the monster is unseen */
			if (!m_ptr->ml)
			{
				message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			}
		}
	}

	/* Disturb the monster */
	m_ptr->sleep = 0;

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
	}

	/* Mega-Hack -- apply earthquake brand */
	if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10);
}


/* Helper function for secondary attacks */
static bool can_attack_monster(int y, int x)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

	/* You can attack if you can see the monster and it is not in a wall */
	if (cave_floor_bold(y, x))
	{
		if (m_ptr->ml) return TRUE;
	}

	return FALSE;
}

/* Helper function for secondary attacks */
static bool calmed_monster(int y, int x)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

	/* Return true if the monster is calmed */
	if (cave_floor_bold(y, x))
	{
		if (m_ptr->calmed) return TRUE;
	}

	return FALSE;
}

/* Helper function for secondary attacks */
static bool sleeping_monster(int y, int x)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

	/* Return true if the monster is sleeping */
	if (cave_floor_bold(y, x))
	{
		if (m_ptr->sleep) return TRUE;
	}

	return FALSE;
}

/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
static void move_player(int dir, int jumping)
{
	int y, x;
	int wild_melee = 0;

	bool high_ground = FALSE;
	bool movement_stopped_by_terrain = FALSE;

	/* Find the result of moving */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Is the player standing on a table, a platform, or an altar? */
	if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_TABLE) high_ground = TRUE;
	if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_PLATFORM) high_ground = TRUE;
	if ((t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx >= WG_ALTAR_OBSESSION) &&
		(t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx <= WG_ALTAR_DECEIT)) high_ground = TRUE;

	/* Difficult terrain has a chance of blocking movement or hindering melee attacks */
	switch (t_list[cave_t_idx[y][x]].w_idx)
	{
		case WG_VEGETATION:
		case WG_INTERESTING_VEGETATION:
		{
			/* Vegetation doesn't hinder attacking */
			if (cave_m_idx[y][x] > 0)
			{
			}
			/* Flying over */
			else if (p_ptr->flying)
			{
			}
			else if (rand_int(100) >= p_ptr->skill[SK_MOB])
			{
				movement_stopped_by_terrain = TRUE;
				message(MSG_HITWALL, 0, "You couldn't find a way through the thick vegetation.");
			}
			else
			{
				message(MSG_GENERIC, 0, "You hop over some bushes.");
			}

			break;
		}
		case WG_SPIKES:
		{
			/* Spikes do not hinder attacking */
			if (cave_m_idx[y][x] > 0)
			{
			}
			/* Flying over */
			if (p_ptr->flying)
			{
			}
			else if (rand_int(100) >= p_ptr->skill[SK_MOB])
			{
				if (rand_int(100) < 85)
				{
					message(MSG_HITWALL, 0, "Ouch! You step on a spike.");
				}
				else
				{
					message(MSG_HITWALL, 0, "Ouch! You step on spikes, destroying them.");
					delete_trap(y, x);
				}

				if (!p_ptr->no_poison && !resist_effect(RS_PSN))
				{
					damage_player(damroll(1, 10), "spikes");
					set_poisoned(p_ptr->poisoned + rand_int(30) + 10);
				}
			}
			else
			{
				message(MSG_GENERIC, 0, "You hop over spikes.");
			}

			break;
		}
		case WG_TABLE:
		case WG_PLATFORM:
		{
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_TABLE) break;
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_PLATFORM) break;
			if ((t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx >= WG_ALTAR_OBSESSION) &&
				(t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx <= WG_ALTAR_DECEIT)) break;

			/* Skip testing if attacking */
			if (cave_m_idx[y][x] > 0)
			{
			}
			/* Flying over */
			else if (p_ptr->flying)
			{
			}
			/* Climbing steps never fails */
			else if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_STEPS) break;

			/* Check whether movement is stopped */
			else if (rand_int(100) >= p_ptr->skill[SK_MOB])
			{
				movement_stopped_by_terrain = TRUE;
				message(MSG_HITWALL, 0, "Your jump didn't succeed.");
			}
			else if (t_list[cave_t_idx[y][x]].w_idx == WG_TABLE)
			{
				message(MSG_GENERIC, 0, "You hop on the table.");
			}
			else if (t_list[cave_t_idx[y][x]].w_idx == WG_PLATFORM)
			{
				message(MSG_GENERIC, 0, "You hop on the platform.");
			}

			break;
		}
	}

	/* If we are behind a cover or on a higher ground, don't remind the player of moving to a good firing position */
	int xx;
	int yy;
	bool remind = TRUE;

	for (xx = p_ptr->px - 1; xx <= p_ptr->px + 1; xx++)
	{
		for (yy = p_ptr->py - 1; yy <= p_ptr->py + 1; yy++)
		{
			if ((t_list[cave_t_idx[yy][xx]].w_idx == WG_TABLE) || (t_list[cave_t_idx[yy][xx]].w_idx == WG_PLATFORM))
			{
				remind = FALSE;
			}
		}
	}

	/* Attack two monsters next to you. */
	int calmed_monsters = 0;
	int sleeping_monsters = 0;
	if (cave_m_idx[y][x] > 0)
	{
		/* Are we in a wild melee with multiple monsters? Don't count unseen monsters or monster in walls.*/
		if (cave_m_idx[p_ptr->py + 1][p_ptr->px] > 0)
		{
			if ((y == p_ptr->py + 1) && (x == p_ptr->px)) {}
			else if (can_attack_monster(p_ptr->py + 1, p_ptr->px))
			{
				if (calmed_monster(p_ptr->py + 1, p_ptr->px)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py + 1, p_ptr->px)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py][p_ptr->px + 1] > 0)
		{
			if ((y == p_ptr->py) && (x == p_ptr->px + 1)) {}
			else if (can_attack_monster(p_ptr->py, p_ptr->px + 1))
			{
				if (calmed_monster(p_ptr->py, p_ptr->px + 1)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py, p_ptr->px + 1)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py - 1][p_ptr->px] > 0)
		{
			if ((y == p_ptr->py - 1) && (x == p_ptr->px)) {}
			else if (can_attack_monster(p_ptr->py - 1, p_ptr->px))
			{
				if (calmed_monster(p_ptr->py - 1, p_ptr->px)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py - 1, p_ptr->px)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py][p_ptr->px - 1] > 0)
		{
			if ((y == p_ptr->py) && (x == p_ptr->px - 1)) {}
			else if (can_attack_monster(p_ptr->py, p_ptr->px - 1))
			{
				if (calmed_monster(p_ptr->py, p_ptr->px - 1)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py, p_ptr->px - 1)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py + 1][p_ptr->px + 1] > 0)
		{
			if ((y == p_ptr->py + 1) && (x == p_ptr->px + 1)) {}
			else if (can_attack_monster(p_ptr->py + 1, p_ptr->px + 1))
			{
				if (calmed_monster(p_ptr->py + 1, p_ptr->px + 1)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py + 1, p_ptr->px + 1)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py - 1][p_ptr->px - 1] > 0)
		{
			if ((y == p_ptr->py - 1) && (x == p_ptr->px - 1)) {}
			else if (can_attack_monster(p_ptr->py - 1, p_ptr->px - 1))
			{
				if (calmed_monster(p_ptr->py - 1, p_ptr->px - 1)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py - 1, p_ptr->px - 1)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py + 1][p_ptr->px - 1] > 0)
		{
			if ((y == p_ptr->py + 1) && (x == p_ptr->px - 1)) {}
			else if (can_attack_monster(p_ptr->py + 1, p_ptr->px - 1))
			{
				if (calmed_monster(p_ptr->py + 1, p_ptr->px - 1)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py + 1, p_ptr->px - 1)) sleeping_monsters ++;
				else wild_melee++;
			}
		}
		if (cave_m_idx[p_ptr->py - 1][p_ptr->px + 1] > 0)
		{
			if ((y == p_ptr->py - 1) && (x == p_ptr->px + 1)) {}
			else if (can_attack_monster(p_ptr->py - 1, p_ptr->px + 1))
			{
				if (calmed_monster(p_ptr->py - 1, p_ptr->px + 1)) calmed_monsters ++;
				else if (sleeping_monster(p_ptr->py - 1, p_ptr->px + 1)) sleeping_monsters ++;
				else wild_melee++;
			}
		}

		/* Attack the main enemy. First check whether lower ground hinders attacking */
		if (((t_list[cave_t_idx[y][x]].w_idx == WG_TABLE)
			|| (t_list[cave_t_idx[y][x]].w_idx == WG_PLATFORM)) && (!high_ground))
		{
			if (rand_int(5) < 2)
			{
				message(MSG_HITWALL, 0, "You could not attack because of your bad position.");
			}
			else py_attack(y, x, FALSE);
		}
		else py_attack(y, x, FALSE);

		int extra_y = 0;
		int extra_x = 0;
		int attack_calmed = TRUE;
		int attack_sleeping = TRUE;

		while (wild_melee + sleeping_monsters + calmed_monsters > 0)
		{
			extra_y = p_ptr->py + rand_int(3) - 1;
			extra_x = p_ptr->px + rand_int(3) - 1;

			if ((y == extra_y) && (x == extra_x)) continue;

			if (cave_m_idx[extra_y][extra_x] > 0)
			{
				/* You can't second-attack unseen monsters or monsters in the wall. */
				if (!can_attack_monster(extra_y, extra_x)) continue;

				/* Ask confirmation to second-attack calmed or sleeping monsters;
				if not, move on to the next possible monster if there is one. */
				if (calmed_monster(extra_y, extra_x))
				{

					if (!attack_calmed) break;

					if (!get_check("Attack a calmed monster? "))
					{
						attack_calmed = FALSE;
						calmed_monsters = 0;
						continue;
					}
				}
				if (sleeping_monster(extra_y, extra_x))
				{
					if (!attack_sleeping) break;

					if (!get_check("Attack a sleeping monster? "))
					{
						attack_sleeping = FALSE;
						sleeping_monsters = 0;
						continue;
					}
				}

				/* First check whether lower ground hinders attacking */
				if (((t_list[cave_t_idx[extra_y][extra_x]].w_idx == WG_TABLE)
					|| (t_list[cave_t_idx[extra_y][extra_x]].w_idx == WG_PLATFORM)) && (!high_ground))
				{
					if (rand_int(5) < 2)
					{
						message(MSG_HITWALL, 0, "You could not attack because of your bad position.");
					}
					else py_attack(extra_y, extra_x, TRUE);
				}
				else py_attack(extra_y, extra_x, TRUE);

				wild_melee = 0;
				calmed_monsters = 0;
				sleeping_monsters = 0;
			}
		}

	}

	else if (movement_stopped_by_terrain)
	{
	}

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if (easy_alter && !jumping &&
	         (((cave_info[y][x] & (CAVE_MARK)) && (cave_feat[y][x] == FEAT_CLOSED)) ||
			 trap_disarmable(y, x)))

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

	/* Player can not walk through "walls" */
	else if (!cave_floor_bold(y, x))
	{
		/* Disturb the player */
		disturb(0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);

				if (t_list[cave_t_idx[y][x]].w_idx == WG_TREE)
				{
					message(MSG_HITWALL, 0, "You feel a tree blocking you way.");
				}

				else
				{
					message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				}
			}

			/* Closed door */
			else if (cave_feat[y][x] == FEAT_CLOSED)
			{
				message(MSG_HITWALL, 0, "You feel a door blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}


		/* Mention known obstacles */
		else
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				if decoration(y, x)
				{
					message(MSG_HITWALL, 0, "There is a tree blocking you way.");

				}

				else
				{
					message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
				}
			}

			/* Closed door */
			else if (cave_feat[y][x] == FEAT_CLOSED)
			{
				message(MSG_HITWALL, 0, "There is a door blocking your way.");
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "There is a wall blocking your way.");
			}
		}
	}

	/* Normal movement */
	else
	{
		/* Move player */
		monster_swap(p_ptr->py, p_ptr->px, y, x);

		/* New location */
		y = p_ptr->py;
		x = p_ptr->px;

		/* Spontaneous Searching -- now automatic */
		search();

		/* Continuous Searching */
		if (p_ptr->searching) search();

		/* Handle "objects" */
		py_pickup(jumping != always_pickup);

		/* Handle "store doors" */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Free turn XXX XXX XXX */
			p_ptr->energy_use = 0;
		}

		/* Discover invisible traps */
		else if (w_info[t_list[cave_t_idx[y][x]].w_idx].flags & WGF_PLAYER)
		{
			trap_type *t_ptr = &t_list[cave_t_idx[y][x]];
		
			/* Disturb */
			disturb(0);

			/* Non-detectable traps */
			if (!trap_detectable(y, x))
			{
				/* Do nothing */
			}
			else if (!t_ptr->visible)
			{
				t_ptr->visible = TRUE;

				/* Hack - light the spot */
				lite_spot(y, x);
				
				/* Protection from traps */
				if (p_ptr->safety)
				{
					/* Message */
					message(MSG_FIND, 0, "You feel a sudden sense of danger, and narrowly avoid a trap!");
				}
				else
				{
					/* Message */
					message(MSG_FIND, 0, "You stumble on a trap!");

					/* Hit the trap */
					hit_trap(y, x);
				}
			}
			else
			{
				/* Protection from traps */
				if (p_ptr->safety)
				{
					message_format(MSG_RESIST, 0, "Through divine influence, %s fails to trigger!", trap_name(t_ptr->w_idx, 2));
				}
				else 
				{
					/* Hit the trap */
					hit_trap(y, x);
				}
			}
		}

		/* Get hit by Warding Runes? Check all four cardinal directions */
		int i = 0;
		int x_add = 0;
		int y_add = 0;
		int distance = 0;

		for (i = 1; i < 5; i++)
		{
			switch (i)
			{
				case 1: y_add = 1; x_add = 0; break;
				case 2: y_add = -1; x_add = 0; break;
				case 3: x_add = 1; y_add = 0; break;
				case 4: x_add = -1; y_add = 0; break;
			}

			for (distance = 1; distance <= MAX_SIGHT; distance++)
			{
				/* Don't continue outside rooms */
				if (!(cave_info[y][x] & (CAVE_ROOM))) break;

				switch (t_list[cave_t_idx[y+(y_add * distance)][x+x_add * distance]].w_idx)
				{
					case WG_WARD_SLUMBER_ACTIVE_HIDDEN:
					{
						place_decoration(y+(y_add * distance), x+(x_add * distance), WG_WARD_SLUMBER_ACTIVE);
						/* Fall through */
					}
					case WG_WARD_SLUMBER_ACTIVE:
					case WG_WARD_SLUMBER_ACTIVE_MASTERED:
					case WG_WARD_SLUMBER_ACTIVE_INCOMPREHENSIBLE:
					{
						if (p_ptr->safety)
						{
							/* Message */
							message(MSG_FIND, 0, "A Rune of Slumber flashes, but you are safe!");
						}
						else
						{
							message(MSG_FIND, 0, "A Rune of Slumber strikes you!");

							if ((rand_int(100) < p_ptr->skill[SK_SAV]) || (p_ptr->free_act))
							{
								message(MSG_RESIST, 0, "You resist the effects!");
							}
							else
							{
								(set_paralyzed(p_ptr->paralyzed + rand_int(8) + 6));
							}
						}
						break;
					}
					case WG_WARD_TERROR_ACTIVE_HIDDEN:
					{
						place_decoration(y+(y_add * distance), x+(x_add * distance), WG_WARD_TERROR_ACTIVE);
						/* Fall through */
					}
					case WG_WARD_TERROR_ACTIVE:
					case WG_WARD_TERROR_ACTIVE_MASTERED:
					case WG_WARD_TERROR_ACTIVE_INCOMPREHENSIBLE:
					{
						if (p_ptr->safety)
						{
							/* Message */
							message(MSG_FIND, 0, "A Rune of Terror flashes, but you are safe!");
						}
						else
						{
							message(MSG_FIND, 0, "A Rune of Terror strikes you!");

							if ((rand_int(100) < p_ptr->skill[SK_SAV]) || (p_ptr->bravery))
							{
								message(MSG_RESIST, 0, "You resist the effects!");
							}
							else
							{
								set_afraid(p_ptr->afraid + rand_int(10) + 20);

								/* Sometimes lose INT & WIS */
								if (rand_int(3) < 1)
								{
									message(MSG_EFFECT, 0, "Paranoia creeps in...");
									do_dec_stat(A_INT, 1, FALSE, TRUE);
									do_dec_stat(A_WIS, 1, FALSE, TRUE);
								}
							}
						}

						break;
					}
					case WG_WARD_CHANGE_ACTIVE_HIDDEN:
					{
						place_decoration(y+(y_add * distance), x+(x_add * distance), WG_WARD_CHANGE_ACTIVE);
						/* Fall through */
					}
					case WG_WARD_CHANGE_ACTIVE:
					case WG_WARD_CHANGE_ACTIVE_MASTERED:
					case WG_WARD_CHANGE_ACTIVE_INCOMPREHENSIBLE:
					{
						if (p_ptr->safety)
						{
							/* Message */
							message(MSG_FIND, 0, "A Rune of Change flashes, but you are safe!");
						}
						else
						{
							message(MSG_FIND, 0, "A Rune of Change strikes you!");
							if (!(p_ptr->shape == SHAPE_PERSON))
							{
								message(MSG_RESIST, 0, "You are already in a different form, no effect.");
							}
							else if (rand_int(100) < p_ptr->skill[SK_SAV])
							{
								message(MSG_RESIST, 0, "You resist the effects!");
							}
							else
							{
								/* Shapeshift into a random form */
								bool ignore_me;
								do_power(POW_HARPY_FORM + rand_int(8), 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
							}
						}

						break;
					}
					case WG_WARD_DEATH_ACTIVE_HIDDEN:
					{
						place_decoration(y+(y_add * distance), x+(x_add * distance), WG_WARD_DEATH_ACTIVE);
						/* Fall through */
					}
					case WG_WARD_DEATH_ACTIVE:
					case WG_WARD_DEATH_ACTIVE_MASTERED:
					case WG_WARD_DEATH_ACTIVE_INCOMPREHENSIBLE:
					{
						if (p_ptr->safety)
						{
							/* Message */
							message(MSG_FIND, 0, "A Rune of Blood flashes, but you are safe!");
						}
						else
						{
							message(MSG_FIND, 0, "A Rune of Blood strikes you!");

							if ((rand_int(100) < p_ptr->skill[SK_SAV]) || (p_ptr->no_cut))
							{
								message(MSG_RESIST, 0, "You resist the effects!");
							}
							else
							{
								int cut = rand_int(100 + (3 * p_ptr->depth));
								if (rand_int(150) < p_ptr->depth + 5) cut = 2000;
								(void)set_cut(p_ptr->cut + cut);
							}
						}

						break;
					}
					case WG_WARD_CURSING_ACTIVE_HIDDEN:
					{
						place_decoration(y+(y_add * distance), x+(x_add * distance), WG_WARD_CURSING_ACTIVE);
						/* Fall through */
					}
					case WG_WARD_CURSING_ACTIVE:
					case WG_WARD_CURSING_ACTIVE_MASTERED:
					case WG_WARD_CURSING_ACTIVE_INCOMPREHENSIBLE:
					{
						if (p_ptr->safety)
						{
							/* Message */
							message(MSG_FIND, 0, "A Rune of Evil Eye flashes, but you are safe!");
						}
						else
						{
							message(MSG_FIND, 0, "A Rune of Evil Eye strikes you!");

							if (rand_int(100) < p_ptr->skill[SK_SAV])
							{
								message(MSG_RESIST, 0, "You resist the effects!");
							}
							else
							{
								if (rand_int(100) < 50)
								{
									/* 50% chance to curse equipment */
									int curse = rand_int(8);
									if (curse == 0) curse_armor();
									if (curse == 1) curse_weapon();
									else curse_minor();
								}
								else
								{
									/* 50% chance to drain experience */
									if (p_ptr->hold_life && (rand_int(100) < 75))
									{
										message(MSG_RESIST, 0, "You keep hold of your life force!");
									}
									else if (p_ptr->hold_life)
									{
										message(MSG_EFFECT, 0, "You feel your life slipping away!");
										lose_exp(200 + (p_ptr->exp / 250));
									}
									else
									{
										message(MSG_EFFECT, 0, "You feel your life draining away!");
										lose_exp(200 + (p_ptr->exp / 25));
									}
								}
							}
						}

						break;
					}
				}

				/* Don't continue through walls */
				if (!cave_floor_bold((y + (y_add * distance)), (x + (x_add * distance)))) break;
			}
		}

		/* If walking on a Circle of Lifeforce, cure disease, restore life energies and physical stats */
		if (t_list[cave_t_idx[y][x]].w_idx == WG_CIRCLE_OF_LIFEFORCE)
		{
			set_diseased(0);
			restore_exp();
			do_res_stat(A_STR);
			do_res_stat(A_DEX);
			do_res_stat(A_CON);
		}

		/* If walking on a Circle of Nexus, restore mental stats */
		if (t_list[cave_t_idx[y][x]].w_idx == WG_CIRCLE_OF_NEXUS)
		{
			do_res_stat(A_INT);
			do_res_stat(A_WIS);
			do_res_stat(A_CHR);
		}

		/* Remind the player of a good firing position */
		if (!(((t_list[cave_t_idx[y][x]].w_idx == WG_TABLE) || (t_list[cave_t_idx[y][x]].w_idx == WG_PLATFORM)) ||
			((t_list[cave_t_idx[y][x]].w_idx >= WG_ALTAR_OBSESSION) && (t_list[cave_t_idx[y][x]].w_idx <= WG_ALTAR_DECEIT))))
		{
			for (xx = x - 1; xx <= x + 1; xx++)
			{
				for (yy = y - 1; yy <= y + 1; yy++)
				{
					if ((t_list[cave_t_idx[yy][xx]].w_idx == WG_TABLE) || (t_list[cave_t_idx[yy][xx]].w_idx == WG_PLATFORM))
					{
						if (remind)
						{
							message(MSG_GENERIC, 0, "This is a good firing position behind a cover.");
							xx = x+1;
							yy = y+1;
						}
					}
				}
			}
		}

		/* Update stuff so that terrain special bonuses have effect */
		p_ptr->update |= (PU_BONUS | PR_MANA);
	}
}

/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls XXX XXX XXX */
	if (!in_bounds(y, x)) return FALSE;

	/* Non-wall grids are not known walls */
	if (cave_feat[y][x] < FEAT_SECRET) return FALSE;
	if (cave_feat[y][x] > FEAT_PERM_SOLID) return FALSE;

	/* Unknown walls are not known walls */
	if (!(cave_info[y][x] & (CAVE_MARK))) return FALSE;

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
 * to which you were not previously adjacent (marked as /).
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
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
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
	row = p_ptr->py + ddy[dir];
	col = p_ptr->px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], p_ptr->py, p_ptr->px))
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
	if (see_wall(cycle[i-1], p_ptr->py, p_ptr->px))
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
	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max, inv;
	int option, option2;

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
		row = p_ptr->py + ddy[new_dir];
		col = p_ptr->px + ddx[new_dir];

		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[row][col]];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
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
			bool notice = TRUE;

			/* Examine the terrain */
			switch (cave_feat[row][col])
			{
				/* Floors */
				case FEAT_FLOOR:
				{
					/* Break if there's a trap on it */
					if (trap_player(row, col)) break;
				}

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (run_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (run_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
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

			row = p_ptr->py + ddy[new_dir];
			col = p_ptr->px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET) || (cave_feat[row][col] > FEAT_PERM_SOLID))
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

			row = p_ptr->py + ddy[new_dir];
			col = p_ptr->px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET) || (cave_feat[row][col] > FEAT_PERM_SOLID))
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
		else if (!run_cut_corners)
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
			row = p_ptr->py + ddy[option];
			col = p_ptr->px + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (see_nothing(option, row, col) &&
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
	if (see_wall(p_ptr->run_cur_dir, p_ptr->py, p_ptr->px))
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
			disturb(0);

			/* Done */
			return;
		}
	}

	/* Decrease counter */
	p_ptr->running--;

	/* Take time */
	p_ptr->energy_use = 100;

	/* Move the player */
	move_player(p_ptr->run_cur_dir, FALSE);
}

/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	char out_val[160];
	int use_charge;
	bool ignore_me;
	byte quest;

	/* Faery portal? */
	if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_FAERY_PORTAL)
	{
		use_charge = do_power(332, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->energy_use = 100;
		return;
	}

	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_LESS)
	{
		message(MSG_FAIL, 0, "I see no up staircase here.");
		return;
	}

	/* Verify leaving quest level */
	if (verify_leave_quest && 
		((quest = quest_check(p_ptr->depth)) == QUEST_GUILD ||
		quest == QUEST_UNIQUE || ((quest == QUEST_VAULT) && (quest_item_slot() == -1))))
	{
		sprintf(out_val, "Really risk failing your quest? ");
		if (!get_check(out_val)) return;
	}

	/* Ironman */
	if (adult_ironman)
	{
		message(MSG_FAIL, 0, "Nothing happens!");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Reset coordinates to Circle of Recall and Circle of Nexus */
	p_ptr->recall_y = 0;
	p_ptr->recall_x = 0;
	p_ptr->nexus_y = 0;
	p_ptr->nexus_x = 0;

	/* Reset alertness, fencing, and archery */
	p_ptr->alertness = 0;
	p_ptr->fencing = 0;
	p_ptr->archery = 0;

	/* Reset monster summon power */
	p_ptr->monster_summon_power = 0;

	/* Reset permanent spells */
	p_ptr->tim_see_invis_perm = 0;
	p_ptr->tim_invis_perm = 0;
	p_ptr->tim_infra_perm = 0;
	p_ptr->tim_stealth_perm = 0;
	p_ptr->fast_perm = 0;
	p_ptr->absorb_perm = 0;
	p_ptr->protevil_perm = 0;
	p_ptr->protchaos_perm = 0;
	p_ptr->flaming_hands_perm = 0;
	p_ptr->icy_hands_perm = 0;
	p_ptr->resilient_perm = 0;
	p_ptr->hero_perm = 0;
	p_ptr->rage_perm = 0;
	p_ptr->blessed_perm = 0;
	p_ptr->safety_perm = 0;
	p_ptr->shield_perm = 0;
	p_ptr->stability_perm = 0;
	p_ptr->tim_bravery_perm = 0;
	p_ptr->sp_dur_perm = 0;
	p_ptr->tim_sp_dam_perm = 0;
	p_ptr->tim_sp_inf_perm = 0;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* In FayAngband, stairs up always take you to town. */
	p_ptr->depth = 0;

	/* Leaving */
	p_ptr->leaving = TRUE;
}

/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	char out_val[160];
	byte quest;

	/* Verify stairs */
 	if (cave_feat[p_ptr->py][p_ptr->px] == FEAT_MORE)
	{
		/* Verify leaving quest level */
		if (verify_leave_quest && 
		((quest = quest_check(p_ptr->depth)) == QUEST_GUILD ||
		quest == QUEST_UNIQUE || ((quest == QUEST_VAULT) && (quest_item_slot() == -1))))
		{
			sprintf(out_val, "Really risk failing your quest? ");
			if (!get_check(out_val)) return;
		}

		/* Verify leaving with unused Lore points */
		if (p_ptr->lore > p_ptr->lore_uses)
		{
			sprintf(out_val, "You haven't used all your Lore points yet. Are you sure? ");
			if (!get_check(out_val)) return;
		}

		/* Verify entering Halls of Mist without a torch */
		object_type *o_ptr;
		o_ptr = &inventory[INVEN_LITE];
		if ((!(o_ptr->tval == TV_LITE)) && !(p_ptr->depth))
		{
			sprintf(out_val, "Are you sure you want to enter the Halls of Mist without a torch? ");
			if (!get_check(out_val)) return;
		}

		/* Ask the player where she wants to navigate. Choose the right question depending on circumstances. */

		/* First set of questions: Max Depth is quest depth */
		if ((quest_check(p_ptr->max_depth) == QUEST_FIXED) || (quest_check(p_ptr->max_depth) == QUEST_FIXED_U))
		{
			if ((p_ptr->depth) && ((quest_check(p_ptr->depth +1) == QUEST_FIXED) || (quest_check(p_ptr->depth +1) == QUEST_FIXED_U)))
			{
				prt("Descend to next (l)evel?", 0, 0);
			}
			else if (p_ptr->depth)
			{
				prt("Descend to next (l)evel or (d)ive two levels?", 0, 0);
			}
			else if (p_ptr->max_depth > p_ptr->min_depth +1)
			{
				prt("Navigate to which depth: (s)ame as before or (e)asier?", 0, 0);
			}
			else
			{
				prt("Navigate to (s)ame depth as before?", 0, 0);
			}

			flush();
			int ch;
			ch = inkey();
			prt("", 0, 0);

			/* Analyze the answer, then adjust max_depth, min_depth, and current depth. */
			if ((ch == 'e') && (p_ptr->max_depth > p_ptr->min_depth +1) && !(p_ptr->depth))
			{
				p_ptr->max_depth--;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if ((ch == 's') && (p_ptr->max_depth > p_ptr->min_depth) && !(p_ptr->depth))
			{
				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->min_depth < p_ptr->max_depth) p_ptr->min_depth++;
			}
			else if ((ch == 's') && (p_ptr->max_depth == 48))
			{
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if ((ch == 'l') && (p_ptr->depth))
			{
				p_ptr->max_depth++;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if ((ch == 'd') && (p_ptr->depth)
				&& !((quest_check(p_ptr->depth +1) == QUEST_FIXED) || (quest_check(p_ptr->depth +1) == QUEST_FIXED_U)))
			{
				p_ptr->max_depth = p_ptr->max_depth +2;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else
			{
				return;
			}
		}

		/* Second set of questions: Max Depth +1 is quest level */
		else if ((quest_check(p_ptr->max_depth +1) == QUEST_FIXED) || (quest_check(p_ptr->max_depth +1) == QUEST_FIXED_U))
		{
			if (p_ptr->depth)
			{
				prt("Descend one (l)evel?", 0, 0);
			}
			else if (p_ptr->max_depth <= p_ptr->min_depth)
			{
				prt("Descend to (l)ower depth than before?", 0, 0);
			}
			else if (p_ptr->max_depth == p_ptr->min_depth +1)
			{
				prt("Navigate to which depth: (s)ame as before or (l)ower?", 0, 0);
			}	
			else
			{
				prt("Navigate to which depth: (e)asier, (s)ame, or (l)ower?", 0, 0);
			}
			flush();
			int ch;
			ch = inkey();
			prt("", 0, 0);

			/* Analyze the answer, then adjust max_depth, min_depth, and current depth. */
			if ((ch == 'e') && (p_ptr->max_depth > p_ptr->min_depth +1) && !(p_ptr->depth))
			{
				p_ptr->max_depth--;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if ((ch == 's') && (p_ptr->max_depth > p_ptr->min_depth) && !(p_ptr->depth))
			{
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if (ch == 'l')
			{
				p_ptr->max_depth++;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else
			{
				return;
			}
		}

		/* Third set of questions: Quest depth is two levels away*/
		else if ((quest_check(p_ptr->max_depth +2) == QUEST_FIXED) || (quest_check(p_ptr->max_depth +2) == QUEST_FIXED_U))
		{
			if ((p_ptr->depth) || (p_ptr->max_depth < 2))
			{
				prt("Descend a (l)evel or (d)ive two levels?", 0, 0);
			}
			else if ((p_ptr->depth) || (p_ptr->max_depth < 1))
			{
				prt("Descend one (l)evel or (d)ive two levels?", 0, 0);
			}
			else if (p_ptr->max_depth <= p_ptr->min_depth)
			{
				prt("Descend to (l)ower depth than before or (d)ive even deeper?", 0, 0);
			}
			else if (p_ptr->max_depth == p_ptr->min_depth +1)
			{
				prt("Navigate to which depth: (s)ame as before, (l)ower, or (d)ive?", 0, 0);
			}	
			else
			{
				prt("Navigate to which depth: (e)asier, (s)ame, (l)ower, (d)ive?", 0, 0);
			}
			flush();
			int ch;
			ch = inkey();
			prt("", 0, 0);

			/* Analyze the answer, then adjust max_depth, min_depth, and current depth. */
			if ((ch == 'e') && (p_ptr->max_depth > p_ptr->min_depth +1) && !(p_ptr->depth))
			{
				p_ptr->max_depth--;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if ((ch == 's') && (p_ptr->max_depth > p_ptr->min_depth) && !(p_ptr->depth))
			{
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if (ch == 'l')
			{
				p_ptr->max_depth++;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if (ch == 'd')
			{
				p_ptr->max_depth = p_ptr->max_depth +2;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else
			{
				return;
			}
		}

		/* Fourth set of questions: Not near the quest depth */
		else
		{
			if ((p_ptr->depth) || (p_ptr->max_depth < 2))
			{
				prt("Descend a (l)evel, (d)ive two levels, or (p)lunge three levels?", 0, 0);
			}
			else if ((p_ptr->depth) || (p_ptr->max_depth < 1))
			{
				prt("Descend one (l)evel or (d)ive two levels?", 0, 0);
			}
			else if (p_ptr->max_depth <= p_ptr->min_depth)
			{
				prt("Descend to (l)ower depth than before or (d)ive even deeper?", 0, 0);
			}
			else if (p_ptr->max_depth == p_ptr->min_depth +1)
			{
				prt("Navigate to which depth: (s)ame as before, (l)ower, or (d)ive?", 0, 0);
			}	
			else
			{
				prt("Navigate to which depth: (e)asier, (s)ame, (l)ower, (d)ive?", 0, 0);
			}
			flush();
			int ch;
			ch = inkey();
			prt("", 0, 0);

			/* Analyze the answer, then adjust max_depth, min_depth, and current depth. */
			if ((ch == 'e') && (p_ptr->max_depth > p_ptr->min_depth +1) && !(p_ptr->depth))
			{
				p_ptr->max_depth--;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if ((ch == 's') && (p_ptr->max_depth > p_ptr->min_depth) && !(p_ptr->depth))
			{
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if (ch == 'l')
			{
				p_ptr->max_depth++;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if (ch == 'd')
			{
				p_ptr->max_depth = p_ptr->max_depth +2;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else if (ch == 'p')
			{
				p_ptr->max_depth = p_ptr->max_depth +3;
				p_ptr->depth = p_ptr->max_depth;
				p_ptr->min_depth++;
			}
			else
			{
				return;
			}
		}

		/* Hack -- take a turn */
		p_ptr->energy_use = 100;
		
		/* Reset Proficiency uses */
		p_ptr->lore_uses = 0;
		p_ptr->reserves_uses = 0;
		p_ptr->escapes_uses = 0;

		/* Reset coordinates to Circle of Recall and Circle of Nexus */
		p_ptr->recall_y = 0;
		p_ptr->recall_x = 0;
		p_ptr->nexus_y = 0;
		p_ptr->nexus_x = 0;

		/* Reset alertness, fencing, and archery */
		p_ptr->alertness = 0;
		p_ptr->fencing = 0;
		p_ptr->archery = 0;

		/* Reset monster summon power */
		p_ptr->monster_summon_power = 0;

		/* Reset permanent spells */
		p_ptr->tim_see_invis_perm = 0;
		p_ptr->tim_invis_perm = 0;
		p_ptr->tim_infra_perm = 0;
		p_ptr->tim_stealth_perm = 0;
		p_ptr->fast_perm = 0;
		p_ptr->absorb_perm = 0;
		p_ptr->protevil_perm = 0;
		p_ptr->protchaos_perm = 0;
		p_ptr->flaming_hands_perm = 0;
		p_ptr->icy_hands_perm = 0;
		p_ptr->resilient_perm = 0;
		p_ptr->hero_perm = 0;
		p_ptr->rage_perm = 0;
		p_ptr->blessed_perm = 0;
		p_ptr->safety_perm = 0;
		p_ptr->shield_perm = 0;
		p_ptr->stability_perm = 0;
		p_ptr->tim_bravery_perm = 0;
		p_ptr->sp_dur_perm = 0;
		p_ptr->tim_sp_dam_perm = 0;
		p_ptr->tim_sp_inf_perm = 0;

		/* Handle temporary, non-recent blessings, and shapeshifting back to original form */
		if ((p_ptr->shape_timer == 0) && (p_ptr->shape > 0) && (rand_int(100) < 50))
		{
			p_ptr->shape = 0;
			message(MSG_GENERIC, 0, "You return to your original form.");
		}
		if ((p_ptr->obsession_status == 3) && (rand_int(100) < 50))
		{
			p_ptr->obsession_status = 1;
			message(MSG_GENERIC, 0, "Beleth withdraws her blessings.");
		}
		if ((p_ptr->conflict_status == 3) && (rand_int(100) < 50))
		{
			p_ptr->conflict_status = 1;
			message(MSG_GENERIC, 0, "Discordia withdraws her blessings.");
		}
		if ((p_ptr->purity_status == 3) && (rand_int(100) < 50))
		{
			p_ptr->purity_status = 1;
			message(MSG_GENERIC, 0, "Eostre withdraws her blessings.");
		}
		if ((p_ptr->transformation_status == 3) && (rand_int(100) < 50))
		{
			p_ptr->transformation_status = 1;
			message(MSG_GENERIC, 0, "Cyrridven withdraws her blessings.");
		}
		if ((p_ptr->deceit_status == 3) && (rand_int(100) < 50))
		{
			p_ptr->deceit_status = 1;
			message(MSG_GENERIC, 0, "Laverna withdraws her blessings.");
		}

		/* Recent blessing are not so recent anymore */
		if (p_ptr->obsession_status == 2) p_ptr->obsession_status = 3;
		if (p_ptr->conflict_status == 2) p_ptr->conflict_status = 3;
		if (p_ptr->purity_status == 2) p_ptr->purity_status = 3;
		if (p_ptr->transformation_status == 2) p_ptr->transformation_status = 3;
		if (p_ptr->deceit_status == 2) p_ptr->deceit_status = 3;

		/* Handle temporary, non-recent curses */
		if ((p_ptr->obsession_status == 8) && (rand_int(100) < 50))
		{
			p_ptr->obsession_status = 4;
			message(MSG_GENERIC, 0, "Beleth is not so angry anymore.");
		}
		if ((p_ptr->conflict_status == 8) && (rand_int(100) < 50))
		{
			p_ptr->conflict_status = 4;
			message(MSG_GENERIC, 0, "Discordia is not so angry anymore.");
		}
		if ((p_ptr->purity_status == 8) && (rand_int(100) < 50))
		{
			p_ptr->purity_status = 4;
			message(MSG_GENERIC, 0, "Eostre is not so angry anymore.");
		}
		if ((p_ptr->transformation_status == 8) && (rand_int(100) < 50))
		{
			p_ptr->transformation_status = 4;
			message(MSG_GENERIC, 0, "Cyrridven is not so angry anymore.");
		}
		if ((p_ptr->deceit_status == 8) && (rand_int(100) < 50))
		{
			p_ptr->deceit_status = 4;
			message(MSG_GENERIC, 0, "Laverna is not so angry anymore.");
		}

		/* Recent curses are not so recent anymore */
		if (p_ptr->obsession_status == 7) p_ptr->obsession_status = 8;
		if (p_ptr->conflict_status == 7) p_ptr->conflict_status = 8;
		if (p_ptr->purity_status == 7) p_ptr->purity_status = 8;
		if (p_ptr->transformation_status == 7) p_ptr->transformation_status = 8;
		if (p_ptr->deceit_status == 7) p_ptr->deceit_status = 8;

		/* Count down shape timer */
		if (p_ptr->shape_timer > 0) p_ptr->shape_timer -= 1;

		/* If you make a Mapping check, you start standing on a stair */
		if (rand_int(100) < p_ptr->skill[SK_MAP])
		{
			p_ptr->create_up_stair = TRUE;
		}
		else
		{
			p_ptr->create_up_stair = FALSE;
			message(MSG_GENERIC, 0, "You enter the confusing maze of down staircases.");

			/* Reset the mapping bonus */
			p_ptr->mapping_bonus = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	/* Try activating a trap */
	else if (cave_t_idx[p_ptr->py][p_ptr->px])
	{
		hit_trap(p_ptr->py, p_ptr->px);
	}

	else 
	{
		message(MSG_FAIL, 0, "I see no down staircase here.");
	}
}

/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Search */
	search();
}

/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
	/* Stop searching */
	if (p_ptr->searching)
	{
		/* Clear the searching flag */
		p_ptr->searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);


		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start searching */
	else
	{
		/* Set the searching flag */
		p_ptr->searching = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
}

/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int y, int x)
{
	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Require open space */
	if (!cave_floor_bold(y, x))
	{
		/* Rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE)
		{
			/* Message */
			if decoration(y, x)
			{
				message(MSG_HITWALL, 0, "There is a tree blocking you way.");
			}

			else
			{
				message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
			}
		}

		/* Door */
		else if (cave_feat[y][x] == FEAT_CLOSED)
		{

			/* Hack -- Handle "easy_alter" */
			if (easy_alter) return (TRUE);

			/* Message */
			message(MSG_HITWALL, 0, "There is a door in the way!");
		}

		/* Door */
		else if ((cave_feat[y][x] == FEAT_CHEST) || (cave_feat[y][x] == FEAT_QST_CHEST))
		{
			/* Message */
			message(MSG_HITWALL, 0, "There is a chest in the way!");
		}

		/* Wall */
		else
		{
			/* Message */
			message(MSG_HITWALL, 0, "There is a wall in the way!");
		}

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for the "walk" and "jump" commands.
 */
static void do_cmd_walk_or_jump(int jumping)
{
	int y, x, dir;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Verify legality */
		if (!do_cmd_walk_test(y, x)) return;
	}

	/* There's a known trap in the target square */
	else if (w_info[t_list[cave_t_idx[y][x]].w_idx].flags & WGF_PLAYER)
	{
		trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

		/* Confirm stepping on a known trap */
		/* Easy-disarm not on */
		/* Protection from traps not on */
		if ((t_ptr->visible) && (cave_m_idx[y][x] <= 0) && (!p_ptr->confused) && (!easy_alter) && (!p_ptr->safety))
		{
			if (!get_check("Do you really want to step on a trap? "))
			{
				p_ptr->energy_use = 0;
				return;
			}
		}
	}

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Move the player */
	move_player(dir, jumping);
}

/*
 * Walk into a grid.
 */
void do_cmd_walk(void)
{
	/* Move (normal) */
	do_cmd_walk_or_jump(FALSE);
}

/*
 * Jump into a grid.
 */
void do_cmd_jump(void)
{
	/* Move (jump) */
	do_cmd_walk_or_jump(TRUE);
}

/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
	int y, x, dir;

	/* Hack XXX XXX XXX */
	if (p_ptr->confused)
	{
		message(MSG_FAIL, 0, "You are too confused!");
		return;
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Start run */
	run_step(dir);
}

/*
 * Stay still.  Search.  Enter stores. Pick up treasure if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
{
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Spontaneous Searching -- now automatic */
	search();

	/* Continuous Searching */
	if (p_ptr->searching) search();

	/* Handle "objects" */
	py_pickup(pickup);

	/* Hack -- enter a store if we are on one */
	if ((cave_feat[p_ptr->py][p_ptr->px] >= FEAT_SHOP_HEAD) &&
	    (cave_feat[p_ptr->py][p_ptr->px] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';

		/* Free turn XXX XXX XXX */
		p_ptr->energy_use = 0;
	}
}

/*
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	do_cmd_hold_or_stay(always_pickup);
}

/*
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
	/* Stay still (usually do not pickup) */
	do_cmd_hold_or_stay(!always_pickup);
}

/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, h for HP, s for SP, * HP/SP, & full): ";

		char out_val[5];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, sizeof(out_val))) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->command_arg = (-2);
		}

		/* Rest hit points and spell points */
		else if (out_val[0] == '*')
		{
			p_ptr->command_arg = (-1);
		}

		/* Rest hit points */
		else if (out_val[0] == 'h')
		{
			p_ptr->command_arg = (-3);
		}

		/* Rest spell points */
		else if (out_val[0] == 's')
		{
			p_ptr->command_arg = (-4);
		}


		/* Rest some */
		else
		{
			p_ptr->command_arg = atoi(out_val);
			if (p_ptr->command_arg <= 0) return;
		}
	}

	/* Paranoia */
	if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;

	/* Take a turn XXX XXX XXX (?) */
	p_ptr->energy_use = 100;

	/* Save the rest code */
	p_ptr->resting = p_ptr->command_arg;

	/* Cancel the arg */
	p_ptr->command_arg = 0;

	/* Cancel searching */
	p_ptr->searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	if (fresh_before) Term_fresh();
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
	int tdam, tdis, thits;
	int chance;
	int critical_hit_chance = 0;
	u32b f1, f2, f3;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;
	bool ambush = FALSE;
	bool deadly_crit = FALSE;

	byte missile_attr;
	char missile_char;

	byte special = 0;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Extract the flags */
	object_flags(j_ptr, &f1, &f2, &f3);

	/* Require a usable launcher */
	if (!j_ptr->k_idx)
	{
		message(MSG_FAIL, 0, "You have nothing to fire with.");
		return;
	}

	/* Handle player fear */
	if (p_ptr->afraid > PY_FEAR_TERROR)
	{
		/* Message */
		message(MSG_FAIL, 0, "You are too afraid!");

		/* Done */
		return;
	}
	
	/* Require proper missile */
	item_tester_tval = TV_ARROW;

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

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Tripled critical damage? */
	if (f2 & TR2_DEADLY_CRIT) deadly_crit = TRUE;

	/* Single object */
	i_ptr->number = 1;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, (display_insc_msg ? 3 : 2));

	/* Calculate hit-bonus */
	chance = p_ptr->skill[SK_THB] + p_ptr->to_h_shooting + object_to_h(i_ptr) + object_to_h(j_ptr);

	/* Extract the "base range" */
	tdis = bow_range(j_ptr);

	/* Apply divine range bonus */
	tdis += p_ptr->range_bonus;

	/* Hack - Arrow pval is always a range bonus/penalty */
	tdis += i_ptr->pval;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir, chance, object_to_h(i_ptr) + object_to_h(j_ptr), tdis)) return;

	/* Nullify invisibility */
	if (p_ptr->invis) nullify_invis();

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

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from fired object plus launcher bonus */
	tdam = damroll(object_dd(i_ptr), object_ds(i_ptr));

	/* Boost the damage by multiplier */
	tdam *= bow_might(j_ptr);

	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);

	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay(tdis))
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, ty, tx, 0);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Starting point: 0 = low. stops at any high ground. 1 = high. flies over the first cover. */
	/* Note that standing just next to the cover is the optimal shooting position */

	int starting_point = 0;
	int end_point = 0;
	int xx, yy;

	/* Is the target standing on a table or a platform? */	
	if ((t_list[cave_t_idx[ty][tx]].w_idx == WG_TABLE) ||
		(t_list[cave_t_idx[ty][tx]].w_idx == WG_PLATFORM))
	{
		end_point = 1;
	}

	/* Is the target standing on an altar, next to a platform? */	
	if ((t_list[cave_t_idx[ty][tx]].w_idx >= WG_ALTAR_OBSESSION) &&
		(t_list[cave_t_idx[ty][tx]].w_idx <= WG_ALTAR_DECEIT))
	{
		for (xx = tx - 1; xx <= tx + 1; xx++)
		{
			for (yy = ty - 1; yy <= ty + 1; yy++)
			{
				if (t_list[cave_t_idx[yy][xx]].w_idx == WG_PLATFORM) end_point = 1;
			}
		}
	}

	/* Is the player standing on or next to a table or a platform? */
	for (xx = p_ptr->px - 1; xx <= p_ptr->px + 1; xx++)
	{
		for (yy = p_ptr->py - 1; yy <= p_ptr->py + 1; yy++)
		{
			if ((t_list[cave_t_idx[yy][xx]].w_idx == WG_TABLE) ||
				(t_list[cave_t_idx[yy][xx]].w_idx == WG_PLATFORM))
			{
				starting_point = 1;
			}
		}
	}

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Sometimes stop before hitting cover */
		if (end_point == 0)
		{
			if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_TABLE) ||
				(t_list[cave_t_idx[ny][nx]].w_idx == WG_PLATFORM))
			{
				if ((starting_point == 0) && (rand_int(5) < 2))
				{
					if (t_list[cave_t_idx[ny][nx]].w_idx == WG_TABLE)
					{
						message_format(MSG_SHOOT, o_ptr->k_idx, "The %s hits the table.", o_name);
					}
					else message_format(MSG_SHOOT, o_ptr->k_idx, "The %s hits the platform.", o_name);
	
					break;
				}

				/* Only one chance to hit any table or platform */
				else starting_point = 1;
			}

			/* Ignore altars (they may be either high or low) */
			else if ((t_list[cave_t_idx[ny][nx]].w_idx >= WG_ALTAR_OBSESSION) &&
				(t_list[cave_t_idx[ny][nx]].w_idx <= WG_ALTAR_DECEIT))
				{}

			/* Any missile only gets to ignore the first cover */
			else if (distance(p_ptr->py, p_ptr->px, ny, nx) > 7)
			{
				starting_point = 0;
			}
		}

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

			/* If the target was higher, fly over */
			if (end_point > starting_point)
			{
				if (!((t_list[cave_t_idx[y][x]].w_idx == WG_TABLE) ||
					(t_list[cave_t_idx[y][x]].w_idx == WG_PLATFORM)))
				{
					continue;
				}
			}

			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = get_monster_real(m_ptr);

			/* Range penalty is the square root of range, minus one */
			int range = distance(p_ptr->py, p_ptr->px, y, x);
			int chance2 = chance;
			if (range >= 25) chance2 -= 4;
			else if (range >= 16) chance2 -= 3;
			else if (range >= 9) chance2 -= 2;
			else if (range >= 4) chance2 -= 1;

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Check for evasion */
			if ((r_ptr->flags2 & RF2_EVASIVE) && (rand_int(4) == 0))
			{
				char m_name[80];

				/* Get "the monster" or "it" */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				message_format(MSG_MISS, m_ptr->r_idx,
					"%^s magically evades your %s!", m_name, o_name);

				/* Wake it up */
				m_ptr->sleep = 0;

				/* Anger it */
				m_ptr->calmed = 0;

				teleport_away(cave_m_idx[y][x], 4);
				lore_learn(m_ptr, LRN_FLAG2, RF2_EVASIVE, FALSE);

				/* Arrow continues */
				continue;
			}

			/* Did we hit it (penalize distance travelled) */
			critical_hit_chance = (test_hit(chance2, r_ptr->ac / (m_ptr->cursed + 1), m_ptr->ml));

			if (critical_hit_chance >= 0)
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if (!monster_alive(TRUE, m_ptr)) note_dies = " is destroyed.";

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					message_format(MSG_SHOOT, o_ptr->k_idx, "The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Message */
					message_format(MSG_SHOOT, o_ptr->k_idx, "The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_track(m_ptr->r_idx, m_ptr->u_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr, &special);

				/* Critical hits only against visible monsters not in melee range */
				if ((m_ptr->ml) && (distance(p_ptr->py, p_ptr->px, y, x) > 1))
				{
					/* Ambush criticals only against distracted monsters */
					if (m_ptr->blinded || m_ptr->confused || m_ptr->monfear || m_ptr->sleep)
					{
						ambush = TRUE;
					}

					tdam = critical_shot(i_ptr, &special, tdam, ambush, critical_hit_chance, deadly_crit);
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (cheat_wizard)
				{
					message_format(MSG_CHEAT, 0, "You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* HACK - Special - Force fear */
				if (special & SPEC_FEAR) 
				{
					fear = TRUE;
					special &= ~SPEC_FEAR;
				}
	
				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Handle special effects (confusing, stunning, etc */
					if (special) attack_special(cave_m_idx[y][x], special, tdam);

					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? k_info[i_ptr->k_idx].breakage : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x, FALSE);
}

/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
void do_cmd_throw(void)
{
	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdam, tdis;
	int critical_hit_chance = 0;
	int plev = p_ptr->lev;
	u32b f1, f2, f3;

	int mul, div;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;
	bool aware;
	bool ambush = FALSE;
	bool deadly_crit = FALSE;

	byte missile_attr;
	char missile_char;

	byte special = 0;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;

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

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Tripled critical damage? */
	if (f2 & TR2_DEADLY_CRIT) deadly_crit = TRUE;

	/* Distribute the charges of rods between the stacks */
	distribute_charges(o_ptr, i_ptr, 1);

	/* Single object */
	i_ptr->number = 1;

	/* Description */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, (display_insc_msg ? 3 : 2));

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Chance of hitting */
	chance = p_ptr->skill[SK_THT] + p_ptr->to_h_throwing + object_to_h(i_ptr);

	/* Extract a "distance multiplier" */
	mul = 7;

	/* Enforce a minimum "weight" of 3.5 pounds */
	div = ((object_weight(i_ptr) > 35) ? object_weight(i_ptr) : 35);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_stat(A_STR)] + 23) * mul / div;

	/* Max distance of 8 without divine range bonus */
	if (tdis > 7) tdis = 7;

	/* Apply divine range bonus */
	tdis += p_ptr->range_bonus;

	/* Apply Mighty Throw */
	if (p_ptr->mighty_throw) tdis *= 2;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir, chance, object_to_h(i_ptr), tdis)) return;

	/* Hack -- Base damage from thrown object */
	if (!(i_ptr->tval == TV_BOW) && !(i_ptr->tval == TV_ARROW) && (object_weight(i_ptr) <= 100) && (object_weight(i_ptr) <= (10 * p_stat(A_STR))))
	{
		tdam = damroll(object_dd(i_ptr), object_ds(i_ptr));

		/* High strength gives damage multipliers */
		if (p_stat(A_STR) >= 29) tdam = tdam * 6;
		else if (p_stat(A_STR) >= 25) tdam = tdam * 5;
		else if (p_stat(A_STR) >= 21) tdam = tdam * 4;
		else if (p_stat(A_STR) >= 17) tdam = tdam * 3;
		else if (p_stat(A_STR) >= 13) tdam = tdam * 2;

		/* Berserk strength doubles damage */
		if (p_ptr->rage) tdam = tdam * 2;

		/* Mighty Throw doubles damage */
		if (p_ptr->mighty_throw) tdam *= 2;
	}

	else tdam = damroll(0, 0);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay(tdis))
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, ty, tx, 0);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Starting point: 0 = low. stops at any high ground. 1 = high. flies over the first cover. */
	/* Note that standing just next to the cover is the optimal shooting position. */

	int starting_point = 0;
	int end_point = 0;
	int xx, yy;

	/* Is the target standing on a table or a platform? */	
	if ((t_list[cave_t_idx[ty][tx]].w_idx == WG_TABLE) ||
		(t_list[cave_t_idx[ty][tx]].w_idx == WG_PLATFORM))
	{
		end_point = 1;
	}

	/* Is the target standing on an altar, next to a platform? */	
	if ((t_list[cave_t_idx[ty][tx]].w_idx >= WG_ALTAR_OBSESSION) &&
		(t_list[cave_t_idx[ty][tx]].w_idx <= WG_ALTAR_DECEIT))
	{
		for (xx = tx - 1; xx <= tx + 1; xx++)
		{
			for (yy = ty - 1; yy <= ty + 1; yy++)
			{
				if (t_list[cave_t_idx[yy][xx]].w_idx == WG_PLATFORM) end_point = 1;
			}
		}
	}

	/* Or is the player standing on or next to a table or platform? */
	for (xx = p_ptr->px - 1; xx <= p_ptr->px + 1; xx++)
	{
		for (yy = p_ptr->py - 1; yy <= p_ptr->py + 1; yy++)
		{
			if ((t_list[cave_t_idx[yy][xx]].w_idx == WG_TABLE) ||
				(t_list[cave_t_idx[yy][xx]].w_idx == WG_PLATFORM))
			{
				starting_point = 1;
			}
		}
	}

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Sometimes stop before hitting cover */
		if (end_point == 0)
		{
			if ((t_list[cave_t_idx[ny][nx]].w_idx == WG_TABLE) ||
				(t_list[cave_t_idx[ny][nx]].w_idx == WG_PLATFORM))
			{
				if ((starting_point == 0) && (rand_int(5) < 2))
				{
					if (t_list[cave_t_idx[ny][nx]].w_idx == WG_TABLE)
					{
						message_format(MSG_SHOOT, o_ptr->k_idx, "The %s hits the table.", o_name);
					}
					else message_format(MSG_SHOOT, o_ptr->k_idx, "The %s hits the platform.", o_name);

					break;
				}

				/* Only one chance to hit any table or platform */
				else starting_point = 1;
			}

			/* Ignore altars (they may be either high or low) */
			else if ((t_list[cave_t_idx[ny][nx]].w_idx >= WG_ALTAR_OBSESSION) &&
				(t_list[cave_t_idx[ny][nx]].w_idx <= WG_ALTAR_DECEIT))
				{}

			/* Any missile only gets to ignore the first cover */
			else starting_point = 0;
		}

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
			/* If the target was higher, fly over */
			if (end_point > starting_point)
			{
				if (!((t_list[cave_t_idx[y][x]].w_idx == WG_TABLE) ||
					(t_list[cave_t_idx[y][x]].w_idx == WG_PLATFORM)))
				{
					continue;
				}
			}

			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = get_monster_real(m_ptr);

			/* Range penalty is the square root of range, minus one */
			int range = distance(p_ptr->py, p_ptr->px, y, x);
			int chance2 = chance;
			if (range >= 25) chance2 -= 4;
			else if (range >= 16) chance2 -= 3;
			else if (range >= 9) chance2 -= 2;
			else if (range >= 4) chance2 -= 1;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			critical_hit_chance = (test_hit(chance2, r_ptr->ac / (m_ptr->cursed + 1), m_ptr->ml));

			if (critical_hit_chance >= 0)
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if (!monster_alive(TRUE, m_ptr)) note_dies = " is destroyed.";

				/* Handle unseen monster */
				if (!m_ptr->ml)
				{
					/* Invisible monster */
					message_format(MSG_THROW, o_ptr->k_idx, "The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Message */
					message_format(MSG_THROW, o_ptr->k_idx, "The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_track(m_ptr->r_idx, m_ptr->u_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				if	(i_ptr->tval == TV_POWDER)
				{
					aware = FALSE;

					int spread = 0;

					/* Critical hits only against visible monsters not in melee range */
					if ((m_ptr->ml) && (distance(p_ptr->py, p_ptr->px, y, x) > 1))
					{
						/* Ambush criticals only against distracted monsters */
						if (m_ptr->blinded || m_ptr->confused || m_ptr->monfear || m_ptr->sleep)
						{
							ambush = TRUE;
						}

						spread = critical_powder(ambush, critical_hit_chance);
					}

					switch (i_ptr->sval)
					{
						case SV_POWDER_SLEEP:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_SLEEP_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_CONFUSE:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_CONF_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_STARTLE:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_SCARE_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_FLASH:
						{
							message(MSG_EFFECT, 0, "The powder bursts in a bright flash of light.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_BLIND_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread);
							(void)strike(GF_LITE_WEAK, y, x, damroll(3 , 16), spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_DARKNESS:
						{
							message(MSG_EFFECT, 0, "The powder bursts into a sinister cloud of darkness.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_SCARE_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread);
							(void)strike(GF_DARK_WEAK, y, x, damroll(3 , 16), spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_FIRE1:
						{
							message(MSG_EFFECT, 0, "The powder bursts in a firey explosion.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_FIRE, y, x, damroll(3 , 16), spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_FIRE2:
						{
							message(MSG_EFFECT, 0, "The powder bursts in a firey inferno.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_FIRE, y, x, damroll(8 , 30), 2 + spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_COLD1:
						{
							message(MSG_EFFECT, 0, "The powder bursts into an icy mist.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_COLD, y, x, damroll(3 , 16), spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_COLD2:
						{
							message(MSG_EFFECT, 0, "The powder bursts in an explosion of frost.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_ICE , y, x, damroll(8 , 30), 2 + spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_ENERGY:
						{
							message(MSG_EFFECT, 0, "The powder bursts in an explosion of pure energy.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_MANA, y, x, damroll(10 , 60), 2 + spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_POISON:
						{
							message(MSG_EFFECT, 0, "The powder bursts into noxius vapours.");
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							(void)strike(GF_POIS, y, x, damroll(3 , 14), spread);
							aware = TRUE;
							break;
						}
						case SV_POWDER_HASTE:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_SPEED_ALL, y, x, 0, spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_HEAL:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_HEAL_ALL, y, x, damroll(4, 8), spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_SLOW:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_SLOW_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_CALM:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_CALM_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread)) aware = TRUE;
							break;
						}
						case SV_POWDER_POLYMORPH:
						{
							if (spread) message(MSG_EFFECT, 0, "The cloud spreads.");
							if (strike(GF_POLY_ALL, y, x, ((plev > 12) ? plev : 6 + plev/2), spread)) aware = TRUE;
							break;
						}
					}

					if ((!object_aware_p(i_ptr)) && aware)
					{
						object_aware(i_ptr);

						/* Squelch */
						if (squelch_itemp(i_ptr)) do_squelch_item(i_ptr);
					}

					/* Combine / Reorder the pack (later) */
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);
				}

				else if	(i_ptr->tval == TV_FLASK)
				{
					aware = FALSE;
					int spread = 0;
					if (1 > rand_int(3)) spread = 1;

					/* Critical hits only against visible monsters not in melee range */
					if ((m_ptr->ml) && (distance(p_ptr->py, p_ptr->px, y, x)))
					{
						/* Ambush criticals only against distracted monsters */
						if (m_ptr->blinded || m_ptr->confused || m_ptr->monfear || m_ptr->sleep)
						{
							ambush = TRUE;
						}

						spread = critical_powder(ambush, critical_hit_chance);
					}

					switch (i_ptr->sval)
					{
						case SV_FLASK_LANTERN:
						{
							message(MSG_EFFECT, 0, "The oil ignites.");
							if (spread) message(MSG_EFFECT, 0, "The oil slick spreads.");
							(void)strike(GF_FIRE, y, x, damroll(2 , 4), spread);
							break;
						}
						case SV_FLASK_BURNING:
						{
							message(MSG_EFFECT, 0, "The oil burns with intense flames.");
							if (spread) message(MSG_EFFECT, 0, "The oil slick spreads.");
							(void)strike(GF_FIRE, y, x, damroll(3 , 10), spread);
							break;
						}
					}
				}
				else
				{
					/* Apply special damage XXX XXX XXX */
					tdam = tot_dam_aux(i_ptr, tdam, m_ptr, &special);

					/* Critical hits only against visible monsters not in melee range */
					if ((m_ptr->ml) && (distance(p_ptr->py, p_ptr->px, y, x)))
					{
						/* Ambush criticals only against distracted monsters */
						if (m_ptr->blinded || m_ptr->confused || m_ptr->monfear || m_ptr->sleep)
						{
							ambush = TRUE;
						}

						tdam = critical_throw(i_ptr, tdam, ambush, critical_hit_chance, deadly_crit);
					}

					/* No negative damage */
					if (tdam < 0) tdam = 0;

					/* Complex message */
					if (cheat_wizard)
					{
						message_format(MSG_CHEAT, 0, "You do %d (out of %d) damage.",
								   tdam, m_ptr->hp);
					}

					/* HACK - Special - Force fear */
					if (special & SPEC_FEAR) 
					{
						fear = TRUE;
						special &= ~SPEC_FEAR;
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

						/* Handle special effects (confusing, stunning, etc */
						if (special) attack_special(cave_m_idx[y][x], special, tdam);

						/* Take note */
						if (fear && m_ptr->ml)
						{
							char m_name[80];

							/* Get the monster name (or "it") */
							monster_desc(m_name, sizeof(m_name), m_ptr, 0);

							/* Message */
							message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
						}			
					}
				}
			}
			else
			{
				char m_name[80];

				/* Get the monster name (or "it") */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			}

			/* Stop looking */
			break;
		}
	}

	p_ptr->window |= (PW_INVEN | PW_EQUIP);

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

	/* Chance of breakage (during attacks) */
	j = (hit_body ? k_info[i_ptr->k_idx].breakage : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x, FALSE);
}
