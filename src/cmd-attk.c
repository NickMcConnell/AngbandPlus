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
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
static bool test_hit_combat(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return ((k < 5) ? TRUE : FALSE);

	/* No point in continuing if you don't have the skill */
	if (chance <= 0) return (FALSE);

	/* Basic chance to hit */
	chance *= 4;

	/* Invisible monsters are harder to hit */
	if (!vis) ac *= 2;

	/* High ACs are harder to hit */
	if (ac > 5 + (chance / 2)) chance -= (chance / 4);

	/* No point in continuing if you don't have the skill */
	if (!chance) return (FALSE);

	/* Power competes against armor */
	if (rand_int(chance) >= ac) return (TRUE);

	/* Assume miss */
	return (FALSE);
}

/*
 * Critical hits (from objects fired by player)
 * Factor in item weight, total plusses, and player level.
 */
static sint critical_shot(object_type *o_ptr, byte *special, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = ((p_ptr->to_h + actual_to_h(o_ptr)) * 4) + (p_ptr->lev * 2);

	/* Improved critical hits for some classes */
	if (cp_ptr->flags & CF_BETTER_SHOT) i += 10 + (p_ptr->lev * 2);

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = p_ptr->lev + randint(500);

		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_SHOT) k *= 2;

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
	
		if ((o_ptr->tval == TV_ARROW) || (o_ptr->tval == TV_BOLT)) (*special) |= SPEC_CUT;
		if (o_ptr->tval == TV_SHOT) (*special) |= SPEC_STUN;
	}

	return (dam);
}

/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
static sint critical_throw(object_type *o_ptr, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = ((p_ptr->to_h + actual_to_h(o_ptr)) * 4) + (p_ptr->lev * 2);

	/* Improved critical hits for some classes */
	if (cp_ptr->flags & CF_BETTER_THROW) i += 10 + (p_ptr->lev * 2);

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = p_ptr->lev + randint(500);
		
		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_THROW) k *= 2;

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
	}

	return (dam);
}

/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
static sint critical_norm(object_type *o_ptr, byte *special, int dam)
{
	int i, k;

	/* No critical hit with "bad" weapons */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (p_ptr->icky_wield)) return (dam);
	
	/* Extract "blow" power */
	i = ((p_ptr->to_h + actual_to_h(o_ptr)) * 6) + (p_ptr->lev * 4);

	/* Improved critical hits for blessed blades */
	if (p_ptr->bless_blade) i += 100;

	/* Improved critical hits for some classes */
	if (cp_ptr->flags & CF_BETTER_CRITICAL) i += 50 + (p_ptr->lev * 4);

	/* Chance */
	if (randint(5000) <= i)
	{
		k = ((p_ptr->to_h + actual_to_h(o_ptr)) * 4) + (p_ptr->lev * 2) + randint(600);

		/* Improved critical hits for some classes */
		if (cp_ptr->flags & CF_BETTER_CRITICAL) k *= 2;

		/* Improved critical hits for blessed blades */
		if (p_ptr->bless_blade) k += 100;
		
		if (k < 400)
		{
			message(MSG_CRITICAL_HIT, 0,"It was a good hit!");
			if (o_ptr->tval == TV_POLEARM) dam = 2 * dam + 5;
			else if (o_ptr->tval == TV_SWORD) dam = ((6 * dam) / 5) + 5;
		}
		else if (k < 700)
		{
			message(MSG_CRITICAL_HIT, 0, "It was a great hit!");
			if (o_ptr->tval == TV_POLEARM) dam = ((11 * dam) / 5) + 10;
			else if (o_ptr->tval == TV_SWORD) dam = ((3 * dam) / 2) + 5;
			else dam = dam + 5;
		}
		else if (k < 900)
		{
			message(MSG_CRITICAL_HIT, 0, "It was a superb hit!");
			if (o_ptr->tval == TV_POLEARM) dam = 3 * dam + 15;
			else if (o_ptr->tval == TV_SWORD) dam = ((8 * dam) / 5) + 8;
			else dam = ((3 * dam) / 2) + 8;
		}
		else if (k < 1300)
		{
			message(MSG_CRITICAL_HIT, 0, "It was a *GREAT* hit!");
			if (o_ptr->tval == TV_POLEARM) dam = ((16 * dam) / 5) + 20;
			else if (o_ptr->tval == TV_SWORD) dam = 2 * dam + 13;
			else dam = ((8 * dam) / 5) + 13;

			if ((o_ptr->tval == TV_HAFTED) && (rand_int(100) < 10)) (*special) |= SPEC_CONF;
		}
		else
		{
			message(MSG_CRITICAL_HIT, 0, "It was a *SUPERB* hit!");
			if (o_ptr->tval == TV_POLEARM) dam = ((7 * dam) / 2) + 25;
			if (o_ptr->tval == TV_SWORD) dam = ((12 * dam) / 5) + 16;
			else dam = 2 * dam + 16;

			if ((o_ptr->tval == TV_HAFTED) && (rand_int(100) < 25)) (*special) |= SPEC_CONF;
		}
	
		if (o_ptr->tval == TV_SWORD) (*special) |= SPEC_CUT;
		if (o_ptr->tval == TV_HAFTED) (*special) |= SPEC_STUN;
	}

	return (dam);
}

/*
 * Apply the special effects of an attack
 */
static void attack_special(int m_idx, byte special, int dam)
{
	char m_name[80];
	
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

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

			m_ptr->bleeding += dam*2;
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

			m_ptr->poisoned += dam;
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

			m_ptr->blinded += 1 + dam/3 + rand_int(dam)/3;
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

			m_ptr->stunned += 10 + rand_int(dam) / 5;
		}
	}

	/* Special - Confuse monster */
	if (special & SPEC_CONF) 
	{
		/* Confuse the monster */
		if (r_ptr->flags3 & (RF3_RES_CONF)) lore_learn(m_ptr, LRN_FLAG3, RF3_RES_CONF, FALSE);
		else if (rand_int(100) >= r_ptr->level)
		{
			/* Already partially confused */
			if (m_ptr->confused) message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears more confused.", m_name);
			/* Was not stunned */
			else message_format(MSG_SUCCEED, m_ptr->r_idx, "%^s appears confused.", m_name);

			m_ptr->confused += 10 + rand_int(dam) / 5;
		}
	}
}

/*
 * Extract the "total damage" from a given object hitting a given monster.
 */
static sint tot_dam_aux(object_type *o_ptr, int tdam, monster_type *m_ptr, byte *special)
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
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Extract slays */
			weapon_slays(o_ptr, slays);

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

			/* Slay Chaotic */
			if ((slays[SL_ANTI_CHAOS] > 10) && (r_ptr->flags4 & (RF4_CHAOTIC)))
			{
				lore_learn(m_ptr, LRN_FLAG4, RF4_CHAOTIC, FALSE);
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
			if (slays[SL_BRAND_ACID] > 10)
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

	/* Return the total damage */
	return ((tdam * mult) / 10);
}

/*
 * Search for hidden things
 */
void search(void)
{
	int y, x, chance;

	/* Start with base search ability */
	chance = p_ptr->skill[SK_SRH];

	/* Penalize various conditions */
	if (p_ptr->blind || no_lite()) chance = chance / 10;
	if (p_ptr->confused || p_ptr->image) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (p_ptr->py - 1); y <= (p_ptr->py + 1); y++)
	{
		for (x = (p_ptr->px - 1); x <= (p_ptr->px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Secret door */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Message */
					message(MSG_FIND, 0, "You have found a secret door.");

					/* Create closed door */
					cave_set_feat(y, x, FEAT_CLOSED);

					if (trap_lock(y, x)) t_list[cave_t_idx[y][x]].visible = TRUE;

					/* Disturb */
					disturb(0);
				}
	
				/* Discover invisible traps */
				else if (trap_detectable(y, x))
				{
					trap_type *t_ptr = &t_list[cave_t_idx[y][x]];
		
					if (!t_ptr->visible)
					{
						/* Message */
						message(MSG_FIND, 0, "You found a trap!");

						t_ptr->visible = TRUE;

						/* Show trap */
						lite_spot(y, x);

						/* Disturb */
						disturb(0);
					}
				}
			}
		}
	}
}

/*
 * Determine if the object can be picked up, and has "=g" in its inscription.
 */
static bool auto_pickup_okay(object_type *o_ptr)
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
	object_desc(o_name, o_ptr, TRUE, (display_insc_msg ? 3 : 2));

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
		object_desc(o_name, o_ptr, TRUE, (display_insc_msg ? 3 : 2));

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
			message_format(MSG_PICKUP, 0, "You have found %ld gold pieces worth of %s.",
			           (long)o_ptr->pval, o_name);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

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
			j += (o_ptr->number * actual_weight(o_ptr));

			/* Apply encumbarance from weight */
			if (j > i/2) new_enc = ((j - (i/2)) / (i / 10));

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
			object_desc(o_name, o_ptr, TRUE, 3);

			if (!heavy) sprintf(out_val, "Pick up %s? [%s] ", o_name, keys);
			else sprintf (out_val, "Pick up %s (heavy)? [%s] ", o_name, keys);
			
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
					object_desc(o_name, o_ptr, TRUE, (display_insc_msg ? 3 : 2));

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
				object_desc(o_name, o_ptr, TRUE, (display_insc_msg ? 3 : 2));

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
				object_desc(o_name, o_ptr, TRUE, (display_insc_msg ? 3 : 2));

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
void py_attack(int y, int x)
{
	int num = 0;
	int k, chance;

	monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
	monster_race *r_ptr = get_monster_real(m_ptr);

	object_type *o_ptr;

	char m_name[80];

	byte special = 0;

	bool fear = FALSE;
	bool do_quake = FALSE;

	/* Disturb the player */
	disturb(0);

	/* Disturb the monster */
	m_ptr->csleep = 0;
	
	/* Anger the monster */
	m_ptr->calmed = 0;

	/* Nullify invisiblity */
	if (p_ptr->invis) nullify_invis();

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_track(m_ptr->r_idx, m_ptr->u_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);

	/* Handle player fear */
	if (p_ptr->afraid > PY_FEAR_AFRAID)
	{
		/* Message */
		message_format(MSG_FAIL, 0, "You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}

	/* Get the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Calculate the "attack quality" */
	chance = p_ptr->skill[SK_THN] + p_ptr->to_h + actual_to_h(o_ptr);

	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
		/* Check for evasion */
		if ((r_ptr->flags2 & RF2_EVASIVE) && (rand_int(3) == 0))
		{
			message_format(MSG_MISS, m_ptr->r_idx, "%^s magically evades your blow!", m_name);

			/* Wake it up */
			m_ptr->csleep = 0;

			/* Anger it */
			m_ptr->calmed = 0;

			teleport_away(cave_m_idx[y][x], 4);
			lore_learn(m_ptr, LRN_FLAG2, RF2_EVASIVE, FALSE);
			break;
		}

		/* Test for hit */
		if (test_hit_combat(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Message */
			message_format(MSG_HIT, m_ptr->r_idx, "You hit %s.", m_name);

			k = 1;

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				u32b f1, f2, f3;

				/* Extract the flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				k = damroll(actual_dd(o_ptr), actual_ds(o_ptr)) + actual_to_d(o_ptr) + p_ptr->to_d;

				/* Always some damage per hit */
				if (k < 1) k = 1;

				k = tot_dam_aux(o_ptr, k, m_ptr, &special);
				if ((f2 & TR2_IMPACT) && (k > 50)) do_quake = TRUE;
				k = critical_norm(o_ptr, &special, k);
			}
			/* Hack -- Bare hands */
			else k = 1 + p_ptr->to_d;

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
			/* Message */
			message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
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

	/* Find the result of moving */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
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
				message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
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
				message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
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

		/* Spontaneous Searching */
		if ((p_ptr->skill[SK_FOS] >= 50) ||
		    (0 == rand_int(50 - p_ptr->skill[SK_FOS])))
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
				/* Message */
				message(MSG_FIND, 0, "You found a trap!");

				t_ptr->visible = TRUE;

				/* Hit the trap */
				hit_trap(y, x);
			}
			else
			{
				int i, diff;

				trap_widget *w_ptr = &w_info[t_ptr->w_idx];

				/* Get the "bypass" factor */
				i = p_ptr->skill[SK_BYP];

				/* Penalize some conditions */
				if (p_ptr->blind || no_lite()) i = i / 10;
				if (p_ptr->confused || p_ptr->image) i = i / 7;

				/* Extract the difficulty */
				diff = 1 + (w_ptr->bypass_factor * p_ptr->depth) / 100;

				/* Extract power */
				i -= diff;

				/* Attempt bypass */
				if (rand_int(100) < i)
				{
					message_format(MSG_RESIST, 0, "You have avoided the %s!", w_name + w_ptr->name);
				}
				else 
				{
					/* Hit the trap */
					hit_trap(y, x);
				}
			}
		}
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
			monster_type *m_ptr = &m_list[cave_m_idx[row][col]];

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

	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_LESS)
	{
		message(MSG_FAIL, 0, "I see no up staircase here.");
		return;
	}

	/* Verify leaving quest level */
	if ((verify_leave_quest) && (quest_check(p_ptr->depth) == QUEST_GUILD))
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

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* New depth */
	p_ptr->depth--;

	/* Leaving */
	p_ptr->leaving = TRUE;
}

/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	char out_val[160];
	
	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] == FEAT_MORE)
	{
		/* Verify leaving quest level */
		if ((verify_leave_quest) && (quest_check(p_ptr->depth) == QUEST_GUILD))
		{
			sprintf(out_val, "Really risk failing your quest? ");
			if (!get_check(out_val)) return;
		}

		/* Hack -- take a turn */
		p_ptr->energy_use = 100;

		/* Success */
		message(MSG_STAIRS, 0, "You enter a maze of down staircases.");

		/* Create a way back */
		p_ptr->create_up_stair = TRUE;

		/* New level */
		p_ptr->depth++;

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
			message(MSG_HITWALL, 0, "There is a pile of rubble in the way!");
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
		else if (cave_feat[y][x] == FEAT_CHEST)
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
	}

	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

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

	/* Spontaneous Searching */
	if ((p_ptr->skill[SK_FOS] >= 50) || (0 == rand_int(50 - p_ptr->skill[SK_FOS])))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}

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

		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 5)) return;

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
	int tdam, tdis, thits, tmul;
	int chance;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

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

	/* Require a usable launcher */
	if (!j_ptr->tval || !p_ptr->ammo_tval)
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

	/* Nullify invisiblity */
	if (p_ptr->invis) nullify_invis();

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Single object */
	i_ptr->number = 1;

	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, (display_insc_msg ? 3 : 2));

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
	tdam = damroll(actual_dd(i_ptr), actual_ds(i_ptr)) + actual_to_d(i_ptr) + actual_to_d(j_ptr);

	/* Actually "fire" the object */
	chance = p_ptr->skill[SK_THB] + p_ptr->to_h + actual_to_h(i_ptr) + actual_to_h(j_ptr);

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range XXX XXX */
	tdis = 10 + 5 * tmul;

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
			monster_race *r_ptr = get_monster_real(m_ptr);

			int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Check for evasion */
			if ((r_ptr->flags2 & RF2_EVASIVE) && (rand_int(4) == 0))
			{
				char m_name[80];

				/* Get "the monster" or "it" */
				monster_desc(m_name, m_ptr, 0);

				message_format(MSG_MISS, m_ptr->r_idx,
					"%s magically evades your %s!", m_name, o_name);

				/* Wake it up */
				m_ptr->csleep = 0;

				/* Anger it */
				m_ptr->calmed = 0;

				teleport_away(cave_m_idx[y][x], 4);
				lore_learn(m_ptr, LRN_FLAG2, RF2_EVASIVE, FALSE);

				/* Arrow continues */
				continue;
			}

			/* Did we hit it (penalize distance travelled) */
			if (test_hit_combat(chance2, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags4 & (RF4_DEMON)) ||
				    (r_ptr->flags4 & (RF4_UNDEAD)) ||
				    (r_ptr->flags4 & (RF4_PLANT)) ||
				    (r_ptr->flags1 & (RF1_STUPID)) ||
				    (strchr("Evg$|!?~=", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

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
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					message_format(MSG_SHOOT, o_ptr->k_idx, "The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_track(m_ptr->r_idx, m_ptr->u_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr, &special);
				tdam = critical_shot(i_ptr, &special, tdam);

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
						monster_desc(m_name, m_ptr, 0);

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
 */
void do_cmd_throw(void)
{
	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdam, tdis;
	int mul, div;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;
	bool aware;

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

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Distribute the charges of rods between the stacks */
	distribute_charges(o_ptr, i_ptr, 1);

	/* Single object */
	i_ptr->number = 1;

	/* Description */
	object_desc(o_name, i_ptr, FALSE, (display_insc_msg ? 3 : 2));

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((actual_weight(i_ptr) > 10) ? actual_weight(i_ptr) : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_stat(A_STR)] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(actual_dd(i_ptr), actual_ds(i_ptr)) + actual_to_d(i_ptr);

	/* Chance of hitting */
	chance = p_ptr->skill[SK_THT] + p_ptr->to_h;

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
			monster_race *r_ptr = get_monster_real(m_ptr);

			int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_combat(chance2, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags4 & (RF4_DEMON)) ||
				    (r_ptr->flags4 & (RF4_UNDEAD)) ||
				    (r_ptr->flags4 & (RF4_PLANT)) ||
				    (r_ptr->flags1 & (RF1_STUPID)) ||
				    (strchr("Evg$|!?~=", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					message_format(MSG_THROW, o_ptr->k_idx, "The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

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
					switch (i_ptr->sval)
					{
						case SV_POWDER_SLEEP:
						{
							if (strike(GF_OLD_SLEEP, y, x, 20, 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_CONFUSE:
						{
							if (strike(GF_OLD_CONF, y, x, 20, 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_STARTLE:
						{
							if (strike(GF_TURN_ALL, y, x, 20, 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_FLASH:
						{
							message(MSG_EFFECT, 0, "The powder bursts in a bright flash of light.");
							(void)strike(GF_OLD_BLIND, y, x, 12, 0);
							(void)strike(GF_LITE_WEAK, y, x, damroll(5 , 7), 0);
							aware = TRUE;
							break;
						}
						case SV_POWDER_DARKNESS:
						{
							message(MSG_EFFECT, 0, "The powder bursts into a cloud of darkness.");
							(void)strike(GF_OLD_BLIND, y, x, 12, 0);
							(void)strike(GF_DARK_WEAK, y, x, damroll(5 , 7), 0);
							aware = TRUE;
							break;
						}
						case SV_POWDER_FIRE1:
						{
							message(MSG_EFFECT, 0, "The powder bursts in a firey explosion.");
							(void)strike(GF_FIRE, y, x, 2 * damroll(3 , 8), 0);
							aware = TRUE;
							break;
						}
						case SV_POWDER_FIRE2:
						{
							message(MSG_EFFECT, 0, "The powder bursts in a firey inferno.");
							(void)strike(GF_FIRE, y, x, 60 + 2 * damroll(8 , 8), 2);
							aware = TRUE;
							break;
						}
						case SV_POWDER_COLD1:
						{
							message(MSG_EFFECT, 0, "The powder bursts into an icy mist.");
							(void)strike(GF_COLD, y, x, 2 * damroll(3 , 8), 0);
							aware = TRUE;
							break;
						}
						case SV_POWDER_COLD2:
						{
							message(MSG_EFFECT, 0, "The powder bursts in an explosion of frost.");
							(void)strike(GF_ICE , y, x, 60 + 2 * damroll(8 , 8), 2);
							aware = TRUE;
							break;
						}
						case SV_POWDER_ENERGY:
						{
							message(MSG_EFFECT, 0, "The powder bursts in an explosion of pure energy.");
							(void)strike(GF_MANA, y, x, 100 + 4 * damroll(10 , 10), 2);
							aware = TRUE;
							break;
						}
						case SV_POWDER_POISON:
						{
							message(MSG_EFFECT, 0, "The powder bursts into noxius vapours.");
							(void)strike(GF_POIS, y, x, 2 * damroll(3 , 7), 0);
							aware = TRUE;
							break;
						}
						case SV_POWDER_HASTE:
						{
							if (strike(GF_OLD_SPEED, y, x, 0, 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_HEAL:
						{
							if (strike(GF_OLD_HEAL, y, x, damroll(4, 8), 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_SLOW:
						{
							if (strike(GF_OLD_SLOW, y, x, 20, 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_CALM:
						{
							if (strike(GF_OLD_CALM, y, x, 20, 0)) aware = TRUE;
							break;
						}
						case SV_POWDER_POLYMORPH:
						{
							if (strike(GF_OLD_POLY, y, x, 20, 0)) aware = TRUE;
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

				else
				{
					/* Apply special damage XXX XXX XXX */
					tdam = tot_dam_aux(i_ptr, tdam, m_ptr, &special);
					tdam = critical_throw(i_ptr, tdam);

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
							monster_desc(m_name, m_ptr, 0);

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
				monster_desc(m_name, m_ptr, 0);

				message_format(MSG_MISS, m_ptr->r_idx, "Your miss %s.", m_name);
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
	drop_near(i_ptr, j, y, x);
}
