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
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10)
		return (k < 5);

	/* Never hit */
	if (chance <= 0)
		return (FALSE);

	/* Invisible monsters are harder to hit */
	if (!vis)
		chance = (chance + 1) / 2;

	/* Power competes against armor */
	if (rand_int(chance) < (ac * 3 / 4))
		return (FALSE);

	/* Assume hit */
	return (TRUE);
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
	if (k < 10)
		return (k < 5);

	/* Wimpy attack never hits */
	if (chance <= 0)
		return (FALSE);

	/* Penalize invisible targets */
	if (!vis)
		chance = (chance + 1) / 2;

	/* Power must defeat armor */
	if (rand_int(chance) < (ac * 3 / 4))
		return (FALSE);

	/* Assume hit */
	return (TRUE);
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
s16b critical_shot(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = weight + randint(500);

		if (k < 500)
		{
			mprint(MSG_BONUS, "It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			mprint(MSG_BONUS, "It was a great hit!");
			dam = 2 * dam + 10;
		}
		else
		{
			mprint(MSG_BONUS, "It was a superb hit!");
			dam = 3 * dam + 15;
		}
	}

	return (dam);
}



/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 * AND monster sleep status - Added by GJW	-KMW-
 */
s16b critical_norm(int weight, int plus, int dam, int weap_tval,
	monster_type * m_ptr)
{
	int i, k, tx, ty;
	char m_name[80];

	monster_desc(m_name, m_ptr, 0);

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3));

	/* Chance */
	/* Rogues always get critical hits against sleeping foes. From GJW -KMW- */
	if ((p_ptr->pclass == CLASS_ROGUE && m_ptr->csleep > 0) ||
		(randint(5000) <= i))
	{
		k = weight + randint(650);

		if (k < 400)
		{
			mprint(MSG_BONUS, "It was a good hit!");
			dam = 2 * dam + 5;

			if (weap_tval == TV_HAFTED)
			{
				m_ptr->stunned += 10;
				msg_format("%^s looks dazed.", m_name);
			}
		}
		else if (k < 700)
		{
			mprint(MSG_BONUS, "It was a great hit!");
			dam = 2 * dam + 10;

			if (weap_tval == TV_HAFTED)
			{
				m_ptr->stunned += 50;
				m_ptr->confused += 50;
				msg_format("%^s is addled by your blow!", m_name);
			}
		}
		else if (k < 900)
		{
			mprint(MSG_BONUS, "It was a superb hit!");
			dam = 3 * dam + 15;

			if (weap_tval == TV_HAFTED)
			{
				ty = m_ptr->fy + (m_ptr->fy - p_ptr->py);
				tx = m_ptr->fx + (m_ptr->fx - p_ptr->px);

				if (cave_empty_bold(ty, tx))
				{
					monster_swap(m_ptr->fy, m_ptr->fx, ty, tx);
					msg_format("%^s is thrown back by your blow!", m_name);
				}
			}
		}
		else if (k < 1300)
		{
			mprint(MSG_BIG_BONUS, "It was a *GREAT* hit!");
			dam = 3 * dam + 20;

			if (weap_tval == TV_HAFTED)
			{
				m_ptr->stunned += 10;
				msg_format("%^s looks dazed.", m_name);

				ty = m_ptr->fy + (m_ptr->fy - p_ptr->py);
				tx = m_ptr->fx + (m_ptr->fx - p_ptr->px);

				if (cave_empty_bold(ty, tx))
				{
					monster_swap(m_ptr->fy, m_ptr->fx, ty, tx);
					msg_format("%^s is thrown back by your blow!", m_name);
				}
			}
		}
		else
		{
			mprint(MSG_BIG_BONUS, "It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;

			if (weap_tval == TV_HAFTED)
			{
				m_ptr->stunned += 50;
				m_ptr->confused += 50;
				msg_format("%^s is addled by your blow!", m_name);

				ty = m_ptr->fy + (m_ptr->fy - p_ptr->py);
				tx = m_ptr->fx + (m_ptr->fx - p_ptr->px);

				if (cave_empty_bold(ty, tx))
				{
					monster_swap(m_ptr->fy, m_ptr->fx, ty, tx);
					msg_format("%^s is thrown back by your blow!", m_name);
				}
			}
		}
	}

	return (dam);
}



/*
 * Compute the weapon damage based on the weapon's base damage.
 */
static s16b tot_dam_aux3(s16b k, s16b ac)
{
	int mul = 1, div = 1;

	/* Warriors don't damage their weapon as much as
	 * others. */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		div += (p_ptr->skill_thn / 50);
	}

	/* Strong characters will tend to break their weapon. */
	mul += (adj_str_hold[p_ptr->stat_ind[A_STR]] / 30);

	/* Soft monsters don't damage the weapon much. */
	mul += (ac / 25);

	return ((k * mul) / div);

}


/*
 * Check weapon type resists/immunities.
 */
static s16b tot_dam_aux2(s16b k, s16b weapon_tval, monster_race * r_ptr)
{
	/* Apply the player damage bonuses */
	k += p_ptr->to_d;

	/* Swords/Polearms are more random. */
	if (weapon_tval == TV_SWORD)
	{
		k += rand_spread(0, k / 3);
	}

	if (weapon_tval == TV_POLEARM)
	{
		k += rand_spread(0, k / 6);
	}

	/* No negative damage */
	if (k < 0)
		k = 0;

	/* Handle weapon type resistances. */

	if (weapon_tval == TV_HAFTED)
	{

		if (r_ptr->flags7 & RF7_RES_BASH)
		{
			k /= 2;
			r_ptr->r_flags7 |= RF7_RES_BASH;

		}
		else if (r_ptr->flags7 & RF7_IMM_BASH)
		{
			k = 0;
			r_ptr->r_flags7 |= RF7_IMM_BASH;
		}

	}
	else if (weapon_tval == TV_POLEARM)
	{

		if (r_ptr->flags7 & RF7_RES_STAB)
		{
			k /= 2;
			r_ptr->r_flags7 |= RF7_RES_STAB;

		}
		else if (r_ptr->flags7 & RF7_IMM_STAB)
		{
			k = 0;
			r_ptr->r_flags7 |= RF7_IMM_STAB;
		}

	}
	else if (weapon_tval == TV_SWORD)
	{

		if (r_ptr->flags7 & RF7_RES_SLASH)
		{
			k /= 2;
			r_ptr->r_flags7 |= RF7_RES_SLASH;

		}
		else if (r_ptr->flags7 & RF7_IMM_SLASH)
		{
			k = 0;
			r_ptr->r_flags7 |= RF7_IMM_SLASH;
		}

	}

	return k;
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
s16b tot_dam_aux(object_type * o_ptr, int tdam, monster_type * m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

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
			if ((f1 & (TR1_SLAY_ANIMAL)) && (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ANIMAL);
				}

				if (mult < 2)
					mult = 2;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) && (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_EVIL);
				}

				if (mult < 2)
					mult = 2;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) && (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				if (mult < 3)
					mult = 3;
			}

			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) && (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DEMON);
				}

				if (mult < 3)
					mult = 3;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) && (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ORC);
				}

				if (mult < 3)
					mult = 3;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) && (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_TROLL);
				}

				if (mult < 3)
					mult = 3;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) && (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_GIANT);
				}

				if (mult < 3)
					mult = 3;
			}

			/* Slay Dragon  */
			if ((f1 & (TR1_SLAY_DRAGON)) && (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 3)
					mult = 3;
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) && (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

				if (mult < 5)
					mult = 5;
			}


			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_IM_ACID);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3)
						mult = 3;
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
						r_ptr->r_flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3)
						mult = 3;
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
						r_ptr->r_flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3)
						mult = 3;
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
						r_ptr->r_flags3 |= (RF3_IM_COLD);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3)
						mult = 3;
				}
			}

			/* added poison brand by GJW    -KMW- */
			/* Brand (Poison) */
			if (f1 & (TR1_BRAND_POIS))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 3)
						mult = 3;
				}
			}

			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
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
	if (p_ptr->blind || no_lite())
		chance = chance / 10;
	if (p_ptr->confused || p_ptr->image)
		chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Invisible trap */
				if (cave_feat[y][x] == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					mprint(MSG_WARNING, "You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door XXX XXX XXX */
					cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (o_ptr = cave_o_idx[y][x]; o_ptr != NULL;
					o_ptr = o_ptr->next)
				{

					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST)
						continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval])
						continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						mprint(MSG_WARNING,
							"You have discovered a trap on the chest!");

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
 * Return TRUE if the given object is inscribed with "=g".
 */
static bool auto_pickup_okay(object_type *o_ptr)
{
	cptr s;

	/* XXX Hack -- Always pick up gold */
	if (o_ptr->tval == TV_GOLD)
		return (TRUE);

	/* No inscription */
	if (!o_ptr->note)
		return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->note), '=');

	/* Process preventions */
	while (s)
	{
		/* =g ('g'et) means auto pickup */
		if (s[1] == 'g') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

	/* Don't auto pickup */
	return (FALSE);
}


/*
 * Make the player carry everything in a grid.
 */
void py_pickup(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *item;
	object_type *next;

	cptr q;
	int mode = (USE_FLOOR | USE_REMOVE);

	char o_name[80];


	/* Require at least one object */
	if (cave_o_idx[py][px] == NULL) return;

	/* XXX Hack -- Update view after moving */
	handle_stuff();

	/* Examine each object */
	for (item = cave_o_idx[py][px]; item != NULL; item = next)
	{
		/* Access the "next" item */
		next = item->next;

		/* Describe the object */
		object_desc_mode |= ODESC_GOLD;
		object_desc(o_name, item, TRUE, 3);

		/* Pick up gold */
		if (item->tval == TV_GOLD)
		{
			/* Message */
			msg_format("You have found %ld gold pieces worth of %s.",
				(long) item->pval, o_name);

			/* Pick up the object */
			(void) inven_carry(item);

			/* Check the next object */
			continue;
		}

		/* Skip heavy objects */
		if (item->weight > 1000)
			continue;

		/* Don't auto-pickup store items (home is okay) */
		if (item->world == WORLD_STORE)
			continue;

		/* Check for auto-pickup */
		if (auto_pickup_okay(item))
		{
			/* Message */
			msg_format("You pick up %s.", o_name);

			/* Pick up the object */
			(void) inven_carry(item);
		}
	}

	/* Require at least one object */
	if (cave_o_idx[py][px] == NULL) return;

	/* Hack -- mention the object pile. */
	if (!pickup)
	{
		char buff[256];
		char o_name[80];

		item = cave_o_idx[py][px];

		object_desc(o_name, item, TRUE, 3);

		sprintf(buff, "There%s %s laying here",
			(item->number > 1) ? " are" : "'s", o_name);

		if (item->next)
		{
			strcat(buff, ", with some other stuff.");
		}
		else
		{
			strcat(buff, ".");
		}

		mprint(MSG_TEMP, buff);
		return;
	}

	if (p_ptr->inside_special == SPECIAL_STORE)
	{
		mode |= USE_BY_PARTS;
	}

	/* XXX Hack -- object_unabsorb() should not combine items */
	store_combine_flag = FALSE;

	q = "Pickup what";
	item = get_item(q, NULL, py, px, mode);

	/* XXX Hack -- undo the hack above */
	store_combine_flag = TRUE;

	if (item == NULL)
		return;

	if (item->weight > 1000)
	{
		int single = item->weight / item->number;

		if (single > 1000)
		{
			mprint(MSG_WARNING, "You can't carry something that heavy!");
			floor_carry(py, px, item);
			return;
		}
		else
		{
			object_type *foo = item;

			mprint(MSG_WARNING,
				"The whole pile is too heavy for you to carry!");
			item = object_unabsorb(item, 1);

			floor_carry(py, px, foo);
		}
	}

	inven_carry(item);
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
	if (k < 10)
		return (k < 5);

	/* Paranoia -- No power */
	if (power <= 0)
		return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if (randint(power) > ((ac * 3) / 4))
		return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Handle monster hitting a trap.
 */
void mon_hit_trap(int m_idx, int y, int x)
{
	char m_name[80];

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_desc(m_name, m_ptr, 0);

	switch (cave_feat[y][x])
	{

			/* Trap door. */
		case FEAT_TRAP_HEAD + 0x00:
		{
			if (m_ptr->ml)
				msg_format("%^s falls through a trap door!", m_name);

			delete_monster_idx(m_idx);
			break;
		}

			/* Pit. */
		case FEAT_TRAP_HEAD + 0x01:
		{
			bool fear = FALSE;

			if (m_ptr->ml)
				msg_format("%^s falls into a pit!", m_name);
			mon_take_hit(m_idx, damroll(2, 6), &fear, " splatters.", FALSE,
				FALSE);

			break;
		}

			/* Spiked pits. */
		case FEAT_TRAP_HEAD + 0x03:
		case FEAT_TRAP_HEAD + 0x02:
		{
			bool fear = FALSE;

			if (m_ptr->ml)
				msg_format("%^s falls into a pit!", m_name);
			mon_take_hit(m_idx, damroll(4, 6), &fear, " dies.", FALSE,
				FALSE);

			break;
		}

			/* Summoning trap. */
		case FEAT_TRAP_HEAD + 0x04:
		{
			int num = randint(3) + 2;
			int i;

			if (m_ptr->ml)
				msg_format("%^s is enveloped in a cloud of smoke!",
					m_name);

			for (i = 0; i < num; i++)
			{
				summon_specific_friendly(m_ptr->fy, m_ptr->fx,
					p_ptr->depth, 0);
			}
			break;
		}

			/* Teleport trap. */
		case FEAT_TRAP_HEAD + 0x05:
		{
			if (m_ptr->ml)
				msg_format("%^s hits a teleport trap!", m_name);

			teleport_away(m_idx, 100);
			break;
		}

			/* Fire trap. */
		case FEAT_TRAP_HEAD + 0x06:
		{
			if (m_ptr->ml)
				msg_format("%^s is enveloped with flames!", m_name);

			if (!(r_ptr->flags3 & RF3_IM_FIRE))
			{
				bool fear = FALSE;
				int dam = damroll(2, 6);

				if (r_ptr->flags3 & RF3_HURT_FIRE)
					dam *= 2;

				mon_take_hit(m_idx, dam, &fear, " burns up.", FALSE,
					FALSE);
			}
			break;
		}

			/* Acid trap. */
		case FEAT_TRAP_HEAD + 0x07:
		{
			if (m_ptr->ml)
				msg_format("%^s is splashed with acid!", m_name);

			if (!(r_ptr->flags3 & RF3_IM_ACID))
			{
				bool fear = FALSE;
				int dam = damroll(2, 6);

				mon_take_hit(m_idx, dam, &fear, " melts.", FALSE, FALSE);
			}
			break;
		}

			/* Slowness trap. */
		case FEAT_TRAP_HEAD + 0x08:
		{
			if (m_ptr->ml)
				msg_format("%^s is hit with a small dart.", m_name);

			if (!(r_ptr->flags1 & RF1_UNIQUE))
			{
				if (m_ptr->mspeed > 60)
					m_ptr->mspeed -= 10;
			}

			break;
		}

			/* Stat loss traps. */
			/* Do nothing for now, maybe some fancy tricks later on. */
		case FEAT_TRAP_HEAD + 0x09: /* STR */
		case FEAT_TRAP_HEAD + 0x0A: /* DEX */
		case FEAT_TRAP_HEAD + 0x0B: /* CON */
		{
			if (m_ptr->ml)
				msg_format("%^s is hit with a small dart.", m_name);
			break;
		}

			/* Blindness/Confusion traps. */
		case FEAT_TRAP_HEAD + 0x0C:
		case FEAT_TRAP_HEAD + 0x0D:
		{
			int dam = randint(50) + 25;

			if (m_ptr->ml)
				msg_format("%^s is enveloped in gas!", m_name);

			m_ptr->confused = dam;
			break;
		}

			/* Poison trap. */
		case FEAT_TRAP_HEAD + 0x0E:
		{
			if (m_ptr->ml)
				msg_format("%^s is enveloped in a disgusting gas!",
					m_name);

			if (!(r_ptr->flags3 & RF3_IM_POIS))
			{
				bool fear = FALSE;
				int dam = randint(20) + 10;

				mon_take_hit(m_idx, dam, &fear, " dies.", FALSE, FALSE);
			}
			break;
		}

			/* Paralyzation trap -- do nothing for now. */
		case FEAT_TRAP_HEAD + 0x0F:
		{
			if (m_ptr->ml)
				msg_format("%^s is hit with a small dart.", m_name);
			break;
		}
	}
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
			mprint(MSG_STUPID, "You fall through a trap door!");
			if (p_ptr->ffall)
			{
			  if (!p_ptr->inside_special) {
			    msg_print("You float gently down to the "
				      "next level.");
			  }
			}
			else
			{
				dam = damroll(2, 8);
				take_hit(dam, name);
			}
			
			if (!p_ptr->inside_special) {
			  /* New depth */
			  p_ptr->depth++;

			  /* Leaving */
			  p_ptr->leaving = TRUE;
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{
			mprint(MSG_STUPID, "You fall into a pit!");
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
			mprint(MSG_STUPID, "You fall into a spiked pit!");

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
					(void) set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{
			mprint(MSG_STUPID, "You fall into a spiked pit!");

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
					(void) set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");
					}

					else
					{
						dam = dam * 2;
						(void) set_poisoned(p_ptr->poisoned +
							randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
			mprint(MSG_STUPID, "You are enveloped in a cloud of smoke!");
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void) summon_specific(y, x, p_ptr->depth, 0);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			mprint(MSG_STUPID, "You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			mprint(MSG_STUPID, "You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			mprint(MSG_STUPID, "You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125))
			{
				mprint(MSG_STUPID, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void) set_slow(p_ptr->slow + rand_int(20) + 20);
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
				mprint(MSG_STUPID, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void) do_dec_stat(A_STR);
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
				mprint(MSG_STUPID, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void) do_dec_stat(A_DEX);
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
				mprint(MSG_STUPID, "A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);
				(void) do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			mprint(MSG_STUPID, "You are surrounded by a black gas!");
			if (!p_ptr->resist_blind)
			{
				(void) set_blind(p_ptr->blind + rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			mprint(MSG_STUPID,
				"You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->resist_confu)
			{
				(void) set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			mprint(MSG_STUPID,
				"You are surrounded by a pungent green gas!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void) set_poisoned(p_ptr->poisoned + rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			mprint(MSG_STUPID,
				"You are surrounded by a strange white mist!");
			if (!p_ptr->free_act)
			{
				(void) set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
			}
			break;
		}
	}
}


/*
 * Fetch an attack description from dam_*.txt files.
 */

static void flavored_attack(int percent, char *output)
{
	if (percent < 5)
	{
		if (!flavored_attacks)
			strcpy(output, "You scratch %s.");
		else
			strcpy(output, get_random_line("dam_none.txt"));

	}
	else if (percent < 30)
	{
		if (!flavored_attacks)
			strcpy(output, "You hit %s.");
		else
			strcpy(output, get_random_line("dam_med.txt"));

	}
	else if (percent < 60)
	{
		if (!flavored_attacks)
			strcpy(output, "You wound %s.");
		else
			strcpy(output, get_random_line("dam_lots.txt"));

	}
	else if (percent < 95)
	{
		if (!flavored_attacks)
			strcpy(output, "You cripple %s.");
		else
			strcpy(output, get_random_line("dam_huge.txt"));

	}
	else
	{
		if (!flavored_attacks)
			strcpy(output, "You demolish %s.");
		else
			strcpy(output, get_random_line("dam_xxx.txt"));

	}

}


static int barehanded_damage(void)
{
	if (p_ptr->engulfs)
	{
		return damroll((p_ptr->lev / 5) + 1, 5);

	}
	else
	{
		return 1;
	}
}

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	int num = 0, k = 0, base_dam = 0, bonus, chance;

	monster_type *m_ptr;
	monster_race *r_ptr;

	object_type *o_ptr;

	char m_name[80];

	bool fear = FALSE;

	bool do_quake = FALSE;

	/* Hack -- Assume bare hands are a ``hafted'' weapon. */

	int weapon_tval = TV_HAFTED;

	/* Magical Arena. */
	if (p_ptr->inside_special == SPECIAL_MAGIC_ARENA)
	{
		mprint(MSG_TEMP,
			"You're in the magical arena, melee is forbidden!");
		msg_print(NULL);
		return;
	}

	/* Access the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];

	/* 
	 * Melkor does not particularly appreciate this code.
	 */

	if (m_ptr->is_pet && 
	    !(m_ptr->fate == FATE_KILL || m_ptr->fate == FATE_SACRIFICE)) {

	  msg_print("Be nice to your pets, please!");
	  return;
	}

	if (confirm_blasphemy && sacred_monster(r_ptr))
	{
		if (!get_check("Commit blasphemy? "))
		{
			return;
		}
	}

	if (confirm_crime && r_ptr->flags2 & (RF2_INNOCENT))
	{
		if (!get_check("Break the law? "))
		{
			return;
		}
	}

	/* Disturb the player */
	disturb(0, 0);


	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);


	/* Auto-Recall if possible and visible */
	if (m_ptr->ml)
		monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml)
		health_track(cave_m_idx[y][x]);

	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		mformat(MSG_WARNING, "You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}


	if (m_ptr->is_pet) {
	  hostile_monsters(cave_m_idx[m_ptr->fy][m_ptr->fx]);
	}

	/* Access the weapon */
	o_ptr = equipment[EQUIP_WIELD];

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h;

	if (o_ptr)
		bonus += o_ptr->to_h;

	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
		/* Test for hit */
		if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			/* Hack -- bare hands do one damage */
			k = barehanded_damage();

			/* Handle normal weapon */
			if (o_ptr)
			{
				weapon_tval = o_ptr->tval;

				k = damroll(o_ptr->dd, o_ptr->ds);
				base_dam = k;

				k = tot_dam_aux(o_ptr, k, m_ptr);

				if (p_ptr->impact && (k > 50))
					do_quake = TRUE;

				k =
					critical_norm(o_ptr->weight, o_ptr->to_h, k,
					weapon_tval, m_ptr);
				/* factor in monster for critical_norm (GJW) -KMW- */
				k += o_ptr->to_d;

			}


			k = tot_dam_aux2(k, weapon_tval, r_ptr);


			/* Complex message */
			if (p_ptr->wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}


			/* Message */
			if (strchr("vwjmelX,.*", r_ptr->d_char))
			{
				msg_format("You hit %s.", m_name);

			}
			else if (p_ptr->engulfs)
			{
				msg_format("You engulf %s.", m_name);

			}
			else
			{
				char buff[255];

				flavored_attack((100 * k) / m_ptr->maxhp, buff);
				msg_format(buff, m_name);
			}

			/* Damage the weapon. */
			if (o_ptr)
			{
				int tmp = tot_dam_aux3(base_dam, r_ptr->ac);

				object_take_hit(o_ptr, tmp, "broke");


				if (p_ptr->wizard)
				{
					msg_format("Weapon took %d damage.", tmp);
				}
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[m_ptr->fy][m_ptr->fx], k, &fear,
					NULL, TRUE, FALSE))
				break;

			/* Disturb the monster (AFTER damage is done -GJW)  -KMW- */
			m_ptr->csleep = 0;

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
						r_ptr->r_flags3 |= (RF3_NO_CONF);
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
			/* Sound */
			sound(SOUND_MISS);

			/* Message */
			msg_format("You miss %s.", m_name);
		}
	}


	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		int py = p_ptr->py;
		int px = p_ptr->px;

		fire_mega_blast(py, px, GF_QUAKE, 10, damroll(2, 5));
	}

	/* Handle ``weird attacks''. */
	if (p_ptr->weird_attack && p_ptr->inside_special != SPECIAL_ARENA)
	{
		int rad = (p_ptr->lev / 10) * randint(2);
		int typ = rand_range(GF_ARROW, GF_EARTHQUAKE);
		char d_attack[80];

		if (rad < 1)
			rad = 1;
		if (rad > 9)
			rad = 9;

		describe_attack(typ, d_attack);

		msg_format("A ball of %s sprouts from your fingers.", d_attack);

		fire_ball(typ, 0, damroll(5, p_ptr->lev / 3 + 8), rad);
	}

	/* Heal some when vampiric */
	/* Heals 50% chance of healing the damage done. */
	if (p_ptr->vampiric && randint(100) < 50 && k &&
		!(r_ptr->flags3 & RF3_UNDEAD))
	{
		hp_player(k * rand_range(80, 100) / 100);
	}

	/* Feed the player if he is truly vampiric. */
	if (p_ptr->true_vampirism && k && !(r_ptr->flags3 & RF3_UNDEAD))
	{
		set_food(p_ptr->food + k * 5);
	}
}


/*
 * Return TRUE if a monster can enter the given location.
 */
static bool monster_can_enter(int m_idx, int y, int x)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int feat;
	bool do_move;

	/* Paranoia */
	if (!in_bounds_fully(y, x))
		return (FALSE);

	/* Assume no movement is possible */
	do_move = FALSE;

	/* Get the feature */
	feat = cave_feat[y][x];

	/* Empty space. */
	if (feat == FEAT_UNSEEN)
	{
		/* Nothing */
	}

	/* Floor is open */
	else if (cave_floor_bold(y, x))
	{
		/* Go ahead and move */
		do_move = TRUE;

		/* handle deep water -KMW- */
		if ((feat == FEAT_DEEP_WATER) &&
			!(r_ptr->flags2 & RF2_SWIM) &&
			!(r_ptr->flags2 & RF2_PASS_WALL) &&
			!(r_ptr->flags2 & RF2_FLY) &&
			!(r_ptr->flags2 & RF2_AQUATIC))
		{
			do_move = FALSE;
		}

		/* handle deep lava -KMW- */
		else if ((feat == FEAT_DEEP_LAVA) &&
			!(r_ptr->flags2 & RF2_DEEPLAVA) &&
			!(r_ptr->flags2 & RF2_PASS_WALL) &&
			!(r_ptr->flags2 & RF2_FLY))
		{
			do_move = FALSE;
		}

		/* handle shallow lava -KMW- */
		else if ((feat == FEAT_SHAL_LAVA) &&
			!(r_ptr->flags3 & RF3_IM_FIRE) &&
			!(r_ptr->flags2 & RF2_PASS_WALL) &&
			!(r_ptr->flags2 & RF2_FLY))
		{
			do_move = FALSE;
		}

		/* handle aquatic monsters -KMW- */
		else if ((r_ptr->flags2 & RF2_AQUATIC) &&
			(feat != FEAT_DEEP_WATER) &&
			(feat != FEAT_SHAL_WATER))
		{
			do_move = FALSE;
		}
	}

	/* Aquatic monsters never move through solid terrain. */
	else if (r_ptr->flags2 & RF2_AQUATIC)
	{
		/* Nothing. */
	}

	/* Permanent wall. Adjusted -KMW- */
	else if ((feat >= FEAT_PERM_EXTRA) &&
		(feat <= FEAT_PERM_SOLID))
	{
		/* Nothing */
	}

	/* Handle Chaos Fog */
	else if (feat == FEAT_CHAOS_FOG)
	{
		do_move = TRUE;
	}

	/* Handle trees */
	else if (feat == FEAT_TREES)
	{
		do_move = TRUE;
	}

	/* Monster moves through walls (and doors) */
	else if (r_ptr->flags2 & RF2_PASS_WALL)
	{
		do_move = TRUE;
	}

	return do_move;
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid.
 */
void move_player(int dir, int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int y, x;

	bool oktomove = TRUE;
	monster_type *m_ptr;

	/* Find the new grid */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Attack monsters, not pets. */
	if (cave_m_idx[y][x] > 0)
	{
		m_ptr = &m_list[cave_m_idx[y][x]];

		/* Not a pet */
		if (!m_ptr->is_pet)
		{
			py_attack(y, x);
			oktomove = FALSE;
			disturb(0, 0);
		}

		/* Pet, but player is in a bad location */
		else if (!monster_can_enter(cave_m_idx[y][x], py, px))
		{
			msg_print("Your pet is blocking your way.");
			oktomove = FALSE;
			disturb(0, 0);
		}

		/* A pet */
		else
		{
			mprint(MSG_TEMP, "You switch spots with your pet.");
		}
	}

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if (easy_alter && !jumping &&
		(cave_info[y][x] & (CAVE_MARK)) &&
		(cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		(cave_feat[y][x] <= FEAT_DOOR_TAIL))
	{
		/* Not already repeating */
		if (!p_ptr->command_rep)
		{
			/* Hack -- Optional auto-repeat */
			if (always_repeat && (p_ptr->command_arg <= 0))
			{
				/* Repeat 99 times */
				p_ptr->command_arg = 99;
			}
		}

		/* Alter */
		do_cmd_alter();

		oktomove = FALSE;
	}

	if (oktomove)
	{
		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;

		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
			rand_int(50 - p_ptr->skill_fos) == 0)
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle objects */
		py_pickup(jumping != always_pickup);

		/* Landed on a store */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD &&
				cave_feat[y][x] <= FEAT_SHOP_TAIL) ||
			cave_feat[y][x] == FEAT_STORE_EXIT)
		{
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';
		}

		/* Landed on a building */
		else if ((cave_feat[y][x] >= FEAT_BLDG_HEAD) &&
			(cave_feat[y][x] <= FEAT_BLDG_TAIL))
		{
			disturb(0, 0);

			/* Hack -- Enter building */
			p_ptr->command_new = ']';
		}

		/* Landed on a quest entrance */
		else if (cave_feat[y][x] == FEAT_QUEST_ENTER)
		{
			disturb(0, 0);

			/* Hack -- Enter quest level */
			p_ptr->command_new = '[';
		}


		/* Landed on a quest exit */
		else if (cave_feat[y][x] == FEAT_QUEST_EXIT)
		{
			exit_quest();
		}

		else if (cave_feat[y][x] >= FEAT_ALTAR_HEAD &&
			cave_feat[y][x] <= FEAT_ALTAR_TAIL)
		{

			cptr name = f_name + f_info[cave_feat[y][x]].name;
			cptr pref = (is_a_vowel(name[0])) ? "an" : "a";

			msg_format("You see %s %s.", pref, name);
		}

		/* Landed on an invisible trap */
		else if (cave_feat[y][x] == FEAT_INVIS && !p_ptr->flying)
		{
			disturb(0, 0);

			mprint(MSG_WARNING, "You found a trap!");

			pick_trap(y, x);
			hit_trap(y, x);
		}

		/* Landed on a visible trap */
		else if (cave_feat[y][x] >= FEAT_TRAP_HEAD &&
			cave_feat[y][x] <= FEAT_TRAP_TAIL && !p_ptr->flying)
		{
			disturb(0, 0);
			hit_trap(y, x);
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
	if (!in_bounds(y, x))
		return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave_feat[y][x] < FEAT_SECRET || cave_feat[y][x] == FEAT_SHAL_WATER
		|| cave_feat[y][x] == FEAT_DEEP_WATER ||
		cave_feat[y][x] >= FEAT_GRASS) return (FALSE);

	/* Unknown walls are not known walls */
	if (!(cave_info[y][x] & (CAVE_MARK)))
		return (FALSE);

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
	if (!in_bounds(y, x))
		return (TRUE);

	/* Memorized grids are always known */
	if (cave_info[y][x] & (CAVE_MARK))
		return (FALSE);

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
static byte cycle[] =
	{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] = { 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



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
	if (see_wall(cycle[i + 1], py, px))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i + 1], row, col))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i - 1], py, px))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i - 1], row, col))
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

	object_type *o_ptr;

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

		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[row][col]];

			/* Visible monster */
			if (m_ptr->ml)
				return (TRUE);
		}

		/* Visible objects abort running */
		for (o_ptr = cave_o_idx[row][col]; o_ptr != NULL;
			o_ptr = o_ptr->next)
		{

			/* Visible object */
			if (o_ptr->marked)
				return (TRUE);
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

					/* Invis traps */
				case FEAT_INVIS:

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
					/* water, lava, & trees -KMW- */
				case FEAT_DEEP_WATER:
				case FEAT_SHAL_WATER:
				case FEAT_DEEP_LAVA:
				case FEAT_SHAL_LAVA:
				case FEAT_TREES:
				case FEAT_MOUNTAIN:
					/* quest features -KMW- */
				case FEAT_QUEST_ENTER:
				case FEAT_QUEST_EXIT:

					/* Chaos Fog */
				case FEAT_CHAOS_FOG:
				case FEAT_FOG:
				case FEAT_GRASS:
				case FEAT_SWAMP:
				case FEAT_MUD:
				case FEAT_SHRUB:
				case FEAT_ROCKY_HILL:
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
					if (run_ignore_doors)
						notice = FALSE;

					/* Done */
					break;
				}

					/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (run_ignore_stairs)
						notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice)
				return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
		{

			/* Hit upon a wall, stop. */
			if (!cave_floor_bold(row, col))
				return (TRUE);

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

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_bold(row, col)) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
				(cave_feat[row][col] < FEAT_SECRET))
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

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_bold(row, col)) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
				(cave_feat[row][col] < FEAT_SECRET))
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
				if (run_use_corners && see_nothing(option, row, col) &&
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

	/* Take time */
	p_ptr->energy_use = 100;

	/* Move the player, using the "pickup" flag */
	move_player(p_ptr->run_cur_dir, FALSE);
}
