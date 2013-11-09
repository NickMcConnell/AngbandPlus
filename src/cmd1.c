/* File: cmd1.c */

/* Purpose: Movement commands (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#define MAX_VAMPIRIC_DRAIN 50


/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Never hit */
	if (chance <= 0) return (FALSE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = (chance + 1) / 2;

	/* Power competes against armor */
	if (randint0(chance) < (ac * 3 / 4)) return (FALSE);

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
	k = randint0(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Wimpy attack never hits */
	if (chance <= 0) return (FALSE);

	/* Penalize invisible targets */
	if (!vis) chance = (chance + 1) / 2;

	/* Power must defeat armor */
	if (randint0(chance) < (ac * 3 / 4)) return (FALSE);

	/* Assume hit */
	return (TRUE);
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
s32b critical_shot(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h_b + plus) * 4) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint1(5000) <= i)
	{
		k = weight + randint1(500);

		if (k < 500)
		{
#ifdef JP
			msg_print("手ごたえがあった！");
#else
			msg_print("It was a good hit!");
#endif

			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
#ifdef JP
			msg_print("かなりの手ごたえがあった！");
#else
			msg_print("It was a great hit!");
#endif

			dam = 2 * dam + 10;
		}
		else
		{
#ifdef JP
			msg_print("会心の一撃だ！");
#else
			msg_print("It was a superb hit!");
#endif

			dam = 3 * dam + 15;
		}
	}

	return dam;
}



/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
static s32b critical_norm(int weight, int plus, int dam, int meichuu, int mode)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + (meichuu * 3 + plus * 5) + (p_ptr->lev * 3));

	/* Chance */
	if ((randint1((p_ptr->pclass == CLASS_NINJA) ? 4444 : 5000) <= i) || (mode == PY_ATTACK_3DAN))
	{
		k = weight + randint1(650);
		if (mode == PY_ATTACK_3DAN) k+= randint1(650);

		if (k < 400)
		{
#ifdef JP
			msg_print("手ごたえがあった！");
#else
			msg_print("It was a good hit!");
#endif

			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
#ifdef JP
			msg_print("かなりの手ごたえがあった！");
#else
			msg_print("It was a great hit!");
#endif

			dam = 2 * dam + 10;
		}
		else if (k < 900)
		{
#ifdef JP
			msg_print("会心の一撃だ！");
#else
			msg_print("It was a superb hit!");
#endif

			dam = 3 * dam + 15;
		}
		else if (k < 1300)
		{
#ifdef JP
			msg_print("最高の会心の一撃だ！");
#else
			msg_print("It was a *GREAT* hit!");
#endif

			dam = 3 * dam + 20;
		}
		else
		{
#ifdef JP
			msg_print("比類なき最高の会心の一撃だ！");
#else
			msg_print("It was a *SUPERB* hit!");
#endif

			dam = ((7 * dam) / 2) + 25;
		}
	}

	return (dam);
}



/*
 * Handle monsters that resist blunt and/or edged weapons.
 */
s32b tot_dam_div(object_type *o_ptr, monster_type *m_ptr, bool in_hand)
{
	int div = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	cptr p;
	bool resist_weapon = TRUE;

	u32b flgs[TR_FLAG_SIZE];

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	if (in_hand)
	{
		if (p_ptr->cexp_info[CLASS_DRAGOON].clev > 24) add_flag(flgs, TR_SLAY_DRAGON);
		if (p_ptr->cexp_info[CLASS_DRAGOON].clev > 49) add_flag(flgs, TR_KILL_DRAGON);
		if (p_ptr->cexp_info[CLASS_EXORCIST].clev > 39) add_flag(flgs, TR_SLAY_EVIL);
		if (p_ptr->cexp_info[CLASS_EXORCIST].clev > 29) add_flag(flgs, TR_SLAY_DEMON);
		if (p_ptr->cexp_info[CLASS_EXORCIST].clev > 19) add_flag(flgs, TR_SLAY_UNDEAD);
		switch (p_ptr->pclass)
		{
		case CLASS_DRAGOON:
			if (cexp_ptr->clev > 24) add_flag(flgs, TR_KILL_DRAGON);
			else add_flag(flgs, TR_SLAY_DRAGON);
			break;
		case CLASS_EXORCIST:
			if (cexp_ptr->clev > 19) add_flag(flgs, TR_SLAY_EVIL);
			if (cexp_ptr->clev > 9) add_flag(flgs, TR_SLAY_DEMON);
			add_flag(flgs, TR_SLAY_UNDEAD);
			/* Fall through */
		case CLASS_CLERIC:
		case CLASS_PRIEST:
		case CLASS_ANGELKNIGHT:
			add_flag(flgs, TR_BLESSED);
			break;
		case CLASS_LICH:
			add_flag(flgs, TR_UNHOLY);
			break;
		}
	}

	/* Get base name of object kind */
	switch (o_ptr->tval)
	{
#ifdef JP
	case TV_BULLET:
	case TV_ROUND:
	case TV_SHELL:
		p = "銃弾";
		break;
	case TV_ARROW:
	case TV_BOLT:
		p = "矢";
		break;
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
	case TV_DIGGING:
		p = "武器";
		break;
	default:
		p = "投げた物";
		break;
#else
	case TV_BULLET:
		p = "bullet";
		break;
	case TV_ROUND:
		p = "rifle round";
		break;
	case TV_SHELL:
		p = "shot";
		break;
	case TV_ARROW:
		p = "arrow";
		break;
	case TV_BOLT:
		p = "bolt";
		break;
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
	case TV_DIGGING:
		p = "weapon";
		break;
	default:
		p = "missile";
		break;
#endif
	}

	/* Hack -- "Throwing" */
#ifdef JP
	if (!o_ptr->k_idx) p = "投げた物";
#else
	if (!o_ptr->k_idx) p = "missile";
#endif

	if (have_flag(flgs, TR_UNHOLY) && have_flag(flgs, TR_BLESSED))
	{
		resist_weapon = FALSE;
	}
	else if (have_flag(flgs, TR_UNHOLY))
	{
		if (!(r_ptr->flags3 & RF3_EVIL)) resist_weapon = FALSE;
	}
	else if (have_flag(flgs, TR_BLESSED))
	{
		if (!(r_ptr->flags3 & RF3_GOOD)) resist_weapon = FALSE;
	}

	if (!resist_weapon) return div;

	switch (o_ptr->tval)
	{
		case TV_ARROW:
		case TV_BOLT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			if (r_ptr->flagsr & RFR_IM_EDGED)
			{
				div = 60;

				if (m_ptr->ml)
				{
					/* Message */
					if (!(r_ptr->r_flagsr & RFR_IM_EDGED))
					{
#ifdef JP
						msg_format("この%sは全く効かないようだ！", p);
#else
						msg_format("Your %s doesn't seem to be doing any damage!", p);
#endif
						r_ptr->r_flagsr |= RFR_IM_EDGED;
						if (r_ptr->flagsr & RFR_RES_EDGED) r_ptr->r_flagsr |= RFR_RES_EDGED;
					}
				}
			}
			else if (r_ptr->flagsr & RFR_RES_EDGED)
			{
				div = 20;

				if (m_ptr->ml)
				{
					/* Message */
					if (!(r_ptr->r_flagsr & RFR_RES_EDGED))
					{
#ifdef JP
						msg_format("この%sはあまり効かないようだ！", p);
#else
						msg_format("Your %s seems to be doing little damage!", p);
#endif
						r_ptr->r_flagsr |= RFR_RES_EDGED;
					}
				}
			}

			break;
		}

		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_HAFTED:
		default:
		{
			if (r_ptr->flagsr & RFR_IM_BLUNT)
			{
				div = 60;

				if (m_ptr->ml)
				{
					/* Message */
					if (!(r_ptr->r_flagsr & RFR_IM_BLUNT))
					{
#ifdef JP
						msg_format("この%sは全く効かないようだ！", p);
#else
						msg_format("Your %s doesn't seem to be doing any damage!", p);
#endif
						r_ptr->r_flagsr |= RFR_IM_BLUNT;
						if (r_ptr->flagsr & RFR_RES_BLUNT) r_ptr->r_flagsr |= RFR_RES_BLUNT;
					}
				}
			}
			else if (r_ptr->flagsr & RFR_RES_BLUNT)
			{
				div = 20;

				if (m_ptr->ml)
				{
					/* Message */
					if (!(r_ptr->r_flagsr & RFR_RES_BLUNT))
					{
#ifdef JP
						msg_format("この%sはあまり効かないようだ！", p);
#else
						msg_format("Your %s seems to be doing little damage!", p);
#endif
						r_ptr->r_flagsr |= RFR_RES_BLUNT;
					}
				}
			}

			break;
		}
	}

	if (div < 0) div = 10;

	return div;
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
s32b tot_dam_aux(object_type *o_ptr, int tdam, monster_type *m_ptr, bool in_hand)
{
	int mult = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b flgs[TR_FLAG_SIZE];

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	if (in_hand)
	{
		if (p_ptr->cexp_info[CLASS_DRAGOON].max_clev > 24) add_flag(flgs, TR_SLAY_DRAGON);
		if (p_ptr->cexp_info[CLASS_DRAGOON].max_clev > 49) add_flag(flgs, TR_KILL_DRAGON);
		if (p_ptr->cexp_info[CLASS_EXORCIST].max_clev > 49) add_flag(flgs, TR_SLAY_DEMON);
		if (p_ptr->cexp_info[CLASS_EXORCIST].max_clev > 49) add_flag(flgs, TR_SLAY_UNDEAD);
		switch (p_ptr->pclass)
		{
		case CLASS_DRAGOON:
			add_flag(flgs, TR_KILL_DRAGON);
			break;
		case CLASS_EXORCIST:
			add_flag(flgs, TR_SLAY_UNDEAD);
			add_flag(flgs, TR_SLAY_DEMON);
			/* Fall through */
		case CLASS_CLERIC:
		case CLASS_PRIEST:
		case CLASS_ANGELKNIGHT:
			add_flag(flgs, TR_BLESSED);
			break;
		case CLASS_LICH:
			add_flag(flgs, TR_UNHOLY);
			break;
		}
	}

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_ARROW:
		case TV_BOLT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Slay Animal */
			if ((have_flag(flgs, TR_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & RF3_ANIMAL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ANIMAL;
				}

				if (mult < 25) mult = 25;
			}

			/* Slay Evil */
			if ((have_flag(flgs, TR_SLAY_EVIL)) &&
			    (r_ptr->flags3 & RF3_EVIL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_EVIL;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Good */
			if (((have_flag(flgs, TR_SLAY_GOOD)) || (p_ptr->special_attack & (ATTACK_EVIL))) &&
			    (r_ptr->flags3 & RF3_GOOD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GOOD;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Living */
			if (((have_flag(flgs, TR_SLAY_LIVING)) || (p_ptr->special_attack & (ATTACK_EVIL))) &&
			    monster_living(r_ptr))
			{
				if (mult < 30) mult = 30;
			}

			/* Slay Human */
			if ((have_flag(flgs, TR_SLAY_HUMAN)) &&
			    (r_ptr->flags2 & RF2_HUMAN))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags2 |= RF2_HUMAN;
				}

				if (mult < 25) mult = 25;
			}

			/* Slay Undead */
			if (have_flag(flgs, TR_SLAY_UNDEAD) &&
			    (r_ptr->flags3 & RF3_UNDEAD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_UNDEAD;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Demon */
			if (have_flag(flgs, TR_SLAY_DEMON) &&
			    (r_ptr->flags3 & RF3_DEMON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DEMON;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Orc */
			if ((have_flag(flgs, TR_SLAY_ORC)) &&
			    (r_ptr->flags3 & RF3_ORC))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ORC;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Troll */
			if ((have_flag(flgs, TR_SLAY_TROLL)) &&
			    (r_ptr->flags3 & RF3_TROLL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_TROLL;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Giant */
			if ((have_flag(flgs, TR_SLAY_GIANT)) &&
			    (r_ptr->flags3 & RF3_GIANT))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GIANT;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Dragon  */
			if ((have_flag(flgs, TR_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 30) mult = 30;
			}

			/* Execute Dragon */
			if (have_flag(flgs, TR_KILL_DRAGON) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 50) mult = 50;

			}

			/* Brand (Acid) */
			if ((have_flag(flgs, TR_BRAND_ACID)) || (p_ptr->special_attack & (ATTACK_ACID)))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_ACID)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_ACID;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_ACID)
					{
						if (mult < 50) mult = 50;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_ACID;
						}
					}
					else if (mult < 25) mult = 25;
				}
			}

			/* Brand (Elec) */
			if ((have_flag(flgs, TR_BRAND_ELEC)) || (p_ptr->special_attack & (ATTACK_ELEC)))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_ELEC)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_ELEC;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_ELEC)
					{
						if (mult < 50) mult = 50;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_ELEC;
						}
					}
					else if (mult < 25) mult = 25;
				}
			}

			/* Brand (Fire) */
			if ((have_flag(flgs, TR_BRAND_FIRE)) || (p_ptr->special_attack & (ATTACK_FIRE)))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_FIRE)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_FIRE;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_FIRE)
					{
						if (mult < 50) mult = 50;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_FIRE;
						}
					}
					else if (mult < 25) mult = 25;
				}
			}

			/* Brand (Cold) */
			if ((have_flag(flgs, TR_BRAND_COLD)) || (p_ptr->special_attack & (ATTACK_COLD)))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_COLD)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_COLD;
					}
				}
				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_COLD)
					{
						if (mult < 50) mult = 50;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_COLD;
						}
					}
					else if (mult < 25) mult = 25;
				}
			}

			/* Brand (Poison) */
			if ((have_flag(flgs, TR_BRAND_POIS)) || (p_ptr->special_attack & (ATTACK_POIS)))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_POIS)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_POIS;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 25) mult = 25;
				}
			}

			/* Digging */
			if (have_flag(flgs, TR_TUNNEL) && (o_ptr->to_misc[OB_TUNNEL] > 0) &&
			    (r_ptr->flags3 & RF3_HURT_ROCK))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_HURT_ROCK;
				}

				if (mult < 50) mult = 50;
			}

			if ((have_flag(flgs, TR_FORCE_WEAPON)) && (p_ptr->csp > (o_ptr->dd * o_ptr->ds / 5)))
			{
				p_ptr->csp -= (1+(o_ptr->dd * o_ptr->ds / 5));
				p_ptr->redraw |= (PR_MANA);
				mult = mult * 3 / 2 + 20;
			}
			break;
		}
	}
	if (mult > 150) mult = 150;


	/* Return the total damage */
	return tdam * mult / 10;
}


/*
 * Search for hidden things
 */
void search(void)
{
	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;

	cave_type *c_ptr;


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
			if (randint0(100) < chance)
			{
				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Invisible trap */
				if (c_ptr->mimic && is_trap(c_ptr->feat))
				{
					/* Pick a trap */
					disclose_grid(y, x);

					/* Message */
#ifdef JP
					msg_print("トラップを発見した。");
#else
					msg_print("You have found a trap.");
#endif


					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (is_hidden_door(c_ptr))
				{
					/* Message */
#ifdef JP
					msg_print("隠しドアを発見した。");
#else
					msg_print("You have found a secret door.");
#endif

					/* Disclose */
					disclose_grid(y, x);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip non-trapped chests */
					if (o_ptr->pval < 0) continue;
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
#ifdef JP
						msg_print("箱に仕掛けられたトラップを発見した！");
#else
						msg_print("You have discovered a trap on the chest!");
#endif


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
 * Helper routine for py_pickup() and py_pickup_floor().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
void py_pickup_aux(int o_idx)
{
	int slot, i;

#ifdef JP
/*
 * アイテムを拾った際に「２つのケーキを持っている」
 * "You have two cakes." とアイテムを拾った後の合計のみの表示がオリジナル
 * だが、違和感が
 * あるという指摘をうけたので、「〜を拾った、〜を持っている」という表示
 * にかえてある。そのための配列。
 */
	char o_name[MAX_NLEN];
	char old_name[MAX_NLEN];
	char kazu_str[80];
	int hirottakazu;
#else
	char o_name[MAX_NLEN];
#endif

	object_type *o_ptr;

	o_ptr = &o_list[o_idx];

#ifdef JP
	/* Describe the object */
	object_desc(old_name, o_ptr, TRUE, 0);
	object_desc_kosuu(kazu_str, o_ptr);
	hirottakazu = o_ptr->number;
#endif
	/* Carry the object */
	slot = inven_carry(o_ptr);

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Delete the object */
	delete_object_idx(o_idx);

	if (easy_band)
	{
		bool old_known;
		int idx;

		old_known = identify_item(o_ptr);

		/* Auto-inscription/destroy */
		idx = is_autopick(o_ptr);
		auto_inscribe_item(slot, idx);
		if (destroy_identify && !old_known)
		{
			auto_destroy_item(slot, idx);
			if (o_ptr->number <= 0)
			{
				inven_item_optimize(slot);
				return;
			}
		}
	}

	/* Describe the object */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
#ifdef JP
	if (plain_pickup)
	{
		msg_format("%s(%c)を持っている。",o_name, index_to_label(slot));
	}
	else
	{
		if (o_ptr->number > hirottakazu)
		{
			msg_format("%s拾って、%s(%c)を持っている。",
			           kazu_str, o_name, index_to_label(slot));
		}
		else
		{
			msg_format("%s(%c)を拾った。", o_name, index_to_label(slot));
		}
	}
	strcpy(record_o_name, old_name);
#else
	msg_format("You have %s (%c).", o_name, index_to_label(slot));
	strcpy(record_o_name, o_name);
#endif
	record_turn = turn;


	/* Check if completed a quest */
	for (i = 0; i < max_quests; i++)
	{
		if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) &&
		    (quest[i].status == QUEST_STATUS_TAKEN) &&
		    (quest[i].k_idx == o_ptr->name1))
		{
			if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, i, NULL);
			quest[i].status = QUEST_STATUS_COMPLETED;
			quest[i].complev = (byte)p_ptr->lev;

			/* Make a sound */
			sound(SOUND_QUEST);

#ifdef JP
			msg_print("クエストを達成した！");
#else
			msg_print("You completed your quest!");
#endif

			msg_print(NULL);
			if (quest_is_fixed(i)) change_your_alignment_lnc(10);
		}
	}

	/* Player found the "Runeweapon" finally! */
	if (object_is_snapdragon_runeweapon(o_ptr))
	{
		runeweapon_type *runeweapon = &runeweapon_list[o_ptr->xtra3];
		if (!(runeweapon->status & RW_STATUS_FOUND))
		{
			char t_name[MAX_NLEN];
			runeweapon->status |= (RW_STATUS_FOUND);

			object_desc(t_name, o_ptr, TRUE, 0);
			if (runeweapon->ancestor[0])
			{
#ifdef JP
				msg_format("%d階、先祖%sの魂を封じた%sをついに手に入れた！", dun_level, runeweapon->ancestor, t_name);
#else
				msg_format("You found %s spirit of ancestor %s sealed in finally on dungeon level %d!", t_name, runeweapon->ancestor, dun_level);
#endif
			}
			else
			{
#ifdef JP
				msg_format("%d階、ヴァレリアの覇者jnkの魂を封じた%sをついに手に入れた！", dun_level, t_name);
#else
				msg_format("You found %s spirit of jnk the Champion of Valeria sealed in finally on dungeon level %d!", t_name, dun_level);
#endif
			}
			if (record_fix_art) do_cmd_write_nikki(NIKKI_ART, 0, t_name);
		}
	}
}


/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 */
void carry(int pickup)
{
	cave_type *c_ptr = &cave[py][px];

	s16b this_o_idx, next_o_idx = 0;

	char	o_name[MAX_NLEN];

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	/* Automatically pickup/destroy/inscribe items */
	auto_pickup_items(c_ptr);


#ifdef ALLOW_EASY_FLOOR

	if (easy_floor)
	{
		py_pickup_floor(pickup);
		return;
	}

#endif /* ALLOW_EASY_FLOOR */

	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Sense the object */
		sense_floor_object(this_o_idx);

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			int value = (long)o_ptr->pval;

			/* Collect the gold */
			p_ptr->au[o_ptr->sval] += value;

			/* Delete the gold */
			delete_object_idx(this_o_idx);

			/* Message */
#ifdef JP
			msg_format(" $%ld の価値がある%sを見つけた。",
			           (long)value, o_name);
#else
			msg_format("You collect %ld gold pieces worth of %s.",
				   (long)value, o_name);
#endif


			sound(SOUND_SELL);

			/* Update gold */
			p_ptr->update |= (PU_GOLD);

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}

		/* Pick up objects */
		else
		{
			/* Hack - some objects were handled in auto_pickup_items(). */
			if (o_ptr->marked & OM_NOMSG)
			{
				/* Clear the flag. */
				o_ptr->marked &= ~OM_NOMSG;
			}
			/* Describe the object */
			else if (!pickup)

			{
#ifdef JP
				msg_format("%sがある。", o_name);
#else
				msg_format("You see %s.", o_name);
#endif

			}

			/* Note that the pack is too full */
			else if (!inven_carry_okay(o_ptr))
			{
#ifdef JP
				msg_format("ザックには%sを入れる隙間がない。", o_name);
#else
				msg_format("You have no room for %s.", o_name);
#endif

			}

			/* Pick up the item (if requested and allowed) */
			else
			{
				int okay = TRUE;

				/* Hack -- query every item */
				if (carry_query_flag)
				{
					char out_val[MAX_NLEN+20];
#ifdef JP
					sprintf(out_val, "%sを拾いますか? ", o_name);
#else
					sprintf(out_val, "Pick up %s? ", o_name);
#endif

					okay = get_check(out_val);
				}

				/* Attempt to pick up an object. */
				if (okay)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}
			}
		}
	}
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
	k = randint0(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Paranoia -- No power */
	if (power <= 0) return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if (randint1(power) > ((ac * 3) / 4)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Handle player hitting a real trap
 */
static void hit_trap(void)
{
	int i, num, dam;
	int x = px, y = py;

	cave_type *c_ptr;

#ifdef JP
	cptr		name = "トラップ";
#else
	cptr name = "a trap";
#endif



	/* Disturb the player */
	disturb(0, 0);

	/* Get the cave grid */
	c_ptr = &cave[y][x];

	/* Analyze XXX XXX XXX */
	switch (c_ptr->feat)
	{
		case FEAT_TRAP_TRAPDOOR:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("落し戸を飛び越えた。");
#else
				msg_print("You fly over a trap door.");
#endif

			}
			else
			{
				int  down_num;
				bool upward_dun = (d_info[dungeon_type].flags1 & DF1_UPWARD) ? TRUE : FALSE;
				int  dd, dm;

#ifdef JP
				msg_print("落し戸に落ちた！");
#else
				msg_print("You have fallen through a trap door!");
#endif

				sound(SOUND_FALL);
				dam = damroll(2, 8);
#ifdef JP
				name = "落し戸";
#else
				name = "a trap door";
#endif

				take_hit(DAMAGE_NOESCAPE, dam, name);

				/* Still alive and autosave enabled */
				if (autosave_l && (p_ptr->chp >= 0))
					do_cmd_save_game(TRUE);

#ifdef JP
				do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "落し戸に落ちた");
#else
				do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "You have fallen through a trap door!");
#endif
				down_num = randint1(3);

				if (upward_dun) dm = d_info[dungeon_type].mindepth;
				else dm = d_info[dungeon_type].maxdepth;

				for (i = 1; i < down_num; i++)
				{
					dd = dun_level + (upward_dun ? (0 - i) : i);
					if ((!(upward_dun ^ astral_mode) && quest_number(dd)) || (dd == dm))
					{
						down_num = i;
						break;
					}
				}

				switch (down_num)
				{
				case 1:
					prepare_change_floor_mode(CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
					break;
				case 2:
					prepare_change_floor_mode(CFM_DOWN | CFM_FORCE2L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
					break;
				default:
					dun_level += upward_dun ? (0 - down_num + 2) : (down_num - 2);
					prepare_change_floor_mode(CFM_DOWN | CFM_FORCE2L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
					break;
				}

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			break;
		}

		case FEAT_TRAP_PIT:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("落し穴を飛び越えた。");
#else
				msg_print("You fly over a pit trap.");
#endif

			}
			else
			{
#ifdef JP
				msg_print("落し穴に落ちてしまった！");
#else
				msg_print("You have fallen into a pit!");
#endif

				dam = damroll(2, 6);
#ifdef JP
				name = "落し穴";
#else
				name = "a pit trap";
#endif

				take_hit(DAMAGE_NOESCAPE, dam, name);
			}
			break;
		}

		case FEAT_TRAP_SPIKED_PIT:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("トゲのある落し穴を飛び越えた。");
#else
				msg_print("You fly over a spiked pit.");
#endif

			}
			else
			{
#ifdef JP
				msg_print("スパイクが敷かれた落し穴に落ちてしまった！");
#else
				msg_print("You fall into a spiked pit!");
#endif


				/* Base damage */
#ifdef JP
				name = "落し穴";
#else
				name = "a pit trap";
#endif

				dam = damroll(2, 6);

				/* Extra spike damage */
				if (randint0(100) < 50)
				{
#ifdef JP
					msg_print("スパイクが刺さった！");
#else
					msg_print("You are impaled!");
#endif


#ifdef JP
					name = "トゲのある落し穴";
#else
					name = "a spiked pit";
#endif

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint1(dam));
				}

				/* Take the damage */
				take_hit(DAMAGE_NOESCAPE, dam, name);
			}
			break;
		}

		case FEAT_TRAP_POISON_PIT:
		{
			if (p_ptr->ffall)
			{
#ifdef JP
				msg_print("トゲのある落し穴を飛び越えた。");
#else
				msg_print("You fly over a spiked pit.");
#endif

			}
			else
			{
#ifdef JP
			msg_print("スパイクが敷かれた落し穴に落ちてしまった！");
#else
				msg_print("You fall into a spiked pit!");
#endif


				/* Base damage */
				dam = damroll(2, 6);

#ifdef JP
				name = "落し穴";
#else
				name = "a pit trap";
#endif


				/* Extra spike damage */
				if (randint0(100) < 50)
				{
#ifdef JP
					msg_print("毒を塗られたスパイクが刺さった！");
#else
					msg_print("You are impaled on poisonous spikes!");
#endif


#ifdef JP
					name = "トゲのある落し穴";
#else
					name = "a spiked pit";
#endif


					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint1(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
#ifdef JP
						msg_print("しかし毒の影響はなかった！");
#else
						msg_print("The poison does not affect you!");
#endif

					}

					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint1(dam));
					}
				}

				/* Take the damage */
				take_hit(DAMAGE_NOESCAPE, dam, name);
			}

			break;
		}

		case FEAT_TRAP_TY_CURSE:
		{
#ifdef JP
			msg_print("何かがピカッと光った！");
#else
			msg_print("There is a flash of shimmering light!");
#endif

			c_ptr->info &= ~(CAVE_MARK);
			cave_force_set_floor(y, x);
			num = 2 + randint1(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(0, y, x, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
			}

			if (dun_level > randint1(100)) /* No nasty effect for low levels */
			{
				bool stop_ty = FALSE;
				int count = 0;

				do
				{
					stop_ty = activate_ty_curse(stop_ty, &count);
				}
				while (one_in_(6));
			}
			break;
		}

		case FEAT_TRAP_TELEPORT:
		{
#ifdef JP
			msg_print("テレポート・トラップにひっかかった！");
#else
			msg_print("You hit a teleport trap!");
#endif

			if (p_ptr->earth_spike)
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			else teleport_player(100);
			break;
		}

		case FEAT_TRAP_FIRE:
		{
#ifdef JP
			msg_print("炎に包まれた！");
#else
			msg_print("You are enveloped in flames!");
#endif

			dam = damroll(4, 6);
#ifdef JP
			(void)fire_dam(dam, "炎のトラップ");
#else
			(void)fire_dam(dam, "a fire trap");
#endif

			break;
		}

		case FEAT_TRAP_ACID:
		{
#ifdef JP
			msg_print("酸が吹きかけられた！");
#else
			msg_print("You are splashed with acid!");
#endif

			dam = damroll(4, 6);
#ifdef JP
			(void)acid_dam(dam, "酸のトラップ");
#else
			(void)acid_dam(dam, "an acid trap");
#endif

			break;
		}

		case FEAT_TRAP_SLOW:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
				ACTIVATE_MULTISHADOW();
				take_hit(DAMAGE_ATTACK, dam, name);
				if (!IS_MULTISHADOW(0)) (void)set_slow(p_ptr->slow + randint0(20) + 20, FALSE);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_LOSE_STR:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
				ACTIVATE_MULTISHADOW();
#ifdef JP
				take_hit(DAMAGE_ATTACK, dam, "ダーツの罠");
#else
				take_hit(DAMAGE_ATTACK, dam, "a dart trap");
#endif
				if (!IS_MULTISHADOW(0)) (void)do_dec_stat(A_STR);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_LOSE_DEX:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
				ACTIVATE_MULTISHADOW();
#ifdef JP
				take_hit(DAMAGE_ATTACK, dam, "ダーツの罠");
#else
				take_hit(DAMAGE_ATTACK, dam, "a dart trap");
#endif
				if (!IS_MULTISHADOW(0)) (void)do_dec_stat(A_DEX);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_LOSE_CON:
		{
			if (check_hit(125))
			{
#ifdef JP
				msg_print("小さなダーツが飛んできて刺さった！");
#else
				msg_print("A small dart hits you!");
#endif

				dam = damroll(1, 4);
				ACTIVATE_MULTISHADOW();
#ifdef JP
				take_hit(DAMAGE_ATTACK, dam, "ダーツの罠");
#else
				take_hit(DAMAGE_ATTACK, dam, "a dart trap");
#endif
				if (!IS_MULTISHADOW(0)) (void)do_dec_stat(A_CON);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				msg_print("小さなダーツが飛んできた！が、運良く当たらなかった。");
#else
				msg_print("A small dart barely misses you.");
#endif

			}
			break;
		}

		case FEAT_TRAP_BLIND:
		{
#ifdef JP
			msg_print("黒いガスに包み込まれた！");
#else
			msg_print("A black gas surrounds you!");
#endif

			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint0(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_CONFUSE:
		{
#ifdef JP
			msg_print("きらめくガスに包み込まれた！");
#else
			msg_print("A gas of scintillating colors surrounds you!");
#endif

			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + randint0(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_POISON:
		{
#ifdef JP
			msg_print("刺激的な緑色のガスに包み込まれた！");
#else
			msg_print("A pungent green gas surrounds you!");
#endif

			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + randint0(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_SLEEP:
		{
#ifdef JP
			msg_print("奇妙な白い霧に包まれた！");
#else
			msg_print("A strange white mist surrounds you!");
#endif

			if (!p_ptr->free_act)
			{
#ifdef JP
msg_print("あなたは眠りに就いた。");
#else
				msg_print("You fall asleep.");
#endif


				if (ironman_nightmare)
				{
#ifdef JP
msg_print("身の毛もよだつ光景が頭に浮かんだ。");
#else
					msg_print("A horrible vision enters your mind.");
#endif


					/* Pick a nightmare */
					get_mon_num_prep(get_nightmare, NULL);

					/* Have some nightmares */
					have_nightmare(get_mon_num(MAX_DEPTH));

					/* Remove the monster restriction */
					get_mon_num_prep(NULL, NULL);
				}
				(void)set_paralyzed(p_ptr->paralyzed + randint0(10) + 5);
			}
			break;
		}

		case FEAT_TRAP_TRAPS:
		{
#ifdef JP
msg_print("まばゆい閃光が走った！");
#else
			msg_print("There is a bright flash of light!");
#endif


			/* Destroy this trap */
			cave_force_set_floor(y, x);

			/* Make some new traps */
			project(0, 1, y, x, 0, GF_MAKE_TRAP, PROJECT_HIDE | PROJECT_JUMP | PROJECT_GRID, MODIFY_ELEM_MODE_NONE);

			break;
		}

		case FEAT_TRAP_ALARM:
		{
#ifdef JP
			msg_print("けたたましい音が鳴り響いた！");
#else
			msg_print("An alarm sounds!");
#endif

			aggravate_monsters(0);

			break;
		}

		case FEAT_TRAP_OPEN:
		{
#ifdef JP
			msg_print("大音響と共にまわりの壁が崩れた！");
#else
			msg_print("Suddenly, surrounding walls are opened!");
#endif
			(void)project(0, 3, y, x, 0, GF_DISINTEGRATE, PROJECT_GRID | PROJECT_HIDE, MODIFY_ELEM_MODE_NONE);
			(void)project(0, 3, y, x - 4, 0, GF_DISINTEGRATE, PROJECT_GRID | PROJECT_HIDE, MODIFY_ELEM_MODE_NONE);
			(void)project(0, 3, y, x + 4, 0, GF_DISINTEGRATE, PROJECT_GRID | PROJECT_HIDE, MODIFY_ELEM_MODE_NONE);
			aggravate_monsters(0);

			break;
		}

		case FEAT_TRAP_ARMAGEDDON:
		{
			static int levs[10] = {0, 0, 20, 10, 5, 3, 2, 1, 1, 1};

			int lev;
#ifdef JP
			msg_print("突然天界の戦争に巻き込まれた！");
#else
			msg_print("Suddenly, you are surrounded by immotal beings!");
#endif

			/* Destroy this trap */
			cave_force_set_floor(y, x);

			/* Summon Demons and Angels */
			for (lev = dun_level; lev >= 20; lev -= 1 + lev/16)
			{
				num = levs[MIN(lev/10, 9)];
				for (i = 0; i < num; i++)
				{
					int x1 = rand_spread(x, 7);
					int y1 = rand_spread(y, 5);

					/* Skip illegal grids */
					if (!in_bounds(y1, x1)) continue;

					/* Require line of sight */
					if (!player_has_los_bold(y1, x1)) continue;

					(void)summon_specific(0, y1, x1, lev, SUMMON_DEMON, (PM_NO_PET));
					(void)summon_specific(0, y1, x1, lev, SUMMON_ANGEL, (PM_NO_PET));
				}
			}
			break;
		}

		case FEAT_TRAP_PIRANHA:
		{
#ifdef JP
			msg_print("突然壁から水が溢れ出した！ピラニアがいる！");
#else
			msg_print("Suddenly, the room is filled with water with piranhas!");
#endif

			/* Destroy this trap */
			cave_force_set_floor(y, x);

			/* Water fills room */
			fire_ball_hide(GF_WATER_FLOW, 0, 1, 10, FALSE);
			set_mermaid_in_water();

			/* Summon Piranhas */
			num = 1 + dun_level/20;
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(0, y, x, dun_level, SUMMON_PIRANHAS, (PM_ALLOW_GROUP | PM_NO_PET));
			}
			break;
		}
	}
}


static void touch_zap_player(monster_type *m_ptr)
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
			monster_desc(aura_dam, m_ptr, 0x288);

#ifdef JP
			msg_print("突然とても熱くなった！");
#else
			msg_print("You are suddenly very hot!");
#endif


			if (p_ptr->oppose_fire) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_fire) aura_damage = (aura_damage + 2) / 3;

			take_hit(DAMAGE_NOESCAPE, aura_damage, aura_dam);
			if (m_ptr->ml) r_ptr->r_flags2 |= RF2_AURA_FIRE;
			handle_stuff();
		}
	}

	if (r_ptr->flags3 & RF3_AURA_COLD)
	{
		if (!p_ptr->immune_cold)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x288);

#ifdef JP
			msg_print("突然とても寒くなった！");
#else
			msg_print("You are suddenly very cold!");
#endif


			if (p_ptr->oppose_cold) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_cold) aura_damage = (aura_damage + 2) / 3;

			take_hit(DAMAGE_NOESCAPE, aura_damage, aura_dam);
			if (m_ptr->ml) r_ptr->r_flags3 |= RF3_AURA_COLD;
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
			monster_desc(aura_dam, m_ptr, 0x288);

			if (p_ptr->oppose_elec) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_elec) aura_damage = (aura_damage + 2) / 3;

#ifdef JP
			msg_print("電撃をくらった！");
#else
			msg_print("You get zapped!");
#endif

			take_hit(DAMAGE_NOESCAPE | DAMAGE_ELEC, aura_damage, aura_dam);
			if (m_ptr->ml) r_ptr->r_flags2 |= RF2_AURA_ELEC;
			handle_stuff();
		}
	}
}


static void natural_attack(s16b m_idx, int attack, bool *fear, bool *mdeath)
{
	int             k, bonus, chance;
	int             n_weight = 0;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];

	int             dss, ddd;

	cptr            atk_desc;

	int ac = r_ptr->ac;

	if (m_ptr->stoning) ac += m_ptr->stoning / 5;

	switch (attack)
	{
		case MUT2_SCOR_TAIL:
			dss = 3;
			ddd = 7;
			n_weight = 5;
#ifdef JP
			atk_desc = "尻尾";
#else
			atk_desc = "tail";
#endif

			break;
		case MUT2_HORNS:
			dss = 2;
			ddd = 6;
			n_weight = 15;
#ifdef JP
			atk_desc = "角";
#else
			atk_desc = "horns";
#endif

			break;
		case MUT2_BEAK:
			dss = 2;
			ddd = 4;
			n_weight = 5;
#ifdef JP
			atk_desc = "クチバシ";
#else
			atk_desc = "beak";
#endif

			break;
		case MUT2_TRUNK:
			dss = 1;
			ddd = 4;
			n_weight = 35;
#ifdef JP
			atk_desc = "象の鼻";
#else
			atk_desc = "trunk";
#endif

			break;
		case MUT2_TENTACLES:
			dss = 2;
			ddd = 5;
			n_weight = 5;
#ifdef JP
			atk_desc = "触手";
#else
			atk_desc = "tentacles";
#endif

			break;
		default:
			dss = ddd = n_weight = 1;
#ifdef JP
			atk_desc = "未定義の部位";
#else
			atk_desc = "undefined body part";
#endif

	}

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);


	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h_m;
	bonus += (p_ptr->lev * 6 / 5);
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));
	if (chance > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHN)) chance = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHN;

	/* Test for hit */
	if (test_hit_norm(chance, ac, m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_HIT);

#ifdef JP
		msg_format("%sを%sで攻撃した。", m_name, atk_desc);
#else
		msg_format("You hit %s with your %s.", m_name, atk_desc);
#endif


		k = damroll(ddd, dss);
		k = critical_norm(n_weight, bonus, k, bonus, 0);

		/* Apply the player damage bonuses */
		k += p_ptr->to_d_m;

		/* No negative damage */
		if (k < 0) k = 0;

		/* Modify the damage */
		k = mon_damage_mod(m_ptr, k, FALSE);

		/* Anger the monster */
		if (k > 0) anger_monster(m_ptr);

		/* Damage, check for fear and mdeath */
		switch (attack)
		{
			case MUT2_SCOR_TAIL:
				project(0, 0, m_ptr->fy, m_ptr->fx, k, GF_POIS, PROJECT_KILL, MODIFY_ELEM_MODE_MELEE);
				*mdeath = (m_ptr->r_idx == 0);
				break;
			case MUT2_HORNS:
				k = modify_dam_by_elem(0, m_idx, k, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
				*mdeath = mon_take_hit(m_idx, k, fear, NULL, FALSE);
				break;
			case MUT2_BEAK:
				k = modify_dam_by_elem(0, m_idx, k, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
				*mdeath = mon_take_hit(m_idx, k, fear, NULL, FALSE);
				break;
			case MUT2_TRUNK:
				k = modify_dam_by_elem(0, m_idx, k, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
				*mdeath = mon_take_hit(m_idx, k, fear, NULL, FALSE);
				break;
			case MUT2_TENTACLES:
				k = modify_dam_by_elem(0, m_idx, k, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
				*mdeath = mon_take_hit(m_idx, k, fear, NULL, FALSE);
				break;
			default:
				k = modify_dam_by_elem(0, m_idx, k, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);
				*mdeath = mon_take_hit(m_idx, k, fear, NULL, FALSE);
		}

		touch_zap_player(m_ptr);
	}
	/* Player misses */
	else
	{
		/* Sound */
		sound(SOUND_MISS);

		/* Message */
#ifdef JP
			msg_format("ミス！ %sにかわされた。", m_name);
#else
		msg_format("You miss %s.", m_name);
#endif

	}
}



/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
static void py_attack_aux(int y, int x, bool *fear, bool *mdeath, s16b hand, int mode)
{
	int		num = 0, k, bonus, chance, vir;

	cave_type       *c_ptr = &cave[y][x];

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	int ac = r_ptr->ac;

	object_type     *o_ptr;

	char            m_name[80];

	bool            success_hit = FALSE;
	bool            backstab = FALSE;
	bool            vorpal_cut;
	int             chaos_effect = 0;
	bool            stab_fleeing = FALSE;
	bool            do_quake = FALSE;
	bool            weak = FALSE;
	bool            drain_msg = TRUE;
	int             drain_result = 0, drain_heal = 0;
	bool            can_drain = FALSE;
	int             num_blow;
	int             drain_left = MAX_VAMPIRIC_DRAIN;
	u32b            flgs[TR_FLAG_SIZE]; /* A massive hack -- life-draining weapons */
	bool            is_human = (r_ptr->d_char == 'p');
	bool            dragoon_hit = ((p_ptr->pclass == CLASS_DRAGOON) && (r_ptr->flags3 & RF3_DRAGON));



	if (m_ptr->stoning) ac += m_ptr->stoning / 5;
	if (mode == PY_ATTACK_PENET) penet_ac = ac;
	else if (penet_ac) ac += penet_ac;

	if ((p_ptr->pclass == CLASS_NINJA) && buki_motteruka(INVEN_RARM + hand) && !p_ptr->icky_wield[hand])
	{
		if (m_ptr->csleep && m_ptr->ml)
		{
			/* Can't backstab creatures that we can't see, right? */
			backstab = TRUE;
		}
		else if (m_ptr->monfear && m_ptr->ml)
		{
			stab_fleeing = TRUE;
		}
	}

	/* Disturb the monster */
	m_ptr->csleep = 0;
	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2))
		p_ptr->update |= (PU_MON_LITE);

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Access the weapon */
	o_ptr = &inventory[INVEN_RARM+hand];

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h[hand] + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));
	if (chance > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHN)) chance = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHN;

	if ((mode == PY_ATTACK_MINEUCHI) || (mode == PY_ATTACK_3DAN)) num_blow = 1;
	else
	{
		num_blow = p_ptr->num_blow[hand];
		if (mode == PY_ATTACK_CHARGE)
		{
			num_blow /= 2;
			if (!num_blow) num_blow = 1;
		}
	}

	/* Attack once for each legal blow */
	while ((num++ < num_blow) && !p_ptr->is_dead)
	{
		if ((p_ptr->pclass == CLASS_NINJA) && backstab) success_hit = TRUE;
		else success_hit = test_hit_norm(chance, ac, m_ptr->ml);

		/* Test for hit */
		if (success_hit)
		{
			int vorpal_chance;

			/* Sound */
			sound(SOUND_HIT);

			/* Message */
			if (backstab)
#ifdef JP
				msg_format("あなたは冷酷にも眠っている無力な%sを突き刺した！",
#else
				msg_format("You cruelly stab the helpless, sleeping %s!",
#endif

				    m_name);
			else if (stab_fleeing)
#ifdef JP
				msg_format("逃げる%sを背中から突き刺した！",
#else
				msg_format("You backstab the fleeing %s!",
#endif

				    m_name);
			else
			{
				if (!(empty_hands() & EMPTY_HAND_RARM))
#ifdef JP
					msg_format("%sを攻撃した。", m_name);
#else
					msg_format("You hit %s.", m_name);
#endif

			}

			/* Hack -- bare hands do one damage */
			k = 1;

			object_flags(o_ptr, flgs);

			vorpal_chance = have_flag(flgs, TR_EXTRA_VORPAL) ? 2 : 4;

			/* Select a chaotic effect (50% chance) */
			if ((have_flag(flgs, TR_CHAOTIC)) && one_in_(2))
			{
				if (randint1(5) < 3)
				{
					/* Vampiric (20%) */
					chaos_effect = 1;
				}
				else if (one_in_(250))
				{
					/* Quake (0.12%) */
					chaos_effect = 2;
				}
				else if (!one_in_(10))
				{
					/* Confusion (26.892%) */
					chaos_effect = 3;
				}
				else if (one_in_(2))
				{
					/* Teleport away (1.494%) */
					chaos_effect = 4;
				}
				else
				{
					/* Polymorph (1.494%) */
					chaos_effect = 5;
				}

				change_your_alignment_lnc(-1);
			}

			/* Vampiric drain */
			if ((have_flag(flgs, TR_VAMPIRIC)) || (chaos_effect == 1))
			{
				/* Only drain "living" monsters */
				if (monster_living(r_ptr))
					can_drain = TRUE;
				else
					can_drain = FALSE;
			}

			vorpal_cut = FALSE;
			if (have_flag(flgs, TR_VORPAL) || have_flag(flgs, TR_EXTRA_VORPAL))
			{
				if (randint1(vorpal_chance * 3 / 2) == 1) vorpal_cut = TRUE;
			}

			if (empty_hands() & EMPTY_HAND_RARM)
			{
				int special_effect = 0, stun_effect = 0, times = 0, max_times;
				martial_arts *ma_ptr = &ma_blows[0], *old_ptr = &ma_blows[0];
				int resist_stun = 0;
				int weight = 8;

				if (r_ptr->flags1 & RF1_UNIQUE) resist_stun += 88;
				if (r_ptr->flags3 & RF3_NO_STUN) resist_stun += 66;
				if (r_ptr->flags3 & RF3_NO_CONF) resist_stun += 33;
				if (r_ptr->flags3 & RF3_NO_SLEEP) resist_stun += 33;
				if ((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_NONLIVING))
					resist_stun += 66;

				max_times = (p_ptr->lev < 7 ? 1 : p_ptr->lev / 7);
				/* Attempt 'times' */
				for (times = 0; times < max_times; times++)
				{
					do
					{
						ma_ptr = &ma_blows[randint0(MAX_MA)];
					}
					while ((ma_ptr->min_skill_lev > skill_exp_level(p_ptr->skill_exp[SKILL_MARTIAL_ARTS])) ||
					       (randint1(p_ptr->lev) < ma_ptr->chance));

					/* keep the highest level attack available we found */
					if ((ma_ptr->min_skill_lev > old_ptr->min_skill_lev) &&
					    !p_ptr->stun && !p_ptr->confused)
					{
						old_ptr = ma_ptr;

						if (p_ptr->wizard && cheat_xtra)
						{
#ifdef JP
							msg_print("攻撃を再選択しました。");
#else
							msg_print("Attack re-selected.");
#endif

						}
					}
					else
					{
						ma_ptr = old_ptr;
					}
				}

				k = damroll(ma_ptr->dd, ma_ptr->ds);

				if (ma_ptr->effect == MA_KNEE)
				{
					if (r_ptr->flags1 & RF1_MALE)
					{
#ifdef JP
						msg_format("%sに金的膝蹴りをくらわした！", m_name);
#else
						msg_format("You hit %s in the groin with your knee!", m_name);
#endif

						sound(SOUND_PAIN);
						special_effect = MA_KNEE;
					}
					else
						msg_format(ma_ptr->desc, m_name);
				}

				else if (ma_ptr->effect == MA_SLOW)
				{
					if (!((r_ptr->flags1 & RF1_NEVER_MOVE) ||
					    strchr("~#{}.UjmeEv$,DdsbBIJQSXclnw!=?", r_ptr->d_char)))
					{
#ifdef JP
						msg_format("%sの足首に関節蹴りをくらわした！", m_name);
#else
						msg_format("You kick %s in the ankle.", m_name);
#endif

						special_effect = MA_SLOW;
					}
					else msg_format(ma_ptr->desc, m_name);
				}
				else
				{
					if (ma_ptr->effect)
					{
						stun_effect = (ma_ptr->effect / 2) + randint1(ma_ptr->effect / 2);
					}

					msg_format(ma_ptr->desc, m_name);
				}

				k = critical_norm(p_ptr->lev * weight, p_ptr->lev, k, p_ptr->to_h[0], 0);

				if ((special_effect == MA_KNEE) && ((k + p_ptr->to_d[hand]) < m_ptr->hp))
				{
#ifdef JP
					msg_format("%^sは苦痛にうめいている！", m_name);
#else
					msg_format("%^s moans in agony!", m_name);
#endif

					stun_effect = 7 + randint1(13);
					resist_stun /= 3;
				}

				else if ((special_effect == MA_SLOW) && ((k + p_ptr->to_d[hand]) < m_ptr->hp))
				{
					if (!(r_ptr->flags1 & RF1_UNIQUE) &&
					    (randint1(p_ptr->lev) > r_ptr->level) &&
					    m_ptr->mspeed > 60)
					{
#ifdef JP
						msg_format("%^sは足をひきずり始めた。", m_name);
#else
						msg_format("%^s starts limping slower.", m_name);
#endif

						m_ptr->mspeed -= 10;
					}
				}

				if (stun_effect && ((k + p_ptr->to_d[hand]) < m_ptr->hp))
				{
					if (p_ptr->lev > randint1(r_ptr->level + resist_stun + 10))
					{
						if (m_ptr->stunned)
#ifdef JP
							msg_format("%^sはさらにフラフラになった。", m_name);
#else
							msg_format("%^s is more stunned.", m_name);
#endif

						else
#ifdef JP
							msg_format("%^sはフラフラになった。", m_name);
#else
							msg_format("%^s is stunned.", m_name);
#endif


						m_ptr->stunned += stun_effect;
					}
				}
			}

			/* Handle normal weapon */
			else if (o_ptr->k_idx)
			{
				k = damroll(o_ptr->dd + p_ptr->to_dd[hand], o_ptr->ds + p_ptr->to_ds[hand]);
				k = tot_dam_aux(o_ptr, k, m_ptr, TRUE);

				if (backstab)
				{
					k *= (3 + (p_ptr->lev / 20));
				}
				else if (stab_fleeing)
				{
					k = (3 * k) / 2;
				}

				if ((p_ptr->impact[hand] && ((k > 50) || one_in_(7))) ||
					 (chaos_effect == 2))
				{
					do_quake = TRUE;
				}

				k = critical_norm(o_ptr->weight, o_ptr->to_h, k, p_ptr->to_h[hand], mode);

				drain_result = k;

				if (vorpal_cut)
				{
					int mult = 2;

#ifdef JP
					msg_format("%sをグッサリ切り裂いた！", m_name);
#else
					msg_format("Your weapon cuts deep into %s!", m_name);
#endif

					/* Try to increase the damage */
					while (one_in_(vorpal_chance))
					{
						mult++;
					}

					k *= mult;

					/* Ouch! */
					if (k > m_ptr->hp)
					{
#ifdef JP
						msg_format("%sを真っ二つにした！", m_name);
#else
						msg_format("You cut %s in half!", m_name);
#endif

					}
					else
					{
						switch(mult)
						{
#ifdef JP
case 2:	msg_format("%sを斬った！", m_name);		break;
#else
							case 2:	msg_format("You gouge %s!", m_name);		break;
#endif

#ifdef JP
case 3:	msg_format("%sをぶった斬った！", m_name);			break;
#else
							case 3:	msg_format("You maim %s!", m_name);			break;
#endif

#ifdef JP
case 4:	msg_format("%sをメッタ斬りにした！", m_name);		break;
#else
							case 4:	msg_format("You carve %s!", m_name);		break;
#endif

#ifdef JP
case 5:	msg_format("%sをメッタメタに斬った！", m_name);		break;
#else
							case 5:	msg_format("You cleave %s!", m_name);		break;
#endif

#ifdef JP
case 6:	msg_format("%sを刺身にした！", m_name);		break;
#else
							case 6:	msg_format("You smite %s!", m_name);		break;
#endif

#ifdef JP
case 7:	msg_format("%sを斬って斬って斬りまくった！", m_name);	break;
#else
							case 7:	msg_format("You eviscerate %s!", m_name);	break;
#endif

#ifdef JP
default:	msg_format("%sを細切れにした！", m_name);		break;
#else
							default:	msg_format("You shred %s!", m_name);		break;
#endif

						}
					}
					drain_result = drain_result * 3 / 2;
				}

				k += o_ptr->to_d;
				drain_result += o_ptr->to_d;
			}

			/* Hack -- Handle bare hands slaying */
			else
			{
				switch (p_ptr->pclass)
				{
				case CLASS_DRAGOON:
					if (r_ptr->flags3 & RF3_DRAGON)
					{
						k *= 5;
						if (m_ptr->ml) r_ptr->r_flags3 |= RF3_DRAGON;
					}
					break;
				case CLASS_EXORCIST:
					if (r_ptr->flags3 & (RF3_UNDEAD | RF3_DEMON))
					{
						k *= 3;
						if (m_ptr->ml)
						{
							if (r_ptr->flags3 & RF3_UNDEAD) r_ptr->r_flags3 |= RF3_UNDEAD;
							if (r_ptr->flags3 & RF3_DEMON) r_ptr->r_flags3 |= RF3_DEMON;
						}
					}
					break;
				}
			}

			/* Apply the player damage bonuses */
			k += p_ptr->to_d[hand];
			drain_result += p_ptr->to_d[hand];

			if (mode == PY_ATTACK_3DAN) k *= 2;

			/* No negative damage */
			if (k < 0) k = 0;

			if ((mode == PY_ATTACK_MINEUCHI) ||
				(o_ptr->k_idx && (o_ptr->name2 == EGO_EARTHQUAKES)))
			{
				int tmp = (10 + randint1(15) + p_ptr->lev / 5);

				if (mode == PY_ATTACK_MINEUCHI) k = 0;
				anger_monster(m_ptr);

				if (!(r_ptr->flags3 & (RF3_NO_STUN)))
				{
					/* Get stunned */
					if (m_ptr->stunned)
					{
#ifdef JP
msg_format("%sはひどくもうろうとした。", m_name);
#else
						msg_format("%s is more dazed.", m_name);
#endif

						tmp /= 2;
					}
					else
					{
#ifdef JP
msg_format("%sはもうろうとした。", m_name);
#else
						msg_format("%s is dazed.", m_name);
#endif
					}

					/* Apply stun */
					m_ptr->stunned = MIN(m_ptr->stunned + tmp, 200);
				}
				else if (mode == PY_ATTACK_MINEUCHI)
				{
#ifdef JP
msg_format("%s には効果がなかった。", m_name);
#else
						msg_format("%s is not effected.", m_name);
#endif
				}
			}

			/* Modify the damage */
			k = mon_damage_mod(m_ptr, k, (bool)((p_ptr->pclass == CLASS_TERRORKNIGHT) && one_in_(2)));

			if (o_ptr->k_idx)
			{
				if (k > 0) k = k * 10 / tot_dam_div(o_ptr, m_ptr, TRUE);

				if (((p_ptr->pclass == CLASS_NINJA) && buki_motteruka(INVEN_RARM + hand) && !p_ptr->icky_wield[hand]
					 && (one_in_(7))) || dragoon_hit)
				{
					int maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
#ifdef JP
					bool is_hafted = (o_ptr->tval == TV_HAFTED);
#endif

					if (one_in_(backstab ? 13 : (stab_fleeing ? 15 : 27)))
					{
						k *= 5;
						drain_result *= 2;
#ifdef JP
						if (is_hafted) msg_format("%sに深々とめり込んだ！", m_name);
						else msg_format("刃が%sに深々と突き刺さった！", m_name);
#else
						msg_format("You critically injured %s!", m_name);
#endif
					}
					else if (((m_ptr->hp < maxhp/2) && one_in_((p_ptr->num_blow[0]+p_ptr->num_blow[1]+1)*10)) || ((one_in_(666) || (backstab && one_in_(11))) && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags7 & RF7_UNIQUE2)))
					{
						if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_UNIQUE2) || (m_ptr->hp >= maxhp/2))
						{
							k = MAX(k*5, m_ptr->hp/2);
							drain_result *= 2;
#ifdef JP
							msg_format("%sに致命傷を負わせた！", m_name);
#else
							msg_format("You fatally injured %s!", m_name);
#endif
						}
						else
						{
							k = MAX(k, m_ptr->hp + 1);
#ifdef JP
							if (is_hafted) msg_format("%sの急所を直撃した！", m_name);
							else msg_format("刃が%sの急所を貫いた！", m_name);
#else
							msg_format("You hit %s on a fatal spot!", m_name);
#endif
						}
					}
				}
			}

			k = modify_dam_by_elem(0, c_ptr->m_idx, k, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);

			if (k <= 0) can_drain = FALSE;

			if (drain_result > m_ptr->hp)
				drain_result = m_ptr->hp;

			/* Damage, check for fear and death */
			if (mon_take_hit(c_ptr->m_idx, k, fear, NULL, FALSE))
			{
				*mdeath = TRUE;
				if ((p_ptr->pclass == CLASS_TERRORKNIGHT) && energy_use)
				{
					if (p_ptr->migite && p_ptr->hidarite)
					{
						if (hand) energy_use = energy_use*3/5+energy_use*num*2/(p_ptr->num_blow[hand]*5);
						else energy_use = energy_use*num*3/(p_ptr->num_blow[hand]*5);
					}
					else
					{
						energy_use = energy_use*num/p_ptr->num_blow[hand];
					}
				}

				break;
			}

			/* Anger the monster */
			if (k > 0) anger_monster(m_ptr);

			touch_zap_player(m_ptr);

			/* Are we draining it?  A little note: If the monster is
			dead, the drain does not work... */

			if (can_drain && (drain_result > 0))
			{
					if (drain_result > 5) /* Did we really hurt it? */
					{
						drain_heal = damroll(2, drain_result / 6);

						if (cheat_xtra)
						{
#ifdef JP
							msg_format("Draining left: %d", drain_left);
#else
							msg_format("Draining left: %d", drain_left);
#endif

						}

						if (drain_left)
						{
							if (drain_heal < drain_left)
							{
								drain_left -= drain_heal;
							}
							else
							{
								drain_heal = drain_left;
								drain_left = 0;
							}

							if (drain_msg)
							{
#ifdef JP
								msg_format("刃が%sから生命力を吸い取った！", m_name);
#else
								msg_format("Your weapon drains life from %s!", m_name);
#endif

								drain_msg = FALSE;
							}

							drain_heal = (drain_heal * mutant_regenerate_mod) / 100;

							hp_player(drain_heal);
							/* We get to keep some of it! */
						}
					}
				m_ptr->maxhp -= (k+7)/8;
				if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
				weak = TRUE;
			}
			can_drain = FALSE;
			drain_result = 0;

			/* Confusion attack */
			if ((p_ptr->special_attack & ATTACK_CONFUSE) || (chaos_effect == 3))
			{
				/* Cancel glowing hands */
				if (p_ptr->special_attack & ATTACK_CONFUSE)
				{
					p_ptr->special_attack &= ~(ATTACK_CONFUSE);
#ifdef JP
					msg_print("手の輝きがなくなった。");
#else
					msg_print("Your hands stop glowing.");
#endif
					p_ptr->redraw |= (PR_STATUS);

				}

				/* Confuse the monster */
				if (r_ptr->flags3 & RF3_NO_CONF)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_NO_CONF;
					}

#ifdef JP
					msg_format("%^sには効果がなかった。", m_name);
#else
					msg_format("%^s is unaffected.", m_name);
#endif

				}
				else if (randint0(100) < r_ptr->level)
				{
#ifdef JP
					msg_format("%^sには効果がなかった。", m_name);
#else
					msg_format("%^s is unaffected.", m_name);
#endif

				}
				else
				{
#ifdef JP
					msg_format("%^sは混乱したようだ。", m_name);
#else
					msg_format("%^s appears confused.", m_name);
#endif

					m_ptr->confused += 10 + randint0(p_ptr->lev) / 5;
				}
			}

			else if (chaos_effect == 4)
			{
				bool resists_tele = FALSE;

				if (r_ptr->flags3 & RF3_RES_TELE)
				{
					if (r_ptr->flags1 & RF1_UNIQUE)
					{
						if (m_ptr->ml) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
						msg_format("%^sには効果がなかった。", m_name);
#else
						msg_format("%^s is unaffected!", m_name);
#endif

						resists_tele = TRUE;
					}
					else if (r_ptr->level > randint1(100))
					{
						if (m_ptr->ml) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
						msg_format("%^sは抵抗力を持っている！", m_name);
#else
						msg_format("%^s resists!", m_name);
#endif

						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
#ifdef JP
					msg_format("%^sは消えた！", m_name);
#else
					msg_format("%^s disappears!", m_name);
#endif

					teleport_away(c_ptr->m_idx, 50);
					num = p_ptr->num_blow[hand] + 1; /* Can't hit it anymore! */
					*mdeath = TRUE;
				}
			}

			else if ((chaos_effect == 5) && cave_floor_bold(y, x) &&
			         (randint1(90) > r_ptr->level))
			{
				if (!(r_ptr->flags1 & RF1_UNIQUE) &&
				    !(r_ptr->flagsr & RFR_RES_CHAO) &&
				    !(r_ptr->flags1 & RF1_QUESTOR))
				{
					if (polymorph_monster(y, x, TRUE))
					{
#ifdef JP
						msg_format("%^sは変化した！", m_name);
#else
						msg_format("%^s changes!", m_name);
#endif


						/* Hack -- Get new monster */
						m_ptr = &m_list[c_ptr->m_idx];

						/* Oops, we need a different name... */
						monster_desc(m_name, m_ptr, 0);

						/* Hack -- Get new race */
						r_ptr = &r_info[m_ptr->r_idx];

						*fear = FALSE;
						weak = FALSE;
					}
					else
					{
#ifdef JP
					msg_format("%^sには効果がなかった。", m_name);
#else
						msg_format("%^s is unaffected.", m_name);
#endif

					}
				}
			}
		}

		/* Player misses */
		else
		{
			backstab = FALSE; /* Clumsy! */

			/* Sound */
			sound(SOUND_MISS);

			/* Message */
#ifdef JP
			msg_format("ミス！ %sにかわされた。", m_name);
#else
			msg_format("You miss %s.", m_name);
#endif
		}
		backstab = FALSE;
	}


	if (weak && !(*mdeath))
	{
#ifdef JP
		msg_format("%sは弱くなったようだ。", m_name);
#else
		msg_format("%^s seems weakened.", m_name);
#endif
	}

	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		earthquake(py, px, 10);
		if (!cave[y][x].m_idx) *mdeath = TRUE;
	}
}

bool py_attack(int y, int x, int mode)
{
	bool            fear = FALSE;
	bool            mdeath = FALSE;
	bool            darksword = FALSE;

	cave_type       *c_ptr = &cave[y][x];
	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];

	/* Disturb the player */
	disturb(0, 0);

	energy_use = 100;

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->ap_r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(c_ptr->m_idx);

	/* Stop if friendly */
	if (!is_hostile(m_ptr) &&
	    !(p_ptr->stun || p_ptr->confused || p_ptr->image ||
	    p_ptr->shero || !m_ptr->ml))
	{
		if ((inventory[INVEN_RARM].tval == TV_SWORD) && ((inventory[INVEN_RARM].sval == SV_YOUTOU) || (inventory[INVEN_RARM].sval == SV_DARK_SWORD))) darksword = TRUE;
		if ((inventory[INVEN_LARM].tval == TV_SWORD) && ((inventory[INVEN_LARM].sval == SV_YOUTOU) || (inventory[INVEN_LARM].sval == SV_DARK_SWORD))) darksword = TRUE;
		if (darksword)
		{
#ifdef JP
			msg_format("黒い刃は強欲に%sを攻撃した！", m_name);
#else
			msg_format("Your black blade greedily attacks %s!", m_name);
#endif
		}
		else
		{
#ifdef JP
			if (!get_check("本当に攻撃しますか？"))
#else
			if (!get_check("Really hit it? "))
#endif
			{
#ifdef JP
				msg_format("%sを攻撃するのを止めた。", m_name);
#else
				msg_format("You stop to avoid hitting %s.", m_name);
#endif
				return FALSE;
			}
		}
	}


	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		if (m_ptr->ml)
#ifdef JP
		msg_format("恐くて%sを攻撃できない！", m_name);
#else
			msg_format("You are too afraid to attack %s!", m_name);
#endif

		else
#ifdef JP
			msg_format ("そっちには何か恐いものがいる！");
#else
			msg_format ("There is something scary in your way!");
#endif

		/* Disturb the monster */
		m_ptr->csleep = 0;
		if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

		/* Done */
		return FALSE;
	}

	riding_t_m_idx = c_ptr->m_idx;
	if (p_ptr->migite) py_attack_aux(y, x, &fear, &mdeath, 0, mode);
	if (p_ptr->hidarite && !mdeath) py_attack_aux(y, x, &fear, &mdeath, 1, mode);

	/* Mutations which yield extra 'natural' attacks */
	if (!mdeath)
	{
		if ((p_ptr->muta2 & MUT2_HORNS) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_HORNS, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_BEAK) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_BEAK, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_SCOR_TAIL) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_SCOR_TAIL, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_TRUNK) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_TRUNK, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_TENTACLES) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_TENTACLES, &fear, &mdeath);
	}

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml && !mdeath)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
#ifdef JP
		msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
		msg_format("%^s flees in terror!", m_name);
#endif

	}

	return mdeath;
}



bool player_can_enter(byte feature)
{
	bool pass_wall;

	/* Player can not walk through "walls" unless in Shadow Form */
	if (WRAITH_FORM() || p_ptr->pass_wall)
		pass_wall = TRUE;
	else
		pass_wall = FALSE;

	switch (feature)
	{
		case FEAT_DARK_PIT:
		{
			return (p_ptr->ffall);
		}

		case FEAT_AIR:
		{
			if (!p_ptr->ffall)
			{
				if ((!(d_info[dungeon_type].flags1 & DF1_UPWARD) && (dun_level >= d_info[dungeon_type].maxdepth)) ||
				     ((d_info[dungeon_type].flags1 & DF1_UPWARD) && (dun_level <= d_info[dungeon_type].mindepth)))
					return FALSE;
				else
					return TRUE;
			}
			else
				return (TRUE);
		}

		case FEAT_RUBBLE:
		case FEAT_MAGMA:
		case FEAT_QUARTZ:
		case FEAT_MAGMA_H:
		case FEAT_QUARTZ_H:
		case FEAT_MAGMA_K:
		case FEAT_QUARTZ_K:
		case FEAT_WALL_EXTRA:
		case FEAT_WALL_INNER:
		case FEAT_WALL_OUTER:
		case FEAT_WALL_SOLID:
		{
			return (pass_wall);
		}

		case FEAT_MOUNTAIN:
		{
			return (!dun_level && p_ptr->ffall);
		}
		case FEAT_PERM_EXTRA:
		case FEAT_PERM_INNER:
		case FEAT_PERM_OUTER:
		case FEAT_PERM_SOLID:
		{
			return (FALSE);
		}
	}

	return (TRUE);
}


/*
 * Fall into air
 */
void fall_into_air(void)
{
#ifdef JP
	msg_print("空中に落ちた！");
#else
	msg_print("You have fallen into air!");
#endif

	sound(SOUND_FALL);

	if ((!(d_info[dungeon_type].flags1 & DF1_UPWARD) && (dun_level >= d_info[dungeon_type].maxdepth)) ||
	     ((d_info[dungeon_type].flags1 & DF1_UPWARD) && (dun_level <= d_info[dungeon_type].mindepth)))
	{
#ifdef JP
		msg_print("あなたはどこまでも落ちていく...");
#else
		msg_print("You are falling long long way...");
#endif

		/* Fatal falling and *DIE* */
#ifdef JP
		take_hit(DAMAGE_USELIFE, p_ptr->chp + 1, "空中への致命的な落下");
#else
		take_hit(DAMAGE_USELIFE, p_ptr->chp + 1, "Fatal falling into air");
#endif
	}
	else
	{
		/* Still alive and autosave enabled */
		if (autosave_l && (p_ptr->chp >= 0))
			do_cmd_save_game(TRUE);

#ifdef JP
		do_cmd_write_nikki(NIKKI_STAIR, 1, "空中に落ちた");
#else
		do_cmd_write_nikki(NIKKI_STAIR, 1, "You have fallen into air!");
#endif
		prepare_change_floor_mode(CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(int dir, int do_pickup)
{
	int y, x;

	cave_type *c_ptr;
	monster_type *m_ptr;

	monster_type *riding_m_ptr = &m_list[p_ptr->riding];
	monster_race *riding_r_ptr = &r_info[p_ptr->riding ? riding_m_ptr->r_idx : 0]; /* Paranoia */

	char m_name[80];

	bool p_can_pass_walls = FALSE;
	bool darksword = FALSE;

	bool oktomove = TRUE;
	bool do_past = FALSE;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Examine the destination */
	c_ptr = &cave[y][x];

	/* Exit the area */
	if (!dun_level && !p_ptr->wild_mode && !ironman_forward && !astral_mode &&
		((x == 0) || (x == MAX_WID - 1) ||
		 (y == 0) || (y == MAX_HGT - 1)))
	{
		/* Can the player enter the grid? */
		if (c_ptr->mimic && player_can_enter(c_ptr->mimic))
		{
			s32b old_wilderness_y = p_ptr->wilderness_y;
			s32b old_wilderness_x = p_ptr->wilderness_x;
			s16b old_oldpy = p_ptr->oldpy;
			s16b old_oldpx = p_ptr->oldpx;
			bool old_ambush_flag = ambush_flag;

			/* Hack: move to new area */
			if ((y == 0) && (x == 0))
			{
				p_ptr->wilderness_y--;
				p_ptr->wilderness_x--;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = cur_wid - 2;
				ambush_flag = FALSE;
			}

			else if ((y == 0) && (x == MAX_WID - 1))
			{
				p_ptr->wilderness_y--;
				p_ptr->wilderness_x++;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = 1;
				ambush_flag = FALSE;
			}

			else if ((y == MAX_HGT - 1) && (x == 0))
			{
				p_ptr->wilderness_y++;
				p_ptr->wilderness_x--;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = cur_wid - 2;
				ambush_flag = FALSE;
			}

			else if ((y == MAX_HGT - 1) && (x == MAX_WID - 1))
			{
				p_ptr->wilderness_y++;
				p_ptr->wilderness_x++;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = 1;
				ambush_flag = FALSE;
			}

			else if (y == 0)
			{
				p_ptr->wilderness_y--;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = x;
				ambush_flag = FALSE;
			}

			else if (y == MAX_HGT - 1)
			{
				p_ptr->wilderness_y++;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = x;
				ambush_flag = FALSE;
			}

			else if (x == 0)
			{
				p_ptr->wilderness_x--;
				p_ptr->oldpx = cur_wid - 2;
				p_ptr->oldpy = y;
				ambush_flag = FALSE;
			}

			else if (x == MAX_WID - 1)
			{
				p_ptr->wilderness_x++;
				p_ptr->oldpx = 1;
				p_ptr->oldpy = y;
				ambush_flag = FALSE;
			}

			if ((p_ptr->wilderness_y > 0) && (p_ptr->wilderness_y < (max_wild_y - 1)) &&
				(p_ptr->wilderness_x > 0) && (p_ptr->wilderness_x < (max_wild_x - 1)))
			{
				p_ptr->leftbldg = TRUE;
				p_ptr->leaving = TRUE;
				energy_use = 100;

				return;
			}
			else
			{
				p_ptr->wilderness_y = old_wilderness_y;
				p_ptr->wilderness_x = old_wilderness_x;
				p_ptr->oldpy = old_oldpy;
				p_ptr->oldpx = old_oldpx;
				ambush_flag = old_ambush_flag;
#ifdef JP
				msg_print("それ以上先には進めない。");
#else
				msg_print("You cannot go any more.");
#endif
				return;
			}
		}

		oktomove = FALSE;
	}

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];


	if ((inventory[INVEN_RARM].tval == TV_SWORD) && ((inventory[INVEN_RARM].sval == SV_YOUTOU) || (inventory[INVEN_RARM].sval == SV_DARK_SWORD))) darksword = TRUE;
	if ((inventory[INVEN_LARM].tval == TV_SWORD) && ((inventory[INVEN_LARM].sval == SV_YOUTOU) || (inventory[INVEN_LARM].sval == SV_DARK_SWORD))) darksword = TRUE;

	/* Player can not walk through "walls"... */
	/* unless in Shadow Form */
	if (WRAITH_FORM() || p_ptr->pass_wall)
		p_can_pass_walls = TRUE;
	if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
	    (c_ptr->feat <= FEAT_PERM_SOLID))
	{
		p_can_pass_walls = FALSE;
	}

	/* Hack -- attack monsters */
	if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x) || p_can_pass_walls))
	{
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Attack -- only if we can see it OR it is not in a wall */
		if (!is_hostile(m_ptr) &&
		    !(p_ptr->confused || p_ptr->image || !m_ptr->ml || p_ptr->stun ||
		    ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero)) &&
		    ((cave_floor_bold(y, x)) || (c_ptr->feat == FEAT_TREES) || (p_can_pass_walls)))
		{
			m_ptr->csleep = 0;
			if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

			/* Extract monster name (or "it") */
			monster_desc(m_name, m_ptr, 0);

			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->ap_r_idx);

			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);

			/* displace? */
			if (darksword && (randint1(1000) > 666))
			{
				py_attack(y, x, 0);
				oktomove = FALSE;
			}
			else if (monster_can_cross_terrain(cave[py][px].feat, r_ptr) &&
				 (cave_floor_bold(py, px) || cave[py][px].feat == FEAT_TREES ||
				  (r_ptr->flags2 & RF2_PASS_WALL)))
			{
				do_past = TRUE;
			}
			else
			{
#ifdef JP
				msg_format("%^sが邪魔だ！", m_name);
#else
				msg_format("%^s is in your way!", m_name);
#endif

				energy_use = 0;
				oktomove = FALSE;
			}

			/* now continue on to 'movement' */
		}
		else
		{
			py_attack(y, x, 0);
			oktomove = FALSE;
		}
	}

	if (!oktomove)
	{
	}

	else if ((c_ptr->feat == FEAT_DARK_PIT) && !p_ptr->ffall)
	{
#ifdef JP
		msg_print("裂け目を横切ることはできません。");
#else
		msg_print("You can't cross the chasm.");
#endif

		energy_use = 0;
		running = 0;
		oktomove = FALSE;
	}

	else if (c_ptr->feat == FEAT_MOUNTAIN)
	{
		if (dun_level || !p_ptr->ffall)
		{
#ifdef JP
			msg_print("山には登れません！");
#else
			msg_print("You can't climb the mountains!");
#endif

			running = 0;
			energy_use = 0;
			oktomove = FALSE;
		}
	}

	else if ((c_ptr->feat == FEAT_AIR) && !p_ptr->ffall)
	{
		if (((c_ptr->info & (CAVE_MARK)) || (!p_ptr->blind && (c_ptr->info & (CAVE_LITE)))) && !p_ptr->confused)
		{
			if (!player_can_enter(FEAT_AIR))
			{
#ifdef JP
				msg_print("ここから空中に踏み出すのは危険だ。");
#else
				msg_print("Crossing the air in this level is danger.");
#endif
				energy_use = 0;
				running = 0;
				oktomove = FALSE;
			}
#ifdef JP
			else if (!get_check("空中に踏み出します。よろしいですか？"))
#else
			else if (!get_check("You cross the air. Really? "))
#endif
			{
				energy_use = 0;
				running = 0;
				oktomove = FALSE;
			}
		}
	}
	/*
	 * Player can move through trees and
	 * has effective -10 speed
	 * Rangers can move without penality
	 */
	else if (c_ptr->feat == FEAT_TREES)
	{
		oktomove = TRUE;
		if (!p_ptr->ffall) energy_use *= 2;
	}

	else if ((c_ptr->feat >= FEAT_QUEST_ENTER) &&
		(c_ptr->feat <= FEAT_QUEST_EXIT))
	{
		oktomove = TRUE;
	}

#ifdef ALLOW_EASY_DISARM /* TNB */

	/* Disarm a visible trap */
	else if ((do_pickup != easy_disarm) && is_known_trap(c_ptr))
	{
		bool ignore = FALSE;
		switch (c_ptr->feat)
		{
			case FEAT_TRAP_TRAPDOOR:
			case FEAT_TRAP_PIT:
			case FEAT_TRAP_SPIKED_PIT:
			case FEAT_TRAP_POISON_PIT:
				if (p_ptr->ffall) ignore = TRUE;
				break;
			case FEAT_TRAP_TELEPORT:
				if (p_ptr->anti_tele || p_ptr->earth_spike) ignore = TRUE;
				break;
			case FEAT_TRAP_FIRE:
				if (p_ptr->immune_fire) ignore = TRUE;
				break;
			case FEAT_TRAP_ACID:
				if (p_ptr->immune_acid) ignore = TRUE;
				break;
			case FEAT_TRAP_BLIND:
				if (p_ptr->resist_blind) ignore = TRUE;
				break;
			case FEAT_TRAP_CONFUSE:
				if (p_ptr->resist_conf) ignore = TRUE;
				break;
			case FEAT_TRAP_POISON:
				if (p_ptr->resist_pois) ignore = TRUE;
				break;
			case FEAT_TRAP_SLEEP:
				if (p_ptr->free_act) ignore = TRUE;
				break;
		}

		if (!ignore)
		{
			(void)do_cmd_disarm_aux(y, x, dir);
			return;
		}
	}

#endif /* ALLOW_EASY_DISARM -- TNB */
	else if (p_ptr->riding && (riding_r_ptr->flags1 & RF1_NEVER_MOVE))
	{
#ifdef JP
		msg_print("動けない！");
#else
		msg_print("Can't move!");
#endif
		energy_use = 0;
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if (p_ptr->riding && riding_m_ptr->monfear)
	{
		char m_name[80];

		/* Acquire the monster name */
		monster_desc(m_name, riding_m_ptr, 0);

		/* Dump a message */
#ifdef JP
		msg_format("%sが恐怖していて制御できない。", m_name);
#else
		msg_format("%^s is too scared to control.", m_name);
#endif
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if (p_ptr->riding && p_ptr->riding_ryoute)
	{
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if ((p_ptr->riding && (riding_r_ptr->flags7 & RF7_AQUATIC)) && (c_ptr->feat != FEAT_SHAL_WATER) && (c_ptr->feat != FEAT_DEEP_WATER) && (c_ptr->feat != FEAT_SWAMP))
	{
#ifdef JP
		msg_print("陸上に上がれない。");
#else
		msg_print("Can't land.");
#endif
		energy_use = 0;
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if ((p_ptr->riding && !(riding_r_ptr->flags7 & (RF7_AQUATIC | RF7_CAN_SWIM | RF7_CAN_FLY))) && (c_ptr->feat == FEAT_DEEP_WATER))
	{
#ifdef JP
		msg_print("水上に行けない。");
#else
		msg_print("Can't swim.");
#endif
		energy_use = 0;
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if ((p_ptr->riding && (riding_r_ptr->flags2 & RF2_AURA_FIRE) && !(riding_r_ptr->flags7 & RF7_CAN_FLY)) && (c_ptr->feat == FEAT_SHAL_WATER))
	{
#ifdef JP
		msg_print("水上に行けない。");
#else
		msg_print("Can't swim.");
#endif
		energy_use = 0;
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if ((p_ptr->riding && !(riding_r_ptr->flags7 & (RF7_CAN_FLY)) && !(riding_r_ptr->flagsr & (RFR_RES_FIRE))) && ((c_ptr->feat == FEAT_SHAL_LAVA) || (c_ptr->feat == FEAT_DEEP_LAVA)))
	{
#ifdef JP
		msg_print("溶岩の上に行けない。");
#else
		msg_print("Too hot to go through.");
#endif
		energy_use = 0;
		oktomove = FALSE;
		disturb(0, 0);
	}

	else if (p_ptr->riding && riding_m_ptr->stunned && one_in_(2))
	{
		char m_name[80];
		monster_desc(m_name, riding_m_ptr, 0);
#ifdef JP
		msg_format("%sが朦朧としていてうまく動けない！",m_name);
#else
		msg_format("You cannot control stunned %s!",m_name);
#endif
		oktomove = FALSE;
		disturb(0, 0);
	}

	/* Player can not walk through "walls" unless in wraith form...*/
	else if ((!cave_floor_bold(y, x)) &&
		(!p_can_pass_walls))
	{
		/* Feature code (applying "mimic" field) */
		byte feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		oktomove = FALSE;

		/* Disturb the player */
		disturb(0, 0);

		/* Notice things in the dark */
		if (!(c_ptr->info & (CAVE_MARK)) &&
		     (p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Rubble */
			if (feat == FEAT_RUBBLE)
			{
#ifdef JP
				msg_print("岩石が行く手をはばんでいるようだ。");
#else
				msg_print("You feel some rubble blocking your way.");
#endif

				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Closed door */
			else if (is_closed_door(feat))
			{
#ifdef JP
				msg_print("ドアが行く手をはばんでいるようだ。");
#else
				msg_print("You feel a closed door blocking your way.");
#endif

				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Boundary floor mimic */
			else if (boundary_floor_grid(c_ptr))
			{
				if ((c_ptr->mimic == FEAT_AIR) && !p_ptr->ffall)
				{
					fall_into_air();
				}
				else
				{
#ifdef JP
					msg_print("それ以上先には進めないようだ。");
#else
					msg_print("You feel you cannot go any more.");
#endif
				}
			}

			/* Wall (or secret door) */
			else
			{
#ifdef JP
				msg_print("壁が行く手をはばんでいるようだ。");
#else
				msg_print("You feel a wall blocking your way.");
#endif

				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}

		/* Notice things */
		else
		{
			/* Rubble */
			if (feat == FEAT_RUBBLE)
			{
#ifdef JP
				msg_print("岩石が行く手をはばんでいる。");
#else
				msg_print("There is rubble blocking your way.");
#endif


				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;

				/*
				 * Well, it makes sense that you lose time bumping into
				 * a wall _if_ you are confused, stunned or blind; but
				 * typing mistakes should not cost you a turn...
				 */
			}
			/* Closed doors */
			else if (is_closed_door(feat))
			{
#ifdef ALLOW_EASY_OPEN

				if (easy_open && easy_open_door(y, x)) return;

#endif /* ALLOW_EASY_OPEN */

#ifdef JP
				msg_print("ドアが行く手をはばんでいる。");
#else
				msg_print("There is a closed door blocking your way.");
#endif


				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			/* Boundary floor mimic */
			else if (boundary_floor_grid(c_ptr))
			{
				if ((c_ptr->mimic == FEAT_AIR) && !p_ptr->ffall)
				{
					if (p_ptr->confused)
					{
						fall_into_air();
					}
					else if (!player_can_enter(FEAT_AIR))
					{
#ifdef JP
						msg_print("ここから空中に踏み出すのは危険だ。");
#else
						msg_print("Crossing the air in this level is danger.");
#endif

					if (!(p_ptr->stun || p_ptr->image))
						energy_use = 0;
					}
#ifdef JP
					else if (get_check("空中に踏み出します。よろしいですか？"))
#else
					else if (get_check("You cross the air. Really? "))
#endif
					{
						fall_into_air();
					}
					else if (!(p_ptr->stun || p_ptr->image))
						energy_use = 0;
				}
				else
				{
#ifdef JP
					msg_print("それ以上先には進めない。");
#else
					msg_print("You cannot go any more.");
#endif

					if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
						energy_use = 0;
				}
			}

			/* Wall (or secret door) */
			else
			{
#ifdef JP
				msg_print("壁が行く手をはばんでいる。");
#else
				msg_print("There is a wall blocking your way.");
#endif


				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}
		}

		/* Sound */
		if (!boundary_floor_grid(c_ptr)) sound(SOUND_HITWALL);
	}

	/* Normal movement */
	if (oktomove)
	{
		int oy, ox;

		if (p_ptr->warning)
		{
			if (!process_warning(x, y))
			{
				energy_use = 25;
				return;
			}
		}

		/* Hack -- For moving monster or riding player's moving */
		cave[py][px].m_idx = c_ptr->m_idx;
		c_ptr->m_idx = 0;

		if (do_past)
		{
#ifdef JP
			msg_format("%sを押し退けた。", m_name);
#else
			msg_format("You push past %s.", m_name);
#endif

			m_ptr->fy = py;
			m_ptr->fx = px;
			update_mon(cave[py][px].m_idx, TRUE);
		}

		/* Change oldpx and oldpy to place the player well when going back to big mode */
		if (p_ptr->wild_mode)
		{
			if(ddy[dir] > 0)  p_ptr->oldpy = 1;
			if(ddy[dir] < 0)  p_ptr->oldpy = MAX_HGT - 2;
			if(ddy[dir] == 0) p_ptr->oldpy = MAX_HGT / 2;
			if(ddx[dir] > 0)  p_ptr->oldpx = 1;
			if(ddx[dir] < 0)  p_ptr->oldpx = MAX_WID - 2;
			if(ddx[dir] == 0) p_ptr->oldpx = MAX_WID / 2;
		}

		/* Save old location */
		oy = py;
		ox = px;

		/* Move the player */
		py = y;
		px = x;

		if (p_ptr->kill_wall || (p_ptr->riding && (r_info[m_list[p_ptr->riding].r_idx].flags2 & RF2_KILL_WALL)))
		{
			if (!cave_floor_bold(py, px) && cave_valid_bold(py, px) &&
				(cave[py][px].feat < FEAT_DEEP_WATER ||
				 cave[py][px].feat > FEAT_GRASS))
			{
				/* Forget the wall */
				cave[py][px].info &= ~(CAVE_MARK);

				cave_force_set_floor(py, px);
			}
			/* Update some things -- similar to GF_KILL_WALL */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
		}

		/* Remove "unsafe" flag */
		if ((!p_ptr->blind && !no_lite()) || !is_trap(c_ptr->feat)) c_ptr->info &= ~(CAVE_UNSAFE);

		if (p_ptr->riding)
		{
			riding_m_ptr->fy = py;
			riding_m_ptr->fx = px;
			cave[py][px].m_idx = p_ptr->riding;
			update_mon(p_ptr->riding, TRUE);
			if (riding_r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_SELF_LITE_1 | RF7_HAS_LITE_2 | RF7_SELF_LITE_2))
				p_ptr->update |= (PU_MON_LITE);
		}

		/* Redraw new spot */
		lite_spot(py, px);

		/* Redraw old spot */
		lite_spot(oy, ox);

		/* Sound */
		/* sound(SOUND_WALK); */

		/* Check for new panel (redraw map) */
		verify_panel();

		set_mermaid_in_water();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
		    (0 == randint0(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->action == ACTION_SEARCH)
		{
			search();
		}

		/* Handle "objects" */

#ifdef ALLOW_EASY_DISARM /* TNB */

		carry(do_pickup != always_pickup);

#else /* ALLOW_EASY_DISARM -- TNB */

		carry(do_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

		/* Handle "store doors" */
		if (((c_ptr->feat >= FEAT_SHOP_HEAD) &&
		    (c_ptr->feat <= FEAT_SHOP_TAIL)) ||
		    (c_ptr->feat == FEAT_MUSEUM))
		{
			/* Disturb */
			disturb(0, 0);

			energy_use = 0;
			/* Hack -- Enter store */
			command_new = SPECIAL_KEY_STORE;
		}

		/* Handle "building doors" -KMW- */
		else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
		    (c_ptr->feat <= FEAT_BLDG_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			energy_use = 0;
			/* Hack -- Enter building */
			command_new = SPECIAL_KEY_BUILDING;
		}

		/* Handle quest areas -KMW- */
		else if (c_ptr->feat == FEAT_QUEST_ENTER)
		{
			/* Disturb */
			disturb(0, 0);

			energy_use = 0;
			/* Hack -- Enter quest level */
			command_new = SPECIAL_KEY_QUEST;
		}

		else if (c_ptr->feat == FEAT_QUEST_EXIT)
		{
			if (quest[p_ptr->inside_quest].type == QUEST_TYPE_FIND_EXIT)
			{
				if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, p_ptr->inside_quest, NULL);
				quest[p_ptr->inside_quest].status = QUEST_STATUS_COMPLETED;
				quest[p_ptr->inside_quest].complev = (byte)p_ptr->lev;

				/* Make a sound */
				sound(SOUND_QUEST);

#ifdef JP
				msg_print("クエストを達成した！");
#else
				msg_print("You accomplished your quest!");
#endif

				msg_print(NULL);
				if (quest_is_fixed(p_ptr->inside_quest)) change_your_alignment_lnc(10);
			}

			leave_quest_check();

			p_ptr->inside_quest = c_ptr->special;
			dun_level = 0;
			p_ptr->oldpx = 0;
			p_ptr->oldpy = 0;
			p_ptr->leaving = TRUE;
		}

		/* Handle "air" */
		else if (c_ptr->feat == FEAT_AIR)
		{
			if (!p_ptr->ffall) fall_into_air();
		}

		/* Set off a trap */
		else if (is_trap(c_ptr->feat))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hidden trap */
			if (c_ptr->mimic)
			{
				/* Message */
#ifdef JP
				msg_print("トラップだ！");
#else
				msg_print("You found a trap!");
#endif

				/* Pick a trap */
				disclose_grid(py, px);
			}

			/* Hit the trap */
			hit_trap();
		}

		if (IN_HEAVEN_GATE())
		{
			if ((c_ptr->mimic == FEAT_FLOOR) &&
				((c_ptr->feat == FEAT_GRASS) || (c_ptr->feat == FEAT_DEEP_GRASS)))
			{
				int i;

				for (i = 0; i < INVEN_TOTAL; i++)
				{
					if (inventory[i].k_idx && (inventory[i].name1 == ART_BRUNHILD))
					{
						msg_print("ブリュンヒルドは激しく震えだした！");
						break;
					}
				}
			}
		}

		/* Warn when leaving trap detected region */
		if ((disturb_trap_detect || alert_trap_detect)
		    && p_ptr->dtrap && !(cave[py][px].info & CAVE_IN_DETECT))
		{
			/* No duplicate warning */
			p_ptr->dtrap = FALSE;

			/* You are just on the edge */
			if (!(cave[py][px].info & CAVE_UNSAFE))
			{
				if (alert_trap_detect)
				{
#ifdef JP
					msg_print("* 注意:この先はトラップの感知範囲外です！ *");
#else
					msg_print("*Leaving trap detect region!*");
#endif
				}

				if (disturb_trap_detect)
				{
					disturb(0, 0);
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
	cave_type   *c_ptr;
	byte feat;

	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls */
	if (!in_bounds2(y, x)) return (FALSE);

	/* Access grid */
	c_ptr = &cave[y][x];

	/* Feature code (applying "mimic" field) */
	feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

	/* Must be known to the player */
	if (c_ptr->info & (CAVE_MARK))
	{
		/* Non-wall grids are not known walls */
		switch (feat)
		{
		case FEAT_NONE:
		case FEAT_FLOOR:
		case FEAT_INVIS:
		case FEAT_GLYPH:
		case FEAT_OPEN:
		case FEAT_BROKEN:
		case FEAT_LESS:
		case FEAT_MORE:
		case FEAT_QUEST_ENTER:
		case FEAT_QUEST_EXIT:
		case FEAT_QUEST_DOWN:
		case FEAT_QUEST_UP:
		case FEAT_LESS_LESS:
		case FEAT_MORE_MORE:
		case FEAT_TRAP_TRAPDOOR:
		case FEAT_TRAP_PIT:
		case FEAT_TRAP_SPIKED_PIT:
		case FEAT_TRAP_POISON_PIT:
		case FEAT_TRAP_TY_CURSE:
		case FEAT_TRAP_TELEPORT:
		case FEAT_TRAP_FIRE:
		case FEAT_TRAP_ACID:
		case FEAT_TRAP_SLOW:
		case FEAT_TRAP_LOSE_STR:
		case FEAT_TRAP_LOSE_DEX:
		case FEAT_TRAP_LOSE_CON:
		case FEAT_TRAP_BLIND:
		case FEAT_TRAP_CONFUSE:
		case FEAT_TRAP_POISON:
		case FEAT_TRAP_SLEEP:
		case FEAT_DEEP_WATER:
		case FEAT_SHAL_WATER:
		case FEAT_DEEP_LAVA:
		case FEAT_SHAL_LAVA:
		case FEAT_DIRT:
		case FEAT_GRASS:
		case FEAT_TRAP_TRAPS:
		case FEAT_TRAP_ALARM:
		case FEAT_FLOWER:
		case FEAT_DEEP_GRASS:
		case FEAT_MUSEUM:
		case FEAT_TREES:
		case FEAT_TOWN:
		case FEAT_ENTRANCE:
		case FEAT_SWAMP:
		case FEAT_ENTRANCE_UPWARD:
		case FEAT_TUNDRA:
		case FEAT_DEEP_SEA:
		case FEAT_BETWEEN:
		case FEAT_TRAP_ARMAGEDDON:
		case FEAT_TRAP_PIRANHA:
			return FALSE;

		case FEAT_DARK_PIT:
		case FEAT_AIR:
			if (p_ptr->ffall) return FALSE;
			break;

		case FEAT_MOUNTAIN:
			if (!dun_level && p_ptr->ffall) return FALSE;
			break;

		case FEAT_RUBBLE:
		case FEAT_MAGMA:
		case FEAT_QUARTZ:
		case FEAT_MAGMA_H:
		case FEAT_QUARTZ_H:
		case FEAT_MAGMA_K:
		case FEAT_QUARTZ_K:
		case FEAT_WALL_EXTRA:
		case FEAT_WALL_INNER:
		case FEAT_WALL_OUTER:
		case FEAT_WALL_SOLID:
		case FEAT_PERM_EXTRA:
		case FEAT_PERM_INNER:
		case FEAT_PERM_OUTER:
		case FEAT_PERM_SOLID:
		case FEAT_MINOR_GLYPH:
			break;

		default:
			if ((feat >= FEAT_DOOR_HEAD) &&
			    (feat <= FEAT_DOOR_TAIL)) return FALSE;

			if ((feat >= FEAT_SHOP_HEAD) &&
			    (feat <= FEAT_SHOP_TAIL)) return FALSE;
			break;
		}

		return TRUE;
	}

	/* Default */
	return FALSE;
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown */
	if (!in_bounds2(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave[y][x].info & (CAVE_MARK)) return (FALSE);

	/* Non-floor grids are unknown */
	if (!cave_floor_bold(y, x)) return (TRUE);

	/* Viewable door/wall grids are known */
	if (player_can_see_bold(y, x)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm:                       -CJS-
 *
 * In the diagrams below, the player has just arrived in the
 * grid marked as '@', and he has just come from a grid marked
 * as 'o', and he is about to enter the grid marked as 'x'.
 *
 * Of course, if the "requested" move was impossible, then you
 * will of course be blocked, and will stop.
 *
 * Overview: You keep moving until something interesting happens.
 * If you are in an enclosed space, you follow corners. This is
 * the usual corridor scheme. If you are in an open space, you go
 * straight, but stop before entering enclosed space. This is
 * analogous to reaching doorways. If you have enclosed space on
 * one side only (that is, running along side a wall) stop if
 * your wall opens out, or your open space closes in. Either case
 * corresponds to a doorway.
 *
 * What happens depends on what you can really SEE. (i.e. if you
 * have no light, then running along a dark corridor is JUST like
 * running in a dark room.) The algorithm works equally well in
 * corridors, rooms, mine tailings, earthquake rubble, etc, etc.
 *
 * These conditions are kept in static memory:
 * find_openarea         You are in the open on at least one
 * side.
 * find_breakleft        You have a wall on the left, and will
 * stop if it opens
 * find_breakright       You have a wall on the right, and will
 * stop if it opens
 *
 * To initialize these conditions, we examine the grids adjacent
 * to the grid marked 'x', two on each side (marked 'L' and 'R').
 * If either one of the two grids on a given side is seen to be
 * closed, then that side is considered to be closed. If both
 * sides are closed, then it is an enclosed (corridor) run.
 *
 * LL           L
 * @x          LxR
 * RR          @R
 *
 * Looking at more than just the immediate squares is
 * significant. Consider the following case. A run along the
 * corridor will stop just before entering the center point,
 * because a choice is clearly established. Running in any of
 * three available directions will be defined as a corridor run.
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * #.#
 * ##.##
 * .@x..
 * ##.##
 * #.#
 *
 * Likewise, a run along a wall, and then into a doorway (two
 * runs) will work correctly. A single run rightwards from @ will
 * stop at 1. Another run right and down will enter the corridor
 * and make the corner, stopping at the 2.
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After any move, the function area_affect is called to
 * determine the new surroundings, and the direction of
 * subsequent moves. It examines the current player location
 * (at which the runner has just arrived) and the previous
 * direction (from which the runner is considered to have come).
 *
 * Moving one square in some direction places you adjacent to
 * three or five new squares (for straight and diagonal moves
 * respectively) to which you were not previously adjacent,
 * marked as '!' in the diagrams below.
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * You STOP if any of the new squares are interesting in any way:
 * for example, if they contain visible monsters or treasure.
 *
 * You STOP if any of the newly adjacent squares seem to be open,
 * and you are also looking for a break on that side. (that is,
 * find_openarea AND find_break).
 *
 * You STOP if any of the newly adjacent squares do NOT seem to be
 * open and you are in an open area, and that side was previously
 * entirely open.
 *
 * Corners: If you are not in the open (i.e. you are in a corridor)
 * and there is only one way to go in the new squares, then turn in
 * that direction. If there are more than two new ways to go, STOP.
 * If there are two ways to go, and those ways are separated by a
 * square which does not seem to be open, then STOP.
 *
 * Otherwise, we have a potential corner. There are two new open
 * squares, which are also adjacent. One of the new squares is
 * diagonally located, the other is straight on (as in the diagram).
 * We consider two more squares further out (marked below as ?).
 *
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid, and "check_dir" to the grid marked 's'.
 *
 * ##s
 * @x?
 * #.?
 *
 * If they are both seen to be closed, then it is seen that no benefit
 * is gained from moving straight. It is a known corner.  To cut the
 * corner, go diagonally, otherwise go straight, but pretend you
 * stepped diagonally into that next location for a full view next
 * time. Conversely, if one of the ? squares is not seen to be closed,
 * then there is a potential choice. We check to see whether it is a
 * potential corner or an intersection/room entrance.  If the square
 * two spaces straight ahead, and the space marked with 's' are both
 * unknown space, then it is a potential corner and enter if
 * find_examine is set, otherwise must stop because it is not a
 * corner.
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
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;



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
	int             row, col, deepleft, deepright;
	int             i, shortleft, shortright;


	/* Save the direction */
	find_current = dir;

	/* Assume running straight */
	find_prevdir = dir;

	/* Assume looking for open area */
	find_openarea = TRUE;

	/* Assume not looking for breaks */
	find_breakright = find_breakleft = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	p_ptr->run_py = py;
	p_ptr->run_px = px;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for walls */
	if (see_wall(cycle[i+1], py, px))
	{
		find_breakleft = TRUE;
		shortleft = TRUE;
	}
	else if (see_wall(cycle[i+1], row, col))
	{
		find_breakleft = TRUE;
		deepleft = TRUE;
	}

	/* Check for walls */
	if (see_wall(cycle[i-1], py, px))
	{
		find_breakright = TRUE;
		shortright = TRUE;
	}
	else if (see_wall(cycle[i-1], row, col))
	{
		find_breakright = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (find_breakleft && find_breakright)
	{
		/* Not looking for open area */
		find_openarea = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				find_prevdir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				find_prevdir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				find_prevdir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				find_prevdir = cycle[i + 2];
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
	int         prev_dir, new_dir, check_dir = 0;
	int         row, col;
	int         i, max, inv;
	int         option = 0, option2 = 0;
	cave_type   *c_ptr;

	/* Where we came from */
	prev_dir = find_prevdir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;

	/* break run when leaving trap detected region */
	if ((disturb_trap_detect || alert_trap_detect)
	    && p_ptr->dtrap && !(cave[py][px].info & CAVE_IN_DETECT))
	{
		/* No duplicate warning */
		p_ptr->dtrap = FALSE;

		/* You are just on the edge */
		if (!(cave[py][px].info & CAVE_UNSAFE))
		{
			if (alert_trap_detect)
			{
#ifdef JP
				msg_print("* 注意:この先はトラップの感知範囲外です！ *");
#else
				msg_print("*Leaving trap detect region!*");
#endif
			}

			if (disturb_trap_detect)
			{
				/* Break Run */
				return(TRUE);
			}
		}
	}

	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];

		/* Access grid */
		c_ptr = &cave[row][col];

		/* Visible monsters abort running */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (c_ptr->info & (CAVE_MARK))
		{
			/* Feature code (applying "mimic" field) */
			byte feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

			bool notice = TRUE;

			/* Examine the terrain */
			switch (feat)
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Invis traps */
				case FEAT_INVIS:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Known treasure (almost uninteresting) */
				case FEAT_MAGMA_K:
				case FEAT_QUARTZ_K:

				/* Walls */
				case FEAT_RUBBLE:
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				/* dirt, grass, trees, ... */
				case FEAT_SHAL_WATER:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_DEEP_GRASS:
				case FEAT_FLOWER:
				case FEAT_DARK_PIT:
				case FEAT_TREES:
				case FEAT_MOUNTAIN:
				case FEAT_SWAMP:
				case FEAT_TUNDRA:
				case FEAT_AIR:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* quest features */
				case FEAT_QUEST_ENTER:
				case FEAT_QUEST_EXIT:
				{
					/* Notice */
					notice = TRUE;

					/* Done */
					break;
				}

				case FEAT_DEEP_LAVA:
				case FEAT_SHAL_LAVA:
				{
					/* Ignore */
					if (p_ptr->invuln || p_ptr->immune_fire) notice = FALSE;

					/* Done */
					break;
				}

				case FEAT_DEEP_WATER:
				{
					/* Ignore */
					if (p_ptr->can_swim || p_ptr->ffall || p_ptr->total_weight <= (((u32b)adj_str_wgt[p_ptr->stat_ind[A_STR]]*(p_ptr->pclass == CLASS_TERRORKNIGHT ? 150 : 100))/2)) notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (find_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				case FEAT_LESS_LESS:
				case FEAT_MORE_MORE:
				case FEAT_ENTRANCE:
				case FEAT_ENTRANCE_UPWARD:
				case FEAT_BETWEEN:
				{
					/* Option -- ignore */
					if (find_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors considering mimic */
		if (inv || !see_wall(0, row, col))
		{
			/* Looking for open area */
			if (find_openarea)
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
			if (find_openarea)
			{
				if (i < 0)
				{
					/* Break to the right */
					find_breakright = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					find_breakleft = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (find_openarea)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!see_wall(cycle[chome[prev_dir] + i], py, px))
			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!see_wall(cycle[chome[prev_dir] + i], py, px))
			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (find_breakright)
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
			find_current = option;

			/* No other options */
			find_prevdir = option;
		}

		/* Two options, examining corners */
		else if (find_examine && !find_cut)
		{
			/* Primary option */
			find_current = option;

			/* Hack -- allow curving */
			find_prevdir = option2;
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
				if (find_examine &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					find_current = option;
					find_prevdir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (find_cut)
			{
				find_current = option2;
				find_prevdir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				find_current = option;
				find_prevdir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(find_current, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 */
void run_step(int dir)
{
	/* Start running */
	if (dir)
	{
		/* Hack -- do not start silly run */
		if (see_wall(dir, py, px))
		{
			/* Message */
#ifdef JP
			msg_print("その方向には走れません。");
#else
			msg_print("You cannot run in that direction.");
#endif


			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Initialize */
		run_init(dir);
	}

	/* Keep running */
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

	/* Decrease the run counter */
	if (--running <= 0) return;

	/* Take time */
	energy_use = 100;

	/* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM /* TNB */

	move_player(find_current, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

	move_player(find_current, always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

	if ((py == p_ptr->run_py) && (px == p_ptr->run_px))
	{
		p_ptr->run_py = 0;
		p_ptr->run_px = 0;
		disturb(0, 0);
	}
}
