

/* File: talents.c */

/*
 * Copyright (c) 1998 Julian Lighton, Michael Gorse, Chris Petit
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Some math functions for use in fenneling.
 */

#define sqr(x)	((x)*(x))

static int
isqrt(int x)
{
	int root = 0;

	do
	{
		root++;
		if (root > 250)
			break;
	}
	while (root * root <= x);
	root--;
	return (root);
}

/*
 * Estimate the "power" of an object, for use in fenneling.
 *
 * Should really be in object[12].c.
 */
s16b
eval_object(object_type * o_ptr)
{
	s16b total = 0, tmp = 0;
	s16b pval, basedam, bonusdam;
	int i;
	u32b flags;

	/* save some stuff to save time later */
	pval = o_ptr->pval;
	basedam = (o_ptr->dd * (o_ptr->ds + 1)) / 2;
	bonusdam = o_ptr->to_d;
	switch (o_ptr->tval)
	{
	case TV_SHOT:
	case TV_ARROW:
	case TV_BOLT:
		total += (basedam + bonusdam) * 3 / 2 + o_ptr->to_h;
		break;
	case TV_BOW:
		i = o_ptr->sval % 10;	/* damage mult. */
		if (o_ptr->flags1 & TR1_MIGHT)
			i += cap_pval(o_ptr->pval, MAX_TR1_MIGHT);
		basedam = (((o_ptr->sval / 10) + 4) * i) / 2;	/* hack */
		total += (basedam + bonusdam) * 3 / 2 + o_ptr->to_h + o_ptr->to_a / 2;
		break;
	case TV_DIGGING:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
		total += (basedam + bonusdam) * 3 / 2 + o_ptr->to_h + o_ptr->to_a / 2;
		break;
	case TV_BOOTS:
	case TV_GLOVES:
	case TV_HELM:
	case TV_CROWN:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_DRAG_ARMOR:
		total += (o_ptr->ac + o_ptr->to_a) / 2;
		total += o_ptr->to_h + bonusdam * 3 / 2;
		break;
	default:
		total += bonusdam * 3 / 2 + o_ptr->to_h + o_ptr->to_a / 2;
	}

	/* Evaluate flags1 */
	flags = o_ptr->flags1;
	if (flags & TR1_STR)
		total += cap_pval(pval, MAX_TR1_STAT) * 3;
	if (flags & TR1_INT)
	{
		if (p_ptr->realm == MAGE)
			total += cap_pval(pval, MAX_TR1_STAT) * 2;
		total += cap_pval(pval, MAX_TR1_STAT) * 2;
	}
	if (flags & TR1_WIS)
	{
		if (p_ptr->realm == PRIEST || p_ptr->realm == DRUID)
			total += cap_pval(pval, MAX_TR1_STAT) * 2;
		total += cap_pval(pval, MAX_TR1_STAT) * 2;
	}
	if (flags & TR1_DEX)
		total += cap_pval(pval, MAX_TR1_STAT) * 3;
	if (flags & TR1_CON)
		total += cap_pval(pval, MAX_TR1_STAT) * 3;
	if (flags & TR1_CHR)
	{
		if (p_ptr->realm == NECRO)
			total += cap_pval(pval, MAX_TR1_STAT) * 2;
		total += cap_pval(pval, MAX_TR1_STAT) / 2;
	}
	if (flags & TR1_LUC)
		total += cap_pval(pval, MAX_TR1_STAT) * 2;
	if (flags & TR1_STEALTH)
		total += cap_pval(pval, MAX_TR1_STEALTH) * 2;
	if (flags & TR1_SEARCH)
		total += cap_pval(pval, MAX_TR1_SEARCH) / 2;
	if (flags & TR1_INFRA)
		total += cap_pval(pval, MAX_TR1_INFRA) / 2;
	if (flags & TR1_TUNNEL)
		total += cap_pval(pval, MAX_TR1_TUNNEL) / 2;
	if (flags & TR1_SPEED)
	{
		if (pval > 0)
			total += cap_pval(pval, MAX_TR1_SPEED) *
				cap_pval(pval, MAX_TR1_SPEED);
		else
			total -= pval * pval;
	}
	if (flags & TR1_BLOWS)
		total += cap_pval(pval, MAX_TR1_BLOWS) * (basedam + bonusdam) * 3 / 2;
	if (flags & TR1_INVIS)
		total += cap_pval(pval, MAX_TR1_INVIS) * 8;
	if (flags & TR1_SHOTS)
		total += (basedam + bonusdam) *
			(2 + cap_pval(o_ptr->pval, MAX_TR1_SHOTS)) / 2;
	/* TR1_MIGHT handled elsewhere */
	if (flags & TR1_SLAY_ANIMAL)
		tmp += basedam / 2;
	if (flags & TR1_SLAY_EVIL)
		tmp += basedam;
	else
	{
		if (flags & TR1_SLAY_UNDEAD)
			tmp += basedam;
		if (flags & TR1_SLAY_DEMON)
			tmp += basedam / 3;
		if (flags & TR1_SLAY_ORC)
			tmp += basedam / 4;
		if (flags & TR1_SLAY_TROLL)
			tmp += basedam / 4;
		if (flags & TR1_SLAY_GIANT)
			tmp += basedam / 4;
	}
	if (flags & TR1_KILL_DRAGON)
		tmp += basedam * 2 / 3;
	else if (flags & TR1_SLAY_DRAGON)
		tmp += basedam / 3;
	if (flags & TR1_VORPAL)
		total += (basedam + bonusdam) / 6;
	if (flags & TR1_BRAND_POIS)
		tmp += basedam / 3;
	if (flags & TR1_BRAND_ACID)
		tmp += basedam * 4 / 5;
	if (flags & TR1_BRAND_ELEC)
		tmp += basedam;
	if (flags & TR1_BRAND_FIRE)
		tmp += basedam / 2;
	if (flags & TR1_BRAND_COLD)
		tmp += basedam / 2;
	if (tmp > basedam * 5)
		total += basedam * 5;
	else
		total += tmp;

	/* Eval flags2 */
	flags = o_ptr->flags2;
	if (flags & TR2_SUST_STR)
		total += 2;
	if (flags & TR2_SUST_INT)
		total += 2;
	if (flags & TR2_SUST_WIS)
		total += 2;
	if (flags & TR2_SUST_DEX)
		total += 2;
	if (flags & TR2_SUST_CON)
		total += 2;
	if (flags & TR2_SUST_CHR)
		total += (p_ptr->realm == NECRO ? 2 : 1);
	if (flags & TR2_SUST_LUC)
		total += 2;
	if (flags & TR2_IM_ACID)
		total += 100;
	else if (flags & TR2_RES_ACID)
		total += 10;
	if (flags & TR2_IM_ELEC)
		total += 100;
	else if (flags & TR2_RES_ELEC)
		total += 10;
	if (flags & TR2_IM_FIRE)
		total += 100;
	else if (flags & TR2_RES_FIRE)
		total += 15;
	if (flags & TR2_IM_COLD)
		total += 100;
	else if (flags & TR2_RES_COLD)
		total += 15;
	if (flags & TR2_RES_POIS)
		total += 40;
	if (flags & TR2_RES_FEAR)
		total += 9;
	if (flags & TR2_RES_LITE)
		total += 8;
	if (flags & TR2_RES_DARK)
		total += 10;
	if (flags & TR2_RES_BLIND)
		total += 9;
	if (flags & TR2_RES_SOUND)
		total += 8;
	if (flags & TR2_RES_SHARD)
		total += 6;
	if (flags & TR2_RES_NEXUS)
		total += 6;
	if (flags & TR2_RES_NETHR)
		total += 20;
	if (flags & TR2_RES_CHAOS)
		total += 40;
	else if (flags & TR2_RES_CONFU)
		total += 9;
	if (flags & TR2_RES_DISEN)
		total += 20;

	/* Eval flags3 */
	flags = o_ptr->flags3;
	if (flags & TR3_SLOW_DIGEST)
		total += 3;
	if (flags & TR3_FEATHER)
		total += 2;
	if (flags & TR3_LITE)
		total += 1;
	if (flags & TR3_REGEN)
		total += 8;
	if (flags & TR3_TELEPATHY)
		total += 40;
	if (flags & TR3_SEE_INVIS)
		total += 5;
	if (flags & TR3_FREE_ACT)
		total += 15;
	if (flags & TR3_HOLD_LIFE)
		total += 30;
	if (flags & TR3_SOULSTEAL)
		total -= 15;
	if (flags & TR3_NOMAGIC)
		total -= 50;
	if (flags & TR3_IMPACT)
		total += 10;
	if (flags & TR3_TELEPORT)
		total -= 20;
	if (flags & TR3_AGGRAVATE)
		total -= 20;
	if (flags & TR3_DRAIN_EXP)
		total -= 15;
	if (flags & TR3_BLESSED)
		if (p_ptr->realm == PRIEST &&
			(o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED))
			total += 5;
	if (flags & TR3_PERMA_CURSE)
		total -= 50;
	else if (flags & TR3_HEAVY_CURSE)
		total -= 10;
	else if (flags & TR3_LIGHT_CURSE)
		total -= 5;

	/* Boost the rating of rings because you can wear two. */
	if (o_ptr->tval == TV_RING)
		return (total * 3) / 2;
	return total;
}

/* add a curse to an object. returns the object's new power rating. */
static s16b
hose_obj(object_type * o_ptr)
{
	s16b pow;
	int i;

	pow = eval_object(o_ptr);
	i = rand_int((pow > 200 ? 200 : pow));
	if (i < 100)
	{
		if (rand_int(100) < 85)
		{
			o_ptr->flags3 |= TR3_LIGHT_CURSE;
			o_ptr->ident |= IDENT_CURSED;
		}
		else
			o_ptr->flags3 |= TR3_TELEPORT;
	}
	else if (i < 125)
	{
		if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM)
		{
			o_ptr->flags3 |= TR3_SOULSTEAL;
		}
	}
	else if (i < 150)
	{
		o_ptr->flags3 |= TR3_LIGHT_CURSE;
		o_ptr->flags3 |= TR3_HEAVY_CURSE;
		o_ptr->ident |= IDENT_CURSED;
	}
	else if (i < 165)
	{
		o_ptr->flags3 |= TR3_AGGRAVATE;
	}
	else if (i < 180)
	{
		o_ptr->flags3 |= TR3_DRAIN_EXP;
	}
	else
	{
		if (rand_int(2))
		{
			o_ptr->flags3 |= TR3_LIGHT_CURSE;
			o_ptr->flags3 |= TR3_HEAVY_CURSE;
			o_ptr->flags3 |= TR3_PERMA_CURSE;
			o_ptr->ident |= IDENT_CURSED;
		}
		else
			o_ptr->flags3 |= TR3_NOMAGIC;
	}
	return eval_object(o_ptr);
}

/* a function that decides whether two objects like being fenneled. */

static bool
fennel_ok(object_type * src, object_type * dest)
{
	s16b srcpow, destpow;
	char buf[80];

	srcpow = eval_object(src);
	destpow = eval_object(dest);
	if (srcpow > 200)
	{
		object_desc(buf, src, FALSE, 2);
		msg_format("Your %s will not be fenneled!", buf);
		return FALSE;
	}
	if (destpow > 200)
	{
		object_desc(buf, dest, FALSE, 2);
		msg_format("Your %s will not be fenneled!", buf);
		return FALSE;
	}
	return TRUE;
}

/* A function to make certain that fenneled objects don't get too
 * absurd. */
static void
cap_vals(object_type * o_ptr)
{
	if (o_ptr->to_h > 30)
		o_ptr->to_h = 30;
	if (o_ptr->to_d > 30)
		o_ptr->to_d = 30;
	if (o_ptr->to_a > 40)
		o_ptr->to_a = 40;
	if (o_ptr->dd > 9)
		o_ptr->dd = 9;
	/* pval doesn't need to be capped. It varies dependant on what it raises, 
	 * and is handled elsewhere. */
}

/* Helper function for fenneling. Adds some of the flags belonging to
 * src to dest, then sets the special flags correctly. */
static void
merge_flags(object_type * dest, object_type * src)
{
	u32b nflags[3], tmp;
	s16b pow, i, count = 0;
	int add;

	nflags[0] = src->flags1 & ~(src->flags1 & dest->flags1);
	nflags[1] = src->flags2 & ~(src->flags2 & dest->flags2);
	nflags[2] = src->flags3 & ~(src->flags3 & dest->flags3);
	nflags[2] &= ~(TR3_ACTIVATE);	/* activations cannot wander */

	pow = eval_object(dest);
	for (i = 0; i < 3; i++)
	{
		for (tmp = 1; tmp > 0; tmp <<= 1)
		{
			if (nflags[i] & tmp)
			{
				nflags[i] &= ~(tmp);
				if (pow < 10)
					add = 100;
				else if (pow < 90)
					add = 75;
				else if (pow < 140)
					add = 50;
				else if (pow < 190)
					add = 25;
				else
					add = 10;
				if (randint(100 - luck()) <= add)
					switch (i)
					{
					case 0:
						dest->flags1 |= tmp;
						break;
					case 1:
						dest->flags2 |= tmp;
						break;
					case 2:
						dest->flags3 |= tmp;
						break;
					}
			}
		}
	}

	/* Set the special flags correctly. */
	dest->flags3 &= ~(TR3_EASY_KNOW);
	if (src->flags3 & TR3_SHOW_MODS)
		dest->flags3 |= TR3_SHOW_MODS;
	if (!(dest->flags3 & TR3_HIDE_TYPE && src->flags3 & TR3_HIDE_TYPE))
	{
		count = 0;
		tmp = dest->flags1;
		if (tmp & TR1_STR)
			count++;
		if (tmp & TR1_INT)
			count++;
		if (tmp & TR1_WIS)
			count++;
		if (tmp & TR1_DEX)
			count++;
		if (tmp & TR1_CON)
			count++;
		if (tmp & TR1_CHR)
			count++;
		if (tmp & TR1_LUC)
			count++;
		if (tmp & TR1_STEALTH)
			count++;
		if (tmp & TR1_INVIS)
			count++;
		if (tmp & TR1_SEARCH)
			count++;
		if (tmp & TR1_INFRA)
			count++;
		if (tmp & TR1_TUNNEL)
			count++;
		if (tmp & TR1_SPEED)
			count++;
		if (tmp & TR1_BLOWS)
			count++;
		if (count > 1)
			dest->flags3 |= TR3_HIDE_TYPE;
		else
			dest->flags3 &= ~(TR3_HIDE_TYPE);
	}
}

/*
 * Hook to allow iding of wands and staves with known type
 */
static bool
item_tester_hook_charges(object_type * o_ptr)
{
	if ((o_ptr->tval == TV_WAND || o_ptr->tval == TV_STAFF) &&
		k_info[o_ptr->k_idx].aware)
		return TRUE;
	return FALSE;
}

/*
 * Hook to specify what can be tested with alchemy
 */
static bool
item_tester_hook_alchemy(object_type * o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_SCROLL:
	case TV_POTION:
		return TRUE;
	default:
		return FALSE;
	}
}

/*
 * Hook to specify what is fennelable.
 */
static bool
item_tester_hook_fennel(object_type * o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_SWORD:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_WAND:
	case TV_STAFF:
	case TV_RING:
	case TV_AMULET:
	case TV_HELM:
	case TV_CROWN:
	case TV_BOOTS:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_GLOVES:
	case TV_HARD_ARMOR:
	case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Fenneling talent.
 *
 * Fenneling merges two similar objects into one more powerful one.
 *
 * this part originally Copyright Frits Daalmans, 1995
 * Heavily modified Julian Lighton, 1997-98
 */

static bool
talent_fennel(int level)
{
	int x, src_item, dest_item, difficulty;
	char buf[80], tval;

	object_type *src_ptr, *dest_ptr;
	object_type tmp_obj;

	bool trashsrc = FALSE, trashdest = FALSE;
	bool kaboom = FALSE, extradest = FALSE;

	s16b diff, oldpow = 0, newpow = 0;

	cptr q, s;

	item_tester_hook = item_tester_hook_fennel;

	/* Get an item */
	q = "Fennel which item? ";
	s = "You have nothing to fennel.";
	if (!get_item(&src_item, q, s, (USE_INVEN)))
		return FALSE;

	src_ptr = &inventory[src_item];
	tval = src_ptr->tval;
	switch (tval)
	{
	case TV_WAND:
		if (get_skill(S_INFUSION) < 10)
		{
			msg_print("You do not know enough about wands yet!");
			return FALSE;
		}
		difficulty = 25;
		break;
	case TV_STAFF:
		if (level <= 10)
		{
			msg_print("You are not skilled enough!");
			return FALSE;
		}
		if (get_skill(S_INFUSION) < 20)
		{
			msg_print("You do not know enough about staves yet!");
			return FALSE;
		}
		difficulty = 35;
		break;
	case TV_RING:
		if (level <= 30)
		{
			msg_print("You are not skilled enough!");
			return FALSE;
		}
		if (get_skill(S_INFUSION) < 30)
		{
			msg_print("You need to know more about magical infusion of rings!");
			return FALSE;
		}
		difficulty = 45;
		break;
	case TV_AMULET:
		if (level <= 35)
		{
			msg_print("You are not skilled enough!");
			return FALSE;
		}
		if (get_skill(S_INFUSION) < 35)
		{
			msg_print("You need to know more about magical infusion of amulets!");
			return FALSE;
		}
		difficulty = 45;
		break;
	case TV_HELM:
	case TV_CROWN:
	case TV_BOOTS:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_GLOVES:
	case TV_HARD_ARMOR:
	case TV_SOFT_ARMOR:
		if (level <= 40)
		{
			msg_print("You are not skilled enough!");
			return FALSE;
		}
		if (get_skill(S_INFUSION) < 40)
		{
			msg_print("You are not skilled enough at infusion.");
			return FALSE;
		}
		if (get_skill(S_ARMOR) <= 15)
		{
			msg_print("You need to be better at forging armor first.");
			return FALSE;
		}
		difficulty = 45;
		break;
	case TV_SWORD:
	case TV_HAFTED:
	case TV_POLEARM:
		if (level <= 50)
		{
			msg_print("You are not skilled enough!");
			return FALSE;
		}
		if (get_skill(S_INFUSION) <= 40)
		{
			msg_print("You are not skilled enough at infusion.");
			return FALSE;
		}
		if (get_skill(S_WEAPON) <= 15)
		{
			msg_print("You need to be better at smithing first.");
			return FALSE;
		}
		difficulty = 45;
		break;
	default:
		msg_print("You don't know how to fennel those.");
		return FALSE;
	}

	item_tester_tval = tval;

	/* Get an item */
	q = "Into which other item? ";
	s = "You have nothing to fennel.";
	if (!get_item(&dest_item, q, s, (USE_INVEN)))
		return FALSE;

	if (src_item == dest_item)
	{
		msg_print("You cannot fennel an object into itself.");
		return FALSE;
	}
	if (inventory[dest_item].number > 1)
	{
		extradest = TRUE;

		object_copy(&tmp_obj, &inventory[dest_item]);

		tmp_obj.number = 1;

		dest_ptr = &tmp_obj;
	}
	else
		dest_ptr = &inventory[dest_item];

	if (artifact_p(src_ptr) || artifact_p(dest_ptr))
	{
		msg_print("You cannot fennel artifacts.");
		return FALSE;
	}
	if (broken_p(src_ptr) || broken_p(dest_ptr))
	{
		msg_print("There must be more material left to work with.");
		return FALSE;
	}

	/* Paranoia. This should never be needed. If it is, the weight
	 * gets screwed up. */
	dest_ptr->number = 1;


	switch (tval)
	{
	case TV_WAND:
	case TV_STAFF:
		{
			dest_ptr->pval += (src_ptr->pval * level) / 50;

			/* Hack -- we no longer "know" the item */
			dest_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			dest_ptr->ident &= ~(IDENT_EMPTY);
			trashsrc = TRUE;

			/* Prevent absurd numbers of charges. */
			x = (level + 100 - (k_info[dest_ptr->k_idx].level) -
				 (10 * dest_ptr->pval)) / 15;
			/* Paranoia -- prevent crashes */
			if (x < 1)
				x = 1;
			if (randint(difficulty) > level || rand_int(x) == 0)
			{
				msg_print("There is a bright flash of light.");
				trashdest = TRUE;
			}
			break;
		}
	case TV_RING:
	case TV_AMULET:
	case TV_HELM:
	case TV_CROWN:
	case TV_BOOTS:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_GLOVES:
	case TV_HARD_ARMOR:
	case TV_SOFT_ARMOR:
	case TV_SWORD:
	case TV_HAFTED:
	case TV_POLEARM:
		{
			if (!fennel_ok(src_ptr, dest_ptr))
				break;
			oldpow = eval_object(dest_ptr);

			if (dest_ptr->pval < 1 || src_ptr->pval < 1)
				dest_ptr->pval += src_ptr->pval;
			else
				dest_ptr->pval = isqrt(sqr(dest_ptr->pval) +
									   sqr(src_ptr->pval));

			if (dest_ptr->to_a < 1 || src_ptr->to_a < 1)
				dest_ptr->to_a += src_ptr->to_a;
			else
				dest_ptr->to_a = isqrt(sqr(src_ptr->to_a) +
									   sqr(dest_ptr->to_a));

			/* A little hackery to avoid body armor - to hit plummeting */
			if (dest_ptr->to_h < 1 && src_ptr->to_h < 1 &&
				(tval == TV_SOFT_ARMOR || tval == TV_HARD_ARMOR))
				dest_ptr->to_h = -1 * isqrt(sqr(src_ptr->to_h)
											+ sqr(dest_ptr->to_h));
			else if (dest_ptr->to_h < 1 || src_ptr->to_h < 1)
				dest_ptr->to_h += src_ptr->to_h;
			else
				dest_ptr->to_h = isqrt(sqr(src_ptr->to_h) +
									   sqr(dest_ptr->to_h));

			if (dest_ptr->to_d < 1 || src_ptr->to_d < 1)
				dest_ptr->to_d += src_ptr->to_d;
			else
				dest_ptr->to_d = isqrt(sqr(src_ptr->to_d) +
									   sqr(dest_ptr->to_d));

			if (dest_ptr->dd < 1 || src_ptr->dd < 1)
				dest_ptr->dd += src_ptr->dd;
			else
				dest_ptr->dd = isqrt(sqr(src_ptr->dd) + sqr(dest_ptr->dd));

			dest_ptr->b_cost = isqrt(sqr(dest_ptr->b_cost) +
									 sqr(src_ptr->b_cost));

			merge_flags(dest_ptr, src_ptr);
			cap_vals(dest_ptr);

			trashsrc = TRUE;

			if (randint(difficulty) > level)
			{
				msg_print("There is a bright flash of light.");
				trashdest = TRUE;
				break;
			}
			newpow = eval_object(dest_ptr);
			if (newpow > 0 && rand_int(newpow) > (level * 3) / 2)
			{
				if (newpow < 150)
				{
					if (rand_int(4))
						newpow = hose_obj(dest_ptr);
					else
						kaboom = TRUE;
				}
				else if (newpow < 200)
				{
					if (rand_int(2))
						newpow = hose_obj(dest_ptr);
					else
						kaboom = TRUE;
				}
				else
				{
					if (rand_int(5))
						newpow = hose_obj(dest_ptr);
					else
						kaboom = TRUE;
				}
			}

			/* if you shoved too much power into the object, it could
			 * explode. */
			if (newpow > 10 && oldpow > 10)
			{
				diff = newpow - oldpow;
				if (diff > oldpow * 3)
					kaboom = TRUE;
				else if (diff > oldpow && rand_int(10) == 0)
					kaboom = TRUE;
				else if (diff > 50 && rand_int(20) == 0)
					kaboom = TRUE;
			}

			if (kaboom)
			{
				trashdest = TRUE;
				object_desc(buf, dest_ptr, FALSE, 2);
				msg_format("Your %s explodes violently!", buf);
				take_hit(randint(2) * randint(newpow), "an exploding object.");
			}
			break;
		}
	default:
		{
			msg_print("Bug in fenneling code.");
			break;
		}
	}

	p_ptr->tt = newpow;
	if (p_ptr->tt < 100)
		p_ptr->tt = 100;

	/* Search backwards through the pack, destroying those items
	 * that need destroying */
	for (x = INVEN_PACK; x >= 0; x--)
	{
		if ((x == dest_item && (trashdest && !extradest)) ||
			(x == src_item && trashsrc))
			inven_item_decrease(x);
	}

	/* Handle the case of there having been more than one copy
	 * of the destination object. */
	if (extradest)
	{
		inven_item_decrease(dest_item);
		dest_item = inven_carry(dest_ptr);
		if (trashdest)
			inven_item_decrease(dest_item);
	}

	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->window |= (PW_INVEN);
	return TRUE;
}

static bool
talent_create_obj(int level, char tval)
{
	int j, oblevel, item;
	s16b objnum;
	object_type *o_ptr, *j_ptr, forge;
	cptr q, s;

	item_tester_tval = TV_COMPONENT;

	/* Get an item */
	q = "Use which component? ";
	s = "You have no components.";
	if (!get_item(&item, q, s, (USE_INVEN)))
		return FALSE;

	o_ptr = &inventory[item];
	j = (o_ptr->pval * 5) + 1;
	oblevel = (j + level) / 2;
	inven_item_decrease(item);

	item_tester_tval = tval;
	get_obj_num_hook = kind_fits_tval;
	get_obj_num_prep();
	j_ptr = &forge;
	objnum = get_obj_num(oblevel);
	if (objnum == 0)			/* oops */
	{
		msg_print("Something went wrong!");
		return TRUE;
	}
	object_prep(j_ptr, oblevel);
	item = inven_carry(j_ptr);
	inven_item_describe(item);

	item_tester_tval = 0;
	get_obj_num_hook = NULL;
	get_obj_num_prep();
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->window |= (PW_INVEN);
	return TRUE;
}

/*
 * Returns a string describing how nifty an item is, either in the
 * format "a/an adjective" or just "adjective" dependin on the value
 * of plural. Totally frivolous, but who cares?
 */
static cptr
superb_word(bool plural)
{
	switch (randint(7))
	{
	case 1:
		return (plural ? "excellent" : "an excellent");
	case 2:
		return (plural ? "special" : "a special");
	case 3:
		return (plural ? "superb" : "a superb");
	case 4:
		return (plural ? "good" : "a good");
	case 5:
		return (plural ? "superior" : "a superior");
	case 6:
		return (plural ? "amazing" : "an amazing");
	case 7:
		return (plural ? "very good" : "a very good");
	default:
		return (plural ? "buggy" : "a buggy");
	}
}

static bool
forge_add_special(int level, int type, int num, int pow, object_type * o_ptr)
{
	bool special = FALSE;
	int i, which;
	u32b all_resists = (TR2_RES_COLD | TR2_RES_ACID |
						TR2_RES_FIRE | TR2_RES_ELEC);

	for (i = 0; i < num; i++)
	{
		if (randint(level / 20) >= randint(6))
		{
			which = 0;
			while (randint(90 + (which * 10)) < 60 + (pow * 2))
				which++;
			if (which > 10)
				which = 10;
			special = TRUE;
			switch (type)
			{
			case TAL_F_SWORD:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_h += randint(3);
						o_ptr->to_d += randint(3);
						break;
					case 1:
						add_one_flag(o_ptr, (TR1_VORPAL | TR1_SLAY_ORC |
										   TR1_SLAY_TROLL | TR1_SLAY_GIANT |
										  TR1_SLAY_DEMON | TR1_SLAY_UNDEAD |
										  TR1_SLAY_ANIMAL | TR1_BRAND_FIRE |
											 TR1_BRAND_COLD), (TR2_SUST_STR |
											   TR2_SUST_DEX | TR2_SUST_CON |
												   TR2_SUST_CHR), (NOFLAG));
						break;
					case 2:
					case 3:
						add_one_flag(o_ptr, (TR1_BRAND_ELEC | TR1_BRAND_POIS |
											 TR1_BRAND_ACID | TR1_STR |
											 TR1_DEX | TR1_CON | TR1_CHR |
											 TR1_STEALTH | TR1_INFRA),
									 (TR2_RES_ACID | TR2_RES_COLD |
									  TR2_RES_FIRE | TR2_RES_ELEC),
								  (TR3_FREE_ACT | TR3_LITE | TR3_SEE_INVIS |
								   TR3_SLOW_DIGEST | TR3_BLESSED));
						break;
					case 4:
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_BLOWS |
										   TR1_SLAY_EVIL | TR1_KILL_DRAGON),
									 (TR2_RES_POIS | TR2_RES_CONFU),
									 (TR3_REGEN));
						break;
					case 7:
					case 8:
						o_ptr->ds += rand_int(3);
						break;
					case 9:
					case 10:
						add_one_flag(o_ptr, (TR1_SPEED), (TR2_RES_LITE |
														  TR2_RES_SOUND |
														  TR2_RES_SHARD |
											 TR2_RES_NEXUS | TR2_RES_CHAOS),
									 (TR3_TELEPATHY));
						break;
					}
					break;
				}
			case TAL_F_MACE:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_h += randint(3);
						o_ptr->to_d += randint(3);
						break;
					case 1:
						add_one_flag(o_ptr, (TR1_SLAY_ORC |
										   TR1_SLAY_TROLL | TR1_SLAY_GIANT |
										  TR1_SLAY_DEMON | TR1_SLAY_UNDEAD |
										   TR1_BRAND_FIRE | TR1_BRAND_COLD),
							   (TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS |
								TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHR |
								TR2_SUST_LUC), (NOFLAG));
						break;
					case 2:
					case 3:
						add_one_flag(o_ptr, (TR1_BRAND_ELEC |
											 TR1_BRAND_ACID | TR1_STR |
											 TR1_WIS | TR1_CON | TR1_LUC |
											 TR1_SEARCH | TR1_INFRA),
									 (TR2_RES_ACID | TR2_RES_COLD |
									  TR2_RES_FIRE | TR2_RES_ELEC),
									 (TR3_FREE_ACT | TR3_SEE_INVIS |
									  TR3_SLOW_DIGEST));
						break;
					case 4:
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_BLOWS |
											 TR1_SLAY_EVIL),
									 (TR2_RES_CONFU | TR2_RES_BLIND),
									 (TR3_REGEN));
						break;
					case 7:
					case 8:
						o_ptr->dd += rand_int(3);
						break;
					case 9:
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_DARK |
											 TR2_RES_SHARD | TR2_RES_NETHR |
											 TR2_RES_NEXUS | TR2_RES_CHAOS),
									 (TR3_HOLD_LIFE));
						break;
					}
					break;
				}
			case TAL_F_POLEARM:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_h += randint(3);
						o_ptr->to_d += randint(3);
						break;
					case 1:
						add_one_flag(o_ptr, (TR1_SLAY_ORC |
										   TR1_SLAY_TROLL | TR1_SLAY_GIANT |
										  TR1_SLAY_DEMON | TR1_SLAY_UNDEAD |
										  TR1_SLAY_ANIMAL | TR1_BRAND_FIRE |
											 TR1_BRAND_COLD), (TR2_SUST_STR |
											   TR2_SUST_INT | TR2_SUST_WIS |
								TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHR |
												   TR2_SUST_LUC), (NOFLAG));
						break;
					case 2:
					case 3:
						add_one_flag(o_ptr, (TR1_BRAND_POIS |
											 TR1_BRAND_ACID | TR1_INT |
											 TR1_WIS | TR1_DEX | TR1_LUC |
											 TR1_STEALTH | TR1_SEARCH),
									 (TR2_RES_ACID | TR2_RES_COLD |
									  TR2_RES_FIRE | TR2_RES_ELEC),
								   (TR3_FEATHER | TR3_LITE | TR3_SEE_INVIS |
									TR3_FREE_ACT | TR3_BLESSED));
						break;
					case 4:
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_BLOWS |
										   TR1_SLAY_EVIL | TR1_KILL_DRAGON),
									 (TR2_RES_CONFU | TR2_RES_BLIND),
									 (NOFLAG));
						break;
					case 7:
					case 8:
						o_ptr->ds += rand_int(2);
						o_ptr->dd += rand_int(2);
						break;
					case 9:
					case 10:
						add_one_flag(o_ptr, (TR1_SPEED),
							  (TR2_RES_LITE | TR2_RES_DARK | TR2_RES_SOUND |
							   TR2_RES_NETHR |
							   TR2_RES_CHAOS | TR2_RES_DISEN),
									 (TR3_TELEPATHY));
						break;
					}
					break;
				}
			case TAL_F_ARMOR:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_a += randint(5);
						break;
					case 1:
					case 2:
						add_one_flag(o_ptr, (NOFLAG),
							   (TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS |
								TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHR |
								TR2_SUST_LUC | TR2_RES_ACID |
								TR2_RES_COLD | TR2_RES_FIRE | TR2_RES_ELEC),
								 (TR3_FREE_ACT | TR3_LITE | TR3_SEE_INVIS));
						break;
					case 3:
					case 4:
						if (all_resists == (o_ptr->flags2 & all_resists))
							i--;	/* try again */
						else
							o_ptr->flags2 |= (TR2_RES_COLD | TR2_RES_ACID |
											  TR2_RES_FIRE | TR2_RES_ELEC);
						break;
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_STR | TR1_INT |
									 TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR |
								  TR1_LUC), (TR2_RES_CONFU | TR2_RES_BLIND),
									 (TR3_REGEN));
						break;
					case 7:
					case 8:
					case 9:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_LITE |
											  TR2_RES_DARK | TR2_RES_SOUND |
											 TR2_RES_SHARD | TR2_RES_NETHR |
											 TR2_RES_NEXUS | TR2_RES_CHAOS |
											   TR2_RES_DISEN | TR2_IM_ACID |
											TR2_RES_POIS), (TR3_HOLD_LIFE));
						break;
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_IM_FIRE | TR2_IM_COLD |
													TR2_IM_ELEC), (NOFLAG));
						break;
					}
					break;
				}
			case TAL_F_HELM:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_a += randint(3);
						break;
					case 1:
					case 2:
					case 3:
						add_one_flag(o_ptr, (TR1_SEARCH | TR1_INFRA),
									 (TR2_SUST_INT | TR2_SUST_WIS |
									  TR2_SUST_CHR | TR2_SUST_LUC |
									  TR2_RES_ACID | TR2_RES_COLD |
									  TR2_RES_FIRE | TR2_RES_ELEC),
								 (TR3_FREE_ACT | TR3_LITE | TR3_SEE_INVIS));
						break;
					case 5:
					case 6:
					case 7:
						add_one_flag(o_ptr, (TR1_INT |
											 TR1_WIS | TR1_CHR |
											 TR1_LUC | TR1_INVIS),
									 (TR2_RES_CONFU | TR2_RES_BLIND),
									 (NOFLAG));
						break;
					case 8:
					case 9:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_LITE | TR2_RES_DARK |
											 TR2_RES_SOUND | TR2_RES_NEXUS),
									 (TR3_TELEPATHY));
						break;
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_CHAOS |
									TR2_RES_DISEN | TR2_IM_ACID), (NOFLAG));
						break;
					}
					break;
				}
			case TAL_F_BOOTS:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_a += randint(4);
						break;
					case 1:
					case 2:
					case 3:
						add_one_flag(o_ptr, (TR1_STEALTH),
									 (TR2_SUST_DEX | TR2_SUST_CON |
									  TR2_RES_ACID |
								TR2_RES_COLD | TR2_RES_FIRE | TR2_RES_ELEC),
									 (TR3_FREE_ACT | TR3_FEATHER));
						break;
					case 4:
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_DEX | TR1_CON | TR1_INVIS),
									 (TR2_RES_POIS | TR2_RES_CONFU),
									 (TR3_REGEN));
						break;
					case 7:
					case 8:
					case 9:
						add_one_flag(o_ptr, (TR1_SPEED), (TR2_RES_NETHR |
												  TR2_RES_NEXUS), (NOFLAG));
						break;
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_CHAOS |
									TR2_RES_DISEN | TR2_IM_COLD), (NOFLAG));
						break;
					}
					break;
				}
			case TAL_F_SHIELD:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_a += randint(4);
						break;
					case 1:
					case 2:
						add_one_flag(o_ptr, (NOFLAG),
									 (TR2_SUST_STR | TR2_SUST_DEX |
									  TR2_SUST_CON | TR2_RES_ACID |
								TR2_RES_COLD | TR2_RES_FIRE | TR2_RES_ELEC),
									 (TR3_FREE_ACT));
						break;
					case 3:
					case 4:
						if (all_resists == (o_ptr->flags2 & all_resists))
							i--;	/* try again */
						else
							o_ptr->flags2 |= (TR2_RES_COLD | TR2_RES_ACID |
											  TR2_RES_FIRE | TR2_RES_ELEC);
						break;
					case 5:
					case 6:
					case 7:
						add_one_flag(o_ptr, (TR1_STR | TR1_DEX | TR1_CON),
									 (TR2_RES_POIS | TR2_RES_CONFU),
									 (TR3_REGEN));
						break;
					case 8:
					case 9:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_LITE |
											  TR2_RES_DARK | TR2_RES_SOUND |
											 TR2_RES_SHARD | TR2_RES_NETHR |
											 TR2_RES_NEXUS | TR2_RES_CHAOS |
										   TR2_RES_DISEN), (TR3_HOLD_LIFE));
						break;
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_IM_ACID |
													TR2_IM_FIRE), (NOFLAG));
						break;
					}
					break;
				}
			case TAL_F_GAUNTLETS:
				{
					switch (which)
					{
					case 0:
						o_ptr->to_a += randint(3);
						break;
					case 1:
					case 2:
						add_one_flag(o_ptr, (TR1_SEARCH),
									 (TR2_SUST_STR | TR2_SUST_DEX |
									  TR2_SUST_LUC | TR2_RES_ACID |
								TR2_RES_COLD | TR2_RES_FIRE | TR2_RES_ELEC),
									 (TR3_FREE_ACT | TR3_LITE));
						break;
					case 3:
					case 4:
						o_ptr->to_h += randint(2);
						o_ptr->to_d += randint(2);
						break;
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_STR | TR1_DEX |
											 TR1_LUC),
									 (TR2_RES_POIS | TR2_RES_CONFU),
									 (TR3_REGEN));
						break;
					case 7:
					case 8:
					case 9:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_LITE |
											  TR2_RES_DARK | TR2_RES_DISEN),
									 (TR3_HOLD_LIFE));
						break;
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_IM_ACID |
													TR2_IM_ELEC), (NOFLAG));
						break;
					}
					break;
				}
			case TAL_F_BOW:
				{
					switch (which)
					{
					case 0:
					case 1:
						o_ptr->to_h += randint(2);
						o_ptr->to_d += randint(2);
						break;
					case 2:
					case 3:
					case 4:
					case 5:
						add_one_flag(o_ptr, (TR1_DEX | TR1_CON |
										 TR1_STR | TR1_LUC), (TR2_RES_COLD |
											   TR2_RES_FIRE | TR2_RES_ACID |
											 TR2_RES_ELEC), (TR3_FREE_ACT));
						break;
					case 6:
					case 7:
						if (o_ptr->flags1 & TR1_SHOTS)
							i--;	/* try again */
						else
							o_ptr->flags1 |= TR1_SHOTS;
						break;
					case 8:
						if (o_ptr->flags1 & TR1_MIGHT)
							i--;	/* try again */
						else
							o_ptr->flags1 |= TR1_MIGHT;
						break;
					case 9:
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_BLIND |
											 TR2_RES_DISEN | TR2_RES_CONFU |
								  TR2_RES_SHARD | TR2_RES_NETHR), (NOFLAG));
						break;
					}
					break;
				}
			case TAL_F_XBOW:
				{
					switch (which)
					{
					case 0:
					case 1:
						o_ptr->to_h += 1;
						o_ptr->to_d += randint(3);
						break;
					case 2:
					case 3:
					case 4:
					case 5:
						add_one_flag(o_ptr, (TR1_CON | TR1_STR),
									 (TR2_RES_COLD | TR2_RES_FIRE |
									  TR2_RES_ACID | TR2_RES_ELEC),
									 (TR3_SEE_INVIS | TR3_REGEN | TR3_LITE));
						break;
					case 6:
					case 7:
						if (o_ptr->flags1 & TR1_MIGHT)
							i--;	/* try again */
						else
							o_ptr->flags1 |= TR1_MIGHT;
						break;
					case 8:
						if (o_ptr->flags1 & TR1_SHOTS)
							i--;	/* try again */
						else
							o_ptr->flags1 |= TR1_SHOTS;
						break;
					case 9:
					case 10:
						add_one_flag(o_ptr, (NOFLAG), (TR2_RES_POIS |
											 TR2_RES_SHARD | TR2_RES_CHAOS |
											TR2_RES_LITE), (TR3_HOLD_LIFE));
						break;
					}
					break;
				}
			case TAL_F_ARROW:
			case TAL_F_BOLT:
			case TAL_F_SHOT:
				{
					switch (which)
					{
					case 0:
					case 1:
					case 2:
						o_ptr->to_h += randint(5);
						o_ptr->to_d += randint(5);
						break;
					case 3:
					case 4:
					case 5:
					case 6:
						add_one_flag(o_ptr, (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL |
										  TR1_SLAY_UNDEAD | TR1_SLAY_DEMON |
										 TR1_SLAY_DRAGON | TR1_KILL_DRAGON),
									 (NOFLAG), (NOFLAG));
						break;
					case 7:
					case 8:
					case 9:
					case 10:
						o_ptr->dd += randint(3);
						break;
					}
					break;
				}
			default:
				msg_print("Bug in power assignment.");
				return FALSE;
			}
		}
	}
	/* if the object needs a pval, give it one */
	if (o_ptr->flags1 & TR1_PVAL_MASK)
	{
		o_ptr->pval = m_bonus(pow / 5 + 1, level);
		if (o_ptr->pval < 1)
			o_ptr->pval = 1;
	}

	/* If an object resists an element, it also ignores it */
	if ((o_ptr->flags2 & TR2_RES_ACID) || (o_ptr->flags2 & TR2_IM_ACID))
		o_ptr->flags3 |= TR3_IGNORE_ACID;
	if ((o_ptr->flags2 & TR2_RES_FIRE) || (o_ptr->flags2 & TR2_IM_FIRE))
		o_ptr->flags3 |= TR3_IGNORE_FIRE;
	if ((o_ptr->flags2 & TR2_RES_COLD) || (o_ptr->flags2 & TR2_IM_COLD))
		o_ptr->flags3 |= TR3_IGNORE_COLD;
	if ((o_ptr->flags2 & TR2_RES_ELEC) || (o_ptr->flags2 & TR2_IM_ELEC))
		o_ptr->flags3 |= TR3_IGNORE_ELEC;

	return special;
}

static bool
talent_forge_obj(int level, int type)
{
	int i, item;
	int forge_type, material, quality;
	object_type *o_ptr, forge;
	char tmp = 0;
	cptr q, s;

	switch (type)
	{
	case TAL_MAKE_WEAP:
		{
			do
			{
				if (!get_com("Forge a (S)word, (M)ace, or (P)olearm?", &tmp))
					return FALSE;
				switch (tmp)
				{
				case 's':
				case 'S':
					forge_type = TAL_F_SWORD;
					break;
				case 'm':
				case 'M':
					forge_type = TAL_F_MACE;
					break;
				case 'p':
				case 'P':
					forge_type = TAL_F_POLEARM;
					break;
				default:
					tmp = 0;
					bell("Illegal weapon type.");
					break;
				}
			}
			while (tmp == 0);
			break;
		}
	case TAL_MAKE_ARMOR:
		{
			do
			{
				if (!get_com("Forge a (H)elmet, (B)oots, (S)hield, (G)auntlets, or (A)rmor?", &tmp))
					return FALSE;
				switch (tmp)
				{
				case 'h':
				case 'H':
					forge_type = TAL_F_HELM;
					break;
				case 'a':
				case 'A':
					forge_type = TAL_F_ARMOR;
					break;
				case 'b':
				case 'B':
					forge_type = TAL_F_BOOTS;
					break;
				case 's':
				case 'S':
					forge_type = TAL_F_SHIELD;
					break;
				case 'g':
				case 'G':
					forge_type = TAL_F_GAUNTLETS;
					break;
				default:
					tmp = 0;
					bell("Illegal armor type.");
					break;
				}
			}
			while (tmp == 0);
			break;
		}
	case TAL_MAKE_BOW:
		{
			do
			{
				if (!get_com("Make (B)ow, or (C)rossbow?", &tmp))
					return FALSE;
				switch (tmp)
				{
				case 'b':
				case 'B':
					forge_type = TAL_F_BOW;
					break;
				case 'c':
				case 'C':
					forge_type = TAL_F_XBOW;
					break;
				default:
					tmp = 0;
					bell("Illegal bow type.");
					break;
				}
			}
			while (tmp == 0);
			break;
		}
	case TAL_MAKE_AMMO:
		{
			do
			{
				if (!get_com("Make (C)rossbow bolts, (A)rrows, or (S)ling shot?", &tmp))
					return FALSE;
				switch (tmp)
				{
				case 'A':
				case 'a':
					forge_type = TAL_F_ARROW;
					break;
				case 'C':
				case 'c':
					forge_type = TAL_F_BOLT;
					break;
				case 'S':
				case 's':
					forge_type = TAL_F_SHOT;
					break;
				default:
					tmp = 0;
					bell("Illegal ammo type.");
					break;
				}
			}
			while (tmp == 0);
			break;
		}
	default:
		msg_print("Bug in forging code.");
		return FALSE;
	}
	item_tester_tval = TV_COMPONENT;

	/* Get an item */
	q = "Use which component? ";
	s = "You have no components.";
	if (!get_item(&item, q, s, (USE_INVEN)))
		return FALSE;

	o_ptr = &inventory[item];

	quality = o_ptr->pval;

	material = o_ptr->k_idx - K_MIN_COMPONENT;

	inven_item_decrease(item);

	/* introduce some variance in the quality of components, based on both
	 * luck and skill. Really high-end items are very hard to make. */
	i = randint(100 + luck());
	i += level - 25;
	if (i > 98)
		quality += randint(2);	/* + 1 or 2 */
	if (i > 90)
		quality += 1;
	if (i > 80)
		quality += 1;
	if (i > 65)
		quality += rand_int(2);	/* + 0 or 1 */
	if (i < 35)
		quality -= rand_int(2);	/* - 0 or 1 */
	if (i < 20)
		quality -= 1;
	if (i < 10)
		quality -= 1;
	if (i < 2)
		quality -= randint(2);	/* - 1 or 2 */
	if (quality < 0)
	{
		msg_print("The component was ruined during the forging!");
		return TRUE;
	}

	/* Scale the level to the dungeon depth, for better use with m_bonus */
	level *= 2;

	/* Set up the item */
	o_ptr = &forge;
	if (forge_type < TAL_F_SPECIAL)
	{
		object_prep(o_ptr, K_MIN_FORGED + material * 8 + forge_type);
	}
	else
	{
		switch (forge_type)
		{
		case TAL_F_ARROW:
			object_prep(o_ptr, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
			break;
		case TAL_F_SHOT:
			object_prep(o_ptr, lookup_kind(TV_SHOT, SV_AMMO_NORMAL));
			break;
		case TAL_F_BOLT:
			object_prep(o_ptr, lookup_kind(TV_BOLT, SV_AMMO_NORMAL));
			break;
		case TAL_F_BOW:
			if (randint(quality) > 2)
				object_prep(o_ptr, lookup_kind(TV_BOW, SV_LONG_BOW));
			else
				object_prep(o_ptr, lookup_kind(TV_BOW, SV_SHORT_BOW));
			break;
		case TAL_F_XBOW:
			if (randint(quality) > 4)
				object_prep(o_ptr, lookup_kind(TV_BOW, SV_HEAVY_XBOW));
			else
				object_prep(o_ptr, lookup_kind(TV_BOW, SV_LIGHT_XBOW));
			break;
		default:
			msg_print("Bug in forging");
			return TRUE;
		}
	}
	/* Now forge the item */
	switch (forge_type)
	{
	case TAL_F_ARROW:
		{
			o_ptr->to_h = 2 + rand_int(9) + m_bonus(quality, level);
			o_ptr->to_d = 2 + rand_int(7) + m_bonus(quality, level);
			o_ptr->dd = 1 + m_bonus(quality / 4 + 1, level / 2);
			o_ptr->number = 5 + damroll(5, 5);
			if (forge_add_special(level, forge_type, 1, quality, o_ptr))
				msg_format("These are %s arrows!", superb_word(TRUE));
			break;
		}
	case TAL_F_BOLT:
		{
			o_ptr->to_h = 2 + rand_int(7) + m_bonus(quality, level);
			o_ptr->to_d = 2 + rand_int(9) + m_bonus(quality, level);
			o_ptr->dd = 1 + m_bonus(quality / 4 + 1, level / 2);
			o_ptr->number = 5 + damroll(5, 5);
			if (forge_add_special(level, forge_type, 1, quality, o_ptr))
				msg_format("These are %s bolts!", superb_word(TRUE));
			break;
		}
	case TAL_F_SHOT:
		{
			o_ptr->to_h = 2 + rand_int(8) + m_bonus(quality, level);
			o_ptr->to_d = 2 + rand_int(8) + m_bonus(quality, level);
			o_ptr->dd = 1 + m_bonus(quality / 4 + 1, level / 2);
			o_ptr->number = 5 + damroll(5, 5);
			if (forge_add_special(level, forge_type, 1, quality, o_ptr))
				msg_format("These are %s shots!", superb_word(TRUE));
			break;
		}
	case TAL_F_BOW:
		{
			o_ptr->to_h = m_bonus(quality, level) + damroll(2, 5);
			o_ptr->to_d = m_bonus(quality, level) + damroll(2, 5);
			if (rand_int(10) <= quality / 2)
				o_ptr->to_h += randint(10);
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s bow!", superb_word(FALSE));
			break;
		}
	case TAL_F_XBOW:
		{
			o_ptr->to_h = m_bonus(quality, level) + damroll(2, 5);
			o_ptr->to_d = m_bonus(quality, level) + damroll(2, 5);
			if (rand_int(10) <= quality / 2)
				o_ptr->to_d += randint(10);
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s crossbow!", superb_word(FALSE));
			break;
		}


	case TAL_F_SWORD:
		{
			o_ptr->to_h = rand_int(6) + m_bonus(quality, level);
			o_ptr->to_d = rand_int(6) + m_bonus(quality, level);
			o_ptr->weight += (randint(7) - 4) * 5;
			if (rand_int(10) <= quality / 2)
				o_ptr->to_h += randint(10);
			o_ptr->dd += m_bonus((quality / 5), level / 2);
			o_ptr->ds += m_bonus((quality / 4), level / 2);
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s sword!", superb_word(FALSE));
			break;
		}
	case TAL_F_POLEARM:
		{
			o_ptr->to_h = rand_int(6) + m_bonus(quality, level);
			o_ptr->to_d = rand_int(6) + m_bonus(quality, level);
			o_ptr->weight += (randint(7) - 4) * 8;
			if (rand_int(10) <= quality / 2)
			{
				o_ptr->to_h += randint(5);
				o_ptr->to_d += randint(5);
			}
			o_ptr->dd += m_bonus((quality / 5), level / 2);
			o_ptr->ds += m_bonus((quality / 4), level / 2);
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s axe!", superb_word(FALSE));
			break;
		}
	case TAL_F_MACE:
		{
			o_ptr->to_h = rand_int(6) + m_bonus(quality, level);
			o_ptr->to_d = rand_int(6) + m_bonus(quality, level);
			o_ptr->weight += (randint(7) - 4) * 7;
			if (rand_int(10) <= quality / 2)
				o_ptr->to_d += randint(10);
			o_ptr->dd += m_bonus((quality / 5), level / 2);
			o_ptr->ds += m_bonus((quality / 4), level / 2);
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s mace!", superb_word(FALSE));
			break;
		}
	case TAL_F_HELM:
		{
			o_ptr->ac += m_bonus(quality / 4, level);
			o_ptr->to_a = rand_int(5) + m_bonus(quality / 2, level);
			o_ptr->weight += (randint(7) - 4) * 3;
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s helm!", superb_word(FALSE));
			break;
		}
	case TAL_F_ARMOR:
		{
			o_ptr->ac += m_bonus(quality / 2, level);
			o_ptr->to_a = damroll(3, 4) + m_bonus((quality * 2) / 3, level);
			o_ptr->to_h = -(o_ptr->ac / 6);
			o_ptr->to_h += m_bonus(quality / 5, level);
			if (o_ptr->to_h > 0)
				o_ptr->to_h = 0;
			o_ptr->weight += (randint(7) - 4) * 11;
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s suit of armor!", superb_word(FALSE));
			break;
		}
	case TAL_F_BOOTS:
		{
			o_ptr->ac += m_bonus(quality / 5, level);
			o_ptr->to_a = rand_int(5) + m_bonus(quality / 2, level);
			o_ptr->weight += (randint(7) - 4) * 2;
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("These are %s boots!", superb_word(TRUE));
			break;
		}
	case TAL_F_GAUNTLETS:
		{
			o_ptr->ac += m_bonus(quality / 7, level);
			o_ptr->to_a = rand_int(7) + m_bonus(quality / 2, level);
			o_ptr->weight += (randint(7) - 4) * 2;
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("These are %s gauntlets!", superb_word(TRUE));
			break;
		}
	case TAL_F_SHIELD:
		{
			o_ptr->ac += m_bonus(quality / 4, level);
			o_ptr->to_a = damroll(4, 2) + m_bonus(quality / 2, level);
			o_ptr->weight += (randint(7) - 4) * 4;
			if (forge_add_special(level, forge_type, quality / 2 +
							   rand_int((quality * 3) / 2), quality, o_ptr))
				msg_format("This is %s shield!", superb_word(FALSE));
			break;
		}
	default:
		break;
	}
	item = inven_carry(o_ptr);
	inven_item_describe(item);

	o_ptr = &inventory[item];
	o_ptr->ident |= (IDENT_KNOWN);

	/* give the player a chance of an *Identify*. */
	if (randint(150) < get_skill(S_PRECOG))
	{
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);

		/* Only bother with this if there's something to *id* */
		if (o_ptr->flags1 || o_ptr->flags2 || o_ptr->flags3)
			identify_fully_aux(o_ptr);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Combine and reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

#if 0
	/* Evaluate cost of the item */
	j = 0;						/* Current cost */
	switch (i)
	{
	case TV_ARROW:
	case TV_BOLT:
		j = 1;
		break;
	case TV_BOW:
		j = 300;
		break;
	case TV_SWORD:
		j = 100;
		break;
	case TV_HAFTED:
		j = 150;
		break;
	case TV_POLEARM:
		j = 100;
		break;
	case TV_HELM:
		j = 50;
		break;
	case TV_HARD_ARMOR:
		j = 100;
		break;
	case TV_BOOTS:
		j = 50;
		break;
	case TV_SHIELD:
		j = 100;
		break;
	case TV_GLOVES:
		j = 50;
		break;
	}

	j += 200 * o_ptr->to_h;
	j += 200 * o_ptr->to_d;
	j += 100 * o_ptr->ac;
	j += 150 * o_ptr->to_a;
	j += 300 * o_ptr->pval;
	mask = 1;
	/* This adds value for each special added */
	for (k = 0; k < 32; k++)
	{
		if (o_ptr->flags1 & mask)
			j += flag_levels[k] * 100;
		if (o_ptr->flags2 & mask)
			j += flag_levels[k + 32] * 120;
		if (o_ptr->flags3 & mask)
			j += flag_levels[k + 64] * 120;
		mask <<= 1;
	}
	o_ptr->b_cost = j;
#endif

	return TRUE;
}

/*
 * Determine if a non-skill-based talent may be used.
 */
static bool
spec_tal_valid(talent_type * tal)
{
	if (p_ptr->schange == SHAPE_DRAGON && streq(tal->name, "Breathe Fire"))
		return TRUE;
	return FALSE;
}

/*
 * Print a list of talents.
 */
void
print_talents(byte * talents, int num, int y, int x)
{
	int i, talent;

	talent_type *t_ptr;

	char out_val[160];

	/* Dump the talents */
	for (i = 0; i < num; i++)
	{
		/* Access the talent */
		talent = talents[i];
		t_ptr = &talent_info[talent];

		/* Dump the talent */
		sprintf(out_val, "  %c) %-30s", I2A(talents[i]), t_ptr->name);
		prt(out_val, y + i, x);
	}

	/* Clear the bottom line */
	prt("", y + i, x);
}


/*
 * Allow the user to choose a talent.
 *
 * If a valid talent is chosen, returns the talent's number.
 * If the user hits escape, returns -1.
 * If there are no legal choices, returns -2.
 *
 * If there are ever more than 23 talents, this function will need
 * to break them up in some manner.
 */
static int
get_talent()
{
	int i, j = -1, k;
	int ver;
	byte talent[NUM_TALENTS], num = 0;
	bool flag, redraw, okay = FALSE;
	char choice;
	talent_type *t_ptr;
	char out_val[160];

	/* Extract usable talents */
	for (i = 0; talent_info[i].name != NULL; i++)
	{
		if (talent_info[i].skill == S_NOSKILL)
		{
			/* Non-skill-based talent. */
			if (spec_tal_valid(&talent_info[i]))
			{
				talent[num++] = i;
				okay = TRUE;
			}
		}
		else if (get_raw_skill(talent_info[i].skill) >= talent_info[i].level)
		{
			talent[num++] = i;
			okay = TRUE;
		}
	}
	if (!okay)
		return -2;

	/* Nothing chosen yet */
	flag = FALSE;
	/* No redraw yet */
	redraw = FALSE;

#if 0
	/* Show the list */
	if (redraw)
	{
		/* Save screen */
		screen_save();

		/* Display a list of talents */
		print_talents(talent, num, 1, 50);
	}

#endif

	/* Build a prompt (accept all talents) */
	strnfmt(out_val, 78,
			"(Talents %c-%c, *=List, ESC=exit) Use which talent? ",
			I2A(talent[0]), I2A(talent[num - 1]));

	/* Get a talent from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();

				/* Hide list */
				redraw = FALSE;
			}

			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of talents */
				print_talents(talent, num, 1, 50);
			}

			/* Ask again */
			continue;
		}


		/* Note verify */
		ver = (isupper(choice));

		/* Lowercase */
		choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if (i < 0)
		{
			bell("Illegal talent choice.");
			continue;
		}

		/* Find and save the talent index */
		for (k = 0; k < num; k++)
		{
			if (talent[k] == i)
			{
				j = talent[k];
				break;
			}
		}
		/* Invalid choice */
		if (k == num)
		{
			bell("Illegal talent choice.");
			continue;
		}

		/* Verify it */
		if (ver)
		{
			char tmp_val[160];

			/* Access the talent */
			t_ptr = &talent_info[j];

			/* Prompt */
			strnfmt(tmp_val, 78, "Attempt %s? ", t_ptr->name);

			/* Belay that order */
			if (!get_check(tmp_val))
				continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();

		/* Hack -- forget redraw */
		/* redraw = FALSE; */
	}

	/* Abort if needed */
	if (!flag)
		return -1;

	/* Success */
	return j;
}

/*
 * Check to see if conditions are right to do some work.
 */
static bool
good_work_cond()
{
	if (p_ptr->depth != 0)
	{
		msg_print("It's not safe to work here!");
		return FALSE;
	}
	if (p_ptr->confused > 0)
	{
		msg_print("You are too confused.");
		return FALSE;
	}
	if (no_lite())
	{
		msg_print("You have no light to work by!");
		return FALSE;
	}
	if (p_ptr->blind)
	{
		msg_print("You can't see!");
		return FALSE;
	}

	return TRUE;
}

void
do_cmd_talents()
{
	int level = 0, tal, dir;
	char tval;
	char tmp;
	talent_type *t_ptr;

	if (p_ptr->tt && !p_ptr->wizard)
	{
		msg_print("You can't use any talents right now.");
		return;
	}

	tal = get_talent();

	if (tal < 0)
	{
		if (tal == -2)			/* No talents available */
			msg_print("You have no talents yet.");
		return;
	}

	t_ptr = &talent_info[tal];

	if (t_ptr->skill != S_NOSKILL)
	{
		level = get_skill(t_ptr->skill);
	}

	switch (tal)
	{
	case 0:					/* Detect traps */
		{
			detect_traps();
			break;
		}
	case 1:					/* Predict Weather */
		{
			predict_weather(level * 3);
			break;
		}
	case 2:					/* Detect Evil */
		{
			detect_evil();
			break;
		}
	case 3:					/* Detect Animals */
		{
			detect_animals();
			break;
		}
	case 4:					/* Id scrolls/potions */
		{
			item_tester_hook = item_tester_hook_alchemy;
			if (!ident_spell())
				return;
			break;
		}
	case 5:					/* Id number of charges on known wands/staves */
		{
			item_tester_hook = item_tester_hook_charges;
			if (!ident_spell())
				return;
			break;
		}
	case 6:					/* Forge weapons */
		{
			if (!good_work_cond())
				return;
			if (!talent_forge_obj(level, TAL_MAKE_WEAP))
				return;
			break;
		}
	case 7:					/* Forge armor */
		{
			if (!good_work_cond())
				return;
			if (!talent_forge_obj(level, TAL_MAKE_ARMOR))
				return;
			break;
		}
	case 8:					/* Make ammo */
		{
			if (!good_work_cond())
				return;
			if (!talent_forge_obj(level, TAL_MAKE_AMMO))
				return;
			break;
		}
	case 9:					/* Make bows */
		{
			if (!good_work_cond())
				return;
			if (!talent_forge_obj(level, TAL_MAKE_BOW))
				return;
			break;
		}
	case 10:					/* Fenneling. */
		{
			if (!good_work_cond())
				return;
			if (!talent_fennel(level))
				return;
			break;
		}
	case 11:					/* Make scrolls/potions */
		{
			tmp = 0;
			if (!good_work_cond())
				return;
			do
			{
				if (!get_com("Make a (S)croll or (P)otion?", &tmp))
					return;
				switch (tmp)
				{
				case 'p':
				case 'P':
					tval = TV_POTION;
					break;
				case 's':
				case 'S':
					tval = TV_SCROLL;
					break;
				default:
					tmp = 0;
					bell("Illegal alchemy choice.");
					break;
				}
			}
			while (tmp == 0);
			if (!talent_create_obj(level, tval))
				return;
			break;
		}
	case 12:					/* Make wands/staves */
		{
			tmp = 0;
			if (!good_work_cond())
				return;

			do
			{
				if (!get_com("Make a (W)and or (S)taff?", &tmp))
					return;
				switch (tmp)
				{
				case 'w':
				case 'W':
					tval = TV_WAND;
					break;
				case 's':
				case 'S':
					tval = TV_STAFF;
					break;
				default:
					tmp = 0;
					bell("Illegal infusion choice.");
					break;
				}
			}
			while (tmp == 0);

			if (!talent_create_obj(level, tval))
				return;
			break;
		}
	case 13:					/* Meditate */
		{
			(void)set_afraid(0);
			(void)set_confused(0);
			(void)set_blind(0);
			break;
		}
	case 14:					/* Restore Exp */
		{
			if (!restore_level())
				msg_print("Nothing happened.");
			break;
		}
	case 15:					/* Recharge wands/staves */
		{
			if (!recharge(get_raw_skill(S_INFUSION) / 5))
				return;
			break;
		}
	case 16:					/* Breathe Fire */
		{
			if (!get_aim_dir(&dir))
				return;
			(void)fire_ball(GF_FIRE, dir, p_ptr->chp / 2, 2);
			break;
		}
	default:
		{
			msg_print("Unknown talent");
			return;
		}
	}
	p_ptr->tt = t_ptr->timeout;
	p_ptr->energy_use = 100;
}
