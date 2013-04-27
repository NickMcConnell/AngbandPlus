/*
 * File: attack.c
 * Purpose: Attacking (both throwing and melee) code
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "raceflag.h"
#include "tvalsval.h"

#include "flow.h"
#include "keypad.h"

/*
 * Misc constants
 */
#define BTH_PLUS_ADJ    3       /* Adjust BTH per plus-to-hit */

/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(const object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->obj_id.tval)
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
			return (50);
		}

		/* Sometimes break */
		case TV_ARROW:
		{
			return (35);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		{
			return (25);
		}
	}

	/* Rarely break */
	return (10);
}

/*
 * centralize for AI
 */
static s32b test_hit_threshold(s32b chance,s32b ac)
{
	return (chance <= (ac * 3 / 4)) ?	(95*chance)
									:	(5*chance + 90*(ac*3/4));	/* parentheses needed to preserve original probabilities */
}

/*
 * Determine if the player "hits" a monster.
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit(int chance, int ac, bool vis)
{
	/* Penalize invisible targets */
	if (!vis) chance /= 2;

	return test_hit_threshold(chance,ac) <= rand_int(100*chance);
}

void test_hit_ratio(int chance, int ac, bool vis, s32b& numerator, s32b& denominator)
{
	/* Penalize invisible targets */
	if (!vis) chance /= 2;

	denominator = 100*chance;
	numerator = denominator-test_hit_threshold(chance,ac);
}

/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
int critical_shot(int weight, int plus, int dam)
{
	/* Extract "shot" power */
	int i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));

	/* Critical hit */
	if (randint(5000) <= i)
	{
		int k = weight + randint(500);

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
	/* Extract "blow" power */
	int i = (weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3));

	/* Chance */
	if (randint(5000) <= i)
	{
		int k = weight + randint(650);

		if (k < 400)
		{
			sound(MSG_HIT_GOOD);
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
			sound(MSG_HIT_GREAT);
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else if (k < 900)
		{
			sound(MSG_HIT_SUPERB);
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
		else if (k < 1300)
		{
			sound(MSG_HIT_HI_GREAT);
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam + 20;
		}
		else
		{
			sound(MSG_HIT_HI_SUPERB);
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
		}
	}
	else 
	{
		sound(MSG_HIT);
	}

	return (dam);
}

static int slay_multiplier(const object_type *o_ptr, const monster_type *m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = m_ptr->race();

	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Extract the flags */
	object_flags(o_ptr, f);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->obj_id.tval)
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
			if (r_ptr->flags[2] & RF2_ANIMAL)
			{
				if ((f[0] & (TR1_SLAY_ANIMAL)) && (mult < 2)) mult = 2; 
			}

			/* Slay Evil */
			if (r_ptr->flags[2] & RF2_EVIL)
			{
				if ((f[0] & (TR1_SLAY_EVIL)) && (mult < 2)) mult = 2; 
			}

			/* Slay Undead */
			if (r_ptr->flags[2] & RF2_UNDEAD)
			{
				if ((f[0] & (TR1_SLAY_UNDEAD)) && (mult < 3)) mult = 3; 
				if ((f[0] & (TR1_KILL_UNDEAD)) && (mult < 5)) mult = 5; /* Execute Undead */
			}

			/* Slay Demon */
			if (r_ptr->flags[2] & RF2_DEMON)
			{
				if ((f[0] & (TR1_SLAY_DEMON)) && (mult < 3)) mult = 3; 
				if ((f[0] & (TR1_KILL_DEMON)) && (mult < 5)) mult = 5; /* Execute Demon */
			}

			/* Slay Orc */
			if (r_ptr->flags[2] & RF2_ORC)
			{
				if ((f[0] & (TR1_SLAY_ORC)) && (mult < 3)) mult = 3; 
			}

			/* Slay Troll */
			if (r_ptr->flags[2] & RF2_TROLL)
			{
				if ((f[0] & (TR1_SLAY_TROLL)) && (mult < 3)) mult = 3; 
			}

			/* Slay Giant */
			if (r_ptr->flags[2] & RF2_GIANT)
			{
				if ((f[0] & (TR1_SLAY_GIANT)) && (mult < 3)) mult = 3; 
			}

			/* Slay Dragon */
			if (r_ptr->flags[2] & RF2_DRAGON)
			{
				if ((f[0] & (TR1_SLAY_DRAGON)) && (mult < 3)) mult = 3; 
				if ((f[0] & (TR1_KILL_DRAGON)) && (mult < 5)) mult = 5; /* Execute Dragon */
			}

			/* Brand (Acid) */
			if (r_ptr->flags[2] & RF2_IM_ACID)
			{
				if ((f[0] & (TR1_BRAND_ACID)) && (mult < 3)) mult = 3; 
			}

			/* Brand (Elec) */
			if (r_ptr->flags[2] & RF2_IM_ELEC)
			{
				if ((f[0] & (TR1_BRAND_ELEC)) && (mult < 3)) mult = 3; 
			}

			/* Brand (Fire) */
			if (r_ptr->flags[2] & RF2_IM_FIRE)
			{
				if ((f[0] & (TR1_BRAND_FIRE)) && (mult < 3)) mult = 3; 
			}

			/* Brand (Cold) */
			if (r_ptr->flags[2] & RF2_IM_COLD)
			{
				if ((f[0] & (TR1_BRAND_COLD)) && (mult < 3)) mult = 3; 
			}

			/* Brand (Poison) */
			if (r_ptr->flags[2] & RF2_IM_POIS)
			{
				if ((f[0] & (TR1_BRAND_POIS)) && (mult < 3)) mult = 3; 
			}

			break;
		}
	}

	return mult;
}

static void learn_multiplier(const object_type *o_ptr, const monster_type *m_ptr)
{
	if (!m_ptr->ml) return;

	{	/* C-ish blocking brace */
	monster_race *r_ptr = m_ptr->race();
	monster_lore *l_ptr = m_ptr->lore();

	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Extract the flags */
	object_flags(o_ptr, f);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->obj_id.tval)
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
			if (r_ptr->flags[2] & RF2_ANIMAL)
			{
				if (f[0] & (TR1_SLAY_ANIMAL)) l_ptr->flags[2] |= RF2_ANIMAL;
			}

			/* Slay Evil */
			if (r_ptr->flags[2] & RF2_EVIL)
			{
				if (f[0] & (TR1_SLAY_EVIL)) l_ptr->flags[2] |= RF2_EVIL;
			}

			/* Slay/Execute Undead */
			if (r_ptr->flags[2] & RF2_UNDEAD)
			{
				if (f[0] & (TR1_SLAY_UNDEAD | TR1_KILL_UNDEAD)) l_ptr->flags[2] |= RF2_UNDEAD;
			}

			/* Slay/Execute Demon */
			if (r_ptr->flags[2] & RF2_DEMON)
			{
				if (f[0] & (TR1_SLAY_DEMON | TR1_KILL_DEMON)) l_ptr->flags[2] |= RF2_DEMON;
			}

			/* Slay Orc */
			if (r_ptr->flags[2] & RF2_ORC)
			{
				if (f[0] & (TR1_SLAY_ORC)) l_ptr->flags[2] |= RF2_ORC;
			}

			/* Slay Troll */
			if (r_ptr->flags[2] & RF2_TROLL)
			{
				if (f[0] & (TR1_SLAY_TROLL)) l_ptr->flags[2] |= RF2_TROLL;
			}

			/* Slay Giant */
			if (r_ptr->flags[2] & RF2_GIANT)
			{
				if (f[0] & (TR1_SLAY_GIANT)) l_ptr->flags[2] |= RF2_GIANT;
			}

			/* Slay/Execute Dragon */
			if (r_ptr->flags[2] & RF2_DRAGON)
			{
				if (f[0] & (TR1_SLAY_DRAGON | TR1_KILL_DRAGON)) l_ptr->flags[2] |= RF2_DRAGON;
			}

			/* Brand (Acid) */
			if (f[0] & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags[2] & RF2_IM_ACID) l_ptr->flags[2] |= RF2_IM_ACID;
			}

			/* Brand (Elec) */
			if (f[0] & (TR1_BRAND_ELEC))
			{
				/* Notice immunity */
				if (r_ptr->flags[2] & RF2_IM_ELEC) l_ptr->flags[2] |= RF2_IM_ELEC;
			}

			/* Brand (Fire) */
			if (f[0] & (TR1_BRAND_FIRE))
			{
				/* Notice immunity */
				if (r_ptr->flags[2] & RF2_IM_FIRE) l_ptr->flags[2] |= RF2_IM_FIRE;
			}

			/* Brand (Cold) */
			if (f[0] & (TR1_BRAND_COLD))
			{
				/* Notice immunity */
				if (r_ptr->flags[2] & RF2_IM_COLD) l_ptr->flags[2] |= RF2_IM_COLD;
			}

			/* Brand (Poison) */
			if (f[0] & (TR1_BRAND_POIS))
			{
				/* Notice immunity */
				if (r_ptr->flags[2] & RF2_IM_POIS) l_ptr->flags[2] |= RF2_IM_POIS;
			}

			break;
		}
	}
	}	/* end blocking brace */
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
int tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr)
{
	learn_multiplier(o_ptr,m_ptr);
	return tdam*slay_multiplier(o_ptr,m_ptr);
}

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(coord g)
{
	int num = 0, k, bonus, chance;

	const object_type* const o_ptr = &p_ptr->inventory[INVEN_WIELD]; /* Get the weapon */

	char m_name[80];

	bool fear = FALSE;
	bool do_quake = FALSE;


	/* Get the monster */
	monster_type* const m_ptr = m_ptr_from_m_idx(cave_m_idx[g.y][g.x]);
	monster_race* const r_ptr = m_ptr->race();
	monster_lore* const l_ptr = m_ptr->lore();


	/* Disturb the player */
	disturb(0, 0);


	/* Disturb the monster */
	m_ptr->csleep = 0;


	/* Extract monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);


	if (m_ptr->ml)	/* if visible */
		{
		monster_race_track(m_ptr->r_idx);	/* Auto-Recall if possible */
		health_track(cave_m_idx[g.y][g.x]);	/* Track a new monster */
		}


	/* Handle player fear */
	if (p_ptr->timed[TMD_AFRAID])
	{
		/* Message */
		msg_format("You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}


	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skills[SKILL_TO_HIT_MELEE] + (bonus * BTH_PLUS_ADJ));


	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
		/* Test for hit */
		if (test_hit(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Message */
			message_format(MSG_GENERIC, m_ptr->r_idx, "You hit %s.", m_name);

			/* Hack -- bare hands do one damage */
			k = 1;

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				k = o_ptr->d.damroll();
				k = tot_dam_aux(o_ptr, k, m_ptr);
				if (p_ptr->impact && (k > 50)) do_quake = TRUE;
				k += o_ptr->to_d;
				k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			}

			/* Apply the player damage bonuses */
			k += p_ptr->to_d;

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (p_ptr->wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->chp);
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[g.y][g.x], k, &fear, NULL)) break;

			/* Confusion attack */
			if (p_ptr->confusing)
			{
				/* Cancel glowing hands */
				p_ptr->confusing = FALSE;

				/* Message */
				msg_print("Your hands stop glowing.");

				/* Confuse the monster */
				if (r_ptr->flags[2] & RF2_NO_CONF)
				{
					if (m_ptr->ml) l_ptr->flags[2] |= RF2_NO_CONF;

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
	}


	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand */
	if (do_quake) earthquake(p_ptr->loc, 10);
}

void
player_type::melee_analyze(monster_type* m_ptr, int& min_dam, int& median_dam, int& max_dam)
{
	if (timed[TMD_AFRAID])
	{	/* fear negates melee damage */
		min_dam = median_dam = max_dam = 0;
		return;
	}

	/* Get the weapon */
	const object_type* const o_ptr = &p_ptr->inventory[INVEN_WIELD];

	/* Hack -- bare hands do one damage */
	min_dam = median_dam = max_dam = 1;

	/* Handle normal weapon */
	/* ignore criticals for now */
	if (o_ptr->k_idx)
	{
		int mult = slay_multiplier(o_ptr,m_ptr);
		min_dam = o_ptr->d.minroll();
		median_dam = o_ptr->d.medianroll();
		max_dam = o_ptr->d.maxroll();

		min_dam *= mult;
		median_dam *= mult;
		max_dam *= mult;

		min_dam += o_ptr->to_d;
		median_dam += o_ptr->to_d;
		max_dam += o_ptr->to_d;
	}

	/* Apply the player damage bonuses */
	min_dam += p_ptr->to_d;
	median_dam += p_ptr->to_d;
	max_dam += p_ptr->to_d;

	/* No negative damage */
	if (min_dam < 0) min_dam = 0;
	if (median_dam < 0) median_dam = 0;
	if (max_dam < 0) max_dam = 0;
}


/* Firing and throwing are similar 
 * NOTE: This is a Zaiband change target.  I want (basically) an inverse-square law.
 * For design purposes, assume that XBows are analogous to firearms.
 * "The primary problem with pistols is poor accuracy, it being difficult to hit a 
 *  man-sized target beyond 25 meters." -- James F. Dunnigan, "How to Make War"
 * Not sure what that means...but it looks like 3 squares at 10'/square, or 6 squares at 
 * 5'/square, is important.  Most magical attacks don't have this problem, since they
 * include guidance systems.
 * We can do this either by using inverse-square directly, or by causing inaccuracies in the 
 * 'targeted point' and doing a straight inverse.
 *
 * Borrowing from prototype logic for "Iskandria: a quasi-realistic..."
 * Another way of balancing: assume that a perfect 'snap shot' would have an error of five 
 * degrees in phi, viewing the intended direction as phi=0 in spherical coordinates.  For an 
 * unarmored man-sized target, we get (ignoring the 5% fumble) for ac 0:
 *   100% hit : circle fits within torso
 *   31.8% hit : circle circumscribes figure
 * Beyond the second point, we have inverse-square.  Within the above range, we have the linear overestimate.
 * no-aim: 5 degrees; aim: 1 degree.
 */

/* prepare to generalize:
 * implicit parameters: p_ptr->loc
 * extract monster/player from location
 */
bool missile_test_hit(int chance, int ac, int vis, int distance, coord loc)
{
	const bool is_player = 0>cave_m_idx[loc.y][loc.x];
	bool aiming = TRUE;

	/* count monsters.  If zero are adjacent, allow aiming. */
	/* Yes, undetected monsters prohibit aiming. */
	int i;
	if (is_player)
	{
		for(i=0;i<KEYPAD_DIR_MAX;i++)
			if (0<cave_m_idx[loc.y+ddy_ddd[i]][loc.x+ddx_ddd[i]])
				{	/* found a monster...no aiming! */
				aiming = FALSE;
				break;
				}
	}
	else
	{
		for(i=0;i<KEYPAD_DIR_MAX;i++)
			if (0>cave_m_idx[loc.y+ddy_ddd[i]][loc.x+ddx_ddd[i]])
				{	/* found a player...no aiming! */
				aiming = FALSE;
				break;
				}
	}

	if (aiming)
		{	/* aiming version: normalize with 1 degree of random phi. */
			/* inscribed circle at 41', circumscribed circle at 171' */
		if (4<distance)
			{
			if (17>=distance)
				{	/* linear optimism drops to 1/3 at distance 17 */
					/* distances 5...16 can be improved by one-shot calculations */
/*				if ((39-2*(distance-4))<=rand_int(39)) return (FALSE); */
				if ((47-2*distance)<=rand_int(39)) return (FALSE);
				}
			else{	/* 18 or more away: use inverse square on 1/3 at 17: 289/(3*distance squared) */
				int TRIPLE_OF_DISTANCE_SQUARED = 3*distance*distance;
				if (289<=rand_int(TRIPLE_OF_DISTANCE_SQUARED)) return (FALSE);
				}
			}
		}
	else{	/* no aiming version: normalize with 5 degrees of random phi */
			/* inscribed circle at 8.5', circumscribed circle at 34.2' */
		if (is_player) msg_print("Nearby threats prevent you from aiming your weapon carefully.");
		if (1<distance)
			{
			if (3>=distance)
				{	/* linear optimism: 1/3 not close at distance 2, 2/3 not close at distance 3 */
					/* distance 2 can be improved by a one-shot calculation */
				if ((4-distance)<=rand_int(3)) return (FALSE);
				}
			else{	/* 4 or more away: use inverse-square on 1/3 at 3: 3/distance squared. */
				int DISTANCE_SQUARED = distance*distance;
				if (3<=rand_int(DISTANCE_SQUARED)) return (FALSE);
				}
			}
		}

	/* use melee algorithm */
	return test_hit(chance,ac,vis);
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
	coord t, tt;
	int dir, item;
	int i, j;
	int tdam, tdis, thits, tmul;
	int bonus, chance;

	object_type object_type_body;

	object_type *o_ptr;
	const object_type* const j_ptr = &p_ptr->inventory[INVEN_BOW];	/* Get the "bow" (if any) */
	object_type *i_ptr = &object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	coord path_g[MAX(DUNGEON_HGT,DUNGEON_WID)];

	const char* q = "Fire which item? ";
	const char* s = "You have nothing to fire.";

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Require a usable launcher */
	if (!j_ptr->obj_id.tval || !p_ptr->ammo_tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Get an item */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Obtain a local object */
	*i_ptr = *o_ptr;

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
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, ODESC_FULL);

	/* Find the color and symbol for the object for throwing */
	missile_attr = i_ptr->attr_user();
	missile_char = i_ptr->char_user();


	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from thrown object plus launcher bonus */
	tdam = i_ptr->d.damroll() + i_ptr->to_d + j_ptr->to_d;

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skills[SKILL_TO_HIT_BOW] + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range XXX XXX */
	tdis = 10 + 5 * tmul;


	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);

	extrapolate_target(tt,dir);

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->loc, tt, true, &wall_stop);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		coord n = path_g[i];

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(n.y, n.x)) break;

		/* Advance */
		t = n;

		/* Only do visuals if the player can "see" the missile */
		if (player_can_see_bold(t.y, t.x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, t);
			move_cursor_relative(t);

			Term_fresh();

			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(t);

			Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[t.y][t.x] > 0)
		{
			monster_type* const m_ptr = m_ptr_from_m_idx(cave_m_idx[t.y][t.x]);
			monster_race* const r_ptr = m_ptr->race();
			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize distance travelled) */
			if (missile_test_hit(chance, r_ptr->ac, m_ptr->ml, distance(p_ptr->loc.y, p_ptr->loc.x, t.y, t.x), p_ptr->loc))
			{
				bool fear = FALSE;

				/* grammatically living monsters die; others are destroyed */
				const char* const note_dies = (r_ptr->is_nonliving()) ? " is destroyed." : " dies.";

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					message_format(MSG_SHOOT_HIT, 0, "The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Message */
					message_format(MSG_SHOOT_HIT, 0, "The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race, monster */
					if (m_ptr->ml)
					{
						monster_race_track(m_ptr->r_idx);
						health_track(cave_m_idx[t.y][t.x]);
					}					
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(j_ptr, tdam, m_ptr);
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);
				tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->chp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[t.y][t.x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[t.y][t.x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

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
	drop_near(i_ptr, j, t);
}

void
player_type::missile_analyze(monster_type* m_ptr, int& min_dam, int& median_dam, int& max_dam)
{
	const object_type* const j_ptr = &p_ptr->inventory[INVEN_BOW];	/* Get the "bow" (if any) */

	/* Require a usable launcher */
	if (!j_ptr->obj_id.tval || !p_ptr->ammo_tval)
	{
		min_dam = median_dam = max_dam = 0;
		return;
	}

	assert(0 <= inven_cnt && INVEN_PACK >= inven_cnt && "precondition");
	assert(inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Assume a base multiplier */
	int tmul = p_ptr->ammo_mult;
	int item = -1;
	int i;
	for (i=0; i<inven_cnt; ++i)
	{
		const object_type* const o_ptr = &p_ptr->inventory[i];
		if (p_ptr->ammo_tval!=o_ptr->obj_id.tval) continue;
		if (-1==item)
		{
			int slayfactor = slay_multiplier(o_ptr,m_ptr)*slay_multiplier(j_ptr,m_ptr);
			item = i;
			min_dam = o_ptr->d.minroll() + o_ptr->to_d + j_ptr->to_d;
			median_dam = o_ptr->d.medianroll() + o_ptr->to_d + j_ptr->to_d;
			max_dam = o_ptr->d.maxroll() + o_ptr->to_d + j_ptr->to_d;

			if (0 > min_dam) min_dam = 0;
			if (0 > median_dam) median_dam = 0;
			if (0 > max_dam) max_dam = 0;

			/* Boost the damage */
			min_dam *= tmul;
			median_dam *= tmul;
			max_dam *= tmul;
			min_dam *= slayfactor;
			median_dam *= slayfactor;
			max_dam *= slayfactor;
		}
		else
		{
			int slayfactor = slay_multiplier(o_ptr,m_ptr)*slay_multiplier(j_ptr,m_ptr);
			int test = o_ptr->d.medianroll() + o_ptr->to_d + j_ptr->to_d;
			if (0 > test) test = 0;
			test *= tmul;
			test *= slayfactor;
			if (test<=median_dam) continue;

			item = i;
			min_dam = o_ptr->d.minroll() + o_ptr->to_d + j_ptr->to_d;
			median_dam = o_ptr->d.medianroll() + o_ptr->to_d + j_ptr->to_d;
			max_dam = o_ptr->d.maxroll() + o_ptr->to_d + j_ptr->to_d;

			if (0 > min_dam) min_dam = 0;
			if (0 > median_dam) median_dam = 0;
			if (0 > max_dam) max_dam = 0;

			/* Boost the damage */
			min_dam *= tmul;
			median_dam *= tmul;
			max_dam *= tmul;
			min_dam *= slayfactor;
			median_dam *= slayfactor;
			max_dam *= slayfactor;
		}
	}

	/* todo: check ground as well */
	if (-1==item)
	{
		min_dam = median_dam = max_dam = 0;
		return;
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
 */
void do_cmd_throw(void)
{
	int dir, item;
	coord t, tt;
	int i, j;
	int chance, tdam, tdis;
	int mul, div;

	object_type *o_ptr;

	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	coord path_g[MAX(DUNGEON_HGT,DUNGEON_WID)];

	const char* q = "Throw which item? ";
	const char* s = "You have nothing to throw.";

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get an item */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	o_ptr = get_o_ptr_from_inventory_or_floor(item);

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Obtain a local object */
	*i_ptr = *o_ptr;

	/* Distribute the charges of rods/wands/staves between the stacks */
	distribute_charges(o_ptr, i_ptr, 1);

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
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, ODESC_FULL);

	/* Find the color and symbol for the object for throwing */
	missile_attr = i_ptr->attr_user();
	missile_char = i_ptr->char_user();


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = i_ptr->d.damroll() + i_ptr->to_d;

	/* Chance of hitting */
	chance = (p_ptr->skills[SKILL_TO_HIT_THROW] + (p_ptr->to_h * BTH_PLUS_ADJ));


	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Start at the player */
	t = p_ptr->loc;

	extrapolate_target(tt,dir);

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->loc, tt, true, &wall_stop);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		coord n = path_g[i];

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(n.y, n.x)) break;

		/* Advance */
		t = n;

		/* Only do visuals if the player can "see" the missile */
		if (player_can_see_bold(t.y, t.x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, t);
			move_cursor_relative(t);

			Term_fresh();

			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(t);

			Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[t.y][t.x] > 0)
		{
			monster_type* const m_ptr = m_ptr_from_m_idx(cave_m_idx[t.y][t.x]);
			monster_race* const r_ptr = m_ptr->race();
			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (missile_test_hit(chance, r_ptr->ac, m_ptr->ml, distance(p_ptr->loc.y, p_ptr->loc.x, t.y, t.x), p_ptr->loc))
			{
				bool fear = FALSE;

				/* grammatically living monsters die; others are destroyed */
				const char* const note_dies = (r_ptr->is_nonliving()) ? " is destroyed." : " dies.";

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
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[t.y][t.x]);
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
					           tdam, m_ptr->chp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[t.y][t.x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[t.y][t.x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

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
	drop_near(i_ptr, j, t);
}
