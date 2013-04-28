/* File: spells3.c */

/*
 * Object effect and alteration code.
 *
 * Smash potions, activate scrolls, fire off devices.  Curse and uncurse
 * equipment.  Enchanting and branding objects.  Pseudo-ID, learning,
 * identification.  Recharge and tap magical devices.  The effects of
 * specific objects.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Food hits a monster, observe effects
 *
 * If "who" is -1, use throwing skill for some things.
 * If "who" is -2, use burglary skill for some things.
 * Otherwise, use character power  XXX XXX
 *
 * We never explicitly notice anything ('shrooms are meant to be eaten),
 * but the observant player can avoid certain items by watching for and
 * remembering their effects.
 *
 * We could use user-editable dice and sides to control damage and power,
 * but this would require a deal of coding in various places not to be
 * misleading for some items.
 */
void food_hit_effect(int who, int y, int x, object_type *o_ptr)
{
	int typ = 0;
	int dam = 0;
	u32b flg;

	/* By default, use character power for effects */
	int skill = S_NOSKILL;

	/* If "who" is -1, use throwing skill for some things. */
	if (who == -1) skill = S_THROWING;

	/* If "who" is -2, use burglary skill for some things. */
	if (who == -2) skill = S_BURGLARY;


	/* Analyze the food */
	switch (o_ptr->sval)
	{
		case SV_FOOD_BLINDNESS:
		case SV_FOOD_CONFUSION:
		case SV_FOOD_HALLUCINATION:
		{
			typ = GF_DO_CONF;
			dam = get_skill(skill, 20, 65);
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			typ = GF_DO_FEAR;
			dam = get_skill(skill, 20, 65);
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			typ = GF_DO_SLEEP;
			dam = get_skill(skill, 25, 75);
			break;
		}

		case SV_FOOD_POISON:
		{
			typ = GF_POIS;
			dam = get_skill(skill, 10, 30);
			break;
		}

		case SV_FOOD_ENVENOMATION:
		{
			typ = GF_POIS;
			dam = get_skill(skill, 30, 150);
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			typ = GF_POIS;
			dam = get_skill(skill, 20, 60);
			break;
		}

		case SV_FOOD_DISEASE:
		{
			typ = GF_POIS;
			dam = get_skill(skill, 40, 150);
			break;
		}

		case SV_FOOD_RUINATION:
		{
			typ = GF_POIS;
			dam = get_skill(skill, 60, 250);
			break;
		}

		case SV_FOOD_METAMORPHOSIS:
		{
			typ = GF_DO_POLY;
			dam = get_skill(skill, 40, 120);
			break;
		}

		case SV_FOOD_REGEN_MANA:
		{
			typ = GF_MANA;
			dam = get_skill(skill, 0, 100);
			break;
		}

		/* No special effects for most things */
		default:
		{
			/* Most things must be thrown or used in a trap */
			if (who)
			{
				typ = GF_HURT;

				/* Damage ranges from 0.5x to 1.5x normal */
				dam = damroll(o_ptr->dd, o_ptr->ds) * get_skill(skill, 5, 15);
				dam = div_round(dam, 10);
			}
			break;
		}
	}

	/* No damage, no effects */
	if (!dam) return;


	/* Jump to target, no graphics, ignore objects and the dungeon */
	flg = PROJECT_BOOM | PROJECT_JUMP | PROJECT_KILL | PROJECT_PLAY |
	      PROJECT_HIDE;

	/* Cast the projection, do not notice effects */
	(void)project((who < -1 ? -1 : who), 0, y, x, y, x, dam,
	              typ, flg, 0, 0);
}




/*
 * Smash a potion, observe effects
 *
 * Potions smash at full strength if "who" is not zero.  Otherwise their
 * strength is cut by two-thirds.  This prevents a floor with potions
 * becoming quite so much of a minefield.
 *
 * Note the "allow_activate" variable, which prevents the projections of
 * smashed potions smashing other potions.  XXX XXX
 *
 * If "who" is -1, use throwing skill for some things.
 * If "who" is -2, use burglary skill for some things.
 *
 * Return TRUE if we noticed anything.
 */
bool potion_smash_effect(int who, int y, int x, object_type *o_ptr)
{
	bool do_fire_star = FALSE;

	int rad = 1;
	int typ = 0;
	int dam = 0;
	int dice, sides;
	u32b flg;

	int skill = S_NOSKILL;
	bool notice;


	/* Check if potions are allowed to smash */
	if (!allow_activate) return (FALSE);


	/* If "who" is -1, use throwing skill for some things. */
	if (who == -1) skill = S_THROWING;

	/* If "who" is -2, use burglary skill for some things. */
	if (who == -2) skill = S_BURGLARY;


	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_POTION_GRENADE:
		{
			int dummy, p = o_ptr->pval;

			/* Pval of potion -> sval of essence -> kind of magic */
			typ = essence_to_magic(&dummy, &p);

			dam = damroll(o_ptr->dd, o_ptr->ds);
			rad = 1 + div_round(dam, 100);
			break;
		}

		case SV_POTION_SLOWNESS:
		{
			typ = GF_DO_SLOW;
			if (who < 0) dam = get_skill(skill, 20, 75);
			else         dam = 20;
			break;
		}

		case SV_POTION_POISON:
		{
			typ = GF_POIS;
			if (who < 0) sides = get_skill(skill, 4, 16);
			else         sides = 4;
			dam = damroll(3, sides);
			break;
		}

		case SV_POTION_BLINDNESS:
		case SV_POTION_CONFUSION:
		{
			typ = GF_DO_CONF;
			if (who < 0) dam = get_skill(skill, 20, 65);
			else         dam = 20;
			break;
		}

		case SV_POTION_SLEEP:
		{
			typ = GF_DO_SLEEP;
			if (who < 0) dam = get_skill(skill, 20, 70);
			else         dam = 20;
			break;
		}

		case SV_POTION_LOSE_MEMORIES:
		{
			typ = GF_FORGET;
			if (who < 0) dice = get_skill(skill, 5, 20);
			else         dice = 10;
			dam = damroll(dice, 8);
			break;
		}

		/* General "curse" potions */
		case SV_POTION_DEC_STR:
		case SV_POTION_DEC_INT:
		case SV_POTION_DEC_WIS:
		case SV_POTION_DEC_DEX:
		case SV_POTION_DEC_CON:
		case SV_POTION_DEC_CHR:
		{
			typ = GF_CURSE;
			if (who < 0) dam = get_skill(skill, 20, 80);
			else         dam = 40;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			typ = GF_SHARD;
			dam = damroll(o_ptr->dd, o_ptr->ds);
			break;
		}

		case SV_POTION_DEATH:
		{
			typ = GF_DEATH;
			dam = damroll(o_ptr->dd, o_ptr->ds);
			break;
		}

		case SV_POTION_SPEED:
		{
			typ = GF_DO_SPEED;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			typ = GF_DO_HEAL;
			dam = damroll(2, 8);
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			typ = GF_DO_HEAL;
			dam = damroll(4, 8);
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			typ = GF_DO_HEAL;
			dam = damroll(8, 8);
			break;
		}

		case SV_POTION_HEALING:
		{
			typ = GF_DO_HEAL;
			dam = damroll(10, 30);
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			typ = GF_DO_HEAL;
			dam = damroll(10, 70);
			break;
		}

		case SV_POTION_LIFE:
		{
			typ = GF_DO_HEAL;
			dam = damroll(15, 120);
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			typ = GF_MANA;
			if (who < 0) dice = get_skill(skill, 5, 24);
			else         dice = 12;
			dam = damroll(dice, 12);
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			typ = GF_ENLIGHTENMENT;
			rad = 10;
			do_fire_star = TRUE;
			break;
		}
		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			typ = GF_ENLIGHTENMENT;
			rad = 18;
			do_fire_star = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			typ = GF_ENLIGHTENMENT;
			rad = 6;
			do_fire_star = TRUE;
			break;
		}

		case SV_POTION_GAIN_SKILL:
		{
			typ = GF_GAIN_LEVEL;
			dam = 30;
			break;
		}

		case SV_POTION_STAR_GAIN_SKILL:
		{
			typ = GF_GAIN_LEVEL;
			dam = 150;
			break;
		}
		default:
		{
			/* No effect, no identify */
			return (FALSE);
		}
	}

	/* Require a projection type */
	if (!typ) return (FALSE);

	/* Do not smash more potions */
	allow_activate = FALSE;

	/* Jump to target, affect everything */
	flg = PROJECT_BOOM | PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM |
	      PROJECT_KILL | PROJECT_PLAY;

	/* Optionally, fire a starburst */
	if (do_fire_star) flg |= (PROJECT_STAR);


	/* Hack -- potions are especially powerful when used in traps */
	if (who == -2) dam += dam / 3;

	/* Special case -- tone down accidental destruction of potions */
	if (!who) dam /= 3;


	/* Cast the projection, notice effects, reset "allow_activate" */
	notice = project((who < -1 ? -1 : who),
		rad, y, x, y, x, dam, typ, flg, 0, 0);


	/* If the character or his traps smashed the potion, learn about it */
	if ((who < 0) && (notice))
	{
		if (object_aware_p(o_ptr))
		{
			learn_details(o_ptr);
		}

		/* Throwing a potion is less effective than observing a trap */
		else if ((who == -2) || (one_in_(3)))
		{
			char o_name[DESC_LEN];

			object_aware(o_ptr);

			/* Describe the object */
			strip_name(o_name, o_ptr->k_idx);

			/* Message */
			msg_format("That was a Potion of %s that just exploded.", o_name);
		}
	}

	return (notice);
}


/*
 * Handle scrolls read by any other entity than the character.
 *
 * The effects of some scrolls only make sense when applied to the
 * character; they do nothing when not read by him.
 *
 * Note the "no_activate" variable, which prevents the projections of
 * activated scrolls activating other items.  XXX XXX
 *
 * Return 0 if the scroll should not be used up, 1 if we learnt
 * anything, -1 if we didn't.
 */
int scroll_read_effect(int who, int y, int x, object_type *o_ptr)
{
	monster_race *r_ptr;
	monster_type *m_ptr;

	bool notice = FALSE;

	int m_idx;
	int k, used_up, lev, pow, sav;
	int fy, fx;

	char m_name[DESC_LEN];

	int skill = get_skill(S_BURGLARY, 0, 100);


	/* Forbid scrolls from activating more scrolls or potions */
	if (!allow_activate) return (FALSE);

	/* Do not allow scrolls to activate more scrolls */
	allow_activate = FALSE;

	/* Require a scroll */
	if ((o_ptr->tval != TV_SCROLL) || (o_ptr->number < 1)) return (FALSE);

	/* Require valid coordinates */
	if (!in_bounds_fully(y, x)) return (FALSE);


	/* Get the index of the creature in this grid */
	m_idx = cave_m_idx[y][x];

	/* Require that a monster (not a character) be present */
	if (m_idx <= 0) return (FALSE);

	/* Get the monster in this grid */
	m_ptr = &m_list[m_idx];

	/* Get the monster race */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Get the monster coordinates */
	fy = m_ptr->fy;     fx = m_ptr->fx;

	/* Get a monster description */
	monster_desc(m_name, m_ptr, 0x40);


	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Assume the scroll will get used up */
	used_up = TRUE;


	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			notice = explosion(who, 3, fy, fx, 5, GF_DARK_WEAK);
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(0, FALSE, NULL);
			notice = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			if (summon_specific(fy, fx, FALSE, p_ptr->depth + 2, 0,
				randint(3)))
			{
				/* Assume "noticed"  XXX XXX */
				notice = TRUE;
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			if (summon_specific(fy, fx, FALSE, p_ptr->depth + 3,
				SUMMON_UNDEAD, randint(3)))
			{
				/* Assume "noticed"  XXX XXX */
				notice = TRUE;
			}
			break;
		}

		case SV_SCROLL_SUMMON_DEMONS:
		{
			notice = explosion(who, 3, fy, fx, 5, GF_DARK_WEAK);

			if (summon_specific(fy, fx, FALSE,
					MAX(p_ptr->depth, lev) + 3, SUMMON_HI_DEMON, 6))
			{
				notice = TRUE;
			}

			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			notice = trap_creation(fy, fx);
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			/* Check for resistance */
			pow = 10;
			if (r_ptr->flags3 & (RF3_RES_NEXUS)) pow = 4;
			if (r_ptr->flags3 & (RF3_RES_TPORT)) pow = 0;

			if (pow)
			{
				teleport_away(m_idx, pow, FALSE);
				if (player_can_see_bold(fy, fx)) notice = TRUE;
			}
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			/* Check for resistance */
			pow = 100;
			if (r_ptr->flags3 & (RF3_RES_NEXUS)) pow = 20;
			if (r_ptr->flags3 & (RF3_RES_TPORT)) pow = 0;

			if (pow)
			{
				teleport_away(m_idx, pow, FALSE);
				if (player_can_see_bold(fy, fx)) notice = TRUE;
			}
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			/* Check for resistance */
			sav = 40;
			if (r_ptr->flags3 & (RF3_RES_NEXUS)) sav = 80;
			if (r_ptr->flags3 & (RF3_RES_TPORT)) sav = 100;
			if (r_ptr->flags1 & (RF1_UNIQUE))    sav += 30;

			/* Attempt a saving throw */
			if ((sav < 100) && (sav < randint(100)))
			{
				/* Delete the monster entirely! */
				delete_monster_idx(m_idx);

				if (player_can_see_bold(fy, fx))
				{
					msg_format("%^s vanishes!", m_name);
					notice = TRUE;
				}
			}
			else if (sav >= 100)
			{
				if (player_can_see_bold(fy, fx))
				{
					msg_format("%^s refuses to budge!", m_name);
					notice = TRUE;
				}
			}
			else
			{
				if (player_can_see_bold(fy, fx))
				{
					msg_format("%^s resists the teleport-level spell!",
						m_name);
					notice = TRUE;
				}
			}

			break;
		}

		case SV_SCROLL_LIGHT:
		{
			notice = explosion(who, 3, fy, fx, 5, GF_LITE_WEAK);
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			map_area(fy, fx, FALSE);
			notice = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			notice = explosion(who, 0, fy, fx, 15 + 7 * skill / 10,
				GF_DO_CONF);
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			notice = explosion(who, 0, fy, fx, 75 + skill, GF_HOLY_ORB);
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			msg_print("You hear a terrible explosion!");
			destroy_area(fy, fx, 15, TRUE, FALSE);

			/* You have our attention. */
			notice = TRUE;

			break;
		}

		case SV_SCROLL_POISON_CLOUD:
		{
			/* Get a new effect index */
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_IRREGULAR_CLOUD;

			/* Of poison */
			x_list[k].type = GF_POIS;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = fy;
			x_list[k].x0 = x_list[k].x1 = fx;

			/* Practices the burglary (trap-setting) skill */
			x_list[k].practice_skill = S_BURGLARY;

			/* It attacks every 10 -> 5 game turns, */
			x_list[k].time_delay = 10 - skill / 20;

			/* Does damage, has a large radius, */
			x_list[k].power = rand_range(12, 16);
			x_list[k].power2 = 7;

			/* And lasts for about 10 attacks */
			x_list[k].lifespan = rand_range(7, 13);

			notice = TRUE;
			break;
		}

		case SV_SCROLL_NEXUS_STORM:
		{
			/* Fire a storm of nexus beams */
			fire_storm(-1, GF_NEXUS, fy, fx, 75, 13, 5, 1, FALSE);

			notice = TRUE;
			break;
		}

		case SV_SCROLL_GENOCIDE:
		{
			(void)genocide(r_ptr->d_char);

			notice = TRUE;
			break;
		}

		case SV_SCROLL_MASS_GENOCIDE:
		{
			msg_print("A long, shrill note sounds...");
			(void)mass_genocide(fy, fx);
			notice = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(fy, fx, 1, TRUE);
			if (player_can_see_bold(fy, fx)) notice = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(fy, fx, 3, TRUE);
			if (player_can_see_bold(fy, fx)) notice = TRUE;
			break;
		}

		case SV_SCROLL_MADNESS:
		{
			notice = judgement(p_ptr->depth);
			break;
		}

		case SV_SCROLL_NIGHTFALL:
		{
			nightfall();
			notice = TRUE;
			break;
		}

		case SV_SCROLL_TREES:
		{
			notice = project_star(who, rand_range(6, 9), fy, fx, fy, fx, 0,
				GF_MAKE_TREES, 0L);
			break;
		}

		case SV_SCROLL_WATER:
		{
			notice = project_star(who, rand_range(5, 8), fy, fx, fy, fx, 0,
				GF_MAKE_WATER, 0L);
			break;
		}

		case SV_SCROLL_LAVA:
		{
			notice = project_star(who, rand_range(5, 7), fy, fx, fy, fx, 0,
				GF_MAKE_LAVA, 0L);
			break;
		}

		default:
		{
			/* No effect */
			used_up = notice = FALSE;
			break;
		}
	}

	/* Allow scrolls to activate */
	allow_activate = TRUE;

	/* Hack -- allow certain scrolls to be "preserved" */
	if (!used_up) return (0);

	/* If the character or his traps activated the scroll, learn about it */
	if ((who < 0) && (notice))
	{
		if (object_aware_p(o_ptr))
		{
			learn_details(o_ptr);
		}
		else
		{
			char o_name[DESC_LEN];

			object_aware(o_ptr);

			/* Describe the object */
			strip_name(o_name, o_ptr->k_idx);

			/* Message */
			msg_format("That was a Scroll of %s that just activated.", o_name);
		}
	}

	/* Return "noticed" */
	return (notice ? 1 : -1);
}



/*
 * Devices can be used by traps as well as characters.  They may also be
 * used by monsters at some point.
 *
 * The explosions of devices can activate more devices, causing endless
 * loops.  We therefore use a special "allow_activate" flag.  XXX XXX
 *
 * When called from a monster trap, "power" may be as great as 136.
 */
bool device_use_effect(int mode, int power, int y, int x, object_type *o_ptr)
{
	monster_race *r_ptr;
	monster_type *m_ptr;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	bool notice = FALSE;

	int lev, dam, pow, m_idx;

	int who = mode;
	if (who < -1) who = -1;


	/* Forbid devices from activating more devices */
	if (!allow_activate) return (FALSE);

	/* Do not allow devices to activate more devices */
	allow_activate = FALSE;


	/* Require valid coordinates */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Get the index of the creature in this grid (if any) */
	m_idx = cave_m_idx[y][x];

	/* Require that a monster (not a character) be present */
	if (m_idx <= 0) return (FALSE);

	/* Get the monster in this grid */
	m_ptr = &m_list[m_idx];

	/* Get the monster race */
	r_ptr = &r_info[m_ptr->r_idx];


	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Monsters do not always use objects successfully */


	/* Using a staff */
	if (o_ptr->tval == TV_STAFF)
	{
		/* Nothing happens without charges */
		if (!o_ptr->pval) return (FALSE);

		/* Analyze the staff */
		switch (o_ptr->sval)
		{
			case SV_STAFF_DARKNESS:
			{
				notice = explosion(who, 3, y, x, 5, GF_DARK_WEAK);
				break;
			}

			case SV_STAFF_SLOWNESS:
			{
				dam = 5 + power / 4;
				notice = project_los(y, x, dam, GF_DO_SLOW);
				break;
			}

			case SV_STAFF_HASTE_MONSTERS:
			{
				notice = project_los(y, x, 0, GF_DO_SPEED);
				break;
			}

			case SV_STAFF_SUMMONING:
			{
				if (summon_specific(y, x, FALSE, p_ptr->depth + 3, 0, 4))
						notice = TRUE;
				break;
			}

			case SV_STAFF_MAPPING:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Use extended mapping */
				map_area(y, x, TRUE);
				break;
			}

			case SV_STAFF_DETECT_GOLD:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect gold and treasure */
				if (detect_treasure(TRUE)) notice = TRUE;
				if (detect_objects_gold(TRUE)) notice = TRUE;
				break;
			}

			case SV_STAFF_DETECT_ITEM:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect objects */
				if (detect_objects_normal(TRUE)) notice = TRUE;
				break;
			}

			case SV_STAFF_DETECT_TRAP:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect traps */
				if (detect_traps(TRUE, FALSE)) notice = TRUE;
				break;
			}

			case SV_STAFF_DETECT_DOOR:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect doors and stairs */
				if (detect_doors(TRUE)) notice = TRUE;
				if (detect_stairs(TRUE)) notice = TRUE;
				break;
			}

			case SV_STAFF_DETECT_INVIS:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect invisible monsters */
				if (detect_monsters_invis(TRUE, FALSE)) notice = TRUE;
				break;
			}

			case SV_STAFF_DETECT_EVIL:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect evil */
				if (detect_evil(TRUE, FALSE)) notice = TRUE;
				break;
			}

			case SV_STAFF_DETECTION:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect many things */
				if (detect_all(FALSE, FALSE)) notice = TRUE;
				break;
			}

			case SV_STAFF_TELEPORTATION:
			{
				pow = 100;
				if (r_ptr->flags3 & (RF3_RES_NEXUS)) pow = 20;
				if (r_ptr->flags3 & (RF3_RES_TPORT)) pow = 0;

				if (player_can_see_bold(y, x)) notice = TRUE;

				if (pow) teleport_away(m_idx, pow, FALSE);
				break;
			}

			case SV_STAFF_STARLIGHT:
			{
				dam = 20 + power / 4;
				pow = rand_range(15, 25);

				/* Starlight */
				notice = beam_burst(y, x, GF_LITE, pow, dam);
				break;
			}

			case SV_STAFF_LIGHT:
			{
				notice = explosion(who, 3, y, x, 0, GF_LITE_WEAK);
				break;
			}

			case SV_STAFF_BANISHMENT:
			{
				p_ptr->proj_mon_flags = (RF3_EVIL);
				notice = project_los(y, x, 40, GF_AWAY);
				break;
			}

			case SV_STAFF_CURE_MEDIUM:
			{
				notice = explosion(who, 0, y, x, 40 + power / 3, GF_DO_HEAL);
				break;
			}

			case SV_STAFF_HEALING:
			{
				notice = explosion(who, 0, y, x, 300, GF_DO_HEAL);
				break;
			}

			case SV_STAFF_SLEEP_MONSTERS:
			{
				pow = 25 + power / 2;

				/* Maximum-diameter cloud of sleeping */
				notice = project_los(y, x, pow, GF_DO_SLEEP);
				break;
			}

			case SV_STAFF_SLOW_MONSTERS:
			{
				pow = 25 + power / 2;

				/* Maximum-diameter cloud of slowing */
				notice = project_los(y, x, pow, GF_DO_SLOW);
				break;
			}

			case SV_STAFF_SPEED:
			{
				notice = explosion(who, 3, y, x, 0, GF_DO_SPEED);
				break;
			}

			case SV_STAFF_PROBING:
			{
				(void)lore_do_probe(m_idx);
				if (player_can_see_bold(y, x)) notice = TRUE;
				break;
			}

			case SV_STAFF_DISPEL_EVIL:
			{
				pow = 40 + power / 3;

				p_ptr->proj_mon_flags = (RF3_EVIL);
				notice = project_los(y, x, pow, GF_DISPEL);
				break;
			}

			case SV_STAFF_POWER:
			{
				pow = 50 + power / 2;

				notice = project_los(y, x, pow, GF_DISPEL);
				break;
			}

			case SV_STAFF_HOLINESS:
			{
				pow = 75 + power / 2;

				p_ptr->proj_mon_flags = (RF3_EVIL);
				notice = project_los(y, x, pow, GF_DISPEL);
				break;
			}

			case SV_STAFF_EARTHQUAKES:
			{
				msg_print("The dungeon shakes!");
				earthquake(y, x, 10);
				notice = TRUE;
				break;
			}

			case SV_STAFF_DESTRUCTION:
			{
				msg_print("You hear a terrible explosion!");
				destroy_area(y, x, 15, TRUE, FALSE);
				break;
			}

			case SV_STAFF_DOOMSPELLS:
			{
				pow = 100 + power;

				notice = project_ball(who, 4, y, x, y, x, pow,
					GF_MANA, PROJECT_JUMP, 100);
				break;
			}

			case SV_STAFF_CHAOS:
			{
				pow = 90 + power;

				notice = project_ball(who, 4, y, x, y, x, pow,
					GF_CHAOS, PROJECT_JUMP, 100);
				break;
			}

			default:
			{
				break;
			}
		}

		/* Use up a charge */
		o_ptr->pval--;
	}


	/* Using a wand */
	else if (o_ptr->tval == TV_WAND)
	{
		/* Usually, the sval is simply the object sval */
		int sval = o_ptr->sval;

		/* XXX Hack -- Wand of wonder can do anything before it */
		if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

		/* XXX Hack -- Wand of Wizardry can do anything before it */
		if (sval == SV_WAND_WIZARDRY)
		{
			sval = rand_int(SV_WAND_WIZARDRY);
			while (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WIZARDRY);

			/* Increase power by 50% */
			power += power / 2;
		}

		/* Nothing happens without charges */
		if (!o_ptr->pval) return (FALSE);

		/* Analyze the wand */
		switch (sval)
		{
			case SV_WAND_HEAL_MONSTER:
			{
				notice = explosion(who, 0, y, x, 20 + power, GF_DO_HEAL);
				break;
			}

			case SV_WAND_HASTE_MONSTER:
			{
				notice = explosion(who, 0, y, x, 0, GF_DO_SPEED);
				break;
			}

			case SV_WAND_CLONE_MONSTER:
			{
				notice = explosion(who, 0, y, x, 0, GF_DO_CLONE);
				break;
			}

			case SV_WAND_TELEPORT_AWAY:
			{
				notice = explosion(who, 0, y, x, 0, GF_AWAY);
				break;
			}

			case SV_WAND_DISARMING:
			{
				disarm_trap(5);
				notice = TRUE;
				break;
			}

			case SV_WAND_FORCE_DOOR:
			{
				/* Hack -- use a ball instead of searching for doors */
				notice = explosion(who, 2, y, x, 0, GF_FORCE_DOOR);
				break;
			}

			case SV_WAND_STONE_TO_MUD:
			{
				/* Hack -- use a ball because it's more interesting */
				notice = explosion(who, 2, y, x, 20, GF_KILL_WALL);
				break;
			}

			case SV_WAND_LITE:
			{
				/* Stronger, because it only affects one monster */
				notice = explosion(who, 0, y, x, damroll(6, 7), GF_LITE_WEAK);
				break;
			}

			case SV_WAND_SLEEP_MONSTER:
			{
				notice = explosion(who, 0, y, x, 25 + 2 * power / 3, GF_DO_SLEEP);
				break;
			}

			case SV_WAND_SLOW_MONSTER:
			{
				notice = explosion(who, 0, y, x, 25 + 2 * power / 3, GF_DO_SLOW);
				break;
			}

			case SV_WAND_CONFUSE_MONSTER:
			{
				notice = explosion(who, 0, y, x, 25 + 2 * power / 3, GF_DO_CONF);
				break;
			}

			case SV_WAND_FEAR_MONSTER:
			{
				notice = explosion(who, 0, y, x, 25 + 2 * power / 3, GF_DO_FEAR);
				break;
			}

			case SV_WAND_DRAIN_LIFE:
			{
				notice = explosion(who, 0, y, x, 40 + 3 * power / 5, GF_DEATH);
				break;
			}

			case SV_WAND_POLYMORPH:
			{
				notice = explosion(who, 0, y, x, 0, GF_DO_POLY);
				break;
			}

			case SV_WAND_STINKING_CLOUD:
			{
				notice = explosion(who, 2, y, x, 10, GF_POIS);
				break;
			}

			case SV_WAND_MAGIC_MISSILE:
			{
				notice = explosion(who, 0, y, x, damroll(2, 5), GF_MANA);
				break;
			}

			case SV_WAND_ACID_BOLT:
			{
				dam = damroll(5 + power / 12, 8);
				notice = explosion(who, 0, y, x, dam, GF_ACID);
				break;
			}

			case SV_WAND_ELEC_BOLT:
			{
				dam = damroll(3 + power / 20, 8);
				notice = explosion(who, 0, y, x, dam, GF_ELEC);
				break;
			}

			case SV_WAND_FIRE_BOLT:
			{
				dam = damroll(6 + power / 10, 8);
				notice = explosion(who, 0, y, x, dam, GF_FIRE);
				break;
			}

			case SV_WAND_COLD_BOLT:
			{
				dam = damroll(4 + power / 16, 8);
				notice = explosion(who, 0, y, x, dam, GF_COLD);
				break;
			}

			case SV_WAND_ACID_BALL:
			{
				dam = 60 + 3 * power / 10;
				notice = explosion(who, 2, y, x, dam, GF_ACID);
				break;
			}

			case SV_WAND_ELEC_BALL:
			{
				dam = 40 + 3 * power / 10;
				notice = explosion(who, 2, y, x, dam, GF_ELEC);
				break;
			}

			case SV_WAND_FIRE_BALL:
			{
				dam = 70 + 3 * power / 10;
				notice = explosion(who, 2, y, x, dam, GF_FIRE);
				break;
			}

			case SV_WAND_COLD_BALL:
			{
				dam = 50 + 3 * power / 10;
				notice = explosion(who, 2, y, x, dam, GF_COLD);
				break;
			}

			case SV_WAND_WONDER:
			{
				msg_print("Oops.  Wand of wonder set off.");
				break;
			}

			case SV_WAND_ANNIHILATION:
			{
				dam = 100 + power;
				notice = explosion(who, 0, y, x, dam, GF_DEATH);
				break;
			}

			case SV_WAND_DRAGON_FIRE:
			{
				dam = 90 + power;
				notice = explosion(who, 3, y, x, dam, GF_FIRE);
				break;
			}

			case SV_WAND_DRAGON_COLD:
			{
				dam = 90 + power;
				notice = explosion(who, 3, y, x, dam, GF_COLD);
				break;
			}

			case SV_WAND_DRAGON_BREATH:
			{
				dam = 130 + power;
				notice = explosion(who, 3, y, x, dam, GF_ACID + rand_int(5));
				break;
			}

			case SV_WAND_DOOM_BOLT:
			{
				dam = 80 + power;
				notice = explosion(who, 0, y, x, dam, GF_MANA);
				break;
			}

			case SV_WAND_WIZARDRY:
			{
				msg_print("Oops.  Wand of wizardry set off.");
				break;
			}

			case SV_WAND_SPARK:
			{
				notice = explosion(who, 0, y, x, damroll(2, 4), GF_ELEC);
				break;
			}

			default:
			{
				break;
			}
		}

		/* Use up a charge */
		o_ptr->pval--;
	}


	/* Using a rod */
	else if (o_ptr->tval == TV_ROD)
	{
		/* Nothing happens if the rods are discharged */
		if (o_ptr->timeout > o_ptr->pval - k_ptr->pval) return (FALSE);

		/* Analyze the rod */
		switch (o_ptr->sval)
		{
			case SV_ROD_DETECT_TRAP:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect traps */
				if (detect_traps(FALSE, FALSE)) notice = TRUE;
				break;
			}

			case SV_ROD_DETECT_DOOR:
			{
				/* Set detection center to this grid */
				detect_y = y;
				detect_x = x;

				/* Detect doors and stairs */
				if (detect_doors(FALSE)) notice = TRUE;
				if (detect_stairs(FALSE)) notice = TRUE;
				break;
			}

			case SV_ROD_MAPPING:
			{
				/* Use short-range mapping */
				map_area(y, x, FALSE);
				break;
			}

			case SV_ROD_ILLUMINATION:
			{
				notice = explosion(who, 3, y, x, 0, GF_LITE_WEAK);
				break;
			}

			case SV_ROD_PROBING:
			{
				(void)lore_do_probe(m_idx);
				if (player_can_see_bold(y, x)) notice = TRUE;
				break;
			}

			case SV_ROD_HEALING:
			{
				notice = explosion(0, 0, y, x, 1000, GF_DO_HEAL);
				break;
			}

			case SV_ROD_SPEED:
			{
				notice = explosion(who, 0, y, x, 0, GF_DO_SPEED);
				break;
			}

			case SV_ROD_TELEPORT_AWAY:
			{
				notice = explosion(who, 0, y, x, 50, GF_AWAY);
				break;
			}

			case SV_ROD_DISARMING:
			{
				(void)disarm_trap(5);
				notice = TRUE;
				break;
			}

			case SV_ROD_LITE:
			{
				notice = explosion(who, 0, y, x, damroll(6, 9), GF_LITE_WEAK);
				break;
			}

			case SV_ROD_BLINKING:
			{
				pow = 10;
				if (r_ptr->flags3 & (RF3_RES_NEXUS)) pow = 4;
				if (r_ptr->flags3 & (RF3_RES_TPORT)) pow = 0;

				if (pow)
				{
					teleport_away(m_idx, pow, FALSE);
					if (player_can_see_bold(y, x)) notice = TRUE;
				}

				break;
			}

			case SV_ROD_SUMMON_HITHER:
			{
				/* (Try to) Teleport the player to the monster */
				teleport_player_to(y, x, 1, TRUE, 2);
				notice = TRUE;
				break;
			}

			case SV_ROD_DRAIN_LIFE:
			{
				dam = 50 + 2 * power / 3;
				notice = explosion(who, 0, y, x, dam, GF_DEATH);
				break;
			}

			case SV_ROD_POLYMORPH:
			{
				notice = explosion(who, 0, y, x, 0, GF_DO_POLY);
				break;
			}

			case SV_ROD_ACID_BOLT:
			{
				dam = damroll(5 + power / 12, 8);
				notice = explosion(who, 0, y, x, dam, GF_ACID);
				break;
			}

			case SV_ROD_ELEC_BOLT:
			{
				dam = damroll(3 + power / 20, 8);
				notice = explosion(who, 0, y, x, dam, GF_ELEC);
				break;
			}

			case SV_ROD_FIRE_BOLT:
			{
				dam = damroll(6 + power / 10, 8);
				notice = explosion(who, 0, y, x, dam, GF_FIRE);
				break;
			}

			case SV_ROD_COLD_BOLT:
			{
				dam = damroll(4 + power / 16, 8);
				notice = explosion(who, 0, y, x, dam, GF_COLD);
				break;
			}

			case SV_ROD_ACID_BALL:
			{
				dam = 60 + 7 * power / 20;
				notice = explosion(who, 1, y, x, dam, GF_ACID);
				break;
			}

			case SV_ROD_ELEC_BALL:
			{
				dam = 40 + 7 * power / 20;
				notice = explosion(who, 1, y, x, dam, GF_ELEC);
				break;
			}

			case SV_ROD_FIRE_BALL:
			{
				dam = 70 + 7 * power / 20;
				notice = explosion(who, 1, y, x, dam, GF_FIRE);
				break;
			}

			case SV_ROD_COLD_BALL:
			{
				dam = 50 + 7 * power / 20;
				notice = explosion(who, 1, y, x, dam, GF_COLD);
				break;
			}

			case SV_ROD_LIGHTNINGSTRIKE:
			{
				dam = damroll(1 + power / 3, 8);
				notice = explosion(who, 0, y, x, dam, GF_ELEC);
				break;
			}

			case SV_ROD_NORTHWINDS:
			{
				dam = damroll(4 + power / 3, 8);
				notice = explosion(who, 0, y, x, dam, GF_COLD);
				break;
			}

			case SV_ROD_DRAGONFIRE:
			{
				dam = damroll(7 + power / 3, 8);
				notice = explosion(who, 0, y, x, dam, GF_FIRE);
				break;
			}

			case SV_ROD_GLAURUNGS:
			{
				dam = damroll(10 + power / 3, 8);
				notice = explosion(who, 0, y, x, dam, GF_ACID);
				break;
			}

			default:
			{
				break;
			}
		}

		/* Drain the rod(s) */
		o_ptr->timeout += k_ptr->pval;
	}

	/* If a monster trap used the device, learn about it */
	if ((mode == -2) && (notice))
	{
		char o_name[DESC_LEN];

		if (object_aware_p(o_ptr))
		{
			learn_details(o_ptr);
		}
		else
		{
			cptr tv_desc = "Staff";
			if (o_ptr->tval == TV_WAND) tv_desc = "Wand";
			if (o_ptr->tval == TV_ROD) tv_desc = "Rod";

			/* Learn about the object */
			object_aware(o_ptr);

			/* Describe the object */
			strip_name(o_name, o_ptr->k_idx);

			/* Message */
			msg_format("The trap appears to be using a %s of %s.",
				tv_desc, o_name);
		}
	}

	/* Return "anything noticed" */
	return (notice);
}


/*
 * Fire off magical devices in random directions.
 *
 * Note that this function deliberately cuts out a lot of the learning code;
 * it's hard to pay attention when everything's going off boom.
 */
void fire_off_devices(int chance)
{
	object_type *o_ptr;
	object_kind *k_ptr;

	bool ident;
	bool used;

	int dir = 5;
	int i, j;
	int y, x;
	int num_fired = 0;

	/* Declare target variables */
	TARGET_DECLARE

	/* Store old target coordinates */
	TARGET_PRESERVE


	/* Scan the entire inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		/* Obtain the item in this slot */
		o_ptr = &inventory[i];

		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Affect staffs, wands, and rods */
		if ((o_ptr->tval == TV_STAFF) ||
		    (o_ptr->tval == TV_WAND)  ||
		    (o_ptr->tval == TV_ROD))
		{
			/* Check each device in the stack */
			for (j = 0; j < o_ptr->number; j++)
			{
				ident = FALSE;
				used  = FALSE;

				/* Skip this magical device */
				if (chance < randint(100)) continue;

				/* Ignore empty staffs and wands */
				if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
				{
					if (!o_ptr->pval) continue;
				}

				/* Ignore drained rods */
				if (o_ptr->tval == TV_ROD)
				{
					if (o_ptr->number == 1)
					{
						if (o_ptr->timeout) continue;
					}
					else
					{
						if (o_ptr->timeout > o_ptr->pval - k_ptr->pval) continue;
					}
				}

				/* Target in a random direction */
				get_grid_using_angle(rand_int(240), p_ptr->py, p_ptr->px, &y, &x);
				target_set_location(y, x);

				/* Use device (randomly) */
				(void)do_device(OBJECT_USE, o_ptr, &ident, &used, TRUE);

				/* No charge was used */
				if (!used) continue;


				/* Be nice:  allow basic ID */
				if (ident && !object_aware_p(o_ptr)) object_aware(o_ptr);

				/* Note that item was fired */
				num_fired++;


				/* Item is a staff or wand */
				if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
				{
					/* Use a single charge */
					o_ptr->pval--;
				}

				/* Item is one or more rods with enough energy */
				else if (o_ptr->tval == TV_ROD)
				{
					/* Increase the timeout by the rod kind's pval. */
					o_ptr->timeout += k_ptr->pval;
				}
			}
		}
	}

	/* Restore target coordinates */
	TARGET_RESTORE
}

/*
 * Check for the presence of a magical blanket in inventory.
 *
 * If the right kind of blanket is found, damage it.  Damage value is
 * inventory destruction chance (usually 0-40) for elemental attacks,
 * and 1 for disenchantment, draining, and cursing.
 */
bool check_blanket(int mode, int dam)
{
	int i;
	int ind = -1;


	/* Scan the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip everything but blankets */
		if ((o_ptr->tval != TV_JUNK) || (o_ptr->sval != SV_BLANKET))
			continue;

		/* Skip cursed blankets */
		if (cursed_p(o_ptr)) continue;

		/* Check for the protection we need */
		if ((mode == CHECK_BLANKET_ACID) &&
		    (o_ptr->flags2 & (TR2_RES_ACID))) ind = i;
		else if ((mode == CHECK_BLANKET_ELEC) &&
		    (o_ptr->flags2 & (TR2_RES_ELEC))) ind = i;
		else if ((mode == CHECK_BLANKET_FIRE) &&
		    (o_ptr->flags2 & (TR2_RES_FIRE))) ind = i;
		else if ((mode == CHECK_BLANKET_COLD) &&
		    (o_ptr->flags2 & (TR2_RES_COLD))) ind = i;
		else if ((mode == CHECK_BLANKET_DISEN) &&
		    (o_ptr->flags2 & (TR2_RES_DISEN))) ind = i;
		else if ((mode == CHECK_BLANKET_DRAIN) &&
		    (o_ptr->flags2 & (TR2_RES_DRAIN))) ind = i;
		else if ((mode == CHECK_BLANKET_CURSE) &&
		    (o_ptr->flags3 & (TR3_BLESSED))) ind = i;
	}

	/* We have protection */
	if (ind >= 0)
	{
		/* Get the blanket */
		object_type *o_ptr = &inventory[ind];

		/* Describe the blanket */
		char o_name[DESC_LEN];
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Remember previous protective value */
		i = o_ptr->ac;

		/* Damage the blanket (never by more then 50) */
		o_ptr->ac -= MIN(50, dam);

		/* Blanket has no resistance left */
		if (o_ptr->ac <= 0)
		{
			message_format(MSG_DESTROY, 0, "Your %s is destroyed!", o_name);

			/* Destroy the blanket */
			inven_item_increase(ind, -1);
			inven_item_describe(ind);
			inven_item_optimize(ind);
		}

		/* Blanket is almost ruined */
		else if ((o_ptr->ac <= o_ptr->pval / 4) &&
		         (i > o_ptr->pval / 4))
		{
			msg_format("Your %s is almost destroyed.", o_name);
		}

		/* Blanket has significant damage */
		else if ((o_ptr->ac <= o_ptr->pval / 2) &&
		         (i > o_ptr->pval / 2))
		{
			msg_format("Your %s is damaged.", o_name);
		}

		/* Blanket protects */
		return (TRUE);
	}

	/* No protection available */
	return (FALSE);
}



/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool hates_acid(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable items */
		case TV_ARROW:
		case TV_BOLT:
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls are wood/paper */
		case TV_STAFF:
		case TV_SCROLL:
		case TV_PARCHMENT:
		{
			return (TRUE);
		}

		/* Ouch */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Junk is useless */
		case TV_SKELETON:
		{
			return (TRUE);
		}
		case TV_JUNK:
		{
			if (o_ptr->sval != SV_BOULDER) return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
bool hates_elec(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_RING:
		case TV_AMULET:
		case TV_WAND:
		case TV_ROD:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
bool hates_fire(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable */
		case TV_LITE:
		case TV_ARROW:
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}

		/* Books */
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_NATURE_BOOK:
		case TV_DARK_BOOK:
		{
			return (TRUE);
		}

		/* Chests */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls burn */
		case TV_STAFF:
		case TV_SCROLL:
		case TV_PARCHMENT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate cold?
 */
bool hates_cold(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_POTION:
		case TV_FLASK:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Melt something
 */
int set_acid_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (!hates_acid(o_ptr)) return (FALSE);
	if (f2 & (TR2_IGNORE_ACID)) return (FALSE);

	return (TRUE);
}

/*
 * Electrical damage
 */
int set_elec_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (!hates_elec(o_ptr)) return (FALSE);
	if (f2 & (TR2_IGNORE_ELEC)) return (FALSE);

	return (TRUE);
}


/*
 * Burn something
 */
int set_fire_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (!hates_fire(o_ptr)) return (FALSE);
	if (f2 & (TR2_IGNORE_FIRE)) return (FALSE);

	return (TRUE);
}


/*
 * Freeze things
 */
int set_cold_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (!hates_cold(o_ptr)) return (FALSE);
	if (f2 & (TR2_IGNORE_COLD)) return (FALSE);

	return (TRUE);
}


/*
 * Scans the inventory, looking for items which can be destroyed.
 *
 * Returns number of items destroyed.
 */
int inven_damage(inven_func typ, int perc0, int dam)
{
	int i, j, k, amt;

	char o_name[DESC_LEN];

	/* Have a special message if you get hit hard */
	bool severe = (perc0 >= 10);

	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];
		int perc = perc0;

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Give this item slot a shot at death */
		if ((*typ)(o_ptr))
		{
			/* Scrolls and potions are more vulnerable. */
			if ((o_ptr->tval == TV_SCROLL) || (o_ptr->tval == TV_POTION))
			{
				perc = 3 * perc / 2;

				/* Some potions and scrolls are tough. */
				if (o_ptr->ac > 0) perc -= (perc * o_ptr->ac / 20);
			}

			/* Many object types are hard to destroy */
			if (((is_any_weapon(o_ptr)) || (is_any_armor(o_ptr)) ||
			     (is_magical_device(o_ptr)) || (o_ptr->tval == TV_RING) ||
			     (o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_CHEST)) &&
			    (dam < rand_range(10, 20)))
			{
				perc = 0;
			}
			else if ((is_magic_book(o_ptr)) && (dam < rand_range(5, 10)))
			{
				perc = 0;
			}

			else if (o_ptr->tval == TV_ROD) perc /= 3;

			/* Count the casualties */
			for (amt = j = 0; j < o_ptr->number; ++j)
			{
				if (rand_int(1000) < perc) amt++;
			}

			/* Some casualties */
			if (amt)
			{
				/* Special message (similar to those of Moria) */
				if (severe)
				{
					/* Only once */
					severe = FALSE;

					/* Acid */
					if (typ == set_acid_destroy)
						msg_print("There is an acrid smell coming from your pack!");

					/* Lightning */
					if (typ == set_elec_destroy)
						msg_print("Sparks fly from your pack!");

					/* Fire */
					if (typ == set_fire_destroy)
						msg_print("Smoke billows out of your pack!");

					/* Frost */
					if (typ == set_cold_destroy)
						msg_print("Your pack freezes over!");
				}

				/* Some magical devices can be damaged instead of destroyed. */
				if (((o_ptr->tval == TV_STAFF) ||
					  (o_ptr->tval == TV_WAND)) &&
					  (o_ptr->ac > 0))
				{
					/* Damage item(s) */
					o_ptr->ac -= rand_range(perc, perc * 2);

					/* Item(s) survive */
					if (o_ptr->ac > 0)
					{
						/* Describe */
						object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

						/* Note damage */
						msg_format("Your %s (%c) %s damaged!",
							o_name, index_to_label(i),
							((o_ptr->number > 1) ? "were" : "was"));

						/* No deaths */
						continue;
					}
				}


				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

				/* Message */
				message_format(MSG_DESTROY, 0, "%sour %s (%c) %s destroyed!",
				      ((o_ptr->number > 1) ?
				      ((amt == o_ptr->number) ? "All of y" :
				       (amt > 1 ? "Some of y" : "One of y")) : "Y"),
				       o_name, index_to_label(i),
				      ((amt > 1) ? "were" : "was"));


				/* Reduce the charges of rods/wands */
				reduce_charges(o_ptr, amt);

				/* Potions smash open (for safety, allow only one effect) XXX */
				if (o_ptr->tval == TV_POTION)
				{
					/* Do not learn anything  XXX XXX */
					(void)potion_smash_effect(0, p_ptr->py, p_ptr->px, o_ptr);
				}

				/* Destroy "amt" items */
				inven_item_increase(i, -amt);
				inven_item_optimize(i);

				/* Count the casualties */
				k += amt;
			}
		}
	}

	/* Return the casualty count */
	return (k);
}


/*
 * Curse the player's armor
 */
bool curse_armor(void)
{
	int i, item;
	object_type *o_ptr;

	char o_name[DESC_LEN];

	/* Look several times for a non-artifact armor */
	for (i = 0; i < 100; i++)
	{
		/* Choose a slot at random */
		if (i < 40)
		{
			item = rand_range(INVEN_BODY, INVEN_FEET);
		}
		else
		{
			item = rand_range(INVEN_PACK, INVEN_TOTAL - 1);
		}

		/* Get object in this slot */
		o_ptr = &inventory[item];

		/* Nothing to curse */
		if (!o_ptr->k_idx) continue;

		/* Must be some sort of armor */
		if (!is_any_armor(o_ptr)) continue;

		/* Must not be an artifact */
		if (artifact_p(o_ptr)) continue;

		/* Describe */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

		/* Never allow a saving throw (because artifacts are safe). */

		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->artifact_index = 0;
		o_ptr->ego_item_index = EGO_BLASTED;
		o_ptr->to_a = 0 - damroll(3, 5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);

		/* Note curse */
		o_ptr->ident |= (IDENT_CURSED);

		/* No value */
		o_ptr->b_cost = 0L;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

		break;
	}

	return (i != 100);
}


/*
 * Curse the player's weapon
 */
bool curse_weapon(void)
{
	int i, item;
	object_type *o_ptr;

	char o_name[DESC_LEN];

	/* Look several times for a non-artifact weapon */
	for (i = 0; i < 100; i++)
	{
		/* Choose a slot at random */
		if (one_in_(3))
		{
			item = INVEN_WIELD;
		}
		else if (one_in_(6))
		{
			item = INVEN_ARM;
		}
		else if (one_in_(6))
		{
			item = INVEN_BOW;
		}
		else
		{
			item = rand_range(INVEN_PACK, INVEN_TOTAL - 1);
		}

		/* Get object in this slot */
		o_ptr = &inventory[item];

		/* Nothing to curse */
		if (!o_ptr->k_idx) continue;

		/* Must be some sort of melee or missile weapon */
		if (!is_any_weapon(o_ptr)) continue;

		/* Must not be an artifact */
		if (artifact_p(o_ptr)) continue;

		/* Describe */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

		/* Never allow a saving throw (because artifacts are safe). */

		/* Describe */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->artifact_index = 0;
		o_ptr->ego_item_index = EGO_SHATTERED;
		o_ptr->to_h = 0 - damroll(2, 5);
		o_ptr->to_d = 0 - damroll(2, 5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);

		/* Note curse */
		o_ptr->ident |= (IDENT_CURSED);

		/* No value */
		o_ptr->b_cost = 0L;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

		break;
	}

	/* Notice */
	return (i != 100);
}

/*
 * Add curse flags to equipped items.
 *
 * Taken from Zangband.
 */
void curse_equipment(int power)
{
	bool changed = FALSE;

	u32b f1, f2, f3;

	/* Pick one (non-quiver) equipment slot at random */
	object_type *o_ptr =
		&inventory[rand_range(INVEN_WIELD, INVEN_SUBTOTAL - 1)];


	/* Check for protective blanket */
	if (check_blanket(CHECK_BLANKET_CURSE, 1)) return;

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Empty slot -- nothing gets cursed */
	if (!o_ptr->k_idx) return;


	/* Extra, biased saving throw for blessed items */
	if ((f3 & (TR3_BLESSED)) && (randint(888) > power))
	{
		char o_name[DESC_LEN];
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
		msg_format("Your %s resists cursing!", o_name);
		return;
	}

	/* Try for a heavy curse (rare) */
	if (rand_range(50, 300) <= power)
	{
		if (!(o_ptr->flags3 & (TR3_HEAVY_CURSE)))
		{
			changed = TRUE;
		}
		o_ptr->flags3 |= (TR3_HEAVY_CURSE);
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);
		o_ptr->ident  |= (IDENT_CURSED);
	}

	/* Just a light curse */
	else
	{
		if (!cursed_p(o_ptr))
		{
			changed = TRUE;
		}
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);
		o_ptr->ident |=  (IDENT_CURSED);
	}

	/* Note new curses (special marking for "uncurse_object()" */
	if (changed)
	{
		msg_print("There is a malignant black aura surrounding you...");
		o_ptr->inscrip = INSCRIP_CURSED2;
	}
}

/*
 * Hack -- Removes curse from an object.
 */
void uncurse_object(object_type *o_ptr)
{
	/* Uncurse it */
	o_ptr->flags3 &= ~(TR3_LIGHT_CURSE);
	o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);

	/* Note removal of curse */
	o_ptr->ident &= ~(IDENT_CURSED);

	/* Explicitly mark all normal "cursed" items as "uncursed" */
	if (o_ptr->inscrip != INSCRIP_CURSED2)
	{
		o_ptr->inscrip = INSCRIP_UNCURSED;
	}

	/* Quietly remove special "cursed" indicator (gotten through spells) */
	else
	{
		o_ptr->inscrip = 0;
	}

	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);
}


/*
 * Removes curses from items in inventory (both obvious and non-obvious).
 *
 * Note that Items which are "Perma-Cursed" (The One Ring, The Crown of
 * Morgoth) can NEVER be uncursed.
 *
 * Note that if "heavy" is FALSE, then items which are heavily cursed
 * (Mormegil, Calris, Weapons of Morgul, etc.) will not be uncursed.
 */
static int remove_curse_aux(int heavy)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Object isn't cursed -- ignore */
		if (!cursed_p(o_ptr)) continue;

		/* Heavily cursed items need a special spell */
		if (!heavy && (o_ptr->flags3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-cursed items can NEVER be uncursed */
		if (o_ptr->flags3 & (TR3_PERMA_CURSE)) continue;

		/* Uncurse the object */
		uncurse_object(o_ptr);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove light curses
 */
int remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove light and heavy curses
 */
int remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
}



/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "ammunition"
 */
static bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "armor"
 */
static bool item_tester_hook_armor(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify objects with sensable magic
 */
static bool item_tester_sense_magic(const object_type *o_ptr)
{
	/* No need to sense */
	if (object_known_p(o_ptr)) return (FALSE);

	/* Can sense aware wands, staffs, rings, and amulets */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF) ||
	    (o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET))
	{
		if (object_aware_p(o_ptr)) return (TRUE);
	}

	/* Can sense any weapon or armor */
	if (is_wargear(o_ptr)) return (TRUE);

	return (FALSE);
}

/*
 * Hook to specify "unknown" objects
 */
static bool item_tester_unknown(const object_type *o_ptr)
{
	if (object_known_p(o_ptr)) return (FALSE);

	/* Cannot ID mushrooms */
	if ((o_ptr->tval == TV_FOOD) && (o_ptr->sval < SV_FOOD_MIN_FOOD))
		return (FALSE);

	else return (TRUE);
}

/*
 * Hook to specify objects not fully ID-ed
 */
static bool item_tester_unknown_star(const object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL) return (FALSE);

	/* Cannot ID mushrooms */
	if ((o_ptr->tval == TV_FOOD) && (o_ptr->sval < SV_FOOD_MIN_FOOD))
		return (FALSE);

	else return (TRUE);
}

/*
 * Used by the "enchant" function (chance of failure out of 1000)
 */
static const int enchant_table[16] =
{
	0, 10, 50, 100, 200,
	250, 300, 400, 650, 950,
	990, 992, 995, 997, 999,
	1000
};

/*
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 above
 * the base enchantment if you try very hard.  Going from +9 to +10 only
 * works about 5% of the time, and from +10 to +11 only about 1% of the
 * time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob;

	bool succeed = FALSE;
	bool a = artifact_p(o_ptr);

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if (is_missile(o_ptr))
	{
		prob /= 20;
	}

	/* Some weapons come in bunches */
	else if (is_melee_weapon(o_ptr))
	{
		if ((!artifact_p(o_ptr)) &&
		    (k_ptr->gen_dice * k_ptr->gen_side > 1))
		{
			prob *= 2;
			prob /= (k_ptr->gen_dice * (k_ptr->gen_side + 1));
		}
	}


	/* Try "n" times */
	for (i = 0; i < n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if ((prob > 100) && (rand_int(prob) >= 100)) continue;

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			/* Enchantment is determined in relation to normal value */
			int tmp = o_ptr->to_h - k_ptr->to_h;

			if      (tmp <  0) chance = 0;
			else if (tmp > 15) chance = 1000;
			else               chance = enchant_table[tmp];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (one_in_(2))))
			{
				succeed = TRUE;

				/* Enchant */
				o_ptr->to_h++;

				/* Enchanting to positive values can break curses -CFT */
				if (cursed_p(o_ptr) &&
					(!(o_ptr->flags3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_h >= 0) && (one_in_(4)))
				{
					/* Uncurse the object */
					msg_print("The curse is broken!");
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			/* Enchantment is determined in relation to normal value */
			int tmp = o_ptr->to_d - k_ptr->to_d;

			if      (tmp <  0) chance = 0;
			else if (tmp > 15) chance = 1000;
			else               chance = enchant_table[tmp];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (one_in_(2))))
			{
				succeed = TRUE;

				/* Enchant */
				o_ptr->to_d++;

				/* Enchanting to positive values can break curses -CFT */
				if (cursed_p(o_ptr) &&
					(!(o_ptr->flags3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_d >= 0) && (one_in_(4)))
				{
					/* Uncurse the object */
					msg_print("The curse is broken!");
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			/* Enchantment is determined in relation to normal value */
			int tmp = o_ptr->to_a - MAX(0, k_ptr->to_a);  /* Ignore penalties */

			if      (tmp <  0) chance = 0;
			else if (tmp > 15) chance = 1000;
			else               chance = enchant_table[tmp];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (one_in_(2))))
			{
				succeed = TRUE;

				/* Enchant */
				o_ptr->to_a++;

				/* Enchanting to positive values can break curses -CFT */
				if (cursed_p(o_ptr) &&
					(!(o_ptr->flags3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_a >= 0) && (one_in_(4)))
				{
					/* Uncurse the object */
					msg_print("The curse is broken!");
					uncurse_object(o_ptr);
				}
			}
		}
	}

	/* Failure */
	if (!succeed) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Success */
	return (TRUE);
}


/*
 * Turn the object into an ego-item.
 *
 * Note that this function can be used on "piles" of items, and the larger
 * the pile, the lower the chance of success.
 *
 * We save old hitdice and plusses.  This can be a tad abusable.  XXX
 */
static bool turn_into_ego_item(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	int prob;

	/* Save some variables (sense inscriptions are lost) */
	byte old_iy = o_ptr->iy;
	byte old_ix = o_ptr->ix;
	s16b old_next_o_idx = o_ptr->next_o_idx;
	s16b old_held_m_idx = o_ptr->held_m_idx;
	byte old_number = o_ptr->number;
	u16b old_note = o_ptr->note;
	u16b old_ident = o_ptr->ident & (IDENT_KNOWN | IDENT_MENTAL | IDENT_WORN);

	/* Save the old hitdice and plusses XXX */
	byte old_dd = o_ptr->dd;
	byte old_ds = o_ptr->ds;
	s16b old_to_h = o_ptr->to_h;
	s16b old_to_d = o_ptr->to_d;


	/* Some objects can't be turned into ego-items */
	if (artifact_p(o_ptr) || ego_item_p(o_ptr) || cursed_p(o_ptr))
	{
		return (FALSE);
	}

	/* DSM can't be turned into an ego-item */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		return (FALSE);
	}


	/* Large piles resist enchantment */
	prob = old_number * 100;

	/* Missiles are easy to enchant */
	if (is_missile(o_ptr))
	{
		prob /= 20;
	}

	/* Some other item come in stacks, and are also easy to enchant */
	else if (k_ptr->gen_dice * k_ptr->gen_side > 1)
	{
		prob /= k_ptr->gen_dice * (k_ptr->gen_side + 1) / 2;
	}

	/* Roll for pile resistance */
	if ((prob > 100) && (rand_int(prob) >= 100)) return (FALSE);


	/* High level objects are harder to enchant */
	if ((k_ptr->level + 20) > rand_int(150)) return (FALSE);


	/* Clear object */
	object_prep(o_ptr, o_ptr->k_idx);

	/*
	 * Turn the object into an ego-item (not an artifact).  Using "max_depth"
	 * for "level" is weird, but it works.
	 */
	apply_magic(o_ptr, p_ptr->max_depth, FALSE, TRUE, TRUE);

	/* Restore some things */
	o_ptr->iy = old_iy;
	o_ptr->ix = old_ix;
	o_ptr->next_o_idx = old_next_o_idx;
	o_ptr->held_m_idx = old_held_m_idx;
	o_ptr->number = old_number;
	o_ptr->note = old_note;
	o_ptr->marked = TRUE;
	o_ptr->ident = old_ident;

	/* Use the best hitdice and plusses  XXX */
	if (o_ptr->dd < old_dd) o_ptr->dd = old_dd;
	if (o_ptr->ds < old_ds) o_ptr->ds = old_ds;
	if (o_ptr->to_h < old_to_h) o_ptr->to_h = old_to_h;
	if (o_ptr->to_d < old_to_d) o_ptr->to_d = old_to_d;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Return */
	return (TRUE);
}


/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armor, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac, bool ego)
{
	int item;
	bool okay = FALSE;

	object_type *o_ptr;

	char o_name[DESC_LEN];

	cptr q, s;

	char effect_desc[DESC_LEN];


	/* Enchant armor or weapon as requested */
	if (num_ac) item_tester_hook = item_tester_hook_armor;
	else        item_tester_hook = item_tester_hook_weapon;

	/* Get an item */
	q = "Enchant which item?";
	s = "You have nothing to enchant.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return (FALSE);
	item_to_object(o_ptr, item);


	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Note power of enchantment */
	if (num_hit + num_dam + num_ac == 1) strcpy(effect_desc, "faintly");
	else strcpy(effect_desc, "brightly");

	/* Try to turn into an ego-item */
	if (ego)
	{
		if (turn_into_ego_item(o_ptr))
		{
			/* Special description */
			strcpy(effect_desc, "brilliantly");

			/* No further enchantment */
			num_hit = num_dam = num_ac = 0;

			okay = TRUE;
		}
	}

	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac,  ENCH_TOAC))  okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Aw shucks */
		strcat(effect_desc, ", but fade");
		if (o_ptr->number == 1) strcat(effect_desc, "s");
	}

	/* Describe */
	msg_format("%s %s glow%s %s%c",
		((item >= 0) ? "Your" : "The"), o_name,
		((o_ptr->number > 1) ? "" : "s"), effect_desc, okay ? '!' : '.');



	/* Something happened */
	return (TRUE);
}



/*
 * Enchant some missiles and give them an elemental brand
 *
 * Combines the old brand_bolts and brand_missiles routines.
 *
 * ammo_type is the tval of the relevant ammunition.
 * If set to 0, any ammunition is enchantable.
 *
 * brand_type is the ego index.
 * If set to 0, a non-poison brand is picked randomly.
 */
bool brand_missile(int ammo_type, int brand_type)
{
	int item, choice;
	object_type *o_ptr;
	cptr q, s;


	/* Hack -- check for restricted choice */
	if ((ammo_type >= TV_SHOT) && (ammo_type <= TV_BOLT))
		item_tester_tval = ammo_type;

	/* Otherwise any ammo will do */
	else item_tester_hook = item_tester_hook_ammo;

	/* Get an item */
	q = "Enchant which ammunition?";
	s = "You have no ammunition to brand.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
		return (FALSE);
	item_to_object(o_ptr, item);


	/*
	 * Don't enchant artifacts, ego-items, or cursed items
	 */
	if (artifact_p(o_ptr) || ego_item_p(o_ptr) || cursed_p(o_ptr))
	{
		/* Flush */
		if (flush_failure) flush();

		/* Fail */
		msg_print("The ammunition enchantment failed.");

		/* Notice */
		return (TRUE);
	}

	/* Type of brand may be restricted */
	if (brand_type) choice = brand_type;

	/* Otherwise choose randomly (not poison, though) */
	else
	{
		/* Fire and frost brands are twice as common as acid and lightning */
		choice = EGO_ACIDIC + rand_int(2);
		if (!one_in_(3)) choice += 2;
	}


	switch (choice)
	{
		case EGO_ACIDIC:
		{
			/* Print message and acid brand missiles. */
			msg_print("Your missiles sizzle with acid!");
			break;
		}

		case EGO_ELECT:
		{
			/* Print message and electric brand missiles. */
			msg_print("Your missiles are covered in sparks!");
			break;
		}
		case EGO_FLAME:
		{
			/* Print message and fire brand missiles. */
			msg_print("Your missiles burn with fire!");
			break;
		}

		case EGO_FROST:
		{
			/* Print message and frost brand missiles. */
			msg_print("Your missiles are covered in a frosty sheath!");
			break;
		}
		case EGO_POISON:
		{
			/* Print message and poison brand missiles. */
			msg_print("Your missiles drip with deadly poison!");
			break;
		}

		default:
		{
			/* Oops */
			return (FALSE);
		}
	}

	/* Brand */
	o_ptr->ego_item_index = choice;

	/* Enchant */
	enchant(o_ptr, rand_range(3, 7), ENCH_TOHIT | ENCH_TODAM);

	/* Prevent money-making. */
	o_ptr->cost_adjust = 20;

	/* Notice */
	return (TRUE);
}



/*
 * Sense an object.  "Pseudo-ID"
 *
 * Objects are (weakly) sensed on the ground when the character walks into
 * their grid.  They are (strongly) sensed when the character wears or
 * wields them.
 */
void sense_object(object_type *o_ptr, int slot, bool strong, bool force_heavy)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	char o_name[DESC_LEN];

	int chance;

	/* Equipment bonuses make a real difference, but never too much */
	int aware = MIN(p_ptr->skill_awr, 30);

	/* Use artifact or object kind level */
	int level = (o_ptr->artifact_index ?
		a_info[o_ptr->artifact_index].level : k_ptr->level);

	bool heavy = FALSE;
	bool full  = FALSE;
	bool suppress_msg = FALSE;

	int feel;
	int old_inscrip = o_ptr->inscrip;
	int skill;

	u32b f1, f2, f3;


	/* No sensing when confused or berserk */
	if ((p_ptr->confused) || (p_ptr->image) || (p_ptr->berserk)) return;

	/* Need to be able to look at the object */
	if ((p_ptr->blind) || (no_light() && (p_ptr->see_infra <= 0))) return;

	/* Object is already known */
	if (object_known_p(o_ptr)) return;

	/* Not using strong sensing */
	if (!strong)
	{
		/* It has already been sensed; do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) return;
	}

	/* The charges on aware wands and staffs can sometimes be sensed */
	if ((object_aware_p(o_ptr)) &&
	    ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)))
	{
		/* Require some perception and magic device skill */
		if ((get_skill(S_PERCEPTION, 0, 100) >= 15) &&
			 (get_skill(S_DEVICE, 0, 100) >= 10))
		{
			/* Determine change of learning about the item */
			chance = get_skill(S_PERCEPTION, 0, 100) +
			         get_skill(S_DEVICE, 0, 100) + aware -
			         level;

			/* No information gained */
			if (chance < randint(100))
			{
				/* Note uncertainty */
				o_ptr->inscrip = INSCRIP_UNCERTAIN;

				/* The object has been "sensed" */
				o_ptr->ident |= (IDENT_SENSE);
			}

			/* Learned something new */
			else
			{
				/* Identify it normally */
				object_aware(o_ptr);
				object_known(o_ptr);

				/* Message (only if in inventory) */
				if (slot >= 0)
				{
					/* Get an object description */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 2);

					/* Message */
					msg_format("You sense the charges on %s (%c).",
						o_name, index_to_label(slot));
				}
			}
		}

		/* Done */
		return;
	}


	/* Equipped unaware rings and amulets can sometimes be sensed */
	if ((slot >= INVEN_WIELD) && (!object_aware_p(o_ptr)) &&
	    ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)))
	{
		/* Chance to become aware of the item kind */
		chance = get_skill(S_PERCEPTION, 15, 75) + aware - level / 3;

		/* Try to become aware */
		if (chance >= randint(100))
		{
			cptr tmp = "";

			/* Become aware of this object */
			object_aware(o_ptr);

			/* Get an object description */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 2);

			/* Output a message */
			if (slot == INVEN_LEFT)  tmp = "on your left hand";
			if (slot == INVEN_RIGHT) tmp = "on your right hand";
			if (slot == INVEN_NECK)  tmp = "on your neck";

			message_format(MSG_PSEUDOID, 0, "You feel you are wearing %s %s.", o_name, tmp);

			/* Gain a significant amount of exp  (this is a hack) */
			gain_exp(level * level / 2, S_NOSKILL);
		}

		/* Done */
		return;
	}


	/* Otherwise, object must be a weapon or armor */
	if (!is_wargear(o_ptr)) return;

	/* For wargear, level is drop level */
	level = o_ptr->drop_depth;
	skill = get_skill(S_PERCEPTION, 0, 100);

	/* Get base chance.  Gains are rapid early on, less rapid later */
	chance = 10 + rsqrt(get_skill(S_PERCEPTION, 0, 4900)) + aware;

	/* Keeping perception up to date is very useful */
	if (skill > level) chance += 3 * rsqrt(skill - level);
	else               chance -= 2 * rsqrt(level - skill);

	if (chance < 0) chance = 0;

	/* Chance is improved if we're using strong sensing */
	if (strong) chance += 20;

	/* Get object flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Brands are hard to miss */
	if (f1 & TR1_BRAND_MASK) chance += 20;

	/* Object has a pval - increase sensing chance */
	if (get_object_pval(o_ptr, 0L)) chance += 20;

	/* Those who have taken the Oath of Iron are great at IDing wargear */
	if (p_ptr->oath & (OATH_OF_IRON)) chance = 4 * chance / 3 + 10;

	/* Those who have no magic realm are also fairly good */
	else if (!p_ptr->realm)	chance = 4 * chance / 3;

	/* Roll for "heavy" sensing */
	if (chance >= rand_range(60, 80) || randint(100) < chance) heavy = TRUE;

	/* Adjustments to chance that doesn't determine heaviness of sensing */
	/* Holy alliance helps reveal bless & curse  -clefs- */
	if (cursed_p(o_ptr) || (f3 & (TR3_BLESSED))) chance += rsqrt(get_skill(S_PIETY, 0, 900));

	/* Warriors always sense strongly */
	if (p_ptr->oath & OATH_OF_IRON)	strong = TRUE;

	/* Option to require heavy sensing */
	if ((force_heavy) && (!full)) heavy = TRUE;

	/* Can fully identify if strongly sensing with a heavy feeling */
	if (heavy && strong && randint(100) < chance) full = TRUE;

	/* Didn't get more information */
	if (chance < randint(100))
	{
		feel = INSCRIP_UNCERTAIN;
	}

	/* Learn about the item */
	else
	{
		/* Allow full identification */
		if (full)
		{
			/* Identify */
			object_aware(o_ptr);
			object_known(o_ptr);

			/* Get a full object description */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Describe */
			if (slot >= INVEN_WIELD)
			{
				msg_format("%^s: %s (%c).", describe_use(slot), o_name,
						index_to_label(slot));
			}
			else if (slot >= 0)
			{
				msg_format("In your pack: %s (%c).",
						o_name, index_to_label(slot));
			}
			else
			{
				msg_format("On the ground: %s.", o_name);
			}

			/* All done */
			return;
		}

		/* Indestructible objects are either special or terrible */
		if (old_inscrip == INSCRIP_INDESTRUCTIBLE) heavy = TRUE;

		/* We already have the clearest possible inscription */
		if ((old_inscrip == INSCRIP_TERRIBLE) ||
		    (old_inscrip == INSCRIP_SPECIAL) ||
		    (old_inscrip == INSCRIP_WORTHLESS) ||
		    (old_inscrip == INSCRIP_EXCELLENT))
		{
			return;
		}

		/* We have a clear priestly sense of the object */
		if ((!heavy) &&
			 ((old_inscrip == INSCRIP_VERY_BLESSED) ||
			  (old_inscrip == INSCRIP_VERY_CURSED)))
		{
			return;
		}


		/* Artifacts */
		if (artifact_p(o_ptr))
		{
			/* Cursed/Valueless */
			if (cursed_p(o_ptr) || broken_p(o_ptr))
			{
				if (heavy) feel = INSCRIP_TERRIBLE;
				else       feel = INSCRIP_CURSED;
			}

			/* Normal */
			else
			{
				if (heavy) feel = INSCRIP_SPECIAL;
				else       feel = INSCRIP_GOOD;
			}
		}

		/* Ego-Items */
		else if (ego_item_p(o_ptr))
		{
			/* Cursed/Valueless */
			if (cursed_p(o_ptr) || broken_p(o_ptr))
			{
				if (heavy) feel = INSCRIP_WORTHLESS;
				else       feel = INSCRIP_CURSED;
			}

			/* Normal */
			else
			{
				if (heavy) feel = INSCRIP_EXCELLENT;
				else       feel = INSCRIP_GOOD;
			}
		}

		/* Cursed items */
		else if (cursed_p(o_ptr)) feel = INSCRIP_CURSED;

		/* Valueless items */
		else if (broken_p(o_ptr)) feel = INSCRIP_BROKEN;

		/* Bonus to AC */
		else if (o_ptr->to_a > k_ptr->to_a) feel = INSCRIP_GOOD;

		/* Bonus to sum of Skill and Deadliness */
		else if (o_ptr->to_h + o_ptr->to_d > k_ptr->to_h + k_ptr->to_d)
			feel = INSCRIP_GOOD;

		/* Penalty to AC */
		else if (o_ptr->to_a < k_ptr->to_a) feel = INSCRIP_BROKEN;

		/* Penalty to sum of Skill and Deadliness */
		else if (o_ptr->to_h + o_ptr->to_d < k_ptr->to_h + k_ptr->to_d)
			feel = INSCRIP_BROKEN;


		/* Default to "average" */
		else
		{
			if (heavy) feel = INSCRIP_AVERAGE;
			else       feel = INSCRIP_UNCERTAIN;
		}

		/* Suppress messages if very skilled  -SKY- */
		if (skill >= 90) suppress_msg = TRUE;

		/* We got a definite feeling, and are not suppressing messages */
		if ((feel != INSCRIP_UNCERTAIN) && (feel != old_inscrip) &&
		    (!suppress_msg))
		{
			/* Get a short object description */
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

			/* Message (equipment) */
			if (slot >= INVEN_WIELD)
			{
				message_format(MSG_PSEUDOID, 0, "You feel the %s (%c) you are %s %s%s %s...",
					o_name, index_to_label(slot), describe_use(slot),
					((o_ptr->number == 1) ? "is"        : "are"),
					((old_inscrip)        ? " actually" : ""),
					inscrip_text[feel]);
			}

			/* Message (inventory) */
			else if (slot >= 0)
			{
				message_format(MSG_PSEUDOID, 0, "You feel the %s (%c) in your pack %s%s %s...",
					o_name, index_to_label(slot),
					((o_ptr->number == 1) ? "is"        : "are"),
					((old_inscrip)        ? " actually" : ""),
					inscrip_text[feel]);
			}

			/* Message (used) */
			else
			{
				cptr use_desc = "nearby";
				if (slot == -1) use_desc = "you are firing";
				if (slot == -2) use_desc = "you are throwing";
				if (slot == -3) use_desc = "in your pack";
				if (slot == -4) use_desc = "on the floor";

				/* Message -- if not on floor */
				if (slot != -4)
				{
					message_format(MSG_PSEUDOID, 0, "You feel the %s %s %s%s %s...",
						o_name, use_desc,
						((o_ptr->number == 1) ? "is"            : "are"),
						((old_inscrip)        ? " actually"     : ""),
						inscrip_text[feel]);
				}
			}
		}
	}

	/* Sense the object (but don't overwrite with "unknown") */
	if (!(o_ptr->ident & (IDENT_SENSE)) || (feel != INSCRIP_UNCERTAIN))
	{
		o_ptr->inscrip = feel;
	}

	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);

	/* Combine / Reorder the pack (later) */
	if (slot >= 0) p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}




/*
 * Priests have always had a problem with ID compared to other spellcasters,
 * but there are some object qualities which they ought to be the best at
 * discovering.  -LM-
 */
bool scan_object_priest(bool full)
{
	int item;
	bool intense = FALSE;
	int feel = 0;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[DESC_LEN];

	cptr q, s;


	/* Only unidentified items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Concentrate on which item?";
	s = "You have no objects that you can learn more about.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return (FALSE);
	item_to_object(o_ptr, item);


	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];


	/* It is a ring or amulet */
	if ((full) && ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)))
	{
		/* Identify it normally */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Object is an artifact */
	else if (artifact_p(o_ptr))
	{
		/* Cursed/Valueless */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
		{
			feel = INSCRIP_TERRIBLE;
		}

		/* Normal */
		else
		{
			feel = INSCRIP_SPECIAL;
		}
	}

	/* All other items */
	else
	{
		/* If we know enough about the object already, we get better feelings */
		if ((o_ptr->inscrip == INSCRIP_EXCELLENT) ||
		    (o_ptr->inscrip == INSCRIP_WORTHLESS))
		{
			full = TRUE;
		}

		/* We have access to full sensing */
		if (full)
		{
			/* Object is an ego-item */
			if (ego_item_p(o_ptr)) intense = TRUE;

			/* Object has pval-dependant qualities (with real value) */
			else if ((get_object_pval(o_ptr, 0L)) &&
			         (object_value_real(o_ptr) >
			          object_value(o_ptr) + 500L))
			{
				intense = TRUE;
			}

			/* Object has (unusual) hidden qualities */
			else if ((o_ptr->flags1 & ~(k_ptr->flags1)) ||
			         (o_ptr->flags2 & ~(k_ptr->flags2)) ||
			         ((o_ptr->flags3 & ~(k_ptr->flags3)) &&
			          (((o_ptr->flags3 & ~(k_ptr->flags3)) !=
			             (TR3_LIGHT_CURSE)))))
			{
				intense = TRUE;
			}

			/* Object has a very high actual (identified) value */
			else if (object_value_real(o_ptr) > object_value(o_ptr) +
			         rand_range(2000L, 4000L))
			{
				intense = TRUE;
			}
		}

		/* Actual curses always override all other feelings */
		if (cursed_p(o_ptr))
		{
			if ((o_ptr->flags3 & (TR3_HEAVY_CURSE)) || (intense))
			     feel = INSCRIP_VERY_CURSED;
			else feel = INSCRIP_CURSED;
		}

		/* Object is "blessed" when its actual value exceeds base value */
		else if (object_value_real(o_ptr) > object_value(o_ptr))
		{
			if (intense) feel = INSCRIP_VERY_BLESSED;
			else         feel = INSCRIP_BLESSED;
		}

		/* Object is "cursed" when its actual value is less than base value */
		else if (object_value_real(o_ptr) < object_value(o_ptr))
		{
			/* Hack -- low-value scrolls and potions are not "cursed" */
			if (((o_ptr->tval == TV_SCROLL) ||
			     (o_ptr->tval == TV_POTION)) &&
			    (object_value_real(o_ptr) > 0))
			{
				feel = 0;
			}
			else feel = INSCRIP_CURSED;
		}

		/* No good reason to call the object "cursed" or "blessed" */
		else
		{
			feel = INSCRIP_UNCERTAIN_SENSED;
		}
	}

	/* We got a feeling */
	if (feel)
	{
		/* Inscribe the object (usually) */
		if ((feel != INSCRIP_UNCERTAIN_SENSED) ||
		    (o_ptr->inscrip == INSCRIP_UNCERTAIN))
		{
			o_ptr->inscrip = feel;
		}

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
	}


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);


	/* We identified the object */
	if (object_known_p(o_ptr))
	{
		/* Describe */
		if (item >= INVEN_WIELD)
		{
			msg_format("%^s: %s (%c).", describe_use(item), o_name,
				index_to_label(item));
		}
		else if (item >= 0)
		{
			msg_format("In your pack: %s (%c).",
				o_name, index_to_label(item));
		}
		else
		{
			msg_format("On the ground: %s.",
				o_name);
		}
	}


	/* We got a definite feeling */
	else if ((feel) && (feel != INSCRIP_UNCERTAIN) &&
	         (feel != INSCRIP_UNCERTAIN_SENSED))
	{
		char feeling_text[DESC_LEN];

		/* Usually use the standard feeling inscription */
		strcpy(feeling_text, inscrip_text[feel]);

		/* Use special text where appropriate */
		if (feel == INSCRIP_VERY_CURSED) strcpy(feeling_text, "foully cursed");
		if (feel == INSCRIP_VERY_BLESSED) strcpy(feeling_text, "truly blessed");

		/* Get an object description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Message (equipment) */
		if (item >= INVEN_WIELD)
		{
			message_format(MSG_PSEUDOID, 0, "You feel the %s (%c) you are %s %s %s...",
				o_name, index_to_label(item), describe_use(item),
				((o_ptr->number == 1) ? "is" : "are"), feeling_text);
		}

		/* Message (inventory) */
		else if (item >= 0)
		{
			message_format(MSG_PSEUDOID, 0, "You feel the %s (%c) in your pack %s %s...",
				o_name, index_to_label(item),
				((o_ptr->number == 1) ? "is" : "are"),
				feeling_text);
		}
		else
		{
			message_format(MSG_PSEUDOID, 0, "You feel the %s on the ground %s %s...",
				o_name, ((o_ptr->number == 1) ? "is" : "are"),
				feeling_text);
		}
	}

	/* We didn't learn anything */
	else
	{
		msg_print("You were not able to sense anything special about this object.");
		if (!o_ptr->inscrip) o_ptr->inscrip = INSCRIP_UNCERTAIN;
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Learn more about un-identified wearables as they are used.
 */
void learn_about_wearable(object_type *o_ptr, int slot, bool strong)
{
	/* New sensing is usually not forced heavy */
	bool heavy = strong;

	/* Not an object */
	if (!o_ptr->k_idx) return;

	/* Already known */
	if (object_known_p(o_ptr)) return;

	/* Sense the object (strongly) */
	sense_object(o_ptr, slot, TRUE, heavy);
}


/*
 * Learn about charges on magical devices, pvals on rings and amulets,
 * and scan weapons and armor.
 */
bool sense_magic(void)
{
	int item, feel;

	object_type *o_ptr;

	char o_name[DESC_LEN];

	cptr q, s;


	/* Only magic-sensable items */
	item_tester_hook = item_tester_sense_magic;

	/* Get an item */
	q = "Sense magic on which item?";
	s = "You have nothing to sense.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return (FALSE);
	item_to_object(o_ptr, item);


	/* Object is a weapon or armor -- sense it */
	if (is_wargear(o_ptr))
	{
		/* Get object kind */
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Assume "average" */
		feel = INSCRIP_AVERAGE;

		/* Cursed items */
		if (cursed_p(o_ptr)) feel = INSCRIP_CURSED;

		/* Valueless items */
		else if (broken_p(o_ptr)) feel = INSCRIP_BROKEN;

		/* Bonuses or interesting qualities */
		else if ((artifact_p(o_ptr)) || (ego_item_p(o_ptr)) ||
		         (o_ptr->to_h + o_ptr->to_d > k_ptr->to_h + k_ptr->to_d) ||
		         (o_ptr->to_a > k_ptr->to_a))
		{
			feel = INSCRIP_GOOD;
		}

		/* Penalties */
		else if ((o_ptr->to_h + o_ptr->to_d < k_ptr->to_h + k_ptr->to_d) ||
		         (o_ptr->to_a < k_ptr->to_a))
		{
			feel = INSCRIP_BROKEN;
		}


		/* Sense the object (or re-sense it, if we have no information yet) */
		if (!(o_ptr->ident & (IDENT_SENSE)) ||
		     (o_ptr->inscrip == INSCRIP_UNCERTAIN))
		{
			o_ptr->inscrip = feel;

			/* The object has been "sensed" */
			o_ptr->ident |= (IDENT_SENSE);
		}
	}

	/* Object is a ring or amulet */
	else if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET))
	{
		/* If the object is aware, learn about it */
		if (object_aware_p(o_ptr)) object_known(o_ptr);
	}

	/* Object is a wand or staff */
	else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		/* If the object is aware, learn about it */
		if (object_aware_p(o_ptr)) object_known(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).", describe_use(item), o_name,
			index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
			o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
			o_name);
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Identify an object.
 *
 * Return TRUE if something was identified.
 */
bool ident_spell(void)
{
	int item;

	object_type *o_ptr;

	char o_name[DESC_LEN];

	cptr q, s;

	/* Only un-id'ed items */
	if (!item_tester_hook) item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Identify which item?";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return (FALSE);
	item_to_object(o_ptr, item);


	/* Identify it normally */
	object_aware(o_ptr);
	object_known(o_ptr);


	/* Possibly play a sound depending on object quality. */
	if (object_value(o_ptr) == 0)
	{
		/* This is a cursed item. */
		if (cursed_p(o_ptr)) sound(MSG_IDENT_BAD);
	}
	else if (o_ptr->artifact_index)
	{
		/* We have a good artifact. */
		sound(MSG_IDENT_ART);
	}
	else if (o_ptr->ego_item_index)
	{
		/* We have a good ego item. */
		sound(MSG_IDENT_EGO);
	}


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).", describe_use(item), o_name,
			index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
			o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
			o_name);
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Fully identify an object.
 *
 * Return TRUE if something was identified.
 */
bool identify_fully(void)
{
	int item;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[DESC_LEN];

	cptr q, s;

	/* Only un-*id*'ed items */
	item_tester_hook = item_tester_unknown_star;

	/* Get an item */
	q = "Fully identify which item?";
	s = "You have nothing to fully identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return (FALSE);
	item_to_object(o_ptr, item);


	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);
	object_mental(o_ptr);

	/* Become fully aware of the effects  XXX XXX (think about this some more) */
	k_ptr->special |= (SPECIAL_KNOWN_EFFECT);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
			describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).", o_name,
			index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.", o_name);
	}

	/* Describe it fully */
	do_cmd_observe(o_ptr, FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Identify everything being carried or worn.
 * Done by a potion of *Enlightenment*.
 */
void identify_pack(void)
{
	int i;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}


/*
 * Hook for "get_item()".  Determine if something is rechargeable with magic.
 *
 * Note that rods cannot be recharged with magic.
 */
static bool item_tester_hook_recharge1(const object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Nope */
	return (FALSE);
}

/*
 * Hook for "get_item()".  Determine if something is rechargeable with essences.
 */
static bool item_tester_hook_recharge2(const object_type *o_ptr)
{
	/* Don't have enough essences to recharge this item */
	if (!enough_essences(o_ptr)) return (FALSE);

	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Recharge rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}

/*
 * Hook for "get_item()".  Determine if something is drainable.
 */
static bool item_tester_hook_recharge3(const object_type *o_ptr)
{
	/* Drain staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Drain wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Drain rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}

/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 * This function has been rewritten in Oangband.  -LM-
 *
 * Scroll of recharging --> recharge(130)
 * Scroll of *recharging* --> recharge(200)
 * Thingol --> recharge(170)
 * Amulet of recharging (90 - 140)
 *
 * It is harder to recharge high level and highly charged wands,
 * staffs, and rods.  The more wands in a stack, the more easily and
 * strongly they recharge.  Staffs, however, each get fewer charges if
 * stacked.
 *
 * Rods are very hard to recharge -- this is deliberate!
 *
 * XXX XXX XXX Beware of "sliding index errors".
 */
bool recharge(int power, bool essence)
{
	int item, lev, chance;
	int recharge_strength, recharge_amount;

	object_type *o_ptr;
	object_kind *k_ptr;

	bool fail = FALSE;
	byte fail_type = 1;

	cptr q, s;
	char o_name[DESC_LEN];

	/* Get magical device skill */
	int skill = get_skill(S_DEVICE, 0, 100);


	/* Only accept legal items */
	if (!essence)
	{
		/* Wands and staffs */
		item_tester_hook = item_tester_hook_recharge1;
	}
	else
	{
		/* Wands, staffs, and rods */
		item_tester_hook = item_tester_hook_recharge2;
	}

	/* Get an item */
	q = "Recharge which item?";
	if (!essence) s = "You have nothing to recharge.";
	else s = "You either have nothing to recharge or don't have enough essences.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);
	item_to_object(o_ptr, item);


	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Extract the object "level" */
	lev = k_ptr->level;


	/* We're using essences to recharge something */
	if (essence)
	{
		/* Use up the essences, return on failure */
		if (!use_up_essences(o_ptr)) return (FALSE);
	}


	/* Recharge a rod (only with essences) */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = ((power > lev) ? (power - lev) : 0) / 5;

		/* Back-fire */
		if ((recharge_strength < 0) || (one_in_(recharge_strength)))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* Recharge */
		else
		{
			/* Recharge one (and only one) rod */
			o_ptr->timeout -= k_ptr->pval;
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;
		}
	}

	/* Recharge wand/staff */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		/*
		 * Extract a recharge strength by comparing object level to power.
		 * Divide up a stack of wands' charges to calculate charge penalty.
		 */
		if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
		{
			recharge_strength = (power - lev -
				(8 * o_ptr->pval / o_ptr->number)) / 10;
		}

		/* All staffs, unstacked wands. */
		else
		{
			recharge_strength = (power - lev - (8 * o_ptr->pval)) / 10;
		}

		/* Back-fire */
		if ((recharge_strength < 0) || (one_in_(recharge_strength)))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* If the spell didn't backfire, recharge the wand or staff. */
		else
		{
			/* Recharge based on the standard number of charges. */
			recharge_amount = randint(k_ptr->pval);

			/* Multiple wands in a stack increase recharging somewhat. */
			if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			{
				recharge_amount +=
					randint((k_ptr->pval + 2) * (o_ptr->number - 1) / 4);
			}

			/*
			 * But each staff in a stack gets fewer additional charges,
			 * although always at least one.
			 */
			if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
			{
				recharge_amount /= o_ptr->number;
			}

			/* Recharge the wand or staff (by at least 1). */
			o_ptr->pval += MAX(1, recharge_amount);

			/* Hack - Artifacts have a maximum # of charges. */
			if (artifact_p(o_ptr) && (o_ptr->pval > k_ptr->pval))
			{
				o_ptr->pval = k_ptr->pval;
			}

			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}
	}

	/* Inflict the penalties for failing a recharge. */
	if (fail)
	{
		/* Failure */
		if (flush_failure) flush();

		/* Artifacts are never destroyed. */
		if (artifact_p(o_ptr))
		{
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
			msg_format("The recharging backfires - %s is completely drained!", o_name);

			/* Artifact rods. */
			if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout < 10000))
				o_ptr->timeout = (o_ptr->timeout + 100) * 2;

			/* Artifact wands and staffs. */
			if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
				o_ptr->pval = 0;
		}

		/* Staffs of doomspells have a mind of their own */
		else if ((o_ptr->tval == TV_STAFF) &&
		    (o_ptr->sval == SV_STAFF_DOOMSPELLS) && (one_in_(6)))
		{
			msg_print("The staff escapes from your control!");
			doomspells(one_in_(2), MAX(50, p_ptr->depth));
		}

		/* Normal object */
		else
		{
			/* Get the object description */
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

			/*** Determine Seriousness of Failure ***/

			/* Recharging with essences shifts the risk to the character */
			if (essence)
			{
				/* Oops */
				msg_print("The recharging backfires!  The magic goes out of control!");

				/* Explode */
				essence_wild_magic(o_ptr, lev);

				/* Drain */
				fail_type = 1;
			}

			/* Magical recharging */
			else
			{
				/* Wands */
				if (o_ptr->tval == TV_WAND)
				{
					/* Drain, blow up a wand, or blow up all wands */
					if           (skill > rand_int(100)) fail_type = 1;
					else if (40 + skill > rand_int(100)) fail_type = 2;
					else                                 fail_type = 3;
				}

				/* Staffs -- more resilient */
				else if (o_ptr->tval == TV_STAFF)
				{
					/* Drain, or blow up one staff */
					if (40 + skill > rand_int(100))      fail_type = 1;
					else                                 fail_type = 2;
				}

				/* Default is draining */
				else
				{
					fail_type = 1;
				}
			}


			/*** Apply draining and destruction. ***/

			/* Drain object or stack of objects. */
			if (fail_type == 1)
			{
				if (o_ptr->tval == TV_ROD)
				{
					if (!essence)
					{
						msg_print("The recharge backfires, draining the rod further!");
					}
					if (o_ptr->timeout < 10000)
						o_ptr->timeout = (o_ptr->timeout + 100) * 2;
				}
				else
				{
					if (!essence)
					{
						msg_format("You save your %s from destruction, but all charges are lost.", o_name);
					}
					o_ptr->pval = 0;
				}
			}

			/* Destroy an object or one in a stack of objects. */
			if (fail_type == 2)
			{
				if (o_ptr->number > 1)
					msg_format("Wild magic consumes one of your %s!", o_name);
				else
					msg_format("Wild magic consumes your %s!", o_name);

				/* We also drain any other items in the stack */
				if (o_ptr->tval != TV_ROD) o_ptr->pval = 0;

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
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}

			/* Destroy all members of a stack of objects. */
			if (fail_type == 3)
			{
				if (o_ptr->number > 1)
					msg_format("Wild magic consumes all your %s!", o_name);
				else
					msg_format("Wild magic consumes your %s!", o_name);


				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -999);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -999);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
		}
	}


	/* Successful recharge -- chance to automatically sense charges */
	else if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) &&
	         (object_aware_p(o_ptr)))
	{
		/* Use perception or device skill (device skill is worth more) */
		skill = MAX(get_skill(S_PERCEPTION, 0, 140), get_skill(S_DEVICE, 0, 180));
		chance = skill - lev;

		/* No information gained */
		if (chance < randint(100))
		{
			/* Note uncertainty */
			o_ptr->inscrip = INSCRIP_UNCERTAIN;

			/* The object has been "sensed" */
			o_ptr->ident |= (IDENT_SENSE);
		}

		/* Learned something new */
		else
		{
			/* Note if plural */
			bool plural = (o_ptr->number > 1);

			/* Get the object name */
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

			/* Identify it */
			object_known(o_ptr);

			/* Message */
			msg_format("You sense %s %s %s %d charges%s.",
				(item >= 0 ? "your" : "the"),
				o_name, (plural ? "have" : "has"), o_ptr->pval,
				((o_ptr->tval == TV_STAFF && plural) ? " each" : ""));
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}


/*
 * Mages can get mana from magical objects at need. -LM-
 */
bool tap_magical_energy(void)
{
	int item, lev;
	int energy = 0;

	object_type *o_ptr;

	cptr q, s;
	cptr item_name = "";


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge3;

	/* Get an item */
	q = "Drain charges from which item?";
	s = "You have nothing to drain charges from.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);
	item_to_object(o_ptr, item);


	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;

	/* Extract the object's energy and get its generic name. */
	if (o_ptr->tval == TV_ROD)
	{
		/* Rods have little usable energy, for obvious balance reasons... */
		energy = (5 + lev) * o_ptr->number;

		/* No tapping rods with instant recharge */
		if (!(o_ptr->pval))
		{
			energy = 0;
		}
		else
		{
			/* Modify Based on charged-ness */
			energy = (energy * (o_ptr->pval - o_ptr->timeout)) / o_ptr->pval;
		}

		item_name = "rod";
	}

	/* Staffs don't stack, so they get a 50% bonus */
	if (o_ptr->tval == TV_STAFF)
	{
		energy = (5 + lev) * (3 * o_ptr->pval / 2);
		item_name = "staff";
	}

	/* Wands are nice sources of energy */
	if (o_ptr->tval == TV_WAND)
	{
		energy = (5 + lev) * o_ptr->pval;
		item_name = "wand";
	}

	/* Turn energy into mana. */

	/* Require a reasonable amount of energy */
	if (energy < 30)
	{
		/* Notify of failure. */
		msg_format("That %s had no useable energy.", item_name);
	}
	else
	{
		/* If mana below maximum, increase mana and drain object. */
		if (p_ptr->csp < p_ptr->msp)
		{
			/* Drain the object. */
			if (o_ptr->tval == TV_ROD) o_ptr->timeout = o_ptr->pval;
			else o_ptr->pval = 0;


			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);

			/* Increase mana. */
			(void)sp_player(energy / 10, NULL);
		}

		/* Player is a smart cookie. */
		else
		{
			msg_format("Your mana was already at maximum.  %^s not drained.", item_name);
		}
	}

	return (TRUE);
}



/*
 * "In the dead vast and middle of the night"
 *                       - Shakespeare
 *
 * Blot out memory, snuff all the lights that do not shine eternally, and
 * strike at every creature on the level that does not resist Darkness.
 */
void nightfall(void)
{
	int i;
	u32b f1, f2, f3;

	/* Darken and forget the map */
	wiz_dark(TRUE);

	/* Zap the character, unless resistant */
	if (!p_ptr->resist_dark)
	{
		(void)explosion(0, 0, p_ptr->py, p_ptr->px, 200, GF_DARK);

		/* Character is dead */
		if (p_ptr->is_dead) return;
	}

	/* Seek out light sources */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Skip anything that isn't a light source */
		if (o_ptr->tval != TV_LITE) continue;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Skip permanent lights */
		if (f3 & (TR3_NOFUEL)) continue;

		/* Hack -- Kill the light XXX XXX */
		if (!o_ptr->flags_pval1) o_ptr->pval = 0;
	}

	/* Seek out monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		bool dark = FALSE;
		bool light = FALSE;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Note creatures of darkness */
		if (r_ptr->flags4 & (RF4_BRTH_DARK)) dark = TRUE;

		/* Skip monsters that just resist darkness */
		else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC)) continue;
		else if (r_ptr->flags3 & (RF3_ORC)) continue;
		else if (r_ptr->flags3 & (RF3_UNDEAD)) continue;

		/* Note creatures of light */
		if (r_ptr->flags4 & (RF4_BRTH_LITE)) light = TRUE;
		if (r_ptr->d_char == 'A') light = TRUE;


		/* Wake up */
		m_ptr->csleep = 0;

		/* Go active */
		m_ptr->mflag |= (MFLAG_ACTV);


		/* Creatures of darkness become more powerful */
		if ((dark) && (!light))
		{
			char m_name[DESC_LEN];
			char m_poss[DESC_LEN];

			/* Get the monster name (or "it") */
			monster_desc(m_name, m_ptr, 0x40);

			/* Get the monster possessive ("his"/"her"/"its") */
			monster_desc(m_poss, m_ptr, 0x22);

			/* Message */
			if (m_ptr->ml) msg_format("%^s becomes more powerful!", m_name);

			/* Get stronger */
			if (m_ptr->maxhp < 2 * r_ptr->hitpoints)
			{
				if (m_ptr->hp > 2 * m_ptr->maxhp / 3)
					m_ptr->maxhp += 20 + m_ptr->maxhp / 10;
			}

			/* Heal */
			m_ptr->hp = m_ptr->maxhp;

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				set_mon_fear(m_ptr, 0, FALSE);

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s recovers %s courage.", m_name, m_poss);
			}

			/* Haste */
			m_ptr->hasted = MIN(200, m_ptr->hasted + 100);
		}

		/* All other monsters get zapped hard */
		else
		{
			(void)explosion(-1, 0, m_ptr->fy, m_ptr->fx,
			                (light ? 225 : 150), GF_DARK);
		}
	}
}

/*
 * Special code for staffs of doomspells.
 */
void doomspells(bool hurt, int skill)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Damage depends on skill (or current depth) */
	int dam1 =   0 + skill * 2;
	int dam2 = 100 + skill * 2;

	/* Huge black orb - full-strength attack on everything */
	if (!hurt)
	{
		fire_orb(GF_BLACK_ORB, 0, rand_range(dam1, dam2), 5);

		/* Add a fair amount of noise */
		add_wakeup_chance += 2500;
	}

	/* Dispel the character */
	else
	{
		/* Oh no. */
		message(TERM_RED, 250, "The staff turns on you! You stare death in the face!");

		/* Blast */
		(void)explosion(0, 5, py, px, rand_range(dam1, dam2), GF_BLACK_ORB);

		/* Leave? */
		if (p_ptr->is_dead) return;

		/* Slash */
		set_cut(p_ptr->cut + WOUND_MORTAL);

		/* Drain */
		p_ptr->csp     /= 2;
		p_ptr->csp_frac = 0;

		/* Get 'em ALL awake! */
		aggravate_monsters(-1, TRUE, "You hear monsters screaming balefully!");
	}
}


/*
 * Special code for staffs of chaos.
 */
void call_chaos(int dam)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i;
	int typ, num;

	int chaos_effect;

	/* Staffs of chaos invoke a starburst of chaos */
	fire_star(GF_CHAOS, 0, dam, 3 + p_ptr->chaos_power);

	/* Chaos is a tricky thing to play with... */
	while (TRUE)
	{
		chaos_effect = rand_int(20);

		/* Radiating beams of time, nexus, or wall-removal */
		if (chaos_effect < 12)
		{
			i = randint(3);
			if (i == 1) typ = GF_NEXUS;
			if (i == 2) typ = GF_TIME;
			else        typ = GF_KILL_WALL;

			num = 8 + p_ptr->chaos_power * 2;
			dam = (dam / 5) + p_ptr->chaos_power * 3;

			/* Fire beams in random directions */
			beam_burst(py, px, typ, num, dam);
		}

		/* Secondary explosions of time or nexus */
		else if (chaos_effect < 17)
		{
			typ = ((one_in_(2)) ? GF_NEXUS : GF_TIME);
			dam = (dam / 5) + p_ptr->chaos_power * 3;

			/* Fire balls in all directions */
			for (i = 1; i <= 9; i++)
			{
				if (i != 5) fire_ball(typ, i, dam, 2);
			}
		}

		/* Rearrange the dungeon - starburst of disintegration */
		else if (chaos_effect == 17)
		{
			fire_star(GF_DISINTEGRATE, 0, (dam / 5), 2 + p_ptr->chaos_power);
		}

		/* Rearrange the dungeon - collapse ceiling */
		else if (chaos_effect == 18)
		{
			(void)collapse_ceiling(py, px, dam);
		}

		/* Summon (a pack of) chaos hounds (rarely) */
		else if (one_in_(3))
		{
			int count = 0;

			/* XXX - hard-coding.  Really need to find a better way. */
			summon_index_type = MON_CHAOS_HOUND;

			/* Attempt to summon chaos hounds */
			count = summon_specific(py, px, FALSE, 100, SUMMON_INDEX, 1);

			if ((count) && (player_can_see_bold(py, px)))
			{
				msg_print("The hounds of Chaos appear!");
			}

			/* Always stop (don't want to hurt the hounds) */
			break;
		}

		/* Usually stop, but not always... */
		if (!one_in_(3)) break;
	}

	/* Increase chaos power */
	set_chaos_power(p_ptr->chaos_power + 1, p_ptr->chaos_power_dur + 2);
}



/*
 * The Axe 'Slaughterfield'
 * -LM-
 * This axe can even go through walls...  XXX XXX
 */
void slaughterfield(int dam, object_type *o_ptr)
{
	int y = p_ptr->py;
	int x = p_ptr->px;
	int ty = 0;
	int tx = 0;

	bool fear;

	byte missile_attr;
	char missile_char = object_char(o_ptr);

	int msec = 20 + op_ptr->delay_factor * op_ptr->delay_factor *
		op_ptr->delay_factor;


	/* Get the closest monster (doesn't need to be visible) */
	get_closest_los_monster(0, p_ptr->py, p_ptr->px, &ty, &tx, FALSE);

	/* I hunger, and you WILL feed me. */
	if ((ty == 0) && (tx == 0))
	{
		/* Kill! */
		if (!take_hit(dam * 2, MSG_RED, "You are the only living thing in sight!  The axe satisfies its bloodlust on you!", "the Throwing Axe 'Slaughterfield'"))
		{
			/* Cut */
			set_cut(p_ptr->cut + rand_range(dam, dam * 5));
		}
	}
	else
	{
		/* "Klingon code is not 'released'.  It ESCAPES ..." */
		msg_print("Your throwing axe escapes!");

		/* Keep going until we run out of targets, or are stopped */
		while ((ty != 0) && (tx != 0))
		{
			/* Remember this location */
			y = ty;     x = tx;

			/* Only do visuals if the player can "see" the axe */
			if (panel_contains(y, x) && player_can_see_bold(y, x))
			{
				if (one_in_(2)) missile_attr = TERM_RED;
				else                  missile_attr = TERM_L_RED;

				/* Visual effects */
				print_rel(missile_char, missile_attr, y, x);
				move_cursor_relative(y, x);
				(void)Term_fresh();
				pause_for(msec);
				lite_spot(y, x);
				(void)Term_fresh();
			}

			/* Kill! */
			if (mon_take_hit(cave_m_idx[y][x], -1, dam, &fear, NULL))
			{
				/* Monster is dead -- find another victim */
				get_closest_los_monster(0, y, x, &ty, &tx, FALSE);
			}
			else
			{
				/* We got stopped. */
				break;
			}
		}
	}

	/* No more victims <<whimper>>.  Maybe he will throw us again... */
	drop_near(o_ptr, 0, y, x, DROP_HERE);
}




/*
 * There are few thing riskier - or more rewarding - than staring into the
 * Palantir.  -LM-
 */
int stare_into_the_palantir(void)
{
	monster_type *m_ptr;
	monster_race *r_ptr;

	int timeout;
	int i, maxlev = 0, midx = 0;
	int msex;

	bool safe = FALSE;


	/* Now you've gone and done it... */
	msg_print("You stare into the depths of the Palantir...");

	/* Flash of light, pause */
	lite_area(damroll(2, 15), 5);
	pause_for(50);


	/* Scan the monster list */
	for (i = 1; i < m_max; i++)
	{
		/* Access the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Get the highest-level intelligent unique monster */
		if ((r_ptr->level > maxlev) &&
		    (r_ptr->flags1 & (RF1_UNIQUE)) &&
		    (r_ptr->flags2 & (RF2_SMART)))
		{
			/* Monster must not be in LOF  XXX */
			if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx))
			{
				maxlev = r_ptr->level;
				midx = i;
			}
		}
	}


	/* Fight for control of the Palantir */
	if (midx)
	{
		char m_name[DESC_LEN];
		char m_pron[DESC_LEN];
		char *name1;
		char *name2;

		m_ptr = &m_list[midx];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Get the monster's name */
		(void)my_strcpy(m_name, format("%s", r_name + r_ptr->name), sizeof(m_name));

		/* Get monster pronoun */
		monster_desc(m_pron, m_ptr, 0x31);

		/* Describe the battle -- special-case Sauron, Morgoth */
		if (m_ptr->r_idx == MON_SAURON)
		{
			name1 = "The Lidless Eye";
			short_m_name(m_name);
			name2 = m_name;
		}
		else if (m_ptr->r_idx == MON_MORGOTH)
		{
			name1 = "The Lord of Darkness";
			short_m_name(m_name);
			name2 = m_name;
		}
		else
		{
			/* Note gender */
			if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
			else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;
			else                                   msex = 0;

			name1 = m_name;
			name2 = (msex == 2) ? "she" : ((msex == 1) ? "he" : "it");
			short_m_name(m_name);
		}

		/* Messages */
		message_format(MSG_YELLOW, 0, "%s stares straight back at you!", name1);
		message_format(MSG_YELLOW, 0, "You and %s meet in mental combat!", name2);


		/* Fight */
		while (TRUE)
		{
			bool he_loses = FALSE;
			bool you_lose = FALSE;

			/* Opponent has a saving throw of native level */
			if (rand_int(r_ptr->level) < 5) he_loses = TRUE;

			/* Character uses saving throw */
			if (rand_int(p_ptr->skill_sav) < 5) you_lose = TRUE;

			/* You have lost */
			if ((you_lose) && (!he_loses))
			{
				/* Oh no. */
				msg_format("%s seizes control of the Palantir ...", m_name);

				/* Darkness */
				(void)explosion(0, 3, p_ptr->py, p_ptr->px, 0, GF_DARK_WEAK);

				/* Palantir is useless for a long time */
				timeout = 1000;

				/* Half of maximum HPs */
				take_hit(p_ptr->mhp / 2, 0, "Your mind is smashed!",
					format("gazing at %s in the Palantir", m_name));

				/* Blind (no resist) */
				set_blind(p_ptr->blind + rand_range(200, 300),
					"Everything goes black!");

				/* Confuse (no resist) */
				set_confused(p_ptr->confused + rand_range(100, 150));

				/* Fear (no resist) */
				set_afraid(p_ptr->afraid + rand_range(200, 300));

				/* Paralyzation (no resist) */
				set_paralyzed(p_ptr->paralyzed + rand_range(2, 4));

				/* Can't do anything now... */
				return (timeout);
			}

			/* You have won */
			else if ((he_loses) && (!you_lose))
			{
				/* Yay! */
				msg_print("You regain control of the Palantir ...");

				/* Yay! */
				msg_format("You break %s's will, and beat %s back!", m_name, m_pron);

				/* Confuse (no resist) */
				m_ptr->confused = 20;

				/* Frighten (no resist) */
				m_ptr->monfear = 30;

				/* Stun (no resist) */
				m_ptr->stunned = 40;

				/* We're safe from all negative effects now */
				safe = TRUE;

				/* Fight over */
				break;
			}
		}
	}


	/* Flash of light */
	(void)explosion(0, 2, p_ptr->py, p_ptr->px, 0, GF_LITE_WEAK);

	/* Illuminate entire dungeon, including vaults (!) */
	wiz_lite(TRUE, TRUE);
	msg_print("You suddenly see a vision of the entire dungeon!");

	/* If not immune... */
	if (!safe)
	{
		/* Aggravate nearby monsters */
		aggravate_monsters(0, FALSE, NULL);

		/* Can blind, confuse, cause paralysis, and induce hallucinations */
		if ((!p_ptr->resist_blind) && (!check_save(100)))
		{
			set_blind(p_ptr->blind + rand_range(4, 8), "You go blind!");
		}

		if ((!p_ptr->resist_confu) && (!check_save(100)))
		{
			if (!check_save(100))
			{
				set_image(p_ptr->image + rand_range(15, 30));
				set_confused(p_ptr->confused + rand_range(15, 30));
			}
			else
			{
				set_confused(p_ptr->confused + rand_range(3, 9));
			}
		}

		if ((!p_ptr->free_act) && (!check_save(100)))
		{
			set_paralyzed(p_ptr->paralyzed + rand_range(4, 12));
		}
	}

	/* Standard timeout  XXX */
	return (rand_range(175, 350));
}
