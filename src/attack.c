/* File: attack.c */

/*
 * The non-magical attack code.
 *
 * Hit chance.  Monster resist/evade, effects of terrain.  Critical hits
 * in melee and when shooting/throwing, the effects of weapon attributes.
 * Martial arts, shield bashing, melee attacks.  Archery.  Throwing.
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Determine if character or monster blows hit.
 *
 * The chance to hit increases with attack accuracy and decreases with
 * armor.  It never rises above 95% and never falls below 5%.
 */
bool test_hit_combat(int chance, int ac, int visible)
{
	/* Power competes against armor */
	int percent = div_round(100 * (chance - ac), MAX(1, chance));

	/* Invisible entities are harder to hit */
	if (!visible) percent /= 2;

	/* Entities with limited visibility are harder to hit */
	else if (visible < ML_FULL) percent = 2 * percent / 3;

	/* Minimum of 5% chance to hit */
	if (percent < 5) percent = 5;

	/* Maximum of 95% chance to hit */
	if (percent > 95) percent = 95;

	/* Return hit or miss */
	return (rand_int(100) < percent);
}



/*
 * Determine if monster evades or resists a blow.  -LM-
 *
 * Return percentage of damage resisted.
 */
int monster_evade_or_resist(object_type *o_ptr,
	monster_type *m_ptr, byte blow_type)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3;
	int resist = 0;
	bool learn = FALSE;

	cptr p, note = "";

	char m_name[DESC_LEN];

	/* Get "the monster" or "it" */
	monster_desc(m_name, m_ptr, 0x40);

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Get base name of object kind (or "trap") */
	if      (blow_type == BLOW_TRAP)  p = "trap";
	else if (o_ptr->tval == TV_SHOT)  p = "shot";
	else if (o_ptr->tval == TV_ARROW) p = "arrow";
	else if (o_ptr->tval == TV_BOLT)  p = "bolt";
	else if (is_melee_weapon(o_ptr))  p = "weapon";
	else                              p = "missile";



	/* Some monsters are great at dodging  -EZ- */
	if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
	    (!m_ptr->confused) && (!one_in_(m_ptr->stunned ? 2 : 3)))
	{
		/* Monster is at least partially visible */
		if (m_ptr->ml)
		{
			/* Take note */
			if (blow_type == BLOW_MELEE)
				message_format(MSG_MISS, 0, "%^s evades your blow!", m_name);
			else if (blow_type == BLOW_TRAP)
				message_format(MSG_MISS, 0, "%^s dodges your %s!", m_name, p);
			else
				message_format(MSG_MISS, 0, "%^s dodges!", m_name);

			/* Learn that monster can dodge */
			l_ptr->flags2 |= (RF2_EVASIVE);
		}

		/* Can't hurt me! */
		return (100);
	}


	/*
	 * Handle monsters that resist blunt and/or edged weapons.  Vorpal
	 * (edged) and Concussion (blunt) weapons are less resistible.
	 */
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		case TV_BOLT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Resist */
			if (r_ptr->flags3 & (RF3_IM_EDGED))
			{
				/* Resist */
				if (f1 & (TR1_VORPAL)) resist = 50;
				else resist = 70;

				/* Learn */
				if ((!(l_ptr->flags3 & (RF3_IM_EDGED))) &&
					(mon_fully_visible(m_ptr)))
				{
					l_ptr->flags3 |= (RF3_IM_EDGED);
					learn = TRUE;
				}

				/* Take note */
				note = "glances off of";
			}
			else if (r_ptr->flags3 & (RF3_RES_EDGED))
			{
				if (f1 & (TR1_VORPAL)) resist = 25;
				else resist = 50;

				if ((!(l_ptr->flags3 & (RF3_RES_EDGED))) &&
					(mon_fully_visible(m_ptr)))
				{
					l_ptr->flags3 |= (RF3_RES_EDGED);
					learn = TRUE;
				}

				note = "glances off of";
			}

			break;
		}

		case TV_SHOT:
		case TV_HAFTED:
		{
			if (r_ptr->flags3 & (RF3_IM_BLUNT))
			{
				if (f1 & (TR1_VORPAL)) resist = 50;
				else resist = 70;

				if ((!(l_ptr->flags3 & (RF3_IM_BLUNT))) &&
					(mon_fully_visible(m_ptr)))
				{
					l_ptr->flags3 |= (RF3_IM_BLUNT);
					learn = TRUE;
				}

				if (strchr("G*A", r_ptr->d_char))
					note = "passes through";
				else
					note = "bounces off of";
			}
			else if (r_ptr->flags3 & (RF3_RES_BLUNT))
			{
				if (f1 & (TR1_VORPAL)) resist = 25;
				else resist = 50;

				if ((!(l_ptr->flags3 & (RF3_RES_BLUNT))) &&
					(mon_fully_visible(m_ptr)))
				{
					l_ptr->flags3 |= (RF3_RES_BLUNT);
					learn = TRUE;
				}

				if (strchr("G*A", r_ptr->d_char))
					note = "passes through";
				else
					note = "bounces off of";
			}

			break;
		}

		default:
		{
			break;
		}
	}

	if (resist)
	{
		/* Monster is fully visible */
		if (mon_fully_visible(m_ptr))
		{
			/* Take note of new resist */
			if (learn)
			{
				if (resist >= 85)
					msg_format("Your %s does almost no damage to %s!",
						p, m_name);
				else if (resist >= 70)
					msg_format("Your %s does very little damage to %s!",
						p, m_name);
				else if (resist >= 50)
					msg_format("Your %s does little damage to %s!",
						p, m_name);
				else
					msg_format("Your %s is partially resisted by %s.",
						p, m_name);
			}

			/* Note already known resistance */
			else
			{
				msg_format("Your %s %s %s.", p, note, m_name);
			}
		}
	}

	return (resist);
}

/*
 * Calculate the effects of terrain on monster AC.
 */
static int terrain_ac_adjust(monster_race *r_ptr, int feat)
{
	/* Assume no terrain effects */
	int terrain_adjust = 0;


	/* Monsters in rubble can take advantage of cover. */
	if (feat == FEAT_RUBBLE)
	{
		terrain_adjust = r_ptr->ac / 5 + 5;
	}

	/*
	 * Monsters in trees can take advantage of cover, but those skilled
	 * in nature lore can hit them more easily.
	 */
	if (feat == FEAT_TREE)
	{
		terrain_adjust = r_ptr->ac / 7 + 5;
		terrain_adjust -= get_skill(S_NATURE, 0, 20);
	}

	/* Monsters in water are vulnerable. */
	if (feat == FEAT_WATER)
	{
		terrain_adjust -= (r_ptr->ac / 5 + 3);
	}

	return (terrain_adjust);
}


/*
 * Calculation of critical hits by the player in hand-to-hand combat.
 * -LM-
 *
 * Critical hits represent the ability of a skilled fighter to make his
 * weapon go where he wants it to.  This increased control is represented
 * by adding damage dice; this makes the attack both more powerful and
 * more reliable.
 *
 * Weapons with few damage dice are less reliable in combat (because the
 * damage they do is more variable).  Their great saving grace is precisely
 * this variability; criticals benefit them more.
 *
 * Vorpal blades/weapons of concussion get lots of criticals.
 *
 * This function is responsible for the basic melee combat messages, which
 * vary according to the quality of the hit.  A distinction is made between
 * visible and invisible monsters.
 *
 * Now that py_attack_barehand also calls this, we are careful with o_ptr - JM
 *
 */
static int critical_melee(int chance, bool visible, char m_name[],
	const object_type *o_ptr, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1 = 0L, f2 = 0L, f3 = 0L;
	int max;

	bool vorpal = FALSE;
	bool sneak_attack = FALSE;

	int blows = MAX(p_ptr->num_blow, p_ptr->num_blow2);

	int add_power = 0;

	/* Power of blow */
	int power = chance;

	/* Assume no added dice */
	int add_dice = 0;


	/* Get object flags */
	if(o_ptr->tval) object_flags(o_ptr, &f1, &f2, &f3);

	/* Special quality (vorpal blades/weapons of concussion) */
	if ((f1 & (TR1_VORPAL)) || (p_ptr->special_attack & (ATTACK_VORPAL)))
	{
		power += chance / 2;
		vorpal = TRUE;
	}

	/* Guild-Burglars get more critical hits when in the dark */
	if ((p_ptr->oath & (BURGLARS_GUILD)) &&
	    (no_light()) && (m_ptr->ml))
	{
		/* 50% more critical hits if darkness both here and nearby */
		power += chance * darkness_ratio(1) / 200;
	}


	/* Special ability (sneak attacks) */
	if (p_ptr->special_attack & (ATTACK_SNEAK)) sneak_attack = TRUE;


	/*** Calculate slay bonus (can be as great as 60 points) ***/
	max = 10;

	/* Those without a realm get a larger bonus */
	if (!p_ptr->realm) max = 40;

	/* Pure warriors get the maximum bonus */
	if (p_ptr->oath & (OATH_OF_IRON)) max = 60;

	/* Note that bonuses are not cumulative */
	if (r_ptr->flags3 & (RF3_DEMON))
	{
		add_power = MAX(get_skill(S_PIETY,    0, max), add_power);
	}
	if (r_ptr->flags3 & (RF3_ANIMAL))
	{
		add_power = MAX(get_skill(S_NATURE,   0, max), add_power);
	}
	if (r_ptr->flags3 & (RF3_UNDEAD))
	{
		add_power = MAX(get_skill(S_DOMINION, 0, max), add_power);
	}

	/* Add slay bonus */
	power += add_power;


	/* Penalize some conditions */
	if (p_ptr->confused || p_ptr->image || !visible)
	{
		sneak_attack = FALSE;
		power /= 3;
	}
	else if (visible < ML_FULL)
	{
		power /= 2;
	}


	/* Test for critical hit (weak attacks are penalized). */
	if ((sneak_attack) || (randint(power + 200) <= (power - 20)))
	{
		/* Determine level of critical hit. */
		if      ((power > 160) && (one_in_(40))) add_dice = 5;
		else if ((power >  80) && (one_in_(12))) add_dice = 4;
		else if ((power >  40) && (one_in_(3)))  add_dice = 3;
		else                                     add_dice = 2;

		/* Sneak attacks can be powerful */
		if (sneak_attack) add_dice += get_skill(S_BURGLARY, 0, 2);

		/* Encourage the player to beat on sleeping monsters. */
		if ((m_ptr->csleep) && (visible))
		{
			/* More "interesting" messages if we get a seriously good hit */
			if (add_dice >= 4)
			{
				message(MSG_HIT, 0, "You ruthlessly sneak attack!");
			}

			/* Standard "wakeup call" */
			else
			{
				message(MSG_HIT, 0, "You rudely awaken the monster!");
			}
		}

		/* Print special messages if monster is visible. */
		if (visible)
		{
			/*
			 * Messages depend on quality of critical hit.  A distinction
			 * is often made between edged and blunt weapons.  Unfortu-
			 * nately, whips sometimes display rather odd messages...
			 */
			if (add_dice <= 2)
			{
				if (o_ptr->tval)
				{
					message_format(MSG_HIT_GOOD, 0, "You strike %s.", m_name);
				}
				else if (p_ptr->barehand == S_KARATE)
				{
					message_format(MSG_HIT_GOOD, 0, "You pummel %s.", m_name);
				}
				else message_format(MSG_HIT_GOOD, 0, "You wrench %s.", m_name);
			}

			else if (add_dice == 3)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				{
					message_format(MSG_HIT_GREAT, 0, "You hack at %s!", m_name);
				}
				else if (o_ptr->tval)
				{
					message_format(MSG_HIT_GREAT, 0, "You pound %s!", m_name);
				}
				else if (p_ptr->barehand == S_KARATE)
				{
					message_format(MSG_HIT_GREAT, 0, "You roundhouse %s!", m_name);
				}

				else message_format(MSG_HIT_GREAT, 0, "You crush %s!", m_name);
			}

			else if (add_dice == 4)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				{
					if (vorpal)
					{
						message_format(MSG_HIT_SUPERB, 0,
							"Your vorpal blade goes snicker-snack!");
					}
					else
					{
						message_format(MSG_HIT_SUPERB, 0, "You slice into %s!", m_name);
					}
				}
				else if (o_ptr->tval)
				{
					message_format(MSG_HIT_SUPERB, 0, "You bludgeon %s!", m_name);
				}
				else if (p_ptr->barehand == S_KARATE)
				{
					message_format(MSG_HIT_GREAT, 0, "You tiger claw %s!", m_name);
				}

				else message_format(MSG_HIT_GREAT, 0, "You slam %s!", m_name);
			}

			else if (add_dice >= 5)
			{
				if ((vorpal) && ((o_ptr->tval == TV_SWORD) ||
					(o_ptr->tval == TV_POLEARM)))
				{
					message_format(MSG_HIT_HI_GREAT, 0,
						"Your vorpal blade goes snicker-snack!", m_name);
				}
				else if (o_ptr->tval)
				{
					message_format(MSG_HIT_HI_GREAT, 0, "You *smite* %s!", m_name);
				}
				else if (p_ptr->barehand == S_KARATE)
				{
					message_format(MSG_HIT_GREAT, 0, "You dragon-kick %s!", m_name);
				}

				else message_format(MSG_HIT_GREAT, 0, "You body slam %s!", m_name);
			}
		}

		/* Critical hits with polearms and wrestling rob the monster of energy */
		if (o_ptr->tval == TV_POLEARM || (p_ptr->barehanded && p_ptr->barehand == S_WRESTLING
			&& !((r_ptr->flags1 & (RF1_NEVER_MOVE)) || (monster_immaterial(r_ptr)))))
		{
			/* Discount multiple blows */
			int e_loss = 60 / blows;

			/* Take away energy  */
			mon_adjust_energy(m_ptr, -e_loss);
		}

		/* Critical hits with blunt weapon occasionally stun monsters */
		if ((o_ptr->tval == TV_HAFTED) && (!m_ptr->stunned) &&
		    (one_in_(3 * blows)))
		{
			/* Skill is important */
			int skill = get_skill(TV_HAFTED, 10, 110);

			/* Attempt to stun the monster; handle resistances properly */
			project_bolt(0, 1, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx,
				skill, GF_DO_STUN, PROJECT_HIDE);
		}

		/* Karate frequently stuns, confuses, and slows monsters */
		if(p_ptr->barehand == S_KARATE && p_ptr->barehanded)
		{
			int skill = get_skill(S_KARATE, 10, 110);

			if (one_in_(blows * 6))
			{
				/* Attempt to stun the monster */
				project_bolt(0, 1, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx,
					skill, GF_DO_STUN, PROJECT_HIDE);
			}
			else if (one_in_(blows * 6 - 1))
			{
				/* Attempt to slow the monster */
				project_bolt(0, 1, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx,
					skill, GF_DO_SLOW, PROJECT_HIDE);
			}

			else if (one_in_(blows * 6 - 2))
			{
				/* Attempt to confuse the monster */
				project_bolt(0, 1, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx,
					skill, GF_DO_CONF, PROJECT_HIDE);

			}
		}
	}

	/* If the blow is not a critical hit, then the default message is shown. */
	else if (visible)
	{
		message_format(MSG_HIT, 0, "You hit %s.", m_name);
	}

	/* Hits on non-visible monsters always generate the same message. */
	if (!visible)
	{
		message(MSG_HIT, 0, "You hit something.");
	}

	/* Sometimes cancel vorpal blows */
	if ((p_ptr->special_attack & (ATTACK_VORPAL)) &&
	    (one_in_(30)))
	{
		p_ptr->special_attack &= ~(ATTACK_VORPAL);
		msg_print("Your weapon is no longer gleaming.");
	}

	/* Sneak-attacks practice the burglary skill a little */
	if (sneak_attack) practice_skill(3 + r_ptr->level * r_ptr->level / 3, S_BURGLARY);


	/* Return the number of damage dice to add. */
	return (add_dice);
}



/*
 * Calculation of critical hits for objects fired or thrown by the player.
 * -LM-
 *
 * Critical shots represent the ability of a skilled fighter to make his
 * missiles go where he wants them to.  This increased control is
 * represented by adding damage dice; this makes the attack both more
 * powerful and more reliable.
 *
 * This function is responsible for the basic archery and throwing combat
 * messages, which vary according to the quality of the hit.  A distinction
 * is made between visible and invisible monsters.
 */
static int critical_shot(int chance, bool thrown_weapon, bool visible,
	char m_name[], const object_type *o_ptr, monster_type *m_ptr, char *hit_msg)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char o_name[DESC_LEN];

	u32b f1, f2, f3;
	int max;

	bool vorpal = FALSE;

	int add_power = 0;

	/* Extract missile power. */
	int power = chance;

	/* Assume no added dice */
	int add_dice = 0;


	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Obtain a terse object description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);


	/* Throwing weapons get more critical hits. */
	if (thrown_weapon) power += chance / 3;

	/* Special quality (vorpal arrows/shots of concussion) */
	if (f1 & (TR1_VORPAL))
	{
		power += chance / 2;
		vorpal = TRUE;
	}

	/* Guild-Burglars get more critical hits when in the dark */
	if ((p_ptr->oath & (BURGLARS_GUILD)) &&
	    (no_light()) && (m_ptr->ml))
	{
		/* 50% more critical hits if darkness both here and nearby */
		power += chance * darkness_ratio(1) / 200;
	}


	/*** Calculate slay bonus (can be as great as 60 points) ***/
	max = 10;

	/* Those without a realm get a larger bonus */
	if (!p_ptr->realm) max = 40;

	/* Pure warriors get the maximum bonus */
	if (p_ptr->oath & (OATH_OF_IRON)) max = 60;

	/* Note that bonuses are not cumulative */
	if (r_ptr->flags3 & (RF3_DEMON))
	{
		add_power = MAX(get_skill(S_PIETY,    0, max), add_power);
	}
	if (r_ptr->flags3 & (RF3_ANIMAL))
	{
		add_power = MAX(get_skill(S_NATURE,   0, max), add_power);
	}
	if (r_ptr->flags3 & (RF3_UNDEAD))
	{
		add_power = MAX(get_skill(S_DOMINION, 0, max), add_power);
	}

	/* Add slay bonus */
	power += add_power;


	/* Penalize some conditions */
	if (p_ptr->blind || p_ptr->confused || p_ptr->image || !visible) power /= 3;
	else if (!mon_fully_visible(m_ptr)) power = 2 * power / 3;


	/* Test for critical hit (weak attacks are penalized). */
	if (randint(power + 200) <= (power - 30))
	{
		/* Determine level of critical hit. */
		if      ((power > 120) && (one_in_(32))) add_dice = 5;
		else if ((power >  80) && (one_in_(16))) add_dice = 4;
		else if ((power >  40) && (one_in_(8)))  add_dice = 3;
		else                                     add_dice = 2;

		/* Encourage the player to throw and shoot things at sleeping monsters. */
		if ((m_ptr->csleep) && (mon_fully_visible(m_ptr)))
		{
			/* More "interesting" messages if we get an especially good hit. */
			if ((thrown_weapon) && (add_dice >= 3))
			{
				message(MSG_HIT, 0, "Assassin strike!");
			}

			/* Standard "wakeup call". */
			else
			{
				message(MSG_HIT, 0, "You rudely awaken the monster!");
			}
		}

		/* Print special messages if monster is fully visible. */
		if (mon_fully_visible(m_ptr))
		{
			/* Messages depend on quality of critical hit. */
			if (add_dice <= 2)
			{
				/* Combine with hurt or kill message later */
				strcpy(hit_msg,
					format("The %s penetrates %s", o_name, m_name));
			}

			else if (add_dice == 3)
			{
				message_format(MSG_HIT, 0, "The %s drives into %s!",
					o_name, m_name);
			}

			else if (add_dice >= 4)
			{
				message_format(MSG_HIT_HI_SUPERB, 0, "The %s transpierces %s!",
					o_name, m_name);
			}
		}
	}

	/* If the shot is not a critical hit, then the default message is shown. */
	else if (mon_fully_visible(m_ptr))
	{
		/* Combine with hurt or kill message later */
		strcpy(hit_msg, format("The %s hits %s", o_name, m_name));
	}

	/* Hits on non-visible monsters always generate the same message. */
	if (!mon_fully_visible(m_ptr))
	{
		strcpy(hit_msg, format("The %s finds a mark", o_name));
	}

	/* Return the number of damage dice to add. */
	return (add_dice);
}


/*
 * Use up special attack powers.
 */
static void dec_special_atk(void)
{
	(void)set_acid_attack(p_ptr->acid_attack - 1);
	(void)set_elec_attack(p_ptr->elec_attack - 1);
	(void)set_fire_attack(p_ptr->fire_attack - 1);
	(void)set_cold_attack(p_ptr->cold_attack - 1);
	(void)set_pois_attack(p_ptr->pois_attack - 1);


	/* Use up holy attack  */
	if ((p_ptr->special_attack & (ATTACK_HOLY)) && (one_in_(20)))
	{
		msg_print("You no longer strike with holy force.");
		p_ptr->special_attack &= ~(ATTACK_HOLY);
	}
}

/*
 * Handle all special adjustments to the damage done by a non-magical attack.
 *
 * At present, only weapons (including digging tools) and ammo have their
 * damage adjusted.  Flasks of oil could do fire damage, but they currently
 * don't.
 *
 * Most slays add 18 to damage, except Slay Animal, Slay Evil, and Slay
 * Orc, which all add 10.  Weapons of *slaying* now get a larger bonus.
 * Brands are usually +14, except for acid, which is +10.
 *
 * Monsters can be susceptible to impact (rock remover) and shining
 * weapons.
 *
 * Note:  When the character is using martial arts, this function is
 * called with a wiped object "o_ptr".  Be careful.   XXX XXX
 */
void adjust_dam(int *damage, object_type *o_ptr, monster_type *m_ptr,
	bool is_trap)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	bool visible = (mon_fully_visible(m_ptr) != FALSE);

	/* Assume no special adjustments to damage */
	int mul = 10;
	int divide = 10;
	int add = 0;
	int sub = 0;

	/* Get object attributes */
	if (o_ptr->tval) object_flags(o_ptr, &f1, &f2, &f3);

	/* Handle actual melee combat */
	if (!is_trap)
	{
		u32b fc1, fc2, fc3;
		object_type *i_ptr = &inventory[INVEN_HANDS];

		/* Get character flags */
		player_flags(&fc1, &fc2, &fc3, TRUE, FALSE);

		/* Apply character flags */
		f1 |= fc1;    f2 |= fc2;    f3 |= fc3;

		/* Apply brands and slays from gauntlets for martial artists */
		if (!o_ptr->tval)
		{
			f1 |= i_ptr->flags1; f2 |= i_ptr->flags2; f3 |= i_ptr->flags3;
		}

		/* Get "cancelled" flags */
		player_flags_cancel(&fc1, &fc2, &fc3, TRUE);

		/* Apply character cancellation flags (cancels everything) */
		f1 &= ~fc1;    f2 &= ~fc2;    f3 &= ~fc3;
	}


	/* Wielded weapons and diggers and fired missiles may do extra damage. */
	switch (o_ptr->tval)
	{
		/* Martial arts can get slay and brand bonuses */
		case 0:
		{
			/* Fall through */
		}

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
				if (visible)
				{
					l_ptr->flags3 |= (RF3_ANIMAL);
				}

				if ((o_ptr->ego_item_index == EGO_KILL_ANIMAL) &&
				        (add < 15)) add = 15;
				else if (add < 10)  add = 10;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}

				if (add < 10)  add = 10;
			}

			/* Slay Undead */
			if ((f1 & (TR1_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_UNDEAD);
				}

				if ((o_ptr->ego_item_index == EGO_KILL_UNDEAD) &&
				        (add < 27)) add = 27;
				else if (add < 18)  add = 18;
			}



			/* Slay Demon */
			if ((f1 & (TR1_SLAY_DEMON)) &&
			    (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_DEMON);
				}

				if ((o_ptr->ego_item_index == EGO_KILL_DEMON) &&
				        (add < 27)) add = 27;
				else if (add < 18)  add = 18;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_ORC);
				}

				if (add < 10)  add = 10;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_TROLL);
				}

				if (add < 18)  add = 18;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_GIANT);
				}

				if (add < 18)  add = 18;
			}

			/* Slay Dragon */
			if (((f1 & (TR1_SLAY_DRAGON)) || (f1 & (TR1_KILL_DRAGON))) &&
			     (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (visible)
				{
					l_ptr->flags3 |= (RF3_DRAGON);
				}

				if ((f1 & (TR1_KILL_DRAGON)) && (add < 27)) add = 27;
				else if (add < 18) add = 18;
			}

			/* Brand (Acid) */
			if (f1 & (TR1_BRAND_ACID))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ACID))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_IM_ACID);
					}
				}

				/* Otherwise, take extra damage */
				else if (add < 10) add = 10;
			}

			/* Brand (Elec) */
			if (f1 & (TR1_BRAND_ELEC))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_ELEC))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_IM_ELEC);
					}
				}

				/* Otherwise, take extra damage */
				else if (add < 14) add = 14;
			}

			/* Brand (Fire) */
			if ((f1 & (TR1_BRAND_FIRE)) || (f1 & (TR1_BRAND_FLAME)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_IM_FIRE);
					}
				}

				/* Otherwise, take extra damage */
				else
				{
					if ((f1 & (TR1_BRAND_FLAME)) && (add < 22)) add = 22;
					else if (add < 14) add = 14;

					/* Notice susceptibility */
					if (r_ptr->flags3 & (RF3_HURT_FIRE))
					{
						if ((f1 & (TR1_BRAND_FLAME)) && (add < 33)) add = 33;
						else if (add < 21) add = 21;

						if (visible)
						{
							l_ptr->flags3 |= (RF3_HURT_FIRE);
						}
					}
				}
			}

			/* Brand (Cold) */
			if (f1 & (TR1_BRAND_COLD))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_COLD))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_IM_COLD);
					}
				}

				/* Otherwise, take extra damage */
				else
				{
					if (add < 14) add = 14;

					/* Notice susceptibility */
					if (r_ptr->flags3 & (RF3_HURT_COLD))
					{
						if (add < 21) add = 21;

						if (visible)
						{
							l_ptr->flags3 |= (RF3_HURT_COLD);
						}
					}
				}
			}

			/* Brand (Poison) */
			if ((f1 & (TR1_BRAND_POIS)) || (f1 & (TR1_BRAND_VENOM)))
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_POIS))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_IM_POIS);
					}
				}

				/* Otherwise, take extra damage */
				else if ((f1 & (TR1_BRAND_VENOM)) && (add < 22)) add = 22;
				else if (add < 14) add = 14;
			}

			/* Special attack (Impact and Tunneling) */
			if ((f3 & (TR3_IMPACT)) ||
			    (get_object_pval(o_ptr, TR_PVAL_TUNNEL) > 0))
			{
				/* Notice susceptibility */
				if (r_ptr->flags3 & (RF3_HURT_ROCK))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_HURT_ROCK);
					}

					/* Take extra damage */
					if (add < 14) add = 14;
				}
			}

			/* Special attack (Light) */
			if (f3 & (TR3_LITE))
			{
				/* Creature is hurt by light */
				if (r_ptr->flags3 & (RF3_HURT_LITE))
				{
					if (visible)
					{
						l_ptr->flags3 |= (RF3_HURT_LITE);
					}

					/* Take extra damage */
					if (add < 14) add = 14;
				}
			}

			break;
		}
	}

	/* Hack -- Triple crossbows get 1/3rd the bonus */
	if ((o_ptr->tval == TV_CROSSBOW) && (o_ptr->sval == SV_TRIPLE_XBOW))
		add = div_round(add, 3);


	/* Use up special attack powers, unless a trap */
	if (!is_trap) dec_special_atk();

	/* Apply addition and subtraction */
	/* Scale damage for low level characters */
	if (skill_being_used != S_NOSKILL)
		*damage += ((add - sub) * get_skill(skill_being_used, 50, 100)) / 100;
	else
		*damage += (add - sub);

	/* Apply multiplier, if positive */
	if (mul > 1) *damage *= mul;

	/* Apply divisor, if positive */
	if (divide > 1) *damage = div_round(*damage, divide);
}


/*
 * Modes of combat
 */
#define COMBAT_MELEE    1
#define COMBAT_FIRE     2
#define COMBAT_THROW    3

/*
 * Various special combat damage adjustments
 */
#define DAM_ADJUST_DAM        0x0001
#define DAM_ADJUST_DEADLY     0x0002


/*
 * Calculate damage done by weapons, missiles, and thrown objects.  -LM-
 *
 * Damage depends on damage dice plus brands/slays.  Critical hits and
 * Deadliness can greatly enhance dice, and therefore overall damage.
 *
 * Return the damage.  Store combat hit messages, if needed.
 */
static int calc_combat_dam(int mode, object_type *o_ptr, monster_type *m_ptr,
	char *m_name, int total_deadliness, int chance, char *msg_hit, u16b combat_mods)
{
	int dice;
	long die_average, temp, sides, damage;
	int dam_int;
	u32b f1, f2, f3;

	bool throwing_weapon = FALSE;

	/* Clear the hit message */
	msg_hit[0] = '\0';


	/* Get object flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Notice thrown weapon */
	if (f1 & (TR1_THROWING)) throwing_weapon = TRUE;


	/* Base damage dice */
	dice = o_ptr->dd;

	/* Critical hits may add damage dice. */
	if (mode == COMBAT_MELEE)
	{
		dice += critical_melee(chance, m_ptr->ml, m_name, o_ptr, m_ptr);
	}
	else if (mode == COMBAT_FIRE)
	{
		dice += critical_shot(chance, FALSE, m_ptr->ml,
		                      m_name, o_ptr, m_ptr, msg_hit);
	}
	else if ((mode == COMBAT_THROW) && (throwing_weapon))
	{
		dice += critical_shot(chance, TRUE, m_ptr->ml,
		                      m_name, o_ptr, m_ptr, msg_hit);
	}


	/* Get the average value of a single damage die. (x10) */
	die_average = (10 * (o_ptr->ds + 1)) / 2;

	/* Apply deadliness to average. (100x inflation) */
	apply_deadliness(&die_average, total_deadliness);


	/* When shooting,  apply the launcher multiplier to average. */
	if (mode == COMBAT_FIRE) die_average *= p_ptr->ammo_mult;


	/* Special case:  Throwing a throwing weapon. */
	if ((mode == COMBAT_THROW) && (throwing_weapon))
	{
		/* Bonuses depend on specialization */
		int max = 85;
		if (p_ptr->oath & (OATH_OF_IRON)) max = 100;
		else if (p_ptr->oath & (BURGLARS_GUILD)) max = 90;
		else if (p_ptr->oath) max = 70;

		/*
		 * Multiply the die average by the throwing
		 * weapon multiplier, if applicable.  This is not the
		 * prettiest equation, but it does at least try to keep
		 * throwing weapons competitive.
		 */
		die_average *= get_skill(S_THROWING, 20, max);

		/* Perfectly balanced weapons do 50% extra damage. */
		if (f1 & (TR1_PERFECT_BALANCE))
		{
			die_average += die_average / 2;
		}

		/* Only the real throwing weapons get the full bonus */
		if (!is_any_weapon(o_ptr)) die_average /= 2;

		/* Deflate */
		die_average = div_round(die_average, 10);
	}


	/* Reconvert to die sides. */
	temp = (2L * die_average) - 1000;

	/* Calculate the actual number of sides to each die. */
	sides = div_round(temp, 1000);


	/* Roll out the damage. */
	damage = damroll(dice, (s16b)sides);

	/* Special damage bonuses */
	if (combat_mods & (DAM_ADJUST_DAM))    damage += 10;
	if (combat_mods & (DAM_ADJUST_DEADLY)) damage += 20;

	/* Convert damage to integer */
	dam_int = (int)damage;

	/* Adjust damage for slays, brands, resists. */
	adjust_dam(&dam_int, o_ptr, m_ptr, FALSE);

	/* No negative damage */
	if (dam_int < 0) dam_int = 0;

	/* Return the damage */
	return (dam_int);
}


/*
 * Burglary skill makes you far less noisy in combat.  Even small
 * investments have a significant effect.
 */
int get_combat_noise(int min, int max)
{
	int skill = get_skill(S_BURGLARY, 0, 400);

	/* Get range */
	long silence = max - min;

	/* Get silencing factor (from 0 to 20) */
	silence *= rsqrt(skill);

	/* Divide by maximum possible silencing factor (20) */
	silence = div_round(silence, 20);

	/* Return adjusted value for noise */
	return (max - silence);
}



/*
 * Shield bash monsters.  -LM-
 *
 * We assume that certain checks (wearing a shield, not confused, etc.) have
 * been done.
 *
 * Return TRUE if the monster got killed.
 */
static bool shield_bash(int y, int x, monster_race *r_ptr,
	monster_type *m_ptr, int *blows, bool *fear)
{
	object_type *weapon_ptr = &inventory[INVEN_WIELD];
	object_type *shield_ptr = &inventory[INVEN_ARM];

	/* Variables for the bashing code */
	int bash_chance, bash_quality, bash_dam;

	char m_name[DESC_LEN];

	int wskill = get_skill(sweapon(weapon_ptr->tval), 0, 100);


	/* No shield bashing monsters in non-passable terrain */
	if (!cave_passable_bold(y, x)) return (FALSE);

	/* No bashing immaterial monsters */
	if (monster_immaterial(r_ptr)) return (FALSE);


	/* Get monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x40);

	/* Bashing chance depends on dexterity and a Skill bonus */
	bash_chance = ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128) / 2;

	/* Apply melee skill bonus, depending on realm */
	if (p_ptr->realm)
	{
		if (!p_ptr->oath) bash_chance +=
			get_skill(sweapon(weapon_ptr->tval), 0, 50);
	}
	else
	{
		bash_chance += wskill;
	}

	/* Evasive monsters are very hard to bash */
	if (r_ptr->flags2 & (RF2_EVASIVE))
	{
		bash_chance /= 8;
	}


	/* Players bash more often when they see a real need. */
	if (bash_chance)
	{
		/* We are using bare hands, and we're not very good at it */
		if (!weapon_ptr->k_idx)
		{
			if (wskill < p_ptr->power / 2)
			{
				bash_chance *= MIN(8, p_ptr->power / MAX(1, wskill));
				bash_chance /= 2;
			}
		}

		/* Our weapon is weaker than our shield */
		else if ((weapon_ptr->dd * weapon_ptr->ds * p_ptr->num_blow) <
		         (shield_ptr->dd * shield_ptr->ds * 3))
		{
			bash_chance *= 2;
		}
	}

	/* Try to get in a shield bash. */
	if (bash_chance > rand_int(300 + r_ptr->level * 15))
	{
		message(MSG_HIT, 0, "You get in a shield bash!");

		/* Calculate attack quality, a mix of momentum and accuracy. */
		bash_quality = p_ptr->skill_thn + (p_ptr->wt / 8) +
			(p_ptr->total_weight / 80) + (shield_ptr->weight / 3);

		/* Calculate damage.  Big shields are deadly. */
		bash_dam = damroll(shield_ptr->dd, shield_ptr->ds);

		/* Multiply by bash quality and overall character power */
		bash_dam *= (bash_quality / 20 + p_ptr->power / 12);

		/* Strength bonus */
		bash_dam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);

		/* No huge damages. */
		if (bash_dam > 125) bash_dam = 125;

		/* Encourage the player to keep wearing that heavy shield. */
		if (bash_dam > 20 + randint(bash_dam)) message(MSG_HIT, 0, "WHAMM!");

		/* Damage, check for fear and death. */
		if (mon_take_hit(cave_m_idx[y][x], -1, bash_dam, fear, NULL))
		{
			/* Fight's over. */
			return (TRUE);
		}

		/* Stunning. */
		if (bash_quality > randint(200 + r_ptr->level))
		{
		       message_format(MSG_HIT, 0, "%^s is stunned.", m_name);

			m_ptr->stunned += rand_range(4, 14);
			if (m_ptr->stunned > 24) m_ptr->stunned = 24;
		}

		/* Confusion. */
		if (bash_quality > randint(400 + r_ptr->level * 2) &&
			(!(r_ptr->flags3 & (RF3_NO_CONF))))
		{
			message_format(MSG_HIT, 0, "%^s appears confused.", m_name);

			m_ptr->confused += rand_range(2, 5);
		}

		/* The player will sometimes stumble. */
		if ((20 + (int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128) < randint(80))
		{
			*blows += randint(MAX(p_ptr->num_blow, p_ptr->num_blow2));
			/* msg_print("You stumble!"); */
		}
	}

	return (FALSE);
}


/*
 * Striking certain kinds of monsters with your bare hands is risky,
 * especially if you aren't wearing gloves.
 *
 * We really should use a bitflag, but there are few available.
 *
 * Return TRUE if we get hurt.
 */
static bool contact_danger_check(monster_race *r_ptr)
{
	int i;

	int blow_type[MONSTER_BLOW_MAX] = { 0, 0, 0, 0 };
	int ele_blow_cnt = 0;

	cptr name = (r_name + r_ptr->name);

	int damage, sides, danger_type, protection;


	/* Get the protective value of worn gloves */
	object_type *o_ptr = &inventory[INVEN_HANDS];
	protection = o_ptr->ac + o_ptr->to_a;


	/* If the monster is a Death Mold, you're in trouble. */
	if (strstr(name, "Death mold"))
	{
		(void)take_hit(damroll(25, 25) - (protection * 10),
			0, "Oh no!  You've touched a Death mold!",
			"touching a Death mold");
		return (TRUE);
	}


	/* Some monsters are almost always dangerous to touch. */
	else if (strchr("Ev*mj,i", r_ptr->d_char))
	{
		for (i = 0; i < MONSTER_BLOW_MAX; i++)
		{
			if (r_ptr->blow[i].method)
			{
				if (r_ptr->blow[i].effect == RBE_ACID)
					blow_type[ele_blow_cnt++] = GF_ACID;
				else if (r_ptr->blow[i].effect == RBE_ELEC)
					blow_type[ele_blow_cnt++] = GF_ELEC;
				else if (r_ptr->blow[i].effect == RBE_FIRE)
					blow_type[ele_blow_cnt++] = GF_FIRE;
				else if (r_ptr->blow[i].effect == RBE_COLD)
					blow_type[ele_blow_cnt++] = GF_COLD;
				else if (r_ptr->blow[i].effect == RBE_POISON)
					blow_type[ele_blow_cnt++] = GF_POIS;
			}
		}
	}


	/* Monster was not dangerous */
	if (ele_blow_cnt == 0) return (FALSE);


	/* Pick a type of nastiness at random */
	danger_type = blow_type[rand_int(ele_blow_cnt)];

	/* Calculate damage */
	sides = 1 + div_round(r_ptr->level, 8);
	damage = damroll(2, sides) - protection / 2;

	/* Inflict nastiness */
	if (damage > 0)
	{
		if ((danger_type == GF_ACID) && (!p_ptr->immune_acid))
		{
			acid_dam(damage, 0, "The monster is acidic!",
			   "touching an acidic creature");
			return (TRUE);
		}
		else if ((danger_type == GF_ELEC) && (!p_ptr->immune_elec))
		{
			elec_dam(damage, 0, "You are zapped!",
			   "touching an electric creature");
			return (TRUE);
		}
		else if ((danger_type == GF_FIRE) && (!p_ptr->immune_fire))
		{
			fire_dam(damage, 0, "You get burnt!",
			   "touching a fiery creature");
			return (TRUE);
		}
		else if ((danger_type == GF_COLD) && (!p_ptr->immune_cold))
		{
			cold_dam(damage, 0, "You feel very cold!",
			   "touching an icy creature");
			return (TRUE);
		}
		else if (danger_type == GF_POIS)
		{
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + damage);
				(void)take_hit(damage / 2, 0, NULL, "touching a poisonous creature");
				return (TRUE);
			}
		}
	}

	/* Didn't get hurt */
	return (FALSE);
}

/*
 * Return the dice and sides for a martial arts attack -JM
 */

static int barehand_dam_to_dice(int dam, int *dice, int *sides)
{
	long die_average, temp;

	if (p_ptr->barehand == S_KARATE)
	{
		if (dam >= 12)      *dice = 3; // 3d8
		else if (dam >= 7)  *dice = 2; // 2d6
		else                *dice = 1;
	}
	else
	{
		if (dam >= 20)      *dice = 5; // 5d7
		else if (dam >= 14) *dice = 4; // 4d6
		else if (dam >= 9)  *dice = 3; // 3d5
		else if (dam >= 5)  *dice = 2; // 2d4
		else                *dice = 1;
	}

	/* Get the average value of a single damage die. (x10) */
	die_average = 10 * dam / *dice;

	/* Apply deadliness to average. (100x inflation) */
	apply_deadliness(&die_average, p_ptr->to_d);

	/* Reconvert to die sides. */
	temp = (2L * die_average) - 1000;

	/* Calculate the actual number of sides to each die. */
	*sides = div_round(temp, 1000);

	return 0;
}

/*
 * Learn about the recent hit rate for your attacks
 *
 * The hit rate displayed is an approximate running average, with recent
 * attacks very slowly overriding old ones.
 */

void learn_about_hits(int hit, bool offhand)
{
    s16b *temp;

    /* Select the correct hand */
    if (offhand)    temp = &p_ptr->avg_hit_offhand;
    else            temp = &p_ptr->avg_hit;

    /* Revise (or create) estimate of hit rate */
    if (*temp <= 0) *temp = hit * 100;
    else            *temp = div_round(*temp * 99, 100) + hit;

}


/*
 * Learn about the damage done in martial arts.
 *
 * The damage displayed is an approximate running average, with recent
 * attacks slowly overriding old ones.
 */
void learn_about_damage(int damage, bool offhand)
{
	s16b *temp;

	/* Randomize damage (inaccuracy) */
	damage = rand_spread(damage, div_round(damage, 2));
	if (damage <= 0) damage = 1;

	if (offhand) temp = &p_ptr->avg_dam_offhand;
	else         temp = &p_ptr->avg_dam;

	/* If we have no memory, we use a sample size of 1 */
	if (!*temp)
	{
		*temp = 10 * damage;
	}

	/* Otherwise, we revise our opinion of the damage we do */
	else
	{
		/* Reduce remembered damage by a tenth */
		*temp = div_round(9 * (*temp), 10);

		/* Add the new damage */
		*temp += damage;
	}
}

/*
 * Bare-handed combat.
 *
 * Bare-handed combat is a major candidate for further improvements later.
 *
 * Martial arts should get bonuses from virtually everything that melee
 * weapons do.  However, the effectiveness of special bonuses may vary
 * widely between the two forms of combat.
 */
static int py_attack_barehand(int chance, monster_type *m_ptr, char m_name[])
{
	int dice, sides;
	int damage;

	int skill = get_skill(p_ptr->barehand, 0, 100);

	object_type *o_ptr;
	object_type object_type_body;

	/* Get local object */
	o_ptr = &object_type_body;

	/* Set all object data to zero */
	object_wipe(o_ptr);


	/* Calculate raw damage -JM
	 *
	 * Note that Karate does significantly less raw damage than wrestling,
	 * even taking into account the extra blows it recieves.  Because karate
	 * should be getting better criticals than wrestling due to the smaller dice.
	 *
	 * Note also that both have starting damage equal to a main gauche, which seems
	 * much more reasonable than 1 or 2 measly damage.
	 */
	if (p_ptr->barehand == S_WRESTLING)
	{
		if(p_ptr->oath & (OATH_OF_IRON))
		{
			// Maximum is 28
			damage = 3 + div_round((skill * 100 + skill * skill), 800);
		}
		else
		{
			// Maximum is 25
			damage = 3 + div_round(20 + (skill * 100 + skill * skill), 910);
		}
	}
	else if (p_ptr->barehand == S_KARATE)
	{
		if(p_ptr->oath & (OATH_OF_IRON))
		{
			// Maximum is 15
			damage = 3 + div_round((skill * 200 + skill * skill), 2500);
		}
		else
		{
			// Maximum is 13
			damage = 3 + div_round((skill * 200 + skill * skill), 3000);
		}
	}
	else
	{
		/* In case this was called incorrectly */
		return 0;
	}


	/* Turn base damage into dice */
	barehand_dam_to_dice(damage, &dice, &sides);

	dice += critical_melee(chance, m_ptr->ml, m_name, o_ptr, m_ptr);

	/* Roll out the damage. */
	damage = damroll(dice, (s16b)sides);

	/* Apply slay and brand bonuses (if any) */
	adjust_dam(&damage, o_ptr, m_ptr, FALSE);

	return (damage);
}


/*
 * Deadliness multiplies the damage done by a percentage, which varies
 * from 0% (no damage done at all) to at most 355% (damage is multiplied
 * by more than three and a half times!).
 *
 * We use the table "deadliness_conversion" to translate internal plusses
 * to deadliness to percentage values.
 *
 * This function multiplies damage by 100.
 */
void apply_deadliness(long *die_average, int deadliness)
{
	int i;

	/* Paranoia - ensure legal table access. */
	if (deadliness >  150) deadliness =  150;
	if (deadliness < -150) deadliness = -150;

	/* Deadliness is positive - damage is increased */
	if (deadliness >= 0)
	{
		i = deadliness_conversion[deadliness];

		*die_average *= (100 + i);
	}

	/* Deadliness is negative - damage is decreased */
	else
	{
		i = deadliness_conversion[ABS(deadliness)];

		if (i >= 100) *die_average = 0;
		else *die_average *= (100 - i);
	}
}


/*
 * Player attacks a (poor, defenseless) creature in melee.
 * -RAK-, -BEN-, -LM-
 *
 * Handle various bonuses, and try for a shield bash.
 *
 * (What happens when the character hits a monster)
 *      Critical hits may increase the number of dice rolled.  The Deadliness
 * percentage may affect the average value of each die (this produces the
 * same effect as simply multiplying the damage, but produces a smoother
 * graph of possible damages).
 *      Once both the number of dice and the dice sides are adjusted, they
 * are rolled.  Any special adjustments to damage (like those from slays)
 * are then applied.
 *
 * Example:
 * 1) Dagger (1d4):
 *    1 die with four sides  (average damage is 2.5)
 * 2) Gets a small critical hit for +2 damage dice:
 *    3 dice, each with four sides (average damage is 7.5)
 * 4) Player has a Deadliness value of +72, which gives a bonus of 200%:
 *    average damage is tripled   (average damage is 22.5)
 * 5) Dagger is a weapon of Slay Orc, and monster is an orc:
 *    bonus against orcs is 10, so weapon does 32.5 damage on average
 *
 * If a weapon is not wielded, characters use whatever bare-handed combat
 * method they have chosen.
 *
 * Successful hits may induce various special effects, including earthquakes,
 * confusion blows, monsters panicking, and so on.
 *
 * (Handling multiple weapons)
 *      For each legal blow, we try all the available weapons in order.  As
 * soon as one hits, we go to the next blow without processing any remaining
 * weapons.  We continue in this fashion until the blow count exceeds that
 * of all weapons.  Wielding more than one weapon is very powerful because
 * the chances of hitting are greatly increased.  Because the character
 * does NOT get to hit with each weapon each blow, multiple weapons are no
 * longer guaranteed to be unbalanced.
 */
bool py_attack(int y, int x)
{
	int i;

	/* Damage */
	int damage;

	/* Number of weapons */
	int num_weapons;

	/* blow count */
	int blows = 0;

	/* hit count */
	int hits = 0;

	/* Bonus to attack if monster is sleeping */
	int sleeping_bonus = 0;

	/* Terrain adjustments to effective monster AC */
	int terrain_adjust = 0;

	/* Skill */
	int bonus, chance;

	/* Weapon skill */
	int skill = get_skill(sweapon(inventory[INVEN_WIELD].tval), 0, 100);

	/* Assume no special attack effects */
	bool bare_handed = FALSE;
	bool do_throw = FALSE;
	bool do_conf = FALSE;
	bool do_slow = FALSE;
	bool do_stun = FALSE;
	bool impact = FALSE;
	bool dead = FALSE;
	bool stop = FALSE;
	bool offhand = FALSE;
	int do_force_back = 0;
	u16b combat_mods = 0;

	u32b f1, f2, f3;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	object_type *o_ptr;

	char m_name[DESC_LEN];

	bool fear = FALSE;

	int resist;


	/* Get the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];


	/* Just push past town monsters */
	if (m_ptr->mflag & (MFLAG_TOWN))
	{
		/* Get monster name (or "something") */
		monster_desc(m_name, m_ptr, 0x44);

		/* Message */
		if (r_ptr->flags3 & (RF3_ANIMAL))
		{
			msg_format("%^s scampers out of your way.", m_name);
		}
		else
		{
			msg_format("You push past %s.", m_name);
		}

		return (FALSE);
	}


	/* Reveal mimics (note: mimics cannot be sneak-attacked) */
	if ((m_ptr->mflag & (MFLAG_MIME)) && (m_ptr->ml))
	{
		/* Reveal the monster */
		m_ptr->mflag &= ~(MFLAG_MIME);

		/* Get monster name ("a kobold") */
		monster_desc(m_name, m_ptr, 0xC8);

		/* Message */
		if (!p_ptr->afraid)
		{
			msg_format("You find yourself fighting %s!", m_name);
		}
		else
		{
			char m_pronoun[DESC_LEN];

			/* Get monster pronoun ("him", "her", "it") */
			monster_desc(m_pronoun, m_ptr, 0x31);

			msg_format("%^s appears, but you are too frightened to fight %s!",
				m_name, m_pronoun);
			return (TRUE);
		}
	}

	/* Character is not blind, confused, or hallucinating */
	else if ((!p_ptr->blind) && (!p_ptr->confused) && (!p_ptr->image))
	{
		/* Monster is fully visible */
		if (mon_fully_visible(m_ptr))
		{
			/* If the monster is sleeping, it can be hit more easily. */
			if (m_ptr->csleep)
			{
				sleeping_bonus = get_skill(S_BURGLARY, 5, 45);

				/* If the character is sneaking, he may get sneak attacks */
				if ((p_ptr->sneaking) && (!(m_ptr->mflag & (MFLAG_ACTV))) &&
				    (get_skill(S_BURGLARY, 0, 100) > rand_int(80)))
				{
					p_ptr->special_attack |= (ATTACK_SNEAK);
				}
			}

			/* If the monster is running away, it can be backstabbed. */
			else if (m_ptr->min_range > MAX_SIGHT)
			{
				sleeping_bonus = get_skill(S_BURGLARY, 0, 50);
			}
		}
	}

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml >= ML_FULL) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);


	/* Get monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x40);

	/* Handle player fear */
	if (p_ptr->afraid)
	{
		if (m_ptr->ml)
		{
			/* Message */
			msg_format("You are too afraid to attack %s!", m_name);

			/* You already knew this */
			p_ptr->energy_use = 0;
		}
		else
		{
			/* Special Message */
			msg_print("Something scary is in your way!");
		}

		/* Done */
		return (TRUE);
	}


	/* Apply terrain effects */
	terrain_adjust = terrain_ac_adjust(r_ptr, cave_feat[y][x]);


	/* No shield on arm, no bash.  */
	if ((inventory[INVEN_ARM].k_idx) &&
	    (inventory[INVEN_ARM].tval == TV_SHIELD) &&
	    (!p_ptr->shield_on_back))
	{
		/*
		 * Players do not bash if they could otherwise take advantage of
		 * special bonuses, or if the monster is low-level or not visible.
		 */
		if ((!sleeping_bonus) && (!p_ptr->special_attack) &&
		    (m_ptr->ml) && (r_ptr->level >= p_ptr->max_depth / 4))
		{
			/* Attempt a shield bash sometimes */
			if (shield_bash(y, x, r_ptr, m_ptr, &blows, &fear)) return (TRUE);
		}
	}


	/* Martial arts cannot be used in some situations */
	if (p_ptr->barehanded)
	{
		/* Immaterial beings cannot be hurt with unblessed bare hands */
		if ((monster_immaterial(r_ptr)) &&
		    (!p_ptr->blessed) && (!p_ptr->holy))
		{
			/* Monster cannot be seen -- allow movement (sometimes) */
			if (!m_ptr->ml)
			{
				msg_print("You sense a presence, but can neither see nor touch it.");
				disturb(0, 0);
				return (FALSE);
			}

			/* Monster can be seen -- explain why nothing happened */
			else
			{
				/* Message */
				msg_format("You cannot harm %s with martial arts unless blessed.", m_name);

				/* If you already knew this, don't take a turn */
				if (l_ptr->flags2 & (RF2_PASS_WALL)) p_ptr->energy_use = 0;

				/* Learn about the monster */
				l_ptr->flags2 |= (RF2_PASS_WALL);
			}

			/* No fight */
			return (TRUE);
		}

		/* One "weapon" */
		num_weapons = 1;

		/* Barehanded combat */
		bare_handed = TRUE;
	}

	/* Count weapons, and determine if we are fighting bare-handed */
	else
	{
		/* Assume no weapons */
		num_weapons = 0;

		/* Check first arm */
		o_ptr = &inventory[INVEN_WIELD];

		/* Count main weapon */
		if (is_melee_weapon(o_ptr)) num_weapons++;

		/* Check second arm */
		o_ptr = &inventory[INVEN_ARM];

		/* Count secondary weapon */
		if (is_melee_weapon(o_ptr)) num_weapons++;

		/* No weapons -- we're fighting bare-handed. */
		if (num_weapons == 0)
		{
			num_weapons = 1;
			bare_handed = TRUE;
		}
	}

	/* Note if we have a hungry weapon */
	if (p_ptr->soulsteal) p_ptr->feed_weapon = TRUE;


	/* Attack until the monster is dead or we have to stop. */
	while (!dead && !stop)
	{
		/* Assume a miss */
		bool hit = FALSE;
		bool hurt = FALSE;
		damage = 0;

		/* Use another blow */
		blows++;

		/* We've used up all our blows. */
		if (blows > MAX(p_ptr->num_blow, p_ptr->num_blow2))
		{
			break;
		}

		/* Shorten unique name for blows after the first */
		if ((blows == 2) && (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			short_m_name(m_name);
		}

		/* Try each of the weapons being wielded in turn. */
		for (i = 1; i <= num_weapons; i++)
		{
			/* Get the weapon (if any) */
			if (i == 1)
			{
				/* No more blows allowed with this weapon */
				if (blows > p_ptr->num_blow) continue;

				/* Check first arm -- primary weapon must be in this slot */
				o_ptr = &inventory[INVEN_WIELD];

				offhand = FALSE;
			}
			else if (i == 2)
			{
				/* No more blows allowed with this weapon */
				if (blows > p_ptr->num_blow2) continue;

				/* Check second arm -- require a weapon */
				o_ptr = &inventory[INVEN_ARM];
				if (!is_melee_weapon(o_ptr)) continue;

				offhand = TRUE;
			}

			/* Characters cannot wield more than two weapons. */
			else continue;

			/* Monster is dead */
			if (dead) break;


			/*** Attack with this weapon (or bare hands) ***/

			/* Calculate the weapon bonus (if any) */
			if (bare_handed) bonus = 0;
			else             bonus = o_ptr->to_h;

			/* Calculate the attack quality. */
			if (!offhand) chance = p_ptr->skill_thn + BTH_PLUS_ADJ * bonus;
			else chance = p_ptr->skill_thn2 + BTH_PLUS_ADJ * bonus;

			/* This blow missed */
			if (!test_hit_combat(chance + sleeping_bonus,
			         r_ptr->ac + terrain_adjust, m_ptr->ml))
			{
                learn_about_hits(0, offhand);
				continue;
			}
			else learn_about_hits(1, offhand);

			/* Monster evaded or resisted */
			resist = monster_evade_or_resist(o_ptr, m_ptr, BLOW_MELEE);

			/* Monster can evade */
			if (resist == 100) continue;

			/* Practice the melee skill */
			skill_being_used = sweapon(o_ptr->tval);

			/* Character is wielding a weapon */
			if (is_melee_weapon(o_ptr))
			{
				char dummy[DESC_LEN];

				/* Get object flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Note impact weapon */
				if (f3 & (TR3_IMPACT)) impact = TRUE;

				/* Calculate the damage */
				damage = calc_combat_dam(COMBAT_MELEE, o_ptr, m_ptr,
					m_name, p_ptr->to_d + o_ptr->to_d,
					chance + sleeping_bonus, dummy, combat_mods);
			}

			/* Character is fighting bare-handed (first weapon only) */
			else if (i == 1)
			{
				damage = py_attack_barehand(chance, m_ptr, m_name);
			}
			else
			{
				/* No attack */
				continue;
			}

			/* Resisted */
			if (resist) damage -= (damage * resist + 50) / 100;

			/* Count hits */
			hits++;
			hit = TRUE;

			/* Sound */
			sound(MSG_HIT);

			/* If this is the first hit, make some noise. */
			if (hits == 1)
			{
				/* Noise partly depends on stealth */
				int noise = p_ptr->base_wakeup_chance / 2;

				/* Noise greatly depends on burglary skill */
				noise += get_combat_noise(0, 800);

				/* Characters fighting barehanded make less noise */
				if (bare_handed) noise /= 2;

				/* Increase the noise level */
				add_wakeup_chance += noise;
			}

			/* If fighting with bare hands, check for contact danger */
			if (bare_handed)
			{
				/* Note if the character got hurt */
				hurt = contact_danger_check(r_ptr);
			}

			/* Paranoia -- No negative damage */
			if (damage < 0) damage = 0;

			/* Learn about damage */
			learn_about_damage(damage, offhand);

			/* Damage, check for fear and death. */
			if (mon_take_hit(cave_m_idx[y][x], -1, damage, &fear, NULL))
			{
				/*
				 * Hack -- High-level warriors and burglars can spread their
				 * attacks out among weaker foes.
				 */
				if ((p_ptr->oath & (OATH_OF_IRON | BURGLARS_GUILD)) &&
					 (skill >= rand_range(60, 80)) &&
					 (blows < MAX(p_ptr->num_blow, p_ptr->num_blow2)) &&
					 (p_ptr->energy_use))
				{
					/* Use energy only for blows expended */
					p_ptr->energy_use = p_ptr->energy_use * blows /
						MAX(p_ptr->num_blow, p_ptr->num_blow2);
				}

				/* Fight's over. */
				dead = TRUE;
				break;
			}


			/* Confusion attack */
			if ((p_ptr->special_attack & (ATTACK_CONFUSE)) &&
				 (!m_ptr->confused))
			{
				/* Notice resistance */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml >= ML_FULL)
					{
						l_ptr->flags3 |= (RF3_NO_CONF);
						msg_format("%^s is unaffected.", m_name);
					}
				}

				/* Allow saving throw */
				else if (rand_int(p_ptr->power + 25) <
							r_ptr->level + randint(10))
				{
					if (m_ptr->ml >= ML_FULL)
					{
						msg_format("%^s shakes off the confusion.", m_name);
					}
				}

				/* Confuse the monster (never cumulative) */
				else
				{
					if (m_ptr->ml >= ML_FULL)
					{
						message_format(MSG_HIT, 0, "%^s appears confused.", m_name);
					}

					m_ptr->confused = 10;
				}

				/* Sometimes cancel attack */
				if (one_in_(3))
				{
					/* Message */
					message(MSG_HIT, 0, "Your hands stop glowing.");

					/* Cancel special confusion attack */
					p_ptr->special_attack &= ~(ATTACK_CONFUSE);

					/* Redraw the state */
					p_ptr->redraw |= (PR_STATE);
				}
			}

			/* Black Breath attack */
			if (p_ptr->special_attack & (ATTACK_BLKBRTH))
			{
				/* The undead are immune */
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					/* Learn about visible monster */
					if (m_ptr->ml >= ML_FULL)
					{
						l_ptr->flags3 |= (RF3_UNDEAD);
					}

					msg_format("%^s is immune!", m_name);
				}

				/* All other monsters get a saving throw. */
				else if ((rand_int(160)) < (r_ptr->level + rand_int(60)))
				{
					if (m_ptr->ml >= ML_FULL)
					{
						msg_format("%^s wards off your deadly blow.", m_name);
					}
				}

				/* Tasting some of their own medicine... */
				else if (!(m_ptr->mflag & (MFLAG_BLBR)))
				{
					m_ptr->mflag |= (MFLAG_BLBR);

					if (m_ptr->ml >= ML_FULL)
					{
						message_format(MSG_HIT, 0, "%^s is stricken with the Black Breath!", m_name);
					}
				}

				/* Cancel black breath */
				p_ptr->special_attack &= ~(ATTACK_BLKBRTH);

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);

				/* Message */
				msg_print("Your hands stop radiating Night.");
			}

			/* Burglar's "Hit and Run" attack */
			if (p_ptr->special_attack & (ATTACK_FLEE))
			{
				/* Flee after a random number of blows */
				if (blows >= rand_int(MAX(p_ptr->num_blow, p_ptr->num_blow2)))
				{
					/* Message */
					msg_print("You escape into the shadows!");

					/* Teleport. */
					teleport_player(get_skill(S_BURGLARY, 6, 12), TRUE, FALSE);

					/* Cancel the fleeing spell */
					p_ptr->special_attack &= ~(ATTACK_FLEE);

					/* Redraw the state */
					p_ptr->redraw |= (PR_STATE);

					/* Stop attacking */
					stop = TRUE;
				}
			}

			/* We successfully landed a blow.  No need to try other weapons. */
			break;
		}

		/* End of weapon sequence for this blow */

		/* Monster is no longer asleep */
		sleeping_bonus = 0;


		/* This blow missed. */
		if (hit == FALSE)
		{
			/* Sound */
			sound(MSG_MISS);

			/* Message */
			message_format(MSG_MISS, 0, "You miss %s.", m_name);
		}

		/*
		 * Monsters can be thrust back by blows from an impact weapon.
		 * The first time you hit a monster, you have a chance to thrust
		 * it back.
		 */
		if ((impact) && (hits == 1) && (damage > 0))
		{
			/* Minimum chance is 25%, and it rises for powerful hits */
			int tmp = 25 + (100 * damage / MAX(1, m_ptr->maxhp));

			/* Force the monster back (later) */
			if (tmp > rand_int(100))
			{
				do_force_back = damage;

				/* Sometimes you can keep hitting, sometimes you can't. */
				if (one_in_(3)) break;
			}
		}

		/*
		 * If the character got hurt attacking a monster, he
		 * automatically pauses to let the player re-assess.
		 */
		if (hurt)
		{
			/* Hack -- Recover unused energy */
			if ((blows < MAX(p_ptr->num_blow, p_ptr->num_blow2)) &&
				(p_ptr->energy_use))
			{
				p_ptr->energy_use = p_ptr->energy_use * blows /
					MAX(p_ptr->num_blow, p_ptr->num_blow2);
			}

			/* Stop attacking */
			break;
		}
	}

	/* Cancel sneak attacks */
	p_ptr->special_attack &= ~(ATTACK_SNEAK);

	/* Disturb the monster (now it is able to react) */
	m_ptr->csleep = 0;

	/* Disturb the player */
	disturb(0, 0);


	/* Monster is still alive */
	if (!dead)
	{
		/*
		 * Wrestling throw. Not effective against immaterial
		 * or immobile monsters.
		 */
		if ((do_throw) && !((r_ptr->flags1 & (RF1_NEVER_MOVE)) ||
		                    (monster_immaterial(r_ptr))))
		{
			int e_loss;

			/* Message  XXX */
			msg_format("You throw %s to the ground!", m_name);

			/* The monster loses some energy while getting up */
			e_loss = rand_range(skill / 3, skill * 2 / 3);

			/* Lose energy */
			mon_adjust_energy(m_ptr, -e_loss);
		}

		/* Karate -- Attempt a confusion attack */
		if ((do_conf) && (!m_ptr->confused))
		{
			/* Note resistance */
			if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_NO_CONF);
				}
			}

			/* Confuse the monster */
			else if (rand_int(skill + 30) >= r_ptr->level / 2 + 20)
			{
				message_format(MSG_HIT, 0, "%^s appears confused.", m_name);
				m_ptr->confused += rand_range(2, 2 + skill / 7);
			}
		}

		/* Karate -- Attempt a slowing attack (not cumulative) */
		if ((do_slow) && (m_ptr->mspeed >= r_ptr->speed - 8))
		{
			/* Note resistance */
			if (r_ptr->flags3 & (RF3_NO_SLEEP))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_NO_SLEEP);
				}
			}

			/* Slow the monster */
			else if (rand_int(skill + 30) >= r_ptr->level / 2 + 20)
			{
				if (!m_ptr->slowed)
				{
					m_ptr->slowed = 5;
					message_format(MSG_HIT, 0, "%^s is hindered.", m_name);
				}
			}
		}

		/* Karate -- Attempt a stunning attack (not cumulative) */
		if ((do_stun) && (!m_ptr->stunned))
		{
			/* Note resistance */
			if (r_ptr->flags3 & (RF3_NO_STUN))
			{
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_NO_STUN);
				}
			}

			/* Stun the monster */
			else if (rand_int(skill + 30) >= r_ptr->level / 2 + 20)
			{
				message_format(MSG_HIT, 0, "%^s is stunned.", m_name);
				m_ptr->stunned += rand_range(3, 3 + skill / 6);
			}
		}


		/* We thrust away a monster */
		if (do_force_back)
		{
			int k = do_force_back;

			/* Force breathers are immune */
			if (!(r_ptr->flags4 & (RF4_BRTH_FORCE)))
			{
				/* Big, heavy monsters */
				if (strchr("DGP#X", r_ptr->d_char)) k /= 3;
				else if (strchr("OTdgv&", r_ptr->d_char)) k /= 2;

				/* Thrust away */
				thrust_away(-1, m_ptr->fy, m_ptr->fx, MIN(3, 1 + k / 15));
			}
		}


		/* Hack -- delayed fear messages */
		if (fear && m_ptr->ml)
		{
			/* Sound */
			sound(MSG_FLEE);

			/* Message */
			message_format(MSG_FLEE, 0, "%^s flees in terror!", m_name);
		}
	}


	/* Handle various things if we landed a blow */
	if (hits)
	{
		/* Check first arm */
		o_ptr = &inventory[INVEN_WIELD];

		/* Wielding a weapon */
		if ((o_ptr->k_idx) && (is_melee_weapon(o_ptr)))
		{
			/* Sometimes learn more about un-IDed wielded objects */
			if (one_in_(10))
			{
				learn_about_wearable(o_ptr, INVEN_WIELD, FALSE);
			}
		}

		/* Check second arm */
		o_ptr = &inventory[INVEN_ARM];

		/* Wielding a weapon */
		if ((o_ptr->k_idx) && (is_melee_weapon(o_ptr)))
		{
			/* Sometimes learn more about un-IDed wielded objects */
			if (one_in_(10))
			{
				learn_about_wearable(o_ptr, INVEN_ARM, FALSE);
			}
		}
	}

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

	/* Return */
	return (TRUE);
}



/*
 * Transfer launcher attributes to missile.
 */
void transfer_attributes_to_missile(object_type *i_ptr,
	const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Get launcher attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Transfer all the flags */
	i_ptr->flags1 |= (f1);
	i_ptr->flags2 |= (f2);
	i_ptr->flags3 |= (f3);

	/* We could also have launchers that took away attributes... */
}


/*
 * Calculate the projection path for fired or thrown objects.  -LM-
 *
 * Apply variable inaccuracy, using angular comparison.
 *
 * If inaccuracy is non-zero, we reduce corridor problems by reducing
 * range instead of immediately running the projectile into the wall.
 * This favour is not extended to projection sources out in the open,
 * which makes it much better to be aiming from a firing slit than into
 * one.
 */
static void calc_ranged_path(int range, int y0, int x0, int *ty, int *tx,
	int dir, int inaccuracy)
{
	int reduce, expected_distance;
	int dy, dx, delta, angle;
	int mult = 1;


	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		*ty = p_ptr->target_row;
		*tx = p_ptr->target_col;
	}

	/* We have a legal compass direction */
	else if ((dir > 0) && (dir < 10) && (dir != 5))
	{
		/* Target ten grids away in this direction */
		*ty = y0 + 10 * ddy[dir];
		*tx = x0 + 10 * ddx[dir];
	}


	/* No inaccuracy or no distance -- calculate path and return */
	if ((inaccuracy <= 0) || ((*ty == y0) && (*tx == x0)))
	{
		(void)project_path(range, y0, x0, ty, tx, PROJECT_THRU);
		return;
	}

	/* Inaccuracy penalizes range */
	reduce = rand_int(1 + inaccuracy / 2);
	if (reduce > range / 3) reduce = range / 3;
	range -= reduce;

	/* Get distance to target */
	dy = *ty - y0;
	dx = *tx - x0;
	delta = MAX(ABS(dy), ABS(dx));


	/* Extend target away from source, if necessary */
	if ((delta > 0) && (delta <= 7))
	{
		if      (delta == 1) mult = 10;
		else if (delta == 2) mult = 5;
		else if (delta == 3) mult = 4;
		else if (delta == 4) mult = 3;
		else                 mult = 2;

		*ty = y0 + (dy * mult);
		*tx = x0 + (dx * mult);
	}

	/* Note whether we expect the projectile to travel anywhere */
	expected_distance = project_path(range, y0, x0, ty, tx, PROJECT_THRU);

	/* Path enters no grids except the origin -- return */
	if (expected_distance < 2) return;


	/* Continue until satisfied */
	while (TRUE)
	{
		int ty2 = *ty;
		int tx2 = *tx;

		/* Get angle to this target */
		angle = get_angle_to_target(y0, x0, ty2, tx2, 0);

		/* Inaccuracy changes the angle of projection */
		angle = Rand_normal(angle, inaccuracy);

		/* Handle the 0-240 angle discontinuity */
		if (angle < 0) angle = 240 + angle;

		/* Get a grid using the adjusted angle, target it */
		get_grid_using_angle(angle, y0, x0, &ty2, &tx2);

		/* Calculate the path, accept if it enters at least two grids */
		if (project_path(range, y0, x0, &ty2, &tx2, PROJECT_THRU) > 1)
		{
			break;
		}

		/* Otherwise, reduce range and expected distance */
		else
		{
			range -= rand_int(div_round(range, 4));
			if (expected_distance > range) expected_distance = range;
		}
	}
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that match your missile launcher.
 *
 * Project the missile along the path from player to target.
 * Take terrain and sleeping bonuses into account, make noise, and check
 * for a hit.
 *
 * The basic damage calculations in archery are the same as in melee,
 * except that a launcher multiplier is also applied to the dice sides.
 *
 * Apply any special attack or class bonuses, and check for death.
 * Drop the missile near the target (or end of path), sometimes breaking it.
 */
void do_cmd_fire(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, y, x, ty, tx;
	int chance, tdis;

	int break_chance;

	int armor, bonus = 0, total_deadliness;

	int sleeping_bonus = 0;
	int inaccuracy;

	int damage;

	/* Assume no special bonuses */
	int special_pierce = 0;
	int special_dam = 0;
	int special_impact = 0;
	int special_hit = 0;
	int special_deadly = 0;
	u16b combat_mods = 0;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type *j_ptr;
	object_type *backup_i_ptr;
	object_type object_type_body;
	object_type object_type_body2;

	u32b f1, f2, f3;

	int hit_body = 0;
	bool hit_wall = FALSE;
	bool impact = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[DESC_LEN];
	char m_name[DESC_LEN];
	char msg_hit[DESC_LEN];

	bool no_pile = FALSE;
	int resist;

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get the "bow" (if any) */
	o_ptr = &inventory[INVEN_BOW];

	/* Require a usable launcher */
	if (!o_ptr->tval || !p_ptr->ammo_tval)
	{
		msg_print("You have no missile launcher equipped.");
		return;
	}

	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Get an item */
	q = "Fire which item?";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR)))
		return;
	item_to_object(j_ptr, item);

	/* Strength increases distance, weight reduces it */
	tdis = adj_str_blow[p_ptr->stat_ind[A_STR]] * 8 /
		((j_ptr->weight > 3) ? j_ptr->weight : 3);

	/* Max distance depends on skill */
	if (tdis > 12 + get_skill(sbow(o_ptr->tval), 0, 8))
	    tdis = 12 + get_skill(sbow(o_ptr->tval), 0, 8);

	/* Hack -- set maximum distance for use in target_able */
	p_ptr->max_dist = tdis;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Use some energy ("p_ptr->num_fire" has a standard value of 2) */
	p_ptr->energy_use = div_round(200, p_ptr->num_fire);

	/* Sound */
	sound(MSG_SHOOT);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the missile */
	object_copy(i_ptr, j_ptr);

	/* Note a single object */
	if (j_ptr->number == 1) no_pile = TRUE;

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		use_item_describe(item, USE_ITEM_DESC_FIRE);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		use_item_describe(item, USE_ITEM_DESC_FIRE);
		floor_item_optimize(0 - item);
	}

	/* Get local object */
	backup_i_ptr = &object_type_body2;

	/* Save a backup copy of the fired missile */
	object_copy(backup_i_ptr, i_ptr);

	/* Hack -- transfer launcher attributes to the missile */
	transfer_attributes_to_missile(i_ptr, o_ptr);


	/* Fire ammo of backbiting, and it will turn on you.  -LM- */
	if (i_ptr->ego_item_index == EGO_BACKBITING)
	{
		/* Calculate damage. */
		damage = damroll(p_ptr->ammo_mult * i_ptr->dd * randint(2),
		                 i_ptr->ds * 4);
		damage += special_dam;

		/* Inflict both normal and wound damage. */
		if (!take_hit(damage, 0, "Your missile turns in midair and strikes you!",
		   "ammo of backbiting"))
		{
			set_cut(randint(damage * 3));
		}

		/* That ends that shot! */
		return;
	}

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Shots are usually not perfectly accurate */
	inaccuracy = 5 - get_skill(sbow(o_ptr->tval), 0, 5);

	/* Cursed ammo can escape from your control  -clefs- */
	if (cursed_p(i_ptr))
	{
		/* Warn the player */
		msg_print("Your finger slips!");

		/* Note the curse */
		i_ptr->ident |= (IDENT_SENSE);
		if ((i_ptr->inscrip != INSCRIP_VERY_CURSED) &&
		    (i_ptr->inscrip != INSCRIP_WORTHLESS) &&
		    (i_ptr->inscrip != INSCRIP_TERRIBLE))
		{
			i_ptr->inscrip = INSCRIP_CURSED;
		}

		/* Randomize target */
		inaccuracy += 20;
	}

	/* Cursed launchers are tricky to use */
	else if (cursed_p(o_ptr))
	{
		char weapon_name[DESC_LEN];

		/* Get the short name of the missile weapon */
		strip_name(weapon_name, o_ptr->k_idx);

		/* Warn the player */
		msg_format("Your %s jerks suddenly!", weapon_name);

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
		if ((o_ptr->inscrip != INSCRIP_VERY_CURSED) &&
		    (o_ptr->inscrip != INSCRIP_WORTHLESS) &&
		    (o_ptr->inscrip != INSCRIP_TERRIBLE))
		{
			o_ptr->inscrip = INSCRIP_CURSED;
		}

		/* Randomize target */
		inaccuracy += 10;
	}

	/* Handle various special attacks  -LM- */
	else if (p_ptr->special_attack)
	{
		/* Piercing shot */
		if (p_ptr->special_attack & (ATTACK_PIERCING))
		{
			special_pierce = TRUE;
			p_ptr->special_attack &= ~(ATTACK_PIERCING);
		}

		/* Deadly shot */
		if (p_ptr->special_attack & (ATTACK_DEADLY))
		{
			special_deadly = TRUE;
			p_ptr->special_attack &= ~(ATTACK_DEADLY);
		}

		/* Damaging shot */
		if (p_ptr->special_attack & (ATTACK_DAMAGE))
		{
			special_dam = TRUE;
			p_ptr->special_attack &= ~(ATTACK_DAMAGE);
		}

		/* Impact shot */
		if (p_ptr->special_attack & (ATTACK_IMPACT))
		{
			impact = special_impact = TRUE;
			p_ptr->special_attack &= ~(ATTACK_IMPACT);
		}

		/* Accurate shot */
		if (p_ptr->special_attack & (ATTACK_ACCURATE))
		{
			special_hit = TRUE;
			inaccuracy /= 2;
			p_ptr->special_attack &= ~(ATTACK_ACCURATE);
		}

		/* Flaming shot */
		if (p_ptr->special_attack & (ATTACK_SHOT_FIRE))
		{
			i_ptr->flags1 |= (TR1_BRAND_FIRE);
			p_ptr->special_attack &= ~(ATTACK_SHOT_FIRE);
		}

		/* Freezing shot */
		if (p_ptr->special_attack & (ATTACK_SHOT_COLD))
		{
			i_ptr->flags1 |= (TR1_BRAND_COLD);
			p_ptr->special_attack &= ~(ATTACK_SHOT_COLD);
		}

		/* The Longbow of Bard does terrible things to dragons */
		if (p_ptr->special_attack & (ATTACK_BARD))
		{
			/* Get the targeted monster (if any) */
			if (p_ptr->health_who)
			{
				monster_type *m_ptr = &m_list[p_ptr->health_who];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* We're deliberately aiming at a dragon */
				if (r_ptr->flags3 & (RF3_DRAGON))
				{
					msg_print("\"Go now, and speed true!\"");
				}
			}

			/* Does nasty to dragons */
			i_ptr->flags1 |= (TR1_KILL_DRAGON);

			/* Hits very often */
			inaccuracy = 0;
			bonus += 20;

			/* Cancel the special attack */
			p_ptr->special_attack &= ~(ATTACK_BARD);
		}

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Get (adjusted) missile attributes */
	object_flags(i_ptr, &f1, &f2, &f3);

	/* Get the color and symbol for the fired object */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Learn some flags */
	if (f3 & (TR3_IMPACT)) impact = TRUE;


	/* Calculate the quality of the shot */
	bonus = (o_ptr->to_h + i_ptr->to_h);
	chance = p_ptr->skill_thb + (BTH_PLUS_ADJ * bonus);

	/* Sum all the applicable additions to Deadliness. */
	total_deadliness = p_ptr->to_d + o_ptr->to_d + i_ptr->to_d;


	/* Start at the player */
	y = py;
	x = px;

	/* Clear hit message */
	strcpy(msg_hit, "");

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Calculate path of projection */
	calc_ranged_path(tdis, py, px, &ty, &tx, dir, inaccuracy);

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Delay depends on length of path */
		int m = (path_n <= 4 ? msec : (path_n <= 8 ? msec*2/3 : msec/2));


		/* Collide with anything non-projectable */
		if (!cave_project_bold(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_or_infra_bold(y, x) &&
		    (path_gx[i] != PATH_G_NONE))
		{
			/* Full delay if monster in way */
			if (cave_m_idx[y][x] != 0) m = msec;

			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			(void)Term_fresh();
			pause_for(m);
			lite_spot(y, x);
			(void)Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistency */
			pause_for(m);
		}

		/* This grid is ignored by the projection */
		if (path_gx[i] >= PATH_G_NONE)
		{
			/* If no monster, we just fly by */
			if (!cave_m_idx[y][x]) continue;

			/* Otherwise, we are blocked  XXX XXX */
			else break;
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			bool fear = FALSE;

			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0x40);

			/* Character is not blind, confused, or hallucinating */
			if ((!p_ptr->blind) && (!p_ptr->confused) && (!p_ptr->image))
			{
				/* Monster is visible */
				if (m_ptr->ml)
				{
					/* If the monster is sleeping, it can be hit more easily. */
					if (m_ptr->csleep)
					{
						/* Bonus goes up more and more rapidly with burglary skill */
						int max_bonus  = get_skill(S_BURGLARY, 25, 45);
						sleeping_bonus = get_skill(S_BURGLARY, 5, max_bonus);
					}
				}
			}

			/* Apply effects of terrain to AC of monster. */
			armor = r_ptr->ac + terrain_ac_adjust(r_ptr, cave_feat[y][x]);

			/* Weapons of velocity sometimes almost negate monster armor. */
			if (special_hit) armor /= 3;

			/* Deadly shots hit more often */
			if (special_deadly) armor = armor * 2 / 3;

			/* Did we hit it? */
			if (!test_hit_combat(chance + sleeping_bonus, armor, m_ptr->ml))
			{
				/* Object whizzes right past the monster */
				continue;
			}

			/* Monster evaded or resisted */
			resist = monster_evade_or_resist(i_ptr, m_ptr, BLOW_MISSILE);

			/* Evaded */
			if (resist == 100)	continue;

			/* Note the collision */
			hit_body++;

			/* Make some noise. */
			if (TRUE)
			{
				/* Noise partly depends on stealth */
				int noise = p_ptr->base_wakeup_chance / 2;

				/* Noise greatly depends on burglary skill */
				noise += get_combat_noise(0, 800);

				/* Increase the noise level */
				add_wakeup_chance += noise;
			}

			/* Reveal fully visible mimics */
			if ((m_ptr->mflag & (MFLAG_MIME)) && (m_ptr->ml >= ML_FULL))
			{
				/* Reveal the monster */
				m_ptr->mflag &= ~(MFLAG_MIME);

				/* Get monster name again */
				monster_desc(m_name, m_ptr, 0xC8);

				/* Message  XXX */
				msg_print("It was a mimic!");
			}

			/* Hack -- Track this monster race, if monster is visible */
			if (m_ptr->ml >= ML_FULL) monster_race_track(m_ptr->r_idx);

			/* Hack -- Track this monster, if visible */
			if (m_ptr->ml) health_track(cave_m_idx[y][x]);

			/* Practice the shooting skill */
			skill_being_used = sbow(o_ptr->tval);


			/* Special damage bonuses */
			if (special_dam)    combat_mods |= (DAM_ADJUST_DAM);
			if (special_deadly) combat_mods |= (DAM_ADJUST_DEADLY);

			/* Calculate the damage */
			damage = calc_combat_dam(COMBAT_FIRE, i_ptr, m_ptr,
				m_name, total_deadliness, chance + sleeping_bonus,
				msg_hit, combat_mods);

			/* Damage was resisted */
			if (resist)		damage -= (damage * resist + 50) / 100;

			/* Hack! -- display hit messages before monster dies  XXX */
			if ((damage > m_ptr->hp) && (strlen(msg_hit)))
				message(MSG_HIT, 0, format("%s.", msg_hit));

			/* Hit the monster, check for death. */
			if (mon_take_hit(cave_m_idx[y][x], -1, damage, &fear, death_string(r_ptr)))
			{
				/* Dead monster */
			}

			/* No death */
			else
			{
				/* Message */
				message_pain(cave_m_idx[y][x], damage, fear, msg_hit);

				/* Monsters can be thrust back by an impact missile. */
				if (impact)
				{
					/* Minimum chance is 40%, and it rises for powerful hits */
					int tmp = 40 + (200 * damage / m_ptr->maxhp);

					/* Special impact hits are guaranteed */
					if (special_impact) tmp = 100;

					/* Force the monster back */
					if (tmp > rand_int(100))
					{
						int k = damage;
						if (special_impact) k *= 4;

						if (!(r_ptr->flags4 & (RF4_BRTH_FORCE)))
						{
							/* Big, heavy monsters (or ghosts) */
							if (strchr("DGP#", r_ptr->d_char)) k /= 3;
							else if (strchr("OTdgv", r_ptr->d_char)) k /= 2;

							/* Thrust away (minimum of 2 spaces) */
							thrust_away(-1, m_ptr->fy, m_ptr->fx, MIN(6, 2 + k / 10));
						}
					}
				}
			}

			/* Allow piercing shots (go through 2 to 5 opponents) */
			if ((special_pierce) && (hit_body < rand_range(2, 5)))
			{
				continue;
			}

			/* Object has hit a monster */
			break;
		}
	}


	/* Chance of breakage (during attacks) */
	break_chance = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Roll for breakage now */
	if ((break_chance) && (rand_int(100) < break_chance)) break_chance = 100;
	else                                                  break_chance =   0;


	/* Chance to learn about the launcher and ammo */
	if (hit_body)
	{
		/* Learn about the launcher */
		if ((o_ptr->k_idx) && (one_in_(10)))
		{
			learn_about_wearable(o_ptr, INVEN_BOW, FALSE);
		}

		/* Learn about the stack of ammo, if any remains */
		if (!no_pile)
		{
			if (one_in_(6))
			{
				/* Learn about the pile */
				learn_about_wearable(j_ptr, -1, FALSE);

				/* Copy updated information to the backup missile */
				object_copy(backup_i_ptr, j_ptr);
				backup_i_ptr->number = 1;
			}
		}

		/* No ammunition remains -- learn about the backup copy */
		else if (!break_chance)
		{
			if (one_in_(6))
			{
				learn_about_wearable(backup_i_ptr, -1, FALSE);
			}
		}
	}


	/* Restore old flags and grant new knowledge to the fired missile */
	object_copy(i_ptr, backup_i_ptr);


	/* The whole pile of missiles has been "worn" (will now stack better) */
	i_ptr->ident |= (IDENT_WORN);
	if (!no_pile) j_ptr->ident |= (IDENT_WORN);

	/* Drop (or break/shatter) near that location */
	drop_near(i_ptr, break_chance, y, x, DROP_HERE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);
}



/*
 * Throw an object from the pack or floor.
 *
 * Now allows for throwing weapons.  Unlike all other thrown objects,
 * throwing weapons can get critical hits and take advantage of bonuses to
 * Skill and Deadliness from other equipped items.
 *
 * You cannot throw cursed objects.
 */
void do_cmd_throw(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, y, x, ty, tx;
	int chance, tdis;

	int break_chance;

	int total_deadliness;
	int sleeping_bonus = 0;
	int terrain_adjust = 0;
	int inaccuracy;
	u16b combat_mods = 0;

	int damage;

	u32b f1, f2, f3;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;
	bool hit_wall = FALSE;
	bool impact = FALSE;
	bool throwing_weapon = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[DESC_LEN];
	char m_name[DESC_LEN];

	cptr q, s;
	char msg_hit[DESC_LEN];

	bool no_pile = FALSE;
	int resist;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;
	int returning = 0;


	/* Get an item */
	q = "Throw which item?";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR)))
		return;
	item_to_object(o_ptr, item);

	/* Cannot throw most equipment items  XXX */
	if ((item == INVEN_NECK) || (item == INVEN_BODY) || (item == INVEN_OUTER) ||
	    (item == INVEN_HEAD) || (item == INVEN_HANDS) || (item == INVEN_FEET) ||
	    (item == INVEN_POUCH))
	{
		/* Can't do it, mister */
		msg_print("You need to take off this item first.");
		return;
	}


	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* You cannot unwield cursed objects by throwing them */
	if ((cursed_cling(o_ptr)) && (item >= INVEN_WIELD))
	{
		/* Get an object kind name */
		strip_name(o_name, o_ptr->k_idx);

		if ((artifact_p(o_ptr)) && (object_known_p(o_ptr)))
			msg_format("The %s clings to your hand, and cannot be thrown!",
			o_name);
		else
			msg_format("Your %s clings to your hand, and cannot be thrown!",
			o_name);

		return;
	}

	/* Check for throwing weapon */
	if (f1 & (TR1_THROWING)) throwing_weapon = TRUE;

	/* Strength increases distance, weight reduces it */
	tdis = adj_str_blow[p_ptr->stat_ind[A_STR]] * 6 / ((o_ptr->weight > 5) ? o_ptr->weight : 5);

	/* Max distance depends on skill */
	if (tdis > 10 + get_skill(S_THROWING, 0, 10))
	    tdis = 10 + get_skill(S_THROWING, 0, 10);

    /* Set max_dist for use in target_able */
    p_ptr->max_dist = tdis;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Distribute the charges of rods/wands between the stacks */
	distribute_charges(o_ptr, i_ptr, 1);

	/* Note last of pile */
	if (o_ptr->number == 1) no_pile = TRUE;

	/* Single object */
	i_ptr->number = 1;


	/* Get object attributes */
	object_flags(i_ptr, &f1, &f2, &f3);


	/* Hack -- Returning weapons are handled later  XXX XXX */
	if (f1 & (TR1_RETURNING))
	{
		returning = 1;
	}

	/* Reduce and describe floor item */
	else if (item >= 0)
	{
		inven_item_increase(item, -1);
		use_item_describe(item, USE_ITEM_DESC_THROW);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		use_item_describe(item, USE_ITEM_DESC_THROW);
		floor_item_optimize(0 - item);
	}


	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 2);



	/* Thrown objects always deviate slightly from their course */
	inaccuracy = 5 - get_skill(S_THROWING, 0, 5);


	/* Note impact object -- impact ammo doesn't work, sorry */
	if (f3 & (TR3_IMPACT))
	{
		if (!is_missile(i_ptr)) impact = TRUE;
	}

	/* Cursed thrown items can escape from your control */
	if (cursed_p(i_ptr) && one_in_(2))
	{
		/* Warn the player */
		msg_print("Your throwing hand jerks suddenly!");

		/* Note the curse */
		i_ptr->ident |= (IDENT_SENSE);
		if ((i_ptr->inscrip != INSCRIP_VERY_CURSED) &&
		    (i_ptr->inscrip != INSCRIP_WORTHLESS) &&
		    (i_ptr->inscrip != INSCRIP_TERRIBLE))
		{
			i_ptr->inscrip = INSCRIP_CURSED;
		}

		/* Randomize target */
		inaccuracy += 20;
	}

	/*
	 * Throwing weapons are harder to use, but often have plusses to Skill.
	 * They can also take advantage of plusses to Deadliness from equipment.
	 */
	if (throwing_weapon)
	{
		chance = p_ptr->skill_tht + (BTH_PLUS_ADJ * i_ptr->to_h);
		total_deadliness = p_ptr->to_d + i_ptr->to_d;

		/* Special case -- Half-Trolls are clumsy with throwing weapons */
		if (p_ptr->prace == RACE_HALF_TROLL)
			chance -= get_skill(S_THROWING, 5, 25);
	}
	else
	{
		chance = (3 * p_ptr->skill_tht / 2) + (BTH_PLUS_ADJ * i_ptr->to_h);
		total_deadliness = i_ptr->to_d;
	}


	/* Get the color and symbol of the thrown object */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Start at the player */
	y = py;
	x = px;

	/* Clear hit message */
	strcpy(msg_hit, "");


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Allow returning weapons to travel out and back */
	start_travel:


	/* Calculate path of projection */
	calc_ranged_path(tdis, py, px, &ty, &tx, dir, inaccuracy);

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Delay depends on length of path */
		int m = (path_n <= 4 ? msec : (path_n <= 8 ? 2*msec/3 : msec/2));

		/* Collide with anything non-projectable */
		if (!cave_project_bold(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_or_infra_bold(y, x) &&
		    (path_gx[i] != PATH_G_NONE))
		{
			/* Full delay if monster in way */
			if (cave_m_idx[y][x] != 0) m = msec;

			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			(void)Term_fresh();
			pause_for(m);
			lite_spot(y, x);
			(void)Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistency */
			pause_for(m);
		}

		/* This grid is ignored by the projection */
		if (path_gx[i] >= PATH_G_NONE)
		{
			/* If no monster, we just fly by */
			if (!cave_m_idx[y][x]) continue;

			/* Otherwise, we are blocked  XXX XXX */
			else break;
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			bool fear = FALSE;

			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0x40);

			/* Character is not blind, confused, or hallucinating */
			if ((!p_ptr->blind) && (!p_ptr->confused) && (!p_ptr->image))
			{
				/* Monster is visible.  Character is using throwing weapons */
				if ((m_ptr->ml >= ML_FULL) && (throwing_weapon))
				{
					/* If the monster is sleeping, it can be hit more easily. */
					if (m_ptr->csleep)
					{
						/* Bonus goes up more and more rapidly with burglary skill */
						int max_bonus  = get_skill(S_BURGLARY, 30, 60);
						sleeping_bonus = get_skill(S_BURGLARY, 10, max_bonus);
					}
				}
			}

			/* Apply terrain effects */
			terrain_adjust = terrain_ac_adjust(r_ptr, cave_feat[y][x]);

			/* Did we hit it? */
			if (!test_hit_combat(chance + sleeping_bonus,
				r_ptr->ac + terrain_adjust, m_ptr->ml))
			{
				/* Object whizzes right past the monster */
				continue;
			}

			/* Monster evaded or resisted */
			resist = monster_evade_or_resist(i_ptr, m_ptr, BLOW_THROWN);

			/* Evaded */
			if (resist == 100) continue;

			/* Note the collision */
			hit_body = TRUE;

			/* Make some noise. */
			if (TRUE)
			{
				/* Noise partly depends on stealth */
				int noise = p_ptr->base_wakeup_chance / 2;

				/* Noise greatly depends on burglary skill */
				noise += get_combat_noise(0, 800);

				/* Increase the noise level */
				add_wakeup_chance += noise;
			}

			/* Reveal fully visible mimics */
			if ((m_ptr->mflag & (MFLAG_MIME)) && (m_ptr->ml >= ML_FULL))
			{
				/* Reveal the monster */
				m_ptr->mflag &= ~(MFLAG_MIME);

				/* Get monster name again */
				monster_desc(m_name, m_ptr, 0xC8);

				/* Message  XXX */
				msg_print("It was a mimic!");
			}

			/* Hack -- Track this monster race, if monster is visible */
			if (m_ptr->ml >= ML_FULL) monster_race_track(m_ptr->r_idx);

			/* Hack -- Track this monster, if visible */
			if (m_ptr->ml) health_track(cave_m_idx[y][x]);

			/* Practice the throwing skill */
			skill_being_used = S_THROWING;


			/* Potions smash */
			if (i_ptr->tval == TV_POTION)
			{
				break;
			}

			/* Ordinary thrown object */
			if (!throwing_weapon)
			{
				/* Store a default hit message. */
				if (m_ptr->ml)
				{
					strcpy(msg_hit, format("The %s hits %s", o_name, m_name));
				}
				else
				{
					strcpy(msg_hit, format("The %s finds a mark", o_name));
				}
			}

			/* Food and mushrooms do special things on impact */

			if (i_ptr->tval == TV_FOOD)
			{
				food_hit_effect(-1, y, x, i_ptr);
				break;
			}


			/* Calculate the damage */
			damage = calc_combat_dam(COMBAT_THROW, i_ptr, m_ptr,
				m_name, total_deadliness, chance + sleeping_bonus,
				msg_hit, combat_mods);

			/* Damage was resisted */
			if (resist)  damage -= (damage * resist + 50) / 100;


			/* Hack! -- display hit messages before monster dies  XXX */
			if ((damage > m_ptr->hp) && (strlen(msg_hit)))
				message(MSG_HIT, 0, format("%s.", msg_hit));

			/* Special case -- The Lightning Lance */
			if (i_ptr->artifact_index == ART_LIGHTNING_LANCE)
			{
				project_beam(-1, tdis, p_ptr->py, p_ptr->px,
				             y, x, damage, GF_ELEC, 0L);
			}

			/* Hit the monster, check for death */
			else if (mon_take_hit(cave_m_idx[y][x], -1, damage,
				&fear, death_string(r_ptr)))
			{
				/* Dead monster */
			}

			/* No death */
			else
			{
				/* Message */
				message_pain(cave_m_idx[y][x], damage, fear, msg_hit);

				/* Monsters can be thrust back by an impact thrown object. */
				if (impact)
				{
					/* Minimum chance is 35%, and it rises for powerful hits */
					int perc = 35 + (125 * damage / m_ptr->maxhp);

					/* Force the monster back */
					if (perc > rand_int(100))
					{
						int k = damage;

						if (!(r_ptr->flags4 & (RF4_BRTH_FORCE)))
						{
							/* Big, heavy monsters */
							if (strchr("DGP#", r_ptr->d_char)) k /= 3;
							else if (strchr("OTdgv", r_ptr->d_char)) k /= 2;

							/* Thrust away */
							if (returning != 2)
								thrust_away(-1, m_ptr->fy, m_ptr->fx, MIN(3, 1 + k / 15));
							/* On the return, thrust toward player! */
							else thrust_toward(-1, m_ptr->fy, m_ptr->fx, MIN(3, 1 + k / 15));
						}
					}
				}
			}

			/* Object has hit a monster -- it falls to the floor */
			break;
		}

		/* Hack -- Returning weapon has returned */
		else if ((cave_m_idx[y][x] < 0) && (returning == 2))
		{
			int chance2 = get_skill(S_THROWING, 50, 95);

			if ((p_ptr->blind) || (no_light())) chance2 = 0;
			if (p_ptr->confused) chance2 -= 20;
			if (p_ptr->image)    chance2 -= 20;
			if (p_ptr->stun)     chance2 -= 20;

			if (randint(100) < chance2)
			{
				msg_format("You skillfully catch the %s.", o_name);
				returning = 3;
				break;
			}
			else
			{
				msg_format("You failed to catch the %s!", o_name);

				/* TODO: sometimes the weapon hits the player! */

				/* Go on travelling */
			}
		}
	}

	/* Handle 'returning' weapons  - clefs - */
	if (returning == 1)
	{
		returning = 2;

		/* Project in the opposite direction */
		ty = p_ptr->py;
		tx = p_ptr->px;
		py = y;
		px = x;
		inaccuracy = 0;
		dir = 0;

		goto start_travel;
	}

	/* Failed to catch it */
	else if (returning == 2)
	{
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
	}

	/* Successfully caught it */
	else if (returning == 3)
	{
		return;
	}


	/* Object can break if it hits a wall or monster */
	break_chance = ((hit_body || hit_wall) ? breakage_chance(i_ptr) : 0);

	/* Roll for breakage now */
	if ((break_chance) && (rand_int(100) < break_chance)) break_chance = 100;
	else                                                  break_chance =   0;

	/* Chance to learn about any sort of melee or throwing weapon */
	if ((i_ptr->k_idx) && (hit_body) && (!break_chance) && (one_in_(6)))
	{
		/* Item is a melee or throwing weapon */
		if ((is_melee_weapon(i_ptr)) || (throwing_weapon))
		{
			/* Only one weapon */
			if (no_pile)
			{
				/* Learn about the thrown weapon */
				learn_about_wearable(i_ptr, -2, FALSE);
			}

			/* A pile of thrown weapons */
			else
			{
				/* Learn about the pile */
				learn_about_wearable(o_ptr, -2, FALSE);

				/* Re-copy the object (so the thrown weapon stacks) */
				object_copy(i_ptr, o_ptr);
				distribute_charges(o_ptr, i_ptr, 1);
				i_ptr->number = 1;
			}
		}

		/* Item is an unknown boulder (1 in 18 chance) */
		else if ((i_ptr->tval == TV_JUNK) && (i_ptr->sval == SV_BOULDER) &&
		         (!(k_info[i_ptr->k_idx].special & (SPECIAL_KNOWN_EFFECT))) &&
		         (one_in_(3)))
		{
			/* Get object kind */
			object_kind *k_ptr = &k_info[i_ptr->k_idx];

			/* We now know about boulders */
			k_ptr->special |= (SPECIAL_KNOWN_EFFECT);

			/* Happy message */
			msg_print("You feel you know more about boulders.");
		}
	}


	/* The whole pile of objects has been "worn" (will now stack better) */
	i_ptr->ident |= (IDENT_WORN);
	if (!no_pile) o_ptr->ident |= (IDENT_WORN);

	/* Drop (or break/smash) near that location */
	drop_near(i_ptr, break_chance, y, x, DROP_HERE);

	/* Print "special attacks" */
	left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);
}


/*
 * Either switch between karate and wrestling, or switch primary/secondary weapons.
 */
void do_cmd_weapon_switch()
{
	if (p_ptr->barehanded)	do_cmd_barehanded();
	else if (switch_weapons(FALSE))
	{
		p_ptr->energy_use = 100;
		msg_print("You swap your weapons.");
	}
}


/*
 * Select unarmed combat skill.
 */
void do_cmd_barehanded()
{
	/* Toggle between wrestling and karate */
	if (p_ptr->barehand == S_KARATE)
	{
		msg_print("You are now wrestling.");
		p_ptr->barehand = S_WRESTLING;
	}
	else
	{
		msg_print("You are now using karate.");
		p_ptr->barehand = S_KARATE;
	}

	/* Update bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Equippy chars */
	p_ptr->redraw |= (PR_EQUIPPY);
}
