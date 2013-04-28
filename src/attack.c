/* File: attack.c */

/*
 * The non-magical attack code.
 *
 * Hit chance, critical hits in melee and when shooting/throwing, calculate
 * slays,/brands/resists.  Martial arts.   Deadliness adjustment, shield
 * bashes, melee attacks.  Chance of object breakage, the shooting code, the
 * throwing code.
 *
 * Copyright (c) 2002
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Determine if player or monster blows hit.
 *
 * The chance to hit increases with attack accuracy and decreases with
 * armour.  It never rises above 95% and never falls below 5%.
 */
bool test_hit_combat(int chance, int ac, int visible)
{
	int percent = 0;

	/* Power competes against armour */
	if (chance) percent = div_round(100 * (chance - ac), chance);

	/* Minimum of 5% chance to hit */
	if (percent < 5) percent = 5;

	/* Maximum of 95% chance to hit */
	if (percent > 95) percent = 95;

	/* Invisible monsters are harder to hit */
	if (!visible) percent /= 2;

	/* Return hit or miss */
	return (rand_int(100) < percent);
}

/*
 * Silly combat messages, used when the character gets a critical hit
 * against a sleeping, visible monster.  Do not display them too often.
 */
static cptr silly_hit_msg[8] =
{
	"Wakey, wakey!",
	"Good morning!",
	"Gotcha!",
	"Take that!",
	"Eat some!",
	"Having fun yet?",
	"Yeah!",
	"What a way to start up a fight!"
};


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
 */
static int critical_melee(int chance, bool visible, char m_name[],
	const object_type *o_ptr, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1, f2, f3;

	bool vorpal = FALSE;
	bool sneak_attack = FALSE;

	int blows = MAX(p_ptr->num_blow, p_ptr->num_blow2);

	int add_power = 0;

	/* Power of blow */
	int power = chance;

	/* Assume no added dice */
	int add_dice = 0;


	/* Get object flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Special quality (vorpal blades/weapons of concussion) */
	if ((f1 & (TR1_VORPAL)) || (p_ptr->special_attack & (ATTACK_VORPAL)))
	{
		power += chance / 2;
		vorpal = TRUE;
	}

	/* Special ability (sneak attacks) */
	if (p_ptr->special_attack & (ATTACK_SNEAK)) sneak_attack = TRUE;


	/* Calculate slay bonus (can - rarely - be as great as 60 points). */
	if (TRUE)
	{
		int max = 10;

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
	}

	/* Penalize some conditions */
	if (p_ptr->blind || p_ptr->confused || p_ptr->image || !visible)
	{
		sneak_attack = FALSE;
		power /= 3;
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
			/* More "interesting" messages if we get a seriously good hit. */
			if (add_dice >= 4)
			{
				/* Upon rare occasion, display a cute message. */
				if (one_in_(15))
				{
					cptr msg = silly_hit_msg[rand_int(8)];

					message(MSG_HIT, 0, format("%s", msg));
				}
				else
				{
					message(MSG_HIT, 0, "You ruthlessly sneak attack!");
				}
			}

			/* Standard "wakeup call". */
			else
			{
				message(MSG_HIT, 0, "You rudely awaken the monster.");
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
				message_format(MSG_HIT, 0, "You strike %s.", m_name);
			}

			else if (add_dice == 3)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				{
					message_format(MSG_HIT, 0, "You hack at %s.", m_name);
				}
				else
				{
					message_format(MSG_HIT, 0, "You pound %s.", m_name);
				}
			}

			else if (add_dice == 4)
			{
				if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				{
					if (vorpal)
					{
						message_format(MSG_HIT, 0,
							"Your vorpal blade goes snicker-snack!", m_name);
					}
					else
					{
						message_format(MSG_HIT, 0, "You slice into %s.", m_name);
					}
				}
				else
				{
					message_format(MSG_HIT, 0, "You bludgeon %s.", m_name);
				}
			}

			else if (add_dice >= 5)
			{
				if ((vorpal) && ((o_ptr->tval == TV_SWORD) ||
					(o_ptr->tval == TV_POLEARM)))
				{
					message_format(MSG_HIT, 0,
						"Your vorpal blade goes snicker-snack!", m_name);
				}
				else
				{
					message_format(MSG_HIT, 0, "You *smite* %s!", m_name);
				}
			}
		}

		/* Critical hits with polearms rob the monster of energy */
		if (o_ptr->tval == TV_POLEARM)
		{
			/* Discount multiple blows */
			int e_loss = 60 / blows;

			/* Take away energy (but not too much) */
			m_ptr->energy -= e_loss;
		}

		/* Critical hits with blunt weapon occasionaly stun monsters */
		if ((o_ptr->tval == TV_HAFTED) && (!m_ptr->stunned) &&
		    (one_in_(3 * blows)))
		{
			/* Skill is important */
			int dam = get_skill(TV_HAFTED, 10, 110);

			/* Attempt to stun the monster; handle resistances properly */
			project_bolt(0, 1, p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx,
				dam, GF_DO_STUN, PROJECT_HIDE);
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
 * powerful and more reliable.  Because ammo normally rolls one die,
 * critical hits are very powerful in archery.
 *
 * This function is responsible for the basic archery and throwing combat
 * messages, which vary according to the quality of the hit.  A distinction
 * is made between visible and invisible monsters.
 */
static int critical_shot(int chance, bool thrown_weapon, bool visible,
	char m_name[], const object_type *o_ptr, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char o_name[80];

	u32b f1, f2, f3;

	bool vorpal = FALSE;

	int add_power = 0;

	/* Extract missile power. */
	int power = chance;

	/* Assume no added dice */
	int add_dice = 0;


	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Obtain a terse object description */
	object_desc(o_name, o_ptr, FALSE, 0);


	/* Throwing weapons get lots of critical hits. */
	if (thrown_weapon) power += chance / 2;

	/* Special quality (vorpal arrows/shots of concussion) */
	if (f1 & (TR1_VORPAL))
	{
		power += chance / 2;
		vorpal = TRUE;
	}

	/* Calculate slay bonus (can - rarely - be as great as 60 points). */
	if (TRUE)
	{
		int max = 10;

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
	}

	/* Penalize some conditions */
	if (p_ptr->blind || p_ptr->confused || p_ptr->image || !visible) power /= 3;


	/* Test for critical hit (weak attacks are penalized). */
	if (randint(power + 200) <= (power - 30))
	{
		/* Determine level of critical hit. */
		if      ((power > 120) && (one_in_(50))) add_dice = 3;
		else if ((power >  40) && (one_in_(10))) add_dice = 2;
		else                                     add_dice = 1;

		/* Encourage the player to throw and shoot things at sleeping monsters. */
		if ((m_ptr->csleep) && (mon_fully_visible(m_ptr)))
		{
			/* More "interesting" messages if we get an especially good hit. */
			if ((add_dice >= 2) && (one_in_(10)))
			{
				message(MSG_HIT, 0, format("%s", silly_hit_msg[rand_int(8)]));
			}

			else if ((thrown_weapon) && (add_dice >= 2))
			{
				message(MSG_HIT, 0, "Assassin strike!");
			}

			/* Standard "wakeup call". */
			else
			{
				message(MSG_HIT, 0, "You rudely awaken the monster.");
			}
		}

		/* Print special messages if monster is fully visible. */
		if (mon_fully_visible(m_ptr))
		{
			/* Messages depend on quality of critical hit. */
			if (add_dice == 1)
			{
				message_format(MSG_HIT, 0, "The %s penetrates %s.",
					o_name, m_name);
			}

			else if (add_dice == 2)
			{
				message_format(MSG_HIT, 0, "The %s drives into %s.",
					o_name, m_name);
			}

			else if (add_dice >= 3)
			{
				message_format(MSG_HIT, 0, "The %s transpierces %s!",
					o_name, m_name);
			}
		}
	}

	/* If the shot is not a critical hit, then the default message is shown. */
	else if (mon_fully_visible(m_ptr))
	{
		message_format(MSG_HIT, 0, "The %s hits %s.",
			o_name, m_name);
	}

	/* Hits on non-visible monsters always generate the same message. */
	if (!mon_fully_visible(m_ptr))
	{
		message_format(MSG_HIT, 0, "The %s finds a mark.", o_name);
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
 * Monsters can resist or be almost immune to edged and/or blunt weapons.
 * Monsters can be susceptible to impact (rock remover) weapons.
 *
 * Note:  When the character is using martial arts, this function is
 * called with a wiped object "o_ptr".  Be careful.   XXX XXX
 */
void adjust_dam(int *damage, object_type *o_ptr, monster_type *m_ptr,
	bool is_trap)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f1, f2, f3;
	u32b fc1, fc2, fc3;

	bool visible = (mon_fully_visible(m_ptr) != FALSE);

	cptr p;

	/* Assume no special adjustments to damage */
	int mul = 10;
	int div = 10;
	int add = 0;
	int sub = 0;

	int tmp;


	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Get character flags */
	player_flags(&fc1, &fc2, &fc3, TRUE, FALSE);

	/* Apply character flags */
	f1 |= fc1;    f2 |= fc2;    f3 |= fc3;

	/* Get "cancelled" flags */
	player_flags_cancel(&fc1, &fc2, &fc3, TRUE);

	/* Apply character cancellation flags (cancels everything) */
	f1 &= ~fc1;    f2 &= ~fc2;    f3 &= ~fc3;


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

				/* Use up holy attack  XXX */
				if ((p_ptr->special_attack & (ATTACK_HOLY)) &&
				    (one_in_(20)))
				{
					msg_print("You no longer strike with holy force.");
					p_ptr->special_attack &= ~(ATTACK_HOLY);
				}
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

				else if (add < 18)  add = 18;
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
						if ((f1 & (TR1_BRAND_FLAME)) && (add < 30)) add = 33;
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

			/* Special attack (Impact) */
			if (f3 & (TR3_IMPACT))
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

			break;
		}
	}

	/* Hack -- Triple crossbows get 1/3rd the bonus */
	if ((o_ptr->tval == TV_BOW) && (o_ptr->sval == SV_TRIPLE_XBOW))
		add = div_round(add, 3);


	/* Use up special attack powers */
	dec_special_atk();


	/* Get base name of object kind */
	if      (o_ptr->tval == TV_SHOT)  p = "shot";
	else if (o_ptr->tval == TV_ARROW) p = "arrow";
	else if (o_ptr->tval == TV_BOLT)  p = "bolt";
	else if (is_melee_weapon(o_ptr))  p = "weapon";
	else                              p = "missile";

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
			if (r_ptr->flags3 & (RF3_IM_EDGED))
			{
				if (f1 & (TR1_VORPAL)) div = 40;
				else div = 60;

				if ((visible) && (!is_trap))
				{
					/* Message */
					if (!(l_ptr->flags3 & (RF3_IM_EDGED)))
					{
						msg_format("Your %s doesn't seem to be doing any damage!", p);
						l_ptr->flags3 |= (RF3_IM_EDGED);
					}
				}
				else if (!is_trap)
				{
					/* An ugly but necessary message  XXX */
					msg_format("It seems unharmed.", p);
				}
			}
			else if (r_ptr->flags3 & (RF3_RES_EDGED))
			{
				if (f1 & (TR1_VORPAL)) div = 15;
				else div = 20;

				if ((visible) && (!is_trap))
				{
					/* Message */
					if (!(l_ptr->flags3 & (RF3_RES_EDGED)))
					{
						msg_format("Your %s seems to be doing little damage!", p);
						l_ptr->flags3 |= (RF3_RES_EDGED);
					}
				}
			}

			break;
		}

		case TV_SHOT:
		case TV_HAFTED:
		default:
		{
			if (r_ptr->flags3 & (RF3_IM_BLUNT))
			{
				if (f1 & (TR1_VORPAL)) div = 40;
				else div = 60;

				if ((visible) && (!is_trap))
				{
					/* Message */
					if (!(l_ptr->flags3 & (RF3_IM_BLUNT)))
					{
						msg_format("Your %s doesn't seem to be doing any damage!", p);
						l_ptr->flags3 |= (RF3_IM_BLUNT);
					}
				}
				else if (!is_trap)
				{
					/* An ugly but necessary message  XXX */
					msg_print("It seems unharmed.");
				}
			}
			else if (r_ptr->flags3 & (RF3_RES_BLUNT))
			{
				if (f1 & (TR1_VORPAL)) div = 15;
				else div = 20;

				if ((visible) && (!is_trap))
				{
					/* Message */
					if (!(l_ptr->flags3 & (RF3_RES_BLUNT)))
					{
						msg_format("Your %s seems to be doing little damage!", p);
						l_ptr->flags3 |= (RF3_RES_BLUNT);
					}
				}
			}

			break;
		}
	}

	/* Apply addition and subtraction */
	*damage += (add - sub);

	/* Apply multiplier */
	*damage *= mul;

	/* Remember damage */
	tmp = *damage;

	/* Paranoia -- Require a positive divisor */
	if (div > 0)
	{
		/* Apply divisor */
		*damage = div_round(*damage, div);
	}
}


/*
 * Shield bash monsters.
 *
 * We assume that certain checks (wearing a shield, not confused, etc.) have
 * been done.
 *
 * Return TRUE if the monster got killed.
 */
static bool shield_bash(int y, int x, monster_race *r_ptr, monster_type *m_ptr, int *blows, bool *fear)
{
	object_type *weapon_ptr = &inventory[INVEN_WIELD];
	object_type *shield_ptr = &inventory[INVEN_ARM];

	/* Variables for the bashing code */
	int bash_chance, bash_quality, bash_dam;

	char m_name[80];

	int wskill = get_skill(sweapon(), 0, 100);

	/* Get monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Bashing chance depends on dexterity and a Skill bonus */
	bash_chance = (adj_dex_th[p_ptr->stat_ind[A_DEX]] - 128) / 2;

	/* Apply melee skill bonus, depending on realm */
	if (p_ptr->realm)
	{
		if (!p_ptr->oath) bash_chance += get_skill(sweapon(), 0, 50);
	}
	else
	{
		bash_chance += get_skill(sweapon(), 0, 100);
	}


	/* Players bash more often when they see a real need. */
	if (bash_chance)
	{
		/* We are using bare hands, and we're not very good at it */
		if (!weapon_ptr->k_idx)
		{
			if (wskill < p_ptr->power / 2)
			{
				bash_chance *= MIN(6, p_ptr->power / MAX(1, wskill));
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
		bash_dam += (adj_str_td[p_ptr->stat_ind[A_STR]] - 128);

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
		if ((20 + adj_dex_th[p_ptr->stat_ind[A_DEX]] - 128) < randint(80))
		{
			*blows += randint(MAX(p_ptr->num_blow, p_ptr->num_blow2));
		}
	}

	return (FALSE);
}


/*
 * Striking certain kinds of monsters with your bare hands is risky,
 * especially if you aren't wearing gloves.
 *
 * Return TRUE if we get hurt.
 */
static bool contact_danger_check(monster_race *r_ptr)
{
	int i;
	int blow_type[MONSTER_BLOW_MAX] = {0, 0, 0, 0};
	int ele_blow_cnt = 0;

	cptr name = (r_name + r_ptr->name);

	/* Assume not dangerous */
	int damage = 0;
	int danger_type = 0;

	int protection;
	object_type *o_ptr;

	/* Get the protective value of worn gloves */
	o_ptr = &inventory[INVEN_HANDS];
	protection = o_ptr->ac + o_ptr->to_a;


	/* If the monster is a Death Mold, you're in trouble. */
	if (strstr(name, "Death mold"))
	{
		take_hit(damroll(30, 30) - (protection * 10),
		   0, "Oh no!  You've touched a Death mold!",
			"touching a Death mold");
		return (TRUE);
	}


	/* Some monsters are almost always dangerous to touch. */
	else if (strchr("Ev*", r_ptr->d_char))
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

	/*
	 * If every one of the monster's melee attacks does acid or poison
	 * damage, it is dangerous to touch.
	 */
	else
	{
		for (i = 0; i < MONSTER_BLOW_MAX; i++)
		{
			if (r_ptr->blow[i].method)
			{
				if (r_ptr->blow[i].effect == RBE_ACID)
					blow_type[ele_blow_cnt++] = GF_ACID;
				else if (r_ptr->blow[i].effect == RBE_POISON)
					blow_type[ele_blow_cnt++] = GF_POIS;

				/* Non-poison or acid attack - assume not very dangerous */
				else return (FALSE);
			}
		}
	}

	/* Monster was not dangerous */
	if (danger_type == 0) return (FALSE);


	/* Pick a type of nastiness at random */
	danger_type = blow_type[rand_int(ele_blow_cnt)];

	/* Calculate damage */
	damage = damroll(2, 2 + r_ptr->level / 8) - protection / 2;

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
				take_hit(damage / 2, 0, NULL, "touching a poisonous creature");
				return (TRUE);
			}
		}
	}

	/* Didn't get hurt */
	return (FALSE);
}

/*
 * Describe a bare-handed attack.
 *
 * ToDo:  Replace this wimpy stuff.  XXX XXX
 */
static cptr barehand_attack_desc[4][3] =
{
	{ "punch", "kick", "hit" },
	{ "Flying-kick", "Belt", "Strike" },
	{ "grab", "squeeze", "slam" },
	{ "Grip", "Crush", "Body-slam" }
};



/*
 * Learn about the damage done in martial arts.
 *
 * The damage displayed is a approximate running average, with recent
 * attacks slowly overriding old ones.
 */
static void learn_about_ma_damage(int damage, bool karate)
{
	s16b *temp;

	/* Randomize damage slightly (inaccuracy) */
	damage = rand_spread(damage, div_round(damage, 6));
	if (damage <= 0) damage = 1;

	/* Point to the right variable */
	if (karate) temp = &p_ptr->karate_dam;
	else        temp = &p_ptr->wrestling_dam;

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
static int py_attack_barehand(monster_type *m_ptr, bool *do_slow,
	bool *do_stun, bool *do_conf, bool *do_throw)
{
	int bonus, max;
	byte dice, sides;
	int damage;
	int add_power = 0;
	int set;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	char m_name[80];

	object_type *o_ptr;
	object_type object_type_body;

	/* Martial arts normally don't get criticals */
	int critical_hit_chance = 0;
	bool critical = FALSE;



	/* Using karate - about 105 * 2 max average damage */
	if (p_ptr->barehand == S_KARATE)
	{
		/* Dexterity and strength are helpful */
		bonus = (adj_ma[p_ptr->stat_ind[A_STR]] / 2) +
					adj_ma[p_ptr->stat_ind[A_DEX]];

		/* Effective skill max depends on specialization */
		max = 60;
		if (p_ptr->oath & (OATH_OF_IRON)) max = 70;
		else if (p_ptr->oath)             max = 50;

		/* Damage depends on skill (and also stats) */
		damage = get_skill(S_KARATE, 1, max + bonus);

		/* Chance of various special attacks */
		if ((one_in_(3)) &&
			 (randint(damage) >= 5 + r_ptr->level / 2))
		{
			if      (one_in_(3)) *do_conf = TRUE;
			else if (one_in_(2)) *do_slow = TRUE;
			else                 *do_stun = TRUE;
		}
	}

	/* Wrestling - about 125 * 2 max average damage */
	else
	{
		/* Strength is very important */
		bonus = adj_ma[p_ptr->stat_ind[A_STR]] * 2;

		/* Effective skill max depends on specialization */
		max = 70;
		if (p_ptr->oath & (OATH_OF_IRON)) max = 80;
		else if (p_ptr->oath)             max = 60;

		/* Damage depends on skill (and also stats) */
		damage = get_skill(S_WRESTLING, 1, max + bonus);

		/* Give the player a chance to throw the opponent */
		if ((one_in_(5)) &&
			 (randint(damage) >= 2 * r_ptr->level / 3 + 10))
		{
			*do_throw = TRUE;
		}
	}

	/* Turn base damage into dice */
	dam_to_dice(damage, &dice, &sides, TRUE);

	/* Roll the dice, get actual damage */
	damage = damroll(dice, sides);


	/* Allow sneak attacks */
	if ((p_ptr->special_attack & (ATTACK_SNEAK)) &&
	    !(p_ptr->blind || p_ptr->confused || p_ptr->image || !m_ptr->ml))
	{
		/* Random chance for critical */
		if (rand_int(get_skill(p_ptr->barehand, 0, 100)) >
		    rand_int(r_ptr->level))
		{
			msg_print("You get in a sneak attack!");
			critical_hit_chance = 100;
		}
	}

	/* Calculate slay bonus (can be as great as 20% extra damage). */
	max = 10;

	/* Those without a realm get a larger bonus */
	if (!p_ptr->realm) max = 30;

	/* Pure warriors get the maximum bonus */
	if (p_ptr->oath & (OATH_OF_IRON)) max = 40;

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
	critical_hit_chance += add_power;


	/* Bonus for vorpal blows */
	if (p_ptr->special_attack & (ATTACK_VORPAL))
		critical_hit_chance += 20;


	/* Penalize some conditions (note that blindness is not a problem) */
	if (p_ptr->confused || p_ptr->image || !m_ptr->ml)
	{
		critical_hit_chance /= 5;
	}

	/* Try for a critical hit */
	if (rand_int(100) < critical_hit_chance)
	{
		/* Add 50% to damage XXX */
		damage += damage / 2;

		/* Use critical hit messages */
		if (p_ptr->barehand == S_KARATE) set = 1;
		else                             set = 3;
		critical = TRUE;
	}
	else
	{
		/* Use normal messages */
		if (p_ptr->barehand == S_KARATE) set = 0;
		else                             set = 2;
	}


	/* Get monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Message */
	message_format(MSG_HIT, 0, "You %s %s%c",
		barehand_attack_desc[set][rand_int(3)], m_name,
		critical ? '!' : '.');

	/* Learn about martial art damage (before slay/brand) */
	learn_about_ma_damage(damage, p_ptr->barehand == S_KARATE);


	/* Get local object */
	o_ptr = &object_type_body;

	/* Set all object data to zero */
	object_wipe(o_ptr);

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

	/* Bonus to attack if monster is sleeping. */
	int sleeping_bonus = 0;

	/* Terrain adjustments to effective monster ac. */
	int terrain_adjust = 0;

	/* Skill and Deadliness */
	int bonus, chance, total_deadliness;

	/* Weapon skill */
	int skill = get_skill(sweapon(), 0, 100);

	/* Assume no special attack effects */
	bool bare_handed = FALSE;
	bool do_throw = FALSE;
	bool do_conf = FALSE;
	bool do_slow = FALSE;
	bool do_stun = FALSE;
	bool impact = FALSE;
	bool dead = FALSE;
	int do_force_back = 0;

	u32b f1, f2, f3;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	object_type *o_ptr;

	char m_name[80];

	bool fear = FALSE;


	/* Get the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];


	/* Reveal minics (note: mimics cannot be sneak-attacked) */
	if ((m_ptr->mflag & (MFLAG_MIME)) && (m_ptr->ml))
	{
		/* Reveal the monster */
		m_ptr->mflag &= ~(MFLAG_MIME);

		/* Get monster name ("a kobold") */
		monster_desc(m_name, m_ptr, 0x88);

		/* Message */
		if (!p_ptr->afraid)
		{
			msg_format("You find yourself fighting %s!", m_name);
		}
		else
		{
			char m_pronoun[80];

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
		/* Monster is visible */
		if (m_ptr->ml)
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
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);


	/* Get monster name (or "it") */
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
			/* Special Message */
			msg_print("Something scary is in your way!");
		}

		/* Done */
		return (TRUE);
	}

	/* Monsters in rubble can take advantage of cover. */
	if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		terrain_adjust = r_ptr->ac / 7 + 5;
	}

	/*
	 * Monsters in trees can take advantage of cover, but those skilled
	 * in nature lore can hit them more easily.
	 */
	if (cave_feat[y][x] == FEAT_TREE)
	{
		terrain_adjust = r_ptr->ac / 7 + 5;
		terrain_adjust -= get_skill(S_NATURE, 0, 20);
	}

	/* Monsters in water are vulnerable. */
	if (cave_feat[y][x] == FEAT_WATER)
	{
		terrain_adjust -= r_ptr->ac / 5;
	}


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
		if ((r_ptr->flags2 & (RF2_PASS_WALL)) &&
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
				msg_format("You cannot harm %s with martial arts unless blessed.", m_name);

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


	/* Attack until we run out of blows. */
	while (!dead)
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


		/* Try each of the weapons being wielded in turn. */
		for (i = 1; i <= num_weapons; i++)
		{
			/* Get the weapon (if any) */
			if (i == 1)
			{
				/* No more blows allowed with this weapon */
				if (blows > p_ptr->num_blow) continue;

				/* Check first arm -- note that primary weapon must be in this slot */
				o_ptr = &inventory[INVEN_WIELD];
			}
			else if (i == 2)
			{
				/* No more blows allowed with this weapon */
				if (blows > p_ptr->num_blow2) continue;

				/* Check second arm -- require a weapon */
				o_ptr = &inventory[INVEN_ARM];
				if (!is_melee_weapon(o_ptr)) continue;
			}

			/* Characters cannot wield more than two weapons. */
			else continue;

			/* Monster is dead */
			if (dead) break;


			/*** Attack with this weapon (or bare hands) ***/


			/* Calculate the attack quality. */
			if (bare_handed)
			{
				/* Note that "p_ptr->skill_thn" is often higher */
				bonus = 0;
			}
			else
			{
				bonus = p_ptr->to_h + o_ptr->to_h;
			}
			chance = (p_ptr->skill_thn + BTH_PLUS_ADJ * bonus);

			/* Some monsters are great at dodging  -EZ- */
			if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
			    (one_in_(2)))
			{
				message_format(MSG_MISS, 0, "%^s evades your blow!",
					m_name);

				/* Learn that monster can dodge */
				l_ptr->flags2 |= (RF2_EVASIVE);

				continue;
			}

			/* Test for hit */
			else if (test_hit_combat(chance + sleeping_bonus,
			         r_ptr->ac + terrain_adjust, m_ptr->ml))
			{
				/* Character is wielding a weapon */
				if (is_melee_weapon(o_ptr))
				{
					int dice;
					long die_average, temp, sides;

					/* Calculate deadliness */
					total_deadliness = p_ptr->to_d + o_ptr->to_d;

					/* Get object flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Note impact weapon */
					if (f3 & (TR3_IMPACT)) impact = TRUE;


					/* Base damage dice */
					dice = o_ptr->dd;

					/* Critical hits may add damage dice. */
					dice += critical_melee(chance + sleeping_bonus, m_ptr->ml,
					                       m_name, o_ptr, m_ptr);


					/* Get the average value of a single damage die. (x10) */
					die_average = (10 * (o_ptr->ds + 1)) / 2;

					/* Apply deadliness to average. (100x inflation) */
					apply_deadliness(&die_average, total_deadliness);

					/* Reconvert to die sides. */
					temp = (2L * die_average) - 1000;

					/* Calculate the actual number of sides to each die. */
					sides = div_round(temp, 1000);


					/* Roll out the damage. */
					damage = damroll(dice, (s16b)sides);

					/* Adjust damage for slays, brands, resists. */
					adjust_dam(&damage, o_ptr, m_ptr, FALSE);
				}

				/* Character is fighting bare-handed (first weapon only) */
				else if (i == 1)
				{
					damage = py_attack_barehand(m_ptr, &do_slow, &do_stun,
					                            &do_conf, &do_throw);
				}

				/* count hits */
				hits++;
				hit = TRUE;

				/* Sound */
				sound(SOUND_HIT);

				/* If this is the first hit, make some noise. */
				if (hits == 1)
				{
					int noise = p_ptr->base_wakeup_chance / 2;

					/* Burglary skill greatly reduces noise */
					noise += 1000 - get_skill(S_BURGLARY, 0, 800);

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

				/* Practice the melee skill */
				skill_being_used = sweapon();

				/* Damage, check for fear and death. */
				if (mon_take_hit(cave_m_idx[y][x], -1, damage, &fear, NULL))
				{
					/*
					 * Hack -- High-level warriors can spread their attacks out
					 * among weaker foes.
					 */
					if ((p_ptr->oath & (OATH_OF_IRON)) &&
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
					/* Confuse the monster */
					if (r_ptr->flags3 & (RF3_NO_CONF))
					{
						if (m_ptr->ml)
						{
							l_ptr->flags3 |= (RF3_NO_CONF);
						}

						msg_format("%^s is unaffected.", m_name);
					}
					else if (rand_int(p_ptr->power + 25) <
					         r_ptr->level + randint(10))
					{
						msg_format("%^s is unaffected.", m_name);
					}
					else
					{
						message_format(MSG_HIT, 0, "%^s appears confused.", m_name);
						m_ptr->confused = 10;
					}

					/* Sometimes cancel attack */
					if (one_in_(3))
					{
						/* Message */
						message(MSG_HIT, 0, "Your hands stop glowing.");

						/* Cancel special confusion attack */
						p_ptr->special_attack &= ~(ATTACK_CONFUSE);
					}
				}

				/* Black Breath attack */
				if (p_ptr->special_attack & (ATTACK_BLKBRTH))
				{
					/* Cancel black breath */
					p_ptr->special_attack &= ~(ATTACK_BLKBRTH);

					/* Message */
					msg_print("Your hands stop radiating Night.");

					/* The undead are immune */
					if (r_ptr->flags3 & (RF3_UNDEAD))
					{
						/* Learn about visible monster */
						if (m_ptr->ml)
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

				/* Burglar's "Hit and Run" attack */
				if (p_ptr->special_attack & (ATTACK_FLEE))
				{
					/* Flee after a random number of blows */
					if (blows >= rand_int(MAX(p_ptr->num_blow,
					                          p_ptr->num_blow2)))
					{
						/* Message */
						msg_print("You escape into the shadows!");

						/* Teleport. */
						teleport_player(get_skill(S_BURGLARY, 6, 12), TRUE);

						/* Cancel the fleeing spell */
						p_ptr->special_attack &= ~(ATTACK_FLEE);

						/* Stop attacking */
						blows = 100;
					}
				}

				/* We successfully landed a blow.  No need to try other weapons. */
				break;
			}
		}

		/* End of weapon sequence for this blow */

		/* Monster is no longer asleep */
		sleeping_bonus = 0;


		/* This blow missed. */
		if (hit == FALSE)
		{
			/* Sound */
			sound(SOUND_MISS);

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
		                    (r_ptr->flags2 & (RF2_PASS_WALL))))
		{
			int e_loss;

			/* Message */
			msg_format("You threw %s to the ground!", m_name);

			/* The monster loses some energy while getting up */
			e_loss = rand_range(skill / 3, 2 * skill / 3);

			/* Lose energy */
			m_ptr->energy -= MIN(m_ptr->energy, e_loss);
		}

		/* Attempt a confusion attack */
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

		/* Attempt a slowing attack (not cumulative) */
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
				message_format(MSG_HIT, 0, "%^s is hindered.", m_name);
				m_ptr->mspeed -= 3;
			}
		}

		/* Attempt a stunning attack (not cumulative) */
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
				if (strchr("DGP#", r_ptr->d_char)) k /= 3;
				else if (strchr("OTdgv", r_ptr->d_char)) k /= 2;

				/* Thrust away */
				thrust_away(-1, m_ptr->fy, m_ptr->fx, MIN(3, 1 + k / 15));
			}
		}


		/* Hack -- delayed fear messages */
		if (fear && m_ptr->ml)
		{
			/* Sound */
			sound(SOUND_FLEE);

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
			/* Throwing weapons sometimes break when used in melee */
			if ((k_info[o_ptr->k_idx].flags1 & (TR1_THROWING)) &&
			    (!artifact_p(o_ptr)) && (one_in_(20)))
			{
				msg_print("Your weapon breaks!");

				/* Reduce and describe inventory */
				inven_item_increase(INVEN_WIELD, -1);
				inven_item_describe(INVEN_WIELD);
				inven_item_optimize(INVEN_WIELD);
			}

			/* Sometimes learn more about un-IDed wielded objects */
			else if (one_in_(12))
			{
				learn_about_wearable(o_ptr, INVEN_WIELD, FALSE);
			}
		}

		/* Check second arm */
		o_ptr = &inventory[INVEN_ARM];

		/* Wielding a weapon */
		if ((o_ptr->k_idx) && (is_melee_weapon(o_ptr)))
		{
			/* Throwing weapons sometimes break when used in melee */
			if ((k_info[o_ptr->k_idx].flags1 & (TR1_THROWING)) &&
			    (one_in_(20)))
			{
				msg_print("Your weapon breaks!");

				/* Reduce and describe inventory */
				inven_item_increase(INVEN_ARM, -1);
				inven_item_describe(INVEN_ARM);
				inven_item_optimize(INVEN_ARM);
			}
			else if (one_in_(12))
			{
				learn_about_wearable(o_ptr, INVEN_ARM, FALSE);
			}
		}
	}

	/* Return */
	return (TRUE);
}


/*
 * Transfer launcher attributes to missile.
 */
void transfer_attributes_to_missile(const object_type *o_ptr,
	object_type *i_ptr)
{
	/* Transfer all the flags */
	i_ptr->flags1 |= o_ptr->flags1;
	i_ptr->flags2 |= o_ptr->flags2;
	i_ptr->flags3 |= o_ptr->flags3;

	/* We could also have launchers that took away attributes... */
}


/*
 * Determine the odds of an object breaking when thrown at a monster.
 *
 * Note that artifacts never break; see the "drop_near()" function.
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
			return (30 - (3 * o_ptr->ac / 2));
		}

		/* Sometimes break */
		case TV_SHOT:
		case TV_BOLT:
		{
			return (20 - o_ptr->ac);
		}

		case TV_JUNK:
		{
			if (o_ptr->sval == SV_BOULDER) return (10);
			else return (40);
		}

		/* Seldom break */
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_WAND:
		{
			return (10 - o_ptr->ac);
		}

		/* Never break */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_RING:
		case TV_ROD:
		{
			return (0);
		}
	}

	/* Rarely break */
	return (5 - (o_ptr->ac / 2));
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
	int tdis;

	int break_chance;

	int armour, bonus, chance, total_deadliness;

	int sleeping_bonus = 0;
	int terrain_adjust = 0;
	int inaccuracy;

	int damage;

	/* Assume no special bonuses */
	int special_pierce = 0;
	int special_dam = 0;
	int special_impact = 0;
	int special_hit = 0;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type *j_ptr;
	object_type *q_ptr;
	object_type object_type_body;
	object_type object_type_body2;

	u32b f1, f2, f3;

	int hit_body = 0;
	bool hit_wall = FALSE;
	bool impact = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[120];
	char m_name[80];

	int path_n = 0;
	u16b path_g[256];

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

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the missile */
	object_copy(i_ptr, j_ptr);

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

	/* Get local object */
	q_ptr = &object_type_body2;

	/* Save a backup copy of the fired missile */
	object_copy(q_ptr, i_ptr);


	/* Use some energy ("p_ptr->num_fire" has a standard value of 2) */
	p_ptr->energy_use = div_round(200, p_ptr->num_fire);

	/* Sound */
	sound(SOUND_SHOOT);


	/* Fire ammo of backbiting, and it will turn on you.  -LM- */
	if (i_ptr->ego_item_index == EGO_BACKBITING)
	{
		/* Calculate damage. */
		damage = damroll(p_ptr->ammo_mult * i_ptr->dd * randint(2), i_ptr->ds * 4);
		damage += special_dam;

		/* Inflict both normal and wound damage. */
		take_hit(damage, 0, "Your missile turns in midair and strikes you!",
		   "ammo of backbiting");

		set_cut(randint(damage * 3));

		/* That ends that shot! */
		return;
	}

	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Get the color and symbol for the fired object */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Base range  (XXX - this formula is a little weird) */
	tdis = 5 + 5 * p_ptr->ammo_mult;

	/* Calculate the quality of the shot */
	bonus = (p_ptr->to_h + o_ptr->to_h + i_ptr->to_h);
	chance = p_ptr->skill_thb + (BTH_PLUS_ADJ * bonus);

	/* Sum all the applicable additions to Deadliness. */
	total_deadliness = p_ptr->to_d + o_ptr->to_d + i_ptr->to_d;

	/* Accuracy at range depends on shooting skill */
	inaccuracy = 6 - get_skill(sbow(o_ptr->sval), 0, 5);


	/* Hack -- transfer launcher attributes to the missile */
	transfer_attributes_to_missile(o_ptr, i_ptr);


	/* Handle various special attacks  -LM- */
	if (p_ptr->special_attack)
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
			special_dam = TRUE;
			p_ptr->special_attack &= ~(ATTACK_DEADLY);
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
			chance *= 2;

			/* Cancel the special attack */
			p_ptr->special_attack &= ~(ATTACK_BARD);
		}
	}


	/* Get (adjusted) missile attributes */
	object_flags(i_ptr, &f1, &f2, &f3);

	/* Learn some flags */
	if (f3 & (TR3_IMPACT)) impact = TRUE;

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

	/* Calculate the path -- missiles fly right past missed targets */
	path_n = project_path(path_g, tdis, py, px, &ty, &tx, PROJECT_THRU);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);


		/* Collide with walls */
		if (!cave_floor_bold(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistency */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			monster_lore *l_ptr = &l_list[m_ptr->r_idx];

			int chance2 =
				chance * (100 - (distance(py, px, y, x) * inaccuracy)) / 100;

			bool fear = FALSE;

			int dice;
			long die_average, temp, sides;

			/* Assume a default death */
			cptr note_dies = " dies.";

			/* Some monsters get "destroyed" */
			if (monster_nonliving(r_ptr))
			{
				/* Special note at death */
				note_dies = " is destroyed.";
			}

			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);


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


			/* Monsters in rubble can take advantage of cover. */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				terrain_adjust = r_ptr->ac / 5 + 5;
			}

			/*
			 * Monsters in trees can take advantage of cover, but those skilled
			 * in nature lore can hit them more easily.
			 */
			if (cave_feat[y][x] == FEAT_TREE)
			{
				terrain_adjust = r_ptr->ac / 7 + 5;
				terrain_adjust -= get_skill(S_NATURE, 0, 20);
			}

			/* Monsters in water are vulnerable. */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				terrain_adjust -= r_ptr->ac / 5;
			}

			/* Get effective armour class of monster. */
			armour = r_ptr->ac + terrain_adjust;

			/* Weapons of velocity sometimes almost negate monster armour. */
			if (special_hit) armour /= 3;


			/* Some monsters are great at dodging  -EZ- */
			else if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
			         (rand_int(5 + m_ptr->cdis) >= 3))
			{
				if (mon_fully_visible(m_ptr))
				{
					message_format(MSG_MISS, 0, "%^s dodges!", m_name);

					/* Learn that monster can dodge */
					l_ptr->flags2 |= (RF2_EVASIVE);
				}

				continue;
			}

			/* Did we hit it (penalize distance travelled) */
			if (!test_hit_combat(chance2 + sleeping_bonus, armour, m_ptr->ml))
			{
				/* Object whizzes right past the monster */
				continue;
			}

			/* Make some noise. */
			if (TRUE)
			{
				int noise = p_ptr->base_wakeup_chance / 2;

				/* Burglary skill greatly reduces noise */
				noise += 1000 - get_skill(S_BURGLARY, 0, 800);

				/* Increase the noise level */
				add_wakeup_chance += noise;
			}

			/* Note the collision */
			hit_body++;

			/* Reveal fully visible mimics */
			if ((m_ptr->mflag & (MFLAG_MIME)) && (m_ptr->ml) &&
			    (!(m_ptr->mflag & (MFLAG_DLIM))))
			{
				/* Reveal the monster */
				m_ptr->mflag &= ~(MFLAG_MIME);

				/* Get monster name again */
				monster_desc(m_name, m_ptr, 0x88);

				/* Message  XXX */
				msg_print("It was a mimic!");
			}

			/* Hack -- Track this monster race, if monster is visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

			/* Hack -- Track this monster, if visible */
			if (m_ptr->ml) health_track(cave_m_idx[y][x]);

			/* Practice the shooting skill */
			skill_being_used = sbow(o_ptr->sval);


			/* Other items hit the monster */

			/*
			 * The basic damage-determination formula is the same in
			 * archery as it is in melee (apart from the launcher mul-
			 * tiplier).
			 */

			/* Base damage dice */
			dice = i_ptr->dd;

			/* Critical hits may add damage dice. */
			dice += critical_shot(chance2 + sleeping_bonus, FALSE, m_ptr->ml,
										  m_name, i_ptr, m_ptr);

			/* Get the average value of a single damage die. (10x inflation) */
			die_average = (10 * (i_ptr->ds + 1)) / 2;


			/* Apply the launcher multiplier to average. */
			die_average *= p_ptr->ammo_mult;

			/* Apply deadliness to average. (100x inflation) */
			apply_deadliness(&die_average, total_deadliness);

			/* Reconvert to die sides. */
			temp = (2L * die_average) - 1000;

			/* Calculate the actual number of sides to each die. */
			sides = div_round(temp, 1000);


			/* Roll out the damage. */
			damage = damroll(dice, (s16b)sides);
debug("dam1 = %d", damage);
			/* Special damage bonus */
			if (special_dam)
			{
				damage += 10;

				/* Crossbow of Harad is deadly */
				if (o_ptr->artifact_index == ART_HARAD) damage += 10;
debug("dam2 = %d", damage);
			}

			/* Adjust damage for slays, brands, resists. */
			adjust_dam(&damage, i_ptr, m_ptr, FALSE);
debug("dam3 = %d", damage);

			/* No negative damage */
			if (damage < 0) damage = 0;

			/* Hit the monster, check for death. */
			if (mon_take_hit(cave_m_idx[y][x], -1, damage, &fear, note_dies))
			{
				/* Dead monster */
			}

			/* No death */
			else
			{
				/* Message */
				message_pain(cave_m_idx[y][x], damage);

				/* Monsters can be thrust back by an impact missile. */
				if (impact)
				{
					/* Minimum chance is 35%, and it rises for powerful hits */
					int tmp = 35 + (125 * damage / m_ptr->maxhp);

					/* Chance triples for special impact hits */
					if (special_impact) tmp *= 3;

					/* Force the monster back */
					if (tmp > rand_int(100))
					{
						int k = damage;
						if (special_impact) k *= 3;

						if (!(r_ptr->flags4 & (RF4_BRTH_FORCE)))
						{
							/* Big, heavy monsters (or ghosts) */
							if (strchr("DGP#", r_ptr->d_char)) k /= 3;
							else if (strchr("OTdgv", r_ptr->d_char)) k /= 2;

							/* Thrust away */
							thrust_away(-1, m_ptr->fy, m_ptr->fx, MIN(6, 1 + k / 10));
						}
					}
				}

				/* Take note */
				if (fear && m_ptr->ml)
				{
					/* Sound */
					sound(SOUND_FLEE);

					/* Message */
					message_format(MSG_FLEE, 0, "%^s flees in terror!", m_name);
				}
			}

			/* Allow piercing shots (go through 2 to 5 opponents) */
			if ((special_pierce) && (hit_body < rand_range(2, 5)))
			{
				continue;
			}

			/* Object has hit a monster -- it falls to the floor */
			break;
		}
	}


	/* Chance of breakage (during attacks) */
	break_chance = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Hack -- roll for breakage now */
	if (rand_int(100) < break_chance) break_chance = 100;
	else                              break_chance =   0;


	/* Chance to learn about the launcher and ammo */
	if (hit_body)
	{
		/* Learn about the launcher */
		if ((o_ptr->k_idx) && (one_in_(10)))
		{
			learn_about_wearable(o_ptr, INVEN_BOW, FALSE);
		}

		/* Learn about the stack of ammo, if any remains */
		if (j_ptr->k_idx)
		{
			if (one_in_(6))
			{
				learn_about_wearable(j_ptr, -1, FALSE);

				/* Copy updated information to the backup missile */
				object_copy(q_ptr, j_ptr);
				q_ptr->number = 1;
			}
		}

		/* No ammunition remains -- learn about the backup copy */
		else if (!break_chance)
		{
			if (one_in_(6))
			{
				learn_about_wearable(q_ptr, -1, FALSE);
			}
		}
	}


	/* Restore old flags and grant new knowledge to the fired missile */
	object_copy(i_ptr, q_ptr);

	/* Object falls to the floor (as opposed to being dropped) */
	if (break_chance == 0) break_chance = -1;

	/* Drop (or break/shatter) near that location */
	drop_near(i_ptr, break_chance, y, x);
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
	int mul, div;

	int break_chance;

	int total_deadliness;
	int sleeping_bonus = 0;
	int terrain_adjust = 0;
	int inaccuracy;

	int damage;

	u32b f1, f2, f3;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;
	bool hit_wall = FALSE;
	bool impact = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[120];
	char m_name[80];

	int path_n = 0;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


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
	if ((cursed_p(o_ptr)) && (item >= INVEN_WIELD))
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Single object */
		i_ptr->number = 1;

		/* Get a short object description */
		object_desc(o_name, i_ptr, FALSE, 0);

		if ((artifact_p(i_ptr)) && (object_known_p(i_ptr)))
			msg_format("The %s clings to your hand, and cannot be thrown!",
			o_name);
		else
			msg_format("Your %s clings to your hand, and cannot be thrown!",
			o_name);

		return;
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Distribute the charges of rods/wands between the stacks */
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
	object_desc(o_name, i_ptr, FALSE, 2);

	/* Get the color and symbol of the thrown object */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Extract a "distance multiplier" */
	mul = 6;

	/* Enforce a minimum weight of half a pound */
	div = ((i_ptr->weight > 5) ? i_ptr->weight : 5);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div;

	/* Max distance of 15 */
	if (tdis > 15) tdis = 15;

	/* Get object attributes */
	object_flags(i_ptr, &f1, &f2, &f3);

	/* Note impact object -- impact ammo doesn't work, sorry */
	if (f3 & (TR3_IMPACT))
	{
		if (!is_missile(i_ptr)) impact = TRUE;
	}


	/*
	 * Other thrown objects are easier to use, but only throwing weapons
	 * take advantage of bonuses to Skill and Deadliness from other
	 * equipped items.
	 */
	if (f1 & (TR1_THROWING))
	{
		chance = p_ptr->skill_tht + BTH_PLUS_ADJ * (p_ptr->to_h + i_ptr->to_h);
		total_deadliness = p_ptr->to_d + i_ptr->to_d;
	}
	else
	{
		chance = (3 * p_ptr->skill_tht / 2) + (BTH_PLUS_ADJ * i_ptr->to_h);
		total_deadliness = i_ptr->to_d;
	}

	/* Special case -- Half-Trolls are pretty good at throwing boulders */
	if ((p_ptr->prace == RACE_HALF_TROLL) &&
	    (i_ptr->tval == TV_JUNK) && (i_ptr->sval == SV_BOULDER))
	{
		chance += get_skill(S_THROWING, 5, 30);
	}


	/* Long-range accuracy greatly depends on throwing skill */
	inaccuracy = 8 - get_skill(S_THROWING, 0, 7);

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

	/* Calculate the path - travel through a missed target */
	path_n = project_path(path_g, tdis, py, px, &ty, &tx, PROJECT_THRU);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Collide with walls */
		if (!cave_floor_bold(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance */
		x = nx;
		y = ny;

		/* We've reached the original target -- reduce remaining moves */
		if ((y == ty) && (x == tx))
		{
			path_n -= rand_int(path_n - i);
		}

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistency */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			monster_lore *l_ptr = &l_list[m_ptr->r_idx];

			/* Calculate the projectile accuracy, modified by distance. */
			int chance2 =
				chance * (100 - (distance(py, px, y, x) * inaccuracy)) / 100;

			bool fear = FALSE;

			int dice;
			long die_average, temp, sides;

			/* Assume a default death */
			cptr note_dies = " dies.";

			/* Some monsters get "destroyed" */
			if (monster_nonliving(r_ptr))
			{
				/* Special note at death */
				note_dies = " is destroyed.";
			}

			/* Character is not blind, confused, or hallucinating */
			if ((!p_ptr->blind) && (!p_ptr->confused) && (!p_ptr->image))
			{
				/* Monster is visible.  Character is using throwing weapons */
				if ((m_ptr->ml) && (f1 & (TR1_THROWING)))
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

			/* Monsters in rubble can take advantage of cover. */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				terrain_adjust = r_ptr->ac / 5 + 5;
			}

			/*
			 * Monsters in trees can take advantage of cover, but those
			 * skilled in nature lore can hit them more easily.
			 */
			if (cave_feat[y][x] == FEAT_TREE)
			{
				terrain_adjust = r_ptr->ac / 7 + 5;
				terrain_adjust -= get_skill(S_NATURE, 0, 20);
			}

			/* Monsters in water are vulnerable. */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				terrain_adjust -= r_ptr->ac / 5;
			}


			/* Get "the monster" or "it" */
			monster_desc(m_name, m_ptr, 0);

			/* Some monsters are great at dodging  -EZ- */
			if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
			    (rand_int(5 + m_ptr->cdis) >= 3))
			{
				if (mon_fully_visible(m_ptr))
				{
					message_format(MSG_MISS, 0, "%^s dodges!", m_name);

					/* Learn that monster can dodge */
					l_ptr->flags2 |= (RF2_EVASIVE);
				}

				continue;
			}

			/* Did we hit it (penalize distance travelled) */
			if (!test_hit_combat(chance2 + sleeping_bonus, r_ptr->ac, m_ptr->ml))
			{
				/* Object whizzes right past the monster */
				continue;
			}

			/* Make some noise. */
			if (TRUE)
			{
				int noise = p_ptr->base_wakeup_chance / 2;

				/* Burglary skill greatly reduces noise */
				noise += 1000 - get_skill(S_BURGLARY, 0, 800);

				/* Increase the noise level */
				add_wakeup_chance += noise;
			}

			/* Note the collision */
			hit_body = TRUE;

			/* Reveal fully visible mimics */
			if ((m_ptr->mflag & (MFLAG_MIME)) && (m_ptr->ml) &&
			    (!(m_ptr->mflag & (MFLAG_DLIM))))
			{
				/* Reveal the monster */
				m_ptr->mflag &= ~(MFLAG_MIME);

				/* Get monster name again */
				monster_desc(m_name, m_ptr, 0x88);

				/* Message  XXX */
				msg_print("It was a mimic!");
			}

			/* Hack -- Track this monster race, if monster is visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

			/* Hack -- Track this monster, if visible */
			if (m_ptr->ml) health_track(cave_m_idx[y][x]);

			/* Practice the throwing skill */
			skill_being_used = S_THROWING;

			/* Potions smash */
			if (i_ptr->tval == TV_POTION)
			{
				break;
			}

			/* Other items hit the monster */

			/*
			 * The basic damage-determination formula is the same in
			 * throwing as it is in melee (with some ommisions for objects
			 * that are not throwing weapons).
			 */

			/* Base damage dice */
			dice = i_ptr->dd;

			/* Object is a throwing weapon. */
			if (f1 & (TR1_THROWING))
			{
				/* Critical hits may add damage dice. */
				dice += critical_shot(chance2 + sleeping_bonus, TRUE,
											 m_ptr->ml, m_name, i_ptr, m_ptr);
			}

			/* Ordinary thrown object */
			else
			{
				/* Display a default hit message. */
				if (m_ptr->ml)
				{
					message_format(MSG_HIT, 0, "The %s hits %s.", o_name, m_name);
				}
				else
				{
					message_format(MSG_HIT, 0, "The %s finds a mark.", o_name);
				}
			}

			/* Get the average value of a single damage die. (10x inflation) */
			die_average = (10 * (i_ptr->ds + 1)) / 2;

			/* Apply deadliness to average. (100x inflation) */
			apply_deadliness(&die_average, total_deadliness);

			/* Object is a throwing weapon. */
			if (f1 & (TR1_THROWING))
			{
				/* Bonuses depend on specialization */
				int max = 85;
				if (p_ptr->oath & (OATH_OF_IRON)) max = 100;
				else if (p_ptr->oath) max = 70;

				/*
				 * Multiply the die average by the throwing
				 * weapon multiplier, if applicable.  This is not the
				 * prettiest equation, but it does at least try to keep
				 * throwing weapons competitive.
				 */
				die_average *= get_skill(S_THROWING, 25, max);

				/* Perfectly balanced weapons do 50% extra damage. */
				if (f1 & (TR1_PERFECT_BALANCE))
				{
					die_average += die_average / 2;
				}

				/* Deflate */
				die_average /= 10;
			}

			/* Convert die average to die sides. */
			temp = (2L * die_average) - 1000;

			/* Calculate the actual number of sides to each die. */
			sides = div_round(temp, 1000);


			/* Roll out the damage. */
			damage = damroll(dice, (s16b)sides);

			/* Adjust damage for slays, brands, resists. */
			adjust_dam(&damage, i_ptr, m_ptr, FALSE);

			/* No negative damage */
			if (damage < 0) damage = 0;

			/* Hack -- The Lightning Lance */
			if (i_ptr->artifact_index == ART_LIGHTNING_LANCE)
			{
				project_beam(-1, MAX_SIGHT, p_ptr->py, p_ptr->px,
				             y, x, damage, GF_ELEC, 0L);
			}

			/* Hit the monster, check for death */
			else if (mon_take_hit(cave_m_idx[y][x], -1, damage,
				 &fear, note_dies))
			{
				/* Dead monster */
			}

			/* No death */
			else
			{
				/* Message */
				message_pain(cave_m_idx[y][x], damage);

				/* Monsters can be thrust back by an impact thrown object. */
				if (impact)
				{
					/* Minimum chance is 25%, and it rises for powerful hits */
					int perc = 25 + (100 * damage / m_ptr->maxhp);

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
							thrust_away(-1, m_ptr->fy, m_ptr->fx, MIN(3, 1 + k / 15));
						}
					}
				}

				/* Take note */
				if (fear && m_ptr->ml)
				{
					/* Sound */
					sound(SOUND_FLEE);

					/* Message */
					msg_format("%^s flees in terror!", m_name);
				}
			}

			/* Object has hit a monster -- it falls to the floor */
			break;
		}
	}

	/* Chance of breakage.   Throwing weapons are designed not to break. */
	if (f1 & (TR1_PERFECT_BALANCE))
	{
		break_chance = 0;
	}
	else if (f1 & (TR1_THROWING))
	{
		break_chance = ((hit_body || hit_wall) ? rand_int(2) : 0);
	}
	else
	{
		break_chance = ((hit_body || hit_wall) ? breakage_chance(i_ptr) : 0);
	}


	/* Potions always break when they hit a wall or a monster */
	if (i_ptr->tval == TV_POTION)
	{
		if (hit_body || hit_wall) break_chance = 100;
	}

	/* Object falls to the floor (as opposed to being dropped) */
	if (break_chance == 0) break_chance = -1;

	/* Drop (or break/smash) near that location */
	drop_near(i_ptr, break_chance, y, x);

	/* Chance to learn about throwing weapon (only if it survives) */
	if ((i_ptr->k_idx) && (f1 & (TR1_THROWING)) && (hit_body) &&
		 (i_ptr->iy != 0) && (i_ptr->ix != 0))
	{
		if (one_in_(8)) learn_about_wearable(i_ptr, -2, FALSE);
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
