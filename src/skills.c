/* File: skills.c */

/*
 * Sangband skills.  Cost of skills, increase, restore, and reduce skills.
 * Realms, Oaths, and specializations, effect of skills on each other, the
 * skills improvement screen.
 *
 * Copyright (c) 2007 Leon Marrick
 *
 * Based on originals by Julian Lighton, Michael Gorse, and Chris Petit
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Special limits on skills.  A "warrior" is a character that has taken the
 * Oath of Iron.  A "spellcaster" is a magic-using character that has taken
 * the Oath associated with his realm.  A "half-spellcaster" is a magic-user
 * that hasn't taken such an Oath.
 *
 * Note:  Holy warriors can raise their blunt weapon skill to the max.
 */
#define WARRIOR_SPELLCASTING_LIMIT    0  /* Warriors can't cast spells */


/* There are no longer limits - penalties to the skill are used instead. -JM */
#define NON_WARRIOR_LIMIT            100  /* Only Warriors are great in melee */
#define SPELLCASTER_WEAPON_LIMIT     100  /* Spellcasters aren't good in melee */
#define PIOUS_EDGED_WEAPON_LIMIT     100  /* Pious chars don't like swords */
#define PRIEST_BLUNT_WEAPON_LIMIT    100  /* Priests are OK with blunt weapons */

#define OATH_OF_IRON_REQ             25  /* Skill required to take the Oath of Iron */
#define NON_GUILD_LIMIT              20  /* Non-Guild Burglars struggle above this */
#define MAGIC_OATH_REQ               20  /* Skill required to take various magic oathes */

static bool cannot_learn_magic;
static bool cannot_learn_prayers;

/*
 * Variable - currently selected skill
 */
int selected = -1;

/* Array of old skill values */
skill_data old_pskills[NUM_SKILLS];

/*
 * Handle boosts from similar skill abilities
 */
static int get_skill_similar(int skill)
{
	int best = 0;

	switch(skill)
	{
		case S_SWORD:
		case S_POLEARM:
		case S_HAFTED:
		{
			best = MAX(best, p_ptr->pskills[S_SWORD].cur / 2);
			best = MAX(best, p_ptr->pskills[S_POLEARM].cur / 2);
			best = MAX(best, p_ptr->pskills[S_HAFTED].cur / 2);

			best = MAX(best, p_ptr->pskills[S_WRESTLING].cur / 3);
			best = MAX(best, p_ptr->pskills[S_KARATE].cur / 3);
			break;
		}
		case S_WRESTLING:
		case S_KARATE:
		{
			best = MAX(best, p_ptr->pskills[S_SWORD].cur / 3);
			best = MAX(best, p_ptr->pskills[S_POLEARM].cur / 3);
			best = MAX(best, p_ptr->pskills[S_HAFTED].cur / 3);

			best = MAX(best, p_ptr->pskills[S_WRESTLING].cur / 2);
			best = MAX(best, p_ptr->pskills[S_KARATE].cur / 2);
			break;
		}

		case S_BOW:
		case S_CROSSBOW:
		case S_SLING:
		case S_THROWING:
		{
			best = MAX(best, p_ptr->pskills[S_BOW].cur / 2);
			best = MAX(best, p_ptr->pskills[S_CROSSBOW].cur / 2);
			best = MAX(best, p_ptr->pskills[S_SLING].cur / 2);
			best = MAX(best, p_ptr->pskills[S_THROWING].cur / 2);

			break;
		}

		/* No related skill minimum */
		default:
		{
			return 0;
			break;
		}
	}

	return best;
}


/*
 * This function is used to get effective values for all skills.
 *
 * We use min and max to set bounds.  A non-zero min will shift the possible
 * range of outputs.  A non-standard difference between min and max
 * will stretch or shrink it.
 *
 * We use average character power when asked to calculate S_NOSKILL.
 *
 * We apply bonuses or penalties in some cases.
 *
 * Non-linear effective values are handled as special cases (see
 * especially "calc_bonuses()".
 */
s16b get_skill(int skill, int min, int max)
{
	int tmp, alt, std_max;


	/* Illegal or empty skill */
	if ((skill < 0) || (skill >= NUM_SKILLS)) return (0);

	/* Verify input maximum */
	if (max <= min) return (min);


	/* Get the current skill percentage */
	tmp = p_ptr->pskills[skill].cur;

	/* Give some partial credit for similar skills */
	alt = get_skill_similar(skill);
	if (alt > tmp) tmp = (tmp + alt) / 2;


	/*** Handle special cases ***/

	/* Oath of Iron weakens magical device skill greatly */
	if ((p_ptr->oath & (OATH_OF_IRON)) && (skill == S_DEVICE))
	{
		tmp = (tmp+1) * 2 / 3;
	}

	/* Not joining the Burglar's Guild weakens Burglary skill */
	else if (!(p_ptr->oath & (BURGLARS_GUILD)) && (skill == S_BURGLARY))
	{
		/* If you don't join the Guild, your max Burglary is 60 */
		if (tmp > NON_GUILD_LIMIT)
			tmp = NON_GUILD_LIMIT + (tmp - NON_GUILD_LIMIT) / 2;
	}


	/* Get the standard maximum */
	std_max = PY_MAX_POWER;


	/* If difference between the maximum and the minimum is non-standard, */
	if ((max - min) != std_max)
	{
		/* Stretch or shrink the range of possible outputs to fit (do not randomize). */
		tmp = (tmp * (max - min) + (std_max / 2)) / std_max;
	}

	/* Adjust the output value by the minimum */
	if (min)
	{
		tmp += min;
	}

	/* Return */
	return (tmp);
}


/*
 * Adjust skill effect depending on racial aptitude.
 *
 * See usage in "calc_bonuses()".
 */
s16b get_skill_race(int skill, int min, int max)
{
	int range = max - min;
	int factor;


	/* Some skills are strongly affected by racial aptitude */
	if (skill == S_SWORD || skill == S_HAFTED || skill == S_POLEARM ||
	    skill == S_CROSSBOW || skill == S_BOW || skill == S_SLING ||
	    skill == S_THROWING || skill == S_WRESTLING || skill == S_KARATE)
	{
		/* 1-point change in cost means ~8% change in base ability */
		factor = 2;
	}

	/* Some other skills are affected less by racial aptitude */
	else if (skill == S_DEVICE || skill == S_PERCEPTION || skill == S_STEALTH ||
	         skill == S_DISARM)
	{
		/* 1-point change in cost means ~5% change in base ability */
		factor = 10;
	}

	/* All other skills are not affected at all */
	else
	{
		factor = -999;
	}


	/* Make adjustments, depending on race */
	if ((range > 0) && (factor > -10))
	{
		/* Get racial ability to learn this skill (plus factor) */
		int cost     = factor + race_adj_cost_skill[skill][p_ptr->prace];
		int cost_ave = factor + 10;

		/* The greater the cost to learn, the less effective the skill */
		range = ((range * cost_ave) + (cost / 2)) / (cost);

		/* Spellcasters are bad at melee */
		if (p_ptr->realm && (skill == S_SWORD || skill == S_POLEARM
			|| skill == S_HAFTED || skill == S_WRESTLING || skill == S_KARATE))
		{
			/* Mages, Necromancers, and Druids */
			if (p_ptr->oath & (OATH_OF_SORCERY | YAVANNAS_FELLOWSHIP
				| BLACK_MYSTERY))
			{
				range = (range + 1) * 2 / 3;
			}

			/* Priests */
			else if (p_ptr->oath & COVENANT_OF_FAITH)
			{
				if (skill == S_SWORD || skill == S_POLEARM)
					range = (range + 1) / 2;
				else
					range = (range + 3) * 5 / 6;
			}

			/* Paladins */
			else if (p_ptr->realm == PRIEST)
			{
				if (skill == S_SWORD || skill == S_POLEARM)
					range = (range + 1) * 2 / 3;
			}

			/* Rogues, Assassins, and Rangers */
			else
				range = (range + 3) * 5 /6;
		}



		/* Adjust maximum */
		max = min + range;
	}

	/* Call the standard function */
	return (get_skill(skill, min, max));
}

int best_melee_skill(void)
{
	int sword, hafted, polearm;
	sword = get_skill(S_SWORD, 0, 100);
	hafted = get_skill(S_HAFTED, 0, 100);
	polearm = get_skill(S_POLEARM, 0, 100);

	if (sword >= hafted && sword >= polearm) return S_SWORD;
	if (hafted >= sword && hafted >= polearm) return S_HAFTED;
	if (polearm >= hafted && polearm >= sword) return S_POLEARM;

	/* Paranoia */
	return S_SWORD;
}

/*
 * Determine which melee weapon skill we're using.
 */
int sweapon(int tval)
{
	/* Check for a known weapon */
	if (tval == TV_SWORD) return (S_SWORD);
	if (tval == TV_HAFTED) return (S_HAFTED);
	if ((tval == TV_POLEARM) || (tval == TV_DIGGING)) return (S_POLEARM);

	/* Otherwise, we are using either wrestling or karate */
	return (p_ptr->barehand);
}

/*
 * Determine which archery skill we're using.
 */
int sbow(int tval)
{
	/* Check for a known missile weapon */
	if (tval == TV_SLING)    return (S_SLING);
	if (tval == TV_BOW)      return (S_BOW);
	if (tval == TV_CROSSBOW) return (S_CROSSBOW);

	/* Otherwise, assume none */
	return (S_NOSKILL);
}



/*
 * Description of magic realm (if any)
 */
static cptr realm_desc(void)
{
	switch (p_ptr->realm)
	{
		case MAGE:   return ("You know the sorcerous arts.");
		case PRIEST: return ("You serve a greater power.");
		case DRUID:  return ("You control the forces of nature.");
		case NECRO:  return ("You understand the forces of life and death.");
		default:
		{
			if (p_ptr->oath & (OATH_OF_IRON))
				return ("You are a specialist warrior.");
			else if (p_ptr->oath & (BURGLARS_GUILD))
				return ("You are a specialist burglar.");
			else
				return ("You are not familiar with any magical art.");
		}
	}
}

/*
 * Describe a magic-user
 */
static cptr realm_user_desc(void)
{
	switch (p_ptr->realm)
	{
		case MAGE:   return ("wizard");
		case PRIEST: return ("priest");    /* Could stand to be changed */
		case DRUID:  return ("druid");
		case NECRO:  return ("necromancer");
		default:
		{
			if (p_ptr->oath & (BURGLARS_GUILD)) return ("burglar");
			return ("warrior");
		}
	}
}


/*
 * Print something on the message line
 */
static void skill_msg(int attr, const char *msg)
{
	/* Clear part of line, position cursor */
	(void)Term_erase(0, 0, 55);
	(void)Term_gotoxy(2, 0);

	/* Display colored text */
	(void)Term_addstr(-1, attr, msg);
}


/*
 * Clear the message area of the skills screen
 */
static void erase_skill_comment(void)
{
	clear_row(18);
	clear_row(19);
	clear_row(20);
	clear_row(21);
}


/*
 * Print comments or warning about a skill
 */
static void skill_comment(int attr, const char *msg)
{
	/* Erase message */
	erase_skill_comment();

	/* Move the cursor, indent message */
	move_cursor(18, 7);

	/* Print new message */
	c_roff(attr, msg, 2, 78);
}


/*
 * Determine if this skill increase made a new talent available.
 */
static int check_for_new_talent(int skill, int level)
{
	int i, j;
	bool check;

	/* Scan the list of talents */
	for (i = 0; i < NUM_TALENTS; i++)
	{
		check = FALSE;

		/* Check all skills for talents */
		for (j = 0; j < talent_info[i].skill_count; j++)
		{
			if (talent_info[i].skill[j] == skill) check = TRUE;
		}

		/* Talent uses this skill */
		if (check)
		{
			/* Talent has just become available */
			if (talent_info[i].min_level == level)
			{
				/* Talent can in fact be used (or at least browsed) */
				if (can_use_talent(i, TALENT_WARRIOR | TALENT_UTILITY) > -1) return (i);
			}
		}
	}

	return (-1);
}



/*
 * Calculate character power.
 *
 * The experience needed to attain the current level in each skill is
 * calculated.  Racial cost adjustments are ignored here, as is unspent
 * experience.  The total is then matched against the master experience
 * table, and the highest power level that does not require more
 * experience than this total is accepted.
 *
 * Power is capped at 100.
 */
void calc_power(void)
{
	int i;

	/* Get total experience to attain current skill levels */
	s32b total = calc_spent_exp();

	/*
	 * Scan the experience table, and get the highest level that does not
	 * cost more than the accumulated exp.
	 */
	for (i = 1; i < PY_MAX_POWER; i++)
	{
		/* Stop when exp cost is too high for us to meet */
		if (player_exp[i+1] > total) break;
	}

	/* Save the power (ranges from 1 to 100) */
	p_ptr->power = i;
}


/*
 * Calculate a character's power for exp-gaining purposes.  To avoid
 * abuse, we use total exp, not just spent exp.
 */
int calc_exp_power(void)
{
	int i;
	s32b tot_exp = calc_spent_exp() + p_ptr->exp;

	/*
	 * Scan the experience table, and get the highest level that does not
	 * cost more than the accumulated exp.
	 */
	for (i = 1; i < PY_MAX_POWER; i++)
	{
		/* Stop when exp cost is too high for us to meet */
		if (player_exp[i+1] > tot_exp) break;
	}

	/* Use experience level or actual character power, whichever is higher */
	return (MAX(p_ptr->power, i));
}


/*
 * Calculate a character's power for exp-gaining purposes.  To avoid
 * abuse, we use total exp, not just spent exp.
 */
int calc_max_power(void)
{
	int i;
	s32b tot_exp = calc_spent_exp_max();

	/*
	 * Scan the experience table, and get the highest level that does not
	 * cost more than the accumulated exp.
	 */
	for (i = 1; i < PY_MAX_POWER; i++)
	{
		/* Stop when exp cost is too high for us to meet */
		if (player_exp[i+1] > tot_exp) break;
	}

	/* Use experience level or actual character power, whichever is higher */
	return (MAX(p_ptr->power, i));
}



/*
 * Calculate the practice penalty for a skill.
 *
 * This function is designed to encourage players not to instantly raise
 * their skills from zero to high levels, without forcing them to do
 * anything, or getting too harsh.
 *
 * Note that intrinsic skill cost or racial adjustments are ignored.  A
 * given amount of "practice_exp" means the same thing for all skills.
 *
 * In lieu of a real solution to the problem of some skills being harder
 * to gain experience in than others, we treat burglary and holy magic
 * more gently.
 */
static void practice_penalty(int skill, s32b *cost)
{
	int cur_level = p_ptr->pskills[skill].cur;
	int practice_level = 0;
	s32b exp = p_ptr->pskills[skill].practice_exp;

	int mult, deficit;
	s32b penalty;

	int i;
	int leeway;


	/* Check skills */
	switch (skill)
	{
		/* These skills have practice requirements */
		case S_SWORD:
		case S_HAFTED:
		case S_POLEARM:
		case S_CROSSBOW:
		case S_BOW:
		case S_SLING:
		case S_THROWING:
		case S_WRESTLING:
		case S_KARATE:
		case S_DISARM:
		{
			leeway = 4;
			break;
		}
		case S_DEVICE:
		{
			leeway = 6;
			break;
		}
		case S_BURGLARY:
		{
			leeway = 9;
			break;
		}
		case S_MAGIC:
		{
			if (p_ptr->realm == PRIEST) leeway = 6;
			else                        leeway = 4;
			break;
		}

		/* Other skills don't */
		default:
		{
			return;
		}
	}


	/*
	 * Scan the experience table, and get the highest level that does not
	 * cost more than the accumulated exp.
	 */
	for (i = 0; i <= PY_MAX_POWER; i++)
	{
		/* Stop when exp cost is too high for us to meet */
		if (player_exp[i] > exp) break;

		/* Remember this level */
		practice_level = i;
	}

	/* Get the "practice deficit".  Allow some leeway. */
	deficit = cur_level - (practice_level + leeway);

	/* No (significant) deficit, no penalty. */
	if (deficit <= 0) return;

	/* Use deficit to determine multiplier (base is 100) */
	mult = 20 * deficit;

	/* Use multiplier to determine penalty */
	penalty = *cost * mult / 100L;

	/* Penalty cannot exceed 3000 exp */
	if (penalty > 3000L) penalty = 3000L;

	/* Apply the penalty */
	*cost += penalty;
}


/*
 * Actually perform the calculations for cost/power/score penalty reduction.
 */
static s32b adv_cost_reduce_aux(int skill, int level, byte mode)
{
	s32b reduction = 0L;

	/* Case #1:  We want to calculate cost reduction */
	if (mode == 1)
	{
		s32b tmp, divisor;

		/* Get the difference in exp needed for this skill level and the next */
		tmp = player_exp[level + 1] - player_exp[level];

		/* Multiply by skill cost adjustment (10x inflation) */
		tmp *= (skill_info[skill].cost_adj);

		/* Divide by the experience adjustment, and deflate */
		divisor = (long)(EXP_ADJ * 10);

		/* Get the actual cost (standard rounding) */
		reduction = (tmp + divisor / 2) / divisor;
	}

	/* Case #2:  We want to calculate power reduction (x10 inflation) */
	else if (mode == 2)
	{
		/* Get the cost to get to the current skill level */
		reduction = player_exp[level];

		/* Apply level difficulty factor (10x inflation) */
		reduction *= skill_info[skill].cost_adj;

		/* Result will be deflated later */
	}

	/* Case #3:  We want to calculate score penalty reduction */
	else
	{
		/* Get maximum skill level and multiply by inherent difficulty */
		reduction = (s32b)(level * skill_info[skill].cost_adj);
	}

	return (reduction);
}


/*
 * Helper function to both "adv_cost" and "total_points()".  Calculate the
 * reduction in:  1) cost, 2) effect on character power, and 3) future score
 * penalty, of investing in this skill when you already have raised a paired
 * skill.
 *
 * The below is a monstrous hard-coded hack, but it's not as bad as what it
 * replaced.  In particular, skill costs, character power, and score now all
 * play by similar rules, and you get the same cost reduction regardless of
 * the order in which you raise skills (before minimum cost is applied).
 *
 * One rule that has to be applied differently for score and cost is the case
 * of both paired skills being equal.  In these cases, cost cannot be reduced -
 * because it would become possible to take advantage of the cost reduction
 * for /each/ skill instead of for the /pair/ - and power and score penalty
 * must be, but only for one of the two tests - because otherwise the benefits
 * would either be forfited or doubled.
 */
void adv_cost_reduce_similar(int skill, s32b *base_cost, byte mode)
{
	int level = p_ptr->pskills[skill].max;
	int skill_max = p_ptr->pskills[skill].max;

	/* Assume no cost, power, or score penalty reduction */
	s32b reduction = 0L;

	/*
	 * 3/4ths of Disarming is included in Burglary.  When calculating cost (or
	 * power), we require that the matching skill be higher.  When calculating
	 * score, we also test (once!) for equality.
	 */
	if ((skill == S_BURGLARY) &&
	    (((mode == 1) && (p_ptr->pskills[S_DISARM].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_DISARM].max >= skill_max))))
	{
		reduction += 3L * adv_cost_reduce_aux(S_DISARM, level, mode) / 4L;
	}
	if ((skill == S_DISARM) &&
	    (p_ptr->pskills[S_BURGLARY].max > skill_max))
	{
		reduction += 3L * adv_cost_reduce_aux(S_DISARM, level, mode) / 4L;
	}

	/* 1/3rd of Stealth is included in Burglary */
	if ((skill == S_BURGLARY) &&
	    (((mode == 1) && (p_ptr->pskills[S_STEALTH].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_STEALTH].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_STEALTH, level, mode) / 3L;
	}
	if ((skill == S_STEALTH) &&
	    (p_ptr->pskills[S_BURGLARY].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_STEALTH, level, mode) / 3L;
	}

	/* 1/3rd of Dodging is included in Burglary */
	if ((skill == S_BURGLARY) &&
	    (((mode == 1) && (p_ptr->pskills[S_DODGING].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_DODGING].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_DODGING, level, mode) / 3L;
	}
	if ((skill == S_DODGING) &&
	    (p_ptr->pskills[S_BURGLARY].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_DODGING, level, mode) / 3L;
	}


	/* 1/2nd of Jousting is included in Swordsmanship, and vice versa */
	if ((skill == S_SWORD) &&
	    (((mode == 1) && (p_ptr->pskills[S_POLEARM].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_POLEARM].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_POLEARM, level, mode) / 2L;
	}
	if ((skill == S_POLEARM) &&
	    (p_ptr->pskills[S_SWORD].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_SWORD, level, mode) / 2L;
	}

	/* 1/4th of Jousting and Swordsmanship are each included in Clubbing, and vice versa */
	if ((skill == S_SWORD) &&
	    (((mode == 1) && (p_ptr->pskills[S_HAFTED].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_HAFTED].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_HAFTED, level, mode) / 4L;
	}
	if ((skill == S_HAFTED) &&
	    (p_ptr->pskills[S_SWORD].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_SWORD, level, mode) / 4L;
	}
	if ((skill == S_POLEARM) &&
	    (((mode == 1) && (p_ptr->pskills[S_HAFTED].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_HAFTED].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_HAFTED, level, mode) / 4L;
	}
	if ((skill == S_HAFTED) &&
	    (p_ptr->pskills[S_POLEARM].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_POLEARM, level, mode) / 4L;
	}


	/* 1/3rd of Bows is included in Crossbows, and vice versa */
	if ((skill == S_BOW) &&
	    (((mode == 1) && (p_ptr->pskills[S_CROSSBOW].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_CROSSBOW].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_CROSSBOW, level, mode) / 3L;
	}
	if ((skill == S_CROSSBOW) &&
	    (p_ptr->pskills[S_BOW].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_BOW, level, mode) / 3L;
	}

	/* 1/6th of Bows and Crossbows are each included in Slings, and vice versa */
	if ((skill == S_BOW) &&
	    (((mode == 1) && (p_ptr->pskills[S_SLING].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_SLING].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_SLING, level, mode) / 6L;
	}
	if ((skill == S_SLING) &&
	    (p_ptr->pskills[S_BOW].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_BOW, level, mode) / 6L;
	}
	if ((skill == S_CROSSBOW) &&
	    (((mode == 1) && (p_ptr->pskills[S_SLING].max >  skill_max)) ||
	     ((mode != 1) && (p_ptr->pskills[S_SLING].max >= skill_max))))
	{
		reduction += 1L * adv_cost_reduce_aux(S_SLING, level, mode) / 6L;
	}
	if ((skill == S_SLING) &&
	    (p_ptr->pskills[S_CROSSBOW].max > skill_max))
	{
		reduction += 1L * adv_cost_reduce_aux(S_CROSSBOW, level, mode) / 6L;
	}

	/* Sanity check:  Cost reduction cannot be more than 90% */
	if (reduction > 9L * *base_cost / 10L)
		reduction = 9L * *base_cost / 10L;

	/* Adjust cost */
	*base_cost -= reduction;
}


/*
 * Calculate the experience point cost to raise the given skill one
 * percentage point.
 *
 * Cost depends on level of skill, inherent difficulty of skill, and
 * racial suitability.
 *
 * Cost can be raised (sometimes by quite a lot) because the character
 * power is much higher then the skill level, or because the skill is
 * poorly practiced.  The point of these two adjustments is to avoid
 * high-level characters from instantly buying up new skills on the cheap,
 * while still allowing the player to do almost anything he's prepared to
 * pay for.
 *
 * This function handles errors by returning -1.
 */
s32b adv_cost(int skill, bool add_practice_cost)
{
	s32b tmp, cost, divisor;

	int level = p_ptr->pskills[skill].cur;


	/* Paranoia - skill must be less than 100 */
	if (level >= PY_MAX_POWER) return (-1);


	/* Get the difference in exp needed for this skill level and the next */
	tmp = player_exp[level + 1] - player_exp[level];

	/* Multiply by skill cost adjustment (10x inflation) */
	tmp *= (skill_info[skill].cost_adj);

	/* Multiply by racial adjustment (10x inflation) */
	tmp *= race_adj_cost_skill[skill][p_ptr->prace];

	/* Divide by the experience adjustment, and deflate */
	divisor = (long)(EXP_ADJ * 100);


	/* Get the actual cost (standard rounding) */
	cost = (tmp + divisor / 2L) / divisor;

	/* Similar skills reduce cost for each other */
	adv_cost_reduce_similar(skill, &cost, 1);


	/* Drained skills are three times as easy to restore */
	if (level < p_ptr->pskills[skill].max)
	{
		cost /= 3L;
	}

	/* If the skill is not drained, ... */
	else
	{
		/* Apply minimum cost rules */
		if (p_ptr->power >= 10)
		{
			int tmp_pow = MAX(1, p_ptr->power - 10);

			/* Minimum cost depends on character power */
			s32b min_cost = player_exp[tmp_pow] - player_exp[tmp_pow - 1];

			/* It also depends on the inherent cost of the skill */
			min_cost *= skill_info[skill].cost_adj;
			min_cost *= race_adj_cost_skill[skill][p_ptr->prace];

			/* Deflate */
			min_cost /= (EXP_ADJ * 100L);

			/* Enforce minimum */
			if (cost < min_cost) cost = min_cost;
		}

		/* Apply practice penalty, if any */
		if (add_practice_cost) practice_penalty(skill, &cost);
	}


	/* Cost must always be at least one */
	if (cost < 1L) cost = 1L;

	/* Round off cost (slightly) */
	cost = round_it(cost, 50);

	/* Return cost */
	return (cost);
}


/*
 * Determine if the character can take a (new) oath
 */
static bool can_take_oath(byte oath)
{
	/* Character cannot (currently) take more than one oath */
	if (p_ptr->oath) return (FALSE);

	/* Check individual oaths */
	switch (oath)
	{
		case OATH_OF_IRON:
		{
			/* Must not have any magic */
			if (p_ptr->pskills[S_MAGIC].max > WARRIOR_SPELLCASTING_LIMIT)
				return (FALSE);

			/* Require skill at any form of non-magical combat */
			if ((p_ptr->pskills[S_SWORD].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_HAFTED].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_POLEARM].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_CROSSBOW].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_BOW].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_SLING].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_THROWING].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_WRESTLING].max < OATH_OF_IRON_REQ) &&
			    (p_ptr->pskills[S_KARATE].max < OATH_OF_IRON_REQ))
			{
				return (FALSE);
			}
			break;
		}

		case OATH_OF_SORCERY:
		case YAVANNAS_FELLOWSHIP:
		case BLACK_MYSTERY:
		{
			/* Must not be too focused on melee weapons */
			if (p_ptr->pskills[S_SWORD].max > SPELLCASTER_WEAPON_LIMIT)
				return (FALSE);
			if (p_ptr->pskills[S_POLEARM].max > SPELLCASTER_WEAPON_LIMIT)
				return (FALSE);
			if (p_ptr->pskills[S_HAFTED].max > SPELLCASTER_WEAPON_LIMIT)
				return (FALSE);

			/* Must already know the realm of magic */
			/* And meet the minimum required realm skill -JM */
			if ((oath == OATH_OF_SORCERY) &&
				((p_ptr->realm != MAGE) || (get_skill(S_WIZARDRY,0,100) < MAGIC_OATH_REQ)))
				return (FALSE);
			if ((oath == YAVANNAS_FELLOWSHIP) &&
				((p_ptr->realm != DRUID) || (get_skill(S_NATURE,0,100) < MAGIC_OATH_REQ)))
				return (FALSE);
			if ((oath == BLACK_MYSTERY) &&
				((p_ptr->realm != NECRO)|| (get_skill(S_DOMINION,0,100) < MAGIC_OATH_REQ)))
				return (FALSE);
			break;
		}

		case COVENANT_OF_FAITH:
		{
			/* Must already be pious */
			if (p_ptr->realm != PRIEST) return (FALSE);

			/* And meet the minimum required Piety -JM */
			if (get_skill(S_PIETY,0,100) < MAGIC_OATH_REQ) return (FALSE);

			/* Must not be too focused on melee weapons */
			if (p_ptr->pskills[S_SWORD].max > PIOUS_EDGED_WEAPON_LIMIT)
				return (FALSE);
			if (p_ptr->pskills[S_POLEARM].max > PIOUS_EDGED_WEAPON_LIMIT)
				return (FALSE);
			if (p_ptr->pskills[S_HAFTED].max > PRIEST_BLUNT_WEAPON_LIMIT)
				return (FALSE);

			break;
		}


		case BURGLARS_GUILD:
		{
			/* Must have a Burglary skill of at least 20 */
			if (p_ptr->pskills[S_BURGLARY].max < LEV_REQ_GUILD) return (FALSE);

			break;
		}

		default:
		{
			return (FALSE);
		}
	}

	/* Allow this oath */
	return (TRUE);
}



/* Warning defines */
#define PIOUS_EDGED            1
#define PRIEST_BLUNT_WEAPON    2
#define SPELLCASTER_WEAPON     3
#define NON_WARRIOR            4
#define WARRIOR_DEVICE         5
#define WARRIOR_SPELLS         6
#define PIETY_AND_DOMINION     7
#define THIEVING               8


/*
 * Warn players if raising a skill would cost them opportunities to take
 * certain oaths or magics.
 */
static bool can_raise_skill_confirm(int warning)
{
	cptr warn_text = "";
	char ch;

	bool accept = FALSE;

	/* Display waning text */
	switch (warning)
	{
		case PIOUS_EDGED:
		{
			warn_text = "If you raise this skill any higher, you will not be able to learn Piety.";
			break;
		}

		case PRIEST_BLUNT_WEAPON:
		{
			warn_text = "If you raise this skill any higher, you will not be able to subscribe to the Covenant of Faith.";
			break;
		}

		case SPELLCASTER_WEAPON:
		{
			warn_text = "If you raise this skill any higher, you will not be able to take a spellcaster's Oath.  You can still become a non-specialist spellcaster, though.";
			break;
		}

		case NON_WARRIOR:
		{
			warn_text = "If you raise this skill any higher, you will not be able to learn magic   (however, pious characters who have not subscribed to the Covenant of Faith can raise their blunt weapons skill to the maximum).";
			break;
		}

		case WARRIOR_DEVICE:
		{
			warn_text = "If you raise this skill any higher, you will not be able to take the Oath of Iron.";
			break;
		}

		case WARRIOR_SPELLS:
		{
			warn_text = "If you learn spells, you will not be able to take the Oath of Iron.";
			break;
		}

		case PIETY_AND_DOMINION:
		{
			warn_text = "Never can one character learn both holy Piety and dark Blood Dominion.  If you choose to raise one skill, the other will be set to zero.";
			break;
		}

		case THIEVING:
		{
			warn_text = "If you raise this skill any higher, shopkeepers will start to notice; your Charisma will drop by 2.";
		}


		/* Called with an undefined warning - assume acceptable  XXX XXX */
		default:  return (TRUE);
	}


	/* Warning */
	skill_comment(TERM_L_BLUE, warn_text);

	/* Ask */
	skill_msg(TERM_WHITE, "Advance this skill? [y/n] ");

	/* Ask until we get a clear answer */
	while (TRUE)
	{
		/* Answer */
		ch = inkey(FALSE);

		/* Note accept */
		if ((ch == 'y') || (ch == 'Y'))
		{
			accept = TRUE;
			break;
		}

		/* Note cancel */
		if ((ch == 'n') || (ch == 'N') || (ch == ESCAPE))
		{
			accept = FALSE;
			skill_msg(TERM_WHITE, "Cancelled.");
			break;
		}
	}


	/* Erase message */
	erase_skill_comment();

	/* Hide cursor XXX */
	move_cursor(26, 0);

	/* Return our answer */
	return (accept);
}


/*
 * Enforce limits on skills.
 *
 * Optionally, output explanatory messages.
 *
 * Allow skills to be raised automatically unless absolutely forbidden,
 * raised after any warnings are read, or raised only if always permitted.
 *
 * Return TRUE if skill can be raised by 1%.
 */
static int can_raise_skill(int skill, bool verbose, int auto_raise)
{

	int lev = p_ptr->pskills[skill].max;

	/* Cap maximum power at 100 -JM */
	if (calc_max_power() == PY_MAX_POWER && !no_skill_cap && (p_ptr->pskills[skill].cur == p_ptr->pskills[skill].max))
	{
		if (verbose) prt("You need no more practice.",18,2);
		return (-1);
	}


	/* Assume able to learn magic */
	cannot_learn_magic   = FALSE;
	cannot_learn_prayers = FALSE;

	/* Can never raise any skill past 100 */
	if (p_ptr->pskills[skill].cur >= PY_MAX_POWER) return (-1);


	/* If we've taken an oath, some warnings are unnecessary */
	if ((p_ptr->oath) && (!auto_raise)) auto_raise = 1;

	/* If the skill is not at maximum, we always allow advances */
	if (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].max)
		return (TRUE);


	/* Handle individual skills */
	switch (skill)
	{
		case S_SWORD:
		case S_POLEARM:
		{
			/* Character has no magic realm */
			if (p_ptr->realm == NONE)
			{
				/* We haven't taken the Oath of Iron */
				if (!(p_ptr->oath & (OATH_OF_IRON)))
				{
					/* Next skill raise would exceed the piety limit */
					if (lev == PIOUS_EDGED_WEAPON_LIMIT)
					{
						/* We already can't become a priest */
						if ((p_ptr->pskills[S_SWORD].max >
						         PIOUS_EDGED_WEAPON_LIMIT) ||
							 (p_ptr->pskills[S_POLEARM].max >
							      PIOUS_EDGED_WEAPON_LIMIT))
						{
							return (TRUE);
						}

						/* Automatically accept or reject */
						if (auto_raise) return (auto_raise > 0);

						/* Manually accept or reject */
						else
						{
							return (can_raise_skill_confirm(PIOUS_EDGED));
						}
					}

					/* Next skill raise would exceed the spellcaster limit */
					if (lev == SPELLCASTER_WEAPON_LIMIT)
					{
						/* We already can't become a pure spellcaster */
						if ((p_ptr->pskills[S_SWORD].max >
						         SPELLCASTER_WEAPON_LIMIT) ||
							 (p_ptr->pskills[S_POLEARM].max >
							      SPELLCASTER_WEAPON_LIMIT) ||
							 (p_ptr->pskills[S_HAFTED].max >
							      SPELLCASTER_WEAPON_LIMIT))
						{
							return (TRUE);
						}

						if (auto_raise) return (auto_raise > 0);
						else
						{
							return (can_raise_skill_confirm(SPELLCASTER_WEAPON));
						}
					}

					/* Next skill raise would exceed the half-spellcaster limit */
					if (lev == NON_WARRIOR_LIMIT)
					{
						/* We already can't become a half-spellcaster */
						if ((p_ptr->pskills[S_SWORD].max > NON_WARRIOR_LIMIT) ||
							 (p_ptr->pskills[S_POLEARM].max > NON_WARRIOR_LIMIT) ||
							 (p_ptr->pskills[S_HAFTED].max > NON_WARRIOR_LIMIT))
						{
							return (TRUE);
						}

						if (auto_raise) return (auto_raise > 0);
						else
						{
							return (can_raise_skill_confirm(NON_WARRIOR));
						}
					}
				}

				/* Allow all other raises */
				return (TRUE);
			}

			/* Character is among the pious */
			else if (p_ptr->realm == PRIEST)
			{
				if (lev < PIOUS_EDGED_WEAPON_LIMIT) return (TRUE);

				else if (verbose) prt("The pious cannot raise their edged weapons skills any higher than this.", 1, 2);
				return (-1);
			}


			/* Character is a pure spellcaster of another realm */
			else if ((p_ptr->oath & (OATH_OF_SORCERY)) ||
			         (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) ||
			         (p_ptr->oath & (BLACK_MYSTERY)))
			{
				/* Allow raises up to the limit */
				if (lev < SPELLCASTER_WEAPON_LIMIT) return (TRUE);

				if (verbose) prt("A specialist spellcaster cannot raise his weapons skills any higher than this.", 1, 2);
				return (-1);
			}

			/* Character is a non-specialist spellcaster */
			else
			{
				/* Next skill raise would exceed the pure spellcaster limit */
				if (lev == SPELLCASTER_WEAPON_LIMIT)
				{
					if (auto_raise) return (auto_raise > 0);
					else
					{
						return (can_raise_skill_confirm(SPELLCASTER_WEAPON));
					}
				}

				/* Allow raises up to the limit */
				else if (lev < NON_WARRIOR_LIMIT) return (TRUE);

				if (verbose) prt("A magic-user cannot raise his weapons skills any higher than this.", 1, 2);
				return (-1);
			}

			break;
		}

		case S_HAFTED:
		{
			/* Character has no magic realm */
			if (p_ptr->realm == NONE)
			{
				/* We haven't taken the Oath of Iron */
				if (!(p_ptr->oath & (OATH_OF_IRON)))
				{
					/* Next skill raise would exceed the pure spellcaster limit */
					if (lev == SPELLCASTER_WEAPON_LIMIT)
					{
						/* We already can't become a pure spellcaster */
						if ((p_ptr->pskills[S_SWORD].max >
						         SPELLCASTER_WEAPON_LIMIT) ||
							 (p_ptr->pskills[S_POLEARM].max >
							      SPELLCASTER_WEAPON_LIMIT) ||
							 (p_ptr->pskills[S_HAFTED].max >
							      SPELLCASTER_WEAPON_LIMIT))
						{
							return (TRUE);
						}

						if (auto_raise) return (auto_raise > 0);
						else
						{
							return (can_raise_skill_confirm(SPELLCASTER_WEAPON));
						}
					}

					/* Next skill raise would exceed the half-spellcaster limit */
					if (lev == NON_WARRIOR_LIMIT)
					{
						/* We already can't become a half-spellcaster */
						if ((p_ptr->pskills[S_SWORD].max > NON_WARRIOR_LIMIT) ||
							 (p_ptr->pskills[S_POLEARM].max > NON_WARRIOR_LIMIT) ||
							 (p_ptr->pskills[S_HAFTED].max > NON_WARRIOR_LIMIT))
						{
							return (TRUE);
						}

						if (auto_raise) return (auto_raise > 0);
						else
						{
							return (can_raise_skill_confirm(NON_WARRIOR));
						}
					}
				}

				/* Allow all other raises */
				return (TRUE);
			}


			/* Character is among the pious */
			else if (p_ptr->realm == PRIEST)
			{
				/* Character has taken the Covenant of Faith */
				if (p_ptr->oath & (COVENANT_OF_FAITH))
				{
					if (lev < PRIEST_BLUNT_WEAPON_LIMIT) return (TRUE);

					if (verbose) prt("The Covenant of Faith forbids you to focus on blunt weapons any more than this.", 1, 2);
					return (-1);
				}

				/* Character hasn't */
				else
				{
					/* Next skill raise would forbid specialization in piety */
					if (lev == PRIEST_BLUNT_WEAPON_LIMIT)
					{
						if (auto_raise) return (auto_raise > 0);
						else
						{
							return (can_raise_skill_confirm(PRIEST_BLUNT_WEAPON));
						}
					}

					/* No limits */
					return (TRUE);
				}
			}

			/* Character is a pure spellcaster of another realm */
			else if ((p_ptr->oath & (OATH_OF_SORCERY)) ||
			         (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) ||
			         (p_ptr->oath & (BLACK_MYSTERY)))
			{
				if (lev < SPELLCASTER_WEAPON_LIMIT) return (TRUE);

				if (verbose) prt(format("Oath-bound %ss cannot raise their weapon skills any higher than this.", realm_user_desc()), 1, 2);
				return (-1);
			}

			/* Character is a non-specialist spellcaster */
			else
			{
				/* Next skill raise would forbid specialization in magic */
				if (lev == SPELLCASTER_WEAPON_LIMIT)
				{
					if (auto_raise) return (auto_raise > 0);
					else
					{
						return (can_raise_skill_confirm(SPELLCASTER_WEAPON));
					}
				}

				if (lev < NON_WARRIOR_LIMIT) return (TRUE);

				if (verbose) prt("Spellcasters of your realm cannot raise their weapons skills any higher than this.", 1, 2);
				return (-1);
			}

			break;
		}

		case S_WRESTLING:
		case S_KARATE:
		{
			/* Reminder of how to switch between martial arts */
			if ((lev == 0) && (verbose))
			{
				skill_comment(TERM_L_BLUE,
					"You may use the '|' key to change barehanded combat methods.");
			}

			break;
		}

		case S_MAGIC:
		{
			/* The Oath of Iron forbids the learning of magic */
			if (p_ptr->oath & (OATH_OF_IRON))
			{
				if (verbose) prt("The Oath of Iron forbids the learning of magic.",
					1, 2);
				return (-1);
			}

			/* Notice possibility of taking the Oath of Iron */
			if (can_take_oath(OATH_OF_IRON))
			{
				if (auto_raise) return (auto_raise > 0);
				else
				{
					if (!can_raise_skill_confirm(WARRIOR_SPELLS))
						return (-1);
				}
			}

			/* Must not be too focused on melee weapons */
			if ((p_ptr->pskills[S_SWORD].max > NON_WARRIOR_LIMIT) ||
			    (p_ptr->pskills[S_POLEARM].max > NON_WARRIOR_LIMIT) ||
			    (p_ptr->pskills[S_HAFTED].max > NON_WARRIOR_LIMIT))
			{
				cannot_learn_magic = TRUE;
			}

			/* Check for priestly rules */
			if ((p_ptr->pskills[S_SWORD].max > PIOUS_EDGED_WEAPON_LIMIT) ||
			    (p_ptr->pskills[S_POLEARM].max > PIOUS_EDGED_WEAPON_LIMIT))
			{
				cannot_learn_prayers = TRUE;
			}

			/* Explain limitations */
			if ((cannot_learn_magic) && (cannot_learn_prayers))
			{
				if (verbose) prt("You are too much the fighter to learn spells or prayers.",
					1, 2);
				return (-1);
			}

			/* Explain limitations */
			else if (cannot_learn_magic)
			{
				if (verbose) prt("You are too much the fighter to learn spells, but you can learn prayers.",
					1, 2);
				return (TRUE);
			}

			/* Explain limitations */
			else if (cannot_learn_prayers)
			{
				if (verbose) prt("You are too much the fighter to learn prayers, but you can learn spells.",
					1, 2);
				return (TRUE);
			}

			break;
		}

		case S_MPOWER:
		{
			/* The Oath of Iron forbids the learning of magic */
			if (p_ptr->oath & (OATH_OF_IRON))
			{
				if (verbose) prt("The Oath of Iron forbids the learning of magic.", 1, 2);
				return (-1);
			}

			/* No realm chosen -- Ask the player to increase Spellcasting */
			else if (!p_ptr->realm)
			{
				if (verbose) prt("Raise your Spellcasting skill to choose a magic realm.", 1, 2);
				return (-1);
			}

			break;
		}

		case S_PIETY:
		{
			/* Necromantic magic forbids holy alliance */
			if (p_ptr->realm == NECRO)
			{
				if (verbose) prt("Users of the necromantic arts cannot learn Holy Alliance.", 1, 2);

				/* Paranoia -- set Holy Alliance to zero */
				p_ptr->pskills[S_PIETY].max = p_ptr->pskills[S_PIETY].cur = 0;

				return (-1);
			}

			/* Increasing Piety sets Blood Dominion to zero */
			if (p_ptr->pskills[S_DOMINION].max > 0)
			{
				/* We never automatically raise a forbidden skill */
				if (auto_raise) return (-1);

				/* We may allow the player to manually raise the skill */
				else
				{
					return (can_raise_skill_confirm(PIETY_AND_DOMINION));
				}
			}
			break;
		}

		case S_DOMINION:
		{
			/* Holy lore forbids gaining dark knowledge */
			if (p_ptr->oath & (COVENANT_OF_FAITH))
			{
				if (verbose) prt("Acolytes of holy lore cannot learn Blood Dominion.", 1, 2);

				/* Paranoia -- set Blood Dominion to zero */
				p_ptr->pskills[S_DOMINION].max = p_ptr->pskills[S_DOMINION].cur = 0;

				return (-1);
			}

			/* Increasing Blood Dominion sets Piety to zero */
			if (p_ptr->pskills[S_PIETY].max > 0)
			{
				/* We never automatically raise a forbidden skill */
				if (auto_raise) return (-1);

				/* We may allow the player to manually raise the skill */
				else
				{
					return (can_raise_skill_confirm(PIETY_AND_DOMINION));
				}
			}
			break;
		}

		default:
		{
			/* Assume no limits */
			return (TRUE);
		}
	}


	/* Assume no limits */
	return (TRUE);
}


/*
 * Increase or decrease the given skill.  This function is used for all
 * adjustments to skills other than those purchased by the player.
 *
 * Will never raise a skill above 100% or below 0.
 * Should not raise skills if this restricts the player's options.  XXX XXX
 *
 * Should never be called with a value for change that is not -1 or 1.
 */
bool alter_skill(int skill, int change, bool perm)
{
	/* Check inputs */
	if (change >  1) change =  1;
	if (change < -1) change = -1;
	if (change == 0) return (FALSE);

	/* We want to raise a skill */
	if (change == 1)
	{
		/* Check to see if the skill can be raised */
		if (can_raise_skill(skill, FALSE, -1) <= 0) return (FALSE);

		/* Adjust maximum skill */
		if (p_ptr->pskills[skill].max == p_ptr->pskills[skill].cur)
		{
			int new_talent;

			/* Adjust maximum skill */
			p_ptr->pskills[skill].max++;

			/* Check for new talents */
			new_talent = check_for_new_talent(skill, p_ptr->pskills[skill].max);

			/* Note that a new talent is available */
			if (new_talent >= 0)
			{
				msg_format("You can now use the talent \"%s\".",
					talent_info[new_talent].name);
			}
		}

		/* Adjust skill */
		p_ptr->pskills[skill].cur++;
	}

	/* We want to lower a skill */
	else if (change == -1)
	{
		/* Cannot reduce this skill any more */
		if (p_ptr->pskills[skill].cur == 0)
		{
			if (!perm) return (FALSE);
			else if (p_ptr->pskills[skill].max == 0) return (FALSE);
		}

		/* Adjust skill */
		if (p_ptr->pskills[skill].cur >= 1) p_ptr->pskills[skill].cur--;

		/* Optionally, adjust maximum skill */
		if (perm)
		{
			if (p_ptr->pskills[skill].max >= 1)
			    p_ptr->pskills[skill].max--;
		}
	}


	/* Recalculate character power */
	calc_power();

	/* Redraw and update some stuff */
	p_ptr->redraw |= (PR_EXP | PR_HP | PR_TITLE);
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SCORE);


	/* Success */
	return (TRUE);
}


/*
 * Raise skills randomly.  Used for potions of Gain Skill and similar.
 *
 * Return TRUE if anything was noticed.
 */
bool raise_skills(int amount)
{
	int i, skill, level, cost;
	s32b k;

	bool notice = FALSE;
	s16b skill_raise_chance[NUM_SK_USED];
	s32b skill_raise_total = 0L;

	/* Build a table of increasable skills */
	for (i = 0; i < NUM_SK_USED; i++)
	{
		/* Get maximum level of this skill */
		level = p_ptr->pskills[i].max;

		/* Bias in favour of higher-level skills */
		skill_raise_chance[i] = level * level;

		/* Sum up probabilities */
		skill_raise_total += level * level;
	}

	/* Raise skills -- if any exist to raise */
	if (skill_raise_total)
	{
		for (i = 0; i < 200; i++)
		{
			/* Randomize */
			k = rand_int(skill_raise_total);

			/* Find the skill */
			for (skill = S_SWORD; skill < NUM_SK_USED; skill++)
			{
				/* Found the entry */
				if (k < skill_raise_chance[skill]) break;

				/* Decrement */
				k -= skill_raise_chance[skill];
			}

			/* Get the cost of raising this skill */
			cost = adv_cost(skill, TRUE);

			/* Cost too high.  Try again. */
			if (cost > amount) continue;

			/* Try to raise the skill */
			if (alter_skill(skill, 1, TRUE))
			{
				/* Notice */
				notice = TRUE;

				/* Message */
				msg_format("You feel your %s improving.",
					skill_info[skill].desc);

				/* Charge cost */
				amount -= cost;


				/* Handle special cases XXX XXX */
				if ((skill == S_INFUSION) &&
					 (p_ptr->pskills[skill].max >= LEV_REQ_GRENADE))
				{
					/* Hack - learn about potions of essences */
					k_info[lookup_kind(TV_POTION,
						SV_POTION_GRENADE)].special |=
						(SPECIAL_AWARE | SPECIAL_EVER_SEEN);
				}
			}
		}
	}

	/* Stash leftover experience in the unspent pool */
	if (amount > 0)
	{
		p_ptr->exp += amount;
		notice = TRUE;
	}

	/* Return "notice" */
	return (notice);
}


/*
 * Raise a given skill by one percent, if allowed.
 *
 * Used when purchasing skill advances.
 */
static int adv_skill(int skill, bool pay_exp)
{
	s32b cost;
	int advance;

	/* Get the cost of raising this skill */
	cost = adv_cost(skill, TRUE);

	/* Note maximum level reached */
	if (cost == -1)
	{
		prt("You have raised this skill to the maximum.", 1, 2);
		pause_for(250);
		return (-1);
	}

	/* Note too expensive */
	if ((pay_exp) && (cost > p_ptr->exp))
	{
		prt("You do not have enough unspent experience to raise this skill.", 1, 2);
		pause_for(250);
		return (-1);
	}

	/* Check to see if the skill can be raised, output error messages */
	advance = can_raise_skill(skill, TRUE, 0);

	/* Skill is forbidden, or raise was cancelled */
	if (advance <= 0) return (advance);


	/* Advance the skill by 1% */
	p_ptr->pskills[skill].cur++;

	/* Advance maximum skill if appropriate */
	if (p_ptr->pskills[skill].cur > p_ptr->pskills[skill].max)
	{
		int new_talent;

		p_ptr->pskills[skill].max = p_ptr->pskills[skill].cur;

		/* Check for new talents */
		new_talent = check_for_new_talent(skill, p_ptr->pskills[skill].max);

		/* Note that a new talent is available */
		if (new_talent >= 0)
		{
			/* Print notice */
			skill_comment(TERM_L_BLUE, format("You can now use the talent \"%s\".",
				talent_info[new_talent].name));
		}

		/* Archery */
		if ((skill == S_CROSSBOW) || (skill == S_BOW) || (skill == S_SLING))
		{
			int old_ammo_tval = p_ptr->ammo_tval;
			int i, j;

			/* Hack -- assume we're ready to use this skill */
			if (skill == S_CROSSBOW) p_ptr->ammo_tval = TV_BOLT;
			if (skill == S_BOW) p_ptr->ammo_tval = TV_ARROW;
			if (skill == S_SLING) p_ptr->ammo_tval = TV_SHOT;

			/* Do we get an increase in shooting speed? */
			i = missile_bonus(TR_PVAL_SHOTS, p_ptr->pskills[skill].cur);
			j = missile_bonus(TR_PVAL_SHOTS, p_ptr->pskills[skill].cur - 1);

			if (i > j) skill_comment(TERM_L_BLUE, "You now shoot more quickly.");

			/* Do we get an increase in shooting force? */
			i = missile_bonus(TR_PVAL_MIGHT, p_ptr->pskills[skill].cur);
			j = missile_bonus(TR_PVAL_MIGHT, p_ptr->pskills[skill].cur - 1);

			if (i > j) skill_comment(TERM_L_BLUE, "You now shoot with greater force.");

			/* Hack -- restore actual ammo */
			p_ptr->ammo_tval = old_ammo_tval;
		}

		/* Wrestling */
		if (skill == S_WRESTLING)
		{
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_STAT1)
				skill_comment(TERM_L_BLUE, "Your strength increases.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_STAT2)
				skill_comment(TERM_L_BLUE, "Your strength increases.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_STAT3)
				skill_comment(TERM_L_BLUE, "Your strength increases.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_FA)
				skill_comment(TERM_L_BLUE, "You feel protected from slowing and paralysis.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_RESIST)
				skill_comment(TERM_L_BLUE, "Your are no longer troubled by sound attacks.");

		}

		/* Karate */
		if (skill == S_KARATE)
		{
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_STAT1)
				skill_comment(TERM_L_BLUE, "Your dexterity increases.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_STAT2)
				skill_comment(TERM_L_BLUE, "Your dexterity increases.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_STAT3)
				skill_comment(TERM_L_BLUE, "Your dexterity increases.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_FA)
				skill_comment(TERM_L_BLUE, "You feel protected from slowing and paralysis.");
			if (p_ptr->pskills[skill].max == LEV_REQ_MARTIAL_RESIST)
				skill_comment(TERM_L_BLUE, "Your are no longer troubled by confusion attacks.");
			if (p_ptr->pskills[skill].max == LEV_REQ_KARATE_SPEED1)
				skill_comment(TERM_L_BLUE, "You accelerate.");
			if (p_ptr->pskills[skill].max == LEV_REQ_KARATE_SPEED2)
				skill_comment(TERM_L_BLUE, "You accelerate.");

		}

		/* Piety */
		if (skill == S_PIETY)
		{
			/* We have an increased light radius */
			if (p_ptr->pskills[skill].max == LEV_REQ_XTRA_LIGHT)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You start to shine with holy light.");
			}
		}

		/* Advancing burglary */
		if (skill == S_BURGLARY)
		{
			/* We can steal */
			if (p_ptr->pskills[skill].max == LEV_REQ_BURGLE)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You can now steal things from monsters.  You also get accuracy bonuses with light melee weapons and penalties with heavy ones.  Your Charisma drops by 2.");
			}

			/* We can lock doors */
			if (p_ptr->pskills[skill].max == LEV_REQ_LOCK_DOOR)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You can now lock closed doors.");
			}

			/* We can set traps */
			if (p_ptr->pskills[skill].max == LEV_REQ_TRAP)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You can now set traps.");
			}
		}

		/* Perception */
		if (skill == S_PERCEPTION)
		{
			/* We can get precognition messages */
			if (p_ptr->pskills[skill].max == LEV_REQ_PRECOG)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You will sometimes feel the presence of monsters or objects on levels you enter.  Note that not all precognition messages are true.");
			}
		}

		/* Alchemy */
		if (skill == S_ALCHEMY)
		{
			/* We can make potions and scrolls */
			if (p_ptr->pskills[skill].max == LEV_REQ_ALCHEMY)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You can now make potions and scrolls, and will now save empty bottles and blank parchments.");

				/* Start saving bottles and parchments */
				p_ptr->suppress_bottle = FALSE;
			}

			/* We can make rings and amulets */
			if (p_ptr->pskills[skill].max == LEV_REQ_ALCHEMY_RING)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You can now make rings and amulets.");
			}
		}

		/* Infusion */
		if (skill == S_INFUSION)
		{
			/* We can infuse forged items */
			if (p_ptr->pskills[skill].max == LEV_REQ_INFUSE)
			{
				/* Print notice */
				skill_comment(TERM_L_BLUE, "You can now infuse forged items and search for essences.");
			}

			/* We can make grenades */
			if (p_ptr->pskills[skill].max == LEV_REQ_GRENADE)
			{
				/* Print notice */
				if (p_ptr->pskills[S_ALCHEMY].cur >= LEV_REQ_ALCHEMY)
				{
					skill_comment(TERM_L_BLUE, "You can now create potions of essences to throw at your foes.");
				}
				else
				{
					skill_comment(TERM_L_BLUE, "Once you raise your Alchemy skill to 10, you will be able to create potions of essences to throw at your foes.");
				}

				/* Hack - learn about potions of essences */
				k_info[lookup_kind(TV_POTION, SV_POTION_GRENADE)].special |=
					(SPECIAL_AWARE | SPECIAL_EVER_SEEN);
			}
		}
	}

	/* Spend experience */
	if (pay_exp) p_ptr->exp -= cost;

	/* Recalculate character power */
	calc_power();


	/* Redraw and update some stuff (later) */
	p_ptr->redraw |= (PR_EXP | PR_HP | PR_TITLE);
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SCORE);

	/* Success */
	return (TRUE);
}

void prt_oath_message()
{
	switch (p_ptr->oath)
	{
		case OATH_OF_SORCERY:
			skill_msg(TERM_RED, "The Oath of Sorcery is binding!");
			break;
		case YAVANNAS_FELLOWSHIP:
			skill_msg(TERM_RED, "You cannot abandon the mystic ways of the Yavanna's Fellowship!");
			break;
		case COVENANT_OF_FAITH:
			skill_msg(TERM_RED, "You cannot abandon the Covenant of Faith!");
			break;
		case BLACK_MYSTERY:
			skill_msg(TERM_RED, "The Black Mystery cannot be undone!");
			break;
		case OATH_OF_IRON:
			skill_msg(TERM_RED, "You cannot unlearn the secrets of the Oath of Iron!");
			break;
		case BURGLARS_GUILD:
			skill_msg(TERM_RED, "Once a thief, always a thief.  The guild brooks no traitors!");
			break;
		default:
			break;
	}
	(void)inkey(ALLOW_CLICK);
}

static bool can_reduce_skill(int skill, bool verbose, bool refund)
{
	int martial_skills = 0;
	int level = p_ptr->pskills[skill].max;

	if (level == 0) return (FALSE);

	/* Check and make sure we don't invalidate any oathes */
	switch (skill)
	{
		case S_SWORD:
		case S_HAFTED:
		case S_POLEARM:
		case S_CROSSBOW:
		case S_BOW:
		case S_SLING:
		case S_THROWING:
		case S_WRESTLING:
		case S_KARATE:
			if (p_ptr->oath & OATH_OF_IRON)
			{
				if (level == OATH_OF_IRON_REQ)
				{
					if (get_skill(S_SWORD, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_HAFTED, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_POLEARM, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_CROSSBOW, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_BOW, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_SLING, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_THROWING, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_WRESTLING, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;
					if (get_skill(S_KARATE, 0, 100) >= OATH_OF_IRON_REQ) martial_skills++;

					/* Make sure at least one other skill satisfies the oath */
					if (martial_skills >= 2) 	return (TRUE);
					else
					{
						if (verbose) prt_oath_message();
						return (FALSE);
					}
				}
			}

			break;

		case S_MAGIC:
			if (level == 1)
			{
				if (oath_caster)
				{
					if (verbose) prt_oath_message();
					return (FALSE);
				}
			}
			return (TRUE);
			break;
		case S_WIZARDRY:
			if (oath_caster && (level == MAGIC_OATH_REQ) && p_ptr->realm == MAGE)
			{
				if (verbose) prt_oath_message();
				return (FALSE);
			}
			return (TRUE);
			break;
		case S_DOMINION:
			if (oath_caster && (level == MAGIC_OATH_REQ) && p_ptr->realm == NECRO)
			{
				if (verbose) prt_oath_message();
				return (FALSE);
			}
			return (TRUE);
			break;
		case S_NATURE:
			if (oath_caster && (level == MAGIC_OATH_REQ) && p_ptr->realm == DRUID)
			{
				if (verbose) prt_oath_message();
				return (FALSE);
			}
			return (TRUE);
			break;
		case S_PIETY:
			if (oath_caster && (level == MAGIC_OATH_REQ) && p_ptr->realm == PRIEST)
			{
				if (verbose) prt_oath_message();
				return (FALSE);
			}
			return (TRUE);
			break;
		case S_BURGLARY:
			if ((p_ptr->oath & BURGLARS_GUILD) && (level == LEV_REQ_GUILD))
			{
				if (verbose) prt_oath_message();
				return (FALSE);
			}
		case S_FORGE_ARMOR:
		case S_FORGE_WEAPON:
		case S_INFUSION:
		case S_ALCHEMY:
		{
			/* Only allow while still on screen */
			return refund;
		}
		default:
			return (TRUE);
			break;
	}

	return (TRUE);
}


/*
 * Reduce a skill, refunding experience if necessary
 */
static bool reduce_skill(int skill, bool refund_exp)
{
	int exp, level;

	if (!can_reduce_skill(skill, TRUE, refund_exp)) return (FALSE);

	/* Reduce current skill if necessary */
	if (p_ptr->pskills[skill].cur == p_ptr->pskills[skill].max)
		p_ptr->pskills[skill].cur = p_ptr->pskills[skill].cur - 1;

	/* Reduce maximum skill */
	p_ptr->pskills[skill].max = p_ptr->pskills[skill].max - 1;

	/* Refund experience if necessary */
	if (refund_exp)
	{
		level = p_ptr->pskills[skill].max;
		exp = adv_cost(skill, TRUE);
		p_ptr->exp += exp;
	}

	/* Recalculate character power */
	calc_power();

	return (TRUE);
}


/*
 * Use '*' to take oaths or do other skill-specific things.
 */
static bool special_skill_command(int skill, bool *must_accept)
{
	int answer;


	/* Determine if this skill allows any special commands */
	switch (skill)
	{
		case S_SWORD:
		case S_HAFTED:
		case S_POLEARM:
		case S_WRESTLING:
		case S_KARATE:
		case S_SLING:
		case S_BOW:
		case S_CROSSBOW:
		case S_THROWING:
		{
			/* Normal skill requirement to take this Oath */
			int req_skill = OATH_OF_IRON_REQ;

			/* Can take the Oath of Iron */
			if ((p_ptr->pskills[skill].cur >= req_skill) &&
			    (can_take_oath(OATH_OF_IRON)))
			{
				/* Warning */
				skill_comment(TERM_L_UMBER, "The Oath of Iron is the greatest of all the commitments you can make, for it alone inducts you into the secrets of the most powerful warrior arts.  But be warned!  Those who take the Oath of Iron can learn no magic, and find magical devices hard to master.\n     Once taken, an Oath cannot be revoked!");

				/* Ask */
				skill_msg(TERM_WHITE, "Take the Oath of Iron? [y/n] ");

				/* Answer */
				answer = inkey(FALSE);

				/* The Oath of Iron */
				if ((answer == 'Y') || (answer == 'y'))
				{
					p_ptr->oath |= (OATH_OF_IRON);

					/* Clear info text */
					erase_skill_comment();

					/* Refuse to cancel */
					*must_accept = TRUE;

					/* Message about new talents */
					skill_comment(TERM_UMBER, "You may be able to use several new talents depending on your skill.");

					/* Wait for it */
					skill_msg(TERM_WHITE, "Press any key to continue.");
					(void)inkey(ALLOW_CLICK);
				}
				else
				{
					skill_msg(TERM_WHITE, "Cancelled.");
				}
			}

			/* Nothing happens */
			else return (FALSE);

			break;
		}

		case S_WIZARDRY:
		{
			/* Check if an oath allowed */
			if (can_take_oath(OATH_OF_SORCERY))
			{
				/* Warning */
				skill_comment(TERM_L_RED, format("No wizards' secrets are hidden from %s who takes the Oath of Sorcery, but specialization in magic comes at a significant cost in hitpoints and warrior skills.\n     Once taken, an Oath cannot be revoked!", sex_info[p_ptr->psex].pronoun));

				/* Ask */
				skill_msg(TERM_WHITE, "Take the Oath of Sorcery? [y/n] ");

				/* Answer */
				answer = inkey(FALSE);

				/* The Oath of Sorcery */
				if ((answer == 'Y') || (answer == 'y'))
				{
					p_ptr->oath |= (OATH_OF_SORCERY);

					/* Refuse to cancel */
					*must_accept = TRUE;
				}
				else
				{
					skill_msg(TERM_WHITE, "Cancelled.");
				}
			}

			/* Nothing happens */
			else return (FALSE);

			break;
		}

		case S_PIETY:
		{
			/* Check if an oath allowed */
			if (can_take_oath(COVENANT_OF_FAITH))
			{
				/* Warning */
				skill_comment(TERM_L_BLUE, format("Mighty and generous is the Almighty, and blessed indeed are those who serve Him.  Subscribing to the Covenant of Faith takes intense commitment however; hitpoints and most warrior skills suffer somewhat.\n     Once taken, an Oath cannot be revoked!"));

				/* Ask */
				skill_msg(TERM_WHITE, "Subscribe to the Covenant of Faith? [y/n] ");

				/* Answer */
				answer = inkey(FALSE);

				/* The Covenant of Faith */
				if ((answer == 'Y') || (answer == 'y'))
				{
					p_ptr->oath |= (COVENANT_OF_FAITH);

					/* Refuse to cancel */
					*must_accept = TRUE;
				}
				else
				{
					skill_msg(TERM_WHITE, "Cancelled.");
				}
			}

			/* Nothing happens */
			else return (FALSE);

			break;
		}

		case S_NATURE:
		{
			/* Check if an oath allowed */
			if (can_take_oath(YAVANNAS_FELLOWSHIP))
			{
				/* Warning */
				skill_comment(TERM_L_GREEN, "The lore of the natural world offers great power, but only the mystics of Yavanna's Fellowship can hope to comprehend it fully.  So intent are they on their chosen paths, however, that they have relatively few hitpoints and poor warrior skills.\n     Once taken, an Oath cannot be revoked!");

				/* Ask */
				skill_msg(TERM_WHITE, "Join Yavanna's Fellowship? [y/n] ");

				/* Answer */
				answer = inkey(FALSE);

				/* Yavanna's Fellowship */
				if ((answer == 'Y') || (answer == 'y'))
				{
					p_ptr->oath |= (YAVANNAS_FELLOWSHIP);

					/* Refuse to cancel */
					*must_accept = TRUE;
				}
				else
				{
					skill_msg(TERM_WHITE, "Cancelled.");
				}
			}

			/* Nothing happens */
			else return (FALSE);

			break;
		}

		case S_DOMINION:
		{
			/* Check if an oath allowed */
			if (can_take_oath(BLACK_MYSTERY))
			{
				/* Warning */
				skill_comment(TERM_PURPLE, "The rituals of the true necromancers are the very stuff of nightmares.  This most perilous of magics requires extraordinary dedication; mastery comes only at the price of lost hitpoints and reduced combat skills.\n     Once taken, an Oath cannot be revoked!");

				/* Ask */
				skill_msg(TERM_WHITE, "Bind yourself to the Black Mystery? [y/n] ");

				/* Answer */
				answer = inkey(FALSE);

				/* The Black Mystery */
				if ((answer == 'Y') || (answer == 'y'))
				{
					p_ptr->oath |= (BLACK_MYSTERY);

					/* Refuse to cancel */
					*must_accept = TRUE;
				}
				else
				{
					skill_msg(TERM_WHITE, "Cancelled.");
				}
			}

			/* Nothing happens */
			else return (FALSE);

			break;
		}

		case S_BURGLARY:
		{
			/* Check if an oath allowed */
			if (can_take_oath(BURGLARS_GUILD))
			{
				/* Warning */
				skill_comment(TERM_SLATE, format("The lore and cunning of the %s of Misrule is shared only with fellow Guild members.  Scofflaws, cutpurses, wastrels, vagabonds, keenfingers, and common thugs need not apply; such scum are mere prey to the Shadowknives.\n     But remember one thing:  \"Once a Guild%s, always a Guild%s!\"", (p_ptr->psex == 0 ? "Daughters" : "Sons"), (p_ptr->psex == 0 ? "sister" : "brother"), (p_ptr->psex == 0 ? "sister" : "brother")));

				/* Ask */
				if (p_ptr->psex == 0)
					skill_msg(TERM_WHITE, "Become a Sister in the Guild of Burglars? [y/n] ");
				else
					skill_msg(TERM_WHITE, "Become a Brother in the Guild of Burglars? [y/n] ");

				/* Answer */
				answer = inkey(FALSE);

				/* The Guild of Burglars */
				if ((answer == 'Y') || (answer == 'y'))
				{
					p_ptr->oath |= (BURGLARS_GUILD);

					/* Refuse to cancel */
					*must_accept = TRUE;

					/* Message about new talents */
					skill_comment(TERM_SLATE, "You may be able to use several new talents depending on your skill.");

					/* Wait for it */
					skill_msg(TERM_WHITE, "Press any key to continue.");
					(void)inkey(ALLOW_CLICK);
				}
				else
				{
					skill_msg(TERM_WHITE, "Cancelled.");
				}
			}

			/* Nothing happens */
			else return (FALSE);

			break;
		}

		default:
		{
			/* Nothing happens */
			return (FALSE);
		}
	}

	/* Erase message */
	erase_skill_comment();

	/* Something happened */
	return (TRUE);
}


/*
 * Print information about the currently selected skill.
 */
static void prt_skill_select(int skill)
{
	char out[DESC_LEN];
	char buf[DESC_LEN];

	cptr cmddesc = "";

	int lev;
	byte attr = TERM_WHITE;


	/* Get and display cost to advance this skill */
	if (can_raise_skill(skill, FALSE, 1) > 0)
	{
		/* Get the cost with and without the practice penalty */
		u16b cost   = adv_cost(skill, TRUE);
		u16b b_cost = adv_cost(skill, FALSE);

		(void)strnfmt(out, sizeof(out), "XP needed to advance %s: ",
			skill_info[skill].name);

		/* Display */
		move_cursor(0, 0);
		roff(out, 2, 80);

		/* Determine a color to display the exp cost in */
		if      (cost > MIN(b_cost * 5, b_cost + 2500)) attr = TERM_RED;
		else if (cost > MIN(b_cost * 3, b_cost + 1500)) attr = TERM_ORANGE;
		else if (cost > MIN(5*b_cost/3, b_cost +  300)) attr = TERM_YELLOW;
		else if (cost > b_cost)                         attr = TERM_SLATE;
		else                                            attr = TERM_WHITE;

		/* Display the exp cost */
		c_roff(attr, format("%d", cost), 2, 80);
	}

	else if (p_ptr->pskills[skill].cur == PY_MAX_POWER)
	{
		skill_msg(TERM_L_GREEN,
			format("You have mastered %s.", skill_info[skill].name));
	}
	else if (p_ptr->pskills[skill].cur == 0)
	{
		skill_msg(TERM_L_RED,
			format("%s is forbidden to you.", skill_info[skill].name));
	}
	else
	{
		skill_msg(TERM_WHITE, "You cannot raise this skill further.");
	}

	/* Print unspent exp */
	(void)strnfmt(out, sizeof(out), "Unspent XP: %ld", p_ptr->exp);
	prt(out, 0, 56);


	/* Special information for the magic level skill for non-spellcasters */
	if ((skill == S_MAGIC) && (p_ptr->realm == NONE))
	{
		/* Print a special message for those forbidden to learn magic */
		if (p_ptr->oath & (OATH_OF_IRON))
		{
			(void)strnfmt(out, sizeof(out), "The Oath of Iron forbids you to learn magic.");
		}

		/* Print a special message for able to start spellcasting */
		else
		{
			(void)strnfmt(out, sizeof(out), "Allows you to choose a realm of magic.");
		}
	}

	/* Generic skill description */
	else
	{
		(void)strnfmt(out, sizeof(out), "Improves your %s.", skill_info[skill].desc);
	}

	/* Display the skill description */
	move_cursor(1, 0);
	c_roff_centered(TERM_L_BLUE, out, 0, 80);

	/* Display a rate of learning indicator */
	if (TRUE)
	{
		cptr rate_desc = "";

		int adj_cost = race_adj_cost_skill[skill][p_ptr->prace];

		/* Describe rate of learning */
		if      (adj_cost >= 17) rate_desc = "extremely slowly";
		else if (adj_cost >= 14) rate_desc = "very slowly";
		else if (adj_cost >= 12) rate_desc = "slowly";
		else if (adj_cost == 11) rate_desc = "a bit slowly";
		else if (adj_cost == 10) rate_desc = "at normal speed";
		else if (adj_cost ==  9) rate_desc = "relatively quickly";
		else if (adj_cost ==  8) rate_desc = "quickly";
		else if (adj_cost <=  7) rate_desc = "very quickly";

		/* Note unusual rate of learning */
		if (adj_cost != 10)
		{
			strcpy(buf, format("(A %s learns this skill %s.)",
				race_info[p_ptr->prace].title, rate_desc));

			/* Display a rate of learning indicator */
			move_cursor(2, 0);
			c_roff_centered(TERM_L_WHITE, buf, 0, 80);
		}
	}


	/* Get this skill's level */
	lev = p_ptr->pskills[skill].cur;

	/* Determine if this skill allows any special commands */
	switch (skill)
	{
		case S_SWORD:
		case S_HAFTED:
		case S_POLEARM:
		case S_WRESTLING:
		case S_KARATE:
		case S_SLING:
		case S_BOW:
		case S_CROSSBOW:
		case S_THROWING:
		{
			int req_skill = OATH_OF_IRON_REQ;

			/* Can take the Oath of Iron */
			if ((lev >= req_skill) && (can_take_oath(OATH_OF_IRON)))
			{
				cmddesc = "Oath of Iron";
				attr = TERM_L_UMBER;
			}
			break;
		}

		case S_WIZARDRY:
		{
			/* Check if an oath allowed */
			if (can_take_oath(OATH_OF_SORCERY))
			{
				cmddesc = "Oath of Sorcery";
				attr = TERM_L_RED;
			}
			break;
		}

		case S_PIETY:
		{
			/* Check if an oath allowed */
			if (can_take_oath(COVENANT_OF_FAITH))
			{
				cmddesc = "Covenant of Faith";
				attr = TERM_BLUE;
			}
			break;
		}

		case S_NATURE:
		{
			/* Check if an oath allowed */
			if (can_take_oath(YAVANNAS_FELLOWSHIP))
			{
				cmddesc = "Yavanna's Fellowship";
				attr = TERM_GREEN;
			}
			break;
		}

		case S_DOMINION:
		{
			/* Check if an oath allowed */
			if (can_take_oath(BLACK_MYSTERY))
			{
				cmddesc = "Black Mystery";
				attr = TERM_PURPLE;
			}
			break;
		}

		case S_BURGLARY:
		{
			/* Check if an oath allowed */
			if (can_take_oath(BURGLARS_GUILD))
			{
				cmddesc = "Burglar's Guild";
				attr = TERM_SLATE;
			}
			break;
		}
	}

	/* Display the special command key */
	if (strlen(cmddesc))
	{
		(void)Term_gotoxy(28, 22);
		c_roff(attr, format("*) %s", cmddesc), 0, 0);
	}

	/* Special key to raise skill back up to maximum */
	else if (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].max)
	{
		(void)Term_gotoxy(28, 22);
		c_roff(TERM_WHITE, "!) Recover skill", 0, 0);
	}
}


/*
 * Print the given skill and its rank in the appropriate column
 * and row.
 */
static void prt_skill_rank(int skill)
{
	char buf1[38], buf2[18];
	int row, col;
	char c;

	bool drained = (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].max);

	byte a = TERM_WHITE;

	/* Highlight selected skill */
	if (skill == selected) a = TERM_YELLOW;


	/* Skip unused skills */
	if (!(skill_info[skill].name)) return;

	/* Skill cannot be raised further */
	if (can_raise_skill(skill, FALSE, 1) <= 0)
	{
		/* Skill is not allowed at all */
		if (p_ptr->pskills[skill].cur == 0)
		{
			a = TERM_L_DARK;
			if (skill == selected) a = TERM_SLATE;
		}

		/* Skill is maxed out */
		else
		{
			a = TERM_L_GREEN;
			if (skill == selected) a = TERM_GREEN;
		}
	}

	/* Skill has been drained */
	else if (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].max)
	{
		/* Darker colors for drained skills */
		if (skill == selected) a = TERM_ORANGE;
		else                   a = TERM_SLATE;
	}

	/* Player has lowered skill and will lose experience */
	else if (p_ptr->pskills[skill].max < old_pskills[skill].max)
	{
		if (skill == selected) a = TERM_L_PURPLE;
		else                   a = TERM_PURPLE;
	}

	/* Skill is zero */
	else if (p_ptr->pskills[skill].max == 0 && !(skill == selected))
	{
		a = TERM_L_WHITE;
	}

	/* Work out the row and column of the screen to use */
	row = 3 + skill;
	if (skill < ((NUM_SK_USED + 1) / 2)) col = 0;
	else
	{
		col = 40;
		row -= (NUM_SK_USED + 1) / 2;
	}

	/* Display the skill percentage */
	(void)strnfmt(buf2, sizeof(buf2), "%3d%%", (int)p_ptr->pskills[skill].cur);

	/* The character corresponding to the skill */
	c = index_chars_lower[skill];

	/* Format the string */
	(void)strnfmt(buf1, sizeof(buf1), "%c) %-18s%c %s", c, skill_info[skill].name,
		(drained ? '!' : ':'), buf2);

	/* Print the skill */
	c_prt(a, buf1, row, col);

	/* Move the cursor */
	(void)Term_gotoxy(col + 28, row);

	/* Get and display cost to advance this skill (unless forbidden) */
	if (can_raise_skill(skill, FALSE, 1) > 0)
	{
		byte attr;

		/* Get the cost with and without the practice penalty */
		u16b cost   = adv_cost(skill, TRUE);
		u16b b_cost = adv_cost(skill, FALSE);

		/* Determine a color to display the exp cost in */
		if      (cost > MIN(b_cost * 5, b_cost + 2500)) attr = TERM_RED;
		else if (cost > MIN(b_cost * 3, b_cost + 1500)) attr = TERM_ORANGE;
		else if (cost > MIN(b_cost*5/3, b_cost +  300)) attr = TERM_YELLOW;
		else if (cost > b_cost)                         attr = TERM_SLATE;
		else                                            attr = TERM_WHITE;

		/* Display the exp cost */
		c_roff(attr, format(" %5d ", cost), 0, 0);

		/* Skill has some investment in it */
		if (p_ptr->pskills[skill].max)
		{
			if (cost <= p_ptr->exp)
			{
				/* Color the ':' green if skill is not too costly to raise */
				a = (skill == selected) ? TERM_GREEN : TERM_L_GREEN;
			}
			else
			{
				/* Color the ':' red if skill is too costly to raise */
				a = (skill == selected) ? TERM_RED : TERM_L_RED;
			}

			/* Print a ":" or a "!" */
			(void)Term_putch(col + 21, row, a, (drained ? '!' : ':'));
		}

	}
	else
	{
		/* Display the exp cost */
		c_roff(TERM_L_DARK, "   -   ", 0, 0);
	}
}

/*
 * Print out all the skills, along with their ranks.
 */
static void print_all_skills(bool must_accept)
{
	int i;

	char buf[DESC_LEN];

	/* Get realm color */
	byte attr = realm_color();

	/* Clear all of the screen except the special message area */
	for (i = 0; i < Term->rows; i++)
	{
		if ((i < 17) || (i > 20)) clear_row(i);
	}

	/* Print the static information */
	prt(format("%c-%c) Select skills                          +/=) Advance skills",
		index_chars_lower[0], index_chars_lower[NUM_SK_USED - 1]),
		22, 8);

	/* Available commands */
	if (!must_accept)
	{
		prt("dir) Move    ESC) Cancel    RETURN) Accept    ?) Get help      ",
			23, 8);
	}
	else
	{
		prt("dir) Move                   RETURN) Accept    ?) Get help      ",
			23, 8);
	}

	/* Update hitpoints and mana */
	calc_hitpoints();
	calc_mana();

	/* Display hitpoints and mana -JM- */
	c_put_str(TERM_L_BLUE, format("Power: %d", p_ptr->power), 21, 0);
	c_put_str(TERM_L_BLUE, format("HP/MP: %d/%d", p_ptr->mhp, p_ptr->msp), 21, 66);

	/* Center realm description */
	center_string(buf, sizeof(buf), realm_desc(), display_width() - 28);

	/* Print out realm description */
	c_put_str(attr, buf, 21, 14);

	/* Print all the skills */
	for (i = 0; i < NUM_SK_USED; i++) prt_skill_rank(i);
}



/*
 * Make certain that a player who has invested in Piety wants to become a
 * Necromancer, or that one who has invested in Blood Dominion wants to
 * become a Priest.
 */
static bool realm_check(int realm)
{
	cptr warn_text = "", lost_skill = "", realm_text = "";
	int warn_attr = TERM_WHITE;

	/* Some realms need no special checks */
	if ((realm != PRIEST) && (realm != NECRO)) return (TRUE);

	/* We only need to check sometimes */
	if ((realm == PRIEST) && (p_ptr->pskills[S_DOMINION].max == 0))
		return (TRUE);

	if ((realm == NECRO) && (p_ptr->pskills[S_PIETY].max == 0))
		return (TRUE);


	/* Get warning text */
	if (realm == PRIEST)
	{
		realm_text = "Piety";
		warn_text = "Acolytes of divine lore must purge themselves of all within them that is unholy; all your skill in Blood Dominion will be lost if you cultivate Piety.";
		warn_attr = TERM_L_BLUE;
		lost_skill = "Blood Dominion";
	}
	if (realm == NECRO)
	{
		realm_text = "Necromancy";
		warn_text = "Students of the Dark Arts cannot retain any ties to the Divine; all your skill in Holy Alliance will be lost if you cultivate necromancy.";
		warn_attr = TERM_L_PURPLE;
		lost_skill = "Holy Alliance";
	}

	/* Warning */
	skill_comment(TERM_L_BLUE, warn_text);

	/* Ask */
	prt("", 0, 0);
	skill_msg(TERM_WHITE, format("Choose %s, and forfeit all %s? [y/n] ", realm_text, lost_skill));

	/* Ask until we get a clear answer */
	while (TRUE)
	{
		/* Answer */
		char ch = inkey(FALSE);

		/* Note accept */
		if ((ch == 'y') || (ch == 'Y'))
		{
			skill_comment(TERM_WHITE, "");
			return (TRUE);
		}

		/* Note cancel */
		if ((ch == 'n') || (ch == 'N') || (ch == ESCAPE))
		{
			skill_msg(TERM_WHITE, "Cancelled.");
			skill_comment(TERM_WHITE, "");
			return (FALSE);
		}
	}

	return (FALSE);
}


/*
 * Choose a realm of magic.
 */
static bool choose_realm(void)
{
	char ch;

	/* May pick a realm */
	skill_msg(TERM_WHITE, "You may pick a realm of magic.");
	(void)inkey(ALLOW_CLICK);

	/* Apply limitations */
	if ((cannot_learn_magic) && (cannot_learn_prayers))
	{
		skill_msg(TERM_WHITE, "You can learn no magic.");
	}

	/* Apply limitations */
	else if (cannot_learn_magic)
	{
		skill_msg(TERM_WHITE, "Press 'P' to study Piety.");
	}

	/* Apply limitations */
	else if (cannot_learn_prayers)
	{
		prt("Will you study S)orcery, D)ruidic magic, or N)ecromancy?", 0, 2);
	}

	/* No limitations */
	else
	{
		prt("Will you study S)orcery, P)iety, D)ruidic magic, or N)ecromancy? ", 0, 2);
	}

	/* Get command */
	ch = inkey(FALSE);


	switch (ch)
	{
		case 's':
		case 'S':
		{
			if (!cannot_learn_magic)
			{
				p_ptr->realm = MAGE;
			}
			break;
		}
		case 'p':
		case 'P':
		{
			if (!cannot_learn_prayers)
			{
				if (realm_check(PRIEST))
					p_ptr->realm = PRIEST;
			}
			break;
		}
		case 'd':
		case 'D':
		{
			if (!cannot_learn_magic)
			{
				p_ptr->realm = DRUID;
			}
			break;
		}
		case 'n':
		case 'N':
		{
			if (!cannot_learn_magic)
			{
				if (realm_check(NECRO))
					p_ptr->realm = NECRO;
			}
			break;
		}
		default:
		{
			break;
		}
	}

	/* Allow cancel */
	if (!p_ptr->realm)
	{
		/* Hack -- undo skill purchase */
		p_ptr->pskills[selected].cur = 0;
		p_ptr->pskills[selected].max = 0;
		p_ptr->exp += adv_cost(selected, TRUE);
		return (FALSE);
	}

	/* New magic realm */
	mp_ptr = &magic_info[p_ptr->realm];

	/* We are now a user of holy lore */
	if (p_ptr->realm == PRIEST)
	{
		/* Set Blood Dominion to zero */
		p_ptr->pskills[S_DOMINION].max = 0;
		p_ptr->pskills[S_DOMINION].cur = 0;
	}

	/* We are now a user of necromantic arts */
	if (p_ptr->realm == NECRO)
	{
		/* Set Piety to zero */
		p_ptr->pskills[S_PIETY].max = 0;
		p_ptr->pskills[S_PIETY].cur = 0;
	}

	/* Update preferences for chosen realm */
	(void)process_pref_file("realm.prf");

	return (TRUE);
}



/*
 * Allow the player to examine and improve his skill levels.
 */
void do_cmd_skills(void)
{
	char ch;
	int num;

	int i;

	bool accepted = FALSE;
	bool must_accept = FALSE;

	/* Old player's skills and unused experience */
	int old_exp;
	byte old_oath, old_realm, old_power, old_ten_power, ten_power;

	char buf[30];


	/*
	 * Winners cannot continue to raise their skills (no trying everything
	 * in a single game).  XXX XXX
	 */
	if (p_ptr->total_winner)
	{
		/* Message */
		message(MSG_L_BLUE, 0, "You have won the game!  You need learn nothing more!");
		return;
	}


	/* Save the screen, use the standard display, and center the view */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX, 80, 0);


	/* Select the skill last advanced */
	selected = p_ptr->lastadv;

	/* Hide the cursor on the main screen (will be cancelled after we leave) */
	inkey_cursor_hack[TERM_MAIN] = -1;

	/* Save old status */
	for (i = 0; i < NUM_SKILLS; i++)
	{
		old_pskills[i].cur = p_ptr->pskills[i].cur;
		old_pskills[i].max = p_ptr->pskills[i].max;
	}

	old_exp = p_ptr->exp;
	old_oath = p_ptr->oath;
	old_realm = p_ptr->realm;
	old_power = calc_max_power();


	/* Continue until satisfied */
	while (TRUE)
	{
		/* Print out the skills */
		print_all_skills(must_accept);

		/* Print information on the current skill */
		prt_skill_select(selected);

		/* Get a command */
		ch = inkey(ALLOW_CLICK);


		/* Cancel */
		if (ch == ESCAPE)
		{
			if (must_accept)
			{
				skill_msg(realm_color(), "Once taken, an Oath is binding!");
				(void)inkey(ALLOW_CLICK);
				continue;
			}
			else
			{
				accepted = FALSE;
				break;
			}
		}

		/* Confirm */
		if ((ch == '\n') || (ch == '\r'))
		{
			accepted = TRUE;
			break;
		}

		/* Help me. */
		if (ch == '?')
		{
			p_ptr->get_help_index = HELP_SKILLS + selected;
			do_cmd_help();
			(void)Term_clear();
			continue;
		}

		/* Go up one skill */
		if (ch == '8')
		{
			if (selected > 0) selected--;
			continue;
		}

		/* Go down one skill */
		if (ch == '2' || ch == '\t' || ch == ' ')
		{
			if (selected < NUM_SK_USED-1) selected++;
			continue;
		}

		/* Go left one column */
		if (ch == '4')
		{
			if (selected >= (NUM_SK_USED+1) / 2)
			    selected -= (NUM_SK_USED+1) / 2;

			if (selected < 0) selected = 0;
			continue;
		}

		/* Go right one column */
		if (ch == '6')
		{
			if (selected <  (NUM_SK_USED+1) / 2)
			    selected += (NUM_SK_USED+1) / 2;

			if (selected >= NUM_SK_USED) selected = NUM_SK_USED - 1;
			continue;
		}

		/* Allow skill-specific actions */
		if (ch == '*')
		{
			/* If something happened, continue */
			if (special_skill_command(selected, &must_accept)) continue;
		}

		/* Advance the skill */
		if (ch == '+' || ch == '=')
		{
			/* Allow user to change their mind about skill reduction */
			bool pay = (p_ptr->pskills[selected].max < old_pskills[selected].max)? FALSE : TRUE;

			int advance = adv_skill(selected, pay);

			/* Cannot raise the skill */
			if (advance < 0)
			{
				skill_msg(TERM_WHITE, "You cannot raise this skill further.");
				(void)Term_fresh();
				pause_for(250);
			}

			/* Choose not to raise the skill */
			else if (advance == 0)
			{
				skill_msg(TERM_WHITE, "Cancelled...");
				(void)Term_fresh();
				pause_for(250);
			}

			/* Can raise the skill */
			else
			{
				/* Choose a magic realm */
				if ((selected == S_MAGIC) && (old_pskills[S_MAGIC].max == 0 && p_ptr->pskills[S_MAGIC].max == 1))
				{
					/* Allow cancel */
					if (!choose_realm()) continue;
				}

				/* Piety and Blood Dominion never mix */
				else if (selected == S_PIETY)
				{
					p_ptr->pskills[S_DOMINION].cur = 0;
					p_ptr->pskills[S_DOMINION].max = 0;
				}

				else if (selected == S_DOMINION)
				{
					p_ptr->pskills[S_PIETY].cur = 0;
					p_ptr->pskills[S_PIETY].max = 0;
				}
			}

			prt_skill_rank(selected);
			continue;
		}

		/* Fix the skill */
		if (ch == '!')
		{
			while (p_ptr->pskills[selected].cur < p_ptr->pskills[selected].max)
			{
				/* Cannot raise the skill */
				if (adv_skill(selected, TRUE) <= 0)
				{
					skill_msg(TERM_WHITE, "You cannot raise this skill further.");
					(void)inkey(ALLOW_CLICK);
					break;
				}
			}

			prt_skill_rank(selected);
			continue;
		}

		/* Allow player to voluntarily reduce skill */
		if (ch == '-' || ch == '_')
		{
			/* Refund skills when reducing skills raised on the char screen */
			/* Note -- this can result in gaining or losing experience artificially, as the order that skills are raised matters */
			/*         however, this is unlikely except at low levels with high amounts of experience to spend. */
			bool refund = (p_ptr->pskills[selected].max > old_pskills[selected].max)? TRUE : FALSE;

			/* Reduce the skill, and update information */
			if (reduce_skill(selected, refund))
			{
				if (p_ptr->exp > old_exp) p_ptr->exp = old_exp;  /* Make it harder to for players to cycle skills up/down for experience */
				prt_skill_rank(selected);
			}
			continue;
		}

		/* Some keys are only used to clear messages */
		if (ch == ' ')
		{
			continue;
		}


		/* Attempt to use the command as an index */
		num = get_index(ch, TRUE);

		/* Jump to a skill */
		if ((num >= 0) && (num < NUM_SK_USED))
		{
			selected = num;
		}

		/* Illegal skill index */
		else
		{
			bell("Illegal skill option.");
		}
	}

	/* Remember selected skill */
	p_ptr->lastadv = selected;


	/* Check attempt to cancel */
	if (!accepted)
	{
		/* You cannot revoke an Oath */
		if (must_accept)
		{
			skill_msg(realm_color(), "Once taken, an Oath is binding!");
			(void)inkey(ALLOW_CLICK);
		}

		/* Allow the cancellation */
		else
		{
			bool changed = FALSE;

			/* Restore previous skill values */
			for (i = 0; i < NUM_SKILLS; i++)
			{
				if (p_ptr->pskills[i].cur != old_pskills[i].cur)
				{
					p_ptr->pskills[i].cur = old_pskills[i].cur;
					changed = TRUE;
				}
				if (p_ptr->pskills[i].max != old_pskills[i].max)
				{
					p_ptr->pskills[i].max = old_pskills[i].max;
					changed = TRUE;
				}
			}

			/* Restore previous unspent experience */
			if (p_ptr->exp != old_exp)
			{
				p_ptr->exp = old_exp;
				changed = TRUE;
			}

			/* Restore old spell realm */
			if (p_ptr->realm != old_realm)
			{
				p_ptr->realm = old_realm;
				mp_ptr = &magic_info[NONE];
				changed = TRUE;
			}

			/* Recalculate character power */
			calc_power();

			/* Flash a message */
			if (changed)
			{
				skill_msg(TERM_WHITE, "Cancelled...");
				pause_for(250);
			}
		}
	}

	/* Flush any pending messages */
	message_flush();

	/* Note any changes in power */
	old_ten_power = (old_power ) / 10;
	ten_power = (calc_max_power()) / 10;
	if (old_ten_power < ten_power)
    {
        strnfmt(buf, sizeof(buf), "Reached power %d", ten_power * 10);
        history_add(buf, HISTORY_GAIN_POWER, 0);
    }


	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);


	/* If one bare-handed skill is obviously the primary, choose it */
	if ((p_ptr->barehand == S_KARATE) &&
	       (p_ptr->pskills[S_WRESTLING].cur >
	    3 * p_ptr->pskills[S_KARATE].cur))
	{
		do_cmd_barehanded();
	}
	if ((p_ptr->barehand == S_WRESTLING) &&
	       (p_ptr->pskills[S_KARATE].cur >
	    3 * p_ptr->pskills[S_WRESTLING].cur))
	{
		do_cmd_barehanded();
	}

	/* Reset magic choice if needed */
	if (get_skill(S_MAGIC, 0, 100) == 0)
	{
		p_ptr->realm = 0;
		mp_ptr = &magic_info[NONE];
	}

	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Update various things */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_CMDLIST);

	/* Handle stuff */
	handle_stuff();
}
