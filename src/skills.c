

/* File: skills.c */

/*
 * Copyright (c) 1998 Julian Lighton, Michael Gorse, Chris Petit
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Look up the raw value of a skill, returning a number from 0 to 255
 * This way, the internal representation can change without breaking
 * everything.
 */
s16b
get_raw_skill(int which)
{
	return (p_ptr->pskills[which].cur) >> 8;
}

/*
 * This returns a "logical" value for ALL NON FIGHTING SKILLS.
 * Fighting skills handled in 3 other routines---one for +todam, one
 * for +tohit, and one for # of blows/round modifier
 *
 * For all magical skills, this returns an appropiate "level" of the
 * caster, for determining spell failure, mana, etc.
 *
 * For weapon/armorsmithing, this returns an effective level of the
 * user--- if 0, cannot use the ability.
 *
 * For backstabbing, this returns the damage multiplier.
 */

s16b
get_skill(int which)
{
	int tmp;

	if (which >= NUM_SKILLS)
		return 0;
	tmp = get_raw_skill(which);
	switch (which)
	{
	case S_SWORD:				/* Not yet accessed through get_skill */
	case S_HAFTED:
	case S_POLEARM:
	case S_ARCHERY:
	case S_KARATE:
	case S_WRESTLING:
		return tmp;
	case S_ENDURANCE:
		/* Effective level for determining hit points. range of 2-100 */
		if (tmp < 15)
			return 2;
		if (tmp < 22)
			return 3;
		if (tmp < 28)
			return 4;
		if (tmp < 34)
			return 5;
		if (tmp < 39)
			return 6;
		if (tmp < 43)
			return 7;
		if (tmp < 47)
			return 8;
		if (tmp < 50)
			return 9;
		if (tmp < 52)
			return 10;
		return 4 * (tmp - 34) / 9 + 2;
	case S_DEVICE:
		if (tmp < 15)
			return 1;
		if (tmp < 23)
			return 2;
		if (tmp < 30)
			return 3;
		if (tmp < 35)
			return 4;
		if (tmp < 38)
			return 5;
		return ((tmp - 30) / 2) + 1;
	case S_MAGIC:
		if (tmp < 18)
			return 0;
		if (tmp < 23)
			return 1;
		if (tmp < 29)
			return 2;
		if (tmp < 36)
			return 3;
		if (tmp < 42)
			return 4;
		if (tmp < 51)
			return 5;
		if (tmp < 59)
			return 6;
		if (tmp < 66)
			return 7;
		if (tmp < 72)
			return 8;
		if (tmp >= 240)
			return 50;
		return ((tmp - 40) / 4) + 1;
	case S_SAVE:
		if (tmp < 66)
			return (tmp / 2);
		return ((tmp + 33) / 3);
	case S_DISARM:
		return (tmp / 3);
	case S_BACKSTAB:
		return (tmp / 80) + 1;
	case S_STEALTH:
		return (tmp / 35);
	case S_MPOWER:
		if (tmp < 18)
			return 0;
		if (tmp < 20)
			return 1;
		if (tmp < 23)
			return 2;
		if (tmp < 27)
			return 3;
		if (tmp < 32)
			return 4;
		if (tmp < 38)
			return 5;
		if (tmp < 43)
			return 6;
		if (tmp < 47)
			return 7;
		if (tmp < 50)
			return 8;
		if (tmp < 53)
			return 9;
		return ((tmp - 35) / 2) + 1;
	case S_2HANDED:
		return tmp / 75;
	case S_DODGING:
		if (tmp < 30)
			return 0;

		/* Determine base AC when wearing nothing */
		tmp = (tmp - 25) / 2;

		/* Adjust for armor weight. The better you are,
		 * the more armor you can wear without penalty. */
		if (armor_weight() > tmp)
			tmp -= (4 * (armor_weight() - tmp) / 10);
		if (tmp < 0)
			return 0;
		return tmp;
	case S_WEAPON:
	case S_ARMOR:
		if (tmp < 45)
			return 0;
		return ((tmp - 45) / 4) + 1;
	case S_PERCEPTION:
		return (tmp / 7);
	case S_SLAY_EVIL:
		return tmp / 2;
	case S_SLAY_ANIMAL:
		return tmp / 2;
	case S_SLAY_UNDEAD:
		return tmp / 2;
	case S_PRECOG:
		return ((tmp - 5) * 2) / 5;
	case S_BOWMAKE:
		if (tmp < 40)
			return 0;
		return ((tmp - 40) / 5) + 1;
	case S_ALCHEMY:
		if (tmp < 100)
			return 0;
		return ((tmp - 90) / 2) - 2;
	case S_INFUSION:
		if (tmp < 120)
			return 0;
		return ((tmp - 110) / 2) + 1;
	case S_NOSKILL:
		msg_print("Warning. get_skill for S_NOSKILL");
		return 0;
	default:
		return tmp;
	}
}


/* This returns which weapon skill we're using */
int
sweapon()
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];
	if (!o_ptr->k_idx)
		return (p_ptr->barehand);
	if (o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_DIGGING)
		return (S_HAFTED);
	if (o_ptr->tval == TV_POLEARM)
		return (S_POLEARM);
	return (S_SWORD);
}

/* Description for realm */
static cptr
realm_desc()
{
	switch (p_ptr->realm)
	{
	case NONE:
		return ("You are not familiar with any magical art.");
	case MAGE:
		return ("You know the sorcerous arts.");
	case PRIEST:
		return ("You serve a greater power.");
	case DRUID:
		return ("You control the forces of nature.");
	case NECRO:
		return ("You understand the forces of life and death.");
	default:
		return ("You are familiar with the forces of software bugs.");
	}
}

/*
 * Calculate the experience point cost to raise the specific skill.
 */
s32b
adv_cost(int skill)
{
	s32b gen_adv = 0, skill_adv, cost;
	int i;

	/* count the number of times we've advanced our other skills */
	for (i = 0; i < NUM_SKILLS; i++)
	{
		if (i == skill)
			continue;
		gen_adv += p_ptr->pskills[i].adv;
	}
	skill_adv = p_ptr->pskills[skill].adv;
	skill_adv = (skill_adv * 5) / 6 + 1;
	cost = (SKILL_BASE_COST * skill_adv) / 2;
	cost += (gen_adv / 75) * skill_adv;

	/* Drained skills cost only 1/3 to restore */
	if (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].max)
		cost = (cost + 1) / 3;
	return cost;
}

/*
 * Increase or decrease the given skill. Will never raise a skill
 * above 255 (plus 255 fractional) or below the initial value.
 *
 * If the number of times we want to raise or lower the skill
 * is greater than one, the function handles this by recursion.
 *
 * See the skill_raises table in tables.c for an explanation.
 *
 * Due to rounding errors, lowering a skill by 1 level will not leave
 * it exactly where it was, but it will be close enough for government
 * work.
 */
bool
alter_skill(int skill, int change)
{
	int index;

	/* to avoid overflow when dealing with 16 bit unsigned numbers, we
	 * use 32 bit signed values. */
	s32b sk_change, new_skill, old_skill;

	if (change == 0)
		return FALSE;

	index = get_raw_skill(skill) / 5;

	if (change < 0 && index > 0)
		index--;

	/* Get the base change */
	sk_change = skill_raises[index];

	/* Adjust for skill difficulty */
	sk_change = (sk_change * 100) / skill_tbl[skill].diff;

	/* Adjust for racial tendencies */
	sk_change = (sk_change * rp_ptr->skills[skill]) / 10;

	old_skill = p_ptr->pskills[skill].cur;

	if (change < 0)
		new_skill = old_skill - sk_change;
	else
		new_skill = old_skill + sk_change;

	set_skill(skill, new_skill / 256, new_skill % 256);

	/* The player's skill can never go below its starting point. */
	if (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].min)
		p_ptr->pskills[skill].cur = p_ptr->pskills[skill].min;

	if (new_skill == old_skill)
		return FALSE;

	/* Set the new maximum skill level, and, if we bought a new level
	 * of a skill, record that we did so. */
	if (p_ptr->pskills[skill].cur > p_ptr->pskills[skill].max)
	{
		p_ptr->pskills[skill].max = p_ptr->pskills[skill].cur;
		p_ptr->pskills[skill].adv++;
	}

	/* Recurse */
	if (change < 0)
		alter_skill(skill, change + 1);
	else
		alter_skill(skill, change - 1);

	/* Redraw and recalc whatever may have changed */
	p_ptr->redraw |= (PR_EXP | PR_HP | PR_LEV | PR_TITLE);
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	return TRUE;
}

/*
 * Set the current value of a skill to the given value and
 * fractional value, handling fractions > 255 correctly.
 */
void
set_skill(int skill, int val, int frac)
{
	/* Sanity checks */
	if (frac > 255)
	{
		val += frac / 256;
		frac %= 256;
	}
	if (val > 255)
		val = 255;
	if (val < 0)
		val = 0;
	if (frac < 0)
		frac = 0;

	p_ptr->pskills[skill].cur = ((u16b) val << 8) + (u16b) frac;
}

/*
 * Buy up the value of a skill, if possible.
 */
static bool
adv_skill(int skill)
{
	s32b cost;

	if (get_raw_skill(skill) == 255)
		return FALSE;

	cost = adv_cost(skill);

	if (cost > p_ptr->exp)
		return FALSE;

	alter_skill(skill, 1);
	p_ptr->exp -= cost;
	p_ptr->max_exp -= cost;
	return TRUE;
}

/*
 * Print the information about the currently selected skill.
 */
static void
prt_skill_select(int skill)
{
	char out[80];
	int i;

	if (get_raw_skill(skill) < 255)
		sprintf(out, "XP needed to advance %s: %ld",
				skill_tbl[skill].name, adv_cost(skill));
	else
		sprintf(out, "You have mastered %s.", skill_tbl[skill].name);

	prt(out, 0, 2);

	sprintf(out, "Current XP: %ld", p_ptr->exp);
	prt(out, 0, 55);

	if (skill != S_MAGIC || p_ptr->realm)
		/* Print the skill description */
		sprintf(out, "Improves your %s.", skill_tbl[skill].desc);
	else
		/* Print a special message for those about to start spellcasting */
		sprintf(out, "Allows you to choose a realm of magic.");

	/* get the string length for centering purposes */
	i = strlen(out);

	/* Clear the line */
	prt("", 1, 0);

	prt(out, 1, (80 - i) / 2);
}

/*
 * Store the appropriate rank description in the provided buffer.
 * Most skills can use the default Awful-Ungodly scale, but some
 * require their own version.
 */
static void
skill_rank_desc(char *buf, int skill)
{
	cptr rank;

	if (raw_skills)
	{
		sprintf(buf, "%u.%u", p_ptr->pskills[skill].cur / 256,
				p_ptr->pskills[skill].cur % 256);
	}
	else
	{
		switch (skill)
		{
		case S_MAGIC:
			{
				if (!p_ptr->realm)
					sprintf(buf, "No magic");
				else
					sprintf(buf, "Level %d", get_skill(S_MAGIC));
				break;
			}
		case S_2HANDED:
			{
				sprintf(buf, "%d attacks", get_skill(S_2HANDED));
				break;
			}
		default:
			{
				switch (get_raw_skill(skill) / 10)
				{
				case 0:
				case 1:
					rank = "Awful";
					break;
				case 2:
				case 3:
				case 4:
					rank = "Poor";
					break;
				case 5:
				case 6:
				case 7:
				case 8:
					rank = "Fair";
					break;
				case 9:
				case 10:
				case 11:
				case 12:
					rank = "Good";
					break;
				case 13:
				case 14:
				case 15:
					rank = "Very Good";
					break;
				case 16:
				case 17:
				case 18:
					rank = "Excellent";
					break;
				case 19:
				case 20:
				case 21:
					rank = "Superb";
					break;
				case 22:
				case 23:
					rank = "Legendary";
					break;
				default:
					rank = "Ungodly";
					break;
				}
				sprintf(buf, rank);
				break;
			}
		}
	}
}

/*
 * Print the given skill and its rank in the appropriate column
 * and row.
 */
static void
prt_skill_rank(int skill)
{
	char buf1[38], buf2[18];
	int row, col;
	char c;
	bool drain = FALSE;

	/* Skip unused skills */
	if (!(skill_tbl[skill].name))
		return;

	/* Determine if the skill has been drained. */
	if (p_ptr->pskills[skill].cur < p_ptr->pskills[skill].max)
		drain = TRUE;

	/* Work out the row and column of the screen to use */
	row = 3 + skill;
	if (skill < (NUM_SK_USED / 2))
		col = 2;
	else
	{
		col = 40;
		row -= NUM_SK_USED / 2;
	}

	/* Get the string describing the rank */
	skill_rank_desc(buf2, skill);

	/* The character corresponding to the skill. If the skill is drained,
	 * we print it in lowercase. */
	c = I2A(skill);
	if (!drain)
		c = toupper(c);

	/* Format the string, then print it. Note that the I2A bit won't
	 * neccesarily work right if we go over 26 skills. */
	sprintf(buf1, "%c) %s: %s", c, skill_tbl[skill].name, buf2);

	/* Print the skill, in white unless it's been drained */
	if (!drain)
		prt(buf1, row, col);
	else
		c_prt(TERM_YELLOW, buf1, row, col);

	/* Since prt() clears the rest of the line, we may need to print
	 * the skill in the second column again. */
	if (skill < NUM_SK_USED / 2)
		prt_skill_rank(skill + (NUM_SK_USED / 2));
}

/*
 * Print out all the skills, along with their ranks.
 */
void
print_all_skills(void)
{
	int i;

	prt(realm_desc(), 20, 15);

	/* Print all the skills. prt_skill_rank has to print both the
	 * skills on the line if it's printing to the first column, so we
	 * get to save some effort. */
	for (i = 0; i < NUM_SK_USED / 2; i++)
		prt_skill_rank(i);
}

/* 
 * Allow the player to examine and improve his skill levels.
 *
 * Really should do something about the cursor wandering.
 */
void
do_cmd_skills()
{
	int selected;
	char key;

	/* Save and clear the screen */
	screen_save();
	clear_from(0);

	/* Print the static information */
	prt("Type the appropriate letter to select a skill and", 21, 15);
	prt("<Spacebar> or <Enter> to advance it.", 22, 15);
	prt("Press ESC to exit.", 23, 15);

	print_all_skills();

	selected = p_ptr->lastadv;

	while (1)
	{
		/* Print the information on the current skill */
		prt_skill_select(selected);

		/* Get the player's selection. */
		key = inkey();

		if (key == ESCAPE)
			break;

		key = tolower(key);
		if (isalpha(key))
		{
			selected = A2I(key);
			continue;
		}

		/* Advance the skill if the key was return or space. */
		if (key == ' ' || key == '\n' || key == '\r')
		{
			if (!adv_skill(selected))
			{
				bell("Insufficient experience to raise skill");
				if (get_raw_skill(selected) < 255)
					prt("You have insufficient experience to advance that skill! -more-",
						0, 2);
				else
					prt("You can learn nothing more on that subject. -more-", 0, 2);

				/* Give them time to read it */
				key = inkey();
			}
			else
			{
				if (selected == S_MAGIC && !p_ptr->realm)
				{
					/* Must pick a realm */
					prt("You must pick a realm of magic. -more-", 0, 2);
					/* Give them time to read it */
					key = inkey();
					prt("Will you become a S)orcerer, P)riest, D)ruid, or N)ecromancer?", 0, 2);
					while (!p_ptr->realm)
					{
						key = inkey();
						switch (key)
						{
						case 's':
						case 'S':
							p_ptr->realm = MAGE;
							break;
						case 'p':
						case 'P':
							p_ptr->realm = PRIEST;
							break;
						case 'd':
						case 'D':
							p_ptr->realm = DRUID;
							break;
						case 'n':
						case 'N':
							p_ptr->realm = NECRO;
							break;
						default:
							bell("Invalid magic realm choice.");
							break;
						}
					}
					prt(realm_desc(), 20, 15);
					mp_ptr = &magic_info[p_ptr->realm];
				}
			}
			prt_skill_rank(selected);
			continue;
		}
		bell("Illegal skill option.");
	}

	p_ptr->lastadv = selected;

	screen_load();
}
