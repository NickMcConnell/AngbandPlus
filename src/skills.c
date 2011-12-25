#include "angband.h"

skill_table *s_info;

int skills_bow_current(int sval)
{
	int max = skills_weapon_max(TV_BOW, sval);
	int cur = p_ptr->weapon_exp[0][sval];

	if (cur > max)
		cur = max;

	return cur;
}

int skills_bow_max(int sval)
{
	if (mut_present(MUT_WEAPON_SKILLS))
		return WEAPON_EXP_MASTER;

	return s_info[p_ptr->pclass].w_max[0][sval];
}

void skills_bow_gain(int sval)
{
	int max = skills_weapon_max(TV_BOW, sval);
	int cur = p_ptr->weapon_exp[0][sval];

	if (cur < max)
	{
		int add = 0;
		
		if (cur < WEAPON_EXP_BEGINNER) add = 80;
		else if (cur < WEAPON_EXP_SKILLED) add = 25;
		else if (cur < WEAPON_EXP_EXPERT && p_ptr->lev > 19) add = 10;
		else if (p_ptr->lev > 34) add = 2;

		if (add > 0)
		{
			cur += add;
			if (cur > max)
				cur = max;
			p_ptr->weapon_exp[0][sval] += add;
			p_ptr->update |= (PU_BONUS);
		}
	}
}

int skills_weapon_current(int tval, int sval)
{
	int max = skills_weapon_max(tval, sval);
	int cur = p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

	if (cur > max)
		cur = max;

	return cur;
}

int skills_weapon_max(int tval, int sval)
{
	if (mut_present(MUT_WEAPON_SKILLS))
		return WEAPON_EXP_MASTER;

	return s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval];
}

void skills_weapon_gain(int tval, int sval)
{
	int max = skills_weapon_max(tval, sval);
	int cur = p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

	if (cur < max)
	{
		int add = 0;
		
		if (cur < WEAPON_EXP_BEGINNER) add = 80;
		else if (cur < WEAPON_EXP_SKILLED) add = 10;
		else if (cur < WEAPON_EXP_EXPERT && p_ptr->lev > 19) add = 1;
		else if (p_ptr->lev > 34 && one_in_(2)) add = 1;

		if (add > 0)
		{
			cur += add;
			if (cur > max)
				cur = max;
			p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval] += add;
			p_ptr->update |= (PU_BONUS);
		}
	}
}

bool skills_weapon_is_icky(int tval, int sval)
{
	bool result = FALSE;

	/* Some classes use weapon skill tables to determine allowable weapons.
	   But if the character gains the Weapon Versatility ability, all weapons
	   will be masterable, even "icky" ones ... */
	switch (p_ptr->pclass)
	{
	case CLASS_MONK:
	case CLASS_FORCETRAINER:
		if (s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] == WEAPON_EXP_UNSKILLED)
			result = TRUE;
		break;

	case CLASS_NINJA:
		if (s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] <= WEAPON_EXP_BEGINNER)
			result = TRUE;
		break;
	}
	return result;
}