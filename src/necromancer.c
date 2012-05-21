#include "angband.h"

/* Stub ... At the moment, all the spells are in do-spell.c */

static void _calc_bonuses(void)
{
	p_ptr->align -= 200;
	p_ptr->spell_cap += 2;
	if (p_ptr->lev >= 5) p_ptr->resist_cold = TRUE;
	if (p_ptr->lev >= 15) p_ptr->see_inv = TRUE;
	if (p_ptr->lev >= 25) p_ptr->hold_life = TRUE;
	if (p_ptr->lev >= 35) p_ptr->resist_pois = TRUE;
}

static int _get_powers(spell_info* spells, int max)
{
	int ct = 0;

	spell_info* spell = &spells[ct++];
	spell->level = 1;
	spell->cost = 1;
	spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
	spell->fn = animate_dead_spell;

	spell = &spells[ct++];
	spell->level = 5;
	spell->cost = 5;
	spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
	spell->fn = enslave_undead_spell;

	return ct;
}

class_t *necromancer_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 30,  40,  38,   4,  16,  20,  34,  20};
	skills_t xs = {  7,  15,  11,   0,   0,   0,   6,   7};

		me.name = "Necromancer";
		me.desc = "A necromancer attempts to gain both power and knowledge through "
					"communion with the dead.  A powerful necromancer is truly "
					"awe inspiring, and may even kill foes with a single touch! "
					"They forever hunt for the legendary Eye and Hand of Vecna in "
					"order to complete their power.",
		me.stats[A_STR] = -2;
		me.stats[A_INT] =  3;
		me.stats[A_WIS] =  0;
		me.stats[A_DEX] =  1;
		me.stats[A_CON] = -1;
		me.stats[A_CHR] =  1;
		me.hd = 1;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.calc_bonuses = _calc_bonuses;
		me.get_powers = _get_powers;
		init = TRUE;
	}

	return &me;
}