#include "angband.h"

static void _calc_bonuses(void)
{
	p_ptr->to_a -= 50;
	p_ptr->dis_to_a -= 50;
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
	int to_d = 10 + p_ptr->lev/2 - o_ptr->weight/10;
	info_ptr->to_d += to_d;
	info_ptr->dis_to_d += to_d;
}

class_t *duelist_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{
		me.calc_bonuses = _calc_bonuses;
		me.calc_weapon_bonuses = _calc_weapon_bonuses;
		init = TRUE;
	}

	/* dynamic info */

	return &me;
}