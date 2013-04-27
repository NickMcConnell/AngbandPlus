#include "angband.h"

bool player_is_monster_king(void)
{
	race_t *race_ptr;
	if (p_ptr->pclass != CLASS_MONSTER) return FALSE;
	race_ptr = get_race_t();
	if (!race_ptr->boss_r_idx) return FALSE;
	if (r_info[race_ptr->boss_r_idx].max_num) return FALSE;
	return TRUE;
}

class_t *monster_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 25,  18,  31,   1,  14,   2,  70,  55};
	skills_t xs = { 12,   7,  10,   0,   0,   0,  30,  30};

		me.name = "Monster";
		me.desc = "";

		me.stats[A_STR] =  0;
		me.stats[A_INT] =  0;
		me.stats[A_WIS] =  0;
		me.stats[A_DEX] =  0;
		me.stats[A_CON] =  0;
		me.stats[A_CHR] =  0;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.life = 100;
		me.exp = 100;
		me.pets = 25;
		init = TRUE;
	}

	return &me;
}
