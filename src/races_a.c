#include "angband.h"

/****************************************************************
 * Human
 ****************************************************************/
 static void _human_gain_level(int new_level)
{
	if (new_level >= 30)
	{
		if (p_ptr->demigod_power[0] < 0)
		{
			int idx = mut_gain_choice(mut_human_pred);
			mut_lock(idx);
			p_ptr->demigod_power[0] = idx;
		}
	}
}

race_t *human_get_race_t(void)
{
	static race_t me = {0};
	static bool init = FALSE;

	if (!init)
	{
		/* TODO */
		me.gain_level = _human_gain_level;
		init = TRUE;
	}

	return &me;
}

