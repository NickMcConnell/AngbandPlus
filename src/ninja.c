#include "angband.h"

void quick_walk_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Quick Walk", "®�"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		if (p_ptr->action == ACTION_HAYAGAKE) set_action(ACTION_NONE);
		else set_action(ACTION_HAYAGAKE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		var_set_int(res, 0);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}