#include "angband.h"

void blue_learning_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Learning", "ラーニング"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		if (p_ptr->action == ACTION_LEARN)
			set_action(ACTION_NONE);
		else
			set_action(ACTION_LEARN);
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