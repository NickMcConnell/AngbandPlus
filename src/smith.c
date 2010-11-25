#include "angband.h"

void smith_judgment_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Judgment", "ÌÜÍø¤­"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		if (p_ptr->lev > 29)
			var_set_bool(res, identify_fully(TRUE));
		else
			var_set_bool(res, ident_spell(TRUE));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}


