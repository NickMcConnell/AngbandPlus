#include "angband.h"

void rodeo_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Rodeo", "荒馬ならし"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
	{
		char m_name[80];
		monster_type *m_ptr;
		monster_race *r_ptr;
		int rlev;

		var_set_bool(res, FALSE);
		if (p_ptr->riding)
		{
			msg_print(T("You are already riding.", "今は乗馬中だ。"));
			return;
		}
		if (!do_riding(TRUE)) return;
		
		var_set_bool(res, TRUE);

		m_ptr = &m_list[p_ptr->riding];
		r_ptr = &r_info[m_ptr->r_idx];
		monster_desc(m_name, m_ptr, 0);
		msg_format(T("You ride on %s.", "%sに乗った。"),m_name);
		if (is_pet(m_ptr)) break;
		rlev = r_ptr->level;
		if (r_ptr->flags1 & RF1_UNIQUE) rlev = rlev * 3 / 2;
		if (rlev > 60) rlev = 60+(rlev-60)/2;
		if ((randint1(p_ptr->skill_exp[GINOU_RIDING] / 120 + p_ptr->lev * 2 / 3) > rlev)
			&& one_in_(2) && !p_ptr->inside_arena && !p_ptr->inside_battle
			&& !(r_ptr->flags7 & (RF7_GUARDIAN)) && !(r_ptr->flags1 & (RF1_QUESTOR))
			&& (rlev < p_ptr->lev * 3 / 2 + randint0(p_ptr->lev / 5)))
		{
			msg_format(T("You tame %s.", "%sを手なずけた。"),m_name);
			set_pet(m_ptr);
		}
		else
		{
			msg_format(T("You have thrown off by %s.", "%sに振り落とされた！"),m_name);
			rakuba(1,TRUE);

			/* Paranoia */
			/* 落馬処理に失敗してもとにかく乗馬解除 */
			p_ptr->riding = 0;
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}