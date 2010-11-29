#include "angband.h"

static bool _monk_check_spell(void)
{
	if (p_ptr->pclass != CLASS_WILD_TALENT && !(empty_hands(TRUE) & EMPTY_HAND_RARM))
	{
		msg_print(T("You need to be bare hand.", "素手じゃないとできません。"));
		return FALSE;
	}
	if (p_ptr->riding)
	{
		msg_print(T("You need to get off a pet.", "乗馬中はできません。"));
		return FALSE;
	}
	return TRUE;
}

static bool choose_kamae(void)
{
	char choice;
	int new_kamae = 0;
	int i;
	char buf[80];

	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて構えられない！");
#else
		msg_print("Too confused.");
#endif
		return FALSE;
	}

	/* Save screen */
	screen_save();

#ifdef JP
	prt(" a) 構えをとく", 2, 20);
#else
	prt(" a) No form", 2, 20);
#endif

	for (i = 0; i < MAX_KAMAE; i++)
	{
		if (p_ptr->lev >= kamae_shurui[i].min_level)
		{
			sprintf(buf," %c) %-12s  %s",I2A(i+1), kamae_shurui[i].desc, kamae_shurui[i].info);
			prt(buf, 3+i, 20);
		}
	}

	prt("", 1, 0);
#ifdef JP
	prt("        どの構えをとりますか？", 1, 14);
#else
	prt("        Choose Form: ", 1, 14);
#endif

	while(1)
	{
		choice = inkey();

		if (choice == ESCAPE)
		{
			screen_load();
			return FALSE;
		}
		else if ((choice == 'a') || (choice == 'A'))
		{
			if (p_ptr->action == ACTION_KAMAE)
			{
				set_action(ACTION_NONE);
			}
			else
#ifdef JP
				msg_print("もともと構えていない。");
#else
				msg_print("You are not assuming a posture.");
#endif
			screen_load();
			return TRUE;
		}
		else if ((choice == 'b') || (choice == 'B'))
		{
			new_kamae = 0;
			break;
		}
		else if (((choice == 'c') || (choice == 'C')) && (p_ptr->lev > 29))
		{
			new_kamae = 1;
			break;
		}
		else if (((choice == 'd') || (choice == 'D')) && (p_ptr->lev > 34))
		{
			new_kamae = 2;
			break;
		}
		else if (((choice == 'e') || (choice == 'E')) && (p_ptr->lev > 39))
		{
			new_kamae = 3;
			break;
		}
	}
	set_action(ACTION_KAMAE);

	if (p_ptr->special_defense & (KAMAE_GENBU << new_kamae))
	{
#ifdef JP
		msg_print("構え直した。");
#else
		msg_print("You reassume a posture.");
#endif
	}
	else
	{
		p_ptr->special_defense &= ~(KAMAE_MASK);
		p_ptr->update |= (PU_BONUS);
		p_ptr->redraw |= (PR_STATE);
#ifdef JP
		msg_format("%sの構えをとった。",kamae_shurui[new_kamae].desc);
#else
		msg_format("You assume a posture of %s form.",kamae_shurui[new_kamae].desc);
#endif
		p_ptr->special_defense |= (KAMAE_GENBU << new_kamae);
	}
	p_ptr->redraw |= PR_STATE;
	screen_load();
	return TRUE;
}

void monk_double_attack_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Double Attack", "百裂拳"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (_monk_check_spell())
		{
			int x, y, dir = 0;

			if (!get_rep_dir(&dir, FALSE)) return;

			y = py + ddy[dir];
			x = px + ddx[dir];
			if (cave[y][x].m_idx)
			{
				if (one_in_(2)) msg_print(T("Ahhhtatatatatatatatatatatatatatataatatatatattaaaaa!!!!",
					                        "あーたたたたたたたたたたたたたたたたたたたたたた！！！" ));
				else msg_print(T("Oraoraoraoraoraoraoraoraoraoraoraoraoraoraoraoraora!!!!",
					                "オラオラオラオラオラオラオラオラオラオラオラオラ！！！"));

				py_attack(y, x, 0);
				if (cave[y][x].m_idx)
				{
					handle_stuff();
					py_attack(y, x, 0);
				}
			}
			else
			{
				msg_print(T("You don't see any monster in this direction", "その方向にはモンスターはいません。"));
				msg_print(NULL);
			}
			var_set_bool(res, TRUE);
		}
		break;
	case SPELL_ENERGY:
		var_set_int(res, 100 + ENERGY_NEED());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void monk_posture_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Assume a Posture", "構える"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if ( _monk_check_spell()
		  && choose_kamae() )
		{
			p_ptr->update |= (PU_BONUS);
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
