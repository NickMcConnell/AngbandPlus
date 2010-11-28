#include "angband.h"

static bool choose_kata(void)
{
	char choice;
	int new_kata = 0;
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

	if (p_ptr->stun)
	{
#ifdef JP
		msg_print("意識がはっきりとしない。");
#else
		msg_print("You are not clear headed");
#endif
		return FALSE;
	}

	if (p_ptr->afraid)
	{
#ifdef JP
		msg_print("体が震えて構えられない！");
#else
		msg_print("You are trembling with fear!");
#endif
		return FALSE;
	}

	/* Save screen */
	screen_save();

#ifdef JP
	prt(" a) 型を崩す", 2, 20);
#else
	prt(" a) No Form", 2, 20);
#endif

	for (i = 0; i < MAX_KATA; i++)
	{
		if (p_ptr->lev >= kata_shurui[i].min_level)
		{
#ifdef JP
			sprintf(buf," %c) %sの型    %s",I2A(i+1), kata_shurui[i].desc, kata_shurui[i].info);
#else
			sprintf(buf," %c) Form of %-12s  %s",I2A(i+1), kata_shurui[i].desc, kata_shurui[i].info);
#endif
			prt(buf, 3+i, 20);
		}
	}

	prt("", 1, 0);
#ifdef JP
	prt("        どの型で構えますか？", 1, 14);
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
			if (p_ptr->action == ACTION_KATA)
			{
				set_action(ACTION_NONE);
			}
			else
#ifdef JP
				msg_print("もともと構えていない。");
#else
				msg_print("You are not assuming posture.");
#endif
			screen_load();
			return TRUE;
		}
		else if ((choice == 'b') || (choice == 'B'))
		{
			new_kata = 0;
			break;
		}
		else if (((choice == 'c') || (choice == 'C')) && (p_ptr->lev > 29))
		{
			new_kata = 1;
			break;
		}
		else if (((choice == 'd') || (choice == 'D')) && (p_ptr->lev > 34))
		{
			new_kata = 2;
			break;
		}
		else if (((choice == 'e') || (choice == 'E')) && (p_ptr->lev > 39))
		{
			new_kata = 3;
			break;
		}
	}
	set_action(ACTION_KATA);

	if (p_ptr->special_defense & (KATA_IAI << new_kata))
	{
#ifdef JP
		msg_print("構え直した。");
#else
		msg_print("You reassume a posture.");
#endif
	}
	else
	{
		p_ptr->special_defense &= ~(KATA_MASK);
		p_ptr->update |= (PU_BONUS);
		p_ptr->update |= (PU_MONSTERS);
#ifdef JP
		msg_format("%sの型で構えた。",kata_shurui[new_kata].desc);
#else
		msg_format("You assume a posture of %s form.",kata_shurui[new_kata].desc);
#endif
		p_ptr->special_defense |= (KATA_IAI << new_kata);
	}
	p_ptr->redraw |= (PR_STATE);
	p_ptr->redraw |= (PR_STATUS);
	screen_load();
	return TRUE;
}

void samurai_concentration_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Concentration", "気合いため"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
	{
		int max_csp = MAX(p_ptr->msp*4, p_ptr->lev*5+5);
		var_set_bool(res, FALSE);
		if (total_friends)
		{
			msg_print(T("You need concentration on the pets now.", "今はペットを操ることに集中していないと。"));
			return;
		}
		if (p_ptr->special_defense & KATA_MASK)
		{
			msg_print(T("You need concentration on your form.", "今は構えに集中している。"));
			return;
		}
		
		msg_print(T("You concentrate to charge your power.", "精神を集中して気合いを溜めた。"));

		p_ptr->csp += p_ptr->msp / 2;
		if (p_ptr->csp >= max_csp)
		{
			p_ptr->csp = max_csp;
			p_ptr->csp_frac = 0;
		}

		p_ptr->redraw |= (PR_MANA);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void samurai_posture_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Assume a Guard Position", "型"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!buki_motteruka(INVEN_RARM) && !buki_motteruka(INVEN_LARM))
		{
			msg_print(T("You need to wield a weapon.", "武器を持たないといけません。"));
			return;
		}
		if (!choose_kata()) return;

		p_ptr->update |= (PU_BONUS);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}