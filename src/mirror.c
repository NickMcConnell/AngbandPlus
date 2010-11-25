#include "angband.h"


/*
 *  Return TRUE if there is a mirror on the grid.
 */
bool is_mirror_grid(cave_type *c_ptr)
{
	if ((c_ptr->info & CAVE_OBJECT) && have_flag(f_info[c_ptr->mimic].flags, FF_MIRROR))
		return TRUE;
	else
		return FALSE;
}

/*
 * Mirror Master's Dimension Door
 */
bool mirror_tunnel(void)
{
	int x = 0, y = 0;

	/* Rerutn FALSE if cancelled */
	if (!tgt_pt(&x, &y)) return FALSE;

	if (dimension_door_aux(x, y)) return TRUE;

#ifdef JP
	msg_print("鏡の世界をうまく通れなかった！");
#else
	msg_print("You fail to pass the mirror plane correctly!");
#endif

	return TRUE;
}

bool place_mirror(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
#ifdef JP
msg_print("床上のアイテムが呪文を跳ね返した。");
#else
		msg_print("The object resists the spell.");
#endif

		return FALSE;
	}

	/* Create a mirror */
	cave[py][px].info |= CAVE_OBJECT;
	cave[py][px].mimic = feat_mirror;

	/* Turn on the light */
	cave[py][px].info |= CAVE_GLOW;

	/* Notice */
	note_spot(py, px);

	/* Redraw */
	lite_spot(py, px);

	update_local_illumination(py, px);

	return TRUE;
}

/* Remove a mirror */
void remove_mirror(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Remove the mirror */
	c_ptr->info &= ~(CAVE_OBJECT);
	c_ptr->mimic = 0;

	if (d_info[dungeon_type].flags1 & DF1_DARKNESS)
	{
		c_ptr->info &= ~(CAVE_GLOW);
		if (!view_torch_grids) c_ptr->info &= ~(CAVE_MARK);

		/* Update the monster */
		if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

		update_local_illumination(y, x);
	}

	/* Notice */
	note_spot(y, x);

	/* Redraw */
	lite_spot(y, x);
}

/*
 * Remove all mirrors in this floor
 */
void remove_all_mirrors(bool explode)
{
	int x, y;

	for (x = 0; x < cur_wid; x++)
	{
		for (y = 0; y < cur_hgt; y++)
		{
			if (is_mirror_grid(&cave[y][x]))
			{
				remove_mirror(y, x);
				if (explode)
					project(0, 2, y, x, p_ptr->lev / 2 + 5, GF_SHARDS,
						(PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP | PROJECT_NO_HANGEKI), -1);
			}
		}
	}
}

void break_mirrors_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Break Mirrors", "鏡割り"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Destroys all mirrors on the current levels.  Monsters close to a mirror take damage.");
		break;
	case SPELL_CAST:
		remove_all_mirrors(TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void mirror_concentration_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mirror Concentration", "静水"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (total_friends)
		{
			msg_print(T("You need concentration on the pets now.", "今はペットを操ることに集中していないと。"));
			return;
		}
		if (is_mirror_grid(&cave[py][px]))
		{
			msg_print(T("You feel your head clear a little.", "少し頭がハッキリした。"));

			p_ptr->csp += (5 + p_ptr->lev * p_ptr->lev / 100);
			if (p_ptr->csp >= p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
			}

			p_ptr->redraw |= (PR_MANA);
			var_set_bool(res, TRUE);
		}
		else
		{
			msg_print(T("You need to on a mirror to use this spell!", "鏡の上でないと集中できない！"));
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}