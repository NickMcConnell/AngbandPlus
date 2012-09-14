#include "angband.h"

/****************************************************************
 * Private Helpers
 ****************************************************************/

/* Finding what monster to evolve into is trivial, since the monster_race type
   keeps a pointer in that direction.  However, we would like to reverse evolution
   turning harder monsters into easier ones.  This fn will scan the monster race
   table looking for a monster that evolves into this one.  Of course, we assume
   there is at most one such race to be found (Not True!)
   Returns 0 if no such race can be found.
*/
static int _find_evolution_idx(int r_idx)
{
	monster_race *r_ptr;

	if (r_idx <= 0) return 0;
	r_ptr = &r_info[r_idx];
	return r_ptr->next_r_idx;
}

static int _find_devolution_idx(int r_idx)
{
	int i;

	if (r_idx <= 0) return 0;

	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];
		if (r_ptr->next_r_idx == r_idx)
			return i;
	}

	return 0;
}

/*	Evolve or Devolve a Monster.  I spiked this from monster_gain_exp() in melee2.c without
	any great understanding on my part.
*/
static void _change_monster_race(int m_idx, int new_r_idx)
{
	char m_name[80];
	int old_hp, old_maxhp, old_r_idx;
	byte old_sub_align;
	monster_type *m_ptr;
	monster_race *r_ptr;

	if (m_idx <= 0 || new_r_idx <= 0) return;

	m_ptr = &m_list[m_idx];
	old_hp = m_ptr->hp;
	old_maxhp = m_ptr->max_maxhp;
	old_r_idx = m_ptr->r_idx;
	old_sub_align = m_ptr->sub_align;

	real_r_ptr(m_ptr)->cur_num--;

	monster_desc(m_name, m_ptr, 0);
	m_ptr->r_idx = new_r_idx;

	real_r_ptr(m_ptr)->cur_num++;

	m_ptr->ap_r_idx = m_ptr->r_idx;
	r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags1 & RF1_FORCE_MAXHP)
	{
		m_ptr->max_maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->max_maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}
	if (ironman_nightmare)
	{
		u32b hp = m_ptr->max_maxhp * 2L;

		m_ptr->max_maxhp = (s16b)MIN(30000, hp);
	}
	m_ptr->maxhp = m_ptr->max_maxhp;
	m_ptr->hp = old_hp * m_ptr->maxhp / old_maxhp;

	m_ptr->mspeed = get_mspeed(r_ptr);

	if (!is_pet(m_ptr) && !(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)))
		m_ptr->sub_align = old_sub_align;
	else
	{
		m_ptr->sub_align = SUB_ALIGN_NEUTRAL;
		if (r_ptr->flags3 & RF3_EVIL) m_ptr->sub_align |= SUB_ALIGN_EVIL;
		if (r_ptr->flags3 & RF3_GOOD) m_ptr->sub_align |= SUB_ALIGN_GOOD;
	}

	m_ptr->exp = 0;

	if (is_pet(m_ptr) || m_ptr->ml)
	{
		if (!ignore_unview || player_can_see_bold(m_ptr->fy, m_ptr->fx))
		{
			if (p_ptr->image)
			{
				monster_race *hallu_race;
				do
				{
					hallu_race = &r_info[randint1(max_r_idx - 1)];
				}
				while (!hallu_race->name || (hallu_race->flags1 & RF1_UNIQUE));
				msg_format(T("%^s changed into %s.", "%sは%sに進化した。"), m_name, r_name + hallu_race->name);
			}
			else
				msg_format(T("%^s changed into %s.", "%sは%sに進化した。"), m_name, r_name + r_ptr->name);
		}
		if (!p_ptr->image) r_info[old_r_idx].r_xtra1 |= MR1_SINKA;
		m_ptr->parent_m_idx = 0;
	}
	update_mon(m_idx, FALSE);
	lite_spot(m_ptr->fy, m_ptr->fx);
}

static bool _monster_save(monster_race* r_ptr, int power)
{
	if (r_ptr->flagsr & RFR_RES_ALL)
		return TRUE;
	else if (r_ptr->flags1 & RF1_UNIQUE)
		return r_ptr->level > randint1(2*power/3);
	else
		return r_ptr->level > randint1(power);
}
		
/****************************************************************
 * Private Spells
 ****************************************************************/
static void _back_to_origins_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Back to Origins");
		break;
	case SPELL_DESC:
		var_set_string(res, "Eliminate monster offspring.");
		break;
	case SPELL_CAST:
	{
		int i, ct;

		ct = 0;
		for (i = 1; i < max_m_idx; i++)
		{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr;

			if (!m_ptr->r_idx) continue;
			r_ptr = real_r_ptr(m_ptr);
			if ( (r_ptr->flags2 & RF2_MULTIPLY)
				&& r_ptr->cur_num > 1  /* shouldn't this be 2 ... well, breeding in *band has never been biologically accurate */
				&& !_monster_save(r_ptr, 3*p_ptr->lev) )
			{
				delete_monster_idx(i);
				ct++;
			}
		}

		if (ct > 0)
			msg_print("You feel the local population has reverted to an earlier state.");
		else
			msg_print("You feel the local population is stable.");
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _decay_door_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Decay Door");
		break;
	case SPELL_DESC:
		var_set_string(res, "Destroy an adjacent door.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		var_set_bool(res, TRUE);

		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!in_bounds(y, x)) return;
		if (!cave_have_flag_bold(y, x, FF_DOOR)) return;
	
		cave_alter_feat(y, x, FF_TUNNEL);
		if (!cave_have_flag_bold(y, x, FF_DOOR)) /* Hack: Permanent Door in Arena! */
		{
			msg_print("The door withers away.");
			p_ptr->update |= (PU_FLOW);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _decay_wall_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Decay Wall");
		break;
	case SPELL_DESC:
		var_set_string(res, "Destroy an adjacent wall.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		var_set_bool(res, TRUE);

		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];
			
		if (!in_bounds(y, x)) return;
		if (!cave_have_flag_bold(y, x, FF_HURT_ROCK)) return;
	
		cave_alter_feat(y, x, FF_HURT_ROCK);
		msg_print("The wall turns to dust.");
	
		p_ptr->update |= (PU_FLOW);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _devolution_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Devolution");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to reverse evolution for a single opponent.");
		break;
	case SPELL_CAST:
	{
		int y, x, r_idx, m_idx, dir;
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		var_set_bool(res, TRUE);

		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!in_bounds(y, x)) return;

		m_idx = cave[y][x].m_idx;
		if (!m_idx)
		{
			msg_print("There is no monster there!");
			return;
		}

		m_ptr = &m_list[m_idx];
		if (!m_ptr->r_idx) return;
		monster_desc(m_name, m_ptr, 0);

		r_idx = real_r_idx(m_ptr);
		if (r_idx <= 0) return;
		r_ptr = &r_info[r_idx];	/* We'll use the current race for a saving throw */
		r_idx = _find_devolution_idx(r_idx);
			
		if (r_idx <= 0)
		{
			msg_format("%^s is too primitive for further devolution.", m_name);
			return;
		}

		set_monster_csleep(m_idx, 0);
		if (_monster_save(r_ptr, 2*p_ptr->lev))
			msg_format("%^s resists.", m_name);
		else
			_change_monster_race(m_idx, r_idx);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _evolution_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Evolution");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to advance evolution for a single opponent.");
		break;
	case SPELL_CAST:
	{
		int y, x, r_idx, m_idx, dir;
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		var_set_bool(res, TRUE);

		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!in_bounds(y, x)) return;

		m_idx = cave[y][x].m_idx;
		if (!m_idx)
		{
			msg_print("There is no monster there!");
			return;
		}

		m_ptr = &m_list[m_idx];
		if (!m_ptr->r_idx) break;
		monster_desc(m_name, m_ptr, 0);

		r_idx = real_r_idx(m_ptr);
		if (r_idx <= 0) return;
		r_idx = _find_evolution_idx(r_idx);
			
		if (r_idx <= 0)
		{
			msg_format("%^s has reached evolutionary perfection.", m_name);
			return;
		}
		r_ptr = &r_info[r_idx];	/* We'll use the target race for a saving throw */
		set_monster_csleep(m_idx, 0);
		if (_monster_save(r_ptr, 2*p_ptr->lev))
			msg_format("%^s resists.", m_name);
		else
			_change_monster_race(m_idx, r_idx);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _haste_monster_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Haste Monster");
		break;
	case SPELL_DESC:
		var_set_string(res, "Target monster gains a temporary speed boost.");
		break;
	case SPELL_CAST:
	{
		int dir;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		var_set_bool(res, TRUE);

		speed_monster(dir);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _haste_self_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Haste Self");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain a temporary speed boost.");
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Grants +15 speed for 10+d(3L/2) rounds.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_duration(10, p_ptr->lev * 3 / 2));
		break;
	case SPELL_CAST:
		set_fast(10 + randint1(p_ptr->lev * 3 / 2), FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _mass_slow_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Mass Slow");
		break;
	case SPELL_DESC:
		var_set_string(res, "Slow all monsters in sight.");
		break;
	case SPELL_CAST:
		project_hack(GF_OLD_SLOW, 3*p_ptr->lev + 10);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _remembrance_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Remembrance");
		break;
	case SPELL_DESC:
		var_set_string(res, "Restores life and stats.");
		break;
	case SPELL_CAST:
		do_res_stat(A_STR);
		do_res_stat(A_INT);
		do_res_stat(A_WIS);
		do_res_stat(A_DEX);
		do_res_stat(A_CON);
		do_res_stat(A_CHR);
		restore_level();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _rewind_time_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rewind Time");
		break;
	case SPELL_DESC:
		var_set_string(res, "Temporal escape:  You flee to safety, but forget some of your recent experiences.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!get_check("You will irreversibly alter the time line. Are you sure?")) return;
		var_set_bool(res, TRUE);

		if (p_ptr->inside_arena || ironman_downward || !dun_level)
		{
			msg_print("Nothing happens.");
			return;
		}

		recall_player(1);
		p_ptr->leaving_method = LEAVING_REWIND_TIME; /* Set after recall_player() to override LEAVING_RECALL */
		process_world_aux_movement(); /* Hack! Recall Now, Now, Now!!! */

		if (p_ptr->prace == RACE_ANDROID)
		{
			dec_stat(A_CON, 10, TRUE);
			if (one_in_(2)) return;
			dec_stat(A_INT, 10, TRUE);
			if (one_in_(2)) return;
			dec_stat(A_DEX, 10, TRUE);
			if (one_in_(2)) return;
			dec_stat(A_WIS, 10, TRUE);
			if (one_in_(2)) return;
			dec_stat(A_STR, 10, TRUE);
			if (one_in_(2)) return;
			dec_stat(A_CHR, 10, TRUE);
		}
		else
		{
			int amount = 0;
			
			if (p_ptr->lev < 3) return;
			amount = exp_requirement(p_ptr->lev-1);
			amount -= exp_requirement(p_ptr->lev-2);
			if (amount > 100000) amount = 100000;
			if (amount > p_ptr->max_exp) amount = p_ptr->max_exp;
			if (amount > p_ptr->exp) p_ptr->exp = 0;
			else p_ptr->exp -= amount;
			p_ptr->max_exp -= amount;
			check_experience();
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _shrike_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Time Mastery");
		break;
	case SPELL_DESC:
		var_set_string(res, "You control the rate of the flow of time. Many actions require the normal "
		                    "passage of time, such as attacking, casting spells or using magical devices. "
							"But other actions, such as movement, reading scrolls or quaffing potions, "
							"are performed much more quickly.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_duration(5, 5));
		break;
	case SPELL_CAST:
		set_tim_shrike(5 + randint1(5), FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _slow_monster_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Slow Monster");
		break;
	case SPELL_DESC:
		var_set_string(res, "Slow a single adjacent monster.");
		break;
	case SPELL_CAST:
	{
		int y, x, m_idx, dir;
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		var_set_bool(res, TRUE);

		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!in_bounds(y, x)) return;

		m_idx = cave[y][x].m_idx;
		if (!m_idx)
		{
			msg_print("There is no monster there!");
			return;
		}

		m_ptr = &m_list[m_idx];
		if (!m_ptr->r_idx) return;
		monster_desc(m_name, m_ptr, 0);
		r_ptr = &r_info[m_ptr->r_idx];
		set_monster_csleep(m_idx, 0);

		if (_monster_save(r_ptr, 3*p_ptr->lev))
		{		
			msg_format("%^s resists.", m_name);
		}
		else if (set_monster_slow(m_idx, MON_SLOW(m_ptr) + 50))
			msg_format("%^s starts moving slower.", m_name);
		else
			msg_format("%^s is already slow.", m_name);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _speed_essentia_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Speed Essentia");
		break;
	case SPELL_DESC:
		var_set_string(res, "Increases your melee and missile attacks for a bit.");
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Grants +2 attacks and +1 shots for 3 + (L-43)/3 rounds.");
		break;
	case SPELL_INFO:
		var_set_string(res, format("dur %d", 3 + (p_ptr->lev - 43)/3));
		break;
	case SPELL_CAST:
		set_tim_speed_essentia(3 + (p_ptr->lev - 43)/3, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void _stop_time_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Stop Time");
		break;
	case SPELL_DESC:
		var_set_string(res, "Spend all of your spell points to stop time. You gain a number of free moves depending on the amount of spell points spent.");
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Stops time for (SP+50)/80 actions (but no more than 6 free actions).");
		break;
	case SPELL_INFO:
		var_set_string(res, format(T("%ld acts.", "行動:%ld回"), MIN((p_ptr->csp + 100-p_ptr->energy_need - 50)/100, 5)));
		break;
	case SPELL_CAST:
	{
		var_set_bool(res, FALSE);
		if (world_player)
		{
			msg_print(T("Time is already stopped.", "既に時は止まっている。"));
			return;
		}

		world_player = TRUE;
		msg_print(T("You yell 'Time!'", "「時よ！」"));
		msg_print(NULL);

		/* Note: We pay the casting cost up front these days.  So, add back the 150
		   to figure the starting sp, and then bash sp down to 0. We can't use the 
		   SPELL_COST_EXTRA mechanism here ... */
		p_ptr->energy_need -= 1000 + (100 + (p_ptr->csp + 150) - 50)*TURNS_PER_TICK/10;
		p_ptr->energy_need = MAX(-1550, p_ptr->energy_need);

		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		p_ptr->redraw |= (PR_MAP | PR_STATUS);
		p_ptr->update |= (PU_MONSTERS);
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		handle_stuff();

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _temporal_prison_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Temporal Prison");
		break;
	case SPELL_DESC:
		var_set_string(res, "Imprison a single opponent in a time cage.");
		break;
	case SPELL_CAST:
	{
		int y, x, m_idx, dir;
		monster_type *m_ptr;
		monster_race *r_ptr;
		char m_name[80];

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		var_set_bool(res, TRUE);

		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!in_bounds(y, x)) return;

		m_idx = cave[y][x].m_idx;
		if (!m_idx)
		{
			msg_print("There is no monster there!");
			return;
		}
		m_ptr = &m_list[m_idx];

		if (!m_ptr->r_idx) return;
		monster_desc(m_name, m_ptr, 0);
		r_ptr = &r_info[m_ptr->r_idx];
		set_monster_csleep(m_idx, 0);

		if (_monster_save(r_ptr, 3*p_ptr->lev))
		{		
			msg_format("%^s resists.", m_name);
		}
		else 
		{
			msg_format("%^s is suspended!", m_name);
			set_monster_csleep(m_idx, 1500);
		}			
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _time_spurt_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Time Spurt");
		break;
	case SPELL_DESC:
		var_set_string(res, "Gives a very short speed boost.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_duration(3, 3));
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Grants +5 speed for 3+d3 rounds. This speed boost does not stack with Haste Self.");
		break;
	case SPELL_CAST:
		set_tim_spurt(spell_power(3 + randint1(3)), FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    {  1,  5, 30, _time_spurt_spell},
	{  5,  4, 30, _decay_door_spell},
	{ 10, 10, 50, _decay_wall_spell},
	{ 13, 10, 50, _devolution_spell},
	{ 15, 10, 50, _evolution_spell},
	{ 17, 15, 50, _slow_monster_spell},
	{ 20, 15, 50, _back_to_origins_spell},
	{ 23, 15, 60, _haste_self_spell},
	{ 23, 15, 60, _haste_monster_spell},
	{ 27, 20, 50, _mass_slow_spell},
	{ 30, 20, 50, _temporal_prison_spell},
	{ 35, 90, 70, _rewind_time_spell},
	{ 40, 80, 70, _remembrance_spell},
	{ 43, 60, 70, _speed_essentia_spell},
	{ 47, 70, 75, _shrike_spell},
/*	{ 50,150, 70, _stop_time_spell}, */
	{ -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
	return get_spells_aux(spells, max, _spells, p_ptr->stat_ind[A_INT]);
}

static void _calc_bonuses(void)
{
	if (p_ptr->lev > 29) p_ptr->resist_time = TRUE;
	p_ptr->pspeed += 3;
	p_ptr->pspeed += (p_ptr->lev) / 7;
	if (p_ptr->lev >= 35) 
		p_ptr->pspeed += 3;
	if (p_ptr->lev >= 45) 
		p_ptr->pspeed += 3;
}

static void _on_fail(const spell_info *spell)
{
	if (randint1(100) < (spell->fail / 2))
	{
		int b = randint1(100);
		if (b <= 80)
		{
		}
		else if (b <= 90)
		{
			set_fast(0, TRUE);
			set_slow(randint1(50) + 25, FALSE);
			msg_print("You feel caught in a temporal inversion!");
		}
		else if (b <= 95)
		{
			lose_exp(p_ptr->exp / 4);
			msg_print("You feel life's experiences fade away!");
		}
		else
		{
			dec_stat(A_DEX, 10, FALSE);
			dec_stat(A_WIS, 10, FALSE);
			dec_stat(A_CON, 10, FALSE);
			dec_stat(A_STR, 10, FALSE);
			dec_stat(A_CHR, 10, FALSE);
			dec_stat(A_INT, 10, FALSE);
			msg_print("You feel as weak as a newborn kitten!");
		}
	}
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "timecraft";
		me.use_sp = TRUE;
		me.on_fail = _on_fail;
		init = TRUE;
	}
	return &me;
}

static void _spoiler_dump(FILE* fff)
{
	spoil_spells_aux(fff, _spells);
	fprintf(fff, "\n\n*Note:* Failing to cast a spell can have bad side effects if 1d100 < Fail/2. Effects include:\n");
	fprintf(fff, "  * Nothing (80%%)\n");
	fprintf(fff, "  * Temporal Inversion (10%%): Player is slowed\n");
	fprintf(fff, "  * Forgetfulness (5%%): Player loses 25%% of their XP\n");
	fprintf(fff, "  * Diminution (5%%): All stats are reduced\n");


	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * +3+L/7 to Speed\n");
	fprintf(fff, "  * +3 to Speed and -2 to Str,Dex,Con at L35 and again at L45\n");
	fprintf(fff, "  * Resist Time at L30\n");
	fprintf(fff, "  * Durations of good effects are increased\n");
	fprintf(fff, "  * Durations of bad effects are decreased\n");
	fprintf(fff, "  * Haste gives +15 speed rather than +10\n");
	fprintf(fff, "  * Slow gives -5 speed rather than -10\n");
}

class_t *time_lord_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 25,  18,  35,   1,  16,   8,  48,  20 };
	skills_t xs = {  7,   7,  10,   0,   0,   0,  13,  13 };

		me.name = "Time-Lord";
		me.desc = "Time-Lords are masters of time.  They are mediocre fighters, but their temporal "
					"mastery gives them great speed, as well as the ability to manipulate time.";

		me.base_skills = bs;
		me.extra_skills = xs;
		me.hd = 0; 
		me.exp = 125;
		me.pets = 20;

		me.calc_bonuses = _calc_bonuses;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		me.spoiler_dump = _spoiler_dump;
		init = TRUE;
	}

	me.stats[A_STR] =  0;
	me.stats[A_INT] =  3;
	me.stats[A_WIS] =  1;
	me.stats[A_DEX] =  0;
	me.stats[A_CON] =  0;
	me.stats[A_CHR] =  1;
	if (!spoiler_hack)
	{
		int amount = 0;
		
		if (p_ptr->lev >= 35) amount += 2;
		if (p_ptr->lev >= 45) amount += 2;

		me.stats[A_STR] -= amount;
		me.stats[A_DEX] -= amount;
		me.stats[A_CON] -= amount;
	}
	return &me;
}
