/* File: spells3.c */

/* Purpose: Spell code (part 3) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* Maximum number of tries for teleporting */
#define MAX_TRIES 100

/* 1/x chance of reducing stats (for elemental attacks) */
#define HURT_CHANCE 16


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
bool teleport_away(int m_idx, int dis)
{
	int oy, ox, d, i, min;
	int tries = 0;
	int ny = 0, nx = 0;

	bool look = TRUE;

	cave_type    *c_ptr;
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Paranoia */
	if (!m_ptr->r_idx) return (FALSE);

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		tries++;

		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				ny = rand_spread(oy, dis);
				nx = rand_spread(ox, dis);
				d = distance(oy, ox, ny, nx);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds(ny, nx)) continue;

			/* Require "empty" floor space */
			if (!cave_empty_bold(ny, nx)) continue;

			c_ptr = &cave[ny][nx];

			if (!monster_can_cross_terrain(c_ptr->feat, r_ptr)) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (is_glyph_grid(&cave[ny][nx])) continue;
			if (is_explosive_rune_grid(&cave[ny][nx])) continue;

			/* No teleporting into vaults and such */
			if (!(p_ptr->inside_quest || p_ptr->inside_arena))
				if (c_ptr->info & CAVE_ICKY) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;

		/* Stop after MAX_TRIES tries */
		if (tries > MAX_TRIES) return (FALSE);
	}

	/* Sound */
	sound(SOUND_TPOTHER);

	/* Update the new location */
	cave[ny][nx].m_idx = m_idx;

	/* Update the old location */
	cave[oy][ox].m_idx = 0;

	/* Move the monster */
	m_ptr->fy = ny;
	m_ptr->fx = nx;

	/* Forget the counter target */
	reset_target(m_ptr);

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(oy, ox);

	/* Redraw the new grid */
	lite_spot(ny, nx);

	return (TRUE);
}



/*
 * Teleport monster next to a grid near the given location
 */
void teleport_to(int m_idx, int ty, int tx, int dis, int power, bool avoid_fall)
{
	int ny, nx, oy, ox, d, i, min;
	int attempts = 500;
	bool look = TRUE;

	cave_type    *c_ptr;
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* "Skill" test */
	if (randint1(100) > power) return;

	/* Initialize */
	ny = m_ptr->fy;
	nx = m_ptr->fx;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look && --attempts)
	{
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				ny = rand_spread(ty, dis);
				nx = rand_spread(tx, dis);
				d = distance(ty, tx, ny, nx);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds(ny, nx)) continue;

			/* Require "empty" floor space */
			if (!cave_empty_bold(ny, nx)) continue;

			c_ptr = &cave[ny][nx];

			if (!monster_can_cross_terrain(c_ptr->feat, r_ptr))
			{
				if ((c_ptr->feat != FEAT_AIR) || avoid_fall) continue;
			}

			/* Hack -- no teleport onto glyph of warding */
			if (is_glyph_grid(&cave[ny][nx])) continue;
			if (is_explosive_rune_grid(&cave[ny][nx])) continue;

			/* No teleporting into vaults and such */
			/* if (c_ptr->info & (CAVE_ICKY)) continue; */

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
	}

	if (attempts < 1) return;

	/* Sound */
	sound(SOUND_TPOTHER);

	/* Update the new location */
	cave[ny][nx].m_idx = m_idx;

	/* Update the old location */
	cave[oy][ox].m_idx = 0;

	/* Move the monster */
	m_ptr->fy = ny;
	m_ptr->fx = nx;

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(oy, ox);

	/* Redraw the new grid */
	lite_spot(ny, nx);

	if (cave[ny][nx].feat == FEAT_AIR) (void)mon_fall_into_air(m_idx);

	p_ptr->update |= (PU_MON_LITE);
}


/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 *
 * When long-range teleport effects are considered, there is a nasty
 * tendency to "bounce" the player between two or three different spots
 * because these are the only spots that are "far enough" way to satisfy
 * the algorithm.  Therefore, if the teleport distance is more than 50,
 * we decrease the minimum acceptable distance to try to increase randomness.
 * -GJW
 */
void teleport_player(int dis)
{
	int d, i, min, ox, oy;
	int tries = 0;

	int xx = -1, yy = -1;

	/* Initialize */
	int y = py;
	int x = px;

	bool look = TRUE;

	cave_type *c_ptr;

	if (p_ptr->wild_mode) return;

	if (p_ptr->anti_tele)
	{
#ifdef JP
msg_print("不思議な力がテレポートを防いだ！");
#else
		msg_print("A mysterious force prevents you from teleporting!");
#endif

		return;
	}

	if (dis > 200) dis = 200; /* To be on the safe side... */

	/* Minimum distance */
	min = dis / (dis > 50 ? 3 : 2);

	/* Look until done */
	while (look)
	{
		tries++;

		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			c_ptr = &cave[y][x];

			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Require "empty" floor space or trees */
			if (!teleportable_grid(c_ptr)) continue;

			/* No teleporting into vaults and such */
			if (cave[y][x].info & CAVE_ICKY) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;

		/* Stop after MAX_TRIES tries */
		if (tries > MAX_TRIES) return;
	}

	/* Sound */
	sound(SOUND_TELEPORT);

	/* Save the old location */
	oy = py;
	ox = px;

	/* Move the player */
	py = y;
	px = x;

	if (p_ptr->riding)
	{
		int tmp;
		tmp = cave[py][px].m_idx;
		cave[py][px].m_idx = cave[oy][ox].m_idx;
		cave[oy][ox].m_idx = tmp;
		m_list[p_ptr->riding].fy = py;
		m_list[p_ptr->riding].fx = px;
		update_mon(cave[py][px].m_idx, TRUE);
	}

	/* Redraw the old spot */
	lite_spot(oy, ox);

	/* Monsters with teleport ability may follow the player */
	while (xx < 2)
	{
		yy = -1;

		while (yy < 2)
		{
			if (xx == 0 && yy == 0)
			{
				/* Do nothing */
			}
			else
			{
				if (cave[oy+yy][ox+xx].m_idx)
				{
					if ((r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags6 & RF6_TPORT) &&
					    !(r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags3 & RF3_RES_TELE))
						/*
						 * The latter limitation is to avoid
						 * totally unkillable suckers...
						 */
					{
						if (!(m_list[cave[oy+yy][ox+xx].m_idx].csleep))
							teleport_to(cave[oy+yy][ox+xx].m_idx, py, px, 2, r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].level, TRUE);
					}
				}
			}
			yy++;
		}
		xx++;
	}

	forget_flow();

	/* Redraw the new spot */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	set_aquatic_in_water();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(int ny, int nx, bool no_tele, bool avoid_fall)
{
	int y, x, oy, ox, dis = 0, ctr = 0;
	cave_type *c_ptr;

	if (p_ptr->anti_tele && no_tele)
	{
#ifdef JP
msg_print("不思議な力がテレポートを防いだ！");
#else
		msg_print("A mysterious force prevents you from teleporting!");
#endif

		return;
	}

	/* Find a usable location */
	while (1)
	{
		/* Pick a nearby legal location */
		while (1)
		{
			y = rand_spread(ny, dis);
			x = rand_spread(nx, dis);
			if (in_bounds(y, x)) break;
		}

		c_ptr = &cave[y][x];

		/* Accept "naked" floor grids */
		if (no_tele && (y == py) && (x == px)) break;
		if (teleportable_grid(c_ptr)) break;
		if (!avoid_fall)
		{
			if ((c_ptr->feat == FEAT_AIR) && !c_ptr->m_idx) break;
		}

		/* Occasionally advance the distance */
		if (++ctr > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}
	}

	/* Sound */
	sound(SOUND_TELEPORT);

	/* Save the old location */
	oy = py;
	ox = px;

	/* Move the player */
	py = y;
	px = x;

	if (p_ptr->riding)
	{
		int tmp;
		tmp = cave[py][px].m_idx;
		cave[py][px].m_idx = cave[oy][ox].m_idx;
		cave[oy][ox].m_idx = tmp;
		m_list[p_ptr->riding].fy = py;
		m_list[p_ptr->riding].fx = px;
		update_mon(cave[py][px].m_idx, TRUE);
	}

	forget_flow();

	/* Redraw the old spot */
	lite_spot(oy, ox);

	/* Redraw the new spot */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	set_aquatic_in_water();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport the player or monster one level up or down (random when legal)
 * Note: m_idx == 0 is player.
 */
void teleport_level(int m_idx)
{
	bool go_up;
	int maxdepth = d_info[dungeon_type].maxdepth - ((dungeon_type == DUNGEON_HEAVEN_WAY) ? 1 : 0);

	monster_type *m_ptr;
	char         m_name[160];
	bool         see_m = TRUE;
	bool         do_delete = FALSE;
	int          old_dungeon_type = dungeon_type;

	/* No effect in arena or quest */
	if (astral_mode || p_ptr->inside_arena || (p_ptr->inside_quest && !random_quest_number(dun_level)) ||
	    (dungeon_type == DUNGEON_HEAVEN) ||
	    ((quest_number(dun_level) || (dun_level >= maxdepth)) && dun_level && (ironman_forward || (d_info[dungeon_type].flags1 & DF1_NO_BACK))))
	{
#ifdef JP
		msg_print("効果がなかった。");
#else
		msg_print("There is no effect.");
#endif

		return;
	}

	if (!m_idx)
	{
		if (p_ptr->anti_tele)
		{
#ifdef JP
			msg_print("不思議な力がテレポートを防いだ！");
#else
			msg_print("A mysterious force prevents you from teleporting!");
#endif

			return;
		}

		if (!dun_level) dungeon_type = p_ptr->recall_dungeon;

#ifdef JP
		strcpy(m_name, "あなた");
#else
		strcpy(m_name, "You");
#endif
	}
	else
	{
		m_ptr = &m_list[m_idx];

		/* Get the monster name (or "it") */
		monster_desc(m_name, m_ptr, 0x00);

		see_m = m_ptr->ml;

		if (!dun_level)
		{
			do
			{
				dungeon_type = randint0(max_d_idx);
			}
			while (!d_info[dungeon_type].name);
		}
	}

	/* Choose up or down */
	if (randint0(100) < 50) go_up = TRUE;
	else go_up = FALSE;

	if (!m_idx && p_ptr->wizard)
	{
		if (get_check("Force to go up? ")) go_up = TRUE;
		else if (get_check("Force to go down? ")) go_up = FALSE;
	}

	/* Forward only */ 
	if ((ironman_forward || (d_info[dungeon_type].flags1 & DF1_NO_BACK)) || (dun_level <= d_info[dungeon_type].mindepth) || !dun_level)
	{
		if (d_info[dungeon_type].flags1 & DF1_UPWARD)
		{
#ifdef JP
			if (see_m) msg_format("%^sは垂直に宙へ浮いていく。", m_name);
#else
			if (see_m) msg_format("%^s rise%s up through vertically.", m_name, m_idx ? "s" : "");
#endif
		}
		else
		{
#ifdef JP
			if (see_m) msg_format("%^sは垂直に沈んでいく。", m_name);
#else
			if (see_m) msg_format("%^s sink%s through vertically.", m_name, m_idx ? "s" : "");
#endif
		}

		if (!m_idx)
		{
			if (!dun_level)
			{
				p_ptr->oldpy = py;
				p_ptr->oldpx = px;
			}

			if (record_stair) do_cmd_write_nikki(NIKKI_TELE_LEV, 0, NULL);

			if (autosave_l) do_cmd_save_game(TRUE);

			if (!dun_level)
			{
				dun_level = d_info[dungeon_type].mindepth;
				prepare_change_floor_mode(CFM_RAND_PLACE | CFM_CLEAR_ALL);
			}
			else
			{
				if (d_info[dungeon_type].flags1 & DF1_UPWARD)
					prepare_change_floor_mode(CFM_UP | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
				else
					prepare_change_floor_mode(CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
			}

			/* Leaving */
			p_ptr->leaving = TRUE;
		}
		else do_delete = TRUE;
	}

	/* Backward only */ 
	else if (quest_number(dun_level) || (dun_level >= maxdepth))
	{
		if (d_info[dungeon_type].flags1 & DF1_UPWARD)
		{
#ifdef JP
			if (see_m) msg_format("%^sは垂直に沈んでいく。", m_name);
#else
			if (see_m) msg_format("%^s sink%s through vertically.", m_name, m_idx ? "s" : "");
#endif
		}
		else
		{
#ifdef JP
			if (see_m) msg_format("%^sは垂直に宙へ浮いていく。", m_name);
#else
			if (see_m) msg_format("%^s rise%s up through vertically.", m_name, m_idx ? "s" : "");
#endif
		}

		if (!m_idx)
		{
			if (record_stair) do_cmd_write_nikki(NIKKI_TELE_LEV, 0, NULL);

			if (autosave_l) do_cmd_save_game(TRUE);

			if (d_info[dungeon_type].flags1 & DF1_UPWARD)
				prepare_change_floor_mode(CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
			else
				prepare_change_floor_mode(CFM_UP | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);

			leave_quest_check();

			/* Leaving */
			p_ptr->inside_quest = 0;
			p_ptr->leaving = TRUE;
		}
		else do_delete = TRUE;
	}
	else if (go_up)
	{
#ifdef JP
		if (see_m) msg_format("%^sは垂直に宙へ浮いていく。", m_name);
#else
		if (see_m) msg_format("%^s rise%s up through vertically.", m_name, m_idx ? "s" : "");
#endif

		if (!m_idx)
		{
			if (record_stair) do_cmd_write_nikki(NIKKI_TELE_LEV, 0, NULL);

			if (autosave_l) do_cmd_save_game(TRUE);

			prepare_change_floor_mode(CFM_UP | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);

			/* Leaving */
			p_ptr->leaving = TRUE;
		}
		else do_delete = TRUE;
	}
	else
	{
#ifdef JP
		if (see_m) msg_format("%^sは垂直に沈んでいく。", m_name);
#else
		if (see_m) msg_format("%^s sink%s through vertically.", m_name, m_idx ? "s" : "");
#endif

		if (!m_idx)
		{
			/* Never reach this code on the surface */
			/* if (!dun_level) dungeon_type = 0; */

			if (record_stair) do_cmd_write_nikki(NIKKI_TELE_LEV, 0, NULL);

			if (autosave_l) do_cmd_save_game(TRUE);

			prepare_change_floor_mode(CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);

			/* Leaving */
			p_ptr->leaving = TRUE;
		}
		else do_delete = TRUE;
	}

	if (do_delete)
	{
		/* Check for quest completion */
		check_quest_completion(m_ptr);

		delete_monster_idx(m_idx);
	}

	if (m_idx && !dun_level) dungeon_type = old_dungeon_type;

	/* Sound */
	sound(SOUND_TPLEVEL);
}



static int choose_dungeon(cptr note)
{
	int select_dungeon;
	int i, num = 0;
	s16b *dun;

	/* Allocate the "dun" array */
	C_MAKE(dun, max_d_idx, s16b);

	screen_save();
	for(i = 1; i < max_d_idx; i++)
	{
		char buf[80];
		bool seiha = FALSE;

		if (d_info[i].flags1 & DF1_CLOSED) continue;
		if (!d_info[i].maxdepth) continue;
		if (!max_dlv[i]) continue;
		if (d_info[i].final_guardian)
		{
			if (!r_info[d_info[i].final_guardian].max_num) seiha = TRUE;
		}
		else if (max_dlv[i] >= d_info[i].maxdepth) seiha = TRUE;

#ifdef JP
		sprintf(buf,"      %c) %c%-12s : 最大 %d 階", 'a'+num, seiha ? '!' : ' ', d_name + d_info[i].name, max_dlv[i]);
#else
		sprintf(buf,"      %c) %c%-16s : Max level %d", 'a'+num, seiha ? '!' : ' ', d_name + d_info[i].name, max_dlv[i]);
#endif
		prt(buf, 2+num, 14);
		dun[num++] = i;
	}
#ifdef JP
	prt(format("どのダンジョン%sしますか:", note), 0, 0);
#else
	prt(format("Which dungeon do you %s?: ", note), 0, 0);
#endif
	while(1)
	{
		i = inkey();
		if (i == ESCAPE)
		{
			/* Free the "dun" array */
			C_KILL(dun, max_d_idx, s16b);

			screen_load();
			return 0;
		}
		if (i >= 'a' && i <('a'+num))
		{
			select_dungeon = dun[i-'a'];
			break;
		}
		else bell();
	}
	screen_load();

	/* Free the "dun" array */
	C_KILL(dun, max_d_idx, s16b);

	return select_dungeon;
}


/*
 * Recall the player to town or dungeon
 */
bool recall_player(int turns)
{
	/*
	 * TODO: Recall the player to the last
	 * visited town when in the wilderness
	 */

	/* Ironman option */
	if (p_ptr->inside_arena || ironman_forward || astral_mode || (d_info[dungeon_type].flags1 & DF1_CLOSED) ||
		(p_ptr->inside_quest && (quest[p_ptr->inside_quest].flags & QUEST_FLAG_NO_RECALL)))
	{
#ifdef JP
msg_print("何も起こらなかった。");
#else
		msg_print("Nothing happens.");
#endif

		return TRUE;
	}

	if (dun_level && (max_dlv[dungeon_type] > dun_level) && !p_ptr->inside_quest && !p_ptr->word_recall)
	{
#ifdef JP
if (get_check("ここは最深到達階より浅い階です。この階に戻って来ますか？ "))
#else
		if (get_check("Reset recall depth? "))
#endif
		{
			max_dlv[dungeon_type] = dun_level;
			if (record_maxdeapth)
#ifdef JP
				do_cmd_write_nikki(NIKKI_TRUMP, dungeon_type, "帰還のときに");
#else
				do_cmd_write_nikki(NIKKI_TRUMP, dungeon_type, "when recall from dungeon");
#endif
		}

	}
	if (!p_ptr->word_recall)
	{
		if (!dun_level)
		{
			int select_dungeon;
#ifdef JP
			select_dungeon = choose_dungeon("に帰還");
#else
			select_dungeon = choose_dungeon("recall");
#endif
			if (!select_dungeon) return FALSE;
			p_ptr->recall_dungeon = select_dungeon;
		}
		p_ptr->word_recall = turns;
#ifdef JP
msg_print("回りの大気が張りつめてきた...");
#else
		msg_print("The air about you becomes charged...");
#endif

		p_ptr->redraw |= (PR_STATUS);
	}
	else
	{
		p_ptr->word_recall = 0;
#ifdef JP
msg_print("張りつめた大気が流れ去った...");
#else
		msg_print("A tension leaves the air around you...");
#endif

		p_ptr->redraw |= (PR_STATUS);
	}
	return TRUE;
}


bool word_of_recall(void)
{
	return(recall_player(randint0(21) + 15));
}


bool reset_recall(void)
{
	int select_dungeon, dummy = 0;
	char ppp[80];
	char tmp_val[160];

	if (astral_mode)
	{
#ifdef JP
		msg_print("何も起こらなかった。");
#else
		msg_print("Nothing happens.");
#endif

		return TRUE;
	}

#ifdef JP
	select_dungeon = choose_dungeon("をセット");
#else
	select_dungeon = choose_dungeon("reset");
#endif

	if (!select_dungeon) return FALSE;
	/* Prompt */
#ifdef JP
sprintf(ppp, "何階にセットしますか (%d-%d):", d_info[select_dungeon].mindepth, max_dlv[select_dungeon]);
#else
	sprintf(ppp, "Reset to which level (%d-%d): ", d_info[select_dungeon].mindepth, max_dlv[select_dungeon]);
#endif


	/* Default */
	sprintf(tmp_val, "%d", MAX(dun_level, 1));

	/* Ask for a level */
	if (get_string(ppp, tmp_val, 10))
	{
		/* Extract request */
		dummy = atoi(tmp_val);

		/* Paranoia */
		if (dummy < 1) dummy = 1;

		/* Paranoia */
		if (dummy > max_dlv[select_dungeon]) dummy = max_dlv[select_dungeon];
		if (dummy < d_info[select_dungeon].mindepth) dummy = d_info[select_dungeon].mindepth;

		max_dlv[select_dungeon] = dummy;

		if (record_maxdeapth)
#ifdef JP
			do_cmd_write_nikki(NIKKI_TRUMP, select_dungeon, "フロア・リセットで");
#else
			do_cmd_write_nikki(NIKKI_TRUMP, select_dungeon, "using a scroll of reset recall");
#endif
					/* Accept request */
#ifdef JP
msg_format("%sの帰還レベルを %d 階にセット。", d_name+d_info[select_dungeon].name, dummy, dummy * 50);
#else
		msg_format("Recall depth set to level %d (%d').", dummy, dummy * 50);
#endif

	}
	else
	{
		return FALSE;
	}
	return TRUE;
}


/*
 * Apply disenchantment to the player's stuff
 *
 * XXX XXX XXX This function is also called from the "melee" code
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(void)
{
	int             t = 0;
	object_type     *o_ptr;
	char            o_name[MAX_NLEN];
	int to_h, to_d, to_a, to_stat[A_MAX], to_misc[OB_MAX];
	bool            flag = FALSE;
	int             i;

	/* Pick a random slot */
	if (prace_is_(RACE_OCTOPUS))
	{
		switch (randint1(2))
		{
			case 1: t = INVEN_OUTER; break;
			case 2: t = INVEN_HEAD; break;
		}
	}
	else
	{
		switch (randint1(8))
		{
			case 1: t = INVEN_RARM; break;
			case 2: t = INVEN_LARM; break;
			case 3: t = INVEN_BOW; break;
			case 4: t = INVEN_BODY; break;
			case 5: t = INVEN_OUTER; break;
			case 6: t = INVEN_HEAD; break;
			case 7: t = INVEN_HANDS; break;
			case 8: t = INVEN_FEET; break;
		}
	}

	/* Get the item */
	o_ptr = &inventory[t];

	/* No item, nothing happens */
	if (!o_ptr->k_idx) return (FALSE);


	for (i = 0; (i < A_MAX) && !flag; i++)
	{
		if (o_ptr->to_stat[i] > 1) flag = TRUE;
	}
	for (i = 0; (i < OB_MAX) && !flag; i++)
	{
		if (o_ptr->to_misc[i] > 1) flag = TRUE;
	}

	/* Nothing to disenchant */
	if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0) && !flag)
	{
		/* Nothing to notice */
		return (FALSE);
	}


	/* Describe the object */
	object_desc(o_name, o_ptr, FALSE, 0);


	/* Artifacts have 71% chance to resist */
	if (((artifact_p(o_ptr) || o_ptr->art_name) && (randint0(100) < 71)) ||
	    object_is_astral_runeweapon(o_ptr))
	{
		/* Message */
#ifdef JP
		msg_format("%s(%c)は劣化を跳ね返した！",o_name, index_to_label(t, TRUE));
#else
		msg_format("Your %s (%c) resist%s disenchantment!",
			   o_name, index_to_label(t, TRUE),
			   ((o_ptr->number != 1) ? "" : "s"));
#endif


		/* Notice */
		return (TRUE);
	}


	if ((t == INVEN_RARM) && mw_diff_to_melee)
	{
		o_ptr->to_h -= mw_diff_to_melee;
		o_ptr->to_d -= mw_diff_to_melee;
	}

	/* Memorize old value */
	for (i = 0; i < A_MAX; i++) to_stat[i] = o_ptr->to_stat[i];
	for (i = 0; i < OB_MAX; i++) to_misc[i] = o_ptr->to_misc[i];
	to_h = o_ptr->to_h;
	to_d = o_ptr->to_d;
	to_a = o_ptr->to_a;

	/* Disenchant tohit */
	if (o_ptr->to_h > 0) o_ptr->to_h--;
	if ((o_ptr->to_h > 5) && (randint0(100) < 20)) o_ptr->to_h--;

	/* Disenchant todam */
	if (o_ptr->to_d > 0) o_ptr->to_d--;
	if ((o_ptr->to_d > 5) && (randint0(100) < 20)) o_ptr->to_d--;

	/* Disenchant toac */
	if (o_ptr->to_a > 0) o_ptr->to_a--;
	if ((o_ptr->to_a > 5) && (randint0(100) < 20)) o_ptr->to_a--;

	/* Disenchant to-bonus (occasionally) */
	for (i = 0; i < A_MAX; i++)
	{
		if ((o_ptr->to_stat[i] > 1) && one_in_(13)) o_ptr->to_stat[i]--;
	}
	for (i = 0; i < OB_MAX; i++)
	{
		if ((o_ptr->to_misc[i] > 1) && one_in_(13)) o_ptr->to_misc[i]--;
	}

	if ((t == INVEN_RARM) && mw_diff_to_melee)
	{
		o_ptr->to_h += mw_diff_to_melee;
		o_ptr->to_d += mw_diff_to_melee;
	}

	flag = FALSE;
	for (i = 0; (i < A_MAX) && !flag; i++)
	{
		if (to_stat[i] != o_ptr->to_stat[i]) flag = TRUE;
	}
	for (i = 0; (i < OB_MAX) && !flag; i++)
	{
		if (to_misc[i] != o_ptr->to_misc[i]) flag = TRUE;
	}

	if ((to_h != o_ptr->to_h) || (to_d != o_ptr->to_d) ||
	    (to_a != o_ptr->to_a) || flag)
	{
		/* Message */
#ifdef JP
		msg_format("%s(%c)は劣化してしまった！",
			   o_name, index_to_label(t, TRUE) );
#else
		msg_format("Your %s (%c) %s disenchanted!",
			   o_name, index_to_label(t, TRUE),
			   ((o_ptr->number != 1) ? "were" : "was"));
#endif

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP | PW_PLAYER);

		/* Notice */
		return TRUE;
	}

	/* Nothing to notice */
	return FALSE;
}


void mutate_player(void)
{
	int max1, cur1, max2, cur2, ii, jj, i;

	/* Pick a pair of stats */
	ii = randint0(A_MAX);
	for (jj = ii; jj == ii; jj = randint0(A_MAX)) /* loop */;

	max1 = p_ptr->stat_max[ii];
	cur1 = p_ptr->stat_cur[ii];
	max2 = p_ptr->stat_max[jj];
	cur2 = p_ptr->stat_cur[jj];

	p_ptr->stat_max[ii] = max2;
	p_ptr->stat_cur[ii] = cur2;
	p_ptr->stat_max[jj] = max1;
	p_ptr->stat_cur[jj] = cur1;

	p_ptr->update |= (PU_BONUS);
}


static bool item_tester_hook_weapon_nobow(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Brand the current weapon
 */
void brand_weapon(int brand_type)
{
	int         item;
	object_type *o_ptr;
	cptr        q, s;


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon_nobow;
	item_tester_no_ryoute = TRUE;

	/* Get an item */
#ifdef JP
q = "どの武器を強化しますか? ";
s = "強化できる武器がない。";
#else
	q = "Enchant which weapon? ";
	s = "You have nothing to enchant.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* you can never modify artifacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if (o_ptr->k_idx && !artifact_p(o_ptr) && !ego_item_p(o_ptr) &&
	    !o_ptr->art_name && !cursed_p(o_ptr) &&
	    !((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DIAMOND_EDGE)))
	{
		cptr act = NULL;

		/* Let's get the name before it is changed... */
		char o_name[MAX_NLEN];
		object_desc(o_name, o_ptr, FALSE, 0);

		switch (brand_type)
		{
		case 17:
			if (o_ptr->tval == TV_SWORD)
			{
#ifdef JP
act = "は鋭さを増した！";
#else
				act = "becomes very sharp!";
#endif

				o_ptr->name2 = EGO_SHARPNESS;
				o_ptr->to_misc[OB_TUNNEL] = m_bonus(5, dun_level) + 1;
			}
			else
			{
#ifdef JP
act = "は破壊力を増した！";
#else
				act = "seems very powerful.";
#endif

				o_ptr->name2 = EGO_EARTHQUAKES;
				o_ptr->to_misc[OB_TUNNEL] = m_bonus(3, dun_level);
			}
			break;
		case 16:
#ifdef JP
act = "は人間の血を求めている！";
#else
			act = "seems to be looking for humans!";
#endif

			o_ptr->name2 = EGO_SLAY_HUMAN;
			break;
		case 15:
#ifdef JP
act = "は電撃に覆われた！";
#else
			act = "covered with lightning!";
#endif

			o_ptr->name2 = EGO_BRAND_ELEC;
			break;
		case 14:
#ifdef JP
act = "は酸に覆われた！";
#else
			act = "coated with acid!";
#endif

			o_ptr->name2 = EGO_BRAND_ACID;
			break;
		case 13:
#ifdef JP
act = "は邪悪なる怪物を求めている！";
#else
			act = "seems to be looking for evil monsters!";
#endif

			o_ptr->name2 = EGO_SLAY_EVIL;
			break;
		case 12:
#ifdef JP
act = "は異世界の住人の肉体を求めている！";
#else
			act = "seems to be looking for demons!";
#endif

			o_ptr->name2 = EGO_SLAY_DEMON;
			break;
		case 11:
#ifdef JP
act = "は屍を求めている！";
#else
			act = "seems to be looking for undead!";
#endif

			o_ptr->name2 = EGO_SLAY_UNDEAD;
			break;
		case 10:
#ifdef JP
act = "は動物の血を求めている！";
#else
			act = "seems to be looking for animals!";
#endif

			o_ptr->name2 = EGO_SLAY_ANIMAL;
			break;
		case 9:
#ifdef JP
act = "はドラゴンの血を求めている！";
#else
			act = "seems to be looking for dragons!";
#endif

			o_ptr->name2 = EGO_SLAY_DRAGON;
			break;
		case 8:
#ifdef JP
act = "はトロルの血を求めている！";
#else
			act = "seems to be looking for trolls!";
#endif

			o_ptr->name2 = EGO_SLAY_TROLL;
			break;
		case 7:
#ifdef JP
act = "はオークの血を求めている！";
#else
			act = "seems to be looking for orcs!";
#endif

			o_ptr->name2 = EGO_SLAY_ORC;
			break;
		case 6:
#ifdef JP
act = "は巨人の血を求めている！";
#else
			act = "seems to be looking for giants!";
#endif

			o_ptr->name2 = EGO_SLAY_GIANT;
			break;
		case 5:
#ifdef JP
act = "は非常に邪悪になったようだ。";
#else
			act = "seems very evil now.";
#endif

			o_ptr->name2 = EGO_ASMODE;
			o_ptr->to_misc[OB_SEARCH] = randint1(2);
			break;
		case 4:
#ifdef JP
act = "は血を求めている！";
#else
			act = "thirsts for blood!";
#endif

			o_ptr->name2 = EGO_VAMPIRIC;
			break;
		case 3:
#ifdef JP
act = "は毒に覆われた。";
#else
			act = "is coated with poison.";
#endif

			o_ptr->name2 = EGO_BRAND_POIS;
			break;
		case 2:
#ifdef JP
act = "は純粋なカオスに飲み込まれた。";
#else
			act = "is engulfed in raw chaos!";
#endif

			o_ptr->name2 = EGO_CHAOTIC;
			break;
		case 1:
#ifdef JP
act = "は炎のシールドに覆われた！";
#else
			act = "is covered in a fiery shield!";
#endif

			o_ptr->name2 = EGO_BRAND_FIRE;
			break;
		default:
#ifdef JP
act = "は深く冷たいブルーに輝いた！";
#else
			act = "glows deep, icy blue!";
#endif

			o_ptr->name2 = EGO_BRAND_COLD;
			break;
		}

#ifdef JP
msg_format("あなたの%s%s", o_name, act);
#else
		msg_format("Your %s %s", o_name, act);
#endif


		if ((item == INVEN_RARM) && mw_diff_to_melee)
		{
			o_ptr->to_h -= mw_diff_to_melee;
			o_ptr->to_d -= mw_diff_to_melee;
		}
		enchant(o_ptr, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);
		if ((item == INVEN_RARM) && mw_diff_to_melee)
		{
			o_ptr->to_h += mw_diff_to_melee;
			o_ptr->to_d += mw_diff_to_melee;
		}

		o_ptr->discount = 99;
	}
	else
	{
		if (flush_failure) flush();

#ifdef JP
msg_print("属性付加に失敗した。");
#else
		msg_print("The Branding failed.");
#endif
	}
}


void call_the_(void)
{
	int i;
	int y, x;
	bool do_call = TRUE;

	for (i = 0; i < 9; i++)
	{
		y = py + ddy_ddd[i];
		x = px + ddx_ddd[i];

		if (!cave_floor_bold(y, x) && !boundary_floor_bold(y, x))
		{
			do_call = FALSE;
			break;
		}
	}

	if (do_call)
	{
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_ROCKET, i, 175, 2, FALSE);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_MANA, i, 175, 3, FALSE);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_NUKE, i, 175, 4, FALSE);
		}
	}
	else
	{
#ifdef JP
		msg_print("あなたは壁に近すぎる場所で使ってしまった！");
		msg_print("大きな爆発音があった！");
#else
		msg_print("You use too close to a wall!");
		msg_print("There is a loud explosion!");
#endif


		if (destroy_area(py, px, 15 + p_ptr->lev + randint0(11)))
#ifdef JP
msg_print("ダンジョンが崩壊した...");
#else
			msg_print("The dungeon collapses...");
#endif

		else
#ifdef JP
msg_print("ダンジョンは大きく揺れた。");
#else
			msg_print("The dungeon trembles.");
#endif


#ifdef JP
take_hit(DAMAGE_NOESCAPE, 100 + randint1(150), "自殺的な虚無招来");
#else
		take_hit(DAMAGE_NOESCAPE, 100 + randint1(150), "a suicidal Call the Void");
#endif

	}
}


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
	int             ty, tx, i;
	cave_type       *c_ptr;
	object_type     *o_ptr;
	char            o_name[MAX_NLEN];

	/* Check to see if an object is already there */
	if (cave[py][px].o_idx)
	{
#ifdef JP
msg_print("自分の足の下にある物は取れません。");
#else
		msg_print("You can't fetch when you're already standing on something.");
#endif

		return;
	}

	/* Use a target */
	if (dir == 5 && target_okay())
	{
		tx = target_col;
		ty = target_row;

		if (distance(py, px, ty, tx) > MAX_RANGE)
		{
#ifdef JP
msg_print("そんなに遠くにある物は取れません！");
#else
			msg_print("You can't fetch something that far away!");
#endif

			return;
		}

		c_ptr = &cave[ty][tx];

		/* We need an item to fetch */
		if (!c_ptr->o_idx)
		{
#ifdef JP
msg_print("そこには何もありません。");
#else
			msg_print("There is no object at this place.");
#endif

			return;
		}

		/* No fetching from vault */
		if (c_ptr->info & CAVE_ICKY)
		{
#ifdef JP
msg_print("アイテムがコントロールを外れて落ちた。");
#else
			msg_print("The item slips from your control.");
#endif

			return;
		}

		/* We need to see the item */
		if (require_los && !player_has_los_bold(ty, tx))
		{
#ifdef JP
msg_print("そこはあなたの視界に入っていません。");
#else
			msg_print("You have no direct line of sight to that location.");
#endif

			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py; /* Where to drop the item */
		tx = px;

		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];

			if ((distance(py, px, ty, tx) > MAX_RANGE) ||
			    !cave_floor_bold(ty, tx)) return;
		}
		while (!c_ptr->o_idx);
	}

	o_ptr = &o_list[c_ptr->o_idx];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
#ifdef JP
msg_print("そのアイテムは重過ぎます。");
#else
		msg_print("The object is too heavy.");
#endif

		return;
	}

	i = c_ptr->o_idx;
	c_ptr->o_idx = o_ptr->next_o_idx;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->next_o_idx = 0;
	o_ptr->iy = (byte)py;
	o_ptr->ix = (byte)px;

	object_desc(o_name, o_ptr, TRUE, 0);
#ifdef JP
msg_format("%^sがあなたの足元に飛んできた。", o_name);
#else
	msg_format("%^s flies through the air to your feet.", o_name);
#endif


	note_spot(py, px);
	p_ptr->redraw |= PR_MAP;
}


void alter_reality(void)
{
	/* Ironman option */
	if (p_ptr->inside_arena || ironman_forward)
	{
#ifdef JP
		msg_print("何も起こらなかった。");
#else
		msg_print("Nothing happens.");
#endif
		return;
	}

	if (!p_ptr->alter_reality)
	{
		int turns = randint0(21) + 15;

		p_ptr->alter_reality = turns;
#ifdef JP
		msg_print("回りの景色が変わり始めた...");
#else
		msg_print("The view around you begins to change...");
#endif

		p_ptr->redraw |= (PR_STATUS);
	}
	else
	{
		p_ptr->alter_reality = 0;
#ifdef JP
		msg_print("景色が元に戻った...");
#else
		msg_print("The view around you got back...");
#endif

		p_ptr->redraw |= (PR_STATUS);
	}
	return;
}


bool alter_with_flood(void)
{
	/* Ironman option */
	if (p_ptr->inside_arena || ironman_forward)
	{
#ifdef JP
		msg_print("何も起こらなかった。");
#else
		msg_print("Nothing happens.");
#endif
		return TRUE;
	}

	if (!quest_number(dun_level) && dun_level)
	{
		if (p_ptr->inhibit_flood)
		{
			msg_print("まだ大洪水を使用できない。");
			return FALSE;
		}
		else
		{
			msg_print("洪水が全てを流し尽くす！");

			prepare_change_floor_mode(CFM_CLEAR_ALL);

			p_ptr->alter_reality_water_flow = TRUE;
			p_ptr->alter_reality = 0;
			p_ptr->inhibit_flood = 100;
			p_ptr->redraw |= (PR_STATUS);

			/* Leaving */
			p_ptr->leaving = TRUE;
		}
	}
	else msg_print("洪水は起きなかった。");

	return TRUE;
}


bool alter_to_fate(void)
{
	saved_floor_type *sf_ptr = get_sf_ptr(p_ptr->floor_id);

	/* Ironman option */
	if (p_ptr->inside_arena || ironman_forward || quest_number(dun_level))
	{
#ifdef JP
		msg_print("何も起こらなかった。");
#else
		msg_print("Nothing happens.");
#endif
		return TRUE;
	}

	if (!dun_level)
	{
		msg_print("地上では使用できない。");
		return FALSE;
	}

	if (dun_level < 50)
	{
		msg_print("50階未満では使用できない。");
		return FALSE;
	}

	if (d_info[dungeon_type].flags1 & DF1_BEGINNER)
	{
		msg_print("このダンジョンでは運命を作り出せない。");
		return FALSE;
	}

	if (sf_ptr->flags & SFF_ENTER_DUNGEON)
	{
		msg_print("地上の入り口から入った直後の階では使用できない。");
		return FALSE;
	}

	if (sf_ptr->flags & SFF_CREATED_BY_FATE)
	{
		msg_print("この階は既に運命で作られた階だ。");
		return FALSE;
	}

	msg_print("運命を作り出す！");

	prepare_change_floor_mode(CFM_CLEAR_ALL);

	p_ptr->alter_reality_to_fate = TRUE;
	p_ptr->alter_reality = 0;
	p_ptr->redraw |= (PR_STATUS);

	/* Leaving */
	p_ptr->leaving = TRUE;

	return TRUE;
}


bool can_create_glyph_bold(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	switch (c_ptr->feat)
	{
	case FEAT_FLOOR:
	case FEAT_SHAL_WATER:
	case FEAT_SHAL_LAVA:
	case FEAT_GRASS:
	case FEAT_DEEP_GRASS:
	case FEAT_FLOWER:
	case FEAT_DIRT:
	case FEAT_SWAMP:
	case FEAT_TUNDRA:
		break;

	default:
		return FALSE;
	}

	if (c_ptr->info & CAVE_OBJECT) return FALSE;

	if (c_ptr->m_idx > 0)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];
		if (!is_pet(m_ptr)) return FALSE;
	}

	return TRUE;
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
bool warding_glyph(void)
{
	/* XXX XXX XXX */
	if (!can_create_glyph_bold(py, px))
	{
#ifdef JP
		msg_print("地形が呪文を跳ね返した。");
#else
		msg_print("The feature resists the spell.");
#endif

		return FALSE;
	}

	/* Create a glyph */
	cave[py][px].info |= CAVE_OBJECT;
	cave[py][px].mimic = FEAT_GLYPH;

	/* Notice */
	note_spot(py, px);
	
	/* Redraw */
	lite_spot(py, px);

	return TRUE;
}


/*
 * Leave an "explosive rune" which prevents monster movement
 */
bool explosive_rune(void)
{
	/* XXX XXX XXX */
	if (!can_create_glyph_bold(py, px))
	{
#ifdef JP
		msg_print("地形が呪文を跳ね返した。");
#else
		msg_print("The feature resists the spell.");
#endif

		return FALSE;
	}

	/* Create a glyph */
	cave[py][px].info |= CAVE_OBJECT;
	cave[py][px].mimic = FEAT_MINOR_GLYPH;

	/* Notice */
	note_spot(py, px);
	
	/* Redraw */
	lite_spot(py, px);

	return TRUE;
}


/*
 * Identify everything being carried.
 */
void identify_pack(bool mental)
{
	int i;
	bool old_known;
	int idx;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Identify it */
		old_known = identify_item(o_ptr);

		/* Mark the item as fully known */
		if (mental) o_ptr->ident |= (IDENT_MENTAL);

		/* Auto-inscription/destroy */
		idx = is_autopick(o_ptr);
		auto_inscribe_item(i, idx);
		if (i <= INVEN_PACK)
		{
			if (destroy_identify && !old_known)
				auto_destroy_item(i, idx);
		}
	}
}


/*
 * Used by the "enchant" function (chance of failure)
 * (modified for Zangband, we need better stuff there...) -- TY
 */
static int enchant_table[16] =
{
	0, 10,  50, 100, 200,
	300, 400, 500, 650, 800,
	950, 987, 993, 995, 998,
	1000
};


/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Heavily Cursed Items need a special spell */
		if (!all && (o_ptr->curse_flags & TRC_HEAVY_CURSE)) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (o_ptr->curse_flags & TRC_PERMA_CURSE)
		{
			/* Uncurse it */
			o_ptr->curse_flags &= (TRC_CURSED | TRC_HEAVY_CURSE | TRC_PERMA_CURSE);
			continue;
		}

		/* Uncurse it */
		o_ptr->curse_flags = 0L;

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Take note */
		o_ptr->feeling = FEEL_NONE;

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
}


/*
 * Turns an object into gold, gain some of its value in a shop
 */
bool alchemy(void)
{
	int item, amt = 1;
	int old_number;
	long price;
	bool force = FALSE;
	object_type *o_ptr;
	char o_name[MAX_NLEN];
	char out_val[MAX_NLEN+40];

	cptr q, s;

	/* Hack -- force destruction */
	if (command_arg > 0) force = TRUE;

	/* Get an item */
#ifdef JP
q = "どのアイテムを金に変えますか？";
s = "金に変えられる物がありません。";
#else
	q = "Turn which item to gold? ";
	s = "You have nothing to turn to gold.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return FALSE;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (confirm_destroy || (object_value(o_ptr) > 0))
		{
			/* Make a verification */
#ifdef JP
sprintf(out_val, "本当に%sを金に変えますか？", o_name);
#else
			sprintf(out_val, "Really turn %s to gold? ", o_name);
#endif

			if (!get_check(out_val)) return FALSE;
		}
	}

	/* Artifacts cannot be destroyed */
	if (!can_player_destroy_object(o_ptr))
	{
		/* Message */
#ifdef JP
		msg_format("%sを金に変えることに失敗した。", o_name);
#else
		msg_format("You fail to turn %s to gold!", o_name);
#endif

		/* Done */
		return FALSE;
	}

	price = object_value_real(o_ptr);

	if (price <= 0)
	{
		/* Message */
#ifdef JP
msg_format("%sをニセの金に変えた。", o_name);
#else
		msg_format("You turn %s to fool's gold.", o_name);
#endif

	}
	else
	{
		price /= 3;

		if (amt > 1) price *= amt;

		if (price > 30000) price = 30000;
#ifdef JP
msg_format("%sを＄%d の金に変えた。", o_name, price);
#else
		msg_format("You turn %s to %ld coins worth of gold.", o_name, price);
#endif

		p_ptr->au[SV_GOLD_GOLD_3] += price;

		/* Update gold */
		p_ptr->update |= (PU_GOLD);

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}



/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_ROCKET:
		case TV_SHELL:
		case TV_ROUND:
		case TV_BULLET:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

static bool item_tester_hook_weapon2(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_ROCKET:
		case TV_SHELL:
		case TV_ROUND:
		case TV_BULLET:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
bool item_tester_hook_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


bool item_tester_hook_corpse(object_type *o_ptr)
{
	if ((o_ptr->tval == TV_CORPSE) && (o_ptr->sval == SV_CORPSE)) return TRUE;

	/* Assume not */
	return FALSE;
}


/*
 * Check if an object is weapon or armour (but not arrow, bolt, or shot)
 */
bool item_tester_hook_weapon_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_ROCKET:
		case TV_SHELL:
		case TV_ROUND:
		case TV_BULLET:
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Check if an object is nameless weapon or armour
 */
static bool item_tester_hook_nameless_weapon_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_ROCKET:
		case TV_SHELL:
		case TV_ROUND:
		case TV_BULLET:
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
			if (object_known_p(o_ptr) && (o_ptr->name1 || o_ptr->art_name || o_ptr->name2 || o_ptr->xtra3))
				return FALSE;
			else return TRUE;
	}

	return FALSE;
}


/*
 * Break the curse of an item
 */
static void break_curse(object_type *o_ptr)
{
	if (cursed_p(o_ptr) && !(o_ptr->curse_flags & TRC_PERMA_CURSE) && !(o_ptr->curse_flags & TRC_HEAVY_CURSE) && (randint0(100) < 25))
	{
#ifdef JP
msg_print("かけられていた呪いが打ち破られた！");
#else
		msg_print("The curse is broken!");
#endif

		o_ptr->curse_flags = 0L;

		o_ptr->ident |= (IDENT_SENSE);

		o_ptr->feeling = FEEL_NONE;
	}
}


/*
 * Enchants a plus onto an item. -RAK-
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item. -CFT-
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int     i, chance, prob;
	bool    res = FALSE;
	bool    a = (artifact_p(o_ptr) || o_ptr->art_name);
	bool    force = (eflag & ENCH_FORCE);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval >= TV_BULLET) && (o_ptr->tval <= TV_BOLT))
	{
		prob = prob / 20;
	}

	/* Try "n" times */
	for (i = 0; i < n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if (!force && randint0(prob) >= 100) continue;

		/* Enchant to hit */
		if (eflag & ENCH_TOHIT)
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];

			if (force || ((randint1(1000) > chance) && (!a || (randint0(100) < 50))))
			{
				o_ptr->to_h++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_h >= 0)
					break_curse(o_ptr);
			}
		}

		/* Enchant to damage */
		if (eflag & ENCH_TODAM)
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			if (force || ((randint1(1000) > chance) && (!a || (randint0(100) < 50))))
			{
				o_ptr->to_d++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_d >= 0)
					break_curse(o_ptr);
			}
		}

		/* Enchant to armor class */
		if (eflag & ENCH_TOAC)
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			if (force || ((randint1(1000) > chance) && (!a || (randint0(100) < 50))))
			{
				o_ptr->to_a++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_a >= 0)
					break_curse(o_ptr);
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Success */
	return (TRUE);
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	int         item;
	bool        okay = FALSE;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];
	cptr        q, s;


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;
	item_tester_no_ryoute = TRUE;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
#ifdef JP
q = "どのアイテムを強化しますか? ";
s = "強化できるアイテムがない。";
#else
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	if ((item == INVEN_RARM) && mw_diff_to_melee)
	{
		o_ptr->to_h -= mw_diff_to_melee;
		o_ptr->to_d -= mw_diff_to_melee;
	}

	/* Describe */
#ifdef JP
msg_format("%s は明るく輝いた！",
    o_name);
#else
	msg_format("%s %s glow%s brightly!",
		   ((item >= 0) ? "Your" : "The"), o_name,
		   ((o_ptr->number > 1) ? "" : "s"));
#endif


	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
#ifdef JP
msg_print("強化に失敗した。");
#else
		msg_print("The enchantment failed.");
#endif
	}

	if ((item == INVEN_RARM) && mw_diff_to_melee)
	{
		o_ptr->to_h += mw_diff_to_melee;
		o_ptr->to_d += mw_diff_to_melee;
	}

	/* Something happened */
	return (TRUE);
}


bool artifact_scroll(void)
{
	int             item;
	bool            okay = FALSE;
	object_type     *o_ptr;
	char            o_name[MAX_NLEN];
	cptr            q, s;


	item_tester_no_ryoute = TRUE;
	/* Enchant weapon/armour */
	item_tester_hook = item_tester_hook_nameless_weapon_armour;

	/* Get an item */
#ifdef JP
q = "どのアイテムを強化しますか? ";
s = "強化できるアイテムがない。";
#else
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Describe */
#ifdef JP
	msg_format("%s は眩い光を発した！", o_name);
#else
	msg_format("%s %s radiate%s a blinding light!",
	          ((item >= 0) ? "Your" : "The"), o_name,
	          ((o_ptr->number > 1) ? "" : "s"));
#endif

	if (o_ptr->name1 || o_ptr->art_name)
	{
#ifdef JP
		msg_format("%sは既に伝説のアイテムです！", o_name);
#else
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "artifacts" : "an artifact"));
#endif

		okay = FALSE;
	}

	else if (o_ptr->name2)
	{
#ifdef JP
		msg_format("%sは既に名のあるアイテムです！", o_name);
#else
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "ego items" : "an ego item"));
#endif

		okay = FALSE;
	}

	else if (o_ptr->xtra3)
	{
#ifdef JP
		msg_format("%sは既に強化されています！", o_name);
#else
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "gunner items" : "an gunner item"));
#endif
	}

	else
	{
		int idx;

		if (o_ptr->number > 1)
		{
#ifdef JP
			msg_print("複数のアイテムに魔法をかけるだけのエネルギーはありません！");
			msg_format("%d 個の%sが壊れた！",(o_ptr->number)-1, o_name);
#else
			msg_print("Not enough enough energy to enchant more than one object!");
			msg_format("%d of your %s %s destroyed!",(o_ptr->number)-1, o_name, (o_ptr->number>2?"were":"was"));
#endif

			if (item >= 0)
			{
				inven_item_increase(item, 1-(o_ptr->number));
			}
			else
			{
				floor_item_increase(0-item, 1-(o_ptr->number));
			}
		}
		if (item == INVEN_RARM)
		{
			if (mw_old_weight)
			{
				o_ptr->weight = mw_old_weight;
			}
			if (mw_diff_to_melee)
			{
				o_ptr->to_h -= mw_diff_to_melee;
				o_ptr->to_d -= mw_diff_to_melee;
			}
		}
		okay = create_artifact(o_ptr, TRUE);
		if (item == INVEN_RARM)
		{
			if (mw_old_weight)
			{
				mw_old_weight = o_ptr->weight;
				o_ptr->weight = 1;
			}
			if (mw_diff_to_melee)
			{
				o_ptr->to_h += mw_diff_to_melee;
				o_ptr->to_d += mw_diff_to_melee;
			}
		}

		/* Auto-inscription */
		idx = is_autopick(o_ptr);
		auto_inscribe_item(item, idx);
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
#ifdef JP
		msg_print("強化に失敗した。");
#else
		msg_print("The enchantment failed.");
#endif
	}

	/* Something happened */
	return TRUE;
}


/*
 * Identify an object
 */
bool identify_item(object_type *o_ptr)
{
	bool old_known = FALSE;
	char o_name[MAX_NLEN];

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	if (o_ptr->ident & IDENT_KNOWN)
		old_known = TRUE;

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	strcpy(record_o_name, o_name);
	record_turn = turn;

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 0);

	if(record_fix_art && !old_known && artifact_p(o_ptr))
		do_cmd_write_nikki(NIKKI_ART, 0, o_name);
	if(record_rand_art && !old_known && o_ptr->art_name)
		do_cmd_write_nikki(NIKKI_ART, 0, o_name);

	return old_known;
}


static bool item_tester_hook_identify(object_type *o_ptr)
{
	return (bool)!object_known_p(o_ptr);
}

static bool item_tester_hook_identify_weapon_armour(object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	return item_tester_hook_weapon_armour(o_ptr);
}

/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(bool only_equip)
{
	int             item;
	object_type     *o_ptr;
	char            o_name[MAX_NLEN];
	cptr            q, s;
	bool old_known;
	int idx;

	item_tester_no_ryoute = TRUE;

	if (only_equip)
		item_tester_hook = item_tester_hook_identify_weapon_armour;
	else
		item_tester_hook = item_tester_hook_identify;

	if (!can_get_item())
	{
		if (only_equip)
		{
			item_tester_hook = item_tester_hook_weapon_armour;
		}
		else
		{
			item_tester_hook = NULL;
		}
	}

	/* Get an item */
#ifdef JP
q = "どのアイテムを鑑定しますか? ";
s = "鑑定するべきアイテムがない。";
#else
	q = "Identify which item? ";
	s = "You have nothing to identify.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Identify it */
	old_known = identify_item(o_ptr);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_RARM)
	{
#ifdef JP
		msg_format("%^s: %s(%c)。", describe_use(item), o_name, index_to_label(item, TRUE));
#else
		msg_format("%^s: %s (%c).", describe_use(item), o_name, index_to_label(item, TRUE));
#endif
	}
	else if (item >= 0)
	{
#ifdef JP
		msg_format("ザック中: %s(%c)。", o_name, index_to_label(item, FALSE));
#else
		msg_format("In your pack: %s (%c).", o_name, index_to_label(item, FALSE));
#endif
	}
	else
	{
#ifdef JP
		msg_format("床上: %s。", o_name);
#else
		msg_format("On the ground: %s.", o_name);
#endif
	}

	/* Auto-inscription/destroy */
	idx = is_autopick(o_ptr);
	auto_inscribe_item(item, idx);
	if (destroy_identify && !old_known)
		auto_destroy_item(item, idx);

	/* Something happened */
	return (TRUE);
}


/*
 * Mundanify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was mundanified, else FALSE.
 */
bool mundane_spell(bool only_equip)
{
	int             item;
	object_type     *o_ptr;
	cptr            q, s;

	if (only_equip) item_tester_hook = item_tester_hook_weapon_armour;
	item_tester_no_ryoute = TRUE;

	/* Get an item */
#ifdef JP
q = "どれを使いますか？";
s = "使えるものがありません。";
#else
	q = "Use which item? ";
	s = "You have nothing you can use.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	if (object_is_astral_runeweapon(o_ptr) ||
	    ((dungeon_type == DUNGEON_HEAVEN) && (o_ptr->name1 == ART_BRUNHILD)))
	{
		msg_print("無力化の光が跳ね返された！");

		return TRUE;
	}

	/* Player broke the "Runeweapon" !! */
	if (object_is_snapdragon_runeweapon(o_ptr))
	{
		runeweapon_type *runeweapon = &runeweapon_list[o_ptr->xtra3];
		if (!(runeweapon->status & RW_STATUS_FOUND)) runeweapon->status |= (RW_STATUS_FOUND);
	}

	/* Oops */
#ifdef JP
	msg_print("まばゆい閃光が走った！");
#else
	msg_print("There is a bright flash of light!");
#endif
	{
		byte iy = o_ptr->iy;                /* Y-position on map, or zero */
		byte ix = o_ptr->ix;                /* X-position on map, or zero */
		s16b next_o_idx = o_ptr->next_o_idx; /* Next object in stack (if any) */
		byte marked = o_ptr->marked;          /* Object is marked */
		s16b weight = (o_ptr->number * o_ptr->weight);
		u16b inscription = o_ptr->inscription;

		/* Wipe it clean */
		object_prep(o_ptr, o_ptr->k_idx);

		o_ptr->iy = iy;
		o_ptr->ix = ix;
		o_ptr->next_o_idx = next_o_idx;
		o_ptr->marked = marked;
		o_ptr->inscription = inscription;
		if (item >= 0) p_ptr->total_weight += (o_ptr->weight - weight);
	}

	if (item == INVEN_RARM)
	{
		if (mw_old_weight)
		{
			mw_old_weight = o_ptr->weight;
			p_ptr->total_weight -= (o_ptr->weight - 1);
			o_ptr->weight = 1;
		}
		if (mw_diff_to_melee)
		{
			o_ptr->to_h += mw_diff_to_melee;
			o_ptr->to_d += mw_diff_to_melee;
		}
	}

	/* Something happened */
	return (TRUE);
}



static bool item_tester_hook_identify_fully(object_type *o_ptr)
{
	return (bool)(!object_known_p(o_ptr) || !(o_ptr->ident & IDENT_MENTAL));
}

static bool item_tester_hook_identify_fully_weapon_armour(object_type *o_ptr)
{
	if (!item_tester_hook_identify_fully(o_ptr))
		return FALSE;
	return item_tester_hook_weapon_armour(o_ptr);
}

/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(bool only_equip)
{
	int             item;
	object_type     *o_ptr;
	char            o_name[MAX_NLEN];
	cptr            q, s;
	bool old_known;
	int idx;

	item_tester_no_ryoute = TRUE;
	if (only_equip)
		item_tester_hook = item_tester_hook_identify_fully_weapon_armour;
	else
		item_tester_hook = item_tester_hook_identify_fully;

	if (!can_get_item())
	{
		if (only_equip)
			item_tester_hook = item_tester_hook_weapon_armour;
		else
			item_tester_hook = NULL;
	}

	/* Get an item */
#ifdef JP
q = "どのアイテムを鑑定しますか? ";
s = "鑑定するべきアイテムがない。";
#else
	q = "Identify which item? ";
	s = "You have nothing to identify.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Identify it */
	old_known = identify_item(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_RARM)
	{
#ifdef JP
		msg_format("%^s: %s(%c)。", describe_use(item), o_name, index_to_label(item, TRUE));
#else
		msg_format("%^s: %s (%c).", describe_use(item), o_name, index_to_label(item, TRUE));
#endif


	}
	else if (item >= 0)
	{
#ifdef JP
		msg_format("ザック中: %s(%c)。", o_name, index_to_label(item, FALSE));
#else
		msg_format("In your pack: %s (%c).", o_name, index_to_label(item, FALSE));
#endif
	}
	else
	{
#ifdef JP
		msg_format("床上: %s。", o_name);
#else
		msg_format("On the ground: %s.", o_name);
#endif
	}

	/* Describe it fully */
	(void)screen_object(o_ptr, NULL, TRUE);

	/* Auto-inscription/destroy */
	idx = is_autopick(o_ptr);
	auto_inscribe_item(item, idx);
	if (destroy_identify && !old_known)
		auto_destroy_item(item, idx);

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool item_tester_hook_recharge(object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return TRUE;

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return TRUE;

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_ROD) return TRUE;

	/* Nope */
	return FALSE;
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 * This function has been rewritten in Oangband and ZAngband.
 *
 * Sorcery/Arcane -- Recharge  --> recharge(plev * 4)
 * Chaos -- Arcane Binding     --> recharge(90)
 *
 * Scroll of recharging        --> recharge(130)
 * Artifact activation/Thingol --> recharge(130)
 *
 * It is harder to recharge high level, and highly charged wands,
 * staffs, and rods.  The more wands in a stack, the more easily and
 * strongly they recharge.  Staffs, however, each get fewer charges if
 * stacked.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 */
bool recharge(int power)
{
	int item, lev;
	int recharge_strength, recharge_amount;

	object_type *o_ptr;
	object_kind *k_ptr;

	bool fail = FALSE;
	byte fail_type = 1;

	cptr q, s;
	char o_name[MAX_NLEN];

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
#ifdef JP
q = "どのアイテムに魔力を充填しますか? ";
s = "魔力を充填すべきアイテムがない。";
#else
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Extract the object "level" */
	lev = get_object_level(o_ptr);


	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = ((power > lev/2) ? (power - lev/2) : 0) / 5;


		/* Back-fire */
		if (one_in_(recharge_strength))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* Recharge */
		else
		{
			/* Recharge amount */
			recharge_amount = (power * damroll(3, 2));

			/* Recharge by that amount */
			if (o_ptr->timeout > recharge_amount)
				o_ptr->timeout -= recharge_amount;
			else
				o_ptr->timeout = 0;
		}
	}


	/* Recharge wand/staff */
	else
	{
		/* Extract a recharge strength by comparing object level to power.
		 * Divide up a stack of wands' charges to calculate charge penalty.
		 */
		if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			recharge_strength = (100 + power - lev -
			(8 * o_ptr->pval / o_ptr->number)) / 15;

		/* All staffs, unstacked wands. */
		else recharge_strength = (100 + power - lev -
			(8 * o_ptr->pval)) / 15;

		/* Paranoia */
		if (recharge_strength < 0) recharge_strength = 0;

		/* Back-fire */
		if (one_in_(recharge_strength))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* If the spell didn't backfire, recharge the wand or staff. */
		else
		{
			/* Recharge based on the standard number of charges. */
			recharge_amount = randint1(1 + k_ptr->pval / 2);

			/* Multiple wands in a stack increase recharging somewhat. */
			if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			{
				recharge_amount +=
					(randint1(recharge_amount * (o_ptr->number - 1))) / 2;
				if (recharge_amount < 1) recharge_amount = 1;
				if (recharge_amount > 12) recharge_amount = 12;
			}

			/* But each staff in a stack gets fewer additional charges,
			 * although always at least one.
			 */
			if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
			{
				recharge_amount /= o_ptr->number;
				if (recharge_amount < 1) recharge_amount = 1;
			}

			/* Recharge the wand or staff. */
			o_ptr->pval += recharge_amount;


			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}
	}


	/* Inflict the penalties for failing a recharge. */
	if (fail)
	{
		/* Artifacts are never destroyed. */
		if (artifact_p(o_ptr))
		{
			object_desc(o_name, o_ptr, TRUE, 0);
#ifdef JP
msg_format("魔力が逆流した！%sは完全に魔力を失った。", o_name);
#else
			msg_format("The recharging backfires - %s is completely drained!", o_name);
#endif


			/* Artifact rods. */
			if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout < 10000))
				o_ptr->timeout = (o_ptr->timeout + 100) * 2;

			/* Artifact wands and staffs. */
			else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
				o_ptr->pval = 0;
		}
		else
		{
			/* Get the object description */
			object_desc(o_name, o_ptr, FALSE, 0);

			/*** Determine Seriousness of Failure ***/

			/* Mages recharge objects more safely. */
			if ((p_ptr->pclass == CLASS_WIZARD) || (p_ptr->pclass == CLASS_WARLOCK) || (p_ptr->pclass == CLASS_WITCH) || (p_ptr->pclass == CLASS_SIRENE) || (p_ptr->pclass == CLASS_LICH) || (p_ptr->pclass == CLASS_HIGHWITCH))
			{
				/* 10% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (one_in_(10)) fail_type = 2;
					else fail_type = 1;
				}
				/* 75% chance to blow up one wand, otherwise draining. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (!one_in_(3)) fail_type = 2;
					else fail_type = 1;
				}
				/* 50% chance to blow up one staff, otherwise no effect. */
				else if (o_ptr->tval == TV_STAFF)
				{
					if (one_in_(2)) fail_type = 2;
					else fail_type = 0;
				}
			}

			/* All other classes get no special favors. */
			else
			{
				/* 33% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (one_in_(3)) fail_type = 2;
					else fail_type = 1;
				}
				/* 20% chance of the entire stack, else destroy one wand. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (one_in_(5)) fail_type = 3;
					else fail_type = 2;
				}
				/* Blow up one staff. */
				else if (o_ptr->tval == TV_STAFF)
				{
					fail_type = 2;
				}
			}

			/*** Apply draining and destruction. ***/

			/* Drain object or stack of objects. */
			if (fail_type == 1)
			{
				if (o_ptr->tval == TV_ROD)
				{
#ifdef JP
msg_print("魔力が逆噴射して、ロッドからさらに魔力を吸い取ってしまった！");
#else
					msg_print("The recharge backfires, draining the rod further!");
#endif

					if (o_ptr->timeout < 10000)
						o_ptr->timeout = (o_ptr->timeout + 100) * 2;
				}
				else if (o_ptr->tval == TV_WAND)
				{
#ifdef JP
msg_format("%sは破損を免れたが、魔力が全て失われた。", o_name);
#else
					msg_format("You save your %s from destruction, but all charges are lost.", o_name);
#endif

					o_ptr->pval = 0;
				}
				/* Staffs aren't drained. */
			}

			/* Destroy an object or one in a stack of objects. */
			if (fail_type == 2)
			{
				if (o_ptr->number > 1)
#ifdef JP
msg_format("乱暴な魔法のために%sが一本壊れた！", o_name);
#else
					msg_format("Wild magic consumes one of your %s!", o_name);
#endif

				else
#ifdef JP
msg_format("乱暴な魔法のために%sが壊れた！", o_name);
#else
					msg_format("Wild magic consumes your %s!", o_name);
#endif


				/* Reduce rod stack maximum timeout, drain wands. */
				if (o_ptr->tval == TV_ROD) o_ptr->timeout = (o_ptr->number - 1) * k_ptr->pval;
				if (o_ptr->tval == TV_WAND) o_ptr->pval = 0;

				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}

			/* Destroy all members of a stack of objects. */
			if (fail_type == 3)
			{
				if (o_ptr->number > 1)
#ifdef JP
msg_format("乱暴な魔法のために%sが全て壊れた！", o_name);
#else
					msg_format("Wild magic consumes all your %s!", o_name);
#endif

				else
#ifdef JP
msg_format("乱暴な魔法のために%sが壊れた！", o_name);
#else
					msg_format("Wild magic consumes your %s!", o_name);
#endif



				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -999);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -999);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}


/*
 * Bless a weapon
 */
bool bless_weapon(void)
{
	int             item;
	object_type     *o_ptr;
	u32b flgs[TR_FLAG_SIZE];
	char            o_name[MAX_NLEN];
	cptr            q, s;

	item_tester_no_ryoute = TRUE;
	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon2;

	/* Get an item */
#ifdef JP
	q = "どのアイテムを祝福しますか？";
	s = "祝福できる武器がありません。";
#else
	q = "Bless which weapon? ";
	s = "You have weapon to bless.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	if (o_ptr->number > 1)
	{
#ifdef JP
		msg_print("複数のアイテムを同時に祝福することはできません。");
#else
		msg_print("It can bless only one item.");
#endif
		return FALSE;
	}

	/* Blessed against Unholy */
	if (have_flag(flgs, TR_UNHOLY))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		remove_flag(q_ptr->art_flags, TR_UNHOLY);

		/* Extract the flags */
		object_flags(q_ptr, flgs);

		if (have_flag(flgs, TR_UNHOLY))
		{
#ifdef JP
			msg_format("%sを覆う不浄なオーラは祝福を跳ね返した！", o_name);
#else
			msg_format("The unholy aura on %s %s disrupts the blessing!",
				((item >= 0) ? "your" : "the"), o_name);
#endif
			return TRUE;
		}

#ifdef JP
		msg_format("%sから不浄なオーラが消えた。", o_name);
#else
		msg_format("A unholy aura leaves %s %s.",
		    ((item >= 0) ? "your" : "the"), o_name);
#endif

		remove_flag(o_ptr->art_flags, TR_UNHOLY);

		/* Re-extract the flags */
		object_flags(o_ptr, flgs);
	}

	if (cursed_p(o_ptr))
	{
		if (o_ptr->curse_flags & TRC_PERMA_CURSE)
		{
#ifdef JP
			msg_format("%sを覆う黒いオーラは祝福を跳ね返した！", o_name);
#else
			msg_format("The black aura on %s %s disrupts the blessing!",
			    ((item >= 0) ? "your" : "the"), o_name);
#endif

			return TRUE;
		}

#ifdef JP
		msg_format("%sから邪悪なオーラが消えた。", o_name);
#else
		msg_format("A malignant aura leaves %s %s.",
		    ((item >= 0) ? "your" : "the"), o_name);
#endif

		/* Uncurse it */
		o_ptr->curse_flags = 0L;

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Take note */
		o_ptr->feeling = FEEL_NONE;

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Next, we try to bless it. */
	if (have_flag(flgs, TR_BLESSED))
	{
#ifdef JP
		msg_format("%sは既に祝福されている。", o_name);
#else
		msg_format("%s %s %s blessed already.",
		    ((item >= 0) ? "Your" : "The"), o_name,
		    ((o_ptr->number > 1) ? "were" : "was"));
#endif

		return TRUE;
	}

	/* Describe */
#ifdef JP
	msg_format("%sは輝いた！", o_name);
#else
	msg_format("%s %s shine%s!", ((item >= 0) ? "Your" : "The"), o_name,
	           ((o_ptr->number > 1) ? "" : "s"));
#endif

	add_flag(o_ptr->art_flags, TR_BLESSED);
	o_ptr->discount = 99;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	return TRUE;
}


/*
 * Unholy a weapon
 */
bool unholy_weapon(void)
{
	int             item;
	object_type     *o_ptr;
	u32b flgs[TR_FLAG_SIZE];
	char            o_name[MAX_NLEN];
	cptr            q, s;

	item_tester_no_ryoute = TRUE;
	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon2;

	/* Get an item */
#ifdef JP
	q = "どのアイテムを不浄化しますか？";
	s = "不浄化できる武器がありません。";
#else
	q = "Unholy which weapon? ";
	s = "You have weapon to unholy.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	if (o_ptr->number > 1)
	{
#ifdef JP
		msg_print("複数のアイテムを同時に不浄化することはできません。");
#else
		msg_print("It can unholy only one item.");
#endif
		return FALSE;
	}

	/* Unholy against Blessed */
	if (have_flag(flgs, TR_BLESSED))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		remove_flag(q_ptr->art_flags, TR_BLESSED);

		/* Extract the flags */
		object_flags(q_ptr, flgs);

		if (have_flag(flgs, TR_BLESSED))
		{
#ifdef JP
			msg_format("%sを覆う神聖なオーラは不浄化を跳ね返した！", o_name);
#else
			msg_format("The holy aura on %s %s disrupts the unholying!",
				((item >= 0) ? "your" : "the"), o_name);
#endif
			return TRUE;
		}

#ifdef JP
		msg_format("%sから神聖なオーラが消えた。", o_name);
#else
		msg_format("A holy aura leaves %s %s.",
		    ((item >= 0) ? "your" : "the"), o_name);
#endif

		remove_flag(o_ptr->art_flags, TR_BLESSED);

		/* Re-extract the flags */
		object_flags(o_ptr, flgs);
	}

	/* Next, we try to unholy it. */
	if (have_flag(flgs, TR_UNHOLY))
	{
#ifdef JP
		msg_format("%sは既に不浄化されている。", o_name);
#else
		msg_format("%s %s %s unholied already.",
		    ((item >= 0) ? "Your" : "The"), o_name,
		    ((o_ptr->number > 1) ? "were" : "was"));
#endif

		return TRUE;
	}

	/* Describe */
#ifdef JP
	msg_format("%sは黒く輝いた！", o_name);
#else
	msg_format("%s %s shine%s black!", ((item >= 0) ? "Your" : "The"), o_name,
	           ((o_ptr->number > 1) ? "" : "s"));
#endif

	add_flag(o_ptr->art_flags, TR_UNHOLY);
	o_ptr->discount = 99;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	return TRUE;
}


/*
 * Potions "smash open" and cause an area effect when
 * (1) they are shattered while in the player's inventory,
 * due to cold (etc) attacks;
 * (2) they are thrown at a monster, or obstacle;
 * (3) they are shattered by a "cold ball" or other such spell
 * while lying on the floor.
 *
 * Arguments:
 *    who   ---  who caused the potion to shatter (0=player)
 *          potions that smash on the floor are assumed to
 *          be caused by no-one (who = 1), as are those that
 *          shatter inside the player inventory.
 *          (Not anymore -- I changed this; TY)
 *    y, x  --- coordinates of the potion (or player if
 *          the potion was in her inventory);
 *    o_ptr --- pointer to the potion object.
 */
bool potion_smash_effect(int who, int y, int x, int k_idx)
{
	int     radius = 2;
	int     dt = 0;
	int     dam = 0;
	bool    angry = FALSE;

	object_kind *k_ptr = &k_info[k_idx];

	switch (k_ptr->sval)
	{
		case SV_POTION_SALT_WATER:
		case SV_POTION_SLIME_MOLD:
		case SV_POTION_LOSE_MEMORIES:
		case SV_POTION_DEC_STR:
		case SV_POTION_DEC_INT:
		case SV_POTION_DEC_WIS:
		case SV_POTION_DEC_DEX:
		case SV_POTION_DEC_CON:
		case SV_POTION_DEC_CHR:
		case SV_POTION_WATER:   /* perhaps a 'water' attack? */
		case SV_POTION_APPLE_JUICE:
			return TRUE;

		case SV_POTION_INFRAVISION:
		case SV_POTION_DETECT_INVIS:
		case SV_POTION_SLOW_POISON:
		case SV_POTION_CURE_POISON:
		case SV_POTION_BOLDNESS:
		case SV_POTION_RESIST_HEAT:
		case SV_POTION_RESIST_COLD:
		case SV_POTION_HEROISM:
		case SV_POTION_BERSERK_STRENGTH:
		case SV_POTION_RES_STR:
		case SV_POTION_RES_INT:
		case SV_POTION_RES_WIS:
		case SV_POTION_RES_DEX:
		case SV_POTION_RES_CON:
		case SV_POTION_RES_CHR:
		case SV_POTION_RESTORING:
		case SV_POTION_ENLIGHTENMENT:
		case SV_POTION_CURE_STONING:
		case SV_POTION_SELF_KNOWLEDGE:
		case SV_POTION_EXPERIENCE:
		case SV_POTION_RESISTANCE:
		case SV_POTION_INVULNERABILITY:
		case SV_POTION_CURE_MUTATIONS:
			/* All of the above potions have no effect when shattered */
			return FALSE;
		case SV_POTION_SLOWNESS:
			dt = GF_OLD_SLOW;
			dam = 5;
			angry = TRUE;
			break;
		case SV_POTION_POISON:
			dt = GF_POIS;
			dam = 3;
			angry = TRUE;
			break;
		case SV_POTION_BLINDNESS:
			dt = GF_DARK;
			angry = TRUE;
			break;
		case SV_POTION_CONFUSION: /* Booze */
			dt = GF_OLD_CONF;
			angry = TRUE;
			break;
		case SV_POTION_SLEEP:
			dt = GF_OLD_SLEEP;
			angry = TRUE;
			break;
		case SV_POTION_RUINATION:
		case SV_POTION_DETONATIONS:
			dt = GF_SHARDS;
			dam = damroll(25, 25);
			angry = TRUE;
			break;
		case SV_POTION_DEATH:
			dt = GF_DEATH_RAY;    /* !! */
			dam = k_ptr->level * 10;
			angry = TRUE;
			radius = 1;
			break;
		case SV_POTION_SPEED:
			dt = GF_OLD_SPEED;
			break;
		case SV_POTION_CURE_LIGHT:
			dt = GF_OLD_HEAL;
			dam = damroll(2, 3);
			break;
		case SV_POTION_CURE_SERIOUS:
			dt = GF_OLD_HEAL;
			dam = damroll(4, 3);
			break;
		case SV_POTION_CURE_CRITICAL:
		case SV_POTION_CURING:
			dt = GF_OLD_HEAL;
			dam = damroll(6, 3);
			break;
		case SV_POTION_HEALING:
			dt = GF_OLD_HEAL;
			dam = damroll(10, 10);
			break;
		case SV_POTION_RESTORE_EXP:
			dt = GF_STAR_HEAL;
			dam = 0;
			radius = 1;
			break;
		case SV_POTION_LIFE:
			dt = GF_STAR_HEAL;
			dam = damroll(50, 50);
			radius = 1;
			break;
		case SV_POTION_STAR_HEALING:
			dt = GF_OLD_HEAL;
			dam = damroll(50, 50);
			radius = 1;
			break;
		case SV_POTION_STAR_CURING:
			dt = GF_STAR_HEAL;
			dam = damroll(20, 20);
			radius = 1;
			break;
		case SV_POTION_RESTORE_MANA:   /* MANA */
			dt = GF_MANA;
			dam = damroll(10, 10);
			radius = 1;
			break;
		default:
			/* Do nothing */  ;
	}

	(void)project(who, radius, y, x, dam, dt,
	    (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL), MODIFY_ELEM_MODE_NONE);

	/* XXX  those potions that explode need to become "known" */
	return angry;
}


/*
 * Hack -- Display all known spells in a window
 *
 * XXX XXX XXX Need to analyze size of the window.
 *
 * XXX XXX XXX Need more color coding.
 */
void display_spell_list(void)
{
	int             i, j;
	int             y, x;
	int             m[9];
	magic_type      *s_ptr;
	char            name[80];
	char            out_val[160];


	/* Erase window */
	clear_from(0);

	/* They have too many spells to list */
	if (p_ptr->pclass == CLASS_LICH) return;
	if (p_ptr->pclass == CLASS_HIGHWITCH) return;

	/* Normal spellcaster with books */

	/* Scan books */
	for (j = 0; j < MAX_REALM; j++)
	{
		int n = 0;

		if (!can_use_realm(j + 1)) continue;

		/* Reset vertical */
		m[j] = 0;

		/* Vertical location */
		y = (j < 3) ? 0 : (m[j - 3] + 2);

		/* Horizontal location */
		x = 27 * (j % 3);

		/* Scan spells */
		for (i = 0; i < 32; i++)
		{
			int  skill_lev = p_ptr->spell_skill_lev[j][i];

			byte a = TERM_WHITE;

			/* Access the spell */
			s_ptr = &mp_ptr->info[j][i];

			strcpy(name, spell_names[j][i]);

			/* Illegible */
			if (s_ptr->slevel >= 99)
			{
				/* Illegible */
#ifdef JP
strcpy(name, "(判読不能)");
#else
				strcpy(name, "(illegible)");
#endif


				/* Unusable */
				a = TERM_L_DARK;
			}

			/* Unknown */
			if ((s_ptr->slevel > p_ptr->max_plv) || (skill_lev == SKILL_LEVEL_BEGINNER))
			{
				/* Unknown */
				a = TERM_RED;
			}

			/* Forgotten */
			else if (s_ptr->slevel > p_ptr->lev)
			{
				/* Forgotten */
				a = TERM_ORANGE;
			}

			/* Dump the spell --(-- */
			sprintf(out_val, "%c/%c) %-20.20s",
				I2A(n / 8), I2A(n % 8), name);

			/* Track maximum */
			m[j] = y + n;

			/* Dump onto the window */
			Term_putstr(x, m[j], -1, a, out_val);

			/* Next */
			n++;
		}
	}
}


int calc_use_mana(int spell, int realm)
{
	/* Access the spell */
	magic_type *s_ptr = &mp_ptr->info[realm - 1][spell];

	int skill_lev = p_ptr->spell_skill_lev[realm - 1][spell];
	int magic_var = skill_lev_var[skill_lev];

	/* Extract mana consumption rate */
	int use_mana = s_ptr->smana * 200 * (19 - magic_var) + 2399;

	if (p_ptr->dec_mana) use_mana *= 3;
	else use_mana *= 4;
	use_mana /= 9600;

	if (use_mana < 1) use_mana = 1;

	return use_mana;
}


/*
 * Returns spell chance of failure for spell -RAK-
 */
s16b spell_chance(int spell, int use_realm)
{
	int             chance, minfail;
	magic_type      *s_ptr;
	int             use_mana;
	int             skill_lev;
	int penalty = (mp_ptr->spell_stat == A_WIS) ? 10 : 4;


	/* Paranoia -- must be literate */
	if (!mp_ptr->spell_book) return (100);

	/* Access the spell */
	s_ptr = &mp_ptr->info[use_realm - 1][spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;
	if (use_realm == (REALM_FIRE + get_cur_pelem())) chance -= 5;
	if (use_realm == (REALM_FIRE + get_opposite_elem(get_cur_pelem()))) chance += 5;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

	if (p_ptr->riding)
		chance += (MAX(r_info[m_list[p_ptr->riding].r_idx].level-(skill_lev_var[p_ptr->misc_skill_lev[SKILL_RIDING]] * 1000)/100-10,0));

	skill_lev = p_ptr->spell_skill_lev[use_realm - 1][spell];

	use_mana = calc_use_mana(spell, use_realm);

	/* Not enough mana to cast */
	if (use_mana > p_ptr->csp)
	{
		chance += 5 * (use_mana - p_ptr->csp);
	}

	chance += p_ptr->to_m_chance;

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

	/*
	 * Non mage/priest characters never get too good
	 * (added high mage, mindcrafter)
	 */
	if (mp_ptr->spell_xtra & MAGIC_FAIL_5PERCENT)
	{
		if (minfail < 5) minfail = 5;
	}

	/* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
	if (p_ptr->icky_wield[0] > 0) chance += p_ptr->icky_wield[0];
	if (p_ptr->icky_wield[1] > 0) chance += p_ptr->icky_wield[1];

	if (p_ptr->heavy_spell) chance += 20;
	if(p_ptr->dec_mana && p_ptr->easy_spell) chance-=4;
	else if (p_ptr->easy_spell) chance-=3;
	else if (p_ptr->dec_mana) chance-=2;

	if (fool_effect_status & FOOL_STATUS_PLAYER) chance += 10;

	if (((use_realm == REALM_HOLY) || (use_realm == REALM_CRUSADE)) && (get_your_alignment_gne() == ALIGN_GNE_EVIL)) chance += penalty;
	if ((use_realm == REALM_DEATH) && (get_your_alignment_gne() == ALIGN_GNE_GOOD)) chance += penalty;

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 200) chance += 40;
	else if (p_ptr->stun > 100) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	switch (skill_lev)
	{
	case SKILL_LEVEL_EXPERT:
		chance--;
		break;
	case SKILL_LEVEL_MASTER:
		chance -= 2;
		break;
	}

	if (p_ptr->dec_mana) chance--;
	if (p_ptr->heavy_spell) chance += 5;

	chance = MAX(chance,0);

	/* Return the chance */
	return (chance);
}



/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, int use_realm)
{
	/* Access the spell */
	magic_type *s_ptr = &mp_ptr->info[use_realm - 1][spell];

	int skill_lev = p_ptr->spell_skill_lev[use_realm - 1][spell];

	/* Spell is illegal */
	if ((s_ptr->slevel > p_ptr->lev) || (skill_lev == SKILL_LEVEL_BEGINNER)) return FALSE;

	return TRUE;
}



/*
 * Extra information on a spell -DRS-
 *
 * We can use up to 14 characters of the buffer 'p'
 *
 * The strings in this function were extracted from the code in the
 * functions "do_cmd_cast()" and "do_cmd_pray()" and may be dated.
 */
static void spell_info(char *p, int spell, int use_realm)
{
	int plev = p_ptr->lev;
	int pstat;
	int dummy;
	char dummy_buf[16];

	/* See below */
	int burst = plev + (plev / (((p_ptr->pclass == CLASS_WIZARD) ||
		(p_ptr->pclass == CLASS_SIRENE) ||
		(p_ptr->pclass == CLASS_LICH)) ? 2 : 4));

	int attacks = 3;

#ifdef JP
	cptr s_dam = "損傷:";
	cptr s_dur = "期間:";
	cptr s_range = "範囲:";
	cptr s_heal = "回復:";
	cptr s_delay = "遅延:";
#else
	cptr s_dam = "dam ";
	cptr s_dur = "dur ";
	cptr s_range = "range ";
	cptr s_heal = "heal ";
	cptr s_delay = "delay ";
#endif
	/* Default */
	strcpy(p, "");

	/* Call the elemental attack number (base = 3) */
	pstat = p_ptr->stat_use[A_INT];
	if (pstat >= (18 + 100)) attacks++;
	if (pstat >= (18 + 150)) attacks++;
	if (pstat >= (18 + 180)) attacks++;
	if (pstat >= (18 + 200)) attacks++;
	if (pstat >= (18 + 220)) attacks++;

	/* Analyze the spell */
	switch (use_realm)
	{
	case REALM_MAGERY:
		switch (spell)
		{
		case  0: sprintf(p, " %s%dd4", s_dam, 3 + ((plev - 1) / 5)); break;
		case  2: sprintf(p, " %s10", s_range); break;
		case  4: sprintf(p, " %s2d%d", s_dam, plev / 2); break;
		case  6: sprintf(p, " %s%d", s_range, plev * 5); break;
		case  7: sprintf(p, " %s3d5+%d", s_dam, burst); break;
		case  8: sprintf(p, " %s%dd8", s_dam, (3 + ((plev - 5) / 4))); break;
		case 11: sprintf(p, " %s25+d30", s_dur); break;
		case 12: sprintf(p, " %s%dd8", s_dam, (11 + ((plev - 5) / 4))); break;
		case 14: sprintf(p, " %s15+d21", s_delay); break;
		case 20: sprintf(p, " %s%d", s_dam, 120 + plev); break;
		case 25: sprintf(p, " %s%d", s_dam, 300 + (plev * 4)); break;
		}
		break;

	case REALM_FIRE:
		switch (spell)
		{
		case  0: sprintf(p, " %s%dd4", s_dam, 3 + ((plev - 1) / 5)); break;
		case  2: sprintf(p, " %s%dd8", s_dam, (8 + ((plev - 5) / 4))); break;
		case  4: sprintf(p, " %s%d+d%d", s_dam, plev * 2 + 10, plev); break;
		case  7: sprintf(p, " %s20+d20", s_dur); break;
#ifdef JP
		case  8: sprintf(p, " 損:(%d+d%d)*%d", plev, plev, attacks); break;
#else
		case  8: sprintf(p, " d (%d+d%d)*%d", plev, plev, attacks); break;
#endif
		case  9: sprintf(p, " %s%d", s_dam, MIN(p_ptr->chp / 2, 350)); break;
#ifdef JP
		case 10: sprintf(p, " %s各%d", s_dam, plev * 3 + 25); break;
#else
		case 10: sprintf(p, " %s%d each", s_dam, plev * 3 + 25); break;
#endif
		case 11: sprintf(p, " %s999", s_dam); break;
		case 12: sprintf(p, " %s10+d10", s_dur); break;
#ifdef JP
		case 13: sprintf(p, " %s各%d+d%d", s_dam, plev * 8, plev * 5); break;
#else
		case 13: sprintf(p, " %s%d+d%d each", s_dam, plev * 8, plev * 5); break;
#endif
		}
		break;

	case REALM_AQUA:
		switch (spell)
		{
		case  1: sprintf(p, " %s%dd8", s_dam, (6 + ((plev - 5) / 4))); break;
		case  2: sprintf(p, " %s%d+d%d", s_dam, plev * 2 + 10, plev); break;
		case  3: sprintf(p, " %s4d8", s_heal); break;
		case  5: sprintf(p, " %s%d+d%d", s_dur, plev, plev + 25); break;
		case  6: sprintf(p, " %s%d", s_dam, 100 + plev * 3 / 2); break;
		case  7:
			pstat = p_ptr->stat_use[A_INT];
			dummy = 3;
			if (pstat >= (18 + 150)) dummy++;
			if (pstat >= (18 + 200)) dummy++;
			sprintf(p, " %s%d*%d", s_dam, 15 + plev / 2, dummy);
			break;
		case  8: sprintf(p, " %s%d", s_range, ((rp_ptr->r_flags & PRF_AQUATIC) ? (plev + 20) : (plev / 2 + 10))); break;
#ifdef JP
		case  9: sprintf(p, " 損:(%d+d%d)*%d", plev, plev, attacks); break;
#else
		case  9: sprintf(p, " d (%d+d%d)*%d", plev, plev, attacks); break;
#endif
		case 10: sprintf(p, " %s500", s_heal); break;
		case 12: sprintf(p, " %s10d%d", s_dam, plev); break;
#ifdef JP
		case 13: sprintf(p, " %s各%d+d%d", s_dam, plev * 8, plev * 5); break;
#else
		case 13: sprintf(p, " %s%d+d%d each", s_dam, plev * 8, plev * 5); break;
#endif
		}
		break;

	case REALM_EARTH:
		switch (spell)
		{
		case  0: sprintf(p, " %s%dd4", s_dam, 3 + ((plev - 1) / 5)); break;
		case  2: sprintf(p, " %s%dd8", s_dam, (8 + ((plev - 5) / 4))); break;
		case  4: sprintf(p, " %s%d+d%d", s_dam, plev * 2 + 10, plev); break;
		case  6: sprintf(p, " %s%d+d%d", s_dur, plev/2, plev/2); break;
		case  7:
			if (p_ptr->stat_use[A_INT] >= (18 + 200)) sprintf(dummy_buf, "+2d%d", plev);
			else dummy_buf[0] = '\0';
#ifdef JP
			sprintf(p, " 損:10d%d*2%s", plev / 3, dummy_buf); break;
#else
			sprintf(p, " d 10d%d*2%s", plev / 3, dummy_buf); break;
#endif
		case  8: sprintf(p, " %s6+d6", s_dur); break;
#ifdef JP
		case  9: sprintf(p, " 損:(%d+d%d)*%d", plev, plev, attacks); break;
#else
		case  9: sprintf(p, " d (%d+d%d)*%d", plev, plev, attacks); break;
#endif
		case 10: sprintf(p, " %s%d+d%d", s_dam, 100 + plev, plev * 2); break;
		case 11: sprintf(p, " %s%d", s_dam, MIN(p_ptr->chp / 2, 350)); break;
#ifdef JP
		case 12: sprintf(p, " %s各%d+d%d", s_dam, plev * 8, plev * 5); break;
#else
		case 12: sprintf(p, " %s%d+d%d each", s_dam, plev * 8, plev * 5); break;
#endif
		}
		break;

	case REALM_WIND:
		switch (spell)
		{
		case  0: sprintf(p, " %s%dd4", s_dam, 3 + ((plev - 1) / 5)); break;
		case  2: sprintf(p, " %s20+d%d", s_dur, plev); break;
		case  3: sprintf(p, " %s%dd8", s_dam, (8 + ((plev - 5) / 4))); break;
		case  4: sprintf(p, " %s%d+d%d", s_dam, plev * 2 + 10, plev); break;
		case  5: sprintf(p, " %s%d+d%d", s_dur, plev, plev + 25); break;
		case  7: sprintf(p, " %s%d+%d*2", s_dam, (40 + plev / 2) / 2, (20 + plev / 2) / 2); break;
#ifdef JP
		case  8: sprintf(p, " 損:(%d+d%d)*%d", plev, plev, attacks); break;
#else
		case  8: sprintf(p, " d (%d+d%d)*%d", plev, plev, attacks); break;
#endif
		case  9: sprintf(p, " %s%d", s_dam, MIN(p_ptr->chp / 2, 350)); break;
		case 10: sprintf(p, " %s%d", s_range, plev / 2 + 10); break;
#ifdef JP
		case 11: sprintf(p, " %s各%d+d%d", s_dam, plev * 8, plev * 5); break;
#else
		case 11: sprintf(p, " %s%d+d%d each", s_dam, plev * 8, plev * 5); break;
#endif
		}
		break;

	case REALM_HOLY:
		switch (spell)
		{
		case  1: sprintf(p, " %s12+d12", s_dur); break;
		case  2: sprintf(p, " %s2d%d", s_dam, plev / 2); break;
		case  3: sprintf(p, " %s8d10", s_heal); break;
		case  9: sprintf(p, " %s100+d%d", s_dam, plev * 2); break;
		case 11: sprintf(p, " %s1000", s_heal); break;
		case 13: sprintf(p, " %s%d+d25", s_dur, plev * 3); break;
		case 14: sprintf(p, " %s300", s_heal); break;
		case 15: sprintf(p, " %s20+d20", s_dur); break;
		case 19:
			pstat = p_ptr->stat_use[A_WIS];
			attacks = 1;
			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;
#ifdef JP
			sprintf(p, " 損:(%d+d%d)*%d", plev, plev, attacks); break;
#else
			sprintf(p, " d (%d+d%d)*%d", plev, plev, attacks); break;
#endif
		case 22: sprintf(p, " %s5000", s_heal); break;
		case 23: sprintf(p, " %s6+d6", s_dur); break;
		}
		break;

	case REALM_DEATH:
		switch (spell)
		{
		case  3: sprintf(p, " %s3d6+%d", s_dam, plev + plev / 2); break;
		case  4: sprintf(p, " %s%d+d%d", s_dur, plev, plev + 25); break;
		case  5: sprintf(p, " %s%d+%dd4", s_dam, plev + 30, 4 + ((plev - 10) / 5)); break;
#ifdef JP
		case  6: sprintf(p, " %s各%dd10", s_dam, plev / 5 + 10); break;
#else
		case  6: sprintf(p, " %s%dd10 each", s_dam, plev / 10 + 5); break;
#endif
		case  9: sprintf(p, " %s%d", s_dam, MAX(p_ptr->mhp - p_ptr->chp, 0)); break;
		case 15: sprintf(p, " %s100*3", s_dam); break;
#ifdef JP
		case 16: sprintf(p, " 損:(%d+d%d)*%d", plev, plev, attacks - 2); break;
#else
		case 16: sprintf(p, " d (%d+d%d)*%d", plev, plev, attacks - 2); break;
#endif
#ifdef JP
		case 17: sprintf(p, " %s各%d", s_dam, plev * 4); break;
#else
		case 17: sprintf(p, " %s%d each", s_dam, plev * 4); break;
#endif
		case 19: sprintf(p, " %s%d", s_dam, 100 + plev * 4); break;
		case 20: sprintf(p, " %s%d+d%d", s_dur, plev/2, plev/2); break;
		case 22: sprintf(p, " %s666", s_dam); break;
		case 23: sprintf(p, " %s4+2d2", s_dur); break;
#ifdef JP
		case 24: sprintf(p, " %s各%d+d%d", s_dam, plev * 8, plev * 5); break;
#else
		case 24: sprintf(p, " %s%d+d%d each", s_dam, plev * 8, plev * 5); break;
#endif
		}
		break;

	case REALM_SYMBIOTIC:
		switch (spell)
		{
		case  2: sprintf(p, " %s4d6", s_heal); break;
#ifdef JP
		case 12: sprintf(p, " %s各200", s_heal); break;
#else
		case 12: sprintf(p, " %s%200 each", s_heal); break;
#endif
		case 14: sprintf(p, " %s1000", s_heal); break;
		}
		break;

	case REALM_WITCH:
		switch (spell)
		{
		case  0: sprintf(p, " %s100+d100", s_dur); break;
		case  3: sprintf(p, " %s25+d25", s_dur); break;
		case  5: sprintf(p, " %s20+d%d", s_dur, plev); break;
		case  6: sprintf(p, " %s4d8", s_heal); break;
		case  7: sprintf(p, " %s%d+d%d", s_dur, plev, plev + 25); break;
		case  9: sprintf(p, " %s3d5+%d", s_dam, plev + plev / 2); break;
		case 13: sprintf(p, " %s%d+d%d", s_dam, plev * 2 + 10, plev); break;
		case 15: sprintf(p, " %s20+d20", s_dur); break;
		case 16: sprintf(p, " %s%d+d%d", s_dur, plev/2, plev/2); break;
		case 20: sprintf(p, " %s100+d%d", s_dam, plev * 2); break;
		case 21: sprintf(p, " %s20+d20", s_dur); break;
		case 26: sprintf(p, " %s500", s_heal); break;
		case 27: sprintf(p, " %s%d", s_dam, 200 + plev * 3); break;
		case 28: sprintf(p, " %s%d", s_range, plev / 2 + 10); break;
		case 30: sprintf(p, " %s%d+d%d", s_dur, (plev / 2), (plev / 2)); break;
		}
		break;

	case REALM_DRAKONITE:
		switch (spell)
		{
#ifdef JP
		case  2: sprintf(p, " %s各%d+d%d", s_dam, plev * 6, plev * 5); break;
#else
		case  2: sprintf(p, " %s%d+d%d each", s_dam, plev * 6, plev * 5); break;
#endif
#ifdef JP
		case  3: sprintf(p, " %s各%d+d%d", s_dam, plev * 6, plev * 5); break;
#else
		case  3: sprintf(p, " %s%d+d%d each", s_dam, plev * 6, plev * 5); break;
#endif
#ifdef JP
		case  4: sprintf(p, " %s各%d+d%d", s_dam, plev * 6, plev * 5); break;
#else
		case  4: sprintf(p, " %s%d+d%d each", s_dam, plev * 6, plev * 5); break;
#endif
#ifdef JP
		case  5: sprintf(p, " %s各%d+d%d", s_dam, plev * 6, plev * 5); break;
#else
		case  5: sprintf(p, " %s%d+d%d each", s_dam, plev * 6, plev * 5); break;
#endif
		}
		break;

	case REALM_CRUSADE:
		switch (spell)
		{
		case  1: sprintf(p, " %s%dd4", s_dam, 3 + ((plev - 1) / 5)); break;
		case  4: sprintf(p, " %s24+d24", s_dur); break;
#ifdef JP
		case  5: sprintf(p, " %s各%dd2", s_dam, 3 + ((plev - 1) / 9)); break;
#else
		case  5: sprintf(p, " %s%dd2 each", s_dam, 3 + ((plev - 1) / 9)); break;
#endif
		case  7: sprintf(p, " %s3d6+%d", s_dam, plev + plev / 2); break;
#ifdef JP
		case  8: sprintf(p, " %s各d%d", s_dam, plev * 4); break;
#else
		case  8: sprintf(p, " %sd%d each", s_dam, plev * 4); break;
#endif
		case  9: sprintf(p, " %s%d", s_dam, plev * 5); break;
#ifdef JP
		case 10: sprintf(p, " %s各%d", s_dam, plev * 3 + 25); break;
#else
		case 10: sprintf(p, " %s%d each", s_dam, plev * 3 + 25); break;
#endif
		case 11: sprintf(p, " %s%d", s_dam, p_ptr->chp); break;
		case 12: sprintf(p, " %s25+d25", s_dur); break;
		case 14: sprintf(p, " %s25+d25", s_dur); break;
		case 15: sprintf(p, " %s20+d20", s_dur); break;
		case 17: sprintf(p, " %s%d", s_dam, p_ptr->chp / 3); break;
#ifdef JP
		case 18: sprintf(p, " 損:d%d/回:100", 6 * plev); break;
#else
		case 18: sprintf(p, " dam:d%d/h100", 6 * plev); break;
#endif
		case 19: sprintf(p, " %s%d", s_dam, 2 * plev + 100); break;
		case 21: sprintf(p, " %s10+d10", s_dur); break;
		case 22: sprintf(p, " %s%d", s_dam, 200 + plev * 2); break;
#ifdef JP
		case 23: sprintf(p, " 回100/損%d+%d", plev * 4, plev*11/2); break;
#else
		case 23: sprintf(p, " h100/dm%d+%d", plev * 4, plev*11/2); break;
#endif
		}
		break;

	default:
#ifdef JP
		sprintf(p, "未知のタイプ: %d", use_realm);
#else
		sprintf(p, "Unknown type: %d.", use_realm);
#endif
	}
}


/*
 * Print a list of spells (for browsing or casting or viewing)
 */
void print_spells(int target_spell, byte *spells, int num, int y, int x, int use_realm)
{
	int             i, spell;
	magic_type      *s_ptr;
	cptr            comment;
	char            info[80];
	char            out_val[160];
	byte            line_attr;
	int             use_mana;
	int             skill_lev;
	char            skill_lev_str_short[5];
	char            buf[256];
	bool            unlearned;


	if (((use_realm <= REALM_NONE) || (use_realm > MAX_REALM)) && p_ptr->wizard)
#ifdef JP
msg_print("警告！ print_spell が領域なしに呼ばれた");
#else
		msg_print("Warning! print_spells called with null realm");
#endif


	/* Title the list */
	prt("", y, x);
#ifdef JP
	strcpy(buf,"       Lv   MP 失率 効果");
#else
	strcpy(buf,"       Lv   SP Fail Effect");
#endif

#ifdef JP
put_str("名前", y, x + 5);
put_str(buf, y, x + 29);
#else
	put_str("Name", y, x + 5);
	put_str(buf, y, x + 29);
#endif

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Access the spell */
		spell = spells[i];

		/* Access the spell */
		s_ptr = &mp_ptr->info[use_realm - 1][spell];

		skill_lev = p_ptr->spell_skill_lev[use_realm - 1][spell];
		unlearned = (skill_lev == SKILL_LEVEL_BEGINNER);

		use_mana = calc_use_mana(spell, use_realm);

		if (unlearned) strncpy(skill_lev_str_short, skill_unlearned_str, 4);
		else strncpy(skill_lev_str_short, skill_lev_str[skill_lev], 4);
		skill_lev_str_short[3] = ']';
		skill_lev_str_short[4] = '\0';

		if (use_menu && target_spell)
		{
			if (i == (target_spell-1))
#ifdef JP
				strcpy(out_val, "  》 ");
#else
				strcpy(out_val, "  >  ");
#endif
			else
				strcpy(out_val, "     ");
		}
		else sprintf(out_val, "  %c) ", I2A(i));
		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
#ifdef JP
			strcat(out_val, format("%-30s", "(判読不能)"));
#else
			strcat(out_val, format("%-30s", "(illegible)"));
#endif

				c_prt(TERM_L_DARK, out_val, y + i + 1, x);
			continue;
		}

		/* XXX XXX Could label spells above the players level */

		/* Get extra info */
		spell_info(info, spell, use_realm);

		/* Use that info */
		comment = info;

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Analyze the spell */
		if ((s_ptr->slevel > p_ptr->max_plv) || unlearned)
		{
#ifdef JP
			comment = " 未知";
#else
			comment = " unknown";
#endif

			line_attr = TERM_L_BLUE;
		}
		else if (s_ptr->slevel > p_ptr->lev)
		{
#ifdef JP
			comment = " 忘却";
#else
			comment = " forgotten";
#endif

			line_attr = TERM_YELLOW;
		}

		/* Dump the spell --(-- */
		strcat(out_val, format("%-25s %-4s %2d %4d %3d%%%s",
		    spell_names[use_realm-1][spell], /* realm, spell */
		    skill_lev_str_short,
		    s_ptr->slevel, use_mana, spell_chance(spell, use_realm), comment));
		c_prt(line_attr, out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}


/*
 * Note that amulets, rods, and high-level spell books are immune
 * to "inventory damage" of any kind.  Also ammo and shovels.
 */


/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool hates_acid(object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable items */
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls are wood/paper */
		case TV_STAFF:
		case TV_SCROLL:
		{
			return (TRUE);
		}

		/* Ouch */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Junk is useless */
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
bool hates_elec(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_RING:
		case TV_WAND:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
bool hates_fire(object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable */
		case TV_LITE:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}

		/* Books */
		case TV_MAGERY_BOOK:
		case TV_FIRE_BOOK:
		case TV_AQUA_BOOK:
		case TV_EARTH_BOOK:
		case TV_WIND_BOOK:
		case TV_HOLY_BOOK:
		case TV_DEATH_BOOK:
		case TV_SYMBIOTIC_BOOK:
		case TV_WITCH_BOOK:
		case TV_DRAKONITE_BOOK:
		case TV_CRUSADE_BOOK:
		{
			return (TRUE);
		}

		/* Chests */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls burn */
		case TV_STAFF:
		case TV_SCROLL:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate cold?
 */
bool hates_cold(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_POTION:
		case TV_FLASK:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Melt something
 */
int set_acid_destroy(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];
	if (!hates_acid(o_ptr)) return (FALSE);
	object_flags(o_ptr, flgs);
	if (have_flag(flgs, TR_IGNORE_ACID)) return (FALSE);
	return (TRUE);
}


/*
 * Electrical damage
 */
int set_elec_destroy(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];
	if (!hates_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, flgs);
	if (have_flag(flgs, TR_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}


/*
 * Burn something
 */
int set_fire_destroy(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];
	if (!hates_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, flgs);
	if (have_flag(flgs, TR_IGNORE_FIRE)) return (FALSE);
	return (TRUE);
}


/*
 * Freeze things
 */
int set_cold_destroy(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];
	if (!hates_cold(o_ptr)) return (FALSE);
	object_flags(o_ptr, flgs);
	if (have_flag(flgs, TR_IGNORE_COLD)) return (FALSE);
	return (TRUE);
}


/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 * Destruction taken from "melee.c" code for "stealing".
 * New-style wands and rods handled correctly. -LM-
 * Returns number of items destroyed.
 */
int inven_damage(inven_func typ, int perc)
{
	int         i, j, k, amt;
	object_type *o_ptr;
	char        o_name[MAX_NLEN];

	/* Multishadow effects is determined by IS_MULTISHADOW() */
	if (IS_MULTISHADOW(0)) return 0;

	if (p_ptr->inside_arena) return 0;

	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr) || o_ptr->art_name) continue;

		/* Give this item slot a shot at death */
		if ((*typ)(o_ptr))
		{
			/* Count the casualties */
			for (amt = j = 0; j < o_ptr->number; ++j)
			{
				if (randint0(100) < perc) amt++;
			}

			/* Some casualities */
			if (amt)
			{
				/* Get a description */
				object_desc(o_name, o_ptr, FALSE, 3);

				/* Message */
#ifdef JP
				msg_format("%s(%c)が%s壊れてしまった！",
#else
				msg_format("%sour %s (%c) %s destroyed!",
#endif

#ifdef JP
				    o_name, index_to_label(i, FALSE),
				    ((o_ptr->number > 1) ?
				    ((amt == o_ptr->number) ? "全部" :
				    (amt > 1 ? "何個か" : "一個")) : "")    );
#else
				    ((o_ptr->number > 1) ?
				    ((amt == o_ptr->number) ? "All of y" :
				    (amt > 1 ? "Some of y" : "One of y")) : "Y"),
				    o_name, index_to_label(i, FALSE),
				    ((amt > 1) ? "were" : "was"));
#endif

				/* Potions smash open */
				if (object_is_potion(o_ptr))
				{
					(void)potion_smash_effect(0, py, px, o_ptr->k_idx);
				}

				/* Reduce the charges of rods/wands */
				reduce_charges(o_ptr, amt);

				/* Destroy "amt" items */
				inven_item_increase(i, -amt);
				inven_item_optimize(i);

				/* Count the casualties */
				k += amt;
			}
		}
	}

	/* Return the casualty count */
	return (k);
}


/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
	object_type *o_ptr = NULL;
	u32b flgs[TR_FLAG_SIZE];
	char        o_name[MAX_NLEN];


	/* Pick a (possibly empty) inventory slot */
	switch (randint1(7))
	{
		case 1: o_ptr = &inventory[INVEN_RARM]; break;
		case 2: o_ptr = &inventory[INVEN_LARM]; break;
		case 3: o_ptr = &inventory[INVEN_BODY]; break;
		case 4: o_ptr = &inventory[INVEN_OUTER]; break;
		case 5: o_ptr = &inventory[INVEN_HANDS]; break;
		case 6: o_ptr = &inventory[INVEN_HEAD]; break;
		case 7: o_ptr = &inventory[INVEN_FEET]; break;
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return (FALSE);

	if (o_ptr->tval < TV_BOOTS) return (FALSE);

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	/* Object resists */
	if (have_flag(flgs, TR_IGNORE_ACID))
	{
#ifdef JP
msg_format("しかし%sには効果がなかった！", o_name);
#else
		msg_format("Your %s is unaffected!", o_name);
#endif


		return (TRUE);
	}

	/* Message */
#ifdef JP
msg_format("%sがダメージを受けた！", o_name);
#else
	msg_format("Your %s is damaged!", o_name);
#endif


	/* Damage the item */
	o_ptr->to_a--;

	/* Calculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	/* Item was damaged */
	return (TRUE);
}


/*
 * Hurt the player with Acid
 */
int acid_dam(int dam, cptr kb_str)
{
	int get_damage;
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = p_ptr->oppose_acid;

	/* Total Immunity */
	if (p_ptr->immune_acid || (dam <= 0)) return 0;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_acid) dam = (dam + 2) / 3;
	if (double_resist) dam = (dam + 2) / 3;

	ACTIVATE_MULTISHADOW();
	if (!IS_MULTISHADOW(0))
	{
		if ((!(double_resist || p_ptr->resist_acid)) &&
		    one_in_(HURT_CHANCE))
			(void)do_dec_stat(A_CHR);

		/* If any armor gets hit, defend the player */
		if (minus_ac()) dam = (dam + 1) / 2;
	}

	/* Take damage */
	get_damage = take_hit(DAMAGE_ATTACK, dam, kb_str);

	/* Inventory damage */
	if (!(double_resist && p_ptr->resist_acid))
		inven_damage(set_acid_destroy, inv);

	STOP_MULTISHADOW();

	return get_damage;
}


/*
 * Hurt the player with electricity
 */
int elec_dam(int dam, cptr kb_str)
{
	int get_damage;
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = p_ptr->oppose_elec;

	/* Total immunity */
	if (p_ptr->immune_elec || (dam <= 0)) return 0;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_elec) dam = (dam + 2) / 3;
	if (double_resist) dam = (dam + 2) / 3;

	ACTIVATE_MULTISHADOW();
	if ((!(double_resist || p_ptr->resist_elec)) &&
	    one_in_(HURT_CHANCE) && !IS_MULTISHADOW(0))
		(void)do_dec_stat(A_DEX);

	/* Take damage */
	get_damage = take_hit(DAMAGE_ATTACK | DAMAGE_ELEC, dam, kb_str);

	/* Inventory damage */
	if (!(double_resist && p_ptr->resist_elec))
		inven_damage(set_elec_destroy, inv);

	STOP_MULTISHADOW();

	return get_damage;
}


/*
 * Hurt the player with Fire
 */
int fire_dam(int dam, cptr kb_str)
{
	int get_damage;
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = p_ptr->oppose_fire;

	/* Totally immune */
	if (p_ptr->immune_fire || (dam <= 0)) return 0;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_fire) dam = (dam + 2) / 3;
	if (double_resist) dam = (dam + 2) / 3;

	ACTIVATE_MULTISHADOW();
	if ((!(double_resist || p_ptr->resist_fire)) &&
	    one_in_(HURT_CHANCE) && !IS_MULTISHADOW(0))
		(void)do_dec_stat(A_STR);

	/* Take damage */
	get_damage = take_hit(DAMAGE_ATTACK, dam, kb_str);

	/* Inventory damage */
	if (!(double_resist && p_ptr->resist_fire))
		inven_damage(set_fire_destroy, inv);

	STOP_MULTISHADOW();

	return get_damage;
}


/*
 * Hurt the player with Cold
 */
int cold_dam(int dam, cptr kb_str)
{
	int get_damage;
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = p_ptr->oppose_cold;

	/* Total immunity */
	if (p_ptr->immune_cold || (dam <= 0)) return 0;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;
	if (p_ptr->zoshonel_protect) dam = dam * 3 / 2;

	/* Resist the damage */
	if (p_ptr->resist_cold) dam = (dam + 2) / 3;
	if (double_resist) dam = (dam + 2) / 3;

	ACTIVATE_MULTISHADOW();
	if ((!(double_resist || p_ptr->resist_cold)) &&
	    one_in_(HURT_CHANCE) && !IS_MULTISHADOW(0))
		(void)do_dec_stat(A_STR);

	/* Take damage */
	get_damage = take_hit(DAMAGE_ATTACK, dam, kb_str);

	/* Inventory damage */
	if (!(double_resist && p_ptr->resist_cold))
		inven_damage(set_cold_destroy, inv);

	STOP_MULTISHADOW();

	return get_damage;
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	int i;
	object_type *o_ptr;

	char o_name[MAX_NLEN];


	/* Nothing to curse */
	if (prace_is_(RACE_OCTOPUS)) return FALSE;

	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if ((o_ptr->art_name || artifact_p(o_ptr)) && (randint0(100) < 50))
	{
		/* Cool */
#ifdef JP
msg_format("%sが%sを包み込もうとしたが、%sはそれを跳ね返した！",
"恐怖の暗黒オーラ", "防具", o_name);
#else
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
#endif

	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
#ifdef JP
msg_format("恐怖の暗黒オーラがあなたの%sを包み込んだ！", o_name);
#else
		msg_format("A terrible black aura blasts your %s!", o_name);
#endif

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint1(5) - randint1(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		for (i = 0; i < A_MAX; i++) o_ptr->to_stat[i] = 0;
		for (i = 0; i < OB_MAX; i++) o_ptr->to_misc[i] = 0;
		for (i = 0; i < TR_FLAG_SIZE; i++) o_ptr->art_flags[i] = 0;

		/* Curse it */
		o_ptr->curse_flags = TRC_CURSED;

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(bool force, int slot)
{
	int i;

	object_type *o_ptr;

	char o_name[MAX_NLEN];


	/* Nothing to curse */
	if (prace_is_(RACE_OCTOPUS)) return FALSE;

	/* Curse the weapon */
	o_ptr = &inventory[slot];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if (((artifact_p(o_ptr) || o_ptr->art_name) && (randint0(100) < 50) && !force) ||
		object_is_astral_runeweapon(o_ptr) ||
		((dungeon_type == DUNGEON_HEAVEN) && (o_ptr->name1 == ART_BRUNHILD)))
	{
		/* Cool */
#ifdef JP
msg_format("%sが%sを包み込もうとしたが、%sはそれを跳ね返した！",
"恐怖の暗黒オーラ", "武器", o_name);
#else
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
#endif

	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
#ifdef JP
if (!force) msg_format("恐怖の暗黒オーラがあなたの%sを包み込んだ！", o_name);
#else
		if (!force) msg_format("A terrible black aura blasts your %s!", o_name);
#endif

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint1(5) - randint1(5);
		o_ptr->to_d = 0 - randint1(5) - randint1(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		for (i = 0; i < A_MAX; i++) o_ptr->to_stat[i] = 0;
		for (i = 0; i < OB_MAX; i++) o_ptr->to_misc[i] = 0;
		for (i = 0; i < TR_FLAG_SIZE; i++) o_ptr->art_flags[i] = 0;

		if ((slot == INVEN_RARM) && mw_diff_to_melee)
		{
			o_ptr->to_h += mw_diff_to_melee;
			o_ptr->to_d += mw_diff_to_melee;
		}


		/* Curse it */
		o_ptr->curse_flags = TRC_CURSED;

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	/* Notice */
	return (TRUE);
}


/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * Note that this function is one of the more "dangerous" ones...
 */
static s16b poly_r_idx(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	int i, r, lev1, lev2;

	/* Hack -- Uniques/Questors never polymorph */
	if ((r_ptr->flags1 & RF1_UNIQUE) ||
	    (r_ptr->flags1 & RF1_QUESTOR))
		return (r_idx);

	/* Allowable range of "levels" for resulting monster */
	lev1 = r_ptr->level - ((randint1(20) / randint1(9)) + 1);
	lev2 = r_ptr->level + ((randint1(20) / randint1(9)) + 1);

	/* Pick a (possibly new) non-unique race */
	for (i = 0; i < 1000; i++)
	{
		/* Pick a new race, using a level calculation */
		r = get_mon_num((dun_level + r_ptr->level) / 2 + 5);

		/* Handle failure */
		if (!r) break;

		/* Obtain race */
		r_ptr = &r_info[r];

		/* Ignore unique monsters */
		if (r_ptr->flags1 & RF1_UNIQUE) continue;

		/* Ignore monsters with incompatible levels */
		if ((r_ptr->level < lev1) || (r_ptr->level > lev2)) continue;

		/* Use that index */
		r_idx = r;

		/* Done */
		break;
	}

	/* Result */
	return (r_idx);
}


bool polymorph_monster(int y, int x, bool change_lnc)
{
	cave_type *c_ptr = &cave[y][x];
	monster_type *m_ptr = &m_list[c_ptr->m_idx];
	bool polymorphed = FALSE;
	int new_r_idx;
	int old_r_idx = m_ptr->r_idx;
	bool targeted = (target_who == c_ptr->m_idx) ? TRUE : FALSE;
	bool health_tracked = (p_ptr->health_who == c_ptr->m_idx) ? TRUE : FALSE;
	monster_type back_m;

	if (p_ptr->inside_arena) return (FALSE);

	if (p_ptr->riding == c_ptr->m_idx) return (FALSE);

	/* Memorize the monster before polymorphing */
	back_m = *m_ptr;

	/* Pick a "new" monster race */
	new_r_idx = poly_r_idx(old_r_idx);

	/* Handle polymorph */
	if (new_r_idx != old_r_idx)
	{
		u32b mode = PM_IGNORE_AMGRID;

		/* Get the monsters attitude */
		if (is_friendly(m_ptr)) mode |= PM_FORCE_FRIENDLY;
		if (is_pet(m_ptr)) mode |= PM_FORCE_PET;
		if (m_ptr->mflag2 & MFLAG_NOPET) mode |= PM_NO_PET;

		/* "Kill" the "old" monster */
		delete_monster_idx(c_ptr->m_idx);

		/* Create a new monster (no groups) */
		if (place_monster_aux(0, y, x, new_r_idx, mode))
		{
			/* Success */
			polymorphed = TRUE;
			if (change_lnc) change_your_alignment_lnc(-1);
		}
		else
		{
			/* Placing the new monster failed */
			if (place_monster_aux(0, y, x, old_r_idx, (mode | PM_IGNORE_TERRAIN)))
				m_list[hack_m_idx_ii] = back_m;
		}

		if (targeted) target_who = hack_m_idx_ii;
		if (health_tracked) health_track(hack_m_idx_ii);
	}

	return polymorphed;
}


/*
 * Dimension Door
 */
bool dimension_door(void)
{
	int	plev = p_ptr->lev;
	int	x = 0, y = 0;
	cave_type *c_ptr;
	bool allow_air = FALSE;

	if (!tgt_pt(&x, &y, FALSE)) return FALSE;
	c_ptr = &cave[y][x];

	p_ptr->energy_need += (s16b)((s32b)(60 - plev) * ENERGY_NEED() / 100L);

	if (c_ptr->feat == FEAT_AIR)
	{
		if (player_can_enter(FEAT_AIR)) allow_air = TRUE;
	}

	if ((!teleportable_grid(c_ptr) && !allow_air) ||
		(c_ptr->info & CAVE_ICKY) ||
		(distance(y, x, py, px) > plev / 2 + 10) ||
		(!randint0(plev / 10 + 10)))
	{
#ifdef JP
		msg_print("精霊界から物質界に戻る時うまくいかなかった！");
#else
		msg_print("You fail to exit the astral plane correctly!");
#endif
		p_ptr->energy_need += (s16b)((s32b)(60 - plev) * ENERGY_NEED() / 100L);
		teleport_player((plev+2)*2);
	}
	else
		teleport_player_to(y, x, TRUE, FALSE);

	return (TRUE);
}


/*
 * Aqua Diving
 */
bool aqua_diving(void)
{
	int	plev = p_ptr->lev;
	int	x = 0, y = 0;
	int	dist_diff;

	if ((cave[py][px].feat != FEAT_DEEP_WATER) &&
	    (cave[py][px].feat != FEAT_SHAL_WATER) &&
	    (cave[py][px].feat != FEAT_SWAMP))
	{
#ifdef JP
		msg_print("足元に水、もしくは沼地がありません。");
#else
		msg_print("You are not on water or swamp.");
#endif
		return FALSE;
	}

	if (!tgt_pt(&x, &y, FALSE)) return FALSE;

	if ((cave[y][x].feat != FEAT_DEEP_WATER) &&
	    (cave[y][x].feat != FEAT_SHAL_WATER) &&
	    (cave[y][x].feat != FEAT_SWAMP))
	{
#ifdef JP
		msg_print("そこは水または沼地ではありません。");
#else
		msg_print("There is neither water nor swamp.");
#endif
		return FALSE;
	}

	if ((cave[y][x].info & CAVE_ICKY) || cave[y][x].m_idx)
	{
#ifdef JP
		msg_print("潜水に失敗した。");
#else
		msg_print("You failed to dive.");
#endif
		return FALSE;
	}

	p_ptr->energy_need += (s16b)((s32b)(60 - plev) * ENERGY_NEED() / 100L);

	dist_diff = distance(y, x, py, px) - ((rp_ptr->r_flags & PRF_AQUATIC) ? (plev + 20) : (plev / 2 + 10));
	if (dist_diff > 0)
	{
#ifdef JP
		msg_print("距離が遠すぎて体に負荷がかかってしまった。");
#else
		msg_print("You are damaged by too long distance.");
#endif
#ifdef JP
		take_hit(DAMAGE_NOESCAPE, dist_diff * 40, "潜水による疲労");
#else
		take_hit(DAMAGE_NOESCAPE, dist_diff * 40, "the strain of aqua diving");
#endif
	}

	teleport_player_to(y, x, FALSE, FALSE);

	return (TRUE);
}


/*
 * Jump through the wall or monster
 */
bool jump_wall(void)
{
	int y, x, dir;
	bool failed = FALSE;

	if (!get_rep_dir2(&dir)) return FALSE;
	if (dir == 5) return FALSE;
	y = py + 2 * ddy[dir];
	x = px + 2 * ddx[dir];
	if (!in_bounds(y, x)) failed = TRUE;
	else if (!cave_empty_bold(y, x) || (cave[y][x].info & CAVE_ICKY)) failed = TRUE;
	else if (!player_can_enter(cave[y][x].feat)) failed = TRUE;
	else teleport_player_to(y, x, FALSE, FALSE);

	if (failed)
	{
#ifdef JP
		msg_print("ジャンプに失敗した。");
#else
		msg_print("You failed to jump through.");
#endif
	}

	return TRUE;
}


bool eat_magic(int power)
{
	object_type * o_ptr;
	object_kind *k_ptr;
	int lev, item;
	int recharge_strength = 0;

	bool fail = FALSE;
	byte fail_type = 1;

	cptr q, s;
	char o_name[MAX_NLEN];

	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
#ifdef JP
q = "どのアイテムから魔力を吸収しますか？";
s = "魔力を吸収できるアイテムがありません。";
#else
	q = "Drain which item? ";
	s = "You have nothing to drain.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	k_ptr = &k_info[o_ptr->k_idx];
	lev = get_object_level(o_ptr);

	if (o_ptr->tval == TV_ROD)
	{
		recharge_strength = ((power > lev/2) ? (power - lev/2) : 0) / 5;

		/* Back-fire */
		if (one_in_(recharge_strength))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}
		else
		{
			if (o_ptr->timeout > (o_ptr->number - 1) * k_ptr->pval)
			{
#ifdef JP
msg_print("充填中のロッドから魔力を吸収することはできません。");
#else
				msg_print("You can't absorb energy from a discharged rod.");
#endif

			}
			else
			{
				p_ptr->csp += lev;
				o_ptr->timeout += k_ptr->pval;
			}
		}
	}
	else
	{
		/* All staffs, wands. */
		recharge_strength = (100 + power - lev) / 15;

		/* Paranoia */
		if (recharge_strength < 0) recharge_strength = 0;

		/* Back-fire */
		if (one_in_(recharge_strength))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}
		else
		{
			if (o_ptr->pval > 0)
			{
				p_ptr->csp += lev / 2;
				o_ptr->pval --;

				/* XXX Hack -- unstack if necessary */
				if ((o_ptr->tval == TV_STAFF) && (item >= 0) && (o_ptr->number > 1))
				{
					object_type forge;
					object_type *q_ptr;

					/* Get local object */
					q_ptr = &forge;

					/* Obtain a local object */
					object_copy(q_ptr, o_ptr);

					/* Modify quantity */
					q_ptr->number = 1;

					/* Restore the charges */
					o_ptr->pval++;

					/* Unstack the used item */
					o_ptr->number--;
					p_ptr->total_weight -= q_ptr->weight;
					item = inven_carry(q_ptr);

					/* Message */
#ifdef JP
					msg_print("杖をまとめなおした。");
#else
					msg_print("You unstack your staff.");
#endif

				}
			}
			else
			{
#ifdef JP
msg_print("吸収できる魔力がありません！");
#else
				msg_print("There's no energy there to absorb!");
#endif

			}
			if (!o_ptr->pval) o_ptr->ident |= IDENT_EMPTY;
		}
	}

	/* Inflict the penalties for failing a recharge. */
	if (fail)
	{
		/* Artifacts are never destroyed. */
		if (artifact_p(o_ptr))
		{
			object_desc(o_name, o_ptr, TRUE, 0);
#ifdef JP
msg_format("魔力が逆流した！%sは完全に魔力を失った。", o_name);
#else
			msg_format("The recharging backfires - %s is completely drained!", o_name);
#endif


			/* Artifact rods. */
			if (o_ptr->tval == TV_ROD)
				o_ptr->timeout = k_ptr->pval * o_ptr->number;

			/* Artifact wands and staffs. */
			else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
				o_ptr->pval = 0;
		}
		else
		{
			/* Get the object description */
			object_desc(o_name, o_ptr, FALSE, 0);

			/*** Determine Seriousness of Failure ***/

			/* Mages recharge objects more safely. */
			if ((p_ptr->pclass == CLASS_WIZARD) || (p_ptr->pclass == CLASS_WARLOCK) || (p_ptr->pclass == CLASS_WITCH) || (p_ptr->pclass == CLASS_SIRENE) || (p_ptr->pclass == CLASS_LICH) || (p_ptr->pclass == CLASS_HIGHWITCH))
			{
				/* 10% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (one_in_(10)) fail_type = 2;
					else fail_type = 1;
				}
				/* 75% chance to blow up one wand, otherwise draining. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (!one_in_(3)) fail_type = 2;
					else fail_type = 1;
				}
				/* 50% chance to blow up one staff, otherwise no effect. */
				else if (o_ptr->tval == TV_STAFF)
				{
					if (one_in_(2)) fail_type = 2;
					else fail_type = 0;
				}
			}

			/* All other classes get no special favors. */
			else
			{
				/* 33% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (one_in_(3)) fail_type = 2;
					else fail_type = 1;
				}
				/* 20% chance of the entire stack, else destroy one wand. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (one_in_(5)) fail_type = 3;
					else fail_type = 2;
				}
				/* Blow up one staff. */
				else if (o_ptr->tval == TV_STAFF)
				{
					fail_type = 2;
				}
			}

			/*** Apply draining and destruction. ***/

			/* Drain object or stack of objects. */
			if (fail_type == 1)
			{
				if (o_ptr->tval == TV_ROD)
				{
#ifdef JP
msg_print("ロッドは破損を免れたが、魔力は全て失なわれた。");
#else
					msg_format("You save your rod from destruction, but all charges are lost.", o_name);
#endif

					o_ptr->timeout = k_ptr->pval * o_ptr->number;
				}
				else if (o_ptr->tval == TV_WAND)
				{
#ifdef JP
msg_format("%sは破損を免れたが、魔力が全て失われた。", o_name);
#else
					msg_format("You save your %s from destruction, but all charges are lost.", o_name);
#endif

					o_ptr->pval = 0;
				}
				/* Staffs aren't drained. */
			}

			/* Destroy an object or one in a stack of objects. */
			if (fail_type == 2)
			{
				if (o_ptr->number > 1)
				{
#ifdef JP
msg_format("乱暴な魔法のために%sが一本壊れた！", o_name);
#else
					msg_format("Wild magic consumes one of your %s!", o_name);
#endif

					/* Reduce rod stack maximum timeout, drain wands. */
					if (o_ptr->tval == TV_ROD) o_ptr->timeout = MIN(o_ptr->timeout, k_ptr->pval * (o_ptr->number - 1));
					else if (o_ptr->tval == TV_WAND) o_ptr->pval = o_ptr->pval * (o_ptr->number - 1) / o_ptr->number;

				}
				else
#ifdef JP
msg_format("乱暴な魔法のために%sが何本か壊れた！", o_name);
#else
					msg_format("Wild magic consumes your %s!", o_name);
#endif

				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}

			/* Destroy all members of a stack of objects. */
			if (fail_type == 3)
			{
				if (o_ptr->number > 1)
#ifdef JP
msg_format("乱暴な魔法のために%sが全て壊れた！", o_name);
#else
					msg_format("Wild magic consumes all your %s!", o_name);
#endif

				else
#ifdef JP
msg_format("乱暴な魔法のために%sが壊れた！", o_name);
#else
					msg_format("Wild magic consumes your %s!", o_name);
#endif



				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -999);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -999);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
		}
	}

	if (p_ptr->csp > p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
	}

	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->window |= (PW_INVEN);

	return TRUE;
}


bool summon_kin_player(int level, int y, int x, u32b mode)
{
	bool pet = (bool)(mode & PM_FORCE_PET);
	if (!pet) mode |= PM_NO_PET;

	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
			summon_kin_type = 'p';
			break;
		case RACE_HAWKMAN:
		case RACE_FAIRY:
			summon_kin_type = 'F';
			break;
		case RACE_LIZARDMAN:
			switch (randint1(13))
			{
			case 1: case 2: case 3: case 4: case 5: case 6: case 7: case 8:
				summon_kin_type = 'h';
				break;
			case 9: case 10: case 11: case 12:
				summon_kin_type = 'd';
				break;
			default:
				summon_kin_type = 'D';
				break;
			}
			break;
		case RACE_GREMLIN:
			summon_kin_type = 'u';
			break;
		case RACE_SKELETON:
			if (one_in_(13)) summon_kin_type = 'L';
			else summon_kin_type = 's';
			break;
		case RACE_GHOST:
			summon_kin_type = 'G';
			break;
		case RACE_PUMPKINHEAD:
		case RACE_GOBLIN:
			summon_kin_type = 'o';
			break;
		case RACE_GORGON:
			summon_kin_type = 'n';
			break;
		case RACE_MERMAID:
		case RACE_OCTOPUS:
			summon_kin_type = 'l';
			break;
		default:
			summon_kin_type = 'p';
			break;
	}
	switch (p_ptr->pclass)
	{
	case CLASS_LICH:
		summon_kin_type = 'L';
		break;
	case CLASS_ANGELKNIGHT:
		summon_kin_type = 'A';
		break;
	}
	return summon_specific((pet ? -1 : 0), y, x, level, SUMMON_KIN, mode);
}


/*
 * Apply reincarnation
 */
void reincarnation(void)
{
	int i;
	s32b tmp_long;
	char buf[80];

	dispel_player(TRUE);
	set_action(ACTION_NONE);

	for (i = 0; i < A_MAX; i++)
	{
		if (p_ptr->stat_max[i] > 18)
		{
			p_ptr->stat_max[i] -= 60;
			if (p_ptr->stat_max[i] < 18) p_ptr->stat_max[i] = 18;
		}

		p_ptr->stat_cur[i] = p_ptr->stat_max[i];
	}

	/* HP and mana are feedbacked 50% to level 1 */
	tmp_long = (s32b)p_ptr->player_ghp + (s32b)p_ptr->player_hp[p_ptr->max_plv - 1];
	if (tmp_long > 30000) tmp_long = 30000;
	tmp_long -= MIN(400, tmp_long / 2);
	p_ptr->player_ghp = tmp_long;

	tmp_long = (s32b)p_ptr->player_gsp + (s32b)p_ptr->player_sp[p_ptr->max_plv - 1];
	if (tmp_long > 30000) tmp_long = 30000;
	tmp_long -= MIN(200, tmp_long / 2);
	p_ptr->player_gsp = tmp_long;

	/* Skills are 20% cut */
	p_ptr->gx_dis = p_ptr->gx_dis * 80 / 100;
	p_ptr->gx_dev = p_ptr->gx_dev * 80 / 100;
	p_ptr->gx_sav = p_ptr->gx_sav * 80 / 100;
	p_ptr->gx_stl = p_ptr->gx_stl * 80 / 100;
	p_ptr->gx_srh = p_ptr->gx_srh * 80 / 100;
	p_ptr->gx_fos = p_ptr->gx_fos * 80 / 100;
	p_ptr->gx_spd = p_ptr->gx_spd * 80 / 100;
	p_ptr->gx_thn = p_ptr->gx_thn * 80 / 100;
	p_ptr->gx_thb = p_ptr->gx_thb * 80 / 100;

	/* Level reset! */
	p_ptr->max_plv = p_ptr->lev = 1;
	p_ptr->max_exp = p_ptr->exp = p_ptr->exp_frac = 0;
	p_ptr->skill_point = p_ptr->exp_for_sp = 0;

	/* Race reset (undeads only) */
	if (rp_ptr->r_flags & PRF_UNDEAD)
	{
		p_ptr->prace = RACE_HUMAN;
		rp_ptr = &race_info[p_ptr->prace];

		if (rp_ptr->r_flags & PCF_NO_DIGEST) p_ptr->food = PY_FOOD_FULL - 1;
	}

	/* Class reset */
	if (rp_ptr->r_flags & PRF_LARGE)
	{
		switch (p_ptr->prace)
		{
		case RACE_OCTOPUS:
			p_ptr->pclass = CLASS_OCTOPUS;
			break;
		}
	}
	else
	{
		p_ptr->pclass = (p_ptr->psex == SEX_MALE) ? CLASS_SOLDIER : CLASS_AMAZONESS;
	}
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &m_info[p_ptr->pclass];
	p_ptr->s_ptr = &s_info[p_ptr->pclass];

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Manadice */
	p_ptr->manadie = rp_ptr->r_msp + cp_ptr->c_msp;

	change_level99_quest(FALSE);

	p_ptr->player_hp[0] = p_ptr->player_sp[0] = 0;
	p_ptr->level_gained_class[0] = p_ptr->pclass;

	/* Paranoia */
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{
		p_ptr->player_hp[i] = p_ptr->player_sp[i] = 0;
		p_ptr->level_gained_class[i] = 0;
	}

	if (cp_ptr->c_flags & PCF_NO_DIGEST) p_ptr->food = PY_FOOD_FULL - 1;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
	if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

	/* Update stuff */
	p_ptr->update |= (PU_HP | PU_MANA);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MAP);

	redraw_stuff();

	/* Window stuff */
	p_ptr->window |= (PW_SPELL | PW_PLAYER);

#ifdef JP
	sprintf(buf, "リーンカーネイトの効果で転生した。");
#else
	sprintf(buf, "reincarnated into the new body.");
#endif

	do_cmd_write_nikki(NIKKI_BUNSHOU, 0, buf);

	if (p_ptr->reincarnate_cnt < MAX_SHORT) p_ptr->reincarnate_cnt++;

	p_ptr->expfact = MIN(rp_ptr->r_exp + 10 * p_ptr->reincarnate_cnt, 500);

	/* Load the "pref" files */
	load_all_pref_files();
}


/*
 * Erase magical effects on player
 */
void dispel_player(bool kill_sing)
{
	set_fast(0, TRUE);
	set_slow(0, TRUE);
	set_shield(0, TRUE);
	set_blessed(0, TRUE);
	set_hero(0, TRUE);
	set_shero(0, TRUE);
	set_protevil(0, TRUE);
	set_invuln(0, TRUE);
	set_wraith_form(0, TRUE);
	set_tim_res_time(0, TRUE);
	/* by henkma */
	set_multishadow(0, TRUE);
	set_dustrobe(0,TRUE);

	set_tim_invis(0, TRUE);
	set_tim_infra(0, TRUE);
	set_tim_esp(0, TRUE);
	set_tim_sh_fire(0, TRUE);
	set_tim_sh_elec(0, TRUE);
	set_tim_sh_cold(0, TRUE);
	set_tim_sh_holy(0, TRUE);
	set_tim_eyeeye(0, TRUE);
	set_tim_inc_blow(0, TRUE);
	set_tim_dec_blow(0, TRUE);
	set_zoshonel_protect(0, TRUE);
	set_tim_octopus_immunity(0, TRUE);
	set_magicdef(0, TRUE);
	set_oppose_acid(0, TRUE);
	set_oppose_elec(0, TRUE);
	set_oppose_fire(0, TRUE);
	set_oppose_cold(0, TRUE);
	set_oppose_pois(0, TRUE);
	set_magical_weapon(0, 0, INVEN_RARM, FALSE);
	set_evil_weapon(0, TRUE, INVEN_RARM, FALSE);
	set_the_immunity(0, 0);
	set_opposite_pelem(0);
	set_chargespell(0, TRUE);
	set_earth_spike(0, TRUE);
	set_wind_guard(0, TRUE);
	set_tim_resurrection(0, TRUE);
	set_tim_immune_magic(0, TRUE);

	/* Cancel glowing hands */
	if (p_ptr->special_attack & ATTACK_CONFUSE)
	{
		p_ptr->special_attack &= ~(ATTACK_CONFUSE);
#ifdef JP
		msg_print("手の輝きがなくなった。");
#else
		msg_print("Your hands stop glowing.");
#endif

	}
	if (p_ptr->singing)
	{
		if (kill_sing)
		{
			p_ptr->restart_singing = 0;
			p_ptr->song_start = 0;
#ifdef JP
			msg_print("歌が止まった。");
#else
			msg_print("Your singing is stopped.");
#endif
		}
		else
		{
			p_ptr->restart_singing = p_ptr->singing;
#ifdef JP
			msg_print("歌が途切れた。");
#else
			msg_print("Your singing is interrupted.");
#endif
		}

		if (p_ptr->singing == MUSIC_SILENT) song_of_silence(0);
		p_ptr->singing = MUSIC_NONE;
		p_ptr->action = ACTION_NONE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS | PU_HP);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP | PR_STATUS | PR_STATE);

		/* Update monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		p_ptr->energy_need += ENERGY_NEED();
	}
}

/*
 * Snap Dragon weapon base item table by class
 */
static byte snap_dragon_class_table[][2] =
{
	{TV_SWORD, SV_RUNEBLADE},   /* Soldier */
	{TV_POLEARM, SV_RUNESPEAR}, /* Knight */
	{TV_POLEARM, SV_RUNEAXE},   /* Berserker */
	{TV_HAFTED, SV_RUNEHAMMER}, /* Terror-Knight */
	{TV_HAFTED, SV_RUNEWHIP},   /* BeastTamer */
	{TV_SWORD, SV_RUNEBLADE},   /* Sword-Master */
	{TV_SWORD, SV_RUNEBLADE},   /* Dragoon */
	{TV_SWORD, SV_RUNECLAW},    /* Ninja */
	{TV_HAFTED, SV_RUNESTAFF},  /* Wizard */
	{TV_HAFTED, SV_RUNESTAFF},  /* Warlock */
	{TV_HAFTED, SV_RUNESTAFF},  /* Exorcist */
	{TV_SWORD, SV_RUNEBLADE},   /* Amazoness */
	{TV_POLEARM, SV_RUNESPEAR}, /* Valkyrie */
	{TV_BOW, SV_RUNEBOW},       /* Archer */
	{TV_HAFTED, SV_RUNEWHIP},   /* DragonTamer */
	{TV_HAFTED, SV_RUNESTAFF},  /* Witch */
	{TV_HAFTED, SV_RUNEFAN},    /* Sirene */
	{TV_HAFTED, SV_RUNESTAFF},  /* Cleric */
	{TV_HAFTED, SV_RUNESTAFF},  /* Priest */
	{TV_HAFTED, SV_RUNESTAFF},  /* Lich */
	{TV_POLEARM, SV_RUNESPEAR}, /* Angel-Knight */
	{TV_HAFTED, SV_RUNESTAFF},  /* High-Witch */
	{TV_BOW, SV_RUNEGUN},       /* Gunner */
	{TV_SWORD, SV_RUNEBLADE},   /* Temple-Knight */
	{TV_SWORD, SV_RUNEBLADE},   /* White-Knight */
	{0, 0},                     /* Octopus */
};

/*
 * Obtain Snap Dragon weapon flags by class
 */
static void snap_dragon_class_flags(object_type *o_ptr)
{
	/* Analyze the class */
	switch (p_ptr->pclass)
	{
	case CLASS_SOLDIER:
		add_flag(o_ptr->art_flags, TR_SLAY_ORC);
		add_flag(o_ptr->art_flags, TR_VORPAL);
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		break;

	case CLASS_KNIGHT:
		add_flag(o_ptr->art_flags, TR_SLAY_ORC);
		add_flag(o_ptr->art_flags, TR_SUST_STR);
		break;

	case CLASS_BERSERKER:
		add_flag(o_ptr->art_flags, TR_SLAY_TROLL);
		add_flag(o_ptr->art_flags, TR_SUST_CON);
		add_flag(o_ptr->art_flags, TR_REGEN);
		break;

	case CLASS_TERRORKNIGHT:
		add_flag(o_ptr->art_flags, TR_SUST_STR);
		add_flag(o_ptr->art_flags, TR_SUST_CON);
		add_flag(o_ptr->art_flags, TR_REGEN);
		add_flag(o_ptr->art_flags, TR_FEAR_FIELD);
		o_ptr->to_misc[OB_ANTI_MAGIC] = 3;
		break;

	case CLASS_BEASTTAMER:
		add_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
		add_flag(o_ptr->art_flags, TR_SUST_CHR);
		break;

	case CLASS_SWORDMASTER:
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		add_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
		if (one_in_(10)) add_flag(o_ptr->art_flags, TR_EXTRA_VORPAL);
		break;

	case CLASS_DRAGOON:
		add_flag(o_ptr->art_flags, TR_KILL_DRAGON);
		add_flag(o_ptr->art_flags, TR_SUST_STR);
		break;

	case CLASS_NINJA:
		add_flag(o_ptr->art_flags, TR_VORPAL);
		add_flag(o_ptr->art_flags, TR_BRAND_POIS);
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		break;

	case CLASS_WIZARD:
		add_flag(o_ptr->art_flags, TR_SLAY_TROLL);
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		if (one_in_(4)) add_flag(o_ptr->art_flags, TR_REGEN_MANA);
		break;

	case CLASS_WARLOCK:
		add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		break;

	case CLASS_EXORCIST:
		add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
		add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_BLESSED);
		break;

	case CLASS_AMAZONESS:
		add_flag(o_ptr->art_flags, TR_SLAY_ORC);
		add_flag(o_ptr->art_flags, TR_VORPAL);
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		break;

	case CLASS_VALKYRIE:
		add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
		add_flag(o_ptr->art_flags, TR_SUST_STR);
		break;

	case CLASS_ARCHER:
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		add_flag(o_ptr->art_flags, TR_XTRA_SHOTS);
		break;

	case CLASS_DRAGONTAMER:
		add_flag(o_ptr->art_flags, TR_SLAY_DRAGON);
		add_flag(o_ptr->art_flags, TR_SUST_CHR);
		break;

	case CLASS_WITCH:
		add_flag(o_ptr->art_flags, TR_SUST_CHR);
		add_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
		break;

	case CLASS_SIRENE:
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		if (one_in_(3)) add_flag(o_ptr->art_flags, TR_REGEN_MANA);
		break;

	case CLASS_CLERIC:
		add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
		add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_BLESSED);
		break;

	case CLASS_PRIEST:
		add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
		add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_BLESSED);
		break;

	case CLASS_LICH:
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		add_flag(o_ptr->art_flags, TR_FREE_ACT);
		add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
		add_flag(o_ptr->art_flags, TR_RES_COLD);
		add_flag(o_ptr->art_flags, TR_RES_POIS);
		add_flag(o_ptr->art_flags, TR_RES_NETHER);
		add_flag(o_ptr->art_flags, TR_UNHOLY);
		add_flag(o_ptr->art_flags, TR_SLAY_LIVING);
		add_flag(o_ptr->art_flags, TR_SEE_INVIS);
		add_flag(o_ptr->art_flags, TR_SLOW_DIGEST);
		break;

	case CLASS_ANGELKNIGHT:
		add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
		add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_RES_FEAR);
		add_flag(o_ptr->art_flags, TR_RES_CONF);
		add_flag(o_ptr->art_flags, TR_FEATHER);
		add_flag(o_ptr->art_flags, TR_SEE_INVIS);
		add_flag(o_ptr->art_flags, TR_BLESSED);
		break;

	case CLASS_HIGHWITCH:
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_DEC_MANA);
		add_flag(o_ptr->art_flags, TR_RES_MAGIC);
		if (one_in_(2)) add_flag(o_ptr->art_flags, TR_EASY_SPELL);
		o_ptr->to_a = -20;
		break;

	case CLASS_GUNNER:
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		add_flag(o_ptr->art_flags, TR_RES_SOUND);
		add_flag(o_ptr->art_flags, TR_RES_SHARDS);
		add_flag(o_ptr->art_flags, TR_NO_TELE);
		break;

	case CLASS_TEMPLEKNIGHT:
		add_flag(o_ptr->art_flags, TR_SLAY_ORC);
		add_flag(o_ptr->art_flags, TR_SLAY_TROLL);
		add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		break;

	case CLASS_WHITEKNIGHT:
		add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
		add_flag(o_ptr->art_flags, TR_VORPAL);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		break;
	}
}

/*
 * Obtain Snap Dragon weapon flags by race
 */
static void snap_dragon_race_flags(object_type *o_ptr)
{
	switch (p_ptr->prace)
	{
	case RACE_HUMAN:
		add_flag(o_ptr->art_flags, TR_SUST_STR);
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		add_flag(o_ptr->art_flags, TR_SUST_CON);
		add_flag(o_ptr->art_flags, TR_SUST_CHR);
		break;

	case RACE_HAWKMAN:
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		add_flag(o_ptr->art_flags, TR_RES_FEAR);
		add_flag(o_ptr->art_flags, TR_FEATHER);
		break;

	case RACE_LIZARDMAN:
		add_flag(o_ptr->art_flags, TR_SLAY_TROLL);
		add_flag(o_ptr->art_flags, TR_SUST_STR);
		add_flag(o_ptr->art_flags, TR_RES_ACID);
		add_flag(o_ptr->art_flags, TR_RES_COLD);
		break;

	case RACE_FAIRY:
		add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
		add_flag(o_ptr->art_flags, TR_SUST_CHR);
		add_flag(o_ptr->art_flags, TR_RES_LITE);
		add_flag(o_ptr->art_flags, TR_FEATHER);
		add_flag(o_ptr->art_flags, TR_LITE);
		break;

	case RACE_GREMLIN:
		add_flag(o_ptr->art_flags, TR_VAMPIRIC);
		add_flag(o_ptr->art_flags, TR_SUST_DEX);
		add_flag(o_ptr->art_flags, TR_RES_DARK);
		add_flag(o_ptr->art_flags, TR_RES_NETHER);
		add_flag(o_ptr->art_flags, TR_SLAY_LIVING);
		add_flag(o_ptr->art_flags, TR_FEATHER);
		break;

	case RACE_SKELETON:
		add_flag(o_ptr->art_flags, TR_SLAY_DRAGON);
		add_flag(o_ptr->art_flags, TR_SUST_CON);
		add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
		add_flag(o_ptr->art_flags, TR_RES_COLD);
		add_flag(o_ptr->art_flags, TR_RES_POIS);
		add_flag(o_ptr->art_flags, TR_RES_SHARDS);
		add_flag(o_ptr->art_flags, TR_SEE_INVIS);
		break;

	case RACE_GHOST:
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		add_flag(o_ptr->art_flags, TR_FREE_ACT);
		add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
		add_flag(o_ptr->art_flags, TR_RES_COLD);
		add_flag(o_ptr->art_flags, TR_RES_POIS);
		add_flag(o_ptr->art_flags, TR_RES_NETHER);
		add_flag(o_ptr->art_flags, TR_SLAY_LIVING);
		add_flag(o_ptr->art_flags, TR_FEATHER);
		add_flag(o_ptr->art_flags, TR_SEE_INVIS);
		if (one_in_(2)) add_flag(o_ptr->art_flags, TR_TELEPATHY);
		if (one_in_(5)) add_flag(o_ptr->art_flags, TR_WRAITH);
		break;

	case RACE_PUMPKINHEAD:
		add_flag(o_ptr->art_flags, TR_CHAOTIC);
		add_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
		add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
		add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
		add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
		add_flag(o_ptr->art_flags, TR_SUST_WIS);
		add_flag(o_ptr->art_flags, TR_REFLECT);
		add_flag(o_ptr->art_flags, TR_FREE_ACT);
		add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
		add_flag(o_ptr->art_flags, TR_RES_CHAOS);
		add_flag(o_ptr->art_flags, TR_RES_DISEN);
		add_flag(o_ptr->art_flags, TR_TELEPATHY);
		add_flag(o_ptr->art_flags, TR_SLOW_DIGEST);
		add_flag(o_ptr->art_flags, TR_AGGRAVATE);
		break;

	case RACE_GOBLIN:
		add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
		add_flag(o_ptr->art_flags, TR_SUST_CON);
		add_flag(o_ptr->art_flags, TR_RES_DARK);
		break;

	case RACE_GORGON:
		add_flag(o_ptr->art_flags, TR_SUST_INT);
		add_flag(o_ptr->art_flags, TR_REFLECT);
		add_flag(o_ptr->art_flags, TR_RES_STONE);
		if (!one_in_(5)) add_flag(o_ptr->art_flags, TR_RES_COLD);
		if (randint1(5) >= 3) add_flag(o_ptr->art_flags, TR_RES_ACID);
		if (randint1(5) >= 4) add_flag(o_ptr->art_flags, TR_RES_CHAOS);
		if (one_in_(5)) add_flag(o_ptr->art_flags, TR_RES_POIS);
		if (one_in_(10)) add_flag(o_ptr->art_flags, TR_RES_NETHER);
		break;

	case RACE_MERMAID:
		add_flag(o_ptr->art_flags, TR_CHAOTIC);
		add_flag(o_ptr->art_flags, TR_SUST_CHR);
		add_flag(o_ptr->art_flags, TR_RES_ACID);
		if (one_in_(2)) add_flag(o_ptr->art_flags, TR_RES_CONF);
		break;
	}
};

/*
 * Snap Dragon - Change player into a weapon
 * Note: Player "dies" after this.
 */
void snap_dragon(void)
{
	runeweapon_type *runeweapon = &runeweapon_list[0];
	object_type     *q_ptr;
	int             i;
	char            new_name[MAX_NLEN];
	char            dummy_name[80] = "";
	bool            old_total_winner = p_ptr->total_winner;
	bool            string_input;
	s16b            tmp_hp, tmp_sp;

	if (rp_ptr->r_flags & PRF_LARGE)
	{
#ifdef JP
		msg_print("何も起こらなかった。");
#else
		msg_print("Nothing happens.");
#endif
		return;
	}

#ifdef JP
	msg_print("あなたは生きたまま武器に変化する...");
#else
	msg_print("You turn into a living weapon...");
#endif

	(void)WIPE(runeweapon, runeweapon_type);

	/* Memorize dungeon depth or town */
	strcpy(runeweapon->ancestor, player_name);
	for (i = 0; i < 4; i++) strcpy(runeweapon->history[i], p_ptr->history[i]);

	tmp_hp = (s16b)MIN((s32b)p_ptr->player_hp[p_ptr->max_plv - 1] + (s32b)p_ptr->player_ghp, 30000);
	tmp_sp = (s16b)MIN((s32b)p_ptr->player_sp[p_ptr->max_plv - 1] + (s32b)p_ptr->player_gsp, 30000);

	runeweapon->hp = tmp_hp;
	runeweapon->sp = tmp_sp;
	runeweapon->level = p_ptr->max_max_plv;
	runeweapon->reincarnate_cnt = p_ptr->reincarnate_cnt;
	runeweapon->race = p_ptr->prace;
	runeweapon->elem = p_ptr->pelem;
	if (p_ptr->noscore || easy_band)
		runeweapon->status = RW_STATUS_ILLEGAL;
	else runeweapon->status = RW_STATUS_NONE;

	/* Get global object */
	q_ptr = &runeweapon->weapon;

	/* Prepare a "rune" weapon */
	object_prep(q_ptr, lookup_kind(snap_dragon_class_table[p_ptr->pclass][0],
		snap_dragon_class_table[p_ptr->pclass][1]));

	if (q_ptr->tval != TV_BOW)
	{
		q_ptr->dd = p_ptr->max_max_plv / 10;
		q_ptr->ds = (byte)MIN(p_ptr->max_max_exp / 1000000L, 255);
	}
	else
	{
		runeweapon->bow_energy = (s16b)MAX(12000 - (p_ptr->max_max_exp / 10000000L) * 1000, 1000);
		runeweapon->bow_tmul = p_ptr->max_max_plv / 10;
	}
	q_ptr->to_h = tmp_sp / 20;
	q_ptr->to_d = tmp_hp / 20;

	/* Determine "bonus" */
	for (i = 0; i < A_MAX; i++)
	{
		int cur_stat = p_ptr->stat_max[i];

		if (cur_stat >= (18 + 200)) q_ptr->to_stat[i] = 6;
		else if (cur_stat >= (18 + 190)) q_ptr->to_stat[i] = 5;
		else if (cur_stat >= (18 + 180)) q_ptr->to_stat[i] = 4;
		else if (cur_stat >= (18 + 170)) q_ptr->to_stat[i] = 3;
		else if (cur_stat >= (18 + 160)) q_ptr->to_stat[i] = 2;
		else if (cur_stat >= (18 + 150)) q_ptr->to_stat[i] = 1;
	}
	if (p_ptr->stat_max[A_STR] >= (18 + 150)) q_ptr->to_misc[OB_TUNNEL] = q_ptr->to_stat[A_STR];
	if (p_ptr->stat_max[A_INT] >= (18 + 150)) q_ptr->to_misc[OB_SEARCH] = q_ptr->to_stat[A_INT];
	if (p_ptr->stat_max[A_WIS] >= (18 + 150)) q_ptr->to_misc[OB_STEALTH] = q_ptr->to_stat[A_WIS];
	if (p_ptr->stat_max[A_DEX] >= (18 + 150)) q_ptr->to_misc[OB_SPEED] = q_ptr->to_stat[A_DEX];
	if (p_ptr->stat_max[A_CON] >= (18 + 150)) q_ptr->to_misc[OB_INFRA] = q_ptr->to_stat[A_CON];

	/* Flags determined by alignment (GNE, LNC) */
	runeweapon->align = SUB_ALIGN_NEUTRAL;
	switch (get_your_alignment_gne())
	{
	case ALIGN_GNE_GOOD:
		add_flag(q_ptr->art_flags, TR_SLAY_EVIL);
		runeweapon->align |= (SUB_ALIGN_GOOD);
		break;

	case ALIGN_GNE_EVIL:
		add_flag(q_ptr->art_flags, TR_SLAY_GOOD);
		runeweapon->align |= (SUB_ALIGN_EVIL);
		break;
	}
	switch (get_your_alignment_lnc())
	{
	case ALIGN_LNC_LAWFUL:
		runeweapon->align |= (SUB_ALIGN_LAWFUL);
		break;

	case ALIGN_LNC_CHAOTIC:
		runeweapon->align |= (SUB_ALIGN_CHAOTIC);
		break;
	}

	/* Flags determined by element */
	switch (p_ptr->pelem)
	{
	case ELEM_FIRE:
		add_flag(q_ptr->art_flags, TR_BRAND_FIRE);
		add_flag(q_ptr->art_flags, TR_RES_FIRE);
		if (one_in_(3)) add_flag(q_ptr->art_flags, TR_IM_FIRE);
		break;

	case ELEM_AQUA:
		add_flag(q_ptr->art_flags, TR_BRAND_COLD);
		add_flag(q_ptr->art_flags, TR_RES_COLD);
		if (one_in_(3)) add_flag(q_ptr->art_flags, TR_IM_COLD);
		break;

	case ELEM_EARTH:
		add_flag(q_ptr->art_flags, TR_BRAND_ACID);
		add_flag(q_ptr->art_flags, TR_RES_ACID);
		if (one_in_(3)) add_flag(q_ptr->art_flags, TR_IM_ACID);
		break;

	case ELEM_WIND:
		add_flag(q_ptr->art_flags, TR_BRAND_ELEC);
		add_flag(q_ptr->art_flags, TR_RES_ELEC);
		if (one_in_(3)) add_flag(q_ptr->art_flags, TR_IM_ELEC);
		break;
	}

	/* Flags determined by class */
	snap_dragon_class_flags(q_ptr);
	if (cp_ptr->c_to_a > 0) q_ptr->to_a = cp_ptr->c_to_a;

	/* Flags determined by race */
	snap_dragon_race_flags(q_ptr);

	/* Random resistance or ability */
	for (i = randint1(3); i; i--)
	{
		if (one_in_(2)) one_resistance(q_ptr);
		else one_ability(q_ptr);
	}

	if (q_ptr->tval == TV_BOW)
	{
		remove_flag(q_ptr->art_flags, TR_FORCE_WEAPON);
		remove_flag(q_ptr->art_flags, TR_CHAOTIC);
		remove_flag(q_ptr->art_flags, TR_VAMPIRIC);
		remove_flag(q_ptr->art_flags, TR_SLAY_ANIMAL);
		remove_flag(q_ptr->art_flags, TR_SLAY_EVIL);
		remove_flag(q_ptr->art_flags, TR_SLAY_UNDEAD);
		remove_flag(q_ptr->art_flags, TR_SLAY_DEMON);
		remove_flag(q_ptr->art_flags, TR_SLAY_ORC);
		remove_flag(q_ptr->art_flags, TR_SLAY_TROLL);
		remove_flag(q_ptr->art_flags, TR_SLAY_GIANT);
		remove_flag(q_ptr->art_flags, TR_SLAY_DRAGON);
		remove_flag(q_ptr->art_flags, TR_KILL_DRAGON);
		remove_flag(q_ptr->art_flags, TR_VORPAL);
		remove_flag(q_ptr->art_flags, TR_IMPACT);
		remove_flag(q_ptr->art_flags, TR_BRAND_POIS);
		remove_flag(q_ptr->art_flags, TR_BRAND_ACID);
		remove_flag(q_ptr->art_flags, TR_BRAND_ELEC);
		remove_flag(q_ptr->art_flags, TR_BRAND_FIRE);
		remove_flag(q_ptr->art_flags, TR_BRAND_COLD);
		remove_flag(q_ptr->art_flags, TR_SLAY_GOOD);
		remove_flag(q_ptr->art_flags, TR_SLAY_HUMAN);
		remove_flag(q_ptr->art_flags, TR_SLAY_LIVING);
	}

	object_aware(q_ptr);
	object_known(q_ptr);
	q_ptr->ident |= (IDENT_MENTAL);

	(void)screen_object(q_ptr, NULL, TRUE);

#ifdef JP
	string_input = get_string("この武器を何と名付けますか？", dummy_name, 80);
#else
	string_input = get_string("What do you want to call the weapon? ", dummy_name, 80);
#endif
	if (string_input && strlen(dummy_name))
	{
#ifdef JP
		strcpy(new_name, "『");
#else
		strcpy(new_name, "'");
#endif

		strcat(new_name, dummy_name);
#ifdef JP
		strcat(new_name, "』");
#else
		strcat(new_name, "'");
#endif
	}
	else
	{
#ifdef JP
		sprintf(new_name, "%s%sの", cp_ptr->title, player_name);
#else
		sprintf(new_name, "of %s the %s", player_name, cp_ptr->title);
#endif
	}

	/* Save the inscription */
	q_ptr->art_name = quark_add(new_name);

	/* Note death */
	p_ptr->is_dead |= DEATH_SNAP_DRAGON;
#ifdef JP
	take_hit(DAMAGE_USELIFE, 0, "スナップドラゴン使用");
#else
	take_hit(DAMAGE_USELIFE, 0, "using Snap Dragon");
#endif
	/* Winner is still winner */
	p_ptr->total_winner = old_total_winner;
}

typedef struct wish_result_type wish_result_type;

struct wish_result_type
{
	object_type obj;
	struct wish_result_type *next;
};

static void add_wish_result(wish_result_type **top, object_type *o_ptr)
{
	wish_result_type *new_result, *cur;

	MAKE(new_result, wish_result_type);

	object_copy(&new_result->obj, o_ptr);

	if (*top)
	{
		for (cur = *top; cur->next; cur = cur->next)
			;
		cur->next = new_result;
	}
	else
	{
		*top = new_result;
	}
}

static int tval_to_slot(byte tval)
{
	switch (tval)
	{
	case TV_BULLET:
	case TV_ROUND:
	case TV_SHELL:
	case TV_ROCKET:
	case TV_ARROW:
	case TV_BOLT:
		return INVEN_AMMO;

	case TV_BOW:
		return INVEN_BOW;

	case TV_DIGGING:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
		return INVEN_RARM;

	case TV_BOOTS:
		return INVEN_FEET;

	case TV_GLOVES:
		return INVEN_HANDS;

	case TV_HELM:
		return INVEN_HEAD;

	case TV_CROWN:
		return INVEN_HEAD;

	case TV_SHIELD:
		return INVEN_LARM;

	case TV_CLOAK:
		return INVEN_OUTER;

	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
		return INVEN_BODY;

	case TV_LITE:
		return INVEN_LITE;

	case TV_AMULET:
		return INVEN_NECK;

	case TV_RING:
		return INVEN_RIGHT;

	default:
		return 0;
	}
}

/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 * Hack -- note special rating boost for dragon scale mail
 */
static bool wish_a_m_aux_1(object_type *o_ptr, int level, bool do_wish)
{
	int tohit1 = randint1(5) + m_bonus(5, level);
	int todam1 = randint1(5) + m_bonus(5, level);

	int tohit2 = m_bonus(10, level);
	int todam2 = m_bonus(10, level);

	if ((o_ptr->tval >= TV_BULLET) && (o_ptr->tval <= TV_BOLT))
	{
		tohit2 = (tohit2+1)/2;
		todam2 = (todam2+1)/2;
	}

	if (o_ptr->name2 && !e_info[o_ptr->name2].cost)
	{
		if (do_wish)
		{
			/* Penalize */
			o_ptr->to_h -= tohit1;
			o_ptr->to_d -= todam1;
		}

		/* Penalize again */
		o_ptr->to_h -= tohit2;
		o_ptr->to_d -= todam2;
	}
	else
	{
		if (do_wish)
		{
			/* Enchant */
			o_ptr->to_h += tohit1;
			o_ptr->to_d += todam1;
		}

		if (o_ptr->name2 || (o_ptr->tval == TV_ROCKET) || ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DIAMOND_EDGE)))
		{
			/* Enchant again */
			o_ptr->to_h += tohit2;
			o_ptr->to_d += todam2;
		}
	}

	/* Analyze type */
	switch (o_ptr->tval)
	{
	case TV_BULLET:
	case TV_ROUND:
	case TV_SHELL:
	case TV_ARROW:
	case TV_BOLT:
		switch (o_ptr->name2)
		{
		case EGO_SLAYING_BOLT:
			o_ptr->dd++;
			break;
		}

		if (o_ptr->name2 && e_info[o_ptr->name2].cost)
		{
			/* Hack -- super-charge the damage dice */
			while (one_in_(10L * o_ptr->dd * o_ptr->ds)) o_ptr->dd++;

			/* Hack -- restrict the damage dice */
			if (o_ptr->dd > 9) o_ptr->dd = 9;
		}
		break;

	case TV_ROCKET:
		if (o_ptr->name2) return FALSE;
		break;

	case TV_BOW:
		switch (o_ptr->name2)
		{
		case EGO_EXTRA_MIGHT:
			if (o_ptr->sval == SV_ROCKET_LAUNCHER) return FALSE;
			break;
		case EGO_BALDAR_BOW:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		}
		break;

	case TV_DIGGING:
		switch (o_ptr->name2)
		{
		case 0:
		case EGO_DIGGING:
		case EGO_SHATTERED:
			break;
		default:
			return FALSE;
		}
		break;

	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
		if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DIAMOND_EDGE))
		{
			if (o_ptr->name2 && (o_ptr->name2 != EGO_SHATTERED)) return FALSE;
		}

		switch (o_ptr->name2)
		{
		case EGO_HA:
			if (one_in_(4) && (level > 40))
			{
				o_ptr->to_misc[OB_BLOWS]++;
				if ((level > 60) && one_in_(3) && ((o_ptr->dd*(o_ptr->ds+1)) < 15)) o_ptr->to_misc[OB_BLOWS]++;
			}
			break;
		case EGO_DF:
			if (one_in_(3)) add_flag(o_ptr->art_flags, TR_RES_POIS);
			if (one_in_(3)) add_flag(o_ptr->art_flags, TR_WARNING);
			break;
		case EGO_SHOOTING_STAR:
			if (!shooting_star_generation_okay(o_ptr)) return FALSE;
			break;
		case EGO_DRAGOON:
			if (one_in_(3)) add_flag(o_ptr->art_flags, TR_RES_POIS);
			break;
		case EGO_LODIS:
			if (one_in_(3)) add_flag(o_ptr->art_flags, TR_RES_FEAR);
			break;
		case EGO_SLAYING_WEAPON:
			if (one_in_(3)) o_ptr->dd *= 2; /* double damage */
			else
			{
				do
				{
					o_ptr->dd++;
				}
				while (one_in_(o_ptr->dd));
				
				do
				{
					o_ptr->ds++;
				}
				while (one_in_(o_ptr->ds));
			}

			if (one_in_(5)) add_flag(o_ptr->art_flags, TR_BRAND_POIS);
			if ((o_ptr->tval == TV_SWORD) && one_in_(3)) add_flag(o_ptr->art_flags, TR_VORPAL);
			if (one_in_(200)) o_ptr->to_misc[OB_ANTI_MAGIC] = 3;
			break;
		case EGO_ASMODE:
			if (one_in_(7)) one_ability(o_ptr);
			break;
		case EGO_ISHTALLE:
			if (one_in_(3)) add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
			if (one_in_(5)) add_flag(o_ptr->art_flags, TR_RES_FEAR);
			break;
		case EGO_DIGGING:
			return FALSE;
		case EGO_SHARPNESS:
			if (o_ptr->tval != TV_SWORD) return FALSE;
			o_ptr->to_misc[OB_TUNNEL] = m_bonus(5, level) + 1;
			if (one_in_(20)) add_flag(o_ptr->art_flags, TR_EXTRA_VORPAL);
			break;
		case EGO_EARTHQUAKES:
			if (o_ptr->tval != TV_HAFTED) return FALSE;
			o_ptr->to_misc[OB_TUNNEL] = m_bonus(3, level);
			if (one_in_(3) && (level > 60)) o_ptr->to_misc[OB_BLOWS] = randint1(2);
			break;
		case EGO_VAMPIRIC:
			if (one_in_(5)) add_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
			break;
		case EGO_MORGUL:
			if (one_in_(6)) add_flag(o_ptr->art_flags, TR_TY_CURSE);
			if (one_in_(500)) add_flag(o_ptr->art_flags, TR_WRAITH);
			if (one_in_(100)) add_flag(o_ptr->art_flags, TR_RES_MAGIC);
			if (one_in_(30)) add_flag(o_ptr->art_flags, TR_FEAR_FIELD);
			if ((o_ptr->tval == TV_SWORD) && one_in_(50)) add_flag(o_ptr->art_flags, TR_EXTRA_VORPAL);
			break;
		case EGO_RIPPER:
			if (o_ptr->tval == TV_HAFTED) return FALSE;
			if (one_in_(3))
			{
				switch (randint1(3))
				{
				case 1:
					add_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
					break;
				case 2:
					add_flag(o_ptr->art_flags, TR_SLAY_ORC);
					break;
				default:
					add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
					break;
				}
			}
			break;
		case EGO_BALDAR_WEAPON:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		}

		if (o_ptr->name2 && e_info[o_ptr->name2].cost)
		{
			/* Hack -- Super-charge the damage dice */
			while (one_in_(10L * o_ptr->dd * o_ptr->ds)) o_ptr->dd++;

			/* Hack -- Lower the damage dice */
			if (o_ptr->dd > 9) o_ptr->dd = 9;
		}
		break;
	}

	if (o_ptr->name2 == EGO_SHATTERED)
	{
		int i;

		o_ptr->to_h = 0 - randint1(5) - randint1(5);
		o_ptr->to_d = 0 - randint1(5) - randint1(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		for (i = 0; i < A_MAX; i++) o_ptr->to_stat[i] = 0;
		for (i = 0; i < OB_MAX; i++) o_ptr->to_misc[i] = 0;
		for (i = 0; i < TR_FLAG_SIZE; i++) o_ptr->art_flags[i] = 0;
		o_ptr->curse_flags = TRC_CURSED;
		o_ptr->ident |= (IDENT_BROKEN);
	}

	return TRUE;
}

/*
 * Apply magic to an item known to be "armor"
 *
 * Hack -- note special processing for crown/helm
 * Hack -- note special processing for robe of permanence
 */
static bool wish_a_m_aux_2(object_type *o_ptr, int level, bool do_wish)
{
	int toac1 = randint1(5) + m_bonus(5, level);
	int toac2 = m_bonus(10, level);

	/* Bolmarkan Cloak and Black Clothes ... */
	if (do_wish)
	{
		if ((o_ptr->tval == TV_CLOAK) && (o_ptr->sval == SV_BOLMARKAN_CLOAK))
			o_ptr->to_misc[OB_SEARCH] = randint1(4);
		if ((o_ptr->tval == TV_SOFT_ARMOR) && (o_ptr->sval == SV_KUROSHOUZOKU))
			o_ptr->to_misc[OB_STEALTH] = randint1(4);
	}

	if (o_ptr->name2 && !e_info[o_ptr->name2].cost)
	{
		/* Penalize */
		if (do_wish) o_ptr->to_a -= toac1;

		/* Penalize again */
		o_ptr->to_a -= toac2;
	}
	else
	{
		/* Enchant */
		if (do_wish) o_ptr->to_a += toac1;

		if (o_ptr->name2 || (o_ptr->tval == TV_DRAG_ARMOR))
		{
			/* Enchant again */
			o_ptr->to_a += toac2;
		}
	}

	/* Analyze type */
	switch (o_ptr->tval)
	{
	case TV_BOOTS:
		if ((o_ptr->sval == SV_PAIR_OF_DRAGON_GREAVE) && do_wish)
			dragon_resist(o_ptr);

		switch (o_ptr->name2)
		{
		case EGO_SLOW_DESCENT:
			if (one_in_(2)) one_high_resistance(o_ptr);
			break;
		case EGO_BALDAR_BOOTS:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		}
		break;

	case TV_GLOVES:
		if ((o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) && do_wish)
			dragon_resist(o_ptr);

		switch (o_ptr->name2)
		{
		case EGO_BALDAR_GLOVES:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		}
		break;

	case TV_HELM:
		if ((o_ptr->sval == SV_DRAGON_HELM) && do_wish)
			dragon_resist(o_ptr);

		switch (o_ptr->name2)
		{
		case EGO_LITE:
		case EGO_INFRAVISION:
			break;
		case EGO_SEEING:
			if (one_in_(7)) add_flag(o_ptr->art_flags, TR_TELEPATHY);
			break;
		case EGO_BALDAR_HELM:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		default:/* not existing helm (Magi, Might, etc...)*/
			return FALSE;
		}
		break;

	case TV_CROWN:
		switch (o_ptr->name2)
		{
		case EGO_MAGI:
		case EGO_MIGHT:
		case EGO_TELEPATHY:
		case EGO_REGENERATION:
		case EGO_LORDLINESS:
			break;
		case EGO_SEEING:
			if (one_in_(3)) add_flag(o_ptr->art_flags, TR_TELEPATHY);
			break;
		default:/* not existing crown (wisdom,lite, etc...) */
			return FALSE;
		}
		break;

	case TV_SHIELD:
		if ((o_ptr->sval == SV_DRAGON_SHIELD) && do_wish)
			dragon_resist(o_ptr);

		switch (o_ptr->name2)
		{
		case EGO_ENDURANCE:
			if (!one_in_(3)) one_high_resistance(o_ptr);
			if (one_in_(4)) add_flag(o_ptr->art_flags, TR_RES_POIS);
			break;
		case EGO_REFLECTION:
			if (o_ptr->sval == SV_MIRROR_SHIELD) return FALSE;
			break;
		case EGO_BALDAR_SHIELD:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		}
		break;

	case TV_CLOAK:
		switch (o_ptr->name2)
		{
		case EGO_BAT:
			o_ptr->to_d -= 6;
			o_ptr->to_h -= 6;
			break;
		case EGO_SIRENE:
			{
				object_kind *k_ptr = &k_info[o_ptr->k_idx];
				if (have_flag(k_ptr->flags, TR_MALE_ONLY)) return FALSE;
				if (one_in_(8)) add_flag(o_ptr->art_flags, TR_EASY_SPELL);
			}
			break;
		}
		break;

	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
		switch (o_ptr->name2)
		{
		case EGO_RESISTANCE:
			if (one_in_(4)) add_flag(o_ptr->art_flags, TR_RES_POIS);
			break;
		case EGO_GAMP:
			if (o_ptr->tval != TV_HARD_ARMOR) return FALSE;
			o_ptr->weight = (2 * k_info[o_ptr->k_idx].weight / 3);
			o_ptr->ac = k_info[o_ptr->k_idx].ac + 5;
			break;
		case EGO_PERMANENCE:
			if ((o_ptr->tval != TV_SOFT_ARMOR) || (o_ptr->sval != SV_ROBE)) return FALSE;
			break;
		case EGO_ARCH_MAGI:
			if ((o_ptr->tval != TV_SOFT_ARMOR) || (o_ptr->sval != SV_ROBE_OF_MAGE)) return FALSE;
			break;
		case EGO_BALDAR_ARMOR:
			if (!baldar_generation_okay(o_ptr)) return FALSE;
			o_ptr->weight = o_ptr->weight * 6 / 5;
			break;
		}
		break;

	case TV_DRAG_ARMOR:
		if (o_ptr->name2 && (o_ptr->name2 != EGO_BLASTED)) return FALSE;
		break;
	}

	if (o_ptr->name2 == EGO_BLASTED)
	{
		int i;

		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint1(5) - randint1(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		for (i = 0; i < A_MAX; i++) o_ptr->to_stat[i] = 0;
		for (i = 0; i < OB_MAX; i++) o_ptr->to_misc[i] = 0;
		for (i = 0; i < TR_FLAG_SIZE; i++) o_ptr->art_flags[i] = 0;
		o_ptr->curse_flags = TRC_CURSED;
		o_ptr->ident |= (IDENT_BROKEN);
	}

	return TRUE;
}

/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special rating boost for ring of speed
 * Hack -- note special rating boost for amulet of the magi
 * Hack -- note special "bonus boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
static bool wish_a_m_aux_3(object_type *o_ptr, int level, bool do_wish)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
	case TV_RING:
		if (do_wish)
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				case SV_RING_ATTACKS:
				{
					/* Stat bonus */
					o_ptr->to_misc[OB_BLOWS] = m_bonus(2, level);
					if (one_in_(15)) o_ptr->to_misc[OB_BLOWS]++;
					if (o_ptr->to_misc[OB_BLOWS] < 1) o_ptr->to_misc[OB_BLOWS] = 1;

					break;
				}

				/* Ring of Speed! */
				case SV_RING_SPEED:
				{
					/* Base speed (1 to 10) */
					o_ptr->to_misc[OB_SPEED] = randint1(5) + m_bonus(5, level);

					/* Super-charge the ring */
					while (randint0(100) < 50) o_ptr->to_misc[OB_SPEED]++;

					break;
				}

				case SV_RING_LORDLY:
				{
					do
					{
						one_lordly_high_resistance(o_ptr);
					}
					while (one_in_(4));

					/* Bonus to armor class */
					o_ptr->to_a = 10 + randint1(5) + m_bonus(10, level);
				}
				break;

				/* Searching */
				case SV_RING_SEARCHING:
				{
					/* Bonus to searching */
					o_ptr->to_misc[OB_SEARCH] = 1 + m_bonus(5, level);

					break;
				}

				/* Flames, Acid, Ice */
				case SV_RING_FLAMES:
				case SV_RING_ACID:
				case SV_RING_ICE:
				case SV_RING_ELEC:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5 + randint1(5) + m_bonus(10, level);
					break;
				}

				/* Weakness */
				case SV_RING_WEAKNESS:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->curse_flags |= TRC_CURSED;

					/* Penalize */
					o_ptr->to_stat[A_STR] = 0 - (1 + m_bonus(5, level));

					break;
				}

				/* Stupidity */
				case SV_RING_STUPIDITY:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->curse_flags |= TRC_CURSED;

					/* Penalize */
					o_ptr->to_stat[A_INT] = 0 - (1 + m_bonus(5, level));

					break;
				}

				/* WOE */
				case SV_RING_WOE:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->curse_flags |= TRC_CURSED;

					/* Penalize */
					o_ptr->to_a = 0 - (5 + m_bonus(10, level));
					o_ptr->to_stat[A_WIS] = 0 - (5 + m_bonus(10, level));

					break;
				}

				/* Ring of damage */
				case SV_RING_DAMAGE:
				{
					/* Bonus to damage */
					o_ptr->to_d = 1 + randint1(5) + m_bonus(16, level);

					break;
				}

				/* Ring of Accuracy */
				case SV_RING_ACCURACY:
				{
					/* Bonus to hit */
					o_ptr->to_h = 1 + randint1(5) + m_bonus(16, level);

					break;
				}

				/* Ring of Protection */
				case SV_RING_PROTECTION:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5 + randint1(8) + m_bonus(10, level);

					break;
				}

				/* Ring of Slaying */
				case SV_RING_SLAYING:
				{
					/* Bonus to damage and to hit */
					o_ptr->to_d = randint1(5) + m_bonus(12, level);
					o_ptr->to_h = randint1(5) + m_bonus(12, level);

					break;
				}

				case SV_RING_AGGRAVATION:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->curse_flags |= TRC_CURSED;

					break;
				}
			}
		}

		if (o_ptr->name2 && !e_info[o_ptr->name2].cost)
		{
			int i;

			for (i = 0; i < A_MAX; i++)
			{
				if (o_ptr->to_stat[i] > 0) o_ptr->to_stat[i] = 0 - o_ptr->to_stat[i];
			}
			for (i = 0; i < OB_MAX; i++)
			{
				if (o_ptr->to_misc[i] > 0) o_ptr->to_misc[i] = 0 - o_ptr->to_misc[i];
			}
			if (o_ptr->to_h > 0) o_ptr->to_h = 0 - o_ptr->to_h;
			if (o_ptr->to_d > 0) o_ptr->to_d = 0 - o_ptr->to_d;
			if (o_ptr->to_a > 0) o_ptr->to_a = 0 - o_ptr->to_a;
			o_ptr->art_flags[0] = 0L;
			o_ptr->art_flags[1] = 0L;
		}

		switch (o_ptr->name2)
		{
		case EGO_RING_HERO:
		case EGO_RING_MAGIC_MIS:
		case EGO_RING_MANA_BOLT:
		case EGO_RING_MANA_BALL:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_ACTIVATE)) return FALSE;
			break;

		case EGO_RING_SLAY:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if ((o_ptr->to_h) || (o_ptr->to_d)) return FALSE;
			break;

		case EGO_RING_SUPER_AC:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_PROTECTION) return FALSE;
			o_ptr->to_a += 7 + m_bonus(5, level);
			break;

		case EGO_RING_FIRE_BOLT:
		case EGO_RING_FIRE_BALL:
		case EGO_RING_DRAGON_F:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if ((o_ptr->name2 == EGO_RING_DRAGON_F) && (o_ptr->sval == SV_RING_FLAMES)) break;
			if (have_flag(k_ptr->flags, TR_ACTIVATE)) return FALSE;
			if (!(have_flag(k_ptr->flags, TR_RES_FIRE)) && (have_flag(k_ptr->flags, TR_RES_COLD) || have_flag(k_ptr->flags, TR_RES_ELEC) || have_flag(k_ptr->flags, TR_RES_ACID))) return FALSE;
			break;

		case EGO_RING_COLD_BOLT:
		case EGO_RING_COLD_BALL:
		case EGO_RING_DRAGON_C:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if ((o_ptr->name2 == EGO_RING_DRAGON_C) && (o_ptr->sval == SV_RING_ICE)) break;
			if (have_flag(k_ptr->flags, TR_ACTIVATE)) return FALSE;
			if (!(have_flag(k_ptr->flags, TR_RES_COLD)) && (have_flag(k_ptr->flags, TR_RES_FIRE) || have_flag(k_ptr->flags, TR_RES_ELEC) || have_flag(k_ptr->flags, TR_RES_ACID))) return FALSE;
			break;

		case EGO_RING_ELEC_BOLT:
		case EGO_RING_ELEC_BALL:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_ACTIVATE)) return FALSE;
			if (!(have_flag(k_ptr->flags, TR_RES_ELEC)) && (have_flag(k_ptr->flags, TR_RES_COLD) || have_flag(k_ptr->flags, TR_RES_FIRE) || have_flag(k_ptr->flags, TR_RES_ACID))) return FALSE;
			break;

		case EGO_RING_ACID_BOLT:
		case EGO_RING_ACID_BALL:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_ACTIVATE)) return FALSE;
			if (!(have_flag(k_ptr->flags, TR_RES_ACID)) && (have_flag(k_ptr->flags, TR_RES_COLD) || have_flag(k_ptr->flags, TR_RES_ELEC) || have_flag(k_ptr->flags, TR_RES_FIRE))) return FALSE;
			break;

		case EGO_RING_D_SPEED:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_SPEED) return FALSE;
			break;

		case EGO_RING_BERSERKER:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			switch (o_ptr->sval)
			{
			case SV_RING_DAMAGE:
			case SV_RING_ACCURACY:
			case SV_RING_SLAYING:
				o_ptr->to_h -= 2+randint1(4);
				o_ptr->to_d += 2+randint1(4);
				break;
			default:
				return FALSE;
			}
			break;

		case EGO_RING_HUNTER:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_SHOTS) return FALSE;
			break;

		case EGO_RING_THROW:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			break;

		case EGO_RING_REGEN:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_REGEN)) return FALSE;
			break;

		case EGO_RING_LITE:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_LITE)) return FALSE;
			break;

		case EGO_RING_M_DETECT:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_WARNING) return FALSE;
			break;

		case EGO_RING_STEALTH:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_SEARCHING) return FALSE;
			o_ptr->to_misc[OB_STEALTH] = o_ptr->to_misc[OB_SEARCH];
			break;

		case EGO_RING_TELE_AWAY:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_TELEPORTATION) return FALSE;
			break;

		case EGO_RING_TO_H:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->to_h) return FALSE;
			break;

		case EGO_RING_TO_D:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->to_d) return FALSE;
			break;

		case EGO_RING_RES_LITE:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_RES_BLINDNESS) return FALSE;
			break;

		case EGO_RING_RES_DARK:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_RES_BLINDNESS) return FALSE;
			break;

		case EGO_RING_WIZARD:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if ((have_flag(k_ptr->flags, TR_STR)) || o_ptr->to_h || o_ptr->to_d) return FALSE;
			break;

		case EGO_RING_DRAIN_EXP:
			if (have_flag(k_ptr->flags, TR_DRAIN_EXP)) return FALSE;
			break;

		case EGO_RING_AGGRAVATE:
			if (have_flag(k_ptr->flags, TR_AGGRAVATE)) return FALSE;
			break;

		case EGO_RING_TY_CURSE:
			if (have_flag(k_ptr->flags, TR_TY_CURSE)) return FALSE;
			break;

		case EGO_RING_RES_TIME:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_RING_SUSTAIN) return FALSE;
			break;

		case EGO_RING_TELEPORT:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_TELEPORT)) return FALSE;
			break;
		}
		break;

	case TV_AMULET:
		if (do_wish)
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				case SV_AMULET_RESISTANCE:
				{
					if (one_in_(5)) one_high_resistance(o_ptr);
					if (one_in_(5)) add_flag(o_ptr->art_flags, TR_RES_POIS);
				}
				break;

				/* Amulet of searching */
				case SV_AMULET_SEARCHING:
				{
					o_ptr->to_misc[OB_SEARCH] = randint1(2) + m_bonus(4, level);
					break;
				}

				/* Amulet of the Magi -- never cursed */
				case SV_AMULET_THE_MAGI:
				{
					int bonus = randint1(5) + m_bonus(5, level);

					o_ptr->to_misc[OB_SEARCH] = bonus;
					o_ptr->to_misc[OB_INFRA] = bonus;
					o_ptr->to_a = randint1(5) + m_bonus(5, level);

					break;
				}

				/* Amulet of Doom -- always cursed */
				case SV_AMULET_DOOM:
				{
					int penalty = 0 - (randint1(10) + m_bonus(10, level));

					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->curse_flags |= (TRC_CURSED);

					/* Penalize */
					o_ptr->to_stat[A_STR] = penalty;
					o_ptr->to_stat[A_INT] = penalty;
					o_ptr->to_stat[A_WIS] = penalty;
					o_ptr->to_stat[A_DEX] = penalty;
					o_ptr->to_stat[A_CON] = penalty;
					o_ptr->to_stat[A_CHR] = penalty;
					o_ptr->to_a = 0 - (randint1(5) + m_bonus(5, level));

					break;
				}

				case SV_AMULET_MAGIC_MASTERY:
				{
					o_ptr->to_misc[OB_MAGIC_MASTERY] = 1 + m_bonus(4, level);
					break;
				}

				case SV_AMULET_FOL:
				case SV_AMULET_OHN:
				case SV_AMULET_SOL:
				case SV_AMULET_VAN:
				{
					if (o_ptr->name2) return FALSE;
				}
			}
		}

		if (o_ptr->name2 && !e_info[o_ptr->name2].cost)
		{
			int i;

			for (i = 0; i < A_MAX; i++)
			{
				if (o_ptr->to_stat[i] > 0) o_ptr->to_stat[i] = 0 - o_ptr->to_stat[i];
			}
			for (i = 0; i < OB_MAX; i++)
			{
				if (o_ptr->to_misc[i] > 0) o_ptr->to_misc[i] = 0 - o_ptr->to_misc[i];
			}
			if (o_ptr->to_h > 0) o_ptr->to_h = 0 - o_ptr->to_h;
			if (o_ptr->to_d > 0) o_ptr->to_d = 0 - o_ptr->to_d;
			if (o_ptr->to_a > 0) o_ptr->to_a = 0 - o_ptr->to_a;
			o_ptr->art_flags[0] = 0L;
			o_ptr->art_flags[1] = 0L;
		}

		switch (o_ptr->name2)
		{
		case EGO_AMU_SLOW_D:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_SLOW_DIGEST)) return FALSE;
			break;

		case EGO_AMU_INFRA:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_INFRA)) return FALSE;
			break;

		case EGO_AMU_SEE_INVIS:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_SEE_INVIS)) return FALSE;
			break;

		case EGO_AMU_HOLD_LIFE:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_HOLD_LIFE)) return FALSE;
			break;

		case EGO_AMU_DRAIN_EXP:
			if (have_flag(k_ptr->flags, TR_DRAIN_EXP)) return FALSE;
			break;

		case EGO_AMU_AGGRAVATE:
			if (have_flag(k_ptr->flags, TR_AGGRAVATE)) return FALSE;
			break;

		case EGO_AMU_TY_CURSE:
			if (have_flag(k_ptr->flags, TR_TY_CURSE)) return FALSE;
			break;

		case EGO_AMU_AC:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			break;

		case EGO_AMU_ANTI_MAGIC:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_AMULET_NO_MAGIC) return FALSE;
			break;

		case EGO_AMU_STEALTH:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_AMULET_SEARCHING) return FALSE;
			o_ptr->to_misc[OB_STEALTH] = o_ptr->to_misc[OB_SEARCH];
			break;

		case EGO_AMU_JUMP:
		case EGO_AMU_TELEPORT:
		case EGO_AMU_D_DOOR:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_AMULET_TELEPORT) return FALSE;
			break;

		case EGO_AMU_DEFENDER:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_AMULET_RESISTANCE) return FALSE;
			break;

		case EGO_AMU_RES_FIRE:
		case EGO_AMU_RES_FIRE_:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_RES_FIRE)) return FALSE;
			break;

		case EGO_AMU_RES_COLD:
		case EGO_AMU_RES_COLD_:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_RES_COLD)) return FALSE;
			break;

		case EGO_AMU_RES_ELEC:
		case EGO_AMU_RES_ELEC_:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_RES_ELEC)) return FALSE;
			break;

		case EGO_AMU_RES_ACID:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_RES_ACID)) return FALSE;
			break;

		case EGO_AMU_RES_ACID_:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if ((have_flag(k_ptr->flags, TR_RES_ACID)) && (o_ptr->sval != SV_AMULET_RESIST_ACID)) return FALSE;
			break;

		case EGO_AMU_LEVITATION:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (have_flag(k_ptr->flags, TR_FEATHER)) return FALSE;
			break;

		case EGO_AMU_GREAT:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_AMULET_THE_MAGI) return FALSE;
			break;

		case EGO_AMU_DETECTION:
			if (o_ptr->ident & IDENT_BROKEN) return FALSE;
			if (o_ptr->sval != SV_AMULET_TELEPATHY) return FALSE;
			break;
		}
		break;
	}

	return TRUE;
}

/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static bool wish_a_m_aux_4(object_type *o_ptr, bool do_wish)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
	case TV_CHEST:
		{
			byte obj_level = get_object_level(o_ptr);

			if (o_ptr->name2) return FALSE;

			/* Hack -- skip ruined chests */
			if (obj_level <= 0) break;

			/* Hack -- pick a "difficulty" */
			o_ptr->pval = randint1(obj_level);

			o_ptr->xtra3 = dun_level + 5;

			/* Mega-Hack -- Disarmed level 127 chest */
			o_ptr->pval = -127;
		}
		break;

	case TV_LITE:
		/* Hack -- Wishing item must not be wished */
		if (o_ptr->sval == SV_LITE_MAGICAL_LAMP) return FALSE;

		/* Hack -- Torches & Lanterns -- max fuel */
		if (((o_ptr->sval == SV_LITE_TORCH) || (o_ptr->sval == SV_LITE_LANTERN)) && do_wish)
		{
			if (o_ptr->pval > 0) o_ptr->xtra4 = o_ptr->pval;
			o_ptr->pval = 0;
		}

		switch (o_ptr->name2)
		{
		case EGO_LITE_LONG:
			if (o_ptr->sval == SV_LITE_FEANOR) return FALSE;
			break;
		case EGO_LITE_DARKNESS:
			o_ptr->xtra4 = 0;
			break;
		}
		break;

	case TV_STAFF:
	case TV_WAND:
	case TV_ROD:
		if (o_ptr->name2) return FALSE;

		/* Transfer the pval. -LM- */
		o_ptr->pval = k_ptr->pval;
		break;

	case TV_FLASK:
		if (o_ptr->name2) return FALSE;

		o_ptr->xtra4 = o_ptr->pval;
		o_ptr->pval = 0;
		break;
	}

	return TRUE;
}

/*
 * Complete the "creation" of an object by applying "magic" to the item
 * (For wishing)
 */
static bool wish_apply_magic(object_type *o_ptr, bool do_wish)
{
	int level, i;

	if (do_wish) level = 127;
	else level = p_ptr->lev + max_dlv[DUNGEON_PALACE];

	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Extract the fields */
		o_ptr->pval = a_ptr->pval;
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;
		for (i = 0; i < A_MAX; i++) o_ptr->to_stat[i] = a_ptr->to_stat[i];
		for (i = 0; i < OB_MAX; i++) o_ptr->to_misc[i] = a_ptr->to_misc[i];
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		o_ptr->to_d = a_ptr->to_d;
		o_ptr->weight = a_ptr->weight;

		/* Hack -- extract the "cursed" flag */
		if (a_ptr->gen_flags & TRG_CURSED) o_ptr->curse_flags |= (TRC_CURSED);
		if (a_ptr->gen_flags & TRG_HEAVY_CURSE) o_ptr->curse_flags |= (TRC_HEAVY_CURSE);
		if (a_ptr->gen_flags & TRG_PERMA_CURSE) o_ptr->curse_flags |= (TRC_PERMA_CURSE);
		if (a_ptr->gen_flags & (TRG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
		if (a_ptr->gen_flags & (TRG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
		if (a_ptr->gen_flags & (TRG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);

		random_artifact_resistance(o_ptr, a_ptr);

		return TRUE;
	}

	/* Apply magic */
	switch (o_ptr->tval)
	{
	case TV_BULLET:
	case TV_ROUND:
	case TV_SHELL:
	case TV_ROCKET:
	case TV_ARROW:
	case TV_BOLT:
	case TV_BOW:
	case TV_DIGGING:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
		if (!wish_a_m_aux_1(o_ptr, level, do_wish)) return FALSE;
		break;

	case TV_BOOTS:
	case TV_GLOVES:
	case TV_HELM:
	case TV_CROWN:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_DRAG_ARMOR:
		if (!wish_a_m_aux_2(o_ptr, level, do_wish)) return FALSE;
		break;

	case TV_AMULET:
	case TV_RING:
		if (!wish_a_m_aux_3(o_ptr, level, do_wish)) return FALSE;
		break;

	default:
		if (!wish_a_m_aux_4(o_ptr, do_wish)) return FALSE;
		break;
	}

	/* Analyze ego-items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		/* Hack -- acquire "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->gen_flags & TRG_CURSED) o_ptr->curse_flags |= (TRC_CURSED);
		if (e_ptr->gen_flags & TRG_HEAVY_CURSE) o_ptr->curse_flags |= (TRC_HEAVY_CURSE);
		if (e_ptr->gen_flags & TRG_PERMA_CURSE) o_ptr->curse_flags |= (TRC_PERMA_CURSE);
		if (e_ptr->gen_flags & (TRG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
		if (e_ptr->gen_flags & (TRG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
		if (e_ptr->gen_flags & (TRG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);

		if (e_ptr->gen_flags & (TRG_ONE_SUSTAIN)) one_sustain(o_ptr);
		if (e_ptr->gen_flags & (TRG_XTRA_POWER)) one_ability(o_ptr);
		if (e_ptr->gen_flags & (TRG_XTRA_H_RES)) one_high_resistance(o_ptr);
		if (e_ptr->gen_flags & (TRG_XTRA_E_RES)) one_ele_resistance(o_ptr);
		if (e_ptr->gen_flags & (TRG_XTRA_D_RES)) one_dragon_ele_resistance(o_ptr);
		if (e_ptr->gen_flags & (TRG_XTRA_L_RES)) one_lordly_high_resistance(o_ptr);
		if (e_ptr->gen_flags & (TRG_XTRA_RES)) one_resistance(o_ptr);

		/* Hack -- apply extra penalties if needed */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
		{
			switch (o_ptr->name2)
			{
			case EGO_SHATTERED:
			case EGO_BLASTED:
				break;
			default:
				/* Hack -- obtain bonuses */
				for (i = 0; i < A_MAX; i++)
				{
					if (e_ptr->max_to_stat[i]) o_ptr->to_stat[i] -= randint1(e_ptr->max_to_stat[i]);
				}
				for (i = 0; i < OB_MAX; i++)
				{
					if (e_ptr->max_to_misc[i]) o_ptr->to_misc[i] -= randint1(e_ptr->max_to_misc[i]);
				}
				if (e_ptr->max_to_h) o_ptr->to_h -= randint1(e_ptr->max_to_h);
				if (e_ptr->max_to_d) o_ptr->to_d -= randint1(e_ptr->max_to_d);
				if (e_ptr->max_to_a) o_ptr->to_a -= randint1(e_ptr->max_to_a);
				break;
			}
		}

		/* Hack -- apply extra bonuses if needed */
		else
		{
			/* Hack -- obtain bonuses */
			for (i = 0; i < A_MAX; i++)
			{
				if (e_ptr->max_to_stat[i])
				{
					if (e_ptr->max_to_stat[i] > 127)
						o_ptr->to_stat[i] -= randint1(256-e_ptr->max_to_stat[i]);
					else o_ptr->to_stat[i] += randint1(e_ptr->max_to_stat[i]);
				}
			}
			for (i = 0; i < OB_MAX; i++)
			{
				if (e_ptr->max_to_misc[i])
				{
					if (e_ptr->max_to_misc[i] > 127)
						o_ptr->to_misc[i] -= randint1(256-e_ptr->max_to_misc[i]);
					else o_ptr->to_misc[i] += randint1(e_ptr->max_to_misc[i]);
				}
			}
			if (e_ptr->max_to_h)
			{
				if (e_ptr->max_to_h > 127)
					o_ptr->to_h -= randint1(256-e_ptr->max_to_h);
				else o_ptr->to_h += randint1(e_ptr->max_to_h);
			}
			if (e_ptr->max_to_d)
			{
				if (e_ptr->max_to_d > 127)
					o_ptr->to_d -= randint1(256-e_ptr->max_to_d);
				else o_ptr->to_d += randint1(e_ptr->max_to_d);
			}
			if (e_ptr->max_to_a)
			{
				if (e_ptr->max_to_a > 127)
					o_ptr->to_a -= randint1(256-e_ptr->max_to_a);
				else o_ptr->to_a += randint1(e_ptr->max_to_a);
			}

			switch (o_ptr->name2)
			{
			case EGO_GAMP:
				if (one_in_(4)) o_ptr->to_stat[A_CON] += randint1(e_ptr->max_to_stat[A_STR]);
				break;
			case EGO_SPEED:
				if (level < 50) o_ptr->to_misc[OB_SPEED] = randint1(o_ptr->to_misc[OB_SPEED]);
				break;
			case EGO_ATTACKS:
				o_ptr->to_misc[OB_BLOWS] = randint1(e_ptr->max_to_misc[OB_BLOWS]*level/100+1);
				if (o_ptr->to_misc[OB_BLOWS] > e_ptr->max_to_misc[OB_BLOWS]) o_ptr->to_misc[OB_BLOWS] = e_ptr->max_to_misc[OB_BLOWS];
				break;
			case EGO_AMU_ANTI_MAGIC:
				o_ptr->to_misc[OB_ANTI_MAGIC] = e_ptr->max_to_misc[OB_ANTI_MAGIC];
				break;
			}
		}

		/* Done */
		return TRUE;
	}

	/* Examine real objects */
	if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Hack -- acquire "broken" flag */
		if (!get_object_cost(o_ptr)) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->gen_flags & (TRG_CURSED)) o_ptr->curse_flags |= (TRC_CURSED);
		if (k_ptr->gen_flags & (TRG_HEAVY_CURSE)) o_ptr->curse_flags |= TRC_HEAVY_CURSE;
		if (k_ptr->gen_flags & (TRG_PERMA_CURSE)) o_ptr->curse_flags |= TRC_PERMA_CURSE;
		if (k_ptr->gen_flags & (TRG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
		if (k_ptr->gen_flags & (TRG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
		if (k_ptr->gen_flags & (TRG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);
	}

	return TRUE;
}

/*
 * Wish an object (**** Balance Breaker!? ****)
 */
bool wish_object(cptr err_msg)
{
	char w_o_name[MAX_NLEN] = "";
	char o_name[MAX_NLEN];
	int i, j, z;

	artifact_type *a_ptr;
	ego_item_type *e_ptr;
	monster_race  *r_ptr;
	object_kind   *k_ptr;

	byte *e_found;
	s16b r_idx_found = 0;

	int e_num = 0;

	object_type forge;
	object_type *q_ptr;

	bool only_artifact = FALSE;

	wish_result_type *w_list_top = NULL;
	wish_result_type *cur_w_list0, *cur_w_list1;

	bool wished = FALSE;

#ifdef JP
	if (!(get_string("願うアイテムの名前は？", w_o_name, MAX_NLEN)))
#else
	if (!(get_string("What object name do you wish? ", w_o_name, MAX_NLEN)))
#endif
		return FALSE;

	if (!w_o_name[0]) return FALSE;

	/* Get local object */
	q_ptr = &forge;

	C_MAKE(e_found, max_e_idx, byte);

	/**** Step 1: Scan artifacts ****/

	if (prefix(w_o_name, "★")) only_artifact = TRUE;

	for (i = 1; i < max_a_idx; i++)
	{
		a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		if (a_ptr->gen_flags & TRG_QUESTITEM) continue;

		/* Obtain the base object type */
		z = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Real object */
		if (z)
		{
			/* Create fake object */
			object_prep(q_ptr, z);

			/* Save the name */
			q_ptr->name1 = i;

			/* Describe the artifact */
			object_desc_store(o_name, q_ptr, FALSE, 0);

			if (streq(w_o_name, o_name + (only_artifact ? 0 : 2))) add_wish_result(&w_list_top, q_ptr);
		}
	}

	if (only_artifact) goto cleanup;

	/**** Step 2: Scan ego names ****/

	e_found[e_num++] = 0;
	for (i = 1; i < max_e_idx; i++)
	{
		e_ptr = &e_info[i];

		/* Skip "empty" ego */
		if (!e_ptr->name) continue;

		if (prefix(w_o_name, e_name + e_ptr->name)) e_found[e_num++] = i;
	}

	/**** Step 3: Scan monster race ****/

	for (i = 1; i < max_r_idx; i++)
	{
		r_ptr = &r_info[i];

		/* Skip "empty" monsters */
		if (!r_ptr->name) continue;

		if (r_ptr->flags7 & RF7_EGG_ONLY) continue;

		if (prefix(w_o_name, r_name + r_ptr->name))
		{
			if (!r_idx_found) r_idx_found = i;
			else
			{
				monster_race *s_ptr;

				s_ptr = &r_info[r_idx_found];

				if (strlen(r_name + r_ptr->name) > strlen(r_name + s_ptr->name)) r_idx_found = i;
				else if (streq(r_name + r_ptr->name, r_name + s_ptr->name))
				{
					if (r_ptr->level > s_ptr->level) r_idx_found = i;
				}
			}
		}
	}

	/**** Step 4: Scan base items ****/

	for (i = 1; i < max_k_idx; i++)
	{
		k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		if (k_ptr->gen_flags & (TRG_QUESTITEM | TRG_INSTA_ART | TRG_RUNEWEAPON)) continue;

		switch (k_ptr->tval)
		{
		case TV_TAROT:
		case TV_GOLD:
			continue;

		case TV_FIGURINE:
		case TV_STATUE:
		case TV_CORPSE:
			r_ptr = &r_info[r_idx_found];

			if ((k_ptr->tval == TV_FIGURINE) &&
				((r_ptr->flags1 & RF1_QUESTOR) || (r_ptr->flags7 & RF7_GUARDIAN) ||
				((r_ptr->flags1 & RF1_UNIQUE) && (!r_ptr->max_num || r_ptr->cur_num))))
				continue;
			if ((k_ptr->tval == TV_CORPSE) && r_idx_found)
			{
				if ((r_ptr->flags1 & RF1_UNIQUE) && r_ptr->max_num) continue;
				if ((k_ptr->sval == SV_SKELETON) && !(r_ptr->flags9 & RF9_DROP_SKELETON)) continue;
				if ((k_ptr->sval == SV_CORPSE) && !(r_ptr->flags9 & RF9_DROP_CORPSE)) continue;
			}

			object_prep(q_ptr, i);
			q_ptr->pval = r_idx_found;
			object_desc_store(o_name, q_ptr, FALSE, 0);
			if (streq(w_o_name, o_name)) add_wish_result(&w_list_top, q_ptr);
			break;

		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_ROCKET:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
			for (j = 0; j < e_num; j++)
			{
				if (j)
				{
					if (e_info[e_found[j]].slot != tval_to_slot(k_ptr->tval)) continue;
				}

				object_prep(q_ptr, i);
				q_ptr->name2 = e_found[j];
				if (!wish_apply_magic(q_ptr, TRUE)) continue;
				object_desc_store(o_name, q_ptr, FALSE, 0);
				if (streq(w_o_name, o_name)) add_wish_result(&w_list_top, q_ptr);
			}
			break;

		case TV_MAGERY_BOOK:
		case TV_FIRE_BOOK:
		case TV_AQUA_BOOK:
		case TV_EARTH_BOOK:
		case TV_WIND_BOOK:
		case TV_HOLY_BOOK:
		case TV_DEATH_BOOK:
		case TV_SYMBIOTIC_BOOK:
		case TV_WITCH_BOOK:
		case TV_DRAKONITE_BOOK:
		case TV_CRUSADE_BOOK:
			object_prep(q_ptr, i);
			object_desc_store(o_name, q_ptr, FALSE, 0);
			if (streq(w_o_name, o_name)) add_wish_result(&w_list_top, q_ptr);
			else if (streq(w_o_name, get_object_name(q_ptr))) add_wish_result(&w_list_top, q_ptr);
			else
			{
				strcpy(o_name, get_object_name(q_ptr) + 1);
				o_name[strlen(o_name) - 1] = '\0';
				if (streq(w_o_name, o_name)) add_wish_result(&w_list_top, q_ptr);
			}
			break;

		case TV_DRAG_ARMOR:
			if (k_ptr->sval == SV_DRAGON_POWER) continue;
			/* Fall through */
		default:
			object_prep(q_ptr, i);
			wish_apply_magic(q_ptr, TRUE);
			object_desc_store(o_name, q_ptr, FALSE, 0);
			if (streq(w_o_name, o_name)) add_wish_result(&w_list_top, q_ptr);
			break;
		}
	}

cleanup:
	for (cur_w_list0 = w_list_top; cur_w_list0; cur_w_list0 = cur_w_list1)
	{
		bool allow_prep = TRUE;

		q_ptr = &cur_w_list0->obj;

		if (q_ptr->name1)
		{
			if (a_info[q_ptr->name1].cur_num)
			{
				object_type *o_ptr;
				int y, x;

				allow_prep = FALSE;

				for (i = 1; i < o_max; i++)
				{
					o_ptr = &o_list[i];

					/* Ignore non-objects */
					if (!o_ptr->k_idx) continue;

					/* Ignore non-artifacts */
					if (!artifact_p(o_ptr)) continue;

					/* Ignore known items */
					if (object_known_p(o_ptr)) continue;

					if (o_ptr->name1 != q_ptr->name1) continue;

					/* Too expensive cost */
					if (p_ptr->lev <= (object_value_real(o_ptr) / 4000)) continue;

					object_copy(&forge, o_ptr);
					delete_object_idx(i);

					if (drop_near(&forge, -1, py, px))
					{
						wished = TRUE;
						break;
					}
				}
			}
			else
			{
				wish_apply_magic(q_ptr, TRUE);

				/* Too expensive cost */
				if (p_ptr->lev <= (object_value_real(q_ptr) / 4000))
					allow_prep = FALSE;
			}
		}

		if (allow_prep)
		{
			if (drop_near(q_ptr, -1, py, px))
			{
				artifact_type *a_ptr = &a_info[q_ptr->name1];

				if (q_ptr->name1) a_ptr->cur_num = 1;

				/* Hack -- Memorize location of artifact in saved floors */
				if (character_dungeon) a_ptr->floor_id = p_ptr->floor_id;
			}
			else
			{
				if (q_ptr->name1 && !preserve_mode) a_info[q_ptr->name1].cur_num = 1;
			}
			wished = TRUE;
		}

		cur_w_list1 = cur_w_list0->next;
		KILL(cur_w_list0, wish_result_type);
	}

	C_KILL(e_found, max_e_idx, byte);

	if (!wished) msg_print(err_msg ? err_msg : "願いは叶わなかった。");

	return wished;
}

/*
 * break your decoy
 */
void break_decoy(void)
{
	if (!p_ptr->use_decoy) return;

#ifdef JP
	msg_print("ダミー人形は壊れた。");
#else
	msg_print("Decoy is broken.");
#endif

	p_ptr->use_decoy = FALSE;
	lite_spot(p_ptr->decoy_y, p_ptr->decoy_x);
	p_ptr->decoy_y = 0;
	p_ptr->decoy_x = 0;

	forget_flow();

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_DISTANCE | PU_FLOW);
}

/*
 * Set your decoy
 */
bool set_decoy(void)
{
	cave_type *c_ptr = &cave[py][px];

	if ((!cave_floor_grid(c_ptr) && (c_ptr->feat != FEAT_TREES)) ||
	    (c_ptr->feat == FEAT_DARK_PIT) || (c_ptr->feat == FEAT_AIR))
	{
#ifdef JP
		msg_print("ここにはダミー人形を設置できません。");
#else
		msg_print("You cannot set the decoy on this place.");
#endif
		return FALSE;
	}

	if (p_ptr->use_decoy)
	{
		if ((py == p_ptr->decoy_y) && (px == p_ptr->decoy_x))
		{
#ifdef JP
			msg_print("あなたはダミー人形の上にいます。");
#else
			msg_print("You are on the decoy.");
#endif
			return FALSE;
		}
		else
		{
			break_decoy();
		}
	}

#ifdef JP
	msg_print("ダミー人形を足元にセットした。");
#else
	msg_print("You set decoy on this floor.");
#endif
	p_ptr->decoy_y = py;
	p_ptr->decoy_x = px;
	p_ptr->use_decoy = TRUE;
	lite_spot(py, px);

	forget_flow();

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_DISTANCE | PU_FLOW);

	return TRUE;
}

/*
 * Check if an object is nameless weapon or armour
 */
static bool item_tester_hook_ego_creatable(object_type *o_ptr)
{
	if (object_is_runeweapon(o_ptr)) return FALSE;

	/* Analyze type */
	switch (o_ptr->tval)
	{
	case TV_BOW:
	case TV_DIGGING:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
	case TV_BOOTS:
	case TV_GLOVES:
	case TV_HELM:
	case TV_CROWN:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_AMULET:
	case TV_RING:
	case TV_LITE:
		if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DIAMOND_EDGE)) return FALSE;

		if (object_known_p(o_ptr) && (o_ptr->name1 || o_ptr->art_name || o_ptr->name2 || o_ptr->xtra3))
			return FALSE;
		else return TRUE;
	}

	return FALSE;
}

bool ego_creation_scroll(void)
{
	int             item;
	bool            okay = FALSE;
	object_type     *o_ptr;
	object_type     forge;
	object_type     *q_ptr = &forge;
	char            o_name[MAX_NLEN];
	cptr            q, s;
	int             attempt;


	item_tester_no_ryoute = TRUE;
	item_tester_hook = item_tester_hook_ego_creatable;

	/* Get an item */
#ifdef JP
q = "どのアイテムを強化しますか? ";
s = "強化できるアイテムがない。";
#else
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Describe */
#ifdef JP
	msg_format("%s は眩い光を発した！",o_name);
#else
	msg_format("%s %s radiate%s a blinding light!",
	          ((item >= 0) ? "Your" : "The"), o_name,
	          ((o_ptr->number > 1) ? "" : "s"));
#endif

	if (o_ptr->name1 || o_ptr->art_name)
	{
#ifdef JP
		msg_format("%sは既に伝説のアイテムです！", o_name);
#else
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "artifacts" : "an artifact"));
#endif

		okay = FALSE;
	}

	else if (o_ptr->name2)
	{
#ifdef JP
		msg_format("%sは既に名のあるアイテムです！", o_name);
#else
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "ego items" : "an ego item"));
#endif

		okay = FALSE;
	}

	else if (o_ptr->xtra3)
	{
#ifdef JP
		msg_format("%sは既に強化されています！", o_name);
#else
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "gunner items" : "an gunner item"));
#endif
	}

	else
	{
		if (o_ptr->number > 1)
		{
#ifdef JP
			msg_print("複数のアイテムに魔法をかけるだけのエネルギーはありません！");
			msg_format("%d 個の%sが壊れた！",(o_ptr->number)-1, o_name);
#else
			msg_print("Not enough enough energy to enchant more than one object!");
			msg_format("%d of your %s %s destroyed!",(o_ptr->number)-1, o_name, (o_ptr->number>2?"were":"was"));
#endif

			if (item >= 0)
			{
				inven_item_increase(item, 1-(o_ptr->number));
			}
			else
			{
				floor_item_increase(0-item, 1-(o_ptr->number));
			}
		}
		if (item == INVEN_RARM)
		{
			if (mw_old_weight)
			{
				o_ptr->weight = mw_old_weight;
			}
			if (mw_diff_to_melee)
			{
				o_ptr->to_h -= mw_diff_to_melee;
				o_ptr->to_d -= mw_diff_to_melee;
			}
		}

		object_copy(q_ptr, o_ptr);
		for (attempt = 10000; attempt; attempt--)
		{
			o_ptr->name2 = randint1(max_e_idx - 1);
			if (e_info[o_ptr->name2].slot != tval_to_slot(o_ptr->tval)) continue;
			if (wish_apply_magic(o_ptr, FALSE))
			{
				okay = TRUE;
				break;
			}

			/* Failure */
			object_copy(o_ptr, q_ptr);
		}

		if (item == INVEN_RARM)
		{
			if (mw_old_weight)
			{
				mw_old_weight = o_ptr->weight;
				o_ptr->weight = 1;
			}
			if (mw_diff_to_melee)
			{
				o_ptr->to_h += mw_diff_to_melee;
				o_ptr->to_d += mw_diff_to_melee;
			}
		}
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
#ifdef JP
		msg_print("強化に失敗した。");
#else
		msg_print("The enchantment failed.");
#endif
	}

	/* Something happened */
	return TRUE;
}

bool get_energy_from_corpse(void)
{
	int         item;
	object_type *o_ptr;
	cptr        q, s;
	char o_name[MAX_NLEN];

	item_tester_hook = item_tester_hook_corpse;

	/* Get an item */
#ifdef JP
	q = "どの死体からエネルギーを吸い取りますか？ ";
	s = "死体を持っていない。";
#else
	q = "Drain from which corpse? ";
	s = "You have no corpse to drain.";
#endif
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	hp_player(66 + randint1(r_info[o_ptr->pval].level * 5));

	object_desc(o_name, o_ptr, FALSE, 0);
#ifdef JP
	msg_format("%sは灰になった。", o_name);
#else
	msg_format("The %s becomes dust.", o_name);
#endif

	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}
