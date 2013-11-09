/* File: cmd2.c */

/* Purpose: Movement commands (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	bool go_up = FALSE;
	bool upward_dun = (d_info[dungeon_type].flags1 & DF1_UPWARD) ? TRUE : FALSE;
	cave_type *c_ptr;
	int up_num = 0;

	/* Player grid */
	c_ptr = &cave[py][px];

	/* Quest up stairs */
	if (c_ptr->feat == FEAT_QUEST_UP)
	{
		/* Success */
#ifdef JP
		msg_print("上の階に登った。");
#else
		msg_print("You enter the up staircase.");
#endif

		leave_quest_check();

		p_ptr->inside_quest = c_ptr->special;

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
		}

		/* Leaving a quest */
		if (!p_ptr->inside_quest)
		{
			dun_level = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;

		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		/* End the command */
		return;
	}

	/* Verify stairs */
	if ((c_ptr->feat != FEAT_LESS) && (c_ptr->feat != FEAT_LESS_LESS) && (c_ptr->feat != FEAT_ENTRANCE_UPWARD))
	{
#ifdef JP
		msg_print("ここには上り階段が見当たらない。");
#else
		msg_print("I see no up staircase here.");
#endif

		return;
	}

	if (!dun_level)
	{
		if ((astral_mode) && (c_ptr->special != DUNGEON_PALACE))
		{
#ifdef JP
			msg_print("ダンジョンの入口は塞がれている！");
#else
			msg_print("The entrance of this dungeon is closed!");
#endif
			return;
		}
		if (!max_dlv[c_ptr->special])
		{
#ifdef JP
			msg_format("ここには%sの入り口(%d階相当)があります", d_name+d_info[c_ptr->special].name, d_info[c_ptr->special].mindepth);
			if (!get_check("本当にこのダンジョンに入りますか？")) return;
#else
			msg_format("There is the entrance of %s (Danger level: %d)", d_name+d_info[c_ptr->special].name, d_info[c_ptr->special].mindepth);
			if (!get_check("Do you really get in this dungeon? ")) return;
#endif
		}
		go_up = TRUE;

		/* Save old player position */
		p_ptr->oldpx = px;
		p_ptr->oldpy = py;
		dungeon_type = (byte)c_ptr->special;
	}
	else
	{
		quest_type *q_ptr = &quest[p_ptr->inside_quest];

		/* Confirm leaving from once only quest */
		if (confirm_quest && p_ptr->inside_quest &&
		    (((q_ptr->type == QUEST_TYPE_RANDOM) && !astral_mode) ||
		     (q_ptr->flags & QUEST_FLAG_ONCE &&
		      q_ptr->status != QUEST_STATUS_COMPLETED)))
		{
#ifdef JP
			msg_print("この階を一度去ると二度と戻って来られません。");
			if (get_check("本当にこの階を去りますか？")) go_up = TRUE;
#else
			msg_print("You can't come back here once you leave this floor.");
			if (get_check("Really leave this floor? ")) go_up = TRUE;
#endif
		}
		else
		{
			go_up = TRUE;
		}
	}

	/* Cancel the command */
	if (!go_up) return;

	/* Hack -- take a turn */
	energy_use = 100;

	if (autosave_l) do_cmd_save_game(TRUE);

	/* For a random quest */
	if (p_ptr->inside_quest &&
	    quest[p_ptr->inside_quest].type == QUEST_TYPE_RANDOM)
	{
		leave_quest_check();

		p_ptr->inside_quest = 0;
	}

	/* For a fixed quest */
	if (p_ptr->inside_quest &&
	    quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM)
	{
		leave_quest_check();

		p_ptr->inside_quest = c_ptr->special;
		dun_level = 0;
		up_num = 0;
	}

	/* For normal dungeon and random quest */
	else
	{
		/* New depth */
		if (c_ptr->feat == FEAT_LESS_LESS)
		{
			/* Create a way back */
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_UP | CFM_SHAFT);
			up_num = 2;
		}
		else
		{
			/* Create a way back */
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_UP);
			up_num = 1;
		}

		if (!dun_level)
		{
			/* Enter the dungeon just now */
			p_ptr->enter_dungeon = TRUE;
			up_num = d_info[c_ptr->special].mindepth;
		}

		if (!upward_dun && (dun_level - up_num < d_info[dungeon_type].mindepth))
			up_num = dun_level;
	}

#ifdef JP
	if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, 0-up_num, "階段を上った");
#else
	if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, 0-up_num, "go up the stairs to");
#endif

	/* Success */
	if (c_ptr->feat == FEAT_ENTRANCE_UPWARD)
	{
#ifdef JP
		msg_format("%sへ入った。", d_text + d_info[dungeon_type].text);
#else
		msg_format("You entered %s.", d_text + d_info[dungeon_type].text);
#endif
	}
	else if (!upward_dun && (up_num == dun_level))
	{
		if (astral_mode && (dungeon_type == DUNGEON_PALACE))
		{
#ifdef JP
			msg_print("あなたは死者の宮殿から見事に脱出した！");
#else
			msg_print("You survived from Death Palace finally!");
#endif
			/* Total winner */
			p_ptr->total_winner = TRUE;

			/* Stop playing */
			p_ptr->playing = FALSE;

			/* Kill the player */
			p_ptr->is_dead |= DEATH_DEAD;
		}
		else
		{
#ifdef JP
			msg_print("地上に戻った。");
#else
			msg_print("You go back to the surface.");
#endif
		}
	}
	else
#ifdef JP
		msg_print("階段を上って新たなる迷宮へと足を踏み入れた。");
#else
		msg_print("You enter a maze of up staircases.");
#endif

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Returns TRUE if we are in the Between...
 */
static bool between_effect(cave_type *c_ptr)
{
	byte bx, by;

	if (c_ptr->feat == FEAT_BETWEEN)
	{
		bx = c_ptr->special & 255;
		by = c_ptr->special >> 8;

#ifdef JP
		msg_print("すさまじく冷たい！");
		cold_dam(damroll(10, 10), "間隙への進入");
#else
		msg_print("Brrrr! It's deadly cold.");
		cold_dam(damroll(10, 10), "going Between");
#endif

		energy_use = 100;

		if (cave[by][bx].m_idx)
		{
			msg_print("何かが向こう側の間隙の上にいるので出られなかった。");
			return TRUE;
		}

		teleport_player_to(by, bx, FALSE, FALSE);

		return TRUE;
	}

	return FALSE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	cave_type *c_ptr;
	bool go_down = FALSE;
	bool upward_dun = (d_info[dungeon_type].flags1 & DF1_UPWARD) ? TRUE : FALSE;
	int fall_type = 0;
	int down_num = 0;
	bool dive_into_astral = FALSE;

	/* Player grid */
	c_ptr = &cave[py][px];

	/* MUST be actived now */
	if (between_effect(c_ptr)) return;

	if (c_ptr->feat == (FEAT_TRAP_TRAPDOOR)) fall_type = 1;
	else if (c_ptr->feat == FEAT_AIR)
	{
		if ((!upward_dun && (dun_level >= d_info[dungeon_type].maxdepth)) ||
		     (upward_dun && (dun_level <= d_info[dungeon_type].mindepth)))
		{
#ifdef JP
			msg_print("これ以上降下するのは危険だ。");
#else
			msg_print("Falling from this level is danger.");
#endif
			return;
		}

		fall_type = 2;
	}

	/* Quest entrance */
	if (c_ptr->feat == FEAT_QUEST_ENTER)
	{
		do_cmd_quest();
	}

	/* Quest down stairs */
	else if (c_ptr->feat == FEAT_QUEST_DOWN)
	{
#ifdef JP
		msg_print("下の階に降りた。");
#else
		msg_print("You enter the down staircase.");
#endif

		leave_quest_check();

		p_ptr->inside_quest = c_ptr->special;

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
		}

		/* Leaving a quest */
		if (!p_ptr->inside_quest)
		{
			dun_level = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;

		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		/* End the command */
		return;
	}

	/* Verify stairs */
	if ((c_ptr->feat != FEAT_MORE) && (c_ptr->feat != FEAT_MORE_MORE) && (c_ptr->feat != FEAT_ENTRANCE) && !fall_type)
	{
#ifdef JP
		msg_print("ここには下り階段が見当たらない。");
#else
		msg_print("I see no down staircase here.");
#endif

		return;
	}

	if (!dun_level)
	{
		if ((astral_mode) && (c_ptr->special != DUNGEON_PALACE))
		{
#ifdef JP
			msg_print("ダンジョンの入口は塞がれている！");
#else
			msg_print("The entrance of this dungeon is closed!");
#endif
			return;
		}
		if (astral_mode && (c_ptr->feat == FEAT_ENTRANCE) && (c_ptr->special == DUNGEON_PALACE))
		{
			msg_print("この街へは二度と戻って来れないだろう…。");
			if (!get_check("覚悟はいいですか？")) return;
			dive_into_astral = TRUE;
		}
		else if (!max_dlv[c_ptr->special])
		{
#ifdef JP
			msg_format("ここには%sの入り口(%d階相当)があります", d_name+d_info[c_ptr->special].name, d_info[c_ptr->special].mindepth);
			if (!get_check("本当にこのダンジョンに入りますか？")) return;
#else
			msg_format("There is the entrance of %s (Danger level: %d)", d_name+d_info[c_ptr->special].name, d_info[c_ptr->special].mindepth);
			if (!get_check("Do you really get in this dungeon? ")) return;
#endif
		}
		go_down = TRUE;

		/* Save old player position */
		p_ptr->oldpx = px;
		p_ptr->oldpy = py;
		dungeon_type = (byte)c_ptr->special;

		/*
		 * Clear all saved floors
		 * and create a first saved floor
		 */
		prepare_change_floor_mode(CFM_FIRST_FLOOR);
	}
	else
	{
		quest_type *q_ptr = &quest[p_ptr->inside_quest];

		if (confirm_quest && p_ptr->inside_quest &&
		    (((q_ptr->type == QUEST_TYPE_RANDOM) && !astral_mode) ||
		     (q_ptr->flags & QUEST_FLAG_ONCE &&
		      q_ptr->status != QUEST_STATUS_COMPLETED)))
		{
#ifdef JP
			msg_print("この階を一度去ると二度と戻って来られません。");
			if (get_check("本当にこの階を去りますか？")) go_down = TRUE;
#else
			msg_print("You can't come back here once you leave this floor.");
			if (get_check("Really leave this floor? ")) go_down = TRUE;
#endif
		}
		else
		{
			go_down = TRUE;
		}
	}

	/* Cancel the command */
	if (!go_down) return;

	/* Hack -- take a turn */
	energy_use = 100;

	if (autosave_l) do_cmd_save_game(TRUE);

	if (p_ptr->inside_quest)
	{
		leave_quest_check();

		if (quest[leaving_quest].type != QUEST_TYPE_RANDOM)
		{
			p_ptr->inside_quest = c_ptr->special;
			dun_level = 0;
		}
		else
		{
			p_ptr->inside_quest = 0;
		}
	}

	/* New depth */
	if (c_ptr->feat == FEAT_MORE_MORE) down_num = 2;
	else if (fall_type == 1)
	{
		int i;
		int dd, dm;

		down_num = randint1(3);

		if (upward_dun) dm = d_info[dungeon_type].mindepth;
		else dm = d_info[dungeon_type].maxdepth;

		for (i = 1; i < down_num; i++)
		{
			dd = dun_level + (upward_dun ? (0 - i) : i);
			if ((!(upward_dun ^ astral_mode) && quest_number(dd)) || (dd == dm))
			{
				down_num = i;
				break;
			}
		}
	}
	else down_num = 1;

	if (!dun_level)
	{
		/* Enter the dungeon just now */
		p_ptr->enter_dungeon = TRUE;
		if (dive_into_astral) down_num = 98;
		else down_num = d_info[c_ptr->special].mindepth;
	}
	if (upward_dun && (dun_level - down_num < d_info[dungeon_type].mindepth))
		down_num = dun_level;

	switch (fall_type)
	{
	case 1:
#ifdef JP
		if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, down_num, "落し戸に落ちた");
		msg_print("わざと落し戸に落ちた。");
#else
		if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, down_num, "fall from trap door");
		msg_print("You deliberately jump through the trap door.");
#endif
		switch (down_num)
		{
		case 1:
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
			break;
		case 2:
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_FORCE2L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
			break;
		default:
			dun_level += upward_dun ? (0 - down_num + 2) : (down_num - 2);
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_FORCE2L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
			break;
		}
		break;

	case 2:
#ifdef JP
		if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, down_num, "空中に飛び込んだ");
		msg_print("わざと空中に飛び込んだ。");
#else
		if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, down_num, "dive into air");
		msg_print("You deliberately dive into air.");
#endif
		prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_FORCE1L | CFM_RAND_PLACE | CFM_RAND_CONNECT);
		break;

	default:
#ifdef JP
		if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, down_num, "階段を下りた");
#else
		if (record_stair) do_cmd_write_nikki(NIKKI_STAIR, down_num, "go down the stairs to");
#endif
		/* Success */
		if (c_ptr->feat == FEAT_ENTRANCE)
		{
#ifdef JP
			msg_format("%sへ入った。", d_text + d_info[dungeon_type].text);
#else
			msg_format("You entered %s.", d_text + d_info[dungeon_type].text);
#endif
			if (dive_into_astral) dun_level = down_num;
		}
		else if (upward_dun && (down_num == dun_level))
#ifdef JP
			msg_print("地上に戻った。");
#else
			msg_print("You go back to the surface.");
#endif
		else
#ifdef JP
			msg_print("階段を下りて新たなる迷宮へと足を踏み入れた。");
#else
			msg_print("You enter a maze of down staircases.");
#endif

		if (c_ptr->feat == FEAT_MORE_MORE)
		{
			/* Create a way back */
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_SHAFT);
		}
		else if (!dive_into_astral)
		{
			/* Create a way back */
			prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN);
		}

		break;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Take a turn */
	energy_use = 100;

	/* Search */
	search();
}


/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;


	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		/* if (!o_ptr->marked) continue; */

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/*
 * Allocates objects upon opening a chest    -BEN-
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Small chests often contain "gold", while Large chests always contain
 * items.  Wooden chests contain 2 items, Iron chests contain 4 items,
 * and Steel chests contain 6 items.  The "value" of the items in a
 * chest is based on the "power" of the chest, which is in turn based
 * on the level on which the chest is generated.
 */
static void chest_death(bool scatter, int y, int x, s16b o_idx)
{
	int number;

	bool small;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr = &o_list[o_idx];


	/* Small chests often hold "gold" */
	small = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Small chests often drop gold */
		if (small && (randint0(100) < 25))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make a good object */
			if (!make_object(q_ptr, AMF_OKAY | AMF_GOOD)) continue;
		}

		/* If chest scatters its contents, pick any floor square. */
		if (scatter)
		{
			int i;
			for (i = 0; i < 200; i++)
			{
				/* Pick a totally random spot. */
				y = randint0(MAX_HGT);
				x = randint0(MAX_WID);

				/* Must be an empty floor. */
				if (!cave_empty_bold(y, x)) continue;

				/* Place the object there. */
				drop_near(q_ptr, -1, y, x);

				/* Done. */
				break;
			}
		}
		/* Normally, drop object near the chest. */
		else drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = base_level;

	/* No longer opening a chest */
	opening_chest = FALSE;

	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
	int  i, trap;

	object_type *o_ptr = &o_list[o_idx];

	int mon_level = o_ptr->xtra3;

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
#ifdef JP
		msg_print("仕掛けられていた小さな針に刺されてしまった！");
		take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "毒針");
#else
		msg_print("A small needle has pricked you!");
		take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle");
#endif

		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
#ifdef JP
		msg_print("仕掛けられていた小さな針に刺されてしまった！");
		take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "毒針");
#else
		msg_print("A small needle has pricked you!");
		take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle");
#endif

		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
#ifdef JP
		msg_print("突如吹き出した緑色のガスに包み込まれた！");
#else
		msg_print("A puff of green gas surrounds you!");
#endif

		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)set_poisoned(p_ptr->poisoned + 10 + randint1(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
#ifdef JP
		msg_print("突如吹き出した黄色いガスに包み込まれた！");
#else
		msg_print("A puff of yellow gas surrounds you!");
#endif


		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + 10 + randint1(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint1(3);
#ifdef JP
		msg_print("突如吹き出した煙に包み込まれた！");
#else
		msg_print("You are enveloped in a cloud of smoke!");
#endif


		for (i = 0; i < num; i++)
		{
			if (randint1(100)<dun_level)
				activate_hi_summon(py, px);
			else
				(void)summon_specific(0, y, x, mon_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
		}
	}

	/* Elemental summon. */
	if (trap & (CHEST_E_SUMMON))
	{
#ifdef JP
		msg_print("宝を守るためにエレメンタルが現れた！");
#else
		msg_print("Elemental beings appear to protect their treasures!");
#endif
		for (i = 0; i < randint1(3) + 5; i++)
		{
			(void)summon_specific(0, y, x, mon_level, SUMMON_ELEMENTAL, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
		}
	}

	/* Force clouds, then summon birds. */
	if (trap & (CHEST_BIRD_STORM))
	{
#ifdef JP
		msg_print("鳥の群れがあなたを取り巻いた！");
#else
		msg_print("A storm of birds swirls around you!");
#endif

		for (i = 0; i < randint1(3) + 3; i++)
			(void)fire_meteor(-1, GF_FORCE, y, x, o_ptr->pval / 5, 7, FALSE);

		for (i = 0; i < randint1(5) + o_ptr->pval / 5; i++)
		{
			(void)summon_specific(0, y, x, mon_level, SUMMON_BIRD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
		}
	}

	/* Various colorful summonings. */
	if (trap & (CHEST_H_SUMMON))
	{
		/* Summon demons. */
		if (one_in_(4))
		{
#ifdef JP
			msg_print("炎と硫黄の雲の中に悪魔が姿を現した！");
#else
			msg_print("Demons materialize in clouds of fire and brimstone!");
#endif

			for (i = 0; i < randint1(3) + 2; i++)
			{
				(void)fire_meteor(-1, GF_FIRE, y, x, 10, 5, FALSE);
				(void)summon_specific(0, y, x, mon_level, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
			}
		}

		/* Summon dragons. */
		else if (one_in_(3))
		{
#ifdef JP
			msg_print("暗闇にドラゴンの影がぼんやりと現れた！");
#else
			msg_print("Draconic forms loom out of the darkness!");
#endif

			for (i = 0; i < randint1(3) + 2; i++)
			{
				(void)summon_specific(0, y, x, mon_level, SUMMON_DRAGON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
			}
		}

		/* Summon hybrids. */
		else if (one_in_(2))
		{
#ifdef JP
			msg_print("奇妙な姿の怪物が襲って来た！");
#else
			msg_print("Creatures strange and twisted assault you!");
#endif

			for (i = 0; i < randint1(5) + 3; i++)
			{
				(void)summon_specific(0, y, x, mon_level, SUMMON_HYBRID, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
			}
		}

		/* Summon vortices (scattered) */
		else
		{
#ifdef JP
			msg_print("渦巻が合体し、破裂した！");
#else
			msg_print("Vortices coalesce and wreak destruction!");
#endif

			for (i = 0; i < randint1(3) + 2; i++)
			{
				(void)summon_specific(0, y, x, mon_level, SUMMON_VORTEX, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
			}
		}
	}

	/* Dispel player. */
	if ((trap & (CHEST_RUNES_OF_EVIL)) && o_ptr->k_idx)
	{
		/* Determine how many nasty tricks can be played. */
		int nasty_tricks_count = 4 + randint0(3);

		/* Message. */
#ifdef JP
		msg_print("恐しい声が響いた:  「暗闇が汝をつつまん！」");
#else
		msg_print("Hideous voices bid:  'Let the darkness have thee!'");
#endif

		/* This is gonna hurt... */
		for (; nasty_tricks_count > 0; nasty_tricks_count--)
		{
			/* ...but a high saving throw does help a little. */
			if (randint1(100+o_ptr->pval*2) > p_ptr->skill_sav)
			{
#ifdef JP
				if (one_in_(6)) take_hit(DAMAGE_NOESCAPE, damroll(5, 20), "破滅のトラップの宝箱");
#else
				if (one_in_(6)) take_hit(DAMAGE_NOESCAPE, damroll(5, 20), "a chest dispel-player trap");
#endif
				else if (one_in_(5)) (void)set_cut(p_ptr->cut + 200);
				else if (one_in_(4))
				{
					if (!p_ptr->free_act) 
						(void)set_paralyzed(p_ptr->paralyzed + 2 + 
						randint0(6));
					else 
						(void)set_stun(p_ptr->stun + 10 + 
						randint0(100));
				}
				else if (one_in_(3)) apply_disenchant();
				else if (one_in_(2))
				{
					(void)do_dec_stat(A_STR);
					(void)do_dec_stat(A_DEX);
					(void)do_dec_stat(A_CON);
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
					(void)do_dec_stat(A_CHR);
				}
				else (void)fire_meteor(-1, GF_NETHER, y, x, 150, 1, FALSE);
			}
		}
	}

	/* Aggravate monsters. */
	if (trap & (CHEST_ALARM))
	{
#ifdef JP
		msg_print("けたたましい音が鳴り響いた！");
#else
		msg_print("An alarm sounds!");
#endif
		aggravate_monsters(0);
	}

	/* Explode */
	if ((trap & (CHEST_EXPLODE)) && o_ptr->k_idx)
	{
#ifdef JP
		msg_print("突然、箱が爆発した！");
		msg_print("箱の中の物はすべて粉々に砕け散った！");
#else
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
#endif

		o_ptr->pval = 0;
		sound(SOUND_EXPLODE);
		ACTIVATE_MULTISHADOW();
#ifdef JP
		take_hit(DAMAGE_ATTACK, damroll(5, 8), "爆発する箱");
#else
		take_hit(DAMAGE_ATTACK, damroll(5, 8), "an exploding chest");
#endif
		STOP_MULTISHADOW();

	}
	/* Scatter contents. */
	if ((trap & (CHEST_SCATTER)) && o_ptr->k_idx)
	{
#ifdef JP
		msg_print("宝箱の中身はダンジョンじゅうに散乱した！");
#else
		msg_print("The contents of the chest scatter all over the dungeon!");
#endif
		chest_death(TRUE, y, x, o_idx);
		o_ptr->pval = 0;
	}
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
	int i, j;

	bool flag = TRUE;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Take a turn */
	energy_use = 100;

	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (randint0(100) < j)
		{
#ifdef JP
			msg_print("鍵をはずした。");
#else
			msg_print("You have picked the lock.");
#endif

			gain_exp(1);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
#ifdef JP
			msg_print("鍵をはずせなかった。");
#else
			msg_print("You failed to pick the lock.");
#endif

		}
	}

	/* Allowed to open */
	if (flag)
	{
		/* Apply chest traps, if any */
		chest_trap(y, x, o_idx);

		/* Let the Chest drop items */
		chest_death(FALSE, y, x, o_idx);
	}

	/* Result */
	return (more);
}


/*
 * Return TRUE if the given feature is an open door
 */
static bool is_open(int feat)
{
	return (feat == FEAT_OPEN);
}

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_dt(int *y, int *x, bool (*test)(int feat), bool under)
{
	int d, count, xx, yy;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		cave_type *c_ptr;
		byte feat;

		/* if not searching under player continue */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = py + ddy_ddd[d];
		xx = px + ddx_ddd[d];

		/* Get the cave */
		c_ptr = &cave[yy][xx];

		/* Must have knowledge */
		if (!(c_ptr->info & (CAVE_MARK))) continue;

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Not looking for this feature */
		if (!((*test)(feat))) continue;

		/* OK */
		++count;

		/* Remember the location. Only useful if only one match */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}


/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
static int count_chests(int *y, int *x, bool trapped)
{
	int d, count, o_idx;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = py + ddy_ddd[d];
		int xx = px + ddx_ddd[d];

		/* No (visible) chest is there */
		if ((o_idx = chest_check(yy, xx)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Already open */
		if (o_ptr->pval == 0) continue;

		/* No (known) traps here */
		if (trapped && (!object_is_known(o_ptr) ||
			!chest_traps[o_ptr->pval])) continue;

		/* OK */
		++count;

		/* Remember the location. Only useful if only one match */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}


/*
 * Convert an adjacent location to a direction.
 */
static int coords_to_dir(int y, int x)
{
	int d[3][3] = { {7, 4, 1}, {8, 5, 2}, {9, 6, 3} };
	int dy, dx;

	dy = y - py;
	dx = x - px;

	/* Paranoia */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return d[dx + 1][dy + 1];
}



/*
 * Perform the basic "open" command on doors
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
	int i, j;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get requested grid */
	c_ptr = &cave[y][x];

	/* Seeing true feature code (ignore mimic) */

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
#ifdef JP
		msg_print("ドアはがっちりと閉じられているようだ。");
#else
		msg_print("The door appears to be stuck.");
#endif

	}

	/* Locked door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (randint0(100) < j)
		{
			/* Message */
#ifdef JP
			msg_print("鍵をはずした。");
#else
			msg_print("You have picked the lock.");
#endif


			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
#ifdef JP
			msg_print("鍵をはずせなかった。");
#else
			msg_print("You failed to pick the lock.");
#endif


			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else if (c_ptr->feat == FEAT_DOOR_HEAD)
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Result */
	return (more);
}



/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_open)
	{
		int num_doors, num_chests;

		/* Count closed doors (locked or jammed) */
		num_doors = count_dt(&y, &x, is_closed_door, FALSE);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if (num_doors || num_chests)
		{
			bool too_many = (num_doors && num_chests) || (num_doors > 1) ||
			    (num_chests > 1);
			if (!too_many) command_dir = coords_to_dir(y, x);
		}
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir, TRUE))
	{
		byte feat;
		cave_type *c_ptr;

		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get requested grid */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Check for chest */
		o_idx = chest_check(y, x);

		/* Nothing useful */
		if (!is_closed_door(feat) && !o_idx)
		{
			/* Message */
#ifdef JP
		msg_print("そこには開けるものが見当たらない。");
#else
			msg_print("You see nothing there to open.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx && p_ptr->riding != c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x, 0);
		}

		/* Handle chests */
		else if (o_idx)
		{
			/* Open the chest */
			more = do_cmd_open_chest(y, x, o_idx);
		}

		/* Handle doors */
		else
		{
			/* Open the door */
			more = do_cmd_open_aux(y, x);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Perform the basic "close" command
 *
 * Assume destination is an open/broken door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	cave_type	*c_ptr;
	bool		more = FALSE;

	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Seeing true feature code (ignore mimic) */

	/* Broken door */
	if (c_ptr->feat == FEAT_BROKEN)
	{
		/* Message */
#ifdef JP
		msg_print("ドアは壊れてしまっている。");
#else
		msg_print("The door appears to be broken.");
#endif

	}

	/* Open door */
	else if (c_ptr->feat == FEAT_OPEN)
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

		/* Sound */
		sound(SOUND_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
	int y, x, dir;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count open doors */
		if (count_dt(&y, &x, is_open, FALSE) == 1)
		{
			command_dir = coords_to_dir(y, x);
		}
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		cave_type *c_ptr;
		byte feat;

		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Require open/broken door */
		if ((feat != FEAT_OPEN) && (feat != FEAT_BROKEN))
		{
			/* Message */
#ifdef JP
			msg_print("そこには閉じるものが見当たらない。");
#else
			msg_print("You see nothing there to close.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x, 0);
		}

		/* Close the door */
		else
		{
			/* Close the door */
			more = do_cmd_close_aux(y, x);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Get grid */
	cave_type *c_ptr = &cave[y][x];

	/* Must have knowledge */
	if (!(c_ptr->info & (CAVE_MARK)))
	{
		/* Message */
#ifdef JP
		msg_print("そこには何も見当たらない。");
#else
		msg_print("You see nothing there.");
#endif


		/* Nope */
		return (FALSE);
	}

	if (!in_bounds(y, x))
	{
		/* Feature code (applying "mimic" field) */
		byte feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		if (boundary_floor_grid(c_ptr))
		{
#ifdef JP
			msg_print("空気は掘れない。");
#else
			msg_print("You cannot tunnel through air.");
#endif
		}
		else if (feat == FEAT_TREES)
		{
#ifdef JP
			msg_print("この木は硬すぎて切れないようだ。");
#else
			msg_print("This seems to be permanent tree.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("この岩は硬すぎて掘れないようだ。");
#else
			msg_print("This seems to be permanent rock.");
#endif
		}

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
	{
		/* Message */
#ifdef JP
		msg_print("そこには掘るものが見当たらない。");
#else
		msg_print("You see nothing there to tunnel.");
#endif


		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}



/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * This will, however, produce grids which are NOT illuminated
 * (or darkened) along with the rest of the room.
 */
static bool twall(int y, int x, byte feat)
{
	cave_type	*c_ptr = &cave[y][x];

	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

	/* Remove the feature */
	if (feat != FEAT_NONE) cave_set_feat(y, x, feat);
	else cave_force_set_floor(y, x);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

	/* Result */
	return (TRUE);
}



/*
 * Perform the basic "tunnel" command
 *
 * Assumes that the destination is a wall, a vein, a secret
 * door, or rubble.
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	cave_type *c_ptr;
	byte feat;

	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Feature code (applying "mimic" field) */
	feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

	/* Sound */
	sound(SOUND_DIG);

	/* Titanium */
	if ((feat >= FEAT_PERM_EXTRA) &&
	    (feat <= FEAT_PERM_SOLID))
	{
#ifdef JP
		msg_print("この岩は硬すぎて掘れないようだ。");
#else
		msg_print("This seems to be permanent rock.");
#endif
	}

	/* No tunnelling through mountains */
	else if (feat == FEAT_MOUNTAIN)
	{
#ifdef JP
		msg_print("そこは掘れない!");
#else
		msg_print("You can't tunnel through that!");
#endif

	}

	/* Map border (mimiccing Permanent wall) */
	else if ((c_ptr->feat >= FEAT_PERM_EXTRA &&
		  c_ptr->feat <= FEAT_PERM_SOLID) ||
		 c_ptr->feat == FEAT_MOUNTAIN)
	{
#ifdef JP
		msg_print("そこは掘れない!");
#else
		msg_print("You can't tunnel through that!");
#endif

	}

	else if (feat == FEAT_TREES) /* -KMW- */
	{
		/* Chop Down */
		if ((p_ptr->skill_dig > 10 + randint0(400)) && twall(y, x, FEAT_GRASS))
		{
#ifdef JP
			msg_print("木を切り払った。");
#else
			msg_print("You have cleared away the trees.");
#endif
			if (one_in_(20)) change_your_alignment(ALI_LNC, -1);
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
#ifdef JP
			msg_print("木を切っている。");
#else
			msg_print("You chop away at the tree.");
#endif

			more = TRUE;

			/* Occasional Search XXX XXX */
			if (randint0(100) < 25) search();
		}
	}


	/* Granite */
	else if ((feat >= FEAT_WALL_EXTRA) &&
	         (feat <= FEAT_WALL_SOLID))
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + randint0(1600)) && twall(y, x, FEAT_NONE))
		{
#ifdef JP
			msg_print("穴を掘り終えた。");
#else
			msg_print("You have finished the tunnel.");
#endif
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
#ifdef JP
			msg_print("花崗岩の壁に穴を掘っている。");
#else
			msg_print("You tunnel into the granite wall.");
#endif

			more = TRUE;
		}
	}


	/* Quartz / Magma */
	else if ((feat >= FEAT_MAGMA) &&
	    (feat <= FEAT_QUARTZ_K))
	{
		bool okay = FALSE;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold (ignore mimic; maybe a hidden treasure) */
		if (c_ptr->feat >= FEAT_MAGMA_H &&
		    c_ptr->feat <= FEAT_QUARTZ_K) gold = TRUE;

		/* Extract "quartz" flag XXX XXX XXX */
		if ((feat - FEAT_MAGMA) & 0x01) hard = TRUE;

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->skill_dig > 20 + randint0(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->skill_dig > 10 + randint0(400));
		}

		/* Success */
		if (okay && twall(y, x, FEAT_NONE))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(y, x);

				/* Message */
#ifdef JP
				msg_print("何かを発見した！");
#else
				msg_print("You have found something!");
#endif

			}

			/* Found nothing */
			else
			{
				/* Message */
#ifdef JP
				msg_print("穴を掘り終えた。");
#else
				msg_print("You have finished the tunnel.");
#endif
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
#ifdef JP
			msg_print("石英の鉱脈に穴を掘っている。");
#else
			msg_print("You tunnel into the quartz vein.");
#endif

			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
#ifdef JP
			msg_print("溶岩の鉱脈に穴を掘っている。");
#else
			msg_print("You tunnel into the magma vein.");
#endif

			more = TRUE;
		}
	}

	/* Rubble */
	else if (feat == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skill_dig > randint0(200)) && twall(y, x, FEAT_NONE))
		{
			/* Message */
#ifdef JP
			msg_print("岩石をくずした。");
#else
			msg_print("You have removed the rubble.");
#endif

			/* Hack -- place an object */
			if (randint0(100) < (15 - dun_level/2))
			{
				/* Create a simple object */
				place_object(y, x, AMF_OKAY);

				/* Observe new object */
				if (player_can_see_bold(y, x))
				{
#ifdef JP
					msg_print("何かを発見した！");
#else
					msg_print("You have found something!");
#endif

				}
			}
		}

		else
		{
			/* Message, keep digging */
#ifdef JP
			msg_print("岩石をくずしている。");
#else
			msg_print("You dig in the rubble.");
#endif

			more = TRUE;
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 30 + randint0(1200)) && twall(y, x, FEAT_NONE))
		{
#ifdef JP
			msg_print("穴を掘り終えた。");
#else
			msg_print("You have finished the tunnel.");
#endif

		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
#ifdef JP
			msg_print("ドアに穴を開けている。");
#else
			msg_print("You tunnel into the door.");
#endif

			more = TRUE;
		}
	}

	if (is_hidden_door(c_ptr))
	{
		/* Occasional Search XXX XXX */
		if (randint0(100) < 25) search();
	}

	/* Result */
	return (more);
}


/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Note that you must tunnel in order to hit invisible monsters
 * in walls, though moving into walls still takes a turn anyway.
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int			y, x, dir;

	cave_type	*c_ptr;
	byte feat;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction to tunnel, or Abort */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* No tunnelling through doors */
		if (((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL)) ||
		    ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL)) ||
		    ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL)) ||
		    (feat == FEAT_MUSEUM) || (feat == FEAT_DENEB_SHOP))
		{
			/* Message */
#ifdef JP
			msg_print("ドアは掘れない。");
#else
			msg_print("You cannot tunnel through doors.");
#endif

		}

		/* No tunnelling through air */
		else if (cave_floor_grid(c_ptr) || (feat == FEAT_MINOR_GLYPH))
		{
			/* Message */
#ifdef JP
			msg_print("空気は掘れない。");
#else
			msg_print("You cannot tunnel through air.");
#endif

		}

		/* No tunnelling through mountains */
		else if (feat == FEAT_MOUNTAIN)
		{
#ifdef JP
			msg_print("そこは掘れない。");
#else
			msg_print("You can't tunnel through that!");
#endif

		}

		/* A monster is in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x, 0);
		}

		/* Try digging */
		else
		{
			/* Tunnel through walls */
			more = do_cmd_tunnel_aux(y, x);
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}



/*
 * easy_open_door --
 *
 *	If there is a jammed/closed/locked door at the given location,
 *	then attempt to unlock/open it. Return TRUE if an attempt was
 *	made (successful or not), otherwise return FALSE.
 *
 *	The code here should be nearly identical to that in
 *	do_cmd_open_test() and do_cmd_open_aux().
 */
bool easy_open_door(int y, int x)
{
	int i, j;

	cave_type *c_ptr = &cave[y][x];

	/* Must be a closed door */
	if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
	      (c_ptr->feat <= FEAT_DOOR_TAIL)))
	{
		/* Nope */
		return (FALSE);
	}

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
#ifdef JP
		msg_print("ドアはがっちりと閉じられているようだ。");
#else
		msg_print("The door appears to be stuck.");
#endif

	}

	/* Locked door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (randint0(100) < j)
		{
			/* Message */
#ifdef JP
			msg_print("鍵をはずした。");
#else
			msg_print("You have picked the lock.");
#endif


			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
#ifdef JP
			msg_print("鍵をはずせなかった。");
#else
			msg_print("You failed to pick the lock.");
#endif

		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Result */
	return (TRUE);
}



/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, s16b o_idx)
{
	int i, j;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Take a turn */
	energy_use = 100;

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	j = i - o_ptr->pval;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Must find the trap first. */
	if (!object_is_known(o_ptr))
	{
#ifdef JP
		msg_print("トラップが見あたらない。");
#else
		msg_print("I don't see any traps.");
#endif

	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
#ifdef JP
		msg_print("箱にはトラップが仕掛けられていない。");
#else
		msg_print("The chest is not trapped.");
#endif

	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
#ifdef JP
		msg_print("箱にはトラップが仕掛けられていない。");
#else
		msg_print("The chest is not trapped.");
#endif

	}

	/* Success (get a lot of experience) */
	else if (randint0(100) < j)
	{
#ifdef JP
		msg_print("箱に仕掛けられていたトラップを解除した。");
#else
		msg_print("You have disarmed the chest.");
#endif

		gain_exp(o_ptr->pval);
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint1(i) > 5))
	{
		/* We may keep trying */
		more = TRUE;
		if (flush_failure) flush();
#ifdef JP
		msg_print("箱のトラップ解除に失敗した。");
#else
		msg_print("You failed to disarm the chest.");
#endif

	}

	/* Failure -- Set off the trap */
	else
	{
#ifdef JP
		msg_print("トラップを作動させてしまった！");
#else
		msg_print("You set off a trap!");
#endif

		sound(SOUND_FAIL);
		chest_trap(y, x, o_idx);
	}

	/* Result */
	return (more);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
bool do_cmd_disarm_aux(int y, int x, int dir)
{
	int i, j, power;

	cave_type *c_ptr;

	cptr name;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Access trap name */
	name = (f_name + f_info[c_ptr->feat].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Variable power! */

	/* Extract trap "power" */
	switch (c_ptr->feat)
	{
	case FEAT_TRAP_OPEN:
	case FEAT_TRAP_ARMAGEDDON:
	case FEAT_TRAP_PIRANHA:
		/* Special traps are very difficult to disarm */
		power = 100;
		break;
	default:
		power = 5;
		break;
	}

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if (randint0(100) < j)
	{
		/* Message */
#ifdef JP
		msg_format("%sを解除した。", name);
#else
		msg_format("You have disarmed the %s.", name);
#endif


		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_force_set_floor(y, x);

		/* Move the player onto the trap */
		move_player(dir, easy_disarm);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint1(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
#ifdef JP
		msg_format("%sの解除に失敗した。", name);
#else
		msg_format("You failed to disarm the %s.", name);
#endif


		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
#ifdef JP
		msg_format("%sを作動させてしまった！", name);
#else
		msg_format("You set off the %s!", name);
#endif

		/* Move the player onto the trap */
		move_player(dir, easy_disarm);
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_disarm)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_dt(&y, &x, is_trap, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps || num_chests)
		{
			bool too_many = (num_traps && num_chests) || (num_traps > 1) ||
			    (num_chests > 1);
			if (!too_many) command_dir = coords_to_dir(y, x);
		}
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction (or abort) */
	if (get_rep_dir(&dir,TRUE))
	{
		cave_type *c_ptr;
		byte feat;

		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Check for chests */
		o_idx = chest_check(y, x);

		/* Disarm a trap */
		if (!is_trap(feat) && !o_idx)
		{
			/* Message */
#ifdef JP
			msg_print("そこには解除するものが見当たらない。");
#else
			msg_print("You see nothing there to disarm.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx && p_ptr->riding != c_ptr->m_idx)
		{
			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x, 0);
		}

		/* Disarm chest */
		else if (o_idx)
		{
			/* Disarm the chest */
			more = do_cmd_disarm_chest(y, x, o_idx);
		}

		/* Disarm trap */
		else
		{
			/* Disarm the trap */
			more = do_cmd_disarm_aux(y, x, dir);
		}
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Perform the basic "bash" command
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x, int dir)
{
	int			bash, temp;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Message */
#ifdef JP
	msg_print("ドアに体当たりをした！");
#else
	msg_print("You smash into the door!");
#endif


	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (randint0(100) < temp)
	{
		/* Message */
#ifdef JP
		msg_print("ドアを壊した！");
#else
		msg_print("The door crashes open!");
#endif


		/* Break down the door */
		if (randint0(100) < 50)
		{
			cave_set_feat(y, x, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
		}

		/* Sound */
		sound(SOUND_OPENDOOR);

		/* Hack -- Fall through the door */
		move_player(dir, FALSE);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (randint0(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
	         p_ptr->lev)
	{
		/* Message */
#ifdef JP
		msg_print("このドアは頑丈だ。");
#else
		msg_print("The door holds firm.");
#endif


		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
#ifdef JP
		msg_print("体のバランスをくずしてしまった。");
#else
		msg_print("You are off-balance.");
#endif


		/* Hack -- Lose balance ala paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + 2 + randint0(2));
	}

	/* Result */
	return (more);
}


/*
 * Bash open a door, success based on character strength
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 */
void do_cmd_bash(void)
{
	int			y, x, dir;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		cave_type *c_ptr;
		byte feat;

		/* Bash location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Nothing useful */
		if (!((feat >= FEAT_DOOR_HEAD) &&
		      (feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
#ifdef JP
		msg_print("そこには体当たりするものが見当たらない。");
#else
			msg_print("You see nothing there to bash.");
#endif

		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x, 0);
		}

		/* Bash a closed door */
		else
		{
			/* Bash the door */
			more = do_cmd_bash_aux(y, x, dir);
		}
	}

	/* Unless valid action taken, cancel bash */
	if (!more) disturb(0, 0);
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn, to prevent free detection
 * of invisible monsters.
 */
void do_cmd_alter(void)
{
	int			y, x, dir;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction */
	if (get_rep_dir(&dir,TRUE))
	{
		cave_type *c_ptr;
		byte feat;

		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Take a turn */
		energy_use = 100;

		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			py_attack(y, x, 0);
		}

		/* Tunnel through walls */
		else if (((feat >= FEAT_RUBBLE) &&
		          (feat < FEAT_MINOR_GLYPH)) ||
		         ((feat == FEAT_TREES) ||
		          (feat == FEAT_MOUNTAIN)))
		{
			more = do_cmd_tunnel_aux(y, x);
		}

		else if (is_closed_door(feat))
		{
			/* Bash jammed doors */
			if (feat >= FEAT_DOOR_HEAD + 0x08)
			{
				more = do_cmd_bash_aux(y, x, dir);
			}

			/* Locked doors */
			else
			{
				more = do_cmd_open_aux(y, x);
			}
		}

		/* Close open doors */
		else if ((feat == FEAT_OPEN) ||
		         (feat == FEAT_BROKEN))
		{
			more = do_cmd_close_aux(y, x);
		}

		/* Disarm traps */
		else if (is_trap(feat))
		{
			more = do_cmd_disarm_aux(y, x, dir);
		}

		/* Oops */
		else
		{
			/* Oops */
#ifdef JP
			msg_print("何もない空中を攻撃した。");
#else
			msg_print("You attack the empty air.");
#endif

		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static bool get_spike(int *ip)
{
	int i;

	/* Check every item in the pack */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Save the spike index */
			(*ip) = i;

			/* Success */
			return (TRUE);
		}
	}

	/* Oops */
	return (FALSE);
}


/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int dir;


	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		int y, x, item;
		cave_type *c_ptr;
		byte feat;

		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Feature code (applying "mimic" field) */
		feat = c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic;

		/* Require closed door */
		if (!((feat >= FEAT_DOOR_HEAD) &&
		      (feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
#ifdef JP
		msg_print("そこにはくさびを打てるものが見当たらない。");
#else
			msg_print("You see nothing there to spike.");
#endif

		}

		/* Get a spike */
		else if (!get_spike(&item))
		{
			/* Message */
#ifdef JP
		msg_print("くさびを持っていない！");
#else
			msg_print("You have no spikes!");
#endif

		}

		/* Is a monster in the way? */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
#ifdef JP
		msg_print("モンスターが立ちふさがっている！");
#else
			msg_print("There is a monster in the way!");
#endif


			/* Attack */
			py_attack(y, x, 0);
		}

		/* Go for it */
		else
		{
			/* Take a turn */
			energy_use = 100;

			/* Successful jamming */
#ifdef JP
		msg_print("ドアにくさびを打ち込んだ。");
#else
			msg_print("You jam the door with a spike.");
#endif


			/* Convert "locked" to "stuck" XXX XXX XXX */
			if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

			/* Add one spike to the door */
			if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

			/* Use up, and describe, a single spike, from the bottom */
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
	}
}



/*
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(int pickup)
{
	int dir;

	bool more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Take a turn */
		energy_use = 100;

		/* Hack -- In small scale wilderness it takes MUCH more time to move */
		if (p_ptr->wild_mode) energy_use *= ((MAX_HGT + MAX_WID) / 2);

		/* Actually move the character */
		move_player(dir, pickup);

		/* Allow more walking */
		more = TRUE;
	}

	/* Hack again -- Is there a special encounter ??? */
	if (p_ptr->wild_mode && (cave[py][px].feat != FEAT_TOWN))
	{
		int tmp = 120 + p_ptr->lev*10 - wilderness[py][px].level + 5;
		if (tmp < 1) 
			tmp = 1;
		if (((wilderness[py][px].level + 5) > (p_ptr->lev / 2)) && randint0(tmp) < (21-p_ptr->skill_stl))
		{
			if ((wilderness[py][px].terrain != TERRAIN_DEEP_WATER) || !wilderness[py][px].road)
			{
				sound(SOUND_ENCOUNT);

				/* Inform the player of his horrible fate :=) */
				msg_print("FIGHT IT OUT!");

				/* Go into large wilderness view */
				p_ptr->wilderness_x = px;
				p_ptr->wilderness_y = py;
				p_ptr->oldpy = randint1(MAX_HGT-2);
				p_ptr->oldpx = randint1(MAX_WID-2);
				energy_use = 100;
				change_wild_mode();

				/* HACk -- set the encouter flag for the wilderness generation */
				generate_encounter = TRUE;
			}
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Start running.
 */
void do_cmd_run(void)
{
	int dir;

	/* Hack -- no running when confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて走れない！");
#else
		msg_print("You are too confused!");
#endif

		return;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Hack -- Set the run counter */
		running = (command_arg ? command_arg : 1000);

		/* First step */
		run_step(dir);
	}
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay(int pickup)
{
	cave_type *c_ptr = &cave[py][px];


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}


	/* Take a turn */
	energy_use = 100;


	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == randint0(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->action == ACTION_SEARCH)
	{
		search();
	}


	/* Handle "objects" */
	carry(pickup);


	/* Hack -- enter a store if we are on one */
	if (((c_ptr->feat >= FEAT_SHOP_HEAD) &&
	    (c_ptr->feat <= FEAT_SHOP_TAIL)) ||
	    (c_ptr->feat == FEAT_MUSEUM) ||
	    (c_ptr->feat == FEAT_DENEB_SHOP))
	{
		/* Disturb */
		disturb(0, 0);

		energy_use = 0;
		/* Hack -- enter store */
		command_new = SPECIAL_KEY_STORE;
	}

	/* Hack -- enter a building if we are on one -KMW- */
	else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
	    (c_ptr->feat <= FEAT_BLDG_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		energy_use = 0;
		/* Hack -- enter building */
		command_new = SPECIAL_KEY_BUILDING;
	}

	/* Exit a quest if reach the quest exit */
	else if (c_ptr->feat == FEAT_QUEST_EXIT)
	{
		int q_index = p_ptr->inside_quest;

		/* Was quest completed? */
		if (quest[q_index].type == QUEST_TYPE_FIND_EXIT)
		{
			quest[q_index].status = QUEST_STATUS_COMPLETED;
			quest[q_index].complev = (byte)p_ptr->lev;

			/* Make a sound */
			sound(SOUND_QUEST);

#ifdef JP
			msg_print("クエストを達成した！");
#else
			msg_print("You accomplished your quest!");
#endif

			msg_print(NULL);
			if (quest_is_fixed(q_index)) change_your_alignment(ALI_LNC, 10);
			if (quest_is_fixed(q_index) && !(quest[q_index].flags & QUEST_FLAG_ALI_CHAOS)) change_your_alignment(ALI_LNC, 10);
			else change_your_alignment(ALI_LNC, -10);

			if (quest[q_index].flags & QUEST_FLAG_ALI_EVIL) change_your_alignment(ALI_GNE, -10);
			else change_your_alignment(ALI_GNE, 10);
		}

		leave_quest_check();

		p_ptr->inside_quest = cave[py][px].special;
		dun_level = 0;
		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;

		p_ptr->leaving = TRUE;
	}
}



/*
 * Resting allows a player to safely restore his hp	-RAK-
 */
void do_cmd_rest(void)
{

	set_action(ACTION_NONE);

	if (p_ptr->singing || p_ptr->restart_singing) stop_singing();

	/* Prompt for time if needed */
	if (command_arg <= 0)
	{
#ifdef JP
		cptr p = "休憩 (0-9999, '*' で HP/MP全快, '&' で必要なだけ): ";
#else
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";
#endif


		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 4)) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			command_arg = (-1);
		}

		/* Rest some */
		else
		{
			command_arg = atoi(out_val);
			if (command_arg <= 0) return;
		}
	}


	/* Paranoia */
	if (command_arg > 9999) command_arg = 9999;

	/* Take a turn XXX XXX XXX (?) */
	energy_use = 100;

	if ((p_ptr->chp < p_ptr->mhp) ||
	    (p_ptr->csp < p_ptr->msp) ||
	     p_ptr->blind || p_ptr->confused ||
	     p_ptr->poisoned || p_ptr->afraid ||
	     p_ptr->stun || p_ptr->cut ||
	     p_ptr->slow || p_ptr->paralyzed ||
	     p_ptr->image || p_ptr->word_recall ||
	     p_ptr->alter_reality || p_ptr->inhibit_flood ||
	     p_ptr->tim_dec_blow)
	{
		if (one_in_(50)) change_your_alignment(ALI_LNC, -1);
	}

	/* Save the rest code */
	resting = command_arg;
	p_ptr->action = ACTION_REST;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	Term_fresh();
}


/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(object_type *o_ptr)
{
	int archer_bonus = (p_ptr->cexp_info[CLASS_ARCHER].clev + p_ptr->cexp_info[CLASS_CRESCENT].clev / 2 - 1) / 7;

	if ((p_ptr->pclass == CLASS_ARCHER) || (p_ptr->pclass == CLASS_CRESCENT)) archer_bonus += 4;

	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_ROCKET:
			return (100);

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_SKELETON:
			return (50);

		/* Sometimes break */
		case TV_WAND:
		case TV_SPIKE:
			return (25);
		case TV_ARROW:
			return (20 - archer_bonus * 2);

		/* Rarely break */
		case TV_BOLT:
			return (10 - archer_bonus);
		default:
			return (10);
	}
}


static s32b tot_dam_aux_shot(object_type *o_ptr, int tdam, monster_type *m_ptr, int xtra_slay)
{
	int mult = 10;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b flgs[TR_FLAG_SIZE];

	/* Hack -- "Throwing" */
	if (!o_ptr->k_idx) return tdam;

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	if (xtra_slay) add_flag(flgs, xtra_slay);

	if (inventory[INVEN_BOW].k_idx && (inventory[INVEN_BOW].name1 || object_is_runeweapon(&inventory[INVEN_BOW])))
	{
		u32b flgs_g[TR_FLAG_SIZE];
		int i;
		object_flags(&inventory[INVEN_BOW], flgs_g);
		for (i = 0; i < TR_FLAG_SIZE; i++)
			flgs[i] |= flgs_g[i];
	}

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Slay Animal */
			if ((have_flag(flgs, TR_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & RF3_ANIMAL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ANIMAL;
				}

				if (mult < 17) mult = 17;
			}

			/* Kill Animal */
			if ((have_flag(flgs, TR_KILL_ANIMAL)) &&
			    (r_ptr->flags3 & RF3_ANIMAL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ANIMAL;
				}

				if (mult < 27) mult = 27;
			}

			/* Slay Evil */
			if ((have_flag(flgs, TR_SLAY_EVIL)) &&
			    (r_ptr->flags3 & RF3_EVIL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_EVIL;
				}

				if (mult < 15) mult = 15;
			}

			/* Kill Evil */
			if ((have_flag(flgs, TR_KILL_EVIL)) &&
			    (r_ptr->flags3 & RF3_EVIL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_EVIL;
				}

				if (mult < 25) mult = 25;
			}

			/* Slay Good */
			if ((have_flag(flgs, TR_SLAY_GOOD)) &&
			    (r_ptr->flags3 & RF3_GOOD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GOOD;
				}

				if (mult < 15) mult = 15;
			}

			/* Kill Good */
			if ((have_flag(flgs, TR_KILL_GOOD)) &&
			    (r_ptr->flags3 & RF3_GOOD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GOOD;
				}

				if (mult < 25) mult = 25;
			}

			/* Slay Living */
			if ((have_flag(flgs, TR_SLAY_LIVING)) && monster_living(r_ptr))
			{
				if (mult < 20) mult = 20;
			}

			/* Kill Living */
			if ((have_flag(flgs, TR_KILL_LIVING)) && monster_living(r_ptr))
			{
				if (mult < 30) mult = 30;
			}

			/* Slay Human */
			if ((have_flag(flgs, TR_SLAY_HUMAN)) &&
			    (r_ptr->flags2 & RF2_HUMAN))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags2 |= RF2_HUMAN;
				}

				if (mult < 17) mult = 17;
			}

			/* Kill Human */
			if ((have_flag(flgs, TR_KILL_HUMAN)) &&
			    (r_ptr->flags2 & RF2_HUMAN))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags2 |= RF2_HUMAN;
				}

				if (mult < 27) mult = 27;
			}

			/* Slay Undead */
			if ((have_flag(flgs, TR_SLAY_UNDEAD)) &&
			    (r_ptr->flags3 & RF3_UNDEAD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_UNDEAD;
				}

				if (mult < 20) mult = 20;
			}

			/* Kill Undead */
			if ((have_flag(flgs, TR_KILL_UNDEAD)) &&
			    (r_ptr->flags3 & RF3_UNDEAD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_UNDEAD;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Demon */
			if ((have_flag(flgs, TR_SLAY_DEMON)) &&
			    (r_ptr->flags3 & RF3_DEMON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DEMON;
				}

				if (mult < 20) mult = 20;
			}

			/* Kill Demon */
			if ((have_flag(flgs, TR_KILL_DEMON)) &&
			    (r_ptr->flags3 & RF3_DEMON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DEMON;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Orc */
			if ((have_flag(flgs, TR_SLAY_ORC)) &&
			    (r_ptr->flags3 & RF3_ORC))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ORC;
				}

				if (mult < 20) mult = 20;
			}

			/* Kill Orc */
			if ((have_flag(flgs, TR_KILL_ORC)) &&
			    (r_ptr->flags3 & RF3_ORC))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ORC;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Troll */
			if ((have_flag(flgs, TR_SLAY_TROLL)) &&
			    (r_ptr->flags3 & RF3_TROLL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_TROLL;
				}

				if (mult < 20) mult = 20;
			}

			/* Kill Troll */
			if ((have_flag(flgs, TR_KILL_TROLL)) &&
			    (r_ptr->flags3 & RF3_TROLL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_TROLL;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Giant */
			if ((have_flag(flgs, TR_SLAY_GIANT)) &&
			    (r_ptr->flags3 & RF3_GIANT))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GIANT;
				}

				if (mult < 20) mult = 20;
			}

			/* Kill Giant */
			if ((have_flag(flgs, TR_KILL_GIANT)) &&
			    (r_ptr->flags3 & RF3_GIANT))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GIANT;
				}

				if (mult < 30) mult = 30;
			}

			/* Slay Dragon  */
			if ((have_flag(flgs, TR_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 20) mult = 20;
			}

			/* Execute Dragon */
			if ((have_flag(flgs, TR_KILL_DRAGON)) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 30) mult = 30;
			}

			/* Brand (Acid) */
			if (have_flag(flgs, TR_BRAND_ACID))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_ACID)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_ACID;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_ACID)
					{
						if (mult < 34) mult = 34;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_ACID;
						}
					}
					else if (mult < 17) mult = 17;
				}
			}

			/* Brand (Elec) */
			if (have_flag(flgs, TR_BRAND_ELEC))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_ELEC)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_ELEC;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_ELEC)
					{
						if (mult < 34) mult = 34;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_ELEC;
						}
					}
					else if (mult < 17) mult = 17;
				}
			}

			/* Brand (Fire) */
			if (have_flag(flgs, TR_BRAND_FIRE))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_FIRE)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_FIRE;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_FIRE)
					{
						if (mult < 34) mult = 34;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_FIRE;
						}
					}
					else if (mult < 17) mult = 17;
				}
			}

			/* Brand (Cold) */
			if (have_flag(flgs, TR_BRAND_COLD))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_COLD)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_COLD;
					}
				}
				/* Otherwise, take the damage */
				else
				{
					if (r_ptr->flags3 & RF3_HURT_COLD)
					{
						if (mult < 34) mult = 34;
						if (m_ptr->ml)
						{
							r_ptr->r_flags3 |= RF3_HURT_COLD;
						}
					}
					else if (mult < 17) mult = 17;
				}
			}

			/* Brand (Poison) */
			if (have_flag(flgs, TR_BRAND_POIS))
			{
				/* Notice resistance */
				if (r_ptr->flagsr & RFR_RES_POIS)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flagsr |= RFR_RES_POIS;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 17) mult = 17;
				}
			}

			/* Digging */
			if (have_flag(flgs, TR_TUNNEL) && (o_ptr->to_misc[OB_TUNNEL] > 0) &&
			    (r_ptr->flags3 & RF3_HURT_ROCK))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_HURT_ROCK;
				}

				if (mult < 34) mult = 34;
			}

			if ((have_flag(flgs, TR_FORCE_WEAPON)) && (p_ptr->csp > (p_ptr->msp / 30)))
			{
				p_ptr->csp -= (1+(p_ptr->msp / 30));
				p_ptr->redraw |= (PR_MANA);
				mult = mult * 5 / 2;
			}
			break;
		}
	}

	/* Return the total damage */
	return tdam * mult / 10;
}


static void get_shot_shell_target(int ty, int tx, int dist, int *ny, int *nx, int dev)
{
	int ly, lx, ld;

	/* Use an actual "target" */
	lx = dist * (tx - px) + px;
	ly = dist * (ty - py) + py;

	ld = distance(py, px, ly, lx);

	/* Blast */
	while (1)
	{
		/* Get targets for some bolts */
		*ny = rand_spread(ly, ld * dev / dist);
		*nx = rand_spread(lx, ld * dev / dist);

		if (distance(ly, lx, *ny, *nx) <= ld * dev / dist) break;
	}
}


static bool hit_rocket(int y, int x, object_type *o_ptr, object_type *launcher_ptr, int skill_to_d)
{
	int typ = GF_ROCKET;
	int dam;
	int rad = 2;
	int to_d = o_ptr->to_d + launcher_ptr->to_d + skill_to_d;

	/* Type */
	switch ((int)o_ptr->xtra3 - 1)
	{
	case TR_FORCE_WEAPON:
		typ = GF_MANA;
		break;

	case TR_CHAOTIC:
		typ = GF_CHAOS;
		break;

	case TR_VAMPIRIC:
		typ = GF_OLD_DRAIN;
		break;

	case TR_SLAY_EVIL:
		typ = GF_HOLY_FIRE;
		break;

	case TR_BRAND_POIS:
		typ = GF_POIS;
		break;

	case TR_BRAND_ACID:
		typ = GF_ACID;
		break;

	case TR_BRAND_ELEC:
		typ = GF_ELEC;
		break;

	case TR_BRAND_FIRE:
		typ = GF_FIRE;
		break;

	case TR_BRAND_COLD:
		typ = GF_COLD;
		break;

	case TR_SLAY_GOOD:
		typ = GF_HELL_FIRE;
		break;

	default:
		if (o_ptr->xtra4 >= ROCKET_ANTIGRAV) typ = GF_GRAVITY;
		break;
	}

	/* Radius */
	switch (o_ptr->xtra4)
	{
	case ROCKET_MATERIAL_NORMAL + ROCKET_ANTIGRAV:
		rad = 3;
		break;

	case ROCKET_MATERIAL_BALDAR + ROCKET_ANTIGRAV:
		rad = 4;
		break;

	case ROCKET_MATERIAL_MITHRIL + ROCKET_ANTIGRAV:
		rad = 5;
		break;

	case ROCKET_MATERIAL_ADAMANTITE + ROCKET_ANTIGRAV:
		rad = 6;
		break;
	}

	/* Damage */
	dam = rocket_damage(o_ptr, to_d);

	/* No negative damage */
	if (dam < 0) dam = 0;

	project(0, rad, y, x, dam, typ, (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL | PROJECT_GRID), MODIFY_ELEM_MODE_FIRE);
	if (o_ptr->xtra3 == (TR_IMPACT + 1)) earthquake(y, x, 5 + randint1(3));

	return TRUE;
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use guns + shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 *
 * shot_flgs are applied for various cases. see defines.h(DCFA_*)
 * x_to_h will be added to bonus
 * x_to_d will be added to damage
 */
bool do_cmd_fire_aux(int item, object_type *j_ptr, int shot_flgs, int x_to_h, int x_to_d, int penetrate, bool direct)
{
	int dir;
	int j, y, x, ny, nx, ty, tx, t_ty, t_tx;
	int tdam, tdis, thits, tmul;
	int bonus, chance;
	int cur_dis, visible;
	int count, n_fire = 1;
	int typ = GF_MISSILE;
	int xtra_slay = 0;
	bool used_up = FALSE;
	bool explode_rocket = FALSE;
	bool as_beam = FALSE;
	bool force_damege = FALSE;
	bool dcfa_thru = FALSE;

	object_type forge;
	object_type thrown;
	object_type *q_ptr;

	object_type *o_ptr;

	bool hit_body = FALSE;

	char o_name[MAX_NLEN];

	int msec = delay_factor * delay_factor * delay_factor;

	/* STICK TO */
	bool stick_to = FALSE;

	int modify_dam_mode = MODIFY_ELEM_MODE_FIRE;
	int attack_var, skill_to_d = 0;
	cptr base_name = NULL;

	if (p_ptr->pclass == CLASS_MEDIUM) xtra_slay = TR_SLAY_EVIL;

	if (shot_flgs & DCFA_STORM) xtra_slay = TR_BRAND_ELEC;
	if (shot_flgs & DCFA_HAMAYA) xtra_slay = TR_KILL_EVIL;
	if ((shot_flgs & DCFA_STORM) || (shot_flgs & DCFA_HAMAYA)) dcfa_thru = TRUE;

	/* XXX Hack - Make a fake item and copy a fake name */
	if ((item == INVEN_PEBBLE) || (item == INVEN_GRAVE))
	{
		int avgdam, turnavg;

		attack_var = skill_lev_var[p_ptr->skill_exp[SKILL_THROWING]/10];
		skill_to_d = attack_var - 1;

		o_ptr = &thrown;
		object_wipe(o_ptr);
		modify_dam_mode = MODIFY_ELEM_MODE_THROW;

		switch (item)
		{
		case INVEN_PEBBLE:
			o_ptr->dd = 1;
			o_ptr->ds = 1;
			o_ptr->weight = 4;
			o_ptr->number = 1;
			o_ptr->tval = TV_HAFTED;
#ifdef JP
			base_name = "小石";
#else
			base_name = "Pebble";
#endif
			break;

		case INVEN_GRAVE:
			o_ptr->dd = 2;
			o_ptr->ds = 1;
			o_ptr->weight = 400;
			o_ptr->number = 1;
			o_ptr->tval = TV_HAFTED;
#ifdef JP
			base_name = "岩";
#else
			base_name = "Boulder";
#endif
			break;
		}

		avgdam = o_ptr->dd * (o_ptr->ds + 1) * 10 / 2;
		avgdam += ((j_ptr->to_d + skill_to_d) * 10);
		tmul = bow_tmul(j_ptr);
		if ((j_ptr->sval != SV_ROCKET_LAUNCHER) && p_ptr->dis_xtra_might) tmul++;
		tmul = tmul * (100 + ((get_weapon_type(&k_info[j_ptr->k_idx]) != WT_GUN) ?
			((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) : 0));

		/* launcher multiplier */
		avgdam *= tmul;
		avgdam /= (100 * 10);
		if (avgdam < 0) avgdam = 0;

		if (p_ptr->num_fire == 0) turnavg = 0;
		else
		{
			/* calc effects of energy */
			turnavg = avgdam * (p_ptr->num_fire * 100);
			turnavg = turnavg / bow_energy(j_ptr);
		}

		sprintf(o_name, "%s (%dd%d) (+0,+0) (%d/%d)",
			base_name, o_ptr->dd, o_ptr->ds, avgdam, turnavg);
	}

	/* Access the item (if in the pack) */
	else
	{
		if (item >= 0)
		{
			o_ptr = &inventory[item];
		}
		else
		{
			o_ptr = &o_list[0 - item];
		}

		/* Describe the object */
		object_desc(o_name, o_ptr, OD_OMIT_PREFIX);

		if ((j_ptr->sval == SV_ASSAULT_RIFLE) || (j_ptr->sval == SV_RUNEGUN)) n_fire = 3;
		else if (p_ptr->tval_ammo == TV_SHELL) n_fire = 12;

		if ((j_ptr->sval == SV_PISTOL) && (o_ptr->sval == SV_MAGNUM_BULLET)) as_beam = TRUE;

		attack_var = skill_lev_var[p_ptr->weapon_exp[get_weapon_type(&k_info[inventory[INVEN_BOW].k_idx])]/10];
		skill_to_d = attack_var - 1;
	}


	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Use a base distance */
	tdis = 10;

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h_b + o_ptr->to_h + j_ptr->to_h) + x_to_h;
	chance = (p_ptr->skill_thb + (attack_var * 4 - 8 + bonus) * BTH_PLUS_ADJ);
	if (chance > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB)) chance = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB;

	energy_use = bow_energy(j_ptr);
	tmul = bow_tmul(j_ptr);

	/* Get extra "power" from "extra might" */
	if ((j_ptr->sval != SV_ROCKET_LAUNCHER) && p_ptr->xtra_might) tmul++;

	tmul = tmul * (100 + ((get_weapon_type(&k_info[j_ptr->k_idx]) != WT_GUN) ?
		((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) : 0));

	/* Base range */
	tdis = 10 + tmul/40;
	if ((j_ptr->sval == SV_BOWGUN) || (j_ptr->sval == SV_CROSSBOW))
		tdis -= 5;

	project_length = tdis + 1;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir))
	{
		energy_use = 0;

		/* need not to reset project_length (already did)*/

		return FALSE;
	}

	/* Take a (partial) turn */
	energy_use = (energy_use / thits);

	/* Predict the "target" location */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	project_length = 0; /* reset to default */

	for (count = 0; count < n_fire; count++)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Single object */
		q_ptr->number = 1;

		if ((item != INVEN_PEBBLE) && (item != INVEN_GRAVE))
		{
			if ((p_ptr->tval_ammo != TV_SHELL) || (count == (n_fire - 1)))
			{
				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -1);
					if (!o_ptr->number) used_up = TRUE;
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -1);
					if (!o_ptr->number) used_up = TRUE;
					floor_item_optimize(0 - item);
				}
			}

			/* Sound */
			switch (p_ptr->tval_ammo)
			{
			case TV_BULLET:
			case TV_ROUND:
			case TV_SHELL:
				sound(SOUND_SHOOT_GUN);
				break;
			case TV_ROCKET:
				sound(SOUND_ROCKET);
				break;
			case TV_ARROW:
			case TV_BOLT:
				sound(SOUND_SHOOT);
				break;
			}
		}

		/* Start at the player */
		y = py;
		x = px;

		/* Copy the new target */
		if (p_ptr->tval_ammo == TV_SHELL)
		{
			get_shot_shell_target(ty, tx, tdis + 1, &t_ty, &t_tx, 3);
		}
		else
		{
			t_ty = ty;
			t_tx = tx;
		}

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Travel until stopped */
		for (cur_dis = 0; cur_dis <= tdis; )
		{
			/* Hack -- Stop at the target */
			if (!(dcfa_thru) && (y == t_ty) && (x == t_tx))
			{
				if (p_ptr->tval_ammo == TV_ROCKET) explode_rocket = hit_rocket(y, x, q_ptr, j_ptr, skill_to_d);
				break;
			}

			/* Calculate the new location (see "project()") */
			ny = y;
			nx = x;
			mmove2(&ny, &nx, py, px, t_ty, t_tx);


			if ((cave[ny][nx].feat == FEAT_TREES) && (dcfa_thru))
			{
#ifdef JP
				msg_print("木は粉砕された。");
#else
				msg_print("A tree was crushed.");
#endif
				cave_set_feat(ny, nx, (one_in_(3) ? FEAT_DEEP_GRASS : FEAT_GRASS));

				if (one_in_(5)) change_your_alignment(ALI_LNC, -1);

				/* Update some things */
				p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

				/* Reduce duration */
				penetrate -= 7;
				if ((penetrate <= 0) && (!cave[ny][nx].m_idx))
				{
					x = nx;
					y = ny;
					break;
				}
			}

			/* Stopped by walls/doors */
			if (!cave_floor_bold(ny, nx) && !cave[ny][nx].m_idx)
			{
				if (shot_flgs & DCFA_STORM)
				{
					if (cave_perma_bold(ny, nx)) break;

					cave_force_set_floor(ny, nx);

					if (one_in_(5)) change_your_alignment(ALI_LNC, -1);

					/* Update some things */
					p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

					/* Reduce duration */
					penetrate -= 21;
					if ((penetrate <= 0) && (!cave[ny][nx].m_idx))
					{
						x = nx;
						y = ny;
						break;
					}
				}
				else if (p_ptr->tval_ammo == TV_ROCKET)
				{
					explode_rocket = hit_rocket(y, x, q_ptr, j_ptr, skill_to_d);
					break;
				}
				else
				{
					penetrate = 0;

					break;
				}
			}

			/* Advance the distance */
			cur_dis++;


			/* The player can see the (on screen) missile */
			if (panel_contains(ny, nx) && player_can_see_bold(ny, nx)
			    && (item != INVEN_PEBBLE) && (item != INVEN_GRAVE))
			{
				char c = object_char(q_ptr);
				byte a = object_attr(q_ptr);

				/* Draw, Hilite, Fresh, Pause, Erase */
				print_rel(c, a, ny, nx);
				move_cursor_relative(ny, nx);
				Term_fresh();
				Term_xtra(TERM_XTRA_DELAY, msec);
				lite_spot(ny, nx);
				Term_fresh();
			}

			/* The player cannot see the missile */
			else
			{
				/* Pause anyway, for consistancy */
				Term_xtra(TERM_XTRA_DELAY, msec);
			}

			/* Save the new location */
			x = nx;
			y = ny;


			/* Monster here, Try to hit it */
			if (cave[y][x].m_idx
				&& (!direct || ((y == t_ty) && (x == t_tx))))
			{
				cave_type *c_ptr = &cave[y][x];

				monster_type *m_ptr = &m_list[c_ptr->m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				int ac = r_ptr->ac + MON_STONING(m_ptr) / 5;

				/* Check the visibility */
				visible = m_ptr->ml;

				/* Note the collision */
				hit_body = TRUE;

				if (MON_CSLEEP(m_ptr))
				{
					if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) change_your_alignment(ALI_GNE, -1);
				}

				/* Hit effect */
				if (penetrate > 0)
				{
					/* The player can see the (on screen) missile */
					if (panel_contains(y, x) && player_can_see_bold(y, x))
					{
						u16b p = bolt_pict(y, x, y, x, GF_LITE);
						byte a = PICT_A(p);
						char c = PICT_C(p);
	
						/* Draw, Hilite, Fresh, Pause, Erase */
						print_rel(c, a, y, x);
						move_cursor_relative(y, x);
						Term_fresh();
						Term_xtra(TERM_XTRA_DELAY, msec);
						lite_spot(y, x);
						Term_fresh();
					}
	
					/* The player cannot see the missile */
					else
					{
						/* Pause anyway, for consistancy */
						Term_xtra(TERM_XTRA_DELAY, msec);
					}
				}
	
				/* Reduce duration */
				penetrate -= 5;
	
				/* Did we hit it (penalize range) */
				if (test_hit_fire(chance - cur_dis, ac, m_ptr->ml))
				{
					bool fear = FALSE;

					/* Handle unseen monster */
					if (!visible)
					{
						/* Invisible monster */
#ifdef JP
						msg_format("%sが敵を捕捉した。", o_name);
#else
						msg_format("The %s finds a mark.", o_name);
#endif

					}

					/* Handle visible monster */
					else
					{
						char m_name[80];

						/* Get "the monster" or "it" */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
#ifdef JP
						msg_format("%sが%sに命中した。", o_name, m_name);
#else
						msg_format("The %s hits %s.", o_name, m_name);
#endif


						/* Hack -- Track this monster race */
						if (m_ptr->ml) monster_race_track(m_ptr->ap_r_idx);

						/* Hack -- Track this monster */
						if (m_ptr->ml) health_track(c_ptr->m_idx);
					}

					if (p_ptr->tval_ammo != TV_ROCKET)
					{
						/* Base damage from thrown object plus launcher bonus */
						tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d + j_ptr->to_d + skill_to_d + x_to_d;

						/* Boost the damage */
						tdam *= tmul;
						tdam /= 100;

						/* Apply special damage XXX XXX XXX */
						tdam = tot_dam_aux_shot(q_ptr, tdam, m_ptr, xtra_slay);
						tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

						/* No negative damage */
						if (tdam < 0) tdam = 0;

						/* Modify the damage */
						if (shot_flgs & DCFA_PURE_WIND)
						{
							force_damege = TRUE;
							typ = GF_PURE_WIND;
						}
						tdam = mon_damage_mod(m_ptr, tdam, force_damege);
						if (tdam > 0) tdam = tdam * 10 / tot_dam_div(q_ptr, m_ptr, FALSE);
						tdam = modify_dam_by_elem(0, c_ptr->m_idx, tdam, typ, modify_dam_mode);

						/* Hit the monster, check for death */
						if (mon_take_hit(c_ptr->m_idx, tdam, &fear, extract_note_dies(r_ptr), FALSE))
						{
							/* Dead monster */

							/* Reduce duration */
							penetrate -= 1;
						}

						/* No death */
						else
						{
							/* STICK TO */
							if (q_ptr->name1)
							{
								char m_name[80];

								monster_desc(m_name, m_ptr, 0);

								stick_to = TRUE;
#ifdef JP
								msg_format("%sは%sに突き刺さった！",o_name, m_name);
#else
								msg_format("%^s have stuck into %s!",o_name, m_name);
#endif
								/* Reduce duration */
								penetrate -= 2;
							}
							else
							{
								/* Reduce duration */
								penetrate -= 5;
							}

							/* Message */
							message_pain(c_ptr->m_idx, tdam);

							/* Anger the monster */
							if (tdam > 0) anger_monster(m_ptr);

							/* Take note */
							if (fear && m_ptr->ml)
							{
								char m_name[80];

								/* Sound */
								sound(SOUND_FLEE);

								/* Get the monster name (or "it") */
								monster_desc(m_name, m_ptr, 0);

								/* Message */
#ifdef JP
								msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
								msg_format("%^s flees in terror!", m_name);
#endif

							}
							if (!projectable(m_ptr->fy, m_ptr->fx, py, px))
							{
								set_target(m_ptr, py, px);
							}
						}
					}
					else explode_rocket = hit_rocket(y, x, q_ptr, j_ptr, skill_to_d); /* TV_ROCKET */
				}
				else if (p_ptr->tval_ammo == TV_ROCKET) /* Missed it */
				{
					int attempt = 10;
					int tmp_dir;
					int rand_y, rand_x;
					do
					{
						tmp_dir = randint0(10);
						rand_y = y + ddy[tmp_dir];
						rand_x = x + ddx[tmp_dir];
						if (cave_empty_bold(rand_y, rand_x))
						{
							if (cave[rand_y][rand_x].feat != FEAT_AIR)
								explode_rocket = hit_rocket(rand_y, rand_x, q_ptr, j_ptr, skill_to_d);
							break;
						}
					}
					while (attempt);
				}

				/* Stop looking */
  				if (!as_beam || !cave_floor_bold(y, x) || penetrate <= 0) break;

			}
		}

		/* Stopped */
		if ((cur_dis > tdis) && (p_ptr->tval_ammo == TV_ROCKET) && !explode_rocket)
			hit_rocket(y, x, q_ptr, j_ptr, skill_to_d);

		/* Chance of breakage (during attacks) */
		j = (hit_body ? breakage_chance(q_ptr) : 0);
		if (shot_flgs & DCFA_PURE_WIND) j = 100;

		if(stick_to)
		{
			int m_idx = cave[y][x].m_idx;
			monster_type *m_ptr = &m_list[m_idx];
			int o_idx = o_pop();

			if (!o_idx)
			{
#ifdef JP
				msg_format("%sはどこかへ行った。", o_name);
#else
				msg_format("The %s have gone to somewhere.", o_name);
#endif
				if (q_ptr->name1)
				{
					a_info[j_ptr->name1].cur_num = 0;
				}
				return TRUE;
			}

			o_ptr = &o_list[ o_idx ];
			object_copy(o_ptr, q_ptr);

			/* Forget mark */
			o_ptr->marked &= OM_TOUCHED;

			/* Forget location */
			o_ptr->iy = o_ptr->ix = 0;

			/* Memorize monster */
			o_ptr->held_m_idx = m_idx;

			/* Build a stack */
			o_ptr->next_o_idx = m_ptr->hold_o_idx;

			/* Carry object */
			m_ptr->hold_o_idx = o_idx;

		}
		else if ((item == INVEN_PEBBLE) || (item == INVEN_GRAVE))
		{
#ifdef JP
			msg_format("%sは消えた。", base_name);
#else
			msg_format("The %s disappear.", base_name);
#endif
		}
		else if ((p_ptr->tval_ammo < TV_BULLET) || (p_ptr->tval_ammo > TV_ROCKET))
		{
			/* Drop (or break) near that location (Except ammo of guns) */
			(void)drop_near(q_ptr, j, y, x);
		}
		else
		{
#ifdef JP
			msg_format("弾は消えた。");
#else
			msg_format("The ammo disappear.");
#endif
		}

		/* Rifle rounds are used up */
		if (((j_ptr->sval == SV_ASSAULT_RIFLE) || (j_ptr->sval == SV_RUNEGUN)) && used_up) break;
	}

	return TRUE;
}


bool do_cmd_fire(int shot_flgs, int x_to_h, int x_to_d, int penetrate, bool direct)
{
	int item;
	object_type *j_ptr;
	cptr q, s;

	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Require a launcher */
	if (!j_ptr->tval)
	{
#ifdef JP
		msg_print("射撃用の武器を持っていない。");
#else
		msg_print("You have nothing to fire with.");
#endif
		flush();
		return FALSE;
	}

	/* Require proper missile */
	item_tester_tval = p_ptr->tval_ammo;

	/* Get an item */
#ifdef JP
	q = "どれを撃ちますか? ";
	s = "発射されるアイテムがありません。";
#else
	q = "Fire which item? ";
	s = "You have nothing to fire.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
	{
		flush();
		return FALSE;
	}

	/* Fire the item */
	return do_cmd_fire_aux(item, j_ptr, shot_flgs, x_to_h, x_to_d, penetrate, direct);
}


/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
bool do_cmd_throw_aux(int mult, u16b mode, int chosen_item)
{
	int dir, item;
	int i, j, y, x, ty, tx;
	int *ny, *nx;
	int chance, tdam, tdis;
	int mul, div;
	int cur_dis, visible;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	bool hit_body = FALSE;
	bool hit_wall = FALSE;
	bool equiped_item = FALSE;
	bool return_when_thrown = FALSE;

	char o_name[MAX_NLEN];

	int msec = delay_factor * delay_factor * delay_factor;

	u32b flgs[TR_FLAG_SIZE];
	cptr q, s;
	bool come_back = FALSE;
	bool do_drop = TRUE;
	bool shuriken = FALSE;
	bool boomerang = (mode & PY_THROW_BOOMERANG) ? TRUE : FALSE;
	bool shooting_star = (mode & PY_THROW_SHOOTING_STAR) ? TRUE : FALSE;

	int ac = 0;

	int attack_var = skill_lev_var[p_ptr->skill_exp[SKILL_THROWING]/10];


	if (mode & PY_THROW_CHOSEN)
	{
		item = chosen_item;
	}
	else if (boomerang)
	{
		if (buki_motteruka(INVEN_LARM))
		{
			item_tester_hook = object_is_melee_weapon;
#ifdef JP
			q = "どの武器を投げますか? ";
			s = "投げる武器がない。";
#else
			q = "Throw which item? ";
			s = "You have nothing to throw.";
#endif

			if (!get_item(&item, q, s, (USE_EQUIP)))
			{
				flush();
				return FALSE;
			}
		}
		else
		{
			item = INVEN_RARM;
		}
	}
	else
	{
		/* Get an item */
#ifdef JP
		q = "どのアイテムを投げますか? ";
		s = "投げるアイテムがない。";
#else
		q = "Throw which item? ";
		s = "You have nothing to throw.";
#endif

		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_EQUIP)))
		{
			flush();
			return FALSE;
		}
	}

	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}


	if ((p_ptr->pclass == CLASS_BERSERKER) && (o_ptr->tval == TV_POLEARM)
		&& ((o_ptr->sval == SV_FRANCISCA) || (o_ptr->sval == SV_RUNEAXE)))
			boomerang = TRUE;

	if (dungeon_type == DUNGEON_HEAVEN)
	{
		if (object_is_known(o_ptr) && (o_ptr->name1 == ART_BRUNHILD))
		{
			msg_print("天界から帰還するにはブリュンヒルドが必要です。");
			return FALSE;
		}
	}

	/* Item is cursed */
	if (object_is_cursed(o_ptr) && (item >= INVEN_RARM))
	{
		/* Oops */
#ifdef JP
		msg_print("ふーむ、どうやら呪われているようだ。");
#else
		msg_print("Hmmm, it seems to be cursed.");
#endif

		/* Nope */
		return FALSE;
	}

	if (p_ptr->inside_arena && !boomerang)
	{
#ifdef JP
		msg_print("アリーナではアイテムを使えない！");
#else
		msg_print("You're in the arena now. This is hand-to-hand!");
#endif
		msg_print(NULL);

		/* Nope */
		return FALSE;
	}

	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Extract the thrown object's flags. */
	object_flags(q_ptr, flgs);

	/* Distribute the charges of rods/wands between the stacks */
	distribute_charges(o_ptr, q_ptr, 1);

	/* Single object */
	q_ptr->number = 1;

	/* Description */
	object_desc(o_name, q_ptr, OD_OMIT_PREFIX);

	if (p_ptr->mighty_throw) mult += 3;
	if (shooting_star) mult += 3;
	mult += attack_var - 1;

	/* Extract a "distance multiplier" */
	/* Changed for 'launcher' mutation */
	mul = 10 + 2 * (mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);
	if ((have_flag(flgs, TR_THROW)) || boomerang) div /= 2;

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	project_length = tdis + 1;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return FALSE;

	/* Predict the "target" location */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	C_MAKE(ny, tdis + 1, int);
	C_MAKE(nx, tdis + 1, int);
	project_length = 0;  /* reset to default */

	if (boomerang)
		return_when_thrown = TRUE;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		if (!return_when_thrown)
			inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}
	if (item >= INVEN_RARM)
	{
		equiped_item = TRUE;
		p_ptr->redraw |= (PR_EQUIPPY);
	}

	/* Take partial turn */
	energy_use = 100 - attack_var * attack_var;

	/* Start at the player */
	y = py;
	x = px;


	/* Hack -- Handle stuff */
	handle_stuff();

	if (((p_ptr->pclass == CLASS_NINJA) || (p_ptr->pclass == CLASS_NINJAMASTER)) && (have_flag(flgs, TR_THROW)) && (q_ptr->tval == TV_SWORD)) shuriken = TRUE;
	else shuriken = FALSE;

	/* Chance of hitting */
	if (shooting_star || have_flag(flgs, TR_THROW))
		chance = ((p_ptr->skill_tht) + ((p_ptr->to_h_b + q_ptr->to_h) * BTH_PLUS_ADJ));
	else chance = (p_ptr->skill_tht + (p_ptr->to_h_b * BTH_PLUS_ADJ));
	chance += (attack_var - 1) * 6;

	if (shuriken) chance *= 2;
	if (chance > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB)) chance = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB;

	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		ny[cur_dis] = y;
		nx[cur_dis] = x;
		mmove2(&ny[cur_dis], &nx[cur_dis], py, px, ty, tx);

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny[cur_dis], nx[cur_dis]))
		{
			int yy = ny[cur_dis];
			int xx = nx[cur_dis];

			if (shooting_star)
			{
				if (cave_perma_bold(yy, xx))
				{
					hit_wall = TRUE;
					break;
				}

				cave_force_set_floor(yy, xx);

				/* Notice */
				note_spot(yy, xx);

				/* Update some things */
				p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
				update_stuff();
			}
			else
			{
				hit_wall = TRUE;
				break;
			}
		}

		/* Advance the distance */
		cur_dis++;

		/* The player can see the (on screen) missile */
		if (panel_contains(ny[cur_dis-1], nx[cur_dis-1]) && player_can_see_bold(ny[cur_dis-1], nx[cur_dis-1]))
		{
			char c = object_char(q_ptr);
			byte a = object_attr(q_ptr);

			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(c, a, ny[cur_dis-1], nx[cur_dis-1]);
			move_cursor_relative(ny[cur_dis-1], nx[cur_dis-1]);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(ny[cur_dis-1], nx[cur_dis-1]);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Save the new location */
		x = nx[cur_dis-1];
		y = ny[cur_dis-1];


		/* Monster here, Try to hit it */
		if (cave[y][x].m_idx)
		{
			cave_type *c_ptr = &cave[y][x];

			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			ac = (shooting_star ? ac : 0) + r_ptr->ac + MON_STONING(m_ptr) / 5;

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance - cur_dis, ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
#ifdef JP
					msg_format("%sが敵を捕捉した。", o_name);
#else
					msg_format("The %s finds a mark.", o_name);
#endif

				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
#ifdef JP
					msg_format("%sが%sに命中した。", o_name, m_name);
#else
					msg_format("The %s hits %s.", o_name, m_name);
#endif


					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->ap_r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Mega-Hack - Berserker throws with mighty dice */
				{
					int hack_dd = q_ptr->dd;

					if ((p_ptr->pclass == CLASS_BERSERKER) && (q_ptr->tval == TV_POLEARM)
						&& ((q_ptr->sval == SV_FRANCISCA) || (q_ptr->sval == SV_RUNEAXE)))
					{
						hack_dd += (1 + p_ptr->cexp_info[CLASS_BERSERKER].clev / 25);
					}

					/* Hack -- Base damage from thrown object */
					tdam = damroll(hack_dd, q_ptr->ds);
				}
				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr, FALSE);
				tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);
				if (q_ptr->to_d > 0)
					tdam += q_ptr->to_d;
				else
					tdam += -q_ptr->to_d;

				if (boomerang)
				{
					tdam *= (mult+p_ptr->num_blow[item - INVEN_RARM]);
					tdam += p_ptr->to_d_m;
				}
				else if (have_flag(flgs, TR_THROW))
				{
					tdam *= (3+mult);
					tdam += p_ptr->to_d_m;
				}
				else
				{
					tdam *= mult;
				}
				if (shuriken)
				{
					int clev = p_ptr->cexp_info[CLASS_NINJA].clev;
					if (p_ptr->cexp_info[CLASS_NINJA].clev < p_ptr->cexp_info[CLASS_NINJAMASTER].clev)
						clev = p_ptr->cexp_info[CLASS_NINJAMASTER].clev;
					tdam += ((clev+30)*(clev+30)-900)/55;
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Modify the damage */
				tdam = mon_damage_mod(m_ptr, tdam, FALSE);
				if (tdam > 0) tdam = tdam * 10 / tot_dam_div(q_ptr, m_ptr, FALSE);
				tdam = modify_dam_by_elem(0, c_ptr->m_idx, tdam, GF_MISSILE, MODIFY_ELEM_MODE_THROW);

				/* Hit the monster, check for death */
				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, extract_note_dies(r_ptr), FALSE))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(c_ptr->m_idx, tdam);

					/* Anger the monster */
					if ((tdam > 0) && !object_is_potion(q_ptr))
						anger_monster(m_ptr);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Sound */
						sound(SOUND_FLEE);

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
#ifdef JP
						msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
						msg_format("%^s flees in terror!", m_name);
#endif

					}
				}
			}

			/* Stop looking */
			if (!cave_floor_bold(y, x))
			{
				hit_wall = TRUE;
				break;
			}
			if (!shooting_star) break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(q_ptr) : 0);
	if (shooting_star) j = 0;

	/* Figurines transform */
	if ((q_ptr->tval == TV_FIGURINE) && !(p_ptr->inside_arena))
	{
		j = 100;

		if (!(summon_named_creature((object_is_cursed(q_ptr) ? 0 : -1), y, x, q_ptr->pval,
					    !(object_is_cursed(q_ptr)) ? PM_FORCE_PET : PM_IGNORE_AMGRID)))
#ifdef JP
			msg_print("人形は捻じ曲がり砕け散ってしまった！");
#else
			msg_print("The Figurine writhes and then shatters.");
#endif

		else if (object_is_cursed(q_ptr))
#ifdef JP
			msg_print("これはあまり良くない気がする。");
#else
			msg_print("You have a bad feeling about this.");
#endif

	}


	/* Potions smash open */
	if (object_is_potion(q_ptr))
	{
		if (hit_body || hit_wall || (randint1(100) < j))
		{
			/* Message */
#ifdef JP
			msg_format("%sは砕け散った！", o_name);
#else
			msg_format("The %s shatters!", o_name);
#endif


			if (potion_smash_effect(0, y, x, q_ptr->k_idx))
			{
				monster_type *m_ptr = &m_list[cave[y][x].m_idx];

				/* ToDo (Robert): fix the invulnerability */
				if (cave[y][x].m_idx &&
				    is_friendly(&m_list[cave[y][x].m_idx]) &&
				    !MON_INVULNER(m_ptr))
				{
					char m_name[80];
					monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
#ifdef JP
					msg_format("%sは怒った！", m_name);
#else
					msg_format("%^s gets angry!", m_name);
#endif

					set_hostile(&m_list[cave[y][x].m_idx]);
				}
			}
			do_drop = FALSE;
		}
		else
		{
			j = 0;
		}
	}

	if (return_when_thrown)
	{
		int back_chance = randint1(30)+20+((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
		char o2_name[MAX_NLEN];

		j = -1;
		if (boomerang) back_chance += 4+randint1(5);
		object_desc(o2_name, q_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

		if((back_chance > 30) && !one_in_(100))
		{
			for (i = cur_dis-1;i>0;i--)
			{
				if (panel_contains(ny[i], nx[i]) && player_can_see_bold(ny[i], nx[i]))
				{
					char c = object_char(q_ptr);
					byte a = object_attr(q_ptr);

					/* Draw, Hilite, Fresh, Pause, Erase */
					print_rel(c, a, ny[i], nx[i]);
					move_cursor_relative(ny[i], nx[i]);
					Term_fresh();
					Term_xtra(TERM_XTRA_DELAY, msec);
					lite_spot(ny[i], nx[i]);
					Term_fresh();
				}
				else
				{
					/* Pause anyway, for consistancy */
					Term_xtra(TERM_XTRA_DELAY, msec);
				}
			}
			if((back_chance > 37) && !p_ptr->blind && (item >= 0))
			{
#ifdef JP
				msg_format("%sが手元に返ってきた。", o2_name);
#else
				msg_format("%s comes back to you.", o2_name);
#endif
				come_back = TRUE;
			}
			else
			{
				if (item >= 0)
				{
#ifdef JP
					msg_format("%sを受け損ねた！", o2_name);
#else
					msg_format("%s backs, but you can't catch!", o2_name);
#endif
				}
				else
				{
#ifdef JP
					msg_format("%sが返ってきた。", o2_name);
#else
					msg_format("%s comes back.", o2_name);
#endif
				}
				y = py;
				x = px;
			}
		}
		else
		{
#ifdef JP
			msg_format("%sが返ってこなかった！", o2_name);
#else
			msg_format("%s doesn't back!", o2_name);
#endif
		}
	}

	if (come_back)
	{
		if (item == INVEN_RARM || item == INVEN_LARM)
		{
			/* Access the wield slot */
			o_ptr = &inventory[item];

			/* Wear the new stuff */
			object_copy(o_ptr, q_ptr);

			/* Increase the weight */
			p_ptr->total_weight += q_ptr->weight;

			/* Increment the equip counter by hand */
			equip_cnt++;

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Recalculate torch */
			p_ptr->update |= (PU_TORCH);

			/* Recalculate mana XXX */
			p_ptr->update |= (PU_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}
		else
		{
			inven_carry(q_ptr);
		}
		do_drop = FALSE;
	}
	else if (equiped_item)
	{
		kamaenaoshi(item);
	}

	/* Drop (or break) near that location */
	if (do_drop)
	{
		s16b tmp_o_idx = drop_near(q_ptr, j, y, x);

		if (item == INVEN_RARM)
		{
			bool disappear = tmp_o_idx ? FALSE : TRUE;
			set_magical_weapon(0, 0, 0 - tmp_o_idx, disappear);
			set_evil_weapon(0, TRUE, 0 - tmp_o_idx, disappear);
		}
	}

	C_KILL(ny, tdis + 1, int);
	C_KILL(nx, tdis + 1, int);
	return TRUE;
}


/*
 * Throw an object from the pack or floor.
 */
void do_cmd_throw(void)
{
	do_cmd_throw_aux(1, 0, 0);
}
