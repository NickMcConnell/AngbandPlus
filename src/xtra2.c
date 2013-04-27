/* File: xtra2.c */

/* Purpose: effects of various "objects" */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define REWARD_CHANCE 10


/*
 * Choice status to gain
 */
static void choice_gain_stat(void)
{
	int choice;

	/* Save the screen */
	screen_save();

	/* Get status to gain */
	while(1)
	{
		int n;
		char tmp[32];

		/* Display stats */
#ifdef JP
		cnv_stat(p_ptr->stat_max[0], tmp);
		prt(format("        a) 腕力 (現在値 %s)", tmp), 2, 14);
		cnv_stat(p_ptr->stat_max[1], tmp);
		prt(format("        b) 知能 (現在値 %s)", tmp), 3, 14);
		cnv_stat(p_ptr->stat_max[2], tmp);
		prt(format("        c) 賢さ (現在値 %s)", tmp), 4, 14);
		cnv_stat(p_ptr->stat_max[3], tmp);
		prt(format("        d) 器用 (現在値 %s)", tmp), 5, 14);
		cnv_stat(p_ptr->stat_max[4], tmp);
		prt(format("        e) 耐久 (現在値 %s)", tmp), 6, 14);
		cnv_stat(p_ptr->stat_max[5], tmp);
		prt(format("        f) 魅力 (現在値 %s)", tmp), 7, 14);
		prt("", 8, 14);
		prt("        どの能力値を上げますか？", 1, 14);
#else
		cnv_stat(p_ptr->stat_max[0], tmp);
		prt(format("        a) Str (cur %s)", tmp), 2, 14);
		cnv_stat(p_ptr->stat_max[1], tmp);
		prt(format("        b) Int (cur %s)", tmp), 3, 14);
		cnv_stat(p_ptr->stat_max[2], tmp);
		prt(format("        c) Wis (cur %s)", tmp), 4, 14);
		cnv_stat(p_ptr->stat_max[3], tmp);
		prt(format("        d) Dex (cur %s)", tmp), 5, 14);
		cnv_stat(p_ptr->stat_max[4], tmp);
		prt(format("        e) Con (cur %s)", tmp), 6, 14);
		cnv_stat(p_ptr->stat_max[5], tmp);
		prt(format("        f) Chr (cur %s)", tmp), 7, 14);
		prt("", 8, 14);
		prt("        Which stat do you want to raise?", 1, 14);
#endif

		/* Choice */
		while(1)
		{
			choice = inkey();
			if ((choice >= 'a') && (choice <= 'f')) break;
		}

		/* Delete other stats */
		for(n = 0; n < 6; n++)
			if (n != choice - 'a')
				prt("",n+2,14);

		/* Confirm */
#ifdef JP
		if (get_check("よろしいですか？")) break;
#else
		if (get_check("Are you sure? ")) break;
#endif
	}

	/* Gain stat */
	do_inc_stat(choice - 'a');

	/* Restore the screen */
	screen_load();
}  

extern void gain_level_reward(int chosen_reward);

/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	bool level_reward = FALSE;
	bool inc_stat_okay = FALSE;
	int  old_lev = p_ptr->lev;

	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- lower limit */
	if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;

	/* Hack -- maintain "max" experience */
	if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

	if (!exp_need)
	{
		/* Redraw experience */
		p_ptr->redraw |= (PR_EXP);

		/* Handle stuff */
		handle_stuff();
	}


	/* Lose levels while possible */
	while ((p_ptr->lev > 1) &&
		(p_ptr->exp < (player_exp[p_ptr->lev - 2] * p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;
		lite_spot(py, px);

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_STATS);

		/* Handle stuff */
		handle_stuff();
	}


	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
	       (p_ptr->exp >= (player_exp[p_ptr->lev-1] * p_ptr->expfact / 100L)))
	{
		/* Gain a level */
		p_ptr->lev++;
		lite_spot(py, px);

		/*
		 * If auto-note taking enabled, write a note to the file.
		 * Only write this note when the level is gained for the first time.
		 */
		if (take_notes && auto_notes && (p_ptr->lev > p_ptr->max_plv))
		{
			char note[80];

			/* Write note */
			sprintf(note, "%d", p_ptr->lev);

			add_note(note, 'L');
		}

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_plv)
		{
			p_ptr->max_plv = p_ptr->lev;

			level_reward = TRUE;

			/* You can gain stat only when reaching max level */
			inc_stat_okay = TRUE;
		}

		/* Sound */
		sound(SOUND_LEVEL);

		/* Message */
#ifdef JP
		msg_format("レベル %d にようこそ。", p_ptr->lev);
#else
		msg_format("Welcome to level %d.", p_ptr->lev);
#endif

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_SPELL | PW_STATS);

#ifdef JP
/* XTRA HACK LVUP */
		level_up = 1;
#endif
		/* Handle stuff */
		handle_stuff();

#ifdef JP
/* XTRA HACK LVUP */
		level_up = 0;
#endif
		if (inc_stat_okay)
		{
			/* You can gain stat per 5 level */
			if(!(p_ptr->max_plv % 5)) choice_gain_stat();

			/* gain stat randomly */
			else
			{
				int n;

				if (one_in_(3))
					n = valar_stats[p_ptr->valar_patron];
				else
					n = randint0(6);
				do_inc_stat(n);
			}
		}

		if (level_reward)
		{
			gain_level_reward(0);
			level_reward = FALSE;
		}
	}

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();

	/* Load an autopick preference file */
	if (old_lev != p_ptr->lev) autopick_load_pref(FALSE);
}


/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * XXX XXX XXX Note the use of actual "monster names"
 */
static int get_coin_type(int r_idx)
{
	monster_race    *r_ptr = &r_info[r_idx];
#if 0
	/* Analyze "coin" monsters */
	if (r_ptr->d_char == '$')
	{
		/* Look for textual clues */
		switch (r_idx)
		{
		case MON_COPPER_COINS: return (2);
		case MON_SILVER_COINS: return (5);
		case MON_GOLD_COINS: return (10);
		case MON_MITHRIL_COINS: return (16);
		case MON_ADAMANT_COINS: return (17);
		}
	}
#endif
	/* Assume nothing */
	return (0);
}

/*
 * Hack -- determine if a template is Sword
 */
static bool kind_is_sword(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if ((k_ptr->tval == TV_SWORD) && (k_ptr->sval > 2))
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}

/*
 * Hack -- determine if a template is Cloak
 */
static bool kind_is_cloak(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if (k_ptr->tval == TV_CLOAK)
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}

#ifndef TINYANGBAND
/*
 * Hack -- determine if a template is Book
 */
static bool kind_is_book(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if ((k_ptr->tval >= TV_LIFE_BOOK) && (k_ptr->tval <= TV_SORCERY_BOOK))
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Hack -- determine if a template is Good book
 */
static bool kind_is_good_book(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item type */
	if ((k_ptr->tval >= TV_LIFE_BOOK) && (k_ptr->tval <= TV_SORCERY_BOOK) && (k_ptr->sval > 1))
	{
		return (TRUE);
	}

	/* Assume not good */
	return (FALSE);
}
#endif


void check_quest_completion(monster_type *m_ptr)
{
	int i, y, x, ny, nx, i2, j2;

	int quest_num;

	bool create_stairs = FALSE;
	bool reward = FALSE;

	object_type forge;
	object_type *q_ptr;

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Inside a quest */
	quest_num = p_ptr->inside_quest;

	/* Search for an active quest on this dungeon level */
	if (!quest_num)
	{
		for (i = max_quests - 1; i > 0; i--)
		{
			/* Quest is not active */
			if (quest[i].status != QUEST_STATUS_TAKEN)
				continue;

			/* Quest is not a dungeon quest */
			if (quest[i].flags & QUEST_FLAG_PRESET)
				continue;

			/* Quest is not on this level */
			if ((quest[i].level != dun_level) &&
				 (quest[i].type != QUEST_TYPE_KILL_ANY_LEVEL))
				continue;

			/* Not a "kill monster" quest */
			if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) ||
			    (quest[i].type == QUEST_TYPE_FIND_EXIT))
				continue;

			/* Interesting quest */
			if ((quest[i].type == QUEST_TYPE_KILL_NUMBER) ||
			    (quest[i].type == QUEST_TYPE_KILL_ALL))
				break;

			/* Interesting quest */
			if (((quest[i].type == QUEST_TYPE_KILL_LEVEL) ||
			     (quest[i].type == QUEST_TYPE_KILL_ANY_LEVEL) ||
			     (quest[i].type == QUEST_TYPE_RANDOM)) &&
			     (quest[i].r_idx == m_ptr->r_idx))
				break;
		}

		quest_num = i;
	}

	/* Handle the current quest */
	if (quest_num && (quest[quest_num].status == QUEST_STATUS_TAKEN))
	{
		/* Current quest */
		i = quest_num;

		switch (quest[i].type)
		{
			case QUEST_TYPE_KILL_NUMBER:
			{
				quest[i].cur_num++;

				if (quest[i].cur_num >= quest[i].num_mon)
				{
					/* completed quest */
					quest[i].status = QUEST_STATUS_COMPLETED;
					quest[i].complev = (byte)p_ptr->lev;

					/* Take note */
					if (take_notes && record_quest)
					{
						char note[80];
#ifdef JP
						sprintf(note, "「%s」", quest[i].name);
#else
						sprintf(note, "'%s'", quest[i].name);
#endif
						add_note(note, 'Q');
					}

					if (!(quest[i].flags & QUEST_FLAG_SILENT))
					{
#ifdef JP
						msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif
	  					sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
						msg_print(NULL);
					}

					quest[i].cur_num = 0;
				}
				break;
			}
			case QUEST_TYPE_KILL_ALL:
			{
				int number_mon = 0;

				if (!is_hostile(m_ptr)) break;

				/* Count all hostile monsters */
				for (i2 = 0; i2 < cur_wid; ++i2)
					for (j2 = 0; j2 < cur_hgt; j2++)
						if (cave[j2][i2].m_idx > 0)
							if (is_hostile(&m_list[cave[j2][i2].m_idx]))
								number_mon++;

				if ((number_mon - 1) == 0)
				{
					/* completed */
					if (quest[i].flags & QUEST_FLAG_SILENT)
					{
						quest[i].status = QUEST_STATUS_FINISHED;
					}
					else
					{
						/* Take note */
						if (take_notes && record_quest)
						{
							char note[80];
#ifdef JP
							sprintf(note, "「%s」", quest[i].name);
#else
							sprintf(note, "'%s'", quest[i].name);
#endif
							add_note(note, 'Q');
						}

						quest[i].status = QUEST_STATUS_COMPLETED;
						quest[i].complev = (byte)p_ptr->lev;
#ifdef JP
						msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif
	  					sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
						msg_print(NULL);
					}
				}
				break;
			}
			case QUEST_TYPE_KILL_LEVEL:
			case QUEST_TYPE_RANDOM:
			{
				/* Only count valid monsters */
				if (quest[i].r_idx != m_ptr->r_idx)
					break;

				quest[i].cur_num++;

				if (quest[i].cur_num >= quest[i].max_num)
				{
					/* completed quest */
					quest[i].status = QUEST_STATUS_COMPLETED;
					quest[i].complev = (byte)p_ptr->lev;
					if (!(quest[i].flags & QUEST_FLAG_PRESET))
					{
						p_ptr->inside_quest = 0;
						create_stairs = TRUE;
					}

					/* Take note */
					if (take_notes && record_quest)
					{
						char note[80];

						if ((r_info[quest[i].r_idx].flags1) & RF1_UNIQUE) 
#ifdef JP
							sprintf(note, "（%s）", (r_name + r_info[quest[i].r_idx].name));
#else
							sprintf(note, "(%s)", (r_name + r_info[quest[i].r_idx].name));
#endif
						else
#ifdef JP
							sprintf(note, "（%d体の%s）", quest[i].max_num, (r_name + r_info[quest[i].r_idx].name));
#else
							sprintf(note, "(%d %s)", quest[i].max_num, (r_name + r_info[quest[i].r_idx].name));
#endif
						add_note(note, 'Q');
					}
					
					if (!(quest[i].flags & QUEST_FLAG_SILENT))
					{
#ifdef JP
						msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif
	  					sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
						msg_print(NULL);
					}

					/* Finish the two main quests without rewarding */
					if ((i == QUEST_SAURON) || (i == QUEST_MORGOTH))
					{
						quest[i].status = QUEST_STATUS_FINISHED;
					}

					if (quest[i].type == QUEST_TYPE_RANDOM)
					{
						reward = TRUE;
						quest[i].status = QUEST_STATUS_FINISHED;
					}
				}
				break;
			}
			case QUEST_TYPE_KILL_ANY_LEVEL:
			{
				quest[i].cur_num++;
				if (quest[i].cur_num >= quest[i].max_num)
				{
					 /* completed quest */
					quest[i].status = QUEST_STATUS_COMPLETED;
					quest[i].complev = (byte)p_ptr->lev;

					/* Take note */
					if (take_notes && record_quest)
					{
						char note[80];
#ifdef JP
						sprintf(note, "「%s」", quest[i].name);
#else
						sprintf(note, "'%s'", quest[i].name);
#endif
						add_note(note, 'Q');
					}

					if (!(quest[i].flags & QUEST_FLAG_SILENT))
					{
#ifdef JP
						msg_print("クエストを達成した！");
#else
						msg_print("You just completed your quest!");
#endif
	  					sound(SOUND_LEVEL); /* (Sound substitute) No quest sound */
						msg_print(NULL);
					}
					quest[i].cur_num = 0;
				}
				break;
			}
		}
	}

	/* Create a magical staircase */
	if (create_stairs)
	{
		/* Stagger around */
		while (cave_perma_bold(y, x) || cave[y][x].o_idx)
		{
			/* Pick a location */
			scatter(&ny, &nx, y, x, 1, 0);

			/* Stagger */
			y = ny; x = nx;
		}

		/* Explain the staircase */
#ifdef JP
		msg_print("魔法の階段が現れた...");
#else
		msg_print("A magical staircase appears...");
#endif

		/* Create stairs down */
		cave_set_feat(y, x, FEAT_MORE);

		/* Remember to update everything */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_MONSTERS);
	}

	/*
	 * Drop quest reward
	 */
	if (reward)
	{
		int reward_num = MAX((dun_level / 10) + 1, 2);

		while(reward_num --)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Make a great object */
			make_object(q_ptr, TRUE, TRUE);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}
	}
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx, bool drop_item_okay)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & RF1_UNIQUE));

	bool good = (r_ptr->flags1 & RF1_DROP_GOOD) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & RF1_DROP_GREAT) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & RF1_ONLY_ITEM));
	bool do_item = (!(r_ptr->flags1 & RF1_ONLY_GOLD));
	bool cloned = FALSE;
	int force_coin = get_coin_type(m_ptr->r_idx);

	object_type forge;
	object_type *q_ptr;


	/* Notice changes in view */
	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2 | RF7_SELF_LITE_1 | RF7_SELF_LITE_2))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Handle the possibility of player vanquishing arena combatant -KMW- */
	if (p_ptr->inside_arena)
	{
		p_ptr->exit_bldg = TRUE;
#ifdef JP
		msg_print("勝利！チャンピオンへの道を進んでいる。");
#else
		msg_print("Victorious! You're on your way to becoming Champion.");
#endif
		p_ptr->arena_number++;
	}

	if (m_ptr->smart & SM_CLONED)
		cloned = TRUE;

	/* Let monsters explode! */
	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method == RBM_EXPLODE)
		{
			u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
			int typ = mbe_info[r_ptr->blow[i].effect].explode_type;
			int d_dice = r_ptr->blow[i].d_dice;
			int d_side = r_ptr->blow[i].d_side;
			int damage = damroll(d_dice, d_side);

			sound(SOUND_BR_FIRE); /* (Sound substitute) No sound for explode, use fire */
			project(m_idx, 3, y, x, damage, typ, flg);
			break;
		}
	}

	/* Check for quest completion */
	check_quest_completion(m_ptr);

#ifdef USE_CORPSES
	/* Drop a dead corpse? */
	if ((randint1(r_ptr->flags1 & RF1_UNIQUE ? 1 : 4) == 1) &&
	    ((r_ptr->flags9 & RF9_DROP_CORPSE) ||
	    (r_ptr->flags9 & RF9_DROP_SKELETON)) &&
	    !(m_ptr->smart & SM_CLONED))
	{
		/* Assume skeleton */
		bool corpse = FALSE;

		/*
		 * We cannot drop a skeleton? Note, if we are in this check,
		 * we *know* we can drop at least a corpse or a skeleton
		 */
		if (!(r_ptr->flags9 & RF9_DROP_SKELETON))
			corpse = TRUE;

		/* Else, a corpse is more likely unless we did a "lot" of damage */
		else if (r_ptr->flags9 & RF9_DROP_CORPSE)
		{
			/* Lots of damage in one blow */
			if ((0 - ((m_ptr->maxhp) / 4)) > m_ptr->hp)
			{
				if (randint1(5) == 1) corpse = TRUE;
			}
			else
			{
				if (randint1(5) != 1) corpse = TRUE;
			}
		}

		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a corpse */
		object_prep(q_ptr, lookup_kind(TV_CORPSE, (corpse ? SV_CORPSE : SV_SKELETON)));

		apply_magic(q_ptr, object_level, FALSE, FALSE, FALSE, FALSE);

		q_ptr->pval = m_ptr->r_idx;

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}
#endif /* USE_CORPSES */

	/* Drop objects being carried */
	monster_drop_carried_objects(m_ptr);

	/* Death Sword drop only swords */
	if (r_ptr->d_char == '|')
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_sword;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a great object */
		make_object(q_ptr, FALSE, FALSE);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Cloaker drop only cloaks. */
	else if (r_ptr->d_char == '(')
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		get_obj_num_hook = kind_is_cloak;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a great object */
		make_object(q_ptr, FALSE, FALSE);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}
#ifndef TINYANGBAND
	else if ((m_ptr->r_idx == MON_RAAL) && (dun_level > 9))
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Activate restriction */
		if ((dun_level > 49) && one_in_(5))
			get_obj_num_hook = kind_is_good_book;
		else
			get_obj_num_hook = kind_is_book;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Make a great object */
		make_object(q_ptr, FALSE, FALSE);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}
#endif
	/* HACK -- ringwraiths */
	else if ((m_ptr->r_idx == MON_ANGMAR) || (m_ptr->r_idx == MON_KHAMUL) ||
		(m_ptr->r_idx == MON_NAZGUL))
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_RING, SV_RING_WRAITH));

		create_nazgul_ring(q_ptr);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Mega-Hack -- drop "winner" treasures */
	else if (r_ptr->flags1 & RF1_DROP_CHOSEN)
	{
		if (m_ptr->r_idx == MON_MORGOTH)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Mega-Hack -- Prepare to make "Grond" */
			object_prep(q_ptr, lookup_kind(TV_HAFTED, SV_GROND));

			/* Mega-Hack -- Mark this item as "Grond" */
			q_ptr->name1 = ART_GROND;

			/* Mega-Hack -- Actually create "Grond" */
			apply_magic(q_ptr, -1, TRUE, TRUE, TRUE, FALSE);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);

			/* Get local object */
			q_ptr = &forge;

			/* Mega-Hack -- Prepare to make "Morgoth" */
			object_prep(q_ptr, lookup_kind(TV_CROWN, SV_MORGOTH));

			/* Mega-Hack -- Mark this item as "Morgoth" */
			q_ptr->name1 = ART_CHAOS;

			/* Mega-Hack -- Actually create "Morgoth" */
			apply_magic(q_ptr, -1, TRUE, TRUE, TRUE, FALSE);

			/* Drop it in the dungeon */
			(void)drop_near(q_ptr, -1, y, x);
		}
		else
		{
			byte a_idx = 0;
			int chance = 0;

			switch (m_ptr->r_idx)
			{
			case MON_SAURON:
				a_idx = ART_POWER;
				chance = 5;
				break;

			case MON_SARUMAN:
				a_idx = ART_ELENDIL;
				chance = 20;
				break;

			case MON_AR_PHARAZON:
				a_idx = ART_NUMENOR;
				chance = 15;
				break;

			case MON_GOTHMOG:
				a_idx = ART_GOTHMOG;
				chance = 50;
				break;

			}

			if ((a_idx > 0) && ((randint1(99) < chance) || (wizard)))
			{
				if (a_info[a_idx].cur_num == 0)
				{
					/* Create the artifact */
					create_named_art(a_idx, y, x);

					a_info[a_idx].cur_num = 1;
				}
			}
		}
	}

	/* Determine how much we can drop */
	if ((r_ptr->flags1 & RF1_DROP_60) && (randint0(100) < 60)) number++;
	if ((r_ptr->flags1 & RF1_DROP_90) && (randint0(100) < 90)) number++;
	if  (r_ptr->flags1 & RF1_DROP_1D2) number += damroll(1, 2);
	if  (r_ptr->flags1 & RF1_DROP_2D2) number += damroll(2, 2);
	if  (r_ptr->flags1 & RF1_DROP_3D2) number += damroll(3, 2);
	if  (r_ptr->flags1 & RF1_DROP_4D2) number += damroll(4, 2);

	if (cloned && !(r_ptr->flags1 & RF1_UNIQUE))
		number = 0; /* Clones drop no stuff unless Cloning Pits */

	if (!drop_item_okay && ironman_hengband) number = 0;

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Average dungeon and monster levels */
	object_level = (dun_level + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Make Gold */
		if (do_gold && (!do_item || (randint0(100) < 50)))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;

			/* XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(q_ptr, good, great)) continue;

			/* XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = base_level;

	/* Reset "coin" type */
	coin_type = 0;


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags1 & RF1_QUESTOR)) return;

	/* Winner? */
	if (m_ptr->r_idx == MON_MORGOTH)
	{
		/* Total winner */
		total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Congratulations */
#ifdef JP
msg_print("*** おめでとう ***");
#else
		msg_print("*** CONGRATULATIONS ***");
#endif

#ifdef JP
msg_print("あなたはゲームをコンプリートしました。");
#else
		msg_print("You have won the game!");
#endif

#ifdef JP
msg_print("準備が整ったら引退(自殺コマンド)しても結構です。");
#else
		msg_print("You may retire (commit suicide) when you are ready.");
#endif

	}
}

/*
 * Modify the physical damage done to the monster.
 * (for example when it's invulnerable or shielded)
 *
 * ToDo: Accept a damage-type to calculate the modified damage from
 * things like fire, frost, lightning, poison, ... attacks.
 *
 * "type" is not yet used and should be 0.
 */
int mon_damage_mod(monster_type *m_ptr, int dam, int type)
{
	/* Unused */
	(void)type;

	if (m_ptr->invulner && !(randint1(PENETRATE_INVULNERABILITY) == 1))
		return (0);
	else
		return (dam);
}



/*
 * Decreases monsters hit points, handling monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Made name, sex, and capitalization generic -BEN-
 *
 * As always, the "ghost" processing is a total hack.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters.  This would also need
 * to induce changes in the monster recall code.
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	s32b            div, new_exp, new_exp_frac;

	/* Handle stuff (Cleanup old infomation) */
	handle_stuff();

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;
	if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		/* When the player kills a Unique, it stays dead */
		if ((r_ptr->flags1 & RF1_UNIQUE) && !(m_ptr->smart & SM_CLONED))
			r_ptr->max_num = 0;

		/* When the player kills a Nazgul, it stays dead */
		if (r_ptr->flags3 & RF3_UNIQUE_7) r_ptr->max_num--;

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & RF1_UNIQUE))
		{
			/* Count kills this life */
			if (r_ptr->r_pkills < MAX_SHORT) r_ptr->r_pkills++;

			/* Count kills in all lives */
			if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;

			/* Hack -- Auto-recall */
			monster_race_track(m_ptr->r_idx);
		}

		if (r_ptr->flags2 & RF2_CAN_SPEAK)
		{
			char line_got[1024];

			if (speak_unique)
			{
				/* Dump a message */
#ifdef JP
				if (!get_rnd_line("mondeath_j.txt", m_ptr->r_idx, line_got))
						msg_format("%^s %s", m_name, line_got);
#else
				if (!get_rnd_line("mondeath.txt", m_ptr->r_idx, line_got))
						msg_format("%^s says: %s", m_name, line_got);
#endif
			}

			if ((r_ptr->flags1 & RF1_UNIQUE) && !(m_ptr->smart & SM_CLONED) && (randint1(REWARD_CHANCE) == 1) &&
			    !(r_ptr->flags7 & RF7_FRIENDLY))
			{
#ifdef JP
				if (!get_rnd_line("crime_j.txt", m_ptr->r_idx, line_got))
#else
				if (!get_rnd_line("crime.txt", m_ptr->r_idx, line_got))
#endif
				{
					int reward = 250 * (randint1(10) + r_ptr->level - 5);

					/* Force 'good' values */
					if (reward > 32000) reward = 32000;
					else if (reward < 250) reward = 250;

#ifdef JP
					msg_format("%sの首には賞金がかかっていた。", m_name);
					msg_format("%^sは%sの罪で指名手配されていた。", m_name, line_got);
					msg_format("$%d の報酬を受けとった。", reward);
#else
					msg_format("There was a price on %s's head.", m_name);
					msg_format("%^s was wanted for %s", m_name, line_got);
					msg_format("You collect a reward of %d gold pieces.", reward);
#endif
					p_ptr->au += reward;
					p_ptr->redraw |= (PR_GOLD);
				}
			}
		}


        /* Play a special sound if the monster was unique */
        if ((r_ptr->flags1 & RF1_UNIQUE) && !(m_ptr->smart & SM_CLONED))
        {
                /* Mega-Hack -- Morgoth -- see monster_death() */
                if (r_ptr->flags1 & RF1_DROP_CHOSEN)
                         sound(SOUND_KILL_KING);
                else
                         sound(SOUND_KILL_UNIQUE);
        } 
		else 
        {
		 	 sound(SOUND_KILL);
	    }

		/* Death by Missile/Spell attack */
		if (note)
		{
			msg_format("%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
#ifdef JP
			msg_format("%sを殺した。", m_name);
#else
			msg_format("You have killed %s.", m_name);
#endif
		}

		/* Death by Physical attack -- non-living monster */
		else if (!monster_living(r_ptr))
		{
#ifdef JP
			msg_format("%sを倒した。", m_name);
#else
			msg_format("You have destroyed %s.", m_name);
#endif
		}

		/* Death by Physical attack -- living monster */
		else
		{
#ifdef JP
			msg_format("%sを葬り去った。", m_name);
#else
			msg_format("You have slain %s.", m_name);
#endif
		}

		/* Maximum player level */
		div = p_ptr->max_plv;

		/* get only 1/5 exp in wilderness */
		if (!dun_level) div *= 5;

		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;

		/* Handle fractional experience */
		new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
				* 0x10000L / div) + p_ptr->exp_frac;

		/* Keep track of experience */
		if (new_exp_frac >= 0x10000L)
		{
			new_exp++;
			p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
		}
		else
		{
			p_ptr->exp_frac = (u16b)new_exp_frac;
		}

		/* Gain experience */
		gain_exp(new_exp);

		/* Generate treasure */
		monster_death(m_idx, TRUE);

		/*
		 * If the player kills a Unique,
		 * and the notes options are on, write a note
		 */
		if ((r_ptr->flags1 & RF1_UNIQUE) && take_notes && record_unique)
		{
			char note_buf[160];

			/* Get true name even if blinded/hallucinating */
#ifdef JP
			sprintf(note_buf, "%s%s", r_name + r_ptr->name, (m_ptr->smart & SM_CLONED) ? "(クローン)" : "");
#else
			sprintf(note_buf, "%s%s", r_name + r_ptr->name, (m_ptr->smart & SM_CLONED) ? "(Clone)" : "");
#endif

			/* Write note */
			add_note(note_buf, 'U');
		}

		/* Delete the monster */
		delete_monster_idx(m_idx);

		/* Not afraid */
		(*fear) = FALSE;

		/* Monster is dead */
		return (TRUE);
	}


#ifdef ALLOW_FEAR

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint1(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			m_ptr->monfear = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if (((percentage <= 10) && (randint0(10) < (11 - percentage))) ||
		    ((dam >= m_ptr->hp) && (randint0(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear = (randint1(10) +
					  (((dam >= m_ptr->hp) && (percentage > 7)) ?
					   20 : ((11 - percentage) * 5)));
		}
	}

#endif


	/* Not dead yet */
	return (FALSE);
}


/*
 * Get term size and calculate screen size
 */
void get_screen_size(int *wid_p, int *hgt_p)
{
	Term_get_size(wid_p, hgt_p);
	*hgt_p -= ROW_MAP + 2;
	*wid_p -= COL_MAP + 2;
	if (use_bigtile) *wid_p /= 2;
}


/*
 * Calculates current boundaries
 * Called below and from "do_cmd_locate()".
 */
void panel_bounds_center(void)
{
	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	panel_row_max = panel_row_min + hgt - 1;
	panel_row_prt = panel_row_min - 1;
	panel_col_max = panel_col_min + wid - 1;
	panel_col_prt = panel_col_min - 13;
}


/*
 * Map resizing whenever the main term changes size
 */
void resize_map(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;
	
	/* Mega-Hack -- no panel yet */
	panel_row_max = 0;
	panel_col_max = 0;

	/* Reset the panels */
	panel_row_min = cur_hgt;
	panel_col_min = cur_wid;
				
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Forget lite/view */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Hack -- update */
	handle_stuff();

	/* Redraw */
	Term_redraw();

	/*
	 * Waiting command;
	 * Place the cursor on the player
	 */
	if (can_save) move_cursor_relative(py, px);

	/* Refresh */
	Term_fresh();
}

/*
 * Redraw a term when it is resized
 */
void redraw_window(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;
	
	/* Hack - Activate term zero for the redraw */
	Term_activate(&term_screen[0]);
	
	/* Hack -- react to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_STATS);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

	/* Hack -- update */
	handle_stuff();

	/* Redraw */
	Term_redraw();

	/* Refresh */
	Term_fresh();
}


/*
 * Handle a request to change the current panel
 *
 * Return TRUE if the panel was changed.
 *
 * Also used in do_cmd_locate
 */
bool change_panel(int dy, int dx)
{
	int y, x;
	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	/* Apply the motion */
	y = panel_row_min + dy * hgt / 2;
	x = panel_col_min + dx * wid / 2;

	/* Verify the row */
	if (y > cur_hgt - hgt) y = cur_hgt - hgt;
	if (y < 0) y = 0;

	/* Verify the col */
	if (x > cur_wid - wid) x = cur_wid - wid;
	if (x < 0) x = 0;

	/* Handle "changes" */
	if ((y != panel_row_min) || (x != panel_col_min))
	{
		/* Save the new panel info */
		panel_row_min = y;
		panel_col_min = x;

		/* Recalculate the boundaries */
		panel_bounds_center();

		/* Update stuff */
		p_ptr->update |= (PU_MONSTERS);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Handle stuff */
		handle_stuff();

		/* Success */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


/*
 * Given an row (y) and col (x), this routine detects when a move
 * off the screen has occurred and figures new borders. -RAK-
 *
 * "Update" forces a "full update" to take place.
 *
 * The map is reprinted if necessary, and "TRUE" is returned.
 */
void verify_panel(void)
{
	int y = py;
	int x = px;
	int wid, hgt;

	int prow_min;
	int pcol_min;
	int max_prow_min;
	int max_pcol_min;

	/* Get size */
	get_screen_size(&wid, &hgt);

	max_prow_min = cur_hgt - hgt;
	max_pcol_min = cur_wid - wid;

	/* Bounds checking */
	if (max_prow_min < 0) max_prow_min = 0;
	if (max_pcol_min < 0) max_pcol_min = 0;

		/* Center on player */
	if (center_player)
	{
		/* Center vertically */
		prow_min = y - hgt / 2;
		if (prow_min < 0) prow_min = 0;
		else if (prow_min > max_prow_min) prow_min = max_prow_min;

		/* Center horizontally */
		pcol_min = x - wid / 2;
		if (pcol_min < 0) pcol_min = 0;
		else if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
	}
	else
	{
		prow_min = panel_row_min;
		pcol_min = panel_col_min;

		/* Scroll screen when 2 grids from top/bottom edge */
		if (y > panel_row_max - 2)
		{
			while (y > prow_min + hgt-1 - 2)
			{
				prow_min += (hgt / 2);
			}
		}

		if (y < panel_row_min + 2)
		{
			while (y < prow_min + 2)
			{
				prow_min -= (hgt / 2);
			}
		}

		if (prow_min > max_prow_min) prow_min = max_prow_min;
		if (prow_min < 0) prow_min = 0;

		/* Scroll screen when 4 grids from left/right edge */
		if (x > panel_col_max - 4)
		{
			while (x > pcol_min + wid-1 - 4)
			{
				pcol_min += (wid / 2);
			}
		}
		
		if (x < panel_col_min + 4)
		{
			while (x < pcol_min + 4)
			{
				pcol_min -= (wid / 2);
			}
		}

		if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
		if (pcol_min < 0) pcol_min = 0;
	}

	/* Check for "no change" */
	if ((prow_min == panel_row_min) && (pcol_min == panel_col_min)) return;

	/* Save the new panel info */
	panel_row_min = prow_min;
	panel_col_min = pcol_min;

	/* Hack -- optional disturb on "panel change" */
	if (disturb_panel && !center_player) disturb(0, 0);

	/* Recalculate the boundaries */
	panel_bounds_center();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Monster health description
 */
cptr look_mon_desc(int m_idx, bool mode)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	bool living;
	int perc;
	cptr desc;
	cptr attitude;
	cptr clone;

	/* Determine if the monster is "living" */
	living = monster_living(r_ptr);

	/* Calculate a health "percentage" */
	perc = 100L * m_ptr->hp / m_ptr->maxhp;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
#ifdef JP
		desc = living ? "無傷" : "無ダメージ";
#else
		desc = living ? "unhurt" : "undamaged";
#endif
	}
	else if (perc >= 60)
	{
#ifdef JP
		desc = living ? "軽傷" : "小ダメージ";
#else
		desc = living ? "somewhat wounded" : "somewhat damaged";
#endif
	}
	else if (perc >= 25)
	{
#ifdef JP
		desc = living ? "負傷" : "中ダメージ";
#else
		desc = living ? "wounded" : "damaged";
#endif
	}
	else if (perc >= 10)
	{
#ifdef JP
		desc = living ? "重傷" : "大ダメージ";
#else
		desc = living ? "badly wounded" : "badly damaged";
#endif
	}
	else
	{
#ifdef JP
		desc = living ? "半死半生" : "倒れかけ";
#else
		desc = living ? "almost dead" : "almost destroyed";
#endif
	}

	/* Need attitude information? */
	if (!(mode & 0x01))
	{
		attitude = "";
	}
	else if (is_pet(m_ptr))
	{
#ifdef JP
		attitude = ", ペット";
#else
		attitude = ", pet";
#endif
	}
	else if (is_friendly(m_ptr))
	{
#ifdef JP
		attitude = ", 友好的";
#else
		attitude = ", friendly";
#endif
	}
	else
	{
#ifdef JP
		attitude = "";
#else
		attitude = "";
#endif
	}

	/* Clone monster? */
	if (m_ptr->smart & SM_CLONED)
	{
		clone = ", clone";
	}
	else
	{
		clone = "";
	}

	/* Display monster's level --- idea borrowed from ToME */
	if (r_ptr->r_tkills)
	{
#ifdef JP
		return format("レベル%d, %s%s%s", r_ptr->level, desc, attitude, clone);
#else
		return format("Level %d, %s%s%s", r_ptr->level, desc, attitude, clone);
#endif
	}
	else 
	{
#ifdef JP
		return format("レベル???, %s%s%s", desc, attitude, clone);
#else
		return format("Level ???, %s%s%s", desc, attitude, clone);
#endif
	}
}



/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(vptr u, vptr v, int p, int q)
{
	int z, a, b;

	/* Done sort */
	if (p >= q) return;

	/* Pivot */
	z = p;

	/* Begin */
	a = p;
	b = q;

	/* Partition */
	while (TRUE)
	{
		/* Slide i2 */
		while (!(*ang_sort_comp)(u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*ang_sort_comp)(u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*ang_sort_swap)(u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b+1, q);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1);
}



/*** Targeting Code ***/


/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targeting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* XXX XXX XXX Hack -- Never target trappers */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Assume okay */
	return (TRUE);
}




/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
	/* Accept stationary targets */
	if (target_who < 0) return (TRUE);

	/* Check moving targets */
	if (target_who > 0)
	{
		/* Accept reasonable targets */
		if (target_able(target_who))
		{
			monster_type *m_ptr = &m_list[target_who];

			/* Acquire monster location */
			target_row = m_ptr->fy;
			target_col = m_ptr->fx;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}



/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b*)(u);
	s16b *y = (s16b*)(v);

	int da, db, kx, ky;

	/* Absolute distance components */
	kx = x[a]; kx -= px; kx = ABS(kx);
	ky = y[a]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Absolute distance components */
	kx = x[b]; kx -= px; kx = ABS(kx);
	ky = y[b]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Compare the distances */
	return (da <= db);
}


/*
 * Sorting hook -- comp function -- by importance level of grids
 * Originally from Hengband
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by level of monster
 */
static bool ang_sort_comp_importance(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b*)(u);
	s16b *y = (s16b*)(v);
	cave_type *ca_ptr = &cave[y[a]][x[a]];
	cave_type *cb_ptr = &cave[y[b]][x[b]];
	monster_type *ma_ptr = &m_list[ca_ptr->m_idx];
	monster_type *mb_ptr = &m_list[cb_ptr->m_idx];
	monster_race *ap_ra_ptr, *ap_rb_ptr;

	/* The player grid */
	if ((y[a] == py) && (x[a] == px)) return TRUE;
	if ((y[b] == py) && (x[b] == px)) return FALSE;

	/* Extract monster race */
	if (ca_ptr->m_idx && ma_ptr->ml) ap_ra_ptr = &r_info[ma_ptr->r_idx];
	else ap_ra_ptr = NULL;
	if (cb_ptr->m_idx && mb_ptr->ml) ap_rb_ptr = &r_info[mb_ptr->r_idx];
	else ap_rb_ptr = NULL;

	if (ap_ra_ptr && !ap_rb_ptr) return TRUE;
	if (!ap_ra_ptr && ap_rb_ptr) return FALSE;

	/* Compare two monsters */
	if (ap_ra_ptr && ap_rb_ptr)
	{
		/* Unique monsters first */
		if ((ap_ra_ptr->flags1 & RF1_UNIQUE) && !(ap_rb_ptr->flags1 & RF1_UNIQUE)) return TRUE;
		if (!(ap_ra_ptr->flags1 & RF1_UNIQUE) && (ap_rb_ptr->flags1 & RF1_UNIQUE)) return FALSE;

 		/* Unknown monsters first */
		if (!ap_ra_ptr->r_tkills && ap_rb_ptr->r_tkills) return TRUE;
		if (ap_ra_ptr->r_tkills && !ap_rb_ptr->r_tkills) return FALSE;

		/* Higher level monsters first (if known) */
		if (ap_ra_ptr->r_tkills && ap_rb_ptr->r_tkills)
		{
			if (ap_ra_ptr->level > ap_rb_ptr->level) return TRUE;
			if (ap_ra_ptr->level < ap_rb_ptr->level) return FALSE;
		}

		/* Sort by index if all conditions are same */
		if (ma_ptr->r_idx > mb_ptr->r_idx) return TRUE;
		if (ma_ptr->r_idx < mb_ptr->r_idx) return FALSE;
	}

	/* An object get higher priority */
	if (cave[y[a]][x[a]].o_idx && !cave[y[b]][x[b]].o_idx) return TRUE;
	if (!cave[y[a]][x[a]].o_idx && cave[y[b]][x[b]].o_idx) return FALSE;

	/* Priority from the terrain */
	if (f_info[ca_ptr->feat].priority > f_info[cb_ptr->feat].priority) return TRUE;
	if (f_info[ca_ptr->feat].priority < f_info[cb_ptr->feat].priority) return FALSE;

	/* If all conditions are same, compare distance */
	return ang_sort_comp_distance(u, v, a, b);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b*)(u);
	s16b *y = (s16b*)(v);

	s16b temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}



/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
	int i, v;

	int x2, y2, x3, y3, x4, y4;

	int b_i = -1, b_v = 9999;


	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Point 2 */
		x2 = temp_x[i];
		y2 = temp_y[i];

		/* Directed distance */
		x3 = (x2 - x1);
		y3 = (y2 - y1);

		/* Verify quadrant */
		if (dx && (x3 * dx <= 0)) continue;
		if (dy && (y3 * dy <= 0)) continue;

		/* Absolute distance */
		x4 = ABS(x3);
		y4 = ABS(y3);

		/* Verify quadrant */
		if (dy && !dx && (x4 > y4)) continue;
		if (dx && !dy && (y4 > x4)) continue;

		/* Approximate Double Distance */
		v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

		/* XXX XXX XXX Penalize location */

		/* Track best */
		if ((b_i >= 0) && (v >= b_v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Result */
	return (b_i);
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_accept(int y, int x)
{
	cave_type *c_ptr;

	s16b this_o_idx, next_o_idx;

	/* Bounds */
	if (!(in_bounds(y, x))) return (FALSE);

	/* Player grid is always interesting */
	if ((y == py) && (x == px)) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);


	/* Examine the grid */
	c_ptr = &cave[y][x];

	/* Visible monsters */
	if (c_ptr->m_idx)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized object */
		if (o_ptr->marked & OM_FOUND) return (TRUE);
	}

	/* Interesting memorized features */
	if (c_ptr->info & (CAVE_MARK))
	{
		/* Notice glyphs */
		if (c_ptr->feat == FEAT_GLYPH) return (TRUE);
		if (c_ptr->feat == FEAT_MINOR_GLYPH) return (TRUE);

		/* Notice the Pattern */
		if ((c_ptr->feat <= FEAT_PATTERN_XTRA2) &&
		    (c_ptr->feat >= FEAT_PATTERN_START))
			return (TRUE);

		/* Notice doors */
		if (c_ptr->feat == FEAT_OPEN) return (TRUE);
		if (c_ptr->feat == FEAT_BROKEN) return (TRUE);

		/* Notice stairs */
		if (c_ptr->feat == FEAT_LESS) return (TRUE);
		if (c_ptr->feat == FEAT_MORE) return (TRUE);
		if (c_ptr->feat == FEAT_LESS_LESS) return (TRUE);
		if (c_ptr->feat == FEAT_MORE_MORE) return (TRUE);

		/* Notice shops */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
		    (c_ptr->feat <= FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice buildings -KMW- */
		if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
		    (c_ptr->feat <= FEAT_BLDG_TAIL)) return (TRUE);

		/* Notice traps */
		if (is_trap(c_ptr->feat)) return (TRUE);

		/* Notice doors */
		if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		    (c_ptr->feat <= FEAT_DOOR_TAIL)) return (TRUE);

		/* Notice rubble */
		if (c_ptr->feat == FEAT_RUBBLE) return (TRUE);

		/* Notice veins with treasure */
		if (c_ptr->feat == FEAT_MAGMA_K) return (TRUE);
		if (c_ptr->feat == FEAT_QUARTZ_K) return (TRUE);

#if 0
		/* Notice water, lava, ... */
		if (c_ptr->feat == FEAT_DEEP_WATER) return (TRUE);
		if (c_ptr->feat == FEAT_SHAL_WATER) return (TRUE);
		if (c_ptr->feat == FEAT_DEEP_LAVA) return (TRUE);
		if (c_ptr->feat == FEAT_SHAL_LAVA) return (TRUE);
		if (c_ptr->feat == FEAT_DIRT) return (TRUE);
		if (c_ptr->feat == FEAT_GRASS) return (TRUE);
		if (c_ptr->feat == FEAT_DARK_PIT) return (TRUE);
		if (c_ptr->feat == FEAT_TREES) return (TRUE);
		if (c_ptr->feat == FEAT_MOUNTAIN) return (TRUE);
#endif

		/* Notice quest features */
		if (c_ptr->feat == FEAT_QUEST_ENTER) return (TRUE);
		if (c_ptr->feat == FEAT_QUEST_EXIT) return (TRUE);
		if (c_ptr->feat == FEAT_QUEST_DOWN) return (TRUE);
		if (c_ptr->feat == FEAT_QUEST_UP) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			cave_type *c_ptr;

			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_can_see_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_accept(y, x)) continue;

			c_ptr = &cave[y][x];

			/* Require target_able monsters for "TARGET_KILL" */
			if ((mode & (TARGET_KILL)) && !target_able(c_ptr->m_idx)) continue;

			/* Require hostile creatures if "TARGET_HOST" is used */
			if ((mode & (TARGET_HOST)) && !is_hostile(&m_list[c_ptr->m_idx])) continue;

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Set the sort hooks */
	if (mode & TARGET_KILL)
	{
		/* Target the nearest monster for shooting */
		ang_sort_comp = ang_sort_comp_distance;
		ang_sort_swap = ang_sort_swap_distance;
	}
	else
	{
		/* Look important grids first in Look command */
		ang_sort_comp = ang_sort_comp_importance;
		ang_sort_swap = ang_sort_swap_distance;
	}

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
}

/*
 * Evaluate number of kill needed to gain level
 */
static void evaluate_monster_exp(char *acount, monster_type *m_ptr)
{
#define M_INT_GREATER(h1,l1,h2,l2)  ( (h1>h2)||( (h1==h2)&&(l1>=l2)))
#define M_INT_SUB(h1,l1, h2,l2) {h1-=h2;if(l1<l2){l1+=0x10000;h1--;}l1-=l2;}
#define M_INT_ADD(h1,l1, h2,l2) {h1+=h2;l1+=l2;if(l1>=0x10000L){l1&=0xFFFF;h1++;}}
#define M_INT_LSHIFT(h1,l1) {h1=(h1<<1)|(l1>>15);l1=(l1<<1)&0xffff;}
#define M_INT_RSHIFT(h1,l1) {l1=(l1>>1)|((h1&1)<<15);h1>>=1;}
#define M_INT_MULT(h1,l1,mul,h2,l2) {l2=(l1*mul)&0xffff;h2=((l1*mul)>>16)+h1*mul;}

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b tmp_h,tmp_l;
	int bit,result;
	u32b exp_mon= (r_ptr->mexp)*(r_ptr->level);
	u32b exp_mon_h= exp_mon / (p_ptr->max_plv);
	u32b exp_mon_l= ((exp_mon % p_ptr->max_plv)*0x10000/(p_ptr->max_plv))&0xFFFF;
	
	u32b exp_adv_h = player_exp[p_ptr->max_plv -1] * p_ptr->expfact /100;
	u32b exp_adv_l = ((player_exp[p_ptr->max_plv -1]%100) * p_ptr->expfact *0x10000/100)&0xFFFF;
	
	M_INT_SUB(exp_adv_h, exp_adv_l, p_ptr->exp, p_ptr->exp_frac);
	if (p_ptr->lev>=PY_MAX_LEVEL) sprintf(acount,"[**]");
	else if (!r_ptr->r_tkills) sprintf(acount,"[??]");
	else if (M_INT_GREATER(exp_mon_h, exp_mon_l, exp_adv_h, exp_adv_l)) sprintf(acount,"[01]");
	else {
		M_INT_MULT(exp_mon_h, exp_mon_l, 1000,tmp_h, tmp_l);
		if( M_INT_GREATER(exp_adv_h, exp_adv_l, tmp_h, tmp_l) ) sprintf(acount,"[999]");
		else {
			bit=1; result=0;
			M_INT_ADD(exp_adv_h, exp_adv_l, exp_mon_h, exp_mon_l);
			M_INT_SUB(exp_adv_h, exp_adv_l, 0, 1);
			while( M_INT_GREATER(exp_adv_h, exp_adv_l, exp_mon_h,exp_mon_l) ){M_INT_LSHIFT(exp_mon_h,exp_mon_l);bit<<=1;}
			M_INT_RSHIFT(exp_mon_h,exp_mon_l);bit>>=1;
			for(;bit>=1;bit>>=1){
				if(M_INT_GREATER(exp_adv_h,exp_adv_l,exp_mon_h,exp_mon_l))
				{result|=bit;M_INT_SUB(exp_adv_h,exp_adv_l,exp_mon_h,exp_mon_l);}
				M_INT_RSHIFT(exp_mon_h,exp_mon_l); 
			}
			sprintf(acount,"[%03d]",result);
		}
	}
}


bool show_gold_on_floor = FALSE;

/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * Eventually, we may allow multiple objects per grid, or objects
 * and terrain features in the same grid. XXX XXX XXX
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_aux(int y, int x, int mode, cptr info)
{
	cave_type *c_ptr = &cave[y][x];
	s16b this_o_idx, next_o_idx;
	cptr x_info = "";
	bool boring;
	int feat;
	int query;
	char out_val[MAX_NLEN+80];

#ifdef ALLOW_EASY_FLOOR
	int floor_list[23], floor_num=0;

	/* Scan all objects in the grid */
	if (easy_floor)
	{
		scan_floor(floor_list, &floor_num, y, x, 0x02);

		if (floor_num)
		{
#ifdef JP
			x_info = "x床上 ";
#else
			x_info = "x,";
#endif
		}
	}

#endif /* ALLOW_EASY_FLOOR */

	/* Repeat forever */
	while (1)
	{
		cptr s1 = "", s2 = "", s3 = "";

		/* Paranoia */
		query = ' ';

		/* Assume boring */
		boring = TRUE;

		/* Default */
#ifndef JP
		s1 = "You see ";
#endif

		/* Hack -- under the player */
		if ((y == py) && (x == px))
		{
			/* Description */
#ifdef JP
			s1 = "あなたは";
			s2 = "の上";
			s3 = "にいる";
#else
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
#endif
		}


		/* Hack -- hallucination */
		if (p_ptr->image)
		{
#ifdef JP
			cptr name = "何か奇妙な物";
#else
			cptr name = "something strange";
#endif

			/* Display a message */
#ifdef JP
			sprintf(out_val, "%s%s%s%s [%s]", s1, name, s2, s3, info);
#else
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
#endif
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\r') && (query != '\n')) break;

			/* Repeat forever */
			continue;
		}


		/* Actual monsters */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Visible */
			if (m_ptr->ml)
			{
				bool recall = FALSE;

				char m_name[80];

				/* Not boring */
				boring = FALSE;

				/* Get the monster name ("a kobold") */
				monster_desc(m_name, m_ptr, 0x08);

				/* Hack -- track this monster race */
				monster_race_track(m_ptr->r_idx);

				/* Hack -- health bar for this monster */
				health_track(c_ptr->m_idx);

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (1)
				{
				     char acount[10];

					 /* Recall */
					if (recall)
					{
						/* Save */
						screen_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx, 0);

						/* Hack -- Complete the prompt (again) */
#ifdef JP
						Term_addstr(-1, TERM_WHITE, format("  [r思 %s%s]", x_info, info));
#else
						Term_addstr(-1, TERM_WHITE, format("  [r,%s%s]", x_info, info));
#endif

						/* Command */
						query = inkey();

						/* Restore */
						screen_load();
					}

					/* Normal */
					else
					{
						/* Describe, and prompt for recall */
						evaluate_monster_exp(acount, m_ptr);
#ifdef JP
						sprintf(out_val, "%s%s%s(%s)%s%s[r思 %s%s]",
						    acount, s1, m_name, look_mon_desc(c_ptr->m_idx, TRUE), s2, s3, x_info, info);
#else
						sprintf(out_val, "%s%s%s%s%s (%s)[r,%s%s]",
							acount, s1, s2, s3, m_name, look_mon_desc(c_ptr->m_idx, TRUE), x_info, info);
#endif
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey();
					}

					/* Normal commands */
					if (query != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Always stop at "normal" keys */
				if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
#ifdef JP
				s1 = "それは";
#else
				s1 = "It is ";
#endif

				/* Hack -- take account of gender */
#ifdef JP
				if (r_ptr->flags1 & (RF1_FEMALE)) s1 = "彼女は";
				else if (r_ptr->flags1 & (RF1_MALE)) s1 = "彼は";
#else
				if (r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";
				else if (r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";
#endif

				/* Use a preposition */
#ifdef JP
				s2 = "を";
				s3 = "持っている";
#else
				s2 = "carrying ";
#endif

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					char o_name[MAX_NLEN];

					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Obtain an object description */
					object_desc(o_name, o_ptr, 0);

					/* Describe the object */
#ifdef JP
					sprintf(out_val, "%s%s%s%s[%s]", s1, o_name, s2, s3, info);
#else
					sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
#endif
					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Always stop at "normal" keys */
					if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) break;

					/* Sometimes stop at "space" key */
					if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

					/* Change the intro */
#ifdef JP
					s2 = "をまた";
#else
					s2 = "also carrying ";
#endif
				}

				/* Double break */
				if (this_o_idx) break;

				/* Use a preposition */
#ifdef JP
				s2 = "の上";
				s3 = "にいる";
#else
				s2 = "on ";
#endif
			}
		}


#ifdef ALLOW_EASY_FLOOR
		if (floor_num)
		{
			char comkey = rogue_like_commands ? 'x' : 'l';

			while (1)
			{
				if (floor_num == 1)
				{
					char o_name[MAX_NLEN];

					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[floor_list[0]];

					/* Describe the object */
					object_desc(o_name, o_ptr, 0);

					/* Message */
#ifdef JP
					sprintf(out_val, "%s%s%s%s[%s]",
						s1, o_name, s2, s3, info);
#else
					sprintf(out_val, "%s%s%s%s [%s]",
						s1, s2, s3, o_name, info);
#endif
				}
				else
				{
					/* Message */
#ifdef JP
					sprintf(out_val, "%s %d個のアイテム%s%s ['%c'で一覧, %s]",
						s1, floor_num, s2, s3, comkey, info);
#else
					sprintf(out_val, "%s%s%sa pile of %d items [%c,%s]",
						s1, s2, s3, floor_num, comkey, info);
#endif
				}
				prt(out_val, 0, 0);
				move_cursor_relative(y, x);

				/* Command */
				query = inkey();

				/* Display list of items (query == "el", not "won") */
				if ((floor_num < 2) || (!(query == comkey) && !(query == ' '))) break;

				while(1)
				{
					int i, o_idx;
					cave_type *c_ptr;

					/* Save screen */
					screen_save();

					/* Display */
					show_gold_on_floor = TRUE;
					show_floor(y, x);
					show_gold_on_floor = FALSE;

					/* Prompt */
#ifdef JP
					sprintf(out_val, "%s %d個のアイテム%s%s [Enterで次へ, %s]",
						s1, floor_num, s2, s3, info);
#else
					sprintf(out_val, "%s%s%sa pile of %d items [Enter,%s]",
						s1, s2, s3, floor_num, info);
#endif
					prt(out_val, 0, 0);

					/* Wait */
					query = inkey();

					/* Load screen */
					screen_load();

					/* Exit unless 'Enter' */
					if (query != '\n' && query != '\r') break;

					/* Get the object being moved. */
					c_ptr = &cave[y][x];
					o_idx =	c_ptr->o_idx;
 
					/* Only rotate a pile of two or more objects. */
					if (!(o_idx && o_list[o_idx].next_o_idx)) continue;

					/* Remove the first object from the list. */
					excise_object_idx(o_idx);
	
					/* Find end of the list. */
					i = c_ptr->o_idx;
					while (o_list[i].next_o_idx)
						i = o_list[i].next_o_idx;
	
					/* Add after the last object. */
					o_list[i].next_o_idx = o_idx;

					/* Loop and re-display the list */
				}

				/* Exit unless 'Enter' */
				if (query != '\n' && query != '\r') break;
			}

			/* Stop - not boring */
			break;
		}

#endif /* ALLOW_EASY_FLOOR */


		/* Scan all objects in the grid */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Describe it */
			if (o_ptr->marked & OM_FOUND)
			{
				char o_name[MAX_NLEN];

				/* Not boring */
				boring = FALSE;

				/* Obtain an object description */
				object_desc(o_name, o_ptr, 0);

				/* Describe the object */
#ifdef JP
				sprintf(out_val, "%s%s%s%s[%s]", s1, o_name, s2, s3, info);
#else
				sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
#endif
				prt(out_val, 0, 0);
				move_cursor_relative(y, x);
				query = inkey();

				/* Always stop at "normal" keys */
				if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & TARGET_LOOK)) break;

				/* Change the intro */
#ifdef JP
				s1 = "それは";
#else
				s1 = "It is ";
#endif

				/* Plurals */
#ifdef JP
				if (o_ptr->number != 1) s1 = "それらは";
#else
				if (o_ptr->number != 1) s1 = "They are ";
#endif

				/* Preposition */
#ifdef JP
				s2 = "の上";
				s3 = "に見える";
#else
				s2 = "on ";
#endif
			}
		}

		/* Double break */
		if (this_o_idx) break;

		if (c_ptr->mimic)
		{
			feat = c_ptr->mimic;
		}
		else
		{
			feat = f_info[c_ptr->feat].mimic;
		}

		/* Require knowledge about grid, or ability to see grid */
		if (!(c_ptr->info & CAVE_MARK) && !player_can_see_bold(y, x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_INVIS))
		{
			cptr name;

			/* Hack -- special handling for building doors */
			if ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL))
			{
				name = building[feat - FEAT_BLDG_HEAD].name;
			}
			else
			{
				name = f_name + f_info[feat].name;
			}

			/* Hack -- special handling for quest entrances */
			if (feat == FEAT_QUEST_ENTER)
			{
				/* Set the quest number temporary */
				int old_quest = p_ptr->inside_quest;
				p_ptr->inside_quest = c_ptr->special;

				/* Get the quest text */
				init_flags = INIT_SHOW_TEXT;
				quest_text_line = 0;
				process_dungeon_file("q_info.txt", 0, 0, 0, 0);
#ifdef JP
				name = format("クエスト「%s」(%d階相当)", quest[c_ptr->special].name, quest[c_ptr->special].level);
#else
				name = format("quest '%s'(level %d)", quest[c_ptr->special].name, quest[c_ptr->special].level);
#endif

				/* Reset the old quest number */
				p_ptr->inside_quest = old_quest;
			}

			/* Hack -- handle unknown grids */
#ifdef JP
			if (feat == FEAT_NONE) name = "未知の地形";
#else
			if (feat == FEAT_NONE) name = "unknown grid";
#endif

			/* Pick a prefix */
			if (*s2 && ((feat >= FEAT_MINOR_GLYPH) &&
			   (feat <= FEAT_PATTERN_XTRA2)))
			{
#ifdef JP
				s2 = "の上";
#else
				s2 = "on ";
#endif
			}
			else if (*s2 && ((feat >= FEAT_DOOR_HEAD) &&
				(feat <= FEAT_PERM_SOLID)))
			{
#ifdef JP
				s2 = "の中";
#else
				s2 = "in ";
#endif
			}

			/* Hack -- special introduction for store & building doors -KMW- */
			if (((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL)) ||
			    ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL)) ||
			     (feat == FEAT_QUEST_ENTER))
			{
#ifdef JP
				s2 = "の入口";
#else
				s3 = "the entrance to the ";
#endif
			}
			else if ((feat == FEAT_FLOOR) || (feat == FEAT_DIRT))
			{
				s3 ="";
			}
			else
			{
				/* Pick proper indefinite article */
#ifndef JP
				s3 = (is_a_vowel(name[0])) ? "an " : "a ";
#endif
			}

			/* Display a message */
			if (wizard)
#ifdef JP
				sprintf(out_val, "%s%s%s%s [%s] (%d:%d)", s1, name, s2, s3, info, y, x);
#else
				sprintf(out_val, "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, info, y, x);
#endif
			else
#ifdef JP
				sprintf(out_val, "%s%s%s%s[%s]", s1, name, s2, s3, info);
#else
				sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
#endif
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if ((query != '\r') && (query != '\n') && (query != ' ')) break;
		}

		/* Stop on everything but "return" */
		if ((query != '\r') && (query != '\n')) break;
	}

	/* Keep going */
	return (query);
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * All locations must be on the current panel.  Consider the use of
 * "panel_bounds()" to allow "off-panel" targets, perhaps by using
 * some form of "scrolling" the map around the cursor.  XXX XXX XXX
 * That is, consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around.  This may require changes in the
 * "update_mon()" code to allow "visibility" even if off panel, and
 * may require dynamic recalculation of the "temp" grid set.
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 */
bool target_set(int mode)
{
	int		i, d, m, t, bd;
	int		y = py;
	int		x = px;

	bool	done = FALSE;

	bool	flag = TRUE;

	char	query;

	char	info[80];

	cave_type		*c_ptr;

	/* XTRA HACK MOVEFAST */
	bool    move_fast;

	int wid, hgt;

	/* Cancel target */
	target_who = 0;


	/* Cancel tracking */
	/* health_track(0); */

	/* Get size */
	get_screen_size(&wid, &hgt);

	/* Prepare the "temp" array */
	target_set_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		/* Interesting grids */
		if (flag && temp_n)
		{
			y = temp_y[m];
			x = temp_x[m];

			if (!(mode & TARGET_LOOK)) prt_path(y, x);

			/* Access */
			c_ptr = &cave[y][x];

			/* Allow target */
			if (target_able(c_ptr->m_idx))
			{
#ifdef JP
strcpy(info, "q止 t決 p自 o現 +次 -前");
#else
				strcpy(info, "q,t,p,o,+,-,<dir>");
#endif

			}

			/* Dis-allow target */
			else
			{
#ifdef JP
strcpy(info, "q止 p自 o現 +次 -前");
#else
				strcpy(info, "q,p,o,+,-,<dir>");
#endif

			}

			/* Describe and Prompt */
			query = target_set_aux(y, x, mode, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 't':
				case '.':
				case '5':
				case '0':
				{
					if (target_able(c_ptr->m_idx))
					{
						health_track(c_ptr->m_idx);
						target_who = c_ptr->m_idx;
						target_row = y;
						target_col = x;
						done = TRUE;
					}
					else
					{
						bell();
					}
					break;
				}

				case ' ':
				case '*':
				case '+':
				{
					if (++m == temp_n)
					{
						m = 0;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case '-':
				{
					if (m-- == 0)
					{
						m = temp_n - 1;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case 'p':
				{
					/* Recenter the map around the player */
					verify_panel();

					/* Update stuff */
					p_ptr->update |= (PU_MONSTERS);

					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					/* Window stuff */
					p_ptr->window |= (PW_OVERHEAD);

					/* Handle stuff */
					handle_stuff();

					/* Recalculate interesting grids */
					target_set_prepare(mode);

					y = py;
					x = px;
				}

				case 'o':
				{
					flag = FALSE;
					break;
				}

				case 'm':
				{
					break;
				}

				default:
				{
					/* Extract the action (if any) */
					d = get_keymap_dir(query);

					if (!d) bell();
					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				/* Modified to scroll to monster */
				int y2 = panel_row_min;
				int x2 = panel_col_min;

				/* Find a new monster */
				i = target_pick(temp_y[m], temp_x[m], ddy[d], ddx[d]);

				/* Request to target past last interesting grid */
				while (flag && (i < 0))
				{
					/* Note the change */
					if (change_panel(ddy[d], ddx[d]))
					{
						int v = temp_y[m];
						int u = temp_x[m];

						/* Recalculate interesting grids */
						target_set_prepare(mode);

						/* Look at interesting grids */
						flag = TRUE;

						/* Find a new monster */
						i = target_pick(v, u, ddy[d], ddx[d]);

						/* Use that grid */
						if (i >= 0) m = i;
					}

					/* Nothing interesting */
					else
					{
						int dx = ddx[d];
						int dy = ddy[d];

						/* Restore previous position */
						panel_row_min = y2;
						panel_col_min = x2;
						panel_bounds_center();

						/* Update stuff */
						p_ptr->update |= (PU_MONSTERS);

						/* Redraw map */
						p_ptr->redraw |= (PR_MAP);

						/* Window stuff */
						p_ptr->window |= (PW_OVERHEAD);

						/* Handle stuff */
						handle_stuff();

						/* Recalculate interesting grids */
						target_set_prepare(mode);

						/* Look at boring grids */
						flag = FALSE;

						/* Move */
						x += dx;
						y += dy;

						/* Do not move horizontally if unnecessary */
						if (((x < panel_col_min + wid / 2) && (dx > 0)) ||
							 ((x > panel_col_min + wid / 2) && (dx < 0)))
						{
							dx = 0;
						}

						/* Do not move vertically if unnecessary */
						if (((y < panel_row_min + hgt / 2) && (dy > 0)) ||
							 ((y > panel_row_min + hgt / 2) && (dy < 0)))
						{
							dy = 0;
						}

						/* Apply the motion */
						if ((y >= panel_row_min+hgt) || (y < panel_row_min) ||
						    (x >= panel_col_min+wid) || (x < panel_col_min))
						{
							if (change_panel(dy, dx)) target_set_prepare(mode);
						}

						/* Slide into legality */
						if (x >= cur_wid-1) x = cur_wid - 2;
						else if (x <= 0) x = 1;

						/* Slide into legality */
						if (y >= cur_hgt-1) y = cur_hgt- 2;
						else if (y <= 0) y = 1;
					}
				}

				/* Use that grid */
				m = i;

				/* Show objects on floor in subwindow */
				look_y = temp_y[i];
				look_x = temp_x[i];
				p_ptr->window |= (PW_FLOOR);
				handle_stuff();
			}
		}

		/* Arbitrary grids */
		else
		{
			if (!(mode & TARGET_LOOK)) prt_path(y, x);

			/* Default prompt */
#ifdef JP
strcpy(info, "q止 t決 p自 m近 +次 -前");
#else
			strcpy(info, "q,t,p,m,+,-,<dir>");
#endif


			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_aux(y, x, mode | TARGET_LOOK, info);

			/* XTRA HACK MOVEFAST */
			move_fast = FALSE;

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 't':
				case '.':
				case '5':
				case '0':
				{
					target_who = -1;
					target_row = y;
					target_col = x;
					done = TRUE;
					break;
				}

				case 'p':
				{
					/* Recenter the map around the player */
					verify_panel();

					/* Update stuff */
					p_ptr->update |= (PU_MONSTERS);

					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					/* Window stuff */
					p_ptr->window |= (PW_OVERHEAD);

					/* Handle stuff */
					handle_stuff();

					/* Recalculate interesting grids */
					target_set_prepare(mode);

					y = py;
					x = px;
				}

				case 'o':
				{
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '-':
				case 'm':
				{
					flag = TRUE;

					m = 0;
					bd = 999;

					/* Pick a nearby monster */
					for (i = 0; i < temp_n; i++)
					{
						t = distance(y, x, temp_y[i], temp_x[i]);

						/* Pick closest */
						if (t < bd)
						{
							m = i;
							bd = t;
						}
					}

					/* Nothing interesting */
					if (bd == 999) flag = FALSE;

					break;
				}

				default:
				{
					/* Extract the action (if any) */
					d = get_keymap_dir(query);

					/* XTRA HACK MOVEFAST */
					if (isupper(query)) move_fast = TRUE;

					if (!d) bell();
					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				int dx = ddx[d];
				int dy = ddy[d];

				/* XTRA HACK MOVEFAST */
				/* Move */
				if (move_fast)
				{
					x += dx * wid / 2;
					y += dy * hgt / 2;
				}
				else
				{
					x += dx;
					y += dy;
				}

				/* Do not move horizontally if unnecessary */
				if (((x < panel_col_min + wid / 2) && (dx > 0)) ||
					 ((x > panel_col_min + wid / 2) && (dx < 0)))
				{
					dx = 0;
				}

				/* Do not move vertically if unnecessary */
				if (((y < panel_row_min + hgt / 2) && (dy > 0)) ||
					 ((y > panel_row_min + hgt / 2) && (dy < 0)))
				{
					dy = 0;
				}

				/* Apply the motion */
				if ((y >= panel_row_min + hgt) || (y < panel_row_min) ||
					 (x >= panel_col_min + wid) || (x < panel_col_min))
				{
					if (change_panel(dy, dx)) target_set_prepare(mode);
				}

				/* Slide into legality */
				if (x >= cur_wid-1) x = cur_wid - 2;
				else if (x <= 0) x = 1;

				/* Slide into legality */
				if (y >= cur_hgt-1) y = cur_hgt- 2;
				else if (y <= 0) y = 1;

				/* Show objects on floor in subwindow */
				look_y = y;
				look_x = x;
				p_ptr->window |= (PW_FLOOR);
				handle_stuff();
			}
		}
	}

	/* Forget */
	temp_n = 0;
	look_y = py;
	look_x = px;

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_FLOOR);

	/* Handle stuff */
	handle_stuff();

	/* Failure to set target */
	if (!target_who) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Get an "aiming direction" from the user.
 *
 * The "dir" is loaded with 1,2,3,4,6,7,8,9 for "actual direction", and
 * "0" for "current target", and "-1" for "entry aborted".
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Note that confusion over-rides any (explicit?) user choice.
 */
bool get_aim_dir(int *dp)
{
	int		dir;

	char	command;

	cptr	p;
	
	/* Initialize */
	*dp = 0;
	
	/* Global direction */
	dir = command_dir;
	
	/* Hack -- auto-target if requested */
	if (use_old_target && target_okay()) dir = 5;
	
	/* Repeat previous command */
	if (repeat_pull(dp))
	{
		/* Confusion? */

		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
			/* Store direction */
			dir = *dp;
		}
	}

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
#ifdef JP
p = "方向 ('*'でターゲット選択, ESCで中断)? ";
#else
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
#endif

		}
		else
		{
#ifdef JP
p = "方向 ('5'でターゲットへ, '*'でターゲット再選択, ESCで中断)? ";
#else
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
#endif

		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &command)) break;

		/* Convert various keys to "standard" keys */
		switch (command)
		{
			/* Use current target */
			case 'T':
			case 't':
			case '.':
			case '5':
			case '0':
			{
				dir = 5;
				break;
			}

			/* Set new target */
			case ' ':
			case '*':
			{
				if (target_set(TARGET_KILL | TARGET_HOST)) dir = 5;
				break;
			}

			default:
			{
				/* Extract the action (if any) */
				dir = get_keymap_dir(command);

				break;
			}
		}

		/* Verify requested targets */
		if ((dir == 5) && !target_okay()) dir = 0;

		/* Error */
		if (!dir) bell();
	}

	/* No direction */
	if (!dir)
	{
		project_length = 0;
		return (FALSE);
	}

	/* Save the direction */
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* Random direction */
		dir = ddd[randint0(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
#ifdef JP
msg_print("あなたは混乱している。");
#else
		msg_print("You are confused.");
#endif

	}

	/* Save direction */
	(*dp) = dir;

	/* Remember the command for repeating */
	repeat_push(command_dir);

	/* A "valid" direction was entered */
	return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user,
 * and place it into "command_dir", unless we already have one.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.  Note that,
 * for example, it is no longer possible to "disarm" or "open" chests
 * in the same grid as the player.
 *
 * Direction "5" is illegal and will (cleanly) abort the command.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", to which "confusion" is applied.
 */
bool get_rep_dir_aux(int *dp, bool under)
{
	int dir;

	/* Repeat previous command */
	if (repeat_pull(dp))
	{
		return (TRUE);
	}

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

	/* Get a direction */
	while (!dir)
	{
		char ch;

		/* Get a command (or Cancel) */
#ifdef JP
if (!get_com("方向 (ESCで中断)? ", &ch)) break;
#else
		if (!get_com("Direction (Escape to cancel)? ", &ch)) break;
#endif


		/* Look up the direction */
		dir = get_keymap_dir(ch);

		/* Oops */
		if (!dir) bell();
	}

	/* Prevent weirdness */
	if (!under && (dir == 5)) dir = 0;

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	command_dir = dir;

	/* Apply "confusion" */
	if (p_ptr->confused)
	{
		/* Standard confusion */
		if (randint0(100) < 75)
		{
			/* Random direction */
			dir = ddd[randint0(8)];
		}
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
#ifdef JP
msg_print("あなたは混乱している。");
#else
		msg_print("You are confused.");
#endif

	}

	/* Save direction */
	(*dp) = dir;

	/* Remember the command for repeating */
	repeat_push(dir);

	/* Success */
	return (TRUE);
}


bool get_rep_dir(int *dp)
{
	return get_rep_dir_aux(dp, FALSE);
}


/*
 * XAngband: determine if a given location is "interesting"
 * based on target_set_accept function.
 */
static bool tgt_pt_accept(int y, int x)
{
	cave_type *c_ptr;

	/* Bounds */
	if (!(in_bounds(y, x))) return (FALSE);

	/* Player grid is always interesting */
	if ((y == py) && (x == px)) return (TRUE);

	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);

	/* Examine the grid */
	c_ptr = &cave[y][x];

	/* Interesting memorized features */
	if (c_ptr->info & (CAVE_MARK))
	{
		/* Notice stairs */
		if (c_ptr->feat == FEAT_LESS) return (TRUE);
		if (c_ptr->feat == FEAT_LESS_LESS) return (TRUE);

		if (c_ptr->feat == FEAT_MORE) return (TRUE);
		if (c_ptr->feat == FEAT_MORE_MORE) return (TRUE);

		/* Notice quest features */
		if (c_ptr->feat == FEAT_QUEST_ENTER) return (TRUE);
		if (c_ptr->feat == FEAT_QUEST_DOWN) return (TRUE);

		if (c_ptr->feat == FEAT_QUEST_EXIT) return (TRUE);
		if (c_ptr->feat == FEAT_QUEST_UP) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * XAngband: Prepare the "temp" array for "tget_pt"
 * based on target_set_prepare funciton.
 */
static void tgt_pt_prepare(void)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	if (!expand_list) return;

	/* Scan the current panel */
	for (y = 1; y < cur_hgt; y++)
	{
		for (x = 1; x < cur_wid; x++)
		{
			/* Require "interesting" contents */
			if (!tgt_pt_accept(y, x)) continue;

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Target the nearest monster for shooting */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
}

/*
 * old -- from PsiAngband.
 */
bool tgt_pt(int *x_ptr, int *y_ptr)
{
	char ch = 0;
	int d, x, y, n = 0;
	bool success = FALSE;

	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);

	x = px;
	y = py;

	if (expand_list) 
	{
		tgt_pt_prepare();
	}

#ifdef JP
	msg_print("場所を選んでスペースキーを押して下さい。");
#else
	msg_print("Select a point and press space.");
#endif
	msg_flag = FALSE; /* prevents "-more-" message. */

	while ((ch != ESCAPE) && !success)
	{
		bool move_fast = FALSE;

		move_cursor_relative(y, x);
		ch = inkey();
		switch (ch)
		{
		case ESCAPE:
			break;
		case ' ':
		case 't':
		case '.':
		case '5':
		case '0':
			/* illegal place */
			if (x == px && y == py) ch = 0;
			
			/* okay place */
			else success = TRUE;

			break;

		/* XAngband: Move cursor to stairs */
		case '>':
		case '<':
			if (expand_list && temp_n)
			{
				int dx, dy;
				int cx = (panel_col_min + panel_col_max) / 2;
				int cy = (panel_row_min + panel_row_max) / 2;

				n++;

				while(n < temp_n)	/* Skip stairs which have defferent distance */
				{
					cave_type *c_ptr = &cave[temp_y[n]][temp_x[n]];

					if (ch == '>')
					{
						if ((c_ptr->feat == FEAT_LESS) || (c_ptr->feat == FEAT_LESS_LESS) ||
							(c_ptr->feat == FEAT_QUEST_ENTER) || (c_ptr->feat == FEAT_QUEST_DOWN))
							n++;
						else
							break;
					}
					else /* if (ch == '<') */
					{
						if ((c_ptr->feat == FEAT_MORE) || (c_ptr->feat == FEAT_MORE_MORE) ||
							(c_ptr->feat == FEAT_QUEST_EXIT) || (c_ptr->feat == FEAT_QUEST_UP))
							n++;
						else
							break;
					}
				}

				if (n == temp_n)	/* Loop out taget list */
				{
					n = 0;
					y = py;
					x = px;
					verify_panel();	/* Move cursor to player */

					/* Update stuff */
					p_ptr->update |= (PU_MONSTERS);

					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					/* Window stuff */
					p_ptr->window |= (PW_OVERHEAD);

					/* Handle stuff */
					handle_stuff();
				}
				else	/* move cursor to next stair and change panel */
				{
					y = temp_y[n];
					x = temp_x[n];

					dy = 2 * (y - cy) / hgt;
					dx = 2 * (x - cx) / wid;
					if (dy || dx) change_panel(dy, dx);
				}
			}
			break;

		default:
			/* Look up the direction */
			d = get_keymap_dir(ch);
#ifdef JP
			/* XTRA HACK MOVEFAST */
			if (isupper(ch)) move_fast = TRUE;
#endif

			/* Handle "direction" */
			if (d)
			{
				int dx = ddx[d];
				int dy = ddy[d];

#ifdef JP
				/* XTRA HACK MOVEFAST */
				if (move_fast)
				{
					x += dx * wid / 2;
					y += dy * hgt / 2;
				} else {
					x += dx;
					y += dy;
				}
#else
				/* Move */
				x += dx;
				y += dy;
#endif
				/* Do not move horizontally if unnecessary */
				if (((x < panel_col_min + wid / 2) && (dx > 0)) ||
					 ((x > panel_col_min + wid / 2) && (dx < 0)))
				{
					dx = 0;
				}

				/* Do not move vertically if unnecessary */
				if (((y < panel_row_min + hgt / 2) && (dy > 0)) ||
					 ((y > panel_row_min + hgt / 2) && (dy < 0)))
				{
					dy = 0;
				}

				/* Apply the motion */
				if ((y >= panel_row_min + hgt) || (y < panel_row_min) ||
					 (x >= panel_col_min + wid) || (x < panel_col_min))
				{
					/* if (change_panel(dy, dx)) target_set_prepare(mode); */
					change_panel(dy, dx);
				}

				/* Slide into legality */
				if (x >= cur_wid-1) x = cur_wid - 2;
				else if (x <= 0) x = 1;

				/* Slide into legality */
				if (y >= cur_hgt-1) y = cur_hgt- 2;
				else if (y <= 0) y = 1;

			}
			break;
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	*x_ptr = x;
	*y_ptr = y;
	return success;
}


bool get_hack_dir(int *dp)
{
	int		dir;
	cptr    p;
	char    command;


	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = 0;

	/* (No auto-targeting) */

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
#ifdef JP
p = "方向 ('*'でターゲット選択, ESCで中断)? ";
#else
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
#endif

		}
		else
		{
#ifdef JP
p = "方向 ('5'でターゲットへ, '*'でターゲット再選択, ESCで中断)? ";
#else
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
#endif

		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &command)) break;

		/* Convert various keys to "standard" keys */
		switch (command)
		{
			/* Use current target */
			case 'T':
			case 't':
			case '.':
			case '5':
			case '0':
			{
				dir = 5;
				break;
			}

			/* Set new target */
			case ' ':
			case '*':
			{
				if (target_set(TARGET_KILL)) dir = 5;
				break;
			}

			default:
			{
				/* Look up the direction */
				dir = get_keymap_dir(command);

				break;
			}
		}

		/* Verify requested targets */
		if ((dir == 5) && !target_okay()) dir = 0;

		/* Error */
		if (!dir) bell();
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[randint0(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
#ifdef JP
msg_print("あなたは混乱している。");
#else
		msg_print("You are confused.");
#endif

	}

	/* Save direction */
	(*dp) = dir;

	/* A "valid" direction was entered */
	return (TRUE);
}


/*
 * Return bow energy 
 */
s16b bow_energy(int sval)
{
	int energy = 100;

	/* Analyze the launcher */
	switch (sval)
	{
		/* Sling and ammo */
		case SV_SLING:
		{
			energy = 50;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			energy = 100;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			energy = 100;
			break;
		}

		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			energy = 120;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			 if (p_ptr->stat_use[A_DEX] >= 16)
			 {
				 energy = 150;
			 }
			 else
			 {
				 /* players with low dex will take longer to load */
				 energy = 200;
			 }
			break;
		}
	}

	return (energy);
}


/*
 * Return bow tmul
 */
int bow_tmul(int sval)
{
	int tmul = 0;

	/* Analyze the launcher */
	switch (sval)
	{
		/* Sling and ammo */
		case SV_SLING:
		{
			tmul = 2;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			 if (p_ptr->stat_use[A_STR] >= 16)
			 {
				 tmul = 3;
			 }
			 else
			 {
				 /* weak players cannot use a longbow well */
				 tmul = 2;
			 }
			break;
		}

		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			tmul = 4;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			tmul = 5;
			break;
		}
	}

	return (tmul);
}

void gain_level_reward(int chosen_reward)
{
	int i;
	int type, effect;
	cptr reward = NULL;

	if (!chosen_reward)
	{
		if (multi_rew) return;
		else multi_rew = TRUE;
	}

	type = randint0(MAX_REWARDS);
	effect = valar_rewards[p_ptr->valar_patron][type];

	switch (chosen_reward ? chosen_reward : effect)
	{
		case REW_IGNORE:
#ifdef JP
			msg_format("%sはあなたを無視した。",
				valar_patrons[p_ptr->valar_patron]);
			reward = "無視してもらった。";
#else
			msg_format("%s ignores you.",
				valar_patrons[p_ptr->valar_patron]);
			reward = "nothing";
#endif
			break;
		case REW_BLESS:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「汝に祝福あれ。」");
			reward = "祝福された。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me bless thee.'");
			reward = "blessing";
#endif
			set_blessed(p_ptr->blessed + randint1(100) + 100);
			break;
		case REW_HERO:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「汝に勇気を与えよう。」");
			reward = "勇気をもらった。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'I give thine courage.'");
			reward = "heroism";
#endif
			set_hero(p_ptr->hero + randint1(100) + 100);
			break;
		case REW_SPEED:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「汝に素早さを与えよう。」");
			reward = "動きが速くなった。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'I give thine quickness.'");
			reward = "temporary speed";
#endif
			set_slow(0);
			set_fast(randint1(100) + 100);
			break;
		case REW_RESTORE:
#ifdef JP
			msg_format("%sの声がささやいた:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「余が汝の身体を癒さん。」");
			reward = "身体が癒された";
#else
			msg_format("The voice of %s whispers:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me cure thee.'");
			reward = "curing";
#endif
			restore_level();
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			hp_player(50);
			for (i = 0; i < 6; i++)
			{
				(void)do_res_stat(i);
			}
			break;
		case REW_ENLIGHT:
#ifdef JP
			msg_format("%sの声がささやいた:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「余の見えしものを汝に見せようぞ。」");
			reward = "階の情報を手に入れた。";
#else
			msg_format("The voice of %s whispers:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me show thee anything that i can look.'");
			reward = "an enlightenment";
#endif
			wiz_lite();
			break;
		case REW_GREA_OBJ:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「我が与えし物を賢明に使うべし。」");
			reward = "高級品のアイテムを手に入れた。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Use my gift wisely.'");
			reward = "an excellent item";
#endif
			acquirement(py, px, 1, TRUE, FALSE);
			break;
		case REW_GREA_OBS:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「下僕よ、汝の献身への我が惜しみ無き報いを見るがよい。」");
			reward = "高級品のアイテムを手に入れた。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Behold, mortal, how generously I reward thy loyalty.'");
			reward = "excellent items";
#endif
			acquirement(py, px, randint1(2) + 1, TRUE, FALSE);
			break;
		case REW_GAIN_ABL:
#ifdef JP
			msg_format("%sの声が鳴り響いた:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「留まるのだ、下僕よ。余が汝の肉体を鍛えん。」");
			reward = "能力値が1つ上がった。";
#else
			msg_format("The voice of %s rings out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Stay, mortal, and let me mold thee.'");
			reward = "increasing a stat.";
#endif
			do_inc_stat(randint0(6));
			break;
		case REW_AUGM_ABL:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「我がささやかなる賜物を受けとるがよい！」");
			reward = "全能力値が上がった。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Receive this modest gift from me!'");
			reward = "increasing all stats";
#endif
			for (i = 0; i < 6; i++)
			{
				(void)do_inc_stat(i);
			}
			break;
	   case REW_HEAL:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「甦るがよい、我が下僕よ！」");
			reward = "体力が回復した。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Rise, my servant!'");
			reward = "healing";
#endif
			restore_level();
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			hp_player(5000);
			for (i = 0; i < 6; i++)
			{
				(void)do_res_stat(i);
			}
			break;
		case REW_POTION:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「我がささやかなる賜物を受けとるがよい！」");
			reward = "薬を1服もらった。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Receive this modest gift from me!'");
			reward = "an potion";
#endif
			{
				object_type forge;
				object_type *q_ptr = &forge;

				object_prep(q_ptr, lookup_kind(TV_POTION, SV_POTION_HEALING));
				(void)drop_near(q_ptr, -1, py, px);
			}
			break;
		case REW_RE_CURSE:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「我、汝の呪いを消滅せん！」");
			reward = "呪いが解かれた";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me remove all curses, thine!'");
			reward = "removing curses";
#endif
			remove_all_curse();
			break;
		case REW_GENOCIDE:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「我、汝の敵を抹殺せん！」");
			reward = "モンスターが抹殺された。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me relieve thee of thine oppressors!'");
			reward = "genociding monsters";
#endif
			(void)genocide(0);
			break;
		case REW_MASS_GEN:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「我、汝の敵を抹殺せん！」");
			reward = "モンスターが抹殺された。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me relieve thee of thine oppressors!'");
			reward = "genociding nearby monsters";
#endif
			(void)mass_genocide(0);
			break;
		case REW_DISPEL_C:
#ifdef JP
			msg_format("%sの力が敵を攻撃するのを感じた！",
				valar_patrons[p_ptr->valar_patron]);
			reward = "周囲の敵が攻撃された。";
#else
			msg_format("You can feel the power of %s assault your enemies!",
				valar_patrons[p_ptr->valar_patron]);
			reward = "dispel monsters";
#endif
			(void)dispel_monsters(p_ptr->lev * 8);
			break;
		case REW_WISHING:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「汝の願い聞きいれようぞ！」");
			reward = "願いを聞いてもらった。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me accept thine wish!'");
			reward = "wishing";
#endif
			for (i = 0; i < 3; i++)
			{
				int lev = 1 + (p_ptr->lev * 2/ 3);
				if (do_cmd_wishing(lev, TRUE, TRUE, TRUE) != -1) break;
			}
			break;
		case REW_PROTEVIL:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「邪悪より汝を護らん。」");
			reward = "邪悪から護られた。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Let me protect thee from evilness.'");
			reward = "protecting from evil";
#endif
			set_protevil(p_ptr->protevil + randint1(100) + 100);
			break;
		case REW_AMULET:
#ifdef JP
			msg_format("%sの声が響き渡った:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("「暗き中にも華やかであれ！」");
			reward = "特別なアミュレットをもらった。";
#else
			msg_format("The voice of %s booms out:",
				valar_patrons[p_ptr->valar_patron]);
			msg_print("'Be gorgeous, even if in darkness!'");
			reward = "a special amulet";
#endif
			{
				object_type forge;
				object_type *q_ptr = &forge;

				object_prep(q_ptr, lookup_kind(TV_AMULET, SV_AMULET_ADORNMENT));
				(void)create_artifact(q_ptr, FALSE);
				(void)drop_near(q_ptr, -1, py, px);
			}
			break;
		default:
#ifdef JP
			msg_format("%sの声がどもった:",
				valar_patrons[p_ptr->valar_patron]);
			msg_format("「あー、あー、答えは %d/%d。質問は何？」", type, effect);
#else
			msg_format("The voice of %s stammers:",
				valar_patrons[p_ptr->valar_patron]);
			msg_format("'Uh... uh... the answer's %d/%d, what's the question?'", type, effect);
#endif
	}

	if (reward && take_notes)
	{
		add_note(reward, 'r');
	}
}
