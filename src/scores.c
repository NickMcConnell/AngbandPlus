/* File: scores.c */

/* Purpose: Highscores handling */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Seek score 'i' in the highscore file
 */
static int highscore_seek(int i)
{
	/* Seek for the requested record */
	return (fd_seek(highscore_fd, (huge)(i) * sizeof(high_score)));
}


/*
 * Read one score from the highscore file
 */
static errr highscore_read(high_score *score)
{
	/* Read the record, note failure */
	return (fd_read(highscore_fd, (char*)(score), sizeof(high_score)));
}


/*
 * Write one score to the highscore file
 */
static int highscore_write(high_score *score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (char*)(score), sizeof(high_score)));
}


/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(high_score *score)
{
	int			i;

	high_score		the_score;
	int my_score;

	my_score = atoi(score->pts);

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (-1);

	/* Go to the start of the highscore file */
	if (highscore_seek(0)) return (-1);

	/* Read until we get to a higher score */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		int old_score;
		if (highscore_read(&the_score)) return (i);
		old_score = atoi(the_score.pts);
/*		if (strcmp(the_score.pts, score->pts) < 0) return (i); */
		if (my_score > old_score) return (i);
	}

	/* The "last" entry is always usable */
	return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(high_score *score)
{
	int			i, slot;
	bool		done = FALSE;

	high_score		the_score, tmpscore;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (-1);

	/* Determine where the score should go */
	slot = highscore_where(score);

	/* Hack -- Not on the list */
	if (slot < 0) return (-1);

	/* Hack -- prepare to dump the new score */
	the_score = (*score);

	/* Slide all the scores down one */
	for (i = slot; !done && (i < MAX_HISCORES); i++)
	{
		/* Read the old guy, note errors */
		if (highscore_seek(i)) return (-1);
		if (highscore_read(&tmpscore)) done = TRUE;

		/* Back up and dump the score we were holding */
		if (highscore_seek(i)) return (-1);
		if (highscore_write(&the_score)) return (-1);

		/* Hack -- Save the old score, for the next pass */
		the_score = tmpscore;
	}

	/* Return location used */
	return (slot);
}



/*
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 * Only five entries per line, too much info.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
void display_scores_aux(int from, int to, int note, high_score *score)
{
	int		i, j, k, n, place;
	byte attr;

	high_score	the_score;

	char	out_val[256];
	char	tmp_val[160];

	int wid, hgt, per_screen;

	Term_get_size(&wid, &hgt);
	per_screen = (hgt - 4) / 4;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return;


	/* Assume we will show the first 10 */
	if (from < 0) from = 0;
	if (to < 0) to = 10;
	if (to > MAX_HISCORES) to = MAX_HISCORES;


	/* Seek to the beginning */
	if (highscore_seek(0)) return;

	/* Hack -- Count the high scores */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score)) break;
	}

	/* Hack -- allow "fake" entry to be last */
	if ((note == i) && score) i++;

	/* Forget about the last entries */
	if (i > to) i = to;


	/* Show per_screen per page, until "done" */
	for (k = from, place = k+1; k < i; k += per_screen)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
#ifdef JP
		put_str("                TOband2: 勇者の殿堂", 0, 0);
#else
		put_str("                TOband2 Hall of Fame", 0, 0);
#endif


		/* Indicate non-top scores */
		if (k > 0)
		{
#ifdef JP
			sprintf(tmp_val, "( %d 位以下 )", k + 1);
#else
			sprintf(tmp_val, "(from position %d)", k + 1);
#endif

			put_str(tmp_val, 0, 40);
		}

		/* Dump per_screen entries */
		for (j = k, n = 0; j < i && n < per_screen; place++, j++, n++)
		{
			int pr, pc, clev, mlev, cdun, mdun, death_type;

			cptr user, gold, when, aged, die_verb;


			/* Hack -- indicate death in yellow */
			attr = (j == note) ? TERM_YELLOW : TERM_WHITE;


			/* Mega-Hack -- insert a "fake" record */
			if ((note == j) && score)
			{
				the_score = (*score);
				attr = TERM_L_GREEN;
				score = NULL;
				note = -1;
				j--;
			}

			/* Read a normal record */
			else
			{
				/* Read the proper record */
				if (highscore_seek(j)) break;
				if (highscore_read(&the_score)) break;
			}

			/* Extract the race/class */
			pr = atoi(the_score.p_r);
			pc = atoi(the_score.p_c);

			/* Extract the level info */
			clev = atoi(the_score.cur_lev);
			mlev = atoi(the_score.max_lev);
			cdun = atoi(the_score.cur_dun);
			mdun = atoi(the_score.max_dun);

			/* Extract death type */
			death_type = atoi(the_score.death_type);
			switch (death_type)
			{
			case 1:
#ifdef JP
				die_verb = "石化された";
#else
				die_verb = "Stoned";
#endif
				break;
			case 2:
#ifdef JP
				die_verb = "より武器に変化";
#else
				die_verb = "Changed into a weapon";
#endif
				break;
			default:
#ifdef JP
				die_verb = "殺された";
#else
				die_verb = "Killed";
#endif
				break;
			}

			/* Hack -- extract the gold and such */
			for (user = the_score.uid; isspace(*user); user++) /* loop */;
			for (when = the_score.day; isspace(*when); when++) /* loop */;
			for (gold = the_score.gold; isspace(*gold); gold++) /* loop */;
			for (aged = the_score.turns; isspace(*aged); aged++) /* loop */;

			/* Clean up standard encoded form of "when" */
			if ((*when == '@') && strlen(when) == 9)
			{
				sprintf(tmp_val, "%.4s-%.2s-%.2s",
				        when + 1, when + 5, when + 7);
				when = tmp_val;
			}

			/* Dump some info */
#ifdef JP
			/* sprintf(out_val, "%3d.%9s  %s%s%sという名の%sの%s (レベル %d)", */
			sprintf(out_val, "%3d.%9s  %s - %s%s (レベル %d)",
			        place, the_score.pts,
				the_score.who,
				race_info[pr].title, class_info[pc].title,
			        clev);

#else
			sprintf(out_val, "%3d.%9s  %s the %s %s, Level %d",
			        place, the_score.pts,
				the_score.who, race_info[pr].title, class_info[pc].title,
			        clev);
#endif


			/* Append a "maximum level" */
#ifdef JP
			if (mlev > clev) strcat(out_val, format(" (最高%d)", mlev));
#else
			if (mlev > clev) strcat(out_val, format(" (Max %d)", mlev));
#endif


			/* Dump the first line */
			c_put_str(attr, out_val, n*4 + 2, 0);

			/* Another line of info */
#ifdef JP
			if (mdun != 0)
				sprintf(out_val, "    最高%3d階", mdun);
			else
				sprintf(out_val, "             ");


			/* 死亡原因をオリジナルより細かく表示 */
			if (streq(the_score.how, "yet"))
			{
				sprintf(out_val+13, "  まだ生きている (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "ripe"))
			{
				sprintf(out_val+13, "  勝利の後に引退 (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "snap"))
			{
				sprintf(out_val+13, "  勝利の後に武器に変化 (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "walstanian"))
			{
				sprintf(out_val+13, "  勝利の後にウォルスタ人の刺客に暗殺された (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "gargastan"))
			{
				sprintf(out_val+13, "  勝利の後にガルガスタン人の刺客に暗殺された (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "bacrum"))
			{
				sprintf(out_val+13, "  勝利の後にバクラム人の刺客に暗殺された (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "z_or_l"))
			{
				sprintf(out_val+13, "  勝利の後に大陸からの刺客に暗殺された (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "valeria"))
			{
				sprintf(out_val+13, "  勝利の後に身元不明の刺客に暗殺された (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "lord"))
			{
				sprintf(out_val+13, "  勝利の後にヴァレリアの君主となる (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "ogre"))
			{
				sprintf(out_val+13, "  勝利の後に真のオウガとなる (%d%s)",
					cdun, "階");
			}
			else if (streq(the_score.how, "survive"))
			{
				sprintf(out_val+13, "  死者の宮殿から生還 (%d%s)",
					cdun, "階");
			}
			else
			{
				codeconv(the_score.how);

				/* Some people die outside of the dungeon */
				if (!cdun)
					sprintf(out_val+13, "  地上で%sに%s", the_score.how, die_verb);
				else
					sprintf(out_val+13, "  %d階で%sに%s",
						cdun, the_score.how, die_verb);
			}

#else
			/* Some people die outside of the dungeon */
			if (!cdun)
				sprintf(out_val, 
					"               %s by %s on the surface",
					die_verb, the_score.how);
			else
				sprintf(out_val, 
					"               %s by %s on %s %d",
					die_verb, the_score.how, "Dungeon Level", cdun);

			/* Append a "maximum level" */
			if (mdun > cdun) strcat(out_val, format(" (Max %d)", mdun));
#endif

			/* Dump the info */
			c_put_str(attr, out_val, n*4 + 3, 0);

			/* And still another line of info */
#ifdef JP
			{
				char buf[11];

				/* 日付を 19yy/mm/dd の形式に変更する */
				if (strlen(when) == 8 && when[2] == '/' && when[5] == '/')
				{
					sprintf(buf, "%d%s/%.5s", 19 + (when[6] < '8'), when + 6, when);
					when = buf;
				}
				sprintf(out_val,
						"        (ユーザー:%s, 日付:%s, 所持金:%s, ターン:%s)",
						user, when, gold, aged);
			}

#else
			sprintf(out_val,
			        "               (User %s, Date %s, Gold %s, Turn %s).",
			        user, when, gold, aged);
#endif

			c_put_str(attr, out_val, n*4 + 4, 0);
		}


		/* Wait for response */
#ifdef JP
		prt("[ ESCで中断, その他のキーで続けます ]", hgt - 1, 21);
#else
		prt("[Press ESC to quit, any other key to continue.]", hgt - 1, 17);
#endif

		j = inkey();
		prt("", hgt - 1, 0);

		/* Hack -- notice Escape */
		if (j == ESCAPE) break;
	}
}


/*
 * Hack -- Display the scores in a given range and quit.
 *
 * This function is only called from "main.c" when the user asks
 * to see the "high scores".
 */
void display_scores(int from, int to)
{
	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
#ifdef JP
	if (highscore_fd < 0) quit("スコア・ファイルが使用できません。");
#else
	if (highscore_fd < 0) quit("Score file unavailable.");
#endif


	/* Clear screen */
	Term_clear();

	/* Display the scores */
	display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Quit */
	quit(NULL);
}



/*
 * Enters a players name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
errr top_twenty(void)
{
	int          j;

	high_score   the_score;

	time_t ct = time((time_t*)0);

	/* Clear the record */
	(void)WIPE(&the_score, high_score);

	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
	        T_VER_MAJOR, T_VER_MINOR, T_VER_PATCH);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9ld", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au_sum);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn_real(turn));
	the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
	/* Save the date in a hacked up form (9 chars) */
	(void)sprintf(the_score.day, "%-.6s %-.2s", ctime(&ct) + 4, ctime(&ct) + 22);
#else
	/* Save the date in standard form (8 chars) */
/*	(void)strftime(the_score.day, 9, "%m/%d/%y", localtime(&ct)); */
	/* Save the date in standard encoded form (9 chars) */
	strftime(the_score.day, 10, "@%Y%m%d", localtime(&ct));
#endif

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);

	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3ld", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", dun_level);
	sprintf(the_score.max_lev, "%3ld", p_ptr->max_plv);
	sprintf(the_score.max_dun, "%3d", max_dlv[dungeon_type]);

	/* Save death type */
	sprintf(the_score.death_type, "%2d", (p_ptr->is_dead & DEATH_SNAP_DRAGON) ? 2 :
		((p_ptr->is_dead & DEATH_STONED) ? 1 : 0));

	/* Save the cause of death (31 chars) */
	if (strlen(p_ptr->died_from) >= sizeof(the_score.how))
	{
#ifdef JP
		my_strcpy(the_score.how, p_ptr->died_from, sizeof(the_score.how) - 2);
		strcat(the_score.how, "…");
#else
		my_strcpy(the_score.how, p_ptr->died_from, sizeof(the_score.how) - 3);
		strcat(the_score.how, "...");
#endif
	}
	else
	{
		strcpy(the_score.how, p_ptr->died_from);
	}

	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return (1);

	/* Add a new entry to the score list, see where it went */
	j = highscore_add(&the_score);

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return (1);


	/* Hack -- Display the top fifteen scores */
	if (j < 10)
	{
		display_scores_aux(0, 15, j, NULL);
	}

	/* Display the scores surrounding the player */
	else
	{
		display_scores_aux(0, 5, j, NULL);
		display_scores_aux(j - 2, j + 7, j, NULL);
	}


	/* Success */
	return (0);
}


/*
 * Predict the players location, and display it.
 */
errr predict_score(void)
{
	int          j;

	high_score   the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
#ifdef JP
		msg_print("スコア・ファイルが使用できません。");
#else
		msg_print("Score file unavailable.");
#endif

		msg_print(NULL);
		return (0);
	}


	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
	        T_VER_MAJOR, T_VER_MINOR, T_VER_PATCH);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9ld", (long)total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au_sum);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn_real(turn));

	/* Hack -- no time needed */
#ifdef JP
	strcpy(the_score.day, "今日");
#else
	strcpy(the_score.day, "TODAY");
#endif


	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);

	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3ld", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", dun_level);
	sprintf(the_score.max_lev, "%3ld", p_ptr->max_plv);
	sprintf(the_score.max_dun, "%3d", max_dlv[dungeon_type]);

	/* Save death type */
	sprintf(the_score.death_type, "%2d", (p_ptr->is_dead & DEATH_SNAP_DRAGON) ? 2 :
		((p_ptr->is_dead & DEATH_STONED) ? 1 : 0));

	/* Hack -- no cause of death */
#ifdef JP
	/* まだ死んでいないときの識別文字 */
	strcpy(the_score.how, "yet");
#else
	strcpy(the_score.how, "nobody (yet!)");
#endif



	/* See where the entry would be placed */
	j = highscore_where(&the_score);


	/* Hack -- Display the top fifteen scores */
	if (j < 10)
	{
		display_scores_aux(0, 15, j, &the_score);
	}

	/* Display some "useful" scores */
	else
	{
		display_scores_aux(0, 5, -1, NULL);
		display_scores_aux(j - 2, j + 7, j, &the_score);
	}


	/* Success */
	return (0);
}



/*
 * show_highclass - selectively list highscores based on class
 * -KMW-
 */
void show_highclass(void)
{
	register int i = 0, j, m = 0;
	int pr, clev/*, al*/;
	high_score the_score;
	char buf[1024], out_val[256];

	screen_save();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
#ifdef JP
		msg_print("スコア・ファイルが使用できません。");
#else
		msg_print("Score file unavailable.");
#endif

		msg_print(NULL);
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++)
		if (highscore_read(&the_score)) break;

	m = 0;
	j = 0;
	clev = 0;

	while ((m < 9) && (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		clev = atoi(the_score.cur_lev);

#ifdef JP
		sprintf(out_val, "   %3d) %sの%s (レベル %2d)",
		    (m + 1), race_info[pr].title,the_score.who, clev);
#else
		sprintf(out_val, "%3d) %s the %s (Level %2d)",
		    (m + 1), the_score.who, race_info[pr].title, clev);
#endif

		prt(out_val, (m + 7), 0);
		m++;
		j++;
	}

#ifdef JP
	sprintf(out_val, "あなた) %sの%s (レベル %2ld)",
	    race_info[p_ptr->prace].title,player_name, p_ptr->lev);
#else
	sprintf(out_val, "You) %s the %s (Level %2ld)",
	    player_name, race_info[p_ptr->prace].title, p_ptr->lev);
#endif

	prt(out_val, (m + 8), 0);

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
#ifdef JP
	prt("何かキーを押すとゲームに戻ります",0,0);
#else
	prt("Hit any key to continue",0,0);
#endif

	(void)inkey();

	for (j = 5; j < 18; j++) prt("", j, 0);
	screen_load();
}


/*
 * Race Legends
 * -KMW-
 */
void race_score(int race_num)
{
	register int i = 0, j, m = 0;
	int pr, clev, lastlev;
	high_score the_score;
	char buf[1024], out_val[256], tmp_str[80];

	lastlev = 0;

	/* rr9: TODO - pluralize the race */
#ifdef JP
	sprintf(tmp_str,"最高の%s", race_info[race_num].title);
#else
	sprintf(tmp_str,"The Greatest of all the %s", race_info[race_num].title);
#endif

	prt(tmp_str, 5, 15);

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
#ifdef JP
		msg_print("スコア・ファイルが使用できません。");
#else
		msg_print("Score file unavailable.");
#endif

		msg_print(NULL);
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score)) break;
	}

	m = 0;
	j = 0;

	while ((m < 10) || (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		clev = atoi(the_score.cur_lev);

		if (pr == race_num)
		{
#ifdef JP
		sprintf(out_val, "   %3d) %sの%s (レベル %2d)",
			    (m + 1), race_info[pr].title, 
				the_score.who,clev);
#else
			sprintf(out_val, "%3d) %s the %s (Level %3d)",
			    (m + 1), the_score.who,
			race_info[pr].title, clev);
#endif

			prt(out_val, (m + 7), 0);
			m++;
			lastlev = clev;
		}
		j++;
	}

	/* add player if qualified */
	if ((p_ptr->prace == race_num) && (p_ptr->lev >= lastlev))
	{
#ifdef JP
	sprintf(out_val, "あなた) %sの%s (レベル %2ld)",
		     race_info[p_ptr->prace].title,player_name, p_ptr->lev);
#else
		sprintf(out_val, "You) %s the %s (Level %3ld)",
		    player_name, race_info[p_ptr->prace].title, p_ptr->lev);
#endif

		prt(out_val, (m + 8), 0);
	}

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
}


/*
 * Race Legends
 * -KMW-
 */
void race_legends(void)
{
	int i, j;

	for (i = 0; i < MAX_RACES; i++)
	{
		race_score(i);
#ifdef JP
		msg_print("何かキーを押すとゲームに戻ります");
#else
		msg_print("Hit any key to continue");
#endif

		msg_print(NULL);
		for (j = 5; j < 19; j++)
			prt("", j, 0);
	}
}


typedef struct chaos_frame_sort_type chaos_frame_sort_type;

struct chaos_frame_sort_type
{
	int num;
	int val;
};

static int chaos_frame_sort_cmp(chaos_frame_sort_type *a, chaos_frame_sort_type *b)
{
	return a->val - b->val;
}

#define ENDING_TYPE_KILLED_BY_WALSTANIAN 1
#define ENDING_TYPE_KILLED_BY_GARGASTAN  2
#define ENDING_TYPE_KILLED_BY_BACRUM     3
#define ENDING_TYPE_KILLED_BY_Z_OR_L     4
#define ENDING_TYPE_KILLED_BY_VALERIA    5
#define ENDING_TYPE_LORD                 6
#define ENDING_TYPE_OGRE                 7

static void display_ending(void)
{
	int i, ending_type;
	int wid, hgt;
	int cx, cy;
	chaos_frame_sort_type chaos_frame_sort_array[ETHNICITY_NUM];
	cptr align_str = NULL, killer_str = NULL;

	Term_get_size(&wid, &hgt);
	cy = hgt / 2;
	cx = wid / 2;

	for (i = 0; i < ETHNICITY_NUM; i++)
	{
		chaos_frame_sort_array[i].num = i;
		chaos_frame_sort_array[i].val = chaos_frame[i];
	}
	qsort(chaos_frame_sort_array, ETHNICITY_NUM, sizeof(chaos_frame_sort_type), (int(*)(const void *, const void *))chaos_frame_sort_cmp);

	/* Set message type */
	if (!r_info[MON_FILARHH].max_num)
	{
		ending_type = ENDING_TYPE_OGRE;
	}
	else if (chaos_frame_sort_array[0].val >= 0)
	{
		ending_type = ENDING_TYPE_LORD;
	}
	else if (chaos_frame_sort_array[0].val == chaos_frame_sort_array[1].val)
	{
		ending_type = ENDING_TYPE_KILLED_BY_VALERIA;
	}
	else
	{
		switch (chaos_frame_sort_array[0].num)
		{
		case ETHNICITY_WALSTANIAN:
			ending_type = ENDING_TYPE_KILLED_BY_WALSTANIAN;
			break;

		case ETHNICITY_GARGASTAN:
			ending_type = ENDING_TYPE_KILLED_BY_GARGASTAN;
			break;

		case ETHNICITY_BACRUM:
			ending_type = ENDING_TYPE_KILLED_BY_BACRUM;
			break;

		default:
			ending_type = ENDING_TYPE_KILLED_BY_Z_OR_L;
			break;
		}
	}

	/* Clear screen */
	Term_clear();

	if (ending_type != ENDING_TYPE_OGRE)
	{
		put_str("「…大いなる父・フィラーハの名の下に、", cy - 1, cx - 17);
		put_str(format("汝、%sをヴァレリアの王と認め、", player_name), cy, cx - 17);
		put_str("ここにヴァレリアの称号を与える……。」", cy + 1, cx - 17);

		/* Flush input */
		flush();

		/* Wait for response */
		pause_line(hgt - 1);

		/* Clear screen */
		Term_clear();
	}

	switch (ending_type)
	{
	case ENDING_TYPE_KILLED_BY_WALSTANIAN:
	case ENDING_TYPE_KILLED_BY_GARGASTAN:
	case ENDING_TYPE_KILLED_BY_BACRUM:
	case ENDING_TYPE_KILLED_BY_Z_OR_L:
	case ENDING_TYPE_KILLED_BY_VALERIA:
		switch (get_your_alignment_lnc())
		{
		case ALIGN_LNC_LAWFUL:
			align_str = "偽善者";
			break;
		case ALIGN_LNC_NEUTRAL:
			align_str = "日和見";
			break;
		case ALIGN_LNC_CHAOTIC:
			align_str = "オウガ";
			break;
		}

		switch (ending_type)
		{
		case ENDING_TYPE_KILLED_BY_WALSTANIAN:
			killer_str = "ウォルスタに栄光あれーッ！";
			break;
		case ENDING_TYPE_KILLED_BY_GARGASTAN:
			killer_str = "ガルガスタンに栄光あれーッ！";
			break;
		case ENDING_TYPE_KILLED_BY_BACRUM:
			killer_str = "バクラムに栄光あれーッ！";
			break;
		case ENDING_TYPE_KILLED_BY_Z_OR_L:
			killer_str = "大陸の脅威となる前に消えてもらおうッ！";
			break;
		case ENDING_TYPE_KILLED_BY_VALERIA:
			killer_str = "ヴァレリアに栄光あれーッ！";
			break;
		}

		put_str(format("「%sを誅すッ！  %s」", align_str, killer_str), cy - 2, cx - 30);
		sound(SOUND_SHOOT_GUN);
		put_str("刹那、耳をつんざくような破裂音が響き渡り、", cy, cx - 30);
		put_str("後には、銃弾で胸を貫かれたあなたの死体が残った。", cy + 1, cx - 30);
		put_str("その後、ヒッタイト人に統一されるまで、約千年もの間、", cy + 2, cx - 30);
		put_str("ヴァレリアの歴史は血と涙に彩られたという……。", cy + 3, cx - 30);

		/* Flush input */
		flush();

		/* Wait for response */
		pause_line(hgt - 1);

		/* Clear screen */
		Term_clear();

		put_str("「王は全て善良にして清廉とは限らぬ、", cy - 3, cx - 18);
		put_str("勝利する者すべてが、王の器ではない。", cy - 2, cx - 18);
		put_str("殺めるも、強きは絶えぬ、", cy - 1, cx - 18);
		put_str("深き根に、恐怖は届かぬ。", cy, cx - 18);
		put_str("灰の中から呪詛の声は蘇り、", cy + 1, cx - 18);
		put_str("影から憤怒がさしいづるだろう。", cy + 2, cx - 18);
		put_str("折れた刃は、新たに研がれ、", cy + 3, cx - 18);
		put_str("冠はまた主を求めることになろう。」", cy + 4, cx - 18);
		break;

	case ENDING_TYPE_LORD:
		switch (get_your_alignment_lnc())
		{
		case ALIGN_LNC_LAWFUL:
			align_str = "法治";
			break;
		case ALIGN_LNC_NEUTRAL:
			align_str = "共和";
			break;
		case ALIGN_LNC_CHAOTIC:
			align_str = "自由";
			break;
		}

		put_str(format("あなたは%s国家ヴァレリアの初代の君主となった。", align_str), cy - 1, cx - 25);
		put_str("以後、ヒッタイト人に統一されるまで、約千年もの間、", cy, cx - 25);
		put_str("ヴァレリアは独立国家として", cy + 1, cx - 25);
		put_str("その名を歴史に留めることになる……。", cy + 2, cx - 25);
		break;

	case ENDING_TYPE_OGRE:
		put_str("「我を祝え。我を讃えよ。」", cy - 3, cx - 27);
		put_str("「七日七晩火を絶やすな。我こそは真のオウガなり。」", cy - 2, cx - 27);
		put_str("「血を流せ。肉を捧げよ。我こそは世界の所有者である。」", cy - 1, cx - 27);

		put_str("この後、ヒッタイト人に統一されるまで、約千年もの間、", cy + 1, cx - 27);
		put_str("ヴァレリアの歴史をうかがい知ることができるものは、", cy + 2, cx - 27);
		put_str("何ひとつとして存在しない……。", cy + 3, cx - 27);
		break;
	}

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(hgt - 1);

	switch (ending_type)
	{
	case ENDING_TYPE_KILLED_BY_WALSTANIAN:
		(void)strcpy(p_ptr->died_from, "walstanian");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "戴冠式式場でウォルスタ人の暗殺者に暗殺された。");
		break;

	case ENDING_TYPE_KILLED_BY_GARGASTAN:
		(void)strcpy(p_ptr->died_from, "gargastan");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "戴冠式式場でガルガスタン人の暗殺者に暗殺された。");
		break;

	case ENDING_TYPE_KILLED_BY_BACRUM:
		(void)strcpy(p_ptr->died_from, "bacrum");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "戴冠式式場でバクラム人の暗殺者に暗殺された。");
		break;

	case ENDING_TYPE_KILLED_BY_Z_OR_L:
		(void)strcpy(p_ptr->died_from, "z_or_l");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "戴冠式式場で大陸からの暗殺者に暗殺された。");
		break;

	case ENDING_TYPE_KILLED_BY_VALERIA:
		(void)strcpy(p_ptr->died_from, "valeria");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "戴冠式式場で身元不明の暗殺者に暗殺された。");
		break;

	case ENDING_TYPE_LORD:
		(void)strcpy(p_ptr->died_from, "lord");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "戴冠式においてヴァレリアの君主の冠を授けられた。");
		break;

	case ENDING_TYPE_OGRE:
		(void)strcpy(p_ptr->died_from, "ogre");
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "真のオウガとなり、ヴァレリアに長く暗い冬の時代をもたらした。");
		break;
	}

#ifdef JP
	do_cmd_write_nikki(NIKKI_GAMESTART, 1, "-------- ゲームオーバー --------");
#else
	do_cmd_write_nikki(NIKKI_GAMESTART, 1, "--------   Game  Over   --------");
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 1, "\n\n\n\n");
}

/*
 * Change the player into a King!			-RAK-
 */
void kingly(void)
{
	int i;
	int wid, hgt;
	int cx, cy;

	/* Hack -- retire in town */
	dun_level = 0;

#ifdef JP
	/* Fake death */
	/* 引退したときの識別文字 */
	(void)strcpy(p_ptr->died_from, "ripe");
#else
	(void)strcpy(p_ptr->died_from, "Ripe Old Age");
#endif


	/* Restore the experience */
	p_ptr->exp = p_ptr->max_exp;

	/* Restore the level */
	p_ptr->lev = p_ptr->max_plv;

	for (i = 0; i < MAX_CLASS; i++)
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[i];

		/* Restore the class experience */
		cexp_ptr->cexp = cexp_ptr->max_cexp;

		/* Restore the class level */
		cexp_ptr->clev = cexp_ptr->max_clev;
	}

	/* Hack -- Instant Gold */
	p_ptr->au[SV_GOLD_GOLD_3] += 10000000L;
	p_ptr->update |= (PU_GOLD);
	update_stuff();

	Term_get_size(&wid, &hgt);
	cy = hgt / 2;
	cx = wid / 2;

	/* Clear screen */
	Term_clear();

	/* Display a crown */
	put_str("#", cy - 11, cx - 1);
	put_str("#####", cy - 10, cx - 3);
	put_str("#", cy - 9, cx - 1);
	put_str(",,,  $$$  ,,,", cy - 8, cx - 7);
	put_str(",,=$   \"$$$$$\"   $=,,", cy - 7, cx - 11);
	put_str(",$$        $$$        $$,", cy - 6, cx - 13);
	put_str("*>         <*>         <*", cy - 5, cx - 13);
	put_str("$$         $$$         $$", cy - 4, cx - 13);
	put_str("\"$$        $$$        $$\"", cy - 3, cx - 13);
	put_str("\"$$       $$$       $$\"", cy - 2, cx - 12);
	put_str("*#########*#########*", cy - 1, cx - 11);
	put_str("*#########*#########*", cy, cx - 11);

	/* Display a message */
#ifdef JP
	put_str("Veni, Vidi, Vici!", cy + 3, cx - 9);
	put_str("来た、見た、勝った！", cy + 4, cx - 10);
	put_str(format("偉大なる%s万歳！", sp_ptr->winner), cy + 5, cx - 11);
#else
	put_str("Veni, Vidi, Vici!", cy + 3, cx - 9);
	put_str("I came, I saw, I conquered!", cy + 4, cx - 14);
	put_str(format("All Hail the Mighty %s!", sp_ptr->winner), cy + 5, cx - 13);
#endif

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(hgt - 1);

	if (p_ptr->is_dead & DEATH_SNAP_DRAGON)
	{
		(void)strcpy(p_ptr->died_from, "snap");
#ifdef JP
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "勝利の後、スナップドラゴンを使い生きた武器に変化した。");
#else
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "changed into a living weapon after winning.");
#endif

#ifdef JP
		do_cmd_write_nikki(NIKKI_GAMESTART, 1, "-------- ゲームオーバー --------");
#else
		do_cmd_write_nikki(NIKKI_GAMESTART, 1, "--------   Game  Over   --------");
#endif
		do_cmd_write_nikki(NIKKI_BUNSHOU, 1, "\n\n\n\n");
	}
	else display_ending();
}

void survived_finish(void)
{
	int wid, hgt;
	int cx, cy;
	int i;

#ifdef JP
	/* 引退したときの識別文字 */
	(void)strcpy(p_ptr->died_from, "survive");
#else
	(void)strcpy(p_ptr->died_from, "Ripe Old Age (Retire after survive)");
#endif


	/* Restore the experience */
	p_ptr->exp = p_ptr->max_exp;

	/* Restore the level */
	p_ptr->lev = p_ptr->max_plv;

	for (i = 0; i < MAX_CLASS; i++)
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[i];

		/* Restore the class experience */
		cexp_ptr->cexp = cexp_ptr->max_cexp;

		/* Restore the class level */
		cexp_ptr->clev = cexp_ptr->max_clev;
	}

	/* Hack -- Break Runeweapon */
	for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		if (object_is_astral_runeweapon(o_ptr))
		{
			inven_item_increase(i, -255);
			inven_item_optimize(i);
		}
	}

	/* Handle stuff */
	handle_stuff();
	msg_print(NULL);

	Term_get_size(&wid, &hgt);
	cy = hgt / 2;
	cx = wid / 2;

	/* Clear screen */
	Term_clear();

	put_str("魔剣があなたの心に直接話しかけてくる。", cy - 3, cx - 38);
	put_str("「よくやった。私の血を引きし者よ。お前は私の押しつけた過酷な運命を撥ね退け、", cy - 2, cx - 38);
	put_str("私の過ちを正してくれた。これで私の役目は終わった。私の人生は長い旅だったが、", cy - 1, cx - 38);
	put_str("私はこれで、ようやく旅を終えることができる。ありがとう。強き者よ。お前の", cy, cx - 38);
	put_str("人生に幸多きことを祈ろう。」", cy + 1, cx - 38);

	put_str("そう言い終わると、魔剣はさらさらと崩れだし、やがて、消えた。", cy + 3, cx - 38);

#ifdef JP
	do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "死者の宮殿から生還した。");
#else
	do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "survived from Death Palace.");
#endif

#ifdef JP
	do_cmd_write_nikki(NIKKI_GAMESTART, 1, "-------- ゲームオーバー --------");
#else
	do_cmd_write_nikki(NIKKI_GAMESTART, 1, "--------   Game  Over   --------");
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 1, "\n\n\n\n");

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(hgt - 1);
}
