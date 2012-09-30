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
	return (fd_seek(highscore_fd, (huge) (i) * sizeof(high_score)));
}


/*
 * Read one score from the highscore file
 */
static errr highscore_read(high_score *score)
{
	/* Read the record, note failure */
	return (fd_read(highscore_fd, (char *)(score), sizeof(high_score)));
}


/*
 * Write one score to the highscore file
 */
static int highscore_write(const high_score *score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (char *)(score), sizeof(high_score)));
}


/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(const high_score *score)
{
	int i;

	high_score the_score;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (-1);

	/* Go to the start of the highscore file */
	if (highscore_seek(0)) return (-1);

	/* Read until we get to a higher score */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score)) return (i);
		if (strcmp(the_score.pts, score->pts) < 0) return (i);
	}

	/* The "last" entry is always usable */
	return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(const high_score *score)
{
	int i, slot;
	bool done = FALSE;

	high_score the_score, tmpscore;


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
 * How much valuable stuff do we carry. Returns total cost of all found
 * items currently in equipment (ie. those that haven't been storebought
 * or started with)
 */
static long equip_value(void)
{
	object_type *o_ptr;
	long total = 0L;
	int i;

	/* Scan equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		if (o_ptr->info & OB_STOREB) continue;
		if (!(o_ptr->info & OB_KNOWN)) continue;

		total += object_value(o_ptr);
	}

	/* Scan inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		if (o_ptr->info & OB_STOREB) continue;
		if (!(o_ptr->info & OB_KNOWN)) continue;

		total += object_value(o_ptr);
	}
	OBJ_ITT_END;

	return (total);
}


/*
 * Hack -- Calculates the total number of points earned         -JWT-
 * Now with up to 80% penalty for having mutations & other extra things  -GSN-
 * Fixed this up to be "fairer" -CK-
 */
static long total_points(void)
{
	long temp;
	long mult = 85;

	/* AI is not that big a deal (yet) */
	if (stupid_monsters) mult -= 20;

	/* Penalize preserve mode */
	if (preserve_mode) mult -= 10;

	/* Vanilla town is harder than normal */
	if (vanilla_town) mult += 5;

	/* so are hard quests */
	if (ironman_hard_quests) mult += number_of_quests() / 2;
	if (ironman_deep_quests) mult += number_of_quests();

	/* Not too much of a reward since some people like playing with this. */
	if (ironman_small_levels) mult += 5;

	/* More ironman options */
	if (ironman_empty_levels) mult += 10;
	if (ironman_nightmare) mult += 20;

	if (mult < 5) mult = 5;		/* At least 5% of the original score */

	temp = p_ptr->max_exp + (100 * p_ptr->max_depth);

	temp = (temp * mult) / race_info[p_ptr->rp.prace].r_exp;

	temp += equip_value() / 10;

	if (ironman_downward) temp *= 2;

	return (temp);
}


/*
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 * Only five entries per line, too much info.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
void display_scores_aux(int from, int to, int note, const high_score *score)
{
	int i, j, k, n, place;
	byte attr;

	high_score the_score;

	char out_val[256];
	
	int len;

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


	/* Show 5 per page, until "done" */
	for (k = from, place = k + 1; k < i; k += 5)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
		put_fstr(0, 0, "                %s Hall of Fame", VERSION_NAME);

		/* Indicate non-top scores */
		if (k > 0)
		{
			put_fstr(40, 0, "(from position %d)", k + 1);
		}

		/* Dump 5 entries */
		for (j = k, n = 0; j < i && n < 5; place++, j++, n++)
		{
			int pr, pc, clev, mlev, cdun, mdun;

			cptr user, gold, when, aged;


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

			/* Hack -- extract the gold and such */
			for (user = the_score.uid; isspace(*user); user++) /* loop */ ;
			for (when = the_score.day; isspace(*when); when++) /* loop */ ;
			for (gold = the_score.gold; isspace(*gold); gold++) /* loop */ ;
			for (aged = the_score.turns; isspace(*aged); aged++) /* loop */ ;

			/* Dump some info */
			len = strnfmt(out_val, 256, "%3d.%9s  %s the %s %s, Level %d",
					place, the_score.pts, the_score.who,
					race_info[pr].title, class_info[pc].title, clev);

			/* Append a "maximum level" */
			if (mlev > clev) strnfcat(out_val, 256, &len, " (Max %d)", mlev);

			/* Dump the first line */
			put_fstr(0, n * 4 + 2, "%s%s", color_seq[attr], out_val);

			/* Some people die outside of the dungeon */
			if (cdun)
			{
				/* Another line of info */
				len = strnfmt(out_val, 256, "               Killed by %s on %s %d",
						the_score.how, "Dungeon Level", cdun);

			}
			else
			{
				/* Died outside */
				len = strnfmt(out_val, 256,
						"               Killed by %s in the outside world.",
						the_score.how);
			}

			/* Append a "maximum level" */
			if (mdun > cdun) strnfcat(out_val, 256, &len, " (Max %d)", mdun);

			/* Dump the info */
			put_fstr(0, n * 4 + 3, "%s%s", color_seq[attr], out_val);

			/* And still another line of info */
			put_fstr(0, n * 4 + 4,
				"%s               (User %s, Date %s, Gold %s, Turn %s).",
					 color_seq[attr], user, when, gold, aged);
		}


		/* Wait for response */
		prtf(17, 23, "[Press ESC to quit, any other key to continue.]");
		j = inkey();
		clear_row(23);

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
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
	if (highscore_fd < 0) quit("Score file unavailable.");

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
 * Hack - save index of player's high score
 */
static int score_idx = -1;


/*
 * Enters a players name on a hi-score table, if "legal".
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
void enter_score(void)
{
	int i;
	high_score the_score;

#ifndef HIGHSCORE_DATE_HACK
	char long_day[12];
#endif

	time_t ct = time((time_t *) 0);

	/* No score file */
	if (highscore_fd < 0)
	{
		return;
	}

#ifndef SCORE_WIZARDS
	/* Wizard-mode pre-empts scoring */
	if (p_ptr->state.noscore & 0x003F)
	{
		msgf("Score not registered for wizards.");
		message_flush();
		score_idx = -1;
		return;
	}
#endif

#ifndef SCORE_BORGS
	/* Borg-mode pre-empts scoring */
	if (p_ptr->state.noscore & 0x00C0)
	{
		msgf("Score not registered for borgs.");
		message_flush();
		score_idx = -1;
		return;
	}
#endif

#ifndef SCORE_CHEATERS
	/* Cheaters are not scored */
	if (p_ptr->state.noscore & 0xFF00)
	{
		msgf("Score not registered for cheaters.");
		message_flush();
		score_idx = -1;
		return;
	}
#endif

	/* Interupted */
	if (!p_ptr->state.total_winner && streq(p_ptr->state.died_from, "Interrupting"))
	{
		msgf("Score not registered due to interruption.");
		message_flush();
		score_idx = -1;
		return;
	}

	/* Quitter */
	if (!p_ptr->state.total_winner && streq(p_ptr->state.died_from, "Quitting"))
	{
		msgf("Score not registered due to quitting.");
		message_flush();
		score_idx = -1;
		return;
	}


	/* Clear the record */
	(void)WIPE(&the_score, high_score);

	/* Save the version */
	strnfmt(the_score.what, 8, "%u.%u.%u",
			VER_MAJOR, VER_MINOR, VER_PATCH);

	/* Calculate and save the points */
	strnfmt(the_score.pts, 10, "%9lu", (unsigned long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	strnfmt(the_score.gold, 10, "%9lu", (unsigned long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	strnfmt(the_score.turns, 10, "%9lu", (unsigned long)turn);
	the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
	/* Save the date in a hacked up form (9 chars) */
	(void)strnfmt(the_score.day, 10, "%-.6s %-.2s", ctime(&ct) + 4,
				  ctime(&ct) + 22);
#else  /* HIGHSCORE_DATE_HACK */
	/* Get the date with a 4-digit year */
	(void)strftime(long_day, 11, "%m/%d/%Y", localtime(&ct));

	/* Remove the century */
	i = 7;
	while (1)
	{
		i++;
		long_day[i - 2] = long_day[i];

		/* Exit if get a zero */
		if (!long_day[i] || (i == 11)) break;
	}

	/* Save the date in standard form (8 chars) */
	(void)strnfmt(the_score.day, 9, "%s", long_day);
#endif /* HIGHSCORE_DATE_HACK */

	/* Save the player name (15 chars) */
	strnfmt(the_score.who, 16, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	strnfmt(the_score.uid, 8, "%7u", (uint)player_uid);
	strnfmt(the_score.sex, 2, "%c", (p_ptr->rp.psex ? 'm' : 'f'));
	strnfmt(the_score.p_r, 3, "%2d", (int)p_ptr->rp.prace);
	strnfmt(the_score.p_c, 3, "%2d", (int)p_ptr->rp.pclass);

	/* Save the level and such */
	strnfmt(the_score.cur_lev, 4, "%3d", p_ptr->lev);
	strnfmt(the_score.cur_dun, 4, "%3d", p_ptr->depth);
	strnfmt(the_score.max_lev, 4, "%3d", p_ptr->max_lev);
	strnfmt(the_score.max_dun, 4, "%3d", p_ptr->max_depth);

	/* Save the cause of death (31 chars) */
	strnfmt(the_score.how, 32, "%-.31s", p_ptr->state.died_from);

	/* Grab permissions */
	safe_setuid_grab();

	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return;

	/* Drop permissions */
	safe_setuid_drop();

	/* Add a new entry to the score list, see where it went */
	score_idx = highscore_add(&the_score);

	/* Grab permissions */
	safe_setuid_grab();

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return;

	/* Drop permissions */
	safe_setuid_drop();

	/* Success */
	return;
}



/*
 * Displays some relevant portion of the high score list.
 */
void top_twenty(void)
{
	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msgf("Score file unavailable.");
		message_flush();
		return;
	}

	/* Hack -- Display the top fifteen scores */
	if (score_idx < 10)
	{
		display_scores_aux(0, 15, score_idx, NULL);
	}

	/* Display the scores surrounding the player */
	else
	{
		display_scores_aux(0, 5, score_idx, NULL);
		display_scores_aux(score_idx - 2, score_idx + 7, score_idx, NULL);
	}

	/* Success */
	return;
}


/*
 * Predict the players location, and display it.
 */
void predict_score(void)
{
	int j;

	high_score the_score;

	/* No score file */
	if (highscore_fd < 0)
	{
		msgf("Score file unavailable.");
		message_flush();
		return;
	}


	/* Save the version */
	strnfmt(the_score.what, 8, "%u.%u.%u",
			VER_MAJOR, VER_MINOR, VER_PATCH);

	/* Calculate and save the points */
	strnfmt(the_score.pts, 10, "%9lu", (unsigned long)total_points());

	/* Save the current gold */
	strnfmt(the_score.gold, 10, "%9lu", (unsigned long)p_ptr->au);

	/* Save the current turn */
	strnfmt(the_score.turns, 10, "%9lu", (unsigned long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (15 chars) */
	strnfmt(the_score.who, 16, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	strnfmt(the_score.uid, 8, "%7u", (uint)player_uid);
	strnfmt(the_score.sex, 2, "%c", (p_ptr->rp.psex ? 'm' : 'f'));
	strnfmt(the_score.p_r, 3, "%2d", (int)p_ptr->rp.prace);
	strnfmt(the_score.p_c, 3, "%2d", (int)p_ptr->rp.pclass);

	/* Save the level and such */
	strnfmt(the_score.cur_lev, 4, "%3d", p_ptr->lev);
	strnfmt(the_score.cur_dun, 4, "%3d", p_ptr->depth);
	strnfmt(the_score.max_lev, 4, "%3d", p_ptr->max_lev);
	strnfmt(the_score.max_dun, 4, "%3d", p_ptr->max_depth);

	/* Hack -- no cause of death */
	strcpy(the_score.how, "nobody (yet!)");


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
}



/*
 * Selectively list highscores based on class -KMW-
 */
void show_highclass(void)
{
	int i = 0, j, m = 0;
	int pr, pc, clev /*, al */ ;
	high_score the_score;
	char buf[1024];

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msgf("Score file unavailable.");
		message_flush();
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++)
		if (highscore_read(&the_score)) break;

	m = 0;
	j = 0;
	clev = 0;

	while ((m < 9) || (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		pc = atoi(the_score.p_c);
		clev = atoi(the_score.cur_lev);

		prtf(0, m + 7, "%3d) %s the %s (Level %2d)",
				(m + 1), the_score.who, race_info[pr].title, clev);
		m++;
		j++;
	}

	prtf(0, m + 8, "You) %s the %s (Level %2d)",
			player_name, race_info[p_ptr->rp.prace].title, p_ptr->lev);

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
	msgf("Hit any key to continue");
	message_flush();

	clear_region(0, 5, 17);
}


/*
 * Race Legends
 * -KMW-
 */
void race_score(int race_num)
{
	int i = 0, j, m = 0;
	int pr, pc, clev, lastlev;
	high_score the_score;
	char buf[1024];

	lastlev = 0;

	/* rr9: TODO - pluralize the race */
	prtf(15, 5, "The Greatest of all the %s", race_info[race_num].title);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msgf("Score file unavailable.");
		message_flush();
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
		pc = atoi(the_score.p_c);
		clev = atoi(the_score.cur_lev);

		if (pr == race_num)
		{
			prtf(0, m + 7, "%3d) %s the %s (Level %3d)",
					(m + 1), the_score.who, race_info[pr].title, clev);
			m++;
			lastlev = clev;
		}
		j++;
	}

	/* add player if qualified */
	if ((p_ptr->rp.prace == race_num) && (p_ptr->lev >= lastlev))
	{
		prtf(0, m + 8, "You) %s the %s (Level %3d)",
				player_name, race_info[p_ptr->rp.prace].title, p_ptr->lev);
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
	int i;

	for (i = 0; i < MAX_RACES; i++)
	{
		race_score(i);
		msgf("Hit any key to continue");
		message_flush();
        clear_region(0, 5, 18);
	}
}


/*
 * Change the player into a King!			-RAK-
 */
void kingly(void)
{
	/* Hack -- retire in town */
	p_ptr->depth = 0;

	/* Fake death */
	(void)strcpy(p_ptr->state.died_from, "Ripe Old Age");

	/* Restore the experience */
	p_ptr->exp = p_ptr->max_exp;

	/* Restore the level */
	p_ptr->lev = p_ptr->max_lev;

	/* Hack -- Instant Gold */
	p_ptr->au += 10000000L;

	/* Clear screen */
	Term_clear();

	/* Display a crown */
	put_fstr(22, 1, "             %%%\n"
					"             %@%\n"
					"     ___      %      ___\n"
					"  _=$$###\\   #####	 /###$$=_\n"
					" $$_______##$$_____$$##_______$$\n"
					"(**   **   **   **   **   **)\n"
					"(**   **   **   **   **   **)\n"
					" TTTTTTTTTTTTTTTTTTTTTTTTTTT\n"
					" \\. $$$$$$ .\\.  $$$$$$  ./. $$$$$$ ./\n"
					"  \\  *    |   *   |    *  /\n"
					"   \\.    .|.     .|.    ./\n"
					"    ##H##H##H###H##H##H##\n"
					"    ##H##H##H###H##H##H##\n");

	/* Display a message */
	put_fstr(26, 15, "Veni, Vidi, Vici!");
	put_fstr(21, 16, "I came, I saw, I conquered!");
	put_fstr(22, 17, "All Hail the Mighty %s!", sp_ptr->winner);

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(23);
}
