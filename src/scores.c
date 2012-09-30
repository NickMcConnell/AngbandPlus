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
 * The "highscore" file descriptor, if available.
 */
int highscore_fd = -1;


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
	return (MAX_HISCORES);
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
	for (i = slot; !done && (i <= MAX_HISCORES); i++)
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

		if (o_ptr->info & OB_NO_EXP) continue;
		if (!(o_ptr->info & OB_KNOWN)) continue;

		total += object_value(o_ptr);
	}

	/* Scan inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		if (o_ptr->info & OB_NO_EXP) continue;
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

	/* Not too much of a reward since some people like playing with this. */
	if (ironman_small_levels) mult += 5;

	/* More ironman options */
	if (ironman_nightmare) mult += 20;

	if (mult < 5) mult = 5;		/* At least 5% of the original score */

	temp = p_ptr->max_exp;

	temp = (temp * mult) / race_info[p_ptr->rp.prace].r_exp;

	temp += equip_value() / 10;

	if (ironman_downward) temp *= 2;

	return (temp);
}


/*
 * Hack - save index of player's high score
 */
static int score_idx = -1;

/* Hack - save from-to for resizing purposes */
static int score_from = -1;
static int score_to = -1;
static high_score *score_score = NULL;
static bool score_resize = FALSE;


/* find out how many score entries you can have on a page */
static int entries_on_page(void)
{
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* determine how many entries that is on a page */
	return (hgt / 4 - 1);
}


/* Find out what range is nice on the current page size */
static void determine_scores_page(int *from, int *to, int note)
{
	int entries;

	/* Determine how many entries that is on a page */
	entries = entries_on_page();

	/* If the note belongs on the first or second page */
	if (note <= 3 * entries / 2)
	{
		/* Show the second page */
		*from = entries;
		*to = 2 * entries;

		return;
	}

	/* A page with the score nicely in the middle */
	*from = note - entries / 2;
	*to = note + entries / 2;

	/* even entries lead to too large, odd range */
	if (!(entries % 2)) *to -= 1;

	/* If the range overshoots  the last page */
	if (*to >= MAX_HISCORES)
	{
		*from = MAX_HISCORES - entries;
		*to = MAX_HISCORES;

		/* This entry does not get saved, only displayed */
		if (note == MAX_HISCORES)
		{
			*from += 1;
			*to += 1;
		}
	}
}

/*
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
static bool display_scores_aux2(int from, int to, int note,
						 const high_score *score, bool no_wait)
{
	int i, j, k, n, place;
	byte attr;

	high_score the_score;

	char out_val[256];
	
	int len;
	int entries;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (FALSE);

	/* Seek to the beginning */
	if (highscore_seek(0)) return (FALSE);

	/* Determine how many entries that is on a page */
	entries = entries_on_page();

	/* Dimensions of the first page */
	if (to < entries)
	{
		from = 0;
		to = entries;
	}

	/* Remember ending of the list */
	score_to = to;

	/* Hack -- Count the high scores */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score)) break;
	}

	/* Hack -- allow "fake" entry to be last */
	if (note == i) i++;

	/* Forget about the last entries */
	if (i > to) i = to;

	/* Show 'entries' per page, until "done" */
	for (k = from, place = k + 1; k < i; k += entries)
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

		/* Dump entries */
		for (j = k, n = 0; j < i && n < entries; place++, j++, n++)
		{
			int pr, pc, clev, mlev, cdun, mdun;

			cptr user, gold, when, aged;

			/* Remember the current starting point */
			score_from = k;

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
		prtf(15, entries * 4 + 3, "[Press ESC to quit, any other key to continue.]");

		/* No keystrokes needed during resizing */
		if (no_wait) return (TRUE);

		j = inkey();
		clear_row(entries * 4 + 3);

		/* Check for resize, entries may have changed */
		if (score_resize)
		{
			/* set back to default */
			score_resize = FALSE;

			/* Not all of the previous range was shown yet */
			if (score_to < to)
			{
				/* Show rest of the range */
				return (display_scores_aux2(score_from + entries_on_page(),
											to,
											score_idx,
											score_score,
											FALSE));
			}
		}

		/* Hack -- notice Escape */
		if (j == ESCAPE) return (FALSE);
	}

	return (TRUE);
}

/* Make sense of the display range and redraw the screen */
static void resize_scores(void)
{
	int from, to;

	/* Alert the public */
	score_resize = TRUE;

	/* Displaying the whole list? */
	if (score_idx == -1 || score_to < score_idx)
	{
		/* Display the list */
		(void)display_scores_aux2(score_from, score_from + entries_on_page(),
								  -1, NULL, TRUE);
	}
	else
	/* So this page has score_idx at its center */
	{
		/* Make new range for the new page */
		determine_scores_page(&from, &to, score_idx);

		/* Display the list */
		(void)display_scores_aux2(from, to, score_idx, score_score, TRUE);
	}
}


bool display_scores_aux(int from, int to, int note, const high_score *score)
{
	bool outcome;
	void (*hook) (void);

	/* Remember the old hook */
	hook = angband_term[0]->resize_hook
		;
	/* set the resize hook to scores */
	angband_term[0]->resize_hook = resize_scores;

	/* Display the scores */
	outcome = display_scores_aux2(from, to, note, score, FALSE);

	/* Restore the old resize hook */
	angband_term[0]->resize_hook = hook;

	/* The size may have changed during the scores display */
	angband_term[0]->resize_hook();

	/* Hack - Flush it */
	Term_fresh();

	/* Allow another call depending on the outcome of this call */
	return (outcome);
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
	path_make(buf, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
	if (highscore_fd < 0) quit("Score file unavailable.");

	/* Clear screen */
	Term_clear();

	/* Display the scores */
	(void)display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Quit */
	quit(NULL);
}


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
	strnfmt(the_score.max_dun, 4, "%3d", max_dun_level_reached());

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
static void top_twenty(void)
{
	int from, to;
	bool cont;

	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msgf("Score file unavailable.");
		message_flush();
		return;
	}

	/* Show the first page of the highscore */
	cont = display_scores_aux(0, 5, score_idx, NULL);

	/* If the user didn't press ESC, show the second page too */
	if (cont)
	{
		/* Determine what the second page will be */
		determine_scores_page(&from, &to, score_idx);

		/* Show the second page */
		(void)display_scores_aux(from, to, score_idx, NULL);
	}

	/* Success */
	return;
}


/*
 * Predict the players location, and display it.
 */
void predict_score(void)
{
	int j, from, to;
	bool cont;

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
	strnfmt(the_score.max_dun, 4, "%3d", max_dun_level_reached());

	/* Hack -- no cause of death */
	strcpy(the_score.how, "nobody (yet!)");


	/* See where the entry would be placed */
	j = highscore_where(&the_score);

	/* Keep it for resizing */
	score_idx = j;
	score_score = &the_score;

	/* Show the first page of the highscore */
	cont = display_scores_aux(0, 5, score_idx, &the_score);

	/* If the user didn't press ESC, show the second page too */
	if (cont)
	{
		/* Determine what the second page will be */
		determine_scores_page(&from, &to, score_idx);

		/* Show the second page */
		(void)display_scores_aux(from, to, score_idx, &the_score);
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
	path_make(buf, ANGBAND_DIR_APEX, "scores.raw");

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
	path_make(buf, ANGBAND_DIR_APEX, "scores.raw");

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
static void kingly(void)
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

void ingame_score(bool *initialized, bool game_in_progress)
{
	char buf[1024];
	
	/* Build the filename */
	path_make(buf, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
	if (highscore_fd < 0)
	{
		msgf("Score file unavailable.");
	}
	else
	{
		/* Prevent various functions */
		*initialized = FALSE;

		/* Display the scores */
		if (game_in_progress && character_generated)
		{
			/* Show a selection of the score list */
			predict_score();
		}
		else
		{
			/* Save Screen */
			screen_save();

			/* Show all the scores */
			(void)display_scores_aux(0, MAX_HISCORES, -1, NULL);

			/* Load screen */
			screen_load();

			/* Hack - Flush it */
			Term_fresh();
		}

		/* Shut the high score file */
		(void)fd_close(highscore_fd);

		/* Forget the high score fd */
		highscore_fd = -1;

		/* We are ready again */
		*initialized = TRUE;
	}
}


/*
 * Display a "tomb-stone"
 */
static void print_tomb(void)
{
	bool done = FALSE;

	/* Print the text-tombstone */
	if (!done)
	{
		char buf[1024];

		FILE *fp;

		time_t ct = time((time_t) 0);

		/* Clear screen */
		Term_clear();

		/* Build the filename */
		path_make(buf, ANGBAND_DIR_FILE, "dead.txt");

		/* Open the News file */
		fp = my_fopen(buf, "r");

		/* Dump */
		if (fp)
		{
			int i = 0;

			/* Dump the file to the screen */
			while (0 == my_fgets(fp, buf, 1024))
			{
				/* Display and advance */
				put_fstr(0, i++, buf);
			}

			/* Close */
			my_fclose(fp);
		}


		put_fstr(11, 6, "%v", center_string, 31, player_name);

		put_fstr(11, 7, "%v", center_string, 31, "the");

		
		/* King or Queen */
		if (p_ptr->state.total_winner || (p_ptr->lev > PY_MAX_LEVEL))
		{
			put_fstr(11, 8, "%v", center_string, 31, "Magnificent");
		}

		/* Normal */
		else
		{
			put_fstr(11, 8, "%v", center_string, 31,
					 player_title[p_ptr->rp.pclass][(p_ptr->lev - 1) / 5]);
		}

		put_fstr(11, 10, "%v", center_string, 31, cp_ptr->title);

		put_fstr(11, 11, "%v", center_string, 31, "Level: %d", (int)p_ptr->lev);

		put_fstr(11, 12, "%v", center_string, 31, "Exp: %ld", (long)p_ptr->exp);

		put_fstr(11, 13, "%v", center_string, 31, "AU: %ld", (long)p_ptr->au);

		put_fstr(11, 14, "%v", center_string, 31, "Killed on Level %d", p_ptr->depth);


		if (strlen(p_ptr->state.died_from) > 24)
		{
			put_fstr(11, 15, "%v", center_string, 31, "by %.24s.", p_ptr->state.died_from);
		}
		else
		{
			put_fstr(11, 15, "%v", center_string, 31, "by %s.", p_ptr->state.died_from);
		}

		put_fstr(11, 17, "%v", center_string, 31, "%-.24s", ctime(&ct));
	}
}


/*
 * Display some character info
 */
static void show_info(void)
{
	int i, j, l;
	object_type *o_ptr;
	store_type *st_ptr;

	/* Hack -- Know everything in the equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags[0] = o_ptr->flags[0];
		o_ptr->kn_flags[1] = o_ptr->flags[1];
		o_ptr->kn_flags[2] = o_ptr->flags[2];
		o_ptr->kn_flags[3] = o_ptr->flags[3];
	}

	/* Hack -- Know everything in the inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags[0] = o_ptr->flags[0];
		o_ptr->kn_flags[1] = o_ptr->flags[1];
		o_ptr->kn_flags[2] = o_ptr->flags[2];
		o_ptr->kn_flags[3] = o_ptr->flags[3];
	}
	OBJ_ITT_END;

	for (i = 1; i < z_info->wp_max; i++)
	{
		for (j = 0; j < place[i].numstores; j++)
		{
			st_ptr = &place[i].store[j];

			if (st_ptr->type == BUILD_STORE_HOME)
			{
				/* Hack -- Know everything in the home */
				OBJ_ITT_START (st_ptr->stock, o_ptr)
				{
					/* Aware and Known */
					object_aware(o_ptr);
					object_known(o_ptr);
					object_mental(o_ptr);

					/* Save all the known flags */
					o_ptr->kn_flags[0] = o_ptr->flags[0];
					o_ptr->kn_flags[1] = o_ptr->flags[1];
					o_ptr->kn_flags[2] = o_ptr->flags[2];
					o_ptr->kn_flags[3] = o_ptr->flags[3];
				}
				OBJ_ITT_END;
			}
		}
	}

	/* Hack -- Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Display player */
	display_player(DISPLAY_PLAYER_STANDARD);

	/* Prompt for inventory */
	prtf(0, 23, "Hit any key to see more information (ESC to abort): ");

	/* Flush keys */
	flush();

	/* Allow abort at this point */
	if (inkey() == ESCAPE) return;


	/* Show equipment */
	Term_clear();

	/* Equipment -- if any */
	item_tester_full = TRUE;
	show_equip(FALSE);

	prtf(0, 0, "You are using: -more-");

	/* Flush keys */
	flush();

	if (inkey() == ESCAPE) return;


	/* Show inventory */
	Term_clear();

	/* Inventory -- if any */
	item_tester_full = TRUE;
	show_list(p_ptr->inventory, FALSE);

	prtf(0, 0, "You are carrying: -more-");

	/* Flush keys */
	flush();

	if (inkey() == ESCAPE) return;

	for (i = 1; i < z_info->wp_max; i++)
	{
		for (l = 0; l < place[i].numstores; l++)
		{
			st_ptr = &place[i].store[l];

			if (st_ptr->type == BUILD_STORE_HOME)
			{
				/* Home -- if anything there */
				if (st_ptr->stock)
				{
					/* Initialise counter */
					j = 0;

					/* Clear screen */
					Term_clear();

					/* Display contents of the home */
					OBJ_ITT_START (st_ptr->stock, o_ptr)
					{
						/* Print header, clear line */
						prtf(4, j + 2, "%c) %s%v", I2A(j),
							 color_seq[tval_to_attr[o_ptr->tval]],
							 OBJECT_FMT(o_ptr, TRUE, 3));

						/* Show 12 items at a time */
						if (j == 12)
						{
							/* Caption */
							prtf(0, 0, "Your home in %s: -more-",
								 place[i].name);

							/* Flush keys */
							flush();

							/* Wait for it */
							if (inkey() == ESCAPE) return;

							/* Restart counter */
							j = 0;

							/* Clear screen */
							Term_clear();
						}
					}
					OBJ_ITT_END;
				}
			}
		}
	}
}



static void close_game_handle_death(void)
{
	char ch;

	/* Handle retirement */
	if (p_ptr->state.total_winner)
	{
		/* Save winning message to notes file. */
		if (take_notes)
		{
			add_note_type(NOTE_WINNER);
		}

		kingly();
	}

	/* Save memories */
	if (!munchkin_death || get_check("Save death? "))
	{
		if (!save_player()) msgf("death save failed!");
	}

#if 0
	/* Dump bones file */
	make_bones();
#endif

	/* Inform notes file that you are dead */
	if (take_notes)
	{
		char long_day[30];
		time_t ct = time((time_t *) NULL);

		/* Get the date */
		(void)strftime(long_day, 30, "%Y-%m-%d at %H:%M:%S", localtime(&ct));

		/* Output to the notes file */
		output_note("\n%s was killed by %s on %s\n", player_name,
				p_ptr->state.died_from, long_day);
	}

	/* Enter player in high score list */
	enter_score();

	/* You are dead */
	print_tomb();

	/* Describe options */
	prtf(0, 23, "(D) Dump char record  (C) Show char info  (T) Show top scores  (ESC) Exit");

	/* Flush messages */
	message_flush();

	/* Flush all input keys */
	flush();

	/* Player selection */
	while (TRUE)
	{
		/* Save screen */
		/* Note that Term_save() and Term_load() must match in pairs */
		Term_save();

		/* Flush all input keys */
		flush();

		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				/* Flush the keys */
				flush();

				if (get_check("Do you really want to exit? "))
				{
					/* Save dead player */
					if (!save_player())
					{
						msgf("Death save failed!");
						message_flush();
					}

#if 0
					/* Dump bones file */
					make_bones();
#endif

					/* XXX We now have an unmatched Term_save() */
					Term_load();

					/* Go home, we're done */
					return;
				}
				else
				{
					break;
				}
			}

			case 'd':
			case 'D':
			{
				/* Dump char file */
				char tmp[160] = "";

				/* Clear this line first */
				clear_row(23);

				/* Prompt */
				put_fstr(0, 23, "Filename: ");

				/* Ask for filename (or abort) */
				if (!askfor_aux(tmp, 60)) break;

				/* Ignore Return */
				if (!tmp[0]) break;

				/* Dump a character file */
				(void)file_character(tmp, FALSE);

				break;
			}

			case 'c':
			case 'C':
			{
				/* Remove options line, so we don't dump it */
				clear_row(23);

				/* Show char info */
				show_info();
				break;
			}

			case 't':
			case 'T':
			{
				/* Show top twenty */
				top_twenty();
				break;
			}
		}

		/* Restore the screen */
		Term_load();
	}
}


/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c" and "signals.c".
 */
void close_game(void)
{
	char buf[1024];

	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	message_flush();

	/* Flush the input */
	flush();

	/* No suspending now */
	signals_ignore_tstp();

	/* Hack -- Character is now "icky" */
	screen_save();

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_APEX, "scores.raw");

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);

	/* Drop permissions */
	safe_setuid_drop();

	if (p_ptr->state.is_dead)
	{
		/* Handle death */
		close_game_handle_death();
	}

	/* Still alive */
	else
	{
		int wid, hgt;
	
		/* Save the game */
		do_cmd_save_game(FALSE);

		/* If note-taking enabled, write session end to notes file */
		if (take_notes)
		{
			add_note_type(NOTE_SAVE_GAME);
		}
		
		/* Get size */
		Term_get_size(&wid, &hgt);

		/* Prompt for scores XXX XXX XXX */
		prtf(0, hgt - 1, "Press Return (or Escape).");

		/* Predict score (or ESCAPE) */
		if (inkey() != ESCAPE) predict_score();
	}


	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;
	
	/* No longer icky */
	screen_load();

	/* Allow suspending now */
	signals_handle_tstp();
}


/*
 * Handle abrupt death of the visual system
 *
 * This routine is called only in very rare situations, and only
 * by certain visual systems, when they experience fatal errors.
 *
 * XXX XXX Hack -- clear the death flag when creating a HANGUP
 * save file so that player can see tombstone when restart.
 */
void exit_game_panic(void)
{
	/* If nothing important has happened, just quit */
	if (!character_generated || character_saved) quit("panic");

	/* Mega-Hack -- see "msgf()" */
	msg_flag = FALSE;

	/* Clear the top line */
	clear_msg();

	/* Hack -- turn off some things */
	disturb(TRUE);

	/* Mega-Hack -- Delay death */
	if (p_ptr->chp < 0) p_ptr->state.is_dead = FALSE;

	/* Hardcode panic save */
	p_ptr->state.panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	(void)strcpy(p_ptr->state.died_from, "(panic save)");

	/* Panic save, or get worried */
	if (!save_player()) quit("panic save failed!");

	/* Successful panic save */
	quit("panic save succeeded!");
}
