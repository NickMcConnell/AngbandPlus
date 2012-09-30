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
static int highscore_write(const high_score *score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (char*)(score), sizeof(high_score)));
}


/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(const high_score *score)
{
	int			i;

	high_score		the_score;

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
 * How much valuable stuff do we carry. Returns total cost of all found
 * items currently in equipment (ie. those that haven't been storebought
 * or started with)
 */
static long equip_value(void)
{
	object_type *o_ptr;
	long total = 0L;
	int i;

	for (i = 1; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		if (o_ptr->ident & IDENT_STOREB) continue;
		if (!(o_ptr->ident & IDENT_KNOWN)) continue;

		total += object_value(o_ptr);
	}

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
	long mult = 100;
	
	/* AI is not that big a deal (yet) */
	if (stupid_monsters) mult -= 20;
	
	/* Penalize preserve, maximize modes */
	if (preserve_mode) mult -= 10;
	if (maximize_mode) mult -= 15;
	
	/* Vanilla town is harder than normal */
	if (vanilla_town) mult += 5;
	
	/* so are hard quests */
	if (ironman_hard_quests) mult += 10;

	/* Not too much of a reward since some people like playing with this. */
	if (ironman_small_levels) mult += 5;

	/* Moria mode is very hard */
	if (ironman_moria) mult += 20;

	/* More ironman options */
	if (ironman_empty_levels) mult += 10;
	if (ironman_nightmare) mult += 20;
	if (ironman_rooms) mult +=10;

	if (mult < 5) mult = 5; /* At least 5% of the original score */

	temp = p_ptr->max_exp + (100 * p_ptr->max_depth);

	temp = (temp * mult) / race_info[p_ptr->prace].r_exp;

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
	int		i, j, k, n, place;
	byte attr;

	high_score	the_score;

	char	out_val[256];
	char	tmp_val[160];


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
	for (k = from, place = k+1; k < i; k += 5)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
		put_str("                Zangband Hall of Fame", 0, 0);

		/* Indicate non-top scores */
		if (k > 0)
		{
			sprintf(tmp_val, "(from position %d)", k + 1);
			put_str(tmp_val, 0, 40);
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
			for (user = the_score.uid; isspace(*user); user++) /* loop */;
			for (when = the_score.day; isspace(*when); when++) /* loop */;
			for (gold = the_score.gold; isspace(*gold); gold++) /* loop */;
			for (aged = the_score.turns; isspace(*aged); aged++) /* loop */;

			/* Dump some info */
			sprintf(out_val, "%3d.%9s  %s the %s %s, Level %d",
			        place, the_score.pts, the_score.who,
			        race_info[pr].title, class_info[pc].title,
			        clev);

			/* Append a "maximum level" */
			if (mlev > clev) strcat(out_val, format(" (Max %d)", mlev));

			/* Dump the first line */
			c_put_str(attr, out_val, n*4 + 2, 0);

			/* Another line of info */
			sprintf(out_val, "               Killed by %s on %s %d",
			        the_score.how, "Dungeon Level", cdun);


			/* Some people die outside of the dungeon */
			if (!cdun)
			{
				/* Died in town */
				if (p_ptr->town_num)
					sprintf(out_val, "               Killed by %s in the Town",
					        the_score.how);
				/* Died in the wilderness */
				else
					sprintf(out_val, "               Killed by %s in the Wilderness",
					        the_score.how);
			}

			/* Append a "maximum level" */
			if (mdun > cdun) strcat(out_val, format(" (Max %d)", mdun));

			/* Dump the info */
			c_put_str(attr, out_val, n*4 + 3, 0);

			/* And still another line of info */
			sprintf(out_val,
			        "               (User %s, Date %s, Gold %s, Turn %s).",
			        user, when, gold, aged);
			c_put_str(attr, out_val, n*4 + 4, 0);
		}


		/* Wait for response */
		prt("[Press ESC to quit, any other key to continue.]", 23, 17);
		j = inkey();
		prt("", 23, 0);

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
errr enter_score(void)
{
	int i;
	high_score   the_score;

#ifndef HIGHSCORE_DATE_HACK
	char long_day[12];
#endif

	time_t ct = time((time_t*)0);

	/* No score file */
	if (highscore_fd < 0)
	{
		return (0);
	}

#ifndef SCORE_WIZARDS
	/* Wizard-mode pre-empts scoring */
	if (p_ptr->noscore & 0x000F)
	{
		msg_print("Score not registered for wizards.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}
#endif

#ifndef SCORE_BORGS
	/* Borg-mode pre-empts scoring */
	if (p_ptr->noscore & 0x00F0)
	{
		msg_print("Score not registered for borgs.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}
#endif

#ifndef SCORE_CHEATERS
	/* Cheaters are not scored */
	if (p_ptr->noscore & 0xFF00)
	{
		msg_print("Score not registered for cheaters.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}
#endif

	/* Interupted */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Interrupting"))
	{
		msg_print("Score not registered due to interruption.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}

	/* Quitter */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Quitting"))
	{
		msg_print("Score not registered due to quitting.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}


	/* Clear the record */
	(void)WIPE(&the_score, high_score);

	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
	        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);
	the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
	/* Save the date in a hacked up form (9 chars) */
	(void)sprintf(the_score.day, "%-.6s %-.2s", ctime(&ct) + 4, ctime(&ct) + 22);
#else /* HIGHSCORE_DATE_HACK */
	/* Get the date with a 4-digit year */
	(void)strftime(long_day, 11, "%m/%d/%Y", localtime(&ct));

	/* Remove the century */
	i = 7;
	while (1)
	{
		i++;
		long_day[i-2] = long_day[i];

		/* Exit if get a zero */
		if (!long_day[i] || i==11) break;
	}

	/* Save the date in standard form (8 chars) */
	(void)strnfmt(the_score.day, 9, "%s", long_day);
#endif /* HIGHSCORE_DATE_HACK */

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);

	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_lev, "%3d", p_ptr->max_lev);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);

	/* Save the cause of death (31 chars) */
	sprintf(the_score.how, "%-.31s", p_ptr->died_from);


	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return (1);

	/* Add a new entry to the score list, see where it went */
	score_idx = highscore_add(&the_score);

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return (1);

	/* Success */
	return (0);
}



/*
 * Displays some relevant portion of the high score list.
 */
errr top_twenty(void)
{
	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return (0);
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
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return (0);
	}


	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
	        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);

	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_lev, "%3d", p_ptr->max_lev);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);

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


	/* Success */
	return (0);
}



/*
 * Selectively list highscores based on class -KMW-
 */
void show_highclass(void)
{
	int i = 0, j, m = 0;
	int pr, pc, clev/*, al*/;
	high_score the_score;
	char buf[1024], out_val[256];

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
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

		sprintf(out_val, "%3d) %s the %s (Level %2d)",
		    (m + 1), the_score.who, race_info[pr].title, clev);
		prt(out_val, (m + 7), 0);
		m++;
		j++;
	}

	sprintf(out_val, "You) %s the %s (Level %2d)",
	    player_name, race_info[p_ptr->prace].title, p_ptr->lev);
	prt(out_val, (m + 8), 0);

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
	msg_print("Hit any key to continue");
	msg_print(NULL);

	for (j = 5; j < 18; j++) prt("", j, 0);
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
	char buf[1024], out_val[256], tmp_str[80];

	lastlev = 0;

	/* rr9: TODO - pluralize the race */
	sprintf(tmp_str,"The Greatest of all the %s", race_info[race_num].title);
	prt(tmp_str, 5, 15);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
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
		pc = atoi(the_score.p_c);
		clev = atoi(the_score.cur_lev);

		if (pr == race_num)
		{
			sprintf(out_val, "%3d) %s the %s (Level %3d)",
			    (m + 1), the_score.who,
			race_info[pr].title, clev);
			prt(out_val, (m + 7), 0);
			m++;
			lastlev = clev;
		}
		j++;
	}

	/* add player if qualified */
	if ((p_ptr->prace == race_num) && (p_ptr->lev >= lastlev))
	{
		sprintf(out_val, "You) %s the %s (Level %3d)",
		    player_name, race_info[p_ptr->prace].title, p_ptr->lev);
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
		msg_print("Hit any key to continue");
		msg_print(NULL);
		for (j = 5; j < 19; j++)
			prt("", j, 0);
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
	(void)strcpy(p_ptr->died_from, "Ripe Old Age");

	/* Restore the experience */
	p_ptr->exp = p_ptr->max_exp;

	/* Restore the level */
	p_ptr->lev = p_ptr->max_lev;

	/* Hack -- Instant Gold */
	p_ptr->au += 10000000L;

	/* Clear screen */
	Term_clear();

	/* Display a crown */
	put_str("#", 1, 34);
	put_str("#####", 2, 32);
	put_str("#", 3, 34);
	put_str(",,,  $$$  ,,,", 4, 28);
	put_str(",,=$   \"$$$$$\"   $=,,", 5, 24);
	put_str(",$$        $$$        $$,", 6, 22);
	put_str("*>         <*>         <*", 7, 22);
	put_str("$$         $$$         $$", 8, 22);
	put_str("\"$$        $$$        $$\"", 9, 22);
	put_str("\"$$       $$$       $$\"", 10, 23);
	put_str("*#########*#########*", 11, 24);
	put_str("*#########*#########*", 12, 24);

	/* Display a message */
	put_str("Veni, Vidi, Vici!", 15, 26);
	put_str("I came, I saw, I conquered!", 16, 21);
	put_str(format("All Hail the Mighty %s!", sp_ptr->winner), 17, 22);

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(23);
}
