/* File: score.c */

/*
 * Purpose: Highscore handling for Angband
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "option.h"
#include "score.h"

#ifndef __cplusplus
typedef struct high_score high_score;
#endif

/**
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ascii strings.
 *
 * In addition, the "number" fields are right justified, and
 * space padded, to the full available length (minus the "null").
 *
 * Note that "string comparisons" are thus valid on "pts".
 */

struct high_score
{
	char what[8];		/**< Version info (string) */
	char pts[10];		/**< Total Score (number) */
	char gold[10];		/**< Total Gold (number) */
	char turns[10];		/**< Turns Taken (number) */
	char day[10];		/**< Time stamp (string) */
	char who[16];		/**< Player Name (string) */
	char uid[8];		/**< Player UID (number) */

	char sex[2];		/**< Player Sex (string) */
	char p_r[3];		/**< Player Race (number) */
	char p_c[3];		/**< Player Class (number) */

	char cur_lev[4];	/**< Current Player Level (number) */
	char cur_dun[4];	/**< Current Dungeon Level (number) */
	char max_lev[4];	/**< Max Player Level (number) */
	char max_dun[4];	/**< Max Dungeon Level (number) */

	char how[32];		/**< Method of death (string) */
};

/**
 * The "highscore" file descriptor, if available.
 */
static int highscore_fd = -1;

/**
 * Hack - save the time of death
 */
static time_t death_time = (time_t)0;

bool open_scoretable(int mode)
{
	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	if (O_RDWR == mode)
	{
		/* Grab permissions */
		safe_setuid_grab();

		/* Open the high score file, for reading/writing */
		highscore_fd = fd_open(buf, mode);

		/* Drop permissions */
		safe_setuid_drop();
	}
	else
	{
		/* Open the high score file, for reading/writing */
		highscore_fd = fd_open(buf, mode);
	}
	return -1!=highscore_fd;
}

void close_scoretable(void)
{
	/* Shut the high score file */
	fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;
}

/**
 * Centers a string within a 31 character string
 */
static void center_string(char *buf, size_t len, const char* const str)
{
	int i = strlen(str);	/* Total length */
	int j = 15 - i / 2;		/* Necessary border */

	/* Mega-Hack */
	strnfmt(buf, len, "%*s%s%*s", j, "", str, 31 - i - j, "");
}

/**
 * Seek score 'i' in the highscore file
 */
static int highscore_seek(int i)
{
	/* Seek for the requested record */
	return (fd_seek(highscore_fd, i * sizeof(high_score)));
}


/**
 * Read one score from the highscore file
 */
static errr highscore_read(high_score *score)
{
	/* Read the record, note failure */
	return (fd_read(highscore_fd, (char*)(score), sizeof(high_score)));
}


/**
 * Write one score to the highscore file
 */
static errr highscore_write(const high_score *score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (const char*)(score), sizeof(high_score)));
}




/**
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(const high_score *score)
{
	int i;

	high_score the_score;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return -1;

	/* Go to the start of the highscore file */
	if (highscore_seek(0)) return -1;

	/* Read until we get to a higher score */
	for (i = 0; i < MAX_HISCORES; ++i)
	{
		if (highscore_read(&the_score)) return i;
		if (strcmp(the_score.pts, score->pts) < 0) return i;
	}

	/* The "last" entry is always usable */
	return (MAX_HISCORES - 1);
}


/**
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(const high_score *score)
{
	int i, slot;
	bool done = FALSE;

	high_score the_score, tmpscore;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return -1;

	/* Determine where the score should go */
	slot = highscore_where(score);

	/* Hack -- Not on the list */
	if (slot < 0) return -1;

	/* Hack -- prepare to dump the new score */
	the_score = (*score);

	/* Slide all the scores down one */
	for (i = slot; !done && (i < MAX_HISCORES); i++)
	{
		/* Read the old guy, note errors */
		if (highscore_seek(i)) return -1;
		if (highscore_read(&tmpscore)) done = TRUE;

		/* Back up and dump the score we were holding */
		if (highscore_seek(i)) return -1;
		if (highscore_write(&the_score)) return -1;

		/* Hack -- Save the old score, for the next pass */
		the_score = tmpscore;
	}

	/* Return location used */
	return (slot);
}



/**
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 * Only five entries per line, too much info.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
static void display_scores_aux(int from, int to, int note, high_score *score)
{
	char ch;

	int j, k, n, place;
	int count;

	high_score the_score;

	char out_val[160];
	char tmp_val[160];

	byte attr;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return;


	/* Assume we will show the first 10 */
	if (from < 0) from = 0;
	if (to < 0) to = 10;
	if (to > MAX_HISCORES) to = MAX_HISCORES;


	/* Seek to the beginning */
	if (highscore_seek(0)) return;

	/* Hack -- Count the high scores */
	for (count = 0; count < MAX_HISCORES; count++)
	{
		if (highscore_read(&the_score)) break;
	}

	/* Hack -- allow "fake" entry to be last */
	if ((note == count) && score) count++;

	/* Forget about the last entries */
	if (count > to) count = to;


	/* Show 5 per page, until "done" */
	for (k = from, j = from, place = k+1; k < count; k += 5)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
		put_str(format("                %s Hall of Fame", VERSION_NAME),
		        0, 0);

		/* Indicate non-top scores */
		if (k > 0)
		{
			strnfmt(tmp_val, sizeof(tmp_val), "(from position %d)", place);
			put_str(tmp_val, 0, 40);
		}

		/* Dump 5 entries */
		for (n = 0; j < count && n < 5; place++, j++, n++)
		{
			int pr, pc, clev, mlev, cdun, mdun;

			const char* user;
			const char* gold;
			const char* when;
			const char* aged;


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
			for (user = the_score.uid; isspace((unsigned char)*user); user++) /* loop */;
			for (when = the_score.day; isspace((unsigned char)*when); when++) /* loop */;
			for (gold = the_score.gold; isspace((unsigned char)*gold); gold++) /* loop */;
			for (aged = the_score.turns; isspace((unsigned char)*aged); aged++) /* loop */;

			/* Clean up standard encoded form of "when" */
			if ((*when == '@') && strlen(when) == 9)
			{
				strnfmt(tmp_val, sizeof(tmp_val),
				        "%.4s-%.2s-%.2s",
				        when + 1, when + 5, when + 7);
				when = tmp_val;
			}

			/* Dump some info */
			strnfmt(out_val, sizeof(out_val),
			        "%3d.%9s  %s the %s %s, Level %d",
			        place, the_score.pts, the_score.who,
			        player_type::race_name(pr), player_type::class_name(pc),
			        clev);

			/* Append a "maximum level" */
			if (mlev > clev) my_strcat(out_val, format(" (Max %d)", mlev), sizeof(out_val));

			/* Dump the first line */
			c_put_str(attr, out_val, n*4 + 2, 0);

			/* Another line of info */
			strnfmt(out_val, sizeof(out_val),
			        "               Killed by %s on dungeon level %d",
			        the_score.how, cdun);

			/* Hack -- some people die in the town */
			if (!cdun)
			{
				strnfmt(out_val, sizeof(out_val),
				        "               Killed by %s in the town",
				        the_score.how);
			}

			/* Append a "maximum level" */
			if (mdun > cdun) my_strcat(out_val, format(" (Max %d)", mdun), sizeof(out_val));

			/* Dump the info */
			c_put_str(attr, out_val, n*4 + 3, 0);

			/* And still another line of info */
			strnfmt(out_val, sizeof(out_val),
			        "               (User %s, Date %s, Gold %s, Turn %s).",
			        user, when, gold, aged);
			c_put_str(attr, out_val, n*4 + 4, 0);
		}


		/* Wait for response */
		prt("[Press ESC to exit, any other key to continue.]", 23, 17);
		ch = inkey();
		prt("", 23, 0);

		/* Hack -- notice Escape */
		if (ch == ESCAPE) break;
	}
}


/**
 * Hack -- Display the scores in a given range and quit.
 *
 * This function is only called from "main.c" when the user asks
 * to see the "high scores".
 */
void display_scores(int from, int to)
{
	/* Open the binary high score file, for reading */
	open_scoretable(O_RDONLY);

	/* Clear screen */
	Term_clear();

	/* Title */
	put_str(format("                %s Hall of Fame", VERSION_NAME), 0, 0);

	/* Display the scores */
	display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	close_scoretable();

	/* Wait for response */
	prt("[Press any key to exit.]", 23, 17);
	(void)inkey();
	prt("", 23, 0);

	/* Quit */
	quit(NULL);
}


/*
 * Hack - save index of player's high score
 */
static int score_idx = -1;


/**
 * Enters a players name on a hi-score table, if "legal".
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
int enter_score(void)
{
	int j;

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0) return 0;


	/* Wizard-mode pre-empts scoring */
	if (p_ptr->noscore & NOSCORE_WIZARD)
	{
		msg_print("Score not registered for wizards.");
		message_flush();
		score_idx = -1;
		return 0;
	}

#ifndef SCORE_BORGS

	/* Borg-mode pre-empts scoring */
	if (p_ptr->noscore & NOSCORE_BORG)
	{
		msg_print("Score not registered for borgs.");
		message_flush();
		score_idx = -1;
		return 0;
	}
#endif /* SCORE_BORGS */

	/* Cheaters are not scored */
	for (j = OPT_SCORE; j < OPT_MAX; ++j)
	{
		if (!op_ptr->opt[j]) continue;

		msg_print("Score not registered for cheaters.");
		message_flush();
		score_idx = -1;
		return 0;
	}

	/* Hack -- Interupted */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Interrupting"))
	{
		msg_print("Score not registered due to interruption.");
		message_flush();
		score_idx = -1;
		return 0;
	}

	/* Hack -- Quitter */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Quitting"))
	{
		msg_print("Score not registered due to quitting.");
		message_flush();
		score_idx = -1;
		return 0;
	}


	/* Clear the record */
	WIPE(&the_score);

	/* Save the version */
	strnfmt(the_score.what, sizeof(the_score.what), "%s", VERSION_STRING);

	/* Calculate and save the points */
	strnfmt(the_score.pts, sizeof(the_score.pts), "%9lu", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	strnfmt(the_score.gold, sizeof(the_score.gold), "%9lu", (long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	strnfmt(the_score.turns, sizeof(the_score.turns), "%9lu", (long)turn);
	the_score.turns[9] = '\0';

	/* Save the date in standard encoded form (9 chars) */
	strftime(the_score.day, sizeof(the_score.day), "@%Y%m%d", localtime(&death_time));

	/* Save the player name (15 chars) */
	strnfmt(the_score.who, sizeof(the_score.who), "%-.15s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	strnfmt(the_score.uid, sizeof(the_score.uid), "%7u", player_uid);
	strnfmt(the_score.sex, sizeof(the_score.sex), "%c", p_ptr->terse_gender());
	strnfmt(the_score.p_r, sizeof(the_score.p_r), "%2d", p_ptr->prace);
	strnfmt(the_score.p_c, sizeof(the_score.p_c), "%2d", p_ptr->pclass);

	/* Save the level and such */
	strnfmt(the_score.cur_lev, sizeof(the_score.cur_lev), "%3d", p_ptr->lev);
	strnfmt(the_score.cur_dun, sizeof(the_score.cur_dun), "%3d", p_ptr->depth);
	strnfmt(the_score.max_lev, sizeof(the_score.max_lev), "%3d", p_ptr->max_lev);
	strnfmt(the_score.max_dun, sizeof(the_score.max_dun), "%3d", p_ptr->max_depth);

	/* Save the cause of death (31 chars) */
	strnfmt(the_score.how, sizeof(the_score.how), "%-.31s", p_ptr->died_from);

	/* Grab permissions */
	safe_setuid_grab();

	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return 1;

	/* Drop permissions */
	safe_setuid_drop();

	/* Add a new entry to the score list, see where it went */
	score_idx = highscore_add(&the_score);

	/* Grab permissions */
	safe_setuid_grab();

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return 1;

	/* Drop permissions */
	safe_setuid_drop();

	/* Success */
	return 0;
}



/**
 * Enters a players name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
void top_twenty(void)
{
	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		message_flush();
		return;
	}

	/* Player's score unavailable */
	if (score_idx == -1)
	{
		display_scores_aux(0, 10, -1, NULL);
		return;
	}

	/* Hack -- Display the top fifteen scores */
	else if (score_idx < 10)
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


/**
 * Predict the players location, and display it.
 */
int predict_score(void)
{
	int j;

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		message_flush();
		return 0;
	}


	/* Save the version */
	strnfmt(the_score.what, sizeof(the_score.what), "%s", VERSION_STRING);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", p_ptr->terse_gender());
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


void show_scores(void)
{
	/* Open the binary high score file, for reading */
	if (!open_scoretable(O_RDONLY))
	{	/* Paranoia -- No score file */
		msg_print("Score file unavailable.");
	}
	else
	{
		/* Save Screen */
		screen_save();

		/* Clear screen */
		Term_clear();

		/* Display the scores */
		if (character_generated)
			predict_score();
		else
			display_scores_aux(0, MAX_HISCORES, -1, NULL);

		/* Shut the high score file */
		close_scoretable();

		/* Load screen */
		screen_load();

		/* Hack - Flush it */
		Term_fresh();
	}
}

/**
 * Display a "tomb-stone"
 */
void print_tomb(void)
{
	const char* p;

	char tmp[160];

	char buf[1024];

	FILE *fp;

	/* Get time of death */
	(void)time(&death_time);

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead.txt");

	/* Open the News file */
	fp = my_fopen(buf, "r");

	/* Dump */
	if (fp)
	{
		int i = 0;

		/* Dump the file to the screen */
		while (0 == my_fgets(fp, buf, sizeof(buf)))
		{
			/* Display and advance */
			put_str(buf, i++, 0);
		}

		/* Close */
		my_fclose(fp);
	}


	/* King or Queen */
	if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "Magnificent";
	}

	/* Normal */
	else
	{
		p = player_type::c_text + p_ptr->cp_ptr->title[(p_ptr->lev - 1) / 5];
	}

	center_string(buf, sizeof(buf), op_ptr->full_name);
	put_str(buf, 6, 11);

	center_string(buf, sizeof(buf), "the");
	put_str(buf, 7, 11);

	center_string(buf, sizeof(buf), p);
	put_str(buf, 8, 11);


	center_string(buf, sizeof(buf), p_ptr->classname());
	put_str(buf, 10, 11);

	strnfmt(tmp, sizeof(tmp), "Level: %d", (int)p_ptr->lev);
	center_string(buf, sizeof(buf), tmp);
	put_str(buf, 11, 11);

	strnfmt(tmp, sizeof(tmp), "Exp: %ld", (long)p_ptr->exp);
	center_string(buf, sizeof(buf), tmp);
	put_str(buf, 12, 11);

	strnfmt(tmp, sizeof(tmp), "AU: %ld", (long)p_ptr->au);
	center_string(buf, sizeof(buf), tmp);
	put_str(buf, 13, 11);

	strnfmt(tmp, sizeof(tmp), "Killed on Level %d", p_ptr->depth);
	center_string(buf, sizeof(buf), tmp);
	put_str(buf, 14, 11);

	strnfmt(tmp, sizeof(tmp), "by %s.", p_ptr->died_from);
	center_string(buf, sizeof(buf), tmp);
	put_str(buf, 15, 11);


	strnfmt(tmp, sizeof(tmp), "%-.24s", ctime(&death_time));
	center_string(buf, sizeof(buf), tmp);
	put_str(buf, 17, 11);
}

