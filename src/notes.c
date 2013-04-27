/* File: notes.c */

/* Purpose: Note taking to a file */

/*
 * Copyright (c) 1989, 1999 James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * A short helper function for add_note and other
 * functions that returns the file name for the notes file.
 */
cptr notes_file(void)
{
	char fname[45];
	static char buf[1024];
	char base_name[41];

	/* Hack -- extract first 8 characters of name */
	(void)strnfmt(base_name, sizeof(base_name), "%s", savefile_base);

	/* Create the file name from the character's name plus .txt */
	(void)strnfmt(fname, sizeof(fname), "playrecord-%s.txt", base_name);

	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

	/* return the filename */
	return buf;
}


/*
 * Output a string to the notes file.
 * This is the only function that references that file.
 */
void output_note(char *final_note)
{
	FILE *fff;

	/* Drop priv's */
	safe_setuid_drop();

	/* Open notes file */
	fff = my_fopen(notes_file(), "a");

	/* Grab priv's */
	safe_setuid_grab();

	/* Failure */
	if (!fff) return;

	/* Add note, and close note file */
	fprintf(fff, final_note);

	my_fclose(fff);
}

/*
 * Add note to file using a string + character symbol
 * to specify its type so that the notes file can be
 * searched easily by external utilities.
 */
void add_note(cptr note, char code)
{
	int day, hour, min;
	char depths[32];
	char buf[255];
	char g_time[25];

	extract_day_hour_min(&day, &hour, &min);

	/* Get depth */
	if (!dun_level)
	{
#ifdef JP
		strnfmt(depths, 32, "    地上");
#else
		strnfmt(depths, 32, " Surface");
#endif
	}
	else if (p_ptr->inside_quest)
	{
#ifdef JP
		strnfmt(depths, 32, "クエスト");
#else
		strnfmt(depths, 32, "   Quest");
#endif
	}
	else if (depth_in_feet)
	{
#ifdef JP
		strnfmt(depths, 32," %4d ft", dun_level * 50);
#else
		strnfmt(depths, 32," %4d ft", dun_level * 50);
#endif
	}
	else
	{
#ifdef JP
		strnfmt(depths, 32, "  %3d 階", dun_level);
#else
		strnfmt(depths, 32, " Lev %3d", dun_level);
#endif
	}

	/* Get the time */
	strnfmt(g_time, 10, "%02d:%02d", hour, min);

	/* Make note */
	switch (code)
	{
		case 'D': /* New day */
#ifdef JP
			strnfmt(buf, 255, "%d 日目\n", day);
#else
			strnfmt(buf, 255, "Day %d\n", day);
#endif
			break;
		case 'A': /* Found the artifact */
#ifdef JP
			strnfmt(buf, 255, "%s %s : %s を発見。\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : Found The %s.\n", g_time, depths, note);
#endif
			break;
		case 'U': /* Defeated The unique */
#ifdef JP
			strnfmt(buf, 255, "%s %s : %s を撃破。\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : Defeated %s.\n", g_time, depths, note);
#endif
			break;
		case 'Q': /* Complete quest */
#ifdef JP
			strnfmt(buf, 255, "%s %s : クエスト%sを達成。\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : Complete quest %s.\n", g_time, depths, note);
#endif
			break;
		case 'L': /* Level up */
#ifdef JP
			strnfmt(buf, 255, "%s %s : レベル %s に上がった。\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : Reach player level %s.\n", g_time, depths, note);
#endif
			break;
		case 's': /* Level up */
#ifdef JP
			strnfmt(buf, 255, "%s %s : %s の身体を乗っ取った。\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : Snatched %s's body.\n", g_time, depths, note);
#endif
			break;
		case 'r': /* reward from valar */
#ifdef JP
			strnfmt(buf, 255, "%s %s : パトロンの報酬で、%s\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : The patron rewards you with %s.\n", g_time, depths, note);
#endif
			break;
		case 'd': /* Died (Killed) */
#ifdef JP
			strnfmt(buf, 255, "%s %s : %s に殺された。\n", g_time, depths, note);
#else
			strnfmt(buf, 255, "%s %s : Killed by %s.\n", g_time, depths, note);
#endif
			break;
		case 'W': /* Cheat mode */
#ifdef JP
			strnfmt(buf, 255, "      [ メモ ] : %s によりスコアが残せなくなった。\n", note);
#else
			strnfmt(buf, 255, "      [ Note ] : Give up recording score for %s.\n", note);
#endif
			break;
		default :
#ifdef JP
			strnfmt(buf, 255, "      [ メモ ] : %s\n", note);
#else
			strnfmt(buf, 255, "      [ Note ] : %s\n", note);
#endif
	}

	/* Output to the notes file */
	output_note(buf);
}


/*
 * Add note to file using type specified by note_number
 */
void add_note_type(int note_number)
{
	char long_day[30];
	char buf[1024];
	time_t ct = time((time_t*)0);

	/* Get the date */
#ifdef JP
	strftime(long_day, 30, "%Y年%m月%d日 %H:%M:%S", localtime(&ct));
#else
	strftime(long_day, 30, "%Y-%m-%d at %H:%M:%S", localtime(&ct));
#endif


	switch(note_number)
	{
		case NOTE_BIRTH:
		{
			/* Player has just been born */
			char player[100];

			/* Build the string containing the player information */
#ifdef JP
			strnfmt(player, 100, "%sの%s", race_info[p_ptr->prace].title,
				 class_info[p_ptr->pclass].title);
#else
			strnfmt(player, 100, "the %s %s", race_info[p_ptr->prace].title,
				 class_info[p_ptr->pclass].title);
#endif

#ifdef JP
			if (p_ptr->realm1 != REALM_NONE)
			{
				strnfmt(player, 100, "%s(", player);

				strnfmt(player, 100, "%s%s", player, realm_names[p_ptr->realm1]);

				if (p_ptr->realm2 != REALM_NONE)
				{
					strnfmt(player, 100, "%s & ", player);
					strnfmt(player, 100, "%s%s", player, realm_names[p_ptr->realm2]);
				}

				strnfmt(player, 100, "%s)", player);
			}
#else
			if (p_ptr->realm1 != REALM_NONE)
			{
				strnfmt(player, 100, "%s of ", player);
				strnfmt(player, 100, "%s%s", player, realm_names[p_ptr->realm1]);
			}

			if (p_ptr->realm2 != REALM_NONE)
			{
				strnfmt(player, 100, "%s and ", player);
				strnfmt(player, 100, "%s%s", player, realm_names[p_ptr->realm2]);
			}
#endif

			/* Add in "character start" information */
			strnfmt(buf, 1024, "\n================================================\n");
#ifdef JP
			strnfmt(buf, 1024, "%s%s %s\n", buf, player, player_name);
			strnfmt(buf, 1024, "%s%sに生まれる\n", buf, long_day);
#else
			strnfmt(buf, 1024, "%s%s the %s\n", buf, player_name, player);
			strnfmt(buf, 1024, "%sBorn on %s\n", buf, long_day);
#endif
			strnfmt(buf, 1024, "%s================================================\n\n", buf);
			strnfmt(buf, 1024, "%s1 日目\n",buf);
		}
		break;

		case NOTE_WINNER:
		{
#ifdef JP
			strnfmt(buf, 1024, "%s は冥王『モルゴス』を倒した。\n", player_name);
			strnfmt(buf, 1024, "%s偉大なる%sに栄光あれ！ (%s 達成)\n", buf, player_name, long_day);
#else
			strnfmt(buf, 1024, "%s slew Morgoth, Lord of Darkness.\n", player_name);
			strnfmt(buf, 1024, "%sLong live %s! (on %s)\n", buf, player_name, long_day);
#endif
			strnfmt(buf, 1024,  "%s================================================\n", buf);
		}
		break;

		case NOTE_SAVE_GAME:
		{
			/* Saving the game */
			strnfmt(buf, 1024, "\nSession end: %s\n", long_day);
		}
		break;

		case NOTE_ENTER_DUNGEON:
		{
			/* Entering the game after a break. */
			strnfmt(buf, 1024,  "================================================\n");
			strnfmt(buf, 1024, "%sNew session start: %s\n\n", buf, long_day);
		}
		break;

		case NOTE_ABORT_GAME:
		{
			/* Saving the game */
			strnfmt(buf, 1024, "\nSession aborted: %s\n", long_day);
		}
		break;

		default: return;
	}

	/* Output the notes to the file */
	output_note(buf);
}
