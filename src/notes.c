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
 * functions that returns the file name for the note-taking file.
 */
cptr notes_file(void)
{
	char fname[15];
	static char buf[1024];
	char base_name[9];

	/* Hack -- extract first 8 characters of name */
	(void)strnfmt(base_name, 9, "%s", player_base);

	/* Create the file name from the character's name plus .txt */
	(void)strnfmt(fname, 15, "%s.txt", base_name);

	path_build(buf, 1024, ANGBAND_DIR_SAVE, fname);

	/* return the filename */
	return buf;
}


/*
 * Output a string to the notes file.
 * This is the only function that references that file.
 */
void output_note(cptr final_note)
{
	FILE *fff;

	/* Open notes file */
	fff = my_fopen(notes_file(), "a");

	/* Failure */
	if (!fff) return;

	/* Add note, and close note file */
	fprintf(fff, "%s", final_note);

	my_fclose(fff);
}


/*
 * Add note to file using a string + character symbol
 * to specify its type so that the notes file can be
 * searched easily by external utilities.
 */
void add_note(cptr note, char code)
{
	char buf[255];
	char long_day[25];
	time_t ct = time((time_t *) NULL);
	char depths[32];


	/* Get depth */
	if (!p_ptr->depth)
	{
		if (p_ptr->place_num)
		{
			if (place[p_ptr->place_num].quest_num)
			{
				(void)strnfmt(depths, 32, " Quest");
			}
			else
			{
				(void)strnfmt(depths, 32, "  Town");
			}
		}
		else
		{
			(void)strnfmt(depths, 32, "  Wild");
		}
	}
	else if (depth_in_feet)
	{
		(void)strnfmt(depths, 32, "%4dft", p_ptr->depth * 50);
	}
	else
	{
		(void)strnfmt(depths, 32, "Lev%3d", p_ptr->depth);
	}

	/* Get the time */
	(void)strftime(long_day, 10, "%H:%M:%S", localtime(&ct));

	/* Make note */
	(void)strnfmt(buf, 255, "%s %9ld %s %c: %s\n", long_day, turn,
				  depths, code, note);

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
	time_t ct = time((time_t *) 0);

	/* Get the date */
	(void)strftime(long_day, 30, "%Y-%m-%d at %H:%M:%S", localtime(&ct));

	switch (note_number)
	{
		case NOTE_BIRTH:
		{
			/* Player has just been born */
			char player[100];

			/* Build the string containing the player information */
			(void)strnfmt(player, 100, "the %s %s",
						  race_info[p_ptr->prace].title,
						  class_info[p_ptr->pclass].title);

			if (p_ptr->realm1 != REALM_NONE)
			{
				(void)strnfmt(player, 100, "%s of ", player);
				(void)strnfmt(player, 100, "%s%s", player,
							  realm_names[p_ptr->realm1]);
			}

			if (p_ptr->realm2 != REALM_NONE)
			{
				(void)strnfmt(player, 100, "%s and ", player);
				(void)strnfmt(player, 100, "%s%s", player,
							  realm_names[p_ptr->realm2]);
			}

			/* Add in "character start" information */
			(void)strnfmt(buf, 1024,
						  "\n================================================\n");
			(void)strnfmt(buf, 1024, "%s%s the %s\n", buf, player_name, player);
			(void)strnfmt(buf, 1024, "%sBorn on %s\n", buf, long_day);
			(void)strnfmt(buf, 1024,
						  "%s================================================\n\n",
						  buf);
		}
			break;

		case NOTE_WINNER:
		{
			(void)strnfmt(buf, 1024, "%s slew the Serpent of Chaos on %s\n.",
						  player_name, long_day);
			(void)strnfmt(buf, 1024, "%sLong live %s!\n", buf, player_name);
			(void)strnfmt(buf, 1024,
						  "%s================================================\n",
						  buf);
		}
			break;

		case NOTE_SAVE_GAME:
		{
			/* Saving the game */
			(void)strnfmt(buf, 1024, "\nSession end: %s\n", long_day);
		}
			break;

		case NOTE_ENTER_DUNGEON:
		{
			/* Entering the game after a break. */
			(void)strnfmt(buf, 1024,
						  "================================================\n");
			(void)strnfmt(buf, 1024, "%sNew session start: %s\n\n", buf,
						  long_day);
		}
			break;

		default: return;
	}

	/* Output the notes to the file */
	output_note(buf);
}
