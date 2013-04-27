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

	/*
	 * Hack -- extract first 8 characters of name and
	 * Create the file name from the character's name plus .txt
	 */
	(void)strnfmt(fname, 15, "%.8s.txt", player_base);

	path_make(buf, ANGBAND_DIR_USER, fname);

	/* return the filename */
	return buf;
}


/*
 * Output a string to the notes file.
 * This is the only function that references that file.
 */
void output_note(cptr final_note, ...)
{
	FILE *fff;
	
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, final_note);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, 1024, final_note, &vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Open notes file */
	fff = my_fopen(notes_file(), "a");

	/* Failure */
	if (!fff) return;

	/* Add note, and close note file */
	froff(fff, "%s", buf);

	my_fclose(fff);
}


/*
 * Add note to file using a string + character symbol
 * to specify its type so that the notes file can be
 * searched easily by external utilities.
 */
void add_note(char code, cptr note, ...)
{
	char long_day[25];
	time_t ct = time((time_t *) NULL);
	char depths[32];

	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, note);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, 1024, note, &vp);

	/* End the Varargs Stuff */
	va_end(vp);

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

	/* Output to the notes file */
	output_note("%s %9ld %s %c: %s\n", long_day, turn,
				  depths, code, buf);
}


/*
 * Add note to file using type specified by note_number
 */
void add_note_type(int note_number)
{
	char long_day[30];
	time_t ct = time((time_t *) 0);
	
	int len;

	/* Get the date */
	(void)strftime(long_day, 30, "%Y-%m-%d at %H:%M:%S", localtime(&ct));

	switch (note_number)
	{
		case NOTE_BIRTH:
		{
			/* Player has just been born */
			char player[100];

			/* Build the string containing the player information */
			len = strnfmt(player, 100, "the %s %s",
						  race_info[p_ptr->rp.prace].title,
						  class_info[p_ptr->rp.pclass].title);

			/* No "Chaos-Warrior of Chaos" */
			if (p_ptr->rp.pclass != CLASS_CHAOS_WARRIOR &&
				p_ptr->spell.r[0].realm != REALM_NONE)
			{
				strnfcat(player, 100, &len, " of %s", realm_names[p_ptr->spell.r[0].realm]);
			}

			if (p_ptr->spell.r[1].realm != REALM_NONE)
			{
				strnfcat(player, 100, &len, " and %s", realm_names[p_ptr->spell.r[1].realm]);
			}

			/* Add in "character start" information */
			output_note("\n================================================\n"
						"%s the %s\n"
						"Born on %s\n"
						"================================================\n\n",
						player_name, player,
						long_day);
			break;
		}
			

		case NOTE_WINNER:
		{
			output_note("%s slew the Serpent of Chaos on %s\n."
						"Long live %s!\n"
						"================================================\n",
						player_name, long_day,
						player_name);
			break;
		}

		case NOTE_SAVE_GAME:
		{
			/* Saving the game */
			output_note("\nSession end: %s\n", long_day);
			break;
		}

		case NOTE_ENTER_DUNGEON:
		{
			/* Entering the game after a break. */
			output_note("================================================\n"
						"New session start: %s\n\n",
						long_day);
			break;
		}
	}
}
