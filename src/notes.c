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
	char fname[15];
	static char buf[500];
        char base_name[9];

	/* Hack -- extract first 8 characters of name */
	strncpy(base_name, player_name, 8);

        base_name[8] = '\0';

	/* Create the file name from the character's name plus .txt */
        sprintf(fname, "%s.nte", base_name);
        path_build(buf, 500, ANGBAND_DIR_NOTE, fname);

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
	
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Grab */
	safe_setuid_grab();
	
	/* Open notes file */
	fff = my_fopen(notes_file(), "a");
	
	/* Drop */
	safe_setuid_drop();
  
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
void add_note(char *note, char code)
{
        char buf[100];
        char final_note[100];
        char long_day[50];
	char depths[32];
        char *tmp;
	
	/* Get the first 60 chars - so do not have an overflow */
        tmp = C_WIPE(buf, 100, char);
	strncpy(buf, note, 60);

	/* Get date and time */
        sprintf(long_day, "%ld:%02ld %s, %s", (bst(HOUR, turn) % 12 == 0) ? 12 : (bst(HOUR, turn) % 12), bst(MINUTE, turn), (bst(HOUR, turn) < 12) ? "AM" : "PM", get_month_name(bst(DAY, turn), FALSE, FALSE));

	/* Get depth  */
 
	if (!dun_level)
	{
		strcpy(depths, "  Town");
	}
	else if (depth_in_feet)
	{
		sprintf(depths, "%4dft", dun_level * 50);
	}
 	else
	{
		sprintf(depths, "Lev%3d", dun_level);
	}
            
	/* Make note */
        sprintf(final_note, "%-20s %s %c: %s\n", long_day,
                 depths, code, buf);
      
	/* Output to the notes file */
	output_note(final_note);
}


/* Add note to file using type specified by note_number */
void add_note_type(int note_number)
{
        char long_day[50], true_long_day[50];
	char buf[1024];
	time_t ct = time((time_t*)0);
	
	/* Get the date */
        strftime(true_long_day, 30, "%Y-%m-%d at %H:%M:%S", localtime(&ct));
	
	/* Get the date */
        sprintf(buf, "%ld", bst(YEAR, turn) + START_YEAR);
        sprintf(long_day, "%ld:%02ld %s the %s of III %s", (bst(HOUR, turn) % 12 == 0) ? 12 : (bst(HOUR, turn) % 12), bst(MINUTE, turn), (bst(HOUR, turn) < 12) ? "AM" : "PM", get_month_name(bst(DAY, turn), FALSE, FALSE), buf);
	
	switch(note_number)
	{
		case NOTE_BIRTH:
		{
			/* Player has just been born */
			char player[100];
			
			/* Build the string containing the player information */
                        if (p_ptr->pracem) sprintf(player, "the %s %s %s", race_mod_info[p_ptr->pracem].title, race_info[p_ptr->prace].title, class_info[p_ptr->pclass].title);
                        else sprintf(player, "the %s %s", race_info[p_ptr->prace].title, class_info[p_ptr->pclass].title);
			
			if (p_ptr->realm1 != REALM_NONE)
			{
				strcat(player, " of ");
				strcat(player, realm_names[p_ptr->realm1]);
			}
	 		
			if (p_ptr->realm2 != REALM_NONE)
			{
				strcat(player, " and ");
				strcat(player, realm_names[p_ptr->realm2]);
			}
			
			/* Add in "character start" information */
                        sprintf(buf, "\n================================================\n%s %s\nBorn on %s\n================================================\n\n", player_name, player, true_long_day);
		}
		break;
		
		case NOTE_WINNER:
		{
                        sprintf(buf, "%s slew Morgoth on %s\n.sLong live %s!\n================================================\n", player_name, long_day, player_name);
		}
		break;
		
		case NOTE_SAVE_GAME:
		{
			/* Saving the game */
                        sprintf(buf, "\nSession end: %s\n", true_long_day);
		}
                break;
		
		case NOTE_ENTER_DUNGEON:
		{
			/* Entering the game after a break. */
                        sprintf(buf, "================================================\nNew session start: %s\n\n", true_long_day);
		}
		break;
		
		default: return;
	}
	
	/* Output the notes to the file */
	output_note(buf);
}
