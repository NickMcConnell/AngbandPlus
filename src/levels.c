/* File: levels.c */

/* Purpose: Levels functions */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Return the parameter of the given command in the given file
 */
bool get_command(const char *file, char comm, char *param)
{
	char buf[1024];
        int i = -1;
	FILE *fp;
	char *s;

	/* Build the filename */
        path_build(buf, 1024, ANGBAND_DIR_DNGN, file);

	/* Open the file */
	fp = my_fopen(buf, "r");

        /* The file exists ? */
        /* no ? then command not found */
        if (!fp) return FALSE;

        /* Parse to the end of the file or when the command is found */
        while (0 == my_fgets(fp, buf, 1024))
        {
		/* Advance the line number */
                i++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

                /* Is it the command we are looking for ? */
                if (buf[0] == comm)
		{
			/* Acquire the text */
			s = buf+2;

                        /* Get the parameter */
                        strcpy(param, s);

                        /* Close it */
                        my_fclose(fp);

                        return TRUE;
		}
                
        }

	/* Close it */
	my_fclose(fp);

        /* Assume command not found */
        return FALSE;
}


/*
 * Return the dungeon branch starting form the current dungeon/level
 */
int get_branch()
{
        char file[20], buf[5];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the branch */
        if (get_command(file, 'B', buf)) return (atoi(buf));

        /* No branch ? return 0 */
        else return 0;
}

/*
 * Return the father dungeon branch
 */
int get_fbranch()
{
        char file[20], buf[5];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the branch */
        if (get_command(file, 'F', buf)) return (atoi(buf));

        /* No branch ? return 0 */
        else return 0;
}

/*
 * Return the father dungeon level
 */
int get_flevel()
{
        char file[20], buf[5];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        if (get_command(file, 'L', buf)) return (atoi(buf));

        /* No level ? return 0 */
        else return 0;
}

/*
 * Return the extension of the savefile for the level
 */
bool get_dungeon_save(char *buf)
{
        char file[20];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        if (get_command(file, 'S', buf)) return (TRUE);
        else return FALSE;
}
