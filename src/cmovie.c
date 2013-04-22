/* File: cmovie.c */

/* Purpose: play cmovie files -DarkGod-Improv- */

#include "angband.h"

/* Play a given cmovie */
s16b do_play_cmovie(cptr cmov_file)
{
	FILE *fff;

        int y, line = 0, x;
        int delay;

        char *s;

	char buf[1024];
        char cbuf[90];
        char ch;

        char mode = 0;

	/* Build the filename */
        path_build(buf, 1024, ANGBAND_DIR_CMOV, cmov_file);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

        /* Read the file */
        fff = my_fopen(buf, "r");

	/* Failure */
        if (!fff) return (-1);

	/* Save screen */
        character_icky = TRUE;
	Term_save();
        Term_clear();

        /* Give some usefull info */
        prt("While viewing the movie you can press Escape to exit, Space to switch between", 0, 0);
        prt("fluid more and step by step mode and any other key to step a frame in step by", 1, 0);
        prt("step mode.", 2, 0);
        prt("You can also use + and - to speed up/down the playing speed.", 4, 0);
        prt("The base playing speed depends on your delay factor setting (see the options).", 5, 0);
        prt("Press any key when ready.", 7, 0);

        inkey();

        Term_clear();

        line = -1;

        delay = delay_factor * delay_factor * delay_factor * 4;
        if (delay < 1) delay = 1;

        /* Init to white */
        for (x = 0; x < 80; x++)
        {
                cbuf[x] = 'w';
        }

	/* Parse */
        while (0 == my_fgets(fff, buf, 1024))
	{
                /* Do not wait */
                inkey_scan = TRUE;
                ch = inkey();

                /* Stop */
                if (ch == ESCAPE) break;

                /* Change mode */
                else if (ch == ' ')
                {
                        mode = !mode;
                }

                /* Change speed */
                else if (ch == '+')
                {
                        delay -= 20;
                        if (delay < 1) delay = 1;
                }
                else if (ch == '-')
                {
                        delay += 20;
                        if (delay > 5000) delay = 5000;
                }

                line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
                if (buf[1] != ':') break;

                /* Clean screen */
                if (buf[0] == 'C')
                {
                        Term_clear();

                        /* Next */
                        continue;
                }

                /* Wait a key */
                if (buf[0] == 'W')
                {
                        inkey();

                        /* Next */
                        continue;
                }

                /* Sleep */
                if (buf[0] == 'S')
                {
                        int wait;

			/* Scan for the values */
                        if (1 != sscanf(buf+2, "%d:",
                                &wait)) return (-2);

                        if (!mode) Term_xtra(TERM_XTRA_DELAY, wait * delay);
                        else
                        {
                                bool stop = FALSE;

                                while (TRUE)
                                {
                                        ch = inkey();

                                        /* Stop */
                                        if (ch == ESCAPE)
                                        {
                                                stop = TRUE;
                                                break;
                                        }
                                        /* Change mode */
                                        else if (ch == ' ')
                                        {
                                                mode = !mode;
                                                break;
                                        }
                                        /* Change speed */
                                        else if (ch == '+')
                                        {
                                                delay -= 20;
                                                if (delay < 1) delay = 1;
                                        }
                                        else if (ch == '-')
                                        {
                                                delay += 20;
                                                if (delay > 5000) delay = 5000;
                                        }
                                        else break;
                                }
                                if (stop) break;
                        }

                        /* Next */
                        continue;
                }

                /* Get color for the NEXT L line */
                if (buf[0] == 'E')
                {
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
                        if (!s) return (-2);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
                        if (!*s) return (-2);

			/* Get the index */
                        y = atoi(buf+2);

                        C_COPY(cbuf, s, 80, char);

			/* Next... */
			continue;
                }

                /* Print a line */
                if (buf[0] == 'L')
                {
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
                        if (!s) return (-2);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
                        if (!*s) return (-2);

			/* Get the index */
                        y = atoi(buf+2);

                        for (x = 0; x < 80; x++)
                        {
                                Term_putch(x, y, color_char_to_attr(cbuf[x]), s[x]);

                                /* Reinit to white */
                                cbuf[x] = 'w';
                        }
                        Term_redraw_section(0, y, 79, y);

			/* Next... */
			continue;
                }

                /* Update 1 char */
                if (buf[0] == 'P')
                {
                        int x, y, a, c;

			/* Scan for the values */
                        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &x, &y, &c, &a))
                        {
                                a = 'w';
                                if (3 != sscanf(buf+2, "%d:%d:%d",
                                        &x, &y, &c)) return (-2);
                        }

                        Term_putch(x, y, color_char_to_attr(cbuf[x]), c);
                        Term_redraw_section(x, y, x + 1, y + 1);

                        /* Next... */
                        continue;
                }
        }

        /* Load screen */
        Term_load();
	character_icky = FALSE;

	/* Close */
	my_fclose(fff);

        return (0);
}

/* Start the recording of a cmovie */
void do_record_cmovie(cptr cmovie)
{
        char buf[1024];
        int fd = -1;
        int y;

	/* Build the filename */
        path_build(buf, 1024, ANGBAND_DIR_CMOV, cmovie);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Check for existing file */
	fd = fd_open(buf, O_RDONLY);

	/* Existing file */
	if (fd >= 0)
	{
		char out_val[160];

		/* Close the file */
		(void)fd_close(fd);

		/* Build query */
                (void)sprintf(out_val, "Replace existing file %s? ", cmovie);

		/* Ask */
		if (get_check(out_val)) fd = -1;
	}

        /* Be sure */
        if (!get_check("Ready to record?")) return;

	/* Grab privs */
	safe_setuid_grab();

	/* Open the non-existing file */
        if (fd < 0) movfile = my_fopen(buf, "w");

	/* And drop them */
	safe_setuid_drop();

	/* Invalid file */
        if (movfile == NULL)
        {
                msg_format("Cmovie recording failed!");

                return;
        }

        /* First thing: Record clear screen then enable the recording */
        fprintf(movfile, "# Generated by NTAngband %d.%d.%d\n", FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);
        fprintf(movfile, "C:\n");
        last_paused = 0;
        do_movies = 1;

        /* Mega Hack, get the screen */
        for (y = 0; y < Term->hgt; y++)
        {
                fprintf(movfile, "E:%d:%.80s\n", y, clean80(y, Term->scr->a[y], TRUE));
                fprintf(movfile, "L:%d:%.80s\n", y, clean80(y, Term->scr->c[y], FALSE));
        }
}

/* Stop the recording */
void do_stop_cmovie()
{
        if (do_movies == 1)
        {
                do_movies = 0;
		my_fclose(movfile);
        }
}

/* Start a cmovie */
void do_start_cmovie()
{
        char name[90], rname[94];

        /* Should never happen */
        if (do_movies == 1) return;

        /* Default */
        sprintf(name, "%s", player_base);

        if (get_string("Cmovie name: ", name, 80))
        {
                if (name[0] && (name[0] != ' '))
                {
                        sprintf(rname, "%s.cmv", name);

                        if (get_check("Record(y), Play(n)?")) do_record_cmovie(rname);
                        else do_play_cmovie(rname);
                }
        }
}
