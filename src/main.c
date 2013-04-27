/* File: main.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"


/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(ACORN) && !defined(MACH_O_CARBON)


/*
 * List of available modules in the order they are tried.
 */
static const module_type modules[] =
{
#ifdef USE_GTK
	INIT_MODULE(gtk),
#endif /* USE_GTK */

#ifdef USE_XAW
	INIT_MODULE(xaw),
#endif /* USE_XAW */

#ifdef USE_X11
	INIT_MODULE(x11),
#endif /* USE_X11 */

#ifdef USE_XPJ
	INIT_MODULE(xpj),
#endif /* USE_XPJ */

#ifdef USE_TNB
	INIT_MODULE(tnb),
#endif /* USE_TNB */

#ifdef USE_GCU
	INIT_MODULE(gcu),
#endif /* USE_GCU */

#ifdef USE_CAP
	INIT_MODULE(cap),
#endif /* USE_CAP */

#ifdef USE_DOS
	INIT_MODULE(dos),
#endif /* USE_DOS */

#ifdef USE_IBM
	INIT_MODULE(ibm),
#endif /* USE_IBM */

#ifdef USE_EMX
	INIT_MODULE(emx),
#endif /* USE_EMX */

#ifdef USE_SLA
	INIT_MODULE(sla),
#endif /* USE_SLA */

#ifdef USE_LSL
	INIT_MODULE(lsl),
#endif /* USE_LSL */

#ifdef USE_AMI
	INIT_MODULE(ami),
#endif /* USE_AMI */

#ifdef USE_VME
	INIT_MODULE(vme),
#endif /* USE_VME */

#ifdef USE_VCS
	INIT_MODULE(vcs)
#endif /* USE_VCS */
};

/*
 * A hook for "quit()".
 *
 * Close down, then fall back into "quit()".
 */
static void quit_hook(cptr s)
{
	int j;

	/* Unused parameter */
	(void)s;

	/* Scan windows */
	for (j = ANGBAND_TERM_MAX - 1; j >= 0; j--)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Nuke it */
		term_nuke(angband_term[j]);
	}
}



/*
 * Set the stack size (for the Amiga)
 */
#ifdef AMIGA
#include <dos.h>
__near long __stack = 32768L;
#endif /* AMIGA */


/*
 * Set the stack size and overlay buffer (see main-286.c")
 */
#ifdef USE_286
#include <dos.h>
extern unsigned _stklen = 32768U;
extern unsigned _ovrbuffer = 0x1500;
#endif /* USE_286 */


/*
 * Initialize and verify the file paths, and the score file.
 *
 * Use the ANGBAND_PATH environment var if possible, else use
 * DEFAULT_PATH, and in either case, branch off appropriately.
 *
 * First, we'll look for the ANGBAND_PATH environment variable,
 * and then look for the files in there.  If that doesn't work,
 * we'll try the DEFAULT_PATH constant.  So be sure that one of
 * these two things works...
 *
 * We must ensure that the path ends with "PATH_SEP" if needed,
 * since the "init_file_paths()" function will simply append the
 * relevant "sub-directory names" to the given path.
 *
 * Note that the "path" must be "Angband:" for the Amiga, and it
 * is ignored for "VM/ESA", so I just combined the two.
 *
 * Make sure that the path doesn't overflow the buffer.  We have
 * to leave enough space for the path separator, directory, and
 * filenames.
 */
static void init_stuff(void)
{
	char path[1024];
	
	int len = 0;

#if defined(AMIGA) || defined(VM)

	/* Hack -- prepare "path" */
	strnfmt(path, 511, "Angband:");

#else  /* AMIGA / VM */

	cptr tail = NULL;
	
	path[0] = 0;

#ifndef FIXED_PATHS

	/* Get the environment variable */
	tail = getenv("ANGBAND_PATH");

#endif /* FIXED_PATHS */

	/* Use the angband_path, or a default */
	strnfcat(path, 511, &len, "%s", tail ? tail : DEFAULT_PATH);

	/* Hack -- Add a path separator (only if needed) */
	if (!suffix(path, PATH_SEP)) strnfcat(path, 511, &len, PATH_SEP);

#endif /* AMIGA / VM */

#ifdef HAVE_CONFIG_H
	{
		/*
		 * XXX XXX Hack - fall back to './lib/' if DEFAULT_PATH doesn't work.
		 * Autoconf sets the default installation directory to be in
		 * /usr/local/share/games/zangband/lib/ however, for development, that sucks.
		 */
		
		char buf[1024];
		
		int fd = -1;
		
		/* Look for "news" file - see init2.c */
		path_make(buf, path, "file/news.txt");
		
		fd = fd_open(buf, O_RDONLY);

		/* Failure */
		if (fd < 0)
		{
			/* plog_fmt("Cannot access the '%s' file!", buf); */
		
			/* Reset to be "./lib/" */
			strcpy(path, "./lib/");	
		}
		
		fd_close(fd);
	}
#endif /* HAVE_CONFIG_H */

	/* Initialize */
	init_file_paths(path);
}



/*
 * Handle a "-d<what>=<path>" option
 *
 * The "<what>" can be any string starting with the same letter as the
 * name of a subdirectory of the "lib" folder (i.e. "i" or "info").
 *
 * The "<path>" can be any legal path for the given system, and should
 * not end in any special path separator (i.e. "/tmp" or "~/.ang-info").
 */
static void change_path(cptr info)
{
	cptr s;

	/* Find equal sign */
	s = strchr(info, '=');

	/* Verify equal sign */
	if (!s) quit_fmt("Try '-d<what>=<path>' not '-d%s'", info);

	/* Analyze */
	switch (tolower(info[0]))
	{
#ifndef FIXED_PATHS
		case 'a':
		{
			string_free(ANGBAND_DIR_APEX);
			ANGBAND_DIR_APEX = string_make(s + 1);
			break;
		}

		case 'f':
		{
			string_free(ANGBAND_DIR_FILE);
			ANGBAND_DIR_FILE = string_make(s + 1);
			break;
		}

		case 'h':
		{
			string_free(ANGBAND_DIR_HELP);
			ANGBAND_DIR_HELP = string_make(s + 1);
			break;
		}

		case 'i':
		{
			string_free(ANGBAND_DIR_INFO);
			ANGBAND_DIR_INFO = string_make(s + 1);
			break;
		}

		case 'x':
		{
			string_free(ANGBAND_DIR_XTRA);
			ANGBAND_DIR_XTRA = string_make(s + 1);
			break;
		}

		case 'b':
		{
			string_free(ANGBAND_DIR_BONE);
			ANGBAND_DIR_BONE = string_make(s + 1);
			break;
		}

		case 'd':
		{
			string_free(ANGBAND_DIR_DATA);
			ANGBAND_DIR_DATA = string_make(s + 1);
			break;
		}

		case 'e':
		{
			string_free(ANGBAND_DIR_EDIT);
			ANGBAND_DIR_EDIT = string_make(s + 1);
			break;
		}

		case 's':
		{
			string_free(ANGBAND_DIR_SAVE);
			ANGBAND_DIR_SAVE = string_make(s + 1);
			break;
		}

		case 'z':
		{
			string_free(ANGBAND_DIR_SCRIPT);
			ANGBAND_DIR_SCRIPT = string_make(s + 1);
			break;
		}

#endif /* FIXED_PATHS */

		case 'u':
		{
			string_free(ANGBAND_DIR_USER);
			ANGBAND_DIR_USER = string_make(s + 1);
			break;
		}

		default:
		{
			quit_fmt("Bad semantics in '-d%s'", info);
		}
	}
}

/*
 * The default message to print when we get bad input.
 */
static void game_usage(void)
{
	int i, j;

	/* Dump usage information */
	puts("Usage: angband [options] [-- subopts]");
	puts("  -n       Start a new character");
	puts("  -f       Request fiddle (verbose) mode");
	puts("  -w       Request wizard mode");
	puts("  -v       Request sound mode");
	puts("  -g       Request graphics mode");
	puts("  -t       Request bigtile mode");
	puts("  -o       Request original keyset (default)");
	puts("  -r       Request rogue-like keyset");
	puts("  -M       Request monochrome mode");
	puts("  -s<num>  Show <num> high scores (default 10)");
	puts("  -u<who>  Use your <who> savefile");
#ifdef FIXED_PATHS
	puts("  -du=<dir>  Define user dir path");
#else /* FIXED_PATHS */
	puts("  -d<def>  Define a 'lib' dir sub-path");
#endif /* FIXED_PATHS */

	/* Print the name and help for each available module */
	for (i = 0; i < (int)NUM_ELEMENTS(modules); i++)
	{
		/* Spacer */
		puts("");

		for (j = 0; modules[i].help[j]; j++)
		{
			if (j)
			{
				/* Display seperator */
				if (j == 1)
				{
					puts("  --       Sub options");
				}

				puts(format("  -- %s", modules[i].help[j]));
			}
			else
			{
				/* The first line is special */
				puts(format("  -m%s    %s",
							modules[i].name, modules[i].help[j]));
			}
		}
	}

	/* Actually abort the process */
	quit(NULL);
}


/*
 * Simple "main" function for multiple platforms.
 *
 * Note the special "--" option which terminates the processing of
 * standard options.  All non-standard options (if any) are passed
 * directly to the "init_xxx()" function.
 */
int main(int argc, char *argv[])
{
	int i;

	bool done = FALSE;

	bool new_game = FALSE;

	int show_score = 0;

	cptr mstr = NULL;

	bool args = TRUE;


	/* Save the "program name" XXX XXX XXX */
	argv0 = argv[0];


#ifdef USE_286
	/* Attempt to use XMS (or EMS) memory for swap space */
	if (_OvrInitExt(0L, 0L))
	{
		_OvrInitEms(0, 0, 64);
	}
#endif /* USE_286 */

	/* Get the file paths */
	init_stuff();
	
	/* Catch nasty signals */
	signals_init();

	/* Drop permissions and initialize multiuser stuff */
	init_setuid();

	/* Process the command line arguments */
	for (i = 1; args && (i < argc); i++)
	{
		/* Require proper options */
		if (argv[i][0] != '-') game_usage();

		/* Analyze option */
		switch (argv[i][1])
		{
			case 'N':
			case 'n':
			{
				new_game = TRUE;
				break;
			}

			case 'F':
			case 'f':
			{
				arg_fiddle = TRUE;
				break;
			}

			case 'W':
			case 'w':
			{
				arg_wizard = TRUE;
				break;
			}

			case 'V':
			case 'v':
			{
				arg_sound = TRUE;
				break;
			}

			case 'G':
			case 'g':
			{
				arg_graphics = TRUE;
				break;
			}
			
			case 'T':
			case 't':
			{
				arg_bigtile = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				arg_force_roguelike = TRUE;
				break;
			}

			case 'O':
			case 'o':
			{
				arg_force_original = TRUE;
				break;
			}

			case 'S':
			case 's':
			{
				show_score = atoi(&argv[i][2]);
				if (show_score <= 0) show_score = 10;
				break;
			}

			case 'u':
			case 'U':
			{
				if (!argv[i][2]) game_usage();

				/* Get the savefile name */
				strncpy(player_name, &argv[i][2], 32);

				/* Make sure it's terminated */
				player_name[31] = '\0';
				break;
			}

			case 'm':
			{
				if (!argv[i][2]) game_usage();
				mstr = &argv[i][2];
				break;
			}

			case 'M':
			{
				arg_monochrome = TRUE;
				break;
			}

			case 'd':
			case 'D':
			{
				change_path(&argv[i][2]);
				break;
			}

			case '-':
			{
				argv[i] = argv[0];
				argc = argc - i;
				argv = argv + i;
				args = FALSE;
				break;
			}

			default:
			{
				/* Default usage-help */
				game_usage();
			}
		}
	}

	/* Hack -- Forget standard args */
	if (args)
	{
		argc = 1;
		argv[1] = NULL;
	}


	/* Process the player name */
	process_player_name(TRUE);



	/* Install "quit" hook */
	quit_aux = quit_hook;

	for (i = 0; i < (int)NUM_ELEMENTS(modules); i++)
	{
		if (!mstr || (streq(mstr, modules[i].name)))
		{
			/* Try to use port */
			if (0 == modules[i].init(argc, argv, (unsigned char *)&new_game))
			{
				/* Set port name */
				ANGBAND_SYS = modules[i].name;
				done = TRUE;
				break;
			}
		}
	}

	/* Make sure we have a display! */
	if (!done) quit("Unable to prepare any 'display module'!");

	/* Gtk and Tk initialise earlier */
	if (!(streq(ANGBAND_SYS, "gtk") || streq(ANGBAND_SYS, "tnb")))
	{
		/* Initialize */
		init_angband();
	}

	/* Hack -- If requested, display scores and quit */
	if (show_score > 0) display_scores(0, show_score);

	/* Wait for response */
	pause_line(23);

	/* Play the game */
	play_game(new_game);

	/* Free resources */
	cleanup_angband();

	/* Quit */
	quit(NULL);

	/* Exit */
	return (0);
}

#endif /* !defined(MACINTOSH) && !defined(WINDOWS) && !defined(ACORN) */
