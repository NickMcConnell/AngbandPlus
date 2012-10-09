/* File: main.c */
/* lclint-2.4b clean 120798 */

/* Purpose: initialization, main() function and main loop */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */

#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(ACORN)

#ifdef SET_UID

/*
 * Check "wizard permissions"
 */
static bool is_wizard(s16b uid)
{
   bool     allow = FALSE;

   char     buf[1024];


   /* Access the "wizards.txt" file */
   strncpy(buf, ANGBAND_DIR_FILE, 1023);
   buf[1023]='\0';
   strcat(buf, "wizards.txt");

   /* Open the wizard file */
   wizardfile = my_fopen(buf, "r");

   /* No file, allow everyone */
   if (!wizardfile) return (TRUE);

   /* Scan the wizard file */
   while (0 == my_fgets(wizardfile, buf, 1024))
   {
      int test;

      /* Skip comments and blank lines */
      if ((buf[0]==(char)0) || (buf[0] == '#')) continue;

      /* Look for valid entries */
      if (sscanf(buf, "%d", &test) != 1) continue;

      /* Look for matching entries */
      if (test == uid) allow = TRUE;

      /* Done */
      if (allow) break;
   }

   /* Close the file */
   (void)my_fclose(wizardfile);

   /* Result */
   return (allow);
}

#endif

/*
 * A hook for "quit()".
 *
 * Close down, then fall back into "quit()".
 */
static void quit_hook(/*@unused@*/ cptr s)
{
   /* Shut down the term windows */
   int j;

   /* Scan windows */
   for (j = 8 - 1; j >= 0; j--)
   {
     /* Unused */
     if (!angband_term[j]) continue;

     /* Nuke it */
     (void)term_nuke(angband_term[j]);
   }
}

/*
 * Set the stack size (for the Amiga)
 */
#ifdef AMIGA
# include <dos.h>
__near long __stack = 32768L;
#endif

/*
 * Set the stack size and overlay buffer (see main-286.c")
 *
 * XXX XXX XXX Note that "<dos.h>" defines the "delay()" function.
 */
#ifdef USE_286
# include <dos.h>
extern unsigned _stklen = 32768U;
extern unsigned _ovrbuffer = 0x1500;
#endif

/*
 * Initialize and verify the file paths, and the score file.
 *
 * Use the ANGBAND64_PATH environment var if possible, else use
 * DEFAULT_PATH, and in either case, branch off appropriately.
 *
 * First, we'll look for the ANGBAND64_PATH environment variable,
 * and then look for the files in there.  If that doesn't work,
 * we'll try the DEFAULT_PATH constant.  So be sure that one of
 * these two things works...
 *
 * We must ensure that the path ends with "PATH_SEP" if needed.
 *
 * Note that the "path" must be "Angband:" for the Amiga, and it
 * is ignored for "VM/ESA", so I just combined the two.
 */
static void init_stuff(void)
{
   char path[1024];

#if defined(AMIGA) || defined(VM)
   /* Hack -- prepare "path" */
   strncpy(path, "Angband:", 1023);
   path[1023]='\0';

#else /* AMIGA / VM */

   cptr tail;

   /* Get the environment variable */
   tail = getenv("ANGBAND64_PATH");

   /* Use the angband_path, or a default */
   strncpy(path, tail ? tail : DEFAULT_PATH, 1023);
   path[1023]='\0';

   /* Hack -- Add a path separator (only if needed) */
   if (!suffix(path, PATH_SEP)) strcat(path, PATH_SEP);

#endif /* AMIGA / VM */

   /* Initialize */
   init_file_paths(path);
}

/*
 * test and set the debug-flags present in buf
 */
static s16b test_debug_flags(char *buf)
{
   char *s, *t;
   s16b i;

   /* Parse every entry textually */
   for (s = buf ; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ',') && (*t != ' '); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
          *t++ = '\0';
          while (*t == ',') t++;
      }

      /* Parse this entry */
      /* Scan flags1 */
      for (i = 0; i < 32; i++)
      {
         if (streq(s, debug_flag[i].name))
         {
            debuglevel |= (1L << i);
            break;
         }
      }

       /* Start the next entry */
       s = t;
   }
   return(0);
}


/*
 * Studly machines can actually parse command line args
 *
 * XXX XXX XXX The "-c", "-d", and "-i" options should probably require
 * that their "arguments" end in the appropriate path separator.  But it
 * is possible to use them without a path separator, which provides a
 * special "prefix" for all of the file names.  Those options should
 * probably be simplified into some form of "-dWHAT=PATH" syntax.
 */
int main(int argc, char *argv[])
{
   bool done = FALSE;

   bool new_game = FALSE;

   s16b show_score = 0;

   cptr use_module = NULL;

/* jk - we cant use a term until further on */
   term_initialized=FALSE;

/* jk - this is the absolute start of the program.. */

   /* Save the "program name" */
   argv0 = argv[0];

#ifdef USE_286
   /* Attempt to use XMS (or EMS) memory for swap space */
   if (_OvrInitExt(0L, 0L))
   {
      _OvrInitEms(0,0,64);
   }
#endif

#ifdef SET_UID

   /* Default permissions on files */
   (void)umask(022);

# ifdef SECURE
   /* Authenticate */
   Authenticate();
# endif

#endif

   /* Get the file paths */
   init_stuff();

#ifdef SET_UID

   /* Get the user id (?) */
   player_uid = (s16b)getuid();

#ifdef VMS
   /* Mega-Hack -- Factor group id */
   player_uid += (getgid() * 1000);
#endif

# ifdef SAFE_SETUID

#  ifdef _POSIX_SAVED_IDS

   /* Save some info for later */
   player_euid = (s16b)geteuid();
   player_egid = (s16b)getegid();

#  endif

# endif

#endif

   /* Assume "Wizard" permission */
   can_be_wizard = TRUE;

#ifdef SET_UID

   /* Check for "Wizard" permission */
   can_be_wizard = is_wizard(player_uid);

   /* Initialize the "time" checker */
   if ((check_time_init()!=0) || (check_time()!=0))
   {
      quit("The gates to Angband are closed (bad time).");
   }

   /* Initialize the "load" checker */
   if ((check_load_init()!=0) || (check_load()!=0))
   {
      quit("The gates to Angband are closed (bad load).");
   }


   /* Acquire the "user name" as a default player name */
   user_name(player_name, player_uid);

#endif
/* jk */
   force_templates=FALSE;
   read_options=FALSE;

   /* Process the command line arguments */
   for (--argc, ++argv; argc > 0; --argc, ++argv)
   {
      /* Require proper options */
      if (argv[0][0] != '-') goto usage;

      /* Analyze option */
      switch (argv[0][1])
      {
         case 'c':
         case 'C':
            ANGBAND_DIR_USER = &argv[0][2];
            break;

#ifndef VERIFY_SAVEFILE
         case 'd':
         case 'D':
            ANGBAND_DIR_SAVE = &argv[0][2];
            break;
#endif

         case 'G':
         case 'g':
            arg_graphics = TRUE;
            break;

         case 'i':
         case 'I':
            ANGBAND_DIR_INFO = &argv[0][2];
            break;

/* jk */
         case 'l':
         case 'L':
            if (argv[0][2]==(char)0) goto usage;
            strncpy (ANGBAND_OPTION_FILE,&argv[0][2], 1023);
            ANGBAND_OPTION_FILE[1023]='\0';
            read_options=TRUE;
         break;

         case 'm':
         case 'M':
         {
            if (!argv[0][2]) goto usage;
            use_module = &argv[0][2];
            break;
         }

         case 'N':
         case 'n':
            new_game = TRUE;
            break;

         case 'O':
         case 'o':
            arg_force_original = TRUE;
            break;

#ifdef SET_UID
         case 'P':
         case 'p':
            if (can_be_wizard)
            {
               player_uid = (s16b)atoi(&argv[0][2]);
               user_name(player_name, player_uid);
            }
            break;
#endif
         case 'Q':
         case 'q':
            if ( (argv[0][2]=='?') && (argv[0][3]=='2') )
            {
               s16b i;

               puts("options 17-32 for -q (example: -qSAVE,ITEMS,EXTRA)");
               for (i=16; i < 32; i++)
               {
                  puts(format("%-6s %s", debug_flag[i].name, debug_flag[i].explanation));
               }
               puts("incorrect flags will be silently ignored.");
               puts("** be careful with these, they may give a 100 megabyte+ logfile with a few moves.");
               quit("end of -q?2, use -q? to get the first 16 flags output");
            }
            else if (argv[0][2]=='?')
            {
               s16b i;

               puts("options 1-16 for -q (example: -qSAVE,ITEMS,EXTRA)");
               for (i=0; i < 16; i++)
               {
                  puts(format("%-6s %s", debug_flag[i].name, debug_flag[i].explanation));
               }
               puts("incorrect flags will be silently ignored.");
               puts("** be careful with these, they may give a 100 megabyte+ logfile with a few moves.");
               quit("end of -q?, use -q?2 to get the last 16 flags output");
            }
            else
            {
               (void)test_debug_flags(&argv[0][2]);
            }
            break;

         case 'R':
         case 'r':
            arg_force_roguelike = TRUE;
            break;

         case 'S':
         case 's':
            show_score = (s16b)atoi(&argv[0][2]);
            if (show_score <= 0) show_score = 10;
            break;

         case 't':
         case 'T':
            force_templates=TRUE;
            break;

         case 'u':
         case 'U':
            if (argv[0][2]==(char)0) goto usage;
            strncpy(player_name, &argv[0][2],31);
            player_name[31]='\0';
            break;

         case 'V':
         case 'v':
            arg_sound = TRUE;
            break;

         case 'W':
         case 'w':
            if (can_be_wizard) arg_wizard = TRUE;
            break;

         case 'x':
         case 'X':
            if (argv[0][2]==(char)0)
               errlog=NULL;
            else
            {
               strncpy (ANGBAND_ERRLOG_FILE,&argv[0][2], 1023);
               ANGBAND_ERRLOG_FILE[1023]='\0';
               errlog = my_fopen(ANGBAND_ERRLOG_FILE,"a");
               if (!errlog)
               {
                  quit(format("Failed to open log file %s",
                              ANGBAND_ERRLOG_FILE));
               }
               dlog(DEBUGALWAYS,"Angband/64 started\n");
            }
         break;

         default:
         usage:
            /* Note -- the Term is NOT initialized */

            (void)puts("Usage: angband [options]");
            (void)puts("  -c<path> Look for pref files in the directory <path>");
            (void)puts("  -d<path> Look for save files in the directory <path>");
            (void)puts("  -g       Activate the arg_graphics flag");
            (void)puts("  -i<path> Look for info files in the directory <path>");
            (void)puts("  -l<name> Load user-defined options from lib/xtra/<name>");
            (void)puts("  -m<sys>  Force the use of the 'main-<sys>.c' interface module");
            (void)puts("  -n       Start a new character");
            (void)puts("  -o       Use the original keyset");
            (void)puts("  -p<uid>  Play with the <uid> userid");
            (void)puts("  -q<str,str,...>  Sets the debug-flagset. Use -q? to get detailed help.");
            (void)puts("  -r       Use the rogue-like keyset");
            (void)puts("  -s<num>  Show <num> high scores (or top 10).");
            (void)puts("  -t       Force reloading of templates");
            (void)puts("  -u<name> Play with <name> savefile");
            (void)puts("  -v       Activate the arg_sound flag");
            (void)puts("  -w       Activate 'wizard' mode");
            (void)puts("  -x<name> Write debug/error-log to file <name>");
            (void)puts("           use -x without a name to stop writing this log!");
            (void)puts("not all options are supported for the win32-target, check angband.ini!");

            /* Actually abort the process */
            quit("Error: arguments wrong");
      }
   }

#ifdef ALWAYS_LOAD_TEMPLATES
   force_templates = TRUE;
#endif

   /* Process the player name */
   process_player_name(TRUE);

dlog(DEBUGSAVE,"main.c: main: process_player_name called, savefile %s\n", savefile);
   /* Drop privs (so X11 will work correctly) */
   safe_setuid_drop();

#ifdef USE_XAW
   /* Attempt to use the "main-xaw.c" support */
   if (!done && (!use_module || (streq(use_module, "xaw"))))
   {
     extern errr init_xaw(int, char**);
     if (0 == init_xaw(argc, argv)) done=TRUE;
     if (done) ANGBAND_SYS = "xaw";
   }
#endif

#ifdef USE_X11
   /* Attempt to use the "main-x11.c" support */
   if (!done && (!use_module || (streq(use_module, "x11"))))
   {
      extern errr init_x11(int, char**);
      if (0 == init_x11(argc, argv)) done = TRUE;
      if (done) ANGBAND_SYS = "x11";
   }
#endif

#ifdef USE_GCU
   /* Attempt to use the "main-gcu.c" support */
   if (!done && (!use_module || (streq(use_module, "gcu"))))
   {
      extern errr init_gcu(int, char**);
      if (0 == init_gcu(argc, argv)) done = TRUE;
      if (done) ANGBAND_SYS = "gcu";
   }
#endif

#ifdef USE_PDC
   /* Attempt to use the "main-pdc.c" support */
   if (!done && (!use_module || (streq(use_module, "pdc"))))
   {
      extern errr init_pdc(int, char**);
      if (0 == init_gcu(argc, argv)) done = TRUE;
      if (done) ANGBAND_SYS = "pdc";
   }
#endif


#ifdef USE_CAP
   /* Attempt to use the "main-cap.c" support */
   if (!done && (!use_module || (streq(use_module, "cap"))))
   {
      extern errr init_cap(int, char**);
      if (0 == init_cap(argc, argv)) done = TRUE;
      if (done) ANGBAND_SYS = "cap";
   }
#endif

#ifdef USE_IBM
   /* Attempt to use the "main-ibm.c" support */
   if (!done && (!use_module || (streq(use_module, "ibm"))))
   {
      extern errr init_ibm(void);
      if (0 == init_ibm()) done = TRUE;
      if (done) ANGBAND_SYS = "ibm";
   }
#endif

#ifdef USE_DOS
   /* Attempt to use the "main-dos.c" support */
   if (!done && (!use_module || (streq(use_module, "dos"))))
   {
      extern errr init_dos(void);
      if (0 == init_dos()) done = TRUE;
      if (done) ANGBAND_SYS = "dos";
   }
#endif

#ifdef __EMX__
   /* Attempt to use the "main-emx.c" support */
   if (!done && (!use_module || (streq(use_module, "emx"))))
   {
      extern errr init_emx(void);
      if (0 == init_emx()) done = TRUE;
      if (done) ANGBAND_SYS = "emx";
   }
#endif

#ifdef USE_SLA
   /* Attempt to use the "main-sla.c" support */
   if (!done && (!use_module || (streq(use_module, "sla"))))
   {
      extern errr init_sla(void);
      if (0 == init_sla()) done = TRUE;
      if (done) ANGBAND_SYS = "sla";
   }
#endif

#ifdef USE_LSL
   /* Attempt to use the "main-lsl.c" support */
   if (!done && (!use_module || (streq(use_module, "lsl"))))
   {
      extern errr init_lsl(void);
      if (0 == init_lsl()) done = TRUE;
      if (done) ANGBAND_SYS = "lsl";
   }
#endif

#ifdef USE_AMI
   /* Attempt to use the "main-ami.c" support */
   if (!done && (!use_module || (streq(use_module, "ami"))))
   {
      extern errr init_ami(void);
      if (0 == init_ami()) done = TRUE;
      if (done) ANGBAND_SYS = "ami";
   }
#endif


#ifdef USE_VME
   /* Attempt to use the "main-vme.c" support */
   if (!done && (!use_module || (streq(use_module, "vme"))))
   {
      extern errr init_vme(void);
      if (0 == init_vme()) done = TRUE;
      if (done) ANGBAND_SYS = "vme";
   }
#endif

   /* Grab privs (dropped above for X11) */
   safe_setuid_grab();

   /* Make sure we have a display! */
   if (!done) quit("Unable to prepare any 'display module'!");

/* jk - from now on, we can use a term */
   term_initialized=TRUE;

dlog(DEBUGFLOW,"main.c: term initializes as %s\n", ANGBAND_SYS);
   /* Tell "quit()" to call "Term_nuke()" */
   quit_aux = quit_hook;

   /* If requested, display scores and quit */
   if (show_score > 0) display_scores(0, show_score);

   /* Catch nasty signals */
   signals_init();
dlog(DEBUGFLOW,"main.c: signal initializes\n");

   /* Display the 'news' file */
   init_angband();

   /* Wait for response */
   pause_line(23);

   /* Play the game */
   play_game(new_game);

   /* Quit */
   quit(NULL);
   return(0);
}

#endif




