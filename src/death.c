/* death.c: code executed when player dies

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

/* some incorrectly define NULL as integer constant, so load this before
   local includes */
#include <stdio.h>
#include <signal.h>
#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"

#ifdef Pyramid
#include <sys/time.h>
#else
#include <time.h>
#endif

#include <ctype.h>

#ifndef USG
/* only needed for Berkeley UNIX */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#endif

#ifdef MSDOS
#include <io.h>
#else
#if !defined(ATARIST_MWC) && !defined(MAC)
#ifndef VMS
#include <pwd.h>
#else
#include <file.h>
#endif
#endif
#endif

#ifdef USG
#ifndef ATARIST_MWC
#include <string.h>
#include <fcntl.h>
#endif
#else
#include <strings.h>
#endif

#ifndef MIN
#define MIN(a, b)	((a < b) ? a : b)
#endif

#ifndef BSD4_3
long lseek();
#else
off_t lseek();
#endif

#if defined(USG) || defined(VMS)
#ifndef L_SET
#define L_SET 0
#endif
#endif

#ifndef VMS
#ifndef MAC
#if defined(ultrix) || defined(USG)
void perror();
void exit ();
#endif
#endif
#endif

#ifndef MAC
#ifdef SYS_V
struct passwd *getpwuid();
#endif
#endif

#if defined(LINT_ARGS)
static void date(char *);
static char *center_string(char *, char *);
static void print_tomb(void);
static void kingly(void);
#endif

#ifndef MAC
char *getlogin();
#ifndef ATARIST_MWC
long time();
#endif
#endif

static void date(day)
char *day;
{
  register char *tmp;
  long clock;

  clock = time((long *) 0);
  tmp = ctime(&clock);
  tmp[10] = '\0';
  (void) strcpy(day, tmp);
}

/* Centers a string within a 31 character string		-JWT-	 */
static char *center_string(centered_str, in_str)
char *centered_str;
char *in_str;
{
  register int i, j;

  i = strlen(in_str);
  j = 15 - i/2;
  (void) sprintf (centered_str, "%*s%s%*s", j, "", in_str, 31 - i - j, "");
  return centered_str;
}


/* Not touched for Mac port */
void display_scores(from, to)
  int from, to;
{
  return; /* This seems REAL weird, but scorefiles no longer exist---why
	     bother printing them? */
}

/* Pauses for user response before returning		-RAK-	*/
int look_line(prt_line)
  int prt_line;
{
  prt("[Press ESC to quit, any other key to continue.]", prt_line, 17);
  if (inkey() == ESCAPE) {
    erase_line(prt_line, 0);
    return 0;
  } else {
    erase_line(prt_line, 0);
    return 1;
  }
}



/* Prints the gravestone of the character		-RAK-	 */
static void print_tomb()
{
  vtype str, tmp_str;
  register int i;
  char day[11];
  register char *p;
  FILE *fp;

  if (strcmp(died_from, "Interrupting") && !wizard) {
    sprintf(str, "%s%d", ANGBAND_BONES, dun_level);
    if ((fp = (FILE *)my_tfopen(str, "r")) == NULL && (dun_level>1)) {
      if ((fp = (FILE *)my_tfopen(str, "w")) != NULL) {
	(void) fchmod(fileno(fp), 0666);
	fprintf(fp, "%s\n%d\n%d\n", 
		py.misc.name, py.misc.mhp, py.misc.prace);
	fclose(fp);
      }
    } else {
      fclose(fp);
    }
  }
  clear_screen();
  put_buffer ("_______________________", 1, 15);
  put_buffer ("/", 2, 14);
  put_buffer ("\\         ___", 2, 38);
  put_buffer ("/", 3, 13);
  put_buffer ("\\ ___   /   \\      ___", 3, 39);
  put_buffer ("/            RIP            \\   \\  :   :     /   \\", 4, 12);
  put_buffer ("/", 5, 11);
  put_buffer ("\\  : _;,,,;_    :   :", 5, 41);
  (void) sprintf (str, "/%s\\,;_          _;,,,;_",
		  center_string (tmp_str, py.misc.name));
  put_buffer (str, 6, 10);
  put_buffer ("|               the               |   ___", 7, 9);
  if (!total_winner)
    p = title_string ();
  else
    p = "Magnificent";
  (void) sprintf (str, "| %s |  /   \\", center_string (tmp_str, p));
  put_buffer (str, 8, 9);
  put_buffer ("|", 9, 9);
  put_buffer ("|  :   :", 9, 43);
  if (!total_winner)
    p = "";
  else if (py.misc.male)
    p = "*King*";
  else
    p = "*Queen*";
  (void) sprintf(str,"| %s | _;,,,;_   ____", center_string (tmp_str, p));
  put_buffer (str, 10, 9);
  (void) sprintf (str, "Average Level : %d", (int) get_level());
  (void) sprintf (str,"| %s |          /    \\", center_string (tmp_str, str));
  put_buffer (str, 11, 9);
  (void) sprintf(str, "%ld Exp", py.misc.exp);
  (void) sprintf(str,"| %s |          :    :", center_string (tmp_str, str));
  put_buffer (str, 12, 9);
  (void) sprintf(str, "%ld Au", py.misc.au);
  (void) sprintf(str,"| %s |          :    :", center_string (tmp_str, str));
  put_buffer (str, 13, 9);
  (void) sprintf(str, "Died on Level : %d", dun_level);
  (void) sprintf(str,"| %s |         _;,,,,;_", center_string (tmp_str, str));
  put_buffer (str, 14, 9);
  put_buffer ("|            killed by            |", 15, 9);
  p = died_from;
  i = strlen (p);
  p[i] = '.';  /* add a trailing period */
  p[i+1] = '\0';
  (void) sprintf(str, "| %s |", center_string (tmp_str, p));
  put_buffer (str, 16, 9);
  p[i] = '\0';	 /* strip off the period */
  date(day);
  (void) sprintf(str, "| %s |", center_string (tmp_str, day));
  put_buffer (str, 17, 9);
  put_buffer ("*|   *     *     *    *   *     *  | *", 18, 8);
  put_buffer ("________)/\\\\_)_/___(\\/___(//_\\)/_\\//__\\\\(/_|_)_______",
	      19, 0);

 retry:
  flush();
  put_buffer ("(ESC to abort, return to print on screen)", 23, 0);
  put_buffer ("Character record?", 22, 0);
  if (get_string (str, 22, 18, 60))
    {
      for (i = 0; i < INVEN_ARRAY_SIZE; i++)
	{
	  known1(&inventory[i]);
	  known2(&inventory[i]);
	}
      calc_bonuses ();
      clear_screen ();
      display_char ();
      put_buffer ("Type ESC to skip the inventory:", 23, 0);
      if (inkey() != ESCAPE)
	{
	  clear_screen ();
	  msg_print ("You are using:");
	  (void) show_equip (TRUE, 0);
	  msg_print ("You are carrying:");
	  clear_from (1);
	  (void) show_inven (0, inven_ctr-1, TRUE, 0, 0);
	  msg_print (NULL);
	}
    }
}


/* Calculates the total number of points earned		-JWT-	 */
long total_points()
{
  return (py.misc.max_exp + (100 * py.misc.max_dlv));
}



/* Enters a players name on a hi-score table...    SM */
static int top_twenty()
{
  return;
}

/* Enters a players name on the hi-score table     SM	 */
delete_entry(which)
  int which;
{
  return;
}

/* Change the player into a King!			-RAK-	 */
static void kingly()
{
  register struct misc *p_ptr;
  register char *p;

  /* Change the character attributes.		 */
  dun_level = 0;
  (void) strcpy(died_from, "Ripe Old Age");
  p_ptr = &py.misc;
  (void) restore_level ();
  p_ptr->lev += MAX_PLAYER_LEVEL;
  p_ptr->au += 250000L;
  p_ptr->max_exp += 5000000L;
  p_ptr->exp = p_ptr->max_exp;

  /* Let the player know that he did good.	 */
  clear_screen();
  put_buffer("#", 1, 34);
  put_buffer("#####", 2, 32);
  put_buffer("#", 3, 34);
  put_buffer(",,,  $$$  ,,,", 4, 28);
  put_buffer(",,=$   \"$$$$$\"   $=,,", 5, 24);
  put_buffer(",$$        $$$        $$,", 6, 22);
  put_buffer("*>         <*>         <*", 7, 22);
  put_buffer("$$         $$$         $$", 8, 22);
  put_buffer("\"$$        $$$        $$\"", 9, 22);
  put_buffer("\"$$       $$$       $$\"", 10, 23);
  p = "*#########*#########*";
  put_buffer(p, 11, 24);
  put_buffer(p, 12, 24);
  put_buffer("Veni, Vidi, Vici!", 15, 26);
  put_buffer("I came, I saw, I conquered!", 16, 21);
  if (p_ptr->male)
    put_buffer("All Hail the Mighty King!", 17, 22);
  else
    put_buffer("All Hail the Mighty Queen!", 17, 22);
  flush();
  pause_line(23);
}


/* Handles the gravestone end top-twenty routines	-RAK-	 */
void exit_game ()
{
  register int i;

#ifdef MAC
  /* Prevent strange things from happening */
  enablefilemenu(FALSE);
#endif

  /* What happens upon dying.				-RAK-	 */
  msg_print(NULL);
  flush ();  /* flush all input */
  nosignals ();	 /* Can't interrupt or suspend. */
  if (turn >= 0)
    {
      if (total_winner)
	kingly();
      print_tomb();
      if (!wizard && !to_be_wizard) 
	top_twenty();
      else msg_print("Score not registered.");
    }
  i = log_index;
  (void) save_char ();		/* Save the memory at least. */
  if (i > 0)
    display_scores (0, 10);
  erase_line (23, 0);
  restore_term();
  exit(0);
}
