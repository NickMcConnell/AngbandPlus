/* File: cmd4.c */

/* Purpose: Interface commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all of the
 * interesting updates and redraws that I can think of.
 */
void do_cmd_redraw(void)
{
   s16b j;

   term *old = Term;

   /* Hack -- react to changes */
   Term_xtra(TERM_XTRA_REACT, 0);

   /* Combine and Reorder the pack (later) */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Update torch */
   p_ptr->update |= (PU_TORCH);

   /* Update stuff */
   p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

   /* Forget lite/view/monsters */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

   /* Redraw everything */
   p_ptr->redraw1 |= (PR1_WIPE | PR1_BASIC | PR1_EXTRA | PR1_MAP);
   p_ptr->redraw2 |= PR2_EQUIPPY;

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD | PW_MESSAGE | PW_MONSTER | PW_OBJECT);

   Term_clear();

   /* Hack -- update */
   handle_stuff();

   /* Hack -- refresh */
   Term_fresh();

   /* Redraw every window */
   for (j = 0; j < 8; j++)
   {
      /* Dead window */
      if (!angband_term[j]) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Redraw */
      Term_redraw();

      /* Refresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}

/*
 * Hack -- change name
 */
void do_cmd_change_name(void)
{
   char        c;
   bool        flag;
   s16b        mode = 0;
   char        tmp[160];

   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Get command */
   for (flag = FALSE; !flag; )
   {
      /* Display the player */
      display_player(mode, TRUE);

      /* Prompt */

      if (mode==0)
      {
         Term_putstr(1, 23, -1, TERM_WHITE,
            "[change <n>ame/<tT>actic/<m>ode/<eE>xploration, dump <f>ile or ESC to leave]");
      }
      else
      {
         Term_putstr(1, 23, -1, TERM_WHITE,
            "[<c>hange name, dump to <f>ile, change <m>ode or ESC to leave]");
      }

      /* Query */
      c = inkey();

      /* Handle */
      switch (c)
      {
         case 'N':
         case 'n':
            get_name();
            flag = TRUE;
            break;

         case 'F':
         case 'f':
            sprintf(tmp, "%s.txt", player_base);
            if (get_string("File name: ", tmp, 80))
            {
               if (tmp[0] && (tmp[0] != ' '))
               {
                  file_character(tmp, FALSE);
               }
            }
            break;

         case 'M':
         case 'm':
            mode ++;
            mode = mode % (cheat_mode?5:2);
            break;

         case ESCAPE:
         case ' ':
         case '\n':
         case '\r':
            flag = TRUE;
            break;

         case 't':
         case 'T':
            if (mode==0) do_cmd_change_tactic(c=='t');
            break;

         case 'e':
         case 'E':
            if (mode==0) do_cmd_change_movement(c=='e');
            break;

         default:
            bell("Unrecognized key");
            break;
      }

      /* Flush messages */
      msg_print(NULL);
   }

   /* Restore the screen */
   Term_load();
   do_cmd_redraw();

   /* Leave "icky" mode */
   character_icky = FALSE;
}


/*
 * Recall the most recent message
 */
void do_cmd_message_one(void)
{
   /* Recall one message XXX XXX XXX */
   prt(format("> %s", message_str(0)), 0, MESSAGE_ROW);
}

/*
 * Show previous messages to the user when saved in memory
 *
 * The screen format uses line 0 and 23 for headers and prompts,
 * skips line 1 and 22, and uses line 2 thru 21 for old messages.
 *
 * This command shows you which commands you are viewing, and allows
 * you to "search" for strings in the recall.
 *
 * Note that messages may be longer than 80 characters, but they are
 * displayed using "infinite" length, with a special sub-command to
 * "slide" the virtual display to the left or right.
 *
 * Attempt to only hilite the matching portions of the string.
 */
static void show_messages_memory(void)
{
   int i, j, k, n, q;

   char shower[81];
   char finder[81];

   /* Wipe finder */
   strcpy(finder, "");

   /* Wipe shower */
   strcpy(shower, "");


   /* Total messages */
   n = message_num();

   /* Start on first message */
   i = 0;

   /* Start at leftmost edge */
   q = 0;

   /* Save screen */
   Term_save();

   /* Process requests until done */
   while (1)
   {
      /* Clear screen */
      Term_clear();

      /* Dump up to 20 lines of messages */
      for (j = 0; (j < 20) && (i + j < n); j++)
      {
         cptr msg = message_str(i+j);

         /* Apply horizontal scroll */
         msg = (strlen(msg) >= q) ? (msg + q) : "";

         /* Dump the messages, bottom to top */
         Term_putstr(0, 21-j, -1, TERM_WHITE, msg);

         /* Hilite "shower" */
         if (shower[0])
         {
            cptr str = msg;

            /* Display matches */
            while ((str = strstr(str, shower)) != NULL)
            {
               int len = strlen(shower);

               /* Display the match */
               Term_putstr(str-msg, 21-j, len, TERM_YELLOW, shower);

               /* Advance */
               str += len;
            }
         }
      }

      /* Display header XXX XXX XXX */
      prt(format("Message Recall (%d-%d of %d), Offset %d",
                 i, i+j-1, n, q), 0, 0);

      /* Display prompt (not very informative) */
      prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]", 0, 23);

      /* Get a command */
      k = inkey();

      /* Exit on Escape */
      if (k == ESCAPE) break;

      /* Hack -- Save the old index */
      j = i;

      /* Horizontal scroll */
      if (k == '4')
      {
         /* Scroll left */
         q = (q >= 40) ? (q - 40) : 0;

         /* Success */
         continue;
      }

      /* Horizontal scroll */
      if (k == '6')
      {
         /* Scroll right */
         q = q + 40;

         /* Success */
         continue;
      }

      /* Hack -- handle show */
      if (k == '=')
      {
         /* Prompt */
         prt("Show: ", 23, 0);

         /* Get a "shower" string, or continue */
         if (!askfor_aux(shower, 80)) continue;

         /* Okay */
         continue;
      }

      /* Hack -- handle find */
      if (k == '/')
      {
         int z;

         /* Prompt */
         prt("Find: ", 0, 23);

         /* Get a "finder" string, or continue */
         if (!askfor_aux(finder, 80)) continue;

         /* Show it */
         strcpy(shower, finder);

         /* Scan messages */
         for (z = i + 1; z < n; z++)
         {
            cptr msg = message_str(z);

            /* Search for it */
            if (strstr(msg, finder))
            {
               /* New location */
               i = z;

               /* Done */
               break;
            }
         }
      }

      /* Recall 1 older message */
      if ((k == '8') || (k == '\n') || (k == '\r'))
      {
         /* Go newer if legal */
         if (i + 1 < n) i += 1;
      }

      /* Recall 10 older messages */
      if (k == '+')
      {
         /* Go older if legal */
         if (i + 10 < n) i += 10;
      }

      /* Recall 20 older messages */
      if ((k == 'p') || (k == KTRL('P')) || (k == ' '))
      {
         /* Go older if legal */
         if (i + 20 < n) i += 20;
      }

      /* Recall 20 newer messages */
      if ((k == 'n') || (k == KTRL('N')))
      {
         /* Go newer (if able) */
         i = (i >= 20) ? (i - 20) : 0;
      }

      /* Recall 10 newer messages */
      if (k == '-')
      {
         /* Go newer (if able) */
         i = (i >= 10) ? (i - 10) : 0;
      }

      /* Recall 1 newer messages */
      if (k == '2')
      {
         /* Go newer (if able) */
         i = (i >= 1) ? (i - 1) : 0;
      }

      /* Hack -- Error of some kind */
      if (i == j) bell(NULL);
   }

   /* Load screen */
   Term_load();
}

/*
 * Show previous messages to the user
 * when saved in file.
 *
 */
static void show_messages_file(void)
{
   char temp[1024];

   strcpy(temp, levelfile);
   strcat(temp,".msg");

/* jk */
   character_icky = TRUE;
   Term_save();
   message_flush();
   (void)browse_file(temp, TRUE, "Message Log File");
   Term_load();
   character_icky = FALSE;
}

void do_cmd_messages(void)
{
   if (save_messages)
   {
      show_messages_file();
   }
   else
   {
      show_messages_memory();
   }
}

/*
 * Interact with some options
 */
static void do_cmd_options_aux(byte page, cptr info)
{
   char   ch;
   s16b   i, j, n = 0;
   s16b   opt[24];
   char   buf[80];

   /* Lookup the options */
   for (i = 0; i < 24; i++) opt[i] = 0;

   /* Scan the options */
   for (i = 0; options[i].descr; i++)
   {
      /* Notice options on this "page" */
      if (options[i].page == page) opt[n++] = i;
   }

   /* Clear the screen */
   clear_screen();

   /* Prompt XXX XXX XXX */
   sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept) ", info);
   prt(buf, 0, MESSAGE_ROW);

   /* Display the options */
   for (i = 0; i < n; i++)
   {
      s16b len = strlen(options[opt[i]].descr);
      prt(options[opt[i]].descr, 0, i+2);
      if (len < 72)
      {
         for (j=len; j < 72; j++) prt(".",j, i+2);
      }
      sprintf(buf, ": %s ", (*options[opt[i]].variable ? "yes" : "no "));
      prt(buf, 72, i + 2);
   }


   /* Start at the first option */
   i = 0;

   /* Interact with the player */
   while (TRUE)
   {
      move_cursor(74, i + 2);

      ch = inkey();

      switch (ch)
      {
         case ESCAPE:
             return;

         case '-':
         case '8':
             i = (n + i - 1) % n;
             break;

         case ' ':
         case '\n':
         case '\r':
         case '2':
             i = (i + 1) % n;
             break;

         case 'y':
         case 'Y':
         case '6':
             put_str("yes ", 74, i + 2);
             (*options[opt[i]].variable) = TRUE;
             i = (i + 1) % n;
             break;

         case 'n':
         case 'N':
         case '4':
             put_str("no  ", 74, i + 2);
             (*options[opt[i]].variable) = FALSE;
             i = (i + 1) % n;
             break;

         default:
             bell("Unrecognized key");
             break;
      }
   }
}

/*
 * Modify the "window" options
 */
static void do_cmd_options_win(void)
{
   int i, j, d;

   int y = 0;
   int x = 0;

   char ch;

   u32b old_flag[8];


   /* Memorize old flags */
   for (j = 0; j < 8; j++)
   {
      /* Acquire current flags */
      old_flag[j] = op_ptr->window_flag[j];
   }

   /* Clear screen */
   Term_clear();

   /* Interact */
   while (1)
   {
      /* Prompt */
      prt("Window flags (<dir> to move, 't' to toggle, or ESC)", 0, 0);

      /* Display the windows */
      for (j = 0; j < 8; j++)
      {
         byte a = TERM_WHITE;

         cptr s = angband_term_name[j];

         /* Use color */
         if (j == x) a = TERM_L_BLUE;

         /* Window name, staggered, centered */
         Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
      }

      /* Display the options */
      for (i = 0; i < 16; i++)
      {
         byte a = TERM_WHITE;

         cptr str = window_flag_desc[i];

         /* Use color */
         if (i == y) a = TERM_L_BLUE;

         /* Unused option */
         if (!str) str = "(Unused option)";

         /* Flag name */
         Term_putstr(0, i + 5, -1, a, str);

         /* Display the windows */
         for (j = 0; j < 8; j++)
         {
            byte a = TERM_WHITE;

            char c = '.';

            /* Use color */
            if ((i == y) && (j == x)) a = TERM_L_BLUE;

            /* Active flag */
            if (op_ptr->window_flag[j] & (1L << i)) c = 'X';

            /* Flag value */
            Term_putch(35 + j * 5, i + 5, a, c);
         }
      }

      /* Place Cursor */
      Term_gotoxy(35 + x * 5, y + 5);

      /* Get key */
      ch = inkey();

      /* Allow escape */
      if ((ch == ESCAPE) || (ch == 'q')) break;

      /* Toggle */
      if ((ch == '5') || (ch == 't'))
      {
         /* Hack -- ignore the main window */
         if (x == 0)
         {
            bell("Cannot set main window flags!");
         }

         /* Toggle flag (off) */
         else if (op_ptr->window_flag[x] & (1L << y))
         {
            op_ptr->window_flag[x] &= ~(1L << y);
         }

         /* Toggle flag (on) */
         else
         {
            op_ptr->window_flag[x] |= (1L << y);
         }

         /* Continue */
         continue;
      }

      /* Extract direction */
      d = target_dir(ch);

      /* Move */
      if (d != 0)
      {
         x = (x + ddx[d] + 8) % 8;
         y = (y + ddy[d] + 16) % 16;
      }

      /* Oops */
      else
      {
         bell("Illegal command for window options!");
      }
   }

   /* Notice changes */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* Dead window */
      if (!angband_term[j]) continue;

      /* Ignore non-changes */
      if (op_ptr->window_flag[j] == old_flag[j]) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Erase */
      Term_clear();

      /* Refresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}

/*
 * Modify the "debug" options
 */
static void do_cmd_options_debug(void)
{
   int i, j, d;

   int x = 0;
   int y = 0;

   char ch;

   /* Clear screen */
   Term_clear();

   /* Interact */
   while (1)
   {
      /* Prompt */
      prt("Debug flags (<dir> to move, 't' to toggle, or ESC)", 0, 0);
      prt("** be careful with these, they may give a 100 megabyte+ logfile with a few moves.", 0, 22);


      /* Display the windows */
      for (i=0; i < 2; i++)
      {
         for (j = 0; j < 16; j++)
         {
            byte a = TERM_WHITE;

            cptr s = debug_flag[i*16+j].explanation;

            /* Window name, staggered, centered */
            Term_putstr(5 + (i*40), 3 + j, -1, a, s);

            if (debuglevel & (1L << (i*16+j)))
            {
               Term_putstr(1 + (i*40), 3 + j, -1, a, "(*)");
            }
            else
            {
               Term_putstr(1 + (i*40), 3 + j, -1, a, "(.)");
            }
         }
      }

      /* Place Cursor */
      Term_gotoxy(2 + x * 40, 3 + y);

      /* Get key */
      ch = inkey();

      /* Allow escape */
      if ((ch == ESCAPE) || (ch == 'q')) break;

      /* Toggle */
      if ((ch == '5') || (ch == 't'))
      {
         /* Toggle flag (off) */
         if (debuglevel & (1L << (x*16+y)))
         {
            debuglevel &= ~ (1L << (x*16+y));
         }

         /* Toggle flag (on) */
         else
         {
            debuglevel |= (1L << (x*16+y));
         }

         /* Continue */
         continue;
      }

      /* Extract direction */
      d = target_dir(ch);

      /* Move */
      if (d != 0)
      {
         x = (x + ddx[d] + 2) % 2;
         y = (y + ddy[d] + 16) % 16;
      }

      /* Oops */
      else
      {
         bell("Illegal command for debug options!");
      }
   }
}

/*
 * Set or unset various options.
 *
 * The user must use the "Ctrl-R" command to "adapt" to changes
 * in any options which control "visual" aspects of the game.
 */
static void do_cmd_options(void)
{
   s16b k;

   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Interact */
   while (1)
   {
      /* Clear screen */
      clear_screen();

      /* Why are we here */
      prt("Angband options", 0, 2);

      /* Give some choices */
      prt("(1) User Interface Options", 5, 4);
      prt("(2) Disturbance Options", 5, 5);
      prt("(3) Inventory Options", 5, 6);
      prt("(4) Game-Play Options", 5, 7);
      prt("(5) Efficiency Options", 5, 8);
      prt("(6) Special Options", 5, 9);
      prt("(7) Windows Options", 5, 10);
      prt("(8) Debug Options", 5, 11);
      prt("(L) Load option file", 5, 12);
      prt("(S) Save option file", 5, 13);

      /* Cheating */
      if (can_be_wizard) prt("(C) Cheating Options", 5, 14);

      /* Special choices */
      prt("(D) Base Delay Speed", 5, 15);
      prt("(H) Hitpoint Warning", 5, 16);

      /* Prompt */
      prt("Command: ", 0, 17);

      /* Get command */
      k = inkey();
      k= FORCEUPPER(k);

      /* Exit */
      if (k == ESCAPE) break;

      /* General Options */
      if (k == '1')
      {
         /* Process the general options */
         do_cmd_options_aux(1, "User Interface Options");
      }

      /* Disturbance Options */
      else if (k == '2')
      {
         /* Process the running options */
         do_cmd_options_aux(2, "Disturbance Options");
      }

      /* Inventory Options */
      else if (k == '3')
      {
         /* Process the running options */
         do_cmd_options_aux(3, "Inventory Options");
      }

      /* Gameplay Options */
      else if (k == '4')
      {
         /* Process the game-play options */
         do_cmd_options_aux(4, "Game-Play Options");
      }

      /* Efficiency Options */
      else if (k == '5')
      {
         /* Process the efficiency options */
         do_cmd_options_aux(5, "Efficiency Options");
      }

      /* Efficiency Options */
      else if (k == '6')
      {
         /* Process the efficiency options */
         do_cmd_options_aux(6, "Special Options");
      }

      /* Windows Options */
      else if (k == '7')
      {
         /* Process the windows options */
         do_cmd_options_win();
      }

      /* Debug Options */
      else if (k == '8')
      {
         /* Process the debug options */
         do_cmd_options_debug();
      }

      /* Load Options */
      else if (k == 'L')
      {
         read_text_options();
      }

      /* Save Options */
      else if (k == 'S')
      {
         write_text_options();
      }

      /* Cheating Options XXX XXX XXX */
      else if ((k == 'C') && can_be_wizard)
      {
         /* Process the cheating options */
         do_cmd_options_aux(0, "Cheating Options");

         /* Hack -- note use of "cheat" options */
         if (cheat_peek)      noscore |= 0x0100;
         if (cheat_hear)      noscore |= 0x0200;
         if (cheat_room)      noscore |= 0x0400;
         if (cheat_xtra)      noscore |= 0x0800;
         if (cheat_know)      noscore |= 0x1000;
         if (cheat_live)      noscore |= 0x2000;
         if (cheat_hitpoints) noscore |= 0x4000;
         if (cheat_mode)      noscore |= 0x8000;
      }

      /* Hack -- Delay Speed */
      else if (k == 'D')
      {
         /* Prompt */
         prt("Command: Base Delay Speed", 0, 17);

         /* Get a new value */
         while (1)
         {
            prt(format("Current delay speed: %d milliseconds",
                       delay_spd), 0, 19);
            prt("Delay Speed (0-9 or ESC to accept): ", 0, 20);
            k = inkey();
            if (k == ESCAPE) break;
            if (isdigit(k))
            {
               delay_spd = D2I(k);
            }
            else bell("Unrecognized speed value");
         }
      }

      /* Hack -- hitpoint warning factor */
      else if (k == 'H')
      {
         /* Prompt */
         prt("Command: Hitpoint Warning", 0, 17);

         /* Get a new value */
         while (1)
         {
            prt(format("Current hitpoint warning: %d0%%",
                       hitpoint_warn), 0, 19);
            prt("Hitpoint Warning (0-9 or ESC to accept): ", 0, 20);
            k = inkey();
            if (k == ESCAPE) break;
            if (isdigit(k)) hitpoint_warn = D2I(k);
            else bell("Unrecognized hitpoint warning value");
         }
      }

      /* Unknown option */
      else
      {
         /* Oops */
         bell("Unknown option selected");
      }

      /* Flush messages */
      msg_print(NULL);
   }

   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;

}

/*
 * Check various lists
 */
void do_cmd_check(void)
{
   s16b k;

   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Interact */
   while (1)
   {
      /* Clear screen */
      clear_screen();

      /* Why are we here */
      prt("Lists of achievements:", 0, 2);

      /* Give some choices */
      prt("(A) Identified Artifacts", 5, 4);
      prt("(I) Identified Items", 5, 5);
      prt("(K) Killed Monsters", 5, 6);
      prt("(P) Turn-based progress report", 5, 7);
      prt("(R) Intrinsics", 5, 8);
      prt("(S) Spells Cast", 5, 9);
      prt("(U) Killed Uniques", 5, 10);

      /* Prompt */
      prt("Command: ", 0, 17);

      /* Get command */
      k = inkey();
      k= FORCEUPPER(k);

      /* Exit */
      if (k == ESCAPE) break;

      /* General Options */
      switch(k)
      {
         case 'A' : do_cmd_check_artifacts(NULL, TRUE); break;
         case 'I' : do_cmd_check_items(NULL, TRUE); break;
         case 'K' : do_cmd_check_kills(NULL, TRUE); break;
         case 'P' : do_cmd_check_progress(NULL, TRUE); break;
         case 'R' : do_cmd_check_intrinsics(NULL, TRUE); break;
         case 'S' : do_cmd_check_spells(NULL, TRUE); break;
         case 'U' : do_cmd_check_uniques(NULL, TRUE); break;
         default  :  bell("Unknown option selected");
      }

      /* Flush messages */
      msg_print(NULL);
   }

   /* Restore the screen */
   Term_load();
   do_cmd_redraw();

   /* Leave "icky" mode */
   character_icky = FALSE;

}

/*
 * Write options into text file
 */
void write_text_options(void)
{
   s16b  i;

   FILE *f;

   char buf[1024];
   char name[1024];

   clear_screen();

   prt("Options files are saved in lib/xtra, and can be loaded on startup", 0, 5);
   prt("using the -l <file> commandline option.", 0, 6);

   strcpy(name,"");
   if (!get_string("Enter filename to dump options to:", name, 1024)) return;

   /* Access the font file */
   sprintf(buf, "%s%s", ANGBAND_DIR_XTRA, name);

   /* Open the file */
   f = my_fopen(buf, "w");

   /* Okay */
   if (!f)
   {
      msg_print("cmd4.c: write_text_options: error opening text options file");
      return;
   }

   my_fputs(f, "# Angband options file. Accepted values are yes or no", 0);
   /* Scan the options */
   for (i = 0; options[i].descr; i++)
   {
       my_fputs(f, format("%s=%s", options[i].descr,
                   (*options[i].variable) ? "yes" : "no"), 0);
   }
   if (my_fclose(f))
   {
      msg_print("cmd4.c: write_text_options: error closing text options file");
      return;
   }
}

/*
 *  Read the options from a specific file.
 *  we want this separate, so we can add a command-line argument with a filename
 */
void read_text_options_file(cptr filename)
{
   char buf[1024];
   char line[1024];
   char option_name[80];
   char option_value_str[5];
   bool option_value;
   s16b lineno = 0;
   s16b i,j;
   FILE *f;

   sprintf(buf, "%s%s", ANGBAND_DIR_XTRA, filename);

   /* Open the file */
   f = my_fopen(buf, "r");

   /* Okay */
   if (!f)
   {
      msg_print("cmd4.c: read_text_options_file: error opening text options file");
      return;
   }

   /* Scan the options */
   while (0 == my_fgets(f, line, 1024))
   {
      lineno++;
      if (line[0]=='#') continue; /* skip comments */
      j = 0;
      while (line[j] && (line[j]!='='))
      {
         j++;
      }

      if (!line[j])
      {
         msg_format("cmd4.c: read_text_options_file: error parsing line %d: %s",
                   lineno, line);
         (void)my_fclose(f);
         return;
      }

      strcpy(option_value_str,&line[j+1]);
      line[j] = 0;
      strcpy(option_name,line);

      option_value = cmp_strngs(option_value_str, "yes");
      for (i = 0; options[i].descr; i++)
      {
          if (cmp_strngs(options[i].descr, option_name))
          {
             *options[i].variable = option_value;
             break;
          }
      }
      if (!options[i].descr)
      {
         msg_format("cmd4.c: read_text_options_file: unknown option %s",
                    option_name);
         (void)my_fclose(f);
         return;
      }
   }
   if (my_fclose(f))
   {
      msg_print("cmd4.c: read_text_options: error closing text options file");
      return;
   }
}

/*
 * Read options into text file
 */
void read_text_options(void)
{
   char name[1024];

   strcpy(name,"");
   if (!get_string("Enter filename to read options from:", name, 1024)) return;

   read_text_options_file(name);
}


/*
 * Ask for a "user pref line" and process it
 *
 * XXX XXX XXX Allow absolute file names?
 */
static void do_cmd_pref(void)
{
   char buf[80];

   /* Default */
   strcpy(buf, "");

   /* Ask for a "user pref command" */
   if (!get_string("Pref: ", buf, 80)) return;

   /* Process that pref command */
   (void)process_pref_file_aux(buf);
}


#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
   s16b i;

   FILE *fff;

   char buf[1024];

   /* Build the filename */
   strcpy(buf, ANGBAND_DIR_USER);
   strcat(buf, fname);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "text file" */
   _ftype = 'TEXT';
#endif

   /* Append to the file */
   fff = my_fopen(buf, "a");

   /* Failure */
   if (!fff) return (-1);

   /* Skip space */
   fprintf(fff, "\n\n");

   /* Start dumping */
   fprintf(fff, "# Automatic macro dump\n\n");

   /* Dump them */
   for (i = 0; i < macro__num; i++)
   {
      /* Start the macro */
      fprintf(fff, "# Macro '%d'\n\n", i);

      /* Extract the action */
      ascii_ttext(buf, macro__act[i]);

      /* Dump the macro */
      fprintf(fff, "A:%s\n", buf);

      /* Extract the action */
      ascii_ttext(buf, macro__pat[i]);

      /* Dump normal macros */
      fprintf(fff, "P:%s\n", buf);

      /* End the macro */
      fprintf(fff, "\n\n");
   }

   /* Start dumping */
   fprintf(fff, "\n\n\n\n");


   /* Close */
   my_fclose(fff);

   /* Success */
   return (0);
}

/*
 * Hack -- ask for a "trigger" (see below)
 *
 * Note the complex use of the "inkey()" function from "util.c".
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static void do_cmd_macro_aux(char *buf)
{
   int i, n = 0;

   char tmp[1024];


   /* Flush */
   flush();

   /* Do not process macros */
   inkey_base = TRUE;

   /* First key */
   i = inkey();

   /* Read the pattern */
   while (i)
   {
      /* Save the key */
      buf[n++] = i;

      /* Do not process macros */
      inkey_base = TRUE;

      /* Do not wait for keys */
      inkey_scan = TRUE;

      /* Attempt to read a key */
      i = inkey();
   }

   /* Terminate */
   buf[n] = '\0';

   /* Flush */
   flush();


   /* Convert the trigger */
   ascii_ttext(tmp, buf);

   /* Hack -- display the trigger */
   Term_addstr(-1, TERM_WHITE, tmp);
}

/*
 * Hack -- ask for a keymap "trigger" (see below)
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static void do_cmd_macro_aux_keymap(char *buf)
{
   char tmp[1024];


   /* Flush */
   flush();


   /* Get a key */
   buf[0] = inkey();
   buf[1] = '\0';


   /* Convert to ascii */
   ascii_ttext(tmp, buf);

   /* Hack -- display the trigger */
   Term_addstr(-1, TERM_WHITE, tmp);


   /* Flush */
   flush();
}

/*
 * Hack -- append all keymaps to the given file
 */
static errr keymap_dump(cptr fname)
{
   int i;

   FILE *fff;

   char key[1024];
   char buf[1024];

   int mode;

   /* Roguelike */
   if (rogue_like_commands)
   {
      mode = KEYMAP_MODE_ROGUE;
   }

   /* Original */
   else
   {
      mode = KEYMAP_MODE_ORIG;
   }

   /* Build the filename */
   path_build(buf, 1024, ANGBAND_DIR_USER, fname);

   /* Append to the file */
   fff = my_fopen(buf, "a");

   /* Failure */
   if (!fff) return (-1);

   /* Skip space */
   fprintf(fff, "\n\n");

   /* Start dumping */
   fprintf(fff, "# Automatic keymap dump\n\n");

   /* Dump them */
   for (i = 0; i < 256; i++)
   {
      cptr act;

      /* Loop up the keymap */
      act = keymap_act[mode][i];

      /* Skip empty keymaps */
      if (!act) continue;

      /* Encode the key */
      buf[0] = i;
      buf[1] = '\0';
      ascii_ttext(key, buf);

      /* Encode the action */
      ascii_ttext(buf, act);

      /* Dump the macro */
      fprintf(fff, "M:%d  %2s  %s\n", mode, key, buf);
   }

   /* Start dumping */
   fprintf(fff, "\n\n\n");


   /* Close */
   my_fclose(fff);

   /* Success */
   return (0);
}

#endif

/*
 * Interact with "macros"
 *
 * Note that the macro "action" must be defined before the trigger.
 *
 * Could use some helpful instructions on this page.  XXX XXX XXX
 */
void do_cmd_macros(void)
{
   int i;

   char tmp[1024];

   char buf[1024];

   int mode;

   /* Roguelike */
   if (rogue_like_commands)
   {
      mode = KEYMAP_MODE_ROGUE;
   }

   /* Original */
   else
   {
      mode = KEYMAP_MODE_ORIG;
   }

   /* Save screen */
   Term_save();

   /* Process requests until done */
   while (1)
   {
      /* Clear screen */
      Term_clear();

      /* Describe */
      prt("Interact with Macros", 0, 2);


      /* Describe that action */
      prt("Current action (if any) shown below:", 0, 20);

      /* Analyze the current action */
      ascii_ttext(buf, macro_buffer);

      /* Display the current action */
      prt(buf, 0, 22);

      /* Selections */
      prt("(1) Load a user pref file", 5, 4);
#ifdef ALLOW_MACROS
      prt("(2) Append macros to a file", 5, 5);
      prt("(3) Query a macro", 5, 6);
      prt("(4) Create a macro", 5, 7);
      prt("(5) Remove a macro", 5, 8);
      prt("(6) Append keymaps to a file", 5, 9);
      prt("(7) Query a keymap", 5, 10);
      prt("(8) Create a keymap", 5, 11);
      prt("(9) Remove a keymap", 5, 12);
      prt("(0) Enter a new action", 5, 13);
#endif /* ALLOW_MACROS */

      /* Prompt */
      prt("Command: ", 0, 16);

      /* Get a command */
      i = inkey();

      /* Leave */
      if (i == ESCAPE) break;

      /* Load a 'macro' file */
      else if (i == '1')
      {
         /* Prompt */
         prt("Command: Load a user pref file", 0, 16);

         /* Prompt */
         prt("File: ", 0, 18);

         /* Default filename */
         sprintf(tmp, "%s.prf", player_name);

         /* Ask for a file */
         if (!askfor_aux(tmp, 80)) continue;

         /* Process the given filename */
         if (0 != process_pref_file(tmp))
         {
            /* Prompt */
            msg_print("Could not load file!");
         }
      }

#ifdef ALLOW_MACROS

      /* Save macros */
      else if (i == '2')
      {
         /* Prompt */
         prt("Command: Append macros to a file", 0, 16);

         /* Prompt */
         prt("File: ", 0, 18);

         /* Default filename */
         sprintf(tmp, "%s.prf", player_name);

         /* Ask for a file */
         if (!askfor_aux(tmp, 80)) continue;

         /* Drop priv's */
         safe_setuid_drop();

         /* Dump the macros */
         (void)macro_dump(tmp);

         /* Grab priv's */
         safe_setuid_grab();

         /* Prompt */
         msg_print("Appended macros.");
      }

      /* Query a macro */
      else if (i == '3')
      {
         int k;

         /* Prompt */
         prt("Command: Query a macro", 0, 16);

         /* Prompt */
         prt("Trigger: ", 0, 18);

         /* Get a macro trigger */
         do_cmd_macro_aux(buf);

         /* Acquire action */
         k = macro_find_exact(buf);

         /* Nothing found */
         if (k < 0)
         {
            /* Prompt */
            msg_print("Found no macro.");
         }

         /* Found one */
         else
         {
            /* Obtain the action */
            strcpy(macro_buffer, macro__act[k]);

            /* Analyze the current action */
            ascii_ttext(buf, macro_buffer);

            /* Display the current action */
            prt(buf, 0, 22);

            /* Prompt */
            msg_print("Found a macro.");
         }
      }

      /* Create a macro */
      else if (i == '4')
      {
         /* Prompt */
         prt("Command: Create a macro", 0, 16);

         /* Prompt */
         prt("Trigger: ", 0, 18);

         /* Get a macro trigger */
         do_cmd_macro_aux(buf);

         /* Clear */
         clear_from(20);

         /* Prompt */
         prt("Action: ", 0, 20);

         /* Convert to text */
         ascii_ttext(tmp, macro_buffer);

         /* Get an encoded action */
         if (askfor_aux(tmp, 80))
         {
            /* Convert to ascii */
            text_to_ascii(macro_buffer, tmp);

            /* Link the macro */
            macro_add(buf, macro_buffer);

            /* Prompt */
            msg_print("Added a macro.");
         }
      }

      /* Remove a macro */
      else if (i == '5')
      {
         /* Prompt */
         prt("Command: Remove a macro", 0, 16);

         /* Prompt */
         prt("Trigger: ", 0, 18);

         /* Get a macro trigger */
         do_cmd_macro_aux(buf);

         /* Link the macro */
         macro_add(buf, buf);

         /* Prompt */
         msg_print("Removed a macro.");
      }

      /* Save keymaps */
      else if (i == '6')
      {
         /* Prompt */
         prt("Command: Append keymaps to a file", 0, 16);

         /* Prompt */
         prt("File: ", 0, 18);

         /* Default filename */
         sprintf(tmp, "%s.prf", player_name);

         /* Ask for a file */
         if (!askfor_aux(tmp, 80)) continue;

         /* Drop priv's */
         safe_setuid_drop();

         /* Dump the macros */
         (void)keymap_dump(tmp);

         /* Grab priv's */
         safe_setuid_grab();

         /* Prompt */
         msg_print("Appended keymaps.");
      }

      /* Query a keymap */
      else if (i == '7')
      {
         cptr act;

         /* Prompt */
         prt("Command: Query a keymap", 0, 16);

         /* Prompt */
         prt("Keypress: ", 0, 18);

         /* Get a keymap trigger */
         do_cmd_macro_aux_keymap(buf);

         /* Look up the keymap */
         act = keymap_act[mode][(byte)(buf[0])];

         /* Nothing found */
         if (!act)
         {
            /* Prompt */
            msg_print("Found no keymap.");
         }

         /* Found one */
         else
         {
            /* Obtain the action */
            strcpy(macro_buffer, act);

            /* Analyze the current action */
            ascii_ttext(buf, macro_buffer);

            /* Display the current action */
            prt(buf, 0, 22);

            /* Prompt */
            msg_print("Found a keymap.");
         }
      }

      /* Create a keymap */
      else if (i == '8')
      {
         /* Prompt */
         prt("Command: Create a keymap", 0, 16);

         /* Prompt */
         prt("Keypress: ", 0, 18);

         /* Get a keymap trigger */
         do_cmd_macro_aux_keymap(buf);

         /* Clear */
         clear_from(20);

         /* Prompt */
         prt("Action: ", 0, 20);

         /* Convert to text */
         ascii_ttext(tmp, macro_buffer);

         /* Get an encoded action */
         if (askfor_aux(tmp, 80))
         {
            /* Convert to ascii */
            text_to_ascii(macro_buffer, tmp);

            /* Free old keymap */
            string_free(keymap_act[mode][(byte)(buf[0])]);

            /* Make new keymap */
            keymap_act[mode][(byte)(buf[0])] = string_make(macro_buffer);

            /* Prompt */
            msg_print("Added a keymap.");
         }
      }

      /* Remove a keymap */
      else if (i == '9')
      {
         /* Prompt */
         prt("Command: Remove a keymap", 0, 16);

         /* Prompt */
         prt("Keypress: ", 0, 18);

         /* Get a keymap trigger */
         do_cmd_macro_aux_keymap(buf);

         /* Free old keymap */
         string_free(keymap_act[mode][(byte)(buf[0])]);

         /* Make new keymap */
         keymap_act[mode][(byte)(buf[0])] = NULL;

         /* Prompt */
         msg_print("Removed a keymap.");
      }

      /* Enter a new action */
      else if (i == '0')
      {
         /* Prompt */
         prt("Command: Enter a new action", 0, 16);

         /* Go to the correct location */
         Term_gotoxy(0, 22);

         /* Hack -- limit the value */
         tmp[80] = '\0';

         /* Get an encoded action */
         if (!askfor_aux(buf, 80)) continue;

         /* Extract an action */
         text_to_ascii(macro_buffer, buf);
      }

#endif /* ALLOW_MACROS */

      /* Oops */
      else
      {
         /* Oops */
         bell("Illegal command for macros!");
      }

      /* Flush messages */
      msg_print(NULL);
   }


   /* Load screen */
   Term_load();
}

/*
 * Interact with "visuals"
 */
static void do_cmd_visuals(void)
{
   s16b i;

   FILE *fff;

   char tmp[160];

   char buf[1024];


#if defined(MACINTOSH) && !defined(applec)
   _ftype = 'TEXT';
#endif


   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();


   /* Interact until done */
   while (1)
   {
      /* Clear the screen */
      clear_screen();

      /* Ask for a choice */
      prt("Interact with Visuals", 0, 2);

      /* Give some choices */
      prt("(1) Load a user pref file", 5, 4);
#ifdef ALLOW_VISUALS
      prt("(2) Dump monster attr/chars", 5, 5);
      prt("(3) Dump object attr/chars", 5, 6);
      prt("(4) Dump feature attr/chars", 5, 7);
      prt("(5) (unused)", 5, 8);
      prt("(6) Change monster attr/chars", 5, 9);
      prt("(7) Change object attr/chars", 5, 10);
      prt("(8) Change feature attr/chars", 5, 11);
      prt("(9) (unused)", 5, 12);
#endif
      prt("(0) Reset visuals", 5, 13);

      /* Prompt */
      prt("Command: ", 0, 15);

      /* Prompt */
      i = inkey();

      /* Done */
      if (i == ESCAPE) break;

      /* Load a 'pref' file */
      else if (i == '1')
      {
         /* Prompt */
         prt("Command: Load a user pref file", 0, 15);

         /* Prompt */
         prt("File: ", 0, 17);

         /* Default filename */
         sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

         /* Query */
         if (!askfor_aux(tmp, 70)) continue;

         /* Process the given filename */
         (void)process_pref_file(tmp);
      }

#ifdef ALLOW_VISUALS

      /* Dump monster attr/chars */
      else if (i == '2')
      {
         /* Prompt */
         prt("Command: Dump monster attr/chars", 0, 15);

         /* Prompt */
         prt("File: ", 0, 17);

         /* Default filename */
         sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

         /* Get a filename */
         if (!askfor_aux(tmp, 70)) continue;

         /* Build the filename */
         strcpy(buf, ANGBAND_DIR_USER);
         strcat(buf, tmp);

         /* Drop priv's */
         safe_setuid_drop();

         /* Append to the file */
         fff = my_fopen(buf, "a");

         /* Grab priv's */
         safe_setuid_grab();

         /* Failure */
         if (!fff) continue;

         /* Start dumping */
         fprintf(fff, "\n\n");
         fprintf(fff, "# Monster attr/char definitions\n\n");

         /* Dump monsters */
         for (i = 0; i < r_number; i++)
         {
            monster_race *r_ptr = &r_info[i];

            /* Skip non-entries */
            if (!r_ptr->name) continue;

            /* Dump a comment */
            fprintf(fff, "# %s\n", (r_name + r_ptr->name));

            /* Dump the monster attr/char info */
            fprintf(fff, "R:%d:0x%02X:0x%02X\n\n", i,
                  (byte)(r_ptr->x_attr), (byte)(r_ptr->x_char));
         }

         /* All done */
         fprintf(fff, "\n\n\n\n");

         /* Close */
         my_fclose(fff);

         /* Message */
         msg_print("Dumped monster attr/chars.");
      }

      /* Dump object attr/chars */
      else if (i == '3')
      {
         /* Prompt */
         prt("Command: Dump object attr/chars", 0, 15);

         /* Prompt */
         prt("File: ", 0, 17);

         /* Default filename */
         sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

         /* Get a filename */
         if (!askfor_aux(tmp, 70)) continue;

         /* Build the filename */
         strcpy(buf, ANGBAND_DIR_USER);
         strcat(buf, tmp);

         /* Drop priv's */
         safe_setuid_drop();

         /* Append to the file */
         fff = my_fopen(buf, "a");

         /* Grab priv's */
         safe_setuid_grab();

         /* Failure */
         if (!fff) continue;

         /* Start dumping */
         fprintf(fff, "\n\n");
         fprintf(fff, "# Object attr/char definitions\n\n");

         /* Dump objects */
         for (i = 0; i < k_number; i++)
         {
            object_kind *k_ptr = &k_info[i];

            /* Skip non-entries */
            if (!k_ptr->name) continue;

            /* Dump a comment */
            fprintf(fff, "# %s\n", (k_name + k_ptr->name));

            /* Dump the object attr/char info */
            fprintf(fff, "K:%d:0x%02X:0x%02X\n\n", i,
                  (byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));
         }

         /* All done */
         fprintf(fff, "\n\n\n\n");

         /* Close */
         my_fclose(fff);

         /* Message */
         msg_print("Dumped object attr/chars.");
      }

      /* Dump feature attr/chars */
      else if (i == '4')
      {
         /* Prompt */
         prt("Command: Dump feature attr/chars", 0, 15);

         /* Prompt */
         prt("File: ", 0, 17);

         /* Default filename */
         sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

         /* Get a filename */
         if (!askfor_aux(tmp, 70)) continue;

         /* Build the filename */
         strcpy(buf, ANGBAND_DIR_USER);
         strcat(buf, tmp);

         /* Drop priv's */
         safe_setuid_drop();

         /* Append to the file */
         fff = my_fopen(buf, "a");

         /* Grab priv's */
         safe_setuid_grab();

         /* Failure */
         if (!fff) continue;

         /* Start dumping */
         fprintf(fff, "\n\n");
         fprintf(fff, "# Feature attr/char definitions\n\n");

         /* Dump features */
         for (i = 0; i < f_number; i++)
         {
            feature_type *f_ptr = &f_info[i];

            /* Skip non-entries */
            if (!f_ptr->name) continue;

            /* Dump a comment */
            fprintf(fff, "# %s\n", (f_name + f_ptr->name));

            /* Dump the feature attr/char info */
            /* we dump one line for each feature here */
            fprintf(fff, "F:%d:%d:%d:%d:%d/%d\n",
                     f_ptr->mtyp, f_ptr->mtyp, f_ptr->styp, f_ptr->styp,
                     (byte)(f_ptr->x_attr), (byte)(f_ptr->x_char));
         }

         /* All done */
         fprintf(fff, "\n\n\n\n");

         /* Close */
         my_fclose(fff);

         /* Message */
         msg_print("Dumped feature attr/chars.");
      }

      /* Modify monster attr/chars */
      else if (i == '6')
      {
         static s16b r = 0;

         /* Prompt */
         prt("Command: Change monster attr/chars", 0, 15);

         /* Hack -- query until done */
         while (1)
         {
            monster_race *r_ptr = &r_info[r];

            s16b da = (byte)(r_ptr->d_attr);
            s16b dc = (byte)(r_ptr->d_char);
            s16b ca = (byte)(r_ptr->x_attr);
            s16b cc = (byte)(r_ptr->x_char);

            /* Label the object */
            Term_putstr(5, 17, -1, TERM_WHITE,
                     format("Monster = %d, Name = %-40.40s",
                           r, (r_name + r_ptr->name)));

            /* Label the Default values */
            Term_putstr(10, 19, -1, TERM_WHITE,
                     format("Default attr/char = %3u / %3u", da, dc));
            Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
            Term_putch(43, 19, da, dc);

            /* Label the Current values */
            Term_putstr(10, 20, -1, TERM_WHITE,
                     format("Current attr/char = %3u / %3u", ca, cc));
            Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
            Term_putch(43, 20, ca, cc);

            /* Prompt */
            Term_putstr(0, 22, -1, TERM_WHITE,
                     "Command (n/N/a/A/c/C): ");

            /* Get a command */
            i = inkey();

            /* All done */
            if (i == ESCAPE) break;

            /* Analyze */
            if (i == 'n') r = (r + r_number + 1) % r_number;
            if (i == 'N') r = (r + r_number - 1) % r_number;
            if (i == 'a') r_ptr->x_attr = (byte)(ca + 1);
            if (i == 'A') r_ptr->x_attr = (byte)(ca - 1);
            if (i == 'c') r_ptr->x_char = (byte)(cc + 1);
            if (i == 'C') r_ptr->x_char = (byte)(cc - 1);
         }
      }

      /* Modify object attr/chars */
      else if (i == '7')
      {
         static s16b k = 0;

         /* Prompt */
         prt("Command: Change object attr/chars", 0, 15);

         /* Hack -- query until done */
         while (1)
         {
            object_kind *k_ptr = &k_info[k];

            s16b da = (byte)k_ptr->d_attr;
            s16b dc = (byte)k_ptr->d_char;
            s16b ca = (byte)k_ptr->x_attr;
            s16b cc = (byte)k_ptr->x_char;

            /* Label the object */
            Term_putstr(5, 17, -1, TERM_WHITE,
                     format("Object = %d, Name = %-40.40s",
                           k, (k_name + k_ptr->name)));

            /* Label the Default values */
            Term_putstr(10, 19, -1, TERM_WHITE,
                     format("Default attr/char = %3d / %3d", da, dc));
            Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
            Term_putch(43, 19, da, dc);

            /* Label the Current values */
            Term_putstr(10, 20, -1, TERM_WHITE,
                     format("Current attr/char = %3d / %3d", ca, cc));
            Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
            Term_putch(43, 20, ca, cc);

            /* Prompt */
            Term_putstr(0, 22, -1, TERM_WHITE,
                     "Command (n/N/a/A/c/C): ");

            /* Get a command */
            i = inkey();

            /* All done */
            if (i == ESCAPE) break;

            /* Analyze */
            if (i == 'n') k = (k + k_number + 1) % k_number;
            if (i == 'N') k = (k + k_number - 1) % k_number;
            if (i == 'a') k_info[k].x_attr = (byte)(ca + 1);
            if (i == 'A') k_info[k].x_attr = (byte)(ca - 1);
            if (i == 'c') k_info[k].x_char = (byte)(cc + 1);
            if (i == 'C') k_info[k].x_char = (byte)(cc - 1);
         }
      }

      /* Modify feature attr/chars */
      else if (i == '8')
      {
         static s16b f = 0;

         /* Prompt */
         prt("Command: Change feature attr/chars", 0, 15);

         /* Hack -- query until done */
         while (1)
         {
            feature_type *f_ptr = &f_info[f];

            s16b da = (byte)f_ptr->d_attr;
            s16b dc = (byte)f_ptr->d_char;
            s16b ca = (byte)f_ptr->x_attr;
            s16b cc = (byte)f_ptr->x_char;

            /* Label the object */
            Term_putstr(5, 17, -1, TERM_WHITE,
                     format("Terrain = %d, Name = %-40.40s",
                           f, (f_name + f_ptr->name)));

            /* Label the Default values */
            Term_putstr(10, 19, -1, TERM_WHITE,
                     format("Default attr/char = %3d / %3d", da, dc));
            Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
            Term_putch(43, 19, da, dc);

            /* Label the Current values */
            Term_putstr(10, 20, -1, TERM_WHITE,
                     format("Current attr/char = %3d / %3d", ca, cc));
            Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
            Term_putch(43, 20, ca, cc);

            /* Prompt */
            Term_putstr(0, 22, -1, TERM_WHITE,
                     "Command (n/N/a/A/c/C): ");

            /* Get a command */
            i = inkey();

            /* All done */
            if (i == ESCAPE) break;

            /* Analyze */
            if (i == 'n') f = (f + f_number + 1) % f_number;
            if (i == 'N') f = (f + f_number - 1) % f_number;
            if (i == 'a') f_info[f].x_attr = (byte)(ca + 1);
            if (i == 'A') f_info[f].x_attr = (byte)(ca - 1);
            if (i == 'c') f_info[f].x_char = (byte)(cc + 1);
            if (i == 'C') f_info[f].x_char = (byte)(cc - 1);
         }
      }

#endif

      /* Reset visuals */
      else if (i == '0')
      {
         /* Reset */
         reset_visuals();

         /* Message */
         msg_print("Visual attr/char tables reset.");
      }

      /* Unknown option */
      else
      {
         bell("Unknown option selected");
      }

      /* Flush messages */
      msg_print(NULL);
   }


   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;
}


/*
 * Interact with "colors"
 */
static void do_cmd_colors(void)
{
   s16b i;

   FILE *fff;

   char tmp[160];

   char buf[1024];


#if defined(MACINTOSH) && !defined(applec)
   _ftype = 'TEXT';
#endif


   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();


   /* Interact until done */
   while (1)
   {
      /* Clear the screen */
      clear_screen();

      /* Ask for a choice */
      prt("Interact with Colors", 0, 2);

      /* Give some choices */
      prt("(1) Load a user pref file", 5, 4);
#ifdef ALLOW_COLORS
      prt("(2) Dump colors", 5, 5);
      prt("(3) Modify colors", 5, 6);
#endif

      /* Prompt */
      prt("Command: ", 0, 8);

      /* Prompt */
      i = inkey();

      /* Done */
      if (i == ESCAPE) break;

      /* Load a 'pref' file */
      if (i == '1')
      {
         /* Prompt */
         prt("Command: Load a user pref file", 0, 8);

         /* Prompt */
         prt("File: ", 0, 10);

         /* Default file */
         sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

         /* Query */
         if (!askfor_aux(tmp, 70)) continue;

         /* Process the given filename */
         (void)process_pref_file(tmp);

         /* Mega-Hack -- react to changes */
         Term_xtra(TERM_XTRA_REACT, 0);

         /* Mega-Hack -- redraw */
         Term_redraw();
      }

#ifdef ALLOW_COLORS

      /* Dump colors */
      else if (i == '2')
      {
         /* Prompt */
         prt("Command: Dump colors", 0, 8);

         /* Prompt */
         prt("File: ", 0, 10);

         /* Default filename */
         sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

         /* Get a filename */
         if (!askfor_aux(tmp, 70)) continue;

         /* Build the filename */
         strcpy(buf, ANGBAND_DIR_USER);
         strcat(buf, tmp);

         /* Drop priv's */
         safe_setuid_drop();

         /* Append to the file */
         fff = my_fopen(buf, "a");

         /* Grab priv's */
         safe_setuid_grab();

         /* Failure */
         if (!fff) continue;

         /* Start dumping */
         fprintf(fff, "\n\n");
         fprintf(fff, "# Color redefinitions\n\n");

         /* Dump colors */
         for (i = 0; i < 256; i++)
         {
            s16b kv = color_table[i][0];
            s16b rv = color_table[i][1];
            s16b gv = color_table[i][2];
            s16b bv = color_table[i][3];

            cptr name = "unknown";

            /* Skip non-entries */
            if (!kv && !rv && !gv && !bv) continue;

            /* Extract the color name */
            if (i < 16) name = color_names[i];

            /* Dump a comment */
            fprintf(fff, "# Color '%s'\n", name);

            /* Dump the monster attr/char info */
            fprintf(fff, "V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n",
                  i, kv, rv, gv, bv);
         }

         /* All done */
         fprintf(fff, "\n\n\n\n");

         /* Close */
         my_fclose(fff);

         /* Message */
         msg_print("Dumped color redefinitions.");
      }

      /* Edit colors */
      else if (i == '3')
      {
         static s16b a = 0;

         /* Prompt */
         prt("Command: Modify colors", 0, 8);

         /* Hack -- query until done */
         while (1)
         {
            cptr name;

            /* Clear */
            clear_from(10);

            /* Exhibit the normal colors */
            for (i = 0; i < 16; i++)
            {
               /* Exhibit this color */
               Term_putstr(i*4, 20, -1, a, "###");

               /* Exhibit all colors */
               Term_putstr(i*4, 22, -1, i, format("%3d", i));
            }

            /* Describe the color */
            name = ((a < 16) ? color_names[a] : "undefined");

            /* Describe the color */
            Term_putstr(5, 10, -1, TERM_WHITE,
                     format("Color = %d, Name = %s", a, name));

            /* Label the Current values */
            Term_putstr(5, 12, -1, TERM_WHITE,
                     format("K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
                           color_table[a][0], color_table[a][1],
                           color_table[a][2], color_table[a][3]));

            /* Prompt */
            Term_putstr(0, 14, -1, TERM_WHITE,
                     "Command (n/N/k/K/r/R/g/G/b/B): ");

            /* Get a command */
            i = inkey();

            /* All done */
            if (i == ESCAPE) break;

            /* Analyze */
            if (i == 'n') a = (byte)(a + 1);
            if (i == 'N') a = (byte)(a - 1);
            if (i == 'k') color_table[a][0] = (byte)(color_table[a][0] + 1);
            if (i == 'K') color_table[a][0] = (byte)(color_table[a][0] - 1);
            if (i == 'r') color_table[a][1] = (byte)(color_table[a][1] + 1);
            if (i == 'R') color_table[a][1] = (byte)(color_table[a][1] - 1);
            if (i == 'g') color_table[a][2] = (byte)(color_table[a][2] + 1);
            if (i == 'G') color_table[a][2] = (byte)(color_table[a][2] - 1);
            if (i == 'b') color_table[a][3] = (byte)(color_table[a][3] + 1);
            if (i == 'B') color_table[a][3] = (byte)(color_table[a][3] - 1);

            /* Hack -- react to changes */
            Term_xtra(TERM_XTRA_REACT, 0);

            /* Hack -- redraw */
            Term_redraw();
         }
      }

#endif

      /* Unknown option */
      else
      {
         bell("Unknown option selected");
      }

      /* Flush messages */
      msg_print(NULL);
   }


   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;
}


/*
 * Note something in the message recall
 */
void do_cmd_note(void)
{
   char buf[80];

   /* Default */
   strcpy(buf, "");

   /* Input */
   if (!get_string("Note: ", buf, 60)) return;

   /* Ignore empty notes */
   if (!buf[0] || (buf[0] == ' ')) return;

   /* Add the note to the message recall */
   msg_format("Note: %s", buf);
}

/*
 * Mention the current version
 */
void do_cmd_version(void)
{
   /* Silly message */
   msg_format("You are playing Angband/64 beta %d release %d. Press ? for help.",
            VERSION_BETA, VERSION_RELEASE);
}



/*
 * Array of feeling strings
 */
static cptr do_cmd_feeling_text[11] =
{
   "Looks like any other level.",
   "You feel there is something special about this level.",
   "You have a superb feeling about this level.",
   "You have an excellent feeling...",
   "You have a very good feeling...",
   "You have a good feeling...",
   "You feel strangely lucky...",
   "You feel your luck is turning...",
   "You like the look of this place...",
   "This level can't be all bad...",
   "What a boring place..."
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling()
{
   /* Verify the feeling */
   if (feeling < 0) feeling = 0;
   if (feeling > 10) feeling = 10;

   /* No useful feeling in town */
   if ((!p_ptr->mdepth) && (!p_ptr->sdepth))
   {
      msg_print("Looks like a typical town.");
      return;
   }

   /* Display the feeling */
   msg_print(do_cmd_feeling_text[feeling]);
}

/*
 * Encode the screen colors
 */
static char hack[17] = "dwsorgbuDWvyRGBU";

/*
 * Hack -- load a screen dump from a file
 */
void do_cmd_load_screen(void)
{
   s16b     i, x, y;
   byte     a = 0;
   char     c = ' ';
   bool     okay = TRUE;
   FILE    *fff;
   char     buf[1024];

   /* Build the path name */
   strcpy(buf, ANGBAND_DIR_USER);
   strcat(buf, "dump.txt");

   /* Append to the file */
   fff = my_fopen(buf, "r");

   /* Oops */
   if (!fff) return;


   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Clear the screen */
   Term_clear();


   /* Load the screen */
   for (y = 0; okay && (y < 24); y++)
   {
      /* Get a line of data */
      if (my_fgets(fff, buf, 1024)) okay = FALSE;

      /* Show each row */
      for (x = 0; x < 79; x++)
      {
         /* Put the attr/char */
         Term_draw(x, y, TERM_WHITE, buf[x]);
      }
   }

   /* Get the blank line */
   if (my_fgets(fff, buf, 1024)) okay = FALSE;


   /* Dump the screen */
   for (y = 0; okay && (y < 24); y++)
   {
      /* Get a line of data */
      if (my_fgets(fff, buf, 1024)) okay = FALSE;

      /* Dump each row */
      for (x = 0; x < 79; x++)
      {
         /* Get the attr/char */
         (void)(Term_what(x, y, &a, &c));

         /* Look up the attr */
         for (i = 0; i < 16; i++)
         {
            /* Use attr matches */
            if (hack[i] == buf[x]) a = i;
         }

         /* Put the attr/char */
         Term_draw(x, y, a, c);
      }

      /* End the row */
      fprintf(fff, "\n");
   }

   /* Get the blank line */
   if (my_fgets(fff, buf, 1024)) okay = FALSE;


   /* Close it */
   my_fclose(fff);


   /* Message */
   msg_print("Screen dump loaded.");
   msg_print(NULL);


   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;
}


/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
   s16b x, y;

   byte a = 0;
   char c = ' ';

   FILE *fff;

   char buf[1024];


   /* Build the path name */
   strcpy(buf, ANGBAND_DIR_USER);
   strcat(buf, "dump.txt");

#if defined(MACINTOSH) && !defined(applec)
   _ftype = 'TEXT';
#endif

   /* Hack -- drop permissions */
   safe_setuid_drop();

   /* Append to the file */
   fff = my_fopen(buf, "w");

   /* Hack -- grab permissions */
   safe_setuid_grab();

   /* Oops */
   if (!fff) return;


   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();


   /* Dump the screen */
   for (y = 0; y < 24; y++)
   {
      /* Dump each row */
      for (x = 0; x < 79; x++)
      {
         /* Get the attr/char */
         (void)(Term_what(x, y, &a, &c));

         /* Dump it */
         buf[x] = c;
      }

      /* Terminate */
      buf[x] = '\0';

      /* End the row */
      fprintf(fff, "%s\n", buf);
   }

   /* Skip a line */
   fprintf(fff, "\n");


   /* Dump the screen */
   for (y = 0; y < 24; y++)
   {
      /* Dump each row */
      for (x = 0; x < 79; x++)
      {
         /* Get the attr/char */
         (void)(Term_what(x, y, &a, &c));

         /* Dump it */
         buf[x] = hack[a&0x0F];
      }

      /* Terminate */
      buf[x] = '\0';

      /* End the row */
      fprintf(fff, "%s\n", buf);
   }

   /* Skip a line */
   fprintf(fff, "\n");


   /* Close it */
   my_fclose(fff);


   /* Message */
   msg_print("Screen dump saved.");
   msg_print(NULL);


   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;
}

/*
 * Check the status of "artifacts"
 *
 */
void do_cmd_check_artifacts(cptr filename, bool interaction)
{
   s16b i, k, z, x, y;
   bool found_any = FALSE;
/* jk */
   s16b j;
   FILE *fff;
   char base_name[80];
   char temp_file[1024];
   bool okay[MAX_A_IDX];

   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open a new file */
      fff = my_fopen(filename, "at");
      fprintf(fff, "\n\nArtifacts seen:\n\n");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
   }

   /* Scan the artifacts */
   for (k = 0; k < a_number; k++)
   {
      artifact_type *a_ptr = &a_info[k];

      /* Default */
      okay[k] = FALSE;

      /* Skip "empty" artifacts */
      if (!a_ptr->name) continue;

      /* Skip "uncreated" artifacts */
      if (!a_ptr->cur_num) continue;
      
      /* is this an artifact that we know? */
      if (! (a_ptr->ident & ID_KNOWN)) continue;

      /* Assume okay */
      okay[k] = TRUE;
   }

   /* Check the dungeon for artifacts that we haven't found yet */
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         /* Process objects */
         for (j=objects_on_floor(x,y)-1;j>=0;j--)
         {
            object_type *i_ptr = get_item_pointer_floor_xy(j,x,y);
            /* Ignore non-artifacts */
            if (!artifact_p(i_ptr)) continue;
            /* Ignore known items */
            if (object_known_p(i_ptr)) continue;

            /* Note the artifact */
            okay[i_ptr->name1] = FALSE;
         }
      }
   }

   /* Check the inventory for artifacts that we haven't found yet */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      object_type *i_ptr = &inventory[i];

      /* Ignore non-objects */
      if (!i_ptr->k_idx) continue;

      /* Ignore non-artifacts */
      if (!artifact_p(i_ptr)) continue;

      /* Ignore known items */
      if (object_known_p(i_ptr)) continue;

      /* Note the artifact */
      okay[i_ptr->name1] = FALSE;
   }

   /* Scan the artifacts */
   for (k = 0; k < a_number; k++)
   {
      artifact_type *a_ptr = &a_info[k];

      /* List "dead" ones */
      if (!okay[k]) continue;

      /* Paranoia */
      strcpy(base_name, "Unknown Artifact");

      /* Obtain the base object type */
      z = lookup_kind(a_ptr->tval, a_ptr->sval);

      /* Real object */
      if (z)
      {
         object_type forge;

         /* strange things happen otherwise, you have been warned! */
         forge.spell_set = 0;
         invwipe(&forge);

         /* Create the object */
         invcopy(&forge, z);

         /* Create the artifact */
         forge.name1 = k;

         /* Describe the artifact */
         object_desc_store(base_name, &forge, FALSE, 0);
      }

      /* Hack -- Build the artifact name */
      fprintf(fff, "     The %s\n", base_name);
      found_any = TRUE;
   }
   if (found_any == FALSE)
   {
      fprintf(fff, "You have not seen (or identified) any artifacts!\n");
   }

   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Artifacts Seen");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/*
 * print an header for do_cmd_check_items
 * below here
 */
void print_identified_items_header(FILE *fff, s16b tval)
{
   switch (tval)
   {
      case TV_NOTHING: fprintf(fff,"\nNothings:\n\n"); break;
      case TV_SKELETON: fprintf(fff, "Skeletons:\n\n"); break;
      case TV_BOTTLE: fprintf(fff,"\nBottles:\n\n"); break;
      case TV_JUNK: fprintf(fff,"\nWorthless Junk:\n\n"); break;
      case TV_SPIKE: fprintf(fff,"\nSpikes:\n\n"); break;
      case TV_CHEST: fprintf(fff,"\nChests:\n\n"); break;
      case TV_SHOT: fprintf(fff,"\nShots:\n\n"); break;
      case TV_ARROW: fprintf(fff,"\nArrows:\n\n"); break;
      case TV_BOLT: fprintf(fff,"\nBolts:\n\n"); break;
      case TV_BOW: fprintf(fff,"\nBows:\n\n"); break;
      case TV_DIGGING: fprintf(fff,"\nDiggers:\n\n"); break;
      case TV_HAFTED: fprintf(fff,"\nHafted Weapons:\n\n"); break;
      case TV_POLEARM: fprintf(fff,"\nPolearms:\n\n"); break;
      case TV_SWORD: fprintf(fff,"\nSwords:\n\n"); break;
      case TV_BOOTS: fprintf(fff,"\nBoots:\n\n"); break;
      case TV_GLOVES: fprintf(fff,"\nGloves:\n\n"); break;
      case TV_HELM: fprintf(fff,"\nHelms:\n\n"); break;
      case TV_CROWN: fprintf(fff,"\nCrowns:\n\n"); break;
      case TV_SHIELD: fprintf(fff,"\nShields:\n\n"); break;
      case TV_CLOAK: fprintf(fff,"\nCloaks:\n\n"); break;
      case TV_SOFT_ARMOR: fprintf(fff,"\nSoft Armor:\n\n"); break;
      case TV_HARD_ARMOR: fprintf(fff,"\nHard Armor:\n\n"); break;
      case TV_DRAG_ARMOR: fprintf(fff,"\nDragon Armor:\n\n"); break;
      case TV_LITE: fprintf(fff,"\nLite:\n\n"); break;
      case TV_AMULET: fprintf(fff,"\nAmulets:\n\n"); break;
      case TV_RING: fprintf(fff,"\nRings:\n\n"); break;
      case TV_STAFF: fprintf(fff,"\nStaffs:\n\n"); break;
      case TV_WAND: fprintf(fff,"\nWands:\n\n"); break;
      case TV_ROD: fprintf(fff,"\nRods:\n\n"); break;
      case TV_SCROLL: fprintf(fff,"\nScrolls:\n\n"); break;
      case TV_SPELL: fprintf(fff,"\nSpells:\n\n"); break;
      case TV_POTION: fprintf(fff,"\nPotions:\n\n"); break;
      case TV_FLASK: fprintf(fff,"\nFlasks:\n\n"); break;
      case TV_FOOD: fprintf(fff,"\nFood:\n\n"); break;
      case TV_BOOK: fprintf(fff,"\nBooks:\n\n"); break;
      case TV_GOLD: fprintf(fff,"\nValuables:\n\n"); break;
      case TV_CORPSE: fprintf(fff,"\nCorpses:\n\n"); break;
   }
}

/*
 * Check the status of "identified items"
 *
 */
void do_cmd_check_items(cptr filename, bool interaction)
{
   s16b k, z, prev_tval;
   bool identified_any = FALSE;

   FILE *fff;
   char base_name[80];
   char temp_file[1024];

   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open a new file */
      fff = my_fopen(filename, "at");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
      fprintf(fff, "\n\nIdentified Items:\n\n");
   }

   prev_tval = -1;

   /* Scan the artifacts */
   for (k = 0; k < k_number; k++)
   {
      object_kind *k_ptr = &k_info[k];

      /* Skip "empty" items */
      if (!k_ptr->name) continue;
      if (!k_ptr->aware) continue;
      if (!k_ptr->flavor) continue;

      /* Obtain the base object type */
      z = lookup_kind(k_ptr->tval, k_ptr->sval);

      /* Real object */
      if (z)
      {
         object_type forge;

         /* strange things happen otherwise, you have been warned! */
         forge.spell_set = 0;
         invwipe(&forge);

         /* Create the object */
         invcopy(&forge, z);

         /* Describe the item */
         if ((k_ptr->tval == TV_SCROLL) || (k_ptr->tval == TV_SPELL))
         {
            object_desc(base_name, &forge, FALSE, 1);
         }
         else
         {
            object_desc(base_name, &forge, FALSE, 0);
         }
      }

      /* Hack -- Build the item name */
      if (k_ptr->tval != prev_tval)
      {
         prev_tval = k_ptr->tval;
         print_identified_items_header(fff, k_ptr->tval);
      }
      fprintf(fff, "     %s\n", base_name);
      identified_any = TRUE;
   }
   if (identified_any == FALSE)
   {
      fprintf(fff,"You have not identified any items.\n");
   }

   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Identified Items");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/*
 * Check the status of "uniques"
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
void do_cmd_check_uniques(cptr filename, bool interaction)
{
   s16b k;
   bool seen_any = FALSE;

   FILE *fff;

   char temp_file[1024];

   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open a new file */
      fff = my_fopen(filename, "at");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
      fprintf(fff, "\n\nUniques seen:\n\n");
   }

   /* Scan the monster races */
   for (k = 1; k < r_number; k++)
   {
       monster_race *r_ptr = &r_info[k];

       /* Only print Uniques */
       if (r_ptr->flags1 & RF1_UNIQUE)
       {
           bool dead = (r_ptr->max_num == 0);

           /* Only display "known" uniques */
           if (dead || cheat_know || r_ptr->r_sights)
           {
               /* Print a message */
               fprintf(fff, "     %s is %s\n",
                       (r_name + r_ptr->name),
                       (dead ? "dead" : "alive"));
               seen_any = TRUE;
           }
       }
   }
   if (seen_any == FALSE)
   {
      fprintf(fff,"You have not seen any uniques, let alone killed some!\n");
   }

   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Known Uniques");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/*
 * Check the status of "uniques"
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
void do_cmd_check_spells(cptr filename, bool interaction)
{
   s16b s;
   bool cast_any = FALSE;

   FILE *fff;

   char temp_file[1024];

   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open a new file */
      fff = my_fopen(filename, "at");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
      fprintf(fff, "\n\nSpells Cast:\n\n");
   }

   /* Scan the monster races */
   for (s = 1; s < s_number; s++)
   {
       spell_type *s_ptr = &s_info[s];

       if (s_ptr->numcast == 0) continue;

       /* Print a message */
       fprintf(fff, "%12ld %s\n", s_ptr->numcast, s_name + s_ptr->name);
       cast_any = TRUE;
   }
   if (cast_any == FALSE)
   {
      fprintf(fff,"You have not cast any spells!\n");
   }

   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Spells Cast");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/* 
 * hook to sort events.
 *
 * primary key: .turn
 * secondary key: .type
 */
bool ang_sort_comp_hook_progess_index(vptr u, vptr v, s16b a, s16b b)
{
   event_type *event = (event_type *)(u);

   /* make sure experience gains are sorted *before* the monster slay events */
   if (event[a].turn == event[b].turn)
   {
      /* if we gain multiple levels in one turn, make sure that is sorted correctly */
      if (event[a].type == event[b].type)
      {
         return (event[a].index <= event[b].index);
      }
      return (event[a].type <= event[b].type);
      
   }   

   return (event[a].turn <= event[b].turn);
}

void ang_sort_swap_hook_progress_index(vptr u, vptr v, s16b a, s16b b)
{ 
   event_type *event = (event_type *)(u);
   event_type temp;
   
   temp = event[a];
   event[a] = event[b];
   event[b] = temp;
}

/* this adds a line to a file, \n is not added automatically!!!
 *
 * with a possible indent after the first line, so you get lines like
 *
 * 01234567 dis is a long line
 *          which continues here
 */
void add_and_wrap_line(FILE *fff, char *line, s16b width, s16b indent)
{
   s16b pos, i;
 
   if (strlen(line) <= width)
   {
      fprintf (fff, "%s", line);
      return;
   }
   /* add the first part of the line */
   pos = width;
   while (line[pos] != ' ') pos--;
   line[pos]=0;
   fprintf (fff, "%s\n", line);
   line = line + pos + 1;
 
   /* now for the rest of it */
   while (strlen(line) > (width - indent))
   {
      pos = width-indent;
      while (line[pos] != ' ') pos--;
      line[pos]=0;
      for (i=0; i < indent; i++) fprintf (fff, " ");
      fprintf (fff, "%s\n", line);
      line = line + pos + 1;
   }
   /* and the last part */
   for (i=0; i < indent; i++) fprintf (fff, " ");
   fprintf(fff, line);
}
    
/*
 * Check the progress of the player
 *
 * this gathers all sorts of events, then sorts them and prints them in
 * a pretty way.
 */
void do_cmd_check_progress(cptr filename, bool interaction)
{
   u32b t;
   s16b i, eventnum = 0;

   FILE *fff;

   char temp_file[1024];
   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open an existing file (used for char-dumps & such) */
      fff = my_fopen(filename, "at");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
      fprintf(fff, "\n\nProgress:\n\n");
   }

   for (i=0; i < a_number; i++)
   { 
      if (a_info[i].log.found_when == 0) continue;
      event[eventnum].turn = a_info[i].log.found_when;
      event[eventnum].type = EVENT_ARTIFACT;
      event[eventnum++].index = i;
   }

   for (i=0; i < e_number; i++)
   { 
      if (e_info[i].log.found_when == 0) continue;
      event[eventnum].turn = e_info[i].log.found_when;
      event[eventnum].type = EVENT_EGO_ITEM;
      event[eventnum++].index = i;
   }

   /* make sure to test inventory & home before testing all objects */
   /* to prevent sequences like:                                    */
   /* 4190 you found your first green potion                        */
   /* 4190 you found the green potion that is now in your home      */
   for (i=0; i < INVEN_TOTAL; i++)
   { 
      if (inventory[i].log.found_when == 0) continue;
      if (inventory[i].k_idx == 0) continue;
      event[eventnum].turn = inventory[i].log.found_when;
      event[eventnum].type = EVENT_INVENTORY;
      event[eventnum++].index = i;
      /* don't mark this even as an object event too */
      if (k_info[inventory[i].k_idx].log.found_when == inventory[i].log.found_when)
      {
         k_info[inventory[i].k_idx].log.found_when = 0;
      }
   }

   for (i=0; i < STORE_INVEN_MAX; i++)
   { 
      if (store[0].stock[i].log.found_when == 0) continue;
      if (store[0].stock[i].k_idx == 0) continue;
      event[eventnum].turn = store[0].stock[i].log.found_when;
      event[eventnum].type = EVENT_HOME;
      event[eventnum++].index = i;
      /* don't mark this even as an object event too */
      if (k_info[store[0].stock[i].k_idx].log.found_when == store[0].stock[i].log.found_when)
      {
         k_info[store[0].stock[i].k_idx].log.found_when = 0;
      }
   }
   
   for (i=0; i < k_number; i++)
   { 
      if (k_info[i].log.found_when == 0) continue;
      event[eventnum].turn = k_info[i].log.found_when;
      event[eventnum].type = EVENT_OBJECT;
      event[eventnum++].index = i;
   }

   for (i=0; i < PY_MAX_LEVEL; i++)
   {
      if (level_reached[i] == 0) continue;
      event[eventnum].turn = level_reached[i];
      event[eventnum].type = EVENT_PLAYERLEVEL;
      event[eventnum++].index = i;
   }

   for (i=0; i < MAX_LEVEL; i++)
   {
      if (level_info[i].first_visit == 0) continue;
      event[eventnum].turn = level_info[i].first_visit;
      event[eventnum].type = EVENT_DUNGEONENTER;
      event[eventnum++].index = i;
   }

   for (i=0; i < MAX_LEVEL; i++)
   {
      if (level_info[i].last_visit == 0) continue;
      event[eventnum].turn = level_info[i].last_visit;
      event[eventnum].type = EVENT_DUNGEONLEAVE;
      event[eventnum++].index = i;
   }

   for (i=0; i < r_number; i++)
   { 
      if (r_info[i].first_kill == 0) continue;
      event[eventnum].turn = r_info[i].first_kill;
      event[eventnum].type = EVENT_MONSTER;
      event[eventnum++].index = i;
   }

   ang_sort_comp = ang_sort_comp_hook_progess_index;
   ang_sort_swap = ang_sort_swap_hook_progress_index;
   ang_sort(&event, NULL, eventnum);

   fprintf(fff, "%12ld You started your campaign.\n", 0L);

   for (t = 0; t < eventnum; t++)
   {
      switch (event[t].type)
      {
         case EVENT_ARTIFACT    : add_and_wrap_line(fff,
                                                    format("%12ld You found your %s.\n",
                                                            event[t].turn, a_name + a_info[event[t].index].name), 80, 13);
                                  break;
         case EVENT_EGO_ITEM    : add_and_wrap_line(fff,
                                                    format("%12ld You found your first weapon of %s.\n",
                                                            event[t].turn, e_name + e_info[event[t].index].name), 80, 13);
                                  break;
         case EVENT_OBJECT      : 
              {  
                 object_type forge;
                 char        base_name[255];

                 /* strange things happen otherwise, you have been warned! */
                 forge.spell_set = 0;
                 invwipe(&forge);

                 /* Create the object */
                 invcopy(&forge, event[t].index);
                 if (k_info[event[t].index].flavor > 0)
                 {
                    object_desc(base_name, &forge, FALSE, 3);
                 }
                 else
                 {
                    object_desc(base_name, &forge, FALSE, 1);
                 }
                 add_and_wrap_line(fff, format("%12ld You found your first %s.\n",
                                               event[t].turn, base_name), 80, 13);
                 break;
              }
         case EVENT_INVENTORY   : 
              {  
                 char        base_name[255];
                 object_desc(base_name, &inventory[event[t].index], FALSE, 3);
                 add_and_wrap_line(fff, format("%12ld You found the %s that you are %s.\n",
                                               event[t].turn, base_name, describe_use(event[t].index)), 80, 13);
                 break;
              }
         case EVENT_HOME        : 
              {  
                 char        base_name[255];
                 object_desc(base_name, &store[0].stock[event[t].index], FALSE, 3);
                 add_and_wrap_line(fff, format("%12ld You found the %s which is in your home.\n",
                                               event[t].turn, base_name), 80, 13);
                 break;
              }
         case EVENT_PLAYERLEVEL : fprintf (fff, "%12ld You reached experience level %d.\n",
                                           event[t].turn, event[t].index);
                                  break;
         case EVENT_DUNGEONENTER: if (depth_in_feet)
                                  {
                                     fprintf (fff, "%12ld You first entered dungeon level %d ft.\n",
                                     event[t].turn, event[t].index * 50);
                                  }
                                  else
                                  {
                                     fprintf (fff, "%12ld You first entered dungeon level %d.\n",
                                     event[t].turn, event[t].index);
                                  }
                                  break;
         /* last_visit is also used when we restore from savefile.... */
         case EVENT_DUNGEONLEAVE: if ( (depth_in_feet) && (event[t].index != p_ptr->mdepth) )
                                  {
                                     fprintf (fff, "%12ld You last left dungeon level %d ft.\n",
                                     event[t].turn, event[t].index * 50);
                                  }
                                  else if (event[t].index != p_ptr->mdepth)
                                  {
                                     fprintf (fff, "%12ld You last left dungeon level %d.\n",
                                     event[t].turn, event[t].index);
                                  }
                                  break;
         
         case EVENT_MONSTER     : if (r_info[event[t].index].flags1 & RF1_UNIQUE)
                                  {
                                     fprintf (fff, "%12ld You slew %s.\n",
                                              event[t].turn, r_name + r_info[event[t].index].name);
                                  }
                                  else
                                  {
                                     fprintf (fff, "%12ld You slew your first %s.\n",
                                              event[t].turn, r_name + r_info[event[t].index].name);
                                  }

      }
   }
   if (depth_in_feet)
   {
      fprintf (fff, "%12ld You are now on dungeon level %d ft\n",
               turn, p_ptr->mdepth * 50);
   }
   else
   {
      fprintf (fff, "%12ld You are now on dungeon level %d\n",
               turn, p_ptr->mdepth);
   }
   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Progress:");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/*
 * Check the status of kills
 *
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
void do_cmd_check_kills(cptr filename, bool interaction)
{
   s16b k;
   u32b total = 0;

   FILE *fff;

   char temp_file[1024];
   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open an existing file (used for char-dumps & such) */
      fff = my_fopen(filename, "at");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
      fprintf(fff, "\n\nMonsters seen:\n\n");
   }
   /* count total over all monsters */
   for (k = 1; k < r_number; k++)
   {
      monster_race *r_ptr = &r_info[k];
      total += r_ptr->r_pkills;
   }
   fprintf(fff,"You ventured up to level %d\n\n", p_ptr->max_dlv);

   if (save_levels)
   {
      for (k=0;k<MAX_LEVEL;k++)
      {
         if (level_info[k].saved)
            fprintf(fff,"You visited level %d\n", k);
      }
      fprintf(fff, "You are now on level %d/%d, and your current visit lasts %ld turn%s\n",
                    p_ptr->mdepth, p_ptr->sdepth, turn - level_info[p_ptr->mdepth].last_visit,
                    (turn-level_info[p_ptr->mdepth].last_visit)==1?"":"s");
   }
   else
   {
      fprintf(fff, "You are now on level %d/%d, and you have been here for %ld turn%s\n",
                    p_ptr->mdepth, p_ptr->sdepth, turn - level_info[p_ptr->mdepth].last_visit,
                    (turn-level_info[p_ptr->mdepth].last_visit)==1?"":"s");
   }
   fprintf(fff,"You killed %ld monster%s in total, in %ld turn%s.\n\n",
               total, total==1?"":"s", turn, (turn==1)?"":"s");

   /* Scan the monster races */
   for (k = 1; k < r_number; k++)
   {
      monster_race *r_ptr = &r_info[k];
      cptr wd_he[3] = { "it", "he", "she" };
      s16b msex = 0;

      if (r_ptr->flags1 & RF1_FEMALE)
         msex = 2;
      else if (r_ptr->flags1 & RF1_MALE)
         msex = 1;

      /* if we are part of a dump, the uniques have already been dumped */
      if ((r_ptr->flags1 & RF1_UNIQUE) && (!interaction)) continue;

      /* Treat uniques differently */
      if (r_ptr->flags1 & RF1_UNIQUE)
      {
         /* Hack -- Determine if the unique is "dead" */
         bool dead = (r_ptr->max_num == 0);

         /* We've been killed... */
         if (r_ptr->r_deaths && (dead || show_unkilled))
         {
            /* Killed ancestors */
            fprintf(fff, format("%s has slain %d of your ancestors",
                                r_name + r_ptr->name, r_ptr->r_deaths));

            /* But we've also killed it */
            if (dead)
            {
               fprintf(fff, format(", but you have avenged %s!\n",
                           plural(r_ptr->r_deaths, "your ancestor", "them")));
            }

            /* Unavenged (ever) */
            else
            {
               fprintf(fff, format(", who %s unavenged.\n",
                           plural(r_ptr->r_deaths, "remains", "remain")));
            }
         }

         /* Dead unique who never hurt us */
         else if (dead)
         {
            fprintf(fff,"You have slain %s.\n", r_name + r_ptr->name);
         }
         else if (r_ptr->r_sights && show_unkilled)
         {
            fprintf(fff,"%s is still alive.\n",
                        r_name + r_ptr->name);
         }
      }

      /* Not unique, but killed us */
      else if (r_ptr->r_deaths)
      {
          /* Dead ancestors */
          cptr m_name = r_name + r_ptr->name;

          fprintf(fff, format("%d of your ancestors %s been killed by %s%s, ",
                      r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have"),
                      is_a_vowel(m_name[0])?"an":"a",m_name));

          /* Some kills this life */
          if (r_ptr->r_pkills)
          {
              fprintf(fff, format("and you have exterminated %d of the creatures.\n",
                          r_ptr->r_pkills));
          }

          /* Some kills past lives */
          else if (r_ptr->r_tkills)
          {
              fprintf(fff, format("and %s have exterminated %d of the creatures.\n",
                          "your ancestors", r_ptr->r_tkills));
          }

          /* No kills */
          else
          {
              fprintf(fff, format("and %s is not ever known to have been defeated.\n",
                          wd_he[msex]));
          }
      }

      /* Normal monsters */
      else
      {
          /* Killed some this life */
          if (r_ptr->r_pkills)
          {
             cptr name = r_name + r_ptr->name;
             if (r_ptr->r_pkills>1)
             {
                if ( (name[strlen(name)-4]=='m') &&
                          (name[strlen(name)-3]=='a') &&
                          (name[strlen(name)-2]=='s') &&
                          (name[strlen(name)-1]=='s'))
                {
                   /* worm mass becomes worm masses */
                   fprintf(fff, format("You have killed %d %ses.\n",
                               r_ptr->r_pkills, r_name + r_ptr->name));
                }
                else if ( (name[strlen(name)-3]=='m') &&
                          (name[strlen(name)-2]=='a') &&
                          (name[strlen(name)-1]=='n'))
                {
                   char tmpstr[200];
                   strcpy(tmpstr, name);
                   tmpstr[strlen(tmpstr)-2]='e';
                   /* kobold marksmans -> kobold marksmen */
                   fprintf(fff, format("You have killed %d %s.\n",
                               r_ptr->r_pkills, tmpstr));
                }
                else if ( (name[strlen(name)-3]=='o') &&
                          (name[strlen(name)-2]=='u') &&
                          (name[strlen(name)-2]=='s') &&
                          (name[strlen(name)-1]=='e'))
                {
                   char tmpstr[200];
                   strcpy(tmpstr, name);
                   tmpstr[strlen(tmpstr)-3]='i';
                   tmpstr[strlen(tmpstr)-2]='c';
                   tmpstr[strlen(tmpstr)-1]='e';
                   tmpstr[strlen(tmpstr)]='\0';
                   /* mouses -> mice */
                   fprintf(fff, format("You have killed %d %s.\n",
                               r_ptr->r_pkills, tmpstr));
                }
                else if ( name[strlen(name)-1]=='s')
                {
                   /* creeping silver coinss -> creeping silver coins */
                   fprintf(fff, format("You have killed %d %s.\n",
                               r_ptr->r_pkills, name));
                }
                else
                {
                   fprintf(fff, format("You have killed %d %ss.\n",
                               r_ptr->r_pkills, r_name + r_ptr->name));
                }
             }
          }

          /* Killed some last life */
          else if (r_ptr->r_tkills && show_unkilled)
          {
              fprintf(fff,format("Your ancestors have killed %d of these creatures.\n",
                          r_ptr->r_tkills));
          }

          /* Killed none */
          else if (show_unkilled)
          {
             cptr name = r_name + r_ptr->name;
             if ( (name[strlen(name)-4]=='m') &&
                       (name[strlen(name)-3]=='a') &&
                       (name[strlen(name)-2]=='s') &&
                       (name[strlen(name)-1]=='s'))
             {
                /* worm mass becomes worm masses */
                fprintf(fff, format("No battles to the death with %ses are recalled.\n",
                                    r_name + r_ptr->name));
             }
             else
             {
                fprintf(fff, format("No battles to the death with %ss are recalled.\n",
                                    r_name + r_ptr->name));
             }
          }
      }
   }

   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Monsters Seen:");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/*
 * this routine prints out 1 intrinsic
 */
static bool print_1_intrinsic(FILE *fff, u64b value, cptr text, s16b flagset)
{
   s16b cnt = 0, i;
   s16b index[INVEN_TOTAL];
   char temp[80];

   /* 24 empty items */
   char *item[] = { "","","","","","","","","","",
                   "","","","","","","","","","",
                   "","","","",
                   "weapon", "missile weapon", "left ring", "right ring", "neckwear",
                   "light", "body armour", "cloak", "shield", "helmet", "gloves",
                   "boots", "ammunition" };
   if (p_ptr->pclass == CLASS_GLADIATR)
   {
      item[INVEN_ARM]="2nd weapon";
   }
   if (p_ptr->pclass == CLASS_HIGHPRST)
   {
      item[INVEN_WIELD]="1st ring left";
      item[INVEN_BOW]="2nd ring left";
      item[INVEN_LEFT]="1st ring right";
      item[INVEN_RIGHT]="2nd ring right";
   }

   /* find out which items contribute */
   /* for empty or un*id*ed objects, flags=0 */
   for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      u64b f[4];
      object_type *i_ptr = &inventory[i];

      /* Skip empty items */
      if (!i_ptr->k_idx) continue;

      /* only *identified* items */
      if (!(i_ptr->ident & ID_MENTAL)) continue;

      object_flags(i_ptr, &f[1], &f[2], &f[3]);

      if (f[flagset] & value) index[cnt++]=i;
   }

   /* nothing found */
   if (cnt==0) return (FALSE);

   /* one item is simple */
   if (cnt==1)
   {
      object_type *i_ptr = &inventory[index[0]];
      strcpy(temp,"");
      if ((flagset==1) && (i_ptr->p1val != 0) && (i_ptr->p2val != 0))
      {
         sprintf(temp,"%s (%+ld/%+d)", item[index[0]], i_ptr->p1val, i_ptr->p2val);
      }
      else if ((flagset==1) && (value & TR1_P1VAL_MASK))
      {
         sprintf(temp,"%s (%+ld)", item[index[0]], inventory[index[0]].p1val);
      }
      else
         sprintf(temp,"%s", item[index[0]]);

      fprintf(fff,"%s %s.\n", text, temp);
   }
   /* more items is not so simple */
   else if (cnt>1)
   {
      char items[160];
      s16b numcr = 0;
      strcpy(items,"");
      for (i=0; i<cnt; i++)
      {
         object_type *i_ptr = &inventory[index[i]];

         strcpy(temp,"");
         if ((flagset==1) && (i_ptr->p1val != 0) && (i_ptr->p2val != 0))
         {
            sprintf(temp,"%s (%+ld/%+d)", item[index[i]], i_ptr->p1val, i_ptr->p2val);
         }
         else if ((flagset==1) && (value & TR1_P1VAL_MASK))
         {
            sprintf(temp,"%s (%+ld)", item[index[i]], inventory[index[i]].p1val);
         }
         else
            sprintf(temp,"%s", item[index[i]]);

         strcat(items,temp);
         if (i==(cnt-2))
         {
            strcat(items," and ");
         }
         else if (i<(cnt-2))
         {
            strcat(items,", ");
         }
      }

      /* we may have a long string, that will need to be broken down into multiple lines */
      /* insert some \n characters after 80 characters i                                 */
      /* we assume there are some spaces in items (ie don't call it with text itself so  */
      /* big that items doesn't count!!                                                  */
      numcr=0;
      if ((strlen(text) + strlen(items)) > 78)
      {
         s16b pos = 78 - strlen(text);
         /* are we still in the valid range in items? If so, search for a space from the */
         /* end and insert a line-break there */
         while (pos < strlen(items))
         {
            while (items[pos] != ' ') pos--;
            items[pos] = '\n';
            /* now do it again if necessary */
            pos = pos + 78; 
         }
      }
      fprintf(fff,"%s %s.\n", text, items);
   }
   return (TRUE);
}

/*
 * A structure to hold a tval and its description
 */
typedef struct flag_desc
{
   u64b       flag;
   s16b       desc_type;
   cptr       desc;
} flag_desc;

static flag_desc flags1[] =
{
   { TR1_STR1,         0, "strength" },
   { TR1_INT1,         0, "intelligence" },
   { TR1_WIS1,         0, "wisdom" },
   { TR1_DEX1,         0, "dexterity" },
   { TR1_CON1,         0, "constitution" },
   { TR1_CHR1,         0, "charisma" },
   { TR1_STEALTH1,     0, "stealth" },
   { TR1_SEARCH1,      0, "searching ability" },
   { TR1_INFRA1,       0, "infravision" },
   { TR1_TUNNEL1,      0, "tunneling ability" },
   { TR1_SPEED1,       0, "speed" },
   { TR1_BLOWS1,       0, "attack speed" },
   { TR1_MIGHT1,       0, "fire power" },
   { TR1_SHOTS1,       0, "fire speed" },
   { TR1_MAGIC1,       0, "magical ability" },
   { TR1_LITE1,        0, "permanent ligth radius" },
   { TR1_VAMPIRIC1,    0, "health when fighting" },
   { TR1_STR2,         0, "strength" },
   { TR1_INT2,         0, "intelligence" },
   { TR1_WIS2,         0, "wisdom" },
   { TR1_DEX2,         0, "dexterity" },
   { TR1_CON2,         0, "constitution" },
   { TR1_CHR2,         0, "charisma" },
   { TR1_STEALTH2,     0, "stealth" },
   { TR1_SEARCH2,      0, "searching ability" },
   { TR1_INFRA2,       0, "infravision" },
   { TR1_TUNNEL2,      0, "tunneling ability" },
   { TR1_SPEED2,       0, "speed" },
   { TR1_BLOWS2,       0, "attack speed" },
   { TR1_MIGHT2,       0, "fire power" },
   { TR1_SHOTS2,       0, "fire speed" },
   { TR1_MAGIC2,       0, "magical ability" },
   { TR1_LITE2,        0, "permanent ligth radius" },
   { TR1_VAMPIRIC2,    0, "health when fighting" },
   { TR1_SLAY_ANIMAL, 1, "animals" },
   { TR1_KILL_ANIMAL, 2, "animals" },
   { TR1_SLAY_EVIL,   1, "evil creatures" },
   { TR1_KILL_EVIL,   2, "evil creatures" },
   { TR1_SLAY_UNDEAD, 1, "undead beings" },
   { TR1_KILL_UNDEAD, 2, "undead beings" },
   { TR1_SLAY_DEMON,  1, "demons" },
   { TR1_KILL_DEMON,  2, "demons" },
   { TR1_SLAY_ORC,    1, "orcs" },
   { TR1_KILL_ORC,    2, "orcs" },
   { TR1_SLAY_TROLL,  1, "trolls" },
   { TR1_KILL_TROLL,  2, "trolls" },
   { TR1_SLAY_GIANT,  1, "giants" },
   { TR1_KILL_GIANT,  2, "giants" },
   { TR1_SLAY_DRAGON, 1, "dragonkind" },
   { TR1_KILL_DRAGON, 2, "dragonkind" },
   { TR1_IMPACT,      3, "earthquakes" },
   { TR1_BRAND_ACID,  4, "acid" },
   { TR1_BRAND_ELEC,  4, "electricity" },
   { TR1_BRAND_FIRE,  4, "fire" },
   { TR1_BRAND_COLD,  4, "cold"},
   { 0LL, 0, NULL }
};

static flag_desc flags2[] =
{
   { TR2_SUST_STR,      5, "strength"},
   { TR2_SUST_INT,      5, "intelligence"},
   { TR2_SUST_WIS,      5, "wisdom"},
   { TR2_SUST_DEX,      5, "dexterity"},
   { TR2_SUST_CON,      5, "constitution"},
   { TR2_SUST_CHR,      5, "charisma"},
   { TR2_IM_ACID,       6, "acid"},
   { TR2_IM_ELEC,       6, "electricity"},
   { TR2_IM_FIRE,       6, "fire"},
   { TR2_IM_COLD,       6, "cold"},
   { TR2_FREE_ACT,      7, "move freely"},
   { TR2_HOLD_LIFE,     7, "hold on to your experience"},
   { TR2_RES_ACID,      8, "acid vapors"},
   { TR2_RES_ELEC,      8, "bolts of electricity"},
   { TR2_RES_FIRE,      8, "burning gasses"},
   { TR2_RES_COLD,      8, "cold breaths"},
   { TR2_RES_POIS,      8, "poisonous clouds"},
   { TR2_RES_FEAR,      8, "fear and terror"},
   { TR2_RES_LITE,      8, "bright lights"},
   { TR2_RES_DARK,      8, "darkness settling on your soul"},
   { TR2_RES_BLIND,     8, "blindness"},
   { TR2_RES_CONF,      8, "confusion"},
   { TR2_RES_SOUND,     8, "deafness"},
   { TR2_RES_SHARDS,    8, "shards"},
   { TR2_RES_NETHER,    8, "nether eating away your soul"},
   { TR2_RES_NEXUS,     8, "nexus swirling around you"},
   { TR2_RES_CHAOS,     8, "chaos taking over"},
   { TR2_RES_DISEN,     8, "disenchantment"},
   { 0LL, 0, NULL }
};

static flag_desc flags3[] =
{
   { TR3_FEATHER,         7, "land softly" },
   { TR3_SOMELITE,        7, "see the way ahead clearly" },
   { TR3_SEE_INVIS,       7, "see every monster" },
   { TR3_TELEPATHY,       7, "feel nearby monsters" },
   { TR3_SLOW_DIGEST,     7, "have a small appetite" },
   { TR3_REGEN,           7, "heal quickly" },
   { TR3_IGNORE_ACID,     9, "acid" },
   { TR3_IGNORE_ELEC,     9, "electricity" },
   { TR3_IGNORE_FIRE,     9, "fire" },
   { TR3_IGNORE_COLD,     9, "cold" },
   { TR3_ACTIVATE,       10, "have something extra" },
   { TR3_DRAIN_EXP,      10, "have trouble remembering" },
   { TR3_TELEPORT,       10, "move in surprising ways" },
   { TR3_AGGRAVATE,      10, "awake every monster around you" },
   { TR3_BLESSED,        10, "feel a slight glow" },
   { TR3_CURSED,         10, "feel a little cold" },
   { TR3_HEAVY_CURSE,    10, "shiver" },
   { TR3_PERMA_CURSE,    10, "feel very cold inside" },
   { 0LL, 0, NULL }
};

typedef struct descr_type
{
   cptr before;
   cptr after;
} descr_type;

static descr_type description[] =
{
/*  0 */   { "Your ",                         " is affected by your" },
/*  1 */   { "You easily slay ",              " because of your" },
/*  2 */   { "You very easily slay ",         " because of your" },
/*  3 */   { "You cause ",                    " with your" },
/*  4 */   { "You do extra damage by ",       " with your" },
/*  5 */   { "Your ",                         " will never diminish while wearing your" },
/*  6 */   { "You will not be harmed by ",    " while wearing your" },
/*  7 */   { "You will ever ",                " while using your" },
/*  8 */   { "You will resist ",              " while wearing your" },
/*  9 */   { "You will see no damage from ",  " to your" },
/* 10 */   { "You will always ",              " whilst equipped with your" }
};


void do_cmd_check_intrinsics(cptr filename, bool interaction)
{
   s16b         k;
   FILE        *fff;
   char         temp_file[1024];
   bool         found_one = FALSE;

   if (interaction)
   {
      /* Temporary file */
      if (path_temp(temp_file, 1024)) return;
      /* Open a new file */
      fff = my_fopen(temp_file, "w");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", temp_file);
         return;
      }
   }
   else
   {
      /* Open a new file */
      fff = my_fopen(filename, "at");
      if (fff == NULL)
      {
         msg_format("Uh-oh. Unable to open file %s?", filename);
         return;
      }
      fprintf(fff, "\n\nPersonal intrinsics status:\n\n");
   }

   k=0;
   while (flags1[k].flag)
   {
      found_one |= print_1_intrinsic(fff, flags1[k].flag,
                        format("%s%s%s", description[flags1[k].desc_type].before,
                                         flags1[k].desc,
                                         description[flags1[k].desc_type].after), 1);
      k++;
   }
   k=0;
   while (flags2[k].flag)
   {
      found_one |= print_1_intrinsic(fff, flags2[k].flag,
                        format("%s%s%s", description[flags2[k].desc_type].before,
                                         flags2[k].desc,
                                         description[flags2[k].desc_type].after), 2);
      k++;
   }
   k=0;
   while (flags3[k].flag)
   {
      found_one |= print_1_intrinsic(fff, flags3[k].flag,
                        format("%s%s%s", description[flags3[k].desc_type].before,
                                         flags3[k].desc,
                                         description[flags3[k].desc_type].after), 3);
      k++;
   }

   if (!found_one)
      fprintf(fff, "You have no known intrinsics.\n\n");

   /* Close the file */
   my_fclose(fff);

   if (interaction)
   {
      /* Display the file contents */
      show_file(temp_file, "Intrinsics");

      /* Remove the file */
      fd_kill(temp_file);
   }
}

/*
 * Handle system commands
 *
 */
void do_cmd_system_command(void)
{
   s16b k;

   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Interact */
   while (1)
   {
      /* Clear screen */
      clear_screen();

      /* Why are we here */
      prt("Angband system commands", 0, 2);

      /* Give some choices */
      prt("(1) Macros", 5, 4);
      prt("(2) Preferences", 5, 5);
      prt("(3) Visuals", 5, 6);
      prt("(4) system interaction", 5, 7);
      prt("(5) Colors", 5, 8);
      prt("(6) Options", 5, 9);

      /* Prompt */
      prt("Command: ", 0, 11);

      /* Get command */
      k = inkey();

      /* Exit */
      if (k == ESCAPE) break;

      switch(k)
      {
         case '1': do_cmd_macros(); break;
         case '2': do_cmd_pref(); break;
         case '3': do_cmd_visuals(); break;
         case '4': Term_user(0); break;
         case '5': do_cmd_colors(); break;
         case '6': do_cmd_options(); break;
      }
   }

   /* Leave "icky" mode */
   character_icky = FALSE;

   /* Restore the screen - we may have fiddled the window flags for example */
   /* so a complete redraw is done here! */
   do_cmd_redraw();

}
