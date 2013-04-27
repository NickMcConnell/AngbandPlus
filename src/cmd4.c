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
 * A set of functions to maintain automatic dumps of various kinds.
 * -Mogami-
 *
 * remove_auto_dump(orig_file, mark)
 *     Remove the old automatic dump of type "mark".
 * auto_dump_printf(fmt, ...)
 *     Dump a formatted string using fprintf().
 * open_auto_dump(buf, mark)
 *     Open a file, remove old dump, and add new header.
 * close_auto_dump(void)
 *     Add a footer, and close the file.
 *
 *    The dump commands of original Angband simply add new lines to
 * existing files; these files will become bigger and bigger unless
 * an user deletes some or all of these files by hand at some
 * point.
 *
 *     These three functions automatically delete old dumped lines 
 * before adding new ones.  Since there are various kinds of automatic 
 * dumps in a single file, we add a header and a footer with a type 
 * name for every automatic dump, and kill old lines only when the 
 * lines have the correct type of header and footer.
 *
 *     We need to be quite paranoid about correctness; the user might 
 * (mistakenly) edit the file by hand, and see all their work come
 * to nothing on the next auto dump otherwise.  The current code only 
 * detects changes by noting inconsistencies between the actual number 
 * of lines and the number written in the footer.  Note that this will 
 * not catch single-line edits.
 */

/*
 *  Mark strings for auto dump
 */
static char auto_dump_header[] = "# vvvvvvv== %s ==vvvvvvv";
static char auto_dump_footer[] = "# ^^^^^^^== %s ==^^^^^^^";

/*
 * Variables for auto dump
 */
static FILE *auto_dump_stream;
static cptr auto_dump_mark;
static int auto_dump_line_num;

/*
 * Remove old lines automatically generated before.
 */
static void remove_auto_dump(cptr orig_file)
{
	FILE *tmp_fff, *orig_fff;

	char tmp_file[1024];
	char buf[1024];
	bool between_mark = FALSE;
	bool changed = FALSE;
	int line_num = 0;
	long header_location = 0;
	char header_mark_str[80];
	char footer_mark_str[80];
	size_t mark_len;

	/* Prepare a header/footer mark string */
	sprintf(header_mark_str, auto_dump_header, auto_dump_mark);
	sprintf(footer_mark_str, auto_dump_footer, auto_dump_mark);

	mark_len = strlen(footer_mark_str);

	/* Open an old dump file in read-only mode */
	orig_fff = my_fopen(orig_file, "r");

	/* If original file does not exist, nothing to do */
	if (!orig_fff) return;

	/* Open a new (temporary) file */
	tmp_fff = my_fopen_temp(tmp_file, 1024);

	if (!tmp_fff)
	{
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", tmp_file);
#else
	    msg_format("Failed to create temporary file %s.", tmp_file);
#endif
	    msg_print(NULL);
	    return;
	}

	/* Loop for every line */
	while (TRUE)
	{
		/* Read a line */
		if (my_fgets(orig_fff, buf, sizeof(buf)))
		{
			/* Read error: Assume End of File */

			/*
			 * Was looking for the footer, but not found.
			 *
			 * Since automatic dump might be edited by hand,
			 * it's dangerous to kill these lines.
			 * Seek back to the next line of the (pseudo) header,
			 * and read again.
			 */
			if (between_mark)
			{
				fseek(orig_fff, header_location, SEEK_SET);
				between_mark = FALSE;
				continue;
			}

			/* Success -- End the loop */
			else
			{
				break;
			}
		}

		/* We are looking for the header mark of automatic dump */
		if (!between_mark)
		{
			/* Is this line a header? */
			if (!strcmp(buf, header_mark_str))
			{
				/* Memorise seek point of this line */
				header_location = ftell(orig_fff);

				/* Initialize counter for number of lines */
				line_num = 0;

				/* Look for the footer from now */
				between_mark = TRUE;

				/* There are some changes */
				changed = TRUE;
			}

			/* Not a header */
			else
			{
				/* Copy orginally lines */
				fprintf(tmp_fff, "%s\n", buf);
			}
		}

		/* We are looking for the footer mark of automatic dump */
		else
		{
			/* Is this line a footer? */
			if (!strncmp(buf, footer_mark_str, mark_len))
			{
				int tmp;

				/*
				 * Compare the number of lines
				 *
				 * If there is an inconsistency between
				 * actual number of lines and the
				 * number here, the automatic dump
				 * might be edited by hand.  So it's
				 * dangerous to kill these lines.
				 * Seek back to the next line of the
				 * (pseudo) header, and read again.
				 */
				if (!sscanf(buf + mark_len, " (%d)", &tmp)
				    || tmp != line_num)
				{
					fseek(orig_fff, header_location, SEEK_SET);
				}

				/* Look for another header */
				between_mark = FALSE;
			}

			/* Not a footer */
			else
			{
				/* Ignore old line, and count number of lines */
				line_num++;
			}
		}
	}

	/* Close files */
	my_fclose(orig_fff);
	my_fclose(tmp_fff);

	/* If there are some changes, overwrite the original file with new one */
	if (changed)
	{
		/* Copy contents of temporary file */

		tmp_fff = my_fopen(tmp_file, "r");
		orig_fff = my_fopen(orig_file, "w");

		while (!my_fgets(tmp_fff, buf, sizeof(buf)))
			fprintf(orig_fff, "%s\n", buf);

		my_fclose(orig_fff);
		my_fclose(tmp_fff);
	}

	/* Kill the temporary file */
	fd_kill(tmp_file);

	return;
}


/*
 * Dump a formatted line, using "vstrnfmt()".
 */
static void auto_dump_printf(cptr fmt, ...)
{
	cptr p;
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Count number of lines */
	for (p = buf; *p; p++)
	{
		if (*p == '\n') auto_dump_line_num++;
	}

	/* Dump it */
	fprintf(auto_dump_stream, "%s", buf);
}


/*
 *  Open file to append auto dump.
 */
static bool open_auto_dump(cptr buf, cptr mark)
{

	char header_mark_str[80];

	/* Save the mark string */
	auto_dump_mark = mark;

	/* Prepare a header mark string */
	sprintf(header_mark_str, auto_dump_header, auto_dump_mark);

	/* Remove old macro dumps */
	remove_auto_dump(buf);

	/* Append to the file */
	auto_dump_stream = my_fopen(buf, "a");

	/* Failure */
	if (!auto_dump_stream) {
#ifdef JP
		msg_format("%s を開くことができませんでした。", buf);
#else
		msg_format("Failed to open %s.", buf);
#endif
		msg_print(NULL);

		/* Failed */
		return FALSE;
	}

	/* Start dumping */
	fprintf(auto_dump_stream, "%s\n", header_mark_str);

	/* Initialize counter */
	auto_dump_line_num = 0;

#ifdef JP
	auto_dump_printf("# *警告!!* 以降の行は自動生成されたものです。\n");
	auto_dump_printf("# *警告!!* 後で自動的に削除されるので編集しないでください。\n");
#else
	auto_dump_printf("# *Warning!*  The lines below are an automatic dump.\n");
	auto_dump_printf("# Don't edit them; changes will be deleted and replaced automatically.\n");
#endif

	/* Success */
	return TRUE;
}

/*
 *  Append foot part and close auto dump.
 */
static void close_auto_dump(void)
{
	char footer_mark_str[80];

	/* Prepare a footer mark string */
	sprintf(footer_mark_str, auto_dump_footer, auto_dump_mark);

#ifdef JP
	auto_dump_printf("# *警告!!* 以上の行は自動生成されたものです。\n");
	auto_dump_printf("# *警告!!* 後で自動的に削除されるので編集しないでください。\n");
#else
	auto_dump_printf("# *Warning!*  The lines above are an automatic dump.\n");
	auto_dump_printf("# Don't edit them; changes will be deleted and replaced automatically.\n");
#endif

	/* End of dump */
	fprintf(auto_dump_stream, "%s (%d)\n", footer_mark_str, auto_dump_line_num);

	/* Close */
	my_fclose(auto_dump_stream);

	return;
}

/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all of the
 * interesting updates and redraws that I can think of.
 *
 * This command is also used to "instantiate" the results of the user
 * selecting various things, such as graphics mode, so it must call
 * the "TERM_XTRA_REACT" hook before redrawing the windows.
 */
void do_cmd_redraw(void)
{
	int j;

	term *old = Term;


	/* Hack -- react to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Forget lite/view */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_STATS);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT | PW_FLOOR);

	/* Update playtime */
	update_playtime();

	/* Hack -- update */
	handle_stuff();


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
 * Recall the most recent message
 */
void do_cmd_message_one(void)
{
	/* Recall one message XXX XXX XXX */
	prt(format("> %s", message_str(0)), 0, 0);
}


/*
 * Show previous messages to the user	-BEN-
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
void do_cmd_messages(int num_now)
{
	int i, n;

	char shower_str[81];
	char finder_str[81];
	char back_str[81];
	cptr shower = NULL;
	int wid, hgt;
	int num_lines;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Number of message lines in a screen */
	num_lines = hgt - 4;

	/* Wipe finder */
	strcpy(finder_str, "");

	/* Wipe shower */
	strcpy(shower_str, "");

	/* Total messages */
	n = message_num();

	/* Start on first message */
	i = 0;

	/* Save the screen */
	screen_save();

	/* Clear screen */
	Term_clear();

	/* Process requests until done */
	while (1)
	{
		int j;
		int skey;

		/* Dump up to 20 lines of messages */
		for (j = 0; (j < num_lines) && (i + j < n); j++)
		{
			cptr msg = message_str(i+j);

			/* Dump the messages, bottom to top */
			c_prt((i + j < num_now ? TERM_WHITE : TERM_SLATE), msg, num_lines + 1 - j, 0);

			/* Hilite "shower" */
			if (shower && shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = my_strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-msg, num_lines + 1 - j, len, TERM_YELLOW, shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Erase remaining lines */
		for (; j < num_lines; j++)
		{
			Term_erase(0, num_lines + 1 - j, 255);
		}

		/* Display header XXX XXX XXX */
#ifdef JP
		/* translation */
		prt(format("以前のメッセージ %d-%d 全部で(%d)",
			   i, i + j - 1, n), 0, 0);
#else
		prt(format("Message Recall (%d-%d of %d)",
			   i, i + j - 1, n), 0, 0);
#endif

		/* Display prompt (not very informative) */
#ifdef JP
		prt("[ 'p' で更に古いもの, 'n' で更に新しいもの, '/' で検索, ESC で中断 ]", hgt - 1, 0);
#else
		prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]", hgt - 1, 0);
#endif

		/* Get a command */
		skey = inkey_special(TRUE);

		/* Exit on Escape */
		if (skey == ESCAPE) break;

		/* Hack -- Save the old index */
		j = i;

		switch (skey)
		{
		/* Hack -- handle show */
		case '=':
			/* Prompt */
#ifdef JP
			prt("強調: ", hgt - 1, 0);
#else
			prt("Show: ", hgt - 1, 0);
#endif

			/* Get a "shower" string, or continue */
			strcpy(back_str, shower_str);
			if (askfor(shower_str, 80))
			{
				/* Show it */
				shower = shower_str[0] ? shower_str : NULL;
			}
			else strcpy(shower_str, back_str);

			/* Okay */
			continue;

		/* Hack -- handle find */
		case '/':
		case KTRL('s'):
			{
				int z;

				/* Prompt */
#ifdef JP
				prt("検索: ", hgt - 1, 0);
#else
				prt("Find: ", hgt - 1, 0);
#endif

				/* Get a "finder" string, or continue */
				strcpy(back_str, finder_str);
				if (!askfor(finder_str, 80))
				{
					strcpy(finder_str, back_str);
					continue;
				}
				else if (!finder_str[0])
				{
					shower = NULL; /* Stop showing */
					continue;
				}

				/* Show it */
				shower = finder_str;

				/* Scan messages */
				for (z = i + 1; z < n; z++)
				{
					cptr msg = message_str(z);

					/* Search for it */
					if (my_strstr(msg, finder_str))
					{
						/* New location */
						i = z;

						/* Done */
						break;
					}
				}
			}
			break;

		/* Recall 1 older message */
		case SKEY_TOP:
			/* Go to the oldest line */
			i = n - num_lines;
			break;

		/* Recall 1 newer message */
		case SKEY_BOTTOM:
			/* Go to the newest line */
			i = 0;
			break;

		/* Recall 1 older message */
		case '8':
		case SKEY_UP:
		case '\n':
		case '\r':
			/* Go older if legal */
			i = MIN(i + 1, n - num_lines);
			break;

		/* Recall 10 older messages */
		case '+':
			/* Go older if legal */
			i = MIN(i + 10, n - num_lines);
			break;

		/* Recall 20 older messages */
		case 'p':
		case KTRL('P'):
		case ' ':
		case SKEY_PGUP:
			/* Go older if legal */
			i = MIN(i + num_lines, n - num_lines);
			break;

		/* Recall 20 newer messages */
		case 'n':
		case KTRL('N'):
		case SKEY_PGDOWN:
			/* Go newer (if able) */
			i = MAX(0, i - num_lines);
			break;

		/* Recall 10 newer messages */
		case '-':
			/* Go newer (if able) */
			i = MAX(0, i - 10);
			break;

		/* Recall 1 newer messages */
		case '2':
		case SKEY_DOWN:
			/* Go newer (if able) */
			i = MAX(0, i - 1);
			break;
		}

		/* Hack -- Error of some kind */
		if (i == j) bell();
	}

	/* Restore the screen */
	screen_load();
}


/*
 * Number of cheating options
 */
#define CHEAT_MAX 6

/*
 * Cheating options
 */
static option_type cheat_info[CHEAT_MAX] =
{
	{ &cheat_peek,		FALSE,	255,	0x01, 0x00,
#ifdef JP
	"cheat_peek",		"アイテムの生成をのぞき見る" },
#else
	"cheat_peek",		"Peek into object creation" },
#endif

	{ &cheat_hear,		FALSE,	255,	0x02, 0x00,
#ifdef JP
	"cheat_hear",		"モンスターの生成をのぞき見る" },
#else
	"cheat_hear",		"Peek into monster creation" },
#endif

	{ &cheat_room,		FALSE,	255,	0x04, 0x00,
#ifdef JP
	"cheat_room",		"ダンジョンの生成をのぞき見る" },
#else
	"cheat_room",		"Peek into dungeon creation" },
#endif

	{ &cheat_xtra,		FALSE,	255,	0x08, 0x00,
#ifdef JP
	"cheat_xtra",		"その他の事をのぞき見る" },
#else
	"cheat_xtra",		"Peek into something else" },
#endif

	{ &cheat_know,		FALSE,	255,	0x10, 0x00,
#ifdef JP
	"cheat_know",		"完全なモンスターの思い出を知る" },
#else
	"cheat_know",		"Know complete monster info" },
#endif

	{ &cheat_live,		FALSE,	255,	0x20, 0x00,
#ifdef JP
	"cheat_live",		"死を回避することを可能にする" }
#else
	"cheat_live",		"Allow player to avoid death" }
#endif
};


/*
 * Interact with some options for cheating
 */
static void do_cmd_options_cheat(cptr info)
{
	char	ch;
	int		i, k = 0, n = CHEAT_MAX;
	char	buf[80];

	/* Clear screen */
	Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
#ifdef JP
		sprintf(buf, "%s ( リターンで次へ, y/n でセット, ESC で決定 )", info);
#else
		sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept) ", info);
#endif
		prt(buf, 0, 0);

#ifdef JP
		/* 詐欺オプションをうっかりいじってしまう人がいるようなので注意 */
		prt("                                 <<  注意  >>", 11, 0);
		prt("      詐欺オプションを一度でも設定すると、スコア記録が残らなくなります！", 12, 0);
		prt("      後に解除してもダメですので、勝利者を目指す方はここのオプションはい", 13, 0);
		prt("      じらないようにして下さい。", 14, 0);
#endif
		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)",
			    cheat_info[i].o_desc,
#ifdef JP
			    (*cheat_info[i].o_var ? "はい  " : "いいえ"),
#else
			    (*cheat_info[i].o_var ? "yes" : "no "),
#endif
			    cheat_info[i].o_text);
			c_prt(a, buf, i + 2, 0);
		}

		/* Hilite current option */
		move_cursor(k + 2, 50);

		/* Get a key */
		ch = inkey();

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				return;
			}

			case '-':
			case '8':
			{
				k = (n + k - 1) % n;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			{
				k = (k + 1) % n;
				break;
			}

			case 'y':
			case 'Y':
			case '6':
			{
				if (!noscore && take_notes)
				{
#ifdef JP
					add_note("詐欺モード", 'W');
#else
					add_note("Cheat mode", 'W');
#endif
				}
				noscore |= (cheat_info[k].o_set * 256 + cheat_info[k].o_bit);
				(*cheat_info[k].o_var) = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				(*cheat_info[k].o_var) = FALSE;
				k = (k + 1) % n;
				break;
			}

			default:
			{
				bell();
				break;
			}
		}
	}
}


static option_type autosave_info[2] =
{
	{ (bool *)(&autosave_l), FALSE, 255, 0x01, 0x00,
#ifdef JP
	"autosave_l", "新しい階に入る度に自動セーブする" },
#else
	"autosave_l", "Autosave when entering new levels" },
#endif

	{ (bool *)(&autosave_t), FALSE, 255, 0x02, 0x00,
#ifdef JP
	"autosave_t", "一定ターン毎に自動セーブする" },
#else
	"autosave_t", "Timed autosave" },
#endif
};


static s16b toggle_frequency(s16b current)
{
	if (current == 0) return 50;
	if (current == 50) return 100;
	if (current == 100) return 250;
	if (current == 250) return 500;
	if (current == 500) return 1000;
	if (current == 1000) return 2500;
	if (current == 2500) return 5000;
	if (current == 5000) return 10000;
	if (current == 10000) return 25000;

	return 0;
}


/*
 * Interact with some options for cheating
 */
static void do_cmd_options_autosave(cptr info)
{
	char	ch;

	int     i, k = 0, n = 2;

	char	buf[80];


	/* Clear screen */
	Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
#ifdef JP
		sprintf(buf, "%s ( リターンで次へ, y/n でセット, F で頻度を入力, ESC で決定 ) ", info);
#else
		sprintf(buf, "%s (RET to advance, y/n to set, 'F' for frequency, ESC to accept) ", info);
#endif
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)",
			    autosave_info[i].o_desc,
#ifdef JP
			    (*autosave_info[i].o_var ? "はい  " : "いいえ"),
#else
			    (*autosave_info[i].o_var ? "yes" : "no "),
#endif
			    autosave_info[i].o_text);
			c_prt(a, buf, i + 2, 0);
		}

#ifdef JP
		prt(format("自動セーブの頻度： %d ターン毎",  autosave_freq), 5, 0);
#else
		prt(format("Timed autosave frequency: every %d turns",  autosave_freq), 5, 0);
#endif

		/* Hilite current option */
		move_cursor(k + 2, 50);

		/* Get a key */
		ch = inkey();

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				return;
			}

			case '-':
			case '8':
			{
				k = (n + k - 1) % n;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			{
				k = (k + 1) % n;
				break;
			}

			case 'y':
			case 'Y':
			case '6':
			{

				(*autosave_info[k].o_var) = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				(*autosave_info[k].o_var) = FALSE;
				k = (k + 1) % n;
				break;
			}

			case 'f':
			case 'F':
			{
				autosave_freq = toggle_frequency(autosave_freq);
#ifdef JP
				prt(format("自動セーブの頻度： %d ターン毎", 
#else
				prt(format("Timed autosave frequency: every %d turns",
#endif
				    autosave_freq), 5, 0);
				break;
			}

			default:
			{
				bell();
				break;
			}
		}
	}
}


void do_cmd_options_hitpoint_warning(cptr info)
{
	char ch;
	int i, k = 0, n = 0;
	int opt[24];
	char buf[80];

	/* clear screen */
	Term_clear();

	/* Lookup the options */
	for (i = 0; i < 24; i++) opt[i] = 0;

	/* Scan the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		/* Notice options on this "page" */
		if (option_info[i].o_page == 10) opt[n++] = i;
	}

	while(TRUE)
	{
		/* Prompt XXX XXX XXX */
#ifdef JP
		sprintf(buf, "%s ( リターンで次へ, y/n でセット, ESC で決定 ) ", info);
#else
		sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept) ", info);
#endif
		prt(buf, 0, 0);

#ifdef JP
		prt("警告を行うヒットポイント  ： 10%  20%  30%  40%  50%  60%  70%  80%  90%", 3, 0);
		prt("警告を行うマジックポイント： 10%  20%  30%  40%  50%  60%  70%  80%  90%", 4, 0);
#else
		prt("       Hitpoint Warning   :  10%  20%  30%  40%  50%  60%  70%  80%  90%", 3, 0);
		prt("       Spellpoint Warning :  10%  20%  30%  40%  50%  60%  70%  80%  90%", 4, 0);
#endif

		sprintf(buf, "%d%%", hitpoint_warn * 10);
		Term_putstr(5 * hitpoint_warn + 24, 3, -1,
			((k == 0) ? TERM_L_BLUE : TERM_YELLOW), buf);

		sprintf(buf, "%d%%", spellpoint_warn * 10);
		Term_putstr(5 * spellpoint_warn + 24, 4, -1,
			((k == 1) ? TERM_L_BLUE : TERM_YELLOW), buf);

#ifdef JP
		prt("《以下のオプションを使用すると、警告がより強力になります。》", 6, 0);
#else
		prt("<<Following options make hitpoint warning stronger.>>", 6, 0);
#endif

		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i + 2 == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%.19s)",
				option_info[opt[i]].o_desc,
#ifdef JP
				(*option_info[opt[i]].o_var ? "はい  " : "いいえ"),
#else
				(*option_info[opt[i]].o_var ? "yes" : "no "),
#endif
				option_info[opt[i]].o_text);

			c_prt(a, buf, i + 8, 0);
		}

		ch = inkey();

		switch(ch)
		{
			case ESCAPE:
			{
				return;
			}

			case '-':
			case '8':
			case 'k':
			{
				k = (k + n + 1) % (n + 2);
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			case 'j':
			{
				k = (k + 1) % (n + 2);
				break;
			}

			case 'y':
			case 'Y':
			{
				if (k > 1)
				{
					(*option_info[opt[k-2]].o_var) = TRUE;
				}
				k = (k + 1) % (n + 2);
				break;
			}

			case 'n':
			case 'N':
			{
				if (k > 1)
				{
					(*option_info[opt[k-2]].o_var) = FALSE;
				}
				k = (k + 1) % (n + 2);
				break;
			}

			case '6':
			case 'l':
			{
				if (k < 2)
				{
					if (k == 0 && hitpoint_warn < 9) hitpoint_warn++;
					if (k == 1 && spellpoint_warn < 9) spellpoint_warn++;
				}
				else
				{
					(*option_info[opt[k-2]].o_var) = TRUE;
					k = (k + 1) % (n + 2);
				}
				break;
			}

			case '4':
			case 'h':
			{
				if (k < 2)
				{
					if (k == 0 && hitpoint_warn > 1) hitpoint_warn--;
					if (k == 1 && spellpoint_warn > 1) spellpoint_warn--;
				}
				else
				{
					(*option_info[opt[k-2]].o_var) = FALSE;
					k = (k + 1) % (n + 2);
				}
				break;
			}

			case 't':
			case 'T':
			{
				if (k > 1) (*option_info[opt[k-2]].o_var) = !(*option_info[opt[k-2]].o_var);
				k = (k + 1) % (n + 2);
				break;
			}

			default:
			{
				bell();
				break;
			}
		}
	}
}


#define OPT_BIRTH_PAGE       6
#define OPT_AUTODESTROY_PAGE 7
#define OPT_PLAYRECORD_PAGE  9

/*
 * Interact with some options
 */
void do_cmd_options_aux(int page, cptr info)
{
	char    ch;
	int     i, k = 0, n = 0, l;
	int     opt[24];
	char    buf[80];
	bool    browse_only = (page == OPT_BIRTH_PAGE) && character_generated &&
	                      (!wizard || !allow_debug_opts);


	/* Lookup the options */
	for (i = 0; i < 24; i++) opt[i] = 0;

	/* Scan the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		/* Notice options on this "page" */
		if (option_info[i].o_page == page) opt[n++] = i;
	}


	/* Clear screen */
	Term_clear();

#ifdef JP
		if (page == OPT_AUTODESTROY_PAGE) c_prt(TERM_YELLOW, "以下のオプションは、簡易自動破壊を使用するときのみ有効", 6, 6);
		if (page == OPT_PLAYRECORD_PAGE) c_prt(TERM_YELLOW, "以下のオプションは、プレイ記録するときのみ有効", 6, 6);
#else
		if (page == OPT_AUTODESTROY_PAGE) c_prt(TERM_YELLOW, "Following options will protect items from easy auto-destroyer.", 6, 3);
		if (page == OPT_PLAYRECORD_PAGE) c_prt(TERM_YELLOW, "Following options won't work if take_note option is off.", 6, 3);
#endif

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
#ifdef JP
		sprintf(buf, "%s ( リターンで次へ, %sESC で決定 ) ", info, browse_only ? "" : "y/n でセット, ");
#else
		sprintf(buf, "%s (RET to advance, %s) ", info, browse_only ? "ESC to exit" : "y/n to set, ESC to accept");
#endif
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%.19s)",
				option_info[opt[i]].o_desc,
#ifdef JP
				(*option_info[opt[i]].o_var ? "はい  " : "いいえ"),
#else
				(*option_info[opt[i]].o_var ? "yes" : "no "),
#endif
				option_info[opt[i]].o_text);
			if (((page == OPT_AUTODESTROY_PAGE) && i > 2) || 
				((page == OPT_PLAYRECORD_PAGE) && i > 2))
				c_prt(a, buf, i + 5, 0);
			else
				c_prt(a, buf, i + 2, 0);
		}

		if (((page == OPT_AUTODESTROY_PAGE) && (k > 2)) ||
			((page == OPT_PLAYRECORD_PAGE) && (k > 2)))
			l = 3;
		else
			l = 0;

		/* Hilite current option */
		move_cursor(k + 2 + l, 50);

		/* Get a key */
		ch = inkey();

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				return;
			}

			case '-':
			case '8':
			{
				k = (n + k - 1) % n;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			{
				k = (k + 1) % n;
				break;
			}

			case 'y':
			case 'Y':
			case '6':
			{
				if (browse_only) break;
				(*option_info[opt[k]].o_var) = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				if (browse_only) break;
				(*option_info[opt[k]].o_var) = FALSE;
				k = (k + 1) % n;
				break;
			}

			case 't':
			case 'T':
			{
				if (!browse_only) (*option_info[opt[k]].o_var) = !(*option_info[opt[k]].o_var);
				break;
			}

			default:
			{
				bell();
				break;
			}
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

	bool go = TRUE;

	u32b old_flag[8];


	/* Memorize old flags */
	for (j = 0; j < 8; j++)
	{
		/* Acquire current flags */
		old_flag[j] = window_flag[j];
	}


	/* Clear screen */
	Term_clear();

	/* Interact */
	while (go)
	{
		/* Prompt XXX XXX XXX */
#ifdef JP
		prt("ウィンドウ・フラグ (<方向>で移動, tでチェンジ, y/n でセット, ESC)", 0, 0);
#else
		prt("Window Flags (<dir>, t, y, n, ESC) ", 0, 0);
#endif

		/* Display the windows */
		for (j = 0; j < 8; j++)
		{
			byte a = TERM_WHITE;

			cptr s = angband_term_name[j];

			/* Use color */
			if (use_color && (j == x)) a = TERM_L_BLUE;

			/* Window name, staggered, centered */
			Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
		}

		/* Display the options */
		for (i = 0; i < 16; i++)
		{
			byte a = TERM_WHITE;

			cptr str = window_flag_desc[i];

			/* Use color */
			if (use_color && (i == y)) a = TERM_L_BLUE;

			/* Unused option */
#ifdef JP
			if (!str) str = "(未使用)";
#else
			if (!str) str = "(Unused option)";
#endif

			/* Flag name */
			Term_putstr(0, i + 5, -1, a, str);

			/* Display the windows */
			for (j = 0; j < 8; j++)
			{
				byte a = TERM_WHITE;

				char c = '.';

				/* Use color */
				if (use_color && (i == y) && (j == x)) a = TERM_L_BLUE;

				/* Active flag */
				if (window_flag[j] & (1L << i)) c = 'X';

				/* Flag value */
				Term_putch(35 + j * 5, i + 5, a, c);
			}
		}

		/* Place Cursor */
		Term_gotoxy(35 + x * 5, y + 5);

		/* Get key */
		ch = inkey();

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				go = FALSE;
				break;
			}

			case 'T':
			case 't':
			{
				/* Clear windows */
				for (j = 0; j < 8; j++)
				{
					window_flag[j] &= ~(1L << y);
				}

				/* Clear flags */
				for (i = 0; i < 16; i++)
				{
					window_flag[x] &= ~(1L << i);
				}

				/* Fall through */
			}

			case 'y':
			case 'Y':
			{
				/* Ignore screen */
				if (x == 0) break;

				/* Set flag */
				window_flag[x] |= (1L << y);
				break;
			}

			case 'n':
			case 'N':
			{
				/* Clear flag */
				window_flag[x] &= ~(1L << y);
				break;
			}

			default:
			{
				d = get_keymap_dir(ch);

				x = (x + ddx[d] + 8) % 8;
				y = (y + ddy[d] + 16) % 16;

				if (!d) bell();
			}
		}
	}

	/* Notice changes */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* Dead window */
		if (!angband_term[j]) continue;

		/* Ignore non-changes */
		if (window_flag[j] == old_flag[j]) continue;

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


#define OPT_NUM 15

static struct opts
{
	char key;
	cptr name;
	int row;
}
option_fields[OPT_NUM] =
{
#ifdef JP
	{ '1', "    キー入力     オプション", 3 },
	{ '2', "    画面出力     オプション", 4 },
	{ '3', "  ゲームプレイ   オプション", 5 },
	{ '4', "  行動中止関係   オプション", 6 },
	{ '5', "     効率化      オプション", 7 },
	{ '6', "  簡易自動破壊   オプション", 8 },
	{ '7', "      警告       オプション", 9 },
	{ 'r', "   プレイ記録    オプション", 10 },
	{ 'p', "自動拾いエディタ", 12 },
	{ 'd', " 基本ウェイト量", 13 },
	{ 'a', "   自動セーブ    オプション", 14 },
	{ 'w', "ウインドウフラグ", 15 },
	{ 's', "   設定の保存   ", 16 },

	{ 'b', "      初期       オプション", 18 },
	{ 'c', "      詐欺       オプション", 19 },
#else
	{ '1', "Input Options", 3 },
	{ '2', "Output Options", 4 },
	{ '3', "Game-Play Options", 5 },
	{ '4', "Disturbance Options", 6 },
	{ '5', "Efficiency Options", 7 },
	{ '6', "Object auto-destruction Options", 8 },
	{ '7', "Warning Options", 9 },
	{ 'r', "Play record Options", 10 },
	{ 'p', "Auto-picker/destroyer editor", 12 },
	{ 'd', "Base Delay Factor", 13 },
	{ 'a', "Autosave Options", 14 },
	{ 'w', "Window Flags", 15 },
	{ 's', "Save to pref file", 15 },

	{ 'b', "Birth Options (Browse Only)", 18 },
	{ 'c', "Cheat Options", 19 },
#endif
};

/*
 * Set or unset various options.
 *
 * The user must use the "Ctrl-R" command to "adapt" to changes
 * in any options which control "visual" aspects of the game.
 */
void do_cmd_options(void)
{
	char k;
	int i, d, skey;
	int y = 0;

	/* Save the screen */
	screen_save();

	/* Interact */
	while (1)
	{
		int n = OPT_NUM;

		/* Does not list cheat option when cheat option is off */
		if (!noscore && !allow_debug_opts) n--;

		/* Clear screen */
		Term_clear();

		/* Why are we here */
#ifdef JP
		prt("[ オプションの設定 ]", 1, 0);
#else
		prt("TinyAngband options", 1, 0);
#endif

		while(1)
		{
			/* Give some choices */
			for (i = 0; i < n; i++)
			{
				byte a = TERM_WHITE;
				if (i == y) a = TERM_L_BLUE;
				Term_putstr(5, option_fields[i].row, -1, a, 
					format("(%c) %s", toupper(option_fields[i].key), option_fields[i].name));
			}

#ifdef JP
			prt("<方向>で移動, Enterで決定, ESCでキャンセル: ", 21, 0);
#else
			prt("Move to <dir>, Select to Enter, Cancel to ESC: ", 0, 0);
#endif

			/* Get command */
			skey = inkey_special(TRUE);
			if (!(skey & SKEY_MASK)) k = (char)skey;
			else k = 0;

			/* Exit */
			if (k == ESCAPE) break;

			if (my_strchr("\n\r ", k))
			{
				k = option_fields[y].key;
				break;
			}

			for (i = 0; i < n; i++)
			{
				if (tolower(k) == option_fields[i].key) break;
			}

			/* Command is found */
			if (i < n) break;

			/* Hack -- difficulty option */
			if (k == 'F') break;

			/* Move cursor */
			d = 0;
			if (skey == SKEY_UP) d = 8;
			if (skey == SKEY_DOWN) d = 2;
			y = (y + ddy[d] + n) % n;
			if (!d) bell();
		}

		/* Exit */
		if (k == ESCAPE) break;

		/* Analyze */
		switch (k)
		{
			/* Input Options */
			case '1':
			{
				/* Process the general options */
#ifdef JP
				do_cmd_options_aux(1, "キー入力オプション");
#else
				do_cmd_options_aux(1, "Input Options");
#endif
				break;
			}

			/*  Output Options */
			case '2':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(2, "画面出力オプション");
#else
				do_cmd_options_aux(2, "Output Options");
#endif
				break;
			}

			/* Game-Play Options */
			case '3':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(3, "ゲームプレイ・オプション");
#else
				do_cmd_options_aux(3, "Game-Play Options");
#endif
				break;
			}

			/* Disturbance Options */
			case '4':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(4, "行動中止関係のオプション");
#else
				do_cmd_options_aux(4, "Disturbance Options");
#endif
				break;
			}

			/* Efficiency Options */
			case '5':
			{
#ifdef JP
				do_cmd_options_aux(5, "効率化オプション");
#else
				do_cmd_options_aux(5, "Efficiency Options");
#endif
				break;
			}

			/* Object auto-destruction Options */
			case '6':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(7, "簡易自動破壊オプション");
#else
				do_cmd_options_aux(7, "Object auto-destruction Options");
#endif
				break;
			}

			case '7':
			{
#ifdef JP
				do_cmd_options_hitpoint_warning("警告オプション");
#else
				do_cmd_options_hitpoint_warning("Warning Option");
#endif
				break;
			}

			/* Object auto-destruction Options */
			case 'F':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(8, "ゲーム難易度オプション");
#else
				do_cmd_options_aux(8, "Difficulty Options");
#endif
				break;
			}

			/* Play record Options */
			case 'r':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(9, "プレイ記録オプション");
#else
				do_cmd_options_aux(9, "Play record Options");
#endif
				break;
			}

			/* Birth Options */
			case 'b':
			{
				/* Spawn */
#ifdef JP
				do_cmd_options_aux(6, (!wizard || !allow_debug_opts) ? "初期オプション(参照のみ)" : "初期オプション");
#else
				do_cmd_options_aux(6, (!wizard || !allow_debug_opts) ? "Birth Options(browse only)" : "Birth Options");
#endif
				break;
			}

			/* Cheating Options */
			case 'c':
			{
				if (!noscore && !allow_debug_opts)
				{
					/* Cheat options are not permitted */
					bell();
					break;
				}

				/* Spawn */
#ifdef JP
				do_cmd_options_cheat("詐欺師は決して勝利できない！");
#else
				do_cmd_options_cheat("Cheaters never win");
#endif
				break;
			}

			case 'a':
			{
#ifdef JP
				do_cmd_options_autosave("自動セーブ");
#else
				do_cmd_options_autosave("Autosave");
#endif
				break;
			}

			/* Window flags */
			case 'w':
			{
				/* Spawn */
				do_cmd_options_win();
				p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL |
						  PW_PLAYER | PW_STATS | PW_MESSAGE | PW_OVERHEAD |
						  PW_MONSTER | PW_OBJECT | PW_SNAPSHOT |
						  PW_FLOOR | PW_BORG_1 | PW_BORG_2 | PW_DUNGEON);
				break;
			}

			/* Auto-picker/destroyer editor */
			case 'p':
			{
				do_cmd_edit_autopick();
				break;
			}

			/* Hack -- Delay Speed */
			case 'd':
			{
				/* Prompt */
				clear_from(17);
#ifdef JP
				prt("コマンド: 基本ウェイト量", 18, 0);
#else
				prt("Command: Base Delay Factor", 18, 0);
#endif

				/* Get a new value */
				while (1)
				{
					int msec = delay_factor * delay_factor * delay_factor;
#ifdef JP
					prt(format("現在のウェイト: %d (%dミリ秒)",
#else
					prt(format("Current base delay factor: %d (%d msec)",
#endif

						   delay_factor, msec), 22, 0);
#ifdef JP
					prt("ウェイト (0-9) ESCで決定: ", 20, 0);
#else
					prt("Delay Factor (0-9 or ESC to accept): ", 20, 0);
#endif

					k = inkey();
					if (k == ESCAPE) break;
					if (isdigit(k)) delay_factor = D2I(k);
					else bell();
				}

				break;
			}

			/* Dump the current options to file */
			case 's':
			{
				int i;
				FILE    *fff;
				char    buf[1024];

				/* Build the filename */
				path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "pref-opt.prf");

				/* Drop priv's */
				safe_setuid_drop();

				/* Open the file */
				fff = my_fopen(buf, "w");

				/* Grab priv's */
				safe_setuid_grab();

				/* Failed */
				if (!fff) break;

				/* Header */
				fprintf(fff, "# File: pref-opt.prf\n\n");
				fprintf(fff, "# Allow user specification of various options\n\n");

				/* Scan the options */
				for (i = 0; option_info[i].o_desc; i++)
				{
					/* Dump the option */
					fprintf(fff, "%c:%s\n",
						(*option_info[i].o_var ? 'Y' : 'X'),
						option_info[i].o_text);
				}

				/* Close the file */
				my_fclose(fff);

				/* Success message */
#ifdef JP
				msg_print("デフォルトオプションを保存しました。");
#else
				msg_print("Saved default options.");
#endif
				break;
			}

			/* Unknown option */
			default:
			{
				/* Oops */
				bell();
				break;
			}
		}

		/* Flush messages */
		msg_print(NULL);
	}

	/* Restore the screen */
	screen_load();

	/* Hack - Redraw equippy chars */
	p_ptr->redraw |= (PR_EQUIPPY);
}



/*
 * Ask for a "user pref line" and process it
 *
 * XXX XXX XXX Allow absolute file names?
 */
void do_cmd_pref(void)
{
	char buf[80];

	/* Default */
	strcpy(buf, "");

	/* Ask for a "user pref command" */
#ifdef JP
	if (!get_string("設定変更コマンド: ", buf, 80)) return;
#else
	if (!get_string("Pref: ", buf, 80)) return;
#endif

	/* Process that pref command */
	(void)process_pref_file_command(buf);
}

/*XTRA HACK AUTOPICK*/
void do_cmd_reload_autopick(void)
{
#ifdef JP
	if (!get_check("自動拾い設定ファイルをロードしますか? ")) return;
#else
	if (!get_check("Reload auto-pick preference file? ")) return;
#endif

	/* Load the file with messages */
	autopick_load_pref(TRUE);
}

#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
	static cptr mark = "Macro Dump";

	int i;

	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	if (!open_auto_dump(buf, mark)) return (-1);

	/* Start dumping */
#ifdef JP
	auto_dump_printf("\n# 自動マクロセーブ\n\n");
#else
	auto_dump_printf("\n# Automatic macro dump\n\n");
#endif

	/* Dump them */
	for (i = 0; i < macro__num; i++)
	{
		/* Extract the action */
		ascii_to_text(buf, macro__act[i]);

		/* Dump the macro */
		auto_dump_printf("A:%s\n", buf);

		/* Extract the action */
		ascii_to_text(buf, macro__pat[i]);

		/* Dump normal macros */
		auto_dump_printf("P:%s\n", buf);

		/* End the macro */
		auto_dump_printf("\n");
	}

	/* Close */
	close_auto_dump();

	/* Success */
	return (0);
}


/*
 * Hack -- ask for a "trigger" (see below)
 *
 * Note the complex use of the "inkey()" function from "util.c".
 *
 * Note that both "flush()" calls are extremely important.
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
	ascii_to_text(tmp, buf);

	/* Hack -- display the trigger */
	Term_addstr(-1, TERM_WHITE, tmp);
}

#endif


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
	ascii_to_text(tmp, buf);

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
	static cptr mark = "Keymap Dump";
	int i;

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
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	if (!open_auto_dump(buf, mark)) return (-1);

	/* Start dumping */
#ifdef JP
	auto_dump_printf("\n# 自動キー配置セーブ\n\n");
#else
	auto_dump_printf("\n# Automatic keymap dump\n\n");
#endif

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
		ascii_to_text(key, buf);

		/* Encode the action */
		ascii_to_text(buf, act);

		/* Dump the macro */
		auto_dump_printf("A:%s\n", buf);
		auto_dump_printf("C:%d:%s\n", mode, key);
	}

	/* Close */
	close_auto_dump();

	/* Success */
	return (0);
}



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

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save screen */
	screen_save();


	/* Process requests until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Describe */
#ifdef JP
		prt("[ マクロの設定 ]", 2, 0);
#else
		prt("Interact with Macros", 2, 0);
#endif

		/* Describe that action */
#ifdef JP
		prt("マクロ行動が(もしあれば)下に表示されます:", 20, 0);
#else
		prt("Current action (if any) shown below:", 20, 0);
#endif

		/* Analyze the current action */
		ascii_to_text(buf, macro__buf);

		/* Display the current action */
		prt(buf, 22, 0);


		/* Selections */
#ifdef JP
		prt("(1) ユーザー設定ファイルのロード", 4, 5);
#else
		prt("(1) Load a user pref file", 4, 5);
#endif

#ifdef ALLOW_MACROS
#ifdef JP
		prt("(2) ファイルにマクロを追加", 5, 5);
		prt("(3) マクロの確認", 6, 5);
		prt("(4) マクロの作成", 7, 5);
		prt("(5) マクロの削除", 8, 5);
		prt("(6) ファイルにキー配置を追加", 9, 5);
		prt("(7) キー配置の確認", 10, 5);
		prt("(8) キー配置の作成", 11, 5);
		prt("(9) キー配置の削除", 12, 5);
		prt("(0) マクロ行動の入力", 13, 5);
#else
		prt("(2) Append macros to a file", 5, 5);
		prt("(3) Query a macro", 6, 5);
		prt("(4) Create a macro", 7, 5);
		prt("(5) Remove a macro", 8, 5);
		prt("(6) Append keymaps to a file", 9, 5);
		prt("(7) Query a keymap", 10, 5);
		prt("(8) Create a keymap", 11, 5);
		prt("(9) Remove a keymap", 12, 5);
		prt("(0) Enter a new action", 13, 5);
#endif

#endif /* ALLOW_MACROS */

		/* Prompt */
#ifdef JP
		prt("コマンド: ", 16, 0);
#else
		prt("Command: ", 16, 0);
#endif

		/* Get a command */
		i = inkey();

		/* Leave */
		if (i == ESCAPE) break;

		/* Load a 'macro' file */
		else if (i == '1')
		{
			errr err;

			/* Prompt */
#ifdef JP
			prt("コマンド: ユーザー設定ファイルのロード", 16, 0);
#else
			prt("Command: Load a user pref file", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 18, 0);
#else
			prt("File: ", 18, 0);
#endif

			/* Default filename */
			sprintf(tmp, "%s.prf", player_base);

			/* Ask for a file */
			if (!askfor(tmp, 80)) continue;

			/* Process the given filename */
			err = process_pref_file(tmp);
			if (-2 == err)
			{
#ifdef JP
				msg_format("標準の設定ファイル'%s'を読み込みました。", tmp);
#else
				msg_format("Loaded default '%s'.", tmp);
#endif
			}
			else if (err)
			{
				/* Prompt */
#ifdef JP
				msg_format("'%s'の読み込みに失敗しました！", tmp);
#else
				msg_format("Failed to load '%s'!");
#endif
			}
			else
			{
#ifdef JP
				msg_format("'%s'を読み込みました。", tmp);
#else
				msg_format("Loaded '%s'.", tmp);
#endif
			}
		}

#ifdef ALLOW_MACROS

		/* Save macros */
		else if (i == '2')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: マクロをファイルに追加する", 16, 0);
#else
			prt("Command: Append macros to a file", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 18, 0);
#else
			prt("File: ", 18, 0);
#endif

			/* Default filename */
			sprintf(tmp, "%s.prf", player_base);

			/* Ask for a file */
			if (!askfor(tmp, 80)) continue;

			/* Dump the macros */
			(void)macro_dump(tmp);

			/* Prompt */
#ifdef JP
			msg_print("マクロを追加しました。");
#else
			msg_print("Appended macros.");
#endif
		}

		/* Query a macro */
		else if (i == '3')
		{
			int k;

			/* Prompt */
#ifdef JP
			prt("コマンド: マクロの確認", 16, 0);
#else
			prt("Command: Query a macro", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("トリガーキー: ", 18, 0);
#else
			prt("Trigger: ", 18, 0);
#endif

			/* Get a macro trigger */
			do_cmd_macro_aux(buf);

			/* Acquire action */
			k = macro_find_exact(buf);

			/* Nothing found */
			if (k < 0)
			{
				/* Prompt */
#ifdef JP
				msg_print("そのキーにはマクロは定義されていません。");
#else
				msg_print("Found no macro.");
#endif
			}

			/* Found one */
			else
			{
				/* Obtain the action */
				strcpy(macro__buf, macro__act[k]);

				/* Analyze the current action */
				ascii_to_text(buf, macro__buf);

				/* Display the current action */
				prt(buf, 22, 0);

				/* Prompt */
#ifdef JP
				msg_print("マクロを確認しました。");
#else
				msg_print("Found a macro.");
#endif
			}
		}

		/* Create a macro */
		else if (i == '4')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: マクロの作成", 16, 0);
#else
			prt("Command: Create a macro", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("トリガーキー: ", 18, 0);
#else
			prt("Trigger: ", 18, 0);
#endif

			/* Get a macro trigger */
			do_cmd_macro_aux(buf);

			/* Clear */
			clear_from(20);

			/* Help message */
#ifdef JP
			c_prt(TERM_L_RED, "カーソルキーの左右でカーソル位置を移動。BackspaceかDeleteで一文字削除。", 22, 0);
#else
			c_prt(TERM_L_RED, "Press Left/Right arrow keys to move cursor. Backspace/Delete to delete a char.", 22, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("マクロ行動: ", 20, 0);
#else
			prt("Action: ", 20, 0);
#endif

			/* Convert to text */
			ascii_to_text(tmp, macro__buf);

			/* Get an encoded action */
			if (askfor(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro__buf, tmp);

				/* Link the macro */
				macro_add(buf, macro__buf);

				/* Prompt */
#ifdef JP
				msg_print("マクロを追加しました。");
#else
				msg_print("Added a macro.");
#endif
			}
		}

		/* Remove a macro */
		else if (i == '5')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: マクロの削除", 16, 0);
#else
			prt("Command: Remove a macro", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("トリガーキー: ", 18, 0);
#else
			prt("Trigger: ", 18, 0);
#endif

			/* Get a macro trigger */
			do_cmd_macro_aux(buf);

			/* Link the macro */
			macro_add(buf, buf);

			/* Prompt */
#ifdef JP
			msg_print("マクロを削除しました。");
#else
			msg_print("Removed a macro.");
#endif
		}

		/* Save keymaps */
		else if (i == '6')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: キー配置をファイルに追加する", 16, 0);
#else
			prt("Command: Append keymaps to a file", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 18, 0);
#else
			prt("File: ", 18, 0);
#endif

			/* Default filename */
			sprintf(tmp, "%s.prf", player_base);

			/* Ask for a file */
			if (!askfor(tmp, 80)) continue;

			/* Dump the macros */
			(void)keymap_dump(tmp);

			/* Prompt */
#ifdef JP
			msg_print("キー配置を追加しました。");
#else
			msg_print("Appended keymaps.");
#endif
		}

		/* Query a keymap */
		else if (i == '7')
		{
			cptr act;

			/* Prompt */
#ifdef JP
			prt("コマンド: キー配置の確認", 16, 0);
#else
			prt("Command: Query a keymap", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("押すキー: ", 18, 0);
#else
			prt("Keypress: ", 18, 0);
#endif

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(buf);

			/* Look up the keymap */
			act = keymap_act[mode][(byte)(buf[0])];

			/* Nothing found */
			if (!act)
			{
				/* Prompt */
#ifdef JP
				msg_print("キー配置は定義されていません。");
#else
				msg_print("Found no keymap.");
#endif
			}

			/* Found one */
			else
			{
				/* Obtain the action */
				strcpy(macro__buf, act);

				/* Analyze the current action */
				ascii_to_text(buf, macro__buf);

				/* Display the current action */
				prt(buf, 22, 0);

				/* Prompt */
#ifdef JP
				msg_print("キー配置を確認しました。");
#else
				msg_print("Found a keymap.");
#endif
			}
		}

		/* Create a keymap */
		else if (i == '8')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: キー配置の作成", 16, 0);
#else
			prt("Command: Create a keymap", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("押すキー: ", 18, 0);
#else
			prt("Keypress: ", 18, 0);
#endif

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(buf);

			/* Clear */
			clear_from(20);

			/* Help message */
#ifdef JP
			c_prt(TERM_L_RED, "カーソルキーの左右でカーソル位置を移動。BackspaceかDeleteで一文字削除。", 22, 0);
#else
			c_prt(TERM_L_RED, "Press Left/Right arrow keys to move cursor. Backspace/Delete to delete a char.", 22, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("行動: ", 20, 0);
#else
			prt("Action: ", 20, 0);
#endif

			/* Convert to text */
			ascii_to_text(tmp, macro__buf);

			/* Get an encoded action */
			if (askfor(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro__buf, tmp);

				/* Free old keymap */
				string_free(keymap_act[mode][(byte)(buf[0])]);

				/* Make new keymap */
				keymap_act[mode][(byte)(buf[0])] = string_make(macro__buf);

				/* Prompt */
#ifdef JP
				msg_print("キー配置を追加しました。");
#else
				msg_print("Added a keymap.");
#endif
			}
		}

		/* Remove a keymap */
		else if (i == '9')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: キー配置の削除", 16, 0);
#else
			prt("Command: Remove a keymap", 16, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("押すキー: ", 18, 0);
#else
			prt("Keypress: ", 18, 0);
#endif

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(buf);

			/* Free old keymap */
			string_free(keymap_act[mode][(byte)(buf[0])]);

			/* Make new keymap */
			keymap_act[mode][(byte)(buf[0])] = NULL;

			/* Prompt */
#ifdef JP
			msg_print("キー配置を削除しました。");
#else
			msg_print("Removed a keymap.");
#endif
		}

		/* Enter a new action */
		else if (i == '0')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: マクロ行動の入力", 16, 0);
#else
			prt("Command: Enter a new action", 16, 0);
#endif

			/* Clear */
			clear_from(20);

			/* Help message */
#ifdef JP
			c_prt(TERM_L_RED, "カーソルキーの左右でカーソル位置を移動。BackspaceかDeleteで一文字削除。", 22, 0);
#else
			c_prt(TERM_L_RED, "Press Left/Right arrow keys to move cursor. Backspace/Delete to delete a char.", 22, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("マクロ行動: ", 20, 0);
#else
			prt("Action: ", 20, 0);
#endif

			/* Hack -- limit the value */
			tmp[80] = '\0';

			/* Get an encoded action */
			if (!askfor(buf, 80)) continue;

			/* Extract an action */
			text_to_ascii(macro__buf, buf);
		}

#endif /* ALLOW_MACROS */

		/* Oops */
		else
		{
			/* Oops */
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}

	/* Load screen */
	screen_load();
}


static cptr lighting_level_str[F_LIT_MAX] =
{
#ifdef JP
	"標準色",
	"明色",
	"暗色",
#else
	"standard",
	"brightly lit",
	"darkened",
#endif
};


static bool cmd_visuals_aux(int i, int *num, int max)
{
	if (iscntrl(i))
	{
		char str[10] = "";
		int tmp;

		sprintf(str, "%d", *num);

		if (!get_string(format("Input new number(0-%d): ", max-1), str, 4))
			return FALSE;

		tmp = strtol(str, NULL, 0);
		if (tmp >= 0 && tmp < max)
			*num = tmp;
	}
	else if (isupper(i))
		*num = (*num + max - 1) % max;
	else
		*num = (*num + 1) % max;

	return TRUE;
}

static void print_visuals_menu(cptr choice_msg)
{
#ifdef JP
	prt("[ 画面表示の設定 ]", 1, 0);
#else
	prt("Interact with Visuals", 1, 0);
#endif

	/* Give some choices */
#ifdef JP
	prt("(0) ユーザー設定ファイルのロード", 3, 5);
#else
	prt("(0) Load a user pref file", 3, 5);
#endif

#ifdef ALLOW_VISUALS
#ifdef JP
	prt("(1) モンスターの 色/文字 をファイルに書き出す", 4, 5);
	prt("(2) アイテムの   色/文字 をファイルに書き出す", 5, 5);
	prt("(3) 地形の       色/文字 をファイルに書き出す", 6, 5);
	prt("(4) モンスターの 色/文字 を変更する (数値操作)", 7, 5);
	prt("(5) アイテムの   色/文字 を変更する (数値操作)", 8, 5);
	prt("(6) 地形の       色/文字 を変更する (数値操作)", 9, 5);
	prt("(7) モンスターの 色/文字 を変更する (シンボルエディタ)", 10, 5);
	prt("(8) アイテムの   色/文字 を変更する (シンボルエディタ)", 11, 5);
	prt("(9) 地形の       色/文字 を変更する (シンボルエディタ)", 12, 5);
#else
	prt("(1) Dump monster attr/chars", 4, 5);
	prt("(2) Dump object attr/chars", 5, 5);
	prt("(3) Dump feature attr/chars", 6, 5);
	prt("(4) Change monster attr/chars (numeric operation)", 7, 5);
	prt("(5) Change object attr/chars (numeric operation)", 8, 5);
	prt("(6) Change feature attr/chars (numeric operation)", 9, 5);
	prt("(7) Change monster attr/chars (visual mode)", 10, 5);
	prt("(8) Change object attr/chars (visual mode)", 11, 5);
	prt("(9) Change feature attr/chars (visual mode)", 12, 5);
#endif

#endif /* ALLOW_VISUALS */

#ifdef JP
	prt("(R) 画面表示方法の初期化", 13, 5);
#else
	prt("(R) Reset visuals", 13, 5);
#endif

	/* Prompt */
#ifdef JP
	prt(format("コマンド: %s", choice_msg ? choice_msg : ""), 15, 0);
#else
	prt(format("Command: %s", choice_msg ? choice_msg : ""), 15, 0);
#endif
}

static void do_cmd_knowledge_objects(bool *need_redraw, bool visual_only, int direct_k_idx);
static void do_cmd_knowledge_monsters(bool *need_redraw, bool visual_only, int direct_r_idx);
static void do_cmd_knowledge_features(bool *need_redraw, bool visual_only, int direct_f_idx, int *lighting_level);

/*
 * Interact with "visuals"
 */
void do_cmd_visuals(void)
{
	int i;
	char tmp[160];
	char buf[1024];
	bool need_redraw = FALSE;
	const char *empty_symbol = "<< ? >>";

	if (use_bigtile) empty_symbol = "<< ?? >>";

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save the screen */
	screen_save();

	/* Interact until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		print_visuals_menu(NULL);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		switch (i)
		{
		/* Load a 'pref' file */
		case '0':
			/* Prompt */
#ifdef JP
			prt("コマンド: ユーザー設定ファイルのロード", 15, 0);
#else
			prt("Command: Load a user pref file", 15, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 17, 0);
#else
			prt("File: ", 17, 0);
#endif

			/* Default filename */
			sprintf(tmp, "font-%s.prf", ANGBAND_SYS);

			/* Query */
			if (!askfor(tmp, 70)) continue;

			/* Process the given filename */
			(void)process_pref_file(tmp);

			need_redraw = TRUE;
			break;

#ifdef ALLOW_VISUALS

		/* Dump monster attr/chars */
		case '1':
		{
			static cptr mark = "Monster attr/chars";

			/* Prompt */
#ifdef JP
			prt("コマンド: モンスターの[色/文字]をファイルに書き出します", 15, 0);
#else
			prt("Command: Dump monster attr/chars", 15, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 17, 0);
#else
			prt("File: ", 17, 0);
#endif

			/* Default filename */
			sprintf(tmp, "font-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor(tmp, 70)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

			/* Append to the file */
			if (!open_auto_dump(buf, mark)) continue;

			/* Start dumping */
#ifdef JP
			auto_dump_printf("\n# モンスターの[色/文字]の設定\n\n");
#else
			auto_dump_printf("\n# Monster attr/char definitions\n\n");
#endif

			/* Dump monsters */
			for (i = 0; i < max_r_idx; i++)
			{
				monster_race *r_ptr = &r_info[i];

				/* Skip non-entries */
				if (!r_ptr->name) continue;

				/* Dump a comment */
				auto_dump_printf("# %s\n", (r_name + r_ptr->name));

				/* Dump the monster attr/char info */
				auto_dump_printf("R:%d:0x%02X/0x%02X\n\n", i,
					(byte)(r_ptr->x_attr), (byte)(r_ptr->x_char));
			}

			/* Close */
			close_auto_dump();

			/* Message */
#ifdef JP
			msg_print("モンスターの[色/文字]をファイルに書き出しました。");
#else
			msg_print("Dumped monster attr/chars.");
#endif

			break;
		}

		/* Dump object attr/chars */
		case '2':
		{
			static cptr mark = "Object attr/chars";

			/* Prompt */
#ifdef JP
			prt("コマンド: アイテムの[色/文字]をファイルに書き出します", 15, 0);
#else
			prt("Command: Dump object attr/chars", 15, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 17, 0);
#else
			prt("File: ", 17, 0);
#endif

			/* Default filename */
			sprintf(tmp, "font-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor(tmp, 70)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

			/* Append to the file */
			if (!open_auto_dump(buf, mark)) continue;

			/* Start dumping */
#ifdef JP
			auto_dump_printf("\n# アイテムの[色/文字]の設定\n\n");
#else
			auto_dump_printf("\n# Object attr/char definitions\n\n");
#endif

			/* Dump objects */
			for (i = 0; i < max_k_idx; i++)
			{
				char o_name[80];
				object_kind *k_ptr = &k_info[i];

				/* Skip non-entries */
				if (!k_ptr->name) continue;

				if (!k_ptr->flavor)
				{
					/* Tidy name */
					strip_name(o_name, i);
				}
				else
				{
					object_type forge;

					/* Prepare dummy object */
					object_prep(&forge, i);

					/* Get un-shuffled flavor name */
					object_desc(o_name, &forge, OD_FORCE_FLAVOR);
				}

				/* Dump a comment */
				auto_dump_printf("# %s\n", o_name);

				/* Dump the object attr/char info */
				auto_dump_printf("K:%d:0x%02X/0x%02X\n\n", i,
					(byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));
			}

			/* Close */
			close_auto_dump();

			/* Message */
#ifdef JP
			msg_print("アイテムの[色/文字]をファイルに書き出しました。");
#else
			msg_print("Dumped object attr/chars.");
#endif

			break;
		}

		/* Dump feature attr/chars */
		case '3':
		{
			static cptr mark = "Feature attr/chars";

			/* Prompt */
#ifdef JP
			prt("コマンド: 地形の[色/文字]をファイルに書き出します", 15, 0);
#else
			prt("Command: Dump feature attr/chars", 15, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 17, 0);
#else
			prt("File: ", 17, 0);
#endif

			/* Default filename */
			sprintf(tmp, "font-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor(tmp, 70)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

			/* Append to the file */
			if (!open_auto_dump(buf, mark)) continue;

			/* Start dumping */
#ifdef JP
			auto_dump_printf("\n# 地形の[色/文字]の設定\n\n");
#else
			auto_dump_printf("\n# Feature attr/char definitions\n\n");
#endif

			/* Dump features */
			for (i = 0; i < max_f_idx; i++)
			{
				feature_type *f_ptr = &f_info[i];

				/* Skip non-entries */
				if (!f_ptr->name) continue;

				/* Skip mimiccing features */
				if (f_ptr->mimic != i) continue;

				/* Dump a comment */
				auto_dump_printf("# %s\n", (f_name + f_ptr->name));

				/* Dump the feature attr/char info */
				auto_dump_printf("F:%d:0x%02X/0x%02X:0x%02X/0x%02X:0x%02X/0x%02X\n\n", i,
					(byte)(f_ptr->x_attr[F_LIT_STANDARD]), (byte)(f_ptr->x_char[F_LIT_STANDARD]),
					(byte)(f_ptr->x_attr[F_LIT_LITE]), (byte)(f_ptr->x_char[F_LIT_LITE]),
					(byte)(f_ptr->x_attr[F_LIT_DARK]), (byte)(f_ptr->x_char[F_LIT_DARK]));
			}

			/* Close */
			close_auto_dump();

			/* Message */
#ifdef JP
			msg_print("地形の[色/文字]をファイルに書き出しました。");
#else
			msg_print("Dumped feature attr/chars.");
#endif

			break;
		}

		/* Modify monster attr/chars (numeric operation) */
		case '4':
		{
#ifdef JP
			static cptr choice_msg = "モンスターの[色/文字]を変更します";
#else
			static cptr choice_msg = "Change monster attr/chars";
#endif
			static int r = 0;

#ifdef JP
			prt(format("コマンド: %s", choice_msg), 15, 0);
#else
			prt(format("Command: %s", choice_msg), 15, 0);
#endif

			/* Hack -- query until done */
			while (1)
			{
				monster_race *r_ptr = &r_info[r];
				char c;
				int t;

				byte da = r_ptr->d_attr;
				byte dc = r_ptr->d_char;
				byte ca = r_ptr->x_attr;
				byte cc = r_ptr->x_char;

				/* Label the object */
#ifdef JP
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("モンスター = %d, 名前 = %-40.40s",
						   r, (r_name + r_ptr->name)));
#else
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Monster = %d, Name = %-40.40s",
						   r, (r_name + r_ptr->name)));
#endif

				/* Label the Default values */
#ifdef JP
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("初期値  色 / 文字 = %3u / %3u", da, dc));
#else
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3u / %3u", da, dc));
#endif

				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
				Term_queue_bigchar(43, 19, da, dc, 0, 0);

				/* Label the Current values */
#ifdef JP
				Term_putstr(10, 20, -1, TERM_WHITE,
					    format("現在値  色 / 文字 = %3u / %3u", ca, cc));
#else
				Term_putstr(10, 20, -1, TERM_WHITE,
					    format("Current attr/char = %3u / %3u", ca, cc));
#endif

				Term_putstr(40, 20, -1, TERM_WHITE, empty_symbol);
				Term_queue_bigchar(43, 20, ca, cc, 0, 0);

				/* Prompt */
#ifdef JP
				Term_putstr(0, 22, -1, TERM_WHITE,
					    "コマンド (n/N/^N/a/A/^A/c/C/^C/v/V/^V): ");
#else
				Term_putstr(0, 22, -1, TERM_WHITE,
					    "Command (n/N/^N/a/A/^A/c/C/^C/v/V/^V): ");
#endif

				/* Get a command */
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				if (iscntrl(i)) c = 'a' + i - KTRL('A');
				else if (isupper(i)) c = 'a' + i - 'A';
				else c = i;

				switch (c)
				{
				case 'n':
					{
						int prev_r = r;
						do
						{
							if (!cmd_visuals_aux(i, &r, max_r_idx))
							{
								r = prev_r;
								break;
							}
						}
						while (!r_info[r].name);
					}
					break;
				case 'a':
					t = (int)r_ptr->x_attr;
					(void)cmd_visuals_aux(i, &t, 256);
					r_ptr->x_attr = (byte)t;
					need_redraw = TRUE;
					break;
				case 'c':
					t = (int)r_ptr->x_char;
					(void)cmd_visuals_aux(i, &t, 256);
					r_ptr->x_char = (byte)t;
					need_redraw = TRUE;
					break;
				case 'v':
					do_cmd_knowledge_monsters(&need_redraw, TRUE, r);

					/* Clear screen */
					Term_clear();
					print_visuals_menu(choice_msg);
					break;
				}
			}

			break;
		}

		/* Modify object attr/chars (numeric operation) */
		case '5':
		{
#ifdef JP
			static cptr choice_msg = "アイテムの[色/文字]を変更します";
#else
			static cptr choice_msg = "Change object attr/chars";
#endif
			static int k = 0;

#ifdef JP
			prt(format("コマンド: %s", choice_msg), 15, 0);
#else
			prt(format("Command: %s", choice_msg), 15, 0);
#endif

			/* Hack -- query until done */
			while (1)
			{
				object_kind *k_ptr = &k_info[k];
				char c;
				int t;

				byte da = k_ptr->d_attr;
				byte dc = k_ptr->d_char;
				byte ca = k_ptr->x_attr;
				byte cc = k_ptr->x_char;

				/* Label the object */
#ifdef JP
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("アイテム = %d, 名前 = %-40.40s",
						   k, k_name + (!k_ptr->flavor ? k_ptr->name : k_ptr->flavor_name)));
#else
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Object = %d, Name = %-40.40s",
						   k, k_name + (!k_ptr->flavor ? k_ptr->name : k_ptr->flavor_name)));
#endif

				/* Label the Default values */
#ifdef JP
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("初期値  色 / 文字 = %3d / %3d", da, dc));
#else
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3d / %3d", da, dc));
#endif

				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
				Term_queue_bigchar(43, 19, da, dc, 0, 0);

				/* Label the Current values */
#ifdef JP
				Term_putstr(10, 20, -1, TERM_WHITE,
					    format("現在値  色 / 文字 = %3d / %3d", ca, cc));
#else
				Term_putstr(10, 20, -1, TERM_WHITE,
					    format("Current attr/char = %3d / %3d", ca, cc));
#endif

				Term_putstr(40, 20, -1, TERM_WHITE, empty_symbol);
				Term_queue_bigchar(43, 20, ca, cc, 0, 0);

				/* Prompt */
#ifdef JP
				Term_putstr(0, 22, -1, TERM_WHITE,
					    "コマンド (n/N/^N/a/A/^A/c/C/^C/v/V/^V): ");
#else
				Term_putstr(0, 22, -1, TERM_WHITE,
					    "Command (n/N/^N/a/A/^A/c/C/^C/v/V/^V): ");
#endif

				/* Get a command */
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				if (iscntrl(i)) c = 'a' + i - KTRL('A');
				else if (isupper(i)) c = 'a' + i - 'A';
				else c = i;

				switch (c)
				{
				case 'n':
					{
						int prev_k = k;
						do
						{
							if (!cmd_visuals_aux(i, &k, max_k_idx))
							{
								k = prev_k;
								break;
							}
						}
						while (!k_info[k].name);
					}
					break;
				case 'a':
					t = (int)k_ptr->x_attr;
					(void)cmd_visuals_aux(i, &t, 256);
					k_ptr->x_attr = (byte)t;
					need_redraw = TRUE;
					break;
				case 'c':
					t = (int)k_ptr->x_char;
					(void)cmd_visuals_aux(i, &t, 256);
					k_ptr->x_char = (byte)t;
					need_redraw = TRUE;
					break;
				case 'v':
					do_cmd_knowledge_objects(&need_redraw, TRUE, k);

					/* Clear screen */
					Term_clear();
					print_visuals_menu(choice_msg);
					break;
				}
			}

			break;
		}

		/* Modify feature attr/chars (numeric operation) */
		case '6':
		{
#ifdef JP
			static cptr choice_msg = "地形の[色/文字]を変更します";
#else
			static cptr choice_msg = "Change feature attr/chars";
#endif
			static int f = 0;
			static int lighting_level = F_LIT_STANDARD;

#ifdef JP
			prt(format("コマンド: %s", choice_msg), 15, 0);
#else
			prt(format("Command: %s", choice_msg), 15, 0);
#endif

			/* Hack -- query until done */
			while (1)
			{
				feature_type *f_ptr = &f_info[f];
				char c;
				int t;

				byte da = f_ptr->d_attr[lighting_level];
				byte dc = f_ptr->d_char[lighting_level];
				byte ca = f_ptr->x_attr[lighting_level];
				byte cc = f_ptr->x_char[lighting_level];

				/* Label the object */
				prt("", 17, 5);
#ifdef JP
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("地形 = %d, 名前 = %s, 明度 = %s",
						   f, (f_name + f_ptr->name), lighting_level_str[lighting_level]));
#else
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Terrain = %d, Name = %s, Lighting = %s",
						   f, (f_name + f_ptr->name), lighting_level_str[lighting_level]));
#endif

				/* Label the Default values */
#ifdef JP
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("初期値  色 / 文字 = %3d / %3d", da, dc));
#else
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3d / %3d", da, dc));
#endif

				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);

				Term_queue_bigchar(43, 19, da, dc, 0, 0);

				/* Label the Current values */
#ifdef JP
				Term_putstr(10, 20, -1, TERM_WHITE,
					    format("現在値  色 / 文字 = %3d / %3d", ca, cc));
#else
				Term_putstr(10, 20, -1, TERM_WHITE,
					    format("Current attr/char = %3d / %3d", ca, cc));
#endif

				Term_putstr(40, 20, -1, TERM_WHITE, empty_symbol);
				Term_queue_bigchar(43, 20, ca, cc, 0, 0);

				/* Prompt */
#ifdef JP
				Term_putstr(0, 22, -1, TERM_WHITE,
					    "コマンド (n/N/^N/a/A/^A/c/C/^C/l/L/^L/d/D/^D/v/V/^V): ");
#else
				Term_putstr(0, 22, -1, TERM_WHITE,
					    "Command (n/N/^N/a/A/^A/c/C/^C/l/L/^L/d/D/^D/v/V/^V): ");
#endif

				/* Get a command */
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				if (iscntrl(i)) c = 'a' + i - KTRL('A');
				else if (isupper(i)) c = 'a' + i - 'A';
				else c = i;

				switch (c)
				{
				case 'n':
					{
						int prev_f = f;
						do
						{
							if (!cmd_visuals_aux(i, &f, max_f_idx))
							{
								f = prev_f;
								break;
							}
						}
						while (!f_info[f].name || (f_info[f].mimic != f));
					}
					break;
				case 'a':
					t = (int)f_ptr->x_attr[lighting_level];
					(void)cmd_visuals_aux(i, &t, 256);
					f_ptr->x_attr[lighting_level] = (byte)t;
					need_redraw = TRUE;
					break;
				case 'c':
					t = (int)f_ptr->x_char[lighting_level];
					(void)cmd_visuals_aux(i, &t, 256);
					f_ptr->x_char[lighting_level] = (byte)t;
					need_redraw = TRUE;
					break;
				case 'l':
					(void)cmd_visuals_aux(i, &lighting_level, F_LIT_MAX);
					break;
				case 'd':
					apply_default_feat_lighting(f_ptr->x_attr, f_ptr->x_char);
					need_redraw = TRUE;
					break;
				case 'v':
					do_cmd_knowledge_features(&need_redraw, TRUE, f, &lighting_level);

					/* Clear screen */
					Term_clear();
					print_visuals_menu(choice_msg);
					break;
				}
			}

			break;
		}

		/* Modify monster attr/chars (visual mode) */
		case '7':
			do_cmd_knowledge_monsters(&need_redraw, TRUE, -1);
			break;

		/* Modify object attr/chars (visual mode) */
		case '8':
			do_cmd_knowledge_objects(&need_redraw, TRUE, -1);
			break;

		/* Modify feature attr/chars (visual mode) */
		case '9':
		{
			int lighting_level = F_LIT_STANDARD;
			do_cmd_knowledge_features(&need_redraw, TRUE, -1, &lighting_level);
			break;
		}

#endif /* ALLOW_VISUALS */

		/* Reset visuals */
		case 'R':
		case 'r':
			/* Reset */
			reset_visuals();

			/* Message */
#ifdef JP
			msg_print("画面上の[色/文字]を初期値にリセットしました。");
#else
			msg_print("Visual attr/char tables reset.");
#endif

			need_redraw = TRUE;
			break;

		/* Unknown option */
		default:
			bell();
			break;
		}

		/* Flush messages */
		msg_print(NULL);
	}

	/* Restore the screen */
	screen_load();

	if (need_redraw) do_cmd_redraw();
}


/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{
	int i;

	char tmp[160];

	char buf[1024];


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save the screen */
	screen_save();


	/* Interact until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
#ifdef JP
		prt("[ カラーの設定 ]", 2, 0);
#else
		prt("Interact with Colors", 2, 0);
#endif

		/* Give some choices */
#ifdef JP
		prt("(1) ユーザー設定ファイルのロード", 4, 5);
#else
		prt("(1) Load a user pref file", 4, 5);
#endif

#ifdef ALLOW_COLORS
#ifdef JP
		prt("(2) カラーの設定をファイルに書き出す", 5, 5);
		prt("(3) カラーの設定を変更する", 6, 5);
#else
		prt("(2) Dump colors", 5, 5);
		prt("(3) Modify colors", 6, 5);
#endif
#endif

		/* Prompt */
#ifdef JP
		prt("コマンド: ", 8, 0);
#else
		prt("Command: ", 8, 0);
#endif

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		/* Load a 'pref' file */
		if (i == '1')
		{
			/* Prompt */
#ifdef JP
			prt("コマンド: ユーザー設定ファイルをロードします", 8, 0);
#else
			prt("Command: Load a user pref file", 8, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 10, 0);
#else
			prt("File: ", 10, 0);
#endif

			/* Default file */
			sprintf(tmp, "font-%s.prf", ANGBAND_SYS);

			/* Query */
			if (!askfor(tmp, 70)) continue;

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
			static cptr mark = "Colors";

			/* Prompt */
#ifdef JP
			prt("コマンド: カラーの設定をファイルに書き出します", 8, 0);
#else
			prt("Command: Dump colors", 8, 0);
#endif

			/* Prompt */
#ifdef JP
			prt("ファイル: ", 10, 0);
#else
			prt("File: ", 10, 0);
#endif

			/* Default filename */
			sprintf(tmp, "font-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor(tmp, 70)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

			/* Append to the file */
			if (!open_auto_dump(buf, mark)) continue;

			/* Start dumping */
#ifdef JP
			auto_dump_printf("\n# カラーの設定\n\n");
#else
			auto_dump_printf("\n# Color redefinitions\n\n");
#endif

			/* Dump colors */
			for (i = 0; i < 256; i++)
			{
				int kv = angband_color_table[i][0];
				int rv = angband_color_table[i][1];
				int gv = angband_color_table[i][2];
				int bv = angband_color_table[i][3];

#ifdef JP
				cptr name = "未知";
#else
				cptr name = "unknown";
#endif

				/* Skip non-entries */
				if (!kv && !rv && !gv && !bv) continue;

				/* Extract the color name */
				if (i < 16) name = color_names[i];

				/* Dump a comment */
#ifdef JP
				auto_dump_printf("# カラー '%s'\n", name);
#else
				auto_dump_printf("# Color '%s'\n", name);
#endif

				/* Dump the monster attr/char info */
				auto_dump_printf("V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n",
					i, kv, rv, gv, bv);
			}

			/* Close */
			close_auto_dump();

			/* Message */
#ifdef JP
			msg_print("カラーの設定をファイルに書き出しました。");
#else
			msg_print("Dumped color redefinitions.");
#endif
		}

		/* Edit colors */
		else if (i == '3')
		{
			static byte a = 0;

			/* Prompt */
#ifdef JP
			prt("コマンド: カラーの設定を変更します", 8, 0);
#else
			prt("Command: Modify colors", 8, 0);
#endif

			/* Hack -- query until done */
			while (1)
			{
				cptr name;
				byte j;

				/* Clear */
				clear_from(10);

				/* Exhibit the normal colors */
				for (j = 0; j < 16; j++)
				{
					/* Exhibit this color */
					Term_putstr(j * 4, 20, -1, a, "###");

					/* Exhibit all colors */
					Term_putstr(j*4, 22, -1, j, format("%3d", j));
				}

				/* Describe the color */
#ifdef JP
				name = ((a < 16) ? color_names[a] : "未定義");
#else
				name = ((a < 16) ? color_names[a] : "undefined");
#endif

				/* Describe the color */
#ifdef JP
				Term_putstr(5, 10, -1, TERM_WHITE,
					    format("カラー = %d, 名前 = %s", a, name));
#else
				Term_putstr(5, 10, -1, TERM_WHITE,
					    format("Color = %d, Name = %s", a, name));
#endif

				/* Label the Current values */
				Term_putstr(5, 12, -1, TERM_WHITE,
					    format("K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
						   angband_color_table[a][0],
						   angband_color_table[a][1],
						   angband_color_table[a][2],
						   angband_color_table[a][3]));

				/* Prompt */
#ifdef JP
				Term_putstr(0, 14, -1, TERM_WHITE,
					    "コマンド (n/N/k/K/r/R/g/G/b/B): ");
#else
				Term_putstr(0, 14, -1, TERM_WHITE,
					    "Command (n/N/k/K/r/R/g/G/b/B): ");
#endif

				/* Get a command */
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				/* Analyze */
				if (i == 'n') a = (byte)(a + 1);
				if (i == 'N') a = (byte)(a - 1);
				if (i == 'k') angband_color_table[a][0] = (byte)(angband_color_table[a][0] + 1);
				if (i == 'K') angband_color_table[a][0] = (byte)(angband_color_table[a][0] - 1);
				if (i == 'r') angband_color_table[a][1] = (byte)(angband_color_table[a][1] + 1);
				if (i == 'R') angband_color_table[a][1] = (byte)(angband_color_table[a][1] - 1);
				if (i == 'g') angband_color_table[a][2] = (byte)(angband_color_table[a][2] + 1);
				if (i == 'G') angband_color_table[a][2] = (byte)(angband_color_table[a][2] - 1);
				if (i == 'b') angband_color_table[a][3] = (byte)(angband_color_table[a][3] + 1);
				if (i == 'B') angband_color_table[a][3] = (byte)(angband_color_table[a][3] - 1);

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
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	screen_load();
}


/*
 * Take notes.
 */
void do_cmd_note(void)
{
	char buf[80];

	/* Default */
	strcpy(buf, "");

	/* Input */
#ifdef JP
	if (!get_string("メモ: ", buf, 60)) return;
#else
	if (!get_string("Note: ", buf, 60)) return;
#endif

	/* Ignore empty notes */
	if (!buf[0] || (buf[0] == ' ')) return;

	if (take_notes)
	{
		/* Add note to file */
		add_note(buf, ' ');
	}

	/* Add note to message recall */
#ifdef JP
	msg_format("メモ: %s", buf);
#else
	msg_format("Note: %s", buf);
#endif
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{

	/* Silly message */
#ifndef FAKE_VERSION
	msg_format("You are playing Angband %d.%d.%d.",
		   VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
#else
#ifdef JP
	msg_format("%s %d.%d.%d",
		VERSION_NAME, FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);
#else
	msg_format("You are playing %s %d.%d.%d.",
		VERSION_NAME, FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);
#endif
#endif

}


/*
 * Array of feeling strings
 */
static cptr do_cmd_feeling_text[11] =
{
#ifdef JP
	"この階の雰囲気を感じとれなかった...",
#else
	"Looks like any other level.",
#endif

#ifdef JP
	"この階には何か特別なものがあるような気がする。",
#else
	"You feel there is something special about this level.",
#endif

#ifdef JP
	"恐ろしい死の幻が目に浮かび、気絶しそうになった！",
#else
	"You nearly faint as horrible visions of death fill your mind!",
#endif

#ifdef JP
	"この階はとても危険なようだ。",
#else
	"This level looks very dangerous.",
#endif

#ifdef JP
	"とても悪い予感がする...",
#else
	"You have a very bad feeling...",
#endif

#ifdef JP
	"悪い予感がする...",
#else
	"You have a bad feeling...",
#endif

#ifdef JP
	"何か緊張する。",
#else
	"You feel nervous.",
#endif

#ifdef JP
	"多少は運が向いてきたか...",
#else
	"You feel your luck is turning...",
#endif

#ifdef JP
	"この場所は好きになれない。",
#else
	"You don't like the look of this place.",
#endif

#ifdef JP
	"この階はそれなりに安全なようだ。",
#else
	"This level looks reasonably safe.",
#endif

#ifdef JP
	"なんて退屈なところだ..."
#else
	"What a boring place..."
#endif

};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
	/* No useful feeling in quests */
	if (p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM)
	{
#ifdef JP
		msg_print("典型的なクエストのダンジョンのようだ。");
#else
		msg_print("Looks like a typical quest level.");
#endif
		return;
	}

	/* No useful feeling in town */
	else if (p_ptr->town_num && !dun_level)
	{
#ifdef JP
		msg_print("典型的な町のようだ。");
#else
		msg_print("Looks like a typical town.");
#endif
		return;
	}

	/* No useful feeling in the wilderness */
	else if (!dun_level)
	{
#ifdef JP
		msg_print("典型的な荒野のようだ。");
#else
		msg_print("Looks like a typical wilderness.");
#endif
		return;
	}

	/* Display the feeling */
	msg_print(do_cmd_feeling_text[p_ptr->feeling]);
}


/*
 * Description of each monster group.
 */
static cptr monster_group_text[] = 
{
#ifdef JP
	"ユニーク",	/* "Uniques" */
	"アリ",
	"コウモリ",
	"ムカデ",
	"ドラゴン",
	"目玉",
	"ネコ",
	"ゴーレム",
	"標準人間型生物",
	"ベトベト",
	"ゼリー",
	"コボルド",
	"水棲生物",
	"モルド",
	"ナーガ",
	"オーク",
	"人間",
	"四足獣",
	"ネズミ",
	"スケルトン",
	"デーモン",
	"ボルテックス",
	"イモムシ/大群",
	/* "unused", */
	"イーク",
	"ゾンビ/ミイラ",
	"天使",
	"鳥",
	"犬",
	/* "古代ドラゴン/ワイアーム", */
	"エレメンタル",
	"トンボ",
	"ゴースト",
	"雑種",
	"昆虫",
	"ヘビ",
	"キラー・ビートル",
	"リッチ",
	"多首の爬虫類",
	/* "謎の生物", */
	"オーガ",
	"巨大人間型生物",
	"クイルスルグ",
	"爬虫類/両生類",
	"蜘蛛/サソリ/ダニ",
	"トロル",
	/* "上級デーモン", */
	"バンパイア",
	"ワイト/レイス/等",
	"ゾーン/ザレン/等",
	"イエティ",
	"ハウンド",
	"ミミック",
	"壁/植物/気体",
	"おばけキノコ",
	"球体",
	"プレイヤー",
#else
	"Uniques",
	"Ant",
	"Bat",
	"Centipede",
	"Dragon",
	"Floating Eye",
	"Feline",
	"Golem",
	"Hobbit/Elf/Dwarf",
	"Icky Thing",
	"Jelly",
	"Kobold",
	"Aquatic monster",
	"Mold",
	"Naga",
	"Orc",
	"Person/Human",
	"Quadruped",
	"Rodent",
	"Skeleton",
	"Demon",
	"Vortex",
	"Worm/Worm-Mass",
	/* "unused", */
	"Yeek",
	"Zombie/Mummy",
	"Angel",
	"Bird",
	"Canine",
	/* "Ancient Dragon/Wyrm", */
	"Elemental",
	"Dragon Fly",
	"Ghost",
	"Hybrid",
	"Insect",
	"Snake",
	"Killer Beetle",
	"Lich",
	"Multi-Headed Reptile",
	/* "Mystery Living", */
	"Ogre",
	"Giant Humanoid",
	"Quylthulg",
	"Reptile/Amphibian",
	"Spider/Scorpion/Tick",
	"Troll",
	/* "Major Demon", */
	"Vampire",
	"Wight/Wraith/etc",
	"Xorn/Xaren/etc",
	"Yeti",
	"Zephyr Hound",
	"Mimic",
	"Wall/Plant/Gas",
	"Mushroom patch",
	"Ball",
	"Player",
#endif
	NULL
};


/*
 * Symbols of monsters in each group. Note the "Uniques" group
 * is handled differently.
 */
static cptr monster_group_char[] =
{
	(char *) -1L,
	"a",
	"b",
	"c",
	"dD",
	"e",
	"f",
	"g",
	"h",
	"i",
	"j",
	"k",
	"l",
	"m",
	"n",
	"o",
	"pt",
	"q",
	"r",
	"s",
	"uU",
	"v",
	"w",
	/* "x", */
	"y",
	"z",
	"A",
	"B",
	"C",
	/* "D", */
	"E",
	"F",
	"G",
	"H",
	"I",
	"J",
	"K",
	"L",
	"M",
	/* "N", */
	"O",
	"P",
	"Q",
	"R",
	"S",
	"T",
	/* "U", */
	"V",
	"W",
	"X",
	"Y",
	"Z",
	"!$&()+./=>?[\\]`{|~",
	"#%",
	",",
	"*",
	"@",
	NULL
};


/*
 * hook function to sort monsters by level
 */
static bool ang_sort_comp_monster_level(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	int w1 = who[a];
	int w2 = who[b];

	monster_race *r_ptr1 = &r_info[w1];
	monster_race *r_ptr2 = &r_info[w2];

	/* Unused */
	(void)v;

	if (r_ptr2->level > r_ptr1->level) return TRUE;
	if (r_ptr1->level > r_ptr2->level) return FALSE;

	if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return TRUE;
	if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return FALSE;
	return w1 <= w2;
}

/*
 * Build a list of monster indexes in the given group. Return the number
 * of monsters in the group.
 *
 * mode & 0x01 : check for non-empty group
 * mode & 0x02 : visual operation only
 */
static int collect_monsters(int grp_cur, s16b mon_idx[], byte mode)
{
	int i, mon_cnt = 0;
	int dummy_why;

	/* Get a list of x_char in this group */
	cptr group_char = monster_group_char[grp_cur];

	/* XXX Hack -- Check if this is the "Uniques" group */
	bool grp_unique = (monster_group_char[grp_cur] == (char *) -1L);

	/* Check every race */
	for (i = 0; i < max_r_idx; i++)
	{
		/* Access the race */
		monster_race *r_ptr = &r_info[i];

		/* Skip empty race */
		if (!r_ptr->name) continue ;

		/* Require known monsters */
		if (!(mode & 0x02) && !cheat_know && !r_ptr->r_sights) continue;

		if (grp_unique)
		{
			if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;
		}

		else
		{
			/* Check for race in the group */
			if (!my_strchr(group_char, r_ptr->d_char)) continue;
		}

		/* Add the race */
		mon_idx[mon_cnt++] = i;

		/* XXX Hack -- Just checking for non-empty group */
		if (mode & 0x01) break;
	}

	/* Terminate the list */
	mon_idx[mon_cnt] = -1;

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_monster_level;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort by monster level */
	ang_sort(mon_idx, &dummy_why, mon_cnt);

	/* Return the number of races */
	return mon_cnt;
}


/*
 * Description of each monster group.
 */
static cptr object_group_text[] = 
{
#ifdef JP
	"食料",		/* "Foods" */
	"薬",		/* "Potions" */
	"油つぼ",	/* "Flasks" */
	"巻物",		/* "Scrolls" */
	"指輪",		/* "Rings" */
	"アミュレット",	/* "Amulets" */
	"光源",		/* "Lanterns" */
	"ワンド",	/* "Wands" */
	"スタッフ",	/* "Staffs" */
	"ロッド",	/* "Rods" */
	"くさび",
	"箱",
	"人形",
	"像",
	"ゴミ",
	"空のビン",
	"骨",
	"死体",
	"刀剣類",	/* "Swords" */
	"鈍器",		/* "Blunt Weapons" */
	"長柄武器",	/* "Polearms" */
	"採掘道具",	/* "Diggers" */
	"飛び道具",	/* "Bows" */
	"弾",
	"矢",
	"ボルト",
	"軽装鎧",	/* "Soft Armor" */
	"重装鎧",	/* "Hard Armor" */
	"ドラゴン鎧",	/* "Dragon Armor" */
	"盾",	/* "Shields" */
	"クローク",	/* "Cloaks" */
	"籠手",	/* "Gloves" */
	"ヘルメット",	/* "Helms" */
	"冠",	/* "Crowns" */
	"ブーツ",	/* "Boots" */
	"魔法書",
	"財宝",
	"何か",
#else
	"Foods",
	"Potions",
	"Flasks",
	"Scrolls",
	"Rings",
	"Amulets",
	"Lanterns",
	"Wands",
	"Staves",
	"Rods",
	"Spikes",
	"Boxs",
	"Figurines",
	"Statues",
	"Junks",
	"Bottles",
	"Skeletons",
	"Corpses",
	"Swords",
	"Blunt Weapons",
	"Polearms",
	"Diggers",
	"Bows",
	"Shots",
	"Arrows",
	"Bolts",
	"Soft Armor",
	"Hard Armor",
	"Dragon Armor",
	"Shields",
	"Cloaks",
	"Gloves",
	"Helms",
	"Crowns",
	"Boots",
	"Spellbooks",
	"Treasure",
	"Something",
#endif
	NULL
};


/*
 * TVALs of items in each group
 */
static byte object_group_tval[] = 
{
	TV_FOOD,
	TV_POTION,
	TV_FLASK,
	TV_SCROLL,
	TV_RING,
	TV_AMULET,
	TV_LITE,
	TV_WAND,
	TV_STAFF,
	TV_ROD,
	TV_SPIKE,
	TV_CHEST,
	TV_FIGURINE,
	TV_STATUE,
	TV_JUNK,
	TV_BOTTLE,
	TV_SKELETON,
	TV_CORPSE,
	TV_SWORD,
	TV_HAFTED,
	TV_POLEARM,
	TV_DIGGING,
	TV_BOW,
	TV_SHOT,
	TV_ARROW,
	TV_BOLT,
	TV_SOFT_ARMOR,
	TV_HARD_ARMOR,
	TV_DRAG_ARMOR,
	TV_SHIELD,
	TV_CLOAK,
	TV_GLOVES,
	TV_HELM,
	TV_CROWN,
	TV_BOOTS,
	TV_LIFE_BOOK, /* Hack -- all spellbooks */
	TV_GOLD,
	0,
	0,
};


/*
 * Build a list of object indexes in the given group. Return the number
 * of objects in the group.
 *
 * mode & 0x01 : check for non-empty group
 * mode & 0x02 : visual operation only
 */
static int collect_objects(int grp_cur, int object_idx[], byte mode)
{
	int i, j, k, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	/* Check every object */
	for (i = 0; i < max_k_idx; i++)
	{
		/* Access the object */
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects */
		if (!k_ptr->name) continue;

		if (mode & 0x02)
		{
			/* Any objects will be displayed */
		}
		else
		{
			if (!wizard)
			{
				/* Skip non-flavoured objects */
				if (!k_ptr->flavor) continue;

				/* Require objects ever seen */
				if (!k_ptr->aware) continue;
			}

			/* Skip items with no distribution (special artifacts) */
			for (j = 0, k = 0; j < 4; j++) k += k_ptr->chance[j];
			if (!k) continue;
		}

		/* Check for objects in the group */
		if (TV_LIFE_BOOK == group_tval)
		{
			/* Hack -- All spell books */
			if (TV_LIFE_BOOK <= k_ptr->tval && k_ptr->tval <= TV_SORCERY_BOOK)
			{
				/* Add the object */
				object_idx[object_cnt++] = i;
			}
			else continue;
		}
		else if (k_ptr->tval == group_tval)
		{
			/* Add the object */
			object_idx[object_cnt++] = i;
		}
		else continue;

		/* XXX Hack -- Just checking for non-empty group */
		if (mode & 0x01) break;
	}

	/* Terminate the list */
	object_idx[object_cnt] = -1;

	/* Return the number of objects */
	return object_cnt;
}


/*
 * Description of each feature group.
 */
static cptr feature_group_text[] = 
{
	"terrains",
	NULL
};


/*
 * Build a list of feature indexes in the given group. Return the number
 * of features in the group.
 *
 * mode & 0x01 : check for non-empty group
 */
static int collect_features(int grp_cur, int *feat_idx, byte mode)
{
	int i, feat_cnt = 0;

	/* Unused;  There is a single group. */
	(void)grp_cur;

	/* Check every feature */
	for (i = 0; i < max_f_idx; i++)
	{
		/* Access the index */
		feature_type *f_ptr = &f_info[i];

		/* Skip empty index */
		if (!f_ptr->name) continue;

		/* Skip mimiccing features */
		if (f_ptr->mimic != i) continue;

		/* Add the index */
		feat_idx[feat_cnt++] = i;

		/* XXX Hack -- Just checking for non-empty group */
		if (mode & 0x01) break;
	}

	/* Terminate the list */
	feat_idx[feat_cnt] = -1;

	/* Return the number of races */
	return feat_cnt;
}


#if 0
/*
 * Build a list of monster indexes in the given group. Return the number
 * of monsters in the group.
 */
static int collect_artifacts(int grp_cur, int object_idx[])
{
	int i, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	/* Check every object */
	for (i = 0; i < max_a_idx; i++)
	{
		/* Access the artifact */
		artifact_type *a_ptr = &a_info[i];

		/* Skip empty artifacts */
		if (!a_ptr->name) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->cur_num) continue;

		/* Check for race in the group */
		if (a_ptr->tval == group_tval)
		{
			/* Add the race */
			object_idx[object_cnt++] = i;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of races */
	return object_cnt;
}
#endif


/*
 * Encode the screen colors
 */
static char hack[17] = "dwsorgbuDWvyRGBU";


void do_cmd_save_screen_html_aux(char *filename, int message)
{
	int y, x, i;

	byte a = 0, old_a = 0;
	char c = ' ';

	FILE *fff, *tmpfff;
	char buf[2048];

	int yomikomu;
	cptr tags[4] = {
		"HEADER_START:",
		"HEADER_END:",
		"FOOTER_START:",
		"FOOTER_END:",
	};

	cptr html_head[] = {
		"<html><head>\n",
#ifdef JP
#ifdef SJIS
		"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=Shift_JIS\">\n",
#else
		"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=EUC-JP\">\n",
#endif
#endif
		"</head><body text=\"#ffffff\" bgcolor=\"#000000\">\n",
		"<pre>",
		0,
	};
	cptr html_foot[] = {
		"</pre>\n",
		"</body>\n</html>\n",
		0,
	};

	int wid, hgt;

	Term_get_size(&wid, &hgt);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(filename, "w");

	/* Oops */
	if (!fff) {
		if (message) {
#ifdef JP
		    msg_format("ファイル %s を開けませんでした。", filename);
#else
		    msg_format("Failed to open file %s.", filename);
#endif
		    msg_print(NULL);
		}
		
		return;
	}

	/* Save the screen */
	if (message)
		screen_save();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "htmldump.prf");
	tmpfff = my_fopen(buf, "r");
	if (!tmpfff) {
		for (i = 0; html_head[i]; i++)
			fprintf(fff, html_head[i]);
	}
	else
	{
		yomikomu = 0;
		while (!my_fgets(tmpfff, buf, sizeof(buf)))
		{
			if (!yomikomu)
			{
				if (strncmp(buf, tags[0], strlen(tags[0])) == 0)
					yomikomu = 1;
			}
			else
			{
				if (strncmp(buf, tags[1], strlen(tags[1])) == 0)
					break;
				fprintf(fff, "%s\n", buf);
			}
		}
	}

	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		/* Start the row */
		if (y != 0)
			fprintf(fff, "\n");

		/* Dump each row */
		for (x = 0; x < wid - 1; x++)
		{
			int rv, gv, bv;
			cptr cc = NULL;
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			switch (c)
			{
			case '&': cc = "&amp;"; break;
			case '<': cc = "&lt;"; break;
			case '>': cc = "&gt;"; break;
#ifdef WINDOWS
			case 0x1f: c = '.'; break;
			case 0x7f: c = (a == 0x09) ? '%' : '#'; break;
#endif
			}

			a = a & 0x0F;
			if ((y == 0 && x == 0) || a != old_a) {
				rv = angband_color_table[a][1];
				gv = angband_color_table[a][2];
				bv = angband_color_table[a][3];
				fprintf(fff, "%s<font color=\"#%02x%02x%02x\">", 
					((y == 0 && x == 0) ? "" : "</font>"), rv, gv, bv);
				old_a = a;
			}
			if (cc)
				fprintf(fff, "%s", cc);
			else
				fprintf(fff, "%c", c);
		}
	}
	fprintf(fff, "</font>");

	if (!tmpfff) {
		for (i = 0; html_foot[i]; i++)
			fprintf(fff, html_foot[i]);
	}
	else
	{
		rewind(tmpfff);
		yomikomu = 0;
		while (!my_fgets(tmpfff, buf, sizeof(buf)))
		{
			if (!yomikomu)
			{
				if (strncmp(buf, tags[2], strlen(tags[2])) == 0)
					yomikomu = 1;
			}
			else
			{
				if (strncmp(buf, tags[3], strlen(tags[3])) == 0)
					break;
				fprintf(fff, "%s\n", buf);
			}
		}
		my_fclose(tmpfff);
	}

	/* Skip a line */
	fprintf(fff, "\n");

	/* Close it */
	my_fclose(fff);

	/* Message */
	if (message) {
#ifdef JP
	msg_print("画面(記念撮影)をファイルに書き出しました。");
#else
		msg_print("Screen dump saved.");
#endif
		msg_print(NULL);
	}

	/* Restore the screen */
	if (message)
		screen_load();
}

/*
 * Hack -- load a screen dump from a file
 */
void do_cmd_load_screen(void)
{
	int i, y, x;

	byte a = 0;
	char c = ' ';

	bool okay = TRUE;

	FILE *fff;

	char buf[1024];

	int wid, hgt;

	Term_get_size(&wid, &hgt);

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "dump.txt");

	/* Append to the file */
	fff = my_fopen(buf, "r");

	/* Oops */
	if (!fff) return;


	/* Save the screen */
	screen_save();

	/* Clear the screen */
	Term_clear();


	/* Load the screen */
	for (y = 0; okay && (y < hgt); y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, sizeof(buf))) okay = FALSE;

		/* Show each row */
		for (x = 0; x < wid - 1; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	if (my_fgets(fff, buf, sizeof(buf))) okay = FALSE;


	/* Dump the screen */
	for (y = 0; okay && (y < hgt); y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, sizeof(buf))) okay = FALSE;

		/* Dump each row */
		for (x = 0; x < wid - 1; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Look up the attr */
			for (i = 0; i < 16; i++)
			{
				/* Use attr matches */
				if (hack[i] == buf[x]) a = i;
			}

			/* Hack -- fake monochrome */
			if (!use_color) a = TERM_WHITE;

			/* Put the attr/char */
			Term_draw(x, y, a, c);
		}

	}


	/* Get the blank line */
	(void)my_fgets(fff, buf, sizeof(buf));


	/* Close it */
	my_fclose(fff);


	/* Message */
#ifdef JP
	msg_print("ファイルに書き出された画面(記念撮影)をロードしました。");
#else
	msg_print("Screen dump loaded.");
#endif

	msg_print(NULL);


	/* Restore the screen */
	screen_load();
}

/*
 * Hack -- save a screen dump to a file
 */
static void do_cmd_save_screen_html(void)
{
	char buf[1024], tmp[256] = "screen.html";

#ifdef JP
	if (!get_string("ファイル名: ", tmp, 80))
#else
	if (!get_string("File name: ", tmp, 80))
#endif
		return;

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

	msg_print(NULL);

	/* Hack -- drop permissions */
	safe_setuid_drop();

	do_cmd_save_screen_html_aux(buf, 1);

	/* Hack -- grab permissions */
	safe_setuid_grab();
}


/*
 * Redefinable "save_screen" action
 */
void (*screendump_aux)(void) = NULL;


/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
	bool old_use_graphics = use_graphics;
	bool html_dump = FALSE;

	int wid, hgt;

#ifdef JP
	prt("記念撮影しますか？ [(y)es/(h)tml/(n)o] ", 0, 0);
#else
	prt("Save screen dump? [(y)es/(h)tml/(n)o] ", 0, 0);
#endif
	while(TRUE)
	{
		char c = inkey();
		if (c == 'Y' || c == 'y')
			break;
		else if (c == 'H' || c == 'h')
		{
			html_dump = TRUE;
			break;
		}
		else
		{
			prt("", 0, 0);
			return;
		}
	}

	Term_get_size(&wid, &hgt);

	if (old_use_graphics)
	{
		use_graphics = FALSE;
		reset_visuals();

		/* Redraw everything */
		p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

		/* Hack -- update */
		handle_stuff();
	}

#ifdef JP
	if (html_dump == TRUE || get_check("HTMLで出力しますか？"))
#else
	if (html_dump == TRUE || get_check("Save screen dump as HTML? "))
#endif
	{
		do_cmd_save_screen_html();
		do_cmd_redraw();
	}

	/* Do we use a special screendump function ? */
	else if (screendump_aux)
	{
		/* Dump the screen to a graphics file */
		(*screendump_aux)();
	}
	else /* Dump the screen as text */
	{
		int y, x;

		byte a = 0;
		char c = ' ';

		FILE *fff;

		char buf[1024];


		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "dump.txt");

		/* File type is "TEXT" */
		FILE_TYPE(FILE_TYPE_TEXT);

		/* Hack -- drop permissions */
		safe_setuid_drop();

		/* Append to the file */
		fff = my_fopen(buf, "w");

		/* Hack -- grab permissions */
		safe_setuid_grab();

		/* Oops */
		if (!fff) return;


		/* Save the screen */
		screen_save();


		/* Dump the screen */
		for (y = 0; y < hgt; y++)
		{
			/* Dump each row */
			for (x = 0; x < wid - 1; x++)
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
		for (y = 0; y < hgt; y++)
		{
			/* Dump each row */
			for (x = 0; x < wid; x++)
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
#ifdef JP
		msg_print("画面(記念撮影)をファイルに書き出しました。");
#else
		msg_print("Screen dump saved.");
#endif

		msg_print(NULL);


		/* Restore the screen */
		screen_load();
	}

	if (old_use_graphics)
	{
		use_graphics = TRUE;
		reset_visuals();

		/* Redraw everything */
		p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

		/* Hack -- update */
		handle_stuff();
	}
}


/*
 * Check the status of "artifacts"
 */
static void do_cmd_knowledge_artifacts(void)
{
	int i, k, z, x, y;

	FILE *fff;

	char file_name[1024];

	char base_name[MAX_NLEN];

	bool *okay;

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	if (!fff) {
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
	    msg_format("Failed to create temporary file %s.", file_name);
#endif
	    msg_print(NULL);
	    return;
	}

	/* Allocate the "okay" array */
	C_MAKE(okay, max_a_idx, bool);

	/* Scan the artifacts */
	for (k = 0; k < max_a_idx; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* Default */
		okay[k] = FALSE;

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->cur_num) continue;

		/* Assume okay */
		okay[k] = TRUE;
	}

	/* Check the dungeon */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			cave_type *c_ptr = &cave[y][x];

			s16b this_o_idx, next_o_idx;

			/* Scan all objects in the grid */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Ignore non-artifacts */
				if (!artifact_p(o_ptr)) continue;

				/* Ignore known items */
				if (object_known_p(o_ptr)) continue;

				/* Note the artifact */
				okay[o_ptr->name1] = FALSE;
			}
		}
	}

	/* Check the inventory and equipment */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Ignore non-objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore non-artifacts */
		if (!artifact_p(o_ptr)) continue;

		/* Ignore known items */
		if (object_known_p(o_ptr)) continue;

		/* Note the artifact */
		okay[o_ptr->name1] = FALSE;
	}

	/* Scan the artifacts */
	for (k = 0; k < max_a_idx; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* List "dead" ones */
		if (!okay[k]) continue;

		/* Paranoia */
#ifdef JP
strcpy(base_name, "未知の伝説のアイテム");
#else
		strcpy(base_name, "Unknown Artifact");
#endif


		/* Obtain the base object type */
		z = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Real object */
		if (z)
		{
			object_type forge;
			object_type *q_ptr;

			/* Get local object */
			q_ptr = &forge;

			/* Create fake object */
			object_prep(q_ptr, z);

			/* Make it an artifact */
			q_ptr->name1 = k;

			/* Display as if known */
			q_ptr->ident |= IDENT_STORE;

			/* Describe the artifact */
			object_desc(base_name, q_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);
		}

		/* Hack -- Build the artifact name */
#ifdef JP
		fprintf(fff, "     %s\n", base_name);
#else
		fprintf(fff, "     The %s\n", base_name);
#endif

	}

	/* Free the "okay" array */
	C_KILL(okay, max_a_idx, bool);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
	show_file(file_name, "既知の伝説のアイテム", 0, 0);
#else
	show_file(file_name, "Artifacts Seen", 0, 0);
#endif


	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Display known uniques
 */
static void do_cmd_knowledge_uniques(void)
{
	FILE *fff;

	char file_name[1024];

	int i, n;

	u16b	why = 2;
	u16b	*who;

#ifdef JP
	/* XTRA HACK UNIQHIST */
	int n_alive[10], n_dead[10];
	int max_lev = 0;

	for (i = 0; i < 10; i++) n_alive[i] = n_dead[i] =0;
#endif

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	if (!fff) {
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
	    msg_format("Failed to create temporary file %s.", file_name);
#endif
	    msg_print(NULL);
	    return;
	}

	/* Allocate the "who" array */
	C_MAKE(who, max_r_idx, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];
#ifdef JP
		/* XTRA HACK UNIQHIST */
		int lev = (r_ptr->level - 1) / 10;
#endif

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Require unique monsters if needed */
		if (!(r_ptr->flags1 & (RF1_UNIQUE))) continue;

#ifdef JP
		/* XTRA HACK UNIQHIST */
		if (lev < 10)
		{
		     if (r_ptr->max_num == 0) n_dead[lev]++;
		     else n_alive[lev]++;
		     if (max_lev < lev) max_lev = lev;
		}
#endif

		/* Collect "appropriate" monsters */
		who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* No monsters to recall */
#ifdef JP
msg_print("既知のユニーク・モンスターなし。");
#else
		msg_print("No known uniques.");
#endif

		msg_print(NULL);
		return;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

#ifdef JP
	/* XTRA HACK UNIQHIST */
	for (i = 0; i <= max_lev; i++)
	{
	     fprintf(fff, " %2d-%3d階  倒 %3d  生 %3d\n", 
		     1 + i * 10, 10 + i * 10, n_dead[i], n_alive[i]);
	}
	fprintf(fff, "\n");
#endif

	/* Scan the monster races */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];
		bool dead = (r_ptr->max_num == 0);

		/* Print a message */
#ifdef JP
		fprintf(fff, " [%s] %sは%s。\n",
			(dead ? "済" : "--"),
			(r_name + r_ptr->name),
			(dead ? "既に死んでいる" : "まだ生きている"));
#else
		fprintf(fff, "     %s is %s\n",(r_name + r_ptr->name),
			(dead ? "dead" : "alive"));
#endif

	}

	/* Free the "who" array */
	C_KILL(who, max_r_idx, u16b);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
	show_file(file_name, "既知のユニーク・モンスター", 0, 0);
#else
	show_file(file_name, "Known Uniques", 0, 0);
#endif


	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Pluralize a monster name
 */
void plural_aux(char *Name)
{
	int NameLen = strlen(Name);

	if (my_strstr(Name, "Disembodied hand"))
	{
		strcpy(Name, "Disembodied hands that strangled people");
	}
	else if (my_strstr(Name, "Colour out of space"))
	{
		strcpy(Name, "Colours out of space");
	}
	else if (my_strstr(Name, "stairway to hell"))
	{
		strcpy(Name, "stairways to hell");
	}
	else if (my_strstr(Name, "Dweller on the threshold"))
	{
		strcpy(Name, "Dwellers on the threshold");
	}
	else if (my_strstr(Name, " of "))
	{
		cptr aider = my_strstr(Name, " of ");
		char dummy[80];
		int i = 0;
		cptr ctr = Name;

		while (ctr < aider)
		{
			dummy[i] = *ctr;
			ctr++; i++;
		}

		if (dummy[i-1] == 's')
		{
			strcpy(&(dummy[i]), "es");
			i++;
		}
		else
		{
			strcpy(&(dummy[i]), "s");
		}

		strcpy(&(dummy[i+1]), aider);
		strcpy(Name, dummy);
	}
	else if (my_strstr(Name, "coins"))
	{
		char dummy[80];
		strcpy(dummy, "piles of ");
		strcat(dummy, Name);
		strcpy(Name, dummy);
		return;
	}
	else if (my_strstr(Name, "Manes"))
	{
		return;
	}
	else if (streq(&(Name[NameLen - 2]), "ey"))
	{
		strcpy(&(Name[NameLen - 2]), "eys");
	}
	else if (Name[NameLen - 1] == 'y')
	{
		strcpy(&(Name[NameLen - 1]), "ies");
	}
	else if (streq(&(Name[NameLen - 4]), "ouse"))
	{
		strcpy(&(Name[NameLen - 4]), "ice");
	}
	else if (streq(&(Name[NameLen - 2]), "us"))
	{
		strcpy(&(Name[NameLen - 2]), "i");
	}
	else if (streq(&(Name[NameLen - 6]), "kelman"))
	{
		strcpy(&(Name[NameLen - 6]), "kelmen");
	}
	else if (streq(&(Name[NameLen - 8]), "wordsman"))
	{
		strcpy(&(Name[NameLen - 8]), "wordsmen");
	}
	else if (streq(&(Name[NameLen - 7]), "oodsman"))
	{
		strcpy(&(Name[NameLen - 7]), "oodsmen");
	}
	else if (streq(&(Name[NameLen - 7]), "eastman"))
	{
		strcpy(&(Name[NameLen - 7]), "eastmen");
	}
	else if (streq(&(Name[NameLen - 8]), "izardman"))
	{
		strcpy(&(Name[NameLen - 8]), "izardmen");
	}
	else if (streq(&(Name[NameLen - 5]), "geist"))
	{
		strcpy(&(Name[NameLen - 5]), "geister");
	}
	else if (streq(&(Name[NameLen - 2]), "ex"))
	{
		strcpy(&(Name[NameLen - 2]), "ices");
	}
	else if (streq(&(Name[NameLen - 2]), "lf"))
	{
		strcpy(&(Name[NameLen - 2]), "lves");
	}
	else if (suffix(Name, "ch") ||
		 suffix(Name, "sh") ||
			 suffix(Name, "nx") ||
			 suffix(Name, "s") ||
			 suffix(Name, "o"))
	{
		strcpy(&(Name[NameLen]), "es");
	}
	else
	{
		strcpy(&(Name[NameLen]), "s");
	}
}


/*
 * Display current pets
 */
static void do_cmd_knowledge_pets(void)
{
	int             i;
	FILE            *fff;
	monster_type    *m_ptr;
	int             t_friends = 0;
	int             t_levels = 0;
	int             show_upkeep = 0;
	char            file_name[1024];


	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{
			char pet_name[80];
			t_friends++;
			t_levels += r_info[m_ptr->r_idx].level;
			monster_desc(pet_name, m_ptr, 0x88);
			fprintf(fff, "%s (%s)\n", pet_name, look_mon_desc(i, FALSE));
		}
	}

	if (t_friends > 1 + (p_ptr->lev / (cp_ptr->pet_upkeep_div)))
	{
		show_upkeep = t_levels;

		if (show_upkeep > 95) show_upkeep = 95;
		else if (show_upkeep < 5) show_upkeep = 5;
	}


	fprintf(fff, "----------------------------------------------\n");
#ifdef JP
	fprintf(fff, "    合計: %d 匹のペット\n", t_friends);
	fprintf(fff, " 維持コスト: %d%% MP\n", show_upkeep);
#else
	fprintf(fff, "   Total: %d pet%s.\n",
		t_friends, (t_friends == 1 ? "" : "s"));
	fprintf(fff, "   Upkeep: %d%% mana.\n", show_upkeep);
#endif



	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
show_file(file_name, "現在のペット", 0, 0);
#else
	show_file(file_name, "Current Pets", 0, 0);
#endif


	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Total kill count
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
static void do_cmd_knowledge_kill_count(void)
{
	FILE *fff;

	char file_name[1024];

	s32b Total = 0;

	int i, n;

	u16b	why = 2;
	u16b	*who;

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	if (!fff) {
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
	    msg_format("Failed to create temporary file %s.", file_name);
#endif
	    msg_print(NULL);
	    return;
	}

	/* Allocate the "who" array */
	C_MAKE(who, max_r_idx, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Collect "appropriate" monsters */
		who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* No monsters to recall */
#ifdef JP
msg_print("既知のモンスターなし！");
#else
		msg_print("No known monsters!");
#endif

		msg_print(NULL);
		return;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	{
		/* Monsters slain */
		int kk;

		for (kk = 1; kk < max_r_idx; kk++)
		{
			monster_race *r_ptr = &r_info[kk];

			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				bool dead = (r_ptr->max_num == 0);

				if (dead)
				{
					Total++;
				}
			}
			else
			{
				s16b This = r_ptr->r_pkills;

				if (This > 0)
				{
					Total += This;
				}
			}
		}

		if (Total < 1)
#ifdef JP
			fprintf(fff,"あなたはまだ敵を倒していない。\n\n");
#else
			fprintf(fff,"You have defeated no enemies yet.\n\n");
#endif

		else if (Total == 1)
#ifdef JP
			fprintf(fff,"あなたは一匹の敵を倒している。\n\n");
#else
			fprintf(fff,"You have defeated one enemy.\n\n");
#endif

		else
#ifdef JP
			fprintf(fff,"あなたは %lu 匹の敵を倒している。\n\n", Total);
#else
			fprintf(fff,"You have defeated %lu enemies.\n\n", Total);
#endif

	}

	Total = 0;

	/* Scan the monster races */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			bool dead = (r_ptr->max_num == 0);

			if (dead)
			{
				/* Print a message */
				fprintf(fff, "     %s\n",
				    (r_name + r_ptr->name));
				Total++;
			}
		}
		else
		{
			s16b This = r_ptr->r_pkills;

			if (This > 0)
			{
#ifdef JP
/* p,tは人と数える by ita*/
if (my_strchr("pt",r_ptr->d_char))
fprintf(fff, "     %3d 人の %s\n", This, r_name + r_ptr->name);
else
fprintf(fff, "     %3d 匹の %s\n", This, r_name + r_ptr->name);
#else
				if (This < 2)
				{
					if (my_strstr(r_name + r_ptr->name, "coins"))
					{
						fprintf(fff, "     1 pile of %s\n", (r_name + r_ptr->name));
					}
					else
					{
						fprintf(fff, "     1 %s\n", (r_name + r_ptr->name));
					}
				}
				else
				{
					char ToPlural[80];
					strcpy(ToPlural, (r_name + r_ptr->name));
					plural_aux(ToPlural);
					fprintf(fff, "     %d %s\n", This, ToPlural);
				}
#endif


				Total += This;
			}
		}
	}

	fprintf(fff,"----------------------------------------------\n");
#ifdef JP
	fprintf(fff,"    合計: %lu 匹を倒した。\n", Total);
#else
	fprintf(fff,"   Total: %lu creature%s killed.\n",
		Total, (Total == 1 ? "" : "s"));
#endif


	/* Free the "who" array */
	C_KILL(who, max_r_idx, u16b);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
show_file(file_name, "倒した敵の数", 0, 0);
#else
	show_file(file_name, "Kill Count", 0, 0);
#endif


	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Display the object groups.
 */
static void display_group_list(int col, int row, int wid, int per_page,
	int grp_idx[], cptr group_text[], int grp_cur, int grp_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && (grp_idx[i] >= 0); i++)
	{
		/* Get the group index */
		int grp = grp_idx[grp_top + i];

		/* Choose a color */
		byte attr = (grp_top + i == grp_cur) ? TERM_L_BLUE : TERM_WHITE;

		/* Erase the entire line */
		Term_erase(col, row + i, wid);

		/* Display the group label */
		c_put_str(attr, group_text[grp], row + i, col);
	}
}


/* 
 * Move the cursor in a browser window 
 */
static void browser_cursor(char ch, int *column, int *grp_cur, int grp_cnt, 
						   int *list_cur, int list_cnt)
{
	int d;
	int col = *column;
	int grp = *grp_cur;
	int list = *list_cur;

	/* Extract direction */
	if (ch == ' ')
	{
		/* Hack -- scroll up full screen */
		d = 3;
	}
	else if (ch == '-')
	{
		/* Hack -- scroll down full screen */
		d = 9;
	}
	else
	{
		d = get_keymap_dir(ch);
	}

	if (!d) return;

	/* Diagonals - hack */
	if ((ddx[d] > 0) && ddy[d])
	{
		int browser_rows;
		int wid, hgt;

		/* Get size */
		Term_get_size(&wid, &hgt);

		browser_rows = hgt - 8;

		/* Browse group list */
		if (!col)
		{
			int old_grp = grp;

			/* Move up or down */
			grp += ddy[d] * (browser_rows - 1);

			/* Verify */
			if (grp >= grp_cnt)	grp = grp_cnt - 1;
			if (grp < 0) grp = 0;
			if (grp != old_grp)	list = 0;
		}

		/* Browse sub-list list */
		else
		{
			/* Move up or down */
			list += ddy[d] * browser_rows;

			/* Verify */
			if (list >= list_cnt) list = list_cnt - 1;
			if (list < 0) list = 0;
		}

		(*grp_cur) = grp;
		(*list_cur) = list;

		return;
	}

	if (ddx[d])
	{
		col += ddx[d];
		if (col < 0) col = 0;
		if (col > 1) col = 1;

		(*column) = col;

		return;
	}

	/* Browse group list */
	if (!col)
	{
		int old_grp = grp;

		/* Move up or down */
		grp += ddy[d];

		/* Verify */
		if (grp >= grp_cnt)	grp = grp_cnt - 1;
		if (grp < 0) grp = 0;
		if (grp != old_grp)	list = 0;
	}

	/* Browse sub-list list */
	else
	{
		/* Move up or down */
		list += ddy[d];

		/* Verify */
		if (list >= list_cnt) list = list_cnt - 1;
		if (list < 0) list = 0;
	}

	(*grp_cur) = grp;
	(*list_cur) = list;
}


/*
 * Display visuals.
 */
static void display_visual_list(int col, int row, int height, int width, byte attr_top, byte char_left)
{
	int i, j;

	/* Clear the display lines */
	for (i = 0; i < height; i++)
	{
		Term_erase(col, row + i, width);
	}

	/* Bigtile mode uses double width */
	if (use_bigtile) width /= 2;

	/* Display lines until done */
	for (i = 0; i < height; i++)
	{
		/* Display columns until done */
		for (j = 0; j < width; j++)
		{
			byte a;
			char c;
			int x = col + j;
			int y = row + i;
			int ia, ic;

			/* Bigtile mode uses double width */
			if (use_bigtile) x += j;

			ia = attr_top + i;
			ic = char_left + j;

			/* Ignore illegal characters */
			if (ia > 0x7f || ic > 0xff || ic < ' ' ||
			    (!use_graphics && ic > 0x7f))
				continue;

			a = (byte)ia;
			c = (char)ic;

			/* Force correct code for both ASCII character and tile */
			if (c & 0x80) a |= 0x80;

			/* Display symbol */
			Term_queue_bigchar(x, y, a, c, 0, 0);
		}
	}
}


/*
 * Place the cursor at the collect position for visual mode
 */
static void place_visual_list_cursor(int col, int row, byte a, byte c, byte attr_top, byte char_left)
{
	int i = (a & 0x7f) - attr_top;
	int j = c - char_left;

	int x = col + j;
	int y = row + i;

	/* Bigtile mode uses double width */
	if (use_bigtile) x += j;

	/* Place the cursor */
	Term_gotoxy(x, y);
}


/*
 *  Clipboard variables for copy&paste in visual mode
 */
static byte attr_idx = 0;
static byte char_idx = 0;

/* Hack -- for feature lighting */
static byte attr_idx_feat[F_LIT_MAX];
static byte char_idx_feat[F_LIT_MAX];

/*
 *  Do visual mode command -- Change symbols
 */
static bool visual_mode_command(char ch, bool *visual_list_ptr,
				int height, int width,
				byte *attr_top_ptr, byte *char_left_ptr,
				byte *cur_attr_ptr, char *cur_char_ptr, bool *need_redraw)
{
	static byte attr_old = 0, char_old = 0;

	switch (ch)
	{
	case ESCAPE:
		if (*visual_list_ptr)
		{
			/* Cancel change */
			*cur_attr_ptr = attr_old;
			*cur_char_ptr = char_old;
			*visual_list_ptr = FALSE;

			return TRUE;
		}
		break;

	case '\n':
	case '\r':
		if (*visual_list_ptr)
		{
			/* Accept change */
			*visual_list_ptr = FALSE;
			*need_redraw = TRUE;

			return TRUE;
		}
		break;

	case 'V':
	case 'v':
		if (!*visual_list_ptr)
		{
			*visual_list_ptr = TRUE;

			*attr_top_ptr = MAX(0, (*cur_attr_ptr & 0x7f) - 5);
			*char_left_ptr = MAX(0, *cur_char_ptr - 10);

			attr_old = *cur_attr_ptr;
			char_old = *cur_char_ptr;

			return TRUE;
		}
		break;

	case 'C':
	case 'c':
		{
			int i;

			/* Set the visual */
			attr_idx = *cur_attr_ptr;
			char_idx = *cur_char_ptr;

			/* Hack -- for feature lighting */
			for (i = 0; i < F_LIT_MAX; i++)
			{
				attr_idx_feat[i] = 0;
				char_idx_feat[i] = 0;
			}
		}
		return TRUE;

	case 'P':
	case 'p':
		if (attr_idx || (!(char_idx & 0x80) && char_idx)) /* Allow TERM_DARK text */
		{
			/* Set the char */
			*cur_attr_ptr = attr_idx;
			*attr_top_ptr = MAX(0, (*cur_attr_ptr & 0x7f) - 5);
			if (!*visual_list_ptr) *need_redraw = TRUE;
		}

		if (char_idx)
		{
			/* Set the char */
			*cur_char_ptr = char_idx;
			*char_left_ptr = MAX(0, *cur_char_ptr - 10);
			if (!*visual_list_ptr) *need_redraw = TRUE;
		}

		return TRUE;

	default:
		if (*visual_list_ptr)
		{
			int eff_width;
			int d = get_keymap_dir(ch);
			byte a = (*cur_attr_ptr & 0x7f);
			byte c = *cur_char_ptr;

			if (use_bigtile) eff_width = width / 2;
			else eff_width = width;

			/* Restrict direction */
			if ((a == 0) && (ddy[d] < 0)) d = 0;
			if ((c == 0) && (ddx[d] < 0)) d = 0;
			if ((a == 0x7f) && (ddy[d] > 0)) d = 0;
			if ((c == 0xff) && (ddx[d] > 0)) d = 0;

			a += ddy[d];
			c += ddx[d];

			/* Force correct code for both ASCII character and tile */
			if (c & 0x80) a |= 0x80;

			/* Set the visual */
			*cur_attr_ptr = a;
			*cur_char_ptr = c;


			/* Move the frame */
			if ((ddx[d] < 0) && *char_left_ptr > MAX(0, (int)c - 10)) (*char_left_ptr)--;
			if ((ddx[d] > 0) && *char_left_ptr + eff_width < MIN(0xff, (int)c + 10)) (*char_left_ptr)++;
			if ((ddy[d] < 0) && *attr_top_ptr > MAX(0, (int)(a & 0x7f) - 4)) (*attr_top_ptr)--;
			if ((ddy[d] > 0) && *attr_top_ptr + height < MIN(0x7f, (a & 0x7f) + 4)) (*attr_top_ptr)++;
			return TRUE;
		}
		break;
	}

	/* Visual mode command is not used */
	return FALSE;
}


/*
 * Display the objects in a group.
 */
static void display_object_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top, bool visual_only)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && (object_idx[object_top + i] >= 0); i++)
	{
		char o_name[80];
		byte a, c;
		object_kind *flavor_k_ptr;

		/* Get the object index */
		int k_idx = object_idx[object_top + i];

		/* Access the object */
		object_kind *k_ptr = &k_info[k_idx];

		/* Choose a color */
		byte attr = ((k_ptr->aware || visual_only) ? TERM_WHITE : TERM_SLATE);
		byte cursor = ((k_ptr->aware || visual_only) ? TERM_L_BLUE : TERM_BLUE);


		if (!visual_only && k_ptr->flavor)
		{
			/* Appearance of this object is shuffled */
			flavor_k_ptr = &k_info[k_ptr->flavor];
		}
		else
		{
			/* Appearance of this object is very normal */
			flavor_k_ptr = k_ptr;
		}



		attr = ((i + object_top == object_cur) ? cursor : attr);

		if (!k_ptr->flavor || (!visual_only && k_ptr->aware))
		{
			/* Tidy name */
			strip_name(o_name, k_idx);
		}
		else
		{
			/* Flavor name */
			strcpy(o_name, k_name + flavor_k_ptr->flavor_name);
		}

		/* Display the name */
		c_prt(attr, o_name, row + i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			c_prt(attr, format("%02x/%02x", flavor_k_ptr->x_attr, (unsigned char)flavor_k_ptr->x_char), row + i, (wizard || visual_only) ? 64 : 68);
		}
		if (wizard || visual_only)
		{
			c_prt(attr, format("%d", k_idx), row + i, 70);
		}

		a = flavor_k_ptr->x_attr;
		c = flavor_k_ptr->x_char;

		/* Display symbol */
		Term_queue_bigchar(use_bigtile ? 76 : 77, row + i, a, c, 0, 0);
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}

/*
 * Describe fake object
 */
static void desc_obj_fake(int k_idx)
{
	object_type *o_ptr;
	object_type object_type_body;

	/* Get local object */
	o_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Create the artifact */
	object_prep(o_ptr, k_idx);

	/* It's fully know */
	o_ptr->ident |= IDENT_KNOWN;

	/* Track the object */
	/* object_actual_track(o_ptr); */

	/* Hack - mark as fake */
	/* term_obj_real = FALSE; */

	/* Hack -- Handle stuff */
	handle_stuff();

	if (!identify_fully_aux(o_ptr, FALSE))
#ifdef JP
		msg_print("特に変わったところはないようだ。");
#else
		msg_print("You see nothing special.");
#endif
		msg_print(NULL);
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(bool *need_redraw, bool visual_only, int direct_k_idx)
{
	int i, len, max;
	int grp_cur, grp_top, old_grp_cur;
	int object_old, object_cur, object_top;
	int grp_cnt, grp_idx[100];
	int object_cnt;
	int *object_idx;

	int column = 0;
	bool flag;
	bool redraw;

	bool visual_list = FALSE;
	byte attr_top = 0, char_left = 0;

	int browser_rows;
	int wid, hgt;

	byte mode;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

	/* Allocate the "object_idx" array */
	C_MAKE(object_idx, max_k_idx, int);

	max = 0;
	grp_cnt = 0;

	if (direct_k_idx < 0)
	{
		mode = visual_only ? 0x03 : 0x01;

		/* Check every group */
		for (i = 0; object_group_text[i] != NULL; i++)
		{
			/* Measure the label */
			len = strlen(object_group_text[i]);

			/* Save the maximum length */
			if (len > max) max = len;

			/* See if any monsters are known */
			if (collect_objects(i, object_idx, mode))
			{
				/* Build a list of groups with known monsters */
				grp_idx[grp_cnt++] = i;
			}
		}

		object_old = -1;
		object_cnt = 0;
	}
	else
	{
		object_kind *k_ptr = &k_info[direct_k_idx];
		object_kind *flavor_k_ptr;

		if (!visual_only && k_ptr->flavor)
		{
			/* Appearance of this object is shuffled */
			flavor_k_ptr = &k_info[k_ptr->flavor];
		}
		else
		{
			/* Appearance of this object is very normal */
			flavor_k_ptr = k_ptr;
		}

		object_idx[0] = direct_k_idx;
		object_old = direct_k_idx;
		object_cnt = 1;

		/* Terminate the list */
		object_idx[1] = -1;

		(void)visual_mode_command('v', &visual_list, browser_rows - 1, wid - (max + 3),
			&attr_top, &char_left, &flavor_k_ptr->x_attr, &flavor_k_ptr->x_char, need_redraw);
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	old_grp_cur = -1;
	grp_cur = grp_top = 0;
	object_cur = object_top = 0;

	flag = FALSE;
	redraw = TRUE;

	mode = visual_only ? 0x02 : 0x00;

	while (!flag)
	{
		char ch;
		object_kind *k_ptr, *flavor_k_ptr;

		if (redraw)
		{
			clear_from(0);

#ifdef JP
			prt(format("%s - アイテム", !visual_only ? "知識" : "表示"), 2, 0);
			if (direct_k_idx < 0) prt("グループ", 4, 0);
			prt("名前", 4, max + 3);
			if (wizard || visual_only) prt("Idx", 4, 70);
			prt("文字", 4, 74);
#else
			prt(format("%s - objects", !visual_only ? "Knowledge" : "Visuals"), 2, 0);
			if (direct_k_idx < 0) prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (wizard || visual_only) prt("Idx", 4, 70);
			prt("Sym", 4, 75);
#endif

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			if (direct_k_idx < 0)
			{
				for (i = 0; i < browser_rows; i++)
				{
					Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
				}
			}

			redraw = FALSE;
		}

		if (direct_k_idx < 0)
		{
			/* Scroll group list */
			if (grp_cur < grp_top) grp_top = grp_cur;
			if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

			/* Display a list of object groups */
			display_group_list(0, 6, max, browser_rows, grp_idx, object_group_text, grp_cur, grp_top);

			if (old_grp_cur != grp_cur)
			{
				old_grp_cur = grp_cur;

				/* Get a list of objects in the current group */
				object_cnt = collect_objects(grp_idx[grp_cur], object_idx, mode);
			}

			/* Scroll object list */
			while (object_cur < object_top)
				object_top = MAX(0, object_top - browser_rows/2);
			while (object_cur >= object_top + browser_rows)
				object_top = MIN(object_cnt - browser_rows, object_top + browser_rows/2);
		}

		if (!visual_list)
		{
			/* Display a list of objects in the current group */
			display_object_list(max + 3, 6, browser_rows, object_idx, object_cur, object_top, visual_only);
		}
		else
		{
			object_top = object_cur;

			/* Display a list of objects in the current group */
			display_object_list(max + 3, 6, 1, object_idx, object_cur, object_top, visual_only);

			/* Display visual list below first object */
			display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
		}

		/* Get the current object */
		k_ptr = &k_info[object_idx[object_cur]];

		if (!visual_only && k_ptr->flavor)
		{
			/* Appearance of this object is shuffled */
			flavor_k_ptr = &k_info[k_ptr->flavor];
		}
		else
		{
			/* Appearance of this object is very normal */
			flavor_k_ptr = k_ptr;
		}

		/* Prompt */
#ifdef JP
		prt(format("<方向>%s%s%s, ESC",
			(!visual_list && !visual_only) ? ", 'r'で詳細を見る" : "",
			visual_list ? ", ENTERで決定" : ", 'v'でシンボル変更",
			(attr_idx || char_idx) ? ", 'c', 'p'でペースト" : ", 'c'でコピー"),
			hgt - 1, 0);
#else
		prt(format("<dir>%s%s%s, ESC",
			(!visual_list && !visual_only) ? ", 'r' to recall" : "",
			visual_list ? ", ENTER to accept" : ", 'v' for visuals",
			(attr_idx || char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"),
			hgt - 1, 0);
#endif

		if (!visual_only)
		{
			/* Mega Hack -- track this object */
			if (object_cnt) object_kind_track(object_idx[object_cur]);

			/* The "current" object changed */
			if (object_old != object_idx[object_cur])
			{
				/* Hack -- handle stuff */
				handle_stuff();

				/* Remember the "current" object */
				object_old = object_idx[object_cur];
			}
		}

		if (visual_list)
		{
			place_visual_list_cursor(max + 3, 7, flavor_k_ptr->x_attr, flavor_k_ptr->x_char, attr_top, char_left);
		}
		else if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (object_cur - object_top));
		}

		ch = inkey();

		/* Do visual mode command if needed */
		if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, &flavor_k_ptr->x_attr, &flavor_k_ptr->x_char, need_redraw))
		{
			if (direct_k_idx >= 0)
			{
				switch (ch)
				{
				case '\n':
				case '\r':
				case ESCAPE:
					flag = TRUE;
					break;
				}
			}
			continue;
		}

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				if (!visual_list && !visual_only && (grp_cnt > 0))
				{
					desc_obj_fake(object_idx[object_cur]);
					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur, object_cnt);
				break;
			}
		}
	}

	/* Free the "object_idx" array */
	C_KILL(object_idx, max_k_idx, int);
}


/*
 * Display the monsters in a group.
 */
static void display_monster_list(int col, int row, int per_page, s16b mon_idx[],
	int mon_cur, int mon_top, bool visual_only)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && (mon_idx[mon_top + i] >= 0); i++)
	{
		byte attr;

		/* Get the race index */
		int r_idx = mon_idx[mon_top + i] ;

		/* Access the race */
		monster_race *r_ptr = &r_info[r_idx];

		/* Choose a color */
		attr = ((i + mon_top == mon_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the name */
		c_prt(attr, (r_name + r_ptr->name), row + i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			c_prt(attr, format("%02x/%02x", r_ptr->x_attr, (unsigned char)r_ptr->x_char), row + i, (wizard || visual_only) ? 56 : 61);
		}
		if (wizard || visual_only)
		{
			c_prt(attr, format("%d", r_idx), row + i, 62);
		}

		/* Erase chars before overwritten by the race letter */
		Term_erase(69, row + i, 255);

		/* Display symbol */
		Term_queue_bigchar(use_bigtile ? 69 : 70, row + i, r_ptr->x_attr, r_ptr->x_char, 0, 0);

		if (!visual_only)
		{
			/* Display kills */
			if (!(r_ptr->flags1 & RF1_UNIQUE)) put_str(format("%5d", r_ptr->r_pkills), row + i, 73);
#ifdef JP
			else c_put_str((r_ptr->max_num == 0 ? TERM_L_DARK : TERM_WHITE), (r_ptr->max_num == 0 ? "死亡" : "生存"), row + i, 74);
#else
			else c_put_str((r_ptr->max_num == 0 ? TERM_L_DARK : TERM_WHITE), (r_ptr->max_num == 0 ? " dead" : "alive"), row + i, 73);
#endif
		}
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}


/*
 * Display known monsters.
 */
static void do_cmd_knowledge_monsters(bool *need_redraw, bool visual_only, int direct_r_idx)
{
	int i, len, max;
	int grp_cur, grp_top, old_grp_cur;
	int mon_cur, mon_top;
	int grp_cnt, grp_idx[100];
	int mon_cnt;
	s16b *mon_idx;

	int column = 0;
	bool flag;
	bool redraw;

	bool visual_list = FALSE;
	byte attr_top = 0, char_left = 0;

	int browser_rows;
	int wid, hgt;

	byte mode;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

	/* Allocate the "mon_idx" array */
	C_MAKE(mon_idx, max_r_idx, s16b);

	max = 0;
	grp_cnt = 0;

	if (direct_r_idx < 0)
	{
		mode = visual_only ? 0x03 : 0x01;

		/* Check every group */
		for (i = 0; monster_group_text[i] != NULL; i++)
		{
			/* Measure the label */
			len = strlen(monster_group_text[i]);

			/* Save the maximum length */
			if (len > max) max = len;

			/* See if any monsters are known */
			if ((monster_group_char[i] == ((char *) -1L)) || collect_monsters(i, mon_idx, mode))
			{
				/* Build a list of groups with known monsters */
				grp_idx[grp_cnt++] = i;
			}
		}

		mon_cnt = 0;
	}
	else
	{
		mon_idx[0] = direct_r_idx;
		mon_cnt = 1;

		/* Terminate the list */
		mon_idx[1] = -1;

		(void)visual_mode_command('v', &visual_list, browser_rows - 1, wid - (max + 3),
			&attr_top, &char_left, &r_info[direct_r_idx].x_attr, &r_info[direct_r_idx].x_char, need_redraw);
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	old_grp_cur = -1;
	grp_cur = grp_top = 0;
	mon_cur = mon_top = 0;

	flag = FALSE;
	redraw = TRUE;

	mode = visual_only ? 0x02 : 0x00;

	while (!flag)
	{
		char ch;
		monster_race *r_ptr;

		if (redraw)
		{
			clear_from(0);

#ifdef JP
			prt(format("%s - モンスター", !visual_only ? "知識" : "表示"), 2, 0);
			if (direct_r_idx < 0) prt("グループ", 4, 0);
			prt("名前", 4, max + 3);
			if (wizard || visual_only) prt("Idx", 4, 62);
			prt("文字", 4, 67);
			if (!visual_only) prt("殺害数", 4, 72);
#else
			prt(format("%s - monsters", !visual_only ? "Knowledge" : "Visuals"), 2, 0);
			if (direct_r_idx < 0) prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (wizard || visual_only) prt("Idx", 4, 62);
			prt("Sym", 4, 68);
			if (!visual_only) prt("Kills", 4, 73);
#endif

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			if (direct_r_idx < 0)
			{
				for (i = 0; i < browser_rows; i++)
				{
					Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
				}
			}

			redraw = FALSE;
		}

		if (direct_r_idx < 0)
		{
			/* Scroll group list */
			if (grp_cur < grp_top) grp_top = grp_cur;
			if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

			/* Display a list of monster groups */
			display_group_list(0, 6, max, browser_rows, grp_idx, monster_group_text, grp_cur, grp_top);

			if (old_grp_cur != grp_cur)
			{
				old_grp_cur = grp_cur;

				/* Get a list of monsters in the current group */
				mon_cnt = collect_monsters(grp_idx[grp_cur], mon_idx, mode);
			}

			/* Scroll monster list */
			while (mon_cur < mon_top)
				mon_top = MAX(0, mon_top - browser_rows/2);
			while (mon_cur >= mon_top + browser_rows)
				mon_top = MIN(mon_cnt - browser_rows, mon_top + browser_rows/2);
		}

		if (!visual_list)
		{
			/* Display a list of monsters in the current group */
			display_monster_list(max + 3, 6, browser_rows, mon_idx, mon_cur, mon_top, visual_only);
		}
		else
		{
			mon_top = mon_cur;

			/* Display a monster name */
			display_monster_list(max + 3, 6, 1, mon_idx, mon_cur, mon_top, visual_only);

			/* Display visual list below first monster */
			display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
		}

		/* Prompt */
#ifdef JP
		prt(format("<方向>%s%s%s, ESC",
			(!visual_list && !visual_only) ? ", 'r'で思い出を見る" : "",
			visual_list ? ", ENTERで決定" : ", 'v'でシンボル変更",
			(attr_idx || char_idx) ? ", 'c', 'p'でペースト" : ", 'c'でコピー"),
			hgt - 1, 0);
#else
		prt(format("<dir>%s%s%s, ESC",
			(!visual_list && !visual_only) ? ", 'r' to recall" : "",
			visual_list ? ", ENTER to accept" : ", 'v' for visuals",
			(attr_idx || char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"),
			hgt - 1, 0);
#endif

		/* Get the current monster */
		r_ptr = &r_info[mon_idx[mon_cur]];

		if (!visual_only)
		{
			/* Mega Hack -- track this monster race */
			if (mon_cnt) monster_race_track(mon_idx[mon_cur]);

			/* Hack -- handle stuff */
			handle_stuff();
		}

		if (visual_list)
		{
			place_visual_list_cursor(max + 3, 7, r_ptr->x_attr, r_ptr->x_char, attr_top, char_left);
		}
		else if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (mon_cur - mon_top));
		}

		ch = inkey();

		/* Do visual mode command if needed */
		if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, &r_ptr->x_attr, &r_ptr->x_char, need_redraw))
		{
			if (direct_r_idx >= 0)
			{
				switch (ch)
				{
				case '\n':
				case '\r':
				case ESCAPE:
					flag = TRUE;
					break;
				}
			}
			continue;
		}

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				if (!visual_list && !visual_only && (mon_idx[mon_cur] > 0))
				{
					screen_roff(mon_idx[mon_cur], 0);

					(void)inkey();

					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &mon_cur, mon_cnt);

				break;
			}
		}
	}

	/* Free the "mon_idx" array */
	C_KILL(mon_idx, max_r_idx, s16b);
}


/*
 * Display the features in a group.
 */
static void display_feature_list(int col, int row, int per_page, int *feat_idx,
	int feat_cur, int feat_top, bool visual_only, int lighting_level)
{
	int lit_col[F_LIT_MAX], i, j;
	int f_idx_col = use_bigtile ? 62 : 64;

	/* Correct columns 1 and 4 */
	lit_col[F_LIT_STANDARD] = use_bigtile ? (71 - F_LIT_MAX) : 71;
	for (i = F_LIT_NS_BEGIN; i < F_LIT_MAX; i++)
		lit_col[i] = lit_col[F_LIT_STANDARD] + 2 + (i - F_LIT_NS_BEGIN) * 2 + (use_bigtile ? i : 0);

	/* Display lines until done */
	for (i = 0; i < per_page && (feat_idx[feat_top + i] >= 0); i++)
	{
		byte attr;

		/* Get the index */
		int f_idx = feat_idx[feat_top + i];

		/* Access the index */
		feature_type *f_ptr = &f_info[f_idx];

		int row_i = row + i;

		/* Choose a color */
		attr = ((i + feat_top == feat_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the name */
		c_prt(attr, f_name + f_ptr->name, row_i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			/* Display lighting level */
			c_prt(attr, format("(%s)", lighting_level_str[lighting_level]), row_i, col + 1 + strlen(f_name + f_ptr->name));

			c_prt(attr, format("%02x/%02x", f_ptr->x_attr[lighting_level], (unsigned char)f_ptr->x_char[lighting_level]), row_i, f_idx_col - ((wizard || visual_only) ? 6 : 2));
		}
		if (wizard || visual_only)
		{
			c_prt(attr, format("%d", f_idx), row_i, f_idx_col);
		}

		/* Display symbol */
		Term_queue_bigchar(lit_col[F_LIT_STANDARD], row_i, f_ptr->x_attr[F_LIT_STANDARD], f_ptr->x_char[F_LIT_STANDARD], 0, 0);

		Term_putch(lit_col[F_LIT_NS_BEGIN], row_i, TERM_SLATE, '(');
		for (j = F_LIT_NS_BEGIN + 1; j < F_LIT_MAX; j++)
		{
			Term_putch(lit_col[j], row_i, TERM_SLATE, '/');
		}
		Term_putch(lit_col[F_LIT_MAX - 1] + (use_bigtile ? 3 : 2), row_i, TERM_SLATE, ')');

		/* Mega-hack -- Use non-standard colour */
		for (j = F_LIT_NS_BEGIN; j < F_LIT_MAX; j++)
		{
			Term_queue_bigchar(lit_col[j] + 1, row_i, f_ptr->x_attr[j], f_ptr->x_char[j], 0, 0);
		}
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}


/*
 * Interact with feature visuals.
 */
static void do_cmd_knowledge_features(bool *need_redraw, bool visual_only, int direct_f_idx, int *lighting_level)
{
	int i, len, max;
	int grp_cur, grp_top, old_grp_cur;
	int feat_cur, feat_top;
	int grp_cnt, grp_idx[100];
	int feat_cnt;
	int *feat_idx;

	int column = 0;
	bool flag;
	bool redraw;

	bool visual_list = FALSE;
	byte attr_top = 0, char_left = 0;

	int browser_rows;
	int wid, hgt;

	byte attr_old[F_LIT_MAX];
	byte char_old[F_LIT_MAX];
	byte *cur_attr_ptr;
	char *cur_char_ptr;

	C_WIPE(attr_old, F_LIT_MAX, byte);
	C_WIPE(char_old, F_LIT_MAX, byte);

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

	/* Allocate the "feat_idx" array */
	C_MAKE(feat_idx, max_f_idx, int);

	max = 0;
	grp_cnt = 0;

	if (direct_f_idx < 0)
	{
		/* Check every group */
		for (i = 0; feature_group_text[i] != NULL; i++)
		{
			/* Measure the label */
			len = strlen(feature_group_text[i]);

			/* Save the maximum length */
			if (len > max) max = len;

			/* See if any features are known */
			if (collect_features(i, feat_idx, 0x01))
			{
				/* Build a list of groups with known features */
				grp_idx[grp_cnt++] = i;
			}
		}

		feat_cnt = 0;
	}
	else
	{
		feature_type *f_ptr = &f_info[direct_f_idx];

		feat_idx[0] = direct_f_idx;
		feat_cnt = 1;

		/* Terminate the list */
		feat_idx[1] = -1;

		(void)visual_mode_command('v', &visual_list, browser_rows - 1, wid - (max + 3),
			&attr_top, &char_left, &f_ptr->x_attr[*lighting_level], &f_ptr->x_char[*lighting_level], need_redraw);

		for (i = 0; i < F_LIT_MAX; i++)
		{
			attr_old[i] = f_ptr->x_attr[i];
			char_old[i] = f_ptr->x_char[i];
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	old_grp_cur = -1;
	grp_cur = grp_top = 0;
	feat_cur = feat_top = 0;

	flag = FALSE;
	redraw = TRUE;

	while (!flag)
	{
		char ch;
		feature_type *f_ptr;

		if (redraw)
		{
			clear_from(0);

#ifdef JP
			prt("表示 - 地形", 2, 0);
			if (direct_f_idx < 0) prt("グループ", 4, 0);
			prt("名前", 4, max + 3);
			if (use_bigtile)
			{
				if (wizard || visual_only) prt("Idx", 4, 62);
				prt("文字 ( l/ d)", 4, 66);
			}
			else
			{
				if (wizard || visual_only) prt("Idx", 4, 64);
				prt("文字 (l/d)", 4, 68);
			}
#else
			prt("Visuals - features", 2, 0);
			if (direct_f_idx < 0) prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (use_bigtile)
			{
				if (wizard || visual_only) prt("Idx", 4, 62);
				prt("Sym ( l/ d)", 4, 67);
			}
			else
			{
				if (wizard || visual_only) prt("Idx", 4, 64);
				prt("Sym (l/d)", 4, 69);
			}
#endif

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			if (direct_f_idx < 0)
			{
				for (i = 0; i < browser_rows; i++)
				{
					Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
				}
			}

			redraw = FALSE;
		}

		if (direct_f_idx < 0)
		{
			/* Scroll group list */
			if (grp_cur < grp_top) grp_top = grp_cur;
			if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

			/* Display a list of feature groups */
			display_group_list(0, 6, max, browser_rows, grp_idx, feature_group_text, grp_cur, grp_top);

			if (old_grp_cur != grp_cur)
			{
				old_grp_cur = grp_cur;

				/* Get a list of features in the current group */
				feat_cnt = collect_features(grp_idx[grp_cur], feat_idx, 0x00);
			}

			/* Scroll feature list */
			while (feat_cur < feat_top)
				feat_top = MAX(0, feat_top - browser_rows/2);
			while (feat_cur >= feat_top + browser_rows)
				feat_top = MIN(feat_cnt - browser_rows, feat_top + browser_rows/2);
		}

		if (!visual_list)
		{
			/* Display a list of features in the current group */
			display_feature_list(max + 3, 6, browser_rows, feat_idx, feat_cur, feat_top, visual_only, F_LIT_STANDARD);
		}
		else
		{
			feat_top = feat_cur;

			/* Display a list of features in the current group */
			display_feature_list(max + 3, 6, 1, feat_idx, feat_cur, feat_top, visual_only, *lighting_level);

			/* Display visual list below first object */
			display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
		}

		/* Prompt */
#ifdef JP
		prt(format("<方向>%s, 'd'で標準光源効果%s, ESC",
			visual_list ? ", ENTERで決定, 'a'で対象明度変更" : ", 'v'でシンボル変更",
			(attr_idx || char_idx) ? ", 'c', 'p'でペースト" : ", 'c'でコピー"),
			hgt - 1, 0);
#else
		prt(format("<dir>%s, 'd' for default lighting%s, ESC",
			visual_list ? ", ENTER to accept, 'a' for lighting level" : ", 'v' for visuals",
			(attr_idx || char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"),
			hgt - 1, 0);
#endif

		/* Get the current feature */
		f_ptr = &f_info[feat_idx[feat_cur]];
		cur_attr_ptr = &f_ptr->x_attr[*lighting_level];
		cur_char_ptr = &f_ptr->x_char[*lighting_level];

		if (visual_list)
		{
			place_visual_list_cursor(max + 3, 7, *cur_attr_ptr, *cur_char_ptr, attr_top, char_left);
		}
		else if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (feat_cur - feat_top));
		}

		ch = inkey();

		if (visual_list && ((ch == 'A') || (ch == 'a')))
		{
			int prev_lighting_level = *lighting_level;

			if (ch == 'A')
			{
				if (*lighting_level <= 0) *lighting_level = F_LIT_MAX - 1;
				else (*lighting_level)--;
			}
			else
			{
				if (*lighting_level >= F_LIT_MAX - 1) *lighting_level = 0;
				else (*lighting_level)++;
			}

			if (f_ptr->x_attr[prev_lighting_level] != f_ptr->x_attr[*lighting_level])
				attr_top = MAX(0, (f_ptr->x_attr[*lighting_level] & 0x7f) - 5);

			if (f_ptr->x_char[prev_lighting_level] != f_ptr->x_char[*lighting_level])
				char_left = MAX(0, f_ptr->x_char[*lighting_level] - 10);

			continue;
		}

		else if ((ch == 'D') || (ch == 'd'))
		{
			byte prev_x_attr = f_ptr->x_attr[*lighting_level];
			byte prev_x_char = f_ptr->x_char[*lighting_level];

			apply_default_feat_lighting(f_ptr->x_attr, f_ptr->x_char);

			if (visual_list)
			{
				if (prev_x_attr != f_ptr->x_attr[*lighting_level])
					 attr_top = MAX(0, (f_ptr->x_attr[*lighting_level] & 0x7f) - 5);

				if (prev_x_char != f_ptr->x_char[*lighting_level])
					char_left = MAX(0, f_ptr->x_char[*lighting_level] - 10);
			}
			else *need_redraw = TRUE;

			continue;
		}

		/* Do visual mode command if needed */
		else if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, cur_attr_ptr, cur_char_ptr, need_redraw))
		{
			switch (ch)
			{
			/* Restore previous visual settings */
			case ESCAPE:
				for (i = 0; i < F_LIT_MAX; i++)
				{
					f_ptr->x_attr[i] = attr_old[i];
					f_ptr->x_char[i] = char_old[i];
				}

				/* Fall through */

			case '\n':
			case '\r':
				if (direct_f_idx >= 0) flag = TRUE;
				else *lighting_level = F_LIT_STANDARD;
				break;

			/* Preserve current visual settings */
			case 'V':
			case 'v':
				for (i = 0; i < F_LIT_MAX; i++)
				{
					attr_old[i] = f_ptr->x_attr[i];
					char_old[i] = f_ptr->x_char[i];
				}
				*lighting_level = F_LIT_STANDARD;
				break;

			case 'C':
			case 'c':
				if (!visual_list)
				{
					for (i = 0; i < F_LIT_MAX; i++)
					{
						attr_idx_feat[i] = f_ptr->x_attr[i];
						char_idx_feat[i] = f_ptr->x_char[i];
					}
				}
				break;

			case 'P':
			case 'p':
				if (!visual_list)
				{
					/* Allow TERM_DARK text */
					for (i = F_LIT_NS_BEGIN; i < F_LIT_MAX; i++)
					{
						if (attr_idx_feat[i] || (!(char_idx_feat[i] & 0x80) && char_idx_feat[i])) f_ptr->x_attr[i] = attr_idx_feat[i];
						if (char_idx_feat[i]) f_ptr->x_char[i] = char_idx_feat[i];
					}
				}
				break;
			}
			continue;
		}

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &feat_cur, feat_cnt);
				break;
			}
		}
	}

	/* Free the "feat_idx" array */
	C_KILL(feat_idx, max_f_idx, int);
}


/*
 * Print quest status of all finished quests
 */
static bool ang_sort_comp_quest_num(vptr u, vptr v, int a, int b)
{
	int *q_num = (int *)u;
	quest_type *qa = &quest[q_num[a]];
	quest_type *qb = &quest[q_num[b]];

	(void)v;

	if (qa->complev < qb->complev) return TRUE;
	if (qa->complev > qb->complev) return FALSE;
	if (qa->level <= qb->level) return TRUE;
	return FALSE;
}

static void ang_sort_swap_quest_num(vptr u, vptr v, int a, int b)
{
	int *q_num = (int *)u;
	int tmp;

	(void)v;

	tmp = q_num[a];
	q_num[a] = q_num[b];
	q_num[b] = tmp;
}

void do_cmd_knowledge_quests_aux(FILE *fff)
{
	char tmp_str[MAX_NLEN];
	int i;
	int total;
	int *quest_num;
	int dummy;
	int old_quest;

	/* Allocate Memory */
	C_MAKE(quest_num, max_quests, int);

	/* Sort by complete level */
	for (i = 0; i < max_quests; i++)
	{
		quest_num[i] = i;
	}
	ang_sort_comp = ang_sort_comp_quest_num;
	ang_sort_swap = ang_sort_swap_quest_num;
	ang_sort(quest_num, &dummy, max_quests);

#ifdef JP
	fprintf(fff, "\n《達成したクエスト》\n");
#else
	fprintf(fff, "\n< Completed Quest >\n");
#endif
	for (total = 0, i = 0; i < max_quests; i++)
	{
		int num = quest_num[i];

		if (quest[num].status == QUEST_STATUS_FINISHED)
		{
			total++;

			if (num < MIN_RANDOM_QUEST)
			{
				/* Set the quest number temporary */
				old_quest = p_ptr->inside_quest;
				p_ptr->inside_quest = num;

				/* Get the quest */
				init_flags = INIT_ASSIGN;
				process_dungeon_file("q_info.txt", 0, 0, 0, 0);

				/* Reset the old quest number */
				p_ptr->inside_quest = old_quest;

				/* No info from "silent" quests */
				if (quest[num].flags & QUEST_FLAG_SILENT) continue;
			}

			if ((num >= MIN_RANDOM_QUEST) && quest[num].r_idx)
			{
				/* Print the quest info */
				sprintf(tmp_str,
#ifdef JP
					"  %-40s (%3d階)            - レベル%2d\n",
#else
					"  %-40s (Dungeon level: %3d) - level %2d\n",
#endif
					r_name+r_info[quest[num].r_idx].name, quest[num].level, quest[num].complev);
			}
			else
			{
				/* Print the quest info */
#ifdef JP
				sprintf(tmp_str, "  %-40s (危険度:%3d階相当) - レベル%2d\n",
					quest[num].name, quest[num].level, quest[num].complev);
#else
				sprintf(tmp_str, "  %-40s (Danger level: %3d)  - level %2d\n",
					quest[num].name, quest[num].level, quest[num].complev);
#endif
			}

			fprintf(fff, tmp_str);
		}
	}
#ifdef JP
	if (!total) fprintf(fff, "なし\n");
#else
	if (!total) fprintf(fff, "Nothing.\n");
#endif

#ifdef JP
	fprintf(fff, "\n《失敗したクエスト》\n");
#else
	fprintf(fff, "\n< Failed Quest >\n");
#endif
	for (total = 0, i = 0; i < max_quests; i++)
	{
		int num = quest_num[i];

		if ((quest[num].status == QUEST_STATUS_FAILED_DONE) || (quest[num].status == QUEST_STATUS_FAILED))
		{
			total++;

			if (num < MIN_RANDOM_QUEST)
			{
				/* Set the quest number temporary */
				old_quest = p_ptr->inside_quest;
				p_ptr->inside_quest = num;

				/* Get the quest text */
				init_flags = INIT_ASSIGN;
				process_dungeon_file("q_info.txt", 0, 0, 0, 0);

				/* Reset the old quest number */
				p_ptr->inside_quest = old_quest;

				/* No info from "silent" quests */
				if (quest[num].flags & QUEST_FLAG_SILENT) continue;
			}

			if ((num >= MIN_RANDOM_QUEST) && quest[num].r_idx)
			{
				/* Print the quest info */
#ifdef JP
				sprintf(tmp_str, "  %-40s (%3d階)            - レベル%2d\n",
					r_name+r_info[quest[num].r_idx].name, quest[num].level, quest[num].complev);
#else
				sprintf(tmp_str, "  %-40s (Dungeon level: %3d) - level %2d\n",
					r_name+r_info[quest[num].r_idx].name, quest[num].level, quest[num].complev);
#endif
			}
			else
			{
				/* Print the quest info */
#ifdef JP
				sprintf(tmp_str, "  %-40s (危険度:%3d階相当) - レベル%2d\n",
					quest[num].name, quest[num].level, quest[num].complev);
#else
				sprintf(tmp_str, "  %-40s (Danger level: %3d)  - level %2d\n",
					quest[num].name, quest[num].level, quest[num].complev);
#endif
			}
			fprintf(fff, tmp_str);
		}
	}
#ifdef JP
	if (!total) fprintf(fff, "なし\n");
#else
	if (!total) fprintf(fff, "Nothing.\n");
#endif

	if (wizard)
	{
#ifdef JP
		fprintf(fff, "\n《残りのランダムクエスト》\n");
#else
		fprintf(fff, "\n< Remaining Random Quest >\n");
#endif
		for (total = 0, i = 0; i < max_quests; i++)
		{
			int num = quest_num[i];

			/* No info from "silent" quests */
			if (quest[num].flags & QUEST_FLAG_SILENT) continue;

			if ((quest[num].type == QUEST_TYPE_RANDOM) && (quest[num].status == QUEST_STATUS_TAKEN))
			{
				total++;

				/* Print the quest info */
#ifdef JP
				sprintf(tmp_str, "%s (%d階)\n",
#else
				sprintf(tmp_str, "%s (Dungeon level: %d)\n",
#endif
				r_name+r_info[quest[num].r_idx].name, quest[num].level);
				fprintf(fff, tmp_str);
			}
		}
#ifdef JP
		if (!total) fprintf(fff, "なし\n");
#else
		if (!total) fprintf(fff, "Nothing.\n");
#endif
	}

	/* Free Memory */
	C_KILL(quest_num, max_quests, int);
}

/*
 * Print quest status of all active quests
 */
static void do_cmd_knowledge_quests(void)
{
	FILE *fff;
	char file_name[1024];
	char tmp_str[MAX_NLEN];
	char rand_tmp_str[110] = "\0";
	char name[MAX_NLEN];
	monster_race *r_ptr;
	int i;
	int rand_level = 100;
	int total = 0;

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);
	if (!fff) {
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
	    msg_format("Failed to create temporary file %s.", file_name);
#endif
	    msg_print(NULL);
	    return;
	}

	if (p_ptr->max_dlv > 0)
	{
#ifdef JP
		fprintf(fff, "《帰還場所》\n");
		fprintf(fff, "  %d 階 (%d フィート)\n\n", p_ptr->max_dlv, p_ptr->max_dlv * 50);
#else
		fprintf(fff, "< Recall Deapth >\n");
		fprintf(fff, "  Level %d (%d')\n\n", p_ptr->max_dlv, p_ptr->max_dlv * 50);
#endif
	}

#ifdef JP
	fprintf(fff, "《遂行中のクエスト》\n");
#else
	fprintf(fff, "< Current Quest >\n");
#endif

	for (i = 1; i < max_quests; i++)
	{
		/* No info from "silent" quests */
		if (quest[i].flags & QUEST_FLAG_SILENT) continue;

		if (quest[i].status == QUEST_STATUS_TAKEN || quest[i].status == QUEST_STATUS_COMPLETED)
		{
			int old_quest;
			int j;

			/* Clear the text */
			for (j = 0; j < 10; j++)
			{
				quest_text[j][0] = '\0';
			}

			quest_text_line = 0;

			total++;

			/* Set the quest number temporary */
			old_quest = p_ptr->inside_quest;
			p_ptr->inside_quest = i;

			/* Get the quest text */
			init_flags = INIT_SHOW_TEXT;

			process_dungeon_file("q_info.txt", 0, 0, 0, 0);

			/* Reset the old quest number */
			p_ptr->inside_quest = old_quest;

			if (quest[i].type != QUEST_TYPE_RANDOM)
			{
				char note[80] = "\0";

				if (quest[i].status == QUEST_STATUS_TAKEN)
				{
					if (quest[i].type == QUEST_TYPE_KILL_LEVEL || quest[i].type == QUEST_TYPE_KILL_ANY_LEVEL)
					{
						r_ptr = &r_info[quest[i].r_idx];
						strcpy(name, r_name + r_ptr->name);
						if (quest[i].max_num > 1)
						{
#ifdef JP
							sprintf(note," - %d 体の%sを倒す。(あと %d 体)",quest[i].max_num, name, quest[i].max_num-quest[i].cur_num);
#else
							plural_aux(name);
							sprintf(note," - kill %d %s, have killed %d.",quest[i].max_num, name, quest[i].cur_num);
#endif
						}
						else
#ifdef JP
							sprintf(note," - %sを倒す。",name);
#else
							sprintf(note," - kill %s.",name);
#endif
					}
					else if (quest[i].type == QUEST_TYPE_KILL_NUMBER)
					{
#ifdef JP
						sprintf(note," - %d 体のモンスターを倒す。(あと %d 体)",quest[i].max_num, quest[i].max_num-quest[i].cur_num);
#else
						sprintf(note," - Kill %d monsters, have killed %d.",quest[i].max_num, quest[i].cur_num);
#endif
					}
					else if (quest[i].type == QUEST_TYPE_FIND_ARTIFACT)
					{
						strcpy(name, a_name + a_info[quest[i].k_idx].name);
#ifdef JP
						sprintf(note," - %sを見つけ出す。", name);
#else
						sprintf(note," - Find out %s.", name);
#endif
					}
					else if (quest[i].type == QUEST_TYPE_FIND_EXIT)
#ifdef JP
						sprintf(note," - 探索する。");
#else
						sprintf(note," - Search.");
#endif
					else if (quest[i].type == QUEST_TYPE_KILL_ALL)
#ifdef JP
						sprintf(note," - 全てのモンスターを倒す。");
#else
						sprintf(note," - Kill all monsters.");
#endif
				}

				/* Print the quest info */
#ifdef JP
				sprintf(tmp_str, "%s (危険度:%d階相当)%s\n",
#else
				sprintf(tmp_str, "%s (Danger level: %d)%s\n",
#endif

					quest[i].name, quest[i].level, note);

				fprintf(fff, tmp_str);

				if (quest[i].status == QUEST_STATUS_COMPLETED)
				{
#ifdef JP
					sprintf(tmp_str, "  クエスト達成 - まだ報酬を受けとってない。\n");
#else
					sprintf(tmp_str, "  Quest Completed - Unrewarded\n");
#endif
					fprintf(fff, tmp_str);
				}
				else
				{
					j = 0;

					while (quest_text[j][0] && j < 10)
					{
						fprintf(fff, "  %s\n", quest_text[j]);
						j++;
					}
				}
			}
			else if ((quest[i].type == QUEST_TYPE_RANDOM) &&
				 (quest[i].level < rand_level))
			{
				/* New random */
				rand_level = quest[i].level;

				if (p_ptr->max_dlv >= rand_level)
				{
					/* Print the quest info */
					r_ptr = &r_info[quest[i].r_idx];
					strcpy(name, r_name + r_ptr->name);

					if (quest[i].max_num > 1)
					{
#ifdef JP
						sprintf(rand_tmp_str,"%s (%d 階) - %d 体の%sを倒す。(あと %d 体)\n",
							quest[i].name, quest[i].level,
							quest[i].max_num, name, quest[i].max_num-quest[i].cur_num);
#else
						plural_aux(name);
						sprintf(rand_tmp_str,"%s (Dungeon level: %d)\n  Kill %d %s, have killed %d.\n",
							quest[i].name, quest[i].level,
							quest[i].max_num, name, quest[i].cur_num);
#endif
					}
					else
					{
#ifdef JP
						sprintf(rand_tmp_str,"%s (%d 階) - %sを倒す。\n",
#else
						sprintf(rand_tmp_str,"%s (Dungeon level: %d)\n  Kill %s.\n",
#endif
							quest[i].name, quest[i].level, name);
					}
				}
			}
		}
	}

	/* Print the current random quest  */
	if (rand_tmp_str[0]) fprintf(fff, rand_tmp_str);

#ifdef JP
	if (!total) fprintf(fff, "なし\n");
#else
	if (!total) fprintf(fff, "Nothing.\n");
#endif

	/** Print finished quests */
	do_cmd_knowledge_quests_aux(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
	show_file(file_name, "クエスト達成状況", 0, 0);
#else
	show_file(file_name, "Quest status", 0, 0);
#endif

	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Print notes file
 */
static void do_cmd_knowledge_notes(void)
{
	if (!take_notes)
	{
#ifdef JP
		msg_print("プレイ記録を採っていません。");
#else
		msg_print("You can't see play record.");
#endif
		msg_print(NULL);
	}
	else
	{
#ifdef JP
		(void) show_file(notes_file(), "メモ", 0, 0);
#else
		(void) show_file(notes_file(), "Notes", 0, 0);
#endif
	}
}


cptr inven_res_label = 
#ifdef JP
 "                                酸電火冷毒光闇破轟獄因沌劣 盲怖乱痺透命感消復浮";
#else
 "                                AcElFiCoPoLiDkShSoNtNxCaDi BlFeCfFaSeHlEpSdRgLv";
#endif

/* XTRA HACK RESLIST */
static void do_cmd_knowledge_inven_aux(FILE *fff, object_type *o_ptr, 
				       int *j, byte tval, char *where)
{
	char o_name[1024];
	u32b f[3];

	if (!o_ptr->k_idx)return;
	if (o_ptr->tval != tval)return;

	/* 
	* HACK:Ring of Lordly protection and Dragon shield/helm
	* have random resistances.
	*/
	if ( (o_ptr->tval >= TV_BOW && o_ptr->tval<= TV_DRAG_ARMOR && o_ptr->name2)
		|| (o_ptr->tval == TV_RING && o_ptr->sval == SV_RING_LORDLY) 
		|| (o_ptr->tval == TV_SHIELD && o_ptr->sval == SV_DRAGON_SHIELD) 
		|| (o_ptr->tval == TV_HELM && o_ptr->sval == SV_DRAGON_HELM) 
		|| (o_ptr->tval == TV_GLOVES && o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) 
		|| (o_ptr->tval == TV_BOOTS && o_ptr->sval == SV_PAIR_OF_DRAGON_BOOTS) 
		|| (o_ptr->tval == TV_HARD_ARMOR && o_ptr->sval == SV_DRAGON_ARMOR) 
		|| o_ptr->art_name || o_ptr->name1)
    {
		int i;

		if (!(o_ptr->ident & (IDENT_KNOWN))) return;

		object_desc(o_name, o_ptr, OD_NAME_ONLY);

		/* 名前を半角28文字でカット。足りない時はスペースで埋める */
#ifdef JP
		i = null_pos_j(o_name, 28);
#else
		o_name[28] = 0;
		i = strlen(o_name);
#endif
		if(i<28) while(i<28){ o_name[i]=' '; i++; }
		o_name[i] = 0;

		fprintf(fff,"%s %s ", where, o_name);

		if (!(o_ptr->ident & (IDENT_MENTAL))) 
		{
#ifdef JP
			fprintf(fff, "-------不明--------------- -------不明---------\n");
#else
			fprintf(fff, "-------unknown------------ -------unknown------\n");
#endif
		}
		else {
#ifdef JP
			cptr imm = "＊";
			cptr res = "＋";
			cptr non = "・";
#else
			cptr imm = "* ";
			cptr res = "+ ";
			cptr non = ". ";
#endif

			object_flags_known(o_ptr, &f[0], &f[1], &f[2]);

			if (f[1] & TR2_IM_ACID) fprintf(fff, imm);
			else if (f[1] & TR2_RES_ACID) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_IM_ELEC) fprintf(fff, imm);
			else if (f[1] & TR2_RES_ELEC) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_IM_FIRE) fprintf(fff, imm);
			else if (f[1] & TR2_RES_FIRE) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_IM_COLD) fprintf(fff, imm);
			else if (f[1] & TR2_RES_COLD) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_POIS) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_LITE) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_DARK) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_SHARDS) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_SOUND) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_NETHER) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_NEXUS) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_CHAOS) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_DISEN) fprintf(fff, res);
			else fprintf(fff, non);

			fprintf(fff," ");

			if (f[1] & TR2_RES_BLIND) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_FEAR) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_RES_CONF) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_FREE_ACT) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[2] & TR3_SEE_INVIS) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[1] & TR2_HOLD_LIFE) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[2] & TR3_TELEPATHY) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[2] & TR3_SLOW_DIGEST) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[2] & TR3_REGEN) fprintf(fff, res);
			else fprintf(fff, non);

			if (f[2] & TR3_FEATHER) fprintf(fff, res);
			else fprintf(fff, non);

			fprintf(fff,"\n");
		}

		(*j)++;

		if (*j == 9)
		{ 
			*j = 0;
			fprintf(fff,"%s\n", inven_res_label);
		}
    }
}

/*
 * Display *ID* ed weapons/armors's resistances
 */
static void do_cmd_knowledge_inven(void)
{
	FILE *fff;

	char file_name[1024];

	store_type  *st_ptr;
	object_type *o_ptr;

	byte tval;
	int i, j = 0;

	char  where[32];

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);
	if (!fff)
	{
#ifdef JP
		msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
		msg_format("Failed to create temporally file %s.", file_name);
#endif
		msg_print(NULL);
		return;
	}
	fprintf(fff,"%s\n",inven_res_label);

	for (tval=TV_BOW; tval <= TV_RING; tval++)
	{
		if (j != 0 && (tval == TV_DIGGING || tval == TV_BOOTS ||
			tval == TV_GLOVES || tval == TV_HELM ||
			tval == TV_SHIELD || tval == TV_CLOAK ||
			tval == TV_SOFT_ARMOR || tval >=TV_LITE))
		{
			for (; j < 9; j++) fprintf(fff, "\n");
			j = 0;
			fprintf(fff,"%s\n",inven_res_label);
		}

#ifdef JP
		strcpy(where, "装");
#else
		strcpy(where, "E ");
#endif
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			o_ptr = &inventory[i];
			do_cmd_knowledge_inven_aux(fff, o_ptr, &j, tval, where);
		}

#ifdef JP
		strcpy(where, "持");
#else
		strcpy(where, "I ");
#endif
		for (i = 0; i < INVEN_PACK; i++)
		{
			o_ptr = &inventory[i];
			do_cmd_knowledge_inven_aux(fff, o_ptr, &j, tval, where);
		}

		/* Print all homes in the different towns */
		st_ptr = &town[1].store[STORE_HOME];
#ifdef JP
		strcpy(where, "家");
#else
		strcpy(where, "H ");
#endif
		/* Dump all available items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			o_ptr = &st_ptr->stock[i];
			do_cmd_knowledge_inven_aux(fff, o_ptr, &j, tval, where);
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
	show_file(file_name, "*鑑定*済み武器/防具の耐性リスト", 0, 0);
#else
	show_file(file_name, "Resistances of *identified* equipment", 0, 0);
#endif

	/* Remove the file */
	fd_kill(file_name);
}


/*
* List my home
*
*/
static void do_cmd_knowledge_home(void)
{
	FILE *fff;
	
	int i, x;
	char file_name[1024];
	store_type  *st_ptr;
	char o_name[MAX_NLEN];
	cptr paren = ")";

	process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);
	if (!fff) {
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
	    msg_format("Failed to create temporary file %s.", file_name);
#endif
	    msg_print(NULL);
	    return;
	}
	
	if (fff)
	{
		/* Print all homes in the different towns */
		st_ptr = &town[1].store[STORE_HOME];

		/* Home -- if anything there */
		if (st_ptr->stock_num)
		{
			/* Header with name of the town */
#ifdef JP
			fprintf(fff, "  [ 我が家のアイテム ]\n");
#else
			fprintf(fff, "  [Home Inventory]\n");
#endif
			x = 1;

			/* Dump all available items */
			for (i = 0; i < st_ptr->stock_num; i++)
			{
#ifdef JP
				if ((i % 12) == 0) fprintf(fff, "\n ( %d ページ )\n", x++);
				object_desc(o_name, &st_ptr->stock[i], 0);
				if (strlen(o_name) <= 80-3)
				{
					fprintf(fff, "%c%s %s\n", I2A(i%12), paren, o_name);
				}
				else
				{
					int n;
					char *t;
					for (n = 0, t = o_name; n < 80-3; n++, t++)
						if(iskanji(*t)) {t++; n++;}
					if (n == 81-3) n = 79-3; /* 最後が漢字半分 */

					fprintf(fff, "%c%s %.*s\n", I2A(i%12), paren, n, o_name);
					fprintf(fff, "   %.77s\n", o_name+n);
				}
#else
				object_desc(o_name, &st_ptr->stock[i], 0);
				fprintf(fff, "%c%s %s\n", I2A(i%12), paren, o_name);
#endif

			}

			/* Add an empty line */
			fprintf(fff, "\n\n");
		}
	}
	
	/* Close the file */
	my_fclose(fff);
	
	/* Display the file contents */
#ifdef JP
	show_file(file_name, "我が家のアイテム", 0, 0);
#else
	show_file(file_name, "Home Inventory", 0, 0);
#endif
	
	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Check the status of "autopick"
 */
static void do_cmd_knowledge_autopick(void)
{
	int k;
	FILE *fff;
	char file_name[1024];

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	if (!fff)
	{
#ifdef JP
	    msg_format("一時ファイル %s を作成できませんでした。", file_name);
#else
	    msg_format("Failed to create temporary file %s.", file_name);
#endif
	    msg_print(NULL);
	    return;
	}

	if (!max_autopick)
	{
#ifdef JP
	    fprintf(fff, "自動破壊/拾いには何も登録されていません。");
#else
	    fprintf(fff, "No preference for auto picker/destroyer.");
#endif
	}
	else
	{
#ifdef JP
	    fprintf(fff, "   自動拾い/破壊には現在 %d行登録されています。\n\n", max_autopick);
#else
	    fprintf(fff, "   There are %d registered lines for auto picker/destroyer.\n\n", max_autopick);
#endif
	}

	for (k = 0; k < max_autopick; k++)
	{
		cptr tmp;
		byte act = autopick_list[k].action;

		if (act & DONT_AUTOPICK)
		{
#ifdef JP
			tmp = "放置";
#else
			tmp = "Leave";
#endif
		}
		else if (act & DO_AUTODESTROY)
		{
#ifdef JP
			tmp = "破壊";
#else
			tmp = "Destroy";
#endif
		}
		else
		{
#ifdef JP
			tmp = "拾う";
#else
			tmp = "Pickup";
#endif
		}

		if (act & DO_DISPLAY)
			fprintf(fff, "%11s", format("[%s]", tmp));
		else
			fprintf(fff, "%11s", format("(%s)", tmp));

		tmp = autopick_line_from_entry(&autopick_list[k]);
		fprintf(fff, " %s", tmp);
		string_free(tmp);
		fprintf(fff, "\n");
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
	show_file(file_name, "自動拾い/破壊 設定リスト", 0, 0);
#else
	show_file(file_name, "Auto-picker/Destroyer", 0, 0);
#endif

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	int i, p = 0;
	bool need_redraw = FALSE;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save the screen */
	screen_save();

	/* Interact until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
#ifdef JP
		prt(format("%d/2 ページ", (p+1)), 2, 65);
		prt("現在の知識を確認する", 3, 0);
#else
		prt(format("page %d/2", (p+1)), 2, 65);
		prt("Display current knowledge", 3, 0);
#endif

		/* Give some choices */
		if (p == 0)
		{
#ifdef JP
			prt("(1) 既知の 伝説のアイテム       の一覧",  6, 5);
			prt("(2) 既知の アイテム             の一覧",  7, 5);
			prt("(3) 既知の モンスター           の一覧",  8, 5);
			prt("(4) 既知の ユニーク・モンスター の一覧",  9, 5);
			prt("(5) 倒した敵の数                の一覧", 10, 5);
			prt("(6) 現在のペット                の一覧", 11, 5);
			prt("(7) 我が家のアイテム            の一覧", 12, 5);
			prt("(8) *鑑定*済み装備の耐性        の一覧", 13, 5);
			prt("(9) 現在の地形の設定            の一覧", 14, 5);
#else
			prt("(1) Display known artifacts", 6, 5);
			prt("(2) Display known objects", 7, 5);
			prt("(3) Display known monsters", 8, 5);
			prt("(4) Display known uniques", 9, 5);
			prt("(5) Display kill count", 10, 5);
			prt("(6) Display current pets", 11, 5);
			prt("(7) Display home inventory", 12, 5);
			prt("(8) Display *identified* equip.", 13, 5);
			prt("(9) Display features settings", 14, 5);
#endif
		}
		else
		{
#ifdef JP
			prt("(a) 突然変異                    の一覧",  6, 5);
			prt("(b) 実行中のクエスト            の一覧",  7, 5);
			prt("(c) 現在の自動拾い/破壊設定     の一覧",  8, 5);
			prt("(d) プレイ記録                  を表示",  9, 5);
#else
			prt("(a) Display mutations", 6, 5);
			prt("(b) Display current quests", 7, 5);
			prt("(c) Display auto pick/destroy", 8, 5);
			prt("(d) Display play record", 9, 5);
#endif
		}

		/* Prompt */
#ifdef JP
		prt("-続く-", 16, 8);
		prt("ESC) 抜ける", 21, 1);
		prt("SPACE) 次ページ", 21, 30);
		/* prt("-) 前ページ", 21, 60); */
		prt("コマンド:", 20, 0);
#else
		prt("-more-", 16, 8);
		prt("ESC) Exit menu", 21, 1);
		prt("SPACE) Next page", 21, 30);
		/* prt("-) Previous page", 21, 60); */
		prt("Command: ", 20, 0);
#endif

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		switch (i)
		{
		case ' ': /* Page change */
		case '-':
			p = 1 - p;
			break;
		case '1': /* Artifacts */
			do_cmd_knowledge_artifacts();
			break;
		case '2': /* Objects */
			do_cmd_knowledge_objects(&need_redraw, FALSE, -1);
			break;
		case '3': /* Monsters */
			do_cmd_knowledge_monsters(&need_redraw, FALSE, -1);
			break;
		case '4': /* Uniques */
			do_cmd_knowledge_uniques();
			break;
		case '5': /* Kill count */
			do_cmd_knowledge_kill_count();
			break;
		case '6': /* Pets */
			do_cmd_knowledge_pets();
			break;
		case '7': /* Home */
			do_cmd_knowledge_home();
			break;
		case '8': /* Resist list */
			do_cmd_knowledge_inven();
			break;
		case '9': /* Feature list */
			{
				int lighting_level = F_LIT_STANDARD;
				do_cmd_knowledge_features(&need_redraw, FALSE, -1, &lighting_level);
			}
			break;

		case 'a': /* Mutations */
			do_cmd_knowledge_mutations();
			break;
		case 'b': /* Quests */
			do_cmd_knowledge_quests();
			break;
		case 'c': /* Autopick setting list */
			do_cmd_knowledge_autopick();
			break;
		case 'd': /* Notes */
			do_cmd_knowledge_notes();
			break;
		default: /* Unknown option */
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}

	/* Restore the screen */
	screen_load();

	if (need_redraw) do_cmd_redraw();
}


/*
 * Check on the status of an active quest
 */
void do_cmd_checkquest(void)
{
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save the screen */
	screen_save();

	/* Quest info */
	do_cmd_knowledge_quests();

	/* Restore the screen */
	screen_load();
}


/*
 * Display the time and date
 */
void do_cmd_time(void)
{
	int day, hour, min, full;
	int start = 9999;
	int end = -9999;
	int num = 0;
	char desc[1024];
	char buf[1024];

	FILE *fff;

	extract_day_hour_min(&day, &hour, &min);

	full = hour * 100 + min;

#ifdef JP
	strcpy(desc, "変な時刻だ。");
#else
	strcpy(desc, "It is a strange time.");
#endif


	/* Message */
#ifdef JP
	msg_format("%d 日目,時刻は%d:%02d %sです。",
	           day, (hour % 12 == 0) ? 12 : (hour % 12),
	           min, (hour < 12) ? "AM" : "PM");
#else
	msg_format("This is day %d. The time is %d:%02d %s.",
	           day, (hour % 12 == 0) ? 12 : (hour % 12),
	           min, (hour < 12) ? "AM" : "PM");
#endif


	/* Find the path */
	if (!randint0(10) || p_ptr->image)
	{
#ifdef JP
		path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "timefun_j.txt");
#else
		path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "timefun.txt");
#endif
	}
	else
	{
#ifdef JP
		path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "timenorm_j.txt");
#else
		path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "timenorm.txt");
#endif
	}

	/* Open this file */
	fff = my_fopen(buf, "rt");

	/* Oops */
	if (!fff) return;

	/* Find this time */
	while (!my_fgets(fff, buf, sizeof(buf)))
	{
		/* Ignore comments */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Ignore invalid lines */
		if (buf[1] != ':') continue;

		/* Process 'Start' */
		if (buf[0] == 'S')
		{
			/* Extract the starting time */
			start = atoi(buf + 2);

			/* Assume valid for an hour */
			end = start + 59;

			/* Next... */
			continue;
		}

		/* Process 'End' */
		if (buf[0] == 'E')
		{
			/* Extract the ending time */
			end = atoi(buf + 2);

			/* Next... */
			continue;
		}

		/* Ignore incorrect range */
		if ((start > full) || (full > end)) continue;

		/* Process 'Description' */
		if (buf[0] == 'D')
		{
			num++;

			/* Apply the randomizer */
			if (!randint0(num)) strcpy(desc, buf + 2);

			/* Next... */
			continue;
		}
	}

	/* Message */
	msg_print(desc);

	/* Close the file */
	my_fclose(fff);
}
