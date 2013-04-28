/* File: util.c */

/*
 * Simple text processing, macros, keypresses, and object inscriptions.  User
 * input.  Messages.  Save, load, and clear the screen.  Print text out to
 * screen and files.  Ask for commands, strings, and numbers.  Handle repeated
 * commands.  Gamma correction and colors.  Math functions (dice, angular math,
 * etc.).  Special utility functions.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


#include "angband.h"


/*
 * Pause the game.  Argument is number of milliseconds.
 *
 * Note:  The timers in most desktop machines are accurate only to 1/100th of
 * a second.  Requests for a delay of 33 milliseconds is treated as one of 30
 * milliseconds.
 */
void pause_for(int msec)
{
	if (msec > 0) (void)Term_xtra(TERM_XTRA_DELAY, msec);
}


/*
 * Make a string lower case.
 */
void strlower(char *buf)
{
	char *s;

	/* Lowercase the string */
	for (s = buf; *s != 0; s++) *s = my_tolower((unsigned char)*s);
}


/*
 * Convert a decimal to a single digit hex number
 */
static char hexify(int i)
{
	return (hexsym[i % 16]);
}


/*
 * Convert a hexadecimal-digit into a decimal
 */
static int dehex(char c)
{
	if (isdigit((unsigned char)c)) return (D2I(c));
	if (my_isalpha((unsigned char)c)) return (A2I(my_tolower((unsigned char)c)) + 10);
	return (0);
}


/*
 * The string formatting code uses non-doubled '%'s to indicate where arguments
 * need to be inserted.  This causes trouble when we want to include a literal
 * string that may potentially include '%'s.  The solution is simple:  Double
 * them beforehand.
 */
cptr format_literal(char *str)
{
	int i, j;
	char buf[1024];

	/* Copy the string, doubling all '%' characters */
	for (j = 0, i = 0;; i++)
	{
		/* End of input */
		if (str[i] == '\0') break;

		/* Copy */
		buf[j++] = str[i];

		/* Break */
		if (j >= 1023) break;

		/* Double any '%' characters */
		if (str[i] == '%')
		{
			buf[j++] = '%';
			if (j >= 1023) break;
		}
	}

	/* Terminate */
	buf[j] = '\0';

	/* Copy back */
	strcpy(str, buf);

	/* Return a pointer to our edited string */
	return (str);
}



/*
 * Format and translate a string, then print it out to file.
 *
 * Warning:  If you feed a string that may potentially contain '%' characters
 * to this function (as is done in the character dump code), you need to pre-
 * format the string using "format_literal()".
 */
void x_fprintf(FILE *fff, int encoding, cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Translate */
	xstr_trans(buf, encoding);

	fprintf(fff, buf);
}


/*
 * Transform macro trigger name ('\[alt-D]' etc..)
 * into macro trigger key code ('^_O_64\r' or etc..)
 */
static size_t trigger_text_to_ascii(char *buf, size_t max, cptr *strptr)
{
	cptr str = *strptr;
	bool mod_status[MAX_MACRO_MOD];

	int i, len = 0;
	int shiftstatus = 0;
	cptr key_code;

	size_t current_len = strlen(buf);

	/* No definition of trigger names */
	if (macro_template == NULL) return 0;

	/* Initialize modifier key status */
	for (i = 0; macro_modifier_chr[i]; i++)
		mod_status[i] = FALSE;

	str++;

	/* Examine modifier keys */
	while (TRUE)
	{
		/* Look for modifier key name */
		for (i = 0; macro_modifier_chr[i]; i++)
		{
			len = strlen(macro_modifier_name[i]);

			if (!my_strnicmp(str, macro_modifier_name[i], len))
				break;
		}

		/* None found? */
		if (!macro_modifier_chr[i]) break;

		/* Proceed */
		str += len;

		/* This modifier key is pressed */
		mod_status[i] = TRUE;

		/* Shift key might be going to change keycode */
		if ('S' == macro_modifier_chr[i])
			shiftstatus = 1;
	}

	/* Look for trigger name */
	for (i = 0; i < max_macrotrigger; i++)
	{
		len = strlen(macro_trigger_name[i]);

		/* Found it and it is ending with ']' */
		if (!my_strnicmp(str, macro_trigger_name[i], len) && (']' == str[len]))
			break;
	}

	/* Invalid trigger name? */
	if (i == max_macrotrigger)
	{
		/*
		 * If this invalid trigger name ends with ']',
		 * skip all of it to avoid defining a strange macro trigger
		 */
		str = strchr(str, ']');

		if (str)
		{
			strnfcat(buf, max, &current_len, "\x1F\r");

			*strptr = str; /* where **strptr == ']' */
		}

		return (current_len);
	}

	/* Get keycode for this trigger name */
	key_code = macro_trigger_keycode[shiftstatus][i];

	/* Proceed */
	str += len;

	/* Begin with '^_' */
	strnfcat(buf, max, &current_len, "\x1F");

	/* Write key code style trigger using template */
	for (i = 0; macro_template[i]; i++)
	{
		char ch = macro_template[i];
		int j;

		switch (ch)
		{
			case '&':
			{
				/* Modifier key character */
				for (j = 0; macro_modifier_chr[j]; j++)
				{
					if (mod_status[j])
						strnfcat(buf, max, &current_len, "%c", macro_modifier_chr[j]);
				}
				break;
			}
			case '#':
			{
				/* Key code */
				strnfcat(buf, max, &current_len, "%s", key_code);
				break;
			}
			default:
			{
				/* Fixed string */
				strnfcat(buf, max, &current_len, "%c", ch);
				break;
			}
		}
	}

	/* End with '\r' */
	strnfcat(buf, max, &current_len, "\r");

	/* Succeed */
	*strptr = str; /* where **strptr == ']' */

	return (current_len);
}


/*
 * Hack -- convert a printable string into real ascii
 *
 * This function will not work on non-ascii systems.
 *
 * To be safe, "buf" should be at least as large as "str".
 */
void text_to_ascii(char *buf, size_t len, cptr str)
{
	char *s = buf;

	/* Analyze the "ascii" string */
	while (*str)
	{
		/* Check if the buffer is long enough */
		if (s >= buf + len - 1) break;

		/* Backslash codes */
		if (*str == '\\')
		{
			/* Skip the backslash */
			str++;

			/* Paranoia */
			if (!(*str)) break;

			/* Macro Trigger */
			if (*str == '[')
			{
				/* Terminate before appending the trigger */
				*s = '\0';

				s += trigger_text_to_ascii(buf, len, &str);
			}

			/* Hack -- simple way to specify Escape */
			if (*str == 'e')
			{
				*s++ = ESCAPE;
			}

			/* Hack -- simple way to specify "space" */
			else if (*str == 's')
			{
				*s++ = ' ';
			}

			/* Backspace */
			else if (*str == 'b')
			{
				*s++ = '\b';
			}

			/* Newline */
			else if (*str == 'n')
			{
				*s++ = '\n';
			}

			/* Return */
			else if (*str == 'r')
			{
				*s++ = '\r';
			}

			/* Tab */
			else if (*str == 't')
			{
				*s++ = '\t';
			}

			/* Bell */
			else if (*str == 'a')
			{
				*s++ = '\a';
			}

			/* Hack -- Actual "caret" */
			else if (*str == '^')
			{
				*s++ = '^';
			}

			/* Actual "backslash" */
			else if (*str == '\\')
			{
				*s++ = '\\';
			}

			/* Hack -- Hex-mode */
			else if (*str == 'x')
			{
				*s = 16 * dehex(*++str);
				*s++ += dehex(*++str);
			}

			/* Oops */
			else
			{
				*s = *str;
			}

			/* Skip the final char */
			str++;
		}

		/* Normal Control codes */
		else if (*str == '^')
		{
			str++;
			if (*str)
			{
				*s++ = KTRL(*str);
				str++;
			}
		}

		/* Normal chars */
		else
		{
			*s++ = *str++;
		}
	}

	/* Terminate */
	*s = '\0';
}


/*
 * Hack -- convert a string into a printable form
 *
 * This function will not work on non-ascii systems.
 */
void ascii_to_text(char *buf, size_t len, cptr str)
{
	char *s = buf;

	/* Analyze the "ascii" string */
	while (*str)
	{
		byte i = (byte) (*str++);

		/* Check if the buffer is long enough */
		/* HACK - always assume worst case (hex-value + '\0') */
		if (s >= buf + len - 5) break;

		if (i == ESCAPE)
		{
			*s++ = '\\';
			*s++ = 'e';
		}
		else if (i == ' ')
		{
			*s++ = '\\';
			*s++ = 's';
		}
		else if (i == '\b')
		{
			*s++ = '\\';
			*s++ = 'b';
		}
		else if (i == '\t')
		{
			*s++ = '\\';
			*s++ = 't';
		}
		else if (i == '\a')
		{
			*s++ = '\\';
			*s++ = 'a';
		}
		else if (i == '\n')
		{
			*s++ = '\\';
			*s++ = 'n';
		}
		else if (i == '\r')
		{
			*s++ = '\\';
			*s++ = 'r';
		}
		else if (i == '^')
		{
			*s++ = '\\';
			*s++ = '^';
		}
		else if (i == '\\')
		{
			*s++ = '\\';
			*s++ = '\\';
		}
		else if (i < 32)
		{
			*s++ = '^';
			*s++ = UN_KTRL(i);
		}
		else if (i < 127)
		{
			*s++ = i;
		}
		else
		{
			*s++ = '\\';
			*s++ = 'x';
			*s++ = hexify((int)i / 16);
			*s++ = hexify((int)i % 16);
		}
	}

	/* Terminate */
	*s = '\0';
}



/*
 * The "macro" package
 *
 * Functions are provided to manipulate a collection of macros, each
 * of which has a trigger pattern string and a resulting action string
 * and a small set of flags.
 */



/*
 * Determine if any macros have ever started with a given character.
 */
static bool macro__use[256];


/*
 * Find the macro (if any) which exactly matches the given pattern
 */
int macro_find_exact(cptr pat)
{
	int i;

	/* Nothing possible */
	if (!macro__use[(byte) (pat[0])])
	{
		return (-1);
	}

	/* Scan the macros */
	for (i = 0; i < macro__num; ++i)
	{
		/* Skip macros which do not match the pattern */
		if (!streq(macro__pat[i], pat)) continue;

		/* Found one */
		return (i);
	}

	/* No matches */
	return (-1);
}


/*
 * Find the first macro (if any) which contains the given pattern
 */
static int macro_find_check(cptr pat)
{
	int i;

	/* Nothing possible */
	if (!macro__use[(byte) (pat[0])])
	{
		return (-1);
	}

	/* Scan the macros */
	for (i = 0; i < macro__num; ++i)
	{
		/* Skip macros which do not contain the pattern */
		if (!prefix(macro__pat[i], pat)) continue;

		/* Found one */
		return (i);
	}

	/* Nothing */
	return (-1);
}


/*
 * Find the first macro (if any) which contains the given pattern and more
 */
static int macro_find_maybe(cptr pat)
{
	int i;

	/* Nothing possible */
	if (!macro__use[(byte) (pat[0])])
	{
		return (-1);
	}

	/* Scan the macros */
	for (i = 0; i < macro__num; ++i)
	{
		/* Skip macros which do not contain the pattern */
		if (!prefix(macro__pat[i], pat)) continue;

		/* Skip macros which exactly match the pattern XXX XXX */
		if (streq(macro__pat[i], pat)) continue;

		/* Found one */
		return (i);
	}

	/* Nothing */
	return (-1);
}


/*
 * Find the longest macro (if any) which starts with the given pattern
 */
static int macro_find_ready(cptr pat)
{
	int i, t, n = -1, s = -1;

	/* Nothing possible */
	if (!macro__use[(byte) (pat[0])])
	{
		return (-1);
	}

	/* Scan the macros */
	for (i = 0; i < macro__num; ++i)
	{
		/* Skip macros which are not contained by the pattern */
		if (!prefix(pat, macro__pat[i])) continue;

		/* Obtain the length of this macro */
		t = strlen(macro__pat[i]);

		/* Only track the "longest" pattern */
		if ((n >= 0) && (s > t)) continue;

		/* Track the entry */
		n = i;
		s = t;
	}

	/* Result */
	return (n);
}


/*
 * Add a macro definition (or redefinition).
 *
 * We should use "act == NULL" to "remove" a macro, but this might make it
 * impossible to save the "removal" of a macro definition.  XXX XXX XXX
 *
 * We should consider refusing to allow macros which contain existing macros,
 * or which are contained in existing macros, because this would simplify the
 * macro analysis code.  XXX XXX XXX
 *
 * We should consider removing the "command macro" crap, and replacing it
 * with some kind of "powerful keymap" ability, but this might make it hard
 * to change the "roguelike" option from inside the game.  XXX XXX XXX
 */
errr macro_add(cptr pat, cptr act)
{
	int n;


	/* Paranoia -- require data */
	if (!pat || !act) return (-1);


	/* Look for any existing macro */
	n = macro_find_exact(pat);

	/* Replace existing macro */
	if (n >= 0)
	{
		/* Free the old macro action */
		(void)string_free(macro__act[n]);
	}

	/* Create a new macro */
	else
	{
		/* Get a new index */
		n = macro__num++;

		/* Boundary check */
		if (macro__num >= MACRO_MAX) quit("Too many macros!");

		/* Save the pattern */
		macro__pat[n] = string_make(pat);
	}

	/* Save the action */
	macro__act[n] = string_make(act);

	/* Efficiency */
	macro__use[(byte) (pat[0])] = TRUE;

	/* Success */
	return (0);
}



/*
 * Initialize the "macro" package
 */
errr macro_init(void)
{
	/* Macro patterns */
	C_MAKE(macro__pat, MACRO_MAX, cptr);

	/* Macro actions */
	C_MAKE(macro__act, MACRO_MAX, cptr);

	/* Success */
	return (0);
}

/*
 * Free the macro package
 */
errr macro_free(void)
{
	int i, j;

	/* Free the macros */
	for (i = 0; i < macro__num; ++i)
	{
		(void)string_free(macro__pat[i]);
		(void)string_free(macro__act[i]);
	}

	FREE(macro__pat);
	FREE(macro__act);

	/* Free the keymaps */
	for (i = 0; i < KEYMAP_MODES; ++i)
	{
		for (j = 0; j < (int)N_ELEMENTS(keymap_act[i]); ++j)
		{
			(void)string_free(keymap_act[i][j]);
			keymap_act[i][j] = NULL;
		}
	}

	/* Success */
	return (0);
}

/*
 * Free the macro trigger package
 */
errr macro_trigger_free(void)
{
	int i;
	int num;

	if (macro_template != NULL)
	{
		/* Free the template */
		(void)string_free(macro_template);
		macro_template = NULL;

		/* Free the trigger names and keycodes */
		for (i = 0; i < max_macrotrigger; i++)
		{
			(void)string_free(macro_trigger_name[i]);

			(void)string_free(macro_trigger_keycode[0][i]);
			(void)string_free(macro_trigger_keycode[1][i]);
		}

		/* No more macro triggers */
		max_macrotrigger = 0;

		/* Count modifier-characters */
		num = strlen(macro_modifier_chr);

		/* Free modifier names */
		for (i = 0; i < num; i++)
		{
			(void)string_free(macro_modifier_name[i]);
		}

		/* Free modifier chars */
		(void)string_free(macro_modifier_chr);
		macro_modifier_chr = NULL;
	}

	/* Success */
	return (0);
}

/*
 * Flush all pending input.
 *
 * Actually, remember the flush, using the "inkey_xtra" flag, and in the
 * next call to "inkey()", perform the actual flushing, for efficiency,
 * and correctness of the "inkey()" function.
 */
void flush(void)
{
	/* Do it later */
	inkey_xtra = TRUE;
}





/*
 * Local variable -- we are inside a "macro action"
 *
 * Do not match any macros until "ascii 30" is found.
 */
static bool parse_macro = FALSE;


/*
 * Local variable -- we are inside a "macro trigger"
 *
 * Strip all keypresses until a low ascii value is found.
 */
static bool parse_under = FALSE;




/*
 * Helper function called only from "inkey()"
 *
 * This function does almost all of the "macro" processing.
 *
 * We use the "Term_key_push()" function to handle "failed" macros, as well
 * as "extra" keys read in while choosing the proper macro, and also to hold
 * the action for the macro, plus a special "ascii 30" character indicating
 * that any macro action in progress is complete.  Embedded macros are thus
 * illegal, unless a macro action includes an explicit "ascii 30" character,
 * which would probably be a massive hack, and might break things.
 *
 * Only 500 (0+1+2+...+29+30) milliseconds may elapse between each key in
 * the macro trigger sequence.  If a key sequence forms the "prefix" of a
 * macro trigger, 500 milliseconds must pass before the key sequence is
 * known not to be that macro trigger.  XXX XXX XXX
 */
static char inkey_aux(void)
{
	int k, n;
	int p = 0, w = 0;

	char ch;

	cptr pat, act;

	char buf[1024];


	/* Wait for a keypress */
	(void)(Term_inkey(&ch, TRUE, TRUE));


	/* End "macro action" */
	if (ch == 30) parse_macro = FALSE;

	/* Inside "macro action" */
	if (ch == 30) return (ch);

	/* Inside "macro action" */
	if (parse_macro) return (ch);

	/* Inside "macro trigger" */
	if (parse_under) return (ch);


	/* Save the first key, advance */
	buf[p++] = ch;
	buf[p] = '\0';


	/* Check for possible macro */
	k = macro_find_check(buf);

	/* No macro pending */
	if (k < 0) return (ch);


	/* Wait for a macro, or a timeout */
	while (TRUE)
	{
		/* Check for pending macro */
		k = macro_find_maybe(buf);

		/* No macro pending */
		if (k < 0) break;

		/* Check for (and remove) a pending key */
		if (0 == Term_inkey(&ch, FALSE, TRUE))
		{
			/* Append the key */
			buf[p++] = ch;
			buf[p] = '\0';

			/* Restart wait */
			w = 0;
		}

		/* No key ready */
		else
		{
			/* Increase "wait" */
			w += 10;

			/* Excessive delay */
			if (w >= 100) break;

			/* Delay */
			pause_for(w);
		}
	}



	/* Check for available macro */
	k = macro_find_ready(buf);

	/* No macro available */
	if (k < 0)
	{
		/* Push all the keys back on the queue */
		while (p > 0)
		{
			/* Push the key, notice over-flow */
			if (Term_key_push(buf[--p])) return (0);
		}

		/* Wait for (and remove) a pending key */
		(void)Term_inkey(&ch, TRUE, TRUE);

		/* Return the key */
		return (ch);
	}


	/* Get the pattern */
	pat = macro__pat[k];

	/* Get the length of the pattern */
	n = strlen(pat);

	/* Push the "extra" keys back on the queue */
	while (p > n)
	{
		/* Push the key, notice over-flow */
		if (Term_key_push(buf[--p])) return (0);
	}


	/* Begin "macro action" */
	parse_macro = TRUE;

	/* Push the "end of macro action" key */
	if (Term_key_push(30)) return (0);


	/* Get the macro action */
	act = macro__act[k];

	/* Get the length of the action */
	n = strlen(act);

	/* Push the macro "action" onto the key queue */
	while (n > 0)
	{
		/* Push the key, notice over-flow */
		if (Term_key_push(act[--n])) return (0);
	}


	/* Hack -- Force "inkey()" to call us again */
	return (0);
}



/*
 * Mega-Hack -- special "inkey_next" pointer.  XXX XXX XXX
 *
 * This special pointer allows a sequence of keys to be "inserted" into
 * the stream of keys returned by "inkey()".  This key sequence will not
 * trigger any macros, and cannot be bypassed by the Borg.  It is used
 * in Angband to handle "keymaps".
 */
static cptr inkey_next = NULL;


#ifdef ALLOW_BORG

/*
 * Mega-Hack -- special "inkey_hack" hook.  XXX XXX XXX
 *
 * This special function hook allows the "Borg" (see elsewhere) to take
 * control of the "inkey()" function, and substitute in fake keypresses.
 */
char (*inkey_hack) (int flush_first) = NULL;

#endif /* ALLOW_BORG */


/*
 * Get a keypress from the user.
 *
 * This function recognizes a few "global parameters".  These are variables
 * which, if set to TRUE before calling this function, will have an effect
 * on this function, and which are always reset to FALSE by this function
 * before this function returns.  Thus they function just like normal
 * parameters, except that most calls to this function can ignore them.
 *
 * If "inkey_xtra" is TRUE, then all pending keypresses will be flushed,
 * and any macro processing in progress will be aborted.  This flag is
 * set by the "flush()" function, which does not actually flush anything
 * itself, but rather, triggers delayed input flushing via "inkey_xtra".
 *
 * If "inkey_scan" is TRUE, then we will immediately return "zero" if no
 * keypress is available, instead of waiting for a keypress.
 *
 * If "inkey_base" is TRUE, then all macro processing will be bypassed.
 * If "inkey_base" and "inkey_scan" are both TRUE, then this function will
 * not return immediately, but will wait for a keypress for as long as the
 * normal macro matching code would, allowing the direct entry of macro
 * triggers.  The "inkey_base" flag is extremely dangerous!
 *
 * If "inkey_flag" is TRUE, then we will assume that we are waiting for a
 * normal command, and we will only show the cursor if "highlight_player" is
 * TRUE or we are showing a special screen (like stores, options, help, etc.),
 * instead of always showing the cursor.  The various "main-xxx.c" files
 * should avoid saving the game in response to a "menu item" request unless
 * "inkey_flag" is TRUE, to prevent savefile corruption.
 *
 * "inkey_cursor_hack" is a brute-force hack that makes any cursor in any
 * window appear or disappear.  I added it because it was either stick in a
 * quick fix or rethink cursor visibility from scratch, and I lacked the time
 * to do the latter right.  Unlike other inkey variables, "inkey_cursor_hack"
 * is NOT cleared in this function.  -LM-   XXX XXX XXX
 *
 *
 * If we are waiting for a keypress, and no keypress is ready, then we will
 * refresh (once) the window which was active when this function was called.
 *
 * If a compiler define is set, "back-quote" is automatically converted into
 * "escape".  This is done after the macro matching, so the user can still
 * make a macro for "backquote".
 *
 * Note the special handling of "ascii 30" (ctrl-caret, aka ctrl-shift-six)
 * and "ascii 31" (ctrl-underscore, aka ctrl-shift-minus), which are used to
 * provide support for simple keyboard "macros".  These keys are so strange
 * that their loss as normal keys will probably be noticed by nobody.  The
 * "ascii 30" key is used to indicate the "end" of a macro action, which
 * allows recursive macros to be avoided.  The "ascii 31" key is used by
 * some of the "main-xxx.c" files to introduce macro trigger sequences.
 *
 * Hack -- we use "ascii 29" (ctrl-right-bracket) as a special "magic" key,
 * which can be used to give a variety of "sub-commands" which can be used
 * any time.  These sub-commands could include commands to take a picture of
 * the current screen, to start/stop recording a macro action, etc.
 *
 * If "term_main" is not active, we will make it active during this
 * function, so that the various "main-xxx.c" files can assume that input
 * is only requested (via "Term_inkey()") when "term_main" is active.
 *
 * Mega-Hack -- This function is used as the entry point for clearing the
 * "signal_count" variable, and of the "character_saved" variable.
 *
 * Hack -- Note the use of "inkey_next" to allow "keymaps" to be processed.
 *
 * Mega-Hack -- Note the use of "inkey_hack" to allow the "Borg" to steal
 * control of the keyboard from the user.
 */
char inkey(int allow_mouse)
{
	bool cursor_state[TERM_MAX];

	char kk;

	char ch = 0;

	bool done = FALSE;

	term *old = Term;

	int i;
	bool vis;
	bool reset_cursors = FALSE;


	/* Hack -- Use the "inkey_next" pointer */
	if (inkey_next && *inkey_next && !inkey_xtra)
	{
		/* Get next character, and advance */
		ch = *inkey_next++;

		/* Cancel the various "global parameters" */
		inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;

		/* Accept result */
		return (ch);
	}

	/* Forget pointer */
	inkey_next = NULL;


#ifdef ALLOW_BORG

	/* Mega-Hack -- Use the special hook */
	if (inkey_hack && ((ch = (*inkey_hack) (inkey_xtra)) != 0))
	{
		/* Cancel the various "global parameters" */
		inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;

		/* Accept result */
		return (ch);
	}

#endif /* ALLOW_BORG */


	/* Hack -- handle delayed "flush()" */
	if (inkey_xtra)
	{
		/* End "macro action" */
		parse_macro = FALSE;

		/* End "macro trigger" */
		parse_under = FALSE;

		/* Forget old keypresses */
		(void)Term_flush();
	}

	/* Clear the cursor_state storage array */
	WIPE(cursor_state, bool);


	/* Usually change cursor visibility (see comments above) */
	if (!inkey_scan && (!inkey_flag || main_screen_inactive ||
	    (highlight_player && !main_screen_inactive)))
	{
		/* Handle only the major screens (for now) */
		for (i = 0; i < TERM_SUBWINDOW; i++)
		{
			/* No term, or term is unavailable */
			if ((!angband_term[i]) || (!angband_term[i]->mapped_flag)) continue;

			/* Activate the term (may not be present) */
			if ((Term_activate(angband_term[i])) && (Term != angband_term[i])) continue;

			/* Save the previous cursor state */
			(void)Term_get_cursor(&cursor_state[i]);

			/* HACK -- Allow explicit cursor hiding or showing */
			if (inkey_cursor_hack[i])
			{
				/* Show or hide cursor, ignore any other rules */
				(void)Term_set_cursor(inkey_cursor_hack[i] > 0);

				/* Refresh only the cursor */
				(void)Term_fresh_cursor();

				continue;
			}

			/* Assume cursor will be shown on main windows, and hidden otherwise */
			vis = (i < TERM_SUBWINDOW);

			/* This is the main term */
			if (i == TERM_MAIN)
			{
				/* The highlight_player cursor moves to the map if appropriate. */
				if ((use_special_map) && (highlight_player) && (!main_screen_inactive)) vis = FALSE;
			}

			/* This is the special map display term */
			else if (i == TERM_MAP)
			{
				/* Cursor visibility usually depends on highlight_player */
				vis = highlight_player;

				/* Hack -- A full-screen display window always hides the map cursor */
				if ((main_screen_inactive) && (!term_main->popup_hack_flag)) vis = FALSE;

				/* Set visibility */
				(void)Term_set_cursor(vis);
			}

			/* Show cursor */
			(void)Term_set_cursor(vis);

			/* Refresh only the cursor */
			(void)Term_fresh_cursor();
		}

		/* We will need to reset cursor visibility later */
		reset_cursors = TRUE;
	}


	/* Hack -- Always activate main term to accept input */
	(void)Term_activate(term_main);

	/* Get a key */
	while (TRUE)
	{
		/* Hack -- Handle "inkey_scan" */
		if (!inkey_base && inkey_scan &&
			(0 != Term_inkey(&kk, FALSE, FALSE)))
		{
			break;
		}


		/* Hack -- Flush output once when no key ready */
		if (!done && (0 != Term_inkey(&kk, FALSE, FALSE)))
		{
			/* Hack -- activate proper term */
			(void)Term_activate(old);

			/* Flush output */
			(void)Term_fresh();

			/* Hack -- activate main screen */
			(void)Term_activate(term_main);

			/* Mega-Hack -- reset saved flag */
			character_saved = FALSE;

			/* Mega-Hack -- reset signal counter */
			signal_count = 0;

			/* Only once */
			done = TRUE;
		}


		/* Hack -- Handle "inkey_base" */
		if (inkey_base)
		{
			int w = 0;

			/* Wait forever */
			if (!inkey_scan)
			{
				/* Wait for (and remove) a pending key */
				if (0 == Term_inkey(&ch, TRUE, TRUE))
				{
					/* Done */
					break;
				}

				/* Oops */
				break;
			}

			/* Wait */
			while (TRUE)
			{
				/* Check for (and remove) a pending key */
				if (0 == Term_inkey(&ch, FALSE, TRUE))
				{
					/* Done */
					break;
				}

				/* No key ready */
				else
				{
					/* Increase "wait" */
					w += 10;

					/* Excessive delay */
					if (w >= 100) break;

					/* Delay */
					pause_for(w);
				}
			}

			/* Done */
			break;
		}


		/* Get a key (see above) */
		ch = inkey_aux();


		/* Handle "control-right-bracket" */
		if (ch == 29)
		{
			/* Strip this key */
			ch = 0;

			/* Continue */
			continue;
		}

#ifdef USE_BACKQUOTE_AS_ESCAPE

		/* HACK -- Treat back-quote as escape */
		if (ch == '`') ch = ESCAPE;

#endif /* USE_BACKQUOTE_AS_ESCAPE */

		/* End "macro trigger" */
		if (parse_under && (ch <= 32))
		{
			/* Strip this key */
			ch = 0;

			/* End "macro trigger" */
			parse_under = FALSE;
		}


		/* Handle "control-caret" */
		if (ch == 30)
		{
			/* Strip this key */
			ch = 0;
		}

		/* Handle "control-underscore" */
		else if (ch == 31)
		{
			/* Strip this key */
			ch = 0;

			/* Begin "macro trigger" */
			parse_under = TRUE;
		}

		/* Inside "macro trigger" */
		else if (parse_under)
		{
			/* Strip this key */
			ch = 0;
		}

		/* Insist upon a key */
		if (!ch) continue;

		/* Otherwise, always accept if not a mouse key */
		if (ch != MOUSEKEY) break;

		/* Option - allow mouse clicks */
		if ((allow_mouse == ALLOW_CLICK) && (cur_mouse_action.button > MOUSE_MOVEONLY)) break;

		/* Option - allow both mouse clicks and movement */
		if (allow_mouse == ALLOW_ALL) break;
	}


	/* Restore cursor state */
	if (reset_cursors)
	{
		for (i = 0; i < TERM_SUBWINDOW; i++)
		{
			bool term_cursor_state;

			/* No term, or term is unavailable */
			if ((!angband_term[i]) || (!angband_term[i]->mapped_flag)) continue;

			/* Activate the term (may not be present) */
			if ((Term_activate(angband_term[i])) && (Term != angband_term[i])) continue;

			/* Get the cursor */
			(void)Term_get_cursor(&term_cursor_state);

			/* If cursor visibility changed, */
			if (cursor_state[i] != term_cursor_state)
			{
				/* Restore the previous cursor state */
				(void)Term_set_cursor(cursor_state[i]);
				(void)Term_fresh_cursor();
			}
		}
	}


	/* Restore the current term */
	(void)Term_activate(old);


	/* Cancel the various "global parameters" */
	inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;

	/* Return the keypress */
	return (ch);
}

/*
 * Add a message to the list, update any sub-windows
 */
void msg_add(cptr msg)
{
	/* Add the message */
	message_add(msg, MSG_GENERIC);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE);
}


/*
 * Flush the screen, make a noise
 */
void bell(cptr reason)
{
	/* Mega-Hack -- Flush the output */
	(void)Term_fresh();

	/* Hack -- memorize the reason if possible */
	if (character_generated && reason)
	{
		message_add(reason, MSG_BELL);

		/* Window stuff */
		p_ptr->window |= (PW_MESSAGE);

		/* Force window redraw */
		window_stuff();
	}

	/* Make a bell noise (if allowed) */
	if (ring_bell) (void)Term_xtra(TERM_XTRA_NOISE, 0);

	/* Flush the input (later!) */
	flush();
}


/*
 * Make a sound
 */
void sound(int val)
{
	/* No sound */
	if (!use_sound) return;

	/* Make a sound (if allowed) */
	(void)Term_xtra(TERM_XTRA_SOUND, val);
}

/*
 * Play (or stop playing) music
 */
void music(int val)
{
	/* Require correct options */
	if ((use_sound != MUSIC_ONLY) && (use_sound != SOUND_AND_MUSIC)) return;

	/* Play music (if allowed) */
	(void)Term_xtra(TERM_XTRA_MUSIC, val);
}



/*
 * The "quark" package
 *
 * This package is used to reduce the memory usage of object inscriptions.
 *
 * We use dynamic string allocation because otherwise it is necessary to
 * pre-guess the amount of quark activity.  We limit the total number of
 * quarks, but this is much easier to "expand" as needed.  XXX XXX XXX
 *
 * Two objects with the same inscription will have the same "quark" index.
 *
 * Some code uses "zero" to indicate the non-existence of a quark.
 *
 * Note that "quark zero" is NULL and should never be "dereferenced".
 *
 * ToDo: Add reference counting for quarks, so that unused quarks can
 * be overwritten.
 *
 * ToDo: Automatically resize the array if necessary.
 */


/*
 * The number of quarks (first quark is NULL)
 */
static s16b quark__num = 1;


/*
 * The array[QUARK_MAX] of pointers to the quarks
 */
static cptr *quark__str;


/*
 * Add a new "quark" to the set of quarks.
 */
s16b quark_add(cptr str)
{
	int i;

	/* Look for an existing quark */
	for (i = 1; i < quark__num; i++)
	{
		/* Check for equality */
		if (streq(quark__str[i], str)) return (i);
	}

	/* Hack -- Require room XXX XXX XXX */
	if (quark__num == QUARK_MAX) return (0);

	/* New quark */
	i = quark__num++;

	/* Add a new quark */
	quark__str[i] = string_make(str);

	/* Return the index */
	return (i);
}


/*
 * This function looks up a quark
 */
cptr quark_str(s16b i)
{
	cptr q;

	/* Verify */
	if ((i < 0) || (i >= quark__num)) i = 0;

	/* Get the quark */
	q = quark__str[i];

	/* Return the quark */
	return (q);
}


/*
 * Initialize the "quark" package
 */
errr quarks_init(void)
{
	/* Quark variables */
	C_MAKE(quark__str, QUARK_MAX, cptr);

	/* Success */
	return (0);
}


/*
 * Free the "quark" package
 */
errr quarks_free(void)
{
	int i;

	/* Free the "quarks" */
	for (i = 1; i < quark__num; i++)
	{
		(void)string_free(quark__str[i]);
	}

	/* Free the list of "quarks" */
	FREE(quark__str);

	/* Success */
	return (0);
}


/*
 * The "message memorization" package.
 *
 * Each call to "message_add(s)" will add a new "most recent" message
 * to the "message recall list", using the contents of the string "s".
 *
 * The number of memorized messages is available as "message_num()".
 *
 * Old messages can be retrieved by "message_str(age)", where the "age"
 * of the most recently memorized message is zero, and the oldest "age"
 * which is available is "message_num() - 1".  Messages outside this
 * range are returned as the empty string.
 *
 * The messages are stored in a special manner that maximizes "efficiency",
 * that is, we attempt to maximize the number of semi-sequential messages
 * that can be retrieved, given a limited amount of storage space, without
 * causing the memorization of new messages or the recall of old messages
 * to be too expensive.
 *
 * We keep a buffer of chars to hold the "text" of the messages, more or
 * less in the order they were memorized, and an array of offsets into that
 * buffer, representing the actual messages, but we allow the "text" to be
 * "shared" by two messages with "similar" ages, as long as we never cause
 * sharing to reach too far back in the the buffer.
 *
 * The implementation is complicated by the fact that both the array of
 * offsets, and the buffer itself, are both treated as "circular arrays"
 * for efficiency purposes, but the strings may not be "broken" across
 * the ends of the array.
 *
 * When we want to memorize a new message, we attempt to "reuse" the buffer
 * space by checking for message duplication within the recent messages.
 *
 * Otherwise, if we need more buffer space, we grab a full quarter of the
 * total buffer space at a time, to keep the reclamation code efficient.
 *
 * The "message_add()" function is rather "complex", because it must be
 * extremely efficient, both in space and time, for use with the Borg.
 */


/*
 * The next "free" index to use
 */
static u16b message__next;

/*
 * The index of the oldest message (none yet)
 */
static u16b message__last;

/*
 * The next "free" offset
 */
static u16b message__head;

/*
 * The offset to the oldest used char (none yet)
 */
static u16b message__tail;

/*
 * The array[MESSAGE_MAX] of offsets, by index
 */
static u16b *message__ptr;

/*
 * The array[MESSAGE_BUF] of chars, by offset
 */
static char *message__buf;

/*
 * The array[MESSAGE_MAX] of u16b for the types of messages
 */
static u16b *message__type;

/*
 * The array[MESSAGE_MAX] of u16b for the count of messages
 */
static u16b *message__count;


/*
 * Table of colors associated with message types
 */
static byte message__color[MSG_MAX];


/*
 * Calculate the index of a message
 */
static s16b message_age2idx(int age)
{
	return ((message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX);
}


/*
 * How many messages are "available"?
 */
s16b message_num(void)
{
	/* Determine how many messages are "available" */
	return (message_age2idx(message__last - 1));
}



/*
 * Recall the "text" of a saved message
 */
cptr message_str(s16b age)
{
	static char buf[1024];
	s16b x;
	u16b o;
	cptr s;

	/* Forgotten messages have no text */
	if ((age < 0) || (age >= message_num())) return ("");

	/* Get the "logical" index */
	x = message_age2idx(age);

	/* Get the "offset" for the message */
	o = message__ptr[x];

	/* Get the message text */
	s = &message__buf[o];

	/* HACK - Handle repeated messages */
	if (message__count[x] > 1)
	{
		(void)strnfmt(buf, sizeof(buf), "%s <%dx>", s, message__count[x]);
		s = buf;
	}

	/* Return the message text */
	return (s);
}


/*
 * Recall the "type" of a saved message
 */
u16b message_type(s16b age)
{
	s16b x;

	/* Paranoia */
	if (!message__type) return (MSG_GENERIC);

	/* Forgotten messages are generic */
	if ((age < 0) || (age >= message_num())) return (MSG_GENERIC);

	/* Get the "logical" index */
	x = message_age2idx(age);

	/* Return the message type */
	return (message__type[x]);
}


/*
 * Recall the "color" of a message type
 */
static byte message_type_color(u16b type)
{
	byte color;

	/* Special messages */
	if (type < MSG_MAX)
	{
		color = message__color[type];
	}
	else
	{
		color = TERM_WHITE;
	}

	if (color == TERM_DARK) color = TERM_L_DARK;
	return (color);
}


/*
 * Recall the "color" of a saved message
 */
byte message_color(s16b age)
{
	return message_type_color(message_type(age));
}


errr message_color_define(u16b type, byte color)
{
	/* Ignore illegal types */
	if (type >= MSG_MAX) return (1);

	/* Store the color */
	message__color[type] = color;

	/* Success */
	return (0);
}


/*
 * Add a new message, with great efficiency
 *
 * We must ignore long messages to prevent internal overflow, since we
 * assume that we can always get enough space by advancing "message__tail"
 * by one quarter the total buffer space.
 *
 * We must not attempt to optimize using a message index or buffer space
 * which is "far away" from the most recent entries, or we will lose a lot
 * of messages when we "expire" the old message index and/or buffer space.
 */
void message_add(cptr str, u16b type)
{
	int k, i, x, o;
	size_t n;

	cptr s;

	cptr u;
	char *v;


	/*** Step 1 -- Analyze the message ***/

	/* Hack -- Ignore "non-messages" */
	if (!str) return;

	/* Message length */
	n = strlen(str);

	/* Hack -- Ignore "long" messages */
	if (n >= MESSAGE_BUF / 4) return;


	/*** Step 2 -- Attempt to optimize ***/

	/* Get the "logical" last index */
	x = message_age2idx(0);

	/* Get the "offset" for the last message */
	o = message__ptr[x];

	/* Get the message text */
	s = &message__buf[o];

	/* Last message repeated? */
	if (streq(str, s))
	{
		/* Increase the message count */
		message__count[x]++;

		/* Success */
		return;
	}

	/*** Step 3 -- Attempt to optimize ***/

	/* Limit number of messages to check */
	k = message_num() / 4;

	/* Limit number of messages to check */
	if (k > 32) k = 32;

	/* Start just after the most recent message */
	i = message__next;

	/* Check the last few messages for duplication */
	for ( ; k; k--)
	{
		u16b q;

		cptr old;

		/* Back up, wrap if needed */
		if (i-- == 0) i = MESSAGE_MAX - 1;

		/* Stop before oldest message */
		if (i == message__last) break;

		/* Index */
		o = message__ptr[i];

		/* Extract "distance" from "head" */
		q = (message__head + MESSAGE_BUF - o) % MESSAGE_BUF;

		/* Do not optimize over large distances */
		if (q >= MESSAGE_BUF / 4) continue;

		/* Get the old string */
		old = &message__buf[o];

		/* Continue if not equal */
		if (!streq(str, old)) continue;

		/* Get the next available message index */
		x = message__next;

		/* Advance 'message__next', wrap if needed */
		if (++message__next == MESSAGE_MAX) message__next = 0;

		/* Kill last message if needed */
		if (message__next == message__last)
		{
			/* Advance 'message__last', wrap if needed */
			if (++message__last == MESSAGE_MAX) message__last = 0;
		}

		/* Assign the starting address */
		message__ptr[x] = message__ptr[i];

		/* Store the message type */
		message__type[x] = type;

		/* Store the message count */
		message__count[x] = 1;

		/* Success */
		return;
	}

	/*** Step 4 -- Ensure space before end of buffer ***/

	/* Kill messages, and wrap, if needed */
	if (message__head + (n + 1) >= MESSAGE_BUF)
	{
		/* Kill all "dead" messages */
		for (i = message__last; TRUE; i++)
		{
			/* Wrap if needed */
			if (i == MESSAGE_MAX) i = 0;

			/* Stop before the new message */
			if (i == message__next) break;

			/* Get offset */
			o = message__ptr[i];

			/* Kill "dead" messages */
			if (o >= message__head)
			{
				/* Track oldest message */
				message__last = i + 1;
			}
		}

		/* Wrap "tail" if needed */
		if (message__tail >= message__head) message__tail = 0;

		/* Start over */
		message__head = 0;
	}


	/*** Step 5 -- Ensure space for actual characters ***/

	/* Kill messages, if needed */
	if (message__head + (n + 1) > message__tail)
	{
		/* Advance to new "tail" location */
		message__tail += (MESSAGE_BUF / 4);

		/* Kill all "dead" messages */
		for (i = message__last; TRUE; i++)
		{
			/* Wrap if needed */
			if (i == MESSAGE_MAX) i = 0;

			/* Stop before the new message */
			if (i == message__next) break;

			/* Get offset */
			o = message__ptr[i];

			/* Kill "dead" messages */
			if ((o >= message__head) && (o < message__tail))
			{
				/* Track oldest message */
				message__last = i + 1;
			}
		}
	}


	/*** Step 6 -- Grab a new message index ***/

	/* Get the next available message index */
	x = message__next;

	/* Advance 'message__next', wrap if needed */
	if (++message__next == MESSAGE_MAX) message__next = 0;

	/* Kill last message if needed */
	if (message__next == message__last)
	{
		/* Advance 'message__last', wrap if needed */
		if (++message__last == MESSAGE_MAX) message__last = 0;
	}


	/*** Step 7 -- Insert the message text ***/

	/* Assign the starting address */
	message__ptr[x] = message__head;

	/* Inline 'strcpy(message__buf + message__head, str)' */
	v = message__buf + message__head;
	for (u = str; *u; ) *v++ = *u++;
	*v = '\0';

	/* Advance the "head" pointer */
	message__head += (n + 1);

	/* Store the message type */
	message__type[x] = type;

	/* Store the message count */
	message__count[x] = 1;
}


/*
 * Initialize the "message" package
 */
errr messages_init(void)
{
	/* Message variables */
	C_MAKE(message__ptr, MESSAGE_MAX, u16b);
	C_MAKE(message__buf, MESSAGE_BUF, char);
	C_MAKE(message__type, MESSAGE_MAX, u16b);
	C_MAKE(message__count, MESSAGE_MAX, u16b);

	/* Init the message colors to white */
	C_BSET(message__color, TERM_WHITE, MSG_MAX, byte);

	/* Hack -- No messages yet */
	message__tail = MESSAGE_BUF;

	/* Success */
	return (0);
}


/*
 * Free the "message" package
 */
void messages_free(void)
{
	/* Free the messages */
	FREE(message__ptr);
	FREE(message__buf);
	FREE(message__type);
	FREE(message__count);
}


/*
 * Hack -- flush
 */
static void msg_flush(int x)
{
	byte attr = TERM_L_BLUE;

	/* Pause for response */
	(void)Term_putstr(x, 0, -1, attr, "(+)");

	/* Get an acceptable keypress */
	if (!message_to_window_active)
	{
		while (TRUE)
		{
			char ch;
			ch = inkey(ALLOW_CLICK);
			if (!p_ptr->playing) break;
			if (quick_messages)
			{
				if (p_ptr->chp >= p_ptr->mhp * op_ptr->hitpoint_warn / 10) break;
			}
			if ((ch == ESCAPE) || (ch == ' ')) break;
			if ((ch == '\n') || (ch == '\r')) break;
			/* bell("Illegal response to a '(+)' prompt!"); */
		}
	}

	/* Clear the line */
	clear_row(0);
}


static int message_column = 0;


/*
 * Output a message to the top line of the screen.
 *
 * Break long messages into multiple pieces (40-72 chars).
 *
 * Allow multiple short messages to "share" the top line.
 *
 * Prompt the user to make sure he has a chance to read them.
 *
 * These messages are memorized for later reference (see above).
 *
 * We could do a "Term_fresh()" to provide "flicker" if needed.
 *
 * The global "msg_flag" variable can be cleared to tell us to "erase" any
 * "pending" messages still on the screen, instead of using "msg_flush()".
 * This should only be done when the user is known to have read the message.
 *
 * We must be very careful about using the "msg_print()" functions without
 * explicitly calling the special "msg_print(NULL)" function, since this may
 * result in the loss of information if the screen is cleared, or if anything
 * is displayed on the top line.
 *
 * Hack -- Note that "msg_print(NULL)" will clear the top line even if no
 * messages are pending.
 */
static void msg_print_aux(u16b type, cptr msg)
{
	int n;
	char *t;
	char buf[1024];
	byte color;
	int w, h;

#ifdef ALLOW_BORG
	/* Hack -- No messages for the borg */
	if (count_stop) return;
#endif

	/* Obtain the size */
	(void)Term_get_size(&w, &h);

	/* Hack -- Reset */
	if (!msg_flag) message_column = 0;

	/* Message Length */
	n = (msg ? strlen(msg) : 0);

	/* Hack -- flush when requested or needed */
	if (message_column && (!msg || ((message_column + n) > (w - 4))))
	{
		/* Flush */
		msg_flush(message_column);

		/* Forget it */
		msg_flag = FALSE;

		/* Reset */
		message_column = 0;
	}


	/* No message */
	if (!msg) return;

	/* Paranoia */
	if (n > 1000) return;


	/* Memorize the message (if legal) */
	if (character_generated && !(p_ptr->is_dead))
		message_add(msg, type);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE);

	/* Copy it */
	(void)my_strcpy(buf, msg, sizeof(buf));

	n++;

	/* Analyze the buffer */
	t = buf;

	/* Get the color of the message */
	color = message_type_color(type);

	/* Split message */
	while (n > (w - 8))
	{
		char oops;

		int check, split;

		/* Default split */
		split = (w - 4);

		/* Find the "best" split point */
		for (check = (w / 2); check < (w - 4); check++)
		{
			/* Found a valid split point */
			if (t[check] == ' ') split = check;
		}

		/* Save the split character */
		oops = t[split];

		/* Split the message */
		t[split] = '\0';

		/* Display part of the message */
		(void)Term_putstr(0, 0, split, color, t);

		/* Flush it */
		msg_flush(split + 1);

		/* Restore the split character */
		t[split] = oops;

		/* Insert a space */
		t[--split] = ' ';

		/* Prepare to recurse on the rest of "buf" */
		t += split; n -= split;
	}

	/* Display the tail of the message */
	(void)Term_putstr(message_column, 0, n, color, t);

	/* Remember the message */
	msg_flag = TRUE;

	/* Remember the position */
	message_column += n + 1;

	/* Optional refresh */
	if (fresh_after) (void)Term_fresh();
}


/*
 * Print a message in the default color (white)
 */
void msg_print(cptr msg)
{
	msg_print_aux(MSG_GENERIC, msg);
}


/*
 * Display a formatted message, using "vstrnfmt()" and "msg_print()".
 */
void msg_format(cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	msg_print_aux(MSG_GENERIC, buf);
}

/*
 * I like to use "msg_format()" for debugging, but have a bad habit of
 * forgetting to delete testing code when done.  Now I can just do
 * a search.  -LM-
 */
void debug(cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	msg_print_aux(MSG_GENERIC, buf);

	/* Flush the message XXX */
	message_flush();
}


/*
 * Display a message and play the associated sound.
 *
 * The "delay" parameter, if non-zero, flushes pending input and pauses the
 * game.  -LM-
 */
void message(u16b type, s16b delay, cptr text)
{
	/* Play a sound, if specified */
	sound(type);

	/* If this is an important message, ... */
	if (delay)
	{
		/* Flush all pending input */
		flush();

		/* Delay the game */
		pause_for(delay);
	}

	/* Display the message */
	msg_print_aux(type, text);
}

/*
 * Display a formatted message and play the associated sound.
 */
void message_format(u16b type, s16b delay, cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, sizeof(buf), fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	message(type, delay, buf);
}


/*
 * Print the queued messages.
 */
void message_flush(void)
{
	/* Hack -- Reset */
	if (!msg_flag) message_column = 0;

	/* Flush when needed */
	if (message_column)
	{
		/* Print pending messages */
		msg_flush(message_column);

		/* Forget it */
		msg_flag = FALSE;

		/* Reset */
		message_column = 0;
	}
}


/*
 * Move the cursor in the current term
 */
void move_cursor(int row, int col)
{
	/* Move the cursor */
	(void)Term_gotoxy(col, row);
}


/*
 * Set cursor term and visibility.
 */
void set_cursor(int term_idx, int show)
{
	term *old = Term;

	/* Ignore illegal requests */
	if ((term_idx < 0) || (term_idx >= TERM_MAX)) return;

	/* Activate the term (may not be present) */
	if ((Term_activate(angband_term[term_idx])) && (Term != angband_term[term_idx])) return;

	/* Show or hide the cursor, if specifically requested */
	if (show >  0) (void)Term_set_cursor(1);
	if (show == 0) (void)Term_set_cursor(0);

	/* Restore the current term */
	(void)Term_activate(old);
}



/*
 * Center a screen of the given width or height in the current term.  We do
 * this by setting a character position offset.
 */
void screen_center_x(int width)
{
	/* If width is 0, cancel centering */
	if (width <= 0)
	{
		Term->offset_x = 0;
		return;
	}

	/* Adjust width if too great */
	if (width > Term->cols) width = Term->cols;

	/* Center the display */
	Term->offset_x = (Term->cols - width) / 2;
}

void screen_center_y(int height)
{
	/* If height is 0, cancel any centering */
	if (height <= 0)
	{
		Term->offset_y = 0;
		return;
	}

	/* Adjust height if too great */
	if (height > Term->rows) height = Term->rows;

	/* Center the display */
	Term->offset_y = (Term->rows - height) / 2;
}


/*
 * Get the effective width of a centered display
 */
int display_width(void)
{
	return (Term->cols - (Term->offset_x * 2));
}


/*
 * Clear the screen
 */
void screen_clear(void)
{
	term *old = Term;

	/* Activate the main term */
	(void)Term_activate(term_main);

	/* Clear the main term */
	(void)Term_clear();

	/* If the map view is active and the map term overlaps the main term, */
	if ((use_special_map) && (!main_screen_inactive))
	{
		/* Activate the map term */
		(void)Term_activate(term_map);

		/* Notify main term of overlay */
		(void)Term_redraw_section_nofresh(0, 0, 999, 999);

		/* Clear the map term */
		(void)Term_clear();
	}

	/* Restore the current term */
	(void)Term_activate(old);
}



/*
 * Save the main screen and deactivate it.
 *
 * This function must match exactly one call to "screen_load()".
 *
 * This function has gotten fairly messy...  :-/
 */
void screen_save(bool clear_screen)
{
	/* Increase the screen depth, only allow one actual save */
	if (++screen_depth != 1) return;


	/* Hack -- Flush messages */
	message_flush();

	/* Make sure the main screen is active */
	(void)Term_activate(term_main);

	/*
	 * Turn on the pop-up window rules (see z-term.h).   XXX XXX XXX
	 */
	if (!clear_screen)
	{
		term_main->popup_hack_flag = TRUE;
		if (use_special_map) term_map->popup_hack_flag = TRUE;
	}

	/* Update the screen (both main and map) */
	(void)Term_fresh();


	/* Save the main screen */
	(void)Term_save();

	/* If we are using a special map display, ... */
	if (use_special_map)
	{
		/* The main term now overlaps the map term */
		(void)Term_overlap(TERM_MAIN, TERM_MAP);

		/* If the main display is active */
		if (!main_screen_inactive)
		{
			/* Activate the map term */
			(void)Term_activate(term_map);

			/* Save it */
			(void)Term_save();

			/* Activate the main term */
			(void)Term_activate(term_main);
		}
	}


	/* Hack! -- No updates for the main screen */
	if (main_screen_inactive == 0) main_screen_inactive = 1;

	/* Option:  Clear the screen */
	if (clear_screen) screen_clear();
}


/*
 * Restore the main screen.
 *
 * This function must match exactly one call to "screen_save()".
 */
void screen_load(void)
{
	/* Decrease the screen depth, only allow one actual load */
	if (--screen_depth != 0) return;

	/* Allow updates for the main screen, unless suppressed */
	if (main_screen_inactive > 0) main_screen_inactive = 0;

	/* Hack -- Flush messages */
	message_flush();

	/* Paranoia -- make sure the main screen is active */
	(void)Term_activate(term_main);

	/* Turn off the pop-up window rules */
	angband_term[TERM_MAIN]->popup_hack_flag = FALSE;
	if (use_special_map) angband_term[TERM_MAP]->popup_hack_flag = FALSE;

	/* Load the main term */
	(void)Term_load();

	/* We are using a special map display */
	if (use_special_map)
	{
		/* If the main screen is not suppressed */
		if (main_screen_inactive >= 0)
		{
			/* The map term now overlaps the main term */
			(void)Term_overlap(TERM_MAP, TERM_MAIN);
		}

		/* The map was previously saved */
		if (term_map->screen_saved)
		{
			/* Activate the map term */
			(void)Term_activate(term_map);

			/* Load it */
			(void)Term_load();

			/* Activate the main term */
			(void)Term_activate(term_main);
		}
	}

	/* Screen refresh (also refreshes map term if on top) */
	(void)Term_fresh();

	/* Hack -- update certain static sub-windows (later) */
	p_ptr->window |= (PW_CMDLIST);
}



/*
 * Toggle the main view (map, side panel, message bar, etc.) on and off.
 *
 * Used when showing displays containing sub-routines that use screen saving and
 * loading (ex. the store interface), or when we don't have a need to save the
 * screen (as on game load).
 */
static void main_view_lock(bool lock)
{
	/* Lock the main view */
	if (lock)
	{
		/* No main display updates ever, until this function is called again */
		main_screen_inactive = -1;

		/* If we are using a separate map term */
		if (use_special_map)
		{
			/* The main term now overlaps the map term */
			(void)Term_overlap(TERM_MAIN, TERM_MAP);
		}
	}

	/* Unlock it */
	else
	{
		/* Hack -- Load the screen if we failed to do so earlier.  XXX XXX */
		if (screen_depth > 0) screen_load();

		/* Allow main display updates */
		main_screen_inactive = 0;

		/* If we are using a separate map term, */
		if (use_special_map)
		{
			/* The map term now overlaps the main term */
			(void)Term_overlap(TERM_MAP, TERM_MAIN);
		}

		/* Update field of view and monster visibility */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Redraw everything */
		p_ptr->redraw = 0xFFFFFFFF;

		/* Refresh everything */
		p_ptr->window = 0xFFFFFFFF;

		/* Update now */
		handle_stuff();
	}
}


/*
 * Start or stop using the tall display.  -LM-
 *
 * What actually happens is system-dependent, but each port finds a way to show
 * 46+ character rows when using the tall display, and 24+ rows when using
 * the normal display.
 */
static errr switch_display(bool display, bool clear_screen)
{
	/* Verify the hook */
	if (!switch_display_hook) return (-1);

	/* Clear all pending messages */
	message_flush();

	/* If a total screen erase is requested, handle it first */
	if (Term->total_erase)
	{
		/* Refresh the screen, cancel screen clear (no need for it now) */
		(void)Term_fresh();
		clear_screen = FALSE;
	}

	/* Toggle the tall display */
	if ((( display) && (!use_tall_display)) ||
	    ((!display) && ( use_tall_display)))
	{
		/* Toggle the option */
		use_tall_display = display;

		/* Clear and refresh the screen */
		if (clear_screen)
		{
			(void)Term_clear();
			(void)Term_fresh();
		}

		/* System-specific reaction */
		return (*switch_display_hook)(display);
	}

	/* Do nothing */
	return (0);
}



/*
 * Data stored between one call to "display_change" and another.
 *
 * We can handle nested remember-restore cycles up to a depth of 8.
 */
static u16b old_display_flags[8];
static int display_old_cx[32] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static int display_old_cy[32] = { 0, 0, 0, 0, 0, 0, 0, 0 };
static int display_memory_level = 0;


/*
 * The remembered display flags
 */
#define FLAG_DSP_WASNORM    0x0001  /* Restore to normal view later */
#define FLAG_DSP_WASTALL    0x0002  /* Restore to tall view later */


/*
 * A general-purpose screen adjustment tool.  -LM-
 *
 * Puts various mid-level complexities associated with adjusting the screen in
 * one place.
 *
 * The main and special views:
 *      When the main view is active, the map is shown and various updates are
 * freely available.  In order to show special views (such as stores or an
 * inventory list or the character screen), this behavior must be suppressed.
 * We can do this in either of two ways:
 * - If you just want to save the screen contents and don't plan to nest
 * displays, use "DSP_SAVE" and "DSP_LOAD".  To create a pop-up window, do this
 * without calling "DSP_CLEAR".  For a full-Term display, call "DSP_CLEAR".
 * Note that you may only save the screen once and must load it before
 * requesting another save.
 * - If you are creating a multi-layered interface that wants to use screen
 * saving and loading internally (like the death interface does), then
 * "DSP_LOCK" will give you secure control of the active Term until you call
 * "DSP_UNLOCK".
 *
 * Displays:
 *      There are three possible configurations of the main Term:  The standard
 * view showing at least 24 rows, the tall view showing at least 46 rows, and
 * the full-screen view (available only on some ports) that uses the total
 * available space to show almost whatever dimensions are requested.
 *      Displays can also be centered vertically and horizontally within the
 * active Term.  You first figure out what panel size you want (80 columns is
 * common; anything greater is a no-no) and then use "DSP_CX" and "DSP_CY" with
 * appropriate widths and heights of the panel to center within the Term.
 *
 * Remembering previous display configurations:
 *      The various possible main term configurations and screen offsets can
 * easily get confusing when layered (as they are in the knowledge code).  To
 * avoid messing up the display, you call "DSP_REMEMBER" to store certain
 * display variables, and then "DSP_RESTORE" to restore them.  These calls can
 * be layered (up to 8 deep); but make certain you call exactly one restore for
 * each remember; when your code re-activates the main view, the variable
 * "display_memory_level" should always be zero.
 *
 * Handling the overlapping map view:
 *     This part is fiddly.  If present, the map term may be on top of most of
 * the main term (this is the standard view) and underneath the main term (for
 * special displays of all sorts, from the opening screen to pop-up windows).
 * It is vital for this code to call the various updates in correct order,
 * and to properly use Term_clear and Term_fresh.
 *
 * Popup Windows:
 *     At present, popup windows still use the main Term, and are requested the
 * same way they have always been:  by calling a "Term_save" and not clearing
 * the screen afterwards.  "DSP_POPUP" and "DSP_POPDOWN" are merely convenient
 * macros.  This may change in future; using these flags is recommended.
 */
void display_change(u32b flags, int wid, int hgt)
{
	int i;

	/* Explicit request to clear the current screen */
	bool do_clear = ((flags & (DSP_CLEAR)) != 0);

	/* Various commands must clear the screen in a prescribed order */
	if (flags & (DSP_FULL | DSP_RESTORE | DSP_LOCK | DSP_UNLOCK)) do_clear = FALSE;

	/* Some commands never clear the screen */
	if (flags & (DSP_POPUP | DSP_POPDOWN)) do_clear = FALSE;

	/* Flush all pending messages (always?)  XXX */
	message_flush();


	/* Remember current display and centering */
	if (flags & (DSP_REMEMBER))
	{
		i = display_memory_level;

		/* Remember current centering */
		display_old_cx[i] = Term->offset_x;
		display_old_cy[i] = Term->offset_y;

		/* Remember current view */
		if (!use_tall_display) old_display_flags[i] |= (FLAG_DSP_WASNORM);
		else                   old_display_flags[i] |= (FLAG_DSP_WASTALL);

		/* Increment memory level (from 0 to 7) */
		display_memory_level = MIN(7, i + 1);
	}

	/* Save the screen (optional clear - also controls pop-up rules) */
	if (flags & (DSP_SAVE | DSP_POPUP)) screen_save(do_clear);

	/* Otherwise, handle separate request to clear the active term */
	else if (do_clear) (void)Term_clear();


	/* Start or stop using the full-screen display */
	if (flags & (DSP_FULL))
	{
		/* The screen must be cleared  XXX */
		screen_clear();

		/* And refreshed */
		(void)Term_fresh();

		/* Activate the display */
		if ((wid > 0) && (hgt > 0))
		{
			/* Use the special display if available */
			if (special_view_hook) special_view_hook(wid, hgt, TRUE);

			/* Offer the normal or large view otherwise */
			else if (hgt <= 24) (void)switch_display(FALSE, FALSE);
			else                (void)switch_display(TRUE, FALSE);
		}

		/* Deactivate the display */
		else
		{
			/* Use the special display if available */
			if (special_view_hook) special_view_hook(0, 0, FALSE);

			/* Restore the previous view otherwise */
			else display_change(DSP_RESTORE | DSP_CLEAR, 0, 0);
		}

		/* Some possible requests have now been taken care of */
		flags &= ~(DSP_NORM | DSP_TALL | DSP_RESTORE);
	}

	/* The special full-screen view cannot switch displays */
	if (use_fullscreen_view) flags &= ~(DSP_NORM | DSP_TALL);

	/* Adjust display size */
	if (flags & (DSP_NORM)) (void)switch_display(FALSE, FALSE);
	if (flags & (DSP_TALL)) (void)switch_display(TRUE, FALSE);

	/* Start or stop screen centering */
	if (flags & (DSP_CX)) screen_center_x(wid);
	if (flags & (DSP_CY)) screen_center_y(hgt);


	/* Restore previous display and centering */
	if (flags & (DSP_RESTORE))
	{
		/* Decrement memory level */
		i = --display_memory_level;

		/* Restore display (default to normal, always clear the screen) */
		if (old_display_flags[i] & (FLAG_DSP_WASTALL))
			(void)switch_display(TRUE, TRUE);
		else
			(void)switch_display(FALSE, TRUE);

		/* Clear */
		old_display_flags[i] = 0;

		/* Restore old centering */
		Term->offset_x = display_old_cx[i];
		Term->offset_y = display_old_cy[i];

		/* Clear */
		display_old_cx[i] = display_old_cy[i] = 0;
	}


	/* Lock the main view */
	if (flags & (DSP_LOCK))
	{
		/* Turn off the main view */
		main_view_lock(TRUE);

		/* Clear the screen */
		screen_clear();
	}

	/* Unlock the main view */
	if (flags & (DSP_UNLOCK))
	{
		/* Clear the term */
		(void)Term_clear();

		/* Turn on the main view */
		main_view_lock(FALSE);
	}


	/* Load the screen (handle after all changes) */
	if ((flags & (DSP_LOAD | DSP_POPDOWN)) && (screen_depth > 0)) screen_load();

	/* Refresh the screen immediately */
	if (flags & (DSP_FRESH)) (void)Term_fresh();
}



/*
 * Clear a line
 */
void clear_row(int row)
{
	/* Erase the line */
	(void)Term_erase(0, row, Term->cols);
}

/*
 * Clear part of the current term
 */
void clear_from(int row)
{
	int y;

	/* Erase requested rows */
	for (y = row; y < Term->rows; y++) clear_row(y);
}

/*
 * Clear part of the current line.
 *
 * Note:  "Term_erase()" now always marks changed areas, even if they had blanks before.
 */
void clear_space(int row, int col, int num)
{
	if (num < 1) return;

	/* Erase the area */
	(void)Term_erase(col, row, num);
}



/*
 * Display a string on the screen using an attribute.
 *
 * At the given location, using the given attribute, if allowed,
 * add the given string.  Do not clear the line.
 */
void c_put_str(byte attr, cptr str, int row, int col)
{
	/* Translate color if necessary */
	if (attr >= max_system_colors) attr = color_table[attr].color_translate;

	/* Position cursor, Dump the attr/text */
	(void)Term_putstr(col, row, -1, attr, str);
}

/*
 * As above, but in "white"
 */
void put_str(cptr str, int row, int col)
{
	/* Spawn */
	(void)Term_putstr(col, row, -1, TERM_WHITE, str);
}



/*
 * Display a string on the screen using an attribute, and clear
 * to the end of the line.
 */
void c_prt(byte attr, cptr str, int row, int col)
{
	/* Clear line, position cursor */
	(void)Term_erase(col, row, 255);

	/* Translate color if necessary */
	if (attr >= max_system_colors) attr = color_table[attr].color_translate;

	/* Dump the attr/text */
	(void)Term_addstr(-1, attr, str);
}


/*
 * As above, but in "white"
 */
void prt(cptr str, int row, int col)
{
	/* Spawn */
	c_prt(TERM_WHITE, str, row, col);
}


/*
 * Add text at the current cursor location
 */
void add_str(cptr str)
{
	(void)Term_addstr(-1, TERM_WHITE, str);
}


/*
 * Center a string.
 */
void center_string(char *buf, size_t buflen, cptr str, int length)
{
	int n1, n2;

	/* Paranoia -- avoid overflow */
	if (strlen(str) > (unsigned int)buflen) return;

	/* Paranoia -- avoid overflow */
	if (length >= (int)buflen) length = buflen - 1;

	/* Length of spacing needed */
	n1 = (length - strlen(str)) / 2;
	n2 = (length - strlen(str) + 1) / 2;

	/* Paranoia -- refuse to add negative spacing */
	if (n1 < 0) n1 = 0;
	if (n2 < 0) n2 = 0;

	/* Pad the string with spacing on both sides */
	(void)strnfmt(buf, length + 1, "%*s%s%*s",
		n1, "",    str,    n2, "");
}


/*
 * Print some (colored) text to the screen at the current cursor position,
 * automatically "wrapping" existing text (at spaces) when necessary to
 * avoid placing any text into the last column, and clearing every line
 * (within the margins) before placing any text in that line.  Also, allow
 * "newline" to force a "wrap" to the next line.  Advance the cursor as
 * needed so sequential calls to this function will work correctly.
 *
 * Once this function has been called, the cursor should not be moved
 * until all the related "text_out()" calls to the window are complete.
 *
 * This function will correctly handle any width up to the maximum legal
 * value of 256, though it works best for a standard 80 character width.
 *
 * This function insists upon maintaining a 1-column right margin.
 */
void text_out_to_screen(byte a, cptr str)
{
	int x, y;

	int wid, h;

	int wrap;

	cptr s;

	/* Translate color if necessary */
	if (a >= max_system_colors) a = color_table[a].color_translate;

	/* Obtain the size */
	(void)Term_get_size(&wid, &h);

	/* Obtain the cursor */
	(void)Term_locate(&x, &y);

	/* Insist upon left border */
	if (x < text_out_indent + text_border_left)
	{
		(void)Term_gotoxy(text_out_indent + text_border_left, y);
		(void)Term_locate(&x, &y);
	}

	/* Use special wrapping boundary? */
	if ((text_out_wrap > 0) && (text_out_wrap < wid))
		wrap = text_out_wrap;
	else
		wrap = wid;

	/* Paranoia -- insist on space to write in */
	if (text_out_indent > wrap) return;


	/* Process the string */
	for (s = str; *s; s++)
	{
		char ch;

		/* Force wrap */
		if (*s == '\n')
		{
			/* Wrap */
			x = text_out_indent + text_border_left;
			y++;

			/* Clear remaining space */
			clear_space(y, x - text_border_left, wrap - x + text_border_left);

			/* Move cursor to next line */
			(void)Term_gotoxy(x, y);

			continue;
		}

		/* Clean up the char */
		ch = (my_isprint((unsigned char)*s) ? *s : ' ');

		/* Wrap words as needed */
		if ((x >= wrap - 1) && (ch != ' ') && (ch != '-'))
		{
			int i, n = text_out_indent;

			byte av[256];
			char cv[256];

			/* Wrap word */
			if (x < wrap)
			{
				/* Scan existing text */
				for (i = wrap - 2; i >= text_out_indent + text_border_left; i--)
				{
					/* Grab existing attr/char */
					(void)Term_what(i, y, &av[i], &cv[i]);

					/* Break on space or hyphen */
					if ((cv[i] == ' ') || (cv[i] == '-')) break;

					/* Track current word */
					n = i;
				}
			}

			/* Special case of a single word taking up the whole line */
			if (n <= text_out_indent + text_border_left) n = wrap;

			/* Otherwise, clear remaining space on this line */
			clear_space(y, n, wrap - n);

			/* Wrap */
			x = text_out_indent + text_border_left;
			y++;

			/* Clear it (plus the left margin) */
			clear_space(y, x - text_border_left, wrap - x + text_border_left);

			/* Move cursor to next line */
			(void)Term_gotoxy(x, y);

			/* Wrap the word (if any) */
			for (i = n; i < wrap - 1; i++)
			{
				/* Dump */
				(void)Term_addch(av[i], cv[i]);

				/* Advance (no wrap) */
				if (++x > wrap) x = wrap;
			}
		}

		/* Dump */
		(void)Term_addch(a, ch);

		/* Advance */
		if (++x > wrap) x = wrap;
	}
}


/*
 * Write text to the given file and apply line-wrapping.
 *
 * Hook function for text_out(). Make sure that text_out_file points
 * to an open text-file.
 *
 * Long lines will be wrapped at text_out_wrap, or at column 75 if that
 * is not set; or at a newline character.  Note that punctuation can
 * sometimes be placed one column beyond the wrap limit.
 *
 * You must be careful to end all file output with a newline character
 * to "flush" the stored line position.
 */
void text_out_to_file(byte a, cptr str)
{
	cptr s;
	char buf[1024];

	/* Current position on the line */
	static int pos = 0;

	/* Wrap width */
	int wrap = (text_out_wrap ? text_out_wrap : 75);

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;


	/* Unused parameter */
	(void)a;

	/* Copy to a rewriteable string */
	(void)my_strcpy(buf, str, 1024);

	/* Translate it to 7-bit ASCII or system-specific format */
	xstr_trans(buf, encoding);

	/* Current location within "buf" */
	s = buf;

	/* Process the string */
	while (*s)
	{
		char ch;
		int n = 0;
		int len = wrap - pos;
		int l_space = -1;

		/* If we are at the start of the line... */
		if (pos == 0)
		{
			int i;

			/* Output the indent */
			for (i = 0; i < text_out_indent; i++)
			{
				fputc(' ', text_out_file);
				pos++;
			}
		}

		/* Find length of line up to next newline or end-of-string */
		while ((n < len) && !((s[n] == '\n') || (s[n] == '\0')))
		{
			/* Mark the most recent space in the string */
			if (s[n] == ' ') l_space = n;

			/* Increment */
			n++;
		}

		/* If we have encountered no spaces */
		if ((l_space == -1) && (n == len))
		{
			/* If we are at the start of a new line */
			if (pos == text_out_indent)
			{
				len = n;
			}
			/* HACK - Output punctuation at the end of the line */
			else if ((s[0] == ' ') || (s[0] == ',') || (s[0] == '.'))
			{
				len = 1;
			}
			else
			{
				/* Begin a new line */
				fputc('\n', text_out_file);

				/* Reset */
				pos = 0;

				continue;
			}
		}
		else
		{
			/* Wrap at the newline */
			if ((s[n] == '\n') || (s[n] == '\0')) len = n;

			/* Wrap at the last space */
			else len = l_space;
		}

		/* Write that line to file */
		for (n = 0; n < len; n++)
		{
			/* Ensure the character is printable */
			ch = (my_isprint((unsigned char)s[n]) ? s[n] : ' ');

			/* Write out the character */
			fputc(ch, text_out_file);

			/* Increment */
			pos++;
		}

		/* Move 's' past the stuff we've written */
		s += len;

		/* If we are at the end of the string, end */
		if (*s == '\0') return;

		/* Skip newlines */
		if (*s == '\n') s++;

		/* Begin a new line */
		fputc('\n', text_out_file);

		/* Reset */
		pos = 0;

		/* Skip whitespace */
		while (*s == ' ') s++;
	}

	/* We are done */
	return;
}


/*
 * Output text to the screen (in color) or to a file depending on the
 * selected hook.
 */
void text_out_c(byte a, cptr str)
{
	text_out_hook(a, str);
}

/*
 * Output text to the screen or to a file depending on the selected
 * text_out hook.
 */
void text_out(cptr str)
{
	text_out_c(TERM_WHITE, str);
}

/*
 * Print some text out to screen.
 */
void c_roff(byte a, cptr str, byte indent, byte wrap)
{
	int y, x;

	/* Set margins */
	text_out_indent = indent;
	text_out_wrap   = wrap;

	/* Obtain the cursor */
	(void)Term_locate(&x, &y);

	/* If cursor is left of the left margin, move it to the left margin */
	if (x < text_out_indent) (void)Term_gotoxy(text_out_indent, y);

	/* Print (colored) text on screen */
	text_out_to_screen(a, str);

	/* Reset "text_out()" vars */
	text_out_wrap   = 0;
	text_out_indent = 0;
}

/*
 * As above, but in white.
 */
void roff(cptr str, byte l_margin, byte r_margin)
{
	/* Spawn */
	c_roff(TERM_WHITE, str, l_margin, r_margin);
}

/*
 * Format a string, with word wrap and centering.  Accept margins.
 * Overwrite (but do not clear) any existing text.
 *
 * We do not print spaces.  This makes the tombstone look better.
 *
 * This function is derived from from the old "c_roff()" function.
 */
void c_roff_centered(byte a, cptr str, int l_margin, int r_margin)
{
	int i, j, x, y;

	int width;
	int term_width, term_height;

	bool wrap  = FALSE;
	bool leave = FALSE;

	cptr s;

	char ch;
	char tmp[256];
	char tmp2[256];


	/* Obtain the screen size */
	(void)Term_get_size(&term_width, &term_height);

	/* Verify left margin */
	if (l_margin < 0)          return;
	if (l_margin > term_width) return;

	/* Verify right margin */
	if (r_margin > term_width) r_margin = term_width;
	if (!r_margin)             r_margin = term_width;

	/* Calculate width of text */
	width = r_margin - l_margin;

	/* Locate the cursor */
	(void)Term_locate(&x, &y);

	/* Move cursor */
	move_cursor(y, l_margin);

	/* Locate the cursor (again) */
	(void)Term_locate(&x, &y);

	/* Translate color if necessary */
	if (a >= max_system_colors) a = color_table[a].color_translate;

	/* Process the string */
	for (s = str; *s; s++)
	{
		/* Stop if we run out of room on the screen */
		if (y >= term_height) return;

		/* Note string location */
		i = x - l_margin;

		/* Clean up the char */
		ch = (my_isprint((unsigned char)*s) ? *s : ' ');

		/* Store this letter */
		tmp[i] = ch;

		/* Force wrap */
		if ((*s == '\r') || (*s == '\n'))
		{
			/* Activate wrap */
			wrap = TRUE;

			tmp[i] = '\0';
		}

		/* Wrap words as needed */
		else if (x >= r_margin - 1)
		{
			/* Activate wrap */
			wrap = TRUE;

			/* Wrap word */
			if (ch != ' ')
			{
				/* Remember the current string position */
				cptr t = s;

				/* Scan existing text */
				for (; i >= 0; --i, --t)
				{
					/* Break on space */
					if (tmp[i] == ' ')
					{
						/* End word */
						tmp[i] = '\0';
						break;
					}
				}

				/* Special case -- word too long to wrap */
				if (i <= 0)
				{
					/* End word */
					tmp[x - l_margin] = '\0';

					/* Repeat character on next line */
					s--;
				}

				/* Point to start of first word on the next line */
				else
				{
					s = t;
				}
			}

			/* End string at space */
			else
			{
				tmp[i] = '\0';
			}
		}

		/* We've reached the end */
		if (*(s + 1) == '\0')
		{
			tmp[i + 1] = '\0';
			wrap  = TRUE;
			leave = TRUE;
		}

		/* Advance to next position */
		x++;

		/* Wrap */
		if (wrap)
		{
			/* Center the text */
			center_string(tmp2, sizeof(tmp2), tmp, width);

			/* Dump the text to screen */
			for (j = 0; tmp2[j] != '\0'; j++)
			{
				/* Dump */
				if (tmp2[j] != ' ') (void)Term_addch(a, tmp2[j]);
				else                move_cursor(y, l_margin + j + 1);
			}

			/* Wrap */
			x = l_margin;
			y++;

			/* Move cursor to next line */
			move_cursor(y, x);

			/* Done */
			if (leave) return;

			/* Toggle off wrap */
			wrap = FALSE;
		}
	}
}


/*
 * Unify the use of direction keys and mouse actions in game interfaces.  -LM-
 *
 * Handle usage of the shift key combined with either roguelike movement keys
 * or the number pad/arrow keys.  The first is easy; the less said about the
 * second, the better.
 *
 * Allow mouse actions of certain types, if supported by the interface.
 */
void get_ui_direction(char *k, byte flags, bool *shift_key)
{
	bool allow_roguelike = !(flags & (UI_NOROGUE));
	bool allow_numbers   = !(flags & (UI_NONUM));
	bool allow_diagonal  = !(flags & (UI_NODIAG));
	bool allow_mouse     = !(flags & (UI_NOMOUSE));


	/* Assume shift is not pressed (or at least not recognized) */
	*shift_key = FALSE;

	/* Handle roguelike keys */
	if ((allow_roguelike) && (strchr("bjnhlykuBJNHLYKU", *k)))
	{
		/* Option -- ignore diagonals */
		if ((!allow_diagonal) && (strchr("bnyuBNYU", *k))) return;

		/* Notice shift-movement */
		if (my_isupper(*k)) *shift_key = TRUE;

		/* Convert roguelike keys */
		if      ((*k == 'B') || (*k == 'b')) *k = '1';
		else if ((*k == 'J') || (*k == 'j')) *k = '2';
		else if ((*k == 'N') || (*k == 'n')) *k = '3';
		else if ((*k == 'H') || (*k == 'h')) *k = '4';
		else if ((*k == 'L') || (*k == 'l')) *k = '6';
		else if ((*k == 'Y') || (*k == 'y')) *k = '7';
		else if ((*k == 'K') || (*k == 'k')) *k = '8';
		else if ((*k == 'U') || (*k == 'u')) *k = '9';
	}

	/* Handle number keys */
	else if ((allow_numbers) && (isdigit(*k)))
	{
		/* No conversion needed */
	}

	/* Handle mouse actions */
	else if ((*k == MOUSEKEY) && (allow_mouse))
	{
		/* Handle the mouse wheel */
		if (cur_mouse_action.button == MOUSE_WHEEL)
		{
			/* Move up or down quickly */
			if      (cur_mouse_action.y > 100) *k = '2';
			else if (cur_mouse_action.y ==  0) *k = '8';
			else return;

			*shift_key = TRUE;
		}
		else
		{
			/* Do nothing */
			return;
		}
	}

	/* Handle special keys  XXX XXX XXX */
	else if (*k == '\007')
	{
		char ch;

		/* Hack!  -- look for numbers in the pending keystrokes */
		while (!Term_inkey(&ch, FALSE, TRUE))
		{
			/* If we find one, store it and stop looking */
			if (isdigit(ch))
			{
				*k = ch;
				*shift_key = TRUE;
				break;
			}

			/* If we hit the buffer, stop looking */
			if (ch == 30) break;
		}
	}
}


/*
 * Determine whether the digit just typed was created by actually typing a
 * number, or whether a direction or arrow key was in fact used.
 *
 * We do this by assuming that a digit is produced by a macro if a direction
 * key was typed.  This is a nasty hack.
 */
static bool is_direction(char ch)
{
	if (isdigit(ch))
	{
		char c;

		if (!Term_inkey(&c, FALSE, FALSE))
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Get some string input at the cursor location.
 * Assume the buffer is initialized to a default string.
 *
 * The default buffer is in Overwrite mode and displayed in yellow at
 * first.  Normal chars clear the yellow text and append the char in
 * white text.
 *
 * LEFT (^B) and RIGHT (^F) movement keys move the cursor position.
 * If the text is still displayed in yellow (Overwrite mode), it will
 * turn into white (Insert mode) when the cursor moves.
 *
 * DELETE (^D) deletes a char at the cursor position.
 * BACKSPACE (^H) deletes a char at the left of cursor position.
 * ESCAPE clears the buffer and the window and returns FALSE.
 * RETURN accepts the current buffer contents and returns TRUE.
 */
bool askfor_aux(char *buf, int len, bool numpad_cursor)
{
	int y, x;
	s16b d = 0;
	char ch;

	/* Indicate replacement mode by displaying in yellow */
	byte color = TERM_YELLOW;

	/* Locate the cursor */
	(void)Term_locate(&x, &y);

	/* Paranoia -- check len */
	if (len < 1) len = 1;

	/* Paranoia -- check column */
	if ((x < 0) || (x >= Term->cols)) x = 0;

	/* Restrict the length */
	if (x + len > Term->cols) len = Term->cols - x;

	/* Truncate the default entry */
	buf[len-1] = '\0';


	/* Process input */
	while (TRUE)
	{
		/* Display the string */
		(void)Term_erase(x, y, len);
		(void)Term_putstr(x, y, -1, color, buf);

		/* Place cursor */
		(void)Term_gotoxy(x + d, y);

		/* Get a key */
		ch = inkey(FALSE);

		/* Notice direction key */
		if ((numpad_cursor) && (is_direction(ch)))
		{
			if      (ch == '4') ch = '\002';
			else if (ch == '6') ch = '\006';
		}

		/* Analyze the key */
		switch (ch)
		{
			case '?':
			{
				/* Hack -- Get help, if specified  XXX XXX */
				if (p_ptr->get_help_index != HELP_GENERAL)
				{
					do_cmd_help();
				}
				break;
			}

			/* Move cursor backwards */
			case '\002':
			{
				/* Now on insert mode */
				color = TERM_WHITE;

				/* Go to previous position */
				if (d > 0) d--;

				break;
			}

			/* Move cursor forwards */
			case '\006':
			{
				/* Now on insert mode */
				color = TERM_WHITE;

				/* Advance; do not pass the end of the string */
				if (buf[d] && (d < len - 1)) d++;

				break;
			}

			/* Cancel */
			case ESCAPE:
			{
				/* Clear input */
				buf[0] = '\0';

				return (FALSE);
			}

			/* Accept */
			case '\n':
			case '\r':
			{
				/* Translate it to 8-bit (Latin-1) */
				xstr_trans(buf, LATIN1);

				return (TRUE);
			}

			/* Backspace */
			case '\010':
			{
				/* Now on insert mode */
				color = TERM_WHITE;

				/* Go to previous position */
				if (d > 0L) d--;

				/* Fall through to 'Delete key' */
			}

			/* Delete */
			case 0x7F:
			case KTRL('d'):
			{
				int dst, src;

				/* Now on insert mode */
				color = TERM_WHITE;

				/* No move at end of line */
				if (!buf[d]) break;

				/* Position of next character */
				src = d + 1;

				/* Position of this character */
				dst = d;

				/* Move characters at src to dst */
				while ('\0' != (buf[dst++] = buf[src++]))
					/* loop */;

				break;
			}

			/* All other characters */
			default:
			{
				char tmp[1024];

				/* We are in replacement mode */
				if (color == TERM_YELLOW)
				{
					/* Overwrite default string */
					buf[0] = '\0';

					/* Go to insert mode */
					color = TERM_WHITE;
				}

				/* Save right part of string */
				strcpy(tmp, buf + d);

				/* Add printable character if space permits */
				if (d < len && my_isprint(ch))
				{
					buf[d++] = ch;
				}
				else
				{
					bell("Illegal edit key!");
				}

				/* Terminate */
				buf[d] = '\0';

				/* Write back the left part of string */
				(void)my_strcat(buf, tmp, len + 1);

				/* Retreat if end of space reached */
				if (d >= len) d--;

				break;
			}
		}
	}

	return (FALSE);
}



/*
 * Prompt for a string from the user.
 *
 * The "prompt" should take the form "Prompt:".
 *
 * See "askfor_aux" for some notes about "buf" and "len", and about
 * the return value of this function.
 */
bool get_string(cptr prompt, char *buf, size_t len)
{
	bool res;
	int y, x;

	/* Paranoia XXX XXX XXX */
	message_flush();

	/* Locate the cursor */
	(void)Term_locate(&x, &y);

	/* Display prompt */
	prt(format("%s ", prompt), y, x);

	/* Ask the user for a string, allow direction keys to move cursor */
	res = askfor_aux(buf, len, TRUE);

	/* Translate it to 8-bit (Latin-1) */
	xstr_trans(buf, LATIN1);

	/* Clear prompt */
	prt("", y, x);

	/* Result */
	return (res);
}

/*
 * Request a "quantity" from the user
 *
 * Allow "p_ptr->command_arg" to specify a quantity
 *
 * Allow "get_quantity_default" to suggest a default quantity
 */
s32b get_quantity(cptr prompt, s32b min, s32b max)
{
	/* Default quantity must be between min and max, inclusive */
	s32b amt = get_quantity_default;
	int amt_int = (int)amt;

	if (amt < min) amt = min;
	if (amt > max) amt = max;


	/* Use "command_arg" */
	if (p_ptr->command_arg)
	{
		/* Extract a number */
		amt = (s32b)p_ptr->command_arg;

		/* Clear "command_arg" */
		p_ptr->command_arg = 0;
	}

	/* Get the item index (using an integer version of our input) */
	else if ((max != 1L) && allow_quantity && repeat_pull(&amt_int))
	{
		amt = (s32b)amt_int;
	}

	/* Prompt if needed */
	else if ((max != 1L) && allow_quantity)
	{
		char tmp[DESC_LEN];

		char buf[DESC_LEN];

		/* Build a prompt if needed */
		if (!prompt)
		{
			/* Build a prompt */
			(void)strnfmt(tmp, sizeof(tmp), "Quantity (0-%ld):", max);

			/* Use that prompt */
			prompt = tmp;
		}

		/* Build the default */
		(void)strnfmt(buf, sizeof(buf), "%ld", amt);

		/* Ask for a quantity */
		if (!get_string(prompt, buf, 9)) return (0);

		/* Extract a number */
		amt = (s32b)atoi(buf);

		/* A letter means "all" */
		if (isalpha((unsigned char)buf[0])) amt = max;
	}

	/* Enforce the maximum */
	if (amt > max) amt = max;

	/* Enforce the minimum */
	if (amt < min) amt = min;

	/* Save this command */
	if (amt) repeat_push((s16b)amt);

	/* Clear suggested quantity */
	get_quantity_default = 1L;

	/* Return the result */
	return (amt);
}


/*
 * Prompts user with yes/no prompt
 *
 * Entry from get_check and get_check_default
 *
 * mode = 1 default y/n prompt
 * mode = 2 Y/n prompt, defaulting to yes
 * mode = 3 y/N prompt, defaulting to no
 */
bool aux_get_check(cptr prompt, int mode)
{
	char ch;

	char buf[DESC_LEN];

	/* Paranoia XXX XXX XXX */
	message_flush();

#ifdef ALLOW_BORG
	/* The dumbborg never commits to anything */
	if (count_stop) return (FALSE);
#endif

	/* Hack -- Build a "useful" prompt */
	if 		(mode == 2)	(void)strnfmt(buf, 78, "%.70s [Y/n] ", prompt);
	else if	(mode == 3)	(void)strnfmt(buf, 78, "%.70s [y/N] ", prompt);
	else				(void)strnfmt(buf, 78, "%.70s [y/n] ", prompt);

	/* Get an acceptable answer */
	while (TRUE)
	{
		/* Prompt for it */
		prt(buf, 0, 0);

		/* Get a response and process it */
		ch = inkey(FALSE);
		if (ch == ESCAPE) break;
		if (strchr("YyNn", ch)) break;
		if (strchr("\r", ch) && mode > 1) break;  /* Handle default prompts when useful */

		/* Handle errors  XXX XXX (this breaks macros) */
		/* bell("Illegal response to a 'yes/no' question.");*/
	}

	/* Erase the prompt */
	prt("", 0, 0);

	/* Remember the question and the answer  -LM- */
	if (character_generated)
	{
		msg_add(format("%s  %c", prompt, ch));
	}

	/* Hack -- set character to 'y' for default prompt */
	if (mode == 2 && strchr("\r", ch)) ch = 'y';

	/* Normal negation */
	if ((ch != 'Y') && (ch != 'y'))
	{
		/* Note cancelled  XXX */
		prt("Cancelled.", 0, 0);

		return (FALSE);
	}

	/* Success */
	return (TRUE);
}

/*
 * Verify something with the user
 *
 * The "prompt" should take the form "<question>?"
 *
 * Note that "[y/n]" is appended to the prompt.
 */
bool get_check(cptr prompt)
{
	return aux_get_check(prompt, 1);
}

bool get_check_default(cptr prompt, bool yes)
{
	if (yes) 	return aux_get_check(prompt, 2);
	else 		return aux_get_check(prompt, 3);
}


/*
 * Prompts for a keypress
 *
 * The "prompt" should take the form "Command:"
 * (a space is automatically added)
 *
 * Returns TRUE unless the character is "Escape"
 */
bool get_com(cptr prompt, char *command)
{
	char ch;

	/* Paranoia XXX XXX XXX (we lose inkey flag info here) */
	message_flush();

	/* Display a prompt */
	prt(format("%s ", prompt), 0, 0);

	/* Get a key */
	ch = inkey(ALLOW_CLICK);

	/* Clear the prompt */
	prt("", 0, 0);

	/* Save the command */
	*command = ch;

	/* Done */
	return (ch != ESCAPE);
}


/*
 * Pause for user response (hide cursor)
 *
 * This function is stupid.  XXX XXX (but oddly useful...)
 */
void pause_line(int row)
{
	/* Center in the space between margins (assumes centered text XXX) */
	int wid = Term->cols - Term->offset_x * 2;

	/* Clear line */
	prt("", row, 0);

	/* The exit sign (centered) */
	put_str("[Press any key to continue]", row, (wid - 27) / 2);

	/* Hide the cursor (main and map terms) */
	inkey_cursor_hack[TERM_MAIN] = -1;
	inkey_cursor_hack[TERM_MAP] = -1;

	/* Wait for it */
	(void)inkey(ALLOW_CLICK);

	/* Apply standard cursor rules */
	inkey_cursor_hack[TERM_MAIN] = 0;
	inkey_cursor_hack[TERM_MAP] = 0;

	/* Clear the prompt */
	prt("", row, 0);
}


/*
 * Given a character, return an index.
 *
 * The reason why we sometimes forbid uppercase indexes is to allow users
 * of the roguelike keyset to both use their usual navigation keys to move
 * around and use (uppercase) letters to jump to specific indexes.
 */
int get_index(char ch, bool require_lower)
{
	int i;

	/* We do not require lowercase indexes */
	if (!require_lower)
	{
		/* Scan the table "index_chars" */
		for (i = 0; index_chars[i]; i++)
		{
			/* We found the character */
			if (index_chars[i] == ch) return (i);
		}
	}

	/* We cannot have any lowercase indexes */
	else
	{
		/* Change to lower case if necessary */
		ch = my_tolower((unsigned char)ch);

		/* Scan the table "index_chars_lower" */
		for (i = 0; index_chars_lower[i]; i++)
		{
			/* We found the character */
			if (index_chars_lower[i] == ch) return (i);
		}
	}

	/* Note failure */
	return (-1);
}



/*
 * Hack -- special buffer to hold the action of the current keymap
 */
static char request_command_buffer[256];


/*
 * Request a command from the user.
 *
 * Sets p_ptr->command_cmd, p_ptr->command_dir, p_ptr->command_rep,
 * p_ptr->command_arg.  May modify p_ptr->command_new.
 *
 * Note that "caret" ("^") is treated specially, and is used to
 * allow manual input of control characters.  This can be used
 * on many machines to request repeated tunneling (Ctrl-H) and
 * on the Macintosh to request "Control-Caret".
 *
 * Note that "backslash" is treated specially, and is used to bypass any
 * keymap entry for the following character.  This is useful for macros.
 *
 * Note that this command is used both in the dungeon and in
 * stores, and must be careful to work in both situations.
 *
 * "p_ptr->command_new" does work.
 */
void request_command(bool shopping)
{
	int i;

	char ch;

	int mode;

	cptr act;


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

	/* No command yet */
	p_ptr->command_cmd = 0;

	/* No "argument" yet */
	p_ptr->command_arg = 0;

	/* No "direction" yet */
	p_ptr->command_dir = 0;

	/* No keymap */
	p_ptr->using_keymap = FALSE;


	/* Get command */
	while (TRUE)
	{
		bool skip_keymap = FALSE;

		/* Hack -- auto-commands */
		if (p_ptr->command_new)
		{
			/* Flush messages */
			message_flush();

			/* Use auto-command */
			ch = (char)p_ptr->command_new;

			/* Forget it */
			p_ptr->command_new = 0;
		}

		/* Get a keypress in "command" mode */
		else
		{
			/* Hack -- no flush needed */
			msg_flag = FALSE;

			/* Activate "command mode" */
			inkey_flag = TRUE;

			/* Get a command */
			ch = inkey(ALLOW_CLICK);
		}

		/* Clear top line */
		prt("", 0, 0);


		/* Command Count */
		if (ch == '0')
		{
			int old_arg = p_ptr->command_arg;

			/* Reset */
			p_ptr->command_arg = 0;

			/* Begin the input */
			prt("Count: ", 0, 0);

			/* Get a command count */
			while (TRUE)
			{
				/* Get a new keypress */
				ch = inkey(FALSE);

				/* Simple editing (delete or backspace) */
				if ((ch == 0x7F) || (ch == KTRL('H')))
				{
					/* Delete a digit */
					p_ptr->command_arg = p_ptr->command_arg / 10;

					/* Show current count */
					prt(format("Count: %d", p_ptr->command_arg), 0, 0);
				}

				/* Actual numeric data */
				else if (isdigit((unsigned char)ch))
				{
					/* Stop count at 9999 */
					if (p_ptr->command_arg >= 1000)
					{
						/* Warn */
						bell("Invalid repeat count!");

						/* Limit */
						p_ptr->command_arg = 9999;
					}

					/* Increase count */
					else
					{
						/* Incorporate that digit */
						p_ptr->command_arg = p_ptr->command_arg * 10 + D2I(ch);
					}

					/* Show current count */
					prt(format("Count: %d", p_ptr->command_arg), 0, 0);
				}

				/* Exit on "unusable" input */
				else
				{
					break;
				}
			}

			/* Hack -- Handle "zero" */
			if (p_ptr->command_arg == 0)
			{
				/* Default to 99 */
				p_ptr->command_arg = 99;

				/* Show current count */
				prt(format("Count: %d", p_ptr->command_arg), 0, 0);
			}

			/* Hack -- Handle "old_arg" */
			if (old_arg != 0)
			{
				/* Restore old_arg */
				p_ptr->command_arg = old_arg;

				/* Show current count */
				prt(format("Count: %d", p_ptr->command_arg), 0, 0);
			}

			/* Hack -- white-space means "enter command now" */
			if ((ch == ' ') || (ch == '\n') || (ch == '\r'))
			{
				/* Get a real command */
				if (!get_com("Command:", &ch))
				{
					/* Clear count */
					p_ptr->command_arg = 0;

					/* Continue */
					continue;
				}
			}
		}


		/* Allow "keymaps" to be bypassed */
		if (ch == '\\')
		{
			/* Get a real command */
			(void)get_com("Command:", &ch);

			/* Hack -- bypass keymaps */
			skip_keymap = TRUE;

			/*
			 * -TNB-
			 * XXX Can't set inkey_next, because inkey() may get
			 * called to get a control char, which clears inkey_next!
			 */
		}


		/* Allow "control chars" to be entered */
		if (ch == '^')
		{
			/* Get a new command and controlify it */
			if (get_com("Control:", &ch)) ch = KTRL(ch);
		}


		/* Look up applicable keymap */
		act = keymap_act[mode][(byte)(ch)];

		/* Apply keymap if not inside a keymap already */
		if (act && !skip_keymap && !inkey_next)
		{
			/* Install the keymap (limited buffer size) */
			(void)strnfmt(request_command_buffer, 256, "%s", act);

			/* Start using the buffer */
			inkey_next = request_command_buffer;

			/* Using a keymap */
			p_ptr->using_keymap = TRUE;

			/* Continue */
			continue;
		}


		/* Paranoia */
		if (ch == '\0') continue;


		/* Use command */
		p_ptr->command_cmd = ch;

		/* Done */
		break;
	}

	/* Hack -- Auto-repeat certain commands */
	if (p_ptr->command_arg <= 0)
	{
		/* Hack -- auto repeat certain commands */
		if (strchr(AUTO_REPEAT_COMMANDS, p_ptr->command_cmd))
		{
			/* Repeat 99 times */
			p_ptr->command_arg = 99;
		}
	}


	/* Shopping */
	if (shopping)
	{
		/* Hack -- Convert a few special keys */
		switch (p_ptr->command_cmd)
		{
			/* Command "p" -> "purchase" (get) */
			case 'p': p_ptr->command_cmd = 'g'; break;

			/* Command "m" -> "purchase" (get) */
			case 'm': p_ptr->command_cmd = 'g'; break;

			/* Command "s" -> "sell" (drop) */
			case 's': p_ptr->command_cmd = 'd'; break;
		}
	}


	/* Hack -- Scan equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		cptr s;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* No inscription */
		if (!o_ptr->note) continue;

		/* Find a '^' */
		s = strchr(quark_str(o_ptr->note), '^');

		/* Process preventions */
		while (s)
		{
			/* Check the "restriction" character */
			if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
			{
				/* Hack -- Verify command */
				if (!get_check("Are you sure?"))
				{
					/* Hack -- Use "newline" */
					p_ptr->command_cmd = '\n';
				}
			}

			/* Find another '^' */
			s = strchr(s + 1, '^');
		}
	}


	/* Hack -- erase the message line. */
	prt("", 0, 0);
}


/*
 * Replace the first instance of "target" in "buf" with "insert"
 * If "insert" is NULL, just remove the first instance of "target"
 * In either case, return TRUE if "target" is found.
 *
 * Could be made more efficient, especially in the case where
 * "insert" is smaller than "target".
 */
bool insert_str(char *buf, cptr target, cptr insert)
{
	int i, len;
	int b_len, t_len, i_len;

	/* Attempt to find the target (modify "buf") */
	buf = strstr(buf, target);

	/* No target found */
	if (!buf) return (FALSE);

	/* Be sure we have an insertion string */
	if (!insert) insert = "";

	/* Extract some lengths */
	t_len = strlen(target);
	i_len = strlen(insert);
	b_len = strlen(buf);

	/* How much "movement" do we need? */
	len = i_len - t_len;

	/* We need less space (for insert) */
	if (len < 0)
	{
		for (i = t_len; i < b_len; ++i) buf[i + len] = buf[i];
	}

	/* We need more space (for insert) */
	else if (len > 0)
	{
		for (i = b_len - 1; i >= t_len; --i) buf[i + len] = buf[i];
	}

	/* If movement occurred, we need a new terminator */
	if (len) buf[b_len + len] = '\0';

	/* Now copy the insertion string */
	for (i = 0; i < i_len; ++i) buf[i] = insert[i];

	/* Successful operation */
	return (TRUE);
}



/*
 * Code to handle repeated commands.  -TNB-
 */

#define REPEAT_MAX 20

/* Number of chars saved */
static int repeat__cnt = 0;

/* Current index */
static int repeat__idx = 0;

/* Saved "stuff" */
static int repeat__key[REPEAT_MAX];


/*
 * Push data.
 */
void repeat_push(int what)
{
	/* Too many keys */
	if (repeat__cnt == REPEAT_MAX) return;

	/* Push the "stuff" */
	repeat__key[repeat__cnt++] = what;

	/* Prevents us from pulling keys */
	++repeat__idx;
}


/*
 * Pull data.
 */
bool repeat_pull(int *what)
{
	/* All out of keys */
	if (repeat__idx == repeat__cnt) return (FALSE);

	/* Grab the next key, advance */
	*what = repeat__key[repeat__idx++];

	/* Success */
	return (TRUE);
}

/*
 * Clear a repeat.
 */
void repeat_clear(void)
{
	/* Start over from the failed pull */
	if (repeat__idx)
		repeat__cnt = --repeat__idx;
	/* Paranoia */
	else
		repeat__cnt = repeat__idx;

	return;
}

/*
 * Repeat previous command, or begin memorizing new command.
 */
void repeat_check(void)
{
	int what;

	/* Ignore some commands */
	if (p_ptr->command_cmd == ESCAPE) return;
	if (p_ptr->command_cmd == ' ') return;
	if (p_ptr->command_cmd == '\n') return;
	if (p_ptr->command_cmd == '\r') return;
	if (p_ptr->command_cmd == MOUSEKEY) return;

	/* Repeat Last Command */
	if (p_ptr->command_cmd == KTRL('V'))
	{
		/* Reset */
		repeat__idx = 0;

		/* Get the command */
		if (repeat_pull(&what))
		{
			/* Save the command */
			p_ptr->command_cmd = what;
		}
	}

	/* Start saving new command */
	else
	{
		/* Reset */
		repeat__cnt = 0;
		repeat__idx = 0;

		/* Get the current command */
		what = p_ptr->command_cmd;

		/* Save this command */
		repeat_push(what);
	}
}


#ifdef SUPPORT_GAMMA


/*
 * XXX XXX XXX Important note about "colors" XXX XXX XXX
 *
 * The "TERM_*" color definitions list the "composition" of each
 * "Angband color" in terms of "quarters" of each of the three color
 * components (Red, Green, Blue), for example, TERM_UMBER is defined
 * as 2/4 Red, 1/4 Green, 0/4 Blue.
 *
 * The following info is from "Torbjorn Lindgren" (see "main-xaw.c").
 *
 * These values are NOT gamma-corrected.  On most machines (with the
 * Macintosh being an important exception), you must "gamma-correct"
 * the given values, that is, "correct for the intrinsic non-linearity
 * of the phosphor", by converting the given intensity levels based
 * on the "gamma" of the target screen, which is usually 1.7 (or 1.5).
 *
 * The actual formula for conversion is unknown to me at this time,
 * but you can use the table below for the most common gamma values.
 *
 * So, on most machines, simply convert the values based on the "gamma"
 * of the target screen, which is usually in the range 1.5 to 1.7, and
 * usually is closest to 1.7.  The converted value for each of the five
 * different "quarter" values is given below:
 *
 *  Given     Gamma 1.0       Gamma 1.5       Gamma 1.7     Hex 1.7
 *  -----       ----            ----            ----          ---
 *   0/4        0.00            0.00            0.00          #00
 *   1/4        0.25            0.27            0.28          #47
 *   2/4        0.50            0.55            0.56          #8f
 *   3/4        0.75            0.82            0.84          #d7
 *   4/4        1.00            1.00            1.00          #ff
 *
 * Note that some machines (i.e. most IBM x286 machines) are limited to a
 * hard-coded set of colors, and so for them the information above is useless.
 */




/* Table of gamma values */
byte gamma_table[256];

/* Table of ln(x / 256) * 256 for x going from 0 -> 255 */
static const s16b gamma_helper[256] =
{
	0, -1420, -1242, -1138, -1065, -1007, -961, -921, -887, -857, -830,
	-806, -783, -762, -744, -726, -710, -694, -679, -666, -652, -640,
	-628, -617, -606, -596, -586, -576, -567, -577, -549, -541, -532,
	-525, -517, -509, -502, -495, -488, -482, -475, -469, -463, -457,
	-451, -455, -439, -434, -429, -423, -418, -413, -408, -403, -398,
	-394, -389, -385, -380, -376, -371, -367, -363, -359, -355, -351,
	-347, -343, -339, -336, -332, -328, -325, -321, -318, -314, -311,
	-308, -304, -301, -298, -295, -291, -288, -285, -282, -279, -276,
	-273, -271, -268, -265, -262, -259, -257, -254, -251, -248, -246,
	-243, -241, -238, -236, -233, -231, -228, -226, -223, -221, -219,
	-216, -214, -212, -209, -207, -205, -203, -200, -198, -196, -194,
	-192, -190, -188, -186, -184, -182, -180, -178, -176, -174, -172,
	-170, -168, -166, -164, -162, -160, -158, -156, -155, -153, -151,
	-149, -147, -146, -144, -142, -140, -139, -137, -135, -134, -132,
	-130, -128, -127, -125, -124, -122, -120, -119, -117, -116, -114,
	-112, -111, -109, -108, -106, -105, -103, -102, -100, -99, -97, -96,
	-95, -93, -92, -90, -89, -87, -86, -85, -83, -82, -80, -79, -78,
	-76, -75, -74, -72, -71, -70, -68, -67, -66, -65, -63, -62, -61,
	-59, -58, -57, -56, -54, -53, -52, -51, -50, -48, -47, -46, -45,
	-44, -42, -41, -40, -39, -38, -37, -35, -34, -33, -32, -31, -30,
	-29, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16,
	-14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1
};


/*
 * Build the gamma table so that floating point isn't needed.
 *
 * Note gamma goes from 0->256.  The old value of 100 is now 128.
 */
void build_gamma_table(int gamma)
{
	int i, n;

	/*
	 * value is the current sum.
	 * diff is the new term to add to the series.
	 */
	long value, diff;

	/* Hack - convergence is bad in these cases. */
	gamma_table[0] = 0;
	gamma_table[255] = 255;

	/*
	 * Initialize the Taylor series
	 *
	 * value and diff have been scaled by 256
	 */
	for (i = 1; i < 255; i++)
	{

		n = 1;
		value = 256L * 256L;
		diff = ((long)gamma_helper[i]) * (gamma - 256);

		while (diff)
		{
			value += diff;
			n++;

			/*
			 * Use the following identity to calculate the gamma table.
			 * exp(x) = 1 + x + x^2/2 + x^3/(2*3) + x^4/(2*3*4) +...
			 *
			 * n is the current term number.
			 *
			 * The gamma_helper array contains a table of
			 * ln(x/256) * 256
			 * This is used because a^b = exp(b*ln(a))  *
			 * In this case:
			 * a is i / 256
			 * b is gamma.
			 *
			 * Note that everything is scaled by 256 for accuracy,
			 * plus another factor of 256 for the final result to
			 * be from 0-255.  Thus gamma_helper[] * gamma must be
			 * divided by 256*256 each iteration, to get back to
			 * the original power series.
			 */
			diff = (((diff / 256) * gamma_helper[i]) * (gamma - 256)) / (256 * n);
		}

		/*
		 * Store the value in the table so that the
		 * floating point pow function isn't needed.
		 */
		gamma_table[i] = ((long)(value / 256) * i) / 256;
	}
}

#endif /* SUPPORT_GAMMA */


/*
 * Accept a color index character; if legal, return the color.  -LM-
 *
 * We have to maintain 16-color compatability in order to run on VGA
 * monitors.  Therefore, all colors defined above with indexes over 15
 * must be translated if the system only allows 16 colors.
 */
int color_char_to_attr(char c)
{
	int a;

	/* Is negative -- spit it right back out */
	if (c < 0) return (c);

	/* Is a space or '\0' -- return black */
	if (c == '\0' || c == ' ') return (TERM_DARK);

	/* Search the color table */
	for (a = 0; a < MAX_COLORS; a++)
	{
		/* Look for the index */
		if (color_table[a].index_char == c) break;
	}

	/* If we don't find the color, we assume white */
	if (a == MAX_COLORS) return (TERM_WHITE);

	/* System cannot display this color */
	if (a >= max_system_colors)
	{
		/* Translate to 16-color mode */
		a = color_table[a].color_translate;
	}

	/* Return the (possibly translated) color */
	return (a);
}

/*
 * Verify a color.  Change to the 16-color equivalent, if necessary.
 */
byte verify_color(byte attr)
{
	if (attr < max_system_colors) return (attr);  /* Do nothing */

	return (color_table[attr].color_translate);  /* Translate */
}


/*
 * Attempt to get the closest 16-color equivalent of any RGB color.  This
 * is fairly crude code.
 */
byte translate_into_16_colors(int idx)
{
	long delta = 1000000L;
	int i;
	int drv, dgv, dbv, best;

	/* We already have a stored translation value: return it */
	if ((color_table[idx].color_translate >= 0) &&
	    (color_table[idx].color_translate < 16))
	{
		return (color_table[idx].color_translate);
	}

	/* We don't, and we need to find one (skip black) */
	for (best = -1, i = 1; i < 16; i++)
	{
		/* Get difference in RGB values */
		drv = ABS(color_table[idx].rv - color_table[i].rv);
		dgv = ABS(color_table[idx].gv - color_table[i].gv);
		dbv = ABS(color_table[idx].bv - color_table[i].bv);

		/* If squared RGB difference is less, remember this color */
		if (delta > (long)drv * drv + dgv * dgv + dbv * dbv)
		{
			delta = (long)drv * drv + dgv * dgv + dbv * dbv;
			best = i;
		}
	}

	/* Note failure */
	if (best < 0) return (TERM_WHITE);

	/* Success */
	color_table[idx].color_translate = best;
	return (best);
}


/*
 * Generates damage for "2d6" style dice rolls
 */
int damroll(int num, int sides)
{
	int i, sum = 0;

	/* Dice with no sides always come up zero */
	if (sides <= 0) return (0);

	/* Roll the dice */
	for (i = 0; i < num; i++)
	{
		sum += randint(sides);
	}

	return (sum);
}


/*
 * Turn damage into dice (with optional randomization).
 *
 * dam = (dd * (ds + 1) / 2)
 *
 * This function MUST be fed byte values for dice and sides, or it will break
 * on some compilers (such as DJGPP).
 */
void dam_to_dice(int dam, int *dice, int *sides, bool allow_random)
{
	/* Damage is too small */
	if (dam <= 0)
	{
		*dice = 0;
		*sides = 0;
	}

	/* Damage is too great */
	else if (dam > 30000)
	{
		*dice = 240;
		*sides = 249;
	}

	/* Damage is reasonable */
	else
	{
		/* Need a large enough space to calculate integers in */
		int tmp_dice, tmp_sides;

		if      (dam <  4) tmp_dice = 1;
		else if (dam < 16) tmp_dice = 2;
		else               tmp_dice = rsqrt(dam) - 2;

		tmp_sides = (dam * 2 / tmp_dice) - 1;

		/* Adjust dice for accuracy (perfect or standard rounding) */
		if (dam % tmp_dice)
		{
			if (allow_random)
			{
				if (rand_int(tmp_dice) < (dam % (tmp_dice))) tmp_sides += 1;
			}
			else
			{
				if ((tmp_dice / 2) < (dam % (tmp_dice))) tmp_sides += 1;
			}
		}

		/* Store the calculated values */
		*dice  = (byte)tmp_dice;
		*sides = (byte)tmp_sides;
	}
}

/*
 * Convert an input from tenths of a pound to tenths of a kilogram.  -LM-
 */
int make_metric(int wgt)
{
	int metric_wgt;

	/* Convert to metric values, using normal rounding. */
	metric_wgt = wgt * 10 / 22;
	if ((wgt) && ((!metric_wgt) || (wgt * 10) % 22 >= 11)) metric_wgt++;

	return (metric_wgt);
}


/*
 * Calculate square roots using Newton's method.
 *
 * This is not the fastest available integer math square root function
 * (for that, consult the site "www.azillionmonkeys.com/qed/sqroot.html").
 * It is, however, both moderately fast and extremely simple.  This
 * particular implementation features standard rounding (as opposed to
 * truncation).  -LM-
 */
u16b rsqrt(s32b input)
{
	s32b tmp = input;
	s32b tmp2, tmp3;
	s32b root;
	int i = 0;

	/* Stay legal */
	if (input <= 0L) return (0);


	/* Find out how many powers of two this number contains */
	while (tmp)
	{
		tmp /= 2;
		i++;
	}

	/*
	 * A decent first guess with Newton's method is input / 2.  A much
	 * better one is to remove half the powers of two (rounded down).
	 */
	tmp = input >> (i / 2);

	/* Close in on the truncated square root */
	while (TRUE)
	{
		/* Get a better guess */
		root = ((input / tmp) + tmp) / 2;

		/* Require good-enough accuracy (we correct any error later). */
		if ((root >= tmp - 1) && (root <= tmp + 1)) break;

		/* Save current guess */
		else tmp = root;
	}

	/* Get the square of the truncated square root */
	tmp2 = root * root;

	/* Get the square of the next higher integer */
	tmp3 = (root + 1) * (root + 1);

	/* If input > than the midpoint between these two values, round up */
	if (input > (tmp2 + tmp3) / 2) root++;

	/* Return */
	return (u16b)(root);
}


/*
 * Round a given value.
 *
 * "frac" controls the maximum amount of rounding compared to the given
 * value.  If "frac" is 8, for example, the given value must be at least
 * 80 for it to be rounded to the nearest 10.
 */
s32b round_it(const s32b v, int frac)
{
	s32b round = 5L;
	s32b abs_v = ABS(v);

	/* Rounding fraction must be at least 2, or no rounding happens */
	if (frac < 2) return (v);

	/* Small numbers aren't rounded */
	if (v / frac < 2L) return (v);

	/* Round by 5, then by 10, then by 25, then by 50, ... */
	while (TRUE)
	{
		if (v / frac < round) break;
		round *= 2L;  /* 10, 100, ... */
		if (v / frac < round) break;
		round *= 5L;
		round /= 2L;  /* 25, 250, ... */
		if (v / frac < round) break;
		round *= 2L;  /* 50, 500, ... */
	}

	/* Apply rounding */
	if ((abs_v % round) >= round / 2) abs_v += (round - (abs_v % round));
	else abs_v -= (abs_v % round);

	/* Return signed value */
	if (v < 0) return (-abs_v);
	return (abs_v);
}



/*
 * Accept values for y and x (considered as the endpoints of lines) between
 * 0 and 40, and return an angle in degrees (with 240 degrees in a circle).  -LM-
 *
 * This table's input and output need some processing:
 *
 * Because this table gives degrees for a whole circle, up to radius 20, its
 * origin is at (y, x) = (20, 20).  Therefore, the input code needs to find
 * the origin grid (where the lines being compared come from), and then map
 * it to table grid 20, 20.  Do not, however, actually try to compare the
 * angle of a line that begins and ends at the origin with any other line -
 * the table will return the value "255".
 *
 * The output of this table also needs to be massaged in order to avoid the
 * discontinuity at 0/240 degrees.  If we need to compare two angles:
 *   rotate = 120 - first value
 *     -- Get signed difference between 1st angle and 120 degrees
 *   tmp = ABS(second value + rotate) % 240
 *     -- Apply the same change to the 2nd angle, but wrap around
 *   diff = ABS(120 - tmp)
 *     -- absolute angular difference between the two
 *
 * Note that grids exactly diagonal to the origin have unique angles.
 */
byte get_angle_to_grid[41][41] =
{
  {  90,  89,  88,  87,  86,  85,  83,  82,  81,  79,  78,  76,  75,  73,  71,  69,  68,  66,  64,  62,  60,  58,  56,  54,  52,  51,  49,  47,  45,  44,  42,  41,  39,  38,  37,  35,  34,  33,  32,  31,  30 },
  {  91,  90,  89,  88,  87,  86,  84,  83,  82,  80,  79,  77,  75,  73,  72,  70,  68,  66,  64,  62,  60,  58,  56,  54,  52,  50,  48,  47,  45,  43,  41,  40,  38,  37,  36,  34,  33,  32,  31,  30,  29 },
  {  92,  91,  90,  89,  88,  87,  85,  84,  82,  81,  79,  78,  76,  74,  72,  70,  68,  66,  64,  62,  60,  58,  56,  54,  52,  50,  48,  46,  44,  42,  41,  39,  38,  36,  35,  33,  32,  31,  30,  29,  28 },
  {  93,  92,  91,  90,  89,  88,  86,  85,  83,  82,  80,  79,  77,  75,  73,  71,  69,  67,  64,  62,  60,  58,  56,  53,  51,  49,  47,  45,  43,  41,  40,  38,  37,  35,  34,  32,  31,  30,  29,  28,  27 },
  {  94,  93,  92,  91,  90,  89,  87,  86,  85,  83,  81,  80,  78,  76,  74,  72,  69,  67,  65,  62,  60,  58,  55,  53,  51,  48,  46,  44,  42,  40,  39,  37,  35,  34,  33,  31,  30,  29,  28,  27,  26 },
  {  95,  94,  93,  92,  91,  90,  89,  87,  86,  84,  82,  81,  79,  77,  75,  72,  70,  68,  65,  63,  60,  57,  55,  52,  50,  48,  45,  43,  41,  39,  38,  36,  34,  33,  31,  30,  29,  28,  27,  26,  25 },
  {  97,  96,  95,  94,  93,  91,  90,  89,  87,  85,  84,  82,  80,  78,  75,  73,  71,  68,  65,  63,  60,  57,  55,  52,  49,  47,  45,  42,  40,  38,  36,  35,  33,  31,  30,  29,  27,  26,  25,  24,  23 },
  {  98,  97,  96,  95,  94,  93,  91,  90,  88,  87,  85,  83,  81,  79,  77,  74,  71,  69,  66,  63,  60,  57,  54,  51,  49,  46,  43,  41,  39,  37,  35,  33,  32,  30,  29,  27,  26,  25,  24,  23,  22 },
  {  99,  98,  98,  97,  95,  94,  93,  92,  90,  88,  87,  85,  82,  80,  78,  75,  72,  69,  66,  63,  60,  57,  54,  51,  48,  45,  42,  40,  38,  35,  33,  32,  30,  28,  27,  26,  25,  23,  22,  22,  21 },
  { 101, 100,  99,  98,  97,  96,  95,  93,  92,  90,  88,  86,  84,  82,  79,  76,  73,  70,  67,  63,  60,  57,  53,  50,  47,  44,  41,  38,  36,  34,  32,  30,  28,  27,  25,  24,  23,  22,  21,  20,  19 },
  { 102, 101, 101, 100,  99,  98,  96,  95,  93,  92,  90,  88,  86,  83,  81,  78,  75,  71,  68,  64,  60,  56,  52,  49,  45,  42,  39,  37,  34,  32,  30,  28,  27,  25,  24,  22,  21,  20,  19,  19,  18 },
  { 104, 103, 102, 101, 100,  99,  98,  97,  95,  94,  92,  90,  88,  85,  82,  79,  76,  72,  68,  64,  60,  56,  52,  48,  44,  41,  38,  35,  32,  30,  28,  26,  25,  23,  22,  21,  20,  19,  18,  17,  16 },
  { 105, 105, 104, 103, 102, 101, 100,  99,  98,  96,  94,  92,  90,  87,  85,  81,  78,  74,  69,  65,  60,  55,  51,  46,  42,  39,  35,  33,  30,  28,  26,  24,  22,  21,  20,  19,  18,  17,  16,  15,  15 },
  { 107, 107, 106, 105, 104, 103, 102, 101, 100,  98,  97,  95,  93,  90,  87,  84,  80,  75,  71,  65,  60,  55,  49,  45,  40,  36,  33,  30,  27,  25,  23,  22,  20,  19,  18,  17,  16,  15,  14,  13,  13 },
  { 109, 108, 108, 107, 106, 105, 105, 103, 102, 101,  99,  98,  95,  93,  90,  87,  82,  78,  72,  66,  60,  54,  48,  42,  38,  33,  30,  27,  25,  22,  21,  19,  18,  17,  15,  15,  14,  13,  12,  12,  11 },
  { 111, 110, 110, 109, 108, 108, 107, 106, 105, 104, 102, 101,  99,  96,  93,  90,  86,  81,  75,  68,  60,  52,  45,  39,  34,  30,  27,  24,  21,  19,  18,  16,  15,  14,  13,  12,  12,  11,  10,  10,   9 },
  { 112, 112, 112, 111, 111, 110, 109, 109, 108, 107, 105, 104, 102, 100,  98,  94,  90,  85,  78,  69,  60,  51,  42,  35,  30,  26,  22,  20,  18,  16,  15,  13,  12,  11,  11,  10,   9,   9,   8,   8,   8 },
  { 114, 114, 114, 113, 113, 112, 112, 111, 111, 110, 109, 108, 106, 105, 102,  99,  95,  90,  82,  72,  60,  48,  38,  30,  25,  21,  18,  15,  14,  12,  11,  10,   9,   9,   8,   8,   7,   7,   6,   6,   6 },
  { 116, 116, 116, 116, 115, 115, 115, 114, 114, 113, 112, 112, 111, 109, 108, 105, 102,  98,  90,  78,  60,  42,  30,  22,  18,  15,  12,  11,   9,   8,   8,   7,   6,   6,   5,   5,   5,   4,   4,   4,   4 },
  { 118, 118, 118, 118, 118, 117, 117, 117, 117, 117, 116, 116, 115, 115, 114, 112, 111, 108, 102,  90,  60,  30,  18,  12,   9,   8,   6,   5,   5,   4,   4,   3,   3,   3,   3,   3,   2,   2,   2,   2,   2 },
  { 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 255,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 },
  { 122, 122, 122, 122, 122, 123, 123, 123, 123, 123, 124, 124, 125, 125, 126, 128, 129, 132, 138, 150, 180, 210, 222, 228, 231, 232, 234, 235, 235, 236, 236, 237, 237, 237, 237, 237, 238, 238, 238, 238, 238 },
  { 124, 124, 124, 124, 125, 125, 125, 126, 126, 127, 128, 128, 129, 131, 132, 135, 138, 142, 150, 162, 180, 198, 210, 218, 222, 225, 228, 229, 231, 232, 232, 233, 234, 234, 235, 235, 235, 236, 236, 236, 236 },
  { 126, 126, 126, 127, 127, 128, 128, 129, 129, 130, 131, 132, 134, 135, 138, 141, 145, 150, 158, 168, 180, 192, 202, 210, 215, 219, 222, 225, 226, 228, 229, 230, 231, 231, 232, 232, 233, 233, 234, 234, 234 },
  { 128, 128, 128, 129, 129, 130, 131, 131, 132, 133, 135, 136, 138, 140, 142, 146, 150, 155, 162, 171, 180, 189, 198, 205, 210, 214, 218, 220, 222, 224, 225, 227, 228, 229, 229, 230, 231, 231, 232, 232, 232 },
  { 129, 130, 130, 131, 132, 132, 133, 134, 135, 136, 138, 139, 141, 144, 147, 150, 154, 159, 165, 172, 180, 188, 195, 201, 206, 210, 213, 216, 219, 221, 222, 224, 225, 226, 227, 228, 228, 229, 230, 230, 231 },
  { 131, 132, 132, 133, 134, 135, 135, 137, 138, 139, 141, 142, 145, 147, 150, 153, 158, 162, 168, 174, 180, 186, 192, 198, 202, 207, 210, 213, 215, 218, 219, 221, 222, 223, 225, 225, 226, 227, 228, 228, 229 },
  { 133, 133, 134, 135, 136, 137, 138, 139, 140, 142, 143, 145, 147, 150, 153, 156, 160, 165, 169, 175, 180, 185, 191, 195, 200, 204, 207, 210, 213, 215, 217, 218, 220, 221, 222, 223, 224, 225, 226, 227, 227 },
  { 135, 135, 136, 137, 138, 139, 140, 141, 142, 144, 146, 148, 150, 153, 155, 159, 162, 166, 171, 175, 180, 185, 189, 194, 198, 201, 205, 207, 210, 212, 214, 216, 218, 219, 220, 221, 222, 223, 224, 225, 225 },
  { 136, 137, 138, 139, 140, 141, 142, 143, 145, 146, 148, 150, 152, 155, 158, 161, 164, 168, 172, 176, 180, 184, 188, 192, 196, 199, 202, 205, 208, 210, 212, 214, 215, 217, 218, 219, 220, 221, 222, 223, 224 },
  { 138, 139, 139, 140, 141, 142, 144, 145, 147, 148, 150, 152, 154, 157, 159, 162, 165, 169, 172, 176, 180, 184, 188, 191, 195, 198, 201, 203, 206, 208, 210, 212, 213, 215, 216, 218, 219, 220, 221, 221, 222 },
  { 139, 140, 141, 142, 143, 144, 145, 147, 148, 150, 152, 154, 156, 158, 161, 164, 167, 170, 173, 177, 180, 183, 187, 190, 193, 196, 199, 202, 204, 206, 208, 210, 212, 213, 215, 216, 217, 218, 219, 220, 221 },
  { 141, 142, 142, 143, 145, 146, 147, 148, 150, 152, 153, 155, 158, 160, 162, 165, 168, 171, 174, 177, 180, 183, 186, 189, 192, 195, 198, 200, 202, 205, 207, 208, 210, 212, 213, 214, 215, 217, 218, 218, 219 },
  { 142, 143, 144, 145, 146, 147, 149, 150, 152, 153, 155, 157, 159, 161, 163, 166, 169, 171, 174, 177, 180, 183, 186, 189, 191, 194, 197, 199, 201, 203, 205, 207, 208, 210, 211, 213, 214, 215, 216, 217, 218 },
  { 143, 144, 145, 146, 147, 149, 150, 151, 153, 155, 156, 158, 160, 162, 165, 167, 169, 172, 175, 177, 180, 183, 185, 188, 191, 193, 195, 198, 200, 202, 204, 205, 207, 209, 210, 211, 213, 214, 215, 216, 217 },
  { 145, 146, 147, 148, 149, 150, 151, 153, 154, 156, 158, 159, 161, 163, 165, 168, 170, 172, 175, 177, 180, 183, 185, 188, 190, 192, 195, 197, 199, 201, 202, 204, 206, 207, 209, 210, 211, 212, 213, 214, 215 },
  { 146, 147, 148, 149, 150, 151, 153, 154, 155, 157, 159, 160, 162, 164, 166, 168, 171, 173, 175, 178, 180, 182, 185, 187, 189, 192, 194, 196, 198, 200, 201, 203, 205, 206, 207, 209, 210, 211, 212, 213, 214 },
  { 147, 148, 149, 150, 151, 152, 154, 155, 157, 158, 160, 161, 163, 165, 167, 169, 171, 173, 176, 178, 180, 182, 184, 187, 189, 191, 193, 195, 197, 199, 200, 202, 203, 205, 206, 208, 209, 210, 211, 212, 213 },
  { 148, 149, 150, 151, 152, 153, 155, 156, 158, 159, 161, 162, 164, 166, 168, 170, 172, 174, 176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 196, 198, 199, 201, 202, 204, 205, 207, 208, 209, 210, 211, 212 },
  { 149, 150, 151, 152, 153, 154, 156, 157, 158, 160, 161, 163, 165, 167, 168, 170, 172, 174, 176, 178, 180, 182, 184, 186, 188, 190, 192, 193, 195, 197, 199, 200, 202, 203, 204, 206, 207, 208, 209, 210, 211 },
  { 150, 151, 152, 153, 154, 155, 157, 158, 159, 161, 162, 164, 165, 167, 169, 171, 172, 174, 176, 178, 180, 182, 184, 186, 188, 189, 191, 193, 195, 196, 198, 199, 201, 202, 203, 205, 206, 207, 208, 209, 210 }
};



/*
 * Calculates and returns the angle to the target or in the given
 * direction.
 *
 * Note:  If a compass direction is supplied, we ignore any target.
 * Note:  We supply the angle divided by 2.
 */
int get_angle_to_target(int y0, int x0, int y1, int x1, int dir)
{
	int ny, nx;

	/* No valid compass direction given */
	if ((dir == 0) || (dir == 5) || (dir > 9))
	{
		/* Check for a valid target */
		if ((y1) && (x1))
		{
			/* Get absolute distance between source and target */
			int ay = ABS(y1 - y0);
			int ax = ABS(x1 - x0);
			int delta = MAX(ay, ax);

			/* If distance is too great, shorten it */
			if (delta > 20)
			{
				/* Shorten distances in both directions */
				ay = (20 * ay + delta/2) / delta;
				ax = (20 * ax + delta/2) / delta;

				/* Change target, using new distances */
				if (y1 - y0 > 0) y1 = y0 + ay;
				else             y1 = y0 - ay;
				if (x1 - x0 > 0) x1 = x0 + ax;
				else             x1 = x0 - ax;
			}

			/* Reorient grid for table access */
			ny = 20 + (y1 - y0);
			nx = 20 + (x1 - x0);

			/* Paranoia -- Illegal table access is bad */
			if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))
			{
				/* Note error */
				return (-1);
			}
		}

		/* No compass direction and no target --> note error */
		else
		{
			return (-1);
		}
	}

	/* We have a valid compass direction */
	else
	{
		/* Step in that direction a bunch of times, get target */
		y1 = y0 + (ddy[dir] * 10);
		x1 = x0 + (ddx[dir] * 10);

		/* Convert to table grids */
		ny = 20 + (y1 - y0);
		nx = 20 + (x1 - x0);
	}

	/* Get angle to target. */
	return (get_angle_to_grid[ny][nx]);
}


/*
 * Using the angle given, find a grid that is in that direction from the
 * origin.  Angle given must be half of actual (so 240 degrees --> 120).
 *
 * Note:  This function does not yield very good results when the
 * character is adjacent to the outer wall of the dungeon and the
 * projection heads towards it.
 */
void get_grid_using_angle(int angle, int y0, int x0, int *ty, int *tx)
{
	int y_top, y_bottom, x_left, x_right;

	int y, x;
	int best_y = 0, best_x = 0;

	int diff;
	int this_angle;
	int fudge = 180;


	/* Angle must be legal */
	if ((angle < 0) || (angle >= 240)) return;


	/* Assume maximum grids, but stay (fully) within the dungeon */
	y_top =    MAX(y0 - 20, 1);
	y_bottom = MIN(y0 + 20, dungeon_hgt - 1);
	x_left =   MAX(x0 - 20, 1);
	x_right =  MIN(x0 + 20, dungeon_wid - 1);

	/* Translate to table coordinates */
	y_top =    y_top    - y0 + 20;
	y_bottom = y_bottom - y0 + 20;
	x_left =   x_left   - x0 + 20;
	x_right =  x_right  - x0 + 20;

	/* Constrain to the quadrant of the table that contains this angle */
	if (angle >= 180)
	{
		y_top =    MAX(y_top, 20);
		x_left =   MAX(x_left, 20);
	}
	else if (angle >= 120)
	{
		y_top =    MAX(y_top, 20);
		x_right =  MIN(x_right, 21);
	}
	else if (angle >= 60)
	{
		y_bottom = MIN(y_bottom, 21);
		x_right =  MIN(x_right, 21);
	}
	else
	{
		y_bottom = MIN(y_bottom, 21);
		x_left =   MAX(x_left, 20);
	}

	/* Paranoia -- no illegal table access (should never happen) */
	if (y_top < 0 || y_top > 40 || y_bottom < 0 || y_bottom > 40 ||
	    x_left < 0 || x_left > 40 || x_right < 0 || x_right > 40)
	{
		return;
	}

	/* Scan that portion of the table we have limited ourselves to */
	for (y = y_top; y < y_bottom; y++)
	{
		for (x = x_left; x < x_right; x++)
		{
			/* Check this table grid */
			this_angle = get_angle_to_grid[y][x];

			/* Get inaccuracy of this angle */
			diff = ABS(angle - this_angle);

			/* Inaccuracy is lower than previous best */
			if (diff < fudge)
			{
				/* Note coordinates */
				best_y = y;
				best_x = x;

				/* Save inaccuracy as a new best */
				fudge = diff;

				/* Note perfection */
				if (fudge == 0) break;
			}
		}

		/* Note perfection */
		if (fudge == 0) break;
	}

	/* We have an unacceptably large fudge factor */
	if (fudge >= 60)
	{
		/* Set target to original grid */
		*ty = y0;
		*tx = x0;
	}

	/* Usual case */
	else
	{
		/* Set target */
		*ty = y0 - 20 + best_y;
		*tx = x0 - 20 + best_x;
	}
}

/*
 * Returns the position of a bit flag in a 32-bit value (from 0 to 31).
 */
int get_loc_of_flag(u32b flag)
{
	int i = 0;

	while (TRUE)
	{
		if (flag <= 1L) break;
		flag = flag >> 1;
		i++;
	}

	return (i);
}
