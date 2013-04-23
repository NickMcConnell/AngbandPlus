#define UTIL_C
/* File: util.c */

/* Purpose: Angband utilities -BEN- */


#include "angband.h"




#ifndef HAS_MEMSET

/*
 * For those systems that don't have "memset()"
 *
 * Set the value of each of 'n' bytes starting at 's' to 'c', return 's'
 * If 'n' is negative, you will erase a whole lot of memory.
 */
char *memset(char *s, int c, huge n)
{
	char *t;
	for (t = s; len--; ) *t++ = c;
	return (s);
}

#endif



#ifndef HAS_STRICMP

/*
 * For those systems that don't have "stricmp()"
 *
 * Compare the two strings "a" and "b" ala "strcmp()" ignoring case.
 */
int stricmp(cptr a, cptr b)
{
	cptr s1, s2;
	char z1, z2;

	/* Scan the strings */
	for (s1 = a, s2 = b; TRUE; s1++, s2++)
	{
		z1 = FORCEUPPER(*s1);
		z2 = FORCEUPPER(*s2);
		if (z1 < z2) return (-1);
		if (z1 > z2) return (1);
		if (!z1) return (0);
	}
}

#endif


#ifdef SET_UID

# ifndef HAS_USLEEP

/*
 * For those systems that don't have "usleep()" but need it.
 *
 * Fake "usleep()" function grabbed from the inl netrek server -cba
 */
int usleep(huge usecs)
{
	struct timeval      Timer;

	int                 nfds = 0;

#ifdef FD_SET
	fd_set *no_fds = NULL;
#else
	int *no_fds = NULL;
#endif


	/* Was: int readfds, writefds, exceptfds; */
	/* Was: readfds = writefds = exceptfds = 0; */


	/* Paranoia -- No excessive sleeping */
	if (usecs > 4000000L) core("Illegal usleep() call");


	/* Wait for it */
	Timer.tv_sec = (usecs / 1000000L);
	Timer.tv_usec = (usecs % 1000000L);

	/* Wait for it */
	if (select(nfds, no_fds, no_fds, no_fds, &Timer) < 0)
	{
		/* Hack -- ignore interrupts */
		if (errno != EINTR) return -1;
	}

	/* Success */
	return 0;
}

# endif


/*
 * Hack -- External functions
 */
#ifndef _PWD_H
extern struct passwd *getpwuid();
extern struct passwd *getpwnam();
#endif /* _PWD_H */


/*
 * Find a default user name from the system.
 */
void user_name(char *buf, int id)
{
#ifdef SET_UID
	struct passwd *pw;

	/* Look up the user name */
	if ((pw = getpwuid(id)))
	{
		sprintf(buf, "%.16s", pw->pw_name);

#ifdef CAPITALIZE_USER_NAME
		/* Hack -- capitalize the user name */
		if (ISLOWER(buf[0])) buf[0] = TOUPPER(buf[0]);
#endif /* CAPITALIZE_USER_NAME */

		return;
	}
#endif /* SET_UID */

	/* Oops.  Hack -- default to "PLAYER" */
	strcpy(buf, "PLAYER");
}

#endif /* SET_UID */




/*
 * The concept of the "file" routines below (and elsewhere) is that all
 * file handling should be done using as few routines as possible, since
 * every machine is slightly different, but these routines always have the
 * same semantics.
 *
 * In fact, perhaps we should use the "path_parse()" routine below to convert
 * from "canonical" filenames (optional leading tilde's, internal wildcards,
 * slash as the path seperator, etc) to "system" filenames (no special symbols,
 * system-specific path seperator, etc).  This would allow the program itself
 * to assume that all filenames are "Unix" filenames, and explicitly "extract"
 * such filenames if needed (by "path_parse()", or perhaps "path_canon()").
 *
 * Note that "path_temp" should probably return a "canonical" filename.
 *
 * Note that "my_fopen()" and "my_open()" and "my_make()" and "my_kill()"
 * and "my_move()" and "my_copy()" should all take "canonical" filenames.
 *
 * Note that "canonical" filenames use a leading "slash" to indicate an absolute
 * path, and a leading "tilde" to indicate a special directory, and default to a
 * relative path, but MSDOS uses a leading "drivename plus colon" to indicate
 * the use of a "special drive", and then the rest of the path is parsed
 * "normally", and MACINTOSH uses a leading colon to indicate a relative path,
 * and an embedded colon to indicate a "drive plus absolute path", and finally
 * defaults to a file in the current working directory, which may or may not be
 * defined.
 *
 * We should probably parse a leading "~~/" as referring to "ANGBAND_DIR". (?)
 */


#ifdef ACORN


/*
 * Most of the "file" routines for "ACORN" should be in "main-acn.c"
 */


#else /* ACORN */


#ifdef SET_UID

/*
 * Extract a "parsed" path from an initial filename
 * Normally, we simply copy the filename into the buffer
 * But leading tilde symbols must be handled in a special way
 * Replace "~user/" by the home directory of the user named "user"
 * Replace "~/" by the home directory of the current user
 */
static errr path_parse(char *buf, uint max, cptr file)
{
	cptr u, s;
	struct passwd *pw;


	/* Assume no result */
	buf[0] = '\0';

	/* No file? */
	if (!file) return (-1);

	/* File needs no parsing */
	if (file[0] != '~')
	{
		/* Check length. */
		if (strlen(file) >= max) return (1);
		strcpy(buf, file);
		return (0);
	}

	/* Point at the user */
	u = file+1;

	/* Look for non-user portion of the file */
	s = strstr(u, PATH_SEP);

	/* Look up the "current" user */
	if (u == s) u = getlogin();

	/* Extract a user name */
	else if (s) u = format("%.*s", s-u, u);

	/* Look up a user (or "current" user) */
	if (u) pw = getpwnam(u);
	else pw = getpwuid(getuid());

	/* Nothing found? */
	if (!pw) return (1);

	/* Check length. */
	if (strlen(pw->pw_dir)+strlen(s) >= max) return (1);

	/* Make use of the info */
	sprintf(buf, "%s%s", pw->pw_dir, s);

	/* Success */
	return (0);
}


#else /* SET_UID */


/*
 * Extract a "parsed" path from an initial filename
 *
 * This requires no special processing on simple machines,
 * except for verifying the size of the filename.
 */
static errr path_parse(char *buf, int max, cptr file)
{
	/* Accept the filename */
	strnfmt(buf, max, "%s", file);

	/* Success */
	return (0);
}


#endif /* SET_UID */


#ifndef HAVE_MKSTEMP

/*
 * Hack -- acquire a "temporary" file name if possible
 *
 * This filename is always in "system-specific" form.
 */
static errr path_temp(char *buf, uint max)
{
	cptr s;

	/* Temp file */
	s = tmpnam(NULL);

	/* Oops */
	if (!s || strlen(s) >= max) return (-1);

	/* Format to length */
	strcpy(buf, s);

	/* Success */
	return (0);
}

#endif /* HAVE_MKSTEMP */
#endif /* ACORN */

/*
 * Create a new path by appending a file (or directory) to a path
 *
 * This requires no special processing on simple machines, except
 * for verifying the size of the filename, but note the ability to
 * bypass the given "path" with certain special file-names.
 *
 * Note that the "file" may actually be a "sub-path", including
 * a path and a file.
 *
 * Note that this function yields a path which must be "parsed"
 * using the "parse" function above.
 */
static void path_build(char *buf, uint max, cptr path, cptr file)
{
	/* Special file */
	if (file[0] == '~')
	{
		/* Use the file itself */
		strnfmt(buf, max, "%s", file);
	}

	/* Absolute file, on "normal" systems */
	else if (prefix(file, PATH_SEP) && !streq(PATH_SEP, ""))
	{
		/* Use the file itself */
		strnfmt(buf, max, "%s", file);
	}

	/* No path given */
	else if (!path[0])
	{
		/* Use the file itself */
		strnfmt(buf, max, "%s", file);
	}

	/* Path and File */
	else
	{
		/* Build the new path */
		strnfmt(buf, max, "%s%s%s", path, PATH_SEP, file);
	}

	/* Success */
	return;
}

#ifndef ACORN
/*
 * Hack -- replacement for "fopen()"
 */
FILE *my_fopen(cptr file, cptr mode)
{
	char                buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, 1024, file)) return (NULL);

	/* Attempt to fopen the file anyway */
	return (fopen(buf, mode));
}


/*
 * Hack -- replacement for "fclose()"
 */
errr my_fclose(FILE *fff)
{
	/* Require a file */
	if (!fff) return (-1);

	/* Close, check for error */
	if (fclose(fff) == EOF) return (1);

	/* Success */
	return (0);
}


#endif /* ACORN */

/*
 * Process a path_build() function as a vstrnfmt_aux function.
 *
 * Format:
 * "%v", path_build_f2, (cptr)path, (cptr)file
 */
void path_build_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	cptr path = va_arg(*vp, cptr);
	cptr file = va_arg(*vp, cptr);

	path_build(buf, max, path, file);
}

/*
 * Open and return a file at a given path, which may be converted to suit
 * the system.
 * NB This does not convert the FILE_TYPE used in the Macintosh port.
 */
FILE *my_fopen_path(cptr path, cptr file, cptr mode)
{
	char buf[1024];
	strnfmt(buf, 1024, "%v", path_build_f2, path, file);
	return my_fopen(buf, mode);
}

/*
 * Create a temporary file, store its name in buf, open it, and return a
 * pointer to it. Return NULL on failure.
 */

#ifdef HAVE_MKSTEMP

FILE *my_fopen_temp(char *buf, uint max)
{
	int fd;

	/* Paranoia */
	if (strlen("/tmp/anXXXXXX") >= max)
	{
		if (alert_failure)
			msg_print("Buffer too short for temporary file name!");
		return (NULL);
	}

	/* Prepare the buffer for mkstemp */
	strcpy(buf, "/tmp/anXXXXXX");

	/* Secure creation of a temporary file */
	fd = mkstemp(buf);

	/* Check the file-descriptor */
	if (fd < 0) return (NULL);

	/* Return a file stream */
	return (fdopen(fd, "w"));
}

#else /* HAVE_MKSTEMP */

FILE *my_fopen_temp(char *buf, int max)
{
	/* Generate a temporary filename */
	if (path_temp(buf, max)) return (NULL);

	/* Open the file */
	return (my_fopen(buf, "w"));
}

#endif /* HAVE_MKSTEMP */



/*
 * Hack -- replacement for "fgets()"
 *
 * Read a string, without a newline, to a file
 *
 * Process tabs, strip internal non-printables
 */
#define TAB_COLUMNS   8

errr my_fgets(FILE *fff, char *buf, size_t n)
{
	uint i, len;
	int UNREAD(c); /* Not read if n <= 1, written to otherwise. */

	/* Enforce reasonable upper bound */
	n = MIN(n, 65535);

	/* Leave a byte for terminating null */
	len = n - 1;

	/* Paranoia */
	if (len <= 0) return PARSE_ERROR_OUT_OF_MEMORY;

	/* While there's room left in the buffer */
	for (i = 0; i < len; )
	{
		/*
		 * Read next character - stdio buffers I/O, so there's no
		 * need to buffer it again using fgets.
		 */
		c = fgetc(fff);

		/* End of file */
		if (c == EOF)
		{
			/*
			 * Be nice to DOS/Windows, where a last line of a file isn't
			 * always \n terminated.
			 */
			buf[i] = '\0';

			/* No characters read -- signal error */
			if (i == 0)
				return FILE_ERROR_EOF;
			/* Treat as normal otherwise. */
			else
				return SUCCESS;
		}

		/* End of line */
		else if (strchr("\r\n", c))
		{
			/* Handle "\r\n" line endings. */
			if (c == '\r')
			{
				c = fgetc(fff);
				if (c != '\n') ungetc(c, fff);
			}

			/* Null terminate */
			buf[i] = '\0';

			return SUCCESS;
		}

		/* Expand a tab into spaces */
		else if (c == '\t')
		{
			uint tabstop;

			/* Next tab stop */
			tabstop = ((i + TAB_COLUMNS) / TAB_COLUMNS) * TAB_COLUMNS;

			/* Bounds check */
			if (tabstop >= len) break;

			/* Convert it to spaces */
			while (i < tabstop)
			{
				/* Store space */
				buf[i++] = ' ';
			}
		}

		/* Ignore non-printables */
		else if (ISPRINT(c))
		{
			/* Store character in the buffer */
			buf[i++] = c;
		}
	}

	/* Buffer overflow - return the string obtained so far. */
	buf[i] = '\0';

	/* Allow the last (failed) character to be read later. */
	ungetc(c, fff);

	/* Error */
	return FILE_ERROR_OVERFLOW;
}

/*
 * A wrapper around my_fgets() which doesn't treat overflow as an error, but
 * appends \n to calls which don't overflow.
 */
errr my_fgets_long(char *buf, size_t n, FILE *fff)
{
	errr err;

	/* Paranoia - no room for anything. */
	if (!n) return PARSE_ERROR_OUT_OF_MEMORY;

	err = my_fgets(fff, buf, n-1);
	switch (err)
	{
		case FILE_ERROR_OVERFLOW:
		{
			return SUCCESS;
		}
		case SUCCESS:
		{
			strcat(buf, "\n");
			return SUCCESS;
		}
		default:
		{
			return err;
		}
	}
}

/*
 * A macro to turn a format string with arguments into a normal string in the
 * format buffer.
 */
#define get_va_arg_string(fmt) \
do { \
	va_list vp; \
	va_start(vp, (fmt)); \
	vformat((fmt), vp); \
	va_end(vp); \
} while (0)

#define get_va_arg_buf(buf, fmt) \
do { \
	va_list vp; \
	va_start(vp, (fmt)); \
	vstrnfmt((buf), sizeof(buf), (fmt), vp); \
	va_end(vp); \
} while (0)

/*
 * A version of "fprintf()" which uses strnfmt to process its arguments.
 *
 * It uses the format buffer as there is no limit on how long a string
 * could be sensible, although it could loop over the string a piece at a
 * time.
 */
int my_fprintf(FILE *fff, cptr fmt, ...)
{
	get_va_arg_string(fmt);

	return fprintf(fff, "%s", format(0));
}


#ifdef ACORN


/*
 * Most of the "file" routines for "ACORN" should be in "main-acn.c"
 *
 * Many of them can be rewritten now that only "fd_open()" and "fd_make()"
 * and "my_fopen()" should ever create files.
 */


#else /* ACORN */


/*
 * Code Warrior is a little weird about some functions
 */
#ifdef BEN_HACK
extern int open(const char *, int, ...);
extern int close(int);
extern int read(int, void *, unsigned int);
extern int write(int, const void *, unsigned int);
extern long lseek(int, long, int);
#endif /* BEN_HACK */


/*
 * The Macintosh is a little bit brain-dead sometimes
 */
#ifdef MACINTOSH
# define open(N,F,M) \
	((M), open((char*)(N),F))
# define write(F,B,S) \
	write(F,(char*)(B),S)
#endif /* MACINTOSH */


/*
 * Several systems have no "O_BINARY" flag
 */
#ifndef O_BINARY
# define O_BINARY 0
#endif /* O_BINARY */


/*
 * Hack -- attempt to delete a file
 */
errr fd_kill(cptr file)
{
	char                buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, 1024, file)) return (-1);

	/* Remove */
	(void)remove(buf);

	/* XXX XXX XXX */
	return (0);
}


/*
 * Hack -- attempt to move a file
 */
errr fd_move(cptr file, cptr what)
{
	char                buf[1024];
	char                aux[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, 1024, file)) return (-1);

	/* Hack -- Try to parse the path */
	if (path_parse(aux, 1024, what)) return (-1);

	/* Rename */
	(void)rename(buf, aux);

	/* XXX XXX XXX */
	return (0);
}


/*
 * Hack -- attempt to copy a file
 * The player should have read access to "in" and write access to "out".
 */
errr fd_copy(cptr out, cptr in)
{
	char buf[1024];

	/* Open the files. */
	FILE *fin = my_fopen(in, "r");
	FILE *fout = my_fopen(out, "w");

	if (!fin || !fout) return FILE_ERROR_CANNOT_OPEN_FILE;

	/* Copy across faithfully. */
	while (fgets(buf, 1024, fin)) fprintf(fout, "%s", buf);

	return SUCCESS;
}


#ifdef PRIVATE_USER_PATH
/*
 * Create a directory.
 * Return 0 if successful.
 * Return 1 if the directory already exists.
 * Return -1 on other errors.
 */
errr my_mkdir(cptr path, uint mode)
{
	char buf[1024];
	int rc;

	if (path_parse(buf, 1024, path)) return FILE_ERROR_CANNOT_OPEN_FILE;

	rc = mkdir(buf, mode);

	/* Success. */
	if (!rc) return SUCCESS;

	else if (errno == EEXIST) return FILE_ERROR_FILE_EXISTS;

	else return FILE_ERROR_CANNOT_OPEN_FILE;
}
#endif /* PRIVATE_USER_PATH */

/*
 * Hack -- attempt to open a file descriptor (create file)
 *
 * This function should fail if the file already exists
 *
 * Note that we assume that the file should be "binary"
 *
 * XXX XXX XXX The horrible "BEN_HACK" code is for compiling under
 * the CodeWarrior compiler, in which case, for some reason, none
 * of the "O_*" flags are defined, and we must fake the definition
 * of "O_RDONLY", "O_WRONLY", and "O_RDWR" in "A-win-h", and then
 * we must simulate the effect of the proper "open()" call below.
 */
int fd_make(cptr file, int mode)
{
	char                buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, 1024, file)) return (-1);

#ifdef BEN_HACK

	/* Check for existance */
	/* if (fd_close(fd_open(file, O_RDONLY | O_BINARY))) return (1); */

	/* Mega-Hack -- Create the file */
	(void)my_fclose(my_fopen(file, "wb"));

	/* Re-open the file for writing */
	return (open(buf, O_WRONLY | O_BINARY, mode));

#else /* BEN_HACK */

	/* Create the file, fail if exists, write-only, binary */
	return (open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, mode));

#endif /* BEN_HACK */

}


/*
 * Hack -- attempt to open a file descriptor (existing file)
 *
 * Note that we assume that the file should be "binary"
 */
int fd_open(cptr file, int flags)
{
	char                buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, 1024, file)) return (-1);

	/* Attempt to open the file */
	return (open(buf, flags | O_BINARY, 0));
}


/*
 * Hack -- attempt to lock a file descriptor
 *
 * Legal lock types -- F_UNLCK, F_RDLCK, F_WRLCK
 */
errr fd_lock(int fd, int what)
{
	/* XXX XXX */
	what = what ? what : 0;

	/* Verify the fd */
	if (fd < 0) return (-1);

#ifdef SET_UID

# ifdef USG

#  if defined(F_ULOCK) && defined(F_LOCK)

	/* Un-Lock */
	if (what == F_UNLCK)
	{
		/* Unlock it, Ignore errors */
		lockf(fd, F_ULOCK, 0);
	}

	/* Lock */
	else
	{
		/* Lock the score file */
		if (lockf(fd, F_LOCK, 0) != 0) return (1);
	}

#  endif

# else

#  if defined(LOCK_UN) && defined(LOCK_EX)

	/* Un-Lock */
	if (what == F_UNLCK)
	{
		/* Unlock it, Ignore errors */
		(void)flock(fd, LOCK_UN);
	}

	/* Lock */
	else
	{
		/* Lock the score file */
		if (flock(fd, LOCK_EX) != 0) return (1);
	}

#  endif

# endif

#endif

	/* Success */
	return (0);
}


/*
 * Hack -- attempt to seek on a file descriptor
 */
errr fd_seek(int fd, huge n)
{
	long p;

	/* Verify fd */
	if (fd < 0) return (-1);

	/* Seek to the given position */
	p = lseek(fd, n, SEEK_SET);

	/* Failure */
	if (p < 0) return (1);

	/* Failure */
	if (p != (long)n) return (1);

	/* Success */
	return (0);
}


/*
 * Hack -- attempt to read data from a file descriptor
 */
errr fd_read(int fd, char *buf, huge n)
{
	/* Verify the fd */
	if (fd < 0) return (-1);

#ifndef SET_UID

	/* Read pieces */
	while (n >= 16384)
	{
		/* Read a piece */
		if (read(fd, buf, 16384) != 16384) return (1);

		/* Shorten the task */
		buf += 16384;

		/* Shorten the task */
		n -= 16384;
	}

#endif

	/* Read the final piece */
	if (read(fd, buf, n) != (long)n) return (1);

	/* Success */
	return (0);
}


/*
 * Hack -- Attempt to write data to a file descriptor
 */
errr fd_write(int fd, cptr buf, huge n)
{
	/* Verify the fd */
	if (fd < 0) return (-1);

#ifndef SET_UID

	/* Write pieces */
	while (n >= 16384)
	{
		/* Write a piece */
		if (write(fd, buf, 16384) != 16384) return (1);

		/* Shorten the task */
		buf += 16384;

		/* Shorten the task */
		n -= 16384;
	}

#endif

	/* Write the final piece */
	if (write(fd, buf, n) != (long)n) return (1);

	/* Success */
	return (0);
}


/*
 * Hack -- attempt to close a file descriptor
 */
errr fd_close(int fd)
{
	/* Verify the fd */
	if (fd < 0) return (-1);

	/* Close */
	(void)close(fd);

	/* XXX XXX XXX */
	return (0);
}



#endif /* ACORN */




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
 * Note that some machines (i.e. most IBM machines) are limited to a
 * hard-coded set of colors, and so the information above is useless.
 *
 * Also, some machines are limited to a pre-determined set of colors,
 * for example, the IBM can only display 16 colors, and only 14 of
 * those colors resemble colors used by Angband, and then only when
 * you ignore the fact that "Slate" and "cyan" are not really matches,
 * so on the IBM, we use "orange" for both "Umber", and "Light Umber"
 * in addition to the obvious "Orange", since by combining all of the
 * "indeterminate" colors into a single color, the rest of the colors
 * are left with "meaningful" values.
 */


/*
 * Move the cursor
 */
void move_cursor(int row, int col)
{
	Term_gotoxy(col, row);
}



static cptr ascii_text_conv[512];

/* Hard-coded conversions, to be used in preference to the value-based ones. */
typedef struct ascii_conv ascii_conv;
struct ascii_conv
{
	byte ch;
	cptr str;
};

static ascii_conv ascii_text_conv_hardcoded[] =
{
	{ESCAPE, "\\e"},
	{' ', "\\s"},
	{'\b', "\\b"},
	{'\t', "\\t"},
	{'\n', "\\n"},
	{'\r', "\\r"},
	{'^', "\\^"},
	{'\\', "\\\\"}
};

/*
 * Fill the ascii_text_conv[] table with strings for each ascii character.
 */
void init_ascii_text_conv(void)
{
	int i;
	ascii_conv *ptr;
	for (i = 0; i < 256; i++)
	{
		char buf[MAX_ASCII_LEN+1];
		cptr s = NULL;

		FOR_ALL_IN(ascii_text_conv_hardcoded, ptr)
		{
			if (i == ptr->ch) s = ptr->str;
		}

		if (i < 32)
		{
			sprintf(buf, "^%c", i+64);
		}
		else if (i < 127)
		{
			sprintf(buf, "%c", i);
		}
		else
		{
			sprintf(buf, "\\x%c%c", hexsym[i/16], hexsym[i%16]);
		}

		if (s)
		{
			ascii_text_conv[i] = s;
			ascii_text_conv[i+256] = string_make(buf);
		}
		else
		{
			ascii_text_conv[i] = ascii_text_conv[i+256] = string_make(buf);
		}
	}
}

/*
 * Hack -- convert a printable string into real ascii
 *
 * Format:
 * "%v", text_to_ascii_f1, (cptr)str
 */
void text_to_ascii_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	cptr str = va_arg(*vp, cptr);
	char *s = buf;
	int i;

	for (; *str && s+MAX_ASCII_LEN < buf+max-1; )
	{
		for (i = 0; i < 512; i++)
		{
			if (prefix(str, ascii_text_conv[i]))
			{
				*s++ = i % 256;
				str += strlen(ascii_text_conv[i]);
				goto next;
			}
		}
		/* Paranoia - an unrecognised sequence. */
		*s++ = *str++;
next:
		continue;
	}

	/* Finish the string off. */
	*s = '\0';
}

/*
 * Hack - convert a string into a printable form.
 *
 * Format:
 * "%v", ascii_to_text_f1, (cptr)str
 */
void ascii_to_text_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	cptr str = va_arg(*vp, cptr);
	char *s = buf;

	/* Analyze the "ascii" string */
	for (; *str && s+MAX_ASCII_LEN < buf+max-1; str++)
	{
		s += sprintf(s, "%s", ascii_text_conv[(byte)*str]);
	}
}

void s16b_to_string_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	char str[4];

	int cmd = va_arg(*vp, int);

	/* Paranoia - ignore very high bits. */
	cmd &= 0xFFFF;

	if (cmd & 0xFF00)
	{
		sprintf(str, "&%c%c", ((cmd & 0xFF00) >> 8), (cmd & 0x00FF));
	}
	else
	{
		sprintf(str, "%c", cmd);
	}
	strnfmt(buf, max, "%v", ascii_to_text_f1, str);
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
sint macro_find_exact(cptr pat)
{
	int i;

	/* Nothing possible */
	if (!macro__use[(byte)(pat[0])])
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
static sint macro_find_check(cptr pat)
{
	int i;

	/* Nothing possible */
	if (!macro__use[(byte)(pat[0])])
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
static sint macro_find_maybe(cptr pat)
{
	int i;

	/* Nothing possible */
	if (!macro__use[(byte)(pat[0])])
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
static sint macro_find_ready(cptr pat)
{
	int i, t, n = -1, s = -1;

	/* Nothing possible */
	if (!macro__use[(byte)(pat[0])])
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
		FREE(macro__act[n]);
	}

	/* Create a new macro */
	else
	{
		/* Acquire a new index */
		n = macro__num++;

		/* Save the pattern */
		macro__pat[n] = string_make(pat);
	}

	/* Save the action */
	macro__act[n] = string_make(act);

	/* Efficiency */
	macro__use[(byte)(pat[0])] = TRUE;

	/* Success */
	return (0);
}



/*
 * Local "need flush" variable
 */
static bool inkey_xtra = FALSE;


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
 * Flush all input chars.  Actually, remember the flush,
 * and do a "special flush" before the next "inkey()".
 *
 * This is not only more efficient, but also necessary to make sure
 * that various "inkey()" codes are not "lost" along the way.
 */
void flush(void)
{
	/* Do it later */
	inkey_xtra = TRUE;
}


/*
 * Flush the screen, make a noise, give a message.
 * Hack - bell(0) means "no message", not format(0).
 */
void bell(cptr fmt, ...)
{
	/* Mega-Hack -- Flush the output */
	Term_fresh();

	/* Store a message if allowed and requested. */
	if (character_generated && fmt)
	{
		get_va_arg_string(fmt);
		message_add(format("ERROR: %s", format(0)));
	}

	/* Make a bell noise (if allowed) */
	if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);

	/* Flush the input if allowed (later!) */
	if (flush_error) flush();
}


/*
 * Hack -- Make a (relevant?) sound
 */
void sound(int val)
{
	/* No sound */
	if (!use_sound) return;

	/* Make a sound (if allowed) */
	Term_xtra(TERM_XTRA_SOUND, val);
}

/*
 * Check whether the screen is in a suitable mode for writing.
 * character_icky is set whenever the main map is not being displayed on
 * term_screen.
 * If Term is not term_screen, the wrong display is being written to.
 */
bool screen_is_icky(void)
{
	if (character_icky) return TRUE;
	if (Term != term_screen) return TRUE;
	return FALSE;
}


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
	int k = 0, n, p = 0, w = 0;

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
			Term_xtra(TERM_XTRA_DELAY, w);
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


	/* Access the macro action */
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

/*
 * *Hack* - a second such sequence which can be added from the game code
 * as necessary. This is checked before the above.
 */
static cptr inkey_gnext = NULL;

/*
 * Set inkey_gnext. Notice if it was already set, but don't do anything yet
 * as the appropriate course of action is not obvious.
 */
void set_gnext(cptr next)
{
	assert(next); /* Caller */

	if (!gnext_clear() && alert_failure) bell("inkey_gnext overlap error.");

	inkey_gnext = next;
}

bool gnext_clear(void)
{
	return (!inkey_gnext || !*inkey_gnext);
}

#ifdef ALLOW_BORG

/*
 * Mega-Hack -- special "inkey_hack" hook.  XXX XXX XXX
 *
 * This special function hook allows the "Borg" (see elsewhere) to take
 * control of the "inkey()" function, and substitute in fake keypresses.
 */
static char (*inkey_hack)(int flush_first) = NULL;

#endif /* ALLOW_BORG */


/*
 * Hack - test if the last keypress was be from a keymap.
 * This works because inkey_next is reset at the call after the last character
 * is removed from it.
 */
static bool is_keymap(void)
{
	return (inkey_next != 0);
}

/*
 * Hack - are there key ready to be processed?
 */
bool is_keymap_or_macro(void)
{
	char c;

	/* Keymap */
	if (is_keymap()) return TRUE;

	/* Macro */
	if (!Term_inkey(&c, FALSE, FALSE) && c & 0xE0) return TRUE;

	/* Nothing */
	return FALSE;
}

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
 * normal command, and we will only show the cursor if "hilite_player" is
 * TRUE (or if the player is in a store), instead of always showing the
 * cursor.  The various "main-xxx.c" files should avoid saving the game
 * in response to a "menu item" request unless "inkey_flag" is TRUE, to
 * prevent savefile corruption.
 *
 * If we are waiting for a keypress, and no keypress is ready, then we will
 * refresh (once) the window which was active when this function was called.
 *
 * Note that "back-quote" is automatically converted into "escape" for
 * convenience on machines with no "escape" key.  This is done after the
 * macro matching, so the user can still make a macro for "backquote".
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
 * If "term_screen" is not active, we will make it active during this
 * function, so that the various "main-xxx.c" files can assume that input
 * is only requested (via "Term_inkey()") when "term_screen" is active.
 *
 * Mega-Hack -- This function is used as the entry point for clearing the
 * "signal_count" variable, and of the "character_saved" variable.
 *
 * Hack -- Note the use of "inkey_next" to allow "keymaps" to be processed.
 * "inkey_next" is set to 0 on the turn after the last character is removed
 * from it.
 *
 * Mega-Hack -- Note the use of "inkey_hack" to allow the "Borg" to steal
 * control of the keyboard from the user.
 */
char inkey(void)
{
	bool v;

	char kk;

	char ch = 0;

	bool done = FALSE;

	term *old = Term;


	/* *Hack* - use the "inkey_gnext" pointer. */
	if (inkey_gnext && *inkey_gnext && !inkey_xtra)
	{
		/* Get next character, and advance */
		ch = *inkey_gnext++;

		/* Cancel the various "global parameters" */
		inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;

		/* Accept result */
		return (ch);
	}

	/* Forget pointer */
	inkey_gnext = NULL;

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
	if (inkey_hack && ((ch = (*inkey_hack)(inkey_xtra)) != 0))
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
		Term_flush();
	}


	/* Access cursor state */
	(void)Term_get_cursor(&v);

	/* Show the cursor if waiting, except sometimes in "command" mode */
	if (!inkey_scan && (!inkey_flag || hilite_player || screen_is_icky()))
	{
		/* Show the cursor */
		(void)Term_set_cursor(1);
	}


	/* Hack -- Activate main screen */
	Term_activate(term_screen);


	/* Get a key */
	while (!ch)
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
			/* Update windows if waiting. */
			window_stuff();

			/* Hack -- activate proper term */
			Term_activate(old);

			/* Flush output */
			Term_fresh();

			/* Hack -- activate main screen */
			Term_activate(term_screen);

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
					Term_xtra(TERM_XTRA_DELAY, w);
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
	}


	/* Hack -- restore the term */
	Term_activate(old);


	/* Restore the cursor */
	Term_set_cursor(v);


	/* Cancel the various "global parameters" */
	inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;


	/* Return the keypress */
	return (ch);
}




/*
 * We use a global array for all inscriptions to reduce the memory
 * spent maintaining inscriptions.  Of course, it is still possible
 * to run out of inscription memory, especially if too many different
 * inscriptions are used, but hopefully this will be rare.
 *
 * We use dynamic string allocation because otherwise it is necessary
 * to pre-guess the amount of quark activity.  We limit the total
 * number of quarks, but this is much easier to "expand" as needed.
 *
 * Any two items with the same inscription will have the same "quark"
 * index, which should greatly reduce the need for inscription space.
 *
 * Note that "quark zero" is NULL and should not be "dereferenced".
 */

/* The number of quarks */
static u16b quark__num;



/*
 * Add a new "quark" to the set of quarks.
 */
u16b quark_add(cptr str)
{
	uint i;

	/* Look for an existing quark */
	for (i = 1; i < quark__num; i++)
	{
		/* Check for equality */
		if (streq(quark__str[i], str)) return (i);
	}

	/* Paranoia -- Require room */
	if (quark__num == QUARK_MAX) return (0);

	/* New maximal quark */
	quark__num = i + 1;

	/* Add a new quark */
	quark__str[i] = string_make(str);

	/* Return the index */
	return (i);
}


/*
 * This function looks up a quark
 */
cptr quark_str(u16b i)
{
	cptr q;

	/* Verify */
	if (i >= quark__num)
		quit("Out of bounds quark string error.");

	/* Access the quark */
	q = quark__str[i];

	/* Return the quark */
	return (q);
}




/*
 * Second try for the "message" handling routines.
 *
 * Each call to "message_add(s)" will add a new "most recent" message
 * to the "message recall list", using the contents of the string "s".
 *
 * The messages will be stored in such a way as to maximize "efficiency",
 * that is, we attempt to maximize the number of sequential messages that
 * can be retrieved, given a limited amount of storage space.
 *
 * We keep a buffer of chars to hold the "text" of the messages, not
 * necessarily in "order", and an array of offsets into that buffer,
 * representing the actual messages.  This is made more complicated
 * by the fact that both the array of indexes, and the buffer itself,
 * are both treated as "circular arrays" for efficiency purposes, but
 * the strings may not be "broken" across the ends of the array.
 *
 * The "message_add()" function is rather "complex", because it must be
 * extremely efficient, both in space and time, for use with the Borg.
 */

static u16b message__next; /* The next "free" index to use */
static u16b message__last; /* The index of the oldest message. */
static u16b message__head; /* The next "free" offset */
static u16b message__tail = MESSAGE_BUF; /* The oldest used char offset */


/*
 * How many messages are "available"?
 */
s16b message_num(void)
{
	int last, next, n;

	/* Extract the indexes */
	last = message__last;
	next = message__next;

	/* Handle "wrap" */
	if (next < last) next += MESSAGE_MAX;

	/* Extract the space */
	n = (next - last);

	/* Return the result */
	return (n);
}



/*
 * Recall the "text" of a saved message
 */
cptr message_str(s16b age)
{
	s16b x;
	s16b o;
	cptr s;

	/* Forgotten messages have no text */
	if ((age < 0) || (age >= message_num())) return ("");

	/* Acquire the "logical" index */
	x = (message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX;

	/* Get the "offset" for the message */
	o = message__ptr[x];

	/* Access the message text */
	s = &message__buf[o];

	/* Return the message text */
	return (s);
}



/*
 * Add a new message, with great efficiency
 */
/*
 * Add a new message, with great efficiency
 */
void message_add(cptr str)
{
	int i, k, x, m, n;

	char u[1024];

	/*** Step 1 -- Analyze the message ***/

	/* Hack -- Ignore "non-messages" */
	if (!str) return;

	/* Message length */
	n = strlen(str);

	/* Important Hack -- Ignore "long" messages */
	if (n >= MESSAGE_BUF / 4) return;

	/* Window stuff, now we know the message will be printed. */
	p_ptr->window |= PW_MESSAGE;


	/*** Step 2 -- Attempt to optimize ***/

	/* Limit number of messages to check */
	m = message_num();

	k = m / 4;

	/* Limit number of messages to check */
	if (k > MESSAGE_MAX / 32) k = MESSAGE_MAX / 32;

	/* Check previous message */
	for (i = message__next; m; m--)
	{
		int j = 1;

		char buf[1024];
		char *t;

		cptr old;

		/* Back up and wrap if needed */
		if (i-- == 0) i = MESSAGE_MAX - 1;

		/* Access the old string */
		old = &message__buf[message__ptr[i]];

		/* Skip small messages */
		if (!old) continue;

		strcpy(buf, old);

		/* Find multiple */
		for (t = buf; *t && (*t != '<'); t++) ;

		if (*t)
		{
			/* Message is too small */
			if (t - buf < 2) break;

			/* Drop the space */
			*(t - 1) = '\0';

			/* Get multiplier */
			j = atoi(++t);
		}

		/* Limit the multiplier to 1000 */
		if (buf && streq(buf, str) && (j < 1000))
		{
			j++;

			/* Overwrite */
			message__next = i;

			str = u;

			/* Write it out */
			sprintf(u, "%s <%dx>", buf, j);

			/* Message length */
			n = strlen(str);
		}

		/* Done */
		break;
	}

	/* Check the last few messages (if any to count) */
	for (i = message__next; k; k--)
	{
		u16b q;

		cptr old;

		/* Back up and wrap if needed */
		if (i-- == 0) i = MESSAGE_MAX - 1;

		/* Stop before oldest message */
		if (i == message__last) break;

		/* Extract "distance" from "head" */
		q = (message__head + MESSAGE_BUF - message__ptr[i]) % MESSAGE_BUF;

		/* Do not optimize over large distance */
		if (q > MESSAGE_BUF / 2) continue;

		/* Access the old string */
		old = &message__buf[message__ptr[i]];

		/* Compare */
		if (!streq(old, str)) continue;

		/* Get the next message index, advance */
		x = message__next++;

		/* Handle wrap */
		if (message__next == MESSAGE_MAX) message__next = 0;

		/* Kill last message if needed */
		if (message__next == message__last) message__last++;

		/* Handle wrap */
		if (message__last == MESSAGE_MAX) message__last = 0;

		/* Assign the starting address */
		message__ptr[x] = message__ptr[i];

		/* Success */
		return;
	}


	/*** Step 3 -- Ensure space before end of buffer ***/

	/* Kill messages and Wrap if needed */
	if (message__head + n + 1 >= MESSAGE_BUF)
	{
		/* Kill all "dead" messages */
		for (i = message__last; TRUE; i++)
		{
			/* Wrap if needed */
			if (i == MESSAGE_MAX) i = 0;

			/* Stop before the new message */
			if (i == message__next) break;

			/* Kill "dead" messages */
			if (message__ptr[i] >= message__head)
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


	/*** Step 4 -- Ensure space before next message ***/

	/* Kill messages if needed */
	if (message__head + n + 1 > message__tail)
	{
		/* Grab new "tail" */
		message__tail = message__head + n + 1;

		/* Advance tail while possible past first "nul" */
		while (message__buf[message__tail-1]) message__tail++;

		/* Kill all "dead" messages */
		for (i = message__last; TRUE; i++)
		{
			/* Wrap if needed */
			if (i == MESSAGE_MAX) i = 0;

			/* Stop before the new message */
			if (i == message__next) break;

			/* Kill "dead" messages */
			if ((message__ptr[i] >= message__head) &&
				(message__ptr[i] < message__tail))
			{
				/* Track oldest message */
				message__last = i + 1;
			}
		}
	}


	/*** Step 5 -- Grab a new message index ***/

	/* Get the next message index, advance */
	x = message__next++;

	/* Handle wrap */
	if (message__next == MESSAGE_MAX) message__next = 0;

	/* Kill last message if needed */
	if (message__next == message__last) message__last++;

	/* Handle wrap */
	if (message__last == MESSAGE_MAX) message__last = 0;



	/*** Step 6 -- Insert the message text ***/

	/* Assign the starting address */
	message__ptr[x] = message__head;

	/* Append the new part of the message */
	for (i = 0; i < n; i++)
	{
		/* Copy the message */
		message__buf[message__head + i] = str[i];
	}

	/* Terminate */
	message__buf[message__head + i] = '\0';

	/* Advance the "head" pointer */
	message__head += n + 1;
}

#define MORE_STR "-more-"

/*
 * Hack -- flush
 */
static void msg_flush(int x)
{
	/* Pause for response */
	mc_put_fmt(0, x, "$B" MORE_STR);

	/* Get an acceptable keypress */
	while (!auto_more)
	{
		int cmd = inkey();
		if (quick_messages) break;
		if ((cmd == ESCAPE) || (cmd == ' ')) break;
		if ((cmd == '\n') || (cmd == '\r')) break;
		bell(0);
	}

	/* Clear the line */
	Term_erase(0, 0, 255);
}


/*
 * Hack - prevent messages from being printed or remembered until further
 * notice.
 */
bool no_msg_print = FALSE;

/*
 * Output a message to the top line of the screen.
 *
 * Break long messages into multiple pieces (40-lim chars).
 *
 * Allow multiple short messages to "share" the top line.
 *
 * Prompt the user to make sure he has a chance to read them.
 *
 * These messages are memorized for later reference (see above).
 *
 * We could do "Term_fresh()" to provide "flicker" if needed.
 *
 * The global "msg_flag" variable can be cleared to tell us to
 * "erase" any "pending" messages still on the screen.
 *
 * XXX XXX XXX Note that we must be very careful about using the
 * "msg_print()" functions without explicitly calling the special
 * "msg_print(NULL)" function, since this may result in the loss
 * of information if the screen is cleared, or if anything is
 * displayed on the top line.
 *
 * XXX XXX XXX Note that "msg_print(NULL)" will clear the top line
 * even if no messages are pending.  This is probably a hack.
 */
void msg_print(cptr msg)
{
	static int p = 0;

	int n;

	const int lim = Term->wid-strlen(MORE_STR)-2;

	char *t;

	char buf[1024];

	/* *Hack* - disable. */
	if (no_msg_print) return;

	/* Hack -- Reset */
	if (!msg_flag) p = 0;

	/* Message Length */
	n = (msg ? strlen(msg) : 0);

	/* Hack -- flush when requested or needed */
	if (p && (!msg || ((p + n) > lim)))
	{
		/* Flush */
		msg_flush(p);

		/* Forget it */
		msg_flag = FALSE;

		/* Reset */
		p = 0;
	}


	/* No message */
	if (!msg) return;

	/* Paranoia */
	if (n > 1000) return;


	/* Memorize the message */
	if (character_generated) message_add(msg);

	/* Copy it */
	strcpy(buf, msg);

	/* Analyze the buffer */
	t = buf;

	/* Split message */
	while (n > lim)
	{
		char oops;

		int check, split;

		/* Default split */
		split = lim;

		/* Find the "best" split point */
		for (check = 40; check < lim; check++)
		{
			/* Found a valid split point */
			if (t[check] == ' ') split = check;
		}

		/* Save the split character */
		oops = t[split];

		/* Split the message */
		t[split] = '\0';

		/* Display part of the message */
		mc_put_fmt(0, 0, "%.*s", split, t);

		/* Flush it */
		msg_flush(split + 1);

		/* Memorize the piece */
		/* if (character_generated) message_add(t); */

		/* Restore the split character */
		t[split] = oops;

		/* Insert a space */
		t[--split] = ' ';

		/* Prepare to recurse on the rest of "buf" */
		t += split; n -= split;
	}


	/* Display the tail of the message */
	mc_put_fmt(0, p, "%s", t);

	/* Memorize the tail */
	/* if (character_generated) message_add(t); */

	/* Remember the message */
	msg_flag = TRUE;

	/* Remember the position */
	p += n + 1;

	/* Optional refresh */
	if (fresh_message) Term_fresh();
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
	(void)vstrnfmt(buf, 1024, fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	msg_print(buf);
}



/*
 * Display a string on the screen using an attribute.
 *
 * At the given location, using the given attribute, if allowed,
 * add the given string.  Do not clear the line.
 */
void c_put_str(byte attr, cptr str, int row, int col)
{
	/* Position cursor, Dump the attr/text */
	Term_putstr(col, row, -1, attr, str);
}

/*
 * As above, but in "white"
 */
void put_str(cptr str, int row, int col)
{
	/* Spawn */
	c_put_str(TERM_WHITE, str, row, col);
}



/*
 * Display a string on the screen in white, and clear
 * to the end of the line.
 */
void prt(cptr str, int row, int col)
{
	/* Clear line, position cursor */
	Term_erase(col, row, 255);

	/* Dump the attr/text */
	Term_addstr(-1, TERM_WHITE, str);
}




/*
 * Move the cursor to the beginning of the next line and clear it.
 * Return FALSE if already on the bottom line.
 */
static bool next_line(int nx)
{
	int x,y;
	Term_locate(&x, &y);
	return (!Term_erase(nx, y+1, 255));
}

/*
 * Wrap the text on the current line as the cursor.
 * Return FALSE on errors.
 */
static bool wrap_text(int nx)
{
	int x, y, w, h, i, n;

	byte av[256];
	char cv[256];

	Term_get_size(&w, &h);
	Term_locate(&x, &y);

	/* Scan existing text */
	for (i = w - 1, n = 0; i >= nx; i--)
	{
		/* Grab existing attr/char */
		Term_what(i, y, &av[i], &cv[i]);

		/* Break on space */
		if (cv[i] == ' ') break;

		/* Track current word */
		n = i;
	}

	/* Special case */
	if (n == nx) n = w;

	/* Clear the partial word. */
	Term_erase(n, y, 255);

	/* Clear line, move cursor. Return if this is not possible. */
	if (Term_erase(nx, y+1, 255)) return FALSE;

	/* Wrap the word (if any) */
	for (i = n; i < w; i++)
	{
		/* Dump */
		Term_addch(av[i], cv[i]);
	}

	return TRUE;
}


#define DEFAULT -1

/*
 * Print a multi-coloured string where colour-changes are denoted as follows:
 *
 * $d $w $s $o $r $g $b $u : Change to the specified colour.
 * $D $W $v $y $R $G $B $U
 *                      $< : Save a "default" colour.
 *                      $> : Restore the "default" colour.
 *                      $$ : Print a literal $.
 *                      $! : Ignore codes for the remainder of the call.
 *
 * Anything else is printed directly.
 *
 * This function returns under four circumstances:
 *
 * 1. The cursor goes out of bounds (returns the first unprinted character).
 * 2. The end of the string is reached (returns the \0).
 * 3. A \n is found (returns the \n).
 */
static cptr mc_add(cptr s, int mx, int *dattr, int *attr, bool *ignore)
{
	int nattr;

	/* With no specified width, use however much space is left on the line. */
	if (mx == DEFAULT)
	{
		int cx, y;
		Term_get_size(&mx, &y);
		Term_locate(&cx, &y);

		mx -= cx;
	}

	/* Print until either the space or the string is exhausted. */
	for (; *s && *s != '\n' && mx; s++)
	{
		if (*ignore || *s != '$')
		{
			/* Add the character, finish if the cursor has gone too far. */
			Term_addch(*attr, *s);
			mx--;
		}
		/* $$ prints $. */
		else if (*(++s) == '$')
		{
			Term_addch(*attr, '$');
			mx--;
		}
		/* $< saves the current colour as a default. */
		else if (*s == '<')
		{
			*dattr = *attr;
		}
		/* $> loads the default colour. */
		else if (*s == '>')
		{
			*attr = *dattr;
		}
		/* $! forces further $ combinations to be ignored. */
		else if (*s == '!')
		{
			*ignore = TRUE;
		}
		/* A specific colour request. */
		else if (((nattr = color_char_to_attr(*s))) != -1)
		{
			*attr = nattr;
		}
		/* An incorrect request gives a warning. */
		else
		{
			Term_addch(TERM_RED, '$');
			mx--;
		}
	}
	return s;
}

/*
 * Print a multi-coloured string using the above routine, automatically
 * wrapping the text at spaces as necessary to avoid breaking a word between
 * lines. Characters to the left of column x are ignored at all points.
 */
static void mc_roff_aux(int x, cptr s)
{
	int dattr = TERM_WHITE, attr = TERM_WHITE;
	bool ignore = FALSE;

	while (*((s = mc_add(s, DEFAULT, &dattr, &attr, &ignore))))
	{
		if (strchr(" \n", *s))
		{
			s++;
			if (!next_line(x)) return;
		}
		else
		{
			if (!wrap_text(x)) return;
		}
	}
}

/*
 * As above, starting at the specified position and wrapping to x.
 */
void mc_roff_xy(int x, int y, cptr s)
{
	Term_gotoxy(x, y);
	mc_roff_aux(x, s);
}

/*
 * Print a string (as above) which starts in a particular colour and wraps to
 * the left margin.
 */
void c_roff(byte a, cptr str)
{
	char buf[1024];
	sprintf(buf, "$%c$!", atchar[a]);
	while (*str)
	{
		str += sprintf(buf+4, "%.1019s", str);
		mc_roff_aux(0, buf);
	}
}

/*
 * Write a line of (possibly multicolour) text to the screen using mc_add()
 * above. The maximum of characters printed will be l, or as many as will fit
 * if l is DEFAULT.
 */
static void mc_add_str_aux(const int l, cptr str)
{
	int attr, dattr = attr = TERM_WHITE;
	bool ignore = FALSE;
	mc_add(str, l, &dattr, &attr, &ignore);
}

void mc_put_str(const int y, const int x, cptr str)
{
	if (Term_gotoxy(x, y)) return;
	mc_add_str_aux(DEFAULT, str);
}

/*
 * Write a line of formatted multicolour text at the current cursor location
 * using mc_add().
 */
void mc_add_fmt(cptr fmt, ...)
{
	/* Get the string. */
	char str[257];
	get_va_arg_buf(str, fmt);

	/* Print it. */
	mc_add_str_aux(DEFAULT, str);
}

/*
 * Write a line of formatted multicolour text to the screen using mc_add().
 * Note that the format string is decoded before the colour string.
 */
void mc_put_fmt(const int y, const int x, cptr fmt, ...)
{
	/* The screen can only be 256 characters wide, so this is enough. */
	char buf[257];

	/* Print it. */
	get_va_arg_buf(buf, fmt);
	mc_put_str(y, x, buf);
}

/*
 * Hack - enter a coloured formatted string into a field of a specified
 * width.
 *
 * This takes the width as a separate parameter (which should be turned into
 * a general "precision" specifier), and acts as if the last used parameter
 * was clear_f0 without an explicit call.
 * This makes sense as the function is only used in one way at present.
 */
void mc_put_lfmt(const int y, const int x, const int l, cptr fmt, ...)
{
	/* The screen can only be 256 characters wide, so this is enough. */
	char buf[257], *t;

	/* Hack - allow the caller to request a limit. */
	get_va_arg_buf(buf, fmt);

	/* Hack - add a lot of space to the end of buf as an implicit clear_f0. */
	for (t = strchr(buf, '\0'); t < END_PTR(buf)-1; t++) *t = ' ';
	*t = '\0';

	if (Term_gotoxy(x, y)) return;
	mc_add_str_aux(l, buf);
}


/*
 * Clear part of the screen
 */
void clear_from(int row)
{
	int y;

	/* Erase requested rows */
	for (y = row; y < Term->hgt; y++)
	{
		/* Erase part of the screen */
		Term_erase(0, y, 255);
	}
}




/*
 * Get some input at the cursor location.
 * Assume the buffer is initialized to a default string.
 * Note that this string is often "empty" (see below).
 * The default buffer is displayed in yellow until cleared or made the current
 * string.
 * Pressing RETURN right away accepts the default entry.
 * Normal chars clear the default and append the char.
 * Backspace clears the default or deletes the char before the cursor.
 * ESCAPE clears the buffer and the window and returns FALSE.
 * RETURN accepts the current buffer contents and returns TRUE.
 * Tab makes the default the current string.
 * If entered via a macro:
 * 4 moves the cursor one place to the left.
 * 6 moves the cursor one place to the right.
 * 7 moves the cursor to the beginning of the string.
 * 1 moves the cursor to the end of the string.
 * . deletes the char under the cursor.
 */
bool askfor_aux(char *buf, int len)
{
	int y, x;

	int i = 0, j;

	int k = 0, l = 0;

	bool done = FALSE, edit_mode = FALSE;


	/* Locate the cursor */
	Term_locate(&x, &y);


	/* Paranoia -- check len */
	if (len < 1) len = 1;

	/* Paranoia -- check column */
	if ((x < 0) || (x >= 80)) x = 0;

	/* Restrict the length */
	if (x + len > 80) len = 80 - x;


	/* Paranoia -- Clip the default entry */
	buf[len] = '\0';


	/* Display the default answer */
	mc_put_fmt(y, x, "$y$!%-*s", len, buf);

	/* Reset edit_mode to ensure that each prompt is the same. */
	if (!macro_edit) edit_mode = FALSE;

	/* Give help, if requested. */
	help_track("string_prompt_det");

	/* Process input */
	while (!done)
	{
		/* Place cursor */
		Term_gotoxy(x + l, y);

		/* Get a key */
		i = inkey();

		/* Use macros to denote edit mode if allowed. */
		if (macro_edit) edit_mode = parse_macro;

		/* Analyze the key */
		switch (i)
		{
			case ESCAPE:
			k = 0;
			done = TRUE;
			break;

			case '\n':
			case '\r':
			k = strlen(buf);
			done = TRUE;
			break;

			case 0x7F:
			case '\010':
			if (l > 0)
			{
				k--;
				l--;
				for (j = l; j < k; j++)
					buf[j] = buf[j+1];
			}
			break;

			case '\t':
			if (!k)
			{
				k = strlen(buf);
				l = k;
			}
			if (!macro_edit) edit_mode = !edit_mode;
			break;

			/*
			 * Hack - parse 1, 4, 6, 7 and . as editing keys if
			 * inside a macro. These must be before default: as
			 * they pass through to it otherwise.
			 */
			case '1':
			if (edit_mode)
			{
				l = k;
				break;
			}
			case '4':
			if (edit_mode)
			{
				if (l > 0) l--;
			break;
			}
			case '6':
			if (edit_mode)
			{
				if (l < k) l++;
				break;
			}
			case '7':
			if (edit_mode)
			{
				l = 0;
				break;
			}
			case '.':
			if (edit_mode)
			{
				if (l < k)
				{
					k--;
					for (j = l; j < k; j++)
						buf[j] = buf[j+1];
				}
				break;
			}

			/* Parse normall if the above are not macros. */

			default:
			if ((k < len) && (ISPRINT(i)))
			{
				for (j = k; j >= l; j--)
					buf[j+1] = buf[j];
				buf[l++] = i;
				k++;
			}
			else
			{
				bell(0);
			}
			break;
		}

		/* Terminate */
		buf[k] = '\0';

		/* Update the entry */
		mc_put_fmt(y, x, "$!%-*s", len, buf);
	}

	/* Remove help. */
	help_track(NULL);

	/* Aborted */
	if (i == ESCAPE) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Get a string from the user
 *
 * The "prompt" should take the form "Prompt: "
 *
 * Note that the initial contents of the string is used as
 * the default response, so be sure to "clear" it if needed.
 *
 * We clear the input, and return FALSE, on "ESCAPE".
 */
bool get_string(cptr prompt, char *buf, int len)
{
	bool res;

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);

	/* Display prompt */
	prt(prompt, 0, 0);

	/* Ask the user for a string */
	res = askfor_aux(buf, len);

	/* Clear prompt */
	prt("", 0, 0);

	/* Result */
	return (res);
}



/*
 * Verify something with the user
 *
 * This function returns a character (controlled by conv_to), and places a
 * message which represents the prompt in the format buffer for further use.
 *
 * Prompt should be the question to be asked of the user.
 *
 * text should be a prompt of the form "...%.*s...%s%v", where the prompt given
 * will be ...<prompt>..., and the message returned will be
 * ...<prompt>... <keypress>
 *
 * conv_from contains the list of valid keypresses.
 *
 * conv_to contains the list of what should be returned when each key in
 * conv_from is pressed.
 *
 * Hack - if quick_prompt is set, and an otherwise invalid key is pressed, the
 * first element of conv_to is returned.
 */
char get_check_aux(cptr prompt, cptr text, cptr conv_from, cptr conv_to)
{
	char i[2]=" ", buf[257];
	cptr c;

	/* The format buffer is likely to be overwritten. */
	if (prompt == format(0))
	{
		/* Limit to the maximum screen width for X11. */
		sprintf(buf, "%.256s", prompt);
		prompt = buf;
	}

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);

	/* Prompt for it (should "? " be added to long prompts?). */
	mc_put_fmt(0, 0, text, Term->wid-strlen(text)+5, prompt, "", clear_f0);

	/* Get an acceptable answer */
	while (TRUE)
	{
		*i = inkey();
		if ((c = strchr(conv_from, *i))) break;
		else if (quick_prompt)
		{
			c = conv_from;
			break;
		}
		bell("Illegal response to a prompt");
	}

	/* Erase the prompt */
	prt("", 0, 0);

	/* Leave a record somewhere convenient. */
	format(text, Term->wid-strlen(text)+5, prompt, " ", ascii_to_text_f1, i);

	/* Tell the calling routine */
	return conv_to[c - conv_from];
}

/*
 * Verify something with the user as above.
 *
 * The "prompt" should take the form "Query? "
 *
 * Note that "[y/n]" is appended to the prompt.
 *
 */
bool get_check(cptr prompt)
{
	char rc;

	/* Help */
	help_track("yn_prompt");

	rc = get_check_aux(prompt, "$!%.*s[y/n]%s%v", "nN\033yY\r", "\0\0\0\1\1\1");

	/* Leave a message. */
	message_add(format(0));

	/* Done with help */
	help_track(NULL);

	return (rc != '\0');
}


/*
 * Prompts for a keypress
 *
 * The "prompt" should take the form "Command: "
 *
 * Returns TRUE unless the character is "Escape"
 */
bool get_com(char *command, cptr fmt, ...)
{
	char prompt[257];

	get_va_arg_buf(prompt, fmt);

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);

	/* Display a prompt */
	prt(prompt, 0, 0);

	/* Get a key */
	*command = inkey();

	/* Clear the prompt */
	prt("", 0, 0);

	/* Handle "cancel" */
	if (*command == ESCAPE) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Request a "quantity" from the user
 *
 * Hack -- allow "command_arg" to specify a quantity
 */
s16b get_quantity(cptr prompt, int max,bool allbydefault)
{
	int amt;

	char tmp[80];

	char buf[80];


	/* Use "command_arg" */
	if (command_arg)
	{
		/* Extract a number */
		amt = command_arg;

		/* Clear "command_arg" */
		command_arg = 0;

		/* Enforce the maximum */
		if (amt > max) amt = max;

		/* Use it */
		return (amt);
	}

#ifdef ALLOW_REPEAT

	/* Get the item index */
	if ((max != 1) && repeat_pull(&amt)) {

		/* Enforce the maximum */
		if (amt > max) amt = max;

		/* Enforce the minimum */
		if (amt < 0) amt = 0;

		/* Use it */
		return (amt);
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Build a prompt if needed */
	if (!prompt)
	{
		/* Build a prompt */
		sprintf(tmp, "Quantity (1-%d): ", max);

		/* Use that prompt */
		prompt = tmp;
	}


	/* Default to one */
	amt =1;
	/* unless allbydefault is true */
	if(allbydefault) amt=max;

	/* Build the default */
	sprintf(buf, "%d", amt);

	/* Ask for a quantity */
	if (!get_string(prompt, buf, 6)) return (0);

	/* Extract a number */
	amt = atoi(buf);

	/* A letter means "all" */
	if (ISALPHA(buf[0])) amt = max;

	/* Enforce the maximum */
	if (amt > max) amt = max;

	/* Enforce the minimum */
	if (amt < 0) amt = 0;

#ifdef ALLOW_REPEAT

	if (amt) repeat_push(amt);

#endif /* ALLOW_REPEAT -- TNB */


	/* Return the result */
	return (amt);
}


/*
 * Pause for user response XXX XXX XXX
 */
static void pause_line_aux(int row)
{
	int i;
	prt("", row, 0);
	put_str("[Press any key to continue]", row, 23);
	i = inkey();
	prt("", row, 0);
}

void pause_line(void)
{
	pause_line_aux(Term->hgt - 1);
}

/*
 * Find the keymap mode.
 */
int keymap_mode(void)
{
	/* Roguelike */
	if (rogue_like_commands)
	{
		return KEYMAP_MODE_ROGUE;
	}

	/* Original */
	else
	{
		return KEYMAP_MODE_ORIG;
	}
}

/*
 * Find a keymap from its trigger.
 */
static cptr get_keymap(char trigger)
{
	return keymap_act[keymap_mode()][(byte)trigger];
}


/*
 * Request a command from the user.
 *
 * Sets p_ptr->command_cmd, p_ptr->command_dir,
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
 * Note that "p_ptr->command_new" may not work any more.  XXX XXX XXX
 */
void request_command(bool shopping)
{
	int i, cmd;

	cptr act;



	/* No "argument" yet (exclude special modes). */
	if (!(command_new & 0xFF00)) command_arg = 0;

	/* No "direction" yet */
	command_dir = 0;


	/* Get command */
	while (1)
	{
		char cmd_char;

		/* Hack -- auto-commands */
		if (command_new)
		{
			/* Flush messages */
			msg_print(NULL);

			/* Use auto-command */
			cmd = command_new;

			/* Forget it */
			command_new = 0;
		}

		/* Get a keypress in "command" mode */
		else
		{
			/* Hack -- no flush needed */
			msg_flag = FALSE;

			/* Activate "command mode" */
			inkey_flag = TRUE;

			/* Get a command */
			cmd = inkey();
		}

		/* Clear top line */
		prt("", 0, 0);

		/* There can be only one argument. */
		if (cmd == '0' && command_arg)
		{
			bell("Command count already present.");
		}
		/* Command Count */
		else if (cmd == '0')
		{
			/* Reset */
			int arg = 0;

			/* Begin the input */
			prt("Count: ", 0, 0);

			/* Get a command count */
			while (1)
			{
				/* Get a new keypress */
				cmd = inkey();

				/* Simple editing (delete or backspace) */
				if ((cmd == 0x7F) || (cmd == KTRL('H')))
				{
					/* Delete a digit */
					arg /= 10;

					/* Show current count */
					prt(format("Count: %d", arg), 0, 0);
				}

				/* Actual numeric data */
				else if (cmd >= '0' && cmd <= '9')
				{
					/* Stop count at 9999 */
					if (arg >= 1000)
					{
						/* Warn */
						bell(0);

						/* Limit */
						arg = 9999;
					}

					/* Increase count */
					else
					{
						/* Incorporate that digit */
						arg = arg * 10 + D2I(cmd);
					}

					/* Show current count */
					mc_put_fmt(0, 0, "Count: %d%v", arg, clear_f0);
				}

				/* Exit on "unusable" input */
				else
				{
					break;
				}
			}

			/* Hack -- Handle "zero" */
			if (!arg)
			{
				/* Default to 99 */
				arg = 99;
			}

			/* Show current count */
			mc_put_fmt(0, 0, "Count: %d%v", arg, clear_f0);

			/* Hack -- white-space means "enter command now" */
			if ((cmd == ' ') || (cmd == '\n') || (cmd == '\r'))
			{
				/* Get a real command, or forget this. */
				if (!get_com(&cmd_char, "Command: ")) continue;
				cmd = cmd_char;
			}

			/* Accept the argument. */
			command_arg = arg;
		}


		/* Allow "keymaps" to be bypassed */
		if (cmd == '\\')
		{
			/* Get a real command */
			(void)get_com(&cmd_char, "Command: ");
			cmd = cmd_char;

			/* Hack -- bypass keymaps */
			if (!inkey_next) inkey_next = "";
		}


		/* Allow "control chars" to be entered */
		if (cmd == '^')
		{
			/* Get a new command and controlify it */
			if (get_com(&cmd_char, "Control: ")) cmd = KTRL(cmd_char);
		}


		/* Look up applicable keymap if allowed. */
		if (!(cmd & 0xFF00) && !inkey_next)
		{
			act = get_keymap(cmd);

			/* Apply keymap if not inside a keymap already */
			if (act)
			{
				/* Hack - store the current keymap here. */
				static char request_command_buffer[256];

				/* Install the keymap (limited buffer size) */
				strnfmt(request_command_buffer, 256, "%s", act);

				/* Start using the buffer */
				inkey_next = request_command_buffer;

				continue;
			}
		}

		/* Found a command, so finish. */
		if (cmd) break;
	}

	/* Use the command */
	command_cmd = cmd;

	/* Hack -- Auto-repeat certain commands */
	if (always_repeat && (command_arg <= 0))
	{
		/* Hack -- auto repeat certain commands */
		if (!(command_cmd & 0xFF00) && strchr("TBDoc+", command_cmd))
		{
			/* Repeat 99 times */
			command_arg = 99;
		}
	}


	/* Shopping */
	if (shopping)
	{
		/* Hack -- Convert a few special keys */
		switch (command_cmd)
		{
			/* Command "p" -> "purchase" (get) */
			case 'p': command_cmd = 'g'; break;

			/* Command "m" -> "purchase" (get) */
			case 'm': command_cmd = 'g'; break;

			/* Command "s" -> "sell" (drop) */
			case 's': command_cmd = 'd'; break;
		}
	}


	/* Hack -- Scan equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		cptr s;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Obtain the inscription, if any. */
		s = get_inscription(o_ptr);

		/* Find a '^' */
		s = strchr(s, '^');

		/* Process preventions */
		while (s)
		{
			/* Check the "restriction" character */
			if ((s[1] == command_cmd) || (s[1] == '*'))
			{
				/* Hack -- Verify command */
				if (!get_check("Are you sure? "))
				{
					/* Hack -- Use space */
					command_cmd = ' ';
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
 * Check a char for "vowel-hood"
 */
bool is_a_vowel(int ch)
{
	switch (ch)
	{
		case 'a':
		case 'e':
		case 'i':
		case 'o':
		case 'u':
		case 'A':
		case 'E':
		case 'I':
		case 'O':
		case 'U':
		return (TRUE);
	}

	return (FALSE);
}




/*
 * Extract a direction from the keymap of the key pressed.
 */
int get_keymap_dir(char ch)
{
	cptr act, s;
	char tmp[2] = " ";
	int d = 0;

	/* If this character was generated by a keymap, avoid transforming
	 * it again. */
	if (is_keymap())
	{
		tmp[0] = ch;
		act = tmp;
	}
	else
	{
		act = get_keymap(ch);
	}

	if (act)
	{
		/* Convert to a direction */
		for (s = act; *s; ++s)
		{
			/* Use any digits in keymap */
			if (ISDIGIT(*s)) d = D2I(*s);
		}
	}
	return d;
}


#ifdef ALLOW_REPEAT

#define REPEAT_MAX 20

/* Number of chars saved */
static int repeat__cnt = 0;

/* Current index */
static int repeat__idx = 0;

/* Saved "stuff" */
static int repeat__key[REPEAT_MAX];

void repeat_push(int what)
{
	/* Too many keys */
	if (repeat__cnt == REPEAT_MAX) return;

	/* Push the "stuff" */
	repeat__key[repeat__cnt++] = what;

	/* Prevents us from pulling keys */
	++repeat__idx;
}

bool repeat_pull(int *what)
{
	/* All out of keys */
	if (repeat__idx == repeat__cnt) return (FALSE);

	/* Grab the next key, advance */
	*what = repeat__key[repeat__idx++];

	/* Success */
	return (TRUE);
}

void repeat_check(void)
{
	int what;

	/* Ignore some commands */
	if (command_cmd == ESCAPE) return;
	if (command_cmd == ' ') return;
	if (command_cmd == '\r') return;
	if (command_cmd == '\n') return;

	/* Repeat Last Command */
	if (command_cmd == 'n') {

		/* Reset */
		repeat__idx = 0;

		/* Get the command */
		if (repeat_pull(&what)) {

			/* Save the command */
			command_cmd = what;
		}
	}

	/* Start saving new command */
	else {

		/* Reset */
		repeat__cnt = 0;
		repeat__idx = 0;

		what = command_cmd;

		/* Save this command */
		repeat_push(what);
	}
}

#endif /* ALLOW_REPEAT -- TNB */


#ifdef SUPPORT_GAMMA

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

	for (i = 1; i < 255; i++)
	{
		/*
		 * Initialise the Taylor series
		 *
		 * value and diff have been scaled by 256
		 */
		n = 1;
		value = 256L * 256L;
		diff = ((long)gamma_helper[i]) * (gamma - 256);

		while (diff)
		{
			value += diff;
			n++;

			/*
			 * Use the following identiy to calculate the gamma table.
			 * exp(x) = 1 + x + x^2/2 + x^3/(2*3) + x^4/(2*3*4) +...
			 *
			 * n is the current term number.
			 *
			 * The gamma_helper array contains a table of
			 * ln(x/256) * 256
			 * This is used because a^b = exp(b*ln(a))
			 *
			 * In this case:
			 * a is i / 256
			 * b is gamma.
			 *
			 * Note that everything is scaled by 256 for accuracy,
			 * plus another factor of 256 for the final result to
			 * be from 0-255.  Thus gamma_helper[] * gamma must be
			 * divided by 256*256 each itteration, to get back to
			 * the original power series.
			 */
			diff = (((diff / 256) * gamma_helper[i]) * (gamma - 256)) /
				(256 * n);
		}

		/*
		 * Store the value in the table so that the
		 * floating point pow function isn't needed.
		 */
		gamma_table[i] = ((long)(value / 256) * i) / 256;
	}
}

#endif /* SUPPORT_GAMMA */

#define MAX_RESIZE_HOOKS 8

static void (*resize_hooks[MAX_RESIZE_HOOKS])(void);
static uint num_resize_hooks = 0;

/*
 * Add an action to term_screen->resize_hook.
 */
errr add_resize_hook(void (*resize_hook)(void))
{
	/* No room. */
	if (num_resize_hooks == MAX_RESIZE_HOOKS)
	{
		/* Warning, as this probably means a hook isn't being removed. */
		if (alert_failure) msg_print("Too many resize hooks.");
		return HOOK_ERROR_OUT_OF_MEMORY;
	}
	else
	{
		resize_hooks[num_resize_hooks++] = resize_hook;
		return SUCCESS;
	}
}

/*
 * Remove an action from term_screen->resize_hook.
 */
errr delete_resize_hook(void (*resize_hook)(void))
{
	uint i;
	for (i = 0; i < num_resize_hooks; i++)
	{
		if (resize_hooks[i] != resize_hook) continue;
		resize_hooks[i] = resize_hooks[num_resize_hooks--];
		return SUCCESS;
	}
	return HOOK_ERROR_NO_SUCH_HOOK;
}

void resize_main_term(void)
{
	uint i;
	for (i = 0; i < num_resize_hooks; i++)
	{
		(*resize_hooks[i])();
	}
}

/*
 * Repeat a specified string (up to) num times into a buffer.
 */
static void repeat_string(char *buf, uint max, cptr str, int num)
{
	const uint strl = strlen(str);

	/* Avoid silly requests. */
	if (num <= 0 || !strl) return;

	/* Handle excessive requests. */
	if (strl*num >= max)
	{
		int excess = (max-1) % strl;
		num = (max-1) / strl;

		/* End with a partial string. */
		sprintf(buf+num*strl, "%.*s", excess, str);
	}
	/* Normal requests are just terminated. */
	else
	{
		/* Simply terminate the string. */
		buf[num*strl] = '\0';
	}

	/* Add in the appropriate number of complete strings. */
	for (; num; num--) strncpy(buf+(num-1)*strl, str, strl);
}

/*
 * A vstrnfmt_aux wrapper around repeat_string().
 */
void repeat_string_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	cptr str = va_arg(*vp, cptr);
	int num = va_arg(*vp, int);
	repeat_string(buf, max, str, num);
}

/*
 * Add a series of keypresses to the "queue".
 *
 * Return any errors generated by Term_keypress() in doing so, or SUCCESS
 * if there are none.
 *
 * Catch the "out of space" error before anything is printed.
 *
 * NB: The keys added here will be interpreted by any macros or keymaps.
 */
errr type_string(cptr str, uint len)
{
	cptr s;

	term *old = Term;

	/* Paranoia - no string. */
	if (!str) return TERM_ERROR_BAD_INPUT;

	/* Hack - calculate the string length here if none given. */
	if (!len) len = strlen(str);

	/* Activate the main window, as all pastes go there. */
	Term_activate(term_screen);

	/* Not enough space for the string. */
	if (Term_queue_space() <= (int)len)
		return PARSE_ERROR_OUT_OF_MEMORY;

	for (s = str; s < str+len; s++)
	{
		errr err = Term_keypress(*s);

		/* Catch errors other than "str[i] == 0", which is ignored. */
		if (err && err != TERM_ERROR_BAD_INPUT) return err;
	}

	/* Activate the original window. */
	Term_activate(old);

	return SUCCESS;
}
