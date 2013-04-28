/* File: z-file.c */

/*
 * File-handling functions.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "z-file.h"
#include "z-util.h"
#include "z-form.h"


/*
 * Player info
 */
int player_egid;

/*
 * Path name: The main "lib" directory
 * This variable is not actually used anywhere in the code
 */
cptr ANGBAND_DIR;

/*
 * High score files (binary)
 * These files may be portable between platforms
 */
cptr ANGBAND_DIR_APEX;

/*
 * Bone files for player ghosts (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_BONE;

/*
 * Binary image files for the "*_info" arrays (binary)
 * These files are not portable between platforms
 */
cptr ANGBAND_DIR_DATA;

/*
 * Textual template files for the "*_info" arrays (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_EDIT;

/*
 * Various extra files (ascii)
 * These files may be portable between platforms
 */
cptr ANGBAND_DIR_FILE;

/*
 * Help files (normal) for the online help (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_HELP;

/*
 * Miscellaneous text files, also contains any spoilers (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_INFO;

/*
 * Savefiles for current characters (binary)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_SAVE;

/*
 * Default user "preference" files (ascii)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_PREF;

/*
 * User Defined "preference" files (ascii)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_USER;

/*
 * Various extra files (binary)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_XTRA;





/*
 * The concept of the "file" routines below (and elsewhere) is that all
 * file handling should be done using as few routines as possible, since
 * every machine is slightly different, but these routines always have the
 * same effects.
 *
 * In fact, perhaps we should use the "path_parse()" routine below to convert
 * from "canonical" filenames (optional leading tilde's, internal wildcards,
 * slash as the path separator, etc) to "system" filenames (no special symbols,
 * system-specific path separator, etc).  This would allow the program itself
 * to assume that all filenames are "UNIX" filenames, and explicitly "extract"
 * such filenames if needed (by "path_parse()", or perhaps "path_canon()").
 *
 * Note that "path_temp" should probably return a "canonical" filename.
 *
 * Note that "my_fopen()" and "my_open()" and "my_make()" and "my_kill()"
 * and "my_move()" and "my_copy()" should all take "canonical" filenames.
 *
 * Note that "canonical" filenames use a leading "slash" to indicate
 * an absolute path, and a leading "tilde" to indicate a special
 * directory, and default to a relative path, but MSDOS uses a leading
 * "drivename plus colon" to indicate the use of a "special drive",
 * and then the rest of the path is parsed "normally", and MACINTOSH
 * uses a leading colon to indicate a relative path, and an embedded
 * colon to indicate a "drive plus absolute path", and finally
 * defaults to a file in the current working directory, which may or
 * may not be defined.
 *
 * We should probably parse a leading "~~/" as referring to "ANGBAND_DIR". (?)
 */


#ifdef RISCOS


/*
 * Most of the "file" routines for "RISCOS" should be in "main-ros.c"
 */


#else /* RISCOS */


#ifdef SET_UID

/*
 * Extract a "parsed" path from an initial filename
 * Normally, we simply copy the filename into the buffer
 * But leading tilde symbols must be handled in a special way
 * Replace "~user/" by the home directory of the user named "user"
 * Replace "~/" by the home directory of the current user
 */
errr path_parse(char *buf, size_t max, cptr file)
{
	cptr u, s;
	struct passwd *pw;
	char user[128];


	/* Assume no result */
	buf[0] = '\0';

	/* No file? */
	if (!file) return (-1);

	/* File needs no parsing */
	if (file[0] != '~')
	{
		(void)my_strcpy(buf, file, max);
		return (0);
	}

	/* Point at the user */
	u = file + 1;

	/* Look for non-user portion of the file */
	s = strstr(u, PATH_SEP);

	/* Hack -- no long user names */
	if (s && (s >= u + sizeof(user))) return (1);

	/* Extract a user name */
	if (s)
	{
		int i;

		for (i = 0; u < s; ++i) user[i] = *u++;
		user[i] = '\0';
		u = user;
	}

	/* Look up the "current" user */
	if (u[0] == '\0') u = getlogin();

	/* Look up a user (or "current" user) */
	if (u) pw = getpwnam(u);
	else   pw = getpwuid(getuid());

	/* Nothing found? */
	if (!pw) return (1);

	/* Make use of the info */
	(void)my_strcpy(buf, pw->pw_dir, max);

	/* Append the rest of the filename, if any */
	if (s) (void)my_strcat(buf, s, max);

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
errr path_parse(char *buf, size_t max, cptr file)
{
	/* Accept the filename */
	(void)strnfmt(buf, max, "%s", file);

# if defined(MAC_MPW) && defined(CARBON)

	/* Fix it according to the current operating system */
	convert_pathname(buf);

# endif

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
static errr path_temp(char *buf, size_t max)
{
	cptr s;

	/* Temp file */
	s = tmpnam(NULL);

	/* Oops */
	if (!s) return (-1);

	/* Format to length */
	(void)strnfmt(buf, max, "%s", s);

	/* Success */
	return (0);
}

#endif /* HAVE_MKSTEMP */



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
errr path_build(char *buf, size_t max, cptr path, cptr file)
{
	/* Special file */
	if (file[0] == '~')
	{
		/* Use the file itself */
		(void)strnfmt(buf, max, "%s", file);
	}

	/* Absolute file, on "normal" systems */
	else if (prefix(file, PATH_SEP) && !streq(PATH_SEP, ""))
	{
		/* Use the file itself */
		(void)strnfmt(buf, max, "%s", file);
	}

	/* No path given */
	else if (!path[0])
	{
		/* Use the file itself */
		(void)strnfmt(buf, max, "%s", file);
	}

	/* Path and File */
	else
	{
		/* Build the new path */
		(void)strnfmt(buf, max, "%s%s%s", path, PATH_SEP, file);
	}

	/* Success */
	return (0);
}



/*
 * Hack -- replacement for "fopen()"
 */
FILE *my_fopen(cptr file, cptr mode)
{
	char buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, sizeof(buf), file)) return (NULL);

#if defined(MAC_MPW) || defined(MACH_O_CARBON)

	/* Set file creator and type */
	if (fff && strchr(mode, 'w')) fsetfileinfo(buf, _fcreator, _ftype);

#endif

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

#endif /* RISCOS */


#ifdef HAVE_MKSTEMP


FILE *my_fopen_temp(char *buf, size_t max)
{
	int fd;

	/* Prepare the buffer for mkstemp */
	(void)my_strcpy(buf, "/tmp/anXXXXXX", max);

	/* Secure creation of a temporary file */
	fd = mkstemp(buf);

	/* Check the file-descriptor */
	if (fd < 0) return (NULL);

	/* Return a file stream */
	return (fdopen(fd, "w"));
}

#else /* HAVE_MKSTEMP */

FILE *my_fopen_temp(char *buf, size_t max)
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
	int i = 0;
	char *s = buf;
	int len;

	bool check_encodes = FALSE;


	/* Paranoia */
	if (n <= 0) return (1);

	/* Enforce historical upper bound */
	if (n > 1024) n = 1024;

	/* Leave a byte for terminating null */
	len = n - 1;

	/* While there's room left in the buffer */
	while (i < len)
	{
		int c;

		/*
		 * Read next character - stdio buffers I/O, so there's no
		 * need to buffer it again using fgets.
		 */
		c = fgetc(fff);

		if (c == EOF)
		{
			/* No characters read -- signal error */
			if (i == 0)
			{
				/* End of file error */
				buf[0] = '\0';

				/* Error */
				return (1);
			}

			/* Success */
			break;
		}

#if defined(MACINTOSH) || defined(MACH_O_CARBON)

		/*
		 * Be nice to the Macintosh, where a file can have Mac or Unix
		 * end of line, especially since the introduction of OS X.
		 * MPW tools were also very tolerant to the Unix EOL.
		 */
		if (c == '\r') c = '\n';

#endif /* MACINTOSH || MACH_O_CARBON */

		/* End of line */
		if (c == '\n') break;

		/* Expand a tab into spaces */
		if (c == '\t')
		{
			int tabstop;

			/* Next tab stop */
			tabstop = ((i + TAB_COLUMNS) / TAB_COLUMNS) * TAB_COLUMNS;

			/* Bounds check */
			if (tabstop >= len)
			{
				/* Buffer overflow - return an empty string */
				buf[0] = '\0';

				/* Error */
				return (1);
			}

			/* Convert it to spaces */
			while (i < tabstop)
			{
				/* Store space */
				*s++ = ' ';

				/* Count */
				i++;
			}
		}

		/* Ignore non-printables */
		else if (isprint(c))
		{
			/* Store character in the buffer */
			*s++ = c;

			/* Count number of characters in the buffer */
			i++;

			/* Notice possible encode */
			if (c == '[') check_encodes = TRUE;
		}
	}

	/* Always terminate the string */
	*s = '\0';

	/* Translate encodes if necessary */
	if (check_encodes) xstr_trans(buf, 0);

	/* Success */
	return (0);
}


/*
 * Hack -- replacement for "fputs()"
 *
 * Dump a string, plus a newline, to a file
 *
 * Perhaps this function should handle internal weirdness.
 *
 * XXX XXX -- Add error checking!
 */
errr my_fputs(FILE *fff, cptr buf, size_t n)
{
	/* Unused parameter */
	(void)n;

	/* Dump, ignore errors */
	(void)fprintf(fff, "%s\n", buf);

	/* Success */
	return (0);
}



#ifdef RISCOS


/*
 * Most of the "file" routines for "RISCOS" should be in "main-ros.c"
 *
 * Many of them can be rewritten now that only "fd_open()" and "fd_make()"
 * and "my_fopen()" should ever create files.
 */


#else /* All systems other than RISCOS */


/*
 * Several systems have no "O_BINARY" flag
 */
#ifndef O_BINARY
#define O_BINARY 0
#endif /* O_BINARY */


/*
 * Hack -- attempt to delete a file
 */
errr fd_kill(cptr file)
{
	char buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, sizeof(buf), file)) return (-1);

	/* Remove, return 0 on success, non-zero on failure */
	return (remove(buf));
}


/*
 * Hack -- attempt to move a file
 */
errr fd_move(cptr file, cptr what)
{
	char buf[1024];
	char aux[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, sizeof(buf), file)) return (-1);

	/* Hack -- Try to parse the path */
	if (path_parse(aux, sizeof(aux), what)) return (-1);

	/* Rename, return 0 on success, non-zero on failure */
	return (rename(buf, aux));
}

/*
 * Hack -- attempt to open a file descriptor (create file)
 *
 * This function should fail if the file already exists
 *
 * Note that we assume that the file should be "binary"
 */
int fd_make(cptr file, int mode)
{
	char buf[1024];
	int fd;

	/* Hack -- Try to parse the path */
	if (path_parse(buf, sizeof(buf), file)) return (-1);

#if defined(MACINTOSH)

	/* Create the file, fail if exists, write-only, binary */
	fd = open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY);

#else

	/* Create the file, fail if exists, write-only, binary */
	fd = open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, mode);

#endif

#if defined(MAC_MPW) || defined(MACH_O_CARBON)

	/* Set file creator and type */
	if (fd >= 0) fsetfileinfo(buf, _fcreator, _ftype);

#endif

	/* Return descriptor */
	return (fd);
}


/*
 * Hack -- attempt to open a file descriptor (existing file)
 *
 * Note that we assume that the file should be "binary"
 */
int fd_open(cptr file, int flags)
{
	char buf[1024];

	/* Hack -- Try to parse the path */
	if (path_parse(buf, sizeof(buf), file)) return (-1);

#if defined(MACINTOSH) || defined(WINDOWS)

	/* Attempt to open the file */
	return (open(buf, flags | O_BINARY));

#else

	/* Attempt to open the file */
	return (open(buf, flags | O_BINARY, 0));

#endif

}


/*
 * Hack -- attempt to lock a file descriptor
 *
 * Legal lock types -- F_UNLCK, F_RDLCK, F_WRLCK
 *
 * XXX XXX -- Clean up!
 */
errr fd_lock(int fd, int what)
{
#ifdef SET_UID

	struct flock lock;

#endif

	/* Verify the fd */
	if (fd < 0) return (-1);

#ifdef SET_UID

	lock.l_type = what;
	lock.l_start = 0; /* Lock the entire file */
	lock.l_whence = SEEK_SET; /* Lock the entire file */
	lock.l_len = 0; /* Lock the entire file */

	/* Wait for access and set lock status */
	/*
	 * Change F_SETLKW to F_SETLK if it's preferable to return
	 * without locking and reporting an error instead of waiting.
	 */
	return (fcntl(fd, F_SETLKW, &lock));


#else /* SET_UID */

	/* Unused parameter */
	(void)what;

#endif /* SET_UID */

	/* Success */
	return (0);
}


/*
 * Hack -- attempt to seek on a file descriptor
 */
errr fd_seek(int fd, long n)
{
	long p;

	/* Verify fd */
	if (fd < 0) return (-1);

	/* Seek to the given position */
	p = lseek(fd, n, SEEK_SET);

	/* Failure */
	if (p < 0) return (1);

	/* Failure */
	if (p != n) return (1);

	/* Success */
	return (0);
}


#ifndef SET_UID
#define FILE_BUF_SIZE 16384
#endif

/*
 * Hack -- attempt to read data from a file descriptor
 */
errr fd_read(int fd, char *buf, size_t n)
{
	/* Verify the fd */
	if (fd < 0) return (-1);

#ifndef SET_UID

	/* Read pieces */
	while (n >= FILE_BUF_SIZE)
	{
		/* Read a piece */
		if (read(fd, buf, FILE_BUF_SIZE) != FILE_BUF_SIZE) return (1);

		/* Shorten the task */
		buf += FILE_BUF_SIZE;

		/* Shorten the task */
		n -= FILE_BUF_SIZE;
	}

#endif

	/* Read the final piece */
	if (read(fd, buf, n) != (int)n) return (1);

	/* Success */
	return (0);
}


/*
 * Hack -- Attempt to write data to a file descriptor
 */
errr fd_write(int fd, cptr buf, size_t n)
{
	/* Verify the fd */
	if (fd < 0) return (-1);

#ifndef SET_UID

	/* Write pieces */
	while (n >= FILE_BUF_SIZE)
	{
		/* Write a piece */
		if (write(fd, buf, FILE_BUF_SIZE) != FILE_BUF_SIZE) return (1);

		/* Shorten the task */
		buf += FILE_BUF_SIZE;

		/* Shorten the task */
		n -= FILE_BUF_SIZE;
	}

#endif

	/* Write the final piece */
	if (write(fd, buf, n) != (int)n) return (1);

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

	/* Close, return 0 on success, -1 on failure */
	return(close(fd));
}


#if defined(CHECK_MODIFICATION_TIME) && !defined(MAC_MPW)
# ifdef MACINTOSH
#  include <stat.h>
# else
#  include <sys/types.h>
#  include <sys/stat.h>
# endif /* MACINTOSH */

errr check_modification_date(int fd, cptr template_file)
{
	char buf[1024];

	struct stat txt_stat, raw_stat;

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, template_file);

	/* Get stats on text file */
	if (stat(buf, &txt_stat))
	{
		/* No text file - continue */
	}

	/* Get stats on raw file */
	else if (fstat(fd, &raw_stat))
	{
		/* Error */
		return (-1);
	}

	/* Ensure text file is not newer than raw file */
	else if (txt_stat.st_mtime > raw_stat.st_mtime)
	{
		/* Reprocess text file */
		return (-1);
	}

	return (0);
}

#endif /* CHECK_MODIFICATION_TIME */


#endif /* End of non-RISCOS section */



/*
 * Hack -- drop permissions
 */
void safe_setuid_drop(void)
{

#ifdef SET_UID

# ifdef HAVE_SETEGID

	if (setegid(getgid()) != 0)
	{
		quit("setegid(): cannot set permissions correctly!");
	}

# else /* HAVE_SETEGID */

	if (setgid(getgid()) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

# endif /* HAVE_SETEGID */

#endif /* SET_UID */

}


/*
 * Hack -- grab permissions
 */
void safe_setuid_grab(void)
{

#ifdef SET_UID

# ifdef HAVE_SETEGID

	if (setegid(player_egid) != 0)
	{
		quit("setegid(): cannot set permissions correctly!");
	}

# else /* HAVE_SETEGID */

	if (setgid(player_egid) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

# endif /* HAVE_SETEGID */

#endif /* SET_UID */

}



