/* File: util.c */

/* Functions, etc. for various machines that need them.  Basic text-handing 
 * functions.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#ifdef _WIN32_WCE
#include "angbandcw.h"
#endif /* _WIN32_WCE */


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

# ifndef HAVE_USLEEP

/*
 * For those systems that don't have "usleep()" but need it.
 *
 * Fake "usleep()" function grabbed from the inl netrek server -cba
 */
int usleep(huge usecs)
{
  struct timeval      Timer;
  
  int nfds = 0;
  
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
extern struct passwd *getpwuid();
extern struct passwd *getpwnam();


/*
 * Find a default user name from the system.
 */
void user_name(char *buf, int id)
{
  struct passwd *pw;
  
  /* Look up the user name */
  if ((pw = getpwuid(id)))
    {
      strcpy(buf, pw->pw_name);
      buf[16] = '\0';
      
#ifdef CAPITALIZE_USER_NAME
      /* Hack -- capitalize the user name */
      if (islower(buf[0])) buf[0] = toupper(buf[0]);
#endif
      
      return;
    }
  
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
 * Note that "canonical" filenames use a leading "slash" to indicate an 
 * absolute path, and a leading "tilde" to indicate a special directory, and 
 * default to a relative path, but MSDOS uses a leading "drivename plus colon" 
 * to indicate the use of a "special drive", and then the rest of the path is 
 * parsed "normally", and MACINTOSH uses a leading colon to indicate a 
 * relative path, and an embedded colon to indicate a "drive plus absolute 
 * path", and finally defaults to a file in the current working directory, 
 * which may or may not be defined.
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
errr path_parse(char *buf, int max, cptr file)
{
  cptr u, s;
  struct passwd	*pw;
  char user[128];
  
  
  /* Assume no result */
  buf[0] = '\0';
  
  /* No file? */
  if (!file) return (-1);
  
  /* File needs no parsing */
  if (file[0] != '~')
    {
      strcpy(buf, file);
      return (0);
    }
  
  /* Point at the user */
  u = file+1;
  
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
  else pw = getpwuid(getuid());
  
  /* Nothing found? */
  if (!pw) return (1);
  
  /* Make use of the info */
  strcpy(buf, pw->pw_dir);
  
  /* Append the rest of the filename, if any */
  if (s) strcat(buf, s);
  
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
errr path_parse(char *buf, int max, cptr file)
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
extern errr path_temp(char *buf, int max)
{
#ifdef _WIN32_WCE
	//SJG Should no longer be called for WINCE
	return (0);
#else  
	cptr s;

	/* Temp file */
	s = tmpnam(NULL);

	/* Oops */
	if (!s) return (-1);

	/* Copy to buffer */
	my_strcpy(buf, s, max);

	/* Success */
	return (0);
#endif
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
errr path_build(char *buf, int max, cptr path, cptr file)
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
  return (0);
}


/*
 * Hack -- replacement for "fopen()"
 */
FILE *my_fopen(cptr file, cptr mode)
{
   FILE *fff;
  char buf[1024];
  
  /* Hack -- Try to parse the path */
  if (path_parse(buf, 1024, file)) return (NULL);
  
  /* Attempt to fopen the file anyway */
  fff = fopen(buf, mode);
  
#ifdef MACH_O_CARBON
  
  /* Set file creator and type */
  if (fff && strchr(mode, 'w')) fsetfileinfo(buf, _fcreator, _ftype);
  
#endif
  
  return fff;
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


#ifdef HAVE_MKSTEMP

FILE *my_fopen_temp(char *buf, size_t max)
{
	int fd;

	/* Prepare the buffer for mkstemp */
	my_strcpy(buf, "/tmp/anXXXXXX", max);

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
#ifdef _WIN32_WCE
  TCHAR wcBuf[1024]; 
  TCHAR wcTmp[1024];
  TCHAR wcPref[1024];
  mbstowcs(wcPref, "ang", 1024);
  /* Get the temp path */
  if (GetTempPath(max, wcTmp) > max) return (NULL);
  /* Generate a temporary filename */
  if (GetTempFileName(wcTmp, wcPref, 0, wcBuf) == 0) return (NULL);
  wcstombs(buf, wcBuf, 1024);
  /* Open the file */
  return (my_fopen(buf, "w"));
#else
  /* Generate a temporary filename */
  if (path_temp(buf, max)) return (NULL);
  
  /* Open the file */
  return (my_fopen(buf, "w"));
#endif
}

#endif /* HAVE_MKSTEMP */



/*
 * Hack -- replacement for "fgets()"
 *
 * Read a string, without a newline, to a file
 *
 * Process tabs, strip internal non-printables
 */
errr my_fgets(FILE *fff, char *buf, huge n)
{
  huge i = 0;
  
  char *s;
  
  char tmp[1024];
  
  /* Read a line */
  if (fgets(tmp, 1024, fff))
    {
      /* Convert weirdness */
      for (s = tmp; *s; s++)
	{
	  /* Handle newline */
	  if (*s == '\n')
	    {
	      /* Terminate */
	      buf[i] = '\0';
	      
	      /* Success */
	      return (0);
	    }
	  
	  /* Handle tabs */
	  else if (*s == '\t')
	    {
	      /* Hack -- require room */
	      if (i + 8 >= n) break;
	      
	      /* Append a space */
	      buf[i++] = ' ';
	      
	      /* Append some more spaces */
	      while (!(i % 8)) buf[i++] = ' ';
	    }
	  
	  /* Handle printables */
	  else if (isprint(*s))
	    {
	      /* Copy */
	      buf[i++] = *s;
	      
	      /* Check length */
	      if (i >= n) break;
	    }
	}
    }
  
  /* Nothing */
  buf[0] = '\0';
  
  /* Failure */
  return (1);
}


/*
 * Hack -- replacement for "fputs()"
 *
 * Dump a string, plus a newline, to a file
 *
 * Perhaps this function should handle internal weirdness.
 */
errr my_fputs(FILE *fff, cptr buf, huge n)
{
  /* Dump, ignore errors */
  (void)fprintf(fff, "%s\n", buf);
  
  /* Success */
  return (0);
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
  char buf[1024];

  /* Hack -- Try to parse the path */
  if (path_parse(buf, 1024, file)) return (-1);
  
#ifdef _WIN32_WCE
  {
    TCHAR wcBuf[1024];
    mbstowcs( wcBuf, buf, 1024);
    
    DeleteFile(wcBuf);
  }
  
#else
  /* Remove */
  (void)remove(buf);

#endif

  /* Assume success XXX XXX XXX */
  return (0);
}


/*
 * Hack -- attempt to move a file
 */
errr fd_move(cptr file, cptr what)
{
  char buf[1024];
  char aux[1024];
  
  /* Hack -- Try to parse the path */
  if (path_parse(buf, 1024, file)) return (-1);
  
  /* Hack -- Try to parse the path */
  if (path_parse(aux, 1024, what)) return (-1);
  
#ifdef _WIN32_WCE
  {
    TCHAR wcBuf[1024];
    TCHAR wcAux[1024];
    mbstowcs( wcBuf, buf, 1024);
    mbstowcs( wcAux, aux, 1024);
    
    MoveFile(wcBuf, wcAux);
  }
#else
  /* Rename */
  (void)rename(buf, aux);
#endif
  
  /* Assume success XXX XXX XXX */
  return (0);
}


/*
 * Hack -- attempt to copy a file
 */
errr fd_copy(cptr file, cptr what)
{
  char buf[1024];
  char aux[1024];
  
  /* Hack -- Try to parse the path */
  if (path_parse(buf, 1024, file)) return (-1);
  
  /* Hack -- Try to parse the path */
  if (path_parse(aux, 1024, what)) return (-1);
  
  /* Copy XXX XXX XXX */
  /* (void)rename(buf, aux); */
  
  /* Assume success XXX XXX XXX */
  return (1);
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

  /* Hack -- Try to parse the path */
  if (path_parse(buf, 1024, file)) return (-1);
  
#ifdef _WIN32_WCE
  {
    TCHAR wcBuf[1024];
    mbstowcs( wcBuf, buf, 1024);
    
    return (CreateFile(wcBuf, GENERIC_WRITE, 0, NULL, CREATE_NEW, 
		       FILE_ATTRIBUTE_NORMAL, 0));
  }
#else

#if defined(WINDOWS)
  
  /* Create the file, fail if exists, write-only, binary */
  return (open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, mode /* TNB */));
  
#elif defined(MACINTOSH) || defined(MACH_O_CARBON)
 
  /* Set file creator and type */
  fd =open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, mode /* TNB */);
  if (fd >= 0) fsetfileinfo(buf, _fcreator, _ftype);
  return fd;
  
  
#else
  
  /* Create the file, fail if exists, write-only, binary */
  return (open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, mode));

#endif
#endif /* WCE */

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
  if (path_parse(buf, 1024, file)) return (-1);
  
#ifdef _WIN32_WCE

  {
    TCHAR wcBuf[1024];
    mbstowcs( wcBuf, buf, 1024);
    
    return (CreateFile(wcBuf, GENERIC_READ | GENERIC_WRITE, 
		       FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, 
		       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));
  }
#else

#if defined(MACINTOSH) || defined(WINDOWS)
  
  /* Attempt to open the file */
  return (open(buf, flags | O_BINARY));
  
#else

  /* Attempt to open the file */
  return (open(buf, flags | O_BINARY, 0));

#endif
#endif /* WCE */
}


/*
 * Hack -- attempt to lock a file descriptor
 *
 * Legal lock types -- F_UNLCK, F_RDLCK, F_WRLCK
 */
errr fd_lock(int fd, int what)
{
  /* Verify the fd */
#ifdef _WIN32_WCE
  if (fd == -1) return (-1);
#else
  if (fd < 0) return (-1);
#endif

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
  
#ifdef _WIN32_WCE
  /* Verify fd */
  if (fd == INVALID_HANDLE_VALUE) return (-1);
  
  /* Seek to the given position */
  p = SetFilePointer(fd, n, NULL, FILE_BEGIN); 
  
  /* Failure */
  if (p < 0) return (1);
  
  /* Failure */
  if (p != n) return (1);
  
  /* Success */
  return (0);
#else
  /* Verify fd */
  if (fd < 0) return (-1);
  
  /* Seek to the given position */
  p = lseek(fd, n, SEEK_SET);
  
  /* Failure */
  if (p < 0) return (1);
  
  /* Failure */
  if ((huge)p != n) return (1);
  
  /* Success */
  return (0);
#endif
}


/*
 * Hack -- attempt to truncate a file descriptor
 */
errr fd_chop(int fd, huge n)
{
  /* Verify the fd */
  if (fd < 0) return (-1);
  
#if defined(SUNOS) || defined(ULTRIX) || defined(NeXT)
  /* Truncate */
  ftruncate(fd, n);
#endif
  
  /* Success */
  return (0);
}

/*
 * Hack -- attempt to read data from a file descriptor
 */
errr fd_read(int fd, char *buf, huge n)
{
#ifdef _WIN32_WCE
  DWORD numBytesRead;
  
  /* Verify the fd */
  if (fd == INVALID_HANDLE_VALUE) return (-1);
  
#ifndef SET_UID
  
  /* Read pieces */
  while (n >= 16384)
    {
      /* Read a piece */
      if (!ReadFile(fd, buf, 16384, &numBytesRead, NULL))
	{
	  return (1);
	}
      
      if (numBytesRead != 16384)
	{
	  return (1);
	}
      
      /* Shorten the task */
      buf += 16384;
      
      /* Shorten the task */
      n -= 16384;
    }
  
#endif
  
  /* Read the final piece */
  if (!ReadFile(fd, buf, n, &numBytesRead, NULL))
    {
      return (1);
    }
  
  if (numBytesRead != n)
    {
      return (1);
    }
  
  /* Success */
  return (0);
#else
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
#endif /* WCE */
}


/*
 * Hack -- Attempt to write data to a file descriptor
 */
errr fd_write(int fd, cptr buf, huge n)
{
#ifdef _WIN32_WCE
  DWORD numBytesWrite;
  
  /* Verify the fd */
  if (fd == INVALID_HANDLE_VALUE) return (-1);
  
#ifndef SET_UID
  
  /* Write pieces */
  while (n >= 16384)
    {
      /* Write a piece */
      if (!WriteFile(fd, buf, 16384, &numBytesWrite, NULL))
	{
	  return (1);
	}
      
      if (numBytesWrite != 16384)
	{
	  return (1);
	}
      
      /* Shorten the task */
      buf += 16384;
      
      /* Shorten the task */
      n -= 16384;
    }
  
#endif
  
  /* Write the final piece */
  if (!WriteFile(fd, buf, n, &numBytesWrite, NULL))
    {
      return (1);
    }
  
  if (numBytesWrite != n)
    {
      return (1);
    }
  
  /* Success */
  return (0);
  
#else
  /* Verify the fd */
  if (fd < 0) return (-1);
  
#ifndef SET_UID
  
  /* Write pieces */
  while (n >= 16384)
    {
      /* Write a piece */
      if (write(fd, (void *)buf, 16384) != 16384) return (1);
      
      /* Shorten the task */
      buf += 16384;
      
      /* Shorten the task */
      n -= 16384;
    }
  
#endif
  
  /* Write the final piece */
  if (write(fd, (void *)buf, n) != (long)n) return (1);
  
  /* Success */
  return (0);
#endif
}


/*
 * Hack -- attempt to close a file descriptor
 */
errr fd_close(int fd)
{
#ifdef _WIN32_WCE
  /* Verify the fd */
  if (fd == INVALID_HANDLE_VALUE) return (-1);
  
  /* Close */
  if (!CloseHandle(fd))
    {
      return (-1);
    }
  
  return (0);
#else
  /* Verify the fd */
  if (fd < 0) return (-1);
  
  /* Close */
  /* fclose(file_descriptors[fd]); */
  (void)close(fd);
  
  /* Assume success XXX XXX XXX */
  return (0);
#endif
}


#ifdef CHECK_MODIFICATION_TIME
# ifdef MACINTOSH
#  include <stat.h>
# else
#  ifdef _WIN32_WCE
#  else
#  include <sys/types.h>
#  include <sys/stat.h>
#  endif /* _WIN32_WCE */
# endif /* MACINTOSH */


errr check_modification_date(int fd, cptr template_file)
{
#ifdef _WIN32_WCE
#else
  char buf[1024];
  
  struct stat txt_stat, raw_stat;
  
  /* Build the filename */
  path_build(buf, 1024, ANGBAND_DIR_EDIT, template_file);
  
  /* Access stats on text file */
  if (stat(buf, &txt_stat))
    {
      /* No text file - continue */
    }
  
  /* Access stats on raw file */
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
#endif  
  return (0);
}

#endif /* CHECK_MODIFICATION_TIME */

#endif /* ACORN */



/*
 * Convert a decimal to a single digit hex number
 */
static char hexify(uint i)
{
  return (hexsym[i%16]);
}



/*
 * Convert a hexidecimal-digit into a decimal
 */
static int dehex(char c)
{
  if (isdigit(c)) return (D2I(c));
  if (isalpha(c)) return (A2I(tolower(c)) + 10);
  return (0);
}


/*
 * Hack -- convert a printable string into real ascii
 *
 * This function will not work on non-ascii systems.
 *
 * To be safe, "buf" should be at least as large as "str".
 */
void text_to_ascii(char *buf, cptr str)
{
  char *s = buf;
  
  /* Analyze the "ascii" string */
  while (*str)
    {
      /* Backslash codes */
      if (*str == '\\')
	{
	  /* Skip the backslash */
	  str++;
	  
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
	  
	  /* Actual "backslash" */
	  else if (*str == '\\')
	    {
	      *s++ = '\\';
	    }
	  
	  /* Hack -- Actual "caret" */
	  else if (*str == '^')
	    {
	      *s++ = '^';
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
	  *s++ = (*str++ & 037);
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
 *
 * To be safe, "buf" should be at least four times as large as "str".
 */
void ascii_to_text(char *buf, cptr str)
{
  char *s = buf;
  
  /* Analyze the "ascii" string */
  while (*str)
    {
      byte i = (byte)(*str++);
      
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
      else if (i == '\\')
	{
	  *s++ = '\\';
	  *s++ = '\\';
	}
      else if (i == '^')
	{
	  *s++ = '\\';
	  *s++ = '^';
	}
      else if (i < 32)
	{
	  *s++ = '^';
	  *s++ = i + 64;
	}
      else if (i < 127)
	{
	  *s++ = i;
	}
      else
	{
	  *s++ = '\\';
	  *s++ = 'x';
	  *s++ = hexify(i / 16);
	  *s++ = hexify(i % 16);
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
      string_free(macro__act[n]);
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
static key_event inkey_aux(void)
{
  int k = 0, n, p = 0, w = 0;
  
  key_event ke, ke0;
  char ch;
  
  cptr pat, act;
  
  char buf[1024];
  
  /* Initialize the no return */
  ke0.key = 0;
  ke0.mousebutton = 0; /* To fix GCC warnings on X11 */
  ke0.mousey = 0;
  ke0.mousex = 0;
 
  /* Wait for a keypress */
  (void)(Term_inkey(&ke, TRUE, TRUE));
  ch = ke.key;
  
  /* End "macro action" */
  if ((ch == 30) || (ch == '\xff'))
    {
      parse_macro = FALSE;
      return (ke);
    }
  
  /* Inside "macro action" */
  if (parse_macro) return (ke);
  
  /* Inside "macro trigger" */
  if (parse_under) return (ke);
  

  /* Save the first key, advance */
  buf[p++] = ch;
  buf[p] = '\0';
  
  
  /* Check for possible macro */
  k = macro_find_check(buf);
  
  /* No macro pending */
  if (k < 0) return (ke);
  
  
  /* Wait for a macro, or a timeout */
  while (TRUE)
    {
      /* Check for pending macro */
      k = macro_find_maybe(buf);
      
      /* No macro pending */
      if (k < 0) break;
      
      /* Check for (and remove) a pending key */
      if (0 == Term_inkey(&ke, FALSE, TRUE))
	{
	  /* Append the key */
	  buf[p++] = ke.key;
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
	  if (Term_key_push(buf[--p])) return (ke0);
	}
      
      /* Wait for (and remove) a pending key */
      (void)Term_inkey(&ke, TRUE, TRUE);
      
      /* Return the key */
      return (ke);
    }
  
  
  /* Get the pattern */
  pat = macro__pat[k];
  
  /* Get the length of the pattern */
  n = strlen(pat);
  
  /* Push the "extra" keys back on the queue */
  while (p > n)
    {
      /* Push the key, notice over-flow */
      if (Term_key_push(buf[--p])) return (ke0);
    }
  
  
  /* Begin "macro action" */
  parse_macro = TRUE;
  
  /* Push the "end of macro action" key */
  if (Term_key_push(30)) return (ke0);
  
  
  /* Access the macro action */
  act = macro__act[k];
  
  /* Get the length of the action */
  n = strlen(act);
  
  /* Push the macro "action" onto the key queue */
  while (n > 0)
    {
      /* Push the key, notice over-flow */
      if (Term_key_push(act[--n])) return (ke0);
    }
  
  
  /* Hack -- Force "inkey()" to call us again */
  return (ke0);
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
char (*inkey_hack)(int flush_first) = NULL;

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
 * If "angband_term[0]" is not active, we will make it active during this
 * function, so that the various "main-xxx.c" files can assume that input
 * is only requested (via "Term_inkey()") when "angband_term[0]" is active.
 *
 * Mega-Hack -- This function is used as the entry point for clearing the
 * "signal_count" variable, and of the "character_saved" variable.
 *
 * Hack -- Note the use of "inkey_next" to allow "keymaps" to be processed.
 *
 * Mega-Hack -- Note the use of "inkey_hack" to allow the "Borg" to steal
 * control of the keyboard from the user.
 */
key_event inkey_ex(void)
{
  int cursor_state;
  
  key_event kk;
  
  key_event ke;
  
  bool done = FALSE;
  
  term *old = Term;
  
  
  /* Initialise keypress */
  ke.key = 0;
  
  /* Hack -- Use the "inkey_next" pointer */
  if (inkey_next && *inkey_next && !inkey_xtra)
    {
      /* Get next character, and advance */
      ke.key = *inkey_next++;
      
      /* Cancel the various "global parameters" */
      inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;
      
      /* Accept result */
      return (ke);
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
      return (ke);
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
  
  
  /* Get the cursor state */
  (void)Term_get_cursor(&cursor_state);
  
  /* Show the cursor if waiting, except sometimes in "command" mode */
  if (!inkey_scan && (!inkey_flag || hilite_player || character_icky))
    {
      /* Show the cursor */
      (void)Term_set_cursor(TRUE);
    }
  
  
  /* Hack -- Activate main screen */
  Term_activate(term_screen);
  
  
  /* Get a key */
  while (!ke.key)
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
	      if (0 == Term_inkey(&ke, TRUE, TRUE))
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
	      if (0 == Term_inkey(&ke, FALSE, TRUE))
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
      ke = inkey_aux();
      
      
      /* Handle "control-right-bracket" */
      if (ke.key == 29)
	{
	  /* Strip this key */
	  ke.key = 0;
	  
	  /* Continue */
	  continue;
	}
      
      
      /* Treat back-quote as escape */
      if (ke.key == '`') ke.key = ESCAPE;
      
      
      /* End "macro trigger" */
      if (parse_under && (ke.key <= 32))
	{
	  /* Strip this key */
	  ke.key = 0;
	  
	  /* End "macro trigger" */
	  parse_under = FALSE;
	}
      
      
      /* Handle "control-caret" */
      if (ke.key == 30)
	{
	  /* Strip this key */
	  ke.key = 0;
	}
      
      /* Handle "control-underscore" */
      else if (ke.key == 31)
	{
	  /* Strip this key */
	  ke.key = 0;
	  
	  /* Begin "macro trigger" */
	  parse_under = TRUE;
	}
      
      /* Inside "macro trigger" */
      else if (parse_under)
	{
	  /* Strip this key */
	  ke.key = 0;
	}
    }
  
  
  /* Hack -- restore the term */
  Term_activate(old);
  
  
  /* Restore the cursor */
  Term_set_cursor(cursor_state);
  
  
  /* Cancel the various "global parameters" */
  inkey_base = inkey_xtra = inkey_flag = inkey_scan = FALSE;
  
  
  /* Return the keypress */
  return (ke);
}


/*
 * Get a keypress or mouse click from the user.
 */
char anykey(void)
{
  key_event ke;
  
  /* Only accept a keypress or mouse click*/
  do
    {
      ke = inkey_ex();
    } while ((ke.key == '\xff') && !(ke.mousebutton));
  
  return ke.key;
}

/*
 * Get a keypress from the user.
 */
char inkey(void)
{
	key_event ke;

	/* Only accept a keypress */
	do
	{
		ke = inkey_ex();
	} while (ke.key == '\xff');

	return ke.key;
}



/*
 * Flush the screen, make a noise
 */
void bell(cptr reason)
{
  /* Mega-Hack -- Flush the output */
  Term_fresh();
  
  /* Hack -- memorize the reason if possible */
  if (character_generated && reason) message_add(reason, MSG_BELL);
  
  /* Make a bell noise (if allowed) */
  if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);
  
  /* Flush the input (later!) */
  flush();
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
 * Some code uses "zero" to indicate the non-existance of a quark.
 *
 * Note that "quark zero" is NULL and should never be "dereferenced".
 */

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
  
  /* Access the quark */
  q = quark__str[i];
  
  /* Return the quark */
  return (q);
}


/*
 * Initialize the "quark" package
 */
errr quark_init(void)
{
  /* Quark variables */
  C_MAKE(quark__str, QUARK_MAX, cptr);
  
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
 * The array[MESSAGE_MAX] of u16b for the count of messages
 */
static u16b *message__count;




/*
 * How many messages are "available"?
 */
s16b message_num(void)
{
  /* Determine how many messages are "available" */
  return (message__next + MESSAGE_MAX - message__last) % MESSAGE_MAX;
}



/*
 * Recall the "text" of a saved message
 */
cptr message_str(s16b age)
{
  static char buf[1024];
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
  
  /* HACK - Handle repeated messages */
  if (message__count[x] > 1)
    {
      strnfmt(buf, sizeof(buf), "%s <%dx>", s, message__count[x]);
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
  
  /* Forgotten messages have no special color */
  if ((age < 0) || (age >= message_num())) return (TERM_WHITE);
  
  /* Get the "logical" index */
  x = (message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX;
  
  /* Return the message type */
  return (message__type[x]);
}

/*
 * Recall the "color" of a saved message
 */
byte message_color(s16b age)
{
  return message__color[message_type(age)];
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
 *
 * We attempt to minimize the use of "string compare" operations in this
 * function, because they are expensive when used in mass quantities.
 */
void message_add(cptr str, u16b type)
{
  int n, k, i, x, o;
  
  cptr s;
  cptr t;
  cptr u;
  char *v;
  

  /*** Step 1 -- Analyze the message ***/
  
  /* Hack -- Ignore "non-messages" */
  if (!str) return;
  
  /* Message length */
  n = strlen(str);
  
  /* Hack -- Ignore "long" messages */
  if (n >= MESSAGE_BUF / 4) return;
  
  /*** Step 1 1/2 -- Attempt to optimize ***/
  
  /* Get the "logical" last index */
  x = (message__next + MESSAGE_MAX - 1) % MESSAGE_MAX;
  
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
  
  /*** Step 2 -- Attempt to optimize ***/
  
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
      
      /* Inline 'streq(str, old)' */
      for (s = str, t = old; (*s == *t) && *s; ++s, ++t) /* loop */ ;
      
      /* Continue if not equal */
      if (*s || *t) continue;
      
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
  
  /*** Step 3 -- Ensure space before end of buffer ***/
  
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
  
  /*** Step 4 -- Ensure space for actual characters ***/
  
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
  
  
  /*** Step 5 -- Grab a new message index ***/
  
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
  
  /*** Step 6 -- Insert the message text ***/
  
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
errr message_init(void)
{
  /* Message variables */
  C_MAKE(message__ptr, MESSAGE_MAX, u16b);
  C_MAKE(message__buf, MESSAGE_BUF, char);
  C_MAKE(message__type, MESSAGE_MAX, u16b);
  C_MAKE(message__count, MESSAGE_MAX, u16b);
  
  /* Init the message colors to white */
  (void)C_BSET(message__color, TERM_WHITE, MSG_MAX, byte);
  
  /* Hack -- No messages yet */
  message__tail = MESSAGE_BUF;
  
  /* Success */
  return (0);
}




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



/*
 * Hack -- flush
 */
static void msg_flush(int x)
{
  byte a = TERM_L_BLUE;
  
  /* Pause for response */
  Term_putstr(x, 0, -1, a, "-more-");
  
  /* Get an acceptable keypress */
  while (1)
    {
      key_event cmd = inkey_ex();
      if (quick_messages) break;
      if ((cmd.key == '\xff') && (cmd.mousex >= x) && 
	  (cmd.mousex < x + 6) && (!cmd.mousey)) 
	break;
      if ((cmd.key == ESCAPE) || (cmd.key == ' ')) break;
      if ((cmd.key == '\n') || (cmd.key == '\r')) break;
      bell("Illegal response to a 'more' prompt!");
    }
  
  /* Clear the line */
  Term_erase(0, 0, 255);
}


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
  static int p = 0;
  int n;
  int trunc_len, check_len;
  char *t;
  char buf[1024];
  byte color = TERM_WHITE;
  
  
  /* Set length to try and truncate */
  trunc_len = Term->wid - 8;

  /* Set length to start looking for split ponts */
  check_len = Term->wid - 40;
  if (check_len < 0) check_len = 6;

  /* Hack -- Reset */
  if (!msg_flag) p = 0;
  
  /* Message Length */
  n = (msg ? strlen(msg) : 0);
  
  /* Hack -- flush when requested or needed */
  if (p && (!msg || ((p + n) > trunc_len)))
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
  
  
  /* Memorize the message (if legal) */
  if (character_generated && !(p_ptr->is_dead))
    message_add(msg, type);

  /* Window stuff */
  p_ptr->window |= (PW_MESSAGE);
  
  /* Handle "auto_more" */
  if (auto_more)
    {
      /* Force window update */
      window_stuff();
      
      /* Done */
      return;
    }
  
  
  /* Copy it */
  strcpy(buf, msg);
  
  /* Analyze the buffer */
  t = buf;
  
  /* Get the color of the message (if legal) */
  if (message__color)
    color = message__color[type];
  
  /* HACK -- no "black" messages */
  if (color == TERM_DARK) color = TERM_WHITE;
  
  /* Split message */
  while (n > trunc_len)
    {
      char oops;
      
      int check, split;
      
      /* Default split */
      split = trunc_len;
      
      /* Find the "best" split point */
      for (check = check_len; check < trunc_len; check++)
	{
	  /* Found a valid split point */
	  if (t[check] == ' ') split = check;
	}
      
      /* Save the split character */
      oops = t[split];

      /* Split the message */
      t[split] = '\0';
      
      /* Display part of the message */
      Term_putstr(0, 0, split, color, t);
      
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
  Term_putstr(p, 0, n, color, t);
  
  /* Remember the message */
  msg_flag = TRUE;
  
  /* Remember the position */
  p += n + 1;
  
  /* Optional refresh */
  if (fresh_after) Term_fresh();
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
  (void)vstrnfmt(buf, 1024, fmt, vp);
  
  /* End the Varargs Stuff */
  va_end(vp);
  
  /* Display */
  msg_print_aux(MSG_GENERIC, buf);
}


/*
 * Display a message and play the associated sound.
 *
 * The "extra" parameter is currently unused.
 */
void message(u16b message_type, s16b extra, cptr message)
{
  sound(message_type);
  
  msg_print_aux(message_type, message);
}

/*
 * Display a formatted message and play the associated sound.
 *
 * The "extra" parameter is currently unused.
 */
void message_format(u16b message_type, s16b extra, cptr fmt, ...)
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
  message(message_type, extra, buf);
}

/*
 * Hack -- prevent "accidents" in "screen_save()" or "screen_load()"
 */
static int screen_depth = 0;


/*
 * Save the screen, and increase the "icky" depth.
 *
 * This function must match exactly one call to "screen_load()".
 */
void screen_save(void)
{
  /* Hack -- Flush messages */
  msg_print(NULL);
  
  /* Save the screen (if legal) */
  if (screen_depth++ == 0) Term_save();
  
  /* Increase "icky" depth */
  character_icky++;
}


/*
 * Load the screen, and decrease the "icky" depth.
 *
 * This function must match exactly one call to "screen_save()".
 */
void screen_load(void)
{
  /* Hack -- Flush messages */
  msg_print(NULL);
  
  /* Hack - wipe screen for big graphics */
  //if (use_dbltile || use_trptile)
  //clear_from(0); 
  
  /* Load the screen (if legal) */
  if (--screen_depth == 0) Term_load();

  /* Decrease "icky" depth */
  character_icky--;
  
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
  Term_putstr(col, row, -1, TERM_WHITE, str);
}


/*
 * As above, but centered horizontally
 */
void put_str_center(cptr str, int row)
{
  int len = strlen(str);
  int col = (Term->wid - len) / 2;
  
  Term_putstr(col, row, -1, TERM_WHITE, str);
}


/*
 * Display a string on the screen using an attribute, and clear
 * to the end of the line.
 */
void c_prt(byte attr, cptr str, int row, int col)
{
  /* Clear line, position cursor */
  Term_erase(col, row, 255);
  
  /* Dump the attr/text */
  Term_addstr(-1, attr, str);
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
 * As above, but in "white"
 */
void prt_center(cptr str, int row)
{
  int len = strlen(str);
  int col = (Term->wid - len) / 2;
  
  /* Spawn */
  c_prt(TERM_WHITE, str, row, col);
}



/*
 * Print some (colored) text to the screen at the current cursor position,
 * automatically "wrapping" existing text (at spaces) when necessary to
 * avoid placing any text into the last column, and clearing every line
 * before placing any text in that line.  Also, allow "newline" to force
 * a "wrap" to the next line.  Advance the cursor as needed so sequential
 * calls to this function will work correctly.
 *
 * Accept values for left and right margins. -LM-
 *
 * Once this function has been called, the cursor should not be moved
 * until all the related "c_roff()" calls to the window are complete.
 *
 * This function will correctly handle any width up to the maximum legal
 * value of 256, though it works best for a standard 80 character width.
 */
void c_roff(byte a, cptr str, byte l_margin, byte r_margin)
{
  int x, y;
  
  int w, h;
  
  cptr s;
  
  
  /* Obtain the size */
  (void)Term_get_size(&w, &h);
  
  /* Accept right margin if provided and legal. */
  if ((r_margin) && (r_margin <= w)) w = r_margin;
  
  /* Obtain the cursor. */
  (void)Term_locate(&x, &y);
  
  /* If cursor is left of the left margin,... */
  if ((l_margin) && (x < l_margin)) 
    {
      /* ...move it to the left margin. */
      move_cursor(y, l_margin);
      
      /* Increment column count. */
      x = l_margin;
    }
  
  
  /* Process the string */
  for (s = str; *s; s++)
    {
      char ch;
      
      /* Force wrap */
      if (*s == '\n')
	{
	  /* Wrap */
	  x = l_margin;
	  y++;
	  
	  /* Clear line, move cursor */
	  Term_erase(x, y, 255);
	  
	  /* Go immediately to the next character. */
	  continue;
	}
      
      /* Clean up the char */
      ch = (isprint(*s) ? *s : ' ');
      
      /* Wrap words as needed */
      if ((x >= w - 1) && (ch != ' '))
	{
	  int i, n = l_margin;
	  
	  byte av[256];
	  char cv[256];
	  
	  /* Wrap word */
	  if (x < w)
	    {
	      /* Scan existing text */
	      for (i = w - 2; i >= l_margin; i--)
		{
		  /* Grab existing attr/char */
		  Term_what(i, y, &av[i], &cv[i]);
		  
		  /* Break on space */
		  if (cv[i] == ' ') break;
		  
		  /* Track current word */
		  n = i;
		}
	    }
	  
	  /* Special case */
	  if (n == l_margin) n = w;
	  
	  /* Clear line */
	  Term_erase(n, y, 255);
	  
	  /* Wrap */
	  x = l_margin;
	  y++;
	  
	  /* Clear line, move cursor */
	  Term_erase(x, y, 255);
	  
	  /* Wrap the word (if any) */
	  for (i = n; i < w - 1; i++)
	    {
	      /* Dump */
	      Term_addch(av[i], cv[i]);
	      
	      /* Advance (no wrap) */
	      if (++x > w) x = w;
	    }
	}
      
      /* Dump */
      Term_addch(a, ch);
      
      /* Advance. */
      if (++x > w) x = w;
    }
}


/*
 * As above, but in "white"
 */
void roff(cptr str, byte l_margin, byte r_margin)
{
  /* Spawn */
  c_roff(TERM_WHITE, str, l_margin, r_margin);
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
 * The default buffer is displayed in yellow until cleared.
 * Pressing RETURN right away accepts the default entry.
 * Normal chars clear the default and append the char.
 * Backspace clears the default or deletes the final char.
 * ESCAPE clears the buffer and the window and returns FALSE.
 * RETURN accepts the current buffer contents and returns TRUE.
 * The buffer must be large enough for 'len+1' characters.
 */
bool askfor_aux(char *buf, int len)
{
  int y, x, wid;
  
  int k = 0;
  
  key_event ke;
  
  bool done = FALSE;

  /* Set max width */
  wid = (small_screen ? 48 : 80);
  
  ke.key = '\0';
  
  /* Locate the cursor */
  Term_locate(&x, &y);
  

  /* Paranoia -- check len */
  if (len < 1) len = 1;
  
  /* Paranoia -- check column */
  if ((x < 0) || (x >= wid)) x = 0;
  
  /* Restrict the length */
  if (x + len > wid) len = wid - x;
  
  
  /* Paranoia -- Clip the default entry */
  buf[len-1] = '\0';
  
  
  /* Display the default answer */
  Term_erase(x, y, len);
  Term_putstr(x, y, -1, TERM_YELLOW, buf);
  
  
  /* Process input */
  while (!done)
    {
      /* Place cursor */
      Term_gotoxy(x + k, y);
      
      /* Get a key */
      ke = inkey_ex();
      
      /* Analyze the key */
      switch (ke.key)
	{
	case ESCAPE:
	  {
	    k = 0;
	    done = TRUE;
	    break;
	  }
	  
	case '\n':
	case '\r':
	  {
	    k = strlen(buf);
	    done = TRUE;
	    break;
	  }
	  
	case 0x7F:
	case '\010':
	  {
	    if (k > 0) k--;
	    break;
	  }
	  
	case '\xff':
	  {
	    /* Horriblest hack in world history */
	    if ((!ke.mousey) && (ke.mousex > (small_screen ? 30 : 43)))
	      {
		buf[0] = '$';
		k = strlen(buf);
		done = TRUE;
	      }
	    else if ((ke.mousebutton) && !(k))
	      {
		if (ke.mousebutton == 1)
		  {
		    k = strlen(buf);
		    done = TRUE;
		  }
		else if (ke.mousebutton == 2)
		  {
		    ke.key = ESCAPE;
		    done = TRUE;
		  }
		break;
	      }
	    else continue;
	  }
	  
	default:
	  {
	    if ((k < len - 1) && (isprint(ke.key)))
	      {
		buf[k++] = ke.key;
	      }
	    else
	      {
		bell("Illegal edit key!");
	      }
	    break;
	  }
	}
      
      /* Terminate */
      buf[k] = '\0';
      
      /* Update the entry */
      Term_erase(x, y, len);
      Term_putstr(x, y, -1, TERM_WHITE, buf);
    }
  
  /* Aborted */
  if (ke.key == ESCAPE) return (FALSE);
  
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
 * Request a "quantity" from the user
 *
 * Allow "p_ptr->command_arg" to specify a quantity
 */
s16b get_quantity(cptr prompt, int max)
{
  int amt = 1;
  
  
  /* Use "command_arg" */
  if (p_ptr->command_arg)
    {
      /* Extract a number */
      amt = p_ptr->command_arg;
      
      /* Clear "command_arg" */
      p_ptr->command_arg = 0;
    }
#ifdef ALLOW_REPEAT /* TNB */
  
  /* Get the item index */
  else if ((max != 1) && repeat_pull(&amt)) {
  }
  
#endif /* ALLOW_REPEAT */
  
  
  /* Prompt if needed - changed for mouse -NRM- */
  else if (max != 1)
    {
      char tmp[80];
      
      char buf[6];
      
      int y, x;
  
      int d, j, k = 0, c = 0;
  
      key_event ke;
  
      bool done = FALSE;
  
      ke.key = '\0';

      /* Get the click offset */
      for (j = 10; j < 1000000; j *=10)
	if (max >= j) c++;
  
      /* Build a prompt if needed */
      if (!prompt)
	{
	  /* Build a prompt */
	  sprintf(tmp, "Quantity (0-%d) ++/--/ESC: ", max);
	  
	  /* Use that prompt */
	  prompt = tmp;
	}
      
      /* Paranoia XXX XXX XXX */
      msg_print(NULL);
      
      /* Display prompt */
      prt(prompt, 0, 0);
  
      /* Build the default */
      sprintf(buf, "%d", amt);
      
      /* Ask for a quantity 
	 if (!get_string(prompt, buf, 6)) return (0); */

      /* Locate the cursor */
      Term_locate(&x, &y);
  
      /* Paranoia -- check column */
      if ((x < 0) || (x >= 80)) x = 0;
  
      /* Paranoia -- Clip the default entry */
      buf[5] = '\0';
  
  
      /* Display the default answer */
      Term_erase(x, y, 6);
      Term_putstr(x, y, -1, TERM_YELLOW, buf);
  
  
      /* Process input */
      while (!done)
	{
	  /* Place cursor */
	  Term_gotoxy(x + k, y);
	  
	  /* Get a key */
	  ke = inkey_ex();
	  
	  /* Analyze the key */
	  switch (ke.key)
	    {
	    case ESCAPE:
	      {
		k = 0;
		done = TRUE;
		break;
	      }
	      
	    case '\n':
	    case '\r':
	      {
		k = strlen(buf);
		done = TRUE;
		break;
	      }
	      
	    case 0x7F:
	    case '\010':
	      {
		if (k > 0) k--;
		break;
	      }
	      
	    case '\xff':
	      {
		if (!ke.mousey)
		  {
		    if ((ke.mousex > 14 + c) && (ke.mousex < 17 + c))
		      {
			bool writing = FALSE;

			k = 0;
			amt++;
			if (amt > max) amt = 0;
			for(j = 100000; j >= 1; j /= 10)
			  {
			    if ((amt >= j) || (writing))
			      {
				writing = TRUE;
				d = amt / j;
				amt -= d * j;
				buf[k++] = I2D(d);
			      }
			  }
			buf[k] = '\0';
			break;
		      }
		    else if ((ke.mousex > 17 + c) && (ke.mousex < 20 + c))
		      {
			bool writing = FALSE;

			k = 0;
			amt--;
			if (amt < 0) amt = max;
			for(j = 100000; j >= 1; j /= 10)
			  {
			    if ((amt >= j) || (writing))
			      {
				writing = TRUE;
				d = amt / j;
				amt -= d * j;
				buf[k++] = I2D(d);
			      }
			  }
			buf[k] = '\0';
			break;
		      }
		    else if ((ke.mousex > 20 + c) && (ke.mousex < 24 + c))
		      {
			k = 0;
			done = TRUE;
			break;
		      }
		    else continue;
		  }
		else		  
		  {
		    k = strlen(buf);
		    done = TRUE;
		    break;
		  }
	      }
	      
	    default:
	      {
		if ((k < 5) && (isdigit(ke.key)))
		  {
		    buf[k++] = ke.key;
		  }
		else
		  {
		    bell("Illegal edit key!");
		  }
		break;
	      }

	    }
	  
	  /* Terminate */
	  buf[k] = '\0';
	  
	  /* Extract a number */
	  amt = atoi(buf);

	  /* Update the entry */
	  Term_erase(x, y, 6);
	  Term_putstr(x, y, -1, TERM_WHITE, buf);
	}
      
      
      /* Extract a number */
      amt = atoi(buf);
      
    }
  
  /* Enforce the maximum */
  if (amt > max) amt = max;
  
  /* Enforce the minimum */
  if (amt < 0) amt = 0;
  
#ifdef ALLOW_REPEAT /* TNB */
  
  if (amt) repeat_push(amt);
  
#endif /* ALLOW_REPEAT */
  
  /* Clear prompt */
  prt("", 0, 0);
  
  /* Return the result */
  return (amt);
}


/*
 * Verify something with the user
 *
 * The "prompt" should take the form "Query? "
 *
 * Note that "[y/n]" is appended to the prompt.
 */
bool get_check(cptr prompt)
{
  key_event ke;
  
  char buf[80];
  
  /* Paranoia XXX XXX XXX */
  msg_print(NULL);
  
  /* Hack -- Build a "useful" prompt */
  if (small_screen)
    strnfmt(buf, 48, "%.38s ['y'/'n']", prompt);
  else
    strnfmt(buf, 78, "%.70s ['y'/'n']", prompt);
  
  /* Prompt for it */
  prt(buf, 0, 0);
  
  /* Get an acceptable answer */
  while (TRUE)
    {
      ke = inkey_ex();
      if ((ke.key == '\xff') && (!ke.mousey))
	{
	  if ((ke.mousex > strlen(buf) - 9) && (ke.mousex < strlen(buf) - 5))
	    ke.key = 'y';
	  else if ((ke.mousex > strlen(buf) - 5) && 
		   (ke.mousex < strlen(buf) - 1))
	    ke.key = 'n';
	}
      if (ke.key == ESCAPE) break;
      if (strchr("YyNn", ke.key)) break;
      if (quick_messages) break;
      bell("Illegal response to a 'yes/no' question!");
    }
  
  /* Erase the prompt */
  prt("", 0, 0);
  
  /* Normal negation */
  if ((ke.key != 'Y') && (ke.key != 'y')) return (FALSE);
  
  /* Success */
  return (TRUE);
}


/*
 * Hack - duplication of get_check prompt to give option of setting destroyed
 * option to squelch.
 *
 * 0 - No
 * 1 = Yes
 * 2 = third option
 *
 * The "prompt" should take the form "Query? "
 *
 * Note that "[y/n/{char}]" is appended to the prompt.
 */
int get_check_other(cptr prompt, char other)
{
  key_event ke;
  
  char buf[80];
  
  /* default set to no */
  int result = 0;
  
  /* Paranoia XXX XXX XXX */
  msg_print(NULL);

  /* Hack -- Build a "useful" prompt */
  if (small_screen)
    strnfmt(buf, 48, "%.34s ['y'/'n'/'%c']", prompt, other);
  else
    strnfmt(buf, 78, "%.64s ['y'/'n'/'%c']", prompt, other);
  
  /* Prompt for it */
  prt(buf, 0, 0);
  
  /* Get an acceptable answer */
  while (TRUE)
    {
      ke = inkey_ex();
      if ((ke.key == '\xff') && (!ke.mousey))
	{
	  if ((ke.mousex > strlen(buf) - 13) && (ke.mousex < strlen(buf) - 9))
	    ke.key = 'y';
	  else if ((ke.mousex > strlen(buf) - 9) && 
		   (ke.mousex < strlen(buf) - 5))
	    ke.key = 'n';
	  else if ((ke.mousex > strlen(buf) - 5) && 
		   (ke.mousex < strlen(buf) - 1))
	    ke.key = other;
	}
      if (ke.key == ESCAPE) break;
      if (strchr("YyNn", ke.key)) break;
      if (ke.key == toupper(other)) break;
      if (ke.key == tolower(other)) break;
      if (quick_messages) break;
      bell("Illegal response to question!");
    }
  
  /* Erase the prompt */
  prt("", 0, 0);
  
  /* Normal negation */
  if ((ke.key == 'Y') || (ke.key == 'y')) result = 1;
  /*other option*/
  else if ((ke.key == toupper(other)) || (ke.key == tolower(other))) 
    result = 2;
  /*all else default to no*/
  
  /* Success */
  return (result);
}


/*
 * Prompts for a keypress
 *
 * The "prompt" should take the form "Command: "
 *
 * Returns TRUE unless the character is "Escape"
 */
bool get_com(cptr prompt, char *command)
{
  key_event ke;
  bool result;

  result = get_com_ex(prompt, &ke);
  *command = ke.key;

  if (ke.key == '\xff')
    bell("Potential loss of mouse input");
  return result;
}

/*
 * Prompts for a keypress or mouse press
 *
 * The "prompt" should take the form "Command: "
 *
 * Returns TRUE unless the character is "Escape"
 */
bool get_com_ex(cptr prompt, key_event *command)
{
  key_event ke;
  
  /* Paranoia XXX XXX XXX */
  msg_print(NULL);
  
  /* Display a prompt */
  prt(prompt, 0, 0);
  
  /* Get a key */
  ke = inkey_ex();
  
  /* Clear the prompt */
  prt("", 0, 0);
  
  /* Save the command */
  *command = ke;

  /* Handle "cancel" */
  if (ke.key == ESCAPE) return (FALSE);
  
  /* Success */
  return (TRUE);
}


/*
 * Pause for user response
 *
 * This function is stupid.  XXX XXX XXX
 */
void pause_line(int row)
{
  prt("", row, 0);
  put_str("[Press any key to continue]", row, (small_screen ? 11 : 23));
  (void) inkey_ex();
  prt("", row, 0);
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
 * Note that "p_ptr->command_new" may not work any more.  XXX XXX XXX
 */
void request_command(bool shopping)
{
  int i;
  
  key_event ke;
  
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
  
  /* No "direction" yet, unless repeated. */
  p_ptr->command_dir = 0;
  
  
  /* Get command */
  while (1)
    {
      /* Hack -- auto-commands */
      if (p_ptr->command_new)
	{
	  /* Flush messages */
	  msg_print(NULL);
	  
	  /* Use auto-command */
	  ke.key = (char)p_ptr->command_new;
	  
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
	  ke = inkey_ex();
	}
      
      /* Clear top line */
      if ((ke.key != '\xff') || (ke.mousebutton)) prt("", 0, 0);

      
      /* Command Count */
      if (ke.key == '0')
	{
	  int old_arg = p_ptr->command_arg;
	  
	  /* Reset */
	  p_ptr->command_arg = 0;
	  
	  /* Begin the input */
	  prt("Count (-- ++): ", 0, 0);
	  
	  /* Get a command count */
	  while (1)
	    {
	      /* Get a new keypress */
	      ke = inkey_ex();
	      
	      /* Simple editing (delete or backspace) */
	      if ((ke.key == 0x7F) || (ke.key == KTRL('H')))
		{
		  /* Delete a digit */
		  p_ptr->command_arg = p_ptr->command_arg / 10;
		  
		  /* Show current count */
		  prt(format("Count (-- ++): %d", p_ptr->command_arg), 0, 0);
		}
	      
	      /* Actual numeric data */
	      else if (ke.key >= '0' && ke.key <= '9')
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
		      p_ptr->command_arg = p_ptr->command_arg * 10 + 
			D2I(ke.key);
		    }
		  
		  /* Show current count */
		  prt(format("Count (-- ++): %d", p_ptr->command_arg), 0, 0);
		}
	      
	      /* Mousepress data */
	      else if (ke.key == '\xff')
		{
		  /* Increment or decrement */
		  if (!ke.mousey)
		    {
		      if ((ke.mousex > 6) && (ke.mousex < 9))
			{
			  if (p_ptr->command_arg == 0)
			    {
			      /* Warn */
			      bell("Invalid repeat count!");
			    }
			  else
			    {
			      p_ptr->command_arg--;
			    }
			}
		      else if ((ke.mousex > 9) && (ke.mousex < 12))
			{
			  if (p_ptr->command_arg == 9999)
			    {
			      /* Warn */
			      bell("Invalid repeat count!");
			    }
			  else
			    {
			      p_ptr->command_arg++;
			    }
			}
		    }  
		  /* Show current count */
		  prt(format("Count (-- ++): %d", p_ptr->command_arg), 0, 0);
		
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
	      prt(format("Count (-- ++): %d", p_ptr->command_arg), 0, 0);
	    }
	  
	  /* Hack -- Handle "old_arg" */
	  if (old_arg != 0)
	    {
	      /* Restore old_arg */
	      p_ptr->command_arg = old_arg;
	      
	      /* Show current count */
	      prt(format("Count (-- ++): %d", p_ptr->command_arg), 0, 0);
	    }
	  
	  /* Hack -- white-space means "enter command now" */
	  if ((ke.key == ' ') || (ke.key == '\n') || (ke.key == '\r'))
	    {
	      /* Get a real command */
	      if (!get_com("Command: ", &ke.key))
		{
		  /* Clear count */
		  p_ptr->command_arg = 0;
		  
		  /* Continue */
		  continue;
		}
	    }
	}
      
      
      /* Allow "keymaps" to be bypassed */
      if (ke.key == '\\')
	{
	  /* Get a real command */
	  (void)get_com("Command: ", &ke.key);
	  
	  /* Hack -- bypass keymaps */
	  if (!inkey_next) inkey_next = "";
	}
      
      
      /* Allow "control chars" to be entered */
      if (ke.key == '^')
	{
	  /* Get a new command and controlify it */
	  if (get_com("Control: ", &ke.key)) ke.key = KTRL(ke.key);
	}
      
      
      /* Look up applicable keymap */
      act = keymap_act[mode][(byte)(ke.key)];
      
      /* Apply keymap if not inside a keymap already */
      if (act && !inkey_next)
	{
	  /* Install the keymap (limited buffer size) */
	  strnfmt(request_command_buffer, 256, "%s", act);
	  
	  /* Start using the buffer */
	  inkey_next = request_command_buffer;
	  
	  /* Continue */
	  continue;
	}
      
      
      /* Paranoia */
      if (ke.key == '\0') continue;
      
      
      /* Use command */
      p_ptr->command_cmd = ke.key;
      p_ptr->command_cmd_ex = ke;

      /* Hack - avoid warning message in shops */
      if ((ke.key == '\xff') && (shopping) && (ke.mousey != 18) &&
	  (ke.mousey != 21) && (ke.mousey != 22) && (ke.mousey != 23)) 
	continue;

      /* Done */
      break;
    }
  
  /* Hack -- Auto-repeat certain commands */
  if (always_repeat && (p_ptr->command_arg <= 0))
    {
      /* Hack -- auto repeat certain commands */
      if (strchr("TBDoc+", p_ptr->command_cmd))
	{
	  /* Repeat 99 times */
	  p_ptr->command_arg = 99;
	}
    }
  
  
  /* Shopping */
  if (shopping)
    {
      /* Take mouse input */
      if (ke.key == '\xff')
	{
	  if (ke.mousey == 18)
	    p_ptr->command_cmd = ' ';
	  if (ke.mousey == 21)
	    {
	      if ((ke.mousex > (small_screen ? 20 : 28)) && 
		  (ke.mousex < (small_screen ? 36 : 55))) 
		p_ptr->command_cmd = '\r';
	      if (ke.mousex > (small_screen ? 36 : 54)) 
		p_ptr->command_cmd = 'i';
	    }
	  if (ke.mousey == 22)
	    {
	      if (ke.mousex < (small_screen ? 20 : 29)) 
		p_ptr->command_cmd = ESCAPE;
	      if ((ke.mousex > (small_screen ? 20 : 28)) && 
		  (ke.mousex < (small_screen ? 36 : 55))) 
		p_ptr->command_cmd = 'g';
	      if (ke.mousex > (small_screen ? 36 : 54)) 
		p_ptr->command_cmd = 'l';
	    }
	  if (ke.mousey == 23)
	    {
	      if (ke.mousex < (small_screen ? 20 : 29)) 
		p_ptr->command_cmd = ' ';
	      if ((ke.mousex > (small_screen ? 20 : 28)) && 
		  (ke.mousex < (small_screen ? 36 : 55))) 
		p_ptr->command_cmd = 'd';
	      if (ke.mousex > (small_screen ? 36 : 54)) 
		p_ptr->command_cmd = 'o';
	    }
	}

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
      
      /* Obtain the inscription */
      s = quark_str(o_ptr->note);
      
      /* Find a '^' */
      s = strchr(s, '^');
      
      /* Process preventions */
      while (s)
	{
	  /* Check the "restriction" character */
	  if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
	    {
	      /* Hack -- Verify command */
	      if (!get_check("Are you sure? "))
		{
		  /* Hack -- Use space */
		  p_ptr->command_cmd = ' ';
		}
	    }
	  
	  /* Find another '^' */
	  s = strchr(s + 1, '^');
	}
    }

  /* Hack -- erase the message line. */
  if ((ke.key != '\xff') || (ke.mousebutton)) prt("", 0, 0);
  
  /* Hack again -- apply the modified key command */
  p_ptr->command_cmd_ex.key = p_ptr->command_cmd;
}




/*
 * Generates damage for "2d6" style dice rolls
 */
uint damroll(uint num, uint sides)
{
  unsigned int i = 0;
  unsigned int sum = num;
  for (i = 0; i < num; i++)
    {
      sum += (rand_int(sides));
    }
  return (sum);
}


/*
 * Same as above, but always maximal
 */
uint maxroll(uint num, uint sides)
{
  return (num * sides);
}

int get_recall_pt(cptr reason, bool extra)
{

  char stage[19];
  
  int i, region, level, new = 0;
  
  key_event choice;
  
  char *abcd[5] = {"a", "b", "c", "d", "e"};
  
  bool valid = FALSE;
  
  

  /* Save screen */
  screen_save();
  
  for (i = 0; i < 4; i++)
    {
      /* Get the recall point description */
      
      region = stage_map[p_ptr->recall[i]][LOCALITY];
      level  = stage_map[p_ptr->recall[i]][DEPTH];
      
      if (level)
	sprintf(stage, "%s %d   ", locality_name[region], level);
      else if (region)
	sprintf(stage, "%s Town   ", locality_name[region]);
      else
	sprintf(stage, "%s     ", locality_name[region]);	      
      
      put_str(format(" %s) %20s     ", abcd[i], stage), i + 1, (small_screen ? 10 : 30));
    }
  if (extra)
    put_str(format(" e) Don't replace     ", stage), i + 1, (small_screen ? 10 : 30));
  
  
  while(!valid){
    if (get_com_ex(reason, &choice))
      {
	/* Mouse input */
	if (choice.key == '\xff') {
	  if (!choice.mousey)
	    {
	      /* ESC */
	      new = 0;
	      screen_load();
	      return 0;
	    }
	  else 
	    choice.key = 'a' + choice.mousey - 1;
	}

	switch (choice.key)
	  {
	  case 'a':
	    {
	      new = 1;
	      valid = TRUE;
	      break;
	    }
	  case 'b':
	    {
	      new = 2;
	      valid = TRUE;
	      break;
	    }
	  case 'c':
	    {
	      new = 3;
	      valid = TRUE;
	      break;
	    }
	  case 'd':
	    {
	      new = 4;
	      valid = TRUE;
	      break;
	    }
	  case 'e':
	    {
	      new = 5;
	      valid = extra;
	      break;
	    }
	  }
      }
    else /* ESC */
      {
	new = 0;
	screen_load();
	return 0;
      }
  }
  
  /* Load screen */
  screen_load();
  
  /* Got it */
  return new;
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





#define REPEAT_MAX             20

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

void repeat_clear()
{
  /* Start over from the failed pull */
  if (repeat__idx) repeat__cnt = --repeat__idx;
  
  /* Paranioa */
  else repeat__cnt = repeat__idx;
  
  return;
}

/*
 * From Tim Baker's Easy Patch.
 */
void repeat_check(void)
{
  int what;
  
  /* Ignore some commands */
  if (p_ptr->command_cmd == ESCAPE) return;
  if (p_ptr->command_cmd == ' ') return;
  if (p_ptr->command_cmd == '\r') return;
  if (p_ptr->command_cmd == '\n') return;

  /* Repeat Last Command */
  if ((p_ptr->command_cmd == 'n') || 
      ((mouse_buttons) && (p_ptr->command_cmd_ex.key == '\xff') &&
       (click_area(p_ptr->command_cmd_ex) == MOUSE_REPEAT)))
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

      what = p_ptr->command_cmd;
      
      /* Save this command */
      repeat_push(what);
    }
}

/*
 * Deal with pre-selected items from the item menu
 */
int handle_item(void)
{
  int item;

  /* Set the item */
  item = p_ptr->command_item;
  if (item == 100) item = 0;

  /* Handle repeat */  
  repeat_push(item);

  /* Reset */
  p_ptr->command_item = 0;
  p_ptr->command_new = 0;

  /* Done */
  return(item);
}



/* 
 * Convert an input from tenths of a pound to tenths of a kilogram. -LM-
 */
int make_metric(int wgt)
{
  int metric_wgt;
  
  /* Convert to metric values, using normal rounding. */
  metric_wgt = wgt * 10 / 22;
  if ((wgt * 10) % 22 > 10) metric_wgt++;
  
  return metric_wgt;
}


/*
 * Accept values for y and x (considered as the endpoints of lines) between 
 * 0 and 40, and return an angle in degrees (divided by two). -LM-
 *
 * This table's input and output needs some processing:
 *
 * Because this table gives degrees for a whole circle, up to radius 20, its 
 * origin is at (x,y) = (20, 20).  Therefore, the input code needs to find 
 * the origin grid (where the lines being compared come from), and then map 
 * it to table grid 20,20.  Do not, however, actually try to compare the 
 * angle of a line that begins and ends at the origin with any other line - 
 * it is impossible mathematically, and the table will return the value "255".
 *
 * The output of this table also needs to be massaged, in order to avoid the 
 * discontinuity at 0/180 degrees.  This can be done by:
 *   rotate = 90 - first value 
 *   this rotates the first input to the 90 degree line)
 *   tmp = ABS(second value + rotate) % 180
 *   diff = ABS(90 - tmp) = the angular difference (divided by two) between 
 *   the first and second values.
 */
byte get_angle_to_grid[41][41] = 
  {
    {  67,  66,  66,  66,  66,  66,  66,  65,  63,  62,  61,  60,  58,  57,  55,  54,  52,  50,  48,  46,  45,  44,  41,  40,  38,  36,  35,  33,  32,  30,  29,  28,  27,  25,  24,  24,  23,  23,  23,  23,  22 },
    {  68,  67,  66,  66,  66,  66,  66,  65,  63,  62,  61,  60,  58,  57,  55,  54,  52,  50,  49,  47,  45,  43,  41,  40,  38,  36,  35,  33,  32,  30,  29,  28,  27,  25,  24,  24,  23,  23,  23,  22,  21 },
    {  68,  68,  67,  66,  66,  66,  66,  65,  63,  62,  61,  60,  58,  57,  55,  54,  52,  50,  49,  47,  45,  43,  41,  40,  38,  36,  35,  33,  32,  30,  29,  28,  27,  25,  24,  24,  23,  23,  22,  21,  21 },
    {  68,  68,  68,  67,  66,  66,  66,  65,  63,  62,  61,  60,  58,  57,  55,  54,  52,  50,  49,  47,  45,  43,  41,  40,  38,  36,  35,  33,  32,  30,  29,  28,  27,  25,  24,  24,  23,  22,  21,  21,  21 },
    {  68,  68,  68,  68,  67,  66,  66,  65,  63,  62,  61,  60,  58,  57,  55,  54,  52,  50,  49,  47,  45,  43,  41,  40,  38,  36,  35,  33,  32,  30,  29,  28,  27,  25,  24,  24,  22,  21,  21,  21,  21 },
    {  68,  68,  68,  68,  68,  67,  66,  65,  64,  63,  62,  60,  59,  58,  56,  54,  52,  51,  49,  47,  45,  43,  41,  39,  38,  36,  34,  32,  31,  30,  28,  27,  26,  25,  24,  22,  21,  21,  21,  21,  21 },
    {  69,  69,  69,  69,  69,  68,  67,  66,  65,  64,  63,  61,  60,  58,  57,  55,  53,  51,  49,  47,  45,  43,  41,  39,  37,  35,  33,  32,  30,  29,  27,  26,  25,  24,  22,  21,  21,  21,  21,  21,  21 },
    {  70,  70,  70,  70,  70,  70,  69,  67,  66,  65,  64,  62,  61,  59,  57,  56,  54,  51,  49,  47,  45,  43,  41,  39,  36,  34,  33,  31,  29,  28,  26,  25,  24,  22,  21,  20,  20,  20,  20,  20,  20 },
    {  72,  72,  72,  72,   72,  71,  70,  69,  67,  66,  65,  63,  62,  60,  58,  56,  54,  52,  50,  47,  45,  43,  40,  38,  36,  34,  32,  30,  28,  27,  25,  24,  22,  21,  20,  19,  18,  18,  18,  18,  18 },
    {  73,  73,  73,  73,  73,  72,  71,  70,  69,  67,  66,  65,  63,  61,  59,  57,  55,  53,  50,  48,  45,  42,  40,  37,  35,  33,  31,  29,  27,  25,  24,  22,  21,  20,  19,  18,  17,  17,  17,  17,  17 },
    {  74,  74,  74,  74,  74,  73,  72,  71,  70,  69,  67,  66,  64,  62,  60,  58,  56,  53,  51,  48,  45,  42,  39,  37,  34,  32,  30,  28,  26,  24,  22,  21,  20,  19,  18,  17,  16,  16,  16,  16,  16 },
    {  75,  75,  75,  75,  75,  75,  74,  73,  72,  70,  69,  67,  66,  64,  62,  60,  57,  54,  51,  48,  45,  42,  39,  36,  33,  30,  28,  26,  24,  22,  21,  20,  18,  17,  16,  15,  15,  15,  15,  15,  15 },
    {  77,  77,  77,  77,  77,  76,  75,  74,  73,  72,  71,  69,  67,  66,  63,  61,  58,  55,  52,  49,  45,  41,  38,  35,  32,  29,  27,  24,  22,  21,  19,  18,  17,  16,  15,  14,  13,  13,  13,  13,  13 },
    {  78,  78,  78,  78,  78,  77,  77,  76,  75,  74,  73,  71,  69,  67,  65,  63,  60,  57,  53,  49,  45,  41,  37,  33,  30,  27,  25,  22,  21,  19,  17,  16,  15,  14,  13,  13,  12,  12,  12,  12,  12 },
    {  80,  80,  80,  80,  80,  79,  78,  78,  77,  76,  75,  73,  72,  70,  67,  65,  62,  58,  54,  50,  45,  40,  36,  32,  28,  25,  22,  20,  18,  17,  15,  14,  13,  12,  12,  11,  10,  10,  10,  10,  10 },
    {  81,  81,  81,  81,  81,  81,  80,  79,  79,  78,  77,  75,  74,  72,  70,  67,  64,  60,  56,  51,  45,  39,  34,  30,  26,  22,  20,  18,  16,  15,  13,  12,  11,  11,  10,   9,   9,   9,   9,   9,   9 },
    {  83,  83,  83,  83,  83,  83,  82,  81,  81,  80,  79,  78,  77,  75,  73,  71,  67,  63,  58,  52,  45,  38,  32,  27,  22,  19,  17,  15,  13,  12,  11,  10,   9,   9,   8,   7,   7,   7,   7,   7,   7 },
    {  85,  85,  85,  85,  85,  84,  84,  84,  83,  82,  82,  81,  80,  78,  77,  75,  72,  67,  62,  54,  45,  36,  28,  22,  18,  15,  13,  12,  10,   9,   8,   8,   7,   6,   6,   6,   5,   5,   5,   5,   5 },
    {  86,  86,  86,  86,  86,  86,  86,  86,  85,  85,  84,  84,  83,  82,  81,  79,  77,  73,  67,  58,  45,  32,  22,  17,  13,  11,   9,   8,   7,   6,   6,   5,   5,   4,   4,   4,   4,   4,   4,   4,   3 },
    {  87,  88,  88,  88,  88,  88,  88,  88,  88,  87,  87,  87,  86,  86,  85,  84,  83,  81,  77,  67,  45,  22,  13,   9,   7,   6,   5,   4,   4,   3,   3,   3,   2,   2,   2,   2,   2,   2,   2,   2,   1 },
    {  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90, 255,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 },
    {  91,  92,  92,  92,  92,  92,  92,  92,  92,  93,  93,  93,  94,  94,  95,  96,  97,  99, 103, 113, 135, 158, 167, 171, 173, 174, 175, 176, 176, 177, 177, 177, 178, 178, 178, 178, 178, 178, 178, 178, 179 },
    {  94,  94,  94,  94,  94,  94,  94,  94,  95,  95,  96,  96,  97,  98,  99, 101, 103, 107, 113, 122, 135, 148, 158, 163, 167, 169, 171, 172, 173, 174, 174, 175, 175, 176, 176, 176, 176, 176, 176, 176, 177 },
    {  95,  95,  95,  95,  95,  96,  96,  96,  97,  98,  98,  99, 100, 102, 103, 105, 108, 113, 118, 126, 135, 144, 152, 158, 162, 165, 167, 168, 170, 171, 172, 172, 173, 174, 174, 174, 175, 175, 175, 175, 175 },
    {  97,  97,  97,  97,  97,  97,  98,  99,  99, 100, 101, 102, 103, 105, 107, 109, 113, 117, 122, 128, 135, 142, 148, 153, 158, 161, 163, 165, 167, 168, 169, 170, 171, 171, 172, 173, 173, 173, 173, 173, 173 },
    {  99,  99,  99,  99,  99,  99, 100, 101, 101, 102, 103, 105, 106, 108, 110, 113, 116, 120, 124, 129, 135, 141, 146, 150, 154, 158, 160, 162, 164, 165, 167, 168, 169, 169, 170, 171, 171, 171, 171, 171, 171 },
    { 100, 100, 100, 100, 100, 101, 102, 102, 103, 104, 105, 107, 108, 110, 113, 115, 118, 122, 126, 130, 135, 140, 144, 148, 152, 155, 158, 160, 162, 163, 165, 166, 167, 168, 168, 169, 170, 170, 170, 170, 170 },
    { 102, 102, 102, 102, 102, 103, 103, 104, 105, 106, 107, 109, 111, 113, 115, 117, 120, 123, 127, 131, 135, 139, 143, 147, 150, 153, 155, 158, 159, 161, 163, 164, 165, 166, 167, 167, 168, 168, 168, 168, 168 },
    { 103, 103, 103, 103, 103, 104, 105, 106, 107, 108, 109, 111, 113, 114, 117, 119, 122, 125, 128, 131, 135, 139, 142, 145, 148, 151, 153, 156, 158, 159, 161, 162, 163, 164, 165, 166, 167, 167, 167, 167, 167 },
    { 105, 105, 105, 105, 105, 105, 106, 107, 108, 110, 111, 113, 114, 116, 118, 120, 123, 126, 129, 132, 135, 138, 141, 144, 147, 150, 152, 154, 156, 158, 159, 160, 162, 163, 164, 165, 165, 165, 165, 165, 165 },
    { 106, 106, 106, 106, 106, 107, 108, 109, 110, 111, 113, 114, 116, 118, 120, 122, 124, 127, 129, 132, 135, 138, 141, 143, 146, 148, 150, 152, 154, 156, 158, 159, 160, 161, 162, 163, 164, 164, 164, 164, 164 },
    { 107, 107, 107, 107, 107, 108, 109, 110, 111, 113, 114, 115, 117, 119, 121, 123, 125, 127, 130, 132, 135, 138, 140, 143, 145, 147, 149, 151, 153, 155, 156, 158, 159, 160, 161, 162, 163, 163, 163, 163, 163 },
    { 108, 108, 108, 108, 108, 109, 110, 111, 113, 114, 115, 117, 118, 120, 122, 124, 126, 128, 130, 133, 135, 137, 140, 142, 144, 146, 148, 150, 152, 153, 155, 156, 158, 159, 160, 161, 162, 162, 162, 162, 162 },
    { 110, 110, 110, 110, 110, 110, 111, 113, 114, 115, 116, 118, 119, 121, 123, 124, 126, 129, 131, 133, 135, 137, 139, 141, 144, 146, 147, 149, 151, 152, 154, 155, 156, 158, 159, 160, 160, 160, 160, 160, 160 },
    { 111, 111, 111, 111, 111, 112, 113, 114, 115, 116, 117, 119, 120, 122, 123, 125, 127, 129, 131, 133, 135, 137, 139, 141, 143, 145, 147, 148, 150, 151, 153, 154, 155, 156, 158, 159, 159, 159, 159, 159, 159 },
    { 112, 112, 112, 112, 112, 113, 114, 115, 116, 117, 118, 120, 121, 122, 124, 126, 128, 129, 131, 133, 135, 137, 139, 141, 142, 144, 146, 148, 149, 150, 152, 153, 154, 155, 157, 158, 159, 159, 159, 159, 159 },
    { 112, 112, 112, 112, 113, 114, 114, 115, 117, 118, 119, 120, 122, 123, 125, 126, 128, 130, 131, 133, 135, 137, 139, 140, 142, 144, 145, 147, 148, 150, 151, 152, 153, 155, 156, 157, 158, 159, 159, 159, 159 },
    { 112, 112, 112, 113, 113, 114, 114, 115, 117, 118, 119, 120, 122, 123, 125, 126, 128, 130, 131, 133, 135, 137, 139, 140, 142, 144, 145, 147, 148, 150, 151, 152, 153, 155, 156, 157, 158, 158, 158, 158, 158 },
    { 112, 112, 113, 114, 114, 114, 114, 115, 117, 118, 119, 120, 122, 123, 125, 126, 128, 130, 131, 133, 135, 137, 139, 140, 142, 144, 145, 147, 148, 150, 151, 152, 153, 155, 156, 157, 158, 158, 158, 158, 158 },
    { 112, 113, 114, 114, 114, 114, 114, 115, 117, 118, 119, 120, 122, 123, 125, 126, 128, 130, 131, 133, 135, 137, 139, 140, 142, 144, 145, 147, 148, 150, 151, 152, 153, 155, 156, 157, 158, 158, 158, 158, 158 },
    { 113, 114, 114, 114, 114, 114, 114, 115, 117, 118, 119, 120, 122, 123, 125, 126, 128, 130, 130, 132, 135, 136, 138, 140, 142, 144, 145, 147, 148, 150, 151, 152, 153, 155, 156, 157, 158, 158, 158, 158, 158 }
    
  };



#ifdef SUPPORT_GAMMA

/* Table of gamma values */
byte gamma_table[256];

/* Table of ln(x/256) * 256 for x going from 0 -> 255 */
static s16b gamma_helper[256] =
  {
    0,-1420,-1242,-1138,-1065,-1007,-961,-921,-887,-857,-830,-806,-783,-762,
    -744,-726,-710,-694,-679,-666,-652,-640,-628,-617,-606,-596,-586,-576,-567,
    -577,-549,-541,-532,-525,-517,-509,-502,-495,-488,-482,-475,-469,-463,-457,
    -451,-455,-439,-434,-429,-423,-418,-413,-408,-403,-398,-394,-389,-385,-380,
    -376,-371,-367,-363,-359,-355,-351,-347,-343,-339,-336,-332,-328,-325,-321,
    -318,-314,-311,-308,-304,-301,-298,-295,-291,-288,-285,-282,-279,-276,-273,
    -271,-268,-265,-262,-259,-257,-254,-251,-248,-246,-243,-241,-238,-236,-233,
    -231,-228,-226,-223,-221,-219,-216,-214,-212,-209,-207,-205,-203,-200,-198,
    -196,-194,-192,-190,-188,-186,-184,-182,-180,-178,-176,-174,-172,-170,-168,
    -166,-164,-162,-160,-158,-156,-155,-153,-151,-149,-147,-146,-144,-142,-140,
    -139,-137,-135,-134,-132,-130,-128,-127,-125,-124,-122,-120,-119,-117,-116,
    -114,-112,-111,-109,-108,-106,-105,-103,-102,-100,-99,-97,
    -96,-95,-93,-92,-90,-89,-87,-86,-85,-83,-82,-80,-79,-78,-76,-75,
    -74,-72,-71,-70,-68,-67,-66,-65,-63,-62,-61,-59,-58,-57,-56,-54,
    -53,-52,-51,-50,-48,-47,-46,-45,-44,-42,-41,-40,-39,-38,-37,-35,
    -34,-33,-32,-31,-30,-29,-27,-26,-25,-24,-23,-22,-21,-20,-19,-18,
    -17,-16,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1
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
      value = 256 * 256;
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
	  diff = (((diff / 256) * gamma_helper[i]) *
				 (gamma - 256)) / (256 * n);
	}
      
      /*
       * Store the value in the table so that the
       * floating point pow function isn't needed .
       */
      gamma_table[i] = ((long)(value / 256) * i) / 256;
    }
}

#endif /* SUPPORT_GAMMA */

/*
 * Returns a string which contains the name of a extended color.
 * Examples: "Dark", "Red1", "Yellow5", etc.
 * IMPORTANT: the returned string is statically allocated so it must *not* be
 * freed and its value changes between calls to this function.
 */
cptr get_ext_color_name(byte ext_color)
{
  static char buf[25];
  
  if (GET_SHADE(ext_color) > 0)
    {
      strnfmt(buf, sizeof(buf), "%s%d", color_names[GET_BASE_COLOR(ext_color)],
	      GET_SHADE(ext_color));
    }
  else
    {
      strnfmt(buf, sizeof(buf), "%s", color_names[GET_BASE_COLOR(ext_color)]);
    }
  
  return buf;
}


/*
 * Converts a string to a terminal color byte.
 */
int color_text_to_attr(cptr name)
{
  int i, len, base, shade;
  
  /* Optimize name searching. See below */
  static byte len_names[MAX_BASE_COLORS];
  
  /* Separate the color name and the shade number */
  /* Only letters can be part of the name */
  for (i = 0; isalpha(name[i]); i++) ;
  
  /* Store the start of the shade number */
  len = i;
  
  /* Check for invalid characters in the shade part */
  while (name[i])
    {
      /* No digit, exit */
      if (!isdigit(name[i])) return (-1);
      ++i;
    }
  
  /* Initialize the shade */
  shade = 0;
  
  /* Only analyze the shade if there is one */
  if (name[len])
    {
      /* Convert to number */
      shade = atoi(name + len);
      
      /* Check bounds */
      if ((shade < 0) || (shade > MAX_SHADES - 1)) return (-1);
    }
  
  /* Extra, allow the use of strings like "r1", "U5", etc. */
  if (len == 1)
    {
      /* Convert one character, check sanity */
      if ((base = color_char_to_attr(name[0])) == -1) return (-1);
      
      /* Build the extended color */
      return (MAKE_EXTENDED_COLOR(base, shade));
    }
  
  /* Hack - Initialize the length array once */
  if (!len_names[0])
    {
      for (base = 0; base < MAX_BASE_COLORS; base++)
    	{
	  /* Store the length of each color name */
	  len_names[base] = (byte)strlen(color_names[base & 0x0F]);
    	}
    }
  
  /* Find the name */
  for (base = 0; base < MAX_BASE_COLORS; base++)
    {
      /* Somewhat optimize the search */
      if (len != len_names[base]) continue;
      
      /* Compare only the found name */
      if (my_strnicmp(name, color_names[base & 0x0F], len) == 0)
    	{
	  /* Build the extended color */
	  return (MAKE_EXTENDED_COLOR(base, shade));
    	}
    }
  
  /* We can not find it */
  return (-1);
}


static char *short_color_names[MAX_BASE_COLORS] =
{
  "Dark",
  "White",
  "Slate",
  "Orange",
  "Red",
  "Green",
  "Blue",
  "Umber",
  "L.Dark",
  "L.Slate",
  "Violet",
  "Yellow",
  "L.Red",
  "L.Green",
  "L.Blue",
  "L.Umber"
};

/*
 * Extract a textual representation of an attribute
 */
cptr attr_to_text(byte a)
{
  char *base;

  base = short_color_names[GET_BASE_COLOR(a)];

#if DO_YOU_WANT_THIS_IN_MONSTER_SPOILERS_Q

  if (GET_SHADE(a) > 0)
  {
    static char buf[25];

    strnfmt(buf, sizeof(buf), "%s%d", base, GET_SHADE(a));

    return (buf);
  }

#endif

  return (base);
}

/* 
 * Path finding algorithm variables 
 */
static int terrain[MAX_PF_RADIUS][MAX_PF_RADIUS];
static int ox, oy, ex, ey;

bool is_valid_pf(int y, int x)
{
  int feat;
  
  /* Hack -- assume unvisited is permitted */
  if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);
  
  /* Get mimiced feat */
  feat = f_info[cave_feat[y][x]].mimic;
  
  /* Optionally alter known traps/doors on movement */
  if (((easy_open) && (feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL)) ||
      ((easy_disarm) && (feat >= FEAT_TRAP_HEAD) && (feat <= FEAT_TRAP_TAIL)))
    {
      return (TRUE);
    }
  
  /* Require moveable space */
  if ((feat >= FEAT_MAGMA) && (feat <= FEAT_PERM_SOLID)) return (FALSE);

  /* Don't move over lava, web or void */
  if ((feat == FEAT_LAVA) || (feat == FEAT_WEB) || (feat == FEAT_VOID)) 
    return (FALSE);

  /* Otherwise good */
  return (TRUE);
}

static void fill_terrain_info(void)
{
  int i,j;
  
  ox = MAX(p_ptr->px - MAX_PF_RADIUS / 2, 0);
  oy = MAX(p_ptr->py - MAX_PF_RADIUS / 2, 0);

  ex = MIN(p_ptr->px + MAX_PF_RADIUS / 2 - 1, DUNGEON_WID);
  ey = MIN(p_ptr->py + MAX_PF_RADIUS / 2 - 1, DUNGEON_HGT);
	
  for (i = 0; i < MAX_PF_RADIUS * MAX_PF_RADIUS; i++)
    terrain[0][i] = -1;

  for (j = oy; j < ey; j++)
    for (i = ox; i < ex; i++)
      if (is_valid_pf(j, i))
	terrain[j - oy][i - ox] = MAX_PF_LENGTH;
  
  terrain[p_ptr->py - oy][p_ptr->px - ox] = 1;
}

#define MARK_DISTANCE(c,d) if ((c <= MAX_PF_LENGTH) && (c > d)) \
                              { c = d; try_again = (TRUE); }

bool findpath(int y, int x)
{
  int i, j, k, dir, starty = 0, startx = 0, startdir, start_index;
  bool try_again;
  int cur_distance;
  int findir[] = {1, 4, 7, 8, 9, 6, 3, 2};

  fill_terrain_info();

  terrain[p_ptr->py - oy][p_ptr->px - ox] = 1;

  if ((x >= ox) && (x < ex) && (y >= oy) && (y < ey))
    {
      if ((cave_m_idx[y][x] > 0) && (m_list[cave_m_idx[y][x]].ml))
	{
	  terrain[y - oy][x - ox] = MAX_PF_LENGTH;
	}
      /* else if (terrain[y - oy][x - ox] != MAX_PF_LENGTH)
	{
	bell("Target blocked");
	return (FALSE);
	}*/
      terrain[y - oy][x - ox] = MAX_PF_LENGTH;
    }
  else
    {
      bell("Target out of range.");
      return (FALSE);
    }
  
  if (terrain[y - oy][x - ox] == -1)
    {
      bell("Target space forbidden");
      return (FALSE);
    }
  
  
  /* 
   * And now starts the very naive and very 
   * inefficient pathfinding algorithm
   */
  do
    {
      try_again = (FALSE);
      for (j = oy + 1; j < ey - 1; j++)
	for (i = ox + 1; i < ex - 1; i++)
	  {
	    cur_distance = terrain[j - oy][i - ox] + 1;
	    if ((cur_distance > 0) && (cur_distance < MAX_PF_LENGTH))
	      {
		for (dir = 1; dir < 10; dir++)
		  {
		    if (dir == 5) dir++;
		    MARK_DISTANCE(terrain[j - oy + ddy[dir]][i - ox +ddx[dir]],
				  cur_distance);
		  } 
	      } 
	  } 
      if (terrain[y - oy][x - ox] < MAX_PF_LENGTH)
	try_again = (FALSE);
    } while (try_again);
  
  /* Failure */
  if (terrain[y - oy][x - ox] == MAX_PF_LENGTH)
    {
      bell("Target space unreachable.");
      return (FALSE);
    }

  /* Success */
  i = x;
  j = y;
  
  pf_result_index = 0;
  
  while ((i != p_ptr->px) || (j != p_ptr->py))
    {
      int xdiff = i - p_ptr->px, ydiff = j - p_ptr->py;
      
      cur_distance = terrain[j - oy][i - ox] - 1;

      /* Starting direction */
      if (xdiff < 0) startx = 1;
      else if (xdiff > 0) startx = -1;
      else startx = 0;

      if (ydiff < 0) starty = 1;
      else if (ydiff > 0) starty = -1;
      else starty = 0;

      for (dir = 1; dir < 10; dir++)
	if ((ddx[dir] == startx) && (ddy[dir] == starty)) break;

      /* Should never happend */
      if ((dir % 5) == 0)
	{
	  bell("Wtf ?");
	  return (FALSE);
	}
      
      for (start_index = 0; findir[start_index % 8] != dir; start_index++) 
	;

      for (k = 0; k < 5; k++)
	{
	  dir = findir[(start_index + k) % 8];
	  if (terrain[j - oy + ddy[dir]][i - ox + ddx[dir]] 
	      == cur_distance)
	    break; 
	  dir = findir[(8 + start_index - k) % 8];
	  if (terrain[j - oy + ddy[dir]][i - ox + ddx[dir]] 
	      == cur_distance)
	    break; 
	}

      /* Should never happend */
      if (k == 5)
	{
	  bell("Heyyy !");
	  return (FALSE);
	}
      
      pf_result[pf_result_index++] = '0' + (char)(10 - dir);
      i += ddx[dir];
      j += ddy[dir];
      starty = 0;
      startx = 0;
      startdir = 0;
    }
  pf_result_index--;
  return (TRUE);
}

