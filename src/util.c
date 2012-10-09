/* File: util.c */
/* Purpose: Angband utilities -BEN- */


#include "angband.h"

void dlog(u32b debugflag, char *fmt, ...)
{
   char          buf1[1024];
#ifdef DEBUG_COPY_AS_MESSAGE
   char          buf2[1024];
#endif
   va_list       ap;

   if ((debugflag != DEBUGALWAYS) && (!(debuglevel & debugflag))) return;

   va_start(ap, fmt);
   vsprintf(buf1, fmt, ap);

   /* if an error-logging file-pointer is present, dump the info */
   if (errlog)
   {
      fprintf(errlog, "%s", buf1);
      fflush(errlog);
   }
#ifdef DEBUG_TO_STDERR
   else
      fprintf(stderr, "%s", buf1);
#endif

#ifdef DEBUG_COPY_AS_MESSAGE
   /* add it to the message log, with dbg: in front and without a LF at the end */
   if (!(debugflag & DEBUGMESG) && (message__buf!=NULL))
   {
      s16b ln;
      strcpy(buf2,"dbg: ");
      strcat(buf2, buf1);
      ln = strlen(buf2)-1;
      if (ln>0)
      {
         if (buf2[ln]=='\n') buf2[ln]=0;
      }
      add_msg(buf2);
   }
#endif

   va_end(ap);
}

/*
 * make a dump to the errlog file of the area around the player
 */
void debug_dump_screen_part(int size)
{
#ifdef MAKE_DIST
   return;
#else

   (void)Term_dump_chars(errlog, px-size, py-size, px+size, py+size,
                         panel_prt_col, panel_prt_row);
#endif
}

#ifdef VM

/*
 * Version of "delay()" for VM
 */
void
{
   /* Do nothing */
}

#endif /* VM */


#ifdef AMIGA
#include <proto/dos.h>
/*
 * Version of "delay()" for AMIGA
 */
void delay(int t)
{
   if (t >= 20) Delay(t / 20);
}
#endif /* AMIGA */

#ifdef __EMX__
/*
 * Version of "delay()" for __EMX__
 */
void delay(int t)
{
   _sleep2(t);
}

#endif /* __EMX__ */

#ifdef WINDOWS
/*
 * Version of "delay()" for WINDOWS
 */
void delay(int t)
{
   Term_xtra(TERM_XTRA_DELAY, t);
}

#endif

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

/* jwk - test if string2 appears in string1 - ignore case */
/* FALSE if string2 not in string1 */
/* TRUE  if string2 appears in string1 */
bool cmp_strngs(cptr string1,cptr string2)
{
   int i = 0;
   int len2 = strlen(string2);
   int len1 = strlen(string1);
   bool result = FALSE;
   if (len2>len1) return FALSE;
   while ((result==0) && (i<=(len1-len2)) )
   {
      result=(strnicmp(string1+i,string2,len2)==0);
      i++;
   }
   return result;
}

/* jwk - test if string2 appears in string1 - ignore case */
/* pointer to occurence if string2 appears in string1 */
/* else NULL */
cptr istrstr(cptr string1,cptr string2)
{
   int i = 0;
   int len2 = strlen(string2);
   int len1 = strlen(string1);
   bool result = FALSE;

   if (len2 == 0) return (string1);
   if (len2>len1) return (NULL);
   while ((result==0) && (i<=(len1-len2)) )
   {
      result=(strnicmp(string1+i,string2,len2)==0);
      i++;
   }
   if (result)
   {
      return (string1+i-1);
   }
   else
   {
      return (NULL);
   }
}


#ifdef SET_UID

# ifndef HAS_USLEEP

/*
 * For those systems that don't have "usleep()" but need it.
 *
 * Fake "usleep()" function grabbed from the inl netrek server -cba
 */
static int usleep(huge microSeconds)
{
   struct timeval    Timer;

   int            nfds = 0;

#ifdef FD_SET
   fd_set            *no_fds = NULL;
#else
   int            *no_fds = NULL;
#endif


   /* Was: int readfds, writefds, exceptfds; */
   /* Was: readfds = writefds = exceptfds = 0; */


   /* Paranoia -- No excessive sleeping */
   if (microSeconds > 4000000L) core("Illegal usleep() call");


   /* Wait for it */
   Timer.tv_sec = (microSeconds / 1000000L);
   Timer.tv_usec = (microSeconds % 1000000L);

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
 * Version of "delay()" for Unix machines
 */
void delay(int t)
{
   /* Do it in micro-seconds */
   usleep(1000 * t);
}


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
      (void)strcpy(buf, pw->pw_name);
      buf[16] = '\0';

#ifdef CAPITALIZE_USER_NAME
      /* Hack -- capitalize the user name */
      if (ang_islower(buf[0])) buf[0] = toupper(buf[0]);
#endif

      return;
   }

   /* Oops.  Hack -- default to "PLAYER" */
   strcpy(buf, "PLAYER");
}

#endif /* SET_UID */




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
   cptr           u, s;
   struct passwd     *pw;
   char           user[128];


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
   (void)strcpy(buf, pw->pw_dir);

   /* Append the rest of the filename, if any */
   if (s) (void)strcat(buf, s);

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
 * Hack -- acquire a "temporary" file name if possible
 */
errr path_temp(char *buf, int max)
{
   /* Extract a path */
   return (path_parse(buf, max, tmpnam(NULL)));
}


/*
 * Hack -- replacement for "fopen()"
 */
FILE *my_fopen(cptr file, cptr mode)
{
   char           buf[1024];

   /* Hack -- Try to parse the path */
   if (path_parse(buf, 1024, file)) return (NULL);

   /* Attempt to fopen the file anyway */
   return (fopen(buf, mode));
}

/* preparation for weird machines that don't support ftell */
u32b my_ftell(FILE *fff)
{
   return(ftell(fff));
}

/* preparation for weird machines that don't support fseek */
errr my_fseek(FILE *fff, long offset, int mode)
{
   return (fseek(fff, offset, mode));
}


s32b my_flength(FILE *fff)
{
   u32b temp = my_ftell(fff);
   u32b result;

   (void)fseek(fff, 0, SEEK_END);
   result=my_ftell(fff);
   /* return to original position */
   (void)fseek(fff, temp, SEEK_SET);
   return((s32b)result);
}

/*
 * Hack -- replacement for "fclose()"
 */
errr my_fclose(FILE *fff)
{
   /* Require a file */
   if (!fff)
   {
      return (-1);
   }

   /* Close, check for error */
   if (fclose(fff) == EOF)
   {
      return (1);
   }

   /* Success */
   return (0);
}


#endif /* ACORN */

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
   char  tmp[1024];

   if (n > 1024)
   {
      dlog(DEBUGALWAYS,"util.c: my_fgets: string over 1024 long requested\n");
      quit("util.c: my_fgets: string over 1024 long requested\n");
   }
   /* Read a line */
   if (fgets(tmp, 1024, fff))
   {
      /* Convert weirdness */
      for (s = tmp, i=0; *s; s++)
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
            if (i + 8 >= n)
            {
               buf[0]='\0';
               return(1);
            }

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
            if (i >= n)
            {
               buf[0]='\0';
               return(1);
            }
         }
      }
   }

   /* Nothing */
   buf[0] = '\0';
   return (1);
}

/*
 * Hack -- replacement for "fputs()"
 *
 * Dump a string, plus a newline, to a file
 *
 * XXX XXX XXX Process internal weirdness?
 */
errr my_fputs(FILE *fff, cptr buf, huge n)
{
   /* XXX XXX */
   n = n ? n : 0;

   /* Dump, ignore errors */
   (void)fprintf(fff, "%s\n", buf);

   /* Success */
   return (0);
}


#ifdef ACORN


/*
 * Most of the "file" routines for "ACORN" should be in "main-acn.c"
 */


#else /* ACORN */


/*
 * The Macintosh is a little bit brain-dead sometimes
 */

#ifdef BEN_HACK

/*
 * Code Warrior is a little weird about some functions
 */

extern int creat(const char *, int);
extern int open(const char *, int, ...);
extern int close(int);
extern int read(int, void *, unsigned int);
extern int write(int, const void *, unsigned int);
extern long lseek(int, long, int);

#endif /* BEN_HACK */


#ifdef MACINTOSH

# define open(N,F,M) open((char*)(N),F)

# define write(F,B,S) write(F,(char*)(B),S)

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
   char           buf[1024];

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
   char           buf[1024];
   char           aux[1024];

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
 */
errr fd_copy(cptr file, cptr what)
{
   char           buf[1024];
   char           aux[1024];

   /* Hack -- Try to parse the path */
   if (path_parse(buf, 1024, file)) return (-1);

   /* Hack -- Try to parse the path */
   if (path_parse(aux, 1024, what)) return (-1);

   /* Copy XXX XXX XXX */
   /* (void)rename(buf, aux); */

   /* XXX XXX XXX */
   return (1);
}


/*
 * Hack -- attempt to open a file descriptor (create file)
 */
s16b fd_make(cptr file, int mode)
{
   char           buf[1024];

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

   /* Attempt to open the file */
   return (open(buf, O_WRONLY | O_CREAT | O_BINARY | O_EXCL, mode));

#endif /* BEN_HACK */

}

/*
 * Hack -- attempt to open a file descriptor (existing file)
 */
s16b fd_open(cptr file, int flags)
{
   char           buf[1024];

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

#else

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

   if (fd < 0) return (-1);    /* Verify fd */
   p = lseek(fd, n, SEEK_SET);  /* Seek to the given position */
   if (p < 0) return (1);      /* Failure */
   if (p != n) return (1);     /* Failure */
   return (0);              /* Success */
}

/*
 * Hack -- attempt to truncate a file descriptor
 */
errr fd_chop(int fd, huge n)
{
   /* XXX XXX */
   n = n ? n : 0;

   /* Verify the fd */
   if (fd < 0) return (-1);

#if defined(sun) || defined(ultrix) || defined(NeXT)
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
   if (read(fd, buf, n) != n) return (1);

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
   if (write(fd, buf, n) != n) return (1);

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
 * components (Red, Green, Blue), for example, TERM_BROWN is defined
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
 * Given   Gamma 1.0      Gamma 1.5      Gamma 1.7    Hex 1.7
 * -----    ----        ----        ----       ---
 *  0/4     0.00        0.00        0.00       #00
 *  1/4     0.25        0.27        0.28       #47
 *  2/4     0.50        0.55        0.56       #8f
 *  3/4     0.75        0.82        0.84       #d7
 *  4/4     1.00        1.00        1.00       #ff
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
void move_cursor(int col, int row)
{
   Term_gotoxy(col, row);
}

/*
 * Convert a decimal to a single digit octal number
 */
static char octify(uint i)
{
   return (hexsym[i%8]);
}

/*
 * Convert a decimal to a single digit hex number
 */
static char hexify(uint i)
{
   return (hexsym[i%16]);
}

/*
 * Convert a octal-digit into a decimal
 */
static int deoct(char c)
{
   if (isdigit(c)) return (D2I(c));
   return (0);
}

/*
 * Convert a hexidecimal-digit into a decimal
 */
static int dehex(char c)
{
   if (isdigit(c)) return (D2I(c));
   if (ang_islower(c)) return (A2I(c) + 10);
   if (ang_isupper(c)) return (A2I(tolower(c)) + 10);
   return (0);
}

/*
 * Hack -- convert a printable string into real ascii
 *
 * I have no clue if this function correctly handles, for example,
 * parsing "\xFF" into a (signed) char.  Whoever thought of making
 * the "sign" of a "char" undefined is a complete moron.  Oh well.
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

         /* Hex-mode XXX */
         if (*str == 'x')
         {
            *s = 16 * dehex(*++str);
            *s++ += dehex(*++str);
         }

         /* Hack -- simple way to specify "backslash" */
         else if (*str == '\\')
         {
            *s++ = '\\';
         }

         /* Hack -- simple way to specify "caret" */
         else if (*str == '^')
         {
            *s++ = '^';
         }

         /* Hack -- simple way to specify "space" */
         else if (*str == 's')
         {
            *s++ = ' ';
         }

         /* Hack -- simple way to specify Escape */
         else if (*str == 'e')
         {
            *s++ = ESCAPE;
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

         /* Octal-mode */
         else if (*str == '0')
         {
            *s = 8 * deoct(*++str);
            *s++ += deoct(*++str);
         }

         /* Octal-mode */
         else if (*str == '1')
         {
            *s = 64 + 8 * deoct(*++str);
            *s++ += deoct(*++str);
         }

         /* Octal-mode */
         else if (*str == '2')
         {
            *s = 64 * 2 + 8 * deoct(*++str);
            *s++ += deoct(*++str);
         }

         /* Octal-mode */
         else if (*str == '3')
         {
            *s = 64 * 3 + 8 * deoct(*++str);
            *s++ += deoct(*++str);
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
 */
void ascii_ttext(char *buf, cptr str)
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
         *s++ = i + 64;
      }
      else if (i < 127)
      {
         *s++ = i;
      }
      else if (i < 64)
      {
         *s++ = '\\';
         *s++ = '0';
         *s++ = octify(i / 8);
         *s++ = octify(i % 8);
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
      if (macro__pat[i] == NULL)
      {
         dlog(DEBUGALWAYS,"util.c: macro_find_maybe:  i %d macro__num %d macro__pat[i] NULL?");
         return (-1);
      }
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
 * Local variable -- we are inside a macro action
 */
static bool parse_macro = FALSE;

/*
 * Local variable -- we are inside a "control-underscore" sequence
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
 * Flush the screen, make a noise
 */
void bell(cptr reason)
{
   /* Mega-Hack -- Flush the output */
   Term_fresh();

   /* Hack -- memorize the reason if possible */
   if (reason) message_add(reason);

   /* Make a bell noise (if allowed) */
   if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);

   /* Flush the input (later!) */
   flush();
}

/*
 * Mega-Hack -- Make a (relevant?) sound
 */
void sound(int val)
{
   /* No sound */
   if (!use_sound) return;

   /* Make a sound (if allowed) */
   Term_xtra(TERM_XTRA_SOUND, val);
}

/*
 * Helper function called only from "inkey()"
 *
 * This function does most of the "macro" processing.
 *
 * We use the "Term_key_push()" function to handle "failed" macros,
 * as well as "extra" keys read in while choosing a macro, and the
 * actual action for the macro.
 *
 * Embedded macros are illegal, although "clever" use of special
 * control chars may bypass this restriction.  Be very careful.
 *
 * The user only gets 500 (1+2+...+29+30) milliseconds for the macro.
 *
 * Note the annoying special processing to "correctly" handle the
 * special "control-backslash" codes following a "control-underscore"
 * macro sequence.   See "main-x11.c" and "main-xaw.c" for details.
 */
static char inkey_aux(void)
{
   int      k = 0, n, p = 0, w = 0;

   char     ch;

   cptr     pat, act;

   char     buf[1024];


   /* Wait for keypress */
   (void)(Term_inkey(&ch, TRUE, TRUE));

dlog(DEBUGKEYS,"util.c: inkey_aux: pressed ascii %d ('%c') parse_macro %d parse_under %d \n",
               (int)ch, ch, parse_macro, parse_under);

   /* End "macro action" */
   if (ch == 30)
   {
dlog(DEBUGKEYS,"util.c: inkey_aux: ascii 30 encountered, parse_macro now FALSE, returning\n");
      parse_macro = FALSE;
      return (ch);
   }

   /* Inside "macro action" || "macro trigger" */
   if (parse_macro || parse_under)
   {
dlog(DEBUGKEYS,"util.c: inkey_aux: parse_macro %d parse_under %d returning %d\n", parse_macro, parse_under, ch);
      return (ch);
   }

   /* Save the first key, advance */
   if (p > 1023)
   {  
      dump_stack(NULL); 
      quit("buffer overflow in util.c: inkey_aux()?\n");
   }
   buf[p++] = ch;
   buf[p] = '\0';

   /* Check for possible macro */
   k = macro_find_check(buf);
dlog(DEBUGKEYS,"util.c: inkey_aux: macro %d found, p %d\n", k, p);

   /* No macro pending */
   if (k < 0) return (ch);

   /* Wait for a macro, or a timeout */
   while (TRUE)
   {
      /* Check for pending macro */
      k = macro_find_maybe(buf);

      /* No macro pending */
      if (k < 0) break;
dlog(DEBUGKEYS,"util.c: inkey_aux: waiting for macro / timeout, macro_find_maybe %d\n", k);

      /* Check for (and remove) a pending key */
      if (0 == Term_inkey(&ch, FALSE, TRUE))
      {
         /* Append the key */
         buf[p++] = ch;
         buf[p] = '\0';
dlog(DEBUGKEYS,"util.c: inkey_aux: waiting for macro / timeout, %d ('%c') added as %d to buf\n", (int)ch, ch, p);

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
 * This function recognizes a few "global parameters".   These are variables
 * which, if set to TRUE before calling this function, will have an effect
 * on this function, and which are always reset to FALSE by this function
 * before this function returns.  Thus they function just like normal
 * parameters, except that most calls to this function can ignore them.
 *
 * Normally, this function will process "macros", but if "inkey_base" is
 * TRUE, then we will bypass all "macro" processing.  This allows direct
 * usage of the "Term_inkey()" function.
 *
 * Normally, this function will do something, but if "inkey_xtra" is TRUE,
 * then something else will happen.
 *
 * Normally, this function will wait until a "real" key is ready, but if
 * "inkey_scan" is TRUE, then we will return zero if no keys are ready.
 *
 * Normally, this function will show the cursor, and will process all normal
 * macros, but if "inkey_flag" is TRUE, then we will only show the cursor if
 * "hilite_player" is TRUE, and also, we will only process "command" macros.
 *
 * Note that the "flush()" function does not actually flush the input queue,
 * but waits until "inkey()" is called to perform the "flush".
 *
 * Refresh the screen if waiting for a keypress and no key is ready.
 *
 * Note that "back-quote" is automatically converted into "escape" for
 * convenience on machines with no "escape" key.  This is done after the
 * macro matching, so the user can still make a macro for "backquote".
 *
 * Note the special handling of a few "special" control-keys, which
 * are reserved to simplify the use of various "main-xxx.c" files,
 * or used by the "macro" code above.
 *
 * Ascii 27 is "control left bracket" -- normal "Escape" key
 * Ascii 28 is "control backslash" -- special macro delimiter
 * Ascii 29 is "control right bracket" -- end of macro action
 * Ascii 30 is "control caret" -- indicates "keypad" key
 * Ascii 31 is "control underscore" -- begin macro-trigger
 *
 * Hack -- Note the use of "inkey_next" to allow "keymaps" to be processed.
 *
 * Hack -- Make sure to allow calls to "inkey()" even if "term_screen"
 * is not the active Term, this allows the various "main-xxx.c" files
 * to only handle input when "term_screen" is "active".
 */
char inkey(void)
{
   int   v;
   char  kk, ch = 0;
   bool  done = FALSE;
   term *old = Term;

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
   if (!inkey_scan && (!inkey_flag || hilite_player || character_icky))
   {
      /* Show the cursor */
      (void)Term_set_cursor(1);
   }

   /* Hack -- Activate main screen */
   Term_activate(angband_term[0]);

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
         /* Hack -- activate proper term */
         Term_activate(old);

         /* Flush output */
         Term_fresh();

         /* Hack -- activate main screen */
         Term_activate(angband_term[0]);

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


      /* Treat back-quote as escape */
      if (ch == '`') ch = ESCAPE;


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
dlog(DEBUGKEYS,"util.c: inkey: returning %d\n", ch);
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

   /* Paranoia -- Require room */
   if (quark__num == QUARK_MAX) return (0);

   /* Add a new quark */
   quark__str[i] = string_make(str);

   /* Count the quarks */
   quark__num++;

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
 * Second try for the "message" handling routines.
 *
 * Each call to "message_add(s)" will add a new "most recent" message
 * to the "message recall list", using the contents of the string "s".
 *
 * The messages will be stored in such a way as to maximize "efficiency",
 * that is, the number of sequential messages that can be retrieved, given
 * a limited amount of space in which to store them.
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

/*
 * How many messages are "available"?
 */
/* jk rewritten for long ints */
u32b message_num(void)
{
   u32b last, next, n;

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
static cptr message_str_sub(u32b age, bool clean)
{
   s32b x;
   s32b o;
   cptr s;
dlog(DEBUGMESG,"util.c: message_str_sub: age %ld clean %d\n", age, clean);
   /* Forgotten messages have no text */
   if (age >= message_num()) return ("");

   /* Acquire the "logical" index */
   x = (message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX;

   /* Get the "offset" for the message */
   o = message__ptr[x];
dlog(DEBUGMESG,"util.c: message_str_sub: x %ld o %ld\n", x, o);

   /* Access the message text */
   s = &message__buf[o];

   if (clean || (message__cnt[x]==1))
   {
   /* Return the message text */
      return (s);
   }
   else
   {
   /* Return the message text, with (20x) appended */
      return (format("%s(%ux)", s, message__cnt[x]));
   }
}

/*
 * recall the normal message string
 * with (20x) added, if necessary
 */
cptr message_str(u32b age)
{
   return message_str_sub(age, FALSE);
}

/*
 * recall the 'clean' message string
 * without (20x) needed for saving
 */
cptr message_str_clean(u32b age)
{
   return message_str_sub(age, TRUE);
}

byte message_cnt(u32b age)
{
   return (message__cnt[age]);
}

/*
 * this function adds one line to the message_log file
 */
static void line_to_logfile(cptr str)
{
   FILE *f;
   char temp[1024];

   strcpy(temp, levelfile);
   strcat(temp,".msg");
   f = fopen(temp, "a+");

   if (f == NULL)
   {
      quit(format("Error: cannot open log file %s for appending/writing.", temp));
   }
   /* print the message */
dlog(DEBUGMESG,"util.c: line_to_logfile: writing %d bytes: %s\n", strlen(str), str);
   fprintf(f, "%s\n", str);
   /* save the offset if necessary */
   fflush(f);
   if (fclose(f)!=0)
   {
      quit(format("Error: error closing log file %s", temp));
   }
}

/*
 * This function makes sure all messages are on disk
 */
void message_flush(void)
{
dlog(DEBUGMESG,"util.c: message_flush: flushing cnt %d valid %d msg %s\n",
               prev_message_cnt, prev_message[0], prev_message);
   /* to prevent more flushes in a row making a set of empty lines */
   if ((prev_message[0]>0) && (save_messages))
   {
      if (prev_message_cnt>1)
      {
         line_to_logfile(format("%s(%dx)", prev_message, prev_message_cnt));
      }
      else
      {
         line_to_logfile(prev_message);
      }
   }
   strcpy(prev_message,"");
   prev_message_cnt = 1;
dlog(DEBUGMESG,"util.c: message_flush: flushing msg now %s\n",
               prev_message);
}

/*
 * initialize all message buffers
 */
static void init_messages(void)
{
   strcpy(prev_message, "Logfile opened");
   prev_message_cnt = 1;

   /* Message variables */
   C_MAKE(message__ptr, MESSAGE_MAX, u32b);
   C_MAKE(message__buf, MESSAGE_BUF, char);
   C_MAKE(message__cnt, MESSAGE_BUF, byte);

   /* Hack -- No messages yet */
   message__tail = MESSAGE_BUF;
   message__prev = 0;

}

/*
 * Add a new message, with great efficiency
 */
void message_add(cptr mystr)
{
   u32b i,x,n;
   char str[1024];
   static bool first_time = TRUE;

   if (first_time)
   {
      first_time = FALSE;
      init_messages();
   }

   /* Hack -- Ignore "non-messages" */
dlog(DEBUGMESG, "util.c: message_add: empty msg, returning\n");
   if (!mystr) return;

   strcpy(str, mystr);
dlog(DEBUGMESG, "util.c: message_add: called:\nstr %s\nprev_message %s\n", str, prev_message);

   /*** Step 0 -- write something to file if needed ***/
   if (streq(str, prev_message))
   {
      /* handle 'you hit a wall (40x) messages */
      prev_message_cnt++;
      /* we don't worry about overflow */
dlog(DEBUGMESG,"util.c: message_add: adding similar message, cnt %d\n",
            prev_message_cnt);
      /* if we store in memory, try to store it correctly!   */
      /* we are only here if there *are* previous message(s) */
      message__cnt[message__prev]++;
      /* if we did not overflow, return now. */
      if (message__cnt!=0) return;
   }
   else /* non-equal message */
   {
dlog(DEBUGMESG,"util.c: message_add: unknown message, flushing old msgs\n");
      message_flush();
      strcpy(prev_message, str);
dlog(DEBUGMESG,"util.c: message_add: prev_message now \n%s\n", prev_message);
   }

   /*** Step 1 -- Analyze the message ***/

   /* Message length */
   n = (long)strlen(str);

   /* Important Hack -- Ignore "long" messages */
   if (n >= MESSAGE_BUF / 4L) return;

   /*** Step 2 -- Attempt to optimize ***/

   /* this step is removed here.
    * we suppose we have enough memory, and if we point to much older
    * messages, we get into trouble when we try to delete these...
    * You hit a wall.(40x) should optimize most messages anyway
    */

dlog(DEBUGMESG,"util.c: message_add: step 3\n");
   /*** Step 3 -- Ensure space before end of buffer ***/

   /* Kill messages and Wrap if needed */
   if (message__head + n + 1L >= MESSAGE_BUF)
   {
      /* Kill all "dead" messages */
      for (i = message__last; TRUE; i++)
      {
         /* Wrap if needed */
         if (i == MESSAGE_MAX) i = 0L;

         /* Stop before the new message */
         if (i == message__next) break;

         /* Kill "dead" messages */
         if (message__ptr[i] >= message__head)
         {
            /* Track oldest message */
            message__last = i + 1L;
         }
      }

      /* Wrap "tail" if needed */
      if (message__tail >= message__head) message__tail = 0L;

      /* Start over */
      message__head = 0L;
   }


   /*** Step 4 -- Ensure space before next message ***/

   /* Kill messages if needed */
   if (message__head + n + 1L > message__tail)
   {
      /* Grab new "tail" */
      message__tail = message__head + n + 1L;

      /* Advance tail while possible past first "nul" */
      while (message__buf[message__tail-1]) message__tail++;

      /* Kill all "dead" messages */
      for (i = message__last; TRUE; i++)
      {
         /* Wrap if needed */
         if (i == MESSAGE_MAX) i = 0L;

         /* Stop before the new message */
         if (i == message__next) break;

         /* Kill "dead" messages */
         if ((message__ptr[i] >= message__head) &&
            (message__ptr[i] < message__tail))
         {
            /* Track oldest message */
            message__last = i + 1L;
         }
      }
   }

   /*** Step 5 -- Grab a new message index ***/

   /* Store the number of the previous message */
   message__prev = message__next;

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
   message__cnt[x] = 1;

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

/*
 * Hack -- flush
 */
static void msg_flush(int x, bool nopause)
{
   byte a = TERM_WHITE;

/* jk - more is less needfull now, so if we have 10 lines to spare */
/* we count half of them more's before we actually print one */

#ifdef USE_COLOR
   /* Use light blue */
   if (use_color) a = TERM_L_BLUE;
#endif

dlog(DEBUGMESG,"util.c: msg_flush: x %d nopause %d inkey_scan %d\n", x, nopause, inkey_scan);
   if (!nopause)
   {
      /* Pause for response */

/* jk - 0 replaced by MESSAGE_ROW */
      Term_putstr(x, MESSAGE_ROW, -1, a, "-more-");

/* jk - this is in line with inkey_scan's definition */

      /* Get an acceptable keypress */
      while (!inkey_scan)
      {
         int cmd = inkey();
         if (quick_messages) break;
         if ((cmd == ESCAPE) || (cmd == ' ')) break;
         if ((cmd == '\n') || (cmd == '\r')) break;
         bell("Illegal response to a 'more' prompt!");
      }
   }

   /* Clear the line */
/* jk - 0 replaced by MESSAGE_ROW */
   Term_erase(0, MESSAGE_ROW, 80);
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

   char *t;

   char buf[1024];

   /* Hack -- Reset */

dlog(DEBUGMESG,"util.c: msg_print: msg_flag %d p %d ln %d msg %s\n",
               msg_flag, p, msg?strlen(msg):-1, msg?msg:"<<null>>");
   if (!msg_flag) p = 0;

   /* Message Length */
   n = (msg ? strlen(msg) : 0);

   /* flush when a message is visible and the new one doesn't fit,
    * or the new one is the null message
    */

   if (p && (!msg || ((p + n) > 72)))
   {
      /* Flush */
      msg_flush(p, (p==0));

      /* Forget it */
      msg_flag = FALSE;

      /* Reset */
      p = 0;
   }

   /* No message */
   if (!msg) return;

   /* Paranoia */
   if (n > 1000) return;

   /* Copy it */
   strcpy(buf, msg);

   /* Analyze the buffer */
   t = buf;

   /* Split message */

   while (n > 72)

   {
      char oops;

      int check, split;

      /* Default split */
      split = 72;

      /* Find the "best" split point */
      for (check = 40; check < 72; check++)
      {
         /* Found a valid split point */
         if (t[check] == ' ') split = check;
      }

      /* Save the split character */
      oops = t[split];

      /* Split the message */
      t[split] = '\0';

      /* Display part of the message */

/* jk - 0 replaced by MESSAGE_ROW */
      Term_putstr(0, MESSAGE_ROW, split, TERM_WHITE, t);

      /* Flush it */
      msg_flush(split + 1, FALSE);

      /* Memorize the piece */
      message_add(t);

      /* Restore the split character */
      t[split] = oops;

      /* Insert a space */
      t[--split] = ' ';

      /* Prepare to recurse on the rest of "buf" */
      t += split; n -= split;
   }
   /* Display the tail of the message */

/* jk - 0 replaced by MESSAGE_ROW */
   Term_putstr(p, MESSAGE_ROW, n, TERM_WHITE, t);

   /* Memorize the tail */

   message_add(t);

   /* Window stuff */
   p_ptr->window |= (PW_MESSAGE);

   /* Remember the message */
   msg_flag = TRUE;

   /* Remember the position */
   if (n>1)
      p += n + 1;
   else
      p += n;

   /* Optional refresh */

   if (fresh_message) Term_fresh();
}

/*
 * this does what msg_print does, but doesn't show it on screen
 */
void add_msg(cptr msg)
{
   int n;

   char *t;

   char buf[1024];

   /* Hack -- Reset */

   /* Message Length */
   n = (msg ? strlen(msg) : 0);

   /* No message */
   if (!msg) return;

   /* Paranoia */
   if (n > 1000) return;

   /* Copy it */
   strcpy(buf, msg);

dlog(DEBUGMESG,"util.c: add_msg: buf now %s\n", buf);

   /* Analyze the buffer */
   t = buf;

   /* Split message */

   while (n > 72)

   {
      char oops;

      int check, split;

      /* Default split */
      split = 72;

      /* Find the "best" split point */
      for (check = 40; check < 72; check++)
      {
         /* Found a valid split point */
         if (t[check] == ' ') split = check;
      }

      /* Save the split character */
      oops = t[split];

      /* Split the message */
      t[split] = '\0';

      /* Display part of the message */

      /* Memorize the piece */
      message_add(t);

      /* Restore the split character */
      t[split] = oops;

      /* Insert a space */
      t[--split] = ' ';

      /* Prepare to recurse on the rest of "buf" */
      t += split; n -= split;
   }
   /* Memorize the tail */
   message_add(t);
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
   if (strlen(buf)>1)
   {
      if (buf[strlen(buf)-1]==10)
      {
         buf[strlen(buf)-1]=0;
      }
   }
   msg_print(buf);
}

/*
 * Add a formatted message, using "vstrnfmt()" and "msg_print()".
 */
void add_msg_format(cptr fmt, ...)
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
   if (strlen(buf)>1)
   {
      if (buf[strlen(buf)-1]==10)
      {
         buf[strlen(buf)-1]=0;
      }
   }
   add_msg(buf);
}

/*
 * Erase the screen
 */
void clear_screen(void)
{
   /* Clear the screen */
   Term_clear();
}

/*
 * Print some (colored) text to the screen at the current cursor position,
 * automatically "wrapping" existing text (at spaces) when necessary to
 * avoid placing any text into the last column, and clearing every line
 * before placing any text in that line.  Also, allow "newline" to force
 * a "wrap" to the next line.  Advance the cursor as needed so sequential
 * calls to this function will work correctly.
 *
 * Once this function has been called, the cursor should not be moved
 * until all the related "c_roff()" calls to the window are complete.
 *
 * This function will correctly handle any width up to the maximum legal
 * value of 256, though it works best for a standard 80 character width.
 */
void c_roff(byte a, cptr str)
{
   int x, y;

   int w, h;

   cptr s;


   /* Obtain the size */
   (void)Term_get_size(&w, &h);

   /* Obtain the cursor */
   (void)Term_locate(&x, &y);

   /* Process the string */
   for (s = str; *s; s++)
   {
      char ch;

      /* Force wrap */
      if (*s == '\n')
      {
         /* Wrap */
         x = 0;
         y++;

         /* Clear line, move cursor */
         Term_erase(x, y, 255);
      }

      /* Clean up the char */
      ch = (isprint(*s) ? *s : ' ');

      /* Wrap words as needed */
      if ((x >= w - 1) && (ch != ' '))
      {
         int i, n = 0;

         byte av[256];
         char cv[256];

         /* Wrap word */
         if (x < w)
         {
            /* Scan existing text */
            for (i = w - 2; i >= 0; i--)
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
         if (n == 0) n = w;

         /* Clear line */
         Term_erase(n, y, 255);

         /* Wrap */
         x = 0;
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

      /* Advance */
      if (++x > w) x = w;
   }
}

/*
 * As above, but in "white"
 */
void roff(cptr str)
{
   /* Spawn */
   c_roff(TERM_WHITE, str);
}




/*
 * Clear part of the screen
 */
void clear_from(int row)
{
/* Erase part of the screen */
   int y;

   /* Erase requested rows */
   for (y = row; y < Term->hgt; y++)
   {
     /* Erase part of the screen */
     Term_erase(0, y, 255);
   }
}

/*
 * Display a string on the screen using an attribute.
 *
 * At the given location, using the given attribute, if allowed,
 * add the given string.  Do not clear the line.
 */
void c_put_str(byte attr, cptr str, int col, int row)
{
   byte a = TERM_WHITE;

#ifdef USE_COLOR
   /* Allow color */
   if (use_color) a = attr;
#endif

   /* Put the string (in color) */
   Term_putstr(col, row, -1, a, str);
}


/*
 * Like "c_put_str()", but without an attribute.
 */
void put_str(cptr str, int col, int row)
{
   /* Put the string (in white) */
   Term_putstr(col, row, -1, TERM_WHITE, str);
}


/*
 * Print a string to the screen in white, clearing to end of line
 */
void prt(cptr str, int col, int row)
{
   /* Clear the line, position the cursor */
   Term_erase(col, row, 255);

   /* Dump the text (in White) */
   Term_addstr(-1, TERM_WHITE, str);
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
 */
bool askfor_aux(char *buf, int len)
{
   int y, x;

   int i = 0;

   int k = 0;

   bool done = FALSE;

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
   Term_erase(x, y, len);
   Term_putstr(x, y, -1, TERM_YELLOW, buf);

   /* Process input */
   while (!done)
   {
      /* Place cursor */
      Term_gotoxy(x + k, y);

      /* Get a key */
      i = inkey();

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
            if (k > 0) k--;
            break;

         default:
            if ((k < len) && (isprint(i)))
            {
               buf[k++] = i;
            }
            else
            {
               bell("Illegal edit key!");
            }
            break;
      }

      /* Terminate */
      buf[k] = '\0';

      /* Update the entry */
      Term_erase(x, y, len);
      Term_putstr(x, y, -1, TERM_WHITE, buf);
   }

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
   prt(prompt, 0, MESSAGE_ROW);

   /* Ask the user for a string */
   res = askfor_aux(buf, len);

   /* Clear prompt */
   prt("", 0, MESSAGE_ROW);

   /* Result */
   return (res);
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
   int i;

   char buf[80];

   /* Paranoia XXX XXX XXX */
   msg_print(NULL);

   /* Ha`k -- Build a "useful" prompt */
   strnfmt(buf, 78, "%.70s[y/n] ", prompt);

   /* Prompt for it */
   prt(buf, 0, MESSAGE_ROW);

   /* Get an acceptable answer */
   while (TRUE)
   {
      i = inkey();
      if (quick_messages) break;
      if (i == ESCAPE) break;
      if (strchr("YyNn", i)) break;
      bell("Illegal response to a 'yes/no' question!");
   }

   /* Erase the prompt */
   prt("", 0, MESSAGE_ROW);

   /* Normal negation */
   if ((i != 'Y') && (i != 'y')) return (FALSE);

   /* Success */
   return (TRUE);
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
   /* Paranoia XXX XXX XXX */
   msg_print(NULL);

   /* Display a prompt */
   prt(prompt, 0, MESSAGE_ROW);

   /* Get a key */
   *command = inkey();

   /* Clear the prompt */
   prt("", 0, MESSAGE_ROW);

   /* Handle "cancel" */
   if (*command == ESCAPE) return (FALSE);

   /* Success */
   return (TRUE);
}


/*
 * Request a "quantity" from the user
 *
 * Hack -- allow "p_ptr->command_arg" to specify a quantity
 */
s32b get_quantity(cptr prompt, u32b max, u32b standard)
{
   u32b amt;

   char tmp[80];

   char buf[80];


   /* Use "p_ptr->command_arg" */
   if (p_ptr->command_arg)
   {
      /* Extract a number */
      amt = p_ptr->command_arg;

      /* Clear "p_ptr->command_arg" */
      p_ptr->command_arg = 0;

      /* Enforce the maximum */
      if (amt > max) amt = max;

      /* Use it */
      return (amt);
   }

   /* Build a prompt if needed */
   if (!prompt)
   {
      /* Build a prompt */
      sprintf(tmp, "Quantity (1-%ld): ", max);

      /* Use that prompt */
      prompt = tmp;
   }

   /* Default to one */
   amt = standard;

   /* Build the default */
   sprintf(buf, "%ld", amt);

   /* Ask for a quantity */
   if (!get_string(prompt, buf, 8)) return (0);

   /* Extract a number */
   amt = atol(buf);

   /* A letter means "all" */
   if (isalpha((int)buf[0])) amt = max;

   /* Enforce the maximum */
   if (amt > max) amt = max;

   /* Enforce the minimum */
   if (amt < 0L) amt = 0L;

   /* Return the result */
   return (amt);
}


/*
 * Pause for user response XXX XXX XXX
 */
void pause_line(int row)
{
   int i;
   prt("", 0, row);
   put_str("[Press any key to continue]", 23, row);
   i = inkey();
   prt("", 0, row);
}

/*
 * Hack -- special buffer to hold the action of the current keymap
 */
static char request_command_buffer[256];

/*
 * Request a command from the user.
 *
 * Sets p_ptr->command_cmd, p_ptr->command_dir, p_ptr->command_rep, p_ptr->command_arg.
 *
 * Note that "caret" ("^") is treated special, and is used to
 * allow manual input of control characters.  This can be used
 * on many machines to request repeated tunneling (Ctrl-H) and
 * on the Macintosh to request "Control-Caret".
 *
 * Note that this command is used both in the dungeon and in
 * stores, and must be careful to work in both situations.
 */
void request_command(void)
{
   char cmd;
   s16b i, mode;
   cptr act;

   p_ptr->command_cmd = 0;     /* No command yet */
   p_ptr->command_arg = 0;     /* No "argument" yet */
   p_ptr->command_dir = 0;     /* No "direction" yet */

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

   /* Get command */
   while (1)
   {
      /* Hack -- auto-commands */
      if (p_ptr->command_new)
      {
         /* Flush messages */
         msg_print(NULL);

         /* Use auto-command */
         cmd = p_ptr->command_new;

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
/* jk */
         cmd = inkey();
         cmd_sliding = cmd;
      }

      /* Clear top line */
      prt("", 0, 0);


      /* Command Count */
      if (cmd == '0')
      {
         int old_arg = p_ptr->command_arg;

         /* Reset */
         p_ptr->command_arg = 0;

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
               p_ptr->command_arg = p_ptr->command_arg / 10;

               /* Show current count */
               prt(format("Count: %d", p_ptr->command_arg), 0, 0);
            }

            /* Actual numeric data */
            else if (cmd >= '0' && cmd <= '9')
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
                  p_ptr->command_arg = p_ptr->command_arg * 10 + D2I(cmd);
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
         if ((cmd == ' ') || (cmd == '\n') || (cmd == '\r'))
         {
            /* Get a real command */
            if (!get_com("Command: ", &cmd))
            {
               /* Clear count */
               p_ptr->command_arg = 0;

               /* Continue */
               continue;
            }
         }
      }


      /* Allow "keymaps" to be bypassed */
      if (cmd == '\\')
      {
         /* Get a real command */
         (void)get_com("Command: ", &cmd);

         /* Hack -- bypass keymaps */
         if (!inkey_next) inkey_next = "";
      }


      /* Allow "control chars" to be entered */
      if (cmd == '^')
      {
         /* Get a new command and controlify it */
         if (get_com("Control: ", &cmd)) cmd = KTRL(cmd);
      }


      /* Look up applicable keymap */
      act = keymap_act[mode][(byte)(cmd)];

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
      if (!cmd) continue;


      /* Use command */
      p_ptr->command_cmd = cmd;

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

bool stat_sort_comp(vptr u, vptr v, s16b a, s16b b)
{
   s16b *x = (s16b*)(u);
   return (x[a]<=x[b]);
}

void stat_sort_swap(vptr u, vptr v, s16b a, s16b b)
{
   s16b *x = (s16b*)(u);
   s16b temp = x[a];
   x[a] = x[b];
   x[b] = temp;
}
