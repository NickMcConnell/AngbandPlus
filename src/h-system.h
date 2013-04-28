/* File: h-system.h */

#ifndef INCLUDED_H_SYSTEM_H
#define INCLUDED_H_SYSTEM_H

/*
 * Include the basic "system" files.
 *
 * Make sure all "system" constants/macros are defined.
 * Make sure all "system" functions have "extern" declarations.
 *
 * This file is a big hack to make other files less of a hack.
 * This file has been rebuilt -- it may need a little more work.
 */


/*** ANSI C headers ***/

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#if defined(NeXT)  /* BAD - NeXT uses non-standard headers!  XXX XXX */
# include <libc.h>
#else
# include <stdlib.h>
#endif
#include <string.h>
#include <time.h>


/*** POSIX headers ***/

#if !defined(NeXT) && !defined(RISCOS)
# include <fcntl.h>
#endif

#ifdef SET_UID
# include <pwd.h>
# include <sys/stat.h>
# include <sys/types.h>
# include <unistd.h>
#endif

#if defined(__DJGPP__) || defined(__MWERKS__)
#include <unistd.h>
#endif /* __DJGPP__ || __MWERKS__ */

/*
 * Other headers
 */
#ifdef MACINTOSH
# include <unix.h>
#endif

#if defined(WINDOWS) || defined(MSDOS) || defined(USE_EMX)
# include <io.h>
#endif


#ifdef SET_UID

#ifndef HAVE_USLEEP

/* struct timeval in usleep requires sys/time.h */
/* Is this last if defined test for system required? */
# if defined(Pyramid) || defined(NeXT) || defined(SUNOS) || \
     defined(NCR3K) || defined(SUNOS) || defined(ibm032) || \
     defined(__osf__) || defined(ISC) || defined(SGI) || \
     defined(linux)
#  include <sys/time.h>
# endif

#endif /* HAVE_USLEEP */

#endif /* SET_UID */



/*** Other headers ***/
/* Will remove if not needed:  tell maintainer if needed.  XXX XXX */

#if 0 /* Deprecated */
#ifdef SET_UID

# if !defined(SGI) && !defined(ULTRIX)
#  include <sys/timeb.h>
# endif

# ifndef USG  /* Probably for pre POSIX locking and can be removed ? */
#  include <sys/param.h>
#  include <sys/file.h>
# endif

# ifdef linux  /* Probably for pre POSIX locking and can be removed ? */
#  include <sys/file.h>
# endif

# if defined(SOLARIS)  /* Probably for CHECK_LOAD ? */
#  include <netdb.h>
# endif

#endif /* SET_UID */
#endif /* Deprecated */

#endif /* INCLUDED_H_SYSTEM_H */


