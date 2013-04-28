/* File: h-system.h */

#ifndef INCLUDED_H_SYSTEM_H
#define INCLUDED_H_SYSTEM_H

/*
 * Include the basic "system" files.
 *
 * Make sure all "system" constants/macros are defined.
 * Make sure all "system" functions have "extern" declarations.
 *
 * This file is a small hack to make other files less of a hack.
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


/*** Other headers ***/

#ifdef MACINTOSH
# include <unix.h>
#endif

#if defined(WINDOWS) || defined(MSDOS)
# include <io.h>
#endif


#endif /* INCLUDED_H_SYSTEM_H */
