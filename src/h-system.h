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
 *
 * It is (very) unlikely that VMS will work without help.
 */


#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#if defined(NeXT)
# include <libc.h>
#else
# include <stdlib.h>
#endif

#ifdef SET_UID

# include <sys/types.h>

# if defined(Pyramid) || defined(NeXT) || defined(sun) || \
     defined(NCR3K) || defined(linux) || defined(ibm032) || \
     defined(__osf__) || defined(ISC) || defined(SGI)
#  include <sys/time.h>
# endif

# if !defined(sgi) && !defined(ultrix)
#  include <sys/timeb.h>
# endif

#endif


#include <time.h>



#ifdef MACINTOSH
# include <unix.h>
#endif

/* msdos doesn't need io.h, at least DJGPP doesn't */
/* other gcc-like compilers also don't */
#if defined(WINDOWS) && !defined(__GNUC__)
# include <io.h>
#endif

#if 0
#if !defined(MACINTOSH) && !defined(AMIGA) && \
    !defined(ACORN) && !defined(VM)
# if defined(__TURBOC__) || defined(__WATCOMC__)
#  include <mem.h>
# else
#  include <memory.h>
# endif
#endif
#endif

#if !defined(NeXT) && !defined(__MWERKS__) && !defined(ACORN)
# include <fcntl.h>
#endif


#ifdef SET_UID

# ifndef USG
#  include <sys/param.h>
#  include <sys/file.h>
# endif

# ifdef linux
#  include <sys/file.h>
# endif

# include <pwd.h>

# include <unistd.h>

# include <sys/stat.h>

# if defined(SOLARIS)
#  include <netdb.h>
# endif

#endif

/* open() and close() seem to be defines here? */
#if defined( __DJGPP__ ) || defined ( WINDOWS )
# include "unistd.h"
#endif

#ifdef SET_UID

# ifdef USG
#  include <string.h>
# else
#  include <strings.h>
   extern char *strchr();
   extern char *strstr();
# endif

#else

# include <string.h>

#endif



#if !defined(linux) && !defined(__MWERKS__) && !defined(ACORN) && !defined(MSDOS)
  extern long atol();
#endif


#include <stdarg.h>


#endif

