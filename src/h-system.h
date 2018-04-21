/* File: h-system.h */

#ifndef INCLUDED_H_SYSTEM_H
#define INCLUDED_H_SYSTEM_H

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 *
 *
 * James E. Wilson and Robert A. Koeneke released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version),
 * or under the terms of the traditional Angband license.
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2),
 * or under the terms of the traditional Angband license.
 */

/*
 * Include the basic "system" files.
 *
 * Make sure all "system" constants/macros are defined.
 * Make sure all "system" functions have "extern" declarations.
 *
 * This file is a big hack to make other files less of a hack.
 * This file has been rebuilt -- it may need a little more work.
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

# if defined(Pyramid) || defined(NeXT) || defined(SUNOS) || \
defined(NCR3K) || defined(SUNOS) || defined(ibm032) || \
defined(__osf__) || defined(ISC) || defined(SGI) || \
defined(linux)
#  include <sys/time.h>
# endif

# if !defined(SGI) && !defined(ULTRIX) && !defined(USE_CLANG)
#  include <sys/timeb.h>
# endif

#endif


#include <time.h>



#if defined(MACINTOSH) && defined(__MWERKS__)
# include <unix.h>
#endif

#if defined(WINDOWS) || defined(MSDOS) || defined(USE_EMX)
# include <io.h>
#endif

#if !defined(MACINTOSH) && !defined(AMIGA) && \
!defined(RISCOS) && !defined(__MWERKS__)
# if defined(__TURBOC__) || defined(__WATCOMC__)
#  include <mem.h>
# else
#  include <memory.h>
# endif
#endif


#if !defined(NeXT) && !defined(RISCOS)
# include <fcntl.h>
#endif

#if defined(SET_UID) || defined(USE_PRIVATE_PATHS)
# include <pwd.h>
#endif

#ifdef SET_UID

# ifndef USG
#  include <sys/param.h>
#  include <sys/file.h>
# endif

# ifdef linux
#  include <sys/file.h>
# endif

# include <unistd.h>

# include <sys/stat.h>

# if defined(SOLARIS)
#  include <netdb.h>
# endif

#endif

#if defined(__DJGPP__) || defined(__MWERKS__)
#include <unistd.h>
#endif /* __DJGPP__ || __MWERKS__ */

#include <string.h>

#include <stdarg.h>


#endif

