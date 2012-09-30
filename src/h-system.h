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
 * It is (very) unlikely that VMS will work without help, primarily
 * because VMS does not use the "ASCII" character set.
 */


#include <stdio.h>
#include <ctype.h>
#include <errno.h>


#if defined(NeXT)
# include <libc.h>
#else
# include <stdlib.h>
#endif /* NeXT */


#ifdef SET_UID

# include <sys/types.h>

# if defined(Pyramid) || defined(NeXT) || defined(SUNOS) || \
     defined(NCR3K) || defined(SUNOS) || defined(ibm032) || \
     defined(__osf__) || defined(ISC) || defined(SGI) || \
     defined(linux)
#  include <sys/time.h>
# endif

# if !defined(SGI) && !defined(ULTRIX)
#  include <sys/timeb.h>
# endif

#endif /* SET_UID */


#include <time.h>


#ifdef MACINTOSH
# include <unix.h>
#endif /* MACINTOSH */


#if defined(WINDOWS) || defined(MSDOS) || defined(USE_EMX)
# include <io.h>
#endif

#if !defined(MACINTOSH) && !defined(AMIGA) && \
    !defined(ACORN) && !defined(VM) && !defined(__MWERKS__)
# if defined(__TURBOC__) || defined(__WATCOMC__)
#  include <mem.h>
# else
#  include <memory.h>
# endif
#endif


#if !defined(NeXT) && !defined(__MWERKS__) && !defined(ACORN)
# include <fcntl.h>
#endif


#ifdef SET_UID

# ifndef USG
#  include <sys/param.h>
#  include <sys/file.h>
# endif	/* !USG */

# ifdef linux
#  include <sys/file.h>
# endif

# include <pwd.h>

# include <unistd.h>

# include <sys/stat.h>

# ifdef SOLARIS
#  include <netdb.h>
# endif

#endif /* SET_UID */

#ifdef __DJGPP__
#include <unistd.h>
#endif /* __DJGPP__ */

#include <string.h>

#include <stdarg.h>


/* Include maid-x11.c */
#if defined(USE_X11) || defined(USE_XAW) || defined(USE_XPJ)
#define USE_XMAID
#endif


/* Hack - this should be in h-types.h, but we need errr here */

/* Error codes for function return values */
/* Success = 0, Failure = -N, Problem = +N */
typedef int errr;


/* The init functions for each port called from main.c */

#ifdef USE_XAW
extern errr init_xaw(int, char **);
#endif

#ifdef USE_X11
extern errr init_x11(int, char **);
#endif

#ifdef USE_XPJ
extern errr init_xpj(int, char **);
#endif

#ifdef USE_GCU
extern errr init_gcu(void);
#endif

#ifdef USE_CAP
extern errr init_cap(int, char **);
#endif

#ifdef USE_DOS
extern errr init_dos(void);
#endif

#ifdef USE_IBM
extern errr init_ibm(void);
#endif

#ifdef USE_EMX
extern errr init_emx(void);
#endif

#ifdef USE_SLA
extern errr init_sla(void);
#endif

#ifdef USE_AMI
extern errr init_ami(void);
#endif

#ifdef USE_VME
extern errr init_vme(void);
#endif

#ifdef USE_LSL
extern errr init_lsl(void);
#endif

#ifdef USE_GTK
extern errr init_gtk(unsigned char *, int, char **);
#endif

#ifdef USE_VCS
extern errr init_vcs(int argc, char **argv);
#endif

#endif /* INCLUDED_H_SYSTEM_H */
