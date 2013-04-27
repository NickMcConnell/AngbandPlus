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



/* Hack - this should be in h-types.h, but we need errr and cptr here */

/* Error codes for function return values */
/* Success = 0, Failure = -N, Problem = +N */
typedef int errr;

/* String pointer */
typedef const char *cptr;



/* The init functions for each port called from main.c */

#ifdef USE_XAW
extern errr init_xaw(int argc, char **argv);
extern cptr help_xaw[];
#endif

#ifdef USE_X11
extern errr init_x11(int argc, char **argv);
extern cptr help_x11[];
#endif

#ifdef USE_XPJ
extern errr init_xpj(int argc, char **argv);
extern cptr help_xpj[];
#endif

#ifdef USE_GCU
extern errr init_gcu(void);
extern cptr help_gcu[];
#endif

#ifdef USE_CAP
extern errr init_cap(void);
extern cptr help_cap[];
#endif

#ifdef USE_DOS
extern errr init_dos(void);
extern cptr help_dos[];
#endif

#ifdef USE_IBM
extern errr init_ibm(void);
extern cptr help_ibm[];
#endif

#ifdef USE_EMX
extern errr init_emx(void);
extern cptr help_emx[];
#endif

#ifdef USE_SLA
extern errr init_sla(void);
extern cptr help_sla[];
#endif

#ifdef USE_AMI
extern errr init_ami(void);
extern cptr help_ami[];
#endif

#ifdef USE_VME
extern errr init_vme(void);
extern cptr help_vme[];
#endif

#ifdef USE_LSL
extern errr init_lsl(void);
extern cptr help_lsl[];
#endif

#ifdef USE_GTK
extern errr init_gtk(int argc, char **argv, unsigned char *new_game);
extern cptr help_gtk[];
#endif

#ifdef USE_VCS
extern errr init_vcs(int argc, char **argv);
extern cptr help_vcs[];
#endif

#ifdef USE_TNB
extern errr init_tnb(int argc, cptr *argv);
extern cptr help_tnb[];
#endif

/*
 * Type used to access a module
 */
typedef struct module_type module_type;

struct module_type
{
	cptr name;
	cptr *help;
	errr (*init) (int argc, char **argv, unsigned char *new_game);
};


/*
 * Macro to make an entry for a port in main.c's list.
 *
 * This expands 'INIT_MODULE(port)' to be:
 *
 * { "port", help_port, init_port }   (Without the type-cast. )
 *
 * When adding new ports make sure you use the correct parameter
 * types to init_"port_name"().  If you need to add a new one,
 * add it on the end of the list.  (You don't have to use all the
 * parameters due to the way C passes them on the stack.)
 */
#define INIT_MODULE(P) \
		{ #P, help_##P, (errr (*)(int, char **, unsigned char *)) init_##P }

#endif /* INCLUDED_H_SYSTEM_H */
