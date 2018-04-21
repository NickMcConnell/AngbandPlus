/* File: h-basic.h */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

#include "msvc_warnings.h"

/*
 * The most basic "include" file.
 *
 * This file simply includes other low level header files.
 */

#ifdef HAVE_CONFIG_H

#include "autoconf.h"

#else

/**
 * Native MSVC compiler doesn't understand inline or snprintf
 */
#ifdef _MSC_VER
#	define inline __inline
#	define snprintf _snprintf
#endif

/* Necessary? */
#ifdef NDS
# include <fat.h>
# include <unistd.h>
# include <reent.h>
# include <sys/iosupport.h>
# include <errno.h>
#endif

/**
 * Using C99, assume we have stdint and stdbool
 */
# if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) \
  || (defined(_MSC_VER) && _MSC_VER >= 1600L)
#  define HAVE_STDINT_H
# endif

# if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#  define HAVE_STDbool_H
# endif

/**
 * Everyone except RISC OS has fcntl.h and sys/stat.h
 */
#define HAVE_FCNTL_H
#define HAVE_STAT
#define HAVE_USLEEP

#endif /* HAVE_CONFIG_H */

/* Use various POSIX functions if available */
#undef _GNU_SOURCE
#define _GNU_SOURCE

/* System Configuration */
#include "h-config.h"

/* System includes/externs */
#include "h-system.h"

/* Basic types */
#include "h-type.h"

/* Basic constants and macros */
#include "h-define.h"

#endif

