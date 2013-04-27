
/* $Id: h-basic.h,v 1.4 2003/04/18 21:45:09 cipher Exp $ */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

/*
 * The most basic "include" file.
 *
 * This file simply includes other low level header files.
 */

#ifdef HAVE_CONFIG_H
# ifndef INCLUDED_CONFIG_H
#  include "autoconf.h"
#  define INCLUDED_CONFIG_H
# endif
#endif /* HAVE_CONFIG_H */

/* System Configuration */
#include "h-config.h"

/* System includes/externs */
#include "h-system.h"

/* Basic types */
#include "h-type.h"

/* Basic constants and macros */
#include "h-define.h"

#endif
