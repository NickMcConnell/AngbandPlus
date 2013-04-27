
/* $Id: h-basic.h,v 1.3 2003/03/17 22:45:24 cipher Exp $ */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

/*
 * The most basic "include" file.
 *
 * This file simply includes other low level header files.
 */

#ifdef HAVE_CONFIG_H
#include "autoconf.h"
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
