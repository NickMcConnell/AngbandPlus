/* File: h-basic.h */

#ifndef INCLUDED_H_BASIC_H
#define INCLUDED_H_BASIC_H

/*
 * The most basic "include" file.
 *
 * This file simply includes other low level header files.
 */

#ifndef INTERFACE
#if defined(WIN_MAKEDLL)
#  define INTERFACE __declspec(dllexport)
#elif defined(WIN_USEDLL)
#  define INTERFACE __declspec(dllimport)
#else
#  define INTERFACE
#endif
#endif /* iface */


/* System Configuration */
#include "h-config.h"

/* System includes/externs */
#include "h-system.h"

/* Basic types */
#include "h-type.h"

/* Basic constants and macros */
#include "h-define.h"

#endif


