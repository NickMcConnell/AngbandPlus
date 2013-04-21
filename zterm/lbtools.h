#ifndef LBTOOLS_H
#define LBTOOLS_H

#include "autoconf.h"
#include <stdio.h>

extern void lbui_format(FILE *ofile, int priority, const char *fmt, ...);
#ifdef DEBUG
extern void DBGPUT(const char *fmt, ...);
#else
#define DBGPUT if(1){}else printf
#endif

extern void ERRORMSG(const char *fmt, ...);
#define INFOMSG ERRORMSG
#define DEBUGPUT DBGPUT

#endif /* lbtools.h */
