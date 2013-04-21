#include <stdarg.h>
#include "lbtools.h"

void
lbui_format(FILE *ofile, int priority, const char *fmt, ...) {
    if (!ofile) {

    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	if (priority)
	    fflush(ofile);
    }
}

FILE *lbui_dbgout = NULL;

#if defined(USE_GCU) || defined(WIN32)
static void
open_dbg_file () {
    lbui_dbgout = fopen("dbgout.txt","a");
    fprintf(lbui_dbgout, "---------------------------------------\n");
}
#endif

#ifdef DEBUG
void
DBGPUT(const char *fmt, ...) {
    FILE *ofile = NULL;

#if defined(USE_GCU) || defined(WIN32)
    if (!lbui_dbgout) open_dbg_file();
    ofile = lbui_dbgout;
#else
    ofile = stderr;
#endif

    if (!ofile) {
	    
    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	fflush(ofile);
    }
}
#endif /* debug */

void
ERRORMSG(const char *fmt, ...) {

    FILE *ofile = NULL;

#if defined(USE_GCU) || defined(WIN32)
    if (!lbui_dbgout) open_dbg_file();
    ofile = lbui_dbgout;
#else
    ofile = stderr;
#endif
    
    if (!ofile) {
	    
    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	fflush(ofile);
    }

}
